use core::panic;
use std::{
    cell::RefCell,
    collections::HashMap,
    ffi::CString,
    fmt::Debug,
    fs, iter,
    ops::{DerefMut, Index, Range},
    path::Path,
    str::FromStr,
};

use cranelift::{
    codegen::ir::{
        ArgumentExtension, ArgumentPurpose, BlockArg, FuncRef, Opcode, StackSlot, ValueLabel,
    },
    frontend::FuncInstBuilder,
    module::{DataDescription, DataId, FuncId, Module},
    object::ObjectModule,
    prelude::{
        AbiParam, Block, EntityRef, FloatCC, FunctionBuilder, FunctionBuilderContext, InstBuilder,
        IntCC, MemFlags, Signature, StackSlotData, StackSlotKind, Type, Value,
        isa::TargetFrontendConfig,
        settings::{self, Flags},
        types,
    },
};

use crate::{
    semantics::{
        self,
        resolved_ast::{ExprRangeRef, ExprRef, NodeRef, ResolvedASTNode, TypedExpressionNode},
        resolver::{ResolvedAST, ResolverContext},
        symtab::{FunctionIdx, ObjectIdx, ObjectRangeRef, Scope, SymbolTable},
    },
    types::{BasicType, CType, FunctionType, TypeQualifier},
};

impl From<BasicType> for Type {
    fn from(value: BasicType) -> Self {
        match value {
            BasicType::Char => types::I8,
            BasicType::UChar => types::I8,
            BasicType::Short => types::I16,
            BasicType::UShort => types::I16,
            BasicType::Int => types::I32,
            BasicType::UInt => types::I32,
            BasicType::Long => types::I64,
            BasicType::ULong => types::I64,
            BasicType::Float => types::F32,
            BasicType::Double => types::F64,
        }
    }
}

// TODO: support static local variables
// We place every object onto the stack
// using Cranelift's def_var, use_var, ... is not suitable
// because can take address of any variable
// a mem2reg pass could make this more efficient
struct Frame {
    object_range: ObjectRangeRef,
    stack_slots: Vec<StackSlot>,
}

impl Frame {
    fn contains(&self, object_idx: ObjectIdx) -> bool {
        self.object_range.0 as usize <= object_idx.get_inner()
            && object_idx.get_inner() < self.object_range.1 as usize
    }

    fn get_object_stack_slot(&self, object_idx: ObjectIdx) -> StackSlot {
        let idx = object_idx.get_inner() - self.object_range.0 as usize;
        self.stack_slots[idx]
    }

    fn value(&self, ins: FuncInstBuilder, value_type: Type, object_idx: ObjectIdx) -> Value {
        let stack_slot = self.stack_slots[object_idx.get_inner() - self.object_range.0 as usize];
        ins.stack_load(value_type, stack_slot, 0)
    }
}

enum StackOrMemory {
    Stack(StackSlot),
    Memory(Value),
}

struct CraneliftBackend {
    object: ObjectModule,
    data: Vec<DataId>,
    functions: Vec<FuncId>,

    // these are used for declare_data_in_func, declare_func_in_func
    // and get cleared after each function is built
    function_refs: Vec<FuncRef>,

    // these are used for function declarations
    function_signatures: Vec<Signature>,
}

struct LowerFunctionContext {
    break_target: Option<Block>,
    continue_target: Option<Block>,
    goto_target: Vec<Block>,

    switch_cases: Vec<Block>,
    current_case: usize,

    // mapping of exprs to Values,
    // as well as the contiguous range of exprs that are part of this function
    expr_range: ExprRangeRef,
    expr_values: Vec<Value>,
}

impl LowerFunctionContext {
    fn expr_value(&self, expr_ref: ExprRef) -> Value {
        self.expr_values[expr_ref.get_inner() - self.expr_range.0 as usize]
    }

    fn set_expr_value(&mut self, expr_ref: ExprRef, value: Value) {
        self.expr_values[expr_ref.get_inner() - self.expr_range.0 as usize] = value;
    }
}

impl CraneliftBackend {
    fn pointer_type(&self) -> Type {
        self.object.target_config().pointer_type()
    }

    pub(crate) fn new(object_name: &str) -> Self {
        let isa_builder =
            cranelift::native::builder_with_options(true).expect("failed to make isa builder");
        let flags_builder = settings::builder();
        let isa = isa_builder
            .finish(Flags::new(flags_builder))
            .expect("failed to make isa");
        let object_builder = cranelift::object::ObjectBuilder::new(
            isa,
            object_name,
            cranelift::module::default_libcall_names(),
        )
        .expect("failed to make builder");

        let object = ObjectModule::new(object_builder);

        CraneliftBackend {
            object,
            data: Vec::new(),
            functions: Vec::new(),

            function_refs: Vec::new(),

            function_signatures: Vec::new(),
        }
    }

    pub(crate) fn process_global_symbols(&mut self, symtab: &SymbolTable) {
        let global_objects = symtab.global_objects();
        let global_object_names = symtab.global_object_names();

        for (global_object, global_object_name) in
            std::iter::zip(global_objects, global_object_names)
        {
            let linkage = match global_object.linkage {
                semantics::symtab::Linkage::External => cranelift::module::Linkage::Export,
                semantics::symtab::Linkage::Internal => cranelift::module::Linkage::Local,
                semantics::symtab::Linkage::None => {
                    panic!("top-level decls should have defined linkage")
                }
            };

            let writable = !global_object
                .object_type
                .qualifier()
                .contains(TypeQualifier::Const);

            let object_id = self
                .object
                .declare_data(global_object_name, linkage, writable, false)
                .expect("failed to declare data");

            // TODO: support initializer
            let mut zero_init = DataDescription::new();
            let bytes = match &global_object.object_type {
                CType::BasicType { basic_type, .. } => basic_type.bytes(),
                CType::PointerType { .. } => self.pointer_type().bytes(),
                _ => todo!(),
            };
            zero_init.define_zeroinit(bytes as usize);

            self.object
                .define_data(object_id, &zero_init)
                .expect("failed to define data");

            self.data.push(object_id);
        }

        let functions = symtab.functions();
        let function_names = symtab.function_names();
        for fn_type in symtab.function_types() {
            let signature = self.lower_function_signature(fn_type);
            self.function_signatures.push(signature);
        }

        for (function, function_name) in std::iter::zip(functions, function_names) {
            let linkage = if function.internal_linkage {
                cranelift::module::Linkage::Local
            } else {
                if function.defined {
                    cranelift::module::Linkage::Export
                } else {
                    cranelift::module::Linkage::Import
                }
            };

            let signature = &self.function_signatures[function.function_type];

            let fn_id = self
                .object
                .declare_function(&function_name, linkage, &signature)
                .expect("failed to declare function");

            self.functions.push(fn_id);
        }
    }

    fn lower_function_signature(&self, fn_type: &FunctionType) -> Signature {
        let mut signature = self.object.make_signature();

        // TODO: non-scalar return / parameter types
        for (_, param_type) in &fn_type.parameter_types {
            match param_type {
                crate::types::CType::BasicType { basic_type, .. } => {
                    let cranelift_type = (*basic_type).into();
                    signature.params.push(AbiParam::new(cranelift_type));
                }
                crate::types::CType::PointerType { .. } => {
                    signature.params.push(AbiParam::new(self.pointer_type()));
                }
                _ => todo!("support non-scalar parameter types"),
            }
        }

        match fn_type.return_type {
            crate::types::CType::BasicType { basic_type, .. } => {
                let cranelift_type = basic_type.into();
                signature.returns.push(AbiParam::new(cranelift_type));
            }
            crate::types::CType::PointerType { .. } => {
                signature.returns.push(AbiParam::new(self.pointer_type()));
            }
            crate::types::CType::Void { .. } => (),
            _ => todo!("support non-scalar return types"),
        }

        dbg!(&signature);
        signature
    }

    pub(crate) fn lower_translation_unit(
        &mut self,
        resolved_ast: &ResolvedAST,
        function_builder_ctx: &mut FunctionBuilderContext,
    ) {
        let root = &resolved_ast.nodes[0];

        match root {
            ResolvedASTNode::TranslationUnit { children } => {
                for (fn_defn_idx, fn_defn_ref_idx) in (children.0..children.1).enumerate() {
                    let fn_defn_ref = resolved_ast.ast_indices[fn_defn_ref_idx as usize];

                    self.lower_function(
                        fn_defn_idx,
                        resolved_ast,
                        fn_defn_ref,
                        function_builder_ctx,
                    );
                }
            }
            _ => panic!("corrupted AST"),
        }
    }

    fn lower_function(
        &mut self,
        function_defn_idx: usize,
        resolved_ast: &ResolvedAST,
        function_node_ref: NodeRef,
        function_builder_ctx: &mut FunctionBuilderContext,
    ) {
        let (cranelift_id, cake_id, body) = match &resolved_ast.nodes[function_node_ref.0 as usize]
        {
            ResolvedASTNode::FunctionDefinition {
                parent,
                body,
                symbol_idx,
            } => {
                let cranelift_id = self.functions[*symbol_idx];
                (cranelift_id, symbol_idx, *body)
            }
            _ => panic!("bad node reference"),
        };

        // get function signature from earlier declaration (process_global_symbols)
        let declared_signature = &self
            .object
            .declarations()
            .get_function_decl(cranelift_id)
            .signature;
        let is_void = declared_signature.returns.is_empty();

        let mut ctx = self.object.make_context();
        ctx.func.signature = declared_signature.clone();
        ctx.func.collect_debug_info();

        self.function_refs.clear();
        for func_id in &self.functions {
            let function_ref = self.object.declare_func_in_func(*func_id, &mut ctx.func);
            self.function_refs.push(function_ref);
        }

        let mut fn_builder = FunctionBuilder::new(&mut ctx.func, function_builder_ctx);
        let object_range = resolved_ast.symtab.function_object_range(*cake_id);
        let object_frame = self.create_frame(&mut fn_builder, &resolved_ast.symtab, object_range);

        let entry = fn_builder.create_block();
        fn_builder.append_block_params_for_function_params(entry);
        fn_builder.switch_to_block(entry);
        fn_builder.seal_block(entry);

        // move ssa parameters into stack slots
        let function_parameter_objects = resolved_ast.symtab.function_parameter_range(*cake_id);
        let enumerate = function_parameter_objects.into_iter().enumerate();
        for (i, param_idx) in enumerate {
            let stack_slot = object_frame.get_object_stack_slot(param_idx);
            let cranelift_params = fn_builder.block_params(entry);
            let cranelift_param = cranelift_params[i];

            fn_builder.ins().stack_store(cranelift_param, stack_slot, 0);
        }

        let expr_range = resolved_ast.function_expr_ranges[function_defn_idx];
        let num_exprs = (expr_range.1 - expr_range.0) as usize;
        let mut expr_values = Vec::new();
        expr_values.resize_with(num_exprs, || Value::new(0));
        let mut lower_fn_ctx = LowerFunctionContext {
            break_target: None,
            continue_target: None,
            goto_target: Vec::new(),

            switch_cases: Vec::new(),
            current_case: 0,

            expr_values,
            expr_range,
        };

        self.lower_statement(
            &mut fn_builder,
            resolved_ast,
            body,
            &object_frame,
            &mut lower_fn_ctx,
        );
        if is_void {
            fn_builder.ins().return_(&[]);
        }

        fn_builder.finalize();
        let display = ctx.func.display();
        print!("{display}");
        ctx.verify(self.object.isa()).expect("verification failed");

        self.object
            .define_function(cranelift_id, &mut ctx)
            .expect("failed to compile function");
    }

    fn create_frame(
        &self,
        fn_builder: &mut FunctionBuilder,
        symtab: &SymbolTable,
        object_range: ObjectRangeRef,
    ) -> Frame {
        let locals = symtab.object_range(object_range);
        let mut stack_slots = Vec::new();

        for local_var in locals {
            let (size, align) = match &local_var.object_type {
                CType::BasicType { basic_type, .. } => {
                    (basic_type.bytes(), basic_type.align().ilog2() as u8)
                }
                CType::PointerType { .. } => (
                    self.pointer_type().bytes(),
                    self.pointer_type().bytes().ilog2() as u8,
                ),
                CType::ArrayType {
                    size, element_type, ..
                } => match element_type.as_ref() {
                    CType::BasicType { basic_type, .. } => {
                        (basic_type.bytes() * size, basic_type.align().ilog2() as u8)
                    }
                    CType::PointerType { .. } => (
                        self.pointer_type().bytes(),
                        self.pointer_type().bytes().ilog2() as u8,
                    ),
                    _ => todo!(),
                },
                _ => todo!(),
            };

            let stack_slot_data = StackSlotData {
                kind: StackSlotKind::ExplicitSlot,
                size,
                align_shift: align,
            };
            let stack_slot = fn_builder.create_sized_stack_slot(stack_slot_data);
            stack_slots.push(stack_slot);
        }

        Frame {
            object_range,
            stack_slots,
        }
    }

    fn lower_statement(
        &mut self,
        fn_builder: &mut FunctionBuilder,
        resolved_ast: &ResolvedAST,
        statement_ref: NodeRef,

        object_frame: &Frame,
        lower_fn_ctx: &mut LowerFunctionContext,
    ) {
        match &resolved_ast.nodes[statement_ref.0 as usize] {
            crate::semantics::resolved_ast::ResolvedASTNode::ExpressionStatement {
                parent,
                expr,
            } => {
                self.lower_expr(fn_builder, resolved_ast, *expr, object_frame, lower_fn_ctx);
            }
            crate::semantics::resolved_ast::ResolvedASTNode::ReturnStatement {
                parent,
                return_value,
            } => {
                let returns: &[Value] = if let Some(return_value) = return_value {
                    let cranelift_value = self.lower_expr(
                        fn_builder,
                        resolved_ast,
                        *return_value,
                        object_frame,
                        lower_fn_ctx,
                    );
                    dbg!(cranelift_value);
                    &[cranelift_value]
                } else {
                    &[]
                };

                fn_builder.ins().return_(returns);

                // create block with no predecessors, represents that `return` diverges
                let after_return = fn_builder.create_block();
                fn_builder.seal_block(after_return);
                fn_builder.switch_to_block(after_return);
            }

            ResolvedASTNode::IfStatement {
                parent,
                condition,
                taken,
                not_taken,
            } => {
                let taken_block = fn_builder.create_block();
                let not_taken_block = fn_builder.create_block();
                let postdominator = fn_builder.create_block();

                let condition = self.lower_expr(
                    fn_builder,
                    resolved_ast,
                    *condition,
                    object_frame,
                    lower_fn_ctx,
                );

                fn_builder
                    .ins()
                    .brif(condition, taken_block, &[], not_taken_block, &[]);
                fn_builder.seal_block(taken_block);
                fn_builder.seal_block(not_taken_block);

                fn_builder.switch_to_block(taken_block);
                self.lower_statement(fn_builder, resolved_ast, *taken, object_frame, lower_fn_ctx);
                fn_builder.ins().jump(postdominator, &[]);

                fn_builder.switch_to_block(not_taken_block);
                if let Some(not_taken_node) = *not_taken {
                    self.lower_statement(
                        fn_builder,
                        resolved_ast,
                        not_taken_node,
                        object_frame,
                        lower_fn_ctx,
                    );
                }

                fn_builder.ins().jump(postdominator, &[]);

                fn_builder.seal_block(postdominator);
                fn_builder.switch_to_block(postdominator);
            }

            ResolvedASTNode::CompoundStatement { parent, stmts } => {
                for stmt_ref_idx in stmts.0..stmts.1 {
                    let stmt_ref = resolved_ast.ast_indices[stmt_ref_idx as usize];
                    self.lower_statement(
                        fn_builder,
                        resolved_ast,
                        stmt_ref,
                        &object_frame,
                        lower_fn_ctx,
                    );
                }
            }

            while_stmt @ ResolvedASTNode::WhileStatement {
                parent,
                condition,
                body,
            }
            | while_stmt @ ResolvedASTNode::DoWhileStatement {
                parent,
                condition,
                body,
            } => {
                let body_block = fn_builder.create_block();
                let loop_continuation = fn_builder.create_block();
                let after_block = fn_builder.create_block();

                dbg!(body_block);
                dbg!(loop_continuation);
                dbg!(after_block);

                if matches!(while_stmt, ResolvedASTNode::WhileStatement { .. }) {
                    let controlling_value = self.lower_expr(
                        fn_builder,
                        resolved_ast,
                        *condition,
                        object_frame,
                        lower_fn_ctx,
                    );

                    fn_builder
                        .ins()
                        .brif(controlling_value, body_block, &[], after_block, &[]);
                } else {
                    fn_builder.ins().jump(body_block, &[]);
                }

                fn_builder.switch_to_block(body_block);

                let old_continue_target = lower_fn_ctx.continue_target;
                let old_break_target = lower_fn_ctx.break_target;
                lower_fn_ctx.continue_target = Some(loop_continuation);
                lower_fn_ctx.break_target = Some(after_block);
                self.lower_statement(fn_builder, resolved_ast, *body, object_frame, lower_fn_ctx);
                lower_fn_ctx.continue_target = old_continue_target;
                lower_fn_ctx.break_target = old_break_target;

                fn_builder.ins().jump(loop_continuation, &[]);
                fn_builder.seal_block(loop_continuation);
                fn_builder.switch_to_block(loop_continuation);

                let controlling_value = self.lower_expr(
                    fn_builder,
                    resolved_ast,
                    *condition,
                    object_frame,
                    lower_fn_ctx,
                );
                fn_builder
                    .ins()
                    .brif(controlling_value, body_block, &[], after_block, &[]);

                fn_builder.seal_block(body_block);
                fn_builder.seal_block(after_block);
                fn_builder.switch_to_block(after_block);
            }

            ResolvedASTNode::ForStatement {
                parent,
                init,
                condition,
                post_body,
                body,
            } => {
                match init {
                    Some(init_expr) => {
                        _ = self.lower_expr(
                            fn_builder,
                            resolved_ast,
                            *init_expr,
                            object_frame,
                            lower_fn_ctx,
                        );
                    }
                    None => (),
                }

                let loop_start = fn_builder.create_block();
                let loop_body = fn_builder.create_block();
                let loop_end = fn_builder.create_block();
                let loop_continuation = fn_builder.create_block();

                fn_builder.ins().jump(loop_start, &[]);
                fn_builder.switch_to_block(loop_start);

                match condition {
                    Some(condition_expr) => {
                        let condition_value = self.lower_expr(
                            fn_builder,
                            resolved_ast,
                            *condition_expr,
                            object_frame,
                            lower_fn_ctx,
                        );

                        fn_builder
                            .ins()
                            .brif(condition_value, loop_body, &[], loop_end, &[]);
                        fn_builder.seal_block(loop_body);
                    }
                    None => {
                        fn_builder.ins().jump(loop_body, &[]);
                        fn_builder.seal_block(loop_body);
                    }
                }

                fn_builder.switch_to_block(loop_body);

                let old_break_target = lower_fn_ctx.break_target;
                let old_continue_target = lower_fn_ctx.continue_target;
                lower_fn_ctx.break_target = Some(loop_end);
                lower_fn_ctx.continue_target = Some(loop_continuation);
                self.lower_statement(fn_builder, resolved_ast, *body, object_frame, lower_fn_ctx);
                lower_fn_ctx.break_target = old_break_target;
                lower_fn_ctx.continue_target = old_continue_target;

                fn_builder.ins().jump(loop_continuation, &[]);
                fn_builder.seal_block(loop_continuation);
                fn_builder.seal_block(loop_end);
                fn_builder.switch_to_block(loop_continuation);

                match post_body {
                    Some(post_body_expr) => {
                        _ = self.lower_expr(
                            fn_builder,
                            resolved_ast,
                            *post_body_expr,
                            object_frame,
                            lower_fn_ctx,
                        );
                    }
                    None => (),
                }

                fn_builder.ins().jump(loop_start, &[]);
                fn_builder.seal_block(loop_start);

                fn_builder.switch_to_block(loop_end);
            }

            ResolvedASTNode::BreakStatement { .. } => {
                let break_target = lower_fn_ctx
                    .break_target
                    .expect("should be required by resolver");
                fn_builder.ins().jump(break_target, &[]);

                let unreachable_block = fn_builder.create_block();
                fn_builder.seal_block(unreachable_block);
                fn_builder.switch_to_block(unreachable_block);
            }

            ResolvedASTNode::ContinueStatement { .. } => {
                let continue_target = lower_fn_ctx
                    .continue_target
                    .expect("should be required by resolver");
                fn_builder.ins().jump(continue_target, &[]);

                let unreachable_block = fn_builder.create_block();
                fn_builder.seal_block(unreachable_block);
                fn_builder.switch_to_block(unreachable_block);
            }

            ResolvedASTNode::NullStatement { parent } => (),

            ResolvedASTNode::SwitchStatement {
                parent,
                controlling_expr,
                body,
                context,
            } => {
                let controlling_expr_value = self.lower_expr(
                    fn_builder,
                    resolved_ast,
                    *controlling_expr,
                    object_frame,
                    lower_fn_ctx,
                );

                let switch_context = match &resolved_ast.resolve_contexts[context.0 as usize] {
                    ResolverContext::Switch(switch_ctx) => switch_ctx,
                    _ => unreachable!("corrupted resolve context table"),
                };

                let switch_body = fn_builder.create_block();
                let after_switch = fn_builder.create_block();

                fn_builder.seal_block(switch_body);

                let num_targets = if switch_context.default_idx.is_some() {
                    switch_context.case_values.len() + 1
                } else {
                    switch_context.case_values.len()
                };

                lower_fn_ctx
                    .switch_cases
                    .resize_with(num_targets, || fn_builder.create_block());
                lower_fn_ctx.current_case = 0;

                let mut cranelift_switch = cranelift::frontend::Switch::new();
                let fallback = if let Some(idx) = switch_context.default_idx {
                    lower_fn_ctx.switch_cases[idx]
                } else {
                    after_switch
                };

                for (i, case_value) in switch_context.case_values.iter().copied().enumerate() {
                    let case_value = match case_value {
                        crate::parser::ast::Constant::Int(i) => i as u128,
                        crate::parser::ast::Constant::LongInt(i) => i as u128,
                        crate::parser::ast::Constant::UInt(i) => i as u128,
                        crate::parser::ast::Constant::ULongInt(i) => i as u128,
                        crate::parser::ast::Constant::Float(_) => unreachable!(),
                        crate::parser::ast::Constant::Double(_) => unreachable!(),
                    };

                    let block_idx = if let Some(idx) = switch_context.default_idx {
                        if i >= idx { i + 1 } else { i }
                    } else {
                        i
                    };
                    cranelift_switch.set_entry(case_value, lower_fn_ctx.switch_cases[block_idx]);
                }

                cranelift_switch.emit(fn_builder, controlling_expr_value, fallback);
                fn_builder.switch_to_block(switch_body);

                let old_break_target = lower_fn_ctx.break_target;
                lower_fn_ctx.break_target = Some(after_switch);
                self.lower_statement(fn_builder, resolved_ast, *body, object_frame, lower_fn_ctx);
                lower_fn_ctx.break_target = old_break_target;

                for target in lower_fn_ctx.switch_cases.iter().copied() {
                    fn_builder.seal_block(target);
                }

                fn_builder.ins().jump(after_switch, &[]);
                fn_builder.seal_block(after_switch);
                fn_builder.switch_to_block(after_switch);

                lower_fn_ctx.switch_cases.clear();
                lower_fn_ctx.current_case = 0;
            }

            ResolvedASTNode::CaseLabel {
                parent,
                labelee,
                case_index,
            } => {
                // fallthrough
                let my_block = lower_fn_ctx.switch_cases[lower_fn_ctx.current_case];
                lower_fn_ctx.current_case += 1;
                fn_builder.ins().jump(my_block, &[]);
                fn_builder.switch_to_block(my_block);
                self.lower_statement(
                    fn_builder,
                    resolved_ast,
                    *labelee,
                    object_frame,
                    lower_fn_ctx,
                );
            }

            ResolvedASTNode::DefaultLabel { parent, labelee } => {
                let my_block = lower_fn_ctx.switch_cases[lower_fn_ctx.current_case];
                fn_builder.ins().jump(my_block, &[]);
                fn_builder.switch_to_block(my_block);
                self.lower_statement(
                    fn_builder,
                    resolved_ast,
                    *labelee,
                    object_frame,
                    lower_fn_ctx,
                );
            }

            statement => {
                dbg!(statement);
                todo!("other statement types")
            }
        }
    }

    fn lower_expr(
        &mut self,
        fn_builder: &mut FunctionBuilder,
        resolved_ast: &ResolvedAST,
        expr_ref: ExprRef,

        object_frame: &Frame,
        lower_fn_ctx: &mut LowerFunctionContext,
    ) -> Value {
        let expr_value = match &resolved_ast.exprs[expr_ref] {
            crate::semantics::resolved_ast::TypedExpressionNode::Multiply(
                result_type,
                lhs,
                rhs,
            ) => {
                let lhs_value =
                    self.lower_expr(fn_builder, resolved_ast, *lhs, object_frame, lower_fn_ctx);
                let rhs_value =
                    self.lower_expr(fn_builder, resolved_ast, *rhs, object_frame, lower_fn_ctx);

                let result_type = Self::get_basic_type(result_type);

                let product = if result_type.is_fp() {
                    fn_builder.ins().fmul(lhs_value, rhs_value)
                } else if result_type.is_signed() {
                    fn_builder.ins().smul_overflow(lhs_value, rhs_value).0
                } else {
                    fn_builder.ins().umul_overflow(lhs_value, rhs_value).0
                };

                product
            }
            crate::semantics::resolved_ast::TypedExpressionNode::Divide(result_type, lhs, rhs) => {
                let lhs_value =
                    self.lower_expr(fn_builder, resolved_ast, *lhs, object_frame, lower_fn_ctx);
                let rhs_value =
                    self.lower_expr(fn_builder, resolved_ast, *rhs, object_frame, lower_fn_ctx);

                let result_type = Self::get_basic_type(result_type);

                let quotient = if result_type.is_fp() {
                    fn_builder.ins().fdiv(lhs_value, rhs_value)
                } else if result_type.is_signed() {
                    fn_builder.ins().sdiv(lhs_value, rhs_value)
                } else {
                    fn_builder.ins().udiv(lhs_value, rhs_value)
                };

                quotient
            }
            crate::semantics::resolved_ast::TypedExpressionNode::Modulo(result_type, lhs, rhs) => {
                let lhs_value =
                    self.lower_expr(fn_builder, resolved_ast, *lhs, object_frame, lower_fn_ctx);
                let rhs_value =
                    self.lower_expr(fn_builder, resolved_ast, *rhs, object_frame, lower_fn_ctx);

                let result_type = Self::get_basic_type(result_type);

                let rem = if result_type.is_signed() {
                    fn_builder.ins().srem(lhs_value, rhs_value)
                } else {
                    fn_builder.ins().urem(lhs_value, rhs_value)
                };

                rem
            }
            crate::semantics::resolved_ast::TypedExpressionNode::Add(result_type, lhs, rhs) => {
                let lhs_value =
                    self.lower_expr(fn_builder, resolved_ast, *lhs, object_frame, lower_fn_ctx);
                let rhs_value =
                    self.lower_expr(fn_builder, resolved_ast, *rhs, object_frame, lower_fn_ctx);

                let result_type = Self::get_basic_type(result_type);

                let sum = if result_type.is_fp() {
                    fn_builder.ins().fadd(lhs_value, rhs_value)
                } else {
                    fn_builder.ins().iadd(lhs_value, rhs_value)
                };

                sum
            }
            crate::semantics::resolved_ast::TypedExpressionNode::Subtract(
                result_type,
                lhs,
                rhs,
            ) => {
                let lhs_value =
                    self.lower_expr(fn_builder, resolved_ast, *lhs, object_frame, lower_fn_ctx);
                let rhs_value =
                    self.lower_expr(fn_builder, resolved_ast, *rhs, object_frame, lower_fn_ctx);

                let result_type = Self::get_basic_type(result_type);

                let difference = if result_type.is_fp() {
                    fn_builder.ins().fsub(lhs_value, rhs_value)
                } else {
                    fn_builder.ins().isub(lhs_value, rhs_value)
                };

                difference
            }
            crate::semantics::resolved_ast::TypedExpressionNode::Constant(.., constant) => {
                let ins = fn_builder.ins();
                match constant {
                    crate::parser::ast::Constant::Int(int) => ins.iconst(types::I32, *int as i64),
                    crate::parser::ast::Constant::LongInt(int) => ins.iconst(types::I64, *int),
                    crate::parser::ast::Constant::UInt(int) => ins.iconst(types::I32, *int as i64),
                    crate::parser::ast::Constant::ULongInt(int) => {
                        ins.iconst(types::I64, *int as i64)
                    }
                    crate::parser::ast::Constant::Float(float) => ins.f32const(*float),
                    crate::parser::ast::Constant::Double(double) => ins.f64const(*double),
                }
            }

            crate::semantics::resolved_ast::TypedExpressionNode::SimpleAssign(
                result_type,
                lhs,
                rhs,
            ) => {
                // LHS should produce stack slot or global value
                let location =
                    self.lower_lvalue(fn_builder, *lhs, resolved_ast, object_frame, lower_fn_ctx);
                let cranelift_type = match result_type {
                    CType::BasicType { basic_type, .. } => (*basic_type).into(),
                    CType::PointerType { .. } => self.pointer_type(),
                    _ => todo!("support other types"),
                };
                let rhs_value =
                    self.lower_expr(fn_builder, resolved_ast, *rhs, object_frame, lower_fn_ctx);

                match location {
                    StackOrMemory::Stack(stack_slot) => {
                        fn_builder.ins().stack_store(rhs_value, stack_slot, 0)
                    }
                    StackOrMemory::Memory(pointer) => {
                        fn_builder
                            .ins()
                            .store(MemFlags::trusted(), rhs_value, pointer, 0)
                    }
                };
                rhs_value
            }
            crate::semantics::resolved_ast::TypedExpressionNode::DirectFunctionCall(
                result_type,
                function,
                arguments,
            ) => {
                let func_id = self.function_refs[*function];
                let arg_range: Range<usize> = (*arguments).into();
                let mut arg_values = Vec::with_capacity(arg_range.len());
                let arg_range = &resolved_ast.expr_indices[arg_range];
                for arg_expr in arg_range {
                    let arg_value = self.lower_expr(
                        fn_builder,
                        resolved_ast,
                        *arg_expr,
                        object_frame,
                        lower_fn_ctx,
                    );
                    arg_values.push(arg_value);
                }

                let function_symbol = resolved_ast.symtab.get_function(*function);
                let function_type = resolved_ast
                    .symtab
                    .get_function_type(function_symbol.function_type);

                let rvals = if function_type.varargs {
                    // direct function call won't work, since we need to massage the signature
                    let func_ref = self.function_refs[*function];
                    let vararg_fn_ptr = fn_builder.ins().func_addr(self.pointer_type(), func_ref);
                    let mut signature =
                        self.function_signatures[function_symbol.function_type].clone();

                    // mangle the signature to fit call site
                    let formal_param_count = function_type.parameter_types.len();
                    for arg_expr in &arg_range[formal_param_count..] {
                        let arg_type = resolved_ast.exprs[*arg_expr].expr_type();
                        match arg_type {
                            CType::BasicType { basic_type, .. } => {
                                let cranelift_type = (*basic_type).into();
                                signature.params.push(AbiParam::new(cranelift_type))
                            }
                            CType::PointerType { .. } => {
                                signature.params.push(AbiParam::new(self.pointer_type()))
                            }
                            _ => todo!(),
                        }
                    }

                    let sig_ref = fn_builder.import_signature(signature);
                    fn_builder
                        .ins()
                        .call_indirect(sig_ref, vararg_fn_ptr, &arg_values)
                } else {
                    fn_builder.ins().call(func_id, &arg_values)
                };

                if result_type.is_void() {
                    // TODO: fix this. void expressions should be allowed
                    fn_builder.ins().iconst(self.pointer_type(), 0)
                } else {
                    fn_builder.inst_results(rvals)[0]
                }
            }

            TypedExpressionNode::IndirectFunctionCall(result_type, function_expr, arguments) => {
                let func_ptr_value = self.lower_expr(
                    fn_builder,
                    resolved_ast,
                    *function_expr,
                    object_frame,
                    lower_fn_ctx,
                );

                let func_ptr_type = resolved_ast.exprs[*function_expr].expr_type();
                let func_signature = match func_ptr_type {
                    CType::PointerType { pointee_type, .. } => match pointee_type.as_ref() {
                        CType::FunctionTypeRef { symtab_idx } => {
                            &self.function_signatures[*symtab_idx]
                        }
                        _ => unreachable!("resolver should prevent this (TODO: put it in ast?)"),
                    },
                    _ => unreachable!("see above"),
                };
                let func_signature_ref = fn_builder.import_signature(func_signature.clone());

                let arg_range: Range<usize> = (*arguments).into();
                let mut arg_values = Vec::with_capacity(arg_range.len());
                for arg_expr in &resolved_ast.expr_indices[arg_range] {
                    let arg_value = self.lower_expr(
                        fn_builder,
                        resolved_ast,
                        *arg_expr,
                        object_frame,
                        lower_fn_ctx,
                    );
                    arg_values.push(arg_value);
                }
                let rvals =
                    fn_builder
                        .ins()
                        .call_indirect(func_signature_ref, func_ptr_value, &arg_values);
                if result_type.is_void() {
                    // TODO: fix this. void expressions should be allowed
                    fn_builder.ins().iconst(self.pointer_type(), 0)
                } else {
                    fn_builder.inst_results(rvals)[0]
                }
            }

            TypedExpressionNode::AddressOf(_, addressee) => match &resolved_ast.exprs[*addressee] {
                TypedExpressionNode::ObjectIdentifier(_, _) => {
                    let location = self.lower_lvalue(
                        fn_builder,
                        *addressee,
                        resolved_ast,
                        object_frame,
                        lower_fn_ctx,
                    );

                    match location {
                        StackOrMemory::Stack(stack_slot) => {
                            fn_builder
                                .ins()
                                .stack_addr(self.pointer_type(), stack_slot, 0)
                        }
                        StackOrMemory::Memory(value) => value,
                    }
                }
                TypedExpressionNode::FunctionIdentifier(_, fn_idx) => fn_builder
                    .ins()
                    .func_addr(self.pointer_type(), self.function_refs[*fn_idx]),
                _ => todo!(),
            },
            crate::semantics::resolved_ast::TypedExpressionNode::ObjectIdentifier(
                object_type,
                object_idx,
            ) => {
                let location = self.lower_lvalue(
                    fn_builder,
                    expr_ref,
                    resolved_ast,
                    object_frame,
                    lower_fn_ctx,
                );

                let cranelift_type = match object_type {
                    CType::BasicType { basic_type, .. } => (*basic_type).into(),
                    CType::PointerType { .. } => self.pointer_type(),
                    _ => todo!("other types"),
                };

                match location {
                    StackOrMemory::Stack(stack_slot) => {
                        fn_builder.ins().stack_load(cranelift_type, stack_slot, 0)
                    }
                    StackOrMemory::Memory(value) => {
                        fn_builder
                            .ins()
                            .load(cranelift_type, MemFlags::trusted(), value, 0)
                    }
                }
            }

            TypedExpressionNode::StringLiteral(_, string) => {
                let cranelift_id = self
                    .object
                    .declare_anonymous_data(false, false)
                    .expect("failed to declare string literal");
                let mut str_contents = DataDescription::new();
                let c_string = CString::from_str(&string).expect("unable to convert to C string");
                str_contents.define(Box::from(c_string.as_bytes_with_nul()));
                self.object
                    .define_data(cranelift_id, &str_contents)
                    .expect("failed to define string literal");
                let str_ptr = self
                    .object
                    .declare_data_in_func(cranelift_id, fn_builder.func);
                fn_builder.ins().global_value(self.pointer_type(), str_ptr)
            }

            TypedExpressionNode::PointerAdd(pointer_type, ptr_ref, int_ref) => {
                let size = match pointer_type {
                    CType::PointerType { pointee_type, .. } => match pointee_type.as_ref() {
                        CType::BasicType { basic_type, .. } => basic_type.bytes(),
                        CType::PointerType { .. } => self.pointer_type().bytes(),
                        _ => todo!(),
                    },
                    _ => unreachable!("corrupted resolvedastnode"),
                };
                let base = self.lower_expr(
                    fn_builder,
                    resolved_ast,
                    *ptr_ref,
                    object_frame,
                    lower_fn_ctx,
                );
                let offset = self.lower_expr(
                    fn_builder,
                    resolved_ast,
                    *int_ref,
                    object_frame,
                    lower_fn_ctx,
                );

                let needs_extension = fn_builder.func.dfg.value_type(offset) != self.pointer_type();
                let offset = if needs_extension {
                    fn_builder.ins().sextend(self.pointer_type(), offset)
                } else {
                    offset
                };

                let offset = fn_builder.ins().imul_imm(offset, size as i64);
                fn_builder.ins().iadd(base, offset)
            }

            TypedExpressionNode::ArrayDecay(_, object_idx) => {
                let location = self.lower_lvalue(
                    fn_builder,
                    expr_ref,
                    resolved_ast,
                    object_frame,
                    lower_fn_ctx,
                );
                match location {
                    StackOrMemory::Stack(stack_slot) => {
                        fn_builder
                            .ins()
                            .stack_addr(self.pointer_type(), stack_slot, 0)
                    }
                    StackOrMemory::Memory(value) => value,
                }
            }

            TypedExpressionNode::Cast(dest_type, expr, source_type) => {
                // bruh
                let (signed, cranelift_dest_type) = match dest_type {
                    CType::BasicType { basic_type, .. } => {
                        (basic_type.is_signed(), (*basic_type).into())
                    }
                    CType::PointerType { .. } => (false, self.pointer_type()),
                    _ => todo!(),
                };

                let cranelift_src_type = match source_type {
                    CType::BasicType { basic_type, .. } => (*basic_type).into(),
                    CType::PointerType { .. } => self.pointer_type(),
                    _ => todo!(),
                };

                let src =
                    self.lower_expr(fn_builder, resolved_ast, *expr, object_frame, lower_fn_ctx);

                if cranelift_dest_type.is_int() && cranelift_src_type.is_float() {
                    if signed {
                        fn_builder.ins().fcvt_to_sint(cranelift_dest_type, src)
                    } else {
                        fn_builder.ins().fcvt_to_uint(cranelift_dest_type, src)
                    }
                } else if cranelift_dest_type.is_float() && cranelift_src_type.is_int() {
                    if signed {
                        fn_builder.ins().fcvt_from_sint(cranelift_dest_type, src)
                    } else {
                        fn_builder.ins().fcvt_from_uint(cranelift_dest_type, src)
                    }
                } else if cranelift_dest_type.is_float() && cranelift_src_type.is_float() {
                    if cranelift_dest_type.bits() < cranelift_src_type.bits() {
                        fn_builder.ins().fdemote(cranelift_dest_type, src)
                    } else if cranelift_dest_type.bits() > cranelift_src_type.bits() {
                        fn_builder.ins().fpromote(cranelift_dest_type, src)
                    } else {
                        src
                    }
                } else if cranelift_dest_type.is_int() && cranelift_src_type.is_int() {
                    if cranelift_dest_type.bits() < cranelift_src_type.bits() {
                        fn_builder.ins().ireduce(cranelift_dest_type, src)
                    } else if cranelift_dest_type.bits() > cranelift_src_type.bits() {
                        if signed {
                            fn_builder.ins().sextend(cranelift_dest_type, src)
                        } else {
                            fn_builder.ins().uextend(cranelift_dest_type, src)
                        }
                    } else {
                        src
                    }
                } else {
                    unreachable!("this should not happen")
                }
            }

            TypedExpressionNode::Dereference(value_type, pointer) => {
                let pointer_value = self.lower_expr(
                    fn_builder,
                    resolved_ast,
                    *pointer,
                    object_frame,
                    lower_fn_ctx,
                );
                let cranelift_type = match value_type {
                    CType::BasicType { basic_type, .. } => (*basic_type).into(),
                    CType::PointerType { .. } => self.pointer_type(),
                    _ => todo!(),
                };
                fn_builder
                    .ins()
                    .load(cranelift_type, MemFlags::trusted(), pointer_value, 0)
            }

            TypedExpressionNode::LogicalAnd(_, lhs, rhs) => {
                let lhs_value =
                    self.lower_expr(fn_builder, resolved_ast, *lhs, object_frame, lower_fn_ctx);

                let rhs_block = fn_builder.create_block();
                let after_block = fn_builder.create_block();
                fn_builder.append_block_param(after_block, BasicType::Int.into());

                let const_zero = fn_builder.ins().iconst(BasicType::Int.into(), 0);
                let const_one = fn_builder.ins().iconst(BasicType::Int.into(), 1);

                // short circuit
                fn_builder
                    .ins()
                    .brif(lhs_value, rhs_block, &[], after_block, &[const_zero.into()]);
                fn_builder.seal_block(rhs_block);
                fn_builder.switch_to_block(rhs_block);

                let rhs_value =
                    self.lower_expr(fn_builder, resolved_ast, *rhs, object_frame, lower_fn_ctx);
                let zero_or_one = fn_builder.ins().select(rhs_value, const_one, const_zero);
                fn_builder.ins().jump(after_block, &[zero_or_one.into()]);

                fn_builder.seal_block(after_block);
                fn_builder.switch_to_block(after_block);
                fn_builder.block_params(after_block)[0]
            }

            TypedExpressionNode::LogicalOr(_, lhs, rhs) => {
                let lhs_value =
                    self.lower_expr(fn_builder, resolved_ast, *lhs, object_frame, lower_fn_ctx);

                let rhs_block = fn_builder.create_block();
                let after_block = fn_builder.create_block();
                fn_builder.append_block_param(after_block, BasicType::Int.into());

                let const_zero = fn_builder.ins().iconst(BasicType::Int.into(), 0);
                let const_one = fn_builder.ins().iconst(BasicType::Int.into(), 1);

                // short circuit
                fn_builder
                    .ins()
                    .brif(lhs_value, after_block, &[const_one.into()], rhs_block, &[]);
                fn_builder.seal_block(rhs_block);
                fn_builder.switch_to_block(rhs_block);

                let rhs_value =
                    self.lower_expr(fn_builder, resolved_ast, *rhs, object_frame, lower_fn_ctx);
                let zero_or_one = fn_builder.ins().select(rhs_value, const_one, const_zero);
                fn_builder.ins().jump(after_block, &[zero_or_one.into()]);

                fn_builder.seal_block(after_block);
                fn_builder.switch_to_block(after_block);
                fn_builder.block_params(after_block)[0]
            }

            TypedExpressionNode::CommaExpr(_, expr_range) => {
                let expr_range: Range<usize> = (*expr_range).into();
                let mut subexpr_value = None;
                for subexpr_ref in &resolved_ast.expr_indices[expr_range] {
                    subexpr_value = Some(self.lower_expr(
                        fn_builder,
                        resolved_ast,
                        *subexpr_ref,
                        object_frame,
                        lower_fn_ctx,
                    ));
                }

                subexpr_value.expect("grammar prohibits empty comma expression")
            }

            TypedExpressionNode::GreaterThan(_, a, b) => {
                let common_type = resolved_ast.exprs[*a].expr_type();
                let a = self.lower_expr(fn_builder, resolved_ast, *a, object_frame, lower_fn_ctx);
                let b = self.lower_expr(fn_builder, resolved_ast, *b, object_frame, lower_fn_ctx);

                compare_op(
                    fn_builder,
                    a,
                    b,
                    common_type,
                    IntCC::SignedGreaterThan,
                    IntCC::UnsignedGreaterThan,
                    FloatCC::GreaterThan,
                )
            }

            TypedExpressionNode::GreaterThanOrEqual(_, a, b) => {
                let common_type = resolved_ast.exprs[*a].expr_type();
                let a = self.lower_expr(fn_builder, resolved_ast, *a, object_frame, lower_fn_ctx);
                let b = self.lower_expr(fn_builder, resolved_ast, *b, object_frame, lower_fn_ctx);

                compare_op(
                    fn_builder,
                    a,
                    b,
                    common_type,
                    IntCC::SignedGreaterThanOrEqual,
                    IntCC::UnsignedGreaterThanOrEqual,
                    FloatCC::GreaterThanOrEqual,
                )
            }

            TypedExpressionNode::LessThan(_, a, b) => {
                let common_type = resolved_ast.exprs[*a].expr_type();
                let a = self.lower_expr(fn_builder, resolved_ast, *a, object_frame, lower_fn_ctx);
                let b = self.lower_expr(fn_builder, resolved_ast, *b, object_frame, lower_fn_ctx);

                compare_op(
                    fn_builder,
                    a,
                    b,
                    common_type,
                    IntCC::SignedLessThan,
                    IntCC::UnsignedLessThan,
                    FloatCC::LessThan,
                )
            }

            TypedExpressionNode::LessThanOrEqual(_, a, b) => {
                let common_type = resolved_ast.exprs[*a].expr_type();
                let a = self.lower_expr(fn_builder, resolved_ast, *a, object_frame, lower_fn_ctx);
                let b = self.lower_expr(fn_builder, resolved_ast, *b, object_frame, lower_fn_ctx);

                compare_op(
                    fn_builder,
                    a,
                    b,
                    common_type,
                    IntCC::SignedLessThanOrEqual,
                    IntCC::UnsignedLessThanOrEqual,
                    FloatCC::LessThanOrEqual,
                )
            }

            TypedExpressionNode::Equal(_, a, b) => {
                let common_type = resolved_ast.exprs[*a].expr_type();
                let a = self.lower_expr(fn_builder, resolved_ast, *a, object_frame, lower_fn_ctx);
                let b = self.lower_expr(fn_builder, resolved_ast, *b, object_frame, lower_fn_ctx);

                compare_op(
                    fn_builder,
                    a,
                    b,
                    common_type,
                    IntCC::Equal,
                    IntCC::Equal,
                    FloatCC::Equal,
                )
            }

            TypedExpressionNode::NotEqual(_, a, b) => {
                let common_type = resolved_ast.exprs[*a].expr_type();
                let a = self.lower_expr(fn_builder, resolved_ast, *a, object_frame, lower_fn_ctx);
                let b = self.lower_expr(fn_builder, resolved_ast, *b, object_frame, lower_fn_ctx);

                compare_op(
                    fn_builder,
                    a,
                    b,
                    common_type,
                    IntCC::NotEqual,
                    IntCC::NotEqual,
                    FloatCC::NotEqual,
                )
            }

            TypedExpressionNode::AugmentedAssign(_, lvalue, operation) => {
                let result = self.lower_expr(
                    fn_builder,
                    resolved_ast,
                    *operation,
                    object_frame,
                    lower_fn_ctx,
                );

                // for an lvalue, its associated value must be a stack load or memory load
                let cranelift_lvalue = lower_fn_ctx.expr_value(*lvalue);
                let value_def = fn_builder.func.dfg.value_def(cranelift_lvalue);
                let location = match value_def {
                    cranelift::codegen::ir::ValueDef::Result(inst_ref, _) => {
                        let inst = fn_builder.func.dfg.insts[inst_ref];
                        match inst.opcode() {
                            Opcode::StackLoad => StackOrMemory::Stack(inst.stack_slot().unwrap()),
                            Opcode::Load => {
                                StackOrMemory::Memory(fn_builder.func.dfg.inst_args(inst_ref)[0])
                            }
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                };

                match location {
                    StackOrMemory::Stack(stack_slot) => {
                        fn_builder.ins().stack_store(result, stack_slot, 0);
                    }
                    StackOrMemory::Memory(pointer) => {
                        fn_builder
                            .ins()
                            .store(MemFlags::trusted(), result, pointer, 0);
                    }
                }

                result
            }

            TypedExpressionNode::PostAugmentedAssign(_, lvalue, operation) => {
                let result = self.lower_expr(
                    fn_builder,
                    resolved_ast,
                    *operation,
                    object_frame,
                    lower_fn_ctx,
                );

                // for an lvalue, its associated value must be a stack load or memory load
                let cranelift_lvalue = lower_fn_ctx.expr_value(*lvalue);
                let value_def = fn_builder.func.dfg.value_def(cranelift_lvalue);
                let location = match value_def {
                    cranelift::codegen::ir::ValueDef::Result(inst_ref, _) => {
                        let inst = fn_builder.func.dfg.insts[inst_ref];
                        match inst.opcode() {
                            Opcode::StackLoad => StackOrMemory::Stack(inst.stack_slot().unwrap()),
                            Opcode::Load => {
                                StackOrMemory::Memory(fn_builder.func.dfg.inst_args(inst_ref)[0])
                            }
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                };

                match location {
                    StackOrMemory::Stack(stack_slot) => {
                        fn_builder.ins().stack_store(result, stack_slot, 0);
                    }
                    StackOrMemory::Memory(pointer) => {
                        fn_builder
                            .ins()
                            .store(MemFlags::trusted(), result, pointer, 0);
                    }
                }

                cranelift_lvalue
            }

            other => {
                dbg!(other);
                todo!("other expressions")
            }
        };

        lower_fn_ctx.set_expr_value(expr_ref, expr_value);
        expr_value
    }

    fn lower_lvalue(
        &mut self,
        fn_builder: &mut FunctionBuilder,
        lvalue_ref: ExprRef,
        resolved_ast: &ResolvedAST,

        object_frame: &Frame,
        lower_fn_ctx: &mut LowerFunctionContext,
    ) -> StackOrMemory {
        let lvalue_expr = &resolved_ast.exprs[lvalue_ref];
        match lvalue_expr {
            TypedExpressionNode::ObjectIdentifier(_, object_idx)
            | TypedExpressionNode::ArrayDecay(_, object_idx) => {
                if object_frame.contains(*object_idx) {
                    StackOrMemory::Stack(object_frame.get_object_stack_slot(*object_idx))
                } else {
                    // must be a global / static variable
                    let global_index: usize = resolved_ast.symtab.global_index(*object_idx);
                    let data_id = self.data[global_index];
                    let global_value = self.object.declare_data_in_func(data_id, fn_builder.func);
                    let global_address = fn_builder
                        .ins()
                        .global_value(self.pointer_type(), global_value);
                    StackOrMemory::Memory(global_address)
                }
            }
            TypedExpressionNode::Dereference(lvalue_type, pointer) => {
                let pointer_value = self.lower_expr(
                    fn_builder,
                    resolved_ast,
                    *pointer,
                    object_frame,
                    lower_fn_ctx,
                );
                StackOrMemory::Memory(pointer_value)
            }

            _ => todo!("other lvalue types"),
        }
    }

    // most expressions don't need qualifier, and are scalar types
    fn get_basic_type(expr_type: &CType) -> BasicType {
        match expr_type {
            crate::types::CType::BasicType { basic_type, .. } => *basic_type,
            _ => panic!("expression should have basic type"),
        }
    }

    pub(crate) fn finish_and_write(self, path: &Path) {
        let finished_object = self.object.finish();
        let bytes = finished_object.emit().expect("failed to emit object file");

        fs::write(path, bytes).expect("failed to write to object file");
    }
}

fn compare_op(
    fn_builder: &mut FunctionBuilder,
    a: Value,
    b: Value,
    common_type: &CType,
    signed_int: IntCC,
    unsigned_int: IntCC,
    float: FloatCC,
) -> Value {
    let (signed_cmp, float_cmp) = match common_type {
        CType::BasicType { basic_type, .. } => (basic_type.is_signed(), basic_type.is_fp()),
        CType::PointerType { pointee_type, .. } => (false, false),
        _ => unreachable!("prohibited by resolver"),
    };

    if float_cmp {
        fn_builder.ins().fcmp(float, a, b)
    } else if signed_cmp {
        fn_builder.ins().icmp(signed_int, a, b)
    } else {
        fn_builder.ins().icmp(unsigned_int, a, b)
    }
}

#[cfg(test)]
mod cranelift_backend_tests {
    use std::{
        fs,
        io::Write,
        path::{Path, PathBuf},
        process::{Command, Stdio},
    };

    use cranelift::{
        module::{DataDescription, Linkage, Module},
        object::ObjectModule,
        prelude::{
            AbiParam, FunctionBuilder, FunctionBuilderContext, InstBuilder,
            settings::{self, Flags},
            types,
        },
    };

    use crate::semantics::resolver::resolve_ast_tests::{ResolveHarnessInput, resolve_harness};

    use super::CraneliftBackend;

    fn test_artifact_dir(test_name: &'static str) -> PathBuf {
        let workspace = std::env::var("CARGO_MANIFEST_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|_| std::env::temp_dir());

        let test_artifacts = workspace.join("test_artifacts");
        let test_artifact = test_artifacts.join(test_name);
        // error is expected if it already exists
        _ = std::fs::create_dir_all(&test_artifact);
        test_artifact
    }

    fn compile_code(test_name: &'static str, code: &'static str) {
        let working_dir = test_artifact_dir(test_name);

        let input = ResolveHarnessInput { code };
        let resolved = resolve_harness(input);

        let resolved_ast_file = working_dir.join("resolved_ast");
        dbg!(&resolved);
        std::fs::write(&resolved_ast_file, format!("{:#?}", &resolved))
            .expect("failed to write resolved AST to file");

        let mut cranelift_backend = CraneliftBackend::new("test_compile_expr");
        let mut function_builder_ctx = FunctionBuilderContext::new();
        cranelift_backend.process_global_symbols(&resolved.symtab);
        cranelift_backend.lower_translation_unit(&resolved, &mut function_builder_ctx);

        let object_file_name = format!("{test_name}.o");
        let object_file = working_dir.join(object_file_name);
        cranelift_backend.finish_and_write(&object_file);

        let binary_file = working_dir.join(test_name);
        let mut compile_command = Command::new("clang");
        compile_command
            .arg("-no-pie") // cranelift is not generating position independent assembly
            .arg("-o")
            .arg(binary_file)
            .arg(object_file);

        compile_command
            .spawn()
            .expect("unable to launch compiler")
            .wait()
            .expect("it didn't run");

        // generate reference binary using clang, for differential testing
        let code_file = working_dir.join("code.c");
        let binary_file_name_clang = format!("{test_name}.clang");
        let binary_file_clang = working_dir.join(binary_file_name_clang);
        let mut compile_command = Command::new("clang");
        compile_command
            .arg("-no-pie") // cranelift is not generating position independent assembly
            .arg("-o")
            .arg(binary_file_clang)
            .arg(code_file);

        compile_command
            .spawn()
            .expect("unable to launch compiler")
            .wait()
            .expect("it didn't run");
    }

    // TODO: support interactive input
    fn run_code(
        test_name: &'static str,
        stdin: &'static str,
        args: &'static [&'static str],
        expected_stdout: &'static str,
        expected_exit: i32,
    ) {
        let working_dir = test_artifact_dir(test_name);
        let executable = working_dir.join(test_name);

        let mut executable_command = Command::new(executable);
        let executable_command = executable_command.args(args);
        let mut child = executable_command
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .stdin(Stdio::piped())
            .spawn()
            .expect("unable to launch executable");

        child
            .stdin
            .take()
            .expect("child has no stdin")
            .write(stdin.as_bytes())
            .expect("failed to write to stdin");

        let output = child.wait_with_output().expect("failed to wait on child");

        let stdout_string =
            String::from_utf8(output.stdout.clone()).expect("stdout contained weird chars");
        dbg!(stdout_string);
        dbg!(output.status.code());

        assert_eq!(output.stdout.as_slice(), expected_stdout.as_bytes());
        assert_eq!(
            output.status.code().expect("unexpected signal"),
            expected_exit
        );
    }

    fn test_harness(
        test_name: &'static str,
        code: &'static str,
        expected_stdout: &'static str,
        expected_exit: i32,
    ) {
        compile_code(test_name, code);
        run_code(test_name, "", &[], expected_stdout, expected_exit);
    }

    #[test]
    fn test_compile_expr() {
        let code = r#"
        int main(int argc, char *argv[]) {
            return 5 + 3 * 2 / (6 % 5);
        }
        "#;

        test_harness("expr", code, "", 11);
    }

    #[test]
    fn test_compile_variables() {
        let code = r#"
        int main(int argc, char *argv[]) {
            int two;
            int three;
            two = 2;
            three = 3;
            return two + three;
        }
        "#;

        test_harness("variables", code, "", 5);
    }

    #[test]
    fn test_compile_function_call() {
        let code = r#"
        int square_three() {
            return 3 * 3;
        }

        int main(int argc, char *argv[]) {
            return square_three();
        }
        "#;

        test_harness("function_call", code, "", 9);
    }

    #[test]
    fn test_compile_string_literal() {
        let code = r#"
        int puts(const char *str);

        int main(int argc, char *argv[]) {
            puts("Hello world!");
            return 0;
        }
        "#;

        test_harness("string_literal", code, "Hello world!\n", 0);
    }

    #[test]
    fn test_array() {
        let code = r#"
        int puts(const char *str);

        int main(int argc, char *argv[]) {
            char buf[8];
            buf[0] = 'a';
            buf[1] = 'r';
            buf[2] = 'g';
            buf[3] = 'c';
            buf[4] = ':';
            buf[5] = ' ';
            buf[6] = '0' + argc;
            buf[7] = '\0';

            puts(buf);
            
            return 0;
        }
        "#;
        test_harness("array", code, "argc: 1\n", 0);
    }

    #[test]
    fn test_conditional() {
        let code = r#"
        int puts(const char *str);
        char *strcpy(char *dest, const char *src);

        int main(int argc, char *argv[]) {
            char buf[64];
            strcpy(buf, "argc=4? x");
            
            if (argc == 4) {
                buf[8] = 'y';
            }
            else {
                buf[8] = 'n';
            }
            puts(buf);
            return 0;
        }
        "#;

        let test_name = "conditional";
        compile_code(test_name, code);
        run_code(test_name, "", &[], "argc=4? n\n", 0);
        run_code(test_name, "", &["x", "x", "x"], "argc=4? y\n", 0);
    }

    #[test]
    fn test_complicated_conditional() {
        let code = r#"
        int main(int argc, char *argv[]) {
            if (0) {
            }
            else if (1) {
            }
            else {
                return 15;
            }
            return 0;
        }
        "#;

        compile_code("complicated_conditional", code);
    }

    #[test]
    fn test_while() {
        let code = r#"
        int puts(const char *str);
        void my_strcpy(char *dest, const char *src) {
            while (*src) {
                *dest = *src;
                dest = dest + 1;
                src = src + 1;
            }
        }

        int main(int argc, char *argv[]) {
            char buf[32];
            my_strcpy(buf, "hello from my_strcpy");
            puts(buf);
            return 0;
        }
        "#;

        test_harness("while", code, "hello from my_strcpy\n", 0);
    }

    #[test]
    fn test_calculator() {
        let code = r#"
        int puts(const char *str);
        char *gets(char *str);
        char *strcpy(char *dest, const char *src);
        int strcmp(const char *str1, const char *str2);
        int atoi(const char *str);
        unsigned long strlen(const char *str);

        int add(int a, int b) {
            return a + b;
        }
        int subtract(int a, int b) {
            return a - b;
        }
        int multiply(int a, int b) {
            return a * b;
        }
        int divide(int a, int b) {
            return a / b;
        }

        int main(int argc, char *argv[]) {
            char buf[32];
            int a;
            int b;
            puts("Enter first number: ");
            gets(buf);
            a = atoi(buf);
            puts("Enter second number: ");
            gets(buf);
            b = atoi(buf);
            
            int (*calc)(int, int);
            
            puts("Choose operation (+, -, *, /): ");
            gets(buf);

            if (strcmp(buf, "+") == 0) {
                calc = &add;
            }
            else if (strcmp(buf, "-") == 0) {
                calc = &subtract;
            }
            else if (strcmp(buf, "*") == 0) {
                calc = &multiply;
            }
            else if (strcmp(buf, "/") == 0) {
                calc = &divide;
            }
            else {
                return 0 - 1;
            }

            int answer;
            answer = calc(a, b);
            
            unsigned long idx;
            strcpy(buf, "The answer is: ");
            idx = strlen(buf);

            do {
                int lsd;
                lsd = answer % 10;
                answer = answer / 10;
                buf[idx] = '0' + lsd;
                idx = idx + 1;
            } while (answer);
            buf[idx] = '\0';

            puts(buf);

            return 0;
        }
        "#;

        compile_code("calculator", code);
    }

    #[test]
    fn test_short_circuit() {
        let code = r#"
        int puts(const char *str);
        int return_false_and_print() {
            puts("false");
            return 0;
        }

        int return_true_and_print() {
            puts("true");
            return 1;
        }

        int main(int argc, char *argv[]) {
            if (return_false_and_print() && return_true_and_print()) {
            }
            puts("sep");
            if (return_true_and_print() || return_false_and_print()) {
            }
            return 0;
        }
        "#;

        let test_name = "short_circuit";
        compile_code(test_name, code);
        run_code(test_name, "", &[], "false\nsep\ntrue\n", 0);
    }

    #[test]
    fn test_for_loop() {
        let code = r#"
        int puts(const char *str);
        void my_strcpy_for(char *dest, char *src) {
            char *tmp;
            for (tmp = src; *tmp != '\0'; tmp = tmp + 1, dest = dest + 1) {
                *dest = *tmp;
            }
            *dest = '\0';
        }

        int main(int argc, char *argv[]) {
            char buf[32];

            my_strcpy_for(buf, "Hello world!");
            puts(buf);
            return 0;
        }
        "#;

        let test_name = "for_loop";
        compile_code(test_name, code);
        run_code(test_name, "", &[], "Hello world!\n", 0);
    }

    #[test]
    fn test_continue_break() {
        let code = r#"
        int puts(const char *str);
        void itoa(int value, char *buf) {
            int value_copy;
            value_copy = value;
            
            int digits;
            digits = 0;

            do {
                value_copy = value_copy / 10;
                digits = digits + 1;
            } while (value_copy);

            buf[digits] = '\0';
            do {
                int lsd;
                lsd = value % 10;
                value = value / 10;
                buf[digits - 1] = '0' + lsd;
                digits = digits - 1;
            } while (value);
        }

        int main(int argc, char *argv[]) {
            int i;
            char buf[32];
            i = 0;
            while (1) {
                itoa(i, buf);
                if (i > 10) {
                    break;
                }

                if (i % 2 == 1) {
                    i = i + 1;
                    continue;
                }

                puts(buf);
                i = i + 1;
            }

            for (i = 0;; i = i + 1) {
                itoa(i, buf);
                if (i > 10) {
                    break;
                }
                
                if (i % 2 == 0) {
                    continue;
                }

                puts(buf);
            }

            return 0;
        }
        "#;

        let test_name = "continue_break";
        compile_code(test_name, code);
        run_code(test_name, "", &[], "0\n2\n4\n6\n8\n10\n1\n3\n5\n7\n9\n", 0);
    }

    #[test]
    fn test_nested_continue() {
        let code = r#"
        int puts(const char *str);
        void itoa(int value, char *buf) {
            int value_copy;
            value_copy = value;
            
            int digits;
            digits = 0;

            do {
                value_copy = value_copy / 10;
                digits = digits + 1;
            } while (value_copy);

            buf[digits] = '\0';
            do {
                int lsd;
                lsd = value % 10;
                value = value / 10;
                buf[digits - 1] = '0' + lsd;
                digits = digits - 1;
            } while (value);
        }

        int main(int argc, char *argv[]) {
            char buf[16];
            int sum;
            sum = 0;
            
            int i, j;
            for (i = 0; i < 10; i = i + 1) {
                if (i % 2 == 0) {
                    continue;
                }
                for (j = 0; j < 10; j = j + 1) {
                    if (j % 2 == 0) {
                        continue;
                    }
                    sum = sum + i * 10 + j;
                }
            }

            itoa(sum, buf);
            puts(buf);
            return 0;
        }
        "#;

        let test_name = "nested_continue";
        compile_code(test_name, code);
        run_code(test_name, "", &[], "1375\n", 0);
    }

    #[test]
    fn test_switch() {
        let code = r#"
        int puts(const char *str);
        char *gets(char *str);

        int main() {
            char buf[16];
            gets(buf);

            int n;
            n = buf[0] - '0';

            switch (n) {
                case 1:
                    puts("one!");
                    break;
                case 2:
                    puts("two!");
                    break;
                case 3:
                    puts("three!");
                    break;
                default:
                    puts("nope");
                    break;
            }

            return 0;
        }
        "#;

        let test_name = "switch";
        compile_code(test_name, code);
        run_code(test_name, "1\n", &[], "one!\n", 0);
        run_code(test_name, "2\n", &[], "two!\n", 0);
        run_code(test_name, "3\n", &[], "three!\n", 0);
        run_code(test_name, "4\n", &[], "nope\n", 0);
    }

    #[test]
    fn test_varargs_call() {
        let code = r#"
        int printf(const char *fmt, ...);
        int main(int argc, char *argv[]) {
            const char *hello;
            hello = "hello!";
            printf("argc: %d, %s", argc, hello);
            return 0;
        }
        "#;

        test_harness("varargs_call", code, "argc: 1, hello!", 0);
    }

    #[test]
    fn test_globals() {
        let code = r#"
        int printf(const char *fmt, ...);
        int counter;
        
        int fetch_add() {
            int old_counter;
            old_counter = counter;
            counter = counter + 1;
            return old_counter;
        }

        int main(int argc, char *argv[]) {
            int x;
            x = fetch_add();
            printf("x: %d, ", x);
            x = fetch_add();
            printf("x: %d, ", x);
            return 0;
        }
        "#;

        test_harness("globals", code, "x: 0, x: 1, ", 0);
    }

    #[test]
    fn test_augmented_assignment() {
        let code = r#"
        int printf(const char *fmt, ...);
        void my_strcpy(char *dest, const char *src) {
            while (*src) {
                *(dest++) = *(src++);
            }
            *dest = '\0';
        }
        int main(int argc, char *argv[]) {
            int i;
            int j;
            j = 1;
            for (i = 0; i < 10; i += 1) {
                j *= 2;
            }
            printf("%d ", j);
            
            int x;
            x = 10;
            printf("%d ", ++x);
            printf("%d ", x++);

            char buf[64];
            my_strcpy(buf, "test test");
            printf("%s", buf);

            return x;
        }
        "#;

        test_harness("augmented_assignment", code, "1024 11 11 test test", 12);
    }

    fn test_initializer() {
        let code = r#"
        int main(int argc, char *argv[]) {
            int inited = 7;
            return inited;
        }
        "#;

        test_harness("initializer", code, "argc: 1, argv[0]: ./test", 0);
    }

    #[test]
    fn test_struct() {
        let code = r#"
        struct data {
            int a;
            float b;
        };
        
        int main() {
            struct data x;
            x.a = 5;
            x.b = 2.0f;

            return 0;
        }
        "#;

        compile_code("struct", code);
    }
}
