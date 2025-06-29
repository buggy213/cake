use core::panic;
use std::{
    cell::RefCell,
    collections::HashMap,
    ffi::CString,
    fs, iter,
    ops::{DerefMut, Index, Range},
    path::Path,
    str::FromStr,
};

use cranelift::{
    codegen::ir::{FuncRef, StackSlot},
    frontend::FuncInstBuilder,
    module::{DataDescription, DataId, FuncId, Module},
    object::ObjectModule,
    prelude::{
        AbiParam, FunctionBuilder, FunctionBuilderContext, InstBuilder, MemFlags, Signature,
        StackSlotData, StackSlotKind, Type, Value,
        settings::{self, Flags},
        types,
    },
};

use crate::{
    semantics::{
        self,
        resolved_ast::{ExprRef, NodeRef, ResolvedASTNode, TypedExpressionNode},
        resolver::ResolvedAST,
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
    fn get_object_stack_slot(&self, object_idx: ObjectIdx) -> StackSlot {
        let idx = object_idx.get_inner() - self.object_range.0 as usize;
        self.stack_slots[idx]
    }
}

impl Frame {
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
    data: HashMap<String, DataId>,
    functions: Vec<FuncId>,

    // these are used for declare_data_in_func, declare_func_in_func
    // and get cleared after each function is built
    function_refs: Vec<FuncRef>,
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
            data: HashMap::new(),
            functions: Vec::new(),

            function_refs: Vec::new(),
        }
    }

    pub(crate) fn process_global_symbols(&mut self, symtab: &SymbolTable) {
        let global_objects = symtab.global_objects();
        let global_object_names = symtab.global_object_names();

        let global_functions = symtab.functions();

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
        }

        let functions = symtab.functions();
        let function_names = symtab.function_names();
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

            let fn_type = symtab.get_function_type(function.function_type);
            let signature = self.lower_function_signature(fn_type);

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
                for fn_defn_ref_idx in children.0..children.1 {
                    let fn_defn_ref = resolved_ast.ast_indices[fn_defn_ref_idx as usize];
                    self.lower_function(resolved_ast, fn_defn_ref, function_builder_ctx);
                }
            }
            _ => panic!("corrupted AST"),
        }
    }

    fn lower_function(
        &mut self,
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

        let mut ctx = self.object.make_context();
        ctx.func.signature = declared_signature.clone();

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

        // for param
        // fn_builder.ins().stack_store();

        match &resolved_ast.nodes[body.0 as usize] {
            ResolvedASTNode::CompoundStatement { parent, stmts } => {
                for stmt_ref_idx in stmts.0..stmts.1 {
                    let stmt_ref = resolved_ast.ast_indices[stmt_ref_idx as usize];
                    self.lower_statement(&mut fn_builder, resolved_ast, stmt_ref, &object_frame);
                }
            }
            _ => panic!("bad function defn body node reference"),
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
    ) {
        match &resolved_ast.nodes[statement_ref.0 as usize] {
            crate::semantics::resolved_ast::ResolvedASTNode::ExpressionStatement {
                parent,
                expr,
            } => {
                self.lower_expr(fn_builder, resolved_ast, *expr, object_frame);
            }
            crate::semantics::resolved_ast::ResolvedASTNode::ReturnStatement {
                parent,
                return_value,
            } => {
                let returns: &[Value] = if let Some(return_value) = return_value {
                    let cranelift_value =
                        self.lower_expr(fn_builder, resolved_ast, *return_value, object_frame);
                    dbg!(cranelift_value);
                    &[cranelift_value]
                } else {
                    &[]
                };

                fn_builder.ins().return_(returns);
            }

            _ => todo!("other statement types"),
        }
    }

    fn lower_expr(
        &mut self,
        fn_builder: &mut FunctionBuilder,
        resolved_ast: &ResolvedAST,
        expr_ref: ExprRef,

        object_frame: &Frame,
    ) -> Value {
        match &resolved_ast.exprs[expr_ref] {
            crate::semantics::resolved_ast::TypedExpressionNode::Multiply(
                result_type,
                lhs,
                rhs,
            ) => {
                let lhs_value = self.lower_expr(fn_builder, resolved_ast, *lhs, object_frame);
                let rhs_value = self.lower_expr(fn_builder, resolved_ast, *rhs, object_frame);

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
                let lhs_value = self.lower_expr(fn_builder, resolved_ast, *lhs, object_frame);
                let rhs_value = self.lower_expr(fn_builder, resolved_ast, *rhs, object_frame);

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
                let lhs_value = self.lower_expr(fn_builder, resolved_ast, *lhs, object_frame);
                let rhs_value = self.lower_expr(fn_builder, resolved_ast, *rhs, object_frame);

                let result_type = Self::get_basic_type(result_type);

                let rem = if result_type.is_signed() {
                    fn_builder.ins().srem(lhs_value, rhs_value)
                } else {
                    fn_builder.ins().urem(lhs_value, rhs_value)
                };

                rem
            }
            crate::semantics::resolved_ast::TypedExpressionNode::Add(result_type, lhs, rhs) => {
                let lhs_value = self.lower_expr(fn_builder, resolved_ast, *lhs, object_frame);
                let rhs_value = self.lower_expr(fn_builder, resolved_ast, *rhs, object_frame);

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
                let lhs_value = self.lower_expr(fn_builder, resolved_ast, *lhs, object_frame);
                let rhs_value = self.lower_expr(fn_builder, resolved_ast, *rhs, object_frame);

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
                let location = self.lower_lvalue(fn_builder, *lhs, resolved_ast, object_frame);
                let cranelift_type = match result_type {
                    CType::BasicType { basic_type, .. } => (*basic_type).into(),
                    CType::PointerType { .. } => self.pointer_type(),
                    _ => todo!("support other types"),
                };
                let rhs_value = self.lower_expr(fn_builder, resolved_ast, *rhs, object_frame);

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
                for arg_expr in &resolved_ast.expr_indices[arg_range] {
                    let arg_value =
                        self.lower_expr(fn_builder, resolved_ast, *arg_expr, object_frame);
                    arg_values.push(arg_value);
                }

                let rvals = fn_builder.ins().call(func_id, &arg_values);
                fn_builder.inst_results(rvals)[0]
            }
            crate::semantics::resolved_ast::TypedExpressionNode::ObjectIdentifier(
                object_type,
                object_idx,
            ) => {
                let stack_slot = object_frame.get_object_stack_slot(*object_idx);
                let cranelift_type = match object_type {
                    CType::BasicType { basic_type, .. } => (*basic_type).into(),
                    CType::PointerType { .. } => self.pointer_type(),
                    _ => todo!("other types"),
                };
                fn_builder.ins().stack_load(cranelift_type, stack_slot, 0)
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
                        _ => todo!(),
                    },
                    _ => unreachable!("corrupted resolvedastnode"),
                };
                let base = self.lower_expr(fn_builder, resolved_ast, *ptr_ref, object_frame);
                let offset = self.lower_expr(fn_builder, resolved_ast, *int_ref, object_frame);
                let offset = fn_builder.ins().imul_imm(offset, size as i64);
                let offset = fn_builder.ins().sextend(self.pointer_type(), offset);
                fn_builder.ins().iadd(base, offset)
            }

            TypedExpressionNode::ArrayDecay(_, object_idx) => {
                let stack_slot = object_frame.get_object_stack_slot(*object_idx);
                fn_builder
                    .ins()
                    .stack_addr(self.pointer_type(), stack_slot, 0)
            }

            TypedExpressionNode::Cast(_, expr, _) => {
                // bruh
                self.lower_expr(fn_builder, resolved_ast, *expr, object_frame)
            }

            other => {
                dbg!(other);
                todo!("other expressions")
            }
        }
    }

    fn lower_lvalue(
        &mut self,
        fn_builder: &mut FunctionBuilder,
        lvalue_ref: ExprRef,
        resolved_ast: &ResolvedAST,
        object_frame: &Frame,
    ) -> StackOrMemory {
        let lvalue_expr = &resolved_ast.exprs[lvalue_ref];
        match lvalue_expr {
            TypedExpressionNode::ObjectIdentifier(lvalue_type, object_idx) => {
                StackOrMemory::Stack(object_frame.get_object_stack_slot(*object_idx))
            }
            TypedExpressionNode::Dereference(lvalue_type, pointer) => {
                let pointer_value =
                    self.lower_expr(fn_builder, resolved_ast, *pointer, object_frame);
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

#[cfg(test)]
mod cranelift_backend_tests {
    use std::{
        fs,
        io::Write,
        path::Path,
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

    #[test]
    fn test_create_object() {
        let isa_builder =
            cranelift::native::builder_with_options(true).expect("failed to make isa builder");
        let flags_builder = settings::builder();
        let isa = isa_builder
            .finish(Flags::new(flags_builder))
            .expect("failed to make isa");
        let object_builder = cranelift::object::ObjectBuilder::new(
            isa,
            "test_object",
            cranelift::module::default_libcall_names(),
        )
        .expect("failed to make builder");

        let mut object = ObjectModule::new(object_builder);
        let ptr_type = object.target_config().pointer_type();

        let mut puts_signature = object.make_signature();
        puts_signature.params.push(AbiParam::new(ptr_type));

        let hello_world_str_id = object
            .declare_data("hello_world_str", Linkage::Local, false, false)
            .expect("failed to declare hello_world_str");
        let mut hello_world_str_contents = DataDescription::new();
        hello_world_str_contents.define(Box::from(c"Hello world".to_bytes()));
        object
            .define_data(hello_world_str_id, &hello_world_str_contents)
            .expect("failed to define hello_world_str");

        let puts_id = object
            .declare_function("puts", Linkage::Import, &puts_signature)
            .expect("failed to declare puts");

        // same context should be reused for multiple functions
        let mut fn_builder_ctx = FunctionBuilderContext::new();
        let mut ctx = object.make_context();
        ctx.func.signature.params.push(AbiParam::new(types::I32));
        ctx.func.signature.params.push(AbiParam::new(ptr_type));

        ctx.func.signature.returns.push(AbiParam::new(types::I32));

        let hello_world_str_val = object.declare_data_in_func(hello_world_str_id, &mut ctx.func);
        let hello_world_str_val = object.declare_data_in_func(hello_world_str_id, &mut ctx.func);
        let hello_world_str_val = object.declare_data_in_func(hello_world_str_id, &mut ctx.func);
        let hello_world_str_val = object.declare_data_in_func(hello_world_str_id, &mut ctx.func);

        let puts_ref = object.declare_func_in_func(puts_id, &mut ctx.func);

        let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);

        let entry = fn_builder.create_block();

        fn_builder.append_block_params_for_function_params(entry);
        fn_builder.switch_to_block(entry);

        let ptr_to_hello_world = fn_builder.ins().global_value(ptr_type, hello_world_str_val);
        fn_builder.ins().call(puts_ref, &[ptr_to_hello_world]);

        let five = fn_builder.ins().iconst(types::I32, 5);
        fn_builder.ins().return_(&[five]);

        fn_builder.seal_block(entry);

        fn_builder.finalize();
        let display = ctx.func.display();
        print!("{display}");
        ctx.verify(object.isa()).expect("verification failed");

        let main_id = object
            .declare_function(
                "main",
                cranelift::module::Linkage::Export,
                &ctx.func.signature,
            )
            .expect("failed to declare function");

        object
            .define_function(main_id, &mut ctx)
            .expect("failed to define function");

        let finished_object = object.finish();
        let bytes = finished_object.emit().expect("failed to emit object file");

        fs::write("test.o", bytes).expect("failed to write to object file");
    }

    fn compile_code(code: &'static str) {
        let input = ResolveHarnessInput { code };
        let resolved = resolve_harness(input);

        dbg!(&resolved);

        let mut cranelift_backend = CraneliftBackend::new("test_compile_expr");
        let mut function_builder_ctx = FunctionBuilderContext::new();
        cranelift_backend.process_global_symbols(&resolved.symtab);
        cranelift_backend.lower_translation_unit(&resolved, &mut function_builder_ctx);
        cranelift_backend.finish_and_write(Path::new("test.o"));

        let mut compile_command = Command::new("clang");
        compile_command
            .arg("-no-pie") // cranelift is not generating position independent relocations
            .arg("-o")
            .arg("test")
            .arg("test.o");

        compile_command
            .spawn()
            .expect("unable to launch compiler")
            .wait()
            .expect("it didn't run");
    }

    // TODO: support interactive input
    fn run_code(stdin: &'static str, expected_stdout: &'static str, expected_exit: i32) {
        let mut executable_command = Command::new("./test");
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

    fn test_harness(code: &'static str, expected_stdout: &'static str, expected_exit: i32) {
        compile_code(code);
        run_code("", expected_stdout, expected_exit);
    }

    #[test]
    fn test_compile_expr() {
        let code = r#"
        int main(int argc, char *argv[]) {
            return 5 + 3 * 2 / (6 % 5);
        }
        "#;

        test_harness(code, "", 11);
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

        test_harness(code, "", 5);
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

        test_harness(code, "", 9);
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

        test_harness(code, "Hello world!\n", 0);
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

        /*

        */
        test_harness(code, "argc: 1\n", 0);
    }
}
