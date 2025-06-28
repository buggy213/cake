use core::panic;
use std::{cell::RefCell, collections::HashMap, fs, path::Path};

use cranelift::{
    module::{DataDescription, DataId, FuncId, Module},
    object::ObjectModule,
    prelude::{
        AbiParam, FunctionBuilder, FunctionBuilderContext, InstBuilder, Signature, Type, Value,
        settings::{self, Flags},
        types,
    },
};

use crate::{
    semantics::resolved_ast::{ExprRef, NodeRef, ResolvedASTNode},
    semantics::{
        self,
        resolver::ResolvedAST,
        symtab::{Scope, SymbolTable},
    },
    types::{BasicType, FunctionType, TypeQualifier},
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

struct CraneliftBackend {
    object: ObjectModule,
    data: HashMap<String, DataId>,
    functions: HashMap<String, FuncId>,

    fn_builder_ctx: FunctionBuilderContext,
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
            functions: HashMap::new(),

            fn_builder_ctx: FunctionBuilderContext::new(),
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
            let init = DataDescription::new();
            init.define_zeroinit(size);
        }

        /*
        for (name, global_symbol) in symtab.all_symbols_at_scope(Scope::new_file_scope()) {
            match global_symbol {
                semantics::symtab::Symbol::Constant(_) => (), // no-op, as these are folded into resolved expressions
                semantics::symtab::Symbol::Object {
                    object_type,
                    linkage,
                } => {
                    let linkage = match linkage {
                        semantics::symtab::Linkage::External => cranelift::module::Linkage::Export,
                        semantics::symtab::Linkage::Internal => cranelift::module::Linkage::Local,
                        semantics::symtab::Linkage::None => {
                            panic!("top-level decls should have defined linkage")
                        }
                    };

                    // TODO: support initializer
                    let object_type = symtab.get_qualified_type(*object_type);
                    let object_type_cranelift: Type = match &object_type.base_type {
                        semantics::types::CType::BasicType { basic_type } => (*basic_type).into(),
                        semantics::types::CType::PointerType { pointee_type } => {
                            self.pointer_type()
                        }
                        _ => todo!("support non-scalar global objects"),
                    };

                    let writable = !object_type.qualifier.contains(TypeQualifier::Const);
                    let object_id = self
                        .object
                        .declare_data(&name, linkage, writable, false)
                        .expect("failed to insert data (ADD ERROR)");

                    // TODO: consider alignment
                    let mut init = DataDescription::new();
                    init.define_zeroinit(object_type_cranelift.bytes() as usize);
                    self.object
                        .define_data(object_id, &init)
                        .expect("failed to define data (ADD ERROR)");

                    self.data.insert(name.clone(), object_id);
                }
                semantics::symtab::Symbol::Function {
                    function_type,
                    internal_linkage,
                    defined,
                } => {
                    let linkage;
                    if *internal_linkage {
                        linkage = cranelift::module::Linkage::Local;
                    } else {
                        if *defined {
                            linkage = cranelift::module::Linkage::Export;
                        } else {
                            linkage = cranelift::module::Linkage::Import;
                        }
                    }

                    let fn_type = match symtab.get_canonical_type(*function_type) {
                        CanonicalType::FunctionType(inner) => inner,
                        _ => panic!("corrupted symtab type table"),
                    };

                    let signature = self.lower_function_signature(fn_type);
                    let fn_id = self
                        .object
                        .declare_function(name, linkage, &signature)
                        .expect("unable to insert function (ADD ERROR)");

                    self.functions.insert(name.clone(), fn_id);
                }
            }
        }
        */
    }

    fn lower_function_signature(&self, fn_type: &FunctionTypeInner) -> Signature {
        let mut signature = self.object.make_signature();

        // TODO: non-scalar return / parameter types
        match fn_type.return_type.base_type {
            semantics::types::CType::Void => (),
            semantics::types::CType::PointerType { .. } => {
                signature.returns.push(AbiParam::new(self.pointer_type()));
            }
            semantics::types::CType::BasicType { basic_type } => {
                let cranelift_type = basic_type.into();
                signature.returns.push(AbiParam::new(cranelift_type));
            }
            _ => {
                todo!("support non-scalar return types")
            }
        }

        for (_, param_type) in &fn_type.parameter_types {
            match param_type.base_type {
                semantics::types::CType::PointerType { .. } => {
                    signature.params.push(AbiParam::new(self.pointer_type()));
                }
                semantics::types::CType::BasicType { basic_type } => {
                    let cranelift_type = basic_type.into();
                    signature.params.push(AbiParam::new(cranelift_type));
                }
                _ => {
                    todo!("support non-scalar parameter types")
                }
            }
        }

        dbg!(&signature);
        signature
    }

    pub(crate) fn lower_translation_unit(&mut self, resolved_ast: &ResolvedAST) {
        let root = &resolved_ast.nodes[0];

        match root {
            ResolvedASTNode::TranslationUnit { children, scope } => {
                for fn_defn_ref_idx in children.0..children.1 {
                    let fn_defn_ref = resolved_ast.ast_indices[fn_defn_ref_idx as usize];
                    self.lower_function(resolved_ast, fn_defn_ref);
                }
            }
            _ => panic!("corrupted AST"),
        }
    }

    fn lower_function(&mut self, resolved_ast: &ResolvedAST, function_node_ref: NodeRef) {
        let (cranelift_id, body) = match &resolved_ast.nodes[function_node_ref.0 as usize] {
            ResolvedASTNode::FunctionDefinition {
                parent,
                ident,
                body,
            } => {
                let cranelift_id = self.functions[&ident.name];
                (cranelift_id, *body)
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

        let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut self.fn_builder_ctx);

        let entry = fn_builder.create_block();
        fn_builder.append_block_params_for_function_params(entry);
        fn_builder.switch_to_block(entry);
        fn_builder.seal_block(entry);

        match &resolved_ast.nodes[body.0 as usize] {
            ResolvedASTNode::CompoundStatement {
                parent,
                stmts,
                scope,
            } => {
                // TODO: declare variables in scope

                for stmt_ref_idx in stmts.0..stmts.1 {
                    let stmt_ref = resolved_ast.ast_indices[stmt_ref_idx as usize];
                    Self::lower_statement(&mut fn_builder, resolved_ast, stmt_ref);
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

    fn lower_statement(
        fn_builder: &mut FunctionBuilder,
        resolved_ast: &ResolvedAST,
        statement_ref: NodeRef,
    ) {
        match &resolved_ast.nodes[statement_ref.0 as usize] {
            crate::parser::ast::ResolvedASTNode::ExpressionStatement {
                parent,
                expr,
                scope,
            } => {
                Self::lower_expr(fn_builder, resolved_ast, *expr);
            }
            crate::parser::ast::ResolvedASTNode::ReturnStatement {
                parent,
                return_value,
            } => {
                let returns: &[Value] = if let Some(return_value) = return_value {
                    let cranelift_value = Self::lower_expr(fn_builder, resolved_ast, *return_value);
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
        fn_builder: &mut FunctionBuilder,
        resolved_ast: &ResolvedAST,
        expr_ref: ExprRef,
    ) -> Value {
        match &resolved_ast.exprs[expr_ref.0 as usize] {
            crate::parser::ast::TypedExpressionNode::Multiply(result_type, lhs, rhs) => {
                let lhs_value = Self::lower_expr(fn_builder, resolved_ast, *lhs);
                let rhs_value = Self::lower_expr(fn_builder, resolved_ast, *rhs);

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
            crate::parser::ast::TypedExpressionNode::Divide(result_type, lhs, rhs) => {
                let lhs_value = Self::lower_expr(fn_builder, resolved_ast, *lhs);
                let rhs_value = Self::lower_expr(fn_builder, resolved_ast, *rhs);

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
            crate::parser::ast::TypedExpressionNode::Modulo(result_type, lhs, rhs) => {
                let lhs_value = Self::lower_expr(fn_builder, resolved_ast, *lhs);
                let rhs_value = Self::lower_expr(fn_builder, resolved_ast, *rhs);

                let result_type = Self::get_basic_type(result_type);

                let rem = if result_type.is_signed() {
                    fn_builder.ins().srem(lhs_value, rhs_value)
                } else {
                    fn_builder.ins().urem(lhs_value, rhs_value)
                };

                rem
            }
            crate::parser::ast::TypedExpressionNode::Add(result_type, lhs, rhs) => {
                let lhs_value = Self::lower_expr(fn_builder, resolved_ast, *lhs);
                let rhs_value = Self::lower_expr(fn_builder, resolved_ast, *rhs);

                let result_type = Self::get_basic_type(result_type);

                let sum = if result_type.is_fp() {
                    fn_builder.ins().fadd(lhs_value, rhs_value)
                } else {
                    fn_builder.ins().iadd(lhs_value, rhs_value)
                };

                sum

                // TODO: pointer arithmetic
            }
            crate::parser::ast::TypedExpressionNode::Subtract(result_type, lhs, rhs) => {
                let lhs_value = Self::lower_expr(fn_builder, resolved_ast, *lhs);
                let rhs_value = Self::lower_expr(fn_builder, resolved_ast, *rhs);

                let result_type = Self::get_basic_type(result_type);

                let difference = if result_type.is_fp() {
                    fn_builder.ins().fsub(lhs_value, rhs_value)
                } else {
                    fn_builder.ins().isub(lhs_value, rhs_value)
                };

                difference

                // TODO: pointer arithmetic
            }
            crate::parser::ast::TypedExpressionNode::Constant(result_type, constant) => {
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
            _ => todo!("other expressions"),
        }
    }

    // most expressions don't need qualifier, and are scalar types
    fn get_basic_type(expr_type: &QualifiedType) -> BasicType {
        match expr_type.base_type {
            semantics::types::CType::BasicType { basic_type } => basic_type,
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
    use std::{fs, path::Path};

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

    #[test]
    fn test_compile_expr() {
        let code = r#"
        int main(int argc, char *argv[]) {
            return 5 + 3 * 2 / (6 % 5);
        }
        "#;

        let input = ResolveHarnessInput { code };
        let resolved = resolve_harness(input);

        dbg!(&resolved);

        let mut cranelift_backend = CraneliftBackend::new("test_compile_expr");
        cranelift_backend.process_global_symbols(&resolved.symtab);
        cranelift_backend.lower_translation_unit(&resolved);
        cranelift_backend.finish_and_write(Path::new("test.o"));
    }

    #[test]
    fn test_compile_variables() {}

    #[test]
    fn test_compile_function_call() {}
}
