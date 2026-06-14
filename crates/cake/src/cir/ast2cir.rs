use crate::{
    cir::{FunctionBuilder, Module, Signature, Type, Value},
    semantics::{
        resolved_ast::{ExprRef, NodeRef, ResolvedASTNode},
        resolver::ResolvedAST,
    },
    types::{BasicType, CType},
};

impl From<BasicType> for Type {
    fn from(value: BasicType) -> Self {
        match value {
            BasicType::Char => Self::i8,
            BasicType::UChar => Self::u8,
            BasicType::Short => Self::i16,
            BasicType::UShort => Self::u16,
            BasicType::Int => Self::i32,
            BasicType::UInt => Self::u32,
            BasicType::Long => Self::i64,
            BasicType::ULong => Self::u64,
            BasicType::Float => Self::f32,
            BasicType::Double => Self::f64,
        }
    }
}

impl TryFrom<&CType> for Type {
    type Error = ();

    fn try_from(value: &CType) -> Result<Self, Self::Error> {
        match value {
            CType::BasicType { basic_type, .. } => Ok((*basic_type).into()),
            CType::PointerType { .. } => Ok(Type::u64),
            _ => todo!("other types"),
        }
    }
}

pub(crate) fn lower_ast(ast: ResolvedAST) -> Module {
    let Some(ResolvedASTNode::TranslationUnit { children }) = ast.nodes.first() else {
        panic!("corrupted ast")
    };

    let mut module = Module::new();

    for func in &ast.ast_indices[children.0 as usize..children.1 as usize] {
        let ResolvedASTNode::FunctionDefinition {
            symbol_idx, body, ..
        } = &ast.nodes[func.0 as usize]
        else {
            panic!("corrupted ast")
        };

        let ast_func = ast.symtab.get_function(*symbol_idx);
        let ast_func_type = ast.symtab.get_function_type(ast_func.function_type);
        let ast_func_args = &ast_func_type.parameter_types;
        let ast_func_args: Vec<CType> = ast_func_args.iter().map(|arg| arg.1.clone()).collect();

        let func_args: Vec<Type> = ast_func_args
            .iter()
            .map(|arg| arg.try_into().unwrap())
            .collect();
        let func_ret = if let CType::Void { .. } = ast_func_type.return_type {
            None
        } else {
            Some((&ast_func_type.return_type).try_into().unwrap())
        };

        let func_sig = Signature::new(func_args, func_ret);
        let func_sig_ref = module.add_signature(func_sig);
        let func_ref = module.add_function(func_sig_ref);

        let func_builder = module.fn_builder(func_ref);
    }

    module
}

fn lower_function(ast: &ResolvedAST, fn_body: NodeRef, func_builder: &mut FunctionBuilder) {
    let ResolvedASTNode::CompoundStatement { stmts, .. } = &ast.nodes[fn_body.0 as usize] else {
        panic!("corrupted ast")
    };

    let (start, end) = (stmts.0 as usize, stmts.1 as usize);
    for &stmt_ref in &ast.ast_indices[start..end] {
        lower_stmt(ast, stmt_ref, func_builder);
    }
}

fn lower_stmt(ast: &ResolvedAST, stmt: NodeRef, func_builder: &mut FunctionBuilder) {
    let stmt_node = &ast.nodes[stmt.0 as usize];
    match stmt_node {
        ResolvedASTNode::TranslationUnit { children } => todo!(),
        ResolvedASTNode::FunctionDefinition {
            parent,
            symbol_idx,
            body,
        } => todo!(),
        ResolvedASTNode::Label { parent, labelee } => todo!(),
        ResolvedASTNode::CaseLabel {
            parent,
            labelee,
            case_index,
        } => todo!(),
        ResolvedASTNode::DefaultLabel { parent, labelee } => todo!(),
        ResolvedASTNode::NullStatement { parent } => {}
        ResolvedASTNode::CompoundStatement { parent, stmts } => {
            let (start, end) = (stmts.0 as usize, stmts.1 as usize);
            for &stmt_ref in &ast.ast_indices[start..end] {
                lower_stmt(ast, stmt_ref, func_builder);
            }
        }
        ResolvedASTNode::ExpressionStatement { parent, expr } => {
            lower_expr(ast, *expr, func_builder);
        }
        ResolvedASTNode::IfStatement {
            parent,
            condition,
            taken,
            not_taken,
        } => todo!(),
        ResolvedASTNode::SwitchStatement {
            parent,
            controlling_expr,
            body,
            context,
        } => todo!(),
        ResolvedASTNode::WhileStatement {
            parent,
            condition,
            body,
        } => todo!(),
        ResolvedASTNode::DoWhileStatement {
            parent,
            condition,
            body,
        } => todo!(),
        ResolvedASTNode::ForStatement {
            parent,
            init,
            condition,
            post_body,
            body,
        } => todo!(),
        ResolvedASTNode::GotoStatement { parent, target } => todo!(),
        ResolvedASTNode::ContinueStatement { parent, target } => todo!(),
        ResolvedASTNode::BreakStatement { parent, target } => todo!(),
        ResolvedASTNode::ReturnStatement {
            parent,
            return_value,
        } => todo!(),
        ResolvedASTNode::Initializer {
            parent,
            object,
            assignment,
        } => todo!(),
    }
}

// TODO: this should be parameterized
fn ptrtype() -> Type {
    Type::u64
}

fn lower_expr(ast: &ResolvedAST, expr: ExprRef, func_builder: &mut FunctionBuilder) -> Value {
    use crate::semantics::resolved_ast::TypedExpressionNode;
    let expr_node = &ast.exprs[expr];
    match expr_node {
        TypedExpressionNode::CommaExpr(ctype, expr_range_ref) => todo!(),
        TypedExpressionNode::SimpleAssign(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::AugmentedAssign(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::PostAugmentedAssign(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::Ternary(ctype, expr_ref, expr_ref1, expr_ref2) => todo!(),
        TypedExpressionNode::LogicalAnd(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::LogicalOr(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::BitwiseAnd(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::BitwiseOr(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::BitwiseXor(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::Equal(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::NotEqual(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::LessThan(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::GreaterThan(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::LessThanOrEqual(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::GreaterThanOrEqual(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::LShift(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::RShift(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::Multiply(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::Divide(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::Modulo(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::Add(ctype, expr_ref, expr_ref1) => {
            let lhs = lower_expr(ast, *expr_ref, func_builder);
            let rhs = lower_expr(ast, *expr_ref1, func_builder);

            func_builder.insert().add(lhs, rhs)
        }
        TypedExpressionNode::Subtract(ctype, expr_ref, expr_ref1) => {
            let lhs = lower_expr(ast, *expr_ref, func_builder);
            let rhs = lower_expr(ast, *expr_ref1, func_builder);

            func_builder.insert().add(lhs, rhs)
        }
        TypedExpressionNode::PointerAdd(ctype, expr_ref, expr_ref1) => {
            assert!(
                ctype.is_object_pointer(),
                "pointer arithmetic requires pointer type"
            );

            let lhs = lower_expr(ast, *expr_ref, func_builder);
            let rhs = lower_expr(ast, *expr_ref1, func_builder);
            let sizeof_object = ctype.as_pointee().unwrap().size(todo!("plumb in layouts"));
            let sizeof_object_val = func_builder.insert().const_u64(sizeof_object as u64);
            let rhs_ptrtype = func_builder.insert().icast(rhs, ptrtype());
            let byte_offset = func_builder.insert().mul(rhs_ptrtype, sizeof_object_val);

            todo!()
        }
        TypedExpressionNode::PointerSub(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::PointerDiff(ctype, expr_ref, expr_ref1) => todo!(),
        TypedExpressionNode::Cast(ctype, expr_ref, ctype1) => todo!(),
        TypedExpressionNode::AddressOf(ctype, expr_ref) => todo!(),
        TypedExpressionNode::Dereference(ctype, expr_ref) => todo!(),
        TypedExpressionNode::UnaryPlus(ctype, expr_ref) => todo!(),
        TypedExpressionNode::UnaryMinus(ctype, expr_ref) => todo!(),
        TypedExpressionNode::BitwiseNot(ctype, expr_ref) => todo!(),
        TypedExpressionNode::Not(ctype, expr_ref) => todo!(),
        TypedExpressionNode::DirectFunctionCall(ctype, function_idx, expr_range_ref) => todo!(),
        TypedExpressionNode::IndirectFunctionCall(ctype, expr_ref, expr_range_ref) => todo!(),
        TypedExpressionNode::DotAccess(ctype, expr_ref, member_ref) => todo!(),
        TypedExpressionNode::ArrowAccess(ctype, expr_ref, member_ref) => todo!(),
        TypedExpressionNode::ArrayDecay(ctype, expr_ref) => todo!(),
        TypedExpressionNode::ObjectIdentifier(ctype, object_idx) => todo!(),
        TypedExpressionNode::FunctionIdentifier(ctype, function_idx) => todo!(),
        TypedExpressionNode::Constant(ctype, constant) => todo!(),
        TypedExpressionNode::StringLiteral(ctype, _) => todo!(),
    }
}
