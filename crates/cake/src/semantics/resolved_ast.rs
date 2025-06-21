use crate::{
    parser::ast::{Constant, Identifier},
    semantics::symtab::Scope,
    types::CType,
};

#[derive(Debug, PartialEq, Copy, Clone)]
pub(crate) struct NodeRef(pub u32);

#[derive(Debug, PartialEq, Copy, Clone)]
pub(crate) struct NodeRangeRef(pub u32, pub u32);

#[derive(Debug, PartialEq, Copy, Clone)]
pub(crate) struct ExprRef(pub u32);

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) struct ContextRef(pub u32);

#[derive(Debug, PartialEq, Copy, Clone)]
pub(crate) struct ExprRangeRef(pub u32, pub u32);

#[derive(Debug, PartialEq)]
pub(crate) enum ResolvedASTNode {
    TranslationUnit {
        children: NodeRangeRef,
        scope: Scope,
    },
    FunctionDefinition {
        parent: NodeRef,
        ident: Identifier,
        body: NodeRef,
    },

    Label {
        parent: NodeRef,
        labelee: NodeRef,
    },
    CaseLabel {
        parent: NodeRef,
        labelee: NodeRef,
        case_value: ExprRef,
    },
    DefaultLabel {
        parent: NodeRef,
        labelee: NodeRef,
    },

    NullStatement {
        parent: NodeRef,
    },
    CompoundStatement {
        parent: NodeRef,
        stmts: NodeRangeRef,
        scope: Scope,
    },
    ExpressionStatement {
        parent: NodeRef,
        expr: ExprRef,
        scope: Scope,
    },
    IfStatement {
        parent: NodeRef,
        condition: ExprRef,
        taken: NodeRef,
        not_taken: Option<NodeRef>,
        scope: Scope,
    },
    SwitchStatement {
        parent: NodeRef,
        controlling_expr: ExprRef,
        body: NodeRef,
        context: ContextRef,
        scope: Scope,
    },
    WhileStatement {
        parent: NodeRef,
        condition: ExprRef,
        body: NodeRef,
        scope: Scope,
    },
    DoWhileStatement {
        parent: NodeRef,
        condition: ExprRef,
        body: NodeRef,
        scope: Scope,
    },
    ForStatement {
        parent: NodeRef,
        init: Option<NodeRef>,
        condition: Option<ExprRef>,
        post_body: Option<ExprRef>,
        body: NodeRef,
        scope: Scope,
    },

    GotoStatement {
        parent: NodeRef,
        target: Identifier,
    },
    ContinueStatement {
        parent: NodeRef,
        target: NodeRef,
    },
    BreakStatement {
        parent: NodeRef,
        target: NodeRef,
    },
    ReturnStatement {
        parent: NodeRef,
        return_value: Option<ExprRef>,
        scope: Scope,
    },
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum TypedExpressionNode {
    CommaExpr(CType, ExprRangeRef),

    SimpleAssign(CType, ExprRef, ExprRef),
    MultiplyAssign(CType, ExprRef, ExprRef),
    DivideAssign(CType, ExprRef, ExprRef),
    ModuloAssign(CType, ExprRef, ExprRef),
    AddAssign(CType, ExprRef, ExprRef),
    SubAssign(CType, ExprRef, ExprRef),
    LShiftAssign(CType, ExprRef, ExprRef),
    RShiftAssign(CType, ExprRef, ExprRef),
    AndAssign(CType, ExprRef, ExprRef),
    XorAssign(CType, ExprRef, ExprRef),
    OrAssign(CType, ExprRef, ExprRef),

    Ternary(CType, ExprRef, ExprRef, ExprRef),

    LogicalAnd(CType, ExprRef, ExprRef),
    LogicalOr(CType, ExprRef, ExprRef),
    BitwiseAnd(CType, ExprRef, ExprRef),
    BitwiseOr(CType, ExprRef, ExprRef),
    BitwiseXor(CType, ExprRef, ExprRef),

    Equal(CType, ExprRef, ExprRef),
    NotEqual(CType, ExprRef, ExprRef),

    LessThan(CType, ExprRef, ExprRef),
    GreaterThan(CType, ExprRef, ExprRef),
    LessThanOrEqual(CType, ExprRef, ExprRef),
    GreaterThanOrEqual(CType, ExprRef, ExprRef),

    LShift(CType, ExprRef, ExprRef),
    RShift(CType, ExprRef, ExprRef),
    Multiply(CType, ExprRef, ExprRef),
    Divide(CType, ExprRef, ExprRef),
    Modulo(CType, ExprRef, ExprRef),
    Add(CType, ExprRef, ExprRef),
    Subtract(CType, ExprRef, ExprRef),
    Cast(CType, ExprRef, CType),

    PreIncrement(CType, ExprRef),
    PreDecrement(CType, ExprRef),
    Sizeof(CType, ExprRef),
    AddressOf(CType, ExprRef),
    Dereference(CType, ExprRef),
    UnaryPlus(CType, ExprRef),
    UnaryMinus(CType, ExprRef),
    BitwiseNot(CType, ExprRef),
    Not(CType, ExprRef),

    PostIncrement(CType, ExprRef),
    PostDecrement(CType, ExprRef),
    ArraySubscript(CType, ExprRef, ExprRef),
    FunctionCall(CType, ExprRef, ExprRangeRef),
    DotAccess(CType, ExprRef, Identifier),
    ArrowAccess(CType, ExprRef, Identifier),
    // TODO: add support for compound initializers
    // CompoundInitializer
    Identifier(CType, Identifier),
    Constant(CType, Constant),
    StringLiteral(CType, String),
}

impl TypedExpressionNode {
    pub(crate) fn expr_type(&self) -> &CType {
        match self {
            TypedExpressionNode::CommaExpr(qualified_type, _)
            | TypedExpressionNode::SimpleAssign(qualified_type, _, _)
            | TypedExpressionNode::MultiplyAssign(qualified_type, _, _)
            | TypedExpressionNode::DivideAssign(qualified_type, _, _)
            | TypedExpressionNode::ModuloAssign(qualified_type, _, _)
            | TypedExpressionNode::AddAssign(qualified_type, _, _)
            | TypedExpressionNode::SubAssign(qualified_type, _, _)
            | TypedExpressionNode::LShiftAssign(qualified_type, _, _)
            | TypedExpressionNode::RShiftAssign(qualified_type, _, _)
            | TypedExpressionNode::AndAssign(qualified_type, _, _)
            | TypedExpressionNode::XorAssign(qualified_type, _, _)
            | TypedExpressionNode::OrAssign(qualified_type, _, _)
            | TypedExpressionNode::Ternary(qualified_type, _, _, _)
            | TypedExpressionNode::LogicalAnd(qualified_type, _, _)
            | TypedExpressionNode::LogicalOr(qualified_type, _, _)
            | TypedExpressionNode::BitwiseAnd(qualified_type, _, _)
            | TypedExpressionNode::BitwiseOr(qualified_type, _, _)
            | TypedExpressionNode::BitwiseXor(qualified_type, _, _)
            | TypedExpressionNode::Equal(qualified_type, _, _)
            | TypedExpressionNode::NotEqual(qualified_type, _, _)
            | TypedExpressionNode::LessThan(qualified_type, _, _)
            | TypedExpressionNode::GreaterThan(qualified_type, _, _)
            | TypedExpressionNode::LessThanOrEqual(qualified_type, _, _)
            | TypedExpressionNode::GreaterThanOrEqual(qualified_type, _, _)
            | TypedExpressionNode::LShift(qualified_type, _, _)
            | TypedExpressionNode::RShift(qualified_type, _, _)
            | TypedExpressionNode::Multiply(qualified_type, _, _)
            | TypedExpressionNode::Divide(qualified_type, _, _)
            | TypedExpressionNode::Modulo(qualified_type, _, _)
            | TypedExpressionNode::Add(qualified_type, _, _)
            | TypedExpressionNode::Subtract(qualified_type, _, _)
            | TypedExpressionNode::Cast(qualified_type, _, _)
            | TypedExpressionNode::PreIncrement(qualified_type, _)
            | TypedExpressionNode::PreDecrement(qualified_type, _)
            | TypedExpressionNode::Sizeof(qualified_type, _)
            | TypedExpressionNode::AddressOf(qualified_type, _)
            | TypedExpressionNode::Dereference(qualified_type, _)
            | TypedExpressionNode::UnaryPlus(qualified_type, _)
            | TypedExpressionNode::UnaryMinus(qualified_type, _)
            | TypedExpressionNode::BitwiseNot(qualified_type, _)
            | TypedExpressionNode::Not(qualified_type, _)
            | TypedExpressionNode::PostIncrement(qualified_type, _)
            | TypedExpressionNode::PostDecrement(qualified_type, _)
            | TypedExpressionNode::ArraySubscript(qualified_type, _, _)
            | TypedExpressionNode::FunctionCall(qualified_type, _, _)
            | TypedExpressionNode::DotAccess(qualified_type, _, _)
            | TypedExpressionNode::ArrowAccess(qualified_type, _, _)
            | TypedExpressionNode::Identifier(qualified_type, _)
            | TypedExpressionNode::Constant(qualified_type, _)
            | TypedExpressionNode::StringLiteral(qualified_type, _) => qualified_type,
        }
    }
}
