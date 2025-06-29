use std::{ops::Range, slice::SliceIndex};

use cake_util::make_type_idx;

use crate::{
    parser::ast::{Constant, Identifier},
    semantics::symtab::{FunctionIdx, ObjectIdx},
    types::CType,
};

#[derive(Debug, PartialEq, Copy, Clone)]
pub(crate) struct NodeRef(pub u32);

#[derive(Debug, PartialEq, Copy, Clone)]
pub(crate) struct NodeRangeRef(pub u32, pub u32);

make_type_idx!(ExprRef, TypedExpressionNode);

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) struct ContextRef(pub u32);

#[derive(Debug, PartialEq, Copy, Clone)]
pub(crate) struct ExprRangeRef(pub u32, pub u32);
impl From<ExprRangeRef> for Range<usize> {
    fn from(value: ExprRangeRef) -> Self {
        value.0 as usize..value.1 as usize
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub(crate) struct MemberRef(pub u32);

#[derive(Debug, PartialEq)]
pub(crate) enum ResolvedASTNode {
    TranslationUnit {
        children: NodeRangeRef,
    },
    FunctionDefinition {
        parent: NodeRef,
        symbol_idx: FunctionIdx,
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
    },
    ExpressionStatement {
        parent: NodeRef,
        expr: ExprRef,
    },
    IfStatement {
        parent: NodeRef,
        condition: ExprRef,
        taken: NodeRef,
        not_taken: Option<NodeRef>,
    },
    SwitchStatement {
        parent: NodeRef,
        controlling_expr: ExprRef,
        body: NodeRef,
        context: ContextRef,
    },
    WhileStatement {
        parent: NodeRef,
        condition: ExprRef,
        body: NodeRef,
    },
    DoWhileStatement {
        parent: NodeRef,
        condition: ExprRef,
        body: NodeRef,
    },
    ForStatement {
        parent: NodeRef,
        init: Option<NodeRef>,
        condition: Option<ExprRef>,
        post_body: Option<ExprRef>,
        body: NodeRef,
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
    },
}

// First member (.0) is always the type of the expression
#[derive(Clone, PartialEq, Debug)]
pub(crate) enum TypedExpressionNode {
    CommaExpr(CType, ExprRangeRef),

    SimpleAssign(CType, ExprRef, ExprRef),

    // a += b is not equivalent to a = a + b since the lvalue
    // of a is only evaluated once
    // ex:
    // int *has_side_effects();
    // *(has_side_effects()) += 1;
    // *(has_side_effects()) = *(has_side_effects()) + 1;
    // As such, we keep around the lvalue as the first ExprRef and
    // the casts / operation / rhs / casts as the second ExprRef.
    // codegen is responsible for ensuring lvalue is only evaluated once
    MultiplyAssign(CType, ExprRef, ExprRef),
    DivideAssign(CType, ExprRef, ExprRef),
    ModuloAssign(CType, ExprRef, ExprRef),
    AddAssign(CType, ExprRef, ExprRef),
    SubAssign(CType, ExprRef, ExprRef),
    PtrAddAssign(CType, ExprRef, ExprRef),
    PtrSubAssign(CType, ExprRef, ExprRef),
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

    // pointer arithmetic given separate variants
    PointerAdd(CType, ExprRef, ExprRef),
    PointerSub(CType, ExprRef, ExprRef),
    PointerDiff(CType, ExprRef, ExprRef),

    // destination type, reference, source type
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

    DirectFunctionCall(CType, FunctionIdx, ExprRangeRef),
    IndirectFunctionCall(CType, ExprRef, ExprRangeRef),

    DotAccess(CType, ExprRef, MemberRef),
    ArrowAccess(CType, ExprRef, MemberRef),

    // TODO: add support for compound initializers
    // CompoundInitializer
    ArrayDecay(CType, ObjectIdx),
    ObjectIdentifier(CType, ObjectIdx),
    FunctionIdentifier(CType, FunctionIdx),

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
            | TypedExpressionNode::PtrAddAssign(qualified_type, _, _)
            | TypedExpressionNode::PtrSubAssign(qualified_type, _, _)
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
            | TypedExpressionNode::PointerAdd(qualified_type, _, _)
            | TypedExpressionNode::PointerSub(qualified_type, _, _)
            | TypedExpressionNode::PointerDiff(qualified_type, _, _)
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
            | TypedExpressionNode::DirectFunctionCall(qualified_type, _, _)
            | TypedExpressionNode::IndirectFunctionCall(qualified_type, _, _)
            | TypedExpressionNode::DotAccess(qualified_type, _, _)
            | TypedExpressionNode::ArrowAccess(qualified_type, _, _)
            | TypedExpressionNode::ArrayDecay(qualified_type, _)
            | TypedExpressionNode::ObjectIdentifier(qualified_type, _)
            | TypedExpressionNode::FunctionIdentifier(qualified_type, _)
            | TypedExpressionNode::Constant(qualified_type, _)
            | TypedExpressionNode::StringLiteral(qualified_type, _) => qualified_type,
        }
    }
}
