use std::rc::Rc;

use crate::semantics::symtab::{Scope, StorageClass};

use crate::types::{BasicType, CType, FunctionSpecifier, QualifiedType};

#[derive(Debug, PartialEq)]
pub(crate) enum ASTNode {
    TranslationUnit(Vec<ASTNode>, Scope),

    FunctionDefinition(Box<Declaration>, Box<ASTNode>),
    Declaration(Vec<Declaration>), // identifier + (optional) initializer
    EmptyDeclaration(CType, Scope), // used for declaring new struct, but not creating an instance of it

    Label(Box<ASTNode>, Identifier), // both symbol table and label node will have reference to labeled stmt
    CaseLabel(Box<ASTNode>, Box<ExpressionNode>),
    DefaultLabel(Box<ASTNode>),

    CompoundStatement(Vec<ASTNode>, Scope),
    ExpressionStatement(Box<ExpressionNode>, Scope),
    NullStatement,
    IfStatement(
        Box<ExpressionNode>,
        Box<ASTNode>,
        Option<Box<ASTNode>>,
        Scope,
    ),
    SwitchStatement(Box<ExpressionNode>, Box<ASTNode>, Scope),
    WhileStatement(Box<ExpressionNode>, Box<ASTNode>, Scope),
    DoWhileStatement(Box<ExpressionNode>, Box<ASTNode>, Scope),
    ForStatement(
        Option<Box<ASTNode>>,
        Option<Box<ExpressionNode>>,
        Option<Box<ExpressionNode>>,
        Box<ASTNode>,
        Scope,
    ),

    GotoStatement(Identifier),
    ContinueStatement,
    BreakStatement,
    ReturnStatement(Option<Box<ExpressionNode>>, Scope),
}

#[derive(Debug, PartialEq)]
pub(crate) struct Declaration {
    pub(crate) name: Identifier,
    pub(crate) qualified_type: QualifiedType,
    pub(crate) storage_class: StorageClass,
    pub(crate) is_typedef: bool,
    pub(crate) function_specifier: FunctionSpecifier,
    pub(crate) initializer: Option<Box<ExpressionNode>>,
}

impl Declaration {
    pub fn new(
        name: Identifier,
        qualified_type: QualifiedType,
        storage_class: StorageClass,
        is_typedef: bool,
        function_specifier: FunctionSpecifier,
        initializer: Option<Box<ExpressionNode>>,
    ) -> Self {
        Self {
            name,
            qualified_type,
            storage_class,
            is_typedef,
            function_specifier,
            initializer,
        }
    }
}

// Essentially just an index into symbol table. Borrow checker makes reference difficult (though maybe this is something to look into)
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Identifier {
    pub(crate) scope: Scope,
    pub(crate) name: String,
}

impl Identifier {
    pub fn new(scope: Scope, name: String) -> Self {
        Identifier { scope, name }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Constant {
    // won't make distinction between long / long long
    Int(i32),
    LongInt(i64),
    UInt(u32),
    ULongInt(u64),

    // again, no difference between double / long double
    Float(f32),
    Double(f64),
    // enums have type int (6.4.4.3)
}

impl Constant {
    pub(crate) fn basic_type(self) -> BasicType {
        match self {
            Constant::Int(_) => BasicType::Int,
            Constant::LongInt(_) => BasicType::Long,
            Constant::UInt(_) => BasicType::UInt,
            Constant::ULongInt(_) => BasicType::ULong,
            Constant::Float(_) => BasicType::Float,
            Constant::Double(_) => BasicType::Double,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum ExpressionNode {
    CommaExpr(Vec<ExpressionNode>),

    SimpleAssign(Box<ExpressionNode>, Box<ExpressionNode>),
    MultiplyAssign(Box<ExpressionNode>, Box<ExpressionNode>),
    DivideAssign(Box<ExpressionNode>, Box<ExpressionNode>),
    ModuloAssign(Box<ExpressionNode>, Box<ExpressionNode>),
    AddAssign(Box<ExpressionNode>, Box<ExpressionNode>),
    SubAssign(Box<ExpressionNode>, Box<ExpressionNode>),
    LShiftAssign(Box<ExpressionNode>, Box<ExpressionNode>),
    RShiftAssign(Box<ExpressionNode>, Box<ExpressionNode>),
    AndAssign(Box<ExpressionNode>, Box<ExpressionNode>),
    XorAssign(Box<ExpressionNode>, Box<ExpressionNode>),
    OrAssign(Box<ExpressionNode>, Box<ExpressionNode>),

    Ternary(
        Box<ExpressionNode>,
        Box<ExpressionNode>,
        Box<ExpressionNode>,
    ),

    LogicalAnd(Box<ExpressionNode>, Box<ExpressionNode>),
    LogicalOr(Box<ExpressionNode>, Box<ExpressionNode>),
    BitwiseAnd(Box<ExpressionNode>, Box<ExpressionNode>),
    BitwiseOr(Box<ExpressionNode>, Box<ExpressionNode>),
    BitwiseXor(Box<ExpressionNode>, Box<ExpressionNode>),

    Equal(Box<ExpressionNode>, Box<ExpressionNode>),
    NotEqual(Box<ExpressionNode>, Box<ExpressionNode>),

    LessThan(Box<ExpressionNode>, Box<ExpressionNode>),
    GreaterThan(Box<ExpressionNode>, Box<ExpressionNode>),
    LessThanOrEqual(Box<ExpressionNode>, Box<ExpressionNode>),
    GreaterThanOrEqual(Box<ExpressionNode>, Box<ExpressionNode>),

    LShift(Box<ExpressionNode>, Box<ExpressionNode>),
    RShift(Box<ExpressionNode>, Box<ExpressionNode>),
    Multiply(Box<ExpressionNode>, Box<ExpressionNode>),
    Divide(Box<ExpressionNode>, Box<ExpressionNode>),
    Modulo(Box<ExpressionNode>, Box<ExpressionNode>),
    Add(Box<ExpressionNode>, Box<ExpressionNode>),
    Subtract(Box<ExpressionNode>, Box<ExpressionNode>),
    Cast(Box<ExpressionNode>, QualifiedType),

    PreIncrement(Box<ExpressionNode>),
    PreDecrement(Box<ExpressionNode>),
    Sizeof(Box<ExpressionNode>),
    AddressOf(Box<ExpressionNode>),
    Dereference(Box<ExpressionNode>),
    UnaryPlus(Box<ExpressionNode>),
    UnaryMinus(Box<ExpressionNode>),
    BitwiseNot(Box<ExpressionNode>),
    Not(Box<ExpressionNode>),

    PostIncrement(Box<ExpressionNode>),
    PostDecrement(Box<ExpressionNode>),
    ArraySubscript(Box<ExpressionNode>, Box<ExpressionNode>),
    FunctionCall(Box<ExpressionNode>, Vec<ExpressionNode>),
    DotAccess(Box<ExpressionNode>, Identifier),
    ArrowAccess(Box<ExpressionNode>, Identifier),
    // TODO: add support for compound initializers
    // CompoundInitializer
    Identifier(Identifier),
    Constant(Constant),
    StringLiteral(String),
}

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
    CommaExpr(QualifiedType, ExprRangeRef),

    SimpleAssign(QualifiedType, ExprRef, ExprRef),
    MultiplyAssign(QualifiedType, ExprRef, ExprRef),
    DivideAssign(QualifiedType, ExprRef, ExprRef),
    ModuloAssign(QualifiedType, ExprRef, ExprRef),
    AddAssign(QualifiedType, ExprRef, ExprRef),
    SubAssign(QualifiedType, ExprRef, ExprRef),
    LShiftAssign(QualifiedType, ExprRef, ExprRef),
    RShiftAssign(QualifiedType, ExprRef, ExprRef),
    AndAssign(QualifiedType, ExprRef, ExprRef),
    XorAssign(QualifiedType, ExprRef, ExprRef),
    OrAssign(QualifiedType, ExprRef, ExprRef),

    Ternary(QualifiedType, ExprRef, ExprRef, ExprRef),

    LogicalAnd(QualifiedType, ExprRef, ExprRef),
    LogicalOr(QualifiedType, ExprRef, ExprRef),
    BitwiseAnd(QualifiedType, ExprRef, ExprRef),
    BitwiseOr(QualifiedType, ExprRef, ExprRef),
    BitwiseXor(QualifiedType, ExprRef, ExprRef),

    Equal(QualifiedType, ExprRef, ExprRef),
    NotEqual(QualifiedType, ExprRef, ExprRef),

    LessThan(QualifiedType, ExprRef, ExprRef),
    GreaterThan(QualifiedType, ExprRef, ExprRef),
    LessThanOrEqual(QualifiedType, ExprRef, ExprRef),
    GreaterThanOrEqual(QualifiedType, ExprRef, ExprRef),

    LShift(QualifiedType, ExprRef, ExprRef),
    RShift(QualifiedType, ExprRef, ExprRef),
    Multiply(QualifiedType, ExprRef, ExprRef),
    Divide(QualifiedType, ExprRef, ExprRef),
    Modulo(QualifiedType, ExprRef, ExprRef),
    Add(QualifiedType, ExprRef, ExprRef),
    Subtract(QualifiedType, ExprRef, ExprRef),
    Cast(QualifiedType, ExprRef, QualifiedType),

    PreIncrement(QualifiedType, ExprRef),
    PreDecrement(QualifiedType, ExprRef),
    Sizeof(QualifiedType, ExprRef),
    AddressOf(QualifiedType, ExprRef),
    Dereference(QualifiedType, ExprRef),
    UnaryPlus(QualifiedType, ExprRef),
    UnaryMinus(QualifiedType, ExprRef),
    BitwiseNot(QualifiedType, ExprRef),
    Not(QualifiedType, ExprRef),

    PostIncrement(QualifiedType, ExprRef),
    PostDecrement(QualifiedType, ExprRef),
    ArraySubscript(QualifiedType, ExprRef, ExprRef),
    FunctionCall(QualifiedType, ExprRef, ExprRangeRef),
    DotAccess(QualifiedType, ExprRef, Identifier),
    ArrowAccess(QualifiedType, ExprRef, Identifier),
    // TODO: add support for compound initializers
    // CompoundInitializer
    Identifier(QualifiedType, Identifier),
    Constant(QualifiedType, Constant),
    StringLiteral(QualifiedType, String),
}

impl TypedExpressionNode {
    pub(crate) fn expr_type(&self) -> &QualifiedType {
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
