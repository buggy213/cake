//

use std::rc::Rc;

use crate::semantics::{
    symtab::{Scope, StorageClass},
    types::{CType, FunctionSpecifier, QualifiedType},
};

#[derive(Debug, PartialEq)]
pub(crate) enum ASTNode {
    TranslationUnit(Vec<ASTNode>, Scope),

    FunctionDefinition(Box<Declaration>, Box<ASTNode>),
    Declaration(Vec<Declaration>), // identifier + (optional) initializer

    Label(Rc<ASTNode>, Identifier), // both symbol table and label node will have reference to labeled stmt
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
    ReturnStatement(Option<Box<ExpressionNode>>),
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

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum TypedExpressionNode {
    CommaExpr(Vec<TypedExpressionNode>, CType),

    SimpleAssign(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    MultiplyAssign(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    DivideAssign(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    ModuloAssign(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    AddAssign(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    SubAssign(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    LShiftAssign(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    RShiftAssign(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    AndAssign(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    XorAssign(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    OrAssign(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),

    Ternary(
        Box<TypedExpressionNode>,
        Box<TypedExpressionNode>,
        Box<TypedExpressionNode>,
        CType,
    ),

    LogicalAnd(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    LogicalOr(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    BitwiseAnd(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    BitwiseOr(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    BitwiseXor(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),

    Equal(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    NotEqual(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),

    LessThan(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    GreaterThan(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    LessThanOrEqual(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    GreaterThanOrEqual(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),

    LShift(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    RShift(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    Multiply(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    Divide(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    Modulo(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    Add(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    Subtract(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    Cast(Box<TypedExpressionNode>, QualifiedType),

    PreIncrement(Box<TypedExpressionNode>, CType),
    PreDecrement(Box<TypedExpressionNode>, CType),
    Sizeof(Box<TypedExpressionNode>, CType),
    AddressOf(Box<TypedExpressionNode>, CType),
    Dereference(Box<TypedExpressionNode>, CType),
    UnaryPlus(Box<TypedExpressionNode>, CType),
    UnaryMinus(Box<TypedExpressionNode>, CType),
    BitwiseNot(Box<TypedExpressionNode>, CType),
    Not(Box<TypedExpressionNode>, CType),

    PostIncrement(Box<TypedExpressionNode>, CType),
    PostDecrement(Box<TypedExpressionNode>, CType),
    ArraySubscript(Box<TypedExpressionNode>, Box<TypedExpressionNode>, CType),
    FunctionCall(Box<TypedExpressionNode>, Vec<TypedExpressionNode>, CType),
    DotAccess(Box<TypedExpressionNode>, Identifier, CType),
    ArrowAccess(Box<TypedExpressionNode>, Identifier, CType),
    // TODO: add support for compound initializers
    // CompoundInitializer
    Identifier(Identifier, CType),
    Constant(Constant, CType),
    StringLiteral(String, CType),
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
