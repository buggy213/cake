//

use std::rc::Rc;

use crate::semantics::{
    symtab::{Scope, StorageClass},
    types::{CType, FunctionSpecifier, QualifiedType},
};

#[derive(Debug, PartialEq)]
pub(crate) enum ASTNode {
    TranslationUnit(Vec<ASTNode>, Scope),

    FunctionDefinition(Box<Declaration>, Box<ASTNode>, Scope),
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
    name: Identifier,
    qualified_type: QualifiedType,
    storage_class: StorageClass,
    function_specifier: FunctionSpecifier,
    initializer: Option<Box<ExpressionNode>>,
}

impl Declaration {
    pub fn new(
        name: Identifier,
        qualified_type: QualifiedType,
        storage_class: StorageClass,
        function_specifier: FunctionSpecifier,
        initializer: Option<Box<ExpressionNode>>,
    ) -> Self {
        Self {
            name,
            qualified_type,
            storage_class,
            function_specifier,
            initializer,
        }
    }
}

// Essentially just an index into symbol table. Borrow checker makes reference difficult (though maybe this is something to look into)
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Identifier {
    scope: Scope,
    name: String,
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
pub(crate) enum ExpressionNode {
    CommaExpr(Vec<ExpressionNode>, Option<CType>),

    SimpleAssign(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    MultiplyAssign(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    DivideAssign(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    ModuloAssign(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    AddAssign(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    SubAssign(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    LShiftAssign(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    RShiftAssign(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    AndAssign(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    XorAssign(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    OrAssign(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),

    Ternary(
        Box<ExpressionNode>,
        Box<ExpressionNode>,
        Box<ExpressionNode>,
        Option<CType>,
    ),

    LogicalAnd(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    LogicalOr(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    BitwiseAnd(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    BitwiseOr(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    BitwiseXor(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),

    Equal(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    NotEqual(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),

    LessThan(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    GreaterThan(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    LessThanOrEqual(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    GreaterThanOrEqual(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),

    LShift(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    RShift(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    Multiply(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    Divide(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    Modulo(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    Add(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    Subtract(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    Cast(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),

    PreIncrement(Box<ExpressionNode>, Option<CType>),
    PreDecrement(Box<ExpressionNode>, Option<CType>),
    Sizeof(Box<ExpressionNode>, Option<CType>),
    AddressOf(Box<ExpressionNode>, Option<CType>),
    Dereference(Box<ExpressionNode>, Option<CType>),
    UnaryPlus(Box<ExpressionNode>, Option<CType>),
    UnaryMinus(Box<ExpressionNode>, Option<CType>),
    BitwiseNot(Box<ExpressionNode>, Option<CType>),
    Not(Box<ExpressionNode>, Option<CType>),

    PostIncrement(Box<ExpressionNode>, Option<CType>),
    PostDecrement(Box<ExpressionNode>, Option<CType>),
    ArraySubscript(Box<ExpressionNode>, Box<ExpressionNode>, Option<CType>),
    FunctionCall(Box<ExpressionNode>, Vec<ExpressionNode>, Option<CType>),
    DotAccess(Box<ExpressionNode>, Identifier, Option<CType>),
    ArrowAccess(Box<ExpressionNode>, Identifier, Option<CType>),
    // TODO: add support for compound initializers
    // CompoundInitializer
    Identifier(Identifier, Option<CType>),
    Constant(Constant),
    StringLiteral(String),
}
