// 

use std::rc::Rc;

use crate::semantics::{symtab::Scope, types::CType};

// AST does not record declarations directly, instead just points to symbol table
// might want to look into more advanced allocation strategy if requiring mutability in AST
// i think this should be ok for now
pub(crate) enum ASTNode {
    TranslationUnit(Vec<ASTNode>, Scope),

    FunctionDefinition(Box<ASTNode>, Scope),
    Declaration(Vec<Identifier>),

    Label(Rc<ASTNode>, Identifier), // both symbol table and AST keep a reference to labeled statement
    CaseLabel(Box<ASTNode>),
    DefaultLabel(Box<ASTNode>),

    CompoundStatement(Vec<ASTNode>, Scope),
    ExpressionStatement(Box<ExpressionNode>, Scope),
    IfStatement(Box<ExpressionNode>, Box<ASTNode>, Scope),
    SwitchStatement(Box<ExpressionNode>, Box<ASTNode>, Scope),
    WhileStatement(Box<ExpressionNode>, Box<ASTNode>, Scope),
    DoWhileStatement(Box<ExpressionNode>, Box<ASTNode>, Scope),
    ForStatement(Option<Box<ASTNode>>, Option<Box<ExpressionNode>>, Option<Box<ExpressionNode>>, Scope),
    
    GotoStatement(Identifier),
    ContinueStatement,
    BreakStatement,
    ReturnStatement(Option<Box<ExpressionNode>>)
}

// Essentially just an index into symbol table. Borrow checker makes reference difficult (though maybe this is something to look into)
pub(crate) struct Identifier {
    scope: Scope,
    name: String
}

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

pub(crate) enum ExpressionNode {
    CommaExpr(Vec<ExpressionNode>, CType),
    
    SimpleAssign(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    MultiplyAssign(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    DivideAssign(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    ModuloAssign(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    AddAssign(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    SubAssign(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    LShiftAssign(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    RShiftAssign(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    AndAssign(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    XorAssign(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    OrAssign(Box<ExpressionNode>, Box<ExpressionNode>, CType),

    Ternary(Box<ExpressionNode>, Box<ExpressionNode>, Box<ExpressionNode>, CType),

    LogicalAnd(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    LogicalOr(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    BitwiseAnd(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    BitwiseOr(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    
    Equal(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    NotEqual(Box<ExpressionNode>, Box<ExpressionNode>, CType),

    LessThan(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    GreaterThan(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    LessThanOrEqual(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    GreaterThanOrEqual(Box<ExpressionNode>, Box<ExpressionNode>, CType),

    LShift(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    RShift(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    Multiply(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    Divide(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    Modulo(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    Add(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    Subtract(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    Cast(Box<ExpressionNode>, Box<ExpressionNode>, CType),

    PreIncrement(Box<ExpressionNode>, CType),
    PreDecrement(Box<ExpressionNode>, CType),
    Sizeof(Box<ExpressionNode>, CType), 
    AddressOf(Box<ExpressionNode>, CType),
    Dereference(Box<ExpressionNode>, CType),
    UnaryPlus(Box<ExpressionNode>, CType),
    UnaryMinus(Box<ExpressionNode>, CType),
    BitwiseNot(Box<ExpressionNode>, CType),
    Not(Box<ExpressionNode>, CType),

    PostIncrement(Box<ExpressionNode>, CType),
    PostDecrement(Box<ExpressionNode>, CType),
    ArraySubscript(Box<ExpressionNode>, Box<ExpressionNode>, CType),
    FunctionCall(Box<ExpressionNode>, Vec<ExpressionNode>, CType),
    DotAccess(Box<ExpressionNode>, Identifier),
    ArrowAccess(Box<ExpressionNode>, Identifier),
    // TODO: add support for compound initializers
    // CompoundInitializer

    Identifier(Identifier),
    Constant(Constant),
    StringLiteral(String)

}