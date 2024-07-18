// 

pub(crate) enum ASTNode {
    Expression(ExpressionNode)
}

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

    Ternary(Box<ExpressionNode>, Box<ExpressionNode>, Box<ExpressionNode>),

    LogicalAnd(Box<ExpressionNode>, Box<ExpressionNode>),
    LogicalOr(Box<ExpressionNode>, Box<ExpressionNode>),
    BitwiseAnd(Box<ExpressionNode>, Box<ExpressionNode>),
    BitwiseOr(Box<ExpressionNode>, Box<ExpressionNode>),
    
    Equal(Box<ExpressionNode>, Box<ExpressionNode>),
    NotEqual(Box<ExpressionNode>, Box<ExpressionNode>),

    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,

    LShift,
    RShift,
    Multiply,
    Divide,
    Modulo,
    Add,
    Subtract,
    Cast,

    PreIncrement,
    PreDecrement,
    Sizeof, 
    AddressOf,
    Dereference,
    UnaryPlus,
    UnaryMinus,
    BitwiseNot,
    Not,

    PostIncrement,
    PostDecrement,
    ArraySubscript,
    FunctionCall,
    DotAccess,
    ArrowAccess,
    CompoundInitializer

}