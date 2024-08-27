//

use std::rc::Rc;

use thiserror::Error;

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
    is_typedef: bool,
    function_specifier: FunctionSpecifier,
    initializer: Option<Box<ExpressionNode>>,
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

#[derive(Debug, Error)]
pub(crate) enum ConstantExprError {
    #[error("error while parsing constant expression in preprocessor conditional directive")]
    PreprocessorConstantExprError,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct TypedExpressionNode {
    expr_type: CType,
    expr_node: ExpressionNode
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum ExpressionNode {
    CommaExpr(Vec<ExpressionNode>, ),

    SimpleAssign(Box<ExpressionNode>, Box<ExpressionNode>, ),
    MultiplyAssign(Box<ExpressionNode>, Box<ExpressionNode>, ),
    DivideAssign(Box<ExpressionNode>, Box<ExpressionNode>, ),
    ModuloAssign(Box<ExpressionNode>, Box<ExpressionNode>, ),
    AddAssign(Box<ExpressionNode>, Box<ExpressionNode>, ),
    SubAssign(Box<ExpressionNode>, Box<ExpressionNode>, ),
    LShiftAssign(Box<ExpressionNode>, Box<ExpressionNode>, ),
    RShiftAssign(Box<ExpressionNode>, Box<ExpressionNode>, ),
    AndAssign(Box<ExpressionNode>, Box<ExpressionNode>, ),
    XorAssign(Box<ExpressionNode>, Box<ExpressionNode>, ),
    OrAssign(Box<ExpressionNode>, Box<ExpressionNode>, ),

    Ternary(
        Box<ExpressionNode>,
        Box<ExpressionNode>,
        Box<ExpressionNode>,
        
    ),

    LogicalAnd(Box<ExpressionNode>, Box<ExpressionNode>, ),
    LogicalOr(Box<ExpressionNode>, Box<ExpressionNode>, ),
    BitwiseAnd(Box<ExpressionNode>, Box<ExpressionNode>, ),
    BitwiseOr(Box<ExpressionNode>, Box<ExpressionNode>, ),
    BitwiseXor(Box<ExpressionNode>, Box<ExpressionNode>, ),

    Equal(Box<ExpressionNode>, Box<ExpressionNode>, ),
    NotEqual(Box<ExpressionNode>, Box<ExpressionNode>, ),

    LessThan(Box<ExpressionNode>, Box<ExpressionNode>, ),
    GreaterThan(Box<ExpressionNode>, Box<ExpressionNode>, ),
    LessThanOrEqual(Box<ExpressionNode>, Box<ExpressionNode>, ),
    GreaterThanOrEqual(Box<ExpressionNode>, Box<ExpressionNode>, ),

    LShift(Box<ExpressionNode>, Box<ExpressionNode>, ),
    RShift(Box<ExpressionNode>, Box<ExpressionNode>, ),
    Multiply(Box<ExpressionNode>, Box<ExpressionNode>, ),
    Divide(Box<ExpressionNode>, Box<ExpressionNode>, ),
    Modulo(Box<ExpressionNode>, Box<ExpressionNode>, ),
    Add(Box<ExpressionNode>, Box<ExpressionNode>, ),
    Subtract(Box<ExpressionNode>, Box<ExpressionNode>, ),
    Cast(Box<ExpressionNode>, Box<ExpressionNode>, ),

    PreIncrement(Box<ExpressionNode>, ),
    PreDecrement(Box<ExpressionNode>, ),
    Sizeof(Box<ExpressionNode>, ),
    AddressOf(Box<ExpressionNode>, ),
    Dereference(Box<ExpressionNode>, ),
    UnaryPlus(Box<ExpressionNode>, ),
    UnaryMinus(Box<ExpressionNode>, ),
    BitwiseNot(Box<ExpressionNode>, ),
    Not(Box<ExpressionNode>, ),

    PostIncrement(Box<ExpressionNode>, ),
    PostDecrement(Box<ExpressionNode>, ),
    ArraySubscript(Box<ExpressionNode>, Box<ExpressionNode>, ),
    FunctionCall(Box<ExpressionNode>, Vec<ExpressionNode>, ),
    DotAccess(Box<ExpressionNode>, Identifier, ),
    ArrowAccess(Box<ExpressionNode>, Identifier, ),
    // TODO: add support for compound initializers
    // CompoundInitializer
    Identifier(Identifier, ),
    Constant(Constant),
    StringLiteral(String),
}

impl ExpressionNode {
    // TODO: research integer promotion / evaluation rules
    pub(crate) fn preprocessor_constant_eval(root: &ExpressionNode) -> Result<i64, ConstantExprError> {
        match root {
            ExpressionNode::CommaExpr(_)
            | ExpressionNode::SimpleAssign(_, _)
            | ExpressionNode::MultiplyAssign(_, _)
            | ExpressionNode::DivideAssign(_, _)
            | ExpressionNode::ModuloAssign(_, _)
            | ExpressionNode::AddAssign(_, _)
            | ExpressionNode::SubAssign(_, _)
            | ExpressionNode::LShiftAssign(_, _)
            | ExpressionNode::RShiftAssign(_, _)
            | ExpressionNode::AndAssign(_, _)
            | ExpressionNode::XorAssign(_, _)
            | ExpressionNode::OrAssign(_, _)
            | ExpressionNode::Identifier(_)
            | ExpressionNode::PostIncrement(_)
            | ExpressionNode::PostDecrement(_)
            | ExpressionNode::ArraySubscript(_, _)
            | ExpressionNode::FunctionCall(_, _)
            | ExpressionNode::DotAccess(_, _)
            | ExpressionNode::ArrowAccess(_, _)
            | ExpressionNode::Cast(_, _) // explicitly forbidden in standard (unclear why?)
            | ExpressionNode::PreIncrement(_)
            | ExpressionNode::PreDecrement(_)
            | ExpressionNode::AddressOf(_)
            | ExpressionNode::Dereference(_)
            | ExpressionNode::StringLiteral(_) => {
                return Err(ConstantExprError::PreprocessorConstantExprError);
            }

            ExpressionNode::Ternary(a, b, c) => {
                let a = Self::preprocessor_constant_eval(a)?;
                if a != 0 {
                    Self::preprocessor_constant_eval(b)
                } else {
                    Self::preprocessor_constant_eval(c)
                } 
            },
            ExpressionNode::LogicalAnd(lhs, rhs) => {
                if Self::preprocessor_constant_eval(lhs)? != 0 && 
                Self::preprocessor_constant_eval(rhs)? != 0 {
                    Ok(1)
                }
                else {
                    Ok(0)
                }
            },
            ExpressionNode::LogicalOr(lhs, rhs) => {
                if Self::preprocessor_constant_eval(lhs)? != 0 || 
                Self::preprocessor_constant_eval(rhs)? != 0 {
                    Ok(1)
                }
                else {
                    Ok(0)
                }
            },
            ExpressionNode::BitwiseAnd(lhs, rhs) => {
                Ok(Self::preprocessor_constant_eval(lhs)? & Self::preprocessor_constant_eval(rhs)?)
            },
            ExpressionNode::BitwiseOr(lhs, rhs) => {
                Ok(Self::preprocessor_constant_eval(lhs)? | Self::preprocessor_constant_eval(rhs)?)
            },
            ExpressionNode::BitwiseXor(lhs, rhs) => {
                Ok(Self::preprocessor_constant_eval(lhs)? ^ Self::preprocessor_constant_eval(rhs)?)
            },
            ExpressionNode::Equal(lhs, rhs) => {
                let val = if Self::preprocessor_constant_eval(lhs)? == Self::preprocessor_constant_eval(rhs)? { 1 } else { 0 };
                Ok(val)
            },
            ExpressionNode::NotEqual(lhs, rhs) => {
                let val = if Self::preprocessor_constant_eval(lhs)? != Self::preprocessor_constant_eval(rhs)? { 1 } else { 0 };
                Ok(val)
            },
            ExpressionNode::LessThan(lhs, rhs) => {
                let val = if Self::preprocessor_constant_eval(lhs)? < Self::preprocessor_constant_eval(rhs)? { 1 } else { 0 };
                Ok(val)
            },
            ExpressionNode::GreaterThan(lhs, rhs) => {
                let val = if Self::preprocessor_constant_eval(lhs)? > Self::preprocessor_constant_eval(rhs)? { 1 } else { 0 };
                Ok(val)
            }
            ExpressionNode::LessThanOrEqual(lhs, rhs) => {
                let val = if Self::preprocessor_constant_eval(lhs)? <= Self::preprocessor_constant_eval(rhs)? { 1 } else { 0 };
                Ok(val)
            },
            ExpressionNode::GreaterThanOrEqual(lhs, rhs) => {
                let val = if Self::preprocessor_constant_eval(lhs)? >= Self::preprocessor_constant_eval(rhs)? { 1 } else { 0 };
                Ok(val)
            },
            ExpressionNode::LShift(lhs, rhs) => {
                let val = Self::preprocessor_constant_eval(lhs)? << Self::preprocessor_constant_eval(rhs)?;
                Ok(val)
            },
            ExpressionNode::RShift(lhs, rhs) => {
                let val = Self::preprocessor_constant_eval(lhs)? >> Self::preprocessor_constant_eval(rhs)?;
                Ok(val)
            },
            ExpressionNode::Multiply(lhs, rhs) => {
                let val = Self::preprocessor_constant_eval(lhs)? * Self::preprocessor_constant_eval(rhs)?;
                Ok(val)
            },
            ExpressionNode::Divide(lhs, rhs) => {
                let val = Self::preprocessor_constant_eval(lhs)? / Self::preprocessor_constant_eval(rhs)?;
                Ok(val)
            },
            ExpressionNode::Modulo(lhs, rhs) => {
                let val = Self::preprocessor_constant_eval(lhs)? % Self::preprocessor_constant_eval(rhs)?;
                Ok(val)
            },
            ExpressionNode::Add(lhs, rhs) => {
                let val = Self::preprocessor_constant_eval(lhs)? + Self::preprocessor_constant_eval(rhs)?;
                Ok(val)
            },
            ExpressionNode::Subtract(lhs, rhs) => {
                let val = Self::preprocessor_constant_eval(lhs)? - Self::preprocessor_constant_eval(rhs)?;
                Ok(val)
            },
            
            ExpressionNode::Sizeof(_) => todo!("implement sizeof"),
            ExpressionNode::UnaryPlus(target) => {
                let val = Self::preprocessor_constant_eval(target)?;
                Ok(val)
            },
            ExpressionNode::UnaryMinus(target) => {
                let val = Self::preprocessor_constant_eval(target)?;
                Ok(-val)
            },
            ExpressionNode::BitwiseNot(target) => {
                let val = Self::preprocessor_constant_eval(target)?;
                Ok(!val)
            },
            ExpressionNode::Not(target) => {
                let val = if Self::preprocessor_constant_eval(target)? != 0 { 0 } else { 1 };
                Ok(val)
            },
            
            ExpressionNode::Constant(constant) => {
                match constant {
                    Constant::Int(v) => Ok(*v as i64),
                    Constant::LongInt(v) => Ok(*v as i64),
                    Constant::UInt(v) => Ok(*v as i64),
                    Constant::ULongInt(v) => Ok(*v as i64),
                    Constant::Float(v) => return Err(ConstantExprError::PreprocessorConstantExprError),
                    Constant::Double(v) => return Err(ConstantExprError::PreprocessorConstantExprError),
                }
            },
            
        }
    }
}
