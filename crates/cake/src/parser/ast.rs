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

impl ExpressionNode {
    // TODO: research integer promotion / evaluation rules
    pub(crate) fn preprocessor_constant_eval(root: &ExpressionNode) -> Result<i64, ConstantExprError> {
        match root {
            ExpressionNode::CommaExpr(_, _)
            | ExpressionNode::SimpleAssign(_, _, _)
            | ExpressionNode::MultiplyAssign(_, _, _)
            | ExpressionNode::DivideAssign(_, _, _)
            | ExpressionNode::ModuloAssign(_, _, _)
            | ExpressionNode::AddAssign(_, _, _)
            | ExpressionNode::SubAssign(_, _, _)
            | ExpressionNode::LShiftAssign(_, _, _)
            | ExpressionNode::RShiftAssign(_, _, _)
            | ExpressionNode::AndAssign(_, _, _)
            | ExpressionNode::XorAssign(_, _, _)
            | ExpressionNode::OrAssign(_, _, _)
            | ExpressionNode::Identifier(_, _)
            | ExpressionNode::PostIncrement(_, _)
            | ExpressionNode::PostDecrement(_, _)
            | ExpressionNode::ArraySubscript(_, _, _)
            | ExpressionNode::FunctionCall(_, _, _)
            | ExpressionNode::DotAccess(_, _, _)
            | ExpressionNode::ArrowAccess(_, _, _)
            | ExpressionNode::Cast(_, _, _) // explicitly forbidden in standard (unclear why?)
            | ExpressionNode::PreIncrement(_, _)
            | ExpressionNode::PreDecrement(_, _)
            | ExpressionNode::AddressOf(_, _)
            | ExpressionNode::Dereference(_, _)
            | ExpressionNode::StringLiteral(_) => {
                return Err(ConstantExprError::PreprocessorConstantExprError);
            }

            ExpressionNode::Ternary(a, b, c, _) => {
                let a = Self::preprocessor_constant_eval(a)?;
                if a != 0 {
                    Self::preprocessor_constant_eval(b)
                } else {
                    Self::preprocessor_constant_eval(c)
                } 
            },
            ExpressionNode::LogicalAnd(lhs, rhs, _) => {
                if Self::preprocessor_constant_eval(lhs)? != 0 && 
                Self::preprocessor_constant_eval(rhs)? != 0 {
                    Ok(1)
                }
                else {
                    Ok(0)
                }
            },
            ExpressionNode::LogicalOr(lhs, rhs, _) => {
                if Self::preprocessor_constant_eval(lhs)? != 0 || 
                Self::preprocessor_constant_eval(rhs)? != 0 {
                    Ok(1)
                }
                else {
                    Ok(0)
                }
            },
            ExpressionNode::BitwiseAnd(lhs, rhs, _) => {
                Ok(Self::preprocessor_constant_eval(lhs)? & Self::preprocessor_constant_eval(rhs)?)
            },
            ExpressionNode::BitwiseOr(lhs, rhs, _) => {
                Ok(Self::preprocessor_constant_eval(lhs)? | Self::preprocessor_constant_eval(rhs)?)
            },
            ExpressionNode::BitwiseXor(lhs, rhs, _) => {
                Ok(Self::preprocessor_constant_eval(lhs)? ^ Self::preprocessor_constant_eval(rhs)?)
            },
            ExpressionNode::Equal(lhs, rhs, _) => {
                let val = if Self::preprocessor_constant_eval(lhs)? == Self::preprocessor_constant_eval(rhs)? { 1 } else { 0 };
                Ok(val)
            },
            ExpressionNode::NotEqual(lhs, rhs, _) => {
                let val = if Self::preprocessor_constant_eval(lhs)? != Self::preprocessor_constant_eval(rhs)? { 1 } else { 0 };
                Ok(val)
            },
            ExpressionNode::LessThan(lhs, rhs, _) => {
                let val = if Self::preprocessor_constant_eval(lhs)? < Self::preprocessor_constant_eval(rhs)? { 1 } else { 0 };
                Ok(val)
            },
            ExpressionNode::GreaterThan(lhs, rhs, _) => {
                let val = if Self::preprocessor_constant_eval(lhs)? > Self::preprocessor_constant_eval(rhs)? { 1 } else { 0 };
                Ok(val)
            }
            ExpressionNode::LessThanOrEqual(lhs, rhs, _) => {
                let val = if Self::preprocessor_constant_eval(lhs)? <= Self::preprocessor_constant_eval(rhs)? { 1 } else { 0 };
                Ok(val)
            },
            ExpressionNode::GreaterThanOrEqual(lhs, rhs, _) => {
                let val = if Self::preprocessor_constant_eval(lhs)? >= Self::preprocessor_constant_eval(rhs)? { 1 } else { 0 };
                Ok(val)
            },
            ExpressionNode::LShift(lhs, rhs, _) => {
                let val = Self::preprocessor_constant_eval(lhs)? << Self::preprocessor_constant_eval(rhs)?;
                Ok(val)
            },
            ExpressionNode::RShift(lhs, rhs, _) => {
                let val = Self::preprocessor_constant_eval(lhs)? >> Self::preprocessor_constant_eval(rhs)?;
                Ok(val)
            },
            ExpressionNode::Multiply(lhs, rhs, _) => {
                let val = Self::preprocessor_constant_eval(lhs)? * Self::preprocessor_constant_eval(rhs)?;
                Ok(val)
            },
            ExpressionNode::Divide(lhs, rhs, _) => {
                let val = Self::preprocessor_constant_eval(lhs)? / Self::preprocessor_constant_eval(rhs)?;
                Ok(val)
            },
            ExpressionNode::Modulo(lhs, rhs, _) => {
                let val = Self::preprocessor_constant_eval(lhs)? % Self::preprocessor_constant_eval(rhs)?;
                Ok(val)
            },
            ExpressionNode::Add(lhs, rhs, _) => {
                let val = Self::preprocessor_constant_eval(lhs)? + Self::preprocessor_constant_eval(rhs)?;
                Ok(val)
            },
            ExpressionNode::Subtract(lhs, rhs, _) => {
                let val = Self::preprocessor_constant_eval(lhs)? - Self::preprocessor_constant_eval(rhs)?;
                Ok(val)
            },
            
            ExpressionNode::Sizeof(_, _) => todo!("implement sizeof"),
            ExpressionNode::UnaryPlus(target, _) => {
                let val = Self::preprocessor_constant_eval(target)?;
                Ok(val)
            },
            ExpressionNode::UnaryMinus(target, _) => {
                let val = Self::preprocessor_constant_eval(target)?;
                Ok(-val)
            },
            ExpressionNode::BitwiseNot(target, _) => {
                let val = Self::preprocessor_constant_eval(target)?;
                Ok(!val)
            },
            ExpressionNode::Not(target, _) => {
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
