use crate::parser::ast::{Constant, ExpressionNode};
use crate::semantics::{constexpr::integer_constant_eval, symtab::SymbolTable};

use super::ASTResolveError;

pub(super) fn resolve_integer_constant_expression(
    symtab: &SymbolTable,
    expr: &ExpressionNode,
) -> Result<Constant, ASTResolveError> {
    let constant = integer_constant_eval(symtab, expr);
    match constant {
        Ok(c) => match c {
            Constant::Int(_) => todo!(),
            Constant::LongInt(_) => todo!(),
            Constant::UInt(_) => todo!(),
            Constant::ULongInt(_) => todo!(),
            Constant::Float(_) => Err(ASTResolveError::IntegerConstantExprError),
            Constant::Double(_) => Err(ASTResolveError::IntegerConstantExprError),
        },
        Err(e) => Err(ASTResolveError::IntegerConstantExprError),
    }
}

// performs type checking for expressions, inserts integer promotions, validates casts
pub(super) fn resolve_expr(expr: &mut ExpressionNode) {
    match expr {
        ExpressionNode::CommaExpr(_) => {}
        ExpressionNode::SimpleAssign(_, _) => {}
        ExpressionNode::MultiplyAssign(_, _) => {}
        ExpressionNode::DivideAssign(_, _) => {}
        ExpressionNode::ModuloAssign(_, _) => {}

        ExpressionNode::PreIncrement(value) | ExpressionNode::PreDecrement(value) => {
            // must be a "modifiable lvalue"
        }
        ExpressionNode::AddAssign(_, _) => {}
        ExpressionNode::SubAssign(_, _) => {}
        ExpressionNode::LShiftAssign(_, _) => {}
        ExpressionNode::RShiftAssign(_, _) => {}
        ExpressionNode::AndAssign(_, _) => {}
        ExpressionNode::XorAssign(_, _) => {}
        ExpressionNode::OrAssign(_, _) => {}

        ExpressionNode::Ternary(_, _, _) => {}
        ExpressionNode::LogicalAnd(_, _) => {}
        ExpressionNode::LogicalOr(_, _) => {}
        ExpressionNode::BitwiseAnd(_, _) => {}
        ExpressionNode::BitwiseOr(_, _) => {}
        ExpressionNode::BitwiseXor(_, _) => {}
        ExpressionNode::Equal(_, _) => {}
        ExpressionNode::NotEqual(_, _) => {}
        ExpressionNode::LessThan(_, _) => {}
        ExpressionNode::GreaterThan(_, _) => {}
        ExpressionNode::LessThanOrEqual(_, _) => {}
        ExpressionNode::GreaterThanOrEqual(_, _) => {}
        ExpressionNode::LShift(_, _) => {}
        ExpressionNode::RShift(_, _) => {}

        ExpressionNode::Multiply(a, b) | ExpressionNode::Divide(a, b) => {
            // ensure a, b are the right types
        }
        ExpressionNode::Modulo(a, b) => {}

        ExpressionNode::Add(a, b) | ExpressionNode::Subtract(a, b) => {}

        ExpressionNode::Cast(_, _) => {}

        ExpressionNode::Sizeof(_) => {}
        ExpressionNode::AddressOf(_) => {}
        ExpressionNode::Dereference(_) => {}
        ExpressionNode::UnaryPlus(_) => {}
        ExpressionNode::UnaryMinus(_) => {}
        ExpressionNode::BitwiseNot(_) => {}
        ExpressionNode::Not(_) => {}
        ExpressionNode::PostIncrement(_) => {}
        ExpressionNode::PostDecrement(_) => {}
        ExpressionNode::ArraySubscript(_, _) => {}
        ExpressionNode::FunctionCall(_, _) => {}
        ExpressionNode::DotAccess(_, _) => {}
        ExpressionNode::ArrowAccess(_, _) => {}
        ExpressionNode::Identifier(_) => {}
        ExpressionNode::Constant(_) => {}
        ExpressionNode::StringLiteral(_) => {}
    }
}
