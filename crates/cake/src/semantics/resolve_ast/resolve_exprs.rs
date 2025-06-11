use thiserror::Error;

use crate::parser::ast::{Constant, ExprRangeRef, ExprRef, ExpressionNode, TypedExpressionNode};
use crate::semantics::symtab::Symbol;
use crate::semantics::{constexpr::integer_constant_eval, symtab::SymbolTable};
use crate::types::{BasicType, CType, CanonicalType, QualifiedType, TypeQualifier};

use super::ASTResolveError;

#[derive(Debug, Error)]
pub(super) enum ResolveExprError {
    #[error("arithmetic operations require arithmetic operands")]
    ArithmeticExprBadOperandType,
    #[error("identifier not found")]
    IdentifierNotFound,
    #[error("unable to resolve function expression")]
    BadFunctionExpr,
}

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
pub(super) fn resolve_expr(
    expr: &ExpressionNode,
    resolved_expr_vec: &mut Vec<TypedExpressionNode>,
    expr_indices: &mut Vec<ExprRef>,
    symtab: &SymbolTable,
) -> Result<ExprRef, ResolveExprError> {
    match expr {
        ExpressionNode::CommaExpr(expression_nodes) => todo!(),
        ExpressionNode::SimpleAssign(expression_node, expression_node1) => todo!(),
        ExpressionNode::MultiplyAssign(expression_node, expression_node1) => todo!(),
        ExpressionNode::DivideAssign(expression_node, expression_node1) => todo!(),
        ExpressionNode::ModuloAssign(expression_node, expression_node1) => todo!(),
        ExpressionNode::AddAssign(expression_node, expression_node1) => todo!(),
        ExpressionNode::SubAssign(expression_node, expression_node1) => todo!(),
        ExpressionNode::LShiftAssign(expression_node, expression_node1) => todo!(),
        ExpressionNode::RShiftAssign(expression_node, expression_node1) => todo!(),
        ExpressionNode::AndAssign(expression_node, expression_node1) => todo!(),
        ExpressionNode::XorAssign(expression_node, expression_node1) => todo!(),
        ExpressionNode::OrAssign(expression_node, expression_node1) => todo!(),
        ExpressionNode::Ternary(expression_node, expression_node1, expression_node2) => todo!(),
        ExpressionNode::LogicalAnd(expression_node, expression_node1) => todo!(),
        ExpressionNode::LogicalOr(expression_node, expression_node1) => todo!(),
        ExpressionNode::BitwiseAnd(a, b) => {
            let bit_and_ref = basic_binary_op(
                a,
                b,
                TypedExpressionNode::BitwiseAnd,
                resolved_expr_vec,
                expr_indices,
                symtab,
            )?;
            Ok(bit_and_ref)
        }
        ExpressionNode::BitwiseOr(a, b) => {
            let bitor_ref = basic_binary_op(
                a,
                b,
                TypedExpressionNode::BitwiseOr,
                resolved_expr_vec,
                expr_indices,
                symtab,
            )?;
            Ok(bitor_ref)
        }
        ExpressionNode::BitwiseXor(a, b) => {
            let bitxor_ref = basic_binary_op(
                a,
                b,
                TypedExpressionNode::BitwiseXor,
                resolved_expr_vec,
                expr_indices,
                symtab,
            )?;
            Ok(bitxor_ref)
        }
        ExpressionNode::Equal(expression_node, expression_node1) => todo!(),
        ExpressionNode::NotEqual(expression_node, expression_node1) => todo!(),
        ExpressionNode::LessThan(expression_node, expression_node1) => todo!(),
        ExpressionNode::GreaterThan(expression_node, expression_node1) => todo!(),
        ExpressionNode::LessThanOrEqual(expression_node, expression_node1) => todo!(),
        ExpressionNode::GreaterThanOrEqual(expression_node, expression_node1) => todo!(),
        ExpressionNode::LShift(expression_node, expression_node1) => todo!(),
        ExpressionNode::RShift(expression_node, expression_node1) => todo!(),
        ExpressionNode::Multiply(a, b) => {
            let multiply_ref = basic_binary_op(
                a,
                b,
                TypedExpressionNode::Multiply,
                resolved_expr_vec,
                expr_indices,
                symtab,
            )?;
            Ok(multiply_ref)
        }
        ExpressionNode::Divide(a, b) => {
            let divide_ref = basic_binary_op(
                a,
                b,
                TypedExpressionNode::Divide,
                resolved_expr_vec,
                expr_indices,
                symtab,
            )?;
            Ok(divide_ref)
        }
        ExpressionNode::Modulo(a, b) => {
            let modulo_ref = basic_binary_op(
                a,
                b,
                TypedExpressionNode::Modulo,
                resolved_expr_vec,
                expr_indices,
                symtab,
            )?;
            Ok(modulo_ref)
        }
        ExpressionNode::Add(a, b) => {
            let add_ref = basic_binary_op(
                a,
                b,
                TypedExpressionNode::Add,
                resolved_expr_vec,
                expr_indices,
                symtab,
            )?;

            return Ok(add_ref);

            // TODO: add integer type to pointer
            todo!()
        }
        ExpressionNode::Subtract(a, b) => {
            let sub_ref = basic_binary_op(
                a,
                b,
                TypedExpressionNode::Subtract,
                resolved_expr_vec,
                expr_indices,
                symtab,
            )?;

            return Ok(sub_ref);

            // TODO: subtract 2 pointers, subtract integer from pointer
            todo!()
        }
        ExpressionNode::Cast(expression_node, qualified_type) => todo!(),
        ExpressionNode::PreIncrement(expression_node) => todo!(),
        ExpressionNode::PreDecrement(expression_node) => todo!(),
        ExpressionNode::Sizeof(expression_node) => todo!(),
        ExpressionNode::AddressOf(expression_node) => todo!(),
        ExpressionNode::Dereference(expression_node) => todo!(),
        ExpressionNode::UnaryPlus(expression_node) => todo!(),
        ExpressionNode::UnaryMinus(expression_node) => todo!(),
        ExpressionNode::BitwiseNot(expression_node) => todo!(),
        ExpressionNode::Not(expression_node) => todo!(),
        ExpressionNode::PostIncrement(expression_node) => todo!(),
        ExpressionNode::PostDecrement(expression_node) => todo!(),
        ExpressionNode::ArraySubscript(expression_node, expression_node1) => todo!(),
        ExpressionNode::FunctionCall(fn_expr, argument_exprs) => {
            // either a pointer to a function, or a "function designator" directly
            let fn_expr_ref = resolve_expr(&fn_expr, resolved_expr_vec, expr_indices, symtab)?;
            let mut argument_expr_refs = Vec::new();
            for argument in argument_exprs {
                let fn_arg_ref = resolve_expr(argument, resolved_expr_vec, expr_indices, symtab)?;
                argument_expr_refs.push(fn_arg_ref);
            }

            let fn_expr_type = resolved_expr_vec[fn_expr_ref.0 as usize].expr_type();
            let fn_expr_type_idx = match fn_expr_type {
                QualifiedType {
                    base_type: CType::FunctionTypeRef { symtab_idx },
                    qualifier,
                } => *symtab_idx,
                QualifiedType {
                    base_type: CType::PointerType { pointee_type },
                    qualifier,
                } => match pointee_type.as_ref() {
                    QualifiedType {
                        base_type: CType::FunctionTypeRef { symtab_idx },
                        qualifier,
                    } => *symtab_idx,
                    _ => return Err(ResolveExprError::BadFunctionExpr),
                },
                _ => {
                    return Err(ResolveExprError::BadFunctionExpr);
                }
            };

            // get return type
            let fn_canonical_type = symtab.get_canonical_type(fn_expr_type_idx);
            let fn_canonical_type = match fn_canonical_type {
                CanonicalType::FunctionType(fn_type_inner) => fn_type_inner,
                _ => panic!("corrupted symtab type table"),
            };

            let return_type = fn_canonical_type.return_type.as_ref().clone();

            // TODO: type check arguments (and insert appropriate casts if needed)

            let arg_range_start = expr_indices.len();
            expr_indices.extend(argument_expr_refs);
            let arg_range_end = expr_indices.len();

            let fn_args_ref = ExprRangeRef(arg_range_start as u32, arg_range_end as u32);
            let fn_call = TypedExpressionNode::FunctionCall(return_type, fn_expr_ref, fn_args_ref);
            let fn_call_ref = ExprRef(resolved_expr_vec.len() as u32);
            resolved_expr_vec.push(fn_call);

            Ok(fn_call_ref)
        }
        ExpressionNode::DotAccess(expression_node, identifier) => todo!(),
        ExpressionNode::ArrowAccess(expression_node, identifier) => todo!(),
        ExpressionNode::Identifier(identifier) => {
            let symbol = symtab.lookup_symbol(identifier.scope, &identifier.name);
            match symbol {
                None => return Err(ResolveExprError::IdentifierNotFound),

                Some(Symbol::Constant(c)) => {
                    let dummy = ExpressionNode::Constant(*c);
                    return resolve_expr(&dummy, resolved_expr_vec, expr_indices, symtab);
                }

                Some(Symbol::Function { function_type, .. }) => {
                    todo!()
                }

                Some(Symbol::Object { object_type, .. }) => {
                    let object_type = symtab.get_qualified_type(*object_type).clone();

                    let object = TypedExpressionNode::Identifier(object_type, identifier.clone());
                    let object_ref = ExprRef(resolved_expr_vec.len() as u32);
                    resolved_expr_vec.push(object);

                    return Ok(object_ref);
                }
            }

            todo!()
        }
        ExpressionNode::Constant(constant) => {
            let constant_type = QualifiedType {
                base_type: CType::BasicType {
                    basic_type: constant.basic_type(),
                },
                qualifier: TypeQualifier::empty(),
            };

            let constant = TypedExpressionNode::Constant(constant_type, *constant);
            let constant_ref = ExprRef(resolved_expr_vec.len() as u32);
            resolved_expr_vec.push(constant);

            Ok(constant_ref)
        }
        ExpressionNode::StringLiteral(_) => todo!(),
    }
}

fn usual_arithmetic_conversions(
    mut a_ref: ExprRef,
    mut b_ref: ExprRef,
    resolved_expr_vec: &mut Vec<TypedExpressionNode>,
) -> Result<(QualifiedType, ExprRef, ExprRef), ResolveExprError> {
    let a_type = &resolved_expr_vec[a_ref.0 as usize].expr_type().base_type;
    let b_type = &resolved_expr_vec[b_ref.0 as usize].expr_type().base_type;

    let (mut a_type, mut b_type) = match (a_type, b_type) {
        (CType::BasicType { basic_type: a_type }, CType::BasicType { basic_type: b_type }) => {
            (a_type, b_type)
        }
        _ => return Err(ResolveExprError::ArithmeticExprBadOperandType),
    };

    // no conversion needed
    if a_type == b_type {
        let common_type = QualifiedType {
            base_type: CType::BasicType {
                basic_type: *a_type,
            },
            qualifier: TypeQualifier::empty(),
        };
        return Ok((common_type, a_ref, b_ref));
    }

    fn basic_type_rank(basic_type: BasicType) -> u32 {
        match basic_type {
            BasicType::Char => 0,
            BasicType::UChar => 1,
            BasicType::Short => 2,
            BasicType::UShort => 3,
            BasicType::Int => 4,
            BasicType::UInt => 5,
            BasicType::Long => 6,
            BasicType::ULong => 7,
            BasicType::Float => 8,
            BasicType::Double => 9,
        }
    }

    // otherwise, convert from lower rank to higher one. i think this matches
    // description of conversions in 6.3.1.8 though not entirely confident
    let swap = basic_type_rank(*a_type) < basic_type_rank(*b_type);
    if swap {
        std::mem::swap(&mut a_ref, &mut b_ref);
        std::mem::swap(&mut a_type, &mut b_type);
    }

    // type qualifier only meaningful for lvalues, which this will not be
    let higher_type = QualifiedType {
        base_type: CType::BasicType {
            basic_type: *a_type,
        },
        qualifier: TypeQualifier::empty(),
    };

    let lower_type = QualifiedType {
        base_type: CType::BasicType {
            basic_type: *b_type,
        },
        qualifier: TypeQualifier::empty(),
    };

    let cast = TypedExpressionNode::Cast(higher_type.clone(), b_ref, lower_type);
    let b_ref = ExprRef(resolved_expr_vec.len() as u32);
    resolved_expr_vec.push(cast);

    if swap {
        Ok((higher_type, b_ref, a_ref))
    } else {
        Ok((higher_type, a_ref, b_ref))
    }
}

fn basic_binary_op(
    a: &ExpressionNode,
    b: &ExpressionNode,
    op_ctor: fn(QualifiedType, ExprRef, ExprRef) -> TypedExpressionNode,

    resolved_expr_vec: &mut Vec<TypedExpressionNode>,
    expr_indices: &mut Vec<ExprRef>,
    symtab: &SymbolTable,
) -> Result<ExprRef, ResolveExprError> {
    let a_ref = resolve_expr(a, resolved_expr_vec, expr_indices, symtab)?;
    let b_ref = resolve_expr(b, resolved_expr_vec, expr_indices, symtab)?;
    let (result_type, a_ref, b_ref) =
        usual_arithmetic_conversions(a_ref, b_ref, resolved_expr_vec)?;

    let result = op_ctor(result_type, a_ref, b_ref);
    let result_ref = ExprRef(resolved_expr_vec.len() as u32);
    resolved_expr_vec.push(result);

    Ok(result_ref)
}
