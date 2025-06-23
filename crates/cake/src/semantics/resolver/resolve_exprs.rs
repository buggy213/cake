use std::iter;

use thiserror::Error;

use crate::parser::ast::{Constant, ExpressionNode};
use crate::semantics::resolved_ast::{ExprRangeRef, ExprRef, MemberRef, TypedExpressionNode};
use crate::semantics::symtab::{Function, Object, ScopedSymtab, Symbol};
use crate::semantics::{constexpr::integer_constant_eval, symtab::SymbolTable};
use crate::types::{AggregateMember, BasicType, CType, TypeQualifier};

use super::ASTResolveError;

#[derive(Debug, Error)]
pub(super) enum ResolveExprError {
    #[error("arithmetic operations require arithmetic operands")]
    ArithmeticExprBadOperandType,
    #[error("identifier not found")]
    IdentifierNotFound,
    #[error("unable to resolve function expression")]
    BadFunctionExpr,
    #[error("attempt to use dot or arrow to access member of non-aggregate type")]
    BadMemberAccess,
    #[error("aggregate member does not exist")]
    MemberNotFound,
    #[error("failed assignment type conversion")]
    InvalidAssignmentConversion,
    #[error("function call has incorrect number of argumnets")]
    IncorrectNumberOfArguments,
}

pub(super) fn resolve_integer_constant_expression(
    symtab: &ScopedSymtab,
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
    symtab: &ScopedSymtab,
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

            let fn_expr_type = resolved_expr_vec[fn_expr_ref.0 as usize].expr_type();
            let fn_expr_type_idx = match fn_expr_type {
                CType::FunctionTypeRef { symtab_idx } => *symtab_idx,
                CType::PointerType { pointee_type, .. } => match pointee_type.as_ref() {
                    CType::FunctionTypeRef { symtab_idx } => *symtab_idx,
                    _ => return Err(ResolveExprError::BadFunctionExpr),
                },
                _ => {
                    return Err(ResolveExprError::BadFunctionExpr);
                }
            };

            // get return type
            let fn_canonical_type = symtab.get_function_type(fn_expr_type_idx);
            let return_type = fn_canonical_type.return_type.clone();

            // TODO: deal with varargs
            let mut argument_expr_refs = Vec::new();
            if fn_canonical_type.parameter_types.len() != argument_exprs.len() {
                return Err(ResolveExprError::IncorrectNumberOfArguments);
            }

            for ((_, formal_arg_type), actual_arg) in
                iter::zip(fn_canonical_type.parameter_types.iter(), argument_exprs)
            {
                let fn_arg_ref = resolve_expr(actual_arg, resolved_expr_vec, expr_indices, symtab)?;
                let converted_arg_ref =
                    assignment_type_conversion(formal_arg_type, fn_arg_ref, resolved_expr_vec)?;
                argument_expr_refs.push(converted_arg_ref);
            }

            let arg_range_start = expr_indices.len();
            expr_indices.extend(argument_expr_refs);
            let arg_range_end = expr_indices.len();

            let fn_args_ref = ExprRangeRef(arg_range_start as u32, arg_range_end as u32);
            let fn_call = TypedExpressionNode::FunctionCall(return_type, fn_expr_ref, fn_args_ref);
            let fn_call_ref = ExprRef(resolved_expr_vec.len() as u32);
            resolved_expr_vec.push(fn_call);

            Ok(fn_call_ref)
        }
        ExpressionNode::DotAccess(accessee, identifier) => {
            let accessee_ref = resolve_expr(&accessee, resolved_expr_vec, expr_indices, symtab)?;
            let accessee = &resolved_expr_vec[accessee_ref.0 as usize];
            let accessee_type = accessee.expr_type();

            let members = match accessee_type {
                CType::StructureTypeRef { symtab_idx, .. } => {
                    symtab.get_structure_type(*symtab_idx).members()
                }
                CType::UnionTypeRef { symtab_idx, .. } => {
                    symtab.get_union_type(*symtab_idx).members()
                }
                _ => {
                    return Err(ResolveExprError::BadMemberAccess);
                }
            };

            let pred = |(member_idx, member): (usize, &AggregateMember)| {
                if member.0 == identifier.name {
                    Some((member_idx, member.1.clone()))
                } else {
                    None
                }
            };

            let (member_idx, member_type) = if let Some((member_idx, member_type)) =
                members.iter().enumerate().find_map(pred)
            {
                (member_idx, member_type)
            } else {
                return Err(ResolveExprError::MemberNotFound);
            };

            let dot_access = TypedExpressionNode::DotAccess(
                member_type,
                accessee_ref,
                MemberRef(member_idx as u32),
            );
            let dot_access_ref = ExprRef(resolved_expr_vec.len() as u32);
            resolved_expr_vec.push(dot_access);

            Ok(dot_access_ref)
        }
        ExpressionNode::ArrowAccess(expression_node, identifier) => todo!(),
        ExpressionNode::Identifier(identifier) => {
            let symbol = symtab.lookup_symbol(identifier.scope, &identifier.name);
            match symbol {
                None => return Err(ResolveExprError::IdentifierNotFound),

                Some(Symbol::Constant(c)) => {
                    let dummy = ExpressionNode::Constant(*c);
                    return resolve_expr(&dummy, resolved_expr_vec, expr_indices, symtab);
                }

                Some(Symbol::Function(idx)) => {
                    let Function { function_type, .. } = symtab.get_function(*idx);
                    let function_type = CType::FunctionTypeRef {
                        symtab_idx: *function_type,
                    };

                    let function_expr =
                        TypedExpressionNode::FunctionIdentifier(function_type, *idx);
                    let function_ref = ExprRef(resolved_expr_vec.len() as u32);
                    resolved_expr_vec.push(function_expr);

                    return Ok(function_ref);
                }

                Some(Symbol::Object(idx)) => {
                    let Object { object_type, .. } = symtab.get_object(*idx);

                    let object_type = object_type.clone();

                    let object = TypedExpressionNode::ObjectIdentifier(object_type, *idx);
                    let object_ref = ExprRef(resolved_expr_vec.len() as u32);
                    resolved_expr_vec.push(object);

                    return Ok(object_ref);
                }
            }
        }
        ExpressionNode::Constant(constant) => {
            let constant_type = CType::BasicType {
                qualifier: TypeQualifier::empty(),
                basic_type: constant.basic_type(),
            };

            let constant = TypedExpressionNode::Constant(constant_type, *constant);
            let constant_ref = ExprRef(resolved_expr_vec.len() as u32);
            resolved_expr_vec.push(constant);

            Ok(constant_ref)
        }
        ExpressionNode::StringLiteral(string) => {
            // TODO: should technically be its own type for sizeof purposes
            let const_char_ptr = CType::PointerType {
                pointee_type: Box::new(CType::BasicType {
                    basic_type: BasicType::Char,
                    qualifier: TypeQualifier::Const,
                }),
                qualifier: TypeQualifier::empty(),
            };
            let string_literal = TypedExpressionNode::StringLiteral(const_char_ptr, string.clone());
            let string_literal_ref = ExprRef(resolved_expr_vec.len() as u32);
            resolved_expr_vec.push(string_literal);

            Ok(string_literal_ref)
        }
    }
}

fn usual_arithmetic_conversions(
    mut a_ref: ExprRef,
    mut b_ref: ExprRef,
    resolved_expr_vec: &mut Vec<TypedExpressionNode>,
) -> Result<(CType, ExprRef, ExprRef), ResolveExprError> {
    let a_type = resolved_expr_vec[a_ref.0 as usize].expr_type();
    let b_type = resolved_expr_vec[b_ref.0 as usize].expr_type();

    let (mut a_type, mut b_type) = match (a_type, b_type) {
        (
            CType::BasicType {
                basic_type: a_type, ..
            },
            CType::BasicType {
                basic_type: b_type, ..
            },
        ) => (a_type, b_type),
        _ => return Err(ResolveExprError::ArithmeticExprBadOperandType),
    };

    // no conversion needed
    if a_type == b_type {
        let common_type = CType::BasicType {
            qualifier: TypeQualifier::empty(),
            basic_type: *a_type,
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
    let higher_type = CType::BasicType {
        qualifier: TypeQualifier::empty(),
        basic_type: *a_type,
    };

    let lower_type = CType::BasicType {
        qualifier: TypeQualifier::empty(),
        basic_type: *b_type,
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
    op_ctor: fn(CType, ExprRef, ExprRef) -> TypedExpressionNode,

    resolved_expr_vec: &mut Vec<TypedExpressionNode>,
    expr_indices: &mut Vec<ExprRef>,
    symtab: &ScopedSymtab,
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

fn insert_cast(
    target: &CType,
    base: ExprRef,
    resolved_expr_vec: &mut Vec<TypedExpressionNode>,
) -> ExprRef {
    let base_type = resolved_expr_vec[base.0 as usize].expr_type();
    let cast = TypedExpressionNode::Cast(target.clone(), base, base_type.clone());
    let cast_ref = ExprRef(resolved_expr_vec.len() as u32);
    resolved_expr_vec.push(cast);
    cast_ref
}

// assignment type conversions: see
// https://www.gnu.org/software/c-intro-and-ref/manual/html_node/Assignment-Type-Conversions.html
// TODO: "upward compatibility" (isn't this essentially a no-op?) and think more about qualifiers
fn assignment_type_conversion(
    target: &CType,
    base_ref: ExprRef,
    resolved_expr_vec: &mut Vec<TypedExpressionNode>,
) -> Result<ExprRef, ResolveExprError> {
    let base_type = resolved_expr_vec[base_ref.0 as usize].expr_type();
    let base = &resolved_expr_vec[base_ref.0 as usize];

    if base_type == target {
        // no conversion needed
        return Ok(base_ref);
    }

    // 1. converting from any numeric type to any numeric type
    match (base_type, target) {
        (CType::BasicType { .. }, CType::BasicType { .. }) => {
            return Ok(insert_cast(target, base_ref, resolved_expr_vec));
        }
        _ => (),
    }

    match (base_type, target) {
        // 2. converting void* to any pointer type and vise-versa (except function pointers)
        (CType::PointerType { .. }, CType::PointerType { .. }) => {
            if base_type.is_void_pointer() && !target.is_function_pointer() {
                return Ok(insert_cast(target, base_ref, resolved_expr_vec));
            }

            if !base_type.is_function_pointer() && target.is_void_pointer() {
                return Ok(insert_cast(target, base_ref, resolved_expr_vec));
            }
        }
        _ => (),
    }

    // 3. converting 0 to any pointer type
    match (base, target) {
        (TypedExpressionNode::Constant(_, Constant::Int(0)), CType::PointerType { .. }) => {
            return Ok(insert_cast(target, base_ref, resolved_expr_vec));
        }
        _ => (),
    }

    // 4. converting any pointer type to bool
    // (TODO: support native bool type?)
    return Err(ResolveExprError::InvalidAssignmentConversion);
}

// used for varargs primarily
fn promote_argument() {
    todo!()
}
