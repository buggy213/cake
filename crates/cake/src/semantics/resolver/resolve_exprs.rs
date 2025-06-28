use std::iter;

use thiserror::Error;

use crate::parser::ast::{Constant, ExpressionNode, Identifier};
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
    #[error("expression assigned to is not an lvalue")]
    ExpressionNotLvalue,
    #[error("logical operators require operands to be scalar types")]
    LogicalOperationBadOperands,
    #[error(
        "relational operator requires both to be arithmetic or both to be pointer to compatible type"
    )]
    RelationalOperatorBadOperands,
    #[error("not operator requires operand have scalar (arithmetic / pointer) type")]
    BadUnaryNotOperand,
    #[error("dereference operator requires operand be pointer")]
    BadDereferenceOperand,
    #[error("shift operators require both operands be integral")]
    BadShiftOperands,
    #[error(
        "subscript operator requires one operand be pointer to object, one operand be integral"
    )]
    BadSubscriptOperands,
    #[error("subtraction of two incompatible pointer types")]
    PtrDiffIncompatibleTypes,
    #[error("arrow operator requires left hand side be pointer")]
    BadArrowOperator,
    #[error("casts only allowed between scalar types")]
    NonScalarCast,
    #[error("unary operators +, - require arithmetic operand. ~ requires integral operand")]
    BadUnaryArithmeticOperand,
    #[error("integer promotion requires arithmetic operand")]
    ExpectedArithmeticType,
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
        ExpressionNode::CommaExpr(expression_nodes) => {
            let mut subexpr_refs = Vec::new();
            for subexpr in expression_nodes {
                let subexpr_ref = resolve_expr(subexpr, resolved_expr_vec, expr_indices, symtab)?;
                subexpr_refs.push(subexpr_ref);
            }

            let start = expr_indices.len() as u32;
            expr_indices.extend(subexpr_refs);
            let end = expr_indices.len() as u32;

            let subexpr_range = ExprRangeRef(start, end);
            let final_subexpr = &resolved_expr_vec[*expr_indices.last().unwrap()];
            let final_subexpr_type = final_subexpr.expr_type().clone();

            let comma = TypedExpressionNode::CommaExpr(final_subexpr_type, subexpr_range);
            let comma_ref = ExprRef::from_push(resolved_expr_vec, comma);
            Ok(comma_ref)
        }
        ExpressionNode::SimpleAssign(lhs, rhs) => {
            // ensure LHS is a modifiable l-value
            let lhs_ref = resolve_expr(lhs, resolved_expr_vec, expr_indices, symtab)?;
            let lhs_is_lvalue = check_lvalue(lhs_ref, resolved_expr_vec);
            let lhs_type = resolved_expr_vec[lhs_ref].expr_type().clone();
            let lhs_type_qualifier = lhs_type.qualifier();
            let lhs_is_const = lhs_type_qualifier.contains(TypeQualifier::Const);

            if !lhs_is_lvalue || lhs_is_const {
                return Err(ResolveExprError::ExpressionNotLvalue);
            }

            let rhs_ref = resolve_expr(rhs, resolved_expr_vec, expr_indices, symtab)?;
            let converted_rhs_ref =
                assignment_type_conversion(&lhs_type, rhs_ref, resolved_expr_vec)?;

            let simple_assign =
                TypedExpressionNode::SimpleAssign(lhs_type, lhs_ref, converted_rhs_ref);
            let simple_assign_ref = ExprRef::from_push(resolved_expr_vec, simple_assign);

            Ok(simple_assign_ref)
        }
        ExpressionNode::MultiplyAssign(a, b) => {
            todo!("think about how best to implement this (and other augmented assigns)")
        }
        ExpressionNode::DivideAssign(a, b) => todo!(),
        ExpressionNode::ModuloAssign(a, b) => todo!(),
        ExpressionNode::AddAssign(a, b) => todo!(),
        ExpressionNode::SubAssign(a, b) => todo!(),
        ExpressionNode::LShiftAssign(a, b) => todo!(),
        ExpressionNode::RShiftAssign(a, b) => todo!(),
        ExpressionNode::AndAssign(a, b) => todo!(),
        ExpressionNode::XorAssign(a, b) => todo!(),
        ExpressionNode::OrAssign(a, b) => todo!(),
        ExpressionNode::Ternary(a, b, expression_node2) => todo!(),
        ExpressionNode::LogicalAnd(lhs, rhs) => {
            let lhs_ref = resolve_expr(&lhs, resolved_expr_vec, expr_indices, symtab)?;
            let rhs_ref = resolve_expr(&rhs, resolved_expr_vec, expr_indices, symtab)?;
            let lhs_type = resolved_expr_vec[lhs_ref].expr_type();
            let rhs_type = resolved_expr_vec[rhs_ref].expr_type();

            if !lhs_type.is_scalar_type() || !rhs_type.is_scalar_type() {
                return Err(ResolveExprError::LogicalOperationBadOperands);
            }

            let result_type = CType::int();

            let logical_and = TypedExpressionNode::LogicalAnd(result_type, lhs_ref, rhs_ref);
            let logical_and_ref = ExprRef::from_push(resolved_expr_vec, logical_and);

            Ok(logical_and_ref)
        }
        ExpressionNode::LogicalOr(lhs, rhs) => {
            let lhs_ref = resolve_expr(&lhs, resolved_expr_vec, expr_indices, symtab)?;
            let rhs_ref = resolve_expr(&rhs, resolved_expr_vec, expr_indices, symtab)?;
            let lhs_type = resolved_expr_vec[lhs_ref].expr_type();
            let rhs_type = resolved_expr_vec[rhs_ref].expr_type();

            if !lhs_type.is_scalar_type() || !rhs_type.is_scalar_type() {
                return Err(ResolveExprError::LogicalOperationBadOperands);
            }

            let result_type = CType::int();

            let logical_or = TypedExpressionNode::LogicalOr(result_type, lhs_ref, rhs_ref);
            let logical_or_ref = ExprRef::from_push(resolved_expr_vec, logical_or);

            Ok(logical_or_ref)
        }
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
        ExpressionNode::Equal(a, b) => relational_operator(
            a,
            b,
            TypedExpressionNode::Equal,
            resolved_expr_vec,
            expr_indices,
            symtab,
        ),
        ExpressionNode::NotEqual(a, b) => relational_operator(
            a,
            b,
            TypedExpressionNode::NotEqual,
            resolved_expr_vec,
            expr_indices,
            symtab,
        ),
        ExpressionNode::LessThan(a, b) => relational_operator(
            a,
            b,
            TypedExpressionNode::LessThan,
            resolved_expr_vec,
            expr_indices,
            symtab,
        ),
        ExpressionNode::GreaterThan(a, b) => relational_operator(
            a,
            b,
            TypedExpressionNode::GreaterThan,
            resolved_expr_vec,
            expr_indices,
            symtab,
        ),
        ExpressionNode::LessThanOrEqual(a, b) => relational_operator(
            a,
            b,
            TypedExpressionNode::LessThanOrEqual,
            resolved_expr_vec,
            expr_indices,
            symtab,
        ),
        ExpressionNode::GreaterThanOrEqual(a, b) => relational_operator(
            a,
            b,
            TypedExpressionNode::GreaterThanOrEqual,
            resolved_expr_vec,
            expr_indices,
            symtab,
        ),
        ExpressionNode::LShift(expr, shamt) => bitshift_operator(
            expr,
            shamt,
            TypedExpressionNode::LShift,
            resolved_expr_vec,
            expr_indices,
            symtab,
        ),
        ExpressionNode::RShift(expr, shamt) => bitshift_operator(
            expr,
            shamt,
            TypedExpressionNode::RShift,
            resolved_expr_vec,
            expr_indices,
            symtab,
        ),
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
            let a_ref = resolve_expr(a, resolved_expr_vec, expr_indices, symtab)?;
            let b_ref = resolve_expr(b, resolved_expr_vec, expr_indices, symtab)?;

            let a_type = resolved_expr_vec[a_ref].expr_type();
            let b_type = resolved_expr_vec[b_ref].expr_type();

            let a_int_b_ptr = a_type.is_integer_type() && b_type.is_object_pointer();
            let a_ptr_b_int = a_type.is_object_pointer() && b_type.is_integer_type();

            if a_int_b_ptr || a_ptr_b_int {
                // pointer arithmetic
                let ptr_type = if a_int_b_ptr { b_type } else { a_type };
                let ptr_ref = if a_int_b_ptr { b_ref } else { a_ref };
                let int_ref = if a_int_b_ptr { a_ref } else { b_ref };
                let pointer_add =
                    TypedExpressionNode::PointerAdd(ptr_type.clone(), ptr_ref, int_ref);
                let pointer_add_ref = ExprRef::from_push(resolved_expr_vec, pointer_add);
                Ok(pointer_add_ref)
            } else {
                // normal arithmetic
                let add_ref = basic_binary_op_inner(
                    a_ref,
                    b_ref,
                    TypedExpressionNode::Add,
                    resolved_expr_vec,
                )?;

                Ok(add_ref)
            }
        }
        ExpressionNode::Subtract(a, b) => {
            let a_ref = resolve_expr(a, resolved_expr_vec, expr_indices, symtab)?;
            let b_ref = resolve_expr(b, resolved_expr_vec, expr_indices, symtab)?;

            let a_type = resolved_expr_vec[a_ref].expr_type();
            let b_type = resolved_expr_vec[b_ref].expr_type();

            let a_int_b_ptr = a_type.is_integer_type() && b_type.is_object_pointer();
            let a_ptr_b_int = a_type.is_object_pointer() && b_type.is_integer_type();
            let a_ptr_b_ptr = a_type.is_object_pointer() && b_type.is_object_pointer();
            if a_ptr_b_ptr {
                // pointer difference
                // check if they are compatible
                if !CType::unqualified_equal(a_type, b_type) {
                    return Err(ResolveExprError::PtrDiffIncompatibleTypes);
                }

                let result_type = CType::ptrdiff_type();
                let ptrdiff = TypedExpressionNode::PointerDiff(result_type, a_ref, b_ref);
                let ptrdiff_ref = ExprRef::from_push(resolved_expr_vec, ptrdiff);
                Ok(ptrdiff_ref)
            } else if a_int_b_ptr || a_ptr_b_int {
                // pointer arithmetic
                let ptr_type = if a_int_b_ptr { b_type } else { a_type };
                let ptr_ref = if a_int_b_ptr { b_ref } else { a_ref };
                let int_ref = if a_int_b_ptr { a_ref } else { b_ref };
                let pointer_sub =
                    TypedExpressionNode::PointerSub(ptr_type.clone(), ptr_ref, int_ref);
                let pointer_sub_ref = ExprRef::from_push(resolved_expr_vec, pointer_sub);
                Ok(pointer_sub_ref)
            } else {
                // normal arithmetic
                let sub_ref = basic_binary_op_inner(
                    a_ref,
                    b_ref,
                    TypedExpressionNode::Subtract,
                    resolved_expr_vec,
                )?;
                Ok(sub_ref)
            }
        }
        ExpressionNode::Cast(expr, destination_type) => {
            // check that both have scalar type
            let expr_ref = resolve_expr(expr, resolved_expr_vec, expr_indices, symtab)?;
            let expr_type = resolved_expr_vec[expr_ref].expr_type();
            if !destination_type.is_scalar_type() || !expr_type.is_scalar_type() {
                return Err(ResolveExprError::NonScalarCast);
            }

            // can't use destination_type directly, have to resolve it (!!)
            // this requires scope and possible mutation of the symtab
            // (since you could, theoretically, declare a new type in a cast...)
            let cast = TypedExpressionNode::Cast(todo!(), expr_ref, expr_type.clone());
            let cast_ref = ExprRef::from_push(resolved_expr_vec, cast);
            Ok(cast_ref)
        }
        ExpressionNode::PreIncrement(expression_node) => todo!(),
        ExpressionNode::PreDecrement(expression_node) => todo!(),
        ExpressionNode::Sizeof(expression_node) => {
            todo!("implement sizeof")
        }
        ExpressionNode::AddressOf(pointee) => {
            let pointee_ref = resolve_expr(&pointee, resolved_expr_vec, expr_indices, symtab)?;
            let pointee_type = resolved_expr_vec[pointee_ref].expr_type();
            let pointer_type = CType::PointerType {
                pointee_type: Box::new(pointee_type.clone()),
                qualifier: TypeQualifier::empty(),
            };

            let address_of = TypedExpressionNode::AddressOf(pointer_type, pointee_ref);
            let address_of_ref = ExprRef::from_push(resolved_expr_vec, address_of);
            Ok(address_of_ref)
        }
        ExpressionNode::Dereference(pointer) => {
            let pointer_ref = resolve_expr(pointer, resolved_expr_vec, expr_indices, symtab)?;
            match resolved_expr_vec[pointer_ref].expr_type() {
                CType::PointerType { pointee_type, .. } => {
                    let pointee_type = pointee_type.as_ref().clone();
                    let deref = TypedExpressionNode::Dereference(pointee_type, pointer_ref);
                    let deref_ref = ExprRef::from_push(resolved_expr_vec, deref);
                    Ok(deref_ref)
                }
                _ => Err(ResolveExprError::BadDereferenceOperand),
            }
        }
        ExpressionNode::UnaryPlus(expr) => {
            let expr_ref = resolve_expr(expr, resolved_expr_vec, expr_indices, symtab)?;
            let expr_type = resolved_expr_vec[expr_ref].expr_type();

            if !expr_type.is_arithmetic_type() {
                return Err(ResolveExprError::BadUnaryArithmeticOperand);
            }

            let promoted_expr_ref = integer_promotion(expr_ref, resolved_expr_vec)?;
            let promoted_expr_type = resolved_expr_vec[promoted_expr_ref].expr_type();

            let unary_plus =
                TypedExpressionNode::UnaryPlus(promoted_expr_type.clone(), promoted_expr_ref);
            let unary_plus_ref = ExprRef::from_push(resolved_expr_vec, unary_plus);
            Ok(unary_plus_ref)
        }
        ExpressionNode::UnaryMinus(expr) => {
            let expr_ref = resolve_expr(expr, resolved_expr_vec, expr_indices, symtab)?;
            let expr_type = resolved_expr_vec[expr_ref].expr_type();

            if !expr_type.is_arithmetic_type() {
                return Err(ResolveExprError::BadUnaryArithmeticOperand);
            }

            let promoted_expr_ref = integer_promotion(expr_ref, resolved_expr_vec)?;
            let promoted_expr_type = resolved_expr_vec[promoted_expr_ref].expr_type();

            let unary_minus =
                TypedExpressionNode::UnaryMinus(promoted_expr_type.clone(), promoted_expr_ref);
            let unary_minus_ref = ExprRef::from_push(resolved_expr_vec, unary_minus);
            Ok(unary_minus_ref)
        }
        ExpressionNode::BitwiseNot(expr) => {
            let expr_ref = resolve_expr(expr, resolved_expr_vec, expr_indices, symtab)?;
            let expr_type = resolved_expr_vec[expr_ref].expr_type();

            if !expr_type.is_integer_type() {
                return Err(ResolveExprError::BadUnaryArithmeticOperand);
            }

            let promoted_expr_ref = integer_promotion(expr_ref, resolved_expr_vec)?;
            let promoted_expr_type = resolved_expr_vec[promoted_expr_ref].expr_type();

            let bitwise_not =
                TypedExpressionNode::BitwiseNot(promoted_expr_type.clone(), promoted_expr_ref);
            let bitwise_not_ref = ExprRef::from_push(resolved_expr_vec, bitwise_not);
            Ok(bitwise_not_ref)
        }
        ExpressionNode::Not(expr) => {
            // equivalent to (0 == E), all we need to check is if it's a scalar type
            let expr_ref = resolve_expr(expr, resolved_expr_vec, expr_indices, symtab)?;
            let expr_type = resolved_expr_vec[expr_ref].expr_type();

            if !expr_type.is_scalar_type() {
                return Err(ResolveExprError::BadUnaryNotOperand);
            }

            let result_type = CType::int();
            let not = TypedExpressionNode::Not(result_type, expr_ref);
            let not_ref = ExprRef::from_push(resolved_expr_vec, not);
            Ok(not_ref)
        }
        ExpressionNode::PostIncrement(expression_node) => todo!(),
        ExpressionNode::PostDecrement(expression_node) => todo!(),
        ExpressionNode::ArraySubscript(a, b) => {
            // check that one is a pointer to an object, the other is an integer
            let a_ref = resolve_expr(&a, resolved_expr_vec, expr_indices, symtab)?;
            let b_ref = resolve_expr(&b, resolved_expr_vec, expr_indices, symtab)?;

            let a_type = resolved_expr_vec[a_ref].expr_type();
            let b_type = resolved_expr_vec[b_ref].expr_type();

            let a_ptr_b_int = a_type.is_object_pointer() && b_type.is_integer_type();
            let a_int_b_ptr = a_type.is_integer_type() && b_type.is_object_pointer();
            if !a_ptr_b_int && !a_int_b_ptr {
                return Err(ResolveExprError::BadSubscriptOperands);
            }

            // otherwise, it's equivalent to *(a + b)
            let ptr_type = if a_ptr_b_int { a_type } else { b_type };
            let elt_type = match ptr_type {
                CType::PointerType { pointee_type, .. } => pointee_type.as_ref().clone(),
                _ => unreachable!(),
            };
            let ptr_ref = if a_ptr_b_int { a_ref } else { b_ref };
            let int_ref = if a_ptr_b_int { b_ref } else { a_ref };

            let pointer_add = TypedExpressionNode::PointerAdd(ptr_type.clone(), ptr_ref, int_ref);
            let pointer_add_ref = ExprRef::from_push(resolved_expr_vec, pointer_add);
            let deref = TypedExpressionNode::Dereference(elt_type, pointer_add_ref);
            let deref_ref = ExprRef::from_push(resolved_expr_vec, deref);
            Ok(deref_ref)
        }
        ExpressionNode::FunctionCall(fn_expr, argument_exprs) => {
            // either a pointer to a function, or a "function designator" directly
            let fn_expr_ref = resolve_expr(&fn_expr, resolved_expr_vec, expr_indices, symtab)?;

            let fn_expr_type = resolved_expr_vec[fn_expr_ref].expr_type();
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
            let fn_call_ref = ExprRef::from_push(resolved_expr_vec, fn_call);

            Ok(fn_call_ref)
        }
        ExpressionNode::DotAccess(accessee, identifier) => {
            let accessee_ref = resolve_expr(&accessee, resolved_expr_vec, expr_indices, symtab)?;
            let accessee = &resolved_expr_vec[accessee_ref];
            let accessee_type = accessee.expr_type();

            let (member_ref, member_type) = aggregate_member(accessee_type, symtab, identifier)?;
            let mut modified_member_type = member_type.clone();
            // member "adopts" type qualifier of its containing type
            *modified_member_type.qualifier_mut() |= accessee_type.qualifier();

            let dot_access =
                TypedExpressionNode::DotAccess(modified_member_type, accessee_ref, member_ref);
            let dot_access_ref = ExprRef::from_push(resolved_expr_vec, dot_access);

            Ok(dot_access_ref)
        }
        ExpressionNode::ArrowAccess(pointer, identifier) => {
            let pointer_ref = resolve_expr(&pointer, resolved_expr_vec, expr_indices, symtab)?;
            let pointer = &resolved_expr_vec[pointer_ref];
            let pointer_type = pointer.expr_type();
            let accessee_type = match pointer_type {
                CType::PointerType { pointee_type, .. } => pointee_type.as_ref(),
                _ => return Err(ResolveExprError::BadArrowOperator),
            };

            let (member_ref, member_type) = aggregate_member(accessee_type, symtab, identifier)?;
            let mut modified_member_type = member_type.clone();
            // member "adopts" type qualifier of its containing type
            *modified_member_type.qualifier_mut() |= accessee_type.qualifier();

            let arrow_access =
                TypedExpressionNode::ArrowAccess(modified_member_type, pointer_ref, member_ref);
            let arrow_access_ref = ExprRef::from_push(resolved_expr_vec, arrow_access);

            Ok(arrow_access_ref)
        }
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
                    let function_ref = ExprRef::from_push(resolved_expr_vec, function_expr);

                    return Ok(function_ref);
                }

                Some(Symbol::Object(idx)) => {
                    let Object { object_type, .. } = symtab.get_object(*idx);

                    let object_type = object_type.clone();

                    let object = TypedExpressionNode::ObjectIdentifier(object_type, *idx);
                    let object_ref = ExprRef::from_push(resolved_expr_vec, object);

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
            let constant_ref = ExprRef::from_push(resolved_expr_vec, constant);

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
            let string_literal_ref = ExprRef::from_push(resolved_expr_vec, string_literal);

            Ok(string_literal_ref)
        }
    }
}

fn usual_arithmetic_conversions(
    mut a_ref: ExprRef,
    mut b_ref: ExprRef,
    resolved_expr_vec: &mut Vec<TypedExpressionNode>,
) -> Result<(CType, ExprRef, ExprRef), ResolveExprError> {
    let a_type = resolved_expr_vec[a_ref].expr_type();
    let b_type = resolved_expr_vec[b_ref].expr_type();

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
    let b_ref = ExprRef::from_push(resolved_expr_vec, cast);
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

    basic_binary_op_inner(a_ref, b_ref, op_ctor, resolved_expr_vec)
}

fn basic_binary_op_inner(
    a_ref: ExprRef,
    b_ref: ExprRef,
    op_ctor: fn(CType, ExprRef, ExprRef) -> TypedExpressionNode,

    resolved_expr_vec: &mut Vec<TypedExpressionNode>,
) -> Result<ExprRef, ResolveExprError> {
    let (result_type, a_ref, b_ref) =
        usual_arithmetic_conversions(a_ref, b_ref, resolved_expr_vec)?;

    let result = op_ctor(result_type, a_ref, b_ref);
    let result_ref = ExprRef::from_push(resolved_expr_vec, result);

    Ok(result_ref)
}

fn insert_cast(
    target: &CType,
    base: ExprRef,
    resolved_expr_vec: &mut Vec<TypedExpressionNode>,
) -> ExprRef {
    let base_type = resolved_expr_vec[base].expr_type();
    let cast = TypedExpressionNode::Cast(target.clone(), base, base_type.clone());
    let cast_ref = ExprRef::from_push(resolved_expr_vec, cast);
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
    let base_type = resolved_expr_vec[base_ref].expr_type();
    let base = &resolved_expr_vec[base_ref];

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

// ensures that an expression is an l-value. caller should check if its const as well.
// see rules here: https://www.gnu.org/software/c-intro-and-ref/manual/html_node/Lvalues.html
// we don't support string constant as l-value or array / structure / union constructor as l-value
// (why would you ever use these...)
fn check_lvalue(typed_expr_ref: ExprRef, resolved_exprs: &[TypedExpressionNode]) -> bool {
    let typed_expr = &resolved_exprs[typed_expr_ref];
    match typed_expr {
        TypedExpressionNode::ObjectIdentifier(object_type, _) => {
            return object_type.is_assignable();
        }

        // members must always be a complete object type, and thus assignable (though maybe const)
        TypedExpressionNode::DotAccess(_, accessee, _) => {
            return check_lvalue(*accessee, resolved_exprs);
        }
        TypedExpressionNode::ArrowAccess(_, _, _) => return true,

        TypedExpressionNode::Dereference(pointee_type, _) => {
            return pointee_type.is_assignable();
        }

        _ => return false,
    }
}

fn relational_operator(
    a: &ExpressionNode,
    b: &ExpressionNode,
    op_ctor: fn(CType, ExprRef, ExprRef) -> TypedExpressionNode,

    resolved_expr_vec: &mut Vec<TypedExpressionNode>,
    expr_indices: &mut Vec<ExprRef>,
    symtab: &ScopedSymtab,
) -> Result<ExprRef, ResolveExprError> {
    let a_ref = resolve_expr(a, resolved_expr_vec, expr_indices, symtab)?;
    let b_ref = resolve_expr(b, resolved_expr_vec, expr_indices, symtab)?;

    let a_type = resolved_expr_vec[a_ref].expr_type();
    let b_type = resolved_expr_vec[b_ref].expr_type();

    let result_type = CType::int();

    // 1. if both are arithmetic types, perform usual artihmetic conversions and compare normally
    if a_type.is_arithmetic_type() && b_type.is_arithmetic_type() {
        let (_, a_converted, b_converted) =
            usual_arithmetic_conversions(a_ref, b_ref, resolved_expr_vec)?;

        let relational_op = op_ctor(result_type, a_converted, b_converted);
        let relational_op_ref = ExprRef::from_push(resolved_expr_vec, relational_op);
        return Ok(relational_op_ref);
    }

    // 2. if both are pointers, must either be pointer to same type (w/o considering qualifier)
    // or at least one must be pointer to void. this is a little looser than what spec requires
    // make use of newly stabilized let chain (yay)
    if let CType::PointerType {
        pointee_type: a_pointee,
        ..
    } = a_type
        && let CType::PointerType {
            pointee_type: b_pointee,
            ..
        } = b_type
    {
        if a_pointee.is_void()
            || b_pointee.is_void()
            || CType::unqualified_equal(&a_pointee, &b_pointee)
        {
            let relational_op = op_ctor(result_type, a_ref, b_ref);
            let relational_op_ref = ExprRef::from_push(resolved_expr_vec, relational_op);
            return Ok(relational_op_ref);
        }
    }

    return Err(ResolveExprError::RelationalOperatorBadOperands);
}

fn bitshift_operator(
    a: &ExpressionNode,
    b: &ExpressionNode,
    op_ctor: fn(CType, ExprRef, ExprRef) -> TypedExpressionNode,

    resolved_expr_vec: &mut Vec<TypedExpressionNode>,
    expr_indices: &mut Vec<ExprRef>,
    symtab: &ScopedSymtab,
) -> Result<ExprRef, ResolveExprError> {
    let a_ref = resolve_expr(a, resolved_expr_vec, expr_indices, symtab)?;
    let b_ref = resolve_expr(b, resolved_expr_vec, expr_indices, symtab)?;

    let a_type = resolved_expr_vec[a_ref].expr_type();
    let b_type = resolved_expr_vec[b_ref].expr_type();

    if !a_type.is_integer_type() || !b_type.is_integer_type() {
        return Err(ResolveExprError::BadShiftOperands);
    }

    let (common_type, a_converted, b_converted) =
        usual_arithmetic_conversions(a_ref, b_ref, resolved_expr_vec)?;

    let bitshift_op = op_ctor(common_type, a_converted, b_converted);
    let bitshift_op_ref = ExprRef::from_push(resolved_expr_vec, bitshift_op);
    Ok(bitshift_op_ref)
}

fn aggregate_member<'symtab>(
    aggregate_type: &CType,
    symtab: &'symtab ScopedSymtab,
    identifier: &Identifier,
) -> Result<(MemberRef, &'symtab CType), ResolveExprError> {
    let members = match aggregate_type {
        CType::StructureTypeRef { symtab_idx, .. } => {
            symtab.get_structure_type(*symtab_idx).members()
        }
        CType::UnionTypeRef { symtab_idx, .. } => symtab.get_union_type(*symtab_idx).members(),
        _ => {
            return Err(ResolveExprError::BadMemberAccess);
        }
    };

    let pred =
        |(member_idx, member): (usize, &'symtab AggregateMember)| -> Option<(usize, &'symtab CType)> {
            if member.0 == identifier.name {
                Some((member_idx, &member.1))
            } else {
                None
            }
        };

    let (member_idx, member_type) =
        if let Some((member_idx, member_type)) = members.iter().enumerate().find_map(pred) {
            (member_idx, member_type)
        } else {
            return Err(ResolveExprError::MemberNotFound);
        };

    let result = (MemberRef(member_idx as u32), member_type);
    Ok(result)
}

// precondition: expr_ref type must be arithmetic type
fn integer_promotion(
    expr_ref: ExprRef,
    resolved_expr_vec: &mut Vec<TypedExpressionNode>,
) -> Result<ExprRef, ResolveExprError> {
    let expr_type = resolved_expr_vec[expr_ref].expr_type();
    let expr_type = match expr_type {
        CType::BasicType { basic_type, .. } => basic_type,
        _ => return Err(ResolveExprError::ExpectedArithmeticType),
    };

    let promoted = match expr_type {
        BasicType::Char | BasicType::UChar | BasicType::Short | BasicType::UShort => BasicType::Int,
        _ => *expr_type,
    };

    let promoted_type = CType::BasicType {
        basic_type: promoted,
        qualifier: TypeQualifier::empty(),
    };

    if promoted != *expr_type {
        Ok(insert_cast(&promoted_type, expr_ref, resolved_expr_vec))
    } else {
        Ok(expr_ref)
    }
}
