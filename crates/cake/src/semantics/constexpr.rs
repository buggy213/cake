use std::cmp::Ordering;

use thiserror::Error;

use crate::{parser::ast::{Constant, ExpressionNode, Identifier}, semantics::symtab::ScopedSymtab, types::CType};

use super::symtab::Symbol;


#[derive(Debug, Error)]
pub(crate) enum ConstantExprError {
    #[error("error while parsing constant expression in preprocessor conditional directive")]
    PreprocessorConstantExprError,
    #[error("error while computing compile-time integer constant")]
    IntegerConstantExprError,
}

#[derive(Debug, Error)]
enum ConstantArithmeticError {
    #[error("mismatched types")]
    MismatchedTypes,
    #[error("operation only defined for integral types")]
    MustBeIntegral,
    #[error("NaN comparison")]
    NaNComparison,
    #[error("bad shift (shift amount is negative or too large)")]
    BadShift
}

impl Constant {
    pub(crate) fn nonzero(&self) -> bool {
        match self {
            Constant::Int(x) => *x != 0,
            Constant::LongInt(x) => *x != 0,
            Constant::UInt(x) => *x != 0,
            Constant::ULongInt(x) => *x != 0,
            Constant::Float(x) => *x != 0.0f32,
            Constant::Double(x) => *x != 0.0f64,
        }
    }

    // size, in bits
    fn width(&self) -> u32 {
        match self {
            Constant::Int(_) => 32,
            Constant::LongInt(_) => 64,
            Constant::UInt(_) => 32,
            Constant::ULongInt(_) => 64,
            Constant::Float(_) => 32,
            Constant::Double(_) => 64,
        }
    }

    // Rust: f32 -> f64 is lossless, integer types are converted to nearest floating
    // C has same semantics
    fn to_double(&self) -> Self {
        match self {
            Constant::Int(v) => Constant::Double(*v as f64),
            Constant::LongInt(v) => Constant::Double(*v as f64),
            Constant::UInt(v) => Constant::Double(*v as f64),
            Constant::ULongInt(v) => Constant::Double(*v as f64),
            Constant::Float(v) => Constant::Double(*v as f64),
            Constant::Double(_) => *self,
        }
    }

    fn to_float(&self) -> Self {
        match self {
            Constant::Int(v) => Constant::Float(*v as f32),
            Constant::LongInt(v) => Constant::Float(*v as f32),
            Constant::UInt(v) => Constant::Float(*v as f32),
            Constant::ULongInt(v) => Constant::Float(*v as f32),
            Constant::Float(_) => *self,
            // on overflow: +/-infty in Rust, UB in C, so infinity is fine
            Constant::Double(v) => Constant::Float(*v as f32)
        }
    }

    fn add(lhs: Self, rhs: Self) -> Result<Self, ConstantArithmeticError> {
        match (lhs, rhs) {
            (Constant::Int(a), Constant::Int(b)) => Ok(Constant::Int(a + b)),
            (Constant::LongInt(a), Constant::LongInt(b)) => Ok(Constant::LongInt(a + b)),
            (Constant::UInt(a), Constant::UInt(b)) => Ok(Constant::UInt(a + b)),
            (Constant::ULongInt(a), Constant::ULongInt(b)) => Ok(Constant::ULongInt(a + b)),
            (Constant::Float(a), Constant::Float(b)) => Ok(Constant::Float(a + b)),
            (Constant::Double(a), Constant::Double(b)) => Ok(Constant::Double(a + b)),
            _ => Err(ConstantArithmeticError::MismatchedTypes),
        }
    }

    fn sub(lhs: Self, rhs: Self) -> Result<Self, ConstantArithmeticError> {
        match (lhs, rhs) {
            (Constant::Int(a), Constant::Int(b)) => Ok(Constant::Int(a - b)),
            (Constant::LongInt(a), Constant::LongInt(b)) => Ok(Constant::LongInt(a - b)),
            (Constant::UInt(a), Constant::UInt(b)) => Ok(Constant::UInt(a - b)),
            (Constant::ULongInt(a), Constant::ULongInt(b)) => Ok(Constant::ULongInt(a - b)),
            (Constant::Float(a), Constant::Float(b)) => Ok(Constant::Float(a - b)),
            (Constant::Double(a), Constant::Double(b)) => Ok(Constant::Double(a - b)),
            _ => Err(ConstantArithmeticError::MismatchedTypes),
        }
    }

    fn mul(lhs: Self, rhs: Self) -> Result<Self, ConstantArithmeticError> {
        match (lhs, rhs) {
            (Constant::Int(a), Constant::Int(b)) => Ok(Constant::Int(a * b)),
            (Constant::LongInt(a), Constant::LongInt(b)) => Ok(Constant::LongInt(a * b)),
            (Constant::UInt(a), Constant::UInt(b)) => Ok(Constant::UInt(a * b)),
            (Constant::ULongInt(a), Constant::ULongInt(b)) => Ok(Constant::ULongInt(a * b)),
            (Constant::Float(a), Constant::Float(b)) => Ok(Constant::Float(a * b)),
            (Constant::Double(a), Constant::Double(b)) => Ok(Constant::Double(a * b)),
            _ => Err(ConstantArithmeticError::MismatchedTypes),
        }
    }

    // Rust / C both have same division / remainder semantics, which is nice
    fn div(lhs: Self, rhs: Self) -> Result<Self, ConstantArithmeticError> {
        match (lhs, rhs) {
            (Constant::Int(a), Constant::Int(b)) => Ok(Constant::Int(a / b)),
            (Constant::LongInt(a), Constant::LongInt(b)) => Ok(Constant::LongInt(a / b)),
            (Constant::UInt(a), Constant::UInt(b)) => Ok(Constant::UInt(a / b)),
            (Constant::ULongInt(a), Constant::ULongInt(b)) => Ok(Constant::ULongInt(a / b)),
            (Constant::Float(a), Constant::Float(b)) => Ok(Constant::Float(a / b)),
            (Constant::Double(a), Constant::Double(b)) => Ok(Constant::Double(a / b)),
            _ => Err(ConstantArithmeticError::MismatchedTypes),
        }
    }

    fn modulo(lhs: Self, rhs: Self) -> Result<Self, ConstantArithmeticError> {
        match (lhs, rhs) {
            (Constant::Int(a), Constant::Int(b)) => Ok(Constant::Int(a % b)),
            (Constant::LongInt(a), Constant::LongInt(b)) => Ok(Constant::LongInt(a % b)),
            (Constant::UInt(a), Constant::UInt(b)) => Ok(Constant::UInt(a % b)),
            (Constant::ULongInt(a), Constant::ULongInt(b)) => Ok(Constant::ULongInt(a % b)),
            (Constant::Float(_), Constant::Float(_)) => Err(ConstantArithmeticError::MustBeIntegral),
            (Constant::Double(_), Constant::Double(_)) => Err(ConstantArithmeticError::MustBeIntegral),
            _ => Err(ConstantArithmeticError::MismatchedTypes),
        }
    }

    fn bit_and(lhs: Self, rhs: Self) -> Result<Self, ConstantArithmeticError> {
        match (lhs, rhs) {
            (Constant::Int(a), Constant::Int(b)) => Ok(Constant::Int(a & b)),
            (Constant::LongInt(a), Constant::LongInt(b)) => Ok(Constant::LongInt(a & b)),
            (Constant::UInt(a), Constant::UInt(b)) => Ok(Constant::UInt(a & b)),
            (Constant::ULongInt(a), Constant::ULongInt(b)) => Ok(Constant::ULongInt(a & b)),
            (Constant::Float(_), Constant::Float(_)) => {
                Err(ConstantArithmeticError::MustBeIntegral)
            }
            (Constant::Double(_), Constant::Double(_)) => {
                Err(ConstantArithmeticError::MustBeIntegral)
            }
            _ => Err(ConstantArithmeticError::MismatchedTypes),
        }
    }

    fn bit_or(lhs: Self, rhs: Self) -> Result<Self, ConstantArithmeticError> {
        match (lhs, rhs) {
            (Constant::Int(a), Constant::Int(b)) => Ok(Constant::Int(a | b)),
            (Constant::LongInt(a), Constant::LongInt(b)) => Ok(Constant::LongInt(a | b)),
            (Constant::UInt(a), Constant::UInt(b)) => Ok(Constant::UInt(a | b)),
            (Constant::ULongInt(a), Constant::ULongInt(b)) => Ok(Constant::ULongInt(a | b)),
            (Constant::Float(_), Constant::Float(_)) => {
                Err(ConstantArithmeticError::MustBeIntegral)
            }
            (Constant::Double(_), Constant::Double(_)) => {
                Err(ConstantArithmeticError::MustBeIntegral)
            }
            _ => Err(ConstantArithmeticError::MismatchedTypes),
        }
    }

    fn bit_xor(lhs: Self, rhs: Self) -> Result<Self, ConstantArithmeticError> {
        match (lhs, rhs) {
            (Constant::Int(a), Constant::Int(b)) => Ok(Constant::Int(a ^ b)),
            (Constant::LongInt(a), Constant::LongInt(b)) => Ok(Constant::LongInt(a ^ b)),
            (Constant::UInt(a), Constant::UInt(b)) => Ok(Constant::UInt(a ^ b)),
            (Constant::ULongInt(a), Constant::ULongInt(b)) => Ok(Constant::ULongInt(a ^ b)),
            (Constant::Float(_), Constant::Float(_)) => {
                Err(ConstantArithmeticError::MustBeIntegral)
            }
            (Constant::Double(_), Constant::Double(_)) => {
                Err(ConstantArithmeticError::MustBeIntegral)
            }
            _ => Err(ConstantArithmeticError::MismatchedTypes),
        }
    }

    fn equal(lhs: Self, rhs: Self) -> bool {
        match (lhs, rhs) {
            (Constant::Int(a), Constant::Int(b)) => a == b,
            (Constant::LongInt(a), Constant::LongInt(b)) => a == b,
            (Constant::UInt(a), Constant::UInt(b)) => a == b,
            (Constant::ULongInt(a), Constant::ULongInt(b)) => a == b,
            (Constant::Float(a), Constant::Float(b)) => a == b,
            (Constant::Double(a), Constant::Double(b)) => a == b,
            _ => false,
        }
    }

    fn ord(lhs: Self, rhs: Self) -> Result<Ordering, ConstantArithmeticError> {
        match (lhs, rhs) {
            (Constant::Int(a), Constant::Int(b)) => Ok(Ord::cmp(&a, &b)),
            (Constant::LongInt(a), Constant::LongInt(b)) => Ok(Ord::cmp(&a, &b)),
            (Constant::UInt(a), Constant::UInt(b)) => Ok(Ord::cmp(&a, &b)),
            (Constant::ULongInt(a), Constant::ULongInt(b)) => Ok(Ord::cmp(&a, &b)),
            (Constant::Float(a), Constant::Float(b)) => {
                if let Some(ord) = PartialOrd::partial_cmp(&a, &b) {
                    Ok(ord)
                } else {
                    Err(ConstantArithmeticError::NaNComparison)
                }
            }
            (Constant::Double(a), Constant::Double(b)) => {
                if let Some(ord) = PartialOrd::partial_cmp(&a, &b) {
                    Ok(ord)
                } else {
                    Err(ConstantArithmeticError::NaNComparison)
                }
            }
            _ => Err(ConstantArithmeticError::MismatchedTypes),
        }
    }

    fn lshift(lhs: Self, rhs: Self) -> Result<Self, ConstantArithmeticError> {
        let width = lhs.width();
        let shift_amt = match rhs {
            Constant::Int(b) => {
                if b < 0 || (b as u32) >= width {
                    return Err(ConstantArithmeticError::BadShift)
                }
                else {
                    b
                }
            },
            Constant::LongInt(b) => {
                if b < 0 || (b as u32) >= width {
                    return Err(ConstantArithmeticError::BadShift)
                }
                else {
                    b as i32
                }
            },
            Constant::UInt(b) => {
                if (b as u32) >= width {
                    return Err(ConstantArithmeticError::BadShift)
                }
                else {
                    b as i32
                }
            },
            Constant::ULongInt(b) => {
                if b > (u32::MAX as u64) || (b as u32) >= width {
                    return Err(ConstantArithmeticError::BadShift)
                }
                else {
                    b as i32
                }
            },
            Constant::Float(_)
            | Constant::Double(_) => {
                return Err(ConstantArithmeticError::MustBeIntegral)
            },
        };

        match lhs {
            Constant::Int(a) => Ok(Constant::Int(a << shift_amt)),
            Constant::LongInt(a) => Ok(Constant::LongInt(a << shift_amt)),
            Constant::UInt(a) => Ok(Constant::UInt(a << shift_amt)),
            Constant::ULongInt(a) => Ok(Constant::ULongInt(a << shift_amt)),
            Constant::Float(_)
            | Constant::Double(_) => Err(ConstantArithmeticError::MustBeIntegral),
        }
    }

    fn rshift(lhs: Self, rhs: Self) -> Result<Self, ConstantArithmeticError> {
        let width = lhs.width();
        let shift_amt = match rhs {
            Constant::Int(b) => {
                if b < 0 || (b as u32) >= width {
                    return Err(ConstantArithmeticError::BadShift)
                }
                else {
                    b
                }
            },
            Constant::LongInt(b) => {
                if b < 0 || (b as u32) >= width {
                    return Err(ConstantArithmeticError::BadShift)
                }
                else {
                    b as i32
                }
            },
            Constant::UInt(b) => {
                if (b as u32) >= width {
                    return Err(ConstantArithmeticError::BadShift)
                }
                else {
                    b as i32
                }
            },
            Constant::ULongInt(b) => {
                if b > (u32::MAX as u64) || (b as u32) >= width {
                    return Err(ConstantArithmeticError::BadShift)
                }
                else {
                    b as i32
                }
            },
            Constant::Float(_)
            | Constant::Double(_) => {
                return Err(ConstantArithmeticError::MustBeIntegral)
            },
        };

        match lhs {
            Constant::Int(a) => Ok(Constant::Int(a >> shift_amt)),
            Constant::LongInt(a) => Ok(Constant::LongInt(a >> shift_amt)),
            Constant::UInt(a) => Ok(Constant::UInt(a >> shift_amt)),
            Constant::ULongInt(a) => Ok(Constant::ULongInt(a >> shift_amt)),
            Constant::Float(_)
            | Constant::Double(_) => Err(ConstantArithmeticError::MustBeIntegral),
        }
    }

    fn neg(&self) -> Self {
        match self {
            Constant::Int(a) => Constant::Int(-a),
            Constant::LongInt(a) => Constant::LongInt(-a),
            Constant::UInt(a) => {
                Constant::UInt(0u32.wrapping_sub(*a))
            },
            Constant::ULongInt(a) => {
                Constant::ULongInt(0u64.wrapping_sub(*a))
            },
            Constant::Float(a) => Constant::Float(-a),
            Constant::Double(a) => Constant::Double(-a),
        }
    }

    fn bit_not(v: Self) -> Result<Self, ConstantArithmeticError> {
        match v {
            Constant::Int(v) => Ok(Constant::Int(!v)),
            Constant::LongInt(v) => Ok(Constant::LongInt(!v)),
            Constant::UInt(v) => Ok(Constant::UInt(!v)),
            Constant::ULongInt(v) => Ok(Constant::ULongInt(!v)),
            Constant::Float(_)
            | Constant::Double(_) => Err(ConstantArithmeticError::MustBeIntegral),
        }
    }
}

// perform "usual arithmetic conversions" on constants
fn convert_constants(lhs: Constant, rhs: Constant) -> (Constant, Constant) {
    match (lhs, rhs) {
        (a, b @ Constant::Double(_)) => {
            (a.to_double(), b)
        }
        (a @ Constant::Double(_), b) => {
            (a, b.to_double())
        },
        (a, b @ Constant::Float(_)) => {
            (a.to_float(), b)
        }
        (a @ Constant::Float(_), b) => {
            (a, b.to_float())
        }

        (a @ Constant::Int(_), b @ Constant::Int(_))
        | (a @ Constant::LongInt(_), b @ Constant::LongInt(_))
        | (a @ Constant::UInt(_), b @ Constant::UInt(_))
        | (a @ Constant::ULongInt(_), b @ Constant::ULongInt(_)) => (a, b),
        
        (Constant::Int(a), b @ Constant::LongInt(_)) => (Constant::LongInt(a as i64), b),
        (a @ Constant::LongInt(_), Constant::Int(b)) => (a, Constant::LongInt(b as i64)),
        
        (Constant::UInt(a), b @ Constant::ULongInt(_)) => (Constant::ULongInt(a as u64), b),
        (a @ Constant::ULongInt(_), Constant::UInt(b)) => (a, Constant::ULongInt(b as u64)),
        

        (Constant::Int(a), b @ Constant::UInt(_)) => {
            let a_converted = 0u32.wrapping_add_signed(a);
            (Constant::UInt(a_converted), b)
        },
        (a @ Constant::UInt(_), Constant::Int(b)) => {
            let b_converted = 0u32.wrapping_add_signed(b);
            (a, Constant::UInt(b_converted))
        },
        (Constant::Int(a), b @ Constant::ULongInt(_)) => {
            let a_converted = 0u64.wrapping_add_signed(a as i64);
            (Constant::ULongInt(a_converted), b)
        },
        (a @ Constant::ULongInt(_), Constant::Int(b)) => {
            let b_converted = 0u64.wrapping_add_signed(b as i64);
            (a, Constant::ULongInt(b_converted))
        },
        (Constant::LongInt(a), b @ Constant::ULongInt(_)) => {
            let a_converted = 0u64.wrapping_add_signed(a);
            (Constant::ULongInt(a_converted), b)
        },
        (a @ Constant::ULongInt(_), Constant::LongInt(b)) => {
            let b_converted = 0u64.wrapping_add_signed(b);
            (a, Constant::ULongInt(b_converted))
        },

        (Constant::UInt(a), b @ Constant::LongInt(_)) => {
            (Constant::LongInt(a as i64), b)
        },
        (a @ Constant::LongInt(_), Constant::UInt(b)) => {
            (a, Constant::LongInt(b as i64))
        },        
    }
}

trait ConstexprBehavior {
    fn handle_identifier(&self, ident: &Identifier) -> Result<Constant, ConstantExprError>;
    fn handle_function(&self, func: &ExpressionNode, args: &[ExpressionNode]) -> Result<Constant, ConstantExprError>;
    fn handle_constant(&self, constant: Constant) -> Result<Constant, ConstantExprError>;
    fn handle_sizeof_expr(&self, expr: &ExpressionNode) -> Result<Constant, ConstantExprError>;
    fn handle_sizeof_type(&self, ty: &CType) -> Result<Constant, ConstantExprError>;
}

// the core logic of evaluating constant expressions is the same between preprocessor (only used for #if / #elif, basically)
// and for the main language, so we factor it out into shared logic and only specialize it at the leaves of the expression tree
fn integer_constant_eval(
    root: &ExpressionNode, 
    constexpr_behavior: &impl ConstexprBehavior
) -> Result<Constant, ConstantExprError> {
    match root {
        ExpressionNode::Ternary(a, b, c) => {
            let a = integer_constant_eval(a, constexpr_behavior)?;
            if a.nonzero() {
                integer_constant_eval(b, constexpr_behavior)
            } else {
                integer_constant_eval(c, constexpr_behavior)
            } 
        },
        ExpressionNode::LogicalAnd(lhs, rhs) => {
            if integer_constant_eval(lhs, constexpr_behavior)?.nonzero() && integer_constant_eval(rhs, constexpr_behavior)?.nonzero() {
                Ok(Constant::Int(1))
            }
            else {
                Ok(Constant::Int(0))
            }
        },
        ExpressionNode::LogicalOr(lhs, rhs) => {
            if integer_constant_eval(lhs, constexpr_behavior)?.nonzero() || integer_constant_eval(rhs, constexpr_behavior)?.nonzero() {
                Ok(Constant::Int(1))
            }
            else {
                Ok(Constant::Int(0))
            }
        },
        ExpressionNode::BitwiseAnd(lhs, rhs) => {
            let lhs = integer_constant_eval(lhs, constexpr_behavior)?;
            let rhs = integer_constant_eval(rhs, constexpr_behavior)?;
            let (lhs, rhs) = convert_constants(lhs, rhs);
            match Constant::bit_and(lhs, rhs) {
                Ok(res) => Ok(res),
                Err(_) => Err(ConstantExprError::IntegerConstantExprError),
            }
        },
        ExpressionNode::BitwiseOr(lhs, rhs) => {
            let lhs = integer_constant_eval(lhs, constexpr_behavior)?;
            let rhs = integer_constant_eval(rhs, constexpr_behavior)?;
            let (lhs, rhs) = convert_constants(lhs, rhs);
            match Constant::bit_or(lhs, rhs) {
                Ok(res) => Ok(res),
                Err(_) => Err(ConstantExprError::IntegerConstantExprError),
            }
        },
        ExpressionNode::BitwiseXor(lhs, rhs) => {
            let lhs = integer_constant_eval(lhs, constexpr_behavior)?;
            let rhs = integer_constant_eval(rhs, constexpr_behavior)?;
            let (lhs, rhs) = convert_constants(lhs, rhs);
            match Constant::bit_xor(lhs, rhs) {
                Ok(res) => Ok(res),
                Err(_) => Err(ConstantExprError::IntegerConstantExprError),
            }
        },
        ExpressionNode::Equal(lhs, rhs) => {
            let lhs = integer_constant_eval(lhs, constexpr_behavior)?;
            let rhs = integer_constant_eval(rhs, constexpr_behavior)?;
            let (lhs, rhs) = convert_constants(lhs, rhs);
            let val = if Constant::equal(lhs, rhs) { 1 } else { 0 };
            Ok(Constant::Int(val))
        },
        ExpressionNode::NotEqual(lhs, rhs) => {
            let lhs = integer_constant_eval(lhs, constexpr_behavior)?;
            let rhs = integer_constant_eval(rhs, constexpr_behavior)?;
            let (lhs, rhs) = convert_constants(lhs, rhs);
            let val = if !Constant::equal(lhs, rhs) { 1 } else { 0 };
            Ok(Constant::Int(val))
        },
        ExpressionNode::LessThan(lhs, rhs) => {
            let lhs = integer_constant_eval(lhs, constexpr_behavior)?;
            let rhs = integer_constant_eval(rhs, constexpr_behavior)?;
            let (lhs, rhs) = convert_constants(lhs, rhs);
            match Constant::ord(lhs, rhs) {
                Ok(Ordering::Less) => Ok(Constant::Int(1)),
                Ok(_) => Ok(Constant::Int(0)),
                Err(_) => Err(ConstantExprError::IntegerConstantExprError),
            }
        },
        ExpressionNode::GreaterThan(lhs, rhs) => {
            let lhs = integer_constant_eval(lhs, constexpr_behavior)?;
            let rhs = integer_constant_eval(rhs, constexpr_behavior)?;
            let (lhs, rhs) = convert_constants(lhs, rhs);
            match Constant::ord(lhs, rhs) {
                Ok(Ordering::Greater) => Ok(Constant::Int(1)),
                Ok(_) => Ok(Constant::Int(0)),
                Err(_) => Err(ConstantExprError::IntegerConstantExprError),
            }
        }
        ExpressionNode::LessThanOrEqual(lhs, rhs) => {
            let lhs = integer_constant_eval(lhs, constexpr_behavior)?;
            let rhs = integer_constant_eval(rhs, constexpr_behavior)?;
            let (lhs, rhs) = convert_constants(lhs, rhs);
            match Constant::ord(lhs, rhs) {
                Ok(Ordering::Less) | Ok(Ordering::Equal) => Ok(Constant::Int(1)),
                Ok(_) => Ok(Constant::Int(0)),
                Err(_) => Err(ConstantExprError::IntegerConstantExprError),
            }
        },
        ExpressionNode::GreaterThanOrEqual(lhs, rhs) => {
            let lhs = integer_constant_eval(lhs, constexpr_behavior)?;
            let rhs = integer_constant_eval(rhs, constexpr_behavior)?;
            let (lhs, rhs) = convert_constants(lhs, rhs);
            match Constant::ord(lhs, rhs) {
                Ok(Ordering::Greater) | Ok(Ordering::Equal) => Ok(Constant::Int(1)),
                Ok(_) => Ok(Constant::Int(0)),
                Err(_) => Err(ConstantExprError::IntegerConstantExprError),
            }
        },

        // shifts do not perform "usual arithmetic conversions"
        ExpressionNode::LShift(lhs, rhs) => {
            let lhs = integer_constant_eval(lhs, constexpr_behavior)?;
            let rhs = integer_constant_eval(rhs, constexpr_behavior)?;
            let val = Constant::lshift(lhs, rhs);
            match val {
                Ok(val) => Ok(val),
                Err(_) => Err(ConstantExprError::IntegerConstantExprError),
            }
        },
        ExpressionNode::RShift(lhs, rhs) => {
            let lhs = integer_constant_eval(lhs, constexpr_behavior)?;
            let rhs = integer_constant_eval(rhs, constexpr_behavior)?;
            let val = Constant::rshift(lhs, rhs);
            match val {
                Ok(val) => Ok(val),
                Err(_) => Err(ConstantExprError::IntegerConstantExprError),
            }
        },
        ExpressionNode::Multiply(lhs, rhs) => {
            let lhs = integer_constant_eval(lhs, constexpr_behavior)?;
            let rhs = integer_constant_eval(rhs, constexpr_behavior)?;
            let (lhs, rhs) = convert_constants(lhs, rhs);
            let val = Constant::mul(lhs, rhs);
            match val {
                Ok(val) => Ok(val),
                Err(_) => Err(ConstantExprError::IntegerConstantExprError),
            }
        },
        ExpressionNode::Divide(lhs, rhs) => {
            let lhs = integer_constant_eval(lhs, constexpr_behavior)?;
            let rhs = integer_constant_eval(rhs, constexpr_behavior)?;
            let (lhs, rhs) = convert_constants(lhs, rhs);
            let val = Constant::div(lhs, rhs);
            match val {
                Ok(val) => Ok(val),
                Err(_) => Err(ConstantExprError::IntegerConstantExprError),
            }
        },
        ExpressionNode::Modulo(lhs, rhs) => {
            let lhs = integer_constant_eval(lhs, constexpr_behavior)?;
            let rhs = integer_constant_eval(rhs, constexpr_behavior)?;
            let (lhs, rhs) = convert_constants(lhs, rhs);
            let val = Constant::modulo(lhs, rhs);
            match val {
                Ok(val) => Ok(val),
                Err(_) => Err(ConstantExprError::IntegerConstantExprError),
            }
        },
        ExpressionNode::Add(lhs, rhs) => {
            let lhs = integer_constant_eval(lhs, constexpr_behavior)?;
            let rhs = integer_constant_eval(rhs, constexpr_behavior)?;
            let (lhs, rhs) = convert_constants(lhs, rhs);
            let val = Constant::add(lhs, rhs);
            match val {
                Ok(val) => Ok(val),
                Err(_) => Err(ConstantExprError::IntegerConstantExprError),
            }
        },
        ExpressionNode::Subtract(lhs, rhs) => {
            let lhs = integer_constant_eval(lhs, constexpr_behavior)?;
            let rhs = integer_constant_eval(rhs, constexpr_behavior)?;
            let (lhs, rhs) = convert_constants(lhs, rhs);
            let val = Constant::sub(lhs, rhs);
            match val {
                Ok(val) => Ok(val),
                Err(_) => Err(ConstantExprError::IntegerConstantExprError),
            }
        },
        
        // only effect is integer promotion, which is not applicable under current model of constants 
        ExpressionNode::UnaryPlus(target) => {
            let val = integer_constant_eval(target, constexpr_behavior)?;
            Ok(val)
        },
        ExpressionNode::UnaryMinus(target) => {
            let val = integer_constant_eval(target, constexpr_behavior)?;
            Ok(val.neg())
        },
        ExpressionNode::BitwiseNot(target) => {
            let val = integer_constant_eval(target, constexpr_behavior)?;
            match Constant::bit_not(val) {
                Ok(v) => Ok(v),
                Err(_) => Err(ConstantExprError::IntegerConstantExprError),
            }
        },
        ExpressionNode::Not(target) => {
            let val = integer_constant_eval(target, constexpr_behavior)?;
            let not = if val.nonzero() { 0 } else { 1 };
            Ok(Constant::Int(not))
        },
        
        ExpressionNode::Identifier(ident) => constexpr_behavior.handle_identifier(ident),
        ExpressionNode::Constant(constant) => constexpr_behavior.handle_constant(*constant),
        ExpressionNode::FunctionCall(func, args) => constexpr_behavior.handle_function(func, args),
        ExpressionNode::Sizeof(expr) => constexpr_behavior.handle_sizeof_expr(expr),
        ExpressionNode::SizeofType(ty) => constexpr_behavior.handle_sizeof_type(ty),

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
        | ExpressionNode::Cast(_, _)
        | ExpressionNode::PreIncrement(_)
        | ExpressionNode::PreDecrement(_)
        | ExpressionNode::AddressOf(_)
        | ExpressionNode::Dereference(_)
        | ExpressionNode::PostIncrement(_)
        | ExpressionNode::PostDecrement(_)
        | ExpressionNode::ArraySubscript(_, _)
        | ExpressionNode::DotAccess(_, _)
        | ExpressionNode::ArrowAccess(_, _)
        | ExpressionNode::StringLiteral(_) => return Err(ConstantExprError::IntegerConstantExprError),
    }
}


// not fully compliant, since not everything is evaluated as being intmax_t / uintmax_t but should be close enough
struct PreprocessorConstexprBehavior<F> where F: Fn(&str) -> bool {
    macro_defined: F
}

impl<F: Fn(&str) -> bool> ConstexprBehavior for PreprocessorConstexprBehavior<F> {
    fn handle_identifier(&self, _ident: &Identifier) -> Result<Constant, ConstantExprError> {
        Err(ConstantExprError::PreprocessorConstantExprError)
    }

    // a bit of a hack, since `defined` is not included in CLexemes.
    // this means that `#if defined MACRO_NAME` *without* parentheses will not compile though
    fn handle_function(&self, func: &ExpressionNode, args: &[ExpressionNode]) -> Result<Constant, ConstantExprError> {
        let ExpressionNode::Identifier(Identifier { name, .. }) = func else {
            return Err(ConstantExprError::PreprocessorConstantExprError);
        };

        if name != "defined" {
            return Err(ConstantExprError::PreprocessorConstantExprError);
        }

        let Some(expr) = args.first() else {
            return Err(ConstantExprError::PreprocessorConstantExprError);
        };

        let ExpressionNode::Identifier(Identifier { name, .. }) = &expr else {
            return Err(ConstantExprError::PreprocessorConstantExprError);
        };

        let defined = (self.macro_defined)(&name);
        return Ok(if defined { Constant::ULongInt(1) } else { Constant::ULongInt(0) });
    }

    fn handle_constant(&self, constant: Constant) -> Result<Constant, ConstantExprError> {
        let padded_constant = match constant {
            Constant::Int(int) => Constant::LongInt(int as i64),
            Constant::LongInt(long) => Constant::LongInt(long),
            Constant::UInt(uint) => Constant::ULongInt(uint as u64),
            Constant::ULongInt(ulong) => Constant::ULongInt(ulong),
            Constant::Float(_) => return Err(ConstantExprError::PreprocessorConstantExprError),
            Constant::Double(_) => return Err(ConstantExprError::PreprocessorConstantExprError),
        };

        Ok(padded_constant)
    }

    // sizeof not allowed in preprocessor constants
    fn handle_sizeof_expr(&self, _expr: &ExpressionNode) -> Result<Constant, ConstantExprError> {
        Err(ConstantExprError::PreprocessorConstantExprError)
    }

    fn handle_sizeof_type(&self, _ty: &CType) -> Result<Constant, ConstantExprError> {
        Err(ConstantExprError::PreprocessorConstantExprError)
    }
}

pub(crate) fn preprocessor_constant_eval(root: &ExpressionNode, macro_defined: impl Fn(&str) -> bool) -> Result<Constant, ConstantExprError> {
    let constexpr_behavior = PreprocessorConstexprBehavior { macro_defined };
    integer_constant_eval(root, &constexpr_behavior)
}

struct ExpressionConstexprBehavior<'symtab> {
    symtab: &'symtab ScopedSymtab
}

impl<'symtab> ConstexprBehavior for ExpressionConstexprBehavior<'symtab> {
    fn handle_identifier(&self, ident: &Identifier) -> Result<Constant, ConstantExprError> {
        let symbol = self.symtab.lookup_symbol(ident.scope, &ident.name);
        match symbol {
            Some(Symbol::Constant(constant)) => {
                match constant {
                    Constant::Float(_) => Err(ConstantExprError::IntegerConstantExprError),
                    Constant::Double(_) => Err(ConstantExprError::IntegerConstantExprError),
                    int_const => Ok(*int_const)
                }
            },
            Some(_) => {
                // symbol is not a constant type (i.e. not an enum constant)
                Err(ConstantExprError::IntegerConstantExprError)
            }
            None => {
                // symbol not found
                Err(ConstantExprError::IntegerConstantExprError)
            },
        }
    }

    fn handle_function(&self, _func: &ExpressionNode, _args: &[ExpressionNode]) -> Result<Constant, ConstantExprError> {
        Err(ConstantExprError::IntegerConstantExprError)
    }

    fn handle_constant(&self, constant: Constant) -> Result<Constant, ConstantExprError> {
        Ok(constant)
    }

    fn handle_sizeof_expr(&self, _expr: &ExpressionNode) -> Result<Constant, ConstantExprError> {
        todo!("handle sizeof_expr")
    }

    fn handle_sizeof_type(&self, _ty: &CType) -> Result<Constant, ConstantExprError> {
        todo!("handle sizeof_type")
    }
}

pub(crate) fn expression_constant_eval(symtab: &ScopedSymtab, root: &ExpressionNode) -> Result<Constant, ConstantExprError> {
    let expression_constexpr_behavior = ExpressionConstexprBehavior { symtab };
    integer_constant_eval(root, &expression_constexpr_behavior)
}

