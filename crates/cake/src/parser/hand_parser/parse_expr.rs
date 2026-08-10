use crate::parser::ast::{Constant, ExpressionNode, Identifier};
use crate::parser::hand_parser::{ParseError, ParserState, Result};
use crate::scanner::InterningTokenStream;
use crate::scanner::lexeme_sets::c_lexemes::CLexemes;
use crate::scanner::string_pool::StringPoolRef;
use crate::semantics::symtab::Scope;

#[derive(Debug, Clone)]
enum Atom {
    Identifier(Identifier),
    Constant(Constant),
    StringLiteral(String),
}

impl From<Atom> for ExpressionNode {
    fn from(value: Atom) -> Self {
        match value {
            Atom::Identifier(ident) => ExpressionNode::Identifier(ident),
            Atom::Constant(constant) => ExpressionNode::Constant(constant),
            Atom::StringLiteral(string) => ExpressionNode::StringLiteral(string),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Operator {
    // "ordinary" operators
    Increment,
    Decrement,
    Dot,
    Arrow,
    Plus,
    Minus,
    Bang,
    Tilde,
    Star,
    BitAnd,
    Sizeof,
    Slash,
    Percent,
    RShift,
    LShift,
    Lt,
    Gt,
    Leq,
    Geq,
    Eq,
    Neq,
    Xor,
    BitOr,
    And,
    Or,
    Assign,

    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
    AddAssign,
    SubAssign,
    LShiftAssign,
    RShiftAssign,
    AndAssign,
    XorAssign,
    OrAssign,

    Comma,

    // special cases - '(' can be a function call or delimiter for a primary-expression
    LParen,
    // '[' can be used for array indexing
    LBracket,
    // '?' and ':' collectively create a ternary
    Question,
    Colon,
}

enum ExprPart {
    Atom(Atom),
    Operator(Operator),
}

fn parse_integer_const(text: &str) -> Result<Constant> {
    let mut text = text;
    let mut suffix = None;
    let hex = if text.starts_with("0x") || text.starts_with("0X") {
        text = &text[2..];
        true
    } else {
        false
    };
    let octal = !hex
        && if text.starts_with("0") {
            text = &text[1..];
            true
        } else {
            false
        };

    for (index, char) in text.char_indices() {
        if !char.is_ascii_hexdigit() {
            suffix = Some(&text[index..]);
            text = &text[..index];
            break;
        }
    }

    enum IntTypes {
        U32,
        U64,
        I32,
        I64,
    }

    let conversion_attempt_list = match (suffix, hex || octal) {
        (None, false) => [IntTypes::I32, IntTypes::I64].as_slice(),
        (None, true) => [IntTypes::I32, IntTypes::U32, IntTypes::I64, IntTypes::U64].as_slice(),
        (Some("u"), false) | (Some("U"), false) | (Some("u"), true) | (Some("U"), true) => {
            [IntTypes::U32, IntTypes::U64].as_slice()
        }

        (Some("l"), false) | (Some("L"), false) | (Some("ll"), false) | (Some("LL"), false) => {
            [IntTypes::I64].as_slice()
        }

        (Some("l"), true) | (Some("L"), true) | (Some("ll"), true) | (Some("LL"), true) => {
            [IntTypes::I64, IntTypes::U64].as_slice()
        }

        (Some("ull"), _)
        | (Some("uLL"), _)
        | (Some("Ull"), _)
        | (Some("ULL"), _)
        | (Some("llu"), _)
        | (Some("LLu"), _)
        | (Some("llU"), _)
        | (Some("LLU"), _) => [IntTypes::U64].as_slice(),

        // compiler bug! shouldn't be possible due to what the lexer is matching against
        _ => {
            #[cfg(debug_assertions)]
            panic!("unexpected integer constant type");
            [IntTypes::U64].as_slice()
        }
    };

    for conversion in conversion_attempt_list {
        match conversion {
            IntTypes::U32 => {
                let result = if text.len() == 0 {
                    Ok(0u32)
                } else if hex {
                    u32::from_str_radix(text, 16)
                } else if octal {
                    u32::from_str_radix(text, 8)
                } else {
                    u32::from_str_radix(text, 10)
                };

                if let Ok(v) = result {
                    return Ok(Constant::UInt(v));
                }
            }
            IntTypes::U64 => {
                let result = if text.len() == 0 {
                    Ok(0u64)
                } else if hex {
                    u64::from_str_radix(text, 16)
                } else if octal {
                    u64::from_str_radix(text, 8)
                } else {
                    u64::from_str_radix(text, 10)
                };

                if let Ok(v) = result {
                    return Ok(Constant::ULongInt(v));
                }
            }
            IntTypes::I32 => {
                let result = if text.len() == 0 {
                    Ok(0i32)
                } else if hex {
                    i32::from_str_radix(text, 16)
                } else if octal {
                    i32::from_str_radix(text, 8)
                } else {
                    i32::from_str_radix(text, 10)
                };

                if let Ok(v) = result {
                    return Ok(Constant::Int(v));
                }
            }
            IntTypes::I64 => {
                let result = if text.len() == 0 {
                    Ok(0i64)
                } else if hex {
                    i64::from_str_radix(text, 16)
                } else if octal {
                    i64::from_str_radix(text, 8)
                } else {
                    i64::from_str_radix(text, 10)
                };

                if let Ok(v) = result {
                    return Ok(Constant::LongInt(v));
                }
            }
        }
    }

    Err(ParseError::BadInt.into())
}

fn parse_char_const(text: &str) -> Constant {
    let text = text
        .strip_prefix('\'')
        .and_then(|t| t.strip_suffix('\''))
        .expect("lexer should ensure it is valid");

    Constant::Int(text.as_bytes()[0] as i32)
}

fn parse_octal_char_const(text: &str) -> Constant {
    let text = text
        .strip_prefix("\'\\")
        .and_then(|t| t.strip_suffix('\''))
        .expect("lexer should ensure it is valid");

    let result = i32::from_str_radix(text, 8).expect("lexer should ensure it is valid");

    Constant::Int(result)
}

fn parse_escaped_char_const(text: &str) -> Result<Constant> {
    let text = text.strip_prefix("\'\\").and_then(|t| t.strip_suffix('\''));
    if let Some(Some(char)) = text.map(|t| t.chars().next()) {
        match char {
            '\'' => todo!(),
            '"' => todo!(),
            '?' => todo!(),
            'a' => todo!(),
            'b' => todo!(),
            'f' => todo!(),
            'n' => todo!(),
            'r' => todo!(),
            't' => todo!(),
            'v' => todo!(),
            _ => {
                debug_assert!(false, "lexer should ensure this doesn't occur");
                Err(ParseError::BadCharConst.into())
            }
        }
    } else {
        debug_assert!(false, "lexer should ensure this doesn't occur");
        Err(ParseError::BadCharConst.into())
    }
}

fn parse_float_const(text: &str) -> Result<Constant> {
    // TODO: support binary floating point numbers
    let mut text = text;
    let double = if text.ends_with("f") {
        text = &text[..text.len() - 1];
        false
    } else {
        true
    };

    if double {
        text.parse::<f64>()
            .map(|v| Constant::Double(v))
            .map_err(|e| ParseError::BadFloat(e).into())
    } else {
        text.parse::<f32>()
            .map(|v| Constant::Float(v))
            .map_err(|e| ParseError::BadFloat(e).into())
    }
}

fn parse_string_literal(text: &str) -> Result<String> {
    let without_quotes = &text[1..text.len() - 1];
    let mut unescaped = String::with_capacity(without_quotes.len());
    // unescape the string
    assert!(text.is_ascii());
    let mut unescape = false;
    for char in without_quotes.bytes() {
        if unescape {
            match char {
                b'a' => unescaped.push(0x07.into()),
                b'b' => unescaped.push(0x08.into()),
                b'f' => unescaped.push(0x0c.into()),
                b'n' => unescaped.push(0x0a.into()),
                b'r' => unescaped.push(0x0d.into()),
                b't' => unescaped.push(0x09.into()),
                b'v' => unescaped.push(0x0b.into()),
                _ => unescaped.push(char.into()),
            }
            unescape = false;
        } else {
            if char == b'\\' {
                unescape = true;
            } else {
                unescaped.push(char.into());
            }
        }
    }

    Ok(unescaped)
}

enum ExprPartResult {
    ExprPart(ExprPart),
    ParseError(ParseError),
}

fn to_expr_part(lexeme: CLexemes, text: &str, ident: StringPoolRef, current_scope: Scope) -> Result<Option<ExprPart>> {
    let expr_part = match lexeme {
        CLexemes::Increment => ExprPart::Operator(Operator::Increment),
        CLexemes::Decrement => ExprPart::Operator(Operator::Decrement),
        CLexemes::Dot => ExprPart::Operator(Operator::Dot),
        CLexemes::Arrow => ExprPart::Operator(Operator::Arrow),
        CLexemes::Plus => ExprPart::Operator(Operator::Plus),
        CLexemes::Minus => ExprPart::Operator(Operator::Minus),
        CLexemes::Bang => ExprPart::Operator(Operator::Bang),
        CLexemes::Tilde => ExprPart::Operator(Operator::Tilde),
        CLexemes::Star => ExprPart::Operator(Operator::Star),
        CLexemes::BitAnd => ExprPart::Operator(Operator::BitAnd),
        CLexemes::Sizeof => ExprPart::Operator(Operator::Sizeof),
        CLexemes::Slash => ExprPart::Operator(Operator::Slash),
        CLexemes::Percent => ExprPart::Operator(Operator::Percent),
        CLexemes::RShift => ExprPart::Operator(Operator::RShift),
        CLexemes::LShift => ExprPart::Operator(Operator::LShift),
        CLexemes::Lt => ExprPart::Operator(Operator::Lt),
        CLexemes::Gt => ExprPart::Operator(Operator::Gt),
        CLexemes::Leq => ExprPart::Operator(Operator::Leq),
        CLexemes::Geq => ExprPart::Operator(Operator::Geq),
        CLexemes::Eq => ExprPart::Operator(Operator::Eq),
        CLexemes::Neq => ExprPart::Operator(Operator::Neq),
        CLexemes::Xor => ExprPart::Operator(Operator::Xor),
        CLexemes::BitOr => ExprPart::Operator(Operator::BitOr),
        CLexemes::And => ExprPart::Operator(Operator::And),
        CLexemes::Or => ExprPart::Operator(Operator::Or),

        CLexemes::Assign => ExprPart::Operator(Operator::Assign),
        CLexemes::MultAssign => ExprPart::Operator(Operator::MultiplyAssign),
        CLexemes::DivAssign => ExprPart::Operator(Operator::DivideAssign),
        CLexemes::ModAssign => ExprPart::Operator(Operator::ModuloAssign),
        CLexemes::AddAssign => ExprPart::Operator(Operator::AddAssign),
        CLexemes::SubAssign => ExprPart::Operator(Operator::SubAssign),
        CLexemes::LShiftAssign => ExprPart::Operator(Operator::LShiftAssign),
        CLexemes::RShiftAssign => ExprPart::Operator(Operator::RShiftAssign),
        CLexemes::AndAssign => ExprPart::Operator(Operator::AndAssign),
        CLexemes::XorAssign => ExprPart::Operator(Operator::XorAssign),
        CLexemes::OrAssign => ExprPart::Operator(Operator::OrAssign),

        CLexemes::Comma => ExprPart::Operator(Operator::Comma),
        CLexemes::LParen => ExprPart::Operator(Operator::LParen),
        CLexemes::LBracket => ExprPart::Operator(Operator::LBracket),
        CLexemes::Question => ExprPart::Operator(Operator::Question),
        CLexemes::Colon => ExprPart::Operator(Operator::Colon),

        CLexemes::Identifier => {
            assert!(ident.is_valid());
            let identifier = Identifier::new(current_scope, ident);
            ExprPart::Atom(Atom::Identifier(identifier))
        }

        CLexemes::IntegerConst => {
            let int_const = parse_integer_const(text)?;
            ExprPart::Atom(Atom::Constant(int_const))
        }

        CLexemes::CharConst => {
            let char_const = parse_char_const(text);
            ExprPart::Atom(Atom::Constant(char_const))
        }

        CLexemes::OctalCharConst => {
            let char_const = parse_octal_char_const(text);
            ExprPart::Atom(Atom::Constant(char_const))
        }

        CLexemes::FloatConst => {
            let float_const = parse_float_const(text)?;
            ExprPart::Atom(Atom::Constant(float_const))
        }

        CLexemes::StringConst => {
            let text = parse_string_literal(text)?;

            ExprPart::Atom(Atom::StringLiteral(text))
        }

        _ => return Ok(None),
    };

    Ok(Some(expr_part))
}

const PREFIX_BINDING_POWER: u32 = 25;
fn prefix_binding_power(op: Operator) -> Option<u32> {
    match op {
        // all prefix operators are implicitly right associative
        Operator::Increment
        | Operator::Decrement
        | Operator::Plus
        | Operator::Minus
        | Operator::Bang
        | Operator::Tilde
        | Operator::Star
        | Operator::BitAnd
        | Operator::Sizeof => Some(PREFIX_BINDING_POWER),
        _ => None,
    }
}

const POSTFIX_BINDING_POWER: u32 = 27;
fn postfix_binding_power(op: Operator) -> Option<u32> {
    match op {
        // all postfix operators are implicitly left-associative
        Operator::Increment | Operator::Decrement | Operator::LParen | Operator::LBracket => {
            Some(POSTFIX_BINDING_POWER)
        }

        // grammatically, Dot / Arrow (member access, either directly or through a pointer)
        // are considered "postfix", left-associative operators. however, like array subscript / function calls,
        // they are a special case, since it is required for following token to be an identifier
        // and not any arbitrary expression. something like
        // ```
        // struct example *s;
        // s->(a + b);
        // ```
        // is clearly nonsensical;
        Operator::Dot | Operator::Arrow => Some(POSTFIX_BINDING_POWER),

        _ => return None,
    }
}

fn infix_binding_power(op: Operator) -> Option<(u32, u32)> {
    match op {
        // multiply, divide, modulo are left-associative and equal precedence
        Operator::Star => Some((23, 24)),
        Operator::Slash => Some((23, 24)),
        Operator::Percent => Some((23, 24)),

        // addition, subtraction are left-associative and equal precedence
        Operator::Plus => Some((21, 22)),
        Operator::Minus => Some((21, 22)),

        // shift operators are left-associative and equal precedence
        Operator::RShift => Some((19, 20)),
        Operator::LShift => Some((19, 20)),

        // relational operators are left-associative and equal precedence
        Operator::Lt => Some((17, 18)),
        Operator::Gt => Some((17, 18)),
        Operator::Leq => Some((17, 18)),
        Operator::Geq => Some((17, 18)),

        // equality operators are left-associative and equal precedence
        Operator::Eq => Some((15, 16)),
        Operator::Neq => Some((15, 16)),

        // bitwise and logical operators are all left-associative
        Operator::BitAnd => Some((13, 14)),
        Operator::Xor => Some((11, 12)),
        Operator::BitOr => Some((9, 10)),
        Operator::And => Some((7, 8)),
        Operator::Or => Some((5, 6)),

        // ternary operator is right associative, is also a special case
        // a ? b : c is grammatically isomorphic to a (b) c and is handled
        // as a special case by expression parser
        Operator::Question => Some((4, 3)),

        // assignment operators all have same precedence, right-associativity
        Operator::Assign
        | Operator::MultiplyAssign
        | Operator::DivideAssign
        | Operator::ModuloAssign
        | Operator::AddAssign
        | Operator::SubAssign
        | Operator::LShiftAssign
        | Operator::RShiftAssign
        | Operator::AndAssign
        | Operator::XorAssign
        | Operator::OrAssign => Some((2, 1)),

        _ => return None,
    }
}

// precedence climbing ("Pratt parsing") algorithm with some special case handling
// for C specific syntax. in the first pass, no type checking is done
pub(super) fn parse_expr<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<ExpressionNode> {
    let first = parse_assignment_expr(toks, state)?;
    let mut assignment_exprs = Vec::new();
    match toks.peek() {
        Some((CLexemes::Comma, _, _)) => {
            toks.eat(CLexemes::Comma);
            assignment_exprs.push(first);
            let second = parse_assignment_expr(toks, state)?;
            assignment_exprs.push(second);
        }
        Some((_, _, _)) => {
            return Ok(first);
        }
        None => return Ok(first),
    }
    loop {
        match toks.peek() {
            Some((CLexemes::Comma, _, _)) => {
                toks.eat(CLexemes::Comma);
                let next = parse_assignment_expr(toks, state)?;
                assignment_exprs.push(next);
            }
            Some((_, _, _)) => {
                let comma_expr = ExpressionNode::CommaExpr(assignment_exprs);
                return Ok(comma_expr);
            }
            None => return Err(ParseError::UnexpectedEOF.into()),
        }
    }
}

pub(super) fn parse_assignment_expr<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<ExpressionNode> {
    parse_expr_rec(toks, state, 0)
}

fn parse_expr_rec<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
    min_bp: u32,
) -> Result<ExpressionNode> {
    let mut lhs: ExpressionNode;
    match toks.advance() {
        Some((lexeme, text, ident_ref)) => {
            let expr_part = match to_expr_part(lexeme, toks.text(text), ident_ref, state.current_scope)? {
                Some(expr_part) => expr_part,
                // we expected a token here, i think?
                None => return Err(ParseError::UnexpectedToken(lexeme).into()),
            };
            match expr_part {
                ExprPart::Atom(atom) => {
                    lhs = atom.into();
                }
                ExprPart::Operator(Operator::LParen) => {
                    if super::is_lookahead_type_name(toks, state) {
                        // must be a cast
                        let cast_target_type = super::parse_type_name(toks, state)?;
                        eat_or_error!(toks, CLexemes::RParen)?;

                        let rhs = parse_expr_rec(toks, state, PREFIX_BINDING_POWER)?;

                        lhs = ExpressionNode::Cast(Box::new(rhs), cast_target_type);
                    } else {
                        lhs = parse_expr_rec(toks, state, 0)?;
                        eat_or_error!(toks, CLexemes::RParen)?;
                    }
                }
                ExprPart::Operator(Operator::Sizeof) => {
                    if let Some((CLexemes::LParen, _, _)) = toks.peek() {
                        toks.eat(CLexemes::LParen);
                        if super::is_lookahead_type_name(toks, state) {
                            let type_name = super::parse_type_name(toks, state)?;
                            eat_or_error!(toks, CLexemes::RParen)?;
                            lhs = ExpressionNode::SizeofType(type_name);
                        } else {
                            let inner = parse_expr_rec(toks, state, 0)?;
                            eat_or_error!(toks, CLexemes::RParen)?;
                            lhs = ExpressionNode::Sizeof(Box::new(inner));
                        }
                    } else {
                        let rhs = parse_expr_rec(toks, state, PREFIX_BINDING_POWER)?;
                        lhs = ExpressionNode::Sizeof(Box::new(rhs));
                    }
                }
                ExprPart::Operator(op) => {
                    // it must be a prefix operator
                    if let Some(power) = prefix_binding_power(op) {
                        let rhs = parse_expr_rec(toks, state, power)?;
                        match op {
                            Operator::Increment => {
                                lhs = ExpressionNode::PreIncrement(Box::new(rhs));
                            }
                            Operator::Decrement => {
                                lhs = ExpressionNode::PreDecrement(Box::new(rhs));
                            }
                            Operator::Plus => {
                                lhs = ExpressionNode::UnaryPlus(Box::new(rhs));
                            }
                            Operator::Minus => {
                                lhs = ExpressionNode::UnaryMinus(Box::new(rhs));
                            }
                            Operator::Bang => {
                                lhs = ExpressionNode::Not(Box::new(rhs));
                            }
                            Operator::Tilde => {
                                lhs = ExpressionNode::BitwiseNot(Box::new(rhs));
                            }
                            Operator::Star => {
                                lhs = ExpressionNode::Dereference(Box::new(rhs));
                            }
                            Operator::BitAnd => {
                                lhs = ExpressionNode::AddressOf(Box::new(rhs));
                            }
                            _ => unreachable!(),
                        }
                    } else {
                        return Err(ParseError::UnexpectedToken(lexeme).into());
                    }
                }
            }
        }
        None => return Err(ParseError::UnexpectedEOF.into()),
    }

    loop {
        match toks.peek() {
            Some((lexeme, text, ident_ref)) => {
                let expr_part = match to_expr_part(lexeme, toks.text(text), ident_ref, state.current_scope) {
                    Ok(Some(expr_part)) => expr_part,
                    // next token might not be part of expression at all
                    Ok(None) => break,
                    Err(e @ _) => return Err(e),
                };
                match expr_part {
                    ExprPart::Operator(op) => {
                        if let Some(left_bp) = postfix_binding_power(op) {
                            if left_bp < min_bp {
                                // some higher precedence operator binds to lhs before op does
                                break;
                            }

                            toks.eat(lexeme);

                            match op {
                                Operator::Increment => {
                                    lhs = ExpressionNode::PostIncrement(Box::new(lhs))
                                }
                                Operator::Decrement => {
                                    lhs = ExpressionNode::PostDecrement(Box::new(lhs))
                                }

                                // function call / array subscript
                                Operator::LParen => {
                                    let arguments =
                                        if let Some((CLexemes::RParen, _, _)) = toks.peek() {
                                            Vec::new()
                                        } else {
                                            match parse_expr(toks, state)? {
                                                ExpressionNode::CommaExpr(arg_exprs) => arg_exprs,
                                                single => vec![single],
                                            }
                                        };

                                    eat_or_error!(toks, CLexemes::RParen)?;

                                    lhs = ExpressionNode::FunctionCall(Box::new(lhs), arguments);
                                }
                                Operator::LBracket => {
                                    let index = parse_expr_rec(toks, state, 0)?;
                                    eat_or_error!(toks, CLexemes::RBracket)?;
                                    lhs = ExpressionNode::ArraySubscript(
                                        Box::new(lhs),
                                        Box::new(index),
                                    );
                                }

                                // member access operators - require identifier on the right
                                Operator::Dot => {
                                    // Parse the member identifier
                                    let member_token =
                                        toks.peek().ok_or(ParseError::UnexpectedEOF)?;
                                    if let CLexemes::Identifier = member_token.0 {
                                        let member = Identifier::new(
                                            state.current_scope,
                                            member_token.2,
                                        );
                                        toks.eat(CLexemes::Identifier);
                                        lhs = ExpressionNode::DotAccess(Box::new(lhs), member);
                                    } else {
                                        return Err(ParseError::BadMemberAccess.into());
                                    }
                                }
                                Operator::Arrow => {
                                    // Parse the member identifier
                                    let member_token =
                                        toks.peek().ok_or(ParseError::UnexpectedEOF)?;
                                    if let CLexemes::Identifier = member_token.0 {
                                        let member = Identifier::new(
                                            state.current_scope,
                                            member_token.2
                                        );
                                        toks.eat(CLexemes::Identifier);
                                        lhs = ExpressionNode::ArrowAccess(Box::new(lhs), member);
                                    } else {
                                        return Err(ParseError::BadMemberAccess.into());
                                    }
                                }

                                _ => unreachable!(),
                            }

                            continue;
                        }

                        if let Some((left_bp, right_bp)) = infix_binding_power(op) {
                            if left_bp < min_bp {
                                break;
                            }

                            toks.eat(lexeme);

                            // special case for ternary operator
                            if op == Operator::Question {
                                let middle = parse_expr_rec(toks, state, 0)?;
                                eat_or_error!(toks, CLexemes::Colon)?;
                                let rhs = parse_expr_rec(toks, state, right_bp)?;
                                lhs = ExpressionNode::Ternary(
                                    Box::new(lhs),
                                    Box::new(middle),
                                    Box::new(rhs),
                                );
                                continue;
                            }

                            let rhs = parse_expr_rec(toks, state, right_bp)?;
                            lhs = match op {
                                Operator::Plus => ExpressionNode::Add(Box::new(lhs), Box::new(rhs)),
                                Operator::Minus => {
                                    ExpressionNode::Subtract(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::Star => {
                                    ExpressionNode::Multiply(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::BitAnd => {
                                    ExpressionNode::BitwiseAnd(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::Slash => {
                                    ExpressionNode::Divide(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::Percent => {
                                    ExpressionNode::Modulo(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::RShift => {
                                    ExpressionNode::RShift(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::LShift => {
                                    ExpressionNode::LShift(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::Lt => {
                                    ExpressionNode::LessThan(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::Gt => {
                                    ExpressionNode::GreaterThan(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::Leq => {
                                    ExpressionNode::LessThanOrEqual(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::Geq => {
                                    ExpressionNode::GreaterThanOrEqual(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::Eq => ExpressionNode::Equal(Box::new(lhs), Box::new(rhs)),
                                Operator::Neq => {
                                    ExpressionNode::NotEqual(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::Xor => {
                                    ExpressionNode::BitwiseXor(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::BitOr => {
                                    ExpressionNode::BitwiseOr(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::And => {
                                    ExpressionNode::LogicalAnd(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::Or => {
                                    ExpressionNode::LogicalOr(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::Assign => {
                                    ExpressionNode::SimpleAssign(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::MultiplyAssign => {
                                    ExpressionNode::MultiplyAssign(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::DivideAssign => {
                                    ExpressionNode::DivideAssign(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::ModuloAssign => {
                                    ExpressionNode::ModuloAssign(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::AddAssign => {
                                    ExpressionNode::AddAssign(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::SubAssign => {
                                    ExpressionNode::SubAssign(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::LShiftAssign => {
                                    ExpressionNode::LShiftAssign(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::RShiftAssign => {
                                    ExpressionNode::RShiftAssign(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::AndAssign => {
                                    ExpressionNode::AndAssign(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::XorAssign => {
                                    ExpressionNode::XorAssign(Box::new(lhs), Box::new(rhs))
                                }
                                Operator::OrAssign => {
                                    ExpressionNode::OrAssign(Box::new(lhs), Box::new(rhs))
                                }
                                _ => unreachable!(),
                            };

                            continue;
                        }

                        // must be a prefix operator, e.g. a + !b
                        break;
                    }
                    ExprPart::Atom(_) => return Err(ParseError::UnexpectedToken(lexeme).into()),
                }
            }
            None => {
                break;
            }
        }
    }

    Ok(lhs)
}
