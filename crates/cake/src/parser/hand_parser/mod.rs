use core::panic;
use std::{
    collections::VecDeque,
    num::{ParseFloatError, ParseIntError},
    rc::Rc,
};

use thiserror::Error;

use crate::{
    parser::ast::Constant,
    scanner::{lexeme_sets::c_lexemes::CLexemes, TokenStream},
    semantics::{
        symtab::{Scope, ScopeType, StorageClass, SymtabError, TypeIdx},
        types::{
            AggregateMember, BasicType, CType, CanonicalType, FunctionArgument, FunctionSpecifier,
            QualifiedType, TypeQualifier,
        },
    },
};

use super::ast::{ASTNode, Declaration, ExpressionNode, Identifier};

type CTokenStream<'a> = crate::scanner::RawTokenStream<'a, CLexemes>;

// can explicitly materialize parse tree for debugging purposes, though not required
#[cfg(debug_assertions)]
enum DebugParseNode {
    EnumSpecifier(usize, usize),
    StructSpecifier(usize, usize),
    UnionSpecifier(usize, usize),
}

#[cfg(debug_assertions)]
// start / end are describe position of the text corresponding to this parse node,
// variant is the enum variant for this parse node
// non-leaf parse nodes must be tuple structs (Box<ParseNode>, usize, usize) or (Vec<ParseNode>, usize, usize)
// leaf parse nodes must be tuple structs of (usize, usize)
macro_rules! materialize_debug_parse_node {
    // child count must be tt for it to be matched correctly by sub-macro
    ($start:expr, $end:expr, $variant:expr, $child_count:tt, $node_vec:expr) => {{
        // generates either a Box or Vec
        let parse_node_children = materialize_debug_parse_node! {
            @MATERIALIZE_CHILDREN,
            $child_count,
            $node_vec
        };
        let parse_node = $variant(parse_node_children, $start, $end);
        $node_vec.push_back(parse_node);
    }};

    ($start:expr, $end:expr, $variant:expr, $node_vec:expr) => {{
        // generates either a Box or Vec
        let parse_node = $variant($start, $end);
        $node_vec.push_back(parse_node);
    }};

    (@MATERIALIZE_CHILDREN, 1, $node_vec:expr) => {
        Box::new(
            $node_vec
                .pop_back()
                .expect("Expected at least one element in parse stack"),
        )
    };

    (@MATERIALIZE_CHILDREN, $n:literal, $node_vec:expr) => {{
        let mut children = Vec::with_capacity($n);
        for _ in 0..$n {
            children.push(
                $node_vec
                    .pop_back()
                    .expect("Expected at least $n elements in the vector"),
            );
        }
        children.reverse(); // Reverse to maintain the original order
        children
    }};
}

macro_rules! eat_or_error {
    ($toks:expr, $tok:path) => {
        match $toks.peek() {
            Some(($tok, _, _)) => {
                $toks.eat($tok);
                Ok(())
            }
            Some((other, _, _)) => Err(ParseError::UnexpectedToken(other)),
            None => Err(ParseError::UnexpectedEOF),
        }
    };
}

#[derive(Error, Debug, PartialEq, Eq)]
pub(crate) enum ParseError {
    #[error("Unexpected end of file while parsing")]
    UnexpectedEOF,
    #[error("Unexpected token {0:?} while parsing")]
    UnexpectedToken(CLexemes),
    #[error("Only one storage class allowed in declaration")]
    UnexpectedStorageClass,
    #[error("restrict qualifier only applies to pointers")]
    BadRestrictQualifier,
    #[error("failed to lookup item in symbol table")]
    LookupError(String),
    #[error("cannot declare enum `{0}` in same scope due to conflicting tag")]
    RedeclaredEnum(String),
    #[error("cannot declare struct `{0}` in same scope due to conflicting tag")]
    RedeclaredStruct(String),
    #[error("cannot declare union `{0}` in same scope due to conflicting tag")]
    RedeclaredUnion(String),
    #[error("cannot redeclare enum constant")]
    RedeclaredEnumConstant(#[source] SymtabError),
    #[error("invalid enum constant (must be integer constant)")]
    InvalidEnumConstant(String),
    #[error("empty enum is not allowed")]
    EmptyEnum,
    #[error("incomplete enum <tag> cannot be declared alongside struct <tag> or union <tag>")]
    EnumDeclarationMustMatch,
    #[error("incomplete struct or enum declaration with wrong type of tag")]
    StructOrEnumDeclarationMustMatch,
    #[error("indeterminate symtab error")]
    OtherSymtabError(#[from] SymtabError),
    #[error("problem while parsing declarator")]
    BadDeclarator,
    #[error("direct declarator requires a name")]
    DeclaratorRequiresName,
    #[error("abstract declarator should not have name")]
    UnexpectedDeclaratorName,
    #[error("error while parsing array bound")]
    BadArrayBound(#[source] ParseIntError),
    #[error("closed file scope while parsing")]
    ClosedFileScope,
    #[error("function specifier (inline) only allowed in function definitions")]
    BadFunctionSpecifier,
    #[error("only register storage class allowed in function argument list")]
    BadStorageClassInArgumentList,
    #[error("incomplete types (void, undefined structs, etc.) not allowed in function arguments")]
    IncompleteFunctionArgument,
    #[error("internal parsing error: canonical types should not be used here")]
    CanonicalTypeOutsideSymtab,
    #[error("unmatched parentheses in declarator")]
    UnmatchedParensInDeclarator,
    #[error("multiple types in declaration")]
    MultipleTypeSpecifiersInDeclaration,
    #[error("at least on type specifier required in declaration")]
    MissingTypeSpecifier,
    #[error("error while parsing basic type")]
    BasicTypeError,
    #[error(
        "member access (with '.' or '->' operators) expects identifier, not arbitrary expression"
    )]
    BadMemberAccess,
    #[error("redeclared label name within same function")]
    RedeclaredLabel(#[source] SymtabError),
    #[error("couldn't parse int")]
    BadInt,
    #[error("couldn't parse float")]
    BadFloat(#[source] ParseFloatError),
    #[error("bad function definition")]
    BadFunctionDefinition,
    #[error("cannot crate array of incomplete type")]
    ArrayOfIncompleteType,
    #[error("function cannot return function / array")]
    BadFunctionReturnType,
    #[error("internal compiler error: bad string constant")]
    BadStringConst,
}

pub(crate) struct ParserState {
    scopes: Vec<Scope>,
    current_scope: Scope,
    types: Vec<CanonicalType>,

    #[cfg(debug_assertions)]
    parse_tree_stack: VecDeque<DebugParseNode>,
}

impl ParserState {
    pub(crate) fn new() -> Self {
        let file_scope = Scope::new_file_scope();
        Self {
            scopes: vec![file_scope],
            current_scope: file_scope,
            types: Vec::new(),

            #[cfg(debug_assertions)]
            parse_tree_stack: VecDeque::new(),
        }
    }

    fn open_scope(&mut self, scope_type: ScopeType) {
        let new_scope = Scope::new(scope_type, self.current_scope.index, self.scopes.len());
        self.scopes.push(new_scope);
        self.current_scope = new_scope;
    }

    fn close_scope(&mut self) -> Result<(), ParseError> {
        let parent_index = self
            .current_scope
            .parent_scope
            .ok_or(ParseError::ClosedFileScope)?;
        self.current_scope = self.scopes[parent_index];

        Ok(())
    }

    fn add_type(&mut self, canonical_type: CanonicalType) -> TypeIdx {
        let idx = self.types.len();
        self.types.push(canonical_type);
        TypeIdx(idx)
    }
}

// TODO: error recovery
#[derive(Debug, Clone)]
enum Atom {
    Identifier(Identifier),
    Constant(Constant),
    StringLiteral(String),
}

impl From<Atom> for ExpressionNode {
    fn from(value: Atom) -> Self {
        match value {
            Atom::Identifier(ident) => ExpressionNode::Identifier(ident, None),
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

fn parse_integer_const(text: &str) -> Result<Constant, ParseError> {
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

    Err(ParseError::BadInt)
}

fn parse_float_const(text: &str) -> Result<Constant, ParseError> {
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
            .map_err(|e| ParseError::BadFloat(e))
    } else {
        text.parse::<f32>()
            .map(|v| Constant::Float(v))
            .map_err(|e| ParseError::BadFloat(e))
    }
}

fn to_expr_part(
    lexeme: CLexemes,
    text: &str,
    state: &mut ParserState,
) -> Result<ExprPart, ParseError> {
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
            let identifier = Identifier::new(state.current_scope, text.to_string());
            ExprPart::Atom(Atom::Identifier(identifier))
        }

        CLexemes::IntegerConst => {
            let int_const = parse_integer_const(text)?;
            ExprPart::Atom(Atom::Constant(int_const))
        }

        CLexemes::FloatConst => {
            let float_const = parse_float_const(text)?;
            ExprPart::Atom(Atom::Constant(float_const))
        }

        CLexemes::StringConst => {
            let text = match (
                text.starts_with('"'),
                text.ends_with('"') && !text.ends_with("\\\""),
            ) {
                (true, true) => text
                    .strip_prefix('"')
                    .and_then(|t| t.strip_suffix('"'))
                    .expect("should be infallible"),
                (true, false) | (false, true) => return Err(ParseError::BadStringConst),
                (false, false) => {
                    text // nop
                }
            };
            let text = text.to_string();
            // remove quotes if needed

            ExprPart::Atom(Atom::StringLiteral(text))
        }

        _ => return Err(ParseError::UnexpectedToken(lexeme)),
    };

    Ok(expr_part)
}

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
        | Operator::Sizeof => Some(25),
        _ => None,
    }
}

fn postfix_binding_power(op: Operator) -> Option<u32> {
    match op {
        // all postfix operators are implicitly left-associative
        Operator::Increment | Operator::Decrement | Operator::LParen | Operator::LBracket => {
            Some(27)
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
        Operator::Dot | Operator::Arrow => Some(27),

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
pub(crate) fn parse_expr(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<ExpressionNode, ParseError> {
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
                let comma_expr = ExpressionNode::CommaExpr(assignment_exprs, None);
                return Ok(comma_expr);
            }
            None => return Err(ParseError::UnexpectedEOF),
        }
    }
}

fn parse_assignment_expr(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<ExpressionNode, ParseError> {
    parse_expr_rec(toks, state, 0)
}

fn parse_expr_rec(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
    min_bp: u32,
) -> Result<ExpressionNode, ParseError> {
    let mut lhs: ExpressionNode;
    match toks.advance() {
        Some((lexeme, text, _)) => {
            match to_expr_part(lexeme, text, state)? {
                ExprPart::Atom(atom) => {
                    lhs = atom.into();
                }
                ExprPart::Operator(Operator::LParen) => {
                    lhs = parse_expr_rec(toks, state, 0)?;
                    eat_or_error!(toks, CLexemes::LParen)?;
                }
                ExprPart::Operator(op) => {
                    // it must be a prefix operator
                    if let Some(power) = prefix_binding_power(op) {
                        let rhs = parse_expr_rec(toks, state, power)?;
                        match op {
                            Operator::Increment => {
                                lhs = ExpressionNode::PreIncrement(Box::new(rhs), None);
                            }
                            Operator::Decrement => {
                                lhs = ExpressionNode::PreDecrement(Box::new(rhs), None);
                            }
                            Operator::Plus => {
                                lhs = ExpressionNode::UnaryPlus(Box::new(rhs), None);
                            }
                            Operator::Minus => {
                                lhs = ExpressionNode::UnaryMinus(Box::new(rhs), None);
                            }
                            Operator::Bang => {
                                lhs = ExpressionNode::Not(Box::new(rhs), None);
                            }
                            Operator::Tilde => {
                                lhs = ExpressionNode::BitwiseNot(Box::new(rhs), None);
                            }
                            Operator::Star => {
                                lhs = ExpressionNode::Dereference(Box::new(rhs), None);
                            }
                            Operator::BitAnd => {
                                lhs = ExpressionNode::AddressOf(Box::new(rhs), None);
                            }
                            Operator::Sizeof => {
                                // TODO: special lookahead for sizeof type name
                                // (as opposed to sizeof an expression)
                                lhs = ExpressionNode::Sizeof(Box::new(rhs), None);
                            }
                            // to avoid cluttering with additional types,
                            // no separate "PrefixOperator" enum. however,
                            // op cannot be any other value
                            _ => unreachable!(),
                        }
                    } else {
                        return Err(ParseError::UnexpectedToken(lexeme));
                    }
                }
            }
        }
        None => return Err(ParseError::UnexpectedEOF),
    }

    loop {
        match toks.peek() {
            Some((lexeme, text, _)) => {
                let expr_part = match to_expr_part(lexeme, text, state) {
                    Ok(expr_part) => expr_part,
                    // next token might not be part of expression at all
                    Err(ParseError::UnexpectedToken(_)) => break,
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
                                    lhs = ExpressionNode::PostIncrement(Box::new(lhs), None)
                                }
                                Operator::Decrement => {
                                    lhs = ExpressionNode::PostDecrement(Box::new(lhs), None)
                                }

                                // function call / array subscript
                                Operator::LParen => {
                                    let arguments = if let Some((CLexemes::RParen, _, _)) =
                                        toks.peek()
                                    {
                                        Vec::new()
                                    } else {
                                        match parse_expr(toks, state)? {
                                            ExpressionNode::CommaExpr(arg_exprs, _) => arg_exprs,
                                            single => vec![single],
                                        }
                                    };

                                    eat_or_error!(toks, CLexemes::RParen)?;

                                    lhs = ExpressionNode::FunctionCall(
                                        Box::new(lhs),
                                        arguments,
                                        None,
                                    );
                                }
                                Operator::LBracket => {
                                    let index = parse_expr_rec(toks, state, 0)?;
                                    eat_or_error!(toks, CLexemes::RBracket)?;
                                    lhs = ExpressionNode::ArraySubscript(
                                        Box::new(lhs),
                                        Box::new(index),
                                        None,
                                    );
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
                                    None,
                                );
                                continue;
                            }

                            let rhs = parse_expr_rec(toks, state, right_bp)?;
                            lhs = match op {
                                Operator::Plus => {
                                    ExpressionNode::Add(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::Minus => {
                                    ExpressionNode::Subtract(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::Star => {
                                    ExpressionNode::Multiply(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::BitAnd => {
                                    ExpressionNode::BitwiseAnd(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::Slash => {
                                    ExpressionNode::Divide(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::Percent => {
                                    ExpressionNode::Modulo(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::RShift => {
                                    ExpressionNode::RShift(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::LShift => {
                                    ExpressionNode::LShift(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::Lt => {
                                    ExpressionNode::LessThan(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::Gt => {
                                    ExpressionNode::GreaterThan(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::Leq => ExpressionNode::LessThanOrEqual(
                                    Box::new(lhs),
                                    Box::new(rhs),
                                    None,
                                ),
                                Operator::Geq => ExpressionNode::GreaterThanOrEqual(
                                    Box::new(lhs),
                                    Box::new(rhs),
                                    None,
                                ),
                                Operator::Eq => {
                                    ExpressionNode::Equal(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::Neq => {
                                    ExpressionNode::NotEqual(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::Xor => {
                                    ExpressionNode::BitwiseXor(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::BitOr => {
                                    ExpressionNode::BitwiseOr(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::And => {
                                    ExpressionNode::LogicalAnd(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::Or => {
                                    ExpressionNode::LogicalOr(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::Assign => {
                                    ExpressionNode::SimpleAssign(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::MultiplyAssign => ExpressionNode::MultiplyAssign(
                                    Box::new(lhs),
                                    Box::new(rhs),
                                    None,
                                ),
                                Operator::DivideAssign => {
                                    ExpressionNode::DivideAssign(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::ModuloAssign => {
                                    ExpressionNode::ModuloAssign(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::AddAssign => {
                                    ExpressionNode::AddAssign(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::SubAssign => {
                                    ExpressionNode::SubAssign(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::LShiftAssign => {
                                    ExpressionNode::LShiftAssign(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::RShiftAssign => {
                                    ExpressionNode::RShiftAssign(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::AndAssign => {
                                    ExpressionNode::AndAssign(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::XorAssign => {
                                    ExpressionNode::XorAssign(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::OrAssign => {
                                    ExpressionNode::OrAssign(Box::new(lhs), Box::new(rhs), None)
                                }
                                Operator::Dot => {
                                    if let ExpressionNode::Identifier(member, _) = rhs {
                                        ExpressionNode::DotAccess(Box::new(lhs), member, None)
                                    } else {
                                        return Err(ParseError::BadMemberAccess);
                                    }
                                }
                                Operator::Arrow => {
                                    if let ExpressionNode::Identifier(member, _) = rhs {
                                        ExpressionNode::ArrowAccess(Box::new(lhs), member, None)
                                    } else {
                                        return Err(ParseError::BadMemberAccess);
                                    }
                                }
                                _ => unreachable!(),
                            };

                            continue;
                        }

                        // must be a prefix operator, e.g. a + !b
                        break;
                    }
                    ExprPart::Atom(_) => return Err(ParseError::UnexpectedToken(lexeme)),
                }
            }
            None => {
                break;
            }
        }
    }

    Ok(lhs)
}

// <translation-unit> ::= <external-declaration>
// | <translation-unit> <external-declaration>
pub(crate) fn parse_translation_unit(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<ASTNode, ParseError> {
    let mut external_declarations = Vec::new();
    while let Some(_) = toks.peek() {
        external_declarations.push(parse_external_declaration(toks, state)?);
    }

    let translation_unit = ASTNode::TranslationUnit(external_declarations, state.current_scope);

    Ok(translation_unit)
}

// <external-declaration> ::= <function-definition>
// | <declaration>
// way to distinguish is that declarations end with a semicolon while function definitions
// have a compound statement
fn parse_external_declaration(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<ASTNode, ParseError> {
    // both will include declaration specifiers
    let declaration_specifiers = parse_declaration_specifiers(toks, state)?;
    let DeclarationSpecifiers {
        qualified_type,
        storage_class,
        function_specifier,
    } = declaration_specifiers.clone();
    // both will include a (concrete) declarator
    let (declaration_type, name) = parse_declarator(toks, state, qualified_type)?;
    let mut declarations: Vec<Declaration> = Vec::new();

    match toks.peek() {
        Some((CLexemes::LBrace, _, _)) => {
            // must be a function definition; declarator must be function type
            match declaration_type.base_type {
                CType::FunctionTypeRef { symtab_idx } => {
                    // set scope to function prototype scope so that we inherit parameter names
                    let fn_type = &state.types[symtab_idx];
                    if let CanonicalType::FunctionType {
                        prototype_scope, ..
                    } = fn_type
                    {
                        state.current_scope = *prototype_scope;
                    } else {
                        panic!("type table is corrupted");
                    }
                }
                _ => return Err(ParseError::BadFunctionDefinition),
            }
            let function_body = parse_compound_statement(toks, state)?;
            // go back to file scope
            state.close_scope()?;

            let fn_declaration = Declaration::new(
                Identifier::new(state.current_scope, name),
                declaration_type,
                storage_class,
                function_specifier,
                None,
            );

            let function_node = ASTNode::FunctionDefinition(
                Box::new(fn_declaration),
                Box::new(function_body),
                state.current_scope,
            );

            return Ok(function_node);
        }

        // otherwise, must be a declaration
        // try to parse initializer; some duplicated logic from parse_init_declarators but
        // this is hard to avoid
        Some((CLexemes::Eq, _, _)) => {
            toks.eat(CLexemes::Eq);
            let initializer = parse_initializer(toks, state)?;
            let declaration = Declaration::new(
                Identifier::new(state.current_scope, name),
                declaration_type,
                storage_class,
                function_specifier,
                Some(Box::new(initializer)),
            );
            declarations.push(declaration)
        }
        Some((_, _, _)) => {
            let declaration = Declaration::new(
                Identifier::new(state.current_scope, name),
                declaration_type,
                storage_class,
                function_specifier,
                None,
            );
            declarations.push(declaration);
        }
        None => {
            return Err(ParseError::UnexpectedEOF);
        }
    }

    loop {
        match toks.peek() {
            Some((CLexemes::Comma, _, _)) => {
                toks.eat(CLexemes::Comma);
                let declaration =
                    parse_init_declarator(toks, state, declaration_specifiers.clone())?;
                declarations.push(declaration);
            }
            Some((CLexemes::Semicolon, _, _)) => {
                let decl_node = ASTNode::Declaration(declarations);
                return Ok(decl_node);
            }
            Some((other, _, _)) => {
                return Err(ParseError::UnexpectedToken(other));
            }
            None => {
                return Err(ParseError::UnexpectedEOF);
            }
        }
    }
}

fn parse_declaration(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<ASTNode, ParseError> {
    let declaration_specifiers = parse_declaration_specifiers(toks, state)?;
    let res = parse_init_declarators(toks, state, declaration_specifiers);
    eat_or_error!(toks, CLexemes::Semicolon)?;

    res
}

// may need to change this interface later
// type names are used during cast expressions, as argument of sizeof, and in compound initializers
fn parse_type_name(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<QualifiedType, ParseError> {
    let base_type = parse_specifier_qualifier_list(toks, state)?;
    let derived_type = parse_abstract_declarator(toks, state, base_type)?;
    Ok(derived_type)
}

// distinguish between a declaration vs. expression
// will need to be careful with typedefs here
fn is_lookahead_declaration(
    toks: &mut impl TokenStream<CLexemes>,
    _state: &mut ParserState,
) -> bool {
    match toks.peek() {
        Some((lexeme, _, _)) => match lexeme {
            CLexemes::Typedef
            | CLexemes::Extern
            | CLexemes::Static
            | CLexemes::Auto
            | CLexemes::Register
            | CLexemes::Void
            | CLexemes::Char
            | CLexemes::Short
            | CLexemes::Int
            | CLexemes::Long
            | CLexemes::Float
            | CLexemes::Double
            | CLexemes::Unsigned
            | CLexemes::Signed
            | CLexemes::Struct
            | CLexemes::Union
            | CLexemes::Enum => true,
            _ => false,
        },
        None => false,
    }
}

#[derive(Debug, Clone)]
struct DeclarationSpecifiers {
    qualified_type: QualifiedType,
    storage_class: StorageClass,
    function_specifier: FunctionSpecifier,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum BasicTypeLexeme {
    Void,
    Unsigned,
    Signed,
    Char,
    Short,
    Long,
    Float,
    Double,
    Int,
}

impl TryFrom<CLexemes> for BasicTypeLexeme {
    type Error = ParseError;

    fn try_from(value: CLexemes) -> Result<Self, Self::Error> {
        match value {
            CLexemes::Void => Ok(Self::Void),
            CLexemes::Unsigned => Ok(Self::Unsigned),
            CLexemes::Signed => Ok(Self::Signed),
            CLexemes::Char => Ok(Self::Char),
            CLexemes::Short => Ok(Self::Short),
            CLexemes::Long => Ok(Self::Long),
            CLexemes::Float => Ok(Self::Float),
            CLexemes::Double => Ok(Self::Double),
            CLexemes::Int => Ok(Self::Int),
            _ => Err(ParseError::BasicTypeError),
        }
    }
}

fn parse_basic_type(basic_type_specifiers: &mut [BasicTypeLexeme]) -> Result<CType, ParseError> {
    // should be ok for now, optimize later
    match basic_type_specifiers {
        [BasicTypeLexeme::Void] => Ok(CType::Void),
        // default is signed char
        [BasicTypeLexeme::Char] | [BasicTypeLexeme::Signed, BasicTypeLexeme::Char] => {
            Ok(CType::BasicType {
                basic_type: BasicType::Char,
            })
        }
        [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Char] => Ok(CType::BasicType {
            basic_type: BasicType::UChar,
        }),
        [BasicTypeLexeme::Short]
        | [BasicTypeLexeme::Signed, BasicTypeLexeme::Short]
        | [BasicTypeLexeme::Short, BasicTypeLexeme::Int]
        | [BasicTypeLexeme::Signed, BasicTypeLexeme::Short, BasicTypeLexeme::Int] => {
            Ok(CType::BasicType {
                basic_type: BasicType::Short,
            })
        }
        [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Short]
        | [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Short, BasicTypeLexeme::Int] => {
            Ok(CType::BasicType {
                basic_type: BasicType::UShort,
            })
        }
        [BasicTypeLexeme::Int]
        | [BasicTypeLexeme::Signed]
        | [BasicTypeLexeme::Signed, BasicTypeLexeme::Int] => Ok(CType::BasicType {
            basic_type: BasicType::Int,
        }),
        [BasicTypeLexeme::Unsigned] | [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Int] => {
            Ok(CType::BasicType {
                basic_type: BasicType::UInt,
            })
        }
        [BasicTypeLexeme::Long]
        | [BasicTypeLexeme::Signed, BasicTypeLexeme::Long]
        | [BasicTypeLexeme::Long, BasicTypeLexeme::Int]
        | [BasicTypeLexeme::Signed, BasicTypeLexeme::Long, BasicTypeLexeme::Int]
        | [BasicTypeLexeme::Long, BasicTypeLexeme::Long]
        | [BasicTypeLexeme::Signed, BasicTypeLexeme::Long, BasicTypeLexeme::Long]
        | [BasicTypeLexeme::Long, BasicTypeLexeme::Long, BasicTypeLexeme::Int]
        | [BasicTypeLexeme::Signed, BasicTypeLexeme::Long, BasicTypeLexeme::Long, BasicTypeLexeme::Int] => {
            Ok(CType::BasicType {
                basic_type: BasicType::Long,
            })
        }
        [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Long]
        | [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Long, BasicTypeLexeme::Int]
        | [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Long, BasicTypeLexeme::Long]
        | [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Long, BasicTypeLexeme::Long, BasicTypeLexeme::Int] => {
            Ok(CType::BasicType {
                basic_type: BasicType::ULong,
            })
        }
        [BasicTypeLexeme::Double] | [BasicTypeLexeme::Long, BasicTypeLexeme::Double] => {
            Ok(CType::BasicType {
                basic_type: BasicType::Double,
            })
        }
        [BasicTypeLexeme::Float] => Ok(CType::BasicType {
            basic_type: BasicType::Float,
        }),

        _ => Err(ParseError::BasicTypeError),
    }
}

// no typedefs for now
fn parse_declaration_specifiers_base(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
    parse_function_specifiers: bool,
    parse_storage_class: bool,
) -> Result<DeclarationSpecifiers, ParseError> {
    let mut storage_class: StorageClass = StorageClass::None;
    let mut function_specifier: FunctionSpecifier = FunctionSpecifier::None;
    let mut type_qualifier: TypeQualifier = TypeQualifier::empty();
    let mut primitive_type_specifiers: Vec<BasicTypeLexeme> = Vec::new();
    let mut struct_or_union_or_enum: Option<CType> = None;
    loop {
        match toks.peek() {
            Some((lexeme, _, _)) => match lexeme {
                CLexemes::Extern
                | CLexemes::Auto
                | CLexemes::Register
                | CLexemes::Static
                | CLexemes::Typedef
                    if !parse_storage_class =>
                {
                    break;
                }

                CLexemes::Typedef => {
                    todo!("add typedef support")
                }

                // storage class specifiers, only 1 allowed
                CLexemes::Extern => {
                    if storage_class != StorageClass::None {
                        return Err(ParseError::UnexpectedStorageClass);
                    }
                    toks.eat(CLexemes::Extern);
                    storage_class = StorageClass::Extern;
                }
                CLexemes::Auto => {
                    if storage_class != StorageClass::None {
                        return Err(ParseError::UnexpectedStorageClass);
                    }
                    toks.eat(CLexemes::Auto);
                    storage_class = StorageClass::Auto;
                }
                CLexemes::Register => {
                    if storage_class != StorageClass::None {
                        return Err(ParseError::UnexpectedStorageClass);
                    }
                    toks.eat(CLexemes::Register);
                    storage_class = StorageClass::Register;
                }
                CLexemes::Static => {
                    if storage_class != StorageClass::None {
                        return Err(ParseError::UnexpectedStorageClass);
                    }
                    toks.eat(CLexemes::Static);
                    storage_class = StorageClass::Static;
                }

                // type qualifiers, multiple allowed
                CLexemes::Const => {
                    toks.eat(CLexemes::Const);
                    type_qualifier |= TypeQualifier::Const;
                }
                CLexemes::Restrict => {
                    /* RESOLVE LOGIC
                    return Err(ParseError::BadRestrictQualifier);
                     */
                    toks.eat(CLexemes::Restrict);
                    type_qualifier |= TypeQualifier::Restrict;
                }
                CLexemes::Volatile => {
                    toks.eat(CLexemes::Volatile);
                    type_qualifier |= TypeQualifier::Volatile;
                }

                // function specifiers
                CLexemes::Inline => {
                    if !parse_function_specifiers {
                        break;
                    }
                    toks.eat(CLexemes::Inline);
                    function_specifier = FunctionSpecifier::Inline;
                }

                // according to standard, these are allowed in any order and allowed to be mixed w/ any part of the specifier
                // (why???)
                CLexemes::Void
                | CLexemes::Char
                | CLexemes::Short
                | CLexemes::Int
                | CLexemes::Long
                | CLexemes::Float
                | CLexemes::Double
                | CLexemes::Signed
                | CLexemes::Unsigned
                    if struct_or_union_or_enum.is_none() =>
                {
                    toks.eat(lexeme);
                    primitive_type_specifiers.push(lexeme.try_into()?);
                }

                CLexemes::Void
                | CLexemes::Char
                | CLexemes::Short
                | CLexemes::Int
                | CLexemes::Long
                | CLexemes::Float
                | CLexemes::Double
                | CLexemes::Signed
                | CLexemes::Unsigned => {
                    return Err(ParseError::MultipleTypeSpecifiersInDeclaration);
                }

                // struct / union specifier (syntactically very similar!)
                CLexemes::Struct
                    if struct_or_union_or_enum.is_none()
                        && primitive_type_specifiers.is_empty() =>
                {
                    let struct_type = parse_struct_or_union_specifier(toks, state)?;
                    struct_or_union_or_enum = Some(CType::StructureTypeRef {
                        symtab_idx: struct_type,
                    });
                }
                CLexemes::Union
                    if struct_or_union_or_enum.is_none()
                        && primitive_type_specifiers.is_empty() =>
                {
                    let union_type = parse_struct_or_union_specifier(toks, state)?;
                    struct_or_union_or_enum = Some(CType::UnionTypeRef {
                        symtab_idx: union_type,
                    });
                }

                CLexemes::Struct | CLexemes::Union => {
                    return Err(ParseError::MultipleTypeSpecifiersInDeclaration);
                }

                // enum specifier
                CLexemes::Enum
                    if struct_or_union_or_enum.is_none()
                        && primitive_type_specifiers.is_empty() =>
                {
                    let enum_type = parse_enum_specifier(toks, state)?;
                    struct_or_union_or_enum = Some(CType::EnumTypeRef {
                        symtab_idx: enum_type,
                    });
                }

                CLexemes::Enum => {
                    return Err(ParseError::MultipleTypeSpecifiersInDeclaration);
                }

                _ => {
                    break;
                }
            },
            None => {
                break;
            }
        }
    }

    let qualified_type = if !primitive_type_specifiers.is_empty() {
        let basic_type = parse_basic_type(&mut primitive_type_specifiers)?;
        QualifiedType {
            base_type: basic_type,
            qualifier: type_qualifier,
        }
    } else if let Some(ty) = struct_or_union_or_enum {
        QualifiedType {
            base_type: ty,
            qualifier: type_qualifier,
        }
    } else {
        return Err(ParseError::MissingTypeSpecifier);
    };
    let declaration_specifiers = DeclarationSpecifiers {
        qualified_type,
        storage_class,
        function_specifier,
    };

    Ok(declaration_specifiers)
}

fn parse_declaration_specifiers(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<DeclarationSpecifiers, ParseError> {
    parse_declaration_specifiers_base(toks, state, true, true)
}

fn parse_specifier_qualifier_list(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<QualifiedType, ParseError> {
    let DeclarationSpecifiers { qualified_type, .. } =
        parse_declaration_specifiers_base(toks, state, false, false)?;
    Ok(qualified_type)
}

fn parse_struct_declaration_list(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<Vec<AggregateMember>, ParseError> {
    let mut members: Vec<AggregateMember> = Vec::new();
    loop {
        match toks.peek() {
            Some((CLexemes::RBrace, _, _)) => {
                break;
            }
            Some((_, _, _)) => {
                let base_type = parse_specifier_qualifier_list(toks, state)?;
                parse_struct_declarator_list(toks, state, base_type, &mut members)?;
                eat_or_error!(toks, CLexemes::Semicolon)?;
            }
            None => {
                return Err(ParseError::UnexpectedEOF);
            }
        }
    }
    Ok(members)
}

fn parse_struct_declarator_list(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
    base_type: QualifiedType,
    members: &mut Vec<AggregateMember>,
) -> Result<(), ParseError> {
    // TODO: support bitfields

    let (member_type, member_name) = parse_declarator(toks, state, base_type.clone())?;
    members.push((member_name, member_type));
    loop {
        match toks.peek() {
            Some((CLexemes::Comma, _, _)) => {
                // cloning is not expensive since base_type is not a derived type
                let (member_type, member_name) = parse_declarator(toks, state, base_type.clone())?;
                members.push((member_name, member_type));
            }
            Some((_, _, _)) => return Ok(()),
            None => {
                return Err(ParseError::UnexpectedEOF);
            }
        }
    }
}

fn parse_struct_or_union_specifier(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<TypeIdx, ParseError> {
    #[derive(PartialEq, Eq)]
    enum StructOrUnion {
        Struct,
        Union,
    }

    let specifier_type: StructOrUnion;
    match toks.peek() {
        Some((CLexemes::Struct, _, _)) => {
            toks.eat(CLexemes::Struct);
            specifier_type = StructOrUnion::Struct;
        }
        Some((CLexemes::Union, _, _)) => {
            toks.eat(CLexemes::Union);
            specifier_type = StructOrUnion::Union;
        }
        Some((other, _, _)) => {
            return Err(ParseError::UnexpectedToken(other));
        }
        None => {
            return Err(ParseError::UnexpectedEOF);
        }
    }

    let struct_or_union_tag = match toks.peek() {
        Some((CLexemes::LBrace, _, _)) => {
            // untagged, won't conflict and always declares a new type (6.7.2.3 5)
            None
        }
        Some((CLexemes::Identifier, tag, _)) => {
            // do lookup, insert incomplete type if not present
            let tag = String::from(tag);
            toks.eat(CLexemes::Identifier);

            match toks.peek() {
                Some((CLexemes::LBrace, _, _)) => Some(tag),
                Some((_, _, _)) => {
                    /* RESOLVE LOGIC
                    let lookup = state.symbol_table.lookup_tag_type_idx(state.current_scope, struct_or_union_tag.as_ref().unwrap());
                    match lookup {
                        Some(tag_type) => {
                            match state.symbol_table.get_type(tag_type) {
                                CanonicalType::UnionType { .. } if specifier_type == StructOrUnion::Union => {
                                    return Ok(tag_type);
                                },
                                CanonicalType::StructureType { .. } if specifier_type == StructOrUnion::Struct => {
                                    return Ok(tag_type);
                                }
                                _ => {
                                    // <...> declared as wrong type of tag
                                    return Err(ParseError::StructOrEnumDeclarationMustMatch);
                                }
                            }
                        },
                        // incomplete struct or union is allowed
                        None => {
                            let incomplete_type = match specifier_type {
                                StructOrUnion::Struct => {
                                    CanonicalType::IncompleteStructureType { tag: struct_or_union_tag.clone().unwrap() }
                                },
                                StructOrUnion::Union => {
                                    CanonicalType::IncompleteUnionType { tag: struct_or_union_tag.clone().unwrap() }
                                },
                            };
                            let incomplete_type = state.symbol_table.add_type(incomplete_type);
                            state.symbol_table.add_tag(state.current_scope, struct_or_union_tag.unwrap(), incomplete_type)?;

                            let end = toks.get_location();
                            #[cfg(debug_assertions)]
                            match specifier_type {
                                StructOrUnion::Struct => state.parse_tree_stack.push_back(DebugParseNode::StructSpecifier(start, end)),
                                StructOrUnion::Union => state.parse_tree_stack.push_back(DebugParseNode::UnionSpecifier(start, end)),
                            }
                            return Ok(incomplete_type);
                        }
                    }
                    */

                    let incomplete_type = match specifier_type {
                        StructOrUnion::Struct => CanonicalType::IncompleteStructureType { tag },
                        StructOrUnion::Union => CanonicalType::IncompleteUnionType { tag },
                    };

                    let incomplete_type_idx = state.add_type(incomplete_type);
                    return Ok(incomplete_type_idx);
                }
                None => {
                    return Err(ParseError::UnexpectedEOF);
                }
            }
        }
        Some((other, _, _)) => {
            return Err(ParseError::UnexpectedToken(other));
        }
        None => {
            return Err(ParseError::UnexpectedEOF);
        }
    };

    // declaring a new struct / union
    toks.eat(CLexemes::LBrace);

    // 1. not allowed to redeclare enum
    /* RESOLVE LOGIC
    if let Some(struct_or_union_tag) = &struct_or_union_tag {
        let direct_lookup = state.symbol_table.direct_lookup_tag_type(state.current_scope, &struct_or_union_tag);
        if direct_lookup.is_some() {
            let err = match specifier_type {
                StructOrUnion::Struct => ParseError::RedeclaredStruct(struct_or_union_tag.to_string()),
                StructOrUnion::Union => ParseError::RedeclaredUnion(struct_or_union_tag.to_string()),
            };
            return Err(err);
        }
    }
     */

    let members: Vec<AggregateMember> = parse_struct_declaration_list(toks, state)?;
    eat_or_error!(toks, CLexemes::RBrace)?;

    let struct_or_union_type = match specifier_type {
        StructOrUnion::Struct => CanonicalType::StructureType {
            tag: struct_or_union_tag,
            members,
        },
        StructOrUnion::Union => CanonicalType::UnionType {
            tag: struct_or_union_tag,
            members,
        },
    };

    let type_idx = state.add_type(struct_or_union_type);
    /* RESOLVE LOGIC
    if let Some(name) = struct_or_union_tag {
        state.symbol_table.add_tag(state.current_scope, name, type_idx)?;
    }
    */

    Ok(type_idx)
}

fn parse_enum_specifier(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<TypeIdx, ParseError> {
    let start = toks.get_location();
    match toks.peek() {
        Some((CLexemes::Enum, _, _)) => {
            toks.eat(CLexemes::Enum);
        }
        Some((other, _, _)) => {
            return Err(ParseError::UnexpectedToken(other));
        }
        None => {
            return Err(ParseError::UnexpectedEOF);
        }
    }

    let enum_tag: Option<String>;
    match toks.peek() {
        Some((CLexemes::LBrace, _, _)) => {
            // untagged enum, always declares a new type
            toks.eat(CLexemes::LBrace);
            enum_tag = None;
        }
        Some((CLexemes::Identifier, ident, _)) => {
            enum_tag = Some(ident.to_string());
            toks.eat(CLexemes::Identifier);

            let next_tok = toks.peek();
            match next_tok {
                Some((CLexemes::LBrace, _, _)) => {
                    toks.eat(CLexemes::LBrace);
                }
                Some(_) => {
                    /* RESOLVE LOGIC
                    // incomplete enum, need to do lookup (6.7.2.3 3)
                    // GCC / Clang both support extension which allows enum to be incomplete type
                    // (only complains if using -Wpedantic)
                    let lookup = state.symbol_table.lookup_tag_type_idx(state.current_scope, enum_tag.as_ref().unwrap());
                    match lookup {
                        Some(tag_type) => {
                            if let CanonicalType::EnumerationType { .. } = state.symbol_table.get_type(tag_type) {
                                let end = toks.get_location();
                                #[cfg(debug_assertions)]
                                state.parse_tree_stack.push_back(DebugParseNode::EnumSpecifier(start, end));
                                return Ok(tag_type);
                            }
                            else {
                                // other type is not an enum, so lookup fails
                                return Err(ParseError::EnumDeclarationMustMatch);
                            }
                        },
                        // incomplete enum not allowed
                        None => return Err(ParseError::LookupError(enum_tag.unwrap()))
                    }
                    */
                }
                None => return Err(ParseError::UnexpectedEOF),
            };
        }
        Some((other, _, _)) => return Err(ParseError::UnexpectedToken(other)),
        None => return Err(ParseError::UnexpectedEOF),
    }

    /* RESOLVE LOGIC
    // 6.7.2.3 1, not allowed to redeclare enum (or struct or union) contents
    if let Some(enum_tag) = &enum_tag {
        let direct_lookup = state.symbol_table.direct_lookup_tag_type(state.current_scope, &enum_tag);
        // shares namespace with struct / unions, so a struct / union named the same thing is also not ok
        if direct_lookup.is_some() {
            return Err(ParseError::RedeclaredEnum(enum_tag.to_string()))
        }
    }
    */

    // footnote 109 - enum constants share namespace with eachother and with "ordinary" identifiers
    // so, just put them into symbol table with everything else
    let mut counter: i32 = 0;
    let mut enum_members: Vec<(String, i32)> = Vec::new();
    loop {
        // 1. match identifier
        let enum_constant_name: String;
        match toks.peek() {
            Some((CLexemes::Identifier, name, _)) => {
                enum_constant_name = name.to_string();
                toks.eat(CLexemes::Identifier);
            }
            Some((CLexemes::RBrace, _, _)) => {
                return Err(ParseError::EmptyEnum);
            }
            Some((other, _, _)) => {
                return Err(ParseError::UnexpectedToken(other));
            }
            None => {
                return Err(ParseError::UnexpectedEOF);
            }
        }

        // 2. comma -> go next, brace -> break, other -> bad token, equal -> expect int
        match toks.peek() {
            Some((CLexemes::Comma, _, _)) => {
                enum_members.push((enum_constant_name.clone(), counter));

                // ensure enum constants don't conflict with existing symbols
                /* RESOLVE LOGIC
                let enum_constant_value = Symbol::Constant(Constant::Int(counter));
                match state.symbol_table.add_symbol(state.current_scope, enum_constant_name, enum_constant_value) {
                    Err(e) => { return Err(ParseError::RedeclaredEnumConstant(e)); }
                    Ok(_) => {}
                }
                 */

                toks.eat(CLexemes::Comma);
                counter += 1;
                if let Some((CLexemes::RBrace, _, _)) = toks.peek() {
                    toks.eat(CLexemes::RBrace);
                    break;
                } else {
                    continue;
                }
            }
            Some((CLexemes::Eq, _, _)) => {
                toks.eat(CLexemes::Eq);
                // fall through
            }
            Some((CLexemes::RBrace, _, _)) => {
                toks.eat(CLexemes::RBrace);
                break;
            }
            Some((other, _, _)) => {
                return Err(ParseError::UnexpectedToken(other));
            }
            None => {
                return Err(ParseError::UnexpectedEOF);
            }
        }

        match toks.peek() {
            Some((CLexemes::IntegerConst, int_str, _)) => {
                // TODO: technically any integer constant expression is ok, but just ignore this for now
                // until can parse constant expressions more generally.
                let enum_value = int_str
                    .parse::<i32>()
                    .map_err(|_| ParseError::InvalidEnumConstant(int_str.to_string()))?;

                enum_members.push((enum_constant_name.clone(), enum_value));

                /* RESOLVE LOGIC
                let enum_constant_value = Symbol::Constant(Constant::Int(enum_value));
                match state.symbol_table.add_symbol(state.current_scope, enum_constant_name, enum_constant_value) {
                    Err(e) => { return Err(ParseError::RedeclaredEnumConstant(e)); }
                    Ok(_) => {}
                }
                */

                counter = enum_value + 1;
                toks.eat(CLexemes::IntegerConst);
            }
            Some((_, text, _)) => {
                return Err(ParseError::InvalidEnumConstant(text.to_string()));
            }
            None => {
                return Err(ParseError::UnexpectedEOF);
            }
        }

        match toks.peek() {
            Some((CLexemes::Comma, _, _)) => {
                toks.eat(CLexemes::Comma);
                if let Some((CLexemes::RBrace, _, _)) = toks.peek() {
                    toks.eat(CLexemes::RBrace);
                    break;
                } else {
                    continue;
                }
            }
            Some((CLexemes::RBrace, _, _)) => {
                toks.eat(CLexemes::RBrace);
                break;
            }
            Some((other, _, _)) => {
                return Err(ParseError::UnexpectedToken(other));
            }
            None => {
                return Err(ParseError::UnexpectedEOF);
            }
        }
    }

    let enum_type = CanonicalType::EnumerationType {
        tag: enum_tag.clone(),
        members: enum_members,
    };

    let enum_type_idx = state.add_type(enum_type);

    /* RESOLVE LOGIC
    if enum_tag.is_some() {
        state.symbol_table.add_tag(state.current_scope, enum_tag.unwrap(), enum_type_idx)?;
    }
    */

    let end = toks.get_location();
    #[cfg(debug_assertions)]
    state
        .parse_tree_stack
        .push_back(DebugParseNode::EnumSpecifier(start, end));
    Ok(enum_type_idx)
}

fn parse_init_declarators(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
    declaration_specifiers: DeclarationSpecifiers,
) -> Result<ASTNode, ParseError> {
    let mut declarations = Vec::new();
    let declaration = parse_init_declarator(toks, state, declaration_specifiers.clone())?;
    declarations.push(declaration);
    loop {
        match toks.peek() {
            Some((CLexemes::Comma, _, _)) => {
                toks.eat(CLexemes::Comma);
                let declaration =
                    parse_init_declarator(toks, state, declaration_specifiers.clone())?;
                declarations.push(declaration);
            }
            Some((CLexemes::Semicolon, _, _)) => {
                let decl_node = ASTNode::Declaration(declarations);
                return Ok(decl_node);
            }
            Some((other, _, _)) => {
                return Err(ParseError::UnexpectedToken(other));
            }
            None => {
                return Err(ParseError::UnexpectedEOF);
            }
        }
    }
}

fn parse_init_declarator(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
    declaration_specifiers: DeclarationSpecifiers,
) -> Result<Declaration, ParseError> {
    let DeclarationSpecifiers {
        qualified_type,
        storage_class,
        function_specifier,
    } = declaration_specifiers;
    let base_type = qualified_type;
    let (qualified_type, name) = parse_declarator(toks, state, base_type)?;
    let initializer = match toks.peek() {
        Some((CLexemes::Eq, _, _)) => {
            toks.eat(CLexemes::Eq);
            let initializer = parse_initializer(toks, state)?;
            Some(Box::new(initializer))
        }
        Some((_, _, _)) => None,
        None => return Err(ParseError::UnexpectedEOF),
    };

    let declaration = Declaration::new(
        Identifier::new(state.current_scope, name),
        qualified_type,
        storage_class,
        function_specifier,
        initializer,
    );

    Ok(declaration)
}

fn parse_initializer(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<ExpressionNode, ParseError> {
    match toks.peek() {
        Some((CLexemes::LBrace, _, _)) => {
            todo!("compound initializers not implemented yet")
        }
        Some((_, _, _)) => parse_assignment_expr(toks, state),
        None => Err(ParseError::UnexpectedEOF),
    }
}

// The concept of a declarator basically only exists during parsing and isn't semantically meaningful elsewhere
// "abstract" declarator has no identifier
struct Declarator(QualifiedType, Option<String>);
fn parse_declarator_base(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
    base_type: QualifiedType,
) -> Result<Declarator, ParseError> {
    let mut pointers: Vec<(TypeQualifier, usize)> = Vec::new();
    #[derive(Debug)]
    enum ArrayOrFunctionDeclarator {
        ArrayDeclarator(ArrayDeclarator),
        FunctionDeclarator(FunctionDeclarator),
    }
    let mut array_or_function_declarators: Vec<(ArrayOrFunctionDeclarator, usize)> = Vec::new();
    let mut identifier: Option<String> = None;
    let mut level: usize = 0;
    let mut max_level: usize = 0;
    loop {
        match toks.peek() {
            Some((CLexemes::Star, _, _)) => {
                let pointer = parse_pointer_declarator(toks, state)?;
                pointers.push((pointer.0, level))
            }
            Some((CLexemes::LParen, _, _)) => {
                // problem: in abstract declarator, identifer is omitted, so this might be grouping paren
                // or a function. need lookahead to determine which
                match toks.peekn(1) {
                    Some((CLexemes::Star, _, _))
                    | Some((CLexemes::LParen, _, _))
                    | Some((CLexemes::LBracket, _, _)) => {
                        // "declarator part", could not appear in a parameter type list
                        // since parameter type requires a type specifier, which is disjoint from these lexemes
                        toks.eat(CLexemes::LParen);
                        level += 1;
                        max_level += 1;
                    }
                    Some((CLexemes::RParen, _, _)) => {
                        // must be void function, so fall through without consuming LParen
                        break;
                    }
                    Some((_, _, _)) => {
                        // must be function declarator by earlier reasoning, fall through
                        break;
                    }
                    None => {
                        return Err(ParseError::UnmatchedParensInDeclarator);
                    }
                }
            }
            Some((CLexemes::Identifier, ident, _)) => {
                identifier = Some(ident.to_string());
                toks.eat(CLexemes::Identifier);
                break;
            }
            Some((_, _, _)) | None => {
                break;
            }
        }
    }

    loop {
        match toks.peek() {
            Some((CLexemes::LBracket, _, _)) => {
                let array = parse_array_declarator(toks, state)?;
                array_or_function_declarators
                    .push((ArrayOrFunctionDeclarator::ArrayDeclarator(array), level));
            }
            Some((CLexemes::LParen, _, _)) => {
                let function = parse_function_declarator(toks, state)?;
                array_or_function_declarators.push((
                    ArrayOrFunctionDeclarator::FunctionDeclarator(function),
                    level,
                ));
            }
            Some((CLexemes::RParen, _, _)) => {
                // e.g. void f(int a, int b)
                // don't want declarator of b to be confused w/ right paren of f's function declaration
                if level == 0 {
                    break;
                }
                toks.eat(CLexemes::RParen);
                level -= 1;
            }
            Some((_, _, _)) | None => {
                break;
            }
        }
    }

    if level != 0 {
        return Err(ParseError::UnmatchedParensInDeclarator);
    }

    // according to grammar, [] and () bind more tightly than *
    // array of array: ok
    // array of function: not ok by 6.7.5.2 1 ("The element type shall not be an incomplete or function type.")
    // function which return function / function which return array: not ok by 6.7.5.3 1
    // furthermore, incomplete return type or parameter types is ok for function prototypes
    // e.g. struct unknown function(struct unknown2);
    // is fine; presumably this is so code which only uses its address is ok (?), but any attempt to call it is obviously
    // not going to work until those incomplete types are filled in
    let mut pointer_index: usize = 0;
    let mut current_type = base_type;
    for level in 0..=max_level {
        while pointer_index < pointers.len() && pointers[pointer_index].1 == level {
            current_type = QualifiedType {
                base_type: CType::PointerType {
                    pointee_type: Box::new(current_type),
                },
                qualifier: pointers[pointer_index].0,
            };

            pointer_index += 1;
        }
        while let Some((_, l)) = array_or_function_declarators.last() {
            if *l == level {
                let (d, _) = array_or_function_declarators.pop().unwrap();
                match d {
                    ArrayOrFunctionDeclarator::ArrayDeclarator(array) => {
                        match current_type.base_type {
                            /* RESOLVE LOGIC
                            CType::IncompleteArrayType { .. }
                            | CType::FunctionType { .. }
                            | CType::Void => {
                                return Err(ParseError::ArrayOfIncompleteType);
                            },
                            // have to do lookup to check if its incomplete
                            CType::StructureTypeRef { symtab_idx }
                            | CType::UnionTypeRef { symtab_idx } => {

                                match state.symbol_table.get_type(symtab_idx) {
                                    CanonicalType::IncompleteUnionType { .. }
                                    | CanonicalType::IncompleteStructureType { .. } => {
                                        return Err(ParseError::ArrayOfIncompleteType);
                                    }
                                    _ => {}
                                }

                            },
                            */
                            _ => {}
                        }

                        let new_type: CType;
                        if let Some(size) = array.1 {
                            new_type = CType::ArrayType {
                                size,
                                element_type: Box::new(current_type),
                            };
                        } else {
                            new_type = CType::IncompleteArrayType {
                                element_type: Box::new(current_type),
                            };
                        }

                        current_type = QualifiedType {
                            base_type: new_type,
                            qualifier: array.0,
                        }
                    }
                    ArrayOrFunctionDeclarator::FunctionDeclarator(func) => {
                        /* RESOLVE LOGIC
                        match current_type.base_type {
                            CType::IncompleteArrayType { .. }
                            | CType::ArrayType { .. }
                            | CType::FunctionType { .. } => {
                                return Err(ParseError::BadFunctionReturnType);
                            }
                            _ => {}
                        }
                        */

                        let fn_type = CanonicalType::FunctionType {
                            parameter_types: func.argument_types,
                            return_type: Box::new(current_type),
                            function_specifier: FunctionSpecifier::None, // TODO: figure out inline
                            varargs: func.varargs,
                            prototype_scope: func.prototype_scope,
                        };

                        let fn_type_idx = state.add_type(fn_type);
                        let new_type = CType::FunctionTypeRef {
                            symtab_idx: fn_type_idx,
                        };

                        current_type = QualifiedType {
                            base_type: new_type,
                            qualifier: TypeQualifier::empty(),
                        }
                    }
                };
            }
            break;
        }
    }

    Ok(Declarator(current_type, identifier))
}

fn parse_declarator(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
    base_type: QualifiedType,
) -> Result<(QualifiedType, String), ParseError> {
    let decl = parse_declarator_base(toks, state, base_type)?;
    if let Some(ident) = decl.1 {
        Ok((decl.0, ident))
    } else {
        Err(ParseError::DeclaratorRequiresName)
    }
}

fn parse_abstract_declarator(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
    base_type: QualifiedType,
) -> Result<QualifiedType, ParseError> {
    let decl = parse_declarator_base(toks, state, base_type)?;
    if let None = decl.1 {
        Ok(decl.0)
    } else {
        Err(ParseError::UnexpectedDeclaratorName)
    }
}

#[derive(Debug)]
struct PointerDeclarator(TypeQualifier);
fn parse_pointer_declarator(
    toks: &mut impl TokenStream<CLexemes>,
    _state: &mut ParserState,
) -> Result<PointerDeclarator, ParseError> {
    let mut qualifier = TypeQualifier::empty();
    eat_or_error!(toks, CLexemes::Star)?;
    loop {
        match toks.peek() {
            Some((CLexemes::Const, _, _)) => {
                qualifier |= TypeQualifier::Const;
                toks.eat(CLexemes::Const);
            }
            Some((CLexemes::Volatile, _, _)) => {
                qualifier |= TypeQualifier::Volatile;
                toks.eat(CLexemes::Volatile);
            }
            Some((CLexemes::Restrict, _, _)) => {
                qualifier |= TypeQualifier::Restrict;
                toks.eat(CLexemes::Restrict);
            }
            Some((_, _, _)) => {
                return Ok(PointerDeclarator(qualifier));
            }
            None => {
                return Err(ParseError::UnexpectedEOF);
            }
        }
    }
}
#[derive(Debug, Clone, Copy)]
struct ArrayDeclarator(TypeQualifier, Option<usize>);
fn parse_array_declarator(
    toks: &mut impl TokenStream<CLexemes>,
    _state: &mut ParserState,
) -> Result<ArrayDeclarator, ParseError> {
    let mut qualifier = TypeQualifier::empty();
    eat_or_error!(toks, CLexemes::LBracket)?;
    loop {
        match toks.peek() {
            Some((CLexemes::Restrict, _, _)) => {
                qualifier |= TypeQualifier::Restrict;
            }
            Some((CLexemes::Volatile, _, _)) => {
                qualifier |= TypeQualifier::Volatile;
            }
            Some((CLexemes::Const, _, _)) => {
                qualifier |= TypeQualifier::Const;
            }
            Some((CLexemes::RBracket, _, _)) => {
                toks.eat(CLexemes::RBracket);
                return Ok(ArrayDeclarator(qualifier, None));
            }
            Some((CLexemes::IntegerConst, i, _)) => {
                let size = i
                    .parse::<usize>()
                    .map_err(|e| ParseError::BadArrayBound(e))?;

                eat_or_error!(toks, CLexemes::RBracket)?;
                return Ok(ArrayDeclarator(qualifier, Some(size)));
            }
            Some((_, _, _)) => {
                // TODO: support integer constant expressions here
                todo!();
                // return Err(ParseError::UnexpectedToken(other));
            }
            None => {
                return Err(ParseError::UnexpectedEOF);
            }
        }
    }
}

// only time scope is needed is if this is actually a function definition, and not
// just a prototype
#[derive(Debug, Clone)]
struct FunctionDeclarator {
    argument_types: Vec<FunctionArgument>,
    varargs: bool,
    prototype_scope: Scope,
}

// will not support old-style function declarations because they are cringe
// this code is used for both function prototypes and function definitions
fn parse_function_declarator(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<FunctionDeclarator, ParseError> {
    eat_or_error!(toks, CLexemes::LParen)?;
    state.open_scope(ScopeType::FunctionScope);

    // ignore special case for empty parameter list
    // technically, this has special behavior (completed by later declarations / usages),
    // a sort of "incomplete" function type. not used very much, won't support
    let mut parameter_types: Vec<FunctionArgument> = Vec::new();
    let mut varargs = false;
    loop {
        match toks.peek() {
            Some((CLexemes::RParen, _, _)) => {
                toks.eat(CLexemes::RParen);
                /* RESOLVE LOGIC
                // 6.7.5.3 4, 7, 8
                // arrays / functions decay to pointers, check that types are not incomplete
                // special case for 1 parameter of type void
                if parameter_types.len() == 1 {
                    let QualifiedType { base_type, qualifier } = &parameter_types[0];
                    if let CType::Void = base_type {
                        if qualifier.is_empty() {
                            parameter_types.remove(0);
                        }
                    }
                }

                for arg_type in &mut parameter_types {
                    let QualifiedType { base_type, qualifier } = arg_type;
                    match base_type {
                        // void not allowed unless special case above
                        CType::Void => {
                            return Err(ParseError::IncompleteFunctionArgument);
                        },

                        // decay to pointer
                        CType::IncompleteArrayType { element_type }
                        | CType::ArrayType { element_type, .. } =>  {
                            // is there a more elegant way to do this?
                            let element_type = std::mem::replace(element_type, Box::new(QualifiedType {
                                base_type: CType::Void,
                                qualifier: TypeQualifier::empty(),
                            }));
                            *arg_type = QualifiedType {
                                base_type: CType::PointerType { pointee_type: element_type },
                                qualifier: *qualifier
                            };
                        },
                        CType::FunctionTypeRef { .. } => {
                            let function_type = std::mem::replace(arg_type, QualifiedType {
                                base_type: CType::Void, qualifier: TypeQualifier::empty() }
                            );

                            *arg_type = QualifiedType {
                                base_type: CType::PointerType {
                                    pointee_type: Box::new(function_type)
                                },
                                qualifier: TypeQualifier::empty(),
                            };
                        },

                        _ => {}
                    }
                }
                */

                let declarator = FunctionDeclarator {
                    argument_types: parameter_types,
                    varargs,
                    prototype_scope: state.current_scope,
                };
                state.close_scope()?;
                return Ok(declarator);
            }
            Some((CLexemes::Ellipsis, _, _)) => {
                toks.eat(CLexemes::Ellipsis);
                varargs = true;
                eat_or_error!(toks, CLexemes::RParen)?;
                let declarator = FunctionDeclarator {
                    argument_types: parameter_types,
                    varargs,
                    prototype_scope: state.current_scope,
                };
                state.close_scope()?;
                return Ok(declarator);
            }
            Some((_, _, _)) => {
                let parameter_base_type =
                    parse_declaration_specifiers_base(toks, state, false, false)?;

                match toks.peek() {
                    Some((CLexemes::Comma, _, _)) => {
                        parameter_types.push((None, parameter_base_type.qualified_type));
                        toks.eat(CLexemes::Comma);
                    }
                    Some((CLexemes::RParen, _, _)) => {
                        parameter_types.push((None, parameter_base_type.qualified_type));
                        // fall through after loop to close function arm
                    }
                    Some((_, _, _)) => {
                        let DeclarationSpecifiers { qualified_type, .. } = parameter_base_type;
                        let parameter_base_type = qualified_type;
                        let Declarator(parameter_type, parameter_name) =
                            parse_declarator_base(toks, state, parameter_base_type)?;
                        if let Some(ref parameter_name) = parameter_name {
                            /* RESOLVE LOGIC
                            let parameter = Symbol::Variable {
                                symbol_type: parameter_type.0.clone(),
                                storage_class,
                                linkage: Linkage::None
                            };
                            state.symbol_table.add_symbol(state.current_scope, parameter_name, parameter)?;
                            */
                        }

                        parameter_types.push((parameter_name, parameter_type));

                        match toks.peek() {
                            Some((CLexemes::Comma, _, _)) => {
                                toks.eat(CLexemes::Comma);
                                continue;
                            }
                            _ => {
                                continue;
                            }
                        }
                    }
                    None => {
                        return Err(ParseError::UnexpectedEOF);
                    }
                }
            }
            None => {
                return Err(ParseError::UnexpectedEOF);
            }
        }
    }
}

fn parse_statement(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<ASTNode, ParseError> {
    // only 1-2 token lookahead required to check for what type of statement it is
    if is_lookahead_label(toks, state) {
        return parse_labeled_statement(toks, state);
    }

    match toks.peek() {
        Some((CLexemes::LBrace, _, _)) => parse_compound_statement(toks, state),
        Some((CLexemes::If, _, _)) | Some((CLexemes::Switch, _, _)) => {
            parse_selection_statement(toks, state)
        }
        Some((CLexemes::While, _, _))
        | Some((CLexemes::Do, _, _))
        | Some((CLexemes::For, _, _)) => parse_iteration_statement(toks, state),
        Some((CLexemes::Goto, _, _))
        | Some((CLexemes::Continue, _, _))
        | Some((CLexemes::Break, _, _))
        | Some((CLexemes::Return, _, _)) => parse_jump_statement(toks, state),
        Some((_, _, _)) => parse_expression_statement(toks, state),
        None => Err(ParseError::UnexpectedEOF),
    }
}

fn is_lookahead_label(toks: &mut impl TokenStream<CLexemes>, _state: &mut ParserState) -> bool {
    match toks.peek() {
        Some((lexeme, _, _)) => {
            match lexeme {
                CLexemes::Default | CLexemes::Case => true,
                CLexemes::Identifier => {
                    // only other place colon appears in grammar is for ternaries
                    // which should get completely parsed by parse_expr
                    if let Some((CLexemes::Colon, _, _)) = toks.peekn(1) {
                        true
                    } else {
                        false
                    }
                }
                _ => false,
            }
        }
        None => false,
    }
}

fn parse_labeled_statement(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<ASTNode, ParseError> {
    match toks.peek() {
        Some((CLexemes::Identifier, label, _)) => {
            let label = label.to_string();
            toks.eat(CLexemes::Identifier);
            eat_or_error!(toks, CLexemes::Colon)?;

            // C99 does not allow labeled statement at end of compound statement
            // but gcc and clang will (only complaining on -Wpedantic)
            // follow standard for now
            let labelee = parse_statement(toks, state)?;
            let labelee = Rc::new(labelee);
            /* RESOLVE LOGIC
            // let res = state.symbol_table.add_label(
            //     state.current_scope,
            //     label.clone(),
            //     Rc::clone(&labelee)
            // );

            // match res {
            //     Ok(()) => {}
            //     Err(e @ SymtabError::LabelAlreadyDeclared(_)) => return Err(ParseError::RedeclaredLabel(e)),
            //     Err(e) => return Err(ParseError::OtherSymtabError(e))
            // }
            */

            let label_ident = Identifier::new(state.current_scope, label);
            let label_node = ASTNode::Label(labelee, label_ident);

            Ok(label_node)
        }
        Some((CLexemes::Case, _, _)) => {
            // again, clang / gcc allow empty case
            // but we won't
            toks.eat(CLexemes::Case);
            let case = parse_expr(toks, state)?;
            eat_or_error!(toks, CLexemes::Colon)?;
            let case_body = parse_statement(toks, state)?;

            let case_node = ASTNode::CaseLabel(Box::new(case_body), Box::new(case));

            Ok(case_node)
        }
        Some((CLexemes::Default, _, _)) => {
            toks.eat(CLexemes::Default);
            eat_or_error!(toks, CLexemes::Colon)?;
            let default_body = parse_statement(toks, state)?;
            let default_node = ASTNode::DefaultLabel(Box::new(default_body));

            Ok(default_node)
        }
        Some((other, _, _)) => Err(ParseError::UnexpectedToken(other)),
        None => Err(ParseError::UnexpectedEOF),
    }
}

fn parse_compound_statement(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<ASTNode, ParseError> {
    eat_or_error!(toks, CLexemes::LBrace)?;
    // open a new block scope
    state.open_scope(ScopeType::BlockScope);
    let mut items = Vec::new();
    loop {
        let ast_node = if is_lookahead_declaration(toks, state) {
            // upcoming tokens must be declaration
            parse_declaration(toks, state)?
        } else {
            // upcoming tokens must be statement
            parse_statement(toks, state)?
        };
        items.push(ast_node);
        match toks.peek() {
            Some((CLexemes::RBrace, _, _)) => {
                break;
            }
            Some((_, _, _)) => {
                continue;
            }
            None => {
                return Err(ParseError::UnexpectedEOF);
            }
        }
    }
    state.close_scope()?;
    eat_or_error!(toks, CLexemes::RBrace)?;

    let compound_node = ASTNode::CompoundStatement(items, state.current_scope);

    Ok(compound_node)
}

fn parse_expression_statement(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<ASTNode, ParseError> {
    match toks.peek() {
        Some((CLexemes::Semicolon, _, _)) => {
            toks.eat(CLexemes::Semicolon);
            Ok(ASTNode::NullStatement)
        }
        Some((_, _, _)) => {
            let expr = parse_expr(toks, state)?;
            eat_or_error!(toks, CLexemes::Semicolon)?;
            Ok(ASTNode::ExpressionStatement(
                Box::new(expr),
                state.current_scope,
            ))
        }
        None => Err(ParseError::UnexpectedEOF),
    }
}

fn parse_selection_statement(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<ASTNode, ParseError> {
    match toks.peek() {
        Some((CLexemes::If, _, _)) => {
            toks.eat(CLexemes::If);
            eat_or_error!(toks, CLexemes::LParen)?;
            let condition = parse_expr(toks, state)?;
            eat_or_error!(toks, CLexemes::RParen)?;
            let body = parse_statement(toks, state)?;
            // Dangling else attaches to lexically *closest* if statement
            let if_node = match toks.peek() {
                Some((CLexemes::Else, _, _)) => {
                    toks.eat(CLexemes::Else);
                    let else_body = parse_statement(toks, state)?;
                    ASTNode::IfStatement(
                        Box::new(condition),
                        Box::new(body),
                        Some(Box::new(else_body)),
                        state.current_scope,
                    )
                }
                Some((_, _, _)) => ASTNode::IfStatement(
                    Box::new(condition),
                    Box::new(body),
                    None,
                    state.current_scope,
                ),
                None => {
                    return Err(ParseError::UnexpectedEOF);
                }
            };

            Ok(if_node)
        }
        Some((CLexemes::Switch, _, _)) => {
            toks.eat(CLexemes::Switch);
            eat_or_error!(toks, CLexemes::LParen)?;
            let switch_expr = parse_expr(toks, state)?;
            eat_or_error!(toks, CLexemes::RParen)?;
            let body = parse_statement(toks, state)?;
            let switch_node = ASTNode::SwitchStatement(
                Box::new(switch_expr),
                Box::new(body),
                state.current_scope,
            );

            Ok(switch_node)
        }
        Some((other, _, _)) => {
            return Err(ParseError::UnexpectedToken(other));
        }
        None => {
            return Err(ParseError::UnexpectedEOF);
        }
    }
}

fn parse_iteration_statement(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<ASTNode, ParseError> {
    match toks.peek() {
        Some((CLexemes::While, _, _)) => {
            toks.eat(CLexemes::While);
            eat_or_error!(toks, CLexemes::LParen)?;
            let expr = parse_expr(toks, state)?;
            eat_or_error!(toks, CLexemes::RParen)?;
            let statement = parse_statement(toks, state)?;
            let while_statement =
                ASTNode::WhileStatement(Box::new(expr), Box::new(statement), state.current_scope);
            Ok(while_statement)
        }
        Some((CLexemes::Do, _, _)) => {
            toks.eat(CLexemes::Do);
            let statement = parse_statement(toks, state)?;
            eat_or_error!(toks, CLexemes::While)?;
            eat_or_error!(toks, CLexemes::LParen)?;
            let expr = parse_expr(toks, state)?;
            eat_or_error!(toks, CLexemes::RParen)?;
            eat_or_error!(toks, CLexemes::Semicolon)?;
            let do_while_statement =
                ASTNode::DoWhileStatement(Box::new(expr), Box::new(statement), state.current_scope);
            Ok(do_while_statement)
        }
        Some((CLexemes::For, _, _)) => {
            // for loop requires some care; clause 1 being possibly a declaration
            // e.g. for(int i = 0; i < 5; i += 1)
            // or just an expression
            toks.eat(CLexemes::For);
            eat_or_error!(toks, CLexemes::LParen)?;
            let first_clause = if is_lookahead_declaration(toks, state) {
                state.open_scope(ScopeType::BlockScope);
                let declaration = parse_declaration(toks, state)?;
                state.close_scope()?;
                Some(declaration)
            } else {
                match toks.peek() {
                    Some((CLexemes::Semicolon, _, _)) => None,
                    Some((_, _, _)) => {
                        let expr = parse_expr(toks, state)?;
                        Some(ASTNode::ExpressionStatement(
                            Box::new(expr),
                            state.current_scope,
                        ))
                    }
                    None => return Err(ParseError::UnexpectedEOF),
                }
            };
            eat_or_error!(toks, CLexemes::Semicolon)?;
            let second_clause = match toks.peek() {
                Some((CLexemes::Semicolon, _, _)) => None,
                Some((_, _, _)) => Some(parse_expr(toks, state)?),
                None => return Err(ParseError::UnexpectedEOF),
            };
            eat_or_error!(toks, CLexemes::Semicolon)?;
            let third_clause = match toks.peek() {
                Some((CLexemes::Semicolon, _, _)) => None,
                Some((_, _, _)) => Some(parse_expr(toks, state)?),
                None => return Err(ParseError::UnexpectedEOF),
            };
            eat_or_error!(toks, CLexemes::RParen)?;

            let loop_body = parse_statement(toks, state)?;
            let for_node = ASTNode::ForStatement(
                first_clause.map(Box::new),
                second_clause.map(Box::new),
                third_clause.map(Box::new),
                Box::new(loop_body),
                state.current_scope,
            );

            Ok(for_node)
        }
        Some((other, _, _)) => Err(ParseError::UnexpectedToken(other)),
        None => Err(ParseError::UnexpectedEOF),
    }
}

fn parse_jump_statement(
    toks: &mut impl TokenStream<CLexemes>,
    state: &mut ParserState,
) -> Result<ASTNode, ParseError> {
    match toks.peek() {
        Some((CLexemes::Goto, _, _)) => {
            toks.eat(CLexemes::Goto);
            match toks.peek() {
                Some((CLexemes::Identifier, ident, _)) => {
                    let ident = Identifier::new(state.current_scope, ident.to_string());
                    eat_or_error!(toks, CLexemes::Semicolon)?;
                    Ok(ASTNode::GotoStatement(ident))
                }
                Some((other, _, _)) => Err(ParseError::UnexpectedToken(other)),
                None => Err(ParseError::UnexpectedEOF),
            }
        }
        Some((CLexemes::Continue, _, _)) => {
            toks.eat(CLexemes::Continue);
            eat_or_error!(toks, CLexemes::Semicolon)?;
            Ok(ASTNode::ContinueStatement)
        }
        Some((CLexemes::Break, _, _)) => {
            toks.eat(CLexemes::Break);
            eat_or_error!(toks, CLexemes::Semicolon)?;
            Ok(ASTNode::BreakStatement)
        }
        Some((CLexemes::Return, _, _)) => {
            toks.eat(CLexemes::Return);
            match toks.peek() {
                Some((CLexemes::Semicolon, _, _)) => {
                    toks.eat(CLexemes::Semicolon);
                    Ok(ASTNode::ReturnStatement(None))
                }
                Some((_, _, _)) => {
                    let expr = parse_expr(toks, state)?;
                    eat_or_error!(toks, CLexemes::Semicolon)?;
                    Ok(ASTNode::ReturnStatement(Some(Box::new(expr))))
                }
                None => Err(ParseError::UnexpectedEOF),
            }
        }
        Some((other, _, _)) => Err(ParseError::UnexpectedToken(other)),
        None => Err(ParseError::UnexpectedEOF),
    }
}

#[cfg(test)]
mod hand_parser_tests;
