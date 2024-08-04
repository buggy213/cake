use core::panic;
use std::{collections::VecDeque, env::var, num::ParseIntError};

use thiserror::Error;

use crate::{parser::ast::Constant, scanner::{lexeme_sets::c_lexemes::CLexemes, TokenStream}, semantics::{symtab::{Linkage, Scope, ScopeType, StorageClass, Symbol, SymbolTable, SymtabError, TypeIdx}, types::{AggregateMember, BasicType, CType, CanonicalType, FunctionSpecifier, QualifiedType, TypeQualifier}}};

use super::ast::{ASTNode, ExpressionNode, Identifier};

type CTokenStream<'a> = crate::scanner::RawTokenStream<'a, CLexemes>;

// can explicitly materialize parse tree for debugging purposes, though not required
#[cfg(debug_assertions)]
enum ParseNode {
    EnumSpecifier(usize, usize),
    StructSpecifier(usize, usize),
    UnionSpecifier(usize, usize)
}

#[cfg(debug_assertions)]
// start / end are describe position of the text corresponding to this parse node,
// variant is the enum variant for this parse node
// non-leaf parse nodes must be tuple structs (Box<ParseNode>, usize, usize) or (Vec<ParseNode>, usize, usize)
// leaf parse nodes must be tuple structs of (usize, usize)
macro_rules! materialize_parse_node {
    // child count must be tt for it to be matched correctly by sub-macro
    ($start:expr, $end:expr, $variant:expr, $child_count:tt, $node_vec:expr) => {
        {
            // generates either a Box or Vec
            let parse_node_children = materialize_parse_node_children![$child_count, $node_vec];
            let parse_node = $variant(parse_node_children, $start, $end);
            $node_vec.push_back(parse_node);
        }
    };

    ($start:expr, $end:expr, $variant:expr, $node_vec:expr) => {
        {
            // generates either a Box or Vec
            let parse_node = $variant($start, $end);
            $node_vec.push_back(parse_node);
        }
    };
}

#[cfg(debug_assertions)]
macro_rules! materialize_parse_node_children {
    (1, $node_vec:expr) => {
        Box::new($node_vec.pop_back().expect("Expected at least one element in parse stack"))
    };

    ($n:literal, $node_vec:expr) => {
        {
            let mut children = Vec::with_capacity($n);
            for _ in 0..$n {
                children.push($node_vec.pop_back().expect("Expected at least $n elements in the vector"));
            }
            children.reverse(); // Reverse to maintain the original order
            children
        }
    }
}

macro_rules! eat_or_error {
    ($toks:expr, $tok:path) => {
        match $toks.peek() {
            Some(($tok, _, _)) => { 
                $toks.eat($tok);
                Ok(())
            }
            Some((other, _, _)) => { Err(ParseError::UnexpectedToken(other)) }
            None => { Err(ParseError::UnexpectedEOF) }
        }   
    };
}

#[derive(Error, Debug)]
enum ParseError {
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
    #[error("member access (with '.' or '->' operators) expects identifier, not arbitrary expression")]
    BadMemberAccess,
}

struct ParserState {
    symbol_table: SymbolTable,
    current_scope: Scope,
    #[cfg(debug_assertions)]
    parse_tree_stack: VecDeque<ParseNode>
}

impl ParserState {
    fn open_scope(&mut self, scope_type: ScopeType) {
        self.current_scope = self.symbol_table.new_scope(Some(self.current_scope), scope_type);
    }

    fn close_scope(&mut self) -> Result<(), ParseError> {
        self.current_scope = self.symbol_table.get_parent_scope(self.current_scope).ok_or(ParseError::ClosedFileScope)?;
        Ok(())
    }
}

// TODO: error recovery
#[derive(Debug, Clone)]
enum Atom {
    Identifier(Identifier),
    Constant(Constant),
    StringLiteral(String)
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

#[derive(Debug, Clone, Copy)]
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
    Colon

}

enum ExprPart {
    Atom(Atom),
    Operator(Operator)
}

fn parse_integer_const(text: &str) -> Constant {
    todo!()
}

fn parse_float_const(text: &str) -> Constant {
    todo!()
}

fn to_expr_part(lexeme: CLexemes, text: &str, state: &mut ParserState) -> Option<ExprPart> {
    match lexeme {
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
        },

        CLexemes::IntegerConst => {
            let int_const = parse_integer_const(text);
            ExprPart::Atom(Atom::Constant(int_const))
        },

        CLexemes::FloatConst => {
            let float_const = parse_float_const(text);
            ExprPart::Atom(Atom::Constant(float_const))
        },

        CLexemes::StringConst => {
            ExprPart::Atom(Atom::StringLiteral(text.to_string()))
        },

        _ => return None
    };
    todo!()
}

fn prefix_binding_power(op: Operator) -> Option<u32> {
    match op {
        Operator::Increment => Some(15),
        Operator::Decrement => Some(15),
        Operator::Plus => Some(15),
        Operator::Minus => Some(15),
        Operator::Bang => Some(15),
        Operator::Tilde => Some(15),
        Operator::Star => Some(15),
        Operator::BitAnd => Some(15),
        Operator::Sizeof => Some(15),
        _ => None
    }
}

fn postfix_binding_power(op: Operator) -> Option<u32> {
    match op {
        Operator::Increment => todo!(),
        Operator::Decrement => todo!(),
        Operator::LParen => todo!(),
        Operator::LBracket => todo!(),

        _ => return None
    }
}

fn infix_binding_power(op: Operator) -> Option<(u32, u32)> {
    match op {
        Operator::Plus => todo!(),
        Operator::Minus => todo!(),
        Operator::Star => todo!(),
        Operator::BitAnd => todo!(),
        Operator::Slash => todo!(),
        Operator::Percent => todo!(),
        Operator::RShift => todo!(),
        Operator::LShift => todo!(),
        Operator::Lt => todo!(),
        Operator::Gt => todo!(),
        Operator::Leq => todo!(),
        Operator::Geq => todo!(),
        Operator::Eq => todo!(),
        Operator::Neq => todo!(),
        Operator::Xor => todo!(),
        Operator::BitOr => todo!(),
        Operator::And => todo!(),
        Operator::Or => todo!(),

        Operator::Assign => todo!(),
        Operator::MultiplyAssign => todo!(),
        Operator::DivideAssign => todo!(),
        Operator::ModuloAssign => todo!(),
        Operator::AddAssign => todo!(),
        Operator::SubAssign => todo!(),
        Operator::LShiftAssign => todo!(),
        Operator::RShiftAssign => todo!(),
        Operator::AndAssign => todo!(),
        Operator::XorAssign => todo!(),
        Operator::OrAssign => todo!(),
        // grammatically, Dot / Arrow (member access, either directly or through a pointer)
        // are considered "postfix", left-associative operators. however, like array subscript / function calls,
        // they are a special case, since it is required for following token to be an identifier
        // and not any arbitrary expression. something like
        // ```
        // struct example *s;
        // s->(a + b);
        // ```
        // is clearly nonsensical;
        Operator::Dot => todo!(),
        Operator::Arrow => todo!(),

        _ => return None
    }
    
}

// precedence climbing ("Pratt parsing") algorithm with some special case handling
// for C specific syntax. in the first pass, no type checking is done
fn parse_expr(toks: &mut CTokenStream, state: &mut ParserState) -> Result<ExpressionNode, ParseError> {
    let first = parse_assignment_expr(toks, state)?;
    let mut assignment_exprs = Vec::new();
    match toks.peek() {
        Some((CLexemes::Comma, _, _)) => {
            toks.eat(CLexemes::Comma);
            assignment_exprs.push(first);
            let second = parse_assignment_expr(toks, state)?;
            assignment_exprs.push(second);
        },
        Some((_, _, _)) => {
            return Ok(first);
        }
        None => return Err(ParseError::UnexpectedEOF)
    }
    loop {
        match toks.peek() {
            Some((CLexemes::Comma, _, _)) => {
                toks.eat(CLexemes::Comma);
                let next = parse_assignment_expr(toks, state)?;
                assignment_exprs.push(next);
            },
            Some((_, _, _)) => {
                let comma_expr = ExpressionNode::CommaExpr(
                    assignment_exprs, 
                    None
                );
                return Ok(comma_expr);
            }
            None => return Err(ParseError::UnexpectedEOF)
        }
    }
}

fn parse_assignment_expr(toks: &mut CTokenStream, state: &mut ParserState) -> Result<ExpressionNode, ParseError> {
    parse_expr_rec(toks, state, 0)
}

fn parse_expr_rec(toks: &mut CTokenStream, state: &mut ParserState, min_bp: u32) -> Result<ExpressionNode, ParseError> {
    let mut lhs: ExpressionNode;
    match toks.advance() {
        Some((lexeme, text, _)) => {
            match to_expr_part(lexeme, text, state) {
                Some(ExprPart::Atom(atom)) => {
                    lhs = atom.into();
                },
                Some(ExprPart::Operator(Operator::LParen)) => {
                    lhs = parse_expr_rec(toks, state, 0)?;
                    eat_or_error!(toks, CLexemes::LParen)?;
                },
                Some(ExprPart::Operator(op)) => {
                    // it must be a prefix operator
                    if let Some(power) = prefix_binding_power(op) {
                        let rhs = parse_expr_rec(toks, state, power)?;
                        match op {
                            Operator::Increment => {
                                lhs = ExpressionNode::PreIncrement(
                                    Box::new(rhs),
                                    None
                                );
                            },
                            Operator::Decrement => {
                                lhs = ExpressionNode::PreDecrement(
                                    Box::new(rhs), 
                                    None
                                );
                            },
                            Operator::Plus => {
                                lhs = ExpressionNode::UnaryPlus(
                                    Box::new(rhs),
                                    None    
                                );
                            },
                            Operator::Minus => {
                                lhs = ExpressionNode::UnaryMinus(
                                    Box::new(rhs),
                                    None    
                                );
                            },
                            Operator::Bang => {
                                lhs = ExpressionNode::Not(
                                    Box::new(rhs),
                                    None    
                                );
                            },
                            Operator::Tilde => {
                                lhs = ExpressionNode::BitwiseNot(
                                    Box::new(rhs),
                                    None    
                                );
                            },
                            Operator::Star => {
                                lhs = ExpressionNode::Dereference(
                                    Box::new(rhs),
                                    None    
                                );
                            },
                            Operator::BitAnd => {
                                lhs = ExpressionNode::AddressOf(
                                    Box::new(rhs),
                                    None    
                                );
                            },
                            Operator::Sizeof => {
                                // TODO: special lookahead for sizeof type name
                                // (as opposed to sizeof an expression)
                                lhs = ExpressionNode::Sizeof(
                                    Box::new(rhs),
                                    None    
                                );
                            },
                            // to avoid cluttering with additional types,
                            // no separate "PrefixOperator" enum. however,
                            // op cannot be any other value
                            _ => unreachable!()
                        }
                    }
                    else {
                        return Err(ParseError::UnexpectedToken(lexeme));
                    }
                },
                None => return Err(ParseError::UnexpectedToken(lexeme)),
            }
        },
        None => return Err(ParseError::UnexpectedEOF)
    }

    loop {
        match toks.peek() {
            Some((lexeme, text, _)) => {
                match to_expr_part(lexeme, text, state) {
                    Some(ExprPart::Operator(op)) => {
                        if let Some(left_bp) = postfix_binding_power(op) {
                            if left_bp < min_bp {
                                // some higher precedence operator binds to lhs before op does
                                break;
                            }

                            toks.eat(lexeme);

                            match op {
                                Operator::Increment => {
                                    lhs = ExpressionNode::PostIncrement(
                                        Box::new(lhs), 
                                        None
                                    )
                                },
                                Operator::Decrement => {
                                    lhs = ExpressionNode::PostDecrement(
                                        Box::new(lhs), 
                                        None
                                    )
                                },
                            
                                // function call / array subscript
                                Operator::LParen => {
                                    let arguments;
                                    if let Some((CLexemes::RParen, _, _)) = toks.peek() {
                                        arguments = Vec::new();
                                    }
                                    else {
                                        arguments = todo!();
                                    }

                                    eat_or_error!(toks, CLexemes::RParen)?;

                                    lhs = ExpressionNode::FunctionCall(
                                        Box::new(lhs), 
                                        arguments, 
                                        None
                                    );
                                },
                                Operator::LBracket => {
                                    let index = parse_expr_rec(toks, state, 0)?;
                                    eat_or_error!(toks, CLexemes::RBracket)?;
                                    lhs = ExpressionNode::ArraySubscript(
                                        Box::new(lhs), 
                                        Box::new(index),
                                        None 
                                    );
                                },
                                
                                _ => unreachable!()
                            }

                            continue;
                        }
                    
                        if let Some((left_bp, right_bp)) = infix_binding_power(op) {
                            if left_bp < min_bp {
                                break;
                            }

                            toks.eat(lexeme);
                            let rhs = parse_expr_rec(toks, state, right_bp)?;
                            lhs = match op {
                                Operator::Plus => {
                                    ExpressionNode::Add(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::Minus => {
                                    ExpressionNode::Subtract(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::Star => {
                                    ExpressionNode::Multiply(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::BitAnd => {
                                    ExpressionNode::BitwiseAnd(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::Slash => {
                                    ExpressionNode::Divide(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::Percent => {
                                    ExpressionNode::Modulo(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::RShift => {
                                    ExpressionNode::RShift(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::LShift => {
                                    ExpressionNode::LShift(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::Lt => {
                                    ExpressionNode::LessThan(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::Gt => {
                                    ExpressionNode::GreaterThan(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::Leq => {
                                    ExpressionNode::LessThanOrEqual(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::Geq => {
                                    ExpressionNode::GreaterThanOrEqual(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::Eq => {
                                    ExpressionNode::Equal(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::Neq => {
                                    ExpressionNode::NotEqual(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::Xor => {
                                    ExpressionNode::BitwiseXor(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::BitOr => {
                                    ExpressionNode::BitwiseOr(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::And => {
                                    ExpressionNode::LogicalAnd(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::Or => {
                                    ExpressionNode::LogicalOr(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::Assign => {
                                    ExpressionNode::SimpleAssign(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::MultiplyAssign => {
                                    ExpressionNode::MultiplyAssign(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::DivideAssign => {
                                    ExpressionNode::DivideAssign(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::ModuloAssign => {
                                    ExpressionNode::ModuloAssign(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::AddAssign => {
                                    ExpressionNode::AddAssign(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::SubAssign => {
                                    ExpressionNode::SubAssign(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::LShiftAssign => {
                                    ExpressionNode::LShiftAssign(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::RShiftAssign => {
                                    ExpressionNode::RShiftAssign(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::AndAssign => {
                                    ExpressionNode::AndAssign(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::XorAssign => {
                                    ExpressionNode::XorAssign(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::OrAssign => {
                                    ExpressionNode::OrAssign(
                                        Box::new(lhs), 
                                        Box::new(rhs), 
                                        None
                                    )
                                },
                                Operator::Dot => {
                                    if let ExpressionNode::Identifier(member, _) = rhs {
                                        ExpressionNode::DotAccess(
                                            Box::new(lhs), 
                                            member, 
                                            None    
                                        )
                                    }
                                    else {
                                        return Err(ParseError::BadMemberAccess);
                                    }
                                },
                                Operator::Arrow => {
                                    if let ExpressionNode::Identifier(member, _) = rhs {
                                        ExpressionNode::ArrowAccess(
                                            Box::new(lhs), 
                                            member,
                                            None 
                                        )
                                    }
                                    else {
                                        return Err(ParseError::BadMemberAccess);
                                    }
                                },
                                _ => unreachable!()
                            };

                            continue;
                        }

                        break; // ? is this the right thing to do
                    },
                    Some(ExprPart::Atom(_)) => return Err(ParseError::UnexpectedToken(lexeme)),
                    None => break,
                }
            },
            None => return Err(ParseError::UnexpectedEOF)
        }
    }

    Ok(lhs)
}


// <translation-unit> ::= <external-declaration>
// | <translation-unit> <external-declaration>
fn parse_translation_unit(toks: &mut CTokenStream, state: &mut ParserState) -> Result<ASTNode, ParseError> {
    while let Some(_) = toks.peek() {
        parse_external_declaration(toks, state)?;
    }

    todo!()
}

// <external-declaration> ::= <function-definition>
// | <declaration>
// way to distinguish is that declarations end with a semicolon while function definitions
// have a compound statement
fn parse_external_declaration(toks: &mut CTokenStream, state: &mut ParserState) -> Result<ASTNode, ParseError> {
    // both will include declaration specifiers
    parse_declaration_specifiers(toks, state);
    if let Some((CLexemes::Semicolon, _, _)) = toks.peek() {
        // 6.7, clause 2: must have declared struct / union with tag
        // or enum constants

    }
    
    todo!()
}

struct DeclarationSpecifiers(QualifiedType, StorageClass, FunctionSpecifier);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum BasicTypeLexeme {
    Void,
    Unsigned,
    Signed,
    Char,
    Short,
    Long,
    Float,
    Double,
    Int
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
            _ => Err(ParseError::BasicTypeError)
        }
    }
}

fn parse_basic_type(basic_type_specifiers: &mut [BasicTypeLexeme]) -> Result<CType, ParseError> {
    // should be ok for now, optimize later
    match basic_type_specifiers {
        [BasicTypeLexeme::Void] => {
            Ok(CType::Void)
        },
        // default is signed char
        [BasicTypeLexeme::Char] | [BasicTypeLexeme::Signed, BasicTypeLexeme::Char] => {
            Ok(CType::BasicType { basic_type: BasicType::Char })
        },
        [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Char] => {
            Ok(CType::BasicType { basic_type: BasicType::UChar })
        },
        [BasicTypeLexeme::Short] | [BasicTypeLexeme::Signed, BasicTypeLexeme::Short]
        | [BasicTypeLexeme::Short, BasicTypeLexeme::Int]
        | [BasicTypeLexeme::Signed, BasicTypeLexeme::Short, BasicTypeLexeme::Int] => {
            Ok(CType::BasicType { basic_type: BasicType::Short })
        },
        [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Short]
        | [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Short, BasicTypeLexeme::Int] => {
            Ok(CType::BasicType { basic_type: BasicType::UShort })
        },
        [BasicTypeLexeme::Int] | [BasicTypeLexeme::Signed]
        | [BasicTypeLexeme::Signed, BasicTypeLexeme::Int] => {
            Ok(CType::BasicType { basic_type: BasicType::Int })
        },
        [BasicTypeLexeme::Unsigned] | [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Int] => {
            Ok(CType::BasicType { basic_type: BasicType::UInt })
        },
        [BasicTypeLexeme::Long] | [BasicTypeLexeme::Signed, BasicTypeLexeme::Long]
        | [BasicTypeLexeme::Long, BasicTypeLexeme::Int]
        | [BasicTypeLexeme::Signed, BasicTypeLexeme::Long, BasicTypeLexeme::Int]
        | [BasicTypeLexeme::Long, BasicTypeLexeme::Long]
        | [BasicTypeLexeme::Signed, BasicTypeLexeme::Long, BasicTypeLexeme::Long]
        | [BasicTypeLexeme::Long, BasicTypeLexeme::Long, BasicTypeLexeme::Int]
        | [BasicTypeLexeme::Signed, BasicTypeLexeme::Long, BasicTypeLexeme::Long, BasicTypeLexeme::Int] => {
            Ok(CType::BasicType { basic_type: BasicType::Long })
        },
        [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Long]
        | [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Long, BasicTypeLexeme::Int]
        | [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Long, BasicTypeLexeme::Long]
        | [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Long, BasicTypeLexeme::Long, BasicTypeLexeme::Int] => {
            Ok(CType::BasicType { basic_type: BasicType::ULong })
        },
        [BasicTypeLexeme::Double] | [BasicTypeLexeme::Long, BasicTypeLexeme::Double] => {
            Ok(CType::BasicType { basic_type: BasicType::Double })
        },
        [BasicTypeLexeme::Float] => {
            Ok(CType::BasicType { basic_type: BasicType::Float })
        },
        
        _ => { Err(ParseError::BasicTypeError) }
    }
}

// no typedefs for now
fn parse_declaration_specifiers_base(toks: &mut CTokenStream, state: &mut ParserState, parse_function_specifiers: bool, parse_storage_class: bool) -> Result<DeclarationSpecifiers, ParseError> {
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
                | CLexemes::Typedef if !parse_storage_class => {
                    break;
                }

                CLexemes::Typedef => {
                    todo!("add typedef support")
                },

                // storage class specifiers, only 1 allowed
                CLexemes::Extern => {
                    if storage_class != StorageClass::None {
                        return Err(ParseError::UnexpectedStorageClass);
                    }
                    toks.eat(CLexemes::Extern);
                    storage_class = StorageClass::Extern;
                },
                CLexemes::Auto => {
                    if storage_class != StorageClass::None {
                        return Err(ParseError::UnexpectedStorageClass);
                    }
                    toks.eat(CLexemes::Auto);
                    storage_class = StorageClass::Auto;
                },
                CLexemes::Register => {
                    if storage_class != StorageClass::None {
                        return Err(ParseError::UnexpectedStorageClass);
                    }
                    toks.eat(CLexemes::Register);
                    storage_class = StorageClass::Register;
                },
                CLexemes::Static => {
                    if storage_class != StorageClass::None {
                        return Err(ParseError::UnexpectedStorageClass);
                    }
                    toks.eat(CLexemes::Static);
                    storage_class = StorageClass::Static;
                },

                // type qualifiers, multiple allowed 
                CLexemes::Const => {
                    toks.eat(CLexemes::Const);
                    type_qualifier |= TypeQualifier::Const;
                },
                CLexemes::Restrict => {
                    return Err(ParseError::BadRestrictQualifier);
                },
                CLexemes::Volatile => {
                    toks.eat(CLexemes::Volatile);
                    type_qualifier |= TypeQualifier::Volatile;
                },
                
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
                | CLexemes::Unsigned if struct_or_union_or_enum.is_none() => {
                    toks.eat(lexeme);
                    primitive_type_specifiers.push(lexeme.try_into()?);
                },

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
                },

                // struct / union specifier (syntactically very similar!)
                CLexemes::Struct if struct_or_union_or_enum.is_none() && primitive_type_specifiers.is_empty() => {
                    let struct_type = parse_struct_or_union_specifier(toks, state)?;
                    struct_or_union_or_enum = Some(CType::StructureTypeRef { symtab_idx: struct_type });
                }
                CLexemes::Union if struct_or_union_or_enum.is_none() && primitive_type_specifiers.is_empty() => {
                    let union_type = parse_struct_or_union_specifier(toks, state)?;
                    struct_or_union_or_enum = Some(CType::UnionTypeRef { symtab_idx: union_type });
                }

                CLexemes::Struct
                | CLexemes::Union => {
                    return Err(ParseError::MultipleTypeSpecifiersInDeclaration);
                }

                // enum specifier
                CLexemes::Enum if struct_or_union_or_enum.is_none() && primitive_type_specifiers.is_empty() => {
                    let enum_type = parse_enum_specifier(toks, state)?;
                    struct_or_union_or_enum = Some(CType::EnumTypeRef { symtab_idx: enum_type });
                }

                CLexemes::Enum => {
                    return Err(ParseError::MultipleTypeSpecifiersInDeclaration);
                }

                _ => {
                    break;
                }
            },
            None => { return Err(ParseError::UnexpectedEOF); }
        }
    }

    let qualified_type = if !primitive_type_specifiers.is_empty() {
        let basic_type = parse_basic_type(&mut primitive_type_specifiers)?;
        QualifiedType {
            base_type: basic_type,
            qualifier: type_qualifier
        }
    }
    else if let Some(ty) = struct_or_union_or_enum {
        QualifiedType {
            base_type: ty,
            qualifier: type_qualifier
        }
    }
    else {
        return Err(ParseError::MissingTypeSpecifier)
    };
    
    Ok(DeclarationSpecifiers(qualified_type, storage_class, function_specifier))
}

fn parse_declaration_specifiers(toks: &mut CTokenStream, state: &mut ParserState) -> Result<DeclarationSpecifiers, ParseError> {
    parse_declaration_specifiers_base(toks, state, true, true)
}

fn parse_struct_declaration_list(toks: &mut CTokenStream, state: &mut ParserState) -> Result<Vec<AggregateMember>, ParseError> {
    let mut members: Vec<AggregateMember> = Vec::new();
    loop {
        match toks.peek() {
            Some((CLexemes::RBrace, _, _)) => { break; }
            Some((_, _, _)) => {
                let base_type = parse_specifier_qualifier_list(toks, state)?;
                parse_struct_declarator_list(toks, state, base_type, &mut members)?;
                eat_or_error!(toks, CLexemes::Semicolon)?;
            }
            None => { return Err(ParseError::UnexpectedEOF); }
        }
    }
    Ok(members)
}

fn parse_struct_declarator_list(toks: &mut CTokenStream, state: &mut ParserState, base_type: QualifiedType, members: &mut Vec<AggregateMember>) -> Result<(), ParseError> {
    // TODO: support bitfields
    
    let (member_type, member_name) = parse_declarator(toks, state, base_type.clone())?;
    members.push((member_name, member_type));
    loop {
        match toks.peek() {
            Some((CLexemes::Comma, _, _)) => {
                // hopefully cloning is not expensive in the common case
                let (member_type, member_name) = parse_declarator(toks, state, base_type.clone())?;
                members.push((member_name, member_type));
            },
            Some((_, _, _)) => {
                return Ok(())
            },
            None => { return Err(ParseError::UnexpectedEOF); }
        }
    }
}

fn parse_specifier_qualifier_list(toks: &mut CTokenStream, state: &mut ParserState) -> Result<QualifiedType, ParseError> {
    let DeclarationSpecifiers(qualified_type, _, _) = parse_declaration_specifiers_base(toks, state, false, false)?;
    Ok(qualified_type)
}

fn parse_struct_or_union_specifier(toks: &mut CTokenStream, state: &mut ParserState) -> Result<TypeIdx, ParseError> {
    #[derive(PartialEq, Eq)]
    enum StructOrUnion {
        Struct, Union
    }
    
    let start = toks.get_location();
    let specifier_type: StructOrUnion;
    match toks.peek() {
        Some((CLexemes::Struct, _, _)) => {
            toks.eat(CLexemes::Struct);
            specifier_type = StructOrUnion::Struct;        
        },
        Some((CLexemes::Union, _, _)) => {
            toks.eat(CLexemes::Union);
            specifier_type = StructOrUnion::Union;
        },
        Some((other, _, _)) => { return Err(ParseError::UnexpectedToken(other)); }
        None => { return Err(ParseError::UnexpectedEOF); }
    }

    let struct_or_union_tag: Option<String>;
    match toks.peek() {
        Some((CLexemes::LBrace, _, _)) => {
            // untagged, won't conflict and always declares a new type (6.7.2.3 5)
            struct_or_union_tag = None;
        },
        Some((CLexemes::Identifier, tag, _)) => {
            struct_or_union_tag = Some(tag.to_string());
            // do lookup, insert incomplete type if not present
            match toks.peek() {
                Some((CLexemes::LBrace, _, _)) => {
                    toks.eat(CLexemes::LBrace);
                },
                Some((_, _, _)) => {
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
                                StructOrUnion::Struct => state.parse_tree_stack.push_back(ParseNode::StructSpecifier(start, end)),
                                StructOrUnion::Union => state.parse_tree_stack.push_back(ParseNode::UnionSpecifier(start, end)),
                            }
                            return Ok(incomplete_type);
                        }
                    }
                },
                None => { return Err(ParseError::UnexpectedEOF); }
            }
        },
        Some((other, _, _)) => { return Err(ParseError::UnexpectedToken(other)); }
        None => { return Err(ParseError::UnexpectedEOF); }
    }

    // declaring a new struct / union
    // 1. not allowed to redeclare enum
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

    let members: Vec<AggregateMember> = parse_struct_declaration_list(toks, state)?;
    eat_or_error!(toks, CLexemes::RBrace)?;

    let struct_or_union_type = match specifier_type {
        StructOrUnion::Struct => {
            CanonicalType::StructureType { tag: struct_or_union_tag.clone(), members }
        },
        StructOrUnion::Union => {
            CanonicalType::UnionType { tag: struct_or_union_tag.clone(), members }
        },
    };

    let type_idx = state.symbol_table.add_type(struct_or_union_type);
    if let Some(name) = struct_or_union_tag {
        state.symbol_table.add_tag(state.current_scope, name, type_idx)?;
    }

    Ok(type_idx)
}

fn parse_enum_specifier(toks: &mut CTokenStream, state: &mut ParserState) -> Result<TypeIdx, ParseError> {
    let start = toks.get_location();
    match toks.peek() {
        Some((CLexemes::Enum, _, _)) => {
            toks.eat(CLexemes::Enum);
        },
        Some((other, _, _)) => {
            return Err(ParseError::UnexpectedToken(other));
        },
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
        },
        Some((CLexemes::Identifier, ident, _)) => {
            enum_tag = Some(ident.to_string());
            toks.eat(CLexemes::Identifier);
                    
            let next_tok = toks.peek();
            match next_tok {
                Some((CLexemes::LBrace, _, _)) => {
                    toks.eat(CLexemes::LBrace);
                },
                Some(_) => {
                    // incomplete enum, need to do lookup (6.7.2.3 3)
                    // GCC / Clang both support extension which allows enum to be incomplete type
                    // (only complains if using -Wpedantic)
                    let lookup = state.symbol_table.lookup_tag_type_idx(state.current_scope, enum_tag.as_ref().unwrap());
                    match lookup {
                        Some(tag_type) => {
                            if let CanonicalType::EnumerationType { .. } = state.symbol_table.get_type(tag_type) {
                                let end = toks.get_location();
                                #[cfg(debug_assertions)]
                                state.parse_tree_stack.push_back(ParseNode::EnumSpecifier(start, end));
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
                },
                None => return Err(ParseError::UnexpectedEOF)
            };
        },
        Some((other, _, _)) => { return Err(ParseError::UnexpectedToken(other)) },
        None => { return Err(ParseError::UnexpectedEOF) }
    }

    // 6.7.2.3 1, not allowed to redeclare enum (or struct or union) contents
    if let Some(enum_tag) = &enum_tag {
        let direct_lookup = state.symbol_table.direct_lookup_tag_type(state.current_scope, &enum_tag);
        // shares namespace with struct / unions, so a struct / union named the same thing is also not ok
        if direct_lookup.is_some() {
            return Err(ParseError::RedeclaredEnum(enum_tag.to_string()))
        }
    }
    
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
            Some((CLexemes::RBrace, _, _)) => { return Err(ParseError::EmptyEnum); }
            Some((other, _, _)) => { return Err(ParseError::UnexpectedToken(other)); }
            None => { return Err(ParseError::UnexpectedEOF); }
        }

        // 2. comma -> go next, brace -> break, other -> bad token, equal -> expect int
        match toks.peek() {
            Some((CLexemes::Comma, _, _)) => {
                let enum_constant_value = Symbol::Constant(Constant::Int(counter));
                enum_members.push((enum_constant_name.clone(), counter));
                
                // ensure enum constants don't conflict with existing symbols
                match state.symbol_table.add_symbol(state.current_scope, enum_constant_name, enum_constant_value) {
                    Err(e) => { return Err(ParseError::RedeclaredEnumConstant(e)); }
                    Ok(_) => {}
                }

                toks.eat(CLexemes::Comma);
                counter += 1;
                if let Some((CLexemes::RBrace, _, _)) = toks.peek() {
                    toks.eat(CLexemes::RBrace);
                    break;
                }
                else {
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
            Some((other, _, _)) => { return Err(ParseError::UnexpectedToken(other)); }
            None => { return Err(ParseError::UnexpectedEOF); }
        }

        match toks.peek() {
            Some((CLexemes::IntegerConst, int_str, _)) => {
                // TODO: technically any integer constant expression is ok, but just ignore this for now
                // until can parse constant expressions more generally.
                let enum_value = int_str.parse::<i32>()
                    .map_err(|_| ParseError::InvalidEnumConstant(int_str.to_string()))?;
                
                let enum_constant_value = Symbol::Constant(Constant::Int(enum_value));
                enum_members.push((enum_constant_name.clone(), enum_value));

                match state.symbol_table.add_symbol(state.current_scope, enum_constant_name, enum_constant_value) {
                    Err(e) => { return Err(ParseError::RedeclaredEnumConstant(e)); }
                    Ok(_) => {}
                }

                counter = enum_value + 1;
                toks.eat(CLexemes::IntegerConst);
            },
            Some((_, text, _)) => {
                return Err(ParseError::InvalidEnumConstant(text.to_string()));
            }
            None => { return Err(ParseError::UnexpectedEOF); },
        }

        match toks.peek() {
            Some((CLexemes::Comma, _, _)) => {
                toks.eat(CLexemes::Comma);
                if let Some((CLexemes::RBrace, _, _)) = toks.peek() {
                    toks.eat(CLexemes::RBrace);
                    break;
                }
                else {
                    continue;
                }
            }
            Some((CLexemes::RBrace, _, _)) => { 
                toks.eat(CLexemes::RBrace);
                break;
            },
            Some((other, _, _)) => { return Err(ParseError::UnexpectedToken(other)); },
            None => { return Err(ParseError::UnexpectedEOF); }
        }
    }

    let enum_type = CanonicalType::EnumerationType { 
        tag: enum_tag.clone(), 
        members: enum_members 
    };
    
    let enum_type_idx = state.symbol_table.add_type(enum_type);
    if enum_tag.is_some() {
        state.symbol_table.add_tag(state.current_scope, enum_tag.unwrap(), enum_type_idx)?;
    }

    let end = toks.get_location();
    #[cfg(debug_assertions)]
    state.parse_tree_stack.push_back(ParseNode::EnumSpecifier(start, end));
    Ok(enum_type_idx)
}

fn parse_init_declarators(toks: &mut CTokenStream, state: &mut ParserState) -> Result<ASTNode, ParseError> {
    let start = toks.get_location();
    parse_init_declarator(toks, state);
    while let Some((lexeme, _, _)) = toks.peek() {
        match lexeme {
            CLexemes::Comma => {
                toks.eat(CLexemes::Comma);
            },
            CLexemes::Semicolon => {
                
            }
            other => return Err(ParseError::UnexpectedToken(other))
        }
    }

    // unexpected EOF
    Err(ParseError::UnexpectedEOF)
}

fn parse_init_declarator(toks: &mut CTokenStream, state: &mut ParserState) {

}

// The concept of a declarator basically only exists during parsing and isn't semantically meaningful elsewhere
// "abstract" declarator has no identifier
struct Declarator(QualifiedType, Option<String>);
fn parse_declarator_base(toks: &mut CTokenStream, state: &mut ParserState, base_type: QualifiedType) -> Result<Declarator, ParseError> {
    let mut pointers: Vec<(TypeQualifier, usize)> = Vec::new();
    enum ArrayOrFunctionDeclarator { ArrayDeclarator(ArrayDeclarator), FunctionDeclarator(FunctionDeclarator) }
    let mut array_or_function_declarators: Vec<(ArrayOrFunctionDeclarator, usize)> = Vec::new();
    let mut identifier: Option<String> = None;
    let mut level: usize = 0;
    loop {
        match toks.peek() {
            Some((CLexemes::Star, _, _)) => {
                let pointer = parse_pointer_declarator(toks, state)?;
                pointers.push((pointer.0, 0))
            },
            Some((CLexemes::LParen, _, _)) => {
                // parse inner declarator
                toks.eat(CLexemes::LParen);
                level += 1;
                break;
            },
            Some((CLexemes::Identifier, ident, _)) => {
                identifier = Some(ident.to_string());
                toks.eat(CLexemes::Identifier);
                break;
            },
            Some((other, _, _)) => { return Err(ParseError::UnexpectedToken(other)); },
            None => { return Err(ParseError::UnexpectedEOF); }
        }
    }

    loop {
        match toks.peek() {
            Some((CLexemes::LBracket, _, _)) => {
                let array = parse_array_declarator(toks, state)?;
                array_or_function_declarators.push((ArrayOrFunctionDeclarator::ArrayDeclarator(array), level));
            },
            Some((CLexemes::LParen, _, _)) => {
                let function = parse_function_declarator(toks, state)?;
                array_or_function_declarators.push((ArrayOrFunctionDeclarator::FunctionDeclarator(function), level));
            },
            Some((CLexemes::RParen, _, _)) => {
                level -= 1;
            },
            Some((other, _, _)) => {
                break;
            },
            None => { return Err(ParseError::UnexpectedEOF); }
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
    let max_level = level;
    let mut pointer_index: usize = 0;
    array_or_function_declarators.reverse();
    let mut current_type = base_type;
    for level in 0..=max_level {
        while pointer_index < pointers.len() && pointers[pointer_index].1 == level {
            current_type = QualifiedType {
                base_type: CType::PointerType { pointee_type: Box::new(current_type) },
                qualifier: pointers[pointer_index].0
            };

            pointer_index += 1;
        }
        while let Some((_, l)) = array_or_function_declarators.last() {
            if *l == level {
                let (d, _) = array_or_function_declarators.pop().unwrap();
                match d {
                    ArrayOrFunctionDeclarator::ArrayDeclarator(array) => {
                        let new_type: CType;
                        if let Some(size) = array.1 {
                            new_type = CType::ArrayType { 
                                size, 
                                element_type: Box::new(current_type) 
                            };
                        }
                        else {
                            new_type = CType::IncompleteArrayType { 
                                element_type: Box::new(current_type) 
                            };
                        }
                        current_type = QualifiedType {
                            base_type: new_type,
                            qualifier: array.0
                        }
                    },
                    ArrayOrFunctionDeclarator::FunctionDeclarator(func) => {
                        let new_type = CType::FunctionType {
                            parameter_types: func.argument_types,
                            return_type: Box::new(current_type),
                            function_specifier: FunctionSpecifier::None, // TODO: figure out inline
                            varargs: func.varargs,
                        };

                        current_type = QualifiedType {
                            base_type: new_type,
                            qualifier: TypeQualifier::empty()
                        }
                    },
                };
            }
        }
    }

    Ok(Declarator(current_type, identifier))
}

fn parse_declarator(toks: &mut CTokenStream, state: &mut ParserState, base_type: QualifiedType) -> Result<(QualifiedType, String), ParseError> {
    let decl = parse_declarator_base(toks, state, base_type)?;
    if let Some(ident) = decl.1 {
        Ok((decl.0, ident))
    }
    else {
        Err(ParseError::DeclaratorRequiresName)
    }
}

fn parse_abstract_declarator(toks: &mut CTokenStream, state: &mut ParserState, base_type: QualifiedType) -> Result<QualifiedType, ParseError> {
    let decl = parse_declarator_base(toks, state, base_type)?;
    if let None = decl.1 {
        Ok(decl.0)
    }
    else {
        Err(ParseError::UnexpectedDeclaratorName)
    }
}

struct PointerDeclarator(TypeQualifier);
fn parse_pointer_declarator(toks: &mut CTokenStream, state: &mut ParserState) -> Result<PointerDeclarator, ParseError> {
    let mut qualifier = TypeQualifier::empty();
    eat_or_error!(toks, CLexemes::Star)?;
    loop {
        match toks.peek() {
            Some((CLexemes::Const, _, _)) => {
                qualifier |= TypeQualifier::Const;
                toks.eat(CLexemes::Const);
            },  
            Some((CLexemes::Volatile, _, _)) => {
                qualifier |= TypeQualifier::Volatile;
                toks.eat(CLexemes::Volatile);
            },
            Some((CLexemes::Restrict, _, _)) => {
                qualifier |= TypeQualifier::Restrict;
                toks.eat(CLexemes::Restrict);
            },
            Some((other, _, _)) => { return Ok(PointerDeclarator(qualifier)); }
            None => { return Err(ParseError::UnexpectedEOF); }
        }
    }
}
struct ArrayDeclarator(TypeQualifier, Option<usize>);
fn parse_array_declarator(toks: &mut CTokenStream, state: &mut ParserState) -> Result<ArrayDeclarator, ParseError> {
    let mut qualifier = TypeQualifier::empty();
    eat_or_error!(toks, CLexemes::LBracket)?;
    loop {
        match toks.peek() {
            Some((CLexemes::Restrict, _, _)) => {
                qualifier |= TypeQualifier::Restrict;
            },
            Some((CLexemes::Volatile, _, _)) => {
                qualifier |= TypeQualifier::Volatile;
            },
            Some((CLexemes::Const, _, _)) => {
                qualifier |= TypeQualifier::Const;
            },
            Some((CLexemes::RBracket, _, _)) => {
                toks.eat(CLexemes::RBracket);
                return Ok(ArrayDeclarator(qualifier, None));
            },
            Some((CLexemes::IntegerConst, i, _)) => {
                let size = i.parse::<usize>().map_err(
                    |e| ParseError::BadArrayBound(e)
                )?;

                eat_or_error!(toks, CLexemes::RBracket)?;
                return Ok(ArrayDeclarator(qualifier, Some(size)));
            },
            Some((other, _, _)) => {
                // TODO: support integer constant expressions here
                return Err(ParseError::UnexpectedToken(other));
            }
            None => { return Err(ParseError::UnexpectedEOF); }
        }
    }
}


// only time scope is needed is if this is actually a function definition, and not
// just a prototype
struct FunctionDeclarator {
    argument_types: Vec<QualifiedType>, 
    varargs: bool, 
    prototype_scope: Scope
}

// will not support old-style function declarations because they are cringe
// this code is used for both function prototypes and function definitions
fn parse_function_declarator(toks: &mut CTokenStream, state: &mut ParserState) -> Result<FunctionDeclarator, ParseError> {
    eat_or_error!(toks, CLexemes::LParen)?;
    state.open_scope(ScopeType::FunctionScope);
    
    // ignore special case for empty parameter list
    // technically, this has special behavior (completed by later declarations / usages),
    // a sort of "incomplete" function type. not used very much, won't support
    let mut parameter_types: Vec<QualifiedType> = Vec::new();
    let mut varargs = false;
    loop {
        match toks.peek() {
            Some((CLexemes::RParen, _, _)) => {
                toks.eat(CLexemes::RParen);
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
                        CType::FunctionType { .. } => {
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
                
                let declarator = FunctionDeclarator {
                    argument_types: parameter_types, 
                    varargs, 
                    prototype_scope: state.current_scope
                };
                state.close_scope()?;
                return Ok(declarator);
            },
            Some((CLexemes::Ellipsis, _, _)) => {
                toks.eat(CLexemes::Ellipsis);
                varargs = true;
                eat_or_error!(toks, CLexemes::RParen)?;
                let declarator = FunctionDeclarator {
                    argument_types: parameter_types, 
                    varargs, 
                    prototype_scope: state.current_scope
                };
                state.close_scope()?;
                return Ok(declarator);
            },
            Some((_, _, _)) => {
                let parameter_base_type = parse_declaration_specifiers(toks, state)?;
                if parameter_base_type.1 != StorageClass::None {
                    return Err(ParseError::BadStorageClassInArgumentList);
                }
                if parameter_base_type.2 != FunctionSpecifier::None {
                    return Err(ParseError::BadFunctionSpecifier);
                }

                match toks.peek() {
                    Some((CLexemes::Comma, _, _)) => {
                        parameter_types.push(parameter_base_type.0);
                        toks.eat(CLexemes::Comma);
                    }
                    Some((CLexemes::RParen, _, _)) => {
                        parameter_types.push(parameter_base_type.0);
                    }
                    Some((_, _, _)) => {
                        let DeclarationSpecifiers(parameter_base_type, storage_class, _) = parameter_base_type;
                        let parameter_type = parse_declarator_base(toks, state, parameter_base_type)?;
                        if let Some(parameter_name) = parameter_type.1 {
                            let parameter = Symbol::Variable { 
                                symbol_type: parameter_type.0.clone(), 
                                storage_class, 
                                linkage: Linkage::None 
                            };
                            state.symbol_table.add_symbol(state.current_scope, parameter_name, parameter)?;
                        }
                        parameter_types.push(parameter_type.0);
                        match toks.peek() {
                            Some((CLexemes::Comma, _, _)) => {
                                toks.eat(CLexemes::Comma);
                                continue;
                            }
                            _ => {
                                continue;
                            }
                        }
                    },
                    None => { return Err(ParseError::UnexpectedEOF); }
                }
            },
            None => { return Err(ParseError::UnexpectedEOF); }
        }
    }
}

fn parse_statement(toks: &mut CTokenStream, state: &mut ParserState) {
    let (lexeme, s, p) = toks.peek()
        .expect("statement: unexpected EOF");

    match lexeme {
        CLexemes::If | CLexemes::Switch => {}
        CLexemes::While | CLexemes::Do | CLexemes::For => {}
        CLexemes::Goto | CLexemes::Continue | CLexemes::Break | CLexemes::Return => {}
        CLexemes::Case | CLexemes::Default => {}
        CLexemes::LParen => {}
        CLexemes::Identifier => {
            // need one additional lookahead
            let (additional_lookahead, _, _) = toks.peekn(1)
                .expect("unexpected eof");

            if additional_lookahead == CLexemes::Colon {

            }
            else {

            }
        }
        _ => panic!("error while parsing statement")
    }
}

fn parse_labeled_statement(toks: &mut CTokenStream, state: &mut ParserState) {

}

fn parse_compound_statement(toks: &mut CTokenStream, state: &mut ParserState) {
    toks.eat(CLexemes::LBrace);
    while let Some((lexeme, _, _)) = toks.peek() {
        if lexeme != CLexemes::RBrace {
            // how to disambiguate declaration vs statement?
        }
        else {
            break;
        }
    }
    toks.eat(CLexemes::RBrace);
}

fn parse_expression_statement(toks: &mut CTokenStream, state: &mut ParserState) -> Result<ASTNode, ParseError> {
    match toks.peek() {
        Some((CLexemes::Semicolon, _, _)) => {
            toks.eat(CLexemes::Semicolon);
            Ok(ASTNode::ExpressionStatement(None, state.current_scope))
        }
        Some((_, _, _)) => {
            let expr = parse_expr(toks, state)?;
            Ok(ASTNode::ExpressionStatement(Some(Box::new(expr)), state.current_scope))
        }
        None => {
            Err(ParseError::UnexpectedEOF)
        }
    }
}

fn parse_selection_statement(toks: &mut CTokenStream, state: &mut ParserState) {

}

fn parse_iteration_statement(toks: &mut CTokenStream, state: &mut ParserState) {

}

fn parse_jump_statement(toks: &mut CTokenStream, state: &mut ParserState) {

}