use core::panic;
use std::collections::VecDeque;

use thiserror::Error;

use crate::{scanner::{lexeme_sets::c_lexemes::CLexemes, TokenStream}, semantics::{symtab::{Scope, StorageClass, SymbolTable}, types::{CType, FunctionSpecifier, TypeQualifier}}};

use super::ast::ASTNode;

type CTokenStream<'a> = crate::scanner::RawTokenStream<'a, CLexemes>;

// can explicitly materialize parse tree for debugging purposes, though not required
#[cfg(debug_assertions)]
enum ParseNode {
    Test(Box<ParseNode>, usize, usize)
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

#[derive(Error, Debug)]
enum ParseError {
    #[error("Unexpected end of file while parsing")]
    UnexpectedEOF,
    #[error("Unexpected token {0:?} while parsing")]
    UnexpectedToken(CLexemes),
    #[error("Only one storage class allowed in declaration")]
    UnexpectedStorageClass,
    #[error("restrict qualifier only applies to pointers")]
    BadRestrictQualifier
}

struct ParserState {
    smybol_table: SymbolTable,
    current_scope: Scope,
    #[cfg(debug_assertions)]
    parse_tree_stack: VecDeque<ParseNode>
}

// TODO: error recovery
fn parse_expr(toks: &mut CTokenStream, state: &mut ParserState) {
    
}

fn parse_postfix_expr(toks: &mut CTokenStream, state: &mut ParserState) {

}

fn parse_primary_expr(toks: &mut CTokenStream, state: &mut ParserState) {
    let (lookahead, _, _) = toks.peek().expect("bad");
    match lookahead {
        CLexemes::Identifier => todo!(),
        CLexemes::IntegerConst => todo!(),
        CLexemes::FloatConst => todo!(),
        CLexemes::CharConst => todo!(),
        CLexemes::OctalCharConst => todo!(),
        CLexemes::StringConst => todo!(),
        
        CLexemes::LParen => {
            toks.eat(CLexemes::LParen);
            parse_expr(toks, state);
            toks.eat(CLexemes::RParen);
        },

        _ => panic!("bad")
    }
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

// no typedefs for now
fn parse_declaration_specifiers(toks: &mut CTokenStream, state: &mut ParserState) -> Result<ASTNode, ParseError> {
    let mut storage_class: StorageClass = StorageClass::None;
    let mut type_qualifier: TypeQualifier = TypeQualifier::empty();
    let mut function_specifier: FunctionSpecifier = FunctionSpecifier::None;
    let mut primitive_type_specifiers: Vec<CLexemes> = Vec::new();
    while let Some((lexeme, _, _)) = toks.peek() {
        match lexeme {
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
            | CLexemes::Unsigned => {
                toks.eat(lexeme);
                primitive_type_specifiers.push(lexeme);
            },

            CLexemes::Struct
            | CLexemes::Union
            | CLexemes::Enum => {
                
            }

            _ => todo!()
        }
    }

    todo!()
}

fn parse_struct_or_union_specifier(toks: &mut CTokenStream, state: &mut ParserState) -> Result<CType, ParseError> {
    todo!()
}

fn parse_enum_specifier(toks: &mut CTokenStream, state: &mut ParserState) -> Result<CType, ParseError> {
    if let Some((CLexemes::Enum, _, _)) = toks.peek() {

    }
    else {
        return Err(ParseError::UnexpectedEOF);
    }
    todo!()
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

    #[cfg(debug_assertions)]
    materialize_parse_node![5, 100, ParseNode::Test, 1, state.parse_tree_stack];

    // unexpected EOF
    Err(ParseError::UnexpectedEOF)
}

fn parse_init_declarator(toks: &mut CTokenStream, state: &mut ParserState) {

}

fn parse_declarator(toks: &mut CTokenStream, state: &mut ParserState) {

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

fn parse_expression_statement(toks: &mut CTokenStream, state: &mut ParserState) {

}

fn parse_selection_statement(toks: &mut CTokenStream, state: &mut ParserState) {

}

fn parse_iteration_statement(toks: &mut CTokenStream, state: &mut ParserState) {

}

fn parse_jump_statement(toks: &mut CTokenStream, state: &mut ParserState) {

}