use core::panic;
use std::collections::VecDeque;

use thiserror::Error;

use crate::{parser::ast::Constant, scanner::{lexeme_sets::c_lexemes::CLexemes, TokenStream}, semantics::{symtab::{Scope, StorageClass, Symbol, SymbolTable, SymtabError, TypeIdx}, types::{AggregateMember, CType, FunctionSpecifier, TypeQualifier}}};

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
    BadRestrictQualifier,
    #[error("failed to lookup item in symbol table")]
    LookupError(#[source] SymtabError),
    #[error("cannot redeclare enum `{0}` in same scope")]
    RedeclaredEnum(String),
    #[error("cannot redeclare enum constant")]
    RedeclaredEnumConstant(#[source] SymtabError),
    #[error("invalid enum constant (must be integer constant)")]
    InvalidEnumConstant(String),
    #[error("empty enum is not allowed")]
    EmptyEnum,
    #[error("enum <tag> cannot be declared alongside struct <tag> or union <tag>")]
    EnumDeclarationMustMatch
}

struct ParserState {
    symbol_table: SymbolTable,
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
    let mut struct_or_union_or_enum: Option<TypeIdx> = None;
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

            // struct / union specifier (syntactically very similar!)
            CLexemes::Struct
            | CLexemes::Union => {

            }

            // enum specifier
            CLexemes::Enum => {
                
            }

            _ => todo!()
        }
    }

    todo!()
}

fn parse_struct_declaration(toks: &mut CTokenStream, state: &mut ParserState) {

}

fn parse_struct_or_union_specifier(toks: &mut CTokenStream, state: &mut ParserState) -> Result<TypeIdx, ParseError> {
    enum StructOrUnion {
        Struct, Union
    }
    
    let members: Vec<AggregateMember> = Vec::new();
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
            // untagged
            struct_or_union_tag = None;
        },
        Some((CLexemes::Identifier, tag, _)) => {
            struct_or_union_tag = Some(tag.to_string());
            // do lookup, insert incomplete type if not present
            
            match toks.peek() {
                Some((CLexemes::LBrace, _, _)) => {
                    toks.eat(CLexemes::LBrace);
                },
                Some((other, _, _)) => {
                    
                },
                None => { return Err(ParseError::UnexpectedEOF); }
            }
        },
        Some((other, _, _)) => { return Err(ParseError::UnexpectedToken(other)); }
        None => { return Err(ParseError::UnexpectedEOF); }
    }    
    

    todo!()
}

fn parse_enum_specifier(toks: &mut CTokenStream, state: &mut ParserState) -> Result<TypeIdx, ParseError> {
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
            // untagged enum
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
                    let lookup = state.symbol_table.lookup_tag_type_idx(state.current_scope, &enum_tag.unwrap());
                    match lookup {
                        Ok(tag_type) => {
                            if let CType::EnumerationType { .. } = state.symbol_table.get_type(tag_type) {
                                return Ok(tag_type);
                            }
                            else {
                                // other type is not an enum, so lookup fails
                                return Err(ParseError::EnumDeclarationMustMatch);
                            }
                        },
                        // incomplete enum not allowed
                        Err(err) => return Err(ParseError::LookupError(err))
                    }
                },
                None => return Err(ParseError::UnexpectedEOF)
            };
        },
        Some((other, _, _)) => { return Err(ParseError::UnexpectedToken(other)) },
        None => { return Err(ParseError::UnexpectedEOF) }
    }

    // 6.7.2.3 1, not allowed to redeclare enum (or struct or union) contents
    match &enum_tag {
        Some(enum_tag) => {
            let direct_lookup = state.symbol_table.direct_lookup_tag_type(state.current_scope, &enum_tag);
            // shares namespace with struct / unions, so a struct / union named the same thing is also not ok
            if direct_lookup.is_ok() {
                return Err(ParseError::RedeclaredEnum(enum_tag.to_string()))
            }
        },
        None => {}, // 6.7.2.3 5, all untagged enums/unions/structs have different types
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

    let enum_type = CType::EnumerationType { 
        tag: enum_tag, 
        members: enum_members 
    };

    return Ok(state.symbol_table.add_type(enum_type));
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