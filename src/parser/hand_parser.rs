use core::panic;

use crate::scanner::{lexeme_sets::c_lexemes::CLexemes, TokenStream};

type CTokenStream<'a, 'b> = crate::scanner::RawTokenStream<'a, CLexemes>;

// explicitly materialize parse tree for now, can think about more efficient
// semantic action based approach later
enum ParseNode {

}

struct ParserState {

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
fn parse_translation_unit(toks: &mut CTokenStream, state: &mut ParserState) {

}

// <external-declaration> ::= <function-definition>
// | <declaration>
fn parse_external_declaration(toks: &mut CTokenStream, state: &mut ParserState) {
    
}

// standard allows for functions like
// ```
// void f(a, b)
// int a, b
// { ... }
// ```

// do this later
fn parse_function_definition(toks: &mut CTokenStream, state: &mut ParserState) {

}

fn parse_declaration(toks: &mut CTokenStream, state: &mut ParserState) {
    parse_declaration_specifiers(toks, state);
    
}

fn parse_declaration_specifiers(toks: &mut CTokenStream, state: &mut ParserState) {
    match toks.peek() {
        Some((lexeme, _, _)) => {
            match lexeme {
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
                | CLexemes::Signed
                | CLexemes::Unsigned
                | CLexemes::Bool
                | CLexemes::Complex
                
                | CLexemes::Const
                | CLexemes::Restrict
                | CLexemes::Volatile 
                
                | CLexemes::Inline => {
                    // TODO
                    toks.eat(lexeme);
                }
                CLexemes::Identifier => todo!("need semantic analysis to resolve ambiguity"),

                _ => panic!("bad")
            }
        },
        None => panic!("error while parsing declaration specifier"),
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

fn parse_expression_statement(toks: &mut CTokenStream, state: &mut ParserState) {

}

fn parse_selection_statement(toks: &mut CTokenStream, state: &mut ParserState) {

}

fn parse_iteration_statement(toks: &mut CTokenStream, state: &mut ParserState) {

}

fn parse_jump_statement(toks: &mut CTokenStream, state: &mut ParserState) {

}