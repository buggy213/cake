use crate::scanner::{lexeme_sets::c_lexemes::CLexemes, TokenStream};

type CTokenStream<'a> = crate::scanner::RawTokenStream<'a, CLexemes>;

struct ParserState {

}

// TODO: error recovery
fn parse_expr(toks: &mut CTokenStream, state: &mut ParserState) {
    
}

fn parse_postfix_expr(toks: &mut CTokenStream, state: &mut ParserState) {

}

fn parse_primary_expr(toks: &mut CTokenStream, state: &mut ParserState) {
    let (lookahead, _) = toks.peek().expect("bad");
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

fn parse_translation_unit(toks: &mut CTokenStream, state: &mut ParserState) {

}

fn parse_external_declaration(toks: &mut CTokenStream, state: &mut ParserState) {

}