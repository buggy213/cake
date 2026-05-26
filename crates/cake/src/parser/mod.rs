pub mod ast;
pub mod earley;
pub mod grammar;
pub mod hand_parser;
pub mod lr;

use std::error::Error;

use crate::scanner::{TokenStream, lexeme_sets::c_lexemes::CLexemes};

pub struct HandParser<T> where T: TokenStream<CLexemes> {
    tokens: T,
    state: hand_parser::ParserState
}

pub struct ParseOutput {
    pub(crate) root_node: ast::ASTNode,
    pub(crate) final_parse_state: hand_parser::ParserState,
}

impl<T: TokenStream<CLexemes>> HandParser<T> {
    pub fn new(tokens: T) -> Self {
        Self { 
            tokens,
            state: hand_parser::ParserState::new()
        }
    }

    pub fn parse(mut self) -> Result<ParseOutput, Box<dyn Error>> {
        let parse_result = hand_parser::parse_translation_unit(&mut self.tokens, &mut self.state);
        match parse_result {
            Ok(root_node) => Ok(
                ParseOutput { 
                    root_node,
                    final_parse_state: self.state
                }
            ),
            Err(e) => Err(e),
        }
    }
}