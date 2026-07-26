pub mod ast;
pub mod hand_parser;

use std::error::Error;

use crate::{Preprocessor, scanner::{PreprocessedCTokenStream, string_pool::StringPool}};

pub struct HandParser {
    tokens: PreprocessedCTokenStream,
    state: hand_parser::ParserState
}

pub struct ParseOutput {
    pub(crate) root_node: ast::ASTNode,
    pub(crate) final_parse_state: hand_parser::ParserState,
    pub(crate) string_pool: StringPool,
}

impl HandParser {
    pub fn new(tokens: Preprocessor) -> Self {
        Self { 
            tokens: PreprocessedCTokenStream::new(tokens),
            state: hand_parser::ParserState::new()
        }
    }

    pub fn parse(mut self) -> Result<ParseOutput, Box<dyn Error>> {
        let parse_result = hand_parser::parse_translation_unit(&mut self.tokens, &mut self.state);
        match parse_result {
            Ok(root_node) => Ok(
                ParseOutput { 
                    root_node,
                    final_parse_state: self.state,
                    string_pool: self.tokens.string_pool,
                }
            ),
            Err(e) => Err(e),
        }
    }
}