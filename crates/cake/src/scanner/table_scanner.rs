use std::collections::VecDeque;

use cake_re::DFATable;

use super::lexemes::LexemeSet;

pub struct DFAScanner {
    table: DFATable,
}

pub enum ScannerResult<'scanner_input> {
    EndOfInput,
    Failed,
    Ok(&'scanner_input str, u32, usize), // output: lexeme text, action, next cursor
}

impl DFAScanner {
    // create and optimize DFA from LexemeSet at runtime
    pub fn build_lexeme_set_scanner<T: LexemeSet>() -> DFAScanner {
        let re_strs: Vec<&str> = T::iter().map(|x| x.pattern()).collect();
        let table = DFATable::from_regex_strs(&re_strs);
        DFAScanner::new(table)
    }

    // load precalculated DFA from LexemeSet
    pub fn load_lexeme_set_scanner<T: LexemeSet>() -> DFAScanner {
        let table = T::table();
        DFAScanner::new(table)
    }

    fn new(table: DFATable) -> Self {
        Self { table }
    }

    // implements "maximal munch" lexing - always try to "eat" as many characters as possible to form next token
    pub fn next_word<'a>(&self, input: &'a [u8], start_cursor: usize) -> ScannerResult<'a> {
        if start_cursor >= input.len() {
            return ScannerResult::EndOfInput;
        }

        let mut cursor = start_cursor;

        let mut state = self.table.initial_state;
        let mut stack: VecDeque<u32> = VecDeque::new();
        stack.push_back(u32::MAX); // MAX as sentinel

        let mut lexeme = String::new();

        while state != self.table.states - 1 {
            if cursor >= input.len() {
                break; // hit EOF
            }
            let c = input[cursor];
            cursor += 1;

            // println!("c = '{:?}'", c);
            lexeme.push(char::from_u32(c as u32).expect("character is somehow invalid"));

            stack.push_back(state);
            let next_state = self.table.get_next_state(state, c);
            state = next_state;

            if self.table.actions[state as usize] != -1 {
                // accept state
                // println!("accepted '{}'", lexeme);
                stack.clear();
            }
        }

        while state != u32::MAX && self.table.actions[state as usize] == -1 {
            state = stack.pop_back().expect("should not be empty");
            lexeme.pop();
            if cursor != start_cursor {
                cursor -= 1;
            } else {
                debug_assert!(state == u32::MAX && lexeme.is_empty());
            }
        }

        // SAFETY: earlier passes ensured its valid utf8 (ascii)
        let slice = unsafe { std::str::from_utf8_unchecked(&input[start_cursor..cursor]) };
        if state == u32::MAX {
            ScannerResult::Failed
        } else {
            let action = self.table.actions[state as usize];
            ScannerResult::Ok(slice, action as u32, cursor)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::{
        lexeme_sets::c_preprocessor::CPreprocessor, table_scanner::ScannerResult,
    };

    use super::DFAScanner;

    impl DFAScanner {
        fn scan_string(&mut self, input: &str) {
            assert!(input.is_ascii(), "only ascii supported");

            let mut cursor = 0;
            let input = input.as_bytes();

            while cursor < input.len() {
                match self.next_word(input, cursor) {
                    ScannerResult::EndOfInput => unreachable!(), // should not happen
                    ScannerResult::Failed => panic!("failed to tokenize string"),
                    ScannerResult::Ok(lexeme_text, action, next_cursor) => {
                        println!("'{}', {}", lexeme_text, action);
                        cursor = next_cursor;
                    }
                }
            }
        }
    }

    #[test]
    fn test_basic() {
        let mut scanner = DFAScanner::build_lexeme_set_scanner::<CPreprocessor>();
        scanner.scan_string(
            "#include <stdio.h> void main() { int x = 0; while (x != 999) x++; return x; }",
        );
    }

    #[test]
    fn test_string_literal() {
        let test = r#"#include "test.h""#;
        let mut scanner = DFAScanner::load_lexeme_set_scanner::<CPreprocessor>();
        scanner.scan_string(test);
    }
}
