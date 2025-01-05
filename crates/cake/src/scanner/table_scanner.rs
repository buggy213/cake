use std::collections::VecDeque;

use cake_re::DFATable;

use super::lexemes::LexemeSet;

pub struct DFAScanner {
    table: DFATable,
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

    pub fn new(table: DFATable) -> Self {
        Self { table }
    }

    // implements "maximal munch" lexing - always try to "eat" as many characters as possible to form next token
    // output: lexeme + action + next token cursor
    pub fn next_word<'a>(&self, input: &'a [u8], start_cursor: usize) -> (&'a str, i32, usize) {
        if start_cursor >= input.len() {
            return ("", -1, start_cursor);
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
            (slice, -1, cursor)
        } else {
            (slice, self.table.actions[state as usize], cursor)
        }
    }

    // scan entire input at once
    #[cfg(test)]
    pub(super) fn scan_string(&mut self, input: &str) {
        // TODO: add support for unicode
        assert!(input.is_ascii(), "only ascii supported");

        let mut cursor = 0;
        let input = input.as_bytes();

        while cursor < input.len() {
            let (lexeme, action, next_cursor) = self.next_word(input, cursor);
            if action == -1 {
                panic!("failed to tokenize string");
            } else {
                println!("'{}', {}", lexeme, action);
                cursor = next_cursor;
            }
        }
    }
}
