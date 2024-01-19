use std::collections::VecDeque;

use crate::scanner::regex::Regex;

use super::{fa::FA, alphabet::AsciiChar, lexemes::LexemeSet};

// states = rows, 1 additional error state
// characters = columns
// -1 = invalid transition
struct DFATable {
    data: Vec<usize>,
    actions: Vec<i32>,
    states: usize,
    inputs: usize
}

pub struct DFAScanner {
    table: DFATable,
    initial_state: usize,
    state: usize
}

impl DFATable {
    fn get_next_state(&self, current_state: usize, ch: AsciiChar) -> usize {
        self.data[current_state * self.inputs + ch as usize]
    }

    fn from_ascii_dfa(dfa: &FA<AsciiChar>) -> DFATable {
        let num_states: usize = dfa.nodes.len() + 1;
        let num_inputs: usize = 128;
        let mut data: Vec<usize> = Vec::with_capacity(num_states * num_inputs);
        data.resize(num_states * num_inputs, num_states - 1);

        // fill table
        for (i, state) in dfa.nodes.iter().enumerate() {
            for (label, next) in &state.transitions {
                let label = label.expect("DFA should not have epsilon transitions");
                let next = *next;
                data[i * num_inputs + label as usize] = next;
            }
        }

        let mut actions = dfa.actions.as_ref()
            .expect("using DFA for table requires associated lexing actions")
            .clone();
        actions.push(-1); // add implicit error state
        
        DFATable { 
            data, 
            actions, 
            states: num_states, 
            inputs: num_inputs 
        }
    } 
}

impl DFAScanner {
    pub fn from_ascii_dfa(dfa: &FA<AsciiChar>) -> DFAScanner {
        DFAScanner {
            table: DFATable::from_ascii_dfa(dfa),
            initial_state: dfa.initial_state,
            state: dfa.initial_state,
        }
    }

    // convenience function to compile lexeme regexes to NFA, then DFA, then optimize, then construct scanner
    pub fn from_lexeme_set<T: LexemeSet>() -> DFAScanner {
        let lexemes: Vec<Regex<_>> = T::iter()
            .map(|x| Regex::from_str(x.pattern()).expect("failed to parse regex"))
            .collect();

        let nfa = FA::combine_res(&lexemes);
        let dfa = FA::dfa_from_nfa(&nfa);
        let dfa = FA::minimize_dfa(&dfa, true);
        
        let scanner = DFAScanner::from_ascii_dfa(&dfa);
        scanner
    }

    // output: lexeme + action + next token cursor
    pub fn next_word<'a>(&mut self, input: &'a [u8], start_cursor: usize) -> (&'a str, i32, usize) {
        if start_cursor >= input.len() {
            return ("", -1, start_cursor);
        }

        let mut cursor = start_cursor;
        let mut failed: Vec<bool> = Vec::new();
        failed.resize(input.len() * self.table.states, false);

        self.state = self.initial_state;
        let mut stack: VecDeque<usize> = VecDeque::new();
        stack.push_back(usize::MAX); // MAX as sentinel
        
        let mut lexeme = String::new();
        
        while self.state != self.table.states - 1 {
            if cursor >= input.len() {
                break; // hit EOF
            }
            let c = AsciiChar::from_u8(input[cursor]);
            cursor += 1;

            // println!("c = '{:?}'", c);
            lexeme.push(char::from_u32(c as u32).expect("character is somehow invalid"));
            if failed[self.state * input.len() + cursor] {
                break;
            }

            stack.push_back(self.state);
            let next_state = self.table.get_next_state(self.state, c);
            self.state = next_state;

            if self.table.actions[self.state] != -1 {
                // accept state
                // println!("accepted '{}'", lexeme);
                stack.clear();
            }
        }

        while self.state != usize::MAX && self.table.actions[self.state] == -1 {
            self.state = stack.pop_back().expect("should not be empty");
            lexeme.pop();

            cursor -= 1;
        }

        // earlier passes ensured its valid utf8 (ascii)
        let slice = unsafe { std::str::from_utf8_unchecked(&input[start_cursor..cursor]) };
        if self.state == usize::MAX {
            (slice, -1, cursor)
        }
        else {
            (slice, self.table.actions[self.state], cursor)
        }
    }

    // scan entire input at once
    pub fn scan_string(&mut self, input: &str) {
        // TODO: add support for unicode
        assert!(input.is_ascii(), "only ascii supported");
        
        // TODO: add preprocessor
        let mut cursor = 0;
        let input = input.as_bytes();

        while cursor < input.len() {
            let (lexeme, action, next_cursor) = self.next_word(input, cursor);
            if action == -1 {
                panic!("failed to tokenize string");
            }
            else {
                println!("'{}', {}", lexeme, action);
                cursor = next_cursor;
            }
        }
    }
}

