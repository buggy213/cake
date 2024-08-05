use std::collections::VecDeque;

use self::{lexemes::LexemeSet, table_scanner::DFAScanner, lexeme_sets::c_lexemes::CLexemes};

pub mod alphabet;
pub mod table_scanner;
pub mod regex;
pub mod lexemes;
pub mod fa;

// autogenerated lexeme sets
pub mod lexeme_sets;

// TODO: think about what interface should be for backtracking due to 
// parsing troubles
pub trait TokenStream<T: LexemeSet> {
    fn eat(&mut self, lexeme: T) -> bool;
    fn peek(&mut self) -> Option<(T, &str, usize)>;
    fn peekn(&mut self, n: usize) -> Option<(T, &str, usize)>;
    fn advance(&mut self) -> Option<(T, &str, usize)>;

    fn rollback(&mut self, target: usize);
    fn get_location(&self) -> usize;
}

// TODO: processed token stream to implement preprocessor
// don't think you have to do a separate pass for preprocessing
const BUFFER_SIZE: usize = 10;
pub struct RawTokenStream<'a, T> where T: LexemeSet {
    cursor: usize,
    scanner: DFAScanner,
    source: &'a [u8],

    buffer: VecDeque<(T, &'a str, usize)>,
    index: usize
}

impl <'a, T: LexemeSet> RawTokenStream<'a, T> {
    pub fn new(scanner: DFAScanner, source: &'a [u8]) -> RawTokenStream<'a, T> {
        let retval = RawTokenStream {
            cursor: 0,
            scanner,
            source,

            buffer: VecDeque::new(),
            index: 0
        };

        retval
    }

    fn refill_buffer(&mut self) {
        self.refill_buffer_to_size(BUFFER_SIZE);
    }

    fn refill_buffer_to_size(&mut self, size: usize) {
        while self.buffer.len() < size {
            let (lexeme, action, next_cursor) = 
                self.scanner.next_word(self.source, self.cursor);

            if action == -1 {
                break;
            }
            if action as u32 == CLexemes::Whitespace.to_id() {
                self.cursor = next_cursor;
                continue;
            } 
            let l = T::from_id(action as u32).expect("invalid action (is the scanner compatible?)");
            self.buffer.push_back((l, lexeme, self.cursor));
            self.cursor = next_cursor;
        }
    }
}



// TODO: optimize this
impl<'a, T> TokenStream<T> for RawTokenStream<'a, T> where T: LexemeSet {
    fn eat(&mut self, expected_lexeme: T) -> bool {
        self.refill_buffer();
        let matched = match self.buffer.pop_front() {
            Some((lexeme, _, _)) if lexeme == expected_lexeme => true,
            _ => false,
        };

        matched
    }

    fn peek(&mut self) -> Option<(T, &str, usize)> {
        self.refill_buffer();
        self.buffer.front().copied()
    }

    fn advance(&mut self) -> Option<(T, &str, usize)> {
        self.refill_buffer();
        let old_memo = self.buffer.pop_front();
        old_memo
    }

    fn rollback(&mut self, target: usize) {
        self.cursor = target;
        self.buffer.clear();
    }

    fn get_location(&self) -> usize {
        self.cursor
    }

    fn peekn(&mut self, n: usize) -> Option<(T, &str, usize)> {
        self.refill_buffer_to_size(n+1);
        self.buffer.get(n).copied()
    }


}