use std::collections::VecDeque;

use self::{lexemes::LexemeSet, table_scanner::DFAScanner};

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
    fn peek(&self) -> Option<(T, &str)>;
    fn advance(&mut self) -> Option<(T, String)>;
}

pub struct VecTokenStream<T: LexemeSet> {
    tokens: VecDeque<(T, String)>
}

impl<T: LexemeSet> TokenStream<T> for VecTokenStream<T> {
    fn eat(&mut self, lexeme: T) -> bool {
        match self.tokens.front() {
            Some((t, _)) if *t == lexeme => {
                self.tokens.pop_front();
                true
            },
            _ => false,
        }
    }

    fn peek(&self) -> Option<(T, &str)> {
        self.tokens.front()
            .map(|(a, b)| (*a, b.as_str()))
    }

    fn advance(&mut self) -> Option<(T, String)> {
        self.tokens.pop_front()
    }
}

// TODO: processed token stream to implement preprocessor
// don't think you have to do a separate pass for preprocessing
pub struct RawTokenStream<'a, T> where T: LexemeSet {
    cursor: usize,
    scanner: DFAScanner,
    source: &'a [u8],

    memo: Option<(T, String)>
}

impl <'a, T: LexemeSet> RawTokenStream<'a, T> {
    fn new(scanner: DFAScanner, source: &'a [u8]) -> RawTokenStream<'a, T> {
        let mut retval = RawTokenStream {
            cursor: 0,
            scanner,
            source,
            memo: None,
        };

        retval.refill_memo();
        retval
    }

    fn refill_memo(&mut self) {
        let (token, lexeme, next_cursor) = 
            self.scanner.next_word(self.source, self.cursor);
        
        if lexeme == -1 {
            self.memo = None;
        }

        let lexeme = T::from_id(lexeme as u32);
        self.cursor = next_cursor;
        self.memo = Some((lexeme, token))
    }
}

// TODO: optimize this
impl<'a, T> TokenStream<T> for RawTokenStream<'a, T> where T: LexemeSet {
    fn eat(&mut self, expected_lexeme: T) -> bool {
        let matched = match self.memo.as_ref() {
            Some((lexeme, _)) if *lexeme == expected_lexeme => true,
            _ => false,
        };

        self.refill_memo();
        matched
    }

    fn peek(&self) -> Option<(T, &str)> {
        self.memo.as_ref().map(|(a, b)| (*a, b.as_str()))
    }

    fn advance(&mut self) -> Option<(T, String)> {
        let old_memo = self.memo.take();
        self.refill_memo();
        old_memo
    }


}