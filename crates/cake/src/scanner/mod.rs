use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::fs;
use std::path::PathBuf;
use std::rc::Rc;

use self::lexeme_sets::c_lexemes::CLexemes;
use cake_lex::DFAScanner;
use cake_lex::LexemeSet;
use lexeme_sets::c_preprocessor::CPreprocessor;

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
pub struct RawTokenStream<'a, T>
where
    T: LexemeSet,
{
    cursor: usize,
    scanner: DFAScanner,
    source: &'a [u8],

    buffer: VecDeque<(T, &'a str, usize)>,
}

impl<'a, T: LexemeSet> RawTokenStream<'a, T> {
    pub fn new(scanner: DFAScanner, source: &'a [u8]) -> RawTokenStream<'a, T> {
        let retval = RawTokenStream {
            cursor: 0,
            scanner,
            source,

            buffer: VecDeque::new(),
        };

        retval
    }

    fn refill_buffer(&mut self) {
        self.refill_buffer_to_size(BUFFER_SIZE);
    }

    fn refill_buffer_to_size(&mut self, size: usize) {
        while self.buffer.len() < size {
            let (lexeme, action, next_cursor) = self.scanner.next_word(self.source, self.cursor);

            if action == -1 {
                break;
            }
            // if action as u32 == CLexemes::Whitespace.to_id() {
            //     self.cursor = next_cursor;
            //     continue;
            // }
            let l = T::from_id(action as u32).expect("invalid action (is the scanner compatible?)");
            self.buffer.push_back((l, lexeme, self.cursor));
            self.cursor = next_cursor;
        }
    }
}

// TODO: optimize this
impl<'a, T> TokenStream<T> for RawTokenStream<'a, T>
where
    T: LexemeSet,
{
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
        self.refill_buffer_to_size(n + 1);
        self.buffer.get(n).copied()
    }
}

// specialized implementation for C lexemes
// implements preprocessing logic
pub struct Preprocessor<'state> {
    preprocess_scanner: DFAScanner,
    main_scanner: DFAScanner,

    state: PreprocessorState,
    current_line: PreprocessorLine<'state>,
}

fn make_pp<'s>() -> Preprocessor<'s> {
    let mut k = Preprocessor {
        preprocess_scanner: todo!(),
        main_scanner: todo!(),
        state: todo!(),
        current_line: todo!(),
    };

    k.current_line = k.state.get_line().unwrap();
    k
}

struct PreprocessorState {
    sources_map: HashMap<PathBuf, SourceFileDescriptor>,
    sources: Vec<Box<str>>, // Box<str> is preferable, since no need to mutate source files (?)

    cursor_stack: Vec<SourceCursor>,
    macros: HashMap<String, PreprocessorMacro>,
}

struct PreprocessorLine<'state> {
    line: &'state str,
    line_num: usize,
}

struct FunctionMacroInvocation {}

struct SourceFileDescriptor {
    // header guard optimization - #pragma once
    header_guard: bool,
    source_idx: usize,
}

#[derive(Debug, Clone)]
struct SourceCursor {
    filepath: PathBuf,
    file_idx: usize,
    cursor: usize,
    line: usize,
}

enum PreprocessorMacro {
    ObjectMacro { replacement: Vec<PreprocessorState> },
    FunctionMacro { varargs: bool },
}

impl PreprocessorState {
    pub fn new(file: PathBuf, contents: String) -> Self {
        let sources = vec![contents.into_boxed_str()];
        let mut sources_map = HashMap::new();
        sources_map.insert(
            file.clone(),
            SourceFileDescriptor {
                header_guard: false,
                source_idx: 0,
            },
        );

        Self {
            sources_map,
            sources,
            cursor_stack: vec![SourceCursor {
                filepath: file,
                file_idx: 0,
                cursor: 0,
                line: 1,
            }],

            macros: HashMap::new(),
        }
    }

    fn include_file(&mut self, path: PathBuf) {
        // 1. check for include guard
        if let Some(src_file) = self.sources_map.get(&path) {
            if src_file.header_guard {
                return;
            }
        }

        // 2. read file
        let contents = fs::read_to_string(&path).expect("err while opening file");

        // 3. update cursor stack and opened src file map
        let idx = self.sources.len();
        self.sources.push(contents.into_boxed_str());
        let src_descriptor = SourceFileDescriptor {
            header_guard: false,
            source_idx: idx,
        };
        self.sources_map.insert(path.clone(), src_descriptor);
        self.cursor_stack.push(SourceCursor {
            filepath: path,
            file_idx: idx,
            cursor: 0,
            line: 1,
        });
    }

    fn get_current_src_str(&self) -> &str {
        self.sources[self.current_cursor().file_idx].as_ref()
    }

    fn get_remaining_src_str(&self) -> &str {
        let current_src_str = self.get_current_src_str();
        &current_src_str[self.current_cursor().cursor..]
    }

    // invariant: cursor stack must never be fully empty until compiler is finished
    fn current_cursor(&self) -> &SourceCursor {
        self.cursor_stack.last().unwrap()
    }

    fn current_cursor_mut(&mut self) -> &mut SourceCursor {
        self.cursor_stack.last_mut().unwrap()
    }

    // line-by-line processing could lead to super pathological cases (e.g. gigantic single line macros)
    // or if someone decides to put their entire source file in one big line
    // but this is much simpler
    fn get_line(&mut self) -> Option<PreprocessorLine<'_>> {
        // if current file is empty, pop cursor stack
        let mut remaining_src_str = self.get_remaining_src_str();
        while remaining_src_str.is_empty() {
            if let None = self.cursor_stack.pop() {
                return None;
            } else {
                remaining_src_str = self.get_remaining_src_str();
            }
        }

        let mut prev_char: char = '\n';
        let mut physical_lines = 0;
        let logical_line_break = remaining_src_str
            .find(|c| {
                if prev_char != '\\' && c == '\n' {
                    physical_lines += 1;
                    true
                } else {
                    physical_lines += if c == '\n' { 1 } else { 0 };
                    prev_char = c;
                    false
                }
            })
            // include newline within line
            .map(|p| p + '\n'.len_utf8());

        let old_cursor = self.current_cursor().cursor;
        let old_line_num = self.current_cursor().line;
        if let Some(line_break) = logical_line_break {
            self.current_cursor_mut().line += physical_lines;
            self.current_cursor_mut().cursor = line_break;
        } else {
            self.current_cursor_mut().line += physical_lines;
            self.current_cursor_mut().cursor = self.get_current_src_str().len();
        }

        let src_str = self.get_current_src_str();
        let new_cursor = self.current_cursor().cursor;
        let pp_line = PreprocessorLine {
            line: &src_str[old_cursor..new_cursor],
            line_num: old_line_num,
        };

        Some(pp_line)
    }
}
