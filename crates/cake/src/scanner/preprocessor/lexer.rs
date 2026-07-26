//! Translation phases 1-3: read one logical line of preprocessing tokens at a
//! time from a source file.
//!
//! Line splices (`\` at end of line) are dropped and comments are treated as
//! whitespace. Tokens refer to their spelling by span, so a `PPToken` is a
//! small `Copy` value and lexing allocates nothing per token.

use crate::scanner::lexeme_sets::c_preprocessor::CPreprocessor;
use crate::scanner::lexemes::LexemeSet;
use crate::scanner::table_scanner::{DFAScanner, ScannerResult};

use super::{Loc, PPToken, Span};

/// A source file being read by the preprocessor. Files are stacked, so an
/// `#include` suspends the current reader and resumes it once the header runs
/// out of lines.
pub(super) struct SourceReader {
    /// index into `Preprocessor::sources` of the text being read
    pub(super) src: u32,
    cursor: usize,
    line: u32,
}

impl SourceReader {
    pub(super) fn new(src: u32) -> SourceReader {
        SourceReader {
            src,
            cursor: 0,
            line: 1,
        }
    }

    /// Reads the next logical line into `out` (cleared first). Returns false at
    /// end of file. An empty line yields `true` with nothing in `out`.
    pub(super) fn next_line(
        &mut self,
        scanner: &DFAScanner,
        text: &str,
        out: &mut Vec<PPToken>,
    ) -> bool {
        out.clear();
        if self.cursor >= text.len() {
            return false;
        }

        let mut ws_before = false;
        while self.cursor < text.len() {
            let (word, kind, next_cursor) = match scanner.next_word(text.as_bytes(), self.cursor) {
                ScannerResult::EndOfInput => break,
                ScannerResult::Failed => {
                    // `Other: .` matches any single character, so this only
                    // happens on non-ascii input; skip the byte and carry on.
                    self.cursor += 1;
                    continue;
                }
                ScannerResult::Ok(word, action, next_cursor) => {
                    let kind = CPreprocessor::from_id(action)
                        .expect("C preprocessor DFA should be infallible");
                    (word, kind, next_cursor)
                }
            };

            let start = self.cursor;
            self.cursor = next_cursor;

            match kind {
                // a newline ends the logical line
                CPreprocessor::Newline => {
                    self.line += 1;
                    return true;
                }
                // a splice glues this line to the next one
                CPreprocessor::Splice => {
                    self.line += 1;
                }
                CPreprocessor::Whitespace
                | CPreprocessor::WeirdWhitespace
                | CPreprocessor::Comment => {
                    ws_before = true;
                }
                CPreprocessor::MultilineComment => {
                    self.line += word.matches('\n').count() as u32;
                    ws_before = true;
                }
                // stray carriage returns (CRLF input on windows) are whitespace
                CPreprocessor::Other if word == "\r" => {
                    ws_before = true;
                }
                _ => {
                    out.push(PPToken {
                        kind,
                        span: Span {
                            src: self.src,
                            start: start as u32,
                            end: next_cursor as u32,
                        },
                        loc: Loc {
                            src: self.src,
                            line: self.line,
                        },
                        ws_before,
                    });
                    ws_before = false;
                }
            }
        }

        // last line of a file with no trailing newline
        true
    }
}
