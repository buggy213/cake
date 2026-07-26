//! The C preprocessor.
//!
//! The preprocessor is a [`TokenStream`] of [`CLexemes`]: it reads the source
//! one logical line at a time, on demand, and only opens a header when an
//! `#include` for it is actually reached.
//!
//! Text is never copied out of the source. A `PPToken` refers to its spelling
//! with a [`Span`] into one of the source files held by the preprocessor;
//! tokens that the preprocessor invents (by `#`, `##`, or string literal
//! concatenation) append their text to a scratch buffer which is indexed just
//! like a source file, so there is only ever one kind of token.

mod cond_expr;
mod lexer;
mod macros;

use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::path::PathBuf;
use std::rc::Rc;

use thiserror::Error;

use crate::platform::Platform;

use super::TokenStream;
use super::lexeme_sets::c_lexemes::CLexemes;
use super::lexeme_sets::c_preprocessor::CPreprocessor;
use super::lexemes::LexemeSet;
use super::table_scanner::{DFAScanner, ScannerResult};

use lexer::SourceReader;
use macros::MacroDef;

#[derive(Debug, Error)]
#[error("{file}:{line}: {message}")]
pub struct PreprocessingError {
    file: String,
    line: u32,
    message: String,
}

type Result<T> = std::result::Result<T, Box<PreprocessingError>>;

/// Where a token's spelling lives: a byte range within one of the
/// preprocessor's sources.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    src: u32,
    start: u32,
    end: u32,
}

/// Where a token came from, for diagnostics. This is not the same as its
/// [`Span`]: a token produced by `##` is spelled in the scratch buffer but
/// still points back at the line that produced it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Loc {
    src: u32,
    line: u32,
}

/// A preprocessing token (translation phase 3).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct PPToken {
    kind: CPreprocessor,
    span: Span,
    loc: Loc,
    /// whether whitespace preceded this token, needed to tell `#define f(x)`
    /// from `#define f (x)` and to space out the operand of `#`
    ws_before: bool,
}

/// Identifies a token handed to the parser: enough to recover both its text and
/// its source location.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TokenRef {
    span: Span,
    loc: Loc,
}

/// The state of one `#if` / `#elif` / `#else` / `#endif` group.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Cond {
    /// no branch has been taken yet, so a later `#elif` or `#else` may be
    Skipping,
    /// this branch is the one being processed
    Active,
    /// a branch was already taken, so the rest are skipped
    Done,
    /// the whole group sits inside skipped text
    Dead,
}

/// A source file being read, and the conditional nesting depth it started at.
struct OpenFile {
    reader: SourceReader,
    cond_depth: usize,
}

/// Index of the scratch buffer in `Preprocessor::sources`.
const SCRATCH: usize = 0;

const MAX_INCLUDE_DEPTH: usize = 200;

pub struct Preprocessor {
    platform: Platform,
    pp_scanner: DFAScanner,
    c_scanner: DFAScanner,

    /// text of every source read so far; `sources[SCRATCH]` holds the spelling
    /// of tokens the preprocessor invented
    sources: Vec<String>,
    /// path of each source, parallel to `sources`
    paths: Vec<PathBuf>,
    /// sources already read, so a second `#include` need not hit the disk
    by_path: HashMap<PathBuf, u32>,
    /// files that asked not to be included twice
    pragma_once: HashSet<PathBuf>,

    /// stack of files being read; the last one is current
    open_files: Vec<OpenFile>,
    /// scratch space for the line being read, reused between lines
    line_buf: Vec<PPToken>,

    macros: HashMap<Box<str>, Rc<MacroDef>>,
    /// macros currently being expanded, which must not expand again
    expanding: Vec<Rc<MacroDef>>,
    conditionals: Vec<Cond>,

    /// tokens carried over from the previous line, when a function-like macro
    /// invocation spans a line break
    pending: VecDeque<PPToken>,
    /// a string literal waiting to see whether another one follows it
    pending_string: Option<PendingString>,
    /// finished tokens, waiting to be handed to the parser
    out: VecDeque<(CLexemes, TokenRef)>,
    error: Option<Box<PreprocessingError>>,
}

/// Adjacent string literals are concatenated. The common case is a literal with
/// nothing after it, which is passed through without copying its text.
struct PendingString {
    first: PPToken,
    merged: Option<String>,
}

impl Preprocessor {
    pub fn new(file: PathBuf, contents: String, platform: Platform) -> Self {
        let mut preprocessor = Self {
            platform,
            pp_scanner: DFAScanner::load_lexeme_set_scanner::<CPreprocessor>(),
            c_scanner: DFAScanner::load_lexeme_set_scanner::<CLexemes>(),

            sources: vec![String::new()],
            paths: vec![PathBuf::from("<generated>")],
            by_path: HashMap::new(),
            pragma_once: HashSet::new(),

            open_files: Vec::new(),
            line_buf: Vec::new(),

            macros: HashMap::new(),
            expanding: Vec::new(),
            conditionals: Vec::new(),

            pending: VecDeque::new(),
            pending_string: None,
            out: VecDeque::new(),
            error: None,
        };

        let src = preprocessor.add_source(file, contents);
        preprocessor.open(src);
        preprocessor
    }

    /// The first error hit while preprocessing. The token stream stops at the
    /// point of the error, so a caller that ran the stream to completion should
    /// check this.
    pub fn take_error(&mut self) -> Option<Box<PreprocessingError>> {
        self.error.take()
    }

    /// The spelling of a token.
    fn spelling(&self, span: Span) -> &str {
        &self.sources[span.src as usize][span.start as usize..span.end as usize]
    }

    /// Creates a token whose spelling is in no source file, by appending it to
    /// the scratch buffer.
    fn synthesize(&mut self, text: &str, kind: CPreprocessor, loc: Loc, ws_before: bool) -> PPToken {
        let start = self.sources[SCRATCH].len();
        self.sources[SCRATCH].push_str(text);
        PPToken {
            kind,
            span: Span {
                src: SCRATCH as u32,
                start: start as u32,
                end: self.sources[SCRATCH].len() as u32,
            },
            loc,
            ws_before,
        }
    }

    fn error(&self, loc: Loc, message: String) -> Box<PreprocessingError> {
        Box::new(PreprocessingError {
            file: self.paths[loc.src as usize].display().to_string(),
            line: loc.line,
            message,
        })
    }

    fn add_source(&mut self, path: PathBuf, contents: String) -> u32 {
        let src = self.sources.len() as u32;
        self.sources.push(contents);
        self.paths.push(path.clone());
        self.by_path.insert(path, src);
        src
    }

    fn open(&mut self, src: u32) {
        self.open_files.push(OpenFile {
            reader: SourceReader::new(src),
            cond_depth: self.conditionals.len(),
        });
    } 

    /// Produces tokens until at least `n` are available or the input runs out.
    fn fill(&mut self, n: usize) -> bool {
        while self.out.len() < n && self.error.is_none() {
            match self.step() {
                Ok(true) => {}
                Ok(false) => break,
                Err(error) => self.error = Some(error),
            }
        }

        self.out.len() >= n
    }

    /// Reads and processes one logical line. Returns false once every open file
    /// has been read to the end.
    fn step(&mut self) -> Result<bool> {
        let mut line = std::mem::take(&mut self.line_buf);

        let read_a_line = match self.open_files.last_mut() {
            None => false,
            Some(open) => {
                let text = &self.sources[open.reader.src as usize];
                open.reader.next_line(&self.pp_scanner, text, &mut line)
            }
        };

        if !read_a_line {
            self.line_buf = line;
            self.close_file()?;
            return Ok(!self.open_files.is_empty());
        }

        let result = self.process_line(&line);
        self.line_buf = line;
        result?;

        Ok(true)
    }

    /// Finishes the file on top of the stack and pops it.
    fn close_file(&mut self) -> Result<()> {
        let Some(open) = self.open_files.pop() else {
            return Ok(());
        };

        // an unterminated macro invocation cannot be completed by another file
        self.flush_pending()?;

        if self.open_files.is_empty() {
            self.flush_string();
        }

        if self.conditionals.len() != open.cond_depth {
            self.conditionals.truncate(open.cond_depth);
            let loc = Loc {
                src: open.reader.src,
                line: 0,
            };
            return Err(self.error(loc, "unterminated #if in this file".into()));
        }

        Ok(())
    }

    fn process_line(&mut self, line: &[PPToken]) -> Result<()> {
        let Some(first) = line.first() else {
            return Ok(());
        };

        if first.kind == CPreprocessor::Hash {
            // a directive cannot appear inside a macro invocation, so anything
            // still pending is never going to be completed
            self.flush_pending()?;
            return self.directive(line);
        }

        if !self.active() {
            return Ok(());
        }

        self.pending.extend(line.iter().copied());
        self.expand_pending(true)
    }

    /// Expands everything carried over from previous lines. With `partial_ok`,
    /// an unfinished macro invocation is left pending for the next line.
    fn expand_pending(&mut self, partial_ok: bool) -> Result<()> {
        let mut pending = std::mem::take(&mut self.pending);
        let mut expanded = Vec::new();
        let result = self.expand(&mut pending, &mut expanded, partial_ok);
        self.pending = pending;
        result?;

        self.emit(&expanded)
    }

    fn flush_pending(&mut self) -> Result<()> {
        if self.pending.is_empty() {
            return Ok(());
        }
        self.expand_pending(false)
    }

    // -- conditional compilation --------------------------------------------

    /// Whether the text being read is included in the output.
    fn active(&self) -> bool {
        self.conditionals.iter().all(|c| *c == Cond::Active)
    }

    /// Whether the enclosing group is active, i.e. whether an `#elif` or
    /// `#else` at this level is worth looking at.
    fn parent_active(&self) -> bool {
        self.conditionals
            .iter()
            .rev()
            .skip(1)
            .all(|c| *c == Cond::Active)
    }

    fn push_conditional(&mut self, taken: bool) {
        let state = if !self.active() {
            Cond::Dead
        } else if taken {
            Cond::Active
        } else {
            Cond::Skipping
        };
        self.conditionals.push(state);
    }

    /// Handles `#elif` and `#else`, given whether their condition holds.
    fn branch(&mut self, taken: bool, loc: Loc, directive: &str) -> Result<()> {
        if self.conditionals.is_empty() {
            return Err(self.error(loc, format!("{directive} without #if")));
        }

        let parent_active = self.parent_active();
        let state = self.conditionals.last_mut().expect("checked above");
        if parent_active {
            *state = match *state {
                Cond::Skipping if taken => Cond::Active,
                Cond::Skipping => Cond::Skipping,
                Cond::Active | Cond::Done => Cond::Done,
                Cond::Dead => Cond::Dead,
            };
        }

        Ok(())
    }

    // -- directives ---------------------------------------------------------

    fn directive(&mut self, line: &[PPToken]) -> Result<()> {
        let hash = line[0];
        let Some(name_token) = line.get(1) else {
            return Ok(()); // a lone `#` is a null directive
        };
        let rest = &line[2..];

        // copy out the kind, so the borrow of the source text ends here
        let directive = match self.spelling(name_token.span) {
            "if" => Directive::If,
            "ifdef" => Directive::Ifdef,
            "ifndef" => Directive::Ifndef,
            "elif" => Directive::Elif,
            "else" => Directive::Else,
            "endif" => Directive::Endif,
            "include" => Directive::Include,
            "define" => Directive::Define,
            "undef" => Directive::Undef,
            "line" => Directive::Line,
            "error" => Directive::Error,
            "pragma" => Directive::Pragma,
            _ => Directive::Unknown,
        };

        // conditional directives are read even within skipped text, so that
        // nesting is tracked correctly; everything else is ignored there
        let conditional = matches!(
            directive,
            Directive::If
                | Directive::Ifdef
                | Directive::Ifndef
                | Directive::Elif
                | Directive::Else
                | Directive::Endif
        );
        if !conditional && !self.active() {
            return Ok(());
        }

        let loc = hash.loc;
        match directive {
            Directive::If => {
                // the condition of a group inside skipped text is not evaluated
                let taken = self.active() && self.eval_condition(rest, loc)?;
                self.push_conditional(taken);
            }
            Directive::Ifdef | Directive::Ifndef => {
                let taken = if self.active() {
                    self.macro_named(rest, loc, "#ifdef")? == (directive == Directive::Ifdef)
                } else {
                    false
                };
                self.push_conditional(taken);
            }
            Directive::Elif => {
                let taken = self.parent_active()
                    && self.conditionals.last() == Some(&Cond::Skipping)
                    && self.eval_condition(rest, loc)?;
                self.branch(taken, loc, "#elif")?;
            }
            Directive::Else => self.branch(true, loc, "#else")?,
            Directive::Endif => {
                if self.conditionals.pop().is_none() {
                    return Err(self.error(loc, "#endif without #if".into()));
                }
            }
            Directive::Include => self.include(rest, loc)?,
            Directive::Define => self.define(rest, loc)?,
            Directive::Undef => {
                let name = self.expect_identifier(rest, loc, "#undef")?;
                let name = self.spelling(name.span).to_string();
                self.macros.remove(name.as_str());
            }
            Directive::Line => {} // line control does not affect tokenization
            Directive::Error => {
                let mut message = String::new();
                for token in rest {
                    if !message.is_empty() {
                        message.push(' ');
                    }
                    message.push_str(self.spelling(token.span));
                }
                return Err(self.error(loc, format!("#error {message}")));
            }
            Directive::Pragma => {
                if rest.len() == 1 && self.spelling(rest[0].span) == "once" {
                    let path = self.paths[loc.src as usize].clone();
                    self.pragma_once.insert(path);
                }
            }
            Directive::Unknown => {
                let name = self.spelling(name_token.span).to_string();
                return Err(self.error(loc, format!("unknown directive #{name}")));
            }
        }

        Ok(())
    }

    fn expect_identifier(&self, line: &[PPToken], loc: Loc, directive: &str) -> Result<PPToken> {
        match line.first() {
            Some(token) if token.kind == CPreprocessor::Identifier => Ok(*token),
            _ => Err(self.error(loc, format!("{directive} expects a macro name"))),
        }
    }

    /// Whether the macro named by an `#ifdef` or `#ifndef` is defined.
    fn macro_named(&self, line: &[PPToken], loc: Loc, directive: &str) -> Result<bool> {
        let name = self.expect_identifier(line, loc, directive)?;
        Ok(self.macros.contains_key(self.spelling(name.span)))
    }

    fn define(&mut self, line: &[PPToken], loc: Loc) -> Result<()> {
        let name_token = self.expect_identifier(line, loc, "#define")?;
        let rest = &line[1..];

        // `#define f(x)` defines a function-like macro, but `#define f (x)`
        // defines an object-like one whose body happens to start with a paren
        let function_like =
            matches!(rest.first(), Some(t) if t.kind == CPreprocessor::LParen && !t.ws_before);

        let (params, varargs, body) = if function_like {
            let (params, varargs, body_start) = self.parse_params(rest, loc)?;
            (Some(params), varargs, &rest[body_start..])
        } else {
            (None, false, rest)
        };

        let name = self.spelling(name_token.span).to_string();
        let def = Rc::new(MacroDef {
            params,
            varargs,
            body: body.to_vec(),
        });

        // redefinition is allowed only if the two definitions are identical
        if let Some(previous) = self.macros.get(name.as_str()) {
            if !self.same_definition(previous, &def) {
                return Err(self.error(loc, format!("macro '{name}' redefined differently")));
            }
            return Ok(());
        }

        self.macros.insert(name.into_boxed_str(), def);
        Ok(())
    }

    /// Parses `( a, b, ... )` after a macro name, returning the parameters and
    /// the index just past the closing paren.
    fn parse_params(&self, line: &[PPToken], loc: Loc) -> Result<(Vec<Span>, bool, usize)> {
        let mut params: Vec<Span> = Vec::new();
        let mut varargs = false;

        // `line[0]` is the opening paren
        let mut i = 1;
        loop {
            let Some(token) = line.get(i) else {
                return Err(self.error(loc, "unterminated macro parameter list".into()));
            };
            i += 1;

            match token.kind {
                CPreprocessor::RParen => return Ok((params, varargs, i)),
                CPreprocessor::Ellipsis => varargs = true,
                CPreprocessor::Identifier if !varargs => {
                    let name = self.spelling(token.span);
                    if params.iter().any(|&p| self.spelling(p) == name) {
                        return Err(self.error(loc, format!("duplicate macro parameter '{name}'")));
                    }
                    params.push(token.span);
                }
                _ => {
                    let text = self.spelling(token.span).to_string();
                    return Err(
                        self.error(loc, format!("unexpected '{text}' in macro parameter list"))
                    );
                }
            }

            // parameters are separated by commas, and `...` must come last
            match line.get(i).map(|t| t.kind) {
                Some(CPreprocessor::Comma) if !varargs => i += 1,
                Some(CPreprocessor::RParen) => {}
                _ => return Err(self.error(loc, "expected ',' or ')' in macro parameters".into())),
            }
        }
    }

    /// Whether two definitions of the same macro agree, spelling for spelling.
    fn same_definition(&self, a: &MacroDef, b: &MacroDef) -> bool {
        let same_params = match (&a.params, &b.params) {
            (None, None) => true,
            (Some(x), Some(y)) => {
                x.len() == y.len()
                    && std::iter::zip(x, y).all(|(&s, &t)| self.spelling(s) == self.spelling(t))
            }
            _ => false,
        };
        let same_body = a.body.len() == b.body.len()
            && std::iter::zip(&a.body, &b.body)
                .all(|(s, t)| self.spelling(s.span) == self.spelling(t.span));

        same_params && a.varargs == b.varargs && same_body
    }

    fn include(&mut self, line: &[PPToken], loc: Loc) -> Result<()> {
        // the operand is macro expanded only if it is not already a header name
        let (name, system) = match self.header_name(line) {
            Some(header) => header,
            None => {
                let mut input: VecDeque<PPToken> = line.iter().copied().collect();
                let mut expanded = Vec::new();
                self.expand(&mut input, &mut expanded, false)?;
                self.header_name(&expanded)
                    .ok_or_else(|| self.error(loc, "invalid #include directive".into()))?
            }
        };

        let Some(path) = self.resolve_include(&name, system, loc) else {
            return Err(self.error(loc, format!("could not find include file '{name}'")));
        };

        if self.pragma_once.contains(&path) {
            return Ok(());
        }
        if self.open_files.len() >= MAX_INCLUDE_DEPTH {
            return Err(self.error(loc, format!("#include nested too deeply at '{name}'")));
        }

        let src = match self.by_path.get(&path) {
            Some(&src) => src,
            None => {
                let contents = fs::read_to_string(&path)
                    .map_err(|e| self.error(loc, format!("could not read '{name}': {e}")))?;
                self.add_source(path, contents)
            }
        };

        self.open(src);
        Ok(())
    }

    /// Reads `"header"` or `<header>` from the operand of `#include`.
    fn header_name(&self, line: &[PPToken]) -> Option<(String, bool)> {
        let first = line.first()?;

        if first.kind == CPreprocessor::StringLiteral && line.len() == 1 {
            let name = self.spelling(first.span).trim_matches('"');
            return Some((name.to_string(), false));
        }

        // `<stdio.h>` is several preprocessing tokens, so the name is put back
        // together from the spellings between the angle brackets
        if self.spelling(first.span) == "<" && self.spelling(line.last()?.span) == ">" {
            let mut name = String::new();
            for token in &line[1..line.len() - 1] {
                name.push_str(self.spelling(token.span));
            }
            return Some((name, true));
        }

        None
    }

    fn resolve_include(&self, name: &str, system: bool, loc: Loc) -> Option<PathBuf> {
        if system {
            return self.platform.resolve_system_include_path(name);
        }

        // a quoted include is looked for next to the file that asked for it
        let including_dir = self.paths[loc.src as usize].parent()?.to_path_buf();
        self.platform
            .resolve_normal_include_path(name, including_dir)
    }

    // -- handing tokens to the parser ---------------------------------------

    /// Converts finished preprocessing tokens into C tokens.
    fn emit(&mut self, tokens: &[PPToken]) -> Result<()> {
        for &token in tokens {
            let lexeme = self.to_clexeme(token)?;
            if lexeme == CLexemes::StringConst {
                self.push_string(token);
                continue;
            }

            self.flush_string();
            self.out.push_back((
                lexeme,
                TokenRef {
                    span: token.span,
                    loc: token.loc,
                },
            ));
        }

        Ok(())
    }

    /// Re-lexes a preprocessing token as a C token. Preprocessing tokens are
    /// coarser (`3.14f` is a single "preprocessing number"), so this is where a
    /// number becomes an integer or float constant and an identifier becomes a
    /// keyword.
    fn to_clexeme(&self, token: PPToken) -> Result<CLexemes> {
        let text = self.spelling(token.span);
        match self.c_scanner.next_word(text.as_bytes(), 0) {
            ScannerResult::Ok(word, action, _) if word.len() == text.len() => {
                Ok(CLexemes::from_id(action).expect("C DFA should be infallible"))
            }
            _ => Err(self.error(token.loc, format!("'{text}' is not a valid C token"))),
        }
    }

    fn push_string(&mut self, token: PPToken) {
        let Some(mut pending) = self.pending_string.take() else {
            self.pending_string = Some(PendingString {
                first: token,
                merged: None,
            });
            return;
        };

        let mut merged = pending
            .merged
            .take()
            .unwrap_or_else(|| self.spelling(pending.first.span).to_string());
        merged.pop(); // closing quote of the literal so far
        merged.push_str(&self.spelling(token.span)['"'.len_utf8()..]);

        pending.merged = Some(merged);
        self.pending_string = Some(pending);
    }

    fn flush_string(&mut self) {
        let Some(pending) = self.pending_string.take() else {
            return;
        };

        let span = match pending.merged {
            None => pending.first.span,
            Some(text) => {
                self.synthesize(
                    &text,
                    CPreprocessor::StringLiteral,
                    pending.first.loc,
                    pending.first.ws_before,
                )
                .span
            }
        };

        self.out.push_back((
            CLexemes::StringConst,
            TokenRef {
                span,
                loc: pending.first.loc,
            },
        ));
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Directive {
    If,
    Ifdef,
    Ifndef,
    Elif,
    Else,
    Endif,
    Include,
    Define,
    Undef,
    Line,
    Error,
    Pragma,
    Unknown,
}

impl TokenStream<CLexemes, TokenRef> for Preprocessor {
    fn eat(&mut self, lexeme: CLexemes) -> Option<TokenRef> {
        match self.peek() {
            Some((next, token)) if next == lexeme => {
                self.out.pop_front();
                Some(token)
            }
            _ => None,
        }
    }

    fn peek(&mut self) -> Option<(CLexemes, TokenRef)> {
        self.peek_n(0)
    }

    fn peek_n(&mut self, n: usize) -> Option<(CLexemes, TokenRef)> {
        if !self.fill(n + 1) {
            return None;
        }
        self.out.get(n).copied()
    }

    fn advance(&mut self) -> Option<(CLexemes, TokenRef)> {
        if !self.fill(1) {
            return None;
        }
        self.out.pop_front()
    }

    fn text(&self, token: TokenRef) -> &str {
        self.spelling(token.span)
    }
}

#[cfg(test)]
mod preprocessor_tests;
