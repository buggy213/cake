use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::fs;
use std::iter;
use std::path::PathBuf;
use std::rc::Rc;

use crate::parser::hand_parser::ParserState;
use crate::parser::hand_parser::parse_expr;
use crate::platform::Platform;
use crate::semantics::constexpr::preprocessor_constant_eval;

use super::TokenStream;
use super::lexeme_sets::c_lexemes::CLexemes;
use super::lexeme_sets::c_preprocessor::CPreprocessor;
use super::lexemes::LexemeSet;
use super::table_scanner::DFAScanner;

use bumpalo::Bump;

// specialized implementation for C lexemes
// implements preprocessing logic
struct Preprocessor {
    preprocess_scanner: DFAScanner,
    main_scanner: DFAScanner,
    platform: Platform,

    sources_map: HashMap<PathBuf, SourceFileDescriptor>,
    sources: Vec<Box<str>>, // Box<str> is preferable, since no need to mutate source files (?)

    cursor_stack: Vec<SourceCursor>,
    macros: HashMap<String, PreprocessorMacro>,

    conditional_stack: Vec<ConditionalState>,
    pp_token_line_buffer: VecDeque<PreprocessorToken>,
    macro_invocation_stack: Vec<MacroInvocation>,
    str_literal_concat_buffer: Option<StringLiteral>,
    clexeme_buffer: VecDeque<CToken>,

    strings_buffer: Bump,
}

#[derive(Clone, Copy, Debug)]
struct TokenSpan {
    file_idx: usize,
    left: usize,
    right: usize,
}

impl TokenSpan {
    fn new(file_idx: usize, left: usize, right: usize) -> Self {
        Self {
            file_idx,
            left,
            right,
        }
    }
}

// invariant: span always refers to current source
// should be upheld as long as we only get_line once pp_token_buffer is empty
#[derive(Clone, Copy, Debug)]
struct PreprocessorToken {
    token: CPreprocessor,
    span: TokenSpan,
    whitespace_left: bool,
}

#[derive(Clone, Debug)]
enum MacroPreprocessorToken {
    FromSource(PreprocessorToken),
    Concatenated(CPreprocessor, String, TokenSpan),
}

impl MacroPreprocessorToken {
    fn get_text<'a, 'b>(&'a self, preprocessor_state: &'b Preprocessor) -> &'a str
    where
        'b: 'a, // 'b is a subtype of 'a, i.e. it lives longer (as it should, since all of the string data ultimately lives inside of it)
    {
        match self {
            MacroPreprocessorToken::FromSource(pp_token) => {
                preprocessor_state.get_text(pp_token.span)
            }
            MacroPreprocessorToken::Concatenated(_, string, _) => &string,
        }
    }

    fn get_cow(self, preprocessor_state: &Preprocessor) -> Cow<str> {
        match self {
            MacroPreprocessorToken::FromSource(pp_token) => {
                Cow::Borrowed(preprocessor_state.get_text(pp_token.span))
            }
            MacroPreprocessorToken::Concatenated(_, string, _) => Cow::Owned(string),
        }
    }

    fn token(&self) -> CPreprocessor {
        match self {
            MacroPreprocessorToken::FromSource(pp_token) => pp_token.token,
            MacroPreprocessorToken::Concatenated(tok, _, _) => *tok,
        }
    }

    fn span(&self) -> TokenSpan {
        match self {
            MacroPreprocessorToken::FromSource(pp_token) => pp_token.span,
            MacroPreprocessorToken::Concatenated(_, _, span) => *span,
        }
    }
}

#[derive(Clone, Debug)]
enum CToken {
    FromSource {
        token: CLexemes,
        span: TokenSpan,
    },
    // this is for concatenated tokens (## operator) and for string literal concatenation (translation phase 6)
    Owned {
        token: CLexemes,
        string: String,
        span: TokenSpan,
    },
}

impl CToken {
    fn token(&self) -> CLexemes {
        match self {
            CToken::FromSource { token, .. } => *token,
            CToken::Owned { token, .. } => *token,
        }
    }

    fn text<'a, 'b>(&'a self, preprocessor_state: &'b Preprocessor) -> &'a str
    where
        'b: 'a,
    {
        match self {
            CToken::FromSource { span, .. } => preprocessor_state.get_text(*span),
            CToken::Owned { string, .. } => &string,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ConditionalState {
    NoneTaken,
    Active,
    SomeTaken,
}

#[derive(Debug)]
struct FunctionMacroInvocation {
    name: MacroPreprocessorToken,
    macro_ref: Rc<FunctionMacro>,
    arguments: Vec<VecDeque<MacroPreprocessorToken>>,
    paren_count: usize,
}

impl FunctionMacroInvocation {
    fn new(name: MacroPreprocessorToken, macro_ref: Rc<FunctionMacro>) -> Self {
        let arguments = if macro_ref.arguments.len() > 0 {
            let a = vec![Default::default()];
            a
        } else {
            Vec::new()
        };

        Self {
            name,
            macro_ref,
            arguments,
            paren_count: 0,
        }
    }

    fn add_token(&mut self, pp_token: MacroPreprocessorToken) -> bool {
        if pp_token.token() == CPreprocessor::LParen {
            self.paren_count += 1;
            return false;
        }
        if pp_token.token() == CPreprocessor::RParen {
            self.paren_count -= 1;
            if self.paren_count == 0 {
                dbg!(&self.arguments);
                return true;
            }
        }

        match (self.paren_count, &pp_token) {
            (1, token) if token.token() == CPreprocessor::Comma => {
                if self.macro_ref.arguments.len() > self.arguments.len() {
                    self.arguments.push(Default::default());
                } else if self.macro_ref.arguments.len() == self.arguments.len() {
                    if self.macro_ref.varargs {
                        self.arguments.push(Default::default());
                    } else {
                        eprintln!("unexpected macro argument");
                        todo!()
                    }
                } else {
                    self.arguments.last_mut().unwrap().push_back(pp_token)
                }
            }
            (_, _) => match self.arguments.last_mut() {
                None => {
                    if self.macro_ref.arguments.len() > 0 || self.macro_ref.varargs {
                        let mut first_arg = VecDeque::new();
                        first_arg.push_back(pp_token);
                        self.arguments.push(first_arg);
                    } else {
                        eprintln!("unexpected macro argument");
                        todo!()
                    }
                }
                Some(arg) => arg.push_back(pp_token),
            },
        }

        false
    }
}

#[derive(Debug)]
enum MacroInvocation {
    FunctionMacroInvocation(FunctionMacroInvocation),
    ObjectMacroInvocation {
        name: MacroPreprocessorToken,
        macro_ref: Rc<ObjectMacro>,
    },
}

enum StringLiteral {
    Single(TokenSpan),
    Concatenated(String, TokenSpan),
}

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

#[derive(Debug)]
struct ObjectMacro {
    replacement_list: VecDeque<PreprocessorToken>,
}

#[derive(Debug)]
struct FunctionMacro {
    arguments: Vec<String>,
    replacement_list: VecDeque<PreprocessorToken>,
    varargs: bool,
}

enum PreprocessorMacro {
    ObjectMacro(Rc<ObjectMacro>),
    FunctionMacro(Rc<FunctionMacro>),
}

impl Preprocessor {
    pub fn new(file: PathBuf, contents: String, platform: Platform) -> Self {
        let preprocess_scanner = DFAScanner::load_lexeme_set_scanner::<CPreprocessor>();
        let main_scanner = DFAScanner::load_lexeme_set_scanner::<CLexemes>();

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
            preprocess_scanner,
            main_scanner,
            platform,

            sources_map,
            sources,
            cursor_stack: vec![SourceCursor {
                filepath: file,
                file_idx: 0,
                cursor: 0,
                line: 1,
            }],

            macros: HashMap::new(),
            pp_token_line_buffer: VecDeque::new(),
            macro_invocation_stack: Vec::new(),
            clexeme_buffer: VecDeque::new(),
            str_literal_concat_buffer: None,
            conditional_stack: Vec::new(),

            strings_buffer: Bump::new(),
        }
    }

    fn include_file(&mut self, file: &str, normal: bool) {
        // 1. resolve path
        let file_path = if normal {
            let current_dir = self
                .current_cursor()
                .filepath
                .parent()
                .unwrap()
                .to_path_buf();
            self.platform.resolve_normal_include_path(file, current_dir)
        } else {
            self.platform.resolve_system_include_path(file)
        }
        .expect("failed to resolve include");

        // 2. check for include guard / if file is already open
        if let Some(src_file) = self.sources_map.get(&file_path) {
            if src_file.header_guard {
                return;
            } else {
                let new_cursor = SourceCursor {
                    filepath: file_path,
                    file_idx: src_file.source_idx,
                    cursor: 0,
                    line: 1,
                };
                self.push_cursor(new_cursor);

                return;
            }
        }

        // 3. read file
        let contents = fs::read_to_string(&file_path).expect("err while opening file");

        // 4. update cursor stack and opened src file map
        let idx = self.sources.len();
        self.sources.push(contents.into_boxed_str());
        let src_descriptor = SourceFileDescriptor {
            header_guard: false,
            source_idx: idx,
        };
        self.sources_map.insert(file_path.clone(), src_descriptor);
        let new_cursor = SourceCursor {
            filepath: file_path,
            file_idx: idx,
            cursor: 0,
            line: 1,
        };
        self.push_cursor(new_cursor);
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

    fn get_text(&self, span: TokenSpan) -> &str {
        &self.sources[span.file_idx][span.left..span.right]
    }

    fn current_conditional_state(&self) -> Option<ConditionalState> {
        self.conditional_stack.last().copied()
    }

    fn push_cursor(&mut self, cursor: SourceCursor) {
        self.cursor_stack.push(cursor);
    }

    fn pop_cursor(&mut self) {
        self.cursor_stack.pop();
    }

    // line-by-line processing could lead to super pathological cases (e.g. gigantic single line macros)
    // or if someone decides to put their entire source file in one big line
    // but this is much simpler
    fn process_line(&mut self) -> bool {
        // if current file is empty, pop cursor stack
        let mut remaining_src_str = self.get_remaining_src_str();
        while remaining_src_str.is_empty() {
            if self.cursor_stack.len() >= 2 {
                self.pop_cursor();
                remaining_src_str = self.get_remaining_src_str();
            } else {
                return false;
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
            // include newline within logical line (if this line doesn't include final line of file)
            .map(|p| p + '\n'.len_utf8());

        let old_cursor = self.current_cursor().cursor;
        if let Some(line_break) = logical_line_break {
            self.current_cursor_mut().line += physical_lines;
            self.current_cursor_mut().cursor += line_break;
        } else {
            self.current_cursor_mut().line += physical_lines;
            self.current_cursor_mut().cursor = self.get_current_src_str().len();
        }

        fn get_next_token(
            scanner: &DFAScanner,
            src_str: &str,
            start_cursor: usize,
        ) -> Option<(CPreprocessor, usize, usize, bool)> {
            let mut cursor = start_cursor;
            let mut prev_whitespace = false;
            loop {
                let (action, next_cursor) = match scanner.next_word(src_str.as_bytes(), cursor) {
                    crate::scanner::table_scanner::ScannerResult::EndOfInput
                    | crate::scanner::table_scanner::ScannerResult::Failed => return None,
                    crate::scanner::table_scanner::ScannerResult::Ok(_, action, next_cursor) => {
                        (action, next_cursor)
                    }
                };

                let token = CPreprocessor::from_id(action as u32)
                    .expect("C preprocessor DFA should be infallible");

                if let CPreprocessor::Newline = token {
                    return None;
                }
                if let CPreprocessor::Splice = token {
                    cursor = next_cursor;
                    continue;
                }
                if let CPreprocessor::Whitespace = token {
                    prev_whitespace = true;
                    cursor = next_cursor;
                    continue;
                }

                // comments generally = whitespace, but //-style comments always continue until end of (logical) line
                // and will continue through line splices
                if let CPreprocessor::Comment = token {
                    return None;
                }
                if let CPreprocessor::MultilineComment = token {
                    prev_whitespace = true;
                    cursor = next_cursor;
                    continue;
                }

                return Some((token, cursor, next_cursor, prev_whitespace));
            }
        }

        // check for conditional compilation
        if let Some(ConditionalState::NoneTaken) | Some(ConditionalState::SomeTaken) =
            self.current_conditional_state()
        {
            // only process enough of line to determine if this is a conditional directive (at the same "level", i.e. else / elif / endif only)
            // only endif if there is already some branch taken
            let cursor = old_cursor;
            let src_str = self.get_current_src_str();
            let first = get_next_token(&self.preprocess_scanner, src_str, cursor);
            let (first_token, _, second_token_start, _) = match first {
                Some(x) => x,
                None => return true,
            };

            if first_token != CPreprocessor::Hash {
                return true;
            }

            let second = get_next_token(&self.preprocess_scanner, src_str, second_token_start);
            let (second_token, left, right, _) = match second {
                Some(x) => x,
                None => return true,
            };

            let token_span = TokenSpan::new(self.current_cursor().file_idx, left, right);
            let keywords =
                if let Some(ConditionalState::NoneTaken) = self.current_conditional_state() {
                    ["else", "elif", "endif"].as_slice()
                } else {
                    ["endif"].as_slice()
                };

            if second_token != CPreprocessor::Identifier
                || keywords.iter().all(|s| *s != self.get_text(token_span))
            {
                return true;
            }

            // fall through to normal processing
        }

        let mut cursor = old_cursor;
        let mut src_str = self.get_current_src_str();
        while let Some((token, left, next_cursor, prev_whitespace)) =
            get_next_token(&self.preprocess_scanner, src_str, cursor)
        {
            let file_idx = self.current_cursor().file_idx;
            let preprocessor_token = PreprocessorToken {
                token,
                span: TokenSpan::new(file_idx, left, next_cursor),
                whitespace_left: prev_whitespace,
            };

            cursor = next_cursor;

            self.pp_token_line_buffer.push_back(preprocessor_token);
            src_str = self.get_current_src_str();
        }

        // dbg!(&self.pp_token_line_buffer);
        self.convert_line_to_clexemes();
        return true;
    }

    // precondition: directive token is already processed, pp_token_line_buffer
    // only contains contents after # <directive> (w/ no leading whitespace)
    fn preprocessor_directive(&mut self, directive_token: PreprocessorToken) {
        #[derive(Clone, Copy, PartialEq, Eq)]
        enum DirectiveType {
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
        }

        let directive_type = match self.get_text(directive_token.span) {
            "if" => DirectiveType::If,
            "ifdef" => DirectiveType::Ifdef,
            "ifndef" => DirectiveType::Ifndef,
            "elif" => DirectiveType::Elif,
            "else" => DirectiveType::Else,
            "endif" => DirectiveType::Endif,
            "include" => DirectiveType::Include,
            "define" => DirectiveType::Define,
            "undef" => DirectiveType::Undef,
            "line" => DirectiveType::Line,
            "error" => DirectiveType::Error,
            "pragma" => DirectiveType::Pragma,
            other => {
                eprintln!("warning: unrecognized directive {}", other);
                self.pp_token_line_buffer.clear();
                return;
            }
        };

        match directive_type {
            DirectiveType::If | DirectiveType::Elif => {
                // perform macro expansion -> parse / evaluate as constant expression
                let rest_of_line = std::mem::take(&mut self.pp_token_line_buffer);
                let rest_of_line = rest_of_line
                    .into_iter()
                    .map(MacroPreprocessorToken::FromSource)
                    .collect();

                // dbg!(&rest_of_line);
                let (substituted, stack) =
                    self.perform_macro_substitution(rest_of_line, Vec::new());
                dbg!(&substituted);

                if !stack.is_empty() {
                    eprintln!("unterminated macro expansion");
                    todo!()
                }

                let mut fixed = FixedTokens::new_from_deque(substituted, self);
                dbg!(&fixed);
                let controlling_expr = parse_expr(&mut fixed, &mut ParserState::new());
                let value = match controlling_expr {
                    Ok(expr_node) => {
                        let res = preprocessor_constant_eval(&expr_node);
                        match res {
                            Ok(v) => v,
                            Err(eval_err) => {
                                eprintln!("failed to evaluate control expression");
                                todo!()
                            }
                        }
                    }
                    Err(parse_err) => {
                        eprintln!("failed to parse control expression");
                        todo!()
                    }
                };

                match directive_type {
                    DirectiveType::If => {
                        if value != 0 {
                            self.conditional_stack.push(ConditionalState::Active);
                        } else {
                            self.conditional_stack.push(ConditionalState::NoneTaken);
                        }
                    }
                    DirectiveType::Elif => {
                        if self.conditional_stack.len() == 0 {
                            eprintln!("unexpected #elif");
                            todo!()
                        }

                        match self.conditional_stack.last_mut().unwrap() {
                            cur @ ConditionalState::NoneTaken => {
                                if value != 0 {
                                    *cur = ConditionalState::Active;
                                }
                            }
                            cur @ ConditionalState::Active => {
                                *cur = ConditionalState::SomeTaken;
                            }
                            ConditionalState::SomeTaken => {}
                        }
                    }
                    _ => unreachable!(),
                }
            }
            x @ (DirectiveType::Ifdef | DirectiveType::Ifndef) => {
                match self.pp_token_line_buffer.front() {
                    Some(PreprocessorToken {
                        token: CPreprocessor::Identifier,
                        span,
                        ..
                    }) => {
                        let macro_name = self.get_text(*span);
                        match (x, self.macros.contains_key(macro_name)) {
                            (DirectiveType::Ifdef, true) | (DirectiveType::Ifndef, false) => {
                                self.conditional_stack.push(ConditionalState::Active);
                            }
                            (DirectiveType::Ifdef, false) | (DirectiveType::Ifndef, true) => {
                                self.conditional_stack.push(ConditionalState::NoneTaken);
                            }
                            _ => unreachable!(),
                        };
                    }
                    None | _ => {
                        eprintln!("#if(n)def must be followed by an identifier");
                        todo!()
                    }
                }

                self.pp_token_line_buffer.pop_front();
                if self.pp_token_line_buffer.len() != 0 {
                    eprintln!("unexpected token after #if(n)def");
                    todo!()
                }
            }

            DirectiveType::Else => {
                if self.pp_token_line_buffer.len() != 0 {
                    eprintln!("warning: unexpected token after #else");
                    todo!()
                }

                match self.conditional_stack.last_mut() {
                    Some(inner) => {
                        *inner = match *inner {
                            ConditionalState::NoneTaken => ConditionalState::Active,
                            ConditionalState::Active | ConditionalState::SomeTaken => {
                                ConditionalState::SomeTaken
                            }
                        }
                    }
                    None => {
                        eprintln!("warning: unexpected #else");
                        todo!()
                    }
                }
            }
            DirectiveType::Endif => {
                if self.pp_token_line_buffer.len() != 0 {
                    eprintln!("warning: unexpected token after #endif");
                    todo!()
                }

                if self.conditional_stack.len() == 0 {
                    eprintln!("warning: unexpected #endif");
                    todo!()
                }

                self.conditional_stack.pop();
            }
            DirectiveType::Include => {
                let file = self.pp_token_line_buffer.pop_front();
                match file {
                    Some(PreprocessorToken {
                        token: CPreprocessor::StringLiteral,
                        span,
                        ..
                    }) if self.pp_token_line_buffer.len() == 0 => {
                        // "normal" (quote) include
                        let file = self.get_text(span).trim_matches('"').to_string();
                        self.include_file(&file, true);
                        return;
                    }
                    Some(PreprocessorToken {
                        token: CPreprocessor::OtherPunctuator,
                        span,
                        ..
                    }) if self.get_text(span) == "<" => {
                        let langle = span.left;
                        let rangle = loop {
                            if let Some(pp_token) = self.pp_token_line_buffer.pop_front() {
                                if pp_token.token == CPreprocessor::OtherPunctuator
                                    && self.get_text(pp_token.span) == ">"
                                {
                                    break Some(pp_token.span.right);
                                }
                            } else {
                                break None;
                            }
                        };

                        // fall through to macro-replacement case if no matching '>' or still have more tokens
                        if let Some(rangle) = rangle {
                            if self.pp_token_line_buffer.len() == 0 {
                                let current_file_idx = self.current_cursor().file_idx;
                                let file =
                                    self.get_text(TokenSpan::new(current_file_idx, langle, rangle));
                                // strip 1 pair of angle brackets only
                                let file = file
                                    .strip_prefix('<')
                                    .expect("must be surrounded by angle brackets")
                                    .strip_suffix('>')
                                    .expect("must be surrounded by angle brackets")
                                    .to_string();
                                self.include_file(&file, false);
                                return;
                            }
                        }
                    }
                    Some(_) => {
                        // fall through to macro replacement case
                    }
                    None => {
                        eprintln!("bad include directive");
                        todo!()
                    }
                }

                todo!("macro replacement case")
            }
            DirectiveType::Define => {
                let macro_name = self.pp_token_line_buffer.pop_front();
                let macro_name_span = if let Some(name) = macro_name {
                    if name.token != CPreprocessor::Identifier {
                        eprintln!("macro name must be identifier");
                        todo!()
                    }
                    name.span
                } else {
                    eprintln!("incomplete #define directive");
                    todo!()
                };

                // determine if function macro or object macro
                let first = self.pp_token_line_buffer.pop_front();
                enum FunctionOrObjectMacro {
                    FnMacro(FunctionMacro),
                    ObjectMacro(ObjectMacro),
                }

                let new_pp_macro = match first {
                    Some(PreprocessorToken {
                        token: CPreprocessor::LParen,
                        whitespace_left: false,
                        ..
                    }) => {
                        // function macro
                        let mut terminated = false;
                        let mut varargs = false;
                        let mut arguments: Vec<String> = Vec::new();
                        let mut i: usize = 0;
                        while let Some(pp_token) = self.pp_token_line_buffer.pop_front() {
                            if pp_token.token == CPreprocessor::RParen {
                                terminated = true;
                                break;
                            }
                            if i % 2 == 1 && pp_token.token != CPreprocessor::Comma {
                                eprintln!("bad function macro argument list");
                                todo!()
                            }
                            if i % 2 == 0 {
                                if pp_token.token == CPreprocessor::Identifier {
                                    let parameter_name = self.get_text(pp_token.span);
                                    if arguments.iter().any(|s| s == parameter_name) {
                                        eprintln!("duplicate macro argument");
                                        todo!()
                                    }
                                    arguments.push(parameter_name.to_string());
                                } else if pp_token.token == CPreprocessor::Ellipsis {
                                    varargs = true;
                                    if let Some(PreprocessorToken {
                                        token: CPreprocessor::RParen,
                                        ..
                                    }) = self.pp_token_line_buffer.front()
                                    {
                                    } else {
                                        eprintln!(
                                            "ellipsis (varargs) must be followed by closing paren"
                                        );
                                        todo!()
                                    }
                                } else {
                                    eprintln!("unexpected token in function macro argument list");
                                    todo!()
                                }
                            }
                            i += 1;
                        }

                        if !terminated {
                            eprintln!("unterminated function macro argument list");
                            todo!()
                        }

                        let replacement_list = std::mem::take(&mut self.pp_token_line_buffer);

                        let fn_macro = FunctionMacro {
                            arguments,
                            replacement_list,
                            varargs,
                        };

                        FunctionOrObjectMacro::FnMacro(fn_macro)
                    }
                    Some(other) => {
                        // object macro
                        let mut replacement_list =
                            VecDeque::with_capacity(1 + self.pp_token_line_buffer.len());
                        replacement_list.push_back(other);
                        replacement_list.append(&mut self.pp_token_line_buffer);
                        let obj_macro = ObjectMacro { replacement_list };

                        FunctionOrObjectMacro::ObjectMacro(obj_macro)
                    }
                    None => {
                        // object macro w/ empty replacement list
                        let object_macro = ObjectMacro {
                            replacement_list: VecDeque::new(),
                        };

                        FunctionOrObjectMacro::ObjectMacro(object_macro)
                    }
                };

                // no duplication allowed, unless it matches
                let macro_name = self.get_text(macro_name_span);
                match self.macros.get(macro_name) {
                    Some(pp_macro) => {
                        let replacement_lists_equal = |r1: &VecDeque<PreprocessorToken>,
                                                       r2: &VecDeque<PreprocessorToken>|
                         -> bool {
                            r1.len() == r2.len()
                                && iter::zip(r1, r2).all(|(t1, t2)| {
                                    self.get_text(t1.span) == self.get_text(t2.span)
                                })
                        };
                        let compatible = match (&new_pp_macro, pp_macro) {
                            (
                                FunctionOrObjectMacro::FnMacro(_),
                                PreprocessorMacro::ObjectMacro(_),
                            ) => false,
                            (
                                FunctionOrObjectMacro::FnMacro(f1),
                                PreprocessorMacro::FunctionMacro(f2),
                            ) => {
                                f1.varargs == f2.varargs
                                    && f1.arguments == f2.arguments
                                    && replacement_lists_equal(
                                        &f1.replacement_list,
                                        &f2.replacement_list,
                                    )
                            }
                            (
                                FunctionOrObjectMacro::ObjectMacro(m1),
                                PreprocessorMacro::ObjectMacro(m2),
                            ) => {
                                replacement_lists_equal(&m1.replacement_list, &m2.replacement_list)
                            }
                            (
                                FunctionOrObjectMacro::ObjectMacro(_),
                                PreprocessorMacro::FunctionMacro(_),
                            ) => false,
                        };

                        if !compatible {
                            eprintln!("warning: macro {macro_name} redefined");
                        } else {
                            // no need to overwrite existing
                            return;
                        }

                        let new_pp_macro = match new_pp_macro {
                            FunctionOrObjectMacro::FnMacro(m) => {
                                PreprocessorMacro::FunctionMacro(Rc::new(m))
                            }
                            FunctionOrObjectMacro::ObjectMacro(m) => {
                                PreprocessorMacro::ObjectMacro(Rc::new(m))
                            }
                        };

                        // borrowck does not like `self.macros.get_mut(macro_name)`
                        *self.macros.get_mut(&macro_name.to_string()).unwrap() = new_pp_macro;
                    }
                    None => {
                        let new_pp_macro = match new_pp_macro {
                            FunctionOrObjectMacro::FnMacro(m) => {
                                PreprocessorMacro::FunctionMacro(Rc::new(m))
                            }
                            FunctionOrObjectMacro::ObjectMacro(m) => {
                                PreprocessorMacro::ObjectMacro(Rc::new(m))
                            }
                        };
                        self.macros.insert(macro_name.to_string(), new_pp_macro);
                    }
                }
            }
            DirectiveType::Undef => {
                let macro_name = self.pp_token_line_buffer.pop_front();
                if let Some(name) = macro_name {
                    if name.token != CPreprocessor::Identifier {
                        eprintln!("unexpected token in #undef");
                        todo!()
                    }
                    let macro_name = self.get_text(name.span);
                    self.macros.remove(&macro_name.to_string()); // borrowck does not like
                } else {
                    todo!()
                }

                if self.pp_token_line_buffer.len() != 0 {
                    eprintln!("Unexpected token");
                    todo!()
                }
            }
            DirectiveType::Line => {
                todo!("line directive")
            }
            DirectiveType::Error => {
                let error_pp_tokens: Vec<_> = self
                    .pp_token_line_buffer
                    .iter()
                    .map(|x| self.get_text(x.span))
                    .collect();

                eprintln!("error: {}", error_pp_tokens.join(" "));
            }
            DirectiveType::Pragma => {
                todo!("pragma directive")
            }
        }

        self.pp_token_line_buffer.clear();
    }

    fn convert_pp_token_to_clexeme(&mut self, pp_token: MacroPreprocessorToken) {
        let text = pp_token.get_text(self);

        let (action, end_cursor) = match self.main_scanner.next_word(text.as_bytes(), 0) {
            super::table_scanner::ScannerResult::EndOfInput
            | super::table_scanner::ScannerResult::Failed => {
                todo!("failed to convert preprocessing token to clexeme")
            }
            super::table_scanner::ScannerResult::Ok(_, action, end_cursor) => (action, end_cursor),
        };
        if end_cursor != text.len() {
            todo!("failed to convert preprocessing token to clexeme");
        }

        let clexeme = CLexemes::from_id(action as u32)
            .expect("failed to convert preprocessing token to clexeme");

        // handle adjacent string literal concatenation
        match (clexeme, &mut self.str_literal_concat_buffer) {
            // need to be very careful in not merge together completely unrelated token spans (e.g. string literal coming from macro at top of file)
            // for now, just be conservative and only ever use left span
            (CLexemes::StringConst, Some(StringLiteral::Single(span))) => {
                let span = *span;
                let mut text = self.get_text(span).to_string();
                text.pop(); // remove rquote
                let second_text = pp_token.get_text(self);
                let second_text = &second_text['"'.len_utf8()..]; // remove lquote
                text.push_str(second_text);
                self.str_literal_concat_buffer = Some(StringLiteral::Concatenated(text, span));
            }
            (CLexemes::StringConst, Some(StringLiteral::Concatenated(s, span))) => {
                let span = *span;
                let mut owned = std::mem::take(s);
                owned.pop(); // remove rquote
                let second_text = pp_token.get_text(self);
                let second_text = &second_text['"'.len_utf8()..]; // remove lquote
                owned.push_str(second_text);
                self.str_literal_concat_buffer = Some(StringLiteral::Concatenated(owned, span));
            }
            (CLexemes::StringConst, None) => {
                self.str_literal_concat_buffer = Some(StringLiteral::Single(pp_token.span()))
            }
            (other, Some(StringLiteral::Single(span))) => {
                self.clexeme_buffer.push_back(CToken::FromSource {
                    token: CLexemes::StringConst,
                    span: *span,
                });
                std::mem::take(&mut self.str_literal_concat_buffer);
                self.clexeme_buffer.push_back(CToken::FromSource {
                    token: other,
                    span: pp_token.span(),
                });
            }
            (other, Some(StringLiteral::Concatenated(concat, span))) => {
                self.clexeme_buffer.push_back(CToken::Owned {
                    token: CLexemes::StringConst,
                    string: std::mem::take(concat),
                    span: *span,
                });
                std::mem::take(&mut self.str_literal_concat_buffer);
                self.clexeme_buffer.push_back(CToken::FromSource {
                    token: other,
                    span: pp_token.span(),
                });
            }
            (other, None) => {
                self.clexeme_buffer.push_back(CToken::FromSource {
                    token: other,
                    span: pp_token.span(),
                });
            }
        }
    }

    // mutually recursive functions to perform macro substitution - need to substitute macros
    // within macro replacement lists
    // overall, the macro substitution code is pretty bad - tons of allocations
    // ideally most code should not be too macro heavy
    fn resolve_macro_invocation(
        &self,
        mut macro_invocation_stack: Vec<MacroInvocation>,
    ) -> (VecDeque<MacroPreprocessorToken>, Vec<MacroInvocation>) {
        enum TokenOrPlacemarker {
            Token(MacroPreprocessorToken),
            Concat,
            Placemarker,
        }

        let substituted = match macro_invocation_stack
            .last_mut()
            .expect("must call with non-empty macro stack")
        {
            MacroInvocation::ObjectMacroInvocation { name: _, macro_ref } => macro_ref
                .replacement_list
                .clone()
                .into_iter()
                .map(|t| {
                    if t.token == CPreprocessor::DoubleHash {
                        TokenOrPlacemarker::Concat
                    } else {
                        let token = MacroPreprocessorToken::FromSource(t);
                        TokenOrPlacemarker::Token(token)
                    }
                })
                .collect(),
            MacroInvocation::FunctionMacroInvocation(FunctionMacroInvocation {
                macro_ref,
                arguments,
                name,
                ..
            }) => {
                let macro_ref = Rc::clone(macro_ref);

                let mut stringified: VecDeque<MacroPreprocessorToken> = VecDeque::new();
                let unexpanded_arguments = std::mem::take(arguments);
                dbg!(name.get_text(self));
                dbg!(&unexpanded_arguments);
                let mut expanded_arguments = Vec::new();
                for parameter in &unexpanded_arguments {
                    let substituted_parameter;
                    (substituted_parameter, macro_invocation_stack) =
                        self.perform_macro_substitution(parameter.clone(), macro_invocation_stack);
                    expanded_arguments.push(substituted_parameter);
                }
                dbg!(&expanded_arguments);

                let mut stringify = false;
                for replacement_tok in macro_ref.replacement_list.iter() {
                    if stringify {
                        let parameter_name = self.get_text(replacement_tok.span);
                        let index = macro_ref
                            .arguments
                            .iter()
                            .position(|x| x == parameter_name)
                            .expect("# operator must be followed by parameter name");
                        let param = &unexpanded_arguments[index]; // no macro substitution for operands of #, ##
                        let mut combined = String::new();
                        combined.push('"');
                        for pp_tok in param {
                            if pp_tok.token() == CPreprocessor::StringLiteral
                                || pp_tok.token() == CPreprocessor::CharConst
                            {
                                let tok_text = pp_tok.get_text(self);
                                let tok_text = tok_text.replace('\\', "\\\\");
                                let tok_text = tok_text.replace('"', "\\\"");
                                combined.push_str(&tok_text);
                            } else {
                                combined.push_str(pp_tok.get_text(self));
                            }
                            combined.push(' ');
                        }
                        combined.pop();
                        combined.push('"');

                        let combined_token = MacroPreprocessorToken::Concatenated(
                            CPreprocessor::StringLiteral,
                            combined,
                            replacement_tok.span,
                        );
                        stringified.push_back(combined_token);

                        stringify = false;
                        continue;
                    }

                    if replacement_tok.token == CPreprocessor::Hash {
                        stringify = true;
                        continue;
                    }

                    let token = MacroPreprocessorToken::FromSource(*replacement_tok);
                    stringified.push_back(token);
                }

                let mut concat_substituted: VecDeque<TokenOrPlacemarker> = VecDeque::new();
                while !stringified.is_empty() {
                    if stringified.len() >= 2 && stringified[1].token() == CPreprocessor::DoubleHash
                    {
                        let lhs = stringified.pop_front().unwrap();
                        let _ = stringified.pop_front().unwrap();
                        let rhs = stringified.pop_front().unwrap();

                        let tok_text = lhs.get_text(self);
                        let index = macro_ref.arguments.iter().position(|x| x == tok_text);
                        if let Some(index) = index {
                            let param = &unexpanded_arguments[index];
                            concat_substituted
                                .extend(param.iter().cloned().map(TokenOrPlacemarker::Token));
                            if param.len() == 0 {
                                concat_substituted.push_back(TokenOrPlacemarker::Placemarker);
                            }
                        } else {
                            concat_substituted.push_back(TokenOrPlacemarker::Token(lhs));
                        }

                        concat_substituted.push_back(TokenOrPlacemarker::Concat);

                        let tok_text = rhs.get_text(self);
                        let index = macro_ref.arguments.iter().position(|x| x == tok_text);
                        if let Some(index) = index {
                            let param = &unexpanded_arguments[index];
                            concat_substituted
                                .extend(param.iter().cloned().map(TokenOrPlacemarker::Token));
                            if param.len() == 0 {
                                concat_substituted.push_back(TokenOrPlacemarker::Placemarker)
                            }
                        } else {
                            concat_substituted.push_back(TokenOrPlacemarker::Token(rhs));
                        }
                    } else {
                        let tok = stringified.pop_front().unwrap();
                        let tok_text = tok.get_text(self);
                        let index = macro_ref.arguments.iter().position(|x| x == tok_text);
                        if let Some(index) = index {
                            let param = &expanded_arguments[index];
                            concat_substituted
                                .extend(param.iter().cloned().map(TokenOrPlacemarker::Token));
                        } else {
                            concat_substituted.push_back(TokenOrPlacemarker::Token(tok));
                        }
                    }
                }

                concat_substituted
            }
        };

        // handle ##, then rescan
        let mut concatenated: VecDeque<TokenOrPlacemarker> = VecDeque::new();
        let mut concat = false;
        for token in substituted {
            if let TokenOrPlacemarker::Concat = token {
                concat = true;
                continue;
            }

            // note: gcc and clang differ on ## behavior
            // i.e. #define concat(a, b) a ## ## b works on gcc, but not clang
            // i find clang's behavior more reasonable
            if concat {
                if let TokenOrPlacemarker::Placemarker = concatenated.back().unwrap() {
                    concatenated.pop_back();
                    concatenated.push_back(token);
                    continue;
                } else {
                    match token {
                        TokenOrPlacemarker::Token(tok) => {
                            if let TokenOrPlacemarker::Token(lhs) = concatenated.back_mut().unwrap()
                            {
                                let rhs = tok.get_text(self);
                                let lhs_text = lhs.get_text(self);
                                let mut pasted = String::with_capacity(lhs_text.len() + rhs.len());
                                pasted.push_str(lhs_text);
                                pasted.push_str(rhs);
                                // relex the pasted token
                                let action = match self
                                    .preprocess_scanner
                                    .next_word(pasted.as_bytes(), 0)
                                {
                                    crate::scanner::table_scanner::ScannerResult::EndOfInput
                                    | crate::scanner::table_scanner::ScannerResult::Failed => {
                                        todo!("bad token pasting")
                                    }
                                    crate::scanner::table_scanner::ScannerResult::Ok(
                                        _,
                                        action,
                                        _,
                                    ) => action,
                                };

                                *lhs = MacroPreprocessorToken::Concatenated(
                                    CPreprocessor::from_id(action as u32).unwrap(),
                                    pasted,
                                    lhs.span(), // conservatively only use lhs for span info
                                );
                                continue;
                            } else {
                                todo!("unexpected token pasting")
                            }
                        }
                        TokenOrPlacemarker::Concat => {
                            eprintln!("bad token pasting");
                            todo!()
                        }
                        TokenOrPlacemarker::Placemarker => {
                            continue;
                        }
                    }
                }
            } else {
                concatenated.push_back(token);
            }
        }

        let rescan: VecDeque<MacroPreprocessorToken> = concatenated
            .into_iter()
            .filter_map(|t| match t {
                TokenOrPlacemarker::Token(t) => Some(t),
                TokenOrPlacemarker::Concat => None,
                TokenOrPlacemarker::Placemarker => None,
            })
            .collect();

        let fully_processed;
        (fully_processed, macro_invocation_stack) =
            self.perform_macro_substitution(rescan, macro_invocation_stack);

        // pop macro off
        dbg!(&macro_invocation_stack);
        macro_invocation_stack.pop();
        (fully_processed, macro_invocation_stack)
    }

    fn perform_macro_substitution(
        &self,
        buffer: VecDeque<MacroPreprocessorToken>,
        mut macro_stack: Vec<MacroInvocation>,
    ) -> (VecDeque<MacroPreprocessorToken>, Vec<MacroInvocation>) {
        // not a preprocessing directive; attempt macro substitution / conversion to CLexemes
        let mut pp_token_buffer_substituted = VecDeque::with_capacity(buffer.len());
        // don't add to function macro arguments while doing rescan / argument expansion of function macro
        let mut add_to_macro = false;
        for macro_pp_token in buffer {
            // handle function macro arguments
            if let Some(MacroInvocation::FunctionMacroInvocation(fn_macro_invocation)) =
                macro_stack.last_mut()
            {
                if add_to_macro {
                    if fn_macro_invocation.arguments.len() == 0 {
                        // check for lparen, and also check if the macro still exists (could've been #undef'd)
                        let macro_name = fn_macro_invocation.name.get_text(self);
                        if macro_pp_token.token() == CPreprocessor::LParen
                            && self.macros.contains_key(macro_name)
                        {
                            fn_macro_invocation.add_token(macro_pp_token);
                            continue;
                        } else {
                            // not a function macro invocation (either #undef or not a Lparen)
                            pp_token_buffer_substituted.push_back(fn_macro_invocation.name.clone());
                            pp_token_buffer_substituted.push_back(macro_pp_token);
                            macro_stack.pop().unwrap();
                            continue;
                        }
                    } else {
                        if fn_macro_invocation.add_token(macro_pp_token) {
                            let mut resolved_macro_tokens;
                            (resolved_macro_tokens, macro_stack) =
                                self.resolve_macro_invocation(macro_stack);
                            add_to_macro = false;
                            pp_token_buffer_substituted.append(&mut resolved_macro_tokens);
                        }
                        continue;
                    }
                }
            }

            // handle macro substitution
            let text = macro_pp_token.get_text(self);
            if let Some(pp_macro) = self.macros.get(text) {
                // don't expand macros recursively
                match pp_macro {
                    PreprocessorMacro::ObjectMacro(inner) => {
                        macro_stack.push(MacroInvocation::ObjectMacroInvocation {
                            name: macro_pp_token,
                            macro_ref: inner.clone(),
                        });
                        // resolve immediately
                        let mut resolved;
                        (resolved, macro_stack) = self.resolve_macro_invocation(macro_stack);
                        pp_token_buffer_substituted.append(&mut resolved);
                        continue;
                    }
                    PreprocessorMacro::FunctionMacro(inner) => {
                        let fn_macro_invocation = MacroInvocation::FunctionMacroInvocation(
                            FunctionMacroInvocation::new(macro_pp_token, Rc::clone(inner)),
                        );
                        add_to_macro = true;
                        macro_stack.push(fn_macro_invocation);
                        continue;
                    }
                }
            }

            // handle "normal" text
            pp_token_buffer_substituted.push_back(macro_pp_token);
        }

        (pp_token_buffer_substituted, macro_stack)
    }

    // precondition: pp_token_line_buffer contains all preprocessing tokens (excluding splices and final newline) from a single logical line
    fn convert_line_to_clexemes(&mut self) {
        // check for preprocessing directive
        if self.pp_token_line_buffer.len() >= 2
            && self.pp_token_line_buffer[0].token == CPreprocessor::Hash
            && self.pp_token_line_buffer[1].token == CPreprocessor::Identifier
        {
            self.pp_token_line_buffer.pop_front().unwrap();
            let directive_token = self.pp_token_line_buffer.pop_front().unwrap();
            self.preprocessor_directive(directive_token);
        } else {
            let owned_buffer = std::mem::take(&mut self.pp_token_line_buffer);
            let owned_buffer = owned_buffer
                .into_iter()
                .map(|t| MacroPreprocessorToken::FromSource(t))
                .collect();
            let macro_stack = std::mem::take(&mut self.macro_invocation_stack);
            let fully_substituted;
            (fully_substituted, self.macro_invocation_stack) =
                self.perform_macro_substitution(owned_buffer, macro_stack);

            for item in fully_substituted {
                dbg!(&item);
                self.convert_pp_token_to_clexeme(item);
            }
        }
    }

    fn fill_buffer(&mut self, size: usize) -> bool {
        while self.clexeme_buffer.len() < size {
            let more_tokens = self.process_line();
            if !more_tokens {
                break;
            }
        }

        self.clexeme_buffer.len() >= size
    }
}

#[derive(Debug)]
struct FixedTokens<'a> {
    buffer: Vec<(CLexemes, Cow<'a, str>)>,
    cursor: usize,
}

impl<'a> FixedTokens<'_> {
    fn new_from_deque<'pp>(
        buffer: VecDeque<MacroPreprocessorToken>,
        pp: &'_ Preprocessor,
    ) -> FixedTokens<'_> {
        let buffer = buffer
            .into_iter()
            .map(|t| {
                let tok_text = t.get_cow(pp);
                let (text, action) = match pp.main_scanner.next_word(tok_text.as_bytes(), 0) {
                    super::table_scanner::ScannerResult::EndOfInput
                    | super::table_scanner::ScannerResult::Failed => todo!("return error here"),
                    super::table_scanner::ScannerResult::Ok(text, action, _) => (text, action),
                };
                if text.len() != tok_text.len() {
                    todo!("return error here")
                }
                (CLexemes::from_id(action as u32).unwrap(), tok_text)
            })
            .collect();

        FixedTokens { buffer, cursor: 0 }
    }
}

impl<'a> TokenStream<CLexemes> for FixedTokens<'a> {
    fn eat(&mut self, lexeme: CLexemes) -> bool {
        if self.cursor >= self.buffer.len() {
            return false;
        }

        let (l, _) = self.buffer[self.cursor];
        self.cursor += 1;
        lexeme == l
    }

    fn peek(&mut self) -> Option<(CLexemes, &str, usize)> {
        if self.cursor >= self.buffer.len() {
            return None;
        }

        let (lexeme, text) = &self.buffer[self.cursor];
        Some((*lexeme, text, 0))
    }

    fn peek_n(&mut self, n: usize) -> Option<(CLexemes, &str, usize)> {
        if self.cursor + n >= self.buffer.len() {
            return None;
        }

        let (lexeme, text) = &self.buffer[self.cursor + n];
        Some((*lexeme, text, 0))
    }

    fn advance(&mut self) -> Option<(CLexemes, &str, usize)> {
        if self.cursor >= self.buffer.len() {
            return None;
        }

        let (lexeme, text) = &self.buffer[self.cursor];
        self.cursor += 1;
        Some((*lexeme, text, 0))
    }

    fn rollback(&mut self, target: usize) {
        todo!()
    }

    fn get_location(&self) -> usize {
        todo!()
    }
}

impl TokenStream<CLexemes> for Preprocessor {
    fn eat(&mut self, lexeme: CLexemes) -> bool {
        if !self.fill_buffer(1) {
            return false;
        }

        let front = self.clexeme_buffer.pop_front().unwrap();
        front.token() == lexeme
    }

    fn peek(&mut self) -> Option<(CLexemes, &str, usize)> {
        if !self.fill_buffer(1) {
            return None;
        }

        let front = self.clexeme_buffer.front().unwrap();
        Some((front.token(), front.text(self), 0))
    }

    fn peek_n(&mut self, n: usize) -> Option<(CLexemes, &str, usize)> {
        if !self.fill_buffer(n) {
            return None;
        }

        let front = self.clexeme_buffer.front().unwrap();
        Some((front.token(), front.text(self), 0))
    }

    fn advance(&mut self) -> Option<(CLexemes, &str, usize)> {
        if !self.fill_buffer(1) {
            return None;
        }

        let front = self.clexeme_buffer.pop_front().unwrap();
        let arena_bytes = self
            .strings_buffer
            .alloc_slice_copy(&front.text(self).as_bytes());
        let str = core::str::from_utf8(arena_bytes).unwrap();
        Some((front.token(), str, 0))
    }

    fn rollback(&mut self, target: usize) {
        unimplemented!("no rollback for preprocessor (?)")
    }

    fn get_location(&self) -> usize {
        todo!()
    }
}

#[cfg(test)]
mod preprocessor_tests;
