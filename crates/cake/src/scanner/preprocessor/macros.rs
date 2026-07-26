//! Macro definition and expansion (translation phase 4).

use std::collections::VecDeque;
use std::rc::Rc;

use crate::scanner::lexeme_sets::c_preprocessor::CPreprocessor;
use crate::scanner::lexemes::LexemeSet;
use crate::scanner::table_scanner::ScannerResult;

use super::{PPToken, Preprocessor, Result, Span};

#[derive(Debug)]
pub(super) struct MacroDef {
    /// parameter name spans; `None` for an object-like macro
    pub(super) params: Option<Vec<Span>>,
    pub(super) varargs: bool,
    pub(super) body: Vec<PPToken>,
}

impl MacroDef {
    pub(super) fn is_function_like(&self) -> bool {
        self.params.is_some()
    }

    /// number of arguments an invocation must supply
    fn arity(&self) -> usize {
        let named = self.params.as_ref().map_or(0, Vec::len);
        named + usize::from(self.varargs)
    }
}

/// One piece of a partially substituted macro body. Placemarkers stand in for
/// empty arguments so that `a ## b` with an empty `a` still pastes correctly.
enum Piece {
    Token(PPToken),
    Paste,
    Placemarker,
}

/// Result of looking for a function-like macro's argument list.
enum ArgList {
    /// found a complete `( ... )`, spanning this many tokens
    Found(Vec<Vec<PPToken>>, usize),
    /// the next token is not `(`, so this is not an invocation
    NotAnInvocation,
    /// ran out of input mid-invocation; a later line may complete it
    Incomplete,
}

impl Preprocessor {
    /// Expands macros in `input`, appending the result to `out`.
    ///
    /// If `partial_ok` is set and a function-like macro invocation runs off the
    /// end of `input`, its tokens are left at the front of `input` so the
    /// caller can append the next line and try again. Otherwise an unterminated
    /// invocation is left alone.
    pub(super) fn expand(
        &mut self,
        input: &mut VecDeque<PPToken>,
        out: &mut Vec<PPToken>,
        partial_ok: bool,
    ) -> Result<()> {
        while let Some(token) = input.pop_front() {
            if token.kind != CPreprocessor::Identifier {
                out.push(token);
                continue;
            }

            let Some(def) = self.macros.get(self.spelling(token.span)).cloned() else {
                out.push(token);
                continue;
            };

            // a macro is never expanded within its own expansion
            if self.expanding.iter().any(|m| Rc::ptr_eq(m, &def)) {
                out.push(token);
                continue;
            }

            if !def.is_function_like() {
                let body = self.substitute(&def, &[], token)?;
                self.rescan(&def, body, out)?;
                continue;
            }

            match self.collect_args(input, &def, partial_ok) {
                ArgList::NotAnInvocation => out.push(token),
                ArgList::Incomplete => {
                    input.push_front(token);
                    return Ok(());
                }
                ArgList::Found(args, consumed) => {
                    input.drain(..consumed);
                    let body = self.substitute(&def, &args, token)?;
                    self.rescan(&def, body, out)?;
                }
            }
        }

        Ok(())
    }

    /// Re-expands a substituted macro body with `def` disabled, so that a macro
    /// mentioning itself does not expand forever.
    fn rescan(
        &mut self,
        def: &Rc<MacroDef>,
        body: Vec<PPToken>,
        out: &mut Vec<PPToken>,
    ) -> Result<()> {
        let mut body: VecDeque<PPToken> = body.into();
        self.expanding.push(Rc::clone(def));
        let result = self.expand(&mut body, out, false);
        self.expanding.pop();
        result
    }

    /// Index of the parameter that `name` refers to, if any. `__VA_ARGS__`
    /// stands for the argument just past the named ones.
    fn param_index(&self, def: &MacroDef, name: Span) -> Option<usize> {
        let params = def.params.as_ref()?;
        let name = self.spelling(name);
        if let Some(i) = params.iter().position(|&p| self.spelling(p) == name) {
            return Some(i);
        }
        if def.varargs && name == "__VA_ARGS__" {
            return Some(params.len());
        }
        None
    }

    /// Looks for `( arg, arg, ... )` at the front of `input` without consuming
    /// it. Arguments are returned unexpanded.
    fn collect_args(
        &self,
        input: &VecDeque<PPToken>,
        def: &MacroDef,
        partial_ok: bool,
    ) -> ArgList {
        let incomplete = if partial_ok {
            ArgList::Incomplete
        } else {
            ArgList::NotAnInvocation
        };

        match input.front() {
            None => return incomplete,
            Some(first) if first.kind != CPreprocessor::LParen => {
                return ArgList::NotAnInvocation;
            }
            Some(_) => {}
        }

        let mut args: Vec<Vec<PPToken>> = vec![Vec::new()];
        let mut depth = 0usize;
        for (i, &token) in input.iter().enumerate() {
            match token.kind {
                CPreprocessor::LParen => {
                    depth += 1;
                    if depth == 1 {
                        continue; // the opening paren itself
                    }
                }
                CPreprocessor::RParen => {
                    depth -= 1;
                    if depth == 0 {
                        // `f()` passes one empty argument, which is also how a
                        // macro that takes no arguments is invoked
                        if def.arity() == 0 && args.len() == 1 && args[0].is_empty() {
                            args.clear();
                        }
                        return ArgList::Found(args, i + 1);
                    }
                }
                // a top level comma starts the next argument, unless we have
                // reached the variadic argument, which swallows commas
                CPreprocessor::Comma if depth == 1 && args.len() < def.arity() => {
                    args.push(Vec::new());
                    continue;
                }
                _ => {}
            }
            args.last_mut().expect("always at least one argument").push(token);
        }

        incomplete
    }

    /// Substitutes arguments into a macro body, applying `#` and `##`.
    fn substitute(
        &mut self,
        def: &MacroDef,
        args: &[Vec<PPToken>],
        invocation: PPToken,
    ) -> Result<Vec<PPToken>> {
        // a variadic macro may be invoked without its variadic argument
        let arity = def.arity();
        if def.is_function_like() && args.len() != arity && !(def.varargs && args.len() + 1 == arity)
        {
            let name = self.spelling(invocation.span).to_string();
            return Err(self.error(
                invocation.loc,
                format!(
                    "macro '{name}' takes {arity} arguments, but {} were given",
                    args.len()
                ),
            ));
        }

        // arguments are only expanded if they are actually used somewhere other
        // than as an operand of `#` or `##`
        let mut expanded: Vec<Option<Vec<PPToken>>> = vec![None; args.len()];
        let mut pieces: Vec<Piece> = Vec::with_capacity(def.body.len());

        let mut i = 0;
        while i < def.body.len() {
            let token = def.body[i];

            // `# param` stringifies the unexpanded argument
            if def.is_function_like() && token.kind == CPreprocessor::Hash {
                let index = def
                    .body
                    .get(i + 1)
                    .and_then(|next| self.param_index(def, next.span));
                let Some(index) = index else {
                    return Err(self.error(
                        token.loc,
                        "'#' must be followed by a macro parameter".into(),
                    ));
                };
                let arg = args.get(index).map_or(&[][..], Vec::as_slice);
                let stringified = self.stringify(arg, token);
                pieces.push(Piece::Token(stringified));
                i += 2;
                continue;
            }

            if token.kind == CPreprocessor::DoubleHash {
                pieces.push(Piece::Paste);
                i += 1;
                continue;
            }

            let next_is_paste = def
                .body
                .get(i + 1)
                .is_some_and(|next| next.kind == CPreprocessor::DoubleHash);
            let prev_is_paste = i > 0 && def.body[i - 1].kind == CPreprocessor::DoubleHash;

            match self.param_index(def, token.span) {
                None => pieces.push(Piece::Token(token)),
                Some(index) => {
                    let arg = args.get(index).map_or(&[][..], Vec::as_slice);
                    if next_is_paste || prev_is_paste {
                        // operands of `##` are substituted unexpanded
                        if arg.is_empty() {
                            pieces.push(Piece::Placemarker);
                        }
                        pieces.extend(arg.iter().map(|&t| Piece::Token(t)));
                    } else {
                        if expanded[index].is_none() {
                            let mut arg_input: VecDeque<PPToken> = arg.iter().copied().collect();
                            let mut arg_out = Vec::new();
                            self.expand(&mut arg_input, &mut arg_out, false)?;
                            expanded[index] = Some(arg_out);
                        }
                        let arg = expanded[index].as_deref().expect("just expanded");
                        pieces.extend(arg.iter().map(|&t| Piece::Token(t)));
                    }
                }
            }

            i += 1;
        }

        self.paste(pieces, invocation)
    }

    /// Carries out the `##` pastes recorded while substituting.
    fn paste(&mut self, pieces: Vec<Piece>, invocation: PPToken) -> Result<Vec<PPToken>> {
        let mut out: Vec<PPToken> = Vec::with_capacity(pieces.len());
        let mut pasting = false;

        for piece in pieces {
            match piece {
                Piece::Paste => {
                    if out.is_empty() {
                        return Err(self.error(
                            invocation.loc,
                            "'##' must not appear at the start of a macro body".into(),
                        ));
                    }
                    pasting = true;
                }
                // pasting with a placemarker leaves the other operand alone
                Piece::Placemarker => pasting = false,
                Piece::Token(token) => {
                    if pasting {
                        let lhs = out.pop().expect("checked above");
                        let pasted = self.paste_tokens(lhs, token)?;
                        out.push(pasted);
                        pasting = false;
                    } else {
                        out.push(token);
                    }
                }
            }
        }

        Ok(out)
    }

    /// Concatenates the spellings of two tokens and re-lexes the result.
    fn paste_tokens(&mut self, lhs: PPToken, rhs: PPToken) -> Result<PPToken> {
        let mut combined = String::new();
        combined.push_str(self.spelling(lhs.span));
        combined.push_str(self.spelling(rhs.span));

        let kind = match self.pp_scanner.next_word(combined.as_bytes(), 0) {
            ScannerResult::Ok(word, action, _) if word.len() == combined.len() => {
                CPreprocessor::from_id(action).expect("C preprocessor DFA should be infallible")
            }
            _ => {
                return Err(self.error(
                    lhs.loc,
                    format!("pasting formed '{combined}', which is not a valid token"),
                ));
            }
        };

        Ok(self.synthesize(&combined, kind, lhs.loc, lhs.ws_before))
    }

    /// Renders an argument as a string literal, for the `#` operator.
    fn stringify(&mut self, arg: &[PPToken], hash: PPToken) -> PPToken {
        let mut literal = String::from("\"");
        for (i, token) in arg.iter().enumerate() {
            if i > 0 && token.ws_before {
                literal.push(' ');
            }
            let text = self.spelling(token.span);
            match token.kind {
                // quotes and backslashes in the spelling have to be escaped
                CPreprocessor::StringLiteral | CPreprocessor::CharConst => {
                    for c in text.chars() {
                        if c == '"' || c == '\\' {
                            literal.push('\\');
                        }
                        literal.push(c);
                    }
                }
                _ => literal.push_str(text),
            }
        }
        literal.push('"');

        self.synthesize(&literal, CPreprocessor::StringLiteral, hash.loc, hash.ws_before)
    }
}
