//! Evaluation of `#if` / `#elif` controlling expressions.
//!
//! These are a small language of their own: integer constants and character
//! constants combined with the usual operators, plus `defined`. Identifiers
//! that survive macro expansion stand for 0. Everything is evaluated as
//! `i64`, which covers `intmax_t` arithmetic for all but the extremes of the
//! unsigned range.

use std::collections::VecDeque;

use crate::scanner::lexeme_sets::c_preprocessor::CPreprocessor;

use super::{Loc, PPToken, Preprocessor, Result};

impl Preprocessor {
    /// Evaluates the controlling expression of an `#if` or `#elif`.
    pub(super) fn eval_condition(&mut self, line: &[PPToken], loc: Loc) -> Result<bool> {
        if line.is_empty() {
            return Err(self.error(loc, "expected an expression after #if".into()));
        }

        // `defined X` must be resolved before macro expansion, so that the
        // macro name is not expanded out from under it
        let resolved = self.resolve_defined(line)?;

        let mut input: VecDeque<PPToken> = resolved.into();
        let mut expanded = Vec::new();
        self.expand(&mut input, &mut expanded, false)?;

        let mut parser = CondParser {
            pp: self,
            tokens: &expanded,
            pos: 0,
        };
        let value = parser.expression(0)?;
        let rest = parser.pos;
        if rest < expanded.len() {
            let token = expanded[rest];
            let text = self.spelling(token.span).to_string();
            return Err(self.error(token.loc, format!("unexpected '{text}' in #if expression")));
        }

        Ok(value != 0)
    }

    /// Replaces `defined X` and `defined ( X )` with 1 or 0.
    fn resolve_defined(&mut self, line: &[PPToken]) -> Result<Vec<PPToken>> {
        let mut out = Vec::with_capacity(line.len());
        let mut i = 0;
        while i < line.len() {
            let token = line[i];
            if token.kind != CPreprocessor::Identifier || self.spelling(token.span) != "defined" {
                out.push(token);
                i += 1;
                continue;
            }

            // `defined X` or `defined ( X )`
            let parenthesized = matches!(line.get(i + 1), Some(t) if t.kind == CPreprocessor::LParen);
            let name_at = if parenthesized { i + 2 } else { i + 1 };
            let name = line.get(name_at).filter(|t| t.kind == CPreprocessor::Identifier);
            let Some(name) = name else {
                return Err(self.error(
                    token.loc,
                    "'defined' must be followed by a macro name".into(),
                ));
            };

            i = name_at + 1;
            if parenthesized {
                if !matches!(line.get(i), Some(t) if t.kind == CPreprocessor::RParen) {
                    return Err(self.error(token.loc, "expected ')' after macro name".into()));
                }
                i += 1;
            }

            let defined = self.macros.contains_key(self.spelling(name.span));
            let text = if defined { "1" } else { "0" };
            out.push(self.synthesize(
                text,
                CPreprocessor::PreprocessingNumber,
                token.loc,
                token.ws_before,
            ));
        }

        Ok(out)
    }
}

struct CondParser<'a> {
    pp: &'a Preprocessor,
    tokens: &'a [PPToken],
    pos: usize,
}

/// Binding power of each binary operator, loosest first. `0` means "not a
/// binary operator".
fn precedence(op: &str) -> u8 {
    match op {
        "||" => 1,
        "&&" => 2,
        "|" => 3,
        "^" => 4,
        "&" => 5,
        "==" | "!=" => 6,
        "<" | ">" | "<=" | ">=" => 7,
        "<<" | ">>" => 8,
        "+" | "-" => 9,
        "*" | "/" | "%" => 10,
        _ => 0,
    }
}

impl CondParser<'_> {
    fn peek(&self) -> Option<PPToken> {
        self.tokens.get(self.pos).copied()
    }

    fn peek_text(&self) -> Option<&str> {
        self.peek().map(|t| self.pp.spelling(t.span))
    }

    fn eat(&mut self, text: &str) -> bool {
        if self.peek_text() == Some(text) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn loc(&self) -> Loc {
        self.peek()
            .map(|t| t.loc)
            .or_else(|| self.tokens.last().map(|t| t.loc))
            .unwrap_or(Loc { src: 0, line: 0 })
    }

    fn err(&self, message: String) -> Box<super::PreprocessingError> {
        self.pp.error(self.loc(), message)
    }

    /// Precedence climbing, plus the conditional operator at the loosest level.
    fn expression(&mut self, min_precedence: u8) -> Result<i64> {
        let mut lhs = self.unary()?;

        loop {
            let Some(op) = self.peek_text() else { break };

            // `? :` is right associative and binds loosest of all
            if op == "?" && min_precedence == 0 {
                self.pos += 1;
                let then_value = self.expression(0)?;
                if !self.eat(":") {
                    return Err(self.err("expected ':' in conditional expression".into()));
                }
                let else_value = self.expression(0)?;
                lhs = if lhs != 0 { then_value } else { else_value };
                continue;
            }

            let precedence = precedence(op);
            if precedence == 0 || precedence < min_precedence {
                break;
            }
            let op = op.to_string();
            self.pos += 1;

            // short circuiting operators must not evaluate (or diagnose
            // division by zero in) the side they skip
            if op == "||" || op == "&&" {
                let rhs = self.expression(precedence + 1)?;
                lhs = match &*op {
                    "||" => i64::from(lhs != 0 || rhs != 0),
                    _ => i64::from(lhs != 0 && rhs != 0),
                };
                continue;
            }

            let rhs = self.expression(precedence + 1)?;
            lhs = self.binary(&op, lhs, rhs)?;
        }

        Ok(lhs)
    }

    fn binary(&self, op: &str, lhs: i64, rhs: i64) -> Result<i64> {
        let value = match op {
            "|" => lhs | rhs,
            "^" => lhs ^ rhs,
            "&" => lhs & rhs,
            "==" => i64::from(lhs == rhs),
            "!=" => i64::from(lhs != rhs),
            "<" => i64::from(lhs < rhs),
            ">" => i64::from(lhs > rhs),
            "<=" => i64::from(lhs <= rhs),
            ">=" => i64::from(lhs >= rhs),
            "<<" => lhs.wrapping_shl(rhs as u32),
            ">>" => lhs.wrapping_shr(rhs as u32),
            "+" => lhs.wrapping_add(rhs),
            "-" => lhs.wrapping_sub(rhs),
            "*" => lhs.wrapping_mul(rhs),
            "/" | "%" => {
                if rhs == 0 {
                    return Err(self.err("division by zero in #if expression".into()));
                }
                if op == "/" {
                    lhs.wrapping_div(rhs)
                } else {
                    lhs.wrapping_rem(rhs)
                }
            }
            _ => unreachable!("not a binary operator: {op}"),
        };

        Ok(value)
    }

    fn unary(&mut self) -> Result<i64> {
        let Some(token) = self.peek() else {
            return Err(self.err("unexpected end of #if expression".into()));
        };
        let text = self.pp.spelling(token.span);

        match text {
            "(" => {
                self.pos += 1;
                let value = self.expression(0)?;
                if !self.eat(")") {
                    return Err(self.err("expected ')'".into()));
                }
                Ok(value)
            }
            "!" => {
                self.pos += 1;
                Ok(i64::from(self.unary()? == 0))
            }
            "~" => {
                self.pos += 1;
                Ok(!self.unary()?)
            }
            "-" => {
                self.pos += 1;
                Ok(self.unary()?.wrapping_neg())
            }
            "+" => {
                self.pos += 1;
                self.unary()
            }
            _ => self.primary(),
        }
    }

    fn primary(&mut self) -> Result<i64> {
        let Some(token) = self.peek() else {
            return Err(self.err("unexpected end of #if expression".into()));
        };
        self.pos += 1;
        let text = self.pp.spelling(token.span);

        match token.kind {
            CPreprocessor::PreprocessingNumber => parse_integer(text)
                .ok_or_else(|| self.pp.error(token.loc, format!("invalid integer '{text}'"))),
            CPreprocessor::CharConst => parse_char(text)
                .ok_or_else(|| self.pp.error(token.loc, format!("invalid character constant '{text}'"))),
            // any identifier left after macro expansion evaluates to 0
            CPreprocessor::Identifier => Ok(0),
            _ => Err(self
                .pp
                .error(token.loc, format!("unexpected '{text}' in #if expression"))),
        }
    }
}

/// Parses a C integer constant: decimal, `0x` hex or leading-zero octal, with
/// any combination of `u` and `l` suffixes.
fn parse_integer(text: &str) -> Option<i64> {
    let digits = text.trim_end_matches(['u', 'U', 'l', 'L']);

    let (radix, digits) = if let Some(hex) = digits.strip_prefix("0x").or(digits.strip_prefix("0X"))
    {
        (16, hex)
    } else if digits.len() > 1 && digits.starts_with('0') {
        (8, &digits[1..])
    } else {
        (10, digits)
    };

    i64::from_str_radix(digits, radix).ok()
}

/// Parses a character constant, including the common escape sequences.
fn parse_char(text: &str) -> Option<i64> {
    let body = text.strip_prefix('\'')?.strip_suffix('\'')?;
    let mut chars = body.chars();
    let value = match chars.next()? {
        '\\' => match chars.next()? {
            'n' => 10,
            't' => 9,
            'r' => 13,
            '0' => 0,
            'a' => 7,
            'b' => 8,
            'f' => 12,
            'v' => 11,
            '\\' => 92,
            '\'' => 39,
            '"' => 34,
            'x' => {
                let hex: String = chars.by_ref().collect();
                return i64::from_str_radix(&hex, 16).ok();
            }
            octal @ '1'..='7' => {
                let mut digits = String::from(octal);
                digits.extend(chars.by_ref());
                return i64::from_str_radix(&digits, 8).ok();
            }
            other => other as i64,
        },
        c => c as i64,
    };

    // reject multi-character constants rather than guess at their value
    if chars.next().is_some() {
        return None;
    }

    Some(value)
}
