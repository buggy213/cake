// super basic regex implementation for learning purposes
// with following grammar (EBNF)
// <regex> ::= <term> '|' <regex>
// |  <term>

// <term> ::= { <factor> }

// <factor> ::= <base> { '*' }
 
// <base> ::= <char>
// |  '\' <char>
// |  '(' <regex> ')'

use std::collections::VecDeque;

use super::alphabet::AsciiChar;

#[derive(Debug)]
pub enum Regex<A: Eq> {
    Alternation(Vec<Regex<A>>),
    Concatenation(Vec<Regex<A>>),
    Kleene(Box<Regex<A>>),
    Char(A)
}

#[derive(Debug)]
pub enum RegexError {
    Malformed
}

// basic recursive descent parsing
impl Regex<AsciiChar> {
    fn parse_regex(tokens: &mut VecDeque<AsciiChar>) -> Result<Regex<AsciiChar>, RegexError> {
        let term = Self::parse_term(tokens)?;
        let mut alternates: Vec<Regex<AsciiChar>> = Vec::new();
        while tokens.front().is_some_and(|x| *x == AsciiChar::VerticalLine) { // '|'
            tokens.pop_front();
            let alternate = Self::parse_term(tokens)?;
            alternates.push(alternate);
        }
        if alternates.is_empty() {
            Ok(term)
        }
        else {
            alternates.push(term); // commutative
            Ok(Regex::Alternation(alternates))
        }
    }

    fn parse_term(tokens: &mut VecDeque<AsciiChar>) -> Result<Regex<AsciiChar>, RegexError> {
        let mut factors: Vec<Regex<AsciiChar>> = Vec::new();
        while tokens.front().is_some_and(|x| *x != AsciiChar::VerticalLine && *x != AsciiChar::RightParenthesis) {
            let factor = Self::parse_factor(tokens)?;
            factors.push(factor);
        }

        assert!(factors.len() != 0);
        
        if factors.len() == 1 {
            Ok(factors.remove(0))
        }
        else {
            Ok(Regex::Concatenation(factors))
        }
    }

    fn parse_factor(tokens: &mut VecDeque<AsciiChar>) -> Result<Regex<AsciiChar>, RegexError> {
        let base = Self::parse_base(tokens)?;
        let mut is_kleene = false;
        while tokens.front().is_some_and(|x| *x == AsciiChar::Asterisk) {
            is_kleene = true;
            tokens.pop_front();
        }

        if is_kleene {
            Ok(Regex::Kleene(Box::new(base)))
        }
        else {
            Ok(base)
        }

    }

    fn parse_base(tokens: &mut VecDeque<AsciiChar>) -> Result<Regex<AsciiChar>, RegexError> {
        let front = tokens.pop_front().ok_or(RegexError::Malformed)?;
        if front == AsciiChar::ReverseSolidus { // funny name for backslash
            // escape next character
            let code = tokens.pop_front().ok_or(RegexError::Malformed)?;
            let escaped = match code {
                AsciiChar::SmallN => AsciiChar::LineFeed,
                x => x
            };
            Ok(Regex::Char(escaped))
        }
        else if front == AsciiChar::LeftParenthesis {
            let parenthesized_regex = Self::parse_regex(tokens)?;
            if tokens.pop_front().is_some_and(|x| x == AsciiChar::RightParenthesis) {
                Ok(parenthesized_regex)
            }
            else {
                Err(RegexError::Malformed)
            }
        }
        else {
            Ok(Regex::Char(front))
        }
    }

    pub fn from_str(re_str: &str) -> Result<Regex<AsciiChar>, RegexError> {
        assert!(re_str.is_ascii());
        let mut tokens: VecDeque<AsciiChar> = re_str.as_bytes().iter().map(|x| AsciiChar::from_u8(*x)).collect();
        let re = Self::parse_regex(&mut tokens)?;
        if tokens.len() > 0 {
            Err(RegexError::Malformed)
        }
        else {
            Ok(re)
        }
    }    
}