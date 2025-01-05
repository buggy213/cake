// super basic regex implementation for learning purposes
// with following grammar (EBNF)
// <regex> ::= <term> '|' <regex>
// |  <term>

// <term> ::= { <factor> }

// <factor> ::= <base> { '*' | '+' | '?' | <count> }

// <count> ::= '{' <int> { ',' } { <int> } '}'

// <base> ::= <char>
// |  '[' <class> ']'               (character classes)
// |  '.'                           (wildcard)
// |  '\' <char>                    (escape sequences)
// |  '(' <regex> ')'

// <class> ::= { '^' } <class'>     (negation)

// <class'> ::= (<class-item>)+     (empty character classes forbidden)

// <class-item> ::= <char>
// | '-' <char>
// | '\' <char>

use std::collections::VecDeque;

use cake_util::RangeUInt;
use thiserror::Error;

use crate::AsciiChar;

#[derive(Clone, Debug)]
pub(super) enum Regex {
    Alternation(Vec<Regex>),
    Concatenation(Vec<Regex>),
    Kleene(Box<Regex>),
    Char(AsciiChar),

    Class(RegexClass),
    Empty,
}

#[derive(Clone, Debug)]
pub(super) struct RegexClass {
    pub(super) ranges: Vec<(AsciiChar, AsciiChar)>,
    pub(super) characters: Vec<AsciiChar>,
    pub(super) negated: bool,
}

#[derive(Debug)]
enum ClassItem<A: Eq> {
    Char(A),
    RangeEnd(A),
}

#[derive(Debug, Error)]
pub(super) enum RegexError {
    #[error("Malformed regex")]
    Malformed,
}

// basic recursive descent parsing
impl Regex {
    fn parse_regex(tokens: &mut VecDeque<AsciiChar>) -> Result<Regex, RegexError> {
        let term = Self::parse_term(tokens)?;
        let mut alternates: Vec<Regex> = Vec::new();
        while tokens.front().is_some_and(|x| *x == b'|') {
            tokens.pop_front();
            let alternate = Self::parse_term(tokens)?;
            alternates.push(alternate);
        }
        if alternates.is_empty() {
            Ok(term)
        } else {
            alternates.push(term); // commutative
            Ok(Regex::Alternation(alternates))
        }
    }

    fn parse_term(tokens: &mut VecDeque<AsciiChar>) -> Result<Regex, RegexError> {
        let mut factors: Vec<Regex> = Vec::new();
        while tokens.front().is_some_and(|x| *x != b'|' && *x != b')') {
            let factor = Self::parse_factor(tokens)?;
            factors.push(factor);
        }

        assert!(factors.len() != 0);

        if factors.len() == 1 {
            Ok(factors.remove(0))
        } else {
            Ok(Regex::Concatenation(factors))
        }
    }

    fn parse_count(tokens: &mut VecDeque<AsciiChar>) -> Result<(RangeUInt, RangeUInt), RegexError> {
        let mut count_string = String::new();
        let mut found_right = false;
        while let Some(x) = tokens.pop_front() {
            count_string.push(char::from_u32(x as u32).expect("should be valid ascii"));
            if x == b'}' {
                found_right = true;
                break;
            }
        }

        if !found_right {
            return Err(RegexError::Malformed);
        }

        // remove parentheses
        count_string.remove(0);
        count_string.pop();

        let parse_str = |x: &str| {
            let x = x.parse::<u32>();
            match x {
                Ok(count) => Ok(count),
                Err(_) => Err(RegexError::Malformed),
            }
        };

        let low_count;
        let high_count;
        if count_string.contains(',') {
            let mut it = count_string.split(',');
            let first = it.next().ok_or(RegexError::Malformed)?;
            let second = it.next().ok_or(RegexError::Malformed)?;

            let first = parse_str(first)?;
            low_count = RangeUInt::Finite(first);

            if second.len() == 0 {
                high_count = RangeUInt::Infinite;
            } else {
                let count = parse_str(second)?;
                high_count = RangeUInt::Finite(count)
            }
        } else {
            let count = parse_str(&count_string)?;
            low_count = RangeUInt::Finite(count);
            high_count = RangeUInt::Finite(count);
        }

        let result = (low_count, high_count);
        match result {
            (RangeUInt::Finite(a), RangeUInt::Finite(b)) => {
                if a <= b {
                    Ok(result)
                } else {
                    Err(RegexError::Malformed)
                }
            }
            (RangeUInt::Finite(_), RangeUInt::Infinite) => Ok(result),
            _ => Err(RegexError::Malformed),
        }
    }

    fn parse_factor(tokens: &mut VecDeque<AsciiChar>) -> Result<Regex, RegexError> {
        let base = Self::parse_base(tokens)?;
        let mut range_low = RangeUInt::Finite(1);
        let mut range_high = RangeUInt::Finite(1);
        while let Some(x) = tokens.front() {
            match x {
                b'*' => {
                    range_low = RangeUInt::Finite(0);
                    range_high = RangeUInt::Infinite;
                    tokens.pop_front();
                }
                b'+' => {
                    range_high = RangeUInt::Infinite;
                    tokens.pop_front();
                }
                b'?' => {
                    range_low = RangeUInt::Finite(0);
                    tokens.pop_front();
                }
                b'{' => {
                    let (low, high) = Self::parse_count(tokens)?;
                    range_low = range_low * low;
                    range_high = range_high * high;
                }
                _ => break,
            }
        }

        let factor = match (range_low, range_high) {
            (RangeUInt::Finite(low), RangeUInt::Finite(high)) if low == 1 && high == 1 => Ok(base),
            (RangeUInt::Finite(low), RangeUInt::Finite(high)) => {
                assert!(low <= high);
                // TODO: is there something more efficient than just cloning
                // parts of the AST? NFA has to contain multiple copies at the end
                // regardless

                let mut alternates: Vec<Regex> = Vec::new();
                for i in low..=high {
                    if i > 1 {
                        let mut base_repeats: Vec<Regex> = Vec::new();
                        base_repeats.resize(i as usize, base.clone());
                        alternates.push(Regex::Concatenation(base_repeats))
                    } else if i == 1 {
                        alternates.push(base.clone());
                    } else {
                        alternates.push(Regex::Empty);
                    }
                }

                // println!("{:?}", alternates);

                Ok(Regex::Alternation(alternates))
            }
            (RangeUInt::Finite(low), RangeUInt::Infinite) => {
                let kleene = Regex::Kleene(Box::new(base.clone()));
                if low != 0 {
                    let mut base_repeats: Vec<Regex> = Vec::new();
                    base_repeats.resize(low as usize, base);
                    base_repeats.push(kleene);
                    Ok(Regex::Concatenation(base_repeats))
                } else {
                    Ok(kleene)
                }
            }
            _ => Err(RegexError::Malformed),
        };

        factor
    }

    fn parse_class(tokens: &mut VecDeque<AsciiChar>) -> Result<RegexClass, RegexError> {
        let negated;
        if let Some(x) = tokens.front() {
            if *x == b'^' {
                tokens.pop_front();
                negated = true;
            } else {
                negated = false;
            }
        } else {
            negated = false;
        }

        let mut class_items: Vec<ClassItem<AsciiChar>> = Vec::new();
        while let Some(x) = tokens.front() {
            if *x == b']' {
                break;
            }

            let class_item = Self::parse_class_item(tokens)?;
            class_items.push(class_item);
        }

        // println!("{:?}", class_items);
        let mut characters: Vec<AsciiChar> = Vec::new();
        let mut ranges: Vec<(AsciiChar, AsciiChar)> = Vec::new();
        let mut i: i32 = (class_items.len() - 1) as i32;
        while i >= 0 {
            let item = &class_items[i as usize];
            match item {
                ClassItem::Char(c) => {
                    characters.push(*c);
                }
                ClassItem::RangeEnd(high) => {
                    i -= 1;
                    if i >= 0 {
                        if let ClassItem::Char(low) = class_items[i as usize] {
                            ranges.push((low, *high));
                        } else {
                            return Err(RegexError::Malformed);
                        }
                    } else {
                        return Err(RegexError::Malformed);
                    }
                }
            }

            i -= 1;
        }
        Ok(RegexClass {
            ranges,
            characters,
            negated,
        })
    }

    fn escape_codes(code: AsciiChar) -> AsciiChar {
        match code {
            b'n' => b'\n',
            b'a' => b'\x07',
            b'f' => b'\x0C',
            b't' => b'\t',
            b'r' => b'\r',
            b'v' => b'\x0B',
            x => x,
        }
    }

    fn parse_class_item(
        tokens: &mut VecDeque<AsciiChar>,
    ) -> Result<ClassItem<AsciiChar>, RegexError> {
        match tokens.pop_front().ok_or(RegexError::Malformed)? {
            b'\\' => {
                let code = tokens.pop_front().ok_or(RegexError::Malformed)?;
                let escaped = Self::escape_codes(code);
                Ok(ClassItem::Char(escaped))
            }
            b'-' => {
                let range_end = tokens.pop_front().ok_or(RegexError::Malformed)?;
                Ok(ClassItem::RangeEnd(range_end))
            }
            x => Ok(ClassItem::Char(x)),
        }
    }

    fn parse_base(tokens: &mut VecDeque<AsciiChar>) -> Result<Regex, RegexError> {
        let front = tokens.pop_front().ok_or(RegexError::Malformed)?;
        if front == b'\\' {
            // funny name for backslash
            // escape next character
            let code = tokens.pop_front().ok_or(RegexError::Malformed)?;
            let escaped = Self::escape_codes(code);
            Ok(Regex::Char(escaped))
        } else if front == b'(' {
            let parenthesized_regex = Self::parse_regex(tokens)?;
            if tokens.pop_front().is_some_and(|x| x == b')') {
                Ok(parenthesized_regex)
            } else {
                Err(RegexError::Malformed)
            }
        } else if front == b'[' {
            let class = Self::parse_class(tokens)?;
            if tokens.pop_front().is_some_and(|x| x == b']') {
                Ok(Regex::Class(class))
            } else {
                Err(RegexError::Malformed)
            }
        } else if front == b'.' {
            let ranges = vec![(b'\0', b'\x7F')];
            let class: RegexClass = RegexClass {
                ranges,
                characters: vec![],
                negated: false,
            };
            Ok(Regex::Class(class))
        } else {
            Ok(Regex::Char(front))
        }
    }

    pub fn from_str(re_str: &str) -> Result<Regex, RegexError> {
        assert!(re_str.is_ascii());
        let mut tokens: VecDeque<AsciiChar> = re_str.as_bytes().iter().map(|x| *x).collect();
        let re = Self::parse_regex(&mut tokens)?;
        if tokens.len() > 0 {
            Err(RegexError::Malformed)
        } else {
            Ok(re)
        }
    }
}
