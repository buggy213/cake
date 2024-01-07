use std::{marker::PhantomData, collections::{HashMap, VecDeque, HashSet}, ops::Deref, iter};

use bit_set::BitSet;

use crate::{scanner::lexemes::LexemeSet, util::RangeUInt};

pub type NT = usize;

// everything is just indices since it is simpler, but maybe less safe
pub enum Symbol<T: LexemeSet> {
    Terminal(T),
    Nonterminal(NT),
    EOF
}

pub enum Production<T: LexemeSet> {
    Empty(NT),
    Nonempty(NT, Vec<Symbol<T>>)
}

pub struct Grammar<T: LexemeSet> {
    pub(super) n_nonterminals: usize,
    pub(super) productions: Vec<Production<T>>,
    pub(super) goal_symbol: NT,
}

#[derive(Clone)]
pub struct FirstSet<T: LexemeSet> {
    // |T| + 2 entries, one for EOF and one for epsilon
    data: BitSet,
    _type: PhantomData<T>
}

impl<T: LexemeSet> FirstSet<T> {
    pub fn new() -> FirstSet<T> {
        FirstSet { 
            data: BitSet::with_capacity(T::size() as usize + 2), 
            _type: Default::default() 
        }
    }
    
    pub fn contains_terminal(&self, terminal: T) -> bool {
        self.data.contains(terminal.to_id() as usize)
    }

    pub fn contains_eof(&self) -> bool {
        self.data.contains(T::size() as usize)
    }

    pub fn contains_empty(&self) -> bool {
        self.data.contains(T::size() as usize + 1)
    }
}

impl<T: LexemeSet> Grammar<T> {
    pub fn compute_nonterminal_index(grammar: &Grammar<T>) -> Vec<Vec<usize>> {
        let mut map: Vec<Vec<usize>> = Vec::new();
        map.resize_with(grammar.n_nonterminals, || Vec::new());
        for (i, production) in grammar.productions.iter().enumerate() {
            match production {
                Production::Empty(nt) | Production::Nonempty(nt, _) => {
                    map[*nt].push(i);
                },
            }
        }
        map
    }
    
    pub fn compute_first_sets(grammar: &Grammar<T>) -> Vec<FirstSet<T>> {
        todo!()
    }
}

#[derive(Debug)]
pub enum EBNF {
    Concatenation(Vec<EBNF>),
    Identifier(String),
    Empty,
    
    Repeat(Box<EBNF>, RangeUInt, RangeUInt),
    Nonterminal(String, Vec<EBNF>)
}

#[derive(Clone, Copy, Debug)]
pub enum BNFError {
    Malformed(&'static str),
    UnexpectedEOF,
}

impl<T: LexemeSet> Grammar<T> {
    // output: index of nonterminal produced
    fn recursive_helper(
        nonterminals: &mut HashMap<&str, usize>
    ) -> usize {
        todo!()
    }

    pub fn from_ebnf(ebnf: &Vec<EBNF>) -> Result<Grammar<T>, BNFError> {
        let mut symbol_to_nonterminal: HashMap<&str, usize> = HashMap::new();
        let mut nonterminal_to_symbol: Vec<&str> = Vec::new();

        let mut i: usize = 0;

        // pass 1: collect names - ebnf is order invariant
        for nonterminal in ebnf {
            if let EBNF::Nonterminal(name, _) = nonterminal {
                symbol_to_nonterminal.insert(&name, i);
                nonterminal_to_symbol.push(&name);
                i += 1;
            }
            else {
                return Err(BNFError::Malformed("top level must be Nonterminal variant"));
            }
        }

        let mut productions: Vec<Production<T>> = Vec::new();
        let goal_symbol: usize = 0;
        // pass 2: convert EBNF to grammar
        for nonterminal in ebnf {
            
        }
        todo!()
    }
}

/// another small hand parser for EBNF
/// Grammar: 
/// 
/// (* other than this, whitespace is only used to delimit symbols *)
/// <ebnf> ::= <goal-nonterminal> ("\n\n" <nonterminal>)* 
///
/// <nonterminal> ::= <symbol> "::=" <productions>
///
/// <productions> ::= <production> ("|" <production>)*
/// 
/// <production> ::= <factor>* (* name is a little confusing... *)
///
/// <factor> ::= <base> ("+"|"*"|"?")*
///  
/// <base> ::= <symbol>
/// | "(" <production> ")"
impl EBNF {
    pub fn from_str(s: &str) -> Result<Vec<EBNF>, BNFError> {
        Self::parse_ebnf(s)
    }

    fn parse_ebnf(input: &str) -> Result<Vec<EBNF>, BNFError> {
        let mut nonterminals: Vec<EBNF> = Vec::new();
        
        // windows :sob:
        let nonterminal_chunks = input.split("\r\n\r\n")
            .map(|s| s.split("\n\n"))
            .flatten();

        for nonterminal in nonterminal_chunks {
            if nonterminal.trim().is_empty() {
                continue;
            }
            let special = |c: char| match c { 
                '(' | ')' | '|' | '+' | '*' | '?' => true,
                _ => false 
            };
            println!("'{}'", nonterminal);
            // super basic lexing
            let mut toks: VecDeque<&str> = nonterminal.split_whitespace()
                .map(|s| {
                    let mut result = Vec::new();
                    let mut last = 0;
                    for (index, matched) in s.match_indices(special) {
                        if last != index {
                            result.push(&s[last..index]);
                        }
                        result.push(matched);
                        last = index + matched.len();
                    }
                    if last < s.len() {
                        result.push(&s[last..]);
                    }
                    result.into_iter()
                })
                .flatten()
                .collect();

            // println!("{:?}", toks);
            let nonterminal = Self::parse_nonterminal(&mut toks)?;
            nonterminals.push(nonterminal);
        }

        Ok(nonterminals)
    }

    fn parse_nonterminal(toks: &mut VecDeque<&str>) -> Result<EBNF, BNFError> {
        let symbol = toks.pop_front()
            .ok_or(BNFError::UnexpectedEOF)?;

        let missing_delimiter = BNFError::Malformed("didn't see ::= delimiter");
        if !(toks.pop_front().ok_or(missing_delimiter)? == "::=") {
            return Err(missing_delimiter);
        }

        let mut productions = Vec::new();
        
        let production = Self::parse_production(toks)?;
        productions.push(production);
        while toks.front().is_some() {
            if toks.pop_front().unwrap() != "|" {
                // println!("{:?}", toks);
                return Err(BNFError::Malformed("expected | (new production)"));
            }
            let production = Self::parse_production(toks)?;
            productions.push(production);
        }

        if productions.len() == 0 {
            return Err(BNFError::Malformed("nonterminal must have >= 1 production"));
        }

        Ok(EBNF::Nonterminal(
            symbol.to_string(),
            productions
        ))
    }

    fn parse_production(toks: &mut VecDeque<&str>) -> Result<EBNF, BNFError> {
        let mut factors: Vec<EBNF> = Vec::new();
        
        while toks.front().is_some() {
            let lookahead = *toks.front().unwrap();
            if lookahead == ")" || lookahead == "|" {
                break;
            }
            let factor = Self::parse_factor(toks)?;
            factors.push(factor);
        }
        
        if factors.len() == 0 {
            Ok(EBNF::Empty) // empty production
        }
        else if factors.len() == 1 {
            Ok(factors.pop().unwrap())
        }
        else {
            Ok(EBNF::Concatenation(factors))
        }
    }

    fn parse_factor(toks: &mut VecDeque<&str>) -> Result<EBNF, BNFError> {
        let base = Self::parse_base(toks)?;
        let count = Self::parse_count(toks)?;

        let result = match count {
            (RangeUInt::Finite(low), RangeUInt::Finite(high)) if low == 1 && high == 1 => base,
            (low, high) => EBNF::Repeat(Box::new(base), low, high)
        };

        Ok(result)
    }

    fn parse_count(toks: &mut VecDeque<&str>) -> Result<(RangeUInt, RangeUInt), BNFError> {
        let mut low = RangeUInt::Finite(1);
        let mut high = RangeUInt::Finite(1);

        while toks.front().is_some() {
            let lookahead = toks.front().copied().unwrap();
            match lookahead {
                "*" => { low = RangeUInt::Finite(0); high = RangeUInt::Infinite },
                "?" => { low = RangeUInt::Finite(0); },
                "+" => { high = RangeUInt::Infinite; }
                _ => break
            }
            toks.pop_front();
        }

        Ok((low, high))
    }

    fn parse_base(toks: &mut VecDeque<&str>) -> Result<EBNF, BNFError> {
        let lookahead = toks.front().copied().ok_or(BNFError::UnexpectedEOF)?;
        let result = if lookahead == "(" {
            toks.pop_front();
            let result = Self::parse_production(toks)?;
            toks.pop_front().ok_or(BNFError::Malformed("expecting )"))?;
            result
        }
        else {
            toks.pop_front();
            EBNF::Identifier(lookahead.to_string())
        };

        Ok(result)
    }    
}