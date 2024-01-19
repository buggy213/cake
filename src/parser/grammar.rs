use std::{marker::PhantomData, collections::{HashMap, VecDeque, HashSet}, ops::Deref, iter, ptr::null};

use bit_set::BitSet;

use crate::{scanner::lexemes::LexemeSet, util::RangeUInt};

pub type NT = usize;

// everything is just indices since it is simpler, but maybe less safe
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Symbol<T: LexemeSet> {
    Terminal(T),
    Nonterminal(NT),
    EOF
}

#[derive(Debug)]
pub enum Production<T: LexemeSet> {
    Empty(NT),
    Nonempty(NT, Vec<Symbol<T>>)
}

impl<T: LexemeSet> Production<T> {
    pub fn nonterminal(&self) -> NT {
        match self {
            Production::Empty(nt) => *nt,
            Production::Nonempty(nt, _) => *nt,
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Production::Empty(_) => 0,
            Production::Nonempty(_, rhs) => rhs.len(),
        }
    }
}

#[derive(Debug)]
pub struct Grammar<T: LexemeSet> {
    pub(super) n_nonterminals: usize,
    pub(super) productions: Vec<Production<T>>,
    pub(super) goal_symbol: NT,
    pub(super) nonterminal_names: Vec<String>
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

    // n^2 in size of grammar, more efficient algorithm does exist
    // TODO: implement later
    pub fn compute_nullable_nonterminals(grammar: &Grammar<T>) -> HashSet<NT> {
        let mut nullables: HashSet<usize> = HashSet::new();
        loop {
            let mut changed = false;
            for rule in &grammar.productions {
                match rule {
                    Production::Empty(nt) => {
                        changed |= nullables.insert(*nt);
                    },
                    Production::Nonempty(nt, p) =>  {
                        let all_nullable = p.iter().all(|x| match x {
                            Symbol::Terminal(_) => false,
                            Symbol::Nonterminal(nt) => nullables.contains(nt),
                            Symbol::EOF => true,
                        });

                        if all_nullable {
                            changed |= nullables.insert(*nt);
                        }
                    },
                }
            }

            if !changed {
                break;
            }
        }

        nullables
    }
}

#[derive(Clone, Debug)]
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
    // 2 mutually recursive functions 
    // output: a production rule
    fn recursive_helper_production_rule(
        name_to_nonterminal: &mut HashMap<&str, usize>,
        nonterminal_to_name: &mut Vec<&str>,
        productions: &mut Vec<Production<T>>,
        associated_nonterminal: NT,
        rule: &EBNF,
    ) -> Result<(), BNFError> {
        let production = match rule {
            EBNF::Concatenation(symbols) => {
                let mut new_rule: Vec<Symbol<T>> = Vec::new();
                for s in symbols {
                    let s = Self::recursive_helper_symbol(
                        name_to_nonterminal, 
                        nonterminal_to_name,
                        productions,
                        associated_nonterminal, 
                        s
                    )?;
                    new_rule.push(s);
                }
                Production::Nonempty(associated_nonterminal, new_rule)
            },
            EBNF::Identifier(x) => {
                return Err(BNFError::Malformed("unexpected identifier node"));
            },
            EBNF::Empty => {
                Production::Empty(associated_nonterminal)
            },
            EBNF::Repeat(clause, low, high) => {
                return Err(BNFError::Malformed("unexpected repeat node"));
                
                // let clause = Self::recursive_helper_symbol(
                //     name_to_nonterminal, 
                //     nonterminal_to_name, 
                //     clause
                // )?;

                // Production::Nonempty(associated_nonterminal, vec![clause]);
                // todo!()
            },
            EBNF::Nonterminal(_, _) => {
                return Err(BNFError::Malformed("unexpected nonterminal node"));
            },
        };

        productions.push(production);
        Ok(())
    }

    // output: symbol (potentially a new nonterminal based on rewrite rules)
    fn recursive_helper_symbol(
        name_to_nonterminal: &mut HashMap<&str, usize>,
        nonterminal_to_name: &mut Vec<&str>,
        productions: &mut Vec<Production<T>>,
        associated_nonterminal: NT,
        symbol: &EBNF,
    ) -> Result<Symbol<T>, BNFError> {
        let result = match symbol {
            EBNF::Concatenation(_) => {
                // rewrite rule 1: replace (...) with X -> ... where X is a new nonterminal
                // 1. create new nonterminal - to avoid dealing with lifetimes, just give them all the same name
                let nt = nonterminal_to_name.len();
                nonterminal_to_name.push("<anonymous-concat>");

                // 2. use other recursive function to create a production rule
                let rule = symbol;
                let _ = Self::recursive_helper_production_rule(
                    name_to_nonterminal, 
                    nonterminal_to_name, 
                    productions, 
                    nt, 
                    rule
                )?;

                Symbol::Nonterminal(nt)
            },
            EBNF::Identifier(x) => {
                let symbol = if let Some(name) = name_to_nonterminal.get(x.as_str()) {
                    Symbol::Nonterminal(*name)
                }
                else if let Some(terminal) = T::from_name(x.as_str()) {
                    Symbol::Terminal(terminal)
                }
                else {
                    return Err(BNFError::Malformed("unable to find matching name or terminal"));
                };

                symbol
            },
            EBNF::Empty => {
                return Err(BNFError::Malformed("unexpected empty node (should only occur at top level productions)"))
            },
            EBNF::Repeat(clause, low, high) => {
                // rewrite rule 2: repetition
                let clause_symbol = Self::recursive_helper_symbol(
                    name_to_nonterminal, 
                    nonterminal_to_name, 
                    productions, 
                    associated_nonterminal, 
                    &clause
                )?;

                let new_nt = nonterminal_to_name.len();
                let new_nt_symbol = Symbol::Nonterminal(new_nt);
                nonterminal_to_name.push("<anonymous-repeat>");


                match (*low, *high) {
                    // Cloning should be ok since EBNF restricts to just 0 / 1 / infinity as bounds
                    // only issue is if things are nested too deeply, but the whole point of 
                    // EBNF is to avoid this through composition...
                    (RangeUInt::Finite(low), RangeUInt::Finite(high)) => {
                        for i in low..=high {
                            if i == 0 {
                                productions.push(Production::Empty(new_nt));
                            }
                            else {
                                productions.push(Production::Nonempty(new_nt, vec![clause_symbol; i as usize]));
                            }
                        }
                    },
                    (RangeUInt::Finite(low), RangeUInt::Infinite) => {
                        if low == 0 {
                            productions.push(Production::Empty(new_nt));
                        }
                        else {
                            productions.push(Production::Nonempty(new_nt, vec![clause_symbol; low as usize]));
                        }
                        // repeat production; prefer left recursion (naive Earley parser is n^2 on right recursion,
                        // so this should help a little bit. won't be suitable for any LL though)
                        productions.push(Production::Nonempty(new_nt, vec![new_nt_symbol, clause_symbol]))
                    },
                    _ => return Err(BNFError::Malformed("infinite lower bound should not occur"))
                }

                new_nt_symbol
            },
            EBNF::Nonterminal(_, _) => {
                return Err(BNFError::Malformed("unexpected nonterminal node"));
            },
        };

        Ok(result)
    }

    pub fn from_ebnf(ebnf: &Vec<EBNF>) -> Result<Grammar<T>, BNFError> {
        let mut name_to_nonterminal: HashMap<&str, usize> = HashMap::new();
        let mut nonterminal_to_name: Vec<&str> = Vec::new();

        let mut i: usize = 0;

        // pass 1: collect names - ebnf is order invariant
        for nonterminal in ebnf {
            if let EBNF::Nonterminal(name, _) = nonterminal {
                name_to_nonterminal.insert(&name, i);
                nonterminal_to_name.push(&name);
                i += 1;
            }
            else {
                return Err(BNFError::Malformed("top level must be Nonterminal variant"));
            }
        }

        let mut productions: Vec<Production<T>> = Vec::new();
        let goal_symbol: usize = 0;
        // pass 2: convert EBNF to grammar
        for (nt_index, nonterminal) in ebnf.iter().enumerate() {
            if let EBNF::Nonterminal(_, production_rules) = nonterminal {
                for production in production_rules {
                    let _ = Self::recursive_helper_production_rule(
                        &mut name_to_nonterminal, 
                        &mut nonterminal_to_name, 
                        &mut productions, 
                        nt_index, 
                        production
                    )?;
                }
            }
            else {
                return Err(BNFError::Malformed("top level must be Nonterminal variant"));
            }
        }

        let n_nonterminals = nonterminal_to_name.len();
        Ok(Grammar {
            n_nonterminals,
            productions,
            goal_symbol,
            nonterminal_names: nonterminal_to_name.into_iter().map(|x| x.to_string()).collect()
        })
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
        
        let production = Self::parse_production(toks, true)?;
        productions.push(production);
        while toks.front().is_some() {
            if toks.pop_front().unwrap() != "|" {
                // println!("{:?}", toks);
                return Err(BNFError::Malformed("expected | (new production)"));
            }
            let production = Self::parse_production(toks, true)?;
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

    fn parse_production(toks: &mut VecDeque<&str>, top_level: bool) -> Result<EBNF, BNFError> {
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
            if top_level {
                Ok(EBNF::Empty) // empty production
            }
            else {
                Err(BNFError::Malformed("empty terms not allowed unless part of a top-level production rule"))
            }
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
            let result = Self::parse_production(toks, false)?;
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