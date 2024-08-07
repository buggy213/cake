use std::collections::HashSet;

use cake_lex::LexemeSet;

use super::grammar::{NT, Grammar};

type LRState = usize;

enum LRAction {
    Invalid,
    Shift(LRState),
    Reduce(NT),
    Accept
}

enum LRStackEntry<T> {
    Sentinel,
    State(LRState),
    Terminal(T),
    Nonterminal(NT)
}

struct LRTables {
    n_states: usize,
    n_terminals: usize,
    n_nonterminals: usize,
    action: Vec<LRAction>,
    goto: Vec<Option<LRState>>
}

struct LR1Item<T: LexemeSet> {
    production: usize,
    dot_position: usize,
    lookahead: Option<T> // None encodes EOF
}

impl LRTables {
    // Canonical LR(1) table construction
    fn closure() {

    }
    
    fn from_grammar<T: LexemeSet>(grammar: Grammar<T>) {
        let n_terminals = T::size();
        let n_nonterminals = grammar.n_nonterminals;

        
    }
}