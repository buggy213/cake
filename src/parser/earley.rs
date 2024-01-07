// Naive Earley implementation
// TODO: optimize to run in worst-case O(n^3) rather than O(|G|n^3)
// https://julesjacobs.com/2019/05/09/earley-parsing-in-o-n-cubed.html
// in fact, current algorithm might be worse then n^3 due to linear time searches
// over state space when adding items to check for duplicates

use crate::scanner::TokenStream;
use crate::scanner::lexemes::LexemeSet;

use super::grammar::Grammar;
use super::grammar::NT;
use super::grammar::Production;
use super::grammar::Symbol;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct EarleyItem {
    dot: usize,
    back: usize,
    production: usize
}

// possible optimization: original paper mentions splitting state set based on
// backpointer, thus limiting search to constant time operation
fn add_state(state_set: &mut Vec<EarleyItem>, state: EarleyItem) {
    for item in state_set.iter() {
        if *item == state {
            return;
        }
    }
    state_set.push(state);
}

fn predict(
    nt: NT, 
    current_state: usize, 
    current_state_set: &mut Vec<EarleyItem>, 
    nonterminal_productions: &Vec<Vec<usize>>
) {
    
    let nt_productions = &nonterminal_productions[nt];
    for production in nt_productions {
        let new_item = EarleyItem {
            dot: 0,
            back: current_state,
            production: *production,
        };
        
        add_state(current_state_set, new_item);
    }

}

fn scan<T: LexemeSet>(item: EarleyItem, a: T, b: T, next_state_set: &mut Vec<EarleyItem>) {
    if a == b {
        let mut new_item = item;
        new_item.dot = item.dot + 1;

        next_state_set.push(new_item);
    }
}

fn complete<T: LexemeSet>(
    item: EarleyItem, 
    completed_nt: usize,
    current_state_set: &mut Vec<EarleyItem>, 
    previous_state_sets: &mut [Vec<EarleyItem>], 
    grammar: &Grammar<T>
) {
    let previous = item.back;

    for previous_state in &previous_state_sets[previous] {
        let p = previous_state.production;
        let j = previous_state.dot;
        match &grammar.productions[p] {
            Production::Empty(_) => continue, // nothing to do here
            Production::Nonempty(_, x) => {
                if j >= x.len() {
                    continue;
                }

                if let Symbol::Nonterminal(nt) = x[j] {
                    if nt == completed_nt {
                        // complete it
                        let completed = EarleyItem {
                            dot: j + 1,
                            back: previous_state.back,
                            production: p,
                        };
                        current_state_set.push(completed);
                    }
                }
            },
        }
    }
}

fn earley_parse<T: LexemeSet>(input: &mut impl TokenStream<T>, grammar: &Grammar<T>) {
    let mut state_sets: Vec<Vec<EarleyItem>> = Vec::new();
    let nonterminal_productions = Grammar::compute_nonterminal_index(grammar);
    
    let goal = grammar.goal_symbol;
    let initial = nonterminal_productions[goal].iter()
        .map(|x| EarleyItem {
            dot: 0,
            back: 0,
            production: *x,
        })
        .collect();
    
    state_sets.push(initial);
    let mut current_state: usize = 0;
    
    while let Some((lexeme, _, _)) = input.advance() {
        let mut next_state_set = Vec::new();
        let (previous_state_sets, current_state_set) = state_sets.split_at_mut(current_state);
        let current_state_set = &mut current_state_set[0];

        let mut i = 0;
        while i < current_state_set.len() {
            let earley_state = current_state_set[i];
            let production = &grammar.productions[earley_state.production];
            match production {
                Production::Empty(nt) => {
                    // can only do completion
                    complete(earley_state, 
                        *nt,
                        current_state_set, 
                        previous_state_sets, 
                        grammar
                    );
                },
                Production::Nonempty(nt, rule) => {
                    if earley_state.dot >= rule.len() {
                        // complete
                        complete(earley_state, 
                            *nt,
                            current_state_set, 
                            previous_state_sets, 
                            grammar
                        );
                    }
                    else {
                        match rule[earley_state.dot] {
                            Symbol::Terminal(t) => {
                                scan(
                                    earley_state, 
                                    t, 
                                    lexeme, 
                                    &mut next_state_set
                                );
                            },
                            Symbol::Nonterminal(nt) => {
                                predict(nt, 
                                    current_state, 
                                    current_state_set, 
                                    &nonterminal_productions
                                );
                            },
                            Symbol::EOF => panic!("earley grammar should not contain EOF"),
                        }
                    }
                },
            }
            i += 1;
        }

        state_sets.push(next_state_set);
        current_state += 1;
    }

}

