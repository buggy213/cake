// Naive Earley implementation
// TODO: optimize to run in worst-case O(n^3) rather than O(|G|n^3)
// https://julesjacobs.com/2019/05/09/earley-parsing-in-o-n-cubed.html
// in fact, current algorithm might be worse then n^3 due to linear time searches
// over state space when adding items to check for duplicates

use std::collections::HashMap;
use std::collections::HashSet;

use petgraph::adj::NodeIndex;
use petgraph::graph::DiGraph;
use petgraph::Graph;

use crate::scanner::lexemes::LexemeSet;
use crate::scanner::TokenStream;

use super::grammar::Grammar;
use super::grammar::Production;
use super::grammar::Symbol;
use super::grammar::NT;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
struct EarleyItem {
    dot: usize,
    back: usize,
    production: usize,
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
    nonterminal_productions: &Vec<Vec<usize>>,
    current_item: EarleyItem,
    nullable_nonterminals: &HashSet<NT>,
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

    if nullable_nonterminals.contains(&nt) {
        let advanced_current_state = EarleyItem {
            dot: current_item.dot + 1,
            ..current_item
        };
        add_state(current_state_set, advanced_current_state);
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
    grammar: &Grammar<T>,
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
            }
        }
    }
}

pub fn earley_parse<T: LexemeSet>(
    input: &mut impl TokenStream<T>,
    grammar: &Grammar<T>,
) -> SPPF<T> {
    let mut state_sets: Vec<Vec<EarleyItem>> = Vec::new();
    let mut sppf: SPPF<T> = HashMap::new();
    let nonterminal_productions = Grammar::compute_nonterminal_index(grammar);

    let goal = grammar.goal_symbol;
    let initial = nonterminal_productions[goal]
        .iter()
        .map(|x| EarleyItem {
            dot: 0,
            back: 0,
            production: *x,
        })
        .collect();

    state_sets.push(initial);
    let mut current_state: usize = 0;

    let nullable_nonterminals = Grammar::compute_nullable_nonterminals(grammar);

    loop {
        // println!("iteration {}", current_state);
        let next_token = input.advance();
        let mut next_state_set = Vec::new();
        let (previous_state_sets, current_state_set) = state_sets.split_at_mut(current_state);
        let current_state_set = &mut current_state_set[0];

        let mut i = 0;
        while i < current_state_set.len() {
            // println!("state {}", i);
            let earley_state = current_state_set[i];
            let production = &grammar.productions[earley_state.production];
            match production {
                Production::Empty(nt) => {
                    // can only do completion
                    complete(
                        earley_state,
                        *nt,
                        current_state_set,
                        previous_state_sets,
                        grammar,
                    );
                }
                Production::Nonempty(nt, rule) => {
                    if earley_state.dot >= rule.len() {
                        // complete
                        complete(
                            earley_state,
                            *nt,
                            current_state_set,
                            previous_state_sets,
                            grammar,
                        );
                    } else if let Some((lexeme, _, _)) = next_token {
                        match rule[earley_state.dot] {
                            Symbol::Terminal(t) => {
                                scan(earley_state, t, lexeme, &mut next_state_set);
                            }
                            Symbol::Nonterminal(nt) => {
                                predict(
                                    nt,
                                    current_state,
                                    current_state_set,
                                    &nonterminal_productions,
                                    earley_state,
                                    &nullable_nonterminals,
                                );
                            }
                            Symbol::EOF => panic!("earley grammar should not contain EOF"),
                        }
                    }
                }
            }
            i += 1;
        }

        if next_token.is_none() {
            break;
        } else {
            let terminal = Symbol::Terminal(next_token.unwrap().0);
            sppf.insert((terminal, current_state, current_state + 1), SPPFNode::Leaf);
        }
        state_sets.push(next_state_set);
        current_state += 1;
    }

    construct_parse_trees(grammar, state_sets, &mut sppf);
    sppf
}

type SPPF<T> = HashMap<SPPFNodeKey<T>, SPPFNode<T>>;
type SPPFNodeKey<T> = (Symbol<T>, usize, usize);
type Derivation<T> = (usize, Vec<SPPFNodeKey<T>>);

#[derive(Debug)]
pub enum SPPFNode<T: LexemeSet> {
    Leaf,
    Inner { derivations: Vec<Derivation<T>> },
}

// impl<T: LexemeSet> SPPFNode<T> {
//     fn get_key(&self) -> SPPFNodeKey<T> {
//         (self.symbol, self.extent.0, self.extent.1)
//     }
// }

// This can be done while building up the earley sets as an optimization
// however this should be fast enough for now
// also, in worst case the parse tree is really big, but hopefully that doesn't occur
// too much for a relatively unambiguous grammar

// 2 mutually recursive functions
// 1. given an earley item + extent, places all possible derivations into SPPF. uses DFS as a helper to do this
// 2. DFS:
fn construct_parse_trees<T: LexemeSet>(
    grammar: &Grammar<T>,
    earley_sets: Vec<Vec<EarleyItem>>,
    sppf: &mut SPPF<T>,
) {
    let mut reversed_earley_sets: Vec<Vec<EarleyItem>> = Vec::with_capacity(earley_sets.len());
    reversed_earley_sets.resize_with(earley_sets.len(), Default::default);

    let mut roots: Vec<EarleyItem> = Vec::new();
    let num_sets = earley_sets.len();
    for (i, earley_set) in earley_sets.into_iter().enumerate() {
        for item in earley_set {
            let EarleyItem {
                back,
                dot,
                production,
            } = item;

            let production = &grammar.productions[production];
            if dot < production.len() {
                continue;
            }

            let reversed_item = EarleyItem { back: i, ..item };
            reversed_earley_sets[back].push(reversed_item);

            if production.nonterminal() == grammar.goal_symbol && back == 0 && i == num_sets - 1 {
                roots.push(reversed_item)
            }
        }
    }

    for root in roots {
        let key = (Symbol::Nonterminal(grammar.goal_symbol), 0, num_sets - 1);
        recursive_helper(sppf, grammar, &reversed_earley_sets, root, key);
    }
}

fn recursive_helper<T: LexemeSet>(
    sppf: &mut SPPF<T>,
    grammar: &Grammar<T>,
    earley_sets: &Vec<Vec<EarleyItem>>,

    item: EarleyItem,
    node: SPPFNodeKey<T>,
) {
    // check for duplicates
    if sppf.contains_key(&node) {
        return;
    }
    // println!("Helper: {:?}, {:?}", item, node);
    sppf.insert(
        node,
        SPPFNode::Inner {
            derivations: Vec::new(),
        },
    );
    let mut trace = Vec::new();
    let (_, start, _) = node;
    dfs_helper(
        sppf,
        grammar,
        earley_sets,
        node,
        item.production,
        start,
        &mut trace,
    );
}

fn dfs_helper<T: LexemeSet>(
    sppf: &mut SPPF<T>,
    grammar: &Grammar<T>,
    earley_sets: &Vec<Vec<EarleyItem>>,
    sppf_key: SPPFNodeKey<T>,
    rule: usize,

    position: usize,
    trace: &mut Vec<SPPFNodeKey<T>>,
) {
    // println!("DFS: {:?}, {:?}, {:?}, {:?}", rule, position, sppf_key, trace);
    // 1. if position > target, we've overshot, return out

    // 2. if position == target && depth == |RHS|, then this search has succeeded, so
    // copy trace into SPPF as a possible derivation

    // 3. if position < target && depth == |RHS|, this search didn't succeed, return out

    // 4. if position < target && depth < |RHS|, then we need to continue searching
    // given that we are decomposing production p = p0 p1 ... pk
    // let length of trace be j, current position x
    // if p_j is terminal, check that input[x] matches it (sppf should contain terminals already)
    // - if it does not, return out
    // if p_j is nonterminal, then look in earley_sets[x] for any earley items with p_j as their LHS
    // - each one defines an out edge and an extent (an item in the SPPF)
    //      - call recursive_helper on that earley item (SPPF acts as "visited set", duplicate visits handled)
    //      - push SPPF key onto trace
    //      - recursively call dfs_helper
    //      - pop SPPF key off of trace
    let (_, _, target) = sppf_key;
    let production = &grammar.productions[rule];
    let rhs = match production {
        Production::Empty(_) => {
            let sppf_node = sppf.get_mut(&sppf_key).unwrap();
            if let SPPFNode::Inner { derivations } = sppf_node {
                derivations.push((rule, trace.clone()));
            } else {
                panic!("dfs should only be called on inner nodes");
            }
            return;
        }
        Production::Nonempty(_, rhs) => rhs,
    };

    if position > target {
        return;
    } else if position == target && trace.len() == rhs.len() {
        let sppf_node = sppf.get_mut(&sppf_key).unwrap();
        if let SPPFNode::Inner { derivations } = sppf_node {
            derivations.push((rule, trace.clone()));
        } else {
            panic!("dfs should only be called on inner nodes");
        }
        return;
    } else if position < target && trace.len() == rhs.len() {
        return;
    }

    // possibility of empty productions
    assert!(position <= target && trace.len() < rhs.len());

    let depth = trace.len();
    let next_symbol = rhs[depth];
    match next_symbol {
        Symbol::Terminal(_t) => {
            let key = (next_symbol, position, position + 1);
            if sppf.contains_key(&key) {
                trace.push(key);
                dfs_helper(
                    sppf,
                    grammar,
                    earley_sets,
                    sppf_key,
                    rule,
                    position + 1,
                    trace,
                );
                assert!(trace.pop().unwrap() == key);
                return;
            } else {
                return;
            }
        }
        Symbol::Nonterminal(nt) => {
            for item in earley_sets[position].iter().copied() {
                let item_nt = grammar.productions[item.production].nonterminal();
                if item_nt != nt {
                    continue;
                }
                let key = (next_symbol, position, item.back);
                recursive_helper(sppf, grammar, earley_sets, item, key);

                trace.push(key);
                dfs_helper(sppf, grammar, earley_sets, sppf_key, rule, item.back, trace);
                assert!(trace.pop().unwrap() == key);
            }
        }
        Symbol::EOF => todo!(),
    }
}

type SPPFGraph<T> = Graph<Option<SPPFNodeKey<T>>, ()>;
pub fn sppf_to_graph<T: LexemeSet>(sppf: &SPPF<T>, grammar: &Grammar<T>) -> Graph<String, ()> {
    let mut node_to_index: HashMap<SPPFNodeKey<T>, _> = HashMap::new();
    let mut graph: SPPFGraph<T> = DiGraph::new();
    for (k, v) in sppf.iter() {
        match v {
            SPPFNode::Leaf => {
                let index = graph.add_node(Some(*k));
                node_to_index.insert(*k, index);
            }
            SPPFNode::Inner { .. } => {
                let index = graph.add_node(Some(*k));
                node_to_index.insert(*k, index);
            }
        }
    }

    for (k, v) in sppf.iter() {
        match v {
            SPPFNode::Leaf => {
                continue;
            }
            SPPFNode::Inner { derivations } => {
                let index = node_to_index[k];
                if derivations.len() == 1 {
                    let (_rule, derivation) = &derivations[0];
                    for child in derivation {
                        let child_index = node_to_index[child];
                        graph.add_edge(index, child_index, ());
                    }
                } else {
                    for (_rule, derivation) in derivations {
                        let tmp = graph.add_node(None);
                        graph.add_edge(index, tmp, ());
                        for child in derivation {
                            let child_index = node_to_index[child];
                            graph.add_edge(tmp, child_index, ());
                        }
                    }
                }
            }
        }
    }

    graph.map(
        |_, x| -> String {
            match x {
                Some((symbol, start, end)) => match symbol {
                    Symbol::Terminal(t) => {
                        format!("{:?}, {}, {}", t, start, end)
                    }
                    Symbol::Nonterminal(nt) => {
                        if *nt >= grammar.nonterminal_names.len() {
                            format!("<intermediate {}>, {}, {}", *nt, start, end)
                        } else {
                            format!("{}, {}, {}", grammar.nonterminal_names[*nt], start, end)
                        }
                    }
                    Symbol::EOF => todo!(),
                },
                None => "Packed".to_string(),
            }
        },
        |_, _| (),
    )
}

pub fn make_graph_printers<T: LexemeSet>() -> (
    Box<dyn Fn(&SPPFGraph<T>, (NodeIndex, &Option<SPPFNodeKey<T>>)) -> String>,
    Box<dyn Fn(&SPPFGraph<T>, u32) -> String>,
) {
    todo!()
}
