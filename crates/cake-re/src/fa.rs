use crate::{regex::Regex, AsciiChar};
use std::{
    collections::{HashMap, VecDeque},
    fmt::Debug,
    iter,
};

use bit_set::BitSet;
use serde::{Deserialize, Serialize};
use serde_binary::binary_stream::Endian;

// pointer-based graphs in safe rust are somewhat tricky, so just do indices to keep things simple
// None represents epsilon-move
#[derive(Debug, Clone)]
struct FANode {
    transitions: Vec<(Option<AsciiChar>, usize)>,
}

#[derive(Debug)]
// action is a way to identify final state in NFA->DFA conversion and specify priority, lower action = higher priority
// when constructing FA from multiple regexes, priority can be assigned to each one.
// -1 = not a final state
pub(super) struct FA {
    nodes: Vec<FANode>,
    initial_state: usize,
    accept_states: Vec<usize>,
    actions: Option<Vec<i32>>,
}

impl FA {
    // re + mutable nodes -> initial, accept state
    // invariant - push stuff into `nodes` before making pointers, otherwise memory unsafe
    fn recursive_helper(re: &Regex, nodes: &mut Vec<FANode>) -> (usize, usize) {
        match re {
            Regex::Alternation(alternates) => {
                let end: FANode = FANode {
                    transitions: Vec::new(),
                };

                nodes.push(end);
                let end_ptr: usize = nodes.len() - 1;

                let mut start_transitions: Vec<(Option<AsciiChar>, usize)> = Vec::new();
                for alternate in alternates {
                    let (alternate_start, alternate_end) = Self::recursive_helper(alternate, nodes);
                    nodes[alternate_end].transitions.push((None, end_ptr));
                    start_transitions.push((None, alternate_start));
                }

                let start: FANode = FANode {
                    transitions: start_transitions,
                };

                nodes.push(start);
                let start_ptr: usize = nodes.len() - 1;

                (start_ptr, end_ptr)
            }
            Regex::Concatenation(factors) => {
                let heads_tails: Vec<(usize, usize)> = factors
                    .into_iter()
                    .map(|x| Self::recursive_helper(x, nodes))
                    .collect();
                let start_ptr = heads_tails.first().expect("Factors must be nonempty").0;
                let end_ptr = heads_tails.last().expect("Factors must be nonempty").1;

                for i in 0..heads_tails.len() - 1 {
                    let this_tail = heads_tails[i].1;
                    let next_head = heads_tails[i + 1].0;
                    nodes[this_tail].transitions.push((None, next_head))
                }

                (start_ptr, end_ptr)
            }
            Regex::Kleene(inner) => {
                let (inner_start, inner_end) = Self::recursive_helper(inner.as_ref(), nodes);
                let end: FANode = FANode {
                    transitions: Vec::new(),
                };
                nodes.push(end);
                let end_ptr: usize = nodes.len() - 1;
                nodes[inner_end].transitions.push((None, inner_start));
                nodes[inner_end].transitions.push((None, end_ptr));

                let start: FANode = FANode {
                    transitions: vec![(None, inner_start), (None, end_ptr)],
                };
                nodes.push(start);
                let start_ptr: usize = nodes.len() - 1;
                (start_ptr, end_ptr)
            }
            Regex::Char(c) => {
                let end: FANode = FANode {
                    transitions: Vec::new(),
                };

                nodes.push(end);
                let end_ptr: usize = nodes.len() - 1;

                let start: FANode = FANode {
                    transitions: vec![(Some(*c), end_ptr)],
                };

                nodes.push(start);
                let start_ptr: usize = nodes.len() - 1;

                (start_ptr, end_ptr)
            }
            Regex::Class(class) => {
                let all_chars: BitSet = (0..128).collect();
                let mut included: BitSet = BitSet::with_capacity(128);

                for x in &class.characters {
                    included.insert(*x as usize);
                }
                for (low, high) in &class.ranges {
                    for x in (*low as usize)..=(*high as usize) {
                        included.insert(x);
                    }
                }

                if class.negated {
                    included.symmetric_difference_with(&all_chars);
                }

                let end: FANode = FANode {
                    transitions: Vec::new(),
                };
                nodes.push(end);
                let end_ptr = nodes.len() - 1;

                let transitions: Vec<(Option<AsciiChar>, usize)> =
                    included.iter().map(|x| (Some(x as u8), end_ptr)).collect();

                let start: FANode = FANode { transitions };
                nodes.push(start);
                let start_ptr = nodes.len() - 1;

                (start_ptr, end_ptr)
            }
            Regex::Empty => {
                let end = FANode {
                    transitions: Vec::new(),
                };

                nodes.push(end);
                let end_ptr = nodes.len() - 1;

                let start = FANode {
                    transitions: vec![(None, end_ptr)],
                };

                nodes.push(start);
                let start_ptr = nodes.len() - 1;

                (start_ptr, end_ptr)
            }
        }
    }

    // creates a NFA from a regex using Thompson's Construction
    // guaranteed to only have one accept state
    #[allow(dead_code)] // used in tests
    pub(super) fn nfa_from_re(re: &Regex) -> FA {
        let mut nodes: Vec<FANode> = Vec::new();

        let (start, end) = Self::recursive_helper(re, &mut nodes);
        FA {
            nodes,
            initial_state: start,
            accept_states: vec![end],
            actions: None,
        }
    }

    pub(super) fn combine_res(res: &[Regex]) -> FA {
        let mut nodes: Vec<FANode> = Vec::new();
        let heads_tails: Vec<_> = res
            .iter()
            .map(|re| Self::recursive_helper(re, &mut nodes))
            .collect();

        let transitions: Vec<(Option<_>, usize)> =
            heads_tails.iter().map(|(head, _)| (None, *head)).collect();

        let initial_state = FANode { transitions };

        nodes.push(initial_state);
        let initial_state = nodes.len() - 1;

        let accept_states: Vec<usize> = heads_tails.iter().map(|x| x.1).collect();

        let mut actions: Vec<i32> = iter::repeat(-1).take(nodes.len()).collect();

        for (priority, x) in accept_states.iter().copied().enumerate() {
            actions[x] = priority as i32;
        }
        let actions = Some(actions);

        FA {
            nodes,
            initial_state,
            accept_states,
            actions,
        }
    }

    // creates a DFA from an NFA using subset construction
    pub(super) fn dfa_from_nfa(nfa: &FA) -> FA {
        let accepting_states: BitSet = nfa.accept_states.iter().copied().collect();
        let mut accept_states_dfa: Vec<usize> = Vec::new();
        let mut actions_dfa: Vec<i32> = Vec::new();

        let mut initial_configuration: BitSet = BitSet::new();
        initial_configuration.insert(nfa.initial_state);

        // compute epsilon closure
        Self::epsilon_closure(&nfa.nodes, &mut initial_configuration);
        assert!(
            initial_configuration.is_disjoint(&accepting_states),
            "initial state should be epsilon disjoint from accepting states"
        );

        let mut id: usize = 0;

        // configuration -> id
        let mut subsets: HashMap<BitSet, usize> = HashMap::new();
        subsets.insert(initial_configuration.clone(), id);
        actions_dfa.push(-1);
        id += 1;

        let mut work_queue: VecDeque<BitSet> = VecDeque::new();
        work_queue.push_back(initial_configuration.clone());

        let mut transitions: HashMap<(BitSet, AsciiChar), BitSet> = HashMap::new();

        while !work_queue.is_empty() {
            let q = work_queue.pop_front().unwrap();
            for char in 0..128 {
                let mut t = Self::delta(&nfa.nodes, &q, char);
                if t.len() == 0 {
                    continue;
                }

                Self::epsilon_closure(&nfa.nodes, &mut t);
                transitions.insert((q.clone(), char), t.clone());

                let present = subsets.contains_key(&t);
                if !present {
                    if !t.is_disjoint(&accepting_states) {
                        accept_states_dfa.push(id);
                        if nfa.actions.is_some() {
                            let nfa_actions = nfa.actions.as_ref().unwrap();
                            let dfa_action = t
                                .iter()
                                .map(|x| nfa_actions[x])
                                .filter(|x| *x != -1)
                                .min()
                                .expect("t should contain accepting state");
                            actions_dfa.push(dfa_action);
                        }
                    } else {
                        actions_dfa.push(-1);
                    }
                    subsets.insert(t.clone(), id);
                    id += 1;

                    work_queue.push_back(t);
                }
            }
        }

        let mut nodes: Vec<FANode> = Vec::with_capacity(id);
        nodes.resize(
            id,
            FANode {
                transitions: vec![],
            },
        );
        // a transitions to b when c is character applied
        for ((a, c), b) in transitions.iter() {
            let a = subsets[a];
            let b = subsets[b];
            nodes[a].transitions.push((Some(*c), b));
        }

        FA {
            nodes,
            initial_state: 0,
            accept_states: accept_states_dfa,
            actions: nfa.actions.as_ref().map(|_| actions_dfa),
        }
    }

    // basic BFS to compute epsilon closure
    fn epsilon_closure(nodes: &[FANode], set: &mut BitSet) {
        let mut queue: VecDeque<usize> = set.iter().collect();
        let mut visited: BitSet = BitSet::with_capacity(set.capacity());

        while !queue.is_empty() {
            let i = queue.pop_front().unwrap();
            if visited.contains(i) {
                continue;
            }

            for (c, next) in &nodes[i].transitions {
                if c.is_none() && !visited.contains(*next) {
                    queue.push_back(*next);
                }
            }

            set.insert(i);
            visited.insert(i);
        }
    }

    fn delta(nodes: &[FANode], set: &BitSet, c: AsciiChar) -> BitSet {
        let mut result = BitSet::with_capacity(set.capacity());

        for i in set.iter() {
            for (label, next) in &nodes[i].transitions {
                if label.is_some_and(|x| x == c) {
                    result.insert(*next);
                }
            }
        }

        result
    }

    fn get_partition(partitions: &[BitSet], id: usize) -> usize {
        partitions
            .iter()
            .enumerate()
            .filter(|x| x.1.contains(id))
            .next()
            .expect("partition should cover domain")
            .0
    }

    // Splits a set of DFA states on a given character, helper function for DFA minimization
    fn split(partitions: &[BitSet], dfa: &FA, set: &BitSet, c: AsciiChar) -> (BitSet, BitSet) {
        let mut a = BitSet::new();
        let mut b = BitSet::new();

        // magic values: -1 for no transition with label ch,
        // i=0... means transition leads to partition i
        let mut action: i32 = -1;
        let mut first = true;
        for e in set.iter() {
            let node = &dfa.nodes[e];
            let mut no_transition = true;

            for (label, next) in &node.transitions {
                let label = label.expect("DFA must not have epsilon transitions");
                let next = (Self::get_partition(partitions, *next)) as i32;

                if label == c {
                    no_transition = false;
                    if action != next {
                        if first {
                            action = next;
                            a.insert(e);
                        } else {
                            // need to split
                            b.insert(e);
                        }
                    } else {
                        // join partition
                        a.insert(e);
                    }
                    break;
                }
            }

            if no_transition {
                if action == -1 {
                    a.insert(e);
                } else {
                    b.insert(e);
                }
            }
            first = false;
        }

        (a, b)
    }

    // use Hopcroft's Algorithm to minimize a DFA
    // works by splitting groups of states until reaching a fixed point to find all, finitely many equivalence classes (Myhill-Nerode)
    // a group of states is equivalent if it has same "behavior" on reading any given input character (i.e. all would transition to the same
    // equivalence class)
    pub(super) fn minimize_dfa(dfa: &FA, separate_finals: bool) -> FA {
        println!("Before minimization: {} states", dfa.nodes.len());

        let accept: BitSet = dfa.accept_states.iter().copied().collect();
        let mut nonaccept: BitSet = (0..dfa.nodes.len()).collect();
        nonaccept.difference_with(&accept);

        let mut b_partition: Vec<BitSet> = if separate_finals {
            let size = nonaccept.capacity();
            accept
                .iter()
                .map(|y| {
                    let mut x = BitSet::with_capacity(size);
                    x.insert(y);
                    x
                })
                .chain(iter::once(nonaccept))
                .collect()
        } else {
            vec![accept.clone(), nonaccept]
        };

        let mut a_partition: Vec<BitSet> = Vec::new();

        // println!("{:?}", a_partition);
        // println!("{:?}", b_partition);

        while a_partition != b_partition {
            std::mem::swap(&mut a_partition, &mut b_partition);

            b_partition.clear();
            for s in &a_partition {
                let mut did_split = false;
                for ch in 0..128 {
                    let (s1, s2) = Self::split(&a_partition, dfa, s, ch);
                    if s2.len() == 0 {
                        continue;
                    }

                    // println!("splitting on {:?}, {:?} / {:?}", ch, s1, s2);

                    b_partition.push(s1);
                    b_partition.push(s2);
                    did_split = true;
                    break;
                }

                if !did_split {
                    b_partition.push(s.clone());
                }
            }

            // println!("{:?}", a_partition);
            // println!("{:?}", b_partition);
            // panic!();
        }

        // println!("{:?}", a_partition);
        // println!("{:?}", b_partition);

        let mut minimal_nodes: Vec<FANode> = Vec::with_capacity(a_partition.len());
        minimal_nodes.resize(
            a_partition.len(),
            FANode {
                transitions: Vec::new(),
            },
        );

        for (i, p) in a_partition.iter().enumerate() {
            let first = p.iter().next().expect("partition should be nonempty");
            let node = &dfa.nodes[first];
            for (c, next) in &node.transitions {
                minimal_nodes[i]
                    .transitions
                    .push((*c, Self::get_partition(&a_partition, *next)))
            }
        }

        let initial_state: Vec<usize> = a_partition
            .iter()
            .enumerate()
            .filter(|x| x.1.contains(dfa.initial_state))
            .map(|x| x.0)
            .collect();

        assert!(initial_state.len() == 1, "partition should be disjoint");
        let initial_state = initial_state[0];

        let accept_states: Vec<usize> = a_partition
            .iter()
            .enumerate()
            .filter(|x| x.1.is_subset(&accept))
            .map(|x| x.0)
            .collect();

        // println!("{:?}", dfa.actions);
        // println!("{:?}", accept_states);
        let actions: Option<Vec<i32>> = dfa.actions.as_ref().map(|actions| {
            let mut new_actions: Vec<i32> = Vec::new();
            new_actions.resize(a_partition.len(), -1);

            for accept_state in accept_states.iter().copied() {
                let minimized_final = &a_partition[accept_state];
                // println!("{:?}", minimized_final);
                if minimized_final.len() > 1 {
                    println!(
                        "warning: {} accepting states were collapsed together when lexing actions are specified, \
                        this is probably unintended. use separate_finals to prevent minimization of accept states",
                        minimized_final.len()
                    );
                }

                // lower actions have higher priority, thus min()
                let new_action = minimized_final.iter()
                    .map(|x| actions[x])
                    .min()
                    .expect("actions should be nonempty");

                new_actions[accept_state] = new_action;
            }

            new_actions
        });

        println!("After minimization: {} states", minimal_nodes.len());
        FA {
            nodes: minimal_nodes,
            initial_state,
            accept_states,
            actions,
        }
    }

    pub(super) fn simulate_dfa(dfa: &FA, input: &str) -> (bool, Option<i32>) {
        assert!(input.is_ascii());
        let mut state = dfa.initial_state;
        let chars = input.as_bytes().iter().copied().map(|x| x);

        for input_char in chars {
            let current_state = &dfa.nodes[state];
            let mut found = false;
            for (ch, next) in &current_state.transitions {
                let c = ch.expect("DFA should not have epsilon transitions");
                if c == input_char {
                    found = true;
                    state = *next;
                    break;
                }
            }
            if !found {
                return (false, Some(-1));
            }
        }

        let accept = dfa.accept_states.contains(&state);
        let action = dfa.actions.as_ref().map(|actions| actions[state]);
        (accept, action)
    }

    // TODO: know this is possible by "ripping" states out within a GNFA
    // but maybe complicated to implement this way.
    // Not really needed for compiler
    pub(super) fn re_from_dfa() -> String {
        todo!()
    }
}

// Serialization format for DFA (and public interface with other code)
// states = rows, 1 additional error state
// characters = columns
// -1 = invalid transition
#[derive(Serialize, Deserialize)]
pub struct DFATable {
    pub data: Vec<u32>,
    pub actions: Vec<i32>,
    pub states: u32,
    pub initial_state: u32,
}

impl DFATable {
    pub fn get_next_state(&self, current_state: u32, ch: AsciiChar) -> u32 {
        self.data[(current_state * 128 + ch as u32) as usize]
    }

    // populate dense lookup table from sparse FA data structure
    // TODO: is this actually a reasonable thing to do? is maintaining sparsity when doing lexing actually better?
    fn from_ascii_dfa(dfa: &FA) -> DFATable {
        let num_states: u32 = dfa.nodes.len() as u32 + 1;
        let num_inputs: usize = 128;
        let table_size = num_states as usize * num_inputs;
        let mut data: Vec<u32> = Vec::with_capacity(table_size);
        data.resize(table_size, num_states - 1);

        // fill table
        for (i, state) in dfa.nodes.iter().enumerate() {
            for (label, next) in &state.transitions {
                let label = label.expect("DFA should not have epsilon transitions");
                let next = *next;
                data[i * num_inputs + label as usize] = next as u32;
            }
        }

        let mut actions = dfa
            .actions
            .as_ref()
            .expect("using DFA for table requires associated lexing actions")
            .clone();
        actions.push(-1); // add implicit error state

        DFATable {
            data,
            actions,
            states: num_states,
            initial_state: dfa.initial_state as u32,
        }
    }

    // convenience function to compile lexeme regexes to NFA, then DFA, then optimize, then create table
    // should mainly be used for testing code only - loading precompiled table is much faster
    pub fn from_regex_strs(re_strs: &[&str]) -> DFATable {
        let lexemes: Vec<Regex> = re_strs
            .iter()
            .map(|x| Regex::from_str(x).expect("failed to parse regex"))
            .collect();

        let nfa = FA::combine_res(&lexemes);
        let dfa = FA::dfa_from_nfa(&nfa);
        let dfa = FA::minimize_dfa(&dfa, true);

        DFATable::from_ascii_dfa(&dfa)
    }

    pub fn compile_table(&self) -> Vec<u8> {
        serde_binary::to_vec(self, Endian::Little).expect("Unable to serialize DFA table")
    }

    pub fn from_precompiled_table(bytes: &[u8]) -> DFATable {
        serde_binary::from_slice(bytes, Endian::Little)
            .expect("Unable to load serialized DFA table")
    }
}
