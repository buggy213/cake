use std::{collections::{VecDeque, HashSet, HashMap}, fmt::Debug};

use crate::scanner::alphabet::AsciiCharIterator;

use self::alphabet::AsciiChar;
use bit_set::BitSet;

mod alphabet;

// super basic regex implementation for learning purposes
// with following grammar (EBNF)
// <regex> ::= <term> '|' <regex>
// |  <term>

// <term> ::= { <factor> }

// <factor> ::= <base> { '*' }
 
// <base> ::= <char>
// |  '\' <char>
// |  '(' <regex> ')'

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

// pointer-based graphs in safe rust are somewhat tricky, so just do indices to keep things simple
#[derive(Debug, Clone)]
pub struct FANode<A: Eq> {
    transitions: Vec<(Option<A>, usize)>
}

#[derive(Debug)]
pub struct FA<A: Eq + Copy> {
    nodes: Vec<FANode<A>>,
    initial_state: usize,
    accept_states: Vec<usize>
}

impl FA<AsciiChar> {
    // creates a NFA from a regex using Thompson's Construction
    // guaranteed to only have one accept state
    pub fn nfa_from_re(re: Regex<AsciiChar>) -> FA<AsciiChar> {
        let mut nodes: Vec<FANode<AsciiChar>> = Vec::new();

        // re + mutable nodes -> initial, accept state 
        // invariant - push stuff into `nodes` before making pointers, otherwise memory unsafe
        fn recursive_helper<A: Eq>(re: Regex<A>, nodes: &mut Vec<FANode<A>>) 
            -> (usize, usize) 
        {
            match re {
                Regex::Alternation(alternates) => {
                    let end: FANode<A> = FANode {
                        transitions: Vec::new()
                    };

                    nodes.push(end);
                    let end_ptr: usize = nodes.len() - 1;

                    let mut start_transitions: Vec<(Option<A>, usize)> = Vec::new();
                    for alternate in alternates {
                        let (alternate_start, alternate_end) = recursive_helper(alternate, nodes);
                        nodes[alternate_end].transitions.push((None, end_ptr));
                        start_transitions.push((None, alternate_start));
                    }

                    let start: FANode<A> = FANode {
                        transitions: start_transitions
                    };

                    nodes.push(start);
                    let start_ptr: usize = nodes.len() - 1;
                    
                    (start_ptr, end_ptr)
                },
                Regex::Concatenation(factors) => {
                    let heads_tails: Vec<(usize, usize)> = factors.into_iter().map(|x| recursive_helper(x, nodes)).collect();
                    let start_ptr = heads_tails.first().unwrap().0;
                    let end_ptr = heads_tails.last().unwrap().1;

                    for i in 0..heads_tails.len() - 1 {
                        let next_head = heads_tails[i+1].0;
                        nodes[heads_tails[i].1].transitions.push((None, next_head))
                    }

                    (start_ptr, end_ptr)
                },
                Regex::Kleene(inner) => {
                    let (inner_start, inner_end) = recursive_helper(*inner, nodes);
                    let end: FANode<A> = FANode {
                        transitions: Vec::new()
                    };
                    nodes.push(end);
                    let end_ptr: usize = nodes.len() - 1;
                    nodes[inner_end].transitions.push((None, inner_start));
                    nodes[inner_end].transitions.push((None, end_ptr));
                    

                    let start: FANode<A> = FANode {
                        transitions: vec![(None, inner_start), (None, end_ptr)]
                    };
                    nodes.push(start);
                    let start_ptr: usize = nodes.len() - 1;
                    (start_ptr, end_ptr)
                },
                Regex::Char(c) => {
                    let end: FANode<A> = FANode {
                        transitions: Vec::new()
                    };

                    nodes.push(end);
                    let end_ptr: usize = nodes.len() - 1;

                    let start: FANode<A> = FANode {
                        transitions: vec![
                            (Some(c), end_ptr)
                        ],
                    };

                    nodes.push(start);
                    let start_ptr: usize = nodes.len() - 1;

                    (start_ptr, end_ptr)
                }
            }
        }
        
        let (start, end) = recursive_helper(re, &mut nodes);
        FA { nodes, initial_state: start, accept_states: vec![end] }
    }

    // creates a DFA from an NFA using subset construction
    pub fn dfa_from_nfa(nfa: &FA<AsciiChar>) -> FA<AsciiChar> {
        let accepting_states: BitSet = nfa.accept_states.iter().copied().collect();
        let mut accept_states_dfa: Vec<usize> = Vec::new();

        let mut initial_configuration: BitSet = BitSet::new();
        initial_configuration.insert(nfa.initial_state);

        // compute epsilon closure
        Self::epsilon_closure(&nfa.nodes, &mut initial_configuration);
        let mut id: usize = 0;

        // configuration -> id
        let mut subsets: HashMap<BitSet, usize> = HashMap::new();
        subsets.insert(initial_configuration.clone(), id);
        id += 1;
        
        let mut work_queue: VecDeque<BitSet> = VecDeque::new();
        work_queue.push_back(initial_configuration.clone());

        let mut transitions: HashMap<(BitSet, AsciiChar), BitSet> = HashMap::new();

        while !work_queue.is_empty() {
            let q = work_queue.pop_front().unwrap();
            for char in AsciiCharIterator::new() {
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
                    }
                    subsets.insert(t.clone(), id);
                    id += 1;

                    work_queue.push_back(t);
                }
            }
        }
        
        let mut nodes: Vec<FANode<AsciiChar>> = Vec::with_capacity(id);
        nodes.resize(id, FANode { transitions: vec![] });
        // a transitions to b when c is character applied
        for ((a, c), b) in transitions.iter() {
            let a = subsets[a];
            let b = subsets[b];
            nodes[a].transitions.push((Some(*c), b));
        }
        
        FA { nodes, initial_state: 0, accept_states: accept_states_dfa }
    }

    // basic BFS to compute epsilon closure
    fn epsilon_closure(nodes: &Vec<FANode<AsciiChar>>, set: &mut BitSet) {
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

    fn delta(nodes: &Vec<FANode<AsciiChar>>, set: &BitSet, c: AsciiChar) -> BitSet {
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

    fn get_partition(partitions: &Vec<BitSet>, id: usize) -> usize {
        partitions.iter()
            .enumerate()
            .filter(|x| x.1.contains(id))
            .next()
            .expect("partition should cover domain")
            .0
    }

    fn split(partitions: &Vec<BitSet>, dfa: &FA<AsciiChar>, set: &BitSet, c: AsciiChar) -> (BitSet, BitSet) {
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
                            first = false;
                            a.insert(e);
                        }
                        else {
                            // need to split
                            b.insert(e);
                        }
                    }
                    break;
                }
            }

            if no_transition {
                if action == -1 {
                    a.insert(e);
                }
                else {
                    b.insert(e);
                }
            }
            first = false;
        }

        (a, b)
    }

    // use Hopcroft's Algorithm to minimize a DFA
    pub fn minimize_dfa(dfa: &FA<AsciiChar>) -> FA<AsciiChar> {
        let accept: BitSet = dfa.accept_states.iter().copied().collect();
        let mut nonaccept: BitSet = (0..dfa.nodes.len()).collect();
        nonaccept.difference_with(&accept);
        
        let mut b_partition: Vec<BitSet> = vec![accept.clone(), nonaccept];
        let mut a_partition: Vec<BitSet> = Vec::new();

        // println!("{:?}", a_partition);
        // println!("{:?}", b_partition);


        while a_partition != b_partition {
            std::mem::swap(&mut a_partition, &mut b_partition);

            b_partition.clear();
            for s in &a_partition {
                let mut did_split = false;
                for ch in AsciiCharIterator::new() {
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


        let mut minimal_nodes: Vec<FANode<AsciiChar>> = Vec::with_capacity(a_partition.len());
        minimal_nodes.resize(a_partition.len(), FANode { transitions: Vec::new() });

        for (i, p) in a_partition.iter().enumerate() {
            let first = p.iter().next().expect("partition should be nonempty");
            let node = &dfa.nodes[first];
            for (c, next) in &node.transitions {
                minimal_nodes[i].transitions.push(
                    (*c, Self::get_partition(&a_partition, *next))
                )
            }
            
        }

        let initial_state: Vec<usize> = a_partition.iter()
            .enumerate()
            .filter(|x| x.1.contains(dfa.initial_state))
            .map(|x| x.0)
            .collect();
        
        assert!(initial_state.len() == 1, "partition should be disjoint");
        let initial_state = initial_state[0];

        let accept_states: Vec<usize> = a_partition.iter()
            .enumerate()
            .filter(|x| x.1.is_subset(&accept))
            .map(|x| x.0)
            .collect();

        FA { nodes: minimal_nodes, initial_state, accept_states }
    }

    pub fn simulate_dfa() -> bool {
        todo!()   
    }

    pub fn re_from_dfa() -> String {
        todo!()
    } 
}
