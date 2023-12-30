use crate::scanner::{Regex, FA};

mod scanner;

fn main() {
    let regex = Regex::from_str(r"a(b|c)*").expect("wtf");
    println!("regex = {:?}", regex);

    let nfa = FA::nfa_from_re(regex);
    println!("nfa = {:?}", nfa);

    let dfa = FA::dfa_from_nfa(&nfa);
    println!("dfa = {:?}", dfa);

    let dfa = FA::minimize_dfa(&dfa);
    println!("minimized = {:?}", dfa);
}
