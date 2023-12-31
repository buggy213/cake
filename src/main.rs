use cake::scanner::FA;
use cake::scanner::regex::Regex;



fn main() {
    let regex = Regex::from_str(r"fee").expect("wtf");
    println!("regex = {:?}", regex);

    let regex2 = Regex::from_str(r"fie").expect("wtf");
    println!("regex = {:?}", regex2);

    let nfa = FA::combine_res(&vec![regex, regex2]);
    println!("nfa = {:?}", nfa);

    let dfa = FA::dfa_from_nfa(&nfa);
    println!("dfa = {:?}", dfa);

    let dfa = FA::minimize_dfa(&dfa, true);
    println!("minimized = {:?}", dfa);
}
