use cake_lex::fa::FA;
use cake_lex::regex::Regex;
use cake_lex::LexemeSet;
use cake_lex::DFAScanner;
use cake::scanner::lexeme_sets::c_lexemes::CLexemes;

#[test]
fn test_basic() {
    let c_lexemes = CLexemes::iter()
        .map(|x| Regex::from_str(x.pattern()).expect("failed to parse regex"))
        .collect::<Vec<_>>();

    let nfa = FA::combine_res(&c_lexemes);
    println!("nfa = {:?}", nfa);

    let dfa = FA::dfa_from_nfa(&nfa);
    println!("dfa = {:?}", dfa);

    let dfa = FA::minimize_dfa(&dfa, true);
    println!("minimized = {:?}", dfa);

    let mut scanner = DFAScanner::from_ascii_dfa(&dfa);
    scanner.scan_string("#include <stdio.h> void main() { int x = 0; while (x != 999) x++; return x; }");
}