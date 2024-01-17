use cake::{parser::grammar::{EBNF, Grammar}, scanner::lexeme_sets::{expressions::Expressions, ebnf::Ebnf}};

fn main() {
    let ebnf_grammar = include_str!("parser/grammars/ebnf.def");
    let parse_result = EBNF::from_str(ebnf_grammar);
    let parse_result = parse_result.expect("expect");
    let ebnf_grammar = Grammar::<Ebnf>::from_ebnf(&parse_result).expect("failed");
    println!("{:?}", ebnf_grammar);
}
