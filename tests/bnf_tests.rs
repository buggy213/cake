use cake::{parser::grammar::{EBNF, Grammar}, scanner::lexeme_sets::{expressions::Expressions, ebnf::Ebnf}};

#[test]
fn expression_grammar() {
    let expression_grammar = include_str!("../src/parser/grammars/expression.def");
    let parse_result = EBNF::from_str(expression_grammar);
    let parse_result = parse_result.expect("expect");
    let expression_grammar = Grammar::<Expressions>::from_ebnf(&parse_result).expect("failed");
    println!("{:?}", expression_grammar);
}

#[test]
fn ebnf_grammar() {
    let ebnf_grammar = include_str!("../src/parser/grammars/ebnf.def");
    let parse_result = EBNF::from_str(ebnf_grammar);
    let parse_result = parse_result.expect("expect");
    let ebnf_grammar = Grammar::<Ebnf>::from_ebnf(&parse_result).expect("failed");
    println!("{:?}", ebnf_grammar);
}