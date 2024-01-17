use cake::{parser::grammar::{EBNF, Grammar}, scanner::lexeme_sets::expressions::Expressions};

#[test]
fn expression_grammar() {
    let expression_grammar = include_str!("../src/parser/grammars/expression.def");
    let parse_result = EBNF::from_str(expression_grammar);
    let parse_result = parse_result.expect("expect");
    let expression_grammar = Grammar::<Expressions>::from_ebnf(&parse_result).expect("failed");
    println!("{:?}", expression_grammar);
}