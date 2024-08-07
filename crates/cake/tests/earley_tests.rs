use cake::{scanner::{lexeme_sets::expressions::Expressions, RawTokenStream}, parser::{grammar::{EBNF, Grammar}, earley}};
use cake_lex::DFAScanner;
use petgraph::dot::{Dot, Config};


#[test]
fn test_expression_parse() {
    let expression_grammar = include_str!("../src/parser/grammars/expression.def");
    let parse_result = EBNF::from_str(expression_grammar);
    let parse_result = parse_result.expect("expect");
    let expression_grammar = Grammar::<Expressions>::from_ebnf(&parse_result).expect("failed");
    println!("{:?}", expression_grammar);

    let expression = "1+(2*3-4)";
    let expression_scanner = DFAScanner::from_lexeme_set::<Expressions>();
    let mut expression_tokenizer: RawTokenStream<'_, Expressions> = 
        RawTokenStream::new(expression_scanner, expression.as_bytes());

    let sppf: _ = earley::earley_parse(&mut expression_tokenizer, &expression_grammar);
    let sppf_graph = earley::sppf_to_graph(&sppf, &expression_grammar);
    println!("{:?}", Dot::with_config(&sppf_graph, &[Config::EdgeNoLabel, ]))
}