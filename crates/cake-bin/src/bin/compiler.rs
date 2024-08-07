use std::time::Instant;

use cake::{parser::{grammar::{EBNF, Grammar}, earley::{self}}, scanner::{lexeme_sets::c_lexemes::CLexemes, RawTokenStream}};
use cake_lex::DFAScanner;

use petgraph::dot::{Dot, Config};

fn main() {
    let c_grammar = include_str!("../../../cake/src/parser/grammars/c_grammar.def");
    let parse_result = EBNF::from_str(c_grammar);
    let parse_result = parse_result.expect("expect");
    let c_grammar = Grammar::<CLexemes>::from_ebnf(&parse_result).expect("failed");
    println!("{:?}", c_grammar);

    let expression = "a = 17, *b = 56, *c = a++ * ++b";
    let expression_scanner = DFAScanner::from_lexeme_set::<CLexemes>();
    let mut expression_tokenizer: RawTokenStream<'_, CLexemes> = 
        RawTokenStream::new(expression_scanner, expression.as_bytes());

    let now = Instant::now();
    let sppf: _ = earley::earley_parse(&mut expression_tokenizer, &c_grammar);
    let elapsed = now.elapsed();
    println!("Parsing took {:.2?}", elapsed);
    let sppf_graph = earley::sppf_to_graph(&sppf, &c_grammar);
    println!("{:?}", Dot::with_config(&sppf_graph, &[Config::EdgeNoLabel, ]))
}
