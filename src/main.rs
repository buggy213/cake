use cake::parser::grammar::EBNF;

fn main() {
    let ebnf_grammar = include_str!("parser/grammars/ebnf.def");
    // println!("{}", ebnf_grammar);

    let parse_result = EBNF::from_str(ebnf_grammar);
    match parse_result {
        Ok(nonterminals) => {
            for nonterminal in nonterminals {
                println!("{:?}", nonterminal);
            }
        },
        Err(c) => panic!("{:?}", c),
    }
}
