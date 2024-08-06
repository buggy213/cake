mod build_lexemes;

fn main() {
    let lexeme_defs = build_lexemes::parse_lexeme_defs().expect("failed to parse lexeme defs!");
    build_lexemes::write_lexeme_defs(&lexeme_defs).expect("failed to write lexeme def code");

}

