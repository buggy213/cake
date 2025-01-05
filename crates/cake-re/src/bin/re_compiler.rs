use std::{env, fs};

use cake_re::{lexeme_def::parse_lexeme_def, DFATable};

fn main() {
    let args: Vec<String> = env::args().collect();

    let input_file = &args[1];
    let output_file = &args[2];

    let input_file = fs::read_to_string(input_file).expect("Input file not found");
    let lexeme_def = parse_lexeme_def(String::new(), &input_file);
    let re_strs: Vec<&str> = lexeme_def
        .lexemes
        .iter()
        .map(|x| x.pattern.as_str())
        .collect();
    let dfa_table = DFATable::from_regex_strs(&re_strs);
    let serialized_table = DFATable::compile_table(&dfa_table);
    fs::write(output_file, &serialized_table).expect("Failed to write output");

    println!("Wrote {}, {} bytes", output_file, serialized_table.len());
}
