use convert_case::{Case, Casing};

#[derive(Debug)]
pub struct Lexeme {
    pub name: String,
    pub pattern: String,
}

#[derive(Debug)]
pub struct LexemeSetDef {
    pub name: String,
    pub pascal_case_name: String,
    pub lexemes: Vec<Lexeme>,
}

pub fn parse_lexeme_def(def_name: String, def_string: &str) -> LexemeSetDef {
    let pascal_case_name = def_name.to_case(Case::Pascal);
    let mut lexemes = Vec::new();
    let lines = def_string.split("\n");
    for line in lines {
        let name_string;
        let name;
        let pattern;
        let split_result = line.split_once(":");
        if split_result.is_some() {
            (name, pattern) = split_result.unwrap();
        } else {
            name_string = line.to_case(Case::Pascal);
            name = &name_string;
            pattern = line;
        }
        let name = name.trim();
        let pattern = pattern.trim();
        lexemes.push(Lexeme {
            name: name.to_string(),
            pattern: pattern.to_string(),
        });
    }

    let lexeme_set_def = LexemeSetDef {
        name: def_name,
        pascal_case_name,
        lexemes,
    };

    lexeme_set_def
}
