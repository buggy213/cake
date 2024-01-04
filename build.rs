use std::{path::{PathBuf, Path}, fs, error::Error};

use codegen::*;
use convert_case::{Casing, Case};
use glob::*;

const LEXEME_DIR: &str = "src/scanner/lexeme_sets";
const LEXEME_DIR_DEFS: &str = "src/scanner/lexeme_sets/defs";

fn main() {
    parse_lexeme_defs();
}

fn parse_lexeme_defs() {
    println!("cargo:rerun-if-changed={}", LEXEME_DIR_DEFS);

    let mut mod_rs = Scope::new();

    for entry in glob(&format!("{}/*.def", LEXEME_DIR_DEFS)).expect("failed to read glob pattern") {
        match entry {
            Ok(p) => {
                match parse_lexeme_def(p) {
                    Ok(s) => {
                        mod_rs.raw(format!("pub mod {};", s).as_str());
                    },
                    Err(e) => println!("{}", e),
                }
            },
            Err(e) => eprintln!("{:?}", e),
        }
    }

    let mod_rs_path = format!("{}/mod.rs", LEXEME_DIR);
    let _ = fs::write(mod_rs_path, mod_rs.to_string());
}

fn parse_lexeme_def(path: PathBuf) -> Result<String, Box<dyn Error>> {
    let def_string = fs::read_to_string(path.clone())?;
    
    let mut base = Scope::new();
    let name = path.file_name()
        .expect("strange filename")
        .to_str()
        .expect("strange filename")
        .replace(".def", "");

    let pascal_case_name = name.to_case(Case::Pascal);

    let mut lexeme_enum = Enum::new(&pascal_case_name);
    let mut lexeme_set_impl = Impl::new(&pascal_case_name);

    let mut from_name = Function::new("from_name");
    from_name.arg("name", "&str")
        .ret("Self");

    let mut from_name_match = Block::new("match name");

    let mut from_id = Function::new("from_id");
    from_id.arg("id", "u32")
        .ret("Self")
        .line("unsafe { std::mem::transmute::<u32, Self>(id) }");

    let mut to_name = Function::new("to_name");
    to_name.arg_self()
        .ret("&'static str");

    let mut to_name_match = Block::new("match self");

    let mut to_id = Function::new("to_id");
    to_id.arg_self()
        .ret(Type::new("u32"))
        .line("self as u32");

    let mut pattern = Function::new("pattern");
    pattern.arg_self()
        .ret("&'static str");

    let mut pattern_match = Block::new("match self");

    let mut variants = 0;
    let lines = def_string.split("\n");
    for line in lines {
        let (name, pattern) = line.split_once(":")
            .expect("incorrectly formatted def");
        let name = name.trim();
        let pattern = pattern.trim();
        
        lexeme_enum.new_variant(name);
        
        let escaped_pattern = pattern.escape_default();
        from_name_match.line(format!("\"{}\" => {}::{},", name, pascal_case_name, name));
        to_name_match.line(format!("{}::{} => \"{}\",", pascal_case_name, name, name));
        pattern_match.line(format!("{}::{} => \"{}\",", pascal_case_name, name, escaped_pattern));
        variants += 1;
    }

    from_name_match.line("_ => panic!(\"unrecognized variant\")");

    let mut next = Function::new("next");
    next.arg_self()
        .ret("Option<Self>")
        .line(format!("if self.to_id() >= {} - 1 {{ None }} else {{ Some(Self::from_id(self.to_id() + 1)) }}", variants));

    base.import("crate::scanner::lexemes", "LexemeSet");
    base.import("crate::scanner::lexemes", "LexemeIterator");

    lexeme_enum.derive("Clone");
    lexeme_enum.derive("Copy");
    lexeme_enum.repr("u32");
    lexeme_enum.vis("pub");
    base.push_enum(lexeme_enum);

    from_name.push_block(from_name_match);
    lexeme_set_impl.push_fn(from_name);

    lexeme_set_impl.push_fn(from_id);
    
    to_name.push_block(to_name_match);
    lexeme_set_impl.push_fn(to_name);

    lexeme_set_impl.push_fn(to_id);

    pattern.push_block(pattern_match);
    lexeme_set_impl.push_fn(pattern);

    lexeme_set_impl.push_fn(next);
    
    lexeme_set_impl.impl_trait("LexemeSet");

    base.push_impl(lexeme_set_impl);
    
    let output_path = Path::new(LEXEME_DIR).join(format!("{}.rs", name));

    let _ = fs::write(output_path, base.to_string())?;

    Ok(name)
}