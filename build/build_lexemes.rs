use std::{path::{PathBuf, Path}, fs, error::Error};

use codegen::*;
use convert_case::{Casing, Case};
use glob::*;

// relative to root of package
const LEXEME_DIR: &str = "src/scanner/lexeme_sets";
const LEXEME_DIR_DEFS: &str = "src/scanner/lexeme_sets/defs";

pub fn parse_lexeme_defs() {
    let package_root = std::env::var("CARGO_MANIFEST_DIR").expect("must use cargo as build system");
    let lexeme_dir_abs = Path::new(package_root.as_str()).join(LEXEME_DIR);
    let lexeme_dir_defs_abs = Path::new(package_root.as_str()).join(LEXEME_DIR_DEFS);
    let lexeme_dir_defs_str = lexeme_dir_defs_abs.to_str().expect("path to package contains non-UTF8 characters, which cargo does not like");
    println!("cargo:rerun-if-changed={}", lexeme_dir_defs_str);
    
    let mut mod_rs = Scope::new();
    
    for entry in glob(&format!("{}/*.def", lexeme_dir_defs_str)).expect("failed to read glob pattern") {
        
        match entry {
            Ok(p) => {
                match parse_lexeme_def(&p, &lexeme_dir_abs) {
                    Ok(s) => {
                        mod_rs.raw(format!("pub mod {};", s).as_str());
                    },
                    Err(e) => panic!("{}", e),
                }
            },
            Err(e) => panic!("{:?}", e),
        }
    }

    let mod_rs_path = lexeme_dir_abs.join("mod.rs");
    let _ = fs::write(mod_rs_path, mod_rs.to_string());
}

fn parse_lexeme_def(def_path: &PathBuf, mod_path: &PathBuf) -> Result<String, Box<dyn Error>> {
    let def_string = fs::read_to_string(def_path.clone())?;
    
    let mut base = Scope::new();
    let name = def_path.file_name()
        .expect("strange filename")
        .to_str()
        .expect("strange filename")
        .replace(".def", "");

    let pascal_case_name = name.to_case(Case::Pascal);

    let mut lexeme_enum = Enum::new(&pascal_case_name);
    let mut lexeme_set_impl = Impl::new(&pascal_case_name);

    let mut from_name = Function::new("from_name");
    from_name.arg("name", "&str")
        .ret("Option<Self>");

    let mut from_name_match = Block::new("match name");

    let mut from_id = Function::new("from_id");
    from_id.arg("id", "u32")
        .ret("Option<Self>")
        .line("if id >= Self::size() { return None; }")
        .line("unsafe { Some(std::mem::transmute::<u32, Self>(id)) }");

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
        let name_string;
        let name;
        let pattern;
        let split_result = line.split_once(":");
        if split_result.is_some() {
            (name, pattern) = split_result.unwrap();
        }
        else {
            name_string = line.to_case(Case::Pascal);
            name = &name_string;
            pattern = line;
        }
        let name = name.trim();
        let pattern = pattern.trim();
        
        lexeme_enum.new_variant(name);
        
        let escaped_pattern = pattern.escape_default();
        from_name_match.line(format!("\"{}\" => Some({}::{}),", name, pascal_case_name, name));
        to_name_match.line(format!("{}::{} => \"{}\",", pascal_case_name, name, name));
        pattern_match.line(format!("{}::{} => \"{}\",", pascal_case_name, name, escaped_pattern));
        variants += 1;
    }

    from_name_match.line("_ => None");

    let mut next = Function::new("next");
    next.arg_self()
        .ret("Option<Self>")
        .line(format!("if self.to_id() >= {} - 1 {{ None }} else {{ Self::from_id(self.to_id() + 1) }}", variants));

    let mut size = Function::new("size");
    size.ret("u32")
        .line(format!("{}", variants));

    base.import("crate::scanner::lexemes", "LexemeSet");

    lexeme_enum.derive("Clone");
    lexeme_enum.derive("Copy");
    lexeme_enum.derive("PartialEq");
    lexeme_enum.derive("Eq");
    lexeme_enum.derive("Debug");
    lexeme_enum.derive("Hash");
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

    lexeme_set_impl.push_fn(size);
    
    lexeme_set_impl.impl_trait("LexemeSet");

    base.push_impl(lexeme_set_impl);

    let rs_path = mod_path.join(format!("{}.rs", name));
    fs::write(rs_path, base.to_string())?;

    Ok(name)
}