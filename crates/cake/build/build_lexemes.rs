use std::{error::Error, fs::{self, Metadata}, path::{Path, PathBuf}};

use anyhow::Context;
use codegen::*;
use convert_case::{Casing, Case};
use glob::*;
use lazy_static::lazy_static;

// relative to root of package
const LEXEME_DIR: &str = "src/scanner/lexeme_sets";
const LEXEME_DIR_DEFS: &str = "src/scanner/lexeme_sets/defs";
const LEXEME_DIR_TABLES: &str = "src/scanner/lexeme_sets/tables";

lazy_static! {
    static ref PACKAGE_ROOT: String = std::env::var("CARGO_MANIFEST_DIR").expect("must use cargo as build system");
    
    static ref LEXEME_DIR_ABS: PathBuf = Path::new(PACKAGE_ROOT.as_str()).join(LEXEME_DIR);
    
    static ref LEXEME_DIR_DEF_ABS: PathBuf = Path::new(PACKAGE_ROOT.as_str()).join(LEXEME_DIR_DEFS);
    static ref LEXEME_DIR_DEF_STR: String = LEXEME_DIR_DEF_ABS.to_str().expect("path to package contains non-UTF8 characters, which cargo does not like").to_string();
    
    static ref LEXEME_DIR_TABLES_ABS: PathBuf = Path::new(PACKAGE_ROOT.as_str()).join(LEXEME_DIR_TABLES);
    static ref LEXEME_DIR_TABLES_STR: String = LEXEME_DIR_DEF_ABS.to_str().expect("path to package contains non-UTF8 characters, which cargo does not like").to_string();
}

pub fn parse_lexeme_defs() -> Result<Vec<LexemeSetDef>, anyhow::Error> {
    println!("cargo:rerun-if-changed={}", *LEXEME_DIR_DEF_STR);
    
    let mut lexeme_set_defs = Vec::new();
    for entry in glob(&format!("{}/*.def", *LEXEME_DIR_DEF_STR))? {
        let entry = entry?;
        let lexeme_set_def = parse_lexeme_def(&entry)?;
        lexeme_set_defs.push(lexeme_set_def);
    }

    Ok(lexeme_set_defs)
}

pub fn write_lexeme_defs(lexeme_set_defs: &[LexemeSetDef]) -> Result<(), anyhow::Error> {
    let mut mod_rs = Scope::new();

    for lexeme_set_def in lexeme_set_defs {
        write_lexeme_rs(&lexeme_set_def, &LEXEME_DIR_ABS)?;
        mod_rs.raw(format!("pub mod {};", &lexeme_set_def.name).as_str());
    }

    let mod_rs_path = LEXEME_DIR_ABS.join("mod.rs");
    fs::write(mod_rs_path, mod_rs.to_string())?;

    Ok(())
}


pub(super) struct Lexeme {
    name: String,
    pattern: String,
}

pub(super) struct LexemeSetDef {
    name: String,
    pascal_case_name: String,
    lexemes: Vec<Lexeme>,
    file_metadata: std::fs::Metadata
}

fn parse_lexeme_def(def_path: &PathBuf) -> Result<LexemeSetDef, anyhow::Error> {
    let def_metadata = fs::metadata(&def_path)?;
    let def_string = fs::read_to_string(&def_path)
        .with_context(|| "unable to read def")?;
    
    
    let name = def_path.file_name()
        .expect("strange filename")
        .to_str()
        .expect("strange filename")
        .replace(".def", "");

    let pascal_case_name = name.to_case(Case::Pascal);
    let mut lexemes = Vec::new();
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
        lexemes.push(Lexeme {
            name: name.to_string(),
            pattern: pattern.to_string(),
        });
    }
    
    let lexeme_set_def = LexemeSetDef {
        name,
        pascal_case_name,
        lexemes,
        file_metadata: def_metadata
    };

    Ok(lexeme_set_def)
}

fn write_lexeme_rs(lexeme_set_def: &LexemeSetDef, mod_path: &PathBuf) -> Result<(), anyhow::Error> {
    // check if rs file already exists and if it is more recently modified than lexeme def
    let rs_path = mod_path.join(format!("{}.rs", &lexeme_set_def.name));
    match fs::metadata(&rs_path) {
        Ok(metadata) => {
            let target_mtime = metadata.modified()
                .with_context(|| "error while checking rs output target mtime")?;
            let dependency_mtime = lexeme_set_def.file_metadata.modified()
                .with_context(|| "error while checking def file target mtime")?;

            // target newer than dependency, no need to write rs file
            if target_mtime > dependency_mtime {
                return Ok(());
            }
        },
        // no-op, might not exist (or might not have permissions) - in either case, writing is ok
        Err(_) => {}, 
    }
    
    let mut base = Scope::new();
    base.import("crate::scanner::lexemes", "LexemeSet");

    let mut lexeme_enum = Enum::new(&lexeme_set_def.pascal_case_name);
    lexeme_enum.derive("Clone");
    lexeme_enum.derive("Copy");
    lexeme_enum.derive("PartialEq");
    lexeme_enum.derive("Eq");
    lexeme_enum.derive("Debug");
    lexeme_enum.derive("Hash");
    lexeme_enum.repr("u32");
    lexeme_enum.vis("pub");
    
    let mut lexeme_set_impl = Impl::new(&lexeme_set_def.pascal_case_name);

    // populate enum variants
    lexeme_set_def.lexemes.iter()
        .for_each(|x| { lexeme_enum.new_variant(&x.name); });

    // populate (name: String) -> (Option<LexemeSet>) mapping
    let mut from_name = Function::new("from_name");
    from_name.arg("name", "&str")
        .ret("Option<Self>");

    let mut from_name_match = Block::new("match name");
    lexeme_set_def.lexemes.iter()
        .for_each(|x| {
            from_name_match.line(format!("\"{}\" => Some({}::{}),", &x.name, &lexeme_set_def.pascal_case_name, &x.name));
        });
    
    from_name_match.line("_ => None");
    from_name.push_block(from_name_match);

    // populate (id: u32) -> (Option<LexemeSet>) mapping
    let mut from_id = Function::new("from_id");
    from_id.arg("id", "u32")
        .ret("Option<Self>");

    let mut from_id_match = Block::new("match id");
    lexeme_set_def.lexemes.iter().enumerate()
        .for_each(|(id, x)| {
            from_id_match.line(format!("{} => Some({}::{}),", id, &lexeme_set_def.pascal_case_name, &x.name));
        });
    from_id_match.line("_ => None");
    from_id.push_block(from_id_match);

    // populate (EnumVariant) -> (name: String) mapping
    let mut to_name = Function::new("to_name");
    to_name.arg_self()
        .ret("&'static str");

    let mut to_name_match = Block::new("match self");
    lexeme_set_def.lexemes.iter()
        .for_each(|x| {
            to_name_match.line(format!("{}::{} => \"{}\",", &lexeme_set_def.pascal_case_name, &x.name, &x.name));
        });
    to_name.push_block(to_name_match);
    
    // (EnumVariant) -> (id: u32) is just a cast since the lexeme set enum is always repr(u32)
    let mut to_id = Function::new("to_id");
    to_id.arg_self()
        .ret(Type::new("u32"))
        .line("self as u32");

    // populate (EnumVariant) -> (pattern: String) mapping
    let mut pattern = Function::new("pattern");
    pattern.arg_self()
        .ret("&'static str");

    let mut pattern_match = Block::new("match self");
    lexeme_set_def.lexemes.iter()
        .for_each(|x| {
            let escaped_pattern = x.pattern.escape_default();
            pattern_match.line(format!("{}::{} => \"{}\",", &lexeme_set_def.pascal_case_name, &x.name, escaped_pattern));
        });
    pattern.push_block(pattern_match);
    
    let mut next = Function::new("next");
    next.arg_self()
        .ret("Option<Self>")
        .line(format!("if self.to_id() >= {} - 1 {{ None }} else {{ Self::from_id(self.to_id() + 1) }}", lexeme_set_def.lexemes.len()));

    let mut size = Function::new("size");
    size.ret("u32")
        .line(format!("{}", lexeme_set_def.lexemes.len()));
    
    // push trait functions into impl
    lexeme_set_impl.push_fn(from_name);
    lexeme_set_impl.push_fn(from_id);
    lexeme_set_impl.push_fn(to_name);
    lexeme_set_impl.push_fn(to_id);
    lexeme_set_impl.push_fn(pattern);
    lexeme_set_impl.push_fn(next);
    lexeme_set_impl.push_fn(size);

    // impl LexemeSet
    lexeme_set_impl.impl_trait("LexemeSet");

    // push enum + implementation of LexemeSet into root
    base.push_enum(lexeme_enum);
    base.push_impl(lexeme_set_impl);

    // write it out
    fs::write(rs_path, base.to_string())?;

    Ok(())
}

pub fn write_lexeme_tables(lexeme_set_defs: &[LexemeSetDef]) -> Result<(), anyhow::Error> {
    for lexeme_set_def in lexeme_set_defs {

    }
    todo!()
} 