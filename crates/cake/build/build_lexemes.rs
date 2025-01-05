use std::{
    fs::{self},
    path::{Path, PathBuf},
};

use anyhow::Context;
use cake_re::lexeme_def::parse_lexeme_def;
use cake_re::lexeme_def::LexemeSetDef;
use codegen::*;
use glob::*;
use lazy_static::lazy_static;

// relative to root of crate
const LEXEME_DIR: &str = "src/scanner/lexeme_sets";

// relative to root of workspace
const LEXEME_DIR_DEFS: &str = "data/lexeme_sets";

lazy_static! {
    static ref PACKAGE_ROOT: String =
        std::env::var("CARGO_MANIFEST_DIR").expect("must use cargo as build system");
    static ref WORKSPACE_ROOT: PathBuf = Path::new(PACKAGE_ROOT.as_str())
        .parent()
        .and_then(Path::parent)
        .expect("corrupted folder structure")
        .to_path_buf();
    static ref LEXEME_DIR_ABS: PathBuf = Path::new(PACKAGE_ROOT.as_str()).join(LEXEME_DIR);
    static ref LEXEME_DIR_DEF_ABS: PathBuf = WORKSPACE_ROOT.join(LEXEME_DIR_DEFS);
    static ref LEXEME_DIR_DEF_STR: String = LEXEME_DIR_DEF_ABS
        .to_str()
        .expect("path to package contains non-UTF8 characters, which cargo does not like")
        .to_string();
}

pub fn parse_lexeme_defs() -> Result<Vec<LexemeSetDef>, anyhow::Error> {
    println!("cargo:rerun-if-changed={}", *LEXEME_DIR_DEF_STR);

    let mut lexeme_set_defs = Vec::new();
    for entry in glob(&format!("{}/*.def", *LEXEME_DIR_DEF_STR))? {
        let entry = entry?;
        let def_name = entry
            .file_name()
            .with_context(|| "should not be ..")?
            .to_str()
            .with_context(|| "strange filename")?
            .replace(".def", "");

        let def_string = fs::read_to_string(&entry).with_context(|| "unable to read def")?;

        let lexeme_set_def = parse_lexeme_def(def_name, &def_string);
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

fn write_lexeme_rs(lexeme_set_def: &LexemeSetDef, mod_path: &PathBuf) -> Result<(), anyhow::Error> {
    // check if rs file already exists and if it is more recently modified than lexeme def
    let rs_path = mod_path.join(format!("{}.rs", &lexeme_set_def.name));

    let mut base = Scope::new();
    base.import("crate::scanner", "LexemeSet");
    base.import("cake_re", "DFATable");

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
    lexeme_set_def.lexemes.iter().for_each(|x| {
        lexeme_enum.new_variant(&x.name);
    });

    // populate (name: String) -> (Option<LexemeSet>) mapping
    let mut from_name = Function::new("from_name");
    from_name.arg("name", "&str").ret("Option<Self>");

    let mut from_name_match = Block::new("match name");
    lexeme_set_def.lexemes.iter().for_each(|x| {
        from_name_match.line(format!(
            "\"{}\" => Some({}::{}),",
            &x.name, &lexeme_set_def.pascal_case_name, &x.name
        ));
    });

    from_name_match.line("_ => None");
    from_name.push_block(from_name_match);

    // populate (id: u32) -> (Option<LexemeSet>) mapping
    let mut from_id = Function::new("from_id");
    from_id.arg("id", "u32").ret("Option<Self>");

    let mut from_id_match = Block::new("match id");
    lexeme_set_def
        .lexemes
        .iter()
        .enumerate()
        .for_each(|(id, x)| {
            from_id_match.line(format!(
                "{} => Some({}::{}),",
                id, &lexeme_set_def.pascal_case_name, &x.name
            ));
        });
    from_id_match.line("_ => None");
    from_id.push_block(from_id_match);

    // populate (EnumVariant) -> (name: String) mapping
    let mut to_name = Function::new("to_name");
    to_name.arg_self().ret("&'static str");

    let mut to_name_match = Block::new("match self");
    lexeme_set_def.lexemes.iter().for_each(|x| {
        to_name_match.line(format!(
            "{}::{} => \"{}\",",
            &lexeme_set_def.pascal_case_name, &x.name, &x.name
        ));
    });
    to_name.push_block(to_name_match);

    // (EnumVariant) -> (id: u32) is just a cast since the lexeme set enum is always repr(u32)
    let mut to_id = Function::new("to_id");
    to_id.arg_self().ret(Type::new("u32")).line("self as u32");

    // populate (EnumVariant) -> (pattern: String) mapping
    let mut pattern = Function::new("pattern");
    pattern.arg_self().ret("&'static str");

    let mut pattern_match = Block::new("match self");
    lexeme_set_def.lexemes.iter().for_each(|x| {
        let escaped_pattern = x.pattern.escape_default();
        pattern_match.line(format!(
            "{}::{} => \"{}\",",
            &lexeme_set_def.pascal_case_name, &x.name, escaped_pattern
        ));
    });
    pattern.push_block(pattern_match);

    let mut next = Function::new("next");
    next.arg_self().ret("Option<Self>").line(format!(
        "if self.to_id() >= {} - 1 {{ None }} else {{ Self::from_id(self.to_id() + 1) }}",
        lexeme_set_def.lexemes.len()
    ));

    let mut size = Function::new("size");
    size.ret("u32")
        .line(format!("{}", lexeme_set_def.lexemes.len()));

    // impl for loading DFATable
    let precompiled_bytes = format!(
        "static PRECOMPILED: &[u8] = include_bytes!(\"{}/{}\");",
        *LEXEME_DIR_DEF_STR, &lexeme_set_def.name
    );

    let load_table_fn = lexeme_set_impl.new_fn("table");
    load_table_fn.ret("DFATable");
    let de_statement = "DFATable::from_precompiled_table(PRECOMPILED)";
    load_table_fn.line(de_statement);

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

    base.raw(&precompiled_bytes);

    // write it out
    fs::write(rs_path, base.to_string())?;

    Ok(())
}
