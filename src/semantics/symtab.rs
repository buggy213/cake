use std::{collections::HashMap, ops::Index, rc::Rc};

use crate::parser::ast::ASTNode;

use super::types::CType;

// "function prototype scope" not included, 
// just ignore symbol table when processing a function prototype
pub(crate) enum ScopeType {
    BlockScope,
    FunctionScope,
    FileScope
}

pub(crate) struct Scope {
    scope_type: ScopeType,
    parent_scope: Option<usize>,
    index: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum StorageClass {
    Extern,
    Static,
    Auto,
    Register,
    None,
}

pub(crate) enum Linkage {
    External,
    Internal,
    None
}

pub(crate) struct Symbol {
    symbol_type: CType,
    storage_class: StorageClass,
    linkage: Linkage
}

// monolithic symbol table contains all symbols in program
// in particular, every symbol has an associated identifier and type
// will likely need to add storage / linkage / layout information as well
pub(crate) struct SymbolTable {
    scopes: Vec<Scope>,
    symbols: Vec<HashMap<String, Symbol>>,
    labels: Vec<HashMap<String, Rc<ASTNode>>>,
    tags: Vec<HashMap<String, CType>>
}

struct ImmutableScopedSymbolTable<'a> {
    table: &'a SymbolTable,
    scope: Scope
}

impl<'a> Index<&str> for ImmutableScopedSymbolTable<'a> {
    type Output = Symbol;

    fn index(&self, index: &str) -> &Self::Output {
        let scoped_table = &self.table.symbols[self.scope.index];
        return &scoped_table[index];
    }
}

impl<'a> ImmutableScopedSymbolTable<'a> {
    fn lookup(&self) {
        todo!()
    }
}