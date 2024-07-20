use std::{collections::HashMap, ops::{Index, IndexMut}, rc::Rc};

use thiserror::Error;

use crate::parser::ast::{ASTNode, Constant};

use super::types::CType;

// "function prototype scope" not included, 
// just ignore symbol table when processing a function prototype
#[derive(Clone, Copy)]
pub(crate) enum ScopeType {
    BlockScope,
    FunctionScope,
    FileScope
}

#[derive(Clone, Copy)]
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

pub(crate) enum Symbol {
    Variable {
        symbol_type: TypeIdx,
        storage_class: StorageClass,
        linkage: Linkage
    },
    Constant(Constant)
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct TypeIdx(usize);
impl Index<TypeIdx> for Vec<CType> {
    type Output = CType;

    fn index(&self, index: TypeIdx) -> &Self::Output {
        return &self[index.0];
    }
}
impl IndexMut<TypeIdx> for Vec<CType> {
    fn index_mut(&mut self, index: TypeIdx) -> &mut Self::Output {
        return &mut self[index.0];
    }
}

// monolithic symbol table contains all symbols in program
// in particular, every symbol has an associated identifier and type
// will likely need to add storage / linkage / layout information as well
pub(crate) struct SymbolTable {
    scopes: Vec<Scope>,
    symbols: Vec<HashMap<String, Symbol>>,
    labels: Vec<HashMap<String, Rc<ASTNode>>>,
    tags: Vec<HashMap<String, TypeIdx>>,
    types: Vec<CType>
}

pub(crate) struct ImmutableScopedSymbolTable<'a> {
    table: &'a SymbolTable,
    scope: Scope
}

#[derive(Debug, Error)]
pub(crate) enum SymtabError {
    #[error("Unable to find tag `{0}` in symbol table")]
    TagNotFound(String),
    #[error("Symbol `{0}` already declared")]
    AlreadyDeclared(String),
    #[error("type provided is not a tag")]
    NotATag(String)
}

impl SymbolTable {
    pub(crate) fn create_scoped_view(&self, scope: Scope) -> ImmutableScopedSymbolTable {
        ImmutableScopedSymbolTable {
            table: self,
            scope,
        }
    }

    // look in current scope and all parent scopes
    pub fn lookup_tag_type(&self, scope: Scope, tag: &str) -> Result<&CType, SymtabError> {
        self.lookup_tag_type_idx(scope, tag)
            .map(|idx| &self.types[idx])
    }

    pub fn lookup_tag_type_mut(&mut self, scope: Scope, tag: &str) -> Result<&mut CType, SymtabError> {
        self.lookup_tag_type_idx(scope, tag)
            .map(|idx| &mut self.types[idx])
    }

    pub fn lookup_tag_type_idx(&self, scope: Scope, tag: &str) -> Result<TypeIdx, SymtabError> {
        let mut current_scope = scope;
        loop {
            match self.direct_lookup_tag_type_idx(current_scope, tag) {
                Ok(tag_type) => return Ok(tag_type),
                Err(_) => {
                    match current_scope.parent_scope {
                        Some(parent) => current_scope = self.scopes[parent],
                        None => return Err(SymtabError::TagNotFound(tag.to_owned())),
                    }
                }
            }
        }
    }

    // only look in current scope
    pub fn direct_lookup_tag_type(&self, scope: Scope, tag: &str) -> Result<&CType, SymtabError> {
        self.direct_lookup_tag_type_idx(scope, tag)
            .map(|idx| &self.types[idx])
    }

    pub fn direct_lookup_tag_type_mut(&mut self, scope: Scope, tag: &str) -> Result<&mut CType, SymtabError> {
        self.direct_lookup_tag_type_idx(scope, tag)
            .map(|idx| &mut self.types[idx])
    }

    pub fn direct_lookup_tag_type_idx(&self, scope: Scope, tag: &str) -> Result<TypeIdx, SymtabError> {
        let direct_lookup = self.tags[scope.index].get(tag);
        match direct_lookup {
            Some(tag_type_idx) => return Ok(*tag_type_idx),
            None => return Err(SymtabError::TagNotFound(tag.to_owned()))
        }
    }

    pub fn add_type(&mut self, ctype: CType) -> TypeIdx {
        let type_idx = TypeIdx(self.types.len() - 1);
        self.types.push(ctype);
        type_idx
    }

    pub fn add_tag(&mut self, scope: Scope, name: String, tag: CType) -> Result<(), SymtabError> {
        match tag {
            CType::UnionType { .. }
            | CType::StructureType { .. }
            | CType::EnumerationType { .. } => {}

            _ => return Err(SymtabError::NotATag(name))
        }

        if self.tags[scope.index].contains_key(&name) {
            return Err(SymtabError::AlreadyDeclared(name));
        }
        
        let type_idx = self.add_type(tag);
        self.tags[scope.index].insert(name, type_idx);

        Ok(())
    }

    pub fn add_symbol(&mut self, scope: Scope, name: String, symbol: Symbol) -> Result<(), SymtabError> {
        if self.symbols[scope.index].contains_key(&name) {
            return Err(SymtabError::AlreadyDeclared(name));
        }
        self.symbols[scope.index].insert(name, symbol);
        Ok(())
    }

    pub fn get_type(&self, idx: TypeIdx) -> &CType {
        &self.types[idx]
    }

    pub fn get_type_mut(&mut self, idx: TypeIdx) -> &mut CType {
        &mut self.types[idx]
    }
}