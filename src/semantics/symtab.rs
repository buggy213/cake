use std::{collections::HashMap, ops::{Index, IndexMut}, rc::Rc};

use thiserror::Error;

use crate::parser::ast::{ASTNode, Constant};

use super::types::{CType, QualifiedType};

// "function prototype scope" not included, 
// just ignore symbol table when processing a function prototype
#[derive(Clone, Copy, PartialEq, Eq)]
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
        symbol_type: QualifiedType,
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
    pub(crate) fn lookup_tag_type(&self, scope: Scope, tag: &str) -> Option<&CType> {
        self.lookup_tag_type_idx(scope, tag)
            .map(|idx| &self.types[idx])
    }

    pub(crate) fn lookup_tag_type_mut(&mut self, scope: Scope, tag: &str) -> Option<&mut CType> {
        self.lookup_tag_type_idx(scope, tag)
            .map(|idx| &mut self.types[idx])
    }

    pub(crate) fn lookup_tag_type_idx(&self, scope: Scope, tag: &str) -> Option<TypeIdx> {
        let mut current_scope = scope;
        loop {
            match self.direct_lookup_tag_type_idx(current_scope, tag) {
                Some(tag_type) => return Some(tag_type),
                None => {
                    match current_scope.parent_scope {
                        Some(parent) => current_scope = self.scopes[parent],
                        None => return None,
                    }
                }
            }
        }
    }

    // only look in current scope
    pub(crate) fn direct_lookup_tag_type(&self, scope: Scope, tag: &str) -> Option<&CType> {
        self.direct_lookup_tag_type_idx(scope, tag)
            .map(|idx| &self.types[idx])
    }

    pub(crate) fn direct_lookup_tag_type_mut(&mut self, scope: Scope, tag: &str) -> Option<&mut CType> {
        self.direct_lookup_tag_type_idx(scope, tag)
            .map(|idx| &mut self.types[idx])
    }

    pub(crate) fn direct_lookup_tag_type_idx(&self, scope: Scope, tag: &str) -> Option<TypeIdx> {
        let direct_lookup = self.tags[scope.index].get(tag);
        match direct_lookup {
            Some(tag_type_idx) => return Some(*tag_type_idx),
            None => return None
        }
    }

    pub(crate) fn add_type(&mut self, ctype: CType) -> TypeIdx {
        let type_idx = TypeIdx(self.types.len() - 1);
        self.types.push(ctype);
        type_idx
    }

    pub(crate) fn add_tag(&mut self, scope: Scope, name: String, tag: TypeIdx) -> Result<(), SymtabError> {
        match self.types[tag] {
            CType::UnionType { .. }
            | CType::StructureType { .. }
            | CType::EnumerationType { .. } => {}

            _ => return Err(SymtabError::NotATag(name))
        }

        if self.tags[scope.index].contains_key(&name) {
            return Err(SymtabError::AlreadyDeclared(name));
        }
        
        self.tags[scope.index].insert(name, tag);

        Ok(())
    }

    pub(crate) fn add_symbol(&mut self, scope: Scope, name: String, symbol: Symbol) -> Result<(), SymtabError> {
        if self.symbols[scope.index].contains_key(&name) {
            return Err(SymtabError::AlreadyDeclared(name));
        }
        self.symbols[scope.index].insert(name, symbol);
        Ok(())
    }

    pub(crate) fn get_type(&self, idx: TypeIdx) -> &CType {
        &self.types[idx]
    }

    pub(crate) fn get_type_mut(&mut self, idx: TypeIdx) -> &mut CType {
        &mut self.types[idx]
    }

    pub(crate) fn new_scope(&mut self, parent: Option<Scope>, scope_type: ScopeType) -> Scope {
        if let None = parent {
            debug_assert!(scope_type == ScopeType::FileScope);
        }

        let new_scope = Scope {
            scope_type,
            parent_scope: parent.map(|s| s.index),
            index: self.scopes.len(),
        };

        self.scopes.push(new_scope);
        new_scope
    }

    pub(crate) fn get_parent_scope(&self, scope: Scope) -> Option<Scope> {
        scope.parent_scope.map(|idx| self.scopes[idx])
    }
}