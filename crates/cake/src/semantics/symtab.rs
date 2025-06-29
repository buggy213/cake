use std::{collections::HashMap, mem::MaybeUninit};

use cake_util::{add_additional_index, make_type_idx};
use cranelift::{codegen::ir::FuncRef, module::FuncId};
use thiserror::Error;

use crate::{
    parser::ast::Constant,
    semantics::resolved_ast::NodeRef,
    types::{
        CType, EnumType, EnumTypeIdx, FunctionType, FunctionTypeIdx, StructureType,
        StructureTypeIdx, UnionType, UnionTypeIdx,
    },
};

// "function prototype scope" not included,
// just ignore symbol table when processing a function prototype
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ScopeType {
    BlockScope,
    FunctionScope,
    FileScope,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Scope {
    pub(crate) scope_type: ScopeType,
    pub(crate) parent_scope: Option<usize>,
    pub(crate) index: usize,
}

impl Scope {
    pub(crate) fn new_file_scope() -> Self {
        Self {
            scope_type: ScopeType::FileScope,
            parent_scope: None,
            index: 0,
        }
    }

    pub(crate) fn new(scope_type: ScopeType, parent_scope: usize, index: usize) -> Self {
        Self {
            scope_type,
            parent_scope: Some(parent_scope),
            index,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum StorageClass {
    Extern,
    Static,
    Auto,
    Register,
    None,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Linkage {
    External,
    Internal,
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Object {
    pub(crate) object_type: CType,
    pub(crate) linkage: Linkage,
}

make_type_idx!(ObjectIdx, Object);
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct ObjectRangeRef(pub u32, pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Function {
    pub(crate) function_type: FunctionTypeIdx,

    // 2 types of linkage: internal and external
    // by default, functions have external linkage, i.e. the linker can see the function and use it to resolve function calls in other translation units
    // internal linkage means that the linker is not allowed to use a function definition to resolve calls in other translation units (i.e. it is local to the current translation unit)
    // TODO: maybe implement inline function linkage rules (actual inlining optimization can be done using any function we have definition of in this translation unit)
    pub(crate) internal_linkage: bool,
    pub(crate) defined: bool,
}

make_type_idx!(FunctionIdx, Function);
add_additional_index!(FunctionIdx, FuncId);
add_additional_index!(FunctionIdx, FuncRef);

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Symbol {
    Object(ObjectIdx),
    // used only for enumeration constants
    Constant(Constant),
    Function(FunctionIdx),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TaggedTypeIdx {
    EnumTypeIdx(EnumTypeIdx),
    StructureTypeIdx(StructureTypeIdx),
    UnionTypeIdx(UnionTypeIdx),
}

impl From<EnumTypeIdx> for TaggedTypeIdx {
    fn from(value: EnumTypeIdx) -> Self {
        Self::EnumTypeIdx(value)
    }
}

impl From<StructureTypeIdx> for TaggedTypeIdx {
    fn from(value: StructureTypeIdx) -> Self {
        Self::StructureTypeIdx(value)
    }
}

impl From<UnionTypeIdx> for TaggedTypeIdx {
    fn from(value: UnionTypeIdx) -> Self {
        Self::UnionTypeIdx(value)
    }
}

impl TaggedTypeIdx {
    pub(crate) fn same_type(a: Self, b: Self) -> bool {
        match (a, b) {
            (TaggedTypeIdx::EnumTypeIdx(_), TaggedTypeIdx::EnumTypeIdx(_)) => true,
            (TaggedTypeIdx::StructureTypeIdx(_), TaggedTypeIdx::StructureTypeIdx(_)) => true,
            (TaggedTypeIdx::UnionTypeIdx(_), TaggedTypeIdx::UnionTypeIdx(_)) => true,
            _ => false,
        }
    }
}

// monolithic symbol table contains all symbols in program

#[derive(Debug, Default, PartialEq)]
pub(crate) struct ScopedSymtab {
    // these are built during parse; types may need to be merged during resolve
    scopes: Vec<Scope>,

    // the rest are built during resolve phase, by extracting information from
    // parsed AST
    symbols: Vec<HashMap<String, Symbol>>,
    labels: Vec<HashMap<String, NodeRef>>,
    tags: Vec<HashMap<String, TaggedTypeIdx>>,

    objects: Vec<Object>,
    functions: Vec<Function>,

    // which objects are associated with every function vs which are global
    function_object_ranges: Vec<ObjectRangeRef>,
    global_objects: Vec<ObjectIdx>,

    enum_types: Vec<EnumType>,
    structure_types: Vec<StructureType>,
    union_types: Vec<UnionType>,

    function_types: Vec<FunctionType>,
}

// subset of symbol table used after resolve
// (i.e. string identifiers are no longer used, only numeric indices)
#[derive(Debug, Default, PartialEq)]
pub(crate) struct SymbolTable {
    objects: Vec<Object>,
    functions: Vec<Function>,

    object_names: Vec<String>,
    function_names: Vec<String>,

    enum_types: Vec<EnumType>,
    structure_types: Vec<StructureType>,
    union_types: Vec<UnionType>,

    function_object_ranges: Vec<ObjectRangeRef>,
    global_objects: Vec<ObjectIdx>,

    function_types: Vec<FunctionType>,
}

impl SymbolTable {
    pub(crate) fn from_scoped_symtab(scoped_symtab: ScopedSymtab) -> SymbolTable {
        let ScopedSymtab {
            objects,
            functions,
            function_object_ranges,
            global_objects,
            enum_types,
            structure_types,
            union_types,
            function_types,

            symbols,
            ..
        } = scoped_symtab;

        let mut object_names: Vec<MaybeUninit<String>> = Vec::with_capacity(objects.len());
        // SAFETY: used with_capacity above, and MaybeUninit<T> does not require initialization
        unsafe { object_names.set_len(objects.len()) }

        let mut function_names: Vec<MaybeUninit<String>> = Vec::with_capacity(functions.len());
        // SAFETY: see above
        unsafe { function_names.set_len(functions.len()) }

        for symbols_at_scope in symbols {
            for (symbol_name, symbol) in symbols_at_scope {
                match symbol {
                    Symbol::Constant(_) => (), // no-op, inlined into expressions
                    Symbol::Object(object_idx) => {
                        object_names[object_idx.0 as usize].write(symbol_name);
                    }
                    Symbol::Function(function_idx) => {
                        function_names[function_idx.0 as usize].write(symbol_name);
                    }
                }
            }
        }

        // SAFETY: Vec<MaybeUninit<String>> should be the same as Vec<String>
        // in terms of layout, and the above code should initialize all entries
        let object_names: Vec<String> = unsafe { std::mem::transmute(object_names) };
        let function_names: Vec<String> = unsafe { std::mem::transmute(function_names) };

        SymbolTable {
            objects,
            functions,
            object_names,
            function_names,

            enum_types,
            structure_types,
            union_types,
            function_object_ranges,
            global_objects,
            function_types,
        }
    }

    pub(crate) fn global_objects(&self) -> impl Iterator<Item = &Object> {
        self.global_objects.iter().map(|idx| &self.objects[*idx])
    }

    pub(crate) fn global_object_names(&self) -> impl Iterator<Item = &str> {
        self.global_objects
            .iter()
            .map(|idx| self.object_names[idx.0 as usize].as_str())
    }

    pub(crate) fn functions(&self) -> &[Function] {
        &self.functions
    }

    pub(crate) fn function_names(&self) -> &[String] {
        &self.function_names
    }

    pub(crate) fn function_object_range(&self, fn_idx: FunctionIdx) -> ObjectRangeRef {
        self.function_object_ranges[fn_idx.0 as usize]
    }

    pub(crate) fn object_range(&self, object_range: ObjectRangeRef) -> &[Object] {
        &self.objects[object_range.0 as usize..object_range.1 as usize]
    }

    pub(crate) fn get_function_type(&self, function_type: FunctionTypeIdx) -> &FunctionType {
        &self.function_types[function_type]
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub(crate) enum SymtabError {
    #[error("Symbol `{0}` already declared")]
    AlreadyDeclared(String),
    #[error("type provided is not a tag")]
    NotATag(String),
    #[error("label `{0}` already declared within function")]
    LabelAlreadyDeclared(String),
}

impl ScopedSymtab {
    pub(crate) fn new_with_scopes(scopes: Vec<Scope>) -> Self {
        let symbols = vec![HashMap::new(); scopes.len()];
        let labels = vec![HashMap::new(); scopes.len()];
        let tags = vec![HashMap::new(); scopes.len()];
        Self {
            scopes,

            symbols,
            labels,
            tags,

            ..Default::default()
        }
    }

    pub(crate) fn num_scopes(&self) -> usize {
        self.scopes.len()
    }

    pub(crate) fn get_scopes(&self) -> &[Scope] {
        &self.scopes
    }

    // only look in current scope
    pub(crate) fn direct_lookup_tag_type_idx(
        &self,
        scope: Scope,
        tag: &str,
    ) -> Option<TaggedTypeIdx> {
        let direct_lookup = self.tags[scope.index].get(tag);
        direct_lookup.copied()
    }

    // look in current scope and all parent scopes
    pub(crate) fn lookup_tag_type_idx(&self, scope: Scope, tag: &str) -> Option<TaggedTypeIdx> {
        let mut current_scope = scope;
        loop {
            match self.direct_lookup_tag_type_idx(current_scope, tag) {
                Some(tag_type) => return Some(tag_type),
                None => match current_scope.parent_scope {
                    Some(parent) => current_scope = self.scopes[parent],
                    None => return None,
                },
            }
        }
    }

    pub(crate) fn direct_lookup_symbol(&self, scope: Scope, name: &str) -> Option<&Symbol> {
        self.symbols[scope.index].get(name)
    }

    pub(crate) fn lookup_symbol(&self, scope: Scope, name: &str) -> Option<&Symbol> {
        let mut current_scope = scope;
        loop {
            match self.direct_lookup_symbol(current_scope, name) {
                Some(sym) => return Some(sym),
                None => match current_scope.parent_scope {
                    Some(parent) => current_scope = self.scopes[parent],
                    None => return None,
                },
            }
        }
    }

    pub(crate) fn lookup_label(&self, scope: Scope, name: &str) -> Option<NodeRef> {
        let mut scope = scope;
        loop {
            if scope.scope_type == ScopeType::FunctionScope {
                break;
            } else if let Some(parent) = scope.parent_scope {
                scope = self.scopes[parent];
            } else {
                // maybe not unrecoverable, but grammar should 100% prevent this from happening
                unreachable!(
                    "label statement declared with no function scope as ancestor (should be impossible)"
                );
            }
        }

        let fn_scope = &self.labels[scope.index];
        fn_scope.get(name).copied()
    }

    pub(crate) fn all_symbols_at_scope(
        &self,
        scope: Scope,
    ) -> impl Iterator<Item = (&String, &Symbol)> {
        self.symbols[scope.index].iter()
    }

    pub(crate) fn add_enum_type(&mut self, enum_type: EnumType) -> EnumTypeIdx {
        EnumTypeIdx::from_push(&mut self.enum_types, enum_type)
    }

    pub(crate) fn add_structure_type(&mut self, structure_type: StructureType) -> StructureTypeIdx {
        StructureTypeIdx::from_push(&mut self.structure_types, structure_type)
    }

    pub(crate) fn add_union_type(&mut self, union_type: UnionType) -> UnionTypeIdx {
        UnionTypeIdx::from_push(&mut self.union_types, union_type)
    }

    pub(crate) fn add_function_type(&mut self, function_type: FunctionType) -> FunctionTypeIdx {
        FunctionTypeIdx::from_push(&mut self.function_types, function_type)
    }

    pub(crate) fn add_tag(
        &mut self,
        scope: Scope,
        name: String,
        tag: TaggedTypeIdx,
    ) -> Result<(), SymtabError> {
        if self.tags[scope.index].contains_key(&name) {
            return Err(SymtabError::AlreadyDeclared(name));
        }

        self.tags[scope.index].insert(name, tag);

        Ok(())
    }

    pub(crate) fn add_object(
        &mut self,
        scope: Scope,
        name: String,
        object: Object,
    ) -> Result<ObjectIdx, SymtabError> {
        if self.symbols[scope.index].contains_key(&name) {
            return Err(SymtabError::AlreadyDeclared(name));
        }

        let object_idx = ObjectIdx::from_push(&mut self.objects, object);
        let object_symbol = Symbol::Object(object_idx);
        self.symbols[scope.index].insert(name, object_symbol);
        Ok(object_idx)
    }

    pub(crate) fn begin_function_definition(&self) -> ObjectIdx {
        ObjectIdx(self.objects.len() as u32)
    }

    pub(crate) fn end_function_definition(&mut self, start: ObjectIdx, fn_idx: FunctionIdx) {
        self.function_object_ranges
            .resize_with(fn_idx.get_inner() + 1, || ObjectRangeRef(0, 0));
        self.function_object_ranges[fn_idx.get_inner()] =
            ObjectRangeRef(start.0, self.objects.len() as u32);
    }

    pub(crate) fn add_function(
        &mut self,
        scope: Scope,
        name: String,
        function: Function,
    ) -> Result<FunctionIdx, SymtabError> {
        if self.symbols[scope.index].contains_key(&name) {
            return Err(SymtabError::AlreadyDeclared(name));
        }

        let function_idx = FunctionIdx::from_push(&mut self.functions, function);
        let function_symbol = Symbol::Function(function_idx);
        self.symbols[scope.index].insert(name, function_symbol);
        Ok(function_idx)
    }

    pub(crate) fn add_constant(
        &mut self,
        scope: Scope,
        name: String,
        constant: Constant,
    ) -> Result<(), SymtabError> {
        if self.symbols[scope.index].contains_key(&name) {
            return Err(SymtabError::AlreadyDeclared(name));
        }

        let constant = Symbol::Constant(constant);
        self.symbols[scope.index].insert(name, constant);
        Ok(())
    }

    pub(crate) fn add_label(
        &mut self,
        scope: Scope,
        name: String,
        labeled_statement: NodeRef,
    ) -> Result<(), SymtabError> {
        // label names must be unique within a function
        // -> easiest is just to associate labels w/ function scopes.
        let mut scope = scope;
        loop {
            if scope.scope_type == ScopeType::FunctionScope {
                break;
            } else if let Some(parent) = scope.parent_scope {
                scope = self.scopes[parent];
            } else {
                // maybe not unrecoverable, but grammar should 100% prevent this from happening
                panic!(
                    "label statement declared with no function scope as ancestor (should be impossible)"
                );
            }
        }

        let fn_scope = &mut self.labels[scope.index];
        if fn_scope.contains_key(&name) {
            return Err(SymtabError::LabelAlreadyDeclared(name));
        }

        fn_scope.insert(name, labeled_statement);
        Ok(())
    }

    pub(crate) fn get_enum_type(&self, idx: EnumTypeIdx) -> &EnumType {
        &self.enum_types[idx]
    }

    pub(crate) fn get_enum_type_mut(&mut self, idx: EnumTypeIdx) -> &mut EnumType {
        &mut self.enum_types[idx]
    }

    pub(crate) fn get_structure_type(&self, idx: StructureTypeIdx) -> &StructureType {
        &self.structure_types[idx]
    }

    pub(crate) fn get_structure_type_mut(&mut self, idx: StructureTypeIdx) -> &mut StructureType {
        &mut self.structure_types[idx]
    }

    pub(crate) fn get_union_type(&self, idx: UnionTypeIdx) -> &UnionType {
        &self.union_types[idx]
    }

    pub(crate) fn get_union_type_mut(&mut self, idx: UnionTypeIdx) -> &mut UnionType {
        &mut self.union_types[idx]
    }

    pub(crate) fn get_function_type(&self, idx: FunctionTypeIdx) -> &FunctionType {
        &self.function_types[idx]
    }

    pub(crate) fn get_function_type_mut(&mut self, idx: FunctionTypeIdx) -> &mut FunctionType {
        &mut self.function_types[idx]
    }

    pub(crate) fn get_object(&self, idx: ObjectIdx) -> &Object {
        &self.objects[idx]
    }

    pub(crate) fn get_object_mut(&mut self, idx: ObjectIdx) -> &mut Object {
        &mut self.objects[idx]
    }

    pub(crate) fn get_function(&self, idx: FunctionIdx) -> &Function {
        &self.functions[idx]
    }

    pub(crate) fn get_function_mut(&mut self, idx: FunctionIdx) -> &mut Function {
        &mut self.functions[idx]
    }
}
