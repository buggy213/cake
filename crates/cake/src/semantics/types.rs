use bitflags::bitflags;

use super::symtab::{Scope, TypeIdx};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum BasicType {
    UChar,
    Char,
    UShort,
    Short,
    UInt,
    Int,
    ULong,
    Long,

    Float,
    Double,
}


// for now, all enums will be 4 bytes
type EnumVariant = (String, i32);

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub(crate) struct TypeQualifier: u8 {
        const Const = 1;
        const Restrict = 1 << 1;
        const Volatile = 1 << 2;
    }
}

// For now, not used
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum FunctionSpecifier {
    Inline,
    None
}

pub(crate) type AggregateMember = (String, QualifiedType);
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct QualifiedType {
    pub(crate) base_type: CType,
    pub(crate) qualifier: TypeQualifier
}

pub(crate) enum CanonicalType {
    // "Canonical" types go into symbol table, should try to only keep 1 around
    // if possible. Incomplete types can be std::mem::replace'd once they are completed
    // in addition, they do not specify type qualifiers (though their members might)
    IncompleteUnionType {
        tag: String
    },
    UnionType {
        tag: Option<String>,
        members: Vec<AggregateMember>,
    },
    IncompleteStructureType {
        tag: String // having an anonymous incomplete struct/union type seems meaningless...
    },
    StructureType {
        tag: Option<String>,
        members: Vec<AggregateMember>,
    },
    EnumerationType {
        tag: Option<String>,
        members: Vec<EnumVariant>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CType {
    // Derived / basic types can be passed around more freely
    BasicType {
        basic_type: BasicType,
    },
    IncompleteArrayType {
        element_type: Box<QualifiedType>,
    },
    ArrayType {
        size: usize,
        element_type: Box<QualifiedType>,
    },
    FunctionType {
        parameter_types: Vec<QualifiedType>,
        return_type: Box<QualifiedType>,
        function_specifier: FunctionSpecifier,
        varargs: bool,
        prototype_scope: Scope
    },
    PointerType {
        pointee_type: Box<QualifiedType>,
    },
    Void,

    // Pointers into symbol table to avoid having deep copies of highly-nested structs / unions
    // need to be careful to avoid circular / recursive structs (infinite size!)
    StructureTypeRef {
        symtab_idx: TypeIdx
    },
    UnionTypeRef {
        symtab_idx: TypeIdx
    },
    EnumTypeRef {
        symtab_idx: TypeIdx
    }
}
