use bitflags::bitflags;

use super::symtab::TypeIdx;

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

pub(crate) type AggregateMember = (String, Box<QualifiedType>);
#[derive(Debug, Clone)]
pub(crate) struct QualifiedType {
    pub(crate) base_type: CType,
    pub(crate) qualifier: TypeQualifier
}

#[derive(Debug, Clone)]
pub(crate) enum CType {
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
        varargs: bool
    },
    PointerType {
        pointee_type: Box<QualifiedType>,
    },
    Void,

    // Pointers into symbol table to avoid having deep copies of highly-nested structs / unions
    // need to be careful to avoid circular / recursive structs (infinite size!)
    StructureTypeRef {
        symtab_idx: TypeIdx,
        qualifier: TypeQualifier
    },
    UnionTypeRef {
        symtab_idx: TypeIdx,
        qualifier: TypeQualifier
    }
}
