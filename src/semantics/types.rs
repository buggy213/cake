use bitflags::bitflags;

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

type AggregateMember = (String, Box<CType>);
type EnumVariant = (String, usize);

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


#[derive(Clone, Debug)]
pub(crate) enum CType {
    BasicType {
        basic_type: BasicType,
        qualifier: TypeQualifier,
    },
    UnionType {
        tag: Option<String>,
        members: Vec<AggregateMember>,
        qualifier: TypeQualifier,
    },
    StructureType {
        tag: Option<String>,
        members: Vec<AggregateMember>,
        qualifier: TypeQualifier,
    },
    EnumerationType {
        tag: Option<String>,
        members: Vec<EnumVariant>,
        qualifier: TypeQualifier,
    },
    ArrayType {
        size: usize,
        element_type: Box<CType>,
        qualifier: TypeQualifier,
    },
    FunctionType {
        parameter_types: Vec<CType>,
        return_type: Box<CType>,
        function_specifier: FunctionSpecifier,
        qualifier: TypeQualifier,
    },
    PointerType {
        pointee_type: Box<CType>,
        qualifier: TypeQualifier,
    },
    Void,
}
