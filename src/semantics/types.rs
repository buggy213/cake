enum BasicType {
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

pub(crate) enum CType {
    BasicType {
        basic_type: BasicType
    },
    UnionType {
        tag: Option<String>,
        members: Vec<AggregateMember>,
    },
    StructureType {
        tag: Option<String>,
        members: Vec<AggregateMember>,
    },
    EnumerationType {
        tag: Option<String>,
        members: Vec<EnumVariant>,
    },
    ArrayType {
        size: usize,
        element_type: Box<CType>,
    },
    FunctionType {
        parameter_types: Vec<CType>,
        return_type: Box<CType>,
    },
    PointerType {
        pointee_type: Box<CType>,
    },
    Void,
}
