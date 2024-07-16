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




type AggregateMember = (Option<String>, Box<CType>);

enum CType {
    BasicType(BasicType),
    UnionType {
        tag: Option<String>,
        members: Vec<AggregateMember>
    },
    StructureType {
        tag: Option<String>,
        members: Vec<AggregateMember>
    },
    EnumerationType {
        tag: Option<String>,
        members: Vec<AggregateMember>
    },
    ArrayType {
        size: usize,
        element_type: Box<CType>
    },
    FunctionType {
        parameter_types: Vec<CType>,
        return_type: Box<CType>
    },
    PointerType {
        pointee_type: Box<CType>
    },
    Void
}
