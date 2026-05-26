use cake_util::make_type_idx;

pub(crate) struct Module {
    functions: Vec<Function>,
    signatures: Vec<Signature>,
    data: Vec<Data>,
}

make_type_idx!(DataRef, Data);
pub(crate) struct Data {

}

impl Module {
    pub(crate) fn new() -> Module {
        Module { 
            functions: Vec::new(),
            signatures: Vec::new(),
            data: Vec::new() 
        }
    }

    pub(crate) fn add_function(&mut self, signature: SigRef) -> FuncRef {
        let func = Function { 
            signature, 
            blocks: vec![Block::new()]
        };

        FuncRef::from_push(&mut self.functions, func)
    }

    pub(crate) fn add_signature(&mut self, signature: Signature) -> SigRef {
        SigRef::from_push(&mut self.signatures, signature)
    }

    pub(crate) fn fn_builder(&'_ mut self, func: FuncRef) -> FunctionBuilder<'_> {
        FunctionBuilder { 
            func: &mut self.functions[func],
            current_block: BlockRef(0)
        }
    }
}

// these are just the best names, what can i say
#[allow(non_camel_case_types)]
#[derive(Clone, Copy)]
pub(crate) enum Type {
    i8,
    u8,
    i16,
    u16,
    i32,
    u32,
    i64,
    u64,

    f32,
    f64,
}

#[allow(non_camel_case_types)]
#[derive(Clone, Copy)]
pub(crate) enum Constant {
    i8(i8),
    u8(u8),
    i16(i16),
    u16(u16),
    i32(i32),
    u32(u32),
    i64(i64),
    u64(u64),

    f32(f32),
    f64(f64)
}

impl Type {
    pub(crate) fn width(self) -> usize {
        match self {
            Type::i8 => 8,
            Type::u8 => 8,
            Type::i16 => 16,
            Type::u16 => 16,
            Type::i32 => 32,
            Type::u32 => 32,
            Type::i64 => 64,
            Type::u64 => 64,

            Type::f32 => 32,
            Type::f64 => 64,
        }
    }
}

make_type_idx!(SigRef, Signature);
pub(crate) struct Signature {
    argument_types: Vec<Type>,
    return_type: Option<Type>,
}

impl Signature {
    pub(crate) fn new(argument_types: Vec<Type>, return_type: Option<Type>) -> Signature {
        Signature { argument_types, return_type }
    }
}

make_type_idx!(FuncRef, Function);
pub(crate) struct Function {
    signature: SigRef,
    blocks: Vec<Block>,
}

pub(crate) struct FunctionBuilder<'func> {
    func: &'func mut Function,
    current_block: BlockRef
}

impl<'func> FunctionBuilder<'func> {
    pub(crate) fn add_block(&mut self) -> BlockRef {
        let block = Block { 
            insts: Vec::new(), 
            inst_types: Vec::new() 
        };

        BlockRef::from_push(&mut self.func.blocks, block)
    }

    pub(crate) fn insert(&'_ mut self) -> BlockBuilder<'_> {
        BlockBuilder { 
            block: &mut self.func.blocks[self.current_block] 
        }
    }
}

pub(crate) struct BlockBuilder<'block> {
    block: &'block mut Block
}

impl<'block> BlockBuilder<'block> {
    pub(crate) fn add(&mut self, a: Value, b: Value) -> Value {
        let add = Inst::Add { a, b };
        Value::from_push(&mut self.block.insts, add)
    }

    pub(crate) fn sub(&mut self, a: Value, b: Value) -> Value {
        let sub = Inst::Sub { a, b };
        Value::from_push(&mut self.block.insts, sub)
    }

    pub(crate) fn mul(&mut self, a: Value, b: Value) -> Value {
        let mul = Inst::Mul { a, b };
        Value::from_push(&mut self.block.insts, mul)
    }

    pub(crate) fn div(&mut self, a: Value, b: Value) -> Value {
        let div = Inst::Div { a, b };
        Value::from_push(&mut self.block.insts, div)
    }

    pub(crate) fn brif(&mut self, cond: Value, con: BlockRef, alt: BlockRef) {
        todo!()
    }
}

make_type_idx!(BlockRef, Block);
pub(crate) struct Block {
    insts: Vec<Inst>,
    inst_types: Vec<Type>,
}

impl Block {
    fn new() -> Block {
        Block { insts: Vec::new(), inst_types: Vec::new() }
    }
}

pub(crate) type Value = InstRef;

make_type_idx!(InstRef, Inst);
pub(crate) enum Inst {
    BlockArgument,
    
    Add {
        a: InstRef,
        b: InstRef
    },
    Sub {
        a: InstRef,
        b: InstRef
    },
    Mul {
        a: InstRef,
        b: InstRef
    },
    Div {
        a: InstRef,
        b: InstRef
    },

    BranchIf {
        cond: InstRef,
        con: BlockRef,
        alt: BlockRef,
    },


}

mod ast2cir;

#[cfg(test)]
mod test {
    #[test]
    fn test_make_block() {

    }
}