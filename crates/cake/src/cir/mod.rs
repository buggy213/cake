use cake_util::{add_additional_index, make_type_idx};

pub(crate) struct Module {
    functions: Vec<Function>,
    signatures: Vec<Signature>,
    data: Vec<Data>,
}

make_type_idx!(DataRef, Data);
pub(crate) struct Data {}

impl Module {
    pub(crate) fn new() -> Module {
        Module {
            functions: Vec::new(),
            signatures: Vec::new(),
            data: Vec::new(),
        }
    }

    pub(crate) fn add_function(&mut self, signature: SigRef) -> FuncRef {
        let func = Function {
            signature,
            blocks: vec![Block::new()],
            stack_slots: Vec::new(),
        };

        FuncRef::from_push(&mut self.functions, func)
    }

    pub(crate) fn add_signature(&mut self, signature: Signature) -> SigRef {
        SigRef::from_push(&mut self.signatures, signature)
    }

    pub(crate) fn fn_builder(&'_ mut self, func: FuncRef) -> FunctionBuilder<'_> {
        FunctionBuilder {
            func: &mut self.functions[func],
            current_block: BlockRef(0),
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
    f64(f64),
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

    pub(crate) fn is_integral(self) -> bool {
        match self {
            Type::i8
            | Type::u8
            | Type::i16
            | Type::u16
            | Type::i32
            | Type::u32
            | Type::i64
            | Type::u64 => true,
            Type::f32 | Type::f64 => false,
        }
    }

    pub(crate) fn is_fp(self) -> bool {
        match self {
            Type::i8
            | Type::u8
            | Type::i16
            | Type::u16
            | Type::i32
            | Type::u32
            | Type::i64
            | Type::u64 => false,
            Type::f32 | Type::f64 => true,
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
        Signature {
            argument_types,
            return_type,
        }
    }
}

make_type_idx!(FuncRef, Function);
pub(crate) struct Function {
    signature: SigRef,
    blocks: Vec<Block>,
    stack_slots: Vec<StackSlot>,
}

make_type_idx!(StackSlotRef, StackSlot);
pub(crate) struct StackSlot {
    size: u32,
    align: u32,
}

pub(crate) struct FunctionBuilder<'func> {
    func: &'func mut Function,
    current_block: BlockRef,
}

impl<'func> FunctionBuilder<'func> {
    pub(crate) fn add_block(&mut self) -> BlockRef {
        let block = Block {
            insts: Vec::new(),
            inst_types: Vec::new(),
        };

        BlockRef::from_push(&mut self.func.blocks, block)
    }

    pub(crate) fn add_stack_slot(&mut self, size: u32, align: u32) -> StackSlotRef {
        let slot = StackSlot { size, align };

        StackSlotRef::from_push(&mut self.func.stack_slots, slot)
    }

    pub(crate) fn insert(&'_ mut self) -> BlockBuilder<'_> {
        BlockBuilder {
            block: &mut self.func.blocks[self.current_block],
        }
    }
}

pub(crate) struct BlockBuilder<'block> {
    block: &'block mut Block,
}

impl<'block> BlockBuilder<'block> {
    fn constant(&mut self, ty: Type, val: Constant) -> Value {
        let constant = Inst::Constant { val };
        let res = InstRef::from_push(&mut self.block.insts, constant);
        self.block.inst_types.push(ty);
        res
    }

    pub(crate) fn const_u32(&mut self, val: u32) -> Value {
        self.constant(Type::u32, Constant::u32(val))
    }

    pub(crate) fn const_u64(&mut self, val: u64) -> Value {
        self.constant(Type::u64, Constant::u64(val))
    }

    pub(crate) fn const_i32(&mut self, val: i32) -> Value {
        self.constant(Type::i32, Constant::i32(val))
    }

    pub(crate) fn const_i64(&mut self, val: i64) -> Value {
        self.constant(Type::i64, Constant::i64(val))
    }

    pub(crate) fn icast(&mut self, val: InstRef, to: Type) -> Value {
        let cast = Inst::CastInt {
            val,
            target_type: to,
        };
        let res = InstRef::from_push(&mut self.block.insts, cast);
        self.block.inst_types.push(to);
        res
    }

    // helper to copy the type of one of the operands.
    // legalization to make sure operand types are actually compatible is deferred
    fn copy_type(&mut self, from: InstRef) {
        self.block.inst_types.push(self.block.inst_types[from]);
    }

    fn binary_op(&mut self, a: Value, b: Value, op: fn(Value, Value) -> Inst) -> Value {
        let op = op(a, b);
        let res = Value::from_push(&mut self.block.insts, op);
        self.copy_type(a);
        res
    }

    pub(crate) fn add(&mut self, a: Value, b: Value) -> Value {
        self.binary_op(a, b, |a, b| Inst::Add { a, b })
    }

    pub(crate) fn sub(&mut self, a: Value, b: Value) -> Value {
        self.binary_op(a, b, |a, b| Inst::Sub { a, b })
    }

    pub(crate) fn mul(&mut self, a: Value, b: Value) -> Value {
        self.binary_op(a, b, |a, b| Inst::Mul { a, b })
    }

    pub(crate) fn div(&mut self, a: Value, b: Value) -> Value {
        self.binary_op(a, b, |a, b| Inst::Div { a, b })
    }

    pub(crate) fn brif(&mut self, cond: Value, con: BlockRef, alt: BlockRef) {
        let brif = Inst::BranchIf { cond, con, alt };
        self.block.insts.push(brif);
    }

    pub(crate) fn load(&mut self, addr: Value) -> Value {
        let load = Inst::Load { addr };
        Value::from_push(&mut self.block.insts, load)
    }

    pub(crate) fn store(&mut self, addr: Value, val: Value) {
        let store = Inst::Store { addr, val };
        self.block.insts.push(store);
    }
}

make_type_idx!(BlockRef, Block);
pub(crate) struct Block {
    insts: Vec<Inst>,
    inst_types: Vec<Type>,
}

impl Block {
    fn new() -> Block {
        Block {
            insts: Vec::new(),
            inst_types: Vec::new(),
        }
    }
}

pub(crate) type Value = InstRef;

make_type_idx!(InstRef, Inst);
add_additional_index!(InstRef, Type);
pub(crate) enum Inst {
    BlockArgument,

    Constant {
        val: Constant,
    },

    Add {
        a: InstRef,
        b: InstRef,
    },
    Sub {
        a: InstRef,
        b: InstRef,
    },
    Mul {
        a: InstRef,
        b: InstRef,
    },
    Div {
        a: InstRef,
        b: InstRef,
    },

    Load {
        addr: InstRef,
    },
    Store {
        addr: InstRef,
        val: InstRef,
    },
    StackSlotAddr {
        slot: StackSlotRef,
    },

    // cast between integer types
    CastInt {
        val: InstRef,
        target_type: Type,
    },

    CompareInt {
        a: InstRef,
        b: InstRef,
        mode: CompareMode,
    },

    CompareFloat {
        a: InstRef,
        b: InstRef,
        mode: CompareMode,
    },

    BranchIf {
        cond: InstRef,
        con: BlockRef,
        alt: BlockRef,
    },

    Jump {
        target: BlockRef,
    },
}

pub(crate) enum CompareMode {
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
}

mod ast2cir;

#[cfg(test)]
mod test {
    #[test]
    fn test_make_block() {}
}
