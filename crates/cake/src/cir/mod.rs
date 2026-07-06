use cake_util::{add_additional_index, make_type_idx};

#[derive(Debug)]
pub(crate) struct Module {
    functions: Vec<Function>,
    signatures: Vec<Signature>,
    data: Vec<Data>,
}

make_type_idx!(DataRef, Data);

#[derive(Debug)]
pub(crate) struct Data {}

impl Module {
    pub(crate) fn new() -> Module {
        Module {
            functions: Vec::new(),
            signatures: Vec::new(),
            data: Vec::new(),
        }
    }

    pub(crate) fn add_function(&mut self, name: String, signature: SigRef) -> FuncRef {
        let func = Function {
            name,
            signature,
            insts: Vec::new(),
            inst_types: Vec::new(),
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

    pub(crate) fn functions(&self) -> &[Function] {
        &self.functions
    }

    pub(crate) fn signatures(&self) -> &[Signature] {
        &self.signatures
    }

    pub(crate) fn data(&self) -> &[Data] {
        &self.data
    }
}

// these are just the best names, what can i say
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Debug, Clone, Copy)]
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

impl Constant {
    pub(crate) fn ty(self) -> Type {
        match self {
            Constant::i8(_) => Type::i8,
            Constant::u8(_) => Type::u8,
            Constant::i16(_) => Type::i16,
            Constant::u16(_) => Type::u16,
            Constant::i32(_) => Type::i32,
            Constant::u32(_) => Type::u32,
            Constant::i64(_) => Type::i64,
            Constant::u64(_) => Type::u64,
            Constant::f32(_) => Type::f32,
            Constant::f64(_) => Type::f64,
        }
    }
}

make_type_idx!(SigRef, Signature);

#[derive(Debug)]
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

#[derive(Debug)]
pub(crate) struct Function {
    pub(crate) name: String,
    pub(crate) signature: SigRef,

    pub(crate) insts: Vec<Inst>,
    pub(crate) inst_types: Vec<Option<Type>>,
    pub(crate) blocks: Vec<Block>,

    pub(crate) stack_slots: Vec<StackSlot>,
}

make_type_idx!(StackSlotRef, StackSlot);

#[derive(Debug)]
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
            inst_refs: Vec::new(),
        };

        BlockRef::from_push(&mut self.func.blocks, block)
    }

    pub(crate) fn add_stack_slot(&mut self, size: u32, align: u32) -> StackSlotRef {
        let slot = StackSlot { size, align };

        StackSlotRef::from_push(&mut self.func.stack_slots, slot)
    }

    pub(crate) fn set_block(&mut self, block: BlockRef) {
        self.current_block = block;
    }

    pub(crate) fn insert(&'_ mut self) -> BlockBuilder<'_> {
        BlockBuilder {
            block: &mut self.func.blocks[self.current_block],
            insts: &mut self.func.insts,
            inst_types: &mut self.func.inst_types,
        }
    }
}

pub(crate) struct BlockBuilder<'block> {
    block: &'block mut Block,
    insts: &'block mut Vec<Inst>,
    inst_types: &'block mut Vec<Option<Type>>,
}

impl<'block> BlockBuilder<'block> {
    fn constant(&mut self, ty: Type, val: Constant) -> Value {
        assert!(val.ty() == ty, "type mismatch while inserting constant");

        let constant = Inst::Constant { val };
        self.inst_types.push(Some(ty));
        let v = Value::from_push(self.insts, constant);
        self.block.inst_refs.push(v);
        v
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

    pub(crate) fn stack_addr(&mut self, slot: StackSlotRef) -> Value {
        let op = Inst::StackAddr { slot };
        self.inst_types.push(Some(Type::u64));
        let v = Value::from_push(self.insts, op);
        self.block.inst_refs.push(v);
        v
    }

    pub(crate) fn icast(&mut self, val: InstRef, to: Type) -> Value {
        let cast = Inst::CastInt {
            val,
            target_type: to,
        };

        self.inst_types.push(Some(to));
        let v = Value::from_push(self.insts, cast);
        self.block.inst_refs.push(v);
        v
    }

    // helper to copy the type of one of the operands.
    // legalization to make sure operand types are actually compatible is deferred
    fn copy_type(&mut self, from: InstRef) {
        self.inst_types.push(self.inst_types[from]);
    }

    fn binary_op(&mut self, a: Value, b: Value, op: fn(Value, Value) -> Inst) -> Value {
        let op = op(a, b);
        let v = Value::from_push(self.insts, op);
        self.copy_type(a);
        self.block.inst_refs.push(v);
        v
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

    pub(crate) fn modulo(&mut self, a: Value, b: Value) -> Value {
        self.binary_op(a, b, |a, b| Inst::Modulo { a, b })
    }

    pub(crate) fn load(&mut self, addr: Value, ty: Type) -> Value {
        let load = Inst::Load { addr };
        let v = Value::from_push(self.insts, load);
        self.block.inst_refs.push(v);
        self.inst_types.push(Some(ty));
        v
    }

    pub(crate) fn store(&mut self, addr: Value, val: Value) {
        let store = Inst::Store { addr, val };
        let v = InstRef::from_push(self.insts, store);
        self.block.inst_refs.push(v);
        self.inst_types.push(None);
    }

    pub(crate) fn brif(&mut self, cond: Value, con: BlockRef, alt: BlockRef) {
        let brif = Inst::BranchIf { cond, con, alt };
        let v = Value::from_push(self.insts, brif);
        self.block.inst_refs.push(v);
        self.inst_types.push(None);
    }

    pub(crate) fn ret(&mut self, value: Option<Value>) {
        let ret = Inst::Return { value };
        let v = InstRef::from_push(self.insts, ret);
        self.block.inst_refs.push(v);
        self.inst_types.push(None);
    }
}

make_type_idx!(BlockRef, Block);

#[derive(Debug)]
pub(crate) struct Block {
    pub(crate) inst_refs: Vec<InstRef>,
}

impl Block {
    fn new() -> Block {
        Block {
            inst_refs: Vec::new(),
        }
    }
}

pub(crate) type Value = InstRef;

make_type_idx!(InstRef, Inst);
add_additional_index!(InstRef, Option<Type>);

#[derive(Debug)]
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
    Modulo {
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
    StackAddr {
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

    Return {
        value: Option<InstRef>,
    },

    Jump {
        target: BlockRef,
    },
}

impl Inst {
    pub(crate) fn mnemonic(&self) -> &str {
        match self {
            Inst::BlockArgument => "block_arg",
            Inst::Constant { val } => "const",
            Inst::Add { a, b } => "add",
            Inst::Sub { a, b } => "sub",
            Inst::Mul { a, b } => "mul",
            Inst::Div { a, b } => "div",
            Inst::Modulo { a, b } => "modulo",
            Inst::Load { addr } => "load",
            Inst::Store { addr, val } => "store",
            Inst::StackAddr { slot } => "stack_addr",
            Inst::CastInt { val, target_type } => "cast",
            Inst::CompareInt { a, b, mode } => "cmp",
            Inst::CompareFloat { a, b, mode } => "fcmp",
            Inst::BranchIf { cond, con, alt } => "brif",
            Inst::Return { value } => "ret",
            Inst::Jump { target } => "jmp",
        }
    }
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::i8(v) => write!(f, "{v}"),
            Constant::u8(v) => write!(f, "{v}"),
            Constant::i16(v) => write!(f, "{v}"),
            Constant::u16(v) => write!(f, "{v}"),
            Constant::i32(v) => write!(f, "{v}"),
            Constant::u32(v) => write!(f, "{v}"),
            Constant::i64(v) => write!(f, "{v}"),
            Constant::u64(v) => write!(f, "{v}"),
            Constant::f32(v) => write!(f, "{v}"),
            Constant::f64(v) => write!(f, "{v}"),
        }
    }
}

impl std::fmt::Display for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let m = self.mnemonic();
        match self {
            Inst::BlockArgument => write!(f, "{m}"),
            Inst::Constant { val } => write!(f, "{m} {val}"),
            Inst::Add { a, b } => write!(f, "{m} v{} v{}", a.0, b.0),
            Inst::Sub { a, b } => write!(f, "{m} v{} v{}", a.0, b.0),
            Inst::Mul { a, b } => write!(f, "{m} v{} v{}", a.0, b.0),
            Inst::Div { a, b } => write!(f, "{m} v{} v{}", a.0, b.0),
            Inst::Modulo { a, b } => write!(f, "{m} v{} v{}", a.0, b.0),
            Inst::Load { addr } => write!(f, "{m} (v{})", addr.0),
            Inst::Store { addr, val } => write!(f, "{m} v{} (v{})", val.0, addr.0),
            Inst::StackAddr { slot } => write!(f, "{m} ss{}", slot.0),
            Inst::CastInt { val, target_type } => write!(f, "{m} v{} -> {target_type}", val.0),
            Inst::CompareInt { a, b, mode } => {
                todo!()
            },
            Inst::CompareFloat { a, b, mode } => {
                todo!()
            },
            Inst::BranchIf { cond, con, alt } => write!(f, "{m} v{} b{} b{}", cond.0, con.0, alt.0),
            Inst::Return { value } => if let Some(value) = value {
                write!(f, "{m} v{}", value.0)
            } else {
                write!(f, "{m}")
            },
            Inst::Jump { target } => {
                write!(f, "{m} b{}", target.0)
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum CompareMode {
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let variant_name = match *self {
            Type::i8 => "i8",
            Type::u8 => "u8",
            Type::i16 => "i16",
            Type::u16 => "u16",
            Type::i32 => "i32",
            Type::u32 => "u32",
            Type::i64 => "i64",
            Type::u64 => "u64",
            Type::f32 => "f32",
            Type::f64 => "f64",
        };

        f.write_str(variant_name)?;
        Ok(())
    }
}

impl std::fmt::Display for Signature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn(")?;
        for (i, arg) in self.argument_types.iter().enumerate() {
            write!(f, "{}", *arg)?;
            if i + 1 < self.argument_types.len() {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")?;
        if let Some(ret_type) = self.return_type {
            write!(f, " -> {}", ret_type)?;
        }

        Ok(())
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} (sig {})", &self.name, self.signature.0)?;
        
        for (idx, slot) in self.stack_slots.iter().enumerate() {
            writeln!(f, "ss{idx} = size {}, align {}", slot.size, slot.align)?;
        }

        for (idx, block) in self.blocks.iter().enumerate() {
            writeln!(f, "b{idx}:")?;
            for &iref in &block.inst_refs {
                if let Some(value_ty) = self.inst_types[iref] {
                    write!(f, "  ")?;
                    writeln!(f, "v{} : {} = {}", iref.0, value_ty, &self.insts[iref])?;
                }
                else {
                    write!(f, "  ")?;
                    writeln!(f, "{}", &self.insts[iref])?;
                }
            }
        }

        Ok(())
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "fn signatures:")?;
        for (i, sig) in self.signatures.iter().enumerate() {
            write!(f, "  ")?;
            writeln!(f, "{}: {}", i, sig)?;
        }

        writeln!(f, "functions:")?;
        for func in &self.functions {
           write!(f, "{func}")?;
        }


        Ok(())
    }
}

pub(crate) mod ast2cir;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_basic_ops() {
        let mut module = Module::new();
        let main_sig = module.add_signature(Signature {
            argument_types: vec![],
            return_type: None,
        });

        let func = module.add_function("test_basic_ops".to_string(), main_sig);
        let mut fn_builder = module.fn_builder(func);

        let a = fn_builder.insert().const_i32(1);
        let b = fn_builder.insert().const_i32(2);
        let add = fn_builder.insert().add(a, b);
    }
}
