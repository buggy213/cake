use std::cell::RefCell;

use cake_util::{add_additional_index, make_type_idx};
use smallvec::{SmallVec, ToSmallVec, smallvec};

#[derive(Debug)]
pub(crate) struct Module {
    functions: Vec<Function>,
    signatures: Vec<Signature>,
    data: Vec<Data>,
}

make_type_idx!(DataRef, Data);

#[derive(Debug)]
pub(crate) struct Data {
    name: Option<String>,
    read_only: bool,
    contents: Option<Box<[u8]>>
}

impl Module {
    pub(crate) fn new() -> Module {
        Module {
            functions: Vec::new(),
            signatures: Vec::new(),
            data: Vec::new(),
        }
    }

    pub(crate) fn add_function(&mut self, name: String, signature: Signature) -> FuncRef {
        self.signatures.push(signature);
        let func = Function {
            name,
            
            external_signatures: vec![],
            definition: None,
        };

        FuncRef::from_push(&mut self.functions, func)
    }

    pub(crate) fn define_function(&'_ mut self, func: FuncRef) -> FunctionBuilder<'_> {
        let Function { 
            name: _, 
            external_signatures, 
            definition 
        } = &mut self.functions[func];
        
        *definition = Some(FunctionDefinition { 
            insts: vec![], 
            inst_types: vec![],
            value_vecs: vec![],
            blocks: vec![Block::new()], 
            stack_slots: vec![]
        });

        FunctionBuilder {
            func: definition.as_mut().unwrap(),
            current_block: BlockRef(0),
            sigs: external_signatures,

            module_sigs: &self.signatures,
            module_data: &mut self.data,
        }
    }

    pub(crate) fn functions(&self) -> &[Function] {
        &self.functions
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
        !self.is_fp()
    }

    pub(crate) fn is_fp(self) -> bool {
        matches!(self, Type::f32 | Type::f64)
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

type TypeVec = SmallVec<[Type; 4]>;
type ValueVec = SmallVec<[Value; 8]>;

make_type_idx!(ValueVecRef, ValueVec);

make_type_idx!(SigRef, Signature);

#[derive(Debug, Clone)]
pub(crate) struct Signature {
    argument_types: Vec<Type>,
    return_types: Vec<Type>,
}

impl Signature {
    pub(crate) fn new(argument_types: Vec<Type>, return_types: Vec<Type>) -> Signature {
        Signature {
            argument_types,
            return_types,
        }
    }
}

make_type_idx!(FuncRef, Function);

#[derive(Debug)]
pub(crate) struct Function {
    pub(crate) name: String,
    pub(crate) external_signatures: Vec<Signature>,
    
    pub(crate) definition: Option<FunctionDefinition> 
}

#[derive(Debug)]
pub(crate) struct FunctionDefinition {
    pub(crate) insts: Vec<Inst>,
    pub(crate) inst_types: Vec<TypeVec>,
    pub(crate) value_vecs: Vec<ValueVec>,
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
    func: &'func mut FunctionDefinition,
    current_block: BlockRef,
    sigs: &'func mut Vec<Signature>,

    module_sigs: &'func [Signature],
    module_data: &'func mut Vec<Data>
}

impl<'func> FunctionBuilder<'func> {
    pub(crate) fn add_block(&mut self) -> BlockRef {
        let block = Block::new();
        BlockRef::from_push(&mut self.func.blocks, block)
    }

    pub(crate) fn add_stack_slot(&mut self, size: u32, align: u32) -> StackSlotRef {
        let slot = StackSlot { size, align };

        StackSlotRef::from_push(&mut self.func.stack_slots, slot)
    }

    pub(crate) fn set_block(&mut self, block: BlockRef) {
        self.current_block = block;
    }

    pub(crate) fn add_block_arg(&mut self, ty: Type) -> Value {
        let block_args = &mut self.func.blocks[self.current_block].block_args;
        let block_arg_idx = block_args.len();
        block_args.push(ty);
        Value::BlockArgument(self.current_block, block_arg_idx as u32)
    }

    pub(crate) fn insert(&'_ mut self) -> BlockBuilder<'_> {
        BlockBuilder {
            block: &self.func.blocks[self.current_block],
            all_blocks: &self.func.blocks,

            insts: &mut self.func.insts,
            inst_types: &mut self.func.inst_types,
            value_vecs: &mut self.func.value_vecs,

            sigs: self.sigs,
            module_sigs: self.module_sigs,
        }
    }

    pub(crate) fn declare_anonymous_data(&mut self, read_only: bool) -> DataRef {
        let data = Data {
            name: None,
            read_only,
            contents: None,
        };
        DataRef::from_push(self.module_data, data)
    }

    pub(crate) fn define_data(&mut self, data_ref: DataRef, contents: Box<[u8]>) {
        self.module_data[data_ref].contents = Some(contents);
    }
}

pub(crate) struct BlockBuilder<'block> {
    block: &'block Block,
    all_blocks: &'block [Block],

    insts: &'block mut Vec<Inst>,
    inst_types: &'block mut Vec<TypeVec>,
    value_vecs: &'block mut Vec<ValueVec>,
    
    sigs: &'block [Signature],
    module_sigs: &'block [Signature]
}

impl<'block> BlockBuilder<'block> {
    pub(crate) fn type_of(&mut self, val: Value) -> Type { 
        match val {
            Value::Inst(inst_ref) => {
                self.inst_types[inst_ref][0]
            },
            Value::BlockArgument(block_ref, idx) => {
                self.all_blocks[block_ref].block_args[idx as usize]
            },
            Value::TupleElement(inst_ref, idx) => {
                self.inst_types[inst_ref][idx as usize]
            },
        }
    }

    fn constant(&mut self, ty: Type, val: Constant) -> Value {
        assert!(val.ty() == ty, "type mismatch while inserting constant");

        let constant = Inst::Constant { val };
        self.inst_types.push(smallvec![ty]);
        let iref = InstRef::from_push(self.insts, constant);
        self.block.inst_refs.borrow_mut().push(iref);
        Value::Inst(iref)
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

    pub(crate) fn const_f32(&mut self, val: f32) -> Value {
        self.constant(Type::f32, Constant::f32(val))
    }
    pub(crate) fn const_f64(&mut self, val: f64) -> Value {
        self.constant(Type::f64, Constant::f64(val))
    }

    // creates a compatible constant from a u64 by truncating
    pub(crate) fn iconst_trunc(&mut self, val: Value, c: u64) -> Value {
        let ty = self.type_of(val);
        assert!(ty.is_integral(), "only integer typed values");

        let v = match ty {
            Type::i8 => Constant::i8(c as i8),
            Type::u8 => Constant::u8(c as u8),
            Type::i16 => Constant::i16(c as i16),
            Type::u16 => Constant::u16(c as u16),
            Type::i32 => Constant::i32(c as i32),
            Type::u32 => Constant::u32(c as u32),
            Type::i64 => Constant::i64(c as i64),
            Type::u64 => Constant::u64(c),
            _ => unreachable!()
        };

        self.constant(ty, v)
    }

    pub(crate) fn stack_addr(&mut self, slot: StackSlotRef) -> Value {
        let op = Inst::StackAddr { slot };
        self.inst_types.push(smallvec![Type::u64]);
        let iref = InstRef::from_push(self.insts, op);
        self.block.inst_refs.borrow_mut().push(iref);
        Value::Inst(iref)
    }

    fn type_conversion(&mut self, val: Value, to: Type, op: fn(Value) -> Inst) -> Value {
        let op = op(val);
        self.inst_types.push(smallvec![to]);
        let iref = InstRef::from_push(self.insts, op);
        self.block.inst_refs.borrow_mut().push(iref);
        Value::Inst(iref)
    }

    pub(crate) fn icast(&mut self, val: Value, to: Type) -> Value {
        self.type_conversion(val, to, |v| Inst::IntegerCast { v })
    }
    pub(crate) fn fcast(&mut self, val: Value, to: Type) -> Value {
        self.type_conversion(val, to, |v| Inst::FpCast { v })
    }

    // helper to copy the type of one of the operands.
    // legalization to make sure operand types are actually compatible is deferred
    fn copy_type(&mut self, from: Value) {
        let ty = match from {
            Value::Inst(inst_ref) => self.inst_types[inst_ref].clone(),
            Value::BlockArgument(block_ref, idx) => smallvec![self.all_blocks[block_ref].block_args[idx as usize]],
            Value::TupleElement(inst_ref, component) => smallvec![self.inst_types[inst_ref][component as usize]]
        };

        self.inst_types.push(ty);
    }

    fn binary_op(&mut self, a: Value, b: Value, op: fn(Value, Value) -> Inst) -> Value {
        let op = op(a, b);
        let iref = InstRef::from_push(self.insts, op);
        self.copy_type(a);
        self.block.inst_refs.borrow_mut().push(iref);
        Value::Inst(iref)
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

    pub(crate) fn and(&mut self, a: Value, b: Value) -> Value {
        self.binary_op(a, b, |a, b| Inst::And { a, b })
    }

    pub(crate) fn or(&mut self, a: Value, b: Value) -> Value {
        self.binary_op(a, b, |a, b| Inst::Or { a, b })
    }

    pub(crate) fn xor(&mut self, a: Value, b: Value) -> Value {
        self.binary_op(a, b, |a, b| Inst::Xor { a, b })
    }

    pub(crate) fn shl(&mut self, a: Value, b: Value) -> Value {
        self.binary_op(a, b, |a, b| Inst::Shl { a, b })
    }

    pub(crate) fn ashr(&mut self, a: Value, b: Value) -> Value {
        self.binary_op(a, b, |a, b| Inst::Ashr { a, b })
    }

    pub(crate) fn lshr(&mut self, a: Value, b: Value) -> Value {
        self.binary_op(a, b, |a, b| Inst::Lshr { a, b })
    }

    pub(crate) fn icmp(&mut self, mode: CompareMode, a: Value, b: Value) -> Value {
        let icmp = Inst::Icmp { mode, a, b };
        let iref = InstRef::from_push(self.insts, icmp);
        self.inst_types.push(smallvec![Type::i8]);
        self.block.inst_refs.borrow_mut().push(iref);
        Value::Inst(iref)
    }

    pub(crate) fn fadd(&mut self, a: Value, b: Value) -> Value {
        self.binary_op(a, b, |a, b| Inst::Fadd { a, b })
    }

    pub(crate) fn fsub(&mut self, a: Value, b: Value) -> Value {
        self.binary_op(a, b, |a, b| Inst::Fsub { a, b })
    }

    pub(crate) fn fmul(&mut self, a: Value, b: Value) -> Value {
        self.binary_op(a, b, |a, b| Inst::Fmul { a, b })
    }

    pub(crate) fn fdiv(&mut self, a: Value, b: Value) -> Value {
        self.binary_op(a, b, |a, b| Inst::Fdiv { a, b })
    }

    pub(crate) fn fcmp(&mut self, mode: CompareMode, a: Value, b: Value) -> Value {
        let fcmp = Inst::Fcmp { mode, a, b };
        let iref = InstRef::from_push(self.insts, fcmp);
        self.inst_types.push(smallvec![Type::i8]);
        self.block.inst_refs.borrow_mut().push(iref);
        Value::Inst(iref)
    }

    pub(crate) fn i2fp(&mut self, v: Value, to: Type) -> Value {
        assert!(to.is_fp(), "i2fp target type must be fp");
        self.type_conversion(v, to, |v| Inst::IntToFp { v })
    }
    pub(crate) fn fp2i(&mut self, v: Value, to: Type) -> Value {
        assert!(to.is_integral(), "fp2i target type must be int");
        self.type_conversion(v, to, |v| Inst::FpToInt { v })
    }

    pub(crate) fn load(&mut self, addr: Value, ty: Type) -> Value {
        let load = Inst::Load { addr };
        let iref = InstRef::from_push(self.insts, load);
        self.block.inst_refs.borrow_mut().push(iref);
        self.inst_types.push(smallvec![ty]);
        Value::Inst(iref)
    }

    pub(crate) fn store(&mut self, addr: Value, val: Value) {
        let store = Inst::Store { addr, val };
        let iref = InstRef::from_push(self.insts, store);
        self.block.inst_refs.borrow_mut().push(iref);
        self.inst_types.push(smallvec![]);
    }

    pub(crate) fn select(&mut self, cond: Value, x: Value, y: Value) -> Value {
        let select = Inst::Select { cond, x, y };
        let iref = InstRef::from_push(self.insts, select);
        self.block.inst_refs.borrow_mut().push(iref);
        self.copy_type(x);
        Value::Inst(iref) 
    }

    pub(crate) fn brif(
        &mut self, 
        cond: Value, 
        con: BlockRef,
        con_args: &[Value],
        alt: BlockRef,
        alt_args: &[Value]
    ) {
        let con_args = ValueVecRef::from_push(self.value_vecs, con_args.to_smallvec());
        let alt_args = ValueVecRef::from_push(self.value_vecs, alt_args.to_smallvec());

        let brif = Inst::BranchIf { 
            cond, 
            con, 
            con_args,
            alt,
            alt_args
        };
        let iref = InstRef::from_push(self.insts, brif);
        self.block.inst_refs.borrow_mut().push(iref);
        self.inst_types.push(smallvec![]);
    }

    pub(crate) fn ret(&mut self, values: &[Value]) {
        let values = ValueVecRef::from_push(self.value_vecs, values.to_smallvec());
        let ret = Inst::Return { values };
        let v = InstRef::from_push(self.insts, ret);
        self.block.inst_refs.borrow_mut().push(v);
        self.inst_types.push(smallvec![]);
    }

    pub(crate) fn call(&mut self, func_ref: FuncRef, arg_values: &[Value]) -> InstRef {
        let arg_values = ValueVecRef::from_push(self.value_vecs, arg_values.to_smallvec());
        let call = Inst::Call { func: func_ref, arguments: arg_values };
        let v = InstRef::from_push(self.insts, call);

        let callee_sig = &self.module_sigs[func_ref.get_inner()];
        self.block.inst_refs.borrow_mut().push(v);
        self.inst_types.push(SmallVec::from_slice(&callee_sig.return_types));

        v
    }

    pub(crate) fn call_indirect(&mut self, callee_sig: SigRef, func_ptr: Value, arg_values: &[Value]) -> InstRef {
        let arg_values = ValueVecRef::from_push(self.value_vecs, arg_values.to_smallvec());
        let call_indirect = Inst::CallIndirect { callee_sig, func_ptr, arguments: arg_values };
        let v = InstRef::from_push(self.insts, call_indirect);
        
        let callee_sig = &self.sigs[callee_sig];
        self.block.inst_refs.borrow_mut().push(v);
        self.inst_types.push(SmallVec::from_slice(&callee_sig.return_types));

        v
    }

    pub(crate) fn jmp(&mut self, target: BlockRef, arg_values: &[Value]) {
        let arg_values = ValueVecRef::from_push(self.value_vecs, arg_values.to_smallvec());
        let jmp = Inst::Jump { target, arguments: arg_values };
        let iref = InstRef::from_push(self.insts, jmp);
        self.block.inst_refs.borrow_mut().push(iref);
        self.inst_types.push(smallvec![]);
    }

    pub(crate) fn data_addr(&mut self, data_ref: DataRef) -> Value {
        let data_addr = Inst::DataAddr { data: data_ref };
        let iref = InstRef::from_push(self.insts, data_addr);
        self.block.inst_refs.borrow_mut().push(iref);
        self.inst_types.push(smallvec![Type::u64 /* TODO: ptrtype */]);
        Value::Inst(iref)
    }

    pub(crate) fn func_addr(&mut self, func_ref: FuncRef) -> Value {
        let func_addr = Inst::FuncAddr { func: func_ref };
        let iref = InstRef::from_push(self.insts, func_addr);
        self.block.inst_refs.borrow_mut().push(iref);
        self.inst_types.push(smallvec![Type::u64 /* TODO: ptrtype */]);
        Value::Inst(iref)
    }
}

make_type_idx!(BlockRef, Block);

#[derive(Debug)]
pub(crate) struct Block {
    pub(crate) inst_refs: RefCell<Vec<InstRef>>,
    pub(crate) block_args: Vec<Type>,
}

impl Block {
    fn new() -> Block {
        Block {
            inst_refs: Vec::new().into(),
            block_args: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Value {
    Inst(InstRef),
    BlockArgument(BlockRef, u32),
    TupleElement(InstRef, u32)
}

make_type_idx!(InstRef, Inst);
add_additional_index!(InstRef, TypeVec);

#[derive(Debug)]
pub(crate) enum Inst {
    Constant {
        val: Constant,
    },

    Add {
        a: Value,
        b: Value,
    },
    Sub {
        a: Value,
        b: Value,
    },
    Mul {
        a: Value,
        b: Value,
    },
    Div {
        a: Value,
        b: Value,
    },
    Modulo {
        a: Value,
        b: Value,
    },
    And {
        a: Value,
        b: Value
    },
    Or {
        a: Value,
        b: Value,
    },
    Xor {
        a: Value,
        b: Value
    },
    Shl {
        a: Value,
        b: Value
    },
    Ashr {
        a: Value,
        b: Value
    },
    Lshr {
        a: Value,
        b: Value
    },
    Icmp {
        mode: CompareMode,
        a: Value,
        b: Value
    },
    
    Fadd {
        a: Value,
        b: Value
    },
    Fsub {
        a: Value,
        b: Value
    },
    Fmul {
        a: Value,
        b: Value
    },
    Fdiv {
        a: Value,
        b: Value,
    },
    Fcmp {
        mode: CompareMode,
        a: Value,
        b: Value
    },

    IntToFp {
        v: Value
    },
    FpToInt {
        v: Value
    },

    Load {
        addr: Value,
    },
    Store {
        addr: Value,
        val: Value,
    },
    StackAddr {
        slot: StackSlotRef,
    },

    IntegerCast {
        v: Value,
    },
    FpCast {
        v: Value
    },

    CompareInt {
        a: Value,
        b: Value,
        mode: CompareMode,
    },
    CompareFloat {
        a: Value,
        b: Value,
        mode: CompareMode,
    },

    Select {
        cond: Value,
        x: Value,
        y: Value
    },

    BranchIf {
        cond: Value,
        con: BlockRef,
        con_args: ValueVecRef,
        alt: BlockRef,
        alt_args: ValueVecRef,
    },

    Return {
        values: ValueVecRef,
    },

    Jump {
        target: BlockRef,
        arguments: ValueVecRef,
    },

    Call {
        func: FuncRef,
        arguments: ValueVecRef,        
    },
    CallIndirect {
        callee_sig: SigRef,
        func_ptr: Value,
        arguments: ValueVecRef
    },

    FuncAddr {
        func: FuncRef
    },
    DataAddr {
        data: DataRef,
    },
}

impl Inst {
    pub(crate) fn mnemonic(&self) -> &str {
        match self {
            Inst::Constant { .. } => "const",
            Inst::Add { .. } => "add",
            Inst::Sub { .. } => "sub",
            Inst::Mul { .. } => "mul",
            Inst::Div { .. } => "div",
            Inst::Modulo { .. } => "modulo",
            Inst::And { .. } => "and",
            Inst::Or { .. } => "or",
            Inst::Xor { .. } => "xor",
            Inst::Shl { .. } => "shl",
            Inst::Ashr { .. } => "ashr",
            Inst::Lshr { .. } => "lshr",
            Inst::Icmp { .. } => "icmp",
            Inst::Fadd { .. } => "fadd",
            Inst::Fsub { .. } => "fsub",
            Inst::Fmul { .. } => "fmul",
            Inst::Fdiv { .. } => "fdiv",
            Inst::Fcmp { .. } => "fcmp",
            Inst::IntToFp { .. } => "i2fp",
            Inst::FpToInt { .. } => "fp2i",
            Inst::Load { .. } => "load",
            Inst::Store { .. } => "store",
            Inst::StackAddr { .. } => "stack_addr",
            Inst::IntegerCast { .. } => "icast",
            Inst::FpCast { .. } => "fcast",
            Inst::CompareInt { .. } => "icmp",
            Inst::CompareFloat { .. } => "fcmp",
            Inst::Select { .. } => "select",
            Inst::BranchIf { .. } => "brif",
            Inst::Return { .. } => "ret",
            Inst::Jump { .. } => "jmp",
            Inst::Call { .. } => "call",
            Inst::CallIndirect { .. } => "call_indirect",
            Inst::FuncAddr { .. } => "func_addr",
            Inst::DataAddr { .. } => "data_addr",
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

impl std::fmt::Display for CompareMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            CompareMode::LessThan => "lt",
            CompareMode::GreaterThan => "gt",
            CompareMode::LessThanOrEqual => "lte",
            CompareMode::GreaterThanOrEqual => "gte",
            CompareMode::Equal => "eq",
            CompareMode::NotEqual => "neq",
        };

        write!(f, "{name}")
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Inst(inst_ref) => write!(f, "v{}", inst_ref.0),
            Value::BlockArgument(block_ref, idx) => write!(f, "p{}.{}", block_ref.0, *idx),
            Value::TupleElement(inst_ref, component) => write!(f, "v{}.{}", inst_ref.0, component)
        }
    }
}

fn write_values(f: &mut std::fmt::Formatter, values: &[Value]) -> std::fmt::Result {
    for (idx, val) in values.iter().enumerate() {
        write!(f, "{val}")?;
        if idx + 1 < values.len() {
            write!(f, ", ")?;
        } 
    }

    Ok(())
}

// to print out an Inst properly, we need additional context 
// (block argument lists are "outlined" to small vectors held by FunctionDefinition)
struct DisplayInst<'inst>(&'inst Inst, &'inst FunctionDefinition);

impl std::fmt::Display for DisplayInst<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let DisplayInst(i, FunctionDefinition { value_vecs, ..}) = self;
        let m = i.mnemonic();
        
        match i {
            Inst::Constant { val } => write!(f, "{m} {val}"),
            Inst::Add { a, b } => write!(f, "{m} {a} {b}"),
            Inst::Sub { a, b } => write!(f, "{m} {a} {b}"),
            Inst::Mul { a, b } => write!(f, "{m} {a} {b}"),
            Inst::Div { a, b } => write!(f, "{m} {a} {b}"),
            Inst::Modulo { a, b } => write!(f, "{m} {a} {b}"),
            Inst::And { a, b } => write!(f, "{m} {a} {b}"),
            Inst::Or { a, b } => write!(f, "{m} {a} {b}"),
            Inst::Xor { a, b } => write!(f, "{m} {a} {b}"),
            Inst::Shl { a, b } => write!(f, "{m} {a} {b}"),
            Inst::Ashr { a, b } => write!(f, "{m} {a} {b}"),
            Inst::Lshr { a, b } => write!(f, "{m} {a} {b}"),
            Inst::Icmp { mode, a, b } => write!(f, "{m} {mode} {a} {b}"),
            Inst::Fadd { a, b } => write!(f, "{m} {a} {b}"),
            Inst::Fsub { a, b } => write!(f, "{m} {a} {b}"),
            Inst::Fmul { a, b } => write!(f, "{m} {a} {b}"),
            Inst::Fdiv { a, b } => write!(f, "{m} {a} {b}"),
            Inst::Fcmp { mode, a, b } => write!(f, "{m} {mode} {a} {b}"),
            Inst::IntToFp { v } => write!(f, "{m} {v}"),
            Inst::FpToInt { v } => write!(f, "{m} {v}"), 
            Inst::Load { addr } => write!(f, "{m} [{}]", addr),
            Inst::Store { addr, val } => write!(f, "{m} {val} [{addr}]"),
            Inst::IntegerCast { v } => write!(f, "{m} {v}"),
            Inst::FpCast { v } => write!(f, "{m} {v}"),
            Inst::StackAddr { slot } => write!(f, "{m} ss{}", slot.0),
            Inst::CompareInt { a, b, mode } => write!(f, "{m}.{mode} {a} {b}"),
            Inst::CompareFloat { a, b, mode } => write!(f, "{m}.{mode} {a} {b}"),
            Inst::Select { cond, x, y } => write!(f, "{m} {cond} {x} {y}"),
            Inst::BranchIf { cond, con, con_args, alt, alt_args } => {      
                write!(f, "{m} {cond} ")?;
                let mut write_block_call = |block: BlockRef, args: ValueVecRef| -> std::fmt::Result {
                    let args = &value_vecs[args];
                    write!(f, "b{}(", block.0)?;
                    write_values(f, args)?; 
                    write!(f, ") ")
                };
                write_block_call(*con, *con_args)?;
                write_block_call(*alt, *alt_args)
            },
            Inst::Return { values } => {
                let values = &value_vecs[*values];
                write!(f, "ret ")?;
                if values.len() > 1 {
                    write!(f, "(")?;
                }
                write_values(f, values)?;
                if values.len() > 1 {
                    write!(f, ")")?;
                }

                Ok(())
            },
            Inst::Jump { target, arguments } => {
                let arguments = &value_vecs[*arguments];
                write!(f, "{m} b{}(", target.0)?;
                write_values(f, arguments)?;
                write!(f, ")")
            },
            Inst::Call { func, arguments } => {
                let arguments = &value_vecs[*arguments];
                write!(f, "call f{}(", func.0)?;
                write_values(f, arguments)?;
                write!(f, ")")
            },
            Inst::CallIndirect { callee_sig, func_ptr, arguments } => {
                let arguments = &value_vecs[*arguments];
                write!(f, "call_indirect ({func_ptr})(")?;
                write_values(f, arguments)?;
                write!(f, ")")
            },
            Inst::FuncAddr { func } => {
                write!(f, "{m} f{}", func.0)
            },
            Inst::DataAddr { data } => {
                write!(f, "{m} d{}", data.0)
            }
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
        if self.return_types.len() >= 1 {
            write!(f, " -> ", )?;
            if self.return_types.len() == 1 {
                write!(f, "{}", self.return_types[0])?;
            }
            else {
                write!(f, "(")?;
                for (idx, t) in self.return_types.iter().enumerate() {
                    write!(f, "{t}")?;
                    if idx + 1 < self.return_types.len() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")?;
            }
        }

        Ok(())
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", &self.name)?;
        
        let Some(defn) = &self.definition else {
            return Ok(());
        };

        for (idx, slot) in defn.stack_slots.iter().enumerate() {
            writeln!(f, "ss{idx} = size {}, align {}", slot.size, slot.align)?;
        }

        for (block_idx, block) in defn.blocks.iter().enumerate() {
            write!(f, "b{block_idx}(")?;
            for (block_arg_idx, block_arg) in block.block_args.iter().enumerate() {
                write!(f, "p{block_idx}.{block_arg_idx} : {block_arg}")?;
                if block_arg_idx + 1 < block.block_args.len() {
                    write!(f, ", ")?;
                }
            }
            writeln!(f, "):")?;
            for &iref in block.inst_refs.borrow().iter() {
                let iref_types = &defn.inst_types[iref];
                if iref_types.len() > 1 {
                    write!(f, "  ")?;
                    write!(f, "v{} : (", iref.0)?;
                    for (idx, component_ty) in iref_types.iter().enumerate() {
                        write!(f, "{component_ty}")?;
                        if idx + 1 < iref_types.len() {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ") = {}", DisplayInst(&defn.insts[iref], defn))?;

                } 
                else if let Some(value_ty) = iref_types.first() {
                    write!(f, "  ")?;
                    writeln!(f, "v{} : {} = {}", iref.0, value_ty, DisplayInst(&defn.insts[iref], defn))?;
                }
                else {
                    write!(f, "  ")?;
                    writeln!(f, "{}", DisplayInst(&defn.insts[iref], defn))?;
                }
            }
        }

        Ok(())
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "functions:")?;
        let mut idx = 0;
        for (func, sig) in std::iter::zip(&self.functions, &self.signatures) {
            write!(f, "f{idx}: {sig}, ")?;
            writeln!(f, "{func}")?;
            idx += 1;
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
        let main_sig = Signature {
            argument_types: vec![],
            return_types: vec![],
        };

        let func = module.add_function("test_basic_ops".to_string(), main_sig);
        let mut fn_builder = module.define_function(func);

        let a = fn_builder.insert().const_i32(1);
        let b = fn_builder.insert().const_i32(2);
        let add = fn_builder.insert().add(a, b);
    }
}
