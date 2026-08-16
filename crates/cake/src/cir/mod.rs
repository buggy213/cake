use std::cell::RefCell;

use cake_util::{add_additional_index, make_type_idx};
use smallvec::{SmallVec, ToSmallVec, smallvec};

use crate::cir::intrinsics::Intrinsic;

#[derive(Debug)]
pub(crate) struct Module {
    functions: Vec<Function>,
    signatures: Vec<Signature>,
    data: Vec<Data>,
}

make_type_idx!(DataRef, Data);

#[derive(Debug)]
pub(crate) struct Data {
    pub(crate) name: Option<String>,
    pub(crate) read_only: bool,
    pub(crate) contents: DataContents
}

#[derive(Debug)]
pub(crate) enum DataContents {
    /// Data is defined as having some specific contents
    Defined(Box<[u8]>),
    /// Data contains zeroes, goes into .bss
    Zeros(usize),
    /// Data is undefined, must be linked against external symbol
    Undefined   
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

        // append function parameters as block params of entry block
        let mut entry_block = Block::new();
        let sig = &self.signatures[func.get_inner()];
        entry_block.block_args.extend_from_slice(&sig.argument_types);
        
        *definition = Some(FunctionDefinition { 
            insts: vec![], 
            inst_types: vec![],
            value_vecs: vec![],
            inst_uses: vec![],
            inst_block: vec![],
            blocks: vec![entry_block], 
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

    pub(crate) fn add_data(&mut self, name: String, read_only: bool) -> DataRef {
        let data = Data {
            name: Some(name),
            read_only,
            contents: DataContents::Undefined,
        };
        DataRef::from_push(&mut self.data, data)
    }

    pub(crate) fn define_data(&mut self, data: DataRef, contents: DataContents) {
        self.data[data].contents = contents;
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
    i16,
    i32,
    i64,
    ptr,

    f32,
    f64,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy)]
pub(crate) enum Constant {
    i8(i8),
    i16(i16),
    i32(i32),
    i64(i64),

    f32(f32),
    f64(f64),
}

impl Constant {
    pub(crate) fn is_zero(self) -> bool {
        match self {
            Constant::i8(v) => v == 0,
            Constant::i16(v) => v == 0,
            Constant::i32(v) => v == 0,
            Constant::i64(v) => v == 0,
            Constant::f32(v) => v == 0.0,
            Constant::f64(v) => v == 0.0,
        }
    }
}

impl Type {
    pub(crate) fn width(self) -> usize {
        match self {
            Type::i8 => 8,
            Type::i16 => 16,
            Type::i32 => 32,
            Type::i64 => 64,
            Type::ptr => 64,

            Type::f32 => 32,
            Type::f64 => 64,
        }
    }

    pub(crate) fn is_integral(self) -> bool {
        matches!(self, Type::i8 | Type::i16 | Type::i32 | Type::i64)
    }

    pub(crate) fn is_fp(self) -> bool {
        matches!(self, Type::f32 | Type::f64)
    }

    pub(crate) fn is_ptr(self) -> bool {
        matches!(self, Type::ptr)
    }
}

impl Constant {
    pub(crate) fn ty(self) -> Type {
        match self {
            Constant::i8(_) => Type::i8,
            Constant::i16(_) => Type::i16,
            Constant::i32(_) => Type::i32,
            Constant::i64(_) => Type::i64,
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
    pub(crate) argument_types: Vec<Type>,
    pub(crate) return_types: Vec<Type>,
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

// note: a use of a tuple element does not distinguish which element of the tuple is being used
// in order to save space
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Use {
    user: InstRef,
    operand_idx: u32,
}

type UseVec = SmallVec<[Use; 4]>;

#[derive(Debug)]
pub(crate) struct FunctionDefinition {
    pub(crate) insts: Vec<Inst>,
    pub(crate) inst_types: Vec<TypeVec>,
    pub(crate) value_vecs: Vec<ValueVec>,
    pub(crate) inst_uses: Vec<UseVec>,
    pub(crate) inst_block: Vec<Option<BlockRef>>,

    pub(crate) blocks: Vec<Block>,

    pub(crate) stack_slots: Vec<StackSlot>,
}

impl FunctionDefinition {
    pub(crate) fn uses(&self, def: Value) -> &[Use] {
        match def {
            Value::Inst(inst_ref) => {
                &self.inst_uses[inst_ref]
            },
            Value::BlockArgument(block_ref, idx) => {
                &self.blocks[block_ref].block_arg_uses[idx as usize]
            },
            Value::TupleElement(inst_ref, _) => {
                &self.inst_uses[inst_ref]
            },
        }
    }

    // returns remaining number of uses
    pub(crate) fn remove_use(&mut self, def: Value, use_: Use) -> usize {
        let use_vec = match def {
            Value::Inst(inst_ref) => {
                &mut self.inst_uses[inst_ref]
            },
            Value::BlockArgument(block_ref, idx) => {
                &mut self.blocks[block_ref].block_arg_uses[idx as usize]
            },
            Value::TupleElement(inst_ref, _) => {
                &mut self.inst_uses[inst_ref]
            },
        };

        let delete_idx = use_vec.iter().position(|x| use_ == *x).expect("failed to remove use");
        use_vec.swap_remove(delete_idx);

        use_vec.len()
    }
}

make_type_idx!(StackSlotRef, StackSlot);

#[derive(Debug, Clone, Copy)]
pub(crate) struct StackSlot {
    pub(crate) size: u32,
    pub(crate) align: u32,
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
            current_block: self.current_block,
            all_blocks: &mut self.func.blocks,

            insts: &mut self.func.insts,
            inst_types: &mut self.func.inst_types,
            inst_uses: &mut self.func.inst_uses,
            inst_block: &mut self.func.inst_block,
            value_vecs: &mut self.func.value_vecs,

            sigs: self.sigs,
            module_sigs: self.module_sigs,
        }
    }

    pub(crate) fn declare_anonymous_data(&mut self, read_only: bool) -> DataRef {
        let data = Data {
            name: None,
            read_only,
            contents: DataContents::Undefined,
        };
        DataRef::from_push(self.module_data, data)
    }

    pub(crate) fn define_data(&mut self, data_ref: DataRef, contents: Box<[u8]>) {
        self.module_data[data_ref].contents = DataContents::Defined(contents);
    }
}

pub(crate) struct BlockBuilder<'block> {
    current_block: BlockRef,
    all_blocks: &'block mut [Block],

    insts: &'block mut Vec<Inst>,
    inst_types: &'block mut Vec<TypeVec>,
    inst_uses: &'block mut Vec<UseVec>,
    inst_block: &'block mut Vec<Option<BlockRef>>,
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
    
    fn add_use(&mut self, def: Value, use_: Use) {
        match def {
            Value::Inst(inst_ref) => {
                self.inst_uses[inst_ref].push(use_);
            },
            Value::BlockArgument(block_ref, idx) => {
                self.all_blocks[block_ref].block_arg_uses[idx as usize].push(use_);
            },
            Value::TupleElement(inst_ref, _) => {
                self.inst_uses[inst_ref].push(use_);
            },
        }
    }

    fn add_inst(
        &mut self, 
        inst: Inst, 
        inst_types: impl Into<TypeVec>
    ) -> InstRef {
        self.inst_types.push(inst_types.into());
        let iref = InstRef::from_push(self.insts, inst);
        self.all_blocks[self.current_block].inst_refs.borrow_mut().push(iref);
        
        let num_operands = inst.num_operands(self.value_vecs);
        for operand_idx in 0..num_operands {
            let use_ = Use {
                user: iref,
                operand_idx: operand_idx as u32,
            };

            let def = inst.operand(self.value_vecs, operand_idx);
            self.add_use(def, use_);
        }
        self.inst_uses.push(smallvec![]);
        self.inst_block.push(Some(self.current_block));
        
        iref
    }

    fn add_pred(&mut self, pred: BlockRef, succ: BlockRef) {
        self.all_blocks[succ].preds.push(pred);
    }

    fn constant(&mut self, ty: Type, val: Constant) -> Value {
        assert!(val.ty() == ty, "type mismatch while inserting constant");

        let constant = Inst::Constant { val };
        Value::Inst(self.add_inst(constant, smallvec![ty]))
    }

    pub(crate) fn const_u32(&mut self, val: u32) -> Value {
        self.const_i32(val as i32)
    }

    pub(crate) fn const_u64(&mut self, val: u64) -> Value {
        self.const_i64(val as i64)
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
            Type::i16 => Constant::i16(c as i16),
            Type::i32 => Constant::i32(c as i32),
            Type::i64 => Constant::i64(c as i64),
            _ => panic!("iconst_trunc called with (val : {ty})")
        };

        self.constant(ty, v)
    }

    pub(crate) fn stack_addr(&mut self, slot: StackSlotRef) -> Value {
        let stack_addr = Inst::StackAddr { slot };
        Value::Inst(self.add_inst(stack_addr, smallvec![Type::ptr]))
    }

    fn type_conversion(&mut self, val: Value, to: Type, op: fn(Value) -> Inst) -> Value {
        let op = op(val);
        Value::Inst(self.add_inst(op, smallvec![to]))
    }

    pub(crate) fn sext(&mut self, val: Value, to: Type) -> Value {
        self.type_conversion(val, to, |v| Inst::Sext { v })
    }
    pub(crate) fn zext(&mut self, val: Value, to: Type) -> Value {
        self.type_conversion(val, to, |v| Inst::Zext { v })
    }
    pub(crate) fn trunc(&mut self, val: Value, to: Type) -> Value {
        self.type_conversion(val, to, |v| Inst::Truncate { v })
    }
    pub(crate) fn fcast(&mut self, val: Value, to: Type) -> Value {
        self.type_conversion(val, to, |v| Inst::FpCast { v })
    }

    pub(crate) fn padd(&mut self, ptr: Value, offset: Value) -> Value {
        self.binary_op(ptr, offset, |ptr, offset| Inst::PtrAdd { ptr, offset })
    }
    pub(crate) fn p2i(&mut self, v: Value) -> Value {
        self.type_conversion(v, Type::i64, |v| Inst::PtrToInt { v })
    }
    pub(crate) fn i2p(&mut self, v: Value) -> Value {
        self.type_conversion(v, Type::i64, |v| Inst::IntToPtr { v })
    }

    fn binary_op(&mut self, a: Value, b: Value, op: fn(Value, Value) -> Inst) -> Value {
        let op = op(a, b);
        let ty = self.type_of(a);
        Value::Inst(self.add_inst(op, smallvec![ty]))
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

    pub(crate) fn icmp(&mut self, mode: CompareMode, a: Value, b: Value, signed: bool) -> Value {
        let icmp = Inst::Icmp { mode, a, b, signed };
        Value::Inst(self.add_inst(icmp, smallvec![Type::i8]))
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
        Value::Inst(self.add_inst(fcmp, smallvec![Type::i8]))
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
        Value::Inst(self.add_inst(load, smallvec![ty]))
    }

    pub(crate) fn store(&mut self, addr: Value, val: Value) {
        let store = Inst::Store { addr, val };
        self.add_inst(store, smallvec![]);
    }

    pub(crate) fn select(&mut self, cond: Value, x: Value, y: Value) -> Value {
        let select = Inst::Select { cond, x, y };
        let ty = self.type_of(x);
        Value::Inst(self.add_inst(select, smallvec![ty]))
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
        self.add_inst(brif, smallvec![]);
        self.add_pred(self.current_block, con);
        self.add_pred(self.current_block, alt);
    }

    pub(crate) fn ret(&mut self, values: &[Value]) {
        let values = ValueVecRef::from_push(self.value_vecs, values.to_smallvec());
        let ret = Inst::Return { values };
        self.add_inst(ret, smallvec![]);
    }

    pub(crate) fn call(&mut self, func_ref: FuncRef, arg_values: &[Value]) -> InstRef {
        let arg_values = ValueVecRef::from_push(self.value_vecs, arg_values.to_smallvec());
        let call = Inst::Call { func: func_ref, arguments: arg_values };
        let callee_sig = &self.module_sigs[func_ref.get_inner()];
        self.add_inst(call, SmallVec::from_slice(&callee_sig.return_types))
    }

    pub(crate) fn call_indirect(&mut self, callee_sig: SigRef, func_ptr: Value, arg_values: &[Value]) -> InstRef {
        let arg_values = ValueVecRef::from_push(self.value_vecs, arg_values.to_smallvec());
        let call_indirect = Inst::CallIndirect { callee_sig, func_ptr, arguments: arg_values };
        let callee_sig = &self.sigs[callee_sig];
        self.add_inst(call_indirect, SmallVec::from_slice(&callee_sig.return_types))
    }

    pub(crate) fn jmp(&mut self, target: BlockRef, arg_values: &[Value]) {
        let arg_values = ValueVecRef::from_push(self.value_vecs, arg_values.to_smallvec());
        let jmp = Inst::Jump { target, arguments: arg_values };
        self.add_inst(jmp, smallvec![]);
        self.add_pred(self.current_block, target);
    }

    pub(crate) fn data_addr(&mut self, data_ref: DataRef) -> Value {
        let data_addr = Inst::DataAddr { data: data_ref };
        Value::Inst(self.add_inst(data_addr, smallvec![Type::ptr]))
    }

    pub(crate) fn func_addr(&mut self, func_ref: FuncRef) -> Value {
        let func_addr = Inst::FuncAddr { func: func_ref };
        Value::Inst(self.add_inst(func_addr, smallvec![Type::ptr]))
    }
}

make_type_idx!(BlockRef, Block);

#[derive(Debug)]
pub(crate) struct Block {
    pub(crate) inst_refs: RefCell<Vec<InstRef>>,
    pub(crate) block_args: Vec<Type>,
    pub(crate) block_arg_uses: Vec<UseVec>,

    pub(crate) preds: Vec<BlockRef>,
    pub(crate) is_entry: bool,
}

impl Block {
    fn new() -> Block {
        Block {
            inst_refs: Vec::new().into(),
            block_args: Vec::new(),
            block_arg_uses: Vec::new(),

            preds: Vec::new(),
            is_entry: false
        }
    }

    fn new_entry_block() -> Block {
        Block {
            is_entry: true,
            ..Block::new()
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
add_additional_index!(InstRef, UseVec);

#[derive(Debug, Clone, Copy)]
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
        b: Value,
        signed: bool
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

    Zext {
        v: Value,
    },
    Sext {
        v: Value
    },
    Truncate {
        v: Value
    },
    FpCast {
        v: Value
    },

    PtrAdd {
        ptr: Value,
        offset: Value
    },
    PtrToInt {
        v: Value
    },
    IntToPtr {
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

    Intrinsic {
        intrinsic: Intrinsic,
        arguments: ValueVecRef,
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

impl Inst {
    pub(crate) fn mnemonic(&self) -> &'static str {
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
            Inst::Sext { .. } => "sext",
            Inst::Zext { .. } => "zext",
            Inst::Truncate { .. } => "truncate",
            Inst::FpCast { .. } => "fcast",
            Inst::PtrAdd { .. } => "padd",
            Inst::PtrToInt { .. } => "p2i",
            Inst::IntToPtr { .. } => "i2p",
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
            Inst::Intrinsic { .. } => "intrinsic",
        }
    }
    pub(crate) fn is_terminator(&self) -> bool {
        matches!(self,
            Inst::BranchIf { .. }
            | Inst::Jump { .. }
            | Inst::Return { .. }
        )
    }
    pub(crate) fn has_side_effects(&self) -> bool {
        match self {
            Inst::Constant { val } => false,
            Inst::Add { a, b } => false,
            Inst::Sub { a, b } => false,
            Inst::Mul { a, b } => false,
            Inst::Div { a, b } => false,
            Inst::Modulo { a, b } => false,
            Inst::And { a, b } => false,
            Inst::Or { a, b } => false,
            Inst::Xor { a, b } => false,
            Inst::Shl { a, b } => false,
            Inst::Ashr { a, b } => false,
            Inst::Lshr { a, b } => false,
            Inst::Icmp { mode, a, b, signed } => false,
            Inst::Fadd { a, b } => false,
            Inst::Fsub { a, b } => false,
            Inst::Fmul { a, b } => false,
            Inst::Fdiv { a, b } => false,
            Inst::Fcmp { mode, a, b } => false,
            Inst::IntToFp { v } => false,
            Inst::FpToInt { v } => false,
            Inst::Load { addr } => false,
            Inst::Store { addr, val } => true,
            Inst::StackAddr { slot } => false,
            Inst::Zext { v } => false,
            Inst::Sext { v } => false,
            Inst::Truncate { v } => false,
            Inst::FpCast { v } => false,
            Inst::PtrAdd { ptr, offset } => false,
            Inst::PtrToInt { v } => false,
            Inst::IntToPtr { v } => false,
            Inst::CompareInt { a, b, mode } => false,
            Inst::CompareFloat { a, b, mode } => false,
            Inst::Select { cond, x, y } => false,
            Inst::BranchIf { cond, con, con_args, alt, alt_args } => true,
            Inst::Return { values } => true,
            Inst::Jump { target, arguments } => true,
            Inst::Call { func, arguments } => true,
            Inst::CallIndirect { callee_sig, func_ptr, arguments } => true,
            Inst::FuncAddr { func } => false,
            Inst::DataAddr { data } => false,
            Inst::Intrinsic { intrinsic, arguments } => true,
        }
    }

    pub(crate) fn num_operands(&self, value_vecs: &[ValueVec]) -> usize {
        match self {
            Inst::Constant { .. } => 0,
            Inst::Add { .. } => 2,
            Inst::Sub { .. } => 2,
            Inst::Mul { .. } => 2,
            Inst::Div { .. } => 2,
            Inst::Modulo { .. } => 2,
            Inst::And { .. } => 2,
            Inst::Or { .. } => 2,
            Inst::Xor { .. } => 2,
            Inst::Shl { .. } => 2,
            Inst::Ashr { .. } => 2,
            Inst::Lshr { .. } => 2,
            Inst::Icmp { .. } => 2,
            Inst::Fadd { .. } => 2,
            Inst::Fsub { .. } => 2,
            Inst::Fmul { .. } => 2,
            Inst::Fdiv { .. } => 2,
            Inst::Fcmp { .. } => 2,
            Inst::IntToFp { .. } => 1,
            Inst::FpToInt { .. } => 1,
            Inst::Load { .. } => 1,
            Inst::Store { .. } => 2,
            Inst::StackAddr { .. } => 0,
            Inst::Zext { .. } => 1,
            Inst::Sext { .. } => 1,
            Inst::Truncate { .. } => 1,
            Inst::FpCast { .. } => 1,
            Inst::PtrAdd { .. } => 2,
            Inst::PtrToInt { .. } => 1,
            Inst::IntToPtr { .. } => 1,
            Inst::CompareInt { .. } => 2,
            Inst::CompareFloat { .. } => 2,
            Inst::Select { .. } => 3,
            Inst::BranchIf { con_args, alt_args, .. } => {
                1 + value_vecs[*con_args].len() + value_vecs[*alt_args].len()
            },
            Inst::Return { values, .. } => {
                value_vecs[*values].len()
            },
            Inst::Jump { arguments, .. } => {
                value_vecs[*arguments].len()
            },
            Inst::Call { arguments, .. } => {
                value_vecs[*arguments].len()
            },
            Inst::CallIndirect { arguments, .. } => {
                1 + value_vecs[*arguments].len()
            },
            Inst::FuncAddr { .. } => 0,
            Inst::DataAddr { .. } => 0,
            Inst::Intrinsic { arguments, .. } => {
                value_vecs[*arguments].len()
            },
        }
    }

    pub(crate) fn operand(&self, value_vecs: &[ValueVec], idx: usize) -> Value {
        let bad_operand_access = || {
            panic!("bad operand access to {idx} for {}", self.mnemonic())
        };

        macro_rules! basic_op {
            ($idx:expr, $v:expr, $bad_operand_access:expr) => {
                match $idx {
                    0 => *$v,
                    _ => $bad_operand_access(),
                }
            };
            ($idx:expr, $a:expr, $b:expr, $bad_operand_access:expr) => {
                match $idx {
                    0 => *$a,
                    1 => *$b,
                    _ => $bad_operand_access(),
                }
            };
            ($idx:expr, $a:expr, $b:expr, $c:expr, $bad_operand_access:expr) => {
                match $idx {
                    0 => *$a,
                    1 => *$b,
                    2 => *$c,
                    _ => $bad_operand_access(),
                }
            };
        }        
        match self {
            Inst::Constant { .. } => bad_operand_access(),
            Inst::Add { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Sub { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Mul { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Div { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Modulo { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::And { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Or { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Xor { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Shl { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Ashr { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Lshr { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Icmp { mode, a, b, signed } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Fadd { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Fsub { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Fmul { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Fdiv { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Fcmp { mode, a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::IntToFp { v } => basic_op!(idx, v, bad_operand_access),
            Inst::FpToInt { v } => basic_op!(idx, v, bad_operand_access),
            Inst::Load { addr } => basic_op!(idx, addr, bad_operand_access),
            Inst::Store { addr, val } => basic_op!(idx, addr, val, bad_operand_access),
            Inst::StackAddr { slot } => bad_operand_access(),
            Inst::Zext { v } => basic_op!(idx, v, bad_operand_access),
            Inst::Sext { v } => basic_op!(idx, v, bad_operand_access),
            Inst::Truncate { v } => basic_op!(idx, v, bad_operand_access),
            Inst::FpCast { v } => basic_op!(idx, v, bad_operand_access),
            Inst::PtrAdd { ptr, offset } => basic_op!(idx, ptr, offset, bad_operand_access),
            Inst::PtrToInt { v } => basic_op!(idx, v, bad_operand_access),
            Inst::IntToPtr { v } => basic_op!(idx, v, bad_operand_access),
            Inst::CompareInt { a, b, mode } => basic_op!(idx, a, b, bad_operand_access),
            Inst::CompareFloat { a, b, mode } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Select { cond, x, y } => basic_op!(idx, cond, x, y, bad_operand_access),
            Inst::BranchIf { cond, con, con_args, alt, alt_args } => {
                let con_args_len = value_vecs[*con_args].len();
                let alt_args_len = value_vecs[*alt_args].len();
                if idx == 0 {
                    *cond
                }
                else if idx < 1 + con_args_len {
                    value_vecs[*con_args][idx - 1]
                }
                else if idx < 1 + con_args_len + alt_args_len {
                    value_vecs[*alt_args][idx - (1 + con_args_len)]
                }
                else {
                    bad_operand_access()
                }
            },
            Inst::Return { values } => {
                let values_len = value_vecs[*values].len();
                if idx < values_len {
                    value_vecs[*values][idx]
                }
                else {
                    bad_operand_access()
                }
            },
            Inst::Jump { arguments, .. }
            | Inst::Call { arguments, .. }
            | Inst::Intrinsic { arguments, .. } => {
                let arguments_len = value_vecs[*arguments].len();
                if idx < arguments_len {
                    value_vecs[*arguments][idx]
                }
                else {
                    bad_operand_access()
                }
            },
            Inst::CallIndirect { callee_sig, func_ptr, arguments } => {
                let arguments_len = value_vecs[*arguments].len();
                if idx == 0 {
                    *func_ptr
                }
                else if idx < 1 + arguments_len {
                    value_vecs[*arguments][idx - 1]
                }
                else {
                    bad_operand_access()
                }
            },
            Inst::FuncAddr { func } => bad_operand_access(),
            Inst::DataAddr { data } => bad_operand_access(),
        }
    }

    pub(crate) fn operand_mut<'inst>(&'inst mut self, value_vecs: &'inst mut [ValueVec], idx: usize) -> &'inst mut Value {
        let mnemonic = self.mnemonic();
        let bad_operand_access = || {
            panic!("bad operand access to {idx} for {mnemonic}")
        };

        macro_rules! basic_op {
            ($idx:expr, $v:expr, $bad_operand_access:expr) => {
                match $idx {
                    0 => $v,
                    _ => $bad_operand_access(),
                }
            };
            ($idx:expr, $a:expr, $b:expr, $bad_operand_access:expr) => {
                match $idx {
                    0 => $a,
                    1 => $b,
                    _ => $bad_operand_access(),
                }
            };
            ($idx:expr, $a:expr, $b:expr, $c:expr, $bad_operand_access:expr) => {
                match $idx {
                    0 => $a,
                    1 => $b,
                    2 => $c,
                    _ => $bad_operand_access(),
                }
            };
        }        
        match self {
            Inst::Constant { .. } => bad_operand_access(),
            Inst::Add { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Sub { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Mul { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Div { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Modulo { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::And { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Or { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Xor { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Shl { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Ashr { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Lshr { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Icmp { mode, a, b, signed } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Fadd { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Fsub { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Fmul { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Fdiv { a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Fcmp { mode, a, b } => basic_op!(idx, a, b, bad_operand_access),
            Inst::IntToFp { v } => basic_op!(idx, v, bad_operand_access),
            Inst::FpToInt { v } => basic_op!(idx, v, bad_operand_access),
            Inst::Load { addr } => basic_op!(idx, addr, bad_operand_access),
            Inst::Store { addr, val } => basic_op!(idx, addr, val, bad_operand_access),
            Inst::StackAddr { slot } => bad_operand_access(),
            Inst::Zext { v } => basic_op!(idx, v, bad_operand_access),
            Inst::Sext { v } => basic_op!(idx, v, bad_operand_access),
            Inst::Truncate { v } => basic_op!(idx, v, bad_operand_access),
            Inst::FpCast { v } => basic_op!(idx, v, bad_operand_access),
            Inst::PtrAdd { ptr, offset } => basic_op!(idx, ptr, offset, bad_operand_access),
            Inst::PtrToInt { v } => basic_op!(idx, v, bad_operand_access),
            Inst::IntToPtr { v } => basic_op!(idx, v, bad_operand_access),
            Inst::CompareInt { a, b, mode } => basic_op!(idx, a, b, bad_operand_access),
            Inst::CompareFloat { a, b, mode } => basic_op!(idx, a, b, bad_operand_access),
            Inst::Select { cond, x, y } => basic_op!(idx, cond, x, y, bad_operand_access),
            Inst::BranchIf { cond, con, con_args, alt, alt_args } => {
                let con_args_len = value_vecs[*con_args].len();
                let alt_args_len = value_vecs[*alt_args].len();
                if idx == 0 {
                    cond
                }
                else if idx < 1 + con_args_len {
                    &mut value_vecs[*con_args][idx - 1]
                }
                else if idx < 1 + con_args_len + alt_args_len {
                    &mut value_vecs[*alt_args][idx - (1 + con_args_len)]
                }
                else {
                    bad_operand_access()
                }
            },
            Inst::Return { values } => {
                let values_len = value_vecs[*values].len();
                if idx < values_len {
                    &mut value_vecs[*values][idx]
                }
                else {
                    bad_operand_access()
                }
            },
            Inst::Jump { arguments, .. }
            | Inst::Call { arguments, .. }
            | Inst::Intrinsic { arguments, .. } => {
                let arguments_len = value_vecs[*arguments].len();
                if idx < arguments_len {
                    &mut value_vecs[*arguments][idx]
                }
                else {
                    bad_operand_access()
                }
            },
            Inst::CallIndirect { callee_sig, func_ptr, arguments } => {
                let arguments_len = value_vecs[*arguments].len();
                if idx == 0 {
                    func_ptr
                }
                else if idx < 1 + arguments_len {
                    &mut value_vecs[*arguments][idx - 1]
                }
                else {
                    bad_operand_access()
                }
            },
            Inst::FuncAddr { func } => bad_operand_access(),
            Inst::DataAddr { data } => bad_operand_access(),
        }
    }
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::i8(v) => write!(f, "{v}"),
            Constant::i16(v) => write!(f, "{v}"),
            Constant::i32(v) => write!(f, "{v}"),
            Constant::i64(v) => write!(f, "{v}"),
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
            Inst::Icmp { mode, a, b, signed } => {
                let signed = if *signed { "s" } else { "u" };
                write!(f, "{m} {signed}{mode} {a} {b}")
            }
            Inst::Fadd { a, b } => write!(f, "{m} {a} {b}"),
            Inst::Fsub { a, b } => write!(f, "{m} {a} {b}"),
            Inst::Fmul { a, b } => write!(f, "{m} {a} {b}"),
            Inst::Fdiv { a, b } => write!(f, "{m} {a} {b}"),
            Inst::Fcmp { mode, a, b } => write!(f, "{m} {mode} {a} {b}"),
            Inst::IntToFp { v } => write!(f, "{m} {v}"),
            Inst::FpToInt { v } => write!(f, "{m} {v}"), 
            Inst::Load { addr } => write!(f, "{m} [{}]", addr),
            Inst::Store { addr, val } => write!(f, "{m} {val} [{addr}]"),
            Inst::Sext { v } => write!(f, "{m} {v}"),
            Inst::Zext { v } => write!(f, "{m} {v}"),
            Inst::Truncate { v } => write!(f, "{m} {v}"),
            Inst::FpCast { v } => write!(f, "{m} {v}"),
            Inst::PtrAdd { ptr, offset } => write!(f, "{m} {ptr} {offset}"),
            Inst::PtrToInt { v } => write!(f, "{m} {v}"),
            Inst::IntToPtr { v } => write!(f, "{m} {v}"),
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
            },

            Inst::Intrinsic { intrinsic, arguments } => {
                let arguments = &value_vecs[*arguments];
                write!(f, "{m} {intrinsic}(")?;
                write_values(f, arguments)?;
                write!(f, ")")
            }
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let variant_name = match *self {
            Type::i8 => "i8",
            Type::i16 => "i16",
            Type::i32 => "i32",
            Type::i64 => "i64",
            Type::f32 => "f32",
            Type::f64 => "f64",
            Type::ptr => "ptr",
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

pub(crate) mod intrinsics;
pub(crate) mod ast2cir;
pub(crate) mod verifier;

// Optimization passes
pub(crate) mod dce;

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
