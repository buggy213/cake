use std::{assert_matches, cell::RefCell};

use cake_util::{IndexSlice, IndexVec, add_additional_index, index_vec, make_type_idx};
use smallvec::{SmallVec, ToSmallVec, smallvec};

use crate::cir::intrinsics::Intrinsic;

#[derive(Debug)]
pub(crate) struct Module {
    functions: IndexVec<FuncRef, Function>,
    signatures: IndexVec<FuncRef, Signature>,
    data: IndexVec<DataRef, Data>,
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
            functions: IndexVec::new(),
            signatures: IndexVec::new(),
            data: IndexVec::new(),
        }
    }

    pub(crate) fn add_function(&mut self, name: String, signature: Signature) -> FuncRef {
        self.signatures.push(signature);
        let func = Function {
            name,
            
            external_signatures: index_vec![],
            definition: None,
        };

        FuncRef::from_push2(&mut self.functions, func)
    }

    pub(crate) fn define_function(&'_ mut self, func: FuncRef) -> FunctionBuilder<'_> {
        let Function { 
            name: _, 
            external_signatures, 
            definition 
        } = &mut self.functions[func];

        // append function parameters as block params of entry block
        let mut entry_block = Block::new();
        let sig = &self.signatures[func];
        entry_block.block_args.extend_from_slice(&sig.argument_types);
        entry_block.block_arg_uses.resize(entry_block.block_args.len(), smallvec![]);
        
        *definition = Some(FunctionDefinition { 
            insts: index_vec![], 
            inst_types: index_vec![],
            value_vecs: index_vec![],
            inst_uses: index_vec![],
            inst_block: index_vec![],
            blocks: index_vec![entry_block], 
            stack_slots: index_vec![]
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
        DataRef::from_push2(&mut self.data, data)
    }

    pub(crate) fn define_data(&mut self, data: DataRef, contents: DataContents) {
        self.data[data].contents = contents;
    }

    pub(crate) fn functions(&self) -> &IndexSlice<FuncRef, [Function]> {
        &self.functions
    }

    pub(crate) fn signatures(&self) -> &IndexSlice<FuncRef, [Signature]> {
        &self.signatures
    }

    pub(crate) fn data(&self) -> &IndexSlice<DataRef, [Data]> {
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
    pub(crate) external_signatures: IndexVec<SigRef, Signature>,
    
    pub(crate) definition: Option<FunctionDefinition> 
}

/// An index into the operands of an `Inst`. SSA values only; References to StackSlot, Function, and Data
/// are not considered as operands (for now)
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct OperandCoord {
    kind: u32,
    idx: u32,
}

/// A use of an SSA value
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Use {
    user: InstRef,
    operand_coord: OperandCoord,
}

type UseVec = SmallVec<[Use; 4]>;

#[derive(Debug)]
pub(crate) struct FunctionDefinition {
    pub(crate) insts: IndexVec<InstRef, Inst>,
    pub(crate) inst_types: IndexVec<InstRef, TypeVec>,
    pub(crate) value_vecs: IndexVec<ValueVecRef, ValueVec>,
    pub(crate) inst_uses: IndexVec<InstRef, UseVec>,
    pub(crate) inst_block: IndexVec<InstRef, Option<BlockRef>>,

    pub(crate) blocks: IndexVec<BlockRef, Block>,

    pub(crate) stack_slots: IndexVec<StackSlotRef, StackSlot>,
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
    
    // Low-level functions to manipulate the def-use chains directly, primarily meant for DCE.
    // Other passes should use RAUW (replace-all-uses-with), RO (replace-operand), etc. to deal
    // with the bookkeeping. 

    fn uses_mut(&mut self, def: Value) -> &mut UseVec {
        match def {
            Value::Inst(inst_ref) => {
                &mut self.inst_uses[inst_ref]
            },
            Value::BlockArgument(block_ref, idx) => {
                &mut self.blocks[block_ref].block_arg_uses[idx as usize]
            },
            Value::TupleElement(inst_ref, _) => {
                &mut self.inst_uses[inst_ref]
            },
        }
    }

    /// Returns remaining number of uses.
    fn remove_use(&mut self, def: Value, use_: Use) -> usize {
        let use_vec = self.uses_mut(def);

        let delete_idx = use_vec.iter().position(|x| use_ == *x).expect("failed to remove use");
        use_vec.swap_remove(delete_idx);

        use_vec.len()
    }
    
    fn add_use(&mut self, def: Value, use_: Use) {
        let use_vec = self.uses_mut(def);
        use_vec.push(use_);
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
    sigs: &'func mut IndexVec<SigRef, Signature>,

    module_sigs: &'func IndexSlice<FuncRef, [Signature]>,
    module_data: &'func mut IndexVec<DataRef, Data>
}

impl<'func> FunctionBuilder<'func> {
    pub(crate) fn add_block(&mut self) -> BlockRef {
        let block = Block::new();
        BlockRef::from_push2(&mut self.func.blocks, block)
    }

    pub(crate) fn add_stack_slot(&mut self, size: u32, align: u32) -> StackSlotRef {
        let slot = StackSlot { size, align };

        StackSlotRef::from_push2(&mut self.func.stack_slots, slot)
    }

    pub(crate) fn set_block(&mut self, block: BlockRef) {
        self.current_block = block;
    }

    pub(crate) fn add_block_arg(&mut self, ty: Type) -> Value {
        let block = &mut self.func.blocks[self.current_block];
        let block_args = &mut block.block_args;
        let block_arg_uses = &mut block.block_arg_uses;

        let block_arg_idx = block_args.len();
        block_args.push(ty);
        block_arg_uses.push(smallvec![]);
        
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
        DataRef::from_push2(self.module_data, data)
    }

    pub(crate) fn define_data(&mut self, data_ref: DataRef, contents: Box<[u8]>) {
        self.module_data[data_ref].contents = DataContents::Defined(contents);
    }
}

pub(crate) struct BlockBuilder<'block> {
    current_block: BlockRef,
    all_blocks: &'block mut IndexSlice<BlockRef, [Block]>,

    insts: &'block mut IndexVec<InstRef, Inst>,
    inst_types: &'block mut IndexVec<InstRef, TypeVec>,
    inst_uses: &'block mut IndexVec<InstRef, UseVec>,
    inst_block: &'block mut IndexVec<InstRef, Option<BlockRef>>,
    value_vecs: &'block mut IndexVec<ValueVecRef, ValueVec>,
    
    sigs: &'block IndexVec<SigRef, Signature>,
    module_sigs: &'block IndexSlice<FuncRef, [Signature]>
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
        let iref = InstRef::from_push2(self.insts, inst);
        self.all_blocks[self.current_block].inst_refs.borrow_mut().push(iref);
        
        for operand_coord in inst.operand_coord_iter(self.value_vecs) {
            let use_ = Use {
                user: iref,
                operand_coord,
            };

            let def = inst.get_operand(self.value_vecs, operand_coord);
            self.add_use(def, use_);
        }
        self.inst_uses.push(smallvec![]);
        self.inst_block.push(Some(self.current_block));
        
        iref
    }

    fn add_pred(&mut self, pred: BlockPredecessor, succ: BlockRef) {
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
        let con_args = ValueVecRef::from_push2(self.value_vecs, con_args.to_smallvec());
        let alt_args = ValueVecRef::from_push2(self.value_vecs, alt_args.to_smallvec());

        let brif = Inst::BranchIf { 
            cond, 
            con, 
            con_args,
            alt,
            alt_args
        };
        self.add_inst(brif, smallvec![]);
        self.add_pred(BlockPredecessor { pred_ref: self.current_block, edge_idx: 0 }, con);
        self.add_pred(BlockPredecessor { pred_ref: self.current_block, edge_idx: 1 }, alt);
    }

    pub(crate) fn ret(&mut self, values: &[Value]) {
        let values = ValueVecRef::from_push2(self.value_vecs, values.to_smallvec());
        let ret = Inst::Return { values };
        self.add_inst(ret, smallvec![]);
    }

    pub(crate) fn call(&mut self, func_ref: FuncRef, arg_values: &[Value]) -> InstRef {
        let arg_values = ValueVecRef::from_push2(self.value_vecs, arg_values.to_smallvec());
        let call = Inst::Call { func: func_ref, arguments: arg_values };
        let callee_sig = &self.module_sigs[func_ref];
        self.add_inst(call, SmallVec::from_slice(&callee_sig.return_types))
    }

    pub(crate) fn call_indirect(&mut self, callee_sig: SigRef, func_ptr: Value, arg_values: &[Value]) -> InstRef {
        let arg_values = ValueVecRef::from_push2(self.value_vecs, arg_values.to_smallvec());
        let call_indirect = Inst::CallIndirect { callee_sig, func_ptr, arguments: arg_values };
        let callee_sig = &self.sigs[callee_sig];
        self.add_inst(call_indirect, SmallVec::from_slice(&callee_sig.return_types))
    }

    pub(crate) fn jmp(&mut self, target: BlockRef, arg_values: &[Value]) {
        let arg_values = ValueVecRef::from_push2(self.value_vecs, arg_values.to_smallvec());
        let jmp = Inst::Jump { target, arguments: arg_values };
        self.add_inst(jmp, smallvec![]);
        self.add_pred(BlockPredecessor { pred_ref: self.current_block, edge_idx: 0 }, target);
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

#[derive(Debug, Clone, Copy)]

pub(crate) struct BlockPredecessor {
    pred_ref: BlockRef,
    edge_idx: u32,
}

#[derive(Debug)]
pub(crate) struct Block {
    pub(crate) inst_refs: RefCell<Vec<InstRef>>,
    pub(crate) block_args: Vec<Type>,
    pub(crate) block_arg_uses: Vec<UseVec>,

    pub(crate) preds: Vec<BlockPredecessor>,
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
            Inst::Constant { .. } => false,
            Inst::Add { .. } => false,
            Inst::Sub { .. } => false,
            Inst::Mul { .. } => false,
            Inst::Div { .. } => false,
            Inst::Modulo { .. } => false,
            Inst::And { .. } => false,
            Inst::Or { .. } => false,
            Inst::Xor { .. } => false,
            Inst::Shl { .. } => false,
            Inst::Ashr { .. } => false,
            Inst::Lshr { .. } => false,
            Inst::Icmp { .. } => false,
            Inst::Fadd { .. } => false,
            Inst::Fsub { .. } => false,
            Inst::Fmul { .. } => false,
            Inst::Fdiv { .. } => false,
            Inst::Fcmp { .. } => false,
            Inst::IntToFp { .. } => false,
            Inst::FpToInt { .. } => false,
            Inst::Load { .. } => false,
            Inst::Store { .. } => true,
            Inst::StackAddr { .. } => false,
            Inst::Zext { .. } => false,
            Inst::Sext { .. } => false,
            Inst::Truncate { .. } => false,
            Inst::FpCast { .. } => false,
            Inst::PtrAdd { .. } => false,
            Inst::PtrToInt { .. } => false,
            Inst::IntToPtr { .. } => false,
            Inst::Select { .. } => false,
            Inst::BranchIf { .. } => true,
            Inst::Return { .. } => true,
            Inst::Jump { .. } => true,
            Inst::Call { .. } => true,
            Inst::CallIndirect { .. } => true,
            Inst::FuncAddr { .. } => false,
            Inst::DataAddr { .. } => false,
            Inst::Intrinsic { .. } => true,
        }
    }

    pub(crate) fn num_operand_kinds(&self) -> u32 {
        match self {
            Inst::Constant { val } => 0,
            Inst::Add { a, b } => 1,
            Inst::Sub { a, b } => 1,
            Inst::Mul { a, b } => 1,
            Inst::Div { a, b } => 1,
            Inst::Modulo { a, b } => 1,
            Inst::And { a, b } => 1,
            Inst::Or { a, b } => 1,
            Inst::Xor { a, b } => 1,
            Inst::Shl { a, b } => 1,
            Inst::Ashr { a, b } => 1,
            Inst::Lshr { a, b } => 1,
            Inst::Icmp { mode, a, b, signed } => 1,
            Inst::Fadd { a, b } => 1,
            Inst::Fsub { a, b } => 1,
            Inst::Fmul { a, b } => 1,
            Inst::Fdiv { a, b } => 1,
            Inst::Fcmp { mode, a, b } => 1,
            Inst::IntToFp { v } => 1,
            Inst::FpToInt { v } => 1,
            Inst::Load { addr } => 1,
            Inst::Store { addr, val } => 1,
            Inst::StackAddr { slot } => 1,
            Inst::Zext { v } => 1,
            Inst::Sext { v } => 1,
            Inst::Truncate { v } => 1,
            Inst::FpCast { v } => 1,
            Inst::PtrAdd { ptr, offset } => 1,
            Inst::PtrToInt { v } => 1,
            Inst::IntToPtr { v } => 1,
            Inst::Select { cond, x, y } => 1,
            Inst::BranchIf { cond, con, con_args, alt, alt_args } => 3,
            Inst::Return { values } => 1,
            Inst::Jump { target, arguments } => 1,
            Inst::Call { func, arguments } => 1,
            Inst::CallIndirect { callee_sig, func_ptr, arguments } => 2,
            Inst::FuncAddr { func } => 0,
            Inst::DataAddr { data } => 0,
            Inst::Intrinsic { intrinsic, arguments } => 1,
        }
    }

    pub(crate) fn num_operands_of_kind(
        &self, 
        value_vecs: &IndexSlice<ValueVecRef, [ValueVec]>, 
        kind: u32
    ) -> u32 {
        match self {
            Inst::Constant { val } => todo!(),
            Inst::Add { a, b } => 2,
            Inst::Sub { a, b } => 2,
            Inst::Mul { a, b } => 2,
            Inst::Div { a, b } => 2,
            Inst::Modulo { a, b } => 2,
            Inst::And { a, b } => 2,
            Inst::Or { a, b } => 2,
            Inst::Xor { a, b } => 2,
            Inst::Shl { a, b } => 2,
            Inst::Ashr { a, b } => 2,
            Inst::Lshr { a, b } => 2,
            Inst::Icmp { mode, a, b, signed } => 2,
            Inst::Fadd { a, b } => 2,
            Inst::Fsub { a, b } => 2,
            Inst::Fmul { a, b } => 2,
            Inst::Fdiv { a, b } => 2,
            Inst::Fcmp { mode, a, b } => 2,
            Inst::IntToFp { v } => 1,
            Inst::FpToInt { v } => 1,
            Inst::Load { addr } => 1,
            Inst::Store { addr, val } => 2,
            Inst::StackAddr { slot } => todo!(),
            Inst::Zext { v } => 1,
            Inst::Sext { v } => 1,
            Inst::Truncate { v } => 1,
            Inst::FpCast { v } => 1,
            Inst::PtrAdd { ptr, offset } => 2,
            Inst::PtrToInt { v } => 1,
            Inst::IntToPtr { v } => 1,
            Inst::Select { cond, x, y } => 3,
            Inst::BranchIf { cond, con, con_args, alt, alt_args } => {
                match kind {
                    0 => 1,
                    1 => value_vecs[*con_args].len() as u32,
                    2 => value_vecs[*alt_args].len() as u32,
                    _ => 0,
                }
            },
            Inst::Return { values } => todo!(),
            Inst::Jump { target, arguments } => todo!(),
            Inst::Call { func, arguments } => todo!(),
            Inst::CallIndirect { callee_sig, func_ptr, arguments } => todo!(),
            Inst::FuncAddr { func } => todo!(),
            Inst::DataAddr { data } => todo!(),
            Inst::Intrinsic { intrinsic, arguments } => todo!(),
        }
    }

    pub(crate) fn operand_coord_iter<'inst, 'vvec>(
        &'inst self, 
        value_vecs: &'vvec IndexSlice<ValueVecRef, [ValueVec]>
    ) -> impl Iterator<Item = OperandCoord> + use<'inst> {        
        struct OperandCoordIter<'inst> {
            inst: &'inst Inst,
            current_kind: u32,
            num_kinds: u32,
            current_idx: u32,
            num_idxs: SmallVec<[u32; 4]>,
        }

        impl<'inst> Iterator for OperandCoordIter<'inst> {
            type Item = OperandCoord;
        
            fn next(&mut self) -> Option<Self::Item> {
                if self.current_kind >= self.num_kinds {
                    return None;
                }

                let coord = OperandCoord {
                    kind: self.current_kind,
                    idx: self.current_idx,
                };

                self.current_idx += 1;
                if self.current_idx >= self.num_idxs[self.current_kind as usize] {
                    self.current_kind += 1;
                    self.current_idx = 0;
                }

                Some(coord)
            }
        }

        let num_kinds = self.num_operand_kinds();
        let num_idxs = (0..num_kinds)
            .map(|k| self.num_operands_of_kind(value_vecs, k)).collect();

        OperandCoordIter {
            inst: self,
            current_kind: 0,
            num_kinds,
            current_idx: 0,
            num_idxs,
        }
    }

    pub(crate) fn get_operand(
        &self, 
        value_vecs: &IndexSlice<ValueVecRef, [ValueVec]>, 
        coord: OperandCoord
    ) -> Value {
        macro_rules! operand_arm {
            // Nullary instruction - panics
            ($self:expr, $variant:path) => {
                match $self {
                    $variant { .. } => panic!("get_operand called on nullary instruction"),
                    _ => ()
                }
            };

            // Unary instruction
            ($self:expr, $variant:path, $x:ident, $coord:expr) => {
                match $self {
                    $variant { $x } if $coord.kind == 0 && $coord.idx == 0 => return *$x,
                    _ => ()
                }
            };

            // Binary instruction
            ($self:expr, $variant:path, $x:ident, $y:ident, $coord:expr) => {
                match $self {
                    $variant { $x, .. } if $coord.kind == 0 && $coord.idx == 0 => return *$x,
                    $variant { $y, .. } if $coord.kind == 0 && $coord.idx == 1 => return *$y,
                    _ => ()
                }
            };

            // Ternary instruction
            ($self:expr, $variant:path, $x:ident, $y:ident, $z:ident, $coord:expr) => {
                match $self {
                    $variant { $x, .. } if $coord.kind == 0 && $coord.idx == 0 => return *$x,
                    $variant { $y, .. } if $coord.kind == 0 && $coord.idx == 1 => return *$y,
                    $variant { $z, .. } if $coord.kind == 0 && $coord.idx == 2 => return *$z,
                    _ => ()
                }
            }
        }

        operand_arm!(self, Inst::Constant);
        operand_arm!(self, Inst::Add, a, b, coord);
        operand_arm!(self, Inst::Sub, a, b, coord);
        operand_arm!(self, Inst::Mul, a, b, coord);
        operand_arm!(self, Inst::Div, a, b, coord);
        operand_arm!(self, Inst::Modulo, a, b, coord);
        operand_arm!(self, Inst::And, a, b, coord);
        operand_arm!(self, Inst::Or, a, b, coord);
        operand_arm!(self, Inst::Xor, a, b, coord); 
        operand_arm!(self, Inst::Shl, a, b, coord); 
        operand_arm!(self, Inst::Ashr, a, b, coord);   
        operand_arm!(self, Inst::Lshr, a, b, coord);   
        operand_arm!(self, Inst::Icmp, a, b, coord);   
        operand_arm!(self, Inst::Fadd, a, b, coord);   
        operand_arm!(self, Inst::Fsub, a, b, coord);   
        operand_arm!(self, Inst::Fmul, a, b, coord);   
        operand_arm!(self, Inst::Fdiv, a, b, coord);
        operand_arm!(self, Inst::Fcmp, a, b, coord);
        operand_arm!(self, Inst::IntToFp, v, coord);
        operand_arm!(self, Inst::FpToInt, v, coord);
        operand_arm!(self, Inst::Load, addr, coord);
        operand_arm!(self, Inst::Store, addr, val, coord);
        operand_arm!(self, Inst::StackAddr);
        operand_arm!(self, Inst::Zext, v, coord);
        operand_arm!(self, Inst::Sext, v, coord);
        operand_arm!(self, Inst::Truncate, v, coord);
        operand_arm!(self, Inst::FpCast, v, coord);
        operand_arm!(self, Inst::PtrAdd, ptr, offset, coord);
        operand_arm!(self, Inst::PtrToInt, v, coord);
        operand_arm!(self, Inst::IntToFp, v, coord);
        operand_arm!(self, Inst::Select, cond, x, y, coord);
        operand_arm!(self, Inst::FuncAddr);
        operand_arm!(self, Inst::DataAddr);

        match self {
            Inst::BranchIf { cond, .. } if coord.kind == 0 && coord.idx == 0 => { return *cond },
            Inst::BranchIf { con_args, .. } 
                if coord.kind == 1 && coord.idx < value_vecs[*con_args].len() as u32 => { return value_vecs[*con_args][coord.idx as usize] },
            Inst::BranchIf { alt_args, .. } 
                if coord.kind == 2 && coord.idx < value_vecs[*alt_args].len() as u32 => { return value_vecs[*alt_args][coord.idx as usize] },
            Inst::Return { values } 
                if coord.kind == 0 && coord.idx < value_vecs[*values].len() as u32 => { return value_vecs[*values][coord.idx as usize] },
            Inst::Jump { target, arguments } 
                if coord.kind == 0 && coord.idx < value_vecs[*arguments].len() as u32 => { return value_vecs[*arguments][coord.idx as usize] },
            Inst::Call { func, arguments } 
                if coord.kind == 0 && coord.idx < value_vecs[*arguments].len() as u32 => { return value_vecs[*arguments][coord.idx as usize] },
            Inst::CallIndirect { callee_sig, func_ptr, arguments } 
                if coord.kind == 0 && coord.idx == 0 => { return *func_ptr },
            Inst::CallIndirect { callee_sig, func_ptr, arguments } 
                if coord.kind == 1 && coord.idx < value_vecs[*arguments].len() as u32 => { return value_vecs[*arguments][coord.idx as usize] },
            Inst::Intrinsic { intrinsic, arguments }
                if coord.kind == 0 && coord.idx < value_vecs[*arguments].len() as u32 => { return value_vecs[*arguments][coord.idx as usize]},
            _ => todo!()
        }

        panic!("get_operand failed (bad OperandCoord?)");
    }
    
    /// Returns info about a single CFG edge leaving this (terminator) instruction: its
    /// target block, the `ValueVecRef` of arguments passed along it, and the `OperandCoord`
    /// kind of those arguments.
    /// Only applies to: `jmp`, `brif` (guarded by assert). Panics for out-of-bounds edges
    pub(crate) fn edge(&self, edge_idx: u32) -> Edge {
        assert_matches!(self, Inst::BranchIf { .. } | Inst::Jump { .. });

        match self {
            Inst::BranchIf { cond: _, con, con_args, alt, alt_args } => {
                assert!(edge_idx < 2);
                match edge_idx {
                    0 => Edge { target: *con, args: *con_args, args_kind: 1 },
                    1 => Edge { target: *alt, args: *alt_args, args_kind: 2 },
                    _ => unreachable!()
                }
            }
            Inst::Jump { target, arguments } => {
                assert!(edge_idx == 0);
                match edge_idx {
                    0 => Edge { target: *target, args: *arguments, args_kind: 0 },
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
        }
    }
}

/// A CFG edge originating from a terminator instruction (`jmp`/`brif`).
#[derive(Debug, Clone, Copy)]
pub(crate) struct Edge {
    pub(crate) target: BlockRef,
    pub(crate) args: ValueVecRef,
    pub(crate) args_kind: u32,
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
        for (func, sig) in std::iter::zip(self.functions.as_ref(), self.signatures.as_ref()) {
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
