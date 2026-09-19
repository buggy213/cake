//! Instruction selection to lower CIR -> MIR
//! 
//! Operates directly on linear IR the whole way through. Instruction selection is 
//! done by matching "tiles" against def-use trees in CIR in a greedy manner (maximal munch).
//! We use the number of uses of an SSA value to determine whether it can be tiled over by a pattern.
//! Concretely, in a use-def "tree" of CIR of the form
//!          %4 = add %1 %2
//!         /        \
//!        %1      %2 = load %3
//!                   |
//!                  %3
//! Assuming that:
//! - we are matching %4
//! - %2 does not have any other uses
//! Then, it would be safe to lower this to AddMemToReg, since nothing else needs %2, 
//! so it's ok to fold the memory load in.
//!
//! Instruction selection is performed globally by iterating over basic blocks in post-order
//! and selecting uses before defs in a dataflow-like fashion. This is similar to
//! LLVM's GlobalISel.

use cake_util::{IndexSlice, IndexVec, index_vec};
use rustc_hash::FxHashMap;
use smallvec::{SmallVec, smallvec};
use crate::{
    cir::{self, BlockRef, Constant, Data, DataContents, Function, FunctionDefinition, InstRef, Value, post_order}, mir::{self, GprOperandWidth, ImmediateOperand, MachineBlock, MachineBlockRef, MachineFunction, MachineFunctionDefinition, MachineFunctionRef, MachineInst, MachineInstOperandCoord, MachineInstRef, MachineModule, MemOperand, PhysReg, Reg, RegClass, SseOperandWidth, VRegDef, VRegDefCoord, VRegRef, VRegUse, VirtualReg, phys_regs}
};

// Whether a CIR instruction has already been selected
enum SelectionStatus {
    // It hasn't been selected yet (it might be the leaf node of some existing tiles)
    NotSelectedYet,
    
    // It has already been selected as an inner node or root of some tile
    Selected
}

/// Handles instruction selection
struct InstructionSelector<'cir_mod> {
    cir_mod: &'cir_mod cir::Module,

    mir_mod: mir::MachineModule,
    
    // while selecting instructions, the "inputs" to a rule may correspond to 
    // the outputs of instructions which have not been selected yet. we use a helper
    // type to manage and fill in the insts defining virtual registers
    vregs: IndexVec<VRegRef, InstSelVReg>,
    insts: IndexVec<MachineInstRef, MachineInst>,

    // marks which CIR insts have been covered by instruction selection already
    used_insts: IndexVec<cir::InstRef, bool>,

    // TODO: it might be more efficient to mirror the organization of values in CIR 
    // rather than using a hashmap
    vreg_by_value: FxHashMap<Value, VRegRef>,

}

/// See note for InstructionSelector::vregs
enum InstSelVReg {
    Undefined {
        class: RegClass,
        uses: SmallVec<[VRegUse; 3]>
    },
    Defined(VirtualReg)
}

impl InstSelVReg {
    fn add_use(&mut self, use_: VRegUse) {
        match self {
            InstSelVReg::Undefined { class, uses } => 
                uses.push(use_),
            InstSelVReg::Defined(virtual_reg) =>
                virtual_reg.uses.push(use_)
        }
    }

    fn as_defined_vreg(self) -> VirtualReg {
        match self {
            InstSelVReg::Undefined { .. } => panic!("vreg is not defined"),
            InstSelVReg::Defined(virtual_reg) => virtual_reg
        }
    }

    fn define(&mut self, def: VRegDef) {
        match self {
            InstSelVReg::Undefined { class, uses } => {
                let defined_vreg = VirtualReg {
                    class: *class,
                    def,
                    uses: std::mem::take(uses),
                };

                *self = InstSelVReg::Defined(defined_vreg);
            }
            InstSelVReg::Defined(_) => panic!("vreg is already defined")
        }
    }
}

impl cir::Type {
    fn to_gpr_width(self) -> GprOperandWidth {
        match self {
            cir::Type::i8 => GprOperandWidth::Byte,
            cir::Type::i16 => GprOperandWidth::Word,
            cir::Type::i32 => GprOperandWidth::Dword,
            cir::Type::i64 => GprOperandWidth::Qword,
            cir::Type::ptr => GprOperandWidth::Qword,
            cir::Type::f32 => panic!("CIR f32 does not correspond to any GPR operand width"),
            cir::Type::f64 => panic!("CIR f64 does not correspond to any GPR operand width"),
        }
    }

    fn to_sse_width(self) -> SseOperandWidth {
        match self {
            cir::Type::f32 => SseOperandWidth::Single,
            cir::Type::f64 => SseOperandWidth::Double,
            _ => panic!("CIR integral types do not correspond to SSE operand widths")
        }
    }

    fn to_register_class(self) -> RegClass {
        if self.is_fp() {
            RegClass::Sse
        }
        else {
            RegClass::Gpr
        }
    }
}

impl MachineFunctionRef {
    /// Cast a CIR FuncRef into a MachineFunctionRef
    /// This is safe because the two are in 1-1 correspondence with eachother by construction
    fn from_func_ref(fref: cir::FuncRef) -> Self {
        Self(fref.get_inner() as u32)
    }
}

impl MachineBlockRef {
    /// Cast a CIR BlockRef into a MachineBlockRef
    /// This is safe because the two are in 1-1 correspondence with eachother by construction
    fn from_block_ref(bref: cir::BlockRef) -> Self {
        Self(bref.get_inner() as u32)
    }
}

impl<'cir_mod> InstructionSelector<'cir_mod> {
    fn new(cir_mod: &'cir_mod cir::Module) -> InstructionSelector<'cir_mod> {
        // prepare MIR module for instruction selection by copying over functions and basic blocks
        // but leaving them unpopulated
        let mut mir_mod = MachineModule {
            functions: index_vec![],
            signatures: cir_mod.signatures.clone(),
            data: cir_mod.data.clone(),
        };

        for func in cir_mod.functions() {
            let mir_func_def = if let Some(func_def) = &func.definition {
                MachineFunctionDefinition {
                    insts: index_vec![],
                    vregs: index_vec![],
                    blocks: index_vec![MachineBlock::new(); func_def.blocks.len()],
                }.into()
            } else { 
                None 
            };

            let mir_func = MachineFunction {
                name: func.name.clone(),
                definition: mir_func_def
            };

            mir_mod.functions.push(mir_func);
        }

        Self {
            cir_mod,
            mir_mod,
            vregs: index_vec![],
            insts: index_vec![],
            used_insts: index_vec![],
            vreg_by_value: FxHashMap::default()
        }
    }

    fn finish(self) -> mir::MachineModule {
        self.mir_mod
    }

    fn select_add() {
        let x: Value = todo!();
        let y: Value = todo!();
    }

    // Allocates a new virtual register and returns its index
    fn allocate_vreg(&mut self, class: RegClass) -> VRegRef {
        let undefined_vreg = InstSelVReg::Undefined { class, uses: smallvec![] };
        self.vregs.push(undefined_vreg)
    }

    // Allocates an operand if it has not already been allocated a slot
    // If it has, then just return the already-allocated operand
    fn operand_vreg(&mut self, class: RegClass, val: Value) -> VRegRef {
        use std::collections::hash_map::Entry;
        let entry = self.vreg_by_value.entry(val);
        match entry {
            Entry::Occupied(occupied_entry) => {
                *occupied_entry.get()
            },
            Entry::Vacant(vacant_entry) => {
                let undefined_vreg = InstSelVReg::Undefined { class, uses: smallvec![] };
                let undefined_vreg_ref = self.vregs.push(undefined_vreg);
                *vacant_entry.insert(undefined_vreg_ref)
            },
        }
    }

    // Uses an operand: adds the val->vreg mapping and adds a use to that vreg
    fn use_operand(
        &mut self, 
        val: Value, 
        vreg: VRegRef, 
        minst_ref: MachineInstRef, 
        coord: MachineInstOperandCoord
    ) {
        self.vreg_by_value.insert(val, vreg);
        let use_ = VRegUse {
            inst: minst_ref,
            coord,
        };
        self.vregs[vreg].add_use(use_);
    }

    fn define_vreg(&mut self, def: VRegDef, vreg: VRegRef) {
        self.vregs[vreg].define(def)
    }

    // When selecting an instruction, we check multiple patterns (in decreasing order of complexity, hence maximal munch)
    // until one matches. Once it is matched, we need to allocate MIR virtual registers, record the mapping of input CIR operands
    // of the rule to the appropriate MIR virtual regs, look up the mapping CIR outputs of the rule, and finally emit
    // the MIR instruction(s) that make up the rule. 
    fn select_inst(
        &mut self, 
        selected_insts: &mut Vec<MachineInstRef>,
        function: &FunctionDefinition, 
        inst_ref: InstRef
    ) {
        let inst = &function.insts[inst_ref];

        use crate::cir::Inst;

        use mir::MachineInstOperandCoord as OpCoord;
        use mir::VRegDefCoord as DefCoord;
        
        match inst {
            Inst::Constant { val } if val.ty().is_fp() => {
                let output_value = Value::Inst(inst_ref);
                let Some(&output_vreg) = self.vreg_by_value.get(&output_value) else {
                    return;
                };
                
                let (data_contents, width): (Box<[u8]>, SseOperandWidth) = match val {
                    Constant::f32(f) => 
                        (Box::from(f.to_le_bytes().as_slice()), SseOperandWidth::Single),
                    Constant::f64(d) => 
                        (Box::from(d.to_le_bytes().as_slice()), SseOperandWidth::Double),
                    _ => unreachable!()
                };

                let data = Data { 
                    name: None, 
                    read_only: true, 
                    contents: DataContents::Defined(data_contents)
                };

                let data_ref = self.mir_mod.add_data(data);

                let minst = MachineInst::LoadFloat { 
                    dst: Reg::VReg(output_vreg),
                    op2: MemOperand::PcRelativeData { target: data_ref }, 
                    width 
                };

                let minst_ref = self.insts.push(minst);

                self.define_vreg(VRegDef::Inst(minst_ref, DefCoord(0)), output_vreg);
                
                selected_insts.push(minst_ref);
            },
            Inst::Constant { val } => {
                let output_value = Value::Inst(inst_ref);
                let Some(&output_vreg) = self.vreg_by_value.get(&output_value) else {
                    return
                };

                let imm = ImmediateOperand::try_from(*val)
                    .expect("fp should be handled above");

                let width = match val {
                    Constant::i8(_) => GprOperandWidth::Byte,
                    Constant::i16(_) => GprOperandWidth::Word,
                    Constant::i32(_) => GprOperandWidth::Dword,
                    Constant::i64(_) => GprOperandWidth::Qword,
                    Constant::f32(_) | Constant::f64(_) => unreachable!("fp handled above"),
                };

                let minst = MachineInst::LoadImm {
                    dst: Reg::VReg(output_vreg),
                    op2: imm,
                    width
                };

                let minst_ref = self.insts.push(minst);

                self.define_vreg(VRegDef::Inst(minst_ref, DefCoord(0)), output_vreg);

                selected_insts.push(minst_ref);
            }
            Inst::Add { a, b } => {
                let output_value = Value::Inst(inst_ref);
                let Some(&output_vreg) = self.vreg_by_value.get(&output_value) else {
                    return
                };

                let a_vreg = self.operand_vreg(RegClass::Gpr, *a);
                let b_vreg = self.operand_vreg(RegClass::Gpr, *b);

                let width = function.type_of_value(output_value).to_gpr_width();
                let minst = MachineInst::AddRegToReg { 
                    dst: Reg::VReg(output_vreg), 
                    op1: Reg::VReg(a_vreg), 
                    op2: Reg::VReg(b_vreg), 
                    width
                };

                let minst_ref = self.insts.push(minst);

                self.use_operand(*a, a_vreg, minst_ref, OpCoord::direct(0));
                self.use_operand(*b, b_vreg, minst_ref, OpCoord::direct(1));
                self.define_vreg(VRegDef::Inst(minst_ref, DefCoord(0)), output_vreg);

                selected_insts.push(minst_ref);
            },
            Inst::Sub { a, b } => todo!(),
            Inst::Mul { a, b } => todo!(),
            Inst::Div { a, b } => todo!(),
            Inst::Modulo { a, b } => todo!(),
            Inst::And { a, b } => todo!(),
            Inst::Or { a, b } => todo!(),
            Inst::Xor { a, b } => todo!(),
            Inst::Shl { a, b } => todo!(),
            Inst::Ashr { a, b } => todo!(),
            Inst::Lshr { a, b } => todo!(),
            Inst::Icmp { mode, a, b, signed } => todo!(),
            Inst::Fadd { a, b } => todo!(),
            Inst::Fsub { a, b } => todo!(),
            Inst::Fmul { a, b } => todo!(),
            Inst::Fdiv { a, b } => todo!(),
            Inst::Fcmp { mode, a, b } => todo!(),
            Inst::IntToFp { v } => todo!(),
            Inst::FpToInt { v } => todo!(),
            Inst::Load { addr } => {
                let output_value = Value::Inst(inst_ref);
                let Some(&output_vreg) = self.vreg_by_value.get(&output_value) else {
                    return
                };

                let addr_vreg = self.operand_vreg(RegClass::Gpr, *addr);

                let mem = MemOperand::BasePlusDisp { 
                    base: Reg::VReg(addr_vreg), 
                    disp: mir::MemOperandDisplacement::Zero 
                };

                let width = function.type_of_value(output_value).to_gpr_width();
                let minst = MachineInst::Load { 
                    dst: Reg::VReg(output_vreg), 
                    op2: mem,
                    width
                };

                let minst_ref = self.insts.push(minst);

                self.use_operand(*addr, addr_vreg, minst_ref, OpCoord::direct(0));
                self.define_vreg(VRegDef::Inst(minst_ref, DefCoord(0)), output_vreg);

                selected_insts.push(minst_ref);
            },
            Inst::Store { addr, val } => {
                let addr_vreg = self.operand_vreg(RegClass::Gpr, *addr);
                let val_vreg = self.operand_vreg(RegClass::Gpr, *val);

                let mem = MemOperand::BasePlusDisp { 
                    base: Reg::VReg(addr_vreg),
                    disp: mir::MemOperandDisplacement::Zero
                };

                let width = function.type_of_value(*val).to_gpr_width();
                let minst = MachineInst::StoreReg { 
                    op1: mem, 
                    op2: Reg::VReg(val_vreg),
                    width
                };

                let minst_ref = self.insts.push(minst);

                self.use_operand(*addr, addr_vreg, minst_ref, OpCoord::direct(0));
                self.use_operand(*val, val_vreg, minst_ref, OpCoord::direct(1));

                selected_insts.push(minst_ref);
            }
            Inst::StackAddr { slot } => todo!(),
            Inst::Zext { v } => todo!(),
            Inst::Sext { v } => todo!(),
            Inst::Truncate { v } => todo!(),
            Inst::FpCast { v } => todo!(),
            Inst::PtrAdd { ptr, offset } => todo!(),
            Inst::PtrToInt { v } => {
                let output_value = Value::Inst(inst_ref);
                let Some(&output_vreg) = self.vreg_by_value.get(&output_value) else {
                    return
                };
                
                // we need to "redirect" the output_vreg which has been allocated for this inst
                // into v_vreg since PtrToInt is really a no-op

                // the vreg that was allocated for this inst still gets defined. even though it
                // is garbage, it is unreferenced, so it should be ok
            },
            Inst::IntToPtr { v } => todo!(),
            Inst::Select { cond, x, y } => {
                todo!()
            },
            Inst::BranchIf { cond, con, con_args, alt, alt_args } => todo!(),
            Inst::Return { values } => {
                // TODO: ABI handling code should probably be separated out somehow
                match function.value_vecs[*values].len() {
                    0 => {
                        let minst = MachineInst::Ret;
                        let minst_ref = self.insts.push(minst);
                        selected_insts.push(minst_ref);
                    },
                    1 => {
                        let retval = function.value_vecs[*values][0];
                        let retval_ty = function.type_of_value(retval);
                        
                        assert!(!retval_ty.is_fp(), "fp not supported yet");
                        let retval_vreg = self.operand_vreg(RegClass::Gpr, retval);

                        let mov_to_rax = MachineInst::Mov { 
                            dst: phys_regs::rax, 
                            op2: Reg::VReg(retval_vreg),
                            width: retval_ty.to_gpr_width() 
                        };

                        let mov_to_rax_ref = self.insts.push(mov_to_rax);

                        let minst = MachineInst::Ret;
                        let minst_ref = self.insts.push(minst);

                        self.use_operand(retval, retval_vreg, mov_to_rax_ref, MachineInstOperandCoord::direct(0));

                        selected_insts.push(mov_to_rax_ref);
                        selected_insts.push(minst_ref);
                    },
                    _ => todo!("multiple returns")
                }
            },
            Inst::Jump { target, arguments } => todo!(),
            Inst::Call { func, arguments } => todo!(),
            Inst::CallIndirect { callee_sig, func_ptr, arguments } => todo!(),
            Inst::FuncAddr { func } => todo!(),
            Inst::DataAddr { data } => todo!(),
            Inst::Intrinsic { intrinsic, arguments } => todo!(),
        }
    }

    fn select_block(&mut self, function: &FunctionDefinition, block_ref: BlockRef) -> Vec<MachineInstRef> {
        let block = &function.blocks[block_ref];
        
        // For efficiency reasons, we build up the block in reverse, then flip it at the end
        let mut mblock_irefs: Vec<MachineInstRef> = Vec::with_capacity(block.inst_refs.borrow().len());
        let mut minst_irefs: Vec<MachineInstRef> = Vec::with_capacity(8);
        for &iref in block.inst_refs.borrow().iter().rev() {
            self.select_inst(&mut minst_irefs, function, iref);
            mblock_irefs.extend(minst_irefs.iter().rev());
            minst_irefs.clear();
        }

        mblock_irefs.reverse();
        mblock_irefs
    }

    fn select_function(&mut self, func_ref: cir::FuncRef) {
        let mfunc_ref = MachineFunctionRef::from_func_ref(func_ref);
        let Some(func) = &self.cir_mod.functions[func_ref].definition else {
            return
        };

        // small helper to circumvent borrowck
        fn get_mfunc_def<'isel>(mir: &'isel mut MachineModule, mfunc_ref: MachineFunctionRef) 
            -> &'isel mut MachineFunctionDefinition {
            mir.functions[mfunc_ref].definition.as_mut().unwrap()
        }

        let post_order_traversal = post_order::post_order(func);
        for bref in post_order_traversal {
            let mblock_irefs = self.select_block(func, bref);
            let mfunc = get_mfunc_def(&mut self.mir_mod, mfunc_ref);
            let mbref = MachineBlockRef::from_block_ref(bref);
            mfunc.blocks[mbref].irefs = mblock_irefs;
        }

        let mfunc = get_mfunc_def(&mut self.mir_mod, mfunc_ref);
        mfunc.insts = std::mem::take(&mut self.insts);
        let vregs = std::mem::take(&mut self.vregs);
        mfunc.vregs = vregs.into_iter()
            .map(|v| v.as_defined_vreg())
            .collect();

    }

    /// The main entry point into instruction selection
    pub(crate) fn select_module(&mut self) {
        for func_ref in cir::FuncRef::iter(&self.cir_mod.functions) {
            self.select_function(func_ref);
        }
    }

}

#[cfg(test)]
mod test {
    use crate::mir::cir2mir::InstructionSelector;

    #[test]
    fn test_basic() {
        use crate::cir::ast2cir::test::basic_module;
        let module = basic_module();
        println!("{module}");
        let mut isel = InstructionSelector::new(&module);
        isel.select_module();
        let mir_mod = isel.finish();

        dbg!(mir_mod);
    }
}