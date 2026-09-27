use cake_util::{Idx, IndexSlice, IndexVec, index_vec};
use regalloc2;
use smallvec::SmallVec;

use crate::{cir, mir};

impl From<mir::MachineBlockRef> for regalloc2::Block {
    fn from(value: mir::MachineBlockRef) -> Self {
        Self(value.get_inner() as u32)
    }
}

impl From<regalloc2::Block> for mir::MachineBlockRef {
    fn from(value: regalloc2::Block) -> Self {
        unsafe { mir::MachineBlockRef::assume_valid(value.0) }
    }
}

impl mir::MachineBlockRef {
    fn convert_slice(s: &[mir::MachineBlockRef]) -> &[regalloc2::Block] {
        // not sure that this is completely sound, but the two are both just newtypes around u32
        assert_eq!(std::mem::size_of::<mir::MachineBlockRef>(), std::mem::size_of::<regalloc2::Block>());
        assert_eq!(std::mem::align_of::<mir::MachineBlockRef>(), std::mem::align_of::<regalloc2::Block>());
        unsafe { std::mem::transmute(s) }
    }
}

impl From<mir::RegClass> for regalloc2::RegClass {
    fn from(value: mir::RegClass) -> Self {
        use mir::RegClass::*;
        use regalloc2::RegClass::*;
        match value {
            Gpr => Int,
            GprNoSp => Int,
            Sse => Float,
        }
    }
}

/// Encoding of physical registers
/// We place rsp and r12 at the front to be able to encode GprNoSp, since operand constraints can only force
/// the allocator to pick registers from a range of 0..n where n is a power of 2
impl From<mir::PhysReg> for regalloc2::PReg {
    fn from(value: mir::PhysReg) -> Self {
        use mir::PhysReg::*;
        use regalloc2::{PReg, RegClass::*};
        match value {
            rax => PReg::new(2, Int),
            rbx => PReg::new(3, Int),
            rcx => PReg::new(4, Int),
            rdx => PReg::new(5, Int),
            rsi => PReg::new(6, Int),
            rdi => PReg::new(7, Int),
            rbp => PReg::new(8, Int),
            rsp => PReg::new(0, Int),
            r8 => PReg::new(9, Int),
            r9 => PReg::new(10, Int),
            r10 => PReg::new(11, Int),
            r11 => PReg::new(12, Int),
            r12 => PReg::new(1, Int),
            r13 => PReg::new(13, Int),
            r14 => PReg::new(14, Int),
            r15 => PReg::new(15, Int),
            xmm0 => PReg::new(0, Float),
            xmm1 => PReg::new(1, Float),
            xmm2 => PReg::new(2, Float),
            xmm3 => PReg::new(3, Float),
            xmm4 => PReg::new(4, Float),
            xmm5 => PReg::new(5, Float),
            xmm6 => PReg::new(6, Float),
            xmm7 => PReg::new(7, Float),
            xmm8 => PReg::new(8, Float),
            xmm9 => PReg::new(9, Float),
            xmm10 => PReg::new(10, Float),
            xmm11 => PReg::new(11, Float),
            xmm12 => PReg::new(12, Float),
            xmm13 => PReg::new(13, Float),
            xmm14 => PReg::new(14, Float),
            xmm15 => PReg::new(15, Float),
        }
    }
}

impl From<regalloc2::PReg> for mir::PhysReg {
    fn from(value: regalloc2::PReg) -> Self {
        use regalloc2::RegClass::*;
        use mir::PhysReg::*;
        match (value.class(), value.hw_enc()) {
            (Int, 0) => rsp,
            (Int, 1) => r12,
            (Int, 2) => rax,
            (Int, 3) => rbx,
            (Int, 4) => rcx,
            (Int, 5) => rdx,
            (Int, 6) => rsi,
            (Int, 7) => rdi,
            (Int, 8) => rbp,
            (Int, 9) => r8,
            (Int, 10) => r9,
            (Int, 11) => r10,
            (Int, 12) => r11,
            (Int, 13) => r13,
            (Int, 14) => r14,
            (Int, 15) => r15,
            (Float, 0) => xmm0,
            (Float, 1) => xmm1,
            (Float, 2) => xmm2,
            (Float, 3) => xmm3,
            (Float, 4) => xmm4,
            (Float, 5) => xmm5,
            (Float, 6) => xmm6,
            (Float, 7) => xmm7,
            (Float, 8) => xmm8,
            (Float, 9) => xmm9,
            (Float, 10) => xmm10,
            (Float, 11) => xmm11,
            (Float, 12) => xmm12,
            (Float, 13) => xmm13,
            (Float, 14) => xmm14,
            (Float, 15) => xmm15,
            _ => unreachable!("not a preg")
        }
    }
}

impl mir::MachineFunctionDefinition {
    /// Produces a regalloc2 VReg from a VRegRef
    fn regalloc2_vreg(&self, vreg_ref: mir::VRegRef) -> regalloc2::VReg {
        let vreg = &self.vregs[vreg_ref];
        regalloc2::VReg::new(vreg_ref.get_inner(), vreg.class.into())
    }
}

impl mir::MachineInst {
    fn inst_operands(
        &self,
        func: &mir::MachineFunctionDefinition,
        operands: &mut Vec<regalloc2::Operand>
    ) {
        use mir::MachineInst::*;
        use mir::Reg;
        use regalloc2::{Operand, OperandConstraint, OperandKind, OperandPos, PReg};

        /// Create a "use" operand from a MIR Reg
        fn make_use(reg: Reg, func: &mir::MachineFunctionDefinition) -> regalloc2::Operand {
            let Reg::VReg(vreg_ref) = reg else {
                unreachable!("physical register operands should not be present before register allocation")
            };

            let class = func.vregs[vreg_ref].class;
            let constraint = match class {
                mir::RegClass::Gpr => OperandConstraint::Reg,
                mir::RegClass::GprNoSp => OperandConstraint::Limit(2),
                mir::RegClass::Sse => OperandConstraint::Reg,
            };

            Operand::new(
                regalloc2::VReg::new(vreg_ref.get_inner(), class.into()),
                constraint,
                OperandKind::Use,
                OperandPos::Early
            )
        }

        type OperandVec = [regalloc2::Operand; 4];
        fn make_mem_use(mem_operand: &mir::MemOperand, func: &mir::MachineFunctionDefinition) -> (usize, OperandVec) {
            let mut operand_buf = [regalloc2::Operand::from_bits(0); 4];
            let mut num_operands = 0;

            match mem_operand {
                mir::MemOperand::PcRelativeFn { target } => (),
                mir::MemOperand::PcRelativeData { target } => (),
                mir::MemOperand::Full { base, index, scale, disp } => {
                    if let mir::StackOrReg::Reg(base) = base {
                        operand_buf[num_operands] = make_use(*base, func);
                        num_operands += 1;
                    }

                    // ensure the NoSp constraint was applied correctly
                    let index_operand = make_use(*index, func);
                    assert!(index_operand.constraint() == OperandConstraint::Limit(2));
                    operand_buf[num_operands] = index_operand;
                    num_operands += 1;
                },
                mir::MemOperand::BasePlusDisp { base, disp } => {
                    if let mir::StackOrReg::Reg(base) = base {
                        operand_buf[num_operands] = make_use(*base, func);
                        num_operands += 1;
                    }
                },
                mir::MemOperand::AbsoluteDisp { disp } => (),
            }

            (num_operands, operand_buf)
        }

        fn make_def(
            reg: Reg, 
            func: &mir::MachineFunctionDefinition,
            reuse: Option<usize>
        ) -> regalloc2::Operand {
            let Reg::VReg(vreg_ref) = reg else {
                unreachable!("physical register operands should not be present before register allocation")
            };

            let class = func.vregs[vreg_ref].class;
            let vreg = regalloc2::VReg::new(vreg_ref.get_inner(), class.into());
            match reuse {
                Some(idx) => Operand::reg_reuse_def(
                    vreg, 
                    idx
                ),
                None => Operand::reg_def(vreg),
            }
        }

        fn make_params_uses(
            params: mir::VRegVecRef,
            func: &mir::MachineFunctionDefinition,
            operands: &mut Vec<regalloc2::Operand>
        ) {
            let param_vregs = &func.vreg_vecs[params];
            for &param_vreg in param_vregs {
                let param_operand = make_use(Reg::VReg(param_vreg), func);
                operands.push(param_operand);
            }
        }

        match self {
            JmpWithParams { target, params } => {
                // no operands: branch args are reported via branch_blockparams
            },
            JmpWithCondAndParams { 
                cond, 
                target, 
                fallthrough, 
                target_params, 
                fallthrough_params 
            } => {
                // no operands: branch args are reported via branch_blockparams
            },
            CallWithParams { target, params } => todo!(),
            CallIndirectWithParams { target, params } => todo!(),
            RetWithParams { params } => {
                let vregs = &func.vreg_vecs[*params];
                assert!(vregs.len() <= 1);

                if vregs.len() == 1 {
                    let vreg_ref = vregs[0];
                    let vreg = &func.vregs[vreg_ref];
                    assert!(vreg.class == mir::RegClass::Gpr);

                    let operand = Operand::reg_fixed_use(
                        regalloc2::VReg::new(
                            vreg_ref.get_inner(), 
                            regalloc2::RegClass::Int
                        ), 
                        mir::PhysReg::rax.into(),
                    );

                    operands.push(operand);
                }

                // make_params_uses(*params, func, operands);
            },
            Lea { dst, op2, width } => {
                let dst_operand = make_def(*dst, func, None);
                let (num_mem_operands, mem_operands) = make_mem_use(op2, func);
                operands.push(dst_operand);
                operands.extend_from_slice(&mem_operands[..num_mem_operands]);
            },
            AddRegToReg { dst, op1, op2, width } => todo!(),
            AddMemToReg { dst, op1, op2, width } => todo!(),
            AddRegToMem { op1, op2, width } => todo!(),
            AddImmToReg { dst, op1, op2, width } => todo!(),
            AddImmToMem { op1, op2, width } => todo!(),
            FAddRegToReg { dst, op1, op2, width } => todo!(),
            FAddMemToReg { dst, op1, op2, width } => todo!(),
            Load { dst, op2, width } => {
                let dst_operand = make_def(*dst, func, None);
                let (num_mem_operands, mem_operands) = make_mem_use(op2, func);
                operands.push(dst_operand);
                operands.extend_from_slice(&mem_operands[..num_mem_operands]);
            },
            LoadImm { dst, op2, width } => {
                let dst_operand = make_def(*dst, func, None);
                operands.push(dst_operand);
            },
            LoadFloat { dst, op2, width } => todo!(),
            StoreReg { op1, op2, width } => {
                let (num_mem_operands, mem_operands) = make_mem_use(op1, func);
                let val_operand = make_use(*op2, func);
                operands.extend_from_slice(&mem_operands[..num_mem_operands]);
                operands.push(val_operand);
            },
            StoreImm { op1, op2, width } => todo!(),
            StoreFloat { op1, op2, width } => todo!(),
            Mov { dst, op2, width } => todo!(),
            Cmov { dst, op2, cond, width } => todo!(),
            Xchg { op1, op2, width } => todo!(),
            ZeroExtend { dst, op2, dst_width, op2_width } => todo!(),
            SignExtend { dst, op2, dst_width, op2_width } => todo!(),
            Push { op1, width } => todo!(),
            PushImm { op1, width } => todo!(),
            Pop { dst, width } => todo!(),
            Jmp { target } => todo!(),
            Call { target } => todo!(),
            CallIndirect { target } => todo!(),
            Ret => todo!(),
            MulRegToReg { dst, op1, op2, width } => todo!(),
            MulMemToReg { dst, op1, op2, width } => todo!(),
            MulRegWithImm { dst, op1, op2, width } => todo!(),
            MulMemWithImm { dst, op1, op2, width } => todo!(),
            UDivByReg { dst_quo, dst_rem, op1, width } => todo!(),
            UDivByMem { dst_quo, dst_rem, op1, width } => todo!(),
            SDivByReg { dst_quo, dst_rem, op1, width } => todo!(),
            SDivByMem { dst_quo, dst_rem, op1, width } => todo!(),
            PrepareDiv { width } => todo!(),
            AndRegToReg { dst, op1, op2, width } => todo!(),
            OrRegToReg { dst, op1, op2, width } => todo!(),
            XorRegToReg { dst, op1, op2, width } => todo!(),
            NotReg { dst, op1, width } => todo!(),
            TestRegWithReg { op1, op2, width } => {
                let a_operand = make_use(*op1, func);
                let b_operand = make_use(*op2, func);
                operands.push(a_operand);
                operands.push(b_operand);
            },
            CmpRegWithReg { op1, op2, width } => {
                let a_operand = make_use(*op1, func);
                let b_operand = make_use(*op2, func);
                operands.push(a_operand);
                operands.push(b_operand);
            },
            SetReg { dst, cond } => {
                let dst_operand = make_def(*dst, func, None);
                operands.push(dst_operand);
            },
            JmpWithCond { cond, target, fallthrough } => todo!(),
        }
    }

    fn rewrite_inst_operands(
        &mut self,
        vreg_vecs: &mut IndexSlice<mir::VRegVecRef, [mir::VRegVec]>,
        mut allocs: &[regalloc2::Allocation]
    ) {
        fn alloc_mir_preg(alloc: regalloc2::Allocation) -> mir::PhysReg {
            alloc.as_reg().expect("spilled").into()
        }

        fn rewrite_mem_operand<'allocs>(
            mem_operand: &mut mir::MemOperand,
            mut allocs: &'allocs [regalloc2::Allocation]
        ) -> &'allocs [regalloc2::Allocation] {
            match mem_operand {
                mir::MemOperand::PcRelativeFn { target } => allocs,
                mir::MemOperand::PcRelativeData { target } => allocs,
                mir::MemOperand::Full { base, index, scale, disp } => {
                    match base {
                        mir::StackOrReg::Stack(stack_slot_ref) => (),
                        mir::StackOrReg::Reg(reg) => {
                            *reg = alloc_mir_preg(allocs[0]).into();
                            allocs = &allocs[1..]
                        },
                    }

                    *index = alloc_mir_preg(allocs[0]).into();
                    allocs = &allocs[1..];

                    allocs
                },
                mir::MemOperand::BasePlusDisp { base, disp } => {
                    match base {
                        mir::StackOrReg::Stack(stack_slot_ref) => (),
                        mir::StackOrReg::Reg(reg) => {
                            *reg = alloc_mir_preg(allocs[0]).into();
                            allocs = &allocs[1..];
                        },
                    }

                    allocs
                },
                mir::MemOperand::AbsoluteDisp { disp } => allocs
            }
        }

        match self {
            mir::MachineInst::JmpWithParams { target, params } => {
                *self = mir::MachineInst::Jmp { target: *target };
            },
            mir::MachineInst::JmpWithCondAndParams { cond, target, fallthrough, target_params, fallthrough_params } => {
                *self = mir::MachineInst::JmpWithCond { cond: *cond, target: *target, fallthrough: *fallthrough }
            },
            mir::MachineInst::CallWithParams { target, params } => todo!(),
            mir::MachineInst::CallIndirectWithParams { target, params } => todo!(),
            mir::MachineInst::RetWithParams { params } => {
                *self = mir::MachineInst::Ret;
            },
            mir::MachineInst::Lea { dst, op2, width } => {
                let dst_alloc = allocs[0].as_reg().expect("spilled");
                let phys_reg: mir::PhysReg = dst_alloc.into();
                *dst = mir::Reg::PReg(phys_reg);
            },
            mir::MachineInst::AddRegToReg { dst, op1, op2, width } => todo!(),
            mir::MachineInst::AddMemToReg { dst, op1, op2, width } => todo!(),
            mir::MachineInst::AddRegToMem { op1, op2, width } => todo!(),
            mir::MachineInst::AddImmToReg { dst, op1, op2, width } => todo!(),
            mir::MachineInst::AddImmToMem { op1, op2, width } => todo!(),
            mir::MachineInst::FAddRegToReg { dst, op1, op2, width } => todo!(),
            mir::MachineInst::FAddMemToReg { dst, op1, op2, width } => todo!(),
            mir::MachineInst::Load { dst, op2, width } => {
                *dst = alloc_mir_preg(allocs[0]).into();
                allocs = rewrite_mem_operand(op2, &allocs[1..]);
            },
            mir::MachineInst::LoadImm { dst, op2, width } => {
                *dst = alloc_mir_preg(allocs[0]).into();
            },
            mir::MachineInst::LoadFloat { dst, op2, width } => todo!(),
            mir::MachineInst::StoreReg { op1, op2, width } => {
                allocs = rewrite_mem_operand(op1, allocs);
                *op2 = alloc_mir_preg(allocs[0]).into();
            },
            mir::MachineInst::StoreImm { op1, op2, width } => todo!(),
            mir::MachineInst::StoreFloat { op1, op2, width } => todo!(),
            mir::MachineInst::Mov { dst, op2, width } => todo!(),
            mir::MachineInst::Cmov { dst, op2, cond, width } => todo!(),
            mir::MachineInst::Xchg { op1, op2, width } => todo!(),
            mir::MachineInst::ZeroExtend { dst, op2, dst_width, op2_width } => todo!(),
            mir::MachineInst::SignExtend { dst, op2, dst_width, op2_width } => todo!(),
            mir::MachineInst::Push { op1, width } => todo!(),
            mir::MachineInst::PushImm { op1, width } => todo!(),
            mir::MachineInst::Pop { dst, width } => todo!(),
            mir::MachineInst::Jmp { target } => todo!(),
            mir::MachineInst::Call { target } => todo!(),
            mir::MachineInst::CallIndirect { target } => todo!(),
            mir::MachineInst::Ret => todo!(),
            mir::MachineInst::MulRegToReg { dst, op1, op2, width } => todo!(),
            mir::MachineInst::MulMemToReg { dst, op1, op2, width } => todo!(),
            mir::MachineInst::MulRegWithImm { dst, op1, op2, width } => todo!(),
            mir::MachineInst::MulMemWithImm { dst, op1, op2, width } => todo!(),
            mir::MachineInst::UDivByReg { dst_quo, dst_rem, op1, width } => todo!(),
            mir::MachineInst::UDivByMem { dst_quo, dst_rem, op1, width } => todo!(),
            mir::MachineInst::SDivByReg { dst_quo, dst_rem, op1, width } => todo!(),
            mir::MachineInst::SDivByMem { dst_quo, dst_rem, op1, width } => todo!(),
            mir::MachineInst::PrepareDiv { width } => todo!(),
            mir::MachineInst::AndRegToReg { dst, op1, op2, width } => todo!(),
            mir::MachineInst::OrRegToReg { dst, op1, op2, width } => todo!(),
            mir::MachineInst::XorRegToReg { dst, op1, op2, width } => todo!(),
            mir::MachineInst::NotReg { dst, op1, width } => todo!(),
            mir::MachineInst::TestRegWithReg { op1, op2, width } => {
                *op1 = alloc_mir_preg(allocs[0]).into();
                *op2 = alloc_mir_preg(allocs[1]).into();
            },
            mir::MachineInst::CmpRegWithReg { op1, op2, width } => {
                *op1 = alloc_mir_preg(allocs[0]).into();
                *op2 = alloc_mir_preg(allocs[1]).into();
            },
            mir::MachineInst::SetReg { dst, cond } => {
                *dst = alloc_mir_preg(allocs[0]).into();
            },
            mir::MachineInst::JmpWithCond { cond, target, fallthrough } => todo!(),
        }
    }
}

/// Constructs the necessary data structures for regalloc2 to perform register allocation.
struct RegAllocAdapter<'func> {
    func: &'func mir::MachineFunctionDefinition,
    ctx: RegAllocAdapterContext,
}

/// Reusable allocations for `RegAllocAdapter`
struct RegAllocAdapterContext {
    insns: Vec<mir::MachineInstRef>,
    block_insns: IndexVec<mir::MachineBlockRef, regalloc2::InstRange>,

    inst_operands_flat: Vec<regalloc2::Operand>,
    inst_operands: IndexVec<mir::MachineInstRef, (u32, u32)>,
    
    block_succs_flat: Vec<mir::MachineBlockRef>,
    block_succs: IndexVec<mir::MachineBlockRef, (u32, u32)>,
    
    block_preds_flat: Vec<mir::MachineBlockRef>,
    block_preds: IndexVec<mir::MachineBlockRef, (u32, u32)>,

    block_params_flat: Vec<regalloc2::VReg>,
    block_params: IndexVec<mir::MachineBlockRef, (u32, u32)>,

    branch_params_flat: Vec<regalloc2::VReg>,
    branch_params: IndexVec<mir::MachineBlockRef, [(u32, u32); 4]>,
}

impl<'func> RegAllocAdapter<'func> {
    /// Helper for block_succs, block_preds, and block_params, 
    /// which all follow the same indexing scheme
    fn index_by_block<'a, T>(
        block: regalloc2::Block,
        range_vec: &IndexSlice<mir::MachineBlockRef, [(u32, u32)]>,
        flat_vec: &'a [T],
    ) -> &'a [T] {
        let block_ref: mir::MachineBlockRef = block.into();
        let range = range_vec[block_ref];
        let (start, end) = (range.0 as usize, range.1 as usize);
        &flat_vec[start..end]
    }

    fn index_by_inst<'a, T>(
        inst: regalloc2::Inst,
        insns: &[mir::MachineInstRef],
        range_vec: &IndexSlice<mir::MachineInstRef, [(u32, u32)]>,
        flat_vec: &'a [T]
    ) -> &'a [T] {
        let inst_ref: mir::MachineInstRef = insns[inst.0 as usize];
        let range = range_vec[inst_ref];
        let (start, end) = (range.0 as usize, range.1 as usize);
        &flat_vec[start..end]
    }
}

impl RegAllocAdapterContext {
    fn new() -> Self {
        Self {
            insns: vec![],
            block_insns: index_vec![],
            inst_operands_flat: vec![],
            inst_operands: index_vec![],
            block_succs_flat: vec![],
            block_succs: index_vec![],
            block_preds_flat: vec![],
            block_preds: index_vec![],
            block_params_flat: vec![],
            block_params: index_vec![],
            branch_params_flat: vec![],
            branch_params: index_vec![],
        }
    }

    fn clear(&mut self) {
        self.insns.clear();
        self.block_insns.clear();
        self.inst_operands_flat.clear();
        self.inst_operands.clear();
        self.block_succs_flat.clear();
        self.block_succs.clear();
        self.block_preds_flat.clear();
        self.block_preds.clear();
        self.block_params_flat.clear();
        self.block_params.clear();
        self.branch_params_flat.clear();
        self.branch_params.clear();
    }

    fn populate(&mut self, func: &mir::MachineFunctionDefinition) {
        for (bref, block) in mir::MachineBlockRef::enumerate2(&func.blocks) {
            let block_succs = block.successors(&func.insts);
            Self::extend_range(
                bref, 
                block_succs, 
                &mut self.block_succs, 
                &mut self.block_succs_flat
            );

            let block_preds = block.preds.iter().copied();
            Self::extend_range(
                bref,
                block_preds,
                &mut self.block_preds,
                &mut self.block_preds_flat
            );

            let block_param_vregs = block.block_params.iter().map(|&vreg_ref| {
                func.regalloc2_vreg(vreg_ref)
            });
            Self::extend_range(
                bref,
                block_param_vregs,
                &mut self.block_params,
                &mut self.block_params_flat,
            );

            let block_insts = &block.inst_refs;
            let start = self.insns.len();
            self.insns.extend_from_slice(block_insts.as_slice());
            let end = self.insns.len();
            let inst_range = regalloc2::InstRange::new(
                regalloc2::Inst(start as u32),
                regalloc2::Inst(end as u32)
            );
            self.block_insns.push(inst_range);

            let terminator_ref = block.terminator_ref();
            let terminator_inst = &func.insts[terminator_ref];
            for edge_idx in 0..terminator_inst.num_edges() {
                assert!(edge_idx < 4);
                let vreg_vec_ref = terminator_inst.edge_params(edge_idx as u32);
                let branch_param_vregs = func.vreg_vecs[vreg_vec_ref].iter().map(|&vreg_ref| {
                    func.regalloc2_vreg(vreg_ref)
                });

                let start = self.branch_params_flat.len() as u32;
                self.branch_params_flat.extend(branch_param_vregs);
                let end = self.branch_params_flat.len() as u32;
                self.branch_params.push(Default::default());
                self.branch_params[bref][edge_idx] = (start, end);
            }
        }
    
        for (iref, inst) in mir::MachineInstRef::enumerate2(&func.insts) {
            let start = self.inst_operands_flat.len() as u32;
            inst.inst_operands(func, &mut self.inst_operands_flat);
            let end = self.inst_operands_flat.len() as u32;
            self.inst_operands.push((start, end));
        }
    }

    fn extend_range<I: Idx, T>(
        idx: I,
        range: impl Iterator<Item = T>,
        range_vec: &mut IndexVec<I, (u32, u32)>,
        flat_vec: &mut Vec<T>
    ) {
        let start = flat_vec.len() as u32;
        flat_vec.extend(range);
        let end = flat_vec.len() as u32;
        if idx.into() >= range_vec.len() {
            range_vec.resize(idx.into() + 1, Default::default());
        }
        range_vec[idx] = (start, end);
    }
}

impl Default for RegAllocAdapterContext {
    fn default() -> Self {
        Self::new()
    }
}

impl<'func> regalloc2::Function for RegAllocAdapter<'func> {
    fn num_insts(&self) -> usize {
        self.ctx.insns.len()
    }

    fn num_blocks(&self) -> usize {
        self.func.blocks.len()
    }

    fn entry_block(&self) -> regalloc2::Block {
        let entry_block = self.func.entry_block();
        entry_block.into()
    }

    fn block_insns(&self, block: regalloc2::Block) -> regalloc2::InstRange {
        let block_ref: mir::MachineBlockRef = block.into();
        self.ctx.block_insns[block_ref]
    }

    fn block_succs(&self, block: regalloc2::Block) -> &[regalloc2::Block] {
        let succs = Self::index_by_block(
            block, 
            &self.ctx.block_succs, 
            &self.ctx.block_succs_flat
        );
        mir::MachineBlockRef::convert_slice(succs)
    }

    fn block_preds(&self, block: regalloc2::Block) -> &[regalloc2::Block] {
        let preds = Self::index_by_block(
            block, 
            &self.ctx.block_preds, 
            &self.ctx.block_preds_flat
        );
        mir::MachineBlockRef::convert_slice(preds)
    }

    fn block_params(&self, block: regalloc2::Block) -> &[regalloc2::VReg] {
        Self::index_by_block(
            block, 
            &self.ctx.block_params, 
            &self.ctx.block_params_flat
        )
    }

    fn is_ret(&self, insn: regalloc2::Inst) -> bool {
        let inst_ref = self.ctx.insns[insn.0 as usize];
        let inst = &self.func.insts[inst_ref];
        matches!(inst, mir::MachineInst::RetWithParams { .. })
    }

    fn is_branch(&self, insn: regalloc2::Inst) -> bool {
        let inst_ref = self.ctx.insns[insn.0 as usize];
        let inst = &self.func.insts[inst_ref];
        matches!(
            inst, 
            mir::MachineInst::JmpWithParams { .. } | mir::MachineInst::JmpWithCondAndParams { .. }
        )
    }

    fn branch_blockparams(
        &self, 
        block: regalloc2::Block, 
        insn: regalloc2::Inst, 
        succ_idx: usize
    ) -> &[regalloc2::VReg] {
        let block_ref: mir::MachineBlockRef = block.into();
        let block_terminator = self.func.blocks[block_ref].terminator_ref();
        let inst_ref = self.ctx.insns[insn.0 as usize];
        if block_terminator != inst_ref {
            return &[];
        }

        let branch_param_ranges = self.ctx.branch_params[block_ref];
        let branch_param_range = branch_param_ranges[succ_idx];
        let (start, end) = (branch_param_range.0 as usize, branch_param_range.1 as usize);
        &self.ctx.branch_params_flat[start..end]
    }

    fn inst_operands(&self, insn: regalloc2::Inst) -> &[regalloc2::Operand] {
        Self::index_by_inst(
            insn, 
            &self.ctx.insns, 
            &self.ctx.inst_operands, 
            &self.ctx.inst_operands_flat
        )
    }

    // TODO: we should probably model EFLAGS as a clobbered reg?
    fn inst_clobbers(&self, insn: regalloc2::Inst) -> regalloc2::PRegSet {
        regalloc2::PRegSet::empty()
    }

    fn num_vregs(&self) -> usize {
        self.func.vregs.len()
    }

    // idk what this function is meant for...
    fn spillslot_size(&self, regclass: regalloc2::RegClass) -> usize {
        1
    }
}

fn amd64_machine_env() -> regalloc2::MachineEnv {
    const fn new_pregset(class: regalloc2::RegClass, max_preg: u8) -> regalloc2::PRegSet {
        let mut set = regalloc2::PRegSet::empty();
        let mut current_preg = 0;
        while current_preg < max_preg {
            set.add(regalloc2::PReg::new(current_preg as usize, class));
            current_preg += 1;
        }

        set
    }

    let mut gprs = new_pregset(regalloc2::RegClass::Int, 16);
    
    // by default, omit rsp from register allocation, since it needs to be kept around for stack
    // manipulation
    gprs.remove(mir::PhysReg::rsp.into());

    let caller_saved_regs = {
        use mir::PhysReg::*;
        [rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11]
    };

    let caller_saved_pregset = {
        let mut set = regalloc2::PRegSet::empty();
        for preg in caller_saved_regs {
            set.remove(preg.into());
        }

        set
    };
    let mut callee_saved_pregset = caller_saved_pregset.invert();
    callee_saved_pregset.intersect_from(gprs);

    regalloc2::MachineEnv {
        preferred_regs_by_class: [
            caller_saved_pregset,
            new_pregset(regalloc2::RegClass::Float, 16),
            regalloc2::PRegSet::empty(),
        ],
        non_preferred_regs_by_class: [
            callee_saved_pregset,
            regalloc2::PRegSet::empty(),
            regalloc2::PRegSet::empty()
        ],
        scratch_by_class: [
            None, 
            None, 
            None
        ],
        fixed_stack_slots: Vec::new(),
    }
}

fn finalize_regalloc(
    func: &mut mir::MachineFunctionDefinition,
    ctx: &RegAllocAdapterContext,
    result: regalloc2::Output
) {
    if result.edits.len() > 0 || result.num_spillslots > 0 {
        todo!();
    }


    for (insn, &offset) in result.inst_alloc_offsets.iter().enumerate() {
        let inst_ref = ctx.insns[insn];
        let inst_mut = &mut func.insts[inst_ref];
        let inst_operand_range = ctx.inst_operands[inst_ref];
        let num_operands = inst_operand_range.1 - inst_operand_range.0;
        
        inst_mut.rewrite_inst_operands(
            &mut func.vreg_vecs, 
            &result.allocs[offset as usize..][..num_operands as usize]
        );
    }
}

#[cfg(test)]
pub(crate) mod test {
    use regalloc2::RegallocOptions;

    use crate::{mir::{MachineModule, cir2mir::InstructionSelector}, regalloc::regalloc2::{RegAllocAdapter, RegAllocAdapterContext, amd64_machine_env, finalize_regalloc}};

    pub(crate) fn conditional_module() -> MachineModule {
        use crate::cir::ast2cir::test::conditional_module;
        let mut module = conditional_module();
        let func = module.functions.iter_mut().next().unwrap().definition.as_mut().unwrap();
        func.blocks.pop();
        func.block_uses.pop();
        let mut isel = InstructionSelector::new(&module);
        isel.select_module();
        let mut mir_mod = isel.finish();

        print!("{mir_mod}");

        let mut regalloc_ctx = RegAllocAdapterContext::new();
        let func = mir_mod.functions.iter_mut().next().unwrap().definition.as_mut().unwrap();
        regalloc_ctx.populate(func);
        let ra2_adapter = RegAllocAdapter {
            func,
            ctx: regalloc_ctx,
        };

        let machine_env = amd64_machine_env();

        let result = regalloc2::run(&ra2_adapter, &machine_env, &RegallocOptions::default());
        let result = result.expect("register alloc failed");
        dbg!(&result);

        finalize_regalloc(func, &ra2_adapter.ctx, result);

        print!("{mir_mod}");
        mir_mod
    }

    #[test]
    fn test_conditional() {
        conditional_module();
    }
}