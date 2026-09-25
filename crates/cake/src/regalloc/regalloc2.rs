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
                let vreg = &func.vregs[vreg_ref];
                regalloc2::VReg::new(vreg_ref.get_inner(), vreg.class.into())
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
                    let vreg = &func.vregs[vreg_ref];
                    regalloc2::VReg::new(vreg_ref.get_inner(), vreg.class.into())
                });

                let start = self.branch_params_flat.len() as u32;
                self.branch_params_flat.extend(branch_param_vregs);
                let end = self.branch_params_flat.len() as u32;
                self.branch_params[bref][edge_idx] = (start, end);
            }
        }
    }

    fn extend_range<I: Idx, T>(
        idx: I,
        range: impl Iterator<Item = T>,
        range_vec: &mut IndexVec<I, (u32, u32)>,
        flat_vec: &mut Vec<T>
    ) {
        let start = range_vec.len() as u32;
        flat_vec.extend(range);
        let end = range_vec.len() as u32;
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

    fn inst_clobbers(&self, insn: regalloc2::Inst) -> regalloc2::PRegSet {
        todo!()
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

    regalloc2::MachineEnv {
        preferred_regs_by_class: [
            new_pregset(regalloc2::RegClass::Int, 16),
            new_pregset(regalloc2::RegClass::Float, 16),
            regalloc2::PRegSet::empty(),
        ],
        non_preferred_regs_by_class: [
            regalloc2::PRegSet::empty(),
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