use cake_util::{IndexSlice, IndexVec, index_vec, make_type_idx};
use regalloc2;

use crate::cir::{self, FunctionDefinition};

impl From<cir::BlockRef> for regalloc2::Block {
    fn from(value: cir::BlockRef) -> Self {
        Self(value.get_inner() as u32)
    }
}

impl From<regalloc2::Block> for cir::BlockRef {
    fn from(value: regalloc2::Block) -> Self {
        unsafe { cir::BlockRef::assume_valid(value.0) }
    }
}

impl cir::BlockRef {
    fn convert_slice(s: &[cir::BlockRef]) -> &[regalloc2::Block] {
        // not sure that this is completely sound, but the two are both just newtypes around u32
        assert_eq!(std::mem::size_of::<cir::BlockRef>(), std::mem::size_of::<regalloc2::Block>());
        assert_eq!(std::mem::align_of::<cir::BlockRef>(), std::mem::align_of::<regalloc2::Block>());
        unsafe { std::mem::transmute(s) }
    }
}

/// Constructs the necessary data structures for regalloc2 to perform register allocation.
struct RegAllocAdapter<'func> {
    func: &'func FunctionDefinition,
    num_vregs: usize,
    ctx: RegAllocAdapterContext,
}

/// Reusable allocations for `RegAllocAdapter`
struct RegAllocAdapterContext {
    insns: Vec<cir::InstRef>,
    block_insns: IndexVec<cir::BlockRef, regalloc2::InstRange>,
    
    block_succs_flat: Vec<cir::BlockRef>,
    block_succs: IndexVec<cir::BlockRef, (u32, u32)>,
    
    block_preds_flat: Vec<cir::BlockRef>,
    block_preds: IndexVec<cir::BlockRef, (u32, u32)>,

    block_params_flat: Vec<regalloc2::VReg>,
    block_params: IndexVec<cir::BlockRef, (u32, u32)>,

    branch_params_flat: Vec<regalloc2::VReg>,
    branch_params: IndexVec<cir::BlockRef, [(u32, u32); 4]>,
}

impl<'func> RegAllocAdapter<'func> {
    /// Helper for block_succs, block_preds, and block_params, which all follow the same indexing scheme
    fn index_by_block<'a, T>(
        block: regalloc2::Block,
        range_vec: &IndexSlice<cir::BlockRef, [(u32, u32)]>,
        flat_vec: &'a [T],
    ) -> &'a [T] {
        let block_ref: cir::BlockRef = block.into();
        let range = range_vec[block_ref];
        let (start, end) = (range.0 as usize, range.1 as usize);
        &flat_vec[start..end]
    }
}

impl RegAllocAdapterContext {
    fn new() -> Self {
        Self {
            insns: vec![],
            block_insns: index_vec![],
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
        let block_ref: cir::BlockRef = block.into();
        self.ctx.block_insns[block_ref]
    }

    fn block_succs(&self, block: regalloc2::Block) -> &[regalloc2::Block] {
        let succs = Self::index_by_block(
            block, 
            &self.ctx.block_succs, 
            &self.ctx.block_succs_flat
        );
        cir::BlockRef::convert_slice(succs)
    }

    fn block_preds(&self, block: regalloc2::Block) -> &[regalloc2::Block] {
        let preds = Self::index_by_block(
            block, 
            &self.ctx.block_preds, 
            &self.ctx.block_preds_flat
        );
        cir::BlockRef::convert_slice(preds)
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
        let inst = self.func.insts[inst_ref];
        matches!(inst, cir::Inst::Return { .. })
    }

    fn is_branch(&self, insn: regalloc2::Inst) -> bool {
        let inst_ref = self.ctx.insns[insn.0 as usize];
        let inst = self.func.insts[inst_ref];
        matches!(inst, cir::Inst::BranchIf { .. } | cir::Inst::Jump { .. })
    }

    fn branch_blockparams(
        &self, 
        block: regalloc2::Block, 
        insn: regalloc2::Inst, 
        succ_idx: usize
    ) -> &[regalloc2::VReg] {
        let block_ref: cir::BlockRef = block.into();
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
        todo!()
    }

    fn inst_clobbers(&self, insn: regalloc2::Inst) -> regalloc2::PRegSet {
        todo!()
    }

    fn num_vregs(&self) -> usize {
        self.num_vregs
    }

    fn spillslot_size(&self, regclass: regalloc2::RegClass) -> usize {
        todo!()
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