use regalloc2;

use crate::cir;

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

impl From<cir::InstRef> for regalloc2::Inst {
    fn from(value: cir::InstRef) -> Self {
        Self(value.get_inner() as u32)
    }
}

impl From<regalloc2::Inst> for cir::InstRef {
    fn from(value: regalloc2::Inst) -> Self {
        unsafe { cir::InstRef::assume_valid(value.0) }
    }
}

/// Constructs the necessary data structures for regalloc2
struct RegAllocAdapter {

}

/// Reusable allocations for `RegAllocAdapter`
struct RegAllocAdapterContext {
    
}

impl regalloc2::Function for cir::FunctionDefinition {
    fn num_insts(&self) -> usize {
        self.insts.len()
    }

    fn num_blocks(&self) -> usize {
        self.blocks.len()
    }

    fn entry_block(&self) -> regalloc2::Block {
        let entry_block = self.entry_block();
        regalloc2::Block(entry_block.get_inner() as u32)
    }

    fn block_insns(&self, block: regalloc2::Block) -> regalloc2::InstRange {
        todo!("need to compact")
    }

    fn block_succs(&self, block: regalloc2::Block) -> &[regalloc2::Block] {
        todo!("need to make it a slice?")
    }

    fn block_preds(&self, block: regalloc2::Block) -> &[regalloc2::Block] {
        todo!("need to make it a slice?")
    }

    fn block_params(&self, block: regalloc2::Block) -> &[regalloc2::VReg] {
        todo!()
    }

    fn is_ret(&self, insn: regalloc2::Inst) -> bool {
        let inst_ref: cir::InstRef = insn.into();
        let inst = self.insts[inst_ref];
        matches!(inst, cir::Inst::Return { .. })
    }

    fn is_branch(&self, insn: regalloc2::Inst) -> bool {
        let inst_ref: cir::InstRef = insn.into();
        let inst = self.insts[inst_ref];
        matches!(inst, cir::Inst::BranchIf { .. } | cir::Inst::Jump { .. })
    }

    fn branch_blockparams(&self, block: regalloc2::Block, insn: regalloc2::Inst, succ_idx: usize) -> &[regalloc2::VReg] {
        todo!()
    }

    fn inst_operands(&self, insn: regalloc2::Inst) -> &[regalloc2::Operand] {
        todo!()
    }

    fn inst_clobbers(&self, insn: regalloc2::Inst) -> regalloc2::PRegSet {
        todo!()
    }

    fn num_vregs(&self) -> usize {
        todo!()
    }

    fn spillslot_size(&self, regclass: regalloc2::RegClass) -> usize {
        todo!()
    }
}