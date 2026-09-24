//! CIR Compaction Pass
//! 
//! Over time, the CIR data structures can become fragmented, as insertions and 
//! removals of instructions and basic blocks piles up. This pass exists to defragment 
//! the arenas holding them. 
//! 
//! This pass also sorts instructions living in the same basic block to live contiguously, as
//! well as sorting the basic blocks themselves into a good order (reverse post-order) for
//! subsequent traversals

use cake_util::IndexVec;

use crate::cir::{BlockRef, FuncRef, Module};

struct CompactionState {
    func_used: IndexVec<FuncRef, bool>,
    func_map: IndexVec<FuncRef, FuncRef>,

    block_used: IndexVec<BlockRef, bool>,
    block_map: IndexVec<BlockRef, BlockRef>,
}

pub(crate) fn compact_module(module: &mut Module) {
    
}

fn compact_functions() {

}