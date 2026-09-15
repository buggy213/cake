//! CIR Compaction Pass
//! 
//! Over time, the CIR data structures can become fragmented, as insertions and 
//! removals of instructions and basic blocks piles up. This pass exists to defragment 
//! the arenas holding them. 
//! 
//! It might also be worth sorting instructions living in the same basic block to
//! live contiguously, and sort the basic blocks themselves into a good order (post-order? RPO?)
//! for subsequent traversals.