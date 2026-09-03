//! CIR Compaction Pass
//! 
//! Over time, the CIR data structures can become fragmented, as insertions and 
//! removals of instructions and basic blocks piles up. This pass exists to defragment 
//! the arenas holding them. 