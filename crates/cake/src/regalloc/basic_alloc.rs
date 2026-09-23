//! Basic Register Allocator
//! 
//! This is the most basic register allocator possible. Between basic blocks, everything is spilled to stack.
//! Within a single basic block, it just does linear scan allocation
//! It is used for testing and to bring up the register allocator interface

use crate::regalloc::RegisterAllocator;

struct BasicRegisterAllocator {

}

impl RegisterAllocator for BasicRegisterAllocator {
    fn allocate(&mut self) {
        todo!()
    }
}