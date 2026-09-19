//! Basic Register Allocator
//! 
//! This is the most basic register allocator possible, as it just spills every value onto the stack.
//! It is used for testing and to bring up the register allocator interface

use crate::regalloc::RegisterAllocator;

struct BasicRegisterAllocator {

}

impl RegisterAllocator for BasicRegisterAllocator {
    fn allocate(&mut self) {
        todo!()
    }
}