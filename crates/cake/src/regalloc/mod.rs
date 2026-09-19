

pub(crate) trait Function {
    
}


pub(crate) trait RegisterAllocator {
    fn allocate(&mut self);
}

// Implementations of the register allocator
mod basic_alloc;