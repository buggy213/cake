//! Register allocator interface. Uses a pull-based approach to query the IR through a set of traits,
//! and a push-based approach to apply edits to the IR at the end

pub(crate) trait Function {
    
}

pub(crate) enum Edit {
    
}


pub(crate) trait RegisterAllocator {
    fn allocate(&mut self);
}

// Implementations of the register allocator
mod basic_alloc;