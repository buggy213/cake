//! Instruction selection to lower CIR -> MIR
//! 
//! Operates directly on linear IR the whole way through. Instruction selection is 
//! done by matching "tiles" against def-use trees in CIR in a greedy manner (maximal munch).
//! We use the number of uses of an SSA value to determine whether it can be tiled over by a pattern.
//! Concretely, in a use-def "tree" of CIR of the form
//!          %4 = add %1 %2
//!         /        \
//!        %1      %2 = load %3
//!                   |
//!                  %3
//! Assuming that:
//! - we are matching %4
//! - %2 does not have any other uses
//! Then, it would be safe to lower this to AddMemToReg, since nothing else needs %2, 
//! so it's ok to fold the memory load in.
//!
//! Instruction selection is performed globally by iterating over basic blocks in post-order
//! and selecting uses before defs in a dataflow-like fashion. This is similar to
//! LLVM's GlobalISel

use crate::cir::{Function, FunctionDefinition};

fn select_function(function: &FunctionDefinition) {
    
}

