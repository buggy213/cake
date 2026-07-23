//! For a first attempt, just build a dead-simple templating instruction selector with no attempt at peephole optimization

use crate::cir::{Constant, Inst, Module};
use crate::elf::Elf;
