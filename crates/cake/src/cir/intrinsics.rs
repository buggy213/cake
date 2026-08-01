use smallvec::SmallVec;

use crate::cir::{BlockBuilder, Inst, InstRef, Type, TypeVec, Value, ValueVecRef};

#[derive(Debug, Clone, Copy)]
pub(crate) enum Intrinsic {
    // (dst: ptr, src: ptr, size: u64)
    Memcopy
}

impl<'block> BlockBuilder<'block> {
    fn intrinsic_call(&mut self, intrinsic: Intrinsic, args: &[Value], outputs: &[Type]) -> InstRef {
        let args = ValueVecRef::from_push(self.value_vecs, SmallVec::from_slice(args));
        let intrinsic_inst = Inst::Intrinsic { intrinsic, arguments: args };
        self.inst_types.push(TypeVec::from_slice(outputs));
        InstRef::from_push(self.insts, intrinsic_inst)
    }
    
    pub(crate) fn memcpy(&mut self, dst: Value, src: Value, size: Value) {
        self.intrinsic_call(Intrinsic::Memcopy, &[dst, src, size], &[]);
    }
}

impl std::fmt::Display for Intrinsic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Intrinsic::Memcopy => write!(f, "memcpy"),
        }
    }
}
