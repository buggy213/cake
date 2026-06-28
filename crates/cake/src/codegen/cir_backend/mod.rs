//! For a first attempt, just build a dead-simple templating instruction selector with no attempt at peephole optimization

use iced_x86::code_asm::*;

use crate::cir::{Constant, Inst, Module};
use crate::elf::Elf;

fn lower_to_x86(module: Module) -> Elf {
    let mut elf = Elf::new();
    let mut asm = CodeAssembler::new(64).expect("64-bit should be valid bitness");

    let fns = module.functions();
    let sigs = module.signatures();

    for func in fns {
        let func_sig_ref = func.signature;
        let func_sig = &sigs[func_sig_ref];

        let mut block_labels: Vec<CodeLabel> = func.blocks.iter().map(|_| asm.create_label()).collect();
        for (i, block) in func.blocks.iter().enumerate() {
            assert!(!block.insts.is_empty());
            asm.set_label(&mut block_labels[i]).expect("label should be valid");

            for (inst, ty) in std::iter::zip(&block.insts, &block.inst_types) {
                match inst {
                    Inst::BlockArgument => todo!(),
                    Inst::Constant { val } => {
                        match *val {
                            Constant::i8(v) => asm.mov(eax, v as i32),
                            Constant::u8(v) => asm.mov(eax, v as u32),
                            Constant::i16(v) => asm.mov(eax, v as i32),
                            Constant::u16(v) => asm.mov(eax, v as u32),
                            Constant::i32(v) => asm.mov(eax, v),
                            Constant::u32(v) => asm.mov(eax, v),
                            Constant::i64(v) => asm.mov(rax, v),
                            Constant::u64(v) => asm.mov(rax, v),
                            Constant::f32(_) => todo!("fp constants"),
                            Constant::f64(_) => todo!("fp constants"),
                        }.expect("bad asm");
                    },
                    Inst::Add { a, b } => {
                        
                    },
                    Inst::Sub { a, b } => todo!(),
                    Inst::Mul { a, b } => todo!(),
                    Inst::Div { a, b } => todo!(),
                    Inst::Load { addr } => todo!(),
                    Inst::Store { addr, val } => todo!(),
                    Inst::StackAddr { slot } => todo!(),
                    Inst::CastInt { val, target_type } => todo!(),
                    Inst::CompareInt { a, b, mode } => todo!(),
                    Inst::CompareFloat { a, b, mode } => todo!(),
                    Inst::BranchIf { cond, con, alt } => todo!(),
                    Inst::Return { value } => todo!(),
                    Inst::Jump { target } => todo!(),
                }
            }
        }
    }

    todo!()
}

#[cfg(test)]
mod test {
    use crate::{cir, semantics::resolver::resolve_ast_tests::{ResolveHarnessInput, resolve_harness}};

    #[test]
    fn test_basic() {
        let code = r#"
        int main() {
            return 5 + 3;
        }
        "#;
        
        let input = ResolveHarnessInput { code };
        let resolved = resolve_harness(input);
        let module = cir::ast2cir::lower_ast(resolved);

        dbg!(module);
    }
}