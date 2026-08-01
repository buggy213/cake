//! For a first attempt, just build a dead-simple templating instruction selector with no attempt at peephole optimization
use cranelift::codegen::ir::Block;
use iced_x86::code_asm::*;
use rustc_hash::{FxHashMap, FxHashSet};

use iced_x86;
use crate::cir::{BlockRef, Constant, Data, DataContents, DataRef, FuncRef, Function, FunctionDefinition, Inst, Module, Signature, StackSlotRef, Type};
use crate::elf::{Elf, ElfSymbolBinding, ElfSymbolType, Section, SymbolTableIndex};

enum OperandSize {
    Byte,
    Word,
    Dword,
    Qword
}

/// helper to get the 32-bit version of a register
fn register_dword(reg: AsmRegister64) -> AsmRegister32 {
    let reg64 = [rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15];
    let reg32 = [eax, ecx, edx, ebx, esp, ebp, esi, edi, r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d];
    
    let pos = reg64.iter().position(|&r| r == reg).unwrap();
    reg32[pos]
}

/// helper to get the 32-bit version of a register
fn register_word(reg: AsmRegister64) -> AsmRegister16 {
    let reg64 = [rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15];
    let reg16 = [ax, cx, dx, bx, sp, bp, si, di, r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w];
    
    let pos = reg64.iter().position(|&r| r == reg).unwrap();
    reg16[pos]
}

/// helper to get the 32-bit version of a register
fn register_byte(reg: AsmRegister64) -> AsmRegister8 {
    let reg64 = [rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15];
    let reg8 = [al, cl, dl, bl, spl, bpl, sil, dil, r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b];
    
    let pos = reg64.iter().position(|&r| r == reg).unwrap();
    reg8[pos]
}

/// helper to emit a memcpy through `reg`, copying a fixed number of bytes
fn emit_memcpy(
    assembler: &mut CodeAssembler, 
    reg: AsmRegister64, 
    src: AsmMemoryOperand, 
    dst: AsmMemoryOperand, 
    size: u32
) {
    assert!(size % 8 == 0 || size % 4 == 0, "i'm lazy, and iced-x86 is annoying");
    if size % 8 == 0 {
        for off in 0..size / 8 {
            assembler.mov(reg, src + off);
            assembler.mov(dst + off, reg);
        }
    }

    if size % 4 == 0 {
        let reg = register_dword(reg);
        for off in 0..size / 4 {
            assembler.mov(reg, src + off);
            assembler.mov(dst + off, reg);
        }
    }
}

struct X86Codegen {
    data_to_symbol: FxHashMap<DataRef, SymbolTableIndex>,
    func_to_symbol: FxHashMap<FuncRef, SymbolTableIndex>,

    /// used to name unnamed global variables `_unnamed_{i}`
    unnamed_global_counter: usize,
}

impl X86Codegen {
    pub(crate) fn emit_module(&mut self, module: &Module) -> Elf {
        let mut elf = Elf::new();
        
        for (data_ref, data) in DataRef::enumerate(module.data()) {            
            self.emit_data(&mut elf, data, data_ref);
        }
        
        for ((func_ref, func), sig) in std::iter::zip(FuncRef::enumerate(module.functions()), module.signatures()) {
            self.emit_function(&mut elf, func, func_ref, sig);
        }

        elf
    }

    fn emit_data(&mut self, elf: &mut Elf, data: &Data, data_ref: DataRef) {
        let unnamed;
        let name = if let Some(name) = &data.name {
            unnamed = false;
            name.as_str()
        }
        else {
            unnamed = true;
            self.unnamed_global_counter += 1;
            &format!("_unnamed_{}", self.unnamed_global_counter)
        };

        match &data.contents {
            DataContents::Defined(items) => {
                let data_offset = elf.add_data(&items);
                let symbol = elf.define_symbol(
                    name, 
                    ElfSymbolBinding::Global, 
                    ElfSymbolType::Object, 
                    Section::Data, 
                    data_offset, 
                    items.len()
                );

                self.data_to_symbol.insert(data_ref, symbol);
            },
            DataContents::Zeros(size) => {
                let bss_offset = elf.add_bss(*size);
                let symbol = elf.define_symbol(
                    name, 
                    ElfSymbolBinding::Global, 
                    ElfSymbolType::Object, 
                    Section::Bss, 
                    bss_offset, 
                    *size
                );

                self.data_to_symbol.insert(data_ref, symbol);
            },
            DataContents::Undefined => {
                debug_assert!(!unnamed, "unnamed and undefined symbol should not be produced by frontend");
                let symbol = elf.undefined_symbol(
                    name, 
                    ElfSymbolBinding::Global,
                    ElfSymbolType::Object
                );

                self.data_to_symbol.insert(data_ref, symbol);
            },
        }
    }

    fn emit_function(&mut self, elf: &mut Elf, func: &Function, func_ref: FuncRef, func_sig: &Signature) {
        if let Some(func_body) = &func.definition {
            let symbol = elf.define_symbol(
                &func.name, 
                ElfSymbolBinding::Global, 
                ElfSymbolType::Func, 
                Section::Text, 
                0, 
                0
            );
            
            let (offset, func_size) = self.emit_function_body(elf, func_body, func_sig);

            // align to 16-byte boundary for the next function
            let padding = func_size.next_multiple_of(16) - func_size;
            const PADDING: [u8; 16] = [0; 16];            
            elf.add_text(&PADDING[..padding]);
            elf.update_symbol(symbol, offset, func_size);

            self.func_to_symbol.insert(func_ref, symbol);
        }
        else {
            let symbol = elf.undefined_symbol(
                &func.name, 
                ElfSymbolBinding::Global, 
                ElfSymbolType::Func
            );

            self.func_to_symbol.insert(func_ref, symbol);
        }

        
    }

    /// returns how many bytes emitted into ELF's .text section
    fn emit_function_body(
        &mut self, 
        elf: &mut Elf, 
        func: &FunctionDefinition,
        func_sig: &Signature,
    ) -> (usize, usize) {
        let mut assembler = CodeAssembler::new(64).unwrap();
        
        // 0. layout the stack slots
        // (very) conservatively allocate 128 bytes for spilling callee-saved registers and 
        // issues with register allocation. if both happen at once we're kinda cooked lol
        let mut static_stack_size = 128;
        let mut stack_slot_to_offset_from_top = FxHashMap::default();
        for (ss_ref, stack_slot) in StackSlotRef::enumerate(&func.stack_slots) {
            static_stack_size += stack_slot.size;
            static_stack_size = static_stack_size.next_multiple_of(stack_slot.align);
            stack_slot_to_offset_from_top.insert(ss_ref, static_stack_size);
        }
        
        // stack needs to be 16-byte aligned
        static_stack_size = static_stack_size.next_multiple_of(16);
        assembler.sub(rsp, static_stack_size as i32);
        
        let stack_slot_to_rsp_offset = |ss_ref: StackSlotRef| -> u32 {
            static_stack_size - stack_slot_to_offset_from_top[&ss_ref]
        };

        let mut dynamic_stack_size = 0;
        
        // 1. perform critical edge splitting

        // 2. obtain a reverse-post order over blocks
        // reversed_edges[b] returns all b' where b' -> b exists in CFG
        let mut reversed_edges: Vec<Vec<BlockRef>> = vec![Vec::new(); func.blocks.len()];

        for (b, block) in BlockRef::enumerate(&func.blocks) {
            let inst_refs = block.inst_refs.borrow();
            let terminator = inst_refs.last();
            let Some(&terminator_inst) = terminator else {
                continue;
            };

            match func.insts[terminator_inst] {
                Inst::BranchIf { con, alt, .. } => {
                    reversed_edges[con.get_inner()].push(b);
                    reversed_edges[alt.get_inner()].push(b);
                },
                Inst::Return { .. } => continue,
                Inst::Jump { target, .. } => {
                    reversed_edges[target.get_inner()].push(b);
                },
                _ => unreachable!("function doesn't have terminator?")
            }
        }

        fn postorder_blocks(
            b: BlockRef,
            adjacency: &Vec<Vec<BlockRef>>, 
            visited: &mut FxHashSet<BlockRef>, 
            ordered: &mut Vec<BlockRef>
        ) {
            if visited.contains(&b) {
                return;
            }

            visited.insert(b);
            for &neighbor in &adjacency[b.get_inner()] {
                postorder_blocks(neighbor, adjacency, visited, ordered);
            }

            ordered.push(b);            
        }

        let mut visited = FxHashSet::default();
        let mut ordered = Vec::new();
        for (block, _) in BlockRef::enumerate(&func.blocks) {
            postorder_blocks(block, &reversed_edges, &mut visited, &mut ordered);
        }

        // 2. iterate in reverse-postorder
        for &block_ref in &ordered {
            let block = &func.blocks[block_ref];
            
            // special handling for the first block, move arguments into assigned stack slots
            if block_ref.get_inner() == 0 {
                let mut param_idx = 0;
                for (&param, (ss_ref, _)) in 
                    Iterator::zip(func_sig.argument_types.iter(), StackSlotRef::enumerate(&func.stack_slots)) {

                    let dst = match param {
                        Type::i8 => byte_ptr(rsp + stack_slot_to_rsp_offset(ss_ref)),
                        Type::i16 => word_ptr(rsp + stack_slot_to_rsp_offset(ss_ref)),
                        Type::i32 => dword_ptr(rsp + stack_slot_to_rsp_offset(ss_ref)),
                        Type::i64 => qword_ptr(rsp + stack_slot_to_rsp_offset(ss_ref)),
                        Type::ptr => qword_ptr(rsp + stack_slot_to_rsp_offset(ss_ref)),
                        Type::f32 => dword_ptr(rsp + stack_slot_to_rsp_offset(ss_ref)),
                        Type::f64 => qword_ptr(rsp + stack_slot_to_rsp_offset(ss_ref)),
                    };

                    if param.is_integral() {
                        let source = match param_idx {
                            0 => rcx,
                            1 => rdx,
                            2 => r8,
                            3 => r9,
                            _ => todo!()
                        };

                        match param {
                            Type::i8 => {
                                assembler.mov(dst, register_byte(source))
                            }
                            Type::i16 => {
                                assembler.mov(dst, register_word(source))
                            },
                            Type::i32 => {
                                assembler.mov(dst, register_dword(source))
                            },
                            Type::i64 => {
                                assembler.mov(dst, source)
                            }
                            Type::ptr => {
                                assembler.mov(dst, source)
                            }
                            _ => unreachable!()
                        }.unwrap();
                    }
                    else {
                        let source = match param_idx {
                            0 => xmm0,
                            1 => xmm1,
                            2 => xmm2,
                            3 => xmm3,
                            _ => todo!()
                        };

                        match param {
                            Type::f32 => {
                                assembler.movss(dst, source)
                            },
                            Type::f64 => {
                                assembler.movsd_2(dst, source)
                            },
                            _ => unreachable!(),
                        }.unwrap();
                    }

                    param_idx += 1;
                }
            }

            // perform liveness analysis to determine live-in and live-out sets of each block
            // the values in both sets, as well as the values defined by the instructions in the
            // block itself, are collected into live ranges (live-in = live range starts at beginning, 
            // live-out = live range goes to end). then, run a local register allocation algorithm
            // 
            // live-in and live-out values must be spilled to memory (i.e. SSA values are only registers within a block)
            // this also requires splitting critical edges
            // if a block has >1 successor, then it spills its live-out (dictating the basic block memory interface)
            // if a block has >1 predecessor, then it loads its live-in (dictating the basic block memory interface)
            for &iref in block.inst_refs.borrow().iter() {
                let inst = &func.insts[iref];    
                


                match inst {
                    Inst::Constant { val } => {
                        // assembler.mov(op0, op1)  
                    },
                    Inst::Add { a, b } => {
                        // assembler.lea()  
                    },
                    Inst::Sub { a, b } => {
                        // assembler.mov() if lhs needs to be used later
                        // assembler.sub(op0, op1)
                    },
                    Inst::Mul { a, b } => {
                        // try to use lea if one side is a nice constant
                        // assembler.mov() if lhs needs to be used later
                        // assembler.imul_2()  
                    },
                    Inst::Div { a, b } => {
                        // this one monopolizes rax and rdx, which is annoying
                        // assembler.idiv(op0)
                    },
                    Inst::Modulo { a, b } => {
                        // modulo comes out in rdx
                        // assembler.idiv(op0)
                    },
                    Inst::And { a, b } => {
                        // assembler.and(op0, op1)
                    },
                    Inst::Or { a, b } => todo!(),
                    Inst::Xor { a, b } => todo!(),
                    Inst::Shl { a, b } => todo!(),
                    Inst::Ashr { a, b } => todo!(),
                    Inst::Lshr { a, b } => todo!(),
                    Inst::Icmp { mode, a, b, signed } => {
                        // 
                    },
                    Inst::Fadd { a, b } => {
                        // assembler.vaddss(op0, op1)
                    },
                    Inst::Fsub { a, b } => {
                        // assembler.vsubss(op0, op1)
                    },
                    Inst::Fmul { a, b } => {
                        // assembler.vmulss()
                    },
                    Inst::Fdiv { a, b } => {
                        // assembler.vdivss(op0, op1)
                    },
                    Inst::Fcmp { mode, a, b } => todo!(),
                    Inst::IntToFp { v } => {
                        // assembler.cvtsi2ss(op0, op1)
                    },
                    Inst::FpToInt { v } => todo!(),
                    Inst::Load { addr } => {
                        // assembler.mov(op0, op1)
                    },
                    Inst::Store { addr, val } => todo!(),
                    Inst::StackAddr { slot } => {
                        let offset = stack_slot_to_rsp_offset(*slot);
                        // assember.lea()
                    },
                    Inst::Zext { v } => {
                        // no-op on x86 due to implicit zero extension
                    },
                    Inst::Sext { v } => {
                        // assembler.movsx(op0, op1)
                    },
                    Inst::Truncate { v } => {
                        // ??
                    },
                    Inst::FpCast { v } => {
                        
                    },
                    Inst::PtrAdd { ptr, offset } => {
                        // assembler.lea()
                    },
                    Inst::PtrToInt { v } => {
                        // no-op
                    },
                    Inst::IntToPtr { v } => todo!(),
                    Inst::CompareInt { a, b, mode } => todo!(),
                    Inst::CompareFloat { a, b, mode } => todo!(),
                    Inst::Select { cond, x, y } => {
                        // assembler.sub(cond, 1)
                        // assembler.cmov()
                    },
                    Inst::BranchIf { cond, con, con_args, alt, alt_args } => todo!(),
                    Inst::Return { values } => todo!(),
                    Inst::Jump { target, arguments } => todo!(),
                    Inst::Call { func, arguments } => todo!(),
                    Inst::CallIndirect { callee_sig, func_ptr, arguments } => todo!(),
                    Inst::FuncAddr { func } => {
                        // assembler.mov()
                        // assembler.instructions().last().unwrap().ip();
                    },
                    Inst::DataAddr { data } => {
                        // assembler.mov(op0, op1);
                        // assembler.instructions().last().unwrap().ip();
                    },
                    Inst::Intrinsic { intrinsic, arguments } => todo!(),
                } 
            }
        }
        
        todo!()
    }
}