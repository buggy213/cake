use std::collections::HashSet;

use thiserror::Error;

use crate::cir::{BlockRef, FunctionDefinition, Module};

#[derive(Debug, Error)]
pub(crate) enum ModuleVerificationError {
    #[error("`{0}` name is duplicated in module")]
    NonUniqueNames(String),

    #[error("error while verifying `{0}`: {1}")]
    FunctionVerificationError(String, Box<FunctionVerificationError>)
}

type ModuleResult<T> = std::result::Result<T, Box<ModuleVerificationError>>;

#[derive(Debug, Error)]
pub(crate) enum FunctionVerificationError {
    #[error("bad operands for operation")]
    BadOperands,
    #[error("block `b{0}` is missing terminator")]
    NoTerminator(usize),
    #[error("block `b{0}` has terminator before its final instruction")]
    EarlyTerminator(usize),
}

type FunctionResult<T> = std::result::Result<T, Box<FunctionVerificationError>>;
struct VerifierState<'module> {
    global_names: HashSet<&'module str>
}

impl<'module> VerifierState<'module> {
    fn new() -> VerifierState<'module> {
        VerifierState { global_names: HashSet::new() }
    }
}

pub(crate) fn verify_cir_module(module: &Module) -> ModuleResult<()> {
    let mut state = VerifierState::new();
    
    check_unique_names(&mut state, module)?;

    todo!()
}

fn check_unique_names<'module>(state: &mut VerifierState<'module>, module: &'module Module) -> ModuleResult<()> {
    for data in module.data() {
        let Some(name) = data.name.as_ref() else {
            continue;
        };

        if state.global_names.insert(&name) {
            return Err(ModuleVerificationError::NonUniqueNames(name.to_string()).into())
        }
    }

    for func in module.functions() {
        if state.global_names.insert(&func.name) {
            return Err(ModuleVerificationError::NonUniqueNames(func.name.clone()).into())
        }
    }

    Ok(())
}

fn verify_cir_function(function: &FunctionDefinition) -> FunctionResult<()> {
    todo!()
}

/// Check that each instruction which transfers control to a block supplies the correct block arguments
fn check_block_arguments() -> FunctionResult<()> {
    todo!()
}

/// Check that instructions have correctly-typed outputs and operands
fn check_inst_types() -> FunctionResult<()> {
    todo!()
}

/// Check that usages of a value are dominated by its definition
fn check_usages() -> FunctionResult<()> {
    todo!()
}

/// Check that all basic blocks have a terminator for final instruction
/// and nowhere else
fn check_terminators<'module>(
    state: &mut VerifierState<'module>, 
    function: &'module FunctionDefinition
) -> FunctionResult<()> {
    for block in &function.blocks {
        for (i, &iref) in block.inst_refs.borrow().iter().enumerate() {
            if i + 1 == block.inst_refs.borrow().len() {
                if !function.insts[iref].is_terminator() {
                    return Err(FunctionVerificationError::NoTerminator(i).into());
                }
            }
            else {
                if function.insts[iref].is_terminator() {
                    return Err(FunctionVerificationError::EarlyTerminator(i).into());
                }
            }
        }
    }

    Ok(())
}
