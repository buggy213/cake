pub(crate) mod elf;
pub(crate) mod parser;
pub(crate) mod platform;
pub(crate) mod scanner;
pub(crate) mod semantics;
pub(crate) mod types;

pub use parser::{HandParser, ParseOutput};
pub use platform::Platform;
pub use scanner::preprocessor::Preprocessor;
pub use semantics::resolver::{ResolveOutput, Resolver};

mod cir;
mod codegen;
mod mir;
mod regalloc;

pub use codegen::CraneliftBackend;
