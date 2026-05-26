pub(crate) mod elf;
pub(crate) mod parser;
pub(crate) mod platform;
pub(crate) mod scanner;
pub(crate) mod semantics;
pub(crate) mod types;

pub use platform::Platform;
pub use scanner::preprocessor::Preprocessor;
pub use parser::{HandParser, ParseOutput};
pub use semantics::resolver::{Resolver, ResolveOutput};


mod codegen;
mod cir;