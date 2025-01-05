mod fa;
mod regex;

#[cfg(test)]
mod fa_tests;

pub type AsciiChar = u8;
pub use fa::DFATable;
pub mod lexeme_def;
