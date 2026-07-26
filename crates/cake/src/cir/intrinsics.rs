#[derive(Debug, Clone, Copy)]
pub(crate) enum Intrinsic {
    Memcopy
}


impl std::fmt::Display for Intrinsic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Intrinsic::Memcopy => write!(f, "memcpy"),
        }
    }
}
