use super::{FA, alphabet::AsciiChar};

// states = rows
// characters = columns, 1 additional column for EOF
struct DFATable {
    data: Vec<u32>,
    columns: usize,
    rows: usize
}

struct DFAScanner {
    table: DFATable,
    state: usize
}

impl DFATable {
    fn from_ascii_dfa(dfa: &FA<AsciiChar>) -> DFATable {
        let num_states = dfa.nodes.len();
        
        
        todo!()
    } 
}

impl DFAScanner {
    pub fn from_ascii_dfa(dfa: &FA<AsciiChar>) -> DFAScanner {
        todo!()
    }
}

