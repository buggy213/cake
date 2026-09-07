//! Computes a post-order, or a reverse post-order traversal over the CFG of a CIR function

use cake_util::{IndexVec, index_vec};

use crate::cir::{BlockRef, FunctionDefinition};

struct PostOrder<'func> {
    func: &'func FunctionDefinition,
    stack: Vec<BlockRef>,
    // 0 = not visited, 1 = added to stack, 2 = visited
    visited: IndexVec<BlockRef, u8>,
}

impl Iterator for PostOrder<'_> {
    type Item = BlockRef;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(top) = self.stack.last() {
            let top = *top;
            if self.visited[top] == 2 {
                self.stack.pop();
                return Some(top);
            }

            let top_block = &self.func.blocks[top];
            for succ in top_block.successors(&self.func.insts) {
                if self.visited[succ] == 0 {
                    self.stack.push(succ);
                    self.visited[succ] = 1;
                }
            }

            self.visited[top] = 2;
        }

        None
    }
}

fn post_order(function: &FunctionDefinition) -> impl Iterator<Item = BlockRef> {
    PostOrder {
        func: function,
        stack: vec![function.entry_block()],
        visited: index_vec![0; function.blocks.len()]
    }
}