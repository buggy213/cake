//! Dominance Info
//! 
//! Computes the dominance relations between basic blocks in a function using iterative data-flow,
//! following the algorithm given in "A Simple, Fast Dominance Algorithm" (Cooper, Harvey, Kennedy 2001)
//! 
//! TODO: switch to Lengauer-Tarjan?

use cake_util::{IndexSlice, IndexVec, SmallIndexVec, index_vec};
use rustc_hash::FxHashSet;
use smallvec::SmallVec;

use crate::cir::{BlockRef, FunctionDefinition, post_order};

/// Immediate dominator of the entry block is itself, and the immediate dominator
/// of nodes not in the reverse postorder traversal (i.e. not reachable from entry block)
/// are also chosen to be the entry block
#[derive(Debug)]
pub(crate) struct DominanceTree {
    immediate_dominator: IndexVec<BlockRef, BlockRef>,
    children: IndexVec<BlockRef, SmallVec<[BlockRef; 8]>>,
}

impl DominanceTree {
    pub(crate) fn preorder_traversal(&self, func: &FunctionDefinition) -> impl Iterator<Item = BlockRef> + use<> {
        let mut stack: Vec<BlockRef> = vec![func.entry_block()];
        let mut result: Vec<BlockRef> = vec![];

        while !stack.is_empty() {
            let top = stack.pop().unwrap();
            result.push(top);

            for &child in &self.children[top] {
                stack.push(child);
            }
        }

        result.into_iter()
    }

    pub(crate) fn num_children(&self, block_ref: BlockRef) -> usize {
        self.children[block_ref].len()
    }
}

/// Dominance frontiers. Allows queries of the form `query(i, j) = is j in i's dominance frontier`
/// Internally implemented as adjacency list like structure
pub(crate) struct DominanceFrontiers {
    inner: IndexVec<BlockRef, SmallVec<[BlockRef; 8]>>,
}

impl DominanceFrontiers {
    fn new(num_blocks: usize) -> Self {
        Self { inner: index_vec![SmallVec::new(); num_blocks] }
    }

    pub(crate) fn query(&self, i: BlockRef, j: BlockRef) -> bool {
        self.inner[i].contains(&j)
    }

    pub(crate) fn frontier(&self, i: BlockRef) -> impl Iterator<Item = BlockRef> {
        self.inner[i].iter().copied()
    }

    pub(crate) fn insert(&mut self, i: BlockRef, j: BlockRef) -> bool {
        if self.inner[i].contains(&j) {
            return false;
        }

        self.inner[i].push(j);
        true
    }
}

pub(crate) fn dom_tree(func: &FunctionDefinition) -> DominanceTree {
    let entry = func.entry_block();
    let num_blocks = func.blocks.len();

    let mut doms: IndexVec<BlockRef, Option<BlockRef>> = index_vec![None; num_blocks];
    doms[entry] = Some(entry);

    let rpo: Vec<BlockRef> = post_order::reverse_post_order(&func).collect();
    let mut changed = true;
    while changed {
        changed = false;
        for &block in &rpo {
            let mut preds = func.blocks[block].preds.iter()
                .map(|p| p.pred_ref)
                .filter(|&j| doms[j] != None);
            
            let Some(mut new) = preds.next() else {
                // block has no processed predecessors
                continue;
            };

            for pred in preds {
                new = intersect_dom(&doms, pred, new);
            }
            
            if doms[block] != Some(new) {
                doms[block] = Some(new);
                changed = true;
            }
        }
    }

    let idoms: IndexVec<BlockRef, BlockRef> = doms.into_iter()
        .map(|d| d.unwrap_or(func.entry_block()))
        .collect();

    let mut children: IndexVec<BlockRef, SmallVec<[BlockRef; 8]>> = index_vec![SmallVec::new(); num_blocks];
    for &block in &rpo {
        if block == func.entry_block() {
            continue
        }

        let parent = idoms[block];
        children[parent].push(block);
    }

    DominanceTree { 
        immediate_dominator: idoms,
        children
    }
}

pub(crate) fn dom_frontiers(func: &FunctionDefinition, dom_tree: &DominanceTree) -> DominanceFrontiers {
    let mut dom_frontier = DominanceFrontiers::new(func.blocks.len());
    
    for (bref, block) in BlockRef::enumerate2(&func.blocks) {
        if block.preds.len() < 2 {
            continue;
        }

        for &pred in &block.preds {
            let mut runner = pred.pred_ref;
            while runner != dom_tree.immediate_dominator[bref] {
                dom_frontier.insert(runner, bref);
                runner = dom_tree.immediate_dominator[runner];
            }
        }
    }

    dom_frontier
}

fn intersect_dom(
    doms: &IndexSlice<BlockRef, [Option<BlockRef>]>, 
    b1: BlockRef, 
    b2: BlockRef
) -> BlockRef {
    let mut finger1 = b1;
    let mut finger2 = b2;

    while finger1 != finger2 {
        while finger1 < finger2 {
            finger2 = doms[finger2].unwrap();
        }
        while finger2 < finger1 {
            finger1 = doms[finger1].unwrap();
        }
    }

    finger1
}

#[cfg(test)]
mod test {
    use crate::cir::{ast2cir, dom_info::dom_tree};

    #[test]
    fn test_basic() {
        let module = ast2cir::test::conditional_module();
        let func = module.functions().iter().next().unwrap();
        let func_defn = func.definition.as_ref().unwrap();
        let dom = dom_tree(&func_defn);

        dbg!(dom);
    }
}