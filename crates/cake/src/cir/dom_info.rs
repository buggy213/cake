//! Dominance Info
//! 
//! Computes the dominance relations between basic blocks in a function using iterative data-flow,
//! following the algorithm given in "A Simple, Fast Dominance Algorithm" (Cooper, Harvey, Kennedy 2001)
//! 
//! TODO: switch to Lengauer-Tarjan?

use cake_util::{IndexSlice, IndexVec, index_vec};

use crate::cir::{BlockRef, FunctionDefinition, post_order};

/// Immediate dominator of the entry block is itself, and the immediate dominator
/// of nodes not in the reverse postorder traversal (i.e. not reachable from entry block)
/// are also chosen to be the entry block
#[derive(Debug)]
struct DominanceTree {
    immediate_dominators: IndexVec<BlockRef, BlockRef>,
}

fn dom_tree(func: &FunctionDefinition) -> DominanceTree {
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

    let idoms = doms.into_iter()
        .map(|d| d.unwrap_or(func.entry_block()))
        .collect();

    DominanceTree { 
        immediate_dominators: idoms
    }
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