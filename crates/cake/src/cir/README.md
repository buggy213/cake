CIR (Cake Intermediate Representation)

A low-level intermediate representation for the Cake compiler. Bears close resemblance to Cranelift and LLVM IRs. Each translation unit corresponds to a `Module`, which can contain `Function`s and `Data`. Every function has a `Signature` describing its formal parameters and return type.  

Within each function is a SoA-style representations of basic blocks and instructions. Specifically, each function has a `block: Vec<Block>` element containing basic blocks, `insts: Vec<Inst>` containing instructions, and `stack_slots: Vec<StackSlot>` containing the stack slots used within a function. 

"Parallel" to these main containers, there is:
- `inst_types: Vec<TypeVec>` containing list of types for an instruction. 
  Most instructions have 1 or 0, but some instructions, like function calls, might return more than one value; since there's no native support for aggregates in the IR, so this is the workaround.
- `value_vecs: Vec<ValueVec>` containing lists of values for use within the instructions (e.g. arguments for 
  a function call or block terminator)
- `inst_uses: Vec<UseVec>` containing the uses of a given instruction's output value
- `inst_block: Vec<Option<BlockRef>>` pointing to the block a given instruction is in 

Blocks are represented in AoS data layout, with the following fields:
- `inst_refs: RefCell<Vec<InstRef>>` containing the instructions actually present within the block.
- `block_args: Vec<Type>` containing the block arguments (isomorphic to phis in more traditional SSA form)
- `block_arg_uses: Vec<UseVec>` containing uses of the block arguments
- `preds: Vec<BlockRef>` containing the predecessors of a given block
- `is_entry: bool` to mark the entry block

A key consequence of this design is that manipulating basic blocks might be `O(n)` in the size of the block, but most blocks are (hopefully) small, so this is not too worrisome. In addition, the cache locality of `inst_refs` makes such an operation less costly. 
