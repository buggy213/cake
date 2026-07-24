use std::{mem::MaybeUninit, ops::Range, str::FromStr};

use rustc_hash::FxHashMap;

use crate::{
    cir::{CompareMode, FuncRef, FunctionBuilder, Inst, Module, Signature, StackSlotRef, Type, Value}, parser::ast, semantics::{
        resolved_ast::{ExprRef, NodeRef, ResolvedASTNode, TypedExpressionNode},
        resolver::ResolvedAST,
        symtab::{ObjectIdx, ObjectRangeRef, SymbolTable},
    }, types::{BasicType, CType, layout::Layouts},
};

impl From<BasicType> for Type {
    fn from(value: BasicType) -> Self {
        match value {
            BasicType::Char => Self::i8,
            BasicType::UChar => Self::u8,
            BasicType::Short => Self::i16,
            BasicType::UShort => Self::u16,
            BasicType::Int => Self::i32,
            BasicType::UInt => Self::u32,
            BasicType::Long => Self::i64,
            BasicType::ULong => Self::u64,
            BasicType::Float => Self::f32,
            BasicType::Double => Self::f64,
        }
    }
}

impl TryFrom<&CType> for Type {
    type Error = ();

    fn try_from(value: &CType) -> Result<Self, Self::Error> {
        match value {
            CType::BasicType { basic_type, .. } => Ok((*basic_type).into()),
            CType::PointerType { .. } => Ok(Type::u64),
            _ => todo!("other types"),
        }
    }
}

struct Frame {
    object_range: ObjectRangeRef,
    stack_slots: Vec<StackSlotRef>,
}

impl Frame {
    fn contains(&self, object_idx: ObjectIdx) -> bool {
        self.object_range.0 as usize <= object_idx.get_inner()
            && object_idx.get_inner() < self.object_range.1 as usize
    }

    fn get_object_stack_slot(&self, object_idx: ObjectIdx) -> StackSlotRef {
        let idx = object_idx.get_inner() - self.object_range.0 as usize;
        self.stack_slots[idx]
    }
}

fn create_frame(
    fn_builder: &mut FunctionBuilder,
    symtab: &SymbolTable,
    layouts: &Layouts,
    object_range: ObjectRangeRef,
) -> Frame {
    let locals = symtab.object_range(object_range);
    let mut stack_slots = Vec::new();

    for local_var in locals {
        let size = local_var.object_type.size(layouts);
        let align = local_var.object_type.align(layouts);

        let stack_slot = fn_builder.add_stack_slot(size, align);
        stack_slots.push(stack_slot);
    }

    Frame {
        object_range,
        stack_slots,
    }
}

struct LowerFunctionContext {
    expr_ref_to_value: FxHashMap<ExprRef, Value>
}

impl LowerFunctionContext {
    fn new() -> Self {
        LowerFunctionContext { expr_ref_to_value: FxHashMap::default() }
    }
}

pub(crate) fn lower_ast(ast: ResolvedAST) -> Module {
    let Some(ResolvedASTNode::TranslationUnit { children }) = ast.nodes.first() else {
        panic!("corrupted ast")
    };

    let mut module = Module::new();

    for func in &ast.ast_indices[children.0 as usize..children.1 as usize] {
        let ResolvedASTNode::FunctionDefinition {
            symbol_idx, body, ..
        } = &ast.nodes[func.0 as usize]
        else {
            panic!("corrupted ast")
        };

        let ast_func = ast.symtab.get_function(*symbol_idx);
        let func_name = ast.symtab.get_function_name(*symbol_idx);
        let ast_func_type = ast.symtab.get_function_type(ast_func.function_type);
        let ast_func_args = &ast_func_type.parameter_types;
        let ast_func_args: Vec<CType> = ast_func_args.iter().map(|arg| arg.1.clone()).collect();

        let func_args: Vec<Type> = ast_func_args
            .iter()
            .map(|arg| arg.try_into().unwrap())
            .collect();
        let func_ret = if let CType::Void { .. } = ast_func_type.return_type {
            None
        } else {
            assert!(
                ast_func_type.return_type.is_scalar_type(),
                "only scalar types supported for now"
            );
            Some((&ast_func_type.return_type).try_into().unwrap())
        };

        let func_sig = Signature::new(func_args, func_ret.as_slice().to_vec());
        let func_ref = module.add_function(func_name.to_string(), func_sig);

        let mut func_builder = module.define_function(func_ref);

        let func_object_range = ast.symtab.function_object_range(*symbol_idx);
        let stack_frame = create_frame(
            &mut func_builder,
            &ast.symtab,
            &ast.layouts,
            func_object_range,
        );

        lower_function_body(&ast, *body, &mut func_builder, &stack_frame);
    }

    module
}

fn lower_function_body(
    ast: &ResolvedAST,
    fn_body: NodeRef,
    func_builder: &mut FunctionBuilder,
    stack_frame: &Frame,
) {
    let ResolvedASTNode::CompoundStatement { stmts, .. } = &ast.nodes[fn_body.0 as usize] else {
        panic!("corrupted ast")
    };

    let mut lower_fn_ctx = LowerFunctionContext::new();
    let (start, end) = (stmts.0 as usize, stmts.1 as usize);
    for &stmt_ref in &ast.ast_indices[start..end] {
        lower_stmt(ast, stmt_ref, func_builder, stack_frame, &mut lower_fn_ctx);
    }
}

fn lower_stmt(
    ast: &ResolvedAST,
    stmt: NodeRef,
    func_builder: &mut FunctionBuilder,
    stack_frame: &Frame,
    lower_fn_ctx: &mut LowerFunctionContext
) {
    let stmt_node = &ast.nodes[stmt.0 as usize];
    match stmt_node {
        ResolvedASTNode::TranslationUnit { children } => todo!(),
        ResolvedASTNode::FunctionDefinition {
            parent,
            symbol_idx,
            body,
        } => todo!(),
        ResolvedASTNode::Label { parent, labelee } => todo!(),
        ResolvedASTNode::CaseLabel {
            parent,
            labelee,
            case_index,
        } => todo!(),
        ResolvedASTNode::DefaultLabel { parent, labelee } => todo!(),
        ResolvedASTNode::NullStatement { parent } => {}
        ResolvedASTNode::CompoundStatement { parent, stmts } => {
            let (start, end) = (stmts.0 as usize, stmts.1 as usize);
            for &stmt_ref in &ast.ast_indices[start..end] {
                lower_stmt(ast, stmt_ref, func_builder, stack_frame, lower_fn_ctx);
            }
        }
        ResolvedASTNode::ExpressionStatement { parent, expr } => {
            lower_expr(ast, *expr, func_builder, stack_frame, lower_fn_ctx);
        }
        ResolvedASTNode::IfStatement {
            parent,
            condition,
            taken,
            not_taken,
        } => todo!(),
        ResolvedASTNode::SwitchStatement {
            parent,
            controlling_expr,
            body,
            context,
        } => todo!(),
        ResolvedASTNode::WhileStatement {
            parent,
            condition,
            body,
        } => todo!(),
        ResolvedASTNode::DoWhileStatement {
            parent,
            condition,
            body,
        } => todo!(),
        ResolvedASTNode::ForStatement {
            parent,
            init,
            condition,
            post_body,
            body,
        } => todo!(),
        ResolvedASTNode::GotoStatement { parent, target } => todo!(),
        ResolvedASTNode::ContinueStatement { parent, target } => todo!(),
        ResolvedASTNode::BreakStatement { parent, target } => todo!(),
        ResolvedASTNode::ReturnStatement {
            parent,
            return_value,
        } => {
            let return_value = return_value.map(|e| lower_expr(ast, e, func_builder, stack_frame, lower_fn_ctx));
            func_builder.insert().ret(return_value.as_slice());
        }
        ResolvedASTNode::Initializer {
            parent,
            object,
            assignment,
        } => todo!(),
    }
}

// TODO: this should be parameterized
fn ptrtype() -> Type {
    Type::u64
}

fn lower_expr(
    ast: &ResolvedAST,
    expr: ExprRef,
    func_builder: &mut FunctionBuilder,
    stack_frame: &Frame,
    lower_fn_ctx: &mut LowerFunctionContext
) -> Value {
    use crate::semantics::resolved_ast::TypedExpressionNode;
    let expr_node = &ast.exprs[expr];

    macro_rules! binary_op {
        // integer op
        ($ast:expr, $lhs:expr, $rhs:expr, $func_builder:expr, $stack_frame:expr, $lower_fn_ctx:expr, $op_name:ident) => {
            {
                let lhs = lower_expr($ast, $lhs, $func_builder, $stack_frame, $lower_fn_ctx);
                let rhs = lower_expr($ast, $rhs, $func_builder, $stack_frame, $lower_fn_ctx);

                $func_builder.insert().$op_name(lhs, rhs)
            }
        };

        // integer op, fp op
        ($ast:expr, $lhs:expr, $rhs:expr, $func_builder:expr, $stack_frame:expr, $lower_fn_ctx:expr, $op_name_int:ident, $op_name_fp:ident) => {
            {
                let lhs = lower_expr($ast, $lhs, $func_builder, $stack_frame, $lower_fn_ctx);
                let rhs = lower_expr($ast, $rhs, $func_builder, $stack_frame, $lower_fn_ctx);
                
                let op_ty = $func_builder.insert().type_of(lhs);
                if op_ty.is_integral() {
                    $func_builder.insert().$op_name_int(lhs, rhs)
                }
                else {
                    $func_builder.insert().$op_name_fp(lhs, rhs)
                }
            }
        };

        // compare op
        ($ast:expr, $lhs:expr, $rhs:expr, $func_builder:expr, $stack_frame:expr, $lower_fn_ctx:expr, $op_name_int:ident, $op_name_fp:ident, $compare_mode:ident) => {
            {
                let lhs = lower_expr($ast, $lhs, $func_builder, $stack_frame, $lower_fn_ctx);
                let rhs = lower_expr($ast, $rhs, $func_builder, $stack_frame, $lower_fn_ctx);

                let op_ty = $func_builder.insert().type_of(lhs);
                if op_ty.is_integral() {
                    $func_builder.insert().$op_name_int(CompareMode::$compare_mode, lhs, rhs)
                } else {
                    $func_builder.insert().$op_name_fp(CompareMode::$compare_mode, lhs, rhs)
                }
            }
        };
    }

    let value = 'match_expr: {
    match expr_node {
        TypedExpressionNode::CommaExpr(ctype, expr_range_ref) => {
            let expr_range: Range<usize> = (*expr_range_ref).into();
            let mut subexpr_value = None;
            for &subexpr_ref in &ast.expr_indices[expr_range] {
                subexpr_value = lower_expr(ast, subexpr_ref, func_builder, stack_frame, lower_fn_ctx).into();
            }

            subexpr_value.expect("grammar prohibits empty comma expr")
        },
        TypedExpressionNode::SimpleAssign(ctype, lhs, rhs) => {
            let location = lower_lvalue(ast, *lhs, func_builder, stack_frame, lower_fn_ctx);
            let rhs_value = lower_expr(ast, *rhs, func_builder, stack_frame, lower_fn_ctx);

            assert!(
                matches!(
                    ctype,
                    CType::BasicType { .. } | CType::PointerType { .. } | CType::EnumTypeRef { .. }
                ),
                "only scalars supported for now"
            );
            
            func_builder.insert().store(location, rhs_value);
            rhs_value
        }
        TypedExpressionNode::AugmentedAssign(ctype, expr_ref, expr_ref1) => {
            let result = lower_expr(ast, *expr_ref1, func_builder, stack_frame, lower_fn_ctx);
            let lvalue = lower_fn_ctx.expr_ref_to_value[expr_ref];
            let Value::Inst(iref) = lvalue else { unreachable!("lvalue must be result of load") };
            let Inst::Load { addr } = func_builder.func.insts[iref] else { 
                unreachable!("lvalue must be result of load") 
            };

            func_builder.insert().store(addr, result);
            result
        }
        TypedExpressionNode::PostAugmentedAssign(ctype, expr_ref, expr_ref1) => {
            let result = lower_expr(ast, *expr_ref1, func_builder, stack_frame, lower_fn_ctx);
            let lvalue = lower_fn_ctx.expr_ref_to_value[expr_ref];
            let Value::Inst(iref) = lvalue else { unreachable!("lvalue must be result of load") };
            let Inst::Load { addr } = func_builder.func.insts[iref] else {
                unreachable!("lvalue must be result of load")
            };

            func_builder.insert().store(addr, result);
            lvalue
        },
        TypedExpressionNode::Ternary(ctype, expr_ref, expr_ref1, expr_ref2) => {
            let cond = lower_expr(ast, *expr_ref, func_builder, stack_frame, lower_fn_ctx);
            let x = lower_expr(ast, *expr_ref1, func_builder, stack_frame, lower_fn_ctx);
            let y = lower_expr(ast, *expr_ref2, func_builder, stack_frame, lower_fn_ctx);

            func_builder.insert().select(cond, x, y)
        }
        TypedExpressionNode::LogicalAnd(ctype, expr_ref, expr_ref1) => {
            let ty: Type = ctype.as_basic().unwrap().into();
            let early_exit = func_builder.add_block();
            let eval_rhs = func_builder.add_block();
            let footer = func_builder.add_block();

            let lhs = lower_expr(ast, *expr_ref, func_builder, stack_frame, lower_fn_ctx);
            let zero = func_builder.insert().iconst_trunc(lhs, 0);
            let one = func_builder.insert().iconst_trunc(lhs, 1);
            let lhs_is_zero = func_builder.insert().icmp(CompareMode::Equal, lhs, zero);
            func_builder.insert().brif(lhs_is_zero, early_exit, &[], eval_rhs, &[]);

            func_builder.set_block(early_exit);
            func_builder.insert().jmp(footer, &[zero]);

            func_builder.set_block(eval_rhs);
            let rhs = lower_expr(ast, *expr_ref, func_builder, stack_frame, lower_fn_ctx);
            let rhs_is_zero = func_builder.insert().icmp(CompareMode::Equal, rhs, zero);
            let result = func_builder.insert().select(rhs_is_zero, zero, one);
            func_builder.insert().jmp(footer, &[result]);

            func_builder.set_block(footer);
            let phi_result = func_builder.add_block_arg(ty);

            phi_result
        },
        TypedExpressionNode::LogicalOr(ctype, expr_ref, expr_ref1) => {
            let ty: Type = ctype.as_basic().unwrap().into();
            let early_exit = func_builder.add_block();
            let eval_rhs = func_builder.add_block();
            let footer = func_builder.add_block();

            let lhs = lower_expr(ast, *expr_ref, func_builder, stack_frame, lower_fn_ctx);
            let zero = func_builder.insert().iconst_trunc(lhs, 0);
            let one = func_builder.insert().iconst_trunc(lhs, 1);
            let lhs_is_one = func_builder.insert().icmp(CompareMode::Equal, lhs, one);
            func_builder.insert().brif(lhs_is_one, early_exit, &[], eval_rhs, &[]);

            func_builder.set_block(early_exit);
            func_builder.insert().jmp(footer, &[one]);

            func_builder.set_block(eval_rhs);
            let rhs = lower_expr(ast, *expr_ref, func_builder, stack_frame, lower_fn_ctx);
            let rhs_is_zero = func_builder.insert().icmp(CompareMode::Equal, rhs, zero);
            let result = func_builder.insert().select(rhs_is_zero, zero, one);
            func_builder.insert().jmp(footer, &[result]);

            func_builder.set_block(footer);
            let phi_result = func_builder.add_block_arg(ty);

            phi_result
        },
        TypedExpressionNode::BitwiseAnd(ctype, expr_ref, expr_ref1) => 
            binary_op!(ast, *expr_ref, *expr_ref1, func_builder, stack_frame, lower_fn_ctx, and),
        TypedExpressionNode::BitwiseOr(ctype, expr_ref, expr_ref1) => 
            binary_op!(ast, *expr_ref, *expr_ref1, func_builder, stack_frame, lower_fn_ctx, or),
        TypedExpressionNode::BitwiseXor(ctype, expr_ref, expr_ref1) => 
            binary_op!(ast, *expr_ref, *expr_ref1, func_builder, stack_frame, lower_fn_ctx, xor),
        TypedExpressionNode::Equal(ctype, expr_ref, expr_ref1) => 
            binary_op!(ast, *expr_ref, *expr_ref1, func_builder, stack_frame, lower_fn_ctx, icmp, fcmp, Equal),
        TypedExpressionNode::NotEqual(ctype, expr_ref, expr_ref1) => 
            binary_op!(ast, *expr_ref, *expr_ref1, func_builder, stack_frame, lower_fn_ctx, icmp, fcmp, NotEqual),
        TypedExpressionNode::LessThan(ctype, expr_ref, expr_ref1) => 
            binary_op!(ast, *expr_ref, *expr_ref1, func_builder, stack_frame, lower_fn_ctx, icmp, fcmp, LessThan),
        TypedExpressionNode::GreaterThan(ctype, expr_ref, expr_ref1) => 
            binary_op!(ast, *expr_ref, *expr_ref1, func_builder, stack_frame, lower_fn_ctx, icmp, fcmp, GreaterThan),
        TypedExpressionNode::LessThanOrEqual(ctype, expr_ref, expr_ref1) => 
            binary_op!(ast, *expr_ref, *expr_ref1, func_builder, stack_frame, lower_fn_ctx, icmp, fcmp, LessThanOrEqual),
        TypedExpressionNode::GreaterThanOrEqual(ctype, expr_ref, expr_ref1) => 
            binary_op!(ast, *expr_ref, *expr_ref1, func_builder, stack_frame, lower_fn_ctx, icmp, fcmp, GreaterThanOrEqual),
        TypedExpressionNode::LShift(ctype, expr_ref, expr_ref1) => 
            binary_op!(ast, *expr_ref, *expr_ref1, func_builder, stack_frame, lower_fn_ctx, shl),
        TypedExpressionNode::RShift(ctype, expr_ref, expr_ref1) => {
            let lhs = lower_expr(ast, *expr_ref, func_builder, stack_frame, lower_fn_ctx);
            let rhs = lower_expr(ast, *expr_ref1, func_builder, stack_frame, lower_fn_ctx);

            let lhs_ty = ast.exprs[*expr_ref1].expr_type().as_basic().unwrap();
            if lhs_ty.is_signed() {
                func_builder.insert().ashr(lhs, rhs)
            }
            else {
                func_builder.insert().lshr(lhs, rhs)
            }
        },
        TypedExpressionNode::Multiply(ctype, expr_ref, expr_ref1) => 
            binary_op!(ast, *expr_ref, *expr_ref1, func_builder, stack_frame, lower_fn_ctx, mul, fmul),
        TypedExpressionNode::Divide(ctype, expr_ref, expr_ref1) => 
            binary_op!(ast, *expr_ref, *expr_ref1, func_builder, stack_frame, lower_fn_ctx, div, fdiv),
        TypedExpressionNode::Modulo(ctype, expr_ref, expr_ref1) => 
            binary_op!(ast, *expr_ref, *expr_ref1, func_builder, stack_frame, lower_fn_ctx, modulo),
        TypedExpressionNode::Add(ctype, expr_ref, expr_ref1) => 
            binary_op!(ast, *expr_ref, *expr_ref1, func_builder, stack_frame, lower_fn_ctx, add, fadd),
        TypedExpressionNode::Subtract(ctype, expr_ref, expr_ref1) => 
            binary_op!(ast, *expr_ref, *expr_ref1, func_builder, stack_frame, lower_fn_ctx, sub, fsub),
        TypedExpressionNode::PointerAdd(ctype, expr_ref, expr_ref1) => {
            assert!(
                ctype.is_object_pointer(),
                "pointer arithmetic requires pointer type"
            );

            let lhs = lower_expr(ast, *expr_ref, func_builder, stack_frame, lower_fn_ctx);
            let rhs = lower_expr(ast, *expr_ref1, func_builder, stack_frame, lower_fn_ctx);
            let sizeof_object = ctype.as_pointee().unwrap().size(todo!("plumb in layouts"));
            let sizeof_object_val = func_builder.insert().const_u64(sizeof_object as u64);
            
            let rhs_ptrtype = func_builder.insert().icast(rhs, ptrtype());
            let byte_offset = func_builder.insert().mul(rhs_ptrtype, sizeof_object_val);

            func_builder.insert().add(lhs, byte_offset)
        }
        TypedExpressionNode::PointerSub(ctype, expr_ref, expr_ref1) => {
            let lhs = lower_expr(ast, *expr_ref, func_builder, stack_frame, lower_fn_ctx);
            let rhs = lower_expr(ast, *expr_ref1, func_builder, stack_frame, lower_fn_ctx);
            let sizeof_object = ctype.as_pointee().unwrap().size(todo!("plumb in layouts"));
            let sizeof_object_val = func_builder.insert().const_u64(sizeof_object as u64);

            let rhs_ptrtype = func_builder.insert().icast(rhs, ptrtype());
            let byte_offset = func_builder.insert().mul(rhs_ptrtype, sizeof_object_val);
            
            func_builder.insert().sub(lhs, byte_offset)
        },
        TypedExpressionNode::PointerDiff(ctype, expr_ref, expr_ref1) => {
            let lhs = lower_expr(ast, *expr_ref, func_builder, stack_frame, lower_fn_ctx);
            let rhs = lower_expr(ast, *expr_ref1, func_builder, stack_frame, lower_fn_ctx);
            let sizeof_object = ctype.as_pointee().unwrap().size(todo!("plumb in layouts"));
            let sizeof_object_val = func_builder.insert().const_u64(sizeof_object as u64);

            let byte_diff = func_builder.insert().sub(rhs, lhs);
            func_builder.insert().div(byte_diff, sizeof_object_val)
        },
        TypedExpressionNode::Cast(ctype, expr_ref, ctype1) => todo!(),
        TypedExpressionNode::AddressOf(ctype, expr_ref) => {
            if let TypedExpressionNode::FunctionIdentifier(_, function_idx) = ast.exprs[*expr_ref] {
                func_builder.insert().func_addr(todo!("function_idx to FuncRef"))
            }
            else {
                lower_lvalue(ast, *expr_ref, func_builder, stack_frame, lower_fn_ctx)
            }
        },
        TypedExpressionNode::Dereference(ctype, expr_ref) => {
            let ptr = lower_expr(ast, *expr_ref, func_builder, stack_frame, lower_fn_ctx);
            let pointee_type: Type = match ctype {
                CType::BasicType { basic_type, qualifier } => (*basic_type).into(),
                CType::PointerType { pointee_type, qualifier } => ptrtype(),
                // aggregates represented as pointers, since the ir deliberately doesn't have aggregate types
                CType::StructureTypeRef { symtab_idx, qualifier } => {
                    break 'match_expr ptr;
                },
                CType::UnionTypeRef { symtab_idx, qualifier } => {
                    break 'match_expr ptr;
                },
                CType::EnumTypeRef { symtab_idx, qualifier } => todo!("handle enum"),
                _ => unreachable!("type check should prevent this")
            };

            func_builder.insert().load(ptr, pointee_type)
        },
        TypedExpressionNode::UnaryPlus(ctype, expr_ref) => 
            lower_expr(ast, *expr_ref, func_builder, stack_frame, lower_fn_ctx),
        TypedExpressionNode::UnaryMinus(ctype, expr_ref) => {
            let val = lower_expr(ast, *expr_ref, func_builder, stack_frame, lower_fn_ctx);
            let zero = func_builder.insert().iconst_trunc(val, 0);
            
            func_builder.insert().sub(zero, val)
        }
        TypedExpressionNode::BitwiseNot(ctype, expr_ref) => {
            let val = lower_expr(ast, *expr_ref, func_builder, stack_frame, lower_fn_ctx);
            let ones = func_builder.insert().iconst_trunc(val, u64::MAX);

            func_builder.insert().xor(val, ones)
        }
        TypedExpressionNode::Not(ctype, expr_ref) => {
            let val = lower_expr(ast, *expr_ref, func_builder, stack_frame, lower_fn_ctx);
            let zero = func_builder.insert().iconst_trunc(val, 0);
            let one = func_builder.insert().iconst_trunc(val, 1);
            let cmp = func_builder.insert().icmp(CompareMode::Equal, val, zero);

            func_builder.insert().select(cmp, one, zero)
        },
        TypedExpressionNode::DirectFunctionCall(result_type, function, arguments) => {
            let arg_range: Range<usize> = (*arguments).into();
            let mut arg_values = Vec::with_capacity(arg_range.len());
            let arg_range = &ast.expr_indices[arg_range];
            for &arg_expr in arg_range {
                let arg_value = lower_expr(
                    ast,
                    arg_expr,
                    func_builder,
                    stack_frame,
                    lower_fn_ctx
                );
                arg_values.push(arg_value);
            }

            let func_ref = FuncRef(function.get_inner() as u32);    
            let call_inst = func_builder.insert().call(func_ref, &arg_values);
        

            if result_type.is_void() {
                // TODO: fix this. void expressions should be allowed
                func_builder.insert().const_u64(0)
            } else {
                Value::TupleElement(call_inst, 0)
            }
        },
        TypedExpressionNode::IndirectFunctionCall(ctype, expr_ref, expr_range_ref) => {
            todo!()
        },
        TypedExpressionNode::DotAccess(ctype, expr_ref, member_ref) => {
            let location = lower_lvalue(ast, expr, func_builder, stack_frame, lower_fn_ctx);
            let cir_type = match ctype {
                CType::BasicType { basic_type, .. } => (*basic_type).into(),
                CType::PointerType { .. } => ptrtype(),
                CType::StructureTypeRef { .. } | CType::UnionTypeRef { .. } => break 'match_expr location,
                CType::EnumTypeRef { .. } => todo!("handle enums"),
                _ => unreachable!("other types shouldn't be possible")
            };

            func_builder.insert().load(location, cir_type)
        },
        TypedExpressionNode::ArrowAccess(ctype, expr_ref, member_ref) => todo!(),
        TypedExpressionNode::ArrayDecay(ctype, expr_ref) => todo!(),
        TypedExpressionNode::ObjectIdentifier(object_type, object_idx) => {
            let location = lower_lvalue(ast, expr, func_builder, stack_frame, lower_fn_ctx);

            let cir_type = match object_type {
                CType::BasicType { basic_type, .. } => (*basic_type).into(),
                CType::PointerType { .. } => ptrtype(),
                CType::StructureTypeRef { .. } | CType::UnionTypeRef { .. } => break 'match_expr location,
                CType::EnumTypeRef { .. } => todo!("handle enums"),
                _ => unreachable!("other types shouldn't be possible")
            };

            func_builder.insert().load(location, cir_type)
        },
        TypedExpressionNode::FunctionIdentifier(ctype, function_idx) => {
            func_builder.insert().func_addr(todo!("function_idx to FuncRef"))  
        },
        TypedExpressionNode::Constant(ctype, constant) => match *constant {
            ast::Constant::Int(v) => func_builder.insert().const_i32(v),
            ast::Constant::LongInt(v) => func_builder.insert().const_i64(v),
            ast::Constant::UInt(v) => func_builder.insert().const_u32(v),
            ast::Constant::ULongInt(v) => func_builder.insert().const_u64(v),
            ast::Constant::Float(v) => func_builder.insert().const_f32(v),
            ast::Constant::Double(v) => func_builder.insert().const_f64(v),
        },
        TypedExpressionNode::StringLiteral(ctype, s) => {
            let id = func_builder.declare_anonymous_data(true);

            // SAFETY: u8 can be 0
            let cstr: Box<[MaybeUninit<u8>]> = Box::new_zeroed_slice(s.len() + 1);
            let mut cstr = unsafe { cstr.assume_init() };
            cstr[..s.len()].copy_from_slice(s.as_bytes());

            func_builder.define_data(id, cstr);
            func_builder.insert().data_addr(id)
        },
    }
    };

    lower_fn_ctx.expr_ref_to_value.insert(expr, value);
    value
}

fn lower_lvalue(
    ast: &ResolvedAST,
    lvalue_ref: ExprRef,
    func_builder: &mut FunctionBuilder,
    stack_frame: &Frame,
    lower_fn_ctx: &mut LowerFunctionContext,
) -> Value {
    let lvalue_expr = &ast.exprs[lvalue_ref];
    match lvalue_expr {
        TypedExpressionNode::ObjectIdentifier(_, object_idx) => {
            assert!(
                stack_frame.contains(*object_idx),
                "only local variables supported for now"
            );

            let stack_slot = stack_frame.get_object_stack_slot(*object_idx);
            func_builder.insert().stack_addr(stack_slot)
        }
        TypedExpressionNode::Dereference(_, ptr) => {
            lower_expr(ast, *ptr, func_builder, stack_frame, lower_fn_ctx)
        }
        TypedExpressionNode::DotAccess(_, accessee, member) => {
            let location = lower_lvalue(ast, *accessee, func_builder, stack_frame, lower_fn_ctx);
            let accessee_type = ast.exprs[*accessee].expr_type();

            assert!(matches!(accessee_type, CType::StructureTypeRef { .. } | CType::UnionTypeRef { .. }));
            let offset = match accessee_type.as_struct() {
                Some(struct_ref) => ast.layouts.get_struct_member_offset(struct_ref, *member),
                None => 0 // must be union
            };

            let offset_val = func_builder.insert().const_u64(offset as u64);
            func_builder.insert().add(location, offset_val)
        }
        TypedExpressionNode::ArrowAccess(_, accessee, member) => {
            let location = lower_expr(ast, *accessee, func_builder, stack_frame, lower_fn_ctx);
            let accessee_type = ast.exprs[*accessee].expr_type().as_pointee().unwrap();

            assert!(matches!(accessee_type, CType::StructureTypeRef { .. } | CType::UnionTypeRef { .. }));
            let offset = match accessee_type.as_struct() {
                Some(struct_ref) => ast.layouts.get_struct_member_offset(struct_ref, *member),
                None => 0 // must be union
            };

            let offset_val = func_builder.insert().const_u64(offset as u64);
            func_builder.insert().add(location, offset_val)
        }
        _ => todo!("other lvalues not supported yet"),
    }
}

#[cfg(test)]
mod test {
    use crate::{
        cir,
        semantics::resolver::resolve_ast_tests::{ResolveHarnessInput, resolve_harness},
    };

    #[test]
    fn test_basic() {
        let code = r#"
        int main() {
            return 5 + 3;
        }
        "#;

        let input = ResolveHarnessInput { code };
        let resolved = resolve_harness(input);
        let module = cir::ast2cir::lower_ast(resolved);

        dbg!(module);
    }

    #[test]
    fn test_compile_expr() {
        let code = r#"
        int main(int argc, char *argv[]) {
            return 5 + 3 * 2 / (6 % 5);
        }
        "#;

        let input = ResolveHarnessInput { code };
        let resolved = resolve_harness(input);
        let module = cir::ast2cir::lower_ast(resolved);

        dbg!(module);
    }

    #[test]
    fn test_compile_variables() {
        let code = r#"
        int main(int argc, char *argv[]) {
            int two;
            int three;
            two = 2;
            three = 3;
            return two + three;
        }
        "#;

        let input = ResolveHarnessInput { code };
        let resolved = resolve_harness(input);
        let module = cir::ast2cir::lower_ast(resolved);

        print!("{module}");
    }

    #[test]
    fn test_compile_function_call() {
        let code = r#"
        int square_three() {
            return 3 * 3;
        }

        int main(int argc, char *argv[]) {
            return square_three();
        }

        int hi() {
            return 1 && 3;
        }
        "#;

        let input = ResolveHarnessInput { code };
        let resolved = resolve_harness(input);
        let module = cir::ast2cir::lower_ast(resolved);

        dbg!(&module);
        print!("{module}");
    }
}
