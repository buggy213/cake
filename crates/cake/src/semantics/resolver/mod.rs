use resolve_decls::{resolve_declaration, resolve_empty_declaration, resolve_function_definition};
use resolve_exprs::{ResolveExprError, resolve_expr, resolve_integer_constant_expression};
use std::mem::MaybeUninit;
use thiserror::Error;

use super::symtab::{Scope, SymbolTable, SymtabError};
use crate::parser::hand_parser::ParserState;
use crate::semantics::resolved_ast::{
    ContextRef, ExprRef, NodeRangeRef, NodeRef, ResolvedASTNode, TypedExpressionNode,
};
use crate::semantics::symtab::{Function, ScopedSymtab};
use crate::types::{EnumType, FunctionType, FunctionTypeIdx, StructureType, UnionType};
use crate::{
    parser::ast::{ASTNode, Constant, ExpressionNode},
    semantics::symtab::ScopeType,
    types::{BasicType, CType, TypeQualifier},
};

#[derive(Debug, Error)]
pub(crate) enum ASTResolveError {
    #[error("function declaration must have storage class of extern or static")]
    BadFunctionStorageClass,
    #[error("symbol redeclaration error")]
    SymbolRedeclaration,
    #[error("function redefinition error")]
    FunctionRedefinition,
    #[error("function has internal / external linkage in different declarations")]
    InconsistentFunctionLinkage,
    #[error("symtab error")]
    SymtabError(#[from] SymtabError),
    #[error("declaration cannot have incomplete type")]
    IncompleteDeclaration,
    #[error("redefined enum / union / struct tag")]
    TagRedefinition,
    #[error("parser type table corrupted")]
    CorruptedParserTypeTable,
    #[error("symtab type table corrupted")]
    CorruptedSymtabTypeTable,
    #[error("restrict qualifier only applies to pointers")]
    BadRestrictQualifier,
    #[error("object type is required")]
    ObjectTypeRequired,
    #[error("function return type is incomplete")]
    IncompleteReturnType,
    #[error("redeclared label name within same function")]
    RedeclaredLabel(#[source] SymtabError),
    #[error("error while evaluating compile-time integer constant")]
    IntegerConstantExprError,
    #[error("case labels only allowed within switch statements")]
    UnexpectedCaseLabel,
    #[error("only one default case within a switch statement")]
    MultipleDefaultLabels,
    #[error("duplicate case label")]
    DuplicateCaseLabel,
    #[error("goto target label doesn't exist in current function")]
    BadGotoTarget,
    #[error("continue statement not within for/while/do-while")]
    BadContinue,
    #[error("break statement not within switch/for/while/do-while")]
    BadBreak,
    #[error("declaration conflicts with existing declaration")]
    ConflictingDeclaration,
    #[error("function definition contains parameter with incomplete type")]
    IncompleteParameter,
    #[error("function declarations are incompatible")]
    IncompatibleFunctionDeclarations,
    #[error("error while resolving expression")]
    ExprResolveError(#[from] ResolveExprError),
    #[error("controlling expression expr type (if/switch/while/for)")]
    BadControllingExprType,
    #[error("bad return value")]
    BadReturnValue,
}

#[derive(Debug, PartialEq)]
pub(crate) struct ResolvedAST {
    pub(crate) nodes: Vec<ResolvedASTNode>,
    pub(crate) ast_indices: Vec<NodeRef>,
    pub(crate) exprs: Vec<TypedExpressionNode>,
    pub(crate) expr_indices: Vec<ExprRef>,
    pub(crate) symtab: SymbolTable,
}

// during postorder walk, need to have uninitialized values
// i.e. "reserve" space in nodes / exprs Vec to have something to point to,
// even though they won't be filled in before their children
struct IntermediateAST {
    nodes: Vec<MaybeUninit<ResolvedASTNode>>,
    ast_indices: Vec<NodeRef>,
    exprs: Vec<TypedExpressionNode>,
    expr_indices: Vec<ExprRef>,
}

impl ResolvedAST {
    // Precondition: every element of intermediate.nodes / intermediate.exprs is initialized
    unsafe fn from_intermediate_ast(
        intermediate: IntermediateAST,
        resolve_state: ResolverState,
    ) -> ResolvedAST {
        let IntermediateAST {
            nodes,
            ast_indices,
            exprs,
            expr_indices,
        } = intermediate;

        let ResolverState { scoped_symtab, .. } = resolve_state;

        // relies on "in-place collect" optimization for efficiency
        // MaybeUninit<T> is guaranteed to have same size and alignment as T
        let nodes: Vec<_> = nodes.into_iter().map(|n| n.assume_init()).collect();

        ResolvedAST {
            nodes,
            ast_indices,
            exprs,
            expr_indices,
            symtab: SymbolTable::from_scoped_symtab(scoped_symtab),
        }
    }
}

// Needed for tracking target of case / default labels, break statements
struct SwitchStatementContext {
    node: NodeRef, // node in AST that this switch statement corresponds to
    enclosing_context: Option<usize>,

    value_type: BasicType, // type of controlling expression, case values are promoted to it
    case_values: Vec<Constant>,
    has_default: bool,
}

// Needed for tracking target of break / continue statements (for, while, do-while loops)
struct IterationStatementContext {
    node: NodeRef, // node in AST that this while statement corresponds to
    enclosing_context: Option<usize>,
}

// Needed for typechecking return statements
struct FunctionDefinitionContext {
    node: NodeRef,

    func_type: FunctionTypeIdx,
}

enum ResolverContext {
    Switch(SwitchStatementContext),
    Iteration(IterationStatementContext),
    Function(FunctionDefinitionContext),
}

impl ResolverContext {
    fn enclosing_context(&self) -> Option<usize> {
        match self {
            ResolverContext::Switch(switch_statement_context) => {
                switch_statement_context.enclosing_context
            }
            ResolverContext::Iteration(iteration_statement_context) => {
                iteration_statement_context.enclosing_context
            }
            // functions cannot have enclosing context (maybe TODO - support nested functions?)
            ResolverContext::Function(_) => None,
        }
    }
}

struct ResolverState {
    pub(crate) scoped_symtab: ScopedSymtab,

    current_context: Option<usize>,
    context_stack: Vec<ResolverContext>,

    deferred_goto_resolve: Vec<NodeRef>,
}

impl ResolverState {
    fn new(scopes: Vec<Scope>) -> ResolverState {
        ResolverState {
            scoped_symtab: ScopedSymtab::new_with_scopes(scopes),
            current_context: None,
            context_stack: Vec::new(),
            deferred_goto_resolve: Vec::new(),
        }
    }

    fn current_context(&self) -> Option<&ResolverContext> {
        self.current_context
            .and_then(|idx| self.context_stack.get(idx))
    }

    fn add_switch(&mut self, value_type: BasicType, node: NodeRef) {
        let new_switch = SwitchStatementContext {
            node,
            enclosing_context: self.current_context,

            value_type,
            case_values: Default::default(),
            has_default: false,
        };
        let new_switch_idx = self.context_stack.len();
        self.context_stack.push(ResolverContext::Switch(new_switch));
        self.current_context = Some(new_switch_idx);
    }

    // Get index of the lexically closest switch statement context
    fn current_switch_idx(&self) -> Option<usize> {
        let mut idx = self.current_context;
        while let Some(current_idx) = idx {
            match &self.context_stack[current_idx] {
                ResolverContext::Switch(_) => return Some(current_idx),
                // continue searching in enclosing context
                ctx => idx = ctx.enclosing_context(),
            }
        }

        None
    }

    // Get reference to lexically closest switch statement context
    fn current_switch(&self) -> Option<&SwitchStatementContext> {
        self.current_switch_idx()
            .and_then(|idx| match self.context_stack.get(idx) {
                Some(ResolverContext::Switch(switch)) => Some(switch),
                Some(_) => unreachable!("current_switch_idx should only return switch contexts"),
                None => None,
            })
    }

    // Get mutable reference to lexically closest switch statement context
    fn current_switch_mut(&mut self) -> Option<&mut SwitchStatementContext> {
        self.current_switch_idx()
            .and_then(|idx| match self.context_stack.get_mut(idx) {
                Some(ResolverContext::Switch(switch)) => Some(switch),
                Some(_) => unreachable!("current_switch_idx should only return switch contexts"),
                None => None,
            })
    }

    // Precondition: current context is a switch statement
    fn close_switch(&mut self) {
        match self.current_context() {
            Some(ResolverContext::Switch(switch)) => {
                // pop switch context
                self.current_context = switch.enclosing_context;
            }
            _ => {
                // Internal compiler error
                panic!("Cannot close switch statement when current context is not a switch");
            }
        }
    }

    fn add_iterstmt(&mut self, node: NodeRef) {
        let new_iterstmt = IterationStatementContext {
            node,
            enclosing_context: self.current_context,
        };
        let new_iterstmt_idx = self.context_stack.len();
        self.context_stack
            .push(ResolverContext::Iteration(new_iterstmt));
        self.current_context = Some(new_iterstmt_idx);
    }

    // Get index of the lexically closest iteration statement context
    fn current_iterstmt_idx(&self) -> Option<usize> {
        let mut idx = self.current_context;
        while let Some(current_idx) = idx {
            match &self.context_stack[current_idx] {
                ResolverContext::Iteration(_) => return Some(current_idx),
                ctx => idx = ctx.enclosing_context(),
            }
        }

        None
    }

    // Get reference to lexically closest switch statement context
    fn current_iterstmt(&self) -> Option<&IterationStatementContext> {
        self.current_iterstmt_idx()
            .and_then(|idx| match self.context_stack.get(idx) {
                Some(ResolverContext::Iteration(iterstmt)) => Some(iterstmt),
                Some(_) => {
                    unreachable!("current_iterstmt_idx should only return iterstmt contexts")
                }
                None => None,
            })
    }

    // Get mutable reference to lexically closest switch statement context
    fn current_iterstmt_mut(&mut self) -> Option<&mut IterationStatementContext> {
        self.current_iterstmt_idx()
            .and_then(|idx| match self.context_stack.get_mut(idx) {
                Some(ResolverContext::Iteration(iterstmt)) => Some(iterstmt),
                Some(_) => {
                    unreachable!("current_iterstmt_idx should only return iterstmt contexts")
                }
                None => None,
            })
    }

    // Precondition: current context is an iteration statement
    fn close_iterstmt(&mut self) {
        match self.current_context() {
            Some(ResolverContext::Iteration(iterstmt)) => {
                // pop context
                self.current_context = iterstmt.enclosing_context
            }
            _ => {
                // Internal compiler error
                panic!(
                    "Cannot close iteration statement when current context is not an iteration statement"
                );
            }
        }
    }

    fn add_function_defn(&mut self, node: NodeRef, func_type: FunctionTypeIdx) {
        let new_function_definition_ctx = FunctionDefinitionContext { node, func_type };
        let idx = self.context_stack.len();
        self.context_stack
            .push(ResolverContext::Function(new_function_definition_ctx));
        self.current_context = Some(idx);
    }

    fn current_function_defn_idx(&self) -> Option<usize> {
        let mut idx = self.current_context;
        while let Some(current_idx) = idx {
            match &self.context_stack[current_idx] {
                ResolverContext::Function(_) => return Some(current_idx),
                ctx => idx = ctx.enclosing_context(),
            }
        }

        None
    }

    // Get reference to lexically closest switch statement context
    fn current_function_defn(&self) -> Option<&FunctionDefinitionContext> {
        self.current_function_defn_idx()
            .and_then(|idx| match self.context_stack.get(idx) {
                Some(ResolverContext::Function(iterstmt)) => Some(iterstmt),
                Some(_) => unreachable!(
                    "current_function_defn_idx should only return function defn contexts"
                ),
                None => None,
            })
    }

    fn close_function_defn(&mut self) {
        match self.current_context() {
            Some(ResolverContext::Function(_)) => {
                // pop context (function has no surrounding context, for now)
                self.current_context = None
            }
            _ => {
                // Internal compiler error
                panic!(
                    "Cannot close function definition when top of stack is not function definition"
                );
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct ParserTypes<'p> {
    enum_types: &'p [EnumType],
    structure_types: &'p [StructureType],
    union_types: &'p [UnionType],
    function_types: &'p [FunctionType],
}

/// 1. resolve declarations (place values into symbol table and enforce rules about redeclaration)
/// 2. resolve identifiers (i.e. check that there is a corresponding definition) within expressions
///    - TODO: consider using numeric indices rather than string-based hashtable lookup for everything
/// 3. perform type checking for expressions
/// 4. evaluate compile time constants
/// goal: by the end of `resolve_ast`, the code is guaranteed to be free of compilation (though maybe not link-time) errors
/// resolve_ast also checks internal compiler invariants
pub fn resolve_ast(
    ast_root: ASTNode,
    parser_state: ParserState,
) -> Result<ResolvedAST, ASTResolveError> {
    let ParserState {
        scopes,
        enum_types,
        structure_types,
        union_types,
        function_types,
        ..
    } = parser_state;

    let mut intermediate_ast = IntermediateAST {
        nodes: Vec::new(),
        ast_indices: Vec::new(),
        exprs: Vec::new(),
        expr_indices: Vec::new(),
    };

    let mut resolver_state = ResolverState::new(scopes);
    let parser_types = ParserTypes {
        enum_types: &enum_types,
        structure_types: &structure_types,
        union_types: &union_types,
        function_types: &function_types,
    };

    resolve_ast_top(
        &mut intermediate_ast,
        &ast_root,
        parser_types,
        &mut resolver_state,
    )?;

    // SAFETY: we initialize all entries of resolved AST
    let resolved_ast =
        unsafe { ResolvedAST::from_intermediate_ast(intermediate_ast, resolver_state) };

    Ok(resolved_ast)
}

// Helper functions for manipulating "linear" AST
fn insert_placeholder(nodes: &mut Vec<MaybeUninit<ResolvedASTNode>>) -> (usize, NodeRef) {
    nodes.push(MaybeUninit::uninit());
    let node_idx = nodes.len() - 1;
    let node_ref = NodeRef(node_idx as u32);

    (node_idx, node_ref)
}

fn add_indices(
    indices: &mut Vec<NodeRef>,
    new_indices: impl Iterator<Item = NodeRef>,
) -> NodeRangeRef {
    let start = indices.len();
    indices.extend(new_indices);
    let end = indices.len();

    NodeRangeRef(start as u32, end as u32)
}

// TODO: is there some way to obviate the need for 3 separate functions?
// using Option is annoying since the only case where a node isn't put into AST is for declarations
// maybe we should just keep declarations around in resolved AST too?
fn resolve_ast_top(
    intermediate_ast: &mut IntermediateAST,
    ast: &ASTNode,
    parser_types: ParserTypes,
    resolve_state: &mut ResolverState,
) -> Result<NodeRef, ASTResolveError> {
    match ast {
        ASTNode::TranslationUnit(definitions, scope) => {
            assert_eq!(
                scope.scope_type,
                ScopeType::FileScope,
                "Translation unit must be at file scope"
            );
            assert_eq!(
                scope.parent_scope, None,
                "Translation unit must be top scope"
            );

            let (node_idx, node_ref) = insert_placeholder(&mut intermediate_ast.nodes);

            let mut children = Vec::new();
            for defn in definitions {
                match defn {
                    ASTNode::Declaration(_) | ASTNode::EmptyDeclaration(_, _) => {
                        resolve_ast_declaration(
                            node_ref,
                            intermediate_ast,
                            defn,
                            parser_types,
                            resolve_state,
                        )?
                    }
                    _ => {
                        let child_ref = resolve_ast_inner(
                            node_ref,
                            intermediate_ast,
                            defn,
                            parser_types,
                            resolve_state,
                        )?;

                        children.push(child_ref);
                    }
                }
            }

            let range = add_indices(&mut intermediate_ast.ast_indices, children.into_iter());

            let translation_unit_node = ResolvedASTNode::TranslationUnit { children: range };
            intermediate_ast.nodes[node_idx].write(translation_unit_node);

            Ok(node_ref)
        }

        _ => {
            unreachable!("Should only be called on top node");
        }
    }
}

fn resolve_ast_inner(
    parent: NodeRef,
    intermediate_ast: &mut IntermediateAST,
    ast: &ASTNode,
    parser_types: ParserTypes,
    resolve_state: &mut ResolverState,
) -> Result<NodeRef, ASTResolveError> {
    match ast {
        ASTNode::TranslationUnit(_, _) => unreachable!("Must call resolve_ast_top"),
        ASTNode::FunctionDefinition(fn_declaration, body) => {
            let begin_token = resolve_state.scoped_symtab.begin_function_definition();
            let func_idx = resolve_function_definition(
                &mut resolve_state.scoped_symtab,
                &fn_declaration,
                parser_types,
            )?;
            resolve_state
                .scoped_symtab
                .end_function_parameters(begin_token, func_idx);

            let func_type_idx = resolve_state
                .scoped_symtab
                .get_function(func_idx)
                .function_type;

            let (node_idx, node_ref) = insert_placeholder(&mut intermediate_ast.nodes);

            resolve_state.add_function_defn(node_ref, func_type_idx);
            let body_ref = resolve_ast_inner(
                node_ref,
                intermediate_ast,
                body,
                parser_types,
                resolve_state,
            )?;
            resolve_state.close_function_defn();

            // check that all goto statements in this function have a proper target
            for goto_node_ref in resolve_state.deferred_goto_resolve.iter().copied() {
                // SAFETY: children nodes of this function definition would necessarily be initialized
                let goto_node =
                    unsafe { intermediate_ast.nodes[goto_node_ref.0 as usize].assume_init_ref() };

                let goto_target = match goto_node {
                    ResolvedASTNode::GotoStatement { target, .. } => target,
                    _ => unreachable!("should only be goto node"),
                };

                resolve_state
                    .scoped_symtab
                    .lookup_label(goto_target.scope, &goto_target.name)
                    .ok_or(ASTResolveError::BadGotoTarget)?;
            }
            resolve_state.deferred_goto_resolve.clear();

            let fn_definition_node = ResolvedASTNode::FunctionDefinition {
                parent,
                symbol_idx: func_idx,
                body: body_ref,
            };
            intermediate_ast.nodes[node_idx].write(fn_definition_node);
            resolve_state
                .scoped_symtab
                .end_function_definition(begin_token, func_idx);
            Ok(node_ref)
        }
        ASTNode::Declaration(declaration_list) => unreachable!("call resolve_ast_declaration"),
        ASTNode::EmptyDeclaration(declared_type, scope) => {
            unreachable!("call resolve_ast_declaration")
        }
        ASTNode::Label(labelee, ident) => {
            let (node_idx, node_ref) = insert_placeholder(&mut intermediate_ast.nodes);

            let labelee_node = resolve_ast_inner(
                node_ref,
                intermediate_ast,
                labelee,
                parser_types,
                resolve_state,
            )?;

            // Prevent duplicate labels in same function
            match resolve_state.scoped_symtab.add_label(
                ident.scope,
                ident.name.clone(),
                labelee_node,
            ) {
                Err(e @ SymtabError::LabelAlreadyDeclared(_)) => {
                    return Err(ASTResolveError::RedeclaredLabel(e));
                }
                Err(e) => return Err(ASTResolveError::SymtabError(e)),

                _ => (),
            }

            let label_node = ResolvedASTNode::Label {
                parent,
                labelee: labelee_node,
            };
            intermediate_ast.nodes[node_idx].write(label_node);

            Ok(node_ref)
        }
        ASTNode::CaseLabel(labelee, case_value) => {
            let current_switch_idx = resolve_state
                .current_switch_idx()
                .ok_or(ASTResolveError::UnexpectedCaseLabel)?;

            let value_type = resolve_state
                .current_switch()
                .expect("guarded by current_switch_idx")
                .value_type;

            let value_expr: &ExpressionNode = &(**case_value);

            let value =
                resolve_integer_constant_expression(&mut resolve_state.scoped_symtab, value_expr)?;

            let case_expr = TypedExpressionNode::Constant(
                CType::BasicType {
                    qualifier: TypeQualifier::empty(),
                    basic_type: value_type,
                },
                value,
            );

            let expr_ref = ExprRef::from_push(&mut intermediate_ast.exprs, case_expr);

            let current_switch = match resolve_state.current_switch_mut() {
                Some(switch) => switch,
                None => return Err(ASTResolveError::UnexpectedCaseLabel),
            };

            // prevent duplicate case labels
            if current_switch.case_values.contains(&value) {
                return Err(ASTResolveError::DuplicateCaseLabel);
            }

            current_switch.case_values.push(value);

            let (node_idx, node_ref) = insert_placeholder(&mut intermediate_ast.nodes);
            let labelee_ref = resolve_ast_inner(
                node_ref,
                intermediate_ast,
                labelee,
                parser_types,
                resolve_state,
            )?;

            let current_switch_ref = ContextRef(current_switch_idx as u32);

            let case_label_node = ResolvedASTNode::CaseLabel {
                parent,
                labelee: labelee_ref,
                case_value: expr_ref,
            };
            intermediate_ast.nodes[node_idx].write(case_label_node);

            Ok(node_ref)
        }
        ASTNode::DefaultLabel(labelee) => match resolve_state.current_switch_mut() {
            Some(SwitchStatementContext { has_default, .. }) => {
                if *has_default {
                    return Err(ASTResolveError::MultipleDefaultLabels);
                }

                *has_default = true;

                let (node_idx, node_ref) = insert_placeholder(&mut intermediate_ast.nodes);
                let labelee_ref = resolve_ast_inner(
                    node_ref,
                    intermediate_ast,
                    labelee,
                    parser_types,
                    resolve_state,
                )?;
                let default_label_node = ResolvedASTNode::DefaultLabel {
                    parent,
                    labelee: labelee_ref,
                };
                intermediate_ast.nodes[node_idx].write(default_label_node);

                Ok(node_ref)
            }
            None => Err(ASTResolveError::UnexpectedCaseLabel),
        },
        ASTNode::CompoundStatement(inner_statements, scope) => {
            let (node_idx, node_ref) = insert_placeholder(&mut intermediate_ast.nodes);

            let mut children = Vec::new();
            for stmt in inner_statements {
                match stmt {
                    ASTNode::Declaration(_) | ASTNode::EmptyDeclaration(_, _) => {
                        resolve_ast_declaration(
                            node_ref,
                            intermediate_ast,
                            stmt,
                            parser_types,
                            resolve_state,
                        )?
                    }
                    _ => {
                        let child_ref = resolve_ast_inner(
                            node_ref,
                            intermediate_ast,
                            stmt,
                            parser_types,
                            resolve_state,
                        )?;

                        children.push(child_ref);
                    }
                }
            }

            let range = add_indices(&mut intermediate_ast.ast_indices, children.into_iter());

            let compound_statement_node = ResolvedASTNode::CompoundStatement {
                parent,
                stmts: range,
            };
            intermediate_ast.nodes[node_idx].write(compound_statement_node);

            Ok(node_ref)
        }
        ASTNode::ExpressionStatement(expr, scope) => {
            let expr_ref = resolve_expr(
                expr,
                &mut intermediate_ast.exprs,
                &mut intermediate_ast.expr_indices,
                &resolve_state.scoped_symtab,
            )?;
            let expr_node = ResolvedASTNode::ExpressionStatement {
                parent,
                expr: expr_ref,
            };
            let expr_node_ref = NodeRef(intermediate_ast.nodes.len() as u32);
            intermediate_ast.nodes.push(MaybeUninit::new(expr_node));
            Ok(expr_node_ref)
        }
        ASTNode::NullStatement => {
            let null_statement_node = ResolvedASTNode::NullStatement { parent };
            intermediate_ast
                .nodes
                .push(MaybeUninit::new(null_statement_node));
            let node_idx = intermediate_ast.nodes.len() - 1;
            let node_ref = NodeRef(node_idx as u32);

            Ok(node_ref)
        }
        ASTNode::IfStatement(controlling_expr, body, else_body, scope) => {
            let controlling_expr_ref = resolve_expr(
                controlling_expr,
                &mut intermediate_ast.exprs,
                &mut intermediate_ast.expr_indices,
                &resolve_state.scoped_symtab,
            )?;

            let controlling_expr_type = intermediate_ast.exprs[controlling_expr_ref].expr_type();

            if !controlling_expr_type.is_scalar_type() {
                return Err(ASTResolveError::BadControllingExprType);
            }

            let (node_idx, node_ref) = insert_placeholder(&mut intermediate_ast.nodes);

            let taken_ref = resolve_ast_inner(
                node_ref,
                intermediate_ast,
                body,
                parser_types,
                resolve_state,
            )?;

            let not_taken_ref = else_body
                .as_ref()
                .map(|else_body| {
                    resolve_ast_inner(
                        node_ref,
                        intermediate_ast,
                        else_body,
                        parser_types,
                        resolve_state,
                    )
                })
                .transpose()?;

            let if_statement_node = ResolvedASTNode::IfStatement {
                parent,
                condition: controlling_expr_ref,
                taken: taken_ref,
                not_taken: not_taken_ref,
            };
            intermediate_ast.nodes[node_idx].write(if_statement_node);

            Ok(node_ref)
        }
        ASTNode::SwitchStatement(controlling_expr, body, scope) => {
            let controlling_expr_ref = resolve_expr(
                &controlling_expr,
                &mut intermediate_ast.exprs,
                &mut intermediate_ast.expr_indices,
                &resolve_state.scoped_symtab,
            )?;

            let controlling_expr_type = intermediate_ast.exprs[controlling_expr_ref].expr_type();

            if !matches!(controlling_expr_type, CType::BasicType { .. }) {
                return Err(ASTResolveError::BadControllingExprType);
            }

            let value_type = todo!();
            let (node_idx, node_ref) = insert_placeholder(&mut intermediate_ast.nodes);

            resolve_state.add_switch(value_type, node_ref);
            let context_idx = resolve_state
                .current_switch_idx()
                .expect("added a switch above");
            let context_ref = ContextRef(context_idx as u32);

            let body_ref = resolve_ast_inner(
                node_ref,
                intermediate_ast,
                body,
                parser_types,
                resolve_state,
            )?;

            resolve_state.close_switch();

            let switch_statement_node = ResolvedASTNode::SwitchStatement {
                parent,
                controlling_expr: controlling_expr_ref,
                body: body_ref,
                context: context_ref,
            };
            intermediate_ast.nodes[node_idx].write(switch_statement_node);

            Ok(node_ref)
        }
        ASTNode::WhileStatement(controlling_expr, body, scope)
        | ASTNode::DoWhileStatement(controlling_expr, body, scope) => {
            let controlling_expr_ref = resolve_expr(
                &controlling_expr,
                &mut intermediate_ast.exprs,
                &mut intermediate_ast.expr_indices,
                &resolve_state.scoped_symtab,
            )?;

            let controlling_expr_type = intermediate_ast.exprs[controlling_expr_ref].expr_type();
            if !controlling_expr_type.is_scalar_type() {
                return Err(ASTResolveError::BadControllingExprType);
            }

            let (node_idx, node_ref) = insert_placeholder(&mut intermediate_ast.nodes);

            resolve_state.add_iterstmt(node_ref);
            let body_ref = resolve_ast_inner(
                node_ref,
                intermediate_ast,
                body,
                parser_types,
                resolve_state,
            )?;
            resolve_state.close_iterstmt();

            let while_statement_node = if matches!(ast, ASTNode::WhileStatement(_, _, _)) {
                ResolvedASTNode::WhileStatement {
                    parent,
                    condition: controlling_expr_ref,
                    body: body_ref,
                }
            } else {
                ResolvedASTNode::DoWhileStatement {
                    parent,
                    condition: controlling_expr_ref,
                    body: body_ref,
                }
            };
            intermediate_ast.nodes[node_idx].write(while_statement_node);

            Ok(node_ref)
        }
        ASTNode::ForStatement(decl_or_expr, condition, post_loop, body, scope) => {
            let (node_idx, node_ref) = insert_placeholder(&mut intermediate_ast.nodes);

            let first_clause = match decl_or_expr {
                None => None,
                Some(decl) if matches!(&**decl, ASTNode::Declaration(_)) => {
                    resolve_ast_declaration(
                        node_ref,
                        intermediate_ast,
                        decl,
                        parser_types,
                        resolve_state,
                    )?;
                    None
                }
                Some(expr) if matches!(&**expr, ASTNode::ExpressionStatement(_, _)) => {
                    let expr_ref = resolve_ast_inner(
                        node_ref,
                        intermediate_ast,
                        expr,
                        parser_types,
                        resolve_state,
                    )?;

                    Some(expr_ref)
                }
                Some(_) => unreachable!("prohibited by grammar"),
            };

            let second_clause: Option<ExprRef> = match condition {
                None => None,
                Some(expr) => {
                    todo!("resolve expr");
                }
            };

            let third_clause: Option<ExprRef> = match post_loop {
                None => None,
                Some(expr) => {
                    todo!("resolve expr");
                }
            };

            resolve_state.add_iterstmt(node_ref);
            let body_ref = resolve_ast_inner(
                node_ref,
                intermediate_ast,
                body,
                parser_types,
                resolve_state,
            )?;
            resolve_state.close_iterstmt();

            let for_statement_node = ResolvedASTNode::ForStatement {
                parent,
                init: first_clause,
                condition: second_clause,
                post_body: third_clause,
                body: body_ref,
            };
            intermediate_ast.nodes[node_idx].write(for_statement_node);

            Ok(node_ref)
        }
        ASTNode::GotoStatement(label_ident) => {
            let goto_statement_node = ResolvedASTNode::GotoStatement {
                parent,
                target: label_ident.clone(),
            };

            intermediate_ast
                .nodes
                .push(MaybeUninit::new(goto_statement_node));
            let node_idx = intermediate_ast.nodes.len() - 1;
            let node_ref = NodeRef(node_idx as u32);

            // add deferred check that label exists, evaluate once function definition body is resolved
            resolve_state.deferred_goto_resolve.push(node_ref);

            Ok(node_ref)
        }
        ASTNode::ContinueStatement => {
            let continue_target_ref = resolve_state
                .current_iterstmt()
                .ok_or(ASTResolveError::BadContinue)?
                .node;

            let continue_statement_node = ResolvedASTNode::ContinueStatement {
                parent,
                target: continue_target_ref,
            };

            intermediate_ast
                .nodes
                .push(MaybeUninit::new(continue_statement_node));
            let node_idx = intermediate_ast.nodes.len() - 1;
            let node_ref = NodeRef(node_idx as u32);
            Ok(node_ref)
        }
        ASTNode::BreakStatement => {
            let break_target = resolve_state
                .current_context()
                .ok_or(ASTResolveError::BadBreak)?;

            let break_target_ref = match break_target {
                ResolverContext::Switch(switch_statement_context) => switch_statement_context.node,
                ResolverContext::Iteration(iteration_statement_context) => {
                    iteration_statement_context.node
                }
                ResolverContext::Function(_) => return Err(ASTResolveError::BadBreak),
            };

            let break_statement_node = ResolvedASTNode::BreakStatement {
                parent,
                target: break_target_ref,
            };
            intermediate_ast
                .nodes
                .push(MaybeUninit::new(break_statement_node));
            let node_idx = intermediate_ast.nodes.len() - 1;
            let node_ref = NodeRef(node_idx as u32);

            Ok(node_ref)
        }
        ASTNode::ReturnStatement(expr, scope) => {
            // type check if it matches function signature
            let current_fn = resolve_state.current_function_defn()
                .expect("internal compiler error: grammar requires return can only be in function definition");

            let current_fn_type = resolve_state
                .scoped_symtab
                .get_function_type(current_fn.func_type);
            let current_fn_return_type = &current_fn_type.return_type;

            // do type-check
            let return_value = match expr {
                Some(expr) => {
                    let expr_ref = resolve_expr(
                        expr,
                        &mut intermediate_ast.exprs,
                        &mut intermediate_ast.expr_indices,
                        &resolve_state.scoped_symtab,
                    )?;
                    let expr_type = intermediate_ast.exprs[expr_ref].expr_type();

                    // qualifiers don't matter here, i think
                    // technically, this is not compliant to standard i think
                    if !CType::unqualified_equal(current_fn_return_type, expr_type) {
                        dbg!(current_fn_return_type);
                        dbg!(expr_type);
                        return Err(ASTResolveError::BadReturnValue);
                    }

                    Some(expr_ref)
                }
                None => {
                    if !current_fn_return_type.is_void() {
                        return Err(ASTResolveError::BadReturnValue);
                    }

                    None
                }
            };

            let return_statement_node = ResolvedASTNode::ReturnStatement {
                parent,
                return_value,
            };
            intermediate_ast
                .nodes
                .push(MaybeUninit::new(return_statement_node));
            let node_idx = intermediate_ast.nodes.len() - 1;
            let node_ref = NodeRef(node_idx as u32);

            Ok(node_ref)
        }
    }
}

fn resolve_ast_declaration(
    parent: NodeRef,
    intermediate_ast: &mut IntermediateAST,
    ast: &ASTNode,
    parser_types: ParserTypes,
    resolve_state: &mut ResolverState,
) -> Result<(), ASTResolveError> {
    if let ASTNode::Declaration(declaration_list) = ast {
        for decl in declaration_list {
            resolve_declaration(&mut resolve_state.scoped_symtab, decl, parser_types)?;
        }
    } else if let ASTNode::EmptyDeclaration(declared_type, scope) = ast {
        resolve_empty_declaration(
            &mut resolve_state.scoped_symtab,
            declared_type,
            *scope,
            parser_types,
        )?;
    } else {
        unreachable!("Must be called on ASTNode::Declaration")
    }

    Ok(())
}

mod resolve_decls;
mod resolve_exprs;

#[cfg(test)]
pub(crate) mod resolve_ast_tests {
    use crate::{
        parser::hand_parser::{CTokenStream, ParserState, parse_translation_unit},
        scanner::{lexeme_sets::c_lexemes::CLexemes, table_scanner::DFAScanner},
    };

    use super::{ResolvedAST, resolve_ast};

    pub(crate) struct ResolveHarnessInput {
        pub(crate) code: &'static str,
    }

    pub(crate) fn resolve_harness(input: ResolveHarnessInput) -> ResolvedAST {
        // parse code
        let scanner = DFAScanner::load_lexeme_set_scanner::<CLexemes>();
        let mut toks = CTokenStream::new(scanner, input.code.as_bytes());
        let mut state = ParserState::new();

        // resolve parsed input
        let parse_result =
            parse_translation_unit(&mut toks, &mut state).expect("unsuccessful parse");
        let resolve_result = resolve_ast(parse_result, state).expect("resolve unsuccessful");

        // compare
        dbg!(&resolve_result);

        resolve_result
    }

    fn resolve_test(input: ResolveHarnessInput /* expected: ??? */) {
        let resolved = resolve_harness(input);
    }

    #[test]
    fn resolve_file_scope_declarations_test() {
        let file_scope_declarations = r#"
        struct node { struct node* next; } first;
        struct node second;
        "#;

        let input = ResolveHarnessInput {
            code: file_scope_declarations,
        };

        // resolve_harness(input);

        let file_scope_declarations = r#"
        struct node *first_ptr;
        struct node { struct node *next; } first;
        struct node second;
        "#;

        let input = ResolveHarnessInput {
            code: file_scope_declarations,
        };

        // resolve_harness(input);

        let file_scope_declarations = r#"
        struct node_data {
            int num;
        };
        struct node { 
            struct node_data data;
            struct node *next; 
        } first;
        "#;

        let input = ResolveHarnessInput {
            code: file_scope_declarations,
        };

        resolve_harness(input);

        // this should resolve, since they are both in the same file scope
        let file_scope_declarations = r#"
        struct node { 
            struct node_data *data_ptr;
            struct node *next; 
        } first;
        struct node_data {
            int num;
        };
        "#;

        let input = ResolveHarnessInput {
            code: file_scope_declarations,
        };

        resolve_harness(input);
    }

    #[test]
    fn resolve_adjust_function_params_test() {
        let function_declaration = r#"
        void f(char *argv[], int q(char));
        "#;

        let input = ResolveHarnessInput {
            code: function_declaration,
        };

        resolve_harness(input);
    }

    #[test]
    fn resolve_arithmetic_exprs() {
        let arithmetic_exprs = r#"
        int main(int argc, char *argv[]) {
            7 + 15;
            7.0f + 12;
            3.0 + 1.0f;
        }
        "#;

        let input = ResolveHarnessInput {
            code: arithmetic_exprs,
        };
        resolve_harness(input);
    }

    #[test]
    fn resolve_hello() {
        let hello = r#"
        int printf(const char *format, ...);
        
        int main(int argc, char *argv[]) {
            printf("Hello world!");
        }
        "#;

        let input = ResolveHarnessInput { code: hello };

        resolve_harness(input);
    }
}
