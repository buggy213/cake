use crate::{cir::{FunctionBuilder, Module, Signature, Type}, semantics::{resolved_ast::{NodeRef, ResolvedASTNode}, resolver::ResolvedAST}, types::{BasicType, CType}};

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
            _ => todo!("other types")
        }
    }
}

pub(crate) fn lower_ast(ast: ResolvedAST) -> Module {
    let Some(ResolvedASTNode::TranslationUnit { children }) = ast.nodes.first() else {
        panic!("corrupted ast")
    };

    let mut module = Module::new();

    for func in &ast.ast_indices[children.0 as usize .. children.1 as usize] {
        let ResolvedASTNode::FunctionDefinition { symbol_idx, body, .. } = &ast.nodes[func.0 as usize] else {
            panic!("corrupted ast")
        };

        let ast_func = ast.symtab.get_function(*symbol_idx);
        let ast_func_type = ast.symtab.get_function_type(ast_func.function_type);
        let ast_func_args = &ast_func_type.parameter_types;
        let ast_func_args: Vec<CType> = ast_func_args.iter().map(|arg| arg.1.clone()).collect();
        
        let func_args: Vec<Type> = ast_func_args.iter().map(|arg| arg.try_into().unwrap()).collect();
        let func_ret = if let CType::Void { .. } = ast_func_type.return_type {
            None
        } else {
            Some((&ast_func_type.return_type).try_into().unwrap())
        };

        let func_sig = Signature::new(func_args, func_ret);
        let func_sig_ref = module.add_signature(func_sig);
        let func_ref = module.add_function(func_sig_ref);

        let func_builder = module.fn_builder(func_ref);
        
    }

    module
}

pub(crate) fn lower_function(ast: &ResolvedAST, fn_body: NodeRef, func_builder: &mut FunctionBuilder) {
    
}