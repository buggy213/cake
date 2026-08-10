use core::panic;
use std::{
    backtrace::Backtrace,
    collections::HashMap,
    num::{ParseFloatError, ParseIntError},
};
#[cfg(debug_assertions)]
use std::{error::Error, fmt::Display};

use thiserror::Error;

use crate::{
    parser::{ast::Constant, hand_parser::parse_expr::parse_assignment_expr}, scanner::{
        InterningTokenStream, lexeme_sets::c_lexemes::CLexemes, string_pool::StringPoolRef
    }, semantics::symtab::{Scope, ScopeType, StorageClass, SymtabError}, types::{
        EnumType, EnumTypeIdx, FunctionTypeIdx, StructureType, StructureTypeIdx, UnionType,
        UnionTypeIdx,
    },
};

use crate::types::{
    AggregateMember, BasicType, CType, FunctionArgument, FunctionSpecifier, FunctionType,
    TypeQualifier,
};

use super::ast::{ASTNode, Declaration, ExpressionNode, Identifier};

#[cfg(test)]
mod hand_parser_tests;

/// Eat a specific token, or generate an `UnexpectedToken` / `UnexpectedEOF` error
macro_rules! eat_or_error {
    ($toks:expr, $tok:path) => {
        match $toks.peek() {
            Some(($tok, _, _)) => {
                $toks.eat($tok);
                Ok(())
            }
            Some((other, _, _)) => Err(ParseError::UnexpectedToken(other)),
            None => Err(ParseError::UnexpectedEOF.into()),
        }
    };
}

/// Parse error augmented with a backtrace generated from the point that the error is generated
#[cfg(debug_assertions)]
#[derive(Debug)]
pub(crate) struct ParseErrorWithBacktrace {
    parse_error: ParseError,
    backtrace: Backtrace,
}

#[cfg(debug_assertions)]
impl From<ParseError> for Box<ParseErrorWithBacktrace> {
    fn from(value: ParseError) -> Self {
        Box::new(ParseErrorWithBacktrace {
            parse_error: value,
            backtrace: Backtrace::capture(),
        })
    }
}

#[cfg(debug_assertions)]
impl Display for ParseErrorWithBacktrace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\n{:?}\n", self.parse_error, self.backtrace)
    }
}

#[cfg(debug_assertions)]
impl Error for ParseErrorWithBacktrace {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(&self.parse_error)
    }
}

#[cfg(debug_assertions)]
pub(crate) type Result<T> = std::result::Result<T, Box<ParseErrorWithBacktrace>>;

#[cfg(not(debug_assertions))]
pub(crate) type Result<T> = std::result::Result<T, Box<ParseError>>;

#[derive(Error, Debug)]
pub(crate) enum ParseError {
    #[error("Unexpected end of file while parsing")]
    UnexpectedEOF,
    #[error("Unexpected token {0:?} while parsing")]
    UnexpectedToken(CLexemes),
    #[error("Only one storage class allowed in declaration")]
    UnexpectedStorageClass,
    #[error("failed to lookup item in symbol table")]
    LookupError(String),
    #[error("cannot declare enum `{0}` in same scope due to conflicting tag")]
    RedeclaredEnum(String),
    #[error("cannot declare struct `{0}` in same scope due to conflicting tag")]
    RedeclaredStruct(String),
    #[error("cannot declare union `{0}` in same scope due to conflicting tag")]
    RedeclaredUnion(String),
    #[error("cannot redeclare enum constant")]
    RedeclaredEnumConstant(#[source] SymtabError),
    #[error("invalid enum constant (must be integer constant)")]
    InvalidEnumConstant(String),
    #[error("empty enum is not allowed")]
    EmptyEnum,
    #[error("incomplete enum <tag> cannot be declared alongside struct <tag> or union <tag>")]
    EnumDeclarationMustMatch,
    #[error("incomplete struct or enum declaration with wrong type of tag")]
    StructOrEnumDeclarationMustMatch,
    #[error("indeterminate symtab error")]
    OtherSymtabError(#[from] SymtabError),
    #[error("problem while parsing declarator")]
    BadDeclarator,
    #[error("direct declarator requires a name")]
    DeclaratorRequiresName,
    #[error("abstract declarator should not have name")]
    UnexpectedDeclaratorName,
    #[error("error while parsing array bound")]
    BadArrayBound(#[source] ParseIntError),
    #[error("closed file scope while parsing")]
    ClosedFileScope,
    #[error("function specifier (inline) only allowed in function definitions")]
    BadFunctionSpecifier,
    #[error("only register storage class allowed in function argument list")]
    BadStorageClassInArgumentList,
    #[error("incomplete types (void, undefined structs, etc.) not allowed in function arguments")]
    IncompleteFunctionArgument,
    #[error("internal parsing error: canonical types should not be used here")]
    CanonicalTypeOutsideSymtab,
    #[error("unmatched parentheses in declarator")]
    UnmatchedParensInDeclarator,
    #[error("multiple types in declaration")]
    MultipleTypeSpecifiersInDeclaration,
    #[error("at least on type specifier required in declaration")]
    MissingTypeSpecifier,
    #[error("error while parsing basic type")]
    BasicTypeError,
    #[error(
        "member access (with '.' or '->' operators) expects identifier, not arbitrary expression"
    )]
    BadMemberAccess,
    #[error("couldn't parse int")]
    BadInt,
    #[error("couldn't parse float")]
    BadFloat(#[source] ParseFloatError),
    #[error("bad function definition")]
    BadFunctionDefinition,
    #[error("cannot crate array of incomplete type")]
    ArrayOfIncompleteType,
    #[error("function cannot return function / array")]
    BadFunctionReturnType,
    #[error("internal compiler error: bad string constant")]
    BadStringConst,

    #[error("redeclared typedef")]
    RedeclaredTypedef,

    #[error("unable to parse character const")]
    BadCharConst,
}

pub(crate) struct ParserState {
    // during parse, we build up a series of lexical scopes
    // these will get passed on to the resolve phase
    pub(crate) scopes: Vec<Scope>,
    current_scope: Scope,

    // similarly, we build up a series of "canonical types"
    // i.e. functions, structs, enums, and incomplete variants.
    // every declaration in the AST gets its own canonical type, and
    // the resolve phase is responsible for "deduplicating" them, i.e.
    // struct node { struct node *next; } a;
    // struct node b;
    // a and b should be recognized as the same type,
    // and next recognized as a pointer to that type.
    pub(crate) enum_types: Vec<EnumType>,
    pub(crate) structure_types: Vec<StructureType>,
    pub(crate) union_types: Vec<UnionType>,
    pub(crate) function_types: Vec<FunctionType>,

    // typedefs are defined and substituted directly during the parsing process
    typedefs: Vec<HashMap<StringPoolRef, CType>>,
}

impl ParserState {
    pub(crate) fn new() -> Self {
        let file_scope = Scope::new_file_scope();
        Self {
            scopes: vec![file_scope],
            current_scope: file_scope,

            enum_types: Vec::new(),
            structure_types: Vec::new(),
            union_types: Vec::new(),
            function_types: Vec::new(),

            typedefs: vec![Default::default()],
        }
    }

    fn open_scope(&mut self, scope_type: ScopeType) {
        let new_scope = Scope::new(scope_type, self.current_scope.index as usize, self.scopes.len());
        self.scopes.push(new_scope);
        self.typedefs.push(Default::default());
        self.current_scope = new_scope;
    }

    fn close_scope(&mut self) -> Result<()> {
        let parent_index = self
            .current_scope
            .parent_scope
            .ok_or(ParseError::ClosedFileScope)?;
        self.current_scope = self.scopes[parent_index as usize];

        Ok(())
    }

    fn add_enum_type(&mut self, enum_type: EnumType) -> EnumTypeIdx {
        EnumTypeIdx::from_push(&mut self.enum_types, enum_type)
    }

    fn add_structure_type(&mut self, structure_type: StructureType) -> StructureTypeIdx {
        StructureTypeIdx::from_push(&mut self.structure_types, structure_type)
    }

    fn add_union_type(&mut self, union_type: UnionType) -> UnionTypeIdx {
        UnionTypeIdx::from_push(&mut self.union_types, union_type)
    }

    fn add_function_type(&mut self, function_type: FunctionType) -> FunctionTypeIdx {
        FunctionTypeIdx::from_push(&mut self.function_types, function_type)
    }

    fn add_typedef(&mut self, name: StringPoolRef, typedef: CType) -> Result<()> {
        if self.get_typedef(name).is_some() {
            return Err(ParseError::RedeclaredTypedef.into());
        }

        self.typedefs[self.current_scope.index as usize].insert(name, typedef);
        Ok(())
    }

    /// Looks up typedefs with a given name, starting from the current scope
    /// and working up to top lexical scope (i.e. file scope)
    fn get_typedef(&self, name: StringPoolRef) -> Option<&CType> {
        let mut scope = self.current_scope;
        loop {
            if let Some(typedef) = self.typedefs[scope.index as usize].get(&name) {
                return Some(typedef);
            }

            if let Some(parent) = scope.parent_scope {
                scope = self.scopes[parent as usize];
            } else {
                return None;
            }
        }
    }
}

mod parse_expr;
use parse_expr::parse_expr;

// <translation-unit> ::= <external-declaration>
// | <translation-unit> <external-declaration>
pub(crate) fn parse_translation_unit<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<ASTNode> {
    let mut external_declarations = Vec::new();
    while let Some(_) = toks.peek() {
        external_declarations.push(parse_external_declaration(toks, state)?);
    }

    let translation_unit = ASTNode::TranslationUnit(external_declarations, state.current_scope);

    Ok(translation_unit)
}

// <external-declaration> ::= <function-definition>
// | <declaration>
// way to distinguish is that declarations end with a semicolon while function definitions
// have a compound statement
fn parse_external_declaration<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<ASTNode> {
    // both will include declaration specifiers
    let declaration_specifiers = parse_declaration_specifiers(toks, state)?;
    let DeclarationSpecifiers {
        qualified_type,
        storage_class,
        function_specifier,
        is_typedef,
    } = declaration_specifiers.clone();

    // handle empty declaration case
    if matches!(toks.peek(), Some((CLexemes::Semicolon, _, _))) {
        // type qualifier, storage class, function_specifier, typedef all have 0 effect on
        // empty declaration.
        eat_or_error!(toks, CLexemes::Semicolon)?;
        let empty_declaration = ASTNode::EmptyDeclaration(qualified_type, state.current_scope);
        return Ok(empty_declaration);
    }

    // both will include a (concrete) declarator
    let (declaration_type, name) = parse_declarator(toks, state, qualified_type)?;
    if is_typedef {
        state.add_typedef(name.clone(), declaration_type.clone())?;
    }

    let mut declarations: Vec<Declaration> = Vec::new();

    match toks.peek() {
        Some((CLexemes::LBrace, _, _)) => {
            // must be a function definition; declarator must be function type
            match declaration_type {
                CType::FunctionTypeRef { symtab_idx } => {
                    // set scope to function prototype scope so that we inherit parameter names
                    let fn_type = &state.function_types[symtab_idx];
                    state.current_scope = fn_type.prototype_scope;
                }
                _ => return Err(ParseError::BadFunctionDefinition.into()),
            }
            let function_body = parse_compound_statement(toks, state)?;
            // go back to file scope
            state.close_scope()?;

            let fn_declaration = Declaration::new(
                Identifier::new(state.current_scope, name),
                declaration_type,
                storage_class,
                is_typedef,
                function_specifier,
                None,
            );

            let function_node =
                ASTNode::FunctionDefinition(Box::new(fn_declaration), Box::new(function_body));

            return Ok(function_node);
        }

        // otherwise, must be a declaration
        // try to parse initializer; some duplicated logic from parse_init_declarators but
        // this is hard to avoid
        Some((CLexemes::Assign, _, _)) => {
            toks.eat(CLexemes::Assign);
            let initializer = parse_initializer(toks, state)?;
            let declaration = Declaration::new(
                Identifier::new(state.current_scope, name),
                declaration_type,
                storage_class,
                is_typedef,
                function_specifier,
                Some(Box::new(initializer)),
            );

            declarations.push(declaration)
        }
        Some((_, _, _)) => {
            let declaration = Declaration::new(
                Identifier::new(state.current_scope, name),
                declaration_type,
                storage_class,
                is_typedef,
                function_specifier,
                None,
            );
            declarations.push(declaration);
        }
        None => {
            return Err(ParseError::UnexpectedEOF.into());
        }
    }

    loop {
        match toks.peek() {
            Some((CLexemes::Comma, _, _)) => {
                toks.eat(CLexemes::Comma);
                let declaration =
                    parse_init_declarator(toks, state, declaration_specifiers.clone())?;
                declarations.push(declaration);
            }
            Some((CLexemes::Semicolon, _, _)) => {
                toks.eat(CLexemes::Semicolon);
                let decl_node = ASTNode::Declaration(declarations);
                return Ok(decl_node);
            }
            Some((other, _, _)) => {
                return Err(ParseError::UnexpectedToken(other).into());
            }
            None => {
                return Err(ParseError::UnexpectedEOF.into());
            }
        }
    }
}

fn parse_declaration<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<ASTNode> {
    let declaration_specifiers = parse_declaration_specifiers(toks, state)?;
    // handle empty declaration case, see parse_external_declaration

    if matches!(toks.peek(), Some((CLexemes::Semicolon, _, _))) {
        eat_or_error!(toks, CLexemes::Semicolon)?;
        let empty_declaration =
            ASTNode::EmptyDeclaration(declaration_specifiers.qualified_type, state.current_scope);
        return Ok(empty_declaration);
    }

    let res = parse_init_declarators(toks, state, declaration_specifiers);
    eat_or_error!(toks, CLexemes::Semicolon)?;

    res
}

// may need to change this interface later
// type names are used during cast expressions, as argument of sizeof, and in compound initializers
fn parse_type_name<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<CType> {
    let base_type = parse_specifier_qualifier_list(toks, state)?;
    let derived_type = parse_abstract_declarator(toks, state, base_type)?;
    Ok(derived_type)
}

fn is_lookahead_type_name<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>, 
    state: &ParserState
) -> bool {
    match toks.peek() {
        Some((lexeme, text, ident_ref)) => match lexeme {
            CLexemes::Const
            | CLexemes::Volatile
            | CLexemes::Restrict
            | CLexemes::Void
            | CLexemes::Char
            | CLexemes::Short
            | CLexemes::Int
            | CLexemes::Long
            | CLexemes::Float
            | CLexemes::Double
            | CLexemes::Unsigned
            | CLexemes::Signed
            | CLexemes::Struct
            | CLexemes::Union
            | CLexemes::Enum => true,
            CLexemes::Identifier => {
                state.get_typedef(ident_ref).is_some()
            }
            _ => false,
        },
        None => false,
    }
}

// distinguish between a declaration vs. expression
// will need to be careful with typedefs here
fn is_lookahead_declaration<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &ParserState,
) -> bool {
    match toks.peek() {
        Some((lexeme, text, ident_ref)) => match lexeme {
            CLexemes::Typedef
            | CLexemes::Extern
            | CLexemes::Static
            | CLexemes::Auto
            | CLexemes::Register
            | CLexemes::Const
            | CLexemes::Volatile
            | CLexemes::Restrict
            | CLexemes::Void
            | CLexemes::Char
            | CLexemes::Short
            | CLexemes::Int
            | CLexemes::Long
            | CLexemes::Float
            | CLexemes::Double
            | CLexemes::Unsigned
            | CLexemes::Signed
            | CLexemes::Struct
            | CLexemes::Union
            | CLexemes::Enum => true,
            CLexemes::Identifier => {
                state.get_typedef(ident_ref).is_some()
            }
            _ => false,
        },
        None => false,
    }
}

#[derive(Debug, Clone)]
struct DeclarationSpecifiers {
    qualified_type: CType,
    storage_class: StorageClass,
    function_specifier: FunctionSpecifier,
    is_typedef: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum BasicTypeLexeme {
    Void,
    Unsigned,
    Signed,
    Char,
    Short,
    Long,
    Float,
    Double,
    Int,
}

impl TryFrom<CLexemes> for BasicTypeLexeme {
    type Error = ParseError;

    fn try_from(value: CLexemes) -> std::result::Result<Self, Self::Error> {
        match value {
            CLexemes::Void => Ok(Self::Void),
            CLexemes::Unsigned => Ok(Self::Unsigned),
            CLexemes::Signed => Ok(Self::Signed),
            CLexemes::Char => Ok(Self::Char),
            CLexemes::Short => Ok(Self::Short),
            CLexemes::Long => Ok(Self::Long),
            CLexemes::Float => Ok(Self::Float),
            CLexemes::Double => Ok(Self::Double),
            CLexemes::Int => Ok(Self::Int),
            _ => Err(ParseError::BasicTypeError),
        }
    }
}

fn parse_basic_type(basic_type_specifiers: &mut [BasicTypeLexeme]) -> Result<CType> {
    // should be ok for now, optimize later
    match basic_type_specifiers {
        [BasicTypeLexeme::Void] => Ok(CType::Void {
            qualifier: TypeQualifier::empty(),
        }),
        // default is signed char
        [BasicTypeLexeme::Char] | [BasicTypeLexeme::Signed, BasicTypeLexeme::Char] => {
            Ok(CType::BasicType {
                qualifier: TypeQualifier::empty(),
                basic_type: BasicType::Char,
            })
        }
        [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Char] => Ok(CType::BasicType {
            qualifier: TypeQualifier::empty(),
            basic_type: BasicType::UChar,
        }),
        [BasicTypeLexeme::Short]
        | [BasicTypeLexeme::Signed, BasicTypeLexeme::Short]
        | [BasicTypeLexeme::Short, BasicTypeLexeme::Int]
        | [
            BasicTypeLexeme::Signed,
            BasicTypeLexeme::Short,
            BasicTypeLexeme::Int,
        ] => Ok(CType::BasicType {
            qualifier: TypeQualifier::empty(),
            basic_type: BasicType::Short,
        }),
        [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Short]
        | [
            BasicTypeLexeme::Unsigned,
            BasicTypeLexeme::Short,
            BasicTypeLexeme::Int,
        ] => Ok(CType::BasicType {
            qualifier: TypeQualifier::empty(),
            basic_type: BasicType::UShort,
        }),
        [BasicTypeLexeme::Int]
        | [BasicTypeLexeme::Signed]
        | [BasicTypeLexeme::Signed, BasicTypeLexeme::Int] => Ok(CType::BasicType {
            qualifier: TypeQualifier::empty(),
            basic_type: BasicType::Int,
        }),
        [BasicTypeLexeme::Unsigned] | [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Int] => {
            Ok(CType::BasicType {
                qualifier: TypeQualifier::empty(),
                basic_type: BasicType::UInt,
            })
        }
        [BasicTypeLexeme::Long]
        | [BasicTypeLexeme::Signed, BasicTypeLexeme::Long]
        | [BasicTypeLexeme::Long, BasicTypeLexeme::Int]
        | [
            BasicTypeLexeme::Signed,
            BasicTypeLexeme::Long,
            BasicTypeLexeme::Int,
        ]
        | [BasicTypeLexeme::Long, BasicTypeLexeme::Long]
        | [
            BasicTypeLexeme::Signed,
            BasicTypeLexeme::Long,
            BasicTypeLexeme::Long,
        ]
        | [
            BasicTypeLexeme::Long,
            BasicTypeLexeme::Long,
            BasicTypeLexeme::Int,
        ]
        | [
            BasicTypeLexeme::Signed,
            BasicTypeLexeme::Long,
            BasicTypeLexeme::Long,
            BasicTypeLexeme::Int,
        ] => Ok(CType::BasicType {
            qualifier: TypeQualifier::empty(),
            basic_type: BasicType::Long,
        }),
        [BasicTypeLexeme::Unsigned, BasicTypeLexeme::Long]
        | [
            BasicTypeLexeme::Unsigned,
            BasicTypeLexeme::Long,
            BasicTypeLexeme::Int,
        ]
        | [
            BasicTypeLexeme::Unsigned,
            BasicTypeLexeme::Long,
            BasicTypeLexeme::Long,
        ]
        | [
            BasicTypeLexeme::Unsigned,
            BasicTypeLexeme::Long,
            BasicTypeLexeme::Long,
            BasicTypeLexeme::Int,
        ] => Ok(CType::BasicType {
            qualifier: TypeQualifier::empty(),
            basic_type: BasicType::ULong,
        }),
        [BasicTypeLexeme::Double] | [BasicTypeLexeme::Long, BasicTypeLexeme::Double] => {
            Ok(CType::BasicType {
                qualifier: TypeQualifier::empty(),
                basic_type: BasicType::Double,
            })
        }
        [BasicTypeLexeme::Float] => Ok(CType::BasicType {
            qualifier: TypeQualifier::empty(),
            basic_type: BasicType::Float,
        }),

        _ => Err(ParseError::BasicTypeError.into()),
    }
}

fn parse_declaration_specifiers_base<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
    parse_function_specifiers: bool,
    parse_storage_class: bool,
) -> Result<DeclarationSpecifiers> {
    let mut storage_class: StorageClass = StorageClass::None;
    let mut function_specifier: FunctionSpecifier = FunctionSpecifier::None;
    let mut type_qualifier: TypeQualifier = TypeQualifier::empty();

    let mut primitive_type_specifiers: Vec<BasicTypeLexeme> = Vec::new();
    let mut struct_or_union_or_enum: Option<CType> = None;
    let mut typedef: Option<CType> = None;

    let mut is_typedef = false;
    loop {
        match toks.peek() {
            Some((lexeme, text, ident_ref)) => match lexeme {
                CLexemes::Extern
                | CLexemes::Auto
                | CLexemes::Register
                | CLexemes::Static
                | CLexemes::Typedef
                    if !parse_storage_class =>
                {
                    break;
                }

                CLexemes::Typedef => {
                    is_typedef = true;
                    toks.eat(CLexemes::Typedef);
                }

                // storage class specifiers, only 1 allowed
                CLexemes::Extern => {
                    if storage_class != StorageClass::None {
                        return Err(ParseError::UnexpectedStorageClass.into());
                    }
                    toks.eat(CLexemes::Extern);
                    storage_class = StorageClass::Extern;
                }
                CLexemes::Auto => {
                    if storage_class != StorageClass::None {
                        return Err(ParseError::UnexpectedStorageClass.into());
                    }
                    toks.eat(CLexemes::Auto);
                    storage_class = StorageClass::Auto;
                }
                CLexemes::Register => {
                    if storage_class != StorageClass::None {
                        return Err(ParseError::UnexpectedStorageClass.into());
                    }
                    toks.eat(CLexemes::Register);
                    storage_class = StorageClass::Register;
                }
                CLexemes::Static => {
                    if storage_class != StorageClass::None {
                        return Err(ParseError::UnexpectedStorageClass.into());
                    }
                    toks.eat(CLexemes::Static);
                    storage_class = StorageClass::Static;
                }

                // type qualifiers, multiple allowed
                CLexemes::Const => {
                    toks.eat(CLexemes::Const);
                    type_qualifier |= TypeQualifier::Const;
                }
                CLexemes::Restrict => {
                    /* RESOLVE LOGIC
                    return Err(ParseError::BadRestrictQualifier);
                     */
                    toks.eat(CLexemes::Restrict);
                    type_qualifier |= TypeQualifier::Restrict;
                }
                CLexemes::Volatile => {
                    toks.eat(CLexemes::Volatile);
                    type_qualifier |= TypeQualifier::Volatile;
                }

                // function specifiers
                CLexemes::Inline => {
                    if !parse_function_specifiers {
                        break;
                    }
                    toks.eat(CLexemes::Inline);
                    function_specifier = FunctionSpecifier::Inline;
                }

                // according to standard, these are allowed in any order and allowed to be mixed w/ any part of the specifier
                // (why???)
                CLexemes::Void
                | CLexemes::Char
                | CLexemes::Short
                | CLexemes::Int
                | CLexemes::Long
                | CLexemes::Float
                | CLexemes::Double
                | CLexemes::Signed
                | CLexemes::Unsigned
                    if struct_or_union_or_enum.is_none() && typedef.is_none() =>
                {
                    toks.eat(lexeme);
                    primitive_type_specifiers.push(lexeme.try_into()?);
                }

                CLexemes::Void
                | CLexemes::Char
                | CLexemes::Short
                | CLexemes::Int
                | CLexemes::Long
                | CLexemes::Float
                | CLexemes::Double
                | CLexemes::Signed
                | CLexemes::Unsigned => {
                    return Err(ParseError::MultipleTypeSpecifiersInDeclaration.into());
                }

                // struct / union specifier (syntactically very similar!)
                CLexemes::Struct
                    if struct_or_union_or_enum.is_none()
                        && primitive_type_specifiers.is_empty()
                        && typedef.is_none() =>
                {
                    let struct_type = parse_struct_specifier(toks, state)?;
                    struct_or_union_or_enum = Some(CType::StructureTypeRef {
                        qualifier: TypeQualifier::empty(),
                        symtab_idx: struct_type,
                    });
                }
                CLexemes::Union
                    if struct_or_union_or_enum.is_none()
                        && primitive_type_specifiers.is_empty()
                        && typedef.is_none() =>
                {
                    let union_type = parse_union_specifier(toks, state)?;
                    struct_or_union_or_enum = Some(CType::UnionTypeRef {
                        qualifier: TypeQualifier::empty(),
                        symtab_idx: union_type,
                    });
                }

                CLexemes::Struct | CLexemes::Union => {
                    return Err(ParseError::MultipleTypeSpecifiersInDeclaration.into());
                }

                // enum specifier
                CLexemes::Enum
                    if struct_or_union_or_enum.is_none()
                        && primitive_type_specifiers.is_empty()
                        && typedef.is_none() =>
                {
                    let enum_type = parse_enum_specifier(toks, state)?;
                    struct_or_union_or_enum = Some(CType::EnumTypeRef {
                        qualifier: TypeQualifier::empty(),
                        symtab_idx: enum_type,
                    });
                }

                CLexemes::Enum => {
                    return Err(ParseError::MultipleTypeSpecifiersInDeclaration.into());
                }

                // typedef
                CLexemes::Identifier => {
                    if let Some(ty) = state.get_typedef(ident_ref) {
                        if struct_or_union_or_enum.is_some()
                            || !primitive_type_specifiers.is_empty()
                            || typedef.is_some()
                        {
                            break;
                        }
                        toks.eat(CLexemes::Identifier);
                        typedef = Some(ty.clone());
                    } else {
                        break;
                    }
                }

                _ => {
                    break;
                }
            },
            None => {
                break;
            }
        }
    }

    let qualified_type = if !primitive_type_specifiers.is_empty() {
        let mut basic_type = parse_basic_type(&mut primitive_type_specifiers)?;
        *basic_type.qualifier_mut() = type_qualifier;
        basic_type
    } else if let Some(mut ty) = struct_or_union_or_enum {
        *ty.qualifier_mut() = type_qualifier;
        ty
    } else if let Some(mut ty) = typedef {
        *ty.qualifier_mut() |= type_qualifier;
        ty
    } else {
        return Err(ParseError::MissingTypeSpecifier.into());
    };

    if is_typedef && storage_class != StorageClass::None {
        return Err(ParseError::UnexpectedStorageClass.into());
    }

    let declaration_specifiers = DeclarationSpecifiers {
        qualified_type,
        storage_class,
        function_specifier,
        is_typedef,
    };

    Ok(declaration_specifiers)
}

fn parse_declaration_specifiers<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<DeclarationSpecifiers> {
    parse_declaration_specifiers_base(toks, state, true, true)
}

fn parse_specifier_qualifier_list<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<CType> {
    let DeclarationSpecifiers { qualified_type, .. } =
        parse_declaration_specifiers_base(toks, state, false, false)?;
    Ok(qualified_type)
}

fn parse_struct_declaration_list<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<Vec<AggregateMember>> {
    let mut members: Vec<AggregateMember> = Vec::new();
    loop {
        match toks.peek() {
            Some((CLexemes::RBrace, _, _)) => {
                break;
            }
            Some((_, _, _)) => {
                let base_type = parse_specifier_qualifier_list(toks, state)?;
                parse_struct_declarator_list(toks, state, base_type, &mut members)?;
                eat_or_error!(toks, CLexemes::Semicolon)?;
            }
            None => {
                return Err(ParseError::UnexpectedEOF.into());
            }
        }
    }
    Ok(members)
}

fn parse_struct_declarator_list<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
    base_type: CType,
    members: &mut Vec<AggregateMember>,
) -> Result<()> {
    // TODO: support bitfields

    let (member_type, member_name) = parse_declarator(toks, state, base_type.clone())?;
    members.push((member_name, member_type));
    loop {
        match toks.peek() {
            Some((CLexemes::Comma, _, _)) => {
                // cloning is not expensive since base_type is not a derived type
                let (member_type, member_name) = parse_declarator(toks, state, base_type.clone())?;
                members.push((member_name, member_type));
            }
            Some((_, _, _)) => return Ok(()),
            None => {
                return Err(ParseError::UnexpectedEOF.into());
            }
        }
    }
}

fn parse_struct_specifier<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<StructureTypeIdx> {
    match toks.peek() {
        Some((CLexemes::Struct, _, _)) => {
            toks.eat(CLexemes::Struct);
        }
        Some((other, _, _)) => {
            return Err(ParseError::UnexpectedToken(other).into());
        }
        None => {
            return Err(ParseError::UnexpectedEOF.into());
        }
    }

    let struct_tag = match toks.peek() {
        Some((CLexemes::LBrace, _, _)) => None,
        Some((CLexemes::Identifier, tag, ident_ref)) => {
            toks.eat(CLexemes::Identifier);

            match toks.peek() {
                Some((CLexemes::LBrace, _, _)) => Some(ident_ref),
                Some((_, _, _)) => {
                    let incomplete_struct = StructureType::new_incomplete_structure_type(Some(ident_ref));
                    let incomplete_type_idx = state.add_structure_type(incomplete_struct);
                    return Ok(incomplete_type_idx);
                }
                None => {
                    return Err(ParseError::UnexpectedEOF.into());
                }
            }
        }
        Some((other, _, _)) => {
            return Err(ParseError::UnexpectedToken(other).into());
        }
        None => {
            return Err(ParseError::UnexpectedEOF.into());
        }
    };

    // declaring a new struct / union
    toks.eat(CLexemes::LBrace);

    let members: Vec<AggregateMember> = parse_struct_declaration_list(toks, state)?;
    eat_or_error!(toks, CLexemes::RBrace)?;

    let complete_struct_type = StructureType::new_complete_structure_type(struct_tag, members);
    let type_idx = state.add_structure_type(complete_struct_type);

    Ok(type_idx)
}

fn parse_union_specifier<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<UnionTypeIdx> {
    match toks.peek() {
        Some((CLexemes::Union, _, _)) => {
            toks.eat(CLexemes::Union);
        }
        Some((other, _, _)) => {
            return Err(ParseError::UnexpectedToken(other).into());
        }
        None => {
            return Err(ParseError::UnexpectedEOF.into());
        }
    }

    let union_tag = match toks.peek() {
        Some((CLexemes::LBrace, _, _)) => None,
        Some((CLexemes::Identifier, tag, ident_ref)) => {
            toks.eat(CLexemes::Identifier);

            match toks.peek() {
                Some((CLexemes::LBrace, _, _)) => Some(ident_ref),
                Some((_, _, _)) => {
                    let incomplete_union = UnionType::new_incomplete_union_type(Some(ident_ref));
                    let incomplete_type_idx = state.add_union_type(incomplete_union);
                    return Ok(incomplete_type_idx);
                }
                None => {
                    return Err(ParseError::UnexpectedEOF.into());
                }
            }
        }
        Some((other, _, _)) => {
            return Err(ParseError::UnexpectedToken(other).into());
        }
        None => {
            return Err(ParseError::UnexpectedEOF.into());
        }
    };

    // declaring a new struct / union
    toks.eat(CLexemes::LBrace);

    let members: Vec<AggregateMember> = parse_struct_declaration_list(toks, state)?;
    eat_or_error!(toks, CLexemes::RBrace)?;

    let complete_union_type = UnionType::new_complete_union_type(union_tag, members);
    let type_idx = state.add_union_type(complete_union_type);

    Ok(type_idx)
}

fn parse_enum_specifier<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<EnumTypeIdx> {
    match toks.peek() {
        Some((CLexemes::Enum, _, _)) => {
            toks.eat(CLexemes::Enum);
        }
        Some((other, _, _)) => {
            return Err(ParseError::UnexpectedToken(other).into());
        }
        None => {
            return Err(ParseError::UnexpectedEOF.into());
        }
    }

    let enum_tag = match toks.peek() {
        Some((CLexemes::LBrace, _, _)) => {
            // untagged enum, always declares a new type
            toks.eat(CLexemes::LBrace);
            None
        }
        Some((CLexemes::Identifier, ident, ident_ref)) => {
            toks.eat(CLexemes::Identifier);

            let next_tok = toks.peek();
            match next_tok {
                Some((CLexemes::LBrace, _, _)) => Some(ident_ref),
                Some(_) => {
                    let incomplete_type = EnumType::new_incomplete_enum_type(Some(ident_ref));
                    let incomplete_type_idx = state.add_enum_type(incomplete_type);
                    return Ok(incomplete_type_idx);
                }
                None => return Err(ParseError::UnexpectedEOF.into()),
            }
        }
        Some((other, _, _)) => return Err(ParseError::UnexpectedToken(other).into()),
        None => return Err(ParseError::UnexpectedEOF.into()),
    };

    toks.eat(CLexemes::LBrace);

    let mut counter: i32 = 0;
    let mut enum_members: Vec<(StringPoolRef, i32)> = Vec::new();
    loop {
        // 1. match identifier
        let enum_constant_name: StringPoolRef;
        match toks.peek() {
            Some((CLexemes::Identifier, name, ident_ref)) => {
                enum_constant_name = ident_ref;
                toks.eat(CLexemes::Identifier);
            }
            Some((CLexemes::RBrace, _, _)) => {
                return Err(ParseError::EmptyEnum.into());
            }
            Some((other, _, _)) => {
                return Err(ParseError::UnexpectedToken(other).into());
            }
            None => {
                return Err(ParseError::UnexpectedEOF.into());
            }
        }

        // 2. comma -> go next, brace -> break, other -> bad token, equal -> expect int
        match toks.peek() {
            Some((CLexemes::Comma, _, _)) => {
                enum_members.push((enum_constant_name, counter));

                toks.eat(CLexemes::Comma);
                counter += 1;
                if let Some((CLexemes::RBrace, _, _)) = toks.peek() {
                    toks.eat(CLexemes::RBrace);
                    break;
                } else {
                    continue;
                }
            }
            Some((CLexemes::Eq, _, _)) => {
                toks.eat(CLexemes::Eq);
                // fall through
            }
            Some((CLexemes::RBrace, _, _)) => {
                toks.eat(CLexemes::RBrace);
                break;
            }
            Some((other, _, _)) => {
                return Err(ParseError::UnexpectedToken(other).into());
            }
            None => {
                return Err(ParseError::UnexpectedEOF.into());
            }
        }

        match toks.peek() {
            Some((CLexemes::IntegerConst, int_span, _)) => {
                // TODO: technically any integer constant expression is ok, but just ignore this for now
                // until can parse constant expressions more generally.
                let int_str = toks.text(int_span);
                let enum_value = int_str
                    .parse::<i32>()
                    .map_err(|_| ParseError::InvalidEnumConstant(int_str.to_string()))?;

                enum_members.push((enum_constant_name.clone(), enum_value));

                counter = enum_value + 1;
                toks.eat(CLexemes::IntegerConst);
            }
            Some((_, text, _)) => {
                return Err(ParseError::InvalidEnumConstant(toks.text(text).to_string()).into());
            }
            None => {
                return Err(ParseError::UnexpectedEOF.into());
            }
        }

        match toks.peek() {
            Some((CLexemes::Comma, _, _)) => {
                toks.eat(CLexemes::Comma);
                if let Some((CLexemes::RBrace, _, _)) = toks.peek() {
                    toks.eat(CLexemes::RBrace);
                    break;
                } else {
                    continue;
                }
            }
            Some((CLexemes::RBrace, _, _)) => {
                toks.eat(CLexemes::RBrace);
                break;
            }
            Some((other, _, _)) => {
                return Err(ParseError::UnexpectedToken(other).into());
            }
            None => {
                return Err(ParseError::UnexpectedEOF.into());
            }
        }
    }

    let enum_type = EnumType::new_complete_enum_type(enum_tag, enum_members);
    let enum_type_idx = state.add_enum_type(enum_type);

    Ok(enum_type_idx)
}

fn parse_init_declarators<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
    declaration_specifiers: DeclarationSpecifiers,
) -> Result<ASTNode> {
    let mut declarations = Vec::new();

    // empty declarations are acceptable (e.g. just defining a struct)
    if let Some((CLexemes::Semicolon, _, _)) = toks.peek() {
        let decl_node = ASTNode::Declaration(declarations);
        return Ok(decl_node);
    }

    let declaration = parse_init_declarator(toks, state, declaration_specifiers.clone())?;
    declarations.push(declaration);
    loop {
        match toks.peek() {
            Some((CLexemes::Comma, _, _)) => {
                toks.eat(CLexemes::Comma);
                let declaration =
                    parse_init_declarator(toks, state, declaration_specifiers.clone())?;
                declarations.push(declaration);
            }
            Some((CLexemes::Semicolon, _, _)) => {
                let decl_node = ASTNode::Declaration(declarations);
                return Ok(decl_node);
            }
            Some((other, _, _)) => {
                return Err(ParseError::UnexpectedToken(other).into());
            }
            None => {
                return Err(ParseError::UnexpectedEOF.into());
            }
        }
    }
}

fn parse_init_declarator<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
    declaration_specifiers: DeclarationSpecifiers,
) -> Result<Declaration> {
    let DeclarationSpecifiers {
        qualified_type,
        storage_class,
        function_specifier,
        is_typedef,
    } = declaration_specifiers;
    let base_type = qualified_type;
    let (qualified_type, name) = parse_declarator(toks, state, base_type)?;
    let initializer = match toks.peek() {
        Some((CLexemes::Assign, _, _)) => {
            toks.eat(CLexemes::Assign);
            let initializer = parse_initializer(toks, state)?;
            Some(Box::new(initializer))
        }
        Some((_, _, _)) => None,
        None => return Err(ParseError::UnexpectedEOF.into()),
    };

    if is_typedef {
        state.add_typedef(name.clone(), qualified_type.clone())?;
    }

    let declaration = Declaration::new(
        Identifier::new(state.current_scope, name),
        qualified_type,
        storage_class,
        is_typedef,
        function_specifier,
        initializer,
    );

    Ok(declaration)
}

fn parse_initializer<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<ExpressionNode> {
    match toks.peek() {
        Some((CLexemes::LBrace, _, _)) => {
            todo!("compound initializers not implemented yet")
        }
        Some((_, _, _)) => parse_assignment_expr(toks, state),
        None => Err(ParseError::UnexpectedEOF.into()),
    }
}

// The concept of a declarator basically only exists during parsing and isn't semantically meaningful elsewhere
// "abstract" declarator has no identifier
struct Declarator(CType, Option<StringPoolRef>);
fn parse_declarator_base<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
    base_type: CType,
) -> Result<Declarator> {
    let mut pointers: Vec<(TypeQualifier, usize)> = Vec::new();
    #[derive(Debug)]
    enum ArrayOrFunctionDeclarator {
        ArrayDeclarator(ArrayDeclarator),
        FunctionDeclarator(FunctionDeclarator),
    }
    let mut array_or_function_declarators: Vec<(ArrayOrFunctionDeclarator, usize)> = Vec::new();
    let mut identifier: Option<StringPoolRef> = None;
    let mut level: usize = 0;
    let mut max_level: usize = 0;
    loop {
        match toks.peek() {
            Some((CLexemes::Star, _, _)) => {
                let pointer = parse_pointer_declarator(toks, state)?;
                pointers.push((pointer.0, level))
            }
            Some((CLexemes::LParen, _, _)) => {
                // problem: in abstract declarator, identifer is omitted, so this might be grouping paren
                // or a function. need lookahead to determine which
                match toks.peek_n(1) {
                    Some((CLexemes::Star, _, _))
                    | Some((CLexemes::LParen, _, _))
                    | Some((CLexemes::LBracket, _, _)) => {
                        // "declarator part", could not appear in a parameter type list
                        // since parameter type requires a type specifier, which is disjoint from these lexemes
                        toks.eat(CLexemes::LParen);
                        level += 1;
                        max_level += 1;
                    }
                    Some((CLexemes::RParen, _, _)) => {
                        // must be void function, so fall through without consuming LParen
                        break;
                    }
                    Some((_, _, _)) => {
                        // must be function declarator by earlier reasoning, fall through
                        break;
                    }
                    None => {
                        return Err(ParseError::UnmatchedParensInDeclarator.into());
                    }
                }
            }
            Some((CLexemes::Identifier, ident, ident_ref)) => {
                identifier = Some(ident_ref);
                toks.eat(CLexemes::Identifier);
                break;
            }
            Some((_, _, _)) | None => {
                break;
            }
        }
    }

    loop {
        match toks.peek() {
            Some((CLexemes::LBracket, _, _)) => {
                let array = parse_array_declarator(toks, state)?;
                array_or_function_declarators
                    .push((ArrayOrFunctionDeclarator::ArrayDeclarator(array), level));
            }
            Some((CLexemes::LParen, _, _)) => {
                let function = parse_function_declarator(toks, state)?;
                array_or_function_declarators.push((
                    ArrayOrFunctionDeclarator::FunctionDeclarator(function),
                    level,
                ));
            }
            Some((CLexemes::RParen, _, _)) => {
                // e.g. void f(int a, int b)
                // don't want declarator of b to be confused w/ right paren of f's function declaration
                if level == 0 {
                    break;
                }
                toks.eat(CLexemes::RParen);
                level -= 1;
            }
            Some((_, _, _)) | None => {
                break;
            }
        }
    }

    if level != 0 {
        return Err(ParseError::UnmatchedParensInDeclarator.into());
    }

    // according to grammar, [] and () bind more tightly than *
    // array of array: ok
    // array of function: not ok by 6.7.5.2 1 ("The element type shall not be an incomplete or function type.")
    // function which return function / function which return array: not ok by 6.7.5.3 1
    // furthermore, incomplete return type or parameter types is ok for function prototypes
    // e.g. struct unknown function(struct unknown2);
    // is fine; presumably this is so code which only uses its address is ok (?), but any attempt to call it is obviously
    // not going to work until those incomplete types are filled in
    let mut pointer_index: usize = 0;
    let mut current_type = base_type;
    for level in 0..=max_level {
        while pointer_index < pointers.len() && pointers[pointer_index].1 == level {
            current_type = CType::PointerType {
                qualifier: pointers[pointer_index].0,
                pointee_type: Box::new(current_type),
            };

            pointer_index += 1;
        }
        while let Some((_, l)) = array_or_function_declarators.last() {
            if *l == level {
                let (d, _) = array_or_function_declarators.pop().unwrap();
                match d {
                    ArrayOrFunctionDeclarator::ArrayDeclarator(array) => {
                        let new_type: CType;
                        if let Some(size) = array.1 {
                            new_type = CType::ArrayType {
                                qualifier: array.0,
                                size,
                                element_type: Box::new(current_type),
                            };
                        } else {
                            new_type = CType::IncompleteArrayType {
                                qualifier: array.0,
                                element_type: Box::new(current_type),
                            };
                        }

                        current_type = new_type;
                    }
                    ArrayOrFunctionDeclarator::FunctionDeclarator(func) => {
                        let fn_type = FunctionType {
                            parameter_types: func.argument_types,
                            return_type: current_type,
                            function_specifier: FunctionSpecifier::None, // TODO: figure out inline
                            varargs: func.varargs,
                            prototype_scope: func.prototype_scope,
                        };

                        let fn_type_idx = state.add_function_type(fn_type);
                        let new_type = CType::FunctionTypeRef {
                            symtab_idx: fn_type_idx,
                        };

                        current_type = new_type;
                    }
                };
            }
            break;
        }
    }

    Ok(Declarator(current_type, identifier))
}

fn parse_declarator<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
    base_type: CType,
) -> Result<(CType, StringPoolRef)> {
    let decl = parse_declarator_base(toks, state, base_type)?;
    if let Some(ident) = decl.1 {
        Ok((decl.0, ident))
    } else {
        Err(ParseError::DeclaratorRequiresName.into())
    }
}

fn parse_abstract_declarator<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
    base_type: CType,
) -> Result<CType> {
    let decl = parse_declarator_base(toks, state, base_type)?;
    if let None = decl.1 {
        Ok(decl.0)
    } else {
        Err(ParseError::UnexpectedDeclaratorName.into())
    }
}

#[derive(Debug)]
struct PointerDeclarator(TypeQualifier);
fn parse_pointer_declarator<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    _state: &mut ParserState,
) -> Result<PointerDeclarator> {
    let mut qualifier = TypeQualifier::empty();
    eat_or_error!(toks, CLexemes::Star)?;
    loop {
        match toks.peek() {
            Some((CLexemes::Const, _, _)) => {
                qualifier |= TypeQualifier::Const;
                toks.eat(CLexemes::Const);
            }
            Some((CLexemes::Volatile, _, _)) => {
                qualifier |= TypeQualifier::Volatile;
                toks.eat(CLexemes::Volatile);
            }
            Some((CLexemes::Restrict, _, _)) => {
                qualifier |= TypeQualifier::Restrict;
                toks.eat(CLexemes::Restrict);
            }
            Some((_, _, _)) => {
                return Ok(PointerDeclarator(qualifier));
            }
            None => {
                return Err(ParseError::UnexpectedEOF.into());
            }
        }
    }
}
#[derive(Debug, Clone, Copy)]
struct ArrayDeclarator(TypeQualifier, Option<u32>);
fn parse_array_declarator<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    _state: &mut ParserState,
) -> Result<ArrayDeclarator> {
    let mut qualifier = TypeQualifier::empty();
    eat_or_error!(toks, CLexemes::LBracket)?;
    loop {
        match toks.peek() {
            Some((CLexemes::Restrict, _, _)) => {
                qualifier |= TypeQualifier::Restrict;
            }
            Some((CLexemes::Volatile, _, _)) => {
                qualifier |= TypeQualifier::Volatile;
            }
            Some((CLexemes::Const, _, _)) => {
                qualifier |= TypeQualifier::Const;
            }
            Some((CLexemes::RBracket, _, _)) => {
                toks.eat(CLexemes::RBracket);
                return Ok(ArrayDeclarator(qualifier, None));
            }
            Some((CLexemes::IntegerConst, i_span, _)) => {
                let size = toks.text(i_span).parse::<u32>().map_err(|e| ParseError::BadArrayBound(e))?;
                eat_or_error!(toks, CLexemes::IntegerConst)?;
                eat_or_error!(toks, CLexemes::RBracket)?;
                return Ok(ArrayDeclarator(qualifier, Some(size)));
            }
            Some((_, _, _)) => {
                // TODO: support integer constant expressions here
                todo!();
                // return Err(ParseError::UnexpectedToken(other));
            }
            None => {
                return Err(ParseError::UnexpectedEOF.into());
            }
        }
    }
}

// only time scope is needed is if this is actually a function definition, and not
// just a prototype
#[derive(Debug, Clone)]
struct FunctionDeclarator {
    argument_types: Vec<FunctionArgument>,
    varargs: bool,
    prototype_scope: Scope,
}

// will not support old-style function declarations because they are cringe
// this code is used for both function prototypes and function definitions
fn parse_function_declarator<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<FunctionDeclarator> {
    eat_or_error!(toks, CLexemes::LParen)?;
    state.open_scope(ScopeType::FunctionScope);

    // ignore special case for empty parameter list
    // technically, this has special behavior (completed by later declarations / usages),
    // a sort of "incomplete" function type. not used very much, won't support
    let mut parameter_types: Vec<FunctionArgument> = Vec::new();
    let mut varargs = false;
    loop {
        match toks.peek() {
            Some((CLexemes::RParen, _, _)) => {
                toks.eat(CLexemes::RParen);
                let declarator = FunctionDeclarator {
                    argument_types: parameter_types,
                    varargs,
                    prototype_scope: state.current_scope,
                };
                state.close_scope()?;
                return Ok(declarator);
            }
            Some((CLexemes::Ellipsis, _, _)) => {
                toks.eat(CLexemes::Ellipsis);
                varargs = true;
                eat_or_error!(toks, CLexemes::RParen)?;
                let declarator = FunctionDeclarator {
                    argument_types: parameter_types,
                    varargs,
                    prototype_scope: state.current_scope,
                };
                state.close_scope()?;
                return Ok(declarator);
            }
            Some((_, _, _)) => {
                let parameter_base_type =
                    parse_declaration_specifiers_base(toks, state, false, false)?;

                match toks.peek() {
                    Some((CLexemes::Comma, _, _)) => {
                        parameter_types.push((None, parameter_base_type.qualified_type));
                        toks.eat(CLexemes::Comma);
                    }
                    Some((CLexemes::RParen, _, _)) => {
                        parameter_types.push((None, parameter_base_type.qualified_type));
                        // fall through after loop to close function arm
                    }
                    Some((_, _, _)) => {
                        let DeclarationSpecifiers { qualified_type, .. } = parameter_base_type;
                        let parameter_base_type = qualified_type;
                        let Declarator(parameter_type, parameter_name) =
                            parse_declarator_base(toks, state, parameter_base_type)?;

                        parameter_types.push((parameter_name, parameter_type));

                        match toks.peek() {
                            Some((CLexemes::Comma, _, _)) => {
                                toks.eat(CLexemes::Comma);
                                continue;
                            }
                            _ => {
                                continue;
                            }
                        }
                    }
                    None => {
                        return Err(ParseError::UnexpectedEOF.into());
                    }
                }
            }
            None => {
                return Err(ParseError::UnexpectedEOF.into());
            }
        }
    }
}

fn parse_statement<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<ASTNode> {
    // only 1-2 token lookahead required to check for what type of statement it is
    if is_lookahead_label(toks, state) {
        return parse_labeled_statement(toks, state);
    }

    match toks.peek() {
        Some((CLexemes::LBrace, _, _)) => parse_compound_statement(toks, state),
        Some((CLexemes::If, _, _)) | Some((CLexemes::Switch, _, _)) => {
            parse_selection_statement(toks, state)
        }
        Some((CLexemes::While, _, _))
        | Some((CLexemes::Do, _, _))
        | Some((CLexemes::For, _, _)) => parse_iteration_statement(toks, state),
        Some((CLexemes::Goto, _, _))
        | Some((CLexemes::Continue, _, _))
        | Some((CLexemes::Break, _, _))
        | Some((CLexemes::Return, _, _)) => parse_jump_statement(toks, state),
        Some((_, _, _)) => parse_expression_statement(toks, state),
        None => Err(ParseError::UnexpectedEOF.into()),
    }
}

fn is_lookahead_label<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>, 
    _state: &mut ParserState
) -> bool {
    match toks.peek() {
        Some((lexeme, _, _)) => {
            match lexeme {
                CLexemes::Default | CLexemes::Case => true,
                CLexemes::Identifier => {
                    // only other place colon appears in grammar is for ternaries
                    // which should get completely parsed by parse_expr
                    if let Some((CLexemes::Colon, _, _)) = toks.peek_n(1) {
                        true
                    } else {
                        false
                    }
                }
                _ => false,
            }
        }
        None => false,
    }
}

fn parse_labeled_statement<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<ASTNode> {
    match toks.peek() {
        Some((CLexemes::Identifier, label, ident_ref)) => {
            toks.eat(CLexemes::Identifier);
            eat_or_error!(toks, CLexemes::Colon)?;

            // C99 does not allow labeled statement at end of compound statement
            // but gcc and clang will (only complaining on -Wpedantic)
            // follow standard for now
            let labelee = parse_statement(toks, state)?;
            let labelee = Box::new(labelee);

            let label_ident = Identifier::new(state.current_scope, ident_ref);
            let label_node = ASTNode::Label(labelee, label_ident);

            Ok(label_node)
        }
        Some((CLexemes::Case, _, _)) => {
            // again, clang / gcc allow empty case
            // but we won't
            toks.eat(CLexemes::Case);
            let case = parse_expr(toks, state)?;
            eat_or_error!(toks, CLexemes::Colon)?;
            let case_body = parse_statement(toks, state)?;

            let case_node = ASTNode::CaseLabel(Box::new(case_body), Box::new(case));

            Ok(case_node)
        }
        Some((CLexemes::Default, _, _)) => {
            toks.eat(CLexemes::Default);
            eat_or_error!(toks, CLexemes::Colon)?;
            let default_body = parse_statement(toks, state)?;
            let default_node = ASTNode::DefaultLabel(Box::new(default_body));

            Ok(default_node)
        }
        Some((other, _, _)) => Err(ParseError::UnexpectedToken(other).into()),
        None => Err(ParseError::UnexpectedEOF.into()),
    }
}

fn parse_compound_statement<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<ASTNode> {
    eat_or_error!(toks, CLexemes::LBrace)?;
    // open a new block scope
    state.open_scope(ScopeType::BlockScope);
    let mut items = Vec::new();
    loop {
        match toks.peek() {
            Some((CLexemes::RBrace, _, _)) => {
                break;
            }
            Some((_, _, _)) => (),
            None => {
                return Err(ParseError::UnexpectedEOF.into());
            }
        }
        let ast_node = if is_lookahead_declaration(toks, state) {
            // upcoming tokens must be declaration
            parse_declaration(toks, state)?
        } else {
            // upcoming tokens must be statement
            parse_statement(toks, state)?
        };
        items.push(ast_node);
    }
    state.close_scope()?;
    eat_or_error!(toks, CLexemes::RBrace)?;

    let compound_node = ASTNode::CompoundStatement(items, state.current_scope);

    Ok(compound_node)
}

fn parse_expression_statement<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<ASTNode> {
    match toks.peek() {
        Some((CLexemes::Semicolon, _, _)) => {
            toks.eat(CLexemes::Semicolon);
            Ok(ASTNode::NullStatement)
        }
        Some((_, _, _)) => {
            let expr = parse_expr(toks, state)?;
            eat_or_error!(toks, CLexemes::Semicolon)?;
            Ok(ASTNode::ExpressionStatement(
                Box::new(expr),
                state.current_scope,
            ))
        }
        None => Err(ParseError::UnexpectedEOF.into()),
    }
}

fn parse_selection_statement<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<ASTNode> {
    match toks.peek() {
        Some((CLexemes::If, _, _)) => {
            toks.eat(CLexemes::If);
            eat_or_error!(toks, CLexemes::LParen)?;
            let condition = parse_expr(toks, state)?;
            eat_or_error!(toks, CLexemes::RParen)?;
            let body = parse_statement(toks, state)?;
            // Dangling else attaches to lexically *closest* if statement
            let if_node = match toks.peek() {
                Some((CLexemes::Else, _, _)) => {
                    toks.eat(CLexemes::Else);
                    let else_body = parse_statement(toks, state)?;
                    ASTNode::IfStatement(
                        Box::new(condition),
                        Box::new(body),
                        Some(Box::new(else_body)),
                        state.current_scope,
                    )
                }
                Some((_, _, _)) => ASTNode::IfStatement(
                    Box::new(condition),
                    Box::new(body),
                    None,
                    state.current_scope,
                ),
                None => {
                    return Err(ParseError::UnexpectedEOF.into());
                }
            };

            Ok(if_node)
        }
        Some((CLexemes::Switch, _, _)) => {
            toks.eat(CLexemes::Switch);
            eat_or_error!(toks, CLexemes::LParen)?;
            let switch_expr = parse_expr(toks, state)?;
            eat_or_error!(toks, CLexemes::RParen)?;
            let body = parse_statement(toks, state)?;
            let switch_node = ASTNode::SwitchStatement(
                Box::new(switch_expr),
                Box::new(body),
                state.current_scope,
            );

            Ok(switch_node)
        }
        Some((other, _, _)) => {
            return Err(ParseError::UnexpectedToken(other).into());
        }
        None => {
            return Err(ParseError::UnexpectedEOF.into());
        }
    }
}

fn parse_iteration_statement<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<ASTNode> {
    match toks.peek() {
        Some((CLexemes::While, _, _)) => {
            toks.eat(CLexemes::While);
            eat_or_error!(toks, CLexemes::LParen)?;
            let expr = parse_expr(toks, state)?;
            eat_or_error!(toks, CLexemes::RParen)?;
            let statement = parse_statement(toks, state)?;
            let while_statement =
                ASTNode::WhileStatement(Box::new(expr), Box::new(statement), state.current_scope);
            Ok(while_statement)
        }
        Some((CLexemes::Do, _, _)) => {
            toks.eat(CLexemes::Do);
            let statement = parse_statement(toks, state)?;
            eat_or_error!(toks, CLexemes::While)?;
            eat_or_error!(toks, CLexemes::LParen)?;
            let expr = parse_expr(toks, state)?;
            eat_or_error!(toks, CLexemes::RParen)?;
            eat_or_error!(toks, CLexemes::Semicolon)?;
            let do_while_statement =
                ASTNode::DoWhileStatement(Box::new(expr), Box::new(statement), state.current_scope);
            Ok(do_while_statement)
        }
        Some((CLexemes::For, _, _)) => {
            // for loop requires some care; clause 1 being possibly a declaration
            // e.g. for(int i = 0; i < 5; i += 1)
            // or just an expression
            toks.eat(CLexemes::For);
            eat_or_error!(toks, CLexemes::LParen)?;
            let mut has_declaration = false;

            let first_clause = if is_lookahead_declaration(toks, state) {
                state.open_scope(ScopeType::BlockScope);
                let declaration = parse_declaration(toks, state)?;
                has_declaration = true;
                Some(declaration)
            } else {
                let expr = match toks.peek() {
                    Some((CLexemes::Semicolon, _, _)) => None,
                    Some((_, _, _)) => {
                        let expr = parse_expr(toks, state)?;
                        Some(ASTNode::ExpressionStatement(
                            Box::new(expr),
                            state.current_scope,
                        ))
                    }
                    None => return Err(ParseError::UnexpectedEOF.into()),
                };

                eat_or_error!(toks, CLexemes::Semicolon)?;
                expr
            };
            
            let second_clause = match toks.peek() {
                Some((CLexemes::Semicolon, _, _)) => None,
                Some((_, _, _)) => Some(parse_expr(toks, state)?),
                None => return Err(ParseError::UnexpectedEOF.into()),
            };
            eat_or_error!(toks, CLexemes::Semicolon)?;
            let third_clause = match toks.peek() {
                Some((CLexemes::Semicolon, _, _)) => None,
                Some((_, _, _)) => Some(parse_expr(toks, state)?),
                None => return Err(ParseError::UnexpectedEOF.into()),
            };
            eat_or_error!(toks, CLexemes::RParen)?;

            let loop_body = parse_statement(toks, state)?;
            let for_node = ASTNode::ForStatement(
                first_clause.map(Box::new),
                second_clause.map(Box::new),
                third_clause.map(Box::new),
                Box::new(loop_body),
                state.current_scope,
            );

            if has_declaration {
                state.close_scope()?;
            }

            Ok(for_node)
        }
        Some((other, _, _)) => Err(ParseError::UnexpectedToken(other).into()),
        None => Err(ParseError::UnexpectedEOF.into()),
    }
}

fn parse_jump_statement<TokenSpan>(
    toks: &mut impl InterningTokenStream<CLexemes, TokenSpan>,
    state: &mut ParserState,
) -> Result<ASTNode> {
    match toks.peek() {
        Some((CLexemes::Goto, _, _)) => {
            toks.eat(CLexemes::Goto);
            match toks.peek() {
                Some((CLexemes::Identifier, ident, ident_ref)) => {
                    let ident = Identifier::new(state.current_scope, ident_ref);
                    eat_or_error!(toks, CLexemes::Semicolon)?;
                    Ok(ASTNode::GotoStatement(ident))
                }
                Some((other, _, _)) => Err(ParseError::UnexpectedToken(other).into()),
                None => Err(ParseError::UnexpectedEOF.into()),
            }
        }
        Some((CLexemes::Continue, _, _)) => {
            toks.eat(CLexemes::Continue);
            eat_or_error!(toks, CLexemes::Semicolon)?;
            Ok(ASTNode::ContinueStatement)
        }
        Some((CLexemes::Break, _, _)) => {
            toks.eat(CLexemes::Break);
            eat_or_error!(toks, CLexemes::Semicolon)?;
            Ok(ASTNode::BreakStatement)
        }
        Some((CLexemes::Return, _, _)) => {
            toks.eat(CLexemes::Return);
            match toks.peek() {
                Some((CLexemes::Semicolon, _, _)) => {
                    toks.eat(CLexemes::Semicolon);
                    Ok(ASTNode::ReturnStatement(None, state.current_scope))
                }
                Some((_, _, _)) => {
                    let expr = parse_expr(toks, state)?;
                    eat_or_error!(toks, CLexemes::Semicolon)?;

                    let expr = Box::new(expr);
                    Ok(ASTNode::ReturnStatement(Some(expr), state.current_scope))
                }
                None => Err(ParseError::UnexpectedEOF.into()),
            }
        }
        Some((other, _, _)) => Err(ParseError::UnexpectedToken(other).into()),
        None => Err(ParseError::UnexpectedEOF.into()),
    }
}
