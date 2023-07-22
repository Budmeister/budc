//! Logic for analyzing `Expr`s. Notably, this file calculates 
//! the sizes of all types and calls `get_inter_instrs()`.
//! 
//! Author:     Brian Smith
//! Year:       2023

use std::collections::{HashMap, HashSet};

use crate::bud::{BinExpr, Expr, NonBinExpr, TypeExpr, VarDecl};
use crate::error::*;
use crate::logging::LoggingOptions;
use crate::tools::ToStringCollection;
use crate::{u_err, c_err};
use crate::{bud, parse::Node};
use log::*;

use crate::m68k::tools::*;
use crate::m68k::bottom::*;

use super::get_inter_instrs;

pub struct BudExpander {}
impl BudExpander {
    pub const BUILT_IN_TYPES: [&str; 3] = ["i8", "i16", "i32"];
    pub const REG_SIZE: u32 = 4;

    // This function should only be called once
    pub fn get_built_in_types() -> HashMap<TypeType, Type> {
        [
            (
                TypeType::Id("i8".to_owned()),
                Type {
                    size: Either::This(1),
                    typtyp: TypeType::Id("i8".to_owned()),
                    magic: true,
                },
            ),
            (
                TypeType::Id("i16".to_owned()),
                Type {
                    size: Either::This(2),
                    typtyp: TypeType::Id("i16".to_owned()),
                    magic: true,
                },
            ),
            (
                TypeType::Id("i32".to_owned()),
                Type {
                    size: Either::This(4),
                    typtyp: TypeType::Id("i32".to_owned()),
                    magic: true,
                },
            ),
        ]
        .into_iter()
        .collect()
    }

    pub fn new() -> BudExpander {
        BudExpander {}
    }

    pub fn get_types_in_expr(
        expr: &Expr,
        types: &mut HashSet<TypeType>,
        struct_names: &HashSet<String>,
        built_in: &HashMap<TypeType, Type>,
    ) -> Result<(), UserErr> {
        match &*expr.bin_expr {
            BinExpr::Binary(nbe, _, be, _) => {
                Self::get_types_in_nonbinexpr(nbe, types, struct_names, built_in)?;
                Self::get_types_in_binexpr(be, types, struct_names, built_in)
            }
            BinExpr::NonBin(nbe, _) => {
                Self::get_types_in_nonbinexpr(nbe, types, struct_names, built_in)
            }
        }
    }

    pub fn get_types_in_binexpr(
        be: &BinExpr,
        types: &mut HashSet<TypeType>,
        struct_names: &HashSet<String>,
        built_in: &HashMap<TypeType, Type>,
    ) -> Result<(), UserErr> {
        match be {
            BinExpr::Binary(nbe, _, be, _) => {
                Self::get_types_in_nonbinexpr(nbe, types, struct_names, built_in);
                Self::get_types_in_binexpr(be, types, struct_names, built_in)
            }
            BinExpr::NonBin(nbe, _) => {
                Self::get_types_in_nonbinexpr(nbe, types, struct_names, built_in)
            }
        }
    }

    pub fn get_types_in_nonbinexpr(
        nbe: &NonBinExpr,
        types: &mut HashSet<TypeType>,
        struct_names: &HashSet<String>,
        built_in: &HashMap<TypeType, Type>,
    ) -> Result<(), UserErr> {
        match nbe {
            NonBinExpr::BlockExpr(exprs, _) => exprs
                .iter()
                .try_for_each(|expr| Self::get_types_in_expr(expr, types, struct_names, built_in)),
            NonBinExpr::AssignExpr(_, expr, _) => {
                Self::get_types_in_expr(expr, types, struct_names, built_in)
            }
            NonBinExpr::VarDeclAssgn(typ, expr, _) => {
                TypeType::from_te(typ.typ.clone(), struct_names, built_in)?.add_subtypes(types);
                Self::get_types_in_expr(expr, types, struct_names, built_in)
            }
            NonBinExpr::ReturnExpr(expr, _) => {
                if let Some(expr) = expr {
                    Self::get_types_in_expr(expr, types, struct_names, built_in)
                } else {
                    Ok(())
                }
            }
            NonBinExpr::CleanupCall(_) => Ok(()),
            NonBinExpr::CleanupExpr(expr, _) => Self::get_types_in_expr(expr, types, struct_names, built_in),
            NonBinExpr::IdExpr(_, _) => Ok(()),
            NonBinExpr::LitExpr(lit, _) => {
                match lit {
                    bud::Literal::Num(_) => { TypeType::Id("i32".to_owned()).add_subtypes(types); },
                    bud::Literal::Str(_) => { TypeType::Pointer(Box::new(TypeType::Id("u8".to_owned()))).add_subtypes(types); },
                }
                Ok(())
            }
            NonBinExpr::ParenExpr(expr, _) => Self::get_types_in_expr(expr, types, struct_names, built_in),
            NonBinExpr::UnaryExpr(_, nbe, _) => Self::get_types_in_nonbinexpr(nbe, types, struct_names, built_in),
            NonBinExpr::IfExpr(expr1, expr2, _) => {
                Self::get_types_in_expr(expr1, types, struct_names, built_in)?;
                Self::get_types_in_expr(expr2, types, struct_names, built_in)
            },
            NonBinExpr::IfElse(expr1, expr2, expr3, _) => {
                Self::get_types_in_expr(expr1, types, struct_names, built_in)?;
                Self::get_types_in_expr(expr2, types, struct_names, built_in)?;
                Self::get_types_in_expr(expr3, types, struct_names, built_in)
            },
            NonBinExpr::UnlExpr(expr1, expr2, _) => {
                Self::get_types_in_expr(expr1, types, struct_names, built_in)?;
                Self::get_types_in_expr(expr2, types, struct_names, built_in)
            },
            NonBinExpr::UnlElse(expr1, expr2, expr3, _) => {
                Self::get_types_in_expr(expr1, types, struct_names, built_in)?;
                Self::get_types_in_expr(expr2, types, struct_names, built_in)?;
                Self::get_types_in_expr(expr3, types, struct_names, built_in)
            },
            NonBinExpr::WhileExpr(expr1, expr2, _) => {
                Self::get_types_in_expr(expr1, types, struct_names, built_in)?;
                Self::get_types_in_expr(expr2, types, struct_names, built_in)
            },
            NonBinExpr::DoWhile(expr1, expr2, _) => {
                Self::get_types_in_expr(expr1, types, struct_names, built_in)?;
                Self::get_types_in_expr(expr2, types, struct_names, built_in)
            },
            NonBinExpr::Break(_) => Ok(()),
            NonBinExpr::Continue(_) => Ok(()),
        }
    }

    pub fn get_types_in_funcs(
        funcs: &Vec<(VarDecl, Vec<VarDecl>, Box<Expr>)>,
        struct_names: &HashSet<String>,
        built_in: &HashMap<TypeType, Type>,
    ) -> Result<HashSet<TypeType>, UserErr> {
        let mut types = HashSet::new();
        for (_, args, expr) in funcs {
            for arg in args {
                TypeType::from_te(arg.typ.clone(), struct_names, built_in)?.add_subtypes(&mut types);
            }
            Self::get_types_in_expr(expr, &mut types, struct_names, built_in)?;
        }
        Ok(types)
    }

    pub fn code_generate(
        &self,
        log_options: &LoggingOptions,
        tree: Node<bud::BudTerminal, bud::BudNonTerminal>,
    ) -> Result<Environment, Vec<(BudErr, Option<String>)>> {
        impl From<BudErr> for Vec<(BudErr, Option<String>)> {
            fn from(value: BudErr) -> Self {
                vec![(value, None)]
            }
        }
        // Get Items
        let children;
        let range;
        if let Node::NonTm{ n: bud::BudNonTerminal::Items, children: children_, range: range_ } = tree {
            children = children_;
            range = range_;
        } else {
            let err: Result<_, BudErr> = c_err!(tree.get_range(),
                "Invalid node for {} {:?}",
                bud::BudNonTerminal::Items,
                tree
            );
            return Ok(err?);
        }
        let items = bud::Item::news(&children, range)?;

        // Separate items into funcs, structs, and imports
        let mut funcs = Vec::new();
        let mut func_names = Vec::new();
        let mut structs = Vec::new();
        let mut imports = Vec::new();

        for item in items {
            match item {
                bud::Item::FuncDecl(name, args, expr, _) => {
                    func_names.push(name.id.clone());
                    funcs.push((name, args, expr));
                }
                bud::Item::StructDecl(name, fields, _) => {
                    structs.push((name, fields));
                }
                bud::Item::ImportDecl(path, _) => imports.push(path),
            }
        }

        // Find all used types and size them
        let built_in = Self::get_built_in_types();
        let (mut environment, funcs) = Environment::new(log_options, structs, built_in, funcs)?;
        
        if log_options.print_types {
            debug!("Types found: ");
            environment.types
                .iter()
                .for_each(|(_, typ)| {
                    debug!("\t{}", typ);
                });
            debug!("End types found");
        }
        if log_options.print_inter_funcs {
            debug!("Funcs: {:?}", funcs.iter().map(|x| x.0.to_owned()).collect::<Vec<String>>());
        }

        // Compile all funcs
        let mut errors = Vec::new();
        for (name, func) in funcs {
            match func.compile(log_options, &environment) {
                Ok(cfunc) => environment.compiled_funcs.push(cfunc),
                Err(err) => errors.push((err, Some(name))),
            };
        }
        if log_options.print_inter_funcs {
            debug!("End inter funcs");
        }
        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(environment)
    }
}


type TypeSize = Either<u32, (u32, HashMap<String, (u32, TypeType)>)>;   // Choice<size, (size_of_struct, HashMap<field_name, (field_offset, field_type)>)>

// #[derive(Eq, PartialEq, Debug)]
enum TypeSizeState {
    NotCalculated,
    BeingCalculated,
    Calculated(TypeSize),
}
impl std::fmt::Debug for TypeSizeState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            TypeSizeState::NotCalculated => "NotCalculated".to_owned(),
            TypeSizeState::BeingCalculated => "BeingCalculated".to_owned(),
            TypeSizeState::Calculated(size) => {
                match size {
                    Either::This(size) => size.to_string(),
                    Either::That((size, _)) => size.to_string(),
                }
            }
        })
    }
}
// #[derive(Hash, Eq, PartialEq)]
struct TypeSizeGenerator {
    tt: TypeType,
    state: TypeSizeState,
}
impl std::fmt::Display for TypeSizeGenerator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "gen({}, {:?})", self.tt, self.state)
    }
}
impl TypeSizeGenerator {
    fn new(typtyp: TypeType) -> TypeSizeGenerator {
        TypeSizeGenerator {
            tt: typtyp,
            state: TypeSizeState::NotCalculated,
        }
    }
    fn from_type(typ: &Type) -> TypeSizeGenerator {
        TypeSizeGenerator {
            tt: typ.typtyp.clone(),
            state: TypeSizeState::Calculated(typ.size.clone()),
        }
    }
    fn get_size(
        size_generators: &mut HashMap<TypeType, TypeSizeGenerator>,
        index: TypeType,
        top: TypeType,
    ) -> Result<TypeSize, BudErr> {
        match size_generators.get_mut(&index) {
            Some(TypeSizeGenerator {
                state: TypeSizeState::Calculated(size),
                tt: _,
            }) => Ok(
                size.clone()
            ),
            Some(TypeSizeGenerator {
                state: TypeSizeState::BeingCalculated,
                tt: _,
            }) => u_err!(
                "In calculating size for type {}, encountered type loop with type {}",
                top, index
            ),
            Some(generator) => {
                match &mut generator.tt {
                    TypeType::Pointer(subtyp) => {
                        let subtyp_clone = (**subtyp).clone();
                        Self::get_size(size_generators, subtyp_clone, top)?;
                        size_generators.get_mut(&index).unwrap().state =
                            TypeSizeState::Calculated(Either::This(BudExpander::REG_SIZE));
                        Ok(Either::This(BudExpander::REG_SIZE))
                    }
                    TypeType::Array(subtyp, len) => {
                        let subtyp_clone = (**subtyp).clone();
                        let len = *len;
                        let (Either::This(subsize) | Either::That((subsize, _))) = Self::get_size(size_generators, subtyp_clone, top)?;
                        size_generators.get_mut(&index).unwrap().state =
                            TypeSizeState::Calculated(Either::This(subsize * len as u32));
                        Ok(Either::This(subsize * len as u32))
                    }
                    TypeType::Id(name) => {
                        // Must be a built-in type
                        c_err!("Built-in types must have their sizes predefined, but type {} did not have a defined size", name)
                    }
                    TypeType::Struct(_, Some(fields)) => {
                        let fields = fields.clone();
                        let sizes = fields
                            .iter()
                            .cloned()
                            .map(|field| {
                                size_generators.get_mut(&index).unwrap().state =
                                    TypeSizeState::BeingCalculated;                                         // This line is really
                                // generator.state = TypeSizeState::BeingCalculated;                        // this line
                                let size = Self::get_size(size_generators, field.tt, top.clone())?;   // I had to change it for the borrow checker
                                size_generators.get_mut(&index).unwrap().state =
                                    TypeSizeState::Calculated(size.clone());                                // And this line is really
                                // generator.state = TypeSizeState::Calculated(size);                       // this line
                                let (Either::This(size) | Either::That((size, _))) = size;
                                Ok(size)
                            })
                            .collect::<Result<Vec<u32>, BudErr>>()?;
                        let (layout, size) = Environment::get_struct_layout_from_sizes(&sizes);
                        let fields = fields
                            .iter()
                            .zip(layout)
                            .map(|(field, off)| (
                                field.name.to_owned(),
                                (
                                    off,
                                    field.tt.clone(),
                                )
                            ))
                            .collect();
                        Ok(Either::That((size, fields)))
                    }
                    TypeType::Struct(name, None) => c_err!(
                        "Struct {} not initialized with fields before calling get_size on type {}",
                        name, top
                    ),
                }
            }
            None => c_err!("Type not found: {}", index),
        }
    }
}

pub struct Environment {
    pub types: HashMap<TypeType, Type>,             // Give me a TypeType, and I'll give you a Type
    pub structs: HashMap<String, TypeType>,         // Give me a struct name, and I'll give you the TypeType for that struct
    pub global_funcs: HashMap<String, Signature>,   // Give me a funciton name, and I'll give you the signature
    pub compiled_funcs: Vec<CompiledFunction>,
}
impl Environment {
    // This is the only chance to add raw types
    // This function also creates the Function objects
    pub fn new(
        log_options: &LoggingOptions,
        structs: Vec<(String, Vec<VarDecl>)>,
        mut built_in: HashMap<TypeType, Type>,
        funcs: Vec<(VarDecl, Vec<VarDecl>, Box<Expr>)>,
    ) -> Result<(Environment, Vec<(String, Function)>), BudErr> {
        if log_options.print_types_trace {
            debug!("Types:");
        }
        // Get the names (TypeTypes) for all types so that
        // Type objects can be created later
        let mut struct_names = HashSet::new();
        let mut typetypes = HashSet::new();
        // Get the list of struct names
        for (name, _) in &structs {
            struct_names.insert(name.to_owned());
        }
        // Get a TypeType for each struct, verified to exist
        let mut struct_fields = Vec::new();
        for (name, fields) in &structs {
            typetypes.insert(TypeType::Struct(
                name.to_owned(),
                Some(
                    fields
                        .iter()
                        .map(|vd| {
                            let tt = TypeType::from_te(vd.typ.clone(), &struct_names, &built_in)?;
                            struct_fields.push(tt.clone());
                            let field = Field { name: name.to_owned(), tt };
                            Ok(field)
                        })
                        .collect::<Result<Vec<Field>, UserErr>>()?,
                ),
            ));
        }
        // Insert TypeType for each field of each struct (struct_fields was generated in the above loop)
        for field in struct_fields {
            typetypes.insert(field);
        }

        let types_in_functions: HashSet<TypeType> =
            BudExpander::get_types_in_funcs(&funcs, &struct_names, &built_in)?;
        if log_options.print_types_trace {
            debug!("Struct types: {}", typetypes.to_string());
            debug!("Built-in types: {}", crate::tools::to_string(&built_in));
            debug!("Types in functions: {}", &types_in_functions.to_string());
        }
        // Generate the sizes of all types
        // structs, built-ins, and types used in functions
        let mut size_generators = typetypes
            .iter()
            .map(|tt| (tt.clone(), TypeSizeGenerator::new(tt.clone())))
            .chain(
                built_in.values().map(|typ| (typ.typtyp.clone(), TypeSizeGenerator::from_type(typ))),
            )
            .chain(
                types_in_functions
                    .into_iter()
                    .filter(|typtyp| !built_in.contains_key(typtyp))
                    .map(|typtyp| (typtyp.clone(), TypeSizeGenerator::new(typtyp))),
            )
            .collect::<HashMap<TypeType, TypeSizeGenerator>>();
        if log_options.print_types_trace {
            debug!("Size generators: {}", crate::tools::to_string(&size_generators));
        }
        
        for tt in size_generators.keys().cloned().collect::<Vec<TypeType>>() {
            TypeSizeGenerator::get_size(&mut size_generators, tt.clone(), tt)?;
        }
        if log_options.print_types_trace {
            debug!("Size generators after get_size: {}", crate::tools::to_string(&size_generators));
        }

        // Create Type objects
        let types = size_generators
            .into_iter()
            .map(|(tt, gen)| {
                if let TypeSizeGenerator {
                    tt: _,
                    state: TypeSizeState::Calculated(size),
                } = gen
                {
                    Self::check_type_size(tt.clone(), &size)?;
                    if built_in.contains_key(&tt) {
                        let mut typ = built_in.remove(&tt).unwrap();
                        typ.size = size;
                        Ok((tt, typ))
                    } else {
                        Ok((
                            tt.clone(),
                            Type {
                                size,
                                typtyp: tt,
                                magic: false,
                            },
                        ))
                    }
                } else {
                    c_err!(
                        "get_size did not calculate the size for {}",
                        tt
                    )
                }
            })
            .collect::<Result<HashMap<TypeType, Type>, BudErr>>()?;
        
        let mut structs = HashMap::new();
        for tt in types.keys() {
            if let TypeType::Struct(name, _) = tt {
                structs.insert(name.to_owned(), tt.to_owned());
            }
        }
        let mut env = Environment { types, structs, global_funcs: HashMap::new(), compiled_funcs: Vec::new() };
        let funcs: Vec<(String, Function)> = funcs
            .into_iter()
            .map(|(name, args, expr)| {
                (
                    name.id.to_owned(),
                    Function::new(name, args, *expr, &env)
                )
            })
            .collect();
        for (name, func) in &funcs {
            env.global_funcs.insert(name.clone(), func.signature.clone());
        }

        if log_options.print_types_trace {
            debug!("End types");
        }
        Ok((env, funcs))
    }

    // Get the positon of each element in a struct and the size of the struct
    // Given the sizes of each element in the struct
    pub fn get_struct_layout_from_sizes(sizes: &[u32]) -> (Vec<u32>, u32) {
        let mut layout = Vec::new();
        let mut position = 0;
        for size in sizes {
            layout.push(position);
            position += size;
            if position % 2 != 0 {
                position += 1;
            }
        }
        (layout, position)
    }

    fn check_type_size(tt: TypeType, size: &Either<u32, (u32, HashMap<String, (u32, TypeType)>)>) -> Result<(), UserErr> {
        let (Either::This(size) | Either::That((size, _))) = size;
        if *size > i32::MAX as u32 {
            u_err!("Type {} is too big. Max size is {}, but {} has size of {}", tt, i32::MAX, tt, size)
        } else {
            Ok(())
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Field {
    pub tt: TypeType,
    pub name: String,
}
impl Field {
    pub fn new(vd: VarDecl, environment: &Environment) -> Field {
        Field {
            tt: TypeType::new(vd.typ, environment),
            name: vd.id,
        }
    }
}
impl std::fmt::Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.tt, self.name)
    }
}

pub struct CompiledEnvironment {
    pub types: HashMap<TypeType, Type>,
    pub funcs: Vec<CompiledFunction>,
}

// Type objects are valid types and used internally
// Only one Type object will exist for each unique type
#[derive(Eq, PartialEq)]
pub struct Type {
    pub size: TypeSize,
    pub typtyp: TypeType,
    pub magic: bool,
}
impl Type {
    pub fn get_size(&self) -> u32 {
        match self.size {
            Either::This(size) | Either::That((size, _)) => size
        }
    }
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}, {}{}>", self.typtyp.name(), match self.size {
            Either::This(size) => size.to_string(),
            Either::That((size, _)) => size.to_string(),
        }, if self.magic { ", magic" } else { "" })
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum TypeType {
    Struct(String, Option<Vec<Field>>),
    Pointer(Box<TypeType>),
    Array(Box<TypeType>, i32),
    Id(String),
}
impl TypeType {
    // Validate that ids used are valid - are struct names or built-in
    // "types_in_functions" will not create any id types (although it will create
    // composite types), so we do not need to have access to them for the purposes
    // of checking id types.
    // If a list of all typetypes used is needed, make sure to add all subtypes,
    // because if a composite type is created, the subtypes will not be automatically
    // added to the list
    pub fn from_te(
        typ: TypeExpr,
        struct_names: &HashSet<String>,
        built_in: &HashMap<TypeType, Type>,
    ) -> Result<TypeType, UserErr> {
        match typ {
            TypeExpr::Id(id, range) => {
                if struct_names.contains(&id) {
                    Ok(TypeType::Struct(id, None))
                } else if built_in.contains_key(&TypeType::Id(id.clone())) {
                    Ok(TypeType::Id(id))
                } else {
                    u_err!(range, "Type {} not found in structs or built-ins", id)
                }
            }
            TypeExpr::TypSqr(typ, sqr, _) => {
                let mut typ = TypeType::from_te(*typ, struct_names, built_in)?;
                for length in sqr {
                    typ = TypeType::Array(Box::new(typ), length);
                }
                Ok(typ)
            }
            TypeExpr::Pointer(typ, _) => Ok(TypeType::Pointer(Box::new(TypeType::from_te(
                *typ,
                struct_names,
                built_in,
            )?))),
        }
    }
    pub fn new(typ: TypeExpr, environment: &Environment) -> TypeType {
        match typ {
            TypeExpr::Id(id, _) => {
                if environment.structs.contains_key(&id) {
                    environment.structs.get(&id).unwrap().clone()
                } else {
                    TypeType::Id(id)
                }
            },
            TypeExpr::TypSqr(typ, sqr, _) => {
                let mut tt = TypeType::new(*typ, environment);
                for len in sqr {
                    tt = TypeType::Array(Box::new(tt), len);
                }
                tt
            }
            TypeExpr::Pointer(typ, _) => TypeType::Pointer(
                Box::new(TypeType::new(*typ, environment))
            ),
        }
    }

    // Also adds self
    pub fn add_subtypes(&self, types: &mut HashSet<TypeType>) {
        types.insert(self.clone());
        match self {
            TypeType::Pointer(typ) => typ.add_subtypes(types),
            TypeType::Array(typ, _) => typ.add_subtypes(types),
            TypeType::Struct(_, _) => {},
            TypeType::Id(_) => {},
        }
    }
    pub fn name(&self) -> String {
        match self {
            TypeType::Id(id) => id.to_owned(),
            TypeType::Pointer(typtyp) => "@(".to_owned() + &typtyp.name() + ")",
            TypeType::Array(typtyp, len) => typtyp.name() + "[" + &len.to_string() + "]",
            TypeType::Struct(name, _) => name.to_owned(),
        }
    }
}
impl std::fmt::Display for TypeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}>", self.name())
    }
}
impl std::fmt::Debug for TypeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<Field>,
}
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct StructLayout {
    pub fields: Vec<(String, u32)>,
}

#[derive(Clone)]
pub struct Signature {
    pub name: Field,
    pub args: Vec<Field>,
}

#[derive(Clone)]
pub struct Function {
    pub signature: Signature,
    pub expr: Expr,
}
impl Function {
    pub fn new(name: VarDecl, args: Vec<VarDecl>, expr: Expr, environment: &Environment) -> Function {
        let name = Field::new(name, environment);
        let args = args
            .into_iter()
            .map(|arg| {
                Field::new(arg, environment)
            })
            .collect();
        Function {
            signature: Signature { name, args },
            expr,
        }
    }
    pub fn compile(self, log_options: &LoggingOptions, env: &Environment) -> Result<CompiledFunction, BudErr> {
        let (instrs, fienv) = get_inter_instrs(self.expr, &self.signature, log_options, env)?;
        let instrs = get_instrs(instrs, fienv, env)?;
        let cfunc = CompiledFunction{ signature: self.signature, instructions: instrs };
        Ok(cfunc)
    }

}
