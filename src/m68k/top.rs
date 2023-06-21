use std::collections::{HashMap, HashSet};
use std::ops::RangeFrom;

use crate::bud::{BinExpr, Expr, NonBinExpr, TypeExpr, VarDecl};
use crate::logging::LoggingOptions;
use crate::tools::ToStringCollection;
use crate::{bud, parse::Node};
use log::*;

use crate::m68k::tools::*;
use crate::m68k::bottom::*;

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
    ) -> Result<(), String> {
        match &*expr.bin_expr {
            BinExpr::Binary(nbe, _, be) => {
                Self::get_types_in_nonbinexpr(nbe, types, struct_names, built_in)?;
                Self::get_types_in_binexpr(be, types, struct_names, built_in)
            }
            BinExpr::NonBin(nbe) => {
                Self::get_types_in_nonbinexpr(nbe, types, struct_names, built_in)
            }
        }
    }

    pub fn get_types_in_binexpr(
        be: &BinExpr,
        types: &mut HashSet<TypeType>,
        struct_names: &HashSet<String>,
        built_in: &HashMap<TypeType, Type>,
    ) -> Result<(), String> {
        match be {
            BinExpr::Binary(nbe, _, be) => {
                Self::get_types_in_nonbinexpr(nbe, types, struct_names, built_in)?;
                Self::get_types_in_binexpr(be, types, struct_names, built_in)
            }
            BinExpr::NonBin(nbe) => {
                Self::get_types_in_nonbinexpr(nbe, types, struct_names, built_in)
            }
        }
    }

    pub fn get_types_in_nonbinexpr(
        nbe: &NonBinExpr,
        types: &mut HashSet<TypeType>,
        struct_names: &HashSet<String>,
        built_in: &HashMap<TypeType, Type>,
    ) -> Result<(), String> {
        match nbe {
            NonBinExpr::BlockExpr(exprs) => exprs
                .iter()
                .try_for_each(|expr| Self::get_types_in_expr(expr, types, struct_names, built_in)),
            NonBinExpr::AssignExpr(_, expr) => {
                Self::get_types_in_expr(expr, types, struct_names, built_in)
            }
            NonBinExpr::VarDeclAssgn(typ, expr) => {
                TypeType::from_te(typ.typ.clone(), struct_names, built_in)?.add_subtypes(types);
                Self::get_types_in_expr(expr, types, struct_names, built_in)
            }
            NonBinExpr::ReturnExpr(expr) => {
                if let Some(expr) = expr {
                    Self::get_types_in_expr(expr, types, struct_names, built_in)
                } else {
                    Ok(())
                }
            }
            NonBinExpr::CleanupCall => Ok(()),
            NonBinExpr::CleanupExpr(expr) => Self::get_types_in_expr(expr, types, struct_names, built_in),
            NonBinExpr::IdExpr(_) => Ok(()),
            NonBinExpr::LitExpr(lit) => {
                match lit {
                    bud::Literal::Num(_) => { TypeType::Id("i32".to_owned()).add_subtypes(types); },
                    bud::Literal::Str(_) => { TypeType::Pointer(Box::new(TypeType::Id("u8".to_owned()))).add_subtypes(types); },
                }
                Ok(())
            }
            NonBinExpr::ParenExpr(expr) => Self::get_types_in_expr(expr, types, struct_names, built_in),
            NonBinExpr::UnaryExpr(_, nbe) => Self::get_types_in_nonbinexpr(nbe, types, struct_names, built_in),
            NonBinExpr::IfExpr(expr1, expr2) => {
                Self::get_types_in_expr(expr1, types, struct_names, built_in)?;
                Self::get_types_in_expr(expr2, types, struct_names, built_in)
            },
            NonBinExpr::IfElse(expr1, expr2, expr3) => {
                Self::get_types_in_expr(expr1, types, struct_names, built_in)?;
                Self::get_types_in_expr(expr2, types, struct_names, built_in)?;
                Self::get_types_in_expr(expr3, types, struct_names, built_in)
            },
            NonBinExpr::UnlExpr(expr1, expr2) => {
                Self::get_types_in_expr(expr1, types, struct_names, built_in)?;
                Self::get_types_in_expr(expr2, types, struct_names, built_in)
            },
            NonBinExpr::UnlElse(expr1, expr2, expr3) => {
                Self::get_types_in_expr(expr1, types, struct_names, built_in)?;
                Self::get_types_in_expr(expr2, types, struct_names, built_in)?;
                Self::get_types_in_expr(expr3, types, struct_names, built_in)
            },
            NonBinExpr::WhileExpr(expr1, expr2) => {
                Self::get_types_in_expr(expr1, types, struct_names, built_in)?;
                Self::get_types_in_expr(expr2, types, struct_names, built_in)
            },
            NonBinExpr::DoWhile(expr1, expr2) => {
                Self::get_types_in_expr(expr1, types, struct_names, built_in)?;
                Self::get_types_in_expr(expr2, types, struct_names, built_in)
            },
            NonBinExpr::Break => Ok(()),
            NonBinExpr::Continue => Ok(()),
        }
    }

    pub fn get_types_in_funcs(
        funcs: &Vec<(VarDecl, Vec<VarDecl>, Box<Expr>)>,
        struct_names: &HashSet<String>,
        built_in: &HashMap<TypeType, Type>,
    ) -> Result<HashSet<TypeType>, String> {
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
    ) -> Result<Environment, String> {
        let children;
        if let Node::NonTm(bud::BudNonTerminal::Items, children_) = tree {
            children = children_;
        } else {
            return Err(format!(
                "Invalid node for {} {:?}",
                bud::BudNonTerminal::Items,
                tree
            ));
        }
        let items = bud::Item::news(&children)?;

        let mut funcs = Vec::new();
        let mut func_names = Vec::new();
        let mut structs = Vec::new();
        let mut imports = Vec::new();

        for item in items {
            match item {
                bud::Item::FuncDecl(name, args, expr) => {
                    func_names.push(name.id.clone());
                    funcs.push((name, args, expr));
                }
                bud::Item::StructDecl(name, fields) => {
                    structs.push((name, fields));
                }
                bud::Item::ImportDecl(path) => imports.push(path),
            }
        }

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
        let mut num_errors = 0;
        if log_options.print_inter_funcs {
            debug!("Funcs: {:?}", funcs.iter().map(|x| x.0.to_owned()).collect::<Vec<String>>());
        }
        for (name, func) in funcs {
            match func.compile(log_options, 0.., &environment) {
                Ok(cfunc) => environment.compiled_funcs.push(cfunc),
                Err(msg) => {
                    error!("In function {},", name);
                    error!("{}", msg);
                    num_errors += 1;
                }
            };
        }
        if log_options.print_inter_funcs {
            debug!("End inter funcs");
        }
        if num_errors != 0 {
            return Err(format!("Unable to compile all functions because of errors in {} functions", num_errors));
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
    typtyp: TypeType,
    state: TypeSizeState,
}
impl std::fmt::Display for TypeSizeGenerator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "gen({}, {:?})", self.typtyp, self.state)
    }
}
impl TypeSizeGenerator {
    fn new(typtyp: TypeType) -> TypeSizeGenerator {
        TypeSizeGenerator {
            typtyp,
            state: TypeSizeState::NotCalculated,
        }
    }
    fn from_type(typ: &Type) -> TypeSizeGenerator {
        TypeSizeGenerator {
            typtyp: typ.typtyp.clone(),
            state: TypeSizeState::Calculated(typ.size.clone()),
        }
    }
    fn get_size(
        size_generators: &mut HashMap<TypeType, TypeSizeGenerator>,
        index: TypeType,
        top: TypeType,
    ) -> Result<TypeSize, String> {
        match size_generators.get_mut(&index) {
            Some(TypeSizeGenerator {
                state: TypeSizeState::Calculated(size),
                typtyp: _,
            }) => Ok(
                size.clone()
            ),
            Some(TypeSizeGenerator {
                state: TypeSizeState::BeingCalculated,
                typtyp: _,
            }) => Err(format!(
                "In calculating size for type {}, encountered type loop with type {}",
                top, index
            )),
            Some(generator) => {
                match &mut generator.typtyp {
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
                        Err(format!("Built-in types must have their sizes predefined, but type {} did not have a defined size", name))
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
                            .collect::<Result<Vec<u32>, String>>()?;
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
                    TypeType::Struct(name, None) => Err(format!(
                        "Struct {} not initialized with fields before calling get_size on type {}",
                        name, top
                    )),
                }
            }
            None => Err(format!("Type not found: {}", index)),
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
    ) -> Result<(Environment, Vec<(String, Function)>), String> {
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
                        .collect::<Result<Vec<Field>, String>>()?,
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
            .map(|(typtyp, gen)| {
                if let TypeSizeGenerator {
                    typtyp: typetype,
                    state: TypeSizeState::Calculated(size),
                } = gen
                {
                    if built_in.contains_key(&typtyp) {
                        let mut typ = built_in.remove(&typtyp).unwrap();
                        typ.size = size;
                        Ok((typetype, typ))
                    } else {
                        Ok((
                            typetype,
                            Type {
                                size,
                                typtyp,
                                magic: false,
                            },
                        ))
                    }
                } else {
                    Err(format!(
                        "get_size did not calculate the size for {}",
                        typtyp
                    ))
                }
            })
            .collect::<Result<HashMap<TypeType, Type>, String>>()?;
        
        let mut structs = HashMap::new();
        for tt in types.keys() {
            if let TypeType::Struct(name, _) = tt {
                structs.insert(name.to_owned(), tt.to_owned());
            }
        }
        let mut env = Environment { types, structs, global_funcs: HashMap::new(), compiled_funcs: Vec::new() };
        let funcs = funcs
            .into_iter()
            .map(|(name, args, expr)| {
                Ok((
                    name.id.to_owned(),
                    Function::new(name, args, *expr, &env)?
                ))
            })
            .collect::<Result<Vec<(String, Function)>, String>>()?;
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
    ) -> Result<TypeType, String> {
        match typ {
            TypeExpr::Id(id) => {
                if struct_names.contains(&id) {
                    Ok(TypeType::Struct(id, None))
                } else if built_in.contains_key(&TypeType::Id(id.clone())) {
                    Ok(TypeType::Id(id))
                } else {
                    Err(format!("Type {} not found in structs or built-ins", id))
                }
            }
            TypeExpr::TypSqr(typ, sqr) => {
                let mut typ = TypeType::from_te(*typ, struct_names, built_in)?;
                for length in sqr {
                    typ = TypeType::Array(Box::new(typ), length);
                }
                Ok(typ)
            }
            TypeExpr::Pointer(typ) => Ok(TypeType::Pointer(Box::new(TypeType::from_te(
                *typ,
                struct_names,
                built_in,
            )?))),
        }
    }
    pub fn new(typ: TypeExpr, environment: &Environment) -> TypeType {
        match typ {
            TypeExpr::Id(id) => {
                if environment.structs.contains_key(&id) {
                    environment.structs.get(&id).unwrap().clone()
                } else {
                    TypeType::Id(id)
                }
            },
            TypeExpr::TypSqr(typ, sqr) => {
                let mut tt = TypeType::new(*typ, environment);
                for len in sqr {
                    tt = TypeType::Array(Box::new(tt), len);
                }
                tt
            }
            TypeExpr::Pointer(typ) => TypeType::Pointer(
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
    pub fn new(name: VarDecl, args: Vec<VarDecl>, expr: Expr, environment: &Environment) -> Result<Function, String> {
        let name = Field::new(name, environment);
        let args = args
            .into_iter()
            .map(|arg| {
                Field::new(arg, environment)
            })
            .collect();
        Ok(Function {
            signature: Signature { name, args },
            expr,
        })
    }
    pub fn compile(self, log_options: &LoggingOptions, mut label_gen: RangeFrom<usize>, env: &Environment) -> Result<CompiledFunction, String> {
        let (instrs, fienv) = Self::get_inter_instrs(self.expr, &self.signature, log_options, &mut label_gen, env)?;
        let instrs = Self::get_instrs(instrs, fienv, env)?;
        let cfunc = CompiledFunction{ signature: self.signature, instructions: instrs };
        Ok(cfunc)
    }

}
