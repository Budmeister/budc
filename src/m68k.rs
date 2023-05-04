use std::collections::{HashMap, HashSet};
use std::ops::RangeFrom;

use crate::bud::{BinExpr, Expr, NonBinExpr, TypeExpr, VarDecl, BudBinop, IdExpr, Literal};
use crate::logging::LoggingOptions;
use crate::tools::ToStringCollection;
use crate::{bud, parse::Node};
use log::*;

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum Choice<T, U> {
    Some(T),
    Other(U),
}

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
                    size: 1,
                    typtyp: TypeType::Id("i8".to_owned()),
                    magic: true,
                },
            ),
            (
                TypeType::Id("i16".to_owned()),
                Type {
                    size: 2,
                    typtyp: TypeType::Id("i16".to_owned()),
                    magic: true,
                },
            ),
            (
                TypeType::Id("i32".to_owned()),
                Type {
                    size: 4,
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
                    bud::Literal::Num(_) => { TypeType::Id("i32".to_owned()); },
                    bud::Literal::Str(_) => { TypeType::Pointer(Box::new(TypeType::Id("u8".to_owned()))); },
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
        trace!("Building Item list");
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
        let (environment, funcs) = Environment::new(structs, built_in, funcs)?;

        debug!("Types found: ");
        environment.types
            .iter()
            .for_each(|(_, typ)| {
                debug!("\t{}", typ);
            });
        debug!("End types found");
        let mut num_errors = 0;
        trace!("Funcs: {:?}", funcs.keys());
        for (name, func) in funcs {
            match func.compile(0.., &environment) {
                Ok(_) => {},
                Err(msg) => {
                    error!("In function {},", name);
                    error!("{}", msg);
                    num_errors += 1;
                }
            };
        }
        if num_errors != 0 {
            return Err(format!("Unable to compile all functions because of errors in {} functions", num_errors));
        }

        Ok(environment)
    }
}

#[derive(Hash, Eq, PartialEq, Debug)]
enum TypeSizeState {
    NotCalculated,
    BeingCalculated,
    Calculated(u32),
}
#[derive(Hash, Eq, PartialEq)]
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
            state: TypeSizeState::Calculated(typ.size),
        }
    }
    fn get_size(
        size_generators: &mut HashMap<TypeType, TypeSizeGenerator>,
        index: TypeType,
        top: TypeType,
    ) -> Result<u32, String> {
        match size_generators.get_mut(&index) {
            Some(TypeSizeGenerator {
                state: TypeSizeState::Calculated(size),
                typtyp: _,
            }) => Ok(*size),
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
                        let subtyp_clone = (&**subtyp).clone();
                        Self::get_size(size_generators, subtyp_clone, top)?;
                        size_generators.get_mut(&index).unwrap().state =
                            TypeSizeState::Calculated(BudExpander::REG_SIZE);
                        Ok(BudExpander::REG_SIZE)
                    }
                    TypeType::Array(subtyp, len) => {
                        let subtyp_clone = (&**subtyp).clone();
                        let len = *len;
                        trace!("Getting size for array type {}", subtyp);
                        let subsize = Self::get_size(size_generators, subtyp_clone, top)?;
                        size_generators.get_mut(&index).unwrap().state =
                            TypeSizeState::Calculated(subsize * len as u32);
                        Ok(subsize * len as u32)
                    }
                    TypeType::Id(name) => {
                        // Must be a built-in type
                        Err(format!("Built-in types must have their sizes predefined, but type {} did not have a defined size", name))
                    }
                    TypeType::Struct(_, Some(fields)) => {
                        let sizes = fields
                            .iter()
                            .map(|tt| tt.clone())
                            .collect::<Vec<TypeType>>()
                            .into_iter()
                            .map(|tt| {
                                size_generators.get_mut(&index).unwrap().state =
                                    TypeSizeState::BeingCalculated;                                         // This line is really
                                // generator.state = TypeSizeState::BeingCalculated;                        // this line
                                let size = Self::get_size(size_generators, tt, top.clone())?;   // I had to change it for the borrow checker
                                size_generators.get_mut(&index).unwrap().state =
                                    TypeSizeState::Calculated(size);                                        // And this line is really
                                // generator.state = TypeSizeState::Calculated(size);                       // this line
                                Ok(size)
                            })
                            .collect::<Result<Vec<u32>, String>>()?;
                        let (_, size) = Environment::get_struct_layout_from_sizes(&sizes);
                        Ok(size)
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
    pub types: HashMap<TypeType, Type>,     // Give me a TypeType, and I'll give you a Type
    pub structs: HashMap<String, TypeType>, // Give me a struct name, and I'll give you the TypeType for that struct
}
impl Environment {
    // This is the only chance to add raw types
    // This function also creates the Function objects
    pub fn new(
        structs: Vec<(String, Vec<VarDecl>)>,
        mut built_in: HashMap<TypeType, Type>,
        funcs: Vec<(VarDecl, Vec<VarDecl>, Box<Expr>)>,
    ) -> Result<(Environment, HashMap<String, Function>), String> {
        debug!("Types:");
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
                            Ok(tt)
                        })
                        .collect::<Result<Vec<TypeType>, String>>()?,
                ),
            ));
        }
        // Insert TypeType for each field of each struct (struct_fields was generated in the above loop)
        for field in struct_fields {
            typetypes.insert(field);
        }

        debug!("Struct types: {}", typetypes.to_string());
        debug!("Built-in types: {}", crate::tools::to_string(&built_in));
        let types_in_functions: HashSet<TypeType> =
            BudExpander::get_types_in_funcs(&funcs, &struct_names, &built_in)?;
        debug!("Types in functions: {}", &types_in_functions.to_string());
        // Generate the sizes of all types
        // structs, built-ins, and types used in functions
        let mut size_generators = typetypes
            .iter()
            .map(|tt| (tt.clone(), TypeSizeGenerator::new(tt.clone())))
            .chain(
                built_in
                    .iter()
                    .map(|(_, typ)| (typ.typtyp.clone(), TypeSizeGenerator::from_type(&typ))),
            )
            .chain(
                types_in_functions
                    .into_iter()
                    .filter(|typtyp| !built_in.contains_key(typtyp))
                    .map(|typtyp| (typtyp.clone(), TypeSizeGenerator::new(typtyp))),
            )
            .collect::<HashMap<TypeType, TypeSizeGenerator>>();
        debug!("Size generators: {}", crate::tools::to_string(&size_generators));
        
        for tt in size_generators.keys().map(|tt| tt.clone()).collect::<Vec<TypeType>>() {
            TypeSizeGenerator::get_size(&mut size_generators, tt.clone(), tt)?;
        }
        debug!("Size generators after get_size: {}", crate::tools::to_string(&size_generators));

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
        for (tt, _) in &types {
            if let TypeType::Struct(name, _) = tt {
                structs.insert(name.to_owned(), tt.to_owned());
            }
        }
        let env = Environment { types, structs };
        let funcs = funcs
            .into_iter()
            .map(|(name, args, expr)| {
                Ok((
                    name.id.to_owned(),
                    Function::new(name, args, expr, &env)?
                ))
            })
            .collect::<Result<HashMap<String, Function>, String>>()?;

        debug!("End types");
        Ok((env, funcs))
    }

    // Get the positon of each element in a struct and the size of the struct
    // Given the sizes of each element in the struct
    pub fn get_struct_layout_from_sizes(sizes: &Vec<u32>) -> (Vec<u32>, u32) {
        let mut layout = sizes.clone();
        let mut position = 0;
        for (i, size) in sizes.iter().enumerate() {
            layout[i] = position;
            position += size;
            if position % 2 != 0 {
                position += 1;
            }
        }
        (layout, position)
    }

    pub fn compile(self, funcs: Vec<Function>, env: &Environment) -> Result<CompiledEnvironment, String> {
        Ok(CompiledEnvironment { types: self.types, funcs: funcs
            .into_iter()
            .map(|func| func.compile(0.., env))
            .collect::<Result<Vec<CompiledFunction>, String>>()?
        })
    }
}

pub struct CompiledEnvironment {
    pub types: HashMap<TypeType, Type>,
    pub funcs: Vec<CompiledFunction>,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Field {
    pub typ: TypeType,
    pub name: String,
}
impl Field {
    pub fn new(vd: VarDecl, environment: &Environment) -> Field {
        Field {
            typ: TypeType::new(vd.typ, environment),
            name: vd.id,
        }
    }
}
impl std::fmt::Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.typ, self.name)
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

// Type objects are valid types and used internally
// Only one Type object will exist for each unique type
#[derive(Eq, PartialEq)]
pub struct Type {
    pub size: u32,
    pub typtyp: TypeType,
    magic: bool,
}
impl Type {}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}, {}{}>", self.typtyp.to_string(), self.size, if self.magic { ", magic" } else { "" })
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum TypeType {
    Struct(String, Option<Vec<TypeType>>),
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
            TypeExpr::Array(typ, size) => Ok(TypeType::Array(
                Box::new(TypeType::from_te(*typ, struct_names, built_in)?),
                size,
            )),
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
            TypeExpr::Array(typ, size) => TypeType::Array(
                Box::new(TypeType::new(*typ, environment)),
                size
            ),
            TypeExpr::Pointer(typ) => TypeType::Pointer(
                Box::new(TypeType::new(*typ, environment))
            ),
        }
    }
    fn to_string(&self) -> String {
        match self {
            TypeType::Id(id) => id.to_owned(),
            TypeType::Pointer(typtyp) => "*(".to_owned() + &typtyp.to_string() + ")",
            TypeType::Array(typtyp, len) => typtyp.to_string() + "[" + &len.to_string() + "]",
            TypeType::Struct(name, _) => name.to_owned(),
        }
    }
    fn is_magic(&self, env: &Environment) -> bool {
        match env.types.get(self) {
            Some(t) => t.magic,
            None => {
                error!("Type {} not found in env.types", self);
                panic!()
            }
        }
    }
    fn is_struct(&self) -> bool {
        if let TypeType::Struct(_, _) = self {
            true
        } else {
            false
        }
    }
    fn is_pointer(&self) -> bool {
        if let TypeType::Pointer(_) = self {
            true
        } else {
            false
        }
    }
    fn is_array(&self) -> bool {
        if let TypeType::Array(_, _) = self {
            true
        } else {
            false
        }
    }
    fn is_id(&self) -> bool {
        if let TypeType::Id(_) = self {
            true
        } else {
            false
        }
    }
    fn is_void(&self) -> bool {
        *self == TypeType::Id("void".to_owned())
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
}
impl std::fmt::Display for TypeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}>", self.to_string())
    }
}

#[derive(Clone)]
pub struct Signature {
    pub name: Field,
    pub args: Vec<Field>,
}


pub struct FunctionEnvironment {
    pub lit_strings: Vec<String>,
}
impl FunctionEnvironment {

}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum Place {
    Var(Field),
    AReg(AReg, TypeType),
    DReg(DReg, TypeType),
    Ref(Box<Place>, Choice<Box<Place>, i32>),    // (location of the pointer, offset)
}
impl Place {
    pub fn get_type(&self) -> TypeType {
        match self {
            Place::Var(f) => {
                f.typ.clone()
            }
            Place::AReg(_, tt) => {
                tt.clone()
            }
            Place::DReg(_, tt) => {
                tt.clone()
            }
            Place::Ref(place, _) => {
                place.get_type()
            }
        }
    }
    pub fn get_size(&self, env: &Environment) -> u32 {
        env.types.get(&self.get_type()).unwrap().size
    }
    // Returns None if the size is not a Byte, Word, or LWord
    pub fn get_data_size(&self, env: &Environment) -> Option<DataSize> {
        let size = self.get_size(env);
        match size {
            8 => Some(DataSize::Byte),
            16 => Some(DataSize::Word),
            32 => Some(DataSize::LWord),
            _ => None,
        }
    }
}
impl std::fmt::Display for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Place::Var(field) => field.to_string(),
            Place::AReg(areg, tt) => format!("{} {}", tt, areg),
            Place::DReg(dreg, tt) => format!("{} {}", tt, dreg),
            Place::Ref(place, offset) => {
                match offset {
                    Choice::Some(off_place) => format!("{}[{}]", place, off_place),
                    Choice::Other(lit) => format!("{}[{}]", place, lit),
                }
            },
        })
    }
}
impl std::fmt::Debug for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

// For telling an expression where to put its output
#[derive(Clone)]
pub enum ReturnPlan {
    Binop(BudBinop, Place), // Place must have a magic type
    Move(Place),
    None,
}
impl ReturnPlan {
    // Return None if there is no return plan
    pub fn into_inter_instr(self, from: Place, env: &Environment) -> Result<Option<Vec<InterInstr>>, String> {
        match self {
            ReturnPlan::Binop(b, to) => {
                let instr = InterInstr::Binop(
                    match to.get_data_size(env) {
                        Some(ds) => ds,
                        None => {
                            return Err(format!("Trying to do binop {} on type {}", b, to.get_type()));
                        }
                    },
                    from, b, to);
                Ok(Some(vec![instr]))
            }
            ReturnPlan::Move(to) => Ok(Some(InterInstr::move_mem(from, to, env))),
            ReturnPlan::None => Ok(None),
        }
    }
    // Return None if there is no return plan
    pub fn imm_into_inter_instr(self, from: Imm, env: &Environment) -> Result<Option<InterInstr>, String> {
        match self {
            ReturnPlan::Binop(b, to) => {
                let size = Self::get_imm_size(&from, &to, env)?;
                let instr = InterInstr::Binopi(from.change_size(size), b, to);
                Ok(Some(instr))
            }
            ReturnPlan::Move(to) => {
                let size = Self::get_imm_size(&from, &to, env)?;
                let instr = InterInstr::Movi(from.change_size(size), to);
                Ok(Some(instr))
            }
            ReturnPlan::None => Ok(None),
        }
    }
    fn get_imm_size(from: &Imm, to: &Place, env: &Environment) -> Result<DataSize, String> {
        if !to.get_type().is_magic(env) {
            return Err(format!("Cannot coerce immediate value {} to non-magic type {}", from, to.get_type()));
        }
        match to.get_data_size(env) {
            Some(ds) => Ok(ds),
            None => Err(format!("Magic type {} has invalid size, {}", to.get_type(), to.get_size(env))),
        }
    }
}

// I am using these Intermediate Instructions to represent 
// instructions before I have generated the stack frame
#[derive(Debug)]
pub enum InterInstr {
    Binop(DataSize, Place, BudBinop, Place),    // Dest. on right (SUB subtracts src from dest. and stores in dest.; DIV, too)
    Binopi(Imm, BudBinop, Place),
    Move(DataSize, Place, Place),
    Movi(Imm, Place),
    MoveSP(i32),    // for calling functions
    Call(String),
    Lbl(usize),
    Goto(usize),
    Bcc(usize),
    Bcs(usize),
    Beq(usize),
    Bge(usize),
    Bgt(usize),
    Ble(usize),
    Blt(usize),
    Bmi(usize),
    Bne(usize),
    Bpl(usize),
    Bra(usize),
}
// Common instruction patterns
impl InterInstr {
    pub fn move_mem(from: Place, to: Place, env: &Environment) -> Vec<InterInstr> {
        match to.get_data_size(env) {
            Some(ds) => vec![InterInstr::Move(ds, from, to)],
            None => {
                todo!()
            }
        }

    }
}

pub struct FunctionInterEnvironment {
    pub vars: HashMap<String, (Field, bool)>,
    pub dregs: [(DReg, bool); 8],
    pub aregs: [(AReg, bool); 8],
}
// Places are variables (local and temp) and register
impl FunctionInterEnvironment {
    pub fn new(args: &Vec<Field>, d0_used: bool) -> FunctionInterEnvironment {
        FunctionInterEnvironment {
            vars: args
                .iter()
                .map(|f| (f.name.clone(), (f.clone(), true)))
                .collect(),
            dregs: [
                (D0, d0_used),
                (D1, false),
                (D2, false),
                (D3, false),
                (D4, false),
                (D5, false),
                (D6, false),
                (D7, false),
            ],
            aregs: [
                (A0, false),
                (A1, false),
                (A2, false),
                (A3, false),
                (A4, false),
                (A5, false),
                (A6, false),
                (SP, true),
            ]
        }
    }
    pub fn var_name_from_temp(temp: usize) -> String {
        format!("[t{}]", temp)
    }
    fn _get_temp(&mut self, tt: TypeType) -> Place {
        for temp in 0.. {
            let name = Self::var_name_from_temp(temp);
            match self.vars.get_mut(&name.clone()) {
                Some((f, used)) => {
                    if !*used {
                        // Variables cannot change type in the middle of the program
                        if f.typ == tt {
                            *used = true;
                            return Place::Var(f.clone());
                        } else {
                            let f_clone = f.clone();
                            self.vars.insert(name.clone(), (Field{ typ: tt, name: name.clone() }, true));
                            return Place::Var(f_clone);
                        }
                    }
                },
                None => {
                    self.vars.insert(name.clone(), (Field{ typ: tt.clone(), name: name.clone() }, true));
                    return Place::Var(Field { name, typ: tt });
                }
            }
        }
        panic!("Iterator 0.. had 0 elements??")
    }
    // Temp variables are stored in vars like "[t0]"
    pub fn get_data_place(&mut self, tt: TypeType, env: &Environment) -> Place {
        // If the requested Place could fit in a register
        let size = match env.types.get(&tt) {
            Some(typ) => typ.size,
            None => panic!("Invalid typetype found: {}", tt),
        };
        if size <= BudExpander::REG_SIZE {
            // If a data register is available, give it out
            for (dreg, used) in &mut self.dregs {
                if !*used {
                    *used = true;
                    return Place::DReg(*dreg, tt);
                }
            }
        }
        // Else, see if a temporary variable is available
        self._get_temp(tt)
    }
    pub fn get_addr_place(&mut self, tt: TypeType, env: &Environment) -> Place {
        // If the requested Place could fit in a register
        let size = match env.types.get(&tt) {
            Some(typ) => typ.size,
            None => panic!("Invalid typetype found: {}", tt),
        };
        if size <= BudExpander::REG_SIZE {
            // If a addr register is available, give it out
            for (areg, used) in &mut self.aregs {
                if !*used {
                    *used = true;
                    return Place::AReg(*areg, tt);
                }
            }
        }
        // Else, see if a temporary variable is available
        self._get_temp(tt)
    }
    pub fn free_place(&mut self, place: &Place) {
        match place {
            Place::Var(id) => {
                if let Some((_, used)) = self.vars.get_mut(&id.name) {
                    *used = false;
                }
            },
            Place::AReg(a, _) => {
                self.aregs[*a as usize].1 = false;
            },
            Place::DReg(d, _) => {
                self.dregs[*d as usize].1 = false;
            },
            Place::Ref(_, _) => {},
        }
    }
    // Saves all used regs by pushing them to the stack
    pub fn save_regs(instrs: &mut Vec<InterInstr>) -> Vec<ADReg> {
        todo!()
    }
    // Retrieves saved regs from the stack
    pub fn retrieve_regs(regs: Vec<ADReg>, instrs: &mut Vec<InterInstr>) {
        todo!()
    }

}


#[derive(Clone)]
pub struct Function {
    pub signature: Signature,
    pub expr: Expr,
}
impl Function {
    pub fn new(name: VarDecl, args: Vec<VarDecl>, expr: Box<Expr>, environment: &Environment) -> Result<Function, String> {
        let name = Field::new(name, environment);
        let args = args
            .into_iter()
            .map(|arg| {
                Field::new(arg, environment)
            })
            .collect();
        Ok(Function {
            signature: Signature { name, args },
            expr: *expr,
        })
    }
    pub fn get_name(&self) -> String {
        self.signature.name.name.to_owned()
    }
    pub fn preable(&self, instructions: &mut Vec<Instruction>) {
        todo!()
    }
    pub fn compile(self, mut label_gen: RangeFrom<usize>, env: &Environment) -> Result<CompiledFunction, String> {
        let mut instrs = Vec::new();
        let mut fienv = FunctionInterEnvironment::new(&self.signature.args, false);
        let plan;
        if self.signature.name.typ.is_void() {
            plan = ReturnPlan::None;
        } else if self.signature.name.typ.is_array() || self.signature.name.typ.is_struct() {
            todo!("Implement returning arrays and structs from functions")
        } else {
            plan = ReturnPlan::Move(Place::DReg(D0, self.signature.name.typ));
        }
        Self::compile_expr(
            self.expr,
            plan,
            &mut instrs,
            &mut label_gen,
            &mut fienv,
            env,
        )?;
        // Things we need to know (from searching the function expression):
        //  * String literals
        //  * All local variables (can be Fields)
        trace!("Instructions for function {}", self.signature.name.name);
        for instr in &instrs {
            trace!("{:?}", instr);
        }
        todo!()
    }
    pub fn compile_expr(expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        Self::compile_bin_expr(*expr.bin_expr,
            if expr.with_semicolon {
                ReturnPlan::None
            } else {
                plan
            },
            instrs, label_gen, fienv, env)?;
        Ok(())
    }
    pub fn compile_bin_expr(be: BinExpr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        match be {
            BinExpr::Binary(nbe, b, be) => {
                match plan {
                    ReturnPlan::Binop(pb, place) => {
                        // Make the temporary variable with the same type as the ReturnPlan
                        // As a future optimization, we do not need to get a new place if
                        // both binops are the same and if the binop is associative
                        let temp = fienv.get_data_place(place.get_type(), env);
                        let plan = ReturnPlan::Move(temp.clone());
                        Self::compile_non_bin_expr(*nbe, plan, instrs, label_gen, fienv, env)?;
                        let plan = ReturnPlan::Binop(b, temp.clone());
                        Self::compile_bin_expr(*be, plan, instrs, label_gen, fienv, env)?;
                        let ret_action = ReturnPlan::Binop(pb, place)
                            .into_inter_instr(temp.clone(), env)?;
                        fienv.free_place(&temp);
                        match ret_action {
                            Some(mut ret_instrs) => instrs.append(&mut ret_instrs),
                            None => {},
                        }
                    },
                    ReturnPlan::Move(place) => {
                        let plan = ReturnPlan::Move(place.clone());
                        Self::compile_non_bin_expr(*nbe, plan, instrs, label_gen, fienv, env)?;
                        let plan = ReturnPlan::Binop(b, place);
                        Self::compile_bin_expr(*be, plan, instrs, label_gen, fienv, env)?;
                    },
                    ReturnPlan::None => {
                        Self::compile_non_bin_expr(*nbe, plan.clone(), instrs, label_gen, fienv, env)?;
                        Self::compile_bin_expr(*be, plan, instrs, label_gen, fienv, env)?;
                    }
                }

            },
            BinExpr::NonBin(nbe) => {
                Self::compile_non_bin_expr(*nbe, plan, instrs, label_gen, fienv, env)?;
            },
        }
        Ok(())
    }
    pub fn compile_non_bin_expr(nbe: NonBinExpr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        match nbe {
            NonBinExpr::BlockExpr(exprs)        => Self::compile_block_expr(exprs, plan, instrs, label_gen, fienv, env)?,
            NonBinExpr::AssignExpr(id, expr)    => Self::compile_assign_expr(*id, *expr, plan, instrs, label_gen, fienv, env)?,
            NonBinExpr::VarDeclAssgn(_, _)  => warn!("Not implemented"),
            NonBinExpr::ReturnExpr(_)       => warn!("Not implemented"),
            NonBinExpr::CleanupCall         => warn!("Not implemented"),
            NonBinExpr::CleanupExpr(_)      => warn!("Not implemented"),
            NonBinExpr::IdExpr(_)           => warn!("Not implemented"),
            NonBinExpr::LitExpr(_)          => warn!("Not implemented"),
            NonBinExpr::ParenExpr(_)        => warn!("Not implemented"),
            NonBinExpr::UnaryExpr(_, _)     => warn!("Not implemented"),
            NonBinExpr::IfExpr(_, _)        => warn!("Not implemented"),
            NonBinExpr::IfElse(_, _, _)     => warn!("Not implemented"),
            NonBinExpr::UnlExpr(_, _)       => warn!("Not implemented"),
            NonBinExpr::UnlElse(_, _, _)    => warn!("Not implemented"),
            NonBinExpr::WhileExpr(_, _)     => warn!("Not implemented"),
            NonBinExpr::DoWhile(_, _)       => warn!("Not implemented"),
            NonBinExpr::Break               => warn!("Not implemented"),
            NonBinExpr::Continue            => warn!("Not implemented"),
        }
        Ok(())
    }
    pub fn compile_block_expr(mut exprs: Vec<Expr>, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
                let last = match exprs.pop() {
                    Some(expr) => expr,
                    None => {
                        // Empty block expression
                        match &plan {
                            ReturnPlan::None => {
                                return Ok(());
                            },
                            ReturnPlan::Binop(b, place) => {
                                return Err(format!("Empty block expression but expected to return to a binop expression, {} at {}", b, place));
                            }
                            ReturnPlan::Move(place) => {
                                return Err(format!("Empty block expression but expected to move result to {}", place));
                            }
                        }
                    },
                };

                for expr in exprs {
                    Self::compile_expr(expr, ReturnPlan::None, instrs, label_gen, fienv, env)?;
                    // Check that the expressions have semicolons?
                    // If we were to print errors and recover, this would be a good point to do it
                }
                Self::compile_expr(last, plan, instrs, label_gen, fienv, env)?;
                Ok(())
    }
    pub fn compile_assign_expr(id: IdExpr, expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        // Check that the variable exists
        let (place, to_free) = Self::place_from_id_expr(id, instrs, label_gen, fienv, env)?;
        let assign_plan = ReturnPlan::Move(place.clone());
        Self::compile_expr(expr, assign_plan, instrs, label_gen, fienv, env)?;
        for tf in to_free {
            fienv.free_place(&tf);
        }
        plan.into_inter_instr(place, env)?;
        Ok(())
    }
    // (id place, places of calculated offsets if any)
    pub fn place_from_id_expr(id: IdExpr, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(Place, Vec<Place>), String> {
        match id {
            IdExpr::SquareIndex(id, offset) => {
                // The id must be assignable
                match *id.bin_expr {
                    BinExpr::NonBin(nbe) => {
                        if let NonBinExpr::IdExpr(id_expr) = *nbe {
                            let (id_place, mut to_free) = Self::place_from_id_expr(*id_expr, instrs, label_gen, fienv, env)?;
                            // Check if the offset is a literal value
                            let mut lit = None;
                            if let BinExpr::NonBin(nbe) = &*offset.bin_expr {
                                if let NonBinExpr::LitExpr(Literal::Num(num)) = &**nbe {
                                    lit = Some(*num);
                                }
                            }
                            match lit {
                                Some(num) => {
                                    let place = Place::Ref(Box::new(id_place), Choice::Other(num));
                                    return Ok((place, to_free));
                                }
                                None => {
                                    let off_place = fienv.get_data_place(TypeType::Id("i32".to_owned()), env);
                                    to_free.push(off_place.clone());
                                    let plan = ReturnPlan::Move(off_place.clone());
                                    Self::compile_expr(*offset, plan, instrs, label_gen, fienv, env)?;
                                    let place = Place::Ref(Box::new(id_place), Choice::Some(Box::new(off_place)));
                                    return Ok((place, to_free));
                                }
                            }
                        }
                        // id was a NonBinExpr but not an IdExpr
                        Err(format!("Expression not assignable"))
                    }
                    BinExpr::Binary(_, b, _) => {
                        // id was not a NonBinExpr (it was a BinExpr)
                        Err(format!("Cannot assign to binary expression, {}", b))
                    }
                }
            },
            IdExpr::RoundIndex(_, _) => todo!(),
            IdExpr::Id(id) => {
                // We need to store the result of expr into the variable, id
                let place = match fienv.vars.get(&id) {
                    Some((field, _)) => {
                        Place::Var(field.clone())
                    },
                    None => {
                        return Err(format!("Undeclared variable {}", id));
                    }
                };
                Ok((place, Vec::new()))
            },
            IdExpr::Reference(id_expr) => {
                // We need to store the result of expr into the memory
                // location pointed to by the variable, id
                let (id_place, to_free) = Self::place_from_id_expr(*id_expr, instrs, label_gen, fienv, env)?;
                let place = Place::Ref(Box::new(id_place), Choice::Other(0));
                Ok((place, to_free))
            },
        }
    }
}

#[derive(Clone)]
pub struct CompiledFunction {
    pub signature: Signature,
    pub instructions: Vec<Instruction>,
}

use DReg::*;
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum DReg {
    D0,
    D1,
    D2,
    D3,
    D4,
    D5,
    D6,
    D7,
}
impl std::fmt::Display for DReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            DReg::D0 => "d0",
            DReg::D1 => "d1",
            DReg::D2 => "d2",
            DReg::D3 => "d3",
            DReg::D4 => "d4",
            DReg::D5 => "d5",
            DReg::D6 => "d6",
            DReg::D7 => "d7",
        })
    }
}

use AReg::*;
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum AReg {
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    SP,
}
impl std::fmt::Display for AReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            AReg::A0 => "a0",
            AReg::A1 => "a1",
            AReg::A2 => "a2",
            AReg::A3 => "a3",
            AReg::A4 => "a4",
            AReg::A5 => "a5",
            AReg::A6 => "a6",
            AReg::SP => "sp",
        })
    }
}

use ADReg::*;
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum ADReg {
    D(DReg),
    A(AReg),
}
impl std::fmt::Display for ADReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            ADReg::D(d) => d.to_string(),
            ADReg::A(a) => a.to_string(),
        })
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum GReg {
    D(DReg),
    A(AReg),
    PC,
}
impl std::fmt::Display for GReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            GReg::D(d) => d.to_string(),
            GReg::A(a) => a.to_string(),
            GReg::PC => "pc".to_owned(),
        })
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum DataSize {
    Byte,
    Word,
    LWord,
}
impl std::fmt::Display for DataSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            DataSize::Byte => "b",
            DataSize::Word => "w",
            DataSize::LWord => "l",
        })
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Imm {
    Byte(i8),
    Word(i16),
    LWord(i32),
}
impl Imm {
    pub fn change_size(self, size: DataSize) -> Imm {
        let val = match self {
            Imm::Byte(v) => v as i32,
            Imm::Word(v) => v as i32,
            Imm::LWord(v) => v,
        };
        match size {
            DataSize::Byte => Imm::Byte(val as i8),
            DataSize::Word => Imm::Word(val as i16),
            DataSize::LWord => Imm::LWord(val),
        }
    }
}
impl std::fmt::Display for Imm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Imm::Byte(v) => write!(f, "{}.b", v.to_string()),
            Imm::Word(v) => write!(f, "{}.w", v.to_string()),
            Imm::LWord(v) => write!(f, "{}.l", v.to_string()),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum NumOrLbl {
    Num(i32),
    Lbl(String),
}
impl std::fmt::Display for NumOrLbl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumOrLbl::Num(num) => write!(f, "{}", num),
            NumOrLbl::Lbl(lbl) => write!(f, "{}", lbl),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum AddrMode {
    D(DReg),                            // Data
    A(AReg),                            // Address
    AInd(AReg),                         // Address indirect
    AIndInc(AReg),                      // Address indirect post-increment
    AIndDec(AReg),                      // Address indirect pre-decrement
    AIndDisp(NumOrLbl, AReg),           // Address indirect displacement
    AIndIdxDisp(NumOrLbl, AReg, ADReg), // Address indirect indexed displacement
    AbsW(i16),                          // Absolute word
    AbsL(NumOrLbl),                     // Absolute long word
    PCIndDisp(NumOrLbl),                // PC indirect displacement
    PCIndIdxDisp(NumOrLbl, ADReg),      // PC indirect indexed displacement
    ImmW(i16),                          // Immediate word
    ImmL(NumOrLbl),                     // Immediate long word
}
impl std::fmt::Display for AddrMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AddrMode::D(d)                    => write!(f, "{}", d),
            AddrMode::A(a)                    => write!(f, "{}", a),
            AddrMode::AInd(a)                 => write!(f, "({})", a),
            AddrMode::AIndInc(a)              => write!(f, "({})+", a),
            AddrMode::AIndDec(a)              => write!(f, "-({})", a),
            AddrMode::AIndDisp(i, a)          => write!(f, "{}({})", i, a),
            AddrMode::AIndIdxDisp(i, a, ad)    => write!(f, "({}, {}, {})", i, a, ad),
            AddrMode::AbsW(abs)                 => write!(f, "{}", abs),
            AddrMode::AbsL(abs)                 => write!(f, "{}", abs),
            AddrMode::PCIndDisp(i)            => write!(f, "{}({})", i, GReg::PC),
            AddrMode::PCIndIdxDisp(i, ad)      => write!(f, "({}, {}, {})", i, GReg::PC, ad),
            AddrMode::ImmW(imm)                 => write!(f, "#{}", imm),
            AddrMode::ImmL(imm)                 => write!(f, "#{}", imm),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct ADBitField {
    d0: bool,
    d1: bool,
    d2: bool,
    d3: bool,
    d4: bool,
    d5: bool,
    d6: bool,
    d7: bool,
    a0: bool,
    a1: bool,
    a2: bool,
    a3: bool,
    a4: bool,
    a5: bool,
    a6: bool,
    sp: bool,
}
impl ADBitField {
    pub fn new(regs: &[ADReg]) -> ADBitField {
        ADBitField {
            d0: regs.contains(&D(D0)),
            d1: regs.contains(&D(D1)),
            d2: regs.contains(&D(D2)),
            d3: regs.contains(&D(D3)),
            d4: regs.contains(&D(D4)),
            d5: regs.contains(&D(D5)),
            d6: regs.contains(&D(D6)),
            d7: regs.contains(&D(D7)),
            a0: regs.contains(&A(A0)),
            a1: regs.contains(&A(A1)),
            a2: regs.contains(&A(A2)),
            a3: regs.contains(&A(A3)),
            a4: regs.contains(&A(A4)),
            a5: regs.contains(&A(A5)),
            a6: regs.contains(&A(A6)),
            sp: regs.contains(&A(SP)),
        }
    }
}
impl std::fmt::Display for ADBitField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &format!("{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}", 
            if self.d0 { "/d0" } else { "" },
            if self.d1 { "/d1" } else { "" },
            if self.d2 { "/d2" } else { "" },
            if self.d3 { "/d3" } else { "" },
            if self.d4 { "/d4" } else { "" },
            if self.d5 { "/d5" } else { "" },
            if self.d6 { "/d6" } else { "" },
            if self.d7 { "/d7" } else { "" },
            if self.a0 { "/a0" } else { "" },
            if self.a1 { "/a1" } else { "" },
            if self.a2 { "/a2" } else { "" },
            if self.a3 { "/a3" } else { "" },
            if self.a4 { "/a4" } else { "" },
            if self.a5 { "/a5" } else { "" },
            if self.a6 { "/a6" } else { "" },
            if self.sp { "/sp" } else { "" },
        )[1..])
    }
}

use Instruction::*;
#[derive(Clone, Eq, PartialEq, Hash)]
pub enum Instruction {
    // Data movement
    Move(DataSize, AddrMode, AddrMode),
    MoveMRtoM(DataSize, ADBitField, AddrMode),
    MoveMMtoR(DataSize, AddrMode, ADBitField),

    // Arithmetic
    Add(DataSize, AddrMode, AddrMode),
    Sub(DataSize, AddrMode, AddrMode),
    Neg(DataSize, AddrMode),
    Clr(DataSize, AddrMode),
    Not(DataSize, AddrMode),
    Tst(DataSize, AddrMode),
    Cmp(DataSize, AddrMode, AddrMode),
    Eor(DataSize, AddrMode, AddrMode),  // a.k.a. xor
    And(DataSize, AddrMode, AddrMode),
    Or(DataSize, AddrMode, AddrMode),
    Divs(AddrMode, AddrMode), // dest = 32bits, src = 16bits
    Divu(AddrMode, AddrMode),
    Muls(AddrMode, AddrMode), // dest = 32bits, src = 16bits
    Mulu(AddrMode, AddrMode),

    // Bit manipulation
    Asl(DataSize, Option<AddrMode>),
    Asr(DataSize, Option<AddrMode>),
    Lsl(DataSize, Option<AddrMode>),
    Lsr(DataSize, Option<AddrMode>),
    Rol(DataSize, Option<AddrMode>),
    Ror(DataSize, Option<AddrMode>),
    Roxl(DataSize, Option<AddrMode>),
    Roxr(DataSize, Option<AddrMode>),
    Swap(DReg),
    Stop,
    Nop,
    Reset,
    Jsr(AddrMode),
    Rte,
    Rts,
    Trap(u32),
    Trapv(u32),
    Link(i16, AReg),
    Unlk(AReg),
    ExtW(DReg),
    ExtL(DReg),
    Pea(AddrMode),
    Lea(AddrMode, AReg),
}
