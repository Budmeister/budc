use std::collections::{HashMap, HashSet};

use crate::bud::{BinExpr, Expr, NonBinExpr, TypeExpr, VarDecl};
use crate::logging::LoggingOptions;
use crate::tools::ToStringCollection;
use crate::{bud, parse::Node};
use log::*;

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
        let mut structs = Vec::new();
        let mut imports = Vec::new();

        for item in items {
            match item {
                bud::Item::FuncDecl(name, args, expr) => {
                    funcs.push((name, args, expr));
                }
                bud::Item::StructDecl(name, fields) => {
                    structs.push((name, fields));
                }
                bud::Item::ImportDecl(path) => imports.push(path),
            }
        }

        let mut environment = Environment::new();
        let built_in = Self::get_built_in_types();
        let funcs = environment.add_types(structs, built_in, funcs)?;

        debug!("Types found: ");
        environment.types
            .iter()
            .for_each(|(_, typ)| {
                debug!("\t{}", typ);
            });
        debug!("End types found");

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
    pub types: HashMap<TypeType, Type>,
    pub structs: HashMap<String, TypeType>,
}
impl Environment {
    pub fn new() -> Environment {
        Environment {
            types: HashMap::new(),
            structs: HashMap::new(),
        }
    }
    // This is the only chance to add raw types
    // This function also creates the Function objects
    pub fn add_types(
        &mut self,
        structs: Vec<(String, Vec<VarDecl>)>,
        mut built_in: HashMap<TypeType, Type>,
        funcs: Vec<(VarDecl, Vec<VarDecl>, Box<Expr>)>,
    ) -> Result<HashMap<String, Function>, String> {
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
        
        self.types = types;
        self.structs = HashMap::new();
        for (tt, _) in &self.types {
            if let TypeType::Struct(name, _) = tt {
                self.structs.insert(name.to_owned(), tt.to_owned());
            }
        }
        let funcs = funcs
            .into_iter()
            .map(|(name, args, expr)| {
                Ok((
                    name.id.to_owned(),
                    Function::new(name, args, expr, self)?
                ))
            })
            .collect::<Result<HashMap<String, Function>, String>>()?;

        debug!("End types");
        Ok(funcs)
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

    pub fn compile(self, funcs: Vec<Function>) -> Result<CompiledEnvironment, String> {
        Ok(CompiledEnvironment { types: self.types, funcs: funcs
            .into_iter()
            .map(|func| func.compile(0..))
            .collect::<Result<Vec<CompiledFunction>, String>>()?
        })
    }
}

pub struct CompiledEnvironment {
    pub types: HashMap<TypeType, Type>,
    pub funcs: Vec<CompiledFunction>,
}

#[derive(Clone, Eq, PartialEq)]
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
    pub fn compile<I: Iterator<Item=usize>>(self, label_gen: I) -> Result<CompiledFunction, String> {
        let mut instructions = Vec::new();
        // Things we need to know (from searching the function expression):
        //  * String literals
        //  * All local variables (can be Fields)
        Ok(CompiledFunction { signature: self.signature, instructions })        
    }
}

pub struct LabelGenerator {
    num: usize,
}
impl LabelGenerator {
    pub fn new() -> LabelGenerator {
        LabelGenerator { num: 0 }
    }
    pub fn next(&mut self) -> usize {
        let prev = self.num;
        self.num += 1;
        prev
    }
}

#[derive(Clone)]
pub struct CompiledFunction {
    pub signature: Signature,
    pub instructions: Vec<Instruction>,
}

#[derive(Copy, Clone)]
pub enum GPReg {
    D0,
    D1,
    D2,
    D3,
    D4,
    D5,
    D6,
    D7,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    SP,
}

#[derive(Copy, Clone)]
pub enum Instruction {}
