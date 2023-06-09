//! In almost all cases, type determination in Bud is top-down. That is,
//! when an expression is evaluated, its return type is given, and if,
//! by its nature, it must evaluate to a different type, then an error
//! is thrown.
//! 
//! However, in `if` statements, it is acceptable for the condition to 
//! evaluate to `i8`, `i16`, or `i32`, and it does not make sense to 
//! force the user to one type of boolean and throw an error if they 
//! use the wrong type. 
//! 
//! In this case, the expression is asked for its `type_preference`, 
//! and if it is not a "large" type, it is given as the return type.
//! 
//! Author:     Brian Smith
//! Year:       2023

use crate::{bud::*, m68k::*};

use super::{fienv::*, place::*, inter_instr::*};

use log::*;


impl Expr {
    pub fn type_preference(&self, fienv: &FunctionInterEnvironment, env: &Environment) -> Result<TypeType, String> {
        self.bin_expr.type_preference(fienv, env)
    }
}

impl BinExpr {
    pub fn type_preference(&self, fienv: &FunctionInterEnvironment, env: &Environment) -> Result<TypeType, String> {
        let (BinExpr::Binary(nbe, _, _) | BinExpr::NonBin(nbe)) = self;
        nbe.type_preference(fienv, env)
    }
}

impl NonBinExpr {
    pub fn type_preference(&self, fienv: &FunctionInterEnvironment, env: &Environment) -> Result<TypeType, String> {
        match self {
            NonBinExpr::BlockExpr(exprs) => {
                match exprs.last() {
                    Some(expr) => expr.type_preference(fienv, env),
                    None => Ok(Environment::get_void_tt()),
                }
            },
            NonBinExpr::AssignExpr(id, _) => IdExpr::id_to_tt(IdExpr::clone(id), fienv, env),
            NonBinExpr::VarDeclAssgn(vd, _) => {
                let field = Field::new(VarDecl::clone(vd), env);
                Ok(field.tt)
            },
            NonBinExpr::ReturnExpr(_) => Ok(Environment::get_void_tt()),
            NonBinExpr::CleanupCall => Ok(Environment::get_void_tt()),
            NonBinExpr::CleanupExpr(_) => Ok(Environment::get_void_tt()),
            NonBinExpr::IdExpr(id) => IdExpr::id_to_tt(IdExpr::clone(id), fienv, env),
            NonBinExpr::LitExpr(_) => Ok(TypeType::Id("i32".to_owned())),
            NonBinExpr::ParenExpr(expr) => expr.type_preference(fienv, env),
            NonBinExpr::UnaryExpr(_, expr) => expr.type_preference(fienv, env),
            NonBinExpr::IfExpr(_, _) => Ok(Environment::get_void_tt()),
            NonBinExpr::IfElse(_, expr, _) => expr.type_preference(fienv, env),         // If the expressions don't agree, then
            NonBinExpr::UnlExpr(_, _) => Ok(Environment::get_void_tt()),                            // the error will be thrown when the 
            NonBinExpr::UnlElse(_, expr, _) => expr.type_preference(fienv, env),        // `if` or `unless` is compiles
            NonBinExpr::WhileExpr(_, _) => Ok(Environment::get_void_tt()),
            NonBinExpr::DoWhile(_, expr) => expr.type_preference(fienv, env),
            NonBinExpr::Break => Ok(Environment::get_void_tt()),
            NonBinExpr::Continue => Ok(Environment::get_void_tt()),
        }
    }

}
impl IdExpr {
    /// Gets the TypeType of this IdExpr
    pub fn id_to_tt(id: IdExpr, fienv: &FunctionInterEnvironment, env: &Environment) -> Result<TypeType, String> {
        match id {
            IdExpr::SquareIndex(expr, _offset) => {
                // TODO Handle struct member access here, too
                let tt = expr.type_preference(fienv, env)?;
                if let TypeType::Array(tt, _) = tt {
                    Ok(*tt)
                } else {
                    Err(format!("Cannot index into non-array type. Struct member access has not been implemented yet."))
                }
            }
            IdExpr::RoundIndex(expr, _) => {
                warn!("Round indexing not yet implemented");
                let tt = expr.type_preference(fienv, env)?;
                if let TypeType::Array(tt, _) = tt {
                    Ok(*tt)
                } else {
                    Err(format!("Cannot index into non-array type. Struct member access has not been implemented yet."))
                }
            }
            IdExpr::Deref(id) => {
                let tt = IdExpr::id_to_tt(*id, fienv, env)?;
                match tt {
                    TypeType::Pointer(tt) => Ok(*tt),
                    TypeType::Id(name) => {
                        let field = fienv.get_var(&name);
                        match field {
                            Some(Field { tt , name: _ }) => Ok(tt),
                            _ => Err(format!("Unknown id {}", name))
                        }
                    }
                    TypeType::Array(_, _) => Err(format!("Cannot dereference array")),
                    TypeType::Struct(name, _) => Err(format!("Cannot dereference struct {}", name)),
                }
            }
            IdExpr::Id(name) => {
                if let Some(Field { tt, name: _ }) = fienv.get_var(&name) {
                    Ok(tt)
                } else {
                    Err(format!("Unknown id {}", name))
                }
            }
        }
    }
}

impl Environment {
    pub fn ret_place(&self, name: String) -> Result<Place, String> {
        let sig = match self.global_funcs.get(&name) {
            Some(sig) => sig,
            None => { return Err(format!("No global function {}", name)); }
        };
        let ret_tt = sig.name.tt.clone();
        if ret_tt.is_struct() || ret_tt.is_array() {
            return Err(format!("Returning arrays and structs from functions not yet supported"));
        }
        Ok(Place::DTemp(0, ret_tt))
    }

    pub fn get_str_tt() -> TypeType {
        TypeType::Pointer(Box::new(TypeType::Id("i8".to_owned())))
    }
    pub fn get_bool_tt() -> TypeType {
        TypeType::Id("i8".to_owned())
    }
    pub fn get_void_tt() -> TypeType {
        TypeType::Id("void".to_owned())
    }

}

impl TypeType {
    pub fn get_size(&self, env: &Environment) -> u32 {
        match env.types.get(self).unwrap().size {
            Either::This(size) | Either::That((size, _)) => size
        }
    }
    // Returns None if the size is not a Byte, Word, or LWord
    pub fn get_data_size(&self, env: &Environment) -> Option<DataSize> {
        let size = self.get_size(env);
        match size {
            1 => Some(DataSize::Byte),
            2 => Some(DataSize::Word),
            4 => Some(DataSize::LWord),
            _ => None,
        }
    }
    pub fn is_magic(&self, env: &Environment) -> bool {
        match env.types.get(self) {
            Some(t) => t.magic,
            None => {
                error!("Type {} not found in env.types", self);
                panic!()
            }
        }
    }
    pub fn is_struct(&self) -> bool {
        matches!(self, TypeType::Struct(_, _))
    }
    pub fn is_pointer(&self) -> bool {
        matches!(self, TypeType::Pointer(_))
    }
    pub fn is_array(&self) -> bool {
        matches!(self, TypeType::Array(_, _))
    }
    pub fn is_id(&self) -> bool {
        matches!(self, TypeType::Id(_))
    }
    pub fn is_void(&self) -> bool {
        *self == Environment::get_void_tt()
    }
}