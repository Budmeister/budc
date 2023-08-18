//! The generic location that a value can have is a `Place`. `Place`s
//! can be variables, address temps, data temps, or refs (an address
//! temp holding a pointer with an optional data temp as an index and
//! an immediate offset). As a Var, ATemp or DTemp, a `Place` refers to 
//! that register, but as a Ref, it refers to the thing pointed to by
//! the Ref. 
//! 
//! Therefore, some `Place`s, if they are Var or Ref, can be too big to
//! fit in a register. So, the user must be careful not to move "large"
//! values into regisers. As a rule, values of array or struct types
//! SHOULD NOT be put into registers, even if they would fit. 
//! 
//! Author:     Brian Smith
//! Year:       2023

use std::ops::Range;

use crate::{m68k::*, bud::BudBinop, error::*, u_err, c_err};

use super::{inter_instr::*, fienv::FunctionInterEnvironment};

use log::*;

pub type ATemp = usize;
pub type DTemp = usize;
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum ADTemp {
    A(ATemp),
    D(DTemp),
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum Place {
    Var(Field),
    /// The place represented by the ATemp is the address register itself, not
    /// the thing pointed to by the ATemp. It represents a void pointer.
    ATemp(ATemp),
    DTemp(DTemp, TypeType), // RetReg is DTemp(0, _)
    /// The place represented by this Ref is the location pointed to by the Ref.
    /// This is not to be confused with ATemp, which place is the address register
    /// itself.
    Ref(ATemp, Option<DTemp>, i32, TypeType),   // (location of the pointer, optional data offset, offset, typetype pointed to)
                                                // When compiling Ref Places, move the reference into the reference register, A0,
                                                // if it is not already in an address register
                                                // and move the offset into an offset register, D1, (if it is not already in a data register)
                                                // Then dereference it using (a0, d1, lit), moving it into the destination. 
                                                // If the dereference is being assigned, then check if the expression doing the 
                                                // assigning uses the reference register. If it does, save the reference register
                                                // on the stack before evaluating the expression. You do not need to save the offset 
                                                // register (it may even already be on the stack), but move it into a data register
                                                // before moving into this reference using (a0, d1, lit)
}
impl Place {
    pub fn get_type(&self) -> TypeType {
        match self {
            Place::Var(f) => {
                f.tt.clone()
            }
            Place::ATemp(_) => {
                TypeType::Pointer(Box::new(Environment::get_void_tt()))
            }
            Place::DTemp(_, tt) => {
                tt.clone()
            }
            Place::Ref(_, _, _, tt) => {
                tt.clone()
            }
        }
    }
    pub fn get_size(&self, env: &Environment, range: Option<&Range<usize>>) -> Result<u32, CompilerErr> {
        self.get_type().get_size(env, range)
    }
    // Returns None if the size is not a Byte, Word, or LWord
    pub fn get_data_size(&self, env: &Environment, range: Option<&Range<usize>>) -> Result<Option<DataSize>, CompilerErr> {
        self.get_type().get_data_size(env, range)
    }
    /// Moves the value in the given DTemp to a new ATemp. This function frees the DTemp, and you must free the ATemp.
    fn d_to_a(d: DTemp, tt: TypeType, range: Range<usize>, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<ATemp, CompilerErr> {
        match tt.get_data_size(env, Some(&range))? {
            Some(_) => {},
            None => { return c_err!(range, "DTemp {} containing large value", d); },
        };
        let a = fienv.get_addr_temp();
        let d_place = Place::DTemp(d, tt);
        let a_place = Place::ATemp(a);
        let instr = InterInstr::Move(d_place, a_place, range);
        instrs.push(instr);
        fienv.free_data_temp(d);
        Ok(a)
    }
    /// Calculates the Place: `self[d+off]`
    /// This function consumes this place (frees it) and returns a Ref that you must free, unless
    /// of course the given place is a Var; Vars cannot be consumed.
    /// 
    /// Calling `place.index_into(None, 0)` is equivalent to dereferencing `place`.
    /// 
    /// If `self.is_struct()`, then off should be the index of the argument in this struct's layout. 
    /// Otherwise, off should be the number of indices to advance (not multiplied by the size of the item).
    /// 
    /// If `self.is_array()`, then off is an index, not a memory offset. If `off >= ` length of `self`, then a 
    /// warning will be printed.
    /// 
    /// * If this Place is DTemp, then the type of the DTemp must be a Reference. This function moves the DTemp 
    ///   into an ATemp and returns that ATemp as a Ref as it would with an ATemp
    /// * If this Place is ATemp, it returns it as a Ref
    /// * If this Place is Var, it moves the location of the Var to an ATemp and returns the ATemp as a Ref
    /// * If this Place is Ref, it moves the thing this Ref points to into an ATemp and returns the ATemp as a Ref
    /// 
    /// This function will return Err if
    /// * the type of this Place is not indexable
    /// * The `self` is DTemp and `self.is_array() || self.is_struct()`
    pub fn index_into(self, d: Option<DTemp>, mut off: i32, checked: bool, range: Range<usize>, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<Place, BudErr> {
        let tt;
        let mut is_array = None;
        let mut is_struct = false;
        match self.get_type() {
            TypeType::Array(tt_, len_) => {
                tt = *tt_;
                is_array = Some(len_);
            }
            TypeType::Id(name) => {
                return u_err!(range, "Cannot index into type {}", name);
            }
            TypeType::Pointer(tt_) => { tt = *tt_; },
            TypeType::Struct(name, Some(fields)) => {
                // This is a very bad way of doing it
                // Right now, this function receives a field index, gets the name
                // of the field from the Struct TypeType, and gets the absolute offset
                // from the Struct Type from the Environment. 
                // It should be like this:
                // Either the struct layout should be stored in the Struct TypeType 
                // or this function should receive the name of the field
                // or this function should receive an absolute offset and TypeType to return.
                if off as usize >= fields.len() {
                    return c_err!(range, "Invalid field index {} for struct {}", off, name);
                }
                let f_name = &fields[off as usize].name;
                let tt_ = fields[off as usize].tt.clone();
                let s_tt = self.get_type();
                let size = env.type_size(s_tt, Some(&range))?;
                if let Either::That((_, layout)) = size {
                    off = match layout.get(f_name) {
                        Some((off, _)) => *off as i32,
                        None => return u_err!(range, "Struct {} has no field {}", name, f_name),
                    };
                } else {
                    return c_err!("Struct {} did not have a layout", name);
                }
                is_struct = true;
                tt = tt_;
            }
            TypeType::Struct(name, None) => {
                return c_err!("Struct {} not fully initialized with fields", name);
            }
        };
        if let Some(len) = is_array {
            if off >= len {
                if checked {
                    return u_err!(range, "Array has length {} but is being indexed with literal {}", len, off);
                } else {
                    warn!("Array has length {} but is being indexed with literal {}", len, off);
                }
            } else if checked {
                let tt = TypeType::Id("i16".to_owned());
                let dtemp = fienv.get_data_temp(tt.clone(), Some(&range))?;
                let d_place = Place::DTemp(dtemp, tt);
                let instr = InterInstr::Move(self.clone(), d_place.clone(), range.to_owned());
                instrs.push(instr);
                if off != 0 {
                    let instr = InterInstr::Binopi(off, BudBinop::Plus, d_place.clone(), range.to_owned());
                    instrs.push(instr);
                }
                let instr = InterInstr::Chki(len as i16, dtemp, range.to_owned());
                instrs.push(instr);
                d_place.free(fienv);
            }
        }
        if !is_struct {
            let size = tt.get_size(env, Some(&range))? as i32;
            if let Some(d) = d {
                let d_place = Place::DTemp(d, TypeType::Id("i32".to_owned()));
                let instr = InterInstr::Binopi(size, BudBinop::Times, d_place, range.to_owned());
                instrs.push(instr);
            }
            off *= size;
        }
        match self {
            Place::ATemp(atemp) => Ok(Place::Ref(atemp, d, off, tt)),
            Place::DTemp(d_, _) => {
                if is_struct || is_array.is_some() {
                    c_err!(range, "Cannot store structs or arrays in data registers")
                } else {
                    Ok(Place::Ref(Self::d_to_a(d_, tt.clone(), range, instrs, fienv, env)?, d, off, tt))
                }
            },
            Place::Ref(a, mut d_, off_, _) => {
                if is_struct || is_array.is_some() {
                    // Shift this Ref by the amount given
                    if let Some(d) = d {
                        if let Some(d_) = d_ {
                            let d_place_ = Place::DTemp(d_, TypeType::Id("i32".to_owned()));
                            let d_place = Place::DTemp(d, TypeType::Id("i32".to_owned()));
                            let instr = InterInstr::Binop(d_place, BudBinop::Plus, d_place_, range);
                            instrs.push(instr);
                        } else {
                            d_ = Some(d);
                        }
                    }
                    off += off_;
                    Ok(Place::Ref(a, d_, off, tt))
                } else {
                    // We actually want to index into the thing this Ref is pointing to
                    // which must be a pointer, since !(is_struct || is_array)
                    let a_place = Place::ATemp(a);
                    let instr = InterInstr::Move(self, a_place, range);
                    if let Some(d_) = d_ { fienv.free_data_temp(d_); }
                    instrs.push(instr);
                    Ok(Place::Ref(a, d, off, tt))
                }
            }
            Place::Var(Field { ref name, tt: _ }) => {
                let a = fienv.get_addr_temp();
                let a_place = Place::ATemp(a);
                let instr = if is_struct || is_array.is_some() {
                    InterInstr::MoVA(name.clone(), a_place, range)
                } else {
                    // self must be a pointer
                    InterInstr::Move(self, a_place, range)
                };
                instrs.push(instr);
                Ok(Place::Ref(a, d, off, tt))
            }
        }
    }
    pub fn free(&self, fienv: &mut FunctionInterEnvironment) {
        match self {
            Place::ATemp(a) => fienv.free_addr_temp(*a),
            Place::DTemp(d, _) => fienv.free_data_temp(*d),
            Place::Ref(a, d, _, _) => {
                fienv.free_addr_temp(*a);
                if let Some(d) = d { fienv.free_data_temp(*d); }
            }
            Place::Var(_) => {}
        }
    }
    pub fn is_magic(&self, env: &Environment) -> Result<bool, CompilerErr> {
        self.get_type().is_magic(env)
    }
    pub fn is_struct(&self) -> bool {
        self.get_type().is_struct()
    }
    pub fn is_pointer(&self) -> bool {
        self.get_type().is_pointer()
    }
    pub fn is_array(&self) -> bool {
        self.get_type().is_array()
    }
    pub fn is_id(&self) -> bool {
        self.get_type().is_id()
    }
    pub fn is_void(&self) -> bool {
        self.get_type() == Environment::get_void_tt()
    }
}
impl std::fmt::Display for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Place::Var(field) => field.to_string(),
            Place::ATemp(atemp) => format!("A{}", atemp),
            Place::DTemp(dtemp, tt) => format!("{} D{}", tt, dtemp),
            Place::Ref(a, d, offset, tt) => {
                match d {
                    Some(d) =>  format!("{} A{}[D{}+{}]", tt, a, d, offset),
                    None =>     format!("{} A{}[{}]",     tt, a, offset),
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