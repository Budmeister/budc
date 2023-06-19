use std::ops::RangeFrom;

use crate::logging::LoggingOptions;
use crate::m68k::tools::*;
use crate::m68k::top::*;
use crate::m68k::bottom::*;
use crate::bud::*;
use log::*;

impl Expr {
    fn type_preference(&self, fienv: &FunctionInterEnvironment, env: &Environment) -> Result<TypeType, String> {
        self.bin_expr.type_preference(fienv, env)
    }
}

impl BinExpr {
    fn type_preference(&self, fienv: &FunctionInterEnvironment, env: &Environment) -> Result<TypeType, String> {
        let (BinExpr::Binary(nbe, _, _) | BinExpr::NonBin(nbe)) = self;
        nbe.type_preference(fienv, env)
    }
}

impl NonBinExpr {
    fn type_preference(&self, fienv: &FunctionInterEnvironment, env: &Environment) -> Result<TypeType, String> {
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
    fn id_to_tt(id: IdExpr, fienv: &FunctionInterEnvironment, env: &Environment) -> Result<TypeType, String> {
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
        if let TypeType::Struct(_, _) = self {
            true
        } else {
            false
        }
    }
    pub fn is_pointer(&self) -> bool {
        if let TypeType::Pointer(_) = self {
            true
        } else {
            false
        }
    }
    pub fn is_array(&self) -> bool {
        if let TypeType::Array(_, _) = self {
            true
        } else {
            false
        }
    }
    pub fn is_id(&self) -> bool {
        if let TypeType::Id(_) = self {
            true
        } else {
            false
        }
    }
    pub fn is_void(&self) -> bool {
        *self == Environment::get_void_tt()
    }
}

pub type ATemp = usize;
pub type DTemp = usize;

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
    pub fn get_size(&self, env: &Environment) -> u32 {
        match env.types.get(&self.get_type()).unwrap().size {
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
    /// Moves the value in the given DTemp to a new ATemp. This function frees the DTemp, and you must free the ATemp.
    fn d_to_a(d: DTemp, tt: TypeType, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<ATemp, String> {
        match tt.get_data_size(env) {
            Some(_) => {},
            None => { return Err(format!("DTemp {} containing large value", d)); },
        };
        let a = fienv.get_addr_temp()?;
        let d_place = Place::DTemp(d, tt.clone());
        let a_place = Place::ATemp(a);
        let instr = InterInstr::Move(d_place, a_place);
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
    pub fn index_into(self, d: Option<DTemp>, mut off: i32, checked: bool, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<Place, String> {
        let tt;
        let mut is_array = None;
        let mut is_struct = false;
        match self.get_type() {
            TypeType::Array(tt_, len_) => {
                tt = *tt_;
                is_array = Some(len_);
            }
            TypeType::Id(name) => {
                return Err(format!("Cannot index into type {}", name));
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
                    return Err(format!("Invalid field index {} for struct {}", off, name));
                }
                let f_name = &fields[off as usize].name;
                let tt_ = fields[off as usize].tt.clone();
                let s_tt = self.get_type();
                let size = match env.types.get(&s_tt) {
                    Some(t) => t.size.clone(),
                    None => { return Err(format!("Struct type not found in environment: {}", name)); },
                };
                if let Either::That((_, layout)) = size {
                    off = match layout.get(f_name) {
                        Some((off, _)) => *off as i32,
                        None => { return Err(format!("Struct {} has no field {}", name, f_name)); },
                    };
                } else {
                    return Err(format!("Struct {} did not have a layout", name));
                }
                is_struct = true;
                tt = tt_;
            }
            TypeType::Struct(name, None) => {
                return Err(format!("Struct {} not fully initialized with fields", name));
            }
        };
        if let Some(len) = is_array {
            if off >= len {
                if checked {
                    return Err(format!("Array has length {} but is being indexed with literal {}", len, off));
                } else {
                    warn!("Array has length {} but is being indexed with literal {}", len, off);
                }
            } else if checked {
                let tt = TypeType::Id("i16".to_owned());
                let dtemp = fienv.get_data_temp(tt.clone())?;
                let d_place = Place::DTemp(dtemp, tt);
                let instr = InterInstr::Move(self.clone(), d_place.clone());
                instrs.push(instr);
                if off != 0 {
                    let instr = InterInstr::Binopi(Imm::Word(off as i16), BudBinop::Plus, d_place.clone());
                    instrs.push(instr);
                }
                let instr = InterInstr::Chki(len as i16, dtemp);
                instrs.push(instr);
                d_place.free(fienv);
            }
        }
        if !is_struct {
            let size = tt.get_size(env);
            match d {
                Some(d) => {
                    let size = Imm::LWord(size as i32);
                    let d_place = Place::DTemp(d, TypeType::Id("i32".to_owned()));
                    let instr = InterInstr::Binopi(size, BudBinop::Times, d_place);
                    instrs.push(instr);
                }
                None => {}
            }
            off *= size as i32;
        }
        match self {
            Place::ATemp(atemp) => Ok(Place::Ref(atemp, d, off, tt.clone())),
            Place::DTemp(d_, _) => {
                if is_struct || is_array != None {
                    Err(format!("Cannot store structs or arrays in data registers"))
                } else {
                    Ok(Place::Ref(Self::d_to_a(d_, tt.clone(), instrs, fienv, env)?, d, off, tt.clone()))
                }
            },
            Place::Ref(a, mut d_, off_, _) => {
                if is_struct || is_array != None {
                    // Shift this Ref by the amount given
                    if let Some(d) = d {
                        if let Some(d_) = d_ {
                            let d_place_ = Place::DTemp(d_, TypeType::Id("i32".to_owned()));
                            let d_place = Place::DTemp(d, TypeType::Id("i32".to_owned()));
                            let instr = InterInstr::Binop(d_place, BudBinop::Plus, d_place_);
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
                    let instr = InterInstr::Move(self, a_place);
                    if let Some(d_) = d_ { fienv.free_data_temp(d_); }
                    instrs.push(instr);
                    Ok(Place::Ref(a, d, off, tt))
                }
            }
            Place::Var(Field { ref name, tt: _ }) => {
                let a = fienv.get_addr_temp()?;
                let a_place = Place::ATemp(a);
                let instr = if is_struct || is_array != None {
                    InterInstr::MoVA(name.clone(), a_place)
                } else {
                    // self must be a pointer
                    InterInstr::Move(self, a_place)
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
    fn is_magic(&self, env: &Environment) -> bool {
        let tt = self.get_type();
        match env.types.get(&tt) {
            Some(t) => t.magic,
            None => {
                error!("Type {} not found in env.types", tt);
                panic!()
            }
        }
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

/// `ReturnPlan`s tell an expression where to put its output. 
/// `ReturnPlan`s are not required to set the condition codes accordingly
/// except for the `Condition` variant.
#[derive(Clone, Debug)]
pub enum ReturnPlan {
    /// Do the binop with the result of this expr and the given place
    /// The place must have a magic type, since only magic types
    /// can do binops.
    Binop(BudBinop, Place),
    /// Move the result of this expr into the given place. Since the
    /// return place is going to be overwritten anyway, it can be used
    /// in the evaluation of this expr
    Move(Place),
    /// Set the condition codes for the result of this expr, but the result
    /// doesn't need to be moved to any place in particular.
    Condition,
    /// Push the result of this expr onto the stack.
    Push(TypeType),
    /// Return the result of this expr from a function. 
    /// `FunctionInterEnvironment.ret`, `.reti`, `.rets`, and `.retva` 
    /// can be used to return a value. When `ReturnPlan::Return`
    /// is given, the callee is expected to push `InterInstr::Rts`
    /// onto `instrs` after moving the result into the retval.
    /// `ReturnPlan::Return` can be given even in a void function because
    /// of return statements.
    Return,
    None,
}
impl ReturnPlan {
    /// Also frees this place. If self is `Return`, also adds `Rts` instruction
    pub fn into_inter_instr(self, from: Place, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        let from_tt = from.get_type();
        let from_tt_size = from_tt.get_size(env);
        let to_tt;
        let to_tt_size;
        match self {
            ReturnPlan::Binop(b, to) => {
                to_tt = to.get_type();
                if !to.is_magic(env) {
                    return Err(format!("Cannot do binary operator {} on non-magic type {}", b, to.get_type()));
                }
                if !from.is_magic(env) {
                    return Err(format!("Cannot do binary operator {} on non-magic type {}", b, from.get_type()));
                }
                let instr = InterInstr::Binop(from.clone(), b, to);
                instrs.push(instr);
                from.free(fienv);
            }
            ReturnPlan::Move(to) => {
                to_tt = to.get_type();
                let instr = InterInstr::Move(from.clone(), to);
                instrs.push(instr);
                from.free(fienv);
            },
            ReturnPlan::Condition => {
                let instr = InterInstr::Tst(from.clone());
                instrs.push(instr);
                from.free(fienv);
                return Ok(());
            }
            ReturnPlan::Push(tt) => {
                to_tt = tt.clone();
                let instr = InterInstr::Push(from.clone(), tt);
                instrs.push(instr);
                from.free(fienv);
            }
            ReturnPlan::Return => {
                return fienv.ret(from.clone(), instrs, env);
            },
            ReturnPlan::None => {
                from.free(fienv);
                return Ok(());
            },
        }
        to_tt_size = to_tt.get_size(env);
        if from_tt != to_tt {
            if from_tt.get_size(env) == to_tt.get_size(env) {
                warn!("Implicit cast between equally sized types: {} -> {}", from_tt, to_tt);
            } else {
                return Err(format!("Implicit cast between unequally sized types: {} -> {} ({} -> {})", from_tt, to_tt, from_tt_size, to_tt_size));
            }
        }
        Ok(())
    }
    /// Has no effect if there is no return plan
    /// Pass a size of LWord if it doesn't matter. This function will coerce the size to the one the plan requires.
    pub fn imm_into_inter_instr(self, from: Imm, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        match self {
            ReturnPlan::Binop(b, to) => {
                let size = Self::get_imm_size(&from, &to, env)?;
                let instr = InterInstr::Binopi(from.change_size(size), b, to);
                instrs.push(instr);
            }
            ReturnPlan::Move(to) => {
                let size = Self::get_imm_size(&from, &to, env)?;
                let instr = InterInstr::Movi(from.change_size(size), to);
                instrs.push(instr);
            }
            ReturnPlan::Condition => {
                let instr = InterInstr::Tsti(from);
                instrs.push(instr);
            }
            ReturnPlan::Push(tt) => {
                let instr = InterInstr::Pusi(from.as_type(tt, env)?);
                instrs.push(instr);
            }
            ReturnPlan::Return => fienv.reti(from, instrs, env)?,
            ReturnPlan::None => {},
        }
        Ok(())
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
    /// Returns type type you are expected to give to this plan. If the plan is None or Condition, then this function returns None.
    pub fn get_type(&self, fienv: &mut FunctionInterEnvironment) -> Option<TypeType> {
        match self {
            ReturnPlan::Binop(_, place) => Some(place.get_type()),
            ReturnPlan::Move(place) => Some(place.get_type()),
            ReturnPlan::Condition => None,
            ReturnPlan::Push(tt) => Some(tt.clone()),
            ReturnPlan::Return => Some(fienv.return_type()),
            ReturnPlan::None => None,
        }
    }
}

// I am using these Intermediate Instructions to represent 
// instructions before I have generated the stack frame
#[derive(Debug)]
pub enum InterInstr {
    // Data operations
    Binop(Place, BudBinop, Place),    // Dest. on right (SUB subtracts src from dest. and stores in dest.; DIV, too)
    Binopi(Imm, BudBinop, Place),
    Neg(Place),                                 // Neg and Bnot must not be an ATemp
    Bnot(Place),                                // Boolean NOT--not bitwise NOT
    Move(Place, Place),
    MoVA(String, Place),                        // Move Var Address
    Movi(Imm, Place),
    Movs(usize, Place),                         // Move string literal (by the string literal's global label)
    Lea(ATemp, Option<DTemp>, i32, ATemp),      // Load effective address into an address register
    Push(Place, TypeType),
    PuVA(String),
    Pusi(Imm),
    Puss(usize),                                // Push string literal (by the string literal's global label)
    Pea(ATemp, Option<DTemp>, i32),             // Push effective address onto stack
    Chk(ATemp, Option<DTemp>, i32, DTemp),
    Chki(i16, DTemp),

    // Stack frame operations
    SMarker(StackMarker),                       // Marks a stack location for the FunctionInterEnvironment
    Call(String, StackMarker),                  // Jsr to function, then move the SP to where it was when the StackMarker
                                                // instruction was encountered and retrieve reg values from most recent Gsr
    Lbl(usize),
    Goto(usize),
    Rts,
    Grs,                                        // GetRegisterSpace - allocate space on the stack to save active regs
    Save,                                       // Save all active regs by moving them to the given RegisterSpace

    // Logic operations
    Tst(Place),
    Tsti(Imm),                                  // Pre-calculate the CC and just move that to the CC using `MOVE <ea>, CCR`
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
}

pub struct LoopEnvironment {
    break_label: usize,
    continue_label: usize,
}

pub struct FunctionInterEnvironment {
    pub sig: Signature,
    /// If this function returns an array or struct (even if its size < 4 bytes),
    /// then an extra variable, `"[retval]"`, will be passed in
    pub vars: Vec<Field>,
    // (label number, string)
    pub lit_strings: Vec<(usize, String)>,
    cleanup_label: Option<usize>,
    cleanup_expr_created: bool,
    loop_stack: Vec<LoopEnvironment>,
    // dtemps and atemps can only hold variables up to 4 bytes
    // Variables greater than 4 bytes must be stored in vars.
    // They cannot be returned from expressions (until that is 
    // implemented) except to be pushed onto the stack. But
    // Move operations between reference Places move to or 
    // from the thing pointed to and can move large values (>4 bytes). 
    pub dtemps: Vec<bool>,
    pub atemps: Vec<bool>,
}
impl FunctionInterEnvironment {
    pub fn new(sig: Signature) -> FunctionInterEnvironment {
        FunctionInterEnvironment {
            vars: sig.args
                .iter()
                .map(|f| f.clone())
                .collect(),
            sig,
            lit_strings: Vec::new(),
            cleanup_label: None,
            cleanup_expr_created: false,
            loop_stack: Vec::new(),
            dtemps: Vec::new(),
            atemps: Vec::new(),
        }
    }
    pub fn get_name(&self) -> String {
        self.sig.name.name.clone()
    }
    pub fn return_type(&self) -> TypeType {
        self.sig.name.tt.clone()
    }
    /// Data temps are stored in dtemps.
    /// Data temps compile to data registers, so this function fails if the given type is a large type (size > 4 bytes).
    /// Arrays and structs cannot be stored in data registers, either, so this function will fail if the given type array or struct.
    /// However, it is not the case that only magic types can be stored in data temps, since pointers are not magic.
    pub fn get_data_temp(&mut self, tt: TypeType) -> Result<usize, String> {
        if tt.is_array() || tt.is_struct() {
            return Err(format!("Cannot store arrays or structs in data temps. TypeType {} given", tt));
        }
        // If a data temp is available, give it out
        for (temp, used) in self.dtemps.iter_mut().enumerate() {
            if !*used {
                *used = true;
                return Ok(temp);
            }
        }
        let place = self.dtemps.len();
        self.dtemps.push(true);
        Ok(place)
    }
    // Addr temps can point to anything
    pub fn get_addr_temp(&mut self) -> Result<usize, String> {
        // If a addr temp is available, give it out
        for (temp, used) in self.atemps.iter_mut().enumerate() {
            if !*used {
                *used = true;
                return Ok(temp);
            }
        }
        let place = self.atemps.len();
        self.atemps.push(true);
        Ok(place)
    }
    fn _free_temp(temps: &mut Vec<bool>, temp: usize, label: &str) {
        if temp < temps.len() {
            if temps[temp] {
                warn!("Trying to free {}{}, which is already freed", label, temp);
            }
            temps[temp] = false;
        }
    }
    pub fn free_data_temp(&mut self, dtemp: DTemp) {
        Self::_free_temp(&mut self.dtemps, dtemp, "D")
    }
    pub fn free_addr_temp(&mut self, atemp: ATemp) {
        Self::_free_temp(&mut self.atemps, atemp, "A")
    }
    pub fn push_loop_stack(&mut self, break_label: usize, continue_label: usize) {
        self.loop_stack.push(LoopEnvironment { break_label, continue_label })
    }
    pub fn get_break_label(&self) -> Option<usize> {
        // Maybe someday, you could break 2 layers out using `break 2;`
        match self.loop_stack.last() {
            Some(loop_environment) => Some(loop_environment.break_label),
            None => None,
        }
    }
    pub fn get_continue_label(&self) -> Option<usize> {
        match self.loop_stack.last() {
            Some(loop_environment) => Some(loop_environment.continue_label),
            None => None,
        }
    }
    pub fn pop_loop_stack(&mut self) -> Option<LoopEnvironment>{
        self.loop_stack.pop()
    }
    pub fn add_lit_string(&mut self, string: String, label_gen: &mut RangeFrom<usize>) -> usize {
        let ind = label_gen.next().unwrap();
        self.lit_strings.push((ind, string));
        ind
    }
    pub fn get_var(&self, name: &String) -> Option<Field> {
        for field in &self.vars {
            if field.name == *name {
                return Some(field.clone());
            }
        }
        None
    }
    pub fn has_var(&self, name: &String) -> bool {
        if let Some(_) = self.get_var(name) {
            true
        } else {
            false
        }
    }
    pub fn add_var(&mut self, field: &Field) -> Result<(), String> {
        if let Some(existing_field) = self.get_var(&field.name) {
            if existing_field.tt == field.tt {
                // Var already exists, so do nothing
                Ok(())
            } else {
                // Var exists with a different type
                Err(format!("Cannot create var {}, because var {} already exists", field, existing_field))
            }
        } else {
            self.vars.push(field.clone());
            Ok(())
        }
    }
    pub fn get_cleanup_label(&mut self, label_gen: &mut RangeFrom<usize>) -> usize {
        if let Some(label) = self.cleanup_label {
            label
        } else {
            let label = label_gen.next().unwrap();
            self.cleanup_label = Some(label);
            label
        }
    }
    pub fn ret(&mut self, place: Place, instrs: &mut Vec<InterInstr>, env: &Environment) -> Result<(), String> {
        let tt = place.get_type();
        if tt != self.return_type() {
            return Err(format!("Function has return type {} but tried to return object of type {}", self.return_type(), tt));
        }
        let to = env.ret_place(self.get_name())?;
        let instr = InterInstr::Move(place, to);
        instrs.push(instr);
        let instr = InterInstr::Rts;
        instrs.push(instr);
        Ok(())
    }
    pub fn reti(&mut self, imm: Imm, instrs: &mut Vec<InterInstr>, env: &Environment) -> Result<(), String> {
        let imm = imm.as_type(self.return_type(), env)?;
        let to = env.ret_place(self.get_name())?;
        let instr = InterInstr::Movi(imm, to);
        instrs.push(instr);
        let instr = InterInstr::Rts;
        instrs.push(instr);
        Ok(())
    }
    pub fn rets(&mut self, string: usize, instrs: &mut Vec<InterInstr>, env: &Environment) -> Result<(), String> {
        let tt = Environment::get_str_tt();
        if tt != self.return_type() {
            return Err(format!("Function has return type {} but tried to return literal string of type {}", self.return_type(), tt));
        }
        let to = env.ret_place(self.get_name())?;
        let instr = InterInstr::Movs(string, to);
        instrs.push(instr);
        let instr = InterInstr::Rts;
        instrs.push(instr);
        Ok(())
    }
    pub fn retva(&mut self, name: &String, instrs: &mut Vec<InterInstr>, env: &Environment) -> Result<(), String> {
        let var = match self.get_var(&name) {
            Some(name) => name,
            None => {
                return Err(format!("Trying to return pointer to local variable {} which does not exist", name));
            }
        };
        let tt = TypeType::Pointer(Box::new(var.tt.clone()));
        if tt != self.return_type() {
            return Err(format!("Function has return type {} but tried to return pointer to local variable {} of type {}", self.return_type(), var.name, var.tt));
        }
        let to = env.ret_place(self.get_name())?;
        let instr = InterInstr::MoVA(var.name, to);
        instrs.push(instr);
        let instr = InterInstr::Rts;
        instrs.push(instr);
        Ok(())
    }
}

impl Function {
    pub fn get_name(&self) -> String {
        self.signature.name.name.to_owned()
    }
    pub fn get_inter_instrs(expr: Expr, signature: &Signature, log_options: &LoggingOptions, label_gen: &mut RangeFrom<usize>, env: &Environment) -> Result<(Vec<InterInstr>, FunctionInterEnvironment), String> {
        let mut instrs = Vec::new();
        let mut fienv = FunctionInterEnvironment::new(signature.clone());
        let plan;
        if signature.name.tt.is_void() {
            plan = ReturnPlan::None;
        } else if signature.name.tt.is_array() || signature.name.tt.is_struct() {
            let ret_field = Field { tt: TypeType::Pointer(Box::new(signature.name.tt.clone())), name: "[retval]".to_owned() };
            fienv.add_var(&ret_field)?;
            // Manually add code at the end that will move the return value to the thing pointed to by [retval]
            // Do this by calling `fienv.ret(place, instrs)`
            plan = ReturnPlan::None;
        } else {
            plan = ReturnPlan::Move(Place::DTemp(0, signature.name.tt.clone()));
        }
        Self::compile_expr(
            expr,
            plan,
            &mut instrs,
            label_gen,
            &mut fienv,
            env,
        )?;
        if !fienv.cleanup_expr_created {
            if let Some(cleanup_label) = fienv.cleanup_label {
                let instr = InterInstr::Lbl(cleanup_label);
                instrs.push(instr);
            }
        }
        // Things we need to know (from searching the function expression):
        //  * String literals
        //  * All local variables (can be Fields)
        if log_options.print_inter_funcs {
            debug!("Instructions for function {}", signature.name.name);
            for instr in &instrs {
                debug!("\t{:?}", instr);
            }
        }
        Ok((instrs, fienv))
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
                let retreg = Place::DTemp(0, fienv.return_type());
                match (plan, fienv.return_type().is_array() || fienv.return_type().is_struct(), retreg) {
                    (ReturnPlan::Binop(pb, place), _, _) => {
                        // Make the temporary variable with the same type as the ReturnPlan
                        // As a future optimization, we do not need to get a new place if
                        // both binops are the same and if the binop is associative
                        let tt = place.get_type();
                        let dtemp = fienv.get_data_temp(tt.clone())?;
                        let d_place = Place::DTemp(dtemp, tt);
                        let plan = ReturnPlan::Move(d_place.clone());
                        Self::compile_non_bin_expr(*nbe, plan, instrs, label_gen, fienv, env)?;
                        let plan = ReturnPlan::Binop(b, d_place.clone());
                        Self::compile_bin_expr(*be, plan, instrs, label_gen, fienv, env)?;
                        ReturnPlan::Binop(pb, place).into_inter_instr(d_place, instrs, fienv, env)?;
                    },
                    (ReturnPlan::Move(place), _, _) | (ReturnPlan::Return, false, place) => {
                        let plan = ReturnPlan::Move(place.clone());
                        Self::compile_non_bin_expr(*nbe, plan, instrs, label_gen, fienv, env)?;
                        let plan = ReturnPlan::Binop(b, place);
                        Self::compile_bin_expr(*be, plan, instrs, label_gen, fienv, env)?;
                    },
                    (ReturnPlan::Condition, _, _) => {
                        let preference = nbe.type_preference(fienv, env)?;
                        if !preference.is_magic(env) {
                            return Err(format!("Cannot read type {} as a condition, because it is not magic", preference));
                        }
                        let dtemp = fienv.get_data_temp(preference.clone())?;
                        let d_place = Place::DTemp(dtemp, preference);
                        let plan = ReturnPlan::Move(d_place.clone());
                        Self::compile_non_bin_expr(*nbe, plan, instrs, label_gen, fienv, env)?;
                        let plan = ReturnPlan::Binop(b, d_place.clone());
                        Self::compile_bin_expr(*be, plan, instrs, label_gen, fienv, env)?;
                        // Condition Codes should be set
                        d_place.free(fienv);
                    }
                    (ReturnPlan::Return, true, _) => {
                        return Err(format!("Cannot return array or struct type `{}` from binary expression {}", fienv.return_type(), b));
                    }
                    (ReturnPlan::Push(tt), _, _) => {
                        let dtemp = fienv.get_data_temp(tt.clone())?;
                        let d_place = Place::DTemp(dtemp, tt.clone());
                        let plan = ReturnPlan::Move(d_place.clone());
                        Self::compile_non_bin_expr(*nbe, plan, instrs, label_gen, fienv, env)?;
                        let plan = ReturnPlan::Binop(b, d_place.clone());
                        Self::compile_bin_expr(*be, plan, instrs, label_gen, fienv, env)?;
                        let instr = InterInstr::Push(d_place, tt);
                        fienv.free_data_temp(dtemp);
                        instrs.push(instr);
                    },
                    (ReturnPlan::None, _, _) => {
                        Self::compile_non_bin_expr(*nbe, ReturnPlan::None, instrs, label_gen, fienv, env)?;
                        Self::compile_bin_expr(*be, ReturnPlan::None, instrs, label_gen, fienv, env)?;
                    },
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
            NonBinExpr::BlockExpr(exprs)        => Self::compile_block_expr(exprs, plan, instrs, label_gen, fienv, env),
            NonBinExpr::AssignExpr(id, expr)    => Self::compile_assign_expr(*id, *expr, plan, instrs, label_gen, fienv, env),
            NonBinExpr::VarDeclAssgn(vd, expr)  => Self::compile_var_decl_assign(*vd, *expr, plan, instrs, label_gen, fienv, env),
            NonBinExpr::ReturnExpr(expr)       => Self::compile_return_expr(expr.map(|x| *x), plan, instrs, label_gen, fienv, env),
            NonBinExpr::CleanupCall         => Self::compile_cleanup_call(plan, instrs, label_gen, fienv, env),
            NonBinExpr::CleanupExpr(expr)      => Self::compile_cleanup_expr(*expr, plan, instrs, label_gen, fienv, env),
            NonBinExpr::IdExpr(id)           => Self::compile_id_expr(*id, plan, instrs, label_gen, fienv, env),
            NonBinExpr::LitExpr(lit)          => Self::compile_lit_expr(lit, plan, instrs, label_gen, fienv, env),
            NonBinExpr::ParenExpr(expr)        => Self::compile_paren_expr(*expr, plan, instrs, label_gen, fienv, env),
            NonBinExpr::UnaryExpr(un, expr)     => Self::compile_unary_expr(un, *expr, plan, instrs, label_gen, fienv, env),
            NonBinExpr::IfExpr(cond, expr)        => Self::compile_if_expr(*cond, *expr, plan, instrs, label_gen, fienv, env),
            NonBinExpr::IfElse(cond, expr1, expr2)     => Self::compile_if_else(*cond, *expr1, *expr2, plan, instrs, label_gen, fienv, env),
            NonBinExpr::UnlExpr(cond, expr)       => Self::compile_unless_expr(*cond, *expr, plan, instrs, label_gen, fienv, env),
            NonBinExpr::UnlElse(cond, expr1, expr2)    => Self::compile_unless_else(*cond, *expr1, *expr2, plan, instrs, label_gen, fienv, env),
            NonBinExpr::WhileExpr(cond, expr)     => Self::compile_while_expr(*cond, *expr, plan, instrs, label_gen, fienv, env),
            NonBinExpr::DoWhile(expr, cond)       => Self::compile_do_while(*cond, *expr, plan, instrs, label_gen, fienv, env),
            NonBinExpr::Break               => Self::compile_break(plan, instrs, label_gen, fienv, env),
            NonBinExpr::Continue            => Self::compile_continue(plan, instrs, label_gen, fienv, env),
        }
    }
    pub fn compile_block_expr(mut exprs: Vec<Expr>, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        let last = match exprs.pop() {
            Some(expr) => expr,
            None => {
                // Empty block expression
                match &plan {
                    ReturnPlan::Binop(b, place) => {
                        return Err(format!("Empty block expression but expected to return to a binop expression, {} at {}", b, place));
                    }
                    ReturnPlan::Move(place) => {
                        return Err(format!("Empty block expression but expected to move result to {}", place));
                    }
                    ReturnPlan::Condition => {
                        return Err(format!("Empty block expression but expected to get condition codes from expr"));
                    }
                    ReturnPlan::Push(tt) => {
                        return Err(format!("Empty block expression but expected to push result of type {}", tt));
                    }
                    ReturnPlan::Return => {
                        return Err(format!("Empty block expression but expected to return result of type {}", fienv.return_type()));
                    }
                    ReturnPlan::None => {
                        return Ok(());
                    },
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
        let place = Self::place_from_id_expr(id, instrs, label_gen, fienv, env)?;
        let assign_plan = ReturnPlan::Move(place.clone());
        Self::compile_expr(expr, assign_plan, instrs, label_gen, fienv, env)?;
        plan.into_inter_instr(place, instrs, fienv, env)?;
        Ok(())
    }
    pub fn compile_var_decl_assign(vd: VarDecl, expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        let field = Field::new(vd, env);
        fienv.add_var(&field)?;
        let place = Place::Var(field);
        let assign_plan = ReturnPlan::Move(place.clone());
        Self::compile_expr(expr, assign_plan, instrs, label_gen, fienv, env)?;
        plan.into_inter_instr(place, instrs, fienv, env)?;
        Ok(())
    }
    pub fn compile_return_expr(expr: Option<Expr>, _plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        let plan = ReturnPlan::Return;
        match expr {
            Some(expr) => Self::compile_expr(expr, plan, instrs, label_gen, fienv, env),
            None => {
                if fienv.return_type().is_void() {
                    Err(format!("Must give an expression to return for function with return type {}", fienv.return_type()))
                } else {
                    Ok(())
                }
            }
        }
    }
    pub fn compile_cleanup_call(_plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, _env: &Environment) -> Result<(), String> {
        let cleanup_label = fienv.get_cleanup_label(label_gen);
        let instr = InterInstr::Goto(cleanup_label);
        instrs.push(instr);
        Ok(())
    }
    pub fn compile_cleanup_expr(expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        if fienv.cleanup_expr_created {
            return Err(format!("Found more than one cleanup expression. There should only be one."));
        }
        match plan {
            ReturnPlan::Return => {
                let cleanup_label = fienv.get_cleanup_label(label_gen);
                let instr = InterInstr::Lbl(cleanup_label);
                instrs.push(instr);
                fienv.cleanup_expr_created = true;
                Self::compile_expr(expr, ReturnPlan::Return, instrs, label_gen, fienv, env)?;
                Ok(())
            }
            _ => {
                Err(format!("Can only compile cleanup expression where a return value is expected. The cleanup expression should be at the end of the function."))
            }
        }
    }
    pub fn compile_id_expr(id: IdExpr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        let place = Self::place_from_id_expr(id, instrs, label_gen, fienv, env)?;
        plan.into_inter_instr(place, instrs, fienv, env)?;
        Ok(())
    }
    pub fn compile_lit_expr(lit: Literal, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        match lit {
            Literal::Num(num) => {
                plan.imm_into_inter_instr(Imm::LWord(num), instrs, fienv, env)?;
            }
            Literal::Str(string) => {
                let str_ind = fienv.add_lit_string(string.clone(), label_gen);
                match plan {
                    ReturnPlan::Move(to) => {
                        let instr = InterInstr::Movs(str_ind, to);
                        instrs.push(instr);
                    }
                    ReturnPlan::Push(tt) => {
                        let str_tt = Environment::get_str_tt();
                        if tt != str_tt {
                            return Err(format!("Expected to push value of type {} to stack but found string type {}", tt, str_tt));
                        }
                        let instr = InterInstr::Puss(str_ind);
                        instrs.push(instr);
                    }
                    ReturnPlan::Condition => {
                        return Err(format!("Cannot get condition codes from string literal \"{}\"", string));
                    }
                    ReturnPlan::Binop(b, _) =>  {
                        return Err(format!("Cannot do binary operation {} on string literal \"{}\"", b, string));
                    }
                    ReturnPlan::Return => fienv.rets(str_ind, instrs, env)?,
                    ReturnPlan::None => {}
                }
            }
        }
        Ok(())
    }
    pub fn compile_paren_expr(expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        Self::compile_expr(expr, plan, instrs, label_gen, fienv, env)
    }
    /// Gets a reference to the given NonBinExpr and follows the given ReturnPlan with it.
    pub fn get_reference(nbe: NonBinExpr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        // nbe needs to be an IdExpr
        if let NonBinExpr::IdExpr(id_expr) = nbe {
            let place = Self::place_from_id_expr(*id_expr, instrs, label_gen, fienv, env)?;
            if let ReturnPlan::Binop(b, _) = plan {
                return Err(format!("Binop {} not supported with references yet", b));
            }
            match place {
                Place::Var(field) => {
                    match plan {
                        ReturnPlan::Binop(_, _) => {
                            panic!("Binop should be handled above in this function")
                        }
                        ReturnPlan::Move(to) => {
                            let instr = InterInstr::MoVA(field.name, to);
                            instrs.push(instr);
                            return Ok(());
                        }
                        ReturnPlan::Condition => {
                            let atemp = fienv.get_addr_temp()?;
                            let a_place = Place::ATemp(atemp);
                            let instr = InterInstr::MoVA(field.name, a_place.clone());
                            instrs.push(instr);
                            a_place.free(fienv);
                            Ok(())
                        }
                        ReturnPlan::Push(tt) => {
                            if tt != field.tt {
                                return Err(format!("Cannot convert {} to {}", field.tt, tt));
                            }
                            let instr = InterInstr::PuVA(field.name);
                            instrs.push(instr);
                            return Ok(());
                        }
                        ReturnPlan::Return => {
                            fienv.retva(&field.name, instrs, env)?;
                            warn!("Returning reference to local variable `{}` from function", field.name);
                            return Ok(());
                        }
                        ReturnPlan::None => {
                            // No return plan
                            return Ok(());
                        }
                    }
                }
                Place::ATemp(a) => {
                    return Err(format!("Cannot get reference to temporary address register A{}", a));
                },
                Place::DTemp(d, tt) => {
                    return Err(format!("Cannot get reference to temporary data register {} D{}", tt, d));
                },
                Place::Ref(a, d, off, tt) => {
                    match plan {
                        ReturnPlan::Binop(_, _) => {
                            panic!("Binop should be handled above in this function")
                        }
                        ReturnPlan::Move(to) => {
                            match &to {
                                Place::ATemp(to_a) => {
                                    // Move from Ref to ATemp
                                    let instr = InterInstr::Lea(a, d, off, *to_a);
                                    instrs.push(instr);
                                    fienv.free_addr_temp(a);
                                    if let Some(d) = d { fienv.free_data_temp(d); }
                                    return Ok(());
                                }
                                Place::DTemp(_, to_tt) => {
                                    // Move from Ref to ATemp to DTemp
                                    // Because we are moving an effective address, we have to move
                                    // to an ATemp before we move to a DTemp
                                    if let TypeType::Pointer(to_val_tt) = to_tt {
                                        if tt == **to_val_tt {
                                            let instr = InterInstr::Lea(a, d, off, a);
                                            instrs.push(instr);
                                            let a_place = Place::ATemp(a);
                                            let instr = InterInstr::Move(a_place, to);
                                            instrs.push(instr);
                                            fienv.free_addr_temp(a);
                                            if let Some(d) = d { fienv.free_data_temp(d); }
                                            return Ok(())
                                        } else {
                                            return Err(format!("Trying to cast pointer of type {} to type {}", tt, to_val_tt));
                                        }
                                    } else {
                                        return Err(format!("Trying to move pointer {} to DTemp of type {}",
                                            Place::Ref(a, d, off, tt),
                                            to_tt
                                        ));
                                    }
                                }
                                Place::Var(field) => {
                                    // Move from Ref to Var
                                    if let TypeType::Pointer(to_val_tt) = &field.tt {
                                        if tt == **to_val_tt {
                                            let instr = InterInstr::Lea(a, d, off, a);
                                            instrs.push(instr);
                                            let a_place = Place::ATemp(a);
                                            let instr = InterInstr::Move(a_place, to);
                                            instrs.push(instr);
                                            fienv.free_addr_temp(a);
                                            if let Some(d) = d { fienv.free_data_temp(d); }
                                            return Ok(())
                                        } else {
                                            return Err(format!("Trying to cast pointer of type {} to type {}", tt, to_val_tt));
                                        }
                                    } else {
                                        return Err(format!("Trying to move pointer {} to DTemp of type {}",
                                            Place::Ref(a, d, off, tt),
                                            field.tt
                                        ));
                                    }
                                }
                                Place::Ref(_, _, _, to_tt) => {
                                    // Move effective address from Ref to Ref
                                    // Remember that we are moving the effective address OUT of self,
                                    // but we are moving TO the thing pointed to by the plan's Ref.
                                    if let TypeType::Pointer(to_val_tt) = &to_tt {
                                        if tt == **to_val_tt {
                                            let instr = InterInstr::Lea(a, d, off, a);
                                            instrs.push(instr);
                                            let a_place = Place::ATemp(a);
                                            let instr = InterInstr::Move(a_place, to);
                                            instrs.push(instr);
                                            fienv.free_addr_temp(a);
                                            if let Some(d) = d { fienv.free_data_temp(d); }
                                            return Ok(())
                                        } else {
                                            return Err(format!("Trying to cast pointer of type {} to type {}", tt, to_val_tt));
                                        }
                                    } else {
                                        return Err(format!("Trying to move pointer {} to DTemp of type {}",
                                            Place::Ref(a, d, off, tt),
                                            to_tt
                                        ));
                                    }
                                }
                            }
                        }
                        ReturnPlan::Condition => {
                            // Move from Ref to ATemp
                            let atemp = fienv.get_addr_temp()?;
                            let instr = InterInstr::Lea(a, d, off, atemp);
                            instrs.push(instr);
                            fienv.free_addr_temp(atemp);
                            fienv.free_addr_temp(a);
                            if let Some(d) = d { fienv.free_data_temp(d); }
                            return Ok(());
                            
                        }
                        ReturnPlan::Push(to_tt) => {
                            if let TypeType::Pointer(to_val_tt) = &to_tt {
                                if tt == **to_val_tt {
                                    let instr = InterInstr::Pea(a, d, off);
                                    instrs.push(instr);
                                    fienv.free_addr_temp(a);
                                    if let Some(d) = d { fienv.free_data_temp(d); }
                                    return Ok(())
                                } else {
                                    return Err(format!("Trying to cast pointer of type {} to type {}", tt, to_val_tt));
                                }
                            } else {
                                return Err(format!("Trying to move pointer {} to DTemp of type {}",
                                    Place::Ref(a, d, off, tt),
                                    to_tt
                                ));
                            }
                        }
                        ReturnPlan::Return => {
                            // Move from Ref to ATemp, then return
                            // Because we are moving an effective address, we have to move
                            // to an ATemp before we move to a DTemp
                            let instr = InterInstr::Lea(a, d, off, a);
                            instrs.push(instr);
                            let a_place = Place::ATemp(a);
                            fienv.ret(a_place, instrs, env)?;
                            if let Some(d) = d { fienv.free_data_temp(d); }
                            return Ok(())
                        }
                        ReturnPlan::None => {
                            fienv.free_addr_temp(a);
                            if let Some(d) = d { fienv.free_data_temp(d); }
                            Ok(())
                        }
                    }
                },
            }
        } else {
            Err(format!("Cannot get reference to non-IdExpr"))
        }
    }
    pub fn compile_unary_expr(un: BudUnop, nbe: NonBinExpr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        let tt = plan.get_type(fienv);
        if let BudUnop::Ref = un {
            return Self::get_reference(nbe, plan, instrs, label_gen, fienv, env);
        }
        if let Some(tt) = &tt {
            if !tt.is_magic(env) {
                return Err(format!("Cannot return non-magic type {} from unary operator {}", tt, un));
            }
        }
        match (plan.clone(), un) {
            (
                ReturnPlan::Binop(
                    BudBinop::Times | BudBinop::Div,
                    to
                ) | ReturnPlan::Move(to),
                un
            ) => {
                // Store result of `nbe` into `to`, and then do the unop on `to`
                Self::compile_non_bin_expr(nbe, plan, instrs, label_gen, fienv, env)?;
                let instr = match un {
                    BudUnop::Neg => InterInstr::Neg(to),
                    BudUnop::Not => InterInstr::Bnot(to),
                    BudUnop::Ref => panic!("Ref should be handled above in this function"),
                };
                instrs.push(instr);
            }
            (ReturnPlan::Binop(BudBinop::And | BudBinop::Or, _), BudUnop::Neg) | 
            (ReturnPlan::None, _) => {
                // Store the result of `nbe` into `to`, but do not do the unop
                Self::compile_non_bin_expr(nbe, plan, instrs, label_gen, fienv, env)?;
            }
            (_, BudUnop::Ref) => panic!("Ref should be handled above in this function"),
            (_, un) => {
                // Store the result of `nbe` into a dtemp, do the unop, and then move the dtemp as the plan dictates
                let tt = tt.unwrap();   // if plan is not None, then tt is not None
                let dtemp = fienv.get_data_temp(tt.clone())?;
                let d_place = Place::DTemp(dtemp, tt.clone());
                let instr = match un {
                    BudUnop::Neg => InterInstr::Neg(d_place.clone()),
                    BudUnop::Not => InterInstr::Bnot(d_place.clone()),
                    BudUnop::Ref => panic!("Ref should be handled above in this function"),
                };
                instrs.push(instr);
                plan.into_inter_instr(d_place, instrs, fienv, env)?;
                fienv.free_data_temp(dtemp);
            }
        }
        Ok(())
    }
    fn eval_cond(cond: Expr, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<Place, String> {
        let tt = cond.type_preference(fienv, env)?;
        // Maybe we could use D0--the return register--instead of getting a new register
        // because we don't care about the actual value, only the condition codes
        // Maybe CC should be a ReturnPlan??
        let dtemp = fienv.get_data_temp(tt.clone())?;
        let d_place = Place::DTemp(dtemp, tt.clone());
        let plan = ReturnPlan::Move(d_place.clone());
        Self::compile_expr(cond, plan, instrs, label_gen, fienv, env)?;
        // Condition codes should be set accordingly. I hope they are.
        Ok(d_place)
    }
    pub fn compile_if_expr(cond: Expr, expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        match plan {
            ReturnPlan::None => {}
            ReturnPlan::Return => {
                if !fienv.return_type().is_void() {
                    return Err(format!("Cannot return a value from if statement. Tried to return type {}", fienv.return_type()));
                }
            }
            ReturnPlan::Condition => { return Err(format!("Cannot get condition codes from if statement.")); }
            ReturnPlan::Binop(b, _) => { return Err(format!("Cannot return a value from if statement. Tried to return to binop {}", b)); }
            ReturnPlan::Move(_) => { return Err(format!("Cannot return a value from if statement.")); }
            ReturnPlan::Push(tt) => { return Err(format!("Cannot return a value from if statement. Tried to push type {}", tt)); }
        }
        let d_place = Self::eval_cond(cond, instrs, label_gen, fienv, env)?;
        let f_label = label_gen.next().unwrap();
        let instr = InterInstr::Beq(f_label);
        instrs.push(instr);
        d_place.free(fienv);
        Self::compile_expr(expr, ReturnPlan::None, instrs, label_gen, fienv, env)?;
        let instr = InterInstr::Lbl(f_label);
        instrs.push(instr);
        if let ReturnPlan::Return = plan {
            let instr = InterInstr::Rts;
            instrs.push(instr);
        }
        Ok(())
    }
    pub fn compile_if_else(cond: Expr, expr1: Expr, expr2: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        let d_place = Self::eval_cond(cond, instrs, label_gen, fienv, env)?;
        let f_label = label_gen.next().unwrap();
        let e_label = label_gen.next().unwrap();    // end label
        let instr = InterInstr::Beq(f_label);
        instrs.push(instr);
        d_place.free(fienv);
        Self::compile_expr(expr1, plan.clone(), instrs, label_gen, fienv, env)?;
        let instr = InterInstr::Goto(e_label);
        instrs.push(instr);
        let instr = InterInstr::Lbl(f_label);
        instrs.push(instr);
        Self::compile_expr(expr2, plan, instrs, label_gen, fienv, env)?;
        let instr = InterInstr::Lbl(e_label);
        instrs.push(instr);
        Ok(())
    }
    pub fn compile_unless_expr(cond: Expr, expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        match plan {
            ReturnPlan::None => {}
            ReturnPlan::Return => {
                if !fienv.return_type().is_void() {
                    return Err(format!("Cannot return a value from unless statement. Tried to return type {}", fienv.return_type()));
                }
            }
            ReturnPlan::Condition => { return Err(format!("Cannot get condition codes from unless statement.")); }
            ReturnPlan::Binop(b, _) => { return Err(format!("Cannot return a value from unless statement. Tried to return to binop {}", b)); }
            ReturnPlan::Move(_) => { return Err(format!("Cannot return a value from unless statement.")); }
            ReturnPlan::Push(tt) => { return Err(format!("Cannot return a value from unless statement. Tried to push type {}", tt)); }
        }
        let d_place = Self::eval_cond(cond, instrs, label_gen, fienv, env)?;
        let f_label = label_gen.next().unwrap();
        let instr = InterInstr::Bne(f_label);
        instrs.push(instr);
        d_place.free(fienv);
        Self::compile_expr(expr, ReturnPlan::None, instrs, label_gen, fienv, env)?;
        let instr = InterInstr::Lbl(f_label);
        instrs.push(instr);
        if let ReturnPlan::Return = plan {
            let instr = InterInstr::Rts;
            instrs.push(instr);
        }
        Ok(())
    }
    pub fn compile_unless_else(cond: Expr, expr1: Expr, expr2: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        let d_place = Self::eval_cond(cond, instrs, label_gen, fienv, env)?;
        let f_label = label_gen.next().unwrap();
        let e_label = label_gen.next().unwrap();    // end_label
        let instr = InterInstr::Bne(f_label);
        instrs.push(instr);
        d_place.free(fienv);
        Self::compile_expr(expr1, plan.clone(), instrs, label_gen, fienv, env)?;
        let instr = InterInstr::Goto(e_label);
        instrs.push(instr);
        let instr = InterInstr::Lbl(f_label);
        instrs.push(instr);
        Self::compile_expr(expr2, plan, instrs, label_gen, fienv, env)?;
        let instr = InterInstr::Lbl(e_label);
        instrs.push(instr);
        Ok(())
    }
    pub fn compile_while_expr(cond: Expr, expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        match plan {
            ReturnPlan::None => {}
            ReturnPlan::Return => {
                if !fienv.return_type().is_void() {
                    return Err(format!("Cannot return a value from while loop. Tried to return type {}", fienv.return_type()));
                }
            }
            ReturnPlan::Condition => { return Err(format!("Cannot get condition codes from while loop.")); }
            ReturnPlan::Binop(b, _) => { return Err(format!("Cannot return a value from while loop. Tried to return to binop {}", b)); }
            ReturnPlan::Move(_) => { return Err(format!("Cannot return a value from while loop.")); }
            ReturnPlan::Push(tt) => { return Err(format!("Cannot return a value from while loop. Tried to push type {}", tt)); }
        }
        let continue_label = label_gen.next().unwrap();
        let break_label = label_gen.next().unwrap();
        let instr = InterInstr::Lbl(continue_label);
        instrs.push(instr);
        let d_place = Self::eval_cond(cond, instrs, label_gen, fienv, env)?;
        let instr = InterInstr::Beq(break_label);
        instrs.push(instr);
        d_place.free(fienv);
        fienv.push_loop_stack(break_label, continue_label);
        Self::compile_expr(expr, ReturnPlan::None, instrs, label_gen, fienv, env)?;
        fienv.pop_loop_stack();
        let instr = InterInstr::Lbl(break_label);
        instrs.push(instr);
        if let ReturnPlan::Return = plan {
            let instr = InterInstr::Rts;
            instrs.push(instr);
        }
        Ok(())
    }
    pub fn compile_do_while(cond: Expr, expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
        let tt = expr.type_preference(fienv, env)?;
        let ret_place;
        match &plan {
            ReturnPlan::None => ret_place = None,
            ReturnPlan::Return => {
                if tt.is_array() || tt.is_struct() {
                    return Err(format!("Returning arrays and structs from functions not yet supported"));
                }
                if fienv.return_type().is_void() {
                    ret_place = None;
                } else {
                    let dtemp = fienv.get_data_temp(tt.clone())?;
                    ret_place = Some(Place::DTemp(dtemp, tt));
                }
            }
            ReturnPlan::Condition => {
                if tt.is_array() || tt.is_struct() {
                    return Err(format!("Cannot get condition codes from array or struct"));
                }
                let dtemp = fienv.get_data_temp(tt.clone())?;
                ret_place = Some(Place::DTemp(dtemp, tt));
            }
            ReturnPlan::Binop(b, _) => {
                if tt.is_array() || tt.is_struct() {
                    return Err(format!("Cannot do binop {} on array or struct", b));
                }
                let dtemp = fienv.get_data_temp(tt.clone())?;
                ret_place = Some(Place::DTemp(dtemp, tt));
            }
            ReturnPlan::Move(to) => ret_place = Some(to.clone()),
            ReturnPlan::Push(push_tt) => {
                let dtemp = fienv.get_data_temp(push_tt.clone())?;
                ret_place = Some(Place::DTemp(dtemp, push_tt.clone()));
            }
        }
        let continue_label = label_gen.next().unwrap();
        let break_label = label_gen.next().unwrap();
        let instr = InterInstr::Lbl(continue_label);
        instrs.push(instr);
        fienv.push_loop_stack(break_label, continue_label);
        let body_plan = match &ret_place {
            Some(ret_place) => ReturnPlan::Move(ret_place.clone()),
            None => ReturnPlan::None,
        };
        Self::compile_expr(expr, body_plan, instrs, label_gen, fienv, env)?;
        let d_place = Self::eval_cond(cond, instrs, label_gen, fienv, env)?;
        let instr = InterInstr::Bne(continue_label);
        instrs.push(instr);
        d_place.free(fienv);
        fienv.pop_loop_stack();
        let instr = InterInstr::Lbl(break_label);
        instrs.push(instr);
        if let Some(ret_place) = ret_place {
            plan.into_inter_instr(ret_place, instrs, fienv, env)?;
        } else if let ReturnPlan::Return = plan {
            let instr = InterInstr::Rts;
            instrs.push(instr);
        }
        Ok(())
    }
    pub fn compile_break(_plan: ReturnPlan, instrs: &mut Vec<InterInstr>, _label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, _env: &Environment) -> Result<(), String> {
        match fienv.get_break_label() {
            Some(break_label) => {
                let instr = InterInstr::Goto(break_label);
                instrs.push(instr);
            }
            None => {
                return Err(format!("Break found outside loop"));
            }
        }
        Ok(())
    }
    pub fn compile_continue(_plan: ReturnPlan, instrs: &mut Vec<InterInstr>, _label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, _env: &Environment) -> Result<(), String> {
        match fienv.get_continue_label() {
            Some(continue_label) => {
                let instr = InterInstr::Goto(continue_label);
                instrs.push(instr);
            }
            None => {
                return Err(format!("Continue found outside loop"));
            }
        }
        Ok(())
    }

    pub fn call_func(id: String, offsets: Vec<Expr>, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<Place, String> {
        // Check if it's a function call
        match env.global_funcs.get(&id) {
            Some(sig) => {
                let args = &sig.args;
                if args.len() != offsets.len() {
                    warn!("Function signature for {} has {} arguments but {} were given", id, args.len(), offsets.len());
                }
                // Get register space so that after the args have been pushed 
                // to the stack, active regs can be saved before the function
                // call
                let instr = InterInstr::Grs;
                instrs.push(instr);
                // Maybe I should use a separate number generator in fienv for
                // StackMarkers, but I'm lazy
                let marker = label_gen.next().unwrap();
                let instr = InterInstr::SMarker(marker);
                instrs.push(instr);
                for (i, offset) in offsets.into_iter().enumerate() {
                    let tt = if i < args.len() {
                        args[i].tt.clone()
                    } else {
                        let tt = offset.type_preference(fienv, env)?;
                        warn!("Type used for {}th argument: {}", i, tt);
                        tt
                    };
                    let plan = ReturnPlan::Push(tt);
                    Self::compile_expr(offset, plan, instrs, label_gen, fienv, env)?;
                }
                let instr = InterInstr::Save;
                instrs.push(instr);
                let instr = InterInstr::Call(id.clone(), marker);
                instrs.push(instr);
                let place = env.ret_place(id)?;
                return Ok(place);
            }
            None => {
                return Err(format!("Undeclared variable {}", id));
            }
        }
    }
    ///
    /// Calculates a place that can be read from or written to that represents this IdExpr. DO NOT put arrays or
    /// structs in data registers. `Place::index_into()` will modify the data register to index into it. This place
    /// is live, meaning you are not free to destroy it or modify it unless you actually want to write to it.
    /// 
    /// `match id {`
    /// 
    /// * `IdExpr::SquareIndex(id, offset) =>`
    /// Call `place_from_id_expr` on `id` and `offset`, and index into the given place using `Place::index_into()`.
    /// 
    /// * `IdExpr::RoundIndex(id, offset_exprs) =>`
    /// The id must refer to a function, an array or a reference. 
    /// If it's an function, then call the function with `offset_exprs` as the arguments.
    /// If it's an array or struct, then insert code to check that the first argument of offset_exprs--
    /// which represents the offset--is within the range of this array. If one extra argument is specified, 
    /// it represents the maximum. No exception will be generated if `0 <= offset < max`. If two extra arguments
    /// are specified, they represent a minimum and a maximum respectively. No exception will be generated if 
    /// `min <= offset < max`. Then index into the place the same way as if id was a `SquareIndex`. 
    /// **Not currently implemented.**
    /// 
    /// * `IdExpr::Id(name) =>`
    /// Return this variable as a place
    /// 
    /// * `IdExpr::Deref(id_expr) =>`
    /// Return the place containing `id_expr` by calling `place_from_id_expr` on `id_expr`. Remember, that if `id` 
    /// is `IdExpr::Deref`, then the user typed the dereference operator and wants to assign to the referenced
    /// object or read from it. 
    /// 
    /// `}`
    pub fn place_from_id_expr(id: IdExpr, instrs: &mut Vec<InterInstr>, label_gen: &mut RangeFrom<usize>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<Place, String> {
        match id {
            IdExpr::SquareIndex(id, offset) => {
                // The id must be assignable
                match *id.bin_expr {
                    BinExpr::NonBin(nbe) => {
                        if let NonBinExpr::IdExpr(id_expr) = *nbe {
                            let id_place = Self::place_from_id_expr(*id_expr, instrs, label_gen, fienv, env)?;
                            // Check if the offset is a literal value
                            let mut lit = None;
                            if let BinExpr::NonBin(nbe) = &*offset.bin_expr {
                                if let NonBinExpr::LitExpr(Literal::Num(num)) = &**nbe {
                                    lit = Some(*num);
                                }
                                // TODO Handle struct member access here
                            }
                            match lit {
                                Some(num) => {
                                    let place = id_place.index_into(None, num, false, instrs, fienv, env)?;
                                    return Ok(place);
                                }
                                None => {
                                    let tt = TypeType::Id("i32".to_owned());
                                    let off_temp = fienv.get_data_temp(tt.clone())?;   // offset as an index (not multiplied by sizeof(T))
                                    let off_place = Place::DTemp(off_temp, tt.clone());
                                    let plan = ReturnPlan::Move(off_place.clone());
                                    Self::compile_expr(*offset, plan, instrs, label_gen, fienv, env)?;
                                    let place = id_place.index_into(Some(off_temp), 0, false, instrs, fienv, env)?;
                                    return Ok(place);
                                }
                            }
                        }
                        // id was a NonBinExpr but not an IdExpr
                        Err(format!("Expression not assignable"))
                    }
                    BinExpr::Binary(_, b, _) => {
                        // id was not a NonBinExpr (it was a BinExpr)
                        Err(format!("Cannot index into binary expression, {}", b))
                    }
                }
            },
            IdExpr::RoundIndex(id, offsets) => {
                // The id must be assignable
                match *id.bin_expr {
                    BinExpr::NonBin(nbe) => {
                        if let NonBinExpr::IdExpr(id_expr) = *nbe {
                            let offsets = match offsets {
                                Some(offsets) => offsets,
                                None => Vec::new(),
                            };
                            let offset;
                            let id_place = if let IdExpr::Id(id) = *id_expr {
                                match fienv.get_var(&id) {
                                    Some(field) => {
                                        // variable exists
                                        if offsets.len() == 1 {
                                            offset = offsets.into_iter().next().unwrap();
                                        } else {
                                            return Err(format!("Array indexing with () should only get 1 argument, but {} were given", offsets.len()));
                                        }
                                        Place::Var(field)
                                    }
                                    None => {
                                        // Check if it's a function call
                                        return Self::call_func(id, offsets, instrs, label_gen, fienv, env);
                                    }
                                }
                            } else {
                                // Function pointers not supported, so any other expression means we're
                                // indexing, not calling a function
                                if offsets.len() == 1 {
                                    offset = offsets.into_iter().next().unwrap();
                                } else {
                                    return Err(format!("Array indexing with () should only get 1 argument, but {} were given", offsets.len()));
                                }
                                Self::place_from_id_expr(*id_expr, instrs, label_gen, fienv, env)?
                            };

                            // Check if the offset is a literal value
                            let mut lit = None;
                            if let BinExpr::NonBin(nbe) = &*offset.bin_expr {
                                if let NonBinExpr::LitExpr(Literal::Num(num)) = &**nbe {
                                    lit = Some(*num);
                                }
                            }
                            match lit {
                                Some(num) => {
                                    let place = id_place.index_into(None, num, true, instrs, fienv, env)?;
                                    return Ok(place);
                                }
                                None => {
                                    let tt = TypeType::Id("i32".to_owned());
                                    let off_temp = fienv.get_data_temp(tt.clone())?;   // offset as an index (not multiplied by sizeof(T))
                                    let off_place = Place::DTemp(off_temp, tt.clone());
                                    let plan = ReturnPlan::Move(off_place.clone());
                                    Self::compile_expr(offset, plan, instrs, label_gen, fienv, env)?;
                                    let place = id_place.index_into(Some(off_temp), 0, true, instrs, fienv, env)?;
                                    return Ok(place);
                                }
                            }
                        }
                        // id was a NonBinExpr but not an IdExpr
                        Err(format!("Expression not assignable"))
                    }
                    BinExpr::Binary(_, b, _) => {
                        // id was not a NonBinExpr (it was a BinExpr)
                        Err(format!("Cannot index into binary expression, {}", b))
                    }
                }
            },
            IdExpr::Id(id) => {
                // We need to store the result of expr into the variable, id
                let place = match fienv.get_var(&id) {
                    Some(field) => {
                        Place::Var(field)
                    },
                    None => {
                        return Err(format!("Undeclared variable {}", id));
                    }
                };
                Ok(place)
            },
            IdExpr::Deref(id_expr) => {
                // We need to store the result of expr into the memory
                // location pointed to by the variable, id
                let place = Self::place_from_id_expr(*id_expr, instrs, label_gen, fienv, env)?;
                Ok(place.index_into(None, 0, false, instrs, fienv, env)?)
            },
        }
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
    pub fn as_type(self, tt: TypeType, env: &Environment) -> Result<Imm, String> {
        if tt.is_magic(env) {
            match tt.get_data_size(env) {
                Some(size) => Ok(self.change_size(size)),
                None => panic!("Type {} was magic but did not have a data size", tt),
            }
        } else {
            Err(format!("Cannot convert immediate value {} into non-magic type {}", self, tt))
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

