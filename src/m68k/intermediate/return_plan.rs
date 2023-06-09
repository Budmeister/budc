//! `ReturnPlan`s tell an expression where to put its output. It is
//! typically desired that the expression being compiled move its output
//! to a given `Place`, but sometimes the compiling function can 
//! optimize the code by knowing that the output is about to be combined
//! with another `Place` with a binop or is about to be pushed onto the
//! stack.
//! 
//! Note: Only magic types (`i8`, `i16`, `i32`) can be combined with 
//! binops.
//! 
//! Author:     Brian Smith
//! Year:       2023

use crate::{bud::BudBinop, m68k::*};

use super::{inter_instr::*, fienv::FunctionInterEnvironment};

use log::*;

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
                to_tt = tt;
                let instr = InterInstr::Push(from.clone());
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
                let instr = InterInstr::Binopi(from, b, to);
                instrs.push(instr);
            }
            ReturnPlan::Move(to) => {
                let instr = InterInstr::Movi(from, to);
                instrs.push(instr);
            }
            ReturnPlan::Condition => {
                let instr = InterInstr::Tsti(from);
                instrs.push(instr);
            }
            ReturnPlan::Push(tt) => {
                let size = Self::get_imm_size(from, tt, env)?;
                let instr = InterInstr::Pusi(from, size);
                instrs.push(instr);
            }
            ReturnPlan::Return => fienv.reti(from, instrs, env)?,
            ReturnPlan::None => {},
        }
        Ok(())
    }
    fn get_imm_size(from: Imm, tt: TypeType, env: &Environment) -> Result<DataSize, String> {
        if !tt.is_magic(env) {
            return Err(format!("Cannot coerce immediate value {} to non-magic type {}", from, tt));
        }
        match tt.get_data_size(env) {
            Some(ds) => Ok(ds),
            None => Err(format!("Magic type {} has invalid size, {}", tt, tt.get_size(env))),
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