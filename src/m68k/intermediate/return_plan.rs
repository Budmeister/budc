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

use std::ops::Range;

use crate::{bud::BudBinop, error::*, m68k::*, u_err, c_err};

use super::{inter_instr::*, fienv::FunctionInterEnvironment};

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
    pub fn into_inter_instr(self, from: Place, range: Range<usize>, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
        let from_tt = from.get_type();
        let to_tt;
        match self {
            ReturnPlan::Binop(b, to) => {
                to_tt = to.get_type();
                if !to.is_magic(env)? {
                    return u_err!(range, "Cannot do binary operator {} on non-magic type {}", b, to.get_type());
                }
                if !from.is_magic(env)? {
                    return u_err!(range, "Cannot do binary operator {} on non-magic type {}", b, from.get_type());
                }
                let instr = InterInstr::Binop(from.clone(), b, to, range.to_owned());
                instrs.push(instr);
                from.free(fienv);
            }
            ReturnPlan::Move(to) => {
                to_tt = to.get_type();
                let instr = InterInstr::Move(from.clone(), to, range.to_owned());
                instrs.push(instr);
                from.free(fienv);
            },
            ReturnPlan::Condition => {
                let instr = InterInstr::Tst(from.clone(), range.to_owned());
                instrs.push(instr);
                from.free(fienv);
                return Ok(());
            }
            ReturnPlan::Push(tt) => {
                to_tt = tt;
                let instr = InterInstr::Push(from.clone(), range.to_owned());
                instrs.push(instr);
                from.free(fienv);
            }
            ReturnPlan::Return => {
                fienv.ret(from.clone(), instrs, env, range)?;
                return Ok(());
            },
            ReturnPlan::None => {
                from.free(fienv);
                return Ok(());
            },
        }
        env.tt_converts_to(from_tt, to_tt, Some(&range))?;
        Ok(())
    }
    /// Has no effect if there is no return plan
    /// Pass a size of LWord if it doesn't matter. This function will coerce the size to the one the plan requires.
    pub fn imm_into_inter_instr(self, from: Imm, range: Range<usize>, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
        match self {
            ReturnPlan::Binop(b, to) => {
                let instr = InterInstr::Binopi(from, b, to, range);
                instrs.push(instr);
            }
            ReturnPlan::Move(to) => {
                let instr = InterInstr::Movi(from, to, range);
                instrs.push(instr);
            }
            ReturnPlan::Condition => {
                let instr = InterInstr::Tsti(from, range);
                instrs.push(instr);
            }
            ReturnPlan::Push(tt) => {
                let size = Self::get_imm_size(from, tt, range.to_owned(), env)?;
                let instr = InterInstr::Pusi(from, size, range);
                instrs.push(instr);
            }
            ReturnPlan::Return => fienv.reti(from, instrs, env, range)?,
            ReturnPlan::None => {},
        }
        Ok(())
    }
    fn get_imm_size(from: Imm, tt: TypeType, range: Range<usize>, env: &Environment) -> Result<DataSize, BudErr> {
        if !tt.is_magic(env)? {
            return u_err!(range, "Cannot coerce immediate value {} to non-magic type {}", from, tt);
        }
        match tt.get_data_size(env, Some(&range))? {
            Some(ds) => Ok(ds),
            None => c_err!(range, "Magic type {} has invalid size, {}", tt, tt.get_size(env, Some(&range))?),
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