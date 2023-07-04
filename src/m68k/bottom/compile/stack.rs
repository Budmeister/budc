//! Compilation from the "stack" `InterInstr`s to `ValidInstructions`
//! 
//! Author:     Brian Smith
//! Year:       2023

use crate::m68k::*;

use log::*;

use super::condition::*;

use Instruction::*;
use Proxy::*;
use DataSize::*;
use NumOrLbl::Num;
use ADReg::*;


pub fn compile_mova_iinstr(name: String, to: Place, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String>
{
    // Only get the destination as AReg if it is already AReg
    let (to_areg, live) = if fenv.place_is_areg(&to) {
        fenv.place_to_areg(to.clone(), instrs, env, Proxy1)?
    } else {
        (Proxy1.into(), false)
    };
    let from = fenv.var_as_addr_mode(name)?;
    let instr = Lea(from, to_areg).validate()?;
    instrs.push(instr);
    if !live {
        let to = fenv.place_to_addr_mode(to, instrs, Proxy2)?;
        let instr = Move(LWord, to_areg.into(), to).validate()?;
        instrs.push(instr);
    }
    Ok(())
}

pub fn compile_movs_iinstr(string_lbl: usize, to: Place, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), String>
{
    let from = NumOrLbl::Lbl(string_lbl).into();
    let to = fenv.place_to_addr_mode(to, instrs, Proxy1)?;
    let instr = Move(LWord, from, to).validate()?;
    instrs.push(instr);
    Ok(())
}