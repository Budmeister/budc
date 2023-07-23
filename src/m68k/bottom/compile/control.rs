//! Compilation from the "control" `InterInstr`s to `ValidInstructions`
//! 
//! Author:     Brian Smith
//! Year:       2023

use crate::{m68k::*, error::BudErr};

use Instruction::*;

pub fn compile_lbl_iinstr(lbl: usize, instrs: &mut Vec<ValidInstruction>) -> Result<(), BudErr> {
    let instr = Lbl(lbl).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_goto_iinstr(lbl: usize, instrs: &mut Vec<ValidInstruction>) -> Result<(), BudErr> {
    let instr = Jmp(lbl).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_rts_iinstr(instrs: &mut Vec<ValidInstruction>) -> Result<(), BudErr> {
    let instr = Rts.validate()?;
    instrs.push(instr);
    Ok(())
}
