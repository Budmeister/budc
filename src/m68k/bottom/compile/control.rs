//! Compilation from the "control" `InterInstr`s to `ValidInstructions`
//! 
//! Author:     Brian Smith
//! Year:       2023

use std::ops::Range;

use crate::{m68k::*, error::BudErr};

use Instruction::*;

pub fn compile_lbl_iinstr(lbl: usize, _range: Range<usize>, instrs: &mut Vec<ValidInstruction>) -> Result<(), BudErr> {
    let instr = Lbl(lbl).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_goto_iinstr(lbl: usize, _range: Range<usize>, instrs: &mut Vec<ValidInstruction>) -> Result<(), BudErr> {
    let instr = Jmp(lbl).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_rts_iinstr(_range: Range<usize>, instrs: &mut Vec<ValidInstruction>) -> Result<(), BudErr> {
    let instr = Unlk(AReg::FP).validate()?;
    instrs.push(instr);
    let instr = Rts.validate()?;
    instrs.push(instr);
    Ok(())
}
