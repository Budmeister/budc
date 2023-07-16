//! Compilation from the "control" `InterInstr`s to `ValidInstructions`
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
use AReg::SP;
use Either::*;

pub fn compile_lbl_iinstr(lbl: usize, instrs: &mut Vec<ValidInstruction>) -> Result<(), String> {
    let instr = Lbl(lbl).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_goto_iinstr(lbl: usize, instrs: &mut Vec<ValidInstruction>) -> Result<(), String> {
    let instr = Jmp(lbl).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_rts_iinstr(instrs: &mut Vec<ValidInstruction>) -> Result<(), String> {
    let instr = Rts.validate()?;
    instrs.push(instr);
    Ok(())
}
