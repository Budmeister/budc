//! Compilation from the "logic" `InterInstr`s to `ValidInstruction`s
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

pub fn compile_tst_iinstr(from: Place, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
    if from.is_array() || from.is_struct() || from.is_void() {
        return Err(format!("Cannot get condition codes from value of type {}", from.get_type()));
    }
    let size = from.get_data_size(env).unwrap();
    let from = if fenv.place_is_areg(&from) {
        fenv.place_to_dreg(from, instrs, env, Proxy1)?.0.into()
    } else {
        fenv.place_to_addr_mode(from, instrs, Proxy1)?
    };
    let instr = Tst(size, from).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_tsti_iinstr(from: Imm, instrs: &mut Vec<ValidInstruction>) -> Result<(), String> {
    const _X: u8 = 0x10;
    const N: u8 = 0x8;
    const Z: u8 = 0x4;
    const V: u8 = 0x2;
    const C: u8 = 0x1;
    let mut and = 0xff;
    let mut or = 0;
    and &= !C;
    and &= !V;
    if from == 0 {
        or |= Z;
    } else {
        and &= !Z;
    }
    if from < 0 {
        or |= N;
    } else {
        and &= !N;
    }
    let instr = AndiCCR(and).validate()?;
    instrs.push(instr);
    let instr = OriCCR(or).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_bcc_iinstr(lbl: usize, instrs: &mut Vec<ValidInstruction>) -> Result<(), String> {
    let instr = Bcc(lbl).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_bcs_iinstr(lbl: usize, instrs: &mut Vec<ValidInstruction>) -> Result<(), String> {
    let instr = Bcs(lbl).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_beq_iinstr(lbl: usize, instrs: &mut Vec<ValidInstruction>) -> Result<(), String> {
    let instr = Beq(lbl).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_bge_iinstr(lbl: usize, instrs: &mut Vec<ValidInstruction>) -> Result<(), String> {
    let instr = Bge(lbl).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_bgt_iinstr(lbl: usize, instrs: &mut Vec<ValidInstruction>) -> Result<(), String> {
    let instr = Bgt(lbl).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_ble_iinstr(lbl: usize, instrs: &mut Vec<ValidInstruction>) -> Result<(), String> {
    let instr = Ble(lbl).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_blt_iinstr(lbl: usize, instrs: &mut Vec<ValidInstruction>) -> Result<(), String> {
    let instr = Blt(lbl).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_bmi_iinstr(lbl: usize, instrs: &mut Vec<ValidInstruction>) -> Result<(), String> {
    let instr = Bmi(lbl).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_bne_iinstr(lbl: usize, instrs: &mut Vec<ValidInstruction>) -> Result<(), String> {
    let instr = Bne(lbl).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_bpl_iinstr(lbl: usize, instrs: &mut Vec<ValidInstruction>) -> Result<(), String> {
    let instr = Bpl(lbl).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_bra_iinstr(lbl: usize, instrs: &mut Vec<ValidInstruction>) -> Result<(), String> {
    let instr = Bra(lbl).validate()?;
    instrs.push(instr);
    Ok(())
}

