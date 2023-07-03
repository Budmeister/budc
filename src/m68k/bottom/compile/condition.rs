use crate::m68k::*;

use Instruction::*;
use NumOrLbl::{Num};
use AddrMode::*;
use super::super::fenv::Proxy::*;


pub fn cmp(src: AddrMode, dest: AddrMode, size_src: DataSize, size_dest: DataSize, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment) -> Result<(), String> {
    // Minus doesn't necessarily need a dreg, but it does need to be extended
    let (src, _) = extend_efficient(src, size_src, size_dest, instrs, fenv, Proxy1)?;
    let (dest, _) = fenv.addr_mode_to_dreg(dest, size_dest, instrs, Proxy2)?;
    extend(dest, size_dest, size_src, instrs)?;
    let instr = Cmp(size_dest, src, dest).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn cmpi(src: crate::m68k::Imm, dest: AddrMode, size: DataSize, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment) -> Result<(), String> {
    let (dest, _) = fenv.addr_mode_to_dreg(dest, size, instrs, Proxy1)?;
    let instr = Cmp(size, src.into(), dest).validate()?;
    instrs.push(instr);
    Ok(())
}

/// Moves $1 into the destination if the CC is in the "equal" state. Otherwise,
/// moves $0 into the destination.
pub fn cc_to_eq(size: DataSize, dest: AddrMode, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment) -> Result<(), String> {
    let t_label = fenv.get_new_label();
    let e_label = fenv.get_new_label();
    let mut instr = vec![
        Beq(t_label).validate()?,
        Move(size, Imm(Num(0)), dest.clone()).validate()?,
        Bra(e_label).validate()?,
        Lbl(t_label).validate()?,
        Move(size, Imm(Num(1)), dest).validate()?,
        Lbl(e_label).validate()?,
    ];
    instrs.append(&mut instr);
    Ok(())
}

/// Moves $1 into the destination if the CC is in the "not equal" state. Otherwise,
/// moves $0 into the destination.
pub fn cc_to_ne(size: DataSize, dest: AddrMode, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment) -> Result<(), String> {
    let t_label = fenv.get_new_label();
    let e_label = fenv.get_new_label();
    let mut instr = vec![
        Bne(t_label).validate()?,
        Move(size, Imm(Num(0)), dest.clone()).validate()?,
        Bra(e_label).validate()?,
        Lbl(t_label).validate()?,
        Move(size, Imm(Num(1)), dest).validate()?,
        Lbl(e_label).validate()?,
    ];
    instrs.append(&mut instr);
    Ok(())
}

/// Moves $1 into the destination if the CC is in the "greater than" state. Otherwise,
/// moves $0 into the destination.
pub fn cc_to_gt(size: DataSize, dest: AddrMode, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment) -> Result<(), String> {
    let t_label = fenv.get_new_label();
    let e_label = fenv.get_new_label();
    let mut instr = vec![
        Bgt(t_label).validate()?,
        Move(size, Imm(Num(0)), dest.clone()).validate()?,
        Bra(e_label).validate()?,
        Lbl(t_label).validate()?,
        Move(size, Imm(Num(1)), dest).validate()?,
        Lbl(e_label).validate()?,
    ];
    instrs.append(&mut instr);
    Ok(())
}

/// Moves $1 into the destination if the CC is in the "greater or equal" state. Otherwise,
/// moves $0 into the destination.
pub fn cc_to_ge(size: DataSize, dest: AddrMode, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment) -> Result<(), String> {
    let t_label = fenv.get_new_label();
    let e_label = fenv.get_new_label();
    let mut instr = vec![
        Bge(t_label).validate()?,
        Move(size, Imm(Num(0)), dest.clone()).validate()?,
        Bra(e_label).validate()?,
        Lbl(t_label).validate()?,
        Move(size, Imm(Num(1)), dest).validate()?,
        Lbl(e_label).validate()?,
    ];
    instrs.append(&mut instr);
    Ok(())
}

/// Moves $1 into the destination if the CC is in the "less than" state. Otherwise,
/// moves $0 into the destination.
pub fn cc_to_lt(size: DataSize, dest: AddrMode, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment) -> Result<(), String> {
    let t_label = fenv.get_new_label();
    let e_label = fenv.get_new_label();
    let mut instr = vec![
        Blt(t_label).validate()?,
        Move(size, Imm(Num(0)), dest.clone()).validate()?,
        Bra(e_label).validate()?,
        Lbl(t_label).validate()?,
        Move(size, Imm(Num(1)), dest).validate()?,
        Lbl(e_label).validate()?,
    ];
    instrs.append(&mut instr);
    Ok(())
}

/// Moves $1 into the destination if the CC is in the "less or equal" state. Otherwise,
/// moves $0 into the destination.
pub fn cc_to_le(size: DataSize, dest: AddrMode, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment) -> Result<(), String> {
    let t_label = fenv.get_new_label();
    let e_label = fenv.get_new_label();
    let mut instr = vec![
        Ble(t_label).validate()?,
        Move(size, Imm(Num(0)), dest.clone()).validate()?,
        Bra(e_label).validate()?,
        Lbl(t_label).validate()?,
        Move(size, Imm(Num(1)), dest).validate()?,
        Lbl(e_label).validate()?,
    ];
    instrs.append(&mut instr);
    Ok(())
}
