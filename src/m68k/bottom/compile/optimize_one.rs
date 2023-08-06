//! Optimization of a single ValidInstruction
//! 
//! Author:     Brian Smith
//! Year:       2023

use crate::c_err;
use crate::error::CompilerErr;
use crate::m68k::AddrMode;
use crate::m68k::FunctionEnvironment;
use crate::m68k::Instruction;
use crate::m68k::NumOrLbl;
use crate::m68k::StackHeight;
use crate::m68k::ValidInstruction;
use crate::m68k::Instruction::*;

/// This function does two things:
/// 1. Replaces NumOrLbl::Sum(UncalculatedStackHeight) with NumOrLbl::Num by calculating the stack height
/// 2. Removes instructions with no effect except nop
pub fn optimize_one(mut instr: ValidInstruction, instrs: &mut Vec<ValidInstruction>, fenv: &FunctionEnvironment) -> Result<(), CompilerErr> {
    // 1. Caluclate each instr
    calculated_instr(instr.get_mut(), fenv)?;

    // 2. Remove useless instructions
    match instr.get_mut() {
        Move(_, src, dest) => {
            if src == dest {
                return Ok(());
            }
        },
        MoveMRtoM(_, adbf, _) |
        MoveMMtoR(_, _, adbf) => {
            if adbf.rs_size() == 0 {
                return Ok(());
            }
        },
        _ => {}
    }

    instrs.push(instr);
    Ok(())
}

fn calculated_instr(instr: &mut Instruction, fenv: &FunctionEnvironment) -> Result<(), CompilerErr> {
    let to_calculate = match instr {
        // zero
        Swap(_) |
        AndiCCR(_) |
        OriCCR(_) |
        Stop |
        Nop |
        Reset |
        Jsr(_) |
        Rte |
        Rts |
        Lbl(_) |
        Jmp(_) |
        Bra(_) |
        Beq(_) |
        Bne(_) |
        Bcc(_) |
        Bcs(_) |
        Bge(_) |
        Bgt(_) |
        Ble(_) |
        Blt(_) |
        Bmi(_) |
        Bpl(_) |
        Bhi(_) |
        Bhs(_) |
        Bls(_) |
        Blo(_) |
        Trap(_) |
        Trapv(_) |
        Link(_, _) |
        Unlk(_) |
        ExtW(_) |
        ExtL(_) => (None, None),
        // one
        Neg(_, one) |
        Clr(_, one) |
        Not(_, one) |
        Tst(_, one) |
        Chk(one, _) |
        Pea(one) |
        Lea(one, _) |
        Divs(one, _) |
        Divu(one, _) |
        Muls(one, _) |
        Mulu(one, _) |
        Eor(_, _, one) |
        Cmp(_, one, _) |
        MoveMRtoM(_, _, one) |
        MoveMMtoR(_, one, _) => (Some(one), None),
        // two
        Move(_, one, two) |
        Add(_, one, two) |
        Sub(_, one, two) |
        And(_, one, two) |
        Or(_, one, two) => (Some(one), Some(two)),
        // two but first is optional
        Asl(_, one, two) |
        Asr(_, one, two) |
        Lsl(_, one, two) |
        Lsr(_, one, two) |
        Rol(_, one, two) |
        Ror(_, one, two) |
        Roxl(_, one, two) |
        Roxr(_, one, two) => (one.as_mut(), Some(two)),
    };
    if let Some(one) = to_calculate.0 {
        calculated_addr_mode(one, fenv)?;
    }
    if let Some(two) = to_calculate.1 {
        calculated_addr_mode(two, fenv)?;
    }
    Ok(())
}

fn calculated_addr_mode(addr_mode: &mut AddrMode, fenv: &FunctionEnvironment) -> Result<(), CompilerErr> {
    match addr_mode {
        AddrMode::AIndDisp(num_or_lbl, _) |
        AddrMode::AIndIdxDisp(num_or_lbl, _, _) |
        AddrMode::AbsL(num_or_lbl) |
        AddrMode::Imm(num_or_lbl) => {
            calculated_num_or_lbl(num_or_lbl, fenv)
        }
        _ => Ok(()),
    }
}

fn calculated_num_or_lbl(num_or_lbl: &mut NumOrLbl, fenv: &FunctionEnvironment) -> Result<(), CompilerErr> {
    if let NumOrLbl::Sum(ush) = num_or_lbl {
        let height: StackHeight = match fenv.calculate_stack_height(ush) {
            Ok(Some(height)) => height,
            Ok(None) => return c_err!("Incalculable stack height: {:?}", ush),
            Err(err) => return Err(err),
        };
        *num_or_lbl = NumOrLbl::Num(height);
    }
    Ok(())
}
