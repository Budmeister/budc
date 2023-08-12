//! Optimization of a single ValidInstruction
//! 
//! Author:     Brian Smith
//! Year:       2023

use crate::error::CompilerErr;
use crate::m68k::AddrMode;
use crate::m68k::NumOrLbl;
use crate::m68k::ValidInstruction;
use crate::m68k::Instruction::*;

/// This function does one thing:
/// 1. Removes instructions with no effect except nop
pub fn optimize_one(mut instr: ValidInstruction, instrs: &mut Vec<ValidInstruction>) -> Result<(), CompilerErr> {
    // 1. Remove useless instructions
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
        Lea(from, to) => {
            if let AddrMode::AInd(areg) | AddrMode::AIndDisp(NumOrLbl::Num(0), areg) = from {
                if areg == to {
                    return Ok(());
                }
            }
        }
        _ => {}
    }

    instrs.push(instr);
    Ok(())
}
