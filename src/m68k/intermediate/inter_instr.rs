//! Author:     Brian Smith
//! Year:       2023

use std::ops::Range;

use crate::{m68k::*, bud::BudBinop, error::CompilerErr};

use super::place::*;



// I am using these Intermediate Instructions to represent 
// instructions before I have generated the stack frame
#[derive(Debug)]
pub enum InterInstr {
    // Data operations
    Binop(Place, BudBinop, Place, Range<usize>),    // Dest. on right (SUB subtracts src from dest. and stores in dest.; DIV, too)
    Binopi(Imm, BudBinop, Place, Range<usize>),
    Neg(Place, Range<usize>),                                 // Neg and Bnot must not be an ATemp
    Bnot(Place, Range<usize>),                                // Boolean NOT--not bitwise NOT
    Move(Place, Place, Range<usize>),
    MoVA(String, Place, Range<usize>),                        // Move Var Address
    Movi(Imm, Place, Range<usize>),
    Movs(usize, Place, Range<usize>),                         // Move string literal (by the string literal's global label)
    Lea(ATemp, Option<DTemp>, i32, ATemp, Range<usize>),      // Load effective address into an address register
    IncSP(StackHeight, Range<usize>),                                 // Add the value to SP
    Push(Place, Range<usize>),
    PuVA(String, Range<usize>),
    Pusi(Imm, DataSize, Range<usize>),
    Puss(usize, Range<usize>),                                // Push string literal (by the string literal's global label)
    Pea(ATemp, Option<DTemp>, i32, Range<usize>),             // Push effective address onto stack
    Chk(ATemp, Option<DTemp>, i32, DTemp, Range<usize>),
    Chki(i16, DTemp, Range<usize>),

    // Stack frame operations
    SMarker(StackMarker, Range<usize>),                       // Marks a stack location for the FunctionInterEnvironment
    Grs(RegisterSpaceLbl, Range<usize>),                      // GetRegisterSpace - allocate space on the stack to save active regs
    Save(RegisterSpaceLbl, Box<[ADTemp]>, Range<usize>),       // Save all active regs by moving them to the given RegisterSpace
    Call(String, StackMarker, Range<usize>),                  // Jsr to function, then move the SP to where it was when the StackMarker
                                                // instruction was encountered and retrieve reg values from most recent Grs
    Lbl(usize, Range<usize>),
    Goto(usize, Range<usize>),
    Rts(Range<usize>),

    // Logic operations
    Tst(Place, Range<usize>),
    Tsti(Imm, Range<usize>),                                  // Pre-calculate the CC and just move that to the CC using `MOVE <ea>, CCR`
    Bcc(usize, Range<usize>),
    Bcs(usize, Range<usize>),
    Beq(usize, Range<usize>),
    Bge(usize, Range<usize>),
    Bgt(usize, Range<usize>),
    Ble(usize, Range<usize>),
    Blt(usize, Range<usize>),
    Bmi(usize, Range<usize>),
    Bne(usize, Range<usize>),
    Bpl(usize, Range<usize>),
    Bra(usize, Range<usize>),
}

#[derive(Copy, Clone, Eq, PartialOrd, Ord, PartialEq, Hash, Debug)]
pub enum DataSize {
    Byte,
    Word,
    LWord,
}
impl std::fmt::Display for DataSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            DataSize::Byte => ".b",
            DataSize::Word => ".w",
            DataSize::LWord => ".l",
        })
    }
}

pub type Imm = i32;

pub fn imm_to_dreg(imm: Imm, instrs: &mut Vec<ValidInstruction>, n: Proxy) -> Result<DReg, CompilerErr> {
    let dreg: DReg = n.into();
    let addr_mode: AddrMode = dreg.into();
    let size = DataSize::LWord;
    let instr = Instruction::Move(size, imm.into(), addr_mode).validate()?;
    instrs.push(instr);
    Ok(dreg)
}
