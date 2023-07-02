use crate::{m68k::*, bud::BudBinop};

use super::place::*;



// I am using these Intermediate Instructions to represent 
// instructions before I have generated the stack frame
#[derive(Debug)]
pub enum InterInstr {
    // Data operations
    Binop(Place, BudBinop, Place),    // Dest. on right (SUB subtracts src from dest. and stores in dest.; DIV, too)
    Binopi(Imm, BudBinop, Place),
    Neg(Place),                                 // Neg and Bnot must not be an ATemp
    Bnot(Place),                                // Boolean NOT--not bitwise NOT
    Move(Place, Place),
    MoVA(String, Place),                        // Move Var Address
    Movi(Imm, Place),
    Movs(usize, Place),                         // Move string literal (by the string literal's global label)
    Lea(ATemp, Option<DTemp>, i32, ATemp),      // Load effective address into an address register
    Push(Place, TypeType),
    PuVA(String),
    Pusi(Imm),
    Puss(usize),                                // Push string literal (by the string literal's global label)
    Pea(ATemp, Option<DTemp>, i32),             // Push effective address onto stack
    Chk(ATemp, Option<DTemp>, i32, DTemp),
    Chki(i16, DTemp),

    // Stack frame operations
    SMarker(StackMarker),                       // Marks a stack location for the FunctionInterEnvironment
    Call(String, StackMarker),                  // Jsr to function, then move the SP to where it was when the StackMarker
                                                // instruction was encountered and retrieve reg values from most recent Gsr
    Lbl(usize),
    Goto(usize),
    Rts,
    Grs,                                        // GetRegisterSpace - allocate space on the stack to save active regs
    Save,                                       // Save all active regs by moving them to the given RegisterSpace

    // Logic operations
    Tst(Place),
    Tsti(Imm),                                  // Pre-calculate the CC and just move that to the CC using `MOVE <ea>, CCR`
    Bcc(usize),
    Bcs(usize),
    Beq(usize),
    Bge(usize),
    Bgt(usize),
    Ble(usize),
    Blt(usize),
    Bmi(usize),
    Bne(usize),
    Bpl(usize),
    Bra(usize),
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
            DataSize::Byte => "b",
            DataSize::Word => "w",
            DataSize::LWord => "l",
        })
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Imm {
    Byte(i8),
    Word(i16),
    LWord(i32),
}
impl Imm {
    pub fn change_size(self, size: DataSize) -> Imm {
        let val = match self {
            Imm::Byte(v) => v as i32,
            Imm::Word(v) => v as i32,
            Imm::LWord(v) => v,
        };
        match size {
            DataSize::Byte => Imm::Byte(val as i8),
            DataSize::Word => Imm::Word(val as i16),
            DataSize::LWord => Imm::LWord(val),
        }
    }
    pub fn as_type(self, tt: TypeType, env: &Environment) -> Result<Imm, String> {
        if tt.is_magic(env) {
            match tt.get_data_size(env) {
                Some(size) => Ok(self.change_size(size)),
                None => panic!("Type {} was magic but did not have a data size", tt),
            }
        } else {
            Err(format!("Cannot convert immediate value {} into non-magic type {}", self, tt))
        }
    }
}
impl std::fmt::Display for Imm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Imm::Byte(v) => write!(f, "{}.b", v),
            Imm::Word(v) => write!(f, "{}.w", v),
            Imm::LWord(v) => write!(f, "{}.l", v),
        }
    }
}