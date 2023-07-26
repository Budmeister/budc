//! 68000 Instructions are represented by the `ValidInstruction` enum.
//! You should only get a `ValidInstruction` by first constructing an
//! `Instruction` and calling `validate()` on it to make sure you passed
//! the parameters in correctly. This is not the best way to do this, but
//! the difference between valid and invalid 68k instructions is quite 
//! complex.
//! 
//! Author:     Brian Smith
//! Year:       2023

use crate::{m68k::{DataSize, Imm}, error::CompilerErr, c_err};

use DReg::*;
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum DReg {
    D0,
    D1,
    D2,
    D3,
    D4,
    D5,
    D6,
    D7,
}
impl std::fmt::Display for DReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                DReg::D0 => "d0",
                DReg::D1 => "d1",
                DReg::D2 => "d2",
                DReg::D3 => "d3",
                DReg::D4 => "d4",
                DReg::D5 => "d5",
                DReg::D6 => "d6",
                DReg::D7 => "d7",
            }
        )
    }
}

use AReg::*;
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum AReg {
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    SP,
}
impl std::fmt::Display for AReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AReg::A0 => "a0",
                AReg::A1 => "a1",
                AReg::A2 => "a2",
                AReg::A3 => "a3",
                AReg::A4 => "a4",
                AReg::A5 => "a5",
                AReg::A6 => "a6",
                AReg::SP => "sp",
            }
        )
    }
}

use ADReg::*;
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum ADReg {
    D(DReg),
    A(AReg),
}
impl std::fmt::Display for ADReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ADReg::D(d) => d.to_string(),
                ADReg::A(a) => a.to_string(),
            }
        )
    }
}
impl std::fmt::Debug for ADReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum GReg {
    D(DReg),
    A(AReg),
    PC,
}
impl std::fmt::Display for GReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                GReg::D(d) => d.to_string(),
                GReg::A(a) => a.to_string(),
                GReg::PC => "pc".to_owned(),
            }
        )
    }
}
impl std::fmt::Debug for GReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum NumOrLbl {
    Num(Imm),
    NamedLbl(String),
    Lbl(usize),
    Sum(UncalculatedStackHeight),
}
impl std::fmt::Display for NumOrLbl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(num) => write!(f, "{}", num),
            Self::NamedLbl(lbl) => write!(f, "{}", lbl),
            Self::Lbl(lbl) => write!(f, "{}", lbl),
            Self::Sum(ush) => write!(f, "{:?}", ush),
        }
    }
}
impl std::fmt::Debug for NumOrLbl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
impl From<Imm> for NumOrLbl {
    fn from(value: Imm) -> Self {
        Self::Num(value)
    }
}
impl From<String> for NumOrLbl {
    fn from(value: String) -> Self {
        Self::NamedLbl(value)
    }
}
impl From<usize> for NumOrLbl {
    fn from(value: usize) -> Self {
        Self::Lbl(value)
    }
}
impl From<UncalculatedStackHeight> for NumOrLbl {
    fn from(value: UncalculatedStackHeight) -> Self {
        Self::Sum(value)
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum AddrMode {
    /// Data
    D(DReg),
    /// Address
    A(AReg),
    /// Address indirect
    AInd(AReg),
    /// Address indirect post-increment
    AIndInc(AReg),
    /// Address indirect pre-decrement
    AIndDec(AReg),
    /// Address indirect displacement
    AIndDisp(NumOrLbl, AReg),
    /// Address indirect indexed displacement
    AIndIdxDisp(NumOrLbl, AReg, ADReg),
    /// Absolute word
    AbsW(i16),
    /// Absolute long word
    AbsL(NumOrLbl),
    /// Immediate long word
    Imm(NumOrLbl),
}
impl AddrMode {
    pub fn is_dreg(&self) -> bool {
        matches!(self, Self::D(_))
    }
    pub fn is_areg(&self) -> bool {
        matches!(self, Self::A(_))
    }
    pub fn is_imm(&self) -> bool {
        matches!(self, Self::Imm(_))
    }
    pub fn get_push() -> Self {
        Self::AIndDec(SP)
    }
    pub fn get_pop() -> Self {
        Self::AIndInc(SP)
    }
}
impl std::fmt::Display for AddrMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AddrMode::D(d) => write!(f, "{}", d),
            AddrMode::A(a) => write!(f, "{}", a),
            AddrMode::AInd(a) => write!(f, "({})", a),
            AddrMode::AIndInc(a) => write!(f, "({})+", a),
            AddrMode::AIndDec(a) => write!(f, "-({})", a),
            AddrMode::AIndDisp(i, a) => write!(f, "{}({})", i, a),
            AddrMode::AIndIdxDisp(i, a, ad) => write!(f, "({}, {}, {})", i, a, ad),
            AddrMode::AbsW(abs) => write!(f, "{}", abs),
            AddrMode::AbsL(abs) => write!(f, "{}", abs),
            AddrMode::Imm(imm) => write!(f, "#{}", imm),
        }
    }
}
impl From<Imm> for AddrMode {
    fn from(value: Imm) -> Self {
        Self::Imm(NumOrLbl::Num(value))
    }
}
impl From<DReg> for AddrMode {
    fn from(value: DReg) -> Self {
        Self::D(value)
    }
}
impl From<AReg> for AddrMode {
    fn from(value: AReg) -> Self {
        Self::A(value)
    }
}
impl From<ADReg> for AddrMode {
    fn from(value: ADReg) -> Self {
        match value {
            ADReg::A(areg) => areg.into(),
            ADReg::D(dreg) => dreg.into(),
        }
    }
}
impl<T> From<(T, AReg, Option<DReg>)> for AddrMode 
where T: Into<NumOrLbl> {
    fn from((off, areg, dreg): (T, AReg, Option<DReg>)) -> Self {
        match dreg {
            Some(dreg) => Self::AIndIdxDisp(off.into(), areg, D(dreg)),
            None => Self::AIndDisp(off.into(), areg),
        }
    }
}
impl From<NumOrLbl> for AddrMode {
    fn from(value: NumOrLbl) -> Self {
        Self::Imm(value)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct ADBitField {
    d0: bool,
    d1: bool,
    d2: bool,
    d3: bool,
    d4: bool,
    d5: bool,
    d6: bool,
    d7: bool,
    a0: bool,
    a1: bool,
    a2: bool,
    a3: bool,
    a4: bool,
    a5: bool,
    a6: bool,
    sp: bool,
}
impl ADBitField {
    pub fn new(regs: &[ADReg]) -> ADBitField {
        ADBitField {
            d0: regs.contains(&D(D0)),
            d1: regs.contains(&D(D1)),
            d2: regs.contains(&D(D2)),
            d3: regs.contains(&D(D3)),
            d4: regs.contains(&D(D4)),
            d5: regs.contains(&D(D5)),
            d6: regs.contains(&D(D6)),
            d7: regs.contains(&D(D7)),
            a0: regs.contains(&A(A0)),
            a1: regs.contains(&A(A1)),
            a2: regs.contains(&A(A2)),
            a3: regs.contains(&A(A3)),
            a4: regs.contains(&A(A4)),
            a5: regs.contains(&A(A5)),
            a6: regs.contains(&A(A6)),
            sp: regs.contains(&A(SP)),
        }
    }
    pub fn rs_size(&self) -> RegisterSpaceSize {
        let mut size = 0;
        if self.d0 { size += 4; }
        if self.d1 { size += 4; }
        if self.d2 { size += 4; }
        if self.d3 { size += 4; }
        if self.d4 { size += 4; }
        if self.d5 { size += 4; }
        if self.d6 { size += 4; }
        if self.d7 { size += 4; }
        if self.a0 { size += 4; }
        if self.a1 { size += 4; }
        if self.a2 { size += 4; }
        if self.a3 { size += 4; }
        if self.a4 { size += 4; }
        if self.a5 { size += 4; }
        if self.a6 { size += 4; }
        if self.sp { size += 4; }
        size
    }
}
impl std::fmt::Display for ADBitField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.rs_size() == 0 {
            write!(f, "")
        } else {
            write!(
                f,
                "{}",
                &format!(
                    "{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}",
                    if self.d0 { "/d0" } else { "" },
                    if self.d1 { "/d1" } else { "" },
                    if self.d2 { "/d2" } else { "" },
                    if self.d3 { "/d3" } else { "" },
                    if self.d4 { "/d4" } else { "" },
                    if self.d5 { "/d5" } else { "" },
                    if self.d6 { "/d6" } else { "" },
                    if self.d7 { "/d7" } else { "" },
                    if self.a0 { "/a0" } else { "" },
                    if self.a1 { "/a1" } else { "" },
                    if self.a2 { "/a2" } else { "" },
                    if self.a3 { "/a3" } else { "" },
                    if self.a4 { "/a4" } else { "" },
                    if self.a5 { "/a5" } else { "" },
                    if self.a6 { "/a6" } else { "" },
                    if self.sp { "/sp" } else { "" },
                )[1..]
            )
        }
    }
}
impl std::fmt::Debug for ADBitField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Clone)]
pub struct Valid;
#[derive(Debug)]
pub struct Unchecked;

use Instruction::*;

use super::{UncalculatedStackHeight, RegisterSpaceSize};

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct ValidInstruction(Instruction);
impl TryFrom<Instruction> for ValidInstruction {
    type Error = CompilerErr;

    fn try_from(value: Instruction) -> Result<Self, Self::Error> {
        value.validate()
    }
}
impl std::fmt::Debug for ValidInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Instruction {
    // Data movement
    Move(DataSize, AddrMode, AddrMode),
    MoveMRtoM(DataSize, ADBitField, AddrMode),
    MoveMMtoR(DataSize, AddrMode, ADBitField),

    // Arithmetic
    Add(DataSize, AddrMode, AddrMode),
    Sub(DataSize, AddrMode, AddrMode),
    Neg(DataSize, AddrMode),
    Clr(DataSize, AddrMode),
    Not(DataSize, AddrMode),
    Tst(DataSize, AddrMode),
    Cmp(DataSize, AddrMode, DReg),
    Eor(DataSize, DReg, AddrMode), // a.k.a. xor
    And(DataSize, AddrMode, AddrMode),
    Or(DataSize, AddrMode, AddrMode),
    /// The result is a 32-bit value arranged so that the quotient is
    /// the lower-order word nad the remainder is the upper-order word
    Divs(AddrMode, DReg), // dest = 32bits, src = 16bits
    Divu(AddrMode, DReg),
    Muls(AddrMode, DReg), // dest = 32bits, src = 16bits
    Mulu(AddrMode, DReg),

    // Bit manipulation
    Asl(DataSize, Option<AddrMode>, AddrMode),
    Asr(DataSize, Option<AddrMode>, AddrMode),
    Lsl(DataSize, Option<AddrMode>, AddrMode),
    Lsr(DataSize, Option<AddrMode>, AddrMode),
    Rol(DataSize, Option<AddrMode>, AddrMode),
    Ror(DataSize, Option<AddrMode>, AddrMode),
    Roxl(DataSize, Option<AddrMode>, AddrMode),
    Roxr(DataSize, Option<AddrMode>, AddrMode),
    Swap(DReg),

    AndiCCR(u8),
    OriCCR(u8),

    // Control
    Stop,
    Nop,
    Reset,
    Jsr(String),
    Rte,
    Rts,
    Lbl(usize),
    Jmp(usize),
    // General
    Bra(usize),
    Beq(usize),
    Bne(usize),
    Bcc(usize),
    Bcs(usize),
    // Signed
    Bge(usize), // greater or equal
    Bgt(usize), // greater than
    Ble(usize), // less or equal
    Blt(usize), // less than
    Bmi(usize), // minus
    Bpl(usize), // plus
    // Unsigned
    Bhi(usize), // higher
    Bhs(usize), // higher or same
    Bls(usize), // lower or same
    Blo(usize), // lower

    Chk(AddrMode, DReg),
    Trap(u32),
    Trapv(u32),
    Link(i16, AReg),
    Unlk(AReg),
    ExtW(DReg),
    ExtL(DReg),
    Pea(AddrMode),
    Lea(AddrMode, AReg),
}
impl Instruction {
    pub fn validate(mut self) -> Result<ValidInstruction, CompilerErr> {
        match &mut self {
            Move(size, _src, dest) => {
                match &dest {
                    AddrMode::D(_) => {}
                    AddrMode::A(_) => {
                        if *size == DataSize::Byte {
                            return c_err!(
                                "Illegal addressing mode for Move instruction: {}",
                                dest
                            )
                        }
                    } // Moving to areg might need MOVEA instruction
                    AddrMode::AInd(_) => {}
                    AddrMode::AIndInc(_) => {}
                    AddrMode::AIndDec(_) => {}
                    AddrMode::AIndDisp(_, _) => {}
                    AddrMode::AIndIdxDisp(_, _, _) => {}
                    AddrMode::AbsW(_) => {}
                    AddrMode::AbsL(_) => {}
                    _ => {
                        return c_err!(
                            "Illegal addressing mode for Move instruction: {}",
                            dest
                        );
                    }
                }
                Ok(ValidInstruction(self))
            }
            MoveMRtoM(size, _regs, ea) | MoveMMtoR(size, ea, _regs) => {
                if *size == DataSize::Byte {
                    return c_err!("Illegal data size for Movem instruction: {}", size);
                }
                match ea {
                    AddrMode::AInd(_) => {}
                    AddrMode::AIndInc(_) => {}
                    AddrMode::AIndDisp(_, _) => {}
                    AddrMode::AIndIdxDisp(_, _, _) => {}
                    AddrMode::AbsW(_) => {}
                    AddrMode::AbsL(_) => {}
                    _ => {
                        return c_err!("Illegal addressing mode: {:?}", self);
                    }
                }
                Ok(ValidInstruction(self))
            }
            Add(_, _, dest) | Sub(_, _, dest) | Eor(_, _, dest) => {
                match dest {
                    AddrMode::D(_) => {}
                    AddrMode::AInd(_) => {}
                    AddrMode::AIndInc(_) => {}
                    AddrMode::AIndDec(_) => {}
                    AddrMode::AIndDisp(_, _) => {}
                    AddrMode::AIndIdxDisp(_, _, _) => {}
                    AddrMode::AbsW(_) => {}
                    AddrMode::AbsL(_) => {}
                    _ => {
                        return c_err!("Illegal addressing mode: {:?}", self);
                    }
                }
                Ok(ValidInstruction(self))
            }
            Neg(_size, dest) => {
                match dest {
                    AddrMode::D(_) => {}
                    AddrMode::AInd(_) => {}
                    AddrMode::AIndInc(_) => {}
                    AddrMode::AIndDec(_) => {}
                    AddrMode::AIndDisp(_, _) => {}
                    AddrMode::AIndIdxDisp(_, _, _) => {}
                    AddrMode::AbsW(_) => {}
                    AddrMode::AbsL(_) => {}
                    _ => {
                        return c_err!("Illegal addressing mode: {:?}", self);
                    }
                }
                Ok(ValidInstruction(self))
            }
            Clr(_size, dest) => {
                match dest {
                    AddrMode::D(_) => {}
                    AddrMode::AInd(_) => {}
                    AddrMode::AIndInc(_) => {}
                    AddrMode::AIndDec(_) => {}
                    AddrMode::AIndDisp(_, _) => {}
                    AddrMode::AIndIdxDisp(_, _, _) => {}
                    AddrMode::AbsW(_) => {}
                    AddrMode::AbsL(_) => {}
                    _ => {
                        return c_err!("Illegal addressing mode: {:?}", self);
                    }
                }
                Ok(ValidInstruction(self))
            }
            Not(_size, dest) => {
                match dest {
                    AddrMode::D(_) => {}
                    AddrMode::AInd(_) => {}
                    AddrMode::AIndInc(_) => {}
                    AddrMode::AIndDec(_) => {}
                    AddrMode::AIndDisp(_, _) => {}
                    AddrMode::AIndIdxDisp(_, _, _) => {}
                    AddrMode::AbsW(_) => {}
                    AddrMode::AbsL(_) => {}
                    _ => {
                        return c_err!("Illegal addressing mode: {:?}", self);
                    }
                }
                Ok(ValidInstruction(self))
            }
            Tst(_size, dest) => {
                match dest {
                    AddrMode::D(_) => {}
                    AddrMode::AInd(_) => {}
                    AddrMode::AIndInc(_) => {}
                    AddrMode::AIndDec(_) => {}
                    AddrMode::AIndDisp(_, _) => {}
                    AddrMode::AIndIdxDisp(_, _, _) => {}
                    AddrMode::AbsW(_) => {}
                    AddrMode::AbsL(_) => {}
                    _ => {
                        return c_err!("Illegal addressing mode: {:?}", self);
                    }
                }
                Ok(ValidInstruction(self))
            }
            And(_, src, dest) | Or(_, src, dest) => {
                let mut one_data = false;
                match src {
                    AddrMode::D(_) => one_data = true,
                    AddrMode::AInd(_) => {}
                    AddrMode::AIndInc(_) => {}
                    AddrMode::AIndDec(_) => {}
                    AddrMode::AIndDisp(_, _) => {}
                    AddrMode::AIndIdxDisp(_, _, _) => {}
                    AddrMode::AbsW(_) => {}
                    AddrMode::AbsL(_) => {}
                    AddrMode::Imm(_) => {}
                    _ => {
                        return c_err!("Illegal addressing mode: {:?}", self);
                    }
                }
                match dest {
                    AddrMode::D(_) => one_data = true,
                    AddrMode::AInd(_) => {}
                    AddrMode::AIndInc(_) => {}
                    AddrMode::AIndDec(_) => {}
                    AddrMode::AIndDisp(_, _) => {}
                    AddrMode::AIndIdxDisp(_, _, _) => {}
                    AddrMode::AbsW(_) => {}
                    AddrMode::AbsL(_) => {}
                    _ => {
                        return c_err!("Illegal addressing mode: {:?}", self);
                    }
                }
                if !one_data {
                    return c_err!(
                        "At least one addressing mode must be DReg: {:?}",
                        self
                    );
                }
                Ok(ValidInstruction(self))
            }
            Divs(src, _) | Divu(src, _) | Muls(src, _) | Mulu(src, _) => {
                match src {
                    AddrMode::D(_) => {}
                    AddrMode::AInd(_) => {}
                    AddrMode::AIndInc(_) => {}
                    AddrMode::AIndDec(_) => {}
                    AddrMode::AIndDisp(_, _) => {}
                    AddrMode::AIndIdxDisp(_, _, _) => {}
                    AddrMode::AbsW(_) => {}
                    AddrMode::AbsL(_) => {}
                    AddrMode::Imm(_) => {}
                    _ => {
                        return c_err!("Illegal addressing mode: {:?}", self);
                    }
                }
                Ok(ValidInstruction(self))
            }
            Asl(_, src, dest)
            | Asr(_, src, dest)
            | Lsl(_, src, dest)
            | Lsr(_, src, dest)
            | Rol(_, src, dest)
            | Ror(_, src, dest)
            | Roxl(_, src, dest)
            | Roxr(_, src, dest) => {
                let new_src;
                match src {
                    Some(src) => {
                        match src {
                            AddrMode::D(_) => new_src = Some(src.to_owned()),
                            AddrMode::Imm(num_or_lbl) => {
                                if let NumOrLbl::Num(num) = num_or_lbl {
                                    if *num < 1 {
                                        new_src = None;
                                    } else if *num > 8 {
                                        return c_err!(
                                            "Shifting and rotating literals can only be [1,8]: {:?}",
                                            self
                                        );
                                    } else {
                                        new_src = Some(src.to_owned())
                                    }
                                } else {
                                    new_src = Some(src.to_owned());
                                }
                            }
                            _ => {
                                return c_err!("Illegal addressing mode: {:?}", self);
                            }
                        }
                        if !matches!(dest, AddrMode::D(_)) {
                            return c_err!("Illegal addressing mode: {:?}", self);
                        }
                    }
                    None => {
                        new_src = None;
                        match dest {
                            AddrMode::D(_) => {}
                            AddrMode::AInd(_) => {}
                            AddrMode::AIndInc(_) => {}
                            AddrMode::AIndDec(_) => {}
                            AddrMode::AIndDisp(_, _) => {}
                            AddrMode::AIndIdxDisp(_, _, _) => {}
                            AddrMode::AbsW(_) => {}
                            AddrMode::AbsL(_) => {}
                            _ => {
                                return c_err!("Illegal addressing mode: {:?}", self);
                            }
                        }
                    }
                }
                *src = new_src;
                Ok(ValidInstruction(self))
            }
            Chk(bound, _) => {
                match bound {
                    AddrMode::D(_) => {},
                    AddrMode::AInd(_) => {},
                    AddrMode::AIndInc(_) => {},
                    AddrMode::AIndDec(_) => {},
                    AddrMode::AIndDisp(_, _) => {},
                    AddrMode::AIndIdxDisp(_, _, _) => {},
                    AddrMode::AbsW(_) => {},
                    AddrMode::AbsL(_) => {},
                    AddrMode::Imm(_) => {},
                    _ => {
                        return c_err!("Illegal addressing mode: {:?}", self);
                    }
                }
                Ok(ValidInstruction(self))
            }
            Trap(trap) | Trapv(trap) => {
                if *trap >= 16 {
                    return c_err!("Illegal trap vector: {:?}", self);
                }
                Ok(ValidInstruction(self))
            }
            Pea(src) | Lea(src, _) => {
                match src {
                    AddrMode::AInd(_) => {}
                    AddrMode::AIndDisp(_, _) => {}
                    AddrMode::AIndIdxDisp(_, _, _) => {}
                    AddrMode::AbsW(_) => {}
                    AddrMode::AbsL(_) => {}
                    _ => {
                        return c_err!("Illegal addressing mode: {:?}", self);
                    }
                }
                Ok(ValidInstruction(self))
            }

            Cmp(_, _, _)
            | Swap(_)
            | AndiCCR(_)
            | OriCCR(_)
            | Stop
            | Reset
            | Nop
            | Jsr(_)
            | Rte
            | Rts
            | Link(_, _)
            | Unlk(_)
            | ExtW(_)
            | ExtL(_)
            | Lbl(_)
            | Jmp(_)
            | Bra(_)
            | Beq(_)
            | Bne(_)
            | Bcc(_)
            | Bcs(_)
            | Bge(_)
            | Bgt(_)
            | Ble(_)
            | Blt(_)
            | Bmi(_)
            | Bpl(_)
            | Bhi(_)
            | Bhs(_)
            | Bls(_)
            | Blo(_) => Ok(ValidInstruction(self)),
        }
    }
}
