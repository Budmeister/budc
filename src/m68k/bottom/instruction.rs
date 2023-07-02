

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
    Num(i32),
    Lbl(String),
}
impl std::fmt::Display for NumOrLbl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumOrLbl::Num(num) => write!(f, "{}", num),
            NumOrLbl::Lbl(lbl) => write!(f, "{}", lbl),
        }
    }
}
impl std::fmt::Debug for NumOrLbl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
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
    /// PC indirect displacement
    PCIndDisp(NumOrLbl),
    /// PC indirect indexed displacement
    PCIndIdxDisp(NumOrLbl, ADReg),
    /// Immediate word
    ImmW(i16),
    /// Immediate long word
    ImmL(NumOrLbl),
}
impl AddrMode {
    pub fn is_dreg(&self) -> bool {
        matches!(self, Self::D(_))
    }
    pub fn is_areg(&self) -> bool {
        matches!(self, Self::A(_))
    }
    pub fn is_imm(&self) -> bool {
        matches!(self, Self::ImmW(_) | Self::ImmL(_))
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
            AddrMode::PCIndDisp(i) => write!(f, "{}({})", i, GReg::PC),
            AddrMode::PCIndIdxDisp(i, ad) => write!(f, "({}, {}, {})", i, GReg::PC, ad),
            AddrMode::ImmW(imm) => write!(f, "#{}", imm),
            AddrMode::ImmL(imm) => write!(f, "#{}", imm),
        }
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
}
impl std::fmt::Display for ADBitField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

use crate::m68k::DataSize;
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Instruction<State = Unchecked> {
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

    // Control
    Stop,
    Nop,
    Reset,
    Jsr(String),
    Rte,
    Rts,
    Lbl(usize),
    // General
    Bra(usize),
    Beq(usize),
    Bne(usize),
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

    Trap(u32),
    Trapv(u32),
    Link(i16, AReg),
    Unlk(AReg),
    ExtW(DReg),
    ExtL(DReg),
    Pea(AddrMode),
    Lea(AddrMode, AReg),

    // To use the type parameter
    _State(State),
}
impl Instruction<Unchecked> {
    pub fn validate(mut self) -> Result<Instruction<Valid>, String> {
        match &mut self {
            Move(_, _src, dest) => {
                match &dest {
                    AddrMode::D(_) => {}
                    AddrMode::A(_) => {} // Moving to areg might need MOVEA instruction
                    AddrMode::AInd(_) => {}
                    AddrMode::AIndInc(_) => {}
                    AddrMode::AIndDec(_) => {}
                    AddrMode::AIndDisp(_, _) => {}
                    AddrMode::AIndIdxDisp(_, _, _) => {}
                    AddrMode::AbsW(_) => {}
                    AddrMode::AbsL(_) => {}
                    _ => {
                        return Err(format!(
                            "Illegal addressing mode for Move instruction: {}",
                            dest
                        ));
                    }
                }
                Ok(self.validate_unchecked())
            }
            MoveMRtoM(size, _regs, ea) | MoveMMtoR(size, ea, _regs) => {
                if *size == DataSize::Byte {
                    return Err(format!("Illegal data size for Movem instruction: {}", size));
                }
                match ea {
                    AddrMode::AInd(_) => {}
                    AddrMode::AIndInc(_) => {}
                    AddrMode::AIndDisp(_, _) => {}
                    AddrMode::AIndIdxDisp(_, _, _) => {}
                    AddrMode::AbsW(_) => {}
                    AddrMode::AbsL(_) => {}
                    AddrMode::PCIndDisp(_) | AddrMode::PCIndIdxDisp(_, _) => {
                        if let MoveMMtoR(_, _, _) = &self {
                            return Err(format!("Illegal addressing mode: {:?}", self));
                        }
                    }
                    _ => {
                        return Err(format!("Illegal addressing mode: {:?}", self));
                    }
                }
                Ok(self.validate_unchecked())
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
                        return Err(format!("Illegal addressing mode: {:?}", self));
                    }
                }
                Ok(self.validate_unchecked())
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
                        return Err(format!("Illegal addressing mode: {:?}", self));
                    }
                }
                Ok(self.validate_unchecked())
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
                        return Err(format!("Illegal addressing mode: {:?}", self));
                    }
                }
                Ok(self.validate_unchecked())
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
                        return Err(format!("Illegal addressing mode: {:?}", self));
                    }
                }
                Ok(self.validate_unchecked())
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
                    AddrMode::PCIndDisp(_) => {}
                    AddrMode::PCIndIdxDisp(_, _) => {}
                    _ => {
                        return Err(format!("Illegal addressing mode: {:?}", self));
                    }
                }
                Ok(self.validate_unchecked())
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
                    AddrMode::PCIndDisp(_) => {}
                    AddrMode::PCIndIdxDisp(_, _) => {}
                    AddrMode::ImmW(_) => {}
                    AddrMode::ImmL(_) => {}
                    _ => {
                        return Err(format!("Illegal addressing mode: {:?}", self));
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
                        return Err(format!("Illegal addressing mode: {:?}", self));
                    }
                }
                if !one_data {
                    return Err(format!(
                        "At least one addressing mode must be DReg: {:?}",
                        self
                    ));
                }
                Ok(self.validate_unchecked())
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
                    AddrMode::PCIndDisp(_) => {}
                    AddrMode::PCIndIdxDisp(_, _) => {}
                    AddrMode::ImmW(_) => {}
                    AddrMode::ImmL(_) => {}
                    _ => {
                        return Err(format!("Illegal addressing mode: {:?}", self));
                    }
                }
                Ok(self.validate_unchecked())
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
                            AddrMode::ImmW(w) => {
                                if *w < 1 {
                                    new_src = None;
                                } else if *w > 8 {
                                    return Err(format!(
                                        "Shifting and rotating literals can only be [1,8]: {:?}",
                                        self
                                    ));
                                } else {
                                    new_src = Some(src.to_owned())
                                }
                            }
                            AddrMode::ImmL(_l) => {
                                new_src = Some(src.to_owned());
                            }
                            _ => {
                                return Err(format!("Illegal addressing mode: {:?}", self));
                            }
                        }
                        if !matches!(dest, AddrMode::D(_)) {
                            return Err(format!("Illegal addressing mode: {:?}", self));
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
                                return Err(format!("Illegal addressing mode: {:?}", self));
                            }
                        }
                    }
                }
                *src = new_src;
                Ok(self.validate_unchecked())
            }
            Trap(trap) | Trapv(trap) => {
                if *trap >= 16 {
                    return Err(format!("Illegal trap vector: {:?}", self));
                }
                Ok(self.validate_unchecked())
            }
            Pea(src) | Lea(src, _) => {
                match src {
                    AddrMode::AInd(_) => {}
                    AddrMode::AIndDisp(_, _) => {}
                    AddrMode::AIndIdxDisp(_, _, _) => {}
                    AddrMode::AbsW(_) => {}
                    AddrMode::AbsL(_) => {}
                    AddrMode::PCIndDisp(_) => {}
                    AddrMode::PCIndIdxDisp(_, _) => {}
                    _ => {
                        return Err(format!("Illegal addressing mode: {:?}", self));
                    }
                }
                Ok(self.validate_unchecked())
            }

            Cmp(_, _, _)
            | Swap(_)
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
            | Bra(_)
            | Beq(_)
            | Bne(_)
            | Bge(_)
            | Bgt(_)
            | Ble(_)
            | Blt(_)
            | Bmi(_)
            | Bpl(_)
            | Bhi(_)
            | Bhs(_)
            | Bls(_)
            | Blo(_) => Ok(self.validate_unchecked()),

            _State(_) => panic!("{:?} not to be used", self),
        }
    }
    fn validate_unchecked(self) -> Instruction<Valid> {
        match self {
            Move(size, src, dest) => Move(size, src, dest),
            MoveMRtoM(size, regs, ea) => MoveMRtoM(size, regs, ea),
            MoveMMtoR(size, ea, regs) => MoveMMtoR(size, ea, regs),
            Add(size, src, dest) => Add(size, src, dest),
            Sub(size, src, dest) => Sub(size, src, dest),
            Neg(size, dest) => Neg(size, dest),
            Clr(size, dest) => Clr(size, dest),
            Not(size, dest) => Not(size, dest),
            Tst(size, dest) => Tst(size, dest),
            Cmp(size, src, dest) => Cmp(size, src, dest),
            Eor(size, src, dest) => Eor(size, src, dest),
            And(size, src, dest) => And(size, src, dest),
            Or(size, src, dest) => Or(size, src, dest),
            Divs(src, dest) => Divs(src, dest),
            Divu(src, dest) => Divu(src, dest),
            Muls(src, dest) => Muls(src, dest),
            Mulu(src, dest) => Mulu(src, dest),
            Asl(size, src, dest) => Asl(size, src, dest),
            Asr(size, src, dest) => Asr(size, src, dest),
            Lsl(size, src, dest) => Lsl(size, src, dest),
            Lsr(size, src, dest) => Lsr(size, src, dest),
            Rol(size, src, dest) => Rol(size, src, dest),
            Ror(size, src, dest) => Ror(size, src, dest),
            Roxl(size, src, dest) => Roxl(size, src, dest),
            Roxr(size, src, dest) => Roxr(size, src, dest),
            Swap(dest) => Swap(dest),
            Stop => Stop,
            Nop => Nop,
            Reset => Reset,
            Jsr(lbl) => Jsr(lbl),
            Rte => Rte,
            Rts => Rts,
            Trap(trap) => Trap(trap),
            Trapv(trap) => Trapv(trap),
            Link(num, areg) => Link(num, areg),
            Unlk(areg) => Unlk(areg),
            ExtW(dreg) => ExtW(dreg),
            ExtL(dreg) => ExtL(dreg),
            Pea(ea) => Pea(ea),
            Lea(ea, areg) => Lea(ea, areg),
            Lbl(lbl) => Lbl(lbl),
            Bra(lbl) => Bra(lbl),
            Beq(lbl) => Beq(lbl),
            Bne(lbl) => Bne(lbl),
            Bge(lbl) => Bge(lbl),
            Bgt(lbl) => Bgt(lbl),
            Ble(lbl) => Ble(lbl),
            Blt(lbl) => Blt(lbl),
            Bmi(lbl) => Bmi(lbl),
            Bpl(lbl) => Bpl(lbl),
            Bhi(lbl) => Bhi(lbl),
            Bhs(lbl) => Bhs(lbl),
            Bls(lbl) => Bls(lbl),
            Blo(lbl) => Blo(lbl),
            _State(_) => panic!(),
        }
    }
}
