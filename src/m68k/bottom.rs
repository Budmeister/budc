use std::collections::HashMap;
use std::collections::HashSet;

use crate::bud::BudBinop;
use crate::m68k::intermediate::*;
use crate::m68k::top::*;
use log::*;

#[derive(Hash, Clone, Eq, PartialEq)]
pub enum StackItem {
    Var(String),
    DTemp(DTemp),
    ATemp(ATemp),
}
impl StackItem {
    pub fn is_var(&self) -> bool {
        matches!(self, Self::Var(_))
    }
    pub fn is_dtemp(&self) -> bool {
        matches!(self, Self::DTemp(_))
    }
    pub fn is_atemp(&self) -> bool {
        matches!(self, Self::ATemp(_))
    }
}

pub struct FunctionEnvironment {
    pub lit_strings: Vec<(usize, String)>,
    pub vars: Vec<Field>,
    dtemp_map: HashMap<DTemp, Option<DReg>>,
    atemp_map: HashMap<ATemp, Option<AReg>>,
    extra_stack_height: usize,
    stack_frame: HashMap<StackItem, i32>,
}
impl FunctionEnvironment {
    pub fn new(
        signature: Signature,
        lit_strings: Vec<(usize, String)>,
        vars: Vec<Field>,
        dtemp_map: HashMap<DTemp, Option<DReg>>,
        atemp_map: HashMap<ATemp, Option<AReg>>,
        env: &Environment,
    ) -> FunctionEnvironment {
        let stack_frame =
            Self::generate_stack_frame(&signature, &vars, &dtemp_map, &atemp_map, env);
        FunctionEnvironment {
            lit_strings,
            vars,
            dtemp_map,
            atemp_map,
            extra_stack_height: 0,
            stack_frame,
        }
    }
    /// Generates the stack frame. The stack frame is organized like this:
    /// ```txt
    /// +-----------------------------------+   higher addresses
    /// |               params              |
    /// +-----------------------------------+
    /// |            spilled regs           |
    /// +-----------------------------------+
    /// |             local vars            |
    /// +-----------------------------------+  <--- extra_stack_height = 0
    /// |    saved regs for function call   |
    /// +-----------------------------------+   lower addresses
    /// ```
    fn generate_stack_frame(
        signature: &Signature,
        vars: &[Field],
        dtemp_map: &HashMap<DTemp, Option<DReg>>,
        atemp_map: &HashMap<ATemp, Option<AReg>>,
        env: &Environment,
    ) -> HashMap<StackItem, i32> {
        // Build stack frame from the bottom up
        let mut stack_height: i32 = 0;
        let params = signature.args
            .iter()
            .cloned()
            .collect::<HashSet<Field>>();
        let mut stack_frame = HashMap::new();

        // Add local vars to stack frame
        for var in vars.iter().rev() {
            if params.contains(var) {
                continue;
            }
            let size = var.tt.get_size(env);
            let item = StackItem::Var(var.name.clone());
            stack_frame.insert(item, stack_height);
            stack_height += size as i32;
        }

        // Add spilled regs to stack frame
        for (dtemp, dreg) in dtemp_map {
            if dreg.is_some() {
                // Only add spilled regs to the stack frame
                continue;
            }
            // We are just going to assume all dregs are 4 bytes, even though they could be 2 or 1
            let size = 4;
            let item = StackItem::DTemp(*dtemp);
            stack_frame.insert(item, stack_height);
            stack_height += size;
        }
        for (atemp, areg) in atemp_map {
            if areg.is_some() {
                continue;
            }
            let size = 4;
            let item = StackItem::ATemp(*atemp);
            stack_frame.insert(item, stack_height);
            stack_height += size;
        }

        // Add params to stack frame
        for param in &signature.args {
            let size = param.tt.get_size(env);
            let item = StackItem::Var(param.name.clone());
            stack_frame.insert(item, stack_height);
            stack_height += size as i32;
        }

        stack_frame
    }
    /// Returns an addressing mode that points to the given place.
    ///
    /// The returned AddrMode is always live, but it may use a proxy if the
    /// given Place is Ref
    ///
    /// n is a `bool` that tells whether to use `ADDR_PROXY1` and `DATA_PROXY1`
    /// or `ADDR_PROXY2` and `DATA_PROXY2` if place is Ref.
    pub fn place_to_addr_mode(
        &self,
        place: Place,
        instrs: &mut Vec<Instruction<Valid>>,
        n: Proxy,
    ) -> Result<AddrMode, String> {
        match place {
            Place::Var(name) => self.stack_item_to_addr_mode(StackItem::Var(name.name)),
            Place::ATemp(atemp) => match self.atemp_map.get(&atemp).unwrap() {
                Some(areg) => Ok(AddrMode::A(*areg)),
                None => Ok(self.stack_item_to_addr_mode(StackItem::ATemp(atemp))?),
            },
            Place::DTemp(dtemp, tt) => {
                if tt.is_array() || tt.is_struct() {
                    return Err(format!(
                        "Array or struct {} being stored in dtemp D{}",
                        tt, dtemp
                    ));
                }
                match self.dtemp_map.get(&dtemp).unwrap() {
                    Some(dreg) => Ok(AddrMode::D(*dreg)),
                    None => self.stack_item_to_addr_mode(StackItem::DTemp(dtemp)),
                }
            }
            Place::Ref(atemp, dtemp, off, tt) => {
                // What do we do with tt?
                let (areg, _) = self.atemp_as_areg(atemp, instrs, n)?;
                match dtemp {
                    Some(dtemp) => {
                        let (dreg, _) = self.dtemp_as_dreg(dtemp, instrs, n)?;
                        Ok(AddrMode::AIndIdxDisp(NumOrLbl::Num(off), areg, D(dreg)))
                    }
                    None => {
                        if off != 0 {
                            Ok(AddrMode::AIndDisp(NumOrLbl::Num(off), areg))
                        } else {
                            Ok(AddrMode::AInd(areg))
                        }
                    }
                }
            }
        }
    }
    /// The returned AddrMode is always live.
    pub fn stack_item_to_addr_mode(&self, stack_item: StackItem) -> Result<AddrMode, String> {
        match stack_item.clone() {
            StackItem::Var(name) => match self.stack_frame.get(&stack_item) {
                Some(stack_diff) => Ok(AddrMode::AIndDisp(NumOrLbl::Num(*stack_diff), SP)),
                None => Err(format!("Unrecognized variable {}", name)),
            },
            StackItem::DTemp(dtemp) => match self.dtemp_map.get(&dtemp) {
                Some(Some(dreg)) => Ok(AddrMode::D(*dreg)),
                _ => match self.stack_frame.get(&stack_item) {
                    Some(stack_diff) => Ok(AddrMode::AIndDisp(NumOrLbl::Num(*stack_diff), SP)),
                    None => Err(format!("DTemp {} not in dtemp_map or stack_frame", dtemp)),
                },
            },
            StackItem::ATemp(atemp) => match self.atemp_map.get(&atemp) {
                Some(Some(areg)) => Ok(AddrMode::A(*areg)),
                _ => match self.stack_frame.get(&stack_item) {
                    Some(stack_diff) => Ok(AddrMode::AIndDisp(NumOrLbl::Num(*stack_diff), SP)),
                    None => Err(format!("ATemp {} not in atemp_map or stack_frame", atemp)),
                },
            },
        }
    }
    /// Returns true if the DReg is live. Otherwise, modifications to the value do not affect the original Place.
    pub fn place_to_dreg(
        &self,
        place: Place,
        instrs: &mut Vec<Instruction<Valid>>,
        env: &Environment,
        n: Proxy,
    ) -> Result<(DReg, bool), String> {
        match place {
            Place::Var(name) => {
                if name.tt.is_array() || name.tt.is_struct() {
                    return Err(format!(
                        "Cannot move array or struct {} into data reg",
                        name
                    ));
                }
                let proxy = n.into();
                let size = name.tt.get_data_size(env).unwrap();
                let from = self.stack_item_to_addr_mode(StackItem::Var(name.name))?;
                let to = AddrMode::D(proxy);
                let instr = Instruction::Move(size, from, to);
                instrs.push(instr);
                Ok((proxy, false))
            }
            Place::ATemp(atemp) => {
                let proxy = n.into();
                let from = match self.atemp_map.get(&atemp).unwrap() {
                    Some(areg) => AddrMode::A(*areg),
                    None => self.stack_item_to_addr_mode(StackItem::ATemp(atemp))?,
                };
                let to = AddrMode::D(proxy);
                let size = DataSize::LWord;
                let instr = Instruction::Move(size, from, to).validate()?;
                instrs.push(instr);
                Ok((proxy, false))
            }
            Place::DTemp(dtemp, tt) => {
                // What do we do with tt?
                self.dtemp_as_dreg(dtemp, instrs, n)
            }
            Place::Ref(_, _, _, _) => todo!(),
        }
    }
    fn atemp_as_areg(
        &self,
        atemp: ATemp,
        instrs: &mut Vec<Instruction<Valid>>,
        n: Proxy,
    ) -> Result<(AReg, bool), String> {
        match self.atemp_map.get(&atemp).unwrap() {
            Some(areg) => Ok((*areg, true)),
            None => {
                let proxy = n.into();
                let from = self.stack_item_to_addr_mode(StackItem::ATemp(atemp))?;
                let to = AddrMode::A(proxy);
                let instr = Instruction::Move(DataSize::LWord, from, to).validate()?;
                instrs.push(instr);
                Ok((proxy, false))
            }
        }
    }
    fn dtemp_as_dreg(
        &self,
        dtemp: DTemp,
        instrs: &mut Vec<Instruction<Valid>>,
        n: Proxy,
    ) -> Result<(DReg, bool), String> {
        match self.dtemp_map.get(&dtemp).unwrap() {
            Some(dreg) => Ok((*dreg, true)),
            None => {
                let proxy = n.into();
                let from = self.stack_item_to_addr_mode(StackItem::DTemp(dtemp))?;
                let to = AddrMode::D(proxy);
                // Data temps are all assumed to be 4 bytes here
                let instr = Instruction::Move(DataSize::LWord, from, to).validate()?;
                instrs.push(instr);
                Ok((proxy, false))
            }
        }
    }
}

pub type StackMarker = usize;

#[derive(Copy, Clone)]
pub enum Proxy {
    Proxy1,
    Proxy2,
}
impl From<Proxy> for DReg {
    fn from(val: Proxy) -> Self {
        match val {
            Proxy1 => Function::DATA_PROXY1,
            Proxy2 => Function::DATA_PROXY2,
        }
    }
}
impl From<Proxy> for AReg {
    fn from(val: Proxy) -> Self {
        match val {
            Proxy1 => Function::ADDR_PROXY1,
            Proxy2 => Function::ADDR_PROXY2,
        }
    }
}
use Proxy::*;

impl Function {
    const DATA_PROXY1: DReg = D6;
    const DATA_PROXY2: DReg = D7;
    const ADDR_PROXY1: AReg = A5;
    const ADDR_PROXY2: AReg = A6;
    pub fn get_instrs(
        iinstrs: Vec<InterInstr>,
        fienv: FunctionInterEnvironment,
        env: &Environment,
    ) -> Result<Vec<Instruction<Valid>>, String> {
        let mut instrs = Vec::new();
        let (dtemp_map, atemp_map) = Self::get_temp_maps(&iinstrs);
        let mut fenv = FunctionEnvironment::new(
            fienv.sig,
            fienv.lit_strings,
            fienv.vars,
            dtemp_map,
            atemp_map,
            env,
        );
        for iinstr in iinstrs {
            Self::compile_iinstr(iinstr, &mut instrs, &mut fenv, env)?;
        }
        Ok(instrs)
    }
    fn update_temp_usages(temp_usages: &mut HashMap<usize, u32>, temp: usize) {
        match temp_usages.get_mut(&temp) {
            Some(usages) => *usages += 1,
            None => {
                temp_usages.insert(temp, 1);
            }
        }
    }
    fn update_temp_usages_by_place(
        dtemp_usages: &mut HashMap<DTemp, u32>,
        atemp_usages: &mut HashMap<ATemp, u32>,
        place: &Place,
    ) {
        match place {
            Place::Var(_) => {}
            Place::ATemp(atemp) => {
                Self::update_temp_usages(atemp_usages, *atemp);
            }
            Place::DTemp(dtemp, _) => {
                Self::update_temp_usages(dtemp_usages, *dtemp);
            }
            Place::Ref(atemp, dtemp, _, _) => {
                Self::update_temp_usages(atemp_usages, *atemp);
                if let Some(dtemp) = dtemp {
                    Self::update_temp_usages(dtemp_usages, *dtemp);
                }
            }
        }
    }
    fn get_temp_maps(
        iinstrs: &Vec<InterInstr>,
    ) -> (HashMap<DTemp, Option<DReg>>, HashMap<ATemp, Option<AReg>>) {
        let mut dtemp_usages = HashMap::new();
        let mut atemp_usages = HashMap::new();
        for iinstr in iinstrs {
            match iinstr {
                InterInstr::Binop(left, _, right) | InterInstr::Move(left, right) => {
                    Self::update_temp_usages_by_place(&mut dtemp_usages, &mut atemp_usages, left);
                    Self::update_temp_usages_by_place(&mut dtemp_usages, &mut atemp_usages, right);
                }

                InterInstr::Binopi(_, _, place)
                | InterInstr::Neg(place)
                | InterInstr::Bnot(place)
                | InterInstr::MoVA(_, place)
                | InterInstr::Movi(_, place)
                | InterInstr::Movs(_, place)
                | InterInstr::Push(place, _)
                | InterInstr::Tst(place) => {
                    Self::update_temp_usages_by_place(&mut dtemp_usages, &mut atemp_usages, place);
                }

                InterInstr::Chki(_, dtemp) => Self::update_temp_usages(&mut dtemp_usages, *dtemp),
                InterInstr::Lea(atemp1, dtemp, _, atemp2) => {
                    Self::update_temp_usages(&mut atemp_usages, *atemp1);
                    if let Some(dtemp) = dtemp {
                        Self::update_temp_usages(&mut dtemp_usages, *dtemp);
                    }
                    Self::update_temp_usages(&mut atemp_usages, *atemp2);
                }
                InterInstr::Pea(atemp, dtemp, _) => {
                    Self::update_temp_usages(&mut atemp_usages, *atemp);
                    if let Some(dtemp) = dtemp {
                        Self::update_temp_usages(&mut dtemp_usages, *dtemp);
                    }
                }
                InterInstr::Chk(atemp, dtemp1, _, dtemp2) => {
                    Self::update_temp_usages(&mut atemp_usages, *atemp);
                    if let Some(dtemp1) = dtemp1 {
                        Self::update_temp_usages(&mut dtemp_usages, *dtemp1);
                    }
                    Self::update_temp_usages(&mut dtemp_usages, *dtemp2);
                }

                InterInstr::Call(_, _) => {}
                InterInstr::SMarker(_) => {}
                InterInstr::Lbl(_) => {}
                InterInstr::Goto(_) => {}
                InterInstr::Rts => {}
                InterInstr::Grs => {}
                InterInstr::Save => {}
                InterInstr::PuVA(_) => {}
                InterInstr::Pusi(_) => {}
                InterInstr::Puss(_) => {}
                InterInstr::Tsti(_) => {}
                InterInstr::Bcc(_) => {}
                InterInstr::Bcs(_) => {}
                InterInstr::Beq(_) => {}
                InterInstr::Bge(_) => {}
                InterInstr::Bgt(_) => {}
                InterInstr::Ble(_) => {}
                InterInstr::Blt(_) => {}
                InterInstr::Bmi(_) => {}
                InterInstr::Bne(_) => {}
                InterInstr::Bpl(_) => {}
                InterInstr::Bra(_) => {}
            }
        }
        let mut dtemp_popular = dtemp_usages
            .into_iter()
            .map(|(dtemp, usages)| (usages, dtemp))
            .collect::<Vec<(u32, DTemp)>>();
        let mut atemp_popular = atemp_usages
            .into_iter()
            .map(|(atemp, usages)| (usages, atemp))
            .collect::<Vec<(u32, ATemp)>>();
        dtemp_popular.sort_by(|a, b| b.0.cmp(&a.0));
        atemp_popular.sort_by(|a, b| b.0.cmp(&a.0));

        let dregs = [D0, D1, D2, D3, D4, D5];
        let aregs = [A0, A1, A2, A3, A4];

        let dtemp_map = Iterator::chain(
            // Make sure DTemp(0) maps to DReg::D0
            [(0, 0)].into_iter(),
            dtemp_popular.into_iter().filter(|(_, dtemp)| *dtemp != 0),
        )
        .enumerate()
        .map(|(i, (_usages, dtemp))| {
            if i < dregs.len() {
                (dtemp, Some(dregs[i]))
            } else {
                (dtemp, None)
            }
        })
        .collect();

        let atemp_map = atemp_popular
            .into_iter()
            .enumerate()
            .map(|(i, (_usages, atemp))| {
                if i < aregs.len() {
                    (atemp, Some(aregs[i]))
                } else {
                    (atemp, None)
                }
            })
            .collect();

        (dtemp_map, atemp_map)
    }
    pub fn compile_iinstr(
        iinstr: InterInstr,
        instrs: &mut Vec<Instruction<Valid>>,
        fenv: &mut FunctionEnvironment,
        env: &Environment,
    ) -> Result<(), String> {
        match iinstr {
            InterInstr::Binop(left, b, right) => {
                match b {
                    BudBinop::Plus => {
                        let size = match right.get_data_size(env) {
                            Some(size) => size,
                            None => {
                                return Err(format!(
                                    "Cannot do binop {} on type {}",
                                    b,
                                    right.get_type()
                                ))
                            }
                        };
                        let left = fenv.place_to_addr_mode(left, instrs, Proxy1)?;
                        let right = fenv.place_to_addr_mode(right, instrs, Proxy2)?;
                        let instr = Instruction::Add(size, left, right).validate()?;
                        instrs.push(instr);
                        Ok(())
                    }
                    BudBinop::Minus => {
                        let size = match right.get_data_size(env) {
                            Some(size) => size,
                            None => {
                                return Err(format!(
                                    "Cannot do binop {} on type {}",
                                    b,
                                    right.get_type()
                                ))
                            }
                        };
                        let left = fenv.place_to_addr_mode(left, instrs, Proxy1)?;
                        let right = fenv.place_to_addr_mode(right, instrs, Proxy2)?;
                        let instr = Instruction::Sub(size, left, right).validate()?;
                        instrs.push(instr);
                        Ok(())
                    }
                    BudBinop::Times => {
                        if left.is_array()
                            || left.is_struct()
                            || right.is_array()
                            || right.is_struct()
                        {
                            return Err(format!(
                                "Cannot do binop {} on types {} and {}",
                                b,
                                left.get_type(),
                                right.get_type()
                            ));
                        }
                        let size_right = right.get_data_size(env).unwrap();
                        let size_left = left.get_data_size(env).unwrap();
                        if size_right == DataSize::LWord || size_left == DataSize::LWord {
                            warn!("Multiplication on 32 bit integers will cast to 16 bit integers")
                        }
                        let left = fenv.place_to_addr_mode(left, instrs, Proxy1)?;
                        let (right_dreg, live) =
                            fenv.place_to_dreg(right.clone(), instrs, env, Proxy2)?;
                        let instr = Instruction::Muls(left, right_dreg).validate()?;
                        instrs.push(instr);
                        if !live {
                            let right_real = fenv.place_to_addr_mode(right, instrs, Proxy1)?;
                            let right_dead = AddrMode::D(right_dreg);
                            let size = DataSize::Word;
                            let instr = Move(size, right_dead, right_real).validate()?;
                            instrs.push(instr);
                        }
                        Ok(())
                    }
                    BudBinop::Div => {
                        if left.is_array()
                            || left.is_struct()
                            || right.is_array()
                            || right.is_struct()
                        {
                            return Err(format!(
                                "Cannot do binop {} on types {} and {}",
                                b,
                                left.get_type(),
                                right.get_type()
                            ));
                        }
                        let size_right = right.get_data_size(env).unwrap();
                        let size_left = left.get_data_size(env).unwrap();
                        if size_left == DataSize::LWord {
                            warn!("Division on 32 bit integers will cast to 16 bit integers")
                        }
                        // Assume divide signed
                        let left = fenv.place_to_addr_mode(left, instrs, Proxy1)?;
                        let (right_dreg, live) =
                            fenv.place_to_dreg(right.clone(), instrs, env, Proxy2)?;
                        if size_right == DataSize::Byte {
                            let instr = Instruction::ExtW(right_dreg).validate()?;
                            instrs.push(instr);
                        }
                        if size_right == DataSize::Byte || size_right == DataSize::Word {
                            let instr = Instruction::ExtL(right_dreg).validate()?;
                            instrs.push(instr);
                        }
                        let instr = Instruction::Divs(left, right_dreg).validate()?;
                        instrs.push(instr);
                        if !live {
                            let right_real = fenv.place_to_addr_mode(right, instrs, Proxy1)?;
                            let right_dead = AddrMode::D(right_dreg);
                            // The top word in this dreg is the remainder. I guess we just lose it...
                            let size = DataSize::Word;
                            let instr = Move(size, right_dead, right_real).validate()?;
                            instrs.push(instr);
                        }
                        Ok(())
                    }
                    BudBinop::And => todo!(),
                    BudBinop::Or => todo!(),
                    BudBinop::BitAnd => todo!(),
                    BudBinop::BitOr => todo!(),
                    BudBinop::BitXor => todo!(),
                    BudBinop::Equal => todo!(),
                    BudBinop::NotEq => todo!(),
                    BudBinop::Greater => todo!(),
                    BudBinop::GrtrEq => todo!(),
                    BudBinop::Less => todo!(),
                    BudBinop::LessEq => todo!(),
                }
            }
            InterInstr::Binopi(_, _, _) => todo!(),
            InterInstr::Neg(_) => todo!(),
            InterInstr::Bnot(_) => todo!(),
            InterInstr::Move(_, _) => todo!(),
            InterInstr::MoVA(_, _) => todo!(),
            InterInstr::Movi(_, _) => todo!(),
            InterInstr::Movs(_, _) => todo!(),
            InterInstr::Lea(_, _, _, _) => todo!(),
            InterInstr::Push(_, _) => todo!(),
            InterInstr::PuVA(_) => todo!(),
            InterInstr::Pusi(_) => todo!(),
            InterInstr::Puss(_) => todo!(),
            InterInstr::Pea(_, _, _) => todo!(),
            InterInstr::Chk(_, _, _, _) => todo!(),
            InterInstr::Chki(_, _) => todo!(),
            InterInstr::SMarker(_) => todo!(),
            InterInstr::Call(_, _) => todo!(),
            InterInstr::Lbl(_) => todo!(),
            InterInstr::Goto(_) => todo!(),
            InterInstr::Rts => todo!(),
            InterInstr::Grs => todo!(),
            InterInstr::Save => todo!(),
            InterInstr::Tst(_) => todo!(),
            InterInstr::Tsti(_) => todo!(),
            InterInstr::Bcc(_) => todo!(),
            InterInstr::Bcs(_) => todo!(),
            InterInstr::Beq(_) => todo!(),
            InterInstr::Bge(_) => todo!(),
            InterInstr::Bgt(_) => todo!(),
            InterInstr::Ble(_) => todo!(),
            InterInstr::Blt(_) => todo!(),
            InterInstr::Bmi(_) => todo!(),
            InterInstr::Bne(_) => todo!(),
            InterInstr::Bpl(_) => todo!(),
            InterInstr::Bra(_) => todo!(),
        }
    }
}

#[derive(Clone)]
pub struct CompiledFunction {
    pub signature: Signature,
    pub instructions: Vec<Instruction<Valid>>,
}

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
    Cmp(DataSize, AddrMode, AddrMode),
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
    Stop,
    Nop,
    Reset,
    Jsr(String),
    Rte,
    Rts,
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
            Asl(_, src, dest) |
            Asr(_, src, dest) |
            Lsl(_, src, dest) |
            Lsr(_, src, dest) |
            Rol(_, src, dest) |
            Ror(_, src, dest) |
            Roxl(_, src, dest) |
            Roxr(_, src, dest) => {
                let new_src;
                match src {
                    Some(src) => {
                        match src {
                            AddrMode::D(_) => new_src = Some(src.to_owned()),
                            AddrMode::ImmW(w) => {
                                if *w < 1 {
                                    new_src = None;
                                } else if *w > 8 {
                                    return Err(format!("Shifting and rotating literals can only be [1,8]: {:?}", self));
                                } else {
                                    new_src = Some(src.to_owned())
                                }
                            },
                            AddrMode::ImmL(_l) => {
                                new_src = Some(src.to_owned());
                            },
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
                            AddrMode::D(_) => {},
                            AddrMode::AInd(_) => {},
                            AddrMode::AIndInc(_) => {},
                            AddrMode::AIndDec(_) => {},
                            AddrMode::AIndDisp(_, _) => {},
                            AddrMode::AIndIdxDisp(_, _, _) => {},
                            AddrMode::AbsW(_) => {},
                            AddrMode::AbsL(_) => {},
                            _ => {
                                return Err(format!("Illegal addressing mode: {:?}", self));
                            }
                        }
                    },
                }
                *src = new_src;
                Ok(self.validate_unchecked())
            }
            Trap(trap) | Trapv(trap) => {
                if *trap >= 16 {
                    return Err(format!("Illegal trap vector: {:?}", self));
                }
                Ok(self.validate_unchecked())
            },
            Pea(src) |
            Lea(src, _) => {
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
            },

            Cmp(_, _, _) |
            Swap(_) |
            Stop |
            Reset |
            Nop |
            Jsr(_) |
            Rte |
            Rts |
            Link(_, _) |
            Unlk(_) |
            ExtW(_) |
            ExtL(_) 
             => Ok(self.validate_unchecked()),

            _State(_) => panic!("{:?} not to be used", self),
        }
    }
    fn validate_unchecked(self) -> Instruction<Valid> {
        match self {
            Move(size, src, dest) => Instruction::Move(size, src, dest),
            MoveMRtoM(size, regs, ea) => Instruction::MoveMRtoM(size, regs, ea),
            MoveMMtoR(size, ea, regs) => Instruction::MoveMMtoR(size, ea, regs),
            Add(size, src, dest) => Instruction::Add(size, src, dest),
            Sub(size, src, dest) => Instruction::Sub(size, src, dest),
            Neg(size, dest) => Instruction::Neg(size, dest),
            Clr(size, dest) => Instruction::Clr(size, dest),
            Not(size, dest) => Instruction::Not(size, dest),
            Tst(size, dest) => Instruction::Tst(size, dest),
            Cmp(size, src, dest) => Instruction::Cmp(size, src, dest),
            Eor(size, src, dest) => Instruction::Eor(size, src, dest),
            And(size, src, dest) => Instruction::And(size, src, dest),
            Or(size, src, dest) => Instruction::Or(size, src, dest),
            Divs(src, dest) => Instruction::Divs(src, dest),
            Divu(src, dest) => Instruction::Divu(src, dest),
            Muls(src, dest) => Instruction::Muls(src, dest),
            Mulu(src, dest) => Instruction::Mulu(src, dest),
            Asl(size, src, dest) => Instruction::Asl(size, src, dest),
            Asr(size, src, dest) => Instruction::Asr(size, src, dest),
            Lsl(size, src, dest) => Instruction::Lsl(size, src, dest),
            Lsr(size, src, dest) => Instruction::Lsr(size, src, dest),
            Rol(size, src, dest) => Instruction::Rol(size, src, dest),
            Ror(size, src, dest) => Instruction::Ror(size, src, dest),
            Roxl(size, src, dest) => Instruction::Roxl(size, src, dest),
            Roxr(size, src, dest) => Instruction::Roxr(size, src, dest),
            Swap(dest) => Instruction::Swap(dest),
            Stop => Instruction::Stop,
            Nop => Instruction::Nop,
            Reset => Instruction::Reset,
            Jsr(lbl) => Instruction::Jsr(lbl),
            Rte => Instruction::Rte,
            Rts => Instruction::Rts,
            Trap(trap) => Instruction::Trap(trap),
            Trapv(trap) => Instruction::Trapv(trap),
            Link(num, areg) => Instruction::Link(num, areg),
            Unlk(areg) => Instruction::Unlk(areg),
            ExtW(dreg) => Instruction::ExtW(dreg),
            ExtL(dreg) => Instruction::ExtL(dreg),
            Pea(ea) => Instruction::Pea(ea),
            Lea(ea, areg) => Instruction::Lea(ea, areg),
            _State(_) => panic!(),
        }
    }
}
