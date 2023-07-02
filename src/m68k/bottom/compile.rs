use std::collections::HashMap;

use crate::{bud::BudBinop, m68k::*};

use super::{instruction::*, fenv::{FunctionEnvironment, Proxy}};
use Instruction::*;
use DReg::*;
use AReg::*;
use Proxy::*;
use log::*;

impl Function {
    pub const DATA_PROXY1: DReg = D6;
    pub const DATA_PROXY2: DReg = D7;
    pub const ADDR_PROXY1: AReg = A5;
    pub const ADDR_PROXY2: AReg = A6;
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
            fienv.label_gen,
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
    fn extend(
        dreg: DReg,
        from: DataSize,
        to: DataSize,
        instrs: &mut Vec<Instruction<Valid>>,
    ) -> Result<(), String> {
        if from >= to {
            return Ok(());
        }
        if from == DataSize::Byte {
            let instr = ExtW(dreg).validate()?;
            instrs.push(instr);
        }
        if to == DataSize::LWord {
            let instr = ExtL(dreg).validate()?;
            instrs.push(instr);
        }
        Ok(())
    }
    /// The returned bool indicates if the AddrMode is live. If no extension occurs, then it will
    /// always be live.
    ///
    /// If `from >= to`, then no extension needs to occur, so place is just converted to AddrMode.
    ///
    /// Otherwise, the place is converted to DReg.
    fn extend_efficient(
        place: Place,
        from: DataSize,
        to: DataSize,
        instrs: &mut Vec<Instruction<Valid>>,
        fenv: &mut FunctionEnvironment,
        env: &Environment,
        n: Proxy,
    ) -> Result<(AddrMode, bool), String> {
        if from >= to {
            Ok((fenv.place_to_addr_mode(place, instrs, n)?, true))
        } else {
            let (dreg, live) = fenv.place_to_dreg(place, instrs, env, n)?;
            Self::extend(dreg, from, to, instrs)?;
            Ok((AddrMode::D(dreg), live))
        }
    }
    pub fn compile_iinstr(
        iinstr: InterInstr,
        instrs: &mut Vec<Instruction<Valid>>,
        fenv: &mut FunctionEnvironment,
        env: &Environment,
    ) -> Result<(), String> {
        match iinstr {
            InterInstr::Binop(src, b, dest) => Self::compile_binop_iinstr(src, b, dest, instrs, fenv, env),
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
    fn compile_binop_iinstr(src: Place, b: BudBinop, dest: Place, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
        if src.is_array() || src.is_struct() || dest.is_array() || dest.is_struct() {
            return Err(format!(
                "Cannot do binop {} on types {} and {}",
                b,
                src.get_type(),
                dest.get_type()
            ));
        }
        let size_dest = dest.get_data_size(env).unwrap();
        let size_src = src.get_data_size(env).unwrap();
        match b {
            BudBinop::Plus => {
                // Plus doesn't necessarily need a dreg, but it does need to be extended
                let (src, _) = Self::extend_efficient(
                    src, size_src, size_dest, instrs, fenv, env, Proxy1,
                )?;
                let dest = fenv.place_to_addr_mode(dest, instrs, Proxy2)?;
                let instr = Add(size_dest, src, dest).validate()?;
                instrs.push(instr);
                Ok(())
            }
            BudBinop::Minus | BudBinop::Equal => {
                // Minus doesn't necessarily need a dreg, but it does need to be extended
                let (src, _) = Self::extend_efficient(
                    src, size_src, size_dest, instrs, fenv, env, Proxy1,
                )?;
                let dest = fenv.place_to_addr_mode(dest, instrs, Proxy2)?;
                let instr = Sub(size_dest, src, dest).validate()?;
                instrs.push(instr);
                Ok(())
            }
            BudBinop::Times => {
                if size_src == DataSize::LWord || size_dest == DataSize::LWord {
                    warn!("Multiplication on 32 bit integers will cast to 16 bit integers")
                }
                let src = fenv.place_to_addr_mode(src, instrs, Proxy1)?;
                // Times needs to go to a dreg
                let (dest_dreg, live) =
                    fenv.place_to_dreg(dest.clone(), instrs, env, Proxy2)?;
                let instr = Muls(src, dest_dreg).validate()?;
                instrs.push(instr);
                if !live {
                    let dest_real = fenv.place_to_addr_mode(dest, instrs, Proxy1)?;
                    let dest_dead = AddrMode::D(dest_dreg);
                    let size = DataSize::Word;
                    let instr = Move(size, dest_dead, dest_real).validate()?;
                    instrs.push(instr);
                }
                Ok(())
            }
            BudBinop::Div => {
                if size_src == DataSize::LWord {
                    warn!("The dividend ")
                }
                // Assume divide signed
                let src = fenv.place_to_addr_mode(src, instrs, Proxy1)?;
                // Div needs to go to a dreg
                let (dest_dreg, live) =
                    fenv.place_to_dreg(dest.clone(), instrs, env, Proxy2)?;
                Self::extend(dest_dreg, size_dest, DataSize::LWord, instrs)?;
                let instr = Divs(src, dest_dreg).validate()?;
                instrs.push(instr);
                if !live {
                    let dest_real = fenv.place_to_addr_mode(dest, instrs, Proxy1)?;
                    let dest_dead = AddrMode::D(dest_dreg);
                    // The top word in this dreg is the remainder. I guess we just lose it...
                    let size = DataSize::Word;
                    let instr = Move(size, dest_dead, dest_real).validate()?;
                    instrs.push(instr);
                }
                Ok(())
            }
            BudBinop::And => {
                let f_lbl = fenv.get_new_label();
                let e_lbl = fenv.get_new_label();
                // We could optimize this by calling place_to_addr_mode for src after dest has been tested
                let src = fenv.place_to_addr_mode(src, instrs, Proxy1)?;
                let dest = fenv.place_to_addr_mode(dest, instrs, Proxy2)?;
                let mut instr = vec![
                    Tst(size_dest, dest.clone()).validate()?,
                    Beq(f_lbl).validate()?,
                    Tst(size_src, src).validate()?,
                    Beq(f_lbl).validate()?,
                    Move(size_dest, AddrMode::ImmL(NumOrLbl::Num(1)), dest.clone())
                        .validate()?,
                    Bra(e_lbl).validate()?,
                    Lbl(f_lbl).validate()?,
                    Move(size_dest, AddrMode::ImmL(NumOrLbl::Num(0)), dest).validate()?,
                    Lbl(e_lbl).validate()?,
                ];
                instrs.append(&mut instr);
                Ok(())
            }
            BudBinop::Or => {
                let t_lbl = fenv.get_new_label();
                let e_lbl = fenv.get_new_label();
                // We could optimize this by calling place_to_addr_mode for src after dest has been tested
                let src = fenv.place_to_addr_mode(src, instrs, Proxy1)?;
                let dest = fenv.place_to_addr_mode(dest, instrs, Proxy2)?;
                let mut instr = vec![
                    Tst(size_dest, dest.clone()).validate()?,
                    Bne(t_lbl).validate()?,
                    Tst(size_src, src).validate()?,
                    Bne(t_lbl).validate()?,
                    Move(size_dest, AddrMode::ImmL(NumOrLbl::Num(0)), dest.clone())
                        .validate()?,
                    Bra(e_lbl).validate()?,
                    Lbl(t_lbl).validate()?,
                    Move(size_dest, AddrMode::ImmL(NumOrLbl::Num(1)), dest).validate()?,
                ];
                instrs.append(&mut instr);
                Ok(())
            }
            BudBinop::BitAnd => {
                let (src, _) = Self::extend_efficient(
                    src, size_src, size_dest, instrs, fenv, env, Proxy1,
                )?;
                let (dest_dead, live) = Self::extend_efficient(
                    dest.clone(),
                    size_dest,
                    size_src,
                    instrs,
                    fenv,
                    env,
                    Proxy2,
                )?;
                let instr = And(size_dest, src, dest_dead.clone()).validate()?;
                instrs.push(instr);
                if !live {
                    let dest_real = fenv.place_to_addr_mode(dest, instrs, Proxy1)?;
                    let instr = Move(size_dest, dest_dead, dest_real).validate()?;
                    instrs.push(instr);
                }
                Ok(())
            }
            BudBinop::BitOr => {
                let (src, _) = Self::extend_efficient(
                    src, size_src, size_dest, instrs, fenv, env, Proxy1,
                )?;
                let (dest_dead, live) = Self::extend_efficient(
                    dest.clone(),
                    size_dest,
                    size_src,
                    instrs,
                    fenv,
                    env,
                    Proxy2,
                )?;
                let instr = Or(size_dest, src, dest_dead.clone()).validate()?;
                instrs.push(instr);
                if !live {
                    let dest_real = fenv.place_to_addr_mode(dest, instrs, Proxy1)?;
                    let instr = Move(size_dest, dest_dead, dest_real).validate()?;
                    instrs.push(instr);
                }
                Ok(())
            }
            BudBinop::BitXor => {
                // Eor needs to come from a dreg
                let (src_dreg, _) = fenv.place_to_dreg(src, instrs, env, Proxy1)?;
                Self::extend(src_dreg, size_src, size_dest, instrs)?;
                let (dest_dead, live) = Self::extend_efficient(
                    dest.clone(),
                    size_dest,
                    size_src,
                    instrs,
                    fenv,
                    env,
                    Proxy2,
                )?;
                let instr = Eor(size_dest, src_dreg, dest_dead.clone()).validate()?;
                instrs.push(instr);
                if !live {
                    let dest_real = fenv.place_to_addr_mode(dest, instrs, Proxy1)?;
                    let instr = Move(size_dest, dest_dead, dest_real).validate()?;
                    instrs.push(instr);
                }
                Ok(())
            }
            BudBinop::NotEq => todo!(),
            BudBinop::Greater => todo!(),
            BudBinop::GrtrEq => todo!(),
            BudBinop::Less => todo!(),
            BudBinop::LessEq => todo!(),
        }
    }
}

#[derive(Clone)]
pub struct CompiledFunction {
    pub signature: Signature,
    pub instructions: Vec<Instruction<Valid>>,
}
