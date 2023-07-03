use std::collections::HashMap;

use crate::m68k::*;

use super::{super::{instruction::*, fenv::{FunctionEnvironment, Proxy}}, binop::compile_binop_iinstr, binopi::compile_binopi_iinstr, condition::cc_to_ne};
use Instruction::*;
use DReg::*;
use AReg::*;
use ADReg::*;
use DataSize::*;
use NumOrLbl::Num;
use Proxy::*;
use log::*;

pub fn get_instrs(
    iinstrs: Vec<InterInstr>,
    fienv: FunctionInterEnvironment,
    env: &Environment,
) -> Result<Vec<ValidInstruction>, String> {
    let mut instrs = Vec::new();
    let (dtemp_map, atemp_map) = get_temp_maps(&iinstrs);
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
        compile_iinstr(iinstr, &mut instrs, &mut fenv, env)?;
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
            update_temp_usages(atemp_usages, *atemp);
        }
        Place::DTemp(dtemp, _) => {
            update_temp_usages(dtemp_usages, *dtemp);
        }
        Place::Ref(atemp, dtemp, _, _) => {
            update_temp_usages(atemp_usages, *atemp);
            if let Some(dtemp) = dtemp {
                update_temp_usages(dtemp_usages, *dtemp);
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
                update_temp_usages_by_place(&mut dtemp_usages, &mut atemp_usages, left);
                update_temp_usages_by_place(&mut dtemp_usages, &mut atemp_usages, right);
            }

            InterInstr::Binopi(_, _, place)
            | InterInstr::Neg(place)
            | InterInstr::Bnot(place)
            | InterInstr::MoVA(_, place)
            | InterInstr::Movi(_, place)
            | InterInstr::Movs(_, place)
            | InterInstr::Push(place, _)
            | InterInstr::Tst(place) => {
                update_temp_usages_by_place(&mut dtemp_usages, &mut atemp_usages, place);
            }

            InterInstr::Chki(_, dtemp) => update_temp_usages(&mut dtemp_usages, *dtemp),
            InterInstr::Lea(atemp1, dtemp, _, atemp2) => {
                update_temp_usages(&mut atemp_usages, *atemp1);
                if let Some(dtemp) = dtemp {
                    update_temp_usages(&mut dtemp_usages, *dtemp);
                }
                update_temp_usages(&mut atemp_usages, *atemp2);
            }
            InterInstr::Pea(atemp, dtemp, _) => {
                update_temp_usages(&mut atemp_usages, *atemp);
                if let Some(dtemp) = dtemp {
                    update_temp_usages(&mut dtemp_usages, *dtemp);
                }
            }
            InterInstr::Chk(atemp, dtemp1, _, dtemp2) => {
                update_temp_usages(&mut atemp_usages, *atemp);
                if let Some(dtemp1) = dtemp1 {
                    update_temp_usages(&mut dtemp_usages, *dtemp1);
                }
                update_temp_usages(&mut dtemp_usages, *dtemp2);
            }

            InterInstr::Call(_, _) => {}
            InterInstr::SMarker(_) => {}
            InterInstr::Lbl(_) => {}
            InterInstr::Goto(_) => {}
            InterInstr::Rts => {}
            InterInstr::Grs => {}
            InterInstr::Save => {}
            InterInstr::PuVA(_) => {}
            InterInstr::Pusi(_, _) => {}
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
pub fn extend(
    dreg: DReg,
    from: DataSize,
    to: DataSize,
    instrs: &mut Vec<ValidInstruction>,
) -> Result<(), String> {
    if from >= to {
        return Ok(());
    }
    if from == Byte {
        let instr = ExtW(dreg).validate()?;
        instrs.push(instr);
    }
    if to == LWord {
        let instr = ExtL(dreg).validate()?;
        instrs.push(instr);
    }
    Ok(())
}
/// The returned bool indicates if the AddrMode is live. If no extension occurs, then it will
/// always be live.
///
/// If `from >= to`, then no extension needs to occur, so addr_mode is returned.
///
/// Otherwise, the place is converted to DReg using the given proxy.
pub fn extend_efficient(
    addr_mode: AddrMode,
    from: DataSize,
    to: DataSize,
    instrs: &mut Vec<ValidInstruction>,
    fenv: &mut FunctionEnvironment,
    n: Proxy,
) -> Result<(AddrMode, bool), String> {
    if from >= to {
        Ok((addr_mode, true))
    } else {
        let (dreg, live) = fenv.addr_mode_to_dreg(addr_mode, from, instrs, n)?;
        extend(dreg, from, to, instrs)?;
        Ok((dreg.into(), live))
    }
}
pub fn compile_iinstr(
    iinstr: InterInstr,
    instrs: &mut Vec<ValidInstruction>,
    fenv: &mut FunctionEnvironment,
    env: &Environment,
) -> Result<(), String> {
    match iinstr {
        InterInstr::Binop(src, b, dest) => compile_binop_iinstr(src, b, dest, instrs, fenv, env),
        InterInstr::Binopi(imm, b, dest) => compile_binopi_iinstr(imm, b, dest, instrs, fenv, env),
        InterInstr::Neg(place) => compile_neg_iinstr(place, instrs, fenv, env),
        InterInstr::Bnot(place) => compile_bnot_iinstr(place, instrs, fenv, env),
        InterInstr::Move(from, to) => compile_move_iinstr(from, to, instrs, fenv, env),
        InterInstr::MoVA(_, _) => todo!(),
        InterInstr::Movi(_, _) => todo!(),
        InterInstr::Movs(_, _) => todo!(),
        InterInstr::Lea(_, _, _, _) => todo!(),
        InterInstr::Push(_, _) => todo!(),
        InterInstr::PuVA(_) => todo!(),
        InterInstr::Pusi(_, _) => todo!(),
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

#[derive(Clone)]
pub struct CompiledFunction {
    pub signature: Signature,
    pub instructions: Vec<ValidInstruction>,
}

fn compile_neg_iinstr(src: Place, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
    let tt = src.get_type();
    if tt.is_array() || tt.is_struct() {
        return Err(format!("Cannot negate value of type {}", tt));
    }
    let size = tt.get_data_size(env).unwrap();
    let src = fenv.place_to_addr_mode(src, instrs, Proxy1)?;
    let instr = Neg(size, src).validate()?;
    instrs.push(instr);
    Ok(())
}

fn compile_bnot_iinstr(src: Place, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
    let tt = src.get_type();
    if tt.is_array() || tt.is_struct() {
        return Err(format!("Cannot find NOT of value of type {}", tt));
    }
    let size = tt.get_data_size(env).unwrap();
    let src = fenv.place_to_addr_mode(src, instrs, Proxy1)?;
    let instr = Tst(size, src.clone()).validate()?;
    instrs.push(instr);
    cc_to_ne(size, src, instrs, fenv)?;
    Ok(())
}

fn compile_move_iinstr(src: Place, dest: Place, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
    // Move using dest type
    let src_tt = src.get_type();
    let dest_tt = dest.get_type();
    let src = fenv.place_to_addr_mode(src, instrs, Proxy1)?;
    let dest = fenv.place_to_addr_mode(dest, instrs, Proxy2)?;
    let src_size = src_tt.get_size(env);
    let src_data_size = src_tt.get_data_size(env);
    let mut dest_size = dest_tt.get_size(env);
    let dest_data_size = dest_tt.get_data_size(env);
    if src_size < dest_size {
        if dest_tt.is_magic(env) || dest_tt.is_pointer() {
            let (src, _) = extend_efficient(src, src_data_size.unwrap(), dest_data_size.unwrap(), instrs, fenv, Proxy1)?;
            let instr = Move(dest_data_size.unwrap(), src, dest).validate()?;
            instrs.push(instr);
            Ok(())
        } else {
            Err(format!("Cannot move from value of type {} to value of type {}. {} has size {}, which is smaller than {} of size {}", src_tt, dest_tt, src_tt, src_size, dest_tt, dest_size))
        }
    } else if dest_tt.is_array() || dest_tt.is_struct() {
        // algorithm to copy bytes
        // Arrays and structs should not be stored in registers
        match src {
            AddrMode::D(dreg) => {
                return Err(format!("Arrays and structs should not be stored in data registers, but value of type {} was stored in {}", src_tt, dreg));
            }
            AddrMode::A(areg) => {
                return Err(format!("Arrays and structs should not be stored in addr registers, but value of type {} was stored in {}", src_tt, areg));
            }
            AddrMode::AIndInc(_) => panic!("This AddrMode came from a place, and places can't be AIndInc"),
            AddrMode::AIndDec(_) => panic!("This AddrMode came from a place, and places can't be AIndDec"),
            AddrMode::Imm(_) => panic!("This AddrMode came from a place, and places can't be Imm"),
            _ => {}
        }
        let src_areg: AReg = Proxy1.into();
        let instr = Lea(src, src_areg).validate()?;
        instrs.push(instr);
        match dest {
            AddrMode::D(dreg) => {
                return Err(format!("Arrays and structs should not be stored in data registers, but value of type {} was stored in {}", src_tt, dreg));
            }
            AddrMode::A(areg) => {
                return Err(format!("Arrays and structs should not be stored in addr registers, but value of type {} was stored in {}", src_tt, areg));
            }
            AddrMode::AIndInc(_) => panic!("This AddrMode came from a place, and places can't be AIndInc"),
            AddrMode::AIndDec(_) => panic!("This AddrMode came from a place, and places can't be AIndDec"),
            AddrMode::Imm(_) => panic!("This AddrMode came from a place, and places can't be Imm"),
            _ => {}
        }
        let dest_areg: AReg = Proxy2.into();
        let instr = Lea(dest, dest_areg).validate()?;
        instrs.push(instr);
        
        if dest_size % 2 == 1 {
            warn!("Type {} has odd size: {}", dest_tt, dest_size);
            dest_size += 1;
        }

        let index: DReg = Proxy1.into();
        let instr = Move(LWord, (dest_size as i32).into(), index.into()).validate()?;
        instrs.push(instr);

        let src = AddrMode::AIndIdxDisp(Num(0), src_areg, D(index));
        let dest = AddrMode::AIndIdxDisp(Num(0), dest_areg, D(index));

        // Loop
        let start_label = fenv.get_new_label();
        let mut instr = vec![
            Lbl(start_label).validate()?,
            Move(Word, src, dest).validate()?,
            Sub(LWord, 2.into(), index.into()).validate()?,
            Bne(start_label).validate()?,
        ];
        instrs.append(&mut instr);
        Ok(())
    } else {
        let instr = Move(dest_data_size.unwrap(), src, dest).validate()?;
        instrs.push(instr);
        Ok(())
    }
}
