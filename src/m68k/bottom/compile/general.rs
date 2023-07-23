//! Compile `InterInstr`s to `ValidInstruction`s
//! 
//! Author:     Brian Smith
//! Year:       2023

use std::collections::HashMap;

use crate::{m68k::*, error::*};

use bottom::{instruction::*, fenv::{FunctionEnvironment, Proxy}};
use super::*;
use binop::compile_binop_iinstr;
use binopi::compile_binopi_iinstr;
use data::*;
use stack::*;
use condition::*;
use control::*;
use logic::*;

use Instruction::*;
use DReg::*;
use AReg::*;
use DataSize::*;

pub fn get_instrs(
    iinstrs: Vec<InterInstr>,
    fienv: FunctionInterEnvironment,
    env: &Environment,
) -> Result<Vec<ValidInstruction>, BudErr> {
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
            | InterInstr::Push(place)
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
            InterInstr::Grs(_) => {}
            InterInstr::Save(_, _) => {}
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
) -> Result<(), CompilerErr> {
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
) -> Result<(AddrMode, bool), CompilerErr> {
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
) -> Result<(), BudErr> {
    match iinstr {
        InterInstr::Binop(src, b, dest) => compile_binop_iinstr(src, b, dest, instrs, fenv, env),
        InterInstr::Binopi(imm, b, dest) => compile_binopi_iinstr(imm, b, dest, instrs, fenv, env),
        InterInstr::Neg(place) => compile_neg_iinstr(place, instrs, fenv, env),
        InterInstr::Bnot(place) => compile_bnot_iinstr(place, instrs, fenv, env),
        InterInstr::Move(from, to) => compile_move_iinstr(from, to, instrs, fenv, env),
        InterInstr::MoVA(name, to) => compile_mova_iinstr(name, to, instrs, fenv, env),
        InterInstr::Movi(imm, to) => compile_movi_iinstr(imm, to, instrs, fenv, env),
        InterInstr::Movs(string_lbl, to) => compile_movs_iinstr(string_lbl, to, instrs, fenv),
        InterInstr::Lea(atemp, dtemp, off, to) => compile_lea_iinstr(atemp, dtemp, off, to, instrs, fenv),
        InterInstr::Push(from) => compile_push_iinstr(from, instrs, fenv, env),
        InterInstr::PuVA(name) => compile_puva_iinstr(name, instrs, fenv),
        InterInstr::Pusi(imm, size) => compile_pusi_iinstr(imm, size, instrs),
        InterInstr::Puss(string_lbl) => compile_puss_iinstr(string_lbl, instrs),
        InterInstr::Pea(atemp, dtemp, off) => compile_pea_iinstr(atemp, dtemp, off, instrs, fenv),
        InterInstr::Chk(atemp, dtemp, off, to) => compile_chk_iinstr(atemp, dtemp, off, to, instrs, fenv),
        InterInstr::Chki(len, to) => compile_chki_iinstr(len as Imm, to, instrs, fenv),
        InterInstr::SMarker(lbl) => { compile_smarker_iinstr(lbl, fenv); Ok(()) },
        InterInstr::Grs(rs_lbl) => { compile_grs_iinstr(rs_lbl, fenv); Ok(()) },
        InterInstr::Save(rs_lbl, temps) => compile_save_iinstr(rs_lbl, &temps, instrs, fenv),
        InterInstr::Call(name, smarker_lbk) => compile_call_iinstr(name, smarker_lbk, instrs, fenv),
        InterInstr::Lbl(lbl) => compile_lbl_iinstr(lbl, instrs),
        InterInstr::Goto(lbl) => compile_goto_iinstr(lbl, instrs),
        InterInstr::Rts => compile_rts_iinstr(instrs),
        InterInstr::Tst(from) => compile_tst_iinstr(from, instrs, fenv, env),
        InterInstr::Tsti(from) => compile_tsti_iinstr(from, instrs),
        InterInstr::Bcc(lbl) => compile_bcc_iinstr(lbl, instrs),
        InterInstr::Bcs(lbl) => compile_bcs_iinstr(lbl, instrs),
        InterInstr::Beq(lbl) => compile_beq_iinstr(lbl, instrs),
        InterInstr::Bge(lbl) => compile_bge_iinstr(lbl, instrs),
        InterInstr::Bgt(lbl) => compile_bgt_iinstr(lbl, instrs),
        InterInstr::Ble(lbl) => compile_ble_iinstr(lbl, instrs),
        InterInstr::Blt(lbl) => compile_blt_iinstr(lbl, instrs),
        InterInstr::Bmi(lbl) => compile_bmi_iinstr(lbl, instrs),
        InterInstr::Bne(lbl) => compile_bne_iinstr(lbl, instrs),
        InterInstr::Bpl(lbl) => compile_bpl_iinstr(lbl, instrs),
        InterInstr::Bra(lbl) => compile_bra_iinstr(lbl, instrs),
    }
}

#[derive(Clone)]
pub struct CompiledFunction {
    pub signature: Signature,
    pub instructions: Vec<ValidInstruction>,
}
