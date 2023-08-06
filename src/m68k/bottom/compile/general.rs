//! Compile `InterInstr`s to `ValidInstruction`s
//! 
//! Author:     Brian Smith
//! Year:       2023

use std::collections::HashMap;

use crate::{m68k::*, error::*};

use bottom::{instruction::*, fenv::{FunctionEnvironment, Proxy}};
use log::debug;
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
    name: &str,
    fienv: FunctionInterEnvironment,
    env: &Environment,
) -> Result<(Vec<ValidInstruction>, FunctionEnvironment), BudErr> {
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
    )?;
    for iinstr in iinstrs {
        compile_iinstr(iinstr, &mut instrs, &mut fenv, env)?;
    }
    debug!("Instructions for function {}", name);
    for instr in &instrs {
        debug!("\t{:?}", instr);
    }
    fenv.print_stack_frame()?;
    Ok((instrs, fenv))
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
            InterInstr::Binop(left, _, right, _) | InterInstr::Move(left, right, _) => {
                update_temp_usages_by_place(&mut dtemp_usages, &mut atemp_usages, left);
                update_temp_usages_by_place(&mut dtemp_usages, &mut atemp_usages, right);
            }

            InterInstr::Binopi(_, _, place, _)
            | InterInstr::Neg(place, _)
            | InterInstr::Bnot(place, _)
            | InterInstr::MoVA(_, place, _)
            | InterInstr::Movi(_, place, _)
            | InterInstr::Movs(_, place, _)
            | InterInstr::Push(place, _)
            | InterInstr::Tst(place, _) => {
                update_temp_usages_by_place(&mut dtemp_usages, &mut atemp_usages, place);
            }

            InterInstr::Chki(_, dtemp, _) => update_temp_usages(&mut dtemp_usages, *dtemp),
            InterInstr::Lea(atemp1, dtemp, _, atemp2, _) => {
                update_temp_usages(&mut atemp_usages, *atemp1);
                if let Some(dtemp) = dtemp {
                    update_temp_usages(&mut dtemp_usages, *dtemp);
                }
                update_temp_usages(&mut atemp_usages, *atemp2);
            }
            InterInstr::Pea(atemp, dtemp, _, _) => {
                update_temp_usages(&mut atemp_usages, *atemp);
                if let Some(dtemp) = dtemp {
                    update_temp_usages(&mut dtemp_usages, *dtemp);
                }
            }
            InterInstr::Chk(atemp, dtemp1, _, dtemp2, _) => {
                update_temp_usages(&mut atemp_usages, *atemp);
                if let Some(dtemp1) = dtemp1 {
                    update_temp_usages(&mut dtemp_usages, *dtemp1);
                }
                update_temp_usages(&mut dtemp_usages, *dtemp2);
            }

            InterInstr::Call(_, _, _) => {}
            InterInstr::SMarker(_, _) => {}
            InterInstr::Lbl(_, _) => {}
            InterInstr::Goto(_, _) => {}
            InterInstr::Rts(_) => {}
            InterInstr::Grs(_, _) => {}
            InterInstr::Save(_, _, _) => {}
            InterInstr::PuVA(_, _) => {}
            InterInstr::Pusi(_, _, _) => {}
            InterInstr::Puss(_, _) => {}
            InterInstr::Tsti(_, _) => {}
            InterInstr::Bcc(_, _) => {}
            InterInstr::Bcs(_, _) => {}
            InterInstr::Beq(_, _) => {}
            InterInstr::Bge(_, _) => {}
            InterInstr::Bgt(_, _) => {}
            InterInstr::Ble(_, _) => {}
            InterInstr::Blt(_, _) => {}
            InterInstr::Bmi(_, _) => {}
            InterInstr::Bne(_, _) => {}
            InterInstr::Bpl(_, _) => {}
            InterInstr::Bra(_, _) => {}
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
        InterInstr::Binop(src, b, dest, range) => compile_binop_iinstr(src, b, dest, range, instrs, fenv, env),
        InterInstr::Binopi(imm, b, dest, range) => compile_binopi_iinstr(imm, b, dest, range, instrs, fenv, env),
        InterInstr::Neg(place, range) => compile_neg_iinstr(place, range, instrs, fenv, env),
        InterInstr::Bnot(place, range) => compile_bnot_iinstr(place, range, instrs, fenv, env),
        InterInstr::Move(from, to, range) => compile_move_iinstr(from, to, range, instrs, fenv, env),
        InterInstr::MoVA(name, to, range) => compile_mova_iinstr(name, to, range, instrs, fenv, env),
        InterInstr::Movi(imm, to, range) => compile_movi_iinstr(imm, to, range, instrs, fenv, env),
        InterInstr::Movs(string_lbl, to, range) => compile_movs_iinstr(string_lbl, to, range, instrs, fenv),
        InterInstr::Lea(atemp, dtemp, off, to, range) => compile_lea_iinstr(atemp, dtemp, off, to, range, instrs, fenv),
        InterInstr::Push(from, range) => compile_push_iinstr(from, range, instrs, fenv, env),
        InterInstr::PuVA(name, range) => compile_puva_iinstr(name, range, instrs, fenv),
        InterInstr::Pusi(imm, size, range) => compile_pusi_iinstr(imm, size, range, instrs),
        InterInstr::Puss(string_lbl, range) => compile_puss_iinstr(string_lbl, range, instrs),
        InterInstr::Pea(atemp, dtemp, off, range) => compile_pea_iinstr(atemp, dtemp, off, range, instrs, fenv),
        InterInstr::Chk(atemp, dtemp, off, to, range) => compile_chk_iinstr(atemp, dtemp, off, to, range, instrs, fenv),
        InterInstr::Chki(len, to, range) => compile_chki_iinstr(len as Imm, to, range, instrs, fenv),
        InterInstr::SMarker(lbl, _) => { compile_smarker_iinstr(lbl, fenv); Ok(()) },
        InterInstr::Grs(rs_lbl, _) => { compile_grs_iinstr(rs_lbl, fenv); Ok(()) },
        InterInstr::Save(rs_lbl, temps, range) => compile_save_iinstr(rs_lbl, &temps, range, instrs, fenv),
        InterInstr::Call(name, smarker_lbk, range) => compile_call_iinstr(name, smarker_lbk, range, instrs, fenv),
        InterInstr::Lbl(lbl, range) => compile_lbl_iinstr(lbl, range, instrs),
        InterInstr::Goto(lbl, range) => compile_goto_iinstr(lbl, range, instrs),
        InterInstr::Rts(range) => compile_rts_iinstr(range, instrs),
        InterInstr::Tst(from, range) => compile_tst_iinstr(from, range, instrs, fenv, env),
        InterInstr::Tsti(from, range) => compile_tsti_iinstr(from, range, instrs),
        InterInstr::Bcc(lbl, range) => compile_bcc_iinstr(lbl, range, instrs),
        InterInstr::Bcs(lbl, range) => compile_bcs_iinstr(lbl, range, instrs),
        InterInstr::Beq(lbl, range) => compile_beq_iinstr(lbl, range, instrs),
        InterInstr::Bge(lbl, range) => compile_bge_iinstr(lbl, range, instrs),
        InterInstr::Bgt(lbl, range) => compile_bgt_iinstr(lbl, range, instrs),
        InterInstr::Ble(lbl, range) => compile_ble_iinstr(lbl, range, instrs),
        InterInstr::Blt(lbl, range) => compile_blt_iinstr(lbl, range, instrs),
        InterInstr::Bmi(lbl, range) => compile_bmi_iinstr(lbl, range, instrs),
        InterInstr::Bne(lbl, range) => compile_bne_iinstr(lbl, range, instrs),
        InterInstr::Bpl(lbl, range) => compile_bpl_iinstr(lbl, range, instrs),
        InterInstr::Bra(lbl, range) => compile_bra_iinstr(lbl, range, instrs),
    }
}

#[derive(Clone)]
pub struct CompiledFunction {
    pub signature: Signature,
    pub lit_strings: Vec<(usize, String)>,
    pub instructions: Vec<ValidInstruction>,
}
