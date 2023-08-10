//! Compilation from the "data" `InterInstr`s to `ValidInstruction`s
//!
//! Author:     Brian Smith
//! Year:       2023

use std::ops::Range;

use crate::{m68k::*, c_err, error::*};

use log::*;

use super::condition::*;

use ADReg::*;
use DataSize::*;
use Instruction::*;
use NumOrLbl::Num;
use Proxy::*;

pub fn compile_neg_iinstr(
    src: Place,
    range: Range<usize>,
    instrs: &mut Vec<ValidInstruction>,
    fenv: &mut FunctionEnvironment,
    env: &Environment,
) -> Result<(), BudErr> {
    let tt = src.get_type();
    if tt.is_array() || tt.is_struct() {
        return c_err!("Cannot negate value of type {}", tt);
    }
    let size = tt.get_data_size(env, Some(&range))?.unwrap();
    let src = fenv.place_to_addr_mode(src, range, instrs, Proxy1)?;
    let instr = Neg(size, src).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_bnot_iinstr(
    src: Place,
    range: Range<usize>,
    instrs: &mut Vec<ValidInstruction>,
    fenv: &mut FunctionEnvironment,
    env: &Environment,
) -> Result<(), BudErr> {
    let tt = src.get_type();
    if tt.is_array() || tt.is_struct() {
        return c_err!("Cannot find NOT of value of type {}", tt);
    }
    let size = tt.get_data_size(env, Some(&range))?.unwrap();
    let src = fenv.place_to_addr_mode(src, range, instrs, Proxy1)?;
    let instr = Tst(size, src.clone()).validate()?;
    instrs.push(instr);
    cc_to_ne(size, src, instrs, fenv)?;
    Ok(())
}

pub fn compile_move_iinstr(
    src: Place,
    dest: Place,
    range: Range<usize>,
    instrs: &mut Vec<ValidInstruction>,
    fenv: &mut FunctionEnvironment,
    env: &Environment,
) -> Result<(), BudErr> {
    // Move using dest type
    let src_tt = src.get_type();
    let dest_tt = dest.get_type();
    let src = fenv.place_to_addr_mode(src, range.to_owned(), instrs, Proxy1)?;
    let dest = fenv.place_to_addr_mode(dest, range.to_owned(), instrs, Proxy2)?;
    let src_size = src_tt.get_size(env, Some(&range))?;
    let src_data_size = src_tt.get_data_size(env, Some(&range))?;
    let mut dest_size = dest_tt.get_size(env, Some(&range))?;
    let dest_data_size = dest_tt.get_data_size(env, Some(&range))?;
    if src_size < dest_size {
        if dest_tt.is_magic(env) || dest_tt.is_pointer() {
            let (src, _) = extend_efficient(
                src,
                src_data_size.unwrap(),
                dest_data_size.unwrap(),
                instrs,
                fenv,
                Proxy1,
            )?;
            let instr = Move(dest_data_size.unwrap(), src, dest).validate()?;
            instrs.push(instr);
            Ok(())
        } else {
            c_err!("Cannot move from value of type {} to value of type {}. {} has size {}, which is smaller than {} of size {}", src_tt, dest_tt, src_tt, src_size, dest_tt, dest_size)
        }
    } else if dest_tt.is_array() || dest_tt.is_struct() {
        // algorithm to copy bytes
        // Arrays and structs should not be stored in registers
        match src {
            AddrMode::D(dreg) => {
                return c_err!("Arrays and structs should not be stored in data registers, but value of type {} was stored in {}", src_tt, dreg);
            }
            AddrMode::A(areg) => {
                return c_err!("Arrays and structs should not be stored in addr registers, but value of type {} was stored in {}", src_tt, areg);
            }
            AddrMode::AIndInc(_) => {
                panic!("This AddrMode came from a place, and places can't be AIndInc")
            }
            AddrMode::AIndDec(_) => {
                panic!("This AddrMode came from a place, and places can't be AIndDec")
            }
            AddrMode::Imm(_) => panic!("This AddrMode came from a place, and places can't be Imm"),
            _ => {}
        }
        let src_areg: AReg = Proxy1.into();
        let instr = Lea(src, src_areg).validate()?;
        instrs.push(instr);
        match dest {
            AddrMode::D(dreg) => {
                return c_err!("Arrays and structs should not be stored in data registers, but value of type {} was stored in {}", src_tt, dreg);
            }
            AddrMode::A(areg) => {
                return c_err!("Arrays and structs should not be stored in addr registers, but value of type {} was stored in {}", src_tt, areg);
            }
            AddrMode::AIndInc(_) => {
                panic!("This AddrMode came from a place, and places can't be AIndInc")
            }
            AddrMode::AIndDec(_) => {
                panic!("This AddrMode came from a place, and places can't be AIndDec")
            }
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
        let instr = Move(LWord, (dest_size as Imm).into(), index.into()).validate()?;
        instrs.push(instr);

        let src = AddrMode::AIndIdxDisp(Num(0), src_areg, D(index));
        let dest = AddrMode::AIndIdxDisp(Num(0), dest_areg, D(index));

        // Loop
        let start_label = fenv.get_new_label();
        let mut instr = vec![
            Lbl(start_label).validate()?,
            Move(Word, src, dest).validate()?,
            Sub(LWord, 2.into(), index.into()).validate()?,
            Blt(start_label).validate()?,
        ];
        instrs.append(&mut instr);
        Ok(())
    } else {
        let instr = Move(dest_data_size.unwrap(), src, dest).validate()?;
        instrs.push(instr);
        Ok(())
    }
}

pub fn compile_movi_iinstr(
    imm: Imm,
    to: Place,
    range: Range<usize>,
    instrs: &mut Vec<ValidInstruction>,
    fenv: &mut FunctionEnvironment,
    env: &Environment,
) -> Result<(), BudErr> {
    // Assigning to pointers will have been warned before this point
    if to.is_array() || to.is_struct() || to.is_void() {
        return c_err!(
            "Cannot assign immediate value {} to value of type {}",
            imm,
            to.get_type()
        );
    }
    let size = to.get_data_size(env, Some(&range))?.unwrap();
    let to = fenv.place_to_addr_mode(to, range, instrs, Proxy1)?;
    let instr = Move(size, imm.into(), to).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_lea_iinstr(
    atemp: ATemp,
    dtemp: Option<DTemp>,
    off: Imm,
    to: ATemp,
    range: Range<usize>,
    instrs: &mut Vec<ValidInstruction>,
    fenv: &mut FunctionEnvironment,
) -> Result<(), BudErr> {
    let areg = fenv.atemp_as_areg(atemp, instrs, Proxy1)?.0;
    let dreg = fenv.opt_dtemp_as_opt_dreg(dtemp, instrs, Proxy1)?
            .map(|dreg| dreg.0);
    let from = (off, areg, dreg).into();
    let (to_areg, live) = fenv.atemp_as_areg(to, instrs, Proxy2)?;
    let instr = Lea(from, to_areg).validate()?;
    instrs.push(instr);
    if !live {
        let to = fenv.place_to_addr_mode(Place::ATemp(to), range, instrs, Proxy2)?;
        let instr = Move(LWord, to_areg.into(), to).validate()?;
        instrs.push(instr);
    }
    Ok(())
}

pub fn compile_incsp_iinstr(
    inc: i32,
    _range: Range<usize>,
    instrs: &mut Vec<ValidInstruction>,
) -> Result<(), BudErr> {
    let from = AddrMode::AIndDisp(inc.into(), AReg::SP);
    let to = AReg::SP.into();
    let instr = Lea(from, to).validate()?;
    instrs.push(instr);
    Ok(())
}
