//! Compilation from the "stack" `InterInstr`s to `ValidInstructions`
//! 
//! Author:     Brian Smith
//! Year:       2023

use std::ops::Range;

use crate::{m68k::*, error::*, c_err};

use log::*;

use Instruction::*;
use Proxy::*;
use DataSize::*;
use AReg::SP;
use Either::*;

pub fn compile_mova_iinstr(name: String, to: Place, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), BudErr> {
    // Only get the destination as AReg if it is already AReg
    let (to_areg, live) = if fenv.place_is_areg(&to) {
        fenv.place_to_areg(to.clone(), range.to_owned(), instrs, env, Proxy1)?
    } else {
        (Proxy1.into(), false)
    };
    let from = fenv.var_as_addr_mode(name)?;
    let instr = Lea(from, to_areg).validate()?;
    instrs.push(instr);
    if !live {
        let to = fenv.place_to_addr_mode(to, range, instrs, Proxy2)?;
        let instr = Move(LWord, to_areg.into(), to).validate()?;
        instrs.push(instr);
    }
    Ok(())
}

pub fn compile_movs_iinstr(string_lbl: usize, to: Place, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let from = NumOrLbl::Str(string_lbl).into();
    let to = fenv.place_to_addr_mode(to, range, instrs, Proxy1)?;
    let instr = Move(LWord, from, to).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_push_iinstr(from: Place, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), BudErr> {
    // `This` if should use loop; `That` if can push in one instruction
    let from_addr_mode: Either<((NumOrLbl, AReg, Option<DReg>), u32), (AddrMode, DataSize)>;
    match from {
        Place::Var(field) => {
            if field.tt.is_array() || field.tt.is_struct() {
                let addr_mode = fenv.var_as_addr_mode(field.name)?;
                if let AddrMode::AIndDisp(off, a) = addr_mode {
                    let mut size = field.tt.get_size(env, Some(&range))?;
                    // This can still happen since size can be 1
                    if size % 2 == 1 {
                        warn!("Type {} has odd size: {}", field.tt, size);
                        size += 1;
                    }
                    from_addr_mode = This(((off, a, None), size));
                } else {
                    return c_err!("`fenv.var_as_addr_mode()` is guaranteed to return `AIndDisp(NumOrLbl, AReg)`, but it returned {:?}", addr_mode);
                }
            } else if field.tt.is_void() {
                return Ok(());
            } else {
                // magic or pointer
                from_addr_mode = That((
                    fenv.var_as_addr_mode(field.name)?,
                    field.tt.get_data_size(env, Some(&range))?.unwrap()
                ));
            }
        }
        Place::Ref(atemp, dtemp, off, tt) => {
            let areg = fenv.atemp_as_areg(atemp, instrs, Proxy1)?.0;
            let dreg = fenv.opt_dtemp_as_opt_dreg(dtemp, instrs, Proxy1)?
                    .map(|dreg| dreg.0);
            if tt.is_array() || tt.is_struct() {
                let mut size = tt.get_size(env, Some(&range))?;
                if size % 2 == 1 {
                    warn!("Type {} has odd size: {}", tt, size);
                    size += 1;
                }
                from_addr_mode = This(((off.into(), areg, dreg), size));
            } else if tt.is_void() {
                return Ok(());
            } else {
                // magic or pointer
                from_addr_mode = That(((Into::<NumOrLbl>::into(off), areg, dreg).into(), tt.get_data_size(env, Some(&range))?.unwrap()));
            }
        }
        Place::ATemp(atemp) => {
            from_addr_mode = That((fenv.atemp_as_areg(atemp, instrs, Proxy1)?.0.into(), LWord));
        }
        Place::DTemp(dtemp, tt) => {
            if tt.is_array() || tt.is_struct() {
                return c_err!("Array or struct, {}, stored in dtemp D{}", tt, dtemp);
            } else if tt.is_void() {
                return Ok(())
            } else {
                from_addr_mode = That((fenv.dtemp_as_dreg(dtemp, instrs, Proxy1)?.0.into(), tt.get_data_size(env, Some(&range))?.unwrap()));
            }
        }
    }
    match from_addr_mode {
        This(((off, areg, dreg), size)) => {
            // Use loop to push this value onto the stack
            let from = (off, areg, dreg).into();
            let index = dreg.unwrap_or(Proxy1.into());
            let start_label = fenv.get_new_label();
            let areg = Proxy1.into();
            let mut instr = vec![
                Lea(from, areg).validate()?,
                Move(LWord, (size as Imm).into(), index.into()).validate()?,
                // Loop
                Lbl(start_label).validate()?,
                Move(Word, areg.into(), AddrMode::get_push()).validate()?,
                Sub(LWord, 2.into(), index.into()).validate()?,
                Blt(start_label).validate()?,
            ];
            instrs.append(&mut instr);
        }
        That((from, size)) => {
            let to = AddrMode::get_push();
            let instr = Move(size, from, to).validate()?;
            instrs.push(instr);
        }
    }
    Ok(())
}

pub fn compile_puva_iinstr(name: String, _range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    // Only get the destination as AReg if it is already AReg
    let from = fenv.var_as_addr_mode(name)?;
    let instr = Pea(from).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_pusi_iinstr(imm: Imm, size: DataSize, _range: Range<usize>, instrs: &mut Vec<ValidInstruction>) -> Result<(), BudErr> {
    let instr = Move(size, imm.into(), AddrMode::get_push()).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_puss_iinstr(string_lbl: usize, _range: Range<usize>, instrs: &mut Vec<ValidInstruction>) -> Result<(), BudErr> {
    let from = NumOrLbl::Str(string_lbl).into();
    let instr = Move(LWord, from, AddrMode::get_push()).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_pea_iinstr(atemp: ATemp, dtemp: Option<DTemp>, off: Imm, _range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let areg = fenv.atemp_as_areg(atemp, instrs, Proxy1)?.0;
    let dreg = fenv.opt_dtemp_as_opt_dreg(dtemp, instrs, Proxy1)?
            .map(|dreg| dreg.0);
    let from = (off, areg, dreg).into();
    let instr = Pea(from).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_save_iinstr(temps: &[ADTemp], _range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    // Assume we save all 4 bytes of all used regs, even if not all the bytes are being used
    // This isn't necessary, but it would make the process way more complicated to think
    // about varying-sized regs
    let size = LWord;
    let adbf = fenv.temps_to_adbitfield(temps)?;

    let rs_size = adbf.rs_size();
    let instr = Lea(AddrMode::AIndDisp((-rs_size).into(), SP), SP).validate()?;
    instrs.push(instr);

    let to = AddrMode::AInd(SP);
    let instr = MoveMRtoM(size, adbf, to).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_load_iinstr(temps: &[ADTemp], _range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let size = LWord;
    let from = AddrMode::get_pop();
    let adbf = fenv.temps_to_adbitfield(temps)?;
    let instr = MoveMMtoR(size, from, adbf).validate()?;
    instrs.push(instr);
    Ok(())
}

pub fn compile_call_iinstr(name: String, _range: Range<usize>, instrs: &mut Vec<ValidInstruction>, _fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let instr = Jsr(name).validate()?;
    instrs.push(instr);
    Ok(())
}
