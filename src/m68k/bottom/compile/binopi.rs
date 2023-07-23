//! Compilation from `InterInstr::Binopi` to `ValidInstruction`
//! 
//! Author:     Brian Smith
//! Year:       2023

use std::ops::Range;

use crate::{m68k::*, bud::BudBinop, error::*, c_err};
use super::{super::fenv::Proxy, condition::*};

use log::*;

use Proxy::*;
use Instruction::*;

pub fn compile_binopi_iinstr(src: Imm, b: BudBinop, dest: Place, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), BudErr> {
    if dest.is_array() || dest.is_struct() {
        return c_err!(
            "Cannot do binop {} on immediate {} and type {}",
            b,
            src,
            dest.get_type()
        );
    }
    let size = dest.get_data_size(env).unwrap();
    match b {
        BudBinop::Plus => compile_plus(src, dest, size, range, instrs, fenv, env),
        BudBinop::Minus => compile_minus(src, dest, size, range, instrs, fenv, env),
        BudBinop::Times => compile_times(src, dest, size, range, instrs, fenv, env),
        BudBinop::Div => compile_div(src, dest, size, range, instrs, fenv, env),
        BudBinop::And => compile_and(src, dest, size, range, instrs, fenv),
        BudBinop::Or => compile_or(src, dest, size, range, instrs, fenv),
        BudBinop::BitAnd => compile_bitand(src, dest, size, range, instrs, fenv),
        BudBinop::BitOr => compile_bitor(src, dest, size, range, instrs, fenv),
        BudBinop::BitXor => compile_bitxor(src, dest, size, range, instrs, fenv),
        BudBinop::Equal => compile_equal(src, dest, size, range, instrs, fenv),
        BudBinop::NotEq => compile_noteq(src, dest, size, range, instrs, fenv),
        BudBinop::Greater => compile_greater(src, dest, size, range, instrs, fenv),
        BudBinop::GrtrEq => compile_grtreq(src, dest, size, range, instrs, fenv),
        BudBinop::Less => compile_less(src, dest, size, range, instrs, fenv),
        BudBinop::LessEq => compile_lesseq(src, dest, size, range, instrs, fenv),
    }
}

fn compile_plus(src: Imm, dest: Place, size: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), BudErr> {
    if fenv.place_is_dreg(&dest) {
        let (dest, _) = fenv.place_to_dreg(dest, range, instrs, env, Proxy1)?;
        let dest = AddrMode::D(dest);
        let instr = Add(size, src.into(), dest).validate()?;
        instrs.push(instr);
    } else {
        let dreg = imm_to_dreg(src, instrs, Proxy1)?;
        let src = dreg.into();
        let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy2)?;
        let instr = Add(size, src, dest).validate()?;
        instrs.push(instr);
    }
    Ok(())
}

fn compile_minus(src: Imm, dest: Place, size: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), BudErr> {
    if fenv.place_is_dreg(&dest) {
        let (dest, _) = fenv.place_to_dreg(dest, range, instrs, env, Proxy1)?;
        let dest = AddrMode::D(dest);
        let instr = Sub(size, src.into(), dest).validate()?;
        instrs.push(instr);
    } else {
        let dreg = imm_to_dreg(src, instrs, Proxy1)?;
        let src = dreg.into();
        let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy2)?;
        let instr = Sub(size, src, dest).validate()?;
        instrs.push(instr);
    }
    Ok(())
}

fn compile_times(src: Imm, dest: Place, size: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), BudErr> {
    if size == DataSize::LWord {
        warn!("Multiplication on 32 bit integers will cast to 16 bit integers");
    }
    let (dest_dreg, live) = fenv.place_to_dreg(dest.clone(), range.to_owned(), instrs, env, Proxy1)?;
    extend(dest_dreg, size, DataSize::Word, instrs)?;
    let instr = Muls(src.into(), dest_dreg).validate()?;
    instrs.push(instr);
    if !live {
        extend(dest_dreg, DataSize::Word, size, instrs)?;
        let dest_dead = dest_dreg.into();
        let dest_live = fenv.place_to_addr_mode(dest, range, instrs, Proxy2)?;
        let instr = Move(size, dest_dead, dest_live).validate()?;
        instrs.push(instr);
    }
    Ok(())
}

fn compile_div(src: Imm, dest: Place, size: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), BudErr> {
    if size == DataSize::LWord {
        info!("If the quotient is stored in a 32 bit location, the top word is the remainder");
    }
    // Div needs to go to a dreg
    let (dest_dreg, live) =
    fenv.place_to_dreg(dest.clone(), range.to_owned(), instrs, env, Proxy2)?;
    // Dest needs to be 32 bits
    extend(dest_dreg, size, DataSize::LWord, instrs)?;
    let instr = Divs(src.into(), dest_dreg).validate()?;
    instrs.push(instr);
    if !live {
        let dest_real = fenv.place_to_addr_mode(dest, range, instrs, Proxy1)?;
        let dest_dead = AddrMode::D(dest_dreg);
        // The top word in this dreg is the remainder. If the destination is not 32 bits, the remainder is lost
        let instr = Move(size, dest_dead, dest_real).validate()?;
        instrs.push(instr);
    }
    Ok(())
}

fn compile_and(src: Imm, dest: Place, size: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    // `<exp> && true` is just <exp>
    // `<exp> && false` is just 0
    if src == 0 {
        let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy1)?;
        let instr = Move(size, 0.into(), dest).validate()?;
        instrs.push(instr);
    }
    Ok(())
}

fn compile_or(src: Imm, dest: Place, size: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    // `<exp> || true` is just 1
    // `<exp> || false` is just <exp>
    if src != 0 {
        let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy1)?;
        let instr = Move(size, 1.into(), dest).validate()?;
        instrs.push(instr);
    }
    Ok(())
}

fn compile_bitand(src: Imm, dest: Place, size: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy1)?;
    let instr = And(size, src.into(), dest).validate()?;
    instrs.push(instr);
    Ok(())
}

fn compile_bitor(src: Imm, dest: Place, size: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy1)?;
    let instr = Or(size, src.into(), dest).validate()?;
    instrs.push(instr);
    Ok(())
}

fn compile_bitxor(src: Imm, dest: Place, size: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let src = imm_to_dreg(src, instrs, Proxy1)?;
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy2)?;
    // Technically this could fail if dest is PCIndDisp or PCIndIdxDisp, but I will probably
    // remove those anyway
    let instr = Eor(size, src, dest).validate()?;
    instrs.push(instr);
    Ok(())
}

fn compile_equal(src: Imm, dest: Place, size: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy1)?;
    cmpi(src, dest.clone(), size, instrs, fenv)?;
    cc_to_eq(size, dest, instrs, fenv)?;
    Ok(())
}

fn compile_noteq(src: Imm, dest: Place, size: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy1)?;
    cmpi(src, dest.clone(), size, instrs, fenv)?;
    cc_to_ne(size, dest, instrs, fenv)?;
    Ok(())
}

fn compile_greater(src: Imm, dest: Place, size: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy1)?;
    cmpi(src, dest.clone(), size, instrs, fenv)?;
    cc_to_gt(size, dest, instrs, fenv)?;
    Ok(())
}

fn compile_grtreq(src: Imm, dest: Place, size: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy1)?;
    cmpi(src, dest.clone(), size, instrs, fenv)?;
    cc_to_ge(size, dest, instrs, fenv)?;
    Ok(())
}

fn compile_less(src: Imm, dest: Place, size: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy1)?;
    cmpi(src, dest.clone(), size, instrs, fenv)?;
    cc_to_lt(size, dest, instrs, fenv)?;
    Ok(())
}

fn compile_lesseq(src: Imm, dest: Place, size: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy1)?;
    cmpi(src, dest.clone(), size, instrs, fenv)?;
    cc_to_le(size, dest, instrs, fenv)?;
    Ok(())
}
