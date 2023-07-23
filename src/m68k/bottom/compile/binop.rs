//! Compilation from `InterInstr::Binop` to `ValidInstruction`
//! 
//! Author:     Brian Smith
//! Year:       2023

use crate::{bud::BudBinop, m68k::*, error::*, c_err};

use super::{super::{instruction::*, fenv::{FunctionEnvironment, Proxy}}, condition::*};
use Instruction::*;
use Proxy::*;
use log::*;
use std::{cmp::max, ops::Range};

pub fn compile_binop_iinstr(src: Place, b: BudBinop, dest: Place, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), BudErr> {
    if src.is_array() || src.is_struct() || dest.is_array() || dest.is_struct() {
        return c_err!(
            "Cannot do binop {} on types {} and {}",
            b,
            src.get_type(),
            dest.get_type()
        );
    }
    let size_src = src.get_data_size(env).unwrap();
    let size_dest = dest.get_data_size(env).unwrap();
    match b {
        BudBinop::Plus => compile_plus(src, dest, size_src, size_dest, range, instrs, fenv, env),
        BudBinop::Minus => compile_minus(src, dest, size_src, size_dest, range, instrs, fenv, env),
        BudBinop::Times => compile_times(src, dest, size_src, size_dest, range, instrs, fenv, env),
        BudBinop::Div => compile_div(src, dest, size_src, size_dest, range, instrs, fenv, env),
        BudBinop::And => compile_and(src, dest, size_src, size_dest, range, instrs, fenv, env),
        BudBinop::Or => compile_or(src, dest, size_src, size_dest, range, instrs, fenv, env),
        BudBinop::BitAnd => compile_bitand(src, dest, size_src, size_dest, range, instrs, fenv, env),
        BudBinop::BitOr => compile_bitor(src, dest, size_src, size_dest, range, instrs, fenv, env),
        BudBinop::BitXor => compile_bitxor(src, dest, size_src, size_dest, range, instrs, fenv, env),
        BudBinop::Equal => compile_equal(src, dest, size_src, size_dest, range, instrs, fenv),
        BudBinop::NotEq => compile_noteq(src, dest, size_src, size_dest, range, instrs, fenv),
        BudBinop::Greater => compile_greater(src, dest, size_src, size_dest, range, instrs, fenv),
        BudBinop::GrtrEq => compile_grtreq(src, dest, size_src, size_dest, range, instrs, fenv),
        BudBinop::Less => compile_less(src, dest, size_src, size_dest, range, instrs, fenv),
        BudBinop::LessEq => compile_lesseq(src, dest, size_src, size_dest, range, instrs, fenv),
    }
}

fn compile_plus(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), BudErr> {
    // Plus needs a dreg, and it needs to be extended anyway
    let (src, _) = fenv.place_to_dreg(src, range.to_owned(), instrs, env, Proxy1)?;
    extend(src, size_src, size_dest, instrs)?;
    let src = AddrMode::D(src);
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy2)?;
    let instr = Add(size_dest, src, dest).validate()?;
    instrs.push(instr);
    Ok(())
}

fn compile_minus(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), BudErr> {
    // Plus needs a dreg, and it needs to be extended anyway
    let (src, _) = fenv.place_to_dreg(src, range.to_owned(), instrs, env, Proxy1)?;
    extend(src, size_src, size_dest, instrs)?;
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy2)?;
    let src = AddrMode::D(src);
    let instr = Sub(size_dest, src, dest).validate()?;
    instrs.push(instr);
    Ok(())
}

fn compile_times(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), BudErr> {
    if size_src == DataSize::LWord || size_dest == DataSize::LWord {
        warn!("Multiplication on 32 bit integers will cast to 16 bit integers")
    }
    let src = fenv.place_to_addr_mode(src, range.to_owned(), instrs, Proxy1)?;
    // Times needs to go to a dreg
    let (dest_dreg, live) =
        fenv.place_to_dreg(dest.clone(), range.to_owned(), instrs, env, Proxy2)?;
    let instr = Muls(src, dest_dreg).validate()?;
    instrs.push(instr);
    if !live {
        let dest_real = fenv.place_to_addr_mode(dest, range, instrs, Proxy1)?;
        let dest_dead = AddrMode::D(dest_dreg);
        let size = DataSize::Word;
        let instr = Move(size, dest_dead, dest_real).validate()?;
        instrs.push(instr);
    }
    Ok(())
}

fn compile_div(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), BudErr> {
    if size_src == DataSize::LWord {
        warn!("32 bit dividends will be cast to 16 bits");
    }
    if size_dest == DataSize::LWord {
        info!("If the quotient is stored in a 32 bit location, the top word is the remainder");
    }
    // Assume divide signed
    let src = fenv.place_to_addr_mode(src, range.to_owned(), instrs, Proxy1)?;
    let (src, _) = extend_efficient(src, size_src, DataSize::Word, instrs, fenv, Proxy1)?;
    // Div needs to go to a dreg
    let (dest_dreg, live) =
        fenv.place_to_dreg(dest.clone(), range.to_owned(), instrs, env, Proxy2)?;
    // Dest needs to be 32 bits
    extend(dest_dreg, size_dest, DataSize::LWord, instrs)?;
    let instr = Divs(src, dest_dreg).validate()?;
    instrs.push(instr);
    if !live {
        let dest_real = fenv.place_to_addr_mode(dest, range.to_owned(), instrs, Proxy1)?;
        let dest_dead = AddrMode::D(dest_dreg);
        // The top word in this dreg is the remainder. If the destination is not 32 bits, the remainder is lost
        let instr = Move(size_dest, dest_dead, dest_real).validate()?;
        instrs.push(instr);
    }
    Ok(())
}

fn compile_and(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, _env: &Environment) -> Result<(), BudErr> {
    let f_lbl = fenv.get_new_label();
    let e_lbl = fenv.get_new_label();
    // We could optimize this by calling place_to_addr_mode for src after dest has been tested
    let src = fenv.place_to_addr_mode(src, range.to_owned(), instrs, Proxy1)?;
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy2)?;
    let mut instr = vec![
        Tst(size_dest, dest.clone()).validate()?,
        Beq(f_lbl).validate()?,
        Tst(size_src, src).validate()?,
        Beq(f_lbl).validate()?,
        Move(size_dest, AddrMode::Imm(NumOrLbl::Num(1)), dest.clone())
            .validate()?,
        Bra(e_lbl).validate()?,
        Lbl(f_lbl).validate()?,
        Move(size_dest, AddrMode::Imm(NumOrLbl::Num(0)), dest).validate()?,
        Lbl(e_lbl).validate()?,
    ];
    instrs.append(&mut instr);
    Ok(())
}

fn compile_or(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, _env: &Environment) -> Result<(), BudErr> {
    let t_lbl = fenv.get_new_label();
    let e_lbl = fenv.get_new_label();
    // We could optimize this by calling place_to_addr_mode for src after dest has been tested
    let src = fenv.place_to_addr_mode(src, range.to_owned(), instrs, Proxy1)?;
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy2)?;
    let mut instr = vec![
        Tst(size_dest, dest.clone()).validate()?,
        Bne(t_lbl).validate()?,
        Tst(size_src, src).validate()?,
        Bne(t_lbl).validate()?,
        Move(size_dest, AddrMode::Imm(NumOrLbl::Num(0)), dest.clone())
            .validate()?,
        Bra(e_lbl).validate()?,
        Lbl(t_lbl).validate()?,
        Move(size_dest, AddrMode::Imm(NumOrLbl::Num(1)), dest).validate()?,
    ];
    instrs.append(&mut instr);
    Ok(())
}

fn compile_bitand(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, _env: &Environment) -> Result<(), BudErr> {
    let src = fenv.place_to_addr_mode(src, range.to_owned(), instrs, Proxy1)?;
    let dest_live = fenv.place_to_addr_mode(dest.clone(), range.to_owned(), instrs, Proxy2)?;
    let (src, _) = extend_efficient(src, size_src, size_dest, instrs, fenv, Proxy1)?;
    let (dest_dead, live) = extend_efficient(
        dest_live,
        size_dest,
        size_src,
        instrs,
        fenv,
        Proxy2,
    )?;
    let instr = And(size_dest, src, dest_dead.clone()).validate()?;
    instrs.push(instr);
    if !live {
        let dest_live = fenv.place_to_addr_mode(dest, range, instrs, Proxy2)?;
        let instr = Move(size_dest, dest_dead, dest_live).validate()?;
        instrs.push(instr);
    }
    Ok(())
}

fn compile_bitor(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, _env: &Environment) -> Result<(), BudErr> {
    let src = fenv.place_to_addr_mode(src, range.to_owned(), instrs, Proxy1)?;
    let dest_live = fenv.place_to_addr_mode(dest.clone(), range.to_owned(), instrs, Proxy2)?;
    let (src, _) = extend_efficient(src, size_src, size_dest, instrs, fenv, Proxy1)?;
    let (dest_dead, live) = extend_efficient(
        dest_live,
        size_dest,
        size_src,
        instrs,
        fenv,
        Proxy2,
    )?;
    let instr = Or(size_dest, src, dest_dead.clone()).validate()?;
    instrs.push(instr);
    if !live {
        let dest_live = fenv.place_to_addr_mode(dest, range.to_owned(), instrs, Proxy2)?;
        let instr = Move(size_dest, dest_dead, dest_live).validate()?;
        instrs.push(instr);
    }
    Ok(())
}

fn compile_bitxor(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), BudErr> {
    // Eor needs to come from a dreg
    let (src_dreg, _) = fenv.place_to_dreg(src, range.to_owned(), instrs, env, Proxy1)?;
    extend(src_dreg, size_src, size_dest, instrs)?;
    let dest_live = fenv.place_to_addr_mode(dest.clone(), range.to_owned(), instrs, Proxy2)?;
    let (dest_dead, live) = extend_efficient(
        dest_live,
        size_dest,
        size_src,
        instrs,
        fenv,
        Proxy2,
    )?;
    let instr = Eor(size_dest, src_dreg, dest_dead.clone()).validate()?;
    instrs.push(instr);
    if !live {
        let dest_live = fenv.place_to_addr_mode(dest, range, instrs, Proxy2)?;
        let instr = Move(size_dest, dest_dead, dest_live).validate()?;
        instrs.push(instr);
    }
    Ok(())
}

fn compile_equal(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let src = fenv.place_to_addr_mode(src, range.to_owned(), instrs, Proxy1)?;
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy2)?;
    cmp(src, dest.clone(), size_src, size_dest, instrs, fenv)?;
    let size = max(size_src, size_dest);
    cc_to_eq(size, dest, instrs, fenv)?;
    Ok(())
}

fn compile_noteq(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let src = fenv.place_to_addr_mode(src, range.to_owned(), instrs, Proxy1)?;
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy2)?;
    cmp(src, dest.clone(), size_src, size_dest, instrs, fenv)?;
    let size = max(size_src, size_dest);
    cc_to_ne(size, dest, instrs, fenv)?;
    Ok(())
}

fn compile_greater(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let src = fenv.place_to_addr_mode(src, range.to_owned(), instrs, Proxy1)?;
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy2)?;
    cmp(src, dest.clone(), size_src, size_dest, instrs, fenv)?;
    let size = max(size_src, size_dest);
    cc_to_gt(size, dest, instrs, fenv)?;
    Ok(())
}

fn compile_grtreq(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let src = fenv.place_to_addr_mode(src, range.to_owned(), instrs, Proxy1)?;
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy2)?;
    cmp(src, dest.clone(), size_src, size_dest, instrs, fenv)?;
    let size = max(size_src, size_dest);
    cc_to_ge(size, dest, instrs, fenv)?;
    Ok(())
}

fn compile_less(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let src = fenv.place_to_addr_mode(src, range.to_owned(), instrs, Proxy1)?;
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy2)?;
    cmp(src, dest.clone(), size_src, size_dest, instrs, fenv)?;
    let size = max(size_src, size_dest);
    cc_to_lt(size, dest, instrs, fenv)?;
    Ok(())
}

fn compile_lesseq(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, range: Range<usize>, instrs: &mut Vec<ValidInstruction>, fenv: &mut FunctionEnvironment) -> Result<(), BudErr> {
    let src = fenv.place_to_addr_mode(src, range.to_owned(), instrs, Proxy1)?;
    let dest = fenv.place_to_addr_mode(dest, range, instrs, Proxy2)?;
    cmp(src, dest.clone(), size_src, size_dest, instrs, fenv)?;
    let size = max(size_src, size_dest);
    cc_to_le(size, dest, instrs, fenv)?;
    Ok(())
}
