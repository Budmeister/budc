use crate::{bud::BudBinop, m68k::*};

use super::super::{instruction::*, fenv::{FunctionEnvironment, Proxy}};
use Instruction::*;
use Proxy::*;
use log::*;

impl Function {
    pub fn compile_binop_iinstr(src: Place, b: BudBinop, dest: Place, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
        if src.is_array() || src.is_struct() || dest.is_array() || dest.is_struct() {
            return Err(format!(
                "Cannot do binop {} on types {} and {}",
                b,
                src.get_type(),
                dest.get_type()
            ));
        }
        let size_src = src.get_data_size(env).unwrap();
        let size_dest = dest.get_data_size(env).unwrap();
        match b {
            BudBinop::Plus => Self::compile_plus(src, dest, size_src, size_dest, instrs, fenv, env),
            BudBinop::Minus => Self::compile_minus(src, dest, size_src, size_dest, instrs, fenv, env),
            BudBinop::Times => Self::compile_times(src, dest, size_src, size_dest, instrs, fenv, env),
            BudBinop::Div => Self::compile_div(src, dest, size_src, size_dest, instrs, fenv, env),
            BudBinop::And => Self::compile_and(src, dest, size_src, size_dest, instrs, fenv, env),
            BudBinop::Or => Self::compile_or(src, dest, size_src, size_dest, instrs, fenv, env),
            BudBinop::BitAnd => Self::compile_bitand(src, dest, size_src, size_dest, instrs, fenv, env),
            BudBinop::BitOr => Self::compile_bitor(src, dest, size_src, size_dest, instrs, fenv, env),
            BudBinop::BitXor => Self::compile_bitxor(src, dest, size_src, size_dest, instrs, fenv, env),
            BudBinop::Equal => Self::compile_equal(src, dest, size_src, size_dest, instrs, fenv, env),
            BudBinop::NotEq => Self::compile_noteq(src, dest, size_src, size_dest, instrs, fenv, env),
            BudBinop::Greater => Self::compile_greater(src, dest, size_src, size_dest, instrs, fenv, env),
            BudBinop::GrtrEq => Self::compile_grtreq(src, dest, size_src, size_dest, instrs, fenv, env),
            BudBinop::Less => Self::compile_less(src, dest, size_src, size_dest, instrs, fenv, env),
            BudBinop::LessEq => Self::compile_lesseq(src, dest, size_src, size_dest, instrs, fenv, env),
        }
    }

    fn compile_plus(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
        // Plus doesn't necessarily need a dreg, but it does need to be extended
        let (src, _) = Self::extend_efficient(
            src, size_src, size_dest, instrs, fenv, env, Proxy1,
        )?;
        let dest = fenv.place_to_addr_mode(dest, instrs, Proxy2)?;
        let instr = Add(size_dest, src, dest).validate()?;
        instrs.push(instr);
        Ok(())
    }

    fn compile_minus(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
        // Minus doesn't necessarily need a dreg, but it does need to be extended
        let (src, _) = Self::extend_efficient(
            src, size_src, size_dest, instrs, fenv, env, Proxy1,
        )?;
        let dest = fenv.place_to_addr_mode(dest, instrs, Proxy2)?;
        let instr = Sub(size_dest, src, dest).validate()?;
        instrs.push(instr);
        Ok(())
    }

    fn compile_times(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
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

    fn compile_div(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
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

    fn compile_and(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment, _env: &Environment) -> Result<(), String> {
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

    fn compile_or(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment, _env: &Environment) -> Result<(), String> {
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

    fn compile_bitand(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
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

    fn compile_bitor(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
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

    fn compile_bitxor(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
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

    fn compile_equal(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
        Self::compile_minus(src, dest, size_src, size_dest, instrs, fenv, env)
    }

    fn compile_noteq(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
        todo!()
    }

    fn compile_greater(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
        todo!()
    }

    fn compile_grtreq(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
        todo!()
    }

    fn compile_less(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
        todo!()
    }

    fn compile_lesseq(src: Place, dest: Place, size_src: DataSize, size_dest: DataSize, instrs: &mut Vec<Instruction<Valid>>, fenv: &mut FunctionEnvironment, env: &Environment) -> Result<(), String> {
        todo!()
    }

}
