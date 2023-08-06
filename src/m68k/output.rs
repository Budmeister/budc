//! Handles saving the output to a .S file
//! 
//! Author:     Brian Smith
//! Year:       2023

use std::{fs::File, io::{BufWriter, Write}, path::Path};

use crate::{error::UserErr, u_err, m68k::DataSize};

use super::{Environment, CompiledFunction, ValidInstruction, AddrMode, ADBitField, DReg, AReg};

type IOErr = std::io::Error;

impl From<IOErr> for UserErr {
    fn from(err: std::io::Error) -> UserErr {
        UserErr { 
            msg: format!("Error while saving to file: {}", err),
            location: None,
        }
    }
}

pub fn write_file(env: Environment, filepath: &str) -> Result<(), UserErr> {
    let path = Path::new(filepath);
    // Create a closure and then immediately call it so that we can use the ? operator
    let name = match (|| {
        Some(path.file_name()?.to_str()?)
    })() {
        Some(name) => name,
        None => return u_err!("Unable to get filename from path: {}", filepath),
    };
    let file = File::create(filepath)?;
    let mut writer = BufWriter::new(file);

    write_file_preamble(name, &mut writer)?;

    for func in env.compiled_funcs {
        write_func(func, &mut writer)?;
    }
    Ok(())
}

fn write_file_preamble(filename: &str, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\t.file\t\"{}\"", filename)?;
    writeln!(writer, ".text")?;
    Ok(())
}

fn write_func(func: CompiledFunction, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    write_func_preamble(&func.signature.name.name, func.lit_strings, writer)?;
    for instr in func.instructions {
        write_instr(instr, writer)?;
    }
    Ok(())
}

fn write_func_preamble(name: &str, lit_strings: Vec<(usize, String)>, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    for (lbl, lit_string) in lit_strings {
        writeln!(writer, ".LC{}:", lbl)?;
        writeln!(writer, "\t.ascii \"{}\\0\"", lit_string)?;
    }
    
    writeln!(writer, "\t.even")?;
    writeln!(writer, ".globl {}", name)?;
    writeln!(writer, "{}:", name)?;
    Ok(())
}

fn write_instr(instr: ValidInstruction, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    use super::Instruction::*;
    match instr.get() {
        Move(size, src, dest) => write_move_instr(size, src, dest, writer),
        MoveMRtoM(size, src, dest) => write_movemrtom_instr(size, src, dest, writer),
        MoveMMtoR(size, src, dest) => write_movemmtor_instr(size, src, dest, writer),
        Add(size, src, dest) => write_add_instr(size, src, dest, writer),
        Sub(size, src, dest) => write_sub_instr(size, src, dest, writer),
        Neg(size, dest) => write_neg_instr(size, dest, writer),
        Clr(size, dest) => write_clr_instr(size, dest, writer),
        Not(size, dest) => write_not_instr(size, dest, writer),
        Tst(size, dest) => write_tst_instr(size, dest, writer),
        Cmp(size, src, dest) => write_cmp_instr(size, src, dest, writer),
        Eor(size, src, dest) => write_eor_instr(size, src, dest, writer),
        And(size, src, dest) => write_and_instr(size, src, dest, writer),
        Or(size, src, dest) => write_or_instr(size, src, dest, writer),
        Divs(src, dest) => write_divs_instr(src, dest, writer),
        Divu(src, dest) => write_divu_instr(src, dest, writer),
        Muls(src, dest) => write_muls_instr(src, dest, writer),
        Mulu(src, dest) => write_mulu_instr(src, dest, writer),
        Asl(size, src, dest) => write_asl_instr(size, src, dest, writer),
        Asr(size, src, dest) => write_asr_instr(size, src, dest, writer),
        Lsl(size, src, dest) => write_lsl_instr(size, src, dest, writer),
        Lsr(size, src, dest) => write_lsr_instr(size, src, dest, writer),
        Rol(size, src, dest) => write_rol_instr(size, src, dest, writer),
        Ror(size, src, dest) => write_ror_instr(size, src, dest, writer),
        Roxl(size, src, dest) => write_roxl_instr(size, src, dest, writer),
        Roxr(size, src, dest) => write_roxr_instr(size, src, dest, writer),
        Swap(dest) => write_swap_instr(dest, writer),
        AndiCCR(src) => write_andiccr_instr(src, writer),
        OriCCR(src) => write_oriccr_instr(src, writer),
        Stop => write_stop_instr(writer),
        Nop => write_nop_instr(writer),
        Reset => write_reset_instr(writer),
        Jsr(func) => write_jsr_instr(func, writer),
        Rte => write_rte_instr(writer),
        Rts => write_rts_instr(writer),
        Lbl(lbl) => write_lbl_instr(lbl, writer),
        Jmp(lbl) => write_jmp_instr(lbl, writer),
        Bra(lbl) => write_bra_instr(lbl, writer),
        Beq(lbl) => write_beq_instr(lbl, writer),
        Bne(lbl) => write_bne_instr(lbl, writer),
        Bcc(lbl) => write_bcc_instr(lbl, writer),
        Bcs(lbl) => write_bcs_instr(lbl, writer),
        Bge(lbl) => write_bge_instr(lbl, writer),
        Bgt(lbl) => write_bgt_instr(lbl, writer),
        Ble(lbl) => write_ble_instr(lbl, writer),
        Blt(lbl) => write_blt_instr(lbl, writer),
        Bmi(lbl) => write_bmi_instr(lbl, writer),
        Bpl(lbl) => write_bpl_instr(lbl, writer),
        Bhi(lbl) => write_bhi_instr(lbl, writer),
        Bhs(lbl) => write_bhs_instr(lbl, writer),
        Bls(lbl) => write_bls_instr(lbl, writer),
        Blo(lbl) => write_blo_instr(lbl, writer),
        Chk(src, dest) => write_chk_instr(src, dest, writer),
        Trap(trap) => write_trap_instr(trap, writer),
        Trapv(trap) => write_trapv_instr(trap, writer),
        Link(height, areg) => write_link_instr(height, areg, writer),
        Unlk(areg) => write_unlk_instr(areg, writer),
        ExtW(dreg) => write_extw_instr(dreg, writer),
        ExtL(dreg) => write_extl_instr(dreg, writer),
        Pea(src) => write_pea_instr(src, writer),
        Lea(src, areg) => write_lea_instr(src, areg, writer),
    }
}

fn write_move_instr(size: DataSize, src: AddrMode, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tmove{} {}, {}", size, src, dest)
}

fn write_movemrtom_instr(size: DataSize, src: ADBitField, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tmovem{} {}, {}", size, src, dest)
}

fn write_movemmtor_instr(size: DataSize, src: AddrMode, dest: ADBitField, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tmovem{} {}, {}", size, src, dest)
}

fn write_add_instr(size: DataSize, src: AddrMode, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tadd{} {}, {}", size, src, dest)
}

fn write_sub_instr(size: DataSize, src: AddrMode, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tsub{} {}, {}", size, src, dest)
}

fn write_neg_instr(size: DataSize, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tadd{} {}", size, dest)
}

fn write_clr_instr(size: DataSize, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tclr{} {}", size, dest)
}

fn write_not_instr(size: DataSize, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tnot{} {}", size, dest)
}

fn write_tst_instr(size: DataSize, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\ttst{} {}", size, dest)
}

fn write_cmp_instr(size: DataSize, src: AddrMode, dest: DReg, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tcmp{}, {}, {}", size, src, dest)
}

fn write_eor_instr(size: DataSize, src: DReg, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\teor{}, {}, {}", size, src, dest)
}

fn write_and_instr(size: DataSize, src: AddrMode, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tand{}, {}, {}", size, src, dest)
}

fn write_or_instr(size: DataSize, src: AddrMode, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tor{}, {}, {}", size, src, dest)
}

fn write_divs_instr(src: AddrMode, dest: DReg, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tdivs {}, {}", src, dest)
}

fn write_divu_instr(src: AddrMode, dest: DReg, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tdivu {}, {}", src, dest)
}

fn write_muls_instr(src: AddrMode, dest: DReg, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tmuls {}, {}", src, dest)
}

fn write_mulu_instr(src: AddrMode, dest: DReg, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tmulu {}, {}", src, dest)
}

fn write_asl_instr(size: DataSize, src: Option<AddrMode>, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    match src {
        Some(src) => writeln!(writer, "asl{} {}, {}", size, src, dest),
        None => writeln!(writer, "\tasl {}", dest),
    }
}

fn write_asr_instr(size: DataSize, src: Option<AddrMode>, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    match src {
        Some(src) => writeln!(writer, "asl{} {}, {}", size, src, dest),
        None => writeln!(writer, "\tasl {}", dest),
    }

}

fn write_lsl_instr(size: DataSize, src: Option<AddrMode>, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    match src {
        Some(src) => writeln!(writer, "lsl{} {}, {}", size, src, dest),
        None => writeln!(writer, "\tlsl {}", dest),
    }
}

fn write_lsr_instr(size: DataSize, src: Option<AddrMode>, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    match src {
        Some(src) => writeln!(writer, "lsr{} {}, {}", size, src, dest),
        None => writeln!(writer, "\tlsr {}", dest),
    }
}

fn write_rol_instr(size: DataSize, src: Option<AddrMode>, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    match src {
        Some(src) => writeln!(writer, "rol{} {}, {}", size, src, dest),
        None => writeln!(writer, "\trol {}", dest),
    }
}

fn write_ror_instr(size: DataSize, src: Option<AddrMode>, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    match src {
        Some(src) => writeln!(writer, "ror{} {}, {}", size, src, dest),
        None => writeln!(writer, "\tror {}", dest),
    }
}

fn write_roxl_instr(size: DataSize, src: Option<AddrMode>, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    match src {
        Some(src) => writeln!(writer, "roxl{} {}, {}", size, src, dest),
        None => writeln!(writer, "\troxl {}", dest),
    }
}

fn write_roxr_instr(size: DataSize, src: Option<AddrMode>, dest: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    match src {
        Some(src) => writeln!(writer, "roxr{} {}, {}", size, src, dest),
        None => writeln!(writer, "\troxr {}", dest),
    }
}

fn write_swap_instr(dest: DReg, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tswap {}", dest)
}

fn write_andiccr_instr(src: u8, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tandi #{}", src)
}

fn write_oriccr_instr(src: u8, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tori #{}", src)
}

fn write_stop_instr(writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tstop")
}

fn write_nop_instr(writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tnop")
}

fn write_reset_instr(writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\treset")
}

fn write_jsr_instr(func: String, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tjsr {}", func)
}

fn write_rte_instr(writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\trte")
}

fn write_rts_instr(writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\trts")
}

fn write_lbl_instr(lbl: usize, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, ".L{}:", lbl)
}

fn write_jmp_instr(lbl: usize, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tjmp .L{}", lbl)
}

fn write_bra_instr(lbl: usize, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tbra .L{}", lbl)
}

fn write_beq_instr(lbl: usize, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tbeq .L{}", lbl)
}

fn write_bne_instr(lbl: usize, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tbne .L{}", lbl)
}

fn write_bcc_instr(lbl: usize, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tbcc .L{}", lbl)
}

fn write_bcs_instr(lbl: usize, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tbcs .L{}", lbl)
}

fn write_bge_instr(lbl: usize, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tbge .L{}", lbl)
}

fn write_bgt_instr(lbl: usize, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tbgt .L{}", lbl)
}

fn write_ble_instr(lbl: usize, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tble .L{}", lbl)
}

fn write_blt_instr(lbl: usize, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tblt .L{}", lbl)
}

fn write_bmi_instr(lbl: usize, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tbmi .L{}", lbl)
}

fn write_bpl_instr(lbl: usize, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tbpl .L{}", lbl)
}

fn write_bhi_instr(lbl: usize, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tbhi .L{}", lbl)
}

fn write_bhs_instr(lbl: usize, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tbhs .L{}", lbl)
}

fn write_bls_instr(lbl: usize, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tbls .L{}", lbl)
}

fn write_blo_instr(lbl: usize, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tblo .L{}", lbl)
}

fn write_chk_instr(src: AddrMode, dest: DReg, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tchk {}, {}", src, dest)
}

fn write_trap_instr(trap: u32, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\ttrap #{}", trap)
}

fn write_trapv_instr(trap: u32, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\ttrapv #{}", trap)
}

fn write_link_instr(height: i16, areg: AReg, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tlink {}, #{}", areg, height)
}

fn write_unlk_instr(areg: AReg, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tunlk {}", areg)
}

fn write_extw_instr(dreg: DReg, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\text.w {}", dreg)
}

fn write_extl_instr(dreg: DReg, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\text.l {}", dreg)
}

fn write_pea_instr(src: AddrMode, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tpea {}", src)
}

fn write_lea_instr(src: AddrMode, areg: AReg, writer: &mut BufWriter<File>) -> Result<(), IOErr> {
    writeln!(writer, "\tlea {}, {}", src, areg)
}

