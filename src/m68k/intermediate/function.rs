//! The functions in this file prepare the compilation process to begin by initializing
//! the FunctionInterEnvironment.
//! 
//! Author:     Brian Smith
//! Year:       2023

use std::ops::RangeFrom;

use crate::{m68k::*, bud::*, error::*, logging::LoggingOptions};

use super::{return_plan::ReturnPlan, inter_instr::InterInstr, fienv::FunctionInterEnvironment, place::Place};

use log::*;

impl Function {
    pub fn get_name(&self) -> String {
        self.signature.name.name.to_owned()
    }
}

pub fn get_inter_instrs(expr: Expr, signature: &Signature, label_gen: RangeFrom<usize>, log_options: &LoggingOptions, env: &Environment) -> Result<(Vec<InterInstr>, FunctionInterEnvironment), BudErr> {
    let mut instrs = Vec::new();
    let mut fienv = FunctionInterEnvironment::new(signature.clone(), label_gen);
    let plan;
    if signature.name.tt.is_void() {
        plan = ReturnPlan::None;
    } else if signature.name.tt.is_array() || signature.name.tt.is_struct() {
        let ret_field = Field { tt: TypeType::Pointer(Box::new(signature.name.tt.clone())), name: "[retval]".to_owned() };
        fienv.add_var(&ret_field)?;
        // Manually add code at the end that will move the return value to the thing pointed to by [retval]
        // Do this by calling `fienv.ret(place, instrs)`
        plan = ReturnPlan::None;
    } else {
        plan = ReturnPlan::Move(Place::DTemp(0, signature.name.tt.clone()));
    }
    let range = expr.get_range_owned();
    compile::compile_expr(
        expr,
        plan,
        &mut instrs,
        &mut fienv,
        env,
    )?;
    if !fienv.cleanup_expr_created {
        if let Some(cleanup_label) = fienv.cleanup_label {
            let instr = InterInstr::Lbl(cleanup_label, range);
            instrs.push(instr);
        }
    }
    // Things we need to know (from searching the function expression):
    //  * String literals
    //  * All local variables (can be Fields)
    if log_options.print_inter_funcs {
        debug!("InterInstrs for function {}", signature.name.name);
        for instr in &instrs {
            debug!("\t{:?}", instr);
        }
    }
    Ok((instrs, fienv))
}