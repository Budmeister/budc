//! Author:     Brian Smith
//! Year:       2023

use log::*;

use crate::{m68k::*, bud::*};

use super::{return_plan::ReturnPlan, inter_instr::{InterInstr, Imm}, fienv::FunctionInterEnvironment, place::Place};


pub fn compile_expr(expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    compile_bin_expr(*expr.bin_expr,
        if expr.with_semicolon {
            ReturnPlan::None
        } else {
            plan
        },
        instrs, fienv, env)?;
    Ok(())
}
pub fn compile_bin_expr(be: BinExpr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    match be {
        BinExpr::Binary(nbe, b, be) => {
            let retreg = Place::DTemp(0, fienv.return_type());
            match (plan, fienv.return_type().is_array() || fienv.return_type().is_struct(), retreg) {
                (ReturnPlan::Binop(pb, place), _, _) => {
                    // Make the temporary variable with the same type as the ReturnPlan
                    // As a future optimization, we do not need to get a new place if
                    // both binops are the same and if the binop is associative
                    let tt = place.get_type();
                    let dtemp = fienv.get_data_temp(tt.clone())?;
                    let d_place = Place::DTemp(dtemp, tt);
                    let plan = ReturnPlan::Move(d_place.clone());
                    compile_non_bin_expr(*nbe, plan, instrs, fienv, env)?;
                    let plan = ReturnPlan::Binop(b, d_place.clone());
                    compile_bin_expr(*be, plan, instrs, fienv, env)?;
                    ReturnPlan::Binop(pb, place).into_inter_instr(d_place, instrs, fienv, env)?;
                },
                (ReturnPlan::Move(place), _, _) | (ReturnPlan::Return, false, place) => {
                    let plan = ReturnPlan::Move(place.clone());
                    compile_non_bin_expr(*nbe, plan, instrs, fienv, env)?;
                    let plan = ReturnPlan::Binop(b, place);
                    compile_bin_expr(*be, plan, instrs, fienv, env)?;
                },
                (ReturnPlan::Condition, _, _) => {
                    let preference = nbe.type_preference(fienv, env)?;
                    if !preference.is_magic(env) {
                        return Err(format!("Cannot read type {} as a condition, because it is not magic", preference));
                    }
                    let dtemp = fienv.get_data_temp(preference.clone())?;
                    let d_place = Place::DTemp(dtemp, preference);
                    let plan = ReturnPlan::Move(d_place.clone());
                    compile_non_bin_expr(*nbe, plan, instrs, fienv, env)?;
                    let plan = ReturnPlan::Binop(b, d_place.clone());
                    compile_bin_expr(*be, plan, instrs, fienv, env)?;
                    // Condition Codes should be set
                    d_place.free(fienv);
                }
                (ReturnPlan::Return, true, _) => {
                    return Err(format!("Cannot return array or struct type `{}` from binary expression {}", fienv.return_type(), b));
                }
                (ReturnPlan::Push(tt), _, _) => {
                    let dtemp = fienv.get_data_temp(tt.clone())?;
                    let d_place = Place::DTemp(dtemp, tt);
                    let plan = ReturnPlan::Move(d_place.clone());
                    compile_non_bin_expr(*nbe, plan, instrs, fienv, env)?;
                    let plan = ReturnPlan::Binop(b, d_place.clone());
                    compile_bin_expr(*be, plan, instrs, fienv, env)?;
                    let instr = InterInstr::Push(d_place);
                    fienv.free_data_temp(dtemp);
                    instrs.push(instr);
                },
                (ReturnPlan::None, _, _) => {
                    compile_non_bin_expr(*nbe, ReturnPlan::None, instrs, fienv, env)?;
                    compile_bin_expr(*be, ReturnPlan::None, instrs, fienv, env)?;
                },
            }

        },
        BinExpr::NonBin(nbe) => {
            compile_non_bin_expr(*nbe, plan, instrs, fienv, env)?;
        },
    }
    Ok(())
}
pub fn compile_non_bin_expr(nbe: NonBinExpr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    match nbe {
        NonBinExpr::BlockExpr(exprs)        => compile_block_expr(exprs, plan, instrs, fienv, env),
        NonBinExpr::AssignExpr(id, expr)    => compile_assign_expr(*id, *expr, plan, instrs, fienv, env),
        NonBinExpr::VarDeclAssgn(vd, expr)  => compile_var_decl_assign(*vd, *expr, plan, instrs, fienv, env),
        NonBinExpr::ReturnExpr(expr)       => compile_return_expr(expr.map(|x| *x), plan, instrs, fienv, env),
        NonBinExpr::CleanupCall         => compile_cleanup_call(plan, instrs, fienv, env),
        NonBinExpr::CleanupExpr(expr)      => compile_cleanup_expr(*expr, plan, instrs, fienv, env),
        NonBinExpr::IdExpr(id)           => compile_id_expr(*id, plan, instrs, fienv, env),
        NonBinExpr::LitExpr(lit)          => compile_lit_expr(lit, plan, instrs, fienv, env),
        NonBinExpr::ParenExpr(expr)        => compile_paren_expr(*expr, plan, instrs, fienv, env),
        NonBinExpr::UnaryExpr(un, expr)     => compile_unary_expr(un, *expr, plan, instrs, fienv, env),
        NonBinExpr::IfExpr(cond, expr)        => compile_if_expr(*cond, *expr, plan, instrs, fienv, env),
        NonBinExpr::IfElse(cond, expr1, expr2)     => compile_if_else(*cond, *expr1, *expr2, plan, instrs, fienv, env),
        NonBinExpr::UnlExpr(cond, expr)       => compile_unless_expr(*cond, *expr, plan, instrs, fienv, env),
        NonBinExpr::UnlElse(cond, expr1, expr2)    => compile_unless_else(*cond, *expr1, *expr2, plan, instrs, fienv, env),
        NonBinExpr::WhileExpr(cond, expr)     => compile_while_expr(*cond, *expr, plan, instrs, fienv, env),
        NonBinExpr::DoWhile(expr, cond)       => compile_do_while(*cond, *expr, plan, instrs, fienv, env),
        NonBinExpr::Break               => compile_break(plan, instrs, fienv, env),
        NonBinExpr::Continue            => compile_continue(plan, instrs, fienv, env),
    }
}
pub fn compile_block_expr(mut exprs: Vec<Expr>, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    let last = match exprs.pop() {
        Some(expr) => expr,
        None => {
            // Empty block expression
            match &plan {
                ReturnPlan::Binop(b, place) => {
                    return Err(format!("Empty block expression but expected to return to a binop expression, {} at {}", b, place));
                }
                ReturnPlan::Move(place) => {
                    return Err(format!("Empty block expression but expected to move result to {}", place));
                }
                ReturnPlan::Condition => {
                    return Err(format!("Empty block expression but expected to get condition codes from expr"));
                }
                ReturnPlan::Push(tt) => {
                    return Err(format!("Empty block expression but expected to push result of type {}", tt));
                }
                ReturnPlan::Return => {
                    return Err(format!("Empty block expression but expected to return result of type {}", fienv.return_type()));
                }
                ReturnPlan::None => {
                    return Ok(());
                },
            }
        },
    };

    for expr in exprs {
        compile_expr(expr, ReturnPlan::None, instrs, fienv, env)?;
        // Check that the expressions have semicolons?
        // If we were to print errors and recover, this would be a good point to do it
    }
    compile_expr(last, plan, instrs, fienv, env)?;
    Ok(())
}
pub fn compile_assign_expr(id: IdExpr, expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    // Check that the variable exists
    let place = place_from_id_expr(id, instrs, fienv, env)?;
    let assign_plan = ReturnPlan::Move(place.clone());
    compile_expr(expr, assign_plan, instrs, fienv, env)?;
    plan.into_inter_instr(place, instrs, fienv, env)?;
    Ok(())
}
pub fn compile_var_decl_assign(vd: VarDecl, expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    let field = Field::new(vd, env);
    fienv.add_var(&field)?;
    let place = Place::Var(field);
    let assign_plan = ReturnPlan::Move(place.clone());
    compile_expr(expr, assign_plan, instrs, fienv, env)?;
    plan.into_inter_instr(place, instrs, fienv, env)?;
    Ok(())
}
pub fn compile_return_expr(expr: Option<Expr>, _plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    let plan = ReturnPlan::Return;
    match expr {
        Some(expr) => compile_expr(expr, plan, instrs, fienv, env),
        None => {
            if fienv.return_type().is_void() {
                Err(format!("Must give an expression to return for function with return type {}", fienv.return_type()))
            } else {
                Ok(())
            }
        }
    }
}
pub fn compile_cleanup_call(_plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, _env: &Environment) -> Result<(), String> {
    let cleanup_label = fienv.get_cleanup_label();
    let instr = InterInstr::Goto(cleanup_label);
    instrs.push(instr);
    Ok(())
}
pub fn compile_cleanup_expr(expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    if fienv.cleanup_expr_created {
        return Err(format!("Found more than one cleanup expression. There should only be one."));
    }
    match plan {
        ReturnPlan::Return => {
            let cleanup_label = fienv.get_cleanup_label();
            let instr = InterInstr::Lbl(cleanup_label);
            instrs.push(instr);
            fienv.cleanup_expr_created = true;
            compile_expr(expr, ReturnPlan::Return, instrs, fienv, env)?;
            Ok(())
        }
        _ => {
            Err(format!("Can only compile cleanup expression where a return value is expected. The cleanup expression should be at the end of the function."))
        }
    }
}
pub fn compile_id_expr(id: IdExpr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    let place = place_from_id_expr(id, instrs, fienv, env)?;
    plan.into_inter_instr(place, instrs, fienv, env)?;
    Ok(())
}
pub fn compile_lit_expr(lit: Literal, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    match lit {
        Literal::Num(num) => {
            plan.imm_into_inter_instr(num, instrs, fienv, env)?;
        }
        Literal::Str(string) => {
            let str_ind = fienv.add_lit_string(string.clone());
            match plan {
                ReturnPlan::Move(to) => {
                    let instr = InterInstr::Movs(str_ind, to);
                    instrs.push(instr);
                }
                ReturnPlan::Push(tt) => {
                    let str_tt = Environment::get_str_tt();
                    if tt != str_tt {
                        return Err(format!("Expected to push value of type {} to stack but found string type {}", tt, str_tt));
                    }
                    let instr = InterInstr::Puss(str_ind);
                    instrs.push(instr);
                }
                ReturnPlan::Condition => {
                    return Err(format!("Cannot get condition codes from string literal \"{}\"", string));
                }
                ReturnPlan::Binop(b, _) =>  {
                    return Err(format!("Cannot do binary operation {} on string literal \"{}\"", b, string));
                }
                ReturnPlan::Return => fienv.rets(str_ind, instrs, env)?,
                ReturnPlan::None => {}
            }
        }
    }
    Ok(())
}
pub fn compile_paren_expr(expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    compile_expr(expr, plan, instrs, fienv, env)
}
/// Gets a reference to the given NonBinExpr and follows the given ReturnPlan with it.
pub fn get_reference(nbe: NonBinExpr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    // nbe needs to be an IdExpr
    if let NonBinExpr::IdExpr(id_expr) = nbe {
        let place = place_from_id_expr(*id_expr, instrs, fienv, env)?;
        if let ReturnPlan::Binop(b, _) = plan {
            return Err(format!("Binop {} not supported with references yet", b));
        }
        match place {
            Place::Var(field) => {
                match plan {
                    ReturnPlan::Binop(_, _) => {
                        panic!("Binop should be handled above in this function")
                    }
                    ReturnPlan::Move(to) => {
                        let instr = InterInstr::MoVA(field.name, to);
                        instrs.push(instr);
                        Ok(())
                    }
                    ReturnPlan::Condition => {
                        let atemp = fienv.get_addr_temp()?;
                        let a_place = Place::ATemp(atemp);
                        let instr = InterInstr::MoVA(field.name, a_place.clone());
                        instrs.push(instr);
                        a_place.free(fienv);
                        Ok(())
                    }
                    ReturnPlan::Push(tt) => {
                        if tt != field.tt {
                            return Err(format!("Cannot convert {} to {}", field.tt, tt));
                        }
                        let instr = InterInstr::PuVA(field.name);
                        instrs.push(instr);
                        Ok(())
                    }
                    ReturnPlan::Return => {
                        fienv.retva(&field.name, instrs, env)?;
                        warn!("Returning reference to local variable `{}` from function", field.name);
                        Ok(())
                    }
                    ReturnPlan::None => {
                        // No return plan
                        Ok(())
                    }
                }
            }
            Place::ATemp(a) => {
                return Err(format!("Cannot get reference to temporary address register A{}", a));
            },
            Place::DTemp(d, tt) => {
                return Err(format!("Cannot get reference to temporary data register {} D{}", tt, d));
            },
            Place::Ref(a, d, off, tt) => {
                match plan {
                    ReturnPlan::Binop(_, _) => {
                        panic!("Binop should be handled above in this function")
                    }
                    ReturnPlan::Move(to) => {
                        match &to {
                            Place::ATemp(to_a) => {
                                // Move from Ref to ATemp
                                let instr = InterInstr::Lea(a, d, off, *to_a);
                                instrs.push(instr);
                                fienv.free_addr_temp(a);
                                if let Some(d) = d { fienv.free_data_temp(d); }
                                Ok(())
                            }
                            Place::DTemp(_, to_tt) => {
                                // Move from Ref to ATemp to DTemp
                                // Because we are moving an effective address, we have to move
                                // to an ATemp before we move to a DTemp
                                if let TypeType::Pointer(to_val_tt) = to_tt {
                                    if tt == **to_val_tt {
                                        let instr = InterInstr::Lea(a, d, off, a);
                                        instrs.push(instr);
                                        let a_place = Place::ATemp(a);
                                        let instr = InterInstr::Move(a_place, to);
                                        instrs.push(instr);
                                        fienv.free_addr_temp(a);
                                        if let Some(d) = d { fienv.free_data_temp(d); }
                                        Ok(())
                                    } else {
                                        Err(format!("Trying to cast pointer of type {} to type {}", tt, to_val_tt))
                                    }
                                } else {
                                    Err(format!("Trying to move pointer {} to DTemp of type {}",
                                        Place::Ref(a, d, off, tt),
                                        to_tt
                                    ))
                                }
                            }
                            Place::Var(field) => {
                                // Move from Ref to Var
                                if let TypeType::Pointer(to_val_tt) = &field.tt {
                                    if tt == **to_val_tt {
                                        let instr = InterInstr::Lea(a, d, off, a);
                                        instrs.push(instr);
                                        let a_place = Place::ATemp(a);
                                        let instr = InterInstr::Move(a_place, to);
                                        instrs.push(instr);
                                        fienv.free_addr_temp(a);
                                        if let Some(d) = d { fienv.free_data_temp(d); }
                                        Ok(())
                                    } else {
                                        Err(format!("Trying to cast pointer of type {} to type {}", tt, to_val_tt))
                                    }
                                } else {
                                    Err(format!("Trying to move pointer {} to DTemp of type {}",
                                        Place::Ref(a, d, off, tt),
                                        field.tt
                                    ))
                                }
                            }
                            Place::Ref(_, _, _, to_tt) => {
                                // Move effective address from Ref to Ref
                                // Remember that we are moving the effective address OUT of self,
                                // but we are moving TO the thing pointed to by the plan's Ref.
                                if let TypeType::Pointer(to_val_tt) = &to_tt {
                                    if tt == **to_val_tt {
                                        let instr = InterInstr::Lea(a, d, off, a);
                                        instrs.push(instr);
                                        let a_place = Place::ATemp(a);
                                        let instr = InterInstr::Move(a_place, to);
                                        instrs.push(instr);
                                        fienv.free_addr_temp(a);
                                        if let Some(d) = d { fienv.free_data_temp(d); }
                                        Ok(())
                                    } else {
                                        Err(format!("Trying to cast pointer of type {} to type {}", tt, to_val_tt))
                                    }
                                } else {
                                    Err(format!("Trying to move pointer {} to DTemp of type {}",
                                        Place::Ref(a, d, off, tt),
                                        to_tt
                                    ))
                                }
                            }
                        }
                    }
                    ReturnPlan::Condition => {
                        // Move from Ref to ATemp
                        let atemp = fienv.get_addr_temp()?;
                        let instr = InterInstr::Lea(a, d, off, atemp);
                        instrs.push(instr);
                        fienv.free_addr_temp(atemp);
                        fienv.free_addr_temp(a);
                        if let Some(d) = d { fienv.free_data_temp(d); }
                        Ok(())
                        
                    }
                    ReturnPlan::Push(to_tt) => {
                        if let TypeType::Pointer(to_val_tt) = &to_tt {
                            if tt == **to_val_tt {
                                let instr = InterInstr::Pea(a, d, off);
                                instrs.push(instr);
                                fienv.free_addr_temp(a);
                                if let Some(d) = d { fienv.free_data_temp(d); }
                                Ok(())
                            } else {
                                Err(format!("Trying to cast pointer of type {} to type {}", tt, to_val_tt))
                            }
                        } else {
                            Err(format!("Trying to move pointer {} to DTemp of type {}",
                                Place::Ref(a, d, off, tt),
                                to_tt
                            ))
                        }
                    }
                    ReturnPlan::Return => {
                        // Move from Ref to ATemp, then return
                        // Because we are moving an effective address, we have to move
                        // to an ATemp before we move to a DTemp
                        let instr = InterInstr::Lea(a, d, off, a);
                        instrs.push(instr);
                        let a_place = Place::ATemp(a);
                        fienv.ret(a_place, instrs, env)?;
                        if let Some(d) = d { fienv.free_data_temp(d); }
                        Ok(())
                    }
                    ReturnPlan::None => {
                        fienv.free_addr_temp(a);
                        if let Some(d) = d { fienv.free_data_temp(d); }
                        Ok(())
                    }
                }
            },
        }
    } else {
        Err(format!("Cannot get reference to non-IdExpr"))
    }
}
pub fn compile_unary_expr(un: BudUnop, nbe: NonBinExpr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    let tt = plan.get_type(fienv);
    if let BudUnop::Ref = un {
        return get_reference(nbe, plan, instrs, fienv, env);
    }
    if let Some(tt) = &tt {
        if !tt.is_magic(env) {
            return Err(format!("Cannot return non-magic type {} from unary operator {}", tt, un));
        }
    }
    match (plan.clone(), un) {
        (
            ReturnPlan::Binop(
                BudBinop::Times | BudBinop::Div,
                to
            ) | ReturnPlan::Move(to),
            un
        ) => {
            // Store result of `nbe` into `to`, and then do the unop on `to`
            compile_non_bin_expr(nbe, plan, instrs, fienv, env)?;
            let instr = match un {
                BudUnop::Neg => InterInstr::Neg(to),
                BudUnop::Not => InterInstr::Bnot(to),
                BudUnop::Ref => panic!("Ref should be handled above in this function"),
            };
            instrs.push(instr);
        }
        (ReturnPlan::Binop(BudBinop::And | BudBinop::Or, _), BudUnop::Neg) | 
        (ReturnPlan::None, _) => {
            // Store the result of `nbe` into `to`, but do not do the unop
            compile_non_bin_expr(nbe, plan, instrs, fienv, env)?;
        }
        (_, BudUnop::Ref) => panic!("Ref should be handled above in this function"),
        (_, un) => {
            // Store the result of `nbe` into a dtemp, do the unop, and then move the dtemp as the plan dictates
            let tt = tt.unwrap();   // if plan is not None, then tt is not None
            let dtemp = fienv.get_data_temp(tt.clone())?;
            let d_place = Place::DTemp(dtemp, tt);
            let instr = match un {
                BudUnop::Neg => InterInstr::Neg(d_place.clone()),
                BudUnop::Not => InterInstr::Bnot(d_place.clone()),
                BudUnop::Ref => panic!("Ref should be handled above in this function"),
            };
            instrs.push(instr);
            plan.into_inter_instr(d_place, instrs, fienv, env)?;
            fienv.free_data_temp(dtemp);
        }
    }
    Ok(())
}
fn eval_cond(cond: Expr, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    let plan = ReturnPlan::Condition;
    compile_expr(cond, plan, instrs, fienv, env)?;
    // Condition codes should be set accordingly. I hope they are.
    Ok(())
}
pub fn compile_if_expr(cond: Expr, expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    match plan {
        ReturnPlan::None => {}
        ReturnPlan::Return => {
            if !fienv.return_type().is_void() {
                return Err(format!("Cannot return a value from if statement. Tried to return type {}", fienv.return_type()));
            }
        }
        ReturnPlan::Condition => { return Err(format!("Cannot get condition codes from if statement.")); }
        ReturnPlan::Binop(b, _) => { return Err(format!("Cannot return a value from if statement. Tried to return to binop {}", b)); }
        ReturnPlan::Move(_) => { return Err(format!("Cannot return a value from if statement.")); }
        ReturnPlan::Push(tt) => { return Err(format!("Cannot return a value from if statement. Tried to push type {}", tt)); }
    }
    let f_label = fienv.get_new_label();
    let instr = InterInstr::Beq(f_label);
    instrs.push(instr);
    compile_expr(expr, ReturnPlan::None, instrs, fienv, env)?;
    let instr = InterInstr::Lbl(f_label);
    instrs.push(instr);
    if let ReturnPlan::Return = plan {
        let instr = InterInstr::Rts;
        instrs.push(instr);
    }
    Ok(())
}
pub fn compile_if_else(cond: Expr, expr1: Expr, expr2: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    eval_cond(cond, instrs, fienv, env)?;
    let f_label = fienv.get_new_label();
    let e_label = fienv.get_new_label();    // end label
    let instr = InterInstr::Beq(f_label);
    instrs.push(instr);
    compile_expr(expr1, plan.clone(), instrs, fienv, env)?;
    let instr = InterInstr::Goto(e_label);
    instrs.push(instr);
    let instr = InterInstr::Lbl(f_label);
    instrs.push(instr);
    compile_expr(expr2, plan, instrs, fienv, env)?;
    let instr = InterInstr::Lbl(e_label);
    instrs.push(instr);
    Ok(())
}
pub fn compile_unless_expr(cond: Expr, expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    match plan {
        ReturnPlan::None => {}
        ReturnPlan::Return => {
            if !fienv.return_type().is_void() {
                return Err(format!("Cannot return a value from unless statement. Tried to return type {}", fienv.return_type()));
            }
        }
        ReturnPlan::Condition => { return Err(format!("Cannot get condition codes from unless statement.")); }
        ReturnPlan::Binop(b, _) => { return Err(format!("Cannot return a value from unless statement. Tried to return to binop {}", b)); }
        ReturnPlan::Move(_) => { return Err(format!("Cannot return a value from unless statement.")); }
        ReturnPlan::Push(tt) => { return Err(format!("Cannot return a value from unless statement. Tried to push type {}", tt)); }
    }
    eval_cond(cond, instrs, fienv, env)?;
    let f_label = fienv.get_new_label();
    let instr = InterInstr::Bne(f_label);
    instrs.push(instr);
    compile_expr(expr, ReturnPlan::None, instrs, fienv, env)?;
    let instr = InterInstr::Lbl(f_label);
    instrs.push(instr);
    if let ReturnPlan::Return = plan {
        let instr = InterInstr::Rts;
        instrs.push(instr);
    }
    Ok(())
}
pub fn compile_unless_else(cond: Expr, expr1: Expr, expr2: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    eval_cond(cond, instrs, fienv, env)?;
    let f_label = fienv.get_new_label();
    let e_label = fienv.get_new_label();    // end_label
    let instr = InterInstr::Bne(f_label);
    instrs.push(instr);
    compile_expr(expr1, plan.clone(), instrs, fienv, env)?;
    let instr = InterInstr::Goto(e_label);
    instrs.push(instr);
    let instr = InterInstr::Lbl(f_label);
    instrs.push(instr);
    compile_expr(expr2, plan, instrs, fienv, env)?;
    let instr = InterInstr::Lbl(e_label);
    instrs.push(instr);
    Ok(())
}
pub fn compile_while_expr(cond: Expr, expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    match plan {
        ReturnPlan::None => {}
        ReturnPlan::Return => {
            if !fienv.return_type().is_void() {
                return Err(format!("Cannot return a value from while loop. Tried to return type {}", fienv.return_type()));
            }
        }
        ReturnPlan::Condition => { return Err(format!("Cannot get condition codes from while loop.")); }
        ReturnPlan::Binop(b, _) => { return Err(format!("Cannot return a value from while loop. Tried to return to binop {}", b)); }
        ReturnPlan::Move(_) => { return Err(format!("Cannot return a value from while loop.")); }
        ReturnPlan::Push(tt) => { return Err(format!("Cannot return a value from while loop. Tried to push type {}", tt)); }
    }
    let continue_label = fienv.get_new_label();
    let break_label = fienv.get_new_label();
    let instr = InterInstr::Lbl(continue_label);
    instrs.push(instr);
    eval_cond(cond, instrs, fienv, env)?;
    let instr = InterInstr::Beq(break_label);
    instrs.push(instr);
    fienv.push_loop_stack(break_label, continue_label);
    compile_expr(expr, ReturnPlan::None, instrs, fienv, env)?;
    fienv.pop_loop_stack();
    let instr = InterInstr::Lbl(break_label);
    instrs.push(instr);
    if let ReturnPlan::Return = plan {
        let instr = InterInstr::Rts;
        instrs.push(instr);
    }
    Ok(())
}
pub fn compile_do_while(cond: Expr, expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), String> {
    let tt = expr.type_preference(fienv, env)?;
    let ret_place;
    match &plan {
        ReturnPlan::None => ret_place = None,
        ReturnPlan::Return => {
            if tt.is_array() || tt.is_struct() {
                return Err(format!("Returning arrays and structs from functions not yet supported"));
            }
            if fienv.return_type().is_void() {
                ret_place = None;
            } else {
                let dtemp = fienv.get_data_temp(tt.clone())?;
                ret_place = Some(Place::DTemp(dtemp, tt));
            }
        }
        ReturnPlan::Condition => {
            if tt.is_array() || tt.is_struct() {
                return Err(format!("Cannot get condition codes from array or struct"));
            }
            let dtemp = fienv.get_data_temp(tt.clone())?;
            ret_place = Some(Place::DTemp(dtemp, tt));
        }
        ReturnPlan::Binop(b, _) => {
            if tt.is_array() || tt.is_struct() {
                return Err(format!("Cannot do binop {} on array or struct", b));
            }
            let dtemp = fienv.get_data_temp(tt.clone())?;
            ret_place = Some(Place::DTemp(dtemp, tt));
        }
        ReturnPlan::Move(to) => ret_place = Some(to.clone()),
        ReturnPlan::Push(push_tt) => {
            let dtemp = fienv.get_data_temp(push_tt.clone())?;
            ret_place = Some(Place::DTemp(dtemp, push_tt.clone()));
        }
    }
    let continue_label = fienv.get_new_label();
    let break_label = fienv.get_new_label();
    let instr = InterInstr::Lbl(continue_label);
    instrs.push(instr);
    fienv.push_loop_stack(break_label, continue_label);
    let body_plan = match &ret_place {
        Some(ret_place) => ReturnPlan::Move(ret_place.clone()),
        None => ReturnPlan::None,
    };
    compile_expr(expr, body_plan, instrs, fienv, env)?;
    eval_cond(cond, instrs, fienv, env)?;
    let instr = InterInstr::Bne(continue_label);
    instrs.push(instr);
    fienv.pop_loop_stack();
    let instr = InterInstr::Lbl(break_label);
    instrs.push(instr);
    if let Some(ret_place) = ret_place {
        plan.into_inter_instr(ret_place, instrs, fienv, env)?;
    } else if let ReturnPlan::Return = plan {
        let instr = InterInstr::Rts;
        instrs.push(instr);
    }
    Ok(())
}
pub fn compile_break(_plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, _env: &Environment) -> Result<(), String> {
    match fienv.get_break_label() {
        Some(break_label) => {
            let instr = InterInstr::Goto(break_label);
            instrs.push(instr);
        }
        None => {
            return Err(format!("Break found outside loop"));
        }
    }
    Ok(())
}
pub fn compile_continue(_plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, _env: &Environment) -> Result<(), String> {
    match fienv.get_continue_label() {
        Some(continue_label) => {
            let instr = InterInstr::Goto(continue_label);
            instrs.push(instr);
        }
        None => {
            return Err(format!("Continue found outside loop"));
        }
    }
    Ok(())
}

pub fn call_func(id: String, offsets: Vec<Expr>, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<Place, String> {
    // Check if it's a function call
    match env.global_funcs.get(&id) {
        Some(sig) => {
            let args = &sig.args;
            if args.len() != offsets.len() {
                warn!("Function signature for {} has {} arguments but {} were given", id, args.len(), offsets.len());
            }
            // Get register space so that after the args have been pushed 
            // to the stack, active regs can be saved before the function
            // call
            let rs_lbl = fienv.get_new_label();
            let instr = InterInstr::Grs(rs_lbl);
            instrs.push(instr);
            // Maybe I should use a separate number generator in fienv for
            // StackMarkers, but I'm lazy
            let marker = fienv.get_new_label();
            let instr = InterInstr::SMarker(marker);
            instrs.push(instr);
            for (i, offset) in offsets.into_iter().enumerate() {
                let tt = if i < args.len() {
                    args[i].tt.clone()
                } else {
                    let tt = offset.type_preference(fienv, env)?;
                    warn!("Type used for {}th argument: {}", i, tt);
                    tt
                };
                let plan = ReturnPlan::Push(tt);
                compile_expr(offset, plan, instrs, fienv, env)?;
            }
            let instr = InterInstr::Save(rs_lbl, fienv.get_active_temps());
            instrs.push(instr);
            let instr = InterInstr::Call(id.clone(), marker);
            instrs.push(instr);
            let place = env.ret_place(id)?;
            Ok(place)
        }
        None => {
            Err(format!("Undeclared variable {}", id))
        }
    }
}
///
/// Calculates a place that can be read from or written to that represents this IdExpr. DO NOT put arrays or
/// structs in data registers. `Place::index_into()` will modify the data register to index into it. This place
/// is live, meaning you are not free to destroy it or modify it unless you actually want to write to it.
/// 
/// `match id {`
/// 
/// * `IdExpr::SquareIndex(id, offset) =>`
/// Call `place_from_id_expr` on `id` and `offset`, and index into the given place using `Place::index_into()`.
/// 
/// * `IdExpr::RoundIndex(id, offset_exprs) =>`
/// The id must refer to a function, an array or a reference. 
/// If it's an function, then call the function with `offset_exprs` as the arguments.
/// If it's an array or struct, then insert code to check that the first argument of offset_exprs--
/// which represents the offset--is within the range of this array. If one extra argument is specified, 
/// it represents the maximum. No exception will be generated if `0 <= offset < max`. If two extra arguments
/// are specified, they represent a minimum and a maximum respectively. No exception will be generated if 
/// `min <= offset < max`. Then index into the place the same way as if id was a `SquareIndex`. 
/// **Not currently implemented.**
/// 
/// * `IdExpr::Id(name) =>`
/// Return this variable as a place
/// 
/// * `IdExpr::Deref(id_expr) =>`
/// Return the place containing `id_expr` by calling `place_from_id_expr` on `id_expr`. Remember, that if `id` 
/// is `IdExpr::Deref`, then the user typed the dereference operator and wants to assign to the referenced
/// object or read from it. 
/// 
/// `}`
pub fn place_from_id_expr(id: IdExpr, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<Place, String> {
    match id {
        IdExpr::SquareIndex(id, offset) => {
            // The id must be assignable
            match *id.bin_expr {
                BinExpr::NonBin(nbe) => {
                    if let NonBinExpr::IdExpr(id_expr) = *nbe {
                        let id_place = place_from_id_expr(*id_expr, instrs, fienv, env)?;
                        // Check if the offset is a literal value
                        let mut lit = None;
                        if let BinExpr::NonBin(nbe) = &*offset.bin_expr {
                            if let NonBinExpr::LitExpr(Literal::Num(num)) = &**nbe {
                                lit = Some(*num);
                            }
                            // TODO Handle struct member access here
                        }
                        match lit {
                            Some(num) => {
                                let place = id_place.index_into(None, num, false, instrs, fienv, env)?;
                                return Ok(place);
                            }
                            None => {
                                let tt = TypeType::Id("i32".to_owned());
                                let off_temp = fienv.get_data_temp(tt.clone())?;   // offset as an index (not multiplied by sizeof(T))
                                let off_place = Place::DTemp(off_temp, tt);
                                let plan = ReturnPlan::Move(off_place);
                                compile_expr(*offset, plan, instrs, fienv, env)?;
                                let place = id_place.index_into(Some(off_temp), 0, false, instrs, fienv, env)?;
                                return Ok(place);
                            }
                        }
                    }
                    // id was a NonBinExpr but not an IdExpr
                    Err(format!("Expression not assignable"))
                }
                BinExpr::Binary(_, b, _) => {
                    // id was not a NonBinExpr (it was a BinExpr)
                    Err(format!("Cannot index into binary expression, {}", b))
                }
            }
        },
        IdExpr::RoundIndex(id, offsets) => {
            // The id must be assignable
            match *id.bin_expr {
                BinExpr::NonBin(nbe) => {
                    if let NonBinExpr::IdExpr(id_expr) = *nbe {
                        let offsets = match offsets {
                            Some(offsets) => offsets,
                            None => Vec::new(),
                        };
                        let offset;
                        let id_place = if let IdExpr::Id(id) = *id_expr {
                            match fienv.get_var(&id) {
                                Some(field) => {
                                    // variable exists
                                    if offsets.len() == 1 {
                                        offset = offsets.into_iter().next().unwrap();
                                    } else {
                                        return Err(format!("Array indexing with () should only get 1 argument, but {} were given", offsets.len()));
                                    }
                                    Place::Var(field)
                                }
                                None => {
                                    // Check if it's a function call
                                    return call_func(id, offsets, instrs, fienv, env);
                                }
                            }
                        } else {
                            // Function pointers not supported, so any other expression means we're
                            // indexing, not calling a function
                            if offsets.len() == 1 {
                                offset = offsets.into_iter().next().unwrap();
                            } else {
                                return Err(format!("Array indexing with () should only get 1 argument, but {} were given", offsets.len()));
                            }
                            place_from_id_expr(*id_expr, instrs, fienv, env)?
                        };

                        // Check if the offset is a literal value
                        let mut lit = None;
                        if let BinExpr::NonBin(nbe) = &*offset.bin_expr {
                            if let NonBinExpr::LitExpr(Literal::Num(num)) = &**nbe {
                                lit = Some(*num);
                            }
                        }
                        match lit {
                            Some(num) => {
                                let place = id_place.index_into(None, num, true, instrs, fienv, env)?;
                                return Ok(place);
                            }
                            None => {
                                let tt = TypeType::Id("i32".to_owned());
                                let off_temp = fienv.get_data_temp(tt.clone())?;   // offset as an index (not multiplied by sizeof(T))
                                let off_place = Place::DTemp(off_temp, tt);
                                let plan = ReturnPlan::Move(off_place);
                                compile_expr(offset, plan, instrs, fienv, env)?;
                                let place = id_place.index_into(Some(off_temp), 0, true, instrs, fienv, env)?;
                                return Ok(place);
                            }
                        }
                    }
                    // id was a NonBinExpr but not an IdExpr
                    Err(format!("Expression not assignable"))
                }
                BinExpr::Binary(_, b, _) => {
                    // id was not a NonBinExpr (it was a BinExpr)
                    Err(format!("Cannot index into binary expression, {}", b))
                }
            }
        },
        IdExpr::Id(id) => {
            // We need to store the result of expr into the variable, id
            let place = match fienv.get_var(&id) {
                Some(field) => {
                    Place::Var(field)
                },
                None => {
                    return Err(format!("Undeclared variable {}", id));
                }
            };
            Ok(place)
        },
        IdExpr::Deref(id_expr) => {
            // We need to store the result of expr into the memory
            // location pointed to by the variable, id
            let place = place_from_id_expr(*id_expr, instrs, fienv, env)?;
            Ok(place.index_into(None, 0, false, instrs, fienv, env)?)
        },
    }
}