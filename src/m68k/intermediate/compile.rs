//! Author:     Brian Smith
//! Year:       2023

use std::ops::Range;

use log::*;

use crate::{m68k::*, bud::*, error::*, u_err, c_err};

use super::{return_plan::ReturnPlan, inter_instr::InterInstr, fienv::FunctionInterEnvironment, place::Place};


pub fn compile_expr(expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    compile_bin_expr(*expr.bin_expr,
        if expr.with_semicolon {
            ReturnPlan::None
        } else {
            plan
        },
        instrs, fienv, env)?;
    Ok(())
}
pub fn compile_bin_expr(be: BinExpr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    match be {
        BinExpr::Binary(nbe, b, be, range) => {
            let retreg = Place::DTemp(0, fienv.return_type());
            match (plan, fienv.return_type().is_array() || fienv.return_type().is_struct(), retreg) {
                (ReturnPlan::Binop(pb, place), _, _) => {
                    // Make the temporary variable with the same type as the ReturnPlan
                    // As a future optimization, we do not need to get a new place if
                    // both binops are the same and if the binop is associative
                    let tt = place.get_type();
                    let dtemp = fienv.get_data_temp(tt.clone(), Some(nbe.get_range_owned()))?;
                    let d_place = Place::DTemp(dtemp, tt);
                    let plan = ReturnPlan::Move(d_place.clone());
                    compile_non_bin_expr(*nbe, plan, instrs, fienv, env)?;
                    let plan = ReturnPlan::Binop(b, d_place.clone());
                    compile_bin_expr(*be, plan, instrs, fienv, env)?;
                    ReturnPlan::Binop(pb, place).into_inter_instr(d_place, range, instrs, fienv, env)?;
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
                        return u_err!(nbe.get_range(), "Cannot read type {} as a condition, because it is not magic", preference);
                    }
                    let dtemp = fienv.get_data_temp(preference.clone(), Some(nbe.get_range_owned()))?;
                    let d_place = Place::DTemp(dtemp, preference);
                    let plan = ReturnPlan::Move(d_place.clone());
                    compile_non_bin_expr(*nbe, plan, instrs, fienv, env)?;
                    let plan = ReturnPlan::Binop(b, d_place.clone());
                    compile_bin_expr(*be, plan, instrs, fienv, env)?;
                    // Condition Codes should be set
                    d_place.free(fienv);
                }
                (ReturnPlan::Return, true, _) => {
                    return u_err!(range, "Cannot return array or struct type `{}` from binary expression {}", fienv.return_type(), b);
                }
                (ReturnPlan::Push(tt), _, _) => {
                    let dtemp = fienv.get_data_temp(tt.clone(), Some(nbe.get_range_owned()))?;
                    let d_place = Place::DTemp(dtemp, tt);
                    let plan = ReturnPlan::Move(d_place.clone());
                    compile_non_bin_expr(*nbe, plan, instrs, fienv, env)?;
                    let plan = ReturnPlan::Binop(b, d_place.clone());
                    compile_bin_expr(*be, plan, instrs, fienv, env)?;
                    let instr = InterInstr::Push(d_place, range);
                    fienv.free_data_temp(dtemp);
                    instrs.push(instr);
                },
                (ReturnPlan::None, _, _) => {
                    compile_non_bin_expr(*nbe, ReturnPlan::None, instrs, fienv, env)?;
                    compile_bin_expr(*be, ReturnPlan::None, instrs, fienv, env)?;
                },
            }

        },
        BinExpr::NonBin(nbe, _) => {
            compile_non_bin_expr(*nbe, plan, instrs, fienv, env)?;
        },
    }
    Ok(())
}
pub fn compile_non_bin_expr(nbe: NonBinExpr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    match nbe {
        NonBinExpr::BlockExpr(exprs, range)                                                    => compile_block_expr(exprs, range, plan, instrs, fienv, env),
        NonBinExpr::AssignExpr(id, expr, _)                                                => compile_assign_expr(*id, *expr, plan, instrs, fienv, env),
        NonBinExpr::VarDeclAssgn(vd, expr, _)                                              => compile_var_decl_assign(*vd, *expr, plan, instrs, fienv, env),
        NonBinExpr::ReturnExpr(expr, range)                                                => compile_return_expr(expr.map(|x| *x), range, plan, instrs, fienv, env),
        NonBinExpr::CleanupCall(range)                                                     => compile_cleanup_call(range, plan, instrs, fienv, env),
        NonBinExpr::CleanupExpr(expr, _)                                               => compile_cleanup_expr(*expr, plan, instrs, fienv, env),
        NonBinExpr::IdExpr(id, _)                                                      => compile_id_expr(*id, plan, instrs, fienv, env),
        NonBinExpr::LitExpr(lit, range)                                                    => compile_lit_expr(lit, range, plan, instrs, fienv, env),
        NonBinExpr::ParenExpr(expr, _)                                                 => compile_paren_expr(*expr, plan, instrs, fienv, env),
        NonBinExpr::UnaryExpr(un, expr, _)                                                 => compile_unary_expr(un, *expr, plan, instrs, fienv, env),
        NonBinExpr::IfExpr(cond, expr, _)                                                  => compile_if_expr(*cond, *expr, plan, instrs, fienv, env),
        NonBinExpr::IfElse(cond, expr1, expr2, _)                                              => compile_if_else(*cond, *expr1, *expr2, plan, instrs, fienv, env),
        NonBinExpr::UnlExpr(cond, expr, _)                                                 => compile_unless_expr(*cond, *expr, plan, instrs, fienv, env),
        NonBinExpr::UnlElse(cond, expr1, expr2, _)                                             => compile_unless_else(*cond, *expr1, *expr2, plan, instrs, fienv, env),
        NonBinExpr::WhileExpr(cond, expr, _)                                               => compile_while_expr(*cond, *expr, plan, instrs, fienv, env),
        NonBinExpr::DoWhile(expr, cond, _)                                                 => compile_do_while(*cond, *expr, plan, instrs, fienv, env),
        NonBinExpr::Break(range)                                                           => compile_break(range, plan, instrs, fienv, env),
        NonBinExpr::Continue(range)                                                        => compile_continue(range, plan, instrs, fienv, env),
    }
}
pub fn compile_block_expr(mut exprs: Vec<Expr>, range: Range<usize>, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    let last = match exprs.pop() {
        Some(expr) => expr,
        None => {
            // Empty block expression
            match &plan {
                ReturnPlan::Binop(b, place) => {
                    return u_err!(range, "Empty block expression but expected to return to a binop expression, {} at {}", b, place);
                }
                ReturnPlan::Move(place) => {
                    return u_err!(range, "Empty block expression but expected to move result to {}", place);
                }
                ReturnPlan::Condition => {
                    return u_err!(range, "Empty block expression but expected to get condition codes from expr");
                }
                ReturnPlan::Push(tt) => {
                    return u_err!(range, "Empty block expression but expected to push result of type {}", tt);
                }
                ReturnPlan::Return => {
                    return u_err!(range, "Empty block expression but expected to return result of type {}", fienv.return_type());
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
pub fn compile_assign_expr(id: IdExpr, expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    let range = expr.get_range_owned();
    // Check that the variable exists
    let place = place_from_id_expr(id, instrs, fienv, env)?;
    let assign_plan = ReturnPlan::Move(place.clone());
    compile_expr(expr, assign_plan, instrs, fienv, env)?;
    plan.into_inter_instr(place, range, instrs, fienv, env)?;
    Ok(())
}
pub fn compile_var_decl_assign(vd: VarDecl, expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    let range = expr.get_range_owned();
    let field = Field::new(vd, env);
    fienv.add_var(&field)?;
    let place = Place::Var(field);
    let assign_plan = ReturnPlan::Move(place.clone());
    compile_expr(expr, assign_plan, instrs, fienv, env)?;
    plan.into_inter_instr(place, range, instrs, fienv, env)?;
    Ok(())
}
pub fn compile_return_expr(expr: Option<Expr>, range: Range<usize>, _plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    let plan = ReturnPlan::Return;
    match expr {
        Some(expr) => compile_expr(expr, plan, instrs, fienv, env),
        None => {
            if !fienv.return_type().is_void() {
                u_err!(range, "Must give an expression to return for function with return type {}", fienv.return_type())
            } else {
                Ok(())
            }
        }
    }
}
pub fn compile_cleanup_call(range: Range<usize>, _plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, _env: &Environment) -> Result<(), BudErr> {
    let cleanup_label = fienv.get_cleanup_label();
    let instr = InterInstr::Goto(cleanup_label, range);
    instrs.push(instr);
    Ok(())
}
pub fn compile_cleanup_expr(expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    if fienv.cleanup_expr_created {
        return u_err!(expr.get_range(), "Found more than one cleanup expression. There should only be one.");
    }
    match plan {
        ReturnPlan::Return => {
            let cleanup_label = fienv.get_cleanup_label();
            let instr = InterInstr::Lbl(cleanup_label, expr.get_range_owned());
            instrs.push(instr);
            fienv.cleanup_expr_created = true;
            compile_expr(expr, ReturnPlan::Return, instrs, fienv, env)?;
            Ok(())
        }
        _ => {
            u_err!(expr.get_range(), "Can only compile cleanup expression where a return value is expected. The cleanup expression should be at the end of the function.")
        }
    }
}
pub fn compile_id_expr(id: IdExpr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    let range = id.get_range_owned();
    let place = place_from_id_expr(id, instrs, fienv, env)?;
    plan.into_inter_instr(place, range, instrs, fienv, env)?;
    Ok(())
}
pub fn compile_lit_expr(lit: Literal, range: Range<usize>, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    match lit {
        Literal::Num(num) => {
            plan.imm_into_inter_instr(num, range, instrs, fienv, env)?;
        }
        Literal::Str(string) => {
            let str_ind = fienv.add_lit_string(string.clone());
            match plan {
                ReturnPlan::Move(to) => {
                    let instr = InterInstr::Movs(str_ind, to, range);
                    instrs.push(instr);
                }
                ReturnPlan::Push(tt) => {
                    let str_tt = Environment::get_str_tt();
                    if tt != str_tt {
                        return u_err!(range, "Expected to push value of type {} to stack but found string type {}", tt, str_tt);
                    }
                    let instr = InterInstr::Puss(str_ind, range);
                    instrs.push(instr);
                }
                ReturnPlan::Condition => {
                    return u_err!(range, "Cannot get condition codes from string literal \"{}\"", string);
                }
                ReturnPlan::Binop(b, _) =>  {
                    return u_err!(range, "Cannot do binary operation {} on string literal \"{}\"", b, string);
                }
                ReturnPlan::Return => fienv.rets(str_ind, instrs, env, range)?,
                ReturnPlan::None => {}
            }
        }
    }
    Ok(())
}
pub fn compile_paren_expr(expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    compile_expr(expr, plan, instrs, fienv, env)
}
/// Gets a reference to the given NonBinExpr and follows the given ReturnPlan with it.
pub fn get_reference(nbe: NonBinExpr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    // nbe needs to be an IdExpr
    if let NonBinExpr::IdExpr(id_expr, range) = nbe {
        let place = place_from_id_expr(*id_expr, instrs, fienv, env)?;
        if let ReturnPlan::Binop(b, _) = plan {
            return u_err!(range, "Binop {} not supported with references yet", b);
        }
        match place {
            Place::Var(field) => {
                match plan {
                    ReturnPlan::Binop(_, _) => {
                        panic!("Binop should be handled above in this function")
                    }
                    ReturnPlan::Move(to) => {
                        let instr = InterInstr::MoVA(field.name, to, range);
                        instrs.push(instr);
                        Ok(())
                    }
                    ReturnPlan::Condition => {
                        let atemp = fienv.get_addr_temp();
                        let a_place = Place::ATemp(atemp);
                        let instr = InterInstr::MoVA(field.name, a_place.clone(), range);
                        instrs.push(instr);
                        a_place.free(fienv);
                        Ok(())
                    }
                    ReturnPlan::Push(tt) => {
                        if tt != field.tt {
                            return u_err!(range, "Cannot convert {} to {}", field.tt, tt);
                        }
                        let instr = InterInstr::PuVA(field.name, range);
                        instrs.push(instr);
                        Ok(())
                    }
                    ReturnPlan::Return => {
                        fienv.retva(&field.name, instrs, env, range)?;
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
                c_err!(range, "Cannot get reference to temporary address register A{}", a)
            },
            Place::DTemp(d, tt) => {
                c_err!(range, "Cannot get reference to temporary data register {} D{}", tt, d)
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
                                let instr = InterInstr::Lea(a, d, off, *to_a, range);
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
                                        let instr = InterInstr::Lea(a, d, off, a, range.to_owned());
                                        instrs.push(instr);
                                        let a_place = Place::ATemp(a);
                                        let instr = InterInstr::Move(a_place, to, range);
                                        instrs.push(instr);
                                        fienv.free_addr_temp(a);
                                        if let Some(d) = d { fienv.free_data_temp(d); }
                                        Ok(())
                                    } else {
                                        u_err!(range, "Trying to cast pointer of type {} to type {}", tt, to_val_tt)
                                    }
                                } else {
                                    c_err!(range, "Trying to move pointer {} to DTemp of type {}",
                                        Place::Ref(a, d, off, tt),
                                        to_tt
                                    )
                                }
                            }
                            Place::Var(field) => {
                                // Move from Ref to Var
                                if let TypeType::Pointer(to_val_tt) = &field.tt {
                                    if tt == **to_val_tt {
                                        let instr = InterInstr::Lea(a, d, off, a, range.to_owned());
                                        instrs.push(instr);
                                        let a_place = Place::ATemp(a);
                                        let instr = InterInstr::Move(a_place, to, range);
                                        instrs.push(instr);
                                        fienv.free_addr_temp(a);
                                        if let Some(d) = d { fienv.free_data_temp(d); }
                                        Ok(())
                                    } else {
                                        u_err!(range, "Trying to cast pointer of type {} to type {}", tt, to_val_tt)
                                    }
                                } else {
                                    c_err!(range, "Trying to move pointer {} to DTemp of type {}",
                                        Place::Ref(a, d, off, tt),
                                        field.tt
                                    )
                                }
                            }
                            Place::Ref(_, _, _, to_tt) => {
                                // Move effective address from Ref to Ref
                                // Remember that we are moving the effective address OUT of self,
                                // but we are moving TO the thing pointed to by the plan's Ref.
                                if let TypeType::Pointer(to_val_tt) = &to_tt {
                                    if tt == **to_val_tt {
                                        let instr = InterInstr::Lea(a, d, off, a, range.to_owned());
                                        instrs.push(instr);
                                        let a_place = Place::ATemp(a);
                                        let instr = InterInstr::Move(a_place, to, range);
                                        instrs.push(instr);
                                        fienv.free_addr_temp(a);
                                        if let Some(d) = d { fienv.free_data_temp(d); }
                                        Ok(())
                                    } else {
                                        u_err!(range, "Trying to cast pointer of type {} to type {}", tt, to_val_tt)
                                    }
                                } else {
                                    c_err!("Trying to move pointer {} to DTemp of type {}",
                                        Place::Ref(a, d, off, tt),
                                        to_tt
                                    )
                                }
                            }
                        }
                    }
                    ReturnPlan::Condition => {
                        // Move from Ref to ATemp
                        let atemp = fienv.get_addr_temp();
                        let instr = InterInstr::Lea(a, d, off, atemp, range);
                        instrs.push(instr);
                        fienv.free_addr_temp(atemp);
                        fienv.free_addr_temp(a);
                        if let Some(d) = d { fienv.free_data_temp(d); }
                        Ok(())
                        
                    }
                    ReturnPlan::Push(to_tt) => {
                        if let TypeType::Pointer(to_val_tt) = &to_tt {
                            if tt == **to_val_tt {
                                let instr = InterInstr::Pea(a, d, off, range);
                                instrs.push(instr);
                                fienv.free_addr_temp(a);
                                if let Some(d) = d { fienv.free_data_temp(d); }
                                Ok(())
                            } else {
                                u_err!(range, "Trying to cast pointer of type {} to type {}", tt, to_val_tt)
                            }
                        } else {
                            c_err!(range, "Trying to move pointer {} to DTemp of type {}",
                                Place::Ref(a, d, off, tt),
                                to_tt
                            )
                        }
                    }
                    ReturnPlan::Return => {
                        // Move from Ref to ATemp, then return
                        // Because we are moving an effective address, we have to move
                        // to an ATemp before we move to a DTemp
                        let instr = InterInstr::Lea(a, d, off, a, range.to_owned());
                        instrs.push(instr);
                        let a_place = Place::ATemp(a);
                        fienv.ret(a_place, instrs, env, range)?;
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
        u_err!(nbe.get_range(), "Cannot get reference to non-IdExpr")
    }
}
pub fn compile_unary_expr(un: BudUnop, nbe: NonBinExpr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    let tt = plan.get_type(fienv);
    if let BudUnop::Ref = un {
        return get_reference(nbe, plan, instrs, fienv, env);
    }
    let range = nbe.get_range_owned();
    if let Some(tt) = &tt {
        if !tt.is_magic(env) {
            return u_err!(range, "Cannot return non-magic type {} from unary operator {}", tt, un);
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
                BudUnop::Neg => InterInstr::Neg(to, range),
                BudUnop::Not => InterInstr::Bnot(to, range),
                BudUnop::Ref => return c_err!(range, "Ref should be handled above in this function"),
            };
            instrs.push(instr);
        }
        (ReturnPlan::Binop(BudBinop::And | BudBinop::Or, _), BudUnop::Neg) | 
        (ReturnPlan::None, _) => {
            // Store the result of `nbe` into `to`, but do not do the unop
            compile_non_bin_expr(nbe, plan, instrs, fienv, env)?;
        }
        (_, BudUnop::Ref) => return c_err!(range, "Ref should be handled above in this function"),
        (_, un) => {
            // Store the result of `nbe` into a dtemp, do the unop, and then move the dtemp as the plan dictates
            let tt = tt.unwrap();   // if plan is not None, then tt is not None
            let dtemp = fienv.get_data_temp(tt.clone(), Some(range.to_owned()))?;
            let d_place = Place::DTemp(dtemp, tt);
            let instr = match un {
                BudUnop::Neg => InterInstr::Neg(d_place.clone(), range.to_owned()),
                BudUnop::Not => InterInstr::Bnot(d_place.clone(), range.to_owned()),
                BudUnop::Ref => return c_err!(range, "Ref should be handled above in this function"),
            };
            instrs.push(instr);
            plan.into_inter_instr(d_place, range, instrs, fienv, env)?;
            fienv.free_data_temp(dtemp);
        }
    }
    Ok(())
}
fn eval_cond(cond: Expr, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    let plan = ReturnPlan::Condition;
    compile_expr(cond, plan, instrs, fienv, env)?;
    // Condition codes should be set accordingly. I hope they are.
    Ok(())
}
pub fn compile_if_expr(cond: Expr, expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    let expr_range = expr.get_range_owned();
    let cond_range = cond.get_range_owned();
    match plan {
        ReturnPlan::None => {}
        ReturnPlan::Return => {
            if !fienv.return_type().is_void() {
                return u_err!(expr.get_range(), "Cannot return a value from if statement. Tried to return type {}", fienv.return_type());
            }
        }
        ReturnPlan::Condition => { return u_err!(expr.get_range(), "Cannot get condition codes from if statement."); }
        ReturnPlan::Binop(b, _) => { return u_err!(expr.get_range(), "Cannot return a value from if statement. Tried to return to binop {}", b); }
        ReturnPlan::Move(_) => { return u_err!(expr.get_range(), "Cannot return a value from if statement."); }
        ReturnPlan::Push(tt) => { return u_err!(expr.get_range(), "Cannot return a value from if statement. Tried to push type {}", tt); }
    }
    eval_cond(cond, instrs, fienv, env)?;
    let f_label = fienv.get_new_label();
    let instr = InterInstr::Beq(f_label, cond_range.to_owned());
    instrs.push(instr);
    compile_expr(expr, ReturnPlan::None, instrs, fienv, env)?;
    let instr = InterInstr::Lbl(f_label, cond_range);
    instrs.push(instr);
    if let ReturnPlan::Return = plan {
        let instr = InterInstr::Rts(expr_range);
        instrs.push(instr);
    }
    Ok(())
}
pub fn compile_if_else(cond: Expr, expr1: Expr, expr2: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    let cond_range = cond.get_range_owned();
    eval_cond(cond, instrs, fienv, env)?;
    let f_label = fienv.get_new_label();
    let e_label = fienv.get_new_label();    // end label
    let instr = InterInstr::Beq(f_label, cond_range.to_owned());
    instrs.push(instr);
    compile_expr(expr1, plan.clone(), instrs, fienv, env)?;
    let instr = InterInstr::Goto(e_label, cond_range.to_owned());
    instrs.push(instr);
    let instr = InterInstr::Lbl(f_label, cond_range.to_owned());
    instrs.push(instr);
    compile_expr(expr2, plan, instrs, fienv, env)?;
    let instr = InterInstr::Lbl(e_label, cond_range);
    instrs.push(instr);
    Ok(())
}
pub fn compile_unless_expr(cond: Expr, expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    let expr_range = expr.get_range_owned();
    let cond_range = cond.get_range_owned();
    match plan {
        ReturnPlan::None => {}
        ReturnPlan::Return => {
            if !fienv.return_type().is_void() {
                return u_err!(expr.get_range(), "Cannot return a value from unless statement. Tried to return type {}", fienv.return_type());
            }
        }
        ReturnPlan::Condition => { return u_err!(expr.get_range(), "Cannot get condition codes from unless statement."); }
        ReturnPlan::Binop(b, _) => { return u_err!(expr.get_range(), "Cannot return a value from unless statement. Tried to return to binop {}", b); }
        ReturnPlan::Move(_) => { return u_err!(expr.get_range(), "Cannot return a value from unless statement."); }
        ReturnPlan::Push(tt) => { return u_err!(expr.get_range(), "Cannot return a value from unless statement. Tried to push type {}", tt); }
    }
    eval_cond(cond, instrs, fienv, env)?;
    let f_label = fienv.get_new_label();
    let instr = InterInstr::Bne(f_label, cond_range.to_owned());
    instrs.push(instr);
    compile_expr(expr, ReturnPlan::None, instrs, fienv, env)?;
    let instr = InterInstr::Lbl(f_label, cond_range);
    instrs.push(instr);
    if let ReturnPlan::Return = plan {
        let instr = InterInstr::Rts(expr_range);
        instrs.push(instr);
    }
    Ok(())
}
pub fn compile_unless_else(cond: Expr, expr1: Expr, expr2: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    let cond_range = cond.get_range_owned();
    eval_cond(cond, instrs, fienv, env)?;
    let f_label = fienv.get_new_label();
    let e_label = fienv.get_new_label();    // end_label
    let instr = InterInstr::Bne(f_label, cond_range.to_owned());
    instrs.push(instr);
    compile_expr(expr1, plan.clone(), instrs, fienv, env)?;
    let instr = InterInstr::Goto(e_label, cond_range.to_owned());
    instrs.push(instr);
    let instr = InterInstr::Lbl(f_label, cond_range.to_owned());
    instrs.push(instr);
    compile_expr(expr2, plan, instrs, fienv, env)?;
    let instr = InterInstr::Lbl(e_label, cond_range);
    instrs.push(instr);
    Ok(())
}
pub fn compile_while_expr(cond: Expr, expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    let expr_range = expr.get_range_owned();
    let cond_range = cond.get_range_owned();
    match plan {
        ReturnPlan::None => {}
        ReturnPlan::Return => {
            if !fienv.return_type().is_void() {
                return u_err!(expr.get_range(), "Cannot return a value from while loop. Tried to return type {}", fienv.return_type());
            }
        }
        ReturnPlan::Condition => { return u_err!(expr.get_range(), "Cannot get condition codes from while loop."); }
        ReturnPlan::Binop(b, _) => { return u_err!(expr.get_range(), "Cannot return a value from while loop. Tried to return to binop {}", b); }
        ReturnPlan::Move(_) => { return u_err!(expr.get_range(), "Cannot return a value from while loop."); }
        ReturnPlan::Push(tt) => { return u_err!(expr.get_range(), "Cannot return a value from while loop. Tried to push type {}", tt); }
    }
    let continue_label = fienv.get_new_label();
    let break_label = fienv.get_new_label();
    let instr = InterInstr::Lbl(continue_label, cond_range.to_owned());
    instrs.push(instr);
    eval_cond(cond, instrs, fienv, env)?;
    let instr = InterInstr::Beq(break_label, cond_range.to_owned());
    instrs.push(instr);
    fienv.push_loop_stack(break_label, continue_label);
    compile_expr(expr, ReturnPlan::None, instrs, fienv, env)?;
    fienv.pop_loop_stack();
    let instr = InterInstr::Lbl(break_label, cond_range);
    instrs.push(instr);
    if let ReturnPlan::Return = plan {
        let instr = InterInstr::Rts(expr_range);
        instrs.push(instr);
    }
    Ok(())
}
pub fn compile_do_while(cond: Expr, expr: Expr, plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<(), BudErr> {
    let expr_range = expr.get_range_owned();
    let cond_range = cond.get_range_owned();
    let tt = expr.type_preference(fienv, env)?;
    let ret_place;
    match &plan {
        ReturnPlan::None => ret_place = None,
        ReturnPlan::Return => {
            if tt.is_array() || tt.is_struct() {
                return u_err!(expr_range, "Returning arrays and structs from functions not yet supported");
            }
            if fienv.return_type().is_void() {
                ret_place = None;
            } else {
                let dtemp = fienv.get_data_temp(tt.clone(), Some(expr.get_range_owned()))?;
                ret_place = Some(Place::DTemp(dtemp, tt));
            }
        }
        ReturnPlan::Condition => {
            if tt.is_array() || tt.is_struct() {
                return u_err!(expr_range, "Cannot get condition codes from array or struct");
            }
            let dtemp = fienv.get_data_temp(tt.clone(), Some(expr.get_range_owned()))?;
            ret_place = Some(Place::DTemp(dtemp, tt));
        }
        ReturnPlan::Binop(b, _) => {
            if tt.is_array() || tt.is_struct() {
                return u_err!(expr_range, "Cannot do binop {} on array or struct", b);
            }
            let dtemp = fienv.get_data_temp(tt.clone(), Some(expr.get_range_owned()))?;
            ret_place = Some(Place::DTemp(dtemp, tt));
        }
        ReturnPlan::Move(to) => ret_place = Some(to.clone()),
        ReturnPlan::Push(push_tt) => {
            let dtemp = fienv.get_data_temp(push_tt.clone(), Some(expr.get_range_owned()))?;
            ret_place = Some(Place::DTemp(dtemp, push_tt.clone()));
        }
    }
    let continue_label = fienv.get_new_label();
    let break_label = fienv.get_new_label();
    let instr = InterInstr::Lbl(continue_label, cond_range.to_owned());
    instrs.push(instr);
    fienv.push_loop_stack(break_label, continue_label);
    let body_plan = match &ret_place {
        Some(ret_place) => ReturnPlan::Move(ret_place.clone()),
        None => ReturnPlan::None,
    };
    compile_expr(expr, body_plan, instrs, fienv, env)?;
    eval_cond(cond, instrs, fienv, env)?;
    let instr = InterInstr::Bne(continue_label, cond_range.to_owned());
    instrs.push(instr);
    fienv.pop_loop_stack();
    let instr = InterInstr::Lbl(break_label, cond_range.to_owned());
    instrs.push(instr);
    if let Some(ret_place) = ret_place {
        plan.into_inter_instr(ret_place, expr_range, instrs, fienv, env)?;
    } else if let ReturnPlan::Return = plan {
        let instr = InterInstr::Rts(cond_range);
        instrs.push(instr);
    }
    Ok(())
}
pub fn compile_break(range: Range<usize>, _plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, _env: &Environment) -> Result<(), BudErr> {
    match fienv.get_break_label() {
        Some(break_label) => {
            let instr = InterInstr::Goto(break_label, range);
            instrs.push(instr);
        }
        None => {
            return u_err!(range, "Break found outside loop");
        }
    }
    Ok(())
}
pub fn compile_continue(range: Range<usize>, _plan: ReturnPlan, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, _env: &Environment) -> Result<(), BudErr> {
    match fienv.get_continue_label() {
        Some(continue_label) => {
            let instr = InterInstr::Goto(continue_label, range);
            instrs.push(instr);
        }
        None => {
            return u_err!(range, "Continue found outside loop");
        }
    }
    Ok(())
}

pub fn call_func(id: String, offsets: Vec<Expr>, range: Range<usize>, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<Place, BudErr> {
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
            let instr = InterInstr::Grs(rs_lbl, range.to_owned());
            instrs.push(instr);
            // Maybe I should use a separate number generator in fienv for
            // StackMarkers, but I'm lazy
            let marker = fienv.get_new_label();
            let instr = InterInstr::SMarker(marker, range.to_owned());
            instrs.push(instr);
            let mut sh: StackHeight = 0;
            for (i, offset) in offsets.into_iter().enumerate() {
                let tt = if i < args.len() {
                    args[i].tt.clone()
                } else {
                    let tt = offset.type_preference(fienv, env)?;
                    warn!("Type used for {}th argument: {}", i, tt);
                    tt
                };
                sh += tt.get_size(env, Some(&range))? as StackHeight;
                let plan = ReturnPlan::Push(tt);
                compile_expr(offset, plan, instrs, fienv, env)?;
            }
            let instr = InterInstr::Save(rs_lbl, fienv.get_active_temps(), range.to_owned());
            instrs.push(instr);
            let instr = InterInstr::Call(id.clone(), marker, range.to_owned());
            instrs.push(instr);
            let instr = InterInstr::IncSP(sh, range.to_owned());
            instrs.push(instr);
            let place = env.ret_place(id, range)?;
            Ok(place)
        }
        None => {
            u_err!("Undeclared function {}", id)
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
pub fn place_from_id_expr(id: IdExpr, instrs: &mut Vec<InterInstr>, fienv: &mut FunctionInterEnvironment, env: &Environment) -> Result<Place, BudErr> {
    match id {
        IdExpr::SquareIndex(id, offset, _) => {
            // The id must be assignable
            match *id.bin_expr {
                BinExpr::NonBin(nbe, range) => {
                    if let NonBinExpr::IdExpr(id_expr, _) = *nbe {
                        let id_place = place_from_id_expr(*id_expr, instrs, fienv, env)?;
                        // Check if the offset is a literal value
                        let mut lit = None;
                        if let BinExpr::NonBin(nbe, _) = &*offset.bin_expr {
                            if let NonBinExpr::LitExpr(Literal::Num(num), _) = &**nbe {
                                lit = Some(*num);
                            }
                            // TODO Handle struct member access here
                        }
                        match lit {
                            Some(num) => {
                                let place = id_place.index_into(None, num, false, range, instrs, fienv, env)?;
                                return Ok(place);
                            }
                            None => {
                                let tt = TypeType::Id("i32".to_owned());
                                let off_temp = fienv.get_data_temp(tt.clone(), Some(range.clone()))?;   // offset as an index (not multiplied by sizeof(T))
                                let off_place = Place::DTemp(off_temp, tt);
                                let plan = ReturnPlan::Move(off_place);
                                compile_expr(*offset, plan, instrs, fienv, env)?;
                                let place = id_place.index_into(Some(off_temp), 0, false, range, instrs, fienv, env)?;
                                return Ok(place);
                            }
                        }
                    }
                    // id was a NonBinExpr but not an IdExpr
                    u_err!(range, "Expression not assignable")
                }
                BinExpr::Binary(_, b, _, range) => {
                    // id was not a NonBinExpr (it was a BinExpr)
                    u_err!(range, "Cannot index into binary expression, {}", b)
                }
            }
        },
        IdExpr::RoundIndex(id, offsets, _) => {
            // The id must be assignable
            match *id.bin_expr {
                BinExpr::NonBin(nbe, range) => {
                    if let NonBinExpr::IdExpr(id_expr, range) = *nbe {
                        let offsets = match offsets {
                            Some(offsets) => offsets,
                            None => Vec::new(),
                        };
                        let offset;
                        let id_place = if let IdExpr::Id(id, range) = *id_expr {
                            match fienv.get_var(&id) {
                                Some(field) => {
                                    // variable exists
                                    if offsets.len() == 1 {
                                        offset = offsets.into_iter().next().unwrap();
                                    } else {
                                        return u_err!(range, "Array indexing with () should only get 1 argument, but {} were given", offsets.len());
                                    }
                                    Place::Var(field)
                                }
                                None => {
                                    // Check if it's a function call
                                    return call_func(id, offsets, range, instrs, fienv, env);
                                }
                            }
                        } else {
                            // Function pointers not supported, so any other expression means we're
                            // indexing, not calling a function
                            if offsets.len() == 1 {
                                offset = offsets.into_iter().next().unwrap();
                            } else {
                                return u_err!(range, "Array indexing with () should only get 1 argument, but {} were given", offsets.len());
                            }
                            place_from_id_expr(*id_expr, instrs, fienv, env)?
                        };

                        // Check if the offset is a literal value
                        let mut lit = None;
                        if let BinExpr::NonBin(nbe, _) = &*offset.bin_expr {
                            if let NonBinExpr::LitExpr(Literal::Num(num), _) = &**nbe {
                                lit = Some(*num);
                            }
                        }
                        match lit {
                            Some(num) => {
                                let place = id_place.index_into(None, num, true, range, instrs, fienv, env)?;
                                return Ok(place);
                            }
                            None => {
                                let tt = TypeType::Id("i32".to_owned());
                                let off_temp = fienv.get_data_temp(tt.clone(), Some(range.clone()))?;   // offset as an index (not multiplied by sizeof(T))
                                let off_place = Place::DTemp(off_temp, tt);
                                let plan = ReturnPlan::Move(off_place);
                                compile_expr(offset, plan, instrs, fienv, env)?;
                                let place = id_place.index_into(Some(off_temp), 0, true, range, instrs, fienv, env)?;
                                return Ok(place);
                            }
                        }
                    }
                    // id was a NonBinExpr but not an IdExpr
                    u_err!(range, "Expression not assignable")
                }
                BinExpr::Binary(_, b, _, range) => {
                    // id was not a NonBinExpr (it was a BinExpr)
                    u_err!(range, "Cannot index into binary expression, {}", b)
                }
            }
        },
        IdExpr::Id(id, range) => {
            // We need to store the result of expr into the variable, id
            let place = match fienv.get_var(&id) {
                Some(field) => {
                    Place::Var(field)
                },
                None => {
                    return u_err!(range, "Undeclared variable {}", id);
                }
            };
            Ok(place)
        },
        IdExpr::Deref(id_expr, range) => {
            // We need to store the result of expr into the memory
            // location pointed to by the variable, id
            let place = place_from_id_expr(*id_expr, instrs, fienv, env)?;
            Ok(place.index_into(None, 0, false, range, instrs, fienv, env)?)
        },
    }
}