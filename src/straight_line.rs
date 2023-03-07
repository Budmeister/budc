// Author:      Brian Smith
// Date:        January 31, 2023
// Description: Tree structure for holding expression information and
//              a pretty print function (json-like) as well as an execute 
//              for statements.



// Stm -> Stm ; Stm         (CompoundStm)
// Stm -> IdExp := Exp      (AssignStm)
// Stm -> print(ExpList)    (PrintStm)
// Exp -> num               (NumExp)
// Exp -> Exp Binop Exp     (OpExp)
// Exp -> (Stm, Exp)        (EseqExp)
// ExpList -> Exp, ExpList  (PairExpList)
// ExpList -> Exp           (LastExpList)

use std::{collections::HashMap, fmt::Display};

pub enum Stm {
    CompoundStm(Box<Stm>, Box<Stm>),
    AssignStm(String, Box<Exp>),
    PrintStm(Box<ExpList>),
}
impl Stm {}

type Number = i32;

pub enum Exp {
    NumExp(Number),
    OpExp(Box<Exp>, Binop, Box<Exp>),
    EseqExp(Box<Stm>, Box<Exp>),
    IdExp(String),
}

impl Exp {}

pub enum ExpList {
    PairExpList(Box<Exp>, Box<ExpList>),
    LastExpList(Box<Exp>),
}
impl ExpList {

    fn to_string(&self, vars: &mut HashMap<String, Number>) -> String {
        match self {
            ExpList::PairExpList(exp, exp_list) => {
                format!("{} {}", ExecVisitor::visit_exp(exp, vars), exp_list.to_string(vars))
            },
            ExpList::LastExpList(exp) => {
                format!("{}", ExecVisitor::visit_exp(exp, vars))
            }
        }
    }
}

#[derive(PartialEq)]
pub enum Binop {
    Plus,
    Minus,
    Times,
    Div,
}
impl Binop {
    fn cmp_rank(&self, other: &Binop) -> i8 {
        let rank1 = match *self {
            Binop::Plus | Binop::Minus => 1,
            Binop::Times | Binop::Div  => 2,
        };
        let rank2 = match *other {
            Binop::Plus | Binop::Minus => 1,
            Binop::Times | Binop::Div  => 2,
        };
        rank1 - rank2
    }
}
impl Display for Binop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Binop::Plus =>  "+",
            Binop::Minus => "-",
            Binop::Times => "*",
            Binop::Div =>   "/"
        })
    }
}

trait Visitor<I, R> {
    fn visit_stm(stm: &Box<Stm>, inh: I) -> R;
    fn visit_exp(exp: &Box<Exp>, inh: I) -> R;
    fn visit_exp_list(exp_list: &Box<ExpList>, inh: I) -> R;
}

struct LeafCounterVisitor {}
impl Visitor<(), i32> for LeafCounterVisitor {
    fn visit_stm(stm: &Box<Stm>, _: ()) -> i32 {
        match **stm {
            Stm::CompoundStm(ref stm1, ref stm2) => {
                Self::visit_stm(stm1, ()) + Self::visit_stm(stm2, ())
            },
            Stm::AssignStm(_, ref exp) => {
                1 + Self::visit_exp(exp, ())
            },
            Stm::PrintStm(ref exp_list) => {
                Self::visit_exp_list(exp_list, ())
            },
        }
    }

    fn visit_exp(exp: &Box<Exp>, _: ()) -> i32 {
        match **exp {
            Exp::NumExp(_) => {
                1
            },
            Exp::OpExp(ref exp1, _, ref exp2) => {
                Self::visit_exp(exp1, ()) + Self::visit_exp(exp2, ())
            },
            Exp::EseqExp(ref stm, ref exp) => {
                Self::visit_stm(stm, ()) + Self::visit_exp(exp, ())
            },
            Exp::IdExp(_) => {
                1
            },
        }
    }

    fn visit_exp_list(exp_list: &Box<ExpList>, _: ()) -> i32 {
        match **exp_list {
            ExpList::PairExpList(ref exp, ref exp_list) => {
                Self::visit_exp(exp, ()) + Self::visit_exp_list(exp_list, ())
            },
            ExpList::LastExpList(ref exp) => {
                Self::visit_exp(exp, ())
            },
        }
    }
}

struct PrintVisitor {}
impl PrintVisitor {
    const DEFAULT_INDENT: i32 = 4;
    fn print(stm: &Box<Stm>) {
        Self::visit_stm(stm, 0);
        println!();
    }
    
    fn get_indent(indent: i32) -> String {
        let mut retval = "".to_string();
        for _ in 0..indent {
            retval = retval + " ";
        }
        retval
    }
}
impl Visitor<i32, ()> for PrintVisitor {
    
    fn visit_stm(stm: &Box<Stm>, indent: i32) -> () {
        let indent_str = Self::get_indent(indent);
        let plus_1 = Self::get_indent(indent + Self::DEFAULT_INDENT);
        match **stm {
            Stm::CompoundStm(ref stm1, ref stm2) => {
                println!("CompoundStm {{");
                print!("{}stm1: ", plus_1);
                // stm1.print_structure_indented(indent + Self::DEFAULT_INDENT);
                Self::visit_stm(&stm1, indent + Self::DEFAULT_INDENT);
                println!(",");
                print!("{}stm2: ", plus_1);
                // stm2.print_structure_indented(indent + Self::DEFAULT_INDENT);
                Self::visit_stm(&stm2, indent + Self::DEFAULT_INDENT);
                print!("\n{}}}", indent_str);
            },
            Stm::AssignStm(ref id, ref exp) => {
                println!("AssignStm {{");
                println!("{}id: {},", plus_1, id);
                print!("{}exp: ", plus_1);
                // exp.print_structure_indented(indent + Self::DEFAULT_INDENT);
                Self::visit_exp(&exp, indent + Self::DEFAULT_INDENT);
                print!("\n{}}}", indent_str);
            },
            Stm::PrintStm(ref exp_list) => {
                println!("PrintStm {{");
                print!("{}exp_list: ", plus_1);
                // exp_list.print_structure_indented(indent + Self::DEFAULT_INDENT);
                Self::visit_exp_list(&exp_list, indent + Self::DEFAULT_INDENT);
                print!("\n{}}}", indent_str);
            },
        }
    }
    fn visit_exp(exp: &Box<Exp>, indent: i32) -> () {
        let indent_str = Self::get_indent(indent);
        let plus_1 = Self::get_indent(indent + Self::DEFAULT_INDENT);
        match **exp {
            Exp::NumExp(num) => {
                print!("NumExp {{ {} }}", num);
            },
            Exp::OpExp(ref exp1, ref binop, ref exp2) => {
                Self::visit_exp(exp1, indent + Self::DEFAULT_INDENT);
                print!(" {} ", binop);
                Self::visit_exp(exp2, indent + Self::DEFAULT_INDENT);
            },
            Exp::EseqExp(ref stm, ref exp) => {
                println!("EseqExp {{");
                print!("{}stm: ", plus_1);
                // stm.print_structure_indented(indent + Self::DEFAULT_INDENT);
                Self::visit_stm(stm, indent + Self::DEFAULT_INDENT);
                println!(",");
                print!("{}exp: ", plus_1);
                // exp.print_structure_indented(indent + Self::DEFAULT_INDENT);
                Self::visit_exp(exp, indent + Self::DEFAULT_INDENT);
                print!("\n{}}}", indent_str);
            },
            Exp::IdExp(ref id) => {
                print!("IdExp {{ {} }}", id);
            },
        }
    }

    fn visit_exp_list(exp_list: &Box<ExpList>, indent: i32) -> () {
        let indent_str = Self::get_indent(indent);
        let plus_1 = Self::get_indent(indent + Self::DEFAULT_INDENT);
        match **exp_list {
            ExpList::PairExpList(ref exp, ref exp_list) => {
                println!("PairExpList {{");
                print!("{}exp: ", plus_1);
                // exp.print_structure_indented(indent + Self::DEFAULT_INDENT);
                Self::visit_exp(exp, indent + Self::DEFAULT_INDENT);
                println!(",");
                print!("{}exp_list: ", plus_1);
                // exp_list.print_structure_indented(indent + Self::DEFAULT_INDENT);
                Self::visit_exp_list(exp_list, indent + Self::DEFAULT_INDENT);
                print!("\n{}}}", indent_str);
            },
            ExpList::LastExpList(ref exp) => {
                println!("LastExpList {{");
                print!("{}exp: ", plus_1);
                // exp.print_structure_indented(indent + Self::DEFAULT_INDENT);
                Self::visit_exp(exp, indent + Self::DEFAULT_INDENT);
                print!("\n{}}}", indent_str);
            },
        }
    }
}

struct ProgPrintVisitor {}
impl ProgPrintVisitor {
    const DEFAULT_INDENT: i32 = 4;
    fn print(stm: &Box<Stm>) {
        Self::visit_stm(stm, 0);
        println!();
    }
    
    fn get_indent(indent: i32) -> String {
        let mut retval = "".to_string();
        for _ in 0..indent {
            retval = retval + " ";
        }
        retval
    }
}
impl Visitor<i32, ()> for ProgPrintVisitor {
    
    fn visit_stm(stm: &Box<Stm>, indent: i32) -> () {
        let indent_str = Self::get_indent(indent);
        match **stm {
            Stm::CompoundStm(ref stm1, ref stm2) => {
                Self::visit_stm(&stm1, indent);
                print!(";\n{}", indent_str);
                Self::visit_stm(&stm2, indent);
            },
            Stm::AssignStm(ref id, ref exp) => {
                print!("{} := ", id);
                Self::visit_exp(exp, indent + Self::DEFAULT_INDENT);
            },
            Stm::PrintStm(ref exp_list) => {
                print!("print(");
                Self::visit_exp_list(exp_list, indent);
                print!(")");
            },
        }
    }

    fn visit_exp(exp: &Box<Exp>, indent: i32) -> () {
        let indent_str = Self::get_indent(indent);
        let plus_1 = Self::get_indent(indent + Self::DEFAULT_INDENT);
        match **exp {
            Exp::NumExp(num) => {
                print!("{}", num);
            },
            Exp::OpExp(ref exp1, ref binop, ref exp2) => {
                let mut parentheses = false;
                if let Exp::OpExp(_, ref binop2, _) = **exp1 {
                    if binop.cmp_rank(binop2) > 0 {
                        parentheses = true;
                    }
                }
                if parentheses { print!("(") };
                Self::visit_exp(exp1, indent + Self::DEFAULT_INDENT);
                if parentheses { print!(")") };
                print!(" {} ", binop);
                parentheses = false;
                if let Exp::OpExp(_, ref binop2, _) = **exp2 {
                    let rnk_cmp = binop.cmp_rank(binop2);
                    if *binop == Binop::Div ||
                            (rnk_cmp > 0) ||
                            (rnk_cmp == 0 && *binop == Binop::Minus) ||
                            (*binop == Binop::Times && *binop2 == Binop::Div) {
                        parentheses = true;
                    }
                }
                if parentheses { print!("(") };
                Self::visit_exp(exp2, indent + Self::DEFAULT_INDENT);
                if parentheses { print!(")") };
            },
            Exp::EseqExp(ref stm, ref exp) => {
                print!("(\n{}", plus_1);
                Self::visit_stm(stm, indent + Self::DEFAULT_INDENT);
                print!(",\n{}", plus_1);
                Self::visit_exp(exp, indent + Self::DEFAULT_INDENT);
                print!("\n{})", indent_str);
            },
            Exp::IdExp(ref id) => {
                print!("{}", id);
            },
        }
    }

    // ExpLists are only used in print Stms. Do not print the 
    // parenthises here. They will be printed by the print Stm.
    fn visit_exp_list(exp_list: &Box<ExpList>, indent: i32) -> () {
        match **exp_list {
            ExpList::PairExpList(ref exp, ref exp_list) => {
                Self::visit_exp(exp, indent + Self::DEFAULT_INDENT);
                print!(", ");
                Self::visit_exp_list(exp_list, indent);
            },
            ExpList::LastExpList(ref exp) => {
                Self::visit_exp(exp, indent + Self::DEFAULT_INDENT);
            },
        }
    }

}

struct ExecVisitor {}
impl ExecVisitor {
    fn exec(stm: &Box<Stm>) -> HashMap<String, Number>{
        let mut map = HashMap::new();
        Self::visit_stm(stm, &mut map);
        map
    }
}
impl<'a> Visitor<&mut HashMap<String, Number>, Number> for ExecVisitor {
    fn visit_stm(stm: &Box<Stm>, vars: &mut HashMap<String, Number>) -> Number {
        match **stm {
            Stm::CompoundStm(ref stm1, ref stm2) => {
                // stm1.exec(vars);
                // stm2.exec(vars);
                Self::visit_stm(stm1, vars);
                Self::visit_stm(stm2, vars);
            },
            Stm::AssignStm(ref id, ref exp) => {
                // let val = exp.eval(vars);
                let val = Self::visit_exp(exp, vars);
                vars.insert(id.clone(), val);
                println!("[{} = {}]", id, val);
            },
            Stm::PrintStm(ref exp_list) => {
                println!("Printing: {}", exp_list.to_string(vars));
            },
        }
        0
    }

    fn visit_exp(exp: &Box<Exp>, vars: &mut HashMap<String, Number>) -> Number {
        match **exp {
            Exp::NumExp(num) => num,
            Exp::OpExp(ref exp1, ref binop, ref exp2) => match binop {
                // Binop::Plus =>  exp1.eval(vars) + exp2.eval(vars),
                // Binop::Minus => exp1.eval(vars) - exp2.eval(vars),
                // Binop::Times => exp1.eval(vars) * exp2.eval(vars),
                // Binop::Div =>   exp1.eval(vars) / exp2.eval(vars),
                Binop::Plus =>  Self::visit_exp(exp1, vars) + Self::visit_exp(exp2, vars),
                Binop::Minus => Self::visit_exp(exp1, vars) - Self::visit_exp(exp2, vars),
                Binop::Times => Self::visit_exp(exp1, vars) * Self::visit_exp(exp2, vars),
                Binop::Div =>   Self::visit_exp(exp1, vars) / Self::visit_exp(exp2, vars),
            },
            Exp::EseqExp(ref stm, ref exp) => {
                // stm.exec(vars);
                // exp.eval(vars)
                Self::visit_stm(stm, vars);
                Self::visit_exp(exp, vars)
            },
            Exp::IdExp(ref id) => {
                match vars.get(id) {
                    Some(val) => *val,
                    None => {
                        println!("Illegal variable lookup {} returning 0.", id);
                        0
                    }
                }
            },
        }
    }

    fn visit_exp_list(_: &Box<ExpList>, _: &mut HashMap<String, Number>) -> Number {
        println!("Cannot execute ExpList!");
        0
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test(){
        // Program: 
        // a := 3;
        // b := a * 4;
        // c := (print(b), b+a);
        // d := (e := 5, (print((print(e),6),(f := e / 7, e-f)), 8))

        let prog: Box<Stm> = Box::new(Stm::CompoundStm(
            Box::new(Stm::AssignStm(
                "a".to_string(),
                Box::new(Exp::NumExp(3))
            )),
            Box::new(Stm::CompoundStm(
                Box::new(Stm::AssignStm(
                    "b".to_string(), 
                    Box::new(Exp::OpExp(
                        Box::new(Exp::IdExp("a".to_string())),
                        Binop::Times,
                        Box::new(Exp::NumExp(4))
                    ))
                )),
                Box::new(Stm::CompoundStm(
                    Box::new(Stm::AssignStm(
                        "c".to_string(),
                        Box::new(Exp::EseqExp(
                            Box::new(Stm::PrintStm(
                                Box::new(ExpList::LastExpList(
                                    Box::new(Exp::IdExp("b".to_string())))
                                )
                            )),
                            Box::new(Exp::OpExp(
                                Box::new(Exp::IdExp("b".to_string())),
                                Binop::Plus,
                                Box::new(Exp::IdExp("a".to_string()))
                            ))
                        ))
                    )),
                    Box::new(Stm::AssignStm(
                        "d".to_string(),
                        Box::new(Exp::EseqExp(
                            Box::new(Stm::AssignStm(
                                "e".to_string(),
                                Box::new(Exp::NumExp(5))
                            )),
                            Box::new(Exp::EseqExp(
                                Box::new(Stm::PrintStm(
                                    Box::new(ExpList::PairExpList(
                                        Box::new(Exp::EseqExp(
                                            Box::new(Stm::PrintStm(
                                                Box::new(ExpList::LastExpList(
                                                    Box::new(Exp::IdExp("e".to_string()))
                                                ))
                                            )),
                                            Box::new(Exp::NumExp(6))
                                        )),
                                        Box::new(ExpList::LastExpList(
                                            Box::new(Exp::EseqExp(
                                                Box::new(Stm::AssignStm(
                                                    "f".to_string(),
                                                    Box::new(Exp::OpExp(
                                                        Box::new(Exp::IdExp("e".to_string())),
                                                        Binop::Div,
                                                        Box::new(Exp::NumExp(7))
                                                    ))
                                                )),
                                                Box::new(Exp::OpExp(
                                                    Box::new(Exp::IdExp("e".to_string())),
                                                    Binop::Minus,
                                                    Box::new(Exp::IdExp("f".to_string()))
                                                ))
                                            ))
                                        ))
                                    ))
                                )),
                                Box::new(Exp::NumExp(8))
                            ))
                        ))
                    ))
                ))
            ))
        ));
        println!("Program structure:");
        // prog.print_structure();
        PrintVisitor::print(&prog);
        println!("Pretty print:");
        ProgPrintVisitor::print(&prog);
        println!("Program execution:");
        // let mut vars = HashMap::<String, Number>::new();
        // prog.exec(&mut vars);
        let mut _vars = ExecVisitor::exec(&prog);
        let num_leaves = LeafCounterVisitor::visit_stm(&prog, ());
        println!("There are {} leaves.", num_leaves);
    }
}
