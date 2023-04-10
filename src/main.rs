
// pub mod slp_names;
// pub mod lexer;
// use crate::lexer::*;
extern crate proc_macros;

pub mod grammar;
pub mod slp;
pub mod bud;

pub mod slp_symbols;
use logos::Logos;

use crate::grammar::*;
use crate::slp::*;

use std::env;
use std::fs::File;
use std::io::Read;

pub mod parse;
use crate::parse::*;

use colored::Colorize;



fn main() {

    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} filename.slp", args[0]);
        return;
    }
    let mut file = match File::open(args[1].as_str()) {
        Ok(x) => { x },
        Err(x) => { 
            println!("IO Error: {}", x);
            return;
        }
    };
    let mut contents = String::new();
    match file.read_to_string(&mut contents) {
        Ok(_) => {},
        Err(err) => {
            println!("{}", err);
            return; 
        },
    }

    use slp::SLPNonTerminal::*;
    use slp::SLPTerminal::*;
    use grammar::Symbol::*;
    // let mut g : Grammar<SLPTerminal, SLPNonTerminal> = Grammar::new(
    //     vec![
    //         Der{ from: Start,    to: vec![NonTm(Stm), Tm(EOF)] },
    //         Der{ from: Stm,      to: vec![NonTm(Stm), Tm(Semicolon), NonTm(Stm)] },
    //         Der{ from: Stm,      to: vec![Tm(IdGen), Tm(Assign), NonTm(Exp)] },
    //         Der{ from: Stm,      to: vec![Tm(Print), NonTm(ExpList), Tm(RightParen)] },
    //         Der{ from: Exp,      to: vec![Tm(IdGen)] },
    //         Der{ from: Exp,      to: vec![Tm(NumGen)] },
    //         Der{ from: Exp,      to: vec![NonTm(Exp), Tm(OperatorGen), NonTm(Exp)] },
    //         Der{ from: Exp,      to: vec![Tm(LeftParen), NonTm(Stm), Tm(Comma), NonTm(Exp), Tm(RightParen)] },
    //         Der{ from: ExpList,  to: vec![NonTm(Exp), Tm(Comma), NonTm(ExpList)] },
    //         Der{ from: ExpList,  to: vec![NonTm(Exp)] },
    //     ],
    // );
    let mut g : Grammar<SLPTerminal, SLPNonTerminal> = Grammar::new(
        vec![
            Der{ from: Start,    to: vec![NonTm(Stm), Tm(EOF)] },
            Der{ from: Stm,      to: vec![NonTm(Stm), Tm(Semicolon), NonTm(Stm)] },
            Der{ from: Stm,      to: vec![Tm(IdGen), Tm(Assign), NonTm(Exp)] },
            Der{ from: Stm,      to: vec![Tm(Print), NonTm(ExpList), Tm(RightParen)] },
            Der{ from: Exp,      to: vec![Tm(IdGen)] },
            Der{ from: Exp,      to: vec![Tm(NumGen)] },
            Der{ from: Exp,      to: vec![NonTm(Exp), Tm(OperatorGen), NonTm(Exp)] },
            Der{ from: Exp,      to: vec![Tm(LeftParen), NonTm(Stm), Tm(Comma), NonTm(Exp), Tm(RightParen)] },
            Der{ from: ExpList,  to: vec![NonTm(Exp), Tm(Comma), NonTm(ExpList)] },
            Der{ from: ExpList,  to: vec![NonTm(Exp)] },
        ],
    );
    print_grammar(&g);

    let states;
    match lr1_generate(&mut g, Start, 
            vec![Start, Stm, Exp, ExpList],
            vec![IdGen, Print, NumGen, Semicolon, Assign, LeftParen, RightParen, Comma, OperatorGen, EOF]
        ) {
        Ok(s)  => {
            states = s;
        },
        Err(msg) => {
            println!("{}", msg.yellow());
            return;
        }
    };
    let mut lex = SLPTerminal::lexer(&contents);
    match lr1_parse(&states, &mut lex, EOF, Error, load_slp_data) {
        Ok(node_stack) => {
            print_tree_visitor(&node_stack[0], 0);
            println!("");
        }
        Err(msg) => {
            println!("{}", msg.yellow());
            return;
        }
    }
}
