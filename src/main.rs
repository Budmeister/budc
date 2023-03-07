
// pub mod slp_names;
// pub mod lexer;
// use crate::lexer::*;

pub mod grammar;
pub mod slp;

pub mod slp_symbols;
use logos::Logos;

use crate::grammar::*;
use crate::slp::*;

use std::env;
use std::fs::File;
use std::io::Read;

pub mod parse;
use crate::parse::*;



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
    let mut g : Grammar<SLPTerminal, SLPNonTerminal> = Grammar::new(
        vec![
            Der{ from: Start,    to: vec![NonTm(Stm), Tm(EOF)] },
            Der{ from: Stm,      to: vec![NonTm(Stm), Tm(Semicolon), NonTm(Stm)] },
            Der{ from: Stm,      to: vec![Tm(Id), Tm(Assign), NonTm(Exp)] },
            Der{ from: Stm,      to: vec![Tm(Print), NonTm(ExpList), Tm(RightParen)] },
            Der{ from: Exp,      to: vec![Tm(Id)] },
            Der{ from: Exp,      to: vec![Tm(Num)] },
            Der{ from: Exp,      to: vec![NonTm(Exp), Tm(Operator), NonTm(Exp)] },
            Der{ from: Exp,      to: vec![Tm(LeftParen), NonTm(Stm), Tm(Comma), NonTm(Exp), Tm(RightParen)]},
            Der{ from: ExpList,  to: vec![NonTm(Exp), Tm(Comma), NonTm(ExpList)] },
            Der{ from: ExpList,  to: vec![NonTm(Exp)] },
        ],
    );
    print_grammar(&g);

    let states;
    match lr1_generate(&mut g, Start, 
            vec![Start, Stm, Exp, ExpList],
            vec![Id, Print, Num, Semicolon, Assign, LeftParen, RightParen, Comma, Operator, EOF]
        ) {
        Ok(s)  => {
            states = s;
        },
        Err(msg) => {
            println!("{}", msg);
            return;
        }
    };
    let mut lex = SLPTerminal::lexer(&contents);
    match lr1_parse(&states, &mut lex, EOF) {
        Ok(_) => {

        }
        Err(msg) => {
            println!("{}", msg);
            return;
        }
    }
}
