// pub mod slp_names;
// pub mod lexer;
// use crate::lexer::*;
extern crate proc_macros;

pub mod bud;
pub mod grammar;
pub mod slp;

pub mod slp_symbols;
use logos::Logos;

use crate::grammar::*;

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
        Ok(x) => x,
        Err(x) => {
            println!("IO Error: {}", x);
            return;
        }
    };
    let mut contents = String::new();
    match file.read_to_string(&mut contents) {
        Ok(_) => {}
        Err(err) => {
            println!("{}", err);
            return;
        }
    }

    use bud::BudNonTerminal::*;
    use bud::BudTerminal::*;
    let mut g = bud::get_bud_grammar();
    print_grammar(&g);

    let states;
    match lr1_generate(
        &mut g,
        Start,
        vec![
            Start,
            Items,
            Item,
            FuncDecl,
            StructDecl,
            ImportDecl,
            Args,
            VarDeclsPar,
            VarDecls,
            VarDecl,
            Expr,
            TypeExpr,
            Expr2,
            Exprs,
            NonBinExpr,
            BlockExpr,
            IdExpr,
            LitExpr,
            ParenExpr,
            UnaryExpr,
            BinaryExpr,
            IfExpr,
            IfElse,
            UnlExpr,
            UnlElse,
            WhileExpr,
            DoWhile,
            Unop,
            Binop,
        ],
        vec![
            NumGen,
            StrGen,
            CharGen,
            LeftRound,
            RightRound,
            LeftSquiggly,
            RightSquiggly,
            LeftSquare,
            RightSquare,
            Semicolon,
            Assign,
            If,
            Unless,
            Else,
            Import,
            Return,
            Do,
            While,
            Break,
            Continue,
            IdGen,
            Plus,
            Minus,
            Star,
            Div,
            And,
            Or,
            Ambersand,
            BitOr,
            BitXor,
            Equal,
            NotEq,
            Greater,
            GrtrEq,
            Less,
            LessEq,
            Not,
        ],
    ) {
        Ok(s) => {
            states = s;
        }
        Err(msg) => {
            println!("{}", msg.yellow());
            return;
        }
    };
    let mut lex = bud::BudTerminal::lexer(&contents);
    match lr1_parse(&states, &mut lex, EOF, Error, bud::load_bud_data) {
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
