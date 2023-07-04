//! Logic for handling the lexer
//! 
//! Author:     Brian Smith
//! Year:       2023

use logos::{Logos, Lexer};
use crate::slp::*;
// use crate::slp_names::*;
use std::env;
use std::fs::File;
use std::io::{prelude::*, Error};



pub fn print_lex(text: &str) {
    let mut lex = SLPTerminal::lexer(text);
    // let mut tok: Option<SLPToken>;

    while let Some(tok) = lex.next() {
        println!("<{}: {}>", tok, lex.slice());
    }
    println!("<EOF: >");
}


pub fn lex_file_arg<'a>() -> Result<Option<Lexer<'a, SLPTerminal>>, Error> {
    
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} filename.slp", args[0]);
        return Ok(None);
    }
    let mut file = File::open(args[1].as_str())?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(Some(lex::<SLPTerminal>(contents)))

}

pub fn lex<'a, L: Logos<'a, Source = str>>(text: String) -> Lexer<'a, L>
where 
    <L as Logos<'a>>::Extras: Default
{
    let lex = L::lexer(text.as_str());
    lex
}

