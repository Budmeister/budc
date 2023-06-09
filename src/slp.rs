use std::fmt::{Display, Debug};

use logos::Logos;


pub fn load_slp_data(t: SLPTerminal, slice: &str) -> SLPTerminal {
    match t {
        SLPTerminal::IdGen => SLPTerminal::Id(slice.to_string()),
        SLPTerminal::NumGen => SLPTerminal::Num(
            if let Ok(num) = slice.parse() {
                num
            } else {
                panic!("Invalid number: {}", slice)
            }
        ),
        SLPTerminal::OperatorGen => {
            match slice {
                "+" => SLPTerminal::Operator(Binop::Plus),
                "-" => SLPTerminal::Operator(Binop::Minus),
                "*" => SLPTerminal::Operator(Binop::Times),
                "/" => SLPTerminal::Operator(Binop::Div),
                _ => panic!("Invalid operator: {}", slice)
            }
        }
        _ => t
    }
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum Binop {
    Plus,
    Minus,
    Times,
    Div,
}
impl Display for Binop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Binop::Plus =>  "+",
            Binop::Minus => "-",
            Binop::Times => "*",
            Binop::Div =>   "/",
        })
    }
}
impl Debug for Binop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Logos, Debug, PartialEq, Eq, Clone, Hash)]
pub enum SLPTerminal {

    // The generic forms of Id and Num
    #[regex(r"[a-zA-Z]")]
    // , |lex| lex.slice().to_string()
    IdGen,
    #[regex("[0-9]+")]
    NumGen,
    #[regex(r"[\+\-\*/]")]
    OperatorGen,
    
    // The forms of Id and Num with information attached    
    Id(String),
    Num(i32),
    Operator(Binop),
    
    #[token("print(")]
    Print,
    #[token(";")]
    Semicolon,
    #[token(":=")]
    Assign,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token(",")]
    Comma,
    #[token("\n")]
    NewLine,
    EOF,

    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    #[error]
    Error,
    // We can also use this variant to define whitespace,
    // or any other matches we wish to skip.
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Whitespace,
}


#[derive(PartialEq, Eq, Clone, Hash)]
pub enum SLPNonTerminal {
    Start,
    Stm,
    Exp,
    ExpList,
}

