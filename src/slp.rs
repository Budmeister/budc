use logos::Logos;

#[derive(Logos, Debug, PartialEq, Eq, Clone, Hash)]
pub enum SLPTerminal {

    #[regex(r"[a-zA-Z]")]
    Id,
    #[token("print(")]
    Print,
    #[regex("[0-9]+")]
    Num,
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
    #[regex(r"[\+\-\*/]")]
    Operator,
    #[token("\n")]
    NewLine,
    EOF,

    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    #[error]
    // We can also use this variant to define whitespace,
    // or any other matches we wish to skip.
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}


#[derive(PartialEq, Eq, Clone, Hash)]
pub enum SLPNonTerminal {
    Start,
    Stm,
    Exp,
    ExpList,
}


pub struct SLPToken<T> {
    pub token: T,
    pub lineno: u32,
}
