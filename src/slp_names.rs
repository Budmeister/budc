use crate::slp::*;


impl std::fmt::Display for SLPTerminal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", 
            match self {
                SLPTerminal::Id =>         "Id",
                SLPTerminal::Print =>      "Print",
                SLPTerminal::Num =>        "Num",
                SLPTerminal::Semicolon =>  "Semicolon",
                SLPTerminal::Assign =>     "Assign",
                SLPTerminal::LeftParen =>  "LeftParen",
                SLPTerminal::RightParen => "RightParen",
                SLPTerminal::Comma =>      "Comma",
                SLPTerminal::Operator =>   "Operator",
                SLPTerminal::EOF =>        "EOF",
                SLPTerminal::NewLine =>    "NewLine",
                SLPTerminal::Error =>      "Error",
            }
        )

    }
}

impl std::fmt::Display for SLPNonTerminal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", 
            match self {
                SLPNonTerminal::Start =>    "Start",
                SLPNonTerminal::Stm =>      "Stm",
                SLPNonTerminal::Exp =>      "Exp",
                SLPNonTerminal::ExpList =>  "ExpList",
            }
        )

    }
}