use crate::slp::*;


impl std::fmt::Display for SLPTerminal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", 
            match self {
                SLPTerminal::Id =>         "id ",
                SLPTerminal::Print =>      "print(",
                SLPTerminal::Num =>        "num ",
                SLPTerminal::Semicolon =>  "; ",
                SLPTerminal::Assign =>     ":= ",
                SLPTerminal::LeftParen =>  "(",
                SLPTerminal::RightParen => ")",
                SLPTerminal::Comma =>      ", ",
                SLPTerminal::Operator =>   "B",
                SLPTerminal::EOF =>        "$",
                SLPTerminal::NewLine =>    "\\n",
                SLPTerminal::Error =>      "Error",
            }
        )

    }
}

impl std::fmt::Display for SLPNonTerminal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", 
            match self {
                SLPNonTerminal::Start =>    "S'",
                SLPNonTerminal::Stm =>      "S",
                SLPNonTerminal::Exp =>      "E",
                SLPNonTerminal::ExpList =>  "L",
            }
        )

    }
}