use crate::slp::*;


impl std::fmt::Display for SLPTerminal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", 
            match self {
                SLPTerminal::IdGen =>       "id ".to_string(),
                SLPTerminal::NumGen =>      "num ".to_string(),
                SLPTerminal::OperatorGen => "B".to_string(),
                SLPTerminal::Id(id) =>      id.to_string(),
                SLPTerminal::Num(num) =>    format!("{}", num),
                SLPTerminal::Operator(b) => b.to_string(),
                SLPTerminal::Print =>       "print(".to_string(),
                SLPTerminal::Semicolon =>   "; ".to_string(),
                SLPTerminal::Assign =>      ":= ".to_string(),
                SLPTerminal::LeftParen =>   "(".to_string(),
                SLPTerminal::RightParen =>  ")".to_string(),
                SLPTerminal::Comma =>       ", ".to_string(),
                SLPTerminal::EOF =>         "$".to_string(),
                SLPTerminal::NewLine =>     "\\n".to_string(),
                SLPTerminal::Error =>       "Error".to_string(),
                SLPTerminal::Whitespace =>  "".to_string(),
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