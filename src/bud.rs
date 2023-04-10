use std::fmt::{Debug, Display};

use crate::grammar;

grammar::grammar!(
    BudTerminal: {
        #[regex("[0-9]+")]
        NumGen,
        #[regex(r#""([^"]*)""#)]
        StrGen,
        #[regex(r"'(\\[^\n]|[^\\])'")]
        CharGen,
        #[token("(")]
        LeftRound,
        #[token(")")]
        RightRound,
        #[token("{")]
        LeftSquiggly,
        #[token("}")]
        RightSquiggly,
        #[token("[")]
        LeftSquare,
        #[token("]")]
        RightSquare,
        #[token(";")]
        Semicolon,
        #[token("=")]
        Assign,

        Num(i32),
        Str(String),
        Char(String),
        Id(String),

        #[token("if")]
        If,
        #[token("unless")]
        Unless,
        #[token("else")]
        Else,
        #[token("import")]
        Import,
        #[token("return")]
        Return,
        #[token("do")]
        Do,
        #[token("while")]
        While,
        #[token("break")]
        Break,
        #[token("continue")]
        Continue,
        #[regex(r"[a-zA-Z]")]
        IdGen,

        #[token("+")]
        Plus,
        #[token("-")]
        Minus,
        #[token("*")]
        Star,
        #[token("/")]
        Div,
        #[token("&&")]
        And,
        #[token("||")]
        Or,
        #[token("&")]
        Ambersand,
        #[token("|")]
        BitOr,
        #[token("^")]
        BitXor,
        #[token("==")]
        Equal,
        #[token("!=")]
        NotEq,
        #[token(">")]
        Greater,
        #[token(">=")]
        GrtrEq,
        #[token("<")]
        Less,
        #[token("<=")]
        LessEq,
        #[token("!")]
        Not,


        // Logos requires one token variant to handle errors,
        // it can be named anything you wish.
        #[error]
        Error,
        EOF,
        // We can also use this variant to define whitespace,
        // or any other matches we wish to skip.
        #[regex(r"[ \t\n\f]+", logos::skip)]
        Whitespace,
    },
    BudNonTerminal: {
        Start,
        Prog,
        Exp,
        Exp2,
        Exps,
        IdExp,
        TypExp,
        Args,
        Binop,
        Unop,
        Func,
        Funcs,
        Imprt,
        Imprts,
        Vss,
        Vs,
        V,
        Oc,
    },
    get_bud_grammar: {
        // Program
        Start   => Prog, EOF;
        Prog    => Imprts, Funcs;
        Prog    => Funcs;
        Func    => V, Vss, E;
        Vs      => IdExp, id;

        // Lists
        Imprts  => Imprt, Imprts;
        Imprts  => Imprt;

        Funcs   => Func, Funcs;
        Funcs   => Func;

        Exps    => Exp, Comma, Exps;
        Exps    => Exp;
        Args    => Oc;
        Args    => LeftRound, Exps, RightRound;
        Oc      => LeftRound, RightRound;

        Vs      => V, Vs;
        Vs      => V;
        Vss     => Oc;
        Vss     => LeftRound, Vs, RightRound;

        // Statements
        Exp     => LeftSquiggly, Exp2, RightSquiggly;
        Exp2    => Exp, Exp2;       // 2 expressions in a row will sometimes pass the grammar
        Exp2    => Exp;             // checker (and it should be allowed, since statements are
                                    // expressions), but if the first is not a statement 
                                    // expression, it will be caught by the expander.
        
        // Expressions
        Exp     => Exp, Semicolon;
        Exp     => IdExp, Assign, Exp;
        Exp     => V;
        Exp     => V, Assign, Exp;
        Exp     => TypExp, Args;    // Function call - This will not always be a TypeExpression.
                                    // If it is, a cast is occurring.
                                    // Sometimes it will just be an id, but the expander will 
                                    // distinguish between the two.
        Exp     => Exp, Binop, Exp;
        Exp     => Unop, Exp;
        Exp     => LeftRound, Exp, RightRound;
        Exp     => If, LeftRound, Exp, RightRound, Exp;
        Exp     => If, LeftRound, Exp, RightRound, Exp, Else, Exp;
        Exp     => Unless, LeftRound, Exp, RightRound, Exp;
        Exp     => Unless, LeftRound, Exp, RightRound, Exp, Else, Exp;
        Exp     => While, LeftRound, Exp, RightRound, Exp;
        Exp     => Do, Exp, While, LeftRound, Exp, RightRound;


        IdExp   => Exp, LeftSquare, Exp, RightSquare;   // If this is illegal (because it is not indexable), it will be caught by the expander
        IdExp   => IdGen;
        TypExp  => IdGen;
        TypExp  => TypExp, LeftSquare, NumGen, RightSquare;
        TypExp  => Star, LeftRound, TypExp, RightRound;

        // Literals
        Exp     => IdExp;
        Exp     => NumGen;
        Exp     => StrGen;
        Exp     => CharGen;
        Exp     => Break;
        Exp     => Continue;

        // Operators
        Binop   => Plus;
        Binop   => Minus;
        Binop   => Star;
        Binop   => Div;
        Binop   => And;
        Binop   => Or;
        Binop   => Ambersand;
        Binop   => BitOr;
        Binop   => BitXor;
        Binop   => Equal;
        Binop   => NotEq;
        Binop   => Greater;
        Binop   => GrtrEq;
        Binop   => Less;
        Binop   => LessEq;
        Unop    => Not;
        Unop    => Star;
        Unop    => Ambersand;
        Unop    => Minus;

    }
);

pub fn load_bud_data(t: BudTerminal, slice: &str) -> BudTerminal {
    match t {
        BudTerminal::IdGen => BudTerminal::Id(slice.to_string()),
        BudTerminal::NumGen => BudTerminal::Num(
            if let Ok(num) = slice.parse() {
                num
            } else {
                panic!("Invalid number: {}", slice)
            }
        ),
        BudTerminal::StrGen => BudTerminal::Str(slice.to_string()),
        BudTerminal::CharGen => BudTerminal::Char(slice.to_string()),
        _ => t
    }
}


#[derive(PartialEq, Eq, Clone, Hash)]
pub enum BudBinop {
    Plus,
    Minus,
    Times,
    Div,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Equal,
    NotEq,
}
impl Display for BudBinop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BudBinop::Plus => "+",
                BudBinop::Minus => "-",
                BudBinop::Times => "*",
                BudBinop::Div => "/",
                BudBinop::And => "&&",
                BudBinop::Or => "||",
                BudBinop::BitAnd => "&",
                BudBinop::BitOr => "|",
                BudBinop::BitXor => "^",
                BudBinop::Equal => "==",
                BudBinop::NotEq => "!=",
            }
        )
    }
}
impl Debug for BudBinop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum BudUnop {
    Not,
    Neg,
    Deref,
    Ref,
}
impl Display for BudUnop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BudUnop::Not =>     "!",
                BudUnop::Neg =>     "-",
                BudUnop::Deref =>   "*",
                BudUnop::Ref =>     "&",
            }
        )
    }
}
impl Debug for BudUnop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::fmt::Display for BudTerminal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", 
            match self {
                BudTerminal::NumGen         => "num".to_string(),
                BudTerminal::StrGen         => "str".to_string(),
                BudTerminal::CharGen        => "char".to_string(),
                BudTerminal::LeftRound      => "(".to_string(),
                BudTerminal::RightRound     => ")".to_string(),
                BudTerminal::LeftSquiggly   => "{".to_string(),
                BudTerminal::RightSquiggly  => "}".to_string(),
                BudTerminal::LeftSquare     => "[".to_string(),
                BudTerminal::RightSquare    => "]".to_string(),
                BudTerminal::Semicolon      => ";".to_string(),
                BudTerminal::Assign         => "=".to_string(),
                BudTerminal::Num(n)         => n.to_string(),
                BudTerminal::Str(s)         => s.to_string(),
                BudTerminal::Char(c)        => c.to_string(),
                BudTerminal::Id(id)         => id.to_string(),
                BudTerminal::If             => "if".to_string(),
                BudTerminal::Unless         => "unless".to_string(),
                BudTerminal::Else           => "else".to_string(),
                BudTerminal::Import         => "import".to_string(),
                BudTerminal::Return         => "return".to_string(),
                BudTerminal::Do             => "do".to_string(),
                BudTerminal::While          => "while".to_string(),
                BudTerminal::Break          => "break".to_string(),
                BudTerminal::Continue       => "continue".to_string(),
                BudTerminal::IdGen          => "id".to_string(),
                BudTerminal::Plus           => "+".to_string(),
                BudTerminal::Minus          => "-".to_string(),
                BudTerminal::Star           => "*".to_string(),
                BudTerminal::Div            => "/".to_string(),
                BudTerminal::And            => "&&".to_string(),
                BudTerminal::Or             => "||".to_string(),
                BudTerminal::Ambersand      => "&".to_string(),
                BudTerminal::BitOr          => "|".to_string(),
                BudTerminal::BitXor         => "^".to_string(),
                BudTerminal::Equal          => "==".to_string(),
                BudTerminal::NotEq          => "!=".to_string(),
                BudTerminal::Greater        => ">".to_string(),
                BudTerminal::GrtrEq         => ">=".to_string(),
                BudTerminal::Less           => "<".to_string(),
                BudTerminal::LessEq         => "<=".to_string(),
                BudTerminal::Not            => "!".to_string(),
                BudTerminal::Error          => "ERROR".to_string(),
                BudTerminal::EOF            => "EOF".to_string(),
                BudTerminal::Whitespace     => "WHITESPACE".to_string(),
            }
        )

    }
}

impl std::fmt::Display for BudNonTerminal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", 
            match self {
                BudNonTerminal::Start       => "S'",
                BudNonTerminal::Prog        => "P",
                BudNonTerminal::Exp         => "E",
                BudNonTerminal::Exp2        => "E2",
                BudNonTerminal::Exps        => "Es",
                BudNonTerminal::IdExp       => "Ie",
                BudNonTerminal::TypExp      => "Te",
                BudNonTerminal::Args        => "A",
                BudNonTerminal::Binop       => "B",
                BudNonTerminal::Unop        => "U",
                BudNonTerminal::Func        => "F",
                BudNonTerminal::Funcs       => "Fs",
                BudNonTerminal::Imprt       => "I",
                BudNonTerminal::Imprts      => "Is",
                BudNonTerminal::Vss         => "Vss",
                BudNonTerminal::Vs          => "Vs",
                BudNonTerminal::V           => "V",
                BudNonTerminal::Oc          => "Oc",
            }
        )

    }
}
