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
        #[token(",")]
        Comma,
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
        #[token("struct")]
        Struct,
        #[regex(r"[_a-zA-Z]([_a-zA-Z0-9])*")]
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
        // We can also use this variant to define whitespace,
        // or any other matches we wish to skip.
        #[regex(r"[ \t\n\r\f]+", logos::skip)]
        Error,
        EOF,
    },
    BudNonTerminal: {
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
        VarDeclAssgn,
        Expr,
        TypeExpr,
        Expr2,
        Exprs,
        NonBinExpr,
        BlockExpr,
        AssignExpr,
        ReturnExpr,
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
        Binop
    },
    get_bud_grammar: {
        // Program
        Start   => Items;
        Item    => FuncDecl;
        Item    => StructDecl;
        Item    => ImportDecl;
        FuncDecl    => VarDecl, VarDeclsPar, Expr;
        StructDecl  => Struct, IdGen, LeftSquiggly, VarDecls, RightSquiggly;
        VarDecl     => IdExpr, IdGen;

        // Lists
        Items   => Item, Items;
        Items   => Item;

        Exprs   => Expr, Comma, Exprs;
        Exprs   => Expr;
        Args    => LeftRound, RightRound;
        Args    => LeftRound, Exprs, RightRound;

        VarDecls => VarDecl, Comma, VarDecl;
        VarDecls => VarDecl;
        VarDeclsPar => LeftRound, RightRound;
        VarDeclsPar => LeftRound, VarDecls, RightRound;


        // Statements
        Expr2   => Expr, Expr2;
        Expr2   => Expr;
            // 2 Expressions in a row will sometimes pass the
            // grammar checker (and should be allowed, since statements
            // are expressions), but if the first is not a statement
            // expression, it will be caught by the expander.

        // Expressions
        Expr    => BinaryExpr;     // Since NonBinExpr work as BinaryExpr
        Expr    => Expr, Semicolon;

        NonBinExpr  => BlockExpr;
        NonBinExpr  => AssignExpr;
        NonBinExpr  => VarDeclAssgn;
        NonBinExpr  => ReturnExpr;
        NonBinExpr  => IdExpr;
        NonBinExpr  => LitExpr;
        NonBinExpr  => ParenExpr;
        NonBinExpr  => UnaryExpr;
        NonBinExpr  => IfExpr;
        NonBinExpr  => IfElse;
        NonBinExpr  => UnlExpr;
        NonBinExpr  => UnlElse;
        NonBinExpr  => WhileExpr;
        NonBinExpr  => DoWhile;

        TypeExpr    => IdGen;
        TypeExpr    => TypeExpr, LeftSquare, NumGen, RightSquare;
        TypeExpr    => Star, LeftRound, TypeExpr, RightRound;

        BlockExpr   => LeftSquiggly, Expr2, RightSquiggly;
        AssignExpr  => IdExpr, Assign, Expr;
        VarDeclAssgn=> VarDecl, Assign, Expr;
        ReturnExpr  => Return;
        ReturnExpr  => Return, Expr;
        IdExpr      => Expr, LeftSquare, Expr, RightSquare;
        IdExpr      => Expr, LeftRound, Exprs, RightRound;
        IdExpr      => IdGen;
        ParenExpr   => LeftRound, Expr, RightRound;
        UnaryExpr   => Unop, NonBinExpr;
        BinaryExpr  => NonBinExpr, Binop, BinaryExpr;
        BinaryExpr  => NonBinExpr;
        IfExpr      => If, Expr, LeftSquiggly, Expr, RightSquiggly;
        IfElse      => If, Expr, LeftSquiggly, Expr, RightSquiggly, Else, LeftSquiggly, Expr, RightSquiggly;
        UnlExpr     => Unless, Expr, LeftSquiggly, Expr, RightSquiggly;
        UnlElse     => Unless, Expr, LeftSquiggly, Expr, RightSquiggly, Else, LeftSquiggly, Expr, RightSquiggly;
        WhileExpr   => While, Expr, LeftSquiggly, Expr, RightSquiggly;
        DoWhile     => Do, LeftSquiggly, Expr, RightSquiggly, While, Expr, Semicolon;

        // Literals
        LitExpr     => NumGen;
        LitExpr     => StrGen;
        LitExpr     => CharGen;
        NonBinExpr  => Break;
        NonBinExpr  => Continue;

        // Operators
        Binop       => Plus;
        Binop       => Minus;
        Binop       => Star;
        Binop       => Div;
        Binop       => And;
        Binop       => Or;
        Binop       => Ambersand;
        Binop       => BitOr;
        Binop       => BitXor;
        Binop       => Equal;
        Binop       => NotEq;
        Binop       => Greater;
        Binop       => GrtrEq;
        Binop       => Less;
        Binop       => LessEq;
        Unop        => NotEq;
        Unop        => Star;
        Unop        => Ambersand;
        Unop        => Minus;

    }
);

pub fn load_bud_data(t: BudTerminal, slice: &str) -> BudTerminal {
    match t {
        BudTerminal::IdGen => BudTerminal::Id(slice.to_string()),
        BudTerminal::NumGen => BudTerminal::Num(if let Ok(num) = slice.parse() {
            num
        } else {
            panic!("Invalid number: {}", slice)
        }),
        BudTerminal::StrGen => BudTerminal::Str(slice.to_string()),
        BudTerminal::CharGen => BudTerminal::Char(slice.to_string()),
        _ => t,
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
                BudUnop::Not => "!",
                BudUnop::Neg => "-",
                BudUnop::Deref => "*",
                BudUnop::Ref => "&",
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
        write!(
            f,
            "{}",
            match self {
                BudTerminal::NumGen => "num".to_string(),
                BudTerminal::StrGen => "str".to_string(),
                BudTerminal::CharGen => "char".to_string(),
                BudTerminal::LeftRound => "(".to_string(),
                BudTerminal::RightRound => ")".to_string(),
                BudTerminal::LeftSquiggly => "{".to_string(),
                BudTerminal::RightSquiggly => "}".to_string(),
                BudTerminal::LeftSquare => "[".to_string(),
                BudTerminal::RightSquare => "]".to_string(),
                BudTerminal::Semicolon => ";".to_string(),
                BudTerminal::Comma => ",".to_string(),
                BudTerminal::Assign => "=".to_string(),
                BudTerminal::Num(n) => n.to_string(),
                BudTerminal::Str(s) => s.to_string(),
                BudTerminal::Char(c) => c.to_string(),
                BudTerminal::Id(id) => id.to_string(),
                BudTerminal::If => "if".to_string(),
                BudTerminal::Unless => "unless".to_string(),
                BudTerminal::Else => "else".to_string(),
                BudTerminal::Import => "import".to_string(),
                BudTerminal::Return => "return".to_string(),
                BudTerminal::Do => "do".to_string(),
                BudTerminal::While => "while".to_string(),
                BudTerminal::Break => "break".to_string(),
                BudTerminal::Continue => "continue".to_string(),
                BudTerminal::Struct => "struct".to_string(),
                BudTerminal::IdGen => "id".to_string(),
                BudTerminal::Plus => "+".to_string(),
                BudTerminal::Minus => "-".to_string(),
                BudTerminal::Star => "*".to_string(),
                BudTerminal::Div => "/".to_string(),
                BudTerminal::And => "&&".to_string(),
                BudTerminal::Or => "||".to_string(),
                BudTerminal::Ambersand => "&".to_string(),
                BudTerminal::BitOr => "|".to_string(),
                BudTerminal::BitXor => "^".to_string(),
                BudTerminal::Equal => "==".to_string(),
                BudTerminal::NotEq => "!=".to_string(),
                BudTerminal::Greater => ">".to_string(),
                BudTerminal::GrtrEq => ">=".to_string(),
                BudTerminal::Less => "<".to_string(),
                BudTerminal::LessEq => "<=".to_string(),
                BudTerminal::Not => "!".to_string(),
                BudTerminal::Error => "ERROR".to_string(),
                BudTerminal::EOF => "EOF".to_string(),
            }
        )
    }
}

impl std::fmt::Display for BudNonTerminal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BudNonTerminal::Start => "S'",
                BudNonTerminal::Items => "Is",
                BudNonTerminal::Item => "I",
                BudNonTerminal::FuncDecl => "F",
                BudNonTerminal::StructDecl => "S",
                BudNonTerminal::ImportDecl => "Im",
                BudNonTerminal::Args => "As",
                BudNonTerminal::VarDeclsPar => "Vsp",
                BudNonTerminal::VarDecls => "Vs",
                BudNonTerminal::VarDecl => "V",
                BudNonTerminal::Expr => "E",
                BudNonTerminal::TypeExpr => "Te",
                BudNonTerminal::Expr2 => "E2",
                BudNonTerminal::Exprs => "Es",
                BudNonTerminal::NonBinExpr => "Nbe",
                BudNonTerminal::BlockExpr => "Ble",
                BudNonTerminal::VarDeclAssgn => "Va",
                BudNonTerminal::AssignExpr => "Ae",
                BudNonTerminal::ReturnExpr => "Re",
                BudNonTerminal::IdExpr => "Ie",
                BudNonTerminal::LitExpr => "Le",
                BudNonTerminal::ParenExpr => "Pe",
                BudNonTerminal::UnaryExpr => "Ue",
                BudNonTerminal::BinaryExpr => "Be",
                BudNonTerminal::IfExpr => "If",
                BudNonTerminal::IfElse => "Ife",
                BudNonTerminal::UnlExpr => "Ul",
                BudNonTerminal::UnlElse => "Ule",
                BudNonTerminal::WhileExpr => "We",
                BudNonTerminal::DoWhile => "Dw",
                BudNonTerminal::Unop => "U",
                BudNonTerminal::Binop => "B",
            }
        )
    }
}
