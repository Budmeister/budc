//! Grammar for the Bud Programming Language
//!
//! Author:     Brian Smith
//! Year:       2023

use crate::{c_err, u_err, error::*};
use std::fmt::{Debug, Display};
use std::ops::Range;

use crate::grammar;

grammar::grammar!(
    BudTerminal: {
        #[regex("-?[0-9]+")]
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
        #[token(":")]
        Colon,
        #[token(".")]
        Dot,
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
        #[token("cleanup")]
        Cleanup,
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
        BitAnd,
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
        #[token("@")]
        Reference,


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
        Path,
        File,
        Args,
        VarDeclsPar,
        VarDecls,
        VarDecl,
        VarDeclAssgn,
        Expr,
        TypeExpr,
        Sqr,
        SqrExpr,
        Expr2,
        Exprs,
        NonBinExpr,
        BlockExpr,
        AssignExpr,
        ReturnExpr,
        CleanupCall,
        CleanupExpr,
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
        Start       => Items, EOF;
        Item        => FuncDecl;
        Item        => StructDecl;
        Item        => ImportDecl;
        FuncDecl    => VarDecl, VarDeclsPar, Expr;
        StructDecl  => Struct, IdGen, LeftSquiggly, VarDecls, RightSquiggly;
        ImportDecl  => Import, Path;
        Path        => File;
        Path        => File, Div, Path;
        Path        => IdGen, Colon, Div, Path;
        File        => IdGen;
        File        => IdGen, Dot, IdGen;
        VarDecl     => TypeExpr, IdGen;

        // Lists
        Items   => Item, Items;
        Items   => Item;

        Exprs   => Expr, Comma, Exprs;
        Exprs   => Expr;
        Args    => LeftRound, RightRound;
        Args    => LeftRound, Exprs, RightRound;

        VarDecls => VarDecl, Comma, VarDecls;
        VarDecls => VarDecl;
        VarDeclsPar => LeftRound, RightRound;
        VarDeclsPar => LeftRound, VarDecls, RightRound;

        Sqr => LeftSquare, Expr, RightSquare;
        Sqr => LeftSquare, Expr, RightSquare, Sqr;

        SqrExpr => IdGen, Sqr;


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
        NonBinExpr  => CleanupCall;
        NonBinExpr  => CleanupExpr;
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
        TypeExpr    => SqrExpr;
        TypeExpr    => Reference, LeftRound, TypeExpr, RightRound;

        BlockExpr   => LeftSquiggly, Expr2, RightSquiggly;
        AssignExpr  => IdExpr, Assign, Expr, Semicolon;
        VarDeclAssgn=> VarDecl, Assign, Expr, Semicolon;
        ReturnExpr  => Return, Semicolon;
        ReturnExpr  => Return, Expr, Semicolon;
        CleanupCall => Cleanup, Semicolon;
        CleanupExpr => Cleanup, Expr, Semicolon;
        IdExpr      => SqrExpr;
        IdExpr      => IdGen, LeftRound, Exprs, RightRound;
        IdExpr      => IdGen, LeftRound, RightRound;
        IdExpr      => IdGen;
        IdExpr      => Star, IdExpr;
        ParenExpr   => LeftRound, Expr, RightRound;
        UnaryExpr   => Unop, NonBinExpr;
        BinaryExpr  => NonBinExpr, Binop, BinaryExpr;
        BinaryExpr  => NonBinExpr;
        IfExpr      => If, Expr, BlockExpr;
        IfElse      => If, Expr, BlockExpr, Else, BlockExpr;
        UnlExpr     => Unless, Expr, BlockExpr;
        UnlElse     => Unless, Expr, BlockExpr, Else, BlockExpr;
        WhileExpr   => While, Expr, BlockExpr;
        DoWhile     => Do, BlockExpr, While, Expr, Semicolon;

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
        Binop       => BitAnd;
        Binop       => BitOr;
        Binop       => BitXor;
        Binop       => Equal;
        Binop       => NotEq;
        Binop       => Greater;
        Binop       => GrtrEq;
        Binop       => Less;
        Binop       => LessEq;
        Unop        => Not;
        Unop        => Reference;
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
    Greater,
    GrtrEq,
    Less,
    LessEq,
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
                BudBinop::Greater => ">",
                BudBinop::GrtrEq => ">=",
                BudBinop::Less => "<",
                BudBinop::LessEq => "<=",
            }
        )
    }
}
impl Debug for BudBinop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
impl BudBinop {
    pub fn new(t: BudTerminal) -> Result<BudBinop, CompilerErr> {
        match t {
            BudTerminal::Plus => Ok(BudBinop::Plus),
            BudTerminal::Minus => Ok(BudBinop::Minus),
            BudTerminal::Star => Ok(BudBinop::Times),
            BudTerminal::Div => Ok(BudBinop::Div),
            BudTerminal::And => Ok(BudBinop::And),
            BudTerminal::Or => Ok(BudBinop::Or),
            BudTerminal::BitAnd => Ok(BudBinop::BitAnd),
            BudTerminal::BitOr => Ok(BudBinop::BitOr),
            BudTerminal::BitXor => Ok(BudBinop::BitXor),
            BudTerminal::Equal => Ok(BudBinop::Equal),
            BudTerminal::NotEq => Ok(BudBinop::NotEq),
            BudTerminal::Greater => Ok(BudBinop::Greater),
            BudTerminal::GrtrEq => Ok(BudBinop::GrtrEq),
            BudTerminal::Less => Ok(BudBinop::Less),
            BudTerminal::LessEq => Ok(BudBinop::LessEq),
            _ => c_err!("Invalid binop: {}", t),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum BudUnop {
    Not,
    Neg,
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
                BudUnop::Ref => "@",
            }
        )
    }
}
impl Debug for BudUnop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
impl BudUnop {
    fn new(u: BudTerminal) -> Result<BudUnop, CompilerErr> {
        match u {
            BudTerminal::Not => Ok(BudUnop::Not),
            BudTerminal::Minus => Ok(BudUnop::Neg),
            BudTerminal::Reference => Ok(BudUnop::Ref),
            _ => c_err!("Invalid unop: {}", u),
        }
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
                BudTerminal::Colon => ":".to_string(),
                BudTerminal::Dot => ".".to_string(),
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
                BudTerminal::Cleanup => "cleanup".to_string(),
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
                BudTerminal::BitAnd => "&".to_string(),
                BudTerminal::BitOr => "|".to_string(),
                BudTerminal::BitXor => "^".to_string(),
                BudTerminal::Equal => "==".to_string(),
                BudTerminal::NotEq => "!=".to_string(),
                BudTerminal::Greater => ">".to_string(),
                BudTerminal::GrtrEq => ">=".to_string(),
                BudTerminal::Less => "<".to_string(),
                BudTerminal::LessEq => "<=".to_string(),
                BudTerminal::Not => "!".to_string(),
                BudTerminal::Reference => "@".to_string(),
                BudTerminal::Error => "ERROR".to_string(),
                BudTerminal::EOF => "$".to_string(),
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
                BudNonTerminal::Path => "P",
                BudNonTerminal::File => "Fi",
                BudNonTerminal::Args => "As",
                BudNonTerminal::VarDeclsPar => "Vsp",
                BudNonTerminal::VarDecls => "Vs",
                BudNonTerminal::VarDecl => "V",
                BudNonTerminal::Expr => "E",
                BudNonTerminal::TypeExpr => "Te",
                BudNonTerminal::Sqr => "Sq",
                BudNonTerminal::SqrExpr => "Sqe",
                BudNonTerminal::Expr2 => "E2",
                BudNonTerminal::Exprs => "Es",
                BudNonTerminal::NonBinExpr => "Nbe",
                BudNonTerminal::BlockExpr => "Ble",
                BudNonTerminal::VarDeclAssgn => "Va",
                BudNonTerminal::AssignExpr => "Ae",
                BudNonTerminal::ReturnExpr => "Re",
                BudNonTerminal::CleanupCall => "Cc",
                BudNonTerminal::CleanupExpr => "Ce",
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

use crate::parse::Node;
type BudNode = Node<BudTerminal, BudNonTerminal>;
type BudNodes = Vec<BudNode>;
type N = BudNonTerminal;
type T = BudTerminal;

// Program
#[derive(Debug)]
pub enum Item {
    FuncDecl(VarDecl, VarDecls, Box<Expr>, Range<usize>),
    StructDecl(Id, VarDecls, Range<usize>),
    ImportDecl(Path, Range<usize>),
}
pub type Items = Vec<Item>;
impl Item {
    pub fn new(children: &BudNodes, range: Range<usize>) -> Result<Item, BudErr> {
        match &children[..] {
            [Node::NonTm {
                n: N::FuncDecl,
                children,
                range,
            }] => match &children[..] {
                [Node::NonTm {
                    n: N::VarDecl,
                    children: name,
                    range,
                }, Node::NonTm {
                    n: N::VarDeclsPar,
                    children: args,
                    range: _,
                }, Node::NonTm {
                    n: N::Expr,
                    children: expr,
                    range: _,
                }] => Ok(Item::FuncDecl(
                    VarDecl::new(name, range.to_owned())?,
                    VarDecl::news(args, range.to_owned())?,
                    Box::new(Expr::new(expr, range.to_owned())?),
                    range.to_owned(),
                )),
                _ => {
                    c_err!(range, "Invalid node for {} {:?}", N::FuncDecl, children)
                }
            },
            [Node::NonTm {
                n: N::StructDecl,
                children,
                range,
            }] => match &children[..] {
                [Node::Tm {
                    t: T::Struct,
                    range: _,
                }, Node::Tm {
                    t: T::Id(id),
                    range,
                }, Node::Tm {
                    t: T::LeftSquiggly,
                    range: _,
                }, Node::NonTm {
                    n: N::VarDecls,
                    children: fields,
                    range: _,
                }, Node::Tm {
                    t: T::RightSquiggly,
                    range: _,
                }] => Ok(Item::StructDecl(
                    id.to_owned(),
                    VarDecl::news(fields, range.to_owned())?,
                    range.to_owned(),
                )),
                _ => {
                    c_err!(range, "Invalid node for {} {:?}", N::StructDecl, children)
                }
            },
            [Node::NonTm {
                n: N::ImportDecl,
                children,
                range,
            }] => match &children[..] {
                [Node::Tm {
                    t: T::Import,
                    range: _,
                }, Node::NonTm {
                    n: N::Path,
                    children: _path,
                    range,
                }] => {
                    u_err!(range, "Import statements not supported yet")
                }
                _ => {
                    c_err!(range, "Invalid node for {} {:?}", N::ImportDecl, children)
                }
            },
            _ => {
                c_err!(range, "Invalid node for {} {:?}", N::Item, children)
            }
        }
    }
    pub fn news(children: &BudNodes, range: Range<usize>) -> Result<Items, BudErr> {
        let mut children = children.clone();
        let mut items = Vec::new();
        loop {
            match &children[..] {
                [Node::NonTm {
                    n: N::Item,
                    children: i_children,
                    range,
                }, Node::NonTm {
                    n: N::Items,
                    children: is_children,
                    range: _,
                }] => {
                    items.push(Item::new(i_children, range.to_owned())?);
                    children = is_children.clone();
                }
                [Node::NonTm {
                    n: N::Item,
                    children,
                    range,
                }] => {
                    items.push(Item::new(children, range.to_owned())?);
                    break;
                }
                _ => {
                    return c_err!(range, "Invalid node for {} {:?}", N::Items, children);
                }
            }
        }
        Ok(items)
    }
}

#[derive(Debug)]
pub enum Path {
    Path(String, Range<usize>),
}
impl Path {
    pub fn new(_children: &BudNodes, range: Range<usize>) -> Result<Path, BudErr> {
        u_err!(range, "Import statements not supported yet")
    }
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub typ: TypeExpr,
    pub id: Id,
    pub range: Range<usize>,
}
pub type VarDecls = Vec<VarDecl>;
pub type Id = String;
impl VarDecl {
    pub fn new(children: &BudNodes, _range: Range<usize>) -> Result<VarDecl, BudErr> {
        match &children[..] {
            [Node::NonTm {
                n: N::TypeExpr,
                children: typ,
                range: _,
            }, Node::Tm {
                t: T::Id(id),
                range,
            }] => Ok(VarDecl {
                typ: TypeExpr::new(typ, range.to_owned())?,
                id: id.to_owned(),
                range: range.to_owned(),
            }),
            _ => {
                c_err!("Invalid node for {} {:?}", N::VarDecl, children)
            }
        }
    }
    pub fn news(children: &BudNodes, range: Range<usize>) -> Result<VarDecls, BudErr> {
        // Be able to read Vsp as well as Vs
        let mut children = children.clone();
        let mut items = Vec::new();
        loop {
            match &children[..] {
                [Node::NonTm {
                    n: N::VarDecl,
                    children: v_children,
                    range,
                }, Node::Tm {
                    t: T::Comma,
                    range: _,
                }, Node::NonTm {
                    n: N::VarDecls,
                    children: vs_children,
                    range: _,
                }] => {
                    items.push(VarDecl::new(v_children, range.to_owned())?);
                    children = vs_children.clone();
                }
                [Node::NonTm {
                    n: N::VarDecl,
                    children,
                    range,
                }] => {
                    items.push(VarDecl::new(children, range.to_owned())?);
                    break;
                }
                [Node::Tm {
                    t: T::LeftRound,
                    range: _,
                }, Node::Tm {
                    t: T::RightRound,
                    range: _,
                }] => {
                    return Ok(Vec::new());
                }
                [Node::Tm {
                    t: T::LeftRound,
                    range: _,
                }, Node::NonTm {
                    n: N::VarDecls,
                    children,
                    range,
                }, Node::Tm {
                    t: T::RightRound,
                    range: _,
                }] => {
                    return VarDecl::news(children, range.to_owned());
                }
                _ => {
                    return c_err!(range, "Invalid node for {} {:?}", N::VarDecls, children);
                }
            }
        }
        Ok(items)
    }
}

// Expressions
#[derive(Debug, Clone)]
pub struct Expr {
    pub bin_expr: Box<BinExpr>,
    pub with_semicolon: bool,
    pub range: Range<usize>,
}
pub type Exprs = Vec<Expr>;
impl Expr {
    pub fn new(children: &BudNodes, range: Range<usize>) -> Result<Expr, BudErr> {
        match &children[..] {
            [Node::NonTm {
                n: N::BinaryExpr,
                children,
                range,
            }] => Ok(Expr {
                bin_expr: Box::new(BinExpr::new(children, range.to_owned())?),
                with_semicolon: false,
                range: range.to_owned(),
            }),
            [Node::NonTm {
                n: N::Expr,
                children,
                range,
            }, Node::Tm {
                t: T::Semicolon,
                range: _,
            }] => {
                let mut expr = Expr::new(children, range.to_owned())?;
                expr.with_semicolon = true;
                Ok(expr)
            }
            _ => {
                c_err!(range, "Invalid node for {} {:?}", N::Expr, children)
            }
        }
    }
    pub fn news(children: &BudNodes, range: Range<usize>) -> Result<Exprs, BudErr> {
        // Be able to read E2 as well as Es
        let mut children = children.clone();
        let mut items = Vec::new();
        loop {
            match &children[..] {
                [Node::NonTm {
                    n: N::Expr,
                    children: e_children,
                    range,
                }, Node::Tm {
                    t: T::Comma,
                    range: _,
                }, Node::NonTm {
                    n: N::Exprs,
                    children: es_children,
                    range: _,
                }] => {
                    items.push(Expr::new(e_children, range.to_owned())?);
                    children = es_children.clone();
                }
                [Node::NonTm {
                    n: N::Expr,
                    children: expr,
                    range,
                }, Node::NonTm {
                    n: N::Expr2,
                    children: expr2,
                    range: _,
                }] => {
                    items.push(Expr::new(expr, range.to_owned())?);
                    children = expr2.clone();
                }
                [Node::NonTm {
                    n: N::Expr,
                    children: expr,
                    range,
                }] => {
                    items.push(Expr::new(expr, range.to_owned())?);
                    break;
                }
                [Node::Tm {
                    t: T::LeftSquiggly,
                    range: _,
                }, Node::NonTm {
                    n: N::Expr2,
                    children,
                    range,
                }, Node::Tm {
                    t: T::RightSquiggly,
                    range: _,
                }] => {
                    return Expr::news(children, range.to_owned());
                }
                _ => {
                    return c_err!(range, "Invalid node for {} {:?}", N::Exprs, children);
                }
            }
        }
        Ok(items)
    }
}
impl Into<Result<i32, BudErr>> for Expr {
    fn into(self) -> Result<i32, BudErr> {
        (*self.bin_expr).into()
    }
}
impl Ranged for Expr {
    fn get_range(&self) -> &Range<usize> {
        &self.range
    }
}

#[derive(Debug, Clone)]
pub enum BinExpr {
    Binary(Box<NonBinExpr>, BudBinop, Box<BinExpr>, Range<usize>),
    NonBin(Box<NonBinExpr>, Range<usize>),
}
impl BinExpr {
    pub fn new(children: &BudNodes, range: Range<usize>) -> Result<BinExpr, BudErr> {
        match &children[..] {
            [Node::NonTm {
                n: N::NonBinExpr,
                children: nbe,
                range: range1,
            }, Node::NonTm {
                n: N::Binop,
                children: b_children,
                range: rangeb,
            }, Node::NonTm {
                n: N::BinaryExpr,
                children: be,
                range: range2,
            }] => {
                let b;
                match &b_children[..] {
                    [Node::Tm {
                        t: b_t,
                        range: rangeb,
                    }] => {
                        b = b_t.clone();
                    }
                    _ => {
                        return c_err!(rangeb, "Invalid node for {} {:?}", N::Binop, children);
                    }
                }
                Ok(BinExpr::Binary(
                    Box::new(NonBinExpr::new(nbe, range1.to_owned())?),
                    BudBinop::new(b)?,
                    Box::new(BinExpr::new(be, range1.to_owned())?),
                    range,
                ))
            }
            [Node::NonTm {
                n: N::NonBinExpr,
                children: nbe,
                range: range1,
            }] => Ok(BinExpr::NonBin(
                Box::new(NonBinExpr::new(nbe, range1.to_owned())?),
                range,
            )),
            _ => {
                c_err!("Invalid node for {} {:?}", N::BinaryExpr, children)
            }
        }
    }
}
impl Into<Result<i32, BudErr>> for BinExpr {
    fn into(self) -> Result<i32, BudErr> {
        match self {
            BinExpr::NonBin(nbe, _) => (*nbe).into(),
            BinExpr::Binary(_, _, _, range) => {
                u_err!(range, "Cannot convert binary expression to num")
            }
        }
    }
}
impl Ranged for BinExpr {
    fn get_range(&self) -> &Range<usize> {
        match self {
            Self::NonBin(_, range) => range,
            Self::Binary(_, _, _, range) => range,
        }
    }
}

#[derive(Debug, Clone)]
pub enum NonBinExpr {
    BlockExpr(Exprs, Range<usize>),
    AssignExpr(Box<IdExpr>, Box<Expr>, Range<usize>),
    VarDeclAssgn(Box<VarDecl>, Box<Expr>, Range<usize>),
    ReturnExpr(Option<Box<Expr>>, Range<usize>),
    CleanupCall(Range<usize>),
    CleanupExpr(Box<Expr>, Range<usize>),
    IdExpr(Box<IdExpr>, Range<usize>),
    LitExpr(Literal, Range<usize>),
    ParenExpr(Box<Expr>, Range<usize>),
    UnaryExpr(BudUnop, Box<NonBinExpr>, Range<usize>),
    IfExpr(Box<Expr>, Box<Expr>, Range<usize>),
    IfElse(Box<Expr>, Box<Expr>, Box<Expr>, Range<usize>),
    UnlExpr(Box<Expr>, Box<Expr>, Range<usize>),
    UnlElse(Box<Expr>, Box<Expr>, Box<Expr>, Range<usize>),
    WhileExpr(Box<Expr>, Box<Expr>, Range<usize>),
    DoWhile(Box<Expr>, Box<Expr>, Range<usize>), // In the order they appear
    Break(Range<usize>),
    Continue(Range<usize>),
}
impl NonBinExpr {
    pub fn new(children: &BudNodes, range: Range<usize>) -> Result<NonBinExpr, BudErr> {
        match &children[..] {
            [Node::NonTm {
                n: N::BlockExpr,
                children,
                range,
            }] => Self::block_expr(children, range.to_owned()),
            [Node::NonTm {
                n: N::AssignExpr,
                children,
                range,
            }] => Self::assign_expr(children, range.to_owned()),
            [Node::NonTm {
                n: N::VarDeclAssgn,
                children,
                range,
            }] => Self::var_decl_assign(children, range.to_owned()),
            [Node::NonTm {
                n: N::ReturnExpr,
                children,
                range,
            }] => Self::return_expr(children, range.to_owned()),
            [Node::NonTm {
                n: N::CleanupCall,
                children,
                range,
            }] => Self::cleanup_call(children, range.to_owned()),
            [Node::NonTm {
                n: N::CleanupExpr,
                children,
                range,
            }] => Self::cleanup_expr(children, range.to_owned()),
            [Node::NonTm {
                n: N::IdExpr,
                children,
                range,
            }] => Self::id_expr(children, range.to_owned()),
            [Node::NonTm {
                n: N::LitExpr,
                children,
                range,
            }] => Self::lit_expr(children, range.to_owned()),
            [Node::NonTm {
                n: N::ParenExpr,
                children,
                range,
            }] => Self::paren_expr(children, range.to_owned()),
            [Node::NonTm {
                n: N::UnaryExpr,
                children,
                range,
            }] => Self::unary_expr(children, range.to_owned()),
            [Node::NonTm {
                n: N::IfExpr,
                children,
                range,
            }] => Self::if_expr(children, range.to_owned()),
            [Node::NonTm {
                n: N::IfElse,
                children,
                range,
            }] => Self::if_else(children, range.to_owned()),
            [Node::NonTm {
                n: N::UnlExpr,
                children,
                range,
            }] => Self::unless_expr(children, range.to_owned()),
            [Node::NonTm {
                n: N::UnlElse,
                children,
                range,
            }] => Self::unless_else(children, range.to_owned()),
            [Node::NonTm {
                n: N::WhileExpr,
                children,
                range,
            }] => Self::while_expr(children, range.to_owned()),
            [Node::NonTm {
                n: N::DoWhile,
                children,
                range,
            }] => Self::do_while(children, range.to_owned()),
            [Node::Tm { t: T::Break, range }] => Ok(NonBinExpr::Break(range.to_owned())),
            [Node::Tm {
                t: T::Continue,
                range,
            }] => Ok(NonBinExpr::Continue(range.to_owned())),
            _ => {
                c_err!(range, "Invalid node for {} {:?}", N::NonBinExpr, children)
            }
        }
    }
    fn block_expr(
        children: &Vec<Node<BudTerminal, BudNonTerminal>>,
        range: Range<usize>,
    ) -> Result<NonBinExpr, BudErr> {
        Ok(NonBinExpr::BlockExpr(Expr::news(children, range.to_owned())?, range))
    }
    fn assign_expr(
        children: &Vec<Node<BudTerminal, BudNonTerminal>>,
        range: Range<usize>,
    ) -> Result<NonBinExpr, BudErr> {
        match &children[..] {
            [Node::NonTm {
                n: N::IdExpr,
                children: id_expr,
                range: range1,
            }, Node::Tm {
                t: T::Assign,
                range: _,
            }, Node::NonTm {
                n: N::Expr,
                children: expr,
                range: range2,
            }, Node::Tm {
                t: T::Semicolon,
                range: _,
            }] => Ok(NonBinExpr::AssignExpr(
                Box::new(IdExpr::new(id_expr, range1.to_owned())?),
                Box::new(Expr::new(expr, range2.to_owned())?),
                range,
            )),
            _ => {
                c_err!(range, "Invalid node for {} {:?}", N::AssignExpr, children)
            }
        }
    }
    fn var_decl_assign(
        children: &Vec<Node<BudTerminal, BudNonTerminal>>,
        range: Range<usize>,
    ) -> Result<NonBinExpr, BudErr> {
        match &children[..] {
            [Node::NonTm {
                n: N::VarDecl,
                children: var,
                range: range1,
            }, Node::Tm {
                t: T::Assign,
                range: _,
            }, Node::NonTm {
                n: N::Expr,
                children: expr,
                range: range2,
            }, Node::Tm {
                t: T::Semicolon,
                range: _,
            }] => Ok(NonBinExpr::VarDeclAssgn(
                Box::new(VarDecl::new(var, range1.to_owned())?),
                Box::new(Expr::new(expr, range2.to_owned())?),
                range,
            )),
            _ => {
                return c_err!(range, "Invalid node for {} {:?}", N::VarDeclAssgn, children);
            }
        }
    }
    fn return_expr(
        children: &Vec<Node<BudTerminal, BudNonTerminal>>,
        range: Range<usize>,
    ) -> Result<NonBinExpr, BudErr> {
        match &children[..] {
            [Node::Tm {
                t: T::Return,
                range: _,
            }, Node::Tm {
                t: T::Semicolon,
                range: _,
            }] => Ok(NonBinExpr::ReturnExpr(None, range)),
            [Node::Tm {
                t: T::Return,
                range: _,
            }, Node::NonTm {
                n: N::Expr,
                children: expr,
                range: range1,
            }, Node::Tm {
                t: T::Semicolon,
                range: _,
            }] => Ok(NonBinExpr::ReturnExpr(
                Some(Box::new(Expr::new(expr, range1.to_owned())?)),
                range,
            )),
            _ => {
                c_err!(range, "Invalid node for {} {:?}", N::ReturnExpr, children)
            }
        }
    }
    fn cleanup_call(
        children: &Vec<Node<BudTerminal, BudNonTerminal>>,
        range: Range<usize>,
    ) -> Result<NonBinExpr, BudErr> {
        match &children[..] {
            [Node::Tm {
                t: T::Cleanup,
                range: _,
            }, Node::Tm {
                t: T::Semicolon,
                range,
            }] => Ok(NonBinExpr::CleanupCall(range.to_owned())),
            _ => {
                c_err!(range, "Invalid node for {} {:?}", N::CleanupCall, children)
            }
        }
    }
    fn cleanup_expr(
        children: &Vec<Node<BudTerminal, BudNonTerminal>>,
        range: Range<usize>,
    ) -> Result<NonBinExpr, BudErr> {
        match &children[..] {
            [Node::Tm {
                t: T::Cleanup,
                range: _,
            }, Node::NonTm {
                n: N::Expr,
                children: expr,
                range: range1,
            }, Node::Tm {
                t: T::Semicolon,
                range: _,
            }] => Ok(NonBinExpr::CleanupExpr(
                Box::new(Expr::new(expr, range1.to_owned())?),
                range,
            )),
            _ => {
                c_err!(range, "Invalid node for {} {:?}", N::CleanupExpr, children)
            }
        }
    }
    fn id_expr(
        children: &Vec<Node<BudTerminal, BudNonTerminal>>,
        range: Range<usize>,
    ) -> Result<NonBinExpr, BudErr> {
        Ok(NonBinExpr::IdExpr(
            Box::new(IdExpr::new(children, range.to_owned())?),
            range,
        ))
    }
    fn lit_expr(
        children: &Vec<Node<BudTerminal, BudNonTerminal>>,
        range: Range<usize>,
    ) -> Result<NonBinExpr, BudErr> {
        Ok(NonBinExpr::LitExpr(Literal::new(children)?, range))
    }
    fn paren_expr(
        children: &Vec<Node<BudTerminal, BudNonTerminal>>,
        range: Range<usize>,
    ) -> Result<NonBinExpr, BudErr> {
        match &children[..] {
            [Node::Tm {
                t: T::LeftRound,
                range: _,
            }, Node::NonTm {
                n: N::Expr,
                children: expr,
                range: range1,
            }, Node::Tm {
                t: T::RightRound,
                range: _,
            }] => Ok(NonBinExpr::ParenExpr(
                Box::new(Expr::new(expr, range1.to_owned())?),
                range,
            )),
            _ => {
                c_err!(range, "Invalid node for {} {:?}", N::ParenExpr, children)
            }
        }
    }
    fn unary_expr(
        children: &Vec<Node<BudTerminal, BudNonTerminal>>,
        range: Range<usize>,
    ) -> Result<NonBinExpr, BudErr> {
        match &children[..] {
            [Node::NonTm {
                n: N::Unop,
                children: u_children,
                range: _rangeu,
            }, Node::NonTm {
                n: N::NonBinExpr,
                children: nbe,
                range: range1,
            }] => {
                let u;
                match &u_children[..] {
                    [Node::Tm { t: u_t, range: _ }] => {
                        u = u_t.clone();
                    }
                    _ => {
                        return c_err!(range, "Invalid node for {} {:?}", N::Unop, children);
                    }
                }
                Ok(NonBinExpr::UnaryExpr(
                    BudUnop::new(u)?,
                    Box::new(NonBinExpr::new(nbe, range1.to_owned())?),
                    range,
                ))
            }
            _ => {
                c_err!(range, "Invalid node for {} {:?}", N::UnaryExpr, children)
            }
        }
    }
    fn if_expr(
        children: &Vec<Node<BudTerminal, BudNonTerminal>>,
        range: Range<usize>,
    ) -> Result<NonBinExpr, BudErr> {
        match &children[..] {
            [Node::Tm { t: T::If, range: _ }, Node::NonTm {
                n: N::Expr,
                children: cond,
                range: range1,
            }, Node::NonTm {
                n: N::BlockExpr,
                children: exprs,
                range: range2,
            }] => {
                let expr = Expr {
                    bin_expr: Box::new(BinExpr::NonBin(
                        Box::new(Self::BlockExpr(Expr::news(exprs, range2.to_owned())?, range2.to_owned())),
                        range2.to_owned(),
                    )),
                    with_semicolon: false,
                    range: range2.to_owned(),
                };
                Ok(NonBinExpr::IfExpr(
                    Box::new(Expr::new(cond, range1.to_owned())?),
                    Box::new(expr),
                    range,
                ))
            }
            _ => {
                c_err!(range, "Invalid node for {} {:?}", N::IfExpr, children)
            }
        }
    }
    fn if_else(children: &Vec<Node<BudTerminal, BudNonTerminal>>, range: Range<usize>) -> Result<NonBinExpr, BudErr> {
        match &children[..] {
            [Node::Tm { t: T::If, range: _ }, Node::NonTm {
                n: N::Expr,
                children: cond,
                range: range1,
            }, Node::NonTm {
                n: N::BlockExpr,
                children: exprs_t,
                range: range2,
            }, Node::Tm { t: T::Else, range: _ }, Node::NonTm {
                n: N::BlockExpr,
                children: exprs_f,
                range: range3,
            }] => {
                let expr_t = Expr {
                    bin_expr: Box::new(BinExpr::NonBin(Box::new(Self::BlockExpr(Expr::news(
                        exprs_t,
                        range2.to_owned(),
                    )?, range2.to_owned())), range2.to_owned())),
                    with_semicolon: false,
                    range: range2.to_owned(),
                };
                let expr_f = Expr {
                    bin_expr: Box::new(BinExpr::NonBin(Box::new(Self::BlockExpr(Expr::news(
                        exprs_f,
                        range3.to_owned(),
                    )?, range3.to_owned())), range3.to_owned())),
                    with_semicolon: false,
                    range: range3.to_owned(),
                };
                Ok(NonBinExpr::IfElse(
                    Box::new(Expr::new(cond, range1.to_owned())?),
                    Box::new(expr_t),
                    Box::new(expr_f),
                    range,
                ))
            }
            _ => {
                c_err!(range, "Invalid node for {} {:?}", N::IfElse, children)
            }
        }
    }
    fn unless_expr(
        children: &Vec<Node<BudTerminal, BudNonTerminal>>,
        range: Range<usize>,
    ) -> Result<NonBinExpr, BudErr> {
        match &children[..] {
            [Node::Tm {
                t: T::Unless,
                range: _,
            }, Node::NonTm {
                n: N::Expr,
                children: cond,
                range: range1,
            }, Node::NonTm {
                n: N::BlockExpr,
                children: exprs,
                range: range2,
            }] => {
                let expr = Expr {
                    bin_expr: Box::new(BinExpr::NonBin(
                        Box::new(Self::BlockExpr(Expr::news(exprs, range2.to_owned())?, range2.to_owned())),
                        range2.to_owned(),
                    )),
                    with_semicolon: false,
                    range: range2.to_owned(),
                };
                Ok(NonBinExpr::UnlExpr(
                    Box::new(Expr::new(cond, range1.to_owned())?),
                    Box::new(expr),
                    range
                ))
            }
            _ => {
                c_err!(range, "Invalid node for {} {:?}", N::UnlExpr, children)
            }
        }
    }
    fn unless_else(
        children: &Vec<Node<BudTerminal, BudNonTerminal>>,
        range: Range<usize>,
    ) -> Result<NonBinExpr, BudErr> {
        match &children[..] {
            [Node::Tm {
                t: T::Unless,
                range: _,
            }, Node::NonTm {
                n: N::Expr,
                children: cond,
                range: range1,
            }, Node::NonTm {
                n: N::BlockExpr,
                children: exprs_f,
                range: range2,
            }, Node::Tm { t: T::Else, range: _ }, Node::NonTm {
                n: N::BlockExpr,
                children: exprs_t,
                range: range3,
            }] => {
                let expr_f = Expr {
                    bin_expr: Box::new(BinExpr::NonBin(Box::new(Self::BlockExpr(Expr::news(
                        exprs_f,
                        range2.to_owned(),
                    )?, range2.to_owned())), range2.to_owned())),
                    with_semicolon: false,
                    range: range2.to_owned(),
                };
                let expr_t = Expr {
                    bin_expr: Box::new(BinExpr::NonBin(Box::new(Self::BlockExpr(Expr::news(
                        exprs_t,
                        range3.to_owned(),
                    )?, range3.to_owned())), range3.to_owned())),
                    with_semicolon: false,
                    range: range3.to_owned(),
                };
                Ok(NonBinExpr::UnlElse(
                    Box::new(Expr::new(cond, range1.to_owned())?),
                    Box::new(expr_f),
                    Box::new(expr_t),
                    range,
                ))
            }
            _ => {
                c_err!(range, "Invalid node for {} {:?}", N::UnlElse, children)
            }
        }
    }
    fn while_expr(children: &Vec<Node<BudTerminal, BudNonTerminal>>, range: Range<usize>) -> Result<NonBinExpr, BudErr> {
        match &children[..] {
            [Node::Tm { t: T::While, range: _ }, Node::NonTm {
                n: N::Expr,
                children: cond,
                range: range1,
            }, Node::NonTm {
                n: N::BlockExpr,
                children: exprs,
                range: range2,
            }] => {
                let expr = Expr {
                    bin_expr: Box::new(BinExpr::NonBin(
                        Box::new(Self::BlockExpr(Expr::news(exprs, range2.to_owned())?, range2.to_owned())),
                        range2.to_owned(),
                    )),
                    with_semicolon: false,
                    range: range2.to_owned(),
                };
                Ok(NonBinExpr::WhileExpr(
                    Box::new(Expr::new(cond, range1.to_owned())?),
                    Box::new(expr),
                    range,
                ))
            }
            _ => {
                c_err!(range, "Invalid node for {} {:?}", N::WhileExpr, children)
            }
        }
    }
    fn do_while(children: &Vec<Node<BudTerminal, BudNonTerminal>>, range: Range<usize>) -> Result<NonBinExpr, BudErr> {
        match &children[..] {
            [Node::Tm { t: T::Do, range: _ }, Node::NonTm {
                n: N::BlockExpr,
                children: exprs,
                range: range1,
            }, Node::Tm { t: T::While, range: _ }, Node::NonTm {
                n: N::Expr,
                children: cond,
                range: range2,
            }, Node::Tm {
                t: T::Semicolon,
                range: _,
            }] => {
                let expr = Expr {
                    bin_expr: Box::new(BinExpr::NonBin(
                        Box::new(Self::BlockExpr(Expr::news(exprs, range1.to_owned())?, range1.to_owned())),
                        range1.to_owned(),
                    )),
                    with_semicolon: false,
                    range: range1.to_owned(),
                };
                Ok(NonBinExpr::DoWhile(
                    Box::new(expr),
                    Box::new(Expr::new(cond, range2.to_owned())?),
                    range,
                ))
            }
            _ => {
                c_err!(range, "Invalid node for {} {:?}", N::DoWhile, children)
            }
        }
    }
}
impl Into<Result<i32, BudErr>> for NonBinExpr {
    fn into(self) -> Result<i32, BudErr> {
        match self {
            NonBinExpr::LitExpr(Literal::Num(num), _) => Ok(num),
            NonBinExpr::LitExpr(Literal::Str(string), range) => c_err!(range, "Cannot convert string \"{}\" to num", string),
            NonBinExpr::BlockExpr(_, range) => u_err!(range, "Cannot convert BlockExpr to num"),
            NonBinExpr::AssignExpr(_, _, range) => u_err!(range, "Cannot convert AssignExpr to num"),
            NonBinExpr::VarDeclAssgn(_, _, range) => u_err!(range, "Cannot convert VarDeclAssgn to num"),
            NonBinExpr::ReturnExpr(_, range) => u_err!(range, "Cannot convert ReturnExpr to num"),
            NonBinExpr::CleanupCall(range) => u_err!(range, "Cannot convert CleanupCall to num"),
            NonBinExpr::CleanupExpr(_, range) => u_err!(range, "Cannot convert CleanupExpr to num"),
            NonBinExpr::IdExpr(_, range) => u_err!(range, "Cannot convert IdExpr to num"),
            NonBinExpr::ParenExpr(_, range) => u_err!(range, "Cannot convert ParenExpr to num"),
            NonBinExpr::UnaryExpr(_, _, range) => u_err!(range, "Cannot convert UnaryExpr to num"),
            NonBinExpr::IfExpr(_, _, range) => u_err!(range, "Cannot convert IfExpr to num"),
            NonBinExpr::IfElse(_, _, _, range) => u_err!(range, "Cannot convert IfElse to num"),
            NonBinExpr::UnlExpr(_, _, range) => u_err!(range, "Cannot convert UnlExpr to num"),
            NonBinExpr::UnlElse(_, _, _, range) => u_err!(range, "Cannot convert UnlElse to num"),
            NonBinExpr::WhileExpr(_, _, range) => u_err!(range, "Cannot convert WhileExpr to num"),
            NonBinExpr::DoWhile(_, _, range) => u_err!(range, "Cannot convert DoWhile to num"),
            NonBinExpr::Break(range) => u_err!(range, "Cannot convert Break to num"),
            NonBinExpr::Continue(range) => u_err!(range, "Cannot convert Continue to num"),
        }
    }
}
impl Ranged for NonBinExpr {
    fn get_range(&self) -> &Range<usize> {
        match self {
            NonBinExpr::BlockExpr(_, range) => range,
            NonBinExpr::AssignExpr(_, _, range) => range,
            NonBinExpr::VarDeclAssgn(_, _, range) => range,
            NonBinExpr::ReturnExpr(_, range) => range,
            NonBinExpr::CleanupCall(range) => range,
            NonBinExpr::CleanupExpr(_, range) => range,
            NonBinExpr::IdExpr(_, range) => range,
            NonBinExpr::LitExpr(_, range) => range,
            NonBinExpr::ParenExpr(_, range) => range,
            NonBinExpr::UnaryExpr(_, _, range) => range,
            NonBinExpr::IfExpr(_, _, range) => range,
            NonBinExpr::IfElse(_, _, _, range) => range,
            NonBinExpr::UnlExpr(_, _, range) => range,
            NonBinExpr::UnlElse(_, _, _, range) => range,
            NonBinExpr::WhileExpr(_, _, range) => range,
            NonBinExpr::DoWhile(_, _, range) => range,
            NonBinExpr::Break(range) => range,
            NonBinExpr::Continue(range) => range,
        }
    }
}

#[derive(Debug, Clone)]
pub enum IdExpr {
    SquareIndex(Box<Expr>, Box<Expr>, Range<usize>),
    RoundIndex(Box<Expr>, Option<Exprs>, Range<usize>),
    Id(Id, Range<usize>),
    Deref(Box<IdExpr>, Range<usize>),
}
impl IdExpr {
    pub fn new(children: &BudNodes, range: Range<usize>) -> Result<IdExpr, BudErr> {
        match &children[..] {
            [Node::NonTm {
                n: N::SqrExpr,
                children: sqe,
                range: range1,
            }] => {
                let id_expr = Self::sqe_to_id_sqr(sqe, range1.to_owned())?;
                Ok(id_expr)
            }
            [Node::Tm {
                t: T::Id(id),
                range: range1,
            }, Node::Tm {
                t: T::LeftRound,
                range: _,
            }, Node::NonTm {
                n: N::Exprs,
                children: args,
                range: range2,
            }, Node::Tm {
                t: T::RightRound,
                range: _,
            }] => Ok(IdExpr::RoundIndex(
                Box::new(Self::id_to_expr(id.to_owned(), range1.to_owned())),
                Some(Expr::news(args, range2.to_owned())?),
                range,
            )),
            [Node::Tm {
                t: T::Id(id),
                range: range1,
            }, Node::Tm {
                t: T::LeftRound,
                range: _,
            }, Node::Tm {
                t: T::RightRound,
                range: _,
            }] => {
                let expr = Self::id_to_expr(id.to_owned(), range1.to_owned());
                Ok(IdExpr::RoundIndex(Box::new(expr), None, range))
            }
            [Node::Tm {
                t: T::Id(id),
                range: range1,
            }] => Ok(IdExpr::Id(id.to_owned(), range1.to_owned())),
            [Node::Tm { t: T::Star, range: _ }, Node::NonTm {
                n: N::IdExpr,
                children: ide,
                range: range1,
            }] => Ok(IdExpr::Deref(Box::new(IdExpr::new(ide, range1.to_owned())?), range)),
            _ => {
                c_err!(range, "Invalid node for {} {:?}", N::IdExpr, children)
            }
        }
    }
    fn id_to_expr(id: String, range: Range<usize>) -> Expr {
        Self::id_expr_to_expr(IdExpr::Id(id, range))
    }
    fn id_expr_to_expr(id_expr: IdExpr) -> Expr {
        let range = id_expr.get_range_owned();
        Expr {
            bin_expr: Box::new(BinExpr::NonBin(Box::new(NonBinExpr::IdExpr(Box::new(
                id_expr,
            ), range.to_owned())), range.to_owned())),
            with_semicolon: false,
            range
        }
    }
    fn sqe_to_id_sqr(children: &BudNodes, range: Range<usize>) -> Result<IdExpr, BudErr> {
        match &children[..] {
            [Node::Tm {
                t: T::Id(id),
                range: range1,
            }, Node::NonTm {
                n: N::Sqr,
                children: sqr,
                range: range2,
            }] => {
                let mut id_expr = IdExpr::Id(id.to_owned(), range1.to_owned());
                id_expr = Self::put_indices_on(id_expr, sqr, range2.to_owned())?;
                Ok(id_expr)
            }
            _ => {
                c_err!(
                    range,
                    "Invalid node for {} being interpreted as {} {:?}",
                    N::SqrExpr,
                    N::IdExpr,
                    children
                )
            }
        }
    }
    fn put_indices_on(id: IdExpr, sqr: &BudNodes, range: Range<usize>) -> Result<IdExpr, BudErr> {
        let id_expr = Self::id_expr_to_expr(id);
        match &sqr[..] {
            [Node::Tm {
                t: T::LeftSquare,
                range: _,
            }, Node::NonTm {
                n: N::Expr,
                children: expr,
                range: range1,
            }, Node::Tm {
                t: T::RightSquare,
                range: _,
            }, Node::NonTm {
                n: N::Sqr,
                children: sqr2,
                range: range2,
            }] => {
                let expr = Expr::new(expr, range1.to_owned())?;
                let mut id_expr = IdExpr::SquareIndex(Box::new(id_expr), Box::new(expr), range);
                id_expr = Self::put_indices_on(id_expr, sqr2, range2.to_owned())?;
                Ok(id_expr)
            }
            [Node::Tm {
                t: T::LeftSquare,
                range: _,
            }, Node::NonTm {
                n: N::Expr,
                children: expr,
                range: range1,
            }, Node::Tm {
                t: T::RightSquare,
                range: _,
            }] => {
                let expr = Expr::new(expr, range1.to_owned())?;
                let id_expr = IdExpr::SquareIndex(Box::new(id_expr), Box::new(expr), range);
                Ok(id_expr)
            }
            _ => {
                c_err!(
                    range,
                    "Invalid node for {} being interpreted as array indices {:?}",
                    N::Sqr,
                    sqr
                )
            }
        }
    }
}
impl Ranged for IdExpr {
    fn get_range(&self) -> &Range<usize> {
        match self {
            IdExpr::SquareIndex(_, _, range) => range,
            IdExpr::RoundIndex(_, _, range) => range,
            IdExpr::Id(_, range) => range,
            IdExpr::Deref(_, range) => range,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeExpr {
    Id(Id, Range<usize>),
    TypSqr(Box<TypeExpr>, Vec<i32>, Range<usize>),
    Pointer(Box<TypeExpr>, Range<usize>),
}
impl TypeExpr {
    pub fn new(children: &BudNodes, range: Range<usize>) -> Result<TypeExpr, BudErr> {
        match &children[..] {
            [Node::Tm {
                t: T::Id(id),
                range: range1,
            }] => Ok(TypeExpr::Id(id.to_owned(), range1.to_owned())),
            [Node::NonTm {
                n: N::SqrExpr,
                children: sqe,
                range: range1,
            }] => Self::sqe_to_typ_sqr(sqe, range1.to_owned()),
            [Node::Tm {
                t: T::Reference,
                range: _,
            }, Node::Tm {
                t: T::LeftRound,
                range: _,
            }, Node::NonTm {
                n: N::TypeExpr,
                children: te,
                range: range1,
            }, Node::Tm {
                t: T::RightRound,
                range: _,
            }, Node::NonTm {
                n: N::Sqr,
                children: sqr,
                range: range2,
            }] => Ok(TypeExpr::TypSqr(
                Box::new(TypeExpr::Pointer(Box::new(TypeExpr::new(te, range1.to_owned())?), range.to_owned())),
                {
                    let mut lengths = Vec::new();
                    Self::sqr_to_lengths(sqr, &mut lengths, range2.to_owned())?;
                    lengths
                },
                range
            )),
            [Node::Tm {
                t: T::Reference,
                range: _,
            }, Node::Tm {
                t: T::LeftRound,
                range: _,
            }, Node::NonTm {
                n: N::TypeExpr,
                children: typ,
                range: range1,
            }, Node::Tm {
                t: T::RightRound,
                range: _,
            }] => Ok(TypeExpr::Pointer(Box::new(TypeExpr::new(typ, range1.to_owned())?), range)),
            _ => {
                c_err!(range, "Invalid node for {} {:?}", N::TypeExpr, children)
            }
        }
    }
    fn sqe_to_typ_sqr(children: &BudNodes, range: Range<usize>) -> Result<TypeExpr, BudErr> {
        match &children[..] {
            [Node::Tm {
                t: T::Id(id),
                range: range1,
            }, Node::NonTm {
                n: N::Sqr,
                children: sqr,
                range: range2,
            }] => {
                let id = Box::new(TypeExpr::Id(id.to_owned(), range1.to_owned()));
                let mut lengths = Vec::new();
                Self::sqr_to_lengths(sqr, &mut lengths, range2.to_owned())?;
                Ok(TypeExpr::TypSqr(id, lengths, range))
            }
            _ => {
                c_err!(
                    range,
                    "Invalid node for {} being interpreted as {} {:?}",
                    N::SqrExpr,
                    N::TypeExpr,
                    children
                )
            }
        }
    }
    fn sqr_to_lengths(children: &BudNodes, lengths: &mut Vec<i32>, range: Range<usize>) -> Result<(), BudErr> {
        match &children[..] {
            [Node::Tm {
                t: T::LeftSquare,
                range: _,
            }, Node::NonTm {
                n: N::Expr,
                children: expr,
                range: range1,
            }, Node::Tm {
                t: T::RightSquare,
                range: _,
            }, Node::NonTm {
                n: N::Sqr,
                children: sqr2,
                range: range2,
            }] => {
                let expr = Expr::new(expr, range1.to_owned())?;
                let length = Into::<Result<i32, BudErr>>::into(expr)?;
                lengths.push(length);
                Self::sqr_to_lengths(sqr2, lengths, range)?;
                Ok(())
            }
            [Node::Tm {
                t: T::LeftSquare,
                range: _,
            }, Node::NonTm {
                n: N::Expr,
                children: expr,
                range: range1,
            }, Node::Tm {
                t: T::RightSquare,
                range: _,
            }] => {
                let expr = Expr::new(expr, range1.to_owned())?;
                let length: i32 = Into::<Result<i32, BudErr>>::into(expr)?;
                lengths.push(length);
                Ok(())
            }
            _ => {
                c_err!(
                    range,
                    "Invalid node for {} being interpreted as array lengths {:?}",
                    N::Sqr,
                    children
                )
            }
        }
    }
}
impl Ranged for TypeExpr {
    fn get_range(&self) -> &Range<usize> {
        match self {
            TypeExpr::Id(_, range) => range,
            TypeExpr::TypSqr(_, _, range) => range,
            TypeExpr::Pointer(_, range) => range,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Num(i32),
    Str(String),
}
impl Literal {
    pub fn new(children: &BudNodes) -> Result<Literal, BudErr> {
        match &children[..] {
            [Node::Tm {
                t: T::Num(num),
                range: _,
            }] => Ok(Literal::Num(*num)),
            [Node::Tm {
                t: T::Str(string),
                range: _,
            }] => Ok(Literal::Str(string.to_owned())),
            [Node::Tm {
                t: T::Char(char),
                range: _,
            }] => Ok(Literal::Num((Self::decode_char(char)? as u8).into())),
            _ => {
                c_err!("Invalid node for {} {:?}", N::LitExpr, children)
            }
        }
    }

    fn decode_char(s: &str) -> Result<char, BudErr> {
        match s {
            // Handle backslash-encoded characters
            "\\\\" => Ok('\\'),
            "\\\"" => Ok('\"'),
            "\\'" => Ok('\''),
            "\\n" => Ok('\n'),
            "\\r" => Ok('\r'),
            "\\t" => Ok('\t'),
            _ => {
                if s.len() > 1 {
                    u_err!("Invalid character, '{}'", s)
                } else {
                    Ok(s.chars().next().unwrap())
                }
            }
        }
    }
}
