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
        Path,
        File,
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
        IdExpr      => Expr, LeftRound, RightRound;
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
        Unop        => Not;
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
    pub fn new(t: BudTerminal) -> Result<BudBinop, String> {
        match t {
            BudTerminal::Plus => Ok(BudBinop::Plus),
            BudTerminal::Minus => Ok(BudBinop::Minus),
            BudTerminal::Star => Ok(BudBinop::Times),
            BudTerminal::Div => Ok(BudBinop::Div),
            BudTerminal::And => Ok(BudBinop::And),
            BudTerminal::Or => Ok(BudBinop::Or),
            BudTerminal::Ambersand => Ok(BudBinop::BitAnd),
            BudTerminal::BitOr => Ok(BudBinop::BitOr),
            BudTerminal::BitXor => Ok(BudBinop::BitXor),
            BudTerminal::Equal => Ok(BudBinop::Equal),
            BudTerminal::NotEq => Ok(BudBinop::NotEq),
            BudTerminal::Greater => Ok(BudBinop::Greater),
            BudTerminal::GrtrEq => Ok(BudBinop::GrtrEq),
            BudTerminal::Less => Ok(BudBinop::Less),
            BudTerminal::LessEq => Ok(BudBinop::LessEq),
            _ => Err(format!("Invalid binop: {}", t)),
        }
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
impl BudUnop {
    fn new(u: BudTerminal) -> Result<BudUnop, String> {
        match u {
            BudTerminal::Not => Ok(BudUnop::Not),
            BudTerminal::Minus => Ok(BudUnop::Neg),
            BudTerminal::Star => Ok(BudUnop::Deref),
            BudTerminal::Ambersand => Ok(BudUnop::Ref),
            _ => Err(format!("Invalid unop: {}", u)),
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

use crate::parse::Node;
type BudNode = Node<BudTerminal, BudNonTerminal>;
type BudNodes = Vec<BudNode>;
type N = BudNonTerminal;
type T = BudTerminal;

// Program
#[derive(Debug)]
pub enum Item {
    FuncDecl(VarDecl, VarDecls, Box<Expr>),
    StructDecl(Id, VarDecls),
    ImportDecl(Path),
}
pub type Items = Vec<Item>;
impl Item {
    pub fn new(children: &BudNodes) -> Result<Item, String> {
        match &children[..] {
            [Node::NonTm(N::FuncDecl, children)] => match &children[..] {
                [Node::NonTm(N::VarDecl, name), Node::NonTm(N::VarDeclsPar, args), Node::NonTm(N::Expr, expr)] => {
                    Ok(Item::FuncDecl(
                        VarDecl::new(name)?,
                        VarDecl::news(args)?,
                        Box::new(Expr::new(expr)?),
                    ))
                }
                _ => {
                    return Err(format!("Invalid node for {} {:?}", N::FuncDecl, children));
                }
            },
            [Node::NonTm(N::StructDecl, children)] => match &children[..] {
                [Node::Tm(T::Struct), Node::Tm(T::Id(id)), Node::Tm(T::LeftSquiggly), Node::NonTm(N::VarDecls, fields), Node::Tm(T::RightSquiggly)] => {
                    Ok(Item::StructDecl(id.to_owned(), VarDecl::news(fields)?))
                }
                _ => {
                    return Err(format!("Invalid node for {} {:?}", N::StructDecl, children));
                }
            },
            [Node::NonTm(N::ImportDecl, children)] => match &children[..] {
                [Node::Tm(T::Import), Node::NonTm(N::Path, _path)] => {
                    return Err(format!("Import statements not supported yet"));
                }
                _ => {
                    return Err(format!("Invalid node for {} {:?}", N::ImportDecl, children));
                }
            },
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::Item, children));
            }
        }
    }
    pub fn news(children: &BudNodes) -> Result<Items, String> {
        let mut children = children.clone();
        let mut items = Vec::new();
        loop {
            match &children[..] {
                [Node::NonTm(N::Item, i_children), Node::NonTm(N::Items, is_children)] => {
                    items.push(Item::new(i_children)?);
                    children = is_children.clone();
                }
                [Node::NonTm(N::Item, children)] => {
                    items.push(Item::new(children)?);
                    break;
                }
                _ => {
                    return Err(format!("Invalid node for {} {:?}", N::Items, children));
                }
            }
        }
        Ok(items)
    }
}

#[derive(Debug)]
pub enum Path {
    Path(String),
}
impl Path {
    pub fn new(_children: &BudNodes) -> Result<Path, String> {
        return Err(format!("Import statements not supported yet"));
    }
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub typ: TypeExpr,
    pub id: Id,
}
pub type VarDecls = Vec<VarDecl>;
pub type Id = String;
impl VarDecl {
    pub fn new(children: &BudNodes) -> Result<VarDecl, String> {
        match &children[..] {
            [Node::NonTm(N::TypeExpr, typ), Node::Tm(T::Id(id))] => Ok(VarDecl {
                typ: TypeExpr::new(typ)?,
                id: id.to_owned(),
            }),
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::VarDecl, children));
            }
        }
    }
    pub fn news(children: &BudNodes) -> Result<VarDecls, String> {
        // Be able to read Vsp as well as Vs
        let mut children = children.clone();
        let mut items = Vec::new();
        loop {
            match &children[..] {
                [Node::NonTm(N::VarDecl, v_children), Node::Tm(T::Comma), Node::NonTm(N::VarDecls, vs_children)] =>
                {
                    items.push(VarDecl::new(v_children)?);
                    children = vs_children.clone();
                }
                [Node::NonTm(N::VarDecl, children)] => {
                    items.push(VarDecl::new(children)?);
                    break;
                }
                [Node::Tm(T::LeftRound), Node::Tm(T::RightRound)] => {
                    return Ok(Vec::new());
                }
                [Node::Tm(T::LeftRound), Node::NonTm(N::VarDecls, children), Node::Tm(T::RightRound)] =>
                {
                    return VarDecl::news(children);
                }
                _ => {
                    return Err(format!("Invalid node for {} {:?}", N::VarDecls, children));
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
}
pub type Exprs = Vec<Expr>;
impl Expr {
    pub fn new(children: &BudNodes) -> Result<Expr, String> {
        match &children[..] {
            [Node::NonTm(N::BinaryExpr, children)] => Ok(Expr {
                bin_expr: Box::new(BinExpr::new(children)?),
                with_semicolon: false,
            }),
            [Node::NonTm(N::Expr, children), Node::Tm(T::Semicolon)] => {
                let mut expr = Expr::new(children)?;
                expr.with_semicolon = true;
                Ok(expr)
            }
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::Expr, children));
            }
        }
    }
    pub fn news(children: &BudNodes) -> Result<Exprs, String> {
        // Be able to read E2 as well as Es
        let mut children = children.clone();
        let mut items = Vec::new();
        loop {
            match &children[..] {
                [Node::NonTm(N::Expr, e_children), Node::Tm(T::Comma), Node::NonTm(N::Exprs, es_children)] =>
                {
                    items.push(Expr::new(e_children)?);
                    children = es_children.clone();
                }
                [Node::NonTm(N::Expr, expr), Node::NonTm(N::Expr2, expr2)] => {
                    items.push(Expr::new(expr)?);
                    children = expr2.clone();
                }
                [Node::NonTm(N::Expr, expr)] => {
                    items.push(Expr::new(expr)?);
                    break;
                }
                [Node::Tm(T::LeftSquiggly), Node::NonTm(N::Expr2, children), Node::Tm(T::RightSquiggly)] =>
                {
                    return Expr::news(children);
                }
                _ => {
                    return Err(format!("Invalid node for {} {:?}", N::Exprs, children));
                }
            }
        }
        Ok(items)
    }
}

#[derive(Debug, Clone)]
pub enum BinExpr {
    Binary(Box<NonBinExpr>, BudBinop, Box<BinExpr>),
    NonBin(Box<NonBinExpr>),
}
impl BinExpr {
    pub fn new(children: &BudNodes) -> Result<BinExpr, String> {
        match &children[..] {
            [Node::NonTm(N::NonBinExpr, nbe), Node::NonTm(N::Binop, b_children), Node::NonTm(N::BinaryExpr, be)] =>
            {
                let b;
                match &b_children[..] {
                    [Node::Tm(b_t)] => {
                        b = b_t.clone();
                    }
                    _ => {
                        return Err(format!("Invalid node for {} {:?}", N::Binop, children));
                    }
                }
                Ok(BinExpr::Binary(
                    Box::new(NonBinExpr::new(nbe)?),
                    BudBinop::new(b)?,
                    Box::new(BinExpr::new(be)?),
                ))
            }
            [Node::NonTm(N::NonBinExpr, nbe)] => {
                Ok(BinExpr::NonBin(Box::new(NonBinExpr::new(nbe)?)))
            }
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::BinaryExpr, children));
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum NonBinExpr {
    BlockExpr(Exprs),
    AssignExpr(Box<IdExpr>, Box<Expr>),
    VarDeclAssgn(Box<VarDecl>, Box<Expr>),
    ReturnExpr(Option<Box<Expr>>),
    IdExpr(Box<IdExpr>),
    LitExpr(Literal),
    ParenExpr(Box<Expr>),
    UnaryExpr(BudUnop, Box<NonBinExpr>),
    IfExpr(Box<Expr>, Box<Expr>),
    IfElse(Box<Expr>, Box<Expr>, Box<Expr>),
    UnlExpr(Box<Expr>, Box<Expr>),
    UnlElse(Box<Expr>, Box<Expr>, Box<Expr>),
    WhileExpr(Box<Expr>, Box<Expr>),
    DoWhile(Box<Expr>, Box<Expr>), // In the order they appear
    Break,
    Continue,
}
impl NonBinExpr {
    pub fn new(children: &BudNodes) -> Result<NonBinExpr, String> {
        match &children[..] {
            [Node::NonTm(N::BlockExpr, children)] => Self::block_expr(children),
            [Node::NonTm(N::AssignExpr, children)] => Self::assign_expr(children),
            [Node::NonTm(N::VarDeclAssgn, children)] => Self::var_decl_assign(children),
            [Node::NonTm(N::ReturnExpr, children)] => Self::return_expr(children),
            [Node::NonTm(N::IdExpr, children)] => Self::id_expr(children),
            [Node::NonTm(N::LitExpr, children)] => Self::lit_expr(children),
            [Node::NonTm(N::ParenExpr, children)] => Self::paren_expr(children),
            [Node::NonTm(N::UnaryExpr, children)] => Self::unary_expr(children),
            [Node::NonTm(N::IfExpr, children)] => Self::if_expr(children),
            [Node::NonTm(N::IfElse, children)] => Self::if_else(children),
            [Node::NonTm(N::UnlExpr, children)] => Self::unless_expr(children),
            [Node::NonTm(N::UnlElse, children)] => Self::unless_else(children),
            [Node::NonTm(N::WhileExpr, children)] => Self::while_expr(children),
            [Node::NonTm(N::DoWhile, children)] => Self::do_while(children),
            [Node::Tm(T::Break)] => Ok(NonBinExpr::Break),
            [Node::Tm(T::Continue)] => Ok(NonBinExpr::Continue),
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::NonBinExpr, children));
            }
        }
    }
    fn block_expr(children: &Vec<Node<BudTerminal, BudNonTerminal>>) -> Result<NonBinExpr, String> {
        Ok(NonBinExpr::BlockExpr(Expr::news(children)?))
    }
    fn assign_expr(
        children: &Vec<Node<BudTerminal, BudNonTerminal>>,
    ) -> Result<NonBinExpr, String> {
        match &children[..] {
            [Node::NonTm(N::IdExpr, id_expr), Node::Tm(T::Assign), Node::NonTm(N::Expr, expr)] => {
                Ok(NonBinExpr::AssignExpr(
                    Box::new(IdExpr::new(id_expr)?),
                    Box::new(Expr::new(expr)?),
                ))
            }
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::AssignExpr, children));
            }
        }
    }
    fn var_decl_assign(
        children: &Vec<Node<BudTerminal, BudNonTerminal>>,
    ) -> Result<NonBinExpr, String> {
        match &children[..] {
            [Node::NonTm(N::VarDecl, var), Node::Tm(T::Assign), Node::NonTm(N::Expr, expr)] => Ok(
                NonBinExpr::VarDeclAssgn(Box::new(VarDecl::new(var)?), Box::new(Expr::new(expr)?)),
            ),
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::VarDeclAssgn, children));
            }
        }
    }
    fn return_expr(
        children: &Vec<Node<BudTerminal, BudNonTerminal>>,
    ) -> Result<NonBinExpr, String> {
        match &children[..] {
            [Node::Tm(T::Return)] => Ok(NonBinExpr::ReturnExpr(None)),
            [Node::Tm(T::Return), Node::NonTm(N::Expr, expr)] => {
                Ok(NonBinExpr::ReturnExpr(Some(Box::new(Expr::new(expr)?))))
            }
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::ReturnExpr, children));
            }
        }
    }
    fn id_expr(children: &Vec<Node<BudTerminal, BudNonTerminal>>) -> Result<NonBinExpr, String> {
        Ok(NonBinExpr::IdExpr(Box::new(IdExpr::new(children)?)))
    }
    fn lit_expr(children: &Vec<Node<BudTerminal, BudNonTerminal>>) -> Result<NonBinExpr, String> {
        Ok(NonBinExpr::LitExpr(Literal::new(children)?))
    }
    fn paren_expr(children: &Vec<Node<BudTerminal, BudNonTerminal>>) -> Result<NonBinExpr, String> {
        match &children[..] {
            [Node::Tm(T::LeftRound), Node::NonTm(N::Expr, expr), Node::Tm(T::RightRound)] => {
                Ok(NonBinExpr::ParenExpr(Box::new(Expr::new(expr)?)))
            }
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::ParenExpr, children));
            }
        }
    }
    fn unary_expr(children: &Vec<Node<BudTerminal, BudNonTerminal>>) -> Result<NonBinExpr, String> {
        match &children[..] {
            [Node::NonTm(N::Unop, u_children), Node::NonTm(N::NonBinExpr, nbe)] => {
                let u;
                match &u_children[..] {
                    [Node::Tm(u_t)] => {
                        u = u_t.clone();
                    }
                    _ => {
                        return Err(format!("Invalid node for {} {:?}", N::Unop, children));
                    }
                }
                Ok(NonBinExpr::UnaryExpr(
                    BudUnop::new(u)?,
                    Box::new(NonBinExpr::new(nbe)?),
                ))
            }
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::UnaryExpr, children));
            }
        }
    }
    fn if_expr(children: &Vec<Node<BudTerminal, BudNonTerminal>>) -> Result<NonBinExpr, String> {
        match &children[..] {
            [Node::Tm(T::If), Node::NonTm(N::Expr, cond), Node::Tm(T::LeftSquiggly), Node::NonTm(N::Expr, expr), Node::Tm(T::RightSquiggly)] => {
                Ok(NonBinExpr::IfExpr(
                    Box::new(Expr::new(cond)?),
                    Box::new(Expr::new(expr)?),
                ))
            }
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::IfExpr, children));
            }
        }
    }
    fn if_else(children: &Vec<Node<BudTerminal, BudNonTerminal>>) -> Result<NonBinExpr, String> {
        match &children[..] {
            [Node::Tm(T::If), Node::NonTm(N::Expr, cond), Node::Tm(T::LeftSquiggly), Node::NonTm(N::Expr, expr_t), Node::Tm(T::RightSquiggly), Node::Tm(T::Else), Node::Tm(T::LeftSquiggly), Node::NonTm(N::Expr, expr_f), Node::Tm(T::RightSquiggly)] => {
                Ok(NonBinExpr::IfElse(
                    Box::new(Expr::new(cond)?),
                    Box::new(Expr::new(expr_t)?),
                    Box::new(Expr::new(expr_f)?),
                ))
            }
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::IfElse, children));
            }
        }
    }
    fn unless_expr(
        children: &Vec<Node<BudTerminal, BudNonTerminal>>,
    ) -> Result<NonBinExpr, String> {
        match &children[..] {
            [Node::Tm(T::Unless), Node::NonTm(N::Expr, cond), Node::Tm(T::LeftSquiggly), Node::NonTm(N::Expr, expr), Node::Tm(T::RightSquiggly)] => {
                Ok(NonBinExpr::UnlExpr(
                    Box::new(Expr::new(cond)?),
                    Box::new(Expr::new(expr)?),
                ))
            }
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::UnlExpr, children));
            }
        }
    }
    fn unless_else(
        children: &Vec<Node<BudTerminal, BudNonTerminal>>,
    ) -> Result<NonBinExpr, String> {
        match &children[..] {
            [Node::Tm(T::Unless), Node::NonTm(N::Expr, cond), Node::Tm(T::LeftSquiggly), Node::NonTm(N::Expr, expr_t), Node::Tm(T::RightSquiggly), Node::Tm(T::Else), Node::Tm(T::LeftSquiggly), Node::NonTm(N::Expr, expr_f), Node::Tm(T::RightSquiggly)] => {
                Ok(NonBinExpr::UnlElse(
                    Box::new(Expr::new(cond)?),
                    Box::new(Expr::new(expr_t)?),
                    Box::new(Expr::new(expr_f)?),
                ))
            }
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::UnlElse, children));
            }
        }
    }
    fn while_expr(children: &Vec<Node<BudTerminal, BudNonTerminal>>) -> Result<NonBinExpr, String> {
        match &children[..] {
            [Node::Tm(T::While), Node::NonTm(N::Expr, cond), Node::Tm(T::LeftSquiggly), Node::NonTm(N::Expr, expr), Node::Tm(T::RightSquiggly)] => {
                Ok(NonBinExpr::WhileExpr(
                    Box::new(Expr::new(cond)?),
                    Box::new(Expr::new(expr)?),
                ))
            }
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::WhileExpr, children));
            }
        }
    }
    fn do_while(children: &Vec<Node<BudTerminal, BudNonTerminal>>) -> Result<NonBinExpr, String> {
        match &children[..] {
            [Node::Tm(T::Do), Node::Tm(T::LeftSquiggly), Node::NonTm(N::Expr, expr), Node::Tm(T::RightSquiggly), Node::NonTm(N::Expr, cond), Node::Tm(T::Semicolon)] => {
                Ok(NonBinExpr::DoWhile(
                    Box::new(Expr::new(expr)?),
                    Box::new(Expr::new(cond)?),
                ))
            }
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::DoWhile, children));
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum IdExpr {
    SquareIndex(Box<Expr>, Box<Expr>),
    RoundIndex(Box<Expr>, Option<Exprs>),
    Id(Id),
}
impl IdExpr {
    pub fn new(children: &BudNodes) -> Result<IdExpr, String> {
        match &children[..] {
            [Node::NonTm(N::Expr, arr), Node::Tm(T::LeftSquare), Node::NonTm(N::Expr, ind), Node::Tm(T::RightSquare)] => {
                Ok(IdExpr::SquareIndex(
                    Box::new(Expr::new(arr)?),
                    Box::new(Expr::new(ind)?),
                ))
            }
            [Node::NonTm(N::Expr, arr), Node::Tm(T::LeftRound), Node::NonTm(N::Exprs, args), Node::Tm(T::RightRound)] => {
                Ok(IdExpr::RoundIndex(
                    Box::new(Expr::new(arr)?),
                    Some(Expr::news(args)?),
                ))
            }
            [Node::NonTm(N::Expr, arr), Node::Tm(T::LeftRound), Node::Tm(T::RightRound)] => {
                Ok(IdExpr::RoundIndex(Box::new(Expr::new(arr)?), None))
            }
            [Node::Tm(T::Id(id))] => Ok(IdExpr::Id(id.to_owned())),
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::IdExpr, children));
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeExpr {
    Id(Id),
    Array(Box<TypeExpr>, i32),
    Pointer(Box<TypeExpr>),
}
impl TypeExpr {
    pub fn new(children: &BudNodes) -> Result<TypeExpr, String> {
        match &children[..] {
            [Node::Tm(T::Id(id))] => Ok(TypeExpr::Id(id.to_owned())),
            [Node::NonTm(N::TypeExpr, typ), Node::Tm(T::LeftSquare), Node::Tm(T::Num(num)), Node::Tm(T::RightSquare)] => {
                Ok(TypeExpr::Array(Box::new(TypeExpr::new(typ)?), *num))
            }
            [Node::Tm(T::Star), Node::Tm(T::LeftRound), Node::NonTm(N::TypeExpr, typ), Node::Tm(T::RightRound)] => {
                Ok(TypeExpr::Pointer(Box::new(TypeExpr::new(typ)?)))
            }
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::TypeExpr, children));
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Num(i32),
    Str(String),
}
impl Literal {
    pub fn new(children: &BudNodes) -> Result<Literal, String> {
        match &children[..] {
            [Node::Tm(T::Num(num))] => Ok(Literal::Num(*num)),
            [Node::Tm(T::Str(string))] => Ok(Literal::Str(string.to_owned())),
            [Node::Tm(T::Char(char))] => Ok(Literal::Num((Self::decode_char(char)? as u8).into())),
            _ => {
                return Err(format!("Invalid node for {} {:?}", N::LitExpr, children));
            }
        }
    }

    fn decode_char(s: &str) -> Result<char, String> {
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
                    Err(format!("Invalid character, '{}'", s))
                } else {
                    Ok(s.chars().next().unwrap())
                }
            }
        }
    }
}
