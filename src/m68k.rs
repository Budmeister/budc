use crate::logging::LoggingOptions;
use crate::{bud, parse::Node};
use crate::bud::{BudTerminal::*, BudNonTerminal::{*, self}};
use log::{trace, warn};

pub struct BudExpander {

}
impl BudExpander {
    const BUILT_IN_TYPES: [&str; 3] = [
        "i8",
        "i16",
        "i32",
    ];

    pub fn new() -> BudExpander {
        BudExpander{}
    }

    pub fn code_generate(&self, log_options: &LoggingOptions, mut tree: Node<bud::BudTerminal, bud::BudNonTerminal>) -> Result<Code, String> {
        let children;
        if let Node::NonTm(bud::BudNonTerminal::Items, children_) = tree {
            children = children_;
        } else {
            return Err(format!("Invalid node for {} {:?}", BudNonTerminal::Items, tree));
        }
        trace!("Building Item list");
        let items = bud::Item::news(&children)?;
        trace!("{:?}", items);
        todo!()
    }
}

pub enum GPReg {
    D0,
    D1,
    D2,
    D3,
    D4,
    D5,
    D6,
    D7,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    SP,
}

type BudNode = Node<bud::BudTerminal, bud::BudNonTerminal>;
type BudNodes = Vec<BudNode>;

#[derive(Copy, Clone)]
pub enum Instruction {

}

#[derive(Clone)]
pub struct Field {
    pub typ: Type,
    pub name: String,
}
impl Field {
    pub fn new(children: BudNodes) -> Result<Field, String> {
        todo!()
    }
}

#[derive(Clone)]
pub struct Struct {
    pub fields: Vec<Field>,
    pub name: String,
}

#[derive(Clone)]
pub enum Type {
    Struct(Struct),
    Pointer(Box<Type>),
    Array(Box<Type>, i32),
    Id(String),
}

#[derive(Clone)]
pub struct Function {
    pub args: Vec<Field>,
    pub name: Field,
    pub instructions: Vec<Instruction>,
}
impl Function {
    fn new(children: BudNodes) -> Result<Function, String> {
        match &children[..] {
            [
                Node::NonTm(VarDecl, name),
                Node::NonTm(VarDeclsPar, args),
                Node::NonTm(Expr, expr)
            ] => {
                todo!()
            }
            _ => {
                return Err(format!("Invalid node for Function: {:?}", children));
            }
        }

    }
}

pub struct Code {
    pub globals: Vec<Field>,
    pub types: Vec<Type>,
    pub funs: Vec<Function>,
}

