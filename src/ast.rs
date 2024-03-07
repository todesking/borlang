use crate::value::Value;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub top_terms: Vec<TopTerm>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TopTerm {
    Let { name: Ident, expr: Expr },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ident(pub String);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Value(Value),
    Var(Ident),
    App {
        f: Box<Expr>,
        args: Vec<Expr>,
    },
    Block {
        terms: Vec<Expr>,
        expr: Option<Box<Expr>>,
    },
    Let {
        name: Ident,
        expr: Box<Expr>,
    },
    Fun {
        params: Vec<Ident>,
        expr: Box<Expr>,
    },
}
