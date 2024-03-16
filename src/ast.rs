use std::{fmt::Display, rc::Rc};

use crate::value::AtomValue;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub top_terms: Vec<TopTerm>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TopTerm {
    Let { name: Ident, expr: Expr },
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub struct Ident(Rc<String>);
impl Ident {
    pub fn new<S: Into<String>>(s: S) -> Ident {
        Ident(Rc::new(s.into()))
    }
}
impl<S: Into<String>> From<S> for Ident {
    fn from(value: S) -> Self {
        Ident::new(value)
    }
}
impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Object {
        exprs: Vec<(Ident, Expr)>,
    },
    Array(Vec<Expr>),
    AtomValue(AtomValue),
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
    Reassign {
        name: Ident,
        expr: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        th: Box<Expr>,
        el: Option<Box<Expr>>,
    },
    Fun {
        params: Vec<Ident>,
        expr: Box<Expr>,
    },
    Prop {
        expr: Box<Expr>,
        name: Ident,
    },
}
