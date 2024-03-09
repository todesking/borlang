use crate::value::AtomValue;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub top_terms: Vec<TopTerm>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TopTerm {
    Let { name: Ident, expr: Expr },
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Ident(pub String);
impl<S: Into<String>> From<S> for Ident {
    fn from(value: S) -> Self {
        Ident(value.into())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
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
    Fun {
        params: Vec<Ident>,
        expr: Box<Expr>,
    },
}
