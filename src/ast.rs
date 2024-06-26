use regex::Regex;
use serde::Deserialize;
use std::{error::Error, fmt::Display, rc::Rc, sync::OnceLock};

use crate::value::ObjectKey;

#[derive(Debug, PartialEq, Eq, Clone, Deserialize)]
#[serde(rename = "program")]
pub struct Program {
    pub top_terms: Vec<TopTerm>,
}

#[derive(Debug, PartialEq, Eq, Clone, Deserialize)]
pub enum TopTerm {
    #[serde(rename = "top_term_let")]
    Let {
        #[serde(rename = "pub")]
        is_pub: Option<()>,
        name: LetPattern,
        expr: Expr,
    },
    #[serde(rename = "top_term_sym")]
    Sym {
        #[serde(rename = "pub")]
        is_pub: Option<()>,
        name: Ident,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord, Deserialize)]
#[serde(rename = "ident")]
pub struct Ident(#[serde(deserialize_with = "deserialize_rc_string")] pub Rc<String>);
fn deserialize_rc_string<'de, D: serde::Deserializer<'de>>(
    deserializer: D,
) -> Result<Rc<String>, D::Error> {
    Ok(String::deserialize(deserializer)?.into())
}
fn deserialize_ident<'de, D: serde::Deserializer<'de>>(deserializer: D) -> Result<Ident, D::Error> {
    Ok(String::deserialize(deserializer)?.into())
}
impl Ident {
    pub fn new<S: Into<String>>(s: S) -> Ident {
        Ident(Rc::new(s.into()))
    }
    pub fn to_object_key(&self) -> ObjectKey {
        ObjectKey::Str(self.0.clone())
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

#[derive(Debug, PartialEq, Eq, Clone, Deserialize)]
#[serde(rename = "array_item")]
pub struct ArrayItem {
    pub spread: Option<()>,
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone, Deserialize)]
pub enum ObjItem {
    #[serde(rename = "obj_item_const")]
    Const { key: Ident, expr: Option<Box<Expr>> },
    #[serde(rename = "obj_item_dyn")]
    Dyn { key: Box<Expr>, expr: Box<Expr> },
    #[serde(rename = "obj_item_spread")]
    Spread(Box<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone, Deserialize)]
#[serde(rename = "block")]
pub struct Block {
    pub terms: Vec<Expr>,
    pub expr: Option<Box<Expr>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Deserialize)]
pub enum Expr {
    #[serde(rename = "expr_object")]
    Object(Vec<ObjItem>),
    #[serde(rename = "expr_array")]
    Array(Vec<ArrayItem>),
    #[serde(rename = "expr_int")]
    Int(i32),
    #[serde(rename = "expr_str")]
    Str {
        #[serde(deserialize_with = "deserialize_string_content_opt")]
        content: Rc<String>,
    },
    #[serde(rename = "expr_var")]
    Var(Ident),
    #[serde(rename = "expr_app")]
    App { expr: Box<Expr>, args: Vec<Expr> },
    #[serde(rename = "expr_do")]
    Do(Block),
    #[serde(rename = "expr_let")]
    Let { name: LetPattern, expr: Box<Expr> },
    #[serde(rename = "expr_reassign")]
    Reassign { lhs: Box<Expr>, rhs: Box<Expr> },
    #[serde(rename = "expr_if")]
    If {
        cond: Box<Expr>,
        th: Block,
        el: Option<Block>,
    },
    #[serde(rename = "expr_for")]
    For {
        name: Ident,
        target: Box<Expr>,
        body: Block,
    },
    #[serde(rename = "expr_fun")]
    Fun { params: Vec<Ident>, expr: Box<Expr> },
    #[serde(rename = "expr_paren")]
    Paren { expr: Box<Expr> },
    #[serde(rename = "expr_prop")]
    Prop { expr: Box<Expr>, prop: PropSpec },
    #[serde(rename = "expr_prop_opt")]
    PropOpt { expr: Box<Expr>, prop: PropSpec },
    #[serde(rename = "expr_index")]
    Index { expr: Box<Expr>, index: Box<Expr> },
    #[serde(rename = "expr_binop")]
    Binop {
        lhs: Box<Expr>,
        #[serde(deserialize_with = "deserialize_ident")]
        op: Ident,
        rhs: Box<Expr>,
    },
    #[serde(rename = "expr_neg")]
    Negate { expr: Box<Expr> },
    #[serde(rename = "expr_not")]
    Not { expr: Box<Expr> },
    #[serde(rename = "expr_import")]
    Import(ExprStr),
    #[serde(rename = "expr_catch")]
    Catch {
        expr: Box<Expr>,
        name: Ident,
        catch_expr: Box<Expr>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Deserialize)]
#[serde(rename = "expr_str")]
pub struct ExprStr {
    #[serde(deserialize_with = "deserialize_string_content_opt")]
    pub content: Rc<String>,
}

#[derive(Debug, PartialEq, Eq, Clone, Deserialize)]
pub enum PropSpec {
    #[serde(rename = "prop_spec_const")]
    Const(String),
    #[serde(rename = "prop_spec_dyn")]
    Dyn(Box<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone, Deserialize)]
pub enum LetPattern {
    #[serde(rename = "let_pattern_name")]
    Name(Ident),
    #[serde(rename = "let_pattern_obj")]
    Obj {
        name: Vec<Ident>,
        rest: Option<Ident>,
    },
    #[serde(rename = "let_pattern_arr")]
    Arr {
        name: Vec<Ident>,
        rest: Option<Ident>,
    },
}

fn deserialize_string_content_opt<'de, D: serde::Deserializer<'de>>(
    deserializer: D,
) -> Result<Rc<String>, D::Error> {
    let c = Option::<String>::deserialize(deserializer)?;
    let Some(c) = c else {
        return Ok(Rc::new("".to_owned()));
    };
    let s = parse_str_content(&c).map_err(<D::Error as serde::de::Error>::custom)?;
    Ok(s.into())
}

static STR_CONTENT_RE: OnceLock<Regex> = OnceLock::new();

fn parse_str_content(s: &str) -> Result<String, Box<dyn Error>> {
    let re = STR_CONTENT_RE.get_or_init(|| Regex::new(r#"\\(.)"#).unwrap());
    try_replace_all(re, s, |captures| match &captures[1] {
        "\\" => Ok("\\"),
        "n" => Ok("\n"),
        r#"""# => Ok(r#"""#),
        other => Err(format!("Invalid escape sequence: \\{}", other).into()),
    })
}
fn try_replace_all<E>(
    re: &Regex,
    haystack: &str,
    replacement: impl Fn(&regex::Captures) -> Result<&'static str, E>,
) -> Result<String, E> {
    let mut new = String::with_capacity(haystack.len());
    let mut last_match = 0;
    for caps in re.captures_iter(haystack) {
        let m = caps.get(0).unwrap();
        new.push_str(&haystack[last_match..m.start()]);
        new.push_str(replacement(&caps)?);
        last_match = m.end();
    }
    new.push_str(&haystack[last_match..]);
    Ok(new)
}
