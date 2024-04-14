use regex::Regex;
use serde::Deserialize;
use std::{error::Error, fmt::Display, rc::Rc, sync::OnceLock};

#[derive(Debug, PartialEq, Eq, Clone, Deserialize)]
#[serde(rename = "program")]
pub struct Program {
    pub top_terms: Vec<TopTerm>,
}

#[derive(Debug, PartialEq, Eq, Clone, Deserialize)]
#[serde(rename = "top_term")]
pub enum TopTerm {
    Let { name: Ident, expr: Expr },
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord, Deserialize)]
#[serde(rename = "ident")]
pub struct Ident(#[serde(deserialize_with = "deserialize_rc_string")] Rc<String>);
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
    #[serde(rename = "obj_item_kv")]
    Kv {
        name: Ident,
        expr: Option<Box<Expr>>,
    },
    #[serde(rename = "obj_item_spread")]
    Spread(Box<Expr>),
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
    #[serde(rename = "expr_block")]
    Block {
        terms: Vec<Expr>,
        expr: Option<Box<Expr>>,
    },
    #[serde(rename = "expr_let")]
    Let { name: Ident, expr: Box<Expr> },
    #[serde(rename = "expr_reassign")]
    Reassign { lhs: Box<Expr>, rhs: Box<Expr> },
    #[serde(rename = "expr_if")]
    If {
        cond: Box<Expr>,
        th: Box<Expr>,
        el: Option<Box<Expr>>,
    },
    #[serde(rename = "expr_for")]
    For {
        name: Ident,
        target: Box<Expr>,
        body: Box<Expr>,
    },
    #[serde(rename = "expr_fun")]
    Fun { params: Vec<Ident>, expr: Box<Expr> },
    #[serde(rename = "expr_paren")]
    Paren { expr: Box<Expr> },
    #[serde(rename = "expr_prop")]
    Prop { expr: Box<Expr>, name: Ident },
    #[serde(rename = "expr_prop_opt")]
    PropOpt { expr: Box<Expr>, name: Ident },
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
