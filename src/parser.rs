use std::{error::Error, sync::OnceLock};

use regex::Regex;
use tree_sitter::Node;

use crate::{
    ast::{Expr, Ident, Program, TopTerm},
    parser_impl,
    value::AtomValue,
};

pub fn parse_program(src: &str) -> Result<Program, Box<dyn Error>> {
    let language = parser_impl::language_program();

    let mut ts = tree_sitter::Parser::new();
    ts.set_language(&language)?;

    let tree = ts
        .parse(src, None)
        .expect("tree_sitter's parse() should return tree");

    to_program(tree.root_node(), src)
}

pub fn parse_expr(src: &str) -> Result<Expr, Box<dyn Error>> {
    let language = parser_impl::language_expr();

    let mut ts = tree_sitter::Parser::new();
    ts.set_language(&language)?;

    let tree = ts
        .parse(src, None)
        .expect("tree_sitter's parse() should return tree");

    get_one(tree.root_node(), "expr", src, to_expr)
}

fn get_many<'a, 'src, T, F>(
    node: Node<'a>,
    field_name: &'static str,
    src: &'src str,
    mut f: F,
) -> Result<Vec<T>, Box<dyn Error>>
where
    F: FnMut(Node<'a>, &'src str) -> Result<T, Box<dyn Error>>,
{
    validate_node(node)?;
    let mut cur = node.walk();
    let mut values = Vec::new();
    for c in node.children_by_field_name(field_name, &mut cur) {
        let v = f(c, src)?;
        values.push(v);
    }
    Ok(values)
}
fn get_one<'a, 'src, T, F>(
    node: Node<'a>,
    field_name: &'static str,
    src: &'src str,
    f: F,
) -> Result<T, Box<dyn Error>>
where
    F: FnMut(Node<'a>, &'src str) -> Result<T, Box<dyn Error>>,
{
    let mut vs = get_many(node, field_name, src, f)?;
    if vs.len() != 1 {
        return Err(format!(
            "Field {field_name}: len({}) != 1, at {:?}",
            vs.len(),
            node.range()
        )
        .into());
    }
    Ok(vs.pop().unwrap())
}
fn get_option<'a, 'src, T, F>(
    node: Node<'a>,
    field_name: &'static str,
    src: &'src str,
    f: F,
) -> Result<Option<T>, Box<dyn Error>>
where
    F: FnMut(Node<'a>, &'src str) -> Result<T, Box<dyn Error>>,
{
    let mut vs = get_many(node, field_name, src, f)?;
    if vs.len() > 1 {
        return Err(format!("Field {field_name}: len({}) != 1", vs.len()).into());
    }
    Ok(vs.pop())
}

fn unexpected_node(node: Node<'_>) -> ! {
    panic!("Parse error: Unexpected node: kind={}", node.kind())
}

fn validate_node(node: Node<'_>) -> Result<(), Box<dyn Error>> {
    if node.is_error() {
        return Err(format!("Error(is_error): {:?} {}", node.byte_range(), node.kind()).into());
    }
    if node.has_error() {
        return Err(format!("Error(has_error): {:?} {}", node.byte_range(), node.kind()).into());
    }
    Ok(())
}

fn validate_kind(node: Node<'_>, kind: &str) -> Result<(), Box<dyn Error>> {
    if node.kind() != kind {
        return Err(format!("Kind error: expected={}, actual={}", kind, node.kind()).into());
    }
    Ok(())
}

fn to_program(node: Node<'_>, src: &'_ str) -> Result<Program, Box<dyn Error>> {
    validate_node(node)?;
    validate_kind(node, "program")?;
    Ok(Program {
        top_terms: get_many(node, "top_terms", src, to_top_term)?,
    })
}

fn to_top_term(node: Node<'_>, src: &'_ str) -> Result<TopTerm, Box<dyn Error>> {
    validate_node(node)?;
    match node.kind() {
        "top_term_let" => Ok(TopTerm::Let {
            name: get_one(node, "name", src, to_ident)?,
            expr: get_one(node, "expr", src, to_expr)?,
        }),
        _ => unexpected_node(node),
    }
}

fn to_string(node: Node<'_>, src: &'_ str) -> Result<String, Box<dyn Error>> {
    validate_node(node)?;
    let bytes = src.as_bytes()[node.start_byte()..node.end_byte()].to_vec();
    String::from_utf8(bytes).map_err(|e| e.into())
}

fn to_str<'a>(node: Node<'_>, src: &'a str) -> Result<&'a str, Box<dyn Error>> {
    validate_node(node)?;
    Ok(&src[node.byte_range()])
}

fn to_ident(node: Node<'_>, src: &'_ str) -> Result<Ident, Box<dyn Error>> {
    Ok(Ident::new(to_string(node, src)?))
}

fn to_expr(node: Node<'_>, src: &'_ str) -> Result<Expr, Box<dyn Error>> {
    validate_node(node)?;
    match node.kind() {
        "expr_int" => Ok(Expr::AtomValue(AtomValue::Int(
            to_string(node, src)?.parse()?,
        ))),
        "expr_var" => Ok(Expr::Var(get_one(node, "ident", src, to_ident)?)),
        "expr_paren" => get_one(node, "expr", src, to_expr),
        "expr_binop" => Ok(Expr::App {
            f: Box::new(Expr::Var(get_one(node, "op", src, to_ident)?)),
            args: vec![
                get_one(node, "lhs", src, to_expr)?,
                get_one(node, "rhs", src, to_expr)?,
            ],
        }),
        "expr_object" => {
            let names = get_many(node, "name", src, to_ident)?;
            let exprs = get_many(node, "expr", src, to_expr)?;
            assert!(names.len() == exprs.len());
            let mut v = Vec::new();
            for (name, expr) in names.into_iter().zip(exprs) {
                v.push((name, expr));
            }
            Ok(Expr::Object { exprs: v })
        }
        "expr_block" => Ok(Expr::Block {
            terms: get_many(node, "terms", src, to_expr)?,
            expr: get_option(node, "expr", src, to_expr)?.map(Box::new),
        }),
        "expr_array" => Ok(Expr::Array(get_many(node, "expr", src, to_expr)?)),
        "expr_let" => Ok(Expr::Let {
            name: get_one(node, "name", src, to_ident)?,
            expr: Box::new(get_one(node, "expr", src, to_expr)?),
        }),
        "expr_fun" => Ok(Expr::Fun {
            params: get_many(node, "params", src, to_ident)?,
            expr: Box::new(get_one(node, "expr", src, to_expr)?),
        }),
        "expr_app" => Ok(Expr::App {
            f: Box::new(get_one(node, "expr", src, to_expr)?),
            args: get_many(node, "args", src, to_expr)?,
        }),
        "expr_reassign" => Ok(Expr::Reassign {
            name: get_one(node, "name", src, to_ident)?,
            expr: Box::new(get_one(node, "expr", src, to_expr)?),
        }),
        "expr_if" => Ok(Expr::If {
            cond: Box::new(get_one(node, "cond", src, to_expr)?),
            th: Box::new(get_one(node, "th", src, to_expr)?),
            el: get_option(node, "el", src, to_expr)?.map(Box::new),
        }),
        "expr_prop" => Ok(Expr::Prop {
            expr: Box::new(get_one(node, "expr", src, to_expr)?),
            name: get_one(node, "name", src, to_ident)?,
        }),
        "expr_str" => Ok(Expr::AtomValue(AtomValue::str(parse_content(
            get_option(node, "content", src, to_str)?.unwrap_or(""),
        )?))),
        "expr_index" => Ok(Expr::Index {
            expr: Box::new(get_one(node, "expr", src, to_expr)?),
            index: Box::new(get_one(node, "index", src, to_expr)?),
        }),
        "expr_neg" => Ok(Expr::App {
            f: Box::new(Expr::Var("#unary-".into())),
            args: vec![get_one(node, "expr", src, to_expr)?],
        }),
        _ => unexpected_node(node),
    }
}

static STR_CONTENT_RE: OnceLock<Regex> = OnceLock::new();

fn parse_content(s: &str) -> Result<String, Box<dyn Error>> {
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
