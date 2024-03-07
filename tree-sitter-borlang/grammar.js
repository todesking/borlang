/// <reference types="tree-sitter-cli/dsl" />
module.exports = grammar({
  name: 'borlang',
  word: $ => $.ident,
  extras: $ => [/\s+/],
  rules: {
    program: $ => field('top_terms', repeat($._top_term)),
    _top_term: $ => choice($.top_term_let),
    top_term_let: $ => seq(
      'let',
      field('name', $.ident),
      '=',
      field('expr', $._expr),
      ';'),
    _expr: $ => choice(
      $.expr_int,
      $.expr_var,
      $.expr_binop,
      $.expr_block,
      $.expr_let,
    ),
    expr_int: $ => /[0-9]+/,
    expr_var: $ => field('ident', $.ident),
    expr_binop: $ => choice(
      prec.left(2, seq(
        field("lhs", $._expr),
        field("op", $.op_mul),
        field("rhs", $._expr))),
      prec.left(1, seq(
        field("lhs", $._expr),
        field("op", choice($.op_plus, $.op_minus)),
        field("rhs", $._expr))),
    ),
    expr_block: $ => seq(
      '{',
      repeat(seq(field('terms', $._expr), ';')),
      field('expr', optional($._expr)),
      '}',
    ),
    expr_let: $ => seq(
      'let',
      field('name', $.ident),
      '=',
      field('expr', $._expr)),
    ident: $ => /[A-Za-z_][a-z0-9_]*/,
    op_plus: $ => '+',
    op_minus: $ => '-',
    op_mul: $ => '*',
  }
})
