/// <reference types="tree-sitter-cli/dsl" />

const {repsep} = require('./util')

/**
 * @type {Grammar<string>['rules']}
 */
const common_rules = {
  _expr: $ => choice(
    $.expr_int,
    $.expr_var,
    $.expr_paren,
    $.expr_binop,
    $.expr_block,
    $.expr_let,
    $.expr_app,
    $.expr_fun,
  ),
  expr_int: $ => /[0-9]+/,
  expr_var: $ => field('ident', $.ident),
  expr_paren: $ => seq('(', field('expr', $._expr), ')'),
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
  expr_let: $ => prec(0, seq(
    'let',
    field('name', $.ident),
    '=',
    field('expr', $._expr))),
  expr_app: $ => prec(9, seq(
    field('expr', $._expr),
    '(',
    repsep(field('args', $._expr), ','),
    ')'
  )),
  expr_fun: $ => seq(
    'fn',
    '(',
    repsep(field('params', $.ident), ','),
    ')',
    '=>',
    field('expr', $._expr),
  ),
  ident: $ => /[A-Za-z_][a-z0-9_]*/,
  op_plus: $ => '+',
  op_minus: $ => '-',
  op_mul: $ => '*',
}

/**
 * @type {Grammar<string>['rules']}
 */
const program_rules = {
  program: $ => field('top_terms', repeat($._top_term)),
  _top_term: $ => choice($.top_term_let),
  top_term_let: $ => seq(
    'let',
    field('name', $.ident),
    '=',
    field('expr', $._expr),
    ';'),
  ...common_rules,
}
module.exports.program_rules = program_rules;

/**
 * @type {Grammar<string>['rules']}
 */
const expr_rules = {
  expr: $ => field('expr', $._expr),
  ...common_rules,
}
module.exports.expr_rules = expr_rules;
