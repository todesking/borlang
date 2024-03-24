/// <reference types="tree-sitter-cli/dsl" />

const {repsep} = require('./util')

const Prec = {
  reassign: 0,
  let: 0,
  eqeq: 1,
  add: 2,
  mul: 3,

  index: 8,
  prop: 9,

  block: 0,
  object: 1,
}

/**
 * @type {Grammar<string>['rules']}
 */
const common_rules = {
  _expr: $ => choice(
    $.expr_int,
    $.expr_str,
    $.expr_var,
    $.expr_object,
    $.expr_paren,
    $.expr_array,
    $.expr_binop,
    $.expr_neg,
    $.expr_block,
    $.expr_let,
    $.expr_reassign,
    $.expr_app,
    $.expr_fun,
    $.expr_if,
    $.expr_prop,
    $.expr_index,
  ),
  expr_int: _ => /[0-9]+/,
  expr_str: $ => seq(
    '"',
    optional(field('content', $.str_body)),
    '"',
  ),
  str_body: _ => repeat1(choice(/[^\\]/, /\\./)),
  expr_var: $ => field('ident', $.ident),
  expr_paren: $ => seq('(', field('expr', $._expr), ')'),
  expr_binop: $ => choice(
    prec.left(Prec.eqeq, seq(
      field('lhs', $._expr),
      field('op', choice($.op_eqeq, $.op_neq)),
      field('rhs', $._expr),
    )),
    prec.left(Prec.mul, seq(
      field("lhs", $._expr),
      field("op", choice($.op_mul, $.op_mod)),
      field("rhs", $._expr))),
    prec.left(Prec.add, seq(
      field("lhs", $._expr),
      field("op", choice($.op_plus, $.op_minus)),
      field("rhs", $._expr))),
  ),
  expr_neg: $ => prec.left(seq(
    '-',
    field('expr', $._expr)
  )),
  expr_object: $ => prec(Prec.object, seq(
    '{',
    repsep(
      seq(
        field('name', $.ident),
        ':',
        field('expr', $._expr),
      ),
      ',',
    ),
    '}',
  )),
  expr_block: $ => prec(Prec.block, seq(
    '{',
    repeat(seq(field('terms', $._expr), ';')),
    field('expr', optional($._expr)),
    '}',
  )),
  expr_array: $ => seq(
    '[',
    repsep(field('expr', $._expr), ','),
    ']',
  ),
  expr_let: $ => prec(Prec.let, seq(
    'let',
    field('name', $.ident),
    '=',
    field('expr', $._expr))),
  expr_reassign: $ => prec.left(Prec.reassign, seq(
    field("name", $.ident),
    '=',
    field('expr', $._expr),
  )),
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
  expr_if: $ => seq(
    'if',
    field('cond', $._expr),
    field('th', $.expr_block),
    optional(seq(
      'else',
      field('el', $.expr_block),
    )),
  ),
  expr_prop: $ => prec(Prec.prop, seq(
    field('expr', $._expr),
    '.',
    field('name', $.ident),
  )),
  expr_index: $ => prec.left(Prec.index, seq(
    field('expr', $._expr),
    '[',
    field('index', $._expr),
    ']',
  )),
  ident: _ => /[A-Za-z_][a-z0-9_]*/,
  op_plus: _ => '+',
  op_minus: _ => '-',
  op_mul: _ => '*',
  op_mod: _ => '%',
  op_eqeq: _ => '==',
  op_neq: _ => '!=',
}

module.exports.program_grammar = grammar({
  name: 'borlang',
  word: $ => $.ident,
  rules: {
    program: $ => field('top_terms', repeat($._top_term)),
    _top_term: $ => choice($.top_term_let),
    top_term_let: $ => seq(
      'let',
      field('name', $.ident),
      '=',
      field('expr', $._expr),
      ';'),
    ...common_rules,
  },
})

module.exports.expr_grammar = grammar({
  name: 'borlang_expr',
  word: $ => $.ident,
  rules: {
    expr: $ => field('expr', $._expr),
    ...common_rules,
  },
})
