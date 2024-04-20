/// <reference types="tree-sitter-cli/dsl" />

const {repsep} = require('./util')

const Prec = {
  reassign: 0,
  let: 0,
  compare: 1,
  add: 2,
  mul: 3,

  index: 8,
  prop: 9,

  block: 0,
  object: 1,

  expr_var: 0,
  obj_item_kv: 1,
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
    $.expr_for,
    $.expr_prop,
    $.expr_prop_opt,
    $.expr_index,
    $.expr_import,
  ),
  expr_int: _ => /[0-9]+/,
  expr_str: $ => seq(
    '"',
    optional(field('content', $.str_body)),
    '"',
  ),
  str_body: _ => repeat1(choice(/[^\\]/, /\\./)),
  expr_var: $ => prec(Prec.expr_var, field('ident', $.ident)),
  expr_paren: $ => seq('(', field('expr', $._expr), ')'),
  expr_binop: $ => choice(
    prec.left(Prec.compare, seq(
      field('lhs', $._expr),
      field('op', choice($.op_eqeq, $.op_neq, $.op_gt, $.op_ge, $.op_lt, $.op_le)),
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
      $._obj_item,
      ',',
    ),
    '}',
  )),
  _obj_item: $ => choice($.obj_item_kv, $.obj_item_spread),
  obj_item_kv: $ => prec(Prec.obj_item_kv, seq(
    field('name', $.ident),
    optional(seq(
      ':',
      field('expr', $._expr),
    ))),
  ),
  obj_item_spread: $ => seq(
    '..',
    $._expr,
  ),
  expr_block: $ => prec(Prec.block, seq(
    '{',
    repeat(seq(field('terms', $._expr), ';')),
    field('expr', optional($._expr)),
    '}',
  )),
  expr_array: $ => seq(
    '[',
    repsep(field('item', $.array_item), ','),
    ']',
  ),
  array_item: $ => seq(field('spread', optional($.op_spread)), field('expr', $._expr)),
  expr_let: $ => prec(Prec.let, seq(
    'let',
    field('name', $._let_pattern),
    '=',
    field('expr', $._expr))),
  _let_pattern: $ => choice($.let_pattern_obj, $.let_pattern_arr, $.let_pattern_name),
  let_pattern_obj: $ => seq(
    '{',
    repsep(field('name', $.ident), ','),
    optional(seq('..', field('rest', $.ident))),
    '}',
  ),
  let_pattern_arr: $ => seq(
    '[',
    repsep(field('name', $.ident), ','),
    optional(seq('..', field('rest', $.ident))),
    ']',
  ),
  let_pattern_name: $ => $.ident,
  expr_reassign: $ => prec.left(Prec.reassign, seq(
    field('lhs', choice(
      $.expr_var,
      $.expr_index,
      $.expr_prop,
    )),
    '=',
    field('rhs', $._expr),
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
  expr_for: $ => seq(
    'for',
    field('name', $.ident),
    'in',
    field('target', $._expr),
    field('body', $.expr_block),
  ),
  expr_prop: $ => prec(Prec.prop, seq(
    field('expr', $._expr),
    '.',
    field('name', $.ident),
  )),
  expr_prop_opt: $ => prec(Prec.prop, seq(
    field('expr', $._expr),
    '?.',
    field('name', $.ident),
  )),
  expr_index: $ => prec.left(Prec.index, seq(
    field('expr', $._expr),
    '[',
    field('index', $._expr),
    ']',
  )),
  expr_import: $ => seq(
    'import',
    $.expr_str
  ),
  ident: _ => /[A-Za-z_][a-z0-9_]*/,
  op_plus: _ => '+',
  op_minus: _ => '-',
  op_mul: _ => '*',
  op_mod: _ => '%',
  op_eqeq: _ => '==',
  op_neq: _ => '!=',
  op_lt: _ => '<',
  op_le: _ => '<=',
  op_gt: _ => '>',
  op_ge: _ => '>=',
  op_spread: _ => '..',
}

module.exports.program_grammar = grammar({
  name: 'borlang',
  word: $ => $.ident,
  rules: {
    program: $ => field('top_terms', repeat($._top_term)),
    _top_term: $ => choice($.top_term_let, $.top_term_sym),
    top_term_let: $ => seq(
      optional(field('pub', 'pub')),
      'let',
      field('name', $._let_pattern),
      '=',
      field('expr', $._expr),
      ';'),
    top_term_sym: $ => seq(
      optional(field('pub', 'pub')),
      'sym',
      field('name', $.ident),
      ';'
    ),
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
