/// <reference types="tree-sitter-cli/dsl" />

const {expr_rules} = require('./rules')

module.exports = grammar({
  name: 'borlang_expr',
  word: $ => $.ident,
  extras: _ => [/\s+/],
  rules: expr_rules,
})
