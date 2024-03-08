/// <reference types="tree-sitter-cli/dsl" />

const {program_rules} = require('./rules')

module.exports = grammar({
  name: 'borlang',
  word: $ => $.ident,
  extras: _ => [/\s+/],
  rules: program_rules,
})
