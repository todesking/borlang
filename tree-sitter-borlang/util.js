/// <reference types="tree-sitter-cli/dsl" />

/**
 * @param item {RuleOrLiteral}
 * @param sep {RuleOrLiteral}
 * @returns {RuleOrLiteral}
 */
module.exports.repsep = function repsep(item, sep) {
  return optional(
    seq(
      item,
      repeat(seq(sep, item)),
      optional(sep),
    )
  )
}
