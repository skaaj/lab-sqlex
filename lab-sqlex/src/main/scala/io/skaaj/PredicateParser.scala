package io.skaaj

import fastparse._, fastparse.NoWhitespace._

object PredicateParser {
  def apply[_: P]: P[Expression] = P(orExpression ~ End)

  private def ws[_: P]: P[Unit] = P(CharIn(" \t").rep(1))
  def identifier[_: P]: P[Expression] = P(CharIn("a-z0-9").rep(1)).!.map(Identifier)
  def parens[_: P]: P[Expression] = P("(" ~ orExpression ~ ")")
  def boolValue[_: P]: P[Expression] = P("true" | "false").!.map(x => Constant(x.toBoolean))
  def handSide[_: P]: P[Expression] = P(notExpression | boolValue | identifier | parens)
  def notExpression[_: P]: P[Expression] = P("not" ~ ws ~ handSide).map(Not)
  def andExpression[_: P]: P[Expression] = P(handSide ~ (ws ~ "and" ~ ws ~ handSide).rep).map {
    case (x, xs) => xs.foldLeft(x) {
      case (left, right) => And(left, right)
    }
  }

  def orExpression[_: P]: P[Expression] = P(andExpression ~ (ws ~ "or" ~ ws ~ andExpression).rep).map {
    case (x, xs) => xs.foldLeft(x) {
      case (left, right) => Or(left, right)
    }
  }
}
