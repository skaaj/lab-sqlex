package io.skaaj

import fastparse.NoWhitespace._
import fastparse._

object PredicateParser {
  def apply[_: P]: P[Expression] = P(orExpression ~ End)

  private def ws[_: P]: P[Unit] =
    P(CharIn(" \t").rep(1))

  private def identifier[_: P]: P[Expression] =
    P(CharIn("a-z0-9").rep(1)).!.map(Identifier)

  private def parens[_: P]: P[Expression] =
    P("(" ~ orExpression ~ ")")

  private def constant[_: P]: P[Expression] =
    P("true" | "false").!.map(x => Constant(x.toBoolean))

  private def handSide[_: P]: P[Expression] =
    P(notExpression | constant | identifier | parens)

  private def notExpression[_: P]: P[Expression] =
    P("not" ~ ws ~ handSide).map(Not)

  private def andExpression[_: P]: P[Expression] =
    P(handSide ~ (ws ~ IgnoreCase("and").! ~ ws ~ handSide).rep).map(buildBinaryExpression)

  private def orExpression[_: P]: P[Expression] =
    P(andExpression ~ (ws ~ IgnoreCase("or").! ~ ws ~ andExpression).rep).map(buildBinaryExpression)

  private def buildBinaryExpression(captured: (Expression, Seq[(String, Expression)])): Expression = captured match {
    case (x, xs) => xs.foldLeft(x) {
      case (left, (operator, right)) if operator.equalsIgnoreCase("or") => Or(left, right)
      case (left, (operator, right)) if operator.equalsIgnoreCase("and") => And(left, right)
    }
  }
}
