package io.skaarj

import fastparse._, NoWhitespace._

object Main {
  case class LogicExpr(lhs: String, operator: String, rhs: String)

  def logicOperator[_: P]: P[Unit] = P(" or " | " and ")
  def logicOperand[_: P]: P[String] = P(CharIn("a-z").rep).!
  def logicExpr[_: P]: P[LogicExpr] = P(logicOperand ~ logicOperator.! ~ logicOperand).map {
    case (lhs, op, rhs) => LogicExpr(lhs, op, rhs)
  }

  def main(args: Array[String]): Unit =
    println(parse("foo and bar or baz and har", logicExpr(_)))
}