package io.skaarj

import fastparse._, NoWhitespace._

object Main {
  sealed trait Expression[A] {
    def eval: A
  }

  case class Constant[A](value: A) extends Expression[A] {
    override def eval: A = value
  }

  abstract class UnaryOperator[A, B](operation: A => B) extends Expression[B] {
    val rhs: Expression[A]

    override def eval: B = operation(rhs.eval)
  }

  abstract class BinaryOperator[A, B, C](operation: (A, B) => C) extends Expression[C] {
    val lhs: Expression[A]
    val rhs: Expression[B]

    override def eval: C = operation(lhs.eval, rhs.eval)
  }

  case class Or(lhs: Expression[Boolean],
                rhs: Expression[Boolean])
    extends BinaryOperator[Boolean, Boolean, Boolean](_ || _)

  case class And(lhs: Expression[Boolean],
                 rhs: Expression[Boolean])
    extends BinaryOperator[Boolean, Boolean, Boolean](_ && _)

  def ws[_: P]: P[Unit] = P(CharIn(" \t"))
  def identifier[_: P]: P[Expression[Boolean]] = P(CharIn("a-z").rep).!.log.map(x => Constant(x > "a"))
  def andExpr[_: P]: P[Expression[Boolean]] = P(identifier ~ (ws ~ "and" ~ ws ~ identifier).rep).log.map {
    case (x, xs) => xs.foldLeft(x) {
      case (left, right) => And(left, right)
    }
  }

  def orExpr[_: P]: P[Expression[Boolean]] = P(andExpr ~ (ws ~ "or" ~ ws ~ andExpr).rep).log.map {
    case (x, xs) => xs.foldLeft(x) {
      case (left, right) => Or(left, right)
    }
  }

  def expr[_: P]: P[Expression[Boolean]] = P(orExpr ~ End)

  def main(args: Array[String]): Unit = {
    parse("a or b", expr(_)) match {
      case result@ Parsed.Success(value, index) =>
        println(result)
        println(value.eval)
      case result @ Parsed.Failure(str, i, extra) =>
        println(result)
    }
  }
}