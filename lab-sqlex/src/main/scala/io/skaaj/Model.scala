package io.skaaj

trait Expression
trait UnaryExpression extends Expression
trait BinaryExpression extends Expression

case class Constant[A](value: A) extends Expression

case class Identifier(name: String) extends Expression

case class Or(left: Expression, right: Expression) extends BinaryExpression
case class And(left: Expression, right: Expression) extends BinaryExpression
case class Not(operand: Expression) extends UnaryExpression

