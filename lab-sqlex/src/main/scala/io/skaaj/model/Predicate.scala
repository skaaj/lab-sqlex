package io.skaaj.model

case class Or(left: Expression, right: Expression) extends BinaryExpression
case class And(left: Expression, right: Expression) extends BinaryExpression
case class Not(operand: Expression) extends UnaryExpression
