package io.skaaj.model

case class Or(left: Expression, right: Expression) extends Expression
case class And(left: Expression, right: Expression) extends Expression
case class Not(operand: Expression) extends Expression
