package io.skaaj.model

trait Expression
trait UnaryExpression extends Expression
trait BinaryExpression extends Expression

case class Constant[A](value: A) extends Expression

case class Identifier(name: String) extends Expression
