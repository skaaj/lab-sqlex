package io.skaaj.model

trait Expression

case class Constant[A](value: A) extends Expression
case class Identifier(name: String) extends Expression
