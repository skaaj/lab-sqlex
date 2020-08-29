package io.skaarj

import fastparse._, NoWhitespace._

object Main extends App {
  def number[_: P] = P(CharIn("0-9").rep(1).!.map(_.toFloat))
  def operator[_: P] = P(CharIn("*/+\\-")).!
  def expr[_: P] = P(number ~ operator ~ number).map {
    case (lhs, "+", rhs) => lhs + rhs
    case (lhs, "-", rhs) => lhs - rhs
    case (lhs, "/", rhs) => lhs / rhs
    case (lhs, "*", rhs) => lhs * rhs
  }

  println(parse("84/2", expr(_)))
}