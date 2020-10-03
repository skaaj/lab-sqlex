package io.skaarj

import fastparse._, NoWhitespace._

object Main {
  sealed trait Item
  case class Node(lhs: Item, rhs: Item) extends Item
  case class Leaf(name: String) extends Item

//  def operator[_: P]: P[Unit] = P("or" | "and")
//  def identifier[_: P]: P[String] = P(CharIn("a-z").rep).!
//  def operand[_: P]: P[Item] = P(identifier | expr).map {
//    case id: String => Leaf(id)
//    case nested: Node => nested
//  }
//  def expr[_: P]: P[Node] = P(operand ~ " " ~ operator ~ " " ~ operand).rep.map {
//    case Seq(x, xs @ _*) => xs.foldLeft(Node(x._1, x._2)){
//      case (l, r) => Node(l, Node(r._1, r._2))
//    }
//  }

  case class Or(lhs: String, rhs: String)
  case class And(lhs: String, rhs: String)

  def operator[_: P]: P[Unit] = P("or" | "and")
  def identifier[_: P]: P[String] = P(CharIn("a-z").rep).!
  def operand[_: P]: P[String] = P(identifier | expr).!
  def expr[_: P]: P[String] = P(operand ~ (" " ~ operator.! ~ " " ~ operand).rep).map {
    case (x, xs) => xs.foldLeft(x) {
      case (left, ("or", right)) => Or(left, right).toString
      case (left, ("and", right)) => And(left, right).toString
    }
  }
  def main(args: Array[String]): Unit =
    println(parse("a and b or c and d", expr(_)))
}