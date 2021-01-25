package io.skaaj

import fastparse._
import NoWhitespace._
import io.skaaj.model._
import io.skaaj.parser.SelectParser

object Main {
  
  def main(args: Array[String]): Unit = {

    sealed trait FromReference

    trait Aliasable {
      val alias: Option[String]
    }

    case class TableLike(
      namespace: Option[String],
      name: String,
      alias: Option[String]
    ) extends FromReference with Aliasable

    case class From(reference: FromReference)

    def ws[_: P]: P[Unit] = P(CharIn(" \t\n").rep(1))

    def _from[_: P]: P[Unit] = P(IgnoreCase("from"))
    def _as[_: P]: P[Unit] = P(IgnoreCase("as"))

    def word[_: P]: P[Unit] = P(CharIn("a-zA-Z").rep(1))
    def alias[_: P]: P[String] = P(ws ~ (_as ~ ws).? ~ word.!)
    def fromSource[_: P]: P[TableLike] = P((word.! ~ ".").? ~ word.! ~ alias.?).map {
      case (namespace, name, alias) => TableLike(namespace, name, alias)
    }

    def fromParser[_: P]: P[From] = P(ws.? ~ _from ~ ws ~ fromSource).map(From)


    val input =
      s"""from namespace.table as alias""".stripMargin

    time {
      val result = parse(input, fromParser(_))
      result match {
        case Parsed.Success(result, index) =>
          println(s"""Input: "$input"""")
          println(s"Result: $result")
        case failure@Parsed.Failure(str, i, extra) =>
          println(failure)
      }
    }

  }

  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }
}