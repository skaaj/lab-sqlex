package io.skaaj

import fastparse.NoWhitespace._
import fastparse._
import io.skaaj.model._
import io.skaaj.parser.SelectParser
import io.skaaj.parser.PredicateParser

object Main {
  
  def main(args: Array[String]): Unit = {
    def _from[_: P]: P[Unit] = P(IgnoreCase("from"))
    def _where[_: P]: P[Unit] = P(IgnoreCase("where"))

    def ws[_: P]: P[Unit] = P(CharIn(" \t\n").rep(1)).log

    def select[_: P]: P[Expression] = SelectParser.apply
    def from[_: P]: P[Unit] = P(_from)
    def where[_: P]: P[Unit] = P(_where)

    def queryParser[_: P]: P[Expression] =
      P(select ~ ws ~ from ~ ws ~ where)

    val input =
      s"""select foo as bar from where""".stripMargin

    time {
      val result = parse(input, queryParser(_))
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