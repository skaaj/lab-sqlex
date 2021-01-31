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

    def ws[_: P]: P[Unit] = P(CharIn(" \t\n").rep(1))

    def select[_: P]: P[Select] = SelectParser.apply
    def predicate[_: P]: P[Expression] = PredicateParser.apply
    def from[_: P]: P[(Seq[String], String)] = P(_from ~ ws ~ (table ~ ", ").rep ~ table)
    def where[_: P]: P[Unit] = P(_where)

    def schema[_: P]: P[String] = P(CharIn("a-z").rep ~ ".").!
    def alias[_: P]: P[String] = P(ws ~ (IgnoreCase("as") ~ ws).? ~ CharIn("a-z").!)
    def table[_: P]: P[String] = P(schema.? ~ CharIn("a-z").rep(1).! ~ alias.?).map {
      case (schema, table, alias) => schema.getOrElse("").concat(table).concat(alias.getOrElse(""))
    }

    def queryParser[_: P]: P[Query] =
      P(select ~ ws ~ from).map { case (select, (initTables, lastTable)) =>
        Query(select, From(initTables :+ lastTable))
      }

    val input =
      s"""select a foo, tablea.b as bar from tablea, tableb""".stripMargin
    
    println(input)
    println((1 to 9).mkString + "0" + (1 to 9).mkString)

    time {
      val result = parse(input, queryParser(_))
      result match {
        case Parsed.Success(result, index) =>
          println(s"""Input: "$input"""")
          println(s"Result: $result")
        case failure@Parsed.Failure(str, i, extra) =>
          println(failure)
        case f@_ =>
          println(f)
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