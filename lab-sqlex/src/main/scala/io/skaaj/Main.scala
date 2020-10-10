package io.skaaj

import fastparse._, NoWhitespace._

object Main {
  def main(args: Array[String]): Unit = {

    sealed trait ResultColumn {
      val alias: Option[String]
    }

    case class ColumnFromRef(sourceObject: Option[String], sourceColumn: String, alias: Option[String] = None)
      extends ResultColumn

    case class ColumnFromExpr(expr: String, alias: Option[String] = None)
      extends ResultColumn

    case class Select(distinct: Boolean,
                      columns: Seq[ResultColumn]) extends Expression {
      override def toString: String = s"Select(distinct: $distinct, columns: [${columns.mkString(", ")}])"
    }

    def ` `[_: P]: P[Unit] = P(CharIn(" \t\n").rep(1))
    def _select[_: P]: P[Unit] = P(IgnoreCase("select"))
    def _duplicateMode[_: P]: P[String] = P(StringInIgnoreCase("all","distinct")).!.map(_.toLowerCase)
    def _as[_: P]: P[Unit] = P(IgnoreCase("as"))


    def rcSourceObject[_: P]: P[String] = P(CharIn("a-z").rep(1).! ~ ".")
    def rcSourceColumn[_: P]: P[String] = P(CharIn("a-z").rep(1) | "*").!
    def rcAlias[_: P]: P[String] = P((_as ~ ` `).? ~ CharIn("a-zA-Z").rep(1).!)
    def rcSeparator[_: P]: P[Unit] = P(` `.? ~ "," ~ ` `.?)
    def rc[_: P]: P[ResultColumn] = P(rcSourceObject.? ~ rcSourceColumn ~ (` ` ~ rcAlias).?).map {
      case (sourceObject, sourceColumn, alias) => ColumnFromRef(sourceObject, sourceColumn, alias)
    }
    def resultColumns[_: P]: P[(Seq[ResultColumn], ResultColumn)] = P((rc ~ rcSeparator).rep ~ rc)
    def duplicateMode[_: P]: P[String] = P(_duplicateMode ~ ` `)
    def select[_: P]: P[Expression] = P(` ` ~ _select ~ ` ` ~ duplicateMode.? ~ resultColumns ~ End).map {
      case (duplicateOption, (initCols, lastCol)) =>
        Select(duplicateOption.contains("distinct"), (initCols :+ lastCol))
    }

    val input =
      s"""
         |Select foo as foul,
         |bar,
         |tabletwo.baz as bazile""".stripMargin
    val result = parse(input, select(_))
    result match {
      case Parsed.Success(result, index) =>
        println(s"""Input: "$input"""")
        println(s"Result: $result")
      case failure @ Parsed.Failure(str, i, extra) =>
        println(failure)
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