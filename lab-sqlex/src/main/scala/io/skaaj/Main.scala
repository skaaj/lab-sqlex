package io.skaaj

import fastparse._, NoWhitespace._

object Main {
  def main(args: Array[String]): Unit = {
    case class Select(items: Seq[SelectItem], distinct: Boolean) extends Expression {
      override def toString: String = s"Select(distinct: $distinct, cols: [${items.mkString(", ")}])"
    }
    case class SelectItem(value: String) extends Expression {
      override def toString: String = value
    }

    def ws[_: P]: P[Unit] = P(CharIn(" \t").rep(1))
    def selectItemPrefix[_: P]: P[String] = P(CharIn("a-z").rep(1).! ~ ".")
    def selectItemSuffix[_: P]: P[String] = P((IgnoreCase("as") ~ ws).? ~ CharIn("a-z").rep(1).!)
    def selectItem[_: P]: P[String] = P(selectItemPrefix.? ~ (CharIn("a-z").rep(1) | "*").! ~ ( ws ~ selectItemSuffix).?).map {
      case (objectName, colName, colAlias) =>
        objectName.map(x => s"$x.").getOrElse("") + colName + colAlias.map(x => s" AS $x").getOrElse("")
    }
    def selectToken[_: P]: P[Unit] = P(IgnoreCase("select"))
    def duplicateToken[_: P]: P[String] = P(StringInIgnoreCase("all","distinct")).!.map(_.toLowerCase)
    def select[_: P]: P[Expression] = P(selectToken ~ (ws ~ duplicateToken).? ~ ws ~ (selectItem ~ ws.rep ~ "," ~ ws.rep).rep ~ selectItem ~ End).map {
      case (duplicateOption, initCols, lastCol) =>
        Select((initCols :+ lastCol).map(SelectItem), duplicateOption.contains("distinct"))
    }

    val input = "SELECT foo ass, bar"
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