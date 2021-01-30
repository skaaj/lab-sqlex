package io.skaaj.model

sealed trait ResultColumn {
  val alias: Option[String]
}

case class ColumnFromRef(sourceObject: Option[String],
                         sourceColumn: String,
                         alias: Option[String] = None) extends ResultColumn

case class ColumnFromExpr(expr: String,
                          alias: Option[String] = None) extends ResultColumn

case class Select(distinct: Boolean,
                  columns: Seq[ResultColumn]) extends Expression {
 // override def toString: String = s"Select(distinct: $distinct, columns: [${columns.mkString(", ")}])"
}
