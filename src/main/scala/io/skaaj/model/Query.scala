package io.skaaj.model

case class Query(select: Select, from: From, where: Option[Where] = None)

case class Select(distinct: Boolean, columns: Seq[ResultColumn])
    extends Expression
case class From(relations: Seq[String])
case class Where(condition: Expression)

sealed trait ResultColumn {
  val alias: Option[String]
}

case class ColRef(
    sourceObject: Option[String],
    sourceColumn: String,
    alias: Option[String] = None
) extends ResultColumn

case class ColExpr(expr: String, alias: Option[String] = None)
    extends ResultColumn
