package io.skaaj.parser

import fastparse.NoWhitespace._
import fastparse._
import io.skaaj.model.{ColumnFromRef, Expression, ResultColumn, Select}

object SelectParser {
  def apply[_: P]: P[Expression] = P(ws.? ~ _select ~ ws ~ duplicateMode ~ resultColumns).map {
    case (isDistinct, (initCols, lastCol)) => Select(isDistinct, initCols :+ lastCol)
  }

  private def ws[_: P]: P[Unit] = P(CharIn(" \t\n").rep(1))

  private def _select[_: P]: P[Unit] = P(IgnoreCase("select"))
  private def _duplicateMode[_: P]: P[String] = P(StringInIgnoreCase("all","distinct")).!
  private def _as[_: P]: P[Unit] = P(IgnoreCase("as"))

  private def rcSourceObject[_: P]: P[String] =
    P(CharIn("a-z").rep(1).! ~ ".")

  private def rcSourceColumn[_: P]: P[String] =
    P(CharIn("a-z").rep(1) | "*").!

  private def rcAlias[_: P]: P[String] =
    P((_as ~ ws).? ~ CharIn("a-zA-Z").rep(1).!)

  private def rcSeparator[_: P]: P[Unit] =
    P(ws.? ~ "," ~ ws.?)

  private def rc[_: P]: P[ResultColumn] = P(rcSourceObject.? ~ rcSourceColumn ~ (ws ~ rcAlias).?).map {
    case (sourceObject, sourceColumn, alias) => ColumnFromRef(sourceObject, sourceColumn, alias)
  }

  private def resultColumns[_: P]: P[(Seq[ResultColumn], ResultColumn)] =
    P((rc ~ rcSeparator).rep ~ rc)

  private def duplicateMode[_: P]: P[Boolean] =
    P(_duplicateMode ~ ws).?.map(_.exists(_.toLowerCase == "distinct"))
}
