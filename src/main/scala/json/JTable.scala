package json

import play.api.libs.json.{Json, Writes}

case class JTable(rows: Seq[JRow]) {

  implicit val jCellWrites = new Writes[JCell] {
    def writes(cell: JCell) = Json.obj(
      "cell" -> cell.cell
    )
  }

  implicit val jRowWrites = new Writes[JRow] {
    def writes(row: JRow) = Json.obj(
      "cells" -> row.cells
    )
  }

  implicit val jTablesWrites = new Writes[JTable] {
    def writes(table: JTable) = Json.obj(
      "rows" -> table.rows
    )
  }
}
