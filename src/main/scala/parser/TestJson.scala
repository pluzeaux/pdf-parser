package parser

class TestJson {
    case class JDocument(name: String, year: Int, pages: Seq[JPage])
    case class JPage(number: Int, paragraphs: Seq[JParagraph], tables: Seq[JTable])
    case class JParagraph(paragraph: String)
    case class JTable(table: Seq[JRow])
    case class JRow(row: Seq[JCell])
    case class JCell(cell: String)

  import play.api.libs.json._

  implicit val jParagraphWrites = new Writes[JParagraph] {
    def writes(paragraph: JParagraph) = Json.obj(
      "paragraph" -> paragraph.paragraph
    )
  }

  implicit val jTablesWrites = new Writes[JTable] {
    def writes(page: JTable) = Json.obj(

    )
  }

  implicit val jPageWrites = new Writes[JPage] {
    def writes(page: JPage) = Json.obj(
      "number" -> page.number,
      "paragraphs" -> page.paragraphs,
      "tables" -> page.tables
    )
  }

    implicit val jDocumentWrites = new Writes[JDocument] {
      def writes(document: JDocument) = Json.obj(
        "name" -> document.name,
        "year" -> document.year,
        "pages" -> document.pages
      )
    }




}
