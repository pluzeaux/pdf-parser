package parser

import org.apache.pdfbox.text.TextPosition
import play.api.libs.json._
import org.json4s.Xml.toJson
import org.json4s.native.JsonMethods._

import scala.math._
import scala.xml.XML

class FunctionalXMLPageWriter2() {

  implicit val config = JsonConfiguration(optionHandlers = OptionHandlers.WritesNull)

  case class JDocument(name: String, year: Int, pages: Seq[JPage])
  case class JPage(number: String, paragraphs: Seq[JParagraph], tables: Seq[JTable])
  case class JParagraph(paragraph: String)
  case class JTable(table: Seq[JRow])
  case class JRow(row: Seq[JCell])
  case class JCell(cell: String)


  implicit val jParagraphWrites = new Writes[JParagraph] {
    def writes(paragraph: JParagraph) = Json.obj(
      "paragraph" -> paragraph.paragraph
    )
  }

  implicit val jTablesWrites = new Writes[JTable] {
    def writes(table: JTable) = Json.obj(

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


  def process(pages: List[List[TextPosition]], labels: Array[String], company: String, year: String): String = {
//    pretty(render(toJson(XML.loadString("""(\()?([0-9]{1,3})( ([0-9]{3}))*(\,[0-9]+)?(\))?""".r
//      .replaceAllIn(writePages(document, labels).toString(), """$2$4$5""")))))

//    val doc = Json.toJson(JDocument(company, year.toInt, writePages(pages, labels))).toString
//    println(doc)
//    doc
    Json.toJson(JDocument(company, year.toInt, writePages(pages, labels))).toString

  }

  def writePages(pages: List[List[TextPosition]], labels: Array[String]): List[JPage] = {
    def writeThem(pages: List[List[TextPosition]], jPages: List[JPage], acc: Int = 0): List[JPage] = {
      pages match {
        case Nil =>
          jPages.reverse
        case x :: xs =>
          writeThem(xs,
            writePage(filterBlocks(groupLines(groupChars(x)(overlap))(isInParagraph), List())(mayBeTabular),
              labels(acc)) :: jPages, acc + 1)
      }
    }
    writeThem(pages, Nil)
  }

  def writePage(blocks: List[Block], label: String): JPage = {
    def writeThem(blocks: List[Block], label: String, accParagraph: List[JParagraph], accTable: List[JTable]): JPage = {
      blocks match {
        case Nil =>
          JPage(label, accParagraph.reverse, accTable.reverse)
        case x :: xs =>
          x match {
            case _: Tabular =>
              writeThem(xs, label,  accParagraph, accTable)
            case _: Paragraph =>
              x.asInstanceOf[Paragraph].getString match {
                case Some(p) =>
                  writeThem(xs, label, JParagraph(p) :: accParagraph, accTable)
                case None =>
                  writeThem(xs, label, accParagraph, accTable)
              }

          }
      }
    }

    writeThem(blocks, label, Nil, Nil)
  }

  def groupChars(list: List[TextPosition])(f: (TextPosition, TextLine) => Boolean): List[TextLine] = {
    def groupThem(lst: List[TextPosition], acc: TextLine): List[TextLine] = lst
    match {
      case Nil =>
        acc.reverse :: Nil
      case x :: xs =>
        acc.line match {
          case Nil => groupThem(xs, x :: acc)
          case _ :: _ if f(x, acc) =>
            groupThem(xs, x :: acc)
          case _ =>
            acc.reverse :: groupThem(xs, x :: new TextLine)
        }
    }

    groupThem(list, new TextLine)
  }

  def groupLines(list: List[TextLine])(f: (TextLine, TextLine) => Boolean): List[Block] = {
    def groupThem(lst: List[TextLine], acc: Option[Block]): List[Block] = lst
    match {
      case Nil =>
        acc match {
          case Some(a) => a.reverse :: Nil
          case None => Nil
        }

      case x :: xs =>
        acc match {
          case Some(a) =>

            a match {
              case _: Tabular =>
                if (x.isTabular) groupThem(xs, Some(x :: a))
                else a.reverse :: groupThem(xs, Some(Paragraph(x :: Nil)))
              case _: Paragraph if x.isTabular =>
                a.reverse :: groupThem(xs, Some(Tabular(x :: Nil)))
              case _: Paragraph =>
                if (f(a.block.head, x)) groupThem(xs, Some(x :: a))
                else a.reverse :: groupThem(xs, Some(Paragraph(x :: Nil)))
            }

          case None =>
            if (x.isTabular) groupThem(xs, Some(Tabular(x :: Nil)))
            else groupThem(xs, Some(Paragraph(x :: Nil)))
        }
    }

    groupThem(list, None)
  }

  //  def filterBlocks(blocks: List[Block])(f: (Block, Block, Block) => Boolean): List[Block] = {
  //    def groupThem(lst: List[Block], acc: List[Block]): List[Block] = lst
  //    match {
  //      case Nil =>
  //        acc.reverse ::: Nil
  //      case x :: y :: z :: xs if f(x, y, z) => groupThem(x.merge(joinLines(y, z)) :: xs, acc)
  //      case x :: y :: z :: xs => groupThem(y :: z :: xs, x :: acc)
  //      case x :: xs => groupThem(xs, x :: acc)
  //    }
  //
  //    groupThem(blocks, List())
  //  }

  def joinLines(block1: Block, block2: Block): Block = {
    block1.block.reduceRight(_ :: _) :: block2.block.head
    block2
  }

  val filterBlocks: (List[Block], List[Block]) => ((Block, Block, Block) => Boolean) => List[Block] =
    (lst: List[Block], acc: List[Block]) => (f: (Block, Block, Block) => Boolean) => {
      lst
      match {
        case Nil =>
          acc.reverse ::: Nil
//        case x :: y :: z :: xs if (f(x, y, z)) => filterBlocks(x.merge(joinLines(y, z)) :: xs, acc)(f)
        case x :: y :: z :: xs if (f(x, y, z)) => filterBlocks(x.merge(joinLines(y, z)) :: xs, acc)(f)
        case x :: y :: z :: xs => filterBlocks(y :: z :: xs, x :: acc)(f)
        case x :: xs => filterBlocks(xs, x :: acc)(f)
      }
    }

//  val filterBlocks: (List[Block]) => ((Block, Block, Block) => Boolean) => List[Block] =
//    (blocks: List[Block]) => (f: (Block, Block, Block) => Boolean) => {
//      groupThem(blocks, List())(f)
//    }

  val mayBeTabular: (Block, Block, Block) => Boolean =
    (x: Block, y: Block, z: Block) => {
      x match {
        case _: Tabular =>
          y match {
            case _: Paragraph =>
              z match {
                case _: Tabular =>
                  z.asInstanceOf[Tabular].minNbCols == x.asInstanceOf[Tabular].minNbCols - 1
                case _ => false
              }
            case _ => false
          }
        case _ => false
      }
    }

  val within: (Float, Float, Float) => Boolean =
    (first: Float, second: Float, variance: Float) => {
      second < first + variance && second > first - variance
    }

  val overlap: (TextPosition, TextLine) => Boolean =
    (p: TextPosition, l: TextLine) => {
      (within(p.getY, l.maxY, .1f)
        || l.maxY <= p.getY
        && l.maxY >= p.getY - p.getHeight
        || p.getY <= l.maxY
        && p.getY >= l.maxY - l.maxHeight
        )
    }

  val isParagraphSeparation: (TextLine, TextLine) => Boolean =
    (line1: TextLine, line2: TextLine) => {
      abs(line1.line.last.getYDirAdj - line2.line.last.getYDirAdj) > line2.maxHeight * line1.dropThreshold
    }

  val isInParagraph: (TextLine, TextLine) => Boolean =
    (line1: TextLine, line2: TextLine) => {
      !isParagraphSeparation(line1, line2)
    }

  val isInTable: (TextLine, TextLine) => Boolean =
    (line1: TextLine, line2: TextLine) => {
      line1.isTabular && line2.isTabular
    }

  val isNotBlocSeparation: (TextLine, TextLine) => Boolean =
    (line1: TextLine, line2: TextLine) => {
      isInParagraph(line1, line2) || (line1.isTabular && line2.isTabular)
    }

  val isCell: (TextPosition, TextPosition, TextLine) => Boolean =
    (p1: TextPosition, p2: TextPosition, line: TextLine) => {
      p2.getX - (p1.getX + p1.getWidth) > Math.max(line.deltaSpace, line.deltaCharWidth) * 20f
    }

//  case class JDocument(name: String, year: Int, pages: Seq[JPage])
//  case class JPage(number: Int, paragraphs: Seq[Paragraph], tables: Seq[JTable])
//  case class JParagraph(paragraph: String)
//  case class JTable(table: Seq[JRow])
//  case class JRow(row: Seq[JCell])
//  case class JCell(cell: String)

}