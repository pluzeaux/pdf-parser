package parser

import java.io.{File, PrintWriter}

import org.apache.pdfbox.text.TextPosition

import scala.math._
import org.json4s.Xml.{toJson}
import org.json4s.native.JsonMethods._
import scala.xml.XML

class FunctionalXMLPageWriter() {

  //  def process(document: List[List[TextPosition]]): Unit = {
  //    val pw = new PrintWriter(new File("BNP_Extracted_Text.txt"))
  //    for (page <- document) {
  //      writePage(page, pw)
  //    }
  //    pw.close()
  //  }
  //
  //  def writePage(page: List[TextPosition], pw: PrintWriter): Unit = {
  //    val p = groupChars(page) _
  //    val lines = p(overlap)
  //    //    val textBlocks = groupLines(lines)(isNotBlocSeparation)
  //    writeBlocks(filterBlocks(groupLines(groupChars(page)(overlap))(isInParagraph))(mayBeTabular), new StringBuilder)
  //    val textBlocks = filterBlocks(groupLines(lines)(isInParagraph))(mayBeTabular)
  //
  //    val sb = new StringBuilder
  //
  //    for (l <- lines)
  //      sb.append(l.toString)
  //
  //    pw.write(sb.toString())
  //    println("Extracted text:\n" + sb.toString)
  //  }

  def process(document: List[List[TextPosition]], labels: Array[String]): String = {
    pretty(render(toJson(XML.loadString("""(\()?([0-9]{1,3})( ([0-9]{3}))*(\,[0-9]+)?(\))?""".r
      .replaceAllIn(writePage(document, labels).toString(), """$2$4$5""")))))

//    def write(document: List[List[TextPosition]], pw: PrintWriter): Unit = {
//
//      pw.write(
//        pretty(render(toJson(XML.loadString("""(\()?([0-9]{1,3})( ([0-9]{3}))*(\,[0-9]+)?(\))?""".r
//          .replaceAllIn(writePage(document).toString(), """$2$4$5""")))))
//      )
//    }

//    val pw = new PrintWriter(new File("LUZEAUX_PHILIPPE_03_2019.xml"))
//    val pw = new PrintWriter(new File("Test_SG_Extracted_Text_33.json"))
////    val pw = new PrintWriter(new File("Test_BNP_Extracted_Text_32.json"))
//    write(document, pw)
//    pw.close()
  }

  def writePage(pages: List[List[TextPosition]], labels: Array[String]): StringBuilder = {
    def writeThem(pages: List[List[TextPosition]], sb: StringBuilder, acc: Int = 0): StringBuilder = {
      pages match {
        case Nil =>
          sb // .append("</page>")
        case x :: xs =>
          writeThem(xs,
            writeBlocks(filterBlocks(groupLines(groupChars(x)(overlap))(isInParagraph), List())(mayBeTabular),
              sb.append(s"""<page number="${labels(acc)}">""")), acc + 1)
      }
    }

    val sb = new StringBuilder
    writeThem(pages, sb.append("<document>")).append("</document>")
  }

  def writeBlocks(blocks: List[Block], sb: StringBuilder): StringBuilder = {
    blocks match {
      case Nil =>
        sb.append("</page>")
      case x :: xs =>
        x match {
          case _: Tabular =>
            writeBlocks(xs, x.asInstanceOf[Tabular].writeTable(x, sb)(isCell))
          case _: Paragraph =>
            writeBlocks(xs, x.asInstanceOf[Paragraph].writeParagraph(x, sb))
        }
    }
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
}