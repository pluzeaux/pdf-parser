package parser

import org.apache.pdfbox.text.TextPosition
import xml.Utility

case class Tabular(var block: List[TextLine]) extends Block {

  def maxNbCols: Int = {
    block.reduceLeft(maxCols).nbTab
  }

  def minNbCols: Int = {
    block.reduceLeft(minCols).nbTab
  }

  def maxCols(l1: TextLine, l2: TextLine): TextLine = {
    if (l1.nbTab <= l2.nbTab) l2 else l1
  }

  def minCols(l1: TextLine, l2: TextLine): TextLine = {
    if (l1.nbTab <= l2.nbTab) l1 else l2
  }

  def isCoherent(block: Block): Boolean = {
    maxNbCols == minNbCols
  }

  def formatTable(block: Block): String = {
    ""
  }

  def writeTable(block: Block, stringBuilder: StringBuilder)(f: (TextPosition, TextPosition, TextLine) => Boolean): StringBuilder = {
    def writeThem(block: List[TextLine], sb: StringBuilder, acc: StringBuilder): StringBuilder = {
      block match {
        case Nil =>
          sb.append("<table>" + acc.toString() + "</table>")
        case x :: xs  => writeThem(xs, sb, acc.append(writeRow(x)(f).toString))
      }
    }

    if (isNotEmpty(block)) writeThem(block.block, stringBuilder, new StringBuilder)
    else stringBuilder
  }

  def writeRow(line: TextLine)(f: (TextPosition, TextPosition, TextLine) => Boolean): StringBuilder = {
    def writeThem(line: List[TextPosition], textLine: TextLine , sb: StringBuilder, acc: StringBuilder): StringBuilder = {
      line match {
        case Nil =>
          new StringBuilder("<row>" +  sb.toString + "</row>")
        case x :: Nil => writeThem(Nil, textLine, sb.append(writeCell(acc.append(x.toString)).toString), new StringBuilder)
        case x :: xs =>
          if (f(x, xs.head, textLine)) writeThem(xs, textLine, sb.append(writeCell(acc.append(x.toString)).toString), new StringBuilder)
          else writeThem(xs, textLine, sb, acc.append(x.toString))

      }
    }

    if (!line.toString.trim.isEmpty) writeThem(line.line, line, new StringBuilder, new StringBuilder)
    else new StringBuilder
  }

  def writeCell(stringBuilder: StringBuilder): StringBuilder = {
   new StringBuilder("<cell>" + Utility.escape(stringBuilder.toString().trim) + "</cell>")
  }

  val isNotEmpty = (block: Block)  => {
    !block.block.filter(!_.toString.trim.isEmpty).isEmpty
  }
}

