package model

import json.{JCell, JRow, JTable}
import org.apache.pdfbox.text.TextPosition

import scala.xml.Utility

case class Tabular(var block: List[TextLine]) extends Block {

  val isNotEmpty = (block: Block) => {
    !block.block.filter(!_.toString.trim.isEmpty).isEmpty
  }

  def isCoherent(block: Block): Boolean = {
    maxNbCols == minNbCols
  }

  def maxNbCols: Int = {
    block.reduceLeft(maxCols).nbTab
  }

  def maxCols(l1: TextLine, l2: TextLine): TextLine = {
    if (l1.nbTab <= l2.nbTab) l2 else l1
  }

  def minNbCols: Int = {
    block.reduceLeft(minCols).nbTab
  }

  def minCols(l1: TextLine, l2: TextLine): TextLine = {
    if (l1.nbTab <= l2.nbTab) l1 else l2
  }

  def formatTable(block: Block): String = {
    ""
  }

  def getTable(block: Block)(f: (TextPosition, TextPosition, TextLine) => Boolean): Option[JTable] = {
    def writeThem(block: List[TextLine], acc: List[JRow]): JTable = {
      block match {
        case Nil =>
          JTable(acc.reverse)
        case x :: xs => writeThem(xs, getRow(x)(f) :: acc)
      }
    }

    if (isNotEmpty(block)) Some(writeThem(block.block, Nil))
    else None
  }

  def getRow(line: TextLine)(f: (TextPosition, TextPosition, TextLine) => Boolean): JRow = {
    def writeThem(line: List[TextPosition], textLine: TextLine, acc: List[JCell], sb: StringBuilder): JRow = {
      line match {
        case Nil =>
          new JRow(acc.reverse)
        case x :: Nil => writeThem(Nil, textLine, JCell(getCell(sb.append(x))) :: acc, new StringBuilder)
        case x :: xs =>
          if (f(x, xs.head, textLine)) writeThem(xs, textLine, JCell(getCell(sb.append(x))) :: acc, new StringBuilder)
          else writeThem(xs, textLine, acc, sb.append(x))

      }
    }

    if (!line.toString.trim.isEmpty) writeThem(line.line, line, Nil, new StringBuilder)
    else JRow(List[JCell]())
  }

  def getCell(stringBuilder: StringBuilder): String = {
    Utility.escape(stringBuilder.toString().trim)
  }
}
