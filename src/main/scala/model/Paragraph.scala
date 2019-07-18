package model

import scala.xml.Utility

case class Paragraph(var block: List[TextLine]) extends Block {

  def writeParagraph(block: Block, stringBuilder: StringBuilder): StringBuilder = {
    def writeThem(block: List[TextLine], sb: StringBuilder, acc: StringBuilder): StringBuilder = {
      block match {
        case Nil =>
          sb.append(acc.toString().trim)
        case x :: xs =>
          if (!x.toString.trim.isEmpty)
            if (x.toString.endsWith(" ")) writeThem(xs, sb, acc.append(Utility.escape(x.toString)))
            else writeThem(xs, sb, acc.append(Utility.escape(x.toString) + " "))
          else writeThem(xs, sb, acc)
      }
    }

    if (!block.block.filter(!_.toString.trim.isEmpty).isEmpty
      && !("[A-Za-z]+".r.findFirstIn(block.block.head.toString) == None))
      writeThem(block.block, stringBuilder, new StringBuilder)
    else stringBuilder
  }

  def getString: Option[String] = {
    def writeThem(block: List[TextLine], acc: StringBuilder): String = {
      block match {
        case Nil =>
          acc.toString().trim
        case x :: xs =>
          if (!x.toString.trim.isEmpty)
            if (x.toString.endsWith(" ")) writeThem(xs, acc.append(Utility.escape(x.toString)))
            else writeThem(xs, acc.append(Utility.escape(x.toString) + " "))
          else writeThem(xs, acc)
      }
    }

    if (!block.filter(!_.toString.trim.isEmpty).isEmpty
      && !("[A-Za-z]+".r.findFirstIn(block.head.toString) == None))
      Some(writeThem(block, new StringBuilder))
    else None
  }
}
