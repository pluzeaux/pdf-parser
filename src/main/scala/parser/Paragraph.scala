package parser

import xml.Utility

case class Paragraph(var block: List[TextLine]) extends Block {

//  def writeParagraph(block: Block, stringBuilder: StringBuilder): StringBuilder = {
//    def writeThem(block: List[TextLine], sb: StringBuilder): StringBuilder = {
//      block match {
//        case Nil =>
//          sb.append("</paragraph>")
//        case x :: xs =>
//          if(!x.toString.trim.isEmpty) writeThem(xs, sb.append(Utility.escape(x.toString).replace('\n', ' ')))
//          else writeThem(xs, sb)
//      }
//    }
//
////    if (!(block.block.length <= 1
////      && (block.block.head.toString.trim.isEmpty
////      || "[A-Za-z]+".r.findFirstIn(block.block.head.toString) == None))
////    )
//    if (!block.block.filter(!_.toString.trim.isEmpty).isEmpty
//      && !("[A-Za-z]+".r.findFirstIn(block.block.head.toString) == None))
//      writeThem(block.block, stringBuilder.append("<paragraph>"))
//    else stringBuilder
//  }

  def writeParagraph(block: Block, stringBuilder: StringBuilder): StringBuilder = {
    def writeThem(block: List[TextLine], sb: StringBuilder, acc: StringBuilder): StringBuilder = {
      block match {
        case Nil =>
 //         sb.append("<paragraph>" + acc.toString().trim + "</paragraph>")
          sb.append(acc.toString().trim)
        case x :: xs =>
          if(!x.toString.trim.isEmpty) writeThem(xs, sb, acc.append(Utility.escape(x.toString)))
          else writeThem(xs, sb, acc)
      }
    }

    //    if (!(block.block.length <= 1
    //      && (block.block.head.toString.trim.isEmpty
    //      || "[A-Za-z]+".r.findFirstIn(block.block.head.toString) == None))
    //    )
    if (!block.block.filter(!_.toString.trim.isEmpty).isEmpty
      && !("[A-Za-z]+".r.findFirstIn(block.block.head.toString) == None))
      writeThem(block.block, stringBuilder, new StringBuilder)
    else stringBuilder
  }

  def getString: Option[String] = {
    def writeThem(block: List[TextLine], acc: StringBuilder): String = {
      block match {
        case Nil =>
          //         sb.append("<paragraph>" + acc.toString().trim + "</paragraph>")
          acc.toString().trim
        case x :: xs =>
          if(!x.toString.trim.isEmpty) writeThem(xs, acc.append(Utility.escape(x.toString)))
          else writeThem(xs, acc)
      }
    }

    //    if (!(block.block.length <= 1
    //      && (block.block.head.toString.trim.isEmpty
    //      || "[A-Za-z]+".r.findFirstIn(block.block.head.toString) == None))
    //    )
    if (!block.filter(!_.toString.trim.isEmpty).isEmpty
      && !("[A-Za-z]+".r.findFirstIn(block.head.toString) == None))
      Some(writeThem(block, new StringBuilder))
    else None
  }
}
