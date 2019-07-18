package model

import org.apache.pdfbox.text.TextPosition

/**
  * internal marker class. Used as a place holder in a line of TextPositions.
  */


class LineItem(val textPosition: TextPosition)

object LineItem {
  val WORD_SEPARATOR = new LineItem(null)

  def getWordSeparator: LineItem = WORD_SEPARATOR

  def isWordSeparator(lineItem: LineItem): Boolean = lineItem.textPosition == null
}
