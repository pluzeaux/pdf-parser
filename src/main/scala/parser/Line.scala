package parser

class Line (val line: Seq[LineItem],
            val current: PositionWrapper,
            val lastPosition: PositionWrapper,
            val lastLineStartPosition: PositionWrapper,
            val maxHeightForLine: Float,
            val expectedStartOfNextWordX: Float
           ) {

//  val fontSize = line.reduceLeft(maxFontSize)
  def maxFontSize(s1: LineItem, s2: LineItem): LineItem = if (s1.textPosition.getFontSizeInPt > s2.textPosition.getFontSizeInPt) s1 else s2

  def getMaxFonSize(): Float = {
    return line.reduceLeft(maxFontSize).textPosition.getFontSizeInPt
  }
}
