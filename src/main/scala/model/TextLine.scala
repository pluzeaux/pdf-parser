package model

import org.apache.pdfbox.text.TextPosition

import scala.math._

class TextLine {
  private val spacingTolerance = .5f
  private val averageCharTolerance = .3f
  private val _dropThreshold = 2.5f
  private var _line: List[TextPosition] = List()
  private var _maxHeight: Float = -1
  private var _maxY: Float = -Float.MaxValue
  private var _isTabular: Boolean = false
  private var _nbTab: Int = 0

  def line: List[TextPosition] = _line

  def maxHeight: Float = _maxHeight

  def maxY: Float = _maxY

  def dropThreshold: Float = _dropThreshold

  def isTabular: Boolean = _isTabular

  def nbTab: Int = _nbTab

  def reverse(): TextLine = {
    _line = _line.reverse.sortWith((l1: TextPosition, l2: TextPosition) => l1.getX < l2.getX)
    val (x, y) = setTabular(_line, false, 0)
    _isTabular = x
    _nbTab = y
    this
  }

  def ::(textPosition: TextPosition): TextLine = {
    _maxHeight = max(_maxHeight, textPosition.getHeight)
    _maxY = max(_maxY, textPosition.getY)
    _line = textPosition :: _line
    this
  }

  def ::(line: TextLine): TextLine = {
    _line = line._line ::: _line
    this
  }

  def deltaSpace: Float =
    _line.map(_.getWidthOfSpace).sum / _line.length * spacingTolerance

  def deltaCharWidth: Float =
    _line.map(p => p.getWidth / p.getIndividualWidths.length).sum / _line.length * averageCharTolerance

  def setTabular(line: List[TextPosition], isTab: Boolean, acc: Int): (Boolean, Int) =
    line match {
      case Nil =>
        (isTab, acc)
      case _ :: Nil =>
        if (isTab)
          (isTab, acc + 1)
        else
          (isTab, acc)
      case x :: xs =>
        if (xs.head.getX - x.getX + x.getWidth > Math.max(deltaSpace, deltaCharWidth) * 20f)
          setTabular(xs, true, acc + 1)
        else
          setTabular(xs, isTab, acc)
    }

  def formatLine: Unit = None

  override def toString: String = {
    val sb = new StringBuilder
    for (t <- _line) {
      sb.append(t.getUnicode)
    }

    sb.toString
  }
}
