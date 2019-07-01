package parser

import java.io.{IOException, StringWriter}
import java.text.Bidi

import org.apache.pdfbox.text.TextPosition

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.round
import scala.util.matching.Regex

class XMLPageWriter() {

  private val lineSeparator = System.getProperty("line.separator")
  private val wordSeparator = " "
  private val documentHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  private val paragraphStart = "<paragraph>\n"
  private val paragraphEnd = "</paragraph>\n\n"
  private val tableStart = "<table>\n"
  private val tableEnd = "</table>\n\n"
  private val pageStart = "<page>"
  private val pageEnd = "</page>"
  private val documentStart = "<document>\n"
  private val documentEnd = "</document>"
  private val titleStart = "<title>\n"
  private val titleEnd = "</title>\n"
  private val noteStart = "<note>\n"
  private val noteEnd = "</note>\n"

  private val END_OF_LAST_TEXT_X_RESET_VALUE = -1
  private val MAX_Y_FOR_LINE_RESET_VALUE = -Float.MaxValue
  private val EXPECTED_START_OF_NEXT_WORD_X_RESET_VALUE = -Float.MaxValue
  private val MAX_HEIGHT_FOR_LINE_RESET_VALUE = -1
  private val MIN_Y_TOP_FOR_LINE_RESET_VALUE = Float.MaxValue
  private val LAST_WORD_SPACING_RESET_VALUE = -1
  private val MAX_TEMP_BUFFER_SIZE = 5

  private val MIRRORING_CHAR_MAP = new mutable.HashMap[Character, Character]

  private val defaultIndentThreshold = 2.0f
  private val defaultDropThreshold = 2.5f

  private val indentThreshold = defaultIndentThreshold
  private val dropThreshold = defaultDropThreshold

  // we will need to estimate where to add spaces, these are used to help guess
  private val spacingTolerance = .5f
  private val averageCharTolerance = .3f

  private var bodyTextSize: Float = 0

  private var titleLevels: mutable.SortedMap[Float, Int] = null

  /**
    * a list of regular expressions that match commonly used list item formats, i.e. bullets, numbers, letters, Roman
    * numerals, etc. Not meant to be comprehensive.
    */
  private val LIST_ITEM_EXPRESSIONS = Array(
    "\\.",
    "\\d+\\.",
    "\\[\\d+\\]",
    "\\d+\\)",
    "[A-Z]\\.",
    "[a-z]\\.",
    "[A-Z]\\)",
    "[a-z]\\)",
    "[IVXL]+\\.",
    "[ivxl]+\\."
  )

  private var listOfPatterns: ArrayBuffer[Regex] = null

  private var inParagraph: Boolean = false
  private var inTable: Boolean = false
  private var inTitle: Boolean = false
  private var inNote: Boolean = false
  private var isTabular: Boolean = false

  def process(document: ArrayBuffer[ArrayBuffer[TextPosition]]): String = {
    val outputStream = new StringWriter
    initTitleLevels(document)

    bodyTextSize = titleLevels.maxBy(_._2)._1
    outputStream.write(documentHeader)

    for (page <- document) {
      outputStream.write(pageStart)
      writePageRefactor(page, outputStream)
      outputStream.write(pageEnd)
    }

    outputStream.write(documentEnd)
    outputStream.toString
  }

  private def initTitleLevels(document: ArrayBuffer[ArrayBuffer[TextPosition]]): Unit = {
    titleLevels = new mutable.TreeMap[Float, Int]
    for (page <- document) {
      for (position <- page) {
        val fontSize = position.getFontSizeInPt
        if (titleLevels.contains(fontSize)) {
          //          println(titleLevels.getOrElse(fontSize, 0))
          var value = titleLevels.getOrElse(fontSize, 0)
          value += 1
          titleLevels += (fontSize -> value)
        }
        else titleLevels += (fontSize -> 1)
      }
    }
  }

  /**
    * This will print the text of the processed page to "output". It will estimate, based on the coordinates of the
    * text, where newlines and word spacings should be placed. The text will be sorted only if that feature was
    * enabled.
    *
    * @throws IOException If there is an error writing the text.
    */
  //  @throws[IOException]
  protected def writePageRefactor(page: ArrayBuffer[TextPosition], output: StringWriter): Unit = {
    var maxYForLine: Float = MAX_Y_FOR_LINE_RESET_VALUE
    var minYTopForLine: Float = MIN_Y_TOP_FOR_LINE_RESET_VALUE
    var endOfLastTextX: Float = END_OF_LAST_TEXT_X_RESET_VALUE
    var lastWordSpacing: Float = LAST_WORD_SPACING_RESET_VALUE
    var maxHeightForLine: Float = MAX_HEIGHT_FOR_LINE_RESET_VALUE
    val maxTempBufferSize: Int = MAX_TEMP_BUFFER_SIZE

    var lastPosition: PositionWrapper = null
    var lastLineStartPosition: PositionWrapper = null
    var startOfPage: Boolean = true

    val inParagraph: Boolean = false
    var inTable: Boolean = false
    var isTitle = false
    var isTabular: Boolean = false

//    if (!page.isEmpty) writePageStart(output)

    val line = new ArrayBuffer[LineItem]

    var tempBuffer = new ArrayBuffer[Line]
    var buffer = new ArrayBuffer[Line]

    var previousAveCharWidth: Float = -1

    //    for (position <- page) {
    //      processPosition(position)
    //    }

    for (position <- page) {
      val current: PositionWrapper = new PositionWrapper(position)
      val characterValue: String = position.getUnicode

      if (lastPosition != null
        && ((position.getFont ne lastPosition.position.getFont)
        || position.getFontSize != lastPosition.position.getFontSize)
      ) previousAveCharWidth = -1

      val positionX: Float = position.getX
      val positionY: Float = position.getY
      val positionWidth: Float = position.getWidth
      val positionHeight: Float = position.getHeight

      val wordCharCount: Int = position.getIndividualWidths.length
      val wordSpacing: Float = position.getWidthOfSpace
      var deltaSpace: Float = 0

      if (wordSpacing == 0 || Float.NaN == wordSpacing) deltaSpace = Float.MaxValue
      else if (lastWordSpacing < 0) deltaSpace = wordSpacing * spacingTolerance
      else deltaSpace = (wordSpacing + lastWordSpacing) / 2f * spacingTolerance

      var averageCharWidth: Float = 0
      if (previousAveCharWidth < 0) averageCharWidth = positionWidth / wordCharCount
      else averageCharWidth = (previousAveCharWidth + positionWidth / wordCharCount) / 2f
      val deltaCharWidth: Float = averageCharWidth * averageCharTolerance
      var expectedStartOfNextWordX: Float = EXPECTED_START_OF_NEXT_WORD_X_RESET_VALUE

      if (endOfLastTextX != END_OF_LAST_TEXT_X_RESET_VALUE)
        expectedStartOfNextWordX = endOfLastTextX + Math.min(deltaSpace, deltaCharWidth)

      val currentLine: Line = new Line(
        cloneLine(line),
        current,
        lastPosition,
        lastLineStartPosition,
        maxHeightForLine,
        expectedStartOfNextWordX
      )

      if (lastPosition != null) {
        if (!overlap(positionY, positionHeight, maxYForLine, maxHeightForLine)) {

          processLine(currentLine, buffer, tempBuffer, output)
          line.clear()

          expectedStartOfNextWordX = EXPECTED_START_OF_NEXT_WORD_X_RESET_VALUE
          maxYForLine = MAX_Y_FOR_LINE_RESET_VALUE
          maxHeightForLine = MAX_HEIGHT_FOR_LINE_RESET_VALUE
          minYTopForLine = MIN_Y_TOP_FOR_LINE_RESET_VALUE
        }

        if (expectedStartOfNextWordX != EXPECTED_START_OF_NEXT_WORD_X_RESET_VALUE
          && expectedStartOfNextWordX < position.getX) {
          if (currentLine.lastPosition.position.getUnicode != null
            && !currentLine.lastPosition.position.getUnicode.endsWith(" ")) {
            line += LineItem.getWordSeparator
          }
          else if (currentLine.expectedStartOfNextWordX * 20f < position.getX) {
            line += new LineItem(new TextPosition(
              position.getRotation,
              position.getPageWidth,
              position.getPageHeight,
              position.getTextMatrix,
              position.getEndX,
              position.getEndY,
              position.getHeight,
              position.getIndividualWidths()(0),
              position.getWidthOfSpace,
              ";",
              position.getCharacterCodes,
              position.getFont,
              position.getFontSize,
              position.getFontSizeInPt.toInt)
            )

            isTabular = true
            inTable = true
          }
        }
      }

      if (positionY >= maxYForLine) maxYForLine = positionY
      endOfLastTextX = positionX + positionWidth

      if (characterValue != null) {
//        if (startOfPage && lastPosition == null) writeParagraphStart(output) // not sure this is correct for RTL?
        line += new LineItem(position)
      }

      maxHeightForLine = Math.max(maxHeightForLine, positionHeight)
      minYTopForLine = Math.min(minYTopForLine, positionY - positionHeight)
      lastPosition = current
      if (startOfPage) {
//        lastPosition.isParagraphStart = true
        lastPosition.isLineStart = true
        lastLineStartPosition = lastPosition
        startOfPage = false
      }
      lastWordSpacing = wordSpacing
      previousAveCharWidth = averageCharWidth
    }

    if (inTable)
      writeTable(buffer, output)
    else if (inParagraph)
    //     writeParagraph(buffer, output)

      if (!tempBuffer.isEmpty)
      //     writeParagraph(tempBuffer, output)

        writePageEnd(output)
  }

  //  private def processPosition(position: TextPosition, lastPosition: PositionWrapper): Unit = {
  //    val current: PositionWrapper = new PositionWrapper (position)
  //    val characterValue: String = position.getUnicode
  //
  //    if (lastPosition != null && ((position.getFont ne lastPosition.position.getFont) || position.getFontSize != lastPosition.position.getFontSize) ) previousAveCharWidth = - 1
  //
  //    var positionX: Float = position.getX
  //    var positionY: Float = position.getY
  //    var positionWidth: Float = position.getWidth
  //    var positionHeight: Float = position.getHeight
  //
  //    val wordCharCount: Int = position.getIndividualWidths.length
  //    val wordSpacing: Float = position.getWidthOfSpace
  //    var deltaSpace: Float = 0
  //
  //    if (wordSpacing == 0 || Float.NaN == wordSpacing) deltaSpace = Float.MaxValue
  //    else if (lastWordSpacing < 0) deltaSpace = wordSpacing * spacingTolerance
  //    else deltaSpace = (wordSpacing + lastWordSpacing) / 2f * spacingTolerance
  //
  //    var averageCharWidth: Float = 0
  //    if (previousAveCharWidth < 0) averageCharWidth = positionWidth / wordCharCount
  //    else averageCharWidth = (previousAveCharWidth + positionWidth / wordCharCount) / 2f
  //    val deltaCharWidth: Float = averageCharWidth * averageCharTolerance
  //    var expectedStartOfNextWordX: Float = EXPECTED_START_OF_NEXT_WORD_X_RESET_VALUE
  //
  //    if (endOfLastTextX != END_OF_LAST_TEXT_X_RESET_VALUE) expectedStartOfNextWordX = endOfLastTextX + Math.min (deltaSpace, deltaCharWidth)
  //
  //    if (lastPosition != null && ! overlap (positionY, positionHeight, maxYForLine, maxHeightForLine) ) {
  //    val currentLine: Line = new Line (cloneLine (line), current, lastPosition, lastLineStartPosition, maxHeightForLine, expectedStartOfNextWordX)
  //    processLine (currentLine, line, buffer, tempBuffer, output)
  //  }
  //
  //    if (positionY >= maxYForLine) maxYForLine = positionY
  //    endOfLastTextX = positionX + positionWidth
  //
  //    if (characterValue != null) {
  //    if (startOfPage && lastPosition == null) writeParagraphStart () // not sure this is correct for RTL?
  //    line += new LineItem (position)
  //  }
  //
  //    maxHeightForLine = Math.max (maxHeightForLine, positionHeight)
  //    minYTopForLine = Math.min (minYTopForLine, positionY - positionHeight)
  //    lastPosition = current
  //    if (startOfPage) {
  //    lastPosition.isParagraphStart = true
  //    lastPosition.isLineStart = true
  //    lastLineStartPosition = lastPosition
  //    startOfPage = false
  //  }
  //    lastWordSpacing = wordSpacing
  //    previousAveCharWidth = averageCharWidth
  //
  //  }

  private def processLine(currentLine: Line,
                          //                   line: ArrayBuffer[LineItem],
                          buffer: ArrayBuffer[Line],
                          tempBuffer: ArrayBuffer[Line],
                          output: StringWriter
                         ): PositionWrapper = {
    val maxTempBufferSize = MAX_TEMP_BUFFER_SIZE

    var lastLineStartPosition: PositionWrapper = null

    if (inTable) {
      if (isTabular) {
        if (tempBuffer != null && tempBuffer.size > 0) {
          buffer ++= tempBuffer
          tempBuffer.clear()
        }
        buffer += currentLine
        isTabular = false
      }
      // tempBuffer pas plein
      else if (tempBuffer != null && tempBuffer.size < maxTempBufferSize) tempBuffer += currentLine

      // pas tabular et tempBuffer plein
      else {
        // on écrit la table
        lastLineStartPosition = writeTable(buffer, output)
        inTable = false
        buffer.clear()


        //        for (l <- tempBuffer) {
        //          if (l.maxFontSize == bodyTextSize) {
        //
        //          }
        //        }

        // TODO analyse tempBuffer pour créer un début paragraph et le gérer dans le buffer
        lastLineStartPosition = writeLineList(tempBuffer, output)
        tempBuffer.clear()
      }
    }
    else {
      buffer += currentLine
      lastLineStartPosition = writeLineList(buffer, output)
      buffer.clear()
    }

    return lastLineStartPosition
  }

  /**
    * This will print the text of the processed page to "output". It will estimate, based on the coordinates of the
    * text, where newlines and word spacings should be placed. The text will be sorted only if that feature was
    * enabled.
    *
    * @throws IOException If there is an error writing the text.
    */
  //  @throws[IOException]
  protected def writePage(page: ArrayBuffer[TextPosition], output: StringWriter): Unit = {
    var maxYForLine: Float = MAX_Y_FOR_LINE_RESET_VALUE
    var minYTopForLine: Float = MIN_Y_TOP_FOR_LINE_RESET_VALUE
    var endOfLastTextX: Float = END_OF_LAST_TEXT_X_RESET_VALUE
    var lastWordSpacing: Float = LAST_WORD_SPACING_RESET_VALUE
    var maxHeightForLine: Float = MAX_HEIGHT_FOR_LINE_RESET_VALUE

    var lastPosition: PositionWrapper = null
    var lastLineStartPosition: PositionWrapper = null
    var startOfPage: Boolean = true
    // flag to indicate start of page
    var startOfArticle: Boolean = false

    if (page.size > 0) writePageStart(output)

    // Now cycle through to print the text.
    // We queue up a line at a time before we print so that we can convert
    // the line from presentation form to logical form (if needed).
    val line = new ArrayBuffer[LineItem]
    var mayBeTabular = new ArrayBuffer[Line]
    val table = new ArrayBuffer[Line]
    val lineToWrite = new ArrayBuffer[Line]

    // PDF files don't always store spaces. We will need to guess where we should add
    // spaces based on the distances between TextPositions. Historically, this was done
    // based on the size of the space character provided by the font. In general, this
    // worked but there were cases where it did not work. Calculating the average character
    // width and using that as a metric works better in some cases but fails in some cases
    // where the spacing worked. So we use both. NOTE: Adobe reader also fails on some of
    // these examples.
    // Keeps track of the previous average character width
    var previousAveCharWidth: Float = -1

    for (position <- page) {
      val current: PositionWrapper = new PositionWrapper(position)
      val characterValue: String = position.getUnicode

      // Resets the average character width when we see a change in font
      // or a change in the font size
      if (lastPosition != null && ((position.getFont ne lastPosition.position.getFont)
        || position.getFontSize != lastPosition.position.getFontSize))
        previousAveCharWidth = -1

      var positionX: Float = 0
      var positionY: Float = 0
      var positionWidth: Float = 0
      var positionHeight: Float = 0

      positionX = position.getX
      positionY = position.getY
      positionWidth = position.getWidth
      positionHeight = position.getHeight

      // The current amount of characters in a word
      val wordCharCount: Int = position.getIndividualWidths.length

      // Estimate the expected width of the space based on the
      // space character with some margin.
      val wordSpacing: Float = position.getWidthOfSpace
      var deltaSpace: Float = 0
      if (wordSpacing == 0 || Float.NaN == wordSpacing) deltaSpace = Float.MaxValue
      else if (lastWordSpacing < 0) deltaSpace = wordSpacing * spacingTolerance
      else deltaSpace = (wordSpacing + lastWordSpacing) / 2f * spacingTolerance

      // Estimate the expected width of the space based on the average character width
      // with some margin. This calculation does not make a true average (average of
      // averages) but we found that it gave the best results after numerous experiments.
      // Based on experiments we also found that .3 worked well.
      var averageCharWidth: Float = 0
      if (previousAveCharWidth < 0) averageCharWidth = positionWidth / wordCharCount
      else averageCharWidth = (previousAveCharWidth + positionWidth / wordCharCount) / 2f
      val deltaCharWidth: Float = averageCharWidth * averageCharTolerance

      // Compares the values obtained by the average method and the wordSpacing method
      // and picks the smaller number.
      var expectedStartOfNextWordX: Float = EXPECTED_START_OF_NEXT_WORD_X_RESET_VALUE
      if (endOfLastTextX != END_OF_LAST_TEXT_X_RESET_VALUE)
        expectedStartOfNextWordX = endOfLastTextX + Math.min(deltaSpace, deltaCharWidth)

      if (lastPosition != null) {
        // RDD - Here we determine whether this text object is on the current
        // line. We use the lastBaselineFontSize to handle the superscript
        // case, and the size of the current font to handle the subscript case.
        // Text must overlap with the last rendered baseline text by at least
        // a small amount in order to be considered as being on the same line.
        // XXX BC: In theory, this check should really check if the next char is in
        // full range seen in this line. This is what I tried to do with minYTopForLine,
        // but this caused a lot of regression test failures. So, I'm leaving it be for
        // now
        if (!overlap(positionY, positionHeight, maxYForLine, maxHeightForLine)) {
          val currentLine: Line = new Line(
            cloneLine(line),
            current,
            lastPosition,
            lastLineStartPosition,
            maxHeightForLine,
            expectedStartOfNextWordX
          )
          for (j <- line) {
            if (LineItem.isWordSeparator(j)) System.out.print(" ")
            else System.out.print(j.textPosition.getUnicode)
          }
          System.out.println()
          if (inTable) if (isTabular) {
            if (mayBeTabular != null && mayBeTabular.size > 0) {
              table ++= mayBeTabular
              mayBeTabular = new ArrayBuffer[Line]
            }
            table += currentLine
            isTabular = false
          }
          else if (mayBeTabular != null && mayBeTabular.size < 5) mayBeTabular += currentLine
          else {
            System.out.println()
            System.out.println("----------------------------------------------------------------------------")
            System.out.println("Write Table")
            for (l <- table) {
              for (i <- l.line) {
                System.out.print(i.textPosition.getUnicode)
              }
              System.out.println()
            }
            System.out.println("End Table")
            System.out.println("----------------------------------------------------------------------------")
            System.out.println()
            lineToWrite ++= table
            table.clear()
            inTable = false
            writeTable(lineToWrite, output)
            lineToWrite.clear()
            lineToWrite ++= mayBeTabular
            //                                    System.out.println("Write mayBeTabular");
            mayBeTabular.clear()
            //                                    lineToWrite.clear();
            lastLineStartPosition = writeLineList(lineToWrite, output)
            lineToWrite.clear()
          }
          else {
            lineToWrite += currentLine
            lastLineStartPosition = writeLineList(lineToWrite, output)
            lineToWrite.clear()
          }
          line.clear()
          //                        lastLineStartPosition = handleLineSeparation(current, lastPosition,
          //                                lastLineStartPosition, maxHeightForLine);
          expectedStartOfNextWordX = EXPECTED_START_OF_NEXT_WORD_X_RESET_VALUE
          maxYForLine = MAX_Y_FOR_LINE_RESET_VALUE
          maxHeightForLine = MAX_HEIGHT_FOR_LINE_RESET_VALUE
          minYTopForLine = MIN_Y_TOP_FOR_LINE_RESET_VALUE
        }

        if (expectedStartOfNextWordX != EXPECTED_START_OF_NEXT_WORD_X_RESET_VALUE && expectedStartOfNextWordX < positionX) if (lastPosition.position.getUnicode != null && !lastPosition.position.getUnicode.endsWith(" ")) line += LineItem.getWordSeparator
        else if (endOfLastTextX + Math.min(deltaSpace, deltaCharWidth) * 20f < positionX) {
          line += new LineItem(new TextPosition(position.getRotation, position.getPageWidth, position.getPageHeight, position.getTextMatrix, position.getEndX, position.getEndY, position.getHeight, position.getIndividualWidths()(0), position.getWidthOfSpace, ";", position.getCharacterCodes, position.getFont, position.getFontSize, position.getFontSizeInPt.toInt))
          isTabular = true
          inTable = true
        }
      }
      if (positionY >= maxYForLine) maxYForLine = positionY
      // RDD - endX is what PDF considers to be the x coordinate of the
      // end position of the text. We use it in computing our metrics below.
      endOfLastTextX = positionX + positionWidth
      // add it to the list
      if (characterValue != null) {
        if (startOfPage && lastPosition == null) writeParagraphStart(output) // not sure this is correct for RTL?
        line += new LineItem(position)
      }
      maxHeightForLine = Math.max(maxHeightForLine, positionHeight)
      minYTopForLine = Math.min(minYTopForLine, positionY - positionHeight)
      lastPosition = current
      if (startOfPage) {
        lastPosition.isParagraphStart = true
        lastPosition.isLineStart = true
        lastLineStartPosition = lastPosition
        startOfPage = false
      }
      lastWordSpacing = wordSpacing
      previousAveCharWidth = averageCharWidth
    }
    // print the final line
    //            if (line.size() > 0) {
    //                writeLine(normalize(line));
    //                writeParagraphEnd();
    //            }
    lineToWrite ++= table
    writeTable(lineToWrite, output)
    //            table.clear();
    //            inTable = false;
    lineToWrite.clear()
    lineToWrite ++= mayBeTabular
    //            System.out.println("Write mayBeTabular");
    //            mayBeTabular.clear();
    //            lineToWrite.clear();
    writeLineList(lineToWrite, output)
    //            if(table.size() > 0) {
    //                System.out.println();
    //                System.out.println("----------------------------------------------------------------------------");
    //                System.out.println("Write Table");
    //                for (List<LineItem> i : table) {
    //                    for (LineItem j : i) {
    //                        if (j.isTabularSeparator()) {
    //                            System.out.print(j.value);
    //                        } else {
    //                            System.out.print(j.getTextPosition().getUnicode());
    //                        }
    //                    System.out.println();
    //                }
    //                System.out.println("End Table");
    //                System.out.println("----------------------------------------------------------------------------");
    //                System.out.println();

    writePageEnd(output)
  }

  private def overlap(y1: Float, height1: Float, y2: Float, height2: Float) = within(y1, y2, .1f) || y2 <= y1 && y2 >= y1 - height1 || y1 <= y2 && y1 >= y2 - height2

  /**
    * This will determine of two floating point numbers are within a specified variance.
    *
    * @param first    The first number to compare to.
    * @param second   The second number to compare to.
    * @param variance The allowed variance.
    */
  private def within(first: Float, second: Float, variance: Float) = second < first + variance && second > first - variance

  /**
    * handles the line separator for a new line given the specified current and previous TextPositions.
    *
    * @param current               the current text position
    * @param lastPosition          the previous text position
    * @param lastLineStartPosition the last text position that followed a line separator.
    * @param maxHeightForLine      max height for positions since lastLineStartPosition
    * @return start position of the last line
    * @throws IOException if something went wrong
    */
  @throws[IOException]
  private def handleLineSeparation(current: PositionWrapper,
                                   lastPosition: PositionWrapper,
                                   lastLineStartPosition: PositionWrapper,
                                   maxHeightForLine: Float,
                                   output: StringWriter) = {
    current.isLineStart = true

    isParagraphSeparation(current, lastPosition, lastLineStartPosition, maxHeightForLine)

    if (current.isParagraphStart) {
      writeLineSeparator(output)
      if (inTitle) {
        writeTitleEnd(output)
        inTitle = false
      }
      else if (inNote) {
        writeNoteEnd(output)
        inNote = false
      }
      else if (inParagraph) {
        writeParagraphEnd(output)
      }

      if (current.position.getFontSizeInPt > bodyTextSize) {
        writeTitleStart(output)
        inTitle = true
      } else if (current.position.getFontSizeInPt < bodyTextSize) {
        writeNoteStart(output)
        inNote = true
      } else {
        writePageStart(output)
        inParagraph = true
      }
    }
    else writeLineSeparator(output)

    current
  }

  /**
    * tests the relationship between the last text position, the current text position and the last text position that
    * followed a line separator to decide if the gap represents a paragraph separation. This should <i>only</i> be
    * called for consecutive text positions that first pass the line separation test.
    * <p>
    * This base implementation tests to see if the lastLineStartPosition is null OR if the current vertical position
    * has dropped below the last text vertical position by at least 2.5 times the current text height OR if the current
    * horizontal position is indented by at least 2 times the current width of a space character.
    * </p>
    * <p>
    * This also attempts to identify text that is indented under a hanging indent.
    * </p>
    * <p>
    * This method sets the isParagraphStart and isHangingIndent flags on the current position object.
    * </p>
    *
    * @param position              the current text position. This may have its isParagraphStart or isHangingIndent flags set upon
    *                              return.
    * @param lastPosition          the previous text position (should not be null).
    * @param lastLineStartPosition the last text position that followed a line separator, or null.
    * @param maxHeightForLine      max height for text positions since lasLineStartPosition.
    */
  private def isParagraphSeparation(position: PositionWrapper,
                                    lastPosition: PositionWrapper,
                                    lastLineStartPosition: PositionWrapper,
                                    maxHeightForLine: Float): Unit = {
    var result = false
    if (lastLineStartPosition == null) result = true
    else {
      val yGap = Math.abs(position.position.getYDirAdj - lastPosition.position.getYDirAdj)
      val newYVal = multiplyFloat(dropThreshold, maxHeightForLine)
      // do we need to flip this for rtl?
      val xGap = position.position.getXDirAdj - lastLineStartPosition.position.getXDirAdj
      val newXVal = multiplyFloat(indentThreshold, position.position.getWidthOfSpace)
      val positionWidth = multiplyFloat(0.25f, position.position.getWidth)

      if (yGap > newYVal) result = true
      else if (xGap > newXVal) {
        // text is indented, but try to screen for hanging indent
        if (!lastLineStartPosition.isParagraphStart) result = true
        else position.isHangingIndent = true
      }
      else if (xGap < -position.position.getWidthOfSpace) {
        // text is left of previous line. Was it a hanging indent?
        if (!lastLineStartPosition.isParagraphStart) result = true
      }
      else if (Math.abs(xGap) < positionWidth) {
        // current horizontal position is within 1/4 a char of the last
        // linestart. We'll treat them as lined up.
        if (lastLineStartPosition.isHangingIndent) position.isHangingIndent = true
        else if (lastLineStartPosition.isParagraphStart) {
          // check to see if the previous line looks like
          // any of a number of standard list item formats
          val liPattern = matchListItemPattern(lastLineStartPosition)

          if (liPattern != null) {
            result = liPattern.findAllIn(position.position.getUnicode).hasNext
          }
        }
      }
    }
    if (result) position.isParagraphStart = true
  }

  /**
    * returns the list item Pattern object that matches the text at the specified PositionWrapper or null if the text
    * does not match such a pattern. The list of Patterns tested against is given by the {@link #getListItemPatterns()}
    * method. To add to the list, simply override that method (if sub-classing) or explicitly supply your own list
    * using {@link #setListItemPatterns(List)}.
    *
    * @param pw position
    * @return the matching pattern
    */
  private def matchListItemPattern(pw: PositionWrapper): Regex = {
    for (p <- getListItemPatterns) {
      if (p.findAllIn(pw.position.getUnicode).hasNext) return p
    }
    null
  }

  /**
    * returns a list of regular expression Patterns representing different common list item formats. For example
    * numbered items of form:
    * <ol>
    * <li>some text</li>
    * <li>more text</li>
    * </ol>
    * or
    * <ul>
    * <li>some text</li>
    * <li>more text</li>
    * </ul>
    * etc., all begin with some character pattern. The pattern "\\d+\." (matches "1.", "2.", ...) or "\[\\d+\]"
    * (matches "[1]", "[2]", ...).
    * <p>
    * This method returns a list of such regular expression Patterns.
    *
    * @return a list of Pattern objects.
    */
  protected def getListItemPatterns: Seq[Regex] = {
    if (listOfPatterns == null) {
      listOfPatterns = new ArrayBuffer[Regex]
      for (expression <- LIST_ITEM_EXPRESSIONS) {
        listOfPatterns += expression.r
      }
    }
    listOfPatterns
  }

  private def multiplyFloat(value1: Float, value2: Float) = {
    // multiply 2 floats and truncate the resulting value to 3 decimal places
    // to avoid wrong results when comparing with another float
    round(value1 * value2 * 1000 / 1000)
  }

  private def cloneLine(line: Seq[LineItem]) = {
    val cloned = new ArrayBuffer[LineItem]
    for (i <- line) {
      cloned += i
    }
    cloned
  }

  /**
    * Normalize the given list of TextPositions.
    *
    * @param line list of TextPositions
    * @return a list of strings, one string for every word
    */
  private def normalize(line: Seq[LineItem]) = {
    val wordPositions = new ArrayBuffer[ArrayBuffer[TextPosition]]

    var word = new ArrayBuffer[TextPosition]
    for (item <- line) {
      if (LineItem.isWordSeparator(item)) {
        wordPositions += word
        word = new ArrayBuffer[TextPosition]
      } else {
        word += item.textPosition
      }
    }
    wordPositions += word
    createWords(wordPositions)
  }

  private def createWords(words: ArrayBuffer[ArrayBuffer[TextPosition]]): mutable.MutableList[WordWithTextPositions] = {
    val normalizeWords = new mutable.MutableList[WordWithTextPositions]

    for (word <- words) {
      val lb = new StringBuilder
      for (item <- word) {
        lb.append(item.getUnicode)
      }
      normalizeWords += createWord(lb.toString(), word)
    }

    normalizeWords
  }

  /**
    * Used within {@link #normalize(List)} to create a single {@link WordWithTextPositions} entry.
    */
  private def createWord(word: String, wordPositions: Seq[TextPosition]): WordWithTextPositions = new WordWithTextPositions(word, wordPositions)

  /**
    * Handles the LTR and RTL direction of the given words. The whole implementation stands and falls with the given
    * word. If the word is a full line, the results will be the best. If the word contains of single words or
    * characters, the order of the characters in a word or words in a line may wrong, due to RTL and LTR marks and
    * characters!
    * <p>
    * Based on http://www.nesterovsky-bros.com/weblog/2013/07/28/VisualToLogicalConversionInJava.aspx
    *
    * @param word The word that shall be processed
    * @return new word with the correct direction of the containing characters
    */
  private def handleDirection(word: String): String = {
    val bidi = new Bidi(word, Bidi.DIRECTION_DEFAULT_LEFT_TO_RIGHT)

    // if there is pure LTR text no need to process further
    if (!bidi.isMixed && bidi.getBaseLevel == Bidi.DIRECTION_LEFT_TO_RIGHT) return word

    // collect individual bidi information
    val runCount = bidi.getRunCount
    val levels = new Array[Byte](runCount)
    //    val runs = new Array[Int](runCount)
    val runs = new Array[AnyRef](runCount)

    for (i <- 1 to runCount) {
      levels(i) = bidi.getRunLevel(i).toByte
      runs(i) = i.asInstanceOf[AnyRef]
    }

    // reorder individual parts based on their levels
    Bidi.reorderVisually(levels, 0, runs, 0, runCount)
    // collect the parts based on the direction within the run
    val result = new StringBuilder

    for (i <- 1 to runCount) {
      val index: Int = runs(i).asInstanceOf[Int]
      val start = bidi.getRunStart(index)
      val end = bidi.getRunLimit(index)
      val level = levels(index)

      if ((level & 1) != 0)
        for (i <- end to start by -1) {
          val character = word.charAt(i)
          if (Character.isMirrored(word.codePointAt(i))) if (MIRRORING_CHAR_MAP.contains(character)) result.append(MIRRORING_CHAR_MAP.get(character))
          else result.append(character)
          else result.append(character)
        }

      else result.append(word, start, end)
    }

    return result.toString
  }

  /**
    * Write a Java string to the output stream. The default implementation will ignore the <code>textPositions</code>
    * and just calls {@link #writeString(String)}.
    *
    * @param text          The text to write to the stream.
    * @param textPositions The TextPositions belonging to the text.
    */
  protected def writeString(text: String, output: StringWriter): Unit = {
    val s = text.trim
    if (!s.isEmpty) output.write(text)
  }

  private def writeTable(lineToWrite: Seq[Line], output: StringWriter): PositionWrapper = {
    output.write(tableStart)
    val lastLineStartPosition = writeLineList(lineToWrite, output)
    output.write(tableEnd)

    return lastLineStartPosition
  }

  private def writeLineList(lineToWrite: Seq[Line], output: StringWriter): PositionWrapper = {
    var pw: PositionWrapper = null
    for (l <- lineToWrite) {
      pw = handleLineSeparation(
        l.current,
        l.lastPosition,
        l.lastLineStartPosition,
        l.maxHeightForLine,
        output
      )

      writeLine(normalize(l.line), output)

//      if (!inTable)
//        pw = handleLineSeparation(
//          l.current,
//          l.lastPosition,
//          l.lastLineStartPosition,
//          l.maxHeightForLine,
//          output
//        )
    }

    pw
  }

  /**
    * Write a list of string containing a whole line of a document.
    *
    * @param line a list with the words of the given line
    * @throws IOException if something went wrong
    */
  @throws[IOException]
  private def writeLine(line: Seq[WordWithTextPositions], output: StringWriter): Unit = {
    val sb = new mutable.StringBuilder()

    for (word <- line) {
      sb.append(word.getText)
      sb.append(wordSeparator)
    }

    writeString(sb.toString(), output)
  }

  /**
    * Write something (if defined) at the start of a paragraph.
    *
    * @throws IOException if something went wrong
    */
  protected def writeParagraphStart(output: StringWriter): Unit = {
    output.write(paragraphStart)
  }

  /**
    * Write something (if defined) at the end of a paragraph.
    *
    * @throws IOException if something went wrong
    */
  protected def writeParagraphEnd(output: StringWriter): Unit = {
    output.write(paragraphEnd)
  }

  /**
    * Write something (if defined) at the start of a page.
    *
    * @throws IOException if something went wrong
    */
  @throws[IOException]
  protected def writePageStart(output: StringWriter): Unit = {
    output.write(pageStart)
  }

  /**
    * Write something (if defined) at the end of a page.
    *
    * @throws IOException if something went wrong
    */
  @throws[IOException]
  protected def writePageEnd(output: StringWriter): Unit = {
    output.write(pageEnd)
  }

  /**
    * Write the word separator value to the output stream.
    *
    * @throws IOException If there is a problem writing out the word separator to the document.
    */
  @throws[IOException]
  protected def writeWordSeparator(output: StringWriter): Unit = {
    output.write(wordSeparator)
  }

  /**
    * Write the line separator value to the output stream.
    *
    * @throws IOException If there is a problem writing out the line separator to the document.
    */
  @throws[IOException]
  protected def writeLineSeparator(output: StringWriter): Unit = {
    output.write(lineSeparator)
  }

  /**
    * writes the paragraph separator string to the output.
    *
    * @throws IOException if something went wrong
    */
  @throws[IOException]
  protected def writeParagraphSeparator(output: StringWriter): Unit = {
    writeParagraphEnd(output)
    //    writeParagraphStart()
  }


  /**
    * Write the line separator value to the output stream.
    *
    * @throws IOException If there is a problem writing out the line separator to the document.
    */
  @throws[IOException]
  protected def writeTitleStart(output: StringWriter): Unit = {
    output.write(titleStart)
  }

  /**
    * Write the line separator value to the output stream.
    *
    * @throws IOException If there is a problem writing out the line separator to the document.
    */
  @throws[IOException]
  protected def writeTitleEnd(output: StringWriter): Unit = {
    output.write(titleEnd)
  }

  /**
    * Write the line separator value to the output stream.
    *
    * @throws IOException If there is a problem writing out the line separator to the document.
    */
  @throws[IOException]
  protected def writeNoteStart(output: StringWriter): Unit = {
    output.write(noteStart)
  }

  /**
    * Write the line separator value to the output stream.
    *
    * @throws IOException If there is a problem writing out the line separator to the document.
    */
  @throws[IOException]
  protected def writeNoteEnd(output: StringWriter): Unit = {
    output.write(noteEnd)
  }
}