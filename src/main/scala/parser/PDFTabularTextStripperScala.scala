package parser

import java.io.{IOException, StringWriter, Writer}
import java.text.{Bidi, Normalizer}
// import java.util
import java.util.regex.Pattern

import org.apache.pdfbox.pdmodel.{PDDocument, PDPage, PDPageTree}
import org.apache.pdfbox.text.TextPosition

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math._

class PDFTabularTextStripperScala() extends LegacyPDFStreamEngine {
  private val END_OF_LAST_TEXT_X_RESET_VALUE = -1
  private val MAX_Y_FOR_LINE_RESET_VALUE = -Float.MaxValue
  private val EXPECTED_START_OF_NEXT_WORD_X_RESET_VALUE = -Float.MaxValue
  private val MAX_HEIGHT_FOR_LINE_RESET_VALUE = -1
  private val MIN_Y_TOP_FOR_LINE_RESET_VALUE = Float.MaxValue
  private val LAST_WORD_SPACING_RESET_VALUE = -1

  protected val LINE_SEPARATOR: String = System.getProperty("line.separator")

  private val lineSeparator = LINE_SEPARATOR
  private val wordSeparator = " "

  private[this] var _paragraphStart: String = ""

  def paragraphStart: String = _paragraphStart

  def paragraphStart_=(value: String): Unit = {
    _paragraphStart = value
  }

  private[this] var _paragraphEnd: String = ""

  def paragraphEnd: String = _paragraphEnd

  def paragraphEnd_=(value: String): Unit = {
    _paragraphEnd = value
  }

  private[this] var _tableStart: String = ""

  def tableStart: String = _tableStart

  def tableStart_=(value: String): Unit = {
    _tableStart = value
  }

  private[this] var _tableEnd: String = ""

  def tableEnd: String = _tableEnd

  def tableEnd_=(value: String): Unit = {
    _tableEnd = value
  }

  private[this] var _pageStart: String = ""

  def pageStart: String = _pageStart

  def pageStart_=(value: String): Unit = {
    _pageStart = value
  }

  private[this] var _pageEnd: String = LINE_SEPARATOR

  def pageEnd: String = _pageEnd

  def pageEnd_=(value: String): Unit = {
    _pageEnd = value
  }

  private var articleStart = ""
  private var articleEnd = ""

  private val defaultIndentThreshold = 2.0f
  private val defaultDropThreshold = 2.5f
  private var spacingTolerance = .5f
  private var averageCharTolerance = .3f

  private var indentThreshold = defaultIndentThreshold
  private var dropThreshold = defaultDropThreshold

  private var charactersByArticle: ArrayBuffer[TextPosition] = null
  protected var output: Writer = null

  /**
    * True if we started a paragraph but haven't ended it yet.
    */
  private var inParagraph: Boolean = false
  private var inTable: Boolean = false
  private var isTabular: Boolean = false

  /**
    * This will return the text of a document. See writeText. <br>
    * NOTE: The document must not be encrypted when coming into this method.
    *
    * @param doc The document to get the text from.
    * @return The text of the PDF document.
    * @throws IOException if the doc state is invalid or it is encrypted.
    */
  //  @throws[IOException]
  def getText(doc: PDDocument): String = {
    writeText(doc)
    output.toString
  }

  /**
    * This will take a PDDocument and write the text of that document to the print writer.
    *
    * @param doc          The document to get the data from.
    * @param outputStream The location to put the text.
    * @throws IOException If the doc is in an invalid state.
    */
  //  @throws[IOException]
  def writeText(doc: PDDocument): Unit = {
    startDocument(doc)
    processPages(doc.getPages)
    endDocument(doc)
  }

  /**
    * This will process all of the pages and the text that is in them.
    *
    * @param pages The pages object in the document.
    * @throws IOException If there is an error parsing the text.
    */
  //  @throws[IOException]
  protected def processPages(pages: PDPageTree): Unit = {
    var currentPageNo: Int = 0
    output = new StringWriter()

    for (page <- pages) {
      charactersByArticle = new ArrayBuffer[TextPosition]
      currentPageNo += 1
      if (page.hasContents) {
        processPage(page)
        writePage()
        endPage(page)
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
  protected def writePage(): Unit = {
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

    if (charactersByArticle.size > 0) writePageStart()
    import scala.collection.JavaConversions._

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

    //      while ( {
    //        textIter.hasNext
    //      }) {
    //        val position: TextPosition = textIter.next

    for (position <- charactersByArticle) {
      val current: PositionWrapper = new PositionWrapper(position)
      val characterValue: String = position.getUnicode
      // Resets the average character width when we see a change in font
      // or a change in the font size
      if (lastPosition != null && ((position.getFont ne lastPosition.position.getFont) || position.getFontSize != lastPosition.position.getFontSize)) previousAveCharWidth = -1
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
      if (endOfLastTextX != END_OF_LAST_TEXT_X_RESET_VALUE) expectedStartOfNextWordX = endOfLastTextX + Math.min(deltaSpace, deltaCharWidth)
      if (lastPosition != null) {
        if (startOfArticle) {
          lastPosition.isArticleStart = true
          startOfArticle = false
        }
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
          val currentLine: Line = new Line(cloneLine(line), current, lastPosition, lastLineStartPosition, maxHeightForLine,expectedStartOfNextWordX)
          import scala.collection.JavaConversions._
          for (j <- line) {
            if (LineItem.isWordSeparator(j)) System.out.print(" ")
            else System.out.print(j.textPosition.getUnicode)
          }
          System.out.println()
          if (inTable) if (isTabular) {
            if (mayBeTabular != null && mayBeTabular.size > 0) {
              table.addAll(mayBeTabular)
              mayBeTabular = new ArrayBuffer[Line]
            }
            table.add(currentLine)
            isTabular = false
          }
          else if (mayBeTabular != null && mayBeTabular.size < 5) mayBeTabular.add(currentLine)
          else {
            System.out.println()
            System.out.println("----------------------------------------------------------------------------")
            System.out.println("Write Table")
            import scala.collection.JavaConversions._
            for (l <- table) {
              for (i <- l.line) {
                System.out.print(i.textPosition.getUnicode)
              }
              System.out.println()
            }
            System.out.println("End Table")
            System.out.println("----------------------------------------------------------------------------")
            System.out.println()
            lineToWrite.addAll(table)
            table.clear()
            inTable = false
            writeTable(lineToWrite)
            lineToWrite.clear()
            lineToWrite.addAll(mayBeTabular)
            //                                    System.out.println("Write mayBeTabular");
            mayBeTabular.clear()
            //                                    lineToWrite.clear();
            lastLineStartPosition = writeLineList(lineToWrite)
            lineToWrite.clear()
          }
          else {
            lineToWrite.add(currentLine)
            lastLineStartPosition = writeLineList(lineToWrite)
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

        if (expectedStartOfNextWordX != EXPECTED_START_OF_NEXT_WORD_X_RESET_VALUE && expectedStartOfNextWordX < positionX) if (lastPosition.position.getUnicode != null && !lastPosition.position.getUnicode.endsWith(" ")) line.add(LineItem.getWordSeparator)
        else if (endOfLastTextX + Math.min(deltaSpace, deltaCharWidth) * 20f < positionX) {
          line.add(new LineItem(new TextPosition(position.getRotation, position.getPageWidth, position.getPageHeight, position.getTextMatrix, position.getEndX, position.getEndY, position.getHeight, position.getIndividualWidths()(0), position.getWidthOfSpace, ";", position.getCharacterCodes, position.getFont, position.getFontSize, position.getFontSizeInPt.toInt)))
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
        if (startOfPage && lastPosition == null) writeParagraphStart() // not sure this is correct for RTL?
        line.add(new LineItem(position))
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
    lineToWrite.addAll(table)
    writeTable(lineToWrite)
    //            table.clear();
    //            inTable = false;
    lineToWrite.clear()
    lineToWrite.addAll(mayBeTabular)
    //            System.out.println("Write mayBeTabular");
    //            mayBeTabular.clear();
    //            lineToWrite.clear();
    writeLineList(lineToWrite)
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

    writePageEnd()
  }

  private def writeTable(lineToWrite: Seq[Line]): Unit = {
    output.write(tableStart)
    for (l <- lineToWrite) {
      writeLine(normalize(l.line))
      output.write(lineSeparator)
    }
    output.write(tableEnd)
  }

  private def writeLineList(lineToWrite: Seq[Line]) = {
    var pw: PositionWrapper = null
    for (l <- lineToWrite) {
      writeLine(normalize(l.line))
      pw = handleLineSeparation(l.current, l.lastPosition, l.lastLineStartPosition, l.maxHeightForLine)
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
  private def writeLine(line: Seq[WordWithTextPositions]): Unit = {
    val numberOfStrings = line.size
    var i = 0
    while ( {
      i < numberOfStrings
    }) {
      val word = line.get(i)
      writeString(word.getText, word.textPositions)
      if (i < numberOfStrings - 1) writeWordSeparator()

      {
        i += 1;
        i - 1
      }
    }
  }

  /**
    * Normalize the given list of TextPositions.
    *
    * @param line list of TextPositions
    * @return a list of strings, one string for every word
    */
  private def normalize(line: Seq[LineItem]) = {
    val normalized = new mutable.MutableList[WordWithTextPositions]
    var lineBuilder = new StringBuilder
    val wordPositions = new ArrayBuffer[TextPosition]
    import scala.collection.JavaConversions._
    for (item <- line) {
      lineBuilder = normalizeAdd(normalized, lineBuilder, wordPositions, item)
    }
    if (lineBuilder.length > 0) normalized.add(createWord(lineBuilder.toString, wordPositions))
    normalized
  }

  /**
    * Used within {@link #normalize(List)} to handle a {@link TextPosition}.
    *
    * @return The StringBuilder that must be used when calling this method.
    */
  private def normalizeAdd(normalized: Seq[WordWithTextPositions], lineBuilder: StringBuilder, wordPositions: Seq[TextPosition], item: LineItem) = {
    var lb: mutable.StringBuilder = null
    if (LineItem.isWordSeparator(item)) {
      normalized.add(createWord(lineBuilder.toString, new ArrayBuffer[TextPosition]))
      lb = new StringBuilder
      wordPositions.clear()
    }
    else {
      val text = item.textPosition
      lb = lineBuilder
      lb.append(text.getUnicode)
      wordPositions.add(text)
    }
    lb
  }

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
  private def handleLineSeparation(current: PositionWrapper, lastPosition: PositionWrapper, lastLineStartPosition: PositionWrapper, maxHeightForLine: Float) = {
    current.isLineStart = true
    isParagraphSeparation(current, lastPosition, lastLineStartPosition, maxHeightForLine)
    if (current.isParagraphStart) if (lastPosition.isArticleStart) {
      if (lastPosition.isLineStart) writeLineSeparator()
      writeParagraphStart()
    }
    else {
      writeLineSeparator()
      writeParagraphSeparator()
    }
    else writeLineSeparator()
    current
  }

  private def cloneLine(line: Seq[LineItem]) = {
    val cloned = new ArrayBuffer[LineItem]
    import scala.collection.JavaConversions._
    for (i <- line) {
      cloned.add(i)
    }
    cloned
  }

  /**
    * Write a Java string to the output stream. The default implementation will ignore the <code>textPositions</code>
    * and just calls {@link #writeString(String)}.
    *
    * @param text          The text to write to the stream.
    * @param textPositions The TextPositions belonging to the text.
    * @throws IOException If there is an error when writing the text.
    */
  protected def writeString(text: String, textPositions: ArrayBuffer[TextPosition]): Unit = {
    writeString(text)
  }

  /**
    * Write a Java string to the output stream. The default implementation will ignore the <code>textPositions</code>
    * and just calls {@link #writeString(String)}.
    *
    * @param text          The text to write to the stream.
    * @param textPositions The TextPositions belonging to the text.
    * @throws IOException If there is an error when writing the text.
    */
  @throws[IOException]
  protected def writeString(text: String, textPositions: Seq[TextPosition]): Unit = {
    writeString(text)
  }

  /**
    * Write a Java string to the output stream.
    *
    * @param text The text to write to the stream.
    * @throws IOException If there is an error when writing the text.
    */
  protected def writeString(text: String): Unit = {
    val s = text.trim
    if (!s.isEmpty) output.write(text)
  }

  /**
    * Used within {@link #normalize(List)} to create a single {@link WordWithTextPositions} entry.
    */
  private def createWord(word: String, wordPositions: Seq[TextPosition]) = new WordWithTextPositions(normalizeWord(word), wordPositions)

  /**
    * Normalize certain Unicode characters. For example, convert the single "fi" ligature to "f" and "i". Also
    * normalises Arabic and Hebrew presentation forms.
    *
    * @param word Word to normalize
    * @return Normalized word
    */
  private def normalizeWord(word: String): String = {
    var builder: mutable.StringBuilder = null
    var p = 0
    var q = 0
    val strLength = word.length

    while ( {
      q < strLength
    }) { // We only normalize if the codepoint is in a given range.
      // Otherwise, NFKC converts too many things that would cause
      // confusion. For example, it converts the micro symbol in
      // extended Latin to the value in the Greek script. We normalize
      // the Unicode Alphabetic and Arabic A&B Presentation forms.
      val c = word.charAt(q)
      if (0xFB00 <= c && c <= 0xFDFF || 0xFE70 <= c && c <= 0xFEFF) {
        if (builder == null) builder = new StringBuilder(strLength * 2)
        builder.append(word.substring(p, q))
        // Some fonts map U+FDF2 differently than the Unicode spec.
        // They add an extra U+0627 character to compensate.
        // This removes the extra character for those fonts.
        if (c == 0xFDF2 && q > 0 && (word.charAt(q - 1) == 0x0627 || word.charAt(q - 1) == 0xFE8D)) builder.append("\u0644\u0644\u0647")
        else { // Trim because some decompositions have an extra space, such as U+FC5E
          builder.append(Normalizer.normalize(word.substring(q, q + 1), Normalizer.Form.NFKC).trim)
        }
        p = q + 1
      }

      {
        q += 1;
        q - 1
      }
    }
    if (builder == null) handleDirection(word)
    else {
      builder.append(word.substring(p, q))
      handleDirection(builder.toString)
    }
  }

  private val MIRRORING_CHAR_MAP = new mutable.HashMap[Character, Character]

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
          val character = word.charAt(end)
          if (Character.isMirrored(word.codePointAt(end))) if (MIRRORING_CHAR_MAP.containsKey(character)) result.append(MIRRORING_CHAR_MAP.get(character))
          else result.append(character)
          else result.append(character)
        }

      else result.append(word, start, end)
    }

    return result.toString
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
      else if (xGap > newXVal) { // text is indented, but try to screen for hanging indent
        if (!lastLineStartPosition.isParagraphStart) result = true
        else position.isHangingIndent = true
      }
      else if (xGap < -position.position.getWidthOfSpace) { // text is left of previous line. Was it a hanging indent?
        if (!lastLineStartPosition.isParagraphStart) result = true
      }
      else if (Math.abs(xGap) < positionWidth) { // current horizontal position is within 1/4 a char of the last
        // linestart. We'll treat them as lined up.
        if (lastLineStartPosition.isHangingIndent) position.isHangingIndent = true
        else if (lastLineStartPosition.isParagraphStart) { // check to see if the previous line looks like
          // any of a number of standard list item formats
          val liPattern = matchListItemPattern(lastLineStartPosition)
          if (liPattern != null) {
            val currentPattern = matchListItemPattern(position)
            if (liPattern eq currentPattern) result = true
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
  private def matchListItemPattern(pw: PositionWrapper) = {
    val tp = pw.position
    val txt = tp.getUnicode
    matchPattern(txt, getListItemPatterns)
  }

  /**
    * iterates over the specified list of Patterns until it finds one that matches the specified string. Then returns
    * the Pattern.
    * <p>
    * Order of the supplied list of patterns is important as most common patterns should come first. Patterns should be
    * strict in general, and all will be used with case sensitivity on.
    * </p>
    *
    * @param string   the string to be searched
    * @param patterns list of patterns
    * @return matching pattern
    */
  protected def matchPattern(string: String, patterns: Seq[Pattern]): Pattern = {
    for (p <- patterns) {
      if (p.matcher(string).matches) return p
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
  protected def getListItemPatterns: Seq[Pattern] = {
    if (listOfPatterns == null) {
      listOfPatterns = new ArrayBuffer[Pattern]
      for (expression <- LIST_ITEM_EXPRESSIONS) {
        val p = Pattern.compile(expression)
        listOfPatterns.add(p)
      }
    }
    listOfPatterns
  }

  /**
    * a list of regular expressions that match commonly used list item formats, i.e. bullets, numbers, letters, Roman
    * numerals, etc. Not meant to be comprehensive.
    */
  private val LIST_ITEM_EXPRESSIONS = Array("\\.", "\\d+\\.", "\\[\\d+\\]", "\\d+\\)", "[A-Z]\\.", "[a-z]\\.", "[A-Z]\\)", "[a-z]\\)", "[IVXL]+\\.", "[ivxl]+\\.")

  private var listOfPatterns: Seq[Pattern] = null

  /**
    * use to supply a different set of regular expression patterns for matching list item starts.
    *
    * @param patterns list of patterns
    */
  protected def setListItemPatterns(patterns: Seq[Pattern]): Unit = {
    listOfPatterns = patterns
  }

  private def multiplyFloat(value1: Float, value2: Float) = { // multiply 2 floats and truncate the resulting value to 3 decimal places
    // to avoid wrong results when comparing with another float
    round(value1 * value2 * 1000 / 1000)
  }

  /**
    * This will process a TextPosition object and add the text to the list of characters on a page. It takes care of
    * overlapping text.
    *
    * @param text The text to process.
    */
  override protected def processTextPosition(text: TextPosition): Unit = {
    if (charactersByArticle.isEmpty)
      charactersByArticle.add(text)
    else {
      val previousTextPosition = charactersByArticle.last
      if (text.isDiacritic && previousTextPosition.contains(text)) previousTextPosition.mergeDiacritic(text)
      else {
        if (previousTextPosition.isDiacritic && text.contains(previousTextPosition)) {
          text.mergeDiacritic(previousTextPosition)
          charactersByArticle.remove(charactersByArticle.last)
          charactersByArticle.add(text)
        }
        else charactersByArticle.add(text)
      }
    }
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
    * End a page. Default implementation is to do nothing. Subclasses may provide additional information.
    *
    * @param page The page we are about to process.
    * @throws IOException If there is any error writing to the stream.
    */
  @throws[IOException]
  protected def endPage(page: PDPage): Unit = {
    // default is to do nothing
  }

  /**
    * Start a new article, which is typically defined as a column on a single page (also referred to as a bead). This
    * assumes that the primary direction of text is left to right. Default implementation is to do nothing. Subclasses
    * may provide additional information.
    *
    * @throws IOException If there is any error writing to the stream.
    */
  @throws[IOException]
  protected def startArticle(): Unit = {
    startArticle(true)
  }

  /**
    * End an article. Default implementation is to do nothing. Subclasses may provide additional information.
    *
    * @throws IOException If there is any error writing to the stream.
    */
  protected def endArticle(): Unit = {
    output.write(articleEnd)
  }

  /**
    * Start a new article, which is typically defined as a column on a single page (also referred to as a bead).
    * Default implementation is to do nothing. Subclasses may provide additional information.
    *
    * @param isLTR true if primary direction of text is left to right.
    * @throws IOException If there is any error writing to the stream.
    */
  protected def startArticle(isLTR: Boolean): Unit = {
    output.write(articleStart)
  }


  /**
    * Write something (if defined) at the start of a paragraph.
    *
    * @throws IOException if something went wrong
    */
  protected def writeParagraphStart(): Unit = {
    if (inParagraph) {
      writeParagraphEnd()
      inParagraph = false
    }
    output.write(paragraphStart)
    inParagraph = true
  }

  /**
    * Write something (if defined) at the end of a paragraph.
    *
    * @throws IOException if something went wrong
    */
  protected def writeParagraphEnd(): Unit = {
    if (!inParagraph) writeParagraphStart()
    output.write(paragraphEnd)
    inParagraph = false
  }

  /**
    * Write something (if defined) at the start of a page.
    *
    * @throws IOException if something went wrong
    */
  @throws[IOException]
  protected def writePageStart(): Unit = {
    output.write(pageStart)
  }

  /**
    * Write something (if defined) at the end of a page.
    *
    * @throws IOException if something went wrong
    */
  @throws[IOException]
  protected def writePageEnd(): Unit = {
    output.write(pageEnd)
  }

  /**
    * This method is available for subclasses of this class. It will be called before processing of the document start.
    *
    * @param document The PDF document that is being processed.
    * @throws IOException If an IO error occurs.
    */
  @throws[IOException]
  protected def startDocument(document: PDDocument): Unit = {
    // no default implementation, but available for subclasses
  }

  /**
    * This method is available for subclasses of this class. It will be called after processing of the document
    * finishes.
    *
    * @param document The PDF document that is being processed.
    * @throws IOException If an IO error occurs.
    */
  @throws[IOException]
  protected def endDocument(document: PDDocument): Unit = {
  }

  /**
    * Write the word separator value to the output stream.
    *
    * @throws IOException If there is a problem writing out the word separator to the document.
    */
  @throws[IOException]
  protected def writeWordSeparator(): Unit = {
    output.write(wordSeparator)
  }

  /**
    * Write the line separator value to the output stream.
    *
    * @throws IOException If there is a problem writing out the line separator to the document.
    */
  @throws[IOException]
  protected def writeLineSeparator(): Unit = {
    output.write(lineSeparator)
  }

  /**
    * writes the paragraph separator string to the output.
    *
    * @throws IOException if something went wrong
    */
  @throws[IOException]
  protected def writeParagraphSeparator(): Unit = {
    writeParagraphEnd()
    writeParagraphStart()
  }
}