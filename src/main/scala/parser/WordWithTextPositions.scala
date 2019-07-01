package parser

import org.apache.pdfbox.text.TextPosition

class WordWithTextPositions(var text: String, var textPositions: Seq[TextPosition]) {
  def getText: String = text

  def getTextPositions: Seq[TextPosition] = textPositions
}
