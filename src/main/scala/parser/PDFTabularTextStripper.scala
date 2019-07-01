package parser

import java.io.{File, IOException}
import java.util
import java.util.HashMap

import org.apache.pdfbox.cos.COSName
import org.apache.pdfbox.pdmodel.common.PDPageLabels
import org.apache.pdfbox.pdmodel.{PDDocument, PDPageTree}
import org.apache.pdfbox.text.TextPosition

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

class PDFTabularTextStripper() extends LegacyPDFStreamEngine {
  private var document: ArrayBuffer[ArrayBuffer[TextPosition]] = null
  private var page: ArrayBuffer[TextPosition] = null
  //  private var characterListMapping: mutable.TreeMap[String, mutable.TreeMap[Float, mutable.TreeSet[Float]]] = null

  private var characterListMapping = new HashMap[String, util.TreeMap[Float, util.TreeSet[Float]]]

  private val suppressDuplicateOverlappingText = true

  /**
    * This will return the text of a document. See writeText. <br>
    * NOTE: The document must not be encrypted when coming into this method.
    *
    * @param doc The document to get the text from.
    * @return The text of the PDF document.
    * @throws IOException if the doc state is invalid or it is encrypted.
    */
  //  @throws[IOException]
  def getText(file: File, company: String, year: String): String = {
    document = new ArrayBuffer[ArrayBuffer[TextPosition]]
    //    characterListMapping = new mutable.TreeMap[String, mutable.TreeMap[Float, mutable.TreeSet[Float]]]
    characterListMapping = new HashMap[String, util.TreeMap[Float, util.TreeSet[Float]]]
    val pdf = PDDocument.load(file)
    processPages(pdf.getPages)
    val writer = new FunctionalXMLPageWriter2()

    val documentArray = new ArrayBuffer[List[TextPosition]]()
    for (p <- document)
      documentArray += p.toList

    val labels = if(pdf.getDocumentCatalog.getPageLabels != null) pdf.getDocumentCatalog.getPageLabels.getLabelsByPageIndices else 1.to(pdf.getPages.getCount).map(_.toString).toArray

    val json = writer.process(documentArray.toList, labels, company, year)
    pdf.close()

    json
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
    page = new ArrayBuffer[TextPosition]
    for (p <- pages) {
      val q = p.getCOSObject.getCOSDictionary(COSName.PARENT).getCOSObject(COSName.COUNT) //Item("Parent").getCOSObject.getItem("Count")
      currentPageNo += 1
      //      println("Traitement de la page n°" + currentPageNo)
      page = new ArrayBuffer[TextPosition]
      characterListMapping.clear()
      if (p.hasContents) {
        val start = System.nanoTime()
        super.processPage(p)
        //        println("Temps de traitement d'une page : " + ((System.nanoTime() - start) / 1000000 )+ " milliseconds")
        document += page
      }
    }
  }

  /**
    * This will process a TextPosition object and add the text to the list of characters on a page. It takes care of
    * overlapping text.
    *
    * @param text The text to process.
    */
  override protected def processTextPosition(text: TextPosition): Unit = {
    val start = System.nanoTime()
    var showCharacter = true
    if (suppressDuplicateOverlappingText) {
      showCharacter = false
      val textCharacter = text.getUnicode
      val textX = text.getX
      val textY = text.getY
      //     var sameTextCharacters: mutable.TreeMap[Float, mutable.TreeSet[Float]] = null
      var sameTextCharacters: util.TreeMap[Float, util.TreeSet[Float]] = null

      val start2 = System.nanoTime()
      //      val opt = characterListMapping.get(textCharacter)
      sameTextCharacters = characterListMapping.get(textCharacter)
//      println("Search time : " + ((System.nanoTime() - start2)) + " nanoseconds")

      if (sameTextCharacters == null) {
        sameTextCharacters = new util.TreeMap[Float, util.TreeSet[Float]]
        characterListMapping.put(textCharacter, sameTextCharacters)
      }

      //      if (opt util.TreeMap[Float, util.TreeSet[Float]]
      //      opt match {
      //        case Some(m) => sameTextCharacters = m
      //        case None => { sameTextCharacters = new mutable.TreeMap[Float, mutable.TreeSet[Float]]
      //          characterListMapping.put(textCharacter, sameTextCharacters)}
      //      }

      var suppressCharacter = false
      val tolerance = text.getWidth / textCharacter.length / 3.0f
//      val xMatches = sameTextCharacters.filterKeys(k => (k >= textX - tolerance && k <= textX + tolerance))
      val xMatches = sameTextCharacters.subMap(textX - tolerance, textX + tolerance)


      //      val yMatches = xMatches.filterKeys(l => (l >= textY - tolerance && l <= textY + tolerance))

      breakable {
        for (xMatch <- xMatches.values) {
//          val yMatches = xMatch.filter(k => (k >= textY - tolerance && k <= textY + tolerance))
          val yMatches = xMatch.subSet(textY - tolerance, textY + tolerance)

          if (!yMatches.isEmpty) {
            suppressCharacter = true
            break
          }
        }
      }

      if (!suppressCharacter) {
        //        var ySet: mutable.TreeSet[Float] = null
        //        var ySet: mutable.TreeSet[Float] = null
        var ySet: util.TreeSet[Float] = null
        //        ySet = sameTextCharacters.getOrElse(textX, null)
        ySet = sameTextCharacters.get(textX)
        if (ySet == null) {
          //          ySet = new mutable.TreeSet[Float]
          ySet = new util.TreeSet[Float]
          sameTextCharacters.put(textX, ySet)
        }
        ySet.add(textY)
        showCharacter = true
      }
    }
    if (showCharacter) {
      if (page.isEmpty) page.add(text)
      else {
        val previousTextPosition = page.get(page.size - 1)
        if (text.isDiacritic && previousTextPosition.contains(text)) previousTextPosition.mergeDiacritic(text)
        else {
          if (previousTextPosition.isDiacritic && text.contains(previousTextPosition)) {
            text.mergeDiacritic(previousTextPosition)
            page.remove(page.size - 1)
            page.add(text)
          }
          else page.add(text)
        }
      }
    }
    //   println("Process text position : " + ((System.nanoTime() - start) ) + " nanoseconds")
  }
}