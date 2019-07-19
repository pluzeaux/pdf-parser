package parser

import json._
import model.{Block, Paragraph, Tabular, TextLine}
import org.apache.pdfbox.text.TextPosition
import play.api.libs.json._

import scala.math._

/**
  * Constitution des blocs de texte à partir de l'extraction du texte du document.
  * Les blocs sont de deux types :
  *   - paragraphes
  *   - tables
  *
  * Création des objets Json à partir des blocs constitués. Ces objets permettent la génération du contenu du
  * fichier json en sortie du traitement
  *
  * La méthode "process" est l'entrée du traitement, elle prend en paramètres une liste de liste de caractères.
  * Un caractère est un objet de type TextPosition et contient tous les éléments d'affichage du caractère :
  *   - position
  *   - police
  *   - etc.
  *
  * Le traitement regroupe les caractère en :
  *   - lignes
  *   - blocs
  *       . paragraphes
  *       . tables
  *
  * @author Philippe LUZEAUX
  *
  */
class JsonDocumentWriter() {

  implicit val config = JsonConfiguration(optionHandlers = OptionHandlers.WritesNull)

  /**
    * Ecriture d'un paragraphe Json
    */
  implicit val jParagraphWrites = new Writes[JParagraph] {
    def writes(paragraph: JParagraph) = Json.obj(
      "paragraph" -> paragraph.paragraph
    )
  }

  /**
    * Ecriture d'une cellule de table Json
    */
  implicit val jCellWrites = new Writes[JCell] {
    def writes(cell: JCell) = Json.obj(
      "cell" -> cell.cell
    )
  }

  /**
    * Ecriture d'une ligne de table Json
    */
  implicit val jRowWrites = new Writes[JRow] {
    def writes(row: JRow) = Json.obj(
      "cells" -> row.cells
    )
  }
  /**
    * Ecriture d'une table Json
    */
  implicit val jTablesWrites = new Writes[JTable] {
    def writes(table: JTable) = Json.obj(
      "rows" -> table.rows
    )
  }

  /**
    * Ecriture d'une page Json
    */
  implicit val jPageWrites = new Writes[JPage] {
    def writes(page: JPage) = Json.obj(
      "number" -> page.number,
      "paragraphs" -> page.paragraphs,
      "tables" -> page.tables
    )
  }

  /**
    * Ecriture d'un document Json
    */
  implicit val jDocumentWrites = new Writes[JDocument] {
    def writes(document: JDocument) = Json.obj(
      "name" -> document.name,
      "year" -> document.year,
      "pages" -> document.pages
    )
  }

  /**
    * Permet le regroupement de blocs correspondant au même élément (table)
    * Nous pouvons avoir un élément détecté comme paragraphe mais étant inclu dans une table
    */
  val filterBlocks: (List[Block], List[Block]) => ((Block, Block, Block) => Boolean) => List[Block] =
    (lst: List[Block], acc: List[Block]) => (f: (Block, Block, Block) => Boolean) => {
      lst
      match {
        case Nil =>
          acc.reverse ::: Nil
        case x :: y :: z :: xs if (f(x, y, z)) => filterBlocks(x.merge(joinLines(y, z)) :: xs, acc)(f)
        case x :: y :: z :: xs => filterBlocks(y :: z :: xs, x :: acc)(f)
        case x :: xs => filterBlocks(xs, x :: acc)(f)
      }
    }

  /**
    * Détection de la nature tabulé de trois blocs de texte
    * Cette fonction est utilisée en paramètre de la fonction "filterBlocks"
    */
  val mayBeTabular: (Block, Block, Block) => Boolean =
    (x: Block, y: Block, z: Block) => {
      x match {
        case _: Tabular =>
          y match {
            case _: Paragraph =>
              z match {
                case _: Tabular =>
                  z.asInstanceOf[Tabular].minNbCols == x.asInstanceOf[Tabular].minNbCols - 1
                case _ => false
              }
            case _ => false
          }
        case _ => false
      }
    }

  /**
    * Un caractère (first) se trouve t-il à l'intérieur d'une ligne (second) ?
    * C'est à dire détermine si le caractère est inclu dans l'alignement horizontal de la ligne
    *
    * @param first, caractère
    * @param second, ligne
    */
  val within: (Float, Float, Float) => Boolean =
    (first: Float, second: Float, variance: Float) => {
      second < first + variance && second > first - variance
    }

  /**
    * Un caractère se trouve t-il en partie dans l'axe horizontal d'une ligne ?
    * C'est à dire détermine si l'intersection entre l'alignement horizontal de la ligne et du caractère n'est pas vide
    *
    * @param p, caractère
    * @param l, ligne
    */
  val overlap: (TextPosition, TextLine) => Boolean =
    (p: TextPosition, l: TextLine) => {
      (within(p.getY, l.maxY, .1f)
        || l.maxY <= p.getY
        && l.maxY >= p.getY - p.getHeight
        || p.getY <= l.maxY
        && p.getY >= l.maxY - l.maxHeight
        )
    }

  /**
    * Détection de séparation de paragraphe
    */
  val isParagraphSeparation: (TextLine, TextLine) => Boolean =
    (line1: TextLine, line2: TextLine) => {
      abs(line1.line.last.getYDirAdj - line2.line.last.getYDirAdj) > line2.maxHeight * line1.dropThreshold
    }

  /**
    * Est-ce que deux lignes font partie du même paragraphe
    * Negation de la fonction "isParagraphSeparation"
    */
  val isInParagraph: (TextLine, TextLine) => Boolean =
    (line1: TextLine, line2: TextLine) => {
      !isParagraphSeparation(line1, line2)
    }

  /**
    * Détermine si deux lignes consécutives sont de type tabulée
    */
  val isInTable: (TextLine, TextLine) => Boolean =
    (line1: TextLine, line2: TextLine) => {
      line1.isTabular && line2.isTabular
    }

  /**
    * Détermine si deux ligne consécutives ne sont pas une rupture de bloc
    */
  val isNotBlocSeparation: (TextLine, TextLine) => Boolean =
    (line1: TextLine, line2: TextLine) => {
      isInParagraph(line1, line2) || (line1.isTabular && line2.isTabular)
    }

  /**
    * Détermine si l'on est dans le cas d'une rupture de cellule de tableau
    */
  val isCell: (TextPosition, TextPosition, TextLine) => Boolean =
    (p1: TextPosition, p2: TextPosition, line: TextLine) => {
      p2.getX - (p1.getX + p1.getWidth) > Math.max(line.deltaSpace, line.deltaCharWidth) * 20f
    }

  /**
    * Point d'entrée du traitement
    * @param pages
    * @param labels
    * @param company
    * @param year
    * @return JSon Document String
    */
  def process(pages: List[List[TextPosition]], labels: Array[String], company: String, year: String): String = {
    Json.toJson(JDocument(company, year.toInt, getPages(pages, labels))).toString
  }

  /**
    * Création de l'ensemble des pages Json du document
    *
    * @param pages
    * @param labels
    * @return Liste de pages
    */
  def getPages(pages: List[List[TextPosition]], labels: Array[String]): List[JPage] = {
    def writeThem(pages: List[List[TextPosition]], jPages: List[JPage], acc: Int = 0): List[JPage] = {
      pages match {
        case Nil =>
          jPages.reverse
        case x :: xs =>
          writeThem(xs,
            getPage(filterBlocks(groupLines(groupChars(x)(overlap))(isInParagraph), List())(mayBeTabular),
              labels(acc)) :: jPages, acc + 1)
      }
    }

    writeThem(pages, Nil)
  }

  /**
    * Création d'une page Json
    *
    * @param blocks
    * @param label
    * @return Une page
    */
  def getPage(blocks: List[Block], label: String): JPage = {
    def writeThem(blocks: List[Block], label: String, accParagraph: List[JParagraph], accTable: List[JTable]): JPage = {
      blocks match {
        case Nil =>
          JPage(label, accParagraph.reverse, accTable.reverse)

        case x :: xs =>
          x match {
            case _: Tabular =>
              x.asInstanceOf[Tabular].getTable(x)(isCell) match {
                case Some(t) =>
                  writeThem(xs, label, accParagraph, t :: accTable)
                case None =>
                  writeThem(xs, label, accParagraph, accTable)
              }

            case _: Paragraph =>
              x.asInstanceOf[Paragraph].getString match {
                case Some(p) =>
                  writeThem(xs, label, JParagraph(p) :: accParagraph, accTable)
                case None =>
                  writeThem(xs, label, accParagraph, accTable)
              }

          }
      }
    }

    writeThem(blocks, label, Nil, Nil)
  }

  /**
    * Regroupement des caractères en lignes
    *
    * @param list
    * @param f
    * @return Liste de lignes
    */
  def groupChars(list: List[TextPosition])(f: (TextPosition, TextLine) => Boolean): List[TextLine] = {
    def groupThem(lst: List[TextPosition], acc: TextLine): List[TextLine] = lst
    match {
      case Nil =>
        acc.reverse :: Nil
      case x :: xs =>
        acc.line match {
          case Nil => groupThem(xs, x :: acc)
          case _ :: _ if f(x, acc) =>
            groupThem(xs, x :: acc)
          case _ =>
            acc.reverse :: groupThem(xs, x :: new TextLine)
        }
    }

    groupThem(list, new TextLine)
  }

  /**
    * Regroupement des lignes en blocs
    *
    * @param list
    * @param f
    * @return Liste de blocs
    */
  def groupLines(list: List[TextLine])(f: (TextLine, TextLine) => Boolean): List[Block] = {
    def groupThem(lst: List[TextLine], acc: Option[Block]): List[Block] = lst
    match {
      case Nil =>
        acc match {
          case Some(a) => a.reverse :: Nil
          case None => Nil
        }

      case x :: xs =>
        acc match {
          case Some(a) =>

            a match {
              case _: Tabular =>
                if (x.isTabular) groupThem(xs, Some(x :: a))
                else a.reverse :: groupThem(xs, Some(Paragraph(x :: Nil)))
              case _: Paragraph if x.isTabular =>
                a.reverse :: groupThem(xs, Some(Tabular(x :: Nil)))
              case _: Paragraph =>
                if (f(a.block.head, x)) groupThem(xs, Some(x :: a))
                else a.reverse :: groupThem(xs, Some(Paragraph(x :: Nil)))
            }

          case None =>
            if (x.isTabular) groupThem(xs, Some(Tabular(x :: Nil)))
            else groupThem(xs, Some(Paragraph(x :: Nil)))
        }
    }

    groupThem(list, None)
  }

  /**
    * regroupement des lignes de deux blocs différents
    *
    * @param block1
    * @param block2
    * @return Block
    */
  def joinLines(block1: Block, block2: Block): Block = {
    block1.block.reduceRight(_ :: _) :: block2.block.head
    block2
  }
}