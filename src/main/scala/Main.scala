
import java.io.{File, PrintWriter}

import java.nio.file.{Paths, Files}
import better.files._
import parser.PDFTabularTextStripper
import util.control.Breaks._

object Main extends App {
  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  def getRecursiveListOfFiles(dir: File): Array[File] = {
    val these = dir.listFiles
    these ++ these.filter(_.isDirectory).flatMap(getRecursiveListOfFiles)
  }

  override def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      usage
    } else if (!Files.exists(Paths.get(args(0)))) {
      println("[ERROR] This Path doesn't exists")
    } else {
      process(args(0))
    }
  }

  def process(path: String): Unit = {
    java.util.logging.Logger
      .getLogger("org.apache.pdfbox").setLevel(java.util.logging.Level.SEVERE)

    val start = System.nanoTime

    val files = getRecursiveListOfFiles(new File(path))

    for (f <- files) {
      breakable {
        if (f.getName.toLowerCase().endsWith(".pdf")) {
          f.getParentFile.getAbsolutePath.replaceAll("pdf", "json").toFile.createIfNotExists(true, true)
          var pw: Option[PrintWriter] = None

          val company = f.getParentFile.getName
          val year = "\\d\\d\\d\\d".r.findFirstIn(f.getName).getOrElse("0")

          val stripper = new PDFTabularTextStripper

          println(f.getAbsoluteFile)

          var text: String = ""

          try {
            text = stripper.getText(f, company, year)
            pw = Some(new PrintWriter(new File(f.getAbsolutePath.replaceAll("pdf", "json"))))
            pw.get.write(text)
          } catch {
            case _ => break
          } finally {
             pw match {
               case Some(p) => p.close()
               case None => Nil
            }
          }
        }
      }
    }

    println(((System.nanoTime - start) / 1000000) + " milliseconds")
  }

  def usage: Unit = {
    println("[ERROR] Usage: java -jar pdf-parser<version>.jar <path-to-pdf>")
  }
}
