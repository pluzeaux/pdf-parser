
import java.io.{File, PrintWriter}

import better.files._
import parser.PDFTabularTextStripper

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
    val start = System.nanoTime()
    //    val files = getRecursiveListOfFiles(new File("/Users/Philippe/Development/POC_NLP_Documents_references/documents/pdf"))
    val files = getRecursiveListOfFiles(new File("/Users/Philippe/Development/POC_NLP_Documents_references/test/pdf"))
    //    val files = getRecursiveListOfFiles(new File("/Users/Philippe/Development/POC_NLP_Documents_references/documents-reference-2018/pdf"))

    //  val filePath = "/Users/Philippe/Development/POC_NLP_Documents_references/documents/pdf/sg_page_8.pdf"
    //  val filePath = "/Users/Philippe/Development/POC_NLP_Documents_references/documents/pdf/ddr-2018-societe-generale-32.pdf"
    //  val filePath = "/Users/Philippe/Development/POC_NLP_Documents_references/documents/pdf/ddr-2018-societe-generale-depot-amf-d18-0112-fr.pdf"
    //  val filePath = "/Users/Philippe/Development/POC_NLP_Documents_references/documents/pdf/ddr2017_bnp_paribas_fr.pdf"
    //  val filePath = "/Users/Philippe/Development/POC_NLP_Documents_references/documents/pdf/ddr-2018-societe-generale-1-50.pdf"
    //  val filePath = "/Users/Philippe/Development/POC_NLP_Documents_references/documents/pdf/LUZEAUX_PHILIPPE_03_2019.pdf"
    //  val filePath = "/Users/Philippe/Development/POC_NLP_Documents_references/documents/pdf/ddr2017_bnp_paribas_fr.pdf"


    for (f <- files) {
      if (f.getName.toLowerCase().endsWith(".pdf")) {
        f.getParentFile.getAbsolutePath.replaceAll("pdf", "json").toFile.createIfNotExists(true, true)
        val pw = new PrintWriter(new File(f.getAbsolutePath.replaceAll("pdf", "json")))

        val company = f.getParentFile.getName
        val year = "\\d\\d\\d\\d".r.findFirstIn(f.getName).getOrElse("0")

        val stripper = new PDFTabularTextStripper

        val text = stripper.getText(f, company, year)

        pw.write(text)
        pw.close()
      }
    }

    println(((System.nanoTime() - start) / 1000000) + " milliseconds")


  }


}
