package json

import play.api.libs.json.{Json, Writes}

case class JParagraph(paragraph: String) {

  implicit val jParagraphWrites = new Writes[JParagraph] {
    def writes(paragraph: JParagraph) = Json.obj(
      "paragraph" -> paragraph.paragraph
    )
  }
}
