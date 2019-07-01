package parser

abstract class Block {
  var block: List[TextLine]
//  var block:  List[TextLine]

  def ::(textLine: TextLine): Block = {
    block = textLine :: block
    this
  }

  def reverse: Block = {
    block = block.reverse
    this
  }

  def merge(b: Block): Block = {
    this.block = (b.reverse.block ::: this.reverse.block).reverse
    this
  }
}
