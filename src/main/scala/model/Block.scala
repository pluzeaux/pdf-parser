package model

abstract class Block { // extends Ordered[Block] {
//  val x = block.head.line.head.getX
//  val y = block.head.line.head.getY
  var block: List[TextLine]

  def ::(textLine: TextLine): Block = {
    block = textLine :: block
    this
  }

  def merge(b: Block): Block = {
    this.block = (b.reverse.block ::: this.reverse.block).reverse
    this
  }

  def reverse: Block = {
    block = block.reverse
    this
  }

//  def compare(that: Block) = {
//    if (this.x == that.x)
//      0
//    else if (this.x > that.x)
//      1
//    else
//      -1
//  }
}
