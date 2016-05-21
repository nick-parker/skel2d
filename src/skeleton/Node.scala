package skeleton

import util.TwoD

abstract class SkelNode extends TwoD

class Node(val x: Double, val y: Double, val ac: Corner, val bc: Corner) extends SkelNode{
  var next: Option[Node] = None
  var prev: Option[Node] = None
  var up : Option[SkelNode] = None
  var marked: Boolean = false
  def bisector : Bisector = {
    (this.prev, this.next) match {
      case (Some(a), Some(b)) => {
        new Bisector(((b-this).unit + (a-this).unit).unit, this, ac, bc)
      }
      case _ => throw new Exception("Corner is missing a neighbor for bisector computation.")
    }
  }
  def hit_prev() = {
    this.prev match {
      case Some(a) => a.bisector.intersect(this.bisector)
      case None => None
    }
  }
  def hit_next() = {
    this.next match {
      case Some(b) => this.bisector.intersect(b.bisector)
      case None => None
    }
  }
  override def toString() = {
    "Node(" + x.toString() + ", " + y.toString() + ")"
  }
}