package skeleton

import util.TwoD
import display.fmt

abstract class SkelNode extends TwoD

class Node(val x: Double, val y: Double, val ep: Edge, val en: Edge, val d: Double = 0) extends SkelNode{
  var next: Option[Node] = None
  var prev: Option[Node] = None
  var up : Option[SkelNode] = None
  var marked: Boolean = false
  def bisector : Bisector = {
    new Bisector((-ep.asVec().unit + en.asVec().unit).unit, this, ep, en)
  }
  def hit_prev() = {
    this.prev match {
      case Some(a) => {
        a.bisector.intersect(this.bisector)
      }
      case None => None
    }
  }
  def hit_next() = {
    this.next match {
      case Some(b) => {
        this.bisector.intersect(b.bisector)
      }
      case None => None
    }
  }
  override def toString() = {
    "Node(" + (fmt.dbl format x)+ ", " + (fmt.dbl format y) + ")"
  }
}