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
    new Bisector(Edge.bisector(ep, en), this, ep, en)
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
  override def equals(o: Any): Boolean = o match {
    case c: Node => c.x == x && c.y == y
    case _ => false
  }
}

object Node{
  val debug: Node = new Node(Double.NaN, Double.NaN, Edge.debug, Edge.debug, Double.NaN) 
}