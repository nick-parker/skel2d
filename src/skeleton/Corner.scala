package skeleton

import util.TwoD
import util.Vec

class Corner(val x: Double, val y: Double) extends TwoD{
  var next: Option[Corner] = None
  var prev: Option[Corner] = None
  var node: Option[Node] = None
  def init_node(): Boolean = {
    (prev, next) match {
      case (Some(a), Some(b)) => {
        this.node = Some(new Node(x, y, a, b))
        return true
      }
      case _ => return false
    }
  }
  def connect_node(): Boolean = {
    if(this.node == None) return false
    val n = this.node.get
    (n.ac.node, n.bc.node) match {
      case (Some(a), Some(b)) => {
        n.prev = Some(a)
        n.next = Some(b)
        return true
      }
      case _ => return false
    }
  }
  def next_edge : Vec = {
    next match {
      case Some(b) => b-this
      case None => throw new Exception("Corner is missing a neighbor for edge computation.") 
    }
  }
  def prev_edge : Vec = {
    prev match {
      case Some(a) => this-a
      case None => throw new Exception("Corner is missing a neighbor for edge computation.") 
    }
  }
  override def toString() = {
    "Corner(" + x.toString() + ", " + y.toString() + ")"
  }
}

object Corner {
  def zero: Corner = {
    return new Corner(0,0)
  }
}