package skeleton

import util.TwoD
import util.Vec

class Corner(val x: Double, val y: Double, var sk: Skeleton = Skeleton.debug) extends TwoD{
  var next: Option[Corner] = None
  var prev: Option[Corner] = None
  var node: Option[Node] = None
  def init_node(): Boolean = {
    (prev, next) match {
      case (Some(a), Some(b)) => {
        this.node = Some(new Node(x, y, sk.get_edge(a, this), sk.get_edge(this, b)))
        return true
      }
      case _ => return false
    }
  }
  def connect_node(): Boolean = {
    if(this.node == None) return false
    val n = this.node.get
    (n.ep.a.node, n.en.b.node) match {
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
  override def equals(o: Any): Boolean = o match {
    case c: Corner => c.x == x && c.y == y
    case _ => false
  }
}

object Corner {
  def zero: Corner = {
    return new Corner(0,0)
  }
}