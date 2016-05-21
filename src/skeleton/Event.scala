package skeleton

import util.Vec

class Event(val p: Vec, val d: Double, ac: Node, bc: Node) extends Ordered[Event]{
  val (a,b) = if(ac.next == Some(bc)) (ac,bc) else (bc,ac)
  //Ordering is backwards for priority queue. Regular sorting will be in descending order.
  def compare(other: Event): Int = this.d compare other.d
}