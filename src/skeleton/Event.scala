package skeleton

import util.Vec

class Event(val p: Vec, val d: Double, val a: Node, val b: Node) extends Ordered[Event]{
  //Ordering is backwards for priority queue. Regular sorting will be in descending order.
  def compare(other: Event): Int = other.d compare this.d
}