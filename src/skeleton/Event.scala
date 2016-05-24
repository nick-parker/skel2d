package skeleton

import util.Vec

abstract class Event extends Ordered[Event]{
  val d: Double
  //Ordering is backwards for priority queue. Regular sorting will be in descending order.
  def compare(other: Event): Int = other.d compare this.d
}

class EdgeEvent(val p: Vec, val d: Double, val a: Node, val b: Node) extends Event{
}

class SplitEvent(val p: Vec, val d: Double, val a: Node, val e: Edge) extends Event{
  
}