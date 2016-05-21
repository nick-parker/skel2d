package skeleton

import util.ApproxEq.approx_eq
import util.TwoD
import util.Vec

class Bisector(val v: Vec, val p: Node, val i: Corner, val j: Corner) {
  def intersect(other: Bisector) : Option[Event] = {
      val perp = this.v.left_perp
      val a2b = this.p - other.p
      val denom = other.v.dot(perp)
      
      if(approx_eq(denom, 0, 0.0001)) {
        None
      } else {
        val t1 = a2b.dot(other.v.left_perp)/denom
        val t2 = a2b.dot(perp)/denom
        if (t1 >= 0 && t2 >=0) {
          //Calculate distance to edge:
          val p = other.p+other.v*t2
          val d = Math.abs(i.next_edge.left_perp.unit.dot(p))
          Some(new Event(p, d, this.p, other.p))
        } else {
          None
        }
      }
  }
}