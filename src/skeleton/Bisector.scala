package skeleton

import util.ApproxEq.approx_eq
import util.TwoD
import util.Vec

class Bisector(val v: Vec, val p: Node, val i: Edge, val j: Edge) {
  def intersect(other: Bisector) : Option[Event] = {
      val perp = this.v.left_perp
      val a2b = this.p - other.p
      val denom = other.v.dot(perp)
      if(approx_eq(denom, 0, 0.0001)) {
        //Check if they're colinear
        if(approx_eq(-v.unit.dot(a2b), a2b.norm, 0.0001)){
          val pnt = (p + other.p)/2
          //Make an event at the midpoint
          //Don't worry about ordering the parents since the angle is 180.
          return Some(new Event(pnt, this.i.distance(pnt), this.p, other.p))
        }
        None
      } else {
        val t1 = a2b.dot(other.v.left_perp)/denom
        val t2 = a2b.dot(perp)/denom
        if (t1 >= 0 && t2 >=0) {
          //Calculate distance to edge:
          val pnt = other.p+other.v*t2
          //Order the parent nodes of the event based on the angle of the bisectors.
          //a should be clockwise from b around p.
          if(this.v.cross(other.v)>=0){
            Some(new Event(pnt, this.i.distance(pnt), other.p, this.p))
          } else {
            Some(new Event(pnt, this.i.distance(pnt), this.p, other.p))
          }
        } else {
          None
        }
      }
  }
}