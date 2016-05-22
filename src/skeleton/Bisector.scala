package skeleton

import util.ApproxEq.approx_eq
import util.TwoD
import util.Vec

class Bisector(val v: Vec, val p: Node, val i: Edge, val j: Edge) {
  def intersect(other: Bisector) : Option[EdgeEvent] = {
    TwoD.intersect(p, p+v, other.p, other.p + other.v, TwoD.VecVec) match {
      case Some(pnt: Vec) => if(this.v.is_cw(other.v)){
          Some(new EdgeEvent(pnt, this.i.distance(pnt), other.p, this.p))
        } else {
          Some(new EdgeEvent(pnt, this.i.distance(pnt), this.p, other.p))
        }
      case None => None
    }
  }
  def intersect(e: Edge) : Option[EdgeEvent] = {
    return None
  }
}