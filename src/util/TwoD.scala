package util

import ApproxEq.approx_eq

trait TwoD {
  val x: Double
  val y: Double
  def +(other: TwoD) : Vec = {
    return new Vec(this.x + other.x, this.y + other.y)
  }
  def -(other: TwoD) : Vec = {
    return new Vec(this.x - other.x, this.y - other.y)
  }
  def cross(o: TwoD) : Double = {
    return (this.x*o.y) - (this.y*o.x);
  }
  /*
   * Return true if o is clockwise from this. 
   */
  def is_cw(o: TwoD) : Boolean = {
    return this.cross(o)>0
  }
  override def equals(o: Any): Boolean = o match {
    case c: TwoD => c.x == x && c.y == y
    case _ => false
  }
}

object TwoD {
  abstract class IntersectType
  case object LineLine extends IntersectType
  case object VecVec extends IntersectType
  case object VecLine extends IntersectType
  /*
   * Handle the different sorts of 2D intersection math. VecLine must be inputted with the vector
   * first.
   */
  def intersect(a1: TwoD, a2: TwoD, b1: TwoD, b2: TwoD, t: IntersectType ): Option[Vec] = {
    val v1 = a2 - a1
    val v2 = b2 - b1
    val perp = v1.left_perp
    val a2b = a1 - b1
    val denom = v2.dot(perp)
    if(approx_eq(denom, 0, 0.0001)) {
      //Check if they're colinear, return midpoint if they collide.
      val pnt = (a1 + b1)/2
      t match {
        case VecVec => if(approx_eq(-v1.unit.dot(a2b), a2b.norm, 0.0001)){
          return Some(pnt)
        } else return None
        case LineLine | VecLine => if(approx_eq(math.abs(v1.unit.dot(a2b)), a2b.norm, 0.0001)){
          return Some(pnt)
        } else return None
      }
      return None
    }
    val t1 = a2b.dot(v2.left_perp)/denom
    val t2 = a2b.dot(perp)/denom
    val pnt = b1+v2*t2
    t match {
      case LineLine => return Some(pnt)
      case VecVec => if(t1 >= 0 && t2 >= 0) return Some(pnt) else return None
      case VecLine => if(t1 >= 0) return Some(pnt) else return None
    }
  }
}
