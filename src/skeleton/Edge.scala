package skeleton

import display.fmt
import util.{Vec, TwoD}
import scala.math.{pow, abs, sqrt}

class Edge(val a: Corner, val b: Corner) {
  def asVec(): Vec = {
    return b-a
  }
  def distance(p: TwoD): Double =  {
    abs((b.y - a.y)*p.x - (b.x - a.x)*p.y + b.x*a.y - b.y*a.x) / sqrt(pow(b.y - a.y, 2) + pow(b.x - a.x, 2))
  }
  override def toString(): String = {
    "Edge(" + a + ", " + b + ")" 
  }
  override def equals(o: Any): Boolean = o match {
    case e: Edge => e.a == a && e.b == b
    case _ => false
  }
  override def hashCode = {
    (a.x + a.y + b.x + b.y).hashCode()
  }
}


object Edge {
  val debug = new Edge(new Corner(0,0), new Corner(1,0))
  def bisector(ep: Edge, en: Edge): Vec = {
    (-ep.asVec().unit + en.asVec().unit).unit
  }
}