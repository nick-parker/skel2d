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
}


object Edge {
  val debug = new Edge(new Corner(0,0), new Corner(1,0))
}