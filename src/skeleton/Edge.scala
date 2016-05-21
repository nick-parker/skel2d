package skeleton

import util.{Vec, TwoD}

class Edge(val a: Corner, val b: Corner) {
  def asVec(): Vec = {
    return b-a
  }
  def distance(p: TwoD): Double =  {
    Math.abs((b.y - a.y)*p.x - (b.x - a.x)*p.y + b.x*a.y - 
              b.y*a.x / Math.sqrt(Math.pow(b.y - a.y, 2) +
              Math.pow(b.x - a.x, 2)))
  }
}
