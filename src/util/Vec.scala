package util

import display.fmt

class Vec(val x: Double, val y: Double) extends TwoD{
  def *(other: Double) : Vec = {
    return new Vec(this.x*other, this.y*other)
  }
  def /(other: Double) : Vec = {
    return new Vec(this.x/other, this.y/other)
  }
  def unary_-() : Vec = {
    return new Vec(-this.x, -this.y)
  }
  def norm: Double = {
    return math.sqrt(math.pow(this.x, 2) + math.pow(this.y, 2))
  }
  def unit: Vec = {
    return new Vec(this.x/this.norm, this.y/this.norm)
  }
  def left_perp = {
    new Vec(-this.y,this.x)
  }
  def dot(other: Vec) : Double = {
    return this.x * other.x + this.y * other.y
  }
  override def toString(): String = {
    return "(" + (fmt.dbl format x) + ", " + (fmt.dbl format y) + ")" 
  }
}

object Vec {
  def zero: Vec = {
    return new Vec(0,0)
  }
  def toVec(p: TwoD): Vec = {
    return new Vec(p.x, p.y)
  }
}