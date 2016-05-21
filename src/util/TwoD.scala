package util

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
}