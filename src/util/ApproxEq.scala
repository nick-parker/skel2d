package util

object ApproxEq {
  def approx_eq(x: Double, y: Double, precision: Double) : Boolean = {
    if ((x - y).abs < precision) true else false
  }
}