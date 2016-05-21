package skeleton

import util.TwoD

class Peak(val x: Double, val y: Double) extends SkelNode{
  override def toString() = {
    "Peak(" + x.toString() + ", " + y.toString() + ")"
  }
}