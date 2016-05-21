package display

import util.Vec

class Line(val a: Vec, val b: Vec, val directed: Boolean, val text: String = "") {
  override def toString() : String = {
    "Line(" + a + ", " + b + ")"
  }
}