package debug

import java.io.File
import util.Vec
import io.ReadFile
import display.{Draw, Line}
import skeleton.{Skeleton, Corner, Edge, Node, Bisector}

object Main extends App{
	override def main(args:Array[String]){
	  for(f <- (new File(".")).listFiles){
	    if(f.toString().startsWith("./Step")){
	      f.delete()
	    }
	  }
		val filename = "points.txt"
		val pnts = ReadFile.read(filename)
		val cnrs = for(p <- pnts(0)) yield new Corner(p._1, p._2)
		var sk: Skeleton = new Skeleton(cnrs, show_steps=true)
		sk.init()
		println("Number of nodes: " + sk.nodes.length)
		val output = "skel.png"
		Draw.draw_lines(sk.get_display(), sk.max.x - sk.min.x, sk.max.y - sk.min.y, 100, output)
		Draw.draw_lines(sk.get_bisectors(), sk.max.x -  sk.min.x, sk.max.y - sk.min.y, 100, "bisectors.png")
	}
	def test_edges() {
		val c1 = new Corner(0, 0)
		val c2 = new Corner(5, 0)
		val p = new Vec(3,2)
		val e1 = new Edge(c1, c2)
		println(e1.distance(p))
	}
	def test_colinear() {
	  val n1 = new Node(0, 10, Edge.debug, Edge.debug)
	  val n2 = new Node(0, 0, Edge.debug, Edge.debug)
	  val b1 = new Bisector(new Vec(0,-1), n1, Edge.debug, Edge.debug)
	  val b2 = new Bisector(new Vec(0, 1), n2, Edge.debug, Edge.debug)
	  val e = b1.intersect(b2)
	  println(e)
	}
}