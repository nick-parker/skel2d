package debug

import util.Vec
import io.ReadFile
import display.{Draw, Line}
import skeleton.{Skeleton, Corner}

object Main extends App{
  override def main(args:Array[String]){
		val filename = "points.txt"
		val pnts = ReadFile.read(filename)
		println(pnts)
		val cnrs = for(p <- pnts(0)) yield new Corner(p._1, p._2)
		var sk: Skeleton = new Skeleton(cnrs)
		sk.init()
		println(sk.nodes)
		print(sk.nodes.length)
		val output = "skel.png"
		Draw.draw_lines(sk.get_display(), sk.max.x - sk.min.x, sk.max.y - sk.min.y, 50, output)
	}
}