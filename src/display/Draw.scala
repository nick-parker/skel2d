package display

import java.awt.image.BufferedImage
import java.awt.{Color, Font, BasicStroke, Graphics2D}
import java.awt.geom._
import util.Vec


object Draw {
	def example(){
		// Size of image
		val size = (500, 500);

		// create an image
		val canvas = new BufferedImage(size._1, size._2, BufferedImage.TYPE_INT_RGB)

		// get Graphics2D for the image
		val g = canvas.createGraphics()

		// clear background
		g.setColor(Color.WHITE)
		g.fillRect(0, 0, canvas.getWidth, canvas.getHeight)

		// enable anti-aliased rendering (prettier lines and circles)
		// Comment it out to see what this does!
		g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, 
				java.awt.RenderingHints.VALUE_ANTIALIAS_ON);

		// draw two filled circles
		g.setColor(Color.RED)
		g.fill(new Ellipse2D.Double(30.0, 30.0, 40.0, 40.0))
		g.fill(new Ellipse2D.Double(230.0, 380.0, 40.0, 40.0))

		// draw an unfilled circle with a pen of width 3
		g.setColor(Color.MAGENTA)
		g.setStroke(new BasicStroke(3f))
		g.draw(new Ellipse2D.Double(400.0, 35.0, 30.0, 30.0))

		// draw a filled and an unfilled Rectangle
		g.setColor(Color.CYAN)
		g.fill(new Rectangle2D.Double(20.0, 400.0, 50.0, 20.0))
		g.draw(new Rectangle2D.Double(400.0, 400.0, 50.0, 20.0))

		// draw a line
		g.setStroke(new BasicStroke())  // reset to default
		g.setColor(new Color(0, 0, 255)) // same as Color.BLUE
		g.draw(new Line2D.Double(50.0, 50.0, 250.0, 400.0))

		// draw some text
		g.setColor(new Color(0, 128, 0)) // a darker green
		g.setFont(new Font("Batang", Font.PLAIN, 20))
		g.drawString("Hello World!", 155, 225)

		// done with drawing
		g.dispose()

		// write image to a file
		javax.imageio.ImageIO.write(canvas, "png", new java.io.File("drawing.png"))
	}
	private def draw_circle(g: Graphics2D, c: Vec, r: Double){
	  g.fill(new Ellipse2D.Double(c.x-r, c.y-r, 2*r, 2*r))
	}
	def draw_lines(lines: List[Line], width: Double, height: Double, res: Double, name: String = "drawing.png"){

		val size = ((width*res).toInt, (height*res).toInt);
		val canvas = new BufferedImage(size._1, size._2, BufferedImage.TYPE_INT_RGB)
		val g = canvas.createGraphics()

		// clear background
		g.setColor(Color.WHITE)
		g.fillRect(0, 0, canvas.getWidth, canvas.getHeight)
		
		// draw a line
		g.setStroke(new BasicStroke())  // reset to default
		g.setColor(Color.BLUE) // same as Color.BLUE

		for(l <- lines){
		  val a = l.a * res
		  val b = l.b * res
		  g.draw(new Line2D.Double(a.x, a.y, b.x, b.y))
			if(l.directed){
			  val a2b = b - a
			  val r = 0.01 * res * width
			  draw_circle(g, a+a2b.unit*((a2b.norm - 2*r)), r)			  
			}
		}
		g.dispose()
		javax.imageio.ImageIO.write(canvas, "png", new java.io.File(name))
	}
}