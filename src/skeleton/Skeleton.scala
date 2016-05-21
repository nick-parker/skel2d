package skeleton

import scala.collection.mutable.PriorityQueue
import display.Line
import util.Vec
import display.{Draw, fmt}

class Skeleton(var pnts: List[Corner], val show_steps : Boolean = false) {
  var min: Vec = Vec.zero
  var max: Vec = Vec.zero 
  var bis: List[Bisector] = List.empty;
  var eve: PriorityQueue[Event] = new PriorityQueue[Event]();
  var nodes: List[SkelNode] = List.empty;
  def init() {
    //Initialize bounds
	  val minX = pnts.foldLeft(Double.MaxValue)(((a:Double, c:Corner) => Math.min(a, c.x)))
    val maxX = pnts.foldLeft(Double.MinValue)(((a:Double, c:Corner) => Math.max(a, c.x)))
    val minY = pnts.foldLeft(Double.MaxValue)(((a:Double, c:Corner) => Math.min(a, c.y)))
    val maxY = pnts.foldLeft(Double.MinValue)(((a:Double, c:Corner) => Math.max(a, c.y)))
    min = new Vec(minX, minY)
	  max = new Vec(maxX, maxY)
    
    //Initialize the doubly linked list.
    val edges = (pnts.last::pnts) sliding 2
    for(a::b::_ <- edges){
      a.next = Some(b)
      b.prev = Some(a)
    }
    //Initialize Nodes
    if(!pnts.foldLeft(true)( (b,x) => b && x.init_node())){
      throw new Exception("One or more corners was missing neighbors. Could not initialize nodes.")
    }
    //Connect nodes
    if(!pnts.foldLeft(true)( (b,x) => b && x.connect_node())){
      throw new Exception("Failed to connect corner nodes.")
    }
    //Add nodes to output
    nodes = (for(p <- pnts) yield p.node) flatten
    //Initialize event list
    val pairs = ((pnts.last::pnts).flatMap(x => x.node)) sliding 2
    for(p0::p1::_ <- pairs){
      p0.bisector.intersect(p1.bisector) match {
        case Some(e) =>{
          eve.enqueue(e)
        }
        case None => 
      }
    }
    //Sort event list
    var i = 0
    while(eve.nonEmpty){
      if(show_steps){
        Draw.draw_lines(this.get_display(), max.x - min.x, max.y - min.y, 100, "Step_" + i)
      }
      i += 1
      handle_event(eve.dequeue());
    }
  }
  private def handle_peak(e: Event) : Boolean = {
    if(e.a.prev == e.b.next && e.a.prev != None){
      val peak = new Peak(e.p.x, e.p.y)
      e.a.up = Some(peak)
      e.a.marked = true
      e.b.up = Some(peak)
      e.b.marked = true
      e.a.prev match {
        case Some(x) => {
          x.up = Some(peak)
          x.marked = true
        }
        case None =>
      }
      nodes = peak::nodes
      return true
    }
    return false
  }
  
  private def handle_event(e: Event){
    if(e.a.marked || e.b.marked){
      return
    }
    //Check for triangular peaks
    if(handle_peak(e)){
      return
    }
    var ep = e.a.ep
    var en = e.b.en
    val new_node = new Node(e.p.x, e.p.y, ep, en, e.d)
    new_node.prev = e.a.prev
    e.a.prev match {
      case Some(n: Node) => n.next = Some(new_node)
      case None =>
    }
    new_node.next = e.b.next
    e.b.next match {
      case Some(n: Node) => n.prev = Some(new_node)
      case None =>
    }
    e.a.up = Some(new_node)
    e.b.up = Some(new_node)
    e.a.marked = true
    e.b.marked = true
    new_node.hit_prev() match {
      case Some(x) =>{
        eve.enqueue(x)
      }
      case None =>
    }
    new_node.hit_next() match {
      case Some(x) =>{
        eve.enqueue(x)        
      }
      case None => 
    }
    nodes = new_node::nodes
  }
  def get_poly(): List[Line] = {
    var output: List[Line] = List.empty
    for(c <- pnts){
      output = c.next match {
        case Some(n) => new Line(Vec.toVec(c), Vec.toVec(n), false)::output;
        case None => output
      }
    }
    return output
  }
  def get_display(): List[Line] = {
    var output = get_poly()
    for(n <- nodes.filter { x => x.isInstanceOf[Node] }){
      val p = n.asInstanceOf[Node]
      output = p.up match {
        case Some(o) => new Line(Vec.toVec(p), Vec.toVec(o), true, fmt.dbl format p.d)::output;
        case None => output
      }
    }
    for(e <- eve) {
      output = new Line(e.p, e.p + (e.a - e.p).unit*0.15, false, fmt.dbl format e.d)::output
      output = new Line(e.p, e.p + (e.b - e.p).unit*0.15, false, fmt.dbl format e.d)::output
    }
    return output
  }
  
  def get_bisectors(mul: Double = 1): List[Line] = {
    var output: List[Line] = List.empty
    for(n <- nodes.filter { x => x.isInstanceOf[Node] }){
      val p = n.asInstanceOf[Node]
      val b = p.bisector
//      println("p: " + b.p + " v: " + b.v)
      output = new Line(Vec.toVec(b.p), b.p + b.v*0.5*mul, true)::output 
    }
    return output
  }
}