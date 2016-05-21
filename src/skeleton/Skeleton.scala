package skeleton

import scala.collection.mutable.PriorityQueue
import display.Line
import util.Vec

class Skeleton(pnts: List[Corner]) {
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
        case Some(e) => eve.enqueue(e)
        case None => 
      }
    }
    //Sort event list
    while(eve.nonEmpty){
      handle_event(eve.dequeue());
    }
  }
  def handle_event(e: Event){
    if(e.a.marked || e.b.marked){
      return
    }
    //Check for triangular peaks
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
      return
    }
    val new_node = new Node(e.p.x, e.p.y, e.a.ac, e.b.bc)
    new_node.prev = e.a.prev
    new_node.next = e.b.next
    e.a.up = Some(new_node)
    e.b.up = Some(new_node)
    e.a.marked = true
    e.b.marked = true
    new_node.hit_prev() match {
      case Some(x) => eve.enqueue(x)
      case None =>
    }
    new_node.hit_next() match {
      case Some(x) => eve.enqueue(x)
      case None => 
    }
    nodes = new_node::nodes
  }
  def get_display(): List[Line] = {
    var output: List[Line] = List.empty
    for(c <- pnts){
      output = c.next match {
        case Some(n) => new Line(Vec.toVec(c), Vec.toVec(n), false)::output;
        case None => output
      }
    }
    for(n <- nodes.filter { x => x.isInstanceOf[Node] }){
      val p = n.asInstanceOf[Node]
      output = p.up match {
        case Some(o) => new Line(Vec.toVec(p), Vec.toVec(o), true)::output;
        case None => output
      }
    }
    return output
  }
}