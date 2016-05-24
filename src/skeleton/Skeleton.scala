package skeleton

import scala.collection.mutable.PriorityQueue
import display.Line
import util.{Vec, TwoD}
import display.{Draw, fmt}
import scala.collection.mutable.HashSet

class Skeleton(pnts_in: List[List[(Double, Double)]], val show_steps : Boolean = false) {
  var pnts: List[Corner] = 
    if(pnts_in.length > 0) for(p <- pnts_in(0)) yield new Corner(p._1, p._2, this) else List.empty
  var min: Vec = Vec.zero
  var max: Vec = Vec.zero 
  var eve: PriorityQueue[Event] = new PriorityQueue[Event]();
  var nodes: List[SkelNode] = List.empty;
  var edges = new HashSet[Edge]
  def get_edge(a: Corner, b: Corner): Edge = {
    val e = new Edge(a, b)
    if(edges.contains(e)){
      return edges.find(e2 => e2 == e).get
    } else {
      edges.add(e)
      return e
    }
  }
  private def active_nodes() = {
    nodes.filter(x => x.isInstanceOf[Node]).map{x => x.asInstanceOf[Node]}
  }
  /*
   * Generate initial nodes from the input points and calculate the straight skeleton of the
   * polygon.
   */
  def init() {
    init_bounds()
    
    //Convert the explicit list input to an implicit doubly linked list.
    val edges = (pnts.last::pnts) sliding 2
    for(a::b::_ <- edges){
      a.next = Some(b)
      b.prev = Some(a)
    }
    //Initialize Nodes. This fold is equivalent to "Call init_node() on all points, and return
    //whether all of them returned true."
    if(!pnts.foldLeft(true)( (b,x) => b && x.init_node())){
      throw new Exception("One or more corners was missing neighbors. Could not initialize nodes.")
    }
    //Connect nodes with a similar fold check. This has to be a separate step since it needs all
    //the neighboring nodes initialized.
    if(!pnts.foldLeft(true)( (b,x) => b && x.connect_node())){
      throw new Exception("Failed to connect corner nodes.")
    }
    //Add the nodes representing each corner of the input polygon to the output.
    nodes = (for(p <- pnts) yield p.node) flatten;
    //Initialize the event queue with all potential edge events.
    for(n <- active_nodes()){
    	n.hit_next() match {
    	  case Some(x) => eve.enqueue(x)
    	  case None =>
    	}
    }
    //Add all potential split events to the event queue.
    for(n <- active_nodes()){
      
    }
    //While there are still events in the queue, dequeue them and either add them to the skeleton
    //or throw them away if a previously handled event has taken one of their parent nodes.
    var i = 0
    while(eve.nonEmpty){
      if(show_steps){
        Draw.draw_lines(this.get_display(), max.x - min.x, max.y - min.y, 100, "Step_" + i)
      }
      i += 1
      handle_event(eve.dequeue());
    }
  }
  private def init_bounds() {
    val minX = pnts.foldLeft(Double.MaxValue)(((a:Double, c:Corner) => Math.min(a, c.x)))
    val maxX = pnts.foldLeft(Double.MinValue)(((a:Double, c:Corner) => Math.max(a, c.x)))
    val minY = pnts.foldLeft(Double.MaxValue)(((a:Double, c:Corner) => Math.min(a, c.y)))
    val maxY = pnts.foldLeft(Double.MinValue)(((a:Double, c:Corner) => Math.max(a, c.y)))
    min = new Vec(minX, minY)
	  max = new Vec(maxX, maxY)
  }
  private def handle_peak(e: EdgeEvent) : Boolean = {
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
  private def intersect_reflex(n: Node){
    for(start <- active_nodes()){
      
    }
  }
  
  private def intersect_node(n: Node){
    //Intersect node's bisector with adjacent bisectors.
    n.hit_prev() match {
      case Some(x) =>{
        eve.enqueue(x)
      }
      case None =>
    }
    n.hit_next() match {
      case Some(x) =>{
        eve.enqueue(x)        
      }
      case None => 
    }
    //If the node isn't reflex, end here.
    if(!n.ep.asVec().is_cw(n.en.asVec())){
      //If the turn is clockwise it's reflex.
      return
    }
    val b = n.bisector
    for(e: Edge <- edges){
      TwoD.intersect(n, n+b.v, e.a, e.b, TwoD.VecLine) match {
        case Some(pnt) =>{
          val prev_bound = e.a.node.get.bisector.v.is_cw(pnt-e.a)
          val next_bound = !e.b.node.get.bisector.v.is_cw(pnt-e.b)
          if(prev_bound && next_bound){
            //Compute the actual location by intersecting node bisector with the
            //bisector of e and one of the node's adjacent edges.
            val other = Edge.bisector(n.en, e)
            val other_origin = TwoD.intersect(n.en.a, n.en.b, e.a, e.b, TwoD.LineLine).get
            val pos = TwoD.intersect(other_origin, other_origin+other, n, n+b.v, TwoD.LineLine).get
            eve.enqueue(new SplitEvent(pos, e.distance(pnt), n, e))
          }
        }
        case None => 
      }
    }
  }
  private def make_node(e: EdgeEvent): Node = {
    //Create the node and set its adjacent edges.
    var ep = e.a.ep
    var en = e.b.en
    val new_node = new Node(e.p.x, e.p.y, ep, en, e.d)
    //Set the new node's neighbor behind it in the node loop. This neighbor might be None.
    new_node.prev = e.a.prev
    //If the neighbor actually exists, update its next reference to point at the new node.
    e.a.prev match {
      case Some(n: Node) => n.next = Some(new_node)
      case None =>
    }
    //Do the same for the neighbor in the other direction.
    new_node.next = e.b.next
    e.b.next match {
      case Some(n: Node) => n.prev = Some(new_node)
      case None =>
    }
    //Point the parents of the new node at it and mark them as used.
    e.a.up = Some(new_node)
    e.b.up = Some(new_node)
    e.a.marked = true
    e.b.marked = true
    return new_node
  }
  private def handle_edge_event(e: EdgeEvent) {
    //Check that neither parent has been taken by a previously handled event.
    if(e.a.marked || e.b.marked){
      return
    }
    //Check for triangular peaks where three edges converge to a point.
    if(handle_peak(e)){
      return
    }
    val new_node = make_node(e)
    //See if the new node's bisector makes any new events with its neighbors' bisectors.
    intersect_node(new_node)
    nodes = new_node::nodes
  }
  
  private def handle_split_event(e: SplitEvent) {
  }
  
  private def handle_event(in: Event){
    in match {
      case e: EdgeEvent => handle_edge_event(e)
      case e: SplitEvent => handle_split_event(e)
        
      }
    }
  
  /*
   * Get the input polygon in a displayable format.
   */
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
  /*
   * Get the input polygon and any skeleton lines which have been created so far in a displayable
   * format.
   */
  def get_display(): List[Line] = {
    var output = get_poly()
    for(n <- active_nodes()){
      output = n.up match {
        case Some(o) => new Line(Vec.toVec(n), Vec.toVec(o), true, fmt.dbl format n.d)::output;
        case None => output
      }
    }
    for(e <- eve) {
      e match {
        case e: EdgeEvent => {
          output = new Line(e.p, e.p + (e.a - e.p).unit*0.15, false, fmt.dbl format e.d)::output
          output = new Line(e.p, e.p + (e.b - e.p).unit*0.15, false, fmt.dbl format e.d)::output
        }
        case e: SplitEvent => {
          
        }
      }
    }
    return output
  }
  /*
   * Get the bisectors of all nodes in a displayable format.
   */
  def get_bisectors(mul: Double = 1): List[Line] = {
    var output: List[Line] = List.empty
    for(n <- active_nodes()){
      val b = n.bisector
//      println("p: " + b.p + " v: " + b.v)
      output = new Line(Vec.toVec(b.p), b.p + b.v*0.5*mul, true)::output 
    }
    return output
  }
}

object Skeleton {
  val debug = new Skeleton(List.empty)
}