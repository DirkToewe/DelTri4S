// THIS IS ANOTHER WAY OF IMPLEMENTING A TRIANGLE MESH USING HashMaps.
// THE IDEA WAS DISCARDED BECAUSE OF INFERIOR PERFORMANCE.
//
//
//package deltri
//
//import deltri.TriMesh._
//import deltri.TriMeshHashed._
//import deltri.TriMeshTaped.{AddNode, AddTri, Change}
//
//import scala.collection.mutable
//import scala.collection.mutable.ArrayBuffer
//
//class TriMeshHashed private() extends TriMesh
//{
//  private val _nodes = mutable.HashSet.empty[HashedNode]
//  private var _nTris = 0
//
//  override def nTris = _nTris
//  override def nNodes: Int = _nodes.size
//
//  def tris = new Tris(this)
//  def nodes = new Nodes(this)
//
//  def castNode( node: Node ): HashedNode = node match {
//    case node: HashedNode => node ensuring (_nodes contains node )
//  }
//
//  override def hasNode( node: Node ) = node match {
//    case node: HashedNode => _nodes contains node
//    case _ => false
//  }
//
//  override def addNode( x: Double, y: Double ): Node = {
//    val node = new HashedNode(x,y)
//    _nodes add node
//    node
//  }
//
//  override def delNode( node: Node ): Unit = {
//    val a = castNode(node)
//    for( (b,c) <- a._adj )
//    {
//      b._adj.remove(c) ensuring (_.get == a)
//      c._adj.remove(a) ensuring (_.get == b)
//      _nTris -= 1
//    }
//    a._adj.clear()
//    _nodes.remove(a)
//  }
//
//
//
//  override def addTri( a: Node, b: Node, c: Node ): Unit =
//  {
//    val _a = castNode(a)
//    val _b = castNode(b)
//    val _c = castNode(c)
//    assert( ! (_orient2d(_a,_b,_c) <= 0) )
//    assert( ! _a._adj.contains(_b) )
//    assert( ! _b._adj.contains(_c) )
//    assert( ! _c._adj.contains(_a) )
//    _a._adj(_b) = _c
//    _b._adj(_c) = _a
//    _c._adj(_a) = _b
//    _nTris += 1
//  }
//
//  override def delTri( a: Node, b: Node, c: Node ): Unit =
//  {
//    val _a = castNode(a)
//    val _b = castNode(b)
//    val _c = castNode(c)
//    assert( _a._adj(_b) == _c )
//    assert( _b._adj(_c) == _a )
//    assert( _c._adj(_a) == _b )
//    _a._adj.remove(_b)
//    _b._adj.remove(_c)
//    _c._adj.remove(_a)
//    _nTris -= 1
//  }
//
//  override def adjacent( a: Node, b: Node ): NodeMaybe[HashedNode] = {
//    val _a = castNode(a)
//    val _b = castNode(b)
//    new NodeMaybe( _a._adj.getOrElse(_b,null) )
//  }
//
//  def toHtml: String = TriMeshTaped.toHtml{
//    val changes = ArrayBuffer.empty[Change]
//    for( node <- nodes ) changes += AddNode(node)
//    tris foreach { changes += AddTri(_,_,_) }
//    changes
//  }
//}
//object TriMeshHashed
//{
//  def empty()
//    = new TriMeshHashed
//
//  def delaunay( nodes: (Double,Double)* ): (TriMeshHashed,Seq[Node]) = {
//    val mesh = new TriMeshHashed
//    mesh -> Delaunay.triangulate(mesh)(nodes:_*)
//  }
//
//  def cdt( plc: PLC, erode: Boolean = true ): TriMeshHashed
//    = ???
//
//  final class HashedNode private[TriMeshHashed](
//    override val x: Double,
//    override val y: Double
//  ) extends Node
//  {
//    private[TriMeshHashed] val _adj = mutable.HashMap.empty[HashedNode,HashedNode]
//  }
//
//  @inline class Tris( val mesh: TriMeshHashed ) extends AnyVal
//  {
//    //    @inline def foreach[U]( consumer: ((IndexedNode,IndexedNode,IndexedNode)) => U ): Unit
//    //      = foreach{ consumer apply (_,_,_) }
//    @inline def foreach[U]( consumer:  (HashedNode,HashedNode,HashedNode)  => U ): Unit =
//    {
//      var i = 0
//      for( a <- mesh._nodes )
//      for( (b,c) <- a._adj )
//        if( a < b && a < c ) {
//          consumer(a,b,c)
//          i += 1
//        }
//      assert( mesh.nTris == i )
//    }
//  }
//
//  @inline class Nodes( val mesh: TriMeshHashed ) extends AnyVal
//  {
//    @inline def foreach[U]( consumer: HashedNode => U ): Unit
//      = mesh._nodes foreach consumer
//  }
//}