package deltri

import TriMesh._
import spire.math.{Algebraic, FpFilter, Rational}
// import org.apache.commons.math3.util.{ FastMath => Math }

/** A 2D triangle mesh representation that is focused on fast traversal and modification.
  * The triangle mesh must be positively oriented and overlap-free.
  */
trait TriMesh
{
  type NodeType <: Node

  /** Returns the number of nodes in this mesh.
    * @return The number of nodes in this mesh.
    */
  def nNodes: Int

  /** Returns the number of triangles in this mesh.
    * @return The number of triangles in this mesh.
    */
  def nTris: Int

  def nSegments: Int

  def foreachNode         [U]              ( consumer:  this.NodeType                              => U ): Unit
  def foreachTriAround    [U]( node: Node )( consumer: (this.NodeType,this.NodeType)               => U ): Unit
  def foreachTri          [U]              ( consumer: (this.NodeType,this.NodeType,this.NodeType) => U ): Unit
  def foreachSegment      [U]              ( consumer: (this.NodeType,this.NodeType)               => U ): Unit
  def foreachSegmentAround[U]( node: Node )( consumer:  this.NodeType                              => U ): Unit

  @inline final def nodes = new Nodes[this.type](this)
  @inline final def tris = new Tris[this.type](this)

  /** Returns all edge strips that have have a triangle to their right side and
    * none to their left in a seq of node-seqs. Each of the node-seqs is closed,
    * i.e. from the last entry to the first entry there is another edge.
    *
    * @return
    */
  def boundaries: Seq[Seq[this.NodeType]]

  def hasEdge( a: Node, b: Node ): Boolean

  /** Adds a new Node with the given coordinates to this triangle mesh.
    *
    * @param x The x-coordinate of the newly added point.
    * @param y The y-coordinate of the newly added point.
    * @return The reference to the newly created point.
    */
  def addNode( x: Double, y: Double ): this.NodeType

  /** Deletes a nodes from this triangle mesh. The x and y coordinate of the Node
    * reference can still be queried after the node has been deleted.
    *
    * @param node The reference to the node that is to be removed.
    */
  def delNode( node: Node ): Unit

  /** Checks if this triangle mesh contains a node given by reference.
    * This method checks for reference equality not coordinate equality.
    *
    * @param node The node that is to be checked for.
    * @return True if an only if this triangle mesh contains the given node.
    */
  def hasNode( node: Node ): Boolean

  /** Adds a new triangle to this mesh with the given vertices.
    *
    * @param a The 1st vertex of the newly created triangle.
    * @param b The 2nd vertex of the newly created triangle.
    * @param c The 3rd vertex of the newly created triangle.
    */
  def addTri( a: Node, b: Node, c: Node ): Unit

  /** Deletes the triangle with the given vertices. This method checks for
    * reference equality not coordinate equality.
    *
    * @param a The 1st vertex of the triangle to be deleted.
    * @param b The 2nd vertex of the triangle to be deleted.
    * @param c The 3rd vertex of the triangle to be deleted.
    */
  def delTri( a: Node, b: Node, c: Node ): Unit

  /** Checks if this mesh contains a triangle with the given vertices.
    * This method checks for reference equality not coordinate equality.
    *
    * @param a The 1st vertex of the triangle to be checked.
    * @param b The 2nd vertex of the triangle to be checked.
    * @param c The 3rd vertex of the triangle to be checked.
    */
  def hasTri( a: Node, b: Node, c: Node ): Boolean
    = adjacent(a,b).nodeOrNull == c

  def addSegment( a: Node, b: Node ): Unit
  def delSegment( a: Node, b: Node ): Unit
  def hasSegment( a: Node, b: Node ): Boolean

  /** For a given pair of nodes (a,b), this method returns a node c
    * if (a,b,c) is a triangle in this mesh. Otherwise null is returned.
    * The result is wrapped in the value type "NodeMaybe" so that it
    * can be conveniently used in a foreach statement.
    *
    * @param a The 1st node of a (potential) triangle.
    * @param b The 2nd node of a (potential) triangle.
    * @return The 2rd node of a triangle or null, wrapped in a NodeMaybe.
    */
  def adjacent( a: Node, b: Node ): NodeMaybe[this.NodeType]

  def toHtml: String
}
object TriMesh
{
  class NodeMaybe[+N <: Node]( val nodeOrNull: N ) extends AnyVal {
    def exists = nodeOrNull != null

    def foreach[U]( consumer: N => U ): Unit
      = if(exists) consumer(nodeOrNull)

    def mapOrElse[T]( mapper: N => T, otherwise: => T ): T
      = if(exists) mapper(nodeOrNull) else otherwise
  }

  trait Node {
    def x: Double
    def y: Double

    def hasNaN
      = x.isNaN ||
        y.isNaN

    private[deltri] def < ( n: Node ) = {
      val ax = x.abs; val bx = n.x.abs
      val ay = y.abs; val by = n.y.abs
      ax < bx || (ax == bx && ay < by)
    }

    private[TriMesh] def =/=(n: Node )
      = ! (n.x == x) ||
        ! (n.y == y)

    override def toString = f"Node($x%g, $y%g)"
  }
  object Node {
    class NodeExtractor( val node: Node ) extends AnyVal
    {
      def isEmpty = null == node
      def get = this
      def _1 = node.x
      def _2 = node.y
    }

    def unapply( node: Node )
      = new NodeExtractor(node)
  }

  @inline class Tris[Mesh <: TriMesh]( val mesh: Mesh ) extends AnyVal
  {
    @inline def foreach[U]( consumer: (mesh.NodeType,mesh.NodeType,mesh.NodeType) => U ): Unit
      = mesh foreachTri consumer

    def toArray: Array[(mesh.NodeType,mesh.NodeType,mesh.NodeType)] =
    {
      val tris = new Array[(mesh.NodeType,mesh.NodeType,mesh.NodeType)](mesh.nTris)
      var i = 0
      foreach{
        (a,b,c) =>
          tris(i) = (a,b,c)
          i += 1
      }
      assert( tris.length == i )
      tris
    }
  }

  @inline class Nodes[Mesh <: TriMesh]( val mesh: Mesh ) extends AnyVal
  {
    @inline def foreach[U]( consumer: mesh.NodeType => U ): Unit
      = mesh foreachNode consumer

    def toArray: Array[Node] =
    {
      val tris = new Array[Node](mesh.nNodes)
      var i = 0
      for( n <- this ) {
        tris(i) = n
        i += 1
      }
      assert( tris.length == i )
      tris
    }
  }

  /** Returns the orientation of a triangle.
    *
    * @param a
    * @param b
    * @param c
    * @return
    */
  def _orient2d( a: Node, b: Node, c: Node ): Double = {
    assert( a =/= b, f"\n  $a\n  $b\n  $c" )
    assert( a =/= c, f"\n  $a\n  $b\n  $c" )
    assert( b =/= c, f"\n  $a\n  $b\n  $c" )

    if( a.hasNaN ) return Double.NaN
    if( b.hasNaN ) return Double.NaN
    if( c.hasNaN ) return Double.NaN

    val ax = FpFilter.exact[Rational](a.x)
    val ay = FpFilter.exact[Rational](a.y)
    val bx = FpFilter.exact[Rational](b.x)
    val by = FpFilter.exact[Rational](b.y)
    val cx = FpFilter.exact[Rational](c.x)
    val cy = FpFilter.exact[Rational](c.y)

//    val ux = ax - cx; val vx = bx - cx
//    val uy = ay - cy; val vy = by - cy
//    (ux*vy - uy*vx).signum
    { (ax-cx)*(by-cy) - (ay-cy)*(bx-cx) }.signum.toDouble
//    { ax*(by-cy) - ay*(bx-cx) - cx*by + cy*bx }.signum
  }

  /** Returns true if a triangle (a,b,c) contains the node u.
    *
    * @param a
    * @param b
    * @param c
    * @param u
    * @return
    */
  def _triContainsNode( a: Node, b: Node, c: Node )( u: Node )
    = ! (_orient2d(a,b,u) < 0) &&
      ! (_orient2d(b,c,u) < 0) &&
      ! (_orient2d(c,a,u) < 0)

  /** Returns true if a node u is in the circumcircle of a triangle (a,b,c).
    * On of the triangle nodes may have NaN values in which case the triangle
    * is assumed to have an infinite circumradius.
    *
    * @param a
    * @param b
    * @param c
    * @param u
    * @return
    */
  def _inCircle( a: Node, b: Node, c: Node )( u: Node ): Boolean =
  {
    assert( a =/= b, f"\n  $a\n  $b\n  $c" )
    assert( a =/= c, f"\n  $a\n  $b\n  $c" )
    assert( b =/= c, f"\n  $a\n  $b\n  $c" )
    assert( ! a.hasNaN )
    assert( ! (_orient2d(a,b,c) < 0), f"Bad orientation (${_orient2d(a,b,c)}) for triangle:\n  $a\n  $b\n  $c" )

    if( u.hasNaN ) return 0 == _orient2d(a,b,c) ensuring ! c.hasNaN && ! b.hasNaN
    if( b.hasNaN ) return 0 <  _orient2d(c,a,u) ensuring ! c.hasNaN
    if( c.hasNaN ) return 0 <  _orient2d(a,b,u) ensuring ! b.hasNaN

    val ax = FpFilter.exact[Rational](a.x)
    val ay = FpFilter.exact[Rational](a.y)
    val bx = FpFilter.exact[Rational](b.x)
    val by = FpFilter.exact[Rational](b.y)
    val cx = FpFilter.exact[Rational](c.x)
    val cy = FpFilter.exact[Rational](c.y)
    val ux = FpFilter.exact[Rational](u.x)
    val uy = FpFilter.exact[Rational](u.y)

//    val ab = ax*by - ay*bx
//    val ac = ax*cy - ay*cx; val bc = bx*cy - by*cx
//    val au = ax*uy - ay*ux; val bu = bx*uy - by*ux; val cu = cx*uy - cy*ux
//    (( (ax*ax + ay*ay) * ( bc - bu + cu)   +
//       (bx*bx + by*by) * (-ac + au - cu) ) +
//     ( (cx*cx + cy*cy) * ( ab - au + bu)   +
//       (ux*ux + uy*uy) * (-ab + ac - bc) )).signum > 0

    val ab = ax*by - ay*bx
    val cu = cx*uy - cy*ux

    val cux = cx - ux
    val cuy = cy - uy
    val abx = ax - bx
    val aby = ay - by

    val auc = ay*cux - ax*cuy
    val bcu = bx*cuy - by*cux
    val uab = ux*aby - uy*abx
    val cba = cy*abx - cx*aby
    (( (ax*ax + ay*ay) * (bcu + cu)   +
       (bx*bx + by*by) * (auc - cu) ) +
     ( (cx*cx + cy*cy) * (uab + ab)   +
       (ux*ux + uy*uy) * (cba - ab) )).signum > 0
  }

//    assert( a =/= b, f"\n  $a\n  $b\n  $c" )
//    assert( a =/= c, f"\n  $a\n  $b\n  $c" )
//    assert( b =/= c, f"\n  $a\n  $b\n  $c" )
//    assert( ! a.hasNaN )
//    assert( ! (_orient2d(a,b,c) < 0), f"Bad orientation (${_orient2d(a,b,c)}) for triangle:\n  $a\n  $b\n  $c" )
//
//    if( _orient2d(a,b,u) <= 0 ) return false
//    if( _orient2d(c,a,u) <= 0 ) return false
//    if( _orient2d(a,b,c) == 0 ) return true ensuring distSqr(a,b) <= distSqr(b,c)
//    if( b.hasNaN ) return 0 < _orient2d(c,a,u) ensuring c != null
//    if( c.hasNaN ) return 0 < _orient2d(a,b,u) ensuring b != null
//
//    @inline def cross( l: Node, r: Node ) = l.x*r.y - l.y*r.x
//    @inline def sqr( n: Node ) = n.x*n.x + n.y*n.y
//
//    def inCircle( a: Node, b: Node, c: Node ): Double = {
//      assert( ! a.hasNaN )
//      assert( ! b.hasNaN )
//      assert( ! c.hasNaN )
//      val ab = cross(a,b)
//      val ac = cross(a,c); val bc = cross(b,c)
//      val au = cross(a,u); val bu = cross(b,u); val cu = cross(c,u)
//      ( sqr(a) * ( bc - bu + cu)   +
//        sqr(b) * (-ac + au - cu) ) +
//      ( sqr(c) * ( ab - au + bu)   +
//        sqr(u) * (-ab + ac - bc) )
//    }
//
//    // make the orientation invariant against cyclic commutations
//    val result = if( a < b && a < c ) inCircle(a,b,c)
//            else if( b < a && b < c ) inCircle(b,c,a)
//            else                      inCircle(c,a,b)
//    0 < result

  /** Returns the area of a triangle.
    *
    * @param a
    * @param b
    * @param c
    * @return
    */
  def area( a: Node, b: Node, c: Node ): Double =
  {
    val ax = a.x - c.x; val bx = b.x - c.x
    val ay = a.y - c.y; val by = b.y - c.y
    (ax*by - ay*bx) / 2
  }

  /** Returns the angle at point a for a triangle (a,b,c).
    *
    * @param a
    * @param b
    * @param c
    * @return
    */
  def angle( a: Node, b: Node, c: Node ): Double =
  {
    val bx = b.x - a.x; val cx = c.x - a.x
    val by = b.y - a.y; val cy = c.y - a.y
    val angle = Math acos (
      (bx*cx + by*cy)
      / Math.hypot(bx,by)
      / Math.hypot(cx,cy)
      max -1 min +1
    )
    if( 0 != angle ) angle else {
      val bx = Algebraic(b.x) - a.x
      val by = Algebraic(b.y) - a.y
      val cx = Algebraic(c.x) - a.x
      val cy = Algebraic(c.y) - a.y
      // acos(x) ≈ π/2 - x
      Math acos ( {bx*cx + by*cy} / { (bx*bx + by*by)*(cx*cx + cy*cy) }.sqrt ).toDouble
    }
  }

  def distance( a: Node, b: Node ): Double
    = Math hypot (
        a.x - b.x,
        a.y - b.y
      )

  def distSqr( a: Node, b: Node ): Double = {
    val dx = a.x - b.x
    val dy = a.y - b.y
    dx*dx + dy*dy
  }
}
