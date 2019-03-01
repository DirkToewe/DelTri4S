/* This file is part of DelTri4S.
 *
 * DelTri4S is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * DelTri4S is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with DelTri4S.  If not, see <https://www.gnu.org/licenses/>.
 */

package deltri

import deltri.TriMesh.{Node, _inCircle, _orient2d, _triContainsNode}

import scala.Double.NaN

/** (2-dimensional, unconstrained) Delauny Triangulation methods. Implementation based on
  * <a href="https://people.eecs.berkeley.edu/~jrs/meshpapers/delnotes.pdf">J.R. Shewchuk's lecture notes</a>.
  */
object Delaunay
{
  def triangulate( mesh: TriMesh )( nodes: (Double,Double)* ): Array[Node]
    = triangulate(
        mesh,
        nodes.view.map(_._1).toArray,
        nodes.view.map(_._2).toArray
      )

  def triangulate( mesh: TriMesh, x: Array[Double], y: Array[Double] ): Array[Node]
    = triangulate( InsertOrder hierarchicalZOrder (_,_) )(mesh,x,y)

  def triangulate( insertionOrder: (Array[Double],Array[Double]) => Array[Int] )( mesh: TriMesh, x: Array[Double], y: Array[Double] ): Array[Node] =
  {
    assert( x.length == y.length )
    val len = x.length

    // insertion order
    val order = insertionOrder(x,y)
    assert(order.length == len)
    // creation order has to match the arguments,
    // so that for each Node n: n == Node( x(n.index), y(n.index) )
    val nodes = Array.tabulate[Node](len){ i => mesh.addNode(x(i), y(i)) }

    if( len < 3 ) return nodes

    // thanks to the virtual point (VP) we can pretend that the entire 2D-plane is covered by
    // the triangle mesh, such that findTri will always find an enclosing triange for a point
    // once the VP is removed, a convex hull remains
    val VP = mesh.addNode(NaN,NaN)

    // these variables are used to keep track of the las triangle added to the mesh
    // said triangle can be used as a starting point for finding the next triangle
    var l0 = nodes( order(0) )
    var l1 = nodes( order(1) )
    var l2 = nodes( order(2) )
    // make sure the first triangle is positively oriented
    if( _orient2d(l0,l1,l2) < 0 ) {
      val tmp = l0; l0 = l2; l2 = tmp
    }

    def digCavity( u: Node, v: Node, w: Node ): Unit =
    {
      val x = mesh.adjacent(w,v).nodeOrNull ensuring (_ != null)
      if( _inCircle(u,v,w)(x) ) {
        mesh.delTri(w, v, x)
        digCavity(u, v, x)
        digCavity(u, x, w)
      } else {
        mesh.addTri(u, v, w)
        // memorize a non-virtual triangle for the next call to findTri()
        assert( ! u.hasNaN )
        if( ! v.hasNaN && ! w.hasNaN ) {
          l0 = u
          l1 = v
          l2 = w
        }
      }
    }

    // add the first triangle and 3 virtual triangles that entirely sorround it
    mesh.addTri(l0,l1,l2)
    mesh.addTri(l0,l2,VP)
    mesh.addTri(l1,l0,VP)
    mesh.addTri(l2,l1,VP)

    var i = 3
    while( i < len ) {
      val u = nodes{ order(i) }
      // Finds the triangle that encloses given node. Said result triangle is written to l0,l1,l2.
      //
      // The triangle is searched by walking along the triangle mesh starting at the last
      // triangle added (which is written to l0,l1,l2 by digCavity()). The search will
      // always succeed because the entire 2D-plane is covered by triangles thanks to VP.
      //
      // Provided the points are added in a somewhat space-filling order, this search is highly efficient.
      if( _orient2d(l1,l0,u) > 0 ) { l2 = l0; l0 = l1; l1 = l2 }
      var continue = true
      while(continue) {
        l2 = mesh.adjacent(l0,l1).nodeOrNull ensuring (_ != null)
             if( ! mesh.adjacent(l0,l2).nodeOrNull.hasNaN && // <- walk along non-virtual triangles as long as possible
                 _orient2d(l0,l2,u) > 0 ) l1 = l2
        else if( _orient2d(l2,l1,u) > 0 ) l0 = l2
        else if( _orient2d(l0,l2,u) > 0 ) l1 = l2
        else continue = false
      }
      assert( _triContainsNode(l0,l1,l2)(u) )
      // cache the found triangle since digCavity overwrites l0,l1,l2
      val a = l0
      val b = l1
      val c = l2
      mesh.delTri(a,b,c)
      digCavity(u, a,b)
      digCavity(u, b,c)
      digCavity(u, c,a)
      i += 1
    }

    // delete the virtual points which also removes the virtual triangles
    mesh.delNode(VP)
    nodes
  }

//  def triangulate( mesh: TriMesh, x: Array[Double], y: Array[Double] ): Array[Node] =
//  {
//    assert( x.length == y.length )
//
//    // insertion order
//    val order = InsertionOrder.hierarchicalZOrder(x,y)
//    assert( order.length == x.length )
//    // creation order has to match the arguments,
//    // so that for each Node n: n == Node( x(n.index), y(n.index) )
//    val nodes = Array.tabulate(x.length)( i => mesh.addNode( x(i), y(i) ) )
//    // bring nodes in insertion order
//    val nds = order map nodes
//
//    // thanks to the virtual point (VP) we can pretend that the entire 2D-plane is covered by
//    // the triangle mesh, such that findTri will always find an enclosing triange for a point
//    // once the VP is removed, a convex hull remains
//    val VP = mesh.addNode(NaN,NaN)
//
//    val len = nds.length
//    if( len < 3 ) return nodes
//
//    // these variables are used to keep track of the las triangle added to the mesh
//    // said triangle can be used as a starting point for finding the next triangle
//    var l0 = nds(0)
//    var l1 = nds(1)
//    var l2 = nds(2)
//    // make sure the first triangle is positively oriented
//    if( _orient2d(l0,l1,l2) < 0 ) {
//      val tmp = l0; l0 = l2; l2 = tmp
//    }
//
//    /** Finds the triangle that encloses given node. Said result triangle is written to l0,l1,l2.
//      *
//      * The triangle is searched by walking along the triangle mesh starting at the last
//      * triangle added (which is written to l0,l1,l2 by digCavity()). The search will
//      * always succeed because the entire 2D-plane is covered by triangles thanks to VP.
//      *
//      * Provided the points are added in a somewhat space-filling order, this search is highly efficient.
//      *
//      * @param u The point for which an enclosing triangle is searched.
//      */
//    @inline def findTri( u: Node ): Unit = {
//      @inline def adj()= mesh.adjacent(l0,l1).nodeOrNull
//      if( _orient2d(l1,l0,u) > 0 ) { l2 = l0; l0 = l1; l1 = l2; l2 = adj() }
//      @tailrec def find(): Unit
//        =    if( _orient2d(l0,l2,u) > 0 ) { l1 = l2; l2 = adj(); find() }
//        else if( _orient2d(l2,l1,u) > 0 ) { l0 = l2; l2 = adj(); find() }
//      find()
//      assert( _triContainsNode(l0,l1,l2)(u) )
//    }
//
//    def digCavity( u: Node, v: Node, w: Node ): Unit
//      = for( x <- mesh.adjacent(w,v) )
//          if( _inCircle(u,v,w)(x) ) {
//            mesh.delTri(w, v, x)
//            digCavity(u, v, x)
//            digCavity(u, x, w)
//          } else {
//            try {
//              mesh.addTri(u, v, w)
//            } catch {
//              case err =>
//                println( f"Error for digCavity(\n  $u\n  $v\n  $w\n  $x\n)" )
//                throw err
//            }
//            l0 = u
//            l1 = v
//            l2 = w
//          }
//
//    // add the first triangle and 3 virtual triangles that entirely sorround it
//    mesh.addTri(l0,l1,l2)
//    mesh.addTri(l0,l2,VP)
//    mesh.addTri(l1,l0,VP)
//    mesh.addTri(l2,l1,VP)
//
//    var i = 3
//    while( i < len ) {
//      val u = nds(i)
//      findTri(u)
//      // cache the found triangle since digCavity overwrites l0,l1,l2
//      val a = l0
//      val b = l1
//      val c = l2
//      mesh.delTri(a,b,c)
//      digCavity(u, a,b)
//      digCavity(u, b,c)
//      digCavity(u, c,a)
//      i += 1
//    }
//
//    // delete the virtual points which also removes the virtual triangles
//    mesh.delNode(VP)
//    nodes
//  }
}
