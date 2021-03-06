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

import deltri.TriMesh.{Node, _inCircle, _orient2d}

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

/** (2-dimensional) Constrained Delauny Triangulation (CDT) methods. Implementation based on
  * <a href="https://people.eecs.berkeley.edu/~jrs/meshpapers/delnotes.pdf">J.R. Shewchuk's lecture notes</a>.
  *
  * The algorithms is roughly structured as follows:
  * <ul>
  *   <li>Perform the unconstrained delaunay triangular triangulation.
  *   <li>For each segment:
  *     <ul>
  *       <li>Cut open a whole for that segment
  *       <li>Insert the segment
  *       <li>Retriangulate to both sides of the insertes segment.
  *     </ul>
  *   <li>For each hole node `n`:
  *   <li>Remove all triangles reachable from `n` without crossing a segment.
  *   <li>...
  * </ul>
  */
object CDT
{
  /** Inserts, into the given mesh, a constrained delaunay triangulation (CDT) of the given
    * piecewise linear complex (PLC).
    *
    * @param mesh The mesh object to which the CDT is added.
    * @param plc  The piecewise linear complex that is to be triangulated.
    * @return The nodes of the CDT in the exact order they appeared in `plc.nodes`.
    */
  def triangulate( mesh: TriMesh, plc: PLC ): Array[Node] = {
    val nodes = Delaunay.triangulate(mesh)(plc.nodes:_*)
//    assert( mesh.boundaries.length == 1 )

    // This method closes the one side of the whole that
    // has been cut to insert a new segment into the CDT.
    def stitchUp( a: Node, b: Node, candidates: ArrayBuffer[Node], from: Int, until: Int ): Unit
      = if( from < until ) {
          var i = until-1
          var split = i
          var C = candidates(split)
          while( i > from ) {
            i -= 1
            val c = candidates(i)
            if(_inCircle(a,b,C){c} ) {
              C = c
              split = i
            }
          }
          mesh.addTri(a,b,C)
          stitchUp(C,b, candidates, from,   split)
          stitchUp(a,C, candidates, split+1,until)
        }

    val V,W = ArrayBuffer.empty[Node]
    for( (a,b) <- plc.segments )
    {
      val A = nodes(a)
      val B = nodes(b)
      if( ! mesh.hasEdge(A,B) )
      {
        // FIND THE FIRST TRI INTERSECTING (A,B)
        var v,w = null: Node
        breakable{ mesh.foreachTriAround(A){
          (V,W) =>
            assert(  _orient2d(A,V,W) > 0 )
            val ov = _orient2d(A,B,V)
            val ow = _orient2d(A,B,W)
            assert( 0 != ov*ow, "PLC has node on segment." )
            if( ov < 0 && ow > 0 ) { // V ist left and W right of (A,B)
              v = V
              w = W
              assert( ! mesh.hasSegment(v,w), "PLC has crossing segments." )
              break() // <- TODO: test performance without break
            }
        }}
        assert( null != v )
        assert( null != w )
        // CUT OPEN
        V += v
        W += w
        var p = mesh.adjacent(w,v).nodeOrNull
        mesh.delTri(A,v,w)
        mesh.delTri(w,v,p)
        while( p != B ) {
          val op = _orient2d(A,B,p) ensuring (_ != 0, "PLC has node on segment.")
          if( op > 0 ) { w = p; W += w }
          else         { v = p; V += v }
          assert( ! mesh.hasSegment(v,w), "PLC has crossing segments." )
          p = mesh.adjacent(w,v).nodeOrNull
          mesh.delTri(w,v,p)
        }
        // REVERSE W
        var i = 0
        var j = W.length-1
        while( i < j ) {
          val tmp = W(i); W(i) = W(j); W(j) = tmp
          i += 1
          j -= 1
        }
        // STITCH UP
        stitchUp(B,A,V,0,V.length); V.clear()
        stitchUp(A,B,W,0,W.length); W.clear()
      }
      mesh.addSegment(A,B)
    }

//    assert( mesh.boundaries.length == 1 )

    if( plc.confinedBySegments ) {
      assert( mesh.nNodes == plc.nNodes, "plc.confinedBySegments=true only supported for initially empty meshes." )
      erode(mesh)
    }

    cutAroundNodes( mesh, plc.holeNodes map nodes )
    cutAlongSegments( mesh, plc.orientedBoundarySegments map { case (i,j) => (nodes(i),nodes(j)) } )

    nodes
  }

  def erode( mesh: TriMesh ): Unit = {
    def del( a: Node, b: Node ): Unit
      = if( ! mesh.hasSegment(a,b) )
          for( c <- mesh.adjacent(a,b) ) {
            mesh.delTri(a,b,c)
            del(a,c)
            del(c,b)
          }

    val Seq(boundary) = mesh.boundaries
    for( Seq(b,a) <- boundary.sliding(2) )
      del(a,b)
  }

  def cutAroundNodes( mesh: TriMesh, nodes: Traversable[Node] ): Unit = {
    def del( a: Node, b: Node ): Unit
      = for( c <- mesh.adjacent(a,b) )
        {
          mesh.delTri(a,b,c)
          if( ! mesh.hasSegment(a,c) ) del(a,c)
          if( ! mesh.hasSegment(c,b) ) del(c,b)
        }
    for( a <- nodes )
      mesh.foreachTriAround(a){ (b,_) => del(a,b) }
  }

  def cutAlongSegments( mesh: TriMesh, segments: Traversable[(Node,Node)] ): Unit = {
    def del( a: Node, b: Node ): Unit
      = for( c <- mesh.adjacent(a,b) )
        {
          mesh.delTri(a,b,c)
          if( ! mesh.hasSegment(a,c) ) del(a,c)
          if( ! mesh.hasSegment(c,b) ) del(c,b)
        }

    for( (a,b) <- segments )
      del(a,b)
  }
}
