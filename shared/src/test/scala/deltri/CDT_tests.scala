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

import deltri.TriMesh.{Node, _inCircle}
import deltri.TriMeshIndexed.IndexedNode
import utest._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object CDT_tests extends TestSuite
{
  private[deltri] def randPLC( rng: Random ): (PLC,Int) =
  {
    var mesh: TriMesh{ type NodeType = IndexedNode }
      = RandomTriMesh.holeFreeUniform( rng.nextInt(32)+3 )
    mesh = TriMeshTaped(mesh)

    var nHoles = rng.nextInt(4)

    for( _ <- 1 to nHoles ) {
      RandomTriMesh.insertNodes(mesh,rng.nextInt(128)+32 )
      if( ! RandomTriMesh.digHole(mesh, minAngle = 10.toRadians, maxAngle = 350.toRadians) )
        nHoles -= 1
    }
    RandomTriMesh.insertNodes(mesh,rng.nextInt(128)+32 )

    assert( mesh.boundaries.length == nHoles+1 )

    val nodes = mesh.nodes.toArray
    val coords = nodes map { n => (n.x,n.y) }
    val n2i = nodes.zipWithIndex.toMap
    val segments: Seq[(Int,Int)]
      = for(
          boundary <- mesh.boundaries;
          Seq(a,b) <-      boundary.sliding(2)
        ) yield n2i(a) -> n2i(b)

    val plc = PLC(
      coords,
      segments,
      false,
      segments
    )

    (plc, nHoles)
  }

  override val tests = Tests{

    'main {

      val rng = new Random(1337)

      for( _ <- 1 to 1024 )
      {
        val (plc,nHoles) = randPLC(rng)

        val mesh = TriMeshIndexed.empty
        val nodes = CDT.triangulate(mesh,plc)

        val xy = plc.nodes.to[mutable.HashSet]
        assert( mesh.nNodes == plc.nNodes )
        mesh foreachNode {
          case Node(x,y) => assert( xy remove (x,y) )
        }
        assert( xy.size == 0 )

        val segs = plc.segments.flatMap{ case (i,j) => Seq(
          nodes(i) -> nodes(j),
          nodes(j) -> nodes(i)
        ) }.to[mutable.HashSet]

        mesh foreachTri {
          (a,b,c) =>
            val ac = segs contains (a,c)
            val ba = segs contains (b,a)
            val cb = segs contains (c,b)
            assert( mesh.hasSegment(a,c) == ac )
            assert( mesh.hasSegment(b,a) == ba )
            assert( mesh.hasSegment(c,b) == cb )
            if( ! ac ) for( d <- mesh.adjacent(a,c) ) { assert( ! _inCircle(a,b,c)(d) ) }
            if( ! ba ) for( d <- mesh.adjacent(b,a) ) { assert( ! _inCircle(a,b,c)(d) ) }
            if( ! cb ) for( d <- mesh.adjacent(c,b) ) { assert( ! _inCircle(a,b,c)(d) ) }
        }

        mesh foreachSegment {
          (m,n) =>
            assert( segs remove (m,n) )
            assert( segs remove (n,m) )
        }
        assert( segs.size == 0 )

        val boundary = mutable.HashMap.empty[Node,Node]
        mesh foreachTri {
          (a,b,c) =>
            if( ! mesh.adjacent(a,c).exists ) { assert( ! boundary.contains(a) ); boundary(a) = c }
            if( ! mesh.adjacent(b,a).exists ) { assert( ! boundary.contains(b) ); boundary(b) = a }
            if( ! mesh.adjacent(c,b).exists ) { assert( ! boundary.contains(c) ); boundary(c) = b }
        }

        for( _ <- 0 to nHoles )
        {
          val (start,_) = boundary.iterator.next
          @tailrec def loop( a: Node, b: Node ): Unit = {
            val Some(c) = boundary remove b

            if( b != start ) loop(b,c)
          }
          loop( start, boundary(start) )
        }
        assert( boundary.size == 0 )
      }
    }

  }
}
