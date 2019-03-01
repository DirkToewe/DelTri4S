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
import utest._

import scala.collection.mutable
import scala.util.Random

object Delaunay_tests extends TestSuite
{
  override val tests = Tests{

    'main {
      val rng = new Random(1337)

      for( _ <- 1 to 1024 )
      {
        val xy = Array.fill( rng.nextInt(1024) + 3 ){(
          rng.nextDouble*32 - 16,
          rng.nextDouble*32 - 16
        )}.to[mutable.HashSet] // <- remove duplicate points

        val mesh = TriMeshIndexed.empty
        Delaunay.triangulate(mesh)(xy.toArray:_*)

        assert( mesh.nNodes == xy.size )
        mesh foreachNode {
          case Node(x,y) => assert( xy remove (x,y) )
        }
        assert( xy.size == 0 )

        mesh foreachTri {
          (a,b,c) =>
            for( d <- mesh.adjacent(a,c) ) { assert( ! _inCircle(a,b,c)(d) ) }
            for( d <- mesh.adjacent(b,a) ) { assert( ! _inCircle(a,b,c)(d) ) }
            for( d <- mesh.adjacent(c,b) ) { assert( ! _inCircle(a,b,c)(d) ) }
        }

        val boundary = mutable.HashMap.empty[Node,Node]
        mesh foreachTri {
          (a,b,c) =>
            if( ! mesh.adjacent(a,c).exists ) { assert( ! boundary.contains(a) ); boundary(a) = c }
            if( ! mesh.adjacent(b,a).exists ) { assert( ! boundary.contains(b) ); boundary(b) = a }
            if( ! mesh.adjacent(c,b).exists ) { assert( ! boundary.contains(c) ); boundary(c) = b }
        }

        val (start,_) = boundary.iterator.next
        var a = start
        var b = boundary(a)

        while( boundary.size > 0 ) {
          val Some(c) = boundary.remove(b)

          assert( _orient2d(a,b,c) <= 0 ) // <- check convexity

          a = b; b = c
        }
        assert( a == start )
      }
    }

  }
}
