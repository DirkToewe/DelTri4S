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

import java.lang.System.nanoTime

import scala.collection.mutable
import scala.util.Random

object TriMesh_experiments
{
  def main( args: Array[String] ) =
  {
    val rng = new Random(1337)

    for( i <- 1 to 1000*1000 )
    {
      val (x,y) = {
        val N = 1000*1000
        val pts = Array.fill(N){(rng.nextDouble,rng.nextDouble)}
          .to[mutable.HashSet] // <- remove duplicate points
          .toArray
        ( Array.tabulate(pts.length){pts(_)._1},
          Array.tabulate(pts.length){pts(_)._2} )
      }
      assert( x.length == y.length )

      val t0 = nanoTime

      val (mesh,_) = TriMeshIndexed.delaunay(x,y)

      val dt = (nanoTime - t0) / 1e9

      assert( mesh.nNodes == x.length )

      println(f"$dt%.3f seconds")
    }
  }
}
