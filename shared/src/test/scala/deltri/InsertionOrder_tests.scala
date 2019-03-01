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

import scala.util.Random
import utest._
import Sort.meanSort

class InsertionOrderTests( order: (Array[Double],Array[Double]) => Array[Int] ) extends TestSuite
{
  private val rng = new Random(1337)

  override val tests = Tests{

    'test {
      for( _ <- 1 to 1024 )
      {
        val N = rng.nextInt(1024)
        val x = Array.tabulate(N)( _ => rng.nextDouble*2 - 1 )
        val y = Array.tabulate(N)( _ => rng.nextDouble*2 - 1 )
        val xRef = x.clone
        val yRef = y.clone

        val idx = order(x,y)
        assert( idx.length == N )

        for( i <- 0 until N ) {
          assert( x(i) == xRef(i) )
          assert( y(i) == yRef(i) )
        }

        val visited = new Array[Boolean](N)
        for(  i <- 0 until N ) {
          val j = idx(i)
          assert( ! visited(j) )
                    visited(j) = true
        }

        for( i <- 0 until N )
          assert( visited(i) )
      }
    }

  }
}

object InsertionOrderTests_zOrder                   extends InsertionOrderTests( InsertOrder.            zOrder            )
object InsertionOrderTests_zOrderSelect             extends InsertionOrderTests( InsertOrder.            zOrderSelect      )
object InsertionOrderTests_hierarchicalZOrder       extends InsertionOrderTests( InsertOrder.hierarchicalZOrder            )
object InsertionOrderTests_hierarchicalZOrderRandom extends InsertionOrderTests( InsertOrder.hierarchicalZOrderRandom(_,_) )
