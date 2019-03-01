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

import utest._
import scala.util.Random
import Sort.meanSort
import Search.binarySearch

object SearchTests extends TestSuite
{
  override val tests = Tests{

    'main {

      val rng = new Random(1337)

      val MAX = 4
      for( _ <- 1 to 1024 )
      {
        val N = rng.nextInt(MAX*MAX)+1
        val arr = Array.tabulate[Int](N)(_ => rng.nextInt(MAX))
        meanSort(arr, (x: Int) => x)

        for( _ <- 1 to 1024 )
        {
          for( i <- 1 until arr.length )
            assert( arr(i-1) <= arr(i) )

          val until = rng.nextInt(arr.length+1)
          val from  = rng.nextInt(     until+1)

          val elem  = rng.nextInt(MAX)

          var i = binarySearch(from,until, i => {arr(i) - elem}.signum)
          var j = until
          if( i <  0 ) {
              i = ~i
            while( j > i    ) { j-= 1; assert( elem   <  arr(j) ) }
            while( j > from ) { j-= 1; assert( arr(j) <= elem   ) }
          }
          else {
            while( j > i    ) { j-= 1; assert( elem   <= arr(j) ) }
            while( j > from ) { j-= 1; assert( arr(j) <= elem   ) }
            assert( arr(i) == elem )
          }
        }
      }

    }

  }
}
