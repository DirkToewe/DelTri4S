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
import java.util.Arrays
import java.lang.{ Double => JDouble }

import utest._
import System.arraycopy

class SortTest( sort: (Array[Double], (Double,Double) => Int, Int, Int) => Unit ) extends TestSuite
{
  private val rng = new Random(1337)

  private def test( compare: (Double,Double) => Int ) =
  {
    for( _ <- 1 to 64*1024 )
    {
      val arr = Array.tabulate[ Double]( rng.nextInt(1024)+1 )( _ => rng.nextDouble )
      val ref = Array.tabulate[JDouble](arr.length){arr(_)}
      val until = rng.nextInt(arr.length+1)
      val from  = rng.nextInt(     until+1)

      Arrays sort ( ref, from,until, compare(_: JDouble,_: JDouble) )

      sort(arr, compare, from,until)

      for( i <- 0 until arr.length )
        assert{ arr(i) == ref(i) }
    }
  }

  override val tests = Tests{

    'ascending {
      test{ (x,y) => (x-y).signum }
    }

    'descending {
      test{ (x,y) => (y-x).signum }
    }

  }
}

object SortTest_heap   extends SortTest( Sort   heapSort (_,_,_,_) )
object SortTest_quick  extends SortTest( Sort  quickSort (_,_,_,_) )
object SortTest_merge  extends SortTest( Sort  mergeSort (_,_,_,_) )
object SortTest_binary extends SortTest( Sort binarySort (_,_,_,_) )
object SortTest_insert extends SortTest( Sort insertSort (_,_,_,_) )

object SortTest_mean   extends SortTest(
  (arr, compare, from,until) => {
    val toDouble: Double => Double
      = if( compare(-1,+1) < 0 ) x =>  x
        else                     x => -x
    Sort meanSort(arr, toDouble, from,until)
  }
)

object SortTest_smooth extends SortTest(
  (arr, compare, from,until) => {
    val slice = arr.slice(from,until)
    Sort smoothSort (slice,compare)
    arraycopy(slice,0, arr,from, until-from)
  }
)

object SortTest_classicMerge extends SortTest(
(arr, compare, from,until) => {
    val slice = arr.slice(from,until)
    Sort classicMergeSort (slice,compare)
    arraycopy(slice,0, arr,from, until-from)
  }
)

object SortTest_jdk   extends SortTest(
  (arr, compare, from,until) => {
    val tmp = Array.tabulate[JDouble](until - from){ i => arr(from+i) }

    Arrays.sort( tmp, compare(_: JDouble,_: JDouble) )

    var    i = until - from
    while( i > 0 ) {
           i-= 1
      arr(from+i) = tmp(i)
    }
  }
) {}
