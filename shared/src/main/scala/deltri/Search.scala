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

import scala.annotation.tailrec

object Search
{
  @inline def binarySearch[@specialized T]( arr: Array[T], key: T, compare: (T,T) => Int ): Int
    = binarySearch(arr,key, compare, 0,arr.length)

  @inline def binarySearch[@specialized T]( arr: Array[T], key: T, compare: (T,T) => Int, from: Int, until: Int ): Int
    = binarySearch( from,until, i => compare(arr(i), key) )

  @inline def binarySearch( from: Int, until: Int, compare: Int => Int ): Int =
  {
    @tailrec def binSearch( from: Int, to: Int ): Int
      = if( from > to )
          ~ from
        else {
          val mid = from + to >>> 1
          val      c = compare(mid)
               if( c < 0 ) binSearch(     mid+1,to)
          else if( c > 0 ) binSearch(from,mid-1   )
          else mid
        }
    binSearch(from,until-1)
  }
}
