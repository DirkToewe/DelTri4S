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
