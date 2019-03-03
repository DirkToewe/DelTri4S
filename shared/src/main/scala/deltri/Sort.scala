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
import scala.reflect.ClassTag
import scala.util.Random
import Search._
import System.arraycopy
import java.lang.Integer.lowestOneBit
import java.lang.Math.abs

/** Implementations of (potentially unstable) sorting algorithms. In contrast to `java.util.Arrays.sort`
  * the algorithms support custom comparators for primitive Arrays.
  */
object Sort
{
  @inline def heapSort[@specialized T]( arr: Array[T], compare: (T,T) => Int ): Unit
    = heapSort(arr, compare, 0,arr.length)

  @inline def heapSort[@specialized T]( arr: Array[T], compare: (T,T) => Int, from: Int, until: Int ): Unit =
  {
    assert( 0 <= from)
    assert(      from <= until )
    assert(              until <= arr.length )

    var len = until

    @inline def less( i: Int, j: Int ) = compare( arr(i), arr(j) ) < 0
    @inline def swap( i: Int, j: Int ) = {
      val tmp = arr(i); arr(i) = arr(j); arr(j) = tmp
    }
    // returns the parent of a left child
    @inline def parent( child: Int ) = from + (child-from-1) / 2
    // returns the index of the left child of a node
    @inline def child( parent: Int ): Int = 1 + 2*parent - from

    @inline @tailrec def siftDown( p: Int ): Unit = {
      var c = child(p)
      if( c < len-1 ) {
        if( ! less(c+1,c) ) c += 1
        if(   less(p  ,c) ) { swap(p,c); siftDown(c) }
      }
      else if( c < len && less(p,c) ) swap(p,c)
    }

    // HEAPIFY
    var i = parent(len-1)
    while( i >= from ) {
      siftDown(i)
      i -= 1
    }

    // EXTRACT MINIMA
    len -= 1
    while( len > from ) {
      swap(from,len)
      siftDown(from)
      len -= 1
    }
  }


  @inline def quickSort[@specialized T]( arr: Array[T], compare: (T,T) => Int ): Unit
    = quickSort(arr, compare, 0,arr.length, new Random(1337) )

  @inline def quickSort[@specialized T]( arr: Array[T], compare: (T,T) => Int, rng: Random ): Unit
    = quickSort(arr, compare, 0,arr.length, rng)

  @inline def quickSort[@specialized T]( arr: Array[T], compare: (T,T) => Int, from: Int, until: Int ): Unit
    = quickSort(arr, compare, from,until, new Random(1337) )

  @inline def quickSort[@specialized T]( arr: Array[T], compare: (T,T) => Int, from: Int, until: Int, rng: Random ): Unit =
  {
    assert( 0 <= from)
    assert(      from <= until )
    assert(              until <= arr.length )

    @inline def swap( i: Int, j: Int ) = {
      val tmp = arr(i); arr(i) = arr(j); arr(j) = tmp
    }

    def qSort( from: Int, until: Int ): Unit
      = if( until-from <= 64 )
          binarySort(arr, compare, from,until)
        else {
          @inline def partition( pivot: T ): Int = {
            val mid = from+until >>> 1
            var j,k = until; while( k > from ) { k -= 1; if( compare(pivot,arr{k}) < 0 ) { j -= 1; swap(j,k) } }
            k = j          ; while( k > from ) { k -= 1; if( compare(pivot,arr{k}) ==0 ) { j -= 1; swap(j,k); if( j <= mid ) return j } }
            return j
          }
          // use median of N as pivot, reducing the probability of picking a bad pivot
          val N = 5
          val unt = from+N
          val mid = from+N/2
          var i = from
          // draw N random elements
          while( i < unt ) {
            swap( rng.nextInt(until-i)+i, i )
            i += 1
          }
          // sort upper half of the N elements
          i = unt; while( i >= mid ) { i -= 1
            var j =  i ; while( j > from ) { j -= 1
              if( compare(arr{i},arr{j}) < 0 ) swap(i,j)
            }}
          val pivot = arr(mid)
          val split = partition(pivot)
          qSort(from,split)
          qSort(split,until)
        }

    qSort(from, until)
  }


  @inline def meanSort( arr: Array[Double] ): Unit
    = meanSort( arr, (x: Double) => x, 0,arr.length )

  @inline def meanSort( arr: Array[Double], from: Int, until: Int ): Unit
    = meanSort( arr, (x: Double) => x, from, until )

  @inline def meanSort[@specialized T]( arr: Array[T], toDouble: T => Double ): Unit
    = meanSort(arr, toDouble, 0,arr.length )

  @inline def meanSort[@specialized T]( arr: Array[T], toDouble: T => Double, from: Int, until: Int ): Unit =
  {
    assert( 0 <= from)
    assert(      from <= until )
    assert(              until <= arr.length )

    @inline def vals( i: Int ): Double
      = toDouble{arr(i)}

    @inline def swap( i: Int, j: Int ) = {
      val tmp = arr(i); arr(i) = arr(j); arr(j) = tmp
    }

    def qSort( from: Int, until: Int ): Unit =
    {
      val len = until - from
      if( len <= 8 )
        binarySort[T](arr, (x: T,y: T) => {toDouble(x) - toDouble(y)}.signum, from,until)
      else {
        @inline def partition( pivot: Double ): Int = {
          val mid = from+until >>> 1
          var j,k = until; while( k > from ) { k -= 1; if( pivot <  vals{k} ) { j -= 1; swap(j,k) } }
          k = j          ; while( k > from ) { k -= 1; if( pivot == vals{k} ) { j -= 1; swap(j,k); if( j <= mid ) return j } }
          return j
        }

        var mean = 0.0
        var j = until; while( j > from ) { j -= 1; mean += vals(j) }
        mean /= len

        //        // https://en.wikipedia.org/wiki/Kahan_summation_algorithm
        //        var mean,rest = 0.0
        //        var j = until; while( j > from ) { j -= 1
        //          val a = vals(j) - rest
        //          val b = a + mean
        //          rest = (b - mean) - a
        //          mean =  b
        //        }
        //        mean /= len

        var pivot = vals(j)
        assert( j == from )
        while( {j+=1; j} < until ) {
          val       vals_j = vals(j)
          if(   abs{vals_j - mean}  <  abs{pivot - mean}   )
            pivot = vals_j
        }

        val split = partition(pivot)
        qSort(from,split)
        qSort(     split,until)
      }
    }
    qSort(from, until)
  }


  @inline def meanSortV2( arr: Array[Double] ): Unit
    = meanSortV2( arr, (x: Double) => x, 0,arr.length )

  @inline def meanSortV2( arr: Array[Double], from: Int, until: Int ): Unit
    = meanSortV2( arr, (x: Double) => x, from, until )

  @inline def meanSortV2[@specialized T]( arr: Array[T], toDouble: T => Double ): Unit
    = meanSortV2(arr, toDouble, 0,arr.length )

  @inline def meanSortV2[@specialized T]( arr: Array[T], toDouble: T => Double, from: Int, until: Int ): Unit =
  {
    assert( 0 <= from)
    assert(      from <= until )
    assert(              until <= arr.length )

    @inline def vals( i: Int ): Double
      = toDouble{arr(i)}

    @inline def swap( i: Int, j: Int ) = {
      val tmp = arr(i)
                arr(i) = arr(j)
                         arr(j) = tmp
    }

    def mSort( from: Int, until: Int ): Unit =
    {
      val len = until - from
      if( len < 16 )
        binarySort[T](arr, (x: T, y: T) => {toDouble(x) - toDouble(y)}.signum, from,until)
      else {
        @inline def partition( mean: Double ): Int = {
          var lo = Double.NegativeInfinity
          var hi = Double.PositiveInfinity
          var  j,k = until
          while( k > from ) {
                 k-= 1
            val vals_k = vals{k}
            if( vals_k >  mean ) { j-=1; swap(j,k) }
            if( vals_k >= mean && vals_k < hi ) hi = vals_k
            if( vals_k <= mean && vals_k > lo ) lo = vals_k
          }
          // following code handles degenerate cases like [0, 1, 1,... , 1]
          val mid = from+until >>> 1
             k = j
          while( j > mid && k > from ) { k-=1; if( lo == vals{k} ) { j-=1; swap(j,k);      }       }
          while( j < mid && k < until) {       if( hi == vals{k} ) {       swap(j,k); j+=1 }; k+=1 }
          return j
        }

        var mean = 0.0
        var j = from; while( j < until ) { mean += vals(j); j+=1 }
        mean /= len

        val        split = partition(mean)
        mSort(from,split)
        mSort(     split,until)
      }
    }
    mSort(from, until)
  }


  @inline def binarySort[@specialized T](arr: Array[T], compare: (T,T) => Int ): Unit
    = binarySort(arr, compare, 0,arr.length)

  /** A stable O(n²) sorting algorithm that is very closely related to Insertion Sort.
    * In contrast to Insertion Sort, only O(n*log(n)) comparisons are required. On
    * pre-sorted array, this algorithm take O(n*log(n)) which is more than the O(n)
    * operations Insertion Sort requires.
    *
    * @param arr
    * @param compare
    * @param from
    * @param until
    * @tparam T
    */
  @inline def binarySort[@specialized T](arr: Array[T], compare: (T,T) => Int, from: Int, until: Int ): Unit =
  {
    assert( 0 <= from)
    assert(      from <= until )
    assert(              until <= arr.length )
    var    i = from+1
    while( i < until )
    {
      val arr_i = arr(i)
      val j = ~ binarySearch( from,i, j => if( compare(arr(j),arr_i) <= 0 ) -1 else +1 )
      arraycopy(arr,j, arr,j+1, i-j)
      arr(j) = arr_i

      i += 1
    }
  }


  @inline def insertSort[@specialized T: ClassTag]( arr: Array[T], compare: (T,T) => Int ): Unit
    = insertSort(arr, compare, 0,arr.length)

  /** An implementation of the Insertion Sort algorithm.
    *
    * @param arr
    * @param compare
    * @param from
    * @param until
    * @tparam T
    */
  @inline def insertSort[@specialized T]( arr: Array[T], compare: (T,T) => Int, from: Int, until: Int ): Unit =
  {
    assert( 0 <= from)
    assert(      from <= until )
    assert(              until <= arr.length )

    var    i = from+1
    while( i < until ) {
      val arr_i = arr(i)
      var j=i
      while( j > from && compare(arr(j-1),arr_i) > 0 ) {
        arr(j) = arr(j-1)
        j -= 1
      }
      arr(j) = arr_i
      i += 1
    }
  }


  @inline def classicMergeSort[@specialized T]( ARR: Array[T], compare: (T,T) => Int ): Unit =
  {
    var tmp1 = ARR
    var tmp2 = ARR.clone

    var    stride = 1
    while( stride < ARR.length )
    {
      var    k = 0
      while( k < ARR.length )
      {
        val mid = k + stride   min  ARR.length
        val end = k + stride*2 min  ARR.length

        var i = k
        var j = k + stride

        while( i < mid && j < end ) {
          if( compare(tmp1{i},tmp1{j}) <= 0 ) { tmp2(k) = tmp1(i); i += 1 }
          else                                { tmp2(k) = tmp1(j); j += 1 }
          k += 1
        }

        if( i < mid ) arraycopy(tmp1,i, tmp2,k, mid-i)
        if( j < end ) arraycopy(tmp1,j, tmp2,k, end-j)
        k = end
      }

      { val tmp = tmp1; tmp1 = tmp2; tmp2 = tmp }
      stride *= 2
    }

    if( tmp1 ne ARR )
      arraycopy(tmp1,0, tmp2,0, ARR.length)
  }

  @inline def mergeSort[@specialized T]( ARR: Array[T], compare: (T,T) => Int, from: Int, until: Int ): Unit =
  {
    assert( 0 <= from )
    assert(      from <= until )
    assert(              until <= ARR.length )
    val tmp = ARR.slice(from,until)
    mergeSort(tmp, compare)
    arraycopy(tmp,0, ARR,from, until-from)
  }

  @inline def mergeSort[@specialized T]( ARR: Array[T], compare: (T,T) => Int ): Unit =
  {
    @inline def MIN = 64
    var   tmp1 = ARR
    var   tmp2 = ARR.clone
    var stride = ARR.length
    if( ARR.length >= 2*MIN )
    {
      val    pow2 = {
        var  pow2 = 1 << 31
        if( {pow2 >>>16} > ARR.length ) pow2 >>>=16
        if( {pow2 >>> 8} > ARR.length ) pow2 >>>= 8
        if( {pow2 >>> 4} > ARR.length ) pow2 >>>= 4
        if( {pow2 >>> 2} > ARR.length ) pow2 >>>= 2
        if( {pow2 >>> 1} > ARR.length ) pow2 >>>= 1
        assert( pow2   >  ARR.length )
        assert( pow2/2 <= ARR.length )
        pow2 / (2*MIN)
      }
      stride = ~ binarySearch( MIN,MIN*2, s => if( s*pow2 < ARR.length ) -1 else +1 )
      assert( stride*pow2     >= ARR.length )
      assert( stride*pow2-pow2 < ARR.length )
    }

    var    k  =  0
    while( k  <  ARR.length ) {
      binarySort(ARR, compare, k,k+stride min ARR.length)
      k += stride
    }

    while( stride < ARR.length ) {
             k = 0
      while( k < ARR.length ) {
        var  i  = k
        var  j  = k + stride
        val mid = k + stride   min  ARR.length
        val end = k + stride*2 min  ARR.length

        while( i < mid && j < end ) {
          if( compare(tmp1{i},tmp1{j}) <= 0 ) { tmp2(k) = tmp1(i); i += 1 }
          else                                { tmp2(k) = tmp1(j); j += 1 }
          k += 1
        }
        if( i < mid ) arraycopy(tmp1,i, tmp2,k, mid-i)
        if( j < end ) arraycopy(tmp1,j, tmp2,k, end-j)
        k = end
      }
      // swap arrays
      { val tmp = tmp1; tmp1 = tmp2; tmp2 = tmp }
      stride *= 2
    }

    if( tmp1 ne ARR )
      arraycopy(tmp1,0, tmp2,0, ARR.length)
  }

  def smoothSort[@specialized T]( arr: Array[T], compare: (T,T) => Int ): Unit =
  {
    /** Sifts the value at index `i` down into the subheap whose root inndex ist `i`.
      *
      * @param off The current (sub)heap contains `2*off-1` elements counting the root node at index `i`.
      * @param i   The root index of current (sub)heap. The value at `i` needs to be sifted down to reinstate the heap property.
      */
    @inline @tailrec def siftDown( off: Int, i: Int ): Unit
      = if( off > 1 ensuring off > 0 )
        {
//          assert( bitCount(off) == 1 )
          val l = i-off; val arrL = arr(l)
          val r = i-1  ; val arrR = arr(r)
                         val arrI = arr(i)
          if(   compare(arrL,arrR) >= 0 ) {
            if( compare(arrL,arrI) >  0 ) {
              arr(i) = arrL
              arr(l) = arrI; siftDown(off >>> 1, l)
            }
          }
          else if( compare(arrR,arrI) > 0 ) {
            arr(i) = arrR
            arr(r) = arrI; siftDown(off >>> 1, r)
          }
        }

    /** Reinstates the order of the heap roots (recursively). At each
      * point this method looks at two adjacent heaps.
      *
      * @param visits The remaining heaps left of j (including i)
      * @param i Root index of the current right heap neighbour.
      * @param j Root index of the current left heap neighbour.
      */
    @inline @tailrec def insertSort( visits: Int, i: Int, j: Int ): Unit =
    {
      val off = j-i+1 >>> 1
//      assert( i > -2 )
//      assert( i <  j )
//      assert( bitCount(off) == 1 )
      // find largest element in right heap
      var arr_j = arr{j}
      if( j-i > 1 ) {
        val l = j-off; val arr_l = arr(l)
        val r = j-1  ; val arr_r = arr(r)
             if( compare(arr_l,arr_r) >= 0 )
           { if( compare(arr_l,arr_j) >  0 ) arr_j = arr_l }
        else if( compare(arr_r,arr_j) >  0 ) arr_j = arr_r
      }

      if( 0 > i || compare(arr{i},arr_j) <= 0 )
        siftDown(off, j)
      else {
        // swap i<->j
        val tmp = arr(i)
                  arr(i) = arr(j)
                           arr(j) = tmp
        // compare/sort left heap with its left neighbor
        val bit = lowestOneBit(visits)
        insertSort( ~bit & visits, i - 2*bit+1, i )
      }
    }

    var stretches = 0 // <- (i+1)-th bit set if left of last node, there is a heap of size (2^(i+1)-1)
    var last      = 1 // <- rightmost heap has a size of 2*last-1
    // BUILD STRETCHED HEAPS (LEFT -> RIGHT)
    var    N = 1
    while( N < arr.length ) {
//      assert(       bitCount(last) == 1 )
//      assert( {last-1 & stretches} == 0 )
      if( {last & stretches} == 0 ) { // new heap
        stretches |= last; last = 1
        insertSort( stretches, N - 2*last + 1, N )
      }
      else { // merge heaps
        stretches &= ~last; last <<= 1
        siftDown(last, N)
      }
//      assert( 0 until N forall { i => compare(arr{i},arr{N}) <= 0 } )
      N += 1
    }

    // DEQUEUE MAXIMA (LEFT <- RIGHT)
    while( N > 2 ) {
           N-= 1
//      assert(       bitCount(last) == 1 )
//      assert( {last-1 & stretches} == 0 )
      if( last == 1 ) { // dissolve heap
          last = lowestOneBit(stretches)
                              stretches &= ~last
      }
      else { // split heap
        last >>>= 1
//        assert( last > 0 )
        insertSort(stretches, N - 4*last + 1, N - 2*last); stretches |= last
        insertSort(stretches, N - 2*last,     N - 1)
      }
//      assert( 0 until N-1 forall { i => compare(arr{i},arr{N-1}) <= 0 } )
    }
  }

//  @inline def  timSort[@specialized T]( arr: Array[T], compare: (T,T) => Int ): Unit = ???
//  @inline def wikiSort[@specialized T]( arr: Array[T], compare: (T,T) => Int ): Unit = ???
}
