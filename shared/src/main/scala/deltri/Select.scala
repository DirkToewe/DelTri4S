package deltri

import scala.annotation.tailrec
import scala.util.Random
import Math.abs

/** A collections of different Selection algorithms. The selection problem is stated as follows: given
  * an Array `values` and an index `ì` the selection algorithm moves the element around such that:
  *   * values(i) <= values(j) forall j >= i
  *   * values(i) >= values(j) forall j <= i
  *
  * The selection problem can be solved in O(n).
  */
object Select
{
  /** An implementation of Quick-Select.
    *
    * @param values
    * @tparam T
    */
  def quick[@specialized T]( values: Array[T], i: Int, compare: (T,T) => Int ): Unit
    = quick(values,i, compare, 0,values.length)

  def quick[@specialized T]( values: Array[T], i: Int, compare: (T,T) => Int, from: Int, until: Int, rng: Random = new Random(1337)/*deterministic seed (reproducable)*/ ): Unit =
  {
    assert( 0 <= from )
    assert(      from <= i )
    assert(              i < until )
    assert(                  until <= values.length )

    @inline def swap( i: Int, j: Int ): Unit = {
      val tmp = values(i)
                values(i) = values(j)
                            values(j) = tmp
    }

    @tailrec def select( from: Int, until: Int ): Unit =
    {
      @inline def partition( pivot: T ): Int = {
        var j,k = until; while( k > from ) { k -= 1; if( compare(pivot,values{k}) < 0 ) { j -= 1; swap(j,k) } }
        k = j          ; while( k > from ) { k -= 1; if( compare(pivot,values{k}) ==0 ) { j -= 1; swap(j,k); if( j <= i ) return j } }
        return j
      }

      val len = until - from ensuring (_ > 0)
      if( len < 32 || until-i <= 2 || i-from < 2 )
        bubble(values,i, compare, from,until)
      else {
        val pivot = values( rng.nextInt(until-from) + from )
        val split = partition(pivot)
        assert( compare(values(split),pivot) == 0 )
        if( split > i ) select(from,    split) else
        if( split < i ) select(split+1, until)
      }
    }

    select(from,until)
  }


  def mom[@specialized T]( values: Array[T], i: Int, compare: (T,T) => Int ): Unit
    = mom(values,i, compare, 0,values.length)

  /** Implementation of the "Repeated Step Algorithm" from the paper "Select with Groups of 3 or 4∗"
    * which is a derivation of the Median of Medians algorithm that works with a group size of 3.
    *
    * @param values
    * @param i
    * @param from
    * @param until
    * @param ord
    * @tparam T
    */
  def mom[@specialized T]( values: Array[T], i: Int, compare: (T,T) => Int, from: Int, until: Int ): Unit =
  {
    assert( 0 <= from )
    assert(      from <= i )
    assert(              i < until )
    assert(                  until <= values.length )

    @inline def swap( i: Int, j: Int ): Unit = {
      val  tmp  = values(i)
                  values(i) = values(j)
                              values(j) = tmp
    }

    def select( i: Int, from: Int, until: Int ): Unit =
    {
      assert( 0 <= from )
      assert(      from <= i )
      assert(              i < until )
      assert(                  until <= values.length )
      /** Splits the values into groups of three, finds the median of each group
        * and move all those medians to the beginning of the range (starting at from)
        * by swapping values.
        *
        * @param until The exclusive end of the range, sel is applied to.
        */
      @inline def sel3( until: Int ) = {
        var i = from
        var j = from
        // find the median for groups of three
        while( j < until-5 ) {
          val ab = compare(values{j+0}, values{j+1}) < 0
          val ac = compare(values{j+0}, values{j+2}) < 0
          val bc = compare(values{j+1}, values{j+2}) < 0
               if( ab != ac ) swap(i, j+0)
          else if( ab == bc ) swap(i, j+1)
          else                swap(i, j+2)
          i += 1
          j += 3
        }
        // find median of the last group of 3-5 values
        val mid = (until+j) / 2
                   while( j <= mid  ) {
        var k=j+1; while( k < until ) {
          if( compare(values{j},values{k}) > 0 ) swap(j,k)
        k += 1 }
        j += 1 }
        swap(i,mid)
      }

      @inline def partition( pivot: T ): Int = {
        var  j,i,k = until
        while( i > from ) {
               i-= 1
          val c = compare(pivot, values{i})
          if( c < 0 ) { j -= 1; swap(i,j) }
          if( c ==0 )   k  = i
        }
        j -= 1; swap(j,k)
        j
      }

      var len = until - from ensuring (_ > 0)
      if( len < 9*3 ) // <- len < 9 is lowest bound possible
        bubble(values,i, compare, from,until)
      else {
        var k = 2; while( k > 0 ) { k -= 1; sel3(from + len); len /= 3 }

        val split  =  from + len/2
        select(split, from,  len+from)

        val j = partition( values(split) )
        if( j > i ) select(i, from, j    ) else
        if( j < i ) select(i, j+1,  until)
      }
    }

    select(i, from, until)
  }


  def mom5[@specialized T]( values: Array[T], i: Int, compare: (T,T) => Int ): Unit
    = mom5(values,i, compare, 0,values.length)

  /** Classic median of medians implementation based with a group size of 5.
    *
    * @param values
    * @param i
    * @param from
    * @param until
    * @param ord
    * @tparam T
    */
  def mom5[@specialized T]( values: Array[T], i: Int, compare: (T,T) => Int, from: Int, until: Int ): Unit =
  {
    assert( 0 <= from )
    assert(      from <= i )
    assert(              i < until )
    assert(                  until <= values.length )

    def select( i: Int, from: Int, until: Int ) =
    {
      var  k,j = 0
      while( j < until-4 ) {
        val end = j+5 min until

        ???

        j = end
      }
    }

    select(i,from,until)
  }

  @inline def bubble[@specialized T]( values: Array[T], i: Int, compare: (T,T) => Int ): Unit
    = bubble(values,i, compare, 0,values.length)

  @inline def bubble[@specialized T]( values: Array[T], i: Int, compare: (T,T) => Int, from: Int, until: Int ): Unit =
  {
    assert( 0 <= from )
    assert(      from <= i )
    assert(              i < until )
    assert(                  until <= values.length )

    @inline def swap( i: Int, j: Int ): Unit = {
      val  tmp  = values(i)
                  values(i) = values(j)
                              values(j) = tmp
    }

    var    k = i
    while( k < until ) {
      var    j = from
      while( j < i ) {
        if(        compare(values{j},values{k}) > 0 ) swap(j,k)
        j += 1
      }
      if( i < k && compare(values{i},values{k}) > 0 ) swap(i,k)
      k += 1
    }
  }


  /** A selection algorithm similar to Quick Select. Instead of a random element however,
    * the average value is used as pivot value.
    *
    * @param values
    * @param i
    */
  def mean(values: Array[Double], i: Int ): Unit
    = mean( values, (x: Double) => x, i, 0, values.length )

  def mean[@specialized T](values: Array[T], toDouble: T => Double, i: Int ): Unit
    = mean( values, toDouble, i, 0, values.length )

  def mean[@specialized T](values: Array[T], toDouble: T => Double, i: Int, from: Int, until: Int ): Unit =
  {
    assert( from <= i )
    assert(         i < until )

    @inline def swap( i: Int, j: Int ): Unit = {
      val  tmp  = values(i)
                  values(i) = values(j)
                              values(j) = tmp
    }

    @inline def vals( i: Int ): Double = toDouble( values(i) )

    @tailrec def select( from: Int, until: Int ): Unit = {
      @inline def partition( pivot: Double ): Int = {
        var j,k = until; while( k > from ) { k -= 1; if( pivot <  vals(k) ) { j -= 1; swap(j,k) } }
              k = j    ; while( k > 0    ) { k -= 1; if( pivot == vals(k) ) { j -= 1; swap(j,k); if( j <= i ) return j } }
        return    j
      }

      val len = until - from ensuring (_ > 0)
      if( len <= 8 ) {
        var j = from; while( j <= i    ) {
        var k = j+1 ; while( k < until ) {
          if( vals(j) > vals(k) ) swap(j,k)
        k += 1 }
        j += 1 }
      }
      else {
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
        assert( pivot == vals(split) )
        if( split > i ) select(from,    split) else
        if( split < i ) select(split+1, until)
      }
    }

    select(from,until)
  }
}
