package deltri

import scala.util.Random
import System.arraycopy
import Sort._

/** As Shewchuck explains in his <a href="https://people.eecs.berkeley.edu/~jrs/meshpapers/delnotes.pdf">lecture notes</a>,
  * the perfomance of the point insertion algorithm is dependent on the insertion order.
  * Two conflicting aspects have to be considered. On the one hand, nodes should be inserted
  * in a space-filling order so that finding the containing triangle by walking is fast.
  * On the other hand inserting the points in a space filling order causes many retriangulations
  * because the new points are added on the border of an already dense mesh and entire segments
  * of the boundary may have to be retriangulated.
  *
  * In order to avoid that, a hybrid approach can be used: The nodes are grouped in sets of
  * increasing size. The nodes in each group are uniformly distributed over the space and
  * ordered in a spacefilling order. Insertion order methods that use this hybrid approach
  * are prefixed with `hierarchical`.
  */
object InsertOrder
{
  /** Recursively splits the points roughly in half by x and y coordinates interleavedly.
    * The resulting tree is then iterated breadth-first
    *
    * @param x
    * @param y
    * @return
    */
  def hierarchicalZOrder( x: Array[Double], y: Array[Double] ): Array[Int] =
  {
    assert( x.length == y.length )
    val len = x.length
    // arr1: the nodes sorted by one coord
    // arr2: pointers in into arr1 sorted by the other coord
    val arr1,
        arr2 = {
          val arr = new Array[Int](len)
          var   i = len
          while(i > 0) {
                i-= 1
            arr(i) = i
          }
          arr
        }
    val tmp = new Array[Int](len) // <- temporary storage

//    meanSort( arr1, (i: Int) =>      x(i)  )
//    meanSort( arr2, (i: Int) => y{arr1(i)} )
    mergeSort( arr1, (i: Int,j: Int) => {     x(i)  -      x(j) }.signum )
    mergeSort( arr2, (i: Int,j: Int) => (y{arr1(i)} - y{arr1(j)}).signum )

    def split( from: Int, until: Int, depth: Int ): Unit =
    {
      val mid = (until+from) / 2
      until-from match {
        case  0  => throw new AssertionError
        case  1  =>
        case len =>
          var i,k = from
          var j = mid+1
          while( k < until ) {
            val l = arr2(k)
            if( l < mid ) { tmp(i) = arr1(l); arr1(l) = i; i += 1 }
            if( l > mid ) { tmp(j) = arr1(l); arr1(l) = j; j += 1 }
            k += 1
          }
          assert(i == mid  )
          assert(j == until)

          tmp(mid) = arr1(mid)

          arraycopy(arr1, from, arr2, from, len)
          arraycopy(tmp,  from, arr1, from, len)

          split(from,  mid,   depth+1)
          if( mid+1 < until )
            split(mid+1, until, depth+1)
      }
      arr2(mid) = depth
    }
    if( len > 0 )
      split(0,len, 0)

    var depth = 31
    if( {1 << depth-16} >= len ) depth -= 16
    if( {1 << depth- 8} >= len ) depth -=  8
    if( {1 << depth- 4} >= len ) depth -=  4
    if( {1 << depth- 2} >= len ) depth -=  2
    if( {1 << depth- 1} >= len ) depth -=  1

    val off = new Array[Int](depth+1) // ensuring depth == arr2.max

    // counting how many values are in the indidual depths
    var i = len
    while( i > 0 ) {
      i -= 1
      val d = arr2(i)
      off(d) += 1
    }

    // determinde the starting indices for each depth
    i = 0
    var sum = 0
    while( i <= depth ) {
      val n = off(i); off(i) = sum; sum += n
      i += 1
    }
//    assert(sum == len)
//    Arrays.fill(tmp,-1) // <- for debugging purposes
    // bring the result in order
    i = len
    while( i > 0 ) {
      i -= 1
      val d = arr2(i)
      val j = off(d); off(d) = j+1
//      assert( tmp(j) == -1 )
      tmp(j) = arr1(i)
    }
//    assert( tmp.forall(_ >= 0) )
//    assert( Set(arr2:_*).size == arr2.length )
    return tmp
  }

  def zOrder( x: Array[Double], y: Array[Double] ): Array[Int] =
  {
    assert( x.length == y.length )
    val len = x.length
    // arr1: the nodes sorted by one coord
    // arr2: pointers in into arr1 sorted by the other coord
    val arr1,
        arr2 = {
          val arr = new Array[Int](len)
          var   i = len
          while(i > 0) {
            i-= 1
            arr(i) = i
          }
          arr
        }
    val tmp = new Array[Int](len) // <- temporary storage

    mergeSort( arr1, (i: Int,j: Int) => {     x(i)  -      x(j) }.signum )
    mergeSort( arr2, (i: Int,j: Int) => (y{arr1(i)} - y{arr1(j)}).signum )

    def split( from: Int, until: Int, depth: Int ): Unit =
    {
      val mid = (until+from) / 2
      until-from match {
        case  0  => throw new AssertionError
        case  1  =>
        case len =>
          var i,k = from
          var j = mid+1
          while( k < until ) {
            val l = arr2(k)
            if( l < mid ) { tmp(i) = arr1(l); arr1(l) = i; i += 1 }
            if( l > mid ) { tmp(j) = arr1(l); arr1(l) = j; j += 1 }
            k += 1
          }
          assert(i == mid  )
          assert(j == until)

          tmp(mid) = arr1(mid)

          arraycopy(arr1, from, arr2, from, len)
          arraycopy(tmp,  from, arr1, from, len)

          split(from,  mid,   depth+1)
          if( mid+1 < until )
            split(mid+1, until, depth+1)
      }
      arr2(mid) = depth
    }
    if( len > 0 )
      split(0,len, 0)

    arr1
  }

  def zOrderSelect( x: Array[Double], y: Array[Double] ): Array[Int] =
  {
    assert( x.length == y.length )
    val order = {
      val arr = new Array[Int](x.length)
      var   i = arr.length
      while(i > 0) {
        i-= 1
        arr(i) = i
      }
      arr
    }

    def split( from: Int, until: Int, f1: Int => Double, f2: Int => Double ): Unit
      = if( until - from > 1 )
        {
          val mid = from+until >>> 1
          Select.mean(order,f1,mid, from,until)
          split(from,mid,         f2,f1)
          split(     mid+1,until, f2,f1)
        }

    split(0, x.length, x, y)
    order
  }

  def hierarchicalZOrderRandom( x: Array[Double], y: Array[Double], rng: Random = new Random(1337) ): Array[Int] =
  {
    assert(   x.length == y.length )
    val len = x.length
    // arr1: the nodes sorted by one coord
    // arr2: pointers in into arr1 sorted by the other coord
    val arr1,
        arr2 = {
          val  arr = new Array[Int](len)
          var   i = len
          while(i > 0) {
            i-= 1
            arr(i) = i
          }
          arr
        }
    val tmp = new Array[Int](len) // <- temporary storage

    // shuffle arr1
    ;{
      var    i = len
      while( i > 0 ) {
        val  j = rng.nextInt(i)
             i-= 1
        val arr1_i = arr1(i)
                     arr1(i) = arr1(j)
                               arr1(j) = arr1_i
      }
    }

    var FROM = 0

    def split( from: Int, until: Int, depth: Int ): Unit =
    {
      val mid = (until+from) / 2
      until-from match {
        case  0  => throw new AssertionError
        case  1  =>
        case len =>
          var i,k = from
          var j = mid+1
          while( k < until ) {
            val l = arr2(k)
            if( l < mid ) { tmp(i) = arr1(l); arr1(l) = i; i += 1 }
            if( l > mid ) { tmp(j) = arr1(l); arr1(l) = j; j += 1 }
            k += 1
          }
          assert(i == mid  )
          assert(j == until)

          tmp(mid) = arr1(mid)

          arraycopy(arr1, from, arr2, from, len)
          arraycopy(tmp,  from, arr1, from, len)

          split(from,  mid,   depth+1)
          if( mid+1 < until )
            split(mid+1, until, depth+1)
      }
      arr2(mid) = FROM+depth
    }

    while( FROM < len ) {
      val until = FROM*2 + 1  min  len
      meanSort( arr1, (i: Int) =>      x(i) , FROM,until )
      meanSort( arr2, (i: Int) => y{arr1(i)}, FROM,until )
//      mergeSort( arr1, (i: Int,j: Int) => {     x(i)  -      x(j) }.signum, FROM,until )
//      mergeSort( arr2, (i: Int,j: Int) => (y{arr1(i)} - y{arr1(j)}).signum, FROM,until )
      split(FROM,until,0)
      FROM = until
    }

    arr1
  }
}
