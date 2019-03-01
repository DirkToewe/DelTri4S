package deltri

import utest._
import Sort.mergeSort

import scala.util.Random

class SelectTests( select: (Array[Double],Int, (Double,Double) => Int, Int,Int) => Unit ) extends TestSuite
{
  private val rng = new Random(1337)

  private def test( compare: (Double,Double) => Int ) =
  {
    for( _ <- 1 to 64*1024 )
    {
      val arr = Array.tabulate[Double]( rng.nextInt(1024)+1 )( _ => rng.nextDouble )
      val ref = arr.clone

      val until = rng.nextInt(arr.length)+1
      val     i = rng.nextInt(until)
      val  from = rng.nextInt(i+1)

      select(arr,i, compare, from,until)
      var    j = 0
      while( j < from ) {
        assert( arr(j) == ref(j) )
        j += 1
      }
      while( j < i ) {
        assert( compare(arr(j),arr(i)) <= 0 )
        j += 1
      };j += 1
      while( j < until ) {
        assert( compare(arr(j),arr(i)) >= 0 )
        j += 1
      }
      while( j < arr.length ) {
        assert( arr(j) == ref(j) )
        j += 1
      }

      mergeSort(arr,compare)
      mergeSort(ref,compare)

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

object SelectTests_mom    extends SelectTests( Select.mom[Double] )
object SelectTests_quick  extends SelectTests( Select.quick(_,_,_,_,_) )
object SelectTests_bubble extends SelectTests( Select.bubble[Double] )
object SelectTests_mean   extends SelectTests(
  (arr,i, compare, from,until) => {
    val toDouble: Double => Double
      = if( compare(-1,+1) < 0 ) x =>  x
        else                     x => -x
    Select mean(arr,toDouble,i, from,until)
  }
)
