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
