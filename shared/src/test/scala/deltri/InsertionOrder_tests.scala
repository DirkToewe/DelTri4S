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
