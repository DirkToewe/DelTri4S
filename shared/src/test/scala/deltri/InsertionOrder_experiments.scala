package deltri

import java.awt.Desktop.getDesktop
import java.nio.file.Files
import java.util.Arrays.asList

import scala.collection.mutable
import scala.util.Random

object InsertionOrder_experiments
{
  def main( args: Array[String] ): Unit =
  {
    val rng = new Random(1337)

    val (x,y) = {
      val N = 2*1024
      val pts = Array.fill(N){(rng.nextDouble,rng.nextDouble)}
        .to[mutable.HashSet] // <- remove duplicate points
        .toArray
      ( Array.tabulate(pts.length){pts(_)._1},
        Array.tabulate(pts.length){pts(_)._2} )
    }
    assert( x.length == y.length )

    for( order <- Seq( InsertOrder hierarchicalZOrder      (_,_),
                       InsertOrder hierarchicalZOrderRandom(_,_,rng),
                       InsertOrder             zOrder      (_,_),
                       InsertOrder             zOrderSelect(_,_) )
    )
    {
      val mesh = TriMeshTaped(TriMeshIndexed.empty)
      Delaunay.triangulate(order)(mesh,x,y)

      assert( mesh.nNodes == x.length )

      val tmp = Files.createTempFile("plot_",".html")
      Files.write( tmp, asList(mesh.toHtml) )
      getDesktop.browse(tmp.toUri)
    }
  }
}
