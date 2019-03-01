package deltri

import java.awt.Desktop.getDesktop
import java.nio.file.Files
import java.util.Arrays.asList

import scala.util.Random

object CDT_experiments
{
  def main( args: Array[String] ): Unit =
  {
    val rng = new Random(1337)

    for( _ <- 1 to 4 )
    {
      val (plc,_) = CDT_tests.randPLC(rng)

      val _mesh = TriMeshTaped( TriMeshIndexed.empty() )
      CDT.triangulate(_mesh,plc)

      val tmp = Files.createTempFile("plot_",".html")
      Files.write( tmp, asList(_mesh.toHtml) )
      getDesktop.browse(tmp.toUri)
    }
  }
}
