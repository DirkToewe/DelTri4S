package deltri

import java.awt.Desktop.getDesktop
import java.nio.file.Files
import java.util.Arrays.asList

object RandTriMesh_experiments
{
  def main( args: Array[String] ) =
  {
    for( i <- 1 to 4 )
    {
      val mesh = RandomTriMesh.generate()

      val tmp = Files.createTempFile("plot_",".html")
      Files.write( tmp, asList(mesh.toHtml) )
      getDesktop.browse(tmp.toUri)
    }
  }
}
