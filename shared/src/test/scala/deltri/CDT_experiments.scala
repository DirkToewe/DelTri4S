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
      getDesktop.browse(tmp.toUri resolve "#-1")
    }
  }
}
