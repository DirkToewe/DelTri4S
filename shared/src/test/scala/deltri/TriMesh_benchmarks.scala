package deltri

import java.awt.Desktop.getDesktop
import java.lang.System.nanoTime
import java.nio.file.Files
import java.util.Arrays
import java.util.Arrays.asList

import deltri.TriMeshTaped.{AddTri, DelTri}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object TriMesh_benchmarks
{
  def main( args: Array[String] ): Unit =
  {
    val orders = Map[String, () => TriMesh](
      "indexed" -> { () => TriMeshIndexed.empty },
      "mutable" -> { () => TriMeshMutable.empty },
    )

    val sizes = ArrayBuffer.empty[Int]
    val times   = orders map ( _._1 -> ArrayBuffer.empty[Double] )

    val rng = new Random()
    for( run <- 1 to 1024 )
    {
      val (x,y) = {
        val N = rng.nextInt(1024)
        val pts = Array.fill(N){(rng.nextDouble,rng.nextDouble)}
          .to[mutable.HashSet] // <- remove duplicate points
          .toArray
        ( Array.tabulate(pts.length){pts(_)._1},
          Array.tabulate(pts.length){pts(_)._2} )
      }
      assert(  x.length == y.length )
      sizes += x.length

      for( (name,emptyMesh) <- orders )
      {
        val t0 = nanoTime

        val mesh = emptyMesh()
        Delaunay.triangulate(mesh,x,y)

        val dt = nanoTime - t0

        assert( mesh.nNodes == x.length, s"${mesh.nNodes} != ${x.length}" )

        times  (name) += dt / 1e3
      }

      if( run % 4 == 0 )
        println(f"Run$run%4d check!")
    }

    def plot( title: String, data: Iterable[String] ) =
    {
      val plot = s"""
      |<!DOCTYPE html>
      |<html lang=\"en\">
      |  <head>
      |    <meta charset=\"utf-8\">
      |    <title>Mesh Comparison $title</title>
      |    <script type=\"text/javascript\" src=\"https://cdn.plot.ly/plotly-latest.min.js\"></script>
      |  </head>
      |  <body>
      |    <script type=\"text/javascript\">
      |    'use strict'; {
      |      let div = document.createElement('div');
      |      div.style = 'width: 100%; height: 1024px';
      |      div.innerHTML = 'Creating Plot...';
      |      document.body.appendChild(div);
      |
      |      const size = [${sizes mkString ", "}];
      |
      |      let
      |        data = [
      |          ${data mkString ",\n"}
      |        ],
      |        layout = { title: 'Mesh Comparison $title' };
      |      div.innerHTML = '';
      |      Plotly.plot(div, data, layout, { showLink: false, modeBarButtonsToRemove: ['sendDataToCloud'] });
      |    }
      |    </script>
      |  </body>
      |</html>
      """.stripMargin

      val tmp = Files.createTempFile("plot_",".html")
      Files.write(tmp, asList(plot) )
      getDesktop.browse(tmp.toUri)
    }

    plot(
      "Timings",
      for( (name,times) <- times )
        yield s"""
         |{
         |  type: 'scattergl', mode: 'markers', name: '$name',
         |  marker: { size: 4 },
         |  x: size,
         |  y: [${times map { x => f"$x%.3f" } mkString ", "}]
         |}
        """.stripMargin
    )
  }
}
