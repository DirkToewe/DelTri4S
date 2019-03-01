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

import scala.util.Random
import java.util.Arrays
import System.nanoTime
import java.awt.Desktop.getDesktop
import java.nio.file.Files
import java.util.Arrays.asList

import scala.collection.mutable.ArrayBuffer

object Select_benchmarks
{
  def main( args: Array[String] ): Unit =
  {
    implicit object ord extends ( (Double,Double) => Int ) {
      var comparisons = 0
      @inline override def apply( x: Double, y: Double ) = {
        comparisons += 1
        (x-y).signum
      }
    }

    val select_methods = Map[String, (Array[Double],Int) => Unit](
      "mean" -> { Select mean (_,_) },
      "mom"    -> { Select.mom   [Double](_,_, ord)  },
      "quick"  -> { Select.quick [Double](_,_, ord) },
//      "bubble" -> { Select.bubble[Double](_,_, ord) },
    )

    val sizes = ArrayBuffer.empty[Int]
    val times       = select_methods map {_._1 -> ArrayBuffer.empty[Double]}
    val comparisons = select_methods map {_._1 -> ArrayBuffer.empty[Int]   }

    val rng = new Random()
    for( run <- 1 to 1024 )
    {
      val VALUES = Array.fill( rng.nextInt(1024*1024)+1 )(rng.nextDouble)
//      val VALUES = Array.fill( rng.nextInt(1024*1024)+1 )( if(rng.nextBoolean) 1.0 else 0.0 )
      val i = rng.nextInt(VALUES.length)

      sizes += VALUES.length

      for( (name,select) <- select_methods )
      {
        ord.comparisons = 0

        val values, before = VALUES.clone

        val t0 = nanoTime
        select(values, i)
        val dt = nanoTime - t0

        times      (name) += dt
        comparisons(name) += ord.comparisons

        for( j <- 0 to i                ) assert( values(j) <= values(i) )
        for( j <- i until values.length ) assert( values(j) >= values(i) )

        Arrays sort before
        Arrays sort values

        for( i <- 0 until {before.length max values.length} )
          assert( before(i) == values(i) )
      }

      if( run % 1024 == 0 )
        println(f"Run$run%7d check!")
    }

    println("TIMES:")
    for( (k,v) <- times ) {
      printf("%8s:%7.3f\n", k, v.sum / 1e9)
    }
    println("COMPARISONS:")

    def plot( title: String, data: Iterable[String] ) =
    {
      val plot = s"""
      |<!DOCTYPE html>
      |<html lang=\"en\">
      |  <head>
      |    <meta charset=\"utf-8\">
      |    <title>Select Algorithm $title</title>
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
      |        layout = { title: 'Select Algorithm $title' };
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
        |  x: size,
        |  y: [${times map { x => f"$x%.3f" } mkString ", "}]
        |}
        """.stripMargin
    )

    plot(
      "Comparisons",
      for( (name,comparisons) <- comparisons )
        yield s"""
        |{
        |  type: 'scattergl', mode: 'markers', name: '$name',
        |  x: size,
        |  y: [${comparisons map { x => f"$x" } mkString ", "}]
        |}
        """.stripMargin
    )
  }
}
