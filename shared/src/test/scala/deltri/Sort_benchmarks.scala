package deltri

import java.awt.Desktop.getDesktop
import java.lang.System.nanoTime
import java.nio.file.Files
import java.util.Arrays
import java.util.Arrays.asList

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Sort_benchmarks
{
  def main( args: Array[String] ): Unit =
  {
    implicit object ord extends ( (Double,Double) => Int ) with Ordering[Double] {
      var comparisons = 0
      override def apply( x: Double, y: Double ) = {
        comparisons += 1
        (x - y).signum
      }
      override def compare( x: Double, y: Double )
        = this(x,y)
    }

    val sort_methods = Map[String, (Array[Double], (Double,Double) => Int) => Array[Double]](
      "scala" -> { (arr,_) => arr.sorted },
      "heap"         -> ( (x,ord) => {Sort.        heapSort(x,ord); x} ),
      "mean"         -> ( (x, _ ) => {Sort.        meanSort(x    ); x} ),
      "merge"        -> ( (x,ord) => {Sort.       mergeSort(x,ord); x} ),
      "quick"        -> ( (x,ord) => {Sort.       quickSort(x,ord); x} ),
//      "binary"       -> ( (x,ord) => {Sort.      binarySort(x,ord); x} ),
//      "insert"       -> ( (x,ord) => {Sort.      insertSort(x,ord); x} ),
      "smooth"       -> ( (x,ord) => {Sort.      smoothSort(x,ord); x} ),
      "classicMerge" -> ( (x,ord) => {Sort.classicMergeSort(x,ord); x} ),
      "JDK"          -> ( (x, _ ) => {          Arrays.sort(x    ); x} )
    )

    val sizes = ArrayBuffer.empty[Int]
    val times       = sort_methods map ( _._1 -> ArrayBuffer.empty[Double] )
    val comparisons = sort_methods map ( _._1 -> ArrayBuffer.empty[Int]    )

    val rng = new Random()
    for( run <- 1 to 1024 )
    {
//      val VALUES = Array.fill[Double](15)( rng.nextInt(32) )
      val VALUES = Array.tabulate[Double]( rng.nextInt(1024*1024)+1 )( _ => rng.nextDouble )
//      val VALUES = Array.fill( rng.nextInt(1024*1024)+1 )( if(rng.nextBoolean) 1.0 else 0.0 )
      val i = rng.nextInt(VALUES.length)

      sizes += VALUES.length

      for( (name,sort) <- sort_methods )
      {
        ord.comparisons = 0

        var values    = VALUES.clone
        val reference = VALUES.clone

        val t0 = nanoTime
        values = sort(values, ord)
        val dt = nanoTime - t0

        times      (name) += dt / 1e9
        comparisons(name) += ord.comparisons

        Arrays sort reference

        for( i <- 0 until values.length )
          assert( reference(i) == values(i) )
      }

      if( run % 16 == 0 )
        println(f"Run$run%4d check!")
    }

    def plot( title: String, data: Iterable[String] ) =
    {
      val plot = s"""
      |<!DOCTYPE html>
      |<html lang=\"en\">
      |  <head>
      |    <meta charset=\"utf-8\">
      |    <title>Sorting Algorithm $title</title>
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
      |        layout = { title: 'Sorting Algorithm $title' };
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

    plot(
      "Comparisons",
      for( (name,comparisons) <- comparisons )
        yield s"""
         |{
         |  type: 'scattergl', mode: 'markers', name: '$name',
         |  marker: { size: 4 },
         |  x: size,
         |  y: [${comparisons map { x => f"$x" } mkString ", "}]
         |}
        """.stripMargin
    )
  }
}
