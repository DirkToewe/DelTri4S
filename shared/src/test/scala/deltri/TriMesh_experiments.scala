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
import System.nanoTime

import scala.collection.mutable
import deltri.TriMeshIndexed.IndexedNode

object TriMesh_experiments
{
  def main( args: Array[String] ) =
  {
    val rng = new Random(1337)

//    {
////      val pts = Array.fill(128){(rng.nextDouble,rng.nextDouble)}.to[mutable.HashSet].toArray
//      val pts = Array.fill(1024){(rng.nextDouble,rng.nextDouble)}.to[mutable.HashSet].toArray
//      val taped = TriMeshTaped( TriMeshIndexed.empty() )
//      Delaunay.triangulate(taped)(pts:_*)
//
//      val tmp = Files.createTempFile("plot_",".html")
//      Files.write( tmp, asList(taped.toHtml) )
//      getDesktop.browse(tmp.toUri)
//    }

//    for( i <- 1 to 2 )
//    {
//      println(i)
//      val pts = Array.fill(1024){(rng.nextDouble,rng.nextDouble)}.to[mutable.HashSet].toArray
//      val segs = Array( (0,1),(1,2) )
//      val plc = PLC(pts,segs)
//      val mesh = TriMeshTaped( TriMeshIndexed.empty() )
//      try {
//        CDT.triangulate(mesh,plc)
//      } catch {
//        case err: Throwable =>
//          val tmp = Files.createTempFile("plot_",".html")
//          Files.write( tmp, asList(mesh.toHtml) )
//          getDesktop.browse(tmp.toUri)
//          throw err
//      }
//    }

    for( run <- 1 to 0 )
    {
      println(f"Run$run%4d")
      var mesh: TriMesh{ type NodeType = IndexedNode }
        = RandomTriMesh.holeFreeUniform(1024)
      mesh = TriMeshTaped(mesh)
      RandomTriMesh.insertNodes(mesh,1024)

//      {
//        val tmp = Files.createTempFile("plot_",".html")
//        Files.write( tmp, asList(mesh.toHtml) )
//        getDesktop.browse(tmp.toUri)
//      }

//      mesh = {
//        val reMesh = TriMeshTaped( TriMeshIndexed.empty() )
//        try {
//          Delaunay.triangulate(reMesh)( mesh.nodes.toArray.map{ n => (n.x,n.y) } :_* )
//        } catch {
//          case err: Throwable =>
//            val tmp = Files.createTempFile("plot_",".html")
//            Files.write( tmp, asList(reMesh.toHtml) )
//            getDesktop.browse(tmp.toUri)
//            throw err
//        }
//        reMesh.mesh.asInstanceOf[TriMeshIndexed]
//      }
      assert( RandomTriMesh digHole mesh )
      assert( RandomTriMesh digHole mesh )
      assert( RandomTriMesh digHole mesh )

//      for( boundary @ Seq(head,tail @ _*) <- mesh.boundaries )
//      {
//        println( boundary map (_.index) )
//        for( (a,b) <- boundary zip tail :+ head )
//        {
//          println{(a.index,b.index)}
//          mesh.addSegment(a,b)
//        }
//      }
//
//      val tmp = Files.createTempFile("plot_",".html")
//      Files.write( tmp, asList(mesh.toHtml) )
//      getDesktop.browse(tmp.toUri)

      val nodes = mesh.nodes.toArray
      val coords = nodes map { n => (n.x,n.y) }
      val n2i = nodes.zipWithIndex.toMap
      val boundaries = mesh.boundaries
      val segments: Seq[(Int,Int)]
        = for(
            boundary @ Seq(head,tail @ _*) <- boundaries;
            (a,b) <- boundary zip tail :+ head
          ) yield ( n2i(a), n2i(b) )

      val plc = PLC(
        coords,
        segments,
        false,
        boundaries
          .map( _ map { case IndexedNode(i,_,_) => i } )
          .flatMap{ case bnd @ Seq(head,tail @ _*) => bnd zip tail :+head }
//        boundaries.map{
//          case Seq(
//            IndexedNode(a,_,_),
//            IndexedNode(b,_,_),
//            _*
//          ) => (a,b)
//        }
      )

      val _mesh = TriMeshTaped( TriMeshIndexed.empty() )
      try {
        CDT.triangulate(_mesh,plc)

        val tmp = Files.createTempFile("plot_",".html")
        Files.write( tmp, asList(_mesh.toHtml) )
        getDesktop.browse(tmp.toUri)
      } catch {
        case err: Throwable =>
          val tmp = Files.createTempFile("plot_",".html")
          Files.write( tmp, asList(_mesh.toHtml) )
          getDesktop.browse(tmp.toUri)
          throw err
      }

//      {
//        val tmp = Files.createTempFile("plot_",".html")
//        Files.write( tmp, asList(_mesh.toHtml) )
//        getDesktop.browse(tmp.toUri)
//      }

//      {
//        val tmp = Files.createTempFile("plot_",".html")
//        Files.write( tmp, asList(_mesh.mesh.toHtml) )
//        getDesktop.browse(tmp.toUri)
//      }
    }

//    for( run <- 1 to 1024*1024 )
//    {
//      val len = rng.nextInt(1024)
//      val arr = Array.fill(len)(rng.nextInt)
//      val before = arr.toSet
//      InsertionZOrder.quickSort[Int](arr, _ < _)
////      InsertionZOrder.heapSort(arr, _ < _)
//      var i = arr.length-1
//      while( i > 0 ) {
//        i -= 1
//        assert( arr(i+1) >= arr(i) )
//      }
//      val after = arr.toSet
//      assert(before == after)
//      println(f"Run$run%4d check!")
//    }

//    val pts = Array.fill(128)(rng.nextDouble,rng.nextDouble).to[mutable.HashSet].toArray
//    val pts = Array.fill(16*1024){(rng.nextDouble,rng.nextDouble)}.to[mutable.HashSet].toArray
//    val mesh = TriMeshIndexed.delaunay(pts:_*)
//
//    val tmp = Files.createTempFile("plot_",".html")
//    Files.write( tmp, asList(mesh.toHtml) )
//    getDesktop.browse(tmp.toUri)

    for( i <- 1 to 1 )
    {
      val (x,y) = {
//        val N = 1000*1000
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
        val t0 = nanoTime

        val mesh = TriMeshTaped(TriMeshIndexed.empty)
        Delaunay.triangulate(order)(mesh,x,y)

        val dt = (nanoTime - t0) / 1e9

        assert( mesh.nNodes == x.length )

        val tmp = Files.createTempFile("plot_",".html")
        Files.write( tmp, asList(mesh.toHtml) )
        getDesktop.browse(tmp.toUri)

        println(f"$dt%.3f seconds")
      }
    }

//    val mesh = TriMesh.delaunay(
//      (0,0),
//      (1,0),
//      (1,1),
//      (0,1)
//    )
  }
}
