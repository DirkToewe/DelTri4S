package deltri

import java.awt.Desktop.getDesktop
import java.nio.file.Files
import java.util.Arrays.asList

import scala.util.Random

object Readme_examples
{
  def main( args: Array[String] ): Unit =
  {
    example_create1()
    example_create2()
    example_traverse1()
    example_traverse2()
    example_traverse3()
    example_delaunay1()
    example_cdt1()
    example_cdt2()
    example_cdt3()
  }

  def example_create1() =
  {
    val mesh = TriMeshIndexed.empty
    val a = mesh.addNode(0,0)
    val b = mesh.addNode(2,0)
    val c = mesh.addNode(1,1)
    mesh.addTri(a,b,c)
    val d = mesh.addNode(0,0.4)
    mesh.addTri(a,c,d)
    val e = mesh.addNode(0,0.8)
    mesh.addTri(d,c,e)
    mesh.addSegment(d,c)

    val tmp = Files.createTempFile("example1_",".html")
    Files.write( tmp, asList(mesh.toHtml) )
    getDesktop.browse(tmp.toUri resolve "#-1")
  }

  def example_create2() =
  {
    val mesh1 = TriMeshImmutable.empty
    val(mesh2,a) = mesh1.addedNode(0,0)
    val(mesh3,b) = mesh2.addedNode(2,0)
    val(mesh4,c) = mesh3.addedNode(1,1)
    val(mesh5,d) = mesh4.addedNode(0,0.4)
    val(mesh6,e) = mesh5.addedNode(0,0.8)
    val mesh7    = mesh6.addedTri(a,b,c)
                        .addedTri(a,c,d)
                        .addedTri(d,c,e)
                        .addedSegment(d,c)

    val tmp = Files.createTempFile("example2_",".html")
    Files.write( tmp, asList(mesh7.toHtml) )
    getDesktop.browse(tmp.toUri resolve "#-1")
  }

  def example_traverse1(): Unit =
  {
    val     mesh = TriMeshIndexed.empty()
    val a = mesh addNode (-1, -1)
    val b = mesh addNode (+1, -1)
    val c = mesh addNode (+1, +1)
    val d = mesh addNode (-1, +1)
    mesh addTri (a,b,c)
    mesh addTri (c,d,a)

    assert( mesh.adjacent(a,c).exists )
    assert( mesh.adjacent(a,c).nodeOrNull  eq  d )

    for( e <- mesh.adjacent(a,b) )
      assert( e eq c )

    val tmp = Files.createTempFile("example3_",".html")
    Files.write( tmp, asList(mesh.toHtml) )
    getDesktop.browse(tmp.toUri resolve "#-1")
  }

  def example_traverse2(): Unit =
  {
    val     mesh = TriMeshIndexed.empty()
    val a = mesh addNode (-1, -1)
    val b = mesh addNode (+1, -1)
    val c = mesh addNode (+1, +1)
    val d = mesh addNode (-1, +1)
    mesh addTri (a,b,c)
    mesh addTri (c,d,a)

    mesh foreachTri {
      (a,b,c) =>
        printf( "Tri(\n  %s,\n  %s,\n  %s\n)\n", a,b,c )
    }
  }

  def example_traverse3(): Unit =
  {
    val     mesh = TriMeshIndexed.empty()
    val a = mesh addNode (-1, -1)
    val b = mesh addNode (+1, -1)
    val c = mesh addNode (+1, +1)
    val d = mesh addNode (-1, +1)
    mesh addTri (a,b,c)
    mesh addTri (c,d,a)

    mesh.foreachTriAround(a){
      (b,c) =>
        printf( "Tri(\n  %s,\n  %s,\n  %s\n)\n", a,b,c )
    }
  }

  def example_delaunay1() =
  {
    val rng = new Random(1337)
    val points = Array.tabulate(128){
      _ => (rng.nextDouble*2-1,
            rng.nextDouble*2-1)
    }.toMap.toSeq // <- remove duplicates

    val (mesh,nodes) = TriMeshTaped.delaunay(points: _*)

    val tmp = Files.createTempFile("example4_",".html")
    Files.write( tmp, asList(mesh.toHtml) )
    getDesktop.browse(tmp.toUri resolve "#-1")
  }

  def example_cdt1() =
  {
    val center = (0.0, 0.0)
    val innerCircle = Vector range (45,405,90) map (_.toRadians) map {
      angle => ( 0.5 * Math.cos(angle),
                 0.5 * Math.sin(angle) )
    }
    val outerCircle = Vector range (45,405,90) map (_.toRadians) map {
      angle => ( 1 * Math.cos(angle),
                 1 * Math.sin(angle) )
    }

    val plc = PLC(
      nodes     = (innerCircle :+ center) ++ outerCircle,
      segments  =  innerCircle.indices map { i => i -> (i+1) % innerCircle.length },
      holeNodes = Set(innerCircle.length) // <- put a hole in the center
    )

    val (mesh, meshNodes) = TriMeshTaped.delaunayConstrained(plc)

    val tmp = Files.createTempFile("example5_",".html")
    Files.write( tmp, asList(mesh.toHtml) )
    getDesktop.browse(tmp.toUri resolve "#-1")
  }

  def example_cdt2() =
  {
    val innerCircle = Vector range (0,360,120) map (_.toRadians) map {
      angle => ( 0.5 * Math.cos(angle),
        0.5 * Math.sin(angle) )
    }
    val outerCircle = Vector range (0,360,120) map (_.toRadians) map {
      angle => ( 1 * Math.cos(angle),
        1 * Math.sin(angle) )
    }

    val plc = PLC(
      nodes     = innerCircle ++ outerCircle,
      segments  = innerCircle.indices map { i => i -> (i+1) % innerCircle.length },
      orientedBoundarySegments = Seq( (0,1) )
    )

    val (mesh, meshNodes) = TriMeshTaped.delaunayConstrained(plc)

    val tmp = Files.createTempFile("example6_",".html")
    Files.write( tmp, asList(mesh.toHtml) )
    getDesktop.browse(tmp.toUri resolve "#-1")
  }

  def example_cdt3() =
  {
    val innerCircle = Vector range ( 0,360,30) map (_.toRadians) map {
      angle => ( 0.6 * Math.cos(angle),
                 0.6 * Math.sin(angle) )
    }
    val outerCircle = Vector range (15,375,30) map (_.toRadians) map {
      angle => ( 1 * Math.cos(angle),
                 1 * Math.sin(angle) )
    }
    val n      = innerCircle.length
    val points = innerCircle ++ outerCircle

    val segments = innerCircle.indices flatMap { i => Seq(
       i    -> (i+n),
      (i+n) -> (i+1) % n
    )}

    val plc = PLC(
      points,
      segments,
      confinedBySegments = true
    )

    val (mesh, nodes) = TriMeshTaped.delaunayConstrained(plc)

    val tmp = Files.createTempFile("example7_",".html")
    Files.write( tmp, asList(mesh.toHtml) )
    getDesktop.browse(tmp.toUri resolve "#-1")
  }
}
