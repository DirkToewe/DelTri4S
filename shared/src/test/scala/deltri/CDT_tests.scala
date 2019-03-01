package deltri

import deltri.TriMesh.{Node, _inCircle}
import deltri.TriMeshIndexed.IndexedNode
import utest._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object CDT_tests extends TestSuite
{
  private[deltri] def randPLC( rng: Random ): (PLC,Int) =
  {
    var mesh: TriMesh{ type NodeType = IndexedNode }
      = RandomTriMesh.holeFreeUniform( rng.nextInt(64)+3 )
    mesh = TriMeshTaped(mesh)

    var nHoles = rng.nextInt(4)

    for( _ <- 1 to nHoles ) {
      RandomTriMesh.insertNodes(mesh,rng.nextInt(128)+32 )
      if( ! RandomTriMesh.digHole(mesh) )
        nHoles -= 1
    }
    RandomTriMesh.insertNodes(mesh,rng.nextInt(128)+32 )

    val nodes = mesh.nodes.toArray
    val coords = nodes map { n => (n.x,n.y) }
    val n2i = nodes.zipWithIndex.toMap
    val boundaries = mesh.boundaries
    val segments: Seq[(Int,Int)]
      = for(
          boundary @ Seq(head,tail @ _*) <- boundaries;
          (a,b) <- boundary zip tail :+ head
        ) yield n2i(a) -> n2i(b)

    val plc = PLC(
      coords,
      segments,
      false,
      boundaries
        .map( _ map {_.index} )
        .flatMap{ case bnd @ Seq(head,tail @ _*) => bnd zip tail :+head }
      //        boundaries.map{
      //          case Seq(
      //            IndexedNode(a,_,_),
      //            IndexedNode(b,_,_),
      //            _*
      //          ) => (a,b)
      //        }
    )

    (plc, nHoles)
  }

  override val tests = Tests{

    'main {

      val rng = new Random(1337)

      for( _ <- 1 to 1024 )
      {
        val (plc,nHoles) = randPLC(rng)

        val mesh = TriMeshIndexed.empty
        val nodes = CDT.triangulate(mesh,plc)

        val xy = plc.nodes.to[mutable.HashSet]
        assert( mesh.nNodes == plc.nNodes )
        mesh foreachNode {
          case Node(x,y) => assert( xy remove (x,y) )
        }
        assert( xy.size == 0 )

        val segs = plc.segments.flatMap{ case (i,j) => Seq(
          nodes(i) -> nodes(j),
          nodes(j) -> nodes(i)
        ) }.to[mutable.HashSet]

        mesh foreachTri {
          (a,b,c) =>
            val ac = segs contains (a,c)
            val ba = segs contains (b,a)
            val cb = segs contains (c,b)
            assert( mesh.hasSegment(a,c) == ac )
            assert( mesh.hasSegment(b,a) == ba )
            assert( mesh.hasSegment(c,b) == cb )
            if( ! ac ) for( d <- mesh.adjacent(a,c) ) { assert( ! _inCircle(a,b,c)(d) ) }
            if( ! ba ) for( d <- mesh.adjacent(b,a) ) { assert( ! _inCircle(a,b,c)(d) ) }
            if( ! cb ) for( d <- mesh.adjacent(c,b) ) { assert( ! _inCircle(a,b,c)(d) ) }
        }

        mesh foreachSegment {
          (m,n) =>
            assert( segs remove (m,n) )
            assert( segs remove (n,m) )
        }
        assert( segs.size == 0 )

        val boundary = mutable.HashMap.empty[Node,Node]
        mesh foreachTri {
          (a,b,c) =>
            if( ! mesh.adjacent(a,c).exists ) { assert( ! boundary.contains(a) ); boundary(a) = c }
            if( ! mesh.adjacent(b,a).exists ) { assert( ! boundary.contains(b) ); boundary(b) = a }
            if( ! mesh.adjacent(c,b).exists ) { assert( ! boundary.contains(c) ); boundary(c) = b }
        }

        for( _ <- 0 to nHoles )
        {
          val (start,_) = boundary.iterator.next
          @tailrec def loop( a: Node, b: Node ): Unit = {
            val Some(c) = boundary remove b

            if( b != start ) loop(b,c)
          }
          loop( start, boundary(start) )
        }
        assert( boundary.size == 0 )
      }
    }

  }
}