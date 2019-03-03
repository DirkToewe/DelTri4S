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

import deltri.TriMesh.Node
import utest._

import scala.collection.immutable.{ IndexedSeq => ISeq }
import scala.collection.mutable
import scala.math.{cos, sin}
import scala.util.Random
import TriMesh.{ _inCircle, _orient2d }

class TriMeshTest( emptyMesh: () => TriMesh ) extends TestSuite
{
  override val tests = Tests{

    'area {
      val rng = new Random(1337)

      for( _ <- 1 to 1024*1024 )
      {
        val ax = rng.nextDouble()*16 - 8
        val ay = rng.nextDouble()*16 - 8
        val ab = rng.nextDouble()*8 + 1
        val ac = rng.nextDouble()*8 + 1
        val abAng = rng.nextDouble() * 360.toRadians
        val  ΔAng = rng.nextDouble() * 180.toRadians
        val acAng = abAng + ΔAng
        val     mesh = emptyMesh()
        val a = mesh.addNode(ax,                 ay                 )
        val b = mesh.addNode(ax + ab*cos(abAng), ay + ab*sin(abAng) )
        val c = mesh.addNode(ax + ac*cos(acAng), ay + ac*sin(acAng) )
        val areaSoll = 0.5*ab*ac*sin(ΔAng)
        val areaIst  = TriMesh.area(a,b,c)
        assert( { areaSoll - areaIst }.abs < 1e-9 )
      }
    }

    'angle {
      val rng = new Random(1337)
      for( _ <- 1 to 1024*1024 )
      {
        val ax = rng.nextDouble()*16 - 8
        val ay = rng.nextDouble()*16 - 8
        val ab = rng.nextDouble()*8 + 1
        val ac = rng.nextDouble()*8 + 1
        val abAng   = rng.nextDouble() * 360.toRadians
        val angSoll = rng.nextDouble() * 180.toRadians
        val acAng = abAng + angSoll
        val     mesh = emptyMesh()
        val a = mesh.addNode(ax,                 ay                 )
        val b = mesh.addNode(ax + ab*cos(abAng), ay + ab*sin(abAng) )
        val c = mesh.addNode(ax + ac*cos(acAng), ay + ac*sin(acAng) )
        val angIst  = TriMesh.angle(a,b,c)
        assert( { angSoll - angIst }.abs < 1e-9 )
      }
    }

    'inCircle1 {
      for( x <- -16 to 16 )
      for( y <- -16 to 16 ) {
        val     mesh = emptyMesh()
        val a = mesh addNode (x-1, y-1)
        val b = mesh addNode (x+1, y-1)
        val c = mesh addNode (x+1, y+1)
        val d = mesh addNode (x-1, y+1)

        assert( ! _inCircle(a,b,c)(d) )
        assert( ! _inCircle(b,c,a)(d) )
        assert( ! _inCircle(c,a,b)(d) )

        assert( ! _inCircle(b,c,d)(a) )
        assert( ! _inCircle(c,d,b)(a) )
        assert( ! _inCircle(d,b,c)(a) )

        assert( ! _inCircle(c,d,a)(b) )
        assert( ! _inCircle(d,a,c)(b) )
        assert( ! _inCircle(a,c,d)(b) )

        assert( ! _inCircle(d,a,b)(c) )
        assert( ! _inCircle(a,b,d)(c) )
        assert( ! _inCircle(b,d,a)(c) )
      }
    }

    'inCircle2 {
      val rng = new Random(1337)
      for( _ <- 1 to 128*1024 )
      {
        val mesh = emptyMesh()
        val d = mesh.addNode(
          rng.nextDouble*16 - 8,
          rng.nextDouble*16 - 8
        )
        val δ = 1e-4
        val R    = rng.nextDouble *   16 + δ
        val aAng = rng.nextDouble *  360.toRadians
        val bAng = rng.nextDouble * (180.toRadians-δ) + aAng + δ
        val cAng = rng.nextDouble * (180.toRadians-δ) + bAng + δ

        val Seq(a,b,c) = Seq(aAng,bAng,cAng) map {
          ang => mesh.addNode(
            d.x + R*cos(ang),
            d.y + R*sin(ang)
          )
        }

        assert( _inCircle(a,b,c)(d) )
        assert( _inCircle(b,c,a)(d) )
        assert( _inCircle(c,a,b)(d) )
      }
    }

    'inCircle3 {
      val rng = new Random(1337)
      for( _ <- 1 to 1024 )
      {
        val x,y = rng.nextDouble*16 + 1e-8

        val     mesh = emptyMesh()
        val a = mesh addNode (-x, -y)
        val b = mesh addNode (+x, -y)
        val c = mesh addNode (+x, +y)
        val d = mesh addNode (-x, +y)

        assert( ! _inCircle(a,b,c)(d) )
        assert( ! _inCircle(b,c,a)(d) )
        assert( ! _inCircle(c,a,b)(d) )

        assert( ! _inCircle(b,c,d)(a) )
        assert( ! _inCircle(c,d,b)(a) )
        assert( ! _inCircle(d,b,c)(a) )

        assert( ! _inCircle(c,d,a)(b) )
        assert( ! _inCircle(d,a,c)(b) )
        assert( ! _inCircle(a,c,d)(b) )

        assert( ! _inCircle(d,a,b)(c) )
        assert( ! _inCircle(a,b,d)(c) )
        assert( ! _inCircle(b,d,a)(c) )
      }
    }

    'orient2d {
      val rng = new Random(1337)
      for( _ <- 1 to 1024*1024 )
      {
        val mesh = emptyMesh()
        val x = rng.nextDouble*16 - 8
        val y = rng.nextDouble*16 - 8
        val R    = rng.nextDouble *  16
        val aAng = rng.nextDouble * 360.toRadians
        val bAng = rng.nextDouble * 180.toRadians + aAng
        val cAng = rng.nextDouble * 180.toRadians + bAng

        val Seq(a,b,c) = Seq(aAng,bAng,cAng) map {
          ang => mesh.addNode(
            x + R*cos(ang),
            y + R*sin(ang)
          )
        }

        assert( _orient2d(a,b,c) >= 0 )
        assert( _orient2d(b,c,a) >= 0 )
        assert( _orient2d(c,a,b) >= 0 )
      }
    }

    'segments {
      val rng = new Random(1337)
      for( _ <- 1 to 1024 )
      {
        val mesh = RandomTriMesh.holeFreeUniform(rng.nextInt(1024)+3, rng=rng)
        val segs = mutable.HashSet.empty[(Node,Node)]
        val tris = mesh.tris.toArray
        for( (a,b,c) <- tris )
        {
          assert( mesh.hasEdge(a,b) ); assert( mesh.hasEdge(b,a) )
          assert( mesh.hasEdge(b,c) ); assert( mesh.hasEdge(c,b) )
          assert( mesh.hasEdge(c,a) ); assert( mesh.hasEdge(a,c) )
          if( ! segs.contains{(a,b)} && rng.nextDouble() < 0.05 ) { mesh.addSegment(a,b); segs += ( (a,b), (b,a) ) }
          if( ! segs.contains{(b,c)} && rng.nextDouble() < 0.05 ) { mesh.addSegment(b,c); segs += ( (b,c), (c,b) ) }
          if( ! segs.contains{(c,a)} && rng.nextDouble() < 0.05 ) { mesh.addSegment(c,a); segs += ( (c,a), (a,c) ) }
          if( ! segs.contains{(a,b)} && rng.nextDouble() < 0.05 ) { mesh.addSegment(b,a); segs += ( (a,b), (b,a) ) }
          if( ! segs.contains{(b,c)} && rng.nextDouble() < 0.05 ) { mesh.addSegment(c,b); segs += ( (b,c), (c,b) ) }
          if( ! segs.contains{(c,a)} && rng.nextDouble() < 0.05 ) { mesh.addSegment(a,c); segs += ( (c,a), (a,c) ) }
        }
        RandomTriMesh.insertNodes(mesh, rng.nextInt(128), rng=rng)
        for( (a,b,c) <- tris )
        {
          assert( segs.contains{(a,b)} == mesh.hasSegment(a,b) )
          assert( segs.contains{(b,c)} == mesh.hasSegment(b,c) )
          assert( segs.contains{(c,a)} == mesh.hasSegment(c,a) )
          assert( segs.contains{(b,a)} == mesh.hasSegment(b,a) )
          assert( segs.contains{(c,b)} == mesh.hasSegment(c,b) )
          assert( segs.contains{(a,c)} == mesh.hasSegment(a,c) )
        }
      }
    }

    'adjacent {
      val mesh = emptyMesh()
      val a = mesh.addNode(0,0)
      val b = mesh.addNode(1,0)
      val c = mesh.addNode(1,1)
      val d = mesh.addNode(0,1)
      mesh.addTri(a,b,c)
      assert( mesh.adjacent(a,b).nodeOrNull == c )
      assert( mesh.adjacent(b,c).nodeOrNull == a )
      assert( mesh.adjacent(c,a).nodeOrNull == b )
      assert( mesh.adjacent(a,c).nodeOrNull == null )
      assert( mesh.adjacent(b,a).nodeOrNull == null )
      assert( mesh.adjacent(c,b).nodeOrNull == null )
      for( _ <- mesh.adjacent(a,c) ) assert(false)
      for( _ <- mesh.adjacent(b,a) ) assert(false)
      for( _ <- mesh.adjacent(c,b) ) assert(false)
      var node = null: TriMesh.Node
      for( x <- mesh.adjacent(a,b) ) node = x; assert(node == c)
      for( x <- mesh.adjacent(b,c) ) node = x; assert(node == a)
      for( x <- mesh.adjacent(c,a) ) node = x; assert(node == b)
      mesh.addTri(c,d,a)
      assert( mesh.adjacent(a,b).nodeOrNull == c )
      assert( mesh.adjacent(b,c).nodeOrNull == a )
      assert( mesh.adjacent(c,a).nodeOrNull == b )
      assert( mesh.adjacent(c,d).nodeOrNull == a )
      assert( mesh.adjacent(d,a).nodeOrNull == c )
      assert( mesh.adjacent(a,c).nodeOrNull == d )
    }

    'boundaries1 {
      val mesh = emptyMesh()
      val a = mesh addNode (0,0)
      val b = mesh addNode (1,0)
      val c = mesh addNode (1,1)
      val d = mesh addNode (0,1)
              mesh addTri (a,b,c)
              mesh addTri (c,d,a)

      val Seq(boundary) = mesh.boundaries
      assert( boundary(0) eq boundary.last )
      assert( boundary.length == 5 )
      assert( boundary == Seq(a,d,c,b,a)
           || boundary == Seq(b,a,d,c,b)
           || boundary == Seq(c,b,a,d,c)
           || boundary == Seq(d,c,b,a,d) )
    }

    'boundaries2 {
      for( off <- Seq(120,60,45,30,15) )
      {
        val mesh = emptyMesh()

        val pts = Vector range (0,360,off) map (_.toRadians) map {
          ang => ( Math cos ang,
                   Math sin ang )
        }
        val len = pts.length
        var outer = for( (x,y) <- pts ) yield mesh addNode (x*2, y*2)
        val inner = for( (x,y) <- pts ) yield mesh addNode (x*1, y*1)

        for(  i <- 0 until len ) {
          val j  = (i+1) % len
          mesh.addTri( inner(i), outer(i), inner(j) )
          mesh.addTri( outer(i), outer(j), inner(j) )
        }

        outer = outer.reverse

        val boundaries = mesh.boundaries
        assert( boundaries.length == 2 )

        def matches( boundary: Seq[Node], ref: Seq[Node] ): Boolean =
        {
          val i = ref indexOf boundary(0)
          if( i < 0 )
            false
          else {
            val reference = ref.drop(i) ++ ref.take(i) :+ ref(i)
            boundary == reference
          }
        }

        for( boundary <- boundaries ) {
          assert( boundary(0) == boundary.last )
          assert( boundary.length == len+1 )

          for( Seq(a,b) <- boundary.sliding(2) )
            assert( ! mesh.adjacent(a,b).exists )

          assert( matches(boundary,inner)
                ^ matches(boundary,outer) )
        }
      }
    }
  }
}

object TriMeshTest_indexed       extends TriMeshTest(                    TriMeshIndexed.empty  )
object TriMeshTest_indexed_taped extends TriMeshTest( () => TriMeshTaped(TriMeshIndexed.empty) )
object TriMeshTest_mutable       extends TriMeshTest( () =>              TriMeshMutable.empty  )
object TriMeshTest_mutable_taped extends TriMeshTest( () => TriMeshTaped(TriMeshMutable.empty) )
