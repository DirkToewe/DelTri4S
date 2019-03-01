package deltri

import java.util.Arrays

import deltri.TriMesh.{Node, angle, distance}
import deltri.TriMeshIndexed.{IndexedNode, delaunay}

import scala.collection.mutable
import scala.math.{sqrt => √}
import scala.util.Random

object RandomTriMesh
{
  def holeFreeUniform(
    nNodes: Int,
    x0: Double=0,
    y0: Double=0,
    dx: Double=1,
    dy: Double=1,
    rng: Random = new Random(),
    minAngle: Double =  20.0.toRadians,
    maxAngle: Double = 340.0.toRadians
  ): TriMeshIndexed =
  {
    val pts = Array.fill(nNodes){
      val ang = rng.nextDouble()*360.toRadians
      val rad = math sqrt rng.nextDouble()
      (math.cos(ang)*rad*dx + x0,
       math.sin(ang)*rad*dy + y0)
    }
    val (mesh,_) = delaunay(pts:_*)

    val deletables = mutable.HashSet.empty[(Node,Node,Node)]
    val `360°` = 360.toRadians

    def isDeletable( A: Node, B: Node, C: Node ): Boolean = {
      assert( A != B )
      assert( A != C )
      assert( B != C )

      if( angle(A,B,C) < minAngle ) return false

      def checkB(): Boolean = {
        var ang = `360°` - maxAngle
        var n = A
        while( ang > 0 ) {
          val m = mesh.adjacent(n,B).nodeOrNull
          if( m == null ) return false
          ang -= angle(B,m,n)
          n = m
        }
        true
      }
      def checkC(): Boolean = {
        var ang = `360°` - maxAngle
        var n = A
        while( ang > 0 ) {
          val m = mesh.adjacent(C,n).nodeOrNull
          if( m == null ) return false
          ang -= angle(C,n,m)
          n = m
        }
        true
      }
      if( ! checkB() ) return false
      if( ! checkC() ) return false

      // check for closed triangle fan around A
      var c = C
      while( c != B ) {
        c = mesh.adjacent(A,c).nodeOrNull
        if( null == c ) return false
      }
      true
    }

    def addDeletable( A: Node, B: Node, C: Node ): Unit
      = if( isDeletable(A,B,C) )
          deletables add {(A,B,C)}
        else {
          deletables remove {(A,B,C)}
          deletables remove {(B,C,A)}
          deletables remove {(C,A,B)}
        }

    mesh.tris foreach {
      (a,b,c) => if( ! mesh.adjacent(a,c).exists ) addDeletable(b,c,a)
            else if( ! mesh.adjacent(b,a).exists ) addDeletable(c,a,b)
            else if( ! mesh.adjacent(c,b).exists ) addDeletable(a,b,c)
    }

    while( ! deletables.isEmpty )
      deletables.iterator.takeWhile{
        case (a,b,c) =>
          if( isDeletable(a,b,c) ) { // <- TODO: add randomness to decision about removal
            deletables.remove{(a,b,c)}
            mesh.delTri(a,b,c)
            for( b <- mesh.adjacent(a,c) ) addDeletable(b,a,c)
            for( c <- mesh.adjacent(b,a) ) addDeletable(c,b,a)
            false
          }
          else true
      }.toSeq foreach deletables.remove

    mesh
  }

  def insertNodes( mesh: TriMesh, nNodes: Int, rng: Random = new Random() ): Unit =
  {
    implicit val _ = mesh
    // fetch triangles
    var tris: Tree = null
    mesh.tris foreach {
      (a,b,c) => {
        if( tris == null ) tris   = Leaf(a,b,c)
        else               tris ++= Leaf(a,b,c)
      }
    }
    for( _ <- 0 until nNodes )
    {
      assert( {1 << tris.height} <= mesh.nTris ) // <- check the Tree to be balanced
      if( rng.nextBoolean() ) tris = tris.insertRandInTri     (mesh,rng)
      else                    tris = tris.insertRandOnBoundary(mesh,rng)
    }
  }

  def generate( nNodesMin: Int = 16, nNodesMax: Int = 128, rng: Random = new Random() ): TriMeshImmutable =
  {
    assert( 2*nNodesMin < nNodesMax )
    val mesh = RandomTriMesh holeFreeUniform (
      rng.nextInt(nNodesMax/2 - nNodesMin)+nNodesMin,
      rng=rng,
      x0 = rng.nextDouble()*16 - 8,
      y0 = rng.nextDouble()*16 - 8,
      dx = rng.nextDouble()* 8 + 1e-3,
      dy = rng.nextDouble()* 8 + 1e-3
    )
    RandomTriMesh insertNodes (mesh, rng.nextInt(nNodesMax/2), rng)
    for( _ <- 0 until rng.nextInt(4) )
      RandomTriMesh digHole (mesh,rng)

    val nodes = mesh.nodes.toArray
    assert( nodes.zipWithIndex forall { case (IndexedNode(i,_,_),j) => i == j } )
    val pts = nodes map { n => (n.x,n.y) }

    val boundaries = for(
      boundary @ Seq(head,tail @ _*) <- mesh.boundaries;
      (a,b) <- boundary zip tail :+ head
    ) yield (a.index,b.index)

    val plc = PLC(
      pts,
      boundaries,
      false,
      boundaries
    )

    TriMeshImmutable( TriMeshIndexed.cdt(plc) )
  }

  def digHole(
    mesh: TriMesh,
    rng: Random = new Random(),
    minAngle: Double =  20.0.toRadians,
    maxAngle: Double = 340.0.toRadians
  ): Boolean =
  {
    def isDeletable( A: Node, B: Node, C: Node ): Boolean = {
      assert( A != B )
      assert( A != C )
      assert( B != C )

      if( angle(A,B,C) < minAngle ) return false

      def checkB(): Boolean = {
        var ang = 360.toRadians - maxAngle
        var n = A
        while( ang > 0 ) {
          val m = mesh.adjacent(n,B).nodeOrNull
          if( m == null ) return false
          ang -= angle(B,m,n)
          n = m
        }
        true
      }
      def checkC(): Boolean = {
        var ang = 360.toRadians - maxAngle
        var n = A
        while( ang > 0 ) {
          val m = mesh.adjacent(C,n).nodeOrNull
          if( m == null ) return false
          ang -= angle(C,n,m)
          n = m
        }
        true
      }
      if( ! checkB() ) return false
      if( ! checkC() ) return false

      // check for closed triangle fan around A
      var c = C
      while( c != B ) {
        c = mesh.adjacent(A,c).nodeOrNull
        if( null == c ) return false
      }
      true
    }

    object Queue {
      private val _queue = new Array[(Node,Node,Node)](mesh.nTris)
      private var _size  = 0

      // init
      {
        mesh.tris foreach {
          (a,b,c) =>
            if(
              isDeletable(a,b,c) &&
              isDeletable(b,c,a) &&
              isDeletable(c,a,b)
            ) {
              _queue(_size) = (a,b,c)
              _size += 1
            }
        }
        if(_size > 0 )
        {
          _queue(0) = _queue( rng nextInt _size )
          Arrays.fill(_queue.asInstanceOf[Array[AnyRef]], 1,_size, null)
          _size = 1
        }
      }

      def size = _size
      def push( a: Node, b: Node, c: Node ) =
      {
        _queue(_size) = (a,b,c)
        _size += 1
      }
      def pop(): (Node,Node,Node) =
      {
        assert(_size > 0 )
        val i = rng.nextInt(_size); _size -= 1
        val result = _queue(i); _queue(i) = _queue(size); _queue(size) = null
        result
      }
    }

    var success = false
    while( Queue.size > 0 ) {
      val (a,b,c) = Queue.pop()
      if( isDeletable(a,b,c) ) {
        mesh.delTri(a,b,c)
        success = true
        Queue.push(mesh.adjacent(a,c).nodeOrNull, a, c)
        Queue.push(mesh.adjacent(b,a).nodeOrNull, b, a)
      }
    }

    success
  }

  /** A left balanced, binary tree of triangles used to quickly insert new nodes randomly.
    */
  private[RandomTriMesh] sealed abstract class Tree
  {
    val len: Double
    val area: Double
    def height: Int
    def ++ ( tree: Tree ): Tree
      = if( this.height == tree.height )
          Branch(this,tree)
        else {
          assert( this.height > tree.height )
          val Branch(l,r) = this
          r ++ tree match {
            case r if l.height >= r.height => Branch(l,r)
            case Branch(rl,rr) =>
              val L = Branch(l,rl)
              Branch( L, rr )
          }
        }
    def insertRandOnBoundary( mesh: TriMesh, rng: Random ): Tree =
    {
      def insertRandomly( tree: Tree ): (Tree,Leaf)
        = tree match {
            case branch @ Branch(l,r) =>
              assert( branch.len > 0 )
              val rand = rng.nextDouble() * branch.len
              if( rand < l.len ) insertRandomly(l) match { case (l,tri1) => (Branch(l,r),tri1) }
              else               insertRandomly(r) match { case (r,tri1) => (Branch(l,r),tri1) }
            case leaf @ Leaf(a,b,c) =>
              def insert( a: Node, b: Node, c: Node ): (Tree,Leaf) =
              {
                assert( mesh.adjacent(b,a).nodeOrNull == null )
                val s = rng.nextDouble()*0.6 + 0.2
                val u = mesh.addNode(
                  a.x*(1-s) + b.x*s,
                  a.y*(1-s) + b.y*s
                )
                mesh.delTri(a,b,c)
                mesh.addTri(b,c,u)
                mesh.addTri(c,a,u)
                implicit val _ = mesh
                ( Leaf(b,c,u),
                  Leaf(c,a,u) )
              }
              assert( leaf.len > 0 )
              val rand = rng.nextDouble()*leaf.len
              val ac = mesh.adjacent(a,c).mapOrElse( _ => 0.0, distance(a,c) )
              val ba = mesh.adjacent(b,a).mapOrElse( _ => 0.0, distance(b,a) ) + ac
              if( rand < ac ) insert(c,a,b)
              else if( rand < ba ) insert(a,b,c)
              else                 insert(b,c,a)
          }
      insertRandomly(this) match {
        case (tree,tri1) => tree ++ tri1
      }
    }
    def insertRandInTri( mesh: TriMesh, rng: Random ): Tree =
    {
      val `√2` = √(2)
      def insertRandomly( tree: Tree ): (Tree,Leaf,Leaf)
        = tree match {
            case branch @ Branch(l,r) =>
              val rand = rng.nextDouble() * branch.area
              if( rand < l.area ) insertRandomly(l) match { case (l,tri1,tri2) => (Branch(l,r),tri1,tri2) }
              else                insertRandomly(r) match { case (r,tri1,tri2) => (Branch(l,r),tri1,tri2) }
            case Leaf(a,b,c) =>
              var p,q = rng.nextDouble()
              if( p+q > 1 ) {
                val pq = p+q
                p += √(2) * (1-pq)
                q += √(2) * (1-pq)
              }
              assert( p+q < 1 )
              // add safety margin
              val margin = 0.225
              p *= 1 - margin*(1 +`√2`); p += margin
              q *= 1 - margin*(1 +`√2`); q += margin
              val u = mesh.addNode(
                a.x*(1-p-q) + b.x*p + c.x*q,
                a.y*(1-p-q) + b.y*p + c.y*q
              )
              mesh.delTri(a,b,c)
              mesh.addTri(a,b,u)
              mesh.addTri(b,c,u)
              mesh.addTri(c,a,u)
              implicit val _ = mesh
              ( Leaf(a,b,u),
                Leaf(b,c,u),
                Leaf(c,a,u) )
          }
      insertRandomly(this) match {
        case (tree,tri1,tri2) => tree ++ tri1 ++ tri2
      }
    }
  }
  private[RandomTriMesh] final class Leaf private( override val len: Double, val a: Node, val b: Node, val c: Node ) extends Tree
  {
    override def height: Int = 0
    override val area = TriMesh.area(a,b,c)
  }
  object Leaf
  {
    def apply( a: Node, b: Node, c: Node )( implicit mesh: TriMesh ) =
    {
      val len
        = mesh.adjacent(a,c).mapOrElse( _ => 0.0, distance(a,c) ) +
          mesh.adjacent(b,a).mapOrElse( _ => 0.0, distance(b,a) ) +
          mesh.adjacent(c,b).mapOrElse( _ => 0.0, distance(c,b) )
      new Leaf(len,a,b,c)
    }

    class LeafExtractor( val leaf: Leaf ) extends AnyVal
    {
      def isEmpty = null == leaf
      def get = this
      def _1 = leaf.a
      def _2 = leaf.b
      def _3 = leaf.c
    }

    def unapply( leaf: Leaf )
      = new LeafExtractor(leaf)
  }
  private[RandomTriMesh] final case class Branch( l: Tree, r: Tree ) extends Tree
  {
    assert( l.height >= r.height   )
    l match {
      case Branch(l,r) => assert( l.height == r.height )
      case _ =>
    }
    override val len = l.len + r.len
    override val area = l.area + r.area
    override val height = r.height+1
  }
}
