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

import deltri.TriMesh.{Node, NodeMaybe, _orient2d}
import deltri.TriMeshImmutable._
import deltri.TriMeshIndexed.IndexedNode
import deltri.TriMeshTaped._

import scala.annotation.tailrec
import scala.collection.immutable.{HashSet, IntMap, IndexedSeq => ISeq}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TriMeshImmutable private(
  val nTris: Int,
  val nSegments: Int,
  val _triSeg: IntMap[V]
)
{
  def nNodes: Int = _triSeg.size

  def boundaries: ISeq[ISeq[IndexedNode]] =
  {
    val edgeMap = mutable.HashMap.empty[(IndexedNode,IndexedNode),IndexedNode]

    for( (ai,(a,bcs,_)) <- _triSeg )
    for( (bi,c @ IndexedNode(ci,_,_)) <- bcs )
      if( ! bcs.contains(ci) )
      {
        val (_,des,_) = _triSeg(ci)

        @inline @tailrec def loop( d: IndexedNode ): IndexedNode
          = des getOrElse (d.index,null) match {
              case null => d
              case    d => loop(d)
            }
        edgeMap{(a,c)} = loop( _triSeg(bi)._1 )
      }

    @inline @tailrec def nextBorder( border: ISeq[IndexedNode], a: IndexedNode, b: IndexedNode ): ISeq[IndexedNode] = {
      val newBorder = border :+ b
      edgeMap remove (a,b) match {
        case None    =>            newBorder
        case Some(d) => nextBorder(newBorder, b,d)
      }
    }

    var borders = ISeq.empty[ISeq[IndexedNode]]

    while( edgeMap.nonEmpty ) {
      val (a,b) = edgeMap.keysIterator.next()
      borders :+= nextBorder(Vector.empty, a, b)
    }

    borders
  }

  private def _hasAdjacent( a: Int, b: Int ): Boolean
    = _triSeg(a)._2 contains b

  def foreachNode[U]( consumer: IndexedNode => U ): Unit
    = _triSeg foreachValue { case (n,_,_) => consumer(n) }

  def foreachTri[U]( consumer: (IndexedNode,IndexedNode,IndexedNode) => U ): Unit
    = for( (ai,(a,bcs,_)) <- _triSeg )
      for( (bi,c) <- bcs )
        if( ai < bi && ai < c.index )
        {
          val b = _triSeg(bi)._1
          consumer(a,b,c)
        }

  def foreachTriPar[U]( consumer: (IndexedNode,IndexedNode,IndexedNode) => U ): Unit
    = for( (ai,(a,bcs,_)) <- _triSeg.par )
      for( (bi,c) <- bcs )
        if( ai < bi && ai < c.index )
        {
          val b = _triSeg(bi)._1
          consumer(a,b,c)
        }

  def foreachTriAround[U]( node: Node )( consumer: (IndexedNode,IndexedNode) => U ): Unit
    = node match {
        case IndexedNode(ai,_,_) =>
          val (`node`,tri,_) = _triSeg(ai)
          for( (bi,c) <- tri ) {
            val (b,_,_) = _triSeg(bi)
            consumer(b,c)
          }
      }

  def foreachSegment[U]( consumer: (IndexedNode,IndexedNode) => U ): Unit
    = _triSeg.foreachValue{
        case (a,_,bs) => for( b <- bs ) consumer(a,b)
      }

  def foreachSegmentAround[U]( node: Node )( consumer: IndexedNode => U ): Unit
    = node match {
        case IndexedNode(ai,_,_) =>
          val  (`node`,_,seg) = _triSeg(ai)
          for( b <- seg ) consumer(b)
      }

  def hasEdge( a: Node, b: Node ): Boolean =
    a match { case IndexedNode(ai,_,_) => val (an,triA,_) = _triSeg.getOrElse(ai,_emptyVal); an == a && (
    b match { case IndexedNode(bi,_,_) => val (bn,triB,_) = _triSeg.getOrElse(bi,_emptyVal); bn == b && (
      triA.contains(bi) ||
      triB.contains(ai)
    ) case _ => false }
    ) case _ => false }

  def adjacent( a: Node, b: Node ): NodeMaybe[IndexedNode] =
    a match { case IndexedNode(ai,_,_) =>
    b match { case IndexedNode(bi,_,_) =>
      val (`a`,tri,_) = _triSeg(ai)
      new NodeMaybe( tri.getOrElse(bi,null) )
    }}

  def hasNode( node: Node ): Boolean
    = node match {
        case IndexedNode(i,_,_) => _triSeg.getOrElse(i,_emptyVal)._1 == node
        case _ => false
      }

  def addedNode[O]( x: Double, y: Double ): (TriMeshImmutable,IndexedNode)
    = addedNode(x,y, (_,_))

  @inline def addedNode[O]( x: Double, y: Double, consumer: (TriMeshImmutable,IndexedNode) => O ): O = {
    val index = try{ _triSeg.lastKey+1 } catch { case _: RuntimeException => 0 }
    val node = new IndexedNode(index,x,y)
    val triSeg = _triSeg updateWith ( index, _emptyVal(node), (_,_) => throw new AssertionError() )
    consumer( new TriMeshImmutable(nTris, nSegments, triSeg), node )
  }

  def deletedNode( node: Node ): TriMeshImmutable
    = node match {
        case a @ IndexedNode(ai,_,_) =>
          val (`a`,tri,seg) = _triSeg(ai)
          var triSeg = _triSeg - ai
          for( (bi,c) <- tri ) {
            val ci = c.index
            triSeg = triSeg
              .updateWith[V](bi, null, { case (( b ,tri,seg),_) => ( b, tri - ci, seg) } )
              .updateWith[V](ci, null, { case ((`c`,tri,seg),_) => ( c, tri - ai, seg) } )
          }
          for( b @ IndexedNode(bi,_,_) <- seg )
            if( bi < ai ) {
              triSeg = triSeg.updateWith[V](bi, null, { case ((`b`,tri,seg),_) => ( b, tri, seg - a) } )
            }
          new TriMeshImmutable( nTris, nSegments, triSeg )
      }

  def hasTri( a: Node, b: Node, c: Node ): Boolean
    = a match { case IndexedNode(ai,_,_) => _triSeg.getOrElse(ai,_emptyVal)._1 == a && (
      b match { case IndexedNode(bi,_,_) => _triSeg.getOrElse(bi,_emptyVal)._1 == b && (
        _triSeg.getOrElse(ai,_emptyVal)._2.getOrElse(bi,null) == c
      ) case _ => false }
      ) case _ => false }

  def addedTri( a: Node, b: Node, c: Node ): TriMeshImmutable
    = a match { case a @ IndexedNode(ai,_,_) =>
      b match { case b @ IndexedNode(bi,_,_) =>
      c match { case c @ IndexedNode(ci,_,_) =>
        assert( ! (_orient2d(a,b,c) <= 0), f"Bad orientation (${_orient2d(a,b,c)}) for triangle:\n  $a\n  $b\n  $c" )
        new TriMeshImmutable(
          nTris + 1,
          nSegments,
          _triSeg
            .updateWith[V]( ai, null, { case ((`a`,triA,segA),_) => ( a, triA updated (bi,c) ensuring (_.size == triA.size+1), segA ) } )
            .updateWith[V]( bi, null, { case ((`b`,triB,segB),_) => ( b, triB updated (ci,a) ensuring (_.size == triB.size+1), segB ) } )
            .updateWith[V]( ci, null, { case ((`c`,triC,segC),_) => ( c, triC updated (ai,b) ensuring (_.size == triC.size+1), segC ) } )
        )
      }}}

  def deletedTri( a: Node, b: Node, c: Node ): TriMeshImmutable
    = a match { case a @ IndexedNode(ai,_,_) =>
      b match { case b @ IndexedNode(bi,_,_) =>
      c match { case c @ IndexedNode(ci,_,_) =>
        new TriMeshImmutable(
          nTris - 1,
          nSegments,
          _triSeg
            .updateWith[V]( ai, null, { case ((`a`,triA,segA),_) => ( a, triA - bi, segA ) ensuring triA(bi) == c } )
            .updateWith[V]( bi, null, { case ((`b`,triB,segB),_) => ( b, triB - ci, segB ) ensuring triB(ci) == a } )
            .updateWith[V]( ci, null, { case ((`c`,triC,segC),_) => ( c, triC - ai, segC ) ensuring triC(ai) == b } )
        )
      }}}

  def hasSegment( a: Node, b: Node ): Boolean
    = a match { case a @ IndexedNode(ai,_,_) => val (an,_,segA) = _triSeg.getOrElse(ai,_emptyVal); an == a && (
      b match { case b @ IndexedNode(bi,_,_) => val (bn,_,segB) = _triSeg.getOrElse(bi,_emptyVal); bn == b && (
        if( ai < bi ) segA.contains(b)
        else          segB.contains(a)
      ) case _ => false }
      ) case _ => false }

  def addedSegment( a: Node, b: Node ): TriMeshImmutable
    = a match { case a @ IndexedNode(ai,_,_) =>
      b match { case b @ IndexedNode(bi,_,_) =>
        val (`a`,triA,segA) = _triSeg(ai)
        val (`b`,triB,segB) = _triSeg(bi)
        val triSeg = if( ai < bi ) _triSeg updated ( ai, (a,triA,segA + b ensuring {_.size == segA.size+1} ) )
                     else          _triSeg updated ( bi, (b,triB,segB + a ensuring {_.size == segB.size+1} ) )
        new TriMeshImmutable(nTris, nSegments + 1, triSeg )
      }}

  def deletedSegment( a: Node, b: Node ): TriMeshImmutable
    = a match { case a @ IndexedNode(ai,_,_) =>
      b match { case b @ IndexedNode(bi,_,_) =>
        val (`a`,triA,segA) = _triSeg(ai)
        val (`b`,triB,segB) = _triSeg(bi)
        val triSeg = if( ai < bi ) _triSeg.updated( ai, (a,triA,segA - b ensuring {_.size == segA.size-1} ) )
                     else          _triSeg.updated( bi, (b,triB,segB - a ensuring {_.size == segB.size-1} ) )
        new TriMeshImmutable(nTris, nSegments-1, triSeg)
      }}

  def toHtml: String = {
    val changes = ArrayBuffer.empty[Change]
    foreachNode{ changes += AddNode(_) }
    foreachTri{ changes += AddTri(_,_,_) }
    foreachSegment{ changes += AddSegment(_,_) }
    TriMeshTaped.toHtml(changes)
  }
}
object TriMeshImmutable
{
  type V = (IndexedNode, IntMap[IndexedNode], HashSet[IndexedNode])

  val empty = new TriMeshImmutable(0,0, IntMap.empty)

  def delaunay( nodes: (Double,Double)* ): (TriMeshImmutable,Array[Node])
    = delaunay(
        nodes.view.map(_._1).toArray,
        nodes.view.map(_._2).toArray
      )

  def delaunay( x: Array[Double], y: Array[Double] ): (TriMeshImmutable,Array[Node]) =
  {
    val mut = TriMeshMutable.empty()
    val nodes = Delaunay.triangulate(mut,x,y)
    (mut.mesh,nodes)
  }

  def delaunayConstrained( plc: PLC ): (TriMeshImmutable,Array[Node]) =
  {
    val mut = TriMeshMutable.empty()
    val nodes = CDT.triangulate(mut,plc)
    (mut.mesh,nodes)
  }

  def apply( triMesh: TriMesh ): TriMeshImmutable
    = triMesh match {
        case TriMeshMutable(mesh) => mesh
        case _ =>
          val mut = TriMeshMutable.empty()
          val nodeMap = mutable.HashMap.empty[Node,IndexedNode]
          triMesh foreachNode {
            case n @ Node(x,y) => nodeMap(n) = mut.addNode(x,y)
          }
          triMesh foreachTri {
            (a,b,c) => mut.addTri(
              nodeMap(a),
              nodeMap(b),
              nodeMap(c)
            )
          }
          triMesh foreachSegment {
            (a,b) => mut.addSegment(
              nodeMap(a),
              nodeMap(b)
            )
          }
          mut.mesh
      }

  private[TriMeshImmutable] val _emptyVal: V
    = ( null, IntMap.empty, HashSet.empty )

  private[TriMeshImmutable] def _emptyVal( node: IndexedNode ): V
    = ( node, IntMap.empty, HashSet.empty )
}
