/* IMPLEMENTATION DISCARDED DUE TO INFERIOR PERFORMANCE
 */
package deltri

import deltri.TriMesh.{Node, NodeMaybe}
import deltri.TriMeshImmutableHashed._
import deltri.TriMeshTaped._

import scala.collection.immutable.{HashMap, HashSet, IndexedSeq => ISeq}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TriMeshImmutableHashed private(
  val nTris: Int,
  val nSegments: Int,
  val _triSeg: HashMap[ Node,( HashMap[Node,Node], HashSet[Node] ) ]
)
{
  def nNodes: Int = _triSeg.size

  def nodes: Iterable[Node] = _triSeg.keys

  def boundaries: ISeq[ISeq[Node]] = {
    var boundaries = ISeq.empty[ISeq[Node]]
    val segMap = mutable.HashMap.empty[Node,Node]

    foreachTri{
      (a,b,c) =>
        if( ! _hasAdjacent(a,c) ) segMap(c) = a
        if( ! _hasAdjacent(b,a) ) segMap(a) = b
        if( ! _hasAdjacent(c,b) ) segMap(b) = c
    }

    while( ! segMap.isEmpty ) {
      var boundary = ISeq.empty[Node]
      var (n,_) = segMap.iterator.next() // <- FIXME: this may be inefficient
      while( segMap contains n ) {
        n = segMap(n)
        boundary :+= n
      }
      boundaries :+= boundary
    }

    boundaries
  }

  private def _hasAdjacent( a: Node, b: Node ): Boolean
    = _triSeg(a)._1 contains b

  def foreachNode[U]( consumer: Node => U ): Unit
    = nodes foreach consumer

  def foreachTri[U]( consumer: (Node,Node,Node) => U ): Unit =
  {
    val visited = mutable.HashSet.empty[(Node,Node,Node)]
    for( (a,(bcs,_)) <- _triSeg )
    for( (b,c) <- bcs )
      if( ! visited.contains{(b,c,a)} &&
          ! visited.contains{(c,a,b)} )
        consumer(a,b,c)
  }

  def foreachTriAround[U]( node: Node )( consumer: (Node,Node) => U ): Unit
    = for( (b,c) <- _triSeg(node)._1 )
        consumer(b,c)

  def foreachSegment[U]( consumer: (Node,Node) => U ): Unit
    = for( (a,(_,bs)) <- _triSeg )
      for(  b <- bs )
        consumer(a,b)

  def hasEdge( a: Node, b: Node )
    = _triSeg.getOrElse(a,_emptyVal)._1.contains(b) ||
      _triSeg.getOrElse(b,_emptyVal)._1.contains(a)

  def adjacent( a: Node, b: Node ): NodeMaybe[Node]
    = new NodeMaybe( _triSeg(a)._1.getOrElse(b,null) )

  def hasNode( node: Node ): Boolean
    = _triSeg contains node

  def addedNode( x: Double, y: Double ): (TriMeshImmutableHashed,Node) = {
    val node = new NodeImpl(x,y)
    val triSeg = _triSeg updated (node,_emptyVal)
    ( new TriMeshImmutableHashed(nTris, nSegments, triSeg), node )
  }

  def deletedNode( node: Node ): TriMeshImmutableHashed
    = new TriMeshImmutableHashed( nTris, nSegments, _triSeg - node  ensuring  {_.size+1 == _triSeg.size} )

  def hasTri( a: Node, b: Node, c: Node ): Boolean
    = _triSeg.getOrElse(a,_emptyVal)._1
             .getOrElse(b,null) == c

  def addedTri( a: Node, b: Node, c: Node ): TriMeshImmutableHashed = {
    val (triA,segA) = _triSeg(a)
    val (triB,segB) = _triSeg(b)
    val (triC,segC) = _triSeg(c)
    new TriMeshImmutableHashed(
      nTris + 1,
      nSegments,
      _triSeg + (
        a -> {( triA updated (b,c) ensuring (_.size == triA.size+1), segA )},
        b -> {( triB updated (c,a) ensuring (_.size == triB.size+1), segB )},
        c -> {( triC updated (a,b) ensuring (_.size == triC.size+1), segC )}
      )
    )
  }

  def deletedTri( a: Node, b: Node, c: Node ): TriMeshImmutableHashed = {
    val (triA,segA) = _triSeg(a); assert( triA(b) == c )
    val (triB,segB) = _triSeg(b); assert( triB(c) == a )
    val (triC,segC) = _triSeg(c); assert( triC(a) == b )
    new TriMeshImmutableHashed(
      nTris - 1,
      nSegments,
      _triSeg + (
        a -> {( triA - b, segA )},
        b -> {( triB - c, segB )},
        c -> {( triC - a, segC )}
      )
    )
  }

  def hasSegment( a: Node, b: Node ): Boolean
    = _triSeg.getOrElse(a,_emptyVal)._2.contains(b) ||
      _triSeg.getOrElse(b,_emptyVal)._2.contains(a)

  def addedSegment( a: Node, b: Node ): TriMeshImmutableHashed = {
    assert( ! hasSegment(a,b) )
    val (tri,seg) = _triSeg(a)
    new TriMeshImmutableHashed(
      nTris,
      nSegments + 1,
      _triSeg updated (a, (tri,seg+b) )
    )
  }

  def deletedSegment( a: Node, b: Node ): TriMeshImmutableHashed = {
    val (triA,segA) = _triSeg(a)
    val (triB,segB) = _triSeg(b)
    val triSeg = if( segA contains b ) _triSeg.updated( a, (triA,segA - b) )
            else if( segB contains a ) _triSeg.updated( b, (triB,segB - a) )
            else throw new NoSuchElementException
    new TriMeshImmutableHashed(nTris, nSegments-1, triSeg)
  }

  def toHtml: String = {
    val changes = ArrayBuffer.empty[Change]
    foreachNode{ changes += AddNode(_) }
    foreachTri{ changes += AddTri(_,_,_) }
    foreachSegment{ changes += AddSegment(_,_) }
    TriMeshTaped.toHtml(changes)
  }
}
object TriMeshImmutableHashed
{
  val empty = new TriMeshImmutableHashed(0,0,HashMap.empty)

  private[TriMeshImmutableHashed] val _emptyVal = ( HashMap.empty[Node,Node], HashSet.empty[Node] )

  class NodeImpl( // <- must not be case class because there may two or more nodes with the same (x,y) coordinates
    override val x: Double,
    override val y: Double
  ) extends Node
  {}
}