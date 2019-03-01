package deltri

import java.util.Arrays

import deltri.TriMesh._
import deltri.TriMeshIndexed._
import deltri.TriMeshTaped._

import scala.collection.mutable.ArrayBuffer

/**
  */
// TODO: move
class TriMeshIndexed private() extends TriMesh
{
  override type NodeType = IndexedNode

  private var _nodes = Array.empty[Node]
  /** Arrays containing both triangle and segment information for each node.
    */
  private var _triSeg = Array.empty[Array[Int]]
  private var _nNodes, _nTris, _nSegs, _len, _gap = 0

  override def nNodes = _nNodes
  override def nTris = _nTris
  override def nSegments = _nSegs

  override def hasEdge( a: Node, b: Node ) =
  {
    val ai = nodeIndex(a)
    val bi = nodeIndex(b)
    _hasAdjacent(ai,bi) ||
    _hasAdjacent(bi,ai)
  }

  override def boundaries: Seq[Seq[IndexedNode]] = {
    val result = ArrayBuffer.empty[Seq[IndexedNode]]
    val edgeMap = new Array[Int](nNodes)
    Arrays.fill(edgeMap,-1)

    val len  = _len
    val nodes= _nodes
    var i,j = 0
    while( i < len ) {
      nodes(i) match {
        case IndexedNode(a,_,_) =>
          _foreachTri(a){
            (b,_) => if( ! _hasAdjacent(b,a) ) {
              assert( edgeMap(a) < 0 )
              edgeMap(a) = b
            }
          }
          j += 1
        case Gap(_) =>
      }
      i += 1
    }
    assert( _nNodes == j ) // <- TODO: remove check

    i = edgeMap.length
    while( i > 0 ) {
      i -= 1
      if( edgeMap(i) >= 0 ) {
        val boundary = ArrayBuffer.empty[IndexedNode]
        j = i
        while( edgeMap(j) >= 0 ) {
          val J = edgeMap(j); edgeMap(j) = -1; j=J
          boundary += nodes(j).asInstanceOf[IndexedNode]
        }
        result += boundary
      }
    }

    result
  }

  private def _hasAdjacent( a: Int, b: Int ) =
  {
    val triSeg = _triSeg(a)
    val nTri = triSeg(0)
    0 <= Arrays.binarySearch(triSeg, 2,2+nTri, b)
  }

  override def foreachNode[U]( consumer: this.NodeType => U ) =
  {
    val len  = _len
    val nodes= _nodes
    var i,j = 0
    while( i < len ) {
      nodes(i) match {
        case node: IndexedNode => consumer(node); j += 1
        case Gap(_) =>
      }
      i += 1
    }
    assert( _nNodes == j ) // <- TODO: remove check
  }

  override def foreachTriAround[U]( node: Node )( consumer: (this.NodeType,this.NodeType) => U ): Unit
    = _foreachTri{ nodeIndex(node) }{
        (b,c) => consumer(
          _nodes(b).asInstanceOf[IndexedNode],
          _nodes(c).asInstanceOf[IndexedNode]
        )
      }

  override def foreachTri[U]( consumer: (this.NodeType,this.NodeType,this.NodeType) => U ) =
  {
    val len  = _len
    val nodes= _nodes
    var i,j,k = 0
    while( i < len ) {
      nodes(i) match {
        case node @ IndexedNode(a,_,_) =>
          _foreachTri(a){ (b,c) =>
            if( a < b && a < c ) {
            consumer( node,
              _nodes(b).asInstanceOf[IndexedNode],
              _nodes(c).asInstanceOf[IndexedNode]
            )
            k += 1
          }}
          j += 1
        case Gap(_) =>
      }
      i += 1
    }
    assert( _nNodes == j ) // <- TODO: remove checks
    assert( _nTris == k )
  }

  @inline private def _foreachTri[U]( a: Int )( consumer: (Int,Int) => U ): Unit =
  {
    val triSeg = _triSeg(a)
    val nTri = triSeg(0)
    var i = 2+nTri; while( i > 2 ) { i -= 1
      consumer( triSeg(i), triSeg(i+nTri) )
    }
  }

  override def foreachSegment[U]( consumer: (this.NodeType,this.NodeType) => U ) =
  {
    val len  = _len
    val nodes= _nodes
    var k,j,i = 0
    while(  i < len ) {
      nodes(i) match {
        case node @ IndexedNode(a,_,_) =>
          _foreachSegment(a){ b =>
            assert( a < b )
            consumer( node, _nodes(b).asInstanceOf[IndexedNode] )
            k += 1
          }
          j += 1
        case Gap(_) =>
      }
      i += 1
    }
    assert( _nNodes == j ) // <- TODO: remove checks
    assert( _nSegs == k )
  }

  override def foreachSegmentAround[U]( node: Node )( consumer: this.NodeType => U ): Unit
    = _foreachSegment{ nodeIndex(node) }{
        b => consumer(
          _nodes(b).asInstanceOf[IndexedNode]
        )
      }

  @inline private def _foreachSegment[U]( a: Int )( consumer: Int => U ): Unit =
  {
    val triSeg = _triSeg(a)
    val nSeg = triSeg(1)
    var i = triSeg.length-nSeg; while( i < triSeg.length ) {
      consumer{ triSeg(i) }
      i += 1
    }
  }

  private def nodeIndex( node: Node ): Int =
  {
    val result = node.asInstanceOf[IndexedNode].index
    assert( node eq _nodes(result) )
    result
  }

  override def hasNode( node: Node )
    = node match {
        case IndexedNode(i,_,_) => node eq _nodes(i)
        case _ => false
      }

  override def addNode( x: Double, y: Double ): this.NodeType = {
    val idx = if( _gap == _len ) {
      if( _nodes.length == _len )
      {
        val arrLen = _len*2 max 8
        _nodes = Arrays.copyOf(_nodes, arrLen)
        _triSeg = Arrays.copyOf(_triSeg, arrLen)
      }
      val idx = _len
      _len += 1
      _gap += 1
      idx
    } else {
      val Gap(next) = _nodes(_gap)
      val idx = _gap; _gap = next
      idx
    }
    val node = new IndexedNode(idx,x,y)
    _nNodes += 1
    _nodes(idx) = node
    _triSeg(idx) = new Array[Int](8)
    node
  }

  override def delNode( node: Node ) =
  {
    val a = nodeIndex(node)
    _foreachSegment(a){
      b =>
        _delSegment(a,b)
        _nSegs -= 1
    }
    _foreachTri(a){
      (b,c) =>
        _delTri(b,c,a)
        _delTri(c,a,b)
        _nTris -= 1
    }
    _nNodes -= 1
    _nodes(a) = Gap(_gap)
    _triSeg(a) = null
    _gap = a
  }

  override def addTri( a: Node, b: Node, c: Node ) =
  {
    assert( ! (_orient2d(a,b,c) <= 0), f"Bad orientation (${_orient2d(a,b,c)}) for triangle:\n  $a\n  $b\n  $c" )
    val ai = nodeIndex(a)
    val bi = nodeIndex(b)
    val ci = nodeIndex(c)
    // FIXME: undo changes when AssertionError occoured
          _addTri(ai,bi,ci)
    try { _addTri(bi,ci,ai)
    try { _addTri(ci,ai,bi)
    } catch { case ae: AssertionError => _delTri(bi,ci,ai); throw ae }
    } catch { case ae: AssertionError => _delTri(ai,bi,ci); throw ae }
    _nTris += 1
  }

  private def _addTri( a: Int, b: Int, c: Int ) =
  {
    val triSeg = _triSeg(a)

    val nTri = triSeg(0)
    val nSeg = triSeg(1)
    val i = ~ Arrays.binarySearch(triSeg, /*from=*/2, /*until=*/2+nTri, b)
    assert( i >= 0 )

    var out = triSeg
    if( 4 + 2*nTri + nSeg > triSeg.length ) {
      out = new Array[Int](2*out.length) ensuring (_.length >= 4 + 2*nTri + nSeg)
      System.arraycopy(triSeg,1, out,1, i-1)
      System.arraycopy(
        triSeg,triSeg.length-nSeg,
        out,      out.length-nSeg,
        nSeg
      )
      _triSeg(a) = out
    }

    System.arraycopy(triSeg,i+nTri, out,i+2+nTri,  nTri-i+2); out(i+1+nTri) = c
    System.arraycopy(triSeg,i,      out,i+1,       nTri    ); out(i) = b
    out(0) = nTri+1
  }

  override def delTri( a: Node, b: Node, c: Node ) =
  {
    assert( ! (_orient2d(a,b,c) <= 0) )
    val ai = nodeIndex(a)
    val bi = nodeIndex(b)
    val ci = nodeIndex(c)
    _delTri(ai,bi,ci)
    _delTri(bi,ci,ai)
    _delTri(ci,ai,bi)
    _nTris -= 1
  }

  private def _delTri( a: Int, b: Int, c: Int ) =
  {
    val triSeg = _triSeg(a)
    val nTri = triSeg(0)
    val i = Arrays.binarySearch(triSeg, /*from=*/2, /*until=*/2+nTri, b)
    assert( i >= 0 )
    assert( triSeg(nTri+i) == c )
    val out = triSeg // <- TODO: maybe shrink if array massively oversized?
    System.arraycopy(triSeg,i+1,        out,i,        nTri-1  )
    System.arraycopy(triSeg,i+2+nTri-1, out,i+nTri-1, nTri+1-i)
    out(0) = nTri-1
  }

  override def hasSegment( a: Node, b: Node ): Boolean =
  {
    assert( a ne b )
    val ai = nodeIndex(a)
    val bi = nodeIndex(b)
    if( ai < bi ) _hasSegment(ai,bi)
    else          _hasSegment(bi,ai)
  }

  private def _hasSegment( a: Int, b: Int ): Boolean =
  {
    val triSeg = _triSeg(a)
    val nSeg = triSeg(1)
    0 <= Arrays.binarySearch(
      triSeg,
      triSeg.length-nSeg,
      triSeg.length,
      b
    )
  }

  override def addSegment( a: Node, b: Node ): Unit =
  {
    assert( a ne b )
    val ai = nodeIndex(a)
    val bi = nodeIndex(b)
    if( ai < bi ) _addSegment(ai,bi)
    else          _addSegment(bi,ai)
    _nSegs += 1
  }

  private def _addSegment( a: Int, b: Int ): Unit =
  {
    val triSeg = _triSeg(a)

    val nTri = triSeg(0)
    val nSeg = triSeg(1)
    var i = ~ Arrays.binarySearch(triSeg, /*from=*/triSeg.length-nSeg, /*until=*/triSeg.length, b)
    assert( i >= 0 )
    i -= triSeg.length

    var out = triSeg
    if( 3 + 2*nTri + nSeg > triSeg.length ) {
      out = new Array[Int](2*out.length) ensuring (_.length >= 3 + 2*nTri + nSeg)
      System.arraycopy(triSeg,0, out,0, 2+2*nTri)
      System.arraycopy(
        triSeg, triSeg.length+i,
        out,       out.length+i,
        -i
      )
      _triSeg(a) = out
    }

    System.arraycopy(
      triSeg, triSeg.length - nSeg,
      out,       out.length - nSeg-1,
      nSeg + i
    )
    out(out.length+i-1) = b
    out(1) = nSeg+1
  }

  override def delSegment( a: Node, b: Node ): Unit =
  {
    assert( a ne b )
    val ai = nodeIndex(a)
    val bi = nodeIndex(b)
    if( ai < bi ) _delSegment(ai,bi)
    else          _delSegment(bi,ai)
    _nSegs -= 1
  }

  private def _delSegment( a: Int, b: Int ): Unit =
  {
    val triSeg = _triSeg(a)
    val nSeg = triSeg(0)
    val i = Arrays.binarySearch(triSeg, /*from=*/triSeg.length-nSeg, /*until=*/triSeg.length, b)
    assert( i >= 0 )
    val out = triSeg // <- TODO: maybe shrink if array massively oversized?
    val triSeg1st = triSeg.length - nSeg
    System.arraycopy(
      triSeg,triSeg1st,
      out,out.length - nSeg+1,
      i - triSeg1st
    )
    out(1) = nSeg-1
  }

  override def adjacent( a: Node, b: Node ): NodeMaybe[IndexedNode] = {
    val ai = nodeIndex(a)
    val bi = nodeIndex(b)
    val triSeg = _triSeg(ai)
    val nTri = triSeg(0)
    val i = Arrays.binarySearch(triSeg, 2,2+nTri, bi)
    new NodeMaybe(
      if( i < 0 ) null else _nodes( triSeg(i+nTri) ).asInstanceOf[IndexedNode]
    )
  }

  def toHtml: String = TriMeshTaped.toHtml{
    val changes = ArrayBuffer.empty[Change]
    for( node <- nodes ) changes += AddNode(node)
    foreachSegment( (a,b) => changes += AddSegment(a,b) )
    tris foreach { changes += AddTri(_,_,_) }
    changes
  }
}
object TriMeshIndexed
{
  def empty() = new TriMeshIndexed

  def delaunay( nodes: (Double,Double)* ): (TriMeshIndexed,Array[Node])
    = delaunay(
        nodes.view.map(_._1).toArray,
        nodes.view.map(_._2).toArray
      )

  def delaunay( x: Array[Double], y: Array[Double] ): (TriMeshIndexed,Array[Node]) =
  {
    val mesh = TriMeshIndexed.empty()
    val nodes = Delaunay.triangulate(mesh,x,y)
    (mesh,nodes)
  }

  def cdt( plc: PLC ): TriMeshIndexed =
  {
    val mesh = TriMeshIndexed.empty()
    CDT.triangulate(mesh,plc)
    mesh
  }

  private[TriMeshIndexed] case class Gap( next: Int ) extends Node
  {
    override def x = throw new AssertionError
    override def y = throw new AssertionError
  }

  final case class IndexedNode private[deltri]( index: Int, x: Double, y: Double ) extends TriMesh.Node
  {
    override def toString = f"IndexedNode($index%4d, $x%8g, $y%8g)"
//    override def equals( obj: Any ) = this eq obj.asInstanceOf[AnyRef]
//    override def hashCode = System.identityHashCode(this)
  }
}
