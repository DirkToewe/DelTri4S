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
import deltri.TriMeshIndexed.IndexedNode

import scala.collection.immutable.{IndexedSeq => ISeq}

/** A wrapper around an immutable triangle mesh that allows to perform mutable
  * operations on it. The mutable operations are internally tracked as an immutable
  * triangle mesh. Converting from and ot an immutable triangle mesh is therefore
  * an O(1) operation.
  *
  * @param mesh An immutable snapshot of the current state of the mutable wrapper.
  */
class TriMeshMutable( var mesh: TriMeshImmutable ) extends TriMesh
{
  override type NodeType = IndexedNode

  override def nNodes = mesh.nNodes
  override def nTris = mesh.nTris
  override def nSegments = mesh.nSegments

  override def boundaries: ISeq[ISeq[this.NodeType]] = mesh.boundaries

  override def foreachNode         [U]              ( consumer:  this.NodeType                              => U ) = mesh foreachNode                consumer
  override def foreachTriAround    [U]( node: Node )( consumer: (this.NodeType,this.NodeType)               => U ) = mesh.foreachTriAround    (node)(consumer)
  override def foreachTri          [U]              ( consumer: (this.NodeType,this.NodeType,this.NodeType) => U ) = mesh foreachTri                 consumer
  override def foreachSegment      [U]              ( consumer: (this.NodeType,this.NodeType)               => U ) = mesh foreachSegment             consumer
  override def foreachSegmentAround[U]( node: Node )( consumer:  this.NodeType                              => U ) = mesh.foreachSegmentAround(node)(consumer)

  override def hasEdge( a: Node, b: Node ) = mesh hasEdge (a,b)

  override def adjacent( a: Node, b: Node ) = mesh adjacent (a,b)

  override def hasNode( node: Node ) = mesh hasNode node
  override def addNode( x: Double, y: Double )
    = mesh.addedNode(x,y, (m,n) => {mesh = m; n})
  override def delNode( node: Node )
    = mesh = mesh deletedNode node

  override def hasTri( a: Node, b: Node, c: Node ) = mesh hasTri (a,b,c)
  override def addTri( a: Node, b: Node, c: Node )
    = mesh = mesh addedTri (a,b,c)
  override def delTri( a: Node, b: Node, c: Node )
    = mesh = mesh deletedTri (a,b,c)

  override def hasSegment( a: Node, b: Node ) = mesh hasSegment (a,b)
  override def addSegment( a: Node, b: Node )
    = mesh = mesh addedSegment (a,b)
  override def delSegment( a: Node, b: Node )
    = mesh = mesh deletedSegment (a,b)

  override def toHtml = mesh.toHtml
}
object TriMeshMutable
{
  def empty() = new TriMeshMutable( TriMeshImmutable.empty )

  def apply( mesh: TriMeshImmutable ) = new TriMeshMutable(mesh)

  def delaunay( nodes: (Double,Double)* ): (TriMeshMutable,Array[Node])
    = delaunay(
        nodes.view.map(_._1).toArray,
        nodes.view.map(_._2).toArray
      )

  def delaunay( x: Array[Double], y: Array[Double] ): (TriMeshMutable,Array[Node]) =
  {
    val mesh = TriMeshMutable.empty()
    val nodes = Delaunay.triangulate(mesh,x,y)
    (mesh,nodes)
  }

  def delaunayConstrained( plc: PLC ): (TriMeshMutable,Array[Node]) =
  {
    val mesh = empty()
    val nodes = CDT.triangulate(mesh,plc)
    (mesh,nodes)
  }

  class TriMeshMutableExtractor( val mut: TriMeshMutable ) extends AnyVal
  {
    def isEmpty = null == mut
    def get = mut.mesh
  }

  def unapply( mut: TriMeshMutable )
    = new TriMeshMutableExtractor(mut)
}
