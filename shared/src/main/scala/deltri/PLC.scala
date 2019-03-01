package deltri

/** A piecewise linear complex, consisting of nodes and segments.
  * PLC is used as input for the constrained delaunay tringulation (CDT).
  *
  * @param nodes The nodes that have to be part of the triangulation.
  * @param segments The edges that have to be part of the triangulation.
  * @param confinedBySegments
  *   If set to true the segments of the PLC are assumed to form a closed
  *   boundary around it. The resulting CDT will have no triangles outside
  *   of the outermost closed sequence of segments.
  * @param orientedBoundarySegments
  *   A list segments to the right of which, there is either a hole or the
  *   outside. One segment per hole is enough. For the outside all boundary
  *   segments have to be given. Otherwise set confinedBySegments to true.
  * @param holeNodes
  *   A list of nodes that lie <i>inside</i> a hole.
  */
case class PLC(
  val nodes: Seq[(Double,Double)],
  val segments: Seq[(Int,Int)],
  val confinedBySegments: Boolean = false,
  val orientedBoundarySegments: Seq[(Int,Int)] = Seq.empty,
  val holeNodes: Set[Int] = Set.empty
)
{
  def nNodes = nodes.length
  assert( Set(nodes:_*).size == nNodes, "No duplicate nodes allowed." )
  for( (a,b) <- segments ) {
    assert( a >= 0 )
    assert( b >= 0 )
    assert( a < nNodes )
    assert( b < nNodes )
    assert( a != b, "Both nodes in a segment must be different (in segments).")
  }
  for( (a,b) <- orientedBoundarySegments ) {
    assert( a >= 0 )
    assert( b >= 0 )
    assert( a < nNodes )
    assert( b < nNodes )
    assert( a != b, "Both nodes in a segment must be different (in orientedBoundarySegments).")
  }
  for( node <- holeNodes ) {
    assert( node >= 0 )
    assert( node < nNodes )
  }
}
