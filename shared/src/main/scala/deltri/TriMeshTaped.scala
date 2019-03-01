package deltri

import scala.collection.mutable
import TriMesh.Node
import TriMeshTaped._

import scala.collection.mutable.ArrayBuffer

/** A wrapper around a TriMesh that records all changes made.
  *
  * Concurrent modifications of the underlying/wrapped TriMesh will cause undefined, erroneous behavior.
  *
  * @param mesh
  */
class TriMeshTaped private( val mesh: TriMesh ) extends TriMesh
{
  override type NodeType = mesh.NodeType

  private var _changes = Vector.empty[Change]
  def changes: IndexedSeq[Change] = _changes

  override def nNodes = mesh.nNodes
  override def nTris = mesh.nTris
  override def nSegments = mesh.nSegments

  override def boundaries = mesh.boundaries

  override def foreachNode         [U]              ( consumer:  this.NodeType                              => U ) = mesh foreachNode consumer
  override def foreachTriAround    [U]( node: Node )( consumer: (this.NodeType,this.NodeType)               => U ) = mesh.foreachTriAround(node)(consumer)
  override def foreachTri          [U]              ( consumer: (this.NodeType,this.NodeType,this.NodeType) => U ) = mesh foreachTri consumer
  override def foreachSegment      [U]              ( consumer: (this.NodeType,this.NodeType)               => U ) = mesh foreachSegment consumer
  override def foreachSegmentAround[U]( node: Node )( consumer:  this.NodeType                              => U ) = mesh.foreachSegmentAround(node)(consumer)

  override def hasEdge( a: Node, b: Node )
    = mesh.hasEdge(a,b)

  override def addNode( x: Double, y: Double ) =
  {
    val result = mesh.addNode(x,y)
    _changes :+= AddNode(result)
    result
  }

  override def delNode( node: Node ) =
  {
    val change = ArrayBuffer.empty[Change]
    mesh.foreachSegmentAround(node){ change += DelSegment(node,_  ) }
    mesh.foreachTriAround    (node){ change += DelTri    (node,_,_) }
    mesh.delNode(node)
     change   += DelNode(node)
    _changes ++= change
  }

  override def hasNode(node: Node)
    = mesh.hasNode(node)

  override def addSegment( a: Node, b: Node ) =
  {
    mesh.addSegment(a,b)
    _changes :+= AddSegment(a,b)
  }

  override def delSegment( a: Node, b: Node ) =
  {
    mesh.delSegment(a,b)
    _changes :+= DelSegment(a,b)
  }

  override def hasSegment( a: Node, b: Node )
    = mesh.hasSegment(a,b)

  override def addTri( a: Node, b: Node, c: Node ) =
  {
    mesh.addTri(a,b,c)
    _changes :+= AddTri(a,b,c)
  }

  override def delTri( a: Node, b: Node, c: Node ) =
  {
    mesh.delTri(a,b,c)
    _changes :+= DelTri(a,b,c)
  }

  override def hasTri(a: Node, b: Node, c: Node)
    = mesh.hasTri(a,b,c)

  override def adjacent(a: Node, b: Node)
    = mesh.adjacent(a,b)

  def toHtml = {
    val nodes= mutable.HashSet.empty[Node]
    val tris = mutable.HashSet.empty[(Node,Node,Node)]
    val segs = mutable.HashSet.empty[(Node,Node)]
    foreachNode( nodes += _ )
    foreachTri( (a,b,c) => tris += ((a,b,c),(b,c,a),(c,a,b)) )
    foreachSegment( (a,b) => segs += ((a,b),(b,a)) )

    assert( nodes.size == nNodes      )
    assert(  tris.size == nTris*3     )
    assert(  segs.size == nSegments*2 )

    _changes.reverse.foreach{
      case AddNode(n) => val s=nodes.size; nodes -= n; assert( nodes.size == s-1 )
      case DelNode(n) => val s=nodes.size; nodes += n; assert( nodes.size == s+1 )

      case AddTri(a,b,c) => val s=tris.size; tris -= ((a,b,c),(b,c,a),(c,a,b)); assert( tris.size == s-3, s"tris.size = ${tris.size} != ${s-3} = s-3" )
      case DelTri(a,b,c) => val s=tris.size; tris += ((a,b,c),(b,c,a),(c,a,b)); assert( tris.size == s+3 )

      case AddSegment(a,b) => val s=segs.size; segs -= ((a,b),(b,a)); assert( segs.size == s-2 )
      case DelSegment(a,b) => val s=segs.size; segs += ((a,b),(b,a)); assert( segs.size == s+2 )
    }

    val changes = ArrayBuffer.empty[Change]
    for( n <- nodes )
      changes += AddNode(n)
    while( ! tris.isEmpty )
    {
      val (a,b,c) = tris.iterator.next()
      tris -= ((a,b,c),(b,c,a),(c,a,b))
      changes += AddTri(a,b,c)
    }
    while( ! segs.isEmpty )
    {
      val (a,b) = segs.iterator.next()
      segs -= ((a,b),(b,a))
      changes += AddSegment(a,b)
    }
    changes ++= _changes

    TriMeshTaped.toHtml(changes)
  }
}
object TriMeshTaped
{
  def apply( triMesh: TriMesh ): TriMeshTaped{ type NodeType = triMesh.NodeType }
    = new TriMeshTaped(triMesh).asInstanceOf[TriMeshTaped{ type NodeType = triMesh.NodeType }]

  sealed trait Change {}
  case class AddNode( node: Node )                   extends Change {}
  case class DelNode( node: Node )                   extends Change {}
  case class     AddTri( a: Node, b: Node, c: Node ) extends Change {}
  case class     DelTri( a: Node, b: Node, c: Node ) extends Change {}
  case class AddSegment( a: Node, b: Node ) extends Change {}
  case class DelSegment( a: Node, b: Node ) extends Change {}

  private[deltri] def toHtml( changes: Seq[Change], title: Seq[Char] = "Triangle Mesh" ) =
  {
    val n2i = {
      val _n2i = mutable.HashMap.empty[Node,Long]
      _n2i.getOrElseUpdate(_: Node, _n2i.size.toLong)
    }

    val changesStr = changes map {
      case AddNode(node) => f"{ type: 'add_pt', nid: ${n2i(node)}, x: ${node.x}, y: ${node.y} }"
      case DelNode(node) => f"{ type: 'del_pt', nid: ${n2i(node)}, x: ${node.x}, y: ${node.y} }"
      case AddTri(a,b,c) => f"{ type: 'add_tri', a: ${n2i(a)}, b: ${n2i(b)}, c: ${n2i(c)} }"
      case DelTri(a,b,c) => f"{ type: 'del_tri', a: ${n2i(a)}, b: ${n2i(b)}, c: ${n2i(c)} }"
      case AddSegment(a,b) => f"{ type: 'add_segment', a: ${n2i(a)}, b: ${n2i(b)} }"
      case DelSegment(a,b) => f"{ type: 'del_segment', a: ${n2i(a)}, b: ${n2i(b)} }"
    }

    f"""
     | <!DOCTYPE html>
     |<html lang="en">
     |  <head>
     |    <meta charset="UTF-8">
     |    <title>$title</title>
     |  </head>
     |  <body>
     |    <script type="text/javascript">
     |    'use strict'; {
     |      const SVG = 'http://www.w3.org/2000/svg';
     |
     |      const
     |        div     = document.createElement('div'),
     |        slider  = document.createElement('input'),
     |        spinner = document.createElement('input'),
     |        span    = document.createElement('span');
     |      div.width = '95vw';
     |      div.height= '95vh';
     |      slider.style = 'width: 70vw';
     |
     |      const
     |        svg = document.createElementNS(SVG,'svg'),
     |        pts = document.createElementNS(SVG,'g'),
     |        tris= document.createElementNS(SVG,'g'),
     |        segs= document.createElementNS(SVG,'g'),
     |        circ= document.createElementNS(SVG,'circle');
     |
     |      circ.setAttributeNS(null, 'fill', '#00FF0088')
     |      circ.style['pointer-events'] = 'none';
     |      segs.style['pointer-events'] = 'none';
     |
     |      svg.setAttributeNS(null, 'height',  '920px');
     |//      svg.setAttributeNS(null, 'height', '100%%');
     |      svg.setAttributeNS(null, 'preserveAspectRatio', 'xMidYMid');
     |
     |      svg.appendChild(tris);
     |      svg.appendChild(segs);
     |      svg.appendChild(circ);
     |      svg.appendChild(pts);
     |
     |      div.appendChild(svg);
     |      document.body.appendChild(slider);
     |      document.body.appendChild(spinner);
     |      document.body.appendChild(span);
     |      document.body.appendChild(div);
     |
     |      let
     |        x_min = +Infinity, y_min = +Infinity,
     |        x_max = -Infinity, y_max = -Infinity;
     |
     |      const
     |        id2pt   = {},
     |        ids2seg = {},
     |        ids2tri = {};
     |
     |      tris.onmousemove = event => {
     |        const
     |          {a,b,c} = event.target,
     |          ax = id2pt[a].x, bx = id2pt[b].x, cx = id2pt[c].x,
     |          ay = id2pt[a].y, by = id2pt[b].y, cy = id2pt[c].y;
     |
     |        // https://en.wikipedia.org/wiki/Circumscribed_circle
     |        const
     |          D = 2 * ( ax*(by-cy) + bx*(cy-ay) + cx*(ay-by) ),
     |          an = (ax*ax + ay*ay),
     |          bn = (bx*bx + by*by),
     |          cn = (cx*cx + cy*cy),
     |          ux = ( an*(by-cy) + bn*(cy-ay) + cn*(ay-by) ) / D,
     |          uy = ( an*(cx-bx) + bn*(ax-cx) + cn*(bx-ax) ) / D;
     |        circ.setAttributeNS(null, 'cx', ux);
     |        circ.setAttributeNS(null, 'cy', uy);
     |        circ.setAttributeNS(null, 'r', Math.hypot(ax-ux, ay-uy) );
     |        circ.style.visibility = 'visible';
     |      };
     |
     |      tris.onmouseout = event => circ.style.visibility = 'hidden';
     |
     |      pts.onmouseover = event => span.innerHTML = `Point $${event.target.nid}`;
     |
     |      const ops_do = {
     |        add_pt: change => {
     |          const {nid,x,y} = change;
     |
     |          if( ! id2pt.hasOwnProperty(nid) )
     |          {
     |            const pt = document.createElementNS(SVG,'circle');
     |            pt.setAttributeNS(null,'cx',x);
     |            pt.setAttributeNS(null,'cy',y);
     |            pt.setAttributeNS(null,'r', '0.1%%');
     |            pt.setAttributeNS(null,'fill', 'black');
     |
     |            pt.nid = nid;
     |            pt.x = x;
     |            pt.y = y;
     |
     |            id2pt[nid] = pt;
     |            pts.appendChild(pt);
     |
     |            if( isFinite(x) ) {
     |              x_min = Math.min(x_min, x);
     |              x_max = Math.max(x_max, x);
     |            }
     |            if( isFinite(y) ) {
     |              y_min = Math.min(y_min, y);
     |              y_max = Math.max(y_max, y);
     |            }
     |
     |            const
     |              margin = 0.01,
     |              w  = (x_max - x_min) * (1+2*margin), x0 = x_min - w*margin/(1+2*margin),
     |              h  = (y_max - y_min) * (1+2*margin), y0 = y_min - h*margin/(1+2*margin);
     |
     |            svg.setAttributeNS(null, 'viewBox', `$${x0} $${y0} $${w} $${h}`);
     |          }
     |          id2pt[nid].style.visibility = 'visible';
     |        },
     |        del_pt: change => {
     |          const {nid} = change;
     |          id2pt[nid].style.visibility = 'hidden';
     |        },
     |        add_tri: change => {
     |          let {a,b,c} = change;
     |
     |          if( b < a && b < c ) { let tmp = a; a = b; b = c; c = tmp; }
     |          else if    ( c < a ) { let tmp = a; a = c; c = b; b = tmp; }
     |
     |          if( ! (a < b && a < c) )
     |            throw new Error('ASSERTION_ERROR');
     |
     |          if( ! ids2tri.hasOwnProperty([a,b,c]) )
     |          {
     |            const tri = document.createElementNS(SVG,'polygon');
     |            Object.assign(tri, {a,b,c});
     |
     |            const
     |              p0 = id2pt[a],
     |              p1 = id2pt[b],
     |              p2 = id2pt[c];
     |
     |            tri.style = 'fill:#0000FF44; stroke:blue; stroke-width:0.05%%;'
     |            tri.setAttributeNS(
     |              null, 'points',
     |              `$${p0.x},$${p0.y} $${p1.x},$${p1.y} $${p2.x},$${p2.y}`
     |            );
     |
     |            ids2tri[[a,b,c]] = tri;
     |            tris.appendChild(tri);
     |          }
     |          ids2tri[[a,b,c]].style.visibility = 'visible';
     |        },
     |        del_tri: change => {
     |          let {a,b,c} = change;
     |
     |          if( b < a && b < c ) { let tmp = a; a = b; b = c; c = tmp; }
     |          else if    ( c < a ) { let tmp = a; a = c; c = b; b = tmp; }
     |
     |          if( ! (a < b && a < c) )
     |            throw new Error('ASSERTION_ERROR');
     |
     |          ids2tri[[a,b,c]].style.visibility = 'hidden';
     |        },
     |        add_segment: change => {
     |          let {a,b} = change;
     |          if( b < a ) { let tmp = a; a = b; b = tmp; }
     |          if( ! ids2seg.hasOwnProperty([a,b]) )
     |          {
     |            const seg = document.createElementNS(SVG,'line');
     |
     |            const
     |              p1 = id2pt[a],
     |              p2 = id2pt[b];
     |
     |            seg.style = 'stroke:red; stroke-width:0.25%%;';
     |            seg.setAttributeNS(null, 'x1', p1.x);
     |            seg.setAttributeNS(null, 'y1', p1.y);
     |            seg.setAttributeNS(null, 'x2', p2.x);
     |            seg.setAttributeNS(null, 'y2', p2.y);
     |
     |            ids2seg[[a,b]] = seg;
     |            segs.appendChild(seg);
     |          }
     |          ids2seg[[a,b]].style.visibility = 'visible';
     |        },
     |        del_segment: change => {
     |          let {a,b} = change;
     |          if( b < a ) { let tmp = a; a = b; b = tmp; }
     |          ids2seg[[a,b]].style.visibility = 'hidden';
     |        }
     |      };
     |      const ops_undo = {
     |        add_pt : ops_do.del_pt, add_tri: ops_do.del_tri,
     |        del_pt : ops_do.add_pt, del_tri: ops_do.add_tri,
     |        add_segment: ops_do.del_segment,
     |        del_segment: ops_do.add_segment
     |      };
     |
     |      const changes = [
     |        ${changesStr mkString ",\n        "}
     |      ];
     |
     |      let state = 0;
     |
     |      function goto_state( s ) {
     |        if( s < 0              ) throw new Error();
     |        if( s > changes.length ) throw new Error();
     |        while( state < s ) { const change = changes[state++]; ops_do  [change.type](change); console.log( "  Do" + JSON.stringify(changes[state-1]) ); }
     |        while( state > s ) { const change = changes[--state]; ops_undo[change.type](change); console.log( "UnDo" + JSON.stringify(changes[state  ]) ); }
     |      }
     |
     |      spinner.type='number';
     |      spinner.min = 0;
     |      spinner.max = changes.length;
     |      spinner.step = 1;
     |
     |      slider.type ='range';
     |      slider.min  = spinner.min;
     |      slider.max  = spinner.max;
     |      slider.step = spinner.step;
     |
     |      slider .oninput = e => { spinner.value =  slider.value; goto_state( slider.value); }
     |      spinner.oninput = e => {  slider.value = spinner.value; goto_state(spinner.value); }
     |
     |      spinner.value = 0;
     |    }
     |    </script>
     |  </body>
     |</html>
     """.stripMargin
  }
}