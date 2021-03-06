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

    var numNodes,numSegs,numTris = 0


    foreachNode   (  n      => { nodes += n;                         numNodes += 1 } )
    foreachSegment( (a,b)   => {  segs += ((a,b),(b,a));             numSegs  += 1 } )
    foreachTri    ( (a,b,c) => {  tris += ((a,b,c),(b,c,a),(c,a,b)); numTris  += 1 } )

    assert( nNodes    == numNodes )
    assert( nSegments == numSegs  )
    assert( nTris     == numTris  )

    assert( nodes.size ==   nNodes   , s"${nodes.size} != ${  nNodes   }" )
    assert(  tris.size == 3*nTris    , s"${ tris.size} != ${3*nTris    }" )
    assert(  segs.size == 2*nSegments, s"${ segs.size} != ${2*nSegments}" )

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

  def empty() = this( TriMeshIndexed.empty )

  def delaunay( nodes: (Double,Double)* ): (TriMeshTaped,Array[Node])
    = delaunay(
        nodes.view.map(_._1).toArray,
        nodes.view.map(_._2).toArray
      )

  def delaunay( x: Array[Double], y: Array[Double] ): (TriMeshTaped,Array[Node]) =
  {
    val mesh = empty()
    val nodes = Delaunay.triangulate(mesh,x,y)
    (mesh,nodes)
  }

  def delaunayConstrained( plc: PLC ): (TriMeshTaped,Array[Node]) =
  {
    val mesh = empty()
    val nodes = CDT.triangulate(mesh,plc)
    (mesh,nodes)
  }

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
     |        save_btn= document.createElement('button'),
     |        anim_btn= document.createElement('button');
     |      slider.style = 'width: 75vw';
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
     |      svg.style = 'width:95vw; height:95vh;';
     |      svg.setAttribute('version',     '1.1');
     |      svg.setAttribute('xmlns',       'http://www.w3.org/2000/svg'  );
     |      svg.setAttribute('xmlns:xlink', 'http://www.w3.org/1999/xlink');
     |      svg.setAttributeNS(null, 'preserveAspectRatio', 'xMidYMid');
     |
     |      svg.appendChild(tris);
     |      svg.appendChild(segs);
     |      svg.appendChild(circ);
     |      svg.appendChild(pts);
     |
     |      save_btn.innerHTML = 'Save SVG';
     |      anim_btn.innerHTML = 'Save Anim.';
     |
     |      div.appendChild(svg);
     |      document.body.appendChild(slider);
     |      document.body.appendChild(spinner);
     |      document.body.appendChild(save_btn);
     |      document.body.appendChild(anim_btn);
     |      document.body.appendChild(div);
     |
     |      let
     |        x_min = +Infinity, y_min = +Infinity,
     |        x_max = -Infinity, y_max = -Infinity;
     |
     |      const id2pt = {},
     |          ids2seg = {},
     |          ids2tri = {};
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
     |        circ.setAttributeNS(null, 'opacity', 1);
     |      };
     |
     |      tris.onmouseout = event => circ.setAttributeNS(null, 'opacity', 0);
     |
     |//      pts.onmouseover = event => span.innerHTML = `Point $${event.target.nid}`;
     |
     |      const ops_do = {
     |        add_pt: (change,state) => {
     |          const {nid,x,y} = change;
     |
     |          if( ! id2pt.hasOwnProperty(nid) )
     |          {
     |            const pt = document.createElementNS(SVG,'circle');
     |            for( const [k,v] of Object.entries({ cx: x, cy: y, r: '0.2%%', fill: 'black' }) )
     |              pt.setAttributeNS(null,k,v);
     |
     |            Object.assign(pt, {nid,x,y, toggles: new Set()});
     |
     |            id2pt[nid] = pt;
     |            if( isFinite(x) && isFinite(y) )
     |              pts.appendChild(pt);
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
     |          const pt = id2pt[nid];
     |          pt.toggles.add(state);
     |          pt.setAttributeNS(null, 'opacity', 1);
     |        },
     |        del_pt: (change,state) => {
     |          const {nid} = change;
     |          const pt = id2pt[nid];
     |          pt.toggles.add(state);
     |          pt.setAttributeNS(null, 'opacity', 0);
     |        },
     |        add_tri: (change,state) => {
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
     |            Object.assign(tri, {a,b,c, toggles: new Set()});
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
     |            if( [p0,p1,p2].every(p => isFinite(p.x) && isFinite(p.y)) )
     |              tris.appendChild(tri);
     |          }
     |          const tri = ids2tri[[a,b,c]];
     |          tri.toggles.add(state);
     |          tri.setAttributeNS(null, 'opacity', 1);
     |        },
     |        del_tri: (change,state) => {
     |          let {a,b,c} = change;
     |
     |          if( b < a && b < c ) { let tmp = a; a = b; b = c; c = tmp; }
     |          else if    ( c < a ) { let tmp = a; a = c; c = b; b = tmp; }
     |
     |          if( ! (a < b && a < c) )
     |            throw new Error('ASSERTION_ERROR');
     |
     |          const tri = ids2tri[[a,b,c]];
     |          tri.toggles.add(state);
     |          tri.setAttributeNS(null, 'opacity', 0);
     |        },
     |        add_segment: (change,state) => {
     |          let {a,b} = change;
     |          if( b < a ) { let tmp = a; a = b; b = tmp; }
     |          if( ! ids2seg.hasOwnProperty([a,b]) )
     |          {
     |            const seg = document.createElementNS(SVG,'line');
     |            seg.toggles = new Set();
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
     |            if( [p1,p2].every(p => isFinite(p.x) && isFinite(p.y)) )
     |              segs.appendChild(seg);
     |          }
     |          const seg = ids2seg[[a,b]];
     |          seg.toggles.add(state);
     |          seg.setAttributeNS(null, 'opacity', 1);
     |        },
     |        del_segment: (change,state) => {
     |          let {a,b} = change;
     |          if( b < a ) { let tmp = a; a = b; b = tmp; }
     |          const seg = ids2seg[[a,b]];
     |          seg.toggles.add(state);
     |          seg.setAttributeNS(null, 'opacity', 0);
     |        }
     |      };
     |
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
     |        while( state < s ) { const change = changes[  state]; ops_do  [change.type](change,state); /*console.log(change);*/ state++ }
     |        while( state > s ) { const change = changes[--state]; ops_undo[change.type](change,state); /*console.log(change);*/         }
     |        document.location.hash = s
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
     |      slider.value = spinner.value = function(){
     |        let step = Math.trunc( document.location.hash.slice(1) ) || 0
     |        if( step < 0 ) step += 1 + parseInt(spinner.max)
     |        step = Math.min( step,   spinner.max )
     |        step = Math.max( step,   spinner.min )
     |        return step
     |      }();
     |      goto_state(spinner.value)
     |
     |      save_btn.onclick = () => {
     |        const dat = svg.cloneNode(true);
     |        dat.style = '';
     |        dat.setAttribute('width', '1280px');
     |        dat.setAttribute('height', '720px');
     |
     |        let str = new XMLSerializer().serializeToString(dat);
     |        str = '<?xml version="1.0" encoding="utf-8"?>\\n'
     |            + '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">\\n'
     |            + str;
     |
     |        const blob = new Blob([str], {type: 'image/svg+xml'});
     |        const url = URL.createObjectURL(blob);
     |
     |        // window.open(url);
     |
     |        const a = document.createElement('a');
     |        document.body.appendChild(a);
     |        a.href = url;
     |        a.download = "mesh.svg";
     |
     |        document.body.appendChild(a);
     |        a.click();
     |        document.body.removeChild(a);
     |      }
     |
     |      anim_btn.onclick = () => {
     |        const dat = svg.cloneNode(false);
     |        dat.style = '';
     |        dat.setAttribute('width', '1280px');
     |        dat.setAttribute('height', '720px');
     |
     |        const dt = 32,
     |               T = (changes.length+2)*dt + 8000;
     |
     |        for( const group of [tris,segs,pts] ) {
     |          const clone = group.cloneNode(false);
     |          for( const child of group.childNodes )
     |          {
     |            const dolly = child.cloneNode(false);
     |            let vis = 0;
     |            const times = [-1,...child.toggles].sort((x,y) => x-y)
     |            for( const t of times )
     |            {
     |              let begin = '';
     |              for( let i=0; i < 1; i++ ) // <- REPEAT N TIMES
     |                begin += `$${i*T + (t+1)*dt}ms;`;
     |
     |              const set = document.createElementNS(SVG,'set');
     |              set.setAttributeNS(null, 'attributeName', 'opacity');
     |              set.setAttributeNS(null, 'to', vis);
     |              set.setAttributeNS(null, 'begin', begin);
     |
     |              dolly.appendChild(set);
     |
     |              vis = 1-vis;
     |            }
     |            clone.appendChild(dolly);
     |          }
     |          dat.appendChild(clone);
     |        }
     |//        const trisClone = tris.cloneNode(false)
     |//              segsClone = segs.cloneNode(false)
     |//               ptsClone = pts .cloneNode(false)
     |
     |        let str = new XMLSerializer().serializeToString(dat);
     |        str = '<?xml version="1.0" encoding="utf-8"?>\\n'
     |            + '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">\\n'
     |            + str;
     |
     |        const blob = new Blob([str], {type: 'image/svg+xml'});
     |        const url = URL.createObjectURL(blob);
     |
     |        window.open(url);
     |
     |//        const a = document.createElement('a');
     |//        document.body.appendChild(a);
     |//        a.href = url;
     |//        a.download = "mesh.svg";
     |//
     |//        document.body.appendChild(a);
     |//        a.click();
     |//        document.body.removeChild(a);
     |      }
     |    }
     |    </script>
     |  </body>
     |</html>
     """.stripMargin
  }
}
