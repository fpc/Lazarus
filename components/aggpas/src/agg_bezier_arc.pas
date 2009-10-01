//----------------------------------------------------------------------------
// Anti-Grain Geometry - Version 2.4 (Public License)
// Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)
//
// Anti-Grain Geometry - Version 2.4 Release Milano 3 (AggPas 2.4 RM3)
// Pascal Port By: Milan Marusinec alias Milano
//                 milan@marusinec.sk
//                 http://www.aggpas.org
// Copyright (c) 2005-2006
//
// Permission to copy, use, modify, sell and distribute this software
// is granted provided this copyright notice appears in all copies.
// This software is provided "as is" without express or implied
// warranty, and with no claim as to its suitability for any purpose.
//
//----------------------------------------------------------------------------
// Contact: mcseem@antigrain.com
//          mcseemagg@yahoo.com
//          http://www.antigrain.com
//
//----------------------------------------------------------------------------
//
// Arc generator. Produces at most 4 consecutive cubic bezier curves, i.e.,
// 4, 7, 10, or 13 vertices.
//
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 18.12.2005-Milano: Unit port establishment
//
{ agg_bezier_arc.pas }
unit
 agg_bezier_arc ;

INTERFACE

{$I agg_mode.inc }

uses
 Math ,
 agg_basics ,
 agg_vertex_source ,
 agg_trans_affine ;

{ TYPES DEFINITION }
type
 bezier_arc_ptr = ^bezier_arc;
 bezier_arc = object(vertex_source )
   m_vertex       ,
   m_num_vertices : unsigned;

   m_vertices : double_26;

   m_cmd : unsigned;

   constructor Construct; overload;
   constructor Construct(x ,y ,rx ,ry ,start_angle ,sweep_angle : double ); overload;

   procedure init(x ,y ,rx ,ry ,start_angle ,sweep_angle : double );

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  // Supplemantary functions. num_vertices() actually returns doubled
  // number of vertices. That is, for 1 vertex it returns 2.
   function  num_vertices : unsigned;
   function  vertices : double_26_ptr;

  end;

//==========================================================bezier_arc_svg
// Compute an SVG-style bezier arc.
//
// Computes an elliptical arc from (x1, y1) to (x2, y2). The size and
// orientation of the ellipse are defined by two radii (rx, ry)
// and an x-axis-rotation, which indicates how the ellipse as a whole
// is rotated relative to the current coordinate system. The center
// (cx, cy) of the ellipse is calculated automatically to satisfy the
// constraints imposed by the other parameters.
// large-arc-flag and sweep-flag contribute to the automatic calculations
// and help determine how the arc is drawn.
 bezier_arc_svg_ptr = ^bezier_arc_svg;
 bezier_arc_svg = object(bezier_arc )
   m_radii_ok : boolean;

   constructor Construct; overload;
   constructor Construct(x1 ,y1 ,rx ,ry ,angle : double; large_arc_flag ,sweep_flag : boolean; x2 ,y2 : double ); overload;

   procedure init(x0 ,y0 ,rx ,ry ,angle : double; large_arc_flag ,sweep_flag : boolean; x2 ,y2 : double );

   function  radii_ok : boolean;

  end;

{ GLOBAL PROCEDURES }
 procedure arc_to_bezier(cx ,cy ,rx ,ry ,start_angle ,sweep_angle : double; curve : double_8_ptr );


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
const
 bezier_arc_angle_epsilon = 0.01;

{ UNIT IMPLEMENTATION }
{ ARC_TO_BEZIER }
procedure arc_to_bezier;
var
 i : unsigned;

 sn ,cs ,
 x0 ,y0 ,
 tx ,ty : double;
 px ,py : array[0..3 ] of double;

begin
 x0:=Cos(sweep_angle / 2.0 );
 y0:=Sin(sweep_angle / 2.0 );
 tx:=(1.0 - x0 ) * 4.0 / 3.0;
 ty:=y0 - tx * x0 / y0;

 px[0 ]:=x0;
 py[0 ]:=-y0;
 px[1 ]:=x0 + tx;
 py[1 ]:=-ty;
 px[2 ]:=x0 + tx;
 py[2 ]:=ty;
 px[3 ]:=x0;
 py[3 ]:=y0;

 sn:=Sin(start_angle + sweep_angle / 2.0 );
 cs:=Cos(start_angle + sweep_angle / 2.0 );

 for i:=0 to 3 do
  begin
   curve[i * 2 ]    :=cx + rx * (px[i ] * cs - py[i ] * sn );
   curve[i * 2 + 1 ]:=cy + ry * (px[i ] * sn + py[i ] * cs );

  end;

end;

{ CONSTRUCT }
constructor bezier_arc.Construct;
begin
 m_vertex:=26;

 m_num_vertices:=0;

 m_cmd:=path_cmd_line_to;

end;

{ CONSTRUCT }
constructor bezier_arc.Construct(x ,y ,rx ,ry ,start_angle ,sweep_angle : double );
begin
 init(x ,y ,rx ,ry ,start_angle ,sweep_angle );

end;

{ INIT }
procedure bezier_arc.init;
var
 i : int;
 f : double;

 total_sweep ,
 local_sweep ,
 prev_sweep  : double;

 done : boolean;

begin
 i:=trunc(start_angle / (2.0 * pi ) );
 f:=start_angle - (i * 2.0 * pi );

 start_angle:=f;

 if sweep_angle >= 2.0 * pi then
  sweep_angle:=2.0 * pi;

 if sweep_angle <= -2.0 * pi then
  sweep_angle:=-2.0 * pi;

 if Abs(sweep_angle ) < 1e-10 then
  begin
   m_num_vertices:=4;

   m_cmd:=path_cmd_line_to;

   m_vertices[0 ]:=x + rx * Cos(start_angle );
   m_vertices[1 ]:=y + ry * Sin(start_angle );
   m_vertices[2 ]:=x + rx * Cos(start_angle + sweep_angle );
   m_vertices[3 ]:=y + ry * Sin(start_angle + sweep_angle );

   exit;

  end;

 total_sweep:=0.0;
 local_sweep:=0.0;

 m_num_vertices:=2;

 m_cmd:=path_cmd_curve4;
 done :=false;

 repeat
  if sweep_angle < 0.0 then
   begin
    prev_sweep :=total_sweep;
    local_sweep:=-pi * 0.5;
    total_sweep:=total_sweep - (pi * 0.5 );

    if total_sweep <= sweep_angle + bezier_arc_angle_epsilon  then
     begin
      local_sweep:=sweep_angle - prev_sweep;

      done:=true;

     end;

   end
  else
   begin
    prev_sweep :=total_sweep;
    local_sweep:=pi * 0.5;
    total_sweep:=total_sweep + (pi * 0.5 );

    if total_sweep >= sweep_angle - bezier_arc_angle_epsilon then
     begin
      local_sweep:=sweep_angle - prev_sweep;

      done:=true;

     end;

   end;

  arc_to_bezier(
   x ,y ,rx ,ry ,
   start_angle ,local_sweep ,
   @m_vertices[m_num_vertices - 2 ] );

  m_num_vertices:=m_num_vertices + 6;
  start_angle   :=start_angle + local_sweep;

 until done or (m_num_vertices >= 26 );

end;

{ REWIND }
procedure bezier_arc.rewind;
begin
 m_vertex:=0;
 
end;

{ VERTEX }
function bezier_arc.vertex;
begin
 if m_vertex >= m_num_vertices then
  begin
   result:=path_cmd_stop;

   exit;

  end;

 x^:=m_vertices[m_vertex ];
 y^:=m_vertices[m_vertex + 1 ];

 inc(m_vertex ,2 );

 if m_vertex = 2 then
  result:=path_cmd_move_to
 else
  result:=m_cmd;

end;

{ NUM_VERTICES }
function bezier_arc.num_vertices;
begin
 result:=m_num_vertices;

end;

{ VERTICES }
function bezier_arc.vertices;
begin
 result:=@m_vertices;

end;

{ CONSTRUCT }
constructor bezier_arc_svg.Construct;
begin
 inherited Construct;

 m_radii_ok:=false;

end;

{ CONSTRUCT }
constructor bezier_arc_svg.Construct(x1 ,y1 ,rx ,ry ,angle : double; large_arc_flag ,sweep_flag : boolean; x2 ,y2 : double );
begin
 inherited Construct;

 m_radii_ok:=false;

 init(x1 ,y1 ,rx ,ry ,angle ,large_arc_flag ,sweep_flag ,x2 ,y2 );

end;

{ INIT }
procedure bezier_arc_svg.init;
var
 i : unsigned;

 v ,
 p , n ,

 sq ,
 x1 ,y1 ,
 cx ,cy ,
 ux ,uy ,
 vx ,vy ,

 dx2 ,dy2 ,
 prx ,pry ,
 px1 ,py1 ,
 cx1 ,cy1 ,
 sx2 ,sy2 ,

 sign ,coef ,

 radii_check ,
 start_angle ,
 sweep_angle ,

 cos_a ,sin_a : double;

 mtx : trans_affine_rotation;
 trn : trans_affine_translation; 

begin
 m_radii_ok:=true;

 if rx < 0.0 then
  rx:=-rx;

 if ry < 0.0 then
  ry:=-rx;

// Calculate the middle point between
// the current and the final points
 dx2:=(x0 - x2 ) / 2.0;
 dy2:=(y0 - y2 ) / 2.0;

// Convert angle from degrees to radians
 cos_a:=Cos(angle );
 sin_a:=Sin(angle );

// Calculate (x1, y1)
 x1:= cos_a * dx2 + sin_a * dy2;
 y1:=-sin_a * dx2 + cos_a * dy2;

// Ensure radii are large enough
 prx:=rx * rx;
 pry:=ry * ry;
 px1:=x1 * x1;
 py1:=y1 * y1;

// Check that radii are large enough
 radii_check:=px1 / prx + py1 / pry;

 if radii_check > 1.0 then
  begin
   rx :=sqrt(radii_check ) * rx;
   ry :=sqrt(radii_check ) * ry;
   prx:=rx * rx;
   pry:=ry * ry;

   if radii_check > 10.0 then
    m_radii_ok:=false;

  end;

// Calculate (cx1, cy1)
 if large_arc_flag = sweep_flag then
  sign:=-1.0
 else
  sign:=1.0;

 sq:=(prx * pry - prx * py1 - pry * px1 ) / (prx * py1 + pry * px1 );

 if sq < 0 then
  coef:=sign * sqrt(0 )
 else
  coef:=sign * sqrt(sq );

 cx1:=coef *  ((rx * y1 ) / ry );
 cy1:=coef * -((ry * x1 ) / rx );

// Calculate (cx, cy) from (cx1, cy1)
 sx2:=(x0 + x2 ) / 2.0;
 sy2:=(y0 + y2 ) / 2.0;
 cx :=sx2 + (cos_a * cx1 - sin_a * cy1 );
 cy :=sy2 + (sin_a * cx1 + cos_a * cy1 );

// Calculate the start_angle (angle1) and the sweep_angle (dangle)
 ux:= (x1 - cx1 ) / rx;
 uy:= (y1 - cy1 ) / ry;
 vx:=(-x1 - cx1 ) / rx;
 vy:=(-y1 - cy1 ) / ry;

// Calculate the angle start
 n:=sqrt(ux * ux + uy * uy );
 p:=ux; // (1 * ux ) + (0 * uy )

 if uy < 0 then
  sign:=-1.0
 else
  sign:=1.0;

 v:=p / n;

 if v < -1.0 then
  v:=-1.0;

 if v > 1.0 then
  v:=1.0;

 start_angle:=sign * ArcCos(v );

// Calculate the sweep angle
 n:=sqrt((ux * ux + uy * uy ) * (vx * vx + vy * vy ) );
 p:=ux * vx + uy * vy;

 if ux * vy - uy * vx < 0 then
  sign:=-1.0
 else
  sign:=1.0;

 v:=p / n;

 if v < -1.0 then
  v:=-1.0;

 if v > 1.0 then
  v:=1.0;

 sweep_angle:=sign * ArcCos(v );

 if (not sweep_flag ) and
    (sweep_angle > 0 ) then
  sweep_angle:=sweep_angle - pi * 2.0
 else
  if sweep_flag and
     (sweep_angle < 0 ) then
   sweep_angle:=sweep_angle + pi * 2.0;

// We can now build and transform the resulting arc
 inherited init(0.0 ,0.0 ,rx ,ry ,start_angle ,sweep_angle );

 mtx.Construct(angle );
 trn.Construct(cx ,cy );

 mtx.multiply(@trn );

 i:=2;

 while i < num_vertices - 2 do
  begin
 //mtx.transform(@m_arc.vertices[i ] ,@m_arc.vertices[i + 1 ] );
   mtx.transform(
    @mtx ,
    double_ptr(ptrcomp(vertices ) + i * sizeof(double ) ) ,
    double_ptr(ptrcomp(vertices ) + (i + 1 ) * sizeof(double ) ) );

   inc(i ,2 );

  end;

// We must make sure that the starting and ending points
// exactly coincide with the initial (x0,y0) and (x2,y2)
 vertices[0 ]:=x0;
 vertices[1 ]:=y0;

 if num_vertices > 2 then
  begin
   vertices[num_vertices - 2 ]:=x2;
   vertices[num_vertices - 1 ]:=y2;

  end;

end;

{ RADII_OK }
function bezier_arc_svg.radii_ok;
begin
 result:=m_radii_ok;

end;

END.

