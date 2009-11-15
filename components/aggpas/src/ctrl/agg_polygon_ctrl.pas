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
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 23.02.2006-Milano: Unit port establishment
//
{ agg_polygon_ctrl.pas }
unit
 agg_polygon_ctrl ;

INTERFACE

{$I agg_mode.inc }

uses
 Math ,
 agg_basics ,
 agg_conv_stroke ,
 agg_ellipse ,
 agg_color ,
 agg_ctrl ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 simple_polygon_vertex_source = object(vertex_source )
   m_polygon    : double_ptr;
   m_num_points ,
   m_vertex     : unsigned;
   m_roundoff   ,
   m_close      : boolean;

   constructor Construct(polygon : double_ptr; np : unsigned; roundoff : boolean = false; close : boolean = true );

   procedure close_(f : boolean );
   function  _close : boolean;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 polygon_ctrl_impl = object(ctrl )
   m_polygon    : double_ptr;
   m_num_points : unsigned;

   m_node ,
   m_edge : int;

   m_vs     : simple_polygon_vertex_source;
   m_stroke : conv_stroke;

   m_ellipse      : ellipse;
   m_point_radius : double;
   m_status       : unsigned;

   m_dx ,
   m_dy : double;

   m_in_polygon_check : boolean;

   constructor Construct(np : unsigned; point_radius : double = 5 );
   destructor  Destruct; virtual;

   function  _num_points : unsigned;

   function  _xn(n : unsigned ) : double;
   function  _yn(n : unsigned ) : double;

   function  xn_ptr(n : unsigned ) : double_ptr;
   function  yn_ptr(n : unsigned ) : double_ptr;

   function  _polygon : double_ptr;

   procedure line_width_(w : double );
   function  _line_width : double;

   procedure point_radius_(r : double );
   function  _point_radius : double;

   procedure in_polygon_check_(f : boolean );
   function  _in_polygon_check : boolean;

   procedure close_(f : boolean );
   function  _close : boolean;

  // Vertex source interface
   function  num_paths : unsigned; virtual;
   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  // Event handlers
   function  in_rect(x ,y : double ) : boolean; virtual;

   function  on_mouse_button_down(x ,y : double ) : boolean; virtual;
   function  on_mouse_button_up  (x ,y : double ) : boolean; virtual;

   function  on_mouse_move(x ,y : double; button_flag : boolean ) : boolean; virtual;
   function  on_arrow_keys(left ,right ,down ,up : boolean ) : boolean; virtual;

  // Private
   function  check_edge      (i : unsigned; x ,y : double ) : boolean;
   function  point_in_polygon(x ,y : double ) : boolean;

  end;

 polygon_ctrl = object(polygon_ctrl_impl )
   m_color : aggclr;

   constructor Construct(np : unsigned; point_radius : double = 5 );

   procedure line_color_(c : aggclr_ptr );

   function  _color(i : unsigned ) : aggclr_ptr; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor simple_polygon_vertex_source.Construct;
begin
 m_polygon   :=polygon;
 m_num_points:=np;
 m_vertex    :=0;
 m_roundoff  :=roundoff;
 m_close     :=close;

end;

{ CLOSE_ }
procedure simple_polygon_vertex_source.close_;
begin
 m_close:=f;

end;

{ _CLOSE }
function simple_polygon_vertex_source._close;
begin
 result:=m_close;

end;

{ REWIND }
procedure simple_polygon_vertex_source.rewind;
begin
 m_vertex:=0;

end;

{ VERTEX }
function simple_polygon_vertex_source.vertex;
begin
 if m_vertex > m_num_points then
  begin
   result:=path_cmd_stop;

   exit;

  end;

 if m_vertex = m_num_points then
  begin
   inc(m_vertex );

   if m_close then
    result:=path_cmd_end_poly or path_flags_close
   else
    result:=path_cmd_end_poly or 0;

   exit; 

  end;

 x^:=double_ptr(ptrcomp(m_polygon ) + (m_vertex * 2 ) * sizeof(double ) )^;
 y^:=double_ptr(ptrcomp(m_polygon ) + (m_vertex * 2 + 1 ) * sizeof(double ) )^;

 if m_roundoff then
  begin
   x^:=Floor(x^ ) + 0.5;
   y^:=Floor(y^ ) + 0.5;

  end;

 inc(m_vertex );

 if  m_vertex = 1 then
  result:=path_cmd_move_to
 else
  result:=path_cmd_line_to;

end;

{ CONSTRUCT }
constructor polygon_ctrl_impl.Construct;
begin
 inherited Construct(0 ,0 ,1 ,1 ,false );

 agg_getmem(pointer(m_polygon ) ,np * 2 * sizeof(double ) );

 m_num_points:=np;

 m_node:=-1;
 m_edge:=-1;

 m_vs.Construct    (m_polygon ,m_num_points ,false );
 m_stroke.Construct(@m_vs );
 m_ellipse.Construct;

 m_point_radius:=point_radius;

 m_status:=0;

 m_dx:=0.0;
 m_dy:=0.0;

 m_in_polygon_check:=false;

 m_stroke.width_(1.0 );

end;

{ DESTRUCT }
destructor polygon_ctrl_impl.Destruct;
begin
 agg_freemem(pointer(m_polygon ) ,m_num_points * 2 * sizeof(double ) );

 m_stroke.Destruct;

end;

{ _NUM_POINTS }
function polygon_ctrl_impl._num_points;
begin
 result:=m_num_points;

end;

{ _XN }
function polygon_ctrl_impl._xn;
begin
 result:=double_ptr(ptrcomp(m_polygon ) + (n * 2 ) * sizeof(double ) )^;

end;

{ _YN }
function polygon_ctrl_impl._yn;
begin
 result:=double_ptr(ptrcomp(m_polygon ) + (n * 2 + 1 ) * sizeof(double ) )^;

end;

{ XN_PTR }
function polygon_ctrl_impl.xn_ptr;
begin
 result:=double_ptr(ptrcomp(m_polygon ) + (n * 2 ) * sizeof(double ) );

end;

{ YN_PTR }
function polygon_ctrl_impl.yn_ptr;
begin
 result:=double_ptr(ptrcomp(m_polygon ) + (n * 2 + 1 ) * sizeof(double ) );

end;

{ _POLYGON }
function polygon_ctrl_impl._polygon;
begin
 result:=m_polygon;

end;

{ LINE_WIDTH_ }
procedure polygon_ctrl_impl.line_width_;
begin
 m_stroke.width_(w );

end;

{ _LINE_WIDTH }
function polygon_ctrl_impl._line_width;
begin
 result:=m_stroke._width;

end;

{ POINT_RADIUS_ }
procedure polygon_ctrl_impl.point_radius_;
begin
 m_point_radius:=r;

end;

{ _POINT_RADIUS }
function polygon_ctrl_impl._point_radius;
begin
 result:=m_point_radius;

end;

{ IN_POLYGON_CHECK_ }
procedure polygon_ctrl_impl.in_polygon_check_;
begin
 m_in_polygon_check:=f;

end;

{ _IN_POLYGON_CHECK }
function polygon_ctrl_impl._in_polygon_check;
begin
 result:=m_in_polygon_check;

end;

{ CLOSE_ }
procedure polygon_ctrl_impl.close_;
begin
 m_vs.close_(f );

end;

{ _CLOSE }
function polygon_ctrl_impl._close;
begin
 result:=m_vs._close;

end;

{ NUM_PATHS }
function polygon_ctrl_impl.num_paths;
begin
 result:=1;

end;

{ REWIND }
procedure polygon_ctrl_impl.rewind;
begin
 m_status:=0;

 m_stroke.rewind(0 );

end;

{ VERTEX }
function polygon_ctrl_impl.vertex;
var
 cmd : unsigned;
 r   : double;

begin
 cmd:=path_cmd_stop;
 r  :=m_point_radius;

 if m_status = 0 then
  begin
   cmd:=m_stroke.vertex(x ,y );

   if not is_stop(cmd ) then
    begin
     transform_xy(x ,y );

     result:=cmd;

     exit;

    end;

   if (m_node >= 0 ) and
      (m_node = m_status ) then
    r:=r * 1.2;

   m_ellipse.init(_xn(m_status ) ,_yn(m_status ) ,r ,r ,32 );

   inc(m_status );

  end;

 cmd:=m_ellipse.vertex(x ,y );

 if not is_stop(cmd ) then
  begin
   transform_xy(x ,y );

   result:=cmd;

   exit;

  end;

 if m_status >= m_num_points then
  begin
   result:=path_cmd_stop;

   exit;

  end;

 if (m_node >= 0 ) and
    (m_node = m_status ) then
  r:=r * 1.2;

 m_ellipse.init(_xn(m_status ) ,_yn(m_status ) ,r ,r ,32 );

 inc(m_status );

 cmd:=m_ellipse.vertex(x ,y );

 if not is_stop(cmd ) then
  transform_xy(x ,y );

 result:=cmd;

end;

{ IN_RECT }
function polygon_ctrl_impl.in_rect;
begin
 result:=false;

end;

{ ON_MOUSE_BUTTON_DOWN }
function polygon_ctrl_impl.on_mouse_button_down;
var
 i   : unsigned;
 ret : boolean;

begin
 ret:=false;

 m_node:=-1;
 m_edge:=-1;

 inverse_transform_xy(@x ,@y );

 for i:=0 to m_num_points - 1 do
  if Sqrt((x - _xn(i ) ) * (x - _xn(i ) ) + (y - _yn(i ) ) * (y - _yn(i ) ) ) < m_point_radius then
   begin
    m_dx  :=x - _xn(i );
    m_dy  :=y - _yn(i );
    m_node:=i;
    ret   :=true;

    break;

   end;

 if not ret then
  for i:=0 to m_num_points - 1 do
   if check_edge(i ,x ,y ) then
    begin
     m_dx  :=x;
     m_dy  :=y;
     m_edge:=i;
     ret   :=true;

     break;

    end;

 if not ret then
  if point_in_polygon(x ,y ) then
   begin
    m_dx  :=x;
    m_dy  :=y;
    m_node:=m_num_points;
    ret   :=true;

   end;

 result:=ret;

end;

{ ON_MOUSE_BUTTON_UP }
function polygon_ctrl_impl.on_mouse_button_up;
var
 ret : boolean;

begin
 ret   :=(m_node >= 0 ) or (m_edge >= 0 );
 m_node:=-1;
 m_edge:=-1;
 result:=ret;

end;

{ ON_MOUSE_MOVE }
function polygon_ctrl_impl.on_mouse_move;
var
 ret : boolean;

 i ,n1 ,n2 : unsigned;

 dx ,dy : double;

begin
 ret:=false;

 inverse_transform_xy(@x ,@y );

 if m_node = m_num_points then
  begin
   dx:=x - m_dx;
   dy:=y - m_dy;

   for i:=0 to m_num_points - 1 do
    begin
     xn_ptr(i )^:=_xn(i ) + dx;
     yn_ptr(i )^:=_yn(i ) + dy;

    end;

   m_dx:=x;
   m_dy:=y;
   ret :=true;

  end
 else
  if m_edge >= 0 then
   begin
    n1:=m_edge;
    n2:=(n1 + m_num_points - 1 ) mod m_num_points;
    dx:=x - m_dx;
    dy:=y - m_dy;

    xn_ptr(n1 )^:=_xn(n1 ) + dx;
    yn_ptr(n1 )^:=_yn(n1 ) + dy;
    xn_ptr(n2 )^:=_xn(n2 ) + dx;
    yn_ptr(n2 )^:=_yn(n2 ) + dy;

    m_dx:=x;
    m_dy:=y;
    ret :=true;

   end
  else
   if m_node >= 0 then
    begin
     xn_ptr(m_node )^:=x - m_dx;
     yn_ptr(m_node )^:=y - m_dy;

     ret:=true;

    end;

 result:=ret;

end;

{ ON_ARROW_KEYS }
function polygon_ctrl_impl.on_arrow_keys;
begin
 result:=false;

end;

{ CHECK_EDGE }
function polygon_ctrl_impl.check_edge;
var
 ret : boolean;

 n1 ,n2 : unsigned;

 x1 ,y1 ,x2 ,y2 ,dx ,dy ,x3 ,y3 ,x4 ,y4 ,den ,u1 ,xi ,yi : double;

begin
 ret:=false;

 n1:=i;
 n2:=(i + m_num_points - 1 ) mod m_num_points;
 x1:=_xn(n1 );
 y1:=_yn(n1 );
 x2:=_xn(n2 );
 y2:=_yn(n2 );

 dx:=x2 - x1;
 dy:=y2 - y1;

 if Sqrt(dx * dx + dy * dy ) > 0.0000001 then
  begin
   x3:=x;
   y3:=y;
   x4:=x3 - dy;
   y4:=y3 + dx;

   den:=(y4 - y3 ) * (x2 - x1 ) - (x4 - x3 ) * (y2 - y1 );
   u1 :=((x4 - x3 ) * (y1 - y3 ) - (y4 - y3 ) * (x1 - x3 ) ) / den;

   xi:=x1 + u1 * (x2 - x1 );
   yi:=y1 + u1 * (y2 - y1 );

   dx:=xi - x;
   dy:=yi - y;

   if (u1 > 0.0 ) and
      (u1 < 1.0 ) and
      (Sqrt(dx * dx + dy * dy ) <= m_point_radius ) then
    ret:=true;

  end;

 result:=ret;

end;

{ POINT_IN_POLYGON }
//======= Crossings Multiply algorithm of InsideTest ========================
//
// By Eric Haines, 3D/Eye Inc, erich@eye.com
//
// This version is usually somewhat faster than the original published in
// Graphics Gems IV; by turning the division for testing the X axis crossing
// into a tricky multiplication test this part of the test became faster,
// which had the additional effect of making the test for "both to left or
// both to right" a bit slower for triangles than simply computing the
// intersection each time.  The main increase is in triangle testing speed,
// which was about 15% faster; all other polygon complexities were pretty much
// the same as before.  On machines where division is very expensive (not the
// case on the HP 9000 series on which I tested) this test should be much
// faster overall than the old code.  Your mileage may (in fact, will) vary,
// depending on the machine and the test data, but in general I believe this
// code is both shorter and faster.  This test was inspired by unpublished
// Graphics Gems submitted by Joseph Samosky and Mark Haigh-Hutchinson.
// Related work by Samosky is in:
//
// Samosky, Joseph, "SectionView: A system for interactively specifying and
// visualizing sections through three-dimensional medical image data",
// M.S. Thesis, Department of Electrical Engineering and Computer Science,
// Massachusetts Institute of Technology, 1993.
//
// Shoot a test ray along +X axis.  The strategy is to compare vertex Y values
// to the testing point's Y and quickly discard edges which are entirely to one
// side of the test ray.  Note that CONVEX and WINDING code can be added as
// for the CrossingsTest() code; it is left out here for clarity.
//
// Input 2D polygon _pgon_ with _numverts_ number of vertices and test point
// _point_, returns 1 if inside, 0 if outside.
function polygon_ctrl_impl.point_in_polygon;
var
 j ,k : unsigned;

 yflag0 ,yflag1 ,inside_flag : int;

 vtx0 ,vty0 ,vtx1 ,vty1 : double;

begin
 if m_num_points < 3 then
  begin
   result:=false;

   exit;

  end;

 if not m_in_polygon_check then
  begin
   result:=false;

   exit;

  end;

 vtx0:=_xn(m_num_points - 1 );
 vty0:=_yn(m_num_points - 1 );

// get test bit for above/below X axis
 yflag0:=int(vty0 >= y);

 vtx1:=_xn(0 );
 vty1:=_yn(0 );

 inside_flag:=0;

 for j:=1 to m_num_points do
  begin
   yflag1:=int(vty1 >= y );

  // Check if endpoints straddle (are on opposite sides) of X axis
  // (i.e. the Y's differ); if so, +X ray could intersect this edge.
  // The old test also checked whether the endpoints are both to the
  // right or to the left of the test point.  However, given the faster
  // intersection point computation used below, this test was found to
  // be a break-even proposition for most polygons and a loser for
  // triangles (where 50% or more of the edges which survive this test
  // will cross quadrants and so have to have the X intersection computed
  // anyway).  I credit Joseph Samosky with inspiring me to try dropping
  // the "both left or both right" part of my code.
   if yflag0 <> yflag1 then
   // Check intersection of pgon segment with +X ray.
   // Note if >= point's X; if so, the ray hits it.
   // The division operation is avoided for the ">=" test by checking
   // the sign of the first vertex wrto the test point; idea inspired
   // by Joseph Samosky's and Mark Haigh-Hutchinson's different
   // polygon inclusion tests.
    if int((vty1 - y ) * (vtx0 - vtx1 ) >=
           (vtx1 - x ) * (vty0 - vty1 ) ) = yflag1 then
     inside_flag:=inside_flag xor 1;

  // Move to the next pair of vertices, retaining info as possible.
   yflag0:=yflag1;
   vtx0  :=vtx1;
   vty0  :=vty1;

   if j >= m_num_points then
    k:=j - m_num_points
   else
    k:=j;

   vtx1:=_xn(k );
   vty1:=_yn(k );

  end;

 result:=inside_flag <> 0;

end;

{ CONSTRUCT }
constructor polygon_ctrl.Construct;
begin
 inherited Construct(np ,point_radius );

 m_color.ConstrDbl(0.0 ,0.0 ,0.0 );

end;

{ LINE_COLOR_ }
procedure polygon_ctrl.line_color_;
begin
 m_color:=c^;

end;

{ _COLOR }
function polygon_ctrl._color;
begin
 result:=@m_color;

end;

END.

