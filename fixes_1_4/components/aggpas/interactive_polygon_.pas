unit
 interactive_polygon_ ;

INTERFACE

{$I agg_mode.inc }

uses
 Math ,
 agg_basics ,
 agg_conv_stroke ,
 agg_ellipse ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 simple_polygon_vertex_source = object(vertex_source )
   m_polygon : double_ptr;

   m_num_points ,
   m_vertex     : unsigned;

   m_roundoff ,
   m_close    : boolean;

   constructor Construct(polygon : double_ptr; np : unsigned; roundoff : boolean = false; close : boolean = true );

   procedure close_(f : boolean );
   function  _close : boolean;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 interactive_polygon = object(vertex_source )
   m_polygon    : double_ptr;
   m_num_points : unsigned;

   m_node ,
   m_edge : int;

   m_vs : simple_polygon_vertex_source;

   m_stroke  : conv_stroke;
   m_ellipse : ellipse;

   m_point_radius : double;
   m_status       : unsigned;

   m_dx ,
   m_dy : double;

   constructor Construct(np : unsigned; point_radius : double );
   destructor  Destruct; virtual;

   function  num_points : unsigned;

   function  xn(n : unsigned ) : double;
   function  yn(n : unsigned ) : double;

   function  xn_ptr(n : unsigned ) : double_ptr;
   function  yn_ptr(n : unsigned ) : double_ptr;

   function  polygon : double_ptr;

   function  _node : int;
   procedure node_(n : int );

   function  _close : boolean;
   procedure close_(f : boolean );

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

   function  on_mouse_move(x ,y : double ) : boolean;

   function  on_mouse_button_down(x ,y : double ) : boolean;
   function  on_mouse_button_up(x ,y : double ) : boolean;

   function  check_edge(i : unsigned; x ,y : double ) : boolean;

   function  point_in_polygon(tx ,ty : double ) : boolean;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor simple_polygon_vertex_source.Construct;
begin
 inherited Construct;

 m_polygon   :=polygon;
 m_num_points:=np;

 m_vertex  :=0;
 m_roundoff:=roundoff;
 m_close   :=close;

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

 if m_vertex = 1 then
  result:=path_cmd_move_to
 else
  result:=path_cmd_line_to;

end;

{ CONSTRUCT }
constructor interactive_polygon.Construct;
begin
 inherited Construct;

 agg_getmem(pointer(m_polygon ) ,np * 2 * sizeof(double ) );

 m_num_points:=np;

 m_node:=-1;
 m_edge:=-1;

 m_vs.Construct    (m_polygon ,m_num_points ,false );
 m_stroke.Construct(@m_vs );
 m_ellipse.Construct;

 m_point_radius:=point_radius;
 m_status      :=0;

 m_dx:=0.0;
 m_dy:=0.0;

 m_stroke.width_(1.0 );

end;

{ DESTRUCT }
destructor interactive_polygon.Destruct;
begin
 agg_freemem(pointer(m_polygon ) ,m_num_points * 2 * sizeof(double ) );

 m_stroke.Destruct;

end;

{ NUM_POINTS }
function interactive_polygon.num_points;
begin
 result:=m_num_points;

end;

{ XN }
function interactive_polygon.xn;
begin
 result:=double_ptr(ptrcomp(m_polygon ) + (n * 2 ) * sizeof(double ) )^;

end;

{ YN }
function interactive_polygon.yn;
begin
 result:=double_ptr(ptrcomp(m_polygon ) + (n * 2 + 1 ) * sizeof(double ) )^;

end;

{ XN_PTR }
function interactive_polygon.xn_ptr;
begin
 result:=double_ptr(ptrcomp(m_polygon ) + (n * 2 ) * sizeof(double ) );

end;

{ YN_PTR }
function interactive_polygon.yn_ptr;
begin
 result:=double_ptr(ptrcomp(m_polygon ) + (n * 2 + 1 ) * sizeof(double ) );

end;

{ POLYGON }
function interactive_polygon.polygon;
begin
 result:=m_polygon;

end;

{ _NODE }
function interactive_polygon._node;
begin
 result:=m_node;

end;

{ NODE_ }
procedure interactive_polygon.node_;
begin
 m_node:=n;

end;

{ _CLOSE }
function interactive_polygon._close;
begin
 result:=m_vs._close;

end;

{ CLOSE_ }
procedure interactive_polygon.close_;
begin
 m_vs.close_(f );

end;

{ REWIND }
procedure interactive_polygon.rewind;
begin
 m_status:=0;

 m_stroke.rewind(0 );

end;

{ VERTEX }
function interactive_polygon.vertex;
var
 r : double;

 cmd : unsigned;

begin
 cmd:=path_cmd_stop;
 r  :=m_point_radius;

 if m_status = 0 then
  begin
   cmd:=m_stroke.vertex(x ,y );

   if not is_stop(cmd ) then
    begin
     result:=cmd;

     exit;

    end;

   if (m_node >= 0 ) and
      (m_node = int(m_status ) ) then
    r:=r * 1.2;

   m_ellipse.init(xn(m_status ) ,yn(m_status ) ,r ,r ,32 );

   inc(m_status );

  end;

 cmd:=m_ellipse.vertex(x ,y );

 if not is_stop(cmd ) then
  begin
   result:=cmd;

   exit;

  end;

 if m_status >= m_num_points then
  begin
   result:=path_cmd_stop;

   exit;

  end;

 if (m_node >= 0 ) and
    (m_node = int(m_status ) ) then
  r:=r * 1.2;

 m_ellipse.init(xn(m_status ) ,yn(m_status ) ,r ,r ,32 );

 inc(m_status );

 result:=m_ellipse.vertex(x ,y );

end;

{ ON_MOUSE_MOVE }
function interactive_polygon.on_mouse_move;
var
 ret : boolean;

 i ,n1 ,n2 : unsigned;

 dx ,dy : double;

begin
 ret:=false;

 if m_node = int(m_num_points ) then
  begin
   dx:=x - m_dx;
   dy:=y - m_dy;

   for i:=0 to m_num_points - 1 do
    begin
     xn_ptr(i )^:=xn_ptr(i )^ + dx;
     yn_ptr(i )^:=yn_ptr(i )^ + dy;

    end;

   m_dx:=x;
   m_dy:=y;

   ret:=true;

  end
 else
  if m_edge >= 0 then
   begin
    n1:=m_edge;
    n2:=(n1 + m_num_points - 1 ) mod m_num_points;

    dx:=x - m_dx;
    dy:=y - m_dy;

    xn_ptr(n1 )^:=xn_ptr(n1 )^ + dx;
    yn_ptr(n1 )^:=yn_ptr(n1 )^ + dy;
    xn_ptr(n2 )^:=xn_ptr(n2 )^ + dx;
    yn_ptr(n2 )^:=yn_ptr(n2 )^ + dy;

    m_dx:=x;
    m_dy:=y;

    ret:=true;

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

{ ON_MOUSE_BUTTON_DOWN }
function interactive_polygon.on_mouse_button_down;
var
 i : unsigned;

 ret : boolean;

begin
 ret:=false;

 m_node:=-1;
 m_edge:=-1;

 for i:=0 to m_num_points - 1 do
  if Sqrt((x - xn(i ) ) * (x - xn(i ) ) + (y - yn(i ) ) * (y - yn(i ) ) ) < m_point_radius then
   begin
    m_dx:=x - xn(i );
    m_dy:=y - yn(i );

    m_node:=int(i );

    ret:=true;

    break;

   end;

 if not ret then  
  for i:=0 to m_num_points - 1 do
   if check_edge(i ,x ,y ) then
    begin
     m_dx:=x;
     m_dy:=y;

     m_edge:=int(i );

     ret:=true;

     break;

    end;

 if not ret then
  if point_in_polygon(x ,y ) then
   begin
    m_dx:=x;
    m_dy:=y;

    m_node:=int(m_num_points );

    ret:=true;

   end;

 result:=ret;

end;

{ ON_MOUSE_BUTTON_UP }
function interactive_polygon.on_mouse_button_up;
var
 ret : boolean;

begin
 ret:=(m_node >= 0 ) or (m_edge >= 0 );

 m_node:=-1;
 m_edge:=-1;
 result:=ret;

end;

{ CHECK_EDGE }
function interactive_polygon.check_edge;
var
 ret : boolean;

 n1 ,n2 : unsigned;

 x1 ,y1 ,x2 ,y2 ,dx ,dy ,x3 ,y3 ,x4 ,y4 ,den ,u1 ,xi ,yi : double;

begin
 ret:=false;

 n1:= i;
 n2:= (i + m_num_points - 1 ) mod m_num_points;
 
 x1:=xn(n1 );
 y1:=yn(n1 );
 x2:=xn(n2 );
 y2:=yn(n2 );

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

   xi:=x1 + u1 * (x2 - x1);
   yi:=y1 + u1 * (y2 - y1);

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
function interactive_polygon.point_in_polygon;
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

 vtx0:=xn(m_num_points - 1 );
 vty0:=yn(m_num_points - 1 );

// get test bit for above/below X axis
 yflag0:=int(vty0 >= ty );

 vtx1:=xn(0 );
 vty1:=yn(0 );

 inside_flag:=0;

 for j:=1 to m_num_points do
  begin
   yflag1:=int(vty1 >= ty );

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
    if int((vty1 - ty ) * (vtx0 - vtx1 ) >= (vtx1 - tx ) * (vty0 - vty1 ) ) = yflag1 then
     inside_flag:=inside_flag xor 1;

  // Move to the next pair of vertices, retaining info as possible.
   yflag0:=yflag1;

   vtx0:=vtx1;
   vty0:=vty1;

   if j >= m_num_points then
    k:=j - m_num_points
   else
    k:=j;

   vtx1:=xn(k );
   vty1:=yn(k );

  end;

 result:=inside_flag <> 0;

end;

END.
