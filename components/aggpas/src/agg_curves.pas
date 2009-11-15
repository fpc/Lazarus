//----------------------------------------------------------------------------
// Anti-Grain Geometry - Version 2.4 (Public License)
// Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)
// Copyright (C) 2005 Tony Juricic (tonygeek@yahoo.com)
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
// 12.02.2006-Milano: Unit port establishment
//
{ agg_curves.pas }
unit
 agg_curves ;

INTERFACE

{$I agg_mode.inc }

uses
 Math ,
 agg_basics ,
 agg_array ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 curve_approximation_method_e = (curve_inc ,curve_div );

 curve_ptr = ^curve;
 curve = object(vertex_source )
   procedure reset; virtual; abstract;
   procedure init3(x1 ,y1 ,x2, y2 ,x3 ,y3 : double ); virtual;
   procedure init4(x1 ,y1 ,x2, y2 ,x3 ,y3 ,x4 ,y4 : double ); virtual;

   procedure approximation_method_(v : curve_approximation_method_e ); virtual; abstract;
   function  _approximation_method : curve_approximation_method_e; virtual; abstract;

   procedure approximation_scale_(s : double ); virtual; abstract;
   function  _approximation_scale : double; virtual; abstract;

   procedure angle_tolerance_(a : double ); virtual; abstract;
   function  _angle_tolerance : double; virtual; abstract;

   procedure cusp_limit_(v : double ); virtual; abstract;
   function  _cusp_limit : double; virtual; abstract;

  end;

 curve3_inc = object(curve )
   m_num_steps ,
   m_step      : int;

   m_scale   ,
   m_start_x ,
   m_start_y ,
   m_end_x   ,
   m_end_y   ,

   m_fx   ,
   m_fy   ,
   m_dfx  ,
   m_dfy  ,
   m_ddfx ,
   m_ddfy ,

   m_saved_fx  ,
   m_saved_fy  ,
   m_saved_dfx ,
   m_saved_dfy : double;

   constructor Construct; overload;
   constructor Construct(x1 ,y1 ,x2, y2 ,x3 ,y3 : double ); overload;

   procedure reset; virtual;
   procedure init3(x1 ,y1 ,x2, y2 ,x3 ,y3 : double ); virtual;

   procedure approximation_method_(v : curve_approximation_method_e ); virtual;
   function  _approximation_method : curve_approximation_method_e; virtual;

   procedure approximation_scale_(s : double ); virtual;
   function  _approximation_scale : double; virtual;

   procedure angle_tolerance_(a : double ); virtual;
   function  _angle_tolerance : double; virtual;

   procedure cusp_limit_(v : double ); virtual;
   function  _cusp_limit : double; virtual;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 curve3_div = object(curve )
   m_approximation_scale          ,
   m_distance_tolerance_square    ,
   m_distance_tolerance_manhattan ,
   m_angle_tolerance              : double;

   m_count  : unsigned;
   m_points : pod_deque;

   constructor Construct; overload;
   constructor Construct(x1 ,y1 ,x2, y2 ,x3 ,y3 : double ); overload;
   destructor  Destruct; virtual;

   procedure reset; virtual;
   procedure init3(x1 ,y1 ,x2, y2 ,x3 ,y3 : double ); virtual;

   procedure approximation_method_(v : curve_approximation_method_e ); virtual;
   function  _approximation_method : curve_approximation_method_e; virtual;

   procedure approximation_scale_(s : double ); virtual;
   function  _approximation_scale : double; virtual;

   procedure angle_tolerance_(a : double ); virtual;
   function  _angle_tolerance : double; virtual;

   procedure cusp_limit_(v : double ); virtual;
   function  _cusp_limit : double; virtual;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

   procedure bezier          (x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double );
   procedure recursive_bezier(x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double; level : unsigned );

  end;

 curve4_points_ptr = ^curve4_points;
 curve4_points = object
   cp : array[0..7 ] of double;

   constructor Construct; overload;
   constructor Construct(x1 ,y1 ,x2, y2 ,x3 ,y3 ,x4 ,y4 : double ); overload;

   procedure init(x1 ,y1 ,x2, y2 ,x3 ,y3 ,x4 ,y4 : double );

   function  array_operator    (i : unsigned ) : double;
   function  array_operator_ptr(i : unsigned ) : double_ptr;

  end;

 curve4_inc = object(curve )
   m_num_steps ,
   m_step      : int;

   m_scale   ,
   m_start_x ,
   m_start_y ,
   m_end_x   ,
   m_end_y   ,

   m_fx    ,
   m_fy    ,
   m_dfx   ,
   m_dfy   ,
   m_ddfx  ,
   m_ddfy  ,
   m_dddfx ,
   m_dddfy ,

   m_saved_fx   ,
   m_saved_fy   ,
   m_saved_dfx  ,
   m_saved_dfy  ,
   m_saved_ddfx ,
   m_saved_ddfy : double;

   constructor Construct; overload;
   constructor Construct(x1 ,y1 ,x2, y2 ,x3 ,y3 ,x4 ,y4 : double ); overload;
   constructor Construct(cp : curve4_points_ptr ); overload;

   procedure reset; virtual;
   procedure init4(x1 ,y1 ,x2, y2 ,x3 ,y3 ,x4 ,y4 : double ); virtual;
   procedure init (cp : curve4_points_ptr );

   procedure approximation_method_(v : curve_approximation_method_e ); virtual;
   function  _approximation_method : curve_approximation_method_e; virtual;

   procedure approximation_scale_(s : double ); virtual;
   function  _approximation_scale : double; virtual;

   procedure angle_tolerance_(a : double ); virtual;
   function  _angle_tolerance : double; virtual;

   procedure cusp_limit_(v : double ); virtual;
   function  _cusp_limit : double; virtual;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 curve4_div = object(curve )
   m_approximation_scale          ,
   m_distance_tolerance_square    ,
   m_distance_tolerance_manhattan ,

   m_angle_tolerance ,
   m_cusp_limit      : double;

   m_count  : unsigned;
   m_points : pod_deque;

   constructor Construct; overload;
   constructor Construct(x1 ,y1 ,x2, y2 ,x3 ,y3 ,x4 ,y4 : double ); overload;
   constructor Construct(cp : curve4_points_ptr ); overload;
   destructor  Destruct; virtual;

   procedure reset; virtual;
   procedure init4(x1 ,y1 ,x2, y2 ,x3 ,y3 ,x4 ,y4 : double ); virtual;
   procedure init (cp : curve4_points_ptr );

   procedure approximation_method_(v : curve_approximation_method_e ); virtual;
   function  _approximation_method : curve_approximation_method_e; virtual;

   procedure approximation_scale_(s : double ); virtual;
   function  _approximation_scale : double; virtual;

   procedure angle_tolerance_(a : double ); virtual;
   function  _angle_tolerance : double; virtual;

   procedure cusp_limit_(v : double ); virtual;
   function  _cusp_limit : double; virtual;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

   procedure bezier          (x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 : double );
   procedure recursive_bezier(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 : double; level : unsigned );

  end;

 curve3_ptr = ^curve3;
 curve3 = object(curve )
   m_curve_inc : curve3_inc;
   m_curve_div : curve3_div;

   m_approximation_method : curve_approximation_method_e;

   constructor Construct; overload;
   constructor Construct(x1 ,y1 ,x2, y2 ,x3 ,y3 : double ); overload;
   destructor  Destruct; virtual;

   procedure reset; virtual;
   procedure init3(x1 ,y1 ,x2, y2 ,x3 ,y3 : double ); virtual;

   procedure approximation_method_(v : curve_approximation_method_e ); virtual;
   function  _approximation_method : curve_approximation_method_e; virtual;

   procedure approximation_scale_(s : double ); virtual;
   function  _approximation_scale : double; virtual;

   procedure angle_tolerance_(a : double ); virtual;
   function  _angle_tolerance : double; virtual;

   procedure cusp_limit_(v : double ); virtual;
   function  _cusp_limit : double; virtual;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 curve4_ptr = ^curve4;
 curve4 = object(curve )
   m_curve_inc : curve4_inc;
   m_curve_div : curve4_div;

   m_approximation_method : curve_approximation_method_e;

   constructor Construct; overload;
   constructor Construct(x1 ,y1 ,x2, y2 ,x3 ,y3 ,x4 ,y4 : double ); overload;
   constructor Construct(cp : curve4_points_ptr ); overload;
   destructor  Destruct; virtual;

   procedure reset; virtual;
   procedure init4(x1 ,y1 ,x2, y2 ,x3 ,y3 ,x4 ,y4 : double ); virtual;
   procedure init (cp : curve4_points_ptr );

   procedure approximation_method_(v : curve_approximation_method_e ); virtual;
   function  _approximation_method : curve_approximation_method_e; virtual;

   procedure approximation_scale_(s : double ); virtual;
   function  _approximation_scale : double; virtual;

   procedure angle_tolerance_(a : double ); virtual;
   function  _angle_tolerance : double; virtual;

   procedure cusp_limit_(v : double ); virtual;
   function  _cusp_limit : double; virtual;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

{ GLOBAL PROCEDURES }
 function  catrom_to_bezier(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 : double ) : curve4_points; overload;
 function  catrom_to_bezier(cp : curve4_points_ptr ) : curve4_points; overload;

 function  ubspline_to_bezier(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 : double ) : curve4_points; overload;
 function  ubspline_to_bezier(cp : curve4_points_ptr ) : curve4_points; overload;

 function  hermite_to_bezier(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 : double ) : curve4_points; overload;
 function  hermite_to_bezier(cp : curve4_points_ptr ) : curve4_points; overload;


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
const
 curve_distance_epsilon        = 1e-30;
 curve_collinearity_epsilon    = 1e-30;
 curve_angle_tolerance_epsilon = 0.01;
 curve_recursion_limit         = 32;

{ UNIT IMPLEMENTATION }
{ CATROM_TO_BEZIER }
function catrom_to_bezier(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 : double ) : curve4_points;
begin
// Trans. matrix Catmull-Rom to Bezier
//
//  0       1       0       0
//  -1/6    1       1/6     0
//  0       1/6     1       -1/6
//  0       0       1       0
//
 result.Construct(
  x2 ,
  y2 ,
  (-x1 + 6 * x2 + x3 ) / 6 ,
  (-y1 + 6 * y2 + y3 ) / 6 ,
  ( x2 + 6 * x3 - x4 ) / 6 ,
  ( y2 + 6 * y3 - y4 ) / 6 ,
  x3 ,
  y3 );

end;

{ CATROM_TO_BEZIER }
function catrom_to_bezier(cp : curve4_points_ptr ) : curve4_points;
begin
 result:=
  catrom_to_bezier(
   cp.array_operator(0 ) ,
   cp.array_operator(1 ) ,
   cp.array_operator(2 ) ,
   cp.array_operator(3 ) ,
   cp.array_operator(4 ) ,
   cp.array_operator(5 ) ,
   cp.array_operator(6 ) ,
   cp.array_operator(7 ) );

end;

{ UBSPLINE_TO_BEZIER }
function ubspline_to_bezier(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 : double ) : curve4_points;
begin
// Trans. matrix Uniform BSpline to Bezier
//
//  1/6     4/6     1/6     0
//  0       4/6     2/6     0
//  0       2/6     4/6     0
//  0       1/6     4/6     1/6
//
 result.Construct(
  (x1 + 4 * x2 + x3 ) / 6 ,
  (y1 + 4 * y2 + y3 ) / 6 ,
  (4 * x2 + 2 * x3 ) / 6 ,
  (4 * y2 + 2 * y3 ) / 6 ,
  (2 * x2 + 4 * x3 ) / 6 ,
  (2 * y2 + 4 * y3 ) / 6 ,
  (x2 + 4 * x3 + x4 ) / 6 ,
  (y2 + 4 * y3 + y4 ) / 6 );

end;

{ UBSPLINE_TO_BEZIER }
function ubspline_to_bezier(cp : curve4_points_ptr ) : curve4_points;
begin
 result:=
  ubspline_to_bezier(
   cp.array_operator(0 ) ,
   cp.array_operator(1 ) ,
   cp.array_operator(2 ) ,
   cp.array_operator(3 ) ,
   cp.array_operator(4 ) ,
   cp.array_operator(5 ) ,
   cp.array_operator(6 ) ,
   cp.array_operator(7 ) );

end;

{ HERMITE_TO_BEZIER }
function hermite_to_bezier(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 : double ) : curve4_points;
begin
// Trans. matrix Hermite to Bezier
//
//  1       0       0       0
//  1       0       1/3     0
//  0       1       0       -1/3
//  0       1       0       0
//
 result.Construct(
  x1 ,
  y1 ,
  (3 * x1 + x3 ) / 3 ,
  (3 * y1 + y3 ) / 3 ,
  (3 * x2 - x4 ) / 3 ,
  (3 * y2 - y4 ) / 3 ,
  x2 ,
  y2 );

end;

{ HERMITE_TO_BEZIER }
function hermite_to_bezier(cp : curve4_points_ptr ) : curve4_points;
begin
 result:=
  hermite_to_bezier(
   cp.array_operator(0 ) ,
   cp.array_operator(1 ) ,
   cp.array_operator(2 ) ,
   cp.array_operator(3 ) ,
   cp.array_operator(4 ) ,
   cp.array_operator(5 ) ,
   cp.array_operator(6 ) ,
   cp.array_operator(7 ) );

end;

{ INIT3 }
procedure curve.init3;
begin
end;

{ INIT4 }
procedure curve.init4;
begin
end;

{ CONSTRUCT }
constructor curve3_inc.Construct;
begin
 m_num_steps:=0;
 m_step     :=0;
 m_scale    :=1.0;

end;

{ CONSTRUCT }
constructor curve3_inc.Construct(x1 ,y1 ,x2, y2 ,x3 ,y3 : double );
begin
 m_num_steps:=0;
 m_step     :=0;
 m_scale    :=1.0;

 init3(x1 ,y1 ,x2 ,y2 ,x3 ,y3 );

end;

{ RESET }
procedure curve3_inc.reset;
begin
 m_num_steps:=0;
 m_step     :=-1;

end;

{ INIT3 }
procedure curve3_inc.init3;
var
 dx1 ,dy1 ,dx2 ,dy2 ,len ,tmpx ,tmpy ,
 subdivide_step     ,subdivide_step2 : double;

begin
 m_start_x:=x1;
 m_start_y:=y1;
 m_end_x  :=x3;
 m_end_y  :=y3;

 dx1:=x2 - x1;
 dy1:=y2 - y1;
 dx2:=x3 - x2;
 dy2:=y3 - y2;

 len:=Sqrt(dx1 * dx1 + dy1 * dy1 ) + Sqrt(dx2 * dx2 + dy2 * dy2 );

 m_num_steps:=trunc(len * 0.25 * m_scale );

 if m_num_steps < 4 then
  m_num_steps:=4;

 subdivide_step :=1.0 / m_num_steps;
 subdivide_step2:=subdivide_step * subdivide_step;

 tmpx:=(x1 - x2 * 2.0 + x3 ) * subdivide_step2;
 tmpy:=(y1 - y2 * 2.0 + y3 ) * subdivide_step2;

 m_saved_fx:=x1;
 m_fx      :=x1;
 m_saved_fy:=y1;
 m_fy      :=y1;

 m_saved_dfx:=tmpx + (x2 - x1 ) * (2.0 * subdivide_step );
 m_dfx      :=m_saved_dfx;
 m_saved_dfy:=tmpy + (y2 - y1 ) * (2.0 * subdivide_step );
 m_dfy      :=m_saved_dfy;

 m_ddfx:=tmpx * 2.0;
 m_ddfy:=tmpy * 2.0;

 m_step:=m_num_steps;

end;

{ APPROXIMATION_METHOD_ }
procedure curve3_inc.approximation_method_;
begin
end;

{ _APPROXIMATION_METHOD }
function curve3_inc._approximation_method;
begin
 result:=curve_inc;

end;

{ APPROXIMATION_SCALE_ }
procedure curve3_inc.approximation_scale_;
begin
 m_scale:=s;

end;

{ _APPROXIMATION_SCALE }
function curve3_inc._approximation_scale;
begin
 result:=m_scale;

end;

{ ANGLE_TOLERANCE_ }
procedure curve3_inc.angle_tolerance_;
begin
end;

{ _ANGLE_TOLERANCE }
function curve3_inc._angle_tolerance;
begin
 result:=0.0;

end;

{ CUSP_LIMIT_ }
procedure curve3_inc.cusp_limit_;
begin
end;

{ _CUSP_LIMIT }
function curve3_inc._cusp_limit;
begin
 result:=0.0;

end;

{ REWIND }
procedure curve3_inc.rewind;
begin
 if m_num_steps = 0 then
  begin
   m_step:=-1;

   exit;

  end;

 m_step:=m_num_steps;
 m_fx  :=m_saved_fx;
 m_fy  :=m_saved_fy;
 m_dfx :=m_saved_dfx;
 m_dfy :=m_saved_dfy;

end;

{ VERTEX }
function curve3_inc.vertex;
begin
 if m_step < 0 then
  begin
   result:=path_cmd_stop;

   exit;

  end;

 if m_step = m_num_steps then
  begin
   x^:=m_start_x;
   y^:=m_start_y;

   dec(m_step );

   result:=path_cmd_move_to;

   exit;

  end;

 if m_step = 0 then
  begin
   x^:=m_end_x;
   y^:=m_end_y;

   dec(m_step );

   result:=path_cmd_line_to;

   exit;

  end;

 m_fx :=m_fx + m_dfx;
 m_fy :=m_fy + m_dfy;
 m_dfx:=m_dfx + m_ddfx;
 m_dfy:=m_dfy + m_ddfy;

 x^:=m_fx;
 y^:=m_fy;

 dec(m_step );

 result:=path_cmd_line_to;

end;

{ CONSTRUCT }
constructor curve3_div.Construct;
begin
 m_points.Construct(sizeof(point_type ) );

 m_approximation_scale:=1.0;
 m_angle_tolerance    :=0.0;
 m_count              :=0;

end;

{ CONSTRUCT }
constructor curve3_div.Construct(x1 ,y1 ,x2, y2 ,x3 ,y3 : double );
begin
 m_points.Construct(sizeof(point_type ) );

 m_approximation_scale:=1.0;
 m_angle_tolerance    :=0.0;
 m_count              :=0;

 init3(x1 ,y1 ,x2, y2 ,x3 ,y3 );

end;

{ DESTRUCT }
destructor curve3_div.Destruct;
begin
 m_points.Destruct;

end;

{ RESET }
procedure curve3_div.reset;
begin
 m_points.remove_all;

 m_count:=0;

end;

{ INIT3 }
procedure curve3_div.init3;
begin
 m_points.remove_all;

 m_distance_tolerance_square   :=0.5 / m_approximation_scale;
 m_distance_tolerance_square   :=m_distance_tolerance_square * m_distance_tolerance_square;
 m_distance_tolerance_manhattan:=4.0 / m_approximation_scale;

 bezier(x1 ,y1 ,x2 ,y2 ,x3 ,y3 );

 m_count:=0;

end;

{ APPROXIMATION_METHOD_ }
procedure curve3_div.approximation_method_;
begin
end;

{ _APPROXIMATION_METHOD }
function curve3_div._approximation_method;
begin
 result:=curve_div;

end;

{ APPROXIMATION_SCALE_ }
procedure curve3_div.approximation_scale_;
begin
 if s = 0 then
  m_approximation_scale:=0.00001
 else
  m_approximation_scale:=s;

end;

{ _APPROXIMATION_SCALE }
function curve3_div._approximation_scale;
begin
 result:=m_approximation_scale;

end;

{ ANGLE_TOLERANCE_ }
procedure curve3_div.angle_tolerance_;
begin
 m_angle_tolerance:=a;

end;

{ _ANGLE_TOLERANCE }
function curve3_div._angle_tolerance;
begin
 result:=m_angle_tolerance;

end;

{ CUSP_LIMIT_ }
procedure curve3_div.cusp_limit_;
begin
end;

{ _CUSP_LIMIT }
function curve3_div._cusp_limit;
begin
 result:=0.0;

end;

{ REWIND }
procedure curve3_div.rewind;
begin
 m_count:=0;

end;

{ VERTEX }
function curve3_div.vertex;
var
 p : point_type_ptr;

begin
 if m_count >= m_points.size then
  begin
   result:=path_cmd_stop;

   exit;

  end;

 p:=m_points.array_operator(m_count );

 inc(m_count );

 x^:=p.x;
 y^:=p.y;

 if m_count = 1 then
  result:=path_cmd_move_to
 else
  result:=path_cmd_line_to;

end;

{ BEZIER }
procedure curve3_div.bezier;
var
 pt : point_type;

begin
 pt.x:=x1;
 pt.y:=y1;

 m_points.add(@pt );

 recursive_bezier(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,0 );

 pt.x:=x3;
 pt.y:=y3;

 m_points.add(@pt );

end;

{ RECURSIVE_BEZIER }
procedure curve3_div.recursive_bezier;
var
 x12 ,y12 ,x23 ,y23 ,x123 ,y123 ,dx ,dy ,d ,da : double;

 pt : point_type;

begin
 if level > curve_recursion_limit then
  exit;

// Calculate all the mid-points of the line segments
 x12 :=(x1 + x2 ) / 2;
 y12 :=(y1 + y2 ) / 2;
 x23 :=(x2 + x3 ) / 2;
 y23 :=(y2 + y3 ) / 2;
 x123:=(x12 + x23 ) / 2;
 y123:=(y12 + y23 ) / 2;

 dx:=x3 - x1;
 dy:=y3 - y1;
 d :=Abs((x2 - x3 ) * dy - (y2 - y3 ) * dx );

 if d > curve_collinearity_epsilon then
 // Regular care
  if d * d <= m_distance_tolerance_square * (dx * dx + dy * dy ) then
   begin
   // If the curvature doesn't exceed the distance_tolerance value
   // we tend to finish subdivisions.
    if m_angle_tolerance < curve_angle_tolerance_epsilon then
     begin
      pt.x:=x123;
      pt.y:=y123;

      m_points.add(@pt );

      exit;

     end;

   // Angle & Cusp Condition
    da:=Abs(ArcTan2(y3 - y2 ,x3 - x2 ) - ArcTan2(y2 - y1 ,x2 - x1 ) );

    if da >= pi then
     da:=2 * pi - da;

    if da < m_angle_tolerance then
     begin
     // Finally we can stop the recursion
      pt.x:=x123;
      pt.y:=y123;

      m_points.add(@pt );

      exit;

     end;

   end
  else
 else
  if Abs(x1 + x3 - x2 - x2 ) + Abs(y1 + y3 - y2 - y2 ) <= m_distance_tolerance_manhattan then
   begin
    pt.x:=x123;
    pt.y:=y123;

    m_points.add(@pt );

    exit;

   end;

// Continue subdivision
 recursive_bezier(x1   ,y1   ,x12 ,y12 ,x123 ,y123 ,level + 1 );
 recursive_bezier(x123 ,y123 ,x23 ,y23 ,x3   ,y3   ,level + 1 );

end;

{ CONSTRUCT }
constructor curve4_points.Construct;
begin
end;

{ CONSTRUCT }
constructor curve4_points.Construct(x1 ,y1 ,x2, y2 ,x3 ,y3 ,x4 ,y4 : double );
begin
 cp[0 ]:=x1;
 cp[1 ]:=y1;
 cp[2 ]:=x2;
 cp[3 ]:=y2;
 cp[4 ]:=x3;
 cp[5 ]:=y3;
 cp[6 ]:=x4;
 cp[7 ]:=y4;

end;

{ INIT }
procedure curve4_points.init;
begin
 cp[0 ]:=x1;
 cp[1 ]:=y1;
 cp[2 ]:=x2;
 cp[3 ]:=y2;
 cp[4 ]:=x3;
 cp[5 ]:=y3;
 cp[6 ]:=x4;
 cp[7 ]:=y4;

end;

{ ARRAY_OPERATOR }
function curve4_points.array_operator;
begin
 result:=cp[i ];

end;

{ ARRAY_OPERATOR_PTR }
function curve4_points.array_operator_ptr;
begin
 result:=@cp[i ];

end;

{ CONSTRUCT }
constructor curve4_inc.Construct;
begin
 m_num_steps:=0;
 m_step     :=0;
 m_scale    :=1.0;

end;

{ CONSTRUCT }
constructor curve4_inc.Construct(x1 ,y1 ,x2, y2 ,x3 ,y3 ,x4 ,y4 : double );
begin
 m_num_steps:=0;
 m_step     :=0;
 m_scale    :=1.0;

 init4(x1 ,y1 ,x2, y2 ,x3 ,y3 ,x4 ,y4 );

end;

{ CONSTRUCT }
constructor curve4_inc.Construct(cp : curve4_points_ptr );
begin
 m_num_steps:=0;
 m_step     :=0;
 m_scale    :=1.0;

 init4(
  cp.array_operator(0 ) ,
  cp.array_operator(1 ) ,
  cp.array_operator(2 ) ,
  cp.array_operator(3 ) ,
  cp.array_operator(4 ) ,
  cp.array_operator(5 ) ,
  cp.array_operator(6 ) ,
  cp.array_operator(7 ) );

end;

{ RESET }
procedure curve4_inc.reset;
begin
 m_num_steps:=0;
 m_step     :=-1;

end;

{ INIT4 }
procedure curve4_inc.init4;
var
 dx1 ,dy1 ,dx2 ,dy2 ,dx3 ,dy3 ,len ,

 subdivide_step  ,
 subdivide_step2 ,
 subdivide_step3 ,

 pre1 ,pre2 ,pre4 ,pre5 ,tmp1x ,tmp1y ,tmp2x ,tmp2y : double;

begin
 m_start_x:=x1;
 m_start_y:=y1;
 m_end_x  :=x4;
 m_end_y  :=y4;

 dx1:=x2 - x1;
 dy1:=y2 - y1;
 dx2:=x3 - x2;
 dy2:=y3 - y2;
 dx3:=x4 - x3;
 dy3:=y4 - y3;

 len:=
  Sqrt(dx1 * dx1 + dy1 * dy1 ) +
  Sqrt(dx2 * dx2 + dy2 * dy2 ) +
  Sqrt(dx3 * dx3 + dy3 * dy3 );

 m_num_steps:=trunc(len * 0.25 * m_scale );

 if m_num_steps < 4 then
  m_num_steps:=4;

 subdivide_step :=1.0 / m_num_steps;
 subdivide_step2:=subdivide_step * subdivide_step;
 subdivide_step3:=subdivide_step * subdivide_step * subdivide_step;

 pre1:=3.0 * subdivide_step;
 pre2:=3.0 * subdivide_step2;
 pre4:=6.0 * subdivide_step2;
 pre5:=6.0 * subdivide_step3;

 tmp1x:=x1 - x2 * 2.0 + x3;
 tmp1y:=y1 - y2 * 2.0 + y3;

 tmp2x:=(x2 - x3 ) * 3.0 - x1 + x4;
 tmp2y:=(y2 - y3 ) * 3.0 - y1 + y4;

 m_saved_fx:=x1;
 m_fx      :=x1;
 m_saved_fy:=y1;
 m_fy      :=y1;

 m_saved_dfx:=(x2 - x1 ) * pre1 + tmp1x * pre2 + tmp2x * subdivide_step3;
 m_dfx      :=m_saved_dfx;
 m_saved_dfy:=(y2 - y1 ) * pre1 + tmp1y * pre2 + tmp2y * subdivide_step3;
 m_dfy      :=m_saved_dfy;

 m_saved_ddfx:=tmp1x * pre4 + tmp2x * pre5;
 m_ddfx      :=m_saved_ddfx;
 m_saved_ddfy:=tmp1y * pre4 + tmp2y * pre5;
 m_ddfy      :=m_saved_ddfy;

 m_dddfx:=tmp2x * pre5;
 m_dddfy:=tmp2y * pre5;

 m_step:=m_num_steps;

end;

{ INIT }
procedure curve4_inc.init;
begin
 init4(
  cp.array_operator(0 ) ,
  cp.array_operator(1 ) ,
  cp.array_operator(2 ) ,
  cp.array_operator(3 ) ,
  cp.array_operator(4 ) ,
  cp.array_operator(5 ) ,
  cp.array_operator(6 ) ,
  cp.array_operator(7 ) );

end;

{ APPROXIMATION_METHOD_ }
procedure curve4_inc.approximation_method_;
begin
end;

{ _APPROXIMATION_METHOD }
function curve4_inc._approximation_method;
begin
 result:=curve_inc;

end;

{ APPROXIMATION_SCALE_ }
procedure curve4_inc.approximation_scale_;
begin
 m_scale:=s;

end;

{ _APPROXIMATION_SCALE }
function curve4_inc._approximation_scale;
begin
 result:=m_scale;

end;

{ ANGLE_TOLERANCE_ }
procedure curve4_inc.angle_tolerance_;
begin
end;

{ _ANGLE_TOLERANCE }
function curve4_inc._angle_tolerance;
begin
 result:=0.0;

end;

{ CUSP_LIMIT_ }
procedure curve4_inc.cusp_limit_;
begin
end;

{ _CUSP_LIMIT }
function curve4_inc._cusp_limit;
begin
 result:=0.0;

end;

{ REWIND }
procedure curve4_inc.rewind;
begin
 if m_num_steps = 0 then
  begin
   m_step:=-1;

   exit;

  end;

 m_step:=m_num_steps;
 m_fx  :=m_saved_fx;
 m_fy  :=m_saved_fy;
 m_dfx :=m_saved_dfx;
 m_dfy :=m_saved_dfy;
 m_ddfx:=m_saved_ddfx;
 m_ddfy:=m_saved_ddfy;

end;

{ VERTEX }
function curve4_inc.vertex;
begin
 if m_step < 0 then
  begin
   result:=path_cmd_stop;

   exit;

  end;

 if m_step = m_num_steps then
  begin
   x^:=m_start_x;
   y^:=m_start_y;

   dec(m_step );

   result:=path_cmd_move_to;

   exit;

  end;

 if m_step = 0 then
  begin
   x^:=m_end_x;
   y^:=m_end_y;

   dec(m_step );

   result:=path_cmd_line_to;

   exit;

  end;

 m_fx  :=m_fx + m_dfx;
 m_fy  :=m_fy + m_dfy;
 m_dfx :=m_dfx + m_ddfx;
 m_dfy :=m_dfy + m_ddfy;
 m_ddfx:=m_ddfx + m_dddfx;
 m_ddfy:=m_ddfy + m_dddfy;

 x^:=m_fx;
 y^:=m_fy;

 dec(m_step );

 result:=path_cmd_line_to;

end;

{ CONSTRUCT }
constructor curve4_div.Construct;
begin
 m_points.Construct(sizeof(point_type ) );

 m_approximation_scale:=1.0;
 m_angle_tolerance    :=0.0;

 m_cusp_limit:=0.0;
 m_count     :=0;

end;

{ CONSTRUCT }
constructor curve4_div.Construct(x1 ,y1 ,x2, y2 ,x3 ,y3 ,x4 ,y4 : double );
begin
 m_points.Construct(sizeof(point_type ) );

 m_approximation_scale:=1.0;
 m_angle_tolerance    :=0.0;

 m_cusp_limit:=0.0;
 m_count     :=0;

 init4(x1 ,y1 ,x2, y2 ,x3 ,y3 ,x4 ,y4 );

end;

{ CONSTRUCT }
constructor curve4_div.Construct(cp : curve4_points_ptr );
begin
 m_points.Construct(sizeof(point_type ) );

 m_approximation_scale:=1.0;
 m_angle_tolerance    :=0.0;

 m_cusp_limit:=0.0;
 m_count     :=0;

 init4(
  cp.array_operator(0 ) ,
  cp.array_operator(1 ) ,
  cp.array_operator(2 ) ,
  cp.array_operator(3 ) ,
  cp.array_operator(4 ) ,
  cp.array_operator(5 ) ,
  cp.array_operator(6 ) ,
  cp.array_operator(7 ) );

end;

{ DESTRUCT }
destructor curve4_div.Destruct;
begin
 m_points.Destruct;

end;

{ RESET }
procedure curve4_div.reset;
begin
 m_points.remove_all;

 m_count:=0;

end;

{ INIT4 }
procedure curve4_div.init4;
begin
 m_points.remove_all;

 m_distance_tolerance_square   :=0.5 / m_approximation_scale;
 m_distance_tolerance_square   :=m_distance_tolerance_square * m_distance_tolerance_square;
 m_distance_tolerance_manhattan:=4.0 / m_approximation_scale;

 bezier(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 );

 m_count:=0;

end;

{ INIT }
procedure curve4_div.init;
begin
 init4(
  cp.array_operator(0 ) ,
  cp.array_operator(1 ) ,
  cp.array_operator(2 ) ,
  cp.array_operator(3 ) ,
  cp.array_operator(4 ) ,
  cp.array_operator(5 ) ,
  cp.array_operator(6 ) ,
  cp.array_operator(7 ) );

end;

{ APPROXIMATION_METHOD_ }
procedure curve4_div.approximation_method_;
begin
end;

{ _APPROXIMATION_METHOD }
function curve4_div._approximation_method;
begin
 result:=curve_div;

end;

{ APPROXIMATION_SCALE_ }
procedure curve4_div.approximation_scale_;
begin
 if s = 0 then
  m_approximation_scale:=0.00001
 else
  m_approximation_scale:=s;

end;

{ _APPROXIMATION_SCALE }
function curve4_div._approximation_scale;
begin
 result:=m_approximation_scale;

end;

{ ANGLE_TOLERANCE_ }
procedure curve4_div.angle_tolerance_;
begin
 m_angle_tolerance:=a;

end;

{ _ANGLE_TOLERANCE }
function curve4_div._angle_tolerance;
begin
 result:=m_angle_tolerance;

end;

{ CUSP_LIMIT_ }
procedure curve4_div.cusp_limit_;
begin
 if v = 0.0 then
  m_cusp_limit:=0.0
 else
  m_cusp_limit:=pi - v;

end;

{ _CUSP_LIMIT }
function curve4_div._cusp_limit;
begin
 if m_cusp_limit = 0.0 then
  result:=0.0
 else
  result:=pi - m_cusp_limit;

end;

{ REWIND }
procedure curve4_div.rewind;
begin
 m_count:=0;

end;

{ VERTEX }
function curve4_div.vertex;
var
 p : point_type_ptr;

begin
 if m_count >= m_points.size then
  begin
   result:=path_cmd_stop;

   exit;

  end;

 p:=m_points.array_operator(m_count );

 inc(m_count );

 x^:=p.x;
 y^:=p.y;

 if m_count = 1 then
  result:=path_cmd_move_to
 else
  result:=path_cmd_line_to;

end;

{ BEZIER }
procedure curve4_div.bezier;
var
 pt : point_type;

begin
 pt.x:=x1;
 pt.y:=y1;

 m_points.add(@pt );

 recursive_bezier(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 ,0 );

 pt.x:=x4;
 pt.y:=y4;

 m_points.add(@pt );

end;

{ RECURSIVE_BEZIER }
procedure curve4_div.recursive_bezier;
var
 x12 ,y12 ,x23 ,y23 ,x34 ,y34 ,x123 ,y123 ,x234 ,y234 ,x1234 ,y1234 ,

 dx ,dy ,d2 ,d3 ,da1 ,da2 ,a23 : double;

 pt : point_type;

begin
 if level > curve_recursion_limit then
  exit;

// Calculate all the mid-points of the line segments
 x12  :=(x1 + x2 ) / 2;
 y12  :=(y1 + y2 ) / 2;
 x23  :=(x2 + x3 ) / 2;
 y23  :=(y2 + y3 ) / 2;
 x34  :=(x3 + x4 ) / 2;
 y34  :=(y3 + y4 ) / 2;
 x123 :=(x12 + x23 ) / 2;
 y123 :=(y12 + y23 ) / 2;
 x234 :=(x23 + x34 ) / 2;
 y234 :=(y23 + y34 ) / 2;
 x1234:=(x123 + x234 ) / 2;
 y1234:=(y123 + y234 ) / 2;

// Try to approximate the full cubic curve by a single straight line
 dx:=x4 - x1;
 dy:=y4 - y1;

 d2:=Abs((x2 - x4 ) * dy - (y2 - y4 ) * dx );
 d3:=Abs((x3 - x4 ) * dy - (y3 - y4 ) * dx );

 case ((int(d2 > curve_collinearity_epsilon) shl 1 ) +
        int(d3 > curve_collinearity_epsilon ) ) of
  // All collinear OR p1==p4
  0 :
   if Abs(x1 + x3 - x2 - x2 ) +
      Abs(y1 + y3 - y2 - y2 ) +
      Abs(x2 + x4 - x3 - x3 ) +
      Abs(y2 + y4 - y3 - y3 ) <= m_distance_tolerance_manhattan then
    begin
     pt.x:=x1234;
     pt.y:=y1234;

     m_points.add(@pt );

     exit;

    end;

  // p1,p2,p4 are collinear, p3 is considerable
  1 :
   if d3 * d3 <= m_distance_tolerance_square * (dx * dx + dy * dy ) then
    begin
     if m_angle_tolerance < curve_angle_tolerance_epsilon then
      begin
       pt.x:=x23;
       pt.y:=y23;

       m_points.add(@pt );

       exit;

      end;

    // Angle Condition
     da1:=Abs(ArcTan2(y4 - y3 ,x4 - x3 ) - ArcTan2(y3 - y2 ,x3 - x2 ) );

     if da1 >= pi then
      da1:=2 * pi - da1;

     if da1 < m_angle_tolerance then
      begin
       pt.x:=x2;
       pt.y:=y2;

       m_points.add(@pt );

       pt.x:=x3;
       pt.y:=y3;

       m_points.add(@pt );

       exit;

      end;

     if m_cusp_limit <> 0.0 then
      if da1 > m_cusp_limit then
       begin
        pt.x:=x3;
        pt.y:=y3;

        m_points.add(@pt );

        exit;

       end;

    end;

  // p1,p3,p4 are collinear, p2 is considerable
  2 :
   if d2 * d2 <= m_distance_tolerance_square * (dx * dx + dy * dy ) then
    begin
     if m_angle_tolerance < curve_angle_tolerance_epsilon then
      begin
       pt.x:=x23;
       pt.y:=y23;

       m_points.add(@pt );

       exit;

      end;

    // Angle Condition
     da1:=Abs(ArcTan2(y3 - y2 ,x3 - x2 ) - ArcTan2(y2 - y1 ,x2 - x1 ) );

     if da1 >= pi then
      da1:=2 * pi - da1;

     if da1 < m_angle_tolerance then
      begin
       pt.x:=x2;
       pt.y:=y2;

       m_points.add(@pt );

       pt.x:=x3;
       pt.y:=y3;

       m_points.add(@pt );

       exit;

      end;

     if m_cusp_limit <> 0.0 then
      if da1 > m_cusp_limit then
       begin
        pt.x:=x2;
        pt.y:=y2;

        m_points.add(@pt );

        exit;

       end;

    end;

  // Regular care
  3 :
   if (d2 + d3 ) * (d2 + d3 ) <= m_distance_tolerance_square * (dx * dx + dy * dy ) then
    begin
    // If the curvature doesn't exceed the distance_tolerance value
    // we tend to finish subdivisions.
     if m_angle_tolerance < curve_angle_tolerance_epsilon then
      begin
       pt.x:=x23;
       pt.y:=y23;

       m_points.add(@pt );

       exit;

      end;

    // Angle & Cusp Condition
     a23:=ArcTan2(y3 - y2 ,x3 - x2 );
     da1:=Abs(a23 - ArcTan2(y2 - y1 ,x2 - x1 ) );
     da2:=Abs(ArcTan2(y4 - y3 ,x4 - x3 ) - a23 );

     if da1 >= pi then
      da1:=2 * pi - da1;

     if da2 >= pi then
      da2:=2 * pi - da2;

     if da1 + da2 < m_angle_tolerance then
      begin
      // Finally we can stop the recursion
       pt.x:=x23;
       pt.y:=y23;

       m_points.add(@pt );

       exit;

      end;

     if m_cusp_limit <> 0.0 then
      begin
       if da1 > m_cusp_limit then
        begin
         pt.x:=x2;
         pt.y:=y2;

         m_points.add(@pt );

         exit;

        end;

       if da2 > m_cusp_limit then
        begin
         pt.x:=x3;
         pt.y:=y3;

         m_points.add(@pt );

         exit;

        end;

      end;

    end;

 end;

// Continue subdivision
 recursive_bezier(x1    ,y1    ,x12  ,y12  ,x123 ,y123 ,x1234 ,y1234 ,level + 1 );
 recursive_bezier(x1234 ,y1234 ,x234 ,y234 ,x34  ,y34  ,x4    ,y4    ,level + 1 );

end;

{ CONSTRUCT }
constructor curve3.Construct;
begin
 m_curve_inc.Construct;
 m_curve_div.Construct;

 m_approximation_method:=curve_div;

end;

{ CONSTRUCT }
constructor curve3.Construct(x1 ,y1 ,x2, y2 ,x3 ,y3 : double );
begin
 m_curve_inc.Construct;
 m_curve_div.Construct;

 m_approximation_method:=curve_div;

 init3(x1 ,y1 ,x2, y2 ,x3 ,y3 );

end;

{ DESTRUCT }
destructor curve3.Destruct;
begin
 m_curve_div.Destruct;

end;

{ RESET }
procedure curve3.reset;
begin
 m_curve_inc.reset;
 m_curve_div.reset;

end;

{ INIT3 }
procedure curve3.init3;
begin
 if m_approximation_method = curve_inc then
  m_curve_inc.init3(x1 ,y1 ,x2 ,y2 ,x3 ,y3 )
 else
  m_curve_div.init3(x1 ,y1 ,x2 ,y2 ,x3 ,y3 );

end;

{ APPROXIMATION_METHOD_ }
procedure curve3.approximation_method_;
begin
 m_approximation_method:=v;

end;

{ _APPROXIMATION_METHOD }
function curve3._approximation_method;
begin
 result:=m_approximation_method;

end;

{ APPROXIMATION_SCALE_ }
procedure curve3.approximation_scale_;
begin
 m_curve_inc.approximation_scale_(s );
 m_curve_div.approximation_scale_(s );

end;

{ _APPROXIMATION_SCALE }
function curve3._approximation_scale;
begin
 result:=m_curve_inc._approximation_scale;

end;

{ ANGLE_TOLERANCE_ }
procedure curve3.angle_tolerance_;
begin
 m_curve_div.angle_tolerance_(a );

end;

{ _ANGLE_TOLERANCE }
function curve3._angle_tolerance;
begin
 result:=m_curve_div._angle_tolerance;

end;

{ CUSP_LIMIT_ }
procedure curve3.cusp_limit_;
begin
 m_curve_div.cusp_limit_(v );

end;

{ _CUSP_LIMIT }
function curve3._cusp_limit;
begin
 result:=m_curve_div._cusp_limit;

end;

{ REWIND }
procedure curve3.rewind;
begin
 if m_approximation_method = curve_inc then
  m_curve_inc.rewind(path_id )
 else
  m_curve_div.rewind(path_id )

end;

{ VERTEX }
function curve3.vertex;
begin
 if m_approximation_method = curve_inc then
  result:=m_curve_inc.vertex(x ,y )
 else
  result:=m_curve_div.vertex(x ,y );

end;

{ CONSTRUCT }
constructor curve4.Construct;
begin
 m_curve_inc.Construct;
 m_curve_div.Construct;

 m_approximation_method:=curve_div;

end;

{ CONSTRUCT }
constructor curve4.Construct(x1 ,y1 ,x2, y2 ,x3 ,y3 ,x4 ,y4 : double );
begin
 m_curve_inc.Construct;
 m_curve_div.Construct;

 m_approximation_method:=curve_div;

 init4(x1 ,y1 ,x2, y2 ,x3 ,y3 ,x4 ,y4 );

end;

{ CONSTRUCT }
constructor curve4.Construct(cp : curve4_points_ptr );
begin
 m_curve_inc.Construct;
 m_curve_div.Construct;

 m_approximation_method:=curve_div;

 init4(
  cp.array_operator(0 ) ,
  cp.array_operator(1 ) ,
  cp.array_operator(2 ) ,
  cp.array_operator(3 ) ,
  cp.array_operator(4 ) ,
  cp.array_operator(5 ) ,
  cp.array_operator(6 ) ,
  cp.array_operator(7 ) );

end;

{ DESTRUCT }
destructor curve4.Destruct;
begin
 m_curve_div.Destruct;

end;

{ RESET }
procedure curve4.reset;
begin
 m_curve_inc.reset;
 m_curve_div.reset;

end;

{ INIT4 }
procedure curve4.init4;
begin
 if m_approximation_method = curve_inc then
  m_curve_inc.init4(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 )
 else
  m_curve_div.init4(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 );

end;

{ INIT }
procedure curve4.init;
begin
 init4(
  cp.array_operator(0 ) ,
  cp.array_operator(1 ) ,
  cp.array_operator(2 ) ,
  cp.array_operator(3 ) ,
  cp.array_operator(4 ) ,
  cp.array_operator(5 ) ,
  cp.array_operator(6 ) ,
  cp.array_operator(7 ) );

end;

{ APPROXIMATION_METHOD_ }
procedure curve4.approximation_method_;
begin
 m_approximation_method:=v;

end;

{ _APPROXIMATION_METHOD }
function curve4._approximation_method;
begin
 result:=m_approximation_method;

end;

{ APPROXIMATION_SCALE_ }
procedure curve4.approximation_scale_;
begin
 m_curve_inc.approximation_scale_(s );
 m_curve_div.approximation_scale_(s );

end;

{ _APPROXIMATION_SCALE }
function curve4._approximation_scale;
begin
 result:=m_curve_inc._approximation_scale;

end;

{ ANGLE_TOLERANCE_ }
procedure curve4.angle_tolerance_;
begin
 m_curve_div.angle_tolerance_(a );

end;

{ _ANGLE_TOLERANCE }
function curve4._angle_tolerance;
begin
 result:=m_curve_div._angle_tolerance;

end;

{ CUSP_LIMIT_ }
procedure curve4.cusp_limit_;
begin
 m_curve_div.cusp_limit_(v );

end;

{ _CUSP_LIMIT }
function curve4._cusp_limit;
begin
 result:=m_curve_div._cusp_limit;

end;

{ REWIND }
procedure curve4.rewind;
begin
 if m_approximation_method = curve_inc then
  m_curve_inc.rewind(path_id )
 else
  m_curve_div.rewind(path_id );

end;

{ VERTEX }
function curve4.vertex;
begin
 if m_approximation_method = curve_inc then
  result:=m_curve_inc.vertex(x ,y )
 else
  result:=m_curve_div.vertex(x ,y );

end;

END.

