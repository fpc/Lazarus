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
// classes bezier_ctrl_impl, bezier_ctrl
//
// [Pascal Port History] -----------------------------------------------------
//
// 23.02.2006-Milano: Unit port establishment
//
{ agg_bezier_ctrl.pas }
unit
 agg_bezier_ctrl ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_ctrl ,
 agg_math ,
 agg_ellipse ,
 agg_trans_affine ,
 agg_color ,
 agg_curves ,
 agg_conv_stroke ,
 agg_conv_curve ,
 agg_polygon_ctrl ;

{ TYPES DEFINITION }
type
 bezier_ctrl_impl = object(ctrl )
   m_curve   : curve4;
   m_ellipse : ellipse;
   m_stroke  : conv_stroke;
   m_poly    : polygon_ctrl_impl;
   m_idx     : unsigned;

   constructor Construct;
   destructor  Destruct; virtual;

   procedure curve_(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 : double );
   function  _curve : curve4_ptr;

   function  _x1 : double;
   function  _y1 : double;
   function  _x2 : double;
   function  _y2 : double;
   function  _x3 : double;
   function  _y3 : double;
   function  _x4 : double;
   function  _y4 : double;

   procedure x1_(x : double );
   procedure y1_(y : double );
   procedure x2_(x : double );
   procedure y2_(y : double );
   procedure x3_(x : double );
   procedure y3_(y : double );
   procedure x4_(x : double );
   procedure y4_(y : double );

   procedure line_width_(w : double );
   function  _line_width : double;

   procedure point_radius_(r : double );
   function  _point_radius : double;

  // Event handlers
   function  in_rect(x ,y : double ) : boolean; virtual;

   function  on_mouse_button_down(x ,y : double ) : boolean; virtual;
   function  on_mouse_button_up  (x ,y : double ) : boolean; virtual;

   function  on_mouse_move(x ,y : double; button_flag : boolean ) : boolean; virtual;
   function  on_arrow_keys(left ,right ,down ,up : boolean ) : boolean; virtual;

  // Vertex source interface
   function  num_paths : unsigned; virtual;
   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 bezier_ctrl = object(bezier_ctrl_impl )
   m_color : aggclr;

   constructor Construct;

   procedure line_color_(c : aggclr_ptr );

   function  _color(i : unsigned ) : aggclr_ptr; virtual;

  end;

 curve3_ctrl_impl = object(ctrl )
   m_curve   : curve3;
   m_ellipse : ellipse;
   m_stroke  : conv_stroke;
   m_poly    : polygon_ctrl_impl;
   m_idx     : unsigned;

   constructor Construct;
   destructor  Destruct; virtual;

   procedure curve_(x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double );
   function  _curve : curve3_ptr;

   function  _x1 : double;
   function  _y1 : double;
   function  _x2 : double;
   function  _y2 : double;
   function  _x3 : double;
   function  _y3 : double;

   procedure x1_(x : double );
   procedure y1_(y : double );
   procedure x2_(x : double );
   procedure y2_(y : double );
   procedure x3_(x : double );
   procedure y3_(y : double );

   procedure line_width_(w : double );
   function  _line_width : double;

   procedure point_radius_(r : double );
   function  _point_radius : double;

  // Event handlers
   function  in_rect(x ,y : double ) : boolean; virtual;

   function  on_mouse_button_down(x ,y : double ) : boolean; virtual;
   function  on_mouse_button_up  (x ,y : double ) : boolean; virtual;

   function  on_mouse_move(x ,y : double; button_flag : boolean ) : boolean; virtual;
   function  on_arrow_keys(left ,right ,down ,up : boolean ) : boolean; virtual;

  // Vertex source interface
   function  num_paths : unsigned; virtual;
   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 curve3_ctrl = object(curve3_ctrl_impl )
   m_color : aggclr;

   constructor Construct;

   procedure line_color_(c : aggclr_ptr );

   function  _color(i : unsigned ) : aggclr_ptr; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor bezier_ctrl_impl.Construct;
begin
 inherited Construct(0 ,0 ,1 ,1 ,false );

 m_curve.Construct;
 m_ellipse.Construct;
 m_stroke.Construct(@m_curve );
 m_poly.Construct  (4 ,5.0 );

 m_idx:=0;

 m_poly.in_polygon_check_(false );

 m_poly.xn_ptr(0 )^:=100.0;
 m_poly.yn_ptr(0 )^:=  0.0;
 m_poly.xn_ptr(1 )^:=100.0;
 m_poly.yn_ptr(1 )^:= 50.0;
 m_poly.xn_ptr(2 )^:= 50.0;
 m_poly.yn_ptr(2 )^:=100.0;
 m_poly.xn_ptr(3 )^:=  0.0;
 m_poly.yn_ptr(3 )^:=100.0;

end;

{ DESTRUCT }
destructor bezier_ctrl_impl.Destruct;
begin
 m_curve.Destruct;
 m_stroke.Destruct;
 m_poly.Destruct;

end;

{ CURVE_ }
procedure bezier_ctrl_impl.curve_;
begin
 m_poly.xn_ptr(0 )^:=x1;
 m_poly.yn_ptr(0 )^:=y1;
 m_poly.xn_ptr(1 )^:=x2;
 m_poly.yn_ptr(1 )^:=y2;
 m_poly.xn_ptr(2 )^:=x3;
 m_poly.yn_ptr(2 )^:=y3;
 m_poly.xn_ptr(3 )^:=x4;
 m_poly.yn_ptr(3 )^:=y4;

 _curve;

end;

{ _CURVE }
function bezier_ctrl_impl._curve;
begin
 m_curve.init4(
  m_poly._xn(0 ) ,m_poly._yn(0 ) ,
  m_poly._xn(1 ) ,m_poly._yn(1 ) ,
  m_poly._xn(2 ) ,m_poly._yn(2 ) ,
  m_poly._xn(3 ) ,m_poly._yn(3 ) );

 result:=@m_curve;

end;

{ _X1 }
function bezier_ctrl_impl._x1;
begin
 result:=m_poly._xn(0 );

end;

{ _Y1 }
function bezier_ctrl_impl._y1;
begin
 result:=m_poly._yn(0 );

end;

{ _X2 }
function bezier_ctrl_impl._x2;
begin
 result:=m_poly._xn(1 );

end;

{ _Y2 }
function bezier_ctrl_impl._y2;
begin
 result:=m_poly._yn(1 );

end;

{ _X3 }
function bezier_ctrl_impl._x3;
begin
 result:=m_poly._xn(2 );

end;

{ _Y3 }
function bezier_ctrl_impl._y3;
begin
 result:=m_poly._yn(2 );

end;

{ _X4 }
function bezier_ctrl_impl._x4;
begin
 result:=m_poly._xn(3 );

end;

{ _Y4 }
function bezier_ctrl_impl._y4;
begin
 result:=m_poly._yn(3 );

end;

{ X1_ }
procedure bezier_ctrl_impl.x1_;
begin
 m_poly.xn_ptr(0 )^:=x;

end;

{ Y1_ }
procedure bezier_ctrl_impl.y1_;
begin
 m_poly.yn_ptr(0 )^:=y;

end;

{ X2_ }
procedure bezier_ctrl_impl.x2_;
begin
 m_poly.xn_ptr(1 )^:=x;

end;

{ Y2_ }
procedure bezier_ctrl_impl.y2_;
begin
 m_poly.yn_ptr(1 )^:=y;

end;

{ X3_ }
procedure bezier_ctrl_impl.x3_;
begin
 m_poly.xn_ptr(2 )^:=x;

end;

{ Y3_ }
procedure bezier_ctrl_impl.y3_;
begin
 m_poly.yn_ptr(2 )^:=y;

end;

{ X4_ }
procedure bezier_ctrl_impl.x4_;
begin
 m_poly.xn_ptr(3 )^:=x;

end;

{ Y4_ }
procedure bezier_ctrl_impl.y4_;
begin
 m_poly.yn_ptr(3 )^:=y;

end;

{ LINE_WIDTH_ }
procedure bezier_ctrl_impl.line_width_;
begin
 m_stroke.width_(w );

end;

{ _LINE_WIDTH }
function bezier_ctrl_impl._line_width;
begin
 result:=m_stroke._width;

end;

{ POINT_RADIUS_ }
procedure bezier_ctrl_impl.point_radius_;
begin
 m_poly.point_radius_(r );

end;

{ _POINT_RADIUS }
function bezier_ctrl_impl._point_radius;
begin
 result:=m_poly._point_radius;

end;

{ IN_RECT }
function bezier_ctrl_impl.in_rect;
begin
 result:=false;

end;

{ ON_MOUSE_BUTTON_DOWN }
function bezier_ctrl_impl.on_mouse_button_down;
begin
 inverse_transform_xy(@x ,@y );

 result:=m_poly.on_mouse_button_down(x ,y );

end;

{ ON_MOUSE_BUTTON_UP }
function bezier_ctrl_impl.on_mouse_button_up;
begin
 result:=m_poly.on_mouse_button_up(x ,y );

end;

{ ON_MOUSE_MOVE }
function bezier_ctrl_impl.on_mouse_move;
begin
 inverse_transform_xy(@x ,@y );

 result:=m_poly.on_mouse_move(x ,y ,button_flag );

end;

{ ON_ARROW_KEYS }
function bezier_ctrl_impl.on_arrow_keys;
begin
 result:=m_poly.on_arrow_keys(left ,right ,down ,up );

end;

{ NUM_PATHS }
function bezier_ctrl_impl.num_paths;
begin
 result:=7;

end;

{ REWIND }
procedure bezier_ctrl_impl.rewind;
begin
 m_idx:=path_id;

 m_curve.approximation_scale_(scale );

 case path_id of
  0 : // Control line 1
   begin
    m_curve.init4(
     m_poly._xn(0 ) ,m_poly._yn(0 ) ,
     (m_poly._xn(0 ) + m_poly._xn(1 ) ) * 0.5 ,
     (m_poly._yn(0 ) + m_poly._yn(1 ) ) * 0.5 ,
     (m_poly._xn(0 ) + m_poly._xn(1 ) ) * 0.5 ,
     (m_poly._yn(0 ) + m_poly._yn(1 ) ) * 0.5 ,
     m_poly._xn(1 ) ,m_poly._yn(1 ) );

    m_stroke.rewind(0 );

   end;

  1 : // Control line 2
   begin
    m_curve.init4(
     m_poly._xn(2 ) ,m_poly._yn(2 ) ,
     (m_poly._xn(2 ) + m_poly._xn(3 ) ) * 0.5,
     (m_poly._yn(2 ) + m_poly._yn(3 ) ) * 0.5,
     (m_poly._xn(2 ) + m_poly._xn(3 ) ) * 0.5,
     (m_poly._yn(2 ) + m_poly._yn(3 ) ) * 0.5,
     m_poly._xn(3 ) ,m_poly._yn(3 ) );

     m_stroke.rewind(0 );

   end;

  2 : // Curve itself
   begin
    m_curve.init4(
     m_poly._xn(0 ) ,m_poly._yn(0 ) ,
     m_poly._xn(1 ) ,m_poly._yn(1 ) ,
     m_poly._xn(2 ) ,m_poly._yn(2 ) ,
     m_poly._xn(3 ) ,m_poly._yn(3 ) );

    m_stroke.rewind(0 );

   end;

  3 : // Point 1
   begin
    m_ellipse.init  (m_poly._xn(0 ) ,m_poly._yn(0 ) ,_point_radius ,_point_radius ,20 );
    m_ellipse.rewind(0 );

   end;

  4 : // Point 2
   begin
    m_ellipse.init  (m_poly._xn(1 ) ,m_poly._yn(1 ) ,_point_radius ,_point_radius ,20 );
    m_ellipse.rewind(0 );

   end;

  5 : // Point 3
   begin
    m_ellipse.init  (m_poly._xn(2 ) ,m_poly._yn(2 ) ,_point_radius ,_point_radius ,20 );
    m_ellipse.rewind(0 );

   end;

  6 : // Point 4
   begin
    m_ellipse.init  (m_poly._xn(3 ) ,m_poly._yn(3 ) ,_point_radius ,_point_radius ,20 );
    m_ellipse.rewind(0 );

   end;

 end;

end;

{ VERTEX }
function bezier_ctrl_impl.vertex;
var
 cmd : unsigned;

begin
 cmd:=path_cmd_stop;

 case m_idx of
  0 ,1 ,2 :
   cmd:=m_stroke.vertex(x ,y );

  3 ,4 ,5 ,6 ,7 :
   cmd:=m_ellipse.vertex(x, y);

 end;

 if not is_stop(cmd ) then
  transform_xy(x ,y );

 result:=cmd;

end;

{ CONSTRUCT }
constructor bezier_ctrl.Construct;
begin
 inherited Construct;

 m_color.ConstrDbl(0.0 ,0.0 ,0.0 );

end;

{ LINE_COLOR_ }
procedure bezier_ctrl.line_color_;
begin
 m_color:=c^;

end;

{ _COLOR }
function bezier_ctrl._color;
begin
 result:=@m_color;

end;

{ CONSTRUCT }
constructor curve3_ctrl_impl.Construct;
begin
 inherited Construct(0 ,0 ,1 ,1 ,false );

 m_curve.Construct;
 m_ellipse.Construct;
 m_stroke.Construct(@m_curve );
 m_poly.Construct  (3 ,5.0 );

 m_idx:=0;

 m_poly.in_polygon_check_(false );

 m_poly.xn_ptr(0 )^:=100.0;
 m_poly.yn_ptr(0 )^:=  0.0;
 m_poly.xn_ptr(1 )^:=100.0;
 m_poly.yn_ptr(1 )^:= 50.0;
 m_poly.xn_ptr(2 )^:= 50.0;
 m_poly.yn_ptr(2 )^:=100.0;

end;

{ DESTRUCT }
destructor curve3_ctrl_impl.Destruct;
begin
 m_curve.Destruct;
 m_stroke.Destruct;
 m_poly.Destruct;

end;

{ CURVE_ }
procedure curve3_ctrl_impl.curve_;
begin
 m_poly.xn_ptr(0 )^:=x1;
 m_poly.yn_ptr(0 )^:=y1;
 m_poly.xn_ptr(1 )^:=x2;
 m_poly.yn_ptr(1 )^:=y2;
 m_poly.xn_ptr(2 )^:=x3;
 m_poly.yn_ptr(2 )^:=y3;

 _curve;

end;

{ _CURVE }
function curve3_ctrl_impl._curve;
begin
 m_curve.init3(
  m_poly._xn(0 ) ,m_poly._yn(0 ) ,
  m_poly._xn(1 ) ,m_poly._yn(1 ) ,
  m_poly._xn(2 ) ,m_poly._yn(2 ) );

 result:=@m_curve;

end;

{ _X1 }
function curve3_ctrl_impl._x1;
begin
 result:=m_poly._xn(0 );

end;

{ _Y1 }
function curve3_ctrl_impl._y1;
begin
 result:=m_poly._yn(0 );

end;

{ _X2 }
function curve3_ctrl_impl._x2;
begin
 result:=m_poly._xn(1 );

end;

{ _Y2 }
function curve3_ctrl_impl._y2;
begin
 result:=m_poly._yn(1 );

end;

{ _X3 }
function curve3_ctrl_impl._x3;
begin
 result:=m_poly._xn(2 );

end;

{ _Y3 }
function curve3_ctrl_impl._y3;
begin
 result:=m_poly._yn(2 );

end;

{ X1_ }
procedure curve3_ctrl_impl.x1_;
begin
 m_poly.xn_ptr(0 )^:=x;

end;

{ Y1_ }
procedure curve3_ctrl_impl.y1_;
begin
 m_poly.yn_ptr(0 )^:=y;

end;

{ X2_ }
procedure curve3_ctrl_impl.x2_;
begin
 m_poly.xn_ptr(1 )^:=x;

end;

{ Y2_ }
procedure curve3_ctrl_impl.y2_;
begin
 m_poly.yn_ptr(1 )^:=y;

end;

{ X3_ }
procedure curve3_ctrl_impl.x3_;
begin
 m_poly.xn_ptr(2 )^:=x;

end;

{ Y3_ }
procedure curve3_ctrl_impl.y3_;
begin
 m_poly.yn_ptr(2 )^:=y;

end;

{ LINE_WIDTH_ }
procedure curve3_ctrl_impl.line_width_;
begin
 m_stroke.width_(w );

end;

{ _LINE_WIDTH }
function curve3_ctrl_impl._line_width;
begin
 result:=m_stroke._width;

end;

{ POINT_RADIUS_ }
procedure curve3_ctrl_impl.point_radius_;
begin
 m_poly.point_radius_(r );

end;

{ _POINT_RADIUS }
function curve3_ctrl_impl._point_radius;
begin
 result:=m_poly._point_radius;

end;

{ IN_RECT }
function curve3_ctrl_impl.in_rect;
begin
 result:=false;

end;

{ ON_MOUSE_BUTTON_DOWN }
function curve3_ctrl_impl.on_mouse_button_down;
begin
 inverse_transform_xy(@x ,@y );

 result:=m_poly.on_mouse_button_down(x ,y );

end;

{ ON_MOUSE_BUTTON_UP }
function curve3_ctrl_impl.on_mouse_button_up;
begin
 result:=m_poly.on_mouse_button_up(x ,y );

end;

{ ON_MOUSE_MOVE }
function curve3_ctrl_impl.on_mouse_move;
begin
 inverse_transform_xy(@x ,@y );

 result:=m_poly.on_mouse_move(x ,y ,button_flag );

end;

{ ON_ARROW_KEYS }
function curve3_ctrl_impl.on_arrow_keys;
begin
 result:=m_poly.on_arrow_keys(left ,right ,down ,up );

end;

{ NUM_PATHS }
function curve3_ctrl_impl.num_paths;
begin
 result:=6;

end;

{ REWIND }
procedure curve3_ctrl_impl.rewind;
begin
 m_idx:=path_id;

 case path_id of
  0 : // Control line
   begin
    m_curve.init3(
     m_poly._xn(0 ) ,m_poly._yn(0 ) ,
     (m_poly._xn(0 ) + m_poly._xn(1 ) ) * 0.5,
     (m_poly._yn(0 ) + m_poly._yn(1 ) ) * 0.5,
     m_poly._xn(1 ) ,m_poly._yn(1 ) );

    m_stroke.rewind(0 );

   end;

  1 : // Control line 2
   begin
    m_curve.init3(
     m_poly._xn(1 ) ,m_poly._yn(1 ) ,
     (m_poly._xn(1 ) + m_poly._xn(2 ) ) * 0.5,
     (m_poly._yn(1 ) + m_poly._yn(2 ) ) * 0.5,
     m_poly._xn(2 ) ,m_poly._yn(2 ) );

     m_stroke.rewind(0 );

   end;

  2 : // Curve itself
   begin
    m_curve.init3(
     m_poly._xn(0 ) ,m_poly._yn(0 ) ,
     m_poly._xn(1 ) ,m_poly._yn(1 ) ,
     m_poly._xn(2 ) ,m_poly._yn(2 ) );

    m_stroke.rewind(0 );

   end;

  3 : // Point 1
   begin
    m_ellipse.init  (m_poly._xn(0 ) ,m_poly._yn(0 ) ,_point_radius ,_point_radius ,20 );
    m_ellipse.rewind(0 );

   end;

  4 : // Point 2
   begin
    m_ellipse.init  (m_poly._xn(1 ) ,m_poly._yn(1 ) ,_point_radius ,_point_radius ,20 );
    m_ellipse.rewind(0 );

   end;

  5 : // Point 3
   begin
    m_ellipse.init  (m_poly._xn(2 ) ,m_poly._yn(2 ) ,_point_radius ,_point_radius ,20 );
    m_ellipse.rewind(0 );

   end;

 end;

end;

{ VERTEX }
function curve3_ctrl_impl.vertex;
var
 cmd : unsigned;

begin
 cmd:=path_cmd_stop;

 case m_idx of
  0 ,1 ,2 :
   cmd:=m_stroke.vertex(x ,y );

  3 ,4 ,5 ,6 :
   cmd:=m_ellipse.vertex(x ,y );

 end;

 if not is_stop(cmd ) then
  transform_xy(x ,y );

 result:=cmd;

end;

{ CONSTRUCT }
constructor curve3_ctrl.Construct;
begin
 inherited Construct;

 m_color.ConstrDbl(0.0 ,0.0 ,0.0 );

end;

{ LINE_COLOR_ }
procedure curve3_ctrl.line_color_;
begin
 m_color:=c^;

end;

{ _COLOR }
function curve3_ctrl._color;
begin
 result:=@m_color;

end;

END.

