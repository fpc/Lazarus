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
// classes slider_ctrl_impl, slider_ctrl
//
// [Pascal Port History] -----------------------------------------------------
//
// 21.12.2005-Milano: Complete unit port
// 18.12.2005-Milano: Unit port establishment
//
{ agg_slider_ctrl.pas }
unit
 agg_slider_ctrl ;

INTERFACE

{$I agg_mode.inc }

uses
 SysUtils ,
 agg_basics ,
 agg_ctrl ,
 agg_color ,
 agg_ellipse ,
 agg_path_storage ,
 agg_conv_stroke ,
 agg_gsv_text ,
 agg_math ,
 agg_math_stroke ;

{ TYPES DEFINITION }
type
 slider_ctrl_impl = object(ctrl )
   m_border_width   ,
   m_border_extra   ,
   m_text_thickness ,
   m_value          ,
   m_preview_value  ,

   m_min ,
   m_max : double;

   m_num_steps  : unsigned;
   m_descending : boolean;

   m_label : array[0..63 ] of byte;

   m_xs1 ,
   m_ys1 ,
   m_xs2 ,
   m_ys2 ,
   m_pdx : double;

   m_mouse_move : boolean;

   m_vx ,
   m_vy : array[0..31 ] of double;

   m_ellipse : ellipse;

   m_idx    ,
   m_vertex : unsigned;

   m_text      : gsv_text;
   m_text_poly : conv_stroke;
   m_storage   : path_storage;

   constructor Construct(x1 ,y1 ,x2 ,y2 : double; flip_y : boolean = false );
   destructor  Destruct; virtual;

   procedure border_width_(t : double; extra : double = 0.0 );

   procedure range_         (min ,max : double );
   procedure num_steps_     (num : unsigned );
   procedure label_        (fmt : PChar );
   procedure text_thickness_(t : double );

   function  _descending : boolean;
   procedure descending_(v : boolean );

   function  _value : double;
   procedure value_(v : double );

   function  in_rect(x ,y : double ) : boolean; virtual;

   function  on_mouse_button_down(x ,y : double ) : boolean; virtual;
   function  on_mouse_button_up  (x ,y : double ) : boolean; virtual;

   function  on_mouse_move(x ,y : double; button_flag : boolean ) : boolean; virtual;
   function  on_arrow_keys(left ,right ,down ,up : boolean ) : boolean; virtual;

  // Vertex source interface
   function  num_paths : unsigned; virtual;
   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  // Private
   procedure calc_box;
   function  normalize_value(preview_value_flag : boolean ) : boolean;

  end;

 slider_ctrl_ptr = ^slider_ctrl; 
 slider_ctrl = object(slider_ctrl_impl )
   m_background_color      ,
   m_triangle_color        ,
   m_text_color            ,
   m_pointer_preview_color ,
   m_pointer_color         : aggclr;

   m_colors : array[0..5 ] of aggclr_ptr;

   constructor Construct(x1 ,y1 ,x2 ,y2 : double; flip_y : boolean = false );

   procedure background_color_(c : aggclr_ptr );
   procedure pointer_color_   (c : aggclr_ptr );

   function  _color(i : unsigned ) : aggclr_ptr; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor slider_ctrl_impl.Construct;
begin
 inherited Construct(x1 ,y1 ,x2 ,y2 ,flip_y );

 m_ellipse.Construct;
 m_text.Construct;
 m_text_poly.Construct(@m_text );
 m_storage.Construct;

 m_border_width  :=1.0;
 m_border_extra  :=(y2 - y1 ) / 2;
 m_text_thickness:=1.0;
 m_pdx           :=0.0;
 m_mouse_move    :=false;
 m_value         :=0.5;
 m_preview_value :=0.5;
 m_min           :=0.0;
 m_max           :=1.0;
 m_num_steps     :=0;
 m_descending    :=false;

 m_label[0 ]:=0;

 calc_box;

end;

{ DESTRUCT }
destructor slider_ctrl_impl.Destruct;
begin
 m_storage.Destruct;
 m_text_poly.Destruct;
 m_text.Destruct;

end;

{ BORDER_WIDTH_ }
procedure slider_ctrl_impl.border_width_;
begin
 m_border_width:=t;
 m_border_extra:=extra;

 calc_box;

end;

{ RANGE_ }
procedure slider_ctrl_impl.range_;
begin
 m_min:=min;
 m_max:=max;

end;

{ NUM_STEPS_ }
procedure slider_ctrl_impl.num_steps_;
begin
 m_num_steps:=num;

end;

{ LABEL_ }
procedure slider_ctrl_impl.label_;
var
 len : unsigned;

begin
 m_label[0 ]:=0;

 if fmt <> NIL then
  begin
   len:=StrLen(fmt );

   if len > 63 then
    len:=63;

   move(fmt^ ,m_label[0 ] ,len );

   m_label[len ]:=0;

  end;

end;

{ TEXT_THICKNESS_ }
procedure slider_ctrl_impl.text_thickness_;
begin
 m_text_thickness:=t;

end;

{ _DESCENDING }
function slider_ctrl_impl._descending;
begin
 result:=m_descending;

end;

{ DESCENDING_ }
procedure slider_ctrl_impl.descending_;
begin
 m_descending:=v;

end;

{ _VALUE }
function slider_ctrl_impl._value;
begin
 result:=m_value * (m_max - m_min ) + m_min;

end;

{ VALUE_ }
procedure slider_ctrl_impl.value_;
begin
 m_preview_value:=(v - m_min ) / (m_max - m_min );

 if m_preview_value > 1.0 then
  m_preview_value:=1.0;

 if m_preview_value < 0.0 then
  m_preview_value:=0.0;

 normalize_value(true );

end;

{ IN_RECT }
function slider_ctrl_impl.in_rect;
begin
 inverse_transform_xy(@x ,@y );
 
 result:=
  (x >= m_x1 ) and
  (x <= m_x2 ) and
  (y >= m_y1 ) and
  (y <= m_y2 );

end;

{ ON_MOUSE_BUTTON_DOWN }
function slider_ctrl_impl.on_mouse_button_down;
var
 xp ,yp : double;

begin
 inverse_transform_xy(@x ,@y );

 xp:=m_xs1 + (m_xs2 - m_xs1 ) * m_value;
 yp:=(m_ys1 + m_ys2 ) / 2.0;

 if calc_distance(x ,y ,xp ,yp ) <= m_y2 - m_y1 then
  begin
   m_pdx:=xp - x;

   m_mouse_move:= true;

   result:=true;
   
  end
 else
  result:=false;

end;

{ ON_MOUSE_BUTTON_UP }
function slider_ctrl_impl.on_mouse_button_up;
begin
 m_mouse_move:=false;

 normalize_value(true );

 result:=true;

end;

{ ON_MOUSE_MOVE }
function slider_ctrl_impl.on_mouse_move;
var
 xp : double;

begin
 inverse_transform_xy(@x ,@y );

 if not button_flag then
  begin
   on_mouse_button_up(x ,y );

   result:=false;

   exit;

  end;

 if m_mouse_move then
  begin
   xp:=x + m_pdx;

   m_preview_value:=(xp - m_xs1 ) / (m_xs2 - m_xs1 );

   if m_preview_value < 0.0 then
    m_preview_value:=0.0;

   if m_preview_value > 1.0 then
    m_preview_value:=1.0;

   result:=true;

  end
 else
  result:=false;

end;

{ ON_ARROW_KEYS }
function slider_ctrl_impl.on_arrow_keys;
var
 d : double;

begin
 d:=0.005;

 if m_num_steps <> 0 then
  d:=1.0 / m_num_steps;

 if right or
    up then
  begin
   m_preview_value:=m_preview_value + d;

   if m_preview_value > 1.0 then
    m_preview_value:=1.0;

   normalize_value(true );

   result:=true;

   exit;

  end;

 if left or
    down then
  begin
   m_preview_value:=m_preview_value - d;

   if m_preview_value < 0.0 then
    m_preview_value:=0.0;

   normalize_value(true );

   result:=true;

  end
 else
  result:=false;

end;

{ NUM_PATHS }
function slider_ctrl_impl.num_paths;
begin
 result:=6;

end;

{ REWIND }
procedure slider_ctrl_impl.rewind;
var
 i : unsigned;

 d ,x : double;

 buf : array[0..255 ] of byte;

label
 _0 ;

begin
 m_idx:=path_id;

 case path_id of
  0 : // Background
   begin
   _0 :
    m_vertex:=0;

    m_vx[0 ]:=m_x1 - m_border_extra;
    m_vy[0 ]:=m_y1 - m_border_extra;
    m_vx[1 ]:=m_x2 + m_border_extra;
    m_vy[1 ]:=m_y1 - m_border_extra;
    m_vx[2 ]:=m_x2 + m_border_extra;
    m_vy[2 ]:=m_y2 + m_border_extra;
    m_vx[3 ]:=m_x1 - m_border_extra;
    m_vy[3 ]:=m_y2 + m_border_extra;

   end;

  1 : // Triangle
   begin
    m_vertex:=0;

    if m_descending then
     begin
      m_vx[0 ]:=m_x1;
      m_vy[0 ]:=m_y1;
      m_vx[1 ]:=m_x2;
      m_vy[1 ]:=m_y1;
      m_vx[2 ]:=m_x1;
      m_vy[2 ]:=m_y2;
      m_vx[3 ]:=m_x1;
      m_vy[3 ]:=m_y1;

     end
    else
     begin
      m_vx[0 ]:=m_x1;
      m_vy[0 ]:=m_y1;
      m_vx[1 ]:=m_x2;
      m_vy[1 ]:=m_y1;
      m_vx[2 ]:=m_x2;
      m_vy[2 ]:=m_y2;
      m_vx[3 ]:=m_x1;
      m_vy[3 ]:=m_y1;

     end;

   end;

  2 :
   begin
    m_text.text_(@m_label[0 ] );

    if m_label[0 ] <> 0 then
     begin
      sprintf(@buf[0 ] ,@m_label[0 ] ,_value );

      m_text.text_(@buf[0 ] );

     end;
    
    m_text.start_point_(m_x1 ,m_y1 );
    m_text.size_       ((m_y2 - m_y1 ) * 1.2 ,m_y2 - m_y1 );

    m_text_poly.width_    (m_text_thickness );
    m_text_poly.line_join_(round_join );
    m_text_poly.line_cap_ (round_cap );

    m_text_poly.rewind(0 );

   end;

  3 : // pointer preview
   m_ellipse.init(
    m_xs1 + (m_xs2 - m_xs1 ) * m_preview_value ,
    (m_ys1 + m_ys2 ) / 2.0 ,
    m_y2 - m_y1 ,
    m_y2 - m_y1 ,
    32 );

  4 : // pointer
   begin
    normalize_value(false );

    m_ellipse.init(
     m_xs1 + (m_xs2 - m_xs1 ) * m_value ,
     (m_ys1 + m_ys2 ) / 2.0 ,
     m_y2 - m_y1 ,
     m_y2 - m_y1 ,
     32 );

    m_ellipse.rewind(0 );

   end;

  5 :
   begin
    m_storage.remove_all;

    if m_num_steps <> 0 then
     begin
      d:=(m_xs2 - m_xs1 ) / m_num_steps;

      if d > 0.004 then
       d:=0.004;

      for i:=0 to m_num_steps do
       begin
        x:=m_xs1 + (m_xs2 - m_xs1 ) * i / m_num_steps;

        m_storage.move_to(x ,m_y1 );

        m_storage.line_to(x - d * (m_x2 - m_x1 ) ,m_y1 - m_border_extra );
        m_storage.line_to(x + d * (m_x2 - m_x1 ) ,m_y1 - m_border_extra );

       end;

     end;

   end;

  else
   goto _0;

 end;

end;

{ VERTEX }
function slider_ctrl_impl.vertex;
var
 cmd : unsigned;

begin
 cmd:=path_cmd_line_to;

 case m_idx of
  0 :
   begin
    if m_vertex = 0 then
     cmd:=path_cmd_move_to;

    if m_vertex >= 4 then
     cmd:=path_cmd_stop;

    x^:=m_vx[m_vertex ];
    y^:=m_vy[m_vertex ];

    inc(m_vertex );

   end;
   
  1 :
   begin
    if m_vertex = 0 then
     cmd:=path_cmd_move_to;

    if m_vertex >= 4 then
     cmd:=path_cmd_stop;

    x^:=m_vx[m_vertex ];
    y^:=m_vy[m_vertex ];

    inc(m_vertex );

   end;

  2 :
   cmd:=m_text_poly.vertex(x ,y );

  3 ,4 :
   cmd:=m_ellipse.vertex(x ,y );

  5 :
   cmd:=m_storage.vertex(x ,y );

  else
   cmd:=path_cmd_stop;

 end;

 if not is_stop(cmd ) then
  transform_xy(x ,y );

 result:=cmd;

end;

{ CALC_BOX }
procedure slider_ctrl_impl.calc_box;
begin
 m_xs1:=m_x1 + m_border_width;
 m_ys1:=m_y1 + m_border_width;
 m_xs2:=m_x2 - m_border_width;
 m_ys2:=m_y2 - m_border_width;

end;

{ NORMALIZE_VALUE }
function slider_ctrl_impl.normalize_value;
var
 ret  : boolean;
 step : int;

begin
 ret:=true;

 if m_num_steps <> 0 then
  begin
   step:=trunc(m_preview_value * m_num_steps + 0.5 );
   ret :=m_value <> (step / m_num_steps );

   m_value:=step / m_num_steps;

  end
 else
  m_value:=m_preview_value;

 if preview_value_flag then
  m_preview_value:=m_value;

 result:=ret;

end;

{ CONSTRUCT }
constructor slider_ctrl.Construct;
begin
 inherited Construct(x1 ,y1 ,x2 ,y2 ,flip_y );

 m_background_color.ConstrDbl     (1.0 ,0.9 ,0.8 );
 m_triangle_color.ConstrDbl       (0.7 ,0.6 ,0.6 );
 m_text_color.ConstrDbl           (0.0 ,0.0 ,0.0 );
 m_pointer_preview_color.ConstrDbl(0.6 ,0.4 ,0.4 ,0.4 );
 m_pointer_color.ConstrDbl        (0.8 ,0.0 ,0.0 ,0.6 );

 m_colors[0 ]:=@m_background_color;
 m_colors[1 ]:=@m_triangle_color;
 m_colors[2 ]:=@m_text_color;
 m_colors[3 ]:=@m_pointer_preview_color;
 m_colors[4 ]:=@m_pointer_color;
 m_colors[5 ]:=@m_text_color;

end;

{ BACKGROUND_COLOR_ }
procedure slider_ctrl.background_color_;
begin
 m_background_color:=c^;

end;

{ POINTER_COLOR_ }
procedure slider_ctrl.pointer_color_;
begin
 m_pointer_color:=c^;

end;

{ _COLOR }
function slider_ctrl._color;
begin
 result:=m_colors[i ];

end;

END.

