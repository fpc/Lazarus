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
// 29.01.2006-Milano: Unit port establishment
//
{ agg_scale_ctrl.pas }
unit
 agg_scale_ctrl ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_ctrl ,
 agg_color ,
 agg_ellipse ,
 agg_math ,
 agg_trans_affine ;

{ TYPES DEFINITION }
type
 move_e = (move_nothing ,move_value1 ,move_value2 ,move_slider );

 scale_ctrl_impl = object(ctrl )
   m_border_thickness ,
   m_border_extra     ,

   m_value1 ,
   m_value2 ,
   m_min_d  ,

   m_xs1 ,
   m_ys1 ,
   m_xs2 ,
   m_ys2 ,
   m_pdx ,
   m_pdy : double;

   m_move_what : move_e;
   m_vx ,
   m_vy : array[0..31 ] of double;

   m_ellipse : ellipse;

   m_idx    ,
   m_vertex : unsigned;

   constructor Construct(x1 ,y1 ,x2 ,y2 : double; flip_y : boolean = false );
   destructor  Destruct; virtual;

   procedure border_thickness_(t : double; extra : double = 0.0 );

   procedure resize(x1 ,y1 ,x2 ,y2 : double );

   function  _min_delta : double;
   procedure min_delta_(d : double );

   function  _value1 : double;
   procedure value1_(value : double );

   function  _value2 : double;
   procedure value2_(value : double );

   procedure move(d : double );

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

  end;

 scale_ctrl = object(scale_ctrl_impl )
   m_background_color ,
   m_border_color     ,
   m_pointers_color   ,
   m_slider_color     : aggclr;

   m_colors : array[0..5 ] of aggclr_ptr;

   constructor Construct(x1 ,y1 ,x2 ,y2 : double; flip_y : boolean = false );

   procedure background_color_(c : aggclr_ptr );
   procedure border_color_    (c : aggclr_ptr );
   procedure pointers_color_  (c : aggclr_ptr );
   procedure slider_color_    (c : aggclr_ptr );

   function  _color(i : unsigned ) : aggclr_ptr; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor scale_ctrl_impl.Construct;
begin
 inherited Construct(x1 ,y1 ,x2 ,y2 ,flip_y );

 m_border_thickness:=1.0;

 if Abs(x2 - x1 ) > Abs(y2 - y1 ) then
  m_border_extra:=(y2 - y1 ) / 2
 else
  m_border_extra:=(x2 - x1 ) / 2;

 m_pdx:=0.0;
 m_pdy:=0.0;

 m_move_what:=move_nothing;

 m_value1:=0.3;
 m_value2:=0.7;
 m_min_d :=0.01;

 m_ellipse.Construct;

 calc_box;

end;

{ DESTRUCT }
destructor scale_ctrl_impl.destruct;
begin
end;

{ BORDER_THICKNESS_ }
procedure scale_ctrl_impl.border_thickness_;
begin
 m_border_thickness:=t;
 m_border_extra    :=extra;

 calc_box;

end;

{ RESIZE }
procedure scale_ctrl_impl.resize;
begin
 m_x1:=x1;
 m_y1:=y1;
 m_x2:=x2;
 m_y2:=y2;

 calc_box;

 if Abs(x2 - x1 ) > Abs(y2 - y1 ) then
  m_border_extra:=(y2 - y1 ) / 2
 else
  m_border_extra:=(x2 - x1 ) / 2;

end;

{ _MIN_DELTA }
function scale_ctrl_impl._min_delta;
begin
 result:=m_min_d;

end;

{ MIN_DELTA_ }
procedure scale_ctrl_impl.min_delta_;
begin
 m_min_d:=d;

end;

{ _VALUE1 }
function scale_ctrl_impl._value1;
begin
 result:=m_value1;

end;

{ VALUE1_ }
procedure scale_ctrl_impl.value1_;
begin
 if value < 0.0 then
  value:=0.0;

 if value > 1.0 then
  value:=1.0;

 if m_value2 - value < m_min_d then
  value:=m_value2 - m_min_d;

 m_value1:=value;

end;

{ _VALUE2 }
function scale_ctrl_impl._value2;
begin
 result:=m_value2;

end;

{ VALUE2_ }
procedure scale_ctrl_impl.value2_;
begin
 if value < 0.0 then
  value:=0.0;

 if value > 1.0 then
  value:=1.0;

 if m_value1 + value < m_min_d then
  value:=m_value1 + m_min_d;

 m_value2:=value;

end;

{ MOVE }
procedure scale_ctrl_impl.move;
begin
 m_value1:=m_value1 + d;
 m_value2:=m_value2 + d;

 if m_value1 < 0.0 then
  begin
   m_value2:=m_value2 - m_value1;
   m_value1:=0.0;

  end;

 if m_value2 > 1.0 then
  begin
   m_value1:=m_value1 - m_value2 - 1.0;
   m_value2:=1.0;

  end;

end;

{ IN_RECT }
function scale_ctrl_impl.in_rect;
begin
 inverse_transform_xy(@x ,@y );

 result:=
  (x >= m_x1 ) and
  (x <= m_x2 ) and
  (y >= m_y1 ) and
  (y <= m_y2 );

end;

{ ON_MOUSE_BUTTON_DOWN }
function scale_ctrl_impl.on_mouse_button_down;
var
 xp1 ,xp2 ,ys1 ,ys2 ,xp ,yp : double;

begin
 inverse_transform_xy(@x ,@y );

 if Abs(m_x2 - m_x1 ) > Abs(m_y2 - m_y1 ) then
  begin
   xp1:=m_xs1 + (m_xs2 - m_xs1 ) * m_value1;
   xp2:=m_xs1 + (m_xs2 - m_xs1 ) * m_value2;
   ys1:=m_y1  - m_border_extra / 2.0;
   ys2:=m_y2  + m_border_extra / 2.0;
   yp :=(m_ys1 + m_ys2 ) / 2.0;

   if (x > xp1 ) and
      (y > ys1 ) and
      (x < xp2 ) and
      (y < ys2 ) then
    begin
     m_pdx:=xp1 - x;

     m_move_what:=move_slider;

     result:=true;

     exit;

    end;

  //if(x < xp1 && calc_distance(x, y, xp1, yp) <= m_y2 - m_y1)
   if calc_distance(x ,y ,xp1 ,yp ) <= m_y2 - m_y1 then
    begin
     m_pdx:=xp1 - x;

     m_move_what:=move_value1;

     result:=true;

     exit;

    end;

  //if(x > xp2 && calc_distance(x, y, xp2, yp) <= m_y2 - m_y1)
    if calc_distance(x ,y ,xp2 ,yp ) <= m_y2 - m_y1 then
     begin
      m_pdx:=xp2 - x;

      m_move_what:=move_value2;

      result:=true;

     end;

  end
 else
  begin
   xp1:=m_x1  - m_border_extra / 2.0;
   xp2:=m_x2  + m_border_extra / 2.0;
   ys1:=m_ys1 + (m_ys2 - m_ys1 ) * m_value1;
   ys2:=m_ys1 + (m_ys2 - m_ys1 ) * m_value2;
   xp :=(m_xs1 + m_xs2 ) / 2.0;

   if (x > xp1 ) and
      (y > ys1 ) and
      (x < xp2 ) and
      (y < ys2 ) then
    begin
     m_pdy:=ys1 - y;

     m_move_what:=move_slider;

     result:=true;

     exit;

    end;

  //if(y < ys1 && calc_distance(x, y, xp, ys1) <= m_x2 - m_x1)
   if calc_distance(x ,y ,xp ,ys1 ) <= m_x2 - m_x1 then
    begin
     m_pdy:=ys1 - y;

     m_move_what:=move_value1;

     result:=true;

     exit;

    end;

  //if(y > ys2 && calc_distance(x, y, xp, ys2) <= m_x2 - m_x1)
   if calc_distance(x, y, xp, ys2) <= m_x2 - m_x1 then
    begin
     m_pdy:=ys2 - y;

     m_move_what:=move_value2;

     result:=true;

     exit;

    end;

  end;

 result:=false;

end;

{ ON_MOUSE_BUTTON_UP }
function scale_ctrl_impl.on_mouse_button_up;
begin
 m_move_what:=move_nothing;

 result:=false;

end;

{ ON_MOUSE_MOVE }
function scale_ctrl_impl.on_mouse_move;
var
 xp ,yp ,dv : double;

begin
 inverse_transform_xy(@x ,@y );

 if not button_flag then
  result:=on_mouse_button_up(x ,y )
 else
  begin
   xp:=x + m_pdx;
   yp:=y + m_pdy;

   case m_move_what of
    move_value1 :
     begin
      if Abs(m_x2 - m_x1 ) > Abs(m_y2 - m_y1 ) then
       m_value1:=(xp - m_xs1 ) / (m_xs2 - m_xs1 )
      else
       m_value1:=(yp - m_ys1 ) / (m_ys2 - m_ys1 );

      if m_value1 < 0.0 then
       m_value1:=0.0;

      if m_value1 > m_value2 - m_min_d then
       m_value1:=m_value2 - m_min_d;

      result:=true;

     end;

    move_value2 :
     begin
      if Abs(m_x2 - m_x1 ) > Abs(m_y2 - m_y1 ) then
       m_value2:=(xp - m_xs1 ) / (m_xs2 - m_xs1 )
      else
       m_value2:=(yp - m_ys1 ) / (m_ys2 - m_ys1 );

      if m_value2 > 1.0 then
       m_value2:=1.0;

      if m_value2 < m_value1 + m_min_d then
       m_value2:=m_value1 + m_min_d;

      result:=true;

     end;

    move_slider :
     begin
      dv:=m_value2 - m_value1;

      if Abs(m_x2 - m_x1 ) > abs(m_y2 - m_y1 ) then
       m_value1:=(xp - m_xs1 ) / (m_xs2 - m_xs1 )
      else
       m_value1:=(yp - m_ys1 ) / (m_ys2 - m_ys1 );

      m_value2:=m_value1 + dv;

      if m_value1 < 0.0 then
       begin
        dv:=m_value2 - m_value1;

        m_value1:=0.0;
        m_value2:=m_value1 + dv;

       end;

      if m_value2 > 1.0 then
       begin
        dv:=m_value2 - m_value1;

        m_value2:=1.0;
        m_value1:=m_value2 - dv;

       end;

      result:=true;

     end;

    else
     result:=false;

   end;

  end;

end;

{ ON_ARROW_KEYS }
function scale_ctrl_impl.on_arrow_keys;
begin
 result:=false

end;

{ NUM_PATHS }
function scale_ctrl_impl.num_paths;
begin
 result:=5;

end;

{ REWIND }
procedure scale_ctrl_impl.rewind;
begin
 m_idx:=path_id;

 case path_id of
  0: // Background
   begin
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

  1: // Border
   begin
    m_vertex:=0;

    m_vx[0 ]:=m_x1;
    m_vy[0 ]:=m_y1;
    m_vx[1 ]:=m_x2;
    m_vy[1 ]:=m_y1;
    m_vx[2 ]:=m_x2;
    m_vy[2 ]:=m_y2;
    m_vx[3 ]:=m_x1;
    m_vy[3 ]:=m_y2;
    m_vx[4 ]:=m_x1 + m_border_thickness;
    m_vy[4 ]:=m_y1 + m_border_thickness;
    m_vx[5 ]:=m_x1 + m_border_thickness;
    m_vy[5 ]:=m_y2 - m_border_thickness;
    m_vx[6 ]:=m_x2 - m_border_thickness;
    m_vy[6 ]:=m_y2 - m_border_thickness;
    m_vx[7 ]:=m_x2 - m_border_thickness;
    m_vy[7 ]:=m_y1 + m_border_thickness;

   end;

  2: // pointer1
   begin
    if Abs(m_x2 - m_x1 ) > Abs(m_y2 - m_y1 ) then
     m_ellipse.init(
      m_xs1 + (m_xs2 - m_xs1 ) * m_value1 ,
      (m_ys1 + m_ys2 ) / 2.0 ,
      m_y2 - m_y1 ,
      m_y2 - m_y1 ,32 )
    else
     m_ellipse.init(
      (m_xs1 + m_xs2 ) / 2.0,
      m_ys1 + (m_ys2 - m_ys1 ) * m_value1 ,
      m_x2 - m_x1 ,
      m_x2 - m_x1 ,32 );

    m_ellipse.rewind(0 );

   end;

  3: // pointer2
   begin
    if Abs(m_x2 - m_x1 ) > Abs(m_y2 - m_y1 ) then
     m_ellipse.init(
      m_xs1 + (m_xs2 - m_xs1 ) * m_value2 ,
      (m_ys1 + m_ys2 ) / 2.0 ,
      m_y2 - m_y1 ,
      m_y2 - m_y1 ,32 )
    else
     m_ellipse.init(
      (m_xs1 + m_xs2 ) / 2.0 ,
      m_ys1 + (m_ys2 - m_ys1 ) * m_value2 ,
      m_x2 - m_x1 ,
      m_x2 - m_x1 ,32 );

    m_ellipse.rewind(0);

   end;

  4: // slider
   begin
    m_vertex:=0;

    if Abs(m_x2 - m_x1 ) > Abs(m_y2 - m_y1 ) then
     begin
      m_vx[0 ]:=m_xs1 + (m_xs2 - m_xs1 ) * m_value1;
      m_vy[0 ]:=m_y1 - m_border_extra / 2.0;
      m_vx[1 ]:=m_xs1 + (m_xs2 - m_xs1 ) * m_value2;
      m_vy[1 ]:=m_vy[0 ];
      m_vx[2 ]:=m_vx[1 ];
      m_vy[2 ]:=m_y2 + m_border_extra / 2.0;
      m_vx[3 ]:=m_vx[0 ];
      m_vy[3 ]:=m_vy[2 ];

     end
    else
     begin
      m_vx[0 ]:=m_x1 - m_border_extra / 2.0;
      m_vy[0 ]:=m_ys1 + (m_ys2 - m_ys1 ) * m_value1;
      m_vx[1 ]:=m_vx[0 ];
      m_vy[1 ]:=m_ys1 + (m_ys2 - m_ys1 ) * m_value2;
      m_vx[2 ]:=m_x2 + m_border_extra / 2.0;
      m_vy[2 ]:=m_vy[1 ];
      m_vx[3 ]:=m_vx[2 ];
      m_vy[3 ]:=m_vy[0 ];

     end;

   end;

 end;

end;

{ VERTEX }
function scale_ctrl_impl.vertex;
var
 cmd : unsigned;

begin
 cmd:=path_cmd_line_to;

 case m_idx of
  0 ,4 :
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
    if (m_vertex = 0 ) or
       (m_vertex = 4 ) then
     cmd:=path_cmd_move_to;

    if m_vertex >= 8 then
     cmd:=path_cmd_stop;

    x^:=m_vx[m_vertex ];
    y^:=m_vy[m_vertex ];

    inc(m_vertex );

   end;

  2 ,3 :
   cmd:=m_ellipse.vertex(x ,y );

  else
   cmd:=path_cmd_stop;

 end;

 if not is_stop(cmd ) then
  transform_xy(x ,y );

 result:=cmd;

end;

{ CALC_BOX }
procedure scale_ctrl_impl.calc_box;
begin
 m_xs1:=m_x1 + m_border_thickness;
 m_ys1:=m_y1 + m_border_thickness;
 m_xs2:=m_x2 - m_border_thickness;
 m_ys2:=m_y2 - m_border_thickness;

end;

{ CONSTRUCT }
constructor scale_ctrl.Construct;
begin
 inherited Construct(x1 ,y1 ,x2 ,y2 ,flip_y );

 m_background_color.ConstrDbl(1.0 ,0.9 ,0.8 );
 m_border_color.ConstrDbl    (0.0 ,0.0 ,0.0 );
 m_pointers_color.ConstrDbl  (0.8 ,0.0 ,0.0 ,0.8 );
 m_slider_color.ConstrDbl    (0.2 ,0.1 ,0.0 ,0.6 );

 m_colors[0 ]:=@m_background_color;
 m_colors[1 ]:=@m_border_color;
 m_colors[2 ]:=@m_pointers_color;
 m_colors[3 ]:=@m_pointers_color;
 m_colors[4 ]:=@m_slider_color;

end;

{ BACKGROUND_COLOR_ }
procedure scale_ctrl.background_color_;
begin
 m_background_color:=c^;

end;

{ BORDER_COLOR_ }
procedure scale_ctrl.border_color_;
begin
 m_border_color:=c^;

end;

{ POINTERS_COLOR_ }
procedure scale_ctrl.pointers_color_;
begin
 m_pointers_color:=c^;

end;

{ SLIDER_COLOR_ }
procedure scale_ctrl.slider_color_;
begin
 m_slider_color:=c^;

end;

{ _COLOR }
function scale_ctrl._color;
begin
 result:=m_colors[i ];

end;


END.

