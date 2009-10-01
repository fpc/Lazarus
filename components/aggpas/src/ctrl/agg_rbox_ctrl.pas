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
// 19.01.2006-Milano: Unit port establishment
//
{ agg_rbox_ctrl.pas }
unit
 agg_rbox_ctrl ;

INTERFACE

{$I agg_mode.inc }

uses
 SysUtils ,
 agg_basics ,
 agg_ctrl ,
 agg_conv_stroke ,
 agg_gsv_text ,
 agg_color ,
 agg_ellipse ,
 agg_math ,
 agg_math_stroke ;

{ TYPES DEFINITION }
type
 rbox_ctrl_impl = object(ctrl )
   m_border_width   ,
   m_border_extra   ,
   m_text_thickness ,
   m_text_height    ,
   m_text_width     : double;

   m_items : array[0..31 ] of char_ptr;
   m_sizes : array[0..31 ] of int8u;

   m_num_items : unsigned;
   m_cur_item  : int;

   m_xs1 ,
   m_ys1 ,
   m_xs2 ,
   m_ys2 ,
   m_dy  : double;

   m_vx ,
   m_vy : array[0..31 ] of double;

   m_draw_item : unsigned;

   m_ellipse      : ellipse;
   m_ellipse_poly : conv_stroke;
   m_text         : gsv_text;
   m_text_poly    : conv_stroke;

   m_idx    ,
   m_vertex : unsigned;

   constructor Construct(x1 ,y1 ,x2 ,y2 : double; flip_y_ : boolean = false );
   destructor  Destruct; virtual;

   procedure border_width_  (t : double; extra : double = 0.0 );
   procedure text_thickness_(t : double );
   procedure text_size_     (h : double; w : double = 0.0 );

   procedure add_item (text : PChar );
   function  _cur_item : int;
   procedure cur_item_(i : int );

   function  in_rect(x ,y : double ) : boolean; virtual;

   function  on_mouse_button_down(x ,y : double ) : boolean; virtual;
   function  on_mouse_button_up  (x ,y : double ) : boolean; virtual;

   function  on_mouse_move(x ,y : double; button_flag : boolean ) : boolean; virtual;
   function  on_arrow_keys(left ,right ,down ,up : boolean ) : boolean; virtual;

  // Vertex source interface
   function  num_paths : unsigned; virtual;
   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

   procedure calc_rbox;

  end;

 rbox_ctrl_ptr = ^rbox_ctrl;
 rbox_ctrl = object(rbox_ctrl_impl )
   m_background_color ,
   m_border_color     ,
   m_text_color       ,
   m_inactive_color   ,
   m_active_color     : aggclr;

   m_colors : array[0..4 ] of aggclr_ptr;

   constructor Construct(x1 ,y1 ,x2 ,y2 : double; flip_y_ : boolean = false );

   procedure background_color_(c : aggclr_ptr );
   procedure border_color_    (c : aggclr_ptr );
   procedure text_color_      (c : aggclr_ptr );
   procedure inactive_color_  (c : aggclr_ptr );
   procedure active_color_    (c : aggclr_ptr );

   function  _color(i : unsigned ) : aggclr_ptr; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor rbox_ctrl_impl.Construct;
begin
 inherited Construct(x1 ,y1 ,x2 ,y2 ,flip_y_ );

 m_ellipse.Construct;
 m_text.Construct;

 m_ellipse_poly.Construct(@m_ellipse );
 m_text_poly.Construct   (@m_text );

 m_border_width  :=1.0;
 m_border_extra  :=0.0;
 m_text_thickness:=1.5;
 m_text_height   :=9.0;
 m_text_width    :=0.0;

 m_num_items:=0;
 m_cur_item :=-1;

 m_idx   :=0;
 m_vertex:=0;

 calc_rbox;

end;

{ DESTRUCT }
destructor rbox_ctrl_impl.Destruct;
begin
 m_ellipse_poly.Destruct;
 m_text_poly.Destruct;

 m_text.Destruct;

 while m_num_items > 0 do
  begin
   agg_freemem(p32(m_items[m_num_items - 1 ] ).ptr ,m_sizes[m_num_items - 1 ] );

   dec(m_num_items );

  end;

end;

{ BORDER_WIDTH_ }
procedure rbox_ctrl_impl.border_width_;
begin
 m_border_width:=t;
 m_border_extra:=extra;

 calc_rbox;

end;

{ TEXT_THICKNESS_ }
procedure rbox_ctrl_impl.text_thickness_;
begin
 m_text_thickness:=t;

end;

{ TEXT_SIZE_ }
procedure rbox_ctrl_impl.text_size_;
begin
 m_text_width :=w;
 m_text_height:=h;

end;

{ ADD_ITEM }
procedure rbox_ctrl_impl.add_item;
begin
 if m_num_items < 32 then
  begin
   if StrLen(text ) > 255 then
    m_sizes[m_num_items ]:=255
   else
    m_sizes[m_num_items ]:=StrLen(text ) + 1;

   agg_getmem(p32(m_items[m_num_items ] ).ptr ,m_sizes[m_num_items ] );

   fillchar(p32(m_items[m_num_items ] ).ptr^ ,m_sizes[m_num_items ] ,0 );
   move    (text^ ,p32(m_items[m_num_items ] ).ptr^ ,m_sizes[m_num_items ] - 1 );

   inc(m_num_items );

  end;

end;

{ _CUR_ITEM }
function rbox_ctrl_impl._cur_item;
begin
 result:=m_cur_item;

end;

{ CUR_ITEM_ }
procedure rbox_ctrl_impl.cur_item_;
begin
 m_cur_item:=i;

end;

{ IN_RECT }
function rbox_ctrl_impl.in_rect;
begin
 inverse_transform_xy(@x ,@y );

 result:=
  (x >= m_x1 ) and
  (x <= m_x2 ) and
  (y >= m_y1 ) and
  (y <= m_y2 );

end;

{ ON_MOUSE_BUTTON_DOWN }
function rbox_ctrl_impl.on_mouse_button_down;
var
 i : unsigned;

 xp ,yp : double;

begin
 inverse_transform_xy(@x ,@y );

 for i:=0 to m_num_items - 1 do
  begin
   xp:=m_xs1 + m_dy / 1.3;
   yp:=m_ys1 + m_dy * i + m_dy / 1.3;

   if calc_distance(x ,y ,xp ,yp ) <= m_text_height / 1.5 then
    begin
     m_cur_item:=i;

     result:=true;

     exit;

    end;

  end;

 result:=false;

end;

{ ON_MOUSE_BUTTON_UP }
function rbox_ctrl_impl.on_mouse_button_up;
begin
 result:=false;

end;

{ ON_MOUSE_MOVE }
function rbox_ctrl_impl.on_mouse_move;
begin
 result:=false;

end;

{ ON_ARROW_KEYS }
function rbox_ctrl_impl.on_arrow_keys;
begin
 if m_cur_item >= 0 then
  begin
   if up or right then
    begin
     inc(m_cur_item );

     if m_cur_item >= m_num_items then
      m_cur_item:=0;

     result:=true;

     exit;

    end;

   if down or left then
    begin
     dec(m_cur_item );

     if m_cur_item < 0 then
      m_cur_item:=m_num_items - 1;

     result:=true;

     exit;

    end;

  end;

 result:=false;

end;

{ NUM_PATHS }
function rbox_ctrl_impl.num_paths;
begin
 result:=5;

end;

{ REWIND }
procedure rbox_ctrl_impl.rewind;
begin
 m_idx:=path_id;
 m_dy :=m_text_height * 2.0;

 m_draw_item:=0;

 case path_id of
  0 : // Background
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

  1 : // Border
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
    m_vx[4 ]:=m_x1 + m_border_width;
    m_vy[4 ]:=m_y1 + m_border_width;
    m_vx[5 ]:=m_x1 + m_border_width;
    m_vy[5 ]:=m_y2 - m_border_width;
    m_vx[6 ]:=m_x2 - m_border_width;
    m_vy[6 ]:=m_y2 - m_border_width;
    m_vx[7 ]:=m_x2 - m_border_width;
    m_vy[7 ]:=m_y1 + m_border_width;

   end;

  2 : // Text
   begin
    m_text.text_       (pointer(m_items[0 ] ) );
    m_text.start_point_(m_xs1 + m_dy * 1.5 ,m_ys1 + m_dy / 2.0 );
    m_text.size_       (m_text_height ,m_text_width );

    m_text_poly.width_    (m_text_thickness);
    m_text_poly.line_join_(round_join );
    m_text_poly.line_cap_ (round_cap );

    m_text_poly.rewind(0 );

   end;

  3 : // Inactive items
   begin
    m_ellipse.init(
     m_xs1 + m_dy / 1.3 ,
     m_ys1 + m_dy / 1.3 ,
     m_text_height / 1.5 ,
     m_text_height / 1.5 ,32 );

    m_ellipse_poly.width_(m_text_thickness );
    m_ellipse_poly.rewind(0 );

   end;

  4 : // Active Item
   if m_cur_item >= 0 then
    begin
     m_ellipse.init(
      m_xs1 + m_dy / 1.3 ,
      m_ys1 + m_dy * m_cur_item + m_dy / 1.3 ,
      m_text_height / 2.0 ,
      m_text_height / 2.0 ,32 );

     m_ellipse.rewind(0 );

    end;

 end;

end;

{ VERTEX }
function rbox_ctrl_impl.vertex;
var
 cmd : unsigned;

label
 _Esc ;

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
    if (m_vertex = 0 ) or
       (m_vertex = 4 ) then
     cmd:=path_cmd_move_to;

    if m_vertex >= 8 then
     cmd:=path_cmd_stop;

    x^:=m_vx[m_vertex ];
    y^:=m_vy[m_vertex ];

    inc(m_vertex );

   end;

  2 :
   begin
    cmd:=m_text_poly.vertex(x ,y );

    if is_stop(cmd ) then
     begin
      inc(m_draw_item );

      if m_draw_item >= m_num_items then
       goto _Esc
      else
       begin
        m_text.text_       (pointer(m_items[m_draw_item ] ) );
        m_text.start_point_(
         m_xs1 + m_dy * 1.5 ,
         m_ys1 + m_dy * (m_draw_item + 1 ) - m_dy / 2.0 );

        m_text_poly.rewind(0 );

        cmd:=m_text_poly.vertex(x ,y );

       end;

     end;

   end;

  3 :
   begin
    cmd:=m_ellipse_poly.vertex(x ,y );

    if is_stop(cmd ) then
     begin
      inc(m_draw_item );

      if m_draw_item >= m_num_items then
       goto _Esc
      else
       begin
        m_ellipse.init(
         m_xs1 + m_dy / 1.3 ,
         m_ys1 + m_dy * m_draw_item + m_dy / 1.3 ,
         m_text_height / 1.5 ,
         m_text_height / 1.5 ,32 );

        m_ellipse_poly.rewind(0 );

        cmd:=m_ellipse_poly.vertex(x ,y );

       end;

     end;

   end;

  4 :
   if m_cur_item >= 0 then
    cmd:=m_ellipse.vertex(x ,y )
   else
    cmd:=path_cmd_stop;

  else
   cmd:=path_cmd_stop;

 end;

_Esc:
 if not is_stop(cmd ) then
  transform_xy(x ,y );

 result:=cmd;

end;

{ CALC_RBOX }
procedure rbox_ctrl_impl.calc_rbox;
begin
 m_xs1:=m_x1 + m_border_width;
 m_ys1:=m_y1 + m_border_width;
 m_xs2:=m_x2 - m_border_width;
 m_ys2:=m_y2 - m_border_width;

end;

{ CONSTRUCT }
constructor rbox_ctrl.Construct;
begin
 inherited Construct(x1 ,y1 ,x2 ,y2 ,flip_y_ );

 m_background_color.ConstrDbl(1.0 ,1.0 ,0.9 );
 m_border_color.ConstrDbl    (0.0 ,0.0 ,0.0 );
 m_text_color.ConstrDbl      (0.0 ,0.0 ,0.0 );
 m_inactive_color.ConstrDbl  (0.0 ,0.0 ,0.0 );
 m_active_color.ConstrDbl    (0.4 ,0.0 ,0.0 );

 m_colors[0 ]:=@m_background_color;
 m_colors[1 ]:=@m_border_color;
 m_colors[2 ]:=@m_text_color;
 m_colors[3 ]:=@m_inactive_color;
 m_colors[4 ]:=@m_active_color;

end;

{ BACKGROUND_COLOR_ }
procedure rbox_ctrl.background_color_;
begin
 m_background_color:=c^;

end;

{ BORDER_COLOR_ }
procedure rbox_ctrl.border_color_;
begin
 m_border_color:=c^;

end;

{ TEXT_COLOR_ }
procedure rbox_ctrl.text_color_;
begin
 m_text_color:=c^;

end;

{ INACTIVE_COLOR_ }
procedure rbox_ctrl.inactive_color_;
begin
 m_inactive_color:=c^;

end;

{ ACTIVE_COLOR_ }
procedure rbox_ctrl.active_color_;
begin
 m_active_color:=c^;

end;

{ _COLOR }
function rbox_ctrl._color;
begin
 result:=m_colors[i ];

end;

END.

