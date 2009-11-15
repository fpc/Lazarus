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
// Function render_ctrl
//
// [Pascal Port History] -----------------------------------------------------
//
// 13.12.2005-Milano: Unit port establishment
//
{ agg_ctrl.pas }
unit
 agg_ctrl ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_trans_affine ,
 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_renderer_scanline ,
 agg_render_scanlines ,
 agg_vertex_source ,
 agg_color ;

{ TYPES DEFINITION }
type
 ctrl_ptr = ^ctrl;
 ctrl = object(vertex_source )
   m_x1 ,
   m_y1 ,
   m_x2 ,
   m_y2 : double;

   m_flip_y : boolean;

   m_mtx : trans_affine_ptr;

   constructor Construct(x1 ,y1 ,x2 ,y2 : double; flip_y : boolean );
   destructor  Destruct; virtual;

   function  in_rect(x ,y : double ) : boolean; virtual;

   function  on_mouse_button_down(x ,y : double ) : boolean; virtual;
   function  on_mouse_button_up  (x ,y : double ) : boolean; virtual;

   function  on_mouse_move(x ,y : double; button_flag : boolean ) : boolean; virtual;
   function  on_arrow_keys(left ,right ,down ,up : boolean ) : boolean; virtual;

   procedure transform(mtx : trans_affine_ptr );
   procedure no_transform;

   procedure transform_xy(x ,y : double_ptr );

   procedure inverse_transform_xy(x ,y : double_ptr );

   function  scale : double;

   function  _color(i : unsigned ) : aggclr_ptr; virtual;

  end;

{ GLOBAL PROCEDURES }
 procedure render_ctrl(ras : rasterizer_scanline_ptr; sl : scanline_ptr; r : renderer_scanline_ptr; c : ctrl_ptr );


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor ctrl.Construct;
begin
 inherited Construct;

 m_x1:=x1;
 m_y1:=y1;
 m_x2:=x2;
 m_y2:=y2;

 m_flip_y:=flip_y;

 m_mtx:=NIL;

end;

{ DESTRUCT }
destructor ctrl.Destruct;
begin
 inherited Destruct;

end;

{ IN_RECT }
function ctrl.in_rect;
begin
 result:=false;

end;

{ ON_MOUSE_BUTTON_DOWN }
function ctrl.on_mouse_button_down;
begin
 result:=false;

end;

{ ON_MOUSE_BUTTON_UP }
function ctrl.on_mouse_button_up;
begin
 result:=false;

end;

{ ON_MOUSE_MOVE }
function ctrl.on_mouse_move;
begin
 result:=false;

end;

{ ON_ARROW_KEYS }
function ctrl.on_arrow_keys;
begin
 result:=false;

end;

{ TRANSFORM }
procedure ctrl.transform;
begin
 m_mtx:=mtx;

end;

{ NO_TRANSFORM }
procedure ctrl.no_transform;
begin
 m_mtx:=NIL;

end;

{ TRANSFORM_XY }
procedure ctrl.transform_xy;
begin
 if m_flip_y then
  y^:=m_y1 + m_y2 - y^;

 if m_mtx <> NIL then
  m_mtx.transform(m_mtx ,x ,y );

end;

{ INVERSE_TRANSFORM_XY }
procedure ctrl.inverse_transform_xy;
begin
 if m_mtx <> NIL then
  m_mtx.inverse_transform(m_mtx ,x ,y );

 if m_flip_y then
  y^:=m_y1 + m_y2 - y^;

end;

{ SCALE }
function ctrl.scale;
begin
 if m_mtx <> NIL then
  result:=m_mtx.scale
 else
  result:=1.0; 

end;

{ _COLOR }
function ctrl._color;
begin
 result:=NIL;

end;

{ RENDER_CTRL }
procedure render_ctrl;
var
 i : unsigned;

begin
 if c.num_paths > 0 then
  for i:=0 to c.num_paths - 1 do
   begin
    ras.reset;
    ras.add_path(c ,i );

    r.color_(c._color(i ) );

    render_scanlines(ras ,sl ,r );

   end;

end;

END.

