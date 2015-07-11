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
// class renderer_primitives
//
// [Pascal Port History] -----------------------------------------------------
//
// 15.01.2006-Milano: Unit port establishment
//
{ agg_renderer_primitives.pas }
unit
 agg_renderer_primitives ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_renderer_base ,
 agg_color ,
 agg_dda_line ,
 agg_ellipse_bresenham ;

{ TYPES DEFINITION }
type
 renderer_primitives_ptr = ^renderer_primitives;
 renderer_primitives = object
   m_ren : renderer_base_ptr;

   m_fill_color ,
   m_line_color : aggclr;

   m_curr_x ,
   m_curr_y : int;

   constructor Construct(ren_ : renderer_base_ptr );

   function  coord(c : double ) : int;

   procedure fill_color_(c : aggclr_ptr );
   procedure line_color_(c : aggclr_ptr );

   function  _fill_color : aggclr_ptr;
   function  _line_color : aggclr_ptr;

   procedure rectangle         (x1 ,y1 ,x2 ,y2 : int );
   procedure solid_rectangle   (x1 ,y1 ,x2 ,y2 : int );
   procedure outlined_rectangle(x1 ,y1 ,x2 ,y2 : int );

   procedure ellipse         (x ,y ,rx ,ry : int );
   procedure solid_ellipse   (x ,y ,rx ,ry : int );
   procedure outlined_ellipse(x ,y ,rx ,ry : int );

   procedure line(x1 ,y1 ,x2 ,y2 : int; last : boolean = false );

   procedure move_to(x ,y : int );
   procedure line_to(x ,y : int; last : boolean = false );

   function  ren : renderer_base_ptr;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor renderer_primitives.Construct;
begin
 m_fill_color.Construct;
 m_line_color.Construct;

 m_ren:=ren_;

 m_curr_x:=0;
 m_curr_y:=0;

end;

{ COORD }
function renderer_primitives.coord;
begin
 result:=trunc(c * subpixel_size );

end;

{ FILL_COLOR_ }
procedure renderer_primitives.fill_color_;
begin
 m_fill_color:=c^;

end;

{ LINE_COLOR_ }
procedure renderer_primitives.line_color_;
begin
 m_line_color:=c^;

end;

{ _FILL_COLOR }
function renderer_primitives._fill_color;
begin
 result:=@m_fill_color;

end;

{ _LINE_COLOR }
function renderer_primitives._line_color;
begin
 result:=@m_line_color;

end;

{ RECTANGLE }
procedure renderer_primitives.rectangle;
begin
 m_ren.blend_hline(x1     ,y1     ,x2 - 1 ,@m_line_color ,cover_full );
 m_ren.blend_vline(x2     ,y1     ,y2 - 1 ,@m_line_color ,cover_full );
 m_ren.blend_hline(x1 + 1 ,y2     ,x2     ,@m_line_color ,cover_full );
 m_ren.blend_vline(x1     ,y1 + 1 ,y2     ,@m_line_color ,cover_full );

end;

{ SOLID_RECTANGLE }
procedure renderer_primitives.solid_rectangle;
begin
 m_ren.blend_bar(x1 ,y1 ,x2 ,y2 ,@m_fill_color ,cover_full );

end;

{ OUTLINED_RECTANGLE }
procedure renderer_primitives.outlined_rectangle;
begin
 rectangle      (x1 ,y1 ,x2 ,y2 );
 m_ren.blend_bar(x1 + 1 ,y1 + 1 ,x2 - 1 ,y2 - 1 ,@m_fill_color ,cover_full );

end;

{ ELLIPSE }
procedure renderer_primitives.ellipse;
var
 ei : ellipse_bresenham_interpolator;
 dx ,
 dy : int;

begin
 ei.Construct(rx ,ry );

 dx:=0;
 dy:=-ry;

 repeat
  inc(dx ,ei._dx );
  inc(dy ,ei._dy );

  m_ren.blend_pixel(x + dx ,y + dy ,@m_line_color ,cover_full );
  m_ren.blend_pixel(x + dx ,y - dy ,@m_line_color ,cover_full );
  m_ren.blend_pixel(x - dx ,y - dy ,@m_line_color ,cover_full );
  m_ren.blend_pixel(x - dx ,y + dy ,@m_line_color ,cover_full );

  ei.inc_operator;

 until dy >= 0;

end;

{ SOLID_ELLIPSE }
procedure renderer_primitives.solid_ellipse;
var
 ei : ellipse_bresenham_interpolator;

 dx ,dy ,dx0 ,dy0 : int;

begin
 ei.Construct(rx ,ry );

 dx :=0;
 dy :=-ry;
 dy0:=dy;
 dx0:=dx;

 repeat
  inc(dx ,ei._dx );
  inc(dy ,ei._dy );

  if dy <> dy0 then
   begin
    m_ren.blend_hline(x - dx0 ,y + dy0 ,x + dx0 ,@m_fill_color ,cover_full );
    m_ren.blend_hline(x - dx0 ,y - dy0 ,x + dx0 ,@m_fill_color ,cover_full );

   end;

  dx0:=dx;
  dy0:=dy;

  ei.inc_operator;

 until dy >= 0;

 m_ren.blend_hline(x - dx0 ,y + dy0 ,x + dx0 ,@m_fill_color ,cover_full );

end;

{ OUTLINED_ELLIPSE }
procedure renderer_primitives.outlined_ellipse;
var
 ei : ellipse_bresenham_interpolator;
 dx ,
 dy : int;

begin
 ei.Construct(rx ,ry );

 dx:=0;
 dy:=-ry;

 repeat
  inc(dx ,ei._dx );
  inc(dy ,ei._dy );

  m_ren.blend_pixel(x + dx ,y + dy ,@m_line_color ,cover_full );
  m_ren.blend_pixel(x + dx ,y - dy ,@m_line_color ,cover_full );
  m_ren.blend_pixel(x - dx ,y - dy ,@m_line_color ,cover_full );
  m_ren.blend_pixel(x - dx ,y + dy ,@m_line_color ,cover_full );

  if (ei._dy <> 0 ) and
     (dx <> 0 ) then
   begin
    m_ren.blend_hline(x - dx + 1 ,y + dy ,x + dx - 1 ,@m_fill_color ,cover_full );
    m_ren.blend_hline(x - dx + 1 ,y - dy ,x + dx - 1 ,@m_fill_color ,cover_full );

   end;

  ei.inc_operator;

 until dy >= 0;

end;

{ LINE }
procedure renderer_primitives.line;
var
 li  : line_bresenham_interpolator;
 len : unsigned;

begin
 li.Construct(x1 ,y1 ,x2 ,y2 );

 len:=li._len;

 if len = 0 then
  begin
   if last then
    m_ren.blend_pixel(li.line_lr(x1 ) ,li.line_lr(y1 ) ,@m_line_color ,cover_full );

   exit;

  end;

 if last then
  inc(len );

 if li._is_ver then
  repeat
   m_ren.blend_pixel(li._x2 ,li._y1 ,@m_line_color ,cover_full );

   li.vstep;

   dec(len );

  until len = 0
 else 
  repeat
   m_ren.blend_pixel(li._x1 ,li._y2 ,@m_line_color ,cover_full );

   li.hstep;

   dec(len );

  until len = 0;

end;

{ MOVE_TO }
procedure renderer_primitives.move_to;
begin
 m_curr_x:=x;
 m_curr_y:=y;

end;

{ LINE_TO }
procedure renderer_primitives.line_to;
begin
 line(m_curr_x ,m_curr_y ,x ,y ,last );

 m_curr_x:=x;
 m_curr_y:=y;

end;

{ REN }
function renderer_primitives.ren;
begin
 result:=m_ren;

end;

END.

