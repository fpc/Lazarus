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
// class renderer_markers
//
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 11.02.2006-Milano: Unit port establishment
//
{ agg_renderer_markers.pas }
unit
 agg_renderer_markers ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_color ,
 agg_renderer_base ,
 agg_renderer_primitives ,
 agg_ellipse_bresenham ;

{ TYPES DEFINITION }
type
 marker_e = (

  marker_square ,
  marker_diamond ,
  marker_circle ,
  marker_crossed_circle ,
  marker_semiellipse_left ,
  marker_semiellipse_right ,
  marker_semiellipse_up ,
  marker_semiellipse_down ,
  marker_triangle_left ,
  marker_triangle_right ,
  marker_triangle_up ,
  marker_triangle_down ,
  marker_four_rays ,
  marker_cross ,
  marker_x ,
  marker_dash ,
  marker_dot ,
  marker_pixel ,

  end_of_markers );

 renderer_markers = object(renderer_primitives )
   constructor Construct(rbuf : renderer_base_ptr );

   function  visible(x ,y ,r : int ) : boolean;

   procedure square (x ,y ,r : int );
   procedure diamond(x ,y ,r : int );

   procedure circle        (x ,y ,r : int );
   procedure crossed_circle(x ,y ,r : int );

   procedure semiellipse_left (x ,y ,r : int );
   procedure semiellipse_right(x ,y ,r : int );
   procedure semiellipse_up   (x ,y ,r : int );
   procedure semiellipse_down (x ,y ,r : int );

   procedure triangle_left (x ,y ,r : int );
   procedure triangle_right(x ,y ,r : int );
   procedure triangle_up   (x ,y ,r : int );
   procedure triangle_down (x ,y ,r : int );

   procedure four_rays(x ,y ,r : int );

   procedure cross(x ,y ,r : int );
   procedure xing (x ,y ,r : int );
   procedure dash (x ,y ,r : int );
   procedure dot  (x ,y ,r : int );
   procedure pixel(x ,y ,r : int );

   procedure marker(x ,y ,r : int; type_ : marker_e );

   procedure markers(n : int; x ,y : int_ptr; r : int; type_ : marker_e ); overload;
   procedure markers(n : int; x ,y ,r : int_ptr; type_ : marker_e ); overload;
   procedure markers(n : int; x ,y ,r : int_ptr; fc : aggclr_ptr; type_ : marker_e ); overload;
   procedure markers(n : int; x ,y ,r : int_ptr; fc ,lc : aggclr_ptr; type_ : marker_e ); overload;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor renderer_markers.Construct;
begin
 inherited Construct(rbuf );

end;

{ VISIBLE }
function renderer_markers.visible;
var
 rc : rect;

begin
 rc.Construct(x - r ,y - r ,x + y ,y + r );

 result:=rc.clip(ren.bounding_clip_box );  

end;

{ SQUARE }
procedure renderer_markers.square;
begin
 if visible(x ,y ,r ) then
  if r <> 0 then
   outlined_rectangle(x - r ,y - r ,x + r ,y + r )
  else
   ren.blend_pixel(x ,y ,_fill_color ,cover_full );

end;

{ DIAMOND }
procedure renderer_markers.diamond;
var
 dx ,dy : int;

begin
 if visible(x ,y ,r ) then
  if r <> 0 then
   begin
    dy:=-r;
    dx:=0;

    repeat
     ren.blend_pixel(x - dx ,y + dy ,_line_color ,cover_full );
     ren.blend_pixel(x + dx ,y + dy ,_line_color ,cover_full );
     ren.blend_pixel(x - dx ,y - dy ,_line_color ,cover_full );
     ren.blend_pixel(x + dx ,y - dy ,_line_color ,cover_full );

     if dx <> 0 then
      begin
       ren.blend_hline(x - dx + 1 ,y + dy ,x + dx - 1 ,_fill_color ,cover_full );
       ren.blend_hline(x - dx + 1 ,y - dy ,x + dx - 1 ,_fill_color ,cover_full );

      end;

     inc(dy );
     inc(dx );

    until dy > 0;

   end
  else
   ren.blend_pixel(x ,y ,_fill_color ,cover_full );

end;

{ CIRCLE }
procedure renderer_markers.circle;
begin
 if visible(x ,y ,r ) then
  if r <> 0 then
   outlined_ellipse(x ,y ,r ,r )
  else
   ren.blend_pixel(x ,y ,_fill_color ,cover_full );

end;

{ CROSSED_CIRCLE }
procedure renderer_markers.crossed_circle;
var
 r6 : int;

begin
 if visible(x ,y ,r ) then
  if r <> 0 then
   begin
    outlined_ellipse(x ,y ,r ,r );

    r6:=r + shr_int32(r ,1 );

    if r <= 2 then
     inc(r6 );

    r:=shr_int32(r ,1 );

    ren.blend_hline(x - r6 ,y      ,x - r  ,_line_color ,cover_full );
    ren.blend_hline(x + r  ,y      ,x + r6 ,_line_color ,cover_full );
    ren.blend_vline(x      ,y - r6 ,y - r  ,_line_color ,cover_full );
    ren.blend_vline(x      ,y + r  ,y + r6 ,_line_color ,cover_full );

   end
  else
   ren.blend_pixel(x ,y ,_fill_color ,cover_full );

end;

{ SEMIELLIPSE_LEFT }
procedure renderer_markers.semiellipse_left;
var
 r8 ,dy ,dx : int;

 ei : ellipse_bresenham_interpolator;

begin
 if visible(x ,y ,r ) then
  if r <> 0 then
   begin
    r8:=r * 4 div 5;
    dy:=-r;
    dx:=0;

    ei.Construct(r * 3 div 5 ,r + r8 );

    repeat
     inc(dx ,ei._dx );
     inc(dy ,ei._dy );

     ren.blend_pixel(x + dy ,y + dx ,_line_color ,cover_full );
     ren.blend_pixel(x + dy ,y - dx ,_line_color ,cover_full );

     if (ei._dy <> 0 ) and
        (dx <> 0 ) then
      ren.blend_vline(x + dy ,y - dx + 1 ,y + dx - 1 ,_fill_color ,cover_full );

     ei.inc_operator; 

    until dy >= r8;

    ren.blend_vline(x + dy ,y - dx ,y + dx ,_line_color ,cover_full );

   end
  else
   ren.blend_pixel(x ,y ,_fill_color ,cover_full );

end;

{ SEMIELLIPSE_RIGHT }
procedure renderer_markers.semiellipse_right;
var
 r8 ,dy ,dx : int;

 ei : ellipse_bresenham_interpolator;

begin
 if visible(x ,y ,r ) then
  if r <> 0 then
   begin
    r8:=r * 4 div 5;
    dy:=-r;
    dx:=0;

    ei.Construct(r * 3 div 5 ,r + r8 );

    repeat
     inc(dx ,ei._dx );
     inc(dy ,ei._dy );

     ren.blend_pixel(x - dy ,y + dx ,_line_color ,cover_full );
     ren.blend_pixel(x - dy ,y - dx ,_line_color ,cover_full );

     if (ei._dy <> 0 ) and
        (dx <> 0 ) then
      ren.blend_vline(x - dy ,y - dx + 1 ,y + dx - 1 ,_fill_color ,cover_full );

     ei.inc_operator;

    until dy >= r8;

    ren.blend_vline(x - dy ,y - dx ,y + dx ,_line_color ,cover_full );

   end
  else
   ren.blend_pixel(x ,y ,_fill_color ,cover_full );

end;

{ SEMIELLIPSE_UP }
procedure renderer_markers.semiellipse_up;
var
 r8 ,dy ,dx : int;

 ei : ellipse_bresenham_interpolator;

begin
 if visible(x ,y ,r ) then
  if r <> 0 then
   begin
    r8:=r * 4 div 5;
    dy:=-r;
    dx:=0;

    ei.Construct(r * 3 div 5 ,r + r8 );

    repeat
     inc(dx ,ei._dx );
     inc(dy ,ei._dy );

     ren.blend_pixel(x + dx ,y - dy ,_line_color ,cover_full );
     ren.blend_pixel(x - dx ,y - dy ,_line_color ,cover_full );

     if (ei._dy <> 0 ) and
        (dx <> 0 ) then
      ren.blend_hline(x - dx + 1 ,y - dy ,x + dx - 1 ,_fill_color ,cover_full );

     ei.inc_operator;

    until dy >= r8;

    ren.blend_hline(x - dx ,y - dy - 1 ,x + dx ,_line_color ,cover_full );

   end
  else
   ren.blend_pixel(x ,y ,_fill_color ,cover_full );

end;

{ SEMIELLIPSE_DOWN }
procedure renderer_markers.semiellipse_down;
var
 r8 ,dy ,dx : int;

 ei : ellipse_bresenham_interpolator;

begin
 if visible(x ,y ,r ) then
  if r <> 0 then
   begin
    r8:=r * 4 div 5;
    dy:=-r;
    dx:=0;

    ei.Construct(r * 3 div 5 ,r + r8 );

    repeat
     inc(dx ,ei._dx );
     inc(dy ,ei._dy );

     ren.blend_pixel(x + dx ,y + dy ,_line_color ,cover_full );
     ren.blend_pixel(x - dx ,y + dy ,_line_color ,cover_full );

     if (ei._dy <> 0 ) and
        (dx <> 0 ) then
      ren.blend_hline(x - dx + 1 ,y + dy ,x + dx - 1 ,_fill_color ,cover_full );

     ei.inc_operator;

    until dy >= r8;

    ren.blend_hline(x - dx ,y + dy + 1 ,x + dx ,_line_color ,cover_full );

   end
  else
   ren.blend_pixel(x ,y ,_fill_color ,cover_full );

end;

{ TRIANGLE_LEFT }
procedure renderer_markers.triangle_left;
var
 dy ,dx ,flip ,r6 : int;

begin
 if visible(x ,y ,r ) then
  if r <> 0 then
   begin
    dy  :=-r;
    dx  :=0;
    flip:=0;
    r6  :=r * 3 div 5;

    repeat
     ren.blend_pixel(x + dy ,y - dx ,_line_color ,cover_full );
     ren.blend_pixel(x + dy ,y + dx ,_line_color ,cover_full );

     if dx <> 0 then
      ren.blend_vline(x + dy ,y - dx + 1 ,y + dx - 1 ,_fill_color ,cover_full );

     inc(dy );
     inc(dx ,flip );

     flip:=flip xor 1;

    until dy >= r6;

    ren.blend_vline(x + dy ,y - dx ,y + dx ,_line_color ,cover_full );

   end
  else
   ren.blend_pixel(x ,y ,_fill_color ,cover_full );

end;

{ TRIANGLE_RIGHT }
procedure renderer_markers.triangle_right;
var
 dy ,dx ,flip ,r6 : int;

begin
 if visible(x ,y ,r ) then
  if r <> 0 then
   begin
    dy  :=-r;
    dx  :=0;
    flip:=0;
    r6  :=r * 3 div 5;

    repeat
     ren.blend_pixel(x - dy ,y - dx ,_line_color ,cover_full );
     ren.blend_pixel(x - dy ,y + dx ,_line_color ,cover_full );

     if dx <> 0 then
      ren.blend_vline(x - dy ,y - dx + 1 ,y + dx - 1 ,_fill_color ,cover_full );

     inc(dy );
     inc(dx ,flip );

     flip:=flip xor 1;

    until dy >= r6;

    ren.blend_vline(x - dy ,y - dx ,y + dx ,_line_color ,cover_full );

   end
  else
   ren.blend_pixel(x ,y ,_fill_color ,cover_full );

end;

{ TRIANGLE_UP }
procedure renderer_markers.triangle_up;
var
 dy ,dx ,flip ,r6 : int;

begin
 if visible(x ,y ,r ) then
  if r <> 0 then
   begin
    dy  :=-r;
    dx  :=0;
    flip:=0;
    r6  :=r * 3 div 5;

    repeat
     ren.blend_pixel(x - dx ,y - dy ,_line_color ,cover_full );
     ren.blend_pixel(x + dx ,y - dy ,_line_color ,cover_full );

     if dx <> 0 then
      ren.blend_hline(x - dx + 1 ,y - dy ,x + dx - 1 ,_fill_color ,cover_full );

     inc(dy );
     inc(dx ,flip );

     flip:=flip xor 1;

    until dy >= r6;

    ren.blend_hline(x - dx ,y - dy ,x + dx ,_line_color ,cover_full );

   end
  else
   ren.blend_pixel(x ,y ,_fill_color ,cover_full );

end;

{ TRIANGLE_DOWN }
procedure renderer_markers.triangle_down;
var
 dy ,dx ,flip ,r6 : int;

begin
 if visible(x ,y ,r ) then
  if r <> 0 then
   begin
    dy  :=-r;
    dx  :=0;
    flip:=0;
    r6  :=r * 3 div 5;

    repeat
     ren.blend_pixel(x - dx ,y + dy ,_line_color ,cover_full );
     ren.blend_pixel(x + dx ,y + dy ,_line_color ,cover_full );

     if dx <> 0 then
      ren.blend_hline(x - dx + 1 ,y + dy ,x + dx - 1 ,_fill_color ,cover_full );

     inc(dy );
     inc(dx ,flip );

     flip:=flip xor 1;

    until dy >= r6;

    ren.blend_hline(x - dx ,y + dy ,x + dx ,_line_color ,cover_full );

   end
  else
   ren.blend_pixel(x ,y ,_fill_color ,cover_full );

end;

{ FOUR_RAYS }
procedure renderer_markers.four_rays;
var
 dy ,dx ,flip ,r3 : int;

begin
 if visible(x ,y ,r ) then
  if r <> 0 then
   begin
    dy  :=-r;
    dx  :=0;
    flip:=0;
    r3  :=-(r div 3);

    repeat
     ren.blend_pixel(x - dx ,y + dy ,_line_color ,cover_full );
     ren.blend_pixel(x + dx ,y + dy ,_line_color ,cover_full );
     ren.blend_pixel(x - dx ,y - dy ,_line_color ,cover_full );
     ren.blend_pixel(x + dx ,y - dy ,_line_color ,cover_full );
     ren.blend_pixel(x + dy ,y - dx ,_line_color ,cover_full );
     ren.blend_pixel(x + dy ,y + dx ,_line_color ,cover_full );
     ren.blend_pixel(x - dy ,y - dx ,_line_color ,cover_full );
     ren.blend_pixel(x - dy ,y + dx ,_line_color ,cover_full );

     if dx <> 0 then
      begin
       ren.blend_hline(x - dx + 1 ,y + dy     ,x + dx - 1 ,_fill_color ,cover_full );
       ren.blend_hline(x - dx + 1 ,y - dy     ,x + dx - 1 ,_fill_color ,cover_full );
       ren.blend_vline(x + dy     ,y - dx + 1 ,y + dx - 1 ,_fill_color ,cover_full );
       ren.blend_vline(x - dy     ,y - dx + 1 ,y + dx - 1 ,_fill_color ,cover_full );

      end;

     inc(dy );
     inc(dx ,flip );

     flip:=flip xor 1;

    until dy > r3;

    solid_rectangle(x + r3 + 1 ,y + r3 + 1 ,x - r3 - 1 ,y - r3 - 1 );

   end
  else
   ren.blend_pixel(x ,y ,_fill_color ,cover_full );

end;

{ CROSS }
procedure renderer_markers.cross;
begin
 if visible(x ,y ,r ) then
  if r <> 0 then
   begin
    ren.blend_vline(x     ,y - r ,y + r ,_line_color ,cover_full );
    ren.blend_hline(x - r ,y     ,x + r ,_line_color ,cover_full );

   end
  else
   ren.blend_pixel(x ,y ,_fill_color ,cover_full );

end;

{ XING }
procedure renderer_markers.xing;
var
 dy : int;

begin
 if visible(x ,y ,r ) then
  if r <> 0 then
   begin
    dy:=-r * 7 div 10;

    repeat
     ren.blend_pixel(x + dy ,y + dy ,_line_color ,cover_full );
     ren.blend_pixel(x - dy ,y + dy ,_line_color ,cover_full );
     ren.blend_pixel(x + dy ,y - dy ,_line_color ,cover_full );
     ren.blend_pixel(x - dy ,y - dy ,_line_color ,cover_full );

     inc(dy );

    until dy >= 0;

   end
  else
   ren.blend_pixel(x ,y ,_fill_color ,cover_full );

end;

{ DASH }
procedure renderer_markers.dash;
begin
 if visible(x ,y ,r ) then
  if r <> 0 then
   ren.blend_hline(x - r ,y ,x + r ,_line_color ,cover_full )
  else
   ren.blend_pixel(x ,y ,_fill_color ,cover_full );

end;

{ DOT }
procedure renderer_markers.dot;
begin
 if visible(x ,y ,r ) then
  if r <> 0 then
   solid_ellipse(x ,y ,r ,r )
  else
   ren.blend_pixel(x ,y ,_fill_color ,cover_full );

end;

{ PIXEL }
procedure renderer_markers.pixel;
begin
 ren.blend_pixel(x ,y ,_fill_color ,cover_full );

end;

{ MARKER }
procedure renderer_markers.marker;
begin
 case type_ of
  marker_square            : square           (x ,y ,r );
  marker_diamond           : diamond          (x ,y ,r );
  marker_circle            : circle           (x ,y ,r );
  marker_crossed_circle    : crossed_circle   (x ,y ,r );
  marker_semiellipse_left  : semiellipse_left (x ,y ,r );
  marker_semiellipse_right : semiellipse_right(x ,y ,r );
  marker_semiellipse_up    : semiellipse_up   (x ,y ,r );
  marker_semiellipse_down  : semiellipse_down (x ,y ,r );
  marker_triangle_left     : triangle_left    (x ,y ,r );
  marker_triangle_right    : triangle_right   (x ,y ,r );
  marker_triangle_up       : triangle_up      (x ,y ,r );
  marker_triangle_down     : triangle_down    (x ,y ,r );
  marker_four_rays         : four_rays        (x ,y ,r );
  marker_cross             : cross            (x ,y ,r );
  marker_x                 : xing             (x ,y ,r );
  marker_dash              : dash             (x ,y ,r );
  marker_dot               : dot              (x ,y ,r );
  marker_pixel             : pixel            (x ,y ,r );

 end;

end;

{ MARKERS }
procedure renderer_markers.markers(n : int; x ,y : int_ptr; r : int; type_ : marker_e );
begin
 if n <= 0 then
  exit;

 if r = 0 then
  begin
   repeat
    ren.blend_pixel(x^ ,y^ ,_fill_color ,cover_full );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

   exit;

  end;

 case type_ of
  marker_square :
   repeat
    square(x^ ,y^ ,r );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_diamond :
   repeat
    diamond(x^ ,y^ ,r );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_circle :
   repeat
    circle(x^ ,y^ ,r );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_crossed_circle :
   repeat
    crossed_circle(x^ ,y^ ,r );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_semiellipse_left :
   repeat
    semiellipse_left(x^ ,y^ ,r );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_semiellipse_right :
   repeat
    semiellipse_right(x^ ,y^ ,r );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_semiellipse_up :
   repeat
    semiellipse_up(x^ ,y^ ,r );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_semiellipse_down :
   repeat
    semiellipse_down(x^ ,y^ ,r );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_triangle_left :
   repeat
    triangle_left(x^ ,y^ ,r );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_triangle_right :
   repeat
    triangle_right(x^ ,y^ ,r );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_triangle_up :
   repeat
    triangle_up(x^ ,y^ ,r );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_triangle_down :
   repeat
    triangle_down(x^ ,y^ ,r );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_four_rays :
   repeat
    four_rays(x^ ,y^ ,r );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_cross :
   repeat
    cross(x^ ,y^ ,r );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_x :
   repeat
    xing(x^ ,y^ ,r );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_dash :
   repeat
    dash(x^ ,y^ ,r );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_dot :
   repeat
    dot(x^ ,y^ ,r );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_pixel :
   repeat
    pixel(x^ ,y^ ,r );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    dec(n );

   until n = 0;

 end;

end;

{ MARKERS }
procedure renderer_markers.markers(n : int; x ,y ,r : int_ptr; type_ : marker_e );
begin
 if n <= 0 then
  exit;

 case type_ of
  marker_square :
   repeat
    square(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_diamond :
   repeat
    diamond(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_circle :
   repeat
    circle(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_crossed_circle :
   repeat
    crossed_circle(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_semiellipse_left :
   repeat
    semiellipse_left(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_semiellipse_right :
   repeat
    semiellipse_right(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_semiellipse_up :
   repeat
    semiellipse_up(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_semiellipse_down :
   repeat
    semiellipse_down(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_triangle_left :
   repeat
    triangle_left(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_triangle_right :
   repeat
    triangle_right(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_triangle_up :
   repeat
    triangle_up(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_triangle_down :
   repeat
    triangle_down(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_four_rays :
   repeat
    four_rays(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_cross :
   repeat
    cross(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_x :
   repeat
    xing(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_dash :
   repeat
    dash(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_dot :
   repeat
    dot(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    dec(n );

   until n = 0;

  marker_pixel :
   repeat
    pixel(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    dec(n );

   until n = 0;

 end;

end;

{ MARKERS }
procedure renderer_markers.markers(n : int; x ,y ,r : int_ptr; fc : aggclr_ptr; type_ : marker_e );
begin
 if n <= 0 then
  exit;

 case type_ of
  marker_square :
   repeat
    fill_color_(fc );

    square(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_diamond :
   repeat
    fill_color_(fc );

    diamond(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_circle :
   repeat
    fill_color_(fc );

    circle(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_crossed_circle :
   repeat
    fill_color_(fc );

    crossed_circle(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_semiellipse_left :
   repeat
    fill_color_(fc );

    semiellipse_left(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_semiellipse_right :
   repeat
    fill_color_(fc );

    semiellipse_right(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_semiellipse_up :
   repeat
    fill_color_(fc );

    semiellipse_up(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_semiellipse_down :
   repeat
    fill_color_(fc );

    semiellipse_down(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_triangle_left :
   repeat
    fill_color_(fc );

    triangle_left(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_triangle_right :
   repeat
    fill_color_(fc );

    triangle_right(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_triangle_up :
   repeat
    fill_color_(fc );

    triangle_up(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_triangle_down :
   repeat
    fill_color_(fc );

    triangle_down(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_four_rays :
   repeat
    fill_color_(fc );

    four_rays(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_cross :
   repeat
    fill_color_(fc );

    cross(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_x :
   repeat
    fill_color_(fc );

    xing(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_dash :
   repeat
    fill_color_(fc );

    dash(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_dot :
   repeat
    fill_color_(fc );

    dot(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_pixel :
   repeat
    fill_color_(fc );

    pixel(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

 end;

end;

{ MARKERS }
procedure renderer_markers.markers(n : int; x ,y ,r : int_ptr; fc ,lc : aggclr_ptr; type_ : marker_e );
begin
 if n <= 0 then
  exit;

 case type_ of
  marker_square :
   repeat
    fill_color_(fc );
    line_color_(lc );

    square(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    inc(ptrcomp(lc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_diamond :
   repeat
    fill_color_(fc );
    line_color_(lc );

    diamond(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    inc(ptrcomp(lc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_circle :
   repeat
    fill_color_(fc );
    line_color_(lc );

    circle(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    inc(ptrcomp(lc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_crossed_circle :
   repeat
    fill_color_(fc );
    line_color_(lc );

    crossed_circle(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    inc(ptrcomp(lc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_semiellipse_left :
   repeat
    fill_color_(fc );
    line_color_(lc );

    semiellipse_left(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    inc(ptrcomp(lc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_semiellipse_right :
   repeat
    fill_color_(fc );
    line_color_(lc );

    semiellipse_right(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    inc(ptrcomp(lc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_semiellipse_up :
   repeat
    fill_color_(fc );
    line_color_(lc );

    semiellipse_up(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    inc(ptrcomp(lc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_semiellipse_down :
   repeat
    fill_color_(fc );
    line_color_(lc );

    semiellipse_down(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    inc(ptrcomp(lc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_triangle_left :
   repeat
    fill_color_(fc );
    line_color_(lc );

    triangle_left(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    inc(ptrcomp(lc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_triangle_right :
   repeat
    fill_color_(fc );
    line_color_(lc );

    triangle_right(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    inc(ptrcomp(lc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_triangle_up :
   repeat
    fill_color_(fc );
    line_color_(lc );

    triangle_up(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    inc(ptrcomp(lc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_triangle_down :
   repeat
    fill_color_(fc );
    line_color_(lc );

    triangle_down(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    inc(ptrcomp(lc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_four_rays :
   repeat
    fill_color_(fc );
    line_color_(lc );

    four_rays(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    inc(ptrcomp(lc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_cross :
   repeat
    fill_color_(fc );
    line_color_(lc );

    cross(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    inc(ptrcomp(lc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_x :
   repeat
    fill_color_(fc );
    line_color_(lc );

    xing(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    inc(ptrcomp(lc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_dash :
   repeat
    fill_color_(fc );
    line_color_(lc );

    dash(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    inc(ptrcomp(lc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_dot :
   repeat
    fill_color_(fc );
    line_color_(lc );

    dot(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    inc(ptrcomp(lc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

  marker_pixel :
   repeat
    fill_color_(fc );
    line_color_(lc );

    pixel(x^ ,y^ ,r^ );

    inc(ptrcomp(x ) ,sizeof(int ) );
    inc(ptrcomp(y ) ,sizeof(int ) );
    inc(ptrcomp(r ) ,sizeof(int ) );
    inc(ptrcomp(fc ) ,sizeof(aggclr ) );
    inc(ptrcomp(lc ) ,sizeof(aggclr ) );
    dec(n );

   until n = 0;

 end;

end;

END.

