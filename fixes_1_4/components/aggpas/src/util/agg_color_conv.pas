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
// Conversion from one colorspace/pixel format to another
//
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 16.12.2005-Milano: Unit port establishment
//
{ agg_color_conv.pas }
unit
 agg_color_conv ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_color ,
 agg_rendering_buffer ;

{ TYPES DEFINITION }
type
 CopyRow = procedure(dst ,src : int8u_ptr; width : unsigned );

{ GLOBAL PROCEDURES }
 procedure color_conv(dst ,src : rendering_buffer_ptr; copy_row_functor : CopyRow );

 procedure color_conv_gray8_to_bgr24  (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_gray8_to_rgb24  (dst ,src : int8u_ptr; width : unsigned );

 procedure color_conv_gray16_to_gray8 (dst ,src : int8u_ptr; width : unsigned );

 procedure color_conv_rgb555_to_rgb555(dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_rgb555_to_rgb565(dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_rgb555_to_rgb24 (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_rgb555_to_bgr24 (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_rgb555_to_abgr32(dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_rgb555_to_argb32(dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_rgb555_to_bgra32(dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_rgb555_to_rgba32(dst ,src : int8u_ptr; width : unsigned );

 procedure color_conv_rgb565_to_rgb555(dst ,src : int8u_ptr; width : unsigned );

 procedure color_conv_bgr24_to_gray8  (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgr24_to_gray16 (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgr24_to_rgb555 (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgr24_to_rgb565 (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgr24_to_rgb24  (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgr24_to_bgr24  (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgr24_to_abgr32 (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgr24_to_argb32 (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgr24_to_bgra32 (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgr24_to_rgba32 (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgr24_to_rgb48  (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgr24_to_bgr48  (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgr24_to_abgr64 (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgr24_to_argb64 (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgr24_to_bgra64 (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgr24_to_rgba64 (dst ,src : int8u_ptr; width : unsigned );

 procedure color_conv_rgb24_to_bgr24  (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_rgb24_to_bgra32 (dst ,src : int8u_ptr; width : unsigned );

 procedure color_conv_bgra32_to_rgb555(dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgra32_to_rgb565(dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgra32_to_rgb24 (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgra32_to_bgr24 (dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgra32_to_abgr32(dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgra32_to_argb32(dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgra32_to_bgra32(dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgra32_to_rgba32(dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_bgra64_to_bgra32(dst ,src : int8u_ptr; width : unsigned );

 procedure color_conv_abgr32_to_argb32(dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_abgr32_to_bgra32(dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_abgr64_to_bgra32(dst ,src : int8u_ptr; width : unsigned );

 procedure color_conv_rgba32_to_argb32(dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_rgba32_to_bgra32(dst ,src : int8u_ptr; width : unsigned );

 procedure color_conv_argb32_to_bgra32(dst ,src : int8u_ptr; width : unsigned );
 procedure color_conv_argb64_to_bgra32(dst ,src : int8u_ptr; width : unsigned );

 procedure color_conv_rgbAAA_to_bgr24 (dst ,src : int8u_ptr; width : unsigned );

 procedure color_conv_bgrAAA_to_bgr24 (dst ,src : int8u_ptr; width : unsigned );

 procedure color_conv_rgbBBA_to_bgr24 (dst ,src : int8u_ptr; width : unsigned );

 procedure color_conv_bgrABB_to_bgr24 (dst ,src : int8u_ptr; width : unsigned );

 procedure color_conv_rgb48_to_bgr24  (dst ,src : int8u_ptr; width : unsigned );

 procedure color_conv_bgr48_to_bgr24  (dst ,src : int8u_ptr; width : unsigned );

 procedure color_conv_rgba64_to_bgra32(dst ,src : int8u_ptr; width : unsigned );


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ COLOR_CONV }
procedure color_conv;
var
 y ,width ,height : unsigned;

begin
 width :=src._width;
 height:=src._height;

 if dst._width < width then
  width:=dst._width;

 if dst._height < height then
  height:=dst._height;

 if width > 0 then
  for y:=0 to height - 1 do
   copy_row_functor(dst.row(y ) ,src.row(y ) ,width );

end;

{ color_conv_bgr24_to_gray8 {..}
procedure color_conv_bgr24_to_gray8;
begin
end;

{ color_conv_gray8_to_bgr24 }
procedure color_conv_gray8_to_bgr24;
begin
 repeat
  int8u_ptr(ptrcomp(dst ) + bgr_order.R )^:=src^;
  int8u_ptr(ptrcomp(dst ) + bgr_order.G )^:=src^;
  int8u_ptr(ptrcomp(dst ) + bgr_order.B )^:=src^;

  inc(ptrcomp(dst ) ,3 );
  inc(ptrcomp(src ) );
  dec(width );

 until width = 0;

end;

{ color_conv_gray8_to_rgb24 }
procedure color_conv_gray8_to_rgb24;
begin
 repeat
  int8u_ptr(ptrcomp(dst ) + rgb_order.R )^:=src^;
  int8u_ptr(ptrcomp(dst ) + rgb_order.G )^:=src^;
  int8u_ptr(ptrcomp(dst ) + rgb_order.B )^:=src^;

  inc(ptrcomp(dst ) ,3 );
  inc(ptrcomp(src ) );
  dec(width );

 until width = 0;

end;

{ color_conv_bgr24_to_gray16 {..}
procedure color_conv_bgr24_to_gray16;
begin
end;

{ color_conv_rgb555_to_rgb555 {..}
procedure color_conv_rgb555_to_rgb555;
begin
end;

{ color_conv_bgr24_to_rgb555 {..}
procedure color_conv_bgr24_to_rgb555;
begin
end;

{ color_conv_bgra32_to_rgb555 {..}
procedure color_conv_bgra32_to_rgb555;
begin
end;

{ color_conv_rgb555_to_rgb565 {..}
procedure color_conv_rgb555_to_rgb565;
begin
end;

{ color_conv_bgr24_to_rgb565 {..}
procedure color_conv_bgr24_to_rgb565;
begin
end;

{ color_conv_bgra32_to_rgb565 {..}
procedure color_conv_bgra32_to_rgb565;
begin
end;

{ color_conv_rgb555_to_rgb24 {..}
procedure color_conv_rgb555_to_rgb24;
begin
end;

{ color_conv_bgr24_to_rgb24 }
procedure color_conv_bgr24_to_rgb24;
begin
 repeat
  int8u_ptr(ptrcomp(dst ) + bgr_order.R )^:=int8u_ptr(ptrcomp(src ) + rgb_order.R )^;
  int8u_ptr(ptrcomp(dst ) + bgr_order.G )^:=int8u_ptr(ptrcomp(src ) + rgb_order.G )^;
  int8u_ptr(ptrcomp(dst ) + bgr_order.B )^:=int8u_ptr(ptrcomp(src ) + rgb_order.B )^;

  inc(ptrcomp(dst ) ,3 );
  inc(ptrcomp(src ) ,3 );
  dec(width );

 until width = 0;

end;

{ color_conv_bgra32_to_rgb24 {..}
procedure color_conv_bgra32_to_rgb24;
begin
end;

{ color_conv_rgb555_to_bgr24 {..}
procedure color_conv_rgb555_to_bgr24;
begin
end;

{ color_conv_bgr24_to_bgr24 }
procedure color_conv_bgr24_to_bgr24;
begin
 move(src^ ,dst^ ,width * 3 );

end;

{ color_conv_bgra32_to_bgr24 {..}
procedure color_conv_bgra32_to_bgr24;
begin
end;

{ color_conv_bgr24_to_rgb48 {..}
procedure color_conv_bgr24_to_rgb48;
begin
end;

{ color_conv_bgr24_to_bgr48 {..}
procedure color_conv_bgr24_to_bgr48;
begin
end;

{ color_conv_rgb555_to_abgr32 {..}
procedure color_conv_rgb555_to_abgr32;
begin
end;

{ color_conv_bgr24_to_abgr32 {..}
procedure color_conv_bgr24_to_abgr32;
begin
end;

{ color_conv_bgra32_to_abgr32 {..}
procedure color_conv_bgra32_to_abgr32;
begin
end;

{ color_conv_rgb555_to_argb32 {..}
procedure color_conv_rgb555_to_argb32;
begin
end;

{ color_conv_bgr24_to_argb32 {..}
procedure color_conv_bgr24_to_argb32;
begin
end;

{ color_conv_bgra32_to_argb32 }
procedure color_conv_bgra32_to_argb32;
begin
 repeat
  int8u_ptr(ptrcomp(dst ) + argb_order.R )^:=int8u_ptr(ptrcomp(src ) + bgra_order.R )^;
  int8u_ptr(ptrcomp(dst ) + argb_order.G )^:=int8u_ptr(ptrcomp(src ) + bgra_order.G )^;
  int8u_ptr(ptrcomp(dst ) + argb_order.B )^:=int8u_ptr(ptrcomp(src ) + bgra_order.B )^;
  int8u_ptr(ptrcomp(dst ) + argb_order.A )^:=int8u_ptr(ptrcomp(src ) + bgra_order.A )^;

  inc(ptrcomp(dst ) ,4 );
  inc(ptrcomp(src ) ,4 );
  dec(width );

 until width = 0;

end;

{ color_conv_abgr32_to_argb32 }
procedure color_conv_abgr32_to_argb32;
begin
 repeat
  int8u_ptr(ptrcomp(dst ) + argb_order.R )^:=int8u_ptr(ptrcomp(src ) + abgr_order.R )^;
  int8u_ptr(ptrcomp(dst ) + argb_order.G )^:=int8u_ptr(ptrcomp(src ) + abgr_order.G )^;
  int8u_ptr(ptrcomp(dst ) + argb_order.B )^:=int8u_ptr(ptrcomp(src ) + abgr_order.B )^;
  int8u_ptr(ptrcomp(dst ) + argb_order.A )^:=int8u_ptr(ptrcomp(src ) + abgr_order.A )^;

  inc(ptrcomp(dst ) ,4 );
  inc(ptrcomp(src ) ,4 );
  dec(width );

 until width = 0;

end;

{ color_conv_rgba32_to_argb32 }
procedure color_conv_rgba32_to_argb32;
begin
 repeat
  int8u_ptr(ptrcomp(dst ) + argb_order.R )^:=int8u_ptr(ptrcomp(src ) + rgba_order.R )^;
  int8u_ptr(ptrcomp(dst ) + argb_order.G )^:=int8u_ptr(ptrcomp(src ) + rgba_order.G )^;
  int8u_ptr(ptrcomp(dst ) + argb_order.B )^:=int8u_ptr(ptrcomp(src ) + rgba_order.B )^;
  int8u_ptr(ptrcomp(dst ) + argb_order.A )^:=int8u_ptr(ptrcomp(src ) + rgba_order.A )^;

  inc(ptrcomp(dst ) ,4 );
  inc(ptrcomp(src ) ,4 );
  dec(width );

 until width = 0;
 
end;

{ color_conv_rgb555_to_bgra32 {..}
procedure color_conv_rgb555_to_bgra32;
begin
end;

{ color_conv_bgr24_to_bgra32 }
procedure color_conv_bgr24_to_bgra32;
begin
 repeat
  int8u_ptr(ptrcomp(dst ) + 0 )^:=src^; inc(ptrcomp(src ) );
  int8u_ptr(ptrcomp(dst ) + 1 )^:=src^; inc(ptrcomp(src ) );
  int8u_ptr(ptrcomp(dst ) + 2 )^:=src^; inc(ptrcomp(src ) );
  int8u_ptr(ptrcomp(dst ) + 3 )^:=255;

  inc(ptrcomp(dst ) ,4 );
  dec(width );

 until width = 0;

end;

{ color_conv_bgra32_to_bgra32 {..}
procedure color_conv_bgra32_to_bgra32;
begin
end;

{ color_conv_rgb555_to_rgba32 {..}
procedure color_conv_rgb555_to_rgba32;
begin
end;

{ color_conv_bgr24_to_rgba32 {..}
procedure color_conv_bgr24_to_rgba32;
begin
end;

{ color_conv_bgra32_to_rgba32 {..}
procedure color_conv_bgra32_to_rgba32;
begin
end;

{ color_conv_bgr24_to_abgr64 {..}
procedure color_conv_bgr24_to_abgr64;
begin
end;

{ color_conv_bgr24_to_argb64 {..}
procedure color_conv_bgr24_to_argb64;
begin
end;

{ color_conv_bgr24_to_bgra64 {..}
procedure color_conv_bgr24_to_bgra64;
begin
end;

{ color_conv_bgr24_to_rgba64 {..}
procedure color_conv_bgr24_to_rgba64;
begin
end;

{ color_conv_gray16_to_gray8 {..}
procedure color_conv_gray16_to_gray8;
begin
end;

{ color_conv_rgb565_to_rgb555 }
procedure color_conv_rgb565_to_rgb555;
var
 rgb : int;

begin
 repeat
  rgb:=int16u(p32(src ).ptr^ );

  int16u(p32(dst ).ptr^ ):=((rgb shr 1 ) and $7FE0 ) or (rgb and $1F );

  inc(ptrcomp(src ) ,2 );
  inc(ptrcomp(dst ) ,2 );
  dec(width );

 until width = 0;

end;

{ color_conv_rgbAAA_to_bgr24 {..}
procedure color_conv_rgbAAA_to_bgr24;
begin
end;

{ color_conv_bgrAAA_to_bgr24 {..}
procedure color_conv_bgrAAA_to_bgr24;
begin
end;

{ color_conv_rgbBBA_to_bgr24 {..}
procedure color_conv_rgbBBA_to_bgr24;
begin
end;

{ color_conv_bgrABB_to_bgr24 {..}
procedure color_conv_bgrABB_to_bgr24;
begin
end;

{ color_conv_rgb24_to_bgr24 }
procedure color_conv_rgb24_to_bgr24;
begin
 repeat
  int8u_ptr(p32(dst ).int + 0 )^:=int8u_ptr(p32(src ).int + 2 )^;
  int8u_ptr(p32(dst ).int + 1 )^:=int8u_ptr(p32(src ).int + 1 )^;
  int8u_ptr(p32(dst ).int + 2 )^:=int8u_ptr(p32(src ).int + 0 )^;

  inc(ptrcomp(src ) ,3 );
  inc(ptrcomp(dst ) ,3 );
  dec(width );

 until width = 0;

end;

{ color_conv_rgb48_to_bgr24 {..}
procedure color_conv_rgb48_to_bgr24;
begin
end;

{ color_conv_bgr48_to_bgr24 {..}
procedure color_conv_bgr48_to_bgr24;
begin
end;

{ color_conv_abgr32_to_bgra32 }
procedure color_conv_abgr32_to_bgra32;
begin
 repeat
  int8u_ptr(p32(dst ).int + 0 )^:=int8u_ptr(p32(src ).int + 1 )^;
  int8u_ptr(p32(dst ).int + 1 )^:=int8u_ptr(p32(src ).int + 2 )^;
  int8u_ptr(p32(dst ).int + 2 )^:=int8u_ptr(p32(src ).int + 3 )^;
  int8u_ptr(p32(dst ).int + 3 )^:=int8u_ptr(p32(src ).int + 0 )^;

  inc(ptrcomp(src ) ,4 );
  inc(ptrcomp(dst ) ,4 );
  dec(width );

 until width = 0;

end;

{ color_conv_argb32_to_bgra32 }
procedure color_conv_argb32_to_bgra32;
begin
 repeat
  int8u_ptr(p32(dst ).int + 0 )^:=int8u_ptr(p32(src ).int + 3 )^;
  int8u_ptr(p32(dst ).int + 1 )^:=int8u_ptr(p32(src ).int + 2 )^;
  int8u_ptr(p32(dst ).int + 2 )^:=int8u_ptr(p32(src ).int + 1 )^;
  int8u_ptr(p32(dst ).int + 3 )^:=int8u_ptr(p32(src ).int + 0 )^;

  inc(ptrcomp(src ) ,4 );
  inc(ptrcomp(dst ) ,4 );
  dec(width );

 until width = 0;

end;

{ color_conv_rgba32_to_bgra32 }
procedure color_conv_rgba32_to_bgra32;
begin
 repeat
  int8u_ptr(p32(dst ).int + 0 )^:=int8u_ptr(p32(src ).int + 2 )^;
  int8u_ptr(p32(dst ).int + 1 )^:=int8u_ptr(p32(src ).int + 1 )^;
  int8u_ptr(p32(dst ).int + 2 )^:=int8u_ptr(p32(src ).int + 0 )^;
  int8u_ptr(p32(dst ).int + 3 )^:=int8u_ptr(p32(src ).int + 3 )^;

  inc(ptrcomp(src ) ,4 );
  inc(ptrcomp(dst ) ,4 );
  dec(width );

 until width = 0;

end;

{ color_conv_bgra64_to_bgra32 {..}
procedure color_conv_bgra64_to_bgra32;
begin
end;

{ color_conv_abgr64_to_bgra32 {..}
procedure color_conv_abgr64_to_bgra32;
begin
end;

{ color_conv_argb64_to_bgra32 {..}
procedure color_conv_argb64_to_bgra32;
begin
end;

{ color_conv_rgba64_to_bgra32 {..}
procedure color_conv_rgba64_to_bgra32;
begin
end;

{ color_conv_rgb24_to_bgra32 }
procedure color_conv_rgb24_to_bgra32;
begin
 repeat
  int8u_ptr(ptrcomp(dst ) + bgra_order.R )^:=src^; inc(ptrcomp(src ) );
  int8u_ptr(ptrcomp(dst ) + bgra_order.G )^:=src^; inc(ptrcomp(src ) );
  int8u_ptr(ptrcomp(dst ) + bgra_order.B )^:=src^; inc(ptrcomp(src ) );
  int8u_ptr(ptrcomp(dst ) + bgra_order.A )^:=255;

  inc(ptrcomp(dst ) ,4 );
  dec(width );

 until width = 0;

end;

END.

