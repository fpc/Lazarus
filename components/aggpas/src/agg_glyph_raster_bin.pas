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
// 23.06.2006-Milano: ptrcomp adjustments
// 13.02.2006-Milano: Unit port establishment
//
{ agg_glyph_raster_bin.pas }
unit
 agg_glyph_raster_bin ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ;

{ TYPES DEFINITION }
type
 glyph_rect_ptr = ^glyph_rect;
 glyph_rect = record
   x1 ,y1 ,x2 ,y2 : int;

   dx ,dy : double;

  end;

 glyph_raster_bin_ptr = ^glyph_raster_bin;
 glyph_raster_bin = object
   m_font : int8u_ptr;

   m_big_endian : boolean;

   m_span : array[0..31 ] of int8u;
   m_bits : int8u_ptr;

   m_glyph_width      ,
   m_glyph_byte_width : unsigned;

   constructor Construct(font : int8u_ptr );

   function  _font : int8u_ptr;
   procedure font_(f : int8u_ptr );

   function  height : double;
   function  base_line : double;

   function  width  (str : PChar ) : double;
   procedure prepare(r : glyph_rect_ptr; x ,y : double; glyph : unsigned; flip : boolean );
   function  span   (i : unsigned ) : int8u_ptr;

   function  value(p : int8u_ptr ) : int16u;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor glyph_raster_bin.Construct;
var
 t : int;

begin
 m_font:=font;

 t:=1;

 if byte(pointer(@t )^ ) = 0 then
  m_big_endian:=true
 else
  m_big_endian:=false;

 fillchar(m_span ,sizeof(m_span ) ,0 );

end;

{ _FONT }
function glyph_raster_bin._font;
begin
 result:=m_font;

end;

{ FONT_ }
procedure glyph_raster_bin.font_;
begin
 m_font:=f;

end;

{ HEIGHT }
function glyph_raster_bin.height;
begin
 result:=int8u_ptr(ptrcomp(m_font ) + 0 * sizeof(int8u ) )^;

end;

{ BASE_LINE }
function glyph_raster_bin.base_line;
begin
 result:=int8u_ptr(ptrcomp(m_font ) + 1 * sizeof(int8u ) )^;

end;

{ WIDTH }
function glyph_raster_bin.width;
var
 start_char ,num_chars ,w ,glyph : unsigned;

 bits : int8u_ptr;

begin
 start_char:=int8u_ptr(ptrcomp(m_font ) + 2 * sizeof(int8u ) )^;
 num_chars :=int8u_ptr(ptrcomp(m_font ) + 3 * sizeof(int8u ) )^;

 w:=0;

 while str <> #0 do
  begin
   glyph:=int8u_ptr(str )^;

   bits:=
    int8u_ptr(
     ptrcomp(m_font ) + 4 + num_chars * 2 +
     value(int8u_ptr(ptrcomp(m_font ) + 4 + (glyph - start_char ) * 2 ) ) );

   inc(w ,bits^ );
   inc(ptrcomp(str ) );

  end;

 result:=w;

end;

{ PREPARE }
procedure glyph_raster_bin.prepare;
var
 start_char ,num_chars : unsigned;

begin
 start_char:=int8u_ptr(ptrcomp(m_font ) + 2 * sizeof(int8u ) )^;
 num_chars :=int8u_ptr(ptrcomp(m_font ) + 3 * sizeof(int8u ) )^;

 m_bits:=
  int8u_ptr(
   ptrcomp(m_font ) + 4 + num_chars * 2 +
   value(int8u_ptr(ptrcomp(m_font ) + 4 + (glyph - start_char ) * 2 ) ) );

 m_glyph_width:=m_bits^;

 inc(ptrcomp(m_bits ) );

 m_glyph_byte_width:=(m_glyph_width + 7 ) shr 3;

 r.x1:=trunc(x );
 r.x2:=r.x1 + m_glyph_width - 1;

 if flip then
  begin
   r.y1:=
    trunc(y ) -
    int8u_ptr(ptrcomp(m_font ) + 0 * sizeof(int8u ) )^ +
    int8u_ptr(ptrcomp(m_font ) + 1 * sizeof(int8u ) )^;

   r.y2:=r.y1 + int8u_ptr(ptrcomp(m_font ) + 0 * sizeof(int8u ) )^ - 1;

  end
 else
  begin
   r.y1:=trunc(y ) - int8u_ptr(ptrcomp(m_font ) + 1 * sizeof(int8u ) )^ + 1;
   r.y2:=r.y1 + int8u_ptr(ptrcomp(m_font ) + 0 * sizeof(int8u ) )^ - 1;

  end;

 r.dx:=m_glyph_width;
 r.dy:=0;

end;

{ SPAN }
function glyph_raster_bin.span;
var
 bits : int8u_ptr;

 j ,val ,nb : unsigned;

begin
 i:=int8u_ptr(ptrcomp(m_font ) + 0 * sizeof(int8u ) )^ - i - 1;

 bits:=int8u_ptr(ptrcomp(m_bits ) + i * m_glyph_byte_width );
 val :=bits^;
 nb  :=0;

 for j:=0 to m_glyph_width - 1 do
  begin
   if val and $80 <> 0 then
    m_span[j ]:=int8u(cover_full )
   else
    m_span[j ]:=int8u(cover_none );

   val:=val shl 1;

   inc(nb );

   if nb >= 8 then
    begin
     inc(ptrcomp(bits ) );

     val:=bits^;
     nb :=0;

    end;

  end;

 result:=@m_span[0 ];

end;

{ VALUE }
function glyph_raster_bin.value;
var
 v : int16u;

begin
 if m_big_endian then
  begin
   int16u_(v ).Low :=int8u_ptr(ptrcomp(p ) + 1 )^;
   int16u_(v ).High:=p^;

  end
 else
  begin
   int16u_(v ).Low :=p^;
   int16u_(v ).High:=int8u_ptr(ptrcomp(p ) + 1 )^;

  end;

 result:=v;

end;

END.

