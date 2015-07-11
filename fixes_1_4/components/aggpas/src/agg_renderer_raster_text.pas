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
{ agg_renderer_raster_text.pas }
unit
 agg_renderer_raster_text ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_color ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_scanline ,
 agg_glyph_raster_bin ;

{ TYPES DEFINITION }
type
 renderer_raster_htext_solid = object
   m_ren   : renderer_base_ptr;
   m_glyph : glyph_raster_bin_ptr;
   m_color : aggclr;

   constructor Construct(ren : renderer_base_ptr; glyph : glyph_raster_bin_ptr );

   procedure color_(c : aggclr_ptr );
   function  _color : aggclr_ptr;

   procedure render_text(x ,y : double; str : PChar; flip : boolean = false );

  end;

 renderer_raster_vtext_solid = object
   m_ren   : renderer_base_ptr;
   m_glyph : glyph_raster_bin_ptr;
   m_color : aggclr;

   constructor Construct(ren : renderer_base_ptr; glyph : glyph_raster_bin_ptr );

   procedure color_(c : aggclr_ptr );
   function  _color : aggclr_ptr;

   procedure render_text(x ,y : double; str : PChar; flip : boolean = false );

  end;

 const_span_ptr = ^const_span;
 const_span = object
   x      ,
   len    : int16;
   covers : int8u_ptr;

   constructor Construct(x_ : int; len_ : unsigned; covers_ : int8u_ptr );

  end;

 scanline_single_span = object(scanline )
   m_y    : int;
   m_span : const_span;

   constructor Construct(x_ ,y_ : int; len_ : unsigned; covers_ : int8u_ptr );

   function  y : int; virtual;
   function  num_spans : unsigned; virtual;
   function  begin_ : pointer; virtual;

   function  sz_of_span : unsigned; virtual;

  end;

 renderer_raster_htext = object
   m_ren   : renderer_scanline_ptr;
   m_glyph : glyph_raster_bin_ptr;

   constructor Construct(ren : renderer_scanline_ptr; glyph : glyph_raster_bin_ptr );

   procedure render_text(x ,y : double; str : PChar; flip : boolean = false );

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor renderer_raster_htext_solid.Construct;
begin
 m_color.Construct;

 m_ren  :=ren;
 m_glyph:=glyph;

end;

{ COLOR_ }
procedure renderer_raster_htext_solid.color_;
begin
 m_color:=c^;

end;

{ _COLOR }
function renderer_raster_htext_solid._color;
begin
 result:=@m_color;

end;

{ RENDER_TEXT }
procedure renderer_raster_htext_solid.render_text;
var
 r : glyph_rect;
 i : int;

begin
 while int8u_ptr(str )^ <> 0 do
  begin
   m_glyph.prepare(@r ,x ,y ,int8u_ptr(str )^ ,flip );

   if r.x2 >= r.x1 then
    if flip then
     begin
      i:=r.y1;

      while i <= r.y2 do
       begin
        m_ren.blend_solid_hspan(
         r.x1 ,i ,r.x2 - r.x1 + 1 ,
         @m_color ,
         m_glyph.span(r.y2 - i ) );

        inc(i );

       end;

     end
    else
     begin
      i:=r.y1;

      while i <= r.y2 do
       begin
        m_ren.blend_solid_hspan(
         r.x1 ,i ,r.x2 - r.x1 + 1 ,
         @m_color ,
         m_glyph.span(i - r.y1 ) );

        inc(i );

       end;

     end;

   x:=x + r.dx;
   y:=y + r.dy;

   inc(ptrcomp(str ) );

  end;

end;

{ CONSTRUCT }
constructor renderer_raster_vtext_solid.Construct;
begin
 m_color.Construct;

 m_ren  :=ren;
 m_glyph:=glyph;

end;

{ COLOR_ }
procedure renderer_raster_vtext_solid.color_;
begin
 m_color:=c^;

end;

{ _COLOR }
function renderer_raster_vtext_solid._color;
begin
 result:=@m_color;

end;

{ RENDER_TEXT }
procedure renderer_raster_vtext_solid.render_text;
var
 r : glyph_rect;
 i : int;

begin
 while int8u_ptr(str )^ <> 0 do
  begin
   m_glyph.prepare(@r ,x ,y ,int8u_ptr(str )^ ,flip );

   if r.x2 >= r.x1 then
    if flip then
     begin
      i:=r.y1;

      while i <= r.y2 do
       begin
        m_ren.blend_solid_vspan(
         i ,r.x1 ,r.x2 - r.x1 + 1 ,
         @m_color ,
         m_glyph.span(i - r.y1 ) );

        inc(i );

       end;

     end
    else
     begin
      i:=r.y1;

      while i <= r.y2 do
       begin
        m_ren.blend_solid_vspan(
         i ,r.x1 ,r.x2 - r.x1 + 1 ,
         @m_color ,
         m_glyph.span(r.y2 - i ) );

        inc(i );

       end;

     end;

   x:=x + r.dx;
   y:=y + r.dy;

   inc(ptrcomp(str ) );

  end;

end;

{ CONSTRUCT }
constructor const_span.Construct;
begin
 x     :=x_;
 len   :=len_;
 covers:=covers_;

end;

{ CONSTRUCT }
constructor scanline_single_span.Construct;
begin
 m_y:=y_;

 m_span.Construct(x_ ,len_ ,covers_ );

end;

{ Y }
function scanline_single_span.y;
begin
 result:=m_y;

end;

{ NUM_SPANS }
function scanline_single_span.num_spans;
begin
 result:=1;

end;

{ BEGIN_ }
function scanline_single_span.begin_;
begin
 result:=@m_span;

end;

{ SZ_OF_SPAN }
function scanline_single_span.sz_of_span;
begin
 result:=sizeof(const_span );

end;

{ CONSTRUCT }
constructor renderer_raster_htext.Construct;
begin
 m_ren  :=ren;
 m_glyph:=glyph;

end;

{ RENDER_TEXT }
procedure renderer_raster_htext.render_text;
var
 r : glyph_rect;
 i : int;
 s : scanline_single_span;

begin
 while int8u_ptr(str )^ <> 0 do
  begin
   m_glyph.prepare(@r ,x ,y ,int8u_ptr(str )^ ,flip );

   if r.x2 >= r.x1 then
    begin
     m_ren.prepare(r.x2 - r.x1 + 1 );

     if flip then
      begin
       i:=r.y1;

       while i <= r.y2 do
        begin
         s.Construct(
          r.x1 ,i ,r.x2 - r.x1 + 1 ,
          m_glyph.span(r.y2 - i ) );

         m_ren.render(@s ); 

         inc(i );

        end;

      end
     else
      begin
       i:=r.y1;

       while i <= r.y2 do
        begin
         s.Construct(
          r.x1 ,i ,(r.x2 - r.x1 + 1 ) ,
          m_glyph.span(i - r.y1 ) );

         m_ren.render(@s ); 

         inc(i );

        end;

      end;

    end;

   x:=x + r.dx;
   y:=y + r.dy;

   inc(ptrcomp(str ) );

  end;

end;

END.

