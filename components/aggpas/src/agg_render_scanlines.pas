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
// 05.02.2006-Milano: render_all_paths
// 21.11.2005-Milano: Unit port establishment, render_scanlines
//
{ agg_render_scanlines.pas }
unit
 agg_render_scanlines ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
 agg_basics ,
 agg_color ,
 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_renderer_scanline ,
 agg_vertex_source ;

{ GLOBAL PROCEDURES }
 procedure render_scanlines(ras : rasterizer_scanline_ptr; sl : scanline_ptr; ren : renderer_scanline_ptr );
 procedure render_all_paths(
            ras : rasterizer_scanline_ptr;
            sl  : scanline_ptr;
            r   : renderer_scanline_ptr;
            vs  : vertex_source_ptr;
            cs  : aggclr_ptr;
            path_id   : unsigned_ptr;
            num_paths : unsigned );


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ RENDER_SCANLINES }
procedure render_scanlines;
begin
 if ras.rewind_scanlines then
  begin
   sl.reset   (ras._min_x ,ras._max_x );
   ren.prepare(unsigned(ras._max_x - ras._min_x + 2 ) );

   if sl.is_embedded then
    while ras.sweep_scanline_em(sl ) do
     ren.render(sl )
   else
    while ras.sweep_scanline(sl ) do
     ren.render(sl );

  end;

end;

{ RENDER_ALL_PATHS }
procedure render_all_paths;
var
 i : unsigned;

begin
 i:=0;

 while i < num_paths do
  begin
   ras.reset;
   ras.add_path(vs ,path_id^ );
   r.color_    (cs );

   render_scanlines(ras ,sl ,r );

   inc(ptrcomp(cs ) ,sizeof(aggclr ) );
   inc(ptrcomp(path_id ) ,sizeof(unsigned ) );
   inc(i );

  end;

end;

END.

