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
// polyline clipping converter
// There an optimized Liang-Basky algorithm is used.
// The algorithm doesn't optimize the degenerate edges, i.e. it will never
// break a closed polyline into two or more ones, instead, there will be
// degenerate edges coinciding with the respective clipping boundaries.
// This is a sub-optimal solution, because that optimization would require
// extra, rather expensive math while the rasterizer tolerates it quite well,
// without any considerable overhead.
//
// [Pascal Port History] -----------------------------------------------------
//
// 01.03.2006-Milano: Unit port establishment
//
{ agg_conv_clip_polyline.pas }
unit
 agg_conv_clip_polyline ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_conv_adaptor_vpgen ,
 agg_vpgen_clip_polyline ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 conv_clip_polyline = object(conv_adaptor_vpgen )
   the_generator : vpgen_clip_polyline;

   constructor Construct(vs : vertex_source_ptr );
   destructor  Destruct; virtual;

   procedure clip_box_(x1 ,y1 ,x2 ,y2 : double );

   function  _x1 : double;
   function  _y1 : double;
   function  _x2 : double;
   function  _y2 : double;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor conv_clip_polyline.Construct;
begin
 the_generator.Construct;

 inherited Construct(vs ,@the_generator );

end;

{ DESTRUCT }
destructor conv_clip_polyline.Destruct;
begin
 inherited Destruct;

 the_generator.Destruct;

end;

{ CLIP_BOX_ }
procedure conv_clip_polyline.clip_box_;
begin
 vpgen_clip_polyline_ptr(vpgen ).clip_box_(x1 ,y1 ,x2 ,y2 );

end;

{ _X1 }
function conv_clip_polyline._x1;
begin
 result:=vpgen_clip_polyline_ptr(vpgen )._x1;

end;

{ _Y1 }
function conv_clip_polyline._y1;
begin
 result:=vpgen_clip_polyline_ptr(vpgen )._y1;

end;

{ _X2 }
function conv_clip_polyline._x2;
begin
 result:=vpgen_clip_polyline_ptr(vpgen )._x2;

end;

{ _Y2 }
function conv_clip_polyline._y2;
begin
 result:=vpgen_clip_polyline_ptr(vpgen )._y2;

end;

END.

