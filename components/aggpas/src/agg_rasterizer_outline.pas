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
// 15.01.2006-Milano: Unit port establishment
//
{ agg_rasterizer_outline.pas }
unit
 agg_rasterizer_outline ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_renderer_primitives ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 rasterizer_outline_ptr = ^rasterizer_outline;
 rasterizer_outline = object
   m_ren : renderer_primitives_ptr;

   m_start_x ,
   m_start_y : int;

   m_vertices : unsigned;

   constructor Construct(ren : renderer_primitives_ptr );

   procedure move_to(x ,y : int );
   procedure line_to(x ,y : int );

   procedure move_to_d(x ,y : double );
   procedure line_to_d(x ,y : double );
   procedure close;

   procedure add_vertex(x ,y : double; cmd : unsigned );
   procedure add_path  (vs : vertex_source_ptr; path_id : unsigned = 0 );

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor rasterizer_outline.Construct;
begin
 m_ren:=ren;

 m_start_x:=0;
 m_start_y:=0;

 m_vertices:=0;

end;

{ MOVE_TO }
procedure rasterizer_outline.move_to;
begin
 m_vertices:=1;

 m_start_x:=x;
 m_start_y:=y;

 m_ren.move_to(x ,y );

end;

{ LINE_TO }
procedure rasterizer_outline.line_to;
begin
 inc(m_vertices );

 m_ren.line_to(x ,y );

end;

{ MOVE_TO_D }
procedure rasterizer_outline.move_to_d;
begin
 move_to(m_ren.coord(x ) ,m_ren.coord(y ) );

end;

{ LINE_TO_D }
procedure rasterizer_outline.line_to_d;
begin
 line_to(m_ren.coord(x ) ,m_ren.coord(y ) );

end;

{ CLOSE }
procedure rasterizer_outline.close;
begin
 if m_vertices > 2 then
  line_to(m_start_x ,m_start_y );

 m_vertices:=0;

end;

{ ADD_VERTEX }
procedure rasterizer_outline.add_vertex;
begin
 if is_move_to(cmd ) then
  move_to_d(x ,y )
 else
  if is_end_poly(cmd ) then
   if is_closed(cmd ) then
    close
   else
  else
   line_to_d(x ,y );

end;

{ ADD_PATH }
procedure rasterizer_outline.add_path;
var
 cmd  : unsigned;
 x ,y : double;

begin
 vs.rewind(path_id );

 cmd:=vs.vertex(@x ,@y );

 while not is_stop(cmd ) do
  begin
   add_vertex(x ,y ,cmd );

   cmd:=vs.vertex(@x ,@y );

  end;

end;

END.

