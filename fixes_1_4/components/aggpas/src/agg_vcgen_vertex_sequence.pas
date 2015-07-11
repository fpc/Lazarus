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
// 26.02.2006-Milano: Unit port establishment
//
{ agg_vcgen_vertex_sequence.pas }
unit
 agg_vcgen_vertex_sequence ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_vertex_sequence ,
 agg_shorten_path ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 vcgen_vertex_sequence_ptr = ^vcgen_vertex_sequence;
 vcgen_vertex_sequence = object(vertex_source )
   m_src_vertices : vertex_sequence;
   m_flags        ,
   m_cur_vertex   : unsigned;

   m_shorten : double;
   m_ready   : boolean;

   constructor Construct;
   destructor  Destruct; virtual;

   procedure shorten_(s : double );
   function  _shorten : double;

  // Vertex Generator Interface
   procedure remove_all; virtual;
   procedure add_vertex(x ,y : double; cmd : unsigned ); virtual;

  // Vertex Source Interface
   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor vcgen_vertex_sequence.Construct;
begin
 m_src_vertices.Construct(sizeof(vertex_dist_cmd ) ,6 );

 m_flags     :=0;
 m_cur_vertex:=0;
 m_shorten   :=0.0;
 m_ready     :=false;

end;

{ DESTRUCT }
destructor vcgen_vertex_sequence.Destruct;
begin
 m_src_vertices.Destruct;

end;

{ SHORTEN_ }
procedure vcgen_vertex_sequence.shorten_;
begin
 m_shorten:=s;

end;

{ _SHORTEN }
function vcgen_vertex_sequence._shorten;
begin
 result:=m_shorten;

end;

{ REMOVE_ALL }
procedure vcgen_vertex_sequence.remove_all;
begin
 m_ready:=false;

 m_src_vertices.remove_all;

 m_cur_vertex:=0;
 m_flags     :=0;

end;

{ ADD_VERTEX }
procedure vcgen_vertex_sequence.add_vertex;
var
 vc : vertex_dist_cmd;

begin
 m_ready:=false;

 vc.x:=x;
 vc.y:=y;

 vc.dist:=0;
 vc.cmd :=cmd;

 if is_move_to(cmd ) then
  m_src_vertices.modify_last(@vc )
 else
  if is_vertex(cmd ) then
   m_src_vertices.add(@vc )
  else
   m_flags:=cmd and path_flags_mask;

end;

{ REWIND }
procedure vcgen_vertex_sequence.rewind;
begin
 if not m_ready then
  begin
   m_src_vertices.close(is_closed(m_flags ) );

   shorten_path(@m_src_vertices ,m_shorten ,get_close_flag(m_flags ) );

  end;

 m_ready     :=true;
 m_cur_vertex:=0;

end;

{ VERTEX }
function vcgen_vertex_sequence.vertex;
var
 v : vertex_dist_cmd_ptr;

begin
 if not m_ready then
  rewind(0 );

 if m_cur_vertex = m_src_vertices.size then
  begin
   inc(m_cur_vertex );

   result:=path_cmd_end_poly or m_flags;

   exit;

  end;

 if m_cur_vertex > m_src_vertices.size then
  begin
   result:=path_cmd_stop;

   exit;

  end;

 v:=m_src_vertices.array_operator(m_cur_vertex );

 inc(m_cur_vertex );

 x^:=v.x;
 y^:=v.y;

 result:=v.cmd;

end;

END.

