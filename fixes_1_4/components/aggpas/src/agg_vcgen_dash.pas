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
// Line dash generator
//
// [Pascal Port History] -----------------------------------------------------
//
// 26.01.2006-Milano: Unit port establishment
//
{ agg_vcgen_dash.pas }
unit
 agg_vcgen_dash ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_vertex_source ,
 agg_vertex_sequence ,
 agg_shorten_path ;

{ TYPES DEFINITION }
const
 max_dashes = 32;

type
 status_e = (initial ,ready ,polyline ,stop );

 vcgen_dash_ptr = ^vcgen_dash;
 vcgen_dash = object(vertex_source )
   m_dashes : array[0..max_dashes - 1 ] of double;

   m_total_dash_len  : double;
   m_num_dashes      : unsigned;
   m_dash_start      ,
   m_shorten         ,
   m_curr_dash_start : double;

   m_curr_dash : unsigned;
   m_curr_rest : double;

   m_v1 ,
   m_v2 : vertex_dist_ptr;

   m_src_vertices : vertex_sequence;

   m_closed : unsigned;
   m_status : status_e;

   m_src_vertex : unsigned;

   constructor Construct;
   destructor  Destruct; virtual;

   procedure remove_all_dashes;
   procedure add_dash  (dash_len ,gap_len : double );
   procedure dash_start(ds : double );

   procedure shorten_(s : double );
   function  _shorten : double;

  // Vertex Generator Interface
   procedure remove_all; virtual;
   procedure add_vertex(x ,y : double; cmd : unsigned ); virtual;

  // Vertex Source Interface
   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  // Private
   procedure calc_dash_start(ds : double );

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor vcgen_dash.Construct;
begin
 m_src_vertices.Construct(sizeof(vertex_dist ) );

 m_total_dash_len :=0.0;
 m_num_dashes     :=0;
 m_dash_start     :=0.0;
 m_shorten        :=0.0;
 m_curr_dash_start:=0.0;
 m_curr_dash      :=0;

 m_closed    :=0;
 m_status    :=initial;
 m_src_vertex:=0;

end;

{ DESTRUCT }
destructor vcgen_dash.Destruct;
begin
 m_src_vertices.Destruct;

end;

{ REMOVE_ALL_DASHES }
procedure vcgen_dash.remove_all_dashes;
begin
 m_total_dash_len :=0.0;
 m_num_dashes     :=0;
 m_curr_dash_start:=0.0;
 m_curr_dash      :=0;

end;

{ ADD_DASH }
procedure vcgen_dash.add_dash;
begin
 if m_num_dashes < max_dashes then
  begin
   m_total_dash_len:=m_total_dash_len + dash_len + gap_len;

   m_dashes[m_num_dashes ]:=dash_len;

   inc(m_num_dashes );

   m_dashes[m_num_dashes ]:=gap_len;

   inc(m_num_dashes );

  end;

end;

{ DASH_START }
procedure vcgen_dash.dash_start;
begin
 m_dash_start:=ds;

 calc_dash_start(Abs(ds ) );

end;

{ SHORTEN_ }
procedure vcgen_dash.shorten_;
begin
 m_shorten:=s;

end;

{ _SHORTEN }
function vcgen_dash._shorten;
begin
 result:=m_shorten;

end;

{ REMOVE_ALL }
procedure vcgen_dash.remove_all;
begin
 m_status:=initial;

 m_src_vertices.remove_all;

 m_closed:=0;

end;

{ ADD_VERTEX }
procedure vcgen_dash.add_vertex;
var
 vd : vertex_dist;

begin
 m_status:=initial;

 vd.x   :=x;
 vd.y   :=y;
 vd.dist:=0;

 if is_move_to(cmd ) then
  m_src_vertices.modify_last(@vd )
 else
  if is_vertex(cmd ) then
   m_src_vertices.add(@vd )
  else
   m_closed:=get_close_flag(cmd );

end;

{ REWIND }
procedure vcgen_dash.rewind;
begin
 if m_status = initial then
  begin
   m_src_vertices.close(boolean(m_closed <> 0 ) );

   shorten_path(@m_src_vertices ,m_shorten ,m_closed );

  end;

 m_status    :=ready;
 m_src_vertex:=0;

end;

{ VERTEX }
function vcgen_dash.vertex;
var
 cmd : unsigned;

 dash_rest : double;

label
 _next ,_ready ;

begin
 cmd:=path_cmd_move_to;

_next:
 while not is_stop(cmd ) do
  case m_status of
   initial:
    begin
     rewind(0 );

     goto _ready;

    end;

   ready :
   _ready:
    begin
     if (m_num_dashes < 2 ) or
        (m_src_vertices.size < 2 ) then
      begin
       cmd:=path_cmd_stop;

       goto _next;

      end;

     m_status    :=polyline;
     m_src_vertex:=1;

     m_v1:=m_src_vertices.array_operator(0 );
     m_v2:=m_src_vertices.array_operator(1 );

     m_curr_rest:=m_v1.dist;

     x^:=m_v1.x;
     y^:=m_v1.y;

     if m_dash_start >= 0.0 then
      calc_dash_start(m_dash_start );

     result:=path_cmd_move_to;

     exit;

    end;

   polyline :
    begin
     dash_rest:=m_dashes[m_curr_dash ] - m_curr_dash_start;

     if m_curr_dash and 1 <> 0 then
      cmd:=path_cmd_move_to
     else
      cmd:=path_cmd_line_to;

     if m_curr_rest > dash_rest then
      begin
       m_curr_rest:=m_curr_rest - dash_rest;

       inc(m_curr_dash );

       if m_curr_dash >= m_num_dashes then
        m_curr_dash:=0;

       m_curr_dash_start:=0.0;

       x^:=m_v2.x - (m_v2.x - m_v1.x ) * m_curr_rest / m_v1.dist;
       y^:=m_v2.y - (m_v2.y - m_v1.y ) * m_curr_rest / m_v1.dist;

      end
     else
      begin
       m_curr_dash_start:=m_curr_dash_start + m_curr_rest;

       x^:=m_v2.x;
       y^:=m_v2.y;

       inc(m_src_vertex );

       m_v1:=m_v2;

       m_curr_rest:=m_v1.dist;

       if m_closed <> 0 then
        if m_src_vertex > m_src_vertices.size then
         m_status:=stop
        else
         if m_src_vertex >= m_src_vertices.size then
          m_v2:=m_src_vertices.array_operator(0 )
         else
          m_v2:=m_src_vertices.array_operator(m_src_vertex )
       else
        if m_src_vertex >= m_src_vertices.size then
         m_status:=stop
        else
         m_v2:=m_src_vertices.array_operator(m_src_vertex );

      end;

     result:=cmd;

     exit;

    end;

   stop :
    cmd:=path_cmd_stop;

  end;

 result:=path_cmd_stop; 

end;

{ CALC_DASH_START }
procedure vcgen_dash.calc_dash_start;
begin
 m_curr_dash      :=0;
 m_curr_dash_start:=0.0;

 while ds > 0.0 do
  if ds > m_dashes[m_curr_dash ] then
   begin
    ds:=ds - m_dashes[m_curr_dash ];

    inc(m_curr_dash );

    m_curr_dash_start:=0.0;

    if m_curr_dash >= m_num_dashes then
     m_curr_dash:=0;

   end
  else
   begin
    m_curr_dash_start:=ds;

    ds:=0.0;

   end;

end;

END.

