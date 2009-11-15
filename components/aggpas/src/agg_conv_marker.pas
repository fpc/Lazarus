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
// 25.02.2006-Milano: Unit port establishment
//
{ agg_conv_marker.pas }
unit
 agg_conv_marker ;

INTERFACE

{$I agg_mode.inc }

uses
 Math ,
 agg_basics ,
 agg_trans_affine ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 status_e = (initial ,markers ,polygon ,stop );

 conv_marker = object(vertex_source )
   m_marker_locator ,
   m_marker_shapes  : vertex_source_ptr;

   m_transform ,
   m_mtx       : trans_affine;

   m_status      : status_e;
   m_marker      ,
   m_num_markers : unsigned;

   constructor Construct(ml ,ms : vertex_source_ptr );

   function  _transform : trans_affine_ptr;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor conv_marker.Construct;
begin
 m_transform.Construct;
 m_mtx.Construct;

 m_marker_locator:=ml;
 m_marker_shapes :=ms;

 m_status:=initial;
 m_marker:=0;

 m_num_markers:=1;

end;

{ _TRANSFORM }
function conv_marker._transform;
begin
 result:=@m_transform;

end;

{ REWIND }
procedure conv_marker.rewind;
begin
 m_status:=initial;
 m_marker:=0;

 m_num_markers:=1;

end;

{ VERTEX }
function conv_marker.vertex;
var
 cmd : unsigned;

 x1 ,y1 ,x2 ,y2 : double;

 tar : trans_affine_rotation;
 tat : trans_affine_translation;

label
 _next ,_markers ,_polygon ,_stop ;

begin
 cmd:=path_cmd_move_to;

_next:
 while not is_stop(cmd ) do
  case m_status of
   initial :
    begin
     if m_num_markers = 0 then
      begin
       cmd:=path_cmd_stop;

       goto _next;

      end;

     m_marker_locator.rewind(m_marker );

     inc(m_marker );

     m_num_markers:=0;
     m_status     :=markers;

     goto _markers;

    end;

   markers :
   _markers:
    begin
     if is_stop(m_marker_locator.vertex(@x1 ,@y1 ) ) then
      begin
       m_status:=initial;

       goto _next;

      end;

     if is_stop(m_marker_locator.vertex(@x2 ,@y2 ) ) then
      begin
       m_status:=initial;

       goto _next;

      end;

     inc(m_num_markers );

     m_mtx:=m_transform;

     tar.Construct(ArcTan2(y2 - y1 ,x2 - x1 ) );
     tat.Construct(x1 ,y1 );

     m_mtx.multiply(@tar );
     m_mtx.multiply(@tat );

     m_marker_shapes.rewind(m_marker - 1 );

     m_status:=polygon;

     goto _polygon;

    end;

   polygon :
   _polygon:
    begin
     cmd:=m_marker_shapes.vertex(x ,y );

     if is_stop(cmd ) then
      begin
       cmd     :=path_cmd_move_to;
       m_status:=markers;

       goto _next;

      end;

     m_mtx.transform(@m_mtx ,x ,y );

     result:=cmd;

     exit;

    end;

   stop :
    begin
     cmd:=path_cmd_stop;

     goto _next;

    end;

  end;

 result:=cmd;

end;

END.

