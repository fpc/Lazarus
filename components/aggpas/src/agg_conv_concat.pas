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
// Concatenation of two paths. Usually used to combine lines or curves
// with markers such as arrowheads
//
// [Pascal Port History] -----------------------------------------------------
//
// 26.02.2006-Milano: Unit port establishment
//
{ agg_conv_concat.pas }
unit
 agg_conv_concat ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 conv_concat = object(vertex_source )
   m_source1 ,
   m_source2 : vertex_source_ptr;
   m_status  : int;

   constructor Construct(source1 ,source2 : vertex_source_ptr );

   procedure set_source1(source : vertex_source_ptr );
   procedure set_source2(source : vertex_source_ptr );

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor conv_concat.Construct;
begin
 m_source1:=source1;
 m_source2:=source2;
 m_status :=2;

end;

{ SET_SOURCE1 }
procedure conv_concat.set_source1;
begin
 m_source1:=source;

end;

{ SET_SOURCE2 }
procedure conv_concat.set_source2;
begin
 m_source2:=source;

end;

{ REWIND }
procedure conv_concat.rewind;
begin
 m_source1.rewind(path_id );
 m_source2.rewind(0 );

 m_status:=0;

end;

{ VERTEX }
function conv_concat.vertex;
var
 cmd : unsigned;

begin
 if m_status = 0 then
  begin
   cmd:=m_source1.vertex(x ,y );

   if not is_stop(cmd ) then
    begin
     result:=cmd;

     exit;

    end;

   m_status:=1;

  end;

 if m_status = 1 then
  begin
   cmd:=m_source2.vertex(x ,y );

   if not is_stop(cmd ) then
    begin
     result:=cmd;

     exit;

    end;

   m_status:=2;

  end;

 result:=path_cmd_stop;

end;

END.

