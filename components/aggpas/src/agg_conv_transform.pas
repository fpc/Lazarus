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
// class conv_transform
//
// [Pascal Port History] -----------------------------------------------------
//
// 21.12.2005-Milano: Unit port establishment
//
{ agg_conv_transform.pas }
unit
 agg_conv_transform ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_trans_affine ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 conv_transform_ptr = ^conv_transform;
 conv_transform = object(vertex_source )
   m_source : vertex_source_ptr;
   m_trans  : trans_affine_ptr;

   constructor Construct(source : vertex_source_ptr; tr : trans_affine_ptr );

   procedure set_source(source : vertex_source_ptr );

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

   procedure transformer_(tr : trans_affine_ptr );

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor conv_transform.Construct;
begin
 inherited Construct;

 m_source:=source;
 m_trans :=tr;

end;

{ SET_SOURCE }
procedure conv_transform.set_source;
begin
 m_source:=source;

end;

{ REWIND }
procedure conv_transform.rewind;
begin
 m_source.rewind(path_id );

end;

{ VERTEX }
function conv_transform.vertex;
var
 cmd : unsigned;

begin
 cmd:=m_source.vertex(x ,y );

 if is_vertex(cmd ) then
  m_trans.transform(m_trans ,x ,y );

 result:=cmd;

end;

{ TRANSFORMER_ }
procedure conv_transform.transformer_;
begin
 m_trans:=tr;

end;

END.

