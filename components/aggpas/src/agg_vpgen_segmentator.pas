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
// 25.01.2006-Milano: Unit port establishment
//
{ agg_vpgen_segmentator.pas }
unit
 agg_vpgen_segmentator ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 vpgen_segmentator_ptr = ^vpgen_segmentator;
 vpgen_segmentator = object(vertex_source )
   m_approximation_scale ,

   m_x1  ,
   m_y1  ,
   m_dx  ,
   m_dy  ,
   m_dl  ,
   m_ddl : double;
   m_cmd : unsigned;

   constructor Construct;

   procedure approximation_scale_(s : double );
   function  _approximation_scale : double;

   function  _auto_close : boolean;
   function  _auto_unclose : boolean;

   procedure reset;
   procedure move_to(x ,y : double );
   procedure line_to(x ,y : double );

   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor vpgen_segmentator.Construct;
begin
 m_approximation_scale:=1.0;

 m_x1 :=0;
 m_y1 :=0;
 m_dx :=0;
 m_dy :=0;
 m_dl :=0;
 m_ddl:=0;
 m_cmd:=0;

end;

{ APPROXIMATION_SCALE_ }
procedure vpgen_segmentator.approximation_scale_;
begin
 m_approximation_scale:=s;

end;

{ _APPROXIMATION_SCALE }
function vpgen_segmentator._approximation_scale;
begin
 result:=m_approximation_scale;

end;

{ _AUTO_CLOSE }
function vpgen_segmentator._auto_close;
begin
 result:=false

end;

{ _AUTO_UNCLOSE }
function vpgen_segmentator._auto_unclose;
begin
 result:=false

end;

{ RESET }
procedure vpgen_segmentator.reset;
begin
 m_cmd:=path_cmd_stop;

end;

{ MOVE_TO }
procedure vpgen_segmentator.move_to;
begin
 m_x1 :=x;
 m_y1 :=y;
 m_dx :=0.0;
 m_dy :=0.0;
 m_dl :=2.0;
 m_ddl:=2.0;
 m_cmd:=path_cmd_move_to;

end;

{ LINE_TO }
procedure vpgen_segmentator.line_to;
var
 len : double;

begin
 m_x1:=m_x1 + m_dx;
 m_y1:=m_y1 + m_dy;
 m_dx:=x - m_x1;
 m_dy:=y - m_y1;

 len:=Sqrt(m_dx * m_dx + m_dy * m_dy ) * m_approximation_scale;

 if len < 1e-30 then
  len:=1e-30;

 m_ddl:=1.0 / len;

 if m_cmd = path_cmd_move_to then
  m_dl:=0.0
 else
  m_dl:=m_ddl;

 if m_cmd = path_cmd_stop then
  m_cmd:=path_cmd_line_to;

end;

{ VERTEX }
function vpgen_segmentator.vertex;
var
 cmd : unsigned;

begin
 if m_cmd = path_cmd_stop then
  result:=path_cmd_stop

 else
  begin
   cmd  :=m_cmd;
   m_cmd:=path_cmd_line_to;

   if m_dl >= 1.0 - m_ddl then
    begin
     m_dl :=1.0;
     m_cmd:=path_cmd_stop;

     x^:=m_x1 + m_dx;
     y^:=m_y1 + m_dy;

     result:=cmd;

    end
   else
    begin
     x^:=m_x1 + m_dx * m_dl;
     y^:=m_y1 + m_dy * m_dl;

     m_dl:=m_dl + m_ddl;

     result:=cmd;

    end;

  end;

end;

END.

