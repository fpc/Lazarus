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
// 07.02.2006-Milano: Unit port establishment
//
{ agg_vpgen_clip_polygon.pas }
unit
 agg_vpgen_clip_polygon ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_vertex_source ,
 agg_clip_liang_barsky ;

{ TYPES DEFINITION }
type
 vpgen_clip_polygon_ptr = ^vpgen_clip_polygon;
 vpgen_clip_polygon = object(vertex_source )
   m_clip_box : rect_d;

   m_x1 ,
   m_y1 : double;

   m_clip_flags : unsigned;

   m_x ,
   m_y : array[0..3 ] of double;

   m_num_vertices ,
   m_vertex       ,
   m_cmd          : unsigned;

   constructor Construct;

   procedure clip_box_(x1 ,y1 ,x2 ,y2 : double );

   function  _x1 : double;
   function  _y1 : double;
   function  _x2 : double;
   function  _y2 : double;

   function  _auto_close : boolean;
   function  _auto_unclose : boolean;

   procedure reset;
   procedure move_to(x ,y : double );
   procedure line_to(x ,y : double );

   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

   function  clipping_flags(x ,y : double ) : unsigned;

  end;

{ GLOBAL VARIABLES & CONSTANTS }
{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor vpgen_clip_polygon.Construct;
begin
 m_clip_box.Construct(0 ,0 ,1 ,1 );

 m_x1:=0;
 m_y1:=0;

 m_clip_flags  :=0;
 m_num_vertices:=0;

 m_vertex:=0;
 m_cmd   :=path_cmd_move_to;

end;

{ CLIP_BOX_ }
procedure vpgen_clip_polygon.clip_box_;
begin
 m_clip_box.x1:=x1;
 m_clip_box.y1:=y1;
 m_clip_box.x2:=x2;
 m_clip_box.y2:=y2;

 m_clip_box.normalize;

end;

{ _X1 }
function vpgen_clip_polygon._x1;
begin
 result:=m_clip_box.x1;

end;

{ _Y1 }
function vpgen_clip_polygon._y1;
begin
 result:=m_clip_box.y1;

end;

{ _X2 }
function vpgen_clip_polygon._x2;
begin
 result:=m_clip_box.x2;

end;

{ _Y2 }
function vpgen_clip_polygon._y2;
begin
 result:=m_clip_box.y2;

end;

{ _AUTO_CLOSE }
function vpgen_clip_polygon._auto_close;
begin
 result:=true;

end;

{ _AUTO_UNCLOSE }
function vpgen_clip_polygon._auto_unclose;
begin
 result:=false;

end;

{ RESET }
procedure vpgen_clip_polygon.reset;
begin
 m_vertex      :=0;
 m_num_vertices:=0;

end;

{ MOVE_TO }
procedure vpgen_clip_polygon.move_to;
begin
 m_vertex      :=0;
 m_num_vertices:=0;
 m_clip_flags  :=clipping_flags(x ,y );

 if m_clip_flags = 0 then
  begin
   m_x[0 ]:=x;
   m_y[0 ]:=y;

   m_num_vertices:=1;

  end;

 m_x1 :=x;
 m_y1 :=y;
 m_cmd:=path_cmd_move_to;

end;

{ LINE_TO }
procedure vpgen_clip_polygon.line_to;
var
 flags : unsigned;

begin
 m_vertex      :=0;
 m_num_vertices:=0;

 flags:=clipping_flags(x ,y );

 if m_clip_flags = flags then
  if flags = 0 then
   begin
    m_x[0 ]:=x;
    m_y[0 ]:=y;

    m_num_vertices:=1;

   end
  else
 else
  m_num_vertices:=
   clip_liang_barsky_d(
    m_x1 ,m_y1 ,x ,y ,@m_clip_box ,@m_x ,@m_y );

 m_clip_flags:=flags;

 m_x1:=x;
 m_y1:=y;

end;

{ VERTEX }
function vpgen_clip_polygon.vertex;
var
 cmd : unsigned;

begin
 if m_vertex < m_num_vertices then
  begin
   x^:=m_x[m_vertex ];
   y^:=m_y[m_vertex ];

   inc(m_vertex );

   cmd  :=m_cmd;
   m_cmd:=path_cmd_line_to;

   result:=cmd;

  end
 else
  result:=path_cmd_stop;

end;

{ CLIPPING_FLAGS }
// Determine the clipping code of the vertex according to the 
// Cyrus-Beck line clipping algorithm
//
//        |        |
//  0110  |  0010  | 0011
//        |        |
// -------+--------+-------- clip_box.y2
//        |        |
//  0100  |  0000  | 0001
//        |        |
// -------+--------+-------- clip_box.y1
//        |        |
//  1100  |  1000  | 1001
//        |        |
//  clip_box.x1  clip_box.x2
function vpgen_clip_polygon.clipping_flags;
begin
 if x < m_clip_box.x1 then
  begin
   if y > m_clip_box.y2 then
    begin
     result:=6;

     exit;

    end;

   if y < m_clip_box.y1 then
    begin
     result:=12;

     exit;

    end;

   result:=4;

   exit;

  end;

 if x > m_clip_box.x2 then
  begin
   if y > m_clip_box.y2 then
    begin
     result:=3;

     exit;

    end;

   if y < m_clip_box.y1 then
    begin
     result:=9;

     exit;

    end;

   result:=1;

   exit;

  end;

 if y > m_clip_box.y2 then
  begin
   result:=2;

   exit;

  end;

 if y < m_clip_box.y1 then
  begin
   result:=8;

   exit;

  end;

 result:=0;

end;

END.

