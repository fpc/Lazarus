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
// Arc vertex generator
//
// [Pascal Port History] -----------------------------------------------------
//
// 17.01.2006-Milano: Unit port establishment
//
{ agg_arc.pas }
unit
 agg_arc ;

INTERFACE

{$I agg_mode.inc }

uses
 Math ,
 agg_basics ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 arc = object(vertex_source )
   m_x      ,
   m_y      ,
   m_rx     ,
   m_ry     ,
   m_angle  ,
   m_start  ,
   m_end    ,
   m_scale  ,
   m_da     : double;

   m_ccw         ,
   m_initialized : boolean;

   m_path_cmd : unsigned;

   constructor Construct; overload;
   constructor Construct(x ,y ,rx ,ry ,a1 ,a2 : double; ccw : boolean = true ); overload;

   procedure init(x ,y ,rx ,ry ,a1 ,a2 : double; ccw : boolean = true );

   procedure approximation_scale_(s : double );
   function  _approximation_scale : double;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

   procedure normalize(a1 ,a2 : double; ccw : boolean );

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor arc.Construct;
begin
 m_x    :=0;
 m_y    :=0;
 m_rx   :=0;
 m_ry   :=0;
 m_angle:=0;
 m_start:=0;
 m_end  :=0;
 m_da   :=0;

 m_ccw     :=false;
 m_path_cmd:=0;

 m_scale:=1;

 m_initialized:=false;

end;

{ CONSTRUCT }
constructor arc.Construct(x ,y ,rx ,ry ,a1 ,a2 : double; ccw : boolean = true );
begin
 Construct;

 m_x :=x;
 m_y :=y;
 m_rx:=rx;
 m_ry:=ry;

 m_scale:=1;

 normalize(a1 ,a2 ,ccw );

end;

{ INIT }
procedure arc.init;
begin
 m_x :=x;
 m_y :=y;
 m_rx:=rx;
 m_ry:=ry;

 normalize(a1 ,a2 ,ccw );

end;

{ APPROXIMATION_SCALE_ }
procedure arc.approximation_scale_;
begin
 m_scale:=s;

 if m_initialized then
  normalize(m_start ,m_end ,m_ccw );

end;

{ _APPROXIMATION_SCALE }
function arc._approximation_scale;
begin
 result:=m_scale;

end;

{ REWIND }
procedure arc.rewind;
begin
 m_path_cmd:=path_cmd_move_to;
 m_angle   :=m_start;

end;

{ VERTEX }
function arc.vertex;
var
 pf : unsigned;

begin
 if is_stop(m_path_cmd ) then
  result:=path_cmd_stop

 else
  if (m_angle < m_end - m_da / 4 ) <> m_ccw then
   begin
    x^:=m_x + Cos(m_end ) * m_rx;
    y^:=m_y + Sin(m_end ) * m_ry;

    m_path_cmd:=path_cmd_stop;

    result:=path_cmd_line_to;

   end
  else
   begin
    x^:=m_x + Cos(m_angle ) * m_rx;
    y^:=m_y + Sin(m_angle ) * m_ry;

    m_angle:=m_angle + m_da;

    pf        :=m_path_cmd;
    m_path_cmd:=path_cmd_line_to;

    result:=pf;

   end;

end;

{ NORMALIZE }
procedure arc.normalize;
var
 ra : double;

begin
 ra  :=(Abs(m_rx ) + Abs(m_ry ) ) / 2;
 m_da:=ArcCos(ra / (ra + 0.125 / m_scale ) ) * 2;

 if ccw then
  while a2 < a1 do
   a2:=a2 + (pi * 2.0 )
 else
  begin
   while a1 < a2 do
    a1:=a1 + (pi * 2.0 );

   m_da:=-m_da; 

  end;

 m_ccw  :=ccw;
 m_start:=a1;
 m_end  :=a2;

 m_initialized:=true;

end;

END.

