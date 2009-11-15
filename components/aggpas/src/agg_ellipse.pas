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
// class ellipse
//
// [Pascal Port History] -----------------------------------------------------
//
// 18.12.2005-Milano: Unit port establishment
//
{ agg_ellipse.pas }
unit
 agg_ellipse ;

INTERFACE

{$I agg_mode.inc }

uses
 Math ,
 agg_basics ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 ellipse_ptr = ^ellipse;
 ellipse = object(vertex_source )
   m_x  ,
   m_y  ,
   m_rx ,
   m_ry ,

   m_scale : double;

   m_num  ,
   m_step : unsigned;
   m_cw   : boolean;

   constructor Construct; overload;
   constructor Construct(x ,y ,rx ,ry : double; num_steps : unsigned = 0; cw : boolean = false ); overload;

   procedure init(x ,y ,rx ,ry : double; num_steps : unsigned = 0; cw : boolean = false );

   procedure approximation_scale_(scale : double );

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

   procedure calc_num_steps;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor ellipse.Construct;
begin
 inherited Construct;

 m_x :=0.0;
 m_y :=0.0;
 m_rx:=1.0;
 m_ry:=1.0;

 m_scale:=1.0;

 m_num :=4;
 m_step:=0;
 m_cw  :=false;

end;

{ CONSTRUCT }
constructor ellipse.Construct(x ,y ,rx ,ry : double; num_steps : unsigned = 0; cw : boolean = false );
begin
 inherited Construct;

 m_x :=x;
 m_y :=y;
 m_rx:=rx;
 m_ry:=ry;

 m_scale:=1.0;

 m_num :=num_steps;
 m_step:=0;
 m_cw  :=cw;

 if m_num = 0 then
  calc_num_steps;

end;

{ INIT }
procedure ellipse.init;
begin
 m_x :=x;
 m_y :=y;
 m_rx:=rx;
 m_ry:=ry;

 m_num :=num_steps;
 m_step:=0;
 m_cw  :=cw;

 if m_num = 0 then
  calc_num_steps;

end;

{ APPROXIMATION_SCALE_ }
procedure ellipse.approximation_scale_;
begin
 m_scale:=scale;

 calc_num_steps;

end;

{ REWIND }
procedure ellipse.rewind;
begin
 m_step:=0;

end;

{ VERTEX }
function ellipse.vertex;
var
 angle : double;

begin
 if m_step = m_num then
  begin
   inc(m_step );

   result:=path_cmd_end_poly or path_flags_close or path_flags_ccw;

   exit;

  end;

 if m_step > m_num then
  begin
   result:=path_cmd_stop;

   exit;

  end;

 angle:=m_step / m_num * 2.0 * pi;

 if m_cw then
  angle:=2.0 * pi - angle;

 x^:=m_x + Cos(angle ) * m_rx;
 y^:=m_y + Sin(angle ) * m_ry;

 inc(m_step );

 if m_step = 1 then
  result:=path_cmd_move_to
 else
  result:=path_cmd_line_to;

end;

{ CALC_NUM_STEPS }
procedure ellipse.calc_num_steps;
var
 ra ,
 da : double;

begin
 ra:=(Abs(m_rx ) + Abs(m_ry ) ) / 2;
 da:=ArcCos(ra / (ra + 0.125 / m_scale ) ) * 2;

 m_num:=trunc(2 * pi / da );

end;

END.

