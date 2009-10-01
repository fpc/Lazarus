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
// Rounded rectangle vertex generator
//
// [Pascal Port History] -----------------------------------------------------
//
// 17.01.2006-Milano: Unit port establishment
//
{ agg_rounded_rect.pas }
unit
 agg_rounded_rect ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_vertex_source ,
 agg_arc ;

type
 rounded_rect = object(vertex_source )
   m_x1  ,
   m_y1  ,
   m_x2  ,
   m_y2  ,
   m_rx1 ,
   m_ry1 ,
   m_rx2 ,
   m_ry2 ,
   m_rx3 ,
   m_ry3 ,
   m_rx4 ,
   m_ry4 : double;

   m_status : unsigned;
   m_arc    : arc;

   constructor Construct; overload;
   constructor Construct(x1 ,y1 ,x2 ,y2 ,r : double ); overload;

   procedure rect(x1 ,y1 ,x2 ,y2 : double );

   procedure radius(r : double ); overload;
   procedure radius(rx ,ry : double ); overload;
   procedure radius(rx_bottom ,ry_bottom ,rx_top ,ry_top : double ); overload;
   procedure radius(rx1 ,ry1 ,rx2 ,ry2 ,rx3 ,ry3 ,rx4 ,ry4 : double ); overload;

   procedure normalize_radius;

   procedure approximation_scale_(s : double );
   function  _approximation_scale : double;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor rounded_rect.Construct;
begin
 m_x1 :=0;
 m_y1 :=0;
 m_x2 :=0;
 m_y2 :=0;
 m_rx1:=0;
 m_ry1:=0;
 m_rx2:=0;
 m_ry2:=0;
 m_rx3:=0;
 m_ry3:=0;
 m_rx4:=0;
 m_ry4:=0;

 m_status:=0;

 m_arc.Construct;

end;

{ CONSTRUCT }
constructor rounded_rect.Construct(x1 ,y1 ,x2 ,y2 ,r : double );
begin
 Construct;

 m_x1 :=x1;
 m_y1 :=y1;
 m_x2 :=x2;
 m_y2 :=y2;
 m_rx1:=r;
 m_ry1:=r;
 m_rx2:=r;
 m_ry2:=r;
 m_rx3:=r;
 m_ry3:=r;
 m_rx4:=r;
 m_ry4:=r;

 if x1 > x2 then
  begin
   m_x1:=x2;
   m_x2:=x1;

  end;

 if y1 > y2 then
  begin
   m_y1:=y2;
   m_y2:=y1;

  end; 

end;

{ RECT }
procedure rounded_rect.rect;
begin
 m_x1:=x1;
 m_y1:=y1;
 m_x2:=x2;
 m_y2:=y2;

 if x1 > x2 then
  begin
   m_x1:=x2;
   m_x2:=x1;

  end;

 if y1 > y2 then
  begin
   m_y1:=y2;
   m_y2:=y1;

  end;

end;

{ RADIUS }
procedure rounded_rect.radius(r : double );
begin
 m_rx1:=r;
 m_ry1:=r;
 m_rx2:=r;
 m_ry2:=r;
 m_rx3:=r;
 m_ry3:=r;
 m_rx4:=r;
 m_ry4:=r;

end;

{ RADIUS }
procedure rounded_rect.radius(rx ,ry : double );
begin
 m_rx1:=rx;
 m_rx2:=rx;
 m_rx3:=rx;
 m_rx4:=rx;
 m_ry1:=ry;
 m_ry2:=ry;
 m_ry3:=ry;
 m_ry4:=ry;

end;

{ RADIUS }
procedure rounded_rect.radius(rx_bottom ,ry_bottom ,rx_top ,ry_top : double );
begin
 m_rx1:=rx_bottom;
 m_rx2:=rx_bottom;
 m_rx3:=rx_top;
 m_rx4:=rx_top;
 m_ry1:=ry_bottom;
 m_ry2:=ry_bottom;
 m_ry3:=ry_top;
 m_ry4:=ry_top;

end;

{ RADIUS }
procedure rounded_rect.radius(rx1 ,ry1 ,rx2 ,ry2 ,rx3 ,ry3 ,rx4 ,ry4 : double );
begin
 m_rx1:=rx1;
 m_ry1:=ry1;
 m_rx2:=rx2;
 m_ry2:=ry2;
 m_rx3:=rx3;
 m_ry3:=ry3;
 m_rx4:=rx4;
 m_ry4:=ry4;

end;

{ NORMALIZE_RADIUS }
procedure rounded_rect.normalize_radius;
var
 dx ,dy  ,k ,t : double;

begin
 dx:=Abs(m_y2 - m_y1 );
 dy:=Abs(m_x2 - m_x1 );

 k:=1.0;

 try
  t:=dx / (m_rx1 + m_rx2 );

  if t < k then
   k:=t;

 except
 end;

 try
  t:=dx / (m_rx3 + m_rx4 );

  if t < k then
   k:=t;

 except
 end;

 try
  t:=dy / (m_ry1 + m_ry2 );

  if t < k then
   k:=t;

 except
 end;

 try
  t:=dy / (m_ry3 + m_ry4 );

  if t < k then
   k:=t;

 except
 end;  

 if k < 1.0 then
  begin
   m_rx1:=m_rx1 * k;
   m_ry1:=m_ry1 * k;
   m_rx2:=m_rx2 * k;
   m_ry2:=m_ry2 * k;
   m_rx3:=m_rx3 * k;
   m_ry3:=m_ry3 * k;
   m_rx4:=m_rx4 * k;
   m_ry4:=m_ry4 * k;

  end;

end;

{ APPROXIMATION_SCALE_ }
procedure rounded_rect.approximation_scale_;
begin
 m_arc.approximation_scale_(s );

end;

{ _APPROXIMATION_SCALE }
function rounded_rect._approximation_scale;
begin
 result:=m_arc._approximation_scale;

end;

{ REWIND }
procedure rounded_rect.rewind;
begin
 m_status:=0;

end;

{ VERTEX }
function rounded_rect.vertex;
var
 cmd : unsigned;

label
 _1 ,_2 ,_3 ,_4 ,_5 ,_6 ,_7 ,_8 ;

begin
 cmd:=path_cmd_stop;

 case m_status of
  0 :
   begin
    m_arc.init  (m_x1 + m_rx1 ,m_y1 + m_ry1 ,m_rx1 ,m_ry1 ,pi ,pi + pi * 0.5 );
    m_arc.rewind(0 );

    inc(m_status );

    goto _1;

   end;

  1 :
  _1:
   begin
    cmd:=m_arc.vertex(x ,y );

    if is_stop(cmd ) then
     begin
      inc(m_status );

      goto _2;

     end
    else
     begin
      result:=cmd;

      exit;

     end;

   end;

  2 :
  _2:
   begin
    m_arc.init  (m_x2 - m_rx2 ,m_y1 + m_ry2 ,m_rx2 ,m_ry2 ,pi + pi * 0.5 ,0.0 );
    m_arc.rewind(0 );

    inc(m_status );

    goto _3;

   end;

  3 :
  _3:
   begin
    cmd:=m_arc.vertex(x ,y );

    if is_stop(cmd ) then
     begin
      inc(m_status );

      goto _4;

     end
    else
     begin
      result:=path_cmd_line_to;

      exit;

     end;

   end;

  4 :
  _4: 
   begin
    m_arc.init  (m_x2 - m_rx3 ,m_y2 - m_ry3 ,m_rx3 ,m_ry3 ,0.0 ,pi * 0.5 );
    m_arc.rewind(0 );

    inc(m_status );

    goto _5;

   end;

  5 :
  _5:
   begin
    cmd:=m_arc.vertex(x ,y );

    if is_stop(cmd ) then
     begin
      inc(m_status );

      goto _6;

     end
    else
     begin
      result:=path_cmd_line_to;

      exit;

     end;

   end;

  6 :
  _6:
   begin
    m_arc.init  (m_x1 + m_rx4 ,m_y2 - m_ry4 ,m_rx4 ,m_ry4 ,pi * 0.5 ,pi );
    m_arc.rewind(0 );

    inc(m_status );

    goto _7;

   end;

  7 :
  _7:
   begin
    cmd:=m_arc.vertex(x ,y );

    if is_stop(cmd ) then
     begin
      inc(m_status );

      goto _8;

     end
    else
     begin
      result:=path_cmd_line_to;

      exit;

     end;

   end;

  8 :
  _8:
   begin
    cmd:=path_cmd_end_poly or path_flags_close or path_flags_ccw;

    inc(m_status );

   end;

 end;

 result:=cmd;

end;

END.

