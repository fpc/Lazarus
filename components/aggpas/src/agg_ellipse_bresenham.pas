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
// Simple Bresenham interpolator for ellipsees
//
// [Pascal Port History] -----------------------------------------------------
//
// 31.01.2006-Milano: Unit port establishment
//
{ agg_ellipse_bresenham.pas }
unit
 agg_ellipse_bresenham ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ;

{ TYPES DEFINITION }
type
 ellipse_bresenham_interpolator = object
   m_rx2 ,
   m_ry2 ,

   m_two_rx2 ,
   m_two_ry2 ,

   m_dx ,
   m_dy ,

   m_inc_x ,
   m_inc_y ,
   m_cur_f : int;

   constructor Construct(rx ,ry : int );

   function _dx : int;
   function _dy : int;

   procedure inc_operator;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor ellipse_bresenham_interpolator.Construct;
begin
 m_rx2:=rx * rx;
 m_ry2:=ry * ry;

 m_two_rx2:=m_rx2 shl 1;
 m_two_ry2:=m_ry2 shl 1;

 m_dx:=0;
 m_dy:=0;

 m_inc_x:=0;
 m_inc_y:=-ry * m_two_rx2;
 m_cur_f:=0;

end;

{ _DX }
function ellipse_bresenham_interpolator._dx;
begin
 result:=m_dx;

end;

{ _DY }
function ellipse_bresenham_interpolator._dy;
begin
 result:=m_dy;

end;

{ INC_OPERATOR }
procedure ellipse_bresenham_interpolator.inc_operator;
var
 mx ,my ,mxy ,min_m ,fx ,fy ,fxy : int;

 flag : boolean;

begin
 mx:=m_cur_f + m_inc_x + m_ry2;
 fx:=mx;

 if mx < 0 then
  mx:=-mx;

 my:=m_cur_f + m_inc_y + m_rx2;
 fy:=my;

 if my < 0 then
  my:=-my;

 mxy:=m_cur_f + m_inc_x + m_ry2 + m_inc_y + m_rx2;
 fxy:=mxy;

 if mxy < 0 then
  mxy:=-mxy;

 min_m:=mx;
 flag :=true;

 if min_m > my then
  begin
   min_m:=my;
   flag :=false;

  end;

 m_dx:=0;
 m_dy:=0;

 if min_m > mxy then
  begin
   inc(m_inc_x ,m_two_ry2 );
   inc(m_inc_y ,m_two_rx2 );

   m_cur_f:=fxy;

   m_dx:=1;
   m_dy:=1;

   exit;

  end;

 if flag then
  begin
   inc(m_inc_x ,m_two_ry2 );

   m_cur_f:=fx;
   m_dx   :=1;

   exit;

  end;

 inc(m_inc_y ,m_two_rx2 );

 m_cur_f:=fy;
 m_dy   :=1;

end;

END.

