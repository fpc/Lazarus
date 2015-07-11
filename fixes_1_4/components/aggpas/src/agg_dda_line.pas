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
// classes dda_line_interpolator, dda2_line_interpolator
//
// [Pascal Port History] -----------------------------------------------------
//
// 27.01.2006-Milano: dda_line_interpolator
// 16.01.2006-Milano: Unit port establishment
//
{ agg_dda_line.pas }
unit
 agg_dda_line ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
 agg_basics ;

{ TYPES DEFINITION }
const
 subpixel_shift = 8;
 subpixel_size  = 1 shl subpixel_shift;
 subpixel_mask  = subpixel_size - 1;

type
//===================================================dda_line_interpolator
 dda_line_interpolator = object
   m_y ,m_inc ,m_dy ,

   FractionShift ,YShift : int;

   constructor Construct(FS : int; YS : int = 0 ); overload;
   constructor Construct(y1 ,y2 : int; count : unsigned; FS : int; YS : int = 0 ); overload;

   procedure plus_operator;
   procedure minus_operator;
   procedure inc_operator(n : int );
   procedure dec_operator(n : int );

   function  _y : int;
   function  _dy : int;

  end;

//---------------------------------------------line_bresenham_interpolator
 dda2_line_interpolator = object
   m_cnt ,
   m_lft ,
   m_rem ,
   m_mod ,
   m_y   : int;

   constructor Construct(y1 ,y2 ,count : int ); overload;  // Forward-adjusted line
   constructor Construct(y ,count : int ); overload;       // Backward-adjusted line

   procedure plus_operator;
   procedure minus_operator;

   function  _mod : int;
   function  _rem : int;
   function  _lft : int;
   function  _y : int;

   procedure adjust_forward;
   procedure adjust_backward;

  end;

 line_bresenham_interpolator = object
   m_x1_lr ,
   m_y1_lr ,
   m_x2_lr ,
   m_y2_lr : int;

   m_ver : boolean;
   m_len : unsigned;
   m_inc : int;

   m_interpolator : dda2_line_interpolator;

   constructor Construct(x1 ,y1 ,x2 ,y2 : int );

   function  line_lr(v : int ) : int;

   function _is_ver : boolean;
   function _len : unsigned;
   function _inc : int;

   procedure hstep;
   procedure vstep;

   function _x1 : int;
   function _y1 : int;
   function _x2 : int;
   function _y2 : int;
   function _x2_hr : int;
   function _y2_hr : int;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor dda_line_interpolator.Construct(FS : int; YS : int = 0 );
begin
 FractionShift:=FS;

 YShift:=YS;

end;

{ CONSTRUCT }
constructor dda_line_interpolator.Construct(y1 ,y2 : int; count : unsigned; FS : int; YS : int = 0 );
begin
 Construct(FS ,YS );

 m_y  :=y1;
 m_inc:=((y2 - y1 ) shl FractionShift ) div count;
 m_dy :=0;

end;

{ PLUS_OPERATOR }
procedure dda_line_interpolator.plus_operator;
begin
 inc(m_dy ,m_inc );

end;

{ MINUS_OPERATOR }
procedure dda_line_interpolator.minus_operator;
begin
 dec(m_dy ,m_inc );

end;

{ INC_OPERATOR }
procedure dda_line_interpolator.inc_operator;
begin
 inc(m_dy ,m_inc * n );

end;

{ DEC_OPERATOR }
procedure dda_line_interpolator.dec_operator;
begin
 dec(m_dy ,m_inc * n );

end;

{ _Y }
function dda_line_interpolator._y;
begin
 result:=m_y + (shr_int32(m_dy ,FractionShift - YShift ) );

end;

{ _DY }
function dda_line_interpolator._dy;
begin
 result:=m_dy;

end;

{ CONSTRUCT }
constructor dda2_line_interpolator.Construct(y1 ,y2 ,count : int );
begin
 if count <= 0 then
  m_cnt:=1
 else
  m_cnt:=count;

 m_lft:=trunc((y2 - y1 ) / m_cnt );
 m_rem:=trunc((y2 - y1 ) mod m_cnt );
 m_mod:=m_rem;
 m_y  :=y1;

 if m_mod <= 0 then
  begin
   m_mod:=m_mod + count;
   m_rem:=m_rem + count;

   dec(m_lft );

  end;

 m_mod:=m_mod - count;

end;

{ CONSTRUCT }
constructor dda2_line_interpolator.Construct(y ,count : int );
begin
 if count <= 0 then
  m_cnt:=1
 else
  m_cnt:=count;

 m_lft:=y div m_cnt;
 m_rem:=y mod m_cnt;
 m_mod:=m_rem;
 m_y  :=0;

 if m_mod <= 0 then
  begin
   inc(m_mod ,count );
   inc(m_rem ,count );
   dec(m_lft );

  end;

end;

{ PLUS_OPERATOR }
procedure dda2_line_interpolator.plus_operator;
begin
 inc(m_mod ,m_rem );
 inc(m_y ,m_lft );

 if m_mod > 0 then
  begin
   dec(m_mod ,m_cnt );
   inc(m_y );

  end;

end;

{ MINUS_OPERATOR }
procedure dda2_line_interpolator.minus_operator;
begin
 if m_mod <= m_rem then
  begin
   inc(m_mod ,m_cnt );
   dec(m_y );

  end;

 dec(m_mod ,m_rem );
 dec(m_y ,m_lft );

end;

{ _MOD }
function dda2_line_interpolator._mod;
begin
 result:=m_mod;

end;

{ _REM }
function dda2_line_interpolator._rem;
begin
 result:=m_rem;

end;

{ _LFT }
function dda2_line_interpolator._lft;
begin
 result:=m_lft;

end;

{ _Y }
function dda2_line_interpolator._y;
begin
 result:=m_y;

end;

{ ADJUST_FORWARD }
procedure dda2_line_interpolator.adjust_forward;
begin
 dec(m_mod ,m_cnt );

end;

{ ADJUST_BACKWARD }
procedure dda2_line_interpolator.adjust_backward;
begin
 inc(m_mod ,m_cnt );

end;

{ CONSTRUCT }
constructor line_bresenham_interpolator.Construct;
begin
 m_x1_lr:=line_lr(x1 );
 m_y1_lr:=line_lr(y1 );
 m_x2_lr:=line_lr(x2 );
 m_y2_lr:=line_lr(y2 );

 m_ver:=Abs(m_x2_lr - m_x1_lr ) < Abs(m_y2_lr - m_y1_lr );

 if m_ver then
  m_len:=Abs(m_y2_lr - m_y1_lr )
 else
  m_len:=Abs(m_x2_lr - m_x1_lr );

 if m_ver then
  if y2 > y1 then
   m_inc:=1
  else
   m_inc:=-1
 else
  if x2 > x1 then
   m_inc:=1
  else
   m_inc:=-1;

 if m_ver then
  m_interpolator.Construct(x1 ,x2 ,m_len )
 else
  m_interpolator.Construct(y1 ,y2 ,m_len );

end;

{ LINE_LR }
function line_bresenham_interpolator.line_lr;
begin
 result:=shr_int32(v ,subpixel_shift );

end;

{ _IS_VER }
function line_bresenham_interpolator._is_ver;
begin
 result:=m_ver;

end;

{ _LEN }
function line_bresenham_interpolator._len;
begin
 result:=m_len;

end;

{ _INC }
function line_bresenham_interpolator._inc;
begin
 result:=m_inc;

end;

{ HSTEP }
procedure line_bresenham_interpolator.hstep;
begin
 m_interpolator.plus_operator;

 m_x1_lr:=m_x1_lr + m_inc;

end;

{ VSTEP }
procedure line_bresenham_interpolator.vstep;
begin
 m_interpolator.plus_operator;

 m_y1_lr:=m_y1_lr + m_inc;

end;

{ _X1 }
function line_bresenham_interpolator._x1;
begin
 result:=m_x1_lr;

end;

{ _Y1 }
function line_bresenham_interpolator._y1;
begin
 result:=m_y1_lr;

end;

{ _X2 }
function line_bresenham_interpolator._x2;
begin
 result:=line_lr(m_interpolator._y );

end;

{ _Y2 }
function line_bresenham_interpolator._y2;
begin
 result:=line_lr(m_interpolator._y );

end;

{ _X2_HR }
function line_bresenham_interpolator._x2_hr;
begin
 result:=m_interpolator._y;

end;

{ _Y2_HR }
function line_bresenham_interpolator._y2_hr;
begin
 result:=m_interpolator._y;

end;

END.


