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
// 15.01.2006-Milano: Unit port establishment
//
{ agg_gamma_functions.pas }
unit
 agg_gamma_functions ;

INTERFACE

{$I agg_mode.inc }

uses
 Math ,
 agg_basics ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 gamma_none = object(vertex_source )
  end;

 gamma_power = object(vertex_source )
   m_gamma : double;

   constructor Construct; overload;
   constructor Construct(g : double ); overload;

   procedure gamma_(g : double );
   function  _gamma : double;

   function func_operator_gamma(x : double ) : double; virtual;

  end;

 gamma_threshold = object(vertex_source )
   m_threshold : double;

   constructor Construct; overload;
   constructor Construct(t : double ); overload;

   procedure threshold_(t : double );
   function  _threshold : double;

   function func_operator_gamma(x : double ) : double; virtual;

  end;

 gamma_linear = object(vertex_source )
   m_start ,
   m_end   : double;

   constructor Construct; overload;
   constructor Construct(s ,e : double ); overload;

   procedure set_  (s ,e : double );
   procedure start_(s : double );
   procedure end_  (e : double );

   function  _start : double;
   function  _end : double;

   function func_operator_gamma(x : double ) : double; virtual;

  end;

 gamma_multiply = object(vertex_source )
   m_mul : double;

   constructor Construct; overload;
   constructor Construct(v : double ); overload;

   procedure value_(v : double );
   function  _value : double;

   function func_operator_gamma(x : double ) : double; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor gamma_power.Construct;
begin
 m_gamma:=1.0;

end;

{ CONSTRUCT }
constructor gamma_power.Construct(g : double );
begin
 m_gamma:=g;

end;

{ GAMMA_ }
procedure gamma_power.gamma_;
begin
 m_gamma:=g;

end;

{ _GAMMA }
function gamma_power._gamma;
begin
 result:=m_gamma;

end;

{ FUNC_OPERATOR_GAMMA }
function gamma_power.func_operator_gamma;
begin
 try
  result:=power(x ,m_gamma );

 except
  result:=1;

 end;

end;

{ CONSTRUCT }
constructor gamma_threshold.Construct;
begin
 m_threshold:=0.5;

end;

{ CONSTRUCT }
constructor gamma_threshold.Construct(t : double );
begin
 m_threshold:=t;

end;

{ THRESHOLD_ }
procedure gamma_threshold.threshold_;
begin
 m_threshold:=t;

end;

{ _THRESHOLD }
function gamma_threshold._threshold;
begin
 result:=m_threshold;

end;

{ FUNC_OPERATOR_GAMMA }
function gamma_threshold.func_operator_gamma;
begin
 if x < m_threshold then
  result:=0.0
 else
  result:=1.0;

end;

{ CONSTRUCT }
constructor gamma_linear.Construct;
begin
 m_start:=0;
 m_end  :=1;

end;

{ CONSTRUCT }
constructor gamma_linear.Construct(s ,e : double );
begin
 m_start:=s;
 m_end  :=e;

end;

{ SET_ }
procedure gamma_linear.set_;
begin
 m_start:=s;
 m_end  :=e;

end;

{ START_ }
procedure gamma_linear.start_;
begin
 m_start:=s;

end;

{ END_ }
procedure gamma_linear.end_;
begin
 m_end:=e;

end;

{ _START }
function gamma_linear._start;
begin
 result:=m_start;

end;

{ _END }
function gamma_linear._end;
begin
 result:=m_end;

end;

{ FUNC_OPERATOR_GAMMA }
function gamma_linear.func_operator_gamma;
begin
 if x < m_start then
  result:=0
 else
  if x > m_end then
   result:=1
  else
   if m_end - m_start <> 0 then
    result:=(x - m_start ) / (m_end - m_start )
   else
    result:=0;

end;

{ CONSTRUCT }
constructor gamma_multiply.Construct;
begin
 m_mul:=1.0;

end;

{ CONSTRUCT }
constructor gamma_multiply.Construct(v : double );
begin
 m_mul:=v;

end;

{ VALUE_ }
procedure gamma_multiply.value_;
begin
 m_mul:=v;

end;

{ _VALUE }
function gamma_multiply._value;
begin
 result:=m_mul;

end;

{ FUNC_OPERATOR_GAMMA }
function gamma_multiply.func_operator_gamma;
var
 y : double;

begin
 y:=x * m_mul;

 if y > 1.0 then
  y:=1.0;

 result:=y;

end;

END.

