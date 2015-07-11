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
// 04.03.2006-Milano: Unit port establishment
//
{ agg_span_subdiv_adaptor.pas }
unit
 agg_span_subdiv_adaptor ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_span_interpolator_linear ,
 agg_trans_affine ;

{ TYPES DEFINITION }
type
 span_subdiv_adaptor = object(span_interpolator )
   m_subdiv_shift ,
   m_subdiv_size  ,
   m_subdiv_mask  : unsigned;

   m_interpolator : span_interpolator_ptr;

   m_src_x : int;
   m_src_y : double;
   m_pos   ,
   m_len   : unsigned;

   constructor Construct(SS : unsigned = 8 ); overload;
   constructor Construct(interpolator : span_interpolator_ptr; subdiv_shift : unsigned = 4; SS : unsigned = 8 ); overload;
   constructor Construct(
                interpolator : span_interpolator_ptr;
                x ,y : double; len : unsigned;
                subdiv_shift : unsigned = 4;
                SS : unsigned = 8 ); overload;

   function  _interpolator : span_interpolator_ptr;
   procedure interpolator_(intr : span_interpolator_ptr );

   function  _transformer : trans_affine_ptr; virtual;
   procedure transformer_(trans : trans_affine_ptr ); virtual;

   function  _subdiv_shift : unsigned;
   procedure subdiv_shift_(shift : unsigned );

   procedure begin_(x ,y  : double; len : unsigned ); virtual;

   procedure inc_operator; virtual;
   procedure coordinates(x ,y : int_ptr ); virtual;

   procedure local_scale(x ,y : int_ptr ); virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor span_subdiv_adaptor.Construct(SS : unsigned = 8 );
begin
 subpixel_shift:=SS;
 subpixel_size :=1 shl subpixel_shift;

 m_subdiv_shift:=4;
 m_subdiv_size :=1 shl m_subdiv_shift;
 m_subdiv_mask :=m_subdiv_size - 1;

 m_interpolator:=NIL;

end;

{ CONSTRUCT }
constructor span_subdiv_adaptor.Construct(interpolator : span_interpolator_ptr; subdiv_shift : unsigned = 4; SS : unsigned = 8 );
begin
 subpixel_shift:=SS;
 subpixel_size :=1 shl subpixel_shift;

 m_subdiv_shift:=subdiv_shift;
 m_subdiv_size :=1 shl m_subdiv_shift;
 m_subdiv_mask :=m_subdiv_size - 1;

 m_interpolator:=interpolator;

end;

{ CONSTRUCT }
constructor span_subdiv_adaptor.Construct(
             interpolator : span_interpolator_ptr;
             x ,y : double; len : unsigned;
             subdiv_shift : unsigned = 4;
             SS : unsigned = 8 );
begin
 subpixel_shift:=SS;
 subpixel_size :=1 shl subpixel_shift;

 m_subdiv_shift:=subdiv_shift;
 m_subdiv_size :=1 shl m_subdiv_shift;
 m_subdiv_mask :=m_subdiv_size - 1;

 m_interpolator:=interpolator;

 begin_(x ,y ,len );

end;

{ _INTERPOLATOR }
function span_subdiv_adaptor._interpolator;
begin
 result:=m_interpolator;

end;

{ INTERPOLATOR_ }
procedure span_subdiv_adaptor.interpolator_;
begin
 m_interpolator:=intr;

end;

{ _TRANSFORMER }
function span_subdiv_adaptor._transformer;
begin
 result:=m_interpolator._transformer;

end;

{ TRANSFORMER_ }
procedure span_subdiv_adaptor.transformer_;
begin
 m_interpolator.transformer_(trans );

end;

{ _SUBDIV_SHIFT }
function span_subdiv_adaptor._subdiv_shift;
begin
 result:=m_subdiv_shift;

end;

{ SUBDIV_SHIFT_ }
procedure span_subdiv_adaptor.subdiv_shift_;
begin
 m_subdiv_shift:=shift;
 m_subdiv_size :=1 shl m_subdiv_shift;
 m_subdiv_mask :=m_subdiv_size - 1;

end;

{ BEGIN_ }
procedure span_subdiv_adaptor.begin_;
begin
 m_pos  :=1;
 m_src_x:=trunc(x * subpixel_size ) + subpixel_size;
 m_src_y:=y;
 m_len  :=len;

 if len > m_subdiv_size then
  len:=m_subdiv_size;

 m_interpolator.begin_(x ,y ,len );

end;

{ INC_OPERATOR }
procedure span_subdiv_adaptor.inc_operator;
var
 len : unsigned;

begin
 m_interpolator.inc_operator;

 if m_pos >= m_subdiv_size then
  begin
   len:=m_len;

   if len > m_subdiv_size then
    len:=m_subdiv_size;

   m_interpolator.resynchronize(m_src_x / subpixel_size + len ,m_src_y ,len );

   m_pos:=0;

  end;

 inc(m_src_x ,subpixel_size );
 inc(m_pos );
 dec(m_len );

end;

{ COORDINATES }
procedure span_subdiv_adaptor.coordinates;
begin
 m_interpolator.coordinates(x ,y );

end;

{ LOCAL_SCALE }
procedure span_subdiv_adaptor.local_scale;
begin
 m_interpolator.local_scale(x ,y );

end;

END.

