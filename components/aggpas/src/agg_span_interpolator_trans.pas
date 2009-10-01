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
// Horizontal span interpolator for use with an arbitrary transformer
// The efficiency highly depends on the operations done in the transformer
//
// [Pascal Port History] -----------------------------------------------------
//
// 05.03.2006-Milano: Unit port establishment
//
{ agg_span_interpolator_trans.pas }
unit
 agg_span_interpolator_trans ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
 agg_basics ,
 agg_trans_affine ,
 agg_span_interpolator_linear ;

{ TYPES DEFINITION }
type
 span_interpolator_trans = object(span_interpolator )
   m_trans : trans_affine_ptr;

   m_x  ,
   m_y  : double;
   m_ix ,
   m_iy : int;

   constructor Construct(SS : unsigned = 8 ); overload;
   constructor Construct(trans : trans_affine_ptr; SS : unsigned = 8 ); overload;
   constructor Construct(trans : trans_affine_ptr; x ,y ,z : unsigned; SS : unsigned = 8 ); overload;

   function  _transformer : trans_affine_ptr; virtual;
   procedure transformer_(trans : trans_affine_ptr ); virtual;

   procedure begin_(x ,y  : double; len : unsigned ); virtual;

   procedure inc_operator; virtual;
   procedure coordinates(x ,y : int_ptr ); virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor span_interpolator_trans.Construct(SS : unsigned = 8 );
begin
 inherited Construct(SS );

 m_trans:=NIL;

end;

{ CONSTRUCT }
constructor span_interpolator_trans.Construct(trans : trans_affine_ptr; SS : unsigned = 8 );
begin
 inherited Construct(SS );

 m_trans:=trans;

end;

{ CONSTRUCT }
constructor span_interpolator_trans.Construct(trans : trans_affine_ptr; x ,y ,z : unsigned; SS : unsigned = 8 );
begin
 inherited Construct(SS );

 m_trans:=trans;

 begin_(x ,y ,0 );

end;

{ _TRANSFORMER }
function span_interpolator_trans._transformer;
begin
 result:=m_trans;

end;

{ TRANSFORMER_ }
procedure span_interpolator_trans.transformer_;
begin
 m_trans:=trans;

end;

{ BEGIN_ }
procedure span_interpolator_trans.begin_;
begin
 m_x:=x;
 m_y:=y;

 m_trans.transform(m_trans ,@x ,@y );

 m_ix:=iround(x * subpixel_size );
 m_iy:=iround(y * subpixel_size );

end;

{ INC_OPERATOR }
procedure span_interpolator_trans.inc_operator;
var
 x ,y : double;

begin
 m_x:=m_x + 1.0;

 x:=m_x;
 y:=m_y;

 m_trans.transform(m_trans ,@x ,@y );

 m_ix:=iround(x * subpixel_size );
 m_iy:=iround(y * subpixel_size );

end;

{ COORDINATES }
procedure span_interpolator_trans.coordinates;
begin
 x^:=m_ix;
 y^:=m_iy;

end;

END.

