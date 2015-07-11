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
// 07.03.2006-Milano: Unit port establishment
//
{ agg_span_interpolator_adaptor.pas }
unit
 agg_span_interpolator_adaptor ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_span_interpolator_linear ,
 agg_trans_affine ;

{ TYPES DEFINITION }
type
 distortion_ptr = ^distortion;
 distortion = object
   procedure calculate(x ,y : int_ptr ); virtual; abstract;

  end;

 span_interpolator_adaptor = object(span_interpolator_linear )
   m_distortion : distortion_ptr;

   constructor Construct; overload;
   constructor Construct(trans : trans_affine_ptr; dist : distortion_ptr ); overload;
   constructor Construct(
                trans : trans_affine_ptr;
                dist : distortion_ptr;
                x ,y : double; len : unsigned ); overload;

   function  _distortion : distortion_ptr;
   procedure distortion_(dist : distortion_ptr );

   procedure coordinates(x ,y : int_ptr ); virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor span_interpolator_adaptor.Construct;
begin
 inherited Construct;

 m_distortion:=NIL;

end;

{ CONSTRUCT }
constructor span_interpolator_adaptor.Construct(trans : trans_affine_ptr; dist : distortion_ptr );
begin
 inherited Construct(trans );

 m_distortion:=dist;

end;

{ CONSTRUCT }
constructor span_interpolator_adaptor.Construct(
                trans : trans_affine_ptr;
                dist : distortion_ptr;
                x ,y : double; len : unsigned );
begin
 inherited Construct(trans ,x ,y ,len );

 m_distortion:=dist;

end;

{ _DISTORTION }
function span_interpolator_adaptor._distortion;
begin
 result:=m_distortion;

end;

{ DISTORTION_ }
procedure span_interpolator_adaptor.distortion_;
begin
 m_distortion:=dist;

end;

{ COORDINATES }
procedure span_interpolator_adaptor.coordinates;
begin
 inherited coordinates(x ,y );

 m_distortion.calculate(x ,y );

end;

END.

