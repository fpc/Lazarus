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
// 23.02.2006-Milano: Unit port establishment
//
{ agg_span_converter.pas }
unit
 agg_span_converter ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_color ,
 agg_span_generator ;

{ TYPES DEFINITION }
type
 span_convertor_ptr = ^span_convertor;
 span_convertor = object
   procedure convert(span : aggclr_ptr; x ,y : int; len : unsigned ); virtual; abstract;

  end;

 span_converter = object(span_generator )
   m_span_gen : span_generator_ptr;
   m_conv     : span_convertor_ptr;

   constructor Construct(span_gen : span_generator_ptr; conv : span_convertor_ptr );

   procedure prepare (max_span_len : unsigned ); virtual;
   function  generate(x ,y : int; len : unsigned ) : aggclr_ptr; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor span_converter.Construct;
begin
 m_span_gen:=span_gen;
 m_conv    :=conv;

end;

{ PREPARE }
procedure span_converter.prepare;
begin
 m_span_gen.prepare(max_span_len );

end;

{ GENERATE }
function span_converter.generate;
var
 span : aggclr_ptr;

begin
 span:=m_span_gen.generate(x ,y ,len );

 m_conv.convert(span ,x ,y ,len );

 result:=span;

end;

END.

