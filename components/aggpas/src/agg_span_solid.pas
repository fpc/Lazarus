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
// span_solid_rgba8
//
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 30.01.2006-Milano: Unit port establishment
//
{ agg_span_solid.pas }
unit
 agg_span_solid ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_span_allocator ,
 agg_span_generator ,
 agg_color ;

{ TYPES DEFINITION }
type
 span_solid_ptr = ^span_solid;
 span_solid = object(span_generator )
   m_color : aggclr;

   constructor Construct(alloc : span_allocator_ptr );

   procedure color_(c : aggclr_ptr );
   function  _color : aggclr_ptr;

   function  generate(x ,y : int; len : unsigned ) : aggclr_ptr; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor span_solid.Construct;
begin
 inherited Construct(alloc );

 m_color.Construct;

end;

{ COLOR_ }
procedure span_solid.color_;
begin
 m_color:=c^;

end;

{ _COLOR }
function span_solid._color;
begin
 result:=@m_color;

end;

{ GENERATE }
function span_solid.generate;
var
 span : aggclr_ptr;

begin
 span:=_allocator.span;

 repeat
  span^:=m_color;

  inc(ptrcomp(span ) ,sizeof(aggclr ) );
  dec(len );

 until len = 0;

 result:=_allocator.span;

end;

END.

