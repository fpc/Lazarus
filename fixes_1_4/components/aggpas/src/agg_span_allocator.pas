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
// 27.01.2006-Milano: Unit port establishment
//
{ agg_span_allocator.pas }
unit
 agg_span_allocator ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_color ;

{ TYPES DEFINITION }
type
 span_allocator_ptr = ^span_allocator;
 span_allocator = object
   m_max_span_len : unsigned;

   m_span : aggclr_ptr;

   constructor Construct;
   destructor  Destruct;

   function allocate(max_span_len : unsigned ) : aggclr_ptr;
   function span : aggclr_ptr;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor span_allocator.Construct;
begin
 m_max_span_len:=0;

 m_span:=NIL;

end;

{ DESTRUCT }
destructor span_allocator.Destruct;
begin
 agg_freemem(pointer(m_span ) ,m_max_span_len * sizeof(aggclr ) );
 
end;

{ ALLOCATE }
function span_allocator.allocate;
begin
 if max_span_len > m_max_span_len then
  begin
   agg_freemem(pointer(m_span ) ,m_max_span_len * sizeof(aggclr ) );

  // To reduce the number of reallocs we align the
  // span_len to 256 color elements.
  // Well, I just like this number and it looks reasonable.
   max_span_len:=((max_span_len + 255 ) shr 8 ) shl 8;

   agg_getmem(pointer(m_span ) ,max_span_len * sizeof(aggclr ) );

   m_max_span_len:=max_span_len;

  end;

 result:=m_span;

end;

{ SPAN }
function span_allocator.span;
begin
 result:=m_span;
 
end;

END.

