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
{ agg_span_generator.pas }
unit
 agg_span_generator ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_span_allocator ,
 agg_vertex_source ,
 agg_color ;

{ TYPES DEFINITION }
type
 span_generator_ptr = ^span_generator;
 span_generator = object(vertex_source )
   m_alloc : span_allocator_ptr;

   constructor Construct(alloc : span_allocator_ptr );

   procedure allocator_(alloc : span_allocator_ptr );
   function  _allocator : span_allocator_ptr;

   procedure prepare (max_span_len : unsigned ); virtual;
   function  generate(x ,y : int; len : unsigned ) : aggclr_ptr; virtual; abstract;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor span_generator.Construct;
begin
 m_alloc:=alloc;

end;

{ ALLOCATOR_ }
procedure span_generator.allocator_;
begin
 m_alloc:=alloc;

end;

{ _ALLOCATOR }
function span_generator._allocator;
begin
 result:=m_alloc;

end;

{ PREPARE }
procedure span_generator.prepare;
begin
 m_alloc.allocate(max_span_len );

end;

END.

