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
// Class scanline_bin - binary scanline.
//
//----------------------------------------------------------------------------
//
// Adaptation for 32-bit screen coordinates (scanline32_bin) has been sponsored by
// Liberty Technology Systems, Inc., visit http://lib-sys.com
//
// Liberty Technology Systems, Inc. is the provider of
// PostScript and PDF technology for software developers.
//
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 15.01.2006-Milano: Unit port establishment
//
{ agg_scanline_bin.pas }
unit
 agg_scanline_bin ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_scanline ;

{ TYPES DEFINITION }
type
 span_bin_ptr = ^span_bin;
 span_bin = record
   x   ,
   len : int16;

  end;

//=============================================================scanline_bin
//
// This is binary scaline container which supports the interface
// used in the rasterizer::render(). See description of agg_scanline_u8
// for details.
//
//------------------------------------------------------------------------
 scanline_bin = object(scanline )
   m_max_len : unsigned;
   m_last_x  ,
   m_y       : int;

   m_spans    ,
   m_cur_span : span_bin_ptr;

   constructor Construct;
   destructor  Destruct;

   procedure reset(min_x ,max_x : int ); virtual;
   procedure reset_spans; virtual;

   procedure finalize(y_ : int ); virtual;
   procedure add_cell(x : int; cover : unsigned ); virtual;
   procedure add_span(x : int; len ,cover : unsigned ); virtual;

   function  y : int; virtual;
   function  num_spans : unsigned; virtual;
   function  begin_ : pointer; virtual;

   function  sz_of_span : unsigned; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor scanline_bin.Construct;
begin
 m_max_len:=0;
 m_last_x :=$7FFFFFF0;

 m_spans   :=NIL;
 m_cur_span:=NIL;

end;

{ DESTRUCT }
destructor scanline_bin.Destruct;
begin
 agg_freemem(pointer(m_spans ) ,m_max_len * sizeof(span_bin ) );

end;

{ RESET }
procedure scanline_bin.reset;
var
 max_len : unsigned;

begin
 max_len:=max_x - min_x + 3;

 if max_len > m_max_len then
  begin
   agg_freemem(pointer(m_spans ) ,m_max_len * sizeof(span_bin ) );
   agg_getmem (pointer(m_spans ) ,max_len * sizeof(span_bin ) );

   m_max_len:=max_len;

  end;

 m_last_x  :=$7FFFFFF0;
 m_cur_span:=m_spans;

end;

{ RESET_SPANS }
procedure scanline_bin.reset_spans;
begin
 m_last_x  :=$7FFFFFF0;
 m_cur_span:=m_spans;

end;

{ FINALIZE }
procedure scanline_bin.finalize;
begin
 m_y:=y_;

end;

{ ADD_CELL }
procedure scanline_bin.add_cell;
begin
 if x = m_last_x + 1 then
  inc(m_cur_span.len )
  
 else
  begin
   inc(ptrcomp(m_cur_span ) ,sizeof(span_bin ) );

   m_cur_span.x  :=int16(x );
   m_cur_span.len:=1;

  end;

 m_last_x:=x;

end;

{ ADD_SPAN }
procedure scanline_bin.add_span;
begin
 if x = m_last_x + 1 then
  m_cur_span.len:=int16(m_cur_span.len + len )

 else
  begin
   inc(ptrcomp(m_cur_span ) ,sizeof(span_bin ) );

   m_cur_span.x  :=int16(x );
   m_cur_span.len:=int16(len );

  end;

 m_last_x:=x + len - 1;

end;

{ Y }
function scanline_bin.y;
begin
 result:=m_y

end;

{ NUM_SPANS }
function scanline_bin.num_spans;
begin
 result:=(ptrcomp(m_cur_span ) - ptrcomp(m_spans ) ) div sizeof(span_bin );

end;

{ BEGIN_ }
function scanline_bin.begin_;
begin
 result:=span_bin_ptr(ptrcomp(m_spans ) + sizeof(span_bin ) );

end;

{ SZ_OF_SPAN }
function scanline_bin.sz_of_span;
begin
 result:=sizeof(span_bin );

end;

END.

