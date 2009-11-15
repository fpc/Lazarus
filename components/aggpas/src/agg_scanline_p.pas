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
// Class scanline_p - a general purpose scanline container with packed spans.
//
//----------------------------------------------------------------------------
//
// Adaptation for 32-bit screen coordinates (scanline32_p) has been sponsored by
// Liberty Technology Systems, Inc., visit http://lib-sys.com
//
// Liberty Technology Systems, Inc. is the provider of
// PostScript and PDF technology for software developers.
//
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 23.11.2005-Milano: ...
// 18.11.2005-Milano: Unit port establishment
//
{ agg_scanline_p.pas }
unit
 agg_scanline_p ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_scanline ;

{ TYPES DEFINITION }
type
 span_p8_ptr = ^span_p8;
 span_p8 = record
   x   ,
   len : int16; // If negative, it's a solid span, covers is valid

   covers : int8u_ptr;

  end;

 scanline_p8_ptr = ^scanline_p8;
 scanline_p8 = object(scanline )
   m_max_len : unsigned;
   m_last_x  ,
   m_y       : int;

   m_covers    ,
   m_cover_ptr : int8u_ptr;

   m_spans    ,
   m_cur_span : span_p8_ptr;

   constructor Construct;
   destructor  Destruct;

   procedure reset(min_x ,max_x : int ); virtual;
   procedure reset_spans; virtual;

   procedure finalize (y_ : int ); virtual;
   procedure add_cell (x : int; cover : unsigned ); virtual;
   procedure add_cells(x : int; len : unsigned; covers : int8u_ptr ); virtual;
   procedure add_span (x : int; len ,cover : unsigned ); virtual;

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
constructor scanline_p8.Construct;
begin
 m_max_len:=0;
 m_last_x :=$7FFFFFF0;

 m_y:=0;

 m_covers   :=NIL;
 m_cover_ptr:=NIL;

 m_spans   :=NIL;
 m_cur_span:=NIL;

end;

{ DESTRUCT }
destructor scanline_p8.Destruct;
begin
 agg_freemem(pointer(m_spans ) ,m_max_len * sizeof(span_p8 ) );
 agg_freemem(pointer(m_covers ) ,m_max_len * sizeof(int8u ) );

end;

{ RESET }
procedure scanline_p8.reset;
var
 max_len : unsigned;

begin
 max_len:=max_x - min_x + 3;

 if max_len > m_max_len then
  begin
   agg_freemem(pointer(m_covers ) ,m_max_len * sizeof(int8u ) );
   agg_freemem(pointer(m_spans ) ,m_max_len * sizeof(span_p8 ) );

   agg_getmem(pointer(m_covers ) ,max_len * sizeof(int8u ) );
   agg_getmem(pointer(m_spans ) ,max_len * sizeof(span_p8 ) );

   m_max_len:=max_len;

  end;

 m_last_x:=$7FFFFFF0;

 m_cover_ptr:=m_covers;
 m_cur_span :=m_spans;

 m_cur_span.len:=0;

end;

{ RESET_SPANS }
procedure scanline_p8.reset_spans;
begin
 m_last_x:=$7FFFFFF0;

 m_cover_ptr:=m_covers;
 m_cur_span :=m_spans;

 m_cur_span.len:=0;

end;

{ FINALIZE }
procedure scanline_p8.finalize;
begin
 m_y:=y_; 

end;

{ ADD_CELL }
procedure scanline_p8.add_cell;
begin
 m_cover_ptr^:=int8u(cover );

 if (x = m_last_x + 1 ) and
    (m_cur_span.len > 0 ) then
  inc(m_cur_span.len )

 else
  begin
   inc(ptrcomp(m_cur_span ) ,sizeof(span_p8 ) );

   m_cur_span.covers:=m_cover_ptr;

   m_cur_span.x  :=int16(x );
   m_cur_span.len:=1;

  end;

 m_last_x:=x;

 inc(ptrcomp(m_cover_ptr ) ,sizeof(int8u ) );

end;

{ ADD_CELLS }
procedure scanline_p8.add_cells;
begin
 move(covers^ ,m_cover_ptr^ ,len * sizeof(int8u ) );

 if (x = m_last_x + 1 ) and
    (m_cur_span.len > 0 ) then
  inc(m_cur_span.len ,int16(len ) )

 else
  begin
   inc(ptrcomp(m_cur_span ) ,sizeof(span_p8 ) );

   m_cur_span.covers:=m_cover_ptr;
   m_cur_span.x     :=int16(x );
   m_cur_span.len   :=int16(len );

  end;

 inc(ptrcomp(m_cover_ptr ) ,len * sizeof(int8u ) );

 m_last_x:=x + len - 1;

end;

{ ADD_SPAN }
procedure scanline_p8.add_span;
begin
 if (x = m_last_x + 1 ) and
    (m_cur_span.len < 0 ) and
    (cover = m_cur_span.covers^ ) then
  dec(m_cur_span.len ,int16(len ) )

 else
  begin
   m_cover_ptr^:=int8u(cover );

   inc(ptrcomp(m_cur_span ) ,sizeof(span_p8 ) );

   m_cur_span.covers:=m_cover_ptr;
   m_cur_span.x     :=int16(x );
   m_cur_span.len   :=int16(len );
   m_cur_span.len   :=-m_cur_span.len;

   inc(ptrcomp(m_cover_ptr ) ,sizeof(int8u ) );

  end;

 m_last_x:=x + len - 1; 

end;

{ Y }
function scanline_p8.y;
begin
 result:=m_y;

end;

{ NUM_SPANS }
function scanline_p8.num_spans;
begin
 result:=(ptrcomp(m_cur_span ) - ptrcomp(m_spans ) ) div sizeof(span_p8 );

end;

{ BEGIN_ }
function scanline_p8.begin_;
begin
 result:=span_p8_ptr(ptrcomp(m_spans ) + sizeof(span_p8 ) );

end;

{ SZ_OF_SPAN }
function scanline_p8.sz_of_span;
begin
 result:=sizeof(span_p8 );

end;

END.

