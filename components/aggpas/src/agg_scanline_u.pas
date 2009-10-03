//----------------------------------------------------------------------------
// Anti-Grain Geometry - Version 2.4 (Public License)
// Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)
//
// Anti-Grain Geometry - Version 2.4 Release Milano 4 (AggPas 2.4 RM3)
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
// Adaptation for 32-bit screen coordinates (scanline32_u) has been sponsored by 
// Liberty Technology Systems, Inc., visit http://lib-sys.com
//
// Liberty Technology Systems, Inc. is the provider of
// PostScript and PDF technology for software developers.
//
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 17.01.2006-Milano: Unit port establishment
//
{ agg_scanline_u.pas }
unit
 agg_scanline_u ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_scanline ,
 agg_alpha_mask_u8 ;

{ TYPES DEFINITION }
type
//==============================================================scanline_u
//
// Unpacked scanline container class
//
// This class is used to transfer data from a scanline rasterizer
// to the rendering buffer. It's organized very simple. The class stores
// information of horizontal spans to render it into a pixel-map buffer.
// Each span has staring X, length, and an array of bytes that determine the
// cover-values for each pixel.
// Before using this class you should know the minimal and maximal pixel
// coordinates of your scanline. The protocol of using is:
// 1. reset(min_x, max_x)
// 2. add_cell() / add_span() - accumulate scanline. 
//    When forming one scanline the next X coordinate must be always greater
//    than the last stored one, i.e. it works only with ordered coordinates.
// 3. Call finalize(y) and render the scanline.
// 3. Call reset_spans() to prepare for the new scanline.
//    
// 4. Rendering:
// 
// Scanline provides an iterator class that allows you to extract
// the spans and the cover values for each pixel. Be aware that clipping
// has not been done yet, so you should perform it yourself.
// Use scanline_u8::iterator to render spans:
//-------------------------------------------------------------------------
//
// int y = sl.y();                    // Y-coordinate of the scanline
//
// ************************************
// ...Perform vertical clipping here...
// ************************************
//
// scanline_u8::const_iterator span = sl.begin();
// 
// unsigned char* row = m_rbuf->row(y); // The the address of the beginning 
//                                      // of the current row
// 
// unsigned num_spans = sl.num_spans(); // Number of spans. It's guaranteed that
//                                      // num_spans is always greater than 0.
//
// do
// {
//     const scanline_u8::cover_type* covers =
//         span->covers;                     // The array of the cover values
//
//     int num_pix = span->len;              // Number of pixels of the span.
//                                           // Always greater than 0, still it's
//                                           // better to use "int" instead of 
//                                           // "unsigned" because it's more
//                                           // convenient for clipping
//     int x = span->x;
//
//     **************************************
//     ...Perform horizontal clipping here...
//     ...you have x, covers, and pix_count..
//     **************************************
//
//     unsigned char* dst = row + x;  // Calculate the start address of the row.
//                                    // In this case we assume a simple 
//                                    // grayscale image 1-byte per pixel.
//     do
//     {
//         *dst++ = *covers++;        // Hypotetical rendering. 
//     }
//     while(--num_pix);
//
//     ++span;
// } 
// while(--num_spans);  // num_spans cannot be 0, so this loop is quite safe
//------------------------------------------------------------------------
//
// The question is: why should we accumulate the whole scanline when we
// could render just separate spans when they're ready?
// That's because using the scanline is generally faster. When is consists
// of more than one span the conditions for the processor cash system
// are better, because switching between two different areas of memory
// (that can be very large) occurs less frequently.
//------------------------------------------------------------------------
 span_u8_ptr = ^span_u8;
 span_u8 = record
   x   ,
   len : int16;

   covers : int8u_ptr;

  end;

 scanline_u8 = object(scanline )
   m_min_x   : int;
   m_max_len : unsigned;
   m_last_x  ,
   m_y       : int;

   m_covers   : int8u_ptr;
   m_spans    ,
   m_cur_span : span_u8_ptr;

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

 scanline_u8_am = object(scanline_u8 )
   m_alpha_mask : alpha_mask_ptr;

   constructor Construct; overload;
   constructor Construct(am : alpha_mask_ptr ); overload;

   procedure finalize(y_ : int ); virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor scanline_u8.Construct;
begin
 m_min_x  :=0;
 m_max_len:=0;
 m_last_x :=$7FFFFFF0;

 m_covers  :=NIL;
 m_spans   :=NIL;
 m_cur_span:=NIL;

end;

{ DESTRUCT }
destructor scanline_u8.Destruct;
begin
 agg_freemem(pointer(m_spans ) ,m_max_len * sizeof(span_u8 ) );
 agg_freemem(pointer(m_covers ) ,m_max_len * sizeof(int8u ) );

end;

{ RESET }
procedure scanline_u8.reset;
var
 max_len : unsigned;

begin
 max_len:=max_x - min_x + 2;

 if max_len > m_max_len then
  begin
   agg_freemem(pointer(m_spans ) ,m_max_len * sizeof(span_u8 ) );
   agg_freemem(pointer(m_covers ) ,m_max_len * sizeof(int8u ) );

   agg_getmem(pointer(m_covers ) ,max_len * sizeof(int8u ) );
   agg_getmem(pointer(m_spans ) ,max_len * sizeof(span_u8 ) );

   m_max_len:=max_len;

  end;

 m_last_x  :=$7FFFFFF0;
 m_min_x   :=min_x;
 m_cur_span:=m_spans;

end;

{ RESET_SPANS }
procedure scanline_u8.reset_spans;
begin
 m_last_x  :=$7FFFFFF0;
 m_cur_span:=m_spans;

end;

{ FINALIZE }
procedure scanline_u8.finalize;
begin
 m_y:=y_;

end;

{ ADD_CELL }
procedure scanline_u8.add_cell;
begin
 dec(x ,m_min_x );

 int8u_ptr(ptrcomp(m_covers ) + x * sizeof(int8u ) )^:=int8u(cover );

 if x = m_last_x + 1 then
  inc(m_cur_span.len )

 else
  begin
   inc(ptrcomp(m_cur_span ) ,sizeof(span_u8 ) );

   m_cur_span.x  :=int16(x + m_min_x );
   m_cur_span.len:=1;

   m_cur_span.covers:=int8u_ptr(ptrcomp(m_covers ) + x * sizeof(int8u ) );

  end;

 m_last_x:=x;

end;

{ ADD_CELLS }
procedure scanline_u8.add_cells;
begin
 dec (x ,m_min_x );
 move(
  covers^ ,
  int8u_ptr(ptrcomp(m_covers ) + x )^ ,
  len * sizeof(int8u ) );

 if x = m_last_x + 1 then
  inc(m_cur_span.len ,int16(len ) )

 else
  begin
   inc(ptrcomp(m_cur_span ) ,sizeof(span_u8 ) );

   m_cur_span.x     :=int16(x + m_min_x );
   m_cur_span.len   :=int16(len );
   m_cur_span.covers:=int8u_ptr(ptrcomp(m_covers ) + x * sizeof(int8u ) );

  end;

 m_last_x:=x + len - 1;

end;

{ ADD_SPAN }
procedure scanline_u8.add_span;
begin
 dec(x ,m_min_x );

 fillchar(int8u_ptr(ptrcomp(m_covers ) + x * sizeof(int8u ) )^ ,len ,cover );

 if x = m_last_x + 1 then
  inc(m_cur_span.len ,int16(len ) )

 else
  begin
   inc(ptrcomp(m_cur_span ) ,sizeof(span_u8 ) );

   m_cur_span.x     :=int16(x + m_min_x );
   m_cur_span.len   :=int16(len );
   m_cur_span.covers:=int8u_ptr(ptrcomp(m_covers ) + x * sizeof(int8u ) );

  end;

 m_last_x:=x + len - 1; 

end;

{ Y }
function scanline_u8.y;
begin
 result:=m_y;

end;

{ NUM_SPANS }
function scanline_u8.num_spans;
begin
 result:=(ptrcomp(m_cur_span ) - ptrcomp(m_spans ) ) div sizeof(span_u8 );

end;

{ BEGIN_ }
function scanline_u8.begin_;
begin
 result:=span_u8_ptr(ptrcomp(m_spans ) + sizeof(span_u8 ) );

end;

{ SZ_OF_SPAN }
function scanline_u8.sz_of_span;
begin
 result:=sizeof(span_u8 );

end;

{ CONSTRUCT }
constructor scanline_u8_am.Construct;
begin
 inherited Construct;

 m_alpha_mask:=NIL;

end;

{ CONSTRUCT }
constructor scanline_u8_am.Construct(am : alpha_mask_ptr );
begin
 inherited Construct;

 m_alpha_mask:=am;

end;

{ FINALIZE }
procedure scanline_u8_am.finalize;
var
 span : span_u8_ptr;

 ss ,count : unsigned;

begin
 inherited finalize(y_ );

 if m_alpha_mask <> NIL then
  begin
   span :=begin_;
   ss   :=sz_of_span;
   count:=num_spans;

   repeat
    m_alpha_mask.combine_hspan(span.x ,y ,span.covers ,span.len );

    inc(ptrcomp(span ) ,ss );
    dec(count );

   until count = 0;

  end;

end;

END.

