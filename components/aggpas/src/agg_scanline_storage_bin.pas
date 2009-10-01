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
// Adaptation for 32-bit screen coordinates has been sponsored by 
// Liberty Technology Systems, Inc., visit http://lib-sys.com
//
// Liberty Technology Systems, Inc. is the provider of
// PostScript and PDF technology for software developers.
//
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 14.02.2006-Milano: Unit port establishment
//
{ agg_scanline_storage_bin.pas }
unit
 agg_scanline_storage_bin ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
 agg_basics ,
 agg_array ,
 agg_scanline ,
 agg_renderer_scanline ,
 agg_render_scanlines ,
 agg_rasterizer_scanline_aa ;

{ TYPES DEFINITION }
type
//-----------------------------------------------scanline_storage_bin
 span_data_ptr = ^span_data;
 span_data = record
   x ,len : int32;

  end;

 scanline_data_ptr = ^scanline_data;
 scanline_data = record
   y : int;

   num_spans ,start_span : unsigned;

  end;

 scanline_storage_bin_ptr = ^scanline_storage_bin;

 embedded_scanline_b_ptr = ^embedded_scanline_b;

 const_iterator_b_ptr = ^const_iterator_b;
 const_iterator_b = object(span_obj )
   m_storage  : scanline_storage_bin_ptr;
   m_span_idx : unsigned;
   m_span     : span_data;

   constructor Construct(sl : embedded_scanline_b_ptr );

   function  x   : int; virtual;
   function  len : int; virtual;

   procedure inc_operator; virtual;

  end;

 embedded_scanline_b = object(scanline )
   m_storage  : scanline_storage_bin_ptr;
   m_scanline : scanline_data;

   m_scanline_idx : unsigned;

   m_result : const_iterator_b;

   constructor Construct(storage : scanline_storage_bin_ptr );

   procedure reset(min_x ,max_x : int ); virtual;

   function  y : int; virtual;
   function  num_spans : unsigned; virtual;
   function  begin_ : pointer; virtual;

   function  sz_of_span : unsigned; virtual;
   function  is_plain_span : boolean; virtual;
   function  is_embedded : boolean; virtual;

   procedure setup(scanline_idx : unsigned ); virtual;

  end;

 scanline_storage_bin = object(renderer_scanline )
   m_spans     ,
   m_scanlines : pod_deque;

   m_fake_span     : span_data;
   m_fake_scanline : scanline_data;

   m_min_x ,
   m_min_y ,
   m_max_x ,
   m_max_y : int;

   m_cur_scanline : unsigned;

   constructor Construct;
   destructor  Destruct;

  // Renderer Interface
   procedure prepare(u : unsigned ); virtual;
   procedure render (sl : scanline_ptr ); virtual;

  // Iterate scanlines interface
   function  _min_x : int; virtual;
   function  _min_y : int; virtual;
   function  _max_x : int; virtual;
   function  _max_y : int; virtual;

   function  rewind_scanlines : boolean; virtual;
   function  sweep_scanline(sl : scanline_ptr ) : boolean; virtual;

  // Specialization for embedded_scanline
   function  sweep_scanline_em(sl : scanline_ptr ) : boolean; virtual;

   function  byte_size : unsigned;
   procedure write_int32(dst : int8u_ptr; val : int32 );
   procedure serialize  (data : int8u_ptr );

   function  scanline_by_index(i : unsigned ) : scanline_data_ptr;
   function  span_by_index    (i : unsigned ) : span_data_ptr;

  end;

//---------------------------------------serialized_scanlines_adaptor_bin
 span_a_ptr = ^span_a;
 span_a = record
   x ,len : int32;

  end;

 embedded_scanline_a_ptr = ^embedded_scanline_a;

 const_iterator_a_ptr = ^const_iterator_a;
 const_iterator_a = object(span_obj )
   m_ptr  : int8u_ptr;
   m_span : span_a;
   m_dx   : int;

   constructor Construct(sl : embedded_scanline_a_ptr );

   function  x   : int; virtual;
   function  len : int; virtual;

   procedure inc_operator; virtual;

   function  read_int32 : int;

  end;

 embedded_scanline_a = object(scanline )
   m_ptr : int8u_ptr;
   m_y   : int;

   m_num_spans : unsigned;

   m_dx : int;

   m_result : const_iterator_a;

   constructor Construct;

   procedure reset(min_x ,max_x : int ); virtual;

   function  y : int; virtual;
   function  num_spans : unsigned; virtual;
   function  begin_ : pointer; virtual;

   function  sz_of_span : unsigned; virtual;
   function  is_plain_span : boolean; virtual;
   function  is_embedded : boolean; virtual;

   function  read_int32 : int;
   procedure init(ptr : int8u_ptr; dx ,dy : int ); virtual;

  end;

 serialized_scanlines_adaptor_bin = object(rasterizer_scanline )
   m_data ,
   m_end  ,
   m_ptr  : int8u_ptr;

   m_dx ,
   m_dy ,

   m_min_x ,
   m_min_y ,
   m_max_x ,
   m_max_y : int;

   constructor Construct; overload;
   constructor Construct(data : int8u_ptr; size : unsigned; dx ,dy : double ); overload;

   procedure init(data : int8u_ptr; size : unsigned; dx ,dy : double );
   function  read_int32 : int;

  // Iterate scanlines interface
   function  rewind_scanlines : boolean; virtual;

   function  _min_x : int; virtual;
   function  _min_y : int; virtual;
   function  _max_x : int; virtual;
   function  _max_y : int; virtual;

   function  sweep_scanline(sl : scanline_ptr ) : boolean; virtual;

  // Specialization for embedded_scanline
   function  sweep_scanline_em(sl : scanline_ptr ) : boolean; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor const_iterator_b.Construct;
begin
 m_storage :=sl.m_storage;
 m_span_idx:=sl.m_scanline.start_span;

 m_span:=m_storage.span_by_index(m_span_idx )^;

end;

{ X }
function const_iterator_b.x;
begin
 result:=m_span.x;

end;

{ LEN }
function const_iterator_b.len;
begin
 result:=m_span.len;

end;

{ INC_OPERATOR }
procedure const_iterator_b.inc_operator;
begin
 inc(m_span_idx );

 m_span:=m_storage.span_by_index(m_span_idx )^;

end;

{ CONSTRUCT }
constructor embedded_scanline_b.Construct;
begin
 m_storage:=storage;

 setup(0 );

end;

{ RESET }
procedure embedded_scanline_b.reset;
begin
end;

{ Y }
function embedded_scanline_b.y;
begin
 result:=m_scanline.y;

end;

{ NUM_SPANS }
function embedded_scanline_b.num_spans;
begin
 result:=m_scanline.num_spans;

end;

{ BEGIN_ }
function embedded_scanline_b.begin_;
begin
 m_result.Construct(@self );

 result:=@m_result;

end;

{ SZ_OF_SPAN }
function embedded_scanline_b.sz_of_span;
begin
 result:=sizeof(span_data );

end;

{ IS_PLAIN_SPAN }
function embedded_scanline_b.is_plain_span;
begin
 result:=false;

end;

{ IS_EMBEDDED }
function embedded_scanline_b.is_embedded;
begin
 result:=true;

end;

{ SETUP }
procedure embedded_scanline_b.setup;
begin
 m_scanline_idx:=scanline_idx;
 m_scanline    :=m_storage.scanline_by_index(m_scanline_idx )^;

end;

{ CONSTRUCT }
constructor scanline_storage_bin.Construct;
begin
 m_spans.Construct    (256 - 2 ,sizeof(span_data ) ,10 ); // Block increment size
 m_scanlines.Construct(sizeof(scanline_data ) ,8 );

 m_min_x:=$7FFFFFFF;
 m_min_y:=$7FFFFFFF;
 m_max_x:=-$7FFFFFFF;
 m_max_y:=-$7FFFFFFF;

 m_cur_scanline   :=0;
 m_fake_scanline.y:=0;

 m_fake_scanline.num_spans :=0;
 m_fake_scanline.start_span:=0;

 m_fake_span.x  :=0;
 m_fake_span.len:=0;

end;

{ DESTRUCT }
destructor scanline_storage_bin.Destruct;
begin
 m_spans.Destruct;
 m_scanlines.Destruct;

end;

{ PREPARE }
procedure scanline_storage_bin.prepare;
begin
 m_scanlines.remove_all;
 m_spans.remove_all;

 m_min_x:=$7FFFFFFF;
 m_min_y:=$7FFFFFFF;
 m_max_x:=-$7FFFFFFF;
 m_max_y:=-$7FFFFFFF;

 m_cur_scanline:=0;

end;

{ RENDER }
procedure scanline_storage_bin.render;
var
 y ,x1 ,x2 : int;

 sl_this   : scanline_data;
 num_spans : unsigned;

 span_pl  : span_ptr;
 span_obj : span_obj_ptr;

 ss : unsigned;
 sp : span_data;

begin
 y:=sl.y;

 if y < m_min_y then
  m_min_y:=y;

 if y > m_max_y then
  m_max_y:=y;

 sl_this.y         :=y;
 sl_this.num_spans :=sl.num_spans;
 sl_this.start_span:=m_spans.size;

 num_spans:=sl_this.num_spans;

 span_pl :=NIL;
 span_obj:=NIL;

 if sl.is_plain_span then
  begin
   span_pl:=sl.begin_;

   ss:=sl.sz_of_span;

  end
 else
  span_obj:=sl.begin_;

 repeat
  if span_pl <> NIL then
   begin
    sp.x  :=span_pl.x;
    sp.len:=span_pl.len;

   end
  else
   begin
    sp.x  :=span_obj.x;
    sp.len:=span_obj.len;

   end;

  m_spans.add(@sp ); 

  x1:=sp.x;
  x2:=sp.x + sp.len - 1;

  if x1 < m_min_x then
   m_min_x:=x1;

  if x2 > m_max_x then
   m_max_x:=x2;

  dec(num_spans );

  if num_spans = 0 then
   break;

  if span_pl <> NIL then
   inc(ptrcomp(span_pl ) ,ss )
  else
   span_obj.inc_operator; 

 until false;

 m_scanlines.add(@sl_this );

end;

{ _MIN_X }
function scanline_storage_bin._min_x;
begin
 result:=m_min_x;

end;

{ _MIN_Y }
function scanline_storage_bin._min_y;
begin
 result:=m_min_y;

end;

{ _MAX_X }
function scanline_storage_bin._max_x;
begin
 result:=m_max_x;

end;

{ _MAX_Y }
function scanline_storage_bin._max_y;
begin
 result:=m_max_y;

end;

{ REWIND_SCANLINES }
function scanline_storage_bin.rewind_scanlines;
begin
 m_cur_scanline:=0;

 result:=m_scanlines.size > 0;

end;

{ SWEEP_SCANLINE }
function scanline_storage_bin.sweep_scanline;
var
 sl_this : scanline_data_ptr;

 num_spans ,span_idx : unsigned;

 sp : span_data_ptr;

begin
 sl.reset_spans;

 repeat
  if m_cur_scanline >= m_scanlines.size then
   begin
    result:=false;

    exit;

   end;

  sl_this:=m_scanlines.array_operator(m_cur_scanline );

  num_spans:=sl_this.num_spans;
  span_idx :=sl_this.start_span;

  repeat
   sp:=m_spans.array_operator(span_idx );

   inc(span_idx );

   sl.add_span(sp.x ,sp.len ,cover_full );

   dec(num_spans );

  until num_spans = 0;

  inc(m_cur_scanline );

  if sl.num_spans <> 0 then
   begin
    sl.finalize(sl_this.y );

    break;

   end;

 until false;

 result:=true;

end;

{ SWEEP_SCANLINE }
function scanline_storage_bin.sweep_scanline_em;
begin
 repeat
  if m_cur_scanline >= m_scanlines.size then
   begin
    result:=false;

    exit;

   end;

  sl.setup(m_cur_scanline );

  inc(m_cur_scanline );

 until sl.num_spans <> 0;

 result:=true;

end;

{ BYTE_SIZE }
function scanline_storage_bin.byte_size;
var
 i ,size : unsigned;

begin
 size:=sizeof(int32 ) * 4; // min_x, min_y, max_x, max_y

 i:=0;

 while i < m_scanlines.size do
  begin
   size:=
    size +
    sizeof(int32 ) * 2 + // Y, num_spans
    unsigned(scanline_data_ptr(m_scanlines.array_operator(i ) ).num_spans ) * sizeof(int32 ) * 2; // X, span_len

   inc(i );

  end;

 result:=size;

end;

{ WRITE_INT32 }
procedure scanline_storage_bin.write_int32;
begin
 int8u_ptr(ptrcomp(dst ) + 0 * sizeof(int8u ) )^:=int32_int8u(val )._0;
 int8u_ptr(ptrcomp(dst ) + 1 * sizeof(int8u ) )^:=int32_int8u(val )._1;
 int8u_ptr(ptrcomp(dst ) + 2 * sizeof(int8u ) )^:=int32_int8u(val )._2;
 int8u_ptr(ptrcomp(dst ) + 3 * sizeof(int8u ) )^:=int32_int8u(val )._3;

end;

{ SERIALIZE }
procedure scanline_storage_bin.serialize;
var
 i ,num_spans ,span_idx : unsigned;

 sl_this : scanline_data_ptr;

 sp : span_data_ptr;

begin
 write_int32(data ,_min_x ); // min_x
 inc(ptrcomp(data ) ,sizeof(int32 ) );

 write_int32(data ,_min_y ); // min_y
 inc(ptrcomp(data ) ,sizeof(int32 ) );

 write_int32(data ,_max_x ); // max_x
 inc(ptrcomp(data ) ,sizeof(int32 ) );

 write_int32(data ,_max_y ); // max_y
 inc(ptrcomp(data ) ,sizeof(int32 ) );

 i:=0;

 while i < m_scanlines.size do
  begin
   sl_this:=m_scanlines.array_operator(i );

   write_int32(data ,sl_this.y );            // Y
   inc(ptrcomp(data ) ,sizeof(int32 ) );

   write_int32(data ,sl_this.num_spans );    // num_spans
   inc(ptrcomp(data ) ,sizeof(int32 ) );

   num_spans:=sl_this.num_spans;
   span_idx :=sl_this.start_span;

   repeat
    sp:=m_spans.array_operator(span_idx );

    inc(span_idx );

    write_int32(data ,sp.x );                // X
    inc(ptrcomp(data ) ,sizeof(int32 ) );

    write_int32(data ,sp.len );              // len
    inc(ptrcomp(data ) ,sizeof(int32 ) );

    dec(num_spans );

   until num_spans = 0;

   inc(i );

  end;

end;

{ SCANLINE_BY_INDEX }
function scanline_storage_bin.scanline_by_index;
begin
 if i < m_scanlines.size then
  result:=m_scanlines.array_operator(i )
 else
  result:=@m_fake_scanline;

end;

{ SPAN_BY_INDEX }
function scanline_storage_bin.span_by_index;
begin
 if i < m_spans.size then
  result:=m_spans.array_operator(i )
 else
  result:=@m_fake_span;

end;

{ CONSTRUCT }
constructor const_iterator_a.Construct;
begin
 m_ptr:=sl.m_ptr;
 m_dx :=sl.m_dx;

 m_span.x  :=read_int32 + m_dx;
 m_span.len:=read_int32;

end;

{ X }
function const_iterator_a.x;
begin
 result:=m_span.x;

end;

{ LEN }
function const_iterator_a.len;
begin
 result:=m_span.len;

end;

{ INC_OPERATOR }
procedure const_iterator_a.inc_operator;
begin
 m_span.x  :=read_int32 + m_dx;
 m_span.len:=read_int32;

end;

{ READ_INT32 }
function const_iterator_a.read_int32;
begin
 int32_int8u(result )._0:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._1:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._2:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._3:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );

end;

{ CONSTRUCT }
constructor embedded_scanline_a.Construct;
begin
 m_ptr:=NIL;
 m_y  :=0;

 m_num_spans:=0;

end;

{ RESET }
procedure embedded_scanline_a.reset;
begin
end;

{ Y }
function embedded_scanline_a.y;
begin
 result:=m_y;

end;

{ NUM_SPANS }
function embedded_scanline_a.num_spans;
begin
 result:=m_num_spans;

end;

{ BEGIN_ }
function embedded_scanline_a.begin_;
begin
 m_result.Construct(@self );

 result:=@m_result;

end;

{ SZ_OF_SPAN }
function embedded_scanline_a.sz_of_span;
begin
 result:=sizeof(span_a );

end;

{ IS_PLAIN_SPAN }
function embedded_scanline_a.is_plain_span;
begin
 result:=false;

end;

{ IS_EMBEDDED }
function embedded_scanline_a.is_embedded;
begin
 result:=true;

end;

{ READ_INT32 }
function embedded_scanline_a.read_int32;
begin
 int32_int8u(result )._0:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._1:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._2:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._3:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );

end;

{ INIT }
procedure embedded_scanline_a.init;
begin
 m_ptr      :=ptr;
 m_y        :=read_int32 + dy;
 m_num_spans:=unsigned(read_int32 );
 m_dx       :=dx;

end;

{ CONSTRUCT }
constructor serialized_scanlines_adaptor_bin.Construct;
begin
 m_data:=NIL;
 m_end :=NIL;
 m_ptr :=NIL;

 m_dx:=0;
 m_dy:=0;

 m_min_x:=$7FFFFFFF;
 m_min_y:=$7FFFFFFF;
 m_max_x:=-$7FFFFFFF;
 m_max_y:=-$7FFFFFFF;

end;

{ CONSTRUCT }
constructor serialized_scanlines_adaptor_bin.Construct(data : int8u_ptr; size : unsigned; dx ,dy : double );
begin
 m_data:=data;
 m_end :=int8u_ptr(ptrcomp(data ) + size );
 m_ptr :=data;

 m_dx:=trunc(dx + 0.5 );
 m_dy:=trunc(dy + 0.5 );

 m_min_x:=$7FFFFFFF;
 m_min_y:=$7FFFFFFF;
 m_max_x:=-$7FFFFFFF;
 m_max_y:=-$7FFFFFFF;

end;

{ INIT }
procedure serialized_scanlines_adaptor_bin.init;
begin
 m_data:=data;
 m_end :=int8u_ptr(ptrcomp(data ) + size );
 m_ptr :=data;

 m_dx:=trunc(dx + 0.5 );
 m_dy:=trunc(dy + 0.5 );

 m_min_x:=$7FFFFFFF;
 m_min_y:=$7FFFFFFF;
 m_max_x:=-$7FFFFFFF;
 m_max_y:=-$7FFFFFFF;

end;

{ READ_INT32 }
function serialized_scanlines_adaptor_bin.read_int32;
begin
 int32_int8u(result )._0:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._1:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._2:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._3:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );

end;

{ REWIND_SCANLINES }
function serialized_scanlines_adaptor_bin.rewind_scanlines;
begin
 m_ptr:=m_data;

 if ptrcomp(m_ptr ) < ptrcomp(m_end ) then
  begin
   m_min_x:=read_int32 + m_dx;
   m_min_y:=read_int32 + m_dy;
   m_max_x:=read_int32 + m_dx;
   m_max_y:=read_int32 + m_dy;

   result:=true;

  end
 else
  result:=false;

end;

{ _MIN_X }
function serialized_scanlines_adaptor_bin._min_x;
begin
 result:=m_min_x;

end;

{ _MIN_Y }
function serialized_scanlines_adaptor_bin._min_y;
begin
 result:=m_min_y;

end;

{ _MAX_X }
function serialized_scanlines_adaptor_bin._max_x;
begin
 result:=m_max_x;

end;

{ _MAX_Y }
function serialized_scanlines_adaptor_bin._max_y;
begin
 result:=m_max_y;

end;

{ SWEEP_SCANLINE }
function serialized_scanlines_adaptor_bin.sweep_scanline;
var
 y ,x ,len : int;

 num_spans : unsigned;

begin
 sl.reset_spans;

 repeat
  if ptrcomp(m_ptr ) >= ptrcomp(m_end ) then
   begin
    result:=false;

    exit;

   end;

  y        :=read_int32 + m_dy;
  num_spans:=read_int32;

  repeat
   x  :=read_int32 + m_dx;
   len:=read_int32;

   if len < 0 then
    len:=-len;

   sl.add_span(x ,unsigned(len ) ,cover_full );

   inc(num_spans );

  until num_spans = 0;

  if sl.num_spans <> 0 then
   begin
    sl.finalize(y );

    break;

   end;

 until false;

 result:=true;

end;

{ SWEEP_SCANLINE }
function serialized_scanlines_adaptor_bin.sweep_scanline_em;
var
 num_spans : int;

begin
 repeat
  if ptrcomp(m_ptr ) >= ptrcomp(m_end ) then
   begin
    result:=false;

    exit;

   end;

  sl.init(m_ptr ,m_dx ,m_dy );

 // Jump to the next scanline
  read_int32;                // Y

  num_spans:=read_int32;    // num_spans

  inc(ptrcomp(m_ptr ) ,num_spans * sizeof(int32 ) * 2 );

 until sl.num_spans <> 0;

 result:=true;

end;

END.

