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
// 23.06.2006-Milano: ptrcomp adjustments
// 15.02.2006-Milano: Unit port establishment
//
{ agg_scanline_storage_aa.pas }
unit
 agg_scanline_storage_aa ;

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
//----------------------------------------------scanline_cell_storage
 extra_span_ptr = ^extra_span;
 extra_span = record
   len : unsigned;
   ptr : pointer;

  end;

 scanline_cell_storage_ptr = ^scanline_cell_storage;
 scanline_cell_storage = object
   m_cells         ,
   m_extra_storage : pod_deque;

   constructor Construct(entry_sz : unsigned ); overload;
   constructor Construct(v : scanline_cell_storage_ptr ); overload;
   destructor  Destruct;

   procedure remove_all;
   function  add_cells(cells : pointer; num_cells : unsigned ) : int;

   function  assign_operator(v : scanline_cell_storage_ptr ) : scanline_cell_storage_ptr;
   function  array_operator (idx : int ) : pointer;

   procedure copy_extra_storage(v : scanline_cell_storage_ptr );

  end;

//-----------------------------------------------scanline_storage_aa
 span_data_ss_ptr = ^span_data_ss;
 span_data_ss = record
   x         ,
   len       : int32; // If negative, it's a solid span, covers is valid
   covers_id : int;   // The index of the cells in the scanline_cell_storage

  end;

 scanline_data_ss_ptr = ^scanline_data_ss; 
 scanline_data_ss = record
   y          : int;
   num_spans  ,
   start_span : unsigned;

  end;

 span_ss_ptr = ^span_ss;
 span_ss = record
   x   ,
   len : int32; // If negative, it's a solid span, covers is valid

   covers : pointer;

  end;

 scanline_storage_aa_ptr = ^scanline_storage_aa;

 embedded_scanline_ss_ptr = ^embedded_scanline_ss;

 const_iterator_ss = object(span_obj )
   m_storage  : scanline_storage_aa_ptr;
   m_span_idx : unsigned;
   m_span     : span_ss;

   constructor Construct(sl : embedded_scanline_ss_ptr );

   function  x      : int; virtual;
   function  len    : int; virtual;
   function  covers : int8u_ptr; virtual;

   procedure inc_operator; virtual;
   procedure init_span;

  end;

 embedded_scanline_ss = object(scanline )
   m_storage      : scanline_storage_aa_ptr;
   m_scanline     : scanline_data_ss;
   m_scanline_idx : unsigned;

   m_result : const_iterator_ss;

   constructor Construct(storage : scanline_storage_aa_ptr );

   procedure reset(min_x ,max_x : int ); virtual;

   function  y : int; virtual;
   function  num_spans : unsigned; virtual;
   function  begin_ : pointer; virtual;

   function  sz_of_span : unsigned; virtual;
   function  is_plain_span : boolean; virtual;
   function  is_embedded : boolean; virtual;

   procedure setup(scanline_idx : unsigned ); virtual;

  end;

 scanline_storage_aa = object(renderer_scanline )
   m_covers    : scanline_cell_storage;
   m_spans     ,
   m_scanlines : pod_deque;

   m_fake_span     : span_data_ss;
   m_fake_scanline : scanline_data_ss;

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

   function  byte_size : unsigned; virtual;
   procedure write_int32(dst : int8u_ptr; val : int32 );
   procedure serialize  (data : int8u_ptr ); virtual;

   function  scanline_by_index(i : unsigned ) : scanline_data_ss_ptr;
   function  span_by_index    (i : unsigned ) : span_data_ss_ptr;
   function  covers_by_index  (i : int ) : pointer;

  end;

 scanline_storage_aa8 = object(scanline_storage_aa )
  end;

 scanline_storage_aa16 = object(scanline_storage_aa )
   constructor Construct;

   function  sweep_scanline(sl : scanline_ptr ) : boolean; virtual;

   function  byte_size : unsigned; virtual;
   procedure serialize(data : int8u_ptr ); virtual;

  end;

 scanline_storage_aa32 = object(scanline_storage_aa )
   constructor Construct;

   function  sweep_scanline(sl : scanline_ptr ) : boolean; virtual;

   function  byte_size : unsigned; virtual;
   procedure serialize(data : int8u_ptr ); virtual;

  end;

//------------------------------------------serialized_scanlines_adaptor_aa
 embedded_scanline_sa_ptr = ^embedded_scanline_sa;

 const_iterator_sa = object(span_obj )
   m_ptr  : int8u_ptr;
   m_span : span_ss;

   m_dx : int;
   m_sz : unsigned;

   constructor Construct(sl : embedded_scanline_sa_ptr; sz : unsigned );

   function  x      : int; virtual;
   function  len    : int; virtual;
   function  covers : int8u_ptr; virtual;

   procedure inc_operator; virtual;
   procedure init_span;

   function  read_int32 : int;

  end;

 embedded_scanline_sa = object(scanline )
   m_ptr : int8u_ptr;
   m_y   : int;

   m_num_spans : unsigned;

   m_dx : int;
   m_sz : unsigned;

   m_result : const_iterator_sa;

   constructor Construct(sz : unsigned );

   procedure reset(min_x ,max_x : int ); virtual;

   function  y : int; virtual;
   function  num_spans : unsigned; virtual;
   function  begin_ : pointer; virtual;

   function  sz_of_span : unsigned; virtual;
   function  is_plain_span : boolean; virtual;
   function  is_embedded : boolean; virtual;

   procedure init(ptr : int8u_ptr; dx ,dy : int ); virtual;

   function  read_int32 : int;

  end;

 serialized_scanlines_adaptor_aa = object(rasterizer_scanline )
   m_data ,
   m_end  ,
   m_ptr  : int8u_ptr;

   m_dx ,
   m_dy ,

   m_min_x ,
   m_min_y ,
   m_max_x ,
   m_max_y : int;

   m_sz : unsigned;

   constructor Construct(sz : unsigned ); overload;
   constructor Construct(sz : unsigned; data : int8u_ptr; size : unsigned; dx ,dy : double ); overload;

   procedure init(data : int8u_ptr; size : unsigned; dx ,dy : double );

   function  read_int32 : int;
   function  read_int32u : unsigned;

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

 serialized_scanlines_adaptor_aa8 = object(serialized_scanlines_adaptor_aa )
   constructor Construct; overload;
   constructor Construct(data : int8u_ptr; size : unsigned; dx ,dy : double ); overload;

  end;

 serialized_scanlines_adaptor_aa16 = object(serialized_scanlines_adaptor_aa )
   constructor Construct; overload;
   constructor Construct(data : int8u_ptr; size : unsigned; dx ,dy : double ); overload;

  end;

 serialized_scanlines_adaptor_aa32 = object(serialized_scanlines_adaptor_aa )
   constructor Construct; overload;
   constructor Construct(data : int8u_ptr; size : unsigned; dx ,dy : double ); overload;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor scanline_cell_storage.Construct(entry_sz : unsigned );
begin
 m_cells.Construct        (128 - 2 ,entry_sz ,12 );
 m_extra_storage.Construct(sizeof(extra_span ) ,6 );

end;

{ CONSTRUCT }
constructor scanline_cell_storage.Construct(v : scanline_cell_storage_ptr );
begin
 m_cells.Construct        (v.m_cells.m_entry_sz );
 m_extra_storage.Construct(sizeof(extra_span ) ,6 );

 assign_operator   (v );
 copy_extra_storage(v );

end;

{ DESTRUCT }
destructor scanline_cell_storage.Destruct;
begin
 remove_all;

 m_cells.Destruct;
 m_extra_storage.Destruct;

end;

{ REMOVE_ALL }
procedure scanline_cell_storage.remove_all;
var
 i : int;
 s : extra_span_ptr;

begin
 i:=m_extra_storage.size; dec(i );

 while i >= 0 do
  begin
   s:=m_extra_storage.array_operator(i );

   agg_freemem(s.ptr ,s.len * m_cells.m_entry_sz );

   dec(i );

  end;

 m_extra_storage.remove_all;
 m_cells.remove_all;

end;

{ ADD_CELLS }
function scanline_cell_storage.add_cells;
var
 idx : int;
 ptr : pointer;

 s : extra_span;

begin
 idx:=m_cells.allocate_continuous_block(num_cells );

 if idx >= 0 then
  begin
   ptr:=m_cells.array_operator(idx );

   move(cells^ ,ptr^ ,m_cells.m_entry_sz * num_cells );

   result:=idx;

   exit;

  end;

 s.len:=num_cells;

 agg_getmem(s.ptr ,s.len * m_cells.m_entry_sz );

 move(cells^ ,s.ptr^ ,s.len * m_cells.m_entry_sz );

 m_extra_storage.add(@s );

 result:=-int(m_extra_storage.size );

end;

{ ASSIGN_OPERATOR }
function scanline_cell_storage.assign_operator;
begin
 remove_all;

 m_cells.assign_operator(@v.m_cells );
 copy_extra_storage     (v );

 result:=@self;

end;

{ ARRAY_OPERATOR }
function scanline_cell_storage.array_operator;
var
 i : unsigned;

begin
 if idx >= 0 then
  begin
   if idx >= m_cells.size then
    begin
     result:=NIL;

     exit;

    end;

   result:=m_cells.array_operator(idx );

   exit;

  end;

 i:=unsigned(-idx - 1 );

 if i >= m_extra_storage.size then
  begin
   result:=0;

   exit;

  end;

 result:=extra_span_ptr(m_extra_storage.array_operator(i ) ).ptr;

end;

{ COPY_EXTRA_STORAGE }
procedure scanline_cell_storage.copy_extra_storage;
var
 i : unsigned;

 src : extra_span_ptr;
 dst : extra_span;

begin
 i:=0;

 while i < v.m_extra_storage.size do
  begin
   src:=v.m_extra_storage.array_operator(i );

   dst.len:=src.len;

   agg_getmem(dst.ptr ,dst.len * v.m_cells.m_entry_sz );

   move(src.ptr^ ,dst.ptr^ ,dst.len * v.m_cells.m_entry_sz );

   m_extra_storage.add(@dst );

   inc(i );

  end;

end;

{ CONSTRUCT }
constructor const_iterator_ss.Construct;
begin
 m_storage :=sl.m_storage;
 m_span_idx:=sl.m_scanline.start_span;

 init_span;

end;

{ X }
function const_iterator_ss.x;
begin
 result:=m_span.x;

end;

{ LEN }
function const_iterator_ss.len;
begin
 result:=m_span.len;

end;

{ COVERS }
function const_iterator_ss.covers;
begin
 result:=m_span.covers;

end;

{ INC_OPERATOR }
procedure const_iterator_ss.inc_operator;
begin
 inc(m_span_idx );

 init_span;

end;

{ INIT_SPAN }
procedure const_iterator_ss.init_span;
var
 s : span_data_ss_ptr;

begin
 s:=m_storage.span_by_index(m_span_idx );

 m_span.x     :=s.x;
 m_span.len   :=s.len;
 m_span.covers:=m_storage.covers_by_index(s.covers_id );
 
end;

{ CONSTRUCT }
constructor embedded_scanline_ss.Construct;
begin
 m_storage:=storage;

 setup(0 );

end;

{ RESET }
procedure embedded_scanline_ss.reset;
begin
end;

{ Y }
function embedded_scanline_ss.y;
begin
 result:=m_scanline.y;

end;

{ NUM_SPANS }
function embedded_scanline_ss.num_spans;
begin
 result:=m_scanline.num_spans;

end;

{ BEGIN_ }
function embedded_scanline_ss.begin_;
begin
 m_result.Construct(@self );

 result:=@m_result;

end;

{ SZ_OF_SPAN }
function embedded_scanline_ss.sz_of_span;
begin
 result:=sizeof(span_ss );

end;

{ IS_PLAIN_SPAN }
function embedded_scanline_ss.is_plain_span;
begin
 result:=false;

end;

{ IS_EMBEDDED }
function embedded_scanline_ss.is_embedded;
begin
 result:=true;

end;

{ SETUP }
procedure embedded_scanline_ss.setup;
begin
 m_scanline_idx:=scanline_idx;
 m_scanline    :=m_storage.scanline_by_index(m_scanline_idx )^;

end;

{ CONSTRUCT }
constructor scanline_storage_aa.Construct;
begin
 m_covers.Construct   (sizeof(int8u ) );
 m_spans.Construct    (256 - 2 ,sizeof(span_data_ss ) ,10 ); // Block increment size
 m_scanlines.Construct(sizeof(scanline_data_ss ) ,8 );

 m_min_x:=$7FFFFFFF;
 m_min_y:=$7FFFFFFF;
 m_max_x:=-$7FFFFFFF;
 m_max_y:=-$7FFFFFFF;

 m_cur_scanline:=0;

 m_fake_scanline.y         :=0;
 m_fake_scanline.num_spans :=0;
 m_fake_scanline.start_span:=0;

 m_fake_span.x        :=0;
 m_fake_span.len      :=0;
 m_fake_span.covers_id:=0;

end;

{ DESTRUCT }
destructor scanline_storage_aa.Destruct;
begin
 m_covers.Destruct;
 m_spans.Destruct;
 m_scanlines.Destruct;

end;

{ PREPARE }
procedure scanline_storage_aa.prepare;
begin
 m_covers.remove_all;
 m_scanlines.remove_all;
 m_spans.remove_all;

 m_min_x:=$7FFFFFFF;
 m_min_y:=$7FFFFFFF;
 m_max_x:=-$7FFFFFFF;
 m_max_y:=-$7FFFFFFF;

 m_cur_scanline:=0;

end;

{ RENDER }
procedure scanline_storage_aa.render;
var
 sl_this : scanline_data_ss;

 y ,x1 ,x2 ,len : int;

 num_spans ,ss : unsigned;

 span_pl  : span_ptr;
 span_obj : span_obj_ptr;

 sp : span_data_ss;

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

  len:=Abs(sp.len );

  if span_pl <> NIL then
   sp.covers_id:=m_covers.add_cells(span_pl.covers ,unsigned(len ) )
  else
   sp.covers_id:=m_covers.add_cells(span_obj.covers ,unsigned(len ) );

  m_spans.add(@sp );

  x1:=sp.x;
  x2:=sp.x + len - 1;

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
function scanline_storage_aa._min_x;
begin
 result:=m_min_x;

end;

{ _MIN_Y }
function scanline_storage_aa._min_y;
begin
 result:=m_min_y;

end;

{ _MAX_X }
function scanline_storage_aa._max_x;
begin
 result:=m_max_x;

end;

{ _MAX_Y }
function scanline_storage_aa._max_y;
begin
 result:=m_max_y;

end;

{ REWIND_SCANLINES }
function scanline_storage_aa.rewind_scanlines;
begin
 m_cur_scanline:=0;

 result:=m_scanlines.size > 0;

end;

{ SWEEP_SCANLINE }
function scanline_storage_aa.sweep_scanline;
var
 sl_this : scanline_data_ss_ptr;

 num_spans ,span_idx : unsigned;

 sp : span_data_ss_ptr;

 covers : int8u_ptr;

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

   covers:=covers_by_index(sp.covers_id );

   if sp.len < 0 then
    sl.add_span(sp.x ,unsigned(-sp.len ) ,covers^ )
   else
    sl.add_cells(sp.x ,sp.len ,covers );

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
function scanline_storage_aa.sweep_scanline_em;
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
function scanline_storage_aa.byte_size;
var
 i ,size ,num_spans ,span_idx : unsigned;
 
 sl_this : scanline_data_ss_ptr;

 sp : span_data_ss_ptr;

begin
 size:=sizeof(int32 ) * 4; // min_x, min_y, max_x, max_y

 i:=0;

 while i < m_scanlines.size do
  begin
   inc(size ,sizeof(int32 ) * 3 ); // scanline size in bytes, Y, num_spans

   sl_this:=m_scanlines.array_operator(i );

   num_spans:=sl_this.num_spans;
   span_idx :=sl_this.start_span;

   repeat
    sp:=m_spans.array_operator(span_idx );

    inc(span_idx );
    inc(size ,sizeof(int32 ) * 2 );                  // X, span_len

    if sp.len < 0 then
     inc(size ,sizeof(int8u ) )                      // cover
    else
     inc(size ,sizeof(int8u ) * unsigned(sp.len ) ); // covers

    dec(num_spans );

   until num_spans = 0;

   inc(i );

  end;

 result:=size;

end;

{ WRITE_INT32 }
procedure scanline_storage_aa.write_int32;
begin
 int8u_ptr(ptrcomp(dst ) + 0 * sizeof(int8u ) )^:=int32_int8u(val )._0;
 int8u_ptr(ptrcomp(dst ) + 1 * sizeof(int8u ) )^:=int32_int8u(val )._1;
 int8u_ptr(ptrcomp(dst ) + 2 * sizeof(int8u ) )^:=int32_int8u(val )._2;
 int8u_ptr(ptrcomp(dst ) + 3 * sizeof(int8u ) )^:=int32_int8u(val )._3;

end;

{ SERIALIZE }
procedure scanline_storage_aa.serialize;
var
 i ,num_spans ,span_idx : unsigned;

 sl_this : scanline_data_ss_ptr;

 sp : span_data_ss_ptr;

 covers : int8u_ptr;

 size_ptr : int8u_ptr;

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
   sl_this :=m_scanlines.array_operator(i );
   size_ptr:=data;

   inc(ptrcomp(data ) ,sizeof(int32 ) );     // Reserve space for scanline size in bytes

   write_int32(data ,sl_this.y );            // Y
   inc(ptrcomp(data ) ,sizeof(int32 ) );

   write_int32(data ,sl_this.num_spans );    // num_spans
   inc(ptrcomp(data ) ,sizeof(int32 ) );

   num_spans:=sl_this.num_spans;
   span_idx :=sl_this.start_span;

   repeat
    sp:=m_spans.array_operator(span_idx );

    inc(span_idx );

    covers:=covers_by_index(sp.covers_id );

    write_int32(data ,sp.x );                // X
    inc(ptrcomp(data ) ,sizeof(int32 ) );

    write_int32(data ,sp.len );              // span_len
    inc(ptrcomp(data ) ,sizeof(int32 ) );

    if sp.len < 0 then
     begin
      move(covers^ ,data^ ,sizeof(int8u ) );
      inc (ptrcomp(data ) ,sizeof(int8u ) );

     end
    else
     begin
      move(covers^ ,data^ ,unsigned(sp.len ) * sizeof(int8u ) );
      inc (ptrcomp(data ) ,sizeof(int8u ) * unsigned(sp.len ) );

     end;

    dec(num_spans );

   until num_spans = 0;

   write_int32(size_ptr ,ptrcomp(data ) - ptrcomp(size_ptr ) );

   inc(i );

  end;

end;

{ SCANLINE_BY_INDEX }
function scanline_storage_aa.scanline_by_index;
begin
 if i < m_scanlines.size then
  result:=m_scanlines.array_operator(i )
 else
  result:=@m_fake_scanline;

end;

{ SPAN_BY_INDEX }
function scanline_storage_aa.span_by_index;
begin
 if i < m_spans.size then
  result:=m_spans.array_operator(i )
 else
  result:=@m_fake_span;

end;

{ COVERS_BY_INDEX }
function scanline_storage_aa.covers_by_index;
begin
 result:=m_covers.array_operator(i );

end;

{ CONSTRUCT }
constructor scanline_storage_aa16.Construct;
begin
 m_covers.Construct   (sizeof(int16u ) );
 m_spans.Construct    (256 - 2 ,sizeof(span_data_ss ) ,10 ); // Block increment size
 m_scanlines.Construct(sizeof(scanline_data_ss ) ,8 );

 m_min_x:=$7FFFFFFF;
 m_min_y:=$7FFFFFFF;
 m_max_x:=-$7FFFFFFF;
 m_max_y:=-$7FFFFFFF;

 m_cur_scanline:=0;

 m_fake_scanline.y         :=0;
 m_fake_scanline.num_spans :=0;
 m_fake_scanline.start_span:=0;

 m_fake_span.x        :=0;
 m_fake_span.len      :=0;
 m_fake_span.covers_id:=0;

end;

{ SWEEP_SCANLINE }
function scanline_storage_aa16.sweep_scanline;
var
 sl_this : scanline_data_ss_ptr;

 num_spans ,span_idx : unsigned;

 sp : span_data_ss_ptr;

 covers : int16u_ptr;

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

   covers:=covers_by_index(sp.covers_id );

   if sp.len < 0 then
    sl.add_span(sp.x ,unsigned(-sp.len ) ,covers^ )
   else
    sl.add_cells(sp.x ,sp.len ,int8u_ptr(covers ) );

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

{ BYTE_SIZE }
function scanline_storage_aa16.byte_size;
var
 i ,size ,num_spans ,span_idx : unsigned;

 sl_this : scanline_data_ss_ptr;

 sp : span_data_ss_ptr;

begin
 size:=sizeof(int32 ) * 4; // min_x, min_y, max_x, max_y

 i:=0;

 while i < m_scanlines.size do
  begin
   inc(size ,sizeof(int32 ) * 3 ); // scanline size in bytes, Y, num_spans

   sl_this:=m_scanlines.array_operator(i );

   num_spans:=sl_this.num_spans;
   span_idx :=sl_this.start_span;

   repeat
    sp:=m_spans.array_operator(span_idx );

    inc(span_idx );
    inc(size ,sizeof(int32 ) * 2 );                   // X, span_len

    if sp.len < 0 then
     inc(size ,sizeof(int16u ) )                      // cover
    else
     inc(size ,sizeof(int16u ) * unsigned(sp.len ) ); // covers

    dec(num_spans );

   until num_spans = 0;

   inc(i );

  end;

 result:=size;

end;

{ SERIALIZE }
procedure scanline_storage_aa16.serialize;
var
 i ,num_spans ,span_idx : unsigned;

 sl_this : scanline_data_ss_ptr;

 sp : span_data_ss_ptr;

 covers : int16u_ptr;

 size_ptr : int8u_ptr;

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
   sl_this :=m_scanlines.array_operator(i );
   size_ptr:=data;

   inc(ptrcomp(data ) ,sizeof(int32 ) );     // Reserve space for scanline size in bytes

   write_int32(data ,sl_this.y );            // Y
   inc(ptrcomp(data ) ,sizeof(int32 ) );

   write_int32(data ,sl_this.num_spans );    // num_spans
   inc(ptrcomp(data ) ,sizeof(int32 ) );

   num_spans:=sl_this.num_spans;
   span_idx :=sl_this.start_span;

   repeat
    sp:=m_spans.array_operator(span_idx );

    inc(span_idx );

    covers:=covers_by_index(sp.covers_id );

    write_int32(data ,sp.x );                // X
    inc(ptrcomp(data ) ,sizeof(int32 ) );

    write_int32(data ,sp.len );              // span_len
    inc(ptrcomp(data ) ,sizeof(int32 ) );

    if sp.len < 0 then
     begin
      move(covers^ ,data^ ,sizeof(int16u ) );
      inc (ptrcomp(data ) ,sizeof(int16u ) );

     end
    else
     begin
      move(covers^ ,data^ ,unsigned(sp.len ) * sizeof(int16u ) );
      inc (ptrcomp(data ) ,sizeof(int16u ) * unsigned(sp.len ) );

     end;

    dec(num_spans );

   until num_spans = 0;

   write_int32(size_ptr ,ptrcomp(data ) - ptrcomp(size_ptr ) );

   inc(i );

  end;

end;

{ CONSTRUCT }
constructor scanline_storage_aa32.Construct;
begin
 m_covers.Construct   (sizeof(int32u ) );
 m_spans.Construct    (256 - 2 ,sizeof(span_data_ss ) ,10 ); // Block increment size
 m_scanlines.Construct(sizeof(scanline_data_ss ) ,8 );

 m_min_x:=$7FFFFFFF;
 m_min_y:=$7FFFFFFF;
 m_max_x:=-$7FFFFFFF;
 m_max_y:=-$7FFFFFFF;

 m_cur_scanline:=0;

 m_fake_scanline.y         :=0;
 m_fake_scanline.num_spans :=0;
 m_fake_scanline.start_span:=0;

 m_fake_span.x        :=0;
 m_fake_span.len      :=0;
 m_fake_span.covers_id:=0;

end;

{ SWEEP_SCANLINE }
function scanline_storage_aa32.sweep_scanline;
var
 sl_this : scanline_data_ss_ptr;

 num_spans ,span_idx : unsigned;

 sp : span_data_ss_ptr;

 covers : int32u_ptr;

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

   covers:=covers_by_index(sp.covers_id );

   if sp.len < 0 then
    sl.add_span(sp.x ,unsigned(-sp.len ) ,covers^ )
   else
    sl.add_cells(sp.x ,sp.len ,int8u_ptr(covers ) );

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

{ BYTE_SIZE }
function scanline_storage_aa32.byte_size;
var
 i ,size ,num_spans ,span_idx : unsigned;

 sl_this : scanline_data_ss_ptr;

 sp : span_data_ss_ptr;

begin
 size:=sizeof(int32 ) * 4; // min_x, min_y, max_x, max_y

 i:=0;

 while i < m_scanlines.size do
  begin
   inc(size ,sizeof(int32 ) * 3 ); // scanline size in bytes, Y, num_spans

   sl_this:=m_scanlines.array_operator(i );

   num_spans:=sl_this.num_spans;
   span_idx :=sl_this.start_span;

   repeat
    sp:=m_spans.array_operator(span_idx );

    inc(span_idx );
    inc(size ,sizeof(int32 ) * 2 );                   // X, span_len

    if sp.len < 0 then
     inc(size ,sizeof(int32u ) )                      // cover
    else
     inc(size ,sizeof(int32u ) * unsigned(sp.len ) ); // covers

    dec(num_spans );

   until num_spans = 0;

   inc(i );

  end;

 result:=size;

end;

{ SERIALIZE }
procedure scanline_storage_aa32.serialize;
var
 i ,num_spans ,span_idx : unsigned;

 sl_this : scanline_data_ss_ptr;

 sp : span_data_ss_ptr;

 covers : int32u_ptr;

 size_ptr : int8u_ptr;

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
   sl_this :=m_scanlines.array_operator(i );
   size_ptr:=data;

   inc(ptrcomp(data ) ,sizeof(int32 ) );     // Reserve space for scanline size in bytes

   write_int32(data ,sl_this.y );            // Y
   inc(ptrcomp(data ) ,sizeof(int32 ) );

   write_int32(data ,sl_this.num_spans );    // num_spans
   inc(ptrcomp(data ) ,sizeof(int32 ) );

   num_spans:=sl_this.num_spans;
   span_idx :=sl_this.start_span;

   repeat
    sp:=m_spans.array_operator(span_idx );

    inc(span_idx );

    covers:=covers_by_index(sp.covers_id );

    write_int32(data ,sp.x );                // X
    inc(ptrcomp(data ) ,sizeof(int32 ) );

    write_int32(data ,sp.len );              // span_len
    inc(ptrcomp(data ) ,sizeof(int32 ) );

    if sp.len < 0 then
     begin
      move(covers^ ,data^ ,sizeof(int32u ) );
      inc (ptrcomp(data ) ,sizeof(int32u ) );

     end
    else
     begin
      move(covers^ ,data^ ,unsigned(sp.len ) * sizeof(int32u ) );
      inc (ptrcomp(data ) ,sizeof(int32u ) * unsigned(sp.len ) );

     end;

    dec(num_spans );

   until num_spans = 0;

   write_int32(size_ptr ,ptrcomp(data ) - ptrcomp(size_ptr ) );

   inc(i );

  end;

end;

{ CONSTRUCT }
constructor const_iterator_sa.Construct;
begin
 m_ptr:=sl.m_ptr;
 m_dx :=sl.m_dx;
 m_sz :=sz;

 init_span;

end;

{ X }
function const_iterator_sa.x;
begin
 result:=m_span.x;

end;

{ LEN }
function const_iterator_sa.len;
begin
 result:=m_span.len;

end;

{ COVERS }
function const_iterator_sa.covers;
begin
 result:=m_span.covers;

end;

{ INC_OPERATOR }
procedure const_iterator_sa.inc_operator;
begin
 if m_span.len < 0 then
  inc(ptrcomp(m_ptr ) ,m_sz )
 else
  inc(ptrcomp(m_ptr ) ,m_span.len * m_sz );

 init_span;

end;

{ INIT_SPAN }
procedure const_iterator_sa.init_span;
begin
 m_span.x     :=read_int32 + m_dx;
 m_span.len   :=read_int32;
 m_span.covers:=m_ptr;

end;

{ READ_INT32 }
function const_iterator_sa.read_int32;
begin
 int32_int8u(result )._0:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._1:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._2:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._3:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );

end;

{ CONSTRUCT }
constructor embedded_scanline_sa.Construct;
begin
 m_ptr:=NIL;
 m_y  :=0;
 m_sz :=sz;

 m_num_spans:=0;

end;

{ RESET }
procedure embedded_scanline_sa.reset;
begin
end;

{ Y }
function embedded_scanline_sa.y;
begin
 result:=m_y;

end;

{ NUM_SPANS }
function embedded_scanline_sa.num_spans;
begin
 result:=m_num_spans;

end;

{ BEGIN_ }
function embedded_scanline_sa.begin_;
begin
 m_result.Construct(@self ,m_sz );

 result:=@m_result;

end;

{ SZ_OF_SPAN }
function embedded_scanline_sa.sz_of_span;
begin
 result:=sizeof(span_ss );

end;

{ IS_PLAIN_SPAN }
function embedded_scanline_sa.is_plain_span;
begin
 result:=false;

end;

{ IS_EMBEDDED }
function embedded_scanline_sa.is_embedded;
begin
 result:=true;

end;

{ INIT }
procedure embedded_scanline_sa.init;
begin
 m_ptr      :=ptr;
 m_y        :=read_int32 + dy;
 m_num_spans:=unsigned(read_int32 );
 m_dx       :=dx;

end;

{ READ_INT32 }
function embedded_scanline_sa.read_int32;
begin
 int32_int8u(result )._0:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._1:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._2:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._3:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );

end;

{ CONSTRUCT }
constructor serialized_scanlines_adaptor_aa.Construct(sz : unsigned );
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

 m_sz:=sz;

end;

{ CONSTRUCT }
constructor serialized_scanlines_adaptor_aa.Construct(sz : unsigned; data : int8u_ptr; size : unsigned; dx ,dy : double );
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

 m_sz:=sz;

end;

{ INIT }
procedure serialized_scanlines_adaptor_aa.init;
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
function serialized_scanlines_adaptor_aa.read_int32;
begin
 int32_int8u(result )._0:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._1:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._2:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._3:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );

end;

{ READ_INT32U }
function serialized_scanlines_adaptor_aa.read_int32u;
begin
 int32_int8u(result )._0:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._1:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._2:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );
 int32_int8u(result )._3:=m_ptr^; inc(ptrcomp(m_ptr ) ,sizeof(int8u ) );

end;

{ REWIND_SCANLINES }
function serialized_scanlines_adaptor_aa.rewind_scanlines;
var
 x : int;

begin
 m_ptr:=m_data;

 if ptrcomp(m_ptr ) < ptrcomp(m_end ) then
  begin
   m_min_x:=read_int32 + m_dx;
   m_min_y:=read_int32 + m_dy;
   m_max_x:=read_int32 + m_dx;
   m_max_y:=read_int32 + m_dy;

   result:=true;

   exit;

  end;

 result:=false;

end;

{ _MIN_X }
function serialized_scanlines_adaptor_aa._min_x;
begin
 result:=m_min_x;

end;

{ _MIN_Y }
function serialized_scanlines_adaptor_aa._min_y;
begin
 result:=m_min_y;

end;

{ _MAX_X }
function serialized_scanlines_adaptor_aa._max_x;
begin
 result:=m_max_x;

end;

{ _MAX_Y }
function serialized_scanlines_adaptor_aa._max_y;
begin
 result:=m_max_y;

end;

{ SWEEP_SCANLINE }
function serialized_scanlines_adaptor_aa.sweep_scanline;
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

  read_int32;      // Skip scanline size in bytes

  y        :=read_int32 + m_dy;
  num_spans:=read_int32;

  repeat
   x  :=read_int32 + m_dx;
   len:=read_int32;

   if len < 0 then
    begin
     sl.add_span(x ,unsigned(-len ) ,m_ptr^ );

     inc(ptrcomp(m_ptr ) ,m_sz );

    end
   else
    begin
     sl.add_cells(x ,len ,m_ptr );

     inc(ptrcomp(m_ptr ) ,len * m_sz );

    end;

   dec(num_spans );

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
function serialized_scanlines_adaptor_aa.sweep_scanline_em;
var
 byte_size : unsigned;

begin
 repeat
  if ptrcomp(m_ptr ) >= ptrcomp(m_end ) then
   begin
    result:=false;

    exit;

   end;

  byte_size:=read_int32u;

  sl.init(m_ptr ,m_dx ,m_dy );

  inc(ptrcomp(m_ptr ) ,byte_size - sizeof(int32 ) );

 until sl.num_spans <> 0;

 result:=true;

end;

{ CONSTRUCT }
constructor serialized_scanlines_adaptor_aa8.Construct;
begin
 inherited Construct(sizeof(int8u ) );

end;

{ CONSTRUCT }
constructor serialized_scanlines_adaptor_aa8.Construct(data : int8u_ptr; size : unsigned; dx ,dy : double );
begin
 inherited Construct(sizeof(int8u ) ,data ,size ,dx ,dy );

end;

{ CONSTRUCT }
constructor serialized_scanlines_adaptor_aa16.Construct;
begin
 inherited Construct(sizeof(int16u ) );

end;

{ CONSTRUCT }
constructor serialized_scanlines_adaptor_aa16.Construct(data : int8u_ptr; size : unsigned; dx ,dy : double );
begin
 inherited Construct(sizeof(int8u ) ,data ,size ,dx ,dy );

end;

{ CONSTRUCT }
constructor serialized_scanlines_adaptor_aa32.Construct;
begin
 inherited Construct(sizeof(int32u ) );

end;

{ CONSTRUCT }
constructor serialized_scanlines_adaptor_aa32.Construct(data : int8u_ptr; size : unsigned; dx ,dy : double );
begin
 inherited Construct(sizeof(int8u ) ,data ,size ,dx ,dy );

end;

END.


