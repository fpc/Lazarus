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
//
// The author gratefully acknowleges the support of David Turner, 
// Robert Wilhelm, and Werner Lemberg - the authors of the FreeType 
// libray - in producing this work. See http://www.freetype.org for details.
//
//----------------------------------------------------------------------------
// Contact: mcseem@antigrain.com
//          mcseemagg@yahoo.com
//          http://www.antigrain.com
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
// 16.10.2007-Milano: Unit port establishment & Finished OK
//
{ agg_rasterizer_cells_aa.pas }
unit
 agg_rasterizer_cells_aa ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_math ,
 agg_array ,
 agg_scanline ;

{ GLOBAL VARIABLES & CONSTANTS }
const
 cell_block_shift = 12;
 cell_block_size  = 1 shl cell_block_shift;
 cell_block_mask  = cell_block_size - 1;
 cell_block_pool  = 256;
 cell_block_limit = 1024;

{ TYPES DEFINITION }
type
// A pixel cell. There're no constructors defined and it was done
// intentionally in order to avoid extra overhead when allocating an
// array of cells.
 cell_style_aa_ptr_ptr_ptr = ^cell_style_aa_ptr_ptr;
 cell_style_aa_ptr_ptr = ^cell_style_aa_ptr;
 cell_style_aa_ptr = ^cell_style_aa;
 cell_style_aa = object
   x ,y ,cover ,area : int;

   left ,right : int16;

   procedure initial;
   procedure style    (c : cell_style_aa_ptr );
   function  not_equal(ex ,ey : int; c : cell_style_aa_ptr ) : int;

  end;

 cell_block_scale_e = int;

 sorted_y_ptr = ^sorted_y;
 sorted_y = record
   start ,
   num   : unsigned;

  end;

{ cell_type_ptr_ptr_ptr = ^cell_type_ptr_ptr;
 cell_type_ptr_ptr = ^cell_type_ptr;
 cell_type_ptr = ^cell_type;
 cell_type = cell_style_aa; }

// An internal class that implements the main rasterization algorithm.
// Used in the rasterizer. Should not be used direcly.
 rasterizer_cells_aa_ptr = ^rasterizer_cells_aa;
 rasterizer_cells_aa = object
  private
   m_num_blocks ,
   m_max_blocks ,
   m_curr_block ,
   m_num_cells  : unsigned;

   m_cells : cell_style_aa_ptr_ptr;

   m_curr_cell_ptr : cell_style_aa_ptr;
   m_sorted_cells  ,
   m_sorted_y      : pod_vector;
   m_curr_cell     ,
   m_style_cell    : cell_style_aa;

   m_min_x ,
   m_min_y ,
   m_max_x ,
   m_max_y : int;

   m_sorted : boolean;

  public
   constructor Construct;
   destructor  Destruct;

   procedure reset;
   procedure style(style_cell : cell_style_aa_ptr );
   procedure line (x1 ,y1 ,x2 ,y2 : int );

   function  min_x : int;
   function  min_y : int;
   function  max_x : int;
   function  max_y : int;

   procedure sort_cells;
   function  total_cells : unsigned;

   function  scanline_num_cells(y : unsigned ) : unsigned;
   function  scanline_cells    (y : unsigned ) : cell_style_aa_ptr_ptr;

   function  sorted : boolean;

  private
   procedure set_curr_cell(x ,y : int );
   procedure add_curr_cell;
   procedure render_hline(ey ,x1 ,y1 ,x2 ,y2 : int );
   procedure allocate_block;

  end;

//------------------------------------------------------scanline_hit_test
 scanline_hit_test = object(scanline )
  private
   m_x   : int;
   m_hit : boolean;

  public
   constructor Construct(x : int );

   procedure reset_spans; virtual;

   procedure finalize(y_ : int ); virtual;
   procedure add_cell(x : int; cover : unsigned ); virtual;
   procedure add_span(x : int; len ,cover : unsigned ); virtual;

   function  num_spans : unsigned; virtual;

   function  hit : boolean;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ INITIAL }
procedure cell_style_aa.initial;
begin
 x    :=$7FFFFFFF;
 y    :=$7FFFFFFF;
 cover:=0;
 area :=0;
 left :=-1;
 right:=-1;

end;

{ STYLE }
procedure cell_style_aa.style(c : cell_style_aa_ptr );
begin
 left :=c.left;
 right:=c.right;

end;

{ NOT_EQUAL }
function cell_style_aa.not_equal(ex ,ey : int; c : cell_style_aa_ptr ) : int;
begin
 result:=(ex - x ) or (ey - y ) or (left - c.left ) or (right - c.right );

end;

{ CONSTRUCT }
constructor rasterizer_cells_aa.Construct;
begin
 m_num_blocks:=0;
 m_max_blocks:=0;
 m_curr_block:=0;
 m_num_cells :=0;

 m_cells        :=NIL;
 m_curr_cell_ptr:=NIL;

 m_sorted_cells.Construct(sizeof(cell_style_aa_ptr ) );
 m_sorted_y.Construct(sizeof(sorted_y ) );

 m_min_x :=$7FFFFFFF;
 m_min_y :=$7FFFFFFF;
 m_max_x :=-$7FFFFFFF;
 m_max_y :=-$7FFFFFFF;
 m_sorted:=false;

 m_style_cell.initial;
 m_curr_cell.initial;

end;

{ DESTRUCT }
destructor rasterizer_cells_aa.Destruct;
var
 ptr : cell_style_aa_ptr_ptr;

begin
 m_sorted_cells.Destruct;
 m_sorted_y.Destruct;

 if m_num_blocks <> 0 then
  begin
   ptr:=cell_style_aa_ptr_ptr(ptrcomp(m_cells ) + (m_num_blocks - 1 ) * sizeof(cell_style_aa_ptr ) );

   while m_num_blocks <> 0 do
    begin
     dec(m_num_blocks );

     agg_freemem(pointer(ptr^ ) ,cell_block_size * sizeof(cell_style_aa ) );

     dec(ptrcomp(ptr ) ,sizeof(cell_style_aa_ptr ) );

    end;

   agg_freemem(pointer(m_cells ) ,m_max_blocks * sizeof(cell_style_aa_ptr ) );

  end;

end;

{ RESET }
procedure rasterizer_cells_aa.reset;
begin
 m_num_cells :=0;
 m_curr_block:=0;

 m_curr_cell.initial;
 m_style_cell.initial;

 m_sorted:=false;
 m_min_x :=$7FFFFFFF;
 m_min_y :=$7FFFFFFF;
 m_max_x :=-$7FFFFFFF;
 m_max_y :=-$7FFFFFFF;

end;

{ STYLE }
procedure rasterizer_cells_aa.style(style_cell : cell_style_aa_ptr );
begin
 m_style_cell.style(style_cell );

end;

{ LINE }
procedure rasterizer_cells_aa.line(x1 ,y1 ,x2 ,y2 : int );
const
 dx_limit = 16384 shl poly_subpixel_shift;

var
 dx ,cx ,cy ,dy ,ex1 ,ex2 ,ey1 ,ey2 ,fy1 ,fy2 ,ex ,two_fx ,area ,

 x_from ,x_to ,p ,rem ,mod_ ,lift ,delta ,first ,incr : int;

begin
 dx:=x2 - x1;

 if (dx >= dx_limit ) or
    (dx <= -dx_limit ) then
  begin
   cx:=shr_int32(x1 + x2 ,1 );
   cy:=shr_int32(y1 + y2 ,1 );

   line(x1 ,y1 ,cx ,cy );
   line(cx ,cy ,x2 ,y2 );

  end;

 dy := y2 - y1;
 ex1:=shr_int32(x1 ,poly_subpixel_shift );
 ex2:=shr_int32(x2 ,poly_subpixel_shift );
 ey1:=shr_int32(y1 ,poly_subpixel_shift );
 ey2:=shr_int32(y2 ,poly_subpixel_shift );
 fy1:=y1 and poly_subpixel_mask;
 fy2:=y2 and poly_subpixel_mask;

 if ex1 < m_min_x then
  m_min_x:=ex1;

 if ex1 > m_max_x then
  m_max_x:=ex1;

 if ey1 < m_min_y then
  m_min_y:=ey1;

 if ey1 > m_max_y then
  m_max_y:=ey1;

 if ex2 < m_min_x then
  m_min_x:=ex2;

 if ex2 > m_max_x then
  m_max_x:=ex2;

 if ey2 < m_min_y then
  m_min_y:=ey2;

 if ey2 > m_max_y then
  m_max_y:=ey2;

 set_curr_cell(ex1 ,ey1 );

// everything is on a single hline
 if ey1 = ey2 then
  begin
   render_hline(ey1 ,x1 ,fy1 ,x2 ,fy2 );
   exit;

  end;

// Vertical line - we have to calculate start and end cells,
// and then - the common values of the area and coverage for
// all cells of the line. We know exactly there's only one
// cell, so, we don't have to call render_hline().
 incr:=1;

 if dx = 0 then
  begin
   ex    :=shr_int32(x1 ,poly_subpixel_shift );
   two_fx:=(x1 - (ex shl poly_subpixel_shift ) ) shl 1;
   first :=poly_subpixel_scale;

   if dy < 0 then
    begin
     first:=0;
     incr :=-1;

    end;

   x_from:=x1;

  // render_hline(ey1 ,x_from ,fy1 ,x_from ,first );
   delta:=first - fy1;

   inc(m_curr_cell.cover ,delta );
   inc(m_curr_cell.area ,two_fx * delta );
   inc(ey1 ,incr );

   set_curr_cell(ex ,ey1 );

   delta:=first + first - poly_subpixel_scale;
   area :=two_fx * delta;

   while ey1 <> ey2 do
    begin
    // render_hline(ey1 ,x_from ,poly_subpixel_scale - first ,x_from ,first );
     m_curr_cell.cover:=delta;
     m_curr_cell.area :=area;

     inc(ey1 ,incr );

     set_curr_cell(ex ,ey1 );

    end;

  // render_hline(ey1 ,x_from ,poly_subpixel_scale - first ,x_from ,fy2 );
   delta:=fy2 - poly_subpixel_scale + first;

   inc(m_curr_cell.cover ,delta );
   inc(m_curr_cell.area ,two_fx * delta );

   exit;

  end;

// ok, we have to render several hlines
 p    :=(poly_subpixel_scale - fy1 ) * dx;
 first:=poly_subpixel_scale;

 if dy < 0 then
  begin
   p    :=fy1 * dx;
   first:=0;
   incr :=-1;
   dy   :=-dy;

  end;

 delta:=p div dy;
 mod_ :=p mod dy;

 if mod_ < 0 then
  begin
   dec(delta );
   inc(mod_,dy );

  end;

 x_from:=x1 + delta;

 render_hline(ey1 ,x1 ,fy1 ,x_from ,first );

 inc(ey1 ,incr );

 set_curr_cell(shr_int32(x_from ,poly_subpixel_shift ) ,ey1 );

 if ey1 <> ey2 then
  begin
   p   :=poly_subpixel_scale * dx;
   lift:=p div dy;
   rem :=p mod dy;

   if rem < 0 then
    begin
     dec(lift );
     inc(rem ,dy );

    end;

   dec(mod_ ,dy );

   while ey1 <> ey2 do
    begin
     delta:=lift;

     inc(mod_ ,rem );

     if mod_ >= 0 then
      begin
       dec(mod_ ,dy );
       inc(delta );

      end;

     x_to:=x_from + delta;

     render_hline(ey1 ,x_from ,poly_subpixel_scale - first ,x_to ,first );

     x_from:=x_to;

     inc(ey1 ,incr );

     set_curr_cell(shr_int32(x_from ,poly_subpixel_shift ) ,ey1 );

    end;

  end;

 render_hline(ey1 ,x_from ,poly_subpixel_scale - first ,x2 ,fy2 );

end;

{ MIN_X }
function rasterizer_cells_aa.min_x : int;
begin
 result:=m_min_x;

end;

{ MIN_Y }
function rasterizer_cells_aa.min_y : int;
begin
 result:=m_min_y;

end;

{ MAX_X }
function rasterizer_cells_aa.max_x : int;
begin
 result:=m_max_x;

end;

{ MAX_Y }
function rasterizer_cells_aa.max_y : int;
begin
 result:=m_max_y;

end;

{ swap_cells }
procedure swap_cells(a ,b : pointer );
var
 temp : pointer;

begin
 temp        :=pointer(a^ );
 pointer(a^ ):=pointer(b^ );
 pointer(b^ ):=temp;

end;

const
 qsort_threshold = 9;

{ qsort_cells }
procedure qsort_cells(start : cell_style_aa_ptr_ptr; num : unsigned );
var
 stack : array[0..79 ] of cell_style_aa_ptr_ptr;
 top   : cell_style_aa_ptr_ptr_ptr;
 limit ,
 base  : cell_style_aa_ptr_ptr;

 len ,x : int;

 i ,j ,pivot : cell_style_aa_ptr_ptr;

begin
 limit:=cell_style_aa_ptr_ptr(ptrcomp(start ) + num * sizeof(cell_style_aa_ptr ) );
 base :=start;
 top  :=@stack[0 ];

 repeat
  len:=(ptrcomp(limit ) - ptrcomp(base ) ) div sizeof(cell_style_aa_ptr );

  if len > qsort_threshold then
   begin
   // we use base + len/2 as the pivot
    pivot:=cell_style_aa_ptr_ptr(ptrcomp(base ) + (len div 2 ) * sizeof(cell_style_aa_ptr ) );

    swap_cells(base ,pivot );

    i:=cell_style_aa_ptr_ptr(ptrcomp(base ) + sizeof(cell_style_aa_ptr ) );
    j:=cell_style_aa_ptr_ptr(ptrcomp(limit ) - sizeof(cell_style_aa_ptr ) );

   // now ensure that *i <= *base <= *j
    if j^^.x < i^^.x then
     swap_cells(i ,j );

    if base^^.x < i^^.x then
     swap_cells(base ,i );

    if j^^.x < base^^.x then
     swap_cells(base ,j );

    repeat
     x:=base^^.x;

     repeat
      inc(ptrcomp(i ) ,sizeof(cell_style_aa_ptr ) );

     until i^^.x >= x;

     repeat
      dec(ptrcomp(j ) ,sizeof(cell_style_aa_ptr ) );

     until x >= j^^.x;

     if ptrcomp(i ) > ptrcomp(j ) then
      break;

     swap_cells(i ,j ); 

    until false;

    swap_cells(base ,j );

   // now, push the largest sub-array
    if ptrcomp(j ) - ptrcomp(base ) > ptrcomp(limit ) - ptrcomp(i ) then
     begin
      top^:=base;

      cell_style_aa_ptr_ptr_ptr(ptrcomp(top ) + sizeof(cell_style_aa_ptr_ptr ) )^:=j;

      base:=i;

     end
    else
     begin
      top^:=i;

      cell_style_aa_ptr_ptr_ptr(ptrcomp(top ) + sizeof(cell_style_aa_ptr_ptr ) )^:=limit;

      limit:=j;

     end;

    inc(ptrcomp(top ) ,2 * sizeof(cell_style_aa_ptr_ptr ) );

   end
  else
   begin
   // the sub-array is small, perform insertion sort
    j:=base;
    i:=cell_style_aa_ptr_ptr(ptrcomp(j ) + sizeof(cell_style_aa_ptr ) );

    while ptrcomp(i ) < ptrcomp(limit ) do
     begin
      while cell_style_aa_ptr_ptr(ptrcomp(j ) + sizeof(cell_style_aa_ptr ) )^^.x < j^^.x do
       begin
        swap_cells(cell_style_aa_ptr_ptr(ptrcomp(j ) + sizeof(cell_style_aa_ptr ) ) ,j );

        if ptrcomp(j ) = ptrcomp(base ) then
         break;

        dec(ptrcomp(j ) ,sizeof(cell_style_aa_ptr ) );

       end;

      j:=i;

      inc(ptrcomp(i ) ,sizeof(cell_style_aa_ptr ) );

     end;

    if ptrcomp(top ) > ptrcomp(@stack[0 ] ) then
     begin
      dec(ptrcomp(top ) ,2 * sizeof(cell_style_aa_ptr_ptr ) );

      base :=top^;
      limit:=cell_style_aa_ptr_ptr_ptr(ptrcomp(top ) + sizeof(cell_style_aa_ptr_ptr ) )^;

     end
    else
     break;

   end;

 until false;

end;

{ SORT_CELLS }
procedure rasterizer_cells_aa.sort_cells;
var
 block_ptr : cell_style_aa_ptr_ptr;
 cell_ptr  : cell_style_aa_ptr;

 nb ,i ,start ,v : unsigned;

 curr_y : sorted_y_ptr;

begin
//Perform sort only the first time.
 if m_sorted then
  exit;

 add_curr_cell;

 m_curr_cell.x    :=$7FFFFFFF;
 m_curr_cell.y    :=$7FFFFFFF;
 m_curr_cell.cover:=0;
 m_curr_cell.area :=0;

 if m_num_cells = 0 then
  exit;

// Allocate the array of cell pointers
 m_sorted_cells.allocate(m_num_cells ,16 );

// Allocate and zero the Y array
 m_sorted_y.allocate(m_max_y - m_min_y + 1 ,16 );
 m_sorted_y.zero;

// Create the Y-histogram (count the numbers of cells for each Y)
 block_ptr:=m_cells;

 nb:=m_num_cells shr cell_block_shift;

 while nb <> 0 do
  begin
   dec(nb );

   cell_ptr:=block_ptr^;

   inc(ptrcomp(block_ptr ) ,sizeof(cell_style_aa_ptr ) );

   i:=cell_block_size;

   while i <> 0 do
    begin
     dec(i );
     inc(sorted_y_ptr(m_sorted_y.array_operator(cell_ptr.y - m_min_y ) ).start );
     inc(ptrcomp(cell_ptr ) ,sizeof(cell_style_aa ) );

    end;

  end;

 cell_ptr:=block_ptr^;

 inc(ptrcomp(block_ptr ) ,sizeof(cell_style_aa_ptr ) );

 i:=m_num_cells and cell_block_mask;

 while i <> 0 do
  begin
   dec(i );
   inc(sorted_y_ptr(m_sorted_y.array_operator(cell_ptr.y - m_min_y ) ).start );
   inc(ptrcomp(cell_ptr ) ,sizeof(cell_style_aa ) );

  end;

// Convert the Y-histogram into the array of starting indexes
 start:=0;
 i    :=0;

 while i < m_sorted_y.size do
  begin
   v:=sorted_y_ptr(m_sorted_y.array_operator(i ) ).start;

   sorted_y_ptr(m_sorted_y.array_operator(i ) ).start:=start;

   inc(start ,v );
   inc(i );

  end;

// Fill the cell pointer array sorted by Y
 block_ptr:=m_cells;

 nb:=m_num_cells shr cell_block_shift;

 while nb <> 0 do
  begin
   dec(nb );

   cell_ptr:=block_ptr^;

   inc(ptrcomp(block_ptr ) ,sizeof(cell_style_aa_ptr ) );

   i:=cell_block_size;

   while i <> 0 do
    begin
     dec(i );

     curr_y:=sorted_y_ptr(m_sorted_y.array_operator(cell_ptr.y - m_min_y ) );

     cell_style_aa_ptr_ptr(m_sorted_cells.array_operator(curr_y.start + curr_y.num ) )^:=cell_ptr;

     inc(curr_y.num );
     inc(ptrcomp(cell_ptr ) ,sizeof(cell_style_aa ) );

    end;

  end;

 cell_ptr:=block_ptr^;

 inc(ptrcomp(block_ptr ) ,sizeof(cell_style_aa_ptr ) );

 i:=m_num_cells and cell_block_mask;

 while i <> 0 do
  begin
   dec(i );

   curr_y:=sorted_y_ptr(m_sorted_y.array_operator(cell_ptr.y - m_min_y ) );

   cell_style_aa_ptr_ptr(m_sorted_cells.array_operator(curr_y.start + curr_y.num ) )^:=cell_ptr;

   inc(curr_y.num );
   inc(ptrcomp(cell_ptr ) ,sizeof(cell_style_aa ) );

  end;

// Finally arrange the X-arrays
 i:=0;

 while i < m_sorted_y.size do
  begin
   curr_y:=sorted_y_ptr(m_sorted_y.array_operator(i ) );

   if curr_y.num <> 0 then
    qsort_cells(
     cell_style_aa_ptr_ptr(ptrcomp(m_sorted_cells.data ) + curr_y.start * sizeof(cell_style_aa_ptr ) ) ,
     curr_y.num );

   inc(i );

  end;

 m_sorted:=true;

end;

{ TOTAL_CELLS }
function rasterizer_cells_aa.total_cells : unsigned;
begin
 result:=m_num_cells;

end;

{ SCANLINE_NUM_CELLS }
function rasterizer_cells_aa.scanline_num_cells(y : unsigned ) : unsigned;
begin
 result:=sorted_y_ptr(m_sorted_y.array_operator(y - m_min_y ) ).num;

end;

{ SCANLINE_CELLS }
function rasterizer_cells_aa.scanline_cells(y : unsigned ) : cell_style_aa_ptr_ptr;
begin
 result:=cell_style_aa_ptr_ptr(ptrcomp(m_sorted_cells.data ) + sorted_y_ptr(m_sorted_y.array_operator(y - m_min_y ) ).start * sizeof(cell_style_aa_ptr ) );

end;

{ SORTED }
function rasterizer_cells_aa.sorted : boolean;
begin
 result:=m_sorted;

end;

{ SET_CURR_CELL }
procedure rasterizer_cells_aa.set_curr_cell(x ,y : int );
begin
 if m_curr_cell.not_equal(x ,y ,@m_style_cell ) <> 0 then
  begin
   add_curr_cell;

   m_curr_cell.style(@m_style_cell );

   m_curr_cell.x    :=x;
   m_curr_cell.y    :=y;
   m_curr_cell.cover:=0;
   m_curr_cell.area :=0;

  end;

end;

{ ADD_CURR_CELL }
procedure rasterizer_cells_aa.add_curr_cell;
begin
 if m_curr_cell.area or m_curr_cell.cover <> 0 then
  begin
   if m_num_cells and cell_block_mask = 0 then
    begin
     if m_num_blocks >= cell_block_limit then
      exit;

     allocate_block;

    end;

   m_curr_cell_ptr^:=m_curr_cell;

   inc(ptrcomp(m_curr_cell_ptr ) ,sizeof(cell_style_aa ) );
   inc(m_num_cells );

  end;

end;

{ RENDER_HLINE }
procedure rasterizer_cells_aa.render_hline(ey ,x1 ,y1 ,x2 ,y2 : int );
var
 ex1 ,ex2 ,fx1 ,fx2 ,delta ,p ,first ,dx ,incr ,lift ,mod_ ,rem : int;

begin
 ex1:=shr_int32(x1 ,poly_subpixel_shift );
 ex2:=shr_int32(x2 ,poly_subpixel_shift );
 fx1:=x1 and poly_subpixel_mask;
 fx2:=x2 and poly_subpixel_mask;

// trivial case. Happens often
 if y1 = y2 then
  begin
   set_curr_cell(ex2 ,ey );
   exit;

  end;

// everything is located in a single cell.  That is easy!
 if ex1 = ex2 then
  begin
   delta:=y2 - y1;

   inc(m_curr_cell.cover ,delta );
   inc(m_curr_cell.area ,(fx1 + fx2 ) * delta );

   exit;

  end;

// ok, we'll have to render a run of adjacent cells on the same
// hline...
 p    :=(poly_subpixel_scale - fx1 ) * (y2 - y1 );
 first:=poly_subpixel_scale;
 incr :=1;

 dx:=x2 - x1;

 if dx < 0 then
  begin
   p    :=fx1 * (y2 - y1 );
   first:=0;
   incr :=-1;
   dx   :=-dx;

  end;

 delta:=p div dx;
 mod_ :=p mod dx;

 if mod_ < 0 then
  begin
   dec(delta );
   inc(mod_ ,dx );

  end;

 inc(m_curr_cell.cover ,delta );
 inc(m_curr_cell.area ,(fx1 + first ) * delta );
 inc(ex1 ,incr );

 set_curr_cell(ex1 ,ey );

 inc(y1 ,delta );

 if ex1 <> ex2 then
  begin
   p   :=poly_subpixel_scale * (y2 - y1 + delta );
   lift:=p div dx;
   rem :=p mod dx;

   if rem < 0 then
    begin
     dec(lift );
     inc(rem ,dx );

    end;

   dec(mod_ ,dx );

   while ex1 <> ex2 do
    begin
     delta:=lift;

     inc(mod_ ,rem );

     if mod_ >= 0 then
      begin
       dec(mod_ ,dx );
       inc(delta );

      end;

     inc(m_curr_cell.cover ,delta );
     inc(m_curr_cell.area ,poly_subpixel_scale * delta );

     inc(y1 ,delta );
     inc(ex1 ,incr );

     set_curr_cell(ex1 ,ey );

    end;

  end;

 delta:=y2 - y1;

 inc(m_curr_cell.cover ,delta );
 inc(m_curr_cell.area ,(fx2 + poly_subpixel_scale - first ) * delta );

end;

{ ALLOCATE_BLOCK }
procedure rasterizer_cells_aa.allocate_block;
var
 new_cells : cell_style_aa_ptr_ptr;

begin
 if m_curr_block >= m_num_blocks then
  begin
   if m_num_blocks >= m_max_blocks then
    begin
     agg_getmem(pointer(new_cells ) ,(m_max_blocks + cell_block_pool ) * sizeof(cell_style_aa_ptr ) );

     if m_cells <> NIL then
      begin
       move(m_cells^ ,new_cells^ ,m_max_blocks * sizeof(cell_style_aa_ptr ) );

       agg_freemem(pointer(m_cells ) ,m_max_blocks * sizeof(cell_style_aa_ptr ) );

      end;

     m_cells:=new_cells;

     inc(m_max_blocks ,cell_block_pool );

    end;

   agg_getmem(
    pointer(cell_style_aa_ptr_ptr(ptrcomp(m_cells ) + m_num_blocks * sizeof(cell_style_aa_ptr ) )^ ) ,
    cell_block_size * sizeof(cell_style_aa ) );

   inc(m_num_blocks );

  end;

 m_curr_cell_ptr:=cell_style_aa_ptr_ptr(ptrcomp(m_cells ) + m_curr_block * sizeof(cell_style_aa_ptr ) )^;

 inc(m_curr_block );

end;

{ CONSTRUCT }
constructor scanline_hit_test.Construct;
begin
 m_x  :=x;
 m_hit:=false;

end;

{ RESET_SPANS }
procedure scanline_hit_test.reset_spans;
begin
end;

{ FINALIZE }
procedure scanline_hit_test.finalize;
begin
end;

{ ADD_CELL }
procedure scanline_hit_test.add_cell;
begin
 if m_x = x then
  m_hit:=true;

end;

{ ADD_SPAN }
procedure scanline_hit_test.add_span;
begin
 if (m_x >= x ) and
    (m_x < x + len ) then
  m_hit:=true;

end;

{ NUM_SPANS }
function scanline_hit_test.num_spans;
begin
 result:=1;

end;

{ HIT }
function scanline_hit_test.hit;
begin
 result:=m_hit;

end;

END.

