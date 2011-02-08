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
//
//----------------------------------------------------------------------------
//
// Adaptation for 32-bit screen coordinates has been sponsored by
// Liberty Technology Systems, Inc., visit http://lib-sys.com
//
// Liberty Technology Systems, Inc. is the provider of
// PostScript and PDF technology for software developers.
//
//----------------------------------------------------------------------------
//
// Class outline_aa - implementation.
//
// Initially the rendering algorithm was designed by David Turner and the
// other authors of the FreeType library - see the above notice. I nearly
// created a similar renderer, but still I was far from David's work.
// I completely redesigned the original code and adapted it for Anti-Grain
// ideas. Two functions - render_line and render_hline are the core of
// the algorithm - they calculate the exact coverage of each pixel cell
// of the polygon. I left these functions almost as is, because there's
// no way to improve the perfection - hats off to David and his group!
//
// All other code is very different from the original.
//
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 28.02.2006-Milano: scanline_hit_test
// 23.11.2005-Milano: outline_aa.sort_cells, rasterizer.rewind,sweep, ...
// 21.11.2005-Milano: outline_aa ,rasterizer_scanline_aa.line_to.move_to
// 17.11.2005-Milano: Unit port establishment
//
{ agg_rasterizer_scanline_aa.pas }
unit
 agg_rasterizer_scanline_aa ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
 agg_basics ,
 agg_array ,
 agg_scanline ,
 agg_vertex_source ,
 agg_gamma_functions ,
 agg_clip_liang_barsky ;

{ TYPES DEFINITION }
const
 aa_shift = 8;
 aa_num   = 1 shl aa_shift;
 aa_mask  = aa_num - 1;
 aa_2num  = aa_num * 2;
 aa_2mask = aa_2num - 1;

 cell_block_shift = 12;
 cell_block_size  = 1 shl cell_block_shift;
 cell_block_mask  = cell_block_size - 1;
 cell_block_pool  = 256;
 cell_block_limit = 1024;

// These constants determine the subpixel accuracy, to be more precise,
// the number of bits of the fractional part of the coordinates.
// The possible coordinate capacity in bits can be calculated by formula:
// sizeof(int) * 8 - poly_base_shift * 2, i.e, for 32-bit integers and
// 8-bits fractional part the capacity is 16 bits or [-32768...32767].
 poly_base_shift = 8;                      //----poly_base_shift
 poly_base_size  = 1 shl poly_base_shift;  //----poly_base_size
 poly_base_mask  = poly_base_size - 1;     //----poly_base_mask

type
//-----------------------------------------------------------------cell_aa
// A pixel cell. There're no constructors defined and it was done
// intentionally in order to avoid extra overhead when allocating an
// array of cells.
 cell_aa_ptr_ptr = ^cell_aa_ptr;
 cell_aa_ptr = ^cell_aa;
 cell_aa = record
   x ,y  ,
   cover ,
   area  : int;

  end;

//--------------------------------------------------------------outline_aa
// An internal class that implements the main rasterization algorithm.
// Used in the rasterizer. Should not be used direcly.
 sorted_y_ptr = ^sorted_y;
 sorted_y = record
   start ,
   num   : unsigned;

  end;

 outline_aa = object
   m_num_blocks ,
   m_max_blocks ,
   m_cur_block  ,
   m_num_cells  : unsigned;

   m_cur_x ,
   m_cur_y ,
   m_min_x ,
   m_min_y ,
   m_max_x ,
   m_max_y : int;

   m_sorted : boolean;

   m_cells        : cell_aa_ptr_ptr;
   m_cur_cell_ptr : cell_aa_ptr;
   m_cur_cell     : cell_aa;

   m_sorted_cells : pod_array;
   m_sorted_y     : pod_array;

   constructor Construct;
   destructor  Destruct;

   procedure move_to(x ,y : int );
   procedure line_to(x ,y : int );

   procedure reset;

   procedure add_cur_cell;
   procedure set_cur_cell(x ,y : int );

   procedure sort_cells;
   function  total_cells : unsigned;
   function  scanline_num_cells(y : unsigned ) : unsigned;
   function  scanline_cells    (y : unsigned ) : cell_aa_ptr_ptr;
   function  sorted : boolean;

   procedure render_line (x1 ,y1 ,x2 ,y2 : int );
   procedure render_hline(ey ,x1 ,y1 ,x2 ,y2 : int );

   procedure allocate_block;

   function  _min_x : int;
   function  _min_y : int;
   function  _max_x : int;
   function  _max_y : int;

  end;

//------------------------------------------------------scanline_hit_test
 scanline_hit_test = object(scanline )
   m_x   : int;
   m_hit : boolean;

   constructor Construct(x : int );

   procedure reset_spans; virtual;

   procedure finalize(y_ : int ); virtual;
   procedure add_cell(x : int; cover : unsigned ); virtual;
   procedure add_span(x : int; len ,cover : unsigned ); virtual;

   function  num_spans : unsigned; virtual;

   function  hit : boolean;

  end;

//==================================================rasterizer_scanline_aa
// Polygon rasterizer that is used to render filled polygons with
// high-quality Anti-Aliasing. Internally, by default, the class uses
// integer coordinates in format 24.8, i.e. 24 bits for integer part
// and 8 bits for fractional - see poly_base_shift. This class can be
// used in the following  way:
//
// 1. filling_rule(filling_rule_e ft) - optional.
//
// 2. gamma() - optional.
//
// 3. reset()
//
// 4. move_to(x, y) / line_to(x, y) - make the polygon. One can create 
//    more than one contour, but each contour must consist of at least 3
//    vertices, i.e. move_to(x1, y1); line_to(x2, y2); line_to(x3, y3);
//    is the absolute minimum of vertices that define a triangle.
//    The algorithm does not check either the number of vertices nor
//    coincidence of their coordinates, but in the worst case it just
//    won't draw anything.
//    The orger of the vertices (clockwise or counterclockwise) 
//    is important when using the non-zero filling rule (fill_non_zero).
//    In this case the vertex order of all the contours must be the same
//    if you want your intersecting polygons to be without "holes".
//    You actually can use different vertices order. If the contours do not
//    intersect each other the order is not important anyway. If they do,
//    contours with the same vertex order will be rendered without "holes"
//    while the intersecting contours with different orders will have "holes".
//
// filling_rule() and gamma() can be called anytime before "sweeping".
//------------------------------------------------------------------------
// filling_rule_e = (fill_non_zero ,fill_even_odd );
 status         = (status_initial ,status_line_to ,status_closed );

 rasterizer_scanline_ptr = ^rasterizer_scanline;
 rasterizer_scanline = object
   procedure reset; virtual; abstract;
   procedure filling_rule(filling_rule_ : filling_rule_e ); virtual; abstract;
   procedure clip_box    (x1 ,y1 ,x2 ,y2 : double ); virtual; abstract;

   procedure gamma(gamma_function  : vertex_source_ptr ); virtual; abstract;

   procedure add_path  (vs : vertex_source_ptr; path_id : unsigned = 0 ); virtual; abstract;
   procedure add_vertex(x ,y : double; cmd : unsigned ); virtual; abstract;

   procedure sort; virtual; abstract;
   function  rewind_scanlines : boolean; virtual; abstract;
   function  sweep_scanline   (sl : scanline_ptr ) : boolean; virtual; abstract;
   function  sweep_scanline_em(sl : scanline_ptr ) : boolean; virtual; abstract;

   function  hit_test(tx ,ty : int ) : boolean; virtual; abstract;

   function  _min_x : int; virtual; abstract;
   function  _min_y : int; virtual; abstract;
   function  _max_x : int; virtual; abstract;
   function  _max_y : int; virtual; abstract;

  end;

 rasterizer_scanline_aa_ptr = ^rasterizer_scanline_aa;
 rasterizer_scanline_aa = object(rasterizer_scanline )
   m_outline : outline_aa;
   m_gamma   : array [0..aa_num - 1 ] of int;

   m_filling_rule    : filling_rule_e;
   m_clipped_start_x ,
   m_clipped_start_y ,

   m_start_x ,
   m_start_y ,
   m_prev_x  ,
   m_prev_y  : int;

   m_prev_flags : unsigned;
   m_status     : status;

   m_clip_box : rect;
   m_clipping : boolean;

   m_cur_y ,
   XScale  : int;

   m_auto_close : boolean;

   constructor Construct;
   destructor  Destruct;

   procedure reset; virtual;
   procedure filling_rule(filling_rule_ : filling_rule_e ); virtual;
   procedure auto_close  (flag : boolean );
   procedure clip_box    (x1 ,y1 ,x2 ,y2 : double ); virtual;

   procedure gamma      (gamma_function  : vertex_source_ptr ); virtual;
   function  apply_gamma(cover : unsigned ) : unsigned;

   procedure move_to_no_clip(x ,y : int );
   procedure line_to_no_clip(x ,y : int );

   procedure close_polygon;
   procedure close_polygon_no_clip;
   procedure clip_segment(x ,y : int );

   procedure move_to_d(x ,y : double );
   procedure line_to_d(x ,y : double );

   procedure move_to(x ,y : int );
   procedure line_to(x ,y : int );

   procedure sort; virtual;
   function  rewind_scanlines : boolean; virtual;
   function  sweep_scanline(sl : scanline_ptr ) : boolean; virtual;

   function  navigate_scanline(y : int ) : boolean;

   function  hit_test(tx ,ty : int ) : boolean; virtual;

   function  _min_x : int; virtual;
   function  _min_y : int; virtual;
   function  _max_x : int; virtual;
   function  _max_y : int; virtual;

   function  calculate_alpha(area : int ) : unsigned;

   procedure add_path  (vs : vertex_source_ptr; path_id : unsigned = 0 ); virtual;
   procedure add_vertex(x ,y : double; cmd : unsigned ); virtual;

  end;

{ GLOBAL PROCEDURES }
 function  poly_coord(c : double ) : int;


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor outline_aa.Construct;
begin
 m_sorted_cells.Construct(sizeof(cell_aa_ptr ) );
 m_sorted_y.Construct    (sizeof(sorted_y ) );

 m_num_blocks:=0;
 m_max_blocks:=0;
 m_cur_block :=0;
 m_num_cells :=0;

 m_cur_x:=0;
 m_cur_y:=0;
 m_min_x:=$7FFFFFFF;
 m_min_y:=$7FFFFFFF;
 m_max_x:=-$7FFFFFFF;
 m_max_y:=-$7FFFFFFF;

 m_sorted:=false;

 m_cells       :=NIL;
 m_cur_cell_ptr:=NIL;

 with m_cur_cell do
  begin
   x:=$7FFF;
   y:=$7FFF;

   cover:=0;
   area :=0;

  end;

end;

{ DESTRUCT }
destructor outline_aa.Destruct;
begin
 m_sorted_cells.Destruct;
 m_sorted_y.Destruct;

 if m_num_blocks > 0 then
  begin
   repeat
    dec(m_num_blocks );

    agg_freemem(
     pointer(pointer(ptrcomp(m_cells ) + m_num_blocks * sizeof(cell_aa_ptr ) )^ ) ,
     cell_block_size * sizeof(cell_aa ) );

   until m_num_blocks = 0;

   agg_freemem(pointer(m_cells ) ,sizeof(cell_aa_ptr ) * m_max_blocks );

  end;

end;

{ MOVE_TO }
procedure outline_aa.move_to;
begin
 if m_sorted then
  reset;

// set_cur_cell(x shr poly_base_shift ,y shr poly_base_shift );
 set_cur_cell(shr_int32(x ,poly_base_shift ) ,shr_int32(y ,poly_base_shift ) );

 m_cur_x:=x;
 m_cur_y:=y;

end;

{ LINE_TO }
procedure outline_aa.line_to;
begin
 render_line(m_cur_x ,m_cur_y ,x ,y );

 m_cur_x:=x;
 m_cur_y:=y;

 m_sorted:=false;

end;

{ RESET }
procedure outline_aa.reset;
begin
 m_num_cells:=0;
 m_cur_block:=0;

 m_cur_cell.x    :=$7FFF;
 m_cur_cell.y    :=$7FFF;
 m_cur_cell.cover:=0;
 m_cur_cell.area :=0;

 m_sorted:=false;

 m_min_x:=$7FFFFFFF;
 m_min_y:=$7FFFFFFF;
 m_max_x:=-$7FFFFFFF;
 m_max_y:=-$7FFFFFFF;

end;

{ ADD_CUR_CELL }
procedure outline_aa.add_cur_cell;
begin
 if (m_cur_cell.area or m_cur_cell.cover ) <> 0 then
  begin
   if (m_num_cells and cell_block_mask ) = 0 then
    begin
     if m_num_blocks >= cell_block_limit then
      exit;

     allocate_block;

    end;

   m_cur_cell_ptr^:=m_cur_cell;

   inc(ptrcomp(m_cur_cell_ptr ) ,sizeof(cell_aa ) );
   inc(m_num_cells );

   if m_cur_cell.x < m_min_x then
    m_min_x:=m_cur_cell.x;

   if m_cur_cell.x > m_max_x then
    m_max_x:=m_cur_cell.x;

   if m_cur_cell.y < m_min_y then
    m_min_y:=m_cur_cell.y;

   if m_cur_cell.y > m_max_y then
    m_max_y:=m_cur_cell.y;

  end;

end;

{ SET_CUR_CELL }
procedure outline_aa.set_cur_cell;
begin
 if (m_cur_cell.x <> x ) or
    (m_cur_cell.y <> y ) then
  begin
   add_cur_cell;

   m_cur_cell.x:=x;
   m_cur_cell.y:=y;

   m_cur_cell.cover:=0;
   m_cur_cell.area :=0;

  end;

end;

{ qsort_cells }
procedure qsort_cells(start : cell_aa_ptr_ptr; num : unsigned );
var
 len ,x : int;
 temp   : pointer;

 stack : array[0..79 ] of cell_aa_ptr_ptr;
 limit ,
 base  ,
 i ,j  ,
 pivot : cell_aa_ptr_ptr;
 top   : ^cell_aa_ptr_ptr;

const
 qsort_threshold = 9;

begin
 limit:=cell_aa_ptr_ptr(ptrcomp(start ) + num * SizeOf(Pointer) );
 base :=start;
 top  :=@stack[0 ];

 repeat
  len:=(ptrcomp(limit ) - ptrcomp(base ) ) div SizeOf(Pointer);

  if len > qsort_threshold then
   begin
   // we use base + len/2 as the pivot
    pivot:=cell_aa_ptr_ptr(ptrcomp(base ) + (len div 2 ) * SizeOf(Pointer) );

    temp:=p32_ptr(base ).ptr;

    p32_ptr(base ).ptr :=p32_ptr(pivot ).ptr;
    p32_ptr(pivot ).ptr:=temp;

    i:=cell_aa_ptr_ptr(ptrcomp(base ) + SizeOf(Pointer) );
    j:=cell_aa_ptr_ptr(ptrcomp(limit ) - SizeOf(Pointer) );

   // now ensure that *i <= *base <= *j
    if j^.x < i^.x then
     begin
      temp:=p32_ptr(i ).ptr;

      p32_ptr(i ).ptr:=p32_ptr(j ).ptr;
      p32_ptr(j ).ptr:=temp;

     end;

    if base^.x < i^.x then
     begin
      temp:=p32_ptr(base ).ptr;

      p32_ptr(base ).ptr:=p32_ptr(i ).ptr;
      p32_ptr(i ).ptr   :=temp;

     end;

    if j^.x < base^.x then
     begin
      temp:=p32_ptr(base ).ptr;

      p32_ptr(base ).ptr:=p32_ptr(j ).ptr;
      p32_ptr(j ).ptr   :=temp;

     end;

    repeat
     x:=base^.x;

     inc(ptrcomp(i ) ,sizeof(cell_aa_ptr ) );

     while i^.x < x do
      inc(ptrcomp(i ) ,sizeof(cell_aa_ptr ) );

     dec(ptrcomp(j ) ,sizeof(cell_aa_ptr ) );

     while x < j^.x do
      dec(ptrcomp(j ) ,sizeof(cell_aa_ptr ) );

     if ptrcomp(i ) > ptrcomp(j ) then
      break;

     temp:=p32_ptr(i ).ptr;

     p32_ptr(i ).ptr:=p32_ptr(j ).ptr;
     p32_ptr(j ).ptr:=temp;

    until false;

    temp:=p32_ptr(base ).ptr;

    p32_ptr(base ).ptr:=p32_ptr(j ).ptr;
    p32_ptr(j ).ptr   :=temp;

   // now, push the largest sub-array
    if (ptrcomp(j ) - ptrcomp(base ) ) div SizeOf(Pointer) > (ptrcomp(limit ) - ptrcomp(i ) ) div SizeOf(Pointer) then
     begin
      top^:=base;

      inc(ptrcomp(top ) ,sizeof(cell_aa_ptr_ptr ) );

      top^:=j;
      base:=i;

     end
    else
     begin
      top^:=i;

      inc(ptrcomp(top ) ,sizeof(cell_aa_ptr_ptr ) );

      top^ :=limit;
      limit:=j;

     end;

    inc(ptrcomp(top ) ,sizeof(cell_aa_ptr_ptr ) );

   end
  else
   begin
   // the sub-array is small, perform insertion sort
    j:=base;
    i:=cell_aa_ptr_ptr(ptrcomp(j ) + 1 * SizeOf(Pointer) );

    while ptrcomp(i ) < ptrcomp(limit ) do
     begin
      try
       while cell_aa_ptr_ptr(ptrcomp(j ) + 1 * SizeOf(Pointer) )^^.x < j^.x do
        begin
         //swap_ptrs(cell_aa_ptr_ptr(ptrcomp(j ) + 1 * SizeOf(Pointer) ) ,j );
          temp:=p32_ptr(cell_aa_ptr_ptr(ptrcomp(j ) + 1 * SizeOf(Pointer) ) ).ptr;

          p32_ptr(cell_aa_ptr_ptr(ptrcomp(j ) + 1 * SizeOf(Pointer) ) ).ptr:=p32_ptr(j ).ptr;
          p32_ptr(j ).ptr:=temp;

         if j = base then
          break;

         dec(ptrcomp(j ) ,sizeof(cell_aa_ptr ) );

        end;

      except
      end;

      j:=i;

      inc(ptrcomp(i ) ,sizeof(cell_aa_ptr ) );

     end;

    if ptrcomp(top ) > ptrcomp(@stack[0 ] ) then
     begin
      dec(ptrcomp(top ) ,sizeof(cell_aa_ptr_ptr ) );

      limit:=top^;

      dec(ptrcomp(top ) ,sizeof(cell_aa_ptr_ptr ) );

      base:=top^;

     end
    else
     break;

   end;

 until false;

end;

{ SORT_CELLS }
procedure outline_aa.sort_cells;
var
 nb ,i ,
 v  ,
 start : unsigned;
 cur_y : sorted_y_ptr;

 block_ptr : cell_aa_ptr_ptr;
 cell_ptr  : cell_aa_ptr;

// fout : text;

begin
//Perform sort only the first time
 if m_sorted then
  exit;

 add_cur_cell;

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

 while nb > 0 do
  begin
   dec(nb );

   cell_ptr:=block_ptr^;

   inc(ptrcomp(block_ptr ) ,sizeof(cell_aa_ptr ) );

   i:=cell_block_size;

   while i > 0 do
    begin
     dec(i );
     inc(sorted_y_ptr(ptrcomp(m_sorted_y.m_array ) + unsigned(cell_ptr^.y - m_min_y ) * m_sorted_y.m_entry_sz ).start );
     inc(ptrcomp(cell_ptr ) ,sizeof(cell_aa ) );

    end;

  end;

 cell_ptr:=block_ptr^;

 inc(ptrcomp(block_ptr ) ,sizeof(cell_aa_ptr ) );

 i:=m_num_cells and cell_block_mask;

 while i > 0 do
  begin
   dec(i );
   inc(sorted_y_ptr(ptrcomp(m_sorted_y.m_array ) + unsigned(cell_ptr^.y - m_min_y ) * m_sorted_y.m_entry_sz ).start );
   inc(ptrcomp(cell_ptr ) ,sizeof(cell_aa ) );

  end;

// Convert the Y-histogram into the array of starting indexes
 start:=0;

 for i:=0 to m_sorted_y.size - 1 do
  begin
   v:=sorted_y_ptr(ptrcomp(m_sorted_y.m_array ) + i * m_sorted_y.m_entry_sz ).start;

   sorted_y_ptr(ptrcomp(m_sorted_y.m_array ) + i * m_sorted_y.m_entry_sz ).start:=start;

   inc(start ,v );

  end;

// Fill the cell pointer array sorted by Y
 block_ptr:=m_cells;

 nb:=m_num_cells shr cell_block_shift;

 while nb > 0 do
  begin
   dec(nb );

   cell_ptr:=block_ptr^;

   inc(ptrcomp(block_ptr ) ,sizeof(cell_aa_ptr ) );

   i:=cell_block_size;

   while i > 0 do
    begin
     dec(i );

     cur_y:=sorted_y_ptr(ptrcomp(m_sorted_y.m_array ) + unsigned(cell_ptr.y - m_min_y ) * m_sorted_y.m_entry_sz );

     p32_ptr(ptrcomp(m_sorted_cells.m_array ) + unsigned(cur_y.start + cur_y.num ) * m_sorted_cells.m_entry_sz ).ptr:=cell_ptr;

     inc(cur_y.num );
     inc(ptrcomp(cell_ptr ) ,sizeof(cell_aa ) );

    end;

  end;

 cell_ptr:=block_ptr^;

 inc(ptrcomp(block_ptr ) ,sizeof(cell_aa_ptr ) );

 i:=m_num_cells and cell_block_mask;

 while i > 0 do
  begin
   dec(i );

   cur_y:=sorted_y_ptr(ptrcomp(m_sorted_y.m_array ) + unsigned(cell_ptr.y - m_min_y ) * m_sorted_y.m_entry_sz );

   p32_ptr(ptrcomp(m_sorted_cells.m_array ) + unsigned(cur_y.start + cur_y.num ) * m_sorted_cells.m_entry_sz ).ptr:=cell_ptr;

   inc(cur_y.num );
   inc(ptrcomp(cell_ptr ) ,sizeof(cell_aa ) );

  end;

// Finally arrange the X-arrays
 for i:=0 to m_sorted_y.size - 1 do
  begin
   cur_y:=sorted_y_ptr(ptrcomp(m_sorted_y.m_array ) + i * m_sorted_y.m_entry_sz );

   if cur_y.num > 0 then
    qsort_cells(cell_aa_ptr_ptr(ptrcomp(m_sorted_cells.m_array ) + cur_y.start * m_sorted_cells.m_entry_sz ) ,cur_y.num );

  end;

 m_sorted:=true;

// M_SORTED_CELLS
(* assignfile(fout ,'sorted.yes' );
 rewrite(fout );

 for i:=0 to m_sorted_cells.size - 1 do
  begin
   cell_ptr:=p32_ptr(ptrcomp(m_sorted_cells.m_array ) + i * m_sorted_cells.m_entry_sz ).ptr;

   write(fout ,cell_ptr.x ,' ,' ,cell_ptr.y ,' ,' );
   write(fout ,cell_ptr.cover ,' ,' ,cell_ptr.area );

   writeln(fout );

  end;

 close(fout );*)

// M_SORTED_Y
(* assignfile(fout ,'y.yes' );
 rewrite(fout );

 for i:=0 to m_sorted_y.size - 1 do
  begin
   cur_y:=sorted_y_ptr(ptrcomp(m_sorted_y.m_array ) + i * m_sorted_y.m_entry_sz );

   writeln(fout ,cur_y.start ,' ,' ,cur_y.num );

  end;

 close(fout );*)

end;

{ TOTAL_CELLS }
function outline_aa.total_cells;
begin
 result:=m_num_cells;

end;

{ SCANLINE_NUM_CELLS }
function outline_aa.scanline_num_cells;
begin
 result:=sorted_y_ptr(ptrcomp(m_sorted_y.m_array ) + unsigned(y - m_min_y ) * m_sorted_y.m_entry_sz ).num;

end;

{ SCANLINE_CELLS }
function outline_aa.scanline_cells;
begin
 result:=
  cell_aa_ptr_ptr(
    ptrcomp(m_sorted_cells.m_array ) +
    sorted_y_ptr(ptrcomp(m_sorted_y.m_array ) + unsigned(y - m_min_y ) * m_sorted_y.m_entry_sz ).start
    * m_sorted_cells.m_entry_sz );

end;

{ SORTED }
function outline_aa.sorted;
begin
 result:=m_sorted;

end;

{ RENDER_LINE }
procedure outline_aa.render_line;
var
 p ,

 cx ,
 cy ,
 dx ,
 dy ,
 ex ,

 ey1 ,
 ey2 ,
 fy1 ,
 fy2 ,
 rem ,
 m_d ,

 x_from ,
 x_to   ,
 lift   ,
 delta  ,
 first  ,
 incr   ,
 two_fx ,
 area   : int;

const
 dx_limit = 16384 shl poly_base_shift;

begin
 dx:=x2 - x1;

 if (dx >= dx_limit ) or
    (dx <= -dx_limit ) then
  begin
   cx:=(x1 + x2) shr 1;
   cy:=(y1 + y2) shr 1;

   render_line(x1 ,y1 ,cx ,cy );
   render_line(cx ,cy ,x2 ,y2 );

  end;

 dy:=y2 - y1;

// ey1:=y1 shr poly_base_shift;
// ey2:=y2 shr poly_base_shift;
 ey1:=shr_int32(y1 ,poly_base_shift );
 ey2:=shr_int32(y2 ,poly_base_shift );

 fy1:=y1 and poly_base_mask;
 fy2:=y2 and poly_base_mask;

//everything is on a single hline
 if ey1 = ey2 then
  begin
   render_hline(ey1 ,x1 ,fy1 ,x2 ,fy2 );
   exit;

  end;

//Vertical line - we have to calculate start and end cells,
//and then - the common values of the area and coverage for
//all cells of the line. We know exactly there's only one
//cell, so, we don't have to call render_hline().
 incr:=1;

 if dx = 0 then
  begin
  // ex:=x1 shr poly_base_shift;
   ex:=shr_int32(x1 ,poly_base_shift );

   two_fx:=(x1 - (ex shl poly_base_shift ) ) shl 1;
   first :=poly_base_size;

   if dy < 0 then
    begin
     first:=0;
     incr :=-1;

    end;

   x_from:=x1;

   //render_hline(ey1 ,x_from ,fy1 ,x_from ,first );
   delta:=first - fy1;

   inc(m_cur_cell.cover ,delta );
   inc(m_cur_cell.area ,two_fx * delta );
   inc(ey1 ,incr );

   set_cur_cell(ex, ey1);

   delta:=first + first - poly_base_size;
   area :=two_fx * delta;

   while ey1 <> ey2 do
    begin
     //render_hline(ey1 ,x_from ,poly_base_size - first ,x_from ,first );
     m_cur_cell.cover:=delta;
     m_cur_cell.area :=area;

     inc(ey1 ,incr );

     set_cur_cell(ex ,ey1 );

    end;

   //render_hline(ey1, x_from, poly_base_size - first, x_from, fy2);
   delta:=fy2 - poly_base_size + first;

   inc(m_cur_cell.cover ,delta );
   inc(m_cur_cell.area ,two_fx * delta );

   exit;

  end;

//ok, we have to render several hlines
 p    :=(poly_base_size - fy1 ) * dx;
 first:=poly_base_size;

 if dy < 0 then
  begin
   p    :=fy1 * dx;
   first:=0;
   incr :=-1;
   dy   :=-dy;

  end;

 delta:=p div dy;
 m_d  :=p mod dy;

 if m_d < 0 then
  begin
   dec(delta );
   inc(m_d ,dy );

  end;

 x_from:=x1 + delta;

 render_hline(ey1 ,x1 ,fy1 ,x_from ,first );

 inc(ey1 ,incr );

// set_cur_cell(x_from shr poly_base_shift ,ey1 );
 set_cur_cell(shr_int32(x_from ,poly_base_shift ) ,ey1 );

 if ey1 <> ey2 then
  begin
   p   :=poly_base_size * dx;
   lift:=p div dy;
   rem :=p mod dy;

   if rem < 0 then
    begin
     dec(lift );
     inc(rem ,dy );

    end;

   dec(m_d ,dy );

   while ey1 <> ey2 do
    begin
     delta:=lift;

     inc(m_d ,rem );

     if m_d >= 0 then
      begin
       dec(m_d ,dy );
       inc(delta );

      end;

     x_to:=x_from + delta;

     render_hline(ey1 ,x_from ,poly_base_size - first ,x_to ,first );

     x_from:=x_to;

     inc(ey1 ,incr );

    // set_cur_cell(x_from shr poly_base_shift ,ey1 );
     set_cur_cell(shr_int32(x_from ,poly_base_shift ) ,ey1 );

    end;

  end;

 render_hline(ey1 ,x_from ,poly_base_size - first ,x2 ,fy2 );

end;

{ RENDER_HLINE }
procedure outline_aa.render_hline;
var
 p  ,
 dx ,

 ex1 ,
 ex2 ,
 fx1 ,
 fx2 ,

 delta ,
 first ,
 incr  ,
 lift  ,
 m_d   ,
 rem   : int;

begin
// ex1:=x1 shr poly_base_shift;
// ex2:=x2 shr poly_base_shift;
 ex1:=shr_int32(x1 ,poly_base_shift );
 ex2:=shr_int32(x2 ,poly_base_shift );

 fx1:=x1 and poly_base_mask;
 fx2:=x2 and poly_base_mask;

//trivial case. Happens often
 if y1 = y2 then
  begin
   set_cur_cell(ex2 ,ey );

   exit;

  end;

//everything is located in a single cell.  That is easy!
 if ex1 = ex2 then
  begin
   delta:=y2 - y1;

   inc(m_cur_cell.cover ,delta );
   inc(m_cur_cell.area ,(fx1 + fx2 ) * delta );

   exit;

  end;

//ok, we'll have to render a run of adjacent cells on the same
//hline...
 p    :=(poly_base_size - fx1 ) * (y2 - y1 );
 first:=poly_base_size;
 incr :=1;
 dx   :=x2 - x1;
 
 if dx < 0 then
  begin
   p    :=fx1 * (y2 - y1 );
   first:=0;
   incr :=-1;
   dx   :=-dx;

  end;

 delta:=p div dx;
 m_d  :=p mod dx;

 if m_d < 0 then
  begin
   dec(delta );
   inc(m_d ,dx );

  end;

 inc(m_cur_cell.cover ,delta );
 inc(m_cur_cell.area ,(fx1 + first ) * delta );

 inc(ex1 ,incr );

 set_cur_cell(ex1 ,ey );

 inc(y1 ,delta );

 if ex1 <> ex2 then
  begin
   p   :=poly_base_size * (y2 - y1 + delta );
   lift:=p div dx;
   rem :=p mod dx;

   if rem < 0 then
    begin
     dec(lift );
     inc(rem ,dx );

    end;

   dec(m_d ,dx );

   while ex1 <> ex2 do
    begin
     delta:=lift;

     inc(m_d ,rem );

     if m_d >= 0 then
      begin
       dec(m_d ,dx );
       inc(delta );

      end;

     inc(m_cur_cell.cover ,delta );
     inc(m_cur_cell.area ,(poly_base_size ) * delta );
     inc(y1 ,delta );
     inc(ex1 ,incr );

     set_cur_cell(ex1 ,ey );

    end;

  end;

 delta:=y2 - y1;

 inc(m_cur_cell.cover ,delta );
 inc(m_cur_cell.area ,(fx2 + poly_base_size - first ) * delta );

end;

{ ALLOCATE_BLOCK }
procedure outline_aa.allocate_block;
var
 new_cells : cell_aa_ptr_ptr;

begin
 if m_cur_block >= m_num_blocks then
  begin
   if m_num_blocks >= m_max_blocks then
    begin
     agg_getmem(pointer(new_cells ) ,sizeof(cell_aa_ptr ) * (m_max_blocks + cell_block_pool ) );

     if m_cells <> NIL then
      begin
       move(m_cells^ ,new_cells^ ,sizeof(cell_aa_ptr ) * m_max_blocks );

       agg_freemem(pointer(m_cells ) ,sizeof(cell_aa_ptr ) * m_max_blocks );

      end;

     m_cells:=new_cells;

     inc(m_max_blocks ,cell_block_pool );

    end;

   agg_getmem(
    pointer(pointer(ptrcomp(m_cells ) + m_num_blocks * sizeof(cell_aa_ptr ) )^ ) ,
    cell_block_size * sizeof(cell_aa ) );

   inc(m_num_blocks );

  end;

 m_cur_cell_ptr:=cell_aa_ptr_ptr(ptrcomp(m_cells ) + m_cur_block * sizeof(cell_aa_ptr ) )^;

 inc(m_cur_block );

end;

{ _MIN_X }
function outline_aa._min_x;
begin
 result:=m_min_x;

end;

{ _MIN_Y }
function outline_aa._min_y;
begin
 result:=m_min_y;

end;

{ _MAX_X }
function outline_aa._max_x;
begin
 result:=m_max_x;

end;

{ _MAX_Y }
function outline_aa._max_y;
begin
 result:=m_max_y;

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

{ CONSTRUCT }
constructor rasterizer_scanline_aa.Construct;
var
 i : integer;

begin
 m_outline.Construct;
 m_clip_box.Construct;

 m_filling_rule:=fill_non_zero;
 m_auto_close  :=true;

 m_clipped_start_x:=0;
 m_clipped_start_y:=0;

 m_start_x:=0;
 m_start_y:=0;
 m_prev_x :=0;
 m_prev_y :=0;

 m_prev_flags:=0;
 m_status    :=status_initial;
 m_clipping  :=false;

 for i:=0 to aa_num - 1 do
  m_gamma[i ]:=i;

 XScale:=1;

end;

{ DESTRUCT }
destructor rasterizer_scanline_aa.Destruct;
begin
 m_outline.Destruct;

end;

{ RESET }
procedure rasterizer_scanline_aa.reset;
begin
 m_outline.reset;

 m_status:=status_initial;

end;

{ FILLING_RULE }
procedure rasterizer_scanline_aa.filling_rule;
begin
 m_filling_rule:=filling_rule_;

end;

{ AUTO_CLOSE }
procedure rasterizer_scanline_aa.auto_close;
begin
 m_auto_close:=flag;

end;

{ CLIP_BOX }
procedure rasterizer_scanline_aa.clip_box;
begin
 reset;

 m_clip_box.x1:=poly_coord(x1 );
 m_clip_box.y1:=poly_coord(y1 );
 m_clip_box.x2:=poly_coord(x2 );
 m_clip_box.y2:=poly_coord(y2 );

 m_clip_box.normalize;

 m_clipping:=true; 

end;

{ GAMMA }
procedure rasterizer_scanline_aa.gamma;
var
 i : int;

begin
 for i:=0 to aa_num - 1 do
  m_gamma[i ]:=
   trunc(
    gamma_function.func_operator_gamma(i / aa_mask ) * aa_mask + 0.5 );

end;

{ APPLY_GAMMA }
function rasterizer_scanline_aa.apply_gamma;
begin
 result:=m_gamma[cover ]; 

end;

{ MOVE_TO_NO_CLIP }
procedure rasterizer_scanline_aa.move_to_no_clip;
begin
 if (m_status = status_line_to ) and
    m_auto_close then
  close_polygon_no_clip;

 m_outline.move_to(x * XScale ,y );

 m_clipped_start_x:=x;
 m_clipped_start_y:=y;

 m_status:=status_line_to;

end;

{ LINE_TO_NO_CLIP }
procedure rasterizer_scanline_aa.line_to_no_clip;
begin
if m_status <> status_initial then
 begin
  m_outline.line_to(x * XScale ,y );

  m_status:=status_line_to;

 end;

end;

{ CLOSE_POLYGON }
procedure rasterizer_scanline_aa.close_polygon;
begin
 if m_clipping then
  clip_segment(m_start_x ,m_start_y );

 if m_auto_close then
  close_polygon_no_clip;

end;

{ CLOSE_POLYGON_NO_CLIP }
procedure rasterizer_scanline_aa.close_polygon_no_clip;
begin
 if m_status = status_line_to then
  begin
   m_outline.line_to(m_clipped_start_x * XScale ,m_clipped_start_y );

   m_status:=status_closed;

  end;

end;

{ CLIP_SEGMENT }
procedure rasterizer_scanline_aa.clip_segment;
var
 flags ,n : unsigned;

 cx ,cy : array[0..3 ] of int;
 px ,py : int_ptr;

begin
 flags:=clipping_flags_int(x ,y ,@m_clip_box );

 if m_prev_flags = flags then
  if flags = 0 then
   if m_status = status_initial then
    move_to_no_clip(x ,y )
   else
    line_to_no_clip(x ,y )
  else
 else
  begin
   n:=clip_liang_barsky_int(m_prev_x ,m_prev_y ,x ,y ,@m_clip_box ,@cx[0 ] ,@cy[0 ] );

   px:=@cx[0 ];
   py:=@cy[0 ];

   while n > 0 do
    begin
     if m_status = status_initial then
      move_to_no_clip(px^ ,py^ )
     else
      line_to_no_clip(px^ ,py^ );

     inc(ptrcomp(px ) ,sizeof(int ) );
     inc(ptrcomp(py ) ,sizeof(int ) ); 
     dec(n );

    end;

  end;

 m_prev_flags:=flags;

 m_prev_x:=x;
 m_prev_y:=y;

end;

{ MOVE_TO_D }
procedure rasterizer_scanline_aa.move_to_d;
begin
 move_to(poly_coord(x ) ,poly_coord(y ) );

end;

{ LINE_TO_D }
procedure rasterizer_scanline_aa.line_to_d;
begin
 line_to(poly_coord(x ) ,poly_coord(y ) );

end;

{ MOVE_TO }
procedure rasterizer_scanline_aa.move_to;
begin
 if m_clipping then
  begin
   if m_outline.sorted then
    reset;

   if (m_status = status_line_to ) and
      m_auto_close then
    close_polygon;

   m_prev_x :=x;
   m_start_x:=x;
   m_prev_y :=y;
   m_start_y:=y;
   m_status :=status_initial;

   m_prev_flags:=clipping_flags_int(x ,y ,@m_clip_box );

   if m_prev_flags = 0 then
    move_to_no_clip(x ,y );

  end
 else
  move_to_no_clip(x ,y );

end;

{ LINE_TO }
procedure rasterizer_scanline_aa.line_to;
begin
 if m_clipping then
  clip_segment(x ,y )
 else
  line_to_no_clip(x ,y );

end;

{ SORT }
procedure rasterizer_scanline_aa.sort;
begin
 m_outline.sort_cells;

end;

{ REWIND_SCANLINES }
function rasterizer_scanline_aa.rewind_scanlines;
begin
 if m_auto_close then
  close_polygon;

 m_outline.sort_cells;

 if m_outline.total_cells = 0 then
  begin
   result:=false;

   exit;

  end;

 m_cur_y:=m_outline._min_y;
 result :=true;

end;

{ SWEEP_SCANLINE }
function rasterizer_scanline_aa.sweep_scanline;
var
 x     ,
 area  ,
 cover : int;
 alpha : unsigned;
 cells : cell_aa_ptr_ptr;

 cur_cell  : cell_aa_ptr;
 num_cells : unsigned;

begin
 repeat
  if m_cur_y > m_outline._max_y then
   begin
    result:=false;

    exit;

   end;

  sl.reset_spans;

  num_cells:=m_outline.scanline_num_cells(m_cur_y );
  cells    :=m_outline.scanline_cells    (m_cur_y );

  cover:=0;

  while num_cells > 0 do
   begin
    cur_cell:=cells^;

    x   :=cur_cell.x;
    area:=cur_cell.area;

    inc(cover ,cur_cell.cover );

   //accumulate all cells with the same X
    dec(num_cells );

    while num_cells > 0 do
     begin
      inc(ptrcomp(cells ) ,sizeof(cell_aa_ptr ) );

      cur_cell:=cells^;

      if cur_cell.x <> x then
       break;

      inc(area  ,cur_cell.area );
      inc(cover ,cur_cell.cover );

      dec(num_cells );

     end;

    if area <> 0 then
     begin
      alpha:=calculate_alpha((cover shl (poly_base_shift + 1 ) ) - area );

      if alpha <> 0 then
       sl.add_cell(x ,alpha );

      inc(x ); 

     end;

    if (num_cells <> 0 ) and
       (cur_cell.x > x ) then
     begin
      alpha:=calculate_alpha(cover shl (poly_base_shift + 1 ) );

      if alpha <> 0 then
       sl.add_span(x ,cur_cell.x - x ,alpha );

     end;  

   end;

  if boolean(sl.num_spans ) then
   break;

  inc(m_cur_y );

 until false;

 sl.finalize(m_cur_y);

 inc(m_cur_y );

 result:=true;

end;

{ NAVIGATE_SCANLINE }
function rasterizer_scanline_aa.navigate_scanline;
begin
 if m_auto_close then
  close_polygon;
  
 m_outline.sort_cells;

 if (m_outline.total_cells = 0 ) or
    (y < m_outline._min_y ) or
    (y > m_outline._max_y ) then
  begin
   result:=false;

   exit;

  end;

 m_cur_y:=y;
 result :=true;

end;

{ HIT_TEST }
function rasterizer_scanline_aa.hit_test;
var
 sl : scanline_hit_test;

begin
 if not navigate_scanline(ty ) then
  begin
   result:=false;

   exit;

  end;

 sl.Construct  (tx );
 sweep_scanline(@sl );

 result:=sl.hit;

end;

{ _MIN_X }
function rasterizer_scanline_aa._min_x;
begin
 result:=m_outline._min_x;

end;

{ _MIN_Y }
function rasterizer_scanline_aa._min_y;
begin
 result:=m_outline._min_y;

end;

{ _MAX_X }
function rasterizer_scanline_aa._max_x;
begin
 result:=m_outline._max_x;

end;

{ _MAX_Y }
function rasterizer_scanline_aa._max_y;
begin
 result:=m_outline._max_y;

end;

{ CALCULATE_ALPHA }
function rasterizer_scanline_aa.calculate_alpha;
var
 cover : system.integer;

begin
// 1: cover:=area shr (poly_base_shift * 2 + 1 - aa_shift );
// 2: cover:=round(area / (1 shl (poly_base_shift * 2 + 1 - aa_shift ) ) );
 cover:=shr_int32(area ,poly_base_shift * 2 + 1 - aa_shift );

 if cover < 0 then
  cover:=-cover;

 if m_filling_rule = fill_even_odd then
  begin
   cover:=cover and aa_2mask;

   if cover > aa_num then
    cover:=aa_2num - cover;

  end;

 if cover > aa_mask then
  cover:=aa_mask;

 result:=m_gamma[cover ];

end;

{ ADD_PATH }
procedure rasterizer_scanline_aa.add_path;
var
 cmd  : unsigned;
 x ,y : double;

begin
 vs.rewind(path_id );

 cmd:=vs.vertex(@x ,@y );

 while not is_stop(cmd ) do
  begin
   add_vertex(x ,y ,cmd );

   cmd:=vs.vertex(@x ,@y );

  end;

end;

{ ADD_VERTEX }
procedure rasterizer_scanline_aa.add_vertex;
begin
 if is_close(cmd ) then
  close_polygon
 else
  if is_move_to(cmd ) then
   move_to(poly_coord(x ) ,poly_coord(y ) )
  else
   if is_vertex(cmd ) then
    line_to(poly_coord(x ) ,poly_coord(y ) );

end;

{ POLY_COORD }
function poly_coord;
begin
 result:=trunc(c * poly_base_size );

end;

END.

