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
// library - in producing this work. See http://www.freetype.org for details.
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
// 17.10.2007-Milano: Finished OK
// 16.10.2007-Milano: Unit port establishment
//
{ agg_rasterizer_compound_aa.pas }
unit
 agg_rasterizer_compound_aa ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_array ,
 agg_rasterizer_cells_aa ,
 agg_rasterizer_sl_clip ,
 agg_vertex_source ,
 agg_scanline ;

{ GLOBAL VARIABLES & CONSTANTS }
const
 aa_shift  = 8;
 aa_scale  = 1 shl aa_shift;
 aa_mask   = aa_scale - 1;
 aa_scale2 = aa_scale * 2;
 aa_mask2  = aa_scale2 - 1;

{ TYPES DEFINITION }
type
 layer_order_e = (layer_unsorted ,layer_direct ,layer_inverse );

 aa_scale_e = int;

 style_info_ptr = ^style_info;
 style_info = record
   start_cell ,
   num_cells  : unsigned;

   last_x : int;

  end;

 cell_info_ptr = ^cell_info; 
 cell_info = record
   x ,area ,cover : int;

  end;

 rasterizer_compound_aa_ptr = ^rasterizer_compound_aa;
 rasterizer_compound_aa = object
  //private
   t_conv : ras_conv_ptr;

   m_outline : rasterizer_cells_aa;
   m_clipper : rasterizer_sl_ptr;

   m_filling_rule : filling_rule_e;
   m_layer_order  : layer_order_e;

   m_styles , // Active Styles
   m_ast    , // Active Style Table (unique values)
   m_asm    , // Active Style Mask
   m_cells  ,

   m_cover_buf    : pod_vector;
   m_master_alpha : pod_bvector;

   m_min_style ,
   m_max_style ,
   m_scan_y    ,
   m_sl_start  : int;
   m_sl_len    : unsigned;

  public
   constructor Construct(clip : rasterizer_sl_ptr );
   destructor  Destruct; virtual;

   procedure reset;
   procedure reset_clipping;
   procedure clip_box(x1 ,y1 ,x2 ,y2 : double );

   procedure filling_rule(filling_rule_ : filling_rule_e );
   procedure layer_order (order : layer_order_e );
   procedure master_alpha(style_ : int; alpha : double );

   procedure styles (left ,right : int );
   procedure move_to(x ,y : int ); virtual; abstract;
   procedure line_to(x ,y : int );

   procedure move_to_d(x ,y : double ); virtual; abstract;
   procedure line_to_d(x ,y : double );

   procedure add_vertex(x ,y : double; cmd : unsigned ); virtual; abstract;

   procedure edge  (x1 ,y1 ,x2 ,y2 : int );
   procedure edge_d(x1 ,y1 ,x2 ,y2 : double );

   procedure add_path(vs : vertex_source_ptr; path_id : unsigned = 0 );

   function  min_x : int;
   function  min_y : int;
   function  max_x : int;
   function  max_y : int;
   function  min_style : int;
   function  max_style : int;

   procedure sort;
   function  rewind_scanlines : boolean;
   function  sweep_styles : unsigned;
   function  scanline_start : int;
   function  scanline_length : unsigned;
   function  style(style_idx : unsigned ) : unsigned;

   function  allocate_cover_buffer(len : unsigned ) : cover_type_ptr;

   function  navigate_scanline(y : int ) : boolean;

   function  hit_test(tx ,ty : int ) : boolean;

   function  calculate_alpha(area : int; master_alpha_ : unsigned ) : unsigned;
   function  sweep_scanline (sl : scanline_ptr; style_idx : int ) : boolean;

  private
   procedure add_style(style_id : int );
   procedure allocate_master_alpha;

  end;

 rasterizer_compound_aa_int = object(rasterizer_compound_aa )
  private
   t_clip : rasterizer_sl_clip_int;

   m_start_x ,
   m_start_y : int;

  public
   constructor Construct(clip : rasterizer_sl_ptr = NIL );
   destructor  Destruct; virtual;

   procedure move_to  (x ,y : int ); virtual;
   procedure move_to_d(x ,y : double ); virtual;

   procedure add_vertex(x ,y : double; cmd : unsigned ); virtual;

  end;

 rasterizer_compound_aa_dbl = object(rasterizer_compound_aa )
  private
   t_clip : rasterizer_sl_clip_dbl;

   m_start_x ,
   m_start_y : double;

  public
   constructor Construct(clip : rasterizer_sl_ptr = NIL );
   destructor  Destruct; virtual;

   procedure move_to  (x ,y : int ); virtual;
   procedure move_to_d(x ,y : double ); virtual;

   procedure add_vertex(x ,y : double; cmd : unsigned ); virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor rasterizer_compound_aa.Construct(clip : rasterizer_sl_ptr );
begin
 m_clipper:=clip;
 t_conv   :=m_clipper.conv_type;

 m_outline.Construct;

 m_filling_rule:=fill_non_zero;
 m_layer_order :=layer_direct;

 m_styles.Construct(sizeof(style_info ) ); // Active Styles
 m_ast.Construct(sizeof(unsigned ) );      // Active Style Table (unique values)
 m_asm.Construct(sizeof(int8u ) );         // Active Style Mask
 m_cells.Construct(sizeof(cell_info ) );
 m_cover_buf.Construct(sizeof(cover_type ) );
 m_master_alpha.Construct(sizeof(unsigned ) );

 m_min_style:=$7FFFFFFF;
 m_max_style:=-$7FFFFFFF;
 m_scan_y   :=$7FFFFFFF;
 m_sl_start :=0;
 m_sl_len   :=0;

end;

{ DESTRUCT }
destructor rasterizer_compound_aa.Destruct;
begin
 m_outline.Destruct;
 m_styles.Destruct;
 m_ast.Destruct;
 m_asm.Destruct;
 m_cells.Destruct;
 m_cover_buf.Destruct;
 m_master_alpha.Destruct;

end;

{ RESET }
procedure rasterizer_compound_aa.reset;
begin
 m_outline.reset;

 m_min_style:=$7FFFFFFF;
 m_max_style:=-$7FFFFFFF;
 m_scan_y   :=$7FFFFFFF;
 m_sl_start :=0;
 m_sl_len   :=0;

end;

{ RESET_CLIPPING }
procedure rasterizer_compound_aa.reset_clipping;
begin
 reset;
 m_clipper.reset_clipping;

end;

{ CLIP_BOX }
procedure rasterizer_compound_aa.clip_box(x1 ,y1 ,x2 ,y2 : double );
begin
 reset;
 m_clipper.clip_box(
  t_conv.upscale(x1 ) ,t_conv.upscale(y1 ) ,
  t_conv.upscale(x2 ) ,t_conv.upscale(y2 ) );

end;

{ FILLING_RULE }
procedure rasterizer_compound_aa.filling_rule(filling_rule_ : filling_rule_e );
begin
 m_filling_rule:=filling_rule_;

end;

{ LAYER_ORDER }
procedure rasterizer_compound_aa.layer_order(order : layer_order_e );
begin
 m_layer_order:=order;

end;

{ MASTER_ALPHA }
procedure rasterizer_compound_aa.master_alpha(style_ : int; alpha : double );
var
 uv : unsigned;

begin
 if style_ >= 0 then
  begin
   uv:=aa_mask;

   while int(m_master_alpha.size ) <= style_ do
    m_master_alpha.add(@uv );

   unsigned_ptr(m_master_alpha.array_operator(style_ ) )^:=uround(alpha * aa_mask );

  end;

end;

{ STYLES }
procedure rasterizer_compound_aa.styles(left ,right : int );
var
 cell : cell_style_aa;

begin
 cell.initial;

 cell.left :=int16(left );
 cell.right:=int16(right );

 m_outline.style(@cell );

 if (left >= 0 ) and
    (left < m_min_style ) then
  m_min_style:=left;

 if (left >= 0 ) and
    (left > m_max_style ) then
  m_max_style:=left;

 if (right >= 0 ) and
    (right < m_min_style ) then
  m_min_style:=right;

 if (right >= 0 ) and
    (right > m_max_style ) then
  m_max_style:=right;

end;

{ LINE_TO }
procedure rasterizer_compound_aa.line_to(x ,y : int );
begin
 m_clipper.line_to(@m_outline ,t_conv.downscale(x ) ,t_conv.downscale(y ) );

end;

{ LINE_TO_D }
procedure rasterizer_compound_aa.line_to_d(x ,y : double );
begin
 m_clipper.line_to(@m_outline ,t_conv.upscale(x ) ,t_conv.upscale(y ) );

end;

{ EDGE }
procedure rasterizer_compound_aa.edge(x1 ,y1 ,x2 ,y2 : int );
begin
 if m_outline.sorted then
  reset;

 m_clipper.move_to(t_conv.downscale(x1 ) ,t_conv.downscale(y1 ) );
 m_clipper.line_to(@m_outline ,t_conv.downscale(x2 ) ,t_conv.downscale(y2 ) );

end;

{ EDGE_D }
procedure rasterizer_compound_aa.edge_d(x1 ,y1 ,x2 ,y2 : double );
begin
 if m_outline.sorted then
  reset;

 m_clipper.move_to(t_conv.upscale(x1 ) ,t_conv.upscale(y1 ) );
 m_clipper.line_to(@m_outline ,t_conv.upscale(x2 ) ,t_conv.upscale(y2 ) );

end;

{ ADD_PATH }
procedure rasterizer_compound_aa.add_path(vs : vertex_source_ptr; path_id : unsigned = 0 );
var
 x ,y : double;
 cmd  : unsigned;

begin
 vs.rewind(path_id );

 if m_outline.sorted then
  reset;

 cmd:=vs.vertex(@x ,@y );

 while not is_stop(cmd ) do
  begin
   add_vertex(x ,y ,cmd );

   cmd:=vs.vertex(@x ,@y );

  end;

end;

{ MIN_X }
function rasterizer_compound_aa.min_x : int;
begin
 result:=m_outline.min_x;

end;

{ MIN_Y }
function rasterizer_compound_aa.min_y : int;
begin
 result:=m_outline.min_y;

end;

{ MAX_X }
function rasterizer_compound_aa.max_x : int;
begin
 result:=m_outline.max_x;

end;

{ MAX_Y }
function rasterizer_compound_aa.max_y : int;
begin
 result:=m_outline.max_y;

end;

{ MIN_STYLE }
function rasterizer_compound_aa.min_style : int;
begin
 result:=m_min_style

end;

{ MAX_STYLE }
function rasterizer_compound_aa.max_style : int;
begin
 result:=m_max_style

end;

{ SORT }
procedure rasterizer_compound_aa.sort;
begin
 m_outline.sort_cells;

end;

{ REWIND_SCANLINES }
function rasterizer_compound_aa.rewind_scanlines : boolean;
begin
 m_outline.sort_cells;

 if m_outline.total_cells = 0 then
  begin
   result:=false;

   exit;

  end;

 if m_max_style < m_min_style then
  begin
   result:=false;

   exit;

  end;

 m_scan_y:=m_outline.min_y;

 m_styles.allocate(m_max_style - m_min_style + 2 ,128 );

 allocate_master_alpha;

 result:=true;

end;

{ SWEEP_STYLES }
function rasterizer_compound_aa.sweep_styles : unsigned;
var
 num_cells ,num_styles ,i ,start_cell ,uv ,v ,style_id : unsigned;

 curr_cell  : cell_style_aa_ptr;
 style_ ,st : style_info_ptr;

 cells : cell_style_aa_ptr_ptr;
 cell  : cell_info_ptr;

 ra : range_adaptor;

begin
 repeat
  if m_scan_y > m_outline.max_y then
   begin
    result:=0;

    exit;

   end;

  num_cells :=m_outline.scanline_num_cells(m_scan_y );
  cells     :=m_outline.scanline_cells    (m_scan_y );
  num_styles:=m_max_style - m_min_style + 2;

  m_cells.allocate(num_cells * 2 ,256 ); // Each cell can have two styles
  m_ast.capacity  (num_styles ,64 );
  m_asm.allocate  (shr_int32(num_styles + 7 ,3 ) , 8 );
  m_asm.zero;

  if num_cells <> 0 then
   begin
   // Pre-add zero (for no-fill style, that is, -1).
   // We need that to ensure that the "-1 style" would go first.
    int8u_ptr(m_asm.array_operator(0 ) )^:=int8u_ptr(m_asm.array_operator(0 ) )^ or 1;

    uv:=0;

    m_ast.add(@uv );

    style_:=m_styles.array_operator(0 );

    style_.start_cell:=0;
    style_.num_cells :=0;
    style_.last_x    :=-$7FFFFFFF;

    m_sl_start:=cells^^.x;
    m_sl_len  :=
     cell_style_aa_ptr_ptr(ptrcomp(cells ) + (num_cells - 1 ) * sizeof(cell_style_aa_ptr ) )^^.x
     - m_sl_start + 1;

    while num_cells <> 0 do
     begin
      dec(num_cells );

      curr_cell:=cells^;

      inc(ptrcomp(cells ) ,sizeof(cell_style_aa_ptr ) );

      add_style(curr_cell.left );
      add_style(curr_cell.right );

     end;

   // Convert the Y-histogram into the array of starting indexes
    start_cell:=0;

    i:=0;

    while i < m_ast.size do
     begin
      st:=style_info_ptr(m_styles.array_operator(unsigned_ptr(m_ast.array_operator(i ) )^ ) );

      v:=st.start_cell;

      st.start_cell:=start_cell;

      inc(start_cell ,v );
      inc(i );

     end;

    cells    :=m_outline.scanline_cells    (m_scan_y );
    num_cells:=m_outline.scanline_num_cells(m_scan_y );

    while num_cells <> 0 do
     begin
      dec(num_cells );

      curr_cell:=cells^;

      inc(ptrcomp(cells ) ,sizeof(cell_style_aa_ptr ) );

      if curr_cell.left < 0 then
       style_id:=0
      else
       style_id:=curr_cell.left - m_min_style + 1;

      style_:=m_styles.array_operator(style_id );

      if curr_cell.x = style_.last_x then
       begin
        cell:=m_cells.array_operator(style_.start_cell + style_.num_cells - 1 );

        inc(cell.area ,curr_cell.area );
        inc(cell.cover ,curr_cell.cover );

       end
      else
       begin
        cell:=m_cells.array_operator(style_.start_cell + style_.num_cells );

        cell.x       :=curr_cell.x;
        cell.area    :=curr_cell.area;
        cell.cover   :=curr_cell.cover;
        style_.last_x:=curr_cell.x;

        inc(style_.num_cells );

       end;

      if curr_cell.right < 0 then
       style_id:=0
      else
       style_id:=curr_cell.right - m_min_style + 1;

      style_:=m_styles.array_operator(style_id );

      if curr_cell.x = style_.last_x then
       begin
        cell:=m_cells.array_operator(style_.start_cell + style_.num_cells - 1 );

        dec(cell.area ,curr_cell.area );
        dec(cell.cover ,curr_cell.cover );

       end
      else
       begin
        cell:=m_cells.array_operator(style_.start_cell + style_.num_cells );

        cell.x       :=curr_cell.x;
        cell.area    :=-curr_cell.area;
        cell.cover   :=-curr_cell.cover;
        style_.last_x:=curr_cell.x;

        inc(style_.num_cells );

       end;

     end;

   end;

  if m_ast.size > 1 then
   break;

  inc(m_scan_y );

 until false;

 inc(m_scan_y );

 if m_layer_order <> layer_unsorted then
  begin
   ra.Construct(@m_ast ,1 ,m_ast.size - 1 );

   if m_layer_order = layer_direct then
    quick_sort(@ra ,@unsigned_greater )
   else
    quick_sort(@ra ,@unsigned_less );

  end;

 result:=m_ast.size - 1;

end;

{ SCANLINE_START }
function rasterizer_compound_aa.scanline_start : int;
begin
 result:=m_sl_start;

end;

{ SCANLINE_LENGTH }
function rasterizer_compound_aa.scanline_length : unsigned;
begin
 result:=m_sl_len;

end;

{ STYLE }
function rasterizer_compound_aa.style(style_idx : unsigned ) : unsigned;
begin
 result:=unsigned_ptr(m_ast.array_operator(style_idx + 1 ) )^ + m_min_style - 1;

end;

{ ALLOCATE_COVER_BUFFER }
function rasterizer_compound_aa.allocate_cover_buffer(len : unsigned ) : cover_type_ptr;
begin
 m_cover_buf.allocate(len ,256 );

 result:=m_cover_buf.array_operator(0 );

end;

{ NAVIGATE_SCANLINE }
function rasterizer_compound_aa.navigate_scanline(y : int ) : boolean;
begin
 m_outline.sort_cells;

 if m_outline.total_cells = 0 then
  begin
   result:=false;

   exit;

  end;

 if m_max_style < m_min_style then
  begin
   result:=false;

   exit;

  end;

 if (y < m_outline.min_y ) or
    (y > m_outline.max_y ) then
  begin
   result:=false;

   exit;

  end;

 m_scan_y:=y;

 m_styles.allocate(m_max_style - m_min_style + 2 ,128 );
 allocate_master_alpha;

 result:=true;

end;

{ HIT_TEST }
function rasterizer_compound_aa.hit_test(tx ,ty : int ) : boolean;
var
 num_styles : unsigned;

 sl : scanline_hit_test;

begin
 if not navigate_scanline(ty ) then
  begin
   result:=false;

   exit;

  end;

 num_styles:=sweep_styles;

 if num_styles <= 0 then
  begin
   result:=false;

   exit;

  end;

 sl.Construct  (tx );
 sweep_scanline(@sl ,-1 );

 result:=sl.hit();

end;

{ CALCULATE_ALPHA }
function rasterizer_compound_aa.calculate_alpha(area : int; master_alpha_ : unsigned ) : unsigned;
var
 cover : int;

begin
 cover:=shr_int32(area ,poly_subpixel_shift * 2 + 1 - aa_shift );

 if cover < 0 then
  cover:=-cover;

 if m_filling_rule = fill_even_odd then
  begin
   cover:=cover and aa_mask2;

   if cover > aa_scale then
    cover:=aa_scale2 - cover;

  end;

 if cover > aa_mask then
  cover:=aa_mask;

 result:=shr_int32(cover * master_alpha_ + aa_mask ,aa_shift );

end;

{ SWEEP_SCANLINE }
function rasterizer_compound_aa.sweep_scanline(sl : scanline_ptr; style_idx : int ) : boolean;
var
 scan_y ,cover ,x ,area : int;

 master_alpha_ ,num_cells ,alpha : unsigned;

 st : style_info_ptr;

 cell : cell_info_ptr;

begin
 scan_y:=m_scan_y - 1;

 if scan_y > m_outline.max_y then
  begin
   result:=false;

   exit;

  end;

 sl.reset_spans;

 master_alpha_:=aa_mask;

 if style_idx < 0 then
  style_idx:=0
 else
  begin
   inc(style_idx );

   master_alpha_:=
    unsigned_ptr(
     m_master_alpha.array_operator(
      unsigned_ptr(
       m_ast.array_operator(style_idx ) )^ + m_min_style - 1 ) )^;

  end;

 st       :=m_styles.array_operator(unsigned_ptr(m_ast.array_operator(style_idx ) )^ );
 num_cells:=st.num_cells;
 cell     :=m_cells.array_operator(st.start_cell );
 cover    :=0;

 while num_cells <> 0 do
  begin
   dec(num_cells );

   x   :=cell.x;
   area:=cell.area;

   inc(cover ,cell.cover );
   inc(ptrcomp(cell ) ,sizeof(cell_info ) );

   if area <> 0 then
    begin
     alpha:=calculate_alpha((cover shl (poly_subpixel_shift + 1 ) ) - area ,master_alpha_ );

     sl.add_cell(x ,alpha );

     inc(x );

    end;

   if (num_cells <> 0 ) and
      (cell.x > x ) then
    begin
     alpha:=calculate_alpha(cover shl (poly_subpixel_shift + 1 ) ,master_alpha_ );

     if alpha <> 0 then
      sl.add_span(x ,cell.x - x ,alpha );

    end;

  end;

 if sl.num_spans = 0 then
  begin
   result:=false;

   exit;

  end;

 sl.finalize(scan_y );

 result:=true;

end;

{ ADD_STYLE }
procedure rasterizer_compound_aa.add_style(style_id : int );
var
 nbyte ,mask : unsigned;

 style_ : style_info_ptr;

 uv : unsigned;

begin
 if style_id < 0 then
  style_id:=0
 else
  dec(style_id ,m_min_style - 1 );

 nbyte:=shr_int32(style_id ,3 );
 mask :=1 shl (style_id and 7 );

 style_:=m_styles.array_operator(style_id );

 if int8u_ptr(m_asm.array_operator(nbyte ) )^ and mask = 0 then
  begin
   uv:=style_id;

   m_ast.add(@uv );

   int8u_ptr(m_asm.array_operator(nbyte ) )^:=
    int8u_ptr(m_asm.array_operator(nbyte ) )^ or mask;

   style_.start_cell:=0;
   style_.num_cells :=0;
   style_.last_x    :=-$7FFFFFFF;

  end;

 inc(style_.start_cell );

end;

{ ALLOCATE_MASTER_ALPHA }
procedure rasterizer_compound_aa.allocate_master_alpha;
var
 uv : unsigned;

begin
 uv:=aa_mask;

 while int(m_master_alpha.size ) <= m_max_style do
  m_master_alpha.add(@uv );

end;

{ CONSTRUCT }
constructor rasterizer_compound_aa_int.Construct(clip : rasterizer_sl_ptr = NIL );
begin
 if clip <> NIL then
  inherited Construct(clip )
 else
  begin
   t_clip.Construct;

   inherited Construct(@t_clip );

  end;

 m_start_x:=0;
 m_start_y:=0;

end;

{ DESTRUCT }
destructor rasterizer_compound_aa_int.Destruct;
begin
 inherited Destruct;

end;

{ MOVE_TO }
procedure rasterizer_compound_aa_int.move_to(x ,y : int );
begin
 if m_outline.sorted then
  reset;

 m_start_x:=int_ptr(t_conv.downscale(x ) )^;
 m_start_y:=int_ptr(t_conv.downscale(y ) )^;

 m_clipper.move_to(@m_start_x ,@m_start_y );

end;

{ MOVE_TO_D }
procedure rasterizer_compound_aa_int.move_to_d(x ,y : double );
begin
 if m_outline.sorted then
  reset;

 m_start_x:=int_ptr(t_conv.upscale(x ) )^;
 m_start_y:=int_ptr(t_conv.upscale(y ) )^;

 m_clipper.move_to(@m_start_x ,@m_start_y );

end;

{ ADD_VERTEX }
procedure rasterizer_compound_aa_int.add_vertex(x ,y : double; cmd : unsigned );
begin
 if is_move_to(cmd ) then
  move_to_d(x ,y )
 else
  if is_vertex(cmd ) then
   line_to_d(x ,y )
  else
   if is_close(cmd ) then
    m_clipper.line_to(@m_outline ,@m_start_x ,@m_start_y );

end;

{ CONSTRUCT }
constructor rasterizer_compound_aa_dbl.Construct(clip : rasterizer_sl_ptr = NIL );
begin
 if clip <> NIL then
  inherited Construct(clip )
 else
  begin
   t_clip.Construct;

   inherited Construct(@t_clip );

  end;

 m_start_x:=0;
 m_start_y:=0;

end;

{ DESTRUCT }
destructor rasterizer_compound_aa_dbl.Destruct;
begin
 inherited Destruct;

end;

{ MOVE_TO }
procedure rasterizer_compound_aa_dbl.move_to(x ,y : int );
begin
 if m_outline.sorted then
  reset;

 m_start_x:=double_ptr(t_conv.downscale(x ) )^;
 m_start_y:=double_ptr(t_conv.downscale(y ) )^;

 m_clipper.move_to(@m_start_x ,@m_start_y );

end;

{ MOVE_TO_D }
procedure rasterizer_compound_aa_dbl.move_to_d(x ,y : double );
begin
 if m_outline.sorted then
  reset;

 m_start_x:=double_ptr(t_conv.upscale(x ) )^;
 m_start_y:=double_ptr(t_conv.upscale(y ) )^;

 m_clipper.move_to(@m_start_x ,@m_start_y );

end;

{ ADD_VERTEX }
procedure rasterizer_compound_aa_dbl.add_vertex(x ,y : double; cmd : unsigned );
begin
 if is_move_to(cmd ) then
  move_to_d(x ,y )
 else
  if is_vertex(cmd ) then
   line_to_d(x ,y )
  else
   if is_close(cmd ) then
    m_clipper.line_to(@m_outline ,@m_start_x ,@m_start_y );

end;

END.

