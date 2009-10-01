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
// Class path_storage
//
// [Pascal Port History] -----------------------------------------------------
//
// 09.10.2007-Milano: Path Affine Transformations
// 23.06.2006-Milano: ptrcomp adjustments
// 19.12.2005-Milano: Unit port establishment
//
{ agg_path_storage.pas }
unit
 agg_path_storage ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_math ,
 agg_bezier_arc ,
 agg_vertex_source ,
 agg_trans_affine ;

{ TYPES DEFINITION }
// Allocation parameters
const
 block_shift = 8;
 block_size  = 1 shl block_shift;
 block_mask  = block_size - 1;
 block_pool  = 256;

type
 path_storage_ptr = ^path_storage;

 ps_vertex_source_ptr = ^ps_vertex_source;
 ps_vertex_source = object(vertex_source )
   m_path : path_storage_ptr;

   m_vertex_idx : unsigned;

   constructor Construct; overload;
   constructor Construct(p : path_storage_ptr ); overload;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

//------------------------------------------------------------path_storage
// A container to store vertices with their flags.
// A path consists of a number of contours separated with "move_to"
// commands. The path storage can keep and maintain more than one
// path.
// To navigate to the beginning of a particular path, use rewind(path_id);
// Where path_id is what start_new_path() returns. So, when you call
// start_new_path() you need to store its return value somewhere else
// to navigate to the path afterwards.
 path_storage = object(vertex_source )
   m_total_vertices ,
   m_total_blocks   ,
   m_max_blocks     : unsigned;

   m_coord_blocks : double_ptr_ptr;
   m_cmd_blocks : int8u_ptr_ptr;

   m_iterator : unsigned;

   constructor Construct; overload;
   constructor Construct(ps : path_storage_ptr ); overload;
   destructor  Destruct; virtual;

   procedure remove_all; virtual;

   function  last_vertex(x ,y : double_ptr ) : unsigned;
   function  prev_vertex(x ,y : double_ptr ) : unsigned;

   function  last_x : double;
   function  last_y : double;

   procedure rel_to_abs(x ,y : double_ptr );

   procedure move_to (x ,y : double );
   procedure move_rel(dx ,dy : double );

   procedure line_to (x ,y : double );
   procedure line_rel(dx ,dy : double );

   procedure hline_to (x : double );
   procedure hline_rel(dx : double );

   procedure vline_to (y : double );
   procedure vline_rel(dy : double );

   procedure arc_to (rx ,ry ,angle : double; large_arc_flag ,sweep_flag : boolean; x ,y : double );
   procedure arc_rel(rx ,ry ,angle : double; large_arc_flag ,sweep_flag : boolean; dx ,dy : double );

   procedure curve3    (x_ctrl ,y_ctrl ,x_to ,y_to : double ); overload;
   procedure curve3_rel(dx_ctrl ,dy_ctrl ,dx_to ,dy_to : double ); overload;

   procedure curve3    (x_to ,y_to : double ); overload;
   procedure curve3_rel(dx_to ,dy_to : double ); overload;

   procedure curve4    (x_ctrl1 ,y_ctrl1 ,x_ctrl2 ,y_ctrl2 ,x_to ,y_to : double ); overload;
   procedure curve4_rel(dx_ctrl1 ,dy_ctrl1 ,dx_ctrl2 ,dy_ctrl2 ,dx_to ,dy_to : double ); overload;

   procedure curve4    (x_ctrl2 ,y_ctrl2 ,x_to ,y_to : double ); overload;
   procedure curve4_rel(dx_ctrl2 ,dy_ctrl2 ,dx_to ,dy_to : double ); overload;

   procedure end_poly     (flags : unsigned = path_flags_close );
   procedure close_polygon(flags : unsigned = path_flags_none );

   procedure add_poly(vertices : double_2_ptr; num : unsigned; solid_path : boolean = false; end_flags : unsigned = path_flags_none );
   procedure add_path(vs : vertex_source_ptr; path_id : unsigned = 0; solid_path : boolean = true );

   function  start_new_path : unsigned;

   procedure copy_from(ps : path_storage_ptr );

   function  total_vertices : unsigned;

   function  vertex_(idx : unsigned; x ,y : double_ptr ) : unsigned;
   function  command(idx : unsigned ) : unsigned;
   procedure rewind (path_id : unsigned ); virtual;
   function  vertex (x ,y : double_ptr ) : unsigned; virtual;

  // Arrange the orientation of a polygon, all polygons in a path,
  // or in all paths. After calling arrange_orientations() or
  // arrange_orientations_all_paths(), all the polygons will have
  // the same orientation, i.e. path_flags_cw or path_flags_ccw
   function  arrange_polygon_orientation   (start ,orientation : unsigned ) : unsigned;
   function  arrange_orientations          (start ,orientation : unsigned ) : unsigned;
   procedure arrange_orientations_all_paths(orientation : unsigned );

  // Flip all the vertices horizontally or vertically
   procedure flip_x(x1 ,x2 : double );
   procedure flip_y(y1 ,y2 : double );

  // This function adds a vertex with its flags directly. Since there's no
  // checking for errors, keeping proper path integrity is the responsibility
  // of the caller. It can be said the function is "not very public".
   procedure add_vertex(x ,y : double; cmd : unsigned ); virtual;

  // Allows you to modify vertex coordinates. The caller must know
  // the index of the vertex.
   procedure modify_vertex(idx : unsigned; x ,y : double );

  // Allows you to modify vertex command. The caller must know
  // the index of the vertex.
   procedure modify_command(idx ,cmd : unsigned );

  // Path Affine Transformations
   procedure transform          (trans : trans_affine_ptr; path_id : unsigned = 0 );
   procedure transform_all_paths(trans : trans_affine_ptr );

  // Private
   procedure allocate_block(nb : unsigned );
   function  storage_ptrs  (xy_ptr : double_ptr_ptr ) : int8u_ptr;

   function  perceive_polygon_orientation(start ,end_ : unsigned ) : unsigned;

   procedure invert_polygon(start ,end_ : unsigned ); overload;
   procedure invert_polygon(start : unsigned ); overload;

  // from 2.4
   procedure concat_path(vs : vertex_source_ptr; path_id : unsigned = 0 );


  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor ps_vertex_source.Construct;
begin
 m_path:=NIL;

 m_vertex_idx:=0;

end;

{ CONSTRUCT }
constructor ps_vertex_source.Construct(p : path_storage_ptr );
begin
 m_path:=p;

 m_vertex_idx:=0;

end;

{ REWIND }
procedure ps_vertex_source.rewind;
begin
 m_vertex_idx:=path_id;

end;

{ VERTEX }
function ps_vertex_source.vertex;
begin
 if m_vertex_idx < m_path.total_vertices then
  begin
   result:=m_path.vertex_(m_vertex_idx ,x ,y );

   inc(m_vertex_idx );

  end
 else
  result:=path_cmd_stop;

end;

{ CONSTRUCT }
constructor path_storage.Construct;
begin
 m_total_vertices:=0;
 m_total_blocks  :=0;
 m_max_blocks    :=0;

 m_coord_blocks:=NIL;
 m_cmd_blocks  :=NIL;

 m_iterator:=0;

end;

{ CONSTRUCT }
constructor path_storage.Construct(ps : path_storage_ptr );
begin
 m_total_vertices:=0;
 m_total_blocks  :=0;
 m_max_blocks    :=0;

 m_coord_blocks:=NIL;
 m_cmd_blocks  :=NIL;

 m_iterator:=0;

 copy_from(ps );

end;

{ DESTRUCT }
destructor path_storage.Destruct;
var
 coord_blk : double_ptr_ptr;

begin
 if m_total_blocks <> 0 then
  begin
   coord_blk:=double_ptr_ptr(ptrcomp(m_coord_blocks ) + (m_total_blocks - 1 ) * sizeof(double_ptr ) );

   while m_total_blocks > 0 do
    begin
     agg_freemem(
      p32(coord_blk^ ).ptr ,
      (block_size * 2 + block_size div (sizeof(double ) div sizeof(int8u ) ) ) * sizeof(double ) );

     dec(ptrcomp(coord_blk ) ,sizeof(double_ptr ) );
     dec(m_total_blocks );

    end;

   agg_freemem(pointer(m_coord_blocks ) ,m_max_blocks * 2 * sizeof(double_ptr ) );
   
  end;

end;

{ REMOVE_ALL }
procedure path_storage.remove_all;
begin
 m_total_vertices:=0;
 m_iterator      :=0;

end;

{ LAST_VERTEX }
function path_storage.last_vertex;
begin
 if m_total_vertices <> 0 then
  result:=vertex_(m_total_vertices - 1 ,x ,y )
 else
  result:=path_cmd_stop;

end;

{ PREV_VERTEX }
function path_storage.prev_vertex;
begin
 if m_total_vertices > 1 then
  result:=vertex_(m_total_vertices - 2 ,x ,y )
 else
  result:=path_cmd_stop;

end;

{ LAST_X }
function path_storage.last_x;
var
 idx : unsigned;

begin
 if m_total_vertices <> 0 then
  begin
   idx:=m_total_vertices - 1;

   result:=
    double_ptr(
     ptrcomp(
      p32_ptr(ptrcomp(m_coord_blocks ) + (idx shr block_shift ) * sizeof(double_ptr ) ).ptr ) +
      ((idx and block_mask) shl 1 ) * sizeof(double ) )^;

  end
 else
  result:=0.0;

end;

{ LAST_Y }
function path_storage.last_y;
var
 idx : unsigned;

begin
 if m_total_vertices <> 0 then
  begin
   idx:=m_total_vertices - 1;

   result:=
    double_ptr(
     ptrcomp(
      p32_ptr(ptrcomp(m_coord_blocks ) + (idx shr block_shift ) * sizeof(double_ptr ) ).ptr ) +
      (((idx and block_mask) shl 1 ) + 1 ) * sizeof(double ) )^;

  end
 else
  result:=0.0;

end;

{ REL_TO_ABS }
procedure path_storage.rel_to_abs;
var
 x2 ,y2 : double;

begin
 if m_total_vertices <> 0 then
  if is_vertex(vertex_(m_total_vertices - 1 ,@x2 ,@y2 ) ) then
   begin
    x^:=x^ + x2;
    y^:=y^ + y2;

   end;

end;

{ MOVE_TO }
procedure path_storage.move_to;
begin
 add_vertex(x ,y ,path_cmd_move_to );

end;

{ MOVE_REL }
procedure path_storage.move_rel;
begin
 rel_to_abs(@dx ,@dy );
 add_vertex(dx ,dy ,path_cmd_move_to );

end;

{ LINE_TO }
procedure path_storage.line_to;
begin
 add_vertex(x ,y ,path_cmd_line_to );

end;

{ LINE_REL }
procedure path_storage.line_rel;
begin
 rel_to_abs(@dx ,@dy);
 add_vertex(dx ,dy ,path_cmd_line_to );

end;

{ HLINE_TO }
procedure path_storage.hline_to;
begin
 add_vertex(x ,last_y ,path_cmd_line_to );

end;

{ HLINE_REL }
procedure path_storage.hline_rel;
var
 dy : double;

begin
 dy:=0;

 rel_to_abs(@dx ,@dy);
 add_vertex(dx ,dy ,path_cmd_line_to );

end;

{ VLINE_TO }
procedure path_storage.vline_to;
begin
 add_vertex(last_x ,y ,path_cmd_line_to );

end;

{ VLINE_REL }
procedure path_storage.vline_rel;
var
 dx : double;

begin
 dx:=0;

 rel_to_abs(@dx, @dy);
 add_vertex(dx ,dy ,path_cmd_line_to );

end;

{ ARC_TO }
procedure path_storage.arc_to;
var
 a : bezier_arc_svg_ptr;

 x0 ,y0 ,epsilon : double;

begin
 a:=NIL;

 if (m_total_vertices <> 0 ) and
    is_vertex(command(m_total_vertices - 1 ) ) then
  begin
   epsilon:=1e-30;

   x0:=0.0;
   y0:=0.0;

   last_vertex(@x0 ,@y0 );

   rx:=Abs(rx );
   ry:=Abs(ry );

  // Ensure radii are valid
   if (rx < epsilon ) or
      (ry < epsilon ) then
    begin
     line_to(x ,y );
     exit;

    end;

  // If the endpoints (x, y) and (x0, y0) are identical, then this
  // is equivalent to omitting the elliptical arc segment entirely.
   if calc_distance(x0 ,y0 ,x ,y ) < epsilon then
    exit;

   new(a ,Construct(x0 ,y0 ,rx ,ry ,angle ,large_arc_flag ,sweep_flag ,x ,y ) );

   if a.radii_ok then
    add_path(a ,0 ,true )
   else
    line_to(x ,y );

  end
 else
  move_to(x ,y );

 if a <> NIL then
  dispose(a ,Destruct ); 

end;

{ ARC_REL }
procedure path_storage.arc_rel;
begin
 rel_to_abs(@dx ,@dy );
 arc_to    (rx ,ry ,angle ,large_arc_flag ,sweep_flag ,dx ,dy );

end;

{ CURVE3 }
procedure path_storage.curve3(x_ctrl ,y_ctrl ,x_to ,y_to : double );
begin
 add_vertex(x_ctrl ,y_ctrl ,path_cmd_curve3 );
 add_vertex(x_to   ,y_to   ,path_cmd_curve3 );

end;

{ CURVE3_REL }
procedure path_storage.curve3_rel(dx_ctrl ,dy_ctrl ,dx_to ,dy_to : double );
begin
 rel_to_abs(@dx_ctrl ,@dy_ctrl );
 rel_to_abs(@dx_to   ,@dy_to );
 add_vertex(dx_ctrl ,dy_ctrl ,path_cmd_curve3 );
 add_vertex(dx_to   ,dy_to   ,path_cmd_curve3 );

end;

{ CURVE3 }
procedure path_storage.curve3(x_to ,y_to : double );
var
 cmd : unsigned;

 x0 ,y0 ,x_ctrl ,y_ctrl : double;

begin
 if is_vertex(last_vertex(@x0 ,@y0 ) ) then
  begin
   cmd:=prev_vertex(@x_ctrl ,@y_ctrl );

   if is_curve(cmd ) then
    begin
     x_ctrl:=x0 + x0 - x_ctrl;
     y_ctrl:=y0 + y0 - y_ctrl;

    end
   else
    begin
     x_ctrl:=x0;
     y_ctrl:=y0;

    end;

   curve3(x_ctrl ,y_ctrl ,x_to ,y_to );

  end;

end;

{ CURVE3_REL }
procedure path_storage.curve3_rel(dx_to ,dy_to : double );
begin
 rel_to_abs(@dx_to ,@dy_to );
 curve3    (dx_to ,dy_to );

end;

{ CURVE4 }
procedure path_storage.curve4(x_ctrl1 ,y_ctrl1 ,x_ctrl2 ,y_ctrl2 ,x_to ,y_to : double );
begin
 add_vertex(x_ctrl1 ,y_ctrl1 ,path_cmd_curve4 );
 add_vertex(x_ctrl2 ,y_ctrl2 ,path_cmd_curve4 );
 add_vertex(x_to    ,y_to    ,path_cmd_curve4 );

end;

{ CURVE4_REL }
procedure path_storage.curve4_rel(dx_ctrl1 ,dy_ctrl1 ,dx_ctrl2 ,dy_ctrl2 ,dx_to ,dy_to : double );
begin
 rel_to_abs(@dx_ctrl1 ,@dy_ctrl1 );
 rel_to_abs(@dx_ctrl2 ,@dy_ctrl2 );
 rel_to_abs(@dx_to    ,@dy_to );
 add_vertex(dx_ctrl1 ,dy_ctrl1 ,path_cmd_curve4 );
 add_vertex(dx_ctrl2 ,dy_ctrl2 ,path_cmd_curve4 );
 add_vertex(dx_to    ,dy_to    ,path_cmd_curve4 );

end;

{ CURVE4 }
procedure path_storage.curve4(x_ctrl2 ,y_ctrl2 ,x_to ,y_to : double );
var
 cmd : unsigned;

 x0 ,y0 ,x_ctrl1 ,y_ctrl1 : double;

begin
 if is_vertex(last_vertex(@x0 ,@y0 ) ) then
  begin
   cmd:=prev_vertex(@x_ctrl1 ,@y_ctrl1 );

   if is_curve(cmd ) then
    begin
     x_ctrl1:=x0 + x0 - x_ctrl1;
     y_ctrl1:=y0 + y0 - y_ctrl1;

    end
   else
    begin
     x_ctrl1:=x0;
     y_ctrl1:=y0;

    end;

   curve4(x_ctrl1 ,y_ctrl1 ,x_ctrl2 ,y_ctrl2 ,x_to ,y_to );

  end;

end;

{ CURVE4_REL }
procedure path_storage.curve4_rel(dx_ctrl2 ,dy_ctrl2 ,dx_to ,dy_to : double );
begin
 rel_to_abs(@dx_ctrl2 ,@dy_ctrl2 );
 rel_to_abs(@dx_to    ,@dy_to );

 curve4(dx_ctrl2 ,dy_ctrl2 ,dx_to ,dy_to );

end;

{ END_POLY }
procedure path_storage.end_poly;
begin
 if m_total_vertices <> 0 then
  if is_vertex(command(m_total_vertices - 1 ) ) then
   add_vertex(0.0 ,0.0 ,path_cmd_end_poly or flags );

end;

{ CLOSE_POLYGON }
procedure path_storage.close_polygon;
begin
 end_poly(path_flags_close or flags );

end;

{ ADD_POLY }
procedure path_storage.add_poly;
begin
 if num <> 0 then
  begin
   if not solid_path then
    begin
     move_to(vertices[0 ] ,vertices[1 ] );

     inc(ptrcomp(vertices ) ,2 * sizeof(double ) );
     dec(num );

    end;

   while num > 0 do
    begin
     line_to(vertices[0 ] ,vertices[1 ] );

     inc(ptrcomp(vertices ) ,2 * sizeof(double ) );
     dec(num );

    end;

   if end_flags <> 0 then
    end_poly(end_flags );

  end;

end;

{ ADD_PATH }
procedure path_storage.add_path;
var
 cmd : unsigned;

 x ,y : double;

begin
 vs.rewind(path_id );

 cmd:=vs.vertex(@x ,@y );

 while not is_stop(cmd ) do
  begin
   if is_move_to(cmd ) and
      solid_path and
      (m_total_vertices <> 0 ) then
    cmd:=path_cmd_line_to;

   add_vertex(x ,y ,cmd );

   cmd:=vs.vertex(@x ,@y );

  end;

end;

{ START_NEW_PATH }
function path_storage.start_new_path;
begin
 if m_total_vertices <> 0 then
  if not is_stop(command(m_total_vertices - 1 ) ) then
   add_vertex(0.0 ,0.0 ,path_cmd_stop );

 result:=m_total_vertices;

end;

{ COPY_FROM }
procedure path_storage.copy_from;
var
 i ,cmd : unsigned;
 x ,y   : double;

begin
 remove_all;

 for i:=0 to ps.total_vertices - 1 do
  begin
   cmd:=ps.vertex_(i ,@x ,@y );

   add_vertex(x ,y ,cmd );

  end;

end;

{ TOTAL_VERTICES }
function path_storage.total_vertices;
begin
 result:=m_total_vertices

end;

{ VERTEX_ }
function path_storage.vertex_;
var
 nb : unsigned;
 pv : double_ptr;

begin
 nb:=idx shr block_shift;

 pv:=
  double_ptr(
   ptrcomp(
    p32_ptr(ptrcomp(m_coord_blocks ) + nb * sizeof(double_ptr ) ).ptr ) +
    ((idx and block_mask ) shl 1 ) * sizeof(double ) );

 x^:=pv^; inc(ptrcomp(pv ) ,sizeof(double ) );
 y^:=pv^;

 result:=
  int8u_ptr(
   ptrcomp(
    p32_ptr(ptrcomp(m_cmd_blocks ) + nb * sizeof(int8u_ptr ) ).ptr ) +
    (idx and block_mask ) * sizeof(int8u ) )^;

end;

{ COMMAND }
function path_storage.command;
begin
 result:=
  int8u_ptr(
   ptrcomp(
    p32_ptr(ptrcomp(m_cmd_blocks ) + (idx shr block_shift ) * sizeof(int8u_ptr ) ).ptr ) +
    (idx and block_mask ) * sizeof(int8u ) )^;

end;

{ REWIND }
procedure path_storage.rewind;
begin
 m_iterator:=path_id;

end;

{ VERTEX }
function path_storage.vertex;
begin
 if m_iterator >= m_total_vertices then
  result:=path_cmd_stop
 else
  begin
   result:=vertex_(m_iterator ,x ,y );

   inc(m_iterator );

  end;

end;

{ ARRANGE_POLYGON_ORIENTATION }
function path_storage.arrange_polygon_orientation;
var
 cmd  ,
 end_ : unsigned;

begin
 if orientation = path_flags_none then
  begin
   result:=start;

   exit;

  end;

// Skip all non-vertices at the beginning
 while (start < m_total_vertices ) and
       not is_vertex(command(start ) ) do
  inc(start );

// Skip all insignificant move_to
 while (start + 1 < m_total_vertices ) and
       is_move_to(command(start ) ) and
       is_move_to(command(start + 1 ) ) do
   inc(start );

// Find the last vertex
 end_:=start + 1;

 while (end_ < m_total_vertices ) and
       not is_next_poly(command(end_ ) ) do
  inc(end_ );

 if end_ - start > 2 then
  if perceive_polygon_orientation(start ,end_ ) <> orientation then
   begin
   // Invert polygon, set orientation flag, and skip all end_poly
    invert_polygon(start ,end_ );

    cmd:=command(end_ );

    while (end_ < m_total_vertices ) and
          is_end_poly(cmd ) do
     begin
      modify_command(end_ ,set_orientation(cmd ,orientation ) );

      inc(end_ );

      cmd:=command(end_ );

     end;

   end;

 result:=end_;  

end;

{ ARRANGE_ORIENTATIONS }
function path_storage.arrange_orientations;
begin
 if orientation <> path_flags_none then
  while start < m_total_vertices do
   begin
    start:=arrange_polygon_orientation(start ,orientation );

    if is_stop(command(start ) ) then
     begin
      inc(start );

      break;

     end;

   end;

 result:=start;  

end;

{ ARRANGE_ORIENTATIONS_ALL_PATHS }
procedure path_storage.arrange_orientations_all_paths;
var
 start : unsigned;

begin
 if orientation <> path_flags_none then
  begin
   start:=0;

   while start < m_total_vertices do
    start:=arrange_orientations(start ,orientation );

  end;

end;

{ FLIP_X }
procedure path_storage.flip_x;
var
 i ,cmd : unsigned;
 x ,y   : double;

begin
 if m_total_vertices > 0 then
  for i:=0 to m_total_vertices - 1 do
   begin
    cmd:=vertex_(i ,@x ,@y );

    if is_vertex(cmd ) then
     modify_vertex(i ,x2 - x + x1 ,y );

   end;

end;

{ FLIP_Y }
procedure path_storage.flip_y;
var
 i ,cmd : unsigned;
 x ,y   : double;

begin
 if m_total_vertices > 0 then
  for i:=0 to m_total_vertices - 1 do
   begin
    cmd:=vertex_(i ,@x ,@y );

    if is_vertex(cmd ) then
     modify_vertex(i ,x ,y2 - y + y1 );

   end;

end;

{ ADD_VERTEX }
procedure path_storage.add_vertex;
var
 coord_ptr : double_ptr;
 cmd_ptr   : int8u_ptr;

begin
 coord_ptr:=NIL;

 cmd_ptr:=storage_ptrs(@coord_ptr );

 cmd_ptr^:=int8u(cmd );

 coord_ptr^:=x; inc(ptrcomp(coord_ptr ) ,sizeof(double ) );
 coord_ptr^:=y;

 inc(m_total_vertices );

end;

{ MODIFY_VERTEX }
procedure path_storage.modify_vertex;
var
 pv : double_ptr;

begin
 pv:=
  double_ptr(
   ptrcomp(
    p32_ptr(ptrcomp(m_coord_blocks ) + (idx shr block_shift ) * sizeof(double_ptr ) ).ptr ) +
    ((idx and block_mask ) shl 1 ) * sizeof(double ) );

 pv^:=x; inc(ptrcomp(pv ) ,sizeof(double ) );
 pv^:=y;

end;

{ MODIFY_COMMAND }
procedure path_storage.modify_command;
begin
 int8u_ptr(
  ptrcomp(
   p32_ptr(ptrcomp(m_cmd_blocks ) + (idx shr block_shift ) * sizeof(int8u_ptr ) ).ptr ) +
   (idx and block_mask ) * sizeof(int8u ) )^:=int8u(cmd );

end;

{ TRANSFORM }
procedure path_storage.transform(trans : trans_affine_ptr; path_id : unsigned = 0 );
var
 x ,y : double;
 cmd  : unsigned;

begin
 while path_id < m_total_vertices do
  begin
   cmd:=vertex_(path_id ,@x ,@y );

   if is_stop(cmd ) then
    break;

   if is_vertex(cmd ) then
    begin
     trans.transform(trans ,@x ,@y );
     modify_vertex  (path_id ,x ,y );

    end;

   inc(path_id );

  end;

end;

{ TRANSFORM_ALL_PATHS }
procedure path_storage.transform_all_paths(trans : trans_affine_ptr );
var
 x ,y : double;
 idx  : unsigned;

begin
 idx:=0;

 while idx < m_total_vertices do
  begin
   if is_vertex(vertex_(idx ,@x ,@y ) ) then
    begin
     trans.transform(trans ,@x ,@y );
     modify_vertex  (idx ,x ,y );

    end;

   inc(idx );

  end;

end;

{ ALLOCATE_BLOCK }
procedure path_storage.allocate_block;
var
 new_coords : double_ptr_ptr;
 new_cmds   : int8u_ptr_ptr;

begin
 if nb >= m_max_blocks then
  begin
   agg_getmem(pointer(new_coords ) ,((m_max_blocks + block_pool ) * 2 ) * sizeof(double_ptr ) );

   new_cmds:=int8u_ptr_ptr(ptrcomp(new_coords ) + (m_max_blocks + block_pool ) * sizeof(double_ptr ) );

   if m_coord_blocks <> NIL then
    begin
     move(m_coord_blocks^ ,new_coords^ ,m_max_blocks * sizeof(double_ptr ) );
     move(m_cmd_blocks^ ,new_cmds^ ,m_max_blocks * sizeof(int8u_ptr ) );

     agg_freemem(pointer(m_coord_blocks ) ,m_max_blocks * 2 * sizeof(double_ptr ) );

    end;

   m_coord_blocks:=new_coords;
   m_cmd_blocks  :=new_cmds;

   inc(m_max_blocks ,block_pool );

  end;

 agg_getmem(
  p32_ptr(ptrcomp(m_coord_blocks ) + nb * sizeof(double_ptr ) ).ptr ,
  (block_size * 2 + block_size div (sizeof(double ) div sizeof(int8u ) ) ) * sizeof(double ) );

 p32_ptr(ptrcomp(m_cmd_blocks ) + nb * sizeof(int8u_ptr ) ).ptr:=
  pointer(
   ptrcomp(p32_ptr(ptrcomp(m_coord_blocks ) + nb * sizeof(double_ptr ) ).ptr ) +
   block_size * 2 * sizeof(double ) );

 inc(m_total_blocks );  

end;

{ STORAGE_PTRS }
function path_storage.storage_ptrs;
var
 nb : unsigned;

begin
 nb:=m_total_vertices shr block_shift;

 if nb >= m_total_blocks then
  allocate_block(nb );

 xy_ptr^:=
  double_ptr(
   ptrcomp(
    p32_ptr(ptrcomp(m_coord_blocks ) + nb * sizeof(double_ptr ) ).ptr ) +
    ((m_total_vertices and block_mask ) shl 1 ) * sizeof(double ) );

 result:=
  int8u_ptr(
   ptrcomp(
    p32_ptr(ptrcomp(m_cmd_blocks ) + nb * sizeof(int8u_ptr ) ).ptr ) +
    (m_total_vertices and block_mask ) * sizeof(int8u ) );

end;

{ PERCEIVE_POLYGON_ORIENTATION }
function path_storage.perceive_polygon_orientation;
var
 np ,i : unsigned;

 area ,x1 ,y1 ,x2 ,y2 : double;

begin
// Calculate signed area (double area to be exact)
 np  :=end_ - start;
 area:=0.0;

 if np > 0 then
  for i:=0 to np - 1 do
   begin
    vertex_(start + i               ,@x1 ,@y1 );
    vertex_(start + (i + 1 ) mod np ,@x2 ,@y2 );

    area:=area + (x1 * y2 - y1 * x2 );

   end;

 if area < 0.0 then
  result:=path_flags_cw
 else
  result:=path_flags_ccw;

end;

{ INVERT_POLYGON }
procedure path_storage.invert_polygon(start ,end_ : unsigned );
var
 i ,tmp_cmd ,start_nb ,end_nb : unsigned;

 start_ptr ,end_ptr : double_ptr;

 tmp_xy : double;

begin
 tmp_cmd:=command(start );

 dec(end_ ); // Make "end" inclusive

// Shift all commands to one position
 i:=start;

 while i < end_ do
  begin
   modify_command(i ,command(i + 1 ) );

   inc(i );

  end;

// Assign starting command to the ending command
 modify_command(end_ ,tmp_cmd );

// Reverse the polygon
 while end_ > start do
  begin
   start_nb:=start shr block_shift;
   end_nb  :=end_  shr block_shift;

   start_ptr:=
    double_ptr(
     ptrcomp(
      p32_ptr(ptrcomp(m_coord_blocks ) + start_nb * sizeof(double_ptr ) ).ptr ) +
      ((start and block_mask ) shl 1 ) * sizeof(double ) );

   end_ptr:=
    double_ptr(
     ptrcomp(
      p32_ptr(ptrcomp(m_coord_blocks ) + end_nb * sizeof(double_ptr ) ).ptr ) +
      ((end_ and block_mask ) shl 1 ) * sizeof(double ) );

   tmp_xy    :=start_ptr^;
   start_ptr^:=end_ptr^;    inc(ptrcomp(start_ptr ) ,sizeof(double ) );
   end_ptr^  :=tmp_xy;      inc(ptrcomp(end_ptr ) ,sizeof(double ) );

   tmp_xy    :=start_ptr^;
   start_ptr^:=end_ptr^;
   end_ptr^  :=tmp_xy;

   tmp_cmd:=
    int8u_ptr(
     ptrcomp(
      p32_ptr(ptrcomp(m_cmd_blocks ) + start_nb * sizeof(int8u_ptr ) ).ptr ) +
      (start and block_mask ) * sizeof(int8u ) )^;

   int8u_ptr(
    ptrcomp(
     p32_ptr(ptrcomp(m_cmd_blocks ) + start_nb * sizeof(int8u_ptr ) ).ptr ) +
     (start and block_mask ) * sizeof(int8u ) )^:=
    int8u_ptr(
     ptrcomp(
      p32_ptr(ptrcomp(m_cmd_blocks ) + end_nb * sizeof(int8u_ptr ) ).ptr ) +
      (end_ and block_mask ) * sizeof(int8u ) )^;

   int8u_ptr(
    ptrcomp(
     p32_ptr(ptrcomp(m_cmd_blocks ) + end_nb * sizeof(int8u_ptr ) ).ptr ) +
     (end_ and block_mask ) * sizeof(int8u ) )^:=int8u(tmp_cmd );

   inc(start );
   dec(end_ );

  end;

end;

{ INVERT_POLYGON }
procedure path_storage.invert_polygon(start : unsigned );
var
 end_ : unsigned;

begin
// Skip all non-vertices at the beginning
 while (start < m_total_vertices ) and
        not is_vertex(command(start ) ) do
  inc(start );

// Skip all insignificant move_to
 while (start + 1 < m_total_vertices ) and
       is_move_to(command(start ) ) and
       is_move_to(command(start + 1 ) ) do
  inc(start );

// Find the last vertex
 end_:=start + 1;

 while (end_ < m_total_vertices ) and
       not is_next_poly(command(end_ ) ) do
  inc(end_ );

 invert_polygon(start ,end_ );

end;

{ CONCAT_PATH }
procedure path_storage.concat_path(vs : vertex_source_ptr; path_id : unsigned = 0 );
var
 x ,y : double;
 cmd  : unsigned;

begin
 vs.rewind(path_id );

 cmd:=vs.vertex(@x ,@y );

 while not is_stop(cmd ) do
  begin
   add_vertex(x ,y ,cmd );

   cmd:=vs.vertex(@x ,@y );

  end;

end;

END.

