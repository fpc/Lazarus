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
// 24.04.2006-Milano: Unit port establishment
//
{ agg_svg_path_renderer.pas }
unit
 agg_svg_path_renderer ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_path_storage ,
 agg_conv_transform ,
 agg_conv_stroke ,
 agg_conv_contour ,
 agg_conv_curve ,
 agg_color ,
 agg_array ,
 agg_bounding_rect ,
 agg_rasterizer_scanline_aa ,
 agg_vertex_source ,
 agg_svg_path_tokenizer ,
 agg_svg_exception ,
 agg_trans_affine ,
 agg_math_stroke ,
 agg_scanline ,
 agg_renderer_scanline ,
 agg_render_scanlines ;

{ TYPES DEFINITION }
type
 conv_count = object(vertex_source )
   m_source : vertex_source_ptr;
   m_count  : unsigned;

   constructor Construct(vs : vertex_source_ptr );

   procedure count_(n : unsigned );
   function  _count : unsigned;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

// Basic path attributes
 path_attributes_ptr = ^path_attributes;
 path_attributes = object
   index : unsigned;

   fill_color    ,
   stroke_color  : aggclr;
   fill_flag     ,
   stroke_flag   ,
   even_odd_flag : boolean;

   line_join     ,
   line_cap      : unsigned;
   miter_limit   ,
   stroke_width  : double;

   transform : trans_affine;

  // Empty constructor
   constructor Construct; overload;

  // Copy constructor
   constructor Construct(attr : path_attributes_ptr ); overload;

  // Copy constructor with new index value
   constructor Construct(attr : path_attributes_ptr; idx : unsigned ); overload;

  end;

// Path container and renderer.
 path_renderer_ptr = ^path_renderer;
 path_renderer = object(unsigned_list )
   m_storage      : path_storage;
   m_attr_storage ,
   m_attr_stack   : pod_deque;
   m_transform    : trans_affine;

   m_curved       : conv_curve;
   m_curved_count : conv_count;

   m_curved_stroked       : conv_stroke;
   m_curved_stroked_trans : conv_transform;

   m_curved_trans         : conv_transform;
   m_curved_trans_contour : conv_contour;

   constructor Construct;
   destructor  Destruct;

   procedure remove_all;

  // Use these functions as follows:
  // begin_path when the XML tag <path> comes ("start_element" handler)
  // parse_path on "d=" tag attribute
  // end_path when parsing of the entire tag is done.
   procedure begin_path;
   procedure parse_path(tok : path_tokenizer_ptr );
   procedure end_path;

  // The following functions are essentially a "reflection" of
  // the respective SVG path commands.
   procedure move_to (x ,y : double; rel : boolean = false ); // M, m
   procedure line_to (x ,y : double; rel : boolean = false ); // L, l
   procedure hline_to(x : double; rel : boolean = false );    // H, h
   procedure vline_to(y : double; rel : boolean = false );    // V, v

   procedure curve3(x1 ,y1 ,x ,y : double; rel : boolean = false ); overload;         // Q, q
   procedure curve3(x ,y : double; rel : boolean = false ); overload;                 // T, t
   procedure curve4(x1 ,y1 ,x2 ,y2 ,x ,y : double; rel : boolean = false ); overload; // C, c
   procedure curve4(x2 ,y2 ,x ,y : double; rel : boolean = false ); overload;         // S, s

   procedure close_subpath; // Z, z

   procedure add_path(vs : vertex_source_ptr; path_id : unsigned = 0; solid_path : boolean = true );
   function  vertex_count : unsigned;

  // Call these functions on <g> tag (start_element, end_element respectively)
   procedure push_attr;
   procedure pop_attr;

  // Attribute setting functions
   procedure fill        (f : aggclr_ptr );
   procedure stroke      (s : aggclr_ptr );
   procedure even_odd    (flag : boolean );
   procedure stroke_width(w : double );

   procedure fill_none;
   procedure stroke_none;

   procedure fill_opacity  (op : double );
   procedure stroke_opacity(op : double );
   procedure line_join     (join : unsigned );
   procedure line_cap      (cap : unsigned );
   procedure miter_limit   (ml : double );

   function  transform : trans_affine_ptr;

  // Make all polygons CCW-oriented
   procedure arrange_orientations;

  // Expand all polygons
   procedure expand(value : double );

   function  array_operator(idx : unsigned ) : unsigned; virtual;
   procedure bounding_rect (x1 ,y1 ,x2 ,y2 : double_ptr );

  // Rendering. One can specify two additional parameters:
  // trans_affine and opacity. They can be used to transform the whole
  // image and/or to make it translucent.
   procedure render(
              ras : rasterizer_scanline_ptr;
              sl  : scanline_ptr;
              ren : renderer_scanline_ptr;
              mtx : trans_affine_ptr;
              cb  : rect_ptr;
              opacity : double = 1.0 );

  // Private
   function  cur_attr : path_attributes_ptr;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor conv_count.Construct;
begin
 m_source:=vs;
 m_count :=0;

end;

{ COUNT_ }
procedure conv_count.count_;
begin
 m_count:=n;

end;

{ _COUNT }
function conv_count._count;
begin
 result:=m_count;

end;

{ REWIND }
procedure conv_count.rewind;
begin
 m_source.rewind(path_id );

end;

{ VERTEX }
function conv_count.vertex;
begin
 inc(m_count );

 result:=m_source.vertex(x ,y );

end;

{ CONSTRUCT }
constructor path_attributes.Construct;
begin
 index:=0;

 fill_color.ConstrInt  (0 ,0 ,0 );
 stroke_color.ConstrInt(0 ,0 ,0 );

 fill_flag    :=true;
 stroke_flag  :=false;
 even_odd_flag:=false;
 line_join    :=miter_join;
 line_cap     :=butt_cap;
 miter_limit  :=4.0;
 stroke_width :=1.0;

 transform.Construct;

end;

{ CONSTRUCT }
constructor path_attributes.Construct(attr : path_attributes_ptr );
begin
 index:=attr.index;

 fill_color.Construct  (@attr.fill_color );
 stroke_color.Construct(@attr.stroke_color );

 fill_flag    :=attr.fill_flag;
 stroke_flag  :=attr.stroke_flag;
 even_odd_flag:=attr.even_odd_flag;
 line_join    :=attr.line_join;
 line_cap     :=attr.line_cap;
 miter_limit  :=attr.miter_limit;
 stroke_width :=attr.stroke_width;

 transform.Construct;
 transform.assign_all(@attr.transform );

end;

{ CONSTRUCT }
constructor path_attributes.Construct(attr : path_attributes_ptr; idx : unsigned );
begin
 index:=idx;

 fill_color.Construct  (@attr.fill_color );
 stroke_color.Construct(@attr.stroke_color );

 fill_flag    :=attr.fill_flag;
 stroke_flag  :=attr.stroke_flag;
 even_odd_flag:=attr.even_odd_flag;
 line_join    :=attr.line_join;
 line_cap     :=attr.line_cap;
 miter_limit  :=attr.miter_limit;
 stroke_width :=attr.stroke_width;

 transform.Construct;
 transform.assign_all(@attr.transform );

end;

{ CONSTRUCT }
constructor path_renderer.Construct;
begin
 m_storage.Construct;
 m_attr_storage.Construct(sizeof(path_attributes ) );
 m_attr_stack.Construct(sizeof(path_attributes ) );
 m_transform.Construct;

 m_curved.Construct      (@m_storage );
 m_curved_count.Construct(@m_curved );

 m_curved_stroked.Construct      (@m_curved_count );
 m_curved_stroked_trans.Construct(@m_curved_stroked ,@m_transform );

 m_curved_trans.Construct        (@m_curved_count ,@m_transform );
 m_curved_trans_contour.Construct(@m_curved_trans );

end;

{ DESTRUCT }
destructor path_renderer.Destruct;
begin
 m_curved_trans_contour.Destruct;
 m_curved_stroked.Destruct;
 m_curved.Destruct;

 m_attr_stack.Destruct;
 m_attr_storage.Destruct;
 m_storage.Destruct;

end;

{ REMOVE_ALL }
procedure path_renderer.remove_all;
begin
 m_storage.remove_all;
 m_attr_storage.remove_all;
 m_attr_stack.remove_all;
 m_transform.reset;

end;

{ BEGIN_PATH }
procedure path_renderer.begin_path;
var
 idx  : unsigned;
 attr : path_attributes;

begin
 push_attr;

 idx:=m_storage.start_new_path;

 attr.Construct(cur_attr ,idx );

 m_attr_storage.add(@attr );

end;

{ PARSE_PATH }
procedure path_renderer.parse_path;
var
 arg : array[0..9 ] of double;
 i   : unsigned;
 cmd : char;
 buf : array[0..99 ] of char;

begin
 while tok.next do
  begin
   cmd:=tok.last_command;

   case cmd of
    'M' ,'m' :
     begin
      arg[0 ]:=tok.last_number;
      arg[1 ]:=tok.next(cmd );

      move_to(arg[0 ] ,arg[1 ] ,cmd = 'm' );

     end;

    'L' ,'l' :
     begin
      arg[0 ]:=tok.last_number;
      arg[1 ]:=tok.next(cmd );

      line_to(arg[0 ] ,arg[1 ] ,cmd = 'l' );

     end;

    'V' ,'v' :
     vline_to(tok.last_number ,cmd = 'v' );

    'H' ,'h' :
     hline_to(tok.last_number ,cmd = 'h' );

    'Q' ,'q' :
     begin
      arg[0 ]:=tok.last_number;

      for i:=1 to 3 do
       arg[i ]:=tok.next(cmd );

      curve3(arg[0 ] ,arg[1 ] ,arg[2 ] ,arg[3 ] ,cmd = 'q' );

     end;

    'T' ,'t' :
     begin
      arg[0 ]:=tok.last_number;
      arg[1 ]:=tok.next(cmd );

      curve3(arg[0 ] ,arg[1 ] ,cmd = 't' );

     end;

    'C' ,'c' :
     begin
      arg[0 ]:=tok.last_number;

      for i:=1 to 5 do
       arg[i ]:=tok.next(cmd );

      curve4(arg[0 ] ,arg[1 ] ,arg[2 ] ,arg[3 ] ,arg[4 ] ,arg[5 ] ,cmd = 'c' );

     end;

    'S' ,'s' :
     begin
      arg[0 ]:=tok.last_number;

      for i:=1 to 3 do
       arg[i ]:=tok.next(cmd );

      curve4(arg[0 ] ,arg[1 ] ,arg[2 ] ,arg[3 ] ,cmd = 's' );

     end;

    'A' ,'a' :
     raise svg_exception.Construct(PChar('parse_path: Command A: NOT IMPLEMENTED YET' ) );

    'Z' ,'z' :
     close_subpath;

    else
     begin
      sprintf(@buf[0 ] ,'parse_path: Invalid Command %c' ,unsigned(cmd ) );

      raise svg_exception.Construct(PChar(@buf[0 ] ) );

     end;

   end;

  end;

end;

{ END_PATH }
procedure path_renderer.end_path;
var
 idx  : unsigned;
 attr : path_attributes;

begin
 if m_attr_storage.size = 0 then
  raise svg_exception(PChar('end_path : The path was not begun' ) );

 attr.Construct(cur_attr );

 idx       :=path_attributes_ptr(m_attr_storage.array_operator(m_attr_storage.size - 1 ) ).index;
 attr.index:=idx;

 move(
  pointer(@attr )^ ,
  path_attributes_ptr(m_attr_storage.array_operator(m_attr_storage.size - 1 ) )^ ,
  sizeof(path_attributes ) );

 pop_attr;

end;

{ MOVE_TO }
procedure path_renderer.move_to;
begin
 if rel then
  m_storage.rel_to_abs(@x ,@y );

 m_storage.move_to(x ,y );

end;

{ LINE_TO }
procedure path_renderer.line_to;
begin
 if rel then
  m_storage.rel_to_abs(@x ,@y );

 m_storage.line_to(x ,y );

end;

{ HLINE_TO }
procedure path_renderer.hline_to;
var
 x2 ,y2 : double;

begin
 x2:=0.0;
 y2:=0.0;

 if m_storage.total_vertices <> 0 then
  begin
   m_storage.vertex_(m_storage.total_vertices - 1 ,@x2 ,@y2 );

   if rel then
    x:=x + x2;

   m_storage.line_to(x ,y2 );

  end;

end;

{ VLINE_TO }
procedure path_renderer.vline_to;
var
 x2 ,y2 : double;

begin
 x2:=0.0;
 y2:=0.0;

 if m_storage.total_vertices <> 0 then
  begin
   m_storage.vertex_(m_storage.total_vertices - 1 ,@x2 ,@y2 );

   if rel then
    y:=y + y2;

   m_storage.line_to(x2 ,y );

  end;

end;

{ CURVE3 }
procedure path_renderer.curve3(x1 ,y1 ,x ,y : double; rel : boolean = false );
begin
 if rel then
  begin
   m_storage.rel_to_abs(@x1 ,@y1 );
   m_storage.rel_to_abs(@x  ,@y );

  end;

 m_storage.curve3(x1 ,y1 ,x ,y );

end;

{ CURVE3 }
procedure path_renderer.curve3(x ,y : double; rel : boolean = false );
begin
// raise exception("curve3(x, y) : NOT IMPLEMENTED YET");
 if rel then
  m_storage.curve3_rel(x ,y )
 else
  m_storage.curve3(x ,y );

end;

{ CURVE4 }
procedure path_renderer.curve4(x1 ,y1 ,x2 ,y2 ,x ,y : double; rel : boolean = false );
begin
 if rel then
  begin
   m_storage.rel_to_abs(@x1 ,@y1 );
   m_storage.rel_to_abs(@x2 ,@y2 );
   m_storage.rel_to_abs(@x  ,@y );

  end;

 m_storage.curve4(x1 ,y1 ,x2 ,y2 ,x ,y );

end;

{ CURVE4 }
procedure path_renderer.curve4(x2 ,y2 ,x ,y : double; rel : boolean = false );
begin
//throw exception("curve4(x2, y2, x, y) : NOT IMPLEMENTED YET");
 if rel then
  m_storage.curve4_rel(x2 ,y2 ,x ,y )
 else
  m_storage.curve4(x2 ,y2 ,x ,y );

end;

{ CLOSE_SUBPATH }
procedure path_renderer.close_subpath;
begin
 m_storage.end_poly(path_flags_close );

end;

{ ADD_PATH }
procedure path_renderer.add_path;
begin
 m_storage.add_path(vs ,path_id ,solid_path );

end;

{ VERTEX_COUNT }
function path_renderer.vertex_count;
begin
 result:=m_curved_count._count;

end;

{ PUSH_ATTR }
procedure path_renderer.push_attr;
var
 attr : path_attributes;

begin
 if m_attr_stack.size <> 0 then
  m_attr_stack.add(
   m_attr_stack.array_operator(m_attr_stack.size - 1 ) )
 else
  begin
   attr.Construct;

   m_attr_stack.add(@attr );

  end;

end;

{ POP_ATTR }
procedure path_renderer.pop_attr;
begin
 if m_attr_stack.size = 0 then
  raise svg_exception(PChar('pop_attr : Attribute stack is empty' ) );

 m_attr_stack.remove_last;

end;

{ FILL }
procedure path_renderer.fill;
var
 attr : path_attributes_ptr;

begin
 attr:=cur_attr;

 attr.fill_color:=f^;
 attr.fill_flag :=true;

end;

{ STROKE }
procedure path_renderer.stroke;
var
 attr : path_attributes_ptr;

begin
 attr:=cur_attr;

 attr.stroke_color:=s^;
 attr.stroke_flag :=true;

end;

{ EVEN_ODD }
procedure path_renderer.even_odd;
begin
 cur_attr.even_odd_flag:=flag;

end;

{ STROKE_WIDTH }
procedure path_renderer.stroke_width;
var
 attr : path_attributes_ptr;

begin
 attr:=cur_attr;

 attr.stroke_width:=w;
 attr.stroke_flag :=true;

end;

{ FILL_NONE }
procedure path_renderer.fill_none;
begin
 cur_attr.fill_flag:=false;

end;

{ STROKE_NONE }
procedure path_renderer.stroke_none;
begin
 cur_attr.stroke_flag:=false;

end;

{ FILL_OPACITY }
procedure path_renderer.fill_opacity;
begin
 cur_attr.fill_color.opacity_(op );

end;

{ STROKE_OPACITY }
procedure path_renderer.stroke_opacity;
begin
 cur_attr.stroke_color.opacity_(op );

end;

{ LINE_JOIN }
procedure path_renderer.line_join;
begin
 cur_attr.line_join:=join;

end;

{ LINE_CAP }
procedure path_renderer.line_cap;
begin
 cur_attr.line_cap:=cap;

end;

{ MITER_LIMIT }
procedure path_renderer.miter_limit;
begin
 cur_attr.miter_limit:=ml;

end;

{ TRANSFORM }
function path_renderer.transform;
begin
 result:=@cur_attr.transform;

end;

{ ARRANGE_ORIENTATIONS }
procedure path_renderer.arrange_orientations;
begin
 m_storage.arrange_orientations_all_paths(path_flags_ccw );

end;

{ EXPAND }
procedure path_renderer.expand;
begin
 m_curved_trans_contour.width_(value );

end;

{ ARRAY_OPERATOR }
function path_renderer.array_operator;
begin
 m_transform.assign_all(
  @path_attributes_ptr(
   m_attr_storage.array_operator(idx ) ).transform );

 result:=path_attributes_ptr(m_attr_storage.array_operator(idx ) ).index;

end;

{ BOUNDING_RECT }
procedure path_renderer.bounding_rect;
var
 trans : conv_transform;

begin
 trans.Construct(@m_storage ,@m_transform );

 bounding_rect_ul(
  @trans ,@self ,0 ,m_attr_storage.size ,x1 ,y1 ,x2 ,y2 );

end;

{ RENDER }
procedure path_renderer.render;
var
 i : unsigned;

 scl : double;

 attr : path_attributes_ptr;

 color : aggclr;

begin
 ras.clip_box         (cb.x1 ,cb.y1 ,cb.x2 ,cb.y2 );
 m_curved_count.count_(0 );

 i:=0;

 while i < m_attr_storage.size do
  begin
   attr:=path_attributes_ptr(m_attr_storage.array_operator(i ) );

   m_transform.assign_all(@attr.transform );
   m_transform.multiply  (mtx );

   scl:=m_transform.scale;

   //m_curved.approximation_method(curve_inc );

   m_curved.approximation_scale_(scl );
   m_curved.angle_tolerance_    (0.0 );

   color.Construct;

   if attr.fill_flag then
    begin
     ras.reset;

     if attr.even_odd_flag then
      ras.filling_rule(fill_even_odd )
     else
      ras.filling_rule(fill_non_zero );

     if Abs(m_curved_trans_contour._width ) < 0.0001 then
      ras.add_path(@m_curved_trans ,attr.index )
     else
      begin
       m_curved_trans_contour.miter_limit_(attr.miter_limit );

       ras.add_path(@m_curved_trans_contour ,attr.index );

      end;

     color:=attr.fill_color;

     color.opacity_  (color._opacity * opacity );
     ren.color_      (@color );
     render_scanlines(ras ,sl ,ren );

    end;

   if attr.stroke_flag then
    begin
     m_curved_stroked.width_(attr.stroke_width );

     //m_curved_stroked.line_join_((attr.line_join == miter_join) ? miter_join_round : attr.line_join);

     m_curved_stroked.line_join_          (attr.line_join );
     m_curved_stroked.line_cap_           (attr.line_cap );
     m_curved_stroked.miter_limit_        (attr.miter_limit );
     m_curved_stroked.approximation_scale_(scl );

     // If the *visual* line width is considerable we
     // turn on processing of curve cusps.
     //---------------------
      if attr.stroke_width * scl > 1.0 then
       m_curved.angle_tolerance_(0.2 );

     ras.reset;
     ras.filling_rule(fill_non_zero );
     ras.add_path    (@m_curved_stroked_trans ,attr.index );

     color:=attr.stroke_color;

     color.opacity_  (color._opacity * opacity);
     ren.color_      (@color );
     render_scanlines(ras ,sl ,ren );

    end;

   inc(i );

  end;

end;

{ CUR_ATTR }
function path_renderer.cur_attr;
begin
 if m_attr_stack.size = 0 then
  raise svg_exception.Construct(PChar('cur_attr : Attribute stack is empty' ) );

 result:=
  path_attributes_ptr(
   m_attr_stack.array_operator(m_attr_stack.size - 1 ) );

end;

END.

