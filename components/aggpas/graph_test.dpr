//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 graph_test ;

uses
 SysUtils ,

 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,

 agg_ctrl ,
 agg_slider_ctrl ,
 agg_cbox_ctrl ,
 agg_rbox_ctrl ,

 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_renderer_primitives ,
 agg_rasterizer_scanline_aa ,
 agg_rasterizer_outline ,
 agg_scanline ,
 agg_scanline_p ,
 agg_scanline_u ,
 agg_render_scanlines ,

 agg_array ,
 agg_curves ,
 agg_ellipse ,
 agg_vertex_source ,
 agg_conv_stroke ,
 agg_conv_dash ,
 agg_conv_curve ,
 agg_conv_contour ,
 agg_conv_marker ,
 agg_conv_shorten_path ,
 agg_conv_marker_adaptor ,
 agg_conv_concat ,
 agg_arrowhead ,
 agg_vcgen_markers_term ,
 agg_span_allocator ,
 agg_span_gradient ,
 agg_span_interpolator_linear ,
 agg_gamma_functions ,
 agg_trans_affine ;

{$I agg_mode.inc }
{$I- }
const
 flip_y = true;

type
 base_renderer       = renderer_base;
 primitives_renderer = renderer_primitives;
 solid_renderer      = renderer_scanline_aa_solid;
 draft_renderer      = renderer_scanline_bin_solid;
 gradient_function   = gradient_radial_d;
 interpolator        = span_interpolator_linear;
 gradient_span_gen   = span_gradient;
 gradient_span_alloc = span_allocator;
 gradient_renderer   = renderer_scanline_aa;
 scanline_rasterizer = rasterizer_scanline_aa;
 outline_rasterizer  = rasterizer_outline;

 node_ptr = ^node;
 node = object
   x ,y : double;

   constructor Construct; overload;
   constructor Construct(x_ ,y_ : double ); overload;

  end;

 edge_ptr = ^edge;
 edge = object
   node1 ,node2 : int;

   constructor Construct; overload;
   constructor Construct(n1 ,n2 : int ); overload;

  end;

 graph = object
   m_num_nodes ,
   m_num_edges : int;

   m_nodes : node_ptr;
   m_edges : edge_ptr;

   constructor Construct(num_nodes ,num_edges : int );
   destructor  Destruct;

   function get_num_nodes : int;
   function get_num_edges : int;

   function get_node(idx : int; w ,h : double ) : node;
   function get_edge(idx : int ) : edge;

  end;

 line = object(vertex_source )
   x1 ,y1 ,x2 ,y2 : double;

   f : int;

   constructor Construct(x1_ ,y1_ ,x2_ ,y2_ : double );

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 curve = object(vertex_source )
   c : curve4;

   constructor Construct(x1 ,y1 ,x2 ,y2 : double; k : double = 0.5 );
   destructor  Destruct; virtual;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 stroke_draft_simple = object(vertex_source )
   s : vertex_source_ptr;

   constructor Construct(src : vertex_source_ptr );

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 stroke_draft_arrow = object(vertex_source )
   s  : conv_marker_adaptor;
   ah : arrowhead;
   m  : conv_marker;
   ma : vcgen_markers_term;
   c  : conv_concat;

   constructor Construct(src : vertex_source_ptr; w : double );
   destructor  Destruct; virtual;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 stroke_fine_simple = object(vertex_source )
   s : conv_stroke;

   constructor Construct(src : vertex_source_ptr; w : double );
   destructor  Destruct; virtual;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 stroke_fine_arrow = object(vertex_source )
   s  : conv_stroke;
   ah : arrowhead;
   m  : conv_marker;
   ma : vcgen_markers_term;
   c  : conv_concat;

   constructor Construct(src : vertex_source_ptr; w : double );
   destructor  Destruct; virtual;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 dash_stroke_draft_simple = object(vertex_source )
   d : conv_dash;

   constructor Construct(src : vertex_source_ptr; dash_len ,gap_len ,w : double );
   destructor  Destruct; virtual;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 dash_stroke_draft_arrow = object(vertex_source )
   d  : conv_dash;
   ah : arrowhead;
   m  : conv_marker;
   ma : vcgen_markers_term;
   c  : conv_concat;

   constructor Construct(src : vertex_source_ptr; dash_len ,gap_len ,w : double );
   destructor  Destruct; virtual;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 dash_stroke_fine_simple = object(vertex_source )
   d : conv_dash;
   s : conv_stroke;

   constructor Construct(src : vertex_source_ptr; dash_len ,gap_len ,w : double );
   destructor  Destruct; virtual;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 dash_stroke_fine_arrow = object(vertex_source )
   d  : conv_dash;
   s  : conv_stroke;
   ah : arrowhead;
   m  : conv_marker;
   ma : vcgen_markers_term;
   c  : conv_concat;

   constructor Construct(src : vertex_source_ptr; dash_len ,gap_len ,w : double );
   destructor  Destruct; virtual;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

{ stroke_draft      = stroke_draft_simple;
 dash_stroke_draft = dash_stroke_draft_simple;
 stroke_fine       = stroke_fine_simple;
 dash_stroke_fine  = dash_stroke_fine_simple;{}

 stroke_draft      = stroke_draft_arrow;
 dash_stroke_draft = dash_stroke_draft_arrow;
 stroke_fine       = stroke_fine_arrow;
 dash_stroke_fine  = dash_stroke_fine_arrow;{}

 the_application = object(platform_support )
   m_type  : rbox_ctrl;
   m_width : slider_ctrl;

   m_benchmark   ,
   m_draw_nodes  ,
   m_draw_edges  ,
   m_draft       ,
   m_translucent : cbox_ctrl;

   m_gradient_colors : pod_auto_array;

   m_graph : graph;
   m_draw  : int;
   m_sl    : scanline_u8;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure draw_nodes_draft;
   procedure draw_nodes_fine(ras : rasterizer_scanline_ptr );

   procedure render_edge_fine(
              ras : rasterizer_scanline_ptr;
              ren_fine  ,
              ren_draft : renderer_scanline_ptr;
              src : vertex_source_ptr );

   procedure draw_lines_draft;
   procedure draw_curves_draft;
   procedure draw_dashes_draft;

   procedure draw_lines_fine(
              ras : rasterizer_scanline_ptr;
              solid ,
              draft : renderer_scanline_ptr );

   procedure draw_curves_fine(
              ras : rasterizer_scanline_ptr;
              solid ,
              draft : renderer_scanline_ptr );

   procedure draw_dashes_fine(
              ras : rasterizer_scanline_ptr;
              solid ,
              draft : renderer_scanline_ptr );

   procedure draw_polygons(
              ras : rasterizer_scanline_ptr;
              solid ,
              draft : renderer_scanline_ptr );

   procedure draw_scene(
              ras : rasterizer_scanline_ptr;
              solid ,
              draft : renderer_scanline_ptr );

   procedure on_draw; virtual;
   procedure on_ctrl_change; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor node.Construct;
begin
 x:=0;
 y:=0;

end;

{ CONSTRUCT }
constructor node.Construct(x_ ,y_ : double );
begin
 x:=x_;
 y:=y_;

end;

{ CONSTRUCT }
constructor edge.Construct;
begin
 node1:=0;
 node2:=0;

end;

{ CONSTRUCT }
constructor edge.Construct(n1 ,n2 : int );
begin
 node1:=n1;
 node2:=n2;

end;

{ CONSTRUCT }
constructor graph.Construct;
var
 i : int;
 n : node_ptr;
 e : edge_ptr;

begin
 m_num_nodes:=num_nodes;
 m_num_edges:=num_edges;

 agg_getmem(pointer(m_nodes ) ,m_num_nodes * sizeof(node ) );
 agg_getmem(pointer(m_edges ) ,m_num_edges * sizeof(edge ) );

 RandSeed:=100;

 for i:=0 to m_num_nodes - 1 do
  begin
   n:=node_ptr(ptrcomp(m_nodes ) + i * sizeof(node ) );

   n.x:=(Random($7fff ) / $7fff ) * 0.75 + 0.2;
   n.y:=(Random($7fff ) / $7fff ) * 0.85 + 0.1;

  end;

 i:=0;

 while i < m_num_edges do
  begin
   e:=edge_ptr(ptrcomp(m_edges ) + i * sizeof(edge ) );

   e.node1:=Random($7fff ) mod m_num_nodes;
   e.node2:=Random($7fff ) mod m_num_nodes;

   if e.node1 = e.node2 then
    dec(i );

   inc(i );

  end;

end;

{ DESTRUCT }
destructor graph.Destruct;
begin
 agg_freemem(pointer(m_nodes ) ,m_num_nodes * sizeof(node ) );
 agg_freemem(pointer(m_edges ) ,m_num_edges * sizeof(edge ) );

end;

{ GET_NUM_NODES }
function graph.get_num_nodes;
begin
 result:=m_num_nodes;

end;

{ GET_NUM_EDGES }
function graph.get_num_edges;
begin
 result:=m_num_edges;

end;

{ GET_NODE }
function graph.get_node;
var
 p : node;

begin
 p.Construct;

 if idx < m_num_nodes then
  begin
   p:=node_ptr(ptrcomp(m_nodes ) + idx * sizeof(node ) )^;

   p.x:=p.x * w;
   p.y:=p.y * h;

  end;

 result:=p; 

end;

{ GET_EDGE }
function graph.get_edge;
var
 b : edge;

begin
 b.Construct;

 if idx < m_num_edges then
  b:=edge_ptr(ptrcomp(m_edges ) + idx * sizeof(edge ) )^;

 result:=b;

end;

{ CONSTRUCT }
constructor line.Construct;
begin
 x1:=x1_;
 y1:=y1_;
 x2:=x2_;
 y2:=y2_;
 f :=0;

end;

{ REWIND }
procedure line.rewind;
begin
 f:=0;

end;

{ VERTEX }
function line.vertex;
begin
 if f = 0 then
  begin
   inc(f );

   x^:=x1;
   y^:=y1;

   result:=path_cmd_move_to;

   exit;

  end;

 if f = 1 then
  begin
   inc(f );

   x^:=x2;
   y^:=y2;

   result:=path_cmd_line_to;

   exit;

  end;

 result:=path_cmd_stop;

end;

{ CONSTRUCT }
constructor curve.Construct;
begin
 c.Construct;
 c.init4(
  x1 ,y1 ,
  x1 - (y2 - y1 ) * k ,
  y1 + (x2 - x1 ) * k ,
  x2 + (y2 - y1 ) * k ,
  y2 - (x2 - x1 ) * k ,
  x2 ,y2 );
  
end;

{ DESTRUCT }
destructor curve.Destruct;
begin
 c.Destruct;

end;

{ REWIND }
procedure curve.rewind;
begin
 c.rewind(path_id );

end;

{ VERTEX }
function curve.vertex;
begin
 result:=c.vertex(x ,y );

end;

{ CONSTRUCT }
constructor stroke_draft_simple.Construct;
begin
 s:=src;

end;

{ REWIND }
procedure stroke_draft_simple.rewind;
begin
 s.rewind(path_id );

end;

{ VERTEX }
function stroke_draft_simple.vertex;
begin
 s.vertex(x ,y );

end;

{ CONSTRUCT }
constructor stroke_draft_arrow.Construct;
begin
 s.Construct(src );
 ma.Construct;
 s.set_markers(@ma );
 ah.Construct;
 m.Construct(s.markers ,@ah );
 c.Construct(@s ,@m );

 ah.head_  (0 ,10 ,5 ,0 );
 s.shorten_(10.0 );

end;

{ DESTRUCT }
destructor stroke_draft_arrow.Destruct;
begin
 s.Destruct;
 ma.Destruct;

end;

{ REWIND }
procedure stroke_draft_arrow.rewind;
begin
 c.rewind(path_id );

end;

{ VERTEX }
function stroke_draft_arrow.vertex;
begin
 result:=c.vertex(x ,y );

end;

{ CONSTRUCT }
constructor stroke_fine_simple.Construct;
begin
 s.Construct(src );
 s.width_   (w );

end;

{ DESTRUCT }
destructor stroke_fine_simple.Destruct;
begin
 s.Destruct;

end;

{ REWIND }
procedure stroke_fine_simple.rewind;
begin
 s.rewind(path_id );

end;

{ VERTEX }
function stroke_fine_simple.vertex;
begin
 result:=s.vertex(x ,y );

end;

{ CONSTRUCT }
constructor stroke_fine_arrow.Construct;
begin
 s.Construct(src );
 ma.Construct;
 s.set_markers(@ma );
 ah.Construct;
 m.Construct(s.markers ,@ah );
 c.Construct(@s ,@m );

 s.width_  (w );
 ah.head_  (0 ,10 ,5 ,0 );
 s.shorten_(w * 2.0 );

end;

{ DESTRUCT }
destructor stroke_fine_arrow.Destruct;
begin
 s.Destruct;
 ma.Destruct;

end;

{ REWIND }
procedure stroke_fine_arrow.rewind;
begin
 c.rewind(path_id );

end;

{ VERTEX }
function stroke_fine_arrow.vertex;
begin
 result:=c.vertex(x ,y );

end;

{ CONSTRUCT }
constructor dash_stroke_draft_simple.Construct;
begin
 d.Construct(src );
 d.add_dash (dash_len ,gap_len );

end;

{ DESTRUCT }
destructor dash_stroke_draft_simple.Destruct;
begin
 d.Destruct;

end;

{ REWIND }
procedure dash_stroke_draft_simple.rewind;
begin
 d.rewind(path_id );

end;

{ VERTEX }
function dash_stroke_draft_simple.vertex;
begin
 result:=d.vertex(x ,y );

end;

{ CONSTRUCT }
constructor dash_stroke_draft_arrow.Construct;
begin
 d.Construct(src );
 ma.Construct;
 d.set_markers(@ma );
 ah.Construct;
 m.Construct(d.markers ,@ah );
 c.Construct(@d ,@m );

 d.add_dash(dash_len ,gap_len );

 ah.head_  (0 ,10 ,5 ,0 );
 d.shorten_(10.0 );

end;

{ DESTRUCT }
destructor dash_stroke_draft_arrow.Destruct;
begin
 d.Destruct;
 ma.Destruct;

end;

{ REWIND }
procedure dash_stroke_draft_arrow.rewind;
begin
 c.rewind(path_id );

end;

{ VERTEX }
function dash_stroke_draft_arrow.vertex;
begin
 result:=c.vertex(x ,y );

end;

{ CONSTRUCT }
constructor dash_stroke_fine_simple.Construct;
begin
 d.Construct(src );
 s.Construct(@d );

 d.add_dash(dash_len ,gap_len );
 s.width_  (w );

end;

{ DESTRUCT }
destructor dash_stroke_fine_simple.Destruct;
begin
 d.Destruct;
 s.Destruct;

end;

{ REWIND }
procedure dash_stroke_fine_simple.rewind;
begin
 s.rewind(path_id );

end;

{ VERTEX }
function dash_stroke_fine_simple.vertex;
begin
 result:=s.vertex(x ,y );

end;

{ CONSTRUCT }
constructor dash_stroke_fine_arrow.Construct;
begin
 d.Construct(src );
 ma.Construct;
 d.set_markers(@ma );
 s.Construct(@d );
 ah.Construct;
 m.Construct(d.markers ,@ah );
 c.Construct(@s ,@m );

 d.add_dash(dash_len ,gap_len );
 s.width_  (w );
 ah.head_  (0 ,10 ,5 ,0 );
 d.shorten_(w * 2.0 );

end;

{ DESTRUCT }
destructor dash_stroke_fine_arrow.Destruct;
begin
 d.Destruct;
 s.Destruct;
 ma.Destruct;

end;

{ REWIND }
procedure dash_stroke_fine_arrow.rewind;
begin
 c.rewind(path_id );

end;

{ VERTEX }
function dash_stroke_fine_arrow.vertex;
begin
 result:=c.vertex(x ,y );

end;

{ CONSTRUCT }
constructor the_application.Construct;
var
 i : int;

 c1 ,c2 : aggclr;

begin
 inherited Construct(format_ ,flip_y_ );

 m_type.Construct (-1 ,-1 ,-1 ,-1 ,not flip_y_ );
 m_width.Construct(110 + 80 ,8.0 ,110 + 200.0 + 80 ,8.0 + 7.0 ,not flip_y_ );

 m_benchmark.Construct  (110 + 200 + 80 + 8 ,8.0 - 2.0 ,'Benchmark' ,not flip_y_ );
 m_draw_nodes.Construct (110 + 200 + 80 + 8 ,8.0 - 2.0 + 15.0 ,'Draw Nodes' ,not flip_y_ );
 m_draw_edges.Construct (200 + 200 + 80 + 8 ,8.0 - 2.0 + 15.0 ,'Draw Edges' ,not flip_y_ );
 m_draft.Construct      (200 + 200 + 80 + 8 ,8.0 - 2.0 ,'Draft Mode' ,not flip_y_ );
 m_translucent.Construct(110 + 80 ,8.0 - 2.0 + 15.0 ,'Translucent Mode' ,not flip_y_ );

 m_graph.Construct(200 ,100 );
 m_sl.Construct;

 m_draw:=3;

 m_gradient_colors.Construct(256 ,sizeof(aggclr ) );

 add_ctrl(@m_type );

 m_type.text_size_(8.0 );
 m_type.add_item  ('Solid lines' );
 m_type.add_item  ('Bezier curves' );
 m_type.add_item  ('Dashed curves' );
 m_type.add_item  ('Poygons AA' );
 m_type.add_item  ('Poygons Bin' );
 m_type.cur_item_ (0 );

 add_ctrl(@m_width );

 m_width.num_steps_(20 );
 m_width.range_    (0.0 ,5.0 );
 m_width.value_    (2.0 );
 m_width.label_    ('Width=%1.2f' );

 m_benchmark.text_size_ (8.0 );
 m_draw_nodes.text_size_(8.0 );
 m_draft.text_size_     (8.0 );
 m_draw_nodes.status_  (true );
 m_draw_edges.status_  (true );

 add_ctrl(@m_benchmark );
 add_ctrl(@m_draw_nodes );
 add_ctrl(@m_draw_edges );
 add_ctrl(@m_draft );
 add_ctrl(@m_translucent );

 c1.ConstrDbl(1 ,1 ,0 ,0.25 );
 c2.ConstrDbl(0 ,0 ,1 );

 for i:=0 to 255 do
  aggclr_ptr(
   m_gradient_colors.array_operator(i ) )^:=c1.gradient(@c2 ,i / 255.0 );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_type.Destruct;
 m_width.Destruct;

 m_benchmark.Destruct;
 m_draw_nodes.Destruct;
 m_draw_edges.Destruct;
 m_draft.Destruct;
 m_translucent.Destruct;

 m_graph.Destruct;
 m_sl.Destruct;
 m_gradient_colors.Destruct;

end;

{ DRAW_NODES_DRAFT }
procedure the_application.draw_nodes_draft;
var
 pixf : pixel_formats;

 rb   : base_renderer;
 prim : primitives_renderer;

 i : int;
 n : node;

begin
 pixfmt_bgr24  (pixf ,rbuf_window );
 rb.Construct  (@pixf );
 prim.Construct(@rb );

 i:=0;

 while i < m_graph.get_num_nodes do
  begin
   n:=m_graph.get_node(i ,_width ,_height );

   prim.fill_color_     (aggclr_ptr(m_gradient_colors.array_operator(147 ) ) );
   prim.line_color_     (aggclr_ptr(m_gradient_colors.array_operator(255 ) ) );
   prim.outlined_ellipse(trunc(n.x ) ,trunc(n.y ) ,10 ,10 );
   prim.fill_color_     (aggclr_ptr(m_gradient_colors.array_operator(50 ) ) );
   prim.solid_ellipse   (trunc(n.x ) ,trunc(n.y ) ,4 ,4 );

   inc(i );

  end;

end;

{ DRAW_NODES_FINE }
procedure the_application.draw_nodes_fine;
var
 pixf : pixel_formats;

 sa : gradient_span_alloc;
 rb : base_renderer;
 gf : gradient_function;
 sg : gradient_span_gen;

 i : int;
 n : node;

 ell : ellipse;
 mtx : trans_affine;
 tas : trans_affine_scaling;
 tat : trans_affine_translation;
 ren : gradient_renderer;

 inter : interpolator;

 x ,y : double;

begin
 sa.Construct;
 pixfmt_bgr24(pixf ,rbuf_window );
 rb.Construct(@pixf );

 i:=0;

 while i < m_graph.get_num_nodes do
  begin
   n:=m_graph.get_node(i ,_width ,_height );

   ell.Construct(n.x ,n.y ,5.0 * m_width._value ,5.0 * m_width._value );

   case m_draw of
    0 :
     begin
      ell.rewind(0);

      while not is_stop(ell.vertex(@x ,@y ) ) do;

     end;

    1 :
     begin
      ras.reset;
      ras.add_path(@ell );

     end;

    2 :
     begin
      ras.reset;
      ras.add_path(@ell );
      ras.sort;

     end;

    3 :
     begin
      gf.Construct;
      mtx.Construct;
      tas.Construct(m_width._value / 2.0 ); mtx.multiply(@tas );
      tat.Construct(n.x ,n.y ); mtx.multiply(@tat );
      mtx.invert;
      inter.Construct(@mtx );
      sg.Construct (@sa ,@inter ,@gf ,@m_gradient_colors ,0.0 ,10.0 );
      ren.Construct(@rb ,@sg );

      ras.add_path    (@ell);
      render_scanlines(ras ,@m_sl ,@ren );

     end;

   end;

   inc(i );

  end;

 sa.Destruct;

end;

{ RENDER_EDGE_FINE }
procedure the_application.render_edge_fine;
var
 rgba : aggclr;
 x ,y : double;

 r ,g ,b ,a : int;

begin
 case m_draw of
  0 :
   begin
    src.rewind(0 );

    while not is_stop(src.vertex(@x ,@y ) ) do;

   end;

  1 :
   begin
    ras.reset;
    ras.add_path(src );

   end;

  2 :
   begin
    ras.reset;
    ras.add_path(src );
    ras.sort;

   end;

  3 :
   begin
    r:=Random($7fff ) and $7F;
    g:=Random($7fff ) and $7F;
    b:=Random($7fff ) and $7F;
    a:=255;

    if m_translucent._status then
     a:=80;

    ras.add_path(src );

    if m_type._cur_item < 4 then
     begin
      rgba.ConstrInt  (r ,g ,b ,a );
      ren_fine.color_ (@rgba );
      render_scanlines(ras ,@m_sl ,ren_fine );

     end
    else
     begin
      rgba.ConstrInt  (r ,g ,b ,a );
      ren_draft.color_(@rgba );
      render_scanlines(ras ,@m_sl ,ren_draft );

     end;

   end;

 end;

end;

{ DRAW_LINES_DRAFT }
procedure the_application.draw_lines_draft;
var
 pixf : pixel_formats;
 prim : primitives_renderer;
 rgba : aggclr;

 rb  : base_renderer;
 ras : outline_rasterizer;

 i ,r ,g ,b ,a : int;

 l  : line;
 s  : stroke_draft;
 e  : edge;
 n1 ,
 n2 : node;

begin
 pixfmt_bgr24(pixf ,rbuf_window );

 rb.Construct  (@pixf );
 prim.Construct(@rb );
 ras.Construct (@prim );

 i:=0;

 while i < m_graph.get_num_edges do
  begin
   e :=m_graph.get_edge(i );
   n1:=m_graph.get_node(e.node1 ,_width ,_height );
   n2:=m_graph.get_node(e.node2 ,_width ,_height );

   l.Construct(n1.x ,n1.y ,n2.x ,n2.y );
   s.Construct(@l ,m_width._value );

   r:=Random($7fff ) and $7F;
   g:=Random($7fff ) and $7F;
   b:=Random($7fff ) and $7F;
   a:=255;

   if m_translucent._status then
    a:=80;

   rgba.ConstrInt  (r ,g ,b ,a );
   prim.line_color_(@rgba );
   ras.add_path    (@s );

   s.Destruct;

   inc(i );

  end;

end;

{ DRAW_CURVES_DRAFT }
procedure the_application.draw_curves_draft;
var
 pixf : pixel_formats;
 prim : primitives_renderer;
 rgba : aggclr;

 rb  : base_renderer;
 ras : outline_rasterizer;

 i ,r ,g ,b ,a : int;

 c  : curve;
 s  : stroke_draft;
 e  : edge;
 n1 ,
 n2 : node;

begin
 pixfmt_bgr24(pixf ,rbuf_window );

 rb.Construct  (@pixf );
 prim.Construct(@rb );
 ras.Construct (@prim );

 i:=0;

 while i < m_graph.get_num_edges do
  begin
   e :=m_graph.get_edge(i );
   n1:=m_graph.get_node(e.node1 ,_width ,_height );
   n2:=m_graph.get_node(e.node2 ,_width ,_height );

   c.Construct(n1.x ,n1.y ,n2.x ,n2.y );
   s.Construct(@c ,m_width._value );

   r:=Random($7fff ) and $7F;
   g:=Random($7fff ) and $7F;
   b:=Random($7fff ) and $7F;
   a:=255;

   if m_translucent._status then
    a:=80;

   rgba.ConstrInt  (r ,g ,b ,a );
   prim.line_color_(@rgba );
   ras.add_path    (@s );

   c.Destruct;
   s.Destruct;

   inc(i );

  end;

end;

{ DRAW_DASHES_DRAFT }
procedure the_application.draw_dashes_draft;
var
 pixf : pixel_formats;
 prim : primitives_renderer;
 rgba : aggclr;

 rb  : base_renderer;
 ras : outline_rasterizer;

 i ,r ,g ,b ,a : int;

 c  : curve;
 s  : dash_stroke_draft;
 e  : edge;
 n1 ,
 n2 : node;

begin
 pixfmt_bgr24(pixf ,rbuf_window );

 rb.Construct  (@pixf );
 prim.Construct(@rb );
 ras.Construct (@prim );

 i:=0;

 while i < m_graph.get_num_edges do
  begin
   e :=m_graph.get_edge(i );
   n1:=m_graph.get_node(e.node1 ,_width ,_height );
   n2:=m_graph.get_node(e.node2 ,_width ,_height );

   c.Construct(n1.x ,n1.y ,n2.x ,n2.y );
   s.Construct(@c ,6.0 ,3.0 ,m_width._value );

   r:=Random($7fff ) and $7F;
   g:=Random($7fff ) and $7F;
   b:=Random($7fff ) and $7F;
   a:=255;

   if m_translucent._status then
    a:=80;

   rgba.ConstrInt  (r ,g ,b ,a );
   prim.line_color_(@rgba );
   ras.add_path    (@s );

   c.Destruct;
   s.Destruct;

   inc(i );

  end;

end;

{ DRAW_LINES_FINE }
procedure the_application.draw_lines_fine;
var
 i : int;
 b : edge;
 l : line;
 s : stroke_fine;

 n1 ,n2 : node;

begin
 i:=0;

 while i < m_graph.get_num_edges do
  begin
   b :=m_graph.get_edge(i );
   n1:=m_graph.get_node(b.node1 ,_width ,_height );
   n2:=m_graph.get_node(b.node2 ,_width ,_height );

   l.Construct(n1.x ,n1.y ,n2.x ,n2.y );
   s.Construct(@l ,m_width._value );

   render_edge_fine(ras ,solid ,draft ,@s );

   s.Destruct;

   inc(i );

  end;

end;

{ DRAW_CURVES_FINE }
procedure the_application.draw_curves_fine;
var
 i : int;
 b : edge;
 c : curve;
 s : stroke_fine;

 n1 ,n2 : node;

begin
 i:=0;

 while i < m_graph.get_num_edges do
  begin
   b :=m_graph.get_edge(i );
   n1:=m_graph.get_node(b.node1 ,_width ,_height );
   n2:=m_graph.get_node(b.node2 ,_width ,_height );

   c.Construct(n1.x ,n1.y ,n2.x ,n2.y );
   s.Construct(@c ,m_width._value );

   render_edge_fine(ras ,solid ,draft ,@s );

   c.Destruct;
   s.Destruct;

   inc(i );

  end;

end;

{ DRAW_DASHES_FINE }
procedure the_application.draw_dashes_fine;
var
 i : int;
 b : edge;
 c : curve;
 s : dash_stroke_fine;

 n1 ,n2 : node;

begin
 i:=0;

 while i < m_graph.get_num_edges do
  begin
   b :=m_graph.get_edge(i );
   n1:=m_graph.get_node(b.node1 ,_width ,_height );
   n2:=m_graph.get_node(b.node2 ,_width ,_height );

   c.Construct(n1.x ,n1.y ,n2.x ,n2.y );
   s.Construct(@c ,6.0 ,3.0 ,m_width._value );

   render_edge_fine(ras ,solid ,draft ,@s );

   c.Destruct;
   s.Destruct;

   inc(i );

  end;

end;

{ DRAW_POLYGONS }
procedure the_application.draw_polygons;
var
 i : int;
 b : edge;
 c : curve;

 n1 ,n2 : node;

 gm_no : gamma_none;
 gm_th : gamma_threshold;

begin
 if m_type._cur_item = 4 then
  begin
   gm_th.Construct(0.5 );
   ras.gamma      (@gm_th );

  end;

 i:=0;

 while i < m_graph.get_num_edges do
  begin
   b :=m_graph.get_edge(i );
   n1:=m_graph.get_node(b.node1 ,_width ,_height );
   n2:=m_graph.get_node(b.node2 ,_width ,_height );

   c.Construct(n1.x ,n1.y ,n2.x ,n2.y );

   render_edge_fine(ras ,solid ,draft ,@c );

   c.Destruct;

   inc(i );

  end;

 gm_no.Construct;
 ras.gamma(@gm_no );

end;

{ DRAW_SCENE }
procedure the_application.draw_scene;
var
 gm_no : gamma_none;

begin
 gm_no.Construct;
 ras.gamma(@gm_no );

 RandSeed:=100;

 if m_draw_nodes._status then
  if m_draft._status then
   draw_nodes_draft
  else
   draw_nodes_fine(ras );

 if m_draw_edges._status then
   if m_draft._status then
    case m_type._cur_item of
     0 : draw_lines_draft;
     1 : draw_curves_draft;
     2 : draw_dashes_draft;

    end
   else
    case m_type._cur_item of
     0 :
      draw_lines_fine(ras ,solid ,draft );

     1 :
      draw_curves_fine(ras ,solid ,draft );

     2 :
      draw_dashes_fine(ras ,solid ,draft );

     3 ,4 :
      draw_polygons(ras ,solid ,draft );

    end;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;
 rgba : aggclr;

 ras : scanline_rasterizer;
 rb  : base_renderer;

 solid : solid_renderer;
 draft : draft_renderer;

begin
// Initialize structures
 ras.Construct;

 pixfmt_bgr24(pixf ,rbuf_window );

 rb.Construct   (@pixf );
 solid.Construct(@rb );
 draft.Construct(@rb );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear      (@rgba );

// Render
 draw_scene(@ras ,@solid ,@draft );

// Render the controls
 ras.filling_rule(fill_non_zero );

 render_ctrl(@ras ,@m_sl ,@solid ,@m_type );
 render_ctrl(@ras ,@m_sl ,@solid ,@m_width );
 render_ctrl(@ras ,@m_sl ,@solid ,@m_benchmark );
 render_ctrl(@ras ,@m_sl ,@solid ,@m_draw_nodes );
 render_ctrl(@ras ,@m_sl ,@solid ,@m_draw_edges );
 render_ctrl(@ras ,@m_sl ,@solid ,@m_draft );
 render_ctrl(@ras ,@m_sl ,@solid ,@m_translucent );

// Free AGG resources
 ras.Destruct;

end;

{ ON_CTRL_CHANGE }
procedure the_application.on_ctrl_change;
var
 pixf : pixel_formats;

 i  : int;
 fd : text;

 ras : scanline_rasterizer;
 rb  : base_renderer;

 solid : solid_renderer;
 draft : draft_renderer;

 buf   : array[0..255 ] of char;
 times : array[0..4 ] of double;

begin
 if m_benchmark._status then
  begin
   on_draw;
   update_window;

   ras.Construct;

   pixfmt_bgr24(pixf ,rbuf_window );

   rb.Construct   (@pixf );
   solid.Construct(@rb );
   draft.Construct(@rb );

   if m_draft._status then
    begin
     start_timer;

     for i:=0 to 9 do
      draw_scene(@ras ,@solid ,@draft );

     sprintf(@buf[0 ] ,'%3.3f milliseconds' ,elapsed_time );

    end
   else
    begin
     m_draw:=0;

     while m_draw < 4 do
      begin
       start_timer;

       for i:=0 to 9  do
        draw_scene(@ras ,@solid ,@draft );

       times[m_draw ]:=elapsed_time;

       inc(m_draw );

      end;

     m_draw:=3;

     times[4 ]:=times[3 ];
     times[3 ]:=times[3 ] - times[2 ];
     times[2 ]:=times[2 ] - times[1 ];
     times[1 ]:=times[1 ] - times[0 ];

     AssignFile(fd ,'benchmark' );
     rewrite   (fd );

     sprintf(@buf[0 ]             ,'%10.3f ' ,times[0 ] );
     sprintf(@buf[StrLen(@buf ) ] ,'%10.3f ' ,times[1 ] );
     sprintf(@buf[StrLen(@buf ) ] ,'%10.3f ' ,times[2 ] );
     sprintf(@buf[StrLen(@buf ) ] ,'%10.3f ' ,times[3 ] );
     sprintf(@buf[StrLen(@buf ) ] ,'%10.3f'  ,times[4 ] );

     writeln(fd ,PChar(@buf[0 ] ) );
     close  (fd );

     sprintf(@buf[0 ]             ,'       pipeline         add_path            sort            render           total'#13'%10.3f ' ,times[0 ] );
     sprintf(@buf[StrLen(@buf ) ] ,'%10.3f ' ,times[1 ] );
     sprintf(@buf[StrLen(@buf ) ] ,'%10.3f ' ,times[2 ] );
     sprintf(@buf[StrLen(@buf ) ] ,'%10.3f ' ,times[3 ] );
     sprintf(@buf[StrLen(@buf ) ] ,'%10.3f'  ,times[4 ] );

    end;

   message_(@buf[0 ] );

   m_benchmark.status_(false );
   force_redraw;

   ras.Destruct;

  end;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'Yet another example of the "general" kind. It was used mostly to compare '#13 +
   'the performance of different steps of rendering in order to see the weaknesses. '#13 +
   'The WIn GDI+ analog of it looks worse and works slower. Try "GDI_graph_test.zip"'#13 +
   '(from www.antigrain.com/demo) and compare it with the AGG one. '#13 +
   'The most disappointing thing in GDI+ is that it cannot draw Bezier curves correctly. '#13#13 +
   'How to play with:'#13#13 +
   'Run the GDI+ example, choose menu Image/Bezier curves, expand the window to '#13 +
   'about 1000x1000 pixels, and then gradually change the size of the window. You will '#13 +
   'see that some curves miss the destination points (the centers of the node circles).' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. Line Join (F1-Help)' );

 if app.init(600 + 100 ,500 + 30 ,window_resize ) then
  app.run;

 app.Destruct;

END.