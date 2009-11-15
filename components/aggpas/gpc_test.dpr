{target:win}
//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 gpc_test ;

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

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_renderer_primitives ,
 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_u ,
 agg_scanline_p ,
 agg_render_scanlines ,

 agg_math_stroke ,
 agg_path_storage ,
 agg_span_solid ,
 agg_conv_curve ,
 agg_conv_stroke ,
 agg_conv_transform ,
 agg_conv_clip_polygon ,
 agg_conv_gpc ,
 agg_gsv_text ,
 agg_trans_affine ,
 agg_vertex_source ,

 make_gb_poly_ ,
 make_arrows_ ;

{$I agg_mode.inc }

const
 flip_y = true;

type
 spiral = object(vertex_source )
   m_x    ,
   m_y    ,
   m_r1   ,
   m_r2   ,
   m_step ,

   m_start_angle ,

   m_angle  ,
   m_curr_r ,
   m_da     ,
   m_dr     : double;
   m_start  : boolean;

   constructor Construct(x ,y ,r1 ,r2 ,step : double; start_angle : double = 0 );

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 conv_poly_counter = object(vertex_source )
   m_src : vertex_source_ptr;

   m_contours ,
   m_points   : unsigned;

   constructor Construct(src : vertex_source_ptr );

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 the_application = object(platform_support )
   m_polygons  ,
   m_operation : rbox_ctrl;

   m_x ,
   m_y : double;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure perform_rendering(
              sl  : scanline_ptr;
              ras : rasterizer_scanline_ptr;
              ren : renderer_scanline_ptr;
              gpc : conv_gpc_ptr );

   function  render_gpc(sl : scanline_ptr; ras : rasterizer_scanline_ptr ) : unsigned;

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

   procedure stress_test;

  end;

{ CONSTRUCT }
constructor spiral.Construct;
begin
 m_x :=x;
 m_y :=y;
 m_r1:=r1;
 m_r2:=r2;

 m_step       :=step;
 m_start_angle:=start_angle;
 m_angle      :=start_angle;

 m_da:=deg2rad(4.0 );
 m_dr:=m_step / 90.0;

end;

{ REWIND }
procedure spiral.rewind;
begin
 m_angle :=m_start_angle;
 m_curr_r:=m_r1;
 m_start :=true;

end;

{ VERTEX }
function spiral.vertex;
begin
 if m_curr_r > m_r2 then
  begin
   result:=path_cmd_stop;

   exit;

  end;

 x^:=m_x + Cos(m_angle ) * m_curr_r;
 y^:=m_y + Sin(m_angle ) * m_curr_r;

 m_curr_r:=m_curr_r + m_dr;
 m_angle :=m_angle + m_da;

 if m_start then
  begin
   m_start:=false;

   result:=path_cmd_move_to;

  end
 else
  result:=path_cmd_line_to;

end;

{ CONSTRUCT }
constructor conv_poly_counter.Construct;
begin
 m_src:=src;

 m_contours:=0;
 m_points  :=0;

end;

{ REWIND }
procedure conv_poly_counter.rewind;
begin
 m_contours:=0;
 m_points  :=0;

 m_src.rewind(path_id );

end;

{ VERTEX }
function conv_poly_counter.vertex;
var
 cmd : unsigned;

begin
 cmd:=m_src.vertex(x ,y );

 if is_vertex(cmd ) then
  inc(m_points );

 if is_move_to(cmd ) then
  inc(m_contours );

 result:=cmd;

end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_polygons.Construct (5.0   ,5.0 ,5.0 + 205.0  ,110.0 ,not flip_y_ );
 m_operation.Construct(555.0 ,5.0 ,555.0 + 80.0 ,130.0 ,not flip_y_ );

 m_operation.add_item ('None' );
 m_operation.add_item ('OR' );
 m_operation.add_item ('AND' );
 m_operation.add_item ('XOR' );
 m_operation.add_item ('A-B' );
 m_operation.add_item ('B-A' );
 m_operation.cur_item_(2 );

 add_ctrl(@m_operation );

 m_polygons.add_item ('Two Simple Paths' );
 m_polygons.add_item ('Closed Stroke' );
 m_polygons.add_item ('Great Britain and Arrows' );
 m_polygons.add_item ('Great Britain and Spiral' );
 m_polygons.add_item ('Spiral and Glyph' );
 m_polygons.cur_item_(3 );

 add_ctrl(@m_polygons );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_polygons.Destruct;
 m_operation.Destruct;

end;

{ PERFORM_RENDERING }
procedure the_application.perform_rendering;
var
 counter : conv_poly_counter;

 t1 ,t2 ,x ,y : double;

 cmd : unsigned;
 buf : array[0..99 ] of char;

 rgba : aggclr;
 txt  : gsv_text;

 txt_stroke : conv_stroke;

begin
 if m_operation._cur_item > 0 then
  begin
  // Render clipped polygon
   ras.reset;

   case m_operation._cur_item of
    1: gpc.operation(gpc_or );
    2: gpc.operation(gpc_and );
    3: gpc.operation(gpc_xor );
    4: gpc.operation(gpc_a_minus_b );
    5: gpc.operation(gpc_b_minus_a );

   end;

   counter.Construct(gpc );

   start_timer;
   counter.rewind(0 );

   t1:=elapsed_time;

   ras.reset;
   start_timer;

   cmd:=counter.vertex(@x ,@y );

   while not is_stop(cmd ) do
    begin
     ras.add_vertex(x ,y ,cmd );

     cmd:=counter.vertex(@x ,@y );

    end;

   rgba.ConstrDbl  (0.5 ,0.0 ,0 ,0.5 );
   ren.color_      (@rgba );
   render_scanlines(ras ,sl ,ren );

   t2:=elapsed_time;

  // Render information text
   sprintf(@buf[0 ]             ,'Contours: %d   ' ,counter.m_contours );
   sprintf(@buf[StrLen(@buf ) ] ,'Points: %d' ,counter.m_points );

   txt.Construct;
   txt_stroke.Construct(@txt );

   txt_stroke.width_   (1.5 );
   txt_stroke.line_cap_(round_cap );
   txt.size_           (10.0 );
   txt.start_point_    (250 ,5 );
   txt.text_           (@buf[0 ] );

   ras.add_path    (@txt_stroke );
   rgba.ConstrDbl  (0.0 ,0.0 ,0.0 );
   ren.color_      (@rgba );
   render_scanlines(ras ,sl ,ren );

   sprintf(@buf[0 ]            ,'GPC=%.3fms ' ,t1 );
   sprintf(@buf[StrLen(buf ) ] ,'Render=%.3fms' ,t2 );

   txt.start_point_(250 ,20 );
   txt.text_       (@buf[0 ] );

   ras.add_path    (@txt_stroke );
   rgba.ConstrDbl  (0.0 ,0.0 ,0.0 );
   ren.color_      (@rgba );
   render_scanlines(ras ,sl ,ren );

  // Free
   txt.Destruct;
   txt_stroke.Destruct;

  end;

end;

{ RENDER_GPC }
function the_application.render_gpc;
var
 pf : pixel_formats;
 rb : renderer_base;

 ren : renderer_scanline_aa_solid;
 ps1 ,
 ps2 ,

 gb_poly ,
 arrows  ,
 glyph   : path_storage;

 rgba : aggclr;
 x ,y : double;

 mtx1 ,
 mtx2 ,
 mtx  : trans_affine;
 tat  : trans_affine_translation;
 tas  : trans_affine_scaling;

 stroke         ,
 stroke_gb_poly : conv_stroke;

 trans         ,
 trans_gb_poly ,
 trans_arrows  : conv_transform;

 curve : conv_curve;

 sp  : spiral;
 gpc : conv_gpc;

begin
 pixfmt_bgr24(pf ,rbuf_window );

 rb.Construct (@pf );
 ren.Construct(@rb );

 case m_polygons._cur_item of
  0 : // Two simple paths
   begin
    ps1.Construct;
    ps2.Construct;

    gpc.Construct(@ps1 ,@ps2 );

    x:=m_x - _initial_width / 2 + 100;
    y:=m_y - _initial_height / 2 + 100;

    ps1.move_to(x + 140 ,y + 145 );
    ps1.line_to(x + 225 ,y + 44 );
    ps1.line_to(x + 296 ,y + 219 );
    ps1.close_polygon;

    ps1.line_to(x + 226 ,y + 289 );
    ps1.line_to(x + 82  ,y + 292 );

    ps1.move_to(x + 220 ,y + 222 );
    ps1.line_to(x + 363 ,y + 249 );
    ps1.line_to(x + 265 ,y + 331 );

    ps1.move_to(x + 242 ,y + 243 );
    ps1.line_to(x + 268 ,y + 309 );
    ps1.line_to(x + 325 ,y + 261 );

    ps1.move_to(x + 259 ,y + 259 );
    ps1.line_to(x + 273 ,y + 288 );
    ps1.line_to(x + 298 ,y + 266 );

    ps2.move_to(100 + 32  ,100 + 77 );
    ps2.line_to(100 + 473 ,100 + 263 );
    ps2.line_to(100 + 351 ,100 + 290 );
    ps2.line_to(100 + 354 ,100 + 374 );

    ras.reset;
    ras.add_path    (@ps1 );
    rgba.ConstrDbl  (0 ,0 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(ras ,sl ,@ren );

    ras.reset;
    ras.add_path    (@ps2 );
    rgba.ConstrDbl  (0 ,0.6 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(ras ,sl ,@ren );

    perform_rendering(sl ,ras ,@ren ,@gpc );

    ps1.Destruct;
    ps2.Destruct;
    gpc.Destruct;

   end;

  1 : // Closed stroke
   begin
    ps1.Construct;
    ps2.Construct;
    stroke.Construct(@ps2 );
    stroke.width_   (10.0 );

    gpc.Construct(@ps1 ,@stroke );

    x:=m_x - _initial_width / 2 + 100;
    y:=m_y - _initial_height / 2 + 100;

    ps1.move_to(x + 140 ,y + 145 );
    ps1.line_to(x + 225 ,y + 44 );
    ps1.line_to(x + 296 ,y + 219 );
    ps1.close_polygon;

    ps1.line_to(x + 226 ,y + 289 );
    ps1.line_to(x + 82  ,y + 292 );

    ps1.move_to(x + 220 - 50 ,y + 222 );
    ps1.line_to(x + 265 - 50 ,y + 331 );
    ps1.line_to(x + 363 - 50 ,y + 249 );
    ps1.close_polygon(path_flags_ccw );

    ps2.move_to(100 + 32  ,100 + 77  );
    ps2.line_to(100 + 473 ,100 + 263 );
    ps2.line_to(100 + 351 ,100 + 290 );
    ps2.line_to(100 + 354 ,100 + 374 );
    ps2.close_polygon;

    ras.reset;
    ras.add_path    (@ps1 );
    rgba.ConstrDbl  (0 ,0 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(ras ,sl ,@ren );

    ras.reset;
    ras.add_path    (@stroke );
    rgba.ConstrDbl  (0 ,0.6 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(ras ,sl ,@ren );

    perform_rendering(sl ,ras ,@ren ,@gpc );

    ps1.Destruct;
    ps2.Destruct;
    stroke.Destruct;
    gpc.Destruct;

   end;

  2 : // Great Britain and Arrows
   begin
    gb_poly.Construct;
    arrows.Construct;

    make_gb_poly(@gb_poly );
    make_arrows (@arrows );

    mtx1.Construct;
    mtx2.Construct;
    tat.Construct(-1150 ,-1150 );
    tas.Construct(2.0 );
    mtx1.multiply(@tat );
    mtx1.multiply(@tas );

    mtx2:=mtx1;

    tat.Construct(m_x - _initial_width / 2 ,m_y - _initial_height / 2 );

    mtx2.multiply(@tat );

    trans_gb_poly.Construct(@gb_poly ,@mtx1 );
    trans_arrows.Construct (@arrows  ,@mtx2 );

    gpc.Construct(@trans_gb_poly ,@trans_arrows );

    ras.add_path    (@trans_gb_poly );
    rgba.ConstrDbl  (0.5 ,0.5 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(ras ,sl ,@ren );

    stroke_gb_poly.Construct(@trans_gb_poly );
    stroke_gb_poly.width_   (0.1);
    ras.add_path            (@stroke_gb_poly );
    rgba.ConstrDbl          (0 ,0 ,0 );
    ren.color_              (@rgba );
    render_scanlines        (ras ,sl ,@ren );

    ras.add_path    (@trans_arrows );
    rgba.ConstrDbl  (0.0 ,0.5 ,0.5 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(ras ,sl ,@ren );

    perform_rendering(sl ,ras ,@ren ,@gpc );

    gb_poly.Destruct;
    arrows.Destruct;
    stroke_gb_poly.Destruct;
    gpc.Destruct;

   end;

  3 : // Great Britain and a Spiral
   begin
    sp.Construct    (m_x ,m_y ,10 ,150 ,30 ,0.0 );
    stroke.Construct(@sp );
    stroke.width_   (15.0 );

    gb_poly.Construct;
    make_gb_poly(@gb_poly );

    mtx.Construct;
    tat.Construct(-1150 ,-1150 );
    tas.Construct(2.0 );
    mtx.multiply(@tat );
    mtx.multiply(@tas );

    trans_gb_poly.Construct(@gb_poly ,@mtx );

    gpc.Construct(@trans_gb_poly ,@stroke );

    ras.add_path    (@trans_gb_poly );
    rgba.ConstrDbl  (0.5 ,0.5 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(ras ,sl ,@ren );

    stroke_gb_poly.Construct(@trans_gb_poly );
    stroke_gb_poly.width_   (0.1 );
    ras.add_path            (@stroke_gb_poly );
    rgba.ConstrDbl          (0 ,0 ,0 );
    ren.color_              (@rgba );
    render_scanlines        (ras ,sl ,@ren );

    ras.add_path    (@stroke );
    rgba.ConstrDbl  (0.0 ,0.5 ,0.5 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(ras ,sl ,@ren );

    perform_rendering(sl ,ras ,@ren ,@gpc );

    stroke.Destruct;
    gb_poly.Destruct;
    stroke_gb_poly.Destruct;
    gpc.Destruct;

   end;

  4 : // Spiral and glyph
   begin
    sp.Construct    (m_x ,m_y ,10 ,150 ,30 ,0.0 );
    stroke.Construct(@sp );
    stroke.width_   (15.0 );

    glyph.Construct;
    glyph.move_to(28.47 ,6.45 );
    glyph.curve3 (21.58 ,1.12  ,19.82 ,0.29 );
    glyph.curve3 (17.19 ,-0.93 ,14.21 ,-0.93 );
    glyph.curve3 (9.57  ,-0.93 ,6.57  ,2.25 );
    glyph.curve3 (3.56  ,5.42  ,3.56  ,10.60 );
    glyph.curve3 (3.56  ,13.87 ,5.03  ,16.26 );
    glyph.curve3 (7.03  ,19.58 ,11.99 ,22.51 );
    glyph.curve3 (16.94 ,25.44 ,28.47 ,29.64 );
    glyph.line_to(28.47 ,31.40 );
    glyph.curve3 (28.47 ,38.09 ,26.34 ,40.58 );
    glyph.curve3 (24.22 ,43.07 ,20.17 ,43.07 );
    glyph.curve3 (17.09 ,43.07 ,15.28 ,41.41 );
    glyph.curve3 (13.43 ,39.75 ,13.43 ,37.60 );
    glyph.line_to(13.53 ,34.77 );
    glyph.curve3 (13.53 ,32.52 ,12.38 ,31.30 );
    glyph.curve3 (11.23 ,30.08 ,9.38  ,30.08 );
    glyph.curve3 (7.57  ,30.08 ,6.42  ,31.35 );
    glyph.curve3 (5.27  ,32.62 ,5.27  ,34.81 );
    glyph.curve3 (5.27  ,39.01 ,9.57  ,42.53 );
    glyph.curve3 (13.87 ,46.04 ,21.63 ,46.04 );
    glyph.curve3 (27.59 ,46.04 ,31.40 ,44.04 );
    glyph.curve3 (34.28 ,42.53 ,35.64 ,39.31 );
    glyph.curve3 (36.52 ,37.21 ,36.52 ,30.71 );
    glyph.line_to(36.52 ,15.53 );
    glyph.curve3 (36.52 ,9.13  ,36.77 ,7.69 );
    glyph.curve3 (37.01 ,6.25  ,37.57 ,5.76 );
    glyph.curve3 (38.13 ,5.27  ,38.87 ,5.27 );
    glyph.curve3 (39.65 ,5.27  ,40.23 ,5.62 );
    glyph.curve3 (41.26 ,6.25  ,44.19 ,9.18 );
    glyph.line_to(44.19 ,6.45 );
    glyph.curve3 (38.72 ,-0.88 ,33.74 ,-0.88 );
    glyph.curve3 (31.35 ,-0.88 ,29.93 ,0.78 );
    glyph.curve3 (28.52 ,2.44  ,28.47 ,6.45 );
    glyph.close_polygon;

    glyph.move_to(28.47 ,9.62 );
    glyph.line_to(28.47 ,26.66 );
    glyph.curve3 (21.09 ,23.73 ,18.95 ,22.51 );
    glyph.curve3 (15.09 ,20.36 ,13.43 ,18.02 );
    glyph.curve3 (11.77 ,15.67 ,11.77 ,12.89 );
    glyph.curve3 (11.77 ,9.38  ,13.87 ,7.06 );
    glyph.curve3 (15.97 ,4.74  ,18.70 ,4.74 );
    glyph.curve3 (22.41 ,4.74  ,28.47 ,9.62 );
    glyph.close_polygon;

    mtx.Construct;
    tas.Construct(4.0 );
    tat.Construct(220 ,200 );
    mtx.multiply(@tas );
    mtx.multiply(@tat );

    trans.Construct(@glyph ,@mtx );
    curve.Construct(@trans );

    gpc.Construct(@stroke ,@curve );

    ras.reset;
    ras.add_path    (@stroke );
    rgba.ConstrDbl  (0 ,0 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(ras ,sl ,@ren );

    ras.reset;
    ras.add_path    (@curve );
    rgba.ConstrDbl  (0 ,0.6 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(ras ,sl ,@ren );

    perform_rendering(sl ,ras ,@ren ,@gpc );

    stroke.Destruct;
    glyph.Destruct;
    curve.Destruct;
    gpc.Destruct;
    
   end;

 end;

 result:=0;

end;

{ ON_INIT }
procedure the_application.on_init;
begin
 m_x:=_width / 2.0;
 m_y:=_height / 2.0;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pf : pixel_formats;

 rgba : aggclr;

 ren_base  : renderer_base;
 ren_solid : renderer_scanline_aa_solid;

 sl  : scanline_u8;
 ras : rasterizer_scanline_aa;

begin
// Initialize structures
 pixfmt_bgr24(pf ,rbuf_window );

 ren_base.Construct (@pf );
 ren_solid.Construct(@ren_base );

 rgba.ConstrDbl(1 ,1 ,1 );
 ren_base.clear(@rgba );

 sl.Construct;
 ras.Construct;

// Render
 render_gpc(@sl ,@ras );

// Render the controls
 render_ctrl(@ras ,@sl ,@ren_solid ,@m_polygons );
 render_ctrl(@ras ,@sl ,@ren_solid ,@m_operation );

// Free AGG resources
 sl.Destruct;
 ras.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 if flags and mouse_left <> 0 then
  begin
   m_x:=x;
   m_y:=y;

   force_redraw;

  end;

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
var
 buf : array[0..99 ] of char;

begin
 if flags and mouse_left <> 0 then
  begin
   m_x:=x;
   m_y:=y;

   force_redraw;

  end;

 if flags and mouse_right <> 0 then
  begin
   sprintf (@buf[0 ]             ,'%d ' ,x );
   sprintf (@buf[StrLen(@buf ) ] ,'%d'  ,y );
   message_(@buf[0 ] );

  end;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 case key of
  byte('t' ) ,
  byte('T' ) :
   stress_test;

 end;

 if key = key_f1 then
  message_(
   'General Polygon Clipper by Alan Murta is the most reliable implementation of the '#13 +
   'polygon boolean algebra. It implements Bala R. Vatti''s algorithm of arbitrary '#13 +
   'polygon clipping and allows you to calculate the Union, Intersection, Difference, '#13 +
   'and Exclusive OR between two poly-polygons (i.e., polygonal areas consisted of '#13 +
   'several contours). AGG has a simple wrapper class that can be used in the coordinate '#13 +
   'conversion pipeline. The implementation by Alan Murta has restrictions of using it '#13 +
   'in commercial software, so that, please contact the author to settle the legal issues. '#13 +
   'The example demonstrates the use of GPC. Note, that all operations are done in the '#13 +
   'vectorial representation of the contours before rendering.'#13#13 +
   'How to play with:'#13#13 +
   'You can drag one polygon with the left mouse button pressed.'#13 +
   'Press the "T" key to perform the random polygon clipping stress testing.'#13 +
   '(may take some time)' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

{ STRESS_TEST }
// Stress-test.
// Works quite well on random polygons, no crashes, no memory leaks!
// Sometimes takes long to produce the result
procedure the_application.stress_test;

{ random }
function random(min ,max : double ) : double;
var
 r : int;

begin
 r:=(System.Random($7fff ) shl 15 ) or System.Random($7fff );

 result:=$FFFFFFF + 1;
 result:=((r and $FFFFFFF) / result ) * (max - min ) + min;

end;

var
 sl  : scanline_u8;
 ras : rasterizer_scanline_aa;

 pf : pixel_formats;

 ren_base  : renderer_base;
 ren_solid : renderer_scanline_aa_solid;

 ps1 ,ps2 : path_storage;

 gpc  : conv_gpc;
 rgba : aggclr;

 i ,num_poly1 ,num_poly2 ,j ,k ,np ,op: unsigned;

 buf : array[0..99 ] of char;
 txt : gsv_text;

 txt_stroke : conv_stroke;

begin
 sl.Construct;
 ras.Construct;

 pixfmt_bgr24(pf ,rbuf_window );

 ren_base.Construct (@pf );
 ren_solid.Construct(@ren_base );

 ps1.Construct;
 ps2.Construct;
 gpc.Construct(@ps1 ,@ps2 );

 txt.Construct;
 txt_stroke.Construct(@txt );

 txt_stroke.width_   (1.5 );
 txt_stroke.line_cap_(round_cap );
 txt.size_           (10.0 );
 txt.start_point_    (5 ,5 );

 for i:=0 to 999 do
  begin
   rgba.ConstrDbl(1 ,1 ,1 );
   ren_base.clear(@rgba );

   num_poly1:=System.Random($7fff ) mod 10 + 1;
   num_poly2:=System.Random($7fff ) mod 10 + 1;

   ps1.remove_all;
   ps2.remove_all;

   for j:=0 to num_poly1 - 1 do
    begin
     ps1.move_to(random(0 ,_width ) ,random(0 ,_height ) );

     np:=System.Random($7fff ) mod 20 + 2;

     for k:=0 to np - 1 do
      ps1.line_to(random(0 ,_width ) ,random(0 ,_height ) );

    end;

   for j:=0 to num_poly2 - 1 do
    begin
     ps2.move_to(random(0 ,_width ) ,random(0 ,_height ) );

     np:=System.Random($7fff ) mod 20 + 2;

     for k:=0 to np - 1 do
      ps2.line_to(random(0 ,_width ) ,random(0 ,_height ) );

    end;

   op:=System.Random($7fff ) mod 5;

   case op of
    0 :
     gpc.operation(gpc_or );

    1 :
     gpc.operation(gpc_and );

    2 :
     gpc.operation(gpc_xor );

    3 :
     gpc.operation(gpc_a_minus_b );

    else
     gpc.operation(gpc_b_minus_a );

   end;

  // Clipping result
   ras.add_path    (@gpc );
   rgba.ConstrDbl  (0.5 ,0.0 ,0 ,0.5 );
   ren_solid.color_(@rgba );
   render_scanlines(@ras ,@sl ,@ren_solid );

  // Counter display
   sprintf  (@buf[0 ] ,'%d / 1000' ,i + 1 );
   txt.text_(@buf[0 ] );

   txt.start_point_(5 ,5 );

   ras.add_path    (@txt_stroke );
   rgba.ConstrDbl  (0.0 ,0.0 ,0.0 );
   ren_solid.color_(@rgba );
   render_scanlines(@ras ,@sl ,@ren_solid );

  // Refresh
   update_window;

  end;

 message_('Done' );

 ps1.Destruct;
 ps2.Destruct;
 gpc.Destruct;

 sl.Destruct;
 ras.Destruct;

 txt.Destruct;
 txt_stroke.Destruct;

 force_redraw;

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. General Polygon Clipping - GPC (F1-Help)' );

 if app.init(640 ,520 ,window_resize ) then
  app.run;

 app.Destruct;

END.