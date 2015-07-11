//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 alpha_mask3 ;

uses
 SysUtils ,

 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,
 agg_pixfmt_gray ,

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
 agg_scanline_p ,
 agg_render_scanlines ,

 agg_conv_curve ,
 agg_conv_stroke ,
 agg_conv_transform ,
 agg_gsv_text ,
 agg_pixfmt_amask_adaptor ,
 agg_alpha_mask_u8 ,
 agg_vertex_source ,
 agg_path_storage ,
 agg_trans_affine ,
 agg_math_stroke ,

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

 the_application = object(platform_support )
   m_polygons  ,
   m_operation : rbox_ctrl;

   m_alpha_buf       : int8u_ptr;
   m_alpha_aloc      : unsigned;
   m_alpha_mask_rbuf : rendering_buffer;
   m_alpha_mask      : amask_no_clip_gray8;

   m_ras : rasterizer_scanline_aa;
   m_sl  : scanline_p8;

   m_x ,
   m_y : double;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure draw_text(x ,y : double; str : char_ptr );

   procedure generate_alpha_mask(vs : vertex_source_ptr );
   procedure perform_rendering  (vs : vertex_source_ptr );

   function  render : unsigned;

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

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
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_polygons.Construct (5.0   ,5.0 ,5.0 + 205.0  ,110.0 ,not flip_y_ );
 m_operation.Construct(555.0 ,5.0 ,555.0 + 80.0 ,55.0  ,not flip_y_ );

 m_alpha_buf :=NIL;
 m_alpha_aloc:=0;

 m_alpha_mask_rbuf.Construct;
 m_alpha_mask.Construct(@m_alpha_mask_rbuf );

 m_ras.Construct;
 m_sl.Construct;

 m_x:=0;
 m_y:=0;

 m_operation.add_item ('AND' );
 m_operation.add_item ('SUB' );
 m_operation.cur_item_(0 );

 add_ctrl(@m_operation );

 m_operation.no_transform;

 m_polygons.add_item ('Two Simple Paths' );
 m_polygons.add_item ('Closed Stroke' );
 m_polygons.add_item ('Great Britain and Arrows' );
 m_polygons.add_item ('Great Britain and Spiral' );
 m_polygons.add_item ('Spiral and Glyph' );
 m_polygons.cur_item_(3 );

 add_ctrl(@m_polygons );

 m_polygons.no_transform;

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_polygons.Destruct;
 m_operation.Destruct;

 m_alpha_mask_rbuf.Destruct;

 m_ras.Destruct;
 m_sl.Destruct;

 agg_freemem(pointer(m_alpha_buf ) ,m_alpha_aloc );

end;

{ DRAW_TEXT }
procedure the_application.draw_text;
var
 pf  : pixel_formats;
 rb  : renderer_base;
 ren : renderer_scanline_aa_solid;
 txt : gsv_text;

 txt_stroke : conv_stroke;

 rgba : aggclr;

begin
 pixfmt_bgr24(pf ,rbuf_window );

 rb.Construct (@pf );
 ren.Construct(@rb );

 txt.Construct;

 txt_stroke.Construct(@txt );
 txt_stroke.width_   (1.5 );
 txt_stroke.line_cap_(round_cap );

 txt.size_       (10.0 );
 txt.start_point_(x ,y );
 txt.text_       (PChar(str ) );

 m_ras.add_path  (@txt_stroke);
 rgba.ConstrDbl  (0.0 ,0.0 ,0.0 );
 ren.color_      (@rgba );
 render_scanlines(@m_ras ,@m_sl ,@ren );

 txt.Destruct;
 txt_stroke.Destruct;

end;

{ GENERATE_ALPHA_MASK }
procedure the_application.generate_alpha_mask;
var
 cx ,cy : unsigned;

 pixf : pixel_formats;
 gray : aggclr;

 rb  : renderer_base;
 ren : renderer_scanline_aa_solid;

 t1  : double;
 buf : array[0..99 ] of char;

begin
 cx:=trunc(_width );
 cy:=trunc(_height );

 agg_freemem(pointer(m_alpha_buf ) ,m_alpha_aloc );

 m_alpha_aloc:=cx * cy;

 agg_getmem(pointer(m_alpha_buf ) ,m_alpha_aloc );

 m_alpha_mask_rbuf.attach(m_alpha_buf ,cx ,cy ,cx );

 pixfmt_gray8(pixf ,@m_alpha_mask_rbuf );

 rb.Construct (@pixf );
 ren.Construct(@rb );

 start_timer;

 if m_operation._cur_item = 0 then
  begin
   gray.ConstrInt(0 );
   rb.clear      (@gray );

   gray.ConstrInt(255 );
   ren.color_    (@gray );

  end
 else
  begin
   gray.ConstrInt(255 );
   rb.clear      (@gray );

   gray.ConstrInt(0 );
   ren.color_    (@gray );

  end;

 m_ras.add_path  (vs );
 render_scanlines(@m_ras ,@m_sl ,@ren );

 t1:=elapsed_time;

 sprintf  (@buf[0 ] ,'Generate AlphaMask: %.3fms' ,t1 );
 draw_text(250 ,20 ,@buf[0 ] );

end;

{ PERFORM_RENDERING }
procedure the_application.perform_rendering;
var
 pixf  : pixel_formats;
 pixfa : pixfmt_amask_adaptor;
 rbase : renderer_base;

 ren  : renderer_scanline_aa_solid;
 rgba : aggclr;

 t1  : double;
 buf : array[0..99 ] of char;
 
begin
 pixfmt_bgr24(pixf ,rbuf_window );

 pixfa.Construct(@pixf ,@m_alpha_mask );
 rbase.Construct(@pixfa );
 ren.Construct  (@rbase );

 rgba.ConstrDbl(0.5 ,0.0 ,0 ,0.5 );
 ren.color_    (@rgba );

 start_timer;
 m_ras.reset;
 m_ras.add_path  (vs );
 render_scanlines(@m_ras ,@m_sl ,@ren );

 t1:=elapsed_time;

 sprintf  (@buf[0 ] ,'Render with AlphaMask: %.3fms' ,t1 );
 draw_text(250 ,5 ,@buf[0 ] );

 pixfa.Destruct;

end;

{ RENDER }
function the_application.render;
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

 sp : spiral;

begin
 pixfmt_bgr24(pf ,rbuf_window );

 rb.Construct (@pf );
 ren.Construct(@rb );

 case m_polygons._cur_item of
  0 : // Two simple paths
   begin
    ps1.Construct;
    ps2.Construct;

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

    m_ras.reset;
    m_ras.add_path  (@ps1 );
    rgba.ConstrDbl  (0 ,0 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(@m_ras ,@m_sl ,@ren );

    m_ras.reset;
    m_ras.add_path  (@ps2 );
    rgba.ConstrDbl  (0 ,0.6 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(@m_ras ,@m_sl ,@ren );

    generate_alpha_mask(@ps1 );
    perform_rendering  (@ps2 );

    ps1.Destruct;
    ps2.Destruct;

   end;

  1 : // Closed stroke
   begin
    ps1.Construct;
    ps2.Construct;
    stroke.Construct(@ps2 );
    stroke.width_   (10.0 );

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

    m_ras.reset;
    m_ras.add_path  (@ps1 );
    rgba.ConstrDbl  (0 ,0 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(@m_ras ,@m_sl ,@ren );

    m_ras.reset;
    m_ras.add_path  (@stroke );
    rgba.ConstrDbl  (0 ,0.6 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(@m_ras ,@m_sl ,@ren );

    generate_alpha_mask(@ps1 );
    perform_rendering  (@stroke );

    ps1.Destruct;
    ps2.Destruct;
    stroke.Destruct;

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

    m_ras.add_path  (@trans_gb_poly );
    rgba.ConstrDbl  (0.5 ,0.5 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(@m_ras ,@m_sl ,@ren );

    stroke_gb_poly.Construct(@trans_gb_poly );
    stroke_gb_poly.width_   (0.1);
    m_ras.add_path          (@stroke_gb_poly );
    rgba.ConstrDbl          (0 ,0 ,0 );
    ren.color_              (@rgba );
    render_scanlines        (@m_ras ,@m_sl ,@ren );

    m_ras.add_path  (@trans_arrows );
    rgba.ConstrDbl  (0.0 ,0.5 ,0.5 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(@m_ras ,@m_sl ,@ren );

    generate_alpha_mask(@trans_gb_poly );
    perform_rendering  (@trans_arrows );

    gb_poly.Destruct;
    arrows.Destruct;
    stroke_gb_poly.Destruct;

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

    m_ras.add_path  (@trans_gb_poly );
    rgba.ConstrDbl  (0.5 ,0.5 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(@m_ras ,@m_sl ,@ren );

    stroke_gb_poly.Construct(@trans_gb_poly );
    stroke_gb_poly.width_   (0.1 );
    m_ras.add_path          (@stroke_gb_poly );
    rgba.ConstrDbl          (0 ,0 ,0 );
    ren.color_              (@rgba );
    render_scanlines        (@m_ras ,@m_sl ,@ren );

    m_ras.add_path  (@stroke );
    rgba.ConstrDbl  (0.0 ,0.5 ,0.5 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(@m_ras ,@m_sl ,@ren );

    generate_alpha_mask(@trans_gb_poly );
    perform_rendering  (@stroke );

    stroke.Destruct;
    gb_poly.Destruct;
    stroke_gb_poly.Destruct;

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

    m_ras.reset;
    m_ras.add_path  (@stroke );
    rgba.ConstrDbl  (0 ,0 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(@m_ras ,@m_sl ,@ren );

    m_ras.reset;
    m_ras.add_path  (@curve );
    rgba.ConstrDbl  (0 ,0.6 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(@m_ras ,@m_sl ,@ren );

    generate_alpha_mask(@stroke );
    perform_rendering  (@curve );

    stroke.Destruct;
    glyph.Destruct;
    curve.Destruct;

   end;

 end;

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

begin
// Initialize structures
 pixfmt_bgr24(pf ,rbuf_window );

 ren_base.Construct (@pf );
 ren_solid.Construct(@ren_base );

 rgba.ConstrDbl(1 ,1 ,1 );
 ren_base.clear(@rgba );

// Render
 render;

// Render the controls
 render_ctrl(@m_ras ,@m_sl ,@ren_solid ,@m_polygons );
 render_ctrl(@m_ras ,@m_sl ,@ren_solid ,@m_operation );

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
 if key = key_f1 then
  message_(
   'Yet another example of alpha-masking. It simulates arbitrary polygon clipping '#13 +
   'similar to "gpc_test". Alpha-Masking allows you to perform only the Intersection '#13 +
   '(AND) and Difference (SUB) operations, but works much faster that conv_gpc. '#13 +
   'Actually, there''re different complexities and different dependencies. '#13 +
   'The performance of conv_gpc depends on the number of vertices, while'#13 +
   'Alpha-Masking depends on the area of the rendered polygons. Still, with typical '#13 +
   'screen resolutions, Alpha-Masking works much faster than General Polygon Clipper. '#13 +
   'Compare the timings between "alpha_mask3" and "gpc_test"".'#13#13 +
   'How to play with:'#13#13 +
   'Use the left mouse button to move the upper shape around.'#13 +
   'Use the right mouse button to display a message with current coordinates.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. Alpha-Mask as a Polygon Clipper (F1-Help)' );

 if app.init(640 ,520 ,window_resize ) then
  app.run;

 app.Destruct;

END.