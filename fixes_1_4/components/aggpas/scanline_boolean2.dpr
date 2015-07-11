//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 scanline_boolean2 ;

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
 agg_scanline_bin ,
 agg_scanline_storage_aa ,
 agg_scanline_storage_bin ,
 agg_scanline_boolean_algebra ,
 agg_render_scanlines ,

 agg_math_stroke ,
 agg_path_storage ,
 agg_span_solid ,
 agg_conv_curve ,
 agg_conv_stroke ,
 agg_conv_transform ,
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

 the_application = object(platform_support )
   m_polygons      ,
   m_fill_rule     ,
   m_scanline_type ,
   m_operation     : rbox_ctrl;

   m_x ,
   m_y : double;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure render_scanline_boolean(ras1 ,ras2 : rasterizer_scanline_ptr );
   function  render_sbool           (ras1 ,ras2 : rasterizer_scanline_ptr ) : unsigned;

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

{ COUNT_SPANS }
function count_spans(ras : rasterizer_scanline_ptr; sl : scanline_ptr ) : unsigned;
var
 n : unsigned;

begin
 n:=0;

 if ras.rewind_scanlines then
  begin
   sl.reset(ras._min_x ,ras._max_x );

   if sl.is_embedded then
    while ras.sweep_scanline_em(sl ) do
     inc(n ,sl.num_spans )
   else
    while ras.sweep_scanline(sl ) do
     inc(n ,sl.num_spans );

  end;

 result:=n;

end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_polygons.Construct     (5.0   ,5.0 ,5.0 + 205.0   ,110.0 ,not flip_y_ );
 m_fill_rule.Construct    (200   ,5.0 ,200 + 105.0   ,50.0  ,not flip_y_ );
 m_scanline_type.Construct(300   ,5.0 ,300 + 115.0   ,70.0  ,not flip_y_ );
 m_operation.Construct    (535.0 ,5.0 ,535.0 + 115.0 ,145.0 ,not flip_y_ );

 m_operation.add_item ('None' );
 m_operation.add_item ('OR' );
 m_operation.add_item ('AND' );
 m_operation.add_item ('XOR Linear' );
 m_operation.add_item ('XOR Saddle' );
 m_operation.add_item ('A-B' );
 m_operation.add_item ('B-A' );
 m_operation.cur_item_(2 );

 add_ctrl(@m_operation );

 m_operation.no_transform;

 m_fill_rule.add_item ('Even-Odd' );
 m_fill_rule.add_item ('Non Zero' );
 m_fill_rule.cur_item_(1 );

 add_ctrl(@m_fill_rule );

 m_fill_rule.no_transform;

 m_scanline_type.add_item ('scanline_p' );
 m_scanline_type.add_item ('scanline_u' );
 m_scanline_type.add_item ('scanline_bin' );
 m_scanline_type.cur_item_(1 );

 add_ctrl(@m_scanline_type );

 m_scanline_type.no_transform;

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
 m_fill_rule.Destruct;
 m_scanline_type.Destruct;
 m_operation.Destruct;

end;

{ RENDER_SCANLINE_BOOLEAN }
procedure the_application.render_scanline_boolean;
var
 op : sbool_op_e;
 rb : renderer_base;

 buf : array[0..99 ] of char;

 pixf : pixel_formats;
 rgba : aggclr;

 t1 ,t2 : double;

 i : int;

 num_spans : unsigned;

 renp ,
 renu ,
 rent : renderer_scanline_aa_solid;
 renb : renderer_scanline_bin_solid;

 slt  ,
 slp  ,
 slp1 ,
 slp2 : scanline_p8;
 slu  ,
 slu1 ,
 slu2 : scanline_u8;
 slb  ,
 slb1 ,
 slb2 : scanline_bin;
 txt  : gsv_text;

 txt_stroke : conv_stroke;

 storage  ,
 storage1 ,
 storage2 : scanline_storage_aa8;

 storageb  ,
 storageb1 ,
 storageb2 : scanline_storage_bin;

begin
 if m_operation._cur_item > 0 then
  begin
   case m_operation._cur_item of
    1 : op:=sbool_or;
    2 : op:=sbool_and;
    3 : op:=sbool_xor;
    4 : op:=sbool_xor_saddle;
    5 : op:=sbool_a_minus_b;
    6 : op:=sbool_b_minus_a;

   end;

   pixfmt_bgr24(pixf ,rbuf_window );
   rb.Construct(@pixf );

   t1:=0.0;
   t2:=0.0;

   num_spans:=0;

  // Render Clipping
   case m_scanline_type._cur_item of
    0 :
     begin
      renp.Construct(@rb );
      slp.Construct;
      slp1.Construct;
      slp2.Construct;

      // The intermediate storage is used only to test the perfoprmance,
      // the short variant can be as follows:
      // ------------------------
      // rgba.ConstrDbl         (0.5 ,0.0 ,0 ,0.5 );
      // renp.color             (@rgba );
      // sbool_combine_shapes_aa(op ,ras1 ,ras2 ,@slp1 ,@slp2 ,@slp ,@renp );

      storage.Construct;
      storage1.Construct;
      storage2.Construct;

      render_scanlines(ras1 ,@slp ,@storage1 );
      render_scanlines(ras2 ,@slp ,@storage2 );

      start_timer;

      for i:=0 to 9 do
       sbool_combine_shapes_aa(op ,@storage1 ,@storage2 ,@slp1 ,@slp2 ,@slp ,@storage );

      t1:=elapsed_time / 10.0;

      start_timer;

      rgba.ConstrDbl  (0.5 ,0.0 ,0 ,0.5 );
      renp.color_     (@rgba );
      render_scanlines(@storage ,@slp ,@renp );

      t2:=elapsed_time;

      num_spans:=count_spans(@storage ,@slp );

      slp.Destruct;
      slp1.Destruct;
      slp2.Destruct;

      storage.Destruct;
      storage1.Destruct;
      storage2.Destruct;

     end;

    1 :
     begin
      renu.Construct(@rb );

      slu.Construct;
      slu1.Construct;
      slu2.Construct;

      {rgba.ConstrDbl         (0.5 ,0.0 ,0 ,0.5 );
      renu.color             (@rgba );
      sbool_combine_shapes_aa(op ,ras1 ,ras2 ,@slu1 ,@slu2 ,@slu ,@renu );}

      storage.Construct;
      storage1.Construct;
      storage2.Construct;

      render_scanlines(ras1 ,@slu ,@storage1 );
      render_scanlines(ras2 ,@slu ,@storage2 );

      start_timer;

      for i:=0 to 9 do
       sbool_combine_shapes_aa(op ,@storage1 ,@storage2 ,@slu1 ,@slu2 ,@slu ,@storage );

      t1:=elapsed_time / 10.0;

      start_timer;

      rgba.ConstrDbl  (0.5 ,0.0 ,0 ,0.5 );
      renu.color_     (@rgba );
      render_scanlines(@storage ,@slu ,@renu );

      t2:=elapsed_time;

      num_spans:=count_spans(@storage ,@slu );{}

      slu.Destruct;
      slu1.Destruct;
      slu2.Destruct;

      storage.Destruct;
      storage1.Destruct;
      storage2.Destruct;{}

     end;

    2 :
     begin
      renb.Construct(@rb );

      slb.Construct;
      slb1.Construct;
      slb2.Construct;

      storageb.Construct;
      storageb1.Construct;
      storageb2.Construct;

      render_scanlines(ras1 ,@slb ,@storageb1 );
      render_scanlines(ras2 ,@slb ,@storageb2 );

      start_timer;

      for i:=0 to 9 do
       sbool_combine_shapes_bin(op ,@storageb1 ,@storageb2 ,@slb1 ,@slb2 ,@slb ,@storageb );

      t1:=elapsed_time / 10.0;

      start_timer;

      rgba.ConstrDbl  ({0.5}1 ,0.0 ,0 {,0.5} );
      renb.color_     (@rgba );
      render_scanlines(@storageb ,@slb ,@renb );

      t2:=elapsed_time;

      num_spans:=count_spans(@storageb ,@slb );

      slb.Destruct;
      slb1.Destruct;
      slb2.Destruct;

      storageb.Destruct;
      storageb1.Destruct;
      storageb2.Destruct;

     end;

   end;

  // Render text
   sprintf(@buf[0 ]             ,'Combine=%.3fms'#13#13 ,t1 );
   sprintf(@buf[StrLen(@buf ) ] ,'Render=%.3fms'#13#13 ,t2 );
   sprintf(@buf[StrLen(@buf ) ] ,'num_spans=%d' ,num_spans );

   rent.Construct(@rb );
   slt.Construct;
   txt.Construct;
   txt_stroke.Construct(@txt );
   txt_stroke.width_   (1.0 );
   txt_stroke.line_cap_(round_cap );
   txt.size_           (8.0 );
   txt.start_point_    (420 ,40 );
   txt.text_           (@buf[0 ] );

   ras1.reset;
   ras1.add_path   (@txt_stroke );
   rgba.ConstrDbl  (0.0 ,0.0 ,0.0 );
   rent.color_     (@rgba );
   render_scanlines(ras1 ,@slt ,@rent );

   slt.Destruct;
   txt.Destruct;
   txt_stroke.Destruct;

  end;

end;

{ RENDER_SBOOL }
function the_application.render_sbool;
var
 pf  : pixel_formats;
 rb  : renderer_base;
 ren : renderer_scanline_aa_solid;
 sl  : scanline_p8;

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

begin
 pixfmt_bgr24(pf ,rbuf_window );

 rb.Construct (@pf );
 ren.Construct(@rb );

 sl.Construct;

 if m_fill_rule._cur_item <> 0 then
  ras1.filling_rule(fill_non_zero )
 else
  ras1.filling_rule(fill_even_odd );

 if m_fill_rule._cur_item <> 0 then
  ras2.filling_rule(fill_non_zero )
 else
  ras2.filling_rule(fill_even_odd );

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

    ras1.reset;
    ras1.add_path   (@ps1 );
    rgba.ConstrDbl  (0 ,0 ,0 ,{0.1}0.0 );
    ren.color_      (@rgba );
   { render_scanlines(ras1 ,@sl ,@ren ); }

    ras2.reset;
    ras2.add_path   (@ps2 );
    rgba.ConstrDbl  (0 ,0.6 ,0 ,{0.1}0.0 );
    ren.color_      (@rgba );
   { render_scanlines(ras2 ,@sl ,@ren ); }

    render_scanline_boolean(ras1 ,ras2 );

    ps1.Destruct;
    ps2.Destruct;

   end;

  1 : // Closed stroke
   begin
    ps1.Construct;
    ps2.Construct;
    stroke.Construct(@ps2 );
    stroke.width_   (15.0 );

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

    ras1.reset;
    ras1.add_path   (@ps1 );
    rgba.ConstrDbl  (0 ,0 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(ras1 ,@sl ,@ren );

    ras2.reset;
    ras2.add_path   (@stroke );
    rgba.ConstrDbl  (0 ,0.6 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(ras2 ,@sl ,@ren );

    render_scanline_boolean(ras1 ,ras2 );

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

    ras2.add_path   (@trans_gb_poly );
    rgba.ConstrDbl  (0.5 ,0.5 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(ras2 ,@sl ,@ren );

    stroke_gb_poly.Construct(@trans_gb_poly );
    stroke_gb_poly.width_   (0.1);
    ras1.add_path           (@stroke_gb_poly );
    rgba.ConstrDbl          (0 ,0 ,0 );
    ren.color_              (@rgba );
    render_scanlines        (ras1 ,@sl ,@ren );

    ras2.add_path   (@trans_arrows );
    rgba.ConstrDbl  (0.0 ,0.5 ,0.5 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(ras2 ,@sl ,@ren );

    ras1.reset;
    ras1.add_path(@trans_gb_poly );

    render_scanline_boolean(ras1 ,ras2 );

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
    mtx.multiply(_trans_affine_resizing );

    trans_gb_poly.Construct(@gb_poly ,@mtx );

    ras1.add_path   (@trans_gb_poly );
    rgba.ConstrDbl  (0.5 ,0.5 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(ras1 ,@sl ,@ren );

    stroke_gb_poly.Construct(@trans_gb_poly );
    stroke_gb_poly.width_   (0.1 );
    ras1.add_path           (@stroke_gb_poly );
    rgba.ConstrDbl          (0 ,0 ,0 );
    ren.color_              (@rgba );
    render_scanlines        (ras1 ,@sl ,@ren );

    ras2.reset;
    ras2.add_path   (@stroke );
    rgba.ConstrDbl  (0.0 ,0.5 ,0.5 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(ras2 ,@sl ,@ren );

    ras1.reset;
    ras1.add_path(@trans_gb_poly );

    render_scanline_boolean(ras1 ,ras2 );

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

    ras1.reset;
    ras1.add_path   (@stroke );
    rgba.ConstrDbl  (0 ,0 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(ras1 ,@sl ,@ren );

    ras2.reset;
    ras2.add_path   (@curve );
    rgba.ConstrDbl  (0 ,0.6 ,0 ,0.1 );
    ren.color_      (@rgba );
    render_scanlines(ras2 ,@sl ,@ren );

    render_scanline_boolean(ras1 ,ras2 );

    stroke.Destruct;
    glyph.Destruct;
    curve.Destruct;

   end;

 end;

 sl.Destruct;

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

 sl   : scanline_u8;
 ras  ,
 ras2 : rasterizer_scanline_aa;

begin
// Initialize structures
 pixfmt_bgr24(pf ,rbuf_window );

 ren_base.Construct (@pf );
 ren_solid.Construct(@ren_base );

 rgba.ConstrDbl(1 ,1 ,1 );
 ren_base.clear(@rgba );

 sl.Construct;
 ras.Construct;
 ras2.Construct;

// Render the controls
 render_ctrl(@ras ,@sl ,@ren_solid ,@m_polygons );
 render_ctrl(@ras ,@sl ,@ren_solid ,@m_fill_rule );
 render_ctrl(@ras ,@sl ,@ren_solid ,@m_scanline_type );
 render_ctrl(@ras ,@sl ,@ren_solid ,@m_operation );

// Render
 render_sbool(@ras ,@ras2 );

// Free AGG resources
 sl.Destruct;
 ras.Destruct;
 ras2.Destruct;

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
   'This is another example of using of the scanline boolean algebra. The example is '#13 +
   'similar to Demo gpc_test. Note that the cost of the boolean operation with Anti-Aliasing '#13 +
   'is comparable with rendering (the rasterization time is not included). Also note that '#13 +
   'there is a difference in timings between using of scanline_u and scanline_p. Most often '#13 +
   'scanline_u works faster, but it''s because of much less number of produced spans.'#13 +
   'Actually, when using the scanline_u the complexity of the algorithm becomes proportional '#13 +
   'to the area of the polygons, while in scanline_p it''s proportional to the perimeter. '#13 +
   'Of course, the binary variant works much faster than the Anti-Aliased one.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. Scanline Boolean (F1-Help)' );

 if app.init(655 ,520 ,window_resize ) then
  app.run;

 app.Destruct;

END.