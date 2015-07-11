//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 rasterizer_compound ;

uses
 agg_basics ,
 agg_color ,
 agg_array ,
 agg_platform_support ,
 agg_ctrl ,
 agg_slider_ctrl ,
 agg_cbox_ctrl ,
 agg_ellipse ,
 agg_gamma_lut ,
 agg_renderer_base ,
 agg_rendering_buffer ,
 agg_rasterizer_scanline_aa ,
 agg_rasterizer_compound_aa ,
 agg_conv_curve ,
 agg_conv_stroke ,
 agg_scanline_u ,
 agg_renderer_scanline ,
 agg_span_allocator ,
 agg_pixfmt ,
 agg_pixfmt_rgba ,
 agg_path_storage ,
 agg_trans_affine ,
 agg_conv_transform ;

{$I agg_mode.inc }

const
 flip_y = true;

type
 style_handler_ = object(style_handler )
  private
   m_transparent : aggclr;

   m_styles : aggclr_ptr;
   m_count  : unsigned;

  public
   constructor Construct(styles : aggclr_ptr; count : unsigned );

   function  is_solid(style : unsigned ) : boolean; virtual;
   function  color   (style : unsigned ) : aggclr_ptr; virtual;

   procedure generate_span(span : aggclr_ptr; x ,y : int; len ,style : unsigned ); virtual;

  end;

 the_application = object(platform_support )
  private
   m_width  ,
   m_alpha1 ,
   m_alpha2 ,
   m_alpha3 ,
   m_alpha4 : slider_ctrl;

   m_invert_order : cbox_ctrl;

   m_path : path_storage;

  public
   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure compose_path;

   procedure on_draw; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor style_handler_.Construct(styles : aggclr_ptr; count : unsigned );
begin
 m_transparent.ConstrInt(0 ,0 ,0 ,0 );

 m_styles:=styles;
 m_count :=count;

end;

{ IS_SOLID }
function style_handler_.is_solid(style : unsigned ) : boolean;
begin
 result:=true;

end;

{ COLOR }
function style_handler_.color(style : unsigned ) : aggclr_ptr;
begin
 if style < m_count then
  result:=aggclr_ptr(ptrcomp(m_styles ) + style * sizeof(aggclr ) )
 else
  result:=@m_transparent;

end;

{ GENERATE_SPAN }
procedure style_handler_.generate_span(span : aggclr_ptr; x ,y : int; len ,style : unsigned );
begin
end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_width.Construct (180 + 10.0 ,5.0 ,130 + 300.0 ,12 ,not flip_y_ );
 m_alpha1.Construct(5          ,5   ,180         ,12 ,not flip_y_ );
 m_alpha2.Construct(5          ,25  ,180         ,32 ,not flip_y_ );
 m_alpha3.Construct(5          ,45  ,180         ,52 ,not flip_y_ );
 m_alpha4.Construct(5          ,65  ,180         ,72 ,not flip_y_ );

 m_invert_order.Construct(190 ,25 ,'Invert Z-Order' );
 m_path.Construct;

 add_ctrl(@m_width );

 m_width.range_(-20.0 ,50.0 );
 m_width.value_(10.0 );
 m_width.label_('Width=%1.2f' );

 add_ctrl(@m_alpha1 );

 m_alpha1.range_(0 ,1 );
 m_alpha1.value_(1 );
 m_alpha1.label_('Alpha1=%1.3f' );

 add_ctrl(@m_alpha2 );

 m_alpha2.range_(0 ,1 );
 m_alpha2.value_(1 );
 m_alpha2.label_('Alpha2=%1.3f' );

 add_ctrl(@m_alpha3 );

 m_alpha3.range_(0 ,1 );
 m_alpha3.value_(1 );
 m_alpha3.label_('Alpha3=%1.3f' );

 add_ctrl(@m_alpha4 );

 m_alpha4.range_(0 ,1 );
 m_alpha4.value_(1 );
 m_alpha4.label_('Alpha4=%1.3f' );

 add_ctrl(@m_invert_order );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_width.Destruct;
 m_alpha1.Destruct;
 m_alpha2.Destruct;
 m_alpha3.Destruct;
 m_alpha4.Destruct;

 m_invert_order.Destruct;
 m_path.Destruct;

end;

{ COMPOSE_PATH }
procedure the_application.compose_path;
begin
 m_path.remove_all;
 m_path.move_to(28.47 ,6.45  );
 m_path.curve3 (21.58 ,1.12  ,19.82 ,0.29  );
 m_path.curve3 (17.19 ,-0.93 ,14.21 ,-0.93 );
 m_path.curve3 (9.57  ,-0.93 ,6.57  ,2.25  );
 m_path.curve3 (3.56  ,5.42  ,3.56  ,10.60 );
 m_path.curve3 (3.56  ,13.87 ,5.03  ,16.26 );
 m_path.curve3 (7.03  ,19.58 ,11.99 ,22.51 );
 m_path.curve3 (16.94 ,25.44 ,28.47 ,29.64 );
 m_path.line_to(28.47 ,31.40 );
 m_path.curve3 (28.47 ,38.09 ,26.34 ,40.58 );
 m_path.curve3 (24.22 ,43.07 ,20.17 ,43.07 );
 m_path.curve3 (17.09 ,43.07 ,15.28 ,41.41 );
 m_path.curve3 (13.43 ,39.75 ,13.43 ,37.60 );
 m_path.line_to(13.53 ,34.77 );
 m_path.curve3 (13.53 ,32.52 ,12.38 ,31.30 );
 m_path.curve3 (11.23 ,30.08 ,9.38  ,30.08 );
 m_path.curve3 (7.57  ,30.08 ,6.42  ,31.35 );
 m_path.curve3 (5.27  ,32.62 ,5.27  ,34.81 );
 m_path.curve3 (5.27  ,39.01 ,9.57  ,42.53 );
 m_path.curve3 (13.87 ,46.04 ,21.63 ,46.04 );
 m_path.curve3 (27.59 ,46.04 ,31.40 ,44.04 );
 m_path.curve3 (34.28 ,42.53 ,35.64 ,39.31 );
 m_path.curve3 (36.52 ,37.21 ,36.52 ,30.71 );
 m_path.line_to(36.52 ,15.53 );
 m_path.curve3 (36.52 ,9.13  ,36.77 ,7.69  );
 m_path.curve3 (37.01 ,6.25  ,37.57 ,5.76  );
 m_path.curve3 (38.13 ,5.27  ,38.87 ,5.27  );
 m_path.curve3 (39.65 ,5.27  ,40.23 ,5.62  );
 m_path.curve3 (41.26 ,6.25  ,44.19 ,9.18  );
 m_path.line_to(44.19 ,6.45  );
 m_path.curve3 (38.72 ,-0.88 ,33.74 ,-0.88 );
 m_path.curve3 (31.35 ,-0.88 ,29.93 ,0.78  );
 m_path.curve3 (28.52 ,2.44  ,28.47 ,6.45  );
 m_path.close_polygon;

 m_path.move_to(28.47 ,9.62  );
 m_path.line_to(28.47 ,26.66 );
 m_path.curve3 (21.09 ,23.73 ,18.95 ,22.51 );
 m_path.curve3 (15.09 ,20.36 ,13.43 ,18.02 );
 m_path.curve3 (11.77 ,15.67 ,11.77 ,12.89 );
 m_path.curve3 (11.77 ,9.38  ,13.87 ,7.06  );
 m_path.curve3 (15.97 ,4.74  ,18.70 ,4.74  );
 m_path.curve3 (22.41 ,4.74  ,28.47 ,9.62  );
 m_path.close_polygon;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf ,pixf_pre : pixel_formats;

 rgba ,c1 ,c2 : aggclr;

 renb ,renb_pre : renderer_base;

 rs : renderer_scanline_aa_solid;
 gr : pod_vector;
 i  : unsigned;

 lut  : gamma_lut;
 ras  : rasterizer_scanline_aa;
 rasc : rasterizer_compound_aa_dbl;

 sl : scanline_u8;
 sh : style_handler_;

 alloc : span_allocator;

 mtx : trans_affine;
 tas : trans_affine_scaling;
 tat : trans_affine_translation;

 trans : conv_transform;
 curve : conv_curve;

 stroke ,str_ell : conv_stroke_math;

 styles : array[0..3 ] of aggclr;

 ell : ellipse;

begin
// Initialize structures
 lut.Construct(2.0 );

 pixfmt_bgra32 (pixf ,rbuf_window );
 renb.Construct(@pixf );
 rs.Construct  (@renb );

 pixfmt_bgra32_pre (pixf_pre ,rbuf_window );
 renb_pre.Construct(@pixf_pre );

// Clear the window with a gradient
 gr.Create(sizeof(aggclr ) ,pixf._width );

 i:=0;

 while i < pixf._width do
  begin
   c1.ConstrInt(255 ,255 ,0 );
   c2.ConstrInt(0   ,255 ,255 );

   rgba:=c1.gradient(@c2 ,i / pixf._width );

   gr.add(@rgba );
   inc   (i );

  end;

 i:=0;

 while i < pixf._height do
  begin
   renb.copy_color_hspan(0 ,i ,pixf._width ,gr.array_operator(0 ) );

   inc(i );

  end;

 pixf.apply_gamma_dir(@lut ,bgra_order );

 ras.Construct;
 rasc.Construct;
 sl.Construct;
 alloc.Construct;

// Draw two triangles
 ras.move_to_d(0 ,0 );
 ras.line_to_d(_width ,0 );
 ras.line_to_d(_width ,_height );

 rgba.ConstrInt(lut.dir(0 ) ,lut.dir(100 ) ,lut.dir(0 ) );

 render_scanlines_aa_solid(@ras ,@sl ,@renb ,@rgba );

 ras.move_to_d(0 ,0 );
 ras.line_to_d(0 ,_height );
 ras.line_to_d(_width ,0 );

 rgba.ConstrInt(lut.dir(0 ) ,lut.dir(100 ) ,lut.dir(100 ) );

 render_scanlines_aa_solid(@ras ,@sl ,@renb ,@rgba );

 mtx.Construct;
 tas.Construct(4.0 );
 mtx.multiply (@tas );
 tat.Construct(150 ,100 );
 mtx.multiply (@tat );

 trans.Construct (@m_path ,@mtx );
 curve.Construct (@trans );
 stroke.Construct(@curve );

 compose_path;

 if m_invert_order._status then
  rasc.layer_order(layer_inverse )
 else
  rasc.layer_order(layer_direct );

 styles[3 ].ConstrInt(lut.dir(255 ) ,lut.dir(0 ) ,lut.dir(108 ) ,200 );
 styles[3 ].premultiply;
 styles[2 ].ConstrInt(lut.dir(51 ) ,lut.dir(0 ) ,lut.dir(151 ) ,180 );
 styles[2 ].premultiply;
 styles[1 ].ConstrInt(lut.dir(143 ) ,lut.dir(90 ) ,lut.dir(6 ) ,200 );
 styles[1 ].premultiply;
 styles[0 ].ConstrInt(lut.dir(0 ) ,lut.dir(0 ) ,lut.dir(255 ) ,220 );
 styles[0 ].premultiply;

 sh.Construct (@styles[0 ] ,4 );
 stroke.width_(m_width._value );

 rasc.reset;
 rasc.master_alpha(3 ,m_alpha1._value );
 rasc.master_alpha(2 ,m_alpha2._value );
 rasc.master_alpha(1 ,m_alpha3._value );
 rasc.master_alpha(0 ,m_alpha4._value );

 ell.Construct    (220.0 ,180.0 ,120.0 ,10.0 ,128 ,false );
 str_ell.Construct(@ell );
 str_ell.width_   (m_width._value / 2 );

 rasc.styles  (3 ,-1 );
 rasc.add_path(@str_ell );

 rasc.styles  (2 ,-1 );
 rasc.add_path(@ell );

 rasc.styles  (1 ,-1 );
 rasc.add_path(@stroke );

 rasc.styles  (0 ,-1 );
 rasc.add_path(@curve );

 render_scanlines_compound_layered(@rasc ,@sl ,@renb_pre ,@alloc ,@sh );

// Render the controls
 render_ctrl(@ras ,@sl ,@rs ,@m_width );
 render_ctrl(@ras ,@sl ,@rs ,@m_alpha1 );
 render_ctrl(@ras ,@sl ,@rs ,@m_alpha2 );
 render_ctrl(@ras ,@sl ,@rs ,@m_alpha3 );
 render_ctrl(@ras ,@sl ,@rs ,@m_alpha4 );
 render_ctrl(@ras ,@sl ,@rs ,@m_invert_order );

 pixf.apply_gamma_inv(@lut ,bgra_order );

// Free AGG resources
 lut.Destruct;
 gr.Destruct;

 ras.Destruct;
 rasc.Destruct;
 sl.Destruct;
 alloc.Destruct;

 curve.Destruct;
 stroke.Destruct;
 str_ell.Destruct;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'This simple example demonstrates a rather advanced technique of using     '#13 +
   'the compound rasterizer. The idea is you assign styles to the polygons '#13 +
   '(left=style, right=-1) and rasterize this "multi-styled" compound shape '#13 +
   'as a whole. If the polygons in the shape overlap, the greater styles '#13 +
   'have higher priority. That is, the result is as if greater styles were '#13 +
   'painted last, but the geometry is flattened before rendering. It means '#13 +
   'there are no pixels will be painted twice. Then the style are associated '#13 +
   'with colors, gradients, images, etc. in a special style handler. '#13 +
   'It simulates Constructive Solid Geometry so that, you can, for example '#13 +
   'draw a translucent fill plus translucent stroke without the overlapped '#13 +
   'part of the fill being visible through the stroke.' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgra32 ,flip_y );
 app.caption_ ('AGG Example. Compound Rasterizer -- Geometry Flattening (F1-Help)' );

 if app.init(440 ,330 ,0 ) then
  app.run;

 app.Destruct;

END.