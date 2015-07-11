//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 rasterizers2 ;

{DEFINE AGG_GRAY8 }
{$DEFINE AGG_BGR24 }
{DEFINE AGG_RGB24 }
{DEFINE AGG_BGRA32 }
{DEFINE AGG_RGBA32 }
{DEFINE AGG_ARGB32 }
{DEFINE AGG_ABGR32 }
{DEFINE AGG_RGB565 }
{DEFINE AGG_RGB555 }

uses
 Math ,SysUtils ,

 agg_basics ,
 agg_platform_support ,

 agg_ctrl ,
 agg_slider_ctrl ,
 agg_cbox_ctrl ,

 agg_rasterizer_scanline_aa ,
 agg_rasterizer_outline ,
 agg_rasterizer_outline_aa ,
 agg_scanline ,
 agg_scanline_p ,

 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_renderer_primitives ,
 agg_renderer_outline_aa ,
 agg_renderer_outline_image ,
 agg_render_scanlines ,

 agg_gsv_text ,
 agg_conv_stroke ,
 agg_conv_transform ,
 agg_vertex_source ,
 agg_math_stroke ,
 agg_trans_affine ,
 agg_pattern_filters_rgba ,
 agg_gamma_functions

{$I pixel_formats.inc }
{$I agg_mode.inc }

const
 flip_y = true;

 pixmap_chain : array[0..113 ] of int32u = (
  16 ,7 ,
  $00ffffff ,$00ffffff ,$00ffffff ,$00ffffff ,$b4c29999 ,$ff9a5757 ,$ff9a5757 ,
  $ff9a5757 ,$ff9a5757 ,$ff9a5757 ,$ff9a5757 ,$b4c29999 ,$00ffffff ,$00ffffff ,
  $00ffffff ,$00ffffff ,$00ffffff ,$00ffffff ,$0cfbf9f9 ,$ff9a5757 ,$ff660000 ,
  $ff660000 ,$ff660000 ,$ff660000 ,$ff660000 ,$ff660000 ,$ff660000 ,$ff660000 ,
  $b4c29999 ,$00ffffff ,$00ffffff ,$00ffffff ,$00ffffff ,$5ae0cccc ,$ffa46767 ,
  $ff660000 ,$ff975252 ,$7ed4b8b8 ,$5ae0cccc ,$5ae0cccc ,$5ae0cccc ,$5ae0cccc ,
  $a8c6a0a0 ,$ff7f2929 ,$ff670202 ,$9ecaa6a6 ,$5ae0cccc ,$00ffffff ,$ff660000 ,
  $ff660000 ,$ff660000 ,$ff660000 ,$ff660000 ,$ff660000 ,$a4c7a2a2 ,$3affff00 ,
  $3affff00 ,$ff975151 ,$ff660000 ,$ff660000 ,$ff660000 ,$ff660000 ,$ff660000 ,
  $ff660000 ,$00ffffff ,$5ae0cccc ,$ffa46767 ,$ff660000 ,$ff954f4f ,$7ed4b8b8 ,
  $5ae0cccc ,$5ae0cccc ,$5ae0cccc ,$5ae0cccc ,$a8c6a0a0 ,$ff7f2929 ,$ff670202 ,
  $9ecaa6a6 ,$5ae0cccc ,$00ffffff ,$00ffffff ,$00ffffff ,$0cfbf9f9 ,$ff9a5757 ,
  $ff660000 ,$ff660000 ,$ff660000 ,$ff660000 ,$ff660000 ,$ff660000 ,$ff660000 ,
  $ff660000 ,$b4c29999 ,$00ffffff ,$00ffffff ,$00ffffff ,$00ffffff ,$00ffffff ,
  $00ffffff ,$00ffffff ,$b4c29999 ,$ff9a5757 ,$ff9a5757 ,$ff9a5757 ,$ff9a5757 ,
  $ff9a5757 ,$ff9a5757 ,$b4c29999 ,$00ffffff ,$00ffffff ,$00ffffff ,$00ffffff );

type
 pattern_pixmap_argb32 = object(pixel_source )
   m_pixmap : int32u_ptr;

   constructor Construct(pixmap : int32u_ptr );

   function  _width : unsigned; virtual;
   function  _height : unsigned; virtual;

   function  pixel(x ,y : int ) : rgba8; virtual;

  end;

 spiral = object(vertex_source )
   m_x ,m_y ,m_r1 ,m_r2 ,
   m_step   ,m_start_angle ,
   m_angle  ,m_curr_r ,m_da ,m_dr : double;

   m_start : boolean;

   constructor Construct(x ,y ,r1 ,r2 ,step : double; start_angle : double = 0 );

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 the_application = object(platform_support )
   m_step   ,
   m_width  : slider_ctrl;
   m_test   ,
   m_rotate ,

   m_accurate_joins ,
   m_scale_pattern  : cbox_ctrl;

   m_start_angle : double;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure draw_aliased_pix_accuracy   (ras : rasterizer_outline_ptr; prim : renderer_primitives_ptr );
   procedure draw_aliased_subpix_accuracy(ras : rasterizer_outline_ptr; prim : renderer_primitives_ptr );

   procedure draw_anti_aliased_outline(
              ras : rasterizer_outline_aa_ptr;
              ren : renderer_outline_aa_ptr );

   procedure draw_anti_aliased_scanline(
              ras : rasterizer_scanline_aa_ptr;
              sl  : scanline_ptr;
              ren : renderer_scanline_aa_solid_ptr );

   procedure draw_anti_aliased_outline_img(
              ras : rasterizer_outline_aa_ptr;
              ren : renderer_outline_ptr );

   procedure text(
              ras : rasterizer_scanline_aa_ptr;
              sl  : scanline_ptr;
              ren : renderer_scanline_aa_solid_ptr;
              x ,y : double; txt : PChar );

   procedure on_draw; virtual;
   procedure on_idle; virtual;

   procedure on_ctrl_change; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor pattern_pixmap_argb32.Construct;
begin
 m_pixmap:=pixmap;

end;

{ _WIDTH }
function pattern_pixmap_argb32._width;
begin
 result:=m_pixmap^;

end;

{ _HEIGHT }
function pattern_pixmap_argb32._height;
begin
 result:=int32u_ptr(ptrcomp(m_pixmap ) + sizeof(int32u ) )^;

end;

{ PIXEL }
function pattern_pixmap_argb32.pixel;
var
 p : int32u;

begin
 p:=int32u_ptr(ptrcomp(m_pixmap ) + (y * _width + x + 2 ) * sizeof(int32u ) )^;

 result.r:=(p shr 16 ) and $FF;
 result.g:=(p shr 8 ) and $FF;
 result.b:=p and $FF;
 result.a:=p shr 24;

end;

{ ROUNDOFF }
procedure roundoff(this : trans_affine_ptr; x ,y : double_ptr );
begin
 x^:=Floor(x^ );
 y^:=Floor(y^ );

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

 m_da:=deg2rad(8.0 );
 m_dr:=m_step / 45.0;

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
  result:=path_cmd_stop

 else
  begin
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

end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_step.Construct  (10.0 ,10.0 + 4.0 ,150.0 ,10.0 + 8.0 + 4.0 ,not flip_y_ );
 m_width.Construct (150.0 + 10.0 ,10.0 + 4.0 ,400 - 10.0 ,10.0 + 8.0 + 4.0 ,not flip_y_ );
 m_test.Construct  (10.0 ,10.0 + 4.0 + 16.0 ,'Test Performance' ,not flip_y_ );
 m_rotate.Construct(130 + 10.0 ,10.0 + 4.0 + 16.0 ,'Rotate' ,not flip_y_ );

 m_accurate_joins.Construct(200 + 10.0 ,10.0 + 4.0 + 16.0 ,'Accurate Joins' ,not flip_y_ );
 m_scale_pattern.Construct (310 + 10.0 ,10.0 + 4.0 + 16.0 ,'Scale Pattern' ,not flip_y_ );

 m_start_angle:=0.0;

 add_ctrl(@m_step );

 m_step.range_(0.0 ,2.0 );
 m_step.value_(0.1 );
 m_step.label_('Step=%1.2f' );
 m_step.no_transform;

 add_ctrl(@m_width );

 m_width.range_(0.0 ,7.0 );
 m_width.value_(3.0 );
 m_width.label_('Width=%1.2f' );
 m_width.no_transform;

 add_ctrl(@m_test );

 m_test.text_size_(9.0 ,7.0 );
 m_test.no_transform;

 add_ctrl(@m_rotate );

 m_rotate.text_size_(9.0 ,7.0 );
 m_rotate.no_transform;

 add_ctrl(@m_accurate_joins);

 m_accurate_joins.text_size_(9.0 ,7.0 );
 m_accurate_joins.no_transform;

 add_ctrl(@m_scale_pattern );

 m_scale_pattern.text_size_(9.0 ,7.0 );
 m_scale_pattern.status_   (true );
 m_scale_pattern.no_transform;

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_step.Destruct;
 m_width.Destruct;
 m_test.Destruct;
 m_rotate.Destruct;

 m_accurate_joins.Destruct;
 m_scale_pattern.Destruct;

end;

{ DRAW_ANTI_ALIASED_OUTLINE }
procedure the_application.draw_anti_aliased_outline;
var
 s3 : spiral;

 rgba : aggclr;

begin
 s3.Construct(_width / 5 ,_height - _height / 4 + 20 ,5 ,70 ,8 ,m_start_angle );

 rgba.ConstrDbl(0.4 ,0.3 ,0.1 );
 ren.color_    (@rgba );
 ras.add_path  (@s3 );

end;

{ DRAW_ANTI_ALIASED_SCANLINE }
procedure the_application.draw_anti_aliased_scanline;
var
 s4 : spiral;

 rgba : aggclr;

 stroke : conv_stroke;

begin
 s4.Construct(_width / 2 ,_height - _height / 4 + 20 ,5 ,70 ,8 ,m_start_angle );

 stroke.Construct(@s4 );
 stroke.width_   (m_width._value );
 stroke.line_cap_(round_cap );

 rgba.ConstrDbl(0.4 ,0.3 ,0.1 );
 ren.color_    (@rgba );

 ras.add_path    (@stroke );
 render_scanlines(ras ,sl ,ren );

 stroke.Destruct;

end;

{ DRAW_ALIASED_PIX_ACCURACY }
procedure the_application.draw_aliased_pix_accuracy;
var
 s1 : spiral;
 rn : trans_affine;

 rgba  : aggclr;
 trans : conv_transform;

begin
 s1.Construct(_width / 5 ,_height / 4 + 50 ,5 ,70 ,8 ,m_start_angle );
 rn.Construct(@roundoff );

 trans.Construct(@s1 ,@rn );

 rgba.ConstrDbl  (0.4 ,0.3 ,0.1 );
 prim.line_color_(@rgba );
 ras.add_path    (@trans );

end;

{ DRAW_ALIASED_SUBPIX_ACCURACY }
procedure the_application.draw_aliased_subpix_accuracy;
var
 s2 : spiral;

 rgba : aggclr;

begin
 s2.Construct(_width / 2 ,_height / 4 + 50 ,5 ,70 ,8 ,m_start_angle );

 rgba.ConstrDbl  (0.4 ,0.3 ,0.1 );
 prim.line_color_(@rgba );
 ras.add_path    (@s2 );

end;

{ DRAW_ANTI_ALIASED_OUTLINE_IMG }
procedure the_application.draw_anti_aliased_outline_img;
var
 s5 : spiral;

begin
 s5.Construct(_width - _width / 5 ,_height - _height / 4 + 20 ,5 ,70 ,8 ,m_start_angle );
 ras.add_path(@s5 );

end;

{ TEXT }
procedure the_application.text;
var
 t : gsv_text;

 stroke : conv_stroke;
 rgba   : aggclr;

begin
 t.Construct;
 t.size_(8 );
 t.text_(txt );

 t.start_point_  (x ,y );
 stroke.Construct(@t );
 stroke.width_   (0.7 );

 ras.add_path  (@stroke );
 rgba.ConstrDbl(0 ,0 ,0 );
 ren.color_    (@rgba );

 render_scanlines(ras ,sl ,ren );

 t.Destruct;
 stroke.Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pf : pixel_formats;

 ren_base : renderer_base;
 ren_aa   : renderer_scanline_aa_solid;
 ren_prim : renderer_primitives;
 ras_aa   : rasterizer_scanline_aa;
 ras_al   : rasterizer_outline;
 sl       : scanline_p8;

 rgba : aggclr;

 prof    : line_profile_aa;
 ren_oaa : renderer_outline_aa;
 ras_oaa : rasterizer_outline_aa;

 filter     : pattern_filter_bilinear_rgba;
 src        : pattern_pixmap_argb32;
 src_scaled : line_image_scale;
 pattern    : line_image_pattern_pow2;

 ren_img : renderer_outline_image;
 ras_img : rasterizer_outline_aa;

 width_  : double;
 profile : line_profile_aa;
 gm_pw   : gamma_power;

 pixf     : pixel_formats;
 base_ren : renderer_base;
 ren      : renderer_outline_aa;
 ras      : rasterizer_outline_aa;

 fltr     : pattern_filter_bilinear_rgba;
 patt_src : pattern_pixmap_argb32;
 patt     : line_image_pattern_pow2;
 renimg   : renderer_outline_image;
 rasimg   : rasterizer_outline_aa;

begin
// Initialize structures
 pixfmt(pf ,rbuf_window );

 ren_base.Construct(@pf );
 ren_aa.Construct  (@ren_base );
 ren_prim.Construct(@ren_base );

 ras_aa.Construct;
 sl.Construct;
 ras_al.Construct(@ren_prim );

 prof.Construct;
 prof.width_(m_width._value );

 ren_oaa.Construct(@ren_base ,@prof );
 ras_oaa.Construct(@ren_oaa );

 ras_oaa.accurate_join_(m_accurate_joins._status );
 ras_oaa.round_cap_    (true );

// Image pattern
 filter.Construct;
 src.Construct(@pixmap_chain );

 src_scaled.Construct(@src ,m_width._value );
 pattern.Construct   (@filter );

 if m_scale_pattern._status then
  pattern.create(@src_scaled )
 else
  pattern.create(@src );

 ren_img.Construct(@ren_base ,@pattern );

 if m_scale_pattern._status then
  ren_img.scale_x_(m_width._value / src._height );

 ras_img.Construct(@ren_img );

// Circles
 rgba.ConstrDbl(1.0 ,1.0 ,0.95 );
 ren_base.clear(@rgba );

 draw_aliased_pix_accuracy    (@ras_al  ,@ren_prim );
 draw_aliased_subpix_accuracy (@ras_al  ,@ren_prim );
 draw_anti_aliased_outline    (@ras_oaa ,@ren_oaa );
 draw_anti_aliased_scanline   (@ras_aa  ,@sl ,@ren_aa );
 draw_anti_aliased_outline_img(@ras_img ,@ren_img );

// Text
 text(@ras_aa ,@sl ,@ren_aa ,50 ,80 ,'Bresenham lines,'#13#13'regular accuracy' );
 text(@ras_aa ,@sl ,@ren_aa ,_width / 2 - 50 ,80 ,'Bresenham lines,'#13#13'subpixel accuracy' );
 text(@ras_aa ,@sl ,@ren_aa ,50 ,_height / 2 + 50 ,'Anti-aliased lines' );
 text(@ras_aa ,@sl ,@ren_aa ,_width / 2 - 50 ,_height / 2 + 50 ,'Scanline rasterizer' );
 text(@ras_aa ,@sl ,@ren_aa ,_width - _width / 5 - 50 ,_height / 2 + 50 ,'Arbitrary Image Pattern' );

// Render the controls
 render_ctrl(@ras_aa ,@sl ,@ren_aa ,@m_step );
 render_ctrl(@ras_aa ,@sl ,@ren_aa ,@m_width );
 render_ctrl(@ras_aa ,@sl ,@ren_aa ,@m_test );
 render_ctrl(@ras_aa ,@sl ,@ren_aa ,@m_rotate );
 render_ctrl(@ras_aa ,@sl ,@ren_aa ,@m_accurate_joins );
 render_ctrl(@ras_aa ,@sl ,@ren_aa ,@m_scale_pattern );

// An example of using anti-aliased outline rasterizer.
// Uncomment it to see the result
{ width_:=5.0 + m_width.value - 3.0;

 profile.Construct;
 gm_pw.Construct(1.2 );

 profile.gamma          (@gm_pw );     //optional
 profile._min_width     (0.75 );       //optional
 profile._smoother_width(3.0);         //optional
 profile.width          (width_ );     //mandatory!

 pixfmt(pixf ,rbuf_window );

 base_ren.Construct(@pixf );
 ren.Construct     (@base_ren ,@profile );

 rgba.ConstrInt(0 ,0 ,0 );
 ren._color    (@rgba );               //mandatory!

 ras.Construct     (@ren );
 ras._round_cap    (true );            //optional
 ras._accurate_join(true );            //optional

 ras.move_to_d(100 ,100 );
 ras.line_to_d(150 ,200 );
 ras.render   (false );                // false means "don't close the polygon", i.e. polyline

 ras.Destruct;
 profile.Destruct;{}

// An example of using image pattern outline rasterizer
// Uncomment it to see the result
{ fltr.Construct;                      // Filtering functor

 patt_src.Construct(@pixmap_chain );  // Source. Must have an interface:
                                      // width() const
                                      // height() const
                                      // pixel(int x, int y) const
                                      // Any agg::renderer_base<> or derived
                                      // is good for the use as a source.

// agg::line_image_pattern is the main container for the patterns. It creates
// a copy of the patterns extended according to the needs of the filter.
// agg::line_image_pattern can operate with arbitrary image width, but if the
// width of the pattern is power of 2, it's better to use the modified
// version agg::line_image_pattern_pow2 because it works about 15-25 percent
// faster than agg::line_image_pattern (because of using simple masking instead
// of expensive '%' operation).
 patt.Construct(@fltr ,@src );

 pixfmt(pixf ,rbuf_window );

 base_ren.Construct(@pixf );
 renimg.Construct  (@base_ren ,@patt );
 //renimg._scale_x   (1.3 );            // Optional

 rasimg.Construct(@renimg );
 rasimg.move_to_d(100 ,150 );
 rasimg.line_to_d(0 ,0 );
 rasimg.line_to_d(300 ,200 );
 rasimg.render   (false );

 patt.Destruct;
 rasimg.Destruct;{}

// Free AGG resources
 ras_aa.Destruct;
 sl.Destruct;

 prof.Destruct;
 ras_oaa.Destruct;

 pattern.Destruct;
 ras_img.Destruct;

end;

{ ON_IDLE }
procedure the_application.on_idle;
begin
 m_start_angle:=m_start_angle + deg2rad(m_step._value );

 if m_start_angle > deg2rad(360.0 ) then
  m_start_angle:=m_start_angle - deg2rad(360.0 );

 force_redraw;

end;

{ ON_CTRL_CHANGE }
procedure the_application.on_ctrl_change;
var
 i : unsigned;

 t2 ,t3 ,t4 ,t5 : double;

 buf : array[0..255 ] of char;

 pf : pixel_formats;

 ren_base : renderer_base;
 ren_aa   : renderer_scanline_aa_solid;
 ren_prim : renderer_primitives;
 ras_aa   : rasterizer_scanline_aa;
 ras_al   : rasterizer_outline;
 sl       : scanline_p8;

 rgba : aggclr;

 prof    : line_profile_aa;
 ren_oaa : renderer_outline_aa;
 ras_oaa : rasterizer_outline_aa;

 filter     : pattern_filter_bilinear_rgba;
 src        : pattern_pixmap_argb32;
 src_scaled : line_image_scale;
 pattern    : line_image_pattern_pow2;

 ren_img : renderer_outline_image;
 ras_img : rasterizer_outline_aa;

begin
 wait_mode_(not m_rotate._status );

 if m_test._status then
  begin
   on_draw;
   update_window;

  // Initialize structures
   pixfmt(pf ,rbuf_window );

   ren_base.Construct(@pf );
   ren_aa.Construct  (@ren_base );
   ren_prim.Construct(@ren_base );

   ras_aa.Construct;
   sl.Construct;
   ras_al.Construct(@ren_prim );

   prof.Construct;
   prof.width_(m_width._value );

   ren_oaa.Construct(@ren_base ,@prof );
   ras_oaa.Construct(@ren_oaa );

   ras_oaa.accurate_join_(m_accurate_joins._status );
   ras_oaa.round_cap_    (true );

  // Image pattern
   filter.Construct;
   src.Construct(@pixmap_chain );

   src_scaled.Construct(@src ,m_width._value );
   pattern.Construct   (@filter );

   if m_scale_pattern._status then
    pattern.create(@src_scaled )
   else
    pattern.create(@src );

   ren_img.Construct(@ren_base ,@pattern );

   if m_scale_pattern._status then
    ren_img.scale_x_(src._height / m_width._value );

   ras_img.Construct(@ren_img );

  // Do Test
   start_timer;

   for i:=1 to 200 do
    begin
     draw_aliased_subpix_accuracy(@ras_al ,@ren_prim );

     m_start_angle:=m_start_angle + deg2rad(m_step._value );

    end;

   t2:=elapsed_time;

   start_timer;

   for i:=1 to 200 do
    begin
     draw_anti_aliased_outline(@ras_oaa ,@ren_oaa );

     m_start_angle:=m_start_angle + deg2rad(m_step._value );

    end;

   t3:=elapsed_time;

   start_timer;

   for i:=1 to 200 do
    begin
     draw_anti_aliased_scanline(@ras_aa ,@sl ,@ren_aa );

     m_start_angle:=m_start_angle + deg2rad(m_step._value );

    end;

   t4:=elapsed_time;

   start_timer;

   for i:=1 to 200 do
    begin
     draw_anti_aliased_outline_img(@ras_img ,@ren_img );

     m_start_angle:=m_start_angle + deg2rad(m_step._value );

    end;

   t5:=elapsed_time;

  // Display results
   m_test.status_(false );
   force_redraw;

   sprintf(@buf[0 ] ,'Aliased=%1.2fms, '#0 ,t2 );
   sprintf(@buf[StrLen(@buf[0 ] ) ] ,'Anti-Aliased=%1.2fms, '#0 ,t3 );
   sprintf(@buf[StrLen(@buf[0 ] ) ] ,'Scanline=%1.2fms, '#0 ,t4 );
   sprintf(@buf[StrLen(@buf[0 ] ) ] ,'Image-Pattern=%1.2fms'#0 ,t5 );

   message_(@buf[0 ] );

  // Free AGG resources
   ras_aa.Destruct;
   sl.Destruct;

   prof.Destruct;
   ras_oaa.Destruct;

   pattern.Destruct;
   ras_img.Destruct;

  end;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'More complex example demostrating different rasterizers. Here you can see how '#13 +
   'the outline rasterizer works, and how to use an image as the line pattern. '#13 +
   'This capability can be very useful to draw geographical maps.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format ,flip_y );
 app.caption_ ('AGG Example. Line Join (F1-Help)' );

 if app.init(500 ,450 ,0 ) then
  app.run;

 app.Destruct;

END.