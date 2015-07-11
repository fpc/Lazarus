//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 gradient_focal ;

uses
 agg_basics ,
 agg_platform_support ,
 agg_ctrl ,
 agg_slider_ctrl ,
 agg_cbox_ctrl ,
 agg_rendering_buffer ,
 agg_rasterizer_scanline_aa ,
 agg_scanline_u ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_span_allocator ,
 agg_span_gradient ,
 agg_gamma_lut ,
 agg_span_interpolator_linear ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,
 agg_color ,
 agg_trans_affine ,
 agg_render_scanlines ,
 agg_ellipse ,
 agg_conv_stroke ,
 agg_conv_transform ,
 agg_gsv_text ,
 agg_gradient_lut ;

{$I agg_mode.inc }

const
 flip_y = true;

type
 the_application = object(platform_support )
  private
   m_gamma  : slider_ctrl;
   m_extend ,
   m_frwave : cbox_ctrl;

   m_scanline   : scanline_u8;
   m_rasterizer : rasterizer_scanline_aa;
   m_alloc      : span_allocator;

   m_gradient_lut : gradient_lut;
   m_gamma_lut    : gamma_lut;

   m_mouse_x   ,
   m_mouse_y   ,
   m_old_gamma : double;

  public
   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure build_gradient_lut;

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_mouse_move(x ,y : int; flags : unsigned ); virtual;

   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;
   procedure on_ctrl_change; virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_gamma.Construct(5.0 ,5.0 ,340.0 ,12.0 ,not flip_y_ );
 m_extend.Construct(10 ,25 ,'Extended radial focus' ,not flip_y_ );
 m_frwave.Construct(10 ,45 ,'Wavelength radial' ,not flip_y_ );

 m_scanline.Construct;
 m_rasterizer.Construct;
 m_alloc.Construct;
 m_gradient_lut.Construct(1024 );
 m_gamma_lut.Construct_;

 m_mouse_x:=200;
 m_mouse_y:=200;

 m_gamma.range_(0.5 ,2.5 );
 m_gamma.value_(1.8 );
 m_gamma.label_('Gamma = %.3f' );

 add_ctrl(@m_gamma );

 m_gamma.no_transform;

 m_gamma_lut.gamma_(m_gamma._value );

 add_ctrl(@m_extend );

 m_extend.no_transform;

 add_ctrl(@m_frwave );

 m_frwave.no_transform;

 m_old_gamma:=m_gamma._value;

 build_gradient_lut;

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_gamma.Destruct;
 m_extend.Destruct;
 m_frwave.Destruct;

 m_scanline.Destruct;
 m_rasterizer.Destruct;
 m_alloc.Destruct;
 m_gradient_lut.Destruct;
 m_gamma_lut.Destruct;

end;

{ BUILD_GRADIENT_LUT }
procedure the_application.build_gradient_lut;
var
 rgba : aggclr;

begin
 m_gradient_lut.remove_all;

 if not m_frwave._status then
  begin
   rgba.ConstrInt          (m_gamma_lut.dir(0 ) ,m_gamma_lut.dir(255 ) ,m_gamma_lut.dir(0 ) );
   m_gradient_lut.add_color(0.0 ,@rgba );

   rgba.ConstrInt          (m_gamma_lut.dir(120 ) ,m_gamma_lut.dir(0 ) ,m_gamma_lut.dir(0 ) );
   m_gradient_lut.add_color(0.2 ,@rgba );

   rgba.ConstrInt          (m_gamma_lut.dir(120 ) ,m_gamma_lut.dir(120 ) ,m_gamma_lut.dir(0 ) );
   m_gradient_lut.add_color(0.7 ,@rgba );

   rgba.ConstrInt          (m_gamma_lut.dir(0 ) ,m_gamma_lut.dir(0 ) ,m_gamma_lut.dir(255 ) );
   m_gradient_lut.add_color(1.0 ,@rgba );

  end
 else
  begin
   rgba.from_wavelength    (380 ,m_gamma._value );
   m_gradient_lut.add_color(0.0 ,@rgba );

   rgba.from_wavelength    (420 ,m_gamma._value );
   m_gradient_lut.add_color(0.1 ,@rgba );

   rgba.from_wavelength    (460 ,m_gamma._value );
   m_gradient_lut.add_color(0.2 ,@rgba );

   rgba.from_wavelength    (500 ,m_gamma._value );
   m_gradient_lut.add_color(0.3 ,@rgba );

   rgba.from_wavelength    (540 ,m_gamma._value );
   m_gradient_lut.add_color(0.4 ,@rgba );

   rgba.from_wavelength    (580 ,m_gamma._value );
   m_gradient_lut.add_color(0.5 ,@rgba );

   rgba.from_wavelength    (620 ,m_gamma._value );
   m_gradient_lut.add_color(0.6 ,@rgba );

   rgba.from_wavelength    (660 ,m_gamma._value );
   m_gradient_lut.add_color(0.7 ,@rgba );

   rgba.from_wavelength    (700 ,m_gamma._value );
   m_gradient_lut.add_color(0.8 ,@rgba );

   rgba.from_wavelength    (740 ,m_gamma._value );
   m_gradient_lut.add_color(0.9 ,@rgba );

   rgba.from_wavelength    (780 ,m_gamma._value );
   m_gradient_lut.add_color(1.0 ,@rgba );

  end;

 m_gradient_lut.build_lut;

end;

{ ON_INIT }
procedure the_application.on_init;
begin
 m_mouse_y:=_initial_height / 2;
 m_mouse_x:=_initial_width / 2;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;
 rgba : aggclr;

 rb : renderer_base;
 rs : renderer_scanline_aa_solid;
 rg : renderer_scanline_aa;

 e : ellipse;

 estr   : conv_stroke;
 etrans : conv_transform;

 cx ,cy ,r ,fx ,fy ,tm : double;

 gf_std : gradient_radial_focus;
 gf_ext : gradient_radial_focus_extended;

 gradient_adaptor : gradient_reflect_adaptor;

 gradient_mtx : trans_affine;

 the_interpolator : span_interpolator_linear;

 the_gradient : span_gradient;

 buf : array[0..63 ] of char;

 t  : gsv_text;
 pt : conv_stroke;

begin
// Initialize structures
 pixfmt_bgr24(pixf ,rbuf_window );

 rb.Construct(@pixf );
 rs.Construct(@rb );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear      (@rgba );

// Gradient center. All gradient functions assume the
// center being in the origin (0,0) and you can't
// change it. But you can apply arbitrary transformations
// to the gradient (see below).
 cx:=_initial_width / 2;
 cy:=_initial_height / 2;
 r :=100;

// Focal center. Defined in the gradient coordinates,
// that is, with respect to the origin (0,0)
 fx:=m_mouse_x - cx;
 fy:=m_mouse_y - cy;

 if m_extend._status then
  begin
   gf_ext.Construct          (r ,fx ,fy );
   gradient_adaptor.Construct(@gf_ext );

  end
 else
  begin
   gf_std.Construct          (r ,fx ,fy );
   gradient_adaptor.Construct(@gf_std );

  end;

 gradient_mtx.Construct;

// Making the affine matrix. Move to (cx,cy),
// apply the resizing transformations and invert
// the matrix. Gradients and images always assume the
// inverse transformations.
 gradient_mtx.translate(cx ,cy );
 gradient_mtx.multiply (_trans_affine_resizing );
 gradient_mtx.invert;

 the_interpolator.Construct(@gradient_mtx );
 the_gradient.Construct(
  @m_alloc ,
  @the_interpolator ,
  @gradient_adaptor ,
  @m_gradient_lut ,
  0 ,r );

// Form the simple rectangle
 m_rasterizer.reset;
 m_rasterizer.move_to_d(0 ,0 );
 m_rasterizer.line_to_d(_width ,0 );
 m_rasterizer.line_to_d(_width ,_height );
 m_rasterizer.line_to_d(0 ,_height );

// Render the gradient to the whole screen and measure the time
 start_timer;

 rg.Construct    (@rb ,@the_gradient );
 render_scanlines(@m_rasterizer ,@m_scanline ,@rg );

 tm:=elapsed_time;

// Draw the transformed circle that shows the gradient boundary
 e.Construct     (cx ,cy ,r ,r );
 estr.Construct  (@e );
 etrans.Construct(@estr ,_trans_affine_resizing );

 m_rasterizer.add_path(@etrans );

 rgba.ConstrDbl(1 ,1 ,1 );
 rs.color_     (@rgba );

 render_scanlines(@m_rasterizer ,@m_scanline ,@rs );

// Show the gradient time
 t.Construct;
 t.size_(10.0 );

 pt.Construct(@t );
 pt.width_   (1.5 );

 sprintf(@buf[0 ] ,'%3.2f ms' ,tm );

 t.start_point_(25.0 ,70.0 );
 t.text_       (@buf[0 ] );

 m_rasterizer.add_path(@pt );

 rgba.ConstrDbl(0 ,0 ,0 );
 rs.color_     (@rgba );

 render_scanlines(@m_rasterizer ,@m_scanline ,@rs );

// Render the controls
 render_ctrl(@m_rasterizer ,@m_scanline ,@rs ,@m_gamma );
 render_ctrl(@m_rasterizer ,@m_scanline ,@rs ,@m_extend );
 render_ctrl(@m_rasterizer ,@m_scanline ,@rs ,@m_frwave );

// Apply the inverse gamma to the whole buffer
// (transform the colors to the perceptually uniform space)
 pixf.apply_gamma_inv(@m_gamma_lut ,bgr_order );

// Free AGG resources
 the_gradient.Destruct;
 estr.Destruct;
 t.Destruct;
 pt.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
var
 tar : trans_affine_ptr;

begin
 if flags and mouse_left <> 0 then
  begin
   m_mouse_x:=x;
   m_mouse_y:=y;

   tar:=_trans_affine_resizing;

   tar.inverse_transform(tar ,@m_mouse_x ,@m_mouse_y );

   force_redraw;

  end;

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
var
 tar : trans_affine_ptr;

begin
 if flags and mouse_left <> 0 then
  begin
   m_mouse_x:=x;
   m_mouse_y:=y;

   tar:=_trans_affine_resizing;

   tar.inverse_transform(tar ,@m_mouse_x ,@m_mouse_y );

   force_redraw;

  end;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'This demo evolved from testing code and performance measurements.   '#13 +
   'In particular, it shows you how to calculate the parameters'#13 +
   'of a radial gradient with a separate focal point, considering '#13 +
   'arbitrary affine transformations. In this example window resizing '#13 +
   'transformations are taken into account. It also demonstrates '#13 +
   'the use case of gradient_lut and gamma correction.' );

end;

{ ON_CTRL_CHANGE }
procedure the_application.on_ctrl_change;
begin
 m_gamma_lut.gamma_(m_gamma._value );

 build_gradient_lut;

 m_old_gamma:=m_gamma._value;

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. PDF linear and radial gradients (F1-Help)' );

 if app.init(600 ,400 ,window_resize ) then
  app.run;

 app.Destruct;

END.