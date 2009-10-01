//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 image_filters2 ;

uses
 SysUtils ,

 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,

 agg_ctrl ,
 agg_slider_ctrl ,
 agg_rbox_ctrl ,
 agg_cbox_ctrl ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_u ,
 agg_scanline_p ,
 agg_render_scanlines ,

 agg_ellipse ,
 agg_trans_affine ,
 agg_conv_transform ,
 agg_conv_stroke ,
 agg_span_allocator ,
 agg_span_interpolator_linear ,
 agg_span_image_filter ,
 agg_span_image_filter_rgb ,
 agg_image_filters ,
 agg_gsv_text ,
 agg_gamma_lut ,
 agg_path_storage ;

{$I agg_mode.inc }

const
 flip_y = true;

 g_image : array[0..47 ] of int8u = (

   0,255,0,     0,0,255,     255,255,255,   255,0,0,
   255,0,0,     0,0,0,       255,255,255,   255,255,255,
   255,255,255, 255,255,255, 0,0,255,       255,0,0,
   0,0,255,     255,255,255, 0,0,0,         0,255,0 );

type
 the_application = object(platform_support )
   m_gamma  ,
   m_radius : slider_ctrl;

   m_filters   : rbox_ctrl;
   m_normalize : cbox_ctrl;

   m_cur_angle  : double;
   m_cur_filter ,
   m_num_steps  : int;

   m_num_pix ,
   m_time1   ,
   m_time2   : double;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_draw; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
var
 rgba : aggclr;

begin
 inherited Construct(format_ ,flip_y_ );

 m_gamma.Construct    (115 ,5 ,500 - 5 ,11 ,not flip_y_ );
 m_radius.Construct   (115 ,5 + 15 ,500 - 5 ,11 + 15 ,not flip_y_ );
 m_filters.Construct  (0.0 ,0.0 ,110.0 ,210.0 ,not flip_y_ );
 m_normalize.Construct(8.0 ,215.0 ,'Normalize Filter' ,not flip_y_ );

 m_cur_angle :=0.0;
 m_cur_filter:=1;
 m_num_steps :=0;

 m_num_pix:=0.0;
 m_time1  :=0;
 m_time2  :=0;

 add_ctrl(@m_gamma );
 add_ctrl(@m_radius );
 add_ctrl(@m_filters );
 add_ctrl(@m_normalize );

 m_normalize.text_size_(7.5 );
 m_normalize.status_  (true );

 m_radius.label_('Filter Radius=%.3f' );
 m_radius.range_(2.0 ,8.0 );
 m_radius.value_(4.0 );

 m_gamma.label_('Gamma=%.3f' );
 m_gamma.range_(0.5 ,3.0 );
 m_gamma.value_(1.0 );

 m_filters.add_item ('simple (NN)' );
 m_filters.add_item ('bilinear' );
 m_filters.add_item ('bicubic' );
 m_filters.add_item ('spline16' );
 m_filters.add_item ('spline36' );
 m_filters.add_item ('hanning' );
 m_filters.add_item ('hamming' );
 m_filters.add_item ('hermite' );
 m_filters.add_item ('kaiser' );
 m_filters.add_item ('quadric' );
 m_filters.add_item ('catrom' );
 m_filters.add_item ('gaussian' );
 m_filters.add_item ('bessel' );
 m_filters.add_item ('mitchell' );
 m_filters.add_item ('sinc' );
 m_filters.add_item ('lanczos' );
 m_filters.add_item ('blackman' );
 m_filters.cur_item_(1 );

 m_filters.border_width_    (0 ,0 );
 rgba.ConstrDbl             (0.0 ,0.0 ,0.0 ,0.1 );
 m_filters.background_color_(@rgba );
 m_filters.text_size_       (6.0 );
 m_filters.text_thickness_  (0.85 );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_gamma.Destruct;
 m_radius.Destruct;
 m_filters.Destruct;
 m_normalize.Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
const
 para : array[0..7 ] of double = (200 ,40 ,200 + 300 ,40 ,200 + 300 ,40 + 300 ,200 ,40 + 300 );

var
 pixf : pixel_formats;
 rgba : aggclr;

 rb : renderer_base;
 rs : renderer_scanline_aa_solid;
 sl : scanline_u8;
 sa : span_allocator;
 ri : renderer_scanline_aa;
 sg : span_image_filter_ptr;
 fi : image_filter_base_ptr;

 ras : rasterizer_scanline_aa;

 norm : boolean;

 gamma  : gamma_lut;
 stroke : conv_stroke;
 filter : image_filter_lut;

 img_rbuf : rendering_buffer;
 img_mtx  : trans_affine;
 weights  : int16_ptr;

 interpolator : span_interpolator_linear;

 x_start ,x_end ,y_start ,y_end ,x_center ,x ,ys ,radius ,dx ,dy ,xs : double;

 i ,n ,nn : unsigned;

 p : path_storage;

begin
 sg:=NIL;
 fi:=NIL;

// Initialize structures
 pixfmt_bgr24(pixf ,rbuf_window );

 rb.Construct(@pixf );
 rs.Construct(@rb );

 rgba.ConstrDbl(1.0 ,1.0 ,1.0 );
 rb.clear      (@rgba );
 rb.copy_from  (rbuf_img(0 ) ,0 ,110 ,35 );

 ras.Construct;
 sl.Construct;
 img_rbuf.Construct(@g_image[0 ] ,4 ,4 ,4 * 3 );

// Render
 img_mtx.Construct(@para[0 ] ,0 ,0 ,4 ,4 );

 interpolator.Construct(@img_mtx );
 sa.Construct;

 ras.reset;
 ras.move_to_d(para[0 ] ,para[1 ] );
 ras.line_to_d(para[2 ] ,para[3 ] );
 ras.line_to_d(para[4 ] ,para[5 ] );
 ras.line_to_d(para[6 ] ,para[7 ] );

 rgba.ConstrDbl(1 ,1 ,1 );
 filter.Construct;

 case m_filters._cur_item of
  0 :
   begin
    sg:=new(
     span_image_filter_rgb_nn_ptr ,
     Construct(
      @sa ,@img_rbuf ,@rgba ,@interpolator ,bgr_order ) );

    ri.Construct    (@rb ,sg );
    render_scanlines(@ras ,@sl ,@ri );

   end;

  1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,10 ,11 ,12 ,13 ,14 ,15 ,16 :
   begin
    norm:=m_normalize._status;

    case m_filters._cur_item of
     1  : fi:=new(image_filter_bilinear_ptr ,Construct );
     2  : fi:=new(image_filter_bicubic_ptr ,Construct );
     3  : fi:=new(image_filter_spline16_ptr ,Construct );
     4  : fi:=new(image_filter_spline36_ptr ,Construct );
     5  : fi:=new(image_filter_hanning_ptr ,Construct );
     6  : fi:=new(image_filter_hamming_ptr ,Construct );
     7  : fi:=new(image_filter_hermite_ptr ,Construct );
     8  : fi:=new(image_filter_kaiser_ptr ,Construct );
     9  : fi:=new(image_filter_quadric_ptr ,Construct );
     10 : fi:=new(image_filter_catrom_ptr ,Construct );
     11 : fi:=new(image_filter_gaussian_ptr ,Construct );
     12 : fi:=new(image_filter_bessel_ptr ,Construct );
     13 : fi:=new(image_filter_mitchell_ptr ,Construct );
     14 : fi:=new(image_filter_sinc_ptr ,Construct(m_radius._value ) );
     15 : fi:=new(image_filter_lanczos_ptr ,Construct(m_radius._value ) );
     16 : fi:=new(image_filter_blackman_ptr ,Construct(m_radius._value ) );

    end;

    filter.calculate(fi ,norm );

    sg:=new(
     span_image_filter_rgb_ptr ,
     Construct(
      @sa ,@img_rbuf ,@rgba ,@interpolator ,@filter ,bgr_order ) );

    ri.Construct    (@rb ,sg );
    render_scanlines(@ras ,@sl ,@ri );

   // Draw Graph
    gamma.Construct     (m_gamma._value );
    pixf.apply_gamma_inv(@gamma ,bgr_order );

    x_start :=5.0;
    x_end   :=195.0;
    y_start :=235.0;
    y_end   :=_initial_height - 5.0;
    x_center:=(x_start + x_end ) / 2;

    p.Construct;
    stroke.Construct(@p );
    stroke.width_   (0.8 );

    for i:=0 to 16 do
     begin
      x:=x_start + (x_end - x_start ) * i / 16.0;

      p.remove_all;
      p.move_to(x + 0.5 ,y_start );
      p.line_to(x + 0.5 ,y_end );

      ras.add_path(@stroke);

      if i = 8 then
       rgba.ConstrInt(0 ,0 ,0 ,255 )
      else
       rgba.ConstrInt(0 ,0 ,0 ,100 );

      rs.color_       (@rgba );
      render_scanlines(@ras ,@sl ,@rs );

     end;

    ys:=y_start + (y_end - y_start ) / 6.0;

    p.remove_all;
    p.move_to(x_start ,ys );
    p.line_to(x_end ,ys );

    ras.add_path    (@stroke);
    rgba.ConstrInt  (0 ,0 ,0 );
    rs.color_       (@rgba );
    render_scanlines(@ras ,@sl ,@rs );

    radius:=filter.radius;

    n :=trunc(radius * 256 * 2 );
    dx:=(x_end - x_start ) * radius / 8.0;
    dy:=y_end - ys;

    weights:=filter.weight_array;

    xs:=(x_end + x_start ) / 2.0 - (filter.diameter * (x_end - x_start ) / 32.0 );
    nn:=filter.diameter * 256;

    p.remove_all();
    p.move_to(xs + 0.5 ,ys + dy * int16_ptr(weights )^ / image_filter_size );

    i:=1;

    while i < nn do
     begin
      p.line_to(
       xs + dx * i / n + 0.5 ,
       ys + dy * int16_ptr(ptrcomp(weights ) + i * sizeof(int16 ) )^ / image_filter_size );

      inc(i );

     end;

    ras.add_path    (@stroke );
    rgba.ConstrInt  (100 ,0 ,0 );
    rs.color_       (@rgba );
    render_scanlines(@ras ,@sl ,@rs );

   // Free
    gamma.Destruct;
    p.Destruct;
    stroke.Destruct;

   end;

  end;

// Render the controls
 render_ctrl(@ras ,@sl ,@rs ,@m_gamma );

 if m_filters._cur_item >= 14 then
  render_ctrl(@ras ,@sl ,@rs ,@m_radius );

 render_ctrl(@ras ,@sl ,@rs ,@m_filters );
 render_ctrl(@ras ,@sl ,@rs ,@m_normalize );

// Free AGG resources
 ras.Destruct;
 sl.Destruct;
 img_rbuf.Destruct;

 sa.Destruct;
 filter.Destruct;

 if sg <> NIL then
  dispose(sg ,Destruct );

 if fi <> NIL then
  dispose(fi );

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'Another example that demonstrates the difference of image filters. '#13 +
   'It just displays a simple 4x4 pixels image with huge zoom. '#13 +
   'You can see how different filters affect the result. '#13 +
   'Also see how gamma correction works.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('Image transformation filters comparison (F1-Help)' );

 if app.init(500 ,340 ,0 ) then
  app.run;

 app.Destruct;

END.