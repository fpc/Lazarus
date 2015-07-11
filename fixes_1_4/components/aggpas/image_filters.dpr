{mac_copy:spheres.bmp}
//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 image_filters ;

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
 file_utils_ ;

{$I agg_mode.inc }

const
 flip_y = true;

 pixfmt     : define_pixfmt = pixfmt_bgr24;
 pixfmt_pre : define_pixfmt = pixfmt_bgr24_pre;

type
 the_application = object(platform_support )
   m_radius      ,
   m_step        : slider_ctrl;
   m_filters     : rbox_ctrl;
   m_normalize   ,
   m_run         ,
   m_single_step ,
   m_refresh     : cbox_ctrl;

   m_cur_angle  : double;
   m_cur_filter ,
   m_num_steps  : int;

   m_num_pix ,
   m_time1   ,
   m_time2   : double;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_draw; virtual;

   procedure transform_image(angle : double );

   procedure on_ctrl_change; virtual;
   procedure on_idle; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
var
 rgba : aggclr;

begin
 inherited Construct(format_ ,flip_y_ );

 m_step.Construct       (115 ,5 ,400 ,11 ,not flip_y_ );
 m_radius.Construct     (115 ,5 + 15 ,400 ,11 + 15 ,not flip_y_ );
 m_filters.Construct    (0.0 ,0.0 ,110.0 ,210.0 ,not flip_y_ );
 m_normalize.Construct  (8.0 ,215.0 ,'Normalize Filter' ,not flip_y_ );
 m_run.Construct        (8.0 ,245.0 ,'RUN Test!' ,not flip_y_ );
 m_single_step.Construct(8.0 ,230.0 ,'Single Step' ,not flip_y_ );
 m_refresh.Construct    (8.0 ,265.0 ,'Refresh' ,not flip_y_ );

 m_cur_angle :=0.0;
 m_cur_filter:=1;
 m_num_steps :=0;

 m_num_pix:=0.0;
 m_time1  :=0;
 m_time2  :=0;

 add_ctrl(@m_radius );
 add_ctrl(@m_step );
 add_ctrl(@m_filters );
 add_ctrl(@m_run );
 add_ctrl(@m_single_step );
 add_ctrl(@m_normalize );
 add_ctrl(@m_refresh );

 m_run.text_size_        (7.5 );
 m_single_step.text_size_(7.5 );
 m_normalize.text_size_  (7.5 );
 m_refresh.text_size_    (7.5 );
 m_normalize.status_    (true );

 m_radius.label_('Filter Radius=%.3f' );
 m_step.label_  ('Step=%3.2f' );
 m_radius.range_(2.0 ,8.0 );
 m_radius.value_(4.0 );
 m_step.range_  (1.0 ,10.0 );
 m_step.value_  (5.0 );

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

 m_step.Destruct;
 m_radius.Destruct;
 m_filters.Destruct;
 m_normalize.Destruct;
 m_run.Destruct;
 m_single_step.Destruct;
 m_refresh.Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;
 rgba : aggclr;

 rb : renderer_base;
 rs : renderer_scanline_aa_solid;

 ras : rasterizer_scanline_aa;
 sl  : scanline_p8;

 t  : gsv_text;
 pt : conv_stroke;

 buf : array[0..63 ] of char;

begin
// Initialize structures
 pixfmt(pixf ,rbuf_window );

 rb.Construct(@pixf );
 rs.Construct(@rb );

 rgba.ConstrDbl(1.0 ,1.0 ,1.0 );
 rb.clear      (@rgba );
 rb.copy_from  (rbuf_img(0 ) ,NIL ,110 ,35 );

 ras.Construct;
 sl.Construct;

// Text
 sprintf(@buf[0 ] ,'NSteps=%d' ,m_num_steps );

 t.Construct;
 t.start_point_(10.0 ,295.0 );
 t.size_       (10.0 );
 t.text_       (@buf[0 ] );

 pt.Construct(@t );
 pt.width_   (1.5 );

 ras.add_path    (@pt );
 rgba.ConstrDbl  (0 ,0 ,0 );
 rs.color_       (@rgba );
 render_scanlines(@ras ,@sl ,@rs );

// Time
 if (m_time1 <> m_time2 ) and
    (m_num_pix > 0.0 ) then
  begin
   sprintf(@buf[0 ] ,'%3.2f Kpix/sec' ,m_num_pix / (m_time2 - m_time1 ) );

   t.start_point_  (10.0 ,310.0 );
   t.text_         (@buf[0 ] );
   ras.add_path    (@pt );
   render_scanlines(@ras ,@sl ,@rs );

  end;

// Render the controls
 if m_filters._cur_item >= 14 then
  render_ctrl(@ras ,@sl ,@rs ,@m_radius );

 render_ctrl(@ras ,@sl ,@rs ,@m_step );
 render_ctrl(@ras ,@sl ,@rs ,@m_filters );
 render_ctrl(@ras ,@sl ,@rs ,@m_run );
 render_ctrl(@ras ,@sl ,@rs ,@m_normalize );
 render_ctrl(@ras ,@sl ,@rs ,@m_single_step );
 render_ctrl(@ras ,@sl ,@rs ,@m_refresh );

// Free AGG resources
 ras.Destruct;
 sl.Destruct;

 t.Destruct;
 pt.Destruct;

end;

{ TRANSFORM_IMAGE }
procedure the_application.transform_image;
var
 width_ ,height_ : double;
 pixf ,pixf_pre  : pixel_formats;

 rb ,rb_pre : renderer_base;

 rgba : aggclr;

 ras : rasterizer_scanline_aa;
 ell : ellipse;

 sl : scanline_u8;
 sa : span_allocator;
 tr : conv_transform;
 fi : image_filter_base_ptr;
 sg : span_image_filter_ptr;
 ri : renderer_scanline_aa;

 filter : image_filter_lut;

 interpolator : span_interpolator_linear;

 src_mtx ,img_mtx : trans_affine;

 tat : trans_affine_translation;
 tar : trans_affine_rotation;

 r : double;

 norm : boolean;

begin
 fi:=NIL;
 sg:=NIL;

// Initialize
 width_ :=rbuf_img(0 )._width;
 height_:=rbuf_img(0 )._height;

 pixfmt    (pixf ,rbuf_img(0 ) );
 pixfmt_pre(pixf_pre , rbuf_img(0 ) );

 rb.Construct    (@pixf );
 rb_pre.Construct(@pixf_pre );

 rgba.ConstrDbl(1.0 ,1.0 ,1.0 );
 rb.clear      (@rgba );

 ras.Construct;
 sl.Construct;
 sa.Construct;

 src_mtx.Construct;

 tat.Construct(-width_ / 2.0 ,-height_ / 2.0 ); src_mtx.multiply(@tat );
 tar.Construct(angle * pi / 180.0 ); src_mtx.multiply(@tar );
 tat.Construct(width_ / 2.0 ,height_ / 2.0 ); src_mtx.multiply(@tat );

 img_mtx.Construct;

 img_mtx:=src_mtx;

 img_mtx.invert;

 r:=width_;

 if height_ < r then
  r:=height_;

 r:=r * 0.5;
 r:=r - 4.0;

 ell.Construct(width_ / 2.0 ,height_ / 2.0 ,r ,r ,200 );
 tr.Construct (@ell ,@src_mtx );

 m_num_pix:=m_num_pix + (r * r * pi );

 interpolator.Construct(@img_mtx );

 filter.Construct;

 norm:=m_normalize._status;

 rgba.ConstrDbl(0 ,0 ,0 ,0 );

// Render
 case m_filters._cur_item of
  0 :
   begin
    sg:=new(
     span_image_filter_rgb_nn_ptr ,
     Construct(
      @sa ,rbuf_img(1 ) ,@rgba ,@interpolator ,bgr_order ) );

    ri.Construct    (@rb_pre ,sg );
    ras.add_path    (@tr );
    render_scanlines(@ras ,@sl ,@ri );

   end;

  1 :
   begin
    sg:=new(
     span_image_filter_rgb_bilinear_ptr ,
     Construct(
      @sa ,rbuf_img(1) ,@rgba ,@interpolator ,bgr_order ) );

    ri.Construct    (@rb_pre ,sg );
    ras.add_path    (@tr );
    render_scanlines(@ras ,@sl ,@ri );

   end;

  5 ,6 ,7 :
   begin
    case m_filters._cur_item of
     5 : fi:=new(image_filter_hanning_ptr ,Construct );
     6 : fi:=new(image_filter_hamming_ptr ,Construct );
     7 : fi:=new(image_filter_hermite_ptr ,Construct );

    end;

    filter.calculate(fi ,norm );

    sg:=new(
     span_image_filter_rgb_2x2_ptr ,
     Construct(
      @sa ,rbuf_img(1) ,@rgba ,@interpolator ,@filter ,bgr_order ) );

    ri.Construct    (@rb_pre ,sg );
    ras.add_path    (@tr );
    render_scanlines(@ras ,@sl ,@ri );

   end;

  2 ,3 ,4 ,8 ,9 ,10 ,11 ,12 ,13 ,14 ,15 ,16 :
   begin
    case m_filters._cur_item of
     2  : fi:=new(image_filter_bicubic_ptr ,Construct );
     3  : fi:=new(image_filter_spline16_ptr ,Construct );
     4  : fi:=new(image_filter_spline36_ptr ,Construct );
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
      @sa ,rbuf_img(1) ,@rgba ,@interpolator ,@filter ,bgr_order ) );

    ri.Construct    (@rb_pre ,sg );
    ras.add_path    (@tr );
    render_scanlines(@ras ,@sl ,@ri );

   end;

 end;

// Free
 ras.Destruct;
 sl.Destruct;
 sa.Destruct;

 filter.Destruct;

 if sg <> NIL then
  dispose(sg ,Destruct );

 if fi <> NIL then
  dispose(fi );

end;

{ ON_CTRL_CHANGE }
procedure the_application.on_ctrl_change;
begin
 if m_single_step._status then
  begin
   m_cur_angle:=m_cur_angle + m_step._value;

   copy_img_to_img(1 ,0 );
   transform_image(m_step._value );

   inc(m_num_steps );

   force_redraw;

   m_single_step.status_(false );

  end;

 if m_run._status then
  begin
   start_timer;

   m_time1:=elapsed_time;
   m_time2:=m_time1;

   m_num_pix:=0.0;

   wait_mode_(false );

  end;

 if m_refresh._status or
    (m_filters._cur_item <> m_cur_filter ) then
  begin
   start_timer;

   m_time1:=0;
   m_time2:=0;

   m_num_pix  :=0.0;
   m_cur_angle:=0.0;

   copy_img_to_img(1 ,2 );
   transform_image(0.0 );

   m_refresh.status_(false );

   m_cur_filter:=m_filters._cur_item;
   m_num_steps :=0;

   force_redraw;

  end;

end;

{ ON_IDLE }
procedure the_application.on_idle;
begin
 if m_run._status then
  begin
   if m_cur_angle < 360.0 then
    begin
     m_cur_angle:=m_cur_angle + m_step._value;

     copy_img_to_img(1 ,0 );
     start_timer;
     transform_image(m_step._value );

     m_time2:=m_time2 + elapsed_time;

     inc(m_num_steps );

    end
   else
    begin
     m_cur_angle:=0.0;

     wait_mode_   (true );
     m_run.status_(false );

    end;

   force_redraw;

  end
 else
  wait_mode_(true );

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'The image transformer algorithm can work with different interpolation filters, '#13 +
   'such as Bilinear, Bicubic, Sinc, Blackman. The example demonstrates the difference '#13 +
   'in quality between different filters. When switch the "Run Test" on, the image '#13 +
   'starts rotating. But at each step there is the previously rotated image taken, '#13 +
   'so the quality degrades. This degradation as well as the performance depend on '#13 +
   'the type of the interpolation filter.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;
 buf : array [0..255 ] of char;
 ext : string[10 ];

 img_name ,p ,n ,x : shortstring;

 w ,h : unsigned;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('Image transformation filters comparison (F1-Help)' );

 img_name:='spheres';

{$IFDEF WIN32 }
 if ParamCount > 0 then
  begin
   spread_name(ParamStr(1 ) ,p ,n ,x );

   img_name:=fold_name(p ,n ,'' );

  end;

{$ENDIF }

 if not app.load_img(0 ,img_name ) then
  begin
   img_name:=img_name + #0;
   ext     :=app._img_ext + #0;

   if img_name = 'spheres'#0 then
    begin
     sprintf(@buf[0 ]             ,'File not found: %s' ,ptrcomp(@img_name[1 ] ) );
     sprintf(@buf[StrLen(@buf ) ] ,'%s. '#13'Download http://www.antigrain.com/' ,ptrcomp(@ext[1 ] ) );
     sprintf(@buf[StrLen(@buf ) ] ,'%s' ,ptrcomp(@img_name[1 ] ) );
     sprintf(@buf[StrLen(@buf ) ] ,'%s'#13'or copy it from another directory if available.' ,ptrcomp(@ext[1 ] ) );

    end
   else
    begin
     sprintf(@buf[0 ]             ,'File not found: %s' ,ptrcomp(@img_name[1 ] ) );
     sprintf(@buf[StrLen(@buf ) ] ,'%s' ,ptrcomp(@ext[1 ] ) );

    end;

   app.message_(@buf[0 ] );

  end
 else
  begin
   app.copy_img_to_img(1 ,0 );
   app.copy_img_to_img(2 ,0 );
   app.transform_image(0.0 );

   w:=app.rbuf_img(0 )._width + 110;
   h:=app.rbuf_img(0 )._height + 40;

   if w < 305 then
    w:=305;

   if h < 325 then
    h:=325;

   if app.init(w ,h ,0 ) then
    app.run;

  end;

 app.Destruct;

END.
