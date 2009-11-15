//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 image_fltr_graph ;

uses
 Math ,

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
 agg_path_storage ;

{$I agg_mode.inc }

const
 flip_y = true;

type
 filter_adaptor_ptr = ^filter_adaptor;
 filter_adaptor = object
   m_filter : image_filter_base_ptr;

   constructor Construct(filter : image_filter_base_ptr );
   destructor  Destruct;

   function  radius : double;
   function  calc_weight(x : double ) : double;

   procedure set_radius(r : double );

  end;

 the_application = object(platform_support )
   m_radius   : slider_ctrl;
   m_bilinear ,
   m_bicubic  ,
   m_spline16 ,
   m_spline36 ,
   m_hanning  ,
   m_hamming  ,
   m_hermite  ,
   m_kaiser   ,
   m_quadric  ,
   m_catrom   ,
   m_gaussian ,
   m_bessel   ,
   m_mitchell ,
   m_sinc     ,
   m_lanczos  ,
   m_blackman : cbox_ctrl;

   m_filters : array[0..31 ] of cbox_ctrl_ptr;

   m_filter_bilinear ,
   m_filter_bicubic  ,
   m_filter_spline16 ,
   m_filter_spline36 ,
   m_filter_hanning  ,
   m_filter_hamming  ,
   m_filter_hermite  ,
   m_filter_kaiser   ,
   m_filter_quadric  ,
   m_filter_catrom   ,
   m_filter_gaussian ,
   m_filter_bessel   ,
   m_filter_mitchell ,
   m_filter_sinc     ,
   m_filter_lanczos  ,
   m_filter_blackman : filter_adaptor;

   m_filter_func : array[0..31 ] of filter_adaptor_ptr;
   m_num_filters : unsigned;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_draw; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor filter_adaptor.Construct;
begin
 m_filter:=filter;

end;

{ DESTRUCT }
destructor filter_adaptor.Destruct;
begin
 if m_filter <> NIL then
  dispose(m_filter );

end;

{ RADIUS }
function filter_adaptor.radius;
begin
 if m_filter <> NIL then
  result:=m_filter.radius
 else
  result:=0;

end;

{ CALC_WEIGHT }
function filter_adaptor.calc_weight;
begin
 if m_filter <> NIL then
  result:=m_filter.calc_weight(Abs(x ) )
 else
  result:=0;

end;

{ SET_RADIUS }
procedure filter_adaptor.set_radius;
begin
 if m_filter <> NIL then
  m_filter.set_radius(r );

end;

{ CONSTRUCT }
constructor the_application.Construct;
var
 i : unsigned;

begin
 inherited Construct(format_ ,flip_y_ );

 m_radius.Construct  (5.0 ,5.0 ,780 - 5 ,10.0 ,not flip_y_ );
 m_bilinear.Construct(8.0 ,30.0 + 15 * 0  ,'bilinear' ,not flip_y_ );
 m_bicubic.Construct (8.0 ,30.0 + 15 * 1  ,'bicubic ' ,not flip_y_ );
 m_spline16.Construct(8.0 ,30.0 + 15 * 2  ,'spline16' ,not flip_y_ );
 m_spline36.Construct(8.0 ,30.0 + 15 * 3  ,'spline36' ,not flip_y_ );
 m_hanning.Construct (8.0 ,30.0 + 15 * 4  ,'hanning ' ,not flip_y_ );
 m_hamming.Construct (8.0 ,30.0 + 15 * 5  ,'hamming ' ,not flip_y_ );
 m_hermite.Construct (8.0 ,30.0 + 15 * 6  ,'hermite ' ,not flip_y_ );
 m_kaiser.Construct  (8.0 ,30.0 + 15 * 7  ,'kaiser  ' ,not flip_y_ );
 m_quadric.Construct (8.0 ,30.0 + 15 * 8  ,'quadric ' ,not flip_y_ );
 m_catrom.Construct  (8.0 ,30.0 + 15 * 9  ,'catrom  ' ,not flip_y_ );
 m_gaussian.Construct(8.0 ,30.0 + 15 * 10 ,'gaussian' ,not flip_y_ );
 m_bessel.Construct  (8.0 ,30.0 + 15 * 11 ,'bessel  ' ,not flip_y_ );
 m_mitchell.Construct(8.0 ,30.0 + 15 * 12 ,'mitchell' ,not flip_y_ );
 m_sinc.Construct    (8.0 ,30.0 + 15 * 13 ,'sinc    ' ,not flip_y_ );
 m_lanczos.Construct (8.0 ,30.0 + 15 * 14 ,'lanczos ' ,not flip_y_ );
 m_blackman.Construct(8.0 ,30.0 + 15 * 15 ,'blackman' ,not flip_y_ );

 m_filter_bilinear.Construct(new(image_filter_bilinear_ptr ,Construct ) );
 m_filter_bicubic.Construct (new(image_filter_bicubic_ptr ,Construct ) );
 m_filter_spline16.Construct(new(image_filter_spline16_ptr ,Construct ) );
 m_filter_spline36.Construct(new(image_filter_spline36_ptr ,Construct ) );
 m_filter_hanning.Construct (new(image_filter_hanning_ptr ,Construct ) );
 m_filter_hamming.Construct (new(image_filter_hamming_ptr ,Construct ) );
 m_filter_hermite.Construct (new(image_filter_hermite_ptr ,Construct ) );
 m_filter_kaiser.Construct  (new(image_filter_kaiser_ptr ,Construct ) );
 m_filter_quadric.Construct (new(image_filter_quadric_ptr ,Construct ) );
 m_filter_catrom.Construct  (new(image_filter_catrom_ptr ,Construct ) );
 m_filter_gaussian.Construct(new(image_filter_gaussian_ptr ,Construct ) );
 m_filter_bessel.Construct  (new(image_filter_bessel_ptr ,Construct ) );
 m_filter_mitchell.Construct(new(image_filter_mitchell_ptr ,Construct ) );
 m_filter_sinc.Construct    (new(image_filter_sinc_ptr ,Construct(2.0 ) ) );
 m_filter_lanczos.Construct (new(image_filter_lanczos_ptr ,Construct(2.0 ) ) );
 m_filter_blackman.Construct(new(image_filter_blackman_ptr ,Construct(2.0 ) ) );

 m_num_filters:=0;

 m_filters[m_num_filters ]:=@m_bilinear; inc(m_num_filters );
 m_filters[m_num_filters ]:=@m_bicubic;  inc(m_num_filters );
 m_filters[m_num_filters ]:=@m_spline16; inc(m_num_filters );
 m_filters[m_num_filters ]:=@m_spline36; inc(m_num_filters );
 m_filters[m_num_filters ]:=@m_hanning;  inc(m_num_filters );
 m_filters[m_num_filters ]:=@m_hamming;  inc(m_num_filters );
 m_filters[m_num_filters ]:=@m_hermite;  inc(m_num_filters );
 m_filters[m_num_filters ]:=@m_kaiser;   inc(m_num_filters );
 m_filters[m_num_filters ]:=@m_quadric;  inc(m_num_filters );
 m_filters[m_num_filters ]:=@m_catrom;   inc(m_num_filters );
 m_filters[m_num_filters ]:=@m_gaussian; inc(m_num_filters );
 m_filters[m_num_filters ]:=@m_bessel;   inc(m_num_filters );
 m_filters[m_num_filters ]:=@m_mitchell; inc(m_num_filters );
 m_filters[m_num_filters ]:=@m_sinc;     inc(m_num_filters );
 m_filters[m_num_filters ]:=@m_lanczos;  inc(m_num_filters );
 m_filters[m_num_filters ]:=@m_blackman; inc(m_num_filters );

 i:=0;

 m_filter_func[i ]:=@m_filter_bilinear; inc(i );
 m_filter_func[i ]:=@m_filter_bicubic;  inc(i );
 m_filter_func[i ]:=@m_filter_spline16; inc(i );
 m_filter_func[i ]:=@m_filter_spline36; inc(i );
 m_filter_func[i ]:=@m_filter_hanning;  inc(i );
 m_filter_func[i ]:=@m_filter_hamming;  inc(i );
 m_filter_func[i ]:=@m_filter_hermite;  inc(i );
 m_filter_func[i ]:=@m_filter_kaiser;   inc(i );
 m_filter_func[i ]:=@m_filter_quadric;  inc(i );
 m_filter_func[i ]:=@m_filter_catrom;   inc(i );
 m_filter_func[i ]:=@m_filter_gaussian; inc(i );
 m_filter_func[i ]:=@m_filter_bessel;   inc(i );
 m_filter_func[i ]:=@m_filter_mitchell; inc(i );
 m_filter_func[i ]:=@m_filter_sinc;     inc(i );
 m_filter_func[i ]:=@m_filter_lanczos;  inc(i );
 m_filter_func[i ]:=@m_filter_blackman; inc(i );

 for i:=0 to m_num_filters - 1 do
  add_ctrl(m_filters[i ] );

 m_radius.range_(2.0 ,8.0 );
 m_radius.value_(4.0 );
 m_radius.label_('Radius=%.3f' );

 add_ctrl(@m_radius );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_radius.Destruct;
 m_bilinear.Destruct;
 m_bicubic.Destruct;
 m_spline16.Destruct;
 m_spline36.Destruct;
 m_hanning.Destruct;
 m_hamming.Destruct;
 m_hermite.Destruct;
 m_kaiser.Destruct;
 m_quadric.Destruct;
 m_catrom.Destruct;
 m_gaussian.Destruct;
 m_bessel.Destruct;
 m_mitchell.Destruct;
 m_sinc.Destruct;
 m_lanczos.Destruct;
 m_blackman.Destruct;

 m_filter_bilinear.Destruct;
 m_filter_bicubic.Destruct;
 m_filter_spline16.Destruct;
 m_filter_spline36.Destruct;
 m_filter_hanning.Destruct;
 m_filter_hamming.Destruct;
 m_filter_hermite.Destruct;
 m_filter_kaiser.Destruct;
 m_filter_quadric.Destruct;
 m_filter_catrom.Destruct;
 m_filter_gaussian.Destruct;
 m_filter_bessel.Destruct;
 m_filter_mitchell.Destruct;
 m_filter_sinc.Destruct;
 m_filter_lanczos.Destruct;
 m_filter_blackman.Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;
 rgba : aggclr;

 rb : renderer_base;
 rs : renderer_scanline_aa_solid;
 sl : scanline_p8;

 ras : rasterizer_scanline_aa;

 normalized : image_filter_lut;
 weights    : int16_ptr;

 x_start ,x_end ,y_start ,y_end ,x_center ,x ,y ,ys ,radius ,dy ,xs ,dx ,sum ,xf : double;

 xfract ,ir : int;

 i ,j ,n ,xint ,nn : unsigned;

 p  : path_storage;
 pl : conv_stroke;
 tr : conv_transform;

begin
// Initialize structures
 pixfmt_bgr24(pixf ,rbuf_window );

 rb.Construct(@pixf );
 rs.Construct(@rb );

 rgba.ConstrDbl(1.0 ,1.0 ,1.0 );
 rb.clear      (@rgba );

 ras.Construct;
 sl.Construct;

// Render
 x_start :=125.0;
 x_end   :=_initial_width - 15.0;
 y_start :=10.0;
 y_end   :=_initial_height - 10.0;
 x_center:=(x_start + x_end ) / 2;

 p.Construct;
 pl.Construct(@p );
 tr.Construct(@pl ,_trans_affine_resizing );

 for i:=0 to 15 do
  begin
   x:=x_start + (x_end - x_start ) * i / 16.0;

   p.remove_all;
   p.move_to(x + 0.5 ,y_start );
   p.line_to(x + 0.5 ,y_end );

   ras.add_path(@tr );

   if i = 8 then
    rgba.ConstrInt(0 ,0 ,0 ,255 )
   else
    rgba.ConstrInt(0 ,0 ,0 ,100 );

   rs.color_       (@rgba );
   render_scanlines(@ras ,@sl ,@rs );

  end;

 ys:=y_start + (y_end - y_start ) / 6.0;

 p.remove_all;
 p.move_to       (x_start ,ys );
 p.line_to       (x_end ,ys );
 ras.add_path    (@tr );
 rgba.ConstrInt  (0 ,0 ,0 );
 rs.color_       (@rgba );
 render_scanlines(@ras ,@sl ,@rs );

 pl.width_(1.0 );

 for i:=0 to m_num_filters - 1 do
  if m_filters[i ]._status then
   begin
    m_filter_func[i ].set_radius(m_radius._value );

    radius:=m_filter_func[i ].radius;

    n :=trunc(radius * 256 * 2 );
    dy:=y_end - ys;

    xs:=(x_end + x_start ) / 2.0 - (radius * (x_end - x_start ) / 16.0 );
    dx:=(x_end - x_start ) * radius / 8.0;

    p.remove_all;
    p.move_to(xs + 0.5 ,ys + dy * m_filter_func[i ].calc_weight(-radius ) );

    j:=1;

    while j < n do
     begin
      p.line_to(
       xs + dx * j / n + 0.5 ,
       ys + dy * m_filter_func[i ].calc_weight(j / 256.0 - radius ) );

      inc(j );

     end;

    ras.add_path    (@tr );
    rgba.ConstrInt  (100 ,0 ,0 );
    rs.color_       (@rgba );
    render_scanlines(@ras ,@sl ,@rs );

    p.remove_all;

    ir:=trunc(Ceil(radius ) + 0.1 );

    for xint:=0 to 255 do
     begin
      sum:=0;

      xfract:=-ir;

      while xfract < ir do
       begin
        xf:=xint / 256.0 + xfract;

        if (xf >= -radius ) or
           (xf <= radius ) then
         sum:=sum + m_filter_func[i ].calc_weight(xf );

        inc(xfract );

       end;

      x:=x_center + ((-128.0 + xint ) / 128.0 ) * radius * (x_end - x_start ) / 16.0;
      y:=ys + sum * 256 - 256;

      if xint = 0 then
       p.move_to(x ,y )
      else
       p.line_to(x ,y );

     end;

    ras.add_path    (@tr );
    rgba.ConstrInt  (0 ,100 ,0 );
    rs.color_       (@rgba );
    render_scanlines(@ras ,@sl ,@rs );

    normalized.Construct(m_filter_func[i ].m_filter );

    weights:=normalized.weight_array;

    xs:=(x_end + x_start ) / 2.0 - (normalized.diameter * (x_end - x_start ) / 32.0 );
    nn:=normalized.diameter * 256;

    p.remove_all;
    p.move_to(xs + 0.5 ,ys + dy * int16_ptr(weights )^ / image_filter_size );

    j:=1;

    while j < nn do
     begin
      p.line_to(
       xs + dx * j / n + 0.5 ,
       ys + dy * int16_ptr(ptrcomp(weights ) + j * sizeof(int16 ) )^ / image_filter_size );

      inc(j );

     end;

    ras.add_path    (@tr );
    rgba.ConstrInt  (0 ,0 ,100 ,255 );
    rs.color_       (@rgba );
    render_scanlines(@ras ,@sl ,@rs );

   // Free
    normalized.Destruct;

   end;

// Render the controls
 for i:=0 to m_num_filters - 1 do
  render_ctrl(@ras ,@sl ,@rs ,m_filters[i ] );

 if m_sinc._status or
    m_lanczos._status or
    m_blackman._status then
  render_ctrl(@ras ,@sl ,@rs ,@m_radius );

// Free AGG resources
 ras.Destruct;
 sl.Destruct;

 p.Destruct;
 pl.Destruct;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'Demonstration of the shapes of different interpolation filters. '#13 +
   'Just in case if you are curious.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('Image filters'' shape comparison (F1-Help)' );

 if app.init(780 ,300 ,window_resize ) then
  app.run;

 app.Destruct;

END.