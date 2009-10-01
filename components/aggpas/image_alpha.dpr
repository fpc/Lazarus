{mac_copy:spheres.bmp}
//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 image_alpha ;

uses
 SysUtils ,

 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,

 agg_ctrl ,
 agg_spline_ctrl ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_u ,
 agg_render_scanlines ,

 agg_ellipse ,
 agg_trans_affine ,
 agg_conv_transform ,
 agg_span_image_filter_rgb ,
 agg_span_interpolator_linear ,
 agg_span_converter ,
 agg_span_allocator ,
 file_utils_ ;

{$I agg_mode.inc }
{$I- }
const
 flip_y = true;

 array_size = 256 * 3;

type
 span_conv_brightness_alpha_rgb8 = object(span_convertor )
   m_alpha_array : int8u_ptr;

   constructor Construct(alpha_array : int8u_ptr );

   procedure convert(span : aggclr_ptr; x ,y : int; len : unsigned ); virtual;

  end;

 the_application = object(platform_support )
   m_alpha : spline_ctrl;

   m_x  ,
   m_y  ,
   m_rx ,
   m_ry : array[0..49 ] of double;

   m_colors : array[0..49 ] of aggclr;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor span_conv_brightness_alpha_rgb8.Construct;
begin
 m_alpha_array:=alpha_array;

end;

{ CONVERT }
procedure span_conv_brightness_alpha_rgb8.convert;
begin
 repeat
  span.a:=
   int8u_ptr(
    ptrcomp(m_alpha_array ) +
    (span.r + span.g + span.b ) * sizeof(int8u ) )^;

  inc(ptrcomp(span ) ,sizeof(aggclr ) );
  dec(len );

 until len = 0;

end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_alpha.Construct(2 ,2 ,200 ,30 ,6 ,not flip_y_ );

 m_alpha.value_(0 ,1.0 );
 m_alpha.value_(1 ,1.0 );
 m_alpha.value_(2 ,1.0 );
 m_alpha.value_(3 ,0.5 );
 m_alpha.value_(4 ,0.5 );
 m_alpha.value_(5 ,1.0 );
 m_alpha.update_spline;

 add_ctrl(@m_alpha );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_alpha.Destruct;

end;

{ ON_INIT }
procedure the_application.on_init;
var
 i : int;

begin
 for i:=0 to 49 do
  begin
   m_x [i ]:=Random($7fff ) mod trunc(_width );
   m_y [i ]:=Random($7fff ) mod trunc(_height );
   m_rx[i ]:=Random($7fff ) mod 60 + 10;
   m_ry[i ]:=Random($7fff ) mod 60 + 10;

   m_colors[i ].ConstrInt(
    Random($7fff ) and $FF ,
    Random($7fff ) and $FF ,
    Random($7fff ) and $FF ,
    Random($7fff ) and $FF );

  end;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;
 rgba : aggclr;

 rb : renderer_base;
 rs : renderer_scanline_aa_solid;
 sa : span_allocator;
 sg : span_image_filter_rgb_bilinear;
 sc : span_converter;
 ri : renderer_scanline_aa;
 sl : scanline_u8;
 tr : conv_transform;

 ras : rasterizer_scanline_aa;
 ell : ellipse;

 interpolator : span_interpolator_linear;

 src_mtx ,img_mtx : trans_affine;

 tat : trans_affine_translation;
 tar : trans_affine_rotation;

 i : unsigned;

 brightness_alpha_array : array[0..array_size - 1 ] of int8u;

 color_alpha : span_conv_brightness_alpha_rgb8;

begin
// Initialize structures
 pixfmt_bgr24(pixf ,rbuf_window );

 rb.Construct(@pixf );
 rs.Construct(@rb );

 rgba.ConstrDbl(1.0 ,1.0 ,1.0 );
 rb.clear      (@rgba );

// Compute
 src_mtx.Construct;

 tat.Construct(-_initial_width / 2 ,-_initial_height / 2 ); src_mtx.multiply(@tat );
 tar.Construct(10.0 * pi / 180.0 ); src_mtx.multiply(@tar );
 tat.Construct(_initial_width / 2 ,_initial_height / 2 ); src_mtx.multiply(@tat );

 src_mtx.multiply(_trans_affine_resizing );

 img_mtx.Construct;
 img_mtx.assign(@src_mtx );
 img_mtx.invert;

 for i:=0 to array_size - 1 do
  brightness_alpha_array[i ]:=
   int8u(trunc(m_alpha._value(i / array_size ) * 255.0 ) );

 color_alpha.Construct(@brightness_alpha_array );

// Render
 sa.Construct;
 interpolator.Construct(@img_mtx );

 rgba.ConstrDbl(1 ,1 ,1 ,0 );
 sg.Construct  (@sa ,rbuf_img(0 ) ,@rgba ,@interpolator ,bgr_order );

 sc.Construct(@sg ,@color_alpha );
 ri.Construct(@rb ,@sc );

 ell.Construct;
 ras.Construct;
 sl.Construct;

 for i:=0 to 49 do
  begin
   ell.init (m_x[i ] ,m_y[i ] ,m_rx[i ] ,m_ry[i ] ,50 );
   rs.color_(@m_colors[i ] );

   ras.add_path    (@ell );
   render_scanlines(@ras ,@sl ,@rs );

  end;

 ell.init(
  _initial_width  / 2.0 ,
  _initial_height / 2.0 ,
  _initial_width  / 1.9 ,
  _initial_height / 1.9 ,200 );

 tr.Construct    (@ell ,@src_mtx );
 ras.add_path    (@tr );
 render_scanlines(@ras ,@sl ,@ri );

// Render the controls
 render_ctrl(@ras ,@sl ,@rs ,@m_alpha );

// Free AGG resources
 ras.Destruct;
 sl.Destruct;

 sa.Destruct;

end;

{ ON_KEY }
procedure the_application.on_key;
var
 fd : text;
 
 i ,alpha : int;

begin
 if key = byte(' ' ) then
  begin
   AssignFile(fd ,'alpha' );
   rewrite   (fd );

   for i:=0 to array_size - 1 do
    begin
     alpha:=
      int8u(trunc(m_alpha._value(i / array_size ) * 255.0 ) );

     if i mod 32 = 0 then
      writeln(fd );

     write(fd ,alpha:3 ,', ' );

    end;

   close(fd );

  end;

 if key = key_f1 then
  message_(
   'A very powerful feature that allows you to simulate the alpha-channel on the basis '#13 +
   'of some functioon. In this example it''s brightness, but it can be of any complexity. '#13 +
   'In the example you can form the brightness function and watch for the translucency.'#13#13 +
   'How to play with:'#13#13 +
   'Resize the windows to move the image over the background.'#13 +
   'Press the spacebar to write down file "alpha" with current alpha values.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;
 buf : array [0..255 ] of char;
 ext : string[10 ];

 img_name ,p ,n ,x : shortstring;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('Image Affine Transformations with Alpha-function (F1-Help)' );

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
  if app.init(app.rbuf_img(0 )._width ,app.rbuf_img(0 )._height ,window_resize ) then
   app.run;

 app.Destruct;

END.