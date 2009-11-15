{mac_copy:spheres.bmp}
//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 image1 ;

{$DEFINE AGG_BGR24 }

uses
 SysUtils ,

 agg_basics ,
 agg_platform_support ,

 agg_ctrl ,
 agg_slider_ctrl ,

 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_u ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_render_scanlines ,

 agg_ellipse ,
 agg_trans_affine ,
 agg_conv_transform ,
 agg_span_allocator ,
 agg_span_interpolator_linear ,
 agg_span_image_filter ,
 agg_span_image_filter_rgb ,
 agg_span_image_filter_rgba ,
 agg_span_image_filter_gray ,
 agg_image_filters ,
 file_utils_

{$I pixel_formats.inc }
{$I agg_mode.inc }

const
 flip_y = true;

type
 the_application = object(platform_support )
   m_angle ,
   m_scale : slider_ctrl;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_draw; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_angle.Construct(5 ,5 ,300 ,12 ,not flip_y_ );
 m_scale.Construct(5 ,5 + 15 ,300 ,12 + 15 ,not flip_y_ );

 add_ctrl(@m_angle );
 add_ctrl(@m_scale );

 m_angle.label_('Angle=%3.2f' );
 m_scale.label_('Scale=%3.2f' );
 m_angle.range_(-180.0 ,180.0 );
 m_angle.value_(0.0 );
 m_scale.range_(0.1 ,5.0 );
 m_scale.value_(1.0 );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_angle.Destruct;
 m_scale.Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf ,pixf_pre : pixel_formats;

 rgba : aggclr;

 rb ,rb_pre : renderer_base;

 rs : renderer_scanline_aa_solid;
 pf : rasterizer_scanline_aa;
 sl : scanline_u8;
 sa : span_allocator;
 sg : span_image_filter_ptr;
 ri : renderer_scanline_aa;
 tr : conv_transform;
 fi : image_filter;

 filter : image_filter_base_ptr;

 interpolator : span_interpolator_linear;

 src_mtx ,img_mtx : trans_affine;

 tat : trans_affine_translation;
 tar : trans_affine_rotation;
 tas : trans_affine_scaling;

 ell : ellipse;

 r : double;

begin
 sg    :=NIL;
 filter:=NIL;

// Initialize structures
 pixfmt    (pixf ,rbuf_window );
 pixfmt_pre(pixf_pre ,rbuf_window );

 rb.Construct    (@pixf );
 rb_pre.Construct(@pixf_pre );
 rs.Construct    (@rb );

 rgba.ConstrDbl(1.0 ,1.0 ,1.0 );
 rb.clear      (@rgba );

 pf.Construct;
 sl.Construct;

// Calc
 src_mtx.Construct;

 tat.Construct(-_initial_width / 2 - 10 ,-_initial_height / 2 - 20 - 10 ); src_mtx.multiply(@tat );
 tar.Construct(m_angle._value * pi / 180.0 ); src_mtx.multiply(@tar );
 tas.Construct(m_scale._value ); src_mtx.multiply(@tas );
 tat.Construct(_initial_width / 2 ,_initial_height / 2 + 20 ); src_mtx.multiply(@tat );

 src_mtx.multiply(_trans_affine_resizing );

 img_mtx.Construct;

 tat.Construct(-_initial_width / 2 + 10 ,-_initial_height / 2 + 20 + 10 ); img_mtx.multiply(@tat );
 tar.Construct(m_angle._value * pi / 180.0 ); img_mtx.multiply(@tar );
 tas.Construct(m_scale._value ); img_mtx.multiply(@tas );
 tat.Construct(_initial_width / 2 ,_initial_height / 2 + 20 ); img_mtx.multiply(@tat );

 img_mtx.multiply(_trans_affine_resizing );
 img_mtx.invert;

 sa.Construct;
 interpolator.Construct(@img_mtx );

 rgba.ConstrPre(0 ,0.4 ,0 ,0.5 );

// Version without filtering (nearest neighbor)
{ sg:=new(
  span_image_filter_rgb_nn_ptr ,
  Construct(@sa ,rbuf_img(0 ) ,@rgba ,@interpolator ,component_order ) );{}

// Version with "hardcoded" bilinear filter
 sg:=new(
  span_image_filter_rgb_bilinear_ptr ,
  Construct(@sa ,rbuf_img(0 ) ,@rgba ,@interpolator ,component_order ) );{}

// Version with arbitrary filter
{ filter:=new(image_filter_mitchell_ptr ,Construct );

 fi.Construct(filter );
 sg:=new(
  span_image_filter_rgb_ptr ,
  Construct(@sa ,rbuf_img(0 ) ,@rgba ,@interpolator ,@fi ,component_order ) );{}

// Render
 ri.Construct(@rb_pre ,sg );

 r:=_initial_width;

 if _initial_height - 60 < r then
  r:=_initial_height - 60;

 ell.Construct(
  _initial_width / 2.0 + 10 ,
  _initial_height / 2.0 + 20 + 10 ,
  r / 2.0 + 16.0 ,
  r / 2.0 + 16.0 ,200 );

 tr.Construct(@ell ,@src_mtx );

 pf.add_path     (@tr );
 render_scanlines(@pf ,@sl ,@ri );

// Render the controls
 render_ctrl(@pf ,@sl ,@rs ,@m_angle );
 render_ctrl(@pf ,@sl ,@rs ,@m_scale );

// Free AGG resources
 pf.Destruct;
 sl.Destruct;

 sa.Destruct;

 if sg <> NIL then
  dispose(sg ,Destruct );

 if filter <> NIL then
  begin
   dispose(filter );

   fi.Destruct;

  end;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'This is the first example with the new "reincarnation" of the image transformation '#13 +
   'algorithms. The example allows you to rotate and scale the image with respect to '#13 +
   'its center. Also, the image is scaled when resizing the window.'#13#13 +
   'How to play with:'#13#13 +
   'Try to recompile the source code  with different image filtering methods.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;
 buf : array [0..255 ] of char;
 ext : string[10 ];

 img_name ,p ,n ,x : shortstring;

BEGIN
 app.Construct(pix_format ,flip_y );
 app.caption_ ('Image Affine Transformations with filtering (F1-Help)' );

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
  if app.init(app.rbuf_img(0 )._width + 20 ,app.rbuf_img(0 )._height + 40 + 20 ,window_resize ) then
   app.run;

 app.Destruct;

END.