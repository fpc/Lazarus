{mac_copy:spheres.bmp}
//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 image_resample ;

uses
 Math ,SysUtils ,

 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgba ,

 agg_ctrl ,
 agg_rbox_ctrl ,
 agg_slider_ctrl ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_u ,
 agg_render_scanlines ,

 agg_path_storage ,
 agg_conv_transform ,
 agg_conv_stroke ,
 agg_trans_affine ,
 agg_trans_perspective ,
 agg_span_interpolator_linear ,
 agg_span_interpolator_trans ,
 agg_span_interpolator_persp ,
 agg_span_subdiv_adaptor ,
 agg_span_image_filter ,
 agg_span_image_filter_rgba ,
 agg_span_image_resample ,
 agg_span_image_resample_rgba ,
 agg_span_allocator ,
 agg_image_filters ,
 agg_gsv_text ,
 agg_gamma_lut ,
 interactive_polygon_ ,
 file_utils_ ;

{$I agg_mode.inc }

const
 flip_y = true;

 base_shift = agg_color.base_shift;

var
 g_x1 ,
 g_y1 ,
 g_x2 ,
 g_y2 : double;

 global_offset : int;

 g_rasterizer : rasterizer_scanline_aa;
 g_scanline   : scanline_u8;

 img_name ,p ,n ,x : shortstring;

type
 the_application = object(platform_support )
   m_gamma_lut  : gamma_lut;
   m_quad       : interactive_polygon;
   m_trans_type : rbox_ctrl;
   m_gamma      ,
   m_blur       : slider_ctrl;
   m_old_gamma  : double;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_gamma_lut.Construct(2.0 ,base_shift ,base_shift );
 m_quad.Construct     (4 ,5.0 );

 m_trans_type.Construct(400 ,5.0 ,430 + 170.0 ,100.0 ,not flip_y_ );
 m_gamma.Construct     (5.0 ,5.0 + 15 * 0 ,400-5 ,10.0 + 15 * 0 ,not flip_y_ );
 m_blur.Construct      (5.0 ,5.0 + 15 * 1 ,400-5 ,10.0 + 15 * 1 ,not flip_y_ );

 m_old_gamma:=2.0;

 m_trans_type.text_size_(7 );
 m_trans_type.add_item  ('Affine No Resample' );
 m_trans_type.add_item  ('Affine Resample' );
 m_trans_type.add_item  ('Perspective No Resample LERP' );
 m_trans_type.add_item  ('Perspective No Resample Exact' );
 m_trans_type.add_item  ('Perspective Resample LERP' );
 m_trans_type.add_item  ('Perspective Resample Exact' );
 m_trans_type.cur_item_ (4 );

 add_ctrl(@m_trans_type );

 m_gamma.range_(0.5 ,3.0 );
 m_gamma.value_(2.0 );
 m_gamma.label_('Gamma=%.3f' );

 add_ctrl(@m_gamma );

 m_blur.range_(0.5, 2.0);
 m_blur.value_(1.0 );
 m_blur.label_('Blur=%.3f' );

 add_ctrl(@m_blur );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_gamma_lut.Destruct;
 m_quad.Destruct;

 m_trans_type.Destruct;
 m_gamma.Destruct;
 m_blur.Destruct;

end;

{ ON_INIT }
procedure the_application.on_init;
var
 x1 ,y1 ,x2 ,y2 ,dx ,dy : double;

 pixf : pixel_formats;

begin
 g_x1:=0.0;
 g_y1:=0.0;
 g_x2:=rbuf_img(0 )._width;
 g_y2:=rbuf_img(0 )._height;

 x1:=g_x1;// * 100.0;
 y1:=g_y1;// * 100.0;
 x2:=g_x2;// * 100.0;
 y2:=g_y2;// * 100.0;

 dx:=_width  / 2.0 - (x2 - x1 ) / 2.0;
 dy:=_height / 2.0 - (y2 - y1 ) / 2.0;

 m_quad.xn_ptr(0 )^:=Floor(x1 + dx );
 m_quad.yn_ptr(0 )^:=Floor(y1 + dy );// - 150;
 m_quad.xn_ptr(1 )^:=Floor(x2 + dx );
 m_quad.yn_ptr(1 )^:=Floor(y1 + dy );// - 110;
 m_quad.xn_ptr(2 )^:=Floor(x2 + dx );
 m_quad.yn_ptr(2 )^:=Floor(y2 + dy );// - 300;
 m_quad.xn_ptr(3 )^:=Floor(x1 + dx );
 m_quad.yn_ptr(3 )^:=Floor(y2 + dy );// - 200;

 pixfmt_bgra32(pixf ,rbuf_img(0 ) );

 pixf.apply_gamma_dir(@m_gamma_lut ,bgra_order );

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf ,pixf_pre : pixel_formats;

 rb ,rb_pre : renderer_base;

 rgba : aggclr;

 r : renderer_scanline_aa_solid;
 b : int;

 sa : span_allocator;
 tr : trans_affine;
 sg : span_image_filter_ptr;
 ri : renderer_scanline_aa;

 trp : trans_perspective23;

 interpolator   : span_interpolator_linear;
 interpsubdiv   : span_interpolator_linear_subdiv;
 interp_trans   : span_interpolator_trans;
 interp_plerp   : span_interpolator_persp_lerp;
 interp_exact   : span_interpolator_persp_exact;
 subdiv_adaptor : span_subdiv_adaptor;

 filter_kernel : image_filter_hanning;
 filter        : image_filter_lut;

 tm : double;
 buf : array[0..63 ] of char;
 t   : gsv_text;
 pt  : conv_stroke;

begin
 sg:=NIL;

 if m_gamma._value <> m_old_gamma then
  begin
   m_gamma_lut.gamma_(m_gamma._value );

   load_img(0 ,img_name );

   pixfmt_bgra32(pixf ,rbuf_img(0 ) );

   pixf.apply_gamma_dir(@m_gamma_lut ,bgra_order );

   m_old_gamma:=m_gamma._value;

  end;

// Initialize structures
 pixfmt_bgra32    (pixf ,rbuf_window );
 pixfmt_bgra32_pre(pixf_pre ,rbuf_window );

 rb.Construct    (@pixf );
 rb_pre.Construct(@pixf_pre );

 r.Construct(@rb );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear      (@rgba );

 if m_trans_type._cur_item < 2 then
  begin
  // For the affine parallelogram transformations we
  // calculate the 4-th (implicit) point of the parallelogram
   m_quad.xn_ptr(3 )^:=m_quad.xn(0 ) + (m_quad.xn(2 ) - m_quad.xn(1 ) );
   m_quad.yn_ptr(3 )^:=m_quad.yn(0 ) + (m_quad.yn(2 ) - m_quad.yn(1 ) );

  end;

// Render the "quad" tool and controls
 g_rasterizer.add_path(@m_quad );

 rgba.ConstrDbl  (0 ,0.3 ,0.5 ,0.1 );
 r.color_        (@rgba );
 render_scanlines(@g_rasterizer ,@g_scanline ,@r );

// Prepare the polygon to rasterize. Here we need to fill
// the destination (transformed) polygon.
 g_rasterizer.clip_box(0 ,0 ,_width ,_height );
 g_rasterizer.reset;

 b:=0;

 g_rasterizer.move_to_d(m_quad.xn(0 ) - b ,m_quad.yn(0 ) - b );
 g_rasterizer.line_to_d(m_quad.xn(1 ) + b ,m_quad.yn(1 ) - b );
 g_rasterizer.line_to_d(m_quad.xn(2 ) + b ,m_quad.yn(2 ) + b );
 g_rasterizer.line_to_d(m_quad.xn(3 ) - b ,m_quad.yn(3 ) + b );

 sa.Construct;
 filter_kernel.Construct;
 filter.Construct(@filter_kernel ,true );

 rgba.ConstrPre(0 ,0 ,0 ,0 );

 start_timer;

 case m_trans_type._cur_item of
  0 :
   begin
    tr.Construct(parallelo_ptr(m_quad.polygon ) ,g_x1 ,g_y1 ,g_x2 ,g_y2 );

    interpolator.Construct(@tr );

    sg:=new(
     span_image_filter_rgba_2x2_ptr ,
     Construct(
      @sa ,rbuf_img(0 ) ,@rgba ,@interpolator ,@filter ,bgra_order ) );

    ri.Construct    (@rb_pre ,sg );
    render_scanlines(@g_rasterizer ,@g_scanline ,@ri );

   end;

  1 :
   begin
    tr.Construct(parallelo_ptr(m_quad.polygon ) ,g_x1 ,g_y1 ,g_x2 ,g_y2 );

    interpolator.Construct(@tr );

    sg:=new(
     span_image_resample_rgba_affine_ptr ,
     Construct(
      @sa ,rbuf_img(0 ) ,@rgba ,@interpolator ,@filter ,bgra_order ) );

    span_image_resample_ptr(sg ).blur_(m_blur._value );

    ri.Construct    (@rb_pre ,sg );
    render_scanlines(@g_rasterizer ,@g_scanline ,@ri );

   end;

  2 :
   begin
    trp.Construct(m_quad.polygon ,g_x1 ,g_y1 ,g_x2 ,g_y2 );

    if trp.is_valid then
     begin
      interpsubdiv.Construct(@trp );

      sg:=new(
       span_image_filter_rgba_2x2_ptr ,
       Construct(
        @sa ,rbuf_img(0 ) ,@rgba ,@interpsubdiv ,@filter ,bgra_order ) );

      ri.Construct    (@rb_pre ,sg );
      render_scanlines(@g_rasterizer ,@g_scanline ,@ri );

     end;

   end;

  3 :
   begin
    trp.Construct(m_quad.polygon ,g_x1 ,g_y1 ,g_x2 ,g_y2 );

    if trp.is_valid then
     begin
      interp_trans.Construct(@trp );

      sg:=new(
       span_image_filter_rgba_2x2_ptr ,
       Construct(
        @sa ,rbuf_img(0 ) ,@rgba ,@interp_trans ,@filter ,bgra_order ) );

      ri.Construct    (@rb_pre ,sg );
      render_scanlines(@g_rasterizer ,@g_scanline ,@ri );

     end;

   end;

  4 :
   begin
    interp_plerp.Construct  (m_quad.polygon ,g_x1 ,g_y1 ,g_x2 ,g_y2 );
    subdiv_adaptor.Construct(@interp_plerp );

    if interp_plerp.is_valid then
     begin
      sg:=new(
       span_image_resample_rgba_ptr ,
       Construct(
        @sa ,rbuf_img(0 ) ,@rgba ,@subdiv_adaptor ,@filter ,bgra_order ) );

      span_image_resample_ptr(sg ).blur_(m_blur._value );

      ri.Construct    (@rb_pre ,sg );
      render_scanlines(@g_rasterizer ,@g_scanline ,@ri );

     end;

   end;

  5 :
   begin
    interp_exact.Construct  (m_quad.polygon ,g_x1 ,g_y1 ,g_x2 ,g_y2 );
    subdiv_adaptor.Construct(@interp_exact );

    if interp_exact.is_valid then
     begin
      sg:=new(
       span_image_resample_rgba_ptr ,
       Construct(
        @sa ,rbuf_img(0 ) ,@rgba ,@subdiv_adaptor ,@filter ,bgra_order ) );

      span_image_resample_ptr(sg ).blur_(m_blur._value );

      ri.Construct    (@rb_pre ,sg );
      render_scanlines(@g_rasterizer ,@g_scanline ,@ri );

     end;

   end;

 end;

// Render Text
 tm:=elapsed_time;

 pixf.apply_gamma_inv(@m_gamma_lut ,bgra_order );

 t.Construct;
 t.size_(10.0 );

 pt.Construct(@t );
 pt.width_   (1.5 );

 sprintf       (@buf[0 ] ,'%3.2f ms' ,tm );
 t.start_point_(10.0 ,70.0 );
 t.text_       (@buf[0 ] );

 g_rasterizer.add_path(@pt );

 rgba.ConstrDbl  (0 ,0 ,0 );
 r.color_        (@rgba );
 render_scanlines(@g_rasterizer ,@g_scanline ,@r );

// Render the controls
 render_ctrl(@g_rasterizer ,@g_scanline ,@r ,@m_trans_type );
 render_ctrl(@g_rasterizer ,@g_scanline ,@r ,@m_gamma );
 render_ctrl(@g_rasterizer ,@g_scanline ,@r ,@m_blur );

// Free AGG resources
 sa.Destruct;
 filter.Destruct;

 if sg <> NIL then
  dispose(sg ,Destruct );

 t.Destruct;
 pt.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 if flags and mouse_left <> 0 then
  if m_quad.on_mouse_move(x ,y ) then
   force_redraw;

 if flags and mouse_left = 0 then
  on_mouse_button_up(x ,y ,flags );

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
begin
 if flags and mouse_left <> 0 then
  if m_quad.on_mouse_button_down(x ,y ) then
   force_redraw;

end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
 if m_quad.on_mouse_button_up(x ,y ) then
  force_redraw;

end;

{ ON_KEY }
procedure the_application.on_key;
var
 cx ,cy : double;

 tr  : trans_affine;
 tar : trans_affine_rotation;
 tat : trans_affine_translation;

begin
 if key = byte(' ' ) then
  begin
   cx:=(m_quad.xn(0 ) + m_quad.xn(1 ) + m_quad.xn(2 ) + m_quad.xn(3 ) ) / 4;
   cy:=(m_quad.yn(0 ) + m_quad.yn(1 ) + m_quad.yn(2 ) + m_quad.yn(3 ) ) / 4;

   tr.Construct;
   tat.construct(-cx ,-cy );

   tr:=tat;

   tar.Construct(pi / 20{2.0} ); tr.multiply(@tar );
   tat.Construct(cx ,cy ); tr.multiply(@tat );

   tr.transform(@tr ,m_quad.xn_ptr(0 ) ,m_quad.yn_ptr(0 ) );
   tr.transform(@tr ,m_quad.xn_ptr(1 ) ,m_quad.yn_ptr(1 ) );
   tr.transform(@tr ,m_quad.xn_ptr(2 ) ,m_quad.yn_ptr(2 ) );
   tr.transform(@tr ,m_quad.xn_ptr(3 ) ,m_quad.yn_ptr(3 ) );

   force_redraw;

  end;

 if key = key_f1 then
  message_(
   'The demonstration of image transformations with resampling. '#13 +
   'You can see the difference in quality between regular image transformers and '#13 +
   'the ones with resampling. Of course, image tranformations with resampling work '#13 +
   'slower because they provide the best possible quality.'#13#13 +
   'How to play with:'#13#13 +
   'Use the left mouse button to manipulate with image.'#13 +
   'Press the spacebar to rotate the image by 1/20 of pi.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;
 buf : array [0..255 ] of char;
 ext : string[10 ];

BEGIN
 g_x1:=0;
 g_y1:=0;
 g_x2:=0;
 g_y2:=0;

 global_offset:=0;

 g_rasterizer.Construct;
 g_scanline.Construct;

 app.Construct(pix_format_bgra32 ,flip_y );
 app.caption_ ('AGG Example. Image Transformations with Resampling (F1-Help)' );

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
  if app.init(600 ,600 ,window_resize ) then
   app.run;

 app.Destruct;

 g_rasterizer.Destruct;
 g_scanline.Destruct;
 
END.