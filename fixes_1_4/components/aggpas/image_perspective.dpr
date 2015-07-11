{mac_copy:spheres.bmp}
//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 image_perspective ;

uses
 Math ,SysUtils ,

 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgba ,

 agg_ctrl ,
 agg_rbox_ctrl ,

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
 agg_trans_bilinear ,
 agg_trans_perspective ,
 agg_span_interpolator_linear ,
 agg_span_interpolator_trans ,
 agg_span_subdiv_adaptor ,
 agg_span_image_filter ,
 agg_span_image_filter_rgba ,
 agg_span_allocator ,
 agg_image_filters ,
 agg_gsv_text ,
 interactive_polygon_ ,
 file_utils_ ;

{$I agg_mode.inc }

const
 flip_y = true;

var
 g_x1 ,
 g_y1 ,
 g_x2 ,
 g_y2 : double;

 g_rasterizer : rasterizer_scanline_aa;
 g_scanline   : scanline_u8;

type
 the_application = object(platform_support )
   m_quad       : interactive_polygon;
   m_trans_type : rbox_ctrl;

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

 m_quad.Construct      (4 ,5.0 );
 m_trans_type.Construct(420 ,5.0 ,420 + 170.0 ,70.0 ,not flip_y_ );

 m_trans_type.add_item ('Affine Parallelogram' );
 m_trans_type.add_item ('Bilinear' );
 m_trans_type.add_item ('Perspective' );
 m_trans_type.cur_item_(2 );

 add_ctrl(@m_trans_type );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_quad.Destruct;
 m_trans_type.Destruct;

end;

{ ON_INIT }
procedure the_application.on_init;
var
 x1 ,y1 ,x2 ,y2 ,dx ,dy : double;

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

{ m_quad.xn_ptr(0 )^:=Floor(x1 + dx + 50 );
 m_quad.yn_ptr(0 )^:=Floor(y1 + dy + 50 );
 m_quad.xn_ptr(1 )^:=Floor(x2 + dx );
 m_quad.yn_ptr(1 )^:=Floor(y1 + dy );
 m_quad.xn_ptr(2 )^:=Floor(x2 + dx );
 m_quad.yn_ptr(2 )^:=Floor(y2 + dy );
 m_quad.xn_ptr(3 )^:=Floor(x1 + dx );
 m_quad.yn_ptr(3 )^:=Floor(y2 + dy ); {}

 m_quad.xn_ptr(0 )^:=100 + 50;
 m_quad.yn_ptr(0 )^:=100 + 50;
 m_quad.xn_ptr(1 )^:=_width  - 100;
 m_quad.yn_ptr(1 )^:=100;
 m_quad.xn_ptr(2 )^:=_width  - 100;
 m_quad.yn_ptr(2 )^:=_height - 100;
 m_quad.xn_ptr(3 )^:=100;
 m_quad.yn_ptr(3 )^:=_height - 100;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf ,pixf_pre : pixel_formats;

 rb ,rb_pre : renderer_base;

 rgba : aggclr;

 r  : renderer_scanline_aa_solid;
 sa : span_allocator;
 tr : trans_affine;
 sg : span_image_filter_ptr;
 ri : renderer_scanline_aa;

 trb : trans_bilinear;
 trp : trans_perspective;

 interpolator   : span_interpolator_linear;
 interp_trans   : span_interpolator_trans;
 subdiv_adaptor : span_subdiv_adaptor;

 filter_kernel : image_filter_hermite;
 filter        : image_filter_lut;

 tm  : double;
 buf : array[0..63 ] of char;
 t   : gsv_text;
 pt  : conv_stroke;

begin
 sg:=NIL;

// Initialize structures
 pixfmt_bgra32    (pixf ,rbuf_window );     
 pixfmt_bgra32_pre(pixf_pre ,rbuf_window );

 rb.Construct    (@pixf );
 rb_pre.Construct(@pixf_pre );
 r.Construct     (@rb );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear      (@rgba );

 if m_trans_type._cur_item = 0 then
  begin
  // For the affine parallelogram transformations we
  // calculate the 4-th (implicit) point of the parallelogram
   m_quad.xn_ptr(3 )^:=m_quad.xn(0 ) + (m_quad.xn(2 ) - m_quad.xn(1 ) );
   m_quad.yn_ptr(3 )^:=m_quad.yn(0 ) + (m_quad.yn(2 ) - m_quad.yn(1 ) );

  end;

// Render the "quad" tool
 g_rasterizer.add_path(@m_quad );

 rgba.ConstrDbl  (0 ,0.3 ,0.5 ,0.6 );
 r.color_        (@rgba );
 render_scanlines(@g_rasterizer ,@g_scanline ,@r );

// Prepare the polygon to rasterize. Here we need to fill
// the destination (transformed) polygon.
 g_rasterizer.clip_box(0 ,0 ,_width ,_height );
 g_rasterizer.reset;
 g_rasterizer.move_to_d(m_quad.xn(0 ) ,m_quad.yn(0 ) );
 g_rasterizer.line_to_d(m_quad.xn(1 ) ,m_quad.yn(1 ) );
 g_rasterizer.line_to_d(m_quad.xn(2 ) ,m_quad.yn(2 ) );
 g_rasterizer.line_to_d(m_quad.xn(3 ) ,m_quad.yn(3 ) );

 sa.Construct;
 filter_kernel.Construct;
 filter.Construct(@filter_kernel ,false );

 rgba.ConstrPre(0 ,0 ,0 ,0 );

 start_timer;

 case m_trans_type._cur_item of
  0 :
   begin
   // Note that we consruct an affine matrix that transforms
   // a parallelogram to a rectangle, i.e., it's inverted.
   // It's actually the same as:
   // tr(g_x1, g_y1, g_x2, g_y2, m_triangle.polygon());
   // tr.invert();
    tr.Construct(parallelo_ptr(m_quad.polygon ) ,g_x1 ,g_y1 ,g_x2 ,g_y2 );

   // Also note that we can use the linear interpolator instead of
   // arbitrary span_interpolator_trans. It works much faster,
   // but the transformations must be linear and parellel.
    interpolator.Construct(@tr );

    sg:=new(
     span_image_filter_rgba_nn_ptr ,
     Construct(
      @sa ,rbuf_img(0 ) ,@rgba ,@interpolator ,bgra_order ) );

    ri.Construct    (@rb_pre ,sg );
    render_scanlines(@g_rasterizer ,@g_scanline ,@ri );

   end;

  1 :
   begin
    trb.Construct(m_quad.polygon ,g_x1 ,g_y1 ,g_x2 ,g_y2 );

    if trb.is_valid then
     begin
      interpolator.Construct(@trb );

      sg:=new(
       span_image_filter_rgba_2x2_ptr ,
       Construct(
        @sa ,rbuf_img(0 ) ,@rgba ,@interpolator ,@filter ,bgra_order ) );

      ri.Construct    (@rb_pre ,sg );
      render_scanlines(@g_rasterizer ,@g_scanline ,@ri );

     end;

   end;

  2 :
   begin
    trp.Construct(pointer(m_quad.polygon ) ,g_x1 ,g_y1 ,g_x2 ,g_y2 );

    if trp.is_valid then
     begin
     // interpolator.Construct  (@trp );
     // subdiv_adaptor.Construct(@interpolator );

      interp_trans.Construct(@trp );

      sg:=new(
       span_image_filter_rgba_2x2_ptr ,
       Construct(
        @sa ,rbuf_img(0 ) ,@rgba ,@interp_trans ,@filter ,bgra_order ) );

      ri.Construct    (@rb_pre ,sg );
      render_scanlines(@g_rasterizer ,@g_scanline ,@ri );

     end;

   end;

 end;

// Render Text
 tm:=elapsed_time;

 t.Construct;
 t.size_(10.0 );

 pt.Construct(@t );
 pt.width_   (1.5 );

 sprintf       (@buf[0 ] ,'%3.2f ms' ,tm );
 t.start_point_(10.0 ,10.0 );
 t.text_       (@buf[0 ] );

 g_rasterizer.add_path(@pt );

 rgba.ConstrDbl  (0 ,0 ,0 );
 r.color_        (@rgba );
 render_scanlines(@g_rasterizer ,@g_scanline ,@r );

// Render the controls
 render_ctrl(@g_rasterizer ,@g_scanline ,@r ,@m_trans_type );

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
var
 buf : array[0..99 ] of char;
 
begin
 if flags and mouse_left <> 0 then
  if m_quad.on_mouse_button_down(x ,y ) then
   force_redraw;

 if flags and mouse_right <> 0 then
  begin
   sprintf (@buf[0 ]             ,'%d ' ,x );
   sprintf (@buf[StrLen(@buf ) ] ,'%d'  ,y );
   message_(@buf[0 ] );

  end;

end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
 if m_quad.on_mouse_button_up(x ,y ) then
  force_redraw;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'Image perspective transformations. There are two types of arbitrary quadrangle '#13 +
   'transformations, Perspective and Bilinear. The image transformer always uses '#13 +
   'reverse transformations, and there is a problem. The Perspective transformations '#13 +
   'are perfectly reversible, so they work correctly with images, but the Bilinear '#13 +
   'transformer behave somehow strange. It can transform a rectangle to a quadrangle, '#13 +
   'but not vice versa. In this example you can see this effect, when the edges of '#13 +
   'the image "sag". I''d highly appreciate if someone could help me with math for '#13 +
   'transformations similar to Bilinear ones, but correctly reversible (i.e., that '#13 +
   'can transform an arbitrary quadrangle to a rectangle). The bilinear transformations '#13 +
   'are simple, see agg_trans_bilinear.h and agg_simul_eq.h' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;
 buf : array [0..255 ] of char;
 ext : string[10 ];

 img_name ,p ,n ,x : shortstring;

BEGIN
 g_x1:=0;
 g_y1:=0;
 g_x2:=0;
 g_y2:=0;

 g_rasterizer.Construct;
 g_scanline.Construct;

 app.Construct(pix_format_bgra32 ,flip_y );
 app.caption_ ('AGG Example. Image Perspective Transformations (F1-Help)' );

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