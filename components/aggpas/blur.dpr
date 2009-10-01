//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 blur ;

uses
 agg_basics ,
 agg_platform_support ,
 agg_ctrl ,
 agg_slider_ctrl ,
 agg_rbox_ctrl ,
 agg_cbox_ctrl ,
 agg_polygon_ctrl ,
 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,
 agg_pixfmt_rgba ,
 agg_pixfmt_gray ,
 agg_renderer_base ,
 agg_rendering_buffer ,
 agg_rasterizer_scanline_aa ,
 agg_conv_curve ,
 agg_conv_contour ,
 agg_conv_stroke ,
 agg_conv_transform ,
 agg_scanline_p ,
 agg_render_scanlines ,
 agg_renderer_scanline ,
 agg_bounding_rect ,
 agg_trans_perspective ,
 agg_path_storage ,
 agg_trans_affine ,
 agg_gsv_text ,
 agg_blur ;

{$I agg_mode.inc }

const
 flip_y = true;

type
 the_application = object(platform_support )
  private
   m_method : rbox_ctrl;
   m_radius : slider_ctrl;

   m_shadow_ctrl : polygon_ctrl;

   m_channel_r ,
   m_channel_g ,
   m_channel_b ,
   m_ctrl_bott : cbox_ctrl;

   m_path  : path_storage;
   m_shape : conv_curve;

   m_ras   : rasterizer_scanline_aa;
   m_sl    : scanline_p8;
   m_rbuf2 : rendering_buffer;

   m_stack_blur     : stack_blur;
   m_recursive_blur : recursive_blur;

   m_shape_bounds : rect_d;

  public
   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;
   
   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
var
 mtx : trans_affine;
 tas : trans_affine_scaling;
 tat : trans_affine_translation;
 clr : aggclr;

begin
 inherited Construct(format_ ,flip_y_ );

 m_method.Construct(10.0       ,10.0       ,150.0       ,85.0             ,not flip_y_ );
 m_radius.Construct(150 + 10.0 ,10.0 + 4.0 ,130 + 300.0 ,10.0 + 8.0 + 4.0 ,not flip_y_ );

 m_shadow_ctrl.Construct(4 );

 m_channel_r.Construct(10.0 ,95.0  ,'Red'   ,not flip_y_ );
 m_channel_g.Construct(10.0 ,110.0 ,'Green' ,not flip_y_ );
 m_channel_b.Construct(10.0 ,125.0 ,'Blue'  ,not flip_y_ );
 m_ctrl_bott.Construct(285  ,30    ,'Draw controls first' ,not flip_y_ );

 m_path.Construct;
 m_shape.Construct(@m_path );

 m_ras.Construct;
 m_sl.Construct;
 m_rbuf2.Construct;
 m_shape_bounds.Construct;

 m_stack_blur.Construct;
 m_recursive_blur.Construct;

 add_ctrl(@m_method );

 m_method.text_size_(8 );
 m_method.add_item  ('Stack Blur - Fast' );
 m_method.add_item  ('Stack Blur' );
 m_method.add_item  ('Recursive Blur' );
 m_method.add_item  ('Channels' );
 m_method.cur_item_ (0 );
 m_method.no_transform;

 add_ctrl(@m_radius );

 m_radius.range_(0.0 ,40.0 );
 m_radius.value_(15.0 );
 m_radius.label_('Blur Radius=%1.2f' );
 m_radius.no_transform;

 add_ctrl(@m_shadow_ctrl );

 m_shadow_ctrl.in_polygon_check_(true );
 m_shadow_ctrl.no_transform;

 add_ctrl(@m_channel_r );

 m_channel_r.no_transform;

 add_ctrl(@m_channel_g );

 m_channel_g.status_(true );
 m_channel_g.no_transform;

 add_ctrl(@m_channel_b );

 m_channel_b.no_transform;

 add_ctrl(@m_ctrl_bott );

 m_ctrl_bott.no_transform;

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

 mtx.Construct;
 tas.Construct(4.0 );
 mtx.multiply (@tas );
 tat.Construct(150 ,100 );
 mtx.multiply (@tat );

 m_path.transform(@mtx );

 bounding_rect_single(
  @m_shape ,0 ,
  @m_shape_bounds.x1 ,@m_shape_bounds.y1 ,
  @m_shape_bounds.x2 ,@m_shape_bounds.y2 );

 m_shadow_ctrl.xn_ptr(0 )^:=m_shape_bounds.x1;
 m_shadow_ctrl.yn_ptr(0 )^:=m_shape_bounds.y1;
 m_shadow_ctrl.xn_ptr(1 )^:=m_shape_bounds.x2;
 m_shadow_ctrl.yn_ptr(1 )^:=m_shape_bounds.y1;
 m_shadow_ctrl.xn_ptr(2 )^:=m_shape_bounds.x2;
 m_shadow_ctrl.yn_ptr(2 )^:=m_shape_bounds.y2;
 m_shadow_ctrl.xn_ptr(3 )^:=m_shape_bounds.x1;
 m_shadow_ctrl.yn_ptr(3 )^:=m_shape_bounds.y2;

 clr.ConstrDbl(0 ,0.3 ,0.5 ,0.3 );

 m_shadow_ctrl.line_color_(@clr );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_method.Destruct;
 m_radius.Destruct;
 m_shadow_ctrl.Destruct;
 m_channel_r.Destruct;
 m_channel_g.Destruct;
 m_channel_b.Destruct;
 m_ctrl_bott.Destruct;

 m_path.Destruct;
 m_shape.Destruct;

 m_ras.Destruct;
 m_sl.Destruct;
 m_rbuf2.Destruct;

 m_stack_blur.Destruct;
 m_recursive_blur.Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf ,pixf2 : pixel_formats;

 renb : renderer_base;
 rens : renderer_scanline_aa_solid;
 rgba : aggclr;

 shadow_persp : trans_perspective23;
 shadow_trans : conv_transform;

 bbox : rect_d;

 tm : double;

 buf : array[0..63 ] of char;
 t   : gsv_text;
 st  : conv_stroke;

begin
// Initialize structures
 pixfmt_bgr24(pixf ,rbuf_window );

 renb.Construct(@pixf );
 rens.Construct(@renb );
 rgba.ConstrDbl(1 ,1 ,1 );
 renb.clear    (@rgba );
 m_ras.clip_box(0 ,0 ,_width ,_height );

 shadow_persp.Construct(
  m_shape_bounds.x1 ,m_shape_bounds.y1 ,
  m_shape_bounds.x2 ,m_shape_bounds.y2 ,
  m_shadow_ctrl._polygon );

 shadow_trans.Construct(@m_shape ,@shadow_persp );

 if m_ctrl_bott._status then
  begin
   render_ctrl(@m_ras ,@m_sl ,@rens ,@m_method );
   render_ctrl(@m_ras ,@m_sl ,@rens ,@m_radius );
   render_ctrl(@m_ras ,@m_sl ,@rens ,@m_channel_r );
   render_ctrl(@m_ras ,@m_sl ,@rens ,@m_channel_g );
   render_ctrl(@m_ras ,@m_sl ,@rens ,@m_channel_b );

  end;

// Render shadow
 m_ras.add_path(@shadow_trans );

 rgba.ConstrDbl  (0.2 ,0.3 ,0 );
 rens.color_     (@rgba );
 render_scanlines(@m_ras ,@m_sl ,@rens );

// Calculate the bounding box and extend it by the blur radius
 bbox.Construct;
 bounding_rect_single(@shadow_trans ,0 ,@bbox.x1 ,@bbox.y1 ,@bbox.x2 ,@bbox.y2 );

 bbox.x1:=bbox.x1 - m_radius._value;
 bbox.y1:=bbox.y1 - m_radius._value;
 bbox.x2:=bbox.x2 + m_radius._value;
 bbox.y2:=bbox.y2 + m_radius._value;

 if m_method._cur_item = 2 then
  begin
  // The recursive blur method represents the true Gussian Blur,
  // with theoretically infinite kernel. The restricted window size
  // results in extra influence of edge pixels. It's impossible to
  // solve correctly, but extending the right and top areas to another
  // radius value produces fair result.
   bbox.x2:=bbox.x2 + m_radius._value;
   bbox.y2:=bbox.y2 + m_radius._value;

  end;

 start_timer;

 if m_method._cur_item <> 3 then
  begin
  // Create a new pixel renderer and attach it to the main one as a child image.
  // It returns true if the attachment suceeded. It fails if the rectangle
  // (bbox) is fully clipped.
   pixfmt_bgr24(pixf2 ,@m_rbuf2 );

   if pixf2.attach(@pixf ,Trunc(bbox.x1 ) ,Trunc(bbox.y1 ) ,Trunc(bbox.x2 ) ,Trunc(bbox.y2 ) ) then
   // Blur it
    if m_method._cur_item = 0 then
    // Faster, but bore specific.
    // Works only for 8 bits per channel and only with radii <= 254.
     stack_blur_rgb24(@pixf2 ,uround(m_radius._value ) ,uround(m_radius._value ) )

    else
     if m_method._cur_item = 1 then
     // More general method, but 30-40% slower.
      m_stack_blur.blur(@pixf2 ,uround(m_radius._value ) )

     else
     // True Gaussian Blur, 3-5 times slower than Stack Blur,
     // but still constant time of radius. Very sensitive
     // to precision, doubles are must here.
      m_recursive_blur.blur(@pixf2 ,m_radius._value );

  end
 else
  begin
  // Blur separate channels
   if m_channel_r._status then
    begin
     pixfmt_gray8_bgr24r(pixf2 ,@m_rbuf2 );

     if pixf2.attach(@pixf ,Trunc(bbox.x1 ) ,Trunc(bbox.y1 ) ,Trunc(bbox.x2 ) ,Trunc(bbox.y2 ) ) then
      stack_blur_gray8(@pixf2 ,uround(m_radius._value ) ,uround(m_radius._value ) );

    end;

   if m_channel_g._status then
    begin
     pixfmt_gray8_bgr24g(pixf2 ,@m_rbuf2 );

     if pixf2.attach(@pixf ,Trunc(bbox.x1 ) ,Trunc(bbox.y1 ) ,Trunc(bbox.x2 ) ,Trunc(bbox.y2 ) ) then
      stack_blur_gray8(@pixf2 ,uround(m_radius._value ) ,uround(m_radius._value ) );

    end;

   if m_channel_b._status then
    begin
     pixfmt_gray8_bgr24b(pixf2 ,@m_rbuf2 );

     if pixf2.attach(@pixf ,Trunc(bbox.x1 ) ,Trunc(bbox.y1 ) ,Trunc(bbox.x2 ) ,Trunc(bbox.y2 ) ) then
      stack_blur_gray8(@pixf2 ,uround(m_radius._value ) ,uround(m_radius._value ) );

    end;

  end;

 tm:=elapsed_time;

 render_ctrl(@m_ras ,@m_sl ,@rens ,@m_shadow_ctrl );

// Render the shape itself
 m_ras.add_path(@m_shape );

 rgba.ConstrDbl  (0.6 ,0.9 ,0.7 ,0.8 );
 rens.color_     (@rgba );
 render_scanlines(@m_ras ,@m_sl ,@rens );

 t.Construct;
 t.size_       (10.0 );
 st.Construct  (@t );
 st.width_     (1.5 );
 sprintf       (@buf[0 ] ,'%3.2f ms' ,tm );
 t.start_point_(150.0, 30.0);
 t.text_       (@buf[0 ] );

 m_ras.add_path(@st );

 rgba.ConstrDbl  (0 ,0 ,0 );
 rens.color_     (@rgba );
 render_scanlines(@m_ras ,@m_sl ,@rens );

// Render the controls
 if not m_ctrl_bott._status then
  begin
   render_ctrl(@m_ras ,@m_sl ,@rens ,@m_method );
   render_ctrl(@m_ras ,@m_sl ,@rens ,@m_radius );
   render_ctrl(@m_ras ,@m_sl ,@rens ,@m_channel_r );
   render_ctrl(@m_ras ,@m_sl ,@rens ,@m_channel_g );
   render_ctrl(@m_ras ,@m_sl ,@rens ,@m_channel_b );

  end;

 render_ctrl(@m_ras ,@m_sl ,@rens ,@m_ctrl_bott );

// Free Method Resources
 t.Destruct;
 st.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 if flags and mouse_left <> 0 then
  if m_shadow_ctrl.on_mouse_move(x ,y ,false ) then
   force_redraw;

 if flags and mouse_left = 0 then
  on_mouse_button_up(x ,y ,flags );

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
begin
 if flags and mouse_left <> 0 then
  if m_shadow_ctrl.on_mouse_button_down(x ,y ) then
   force_redraw;

end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
 if m_shadow_ctrl.on_mouse_button_up(x ,y ) then
  force_redraw;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'Now you can blur rendered images rather fast!'#13 +
   'There two algorithms are used: Stack Blur by Mario Klingemann     '#13 +
   'and Fast Recursive Gaussian Filter. The speed of both methods'#13 +
   'does not depend on the filter radius. Mario''s method works 3-5'#13 +
   'times faster; it doesn''t produce exactly Gaussian response,'#13 +
   'but pretty fair for most practical purposes. The recursive filter'#13 +
   'uses floating point arithmetic and works slower. But it is true'#13 +
   'Gaussian filter, with theoretically infinite impulse response.'#13 +
   'The radius (actually 2*sigma value) can be fractional and the'#13 +
   'filter produces quite adequate result.' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. Gaussian and Stack Blur (F1-Help)' );

 if app.init(440 ,330 ,window_resize ) then
  app.run;

 app.Destruct;

END.