//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 pattern_fill ;

{$DEFINE AGG_BGR24 }

uses
 agg_basics ,
 agg_platform_support ,
 agg_pixfmt_rgba ,

 agg_ctrl ,
 agg_rbox_ctrl ,
 agg_cbox_ctrl ,
 agg_slider_ctrl ,

 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_p ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_render_scanlines ,

 agg_path_storage ,
 agg_trans_affine ,
 agg_conv_stroke ,
 agg_conv_transform ,
 agg_conv_smooth_poly1 ,
 agg_span_allocator ,
 agg_span_pattern ,
 agg_span_pattern_rgba

{$I pixel_formats.inc }
{$I agg_mode.inc }
{$Q- }
{$R- }
const
 flip_y = true;

type
 the_application = object(platform_support )
   m_polygon_angle ,
   m_polygon_scale ,
   m_pattern_angle ,
   m_pattern_size  ,
   m_pattern_alpha : slider_ctrl;

   m_rotate_polygon ,
   m_rotate_pattern ,
   m_tie_pattern    : cbox_ctrl;

   m_polygon_cx ,
   m_polygon_cy ,

   m_dx ,
   m_dy : double;

   m_flag : int;

   m_pattern : int8u_ptr;
   m_pt_aloc : unsigned;

   m_pattern_rbuf : rendering_buffer;

   m_ras : rasterizer_scanline_aa;
   m_sl  : scanline_p8;
   m_ps  : path_storage;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure create_star(xc ,yc ,r1 ,r2 : double; n : unsigned; start_angle : double = 2.0 );
   procedure generate_pattern;

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_ctrl_change; virtual;
   procedure on_idle; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_polygon_angle.Construct (5 ,5 ,145 ,12 ,not flip_y_ );
 m_polygon_scale.Construct (5 ,5 + 14 ,145 ,12 + 14 ,not flip_y_ );
 m_pattern_angle.Construct (155 ,5 ,300 ,12 ,not flip_y_ );
 m_pattern_size.Construct  (155 ,5 + 14 ,300 ,12 + 14 ,not flip_y_ );
 m_pattern_alpha.Construct (310 ,5 ,460 ,12 ,not flip_y_ );
 m_rotate_polygon.Construct(5 ,5 + 14 + 14 ,'Rotate Polygon' ,not flip_y_ );
 m_rotate_pattern.Construct(5 ,5 + 14 + 14 + 14 ,'Rotate Pattern' ,not flip_y_ );
 m_tie_pattern.Construct   (155 ,5 + 14 + 14 ,'Tie pattern to polygon' ,not flip_y_ );

 m_pattern_rbuf.Construct;

 m_ras.Construct;
 m_sl.Construct;
 m_ps.Construct;

 m_flag   :=0;
 m_pattern:=NIL;
 m_pt_aloc:=0;

 add_ctrl(@m_polygon_angle );
 add_ctrl(@m_polygon_scale );
 add_ctrl(@m_pattern_angle );
 add_ctrl(@m_pattern_size );
 add_ctrl(@m_pattern_alpha );
 add_ctrl(@m_rotate_polygon );
 add_ctrl(@m_rotate_pattern );
 add_ctrl(@m_tie_pattern );

 m_polygon_angle.label_('Polygon Angle=%3.2f' );
 m_polygon_angle.range_(-180.0 ,180.0 );

 m_polygon_scale.label_('Polygon Scale=%3.2f' );
 m_polygon_scale.range_(0.1 ,5.0 );
 m_polygon_scale.value_(1.0 );

 m_pattern_angle.label_('Pattern Angle=%3.2f' );
 m_pattern_angle.range_(-180.0 ,180.0 );

 m_pattern_size.label_('Pattern Size=%3.2f' );
 m_pattern_size.range_(10 ,40 );
 m_pattern_size.value_(30 );

 m_pattern_alpha.label_('Background Alpha=%.2f' );
 m_pattern_alpha.value_(0.1 );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_polygon_angle.Destruct;
 m_polygon_scale.Destruct;
 m_pattern_angle.Destruct;
 m_pattern_size.Destruct;
 m_pattern_alpha.Destruct;
 m_rotate_polygon.Destruct;
 m_rotate_pattern.Destruct;
 m_tie_pattern.Destruct;

 m_pattern_rbuf.Destruct;

 m_ras.Destruct;
 m_sl.Destruct;
 m_ps.Destruct;

 agg_freemem(pointer(m_pattern ) ,m_pt_aloc );

end;

{ CREATE_STAR }
procedure the_application.create_star;
var
 i : unsigned;

 a ,dx ,dy : double;

begin
 m_ps.remove_all;

 start_angle:=start_angle * pi / 180.0;

 i:=0;

 while i < n do
  begin
   a :=pi * 2.0 * i / n - pi / 2.0;
   dx:=Cos(a + start_angle );
   dy:=Sin(a + start_angle );

   if i and 1 <> 0 then
    m_ps.line_to(xc + dx * r1 ,yc + dy * r1 )
   else
    if i <> 0 then
     m_ps.line_to(xc + dx * r2 ,yc + dy * r2 )
    else
     m_ps.move_to(xc + dx * r2 ,yc + dy * r2 );

   inc(i );  

  end;

 m_ps.close_polygon;

end;

{ GENERATE_PATTERN }
procedure the_application.generate_pattern;
var
 size : unsigned;
 pixf : pixel_formats;
 rgba : aggclr;

 smooth : conv_smooth_poly1_curve;
 stroke : conv_stroke;

 rb : renderer_base;
 rs : renderer_scanline_aa_solid;

begin
 size:=trunc(m_pattern_size._value );

 create_star(
  m_pattern_size._value / 2.0 ,
  m_pattern_size._value / 2.0 ,
  m_pattern_size._value / 2.5 ,
  m_pattern_size._value / 6.0 ,
  6 ,
  m_pattern_angle._value );

 smooth.Construct(@m_ps );
 stroke.Construct(@smooth );

 smooth.smooth_value_       (0.0 );
 smooth.approximation_scale_(4.0 );

 stroke.width_(m_pattern_size._value / 15.0);

 agg_freemem(pointer(m_pattern ) ,m_pt_aloc );

 m_pt_aloc:=size * size * 4 * sizeof(int8u );

 agg_getmem(pointer(m_pattern ) ,m_pt_aloc );

 m_pattern_rbuf.attach(m_pattern ,size ,size ,size * 4 );

 pixfmt_rgba32(pixf ,@m_pattern_rbuf );
 rb.Construct (@pixf );
 rs.Construct (@rb );

 rgba.ConstrDbl(0.4 ,0.0 ,0.1 ,m_pattern_alpha._value ); // Pattern background color
 rb.clear      (@rgba );

 m_ras.add_path  (@smooth );
 rgba.ConstrInt  (110 ,130 ,50 );
 rs.color_       (@rgba );
 render_scanlines(@m_ras ,@m_sl ,@rs );

 m_ras.add_path  (@stroke );
 rgba.ConstrInt  (0 ,50 ,80 );
 rs.color_       (@rgba );
 render_scanlines(@m_ras ,@m_sl ,@rs );

 smooth.Destruct;
 stroke.Destruct;

end;

{ ON_INIT }
procedure the_application.on_init;
begin
 m_polygon_cx:=_initial_width / 2.0;
 m_polygon_cy:=_initial_height / 2.0;

 generate_pattern;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 width_ ,height_ : double;

 pixf : pixel_formats;
 rgba : aggclr;

 rb : renderer_base;
 rs : renderer_scanline_aa_solid;

 polygon_mtx : trans_affine;

 tat : trans_affine_translation;
 tar : trans_affine_rotation;
 tas : trans_affine_scaling;

 r : double;

 offset_x ,offset_y : unsigned;

 tr : conv_transform;
 wx ,
 wy : wrap_mode_reflect_auto_pow2;
 sa : span_allocator;
 sg : span_pattern_rgba;
 rp : renderer_scanline_aa;

begin
// Initialize structures
 width_ :=rbuf_window._width;
 height_:=rbuf_window._height;

 pixfmt(pixf ,rbuf_window );

 rb.Construct(@pixf );
 rs.Construct(@rb );

 rgba.ConstrDbl(1.0 ,1.0 ,1.0 );
 rb.clear      (@rgba );

// Render
 polygon_mtx.Construct;

 tat.Construct(-m_polygon_cx ,-m_polygon_cy );        polygon_mtx.multiply(@tat );
 tar.Construct(m_polygon_angle._value * pi / 180.0 ); polygon_mtx.multiply(@tar );
 tas.Construct(m_polygon_scale._value );              polygon_mtx.multiply(@tas );
 tat.Construct(m_polygon_cx ,m_polygon_cy );          polygon_mtx.multiply(@tat );

 r:=_initial_width / 3.0 - 8.0;

 create_star(m_polygon_cx ,m_polygon_cy ,r ,r / 1.45 ,14 );

 tr.Construct(@m_ps ,@polygon_mtx );

 offset_x:=0;
 offset_y:=0;

 if m_tie_pattern._status then
  begin
   offset_x:=trunc(width_ - m_polygon_cx );
   offset_y:=trunc(height_ - m_polygon_cy );

  end;

 sa.Construct;
 wx.Construct;
 wy.Construct;
 sg.Construct(@sa ,@m_pattern_rbuf ,offset_x ,offset_y ,@wx ,@wy ,rgba_order );
 rp.Construct(@rb ,@sg );

{ m_ras.clip_box (-1 ,0 ,width_ ,height_ );
 m_ras.move_to_d(-1 ,100 );
 m_ras.line_to_d(100 ,100 );
 m_ras.line_to_d(100 ,200 );
 m_ras.line_to_d(-1 ,200 );
 m_ras.close_polygon;{}

 m_ras.add_path  (@tr );
 rgba.ConstrDbl  (0 ,0 ,0 );
 rs.color_       (@rgba );
 render_scanlines(@m_ras ,@m_sl ,@rp );

// Render the controls
 render_ctrl(@m_ras ,@m_sl ,@rs ,@m_polygon_angle );
 render_ctrl(@m_ras ,@m_sl ,@rs ,@m_polygon_scale );
 render_ctrl(@m_ras ,@m_sl ,@rs ,@m_pattern_angle );
 render_ctrl(@m_ras ,@m_sl ,@rs ,@m_pattern_size );
 render_ctrl(@m_ras ,@m_sl ,@rs ,@m_pattern_alpha );
 render_ctrl(@m_ras ,@m_sl ,@rs ,@m_rotate_polygon );
 render_ctrl(@m_ras ,@m_sl ,@rs ,@m_rotate_pattern );
 render_ctrl(@m_ras ,@m_sl ,@rs ,@m_tie_pattern );

// Free AGG resources
 sa.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 if flags and mouse_left <> 0 then
  if m_flag <> 0 then
   begin
    m_polygon_cx:=x - m_dx;
    m_polygon_cy:=y - m_dy;

    force_redraw;

   end
  else
 else
  on_mouse_button_up(x ,y ,flags );

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
var
 polygon_mtx : trans_affine;

 tat : trans_affine_translation;
 tar : trans_affine_rotation;
 tas : trans_affine_scaling;

 r  : double;
 tr : conv_transform;

begin
 if flags and mouse_left <> 0 then
  begin
   polygon_mtx.Construct;

   tat.Construct(-m_polygon_cx ,-m_polygon_cy );                   polygon_mtx.multiply(@tat );
   tar.Construct(m_polygon_angle._value * pi / 180.0 );            polygon_mtx.multiply(@tar );
   tas.Construct(m_polygon_scale._value ,m_polygon_scale._value ); polygon_mtx.multiply(@tas );
   tat.Construct(m_polygon_cx ,m_polygon_cy );                     polygon_mtx.multiply(@tat );

   r:=_initial_width / 3.0 - 8.0;

   create_star(m_polygon_cx ,m_polygon_cy ,r ,r / 1.45 ,14 );

   tr.Construct  (@m_ps ,@polygon_mtx );
   m_ras.add_path(@tr );

   if m_ras.hit_test(x ,y ) then
    begin
     m_dx:=x - m_polygon_cx;
     m_dy:=y - m_polygon_cy;

     m_flag:=1;

    end;

  end;

end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
 m_flag:=0;

end;

{ ON_CTRL_CHANGE }
procedure the_application.on_ctrl_change;
begin
 if m_rotate_polygon._status or
    m_rotate_pattern._status then
  wait_mode_(false )
 else
  wait_mode_(true);

 generate_pattern;
 force_redraw;

end;

{ ON_IDLE }
procedure the_application.on_idle;
var
 redraw : boolean;

begin
 redraw:=false;
 if m_rotate_polygon._status then
  begin
   m_polygon_angle.value_(m_polygon_angle._value + 0.5 );

   if m_polygon_angle._value >= 180.0 then
    m_polygon_angle.value_(m_polygon_angle._value - 360.0 );

   redraw:=true;

  end;

 if m_rotate_pattern._status then
  begin
   m_pattern_angle.value_(m_pattern_angle._value - 0.5 );

   if m_pattern_angle._value <= -180.0 then
    m_pattern_angle.value_(m_pattern_angle._value + 360.0 );

   generate_pattern;

   redraw:=true;

  end;

 if redraw then
  force_redraw;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'The example demonstrates how to use arbitrary images as fill patterns. '#13 +
   'This span generator is very simple, so, it doesn''t allow you to apply '#13 +
   'arbitrary transformations to the pattern, i.e., it cannot be used as '#13 +
   'a texturing tool. But it works pretty fast and can be useful in some applications.   '#13#13 +
   'How to play with:'#13#13 +
   'Use the left mouse button to move the polygon around.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. Pattern Filling (F1-Help)' );

 if app.init(640 ,480 ,0 ) then
  app.run;

 app.Destruct;

END.