//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 bspline ;

{DEFINE AGG_GRAY8 }
{$DEFINE AGG_BGR24 }
{DEFINE AGG_RGB24 }
{DEFINE AGG_BGRA32 }
{DEFINE AGG_RGBA32 }
{DEFINE AGG_ARGB32 }
{DEFINE AGG_ABGR32 }
{DEFINE AGG_RGB565 }
{DEFINE AGG_RGB555 }

uses
 agg_basics ,
 agg_platform_support ,

 agg_ctrl ,
 agg_slider_ctrl ,
 agg_cbox_ctrl ,

 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_p ,

 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_render_scanlines ,

 agg_conv_bspline ,
 agg_conv_stroke ,
 interactive_polygon_

{$I pixel_formats.inc }
{$I agg_mode.inc }

const
 flip_y = true;

type
 the_application = object(platform_support )
   m_poly : interactive_polygon;

   m_num_points : slider_ctrl;

   m_close : cbox_ctrl;
   m_flip  : int;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_mouse_move(x ,y : int; flags : unsigned ); virtual;

   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_poly.Construct(6 ,5.0 );

 m_num_points.Construct(5.0 ,5.0 ,340.0 ,12.0 ,not flip_y_ );
 m_close.Construct     (350 ,5.0 ,'Close' ,not flip_y_ );

 m_flip:=0;

 add_ctrl(@m_close );

 m_num_points.range_(1.0 ,40.0 );
 m_num_points.value_(20.0 );
 m_num_points.label_('Number of intermediate Points = %.3f' );

 add_ctrl(@m_num_points );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_num_points.Destruct;
 m_close.Destruct;

 m_poly.Destruct;

end;

{ ON_INIT }
procedure the_application.on_init;
begin
 if m_flip <> 0 then
  begin
   m_poly.xn_ptr(0 )^:=100;
   m_poly.yn_ptr(0 )^:=_height - 100;
   m_poly.xn_ptr(1 )^:=_width - 100;
   m_poly.yn_ptr(1 )^:=_height - 100;
   m_poly.xn_ptr(2 )^:=_width - 100;
   m_poly.yn_ptr(2 )^:=100;
   m_poly.xn_ptr(3 )^:=100;
   m_poly.yn_ptr(3 )^:=100;

  end
 else
  begin
   m_poly.xn_ptr(0 )^:=100;
   m_poly.yn_ptr(0 )^:=100;
   m_poly.xn_ptr(1 )^:=_width - 100;
   m_poly.yn_ptr(1 )^:=100;
   m_poly.xn_ptr(2 )^:=_width - 100;
   m_poly.yn_ptr(2 )^:=_height - 100;
   m_poly.xn_ptr(3 )^:=100;
   m_poly.yn_ptr(3 )^:=_height - 100;

  end;

 m_poly.xn_ptr(4 )^:=_width / 2;
 m_poly.yn_ptr(4 )^:=_height / 2;
 m_poly.xn_ptr(5 )^:=_width / 2;
 m_poly.yn_ptr(5 )^:=_height / 3;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;

 rb : renderer_base;
 r  : renderer_scanline_aa_solid;
 sl : scanline_p8;

 ras  : rasterizer_scanline_aa;
 rgba : aggclr;
 path : simple_polygon_vertex_source;

 bspline : conv_bspline;
 stroke  : conv_stroke;

begin
// Initialize structures
 pixfmt(pixf ,rbuf_window );

 rb.Construct(@pixf );
 r.Construct (@rb );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear      (@rgba );

 sl.Construct;
 ras.Construct;

// Draw
 path.Construct(m_poly.polygon ,m_poly.num_points ,false ,m_close._status );

 bspline.Construct          (@path );
 bspline.interpolation_step_(1.0 / m_num_points._value );

 stroke.Construct(@bspline );
 stroke.width_   (2.0 );

 rgba.ConstrDbl(0 ,0 ,0 );
 r.color_      (@rgba );

 ras.add_path(@stroke );

 render_scanlines(@ras ,@sl ,@r );

// Render the "poly" tool
 rgba.ConstrDbl(0 ,0.3 ,0.5 ,0.6 );
 r.color_      (@rgba );

 ras.add_path(@m_poly );

 render_scanlines(@ras ,@sl ,@r );

// Render the controls
 render_ctrl(@ras ,@sl ,@r ,@m_close );
 render_ctrl(@ras ,@sl ,@r ,@m_num_points );

// Free AGG resources
 ras.Destruct;
 sl.Destruct;

 stroke.Destruct;
 bspline.Destruct;
 path.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 if flags and mouse_left <> 0 then
  if m_poly.on_mouse_move(x ,y ) then
   force_redraw;

 if flags and mouse_left = 0 then
  on_mouse_button_up(x ,y ,flags );

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
begin
 if flags and mouse_left <> 0 then
  if m_poly.on_mouse_button_down(x ,y ) then
   force_redraw;

end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
 if m_poly.on_mouse_button_up(x ,y ) then
  force_redraw;
  
end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = unsigned(' ' ) then
  begin
   m_flip:=m_flip xor 1;

   on_init;
   force_redraw;

  end;

 if key = key_f1 then
  message_(
   'Demostration of a very simple class of Bi-cubic Spline interpolation.'#13 +
   'The class supports extrapolation which is a simple linear function.  '#13#13 +
   'How to play with:'#13#13 +
   'Use the mouse to change curve''s shape.'#13 +
   'Press the spacebar to flip the curve. ' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format ,flip_y );
 app.caption_ ('AGG Example. BSpline Interpolator(F1-Help)' );

 if app.init(600 ,600 ,window_resize ) then
  app.run;

 app.Destruct;

END.