//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 lion ;

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
 Math ,

 agg_basics ,
 agg_platform_support ,

 agg_ctrl ,
 agg_slider_ctrl ,

 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_p ,

 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_render_scanlines ,

 agg_path_storage ,
 agg_bounding_rect ,
 agg_trans_affine ,
 agg_conv_transform ,
 parse_lion_

{$I pixel_formats.inc }
{$I agg_mode.inc }

const
 flip_y = true;

var
 g_rasterizer : rasterizer_scanline_aa;
 g_scanline   : scanline_p8;

 g_path     : path_storage;
 g_colors   : array[0..99 ] of aggclr;
 g_path_idx : array[0..99 ] of unsigned;

 g_npaths : unsigned;

 g_x1 ,g_y1 ,g_x2 ,g_y2 ,
 g_base_dx  ,g_base_dy  ,
 g_angle    ,g_scale    ,
 g_skew_x   ,g_skew_y   : double;

 g_nclick : int;

type
 the_application = object(platform_support )
   m_alpha_slider : slider_ctrl;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_resize(sx ,sy : int ); virtual;
   procedure on_draw; virtual;

   procedure transform(width_ ,height_ ,x ,y : double );

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ _PARSE_LION_ }
procedure _parse_lion_;
begin
 g_npaths:=parse_lion(@g_path ,@g_colors ,@g_path_idx );

 bounding_rect(@g_path ,@g_path_idx ,0 ,g_npaths ,@g_x1 ,@g_y1 ,@g_x2 ,@g_y2 );

 g_base_dx:=(g_x2 - g_x1 ) / 2.0;
 g_base_dy:=(g_y2 - g_y1 ) / 2.0;

end;  

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_alpha_slider.Construct(5 ,5 ,512 - 5 ,12 ,not flip_y_ );

 _parse_lion_;

 add_ctrl(@m_alpha_slider );

 m_alpha_slider.no_transform;
 m_alpha_slider.label_('Alpha%3.3f' );
 m_alpha_slider.value_(0.1 );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_alpha_slider.Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 i : unsigned;

 width_ ,height_ : int;

 pixf : pixel_formats;

 rb : renderer_base;
 r  : renderer_scanline_aa_solid;

 mtx : trans_affine;
 tat : trans_affine_translation;
 tas : trans_affine_scaling;
 tar : trans_affine_rotation;
 taw : trans_affine_skewing;

 trans : conv_transform;

begin
 width_ :=rbuf_window._width;
 height_:=rbuf_window._height;

 for i:=0 to g_npaths - 1 do
  aggclr_ptr(ptrcomp(@g_colors[0 ] ) + i * sizeof(aggclr ) ).a:=
   int8u(trunc(m_alpha_slider._value * 255 ) );

// Initialize structures
 pixfmt(pixf ,rbuf_window );

 rb.Construct(@pixf );
 r.Construct (@rb );

// Transform lion
 mtx.Construct;

 tat.Construct(-g_base_dx ,-g_base_dy );
 mtx.multiply (@tat );

 tas.Construct(g_scale ,g_scale );
 mtx.multiply (@tas );

 tar.Construct(g_angle + pi );
 mtx.multiply (@tar );

 taw.Construct(g_skew_x / 1000.0 ,g_skew_y / 1000.0 );
 mtx.multiply (@taw );

 tat.Construct(width_ / 2 ,height_ / 2 );
 mtx.multiply (@tat );

// This code renders the lion
 trans.Construct(@g_path ,@mtx );

 render_all_paths(@g_rasterizer ,@g_scanline ,@r ,@trans ,@g_colors ,@g_path_idx ,g_npaths );

// Render the control
 render_ctrl(@g_rasterizer ,@g_scanline ,@r ,@m_alpha_slider );

end;

{ ON_RESIZE }
procedure the_application.on_resize;
var
 pf : pixel_formats;
 r  : renderer_base;

 rgba : aggclr;

begin
 pixfmt(pf ,rbuf_window );

 r.Construct   (@pf );
 rgba.ConstrDbl(1 ,1 ,1 );
 r.clear       (@rgba );

end;

{ TRANSFORM }
procedure the_application.transform;
begin
 x:=x - (width_ / 2 );
 y:=y - (height_ / 2 );

 g_angle:=ArcTan2(y ,x );
 g_scale:=Sqrt   (y * y + x * x ) / 100.0;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 on_mouse_button_down(x ,y ,flags );

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
var
 width_ ,height_ : int;

begin
 if flags and mouse_left <> 0 then
  begin
   width_ :=rbuf_window._width;
   height_:=rbuf_window._height;

   transform(_width ,_height ,x ,y );
   force_redraw;

  end;

 if flags and mouse_right <> 0 then
  begin
   g_skew_x:=x;
   g_skew_y:=y;

   force_redraw;

  end;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'This is the first example I used to implement and debug the scanline rasterizer, '#13 +
   'affine transformer, and basic renderers. The image is drawn over the old one     '#13 +
   'with a cetrain opacity value.'#13#13 +
   'How to play with:'#13#13 +
   'You can rotate and scale the "Lion" with the left mouse button. '#13 +
   'Right mouse button adds "skewing" transformations, '#13 +
   'proportional to the "X" coordinate. '#13 +
   'Change "Alpha" to draw funny looking "lions". '#13 +
   'Change window size to clear the window.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
// Rendering
 g_rasterizer.Construct;
 g_scanline.Construct;
 g_path.Construct;

 g_npaths:=0;

 g_x1:=0;
 g_y1:=0;
 g_x2:=0;
 g_y2:=0;

 g_base_dx:=0;
 g_base_dy:=0;

 g_angle:=0;
 g_scale:=1.0;

 g_skew_x:=0;
 g_skew_y:=0;
 g_nclick:=0;

// App
 app.Construct(pix_format ,flip_y );
 app.caption_ ('AGG Example. Lion (F1-Help)' );

 if app.init(512 ,400 ,window_resize ) then
  app.run;

 app.Destruct;

// Free
 g_rasterizer.Destruct;
 g_scanline.Destruct;
 g_path.Destruct;

END.