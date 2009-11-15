//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 lion_outline ;

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
 agg_cbox_ctrl ,

 agg_rasterizer_scanline_aa ,
 agg_rasterizer_outline_aa ,
 agg_scanline ,
 agg_scanline_p ,

 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_renderer_outline_aa ,
 agg_render_scanlines ,

 agg_path_storage ,
 agg_bounding_rect ,
 agg_trans_affine ,
 agg_conv_stroke ,
 agg_conv_transform ,
 agg_vertex_source ,
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
   m_width_slider : slider_ctrl;
   m_scanline     : cbox_ctrl;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

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

 m_width_slider.Construct(5   ,5 ,150 ,12 ,not flip_y_ );
 m_scanline.Construct    (160 ,5 ,'Use Scanline Rasterizer' ,not flip_y_ );

 _parse_lion_;

 add_ctrl(@m_width_slider );

 m_width_slider.no_transform;
 m_width_slider.range_(0.0 ,4.0 );
 m_width_slider.value_(1.0 );
 m_width_slider.label_('Width %3.2f' );

 add_ctrl(@m_scanline );

 m_scanline.no_transform;

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_width_slider.Destruct;
 m_scanline.Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 width_ ,height_ : int;

 w : double;

 pixf : pixel_formats;

 rb : renderer_base;
 r  : renderer_scanline_aa_solid;

 rgba : aggclr;

 mtx : trans_affine;
 tat : trans_affine_translation;
 tas : trans_affine_scaling;
 tar : trans_affine_rotation;
 taw : trans_affine_skewing;

 stroke : conv_stroke;
 trans  : conv_transform;

 gm_no   : vertex_source;
 profile : line_profile_aa;

 ren : renderer_outline_aa;
 ras : rasterizer_outline_aa;

begin
 width_ :=rbuf_window._width;
 height_:=rbuf_window._height;

// Initialize structures
 pixfmt(pixf ,rbuf_window );

 rb.Construct(@pixf );
 r.Construct (@rb );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear      (@rgba );

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

// Render lion
 if m_scanline._status then
  begin
   stroke.Construct(@g_path );
   stroke.width_   (m_width_slider._value );
   trans.Construct (@stroke ,@mtx );

   render_all_paths(@g_rasterizer ,@g_scanline ,@r ,@trans ,@g_colors ,@g_path_idx ,g_npaths );

   stroke.Destruct;

  end
 else
  begin
   w:=m_width_slider._value * mtx.scale;

   gm_no.Construct;
   profile.Construct(w ,@gm_no );

   ren.Construct  (@rb ,@profile );
   ras.Construct  (@ren );
   trans.Construct(@g_path ,@mtx );

   ras.render_all_paths(@trans ,@g_colors ,@g_path_idx ,g_npaths );

   profile.Destruct;
   ras.Destruct;

  end;

// Render the control
 render_ctrl(@g_rasterizer ,@g_scanline ,@r ,@m_width_slider );
 render_ctrl(@g_rasterizer ,@g_scanline ,@r ,@m_scanline );

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

   transform(width_ ,height_ ,x ,y );
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
   'The example demonstrates my new algorithm of drawing Anti-Aliased lines. '#13 +
   'The algorithm works about 2.5 times faster than the scanline rasterizer '#13 +
   'but has some restrictions, particularly, line joins can be only of the '#13 +
   '"miter" type, and when so called miter limit is exceded, they are not as '#13 +
   'accurate as generated by the stroke converter (conv_stroke).'#13#13 +
   'How to play with:'#13#13 +
   'To see the difference, maximize the window and try to rotate and scale '#13 +
   'the "lion" with and without using the scanline rasterizer (a checkbox '#13 +
   'at the bottom). The difference in performance is obvious.' +
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

 if app.init(512 ,512 ,window_resize ) then
  app.run;

 app.Destruct;

// Free
 g_rasterizer.Destruct;
 g_scanline.Destruct;
 g_path.Destruct;

END.