//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 lion_lens ;

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

 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_p ,

 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_render_scanlines ,

 agg_path_storage ,
 agg_bounding_rect ,
 agg_trans_affine ,
 agg_trans_warp_magnifier ,
 agg_conv_transform ,
 agg_conv_segmentator ,
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
   m_magn_slider   ,
   m_radius_slider : slider_ctrl;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_init; virtual;
   procedure on_draw; virtual;

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

 m_magn_slider.Construct  (5 ,5  ,495 ,12 ,not flip_y_ );
 m_radius_slider.Construct(5 ,20 ,495 ,27 ,not flip_y_ );

 _parse_lion_;

 add_ctrl(@m_magn_slider );

 m_magn_slider.no_transform;
 m_magn_slider.range_(0.01 ,4.0 );
 m_magn_slider.value_(3.0 );
 m_magn_slider.label_('Scale=%3.2f' );

 add_ctrl(@m_radius_slider );

 m_radius_slider.no_transform;
 m_radius_slider.range_(0.0 ,100.0 );
 m_radius_slider.value_(70.0 );
 m_radius_slider.label_('Radius=%3.2f' );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_magn_slider.Destruct;
 m_radius_slider.Destruct;

end;

{ ON_INIT }
procedure the_application.on_init;
begin
 g_x1:=200;
 g_y1:=150;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;

 rb : renderer_base;
 r  : renderer_scanline_aa_solid;

 rgba : aggclr;

 lens : trans_warp_magnifier;
 segm : conv_segmentator;

 mtx : trans_affine;
 tat : trans_affine_translation;
 tar : trans_affine_rotation;

 trans_mtx  ,
 trans_lens : conv_transform;

begin
// Initialize structures
 pixfmt(pixf ,rbuf_window );

 rb.Construct(@pixf );
 r.Construct (@rb );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear      (@rgba );

// Transform lion
 lens.Construct;
 lens.center       (g_x1 ,g_y1 );
 lens.magnification(m_magn_slider._value );
 lens.radius       (m_radius_slider._value / m_magn_slider._value );

 segm.Construct(@g_path );
 mtx.Construct;

 tat.Construct(-g_base_dx ,-g_base_dy );
 mtx.multiply (@tat );

 tar.Construct(g_angle + pi );
 mtx.multiply (@tar );

 tat.Construct(_width / 2 ,_height / 2);
 mtx.multiply (@tat );

 trans_mtx.Construct (@segm ,@mtx );
 trans_lens.Construct(@trans_mtx ,@lens );

 render_all_paths(@g_rasterizer ,@g_scanline ,@r ,@trans_lens ,@g_colors ,@g_path_idx ,g_npaths );

// Render the controls
 render_ctrl(@g_rasterizer ,@g_scanline ,@r ,@m_magn_slider );
 render_ctrl(@g_rasterizer ,@g_scanline ,@r ,@m_radius_slider );

// Free
 segm.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 on_mouse_button_down(x ,y ,flags );

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
begin
 if flags and mouse_left <> 0 then
  begin
   g_x1:=x;
   g_y1:=y;

   force_redraw;

  end;

 if flags and mouse_right <> 0 then
  begin
   g_x2:=x;
   g_y2:=y;

   force_redraw;

  end;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'This example exhibits a non-linear transformer that "magnifies" vertices that fall '#13 +
   'inside a circle and extends the rest (trans_warp_magnifier). Non-linear transformations '#13 +
   'are tricky because straight lines become curves. To achieve the correct result we need '#13 +
   'to divide long line segments into short ones. The example also demonstrates the use of '#13 +
   'conv_segmentator that does this division job. The transformer can also shrink away '#13 +
   'the image if the scaling value is less than 1.'#13#13 +
   'How to play with:'#13#13 +
   'Drag the center of the "lens" with the left mouse button and change the "Scale" and "Radius".   '#13 +
   'To watch for an amazing effect, set the scale to the minimum (0.01), decrease the radius '#13 +
   'to about 1 and drag the "lens". You will see it behaves like a black hole consuming space '#13 +
   'around it. Move the lens somewhere to the side of the window and change the radius. It looks '#13 +
   'like changing the event horizon of the "black hole".' +
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

 if app.init(500 ,600 ,window_resize ) then
  app.run;

 app.Destruct;

// Free
 g_rasterizer.Destruct;
 g_scanline.Destruct;
 g_path.Destruct;

END.