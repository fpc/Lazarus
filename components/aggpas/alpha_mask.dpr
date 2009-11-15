//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 alpha_mask ;

uses
 Math ,

 agg_basics ,
 agg_platform_support ,
 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,
 agg_pixfmt_gray ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_u ,
 agg_scanline_p ,
 agg_render_scanlines ,

 agg_ellipse ,
 agg_path_storage ,
 agg_conv_transform ,
 agg_bounding_rect ,
 agg_alpha_mask_u8 ,
 agg_trans_affine ,
 parse_lion_ ;

{$I agg_mode.inc }

const
 flip_y = true;

var
 g_path     : path_storage;
 g_colors   : array[0..99 ] of aggclr;
 g_path_idx : array[0..99 ] of unsigned;

 g_npaths : unsigned;

 g_x1 ,g_y1 ,g_x2 ,g_y2 ,
 g_base_dx  ,g_base_dy  ,
 g_angle    ,g_scale    ,
 g_skew_x   ,g_skew_y   : double;

 g_nclick : int;

 g_alpha_mask_rbuf : rendering_buffer;
 g_alpha_mask      : alpha_mask_gray8;
 g_rasterizer      : rasterizer_scanline_aa;

type
 the_application = object(platform_support )
   m_alpha_size : unsigned;
   m_alpha_buf  : char_ptr;
   m_alpha_rbuf : rendering_buffer;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure generate_alpha_mask(cx ,cy : int );

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

 m_alpha_size:=0;
 m_alpha_buf :=NIL;

 _parse_lion_;

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 agg_freemem(pointer(m_alpha_buf ) ,m_alpha_size );

end;

{ GENERATE_ALPHA_MASK }
procedure the_application.generate_alpha_mask;
var
 pixf : pixel_formats;
 rgba : aggclr;

 rb : renderer_base;
 r  : renderer_scanline_aa_solid;
 sl : scanline_p8;

 ell : ellipse;

 i : unsigned;

begin
 agg_freemem(pointer(m_alpha_buf ) ,m_alpha_size );

 m_alpha_size:=cx * cy;

 agg_getmem(pointer(m_alpha_buf ) ,m_alpha_size );

 g_alpha_mask_rbuf.attach(int8u_ptr(m_alpha_buf ) ,cx ,cy ,cx );

 pixfmt_gray8(pixf ,@g_alpha_mask_rbuf );
 rb.Construct(@pixf );
 r.Construct (@rb );
 sl.Construct;

 rgba.ConstrInt(0 );
 rb.clear      (@rgba );

 ell.Construct;

 for i:=0 to 9 do
  begin
   ell.init(
    Random($7fff ) mod cx ,
    Random($7fff ) mod cy ,
    Random($7fff ) mod 100 + 20 ,
    Random($7fff ) mod 100 + 20 ,100 );

   g_rasterizer.add_path(@ell );

   rgba.ConstrInt(Random($7fff ) and $FF ,Random($7fff ) and $FF );
   r.color_      (@rgba );

   render_scanlines(@g_rasterizer ,@sl ,@r );

  end;

 sl.Destruct;

end;

{ ON_RESIZE }
procedure the_application.on_resize;
begin
 generate_alpha_mask(sx ,sy );

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 width_ ,height_ : int;

 pixf : pixel_formats;

 rb : renderer_base;
 r  : renderer_scanline_aa_solid;
 sl : scanline_u8_am;

 rgba  : aggclr;
 trans : conv_transform;

 mtx : trans_affine;
 tat : trans_affine_translation;
 tas : trans_affine_scaling;
 tar : trans_affine_rotation;
 taw : trans_affine_skewing;

begin
 width_ :=rbuf_window._width;
 height_:=rbuf_window._height;

// Initialize structures
 pixfmt_bgr24(pixf ,rbuf_window );

 rb.Construct(@pixf );
 r.Construct (@rb );
 sl.Construct(@g_alpha_mask );

 rgba.ConstrInt(255 ,255 ,255 );
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

// This code renders the lion
 trans.Construct(@g_path ,@mtx );

 render_all_paths(@g_rasterizer ,@sl ,@r ,@trans ,@g_colors ,@g_path_idx ,g_npaths );

// Free AGG resources
 sl.Destruct;

end;

{ TRANSFORM }
procedure the_application.transform;
begin
 x:=x - width_ / 2;
 y:=y - height_ / 2;

 g_angle:=ArcTan2(y ,x );
 g_scale:=Sqrt(y * y + x * x ) / 100.0;

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
   'Alpha-mask is a simple method of clipping and masking polygons to a number'#13 +
   'of other arbitrary polygons. Alpha mask is a buffer that is mixed to the '#13 +
   'scanline container and controls the Anti-Aliasing values in it. It''s not the '#13 +
   'perfect mechanism of clipping, but it allows you not only to clip the polygons, '#13 +
   'but also to change the opacity in certain areas, i.e., the clipping can be '#13 +
   'translucent. '#13#13 +
   'How to play with:'#13#13 +
   'Press and drag the left mouse button to scale and rotate the lion.'#13 +
   'Resize the window to generate new alpha-mask.'#13 +
   'Use the right mouse button to skew the lion.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
// Rendering
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

 g_alpha_mask_rbuf.Construct;
 g_alpha_mask.Construct(@g_alpha_mask_rbuf );
 g_rasterizer.Construct;

// App
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. Lion with Alpha-Masking (F1-Help)' );

 if app.init(512 ,400 ,window_resize ) then
  app.run;

 app.Destruct;

// Free
 g_path.Destruct;
 g_alpha_mask_rbuf.Destruct;
 g_rasterizer.Destruct;

END.