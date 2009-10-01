//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 multi_clip ;

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
 agg_array ,
 agg_platform_support ,

 agg_ctrl ,
 agg_slider_ctrl ,

 agg_rasterizer_scanline_aa ,
 agg_rasterizer_outline_aa ,
 agg_scanline ,
 agg_scanline_u ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_renderer_outline_aa ,
 agg_renderer_primitives ,
 agg_renderer_markers ,
 agg_renderer_mclip ,
 agg_render_scanlines ,

 agg_path_storage ,
 agg_bounding_rect ,
 agg_trans_affine ,
 agg_conv_transform ,
 agg_ellipse ,
 agg_span_gradient ,
 agg_span_allocator ,
 agg_span_interpolator_linear ,
 parse_lion_

{$I pixel_formats.inc }
{$I agg_mode.inc }

const
 flip_y = true;

var
 g_rasterizer : rasterizer_scanline_aa;
 g_scanline   : scanline_u8;

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
 gradient_linear_color = object(pod_auto_array )
   m_c1 ,m_c2 ,c : aggclr;

   constructor Construct(c1 ,c2 : aggclr_ptr );

   function  size : unsigned; virtual;
   function  array_operator(i : unsigned ) : pointer; virtual;

   procedure colors(c1 ,c2 : aggclr_ptr );

  end;

 the_application = object(platform_support )
   m_num_cb : slider_ctrl;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_draw; virtual;

   procedure transform(width_ ,height_ ,x ,y : double );

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor gradient_linear_color.Construct;
begin
 m_c1:=c1^;
 m_c2:=c2^;

end;

{ SIZE }
function gradient_linear_color.size;
begin
 result:=256;

end;

{ ARRAY_OPERATOR }
function gradient_linear_color.array_operator;
begin
 c.v:=int8u((((m_c2.v - m_c1.v ) * i ) + (m_c1.v shl 8 ) ) shr 8 );
 c.r:=int8u((((m_c2.r - m_c1.r ) * i ) + (m_c1.r shl 8 ) ) shr 8 );
 c.g:=int8u((((m_c2.g - m_c1.g ) * i ) + (m_c1.g shl 8 ) ) shr 8 );
 c.b:=int8u((((m_c2.b - m_c1.b ) * i ) + (m_c1.b shl 8 ) ) shr 8 );
 c.a:=int8u((((m_c2.a - m_c1.a ) * i ) + (m_c1.a shl 8 ) ) shr 8 );

 result:=@c;

end;

{ COLORS }
procedure gradient_linear_color.colors;
begin
 m_c1:=c1^;
 m_c2:=c2^;
 
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

 m_num_cb.Construct(5 ,5 ,150 ,12 ,not flip_y_ );

 _parse_lion_;

 add_ctrl(@m_num_cb );

 m_num_cb.range_    (2 ,10 );
 //m_num_cb.num_steps_(8 );
 m_num_cb.label_    ('N=%.2f' );
 m_num_cb.no_transform;

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_num_cb.Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 i : unsigned;

 width_ ,height_ ,x ,y ,x1 ,y1 ,x2 ,y2 : int;

 pf : pixel_formats;
 r  : renderer_mclip;
 rs : renderer_scanline_aa_solid;

 rgba  ,
 rgbb  : aggclr;

 mtx : trans_affine;
 tat : trans_affine_translation;
 tas : trans_affine_scaling;
 tar : trans_affine_rotation;
 taw : trans_affine_skewing;

 trans : conv_transform;

 m : renderer_markers;
 w ,
 n : double;

 profile : line_profile_aa;

 ren : renderer_outline_aa;
 ras : rasterizer_outline_aa;
 grm : trans_affine;
 grf : gradient_circle;
 grc : gradient_linear_color;
 ell : ellipse;

 sa : span_allocator;
 sg : span_gradient;
 rg : renderer_scanline_aa;

 inter : span_interpolator_linear;

begin
 width_ :=rbuf_window._width;
 height_:=rbuf_window._height;

// Initialize structures
 pixfmt(pf ,rbuf_window );

 r.Construct (@pf );
 rs.Construct(@r );

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

 tat.Construct(_width / 2 ,_height / 2 );
 mtx.multiply (@tat );

 rgba.ConstrDbl(1 ,1 ,1 );
 r.clear       (@rgba );

// Custom Clip Renderer Base
 r.reset_clipping(false );  // Visibility: "false" means "no visible regions"

 n:=m_num_cb._value;
 x:=0;

 while x < n do
  begin
   y:=0;

   while y < n do
    begin
     x1:=trunc(width_  * x / n );
     y1:=trunc(height_ * y / n );
     x2:=trunc(width_  * (x + 1 ) / n );
     y2:=trunc(height_ * (y + 1 ) / n );

     r.add_clip_box(x1 + 5 ,y1 + 5 ,x2 - 5 ,y2 - 5 );

     inc(y );

    end;

   inc(x );

  end;{}

// Render the lion
 trans.Construct (@g_path ,@mtx );
 render_all_paths(@g_rasterizer ,@g_scanline ,@rs ,@trans ,@g_colors ,@g_path_idx ,g_npaths );{}

// The scanline rasterizer allows you to perform clipping to multiple
// regions "manually", like in the following code, but the "embedded" method
// shows much better performance.
{ for i:=0 to g_npaths - 1 do
  begin
   g_rasterizer.reset;
   g_rasterizer.add_path(@trans ,g_path_idx[i ] );

   rs.color_(@g_colors[i ] );

   n:=m_num_cb._value;
   x:=0;

   while x < n do
    begin
     y:=0;

     while y < n do
      begin
       x1:=trunc(width_  * x / n );
       y1:=trunc(height_ * y / n );
       x2:=trunc(width_  * (x + 1 ) / n );
       y2:=trunc(height_ * (y + 1 ) / n );

      // r should be of type renderer_base
       r.clip_box_(x1 + 5 ,y1 + 5 ,x2 - 5 ,y2 - 5 );

       render_scanlines(@g_rasterizer ,@g_scanline ,@rs );

       inc(y );

      end;

     inc(x );

    end;

  end;{}

// Render random Bresenham lines and markers
 m.Construct(@r );

 for i:=0 to 49 do
  begin
   rgba.ConstrInt(
    Random($7fff ) and $7F ,
    Random($7fff ) and $7F ,
    Random($7fff ) and $7F ,
    (Random($7fff ) and $7F ) + $7F );

   m.line_color_(@rgba );

   rgba.ConstrInt(
    Random($7fff ) and $7F ,
    Random($7fff ) and $7F ,
    Random($7fff ) and $7F ,
    (Random($7fff ) and $7F ) + $7F );

   m.fill_color_(@rgba );

   m.line(
    m.coord(Random($7fff ) mod width_ ) ,m.coord(Random($7fff ) mod height_ ) ,
    m.coord(Random($7fff ) mod width_ ) ,m.coord(Random($7fff ) mod height_ ) );

   m.marker(
    Random($7fff ) mod width_ ,Random($7fff ) mod height_ ,Random($7fff ) mod 10 + 5 ,
    marker_e(Random($7fff ) mod int(end_of_markers ) ) );

  end;

// Render random anti-aliased lines
 w:=5.0;

 profile.Construct;
 profile.width_(w );

 ren.Construct (@r ,@profile );
 ras.Construct (@ren );
 ras.round_cap_(true );

 for i:=0 to 49 do
  begin
   rgba.ConstrInt(
    Random($7fff ) and $7F ,
    Random($7fff ) and $7F ,
    Random($7fff ) and $7F ,
    (Random($7fff ) and $7F ) + $7F );

   ren.color_(@rgba );

   ras.move_to_d(Random($7fff ) mod width_ ,Random($7fff ) mod height_ );
   ras.line_to_d(Random($7fff ) mod width_ ,Random($7fff ) mod height_ );
   ras.render   (false );

  end;

// Render random circles with gradient
 grm.Construct;
 grf.Construct;
 rgba.ConstrInt(0 ,0 ,0 );
 rgbb.ConstrInt(0 ,0 ,0 );
 grc.Construct (@rgba ,@rgbb );
 ell.Construct;
 sa.Construct;
 inter.Construct(@grm );

 sg.Construct(@sa ,@inter ,@grf ,@grc ,0 ,10 );
 rg.Construct(@r ,@sg );

 for i:=0 to 49 do
  begin
   x:=Random($7fff ) mod width_;
   y:=Random($7fff ) mod height_;
   w:=Random($7fff ) mod 10 + 5;

   grm.reset;

   tas.Construct(w / 10.0 );
   grm.multiply (@tas );

   tat.Construct(x ,y );
   grm.multiply (@tat );

   grm.invert;

   rgba.ConstrInt(255 ,255 ,255 ,0 );
   rgbb.ConstrInt(
    Random($7fff ) and $7F ,
    Random($7fff ) and $7F ,
    Random($7fff ) and $7F ,255 );

   grc.colors(@rgba ,@rgbb );
   sg.color_function_(@grc );

   ell.init(x ,y ,w ,w ,32 );

   g_rasterizer.add_path(@ell );
   render_scanlines     (@g_rasterizer ,@g_scanline ,@rg );

  end;

// Render the controls
 r.reset_clipping(true ); // "true" means "all rendering buffer is visible".

 render_ctrl(@g_rasterizer ,@g_scanline ,@rs ,@m_num_cb );

// Free AGG resources
 profile.Destruct;
 ras.Destruct;
 sa.Destruct;
 r.Destruct;

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
   'A testing example that demonstrates clipping to multiple rectangular regions. '#13 +
   'It''s a low-level (pixel) clipping that can be useful to draw images clipped '#13 +
   'to a complex region with orthogonal boundaries. It can be useful in some window '#13 +
   'interfaces that use a custom mechanism to draw window content. The example    '#13 +
   'uses all possible rendering mechanisms.'#13#13 +
   'How to play with:'#13#13 +
   'Use the left mouse button to rotate and resize the lion.'#13 +
   'Use the right mouse button to skew the lion.' +
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
 app.caption_ ('AGG Example. Clipping to multiple rectangle regions (F1-Help)' );

 if app.init(512 ,400 ,window_resize ) then
  app.run;

 app.Destruct;

// Free
 g_rasterizer.Destruct;
 g_scanline.Destruct;
 g_path.Destruct;

END.