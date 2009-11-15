//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 circles ;

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
 agg_scale_ctrl ,

 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_p ,

 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_render_scanlines ,

 agg_bspline ,
 agg_ellipse ,
 agg_conv_transform ,
 agg_trans_affine ,
 agg_gsv_text 

{$I pixel_formats.inc }
{$I agg_mode.inc }

const
 flip_y = true;

 default_num_points = 10000;

 start_width  = 400;
 start_height = 400;

 spline_r_x : array[0..5 ] of double = (0.000000 ,0.200000 ,0.400000 ,0.910484 ,0.957258 ,1.000000 );
 spline_r_y : array[0..5 ] of double = (1.000000 ,0.800000 ,0.600000 ,0.066667 ,0.169697 ,0.600000 );

 spline_g_x : array[0..5 ] of double = (0.000000 ,0.292244 ,0.485655 ,0.564859 ,0.795607 ,1.000000 );
 spline_g_y : array[0..5 ] of double = (0.000000 ,0.607260 ,0.964065 ,0.892558 ,0.435571 ,0.000000 );

 spline_b_x : array[0..5 ] of double = (0.000000 ,0.055045 ,0.143034 ,0.433082 ,0.764859 ,1.000000 );
 spline_b_y : array[0..5 ] of double = (0.385480 ,0.128493 ,0.021416 ,0.271507 ,0.713974 ,1.000000 );


type
 scatter_point_ptr = ^scatter_point;
 scatter_point = record
   x ,y ,z : double;

   color : aggclr;

  end;

 the_application = object(platform_support )
   m_num_points : unsigned;
   m_points     : scatter_point_ptr;

   m_scale_ctrl_z     : scale_ctrl;
   m_slider_ctrl_sel  ,
   m_slider_ctrl_size : slider_ctrl;

   m_spline_r ,
   m_spline_g ,
   m_spline_b : bspline;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean; num_points : unsigned );
   destructor  Destruct;

   procedure generate;

   procedure on_init; virtual;
   procedure on_draw; virtual;
   procedure on_idle; virtual;

   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ RANDOM_DBL }
function random_dbl(start ,end_ : double ) : double;
var
 r : unsigned;

begin
 r:=system.Random($7fff ) and $7FFF;

 result:=r * (end_ - start ) / 32768.0 + start;

end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_num_points:=num_points;

 agg_getmem(pointer(m_points ) ,num_points * sizeof(scatter_point ) );

 m_scale_ctrl_z.Construct    (5 ,5  ,start_width - 5 ,12 ,not flip_y_ );
 m_slider_ctrl_sel.Construct (5 ,20 ,start_width - 5 ,27 ,not flip_y_ );
 m_slider_ctrl_size.Construct(5 ,35 ,start_width - 5 ,42 ,not flip_y_ );

 m_spline_r.Construct;
 m_spline_g.Construct;
 m_spline_b.Construct;

 m_spline_r.init(6 ,@spline_r_x ,@spline_r_y );
 m_spline_g.init(6 ,@spline_g_x ,@spline_g_y );
 m_spline_b.init(6 ,@spline_b_x ,@spline_b_y );

 add_ctrl(@m_scale_ctrl_z );
 add_ctrl(@m_slider_ctrl_sel );
 add_ctrl(@m_slider_ctrl_size );

 m_slider_ctrl_size.label_('Size' );
 m_slider_ctrl_sel.label_ ('Selectivity' );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_spline_r.Destruct;
 m_spline_g.Destruct;
 m_spline_b.Destruct;

 m_scale_ctrl_z.Destruct;
 m_slider_ctrl_sel.Destruct;
 m_slider_ctrl_size.Destruct;

 agg_freemem(pointer(m_points ) ,m_num_points * sizeof(scatter_point ) );

end;

{ GENERATE }
procedure the_application.generate;
var
 i : unsigned;

 rx ,ry ,z ,x ,y ,dist ,angle : double;

begin
 rx:=_initial_width / 3.5;
 ry:=_initial_height / 3.5;

 for i:=0 to m_num_points - 1 do
  begin
   z:=random_dbl(0.0, 1.0);

   scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).z:=z;

   x:=Cos(z * 2.0 * pi ) * rx;
   y:=Sin(z * 2.0 * pi ) * ry;

   dist :=random_dbl(0.0 ,rx / 2.0 );
   angle:=random_dbl(0.0 ,pi * 2.0 );

   scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).x:=
    _initial_width / 2.0  + x + Cos(angle ) * dist;

   scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).y:=
    _initial_height / 2.0 + y + Sin(angle ) * dist;

   scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).color.ConstrDbl(
    m_spline_r.get(z ) * 0.8 ,
    m_spline_g.get(z ) * 0.8 ,
    m_spline_b.get(z ) * 0.8 ,1.0 );

  end;

end;

{ ON_INIT }
procedure the_application.on_init;
begin
 generate;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pf : rasterizer_scanline_aa;
 sl : scanline_p8;

 pixf : pixel_formats;

 rb : renderer_base;
 rs : renderer_scanline_aa_solid;
 e1 : ellipse;
 t1 : conv_transform;

 rgba : aggclr;

 i ,n_drawn : unsigned;

 z ,alpha : double;

 buf : array[0..9 ] of char;
 txt : gsv_text;

 txt_o : gsv_text_outline;

begin
// Initialize structures
 pf.Construct;
 sl.Construct;

 pixfmt(pixf ,rbuf_window );

 rb.Construct(@pixf );
 rs.Construct(@rb );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear      (@rgba);

// Draw circles
 e1.Construct;
 t1.Construct(@e1 ,_trans_affine_resizing );

 n_drawn:=0;

 for i:=0 to  m_num_points - 1 do
  begin
   z:=scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).z;

   alpha:=1.0;

   if z < m_scale_ctrl_z._value1 then
    alpha:=
     1.0 -
     (m_scale_ctrl_z._value1 - z) *
     m_slider_ctrl_sel._value * 100.0;

   if z > m_scale_ctrl_z._value2 then
    alpha:=
     1.0 -
     (z - m_scale_ctrl_z._value2 ) *
     m_slider_ctrl_sel._value * 100.0;

   if alpha > 1.0 then
    alpha:=1.0;

   if alpha < 0.0 then
    alpha:=0.0;

   if alpha > 0.0 then
    begin
     e1.init(
      scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).x ,
      scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).y ,
      m_slider_ctrl_size._value * 5.0 ,
      m_slider_ctrl_size._value * 5.0 ,8 );

     pf.add_path(@t1 );

     rgba.ConstrMix(
      scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).color.r ,
      scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).color.g ,
      scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).color.b ,
      alpha );

     rs.color_       (@rgba );
     render_scanlines(@pf ,@sl ,@rs );

     inc(n_drawn );

    end;

  end;

// Render the controls
 render_ctrl(@pf ,@sl ,@rs ,@m_scale_ctrl_z );
 render_ctrl(@pf ,@sl ,@rs ,@m_slider_ctrl_sel );
 render_ctrl(@pf ,@sl ,@rs ,@m_slider_ctrl_size );

// Render the Text
 sprintf(@buf[0 ] ,'%08u' ,n_drawn );

 txt.Construct;
 txt.size_       (15.0);
 txt.text_       (@buf[0 ] );
 txt.start_point_(10.0 ,_initial_height - 20.0 );

 txt_o.Construct(@txt ,_trans_affine_resizing );

 pf.add_path(@txt_o );

 rgba.ConstrDbl(0 ,0 ,0 );
 rs.color_     (@rgba );

 render_scanlines(@pf ,@sl ,@rs );

// Free AGG resources
 pf.Destruct;
 sl.Destruct;

 txt.Destruct;
 txt_o.Destruct;
 
end;

{ ON_IDLE }
procedure the_application.on_idle;
var
 i : unsigned;

begin
 for i:=0 to m_num_points - 1 do
  begin
   scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).x:=
    scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).x +
    random_dbl(0 ,m_slider_ctrl_sel._value ) - m_slider_ctrl_sel._value * 0.5;

   scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).y:=
    scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).y +
    random_dbl(0 ,m_slider_ctrl_sel._value ) - m_slider_ctrl_sel._value * 0.5;

   scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).z:=
    scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).z +
    random_dbl(0 ,m_slider_ctrl_sel._value * 0.01 ) - m_slider_ctrl_sel._value * 0.005;

   if scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).z < 0.0 then
    scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).z:= 0.0;

   if scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).z > 1.0 then
    scatter_point_ptr(ptrcomp(m_points ) + i * sizeof(scatter_point ) ).z:= 1.0;

  end;

 force_redraw;

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
begin
 if flags and mouse_left <> 0 then
  begin
   generate;
   force_redraw;

  end;

 if flags and mouse_right <> 0 then
  wait_mode_(not _wait_mode );

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'This example just demonstrates that AGG can be used in different scatter plot '#13 +
   'apllications. There''s a number of small circles drawn. You can change '#13 +
   'the parameters of drawing, watching for the performance and the number '#13 +
   'of circles simultaneously rendered. Note, that the circles are drawn with '#13 +
   'high quality, possibly translucent, and with subpixel accuracy.'#13#13 +
   'How to play with:'#13#13 +
   'Press the left mouse button to generate a new set of points. '#13 +
   'Press the right mouse button to make the points randomly change their coordinates.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;
 err : integer;

 num_points : unsigned;

{$IFDEF WINDOWS}{$R circles.rc}{$ENDIF}

BEGIN
 num_points:=default_num_points;

 if paramcount > 0 then
  begin
   val(paramstr(1 ) ,num_points ,err );

   if num_points = 0 then
    num_points:=default_num_points;

   if num_points > 20000 then
    num_points:=20000;

  end;

 app.Construct(pix_format ,flip_y ,num_points );
 app.caption_ ('AGG Drawing random circles - A scatter plot prototype (F1-Help)' );

 if app.init(start_width ,start_height ,window_resize or window_keep_aspect_ratio ) then
  app.run;

 app.Destruct;

END.