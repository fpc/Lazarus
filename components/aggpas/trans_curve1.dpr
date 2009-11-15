{target:win}
//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 trans_curve1 ;

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
 Windows ,

 agg_basics ,
 agg_platform_support ,

 agg_ctrl ,
 agg_cbox_ctrl ,
 agg_slider_ctrl ,

 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_p ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_render_scanlines ,

 agg_conv_curve ,
 agg_conv_transform ,
 agg_conv_bspline ,
 agg_conv_segmentator ,
 agg_conv_stroke ,
 agg_font_win32_tt ,
 agg_font_cache_manager ,
 agg_trans_single_path ,
 interactive_polygon_ 

{$I pixel_formats.inc }
{$I agg_mode.inc }

const
 flip_y = true;

 text_ : PChar =
  'Anti-Grain Geometry is designed as a set of loosely coupled ' +
  'algorithms and class templates united with a common idea, ' +
  'so that all the components can be easily combined. Also, ' +
  'the template based design allows you to replace any part of ' +
  'the library without the necessity to modify a single byte in ' +
  'the existing code. ';

type
 the_application = object(platform_support )
   m_feng : font_engine_win32_tt_int16;
   m_fman : font_cache_manager;
   m_poly : interactive_polygon;

   m_num_points       : slider_ctrl;
   m_close            ,
   m_preserve_x_scale ,
   m_fixed_len        ,
   m_animate          : cbox_ctrl;

   m_dx ,
   m_dy : array[0..5 ] of double;

   m_prev_animate : boolean;

   constructor Construct(dc : HDC; format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;
   procedure on_ctrl_change; virtual;

   procedure move_point(x ,y ,dx ,dy : double_ptr );

   procedure on_idle; virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_feng.Construct(dc );
 m_fman.Construct(@m_feng );
 m_poly.Construct(6 ,5.0 );

 m_num_points.Construct      (5.0 ,5.0  ,340.0 ,12.0 ,not flip_y_ );
 m_close.Construct           (350 ,5.0  ,'Close' ,not flip_y_ );
 m_preserve_x_scale.Construct(460 ,5.0  ,'Preserve X scale' ,not flip_y_ );
 m_fixed_len.Construct       (350 ,25.0 ,'Fixed Length' ,not flip_y_ );
 m_animate.Construct         (460 ,25.0 ,'Animate' ,not flip_y_ );

 m_prev_animate:=false;

 add_ctrl(@m_close );
 add_ctrl(@m_preserve_x_scale );
 add_ctrl(@m_fixed_len );
 add_ctrl(@m_animate );

 m_preserve_x_scale.status_(true );
 m_fixed_len.status_       (true );

 m_num_points.range_(10.0 ,400.0 );
 m_num_points.value_(200.0 );
 m_num_points.label_('Number of intermediate Points = %.3f' );

 add_ctrl(@m_num_points );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_poly.Destruct;

 m_num_points.Destruct;
 m_close.Destruct;
 m_preserve_x_scale.Destruct;
 m_fixed_len.Destruct;
 m_animate.Destruct;

 m_feng.Destruct;
 m_fman.Destruct;

end;

{ ON_INIT }
procedure the_application.on_init;
begin
 m_poly.xn_ptr(0 )^:=50;
 m_poly.yn_ptr(0 )^:=50;
 m_poly.xn_ptr(1 )^:=150 + 20;
 m_poly.yn_ptr(1 )^:=150 - 20;
 m_poly.xn_ptr(2 )^:=250 - 20;
 m_poly.yn_ptr(2 )^:=250 + 20;
 m_poly.xn_ptr(3 )^:=350 + 20;
 m_poly.yn_ptr(3 )^:=350 - 20;
 m_poly.xn_ptr(4 )^:=450 - 20;
 m_poly.yn_ptr(4 )^:=450 + 20;
 m_poly.xn_ptr(5 )^:=550;
 m_poly.yn_ptr(5 )^:=550;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;
 rgba : aggclr;

 rb : renderer_base;
 r  : renderer_scanline_aa_solid;
 sl : scanline_p8;

 ras : rasterizer_scanline_aa;

 path    : simple_polygon_vertex_source;
 bspline : conv_bspline;
 tcurve  : trans_single_path;
 fcurves : conv_curve;
 fsegm   : conv_segmentator;
 ftrans  : conv_transform;

 x ,y : double;

 p : int8u_ptr;

 glyph : glyph_cache_ptr;

 stroke : conv_stroke;

begin
// Initialize structures
 pixfmt(pixf ,rbuf_window );

 rb.Construct(@pixf );
 r.Construct (@rb );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear      (@rgba );

 sl.Construct;
 ras.Construct;

 m_poly.close_(m_close._status );

// Render the text
 path.Construct   (m_poly.polygon ,m_poly.num_points ,false ,m_close._status );
 bspline.Construct(@path );

 bspline.interpolation_step_(1.0 / m_num_points._value );

 tcurve.Construct;
 tcurve.add_path         (@bspline );
 tcurve.preserve_x_scale_(m_preserve_x_scale._status );

 if m_fixed_len._status then
  tcurve.base_length_(1120 );

 fcurves.Construct(m_fman.path_adaptor );
 fsegm.Construct  (@fcurves );
 ftrans.Construct (@fsegm ,@tcurve );

 fsegm.approximation_scale_  (3.0 );
 fcurves.approximation_scale_(2.0 );

 m_feng.height_(40.0 );
 //m_feng.italic_(true );

 if m_feng.create_font('Times New Roman' ,glyph_ren_outline ) then
  begin
   x:=0.0;
   y:=3.0;
   p:=@text_[0 ];

   while p^ <> 0 do
    begin
     glyph:=m_fman.glyph(p^ );

     if glyph <> NIL then
      begin
       if x > tcurve.total_length then
        break;

       m_fman.add_kerning           (@x ,@y );
       m_fman.init_embedded_adaptors(glyph ,x ,y );

       if glyph.data_type = glyph_data_outline then
        begin
         ras.reset;
         ras.add_path(@ftrans );

         rgba.ConstrInt(0 ,0 ,0 );
         r.color_      (@rgba );

         render_scanlines(@ras ,@sl ,@r );

        end;

      // increment pen position
       x:=x + glyph.advance_x;
       y:=y + glyph.advance_y;

      end;

     inc(ptrcomp(p ) ,sizeof(int8u ) );

    end;

  end;

// Render the path curve
 stroke.Construct(@bspline );
 stroke.width_   (2.0 );

 rgba.ConstrInt(170 ,50 ,20 ,100 );
 r.color_      (@rgba );

 ras.add_path    (@stroke );
 render_scanlines(@ras ,@sl ,@r );

// Render the "poly" tool
 rgba.ConstrDbl(0 ,0.3 ,0.5 ,0.3 );
 r.color_      (@rgba );

 ras.add_path    (@m_poly );
 render_scanlines(@ras ,@sl ,@r );

// Render the controls
 render_ctrl(@ras ,@sl ,@r ,@m_close );
 render_ctrl(@ras ,@sl ,@r ,@m_preserve_x_scale );
 render_ctrl(@ras ,@sl ,@r ,@m_fixed_len );
 render_ctrl(@ras ,@sl ,@r ,@m_animate );
 render_ctrl(@ras ,@sl ,@r ,@m_num_points );

// Free AGG resources
 sl.Destruct;
 ras.Destruct;

 bspline.Destruct;
 tcurve.Destruct;
 fcurves.Destruct;
 fsegm.Destruct;

 stroke.Destruct;

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
 if key = key_f1 then
  message_(
   'This is a "kinda-cool-stuff" demo that performs non-linear transformations and '#13 +
   'draws vector text along a curve. Note that it''s not just calculating of the glyph '#13 +
   'angles and positions, they are transformed as if they were elastic. The curve is '#13 +
   'calculated as a bicubic spline. The option "Preserve X scale" makes the converter '#13 +
   'distribute all the points uniformly along the curve. If it''s unchechked, the scale '#13 +
   'will be proportional to the distance between the control points.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

{ ON_CTRL_CHANGE }
procedure the_application.on_ctrl_change;
var
 i : int;

begin
 if m_animate._status <> m_prev_animate then
  begin
   if m_animate._status then
    begin
     on_init;

     for i:=0 to 5 do
      begin
       m_dx[i ]:=((Random($7fff ) mod 1000 ) - 500 ) * 0.01;
       m_dy[i ]:=((Random($7fff ) mod 1000 ) - 500 ) * 0.01;

      end;

     wait_mode_(false );

    end
   else
    wait_mode_(true );

   m_prev_animate:=m_animate._status;

  end;

end;

{ MOVE_POINT }
procedure the_application.move_point;
begin
 if x^ < 0.0 then
  begin
   x^ :=0.0;
   dx^:=-dx^;

  end;

 if x^ > _width then
  begin
   x^ :=_width;
   dx^:=-dx^;

  end;

 if y^ < 0.0 then
  begin
   y^ :=0.0;
   dy^:=-dy^;

  end;

 if y^ > _height then
  begin
   y^ :=_height;
   dy^:=-dy^;

  end;

 x^:=x^ + dx^;
 y^:=y^ + dy^;

end;

{ ON_IDLE }
procedure the_application.on_idle;
var
 i : int;

begin
 for i:=0 to 5 do
  move_point(m_poly.xn_ptr(i ) ,m_poly.yn_ptr(i ) ,@m_dx[i ] ,@m_dy[i ] );

 force_redraw;

end;

VAR
 app : the_application;
 dc  : HDC;

BEGIN
 dc:=GetDC(0 );

 app.Construct(dc ,pix_format ,flip_y );
 app.caption_ ('AGG Example. Non-linear "Along-A-Curve" Transformer - Win32 (F1-Help)' );

 if app.init(600 ,600 ,window_resize ) then
  app.run;

 app.Destruct;

 ReleaseDC(0 ,dc );

END.