//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 trans_curve2_ft ;

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
 agg_cbox_ctrl ,
 agg_slider_ctrl ,

 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_p ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_render_scanlines ,

 agg_math ,
 agg_conv_curve ,
 agg_conv_transform ,
 agg_trans_double_path ,
 agg_conv_bspline ,
 agg_conv_segmentator ,
 agg_conv_stroke ,
 agg_font_freetype ,
 agg_font_cache_manager ,
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
   m_feng  : font_engine_freetype_int16;
   m_fman  : font_cache_manager;
   m_poly1 ,
   m_poly2 : interactive_polygon;

   m_num_points       : slider_ctrl;
   m_fixed_len        ,
   m_preserve_x_scale ,
   m_animate          : cbox_ctrl;

   m_dx1 ,
   m_dy1 ,
   m_dx2 ,
   m_dy2 : array[0..5 ] of double;

   m_prev_animate : boolean;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;
   procedure on_ctrl_change; virtual;

   procedure move_point     (x ,y ,dx ,dy : double_ptr );
   procedure normalize_point(i : unsigned );

   procedure on_idle; virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_feng.Construct;
 m_fman.Construct (@m_feng );
 m_poly1.Construct(6 ,5.0 );
 m_poly2.Construct(6 ,5.0 );

 m_num_points.Construct      (5.0 ,5.0  ,340.0 ,12.0 ,not flip_y_ );
 m_fixed_len.Construct       (350 ,5.0  ,'Fixed Length' ,not flip_y_ );
 m_preserve_x_scale.Construct(465 ,5.0  ,'Preserve X scale' ,not flip_y_ );
 m_animate.Construct         (350 ,25.0 ,'Animate' ,not flip_y_ );

 m_prev_animate:=false;

 add_ctrl(@m_fixed_len );
 add_ctrl(@m_preserve_x_scale );
 add_ctrl(@m_animate );

 m_fixed_len.status_       (true );
 m_preserve_x_scale.status_(true );

 m_num_points.range_(10.0 ,400.0 );
 m_num_points.value_(200.0 );
 m_num_points.label_('Number of intermediate Points = %.3f' );

 add_ctrl(@m_num_points );

 m_poly1.close_(false );
 m_poly2.close_(false );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_num_points.Destruct;
 m_fixed_len.Destruct;
 m_preserve_x_scale.Destruct;
 m_animate.Destruct;

 m_poly1.Destruct;
 m_poly2.Destruct;

 m_feng.Destruct;
 m_fman.Destruct;

end;

{ ON_INIT }
procedure the_application.on_init;
begin
 m_poly1.xn_ptr(0 )^:= 10 + 50;
 m_poly1.yn_ptr(0 )^:=-10 + 50;
 m_poly1.xn_ptr(1 )^:= 10 + 150 + 20;
 m_poly1.yn_ptr(1 )^:=-10 + 150 - 20;
 m_poly1.xn_ptr(2 )^:= 10 + 250 - 20;
 m_poly1.yn_ptr(2 )^:=-10 + 250 + 20;
 m_poly1.xn_ptr(3 )^:= 10 + 350 + 20;
 m_poly1.yn_ptr(3 )^:=-10 + 350 - 20;
 m_poly1.xn_ptr(4 )^:= 10 + 450 - 20;
 m_poly1.yn_ptr(4 )^:=-10 + 450 + 20;
 m_poly1.xn_ptr(5 )^:= 10 + 550;
 m_poly1.yn_ptr(5 )^:=-10 + 550;

 m_poly2.xn_ptr(0 )^:=-10 + 50;
 m_poly2.yn_ptr(0 )^:= 10 + 50;
 m_poly2.xn_ptr(1 )^:=-10 + 150 + 20;
 m_poly2.yn_ptr(1 )^:= 10 + 150 - 20;
 m_poly2.xn_ptr(2 )^:=-10 + 250 - 20;
 m_poly2.yn_ptr(2 )^:= 10 + 250 + 20;
 m_poly2.xn_ptr(3 )^:=-10 + 350 + 20;
 m_poly2.yn_ptr(3 )^:= 10 + 350 - 20;
 m_poly2.xn_ptr(4 )^:=-10 + 450 - 20;
 m_poly2.yn_ptr(4 )^:= 10 + 450 + 20;
 m_poly2.xn_ptr(5 )^:=-10 + 550;
 m_poly2.yn_ptr(5 )^:= 10 + 550;

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

 path1 ,
 path2 : simple_polygon_vertex_source;

 bspline1 ,
 bspline2 : conv_bspline;

 tcurve  : trans_double_path;
 fcurves : conv_curve;
 fsegm   : conv_segmentator;
 ftrans  : conv_transform;

 x ,y : double;

 p : int8u_ptr;

 glyph : glyph_cache_ptr;
 
 stroke1 ,
 stroke2 : conv_stroke;

begin
// Initialize structures
 pixfmt(pixf ,rbuf_window );

 rb.Construct(@pixf );
 r.Construct (@rb );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear      (@rgba );

 sl.Construct;
 ras.Construct;

// Render the text
 path1.Construct(m_poly1.polygon ,m_poly1.num_points ,false ,false );
 path2.Construct(m_poly2.polygon ,m_poly2.num_points ,false ,false );

 bspline1.Construct(@path1 );
 bspline2.Construct(@path2 );

 bspline1.interpolation_step_(1.0 / m_num_points._value );
 bspline2.interpolation_step_(1.0 / m_num_points._value );

 tcurve.Construct;
 fcurves.Construct(m_fman.path_adaptor );
 fsegm.Construct  (@fcurves);
 ftrans.Construct (@fsegm ,@tcurve );

 tcurve.preserve_x_scale_(m_preserve_x_scale._status );

 if m_fixed_len._status then
  tcurve.base_length_(1140.0 );

 tcurve.base_height_(30.0 );
 tcurve.add_paths   (@bspline1 ,@bspline2 );

 fsegm.approximation_scale_  (3.0 );
 fcurves.approximation_scale_(5.0 );

 if m_feng.load_font('timesi.ttf' ,0 ,glyph_ren_outline ) then
  begin
   x:=0.0;
   y:=3.0;
   p:=@text_[0 ];

   m_feng.hinting_(false );
   m_feng.height_ (40.0 );

   while p^ <> 0 do
    begin
     glyph:=m_fman.glyph(p^ );

     if glyph <> NIL then
      begin
       if x > tcurve.total_length1 then
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

  end
 else
  message_(
   'Please copy file timesi.ttf to the current directory'#13 +
   'or download it from http://www.antigrain.com/timesi.zip' );

// Render the path curve
 stroke1.Construct(@bspline1 );
 stroke2.Construct(@bspline2 );

 stroke1.width_(2.0 );
 stroke2.width_(2.0 );

 rgba.ConstrInt(170 ,50 ,20 ,100 );
 r.color_      (@rgba );

 ras.add_path    (@stroke1 );
 render_scanlines(@ras ,@sl ,@r );

 ras.add_path    (@stroke2);
 render_scanlines(@ras ,@sl ,@r );

// Render the "poly" tool
 rgba.ConstrDbl(0 ,0.3 ,0.5 ,0.2 );
 r.color_      (@rgba );

 ras.add_path    (@m_poly1 );
 render_scanlines(@ras ,@sl ,@r );

 ras.add_path    (@m_poly2 );
 render_scanlines(@ras ,@sl ,@r );

// Render the controls
 render_ctrl(@ras ,@sl ,@r ,@m_fixed_len );
 render_ctrl(@ras ,@sl ,@r ,@m_preserve_x_scale );
 render_ctrl(@ras ,@sl ,@r ,@m_animate );
 render_ctrl(@ras ,@sl ,@r ,@m_num_points );

// Free AGG resources
 sl.Destruct;
 ras.Destruct;

 bspline1.Destruct;
 bspline2.Destruct;
 tcurve.Destruct;
 fcurves.Destruct;
 fsegm.Destruct;

 stroke1.Destruct;
 stroke2.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 if flags and mouse_left <> 0 then
  begin
   if m_poly1.on_mouse_move(x ,y ) then
    force_redraw;

   if m_poly2.on_mouse_move(x ,y ) then
    force_redraw;

  end;

 if flags and mouse_left = 0 then
  on_mouse_button_up(x ,y ,flags );

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
begin
 if flags and mouse_left <> 0 then
  begin
   if m_poly1.on_mouse_button_down(x ,y ) then
    force_redraw;

   if m_poly2.on_mouse_button_down(x ,y ) then
    force_redraw;

  end;
  
end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
 if m_poly1.on_mouse_button_up(x ,y ) then
  force_redraw;

 if m_poly2.on_mouse_button_up(x ,y ) then
  force_redraw;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'Similar to the "trans_curve1" demo, but here the transformer operates with two '#13 +
   'arbitrary curves. It requires more calculations, but gives you more freedom. '#13 +
   'In other words you will see :-).' +
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
       m_dx1[i ]:=((Random($7fff ) mod 1000 ) - 500 ) * 0.01;
       m_dy1[i ]:=((Random($7fff ) mod 1000 ) - 500 ) * 0.01;
       m_dx2[i ]:=((Random($7fff ) mod 1000 ) - 500 ) * 0.01;
       m_dy2[i ]:=((Random($7fff ) mod 1000 ) - 500 ) * 0.01;

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

{ NORMALIZE_POINT }
procedure the_application.normalize_point;
var
 d : double;

begin
 d:=
  calc_distance(
   m_poly1.xn_ptr(i )^ ,m_poly1.yn_ptr(i )^ ,
   m_poly2.xn_ptr(i )^ ,m_poly2.yn_ptr(i )^ );

// 28.8 is 20 * sqrt(2)
 if d > 28.28 then
  begin
   m_poly2.xn_ptr(i )^:=
    m_poly1.xn_ptr(i )^ +
    (m_poly2.xn_ptr(i )^ -
     m_poly1.xn_ptr(i )^ ) * 28.28 / d;

   m_poly2.yn_ptr(i )^:=
    m_poly1.yn_ptr(i )^ +
    (m_poly2.yn_ptr(i )^ -
     m_poly1.yn_ptr(i )^ ) * 28.28 / d;

  end;

end;

{ ON_IDLE }
procedure the_application.on_idle;
var
 i : int;

begin
 for i:=0 to 5 do
  begin
   move_point(m_poly1.xn_ptr(i ) ,m_poly1.yn_ptr(i ) ,@m_dx1[i ] ,@m_dy1[i ] );
   move_point(m_poly2.xn_ptr(i ) ,m_poly2.yn_ptr(i ) ,@m_dx2[i ] ,@m_dy2[i ] );

   normalize_point(i);

  end;

 force_redraw;

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format ,flip_y );
 app.caption_ ('AGG Example. Non-linear "Along-A-Curve" Transformer - FreeType (F1-Help)' );

 if app.init(600 ,600 ,window_resize ) then
  app.run;

 app.Destruct;

END.
