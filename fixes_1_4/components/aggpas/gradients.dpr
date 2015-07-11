//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 gradients ;

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
 Math ,SysUtils ,

 agg_basics ,
 agg_platform_support ,

 agg_ctrl ,
 agg_spline_ctrl ,
 agg_rbox_ctrl ,
 agg_gamma_ctrl ,

 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_u ,
 agg_scanline_p ,

 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_render_scanlines ,

 agg_array ,
 agg_conv_transform ,
 agg_span_gradient ,
 agg_span_interpolator_linear ,
 agg_span_allocator ,
 agg_trans_affine ,
 agg_ellipse

{$I pixel_formats.inc }
{$I agg_mode.inc }
{$I- }
const
 flip_y = true;

 center_x : double = 350;
 center_y : double = 280;

type
 color_function_profile = object(array_base )
   m_colors  : aggclr_ptr;
   m_profile : int8u_ptr;

   constructor Construct(colors : aggclr_ptr; profile : int8u_ptr );
                       
   function  size : unsigned; virtual;
   function  array_operator(i : unsigned ) : pointer; virtual;

  end;

 the_application = object(platform_support )
   m_profile  : gamma_ctrl;
   m_spline_r ,
   m_spline_g ,
   m_spline_b ,
   m_spline_a : spline_ctrl;
   m_rbox     : rbox_ctrl;

   m_pdx          ,
   m_pdy          ,
   m_center_x     ,
   m_center_y     ,
   m_scale        ,
   m_prev_scale   ,
   m_angle        ,
   m_prev_angle   ,
   m_scale_x      ,
   m_prev_scale_x ,
   m_scale_y      ,
   m_prev_scale_y : double;
   m_mouse_move   : boolean;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor color_function_profile.Construct;
begin
 m_colors :=colors;
 m_profile:=profile;
 
end;

{ SIZE }
function color_function_profile.size;
begin
 result:=256;

end;

{ ARRAY_OPERATOR }
function color_function_profile.array_operator;
begin
 result:=
  aggclr_ptr(
   ptrcomp(m_colors ) +
    int8u_ptr(ptrcomp(m_profile ) + i * sizeof(int8u ) )^ * sizeof(aggclr ) );

end;

{ CONSTRUCT }
constructor the_application.Construct;
var
 rgba : aggclr;

 fd  : text;
 err : integer;

 x ,y ,x2 ,y2 ,t : double;

begin
 inherited Construct(format_ ,flip_y_ );

 m_profile.Construct (10.0 ,10.0 ,200.0 ,170.0 - 5.0 ,not flip_y_ );
 m_spline_r.Construct(210 ,10 ,210 + 250 ,5 + 40 ,6 ,not flip_y_ );
 m_spline_g.Construct(210 ,10 + 40 ,210 + 250 ,5 + 80 ,6 ,not flip_y_ );
 m_spline_b.Construct(210 ,10 + 80 ,210 + 250 ,5 + 120 ,6 ,not flip_y_ );
 m_spline_a.Construct(210 ,10 + 120 ,210 + 250 ,5 + 160 ,6 ,not flip_y_ );
 m_rbox.Construct    (10.0 ,180.0 ,200.0 ,300.0 ,not flip_y_ );

 m_pdx:=0.0;
 m_pdy:=0.0;

 m_center_x:=center_x;
 m_center_y:=center_y;

 m_scale       :=1.0;
 m_prev_scale  :=1.0;
 m_angle       :=0.0;
 m_prev_angle  :=0.0;
 m_scale_x     :=1.0;
 m_prev_scale_x:=1.0;
 m_scale_y     :=1.0;
 m_prev_scale_y:=1.0;

 m_mouse_move:=false;

 add_ctrl(@m_profile );
 add_ctrl(@m_spline_r );
 add_ctrl(@m_spline_g );
 add_ctrl(@m_spline_b );
 add_ctrl(@m_spline_a );
 add_ctrl(@m_rbox );

 m_profile.border_width_(2.0 ,2.0 );

 rgba.ConstrDbl              (1.0 ,0.8 ,0.8 );
 m_spline_r.background_color_(@rgba );
 rgba.ConstrDbl              (0.8 ,1.0 ,0.8 );
 m_spline_g.background_color_(@rgba );
 rgba.ConstrDbl              (0.8 ,0.8 ,1.0 );
 m_spline_b.background_color_(@rgba );
 rgba.ConstrDbl              (1.0 ,1.0 ,1.0 );
 m_spline_a.background_color_(@rgba );

 m_spline_r.border_width_(1.0 ,2.0 );
 m_spline_g.border_width_(1.0 ,2.0 );
 m_spline_b.border_width_(1.0 ,2.0 );
 m_spline_a.border_width_(1.0 ,2.0 );
 m_rbox.border_width_    (2.0 ,2.0 );

 m_spline_r.point_(0 ,0.0 ,1.0 );
 m_spline_r.point_(1 ,1.0 / 5.0 ,1.0 - 1.0 / 5.0 );
 m_spline_r.point_(2 ,2.0 / 5.0 ,1.0 - 2.0 / 5.0 );
 m_spline_r.point_(3 ,3.0 / 5.0 ,1.0 - 3.0 / 5.0 );
 m_spline_r.point_(4 ,4.0 / 5.0 ,1.0 - 4.0 / 5.0 );
 m_spline_r.point_(5 ,1.0 ,0.0 );
 m_spline_r.update_spline;

 m_spline_g.point_(0 ,0.0 ,1.0 );
 m_spline_g.point_(1 ,1.0 / 5.0 ,1.0 - 1.0 / 5.0 );
 m_spline_g.point_(2 ,2.0 / 5.0 ,1.0 - 2.0 / 5.0 );
 m_spline_g.point_(3 ,3.0 / 5.0 ,1.0 - 3.0 / 5.0 );
 m_spline_g.point_(4 ,4.0 / 5.0 ,1.0 - 4.0 / 5.0 );
 m_spline_g.point_(5 ,1.0 ,0.0 );
 m_spline_g.update_spline;

 m_spline_b.point_(0 ,0.0 ,1.0 );
 m_spline_b.point_(1 ,1.0 / 5.0 ,1.0 - 1.0 / 5.0 );
 m_spline_b.point_(2 ,2.0 / 5.0 ,1.0 - 2.0 / 5.0 );
 m_spline_b.point_(3 ,3.0 / 5.0 ,1.0 - 3.0 / 5.0 );
 m_spline_b.point_(4 ,4.0 / 5.0 ,1.0 - 4.0 / 5.0 );
 m_spline_b.point_(5 ,1.0 ,0.0 );
 m_spline_b.update_spline;

 m_spline_a.point_(0 ,0.0 ,1.0 );
 m_spline_a.point_(1 ,1.0 / 5.0 , 1.0 );
 m_spline_a.point_(2 ,2.0 / 5.0 , 1.0 );
 m_spline_a.point_(3 ,3.0 / 5.0 , 1.0 );
 m_spline_a.point_(4 ,4.0 / 5.0 , 1.0 );
 m_spline_a.point_(5 ,1.0 ,1.0 );
 m_spline_a.update_spline;

 m_rbox.add_item ('Circular' );
 m_rbox.add_item ('Diamond' );
 m_rbox.add_item ('Linear' );
 m_rbox.add_item ('XY' );
 m_rbox.add_item ('sqrt(XY)' );
 m_rbox.add_item ('Conic' );
 m_rbox.cur_item_(0 );

 err:=IOResult;

 AssignFile(fd ,'settings.dat' );
 reset     (fd );

 err:=IOResult;

 if err = 0 then
  begin
   readln(fd ,t ); m_center_x:=t;
   readln(fd ,t ); m_center_y:=t;
   readln(fd ,t ); m_scale:=t;
   readln(fd ,t ); m_angle:=t;
   readln(fd ,x );
   readln(fd ,y ); m_spline_r.point_(0 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_r.point_(1 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_r.point_(2 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_r.point_(3 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_r.point_(4 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_r.point_(5 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_g.point_(0 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_g.point_(1 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_g.point_(2 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_g.point_(3 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_g.point_(4 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_g.point_(5 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_b.point_(0 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_b.point_(1 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_b.point_(2 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_b.point_(3 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_b.point_(4 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_b.point_(5 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_a.point_(0 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_a.point_(1 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_a.point_(2 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_a.point_(3 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_a.point_(4 ,x ,y );
   readln(fd ,x );
   readln(fd ,y ); m_spline_a.point_(5 ,x ,y );

   m_spline_r.update_spline;
   m_spline_g.update_spline;
   m_spline_b.update_spline;
   m_spline_a.update_spline;

   readln(fd ,x );
   readln(fd ,y );
   readln(fd ,x2 );
   readln(fd ,y2 );

   m_profile.values(x ,y ,x2 ,y2 );

   close(fd );

  end;

end;

{ DESTRUCT }
destructor the_application.Destruct;
var
 fd : text;

 x1 ,y1 ,x2 ,y2 : double;

begin
 AssignFile(fd ,'settings.dat' );
 rewrite   (fd );

 writeln(fd ,m_center_x:0:6 );
 writeln(fd ,m_center_y:0:6 );
 writeln(fd ,m_scale:0:6 );
 writeln(fd ,m_angle:0:6 );
 writeln(fd ,m_spline_r._x(0 ):0:6 );
 writeln(fd ,m_spline_r._y(0 ):0:6 );
 writeln(fd ,m_spline_r._x(1 ):0:6 );
 writeln(fd ,m_spline_r._y(1 ):0:6 );
 writeln(fd ,m_spline_r._x(2 ):0:6 );
 writeln(fd ,m_spline_r._y(2 ):0:6 );
 writeln(fd ,m_spline_r._x(3 ):0:6 );
 writeln(fd ,m_spline_r._y(3 ):0:6 );
 writeln(fd ,m_spline_r._x(4 ):0:6 );
 writeln(fd ,m_spline_r._y(4 ):0:6 );
 writeln(fd ,m_spline_r._x(5 ):0:6 );
 writeln(fd ,m_spline_r._y(5 ):0:6 );
 writeln(fd ,m_spline_g._x(0 ):0:6 );
 writeln(fd ,m_spline_g._y(0 ):0:6 );
 writeln(fd ,m_spline_g._x(1 ):0:6 );
 writeln(fd ,m_spline_g._y(1 ):0:6 );
 writeln(fd ,m_spline_g._x(2 ):0:6 );
 writeln(fd ,m_spline_g._y(2 ):0:6 );
 writeln(fd ,m_spline_g._x(3 ):0:6 );
 writeln(fd ,m_spline_g._y(3 ):0:6 );
 writeln(fd ,m_spline_g._x(4 ):0:6 );
 writeln(fd ,m_spline_g._y(4 ):0:6 );
 writeln(fd ,m_spline_g._x(5 ):0:6 );
 writeln(fd ,m_spline_g._y(5 ):0:6 );
 writeln(fd ,m_spline_b._x(0 ):0:6 );
 writeln(fd ,m_spline_b._y(0 ):0:6 );
 writeln(fd ,m_spline_b._x(1 ):0:6 );
 writeln(fd ,m_spline_b._y(1 ):0:6 );
 writeln(fd ,m_spline_b._x(2 ):0:6 );
 writeln(fd ,m_spline_b._y(2 ):0:6 );
 writeln(fd ,m_spline_b._x(3 ):0:6 );
 writeln(fd ,m_spline_b._y(3 ):0:6 );
 writeln(fd ,m_spline_b._x(4 ):0:6 );
 writeln(fd ,m_spline_b._y(4 ):0:6 );
 writeln(fd ,m_spline_b._x(5 ):0:6 );
 writeln(fd ,m_spline_b._y(5 ):0:6 );
 writeln(fd ,m_spline_a._x(0 ):0:6 );
 writeln(fd ,m_spline_a._y(0 ):0:6 );
 writeln(fd ,m_spline_a._x(1 ):0:6 );
 writeln(fd ,m_spline_a._y(1 ):0:6 );
 writeln(fd ,m_spline_a._x(2 ):0:6 );
 writeln(fd ,m_spline_a._y(2 ):0:6 );
 writeln(fd ,m_spline_a._x(3 ):0:6 );
 writeln(fd ,m_spline_a._y(3 ):0:6 );
 writeln(fd ,m_spline_a._x(4 ):0:6 );
 writeln(fd ,m_spline_a._y(4 ):0:6 );
 writeln(fd ,m_spline_a._x(5 ):0:6 );
 writeln(fd ,m_spline_a._y(5 ):0:6 );

 m_profile.values(@x1 ,@y1 ,@x2 ,@y2 );

 writeln(fd ,x1:0:6 );
 writeln(fd ,y1:0:6 );
 writeln(fd ,x2:0:6 );
 writeln(fd ,y2:0:6 );

 close(fd );

 inherited Destruct;

 m_profile.Destruct;
 m_spline_r.Destruct;
 m_spline_g.Destruct;
 m_spline_b.Destruct;
 m_spline_a.Destruct;
 m_rbox.Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pf : rasterizer_scanline_aa;
 sl : scanline_u8;
 rb : renderer_base;
 r  : renderer_scanline_aa_solid;

 pixf : pixel_formats;
 rgba : aggclr;

 ini_scale : double;

 color_profile : array[0..255 ] of aggclr;

 mtx1 ,mtx_g1 : trans_affine;

 tas : trans_affine_scaling;
 tar : trans_affine_rotation;
 tat : trans_affine_translation;

 e1 : ellipse;
 i  : int;
 t1 : conv_transform;

 gr_circle  : gradient_radial;
 gr_diamond : gradient_diamond;
 gr_x       : gradient_x;
 gr_xy      : gradient_xy;
 gr_sqrt_xy : gradient_sqrt_xy;
 gr_conic   : gradient_conic;

 gr_ptr : gradient_reflect_adaptor;

 span_alloc : span_allocator;
 colors     : color_function_profile;
 inter      : span_interpolator_linear;
 span_gen   : span_gradient;

 r1 : renderer_scanline_aa;

begin
// Initialize structures
 pf.Construct;
 sl.Construct;

 pixfmt(pixf ,rbuf_window );

 rb.Construct(@pixf );
 r.Construct (@rb );

 rgba.ConstrDbl(0 ,0 ,0 );
 rb.clear      (@rgba );

// Render the controls
 m_profile.text_size_(8.0 );

 render_ctrl(@pf ,@sl ,@r ,@m_profile );
 render_ctrl(@pf ,@sl ,@r ,@m_spline_r );
 render_ctrl(@pf ,@sl ,@r ,@m_spline_g );
 render_ctrl(@pf ,@sl ,@r ,@m_spline_b );
 render_ctrl(@pf ,@sl ,@r ,@m_spline_a );
 render_ctrl(@pf ,@sl ,@r ,@m_rbox );

// Render
 ini_scale:=1.0;

 mtx1.Construct;

 tas.Construct(ini_scale ,ini_scale ); mtx1.multiply(@tas );
 tar.Construct(deg2rad(0.0 ) );        mtx1.multiply(@tar );
 tat.Construct(center_x ,center_y );   mtx1.multiply(@tat );

 mtx1.multiply(_trans_affine_resizing );

 e1.Construct;
 e1.init(0.0 ,0.0 ,110.0 ,110.0 ,64 );

 mtx_g1.Construct;

 tas.Construct(ini_scale ,ini_scale );   mtx_g1.multiply(@tas );
 tas.Construct(m_scale ,m_scale );       mtx_g1.multiply(@tas );
 tas.Construct(m_scale_x ,m_scale_y );   mtx_g1.multiply(@tas );
 tar.Construct(m_angle );                mtx_g1.multiply(@tar );
 tat.Construct(m_center_x ,m_center_y ); mtx_g1.multiply(@tat );

 mtx_g1.multiply(_trans_affine_resizing );
 mtx_g1.invert;

 for i:=0 to 255 do
  begin
   rgba.ConstrDbl(
    double_ptr(ptrcomp(m_spline_r._spline ) + i * sizeof(double ) )^ ,
    double_ptr(ptrcomp(m_spline_g._spline ) + i * sizeof(double ) )^ ,
    double_ptr(ptrcomp(m_spline_b._spline ) + i * sizeof(double ) )^ ,
    double_ptr(ptrcomp(m_spline_a._spline ) + i * sizeof(double ) )^ );

   color_profile[i ]:=rgba;

  end;

 t1.Construct(@e1 ,@mtx1 );

 case m_rbox._cur_item of
  0 :
   begin
    gr_circle.Construct;
    gr_ptr.Construct(@gr_circle );

   end;

  1 :
   begin
    gr_diamond.Construct;
    gr_ptr.Construct(@gr_diamond );

   end;

  2 :
   begin
    gr_x.Construct;
    gr_ptr.Construct(@gr_x );

   end;

  3 :
   begin
    gr_xy.Construct;
    gr_ptr.Construct(@gr_xy );

   end;

  4 :
   begin
    gr_sqrt_xy.Construct;
    gr_ptr.Construct(@gr_sqrt_xy );

   end;

  5 :
   begin
    gr_conic.Construct;
    gr_ptr.Construct(@gr_conic );

   end;

 end;

 span_alloc.Construct;
 colors.Construct  (@color_profile ,int8u_ptr(m_profile.gamma ) );
 inter.Construct   (@mtx_g1);
 span_gen.Construct(@span_alloc ,@inter ,@gr_ptr ,@colors ,0 ,150 );
 r1.construct      (@rb ,@span_gen );

 pf.add_path     (@t1 );
 render_scanlines(@pf ,@sl ,@r1 );

// Free AGG resources
 pf.Destruct;
 sl.Destruct;

 span_alloc.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
var
 x2 ,y2 ,dx ,dy : double;

begin
 if m_mouse_move then
  begin
   x2:=x;
   y2:=y;

   _trans_affine_resizing.inverse_transform(_trans_affine_resizing ,@x2 ,@y2 );

   if flags and kbd_ctrl <> 0 then
    begin
     dx:=x2 - m_center_x;
     dy:=y2 - m_center_y;

     m_scale_x:=m_prev_scale_x * dx / m_pdx;
     m_scale_y:=m_prev_scale_y * dy / m_pdy;

     force_redraw;

    end
   else
    begin
     if flags and mouse_left <> 0 then
      begin
       m_center_x:=x2 + m_pdx;
       m_center_y:=y2 + m_pdy;

       force_redraw;

      end;

     if flags and mouse_right <> 0 then
      begin
       dx:=x2 - m_center_x;
       dy:=y2 - m_center_y;

       m_scale:=
        m_prev_scale *
        Sqrt(dx * dx + dy * dy ) /
        Sqrt(m_pdx * m_pdx + m_pdy * m_pdy );

       m_angle:=m_prev_angle + ArcTan2(dy ,dx ) - ArcTan2(m_pdy ,m_pdx );

       force_redraw;

      end;

    end;

  end;

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
var
 x2 ,y2 : double;

begin
 m_mouse_move:=true;

 x2:=x;
 y2:=y;

 _trans_affine_resizing.inverse_transform(_trans_affine_resizing ,@x2 ,@y2 );

 m_pdx:=m_center_x - x2;
 m_pdy:=m_center_y - y2;

 m_prev_scale  :=m_scale;
 m_prev_angle  :=m_angle + pi;
 m_prev_scale_x:=m_scale_x;
 m_prev_scale_y:=m_scale_y;

 force_redraw;

end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
 m_mouse_move:=false;

end;

{ ON_KEY }
procedure the_application.on_key;
var
 i  : int;
 fd : text;

 buf  : array[0..511 ] of char;
 rgba : aggclr;

begin
 if key = byte(' ' ) then
  begin
   AssignFile(fd ,'colors.dat' );
   rewrite   (fd );

   for i:=0 to 255 do
    begin
     rgba.ConstrDbl(
      double_ptr(ptrcomp(m_spline_r._spline ) + i * sizeof(double ) )^ ,
      double_ptr(ptrcomp(m_spline_g._spline ) + i * sizeof(double ) )^ ,
      double_ptr(ptrcomp(m_spline_b._spline ) + i * sizeof(double ) )^ ,
      double_ptr(ptrcomp(m_spline_a._spline ) + i * sizeof(double ) )^ );

     sprintf(@buf[0 ] ,'    %3d, ' ,rgba.r );
     sprintf(@buf[StrLen(@buf ) ] ,'%3d, ' ,rgba.g );
     sprintf(@buf[StrLen(@buf ) ] ,'%3d, ' ,rgba.b );
     sprintf(@buf[StrLen(@buf ) ] ,'%3d,' ,rgba.a );

     writeln(fd ,PChar(@buf[0 ] ) );

    end;

   close(fd );

   AssignFile(fd ,'profile.dat' );
   rewrite   (fd );

   for i:=0 to 255 do
    begin
     sprintf(@buf[0 ] ,'%3d, ' ,int8u_ptr(ptrcomp(m_profile.gamma ) + i * sizeof(int8u ) )^ );

     write(fd ,PChar(@buf[0 ] ) );

     if (i and $F) = $F then
      writeln(fd );

    end;

   close(fd );

  end;

 if key = key_f1 then
  message_(
   'This "sphere" is rendered with color gradients only. Initially there was an idea '#13 +
   'to compensate so called Mach Bands effect. To do so I added a gradient profile '#13 +
   'functor. Then the concept was extended to set a color profile. As a result you '#13 +
   'can render simple geometrical objects in 2D looking like 3D ones. In this example '#13 +
   'you can construct your own color profile and select the gradient function. '#13 +
   'There''re not so many gradient functions in AGG, but you can easily add your own.'#13#13 +
   'How to play with:'#13#13 +
   'Use the left mouse button to drag the "gradient".'#13 +
   'Use the right mouse button to scale and rotate the "gradient".'#13 +
   'Press the spacebar to write down the "colors.dat" file.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format ,flip_y );
 app.caption_ ('AGG gradients with Mach bands compensation (F1-Help)' );

 if app.init(512 ,400 ,window_resize or window_hw_buffer ) then
  app.run;

 app.Destruct;

END.