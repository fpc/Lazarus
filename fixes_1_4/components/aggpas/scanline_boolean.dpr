//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 scanline_boolean ;

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
 agg_rbox_ctrl ,

 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_p ,
 agg_scanline_u ,
 agg_scanline_bin ,
 agg_scanline_boolean_algebra ,

 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_render_scanlines ,

 agg_path_storage ,
 agg_ellipse ,
 agg_gamma_functions ,
 interactive_polygon_

{$I pixel_formats.inc }
{$I agg_mode.inc }

const
 flip_y = true;

type
 the_application = object(platform_support )
   m_quad1 ,
   m_quad2 : interactive_polygon;

   m_trans_type : rbox_ctrl;

   m_reset : cbox_ctrl;
   m_mul1  ,
   m_mul2  : slider_ctrl;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_ctrl_change; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ GENERATE_CIRCLES }
procedure generate_circles(ps : path_storage_ptr; quad : double_ptr; num_circles : unsigned; radius : double );
var
 ell : ellipse;

 i ,j ,n1 ,n2 : unsigned;

begin
 ps.remove_all;
 ell.Construct;

 for i:=0 to 3 do
  begin
   n1:=i * 2;

   if i < 3 then
    n2:=i * 2 + 2
   else
    n2:=0;

   for j:=0 to num_circles - 1 do
    begin
     ell.init(
      double_ptr(ptrcomp(quad ) + n1 * sizeof(double ) )^ +
      (double_ptr(ptrcomp(quad ) + n2 * sizeof(double ) )^ -
       double_ptr(ptrcomp(quad ) + n1 * sizeof(double ) )^ ) *
      j / num_circles ,
      double_ptr(ptrcomp(quad ) + (n1 + 1 ) * sizeof(double ) )^ +
      (double_ptr(ptrcomp(quad ) + (n2 + 1 ) * sizeof(double ) )^ -
       double_ptr(ptrcomp(quad ) + (n1 + 1 ) * sizeof(double ) )^ ) *
      j / num_circles ,
      radius ,radius ,100 );

     ps.add_path(@ell ,0 ,false );

    end;

  end;

end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_quad1.Construct(4 ,5.0 );
 m_quad2.Construct(4 ,5.0 );

 m_trans_type.Construct(420 ,5.0 ,420 + 130.0 ,145.0 ,not flip_y_ );

 m_reset.Construct(350 ,5.0 ,'Reset' ,not flip_y_ );

 m_mul1.Construct(5.0 ,5.0 ,340.0 ,12.0 ,not flip_y_ );
 m_mul2.Construct(5.0 ,20.0 ,340.0 ,27.0 ,not flip_y_ );

 m_trans_type.add_item ('Union' );
 m_trans_type.add_item ('Intersection' );
 m_trans_type.add_item ('Linear XOR' );
 m_trans_type.add_item ('Saddle XOR' );
 m_trans_type.add_item ('Abs Diff XOR' );
 m_trans_type.add_item ('A-B' );
 m_trans_type.add_item ('B-A' );
 m_trans_type.cur_item_(0 );

 add_ctrl(@m_trans_type );
 add_ctrl(@m_reset );
 add_ctrl(@m_mul1 );
 add_ctrl(@m_mul2 );

 m_mul1.value_(1.0 );
 m_mul2.value_(1.0 );
 m_mul1.label_('Opacity1=%.3f' );
 m_mul2.label_('Opacity2=%.3f' );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_quad1.Destruct;
 m_quad2.Destruct;

 m_trans_type.Destruct;
 m_reset.Destruct;
 m_mul1.Destruct;
 m_mul2.Destruct;

end;

{ ON_INIT }
procedure the_application.on_init;
begin
 m_quad1.xn_ptr(0 )^:=50;
 m_quad1.yn_ptr(0 )^:=200 - 20;
 m_quad1.xn_ptr(1 )^:=_width / 2 - 25;
 m_quad1.yn_ptr(1 )^:=200;
 m_quad1.xn_ptr(2 )^:=_width / 2 - 25;
 m_quad1.yn_ptr(2 )^:=_height - 50 - 20;
 m_quad1.xn_ptr(3 )^:=50;
 m_quad1.yn_ptr(3 )^:=_height - 50;

 m_quad2.xn_ptr(0 )^:=_width / 2 + 25;
 m_quad2.yn_ptr(0 )^:=200 - 20;
 m_quad2.xn_ptr(1 )^:=_width - 50;
 m_quad2.yn_ptr(1 )^:=200;
 m_quad2.xn_ptr(2 )^:=_width - 50;
 m_quad2.yn_ptr(2 )^:=_height - 50 - 20;
 m_quad2.xn_ptr(3 )^:=_width / 2 + 25;
 m_quad2.yn_ptr(3 )^:=_height - 50;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;

 rb : renderer_base;
 r  : renderer_scanline_aa_solid;
 sl : scanline_p8;

 ras  ,
 ras1 ,
 ras2 : rasterizer_scanline_aa;
 rgba : aggclr;

 op : sbool_op_e;
 gm : gamma_multiply;

 ps1 ,
 ps2 : path_storage;

 sl_result ,sl1 ,sl2 : scanline_p8;

 sren : renderer_scanline_aa_solid;

begin
// Initialize structures
 pixfmt(pixf ,rbuf_window );

 rb.Construct(@pixf );
 r.Construct (@rb );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear      (@rgba );

 sl.Construct;
 ras.Construct;
 ras1.Construct;
 ras2.Construct;

// Draw
 op:=sbool_op_e(m_trans_type._cur_item );

 gm.Construct(m_mul1._value );
 ras1.gamma  (@gm );

 gm.Construct(m_mul2._value );
 ras2.gamma  (@gm );

 ras.clip_box(0 ,0 ,_width ,_height );

 ps1.Construct;
 generate_circles(@ps1 ,m_quad1.polygon ,5 ,20 );

 ps2.Construct;
 generate_circles(@ps2 ,m_quad2.polygon ,5 ,20 );

 ras1.filling_rule(fill_even_odd );

// Bottom Layer of Polygon1
 rgba.ConstrInt(240, 255, 200, 100 );
 r.color_      (@rgba );

 ras1.add_path   (@ps1 );
 render_scanlines(@ras1 ,@sl ,@r );

// Bottom Layer of Polygon2
 rgba.ConstrInt(255 ,240 ,240 ,100 );
 r.color_      (@rgba );

 ras2.add_path   (@ps2 );
 render_scanlines(@ras2 ,@sl ,@r );

// Combine shapes
 sl_result.Construct;
 sl1.Construct;
 sl2.Construct;
 sren.Construct(@rb );

 rgba.ConstrInt(0 ,0 ,0 );
 sren.color_   (@rgba );

 sbool_combine_shapes_aa(op ,@ras1 ,@ras2 ,@sl1 ,@sl2 ,@sl_result ,@sren );

// Render the "quad" tools
 rgba.ConstrDbl(0 ,0.3 ,0.5 ,0.6 );
 r.color_      (@rgba );

 ras.add_path    (@m_quad1 );
 render_scanlines(@ras ,@sl ,@r );

 ras.add_path    (@m_quad2 );
 render_scanlines(@ras ,@sl ,@r );

// Render the controls
 render_ctrl(@ras ,@sl ,@r ,@m_trans_type);
 render_ctrl(@ras ,@sl ,@r ,@m_reset);
 render_ctrl(@ras ,@sl ,@r ,@m_mul1);
 render_ctrl(@ras ,@sl ,@r ,@m_mul2);

// Free AGG resources
 ps1.Destruct;
 ps2.Destruct;

 ras2.Destruct;
 ras1.Destruct;

 sl_result.Destruct;
 sl1.Destruct;
 sl2.Destruct;

 ras.Destruct;
 sl.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 if flags and mouse_left <> 0 then
  if m_quad1.on_mouse_move(x ,y ) or
     m_quad2.on_mouse_move(x ,y ) then
   force_redraw;

 if flags and mouse_left = 0 then
  on_mouse_button_up(x ,y ,flags );

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
begin
 if flags and mouse_left <> 0 then
  if m_quad1.on_mouse_button_down(x ,y ) or
     m_quad2.on_mouse_button_down(x ,y ) then
   force_redraw;

end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
 if m_quad1.on_mouse_button_up(x ,y ) or
    m_quad2.on_mouse_button_up(x ,y ) then
  force_redraw;

end;

{ ON_CTRL_CHANGE }
procedure the_application.on_ctrl_change;
begin
 if m_reset._status then
  begin
   on_init;
   m_reset.status_(false );
   force_redraw;

  end;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'A new method to perform boolean operations on polygons (Union, Intersection, XOR, '#13 +
   'and Difference). It uses the scanline approach and in typical screen resolutions '#13 +
   'works much faster (about 10 times) than vectorial algorithms like General Polygon '#13 +
   'Clipper. It preserves perfect Anti-Aliasing and besides, can work with translucency. '#13 +
   'There are two XOR operations, Linear XOR and Saddle XOR. The only difference is in '#13 +
   'the formula of XORing of the two cells with Anti-Aliasing. The first one is:'#13#13 +
   'cover = a+b; if(cover > 1) cover = 2.0 - cover;'#13#13 +
   'The second uses the classical "Saddle" formula:'#13#13 +
   'cover = 1.0 - (1.0 - a + a*b) * (1.0 - b + a*b);'#13#13 +
   'The Linear XOR produces more correct intersections and works constistently with the'#13 +
   'scanline rasterizer algorithm. The Saddle XOR works better with semi-transparent polygons.   ' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format ,flip_y );
 app.caption_ ('AGG Example. Scanline Boolean (F1-Help)' );

 if app.init(800 ,600 ,window_resize ) then
  app.run;

 app.Destruct;

END.