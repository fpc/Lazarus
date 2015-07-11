//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 idea ;

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
 agg_scanline ,
 agg_scanline_p ,

 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_render_scanlines ,

 agg_gamma_functions ,
 agg_path_storage ,
 agg_trans_affine ,
 agg_conv_stroke ,
 agg_conv_transform ,
 agg_vertex_source

{$I pixel_formats.inc }
{$I agg_mode.inc }

const
 flip_y = false;

type
 path_attributes = object
   index : unsigned;

   fill_color   ,
   stroke_color : aggclr;
   stroke_width : double;

   constructor Construct; overload;
   constructor Construct(idx : unsigned; fill ,stroke : aggclr_ptr; width : double ); overload;

  end;

const
 g_poly_bulb : array[0..39 ] of double = (
  -6  ,-67   ,-6 ,-71   ,-7 ,-74   ,-8 ,-76   ,-10 ,-79 ,
  -10 ,-82   ,-9 ,-84   ,-6 ,-86   ,-4 ,-87   ,-2  ,-86 ,
  -1  ,-86   ,1  ,-84   ,2  ,-82   ,2  ,-79   ,0   ,-77 ,
  -2  ,-73   ,-2 ,-71   ,-2 ,-69   ,-3 ,-67   ,-4  ,-65 );

 g_poly_beam1 : array[0..9 ] of double = (
  -14 ,-84   ,-22 ,-85  ,-23 ,-87  ,-22 ,-88  ,-21 ,-88 );

 g_poly_beam2 : array[0..9 ] of double = (
  -10 ,-92   ,-14 ,-96  ,-14 ,-98  ,-12 ,-99  ,-11 ,-97 );

 g_poly_beam3 : array[0..9 ] of double = (
  -1  ,-92   ,-2  ,-98  ,0   ,-100 ,2   ,-100 ,1   ,-98 );

 g_poly_beam4 : array[0..9 ] of double = (
  5   ,-89   ,11  ,-94  ,13  ,-93  ,13  ,-92  ,12  ,-91 );

 g_poly_fig1 : array[0..41 ] of double = (
  1   ,-48   ,-3  ,-54  ,-7  ,-58  ,-12 ,-58  ,-17 ,-55 ,
  -20 ,-52   ,-21 ,-47  ,-20 ,-40  ,-17 ,-33  ,-11 ,-28 ,
  -6  ,-26   ,-2  ,-25  ,2   ,-26  ,4   ,-28  ,5   ,-33 ,
  5   ,-39   ,3   ,-44  ,12  ,-48  ,12  ,-50  ,12  ,-51 ,
  3   ,-46 );

 g_poly_fig2 : array[0..75 ] of double = (
  11  ,-27   ,6   ,-23  ,4   ,-22  ,3   ,-19  ,5   ,-16 ,
  6   ,-15   ,11  ,-17  ,19  ,-23  ,25  ,-30  ,32  ,-38 ,
  32  ,-41   ,32  ,-50  ,30  ,-64  ,32  ,-72  ,32  ,-75 ,
  31  ,-77   ,28  ,-78  ,26  ,-80  ,28  ,-87  ,27  ,-89 ,
  25  ,-88   ,24  ,-79  ,24  ,-76  ,23  ,-75  ,20  ,-76 ,
  17  ,-76   ,17  ,-74  ,19  ,-73  ,22  ,-73  ,24  ,-71 ,
  26  ,-69   ,27  ,-64  ,28  ,-55  ,28  ,-47  ,28  ,-40 ,
  26  ,-38   ,20  ,-33  ,14  ,-30 );

 g_poly_fig3 : array[0..69 ] of double = (
  -6  ,-20   ,-9  ,-21  ,-15 ,-21  ,-20 ,-17  ,-28  ,-8 ,
  -32 ,-1    ,-32 ,1    ,-30 ,6    ,-26 ,8    ,-20  ,10 ,
  -16 ,12    ,-14 ,14   ,-15 ,16   ,-18 ,20   ,-22  ,20 ,
  -25 ,19    ,-27 ,20   ,-26 ,22   ,-23 ,23   ,-18  ,23 ,
  -14 ,22    ,-11 ,20   ,-10 ,17   ,-9  ,14   ,-11  ,11 ,
  -16 ,9     ,-22 ,8    ,-26 ,5    ,-28 ,2    ,-27  ,-2 ,
  -23 ,-8    ,-19 ,-11  ,-12 ,-14  ,-6  ,-15  ,-6   ,-18 );

 g_poly_fig4 : array[0..39 ] of double = (
  11  ,-6    ,8   ,-16  ,5   ,-21  ,-1  ,-23  ,-7   ,-22 ,
  -10 ,-17   ,-9  ,-10  ,-8  ,0    ,-8  ,10   ,-10  ,18  ,
  -11 ,22    ,-10 ,26   ,-7  ,28   ,-3  ,30   ,0    ,31  ,
  5   ,31    ,10  ,27   ,14  ,18   ,14  ,11   ,11   ,2 );

 g_poly_fig5 : array[0..55 ] of double = (
  0   ,22    ,-5  ,21   ,-8  ,22   ,-9  ,26   ,-8   ,49  ,
  -8  ,54    ,-10 ,64   ,-10 ,75   ,-9  ,81   ,-10  ,84  ,
  -16 ,89    ,-18 ,95   ,-18 ,97   ,-13 ,100  ,-12  ,99  ,
  -12 ,95    ,-10 ,90   ,-8  ,87   ,-6  ,86   ,-4   ,83  ,
  -3  ,82    ,-5  ,80   ,-6  ,79   ,-7  ,74   ,-6   ,63  ,
  -3  ,52    ,0   ,42   ,1   ,31 );

 g_poly_fig6 : array[0..61 ] of double = (
  12  ,31    ,12  ,24   ,8   ,21   ,3   ,21   ,2    ,24  ,
  3   ,30    ,5   ,40   ,8   ,47   ,10  ,56   ,11   ,64  ,
  11  ,71    ,10  ,76   ,8   ,77   ,8   ,79   ,10   ,81  ,
  13  ,82    ,17  ,82   ,26  ,84   ,28  ,87   ,32   ,86  ,
  33  ,81    ,32  ,80   ,25  ,79   ,17  ,79   ,14   ,79  ,
  13  ,76    ,14  ,72   ,14  ,64   ,13  ,55   ,12   ,44  ,
  12  ,34 );

var
 g_npaths : unsigned;
 g_pflag  : filling_rule_e;
 g_angle  : double;

 g_attr : array[0..2 ] of path_attributes;
 g_path : path_storage;

 g_rasterizer : rasterizer_scanline_aa;
 g_scanline   : scanline_p8;

//AGG_POLY_SIZE: sizeof(p ) / (sizeof(double ) * 2

type
 the_application = object(platform_support )
   m_dx ,
   m_dy : double;

   m_rotate   ,
   m_even_odd ,
   m_draft    ,
   m_roundoff : cbox_ctrl;

   m_angle_delta : slider_ctrl;
   m_redraw_flag : boolean;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_init; virtual;
   procedure on_resize(sx ,sy : int ); virtual;
   procedure on_draw; virtual;

   procedure on_idle; virtual;
   procedure on_ctrl_change; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ trans_roundoff }
procedure trans_roundoff(this : trans_affine_ptr; x ,y : double_ptr );
begin
 x^:=Floor(x^ + 0.5 );
 y^:=Floor(y^ + 0.5 );

end;

{ CONSTRUCT }
constructor path_attributes.Construct;
begin
 fill_color.Construct;
 stroke_color.Construct;

 index       :=0;
 stroke_width:=0;

end;

{ CONSTRUCT }
constructor path_attributes.Construct(idx : unsigned; fill ,stroke : aggclr_ptr; width : double );
begin
 index       :=idx;
 fill_color  :=fill^;
 stroke_color:=stroke^;
 stroke_width:=width;

end;

{ CONSTRUCT }
constructor the_application.Construct;
var
 rgbs ,
 rgbf : aggclr;

begin
 inherited Construct(format_ ,flip_y_ );

// Controls
 m_rotate.Construct  (10  ,3 ,'Rotate' ,not flip_y_ );
 m_even_odd.Construct(60  ,3 ,'Even-Odd' ,not flip_y_ );
 m_draft.Construct   (130 ,3 ,'Draft' ,not flip_y_ );
 m_roundoff.Construct(175 ,3 ,'Roundoff' ,not flip_y_ );

 m_angle_delta.Construct(10 ,21 ,250-10 ,27 ,not flip_y_ );
 m_angle_delta.label_   ('Step=%4.3f degree' );

 m_redraw_flag:=true;

 m_rotate.text_size_  (7 );
 m_even_odd.text_size_(7 );
 m_draft.text_size_   (7 );
 m_roundoff.text_size_(7 );

 add_ctrl(@m_rotate );
 add_ctrl(@m_even_odd );
 add_ctrl(@m_draft );
 add_ctrl(@m_roundoff );
 add_ctrl(@m_angle_delta );

 m_angle_delta.value_(0.01 );

// Polygon
 rgbf.ConstrInt(255 ,255 ,0 );
 rgbs.ConstrInt(0   ,0   ,0 );

 g_attr[g_npaths ].Construct(
  g_path.start_new_path ,
  @rgbf ,@rgbs ,1.0 );

 inc(g_npaths );

 g_path.add_poly(@g_poly_bulb[0 ] ,sizeof(g_poly_bulb ) div (sizeof(double ) * 2 ) ,false ,path_flags_close );

 rgbf.ConstrInt(255 ,255 ,200 );
 rgbs.ConstrInt(90  ,0   ,0   );

 g_attr[g_npaths ].Construct(
  g_path.start_new_path ,
  @rgbf ,@rgbs ,0.7 );

 inc(g_npaths );

 g_path.add_poly(@g_poly_beam1[0 ] ,sizeof(g_poly_beam1 ) div (sizeof(double ) * 2 ) ,false ,path_flags_close );
 g_path.add_poly(@g_poly_beam2[0 ] ,sizeof(g_poly_beam2 ) div (sizeof(double ) * 2 ) ,false ,path_flags_close );
 g_path.add_poly(@g_poly_beam3[0 ] ,sizeof(g_poly_beam3 ) div (sizeof(double ) * 2 ) ,false ,path_flags_close );
 g_path.add_poly(@g_poly_beam4[0 ] ,sizeof(g_poly_beam4 ) div (sizeof(double ) * 2 ) ,false ,path_flags_close );

 rgbf.ConstrInt(0 ,0 ,0 );
 rgbs.ConstrInt(0 ,0 ,0 );

 g_attr[g_npaths ].Construct(
  g_path.start_new_path ,
  @rgbf ,@rgbs ,0.0 );

 inc(g_npaths );

 g_path.add_poly(@g_poly_fig1[0 ] ,sizeof(g_poly_fig1 ) div (sizeof(double ) * 2 ) );
 g_path.add_poly(@g_poly_fig2[0 ] ,sizeof(g_poly_fig2 ) div (sizeof(double ) * 2 ) );
 g_path.add_poly(@g_poly_fig3[0 ] ,sizeof(g_poly_fig3 ) div (sizeof(double ) * 2 ) );
 g_path.add_poly(@g_poly_fig4[0 ] ,sizeof(g_poly_fig4 ) div (sizeof(double ) * 2 ) );
 g_path.add_poly(@g_poly_fig5[0 ] ,sizeof(g_poly_fig5 ) div (sizeof(double ) * 2 ) );
 g_path.add_poly(@g_poly_fig6[0 ] ,sizeof(g_poly_fig6 ) div (sizeof(double ) * 2 ) );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_rotate.Destruct;
 m_even_odd.Destruct;
 m_draft.Destruct;
 m_roundoff.Destruct;

 m_angle_delta.Destruct;

end;

{ ON_INIT }
procedure the_application.on_init;
begin
 m_dx:=rbuf_window._width;
 m_dy:=rbuf_window._height;
 
end;

{ ON_RESIZE }
procedure the_application.on_resize;
begin
 m_redraw_flag:=true;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;

 rbase : renderer_base;

 r  : renderer_scanline_aa_solid;
 rb : renderer_scanline_bin_solid;

 rgba : aggclr;

 gm_no : vertex_source;
 gm_th : gamma_threshold;

 mtx : trans_affine;
 tar : trans_affine_rotation;
 tat : trans_affine_translation;
 tas : trans_affine_scaling;

 roundoff : trans_affine;

 fill ,fill_roundoff : conv_transform;

 stroke ,stroke_roundoff : conv_stroke;

 i : unsigned;

begin
// Initialize structures
 pixfmt(pixf ,rbuf_window );

 rbase.Construct(@pixf );
 r.Construct    (@rbase );
 rb.Construct   (@rbase );

 roundoff.Construct(@trans_roundoff );

// Render the controls
 if m_redraw_flag then
  begin
   gm_no.Construct;
   g_rasterizer.gamma(@gm_no );

   rgba.ConstrInt(255 ,255 ,255 );
   rbase.clear   (@rgba );

   g_rasterizer.filling_rule(fill_non_zero );

   render_ctrl(@g_rasterizer ,@g_scanline ,@r ,@m_rotate );
   render_ctrl(@g_rasterizer ,@g_scanline ,@r ,@m_even_odd );
   render_ctrl(@g_rasterizer ,@g_scanline ,@r ,@m_draft );
   render_ctrl(@g_rasterizer ,@g_scanline ,@r ,@m_roundoff );
   render_ctrl(@g_rasterizer ,@g_scanline ,@r ,@m_angle_delta );

   m_redraw_flag:=false;

  end
 else
  begin
   rgba.ConstrInt(255 ,255 ,255 );
   rbase.copy_bar(
    0 ,trunc(32.0 * rbuf_window._height / m_dy ) ,
    rbuf_window._width ,rbuf_window._height ,
    @rgba );

  end;


// Draft mode
 if m_draft._status then
  begin
   gm_th.Construct   (0.4 );
   g_rasterizer.gamma(@gm_th );

  end;

// Rotate polygon
 tar.Construct(g_angle * pi / 180.0 );
 tat.Construct(m_dx / 2 ,m_dy / 2 + 10 );
 tas.Construct(rbuf_window._width / m_dx ,rbuf_window._height / m_dy );

 mtx.Construct;
 mtx.reset;
 mtx.multiply(@tar );
 mtx.multiply(@tat );
 mtx.multiply(@tas );

 fill.Construct         (@g_path ,@mtx );
 fill_roundoff.Construct(@fill ,@roundoff );

 stroke.Construct         (@fill );
 stroke_roundoff.Construct(@fill_roundoff );

 if m_even_odd._status then
  g_pflag:=fill_even_odd
 else
  g_pflag:=fill_non_zero;

// Render polygon
 for i:=0 to g_npaths - 1 do
  begin
   g_rasterizer.filling_rule(g_pflag );

   r.color_ (@g_attr[i ].fill_color );
   rb.color_(@g_attr[i ].fill_color );

   if m_roundoff._status then
    g_rasterizer.add_path(@fill_roundoff ,g_attr[i ].index )
   else
    g_rasterizer.add_path(@fill ,g_attr[i ].index );

   if m_draft._status then
    render_scanlines(@g_rasterizer ,@g_scanline ,@rb )
   else
    render_scanlines(@g_rasterizer ,@g_scanline ,@r );

   if g_attr[i ].stroke_width > 0.001 then
    begin
     r.color_ (@g_attr[i ].stroke_color );
     rb.color_(@g_attr[i ].stroke_color );

     stroke.width_         (g_attr[i ].stroke_width * mtx.scale );
     stroke_roundoff.width_(g_attr[i ].stroke_width * mtx.scale );

     if m_roundoff._status then
      g_rasterizer.add_path(@stroke_roundoff ,g_attr[i ].index )
     else
      g_rasterizer.add_path(@stroke ,g_attr[i ].index );

     if m_draft._status then
      render_scanlines(@g_rasterizer ,@g_scanline ,@rb )
     else
      render_scanlines(@g_rasterizer ,@g_scanline ,@r );

    end;

  end;

// Free AGG resources
 stroke.Destruct;
 stroke_roundoff.Destruct;

end;

{ ON_IDLE }
procedure the_application.on_idle;
begin
 g_angle:=g_angle + m_angle_delta._value;

 if g_angle > 360.0 then
  g_angle:=g_angle - 360.0;

 force_redraw;

end;

{ ON_CTRL_CHANGE }
procedure the_application.on_ctrl_change;
begin
 wait_mode_(not m_rotate._status );

 m_redraw_flag:=true;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'The polygons for this "idea" were taken from the book "Dynamic HTML in Action" '#13 +
   'by Eric Schurman. An example of using Microsoft Direct Animation can be found here: '#13 +
   '"http://www.antigrain.com/demo/ideaDA.html." If you use Microsoft Internet Explorer '#13 +
   'you can compare the quality of rendering in AGG and Microsoft Direct Animation. '#13 +
   'Note that even when you click "Rotate with High Quality", you will see it "jitters". '#13 +
   'It''s because there are actually no Subpixel Accuracy used in the Microsoft Direct '#13 +
   'Animation.In the AGG example, there''s no jitter even in the "Draft" (low quality) '#13 +
   'mode. You can see the simulated jittering if you turn on the "Roundoff" mode, in which '#13 +
   'there integer pixel coordinated are used. As for the performance, note, that the image '#13 +
   'in AGG is rotated with step of 0.01 degree (initially), while in the Direct Animation '#13 +
   'Example the angle step is 0.1 degree.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 g_npaths:=0;
 g_pflag :=fill_non_zero;
 g_angle :=0.0;

 g_path.Construct;
 g_rasterizer.Construct;
 g_scanline.Construct;

 app.Construct(pix_format ,flip_y );
 app.caption_ ('AGG Example. Idea (F1-Help)' );

 if app.init(250 ,280 ,window_resize ) then
  app.run;

 app.Destruct;

 g_rasterizer.Destruct;
 g_scanline.Destruct;
 g_path.Destruct;

END.