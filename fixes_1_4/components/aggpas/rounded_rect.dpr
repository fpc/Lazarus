//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 rounded_rect ;

uses
 agg_basics ,
 agg_platform_support ,
 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,

 agg_ctrl ,
 agg_slider_ctrl ,
 agg_cbox_ctrl ,

 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_rasterizer_scanline_aa ,
 agg_scanline_p ,
 agg_render_scanlines ,

 agg_gamma_functions ,
 agg_gamma_lut ,
 agg_ellipse ,
 agg_rounded_rect ,
 agg_conv_stroke ,
 agg_vertex_source ;

{$I agg_mode.inc }

const
 flip_y = true;

type
 the_application = object(platform_support )
   m_x ,
   m_y : array[0..1 ] of double;

   m_dx ,
   m_dy : double;

   m_idx : int;

   m_radius ,
   m_gamma  ,
   m_offset : slider_ctrl;

   m_white_on_black : cbox_ctrl;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
var
 rgba8 : aggclr;

begin
 inherited Construct(format_ ,flip_y_ );

 m_radius.Construct(10 ,10 ,600 - 10 ,19 ,not flip_y_ );
 m_gamma.Construct (10 ,10 + 20 ,600 - 10 ,19 + 20 ,not flip_y_ );
 m_offset.Construct(10 ,10 + 40 ,600 - 10 ,19 + 40 ,not flip_y_ );

 m_white_on_black.Construct(10 ,10 + 60 ,'White on black' );

 m_idx:=-1;

 m_x[0 ]:=100; m_y[0 ]:=100;
 m_x[1 ]:=500; m_y[1 ]:=350;

 add_ctrl(@m_radius );
 add_ctrl(@m_gamma );
 add_ctrl(@m_offset );
 add_ctrl(@m_white_on_black );

 m_gamma.label_('gamma=%4.3f' );
 m_gamma.range_(0.0 ,3.0 );
 m_gamma.value_(1.8 );

 m_radius.label_('radius=%4.3f' );
 m_radius.range_(0.0 ,50.0 );
 m_radius.value_(25.0 );

 m_offset.label_('subpixel offset=%4.3f' );
 m_offset.range_(-2.0 ,3.0 );

 rgba8.ConstrInt(127 ,127 ,127 );

 m_white_on_black.text_color_    (@rgba8 );
 m_white_on_black.inactive_color_(@rgba8 );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_radius.Destruct;
 m_gamma.Destruct;
 m_offset.Destruct;

 m_white_on_black.Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;

 rb  : renderer_base;
 ren : renderer_scanline_aa_solid;
 ras : rasterizer_scanline_aa;
 sl  : scanline_p8;

 rgba  : aggclr;
 gamma : gamma_lut;
 gm_no : vertex_source;

 e : ellipse;
 r : agg_rounded_rect.rounded_rect;
 p : conv_stroke;
 d : double;

begin
// Initialize structures
 gamma.Construct(m_gamma._value ,8 ,8 );

 pixfmt_bgr24_gamma(pixf ,rbuf_window ,@gamma );

 rb.Construct (@pixf );
 ren.Construct(@rb );

 if m_white_on_black._status then
  rgba.ConstrDbl(0 ,0 ,0 )
 else
  rgba.ConstrDbl(1 ,1 ,1 );

 rb.clear(@rgba );

 ras.Construct;
 sl.Construct;

// Render two "control" circles
 e.Construct;
 rgba.ConstrInt(127 ,127 ,127 );
 ren.color_    (@rgba );

 e.init      (m_x[0 ] ,m_y[0 ] ,3 ,3 ,16 );
 ras.add_path(@e );

 render_scanlines(@ras ,@sl ,@ren );

 e.init      (m_x[1 ] ,m_y[1 ] ,3 ,3 ,16 );
 ras.add_path(@e );

 render_scanlines(@ras ,@sl ,@ren );

// Creating a rounded rectangle
 d:=m_offset._value;

 r.Construct(m_x[0 ] + d ,m_y[0 ] + d ,m_x[1 ] + d ,m_y[1 ] + d ,m_radius._value );
 r.normalize_radius;

// Drawing as an outline
 p.Construct(@r );
 p.width_   (1.0 );

 ras.add_path(@p );

 if m_white_on_black._status then
  rgba.ConstrDbl(1 ,1 ,1 )
 else
  rgba.ConstrDbl(0 ,0 ,0 );

 ren.color_(@rgba );

 render_scanlines(@ras ,@sl ,@ren );

 gm_no.Construct;
 ras.gamma(@gm_no );

// Render the controls
 render_ctrl(@ras ,@sl ,@ren ,@m_radius );
 render_ctrl(@ras ,@sl ,@ren ,@m_gamma );
 render_ctrl(@ras ,@sl ,@ren ,@m_offset );
 render_ctrl(@ras ,@sl ,@ren ,@m_white_on_black );

// Free AGG resources
 ras.Destruct;
 sl.Destruct;

 gamma.Destruct;
 p.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 if flags and mouse_left <> 0 then
  if m_idx >= 0 then
   begin
    m_x[m_idx ]:=x - m_dx;
    m_y[m_idx ]:=y - m_dy;

    force_redraw;

   end
  else
 else
  on_mouse_button_up(x ,y ,flags );

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
var
 i : unsigned;

begin
 if flags and mouse_left <> 0 then
  for i:=0 to 1 do
   if Sqrt((x - m_x[i ] ) * (x - m_x[i ] ) + (y - m_y[i ] ) * (y - m_y[i ] ) ) < 5.0 then
    begin
     m_dx :=x - m_x[i ];
     m_dy :=y - m_y[i ];
     m_idx:=i;

     break;

    end;

end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
 m_idx:=-1;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'Yet another example dedicated to Gamma Correction. If you have a CRT monitor: '#13 +
   'The rectangle looks bad - the rounded corners are thicker than its side lines. '#13 +
   'First try to drag the "subpixel offset" control — it simply adds some fractional '#13 +
   'value to the coordinates. When dragging you will see that the rectangle is'#13 +
   '"blinking". Then increase "Gamma" to about 1.5. The result will look almost '#13 +
   'perfect — the visual thickness of the rectangle remains the same. That''s good, '#13 +
   'but turn the checkbox "White on black" on — what do we see ? Our rounded '#13 +
   'rectangle looks terrible. Drag the "subpixel offset" slider — it''s blinking as hell. '#13 +
   'Now decrease "Gamma" to about 0.6. What do we see now ? Perfect result ! '#13 +
   'If you use an LCD monitor, the good value of gamma will be closer to 1.0 in both '#13 +
   'cases — black on white or white on black. There''s no perfection in this world, but '#13 +
   'at least you can control Gamma in Anti-Grain Geometry :-)' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. Rounded rectangle with gamma-correction & stuff (F1-Help)' );

 if app.init(600 ,400 ,window_resize ) then
  app.run;

 app.Destruct;

END.