//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 gamma_correction ;

{$DEFINE AGG_BGR24 }
{DEFINE AGG_RGB24 }
{DEFINE AGG_RGB565 }
{DEFINE AGG_RGB555 }

uses
 agg_basics ,
 agg_platform_support ,

 agg_ctrl ,
 agg_slider_ctrl ,

 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_u ,

 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_render_scanlines ,

 agg_gamma_lut ,
 agg_gamma_functions ,
 agg_path_storage ,
 agg_conv_stroke ,
 agg_ellipse

{$I pixel_formats.inc } 
{$I agg_mode.inc }

const
 flip_y = true;

type
 the_application = object(platform_support )
   m_thickness ,
   m_gamma     ,
   m_contrast  : slider_ctrl;

   m_rx ,
   m_ry : double;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_thickness.Construct(5 ,5 ,400 - 5 ,11 ,not flip_y_ );
 m_gamma.Construct    (5 ,5 + 15 ,400 - 5 ,11 + 15 ,not flip_y_ );
 m_contrast.Construct (5 ,5 + 30 ,400 - 5 ,11 + 30 ,not flip_y_ );

 add_ctrl(@m_thickness );
 add_ctrl(@m_gamma );
 add_ctrl(@m_contrast );

 m_thickness.label_('Thickness=%3.2f' );
 m_gamma.label_    ('Gamma=%3.2f' );
 m_contrast.label_ ('Contrast' );

 m_thickness.range_(0.0 ,3.0 );
 m_gamma.range_    (0.5 ,3.0 );
 m_contrast.range_ (0.0 ,1.0 );

 m_thickness.value_(1.0 );
 m_gamma.value_    (1.0 );
 m_contrast.value_ (1.0 );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_thickness.Destruct;
 m_gamma.Destruct;
 m_contrast.Destruct;

end;

{ ON_INIT }
procedure the_application.on_init;
begin
 m_rx:=_width / 3.0;
 m_ry:=_height / 3.0;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 g ,dark ,light ,x ,y ,v ,gval ,dy : double;

 i : unsigned;

 gamma : gamma_lut;
 pixf  : pixel_formats;

 renb : renderer_base;
 ren  : renderer_scanline_aa_solid;

 ras : rasterizer_scanline_aa;
 sl  : scanline_u8;

 rgba : aggclr;
 path : path_storage;

 gm_pw : gamma_power;
 gpoly ,
 poly  : conv_stroke;
 ell   : ellipse;

begin
// Initialize structures
 g:=m_gamma._value;

 gamma.Construct(g );

 pixfmt_gamma(pixf ,rbuf_window ,@gamma );

 renb.Construct(@pixf );
 ren.Construct (@renb );

 rgba.ConstrDbl(1 ,1 ,1 );
 renb.clear    (@rgba );

 dark :=1.0 - m_contrast._value;
 light:=m_contrast._value;

 rgba.ConstrDbl(dark ,dark ,dark );
 renb.copy_bar (0 ,0 ,trunc(_width ) div 2 ,trunc(_height ) ,@rgba );

 rgba.ConstrDbl(light ,light ,light );
 renb.copy_bar (trunc(_width ) div 2 + 1 ,0 ,trunc(_width ) ,trunc(_height ) ,@rgba );

 rgba.ConstrDbl(1.0 ,dark ,dark );
 renb.copy_bar (0 ,trunc(_height ) div 2 + 1 ,trunc(_width ) ,trunc(_height ) ,@rgba );

 ras.Construct;
 sl.Construct;

// Graph line
 path.Construct;

 x:=(_width - 256.0 ) / 2.0;
 y:=50.0;

 for i:=0 to 255 do
  begin
   v:=i / 255.0;

   gm_pw.Construct(g );

   gval:=gm_pw.func_operator_gamma(v );
   dy  :=gval * 255.0;

   if i = 0 then
    path.move_to(x + i ,y + dy )
   else
    path.line_to(x + i ,y + dy );

  end;

 gpoly.Construct(@path );
 gpoly.width_   (2.0 );

 ras.reset;
 ras.add_path(@gpoly );

 rgba.ConstrInt(80 ,127 ,80 );
 ren.color_    (@rgba );

 render_scanlines(@ras ,@sl ,@ren );

// Ellipses
 ell.Construct (_width / 2 ,_height / 2 ,m_rx ,m_ry ,150 );
 poly.Construct(@ell );
 poly.width_   (m_thickness._value );

 ras.reset;
 ras.add_path(@poly );

 rgba.ConstrInt(255 ,0 ,0 );
 ren.color_    (@rgba );

 render_scanlines(@ras ,@sl ,@ren );


 ell.init(_width / 2 ,_height / 2 ,m_rx - 5.0 ,m_ry - 5.0 ,150 );

 ras.reset;
 ras.add_path(@poly );

 rgba.ConstrInt(0 ,255 ,0 );
 ren.color_    (@rgba );

 render_scanlines(@ras ,@sl ,@ren );


 ell.init(_width / 2 ,_height / 2 ,m_rx - 10.0 ,m_ry - 10.0 ,150 );

 ras.reset;
 ras.add_path(@poly );

 rgba.ConstrInt(0 ,0 ,255 );
 ren.color_    (@rgba );

 render_scanlines(@ras ,@sl ,@ren );


 ell.init(_width / 2 ,_height / 2 ,m_rx - 15.0 ,m_ry - 15.0 ,150 );

 ras.reset;
 ras.add_path(@poly );

 rgba.ConstrInt(0 ,0 ,0 );
 ren.color_    (@rgba );

 render_scanlines(@ras ,@sl ,@ren );


 ell.init(_width / 2 ,_height / 2 ,m_rx - 20.0 ,m_ry - 20.0 ,150 );

 ras.reset;
 ras.add_path(@poly );

 rgba.ConstrInt(255 ,255 ,255 );
 ren.color_    (@rgba );

 render_scanlines(@ras ,@sl ,@ren );

// Render the controls
 render_ctrl(@ras ,@sl ,@ren ,@m_thickness );
 render_ctrl(@ras ,@sl ,@ren ,@m_gamma );
 render_ctrl(@ras ,@sl ,@ren ,@m_contrast );

// Free AGG resources
 ras.Destruct;
 sl.Destruct;
 path.Destruct;

 gamma.Destruct;
 gpoly.Destruct;
 poly.Destruct;

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
   m_rx:=Abs(_width / 2 - x );
   m_ry:=Abs(_height / 2 - y );

   force_redraw;

  end;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'Anti-Aliasing is very tricky because everything depends. Particularly, having '#13 +
   'straight linear dependence "pixel coverage" -> "brightness" may be not the best. '#13 +
   'It depends on the type of display (CRT, LCD), contrast, black-on-white vs '#13 +
   'white-on-black, it even depends on your personal vision. There are no linear '#13 +
   'dependencies in this World. This example demonstrates the importance of so called '#13 +
   'Gamma Correction in Anti-Aliasing. There a traditional power function is used, '#13 +
   'in terms of C++ it''s brighness = pow(brighness, gamma). Note, that if you improve '#13 +
   'the quality on the white side, it becomes worse on the black side and vice versa.'#13 +
   #13#13 +
   'How to play with:'#13#13 +
   'Change "Gamma" and see how the quality changes.'#13 +
   'Use the left mouse button to resize the circles.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format ,flip_y );
 app.caption_ ('AGG Example. Thin red ellipse (F1-Help)' );

 if app.init(400 ,320 ,0 ) then
  app.run;

 app.Destruct;

END.