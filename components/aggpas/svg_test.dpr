{mac_copy:tiger.svg}
//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
{$mode delphi}
program
 svg_test ;

uses
 SysUtils ,

 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgba ,

 agg_ctrl ,
 agg_slider_ctrl ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_p ,
 agg_render_scanlines ,

 agg_trans_affine ,
 agg_gamma_functions ,
 agg_gsv_text ,
 agg_conv_stroke ,

 agg_svg_parser ,
 agg_svg_path_renderer ,
 agg_svg_exception ,
 file_utils_, AggPasLCL ;

{$I src/agg_mode.inc }

const
 flip_y = false;

type
 the_application = object(platform_support )
   m_path : path_renderer;

   m_expand ,
   m_gamma  ,
   m_scale  ,
   m_rotate : slider_ctrl;

   m_min_x ,
   m_min_y ,
   m_max_x ,
   m_max_y ,

   m_x  ,
   m_y  ,
   m_dx ,
   m_dy : double;

   m_drag_flag : boolean;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure parse_svg(fname : shortstring );

   procedure on_resize(sx ,sy : int ); virtual;
   procedure on_draw; virtual;

   procedure on_mouse_move(x ,y : int; flags : unsigned ); virtual;

   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_path.Construct;

 m_expand.Construct(5       ,5      ,256 - 5 ,11      ,not flip_y_ );
 m_gamma.Construct (5       ,5 + 15 ,256 - 5 ,11 + 15 ,not flip_y_ );
 m_scale.Construct (256 + 5 ,5      ,512 - 5 ,11      ,not flip_y_ );
 m_rotate.Construct(256 + 5 ,5 + 15 ,512 - 5 ,11 + 15 ,not flip_y_ );

 m_min_x:=0.0;
 m_min_y:=0.0;
 m_max_x:=0.0;
 m_max_y:=0.0;

 m_x :=0.0;
 m_y :=0.0;
 m_dx:=0.0;
 m_dy:=0.0;

 m_drag_flag:=false;

 add_ctrl(@m_expand );
 add_ctrl(@m_gamma );
 add_ctrl(@m_scale );
 add_ctrl(@m_rotate );

 m_expand.label_('Expand=%3.2f' );
 m_expand.range_(-1 ,1.2 );
 m_expand.value_(0.0 );

 m_gamma.label_('Gamma=%3.2f' );
 m_gamma.range_(0.0 ,3.0 );
 m_gamma.value_(1.0 );

 m_scale.label_('Scale=%3.2f' );
 m_scale.range_(0.2 ,10.0 );
 m_scale.value_(1.0 );

 m_rotate.label_('Rotate=%3.2f' );
 m_rotate.range_(-180.0 ,180.0 );
 m_rotate.value_(0.0 );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_path.Destruct;

 m_expand.Destruct;
 m_gamma.Destruct;
 m_scale.Destruct;
 m_rotate.Destruct;

end;

{ PARSE_SVG }
procedure the_application.parse_svg;
var
 p : parser;

begin
 p.Construct(@m_path );

 try
  p.parse(fname );

 except
  p.Destruct;
  raise;

 end;

 m_path.arrange_orientations;
 m_path.bounding_rect(@m_min_x ,@m_min_y ,@m_max_x ,@m_max_y );

 caption_(StrPas(PChar(p.title ) ) + ' (F1-Help)' );

 p.Destruct;

end;

{ ON_RESIZE }
procedure the_application.on_resize;
begin
end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;

 rb  : renderer_base;
 ren : renderer_scanline_aa_solid;
 ras : rasterizer_scanline_aa;
 sl  : scanline_p8;
 mtx : trans_affine;

 rgba : aggclr;
 gmpw : gamma_power;
 gmno : gamma_none;

 tat : trans_affine_translation;
 tas : trans_affine_scaling;
 tar : trans_affine_rotation;

 tm : double;

 vertex_count : unsigned;

 buf : array[0..127 ] of char;
 t   : gsv_text;
 pt  : conv_stroke;

begin
// Initialize structures
 pixfmt_bgra32(pixf ,rbuf_window );

 rb.Construct (@pixf );
 ren.Construct(@rb );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear      (@rgba );

 ras.Construct;
 sl.Construct;
 mtx.Construct;

// Render
 gmpw.Construct(m_gamma._value );
 ras.gamma     (@gmpw );

 tat.Construct((m_min_x + m_max_x ) * -0.5 ,(m_min_y + m_max_y ) * -0.5 );
 mtx.multiply (@tat );

 tas.Construct(m_scale._value );
 mtx.multiply (@tas );

 tar.Construct(deg2rad(m_rotate._value ) );
 mtx.multiply (@tar );

 tat.Construct((m_min_x + m_max_x ) * 0.5 + m_x ,(m_min_y + m_max_y ) * 0.5 + m_y + 30 );
 mtx.multiply (@tat );

 m_path.expand(m_expand._value );

 start_timer;

 m_path.render(@ras ,@sl ,@ren ,@mtx ,rb._clip_box ,1.0 );

 tm:=elapsed_time;

 vertex_count:=m_path.vertex_count;

// Render the controls
 gmno.Construct;
 ras.gamma(@gmno );

 render_ctrl(@ras ,@sl ,@ren ,@m_expand );
 render_ctrl(@ras ,@sl ,@ren ,@m_gamma );
 render_ctrl(@ras ,@sl ,@ren ,@m_scale );
 render_ctrl(@ras ,@sl ,@ren ,@m_rotate );

// Display text
 t.Construct;
 t.size_(10.0 );
 t.flip_(true );

 pt.Construct(@t );
 pt.width_   (1.5 );

 sprintf(@buf[0 ]                 ,'Vertices=%d ' ,vertex_count );
 sprintf(@buf[StrLen(@buf[0 ] ) ] ,'Time=%.3f ms' ,tm );

 t.start_point_(10.0 ,40.0 );
 t.text_       (@buf[0 ] );

 ras.add_path    (@pt );
 rgba.ConstrDbl  (0 ,0 ,0 );
 ren.color_      (@rgba );
 render_scanlines(@ras ,@sl ,@ren );

// Free AGG resources
 ras.Destruct;
 sl.Destruct;

 t.Destruct;
 pt.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 if flags = 0 then
  m_drag_flag:=false;

 if m_drag_flag then
  begin
   m_x:=x - m_dx;
   m_y:=y - m_dy;

   force_redraw;

  end;

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
begin
 m_dx:=x - m_x;
 m_dy:=y - m_y;

 m_drag_flag:=true;

end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
 m_drag_flag:=false;

end;

{ ON_KEY }
procedure the_application.on_key;
var
 mtx : trans_affine;
 tat : trans_affine_translation;
 tas : trans_affine_scaling;
 tar : trans_affine_rotation;

 m   : array[0..5 ] of double;
 buf : array[0..127 ] of char;
 fd  : text;

begin
 if key = byte(' ' ) then
  begin
   mtx.Construct;

   tat.Construct((m_min_x + m_max_x ) * -0.5 ,(m_min_y + m_max_y ) * -0.5 );
   mtx.multiply (@tat );

   tas.Construct(m_scale._value );
   mtx.multiply (@tas );

   tar.Construct(deg2rad(m_rotate._value ) );
   mtx.multiply (@tar );

   tat.Construct((m_min_x + m_max_x ) * 0.5 ,(m_min_y + m_max_y ) * 0.5 );
   mtx.multiply (@tat );

   tat.Construct(m_x ,m_y );
   mtx.multiply (@tat );

   mtx.store_to(@m );

   sprintf(@buf[0 ]                 ,'%3.3f, ' ,m[0 ] );
   sprintf(@buf[StrLen(@buf[0 ] ) ] ,'%3.3f, ' ,m[1 ] );
   sprintf(@buf[StrLen(@buf[0 ] ) ] ,'%3.3f, ' ,m[2 ] );
   sprintf(@buf[StrLen(@buf[0 ] ) ] ,'%3.3f, ' ,m[3 ] );
   sprintf(@buf[StrLen(@buf[0 ] ) ] ,'%3.3f, ' ,m[4 ] );
   sprintf(@buf[StrLen(@buf[0 ] ) ] ,'%3.3f'   ,m[5 ] );

   message_(@buf[0 ] );

  {$I- }
   AssignFile(fd ,'transform.txt' );
   rewrite   (fd );
   writeln   (fd ,PChar(@buf[0 ] ) );
   close     (fd );

  end;

 if key = key_f1 then
  message_(
   'The SVG viewer is just another example of using Anti-Grain Geometry. The viewer '#13 +
   'supports absolute minimum of the SVG specification, it basically can be used as '#13 +
   'a simple example of AGG plus SVG. But of course, its functionality can be extended. '#13 +
   'The main point of the viewer is high quality and high performance. The Anti-Aliasing '#13 +
   'algorithm produces 256 levels of transparency. Actually, AGG computes the exact '#13 +
   'coverage of the outline on each pixel cell.'#13#13 +

   'Besides, the viewer has a very nice feature that I haven''t seen in any other ones. '#13 +
   'It''s eliminating of the "problem of adjacent edges". It appears when rendering '#13 +
   'adjacent polygons with anti-aliasing and looks like thin "web" upon the image. '#13 +
   'Strictly speaking, it''s possible to get rid of it completely only when the polygons '#13 +
   'are fully opaque. When they are translucent, the effect will appear anyway. However, '#13 +
   'it''s possible to reduce the effect so that it becomes almost invisible.'#13#13 +
   'How to play with:'#13#13 +
   'Use mouse to move the drawing around. Change size & rotation with top controls.'#13 +
   'Press the spacebar key to display and save the current transformation matrix.' );

end;

VAR
 app   : the_application;
 fname : shortstring;
 af    : api_file;

label
 Esc ;

BEGIN
 app.Construct(pix_format_bgra32 ,flip_y );

 fname:=app.file_source('svg' ,'tiger.svg' );

 if param_count > 0 then
  fname:=param_str(1 )

 else
  begin
   if api_open_file(af ,fname ) then
    api_close_file(af )
   else
    begin
     app.message_('Usage: svg_test <svg_file>' );

     goto Esc;	
	
    end;

  end;	

 try
  app.parse_svg(fname );

  if app.init(512 ,600 ,window_resize ) then
   app.run;

 except
  on e:svg_exception do
   begin
    app.message_(PChar(e._msg ) );
    e.Free;

   end;

 else
  app.message_('Unknown exception' );

 end;

Esc:
 app.Destruct;

END.
