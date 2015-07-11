{mac_copy:1.bmp}
{mac_copy:2.bmp}
{mac_copy:3.bmp}
{mac_copy:4.bmp}
{mac_copy:5.bmp}
{mac_copy:6.bmp}
{mac_copy:7.bmp}
{mac_copy:8.bmp}
{mac_copy:9.bmp}
//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 line_patterns ;

uses
 SysUtils ,

 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,

 agg_ctrl ,
 agg_slider_ctrl ,
 agg_bezier_ctrl ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_renderer_outline_aa ,
 agg_renderer_outline_image ,
 agg_rasterizer_scanline_aa ,
 agg_rasterizer_outline_aa ,
 agg_scanline ,
 agg_scanline_p ,
 agg_render_scanlines ,

 agg_pattern_filters_rgba ,
 agg_conv_stroke ,
 agg_conv_transform ,
 agg_conv_clip_polyline ,
 agg_vertex_source ;

{$I agg_mode.inc }
{$I- }
const
 flip_y = true;

 brightness_to_alpha : array[0..256 * 3 - 1 ] of int8u = (

  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 254, 254, 254, 254, 254, 254,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 253, 253,
  253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 252,
  252, 252, 252, 252, 252, 252, 252, 252, 252, 252, 252, 251, 251, 251, 251, 251,
  251, 251, 251, 251, 250, 250, 250, 250, 250, 250, 250, 250, 249, 249, 249, 249,
  249, 249, 249, 248, 248, 248, 248, 248, 248, 248, 247, 247, 247, 247, 247, 246,
  246, 246, 246, 246, 246, 245, 245, 245, 245, 245, 244, 244, 244, 244, 243, 243,
  243, 243, 243, 242, 242, 242, 242, 241, 241, 241, 241, 240, 240, 240, 239, 239,
  239, 239, 238, 238, 238, 238, 237, 237, 237, 236, 236, 236, 235, 235, 235, 234,
  234, 234, 233, 233, 233, 232, 232, 232, 231, 231, 230, 230, 230, 229, 229, 229,
  228, 228, 227, 227, 227, 226, 226, 225, 225, 224, 224, 224, 223, 223, 222, 222,
  221, 221, 220, 220, 219, 219, 219, 218, 218, 217, 217, 216, 216, 215, 214, 214,
  213, 213, 212, 212, 211, 211, 210, 210, 209, 209, 208, 207, 207, 206, 206, 205,
  204, 204, 203, 203, 202, 201, 201, 200, 200, 199, 198, 198, 197, 196, 196, 195,
  194, 194, 193, 192, 192, 191, 190, 190, 189, 188, 188, 187, 186, 186, 185, 184,
  183, 183, 182, 181, 180, 180, 179, 178, 177, 177, 176, 175, 174, 174, 173, 172,
  171, 171, 170, 169, 168, 167, 166, 166, 165, 164, 163, 162, 162, 161, 160, 159,
  158, 157, 156, 156, 155, 154, 153, 152, 151, 150, 149, 148, 148, 147, 146, 145,
  144, 143, 142, 141, 140, 139, 138, 137, 136, 135, 134, 133, 132, 131, 130, 129,
  128, 128, 127, 125, 124, 123, 122, 121, 120, 119, 118, 117, 116, 115, 114, 113,
  112, 111, 110, 109, 108, 107, 106, 105, 104, 102, 101, 100,  99,  98,  97,  96,
   95,  94,  93,  91,  90,  89,  88,  87,  86,  85,  84,  82,  81,  80,  79,  78,
   77,  75,  74,  73,  72,  71,  70,  69,  67,  66,  65,  64,  63,  61,  60,  59,
   58,  57,  56,  54,  53,  52,  51,  50,  48,  47,  46,  45,  44,  42,  41,  40,
   39,  37,  36,  35,  34,  33,  31,  30,  29,  28,  27,  25,  24,  23,  22,  20,
   19,  18,  17,  15,  14,  13,  12,  11,   9,   8,   7,   6,   4,   3,   2,   1 );

type
 pattern_src_brightness_to_alpha_rgba8 = object(pixel_source )
   m_rb : rendering_buffer_ptr;
   m_pf : pixel_formats;

   constructor Construct(rb : rendering_buffer_ptr );

   function  _width : unsigned; virtual;
   function  _height : unsigned; virtual;

   function  pixel(x ,y : int ) : rgba8; virtual;

  end;

 the_application = object(platform_support )
   m_ctrl_color : aggclr;

   m_curve1  ,
   m_curve2  ,
   m_curve3  ,
   m_curve4  ,
   m_curve5  ,
   m_curve6  ,
   m_curve7  ,
   m_curve8  ,
   m_curve9  : bezier_ctrl;
   m_scale_x ,
   m_start_x : slider_ctrl;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure draw_curve(
              patt : line_image_pattern_ptr;
              ras  : rasterizer_outline_aa_ptr;
              ren  : renderer_outline_image_ptr;
              src  : pixel_source_ptr;
              vs   : vertex_source_ptr );

   procedure on_draw; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;
   procedure on_ctrl_change; virtual;

  end;

{ CONSTRUCT }
constructor pattern_src_brightness_to_alpha_rgba8.Construct;
begin
 m_rb:=rb;

 pixfmt_bgr24(m_pf ,m_rb );

end;

{ _WIDTH }
function pattern_src_brightness_to_alpha_rgba8._width;
begin
 result:=m_pf._width;

end;

{ _HEIGHT }
function pattern_src_brightness_to_alpha_rgba8._height;
begin
 result:=m_pf._height;

end;

{ PIXEL }
function pattern_src_brightness_to_alpha_rgba8.pixel;
var
 c : aggclr;

begin
 c  :=m_pf.pixel(@m_pf ,x ,y );
 c.a:=brightness_to_alpha[c.r + c.g + c.b ];

 result.r:=c.r;
 result.g:=c.g;
 result.b:=c.b;
 result.a:=c.a;

end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_ctrl_color.ConstrDbl(0 ,0.3 ,0.5 ,0.3 );

 m_scale_x.Construct(5.0 ,5.0 ,240.0 ,12.0 ,not flip_y_ );
 m_start_x.Construct(250.0 ,5.0 ,495.0 ,12.0 ,not flip_y_ );

 m_curve1.Construct;
 m_curve2.Construct;
 m_curve3.Construct;
 m_curve4.Construct;
 m_curve5.Construct;
 m_curve6.Construct;
 m_curve7.Construct;
 m_curve8.Construct;
 m_curve9.Construct;

 m_curve1.line_color_(@m_ctrl_color );
 m_curve2.line_color_(@m_ctrl_color );
 m_curve3.line_color_(@m_ctrl_color );
 m_curve4.line_color_(@m_ctrl_color );
 m_curve5.line_color_(@m_ctrl_color );
 m_curve6.line_color_(@m_ctrl_color );
 m_curve7.line_color_(@m_ctrl_color );
 m_curve8.line_color_(@m_ctrl_color );
 m_curve9.line_color_(@m_ctrl_color );

 m_curve1.curve_(64  ,19  ,14  ,126 ,118 ,266 ,19  ,265 );
 m_curve2.curve_(112 ,113 ,178 ,32  ,200 ,132 ,125 ,438 );
 m_curve3.curve_(401 ,24  ,326 ,149 ,285 ,11  ,177 ,77 );
 m_curve4.curve_(188 ,427 ,129 ,295 ,19  ,283 ,25  ,410 );
 m_curve5.curve_(451 ,346 ,302 ,218 ,265 ,441 ,459 ,400 );
 m_curve6.curve_(454 ,198 ,14  ,13  ,220 ,291 ,483 ,283 );
 m_curve7.curve_(301 ,398 ,355 ,231 ,209 ,211 ,170 ,353 );
 m_curve8.curve_(484 ,101 ,222 ,33  ,486 ,435 ,487 ,138 );
 m_curve9.curve_(143 ,147 ,11  ,45  ,83  ,427 ,132 ,197 );

 add_ctrl(@m_curve1 );
 add_ctrl(@m_curve2 );
 add_ctrl(@m_curve3 );
 add_ctrl(@m_curve4 );
 add_ctrl(@m_curve5 );
 add_ctrl(@m_curve6 );
 add_ctrl(@m_curve7 );
 add_ctrl(@m_curve8 );
 add_ctrl(@m_curve9 );

 m_curve1.no_transform;
 m_curve2.no_transform;
 m_curve3.no_transform;
 m_curve4.no_transform;
 m_curve5.no_transform;
 m_curve6.no_transform;
 m_curve7.no_transform;
 m_curve8.no_transform;
 m_curve9.no_transform;

 m_scale_x.label_('Scale X=%.2f' );
 m_scale_x.range_(0.2 ,3.0 );
 m_scale_x.value_(1.0 );
 m_scale_x.no_transform;

 add_ctrl(@m_scale_x );

 m_start_x.label_('Start X=%.2f' );
 m_start_x.range_(0.0 ,10.0 );
 m_start_x.value_(0.0 );
 m_start_x.no_transform;

 add_ctrl(@m_start_x );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_scale_x.Destruct;
 m_start_x.Destruct;

 m_curve1.Destruct;
 m_curve2.Destruct;
 m_curve3.Destruct;
 m_curve4.Destruct;
 m_curve5.Destruct;
 m_curve6.Destruct;
 m_curve7.Destruct;
 m_curve8.Destruct;
 m_curve9.Destruct;

end;

{ DRAW_CURVE }
procedure the_application.draw_curve;
begin
 patt.create (src );
 ren.scale_x_(m_scale_x._value );
 ren.start_x_(m_start_x._value );
 ras.add_path(vs );

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pf  : pixel_formats;
 ren : renderer_scanline_aa_solid;
 ras : rasterizer_scanline_aa;
 sl  : scanline_p8;

 rgba : aggclr;

 ren_base : renderer_base;

 p1 ,p2 ,p3 ,p4 ,p5 ,p6 ,p7 ,p8 ,p9 : pattern_src_brightness_to_alpha_rgba8;

 fltr : pattern_filter_bilinear_rgba{};
 patt : line_image_pattern;

 ren_img : renderer_outline_image;
 ras_img : rasterizer_outline_aa;

begin
// Initialize structures
 pixfmt_bgr24(pf ,rbuf_window );

 ren_base.Construct(@pf );
 ren.Construct     (@ren_base );

 rgba.ConstrDbl(1.0 ,1.0 ,0.95 );
 ren_base.clear(@rgba );

 ras.Construct;
 sl.Construct;

// Pattern source. Must have an interface:
// width() const
// height() const
// pixel(int x, int y) const
// Any agg::renderer_base<> or derived
// is good for the use as a source.
 p1.Construct(rbuf_img(0 ) );
 p2.Construct(rbuf_img(1 ) );
 p3.Construct(rbuf_img(2 ) );
 p4.Construct(rbuf_img(3 ) );
 p5.Construct(rbuf_img(4 ) );
 p6.Construct(rbuf_img(5 ) );
 p7.Construct(rbuf_img(6 ) );
 p8.Construct(rbuf_img(7 ) );
 p9.Construct(rbuf_img(8 ) );

 fltr.Construct; // Filtering functor

// agg::line_image_pattern is the main container for the patterns. It creates
// a copy of the patterns extended according to the needs of the filter.
// agg::line_image_pattern can operate with arbitrary image width, but if the 
// width of the pattern is power of 2, it's better to use the modified
// version agg::line_image_pattern_pow2 because it works about 15-25 percent
// faster than agg::line_image_pattern (because of using simple masking instead
// of expensive '%' operation).

//-- Create with specifying the source
// patt.Construct(@fltr ,@src );

//-- Create uninitialized and set the source
 patt.Construct   (@fltr );
 ren_img.Construct(@ren_base ,@patt );
 ras_img.Construct(@ren_img );

 draw_curve(@patt ,@ras_img ,@ren_img ,@p1 ,m_curve1._curve );
 draw_curve(@patt ,@ras_img ,@ren_img ,@p2 ,m_curve2._curve );
 draw_curve(@patt ,@ras_img ,@ren_img ,@p3 ,m_curve3._curve );
 draw_curve(@patt ,@ras_img ,@ren_img ,@p4 ,m_curve4._curve );
 draw_curve(@patt ,@ras_img ,@ren_img ,@p5 ,m_curve5._curve );
 draw_curve(@patt ,@ras_img ,@ren_img ,@p6 ,m_curve6._curve );
 draw_curve(@patt ,@ras_img ,@ren_img ,@p7 ,m_curve7._curve );
 draw_curve(@patt ,@ras_img ,@ren_img ,@p8 ,m_curve8._curve );
 draw_curve(@patt ,@ras_img ,@ren_img ,@p9 ,m_curve9._curve );

// Render the controls
 render_ctrl(@ras ,@sl ,@ren ,@m_curve1 );
 render_ctrl(@ras ,@sl ,@ren ,@m_curve2 );
 render_ctrl(@ras ,@sl ,@ren ,@m_curve3 );
 render_ctrl(@ras ,@sl ,@ren ,@m_curve4 );
 render_ctrl(@ras ,@sl ,@ren ,@m_curve5 );
 render_ctrl(@ras ,@sl ,@ren ,@m_curve6 );
 render_ctrl(@ras ,@sl ,@ren ,@m_curve7 );
 render_ctrl(@ras ,@sl ,@ren ,@m_curve8 );
 render_ctrl(@ras ,@sl ,@ren ,@m_curve9 );

 render_ctrl(@ras ,@sl ,@ren ,@m_scale_x );
 render_ctrl(@ras ,@sl ,@ren ,@m_start_x );

// Free AGG resources
 ras.Destruct;
 sl.Destruct;

 patt.Destruct;
 ras_img.Destruct;

end;

{ ON_KEY }
procedure the_application.on_key;
var
 fd  : text;
 buf : array[0..255 ] of char;

begin
 if key = byte(' ' ) then
  begin
   AssignFile(fd ,'coord' );
   rewrite   (fd );

   sprintf(@buf[0 ]             ,'%.0f, ' ,m_curve1._x1 );
   sprintf(@buf[StrLen(@buf ) ] ,'%.0f, ' ,m_curve1._y1 );
   sprintf(@buf[StrLen(@buf ) ] ,'%.0f, ' ,m_curve1._x2 );
   sprintf(@buf[StrLen(@buf ) ] ,'%.0f, ' ,m_curve1._y2 );
   sprintf(@buf[StrLen(@buf ) ] ,'%.0f, ' ,m_curve1._x3 );
   sprintf(@buf[StrLen(@buf ) ] ,'%.0f, ' ,m_curve1._y3 );
   sprintf(@buf[StrLen(@buf ) ] ,'%.0f, ' ,m_curve1._x4 );
   sprintf(@buf[StrLen(@buf ) ] ,'%.0f'   ,m_curve1._y4 );

   write(fd ,PChar(@buf[0 ] ) );
   close(fd );

  end;

 if key = key_f1 then
  message_(
   'The demo shows a very powerful mechanism of using arbitrary images as line patterns. '#13 +
   'The main point of it is that the images are drawn along the path. It allows you to '#13 +
   'draw very fancy looking lines quite easily and very useful in GIS/cartography applications. '#13 +
   'There the bilinear filtering is used, but it''s also possible to add any other filtering '#13 +
   'methods, or just use the nearest neighbour one for the sake of speed. '#13 +
   'Actually, the algorithm uses 32bit images with alpha channel, but in this demo alpha is '#13 +
   'simulated in such a way that wite is transparent, black is opaque. The intermediate colors '#13 +
   'have intermediate opacity that is defined by the brightness_to_alpha array.'#13#13 +
   'How to play with:'#13#13 +
   'In the demo you can drag the control points of the curves and observe that the images '#13 +
   'are transformed quite consistently and smoothly. You can also try to replace the image '#13 +
   'files (1…9) with your own. The BMP files must have 24bit colors (TrueColor), the PPM '#13 +
   'ones must be of type "P6". Also, the heigh should not exceed 64 pixels, and the background '#13 +
   'should be white or very close to white.'#13 +
   'Press the spacebar to write down the "coord" file of the curve 1 (of 1.bmp).' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

{ ON_CTRL_CHANGE }
procedure the_application.on_ctrl_change;
begin
end;

VAR
 app : the_application;
 buf : array [0..255 ] of char;
 ext : string[10 ];

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. Drawing Lines with Image Patterns (F1-Help)' );

 if not app.load_img(0 ,'1' ) or
    not app.load_img(1 ,'2' ) or
    not app.load_img(2 ,'3' ) or
    not app.load_img(3 ,'4' ) or
    not app.load_img(4 ,'5' ) or
    not app.load_img(5 ,'6' ) or
    not app.load_img(6 ,'7' ) or
    not app.load_img(7 ,'8' ) or
    not app.load_img(8 ,'9' ) then
  begin
   ext:=app._img_ext;

   sprintf(@buf[0 ] ,'There must be files 1%s' ,ptrcomp(@ext[1 ] ) );
   sprintf(
    @buf[StrLen(@buf ) ] ,
    '...9%s'#13 +
    'Download and unzip:'#13 +
    'http://www.antigrain.com/line_patterns.bmp.zip'#13 +
    'or'#13 +
    'http://www.antigrain.com/line_patterns.ppm.tar.gz'#13 ,
    ptrcomp(@ext[1 ] ) );

   app.message_(@buf[0 ] );

  end
 else
  if app.init(500 ,450 ,window_resize ) then
   app.run;

 app.Destruct;

END.