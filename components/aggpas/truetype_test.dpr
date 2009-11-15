{target:win}
//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 truetype_test ;

uses
 Windows ,SysUtils ,

 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,

 agg_ctrl ,
 agg_slider_ctrl ,
 agg_cbox_ctrl ,
 agg_rbox_ctrl ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_renderer_primitives ,
 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_u ,
 agg_scanline_bin ,
 agg_render_scanlines ,

 agg_trans_affine ,
 agg_curves ,
 agg_conv_curve ,
 agg_conv_contour ,
 agg_gamma_lut ,
 agg_gamma_functions ,
 agg_font_win32_tt ,
 agg_font_cache_manager ;

{$I agg_mode.inc }

const
 flip_y = true;

 angle_step = 0.5;

 text_ : PChar =
  //'0123456789ABCDEFGHIJKLMNOPRSTUVWXYZabcdefghijklmnoprstuvwxyz ' +
  'Anti-Grain Geometry is designed as a set of loosely coupled ' +
  'algorithms and class templates united with a common idea, ' +
  'so that all the components can be easily combined. Also, ' +
  'the template based design allows you to replace any part of ' +
  'the library without the necessity to modify a single byte in ' +
  'the existing code. ' +
  'AGG is designed keeping in mind extensibility and flexibility. ' +
  'Basically I just wanted to create a toolkit that would allow me ' +
  '(and anyone else) to add new fancy algorithms very easily. ' +
  'AGG does not dictate you any style of its use, you are free to ' +
  'use any part of it. However, AGG is often associated with a tool ' +
  'for rendering images in memory. That is not quite true, but it can ' +
  'be a good starting point in studying. The tutorials describe the ' +
  'use of AGG starting from the low level functionality that deals with ' +
  'frame buffers and pixels. Then you will gradually understand how to ' +
  'abstract different parts of the library and how to use them separately. ' +
  'Remember, the raster picture is often not the only thing you want to ' +
  'obtain, you will probably want to print your graphics with highest ' +
  'possible quality and in this case you can easily combine the "vectorial" ' +
  'part of the library with some API like Windows GDI, having a common ' +
  'external interface. If that API can render multi-polygons with non-zero ' +
  'and even-odd filling rules it''s all you need to incorporate AGG into ' +
  'your application. For example, Windows API PolyPolygon perfectly fits ' +
  'these needs, except certain advanced things like gradient filling, ' +
  'Gouraud shading, image transformations, and so on. Or, as an alternative, ' +
  'you can use all AGG algorithms producing high resolution pixel images and ' +
  'then to send the result to the printer as a pixel map.' +
  'Below is a typical brief scheme of the AGG rendering pipeline. ' +
  'Please note that any component between the Vertex Source ' +
  'and Screen Output is not mandatory. It all depends on your ' +
  'particular needs. For example, you can use your own rasterizer, ' +
  'based on Windows API. In this case you won''t need the AGG rasterizer ' +
  'and renderers. Or, if you need to draw only lines, you can use the ' +
  'AGG outline rasterizer that has certain restrictions but works faster. ' +
  'The number of possibilities is endless. ' +
  'Vertex Source is some object that produces polygons or polylines as ' +
  'a set of consecutive 2D vertices with commands like MoveTo, LineTo. ' +
  'It can be a container or some other object that generates vertices ' +
  'on demand. ' +
  'Coordinate conversion pipeline consists of a number of coordinate ' +
  'converters. It always works with vectorial data (X,Y) represented ' +
  'as floating point numbers (double). For example, it can contain an ' +
  'affine transformer, outline (stroke) generator, some marker ' +
  'generator (like arrowheads/arrowtails), dashed lines generator, ' +
  'and so on. The pipeline can have branches and you also can have ' +
  'any number of different pipelines. You also can write your own ' +
  'converter and include it into the pipeline. ' +
  'Scanline Rasterizer converts vectorial data into a number of ' +
  'horizontal scanlines. The scanlines usually (but not obligatory) ' +
  'carry information about Anti-Aliasing as coverage values. ' +
  'Renderers render scanlines, sorry for the tautology. The simplest ' +
  'example is solid filling. The renderer just adds a color to the ' +
  'scanline and writes the result into the rendering buffer. ' +
  'More complex renderers can produce multi-color result, ' +
  'like gradients, Gouraud shading, image transformations, ' +
  'patterns, and so on. Rendering Buffer is a buffer in memory ' +
  'that will be displayed afterwards. Usually but not obligatory ' +
  'it contains pixels in format that fits your video system. ' +
  'For example, 24 bits B-G-R, 32 bits B-G-R-A, or 15 ' +
  'bits R-G-B-555 for Windows. But in general, there''re no ' +
  'restrictions on pixel formats or color space if you write ' +
  'your own low level class that supports that format. ' +
  'Colors in AGG appear only in renderers, that is, when you ' +
  'actually put some data to the rendering buffer. In general, ' +
  'there''s no general purpose structure or class like color, ' +
  'instead, AGG always operates with concrete color space. ' +
  'There are plenty of color spaces in the world, like RGB, ' +
  'HSV, CMYK, etc., and all of them have certain restrictions. ' +
  'For example, the RGB color space is just a poor subset of ' +
  'colors that a human eye can recognize. If you look at the full ' +
  'CIE Chromaticity Diagram, you will see that the RGB triangle ' +
  'is just a little part of it. ' +
  'In other words there are plenty of colors in the real world ' +
  'that cannot be reproduced with RGB, CMYK, HSV, etc. Any color ' +
  'space except the one existing in Nature is restrictive. Thus, ' +
  'it was decided not to introduce such an object like color in ' +
  'order not to restrict the possibilities in advance. Instead, ' +
  'there are objects that operate with concrete color spaces. ' +
  'Currently there are agg::rgba and agg::rgba8 that operate ' +
  'with the most popular RGB color space (strictly speaking there''s ' +
  'RGB plus Alpha). The RGB color space is used with different ' +
  'pixel formats, like 24-bit RGB or 32-bit RGBA with different ' +
  'order of color components. But the common property of all of ' +
  'them is that they are essentially RGB. Although, AGG doesn''t ' +
  'explicitly support any other color spaces, there is at least ' +
  'a potential possibility of adding them. It means that all ' +
  'class and function templates that depend on the color type ' +
  'are parameterized with the ColorT argument. ' +
  'Basically, AGG operates with coordinates of the output device. ' +
  'On your screen there are pixels. But unlike many other libraries ' +
  'and APIs AGG initially supports Subpixel Accuracy. It means ' +
  'that the coordinates are represented as doubles, where fractional ' +
  'values actually take effect. AGG doesn''t have an embedded ' +
  'conversion mechanism from world to screen coordinates in order ' +
  'not to restrict your freedom. It''s very important where and when ' +
  'you do that conversion, so, different applications can require ' +
  'different approaches. AGG just provides you a transformer of ' +
  'that kind, namely, that can convert your own view port to the ' +
  'device one. And it''s your responsibility to include it into ' +
  'the proper place of the pipeline. You can also write your ' +
  'own very simple class that will allow you to operate with ' +
  'millimeters, inches, or any other physical units. ' +
  'Internally, the rasterizers use integer coordinates of the ' +
  'format 24.8 bits, that is, 24 bits for the integer part and 8 ' +
  'bits for the fractional one. In other words, all the internal ' +
  'coordinates are multiplied by 256. If you intend to use AGG in ' +
  'some embedded system that has inefficient floating point ' +
  'processing, you still can use the rasterizers with their ' +
  'integer interfaces. Although, you won''t be able to use the ' +
  'floating point coordinate pipelines in this case. ';

var
 text_flip : boolean;
 font_name : AnsiString;

type
 the_application = object(platform_support )
   m_ren_type : rbox_ctrl;

   m_height ,
   m_width  ,
   m_weight ,
   m_gamma  : slider_ctrl;

   m_hinting     ,
   m_kerning     ,
   m_performance : cbox_ctrl;

   m_feng : font_engine_win32_tt_int32;
   m_fman : font_cache_manager;

   m_old_height : double;
   m_gamma_lut  : gamma_lut;

  // Pipeline to process the vectors glyph paths (curves + contour)
   m_curves  : conv_curve;
   m_contour : conv_contour;

   m_angle : double;

   constructor Construct(dc : HDC; format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   function  draw_text(
              ras : rasterizer_scanline_aa_ptr;
              sl  : scanline_ptr;
              ren_solid : renderer_scanline_aa_solid_ptr;
              ren_bin   : renderer_scanline_bin_solid_ptr ) : unsigned;

   procedure on_draw; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;
   procedure on_ctrl_change; virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_ren_type.Construct   (5.0 ,5.0  ,5.0 + 150.0 ,110.0 ,not flip_y_ );
 m_height.Construct     (160 ,10.0 ,640 - 5.0 ,18.0 ,not flip_y_ );
 m_width.Construct      (160 ,30.0 ,640 - 5.0 ,38.0 ,not flip_y_ );
 m_weight.Construct     (160 ,50.0 ,640 - 5.0 ,58.0 ,not flip_y_ );
 m_gamma.Construct      (260 ,70.0 ,640 - 5.0 ,78.0 ,not flip_y_ );
 m_hinting.Construct    (160 ,65.0 ,'Hinting' ,not flip_y_ );
 m_kerning.Construct    (160 ,80.0 ,'Kerning' ,not flip_y_ );
 m_performance.Construct(160 ,95.0 ,'Test Performance' ,not flip_y_ );

 m_feng.Construct(dc );
 m_fman.Construct(@m_feng );

 m_old_height:=0.0;

 m_gamma_lut.Construct_(8 ,16 );
 m_curves.Construct    (m_fman.path_adaptor );
 m_contour.Construct   (@m_curves );

 m_ren_type.add_item ('Native Mono' );
 m_ren_type.add_item ('Native Gray 8' );
 m_ren_type.add_item ('AGG Outline' );
 m_ren_type.add_item ('AGG Mono' );
 m_ren_type.add_item ('AGG Gray 8' );
 m_ren_type.cur_item_(1 );

 add_ctrl(@m_ren_type );

 m_ren_type.no_transform;

 m_height.label_('Font Height=%.2f' );
 m_height.range_(8, 32);
 m_height.value_(18 );

 m_height.num_steps_     (32 - 8 );
 m_height.text_thickness_(1.5 );

 add_ctrl(@m_height );

 m_height.no_transform;

 m_width.label_('Font Width=%.2f' );
 m_width.range_(8 ,32 );
 m_width.value_(18 );

 m_width.num_steps_     (32 - 8 );
 m_width.text_thickness_(1.5 );

 add_ctrl(@m_width );

 m_width.no_transform;

 m_weight.label_('Font Weight=%.2f' );
 m_weight.range_(-1 ,1 );

 m_weight.text_thickness_(1.5 );

 add_ctrl(@m_weight );

 m_weight.no_transform;

 m_gamma.label_('Gamma=%.2f' );
 m_gamma.range_(0.1 ,2.0 );
 m_gamma.value_(1.0 );

 m_gamma.text_thickness_(1.5 );

 add_ctrl(@m_gamma );

 m_gamma.no_transform;

 add_ctrl(@m_hinting );

 m_hinting.status_(true );
 m_hinting.no_transform;

 add_ctrl(@m_kerning );

 m_kerning.status_(true );
 m_kerning.no_transform;

 add_ctrl(@m_performance );

 m_performance.no_transform;

 //m_curves.approximation_method_(curve_div );
 //m_curves.approximation_scale_ (0.5 );
 //m_curves.angle_tolerance_     (0.3 );

 m_contour.auto_detect_orientation_(false );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_ren_type.Destruct;
 m_height.Destruct;
 m_width.Destruct;
 m_weight.Destruct;
 m_gamma.Destruct;
 m_hinting.Destruct;
 m_kerning.Destruct;
 m_performance.Destruct;

 m_feng.Destruct;
 m_fman.Destruct;

 m_gamma_lut.Destruct;
 m_curves.Destruct;
 m_contour.Destruct;

end;

{ DRAW_TEXT }
function the_application.draw_text;
var
 gren : glyph_rendering;

 num_glyphs : unsigned;

 mtx : trans_affine;
 taw : trans_affine_skewing;
 tar : trans_affine_rotation;

 x ,y0 ,y : double;

 p : int8u_ptr;

 rgba  : aggclr; 
 glyph : glyph_cache_ptr;

begin
 gren:=glyph_ren_native_mono;

 case m_ren_type._cur_item of
  0 : gren:=glyph_ren_native_mono;
  1 : gren:=glyph_ren_native_gray8;
  2 : gren:=glyph_ren_outline;
  3 : gren:=glyph_ren_agg_mono;
  4 : gren:=glyph_ren_agg_gray8;

 end;

 num_glyphs:=0;

 m_contour.width_(-m_weight._value * m_height._value * 0.05 );

 m_feng.hinting_(m_hinting._status );
 m_feng.height_ (m_height._value );

// Font width in Windows is strange. MSDN says,
// "specifies the average width", but there's no clue what
// this "average width" means. It'd be logical to specify
// the width with regard to the font height, like it's done in
// FreeType. That is, width == height should mean the "natural",
// not distorted glyphs. In Windows you have to specify
// the absolute width, which is very stupid and hard to use
// in practice.
 if m_width._value = m_height._value then
  m_feng.width_(0.0 )
 else
  m_feng.width_(m_width._value / 2.4 );

// m_feng.italic_(true );
 m_feng.flip_y_(text_flip );

 mtx.Construct;

 if m_angle <> 0 then
  begin
   tar.Construct(deg2rad(m_angle ) );
   mtx.multiply (@tar );

  end;

 //taw.Construct(-0.3 ,0 ); mtx.multiply(@taw );

 m_feng.transform_(@mtx );

 if m_feng.create_font(@font_name[1 ] ,gren ) then
  begin
   m_fman.precache(unsigned(' ' ) ,127 );

   x :=10.0;
   y0:=_height - m_height._value - 10.0;
   y :=y0;
   p :=@text_[0 ];

   while p^ <> 0 do
    begin
     glyph:=m_fman.glyph(p^ );

     if glyph <> NIL then
      begin
       if m_kerning._status then
        m_fman.add_kerning(@x ,@y );

       if x >= _width - m_height._value then
        begin
         x :=10.0;
         y0:=y0 - m_height._value;

         if y0 <= 120 then
          break;

         y:=y0;

        end;

       m_fman.init_embedded_adaptors(glyph ,x ,y );

       case glyph.data_type of
        glyph_data_mono :
         begin
          rgba.ConstrInt(0 ,0 ,0 );
          ren_bin.color_(@rgba );
          
          render_scanlines(
           m_fman.mono_adaptor ,
           m_fman.mono_scanline ,
           ren_bin );

         end;

        glyph_data_gray8 :
         begin
          rgba.ConstrInt  (0 ,0 ,0 );
          ren_solid.color_(@rgba );

          render_scanlines(
           m_fman.gray8_adaptor ,
           m_fman.gray8_scanline ,
           ren_solid );

         end;

        glyph_data_outline :
         begin
          ras.reset;

          if Abs(m_weight._value ) <= 0.01 then
          // For the sake of efficiency skip the
          // contour converter if the weight is about zero.
           ras.add_path(@m_curves )
          else
           ras.add_path(@m_contour );

          rgba.ConstrInt  (0 ,0 ,0 );
          ren_solid.color_(@rgba );

          render_scanlines(ras ,sl ,ren_solid );

         end;

       end;

      // increment pen position
       x:=x + glyph.advance_x;
       y:=y + glyph.advance_y;

       inc(num_glyphs );

      end;

     inc(ptrcomp(p ) ,sizeof(int8u ) );

    end;

  end;

 result:=num_glyphs;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pf : pixel_formats;

 rgba : aggclr;

 ren_base  : renderer_base;
 ren_solid : renderer_scanline_aa_solid;
 ren_bin   : renderer_scanline_bin_solid;

 sl  : scanline_u8;
 ras : rasterizer_scanline_aa;

 gm_th : gamma_threshold;
 gm_no : gamma_none;
 gm_pw : gamma_power;

begin
// Initialize structures
 pixfmt_bgr24_gamma(pf ,rbuf_window ,@m_gamma_lut );

 ren_base.Construct (@pf );
 ren_solid.Construct(@ren_base );
 ren_bin.Construct  (@ren_base );

 rgba.ConstrDbl(1 ,1 ,1 );
 ren_base.clear(@rgba );

 sl.Construct;
 ras.Construct;

 if m_height._value <> m_old_height then
  begin
   m_old_height:=m_height._value;

   m_width.value_(m_old_height );

  end;

// Setup Gamma
 if m_ren_type._cur_item = 3 then
  begin
  // When rendering in mono format,
  // Set threshold gamma = 0.5
   gm_th.Construct(m_gamma._value / 2.0 );
   m_feng.gamma_  (@gm_th );

  end
 else
  begin
   gm_no.Construct;
   m_feng.gamma_(@gm_no );

   m_gamma_lut.gamma_(m_gamma._value );

  end;

// Render the text
 draw_text(@ras ,@sl ,@ren_solid ,@ren_bin );

// Render the controls
 gm_pw.Construct(1.0 );
 ras.gamma      (@gm_pw );

 render_ctrl(@ras ,@sl ,@ren_solid ,@m_ren_type );
 render_ctrl(@ras ,@sl ,@ren_solid ,@m_height );
 render_ctrl(@ras ,@sl ,@ren_solid ,@m_width );
 render_ctrl(@ras ,@sl ,@ren_solid ,@m_weight );
 render_ctrl(@ras ,@sl ,@ren_solid ,@m_gamma );
 render_ctrl(@ras ,@sl ,@ren_solid ,@m_hinting );
 render_ctrl(@ras ,@sl ,@ren_solid ,@m_kerning );
 render_ctrl(@ras ,@sl ,@ren_solid ,@m_performance );

// Free AGG resources
 sl.Destruct;
 ras.Destruct;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = byte(' ' ) then
  begin
   text_flip:=not text_flip;

   force_redraw;

  end;

 if key = key_kp_minus then
  begin
   m_angle:=m_angle + angle_step;

   if m_angle > 360 then
    m_angle:=0;

   force_redraw;

  end;

 if key = key_kp_plus then
  begin
   m_angle:=m_angle - angle_step;

   if m_angle < 0 then
    m_angle:=360 - angle_step;

   force_redraw;

  end;

 if key = key_f1 then
  message_(
   'This example demonstrates the use of the Win32 TrueType font engine with cache. '#13 +
   'Cache can keep three types of data, vector path, Anti-Aliased scanline shape, '#13 +
   'and monochrome scanline shape. In case of caching scanline shapes the speed '#13 +
   'is pretty good and comparable with Windows hardware accelerated font rendering.'#13#13 +
   'How to play with:'#13#13 +
   'Press the spacebar to flip the text vertically.'#13#13 +
   'Key Plus - Increase font angle (not for Natives)'#13 +
   'Key Minus - Decrease font angle (not for Natives)' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

{ ON_CTRL_CHANGE }
procedure the_application.on_ctrl_change;
var
 pf : pixel_formats;

 rgba : aggclr;

 ren_base  : renderer_base;
 ren_solid : renderer_scanline_aa_solid;
 ren_bin   : renderer_scanline_bin_solid;

 sl  : scanline_u8;
 ras : rasterizer_scanline_aa;

 num_glyphs ,i : unsigned;

 t : double;

 buf : array[0..99 ] of char;

begin
 if m_performance._status then
  begin
   pixfmt_bgr24_gamma(pf ,rbuf_window ,@m_gamma_lut );

   ren_base.Construct (@pf );
   ren_solid.Construct(@ren_base );
   ren_bin.Construct  (@ren_base );

   rgba.ConstrDbl(1 ,1 ,1 );
   ren_base.clear(@rgba );

   sl.Construct;
   ras.Construct;

   num_glyphs:=0;

   start_timer;

   for i:=0 to 49 do
    inc(num_glyphs ,draw_text(@ras ,@sl ,@ren_solid ,@ren_bin ) );

   t:=elapsed_time;

   sprintf(@buf[0 ]             ,'Glyphs=%u, ' ,num_glyphs );
   sprintf(@buf[StrLen(@buf ) ] ,'Time=%.3fms, ' ,t );
   sprintf(@buf[StrLen(@buf ) ] ,'%.3f glyps/sec, ' ,(num_glyphs / t ) * 1000.0 );
   sprintf(@buf[StrLen(@buf ) ] ,'%.3f microsecond/glyph' , (t / num_glyphs) * 1000.0);

   message_(@buf[0 ] );

   m_performance.status_(false );

   force_redraw;

   sl.Destruct;
   ras.Destruct;

  end;

end;

VAR
 app : the_application;
 dc  : HDC;

BEGIN
 text_flip:=false;
 font_name:='Arial';

{$IFDEF WIN32 }
 if ParamCount > 0 then
  font_name:=ParamStr(1 );

{$ENDIF }

 dc:=GetDC(0 );

 app.Construct(dc ,pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. Rendering TrueType Fonts with WinAPI (F1-Help)' );

 if app.init(640 ,520 ,window_resize ) then
  app.run;

 app.Destruct;

 ReleaseDC(0 ,dc );

END.