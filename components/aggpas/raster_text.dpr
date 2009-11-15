//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 raster_text ;

uses
 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_rasterizer_scanline_aa ,
 agg_scanline_u ,
 agg_render_scanlines ,

 agg_span_allocator ,
 agg_span_gradient ,
 agg_span_interpolator_linear ,
 agg_embedded_raster_fonts ,
 agg_glyph_raster_bin ,
 agg_renderer_raster_text ,
 agg_trans_affine ;

{$I agg_mode.inc }

const
 flip_y = true;

type
 gradient_sine_repeat_adaptor = object(gradient )
   m_gradient : gradient_ptr;
   m_periods  : double;

   constructor Construct(GF : gradient_ptr );

   procedure _periods(p : double );

   function  calculate(x ,y ,d : int ) : int; virtual;

  end;

 the_application = object(platform_support )
   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_draw; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor gradient_sine_repeat_adaptor.Construct;
begin
 m_gradient:=GF;
 m_periods :=pi* 2.0;

end;

{ _PERIODS }
procedure gradient_sine_repeat_adaptor._periods;
begin
 m_periods:=p * pi * 2.0;

end;

{ CALCULATE }
function gradient_sine_repeat_adaptor.calculate;
begin
 result:=trunc((1.0 + Sin(m_gradient.calculate(x ,y ,d ) * m_periods / d ) ) * d / 2 );

end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
type
 font_type = record
   font : int8u_ptr;
   name : PChar;

  end;

const
 fonts : array[0..34 ] of font_type = (
  (font:@gse4x6;               name:'gse4x6'               ) ,
  (font:@gse4x8;               name:'gse4x8'               ) ,
  (font:@gse5x7;               name:'gse5x7'               ) ,
  (font:@gse5x9;               name:'gse5x9'              ) ,
  (font:@gse6x9;               name:'gse6x9'               ) ,
  (font:@gse6x12;              name:'gse6x12'              ) ,
  (font:@gse7x11;              name:'gse7x11'              ) ,
  (font:@gse7x11_bold;         name:'gse7x11_bold'         ) ,
  (font:@gse7x15;              name:'gse7x15'              ) ,
  (font:@gse7x15_bold;         name:'gse7x15_bold'         ) ,
  (font:@gse8x16;              name:'gse8x16'              ) ,
  (font:@gse8x16_bold;         name:'gse8x16_bold'         ) ,
  (font:@mcs11_prop;           name:'mcs11_prop'           ) ,
  (font:@mcs11_prop_condensed; name:'mcs11_prop_condensed' ) ,
  (font:@mcs12_prop;           name:'mcs12_prop'           ) ,
  (font:@mcs13_prop;           name:'mcs13_prop'           ) ,
  (font:@mcs5x10_mono;         name:'mcs5x10_mono'         ) ,
  (font:@mcs5x11_mono;         name:'mcs5x11_mono'         ) ,
  (font:@mcs6x10_mono;         name:'mcs6x10_mono'         ) ,
  (font:@mcs6x11_mono;         name:'mcs6x11_mono'         ) ,
  (font:@mcs7x12_mono_high;    name:'mcs7x12_mono_high'    ) ,
  (font:@mcs7x12_mono_low;     name:'mcs7x12_mono_low'     ) ,
  (font:@verdana12;            name:'verdana12'            ) ,
  (font:@verdana12_bold;       name:'verdana12_bold'       ) ,
  (font:@verdana13;            name:'verdana13'            ) ,
  (font:@verdana13_bold;       name:'verdana13_bold'       ) ,
  (font:@verdana14;            name:'verdana14'            ) ,
  (font:@verdana14_bold;       name:'verdana14_bold'       ) ,
  (font:@verdana16;            name:'verdana16'            ) ,
  (font:@verdana16_bold;       name:'verdana16_bold'       ) ,
  (font:@verdana17;            name:'verdana17'            ) ,
  (font:@verdana17_bold;       name:'verdana17_bold'       ) ,
  (font:@verdana18;            name:'verdana18'            ) ,
  (font:@verdana18_bold;       name:'verdana18_bold'       ) ,
  (font:NIL;                   name:NIL ) );

var
 pixf : pixel_formats;
 rgba ,
 rgbb : aggclr;

 rb : renderer_base;
 rt : renderer_raster_htext_solid;

 glyph : glyph_raster_bin;

 i : int;
 y : double;

 buf : string[100 ];
 mtx : trans_affine;

 grad_circ  : gradient_circle;
 grad_func  : gradient_sine_repeat_adaptor;
 color_func : gradient_linear_color;

 inter : span_interpolator_linear;

 sa  : span_allocator;
 sg  : span_gradient;
 ren : renderer_scanline_aa;
 rt2 : renderer_raster_htext;

begin
// Initialize structures
 glyph.Construct(NIL );

 pixfmt_bgr24(pixf ,rbuf_window );
 rb.Construct(@pixf );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear      (@rgba );

 rt.Construct(@rb ,@glyph );

 y:=5;

 rgba.ConstrDbl(0 ,0 ,0 );
 rt.color_     (@rgba );

// Render all raster fonts
 i:=0;

 while fonts[i ].font <> NIL do
  begin
   buf:='A quick brown fox jumps over the lazy dog 0123456789: ' + fonts[i ].name + #0;

   glyph.font_   (fonts[i ].font );
   rt.render_text(5 ,y ,@buf[1 ] ,not flip_y );

   y:=y + glyph.height + 1;

   inc(i );

  end;

// Render gradient font
 grad_circ.Construct;
 grad_func.Construct(@grad_circ );
 grad_func._periods (5 );

 rgba.ConstrDbl      (1.0 ,0 ,0 );
 rgbb.ConstrDbl      (0 ,0.5 ,0 );
 color_func.Construct(@rgba ,@rgbb );

 mtx.Construct;
 inter.Construct(@mtx );

 sa.Construct;
 sg.Construct (@sa ,@inter ,@grad_func ,@color_func ,0 ,150.0 );
 ren.Construct(@rb ,@sg );
 rt2.Construct(@ren ,@glyph );

 rt2.render_text(5 ,465 ,'RADIAL REPEATING GRADIENT: A quick brown fox jumps over the lazy dog' ,not flip_y );

// Free AGG resources
 sa.Destruct;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'Classes that render raster text was added in AGG mostly to prove the concept of   '#13 +
   'the design. They can be used to draw simple (aliased) raster text. The example '#13 +
   'demonstrates how to use text as a custom scanline generator together with any '#13 +
   'span generator (in this example it''s gradient filling). The font format is '#13 +
   'propriatory, but there are some predefined fonts that are shown in the example.'#13#13 +
   'How to play with:'#13#13 +
   'Change the renderer "rt" to "renderer_raster_vtext_solid" in the source code'#13 +
   'and recompile it, to get the vertical raster font orientation.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. Raster Text (F1-Help)' );

 if app.init(640 ,480 ,window_resize ) then
  app.run;

 app.Destruct;

END.