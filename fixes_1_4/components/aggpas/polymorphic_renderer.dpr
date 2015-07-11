//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 polymorphic_renderer ;

uses
 agg_basics ,
 agg_platform_support ,
 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,
 agg_pixfmt_rgb_packed ,
 agg_pixfmt_rgba ,
 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_rasterizer_scanline_aa ,
 agg_scanline_p ,
 agg_render_scanlines ,
 agg_path_storage ;

{$I agg_mode.inc }

const
 flip_y = true;

 //pix_fmt = pix_format_rgb555;
 //pix_fmt = pix_format_rgb565;
 //pix_fmt = pix_format_rgb24;
 pix_fmt = pix_format_bgr24;
 //pix_fmt = pix_format_rgba32;
 //pix_fmt = pix_format_argb32;
 //pix_fmt = pix_format_abgr32;
 //pix_fmt = pix_format_bgra32;

type
 polymorphic_renderer_solid_rgba8_base = object
   destructor  Destruct; virtual;

   procedure clear(c : aggclr_ptr ); virtual; abstract;
   procedure color(c : aggclr_ptr ); virtual; abstract;

  end;

 polymorphic_renderer_solid_rgba8_adaptor_ptr = ^polymorphic_renderer_solid_rgba8_adaptor;
 polymorphic_renderer_solid_rgba8_adaptor = object(polymorphic_renderer_solid_rgba8_base )
   m_pixfmt   : pixel_formats;
   m_ren_base : renderer_base;
   m_ren      : renderer_scanline_aa_solid;

   constructor Construct(PixFmt : pix_format_e; rbuf : rendering_buffer_ptr );

   procedure clear(c : aggclr_ptr ); virtual;
   procedure color(c : aggclr_ptr ); virtual;

   function  RendererBase : renderer_scanline_aa_solid_ptr;

  end;

 the_application = object(platform_support )
   m_x ,
   m_y : array[0..2 ] of double;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_draw; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ DESTRUCT }
destructor polymorphic_renderer_solid_rgba8_base.Destruct;
begin
end;

{ CONSTRUCT }
constructor polymorphic_renderer_solid_rgba8_adaptor.Construct;
begin
 pixfmt_undefined(m_pixfmt );

 case PixFmt of
  pix_format_rgb555 :
   pixfmt_rgb555(m_pixfmt ,rbuf );

  pix_format_rgb565 :
   pixfmt_rgb565(m_pixfmt ,rbuf );

  pix_format_rgb24  :
   pixfmt_rgb24(m_pixfmt ,rbuf );

  pix_format_bgr24  :
   pixfmt_bgr24(m_pixfmt ,rbuf );

  pix_format_rgba32 :
   pixfmt_rgba32(m_pixfmt ,rbuf );

  pix_format_argb32 :
   pixfmt_argb32(m_pixfmt ,rbuf );

  pix_format_abgr32 :
   pixfmt_abgr32(m_pixfmt ,rbuf );

  pix_format_bgra32 :
   pixfmt_bgra32(m_pixfmt ,rbuf );

 end;

 if m_pixfmt.m_rbuf <> NIL then
  begin
   m_ren_base.Construct(@m_pixfmt );
   m_ren.Construct     (@m_ren_base );

  end; 

end;

{ CLEAR }
procedure polymorphic_renderer_solid_rgba8_adaptor.clear;
begin
 if m_pixfmt.m_rbuf <> NIL then
  m_ren_base.clear(c );

end;

{ COLOR }
procedure polymorphic_renderer_solid_rgba8_adaptor.color;
begin
 if m_pixfmt.m_rbuf <> NIL then
  m_ren.color_(c );

end;

{ RENDERERBASE }
function polymorphic_renderer_solid_rgba8_adaptor.RendererBase;
begin
 if m_pixfmt.m_rbuf <> NIL then
  result:=@m_ren
 else
  result:=NIL;

end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_x[0 ]:=100; m_y[0 ]:=60;
 m_x[1 ]:=369; m_y[1 ]:=170;
 m_x[2 ]:=143; m_y[2 ]:=310;

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 ren : polymorphic_renderer_solid_rgba8_adaptor_ptr;
 ras : rasterizer_scanline_aa;
 sl  : scanline_p8;

 rgba : aggclr;
 path : path_storage;

begin
// Create Path
 path.Construct;

 path.move_to(m_x[0 ] ,m_y[0 ] );
 path.line_to(m_x[1 ] ,m_y[1 ] );
 path.line_to(m_x[2 ] ,m_y[2 ] );

 path.close_polygon;

// Rasterizer, scanlines & Polymorphic renderer class factory
 new(ren ,Construct(pix_fmt ,rbuf_window ) );

 ras.Construct;
 sl.Construct;

// Render
 if (ren <> NIL ) and
    (ren.RendererBase <> NIL ) then
  begin
   rgba.ConstrInt(255 ,255 ,255 );
   ren.clear     (@rgba );

   rgba.ConstrInt(80 ,30 ,20 );
   ren.color     (@rgba );

   ras.add_path(@path );

   render_scanlines(@ras ,@sl ,ren.RendererBase );

  end;

// Free AGG resources
 ras.Destruct;
 sl.Destruct;

 dispose(ren ,Destruct );

 path.Destruct;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'There''s nothing looking effective. AGG has renderers for different pixel formats '#13 +
   'in memory, particularly, for different byte order (RGB or BGR). But the renderers '#13 +
   'are class templates (only C++), where byte order is defined at the compile time. '#13 +
   'It''s done for the sake of performance and in most cases it fits all your needs. '#13 +
   'Still, if you need to switch between different pixel formats dynamically, you can '#13 +
   'write a simple polymorphic class wrapper, like the one in this example.'#13#13 +
   'How to play with:'#13#13 +
   'To use another pixel format for rendering, comment/uncomment the pix_fmt     '#13 +
   'constant in the demo source code and recompile it.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_fmt ,flip_y );
 app.caption_ ('AGG Example. Polymorphic Renderers (F1-Help)' );

 if app.init(400 ,330 ,window_resize ) then
  app.run;

 app.Destruct;

END.