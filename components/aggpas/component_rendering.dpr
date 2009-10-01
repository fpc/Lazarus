//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 component_rendering ;

uses
 agg_basics ,
 agg_platform_support ,
 agg_slider_ctrl ,
 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_gray ,
 agg_pixfmt_rgb ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_rasterizer_scanline_aa ,
 agg_scanline_p ,
 agg_render_scanlines ,
 agg_ctrl ,
 agg_ellipse ;

{$I agg_mode.inc }

const
 flip_y = true;

type
 the_application = object(platform_support )
   m_alpha : slider_ctrl_ptr;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_draw; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 new(m_alpha ,Construct(5 ,5 ,320 - 5 ,10 + 5 ,not flip_y ) );

 m_alpha.label_('Alpha=%1.0f' );
 m_alpha.range_(0 ,255 );
 m_alpha.value_(255 );

 add_ctrl(m_alpha );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 dispose(m_alpha ,Destruct );

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pf  ,
 pfr ,
 pfg ,
 pfb : pixel_formats;

 rbase ,
 rbr ,
 rbg ,
 rbb : renderer_base;

 r  ,
 rr ,
 rg ,
 rb : renderer_scanline_aa_solid;

 ras : rasterizer_scanline_aa;
 sl  : scanline_p8;

 rgba ,
 gray : aggclr;

 er ,
 eg ,
 eb : ellipse;

begin
// Initialize structures
 pixfmt_bgr24       (pf  ,rbuf_window );
 pixfmt_gray8_bgr24r(pfr ,rbuf_window );
 pixfmt_gray8_bgr24g(pfg ,rbuf_window );
 pixfmt_gray8_bgr24b(pfb ,rbuf_window );

 rbase.Construct(@pf );
 rbr.Construct  (@pfr );
 rbg.Construct  (@pfg );
 rbb.Construct  (@pfb );

 r.Construct (@rbase );
 rr.Construct(@rbr );
 rg.Construct(@rbg );
 rb.Construct(@rbb );

 ras.Construct;
 sl.Construct;

// Setup colors & background
 rgba.ConstrDbl(1 ,1 ,1 );
 gray.ConstrInt(0 ,trunc(m_alpha._value ) );

 rbase.clear(@rgba );

// Draw ellipses
 er.Construct    (_width / 2 - 0.87 * 50 ,_height / 2 - 0.5 * 50 ,100 ,100 ,100 );
 rr.color_       (@gray );
 ras.add_path    (@er );
 render_scanlines(@ras ,@sl ,@rr );

 eg.Construct    (_width / 2 + 0.87 * 50 ,_height / 2 - 0.5 * 50 ,100 ,100 ,100 );
 rg.color_       (@gray );
 ras.add_path    (@eg);
 render_scanlines(@ras ,@sl ,@rg );

 eb.Construct    (_width / 2 ,_height / 2 + 50 ,100 ,100 ,100 );
 rb.color_       (@gray );
 ras.add_path    (@eb );
 render_scanlines(@ras ,@sl ,@rb );

// Render control
 render_ctrl(@ras ,@sl ,@r ,m_alpha );

// Free AGG resources
 ras.Destruct;
 sl.Destruct;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'AGG has a gray-scale renderer that can use any 8-bit color channel '#13 +
   'of an RGB or RGBA frame buffer. Most likely it will be used to draw '#13 +
   'gray-scale images directly in the alpha-channel.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. Component Rendering (F1-Help)' );

 if app.init(320 ,320 ,0 ) then
  app.run;

 app.Destruct;

END.