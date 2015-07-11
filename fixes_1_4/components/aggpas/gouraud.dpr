//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 gouraud ;

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

 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_u ,
 agg_scanline_p ,

 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_render_scanlines ,

 agg_math ,
 agg_dda_line ,
 agg_span_allocator ,
 agg_span_gouraud_rgba ,
 agg_span_gouraud_gray ,
 agg_span_solid ,
 agg_vertex_source ,
 agg_gamma_functions 

{$I pixel_formats.inc }
{$I agg_mode.inc }

const
 flip_y = true;

type
 the_application = object(platform_support )
   m_x ,
   m_y : array[0..2 ] of double;

   m_dx ,
   m_dy : double;

   m_idx : int;

   m_dilation ,
   m_gamma    ,
   m_alpha    : slider_ctrl;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure render_gouraud(sl : scanline_ptr; ras : rasterizer_scanline_aa_ptr );

   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_dilation.Construct(5 ,5 ,400 - 5 ,11 ,not flip_y_ );
 m_gamma.Construct   (5 ,5 + 15 ,400 - 5 ,11 + 15 ,not flip_y_ );
 m_alpha.Construct   (5 ,5 + 30 ,400 - 5 ,11 + 30 ,not flip_y_ );

 m_idx:=-1;

 m_x[0 ]:=57;  m_y[0 ]:=60;
 m_x[1 ]:=369; m_y[1 ]:=170;
 m_x[2 ]:=143; m_y[2 ]:=310;

 add_ctrl(@m_dilation );
 add_ctrl(@m_gamma );
 add_ctrl(@m_alpha );

 m_dilation.label_('Dilation=%3.2f' );
 m_gamma.label_   ('Linear gamma=%3.2f' );
 m_alpha.label_   ('Opacity=%3.2f' );

 m_dilation.value_(0.175 );
 m_gamma.value_   (0.809 );
 m_alpha.value_   (1.0 );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_dilation.Destruct;
 m_gamma.Destruct;
 m_alpha.Destruct;

end;

{ RENDER_GOURAUD }
procedure the_application.render_gouraud;
var
 alpha ,brc ,d ,xc ,yc ,x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double; 

 pf : pixel_formats;

{$IFDEF AGG_GRAY8 }
 span_gen : span_gouraud_gray;

{$ELSE }
 span_gen : span_gouraud_rgba;

{$ENDIF } 

 ren_base    : renderer_base;
 span_alloc  : span_allocator;
 ren_gouraud : renderer_scanline_aa;

 gm_li : gamma_linear;

 rgba ,
 rgbb ,
 rgbc : aggclr;

begin
 alpha:=m_alpha._value;
 brc  :=1;

// Initialize structures
 pixfmt(pf ,rbuf_window );

 ren_base.Construct   (@pf );
 span_alloc.Construct;
 span_gen.Construct   (@span_alloc );
 ren_gouraud.Construct(@ren_base ,@span_gen );

 gm_li.Construct(0.0 ,m_gamma._value );
 ras.gamma      (@gm_li );

 d:=m_dilation._value;

// Single triangle
{ rgba.ConstrDbl   (1 ,0 ,0 ,alpha );
 rgbb.ConstrDbl   (0 ,1 ,0 ,alpha );
 rgbc.ConstrDbl   (0 ,0 ,1 ,alpha );
 span_gen.colors  (@rgba ,@rgbb ,@rgbc );
 span_gen.triangle(m_x[0 ] ,m_y[0 ] ,m_x[1 ] ,m_y[1 ] ,m_x[2 ] ,m_y[2 ] ,d );

 ras.add_path    (@span_gen);
 render_scanlines(ras ,sl ,@ren_gouraud );{}

// Six triangles
 xc:=(m_x[0 ] + m_x[1 ] + m_x[2 ] ) / 3.0;
 yc:=(m_y[0 ] + m_y[1 ] + m_y[2 ] ) / 3.0;

 x1:=(m_x[1 ] + m_x[0 ] ) / 2 - (xc - (m_x[1 ] + m_x[0 ] ) / 2 );
 y1:=(m_y[1 ] + m_y[0 ] ) / 2 - (yc - (m_y[1 ] + m_y[0 ] ) / 2 );

 x2:=(m_x[2 ] + m_x[1 ] ) / 2 - (xc - (m_x[2 ] + m_x[1 ] ) / 2 );
 y2:=(m_y[2 ] + m_y[1 ] ) / 2 - (yc - (m_y[2 ] + m_y[1 ] ) / 2 );

 x3:=(m_x[0 ] + m_x[2 ] ) / 2 - (xc - (m_x[0 ] + m_x[2 ] ) / 2 );
 y3:=(m_y[0 ] + m_y[2 ] ) / 2 - (yc - (m_y[0 ] + m_y[2 ] ) / 2 );

 rgba.ConstrDbl   (1 ,0 ,0 ,alpha );
 rgbb.ConstrDbl   (0 ,1 ,0 ,alpha );
 rgbc.ConstrDbl   (brc ,brc ,brc ,alpha );
 span_gen.colors_ (@rgba ,@rgbb ,@rgbc );
 span_gen.triangle(m_x[0 ] ,m_y[0 ] ,m_x[1 ] ,m_y[1 ] ,xc ,yc ,d );

 ras.add_path    (@span_gen );
 render_scanlines(ras ,sl ,@ren_gouraud );

 rgba.ConstrDbl   (0 ,1 ,0 ,alpha );
 rgbb.ConstrDbl   (0 ,0 ,1 ,alpha );
 rgbc.ConstrDbl   (brc ,brc ,brc ,alpha );
 span_gen.colors_ (@rgba ,@rgbb ,@rgbc );
 span_gen.triangle(m_x[1 ] ,m_y[1 ] ,m_x[2 ] ,m_y[2 ] ,xc ,yc ,d );

 ras.add_path    (@span_gen );
 render_scanlines(ras ,sl ,@ren_gouraud );

 rgba.ConstrDbl   (0 ,0 ,1 ,alpha );
 rgbb.ConstrDbl   (1 ,0 ,0 ,alpha );
 rgbc.ConstrDbl   (brc ,brc ,brc ,alpha );
 span_gen.colors_ (@rgba ,@rgbb ,@rgbc );
 span_gen.triangle(m_x[2 ] ,m_y[2 ] ,m_x[0 ] ,m_y[0 ] ,xc ,yc ,d );

 ras.add_path    (@span_gen );
 render_scanlines(ras ,sl ,@ren_gouraud );

 brc:=1 - brc;

 rgba.ConstrDbl   (1 ,0 ,0 ,alpha );
 rgbb.ConstrDbl   (0 ,1 ,0 ,alpha );
 rgbc.ConstrDbl   (brc ,brc ,brc ,alpha );
 span_gen.colors_ (@rgba ,@rgbb ,@rgbc );
 span_gen.triangle(m_x[0 ] ,m_y[0 ] ,m_x[1 ] ,m_y[1 ] ,x1 ,y1 ,d );

 ras.add_path    (@span_gen );
 render_scanlines(ras ,sl ,@ren_gouraud );

 rgba.ConstrDbl   (0 ,1 ,0 ,alpha );
 rgbb.ConstrDbl   (0 ,0 ,1 ,alpha );
 rgbc.ConstrDbl   (brc ,brc ,brc ,alpha );
 span_gen.colors_ (@rgba ,@rgbb ,@rgbc );
 span_gen.triangle(m_x[1 ] ,m_y[1 ] ,m_x[2 ] ,m_y[2 ] ,x2 ,y2 ,d );

 ras.add_path    (@span_gen);
 render_scanlines(ras ,sl ,@ren_gouraud );

 rgba.ConstrDbl   (0 ,0 ,1 ,alpha );
 rgbb.ConstrDbl   (1 ,0 ,0 ,alpha );
 rgbc.ConstrDbl   (brc ,brc ,brc ,alpha );
 span_gen.colors_ (@rgba ,@rgbb ,@rgbc );
 span_gen.triangle(m_x[2 ] ,m_y[2 ] ,m_x[0 ] ,m_y[0 ] ,x3 ,y3 ,d );

 ras.add_path    (@span_gen );
 render_scanlines(ras ,sl ,@ren_gouraud );{}

// Free AGG resources
 span_alloc.Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pf : pixel_formats;

 ren_base  : renderer_base;
 ren_solid : renderer_scanline_aa_solid;

 ras : rasterizer_scanline_aa;
 sl  : scanline_u8;

 rgba  : aggclr;
 gm_no : vertex_source;

begin
// Initialize structures
 pixfmt(pf ,rbuf_window );

 ren_base.Construct (@pf );
 ren_solid.Construct(@ren_base );

 rgba.ConstrDbl(1 ,1 ,1 );
 ren_base.clear(@rgba );

 sl.Construct;
 ras.Construct;

// Render gouraud
 render_gouraud(@sl ,@ras );

// Render the controls
 gm_no.Construct;
 ras.gamma(@gm_no );

 render_ctrl(@ras ,@sl ,@ren_solid ,@m_dilation );
 render_ctrl(@ras ,@sl ,@ren_solid ,@m_gamma );
 render_ctrl(@ras ,@sl ,@ren_solid ,@m_alpha );

// Free AGG resources
 sl.Destruct;
 ras.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
var
 dx ,dy : double;

begin
 if flags and mouse_left <> 0 then
  begin
   if m_idx = 3 then
    begin
     dx:=x - m_dx;
     dy:=y - m_dy;

     m_x[1 ]:=m_x[1 ] - (m_x[0 ] - dx );
     m_y[1 ]:=m_y[1 ] - (m_y[0 ] - dy );
     m_x[2 ]:=m_x[2 ] - (m_x[0 ] - dx );
     m_y[2 ]:=m_y[2 ] - (m_y[0 ] - dy );
     m_x[0 ]:=dx;
     m_y[0 ]:=dy;

     force_redraw;
     exit;

    end;

   if m_idx >= 0 then
    begin
     m_x[m_idx ]:=x - m_dx;
     m_y[m_idx ]:=y - m_dy;

     force_redraw;

    end;
  
  end
 else
  on_mouse_button_up(x ,y ,flags );

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
var
 i : unsigned;

 sl  : scanline_u8;
 ras : rasterizer_scanline_aa;
 buf : array[0..99 ] of char;

begin
 if flags and mouse_right <> 0 then
  begin
   sl.Construct;
   ras.Construct;

   start_timer;

   for i:=0 to 99 do
    render_gouraud(@sl ,@ras );

   sprintf (@buf[0 ] ,'Time=%2.2f ms' ,elapsed_time );
   message_(@buf[0 ] );

   sl.Destruct;
   ras.Destruct;

  end;

 if flags and mouse_left <> 0 then
  begin
   i:=0;

   while i < 3 do
    begin
     if Sqrt((x-m_x[i ] ) * (x-m_x[i ] ) + (y-m_y[i ] ) * (y-m_y[i ] ) ) < 10.0 then
      begin
       m_dx :=x - m_x[i ];
       m_dy :=y - m_y[i ];
       m_idx:=i;

       break;

      end;

     inc(i );

    end;

   if i = 3 then
    if point_in_triangle(m_x[0 ] ,m_y[0 ] ,m_x[1 ] ,m_y[1 ] ,m_x[2 ] ,m_y[2 ] ,x ,y ) then
     begin
      m_dx :=x - m_x[0 ];
      m_dy :=y - m_y[0 ];
      m_idx:=3;

     end;

  end;

end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
 m_idx:=-1;

end;

{ ON_KEY }
procedure the_application.on_key;
var
 dx ,dy : double;

begin
 dx:=0;
 dy:=0;

 case key of
  key_left  : dx:=0.1;
  key_right : dx:=0.1;
  key_up    : dy:=0.1;
  key_down  : dy:=-0.1;

 end;

 m_x[0 ]:=m_x[0 ] + dx;
 m_y[0 ]:=m_y[0 ] + dy;
 m_x[1 ]:=m_x[1 ] + dx;
 m_y[1 ]:=m_y[1 ] + dy;

 force_redraw;

 if key = key_f1 then
  message_(
   'Gouraud shading. It''s a simple method of interpolating colors in a triangle. '#13 +
   'There''s no "cube" drawn, there''re just 6 triangles. You define a triangle '#13 +
   'and colors in its vertices. When rendering, the colors will be linearly interpolated. '#13 +
   'But there''s a problem that appears when drawing adjacent triangles with Anti-Aliasing.'#13 +
   'Anti-Aliased polygons do not "dock" to each other correctly, there visual artifacts '#13 +
   'at the edges appear. I call it "the problem of adjacent edges". AGG has a simple '#13 +
   'mechanism that allows you to get rid of the artifacts, just dilating the polygons '#13 +
   'and/or changing the gamma-correction value. But it''s tricky, because the values '#13 +
   'depend on the opacity of the polygons. In this example you can change the opacity, '#13 +
   'the dilation value and gamma.'#13#13 +
   'How to play with:'#13#13 +
   'Use the left mouse button to drag the Red, Green and Blue corners of the "cube".'#13 +
   'Use the right mouse button to issue a performance test (100x).' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format ,flip_y );
 app.caption_ ('AGG Example. Gouraud Shading (F1-Help)' );

 if app.init(400 ,320 ,window_resize ) then
  app.run;

 app.Destruct;

END.