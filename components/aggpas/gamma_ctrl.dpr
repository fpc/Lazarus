//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 gamma_ctrl_ ;

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
 agg_gamma_ctrl ,

 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_p ,

 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_render_scanlines ,

 agg_gsv_text ,
 agg_conv_stroke ,
 agg_conv_transform ,
 agg_path_storage ,
 agg_ellipse ,
 agg_trans_affine

{$I pixel_formats.inc }
{$I agg_mode.inc }
{$I- }
const
 flip_y = true;

var
 g_ctrl : gamma_ctrl;

type
 the_application = object(platform_support )
   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ READ_GAMMA }
procedure read_gamma(fname : shortstring );
var
 fd : text;

 kx1 ,ky1 ,kx2 ,ky2 : double;

begin
 assignfile(fd ,fname );
 reset     (fd );

 readln(fd ,kx1 );
 readln(fd ,ky1 );
 readln(fd ,kx2 );
 readln(fd ,ky2 );

 g_ctrl.values(kx1 ,ky1 ,kx2 ,ky2 );

 close(fd );

end;

{ WRITE_GAMMA_BIN }
procedure write_gamma_bin(fname : shortstring );
var
 fd : file;

 gamma : char_ptr;

begin
 gamma:=g_ctrl.gamma;

 assignfile(fd ,fname );
 rewrite   (fd ,1 );
 blockwrite(fd ,gamma^ ,256 );
 close     (fd );

end;

{ WRITE_GAMMA_TXT }
procedure write_gamma_txt(fname : shortstring );
var
 fd : text;

 gamma : char_ptr;
 i ,j  : int;

 kx1 ,ky1 ,kx2 ,ky2 : double;

begin
 gamma:=g_ctrl.gamma;

 assignfile(fd ,fname );
 rewrite   (fd );

 g_ctrl.values(@kx1 ,@ky1 ,@kx2 ,@ky2 );

 writeln(fd ,kx1:1:3 );
 writeln(fd ,ky1:1:3 );
 writeln(fd ,kx2:1:3 );
 writeln(fd ,ky2:1:3 );

 for i:=0 to 15 do
  begin
   for j:= 0 to 15 do
    write(fd ,int8u_ptr(ptrcomp(gamma ) + i * 16 + j )^:3 ,',' );

   writeln(fd );

  end;

 close(fd );

end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 add_ctrl(@g_ctrl );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 write_gamma_txt('gamma.txt' );
 write_gamma_bin('gamma.bin' );

end;

{ ON_INIT }
procedure the_application.on_init;
begin
 read_gamma('gamma.txt' );

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 ewidth ,ecenter : double;

 i : int;

 pixf : pixel_formats;

 rb : renderer_base;
 r  : renderer_scanline_aa_solid;

 ras : rasterizer_scanline_aa;
 sl  : scanline_p8;

 rgba  : aggclr;
 elli  : ellipse;
 poly  : conv_stroke;
 tpoly : conv_transform;

 mtx : trans_affine;
 tas : trans_affine_skewing;
 tar : trans_affine_rotation;
 tat : trans_affine_translation;

 text  : gsv_text;
 text1 : gsv_text_outline;
 path  : path_storage;
 trans : conv_transform; 

begin
// Initialize structures
 ewidth :=_initial_width / 2 - 10;
 ecenter:=_initial_width / 2;

 pixfmt(pixf ,rbuf_window );

 rb.Construct(@pixf );
 r.Construct (@rb );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear      (@rgba );

 g_ctrl.text_size_(10.0 ,12.0 );

 ras.Construct;
 sl.Construct;

// Render the controls
 render_ctrl(@ras ,@sl ,@r ,@g_ctrl );

 ras.gamma(@g_ctrl );

// Ellipses
 elli.Construct;
 poly.Construct (@elli );
 tpoly.Construct(@poly ,_trans_affine_resizing );

 rgba.ConstrInt(0 ,0 ,0 );
 r.color_      (@rgba );

 elli.init   (ecenter ,220 ,ewidth ,15 ,100 );
 poly.width_ (2.0 );
 ras.add_path(@tpoly ,0 );

 render_scanlines(@ras ,@sl ,@r );

 elli.init   (ecenter ,220 ,11 ,11 ,100 );
 poly.width_ (2.0 );
 ras.add_path(@tpoly ,0 );

 render_scanlines(@ras ,@sl ,@r );

 rgba.ConstrInt(127 ,127 ,127 );
 r.color_      (@rgba );

 elli.init   (ecenter ,260 ,ewidth ,15 ,100 );
 poly.width_ (2.0 );
 ras.add_path(@tpoly ,0 );

 render_scanlines(@ras ,@sl ,@r );

 elli.init   (ecenter ,260 ,11 ,11 ,100 );
 poly.width_ (2.0 );
 ras.add_path(@tpoly ,0 );

 render_scanlines(@ras ,@sl ,@r );

 rgba.ConstrInt(192 ,192 ,192 );
 r.color_      (@rgba );

 elli.init   (ecenter ,300 ,ewidth ,15 ,100 );
 poly.width_ (2.0 );
 ras.add_path(@tpoly ,0 );

 render_scanlines(@ras ,@sl ,@r );

 elli.init   (ecenter ,300 ,11 ,11 ,100 );
 poly.width_ (2.0 );
 ras.add_path(@tpoly ,0 );

 render_scanlines(@ras ,@sl ,@r );

 rgba.ConstrDbl(0.0 ,0.0 ,0.4 );
 r.color_      (@rgba );

 elli.init   (ecenter ,340 ,ewidth ,15.5 ,100 );
 poly.width_ (1.0 );
 ras.add_path(@tpoly ,0 );

 render_scanlines(@ras ,@sl ,@r );

 elli.init   (ecenter ,340 ,10.5 ,10.5 ,100 );
 poly.width_ (1.0 );
 ras.add_path(@tpoly ,0 );

 render_scanlines(@ras ,@sl ,@r );

 elli.init   (ecenter ,380 ,ewidth ,15.5 ,100 );
 poly.width_ (0.4 );
 ras.add_path(@tpoly ,0 );

 render_scanlines(@ras ,@sl ,@r );

 elli.init   (ecenter ,380 ,10.5 ,10.5 ,100 );
 poly.width_ (0.4 );
 ras.add_path(@tpoly ,0 );

 render_scanlines(@ras ,@sl ,@r );

 elli.init   (ecenter ,420 ,ewidth ,15.5 ,100 );
 poly.width_ (0.1 );
 ras.add_path(@tpoly ,0 );

 render_scanlines(@ras ,@sl ,@r );

 elli.init   (ecenter ,420 ,10.5 ,10.5 ,100 );
 poly.width_ (0.1 );
 ras.add_path(@tpoly ,0 );

 render_scanlines(@ras ,@sl ,@r );

// Text
 mtx.Construct;
 tas.Construct(0.15 ,0.0 );
 mtx.Multiply (@tas );
 mtx.Multiply (_trans_affine_resizing );

 text.Construct;
 text1.Construct(@text ,@mtx );

 text.text_       ('Text 2345' );
 text.size_       (50 ,20 );
 text1.width_     (2.0 );
 text.start_point_(320 ,10 );

 rgba.ConstrDbl(0.0 ,0.5 ,0.0 );
 r.color_      (@rgba );

 ras.add_path    (@text1 ,0 );
 render_scanlines(@ras ,@sl ,@r );

// Triangled circle
 rgba.ConstrDbl(0.5 ,0.0 ,0.0 );
 r.color_      (@rgba );

 path.Construct;
 path.move_to(30 ,-1.0 );
 path.line_to(60 ,0.0 );
 path.line_to(30 ,1.0 );

 path.move_to(27 ,-1.0 );
 path.line_to(10 ,0.0 );
 path.line_to(27 ,1.0 );

 trans.Construct(@path ,@mtx );

 for i:=0 to 34 do
  begin
   tar.Construct(i / 35.0 * pi * 2.0 );
   tat.construct(400 ,130 );

   mtx.reset;
   mtx.multiply(@tar );
   mtx.multiply(@tat );
   mtx.multiply(_trans_affine_resizing );

   ras.add_path    (@trans ,0 );
   render_scanlines(@ras ,@sl ,@r );

  end;

// Free AGG resources
 ras.Destruct;
 sl.Destruct;

 poly.Destruct;
 text.Destruct;
 text1.Destruct;
 path.Destruct;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'This is another experiment with gamma correction. See also Gamma Correction. '#13 +
   'I presumed that we can do better than with a traditional power function. '#13 +
   'So, I created a special control to have an arbitrary gamma function. '#13 +
   'The conclusion is that we can really achieve a better visual result with '#13 +
   'this control, but still, in practice, the traditional power function is good '#13 +
   'enough too.'#13#13 +
   'How to play with:'#13#13 +
   'Feel free to change the gamma curve. The shape you''ll set up, will be'#13 +
   'stored in external file "gamma.txt".' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 g_ctrl.Construct(10.0 ,10.0 ,300.0 ,200.0 ,not flip_y );

 app.Construct(pix_format ,flip_y );
 app.caption_ ('Anti-Aliasing Gamma Correction (F1-Help)' );

 if app.init(500 ,400 ,window_resize ) then
  app.run;

 app.Destruct;
 g_ctrl.Destruct;

END.