//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 aa_test ;

uses
 Math ,SysUtils ,

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
 agg_scanline ,
 agg_scanline_u ,
 agg_render_scanlines ,

 agg_gamma_lut ,
 agg_gamma_functions ,
 agg_vertex_source ,
 agg_conv_stroke ,
 agg_conv_dash ,
 agg_math_stroke ,
 agg_trans_affine ,
 agg_span_gradient ,
 agg_span_interpolator_linear ,
 agg_span_gouraud_rgba ,
 agg_span_allocator ,
 agg_array ,
 agg_ellipse ;

{$I agg_mode.inc }

const
 flip_y = false;

type
 simple_vertex_source = object(vertex_source )
   m_num_vertices ,
   m_count        : unsigned;

   m_x   ,
   m_y   : array[0..7 ] of double;
   m_cmd : array[0..7 ] of unsigned;

   constructor Construct; overload;
   constructor Construct(x1 ,y1 ,x2 ,y2 : double ); overload;
   constructor Construct(x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double ); overload;

   procedure init(x1 ,y1 ,x2 ,y2 : double ); overload;
   procedure init(x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double ); overload;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 dashed_line = object
   m_ras  : rasterizer_scanline_aa_ptr;
   m_ren  : renderer_scanline_aa_solid_ptr;
   m_sl   : scanline_ptr;
   m_src  : simple_vertex_source;
   m_dash : conv_dash;

   m_stroke      ,
   m_dash_stroke : conv_stroke;

   constructor Construct(ras : rasterizer_scanline_aa_ptr; ren : renderer_scanline_aa_solid_ptr; sl : scanline_ptr );
   destructor  Destruct;

   procedure draw(x1 ,y1 ,x2 ,y2 ,line_width ,dash_length : double );

  end;

 the_application = object(platform_support )
   m_gamma : gamma_lut;

   m_slider_gamma : slider_ctrl;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ min }
function min(a ,b : int ) : int;
begin
 if a < b then
  result:=a
 else
  result:=b;

end;

{ min_d }
function min_d(a ,b : double ) : double;
begin
 if a < b then
  result:=a
 else
  result:=b;

end;

{ frand }
function frand(x : double ) : double;
begin
 result:=
  ((((Random($7fff ) shl 15 ) or Random($7fff ) ) and $3FFFFFFF) mod 1000000) * x / 1000000.0;

end;

{ calc_linear_gradient_transform }
// Calculate the affine transformation matrix for the linear gradient 
// from (x1, y1) to (x2, y2). gradient_d2 is the "base" to scale the
// gradient. Here d1 must be 0.0, and d2 must equal gradient_d2.
procedure calc_linear_gradient_transform(
           x1 ,y1 ,x2 ,y2 : double;
           mtx : trans_affine_ptr;
           gradient_d2 : double = 100.0 );
var
 dx ,dy : double;

 tas : trans_affine_scaling;
 tar : trans_affine_rotation;
 tat : trans_affine_translation;

begin
 dx:=x2 - x1;
 dy:=y2 - y1;

 tas.Construct(Sqrt(dx * dx + dy * dy ) / gradient_d2 );
 tar.Construct(ArcTan2(dy ,dx ) );
 tat.Construct(x1 + 0.5 ,y1 + 0.5 );

 mtx.reset;
 mtx.multiply(@tas );
 mtx.multiply(@tar );
 mtx.multiply(@tat );
 mtx.invert;

end;

{ fill_color_array }
// A simple function to form the gradient color array
// consisting of 3 colors, "begin", "middle", "end"
procedure fill_color_array(array_ : pod_auto_array_ptr; begin_ ,end_ : aggclr_ptr );
var
 i : unsigned;

begin
 for i:=0 to 255 do
  aggclr_ptr(array_.array_operator(i ) )^:=begin_.gradient(end_ ,i / 255.0 );

end;

{ CONSTRUCT }
constructor simple_vertex_source.Construct;
begin
 m_num_vertices:=0;
 m_count       :=0;

 m_cmd[0 ]:=path_cmd_stop;

end;

{ CONSTRUCT }
constructor simple_vertex_source.Construct(x1 ,y1 ,x2 ,y2 : double );
begin
 init(x1 ,y1 ,x2 ,y2 );

end;

{ CONSTRUCT }
constructor simple_vertex_source.Construct(x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double );
begin
 init(x1 ,y1 ,x2 ,y2 ,x3 ,y3 );

end;

{ INIT }
procedure simple_vertex_source.init(x1 ,y1 ,x2 ,y2 : double );
begin
 m_num_vertices:=2;
 m_count       :=0;

 m_x[0 ]:=x1;
 m_y[0 ]:=y1;
 m_x[1 ]:=x2;
 m_y[1 ]:=y2;

 m_cmd[0 ]:=path_cmd_move_to;
 m_cmd[1 ]:=path_cmd_line_to;
 m_cmd[2 ]:=path_cmd_stop;

end;

{ INIT }
procedure simple_vertex_source.init(x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double );
begin
 m_num_vertices:=3;
 m_count       :=0;

 m_x[0 ]:=x1;
 m_y[0 ]:=y1;
 m_x[1 ]:=x2;
 m_y[1 ]:=y2;
 m_x[2 ]:=x3;
 m_y[2 ]:=y3;
 m_x[3 ]:=0.0;
 m_y[3 ]:=0.0;
 m_x[4 ]:=0.0;
 m_y[4 ]:=0.0;

 m_cmd[0 ]:=path_cmd_move_to;
 m_cmd[1 ]:=path_cmd_line_to;
 m_cmd[2 ]:=path_cmd_line_to;
 m_cmd[3 ]:=path_cmd_end_poly or path_flags_close;
 m_cmd[4 ]:=path_cmd_stop;

end;

{ REWIND }
procedure simple_vertex_source.rewind;
begin
 m_count:=0;

end;

{ VERTEX }
function simple_vertex_source.vertex;
begin
 x^:=m_x[m_count ];
 y^:=m_y[m_count ];

 result:=m_cmd[m_count ];

 inc(m_count );

end;

{ CONSTRUCT }
constructor dashed_line.Construct;
begin
 m_ras:=ras;
 m_ren:=ren;
 m_sl :=sl;

 m_src.Construct;
 m_dash.Construct       (@m_src );
 m_stroke.Construct     (@m_src );
 m_dash_stroke.Construct(@m_dash );

end;

{ DESTRUCT }
destructor dashed_line.Destruct;
begin
 m_dash.Destruct;
 m_stroke.Destruct;
 m_dash_stroke.Destruct;

end;

{ DRAW }
procedure dashed_line.draw;
begin
 m_src.init(x1 + 0.5 ,y1 + 0.5 ,x2 + 0.5 ,y2 + 0.5 );
 m_ras.reset;

 if dash_length > 0.0 then
  begin
   m_dash.remove_all_dashes;
   m_dash.add_dash(dash_length ,dash_length );

   m_dash_stroke.width_   (line_width );
   m_dash_stroke.line_cap_(round_cap );

   m_ras.add_path(@m_dash_stroke );

  end
 else
  begin
   m_stroke.width_   (line_width );
   m_stroke.line_cap_(round_cap );

   m_ras.add_path(@m_stroke );

  end;

 render_scanlines(m_ras ,m_sl ,m_ren );

end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_gamma.Construct(1.0 );

 m_slider_gamma.Construct(3 ,3 ,480 - 3 ,8 ,not flip_y_ );

 add_ctrl(@m_slider_gamma );

 m_slider_gamma.range_(0.1 ,3.0 );
 m_slider_gamma.value_(1.6 );
 m_slider_gamma.label_('Gamma=%4.3f' );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_gamma.Destruct;

 m_slider_gamma.Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;

 ren_base : renderer_base;
 ren_sl   : renderer_scanline_aa_solid;

 ras : rasterizer_scanline_aa;
 sl  : scanline_u8;

 rgba  ,
 rgbb  : aggclr;
 gm_pw : gamma_power;

 i : int;

 dash : dashed_line;

 cx ,cy ,n ,x1 ,y1 ,x2 ,y2 : double;

 gradient_func     : gradient_x;               // The gradient function
 gradient_mtx      : trans_affine;             // Affine transformer
 span_interpolator : span_interpolator_linear; // Span interpolator
 span_allocator_   : span_allocator;           // Span Allocator
 gradient_colors   : pod_auto_array;           // The gradient colors
 span_gradient_    : span_gradient;
 ren_gradient      : renderer_scanline_aa;
 dash_gradient     : dashed_line;

 ell : ellipse;

begin
// Initialize structures
 pixfmt_bgr24_gamma(pixf ,rbuf_window ,@m_gamma );

 ren_base.Construct(@pixf );
 ren_sl.Construct  (@ren_base );

 ras.Construct;
 sl.Construct;

 rgba.ConstrInt(0 ,0 ,0 );
 ren_base.clear(@rgba );

// gamma correction
 m_gamma.gamma_(m_slider_gamma._value );

// radial line test
 dash.Construct(@ras ,@ren_sl ,@sl );

 cx:=_width / 2.0;
 cy:=_height / 2.0;

 rgba.ConstrDbl(1.0 ,1.0 ,1.0 ,0.2 );
 ren_sl.color_ (@rgba );

 for i:=180 downto 1 do
  begin
   n:=2.0 * pi * i / 180.0;

   if i < 90 then
    dash.draw(
     cx + min_d(cx ,cy ) * Sin(n ) ,
     cy + min_d(cx ,cy ) * Cos(n ) ,
     cx ,cy ,1.0 ,i )
   else
    dash.draw(
     cx + min_d(cx ,cy ) * Sin(n ) ,
     cy + min_d(cx ,cy ) * Cos(n ) ,
     cx ,cy ,1.0 ,0.0 );

  end;

// Initialize gradients
 gradient_func.Construct;
 gradient_mtx.Construct;
 span_interpolator.Construct(@gradient_mtx );
 span_allocator_.Construct;
 gradient_colors.Construct(256 ,sizeof(aggclr ) );
 span_gradient_.Construct(
                 @span_allocator_ ,
                 @span_interpolator ,
                 @gradient_func ,
                 @gradient_colors ,
                 0 ,100 );

 ren_gradient.Construct (@ren_base ,@span_gradient_ );
 dash_gradient.Construct(@ras ,@ren_gradient ,@sl );

 ell.Construct;

// Top patterns
 for i:=1 to 20 do
  begin
   rgba.ConstrDbl(1 ,1 ,1 );
   ren_sl.color_ (@rgba );

  // integral point sizes 1..20
   ell.init(20 + i * (i + 1 ) + 0.5 ,20.5 ,i / 2.0 ,i / 2.0 ,8 + i );

   ras.reset;
   ras.add_path(@ell );

   render_scanlines(@ras ,@sl ,@ren_sl );

  // fractional point sizes 0..2
   ell.init(18 + i * 4 + 0.5 ,33 + 0.5 ,i / 20.0 ,i / 20.0 ,8 );

   ras.reset;
   ras.add_path(@ell );

   render_scanlines(@ras ,@sl ,@ren_sl );

  // fractional point positioning
   ell.init(
    18 + i * 4 + (i - 1 ) / 10.0 + 0.5 ,
    27 + (i - 1 ) / 10.0 + 0.5 ,
    0.5 ,0.5 ,8 );

   ras.reset;
   ras.add_path(@ell );

   render_scanlines(@ras ,@sl ,@ren_sl );

  // integral line widths 1..20
   rgba.ConstrDbl  (1 ,1 ,1 );
   rgbb.ConstrDbl  (i mod 2 ,(i mod 3 ) * 0.5 ,(i mod 5 ) * 0.25 );
   fill_color_array(@gradient_colors ,@rgba ,@rgbb );

   x1:=20 + i* (i + 1 );
   y1:=40.5;
   x2:=20 + i * (i + 1 ) + (i - 1 ) * 4;
   y2:=100.5;

   calc_linear_gradient_transform(x1 ,y1 ,x2 ,y2 ,@gradient_mtx );
   dash_gradient.draw            (x1 ,y1 ,x2 ,y2 ,i ,0 );

   rgba.ConstrDbl  (1 ,0 ,0 );
   rgbb.ConstrDbl  (0 ,0 ,1 );
   fill_color_array(@gradient_colors ,@rgba ,@rgbb );

  // fractional line lengths H (red/blue)
   x1:=17.5 + i * 4;
   y1:=107;
   x2:=17.5 + i * 4 + i / 6.66666667;
   y2:=107;

   calc_linear_gradient_transform(x1 ,y1 ,x2 ,y2 ,@gradient_mtx );
   dash_gradient.draw            (x1 ,y1 ,x2 ,y2 ,1.0 ,0 );

  // fractional line lengths V (red/blue)
   x1:=18 + i * 4;
   y1:=112.5;
   x2:=18 + i * 4;
   y2:=112.5 + i / 6.66666667;

   calc_linear_gradient_transform(x1 ,y1 ,x2 ,y2 ,@gradient_mtx );
   dash_gradient.draw            (x1 ,y1 ,x2 ,y2 ,1.0 ,0 );

  // fractional line positioning (red)
   rgba.ConstrDbl  (1 ,0 ,0 );
   rgbb.ConstrDbl  (1 ,1 ,1 );
   fill_color_array(@gradient_colors ,@rgba ,@rgbb );

   x1:=21.5;
   y1:=120 + (i - 1 ) * 3.1;
   x2:=52.5;
   y2:=120 + (i - 1 ) * 3.1;

   calc_linear_gradient_transform(x1 ,y1 ,x2 ,y2 ,@gradient_mtx );
   dash_gradient.draw            (x1 ,y1 ,x2 ,y2 ,1.0 ,0 );

  // fractional line width 2..0 (green)
   rgba.ConstrDbl  (0 ,1 ,0 );
   rgbb.ConstrDbl  (1 ,1 ,1 );
   fill_color_array(@gradient_colors ,@rgba ,@rgbb );

   x1:=52.5;
   y1:=118 + i * 3;
   x2:=83.5;
   y2:=118 + i * 3;

   calc_linear_gradient_transform(x1 ,y1 ,x2 ,y2 ,@gradient_mtx );
   dash_gradient.draw            (x1 ,y1 ,x2 ,y2 ,2.0 - (i - 1 ) / 10.0 ,0 );

  // stippled fractional width 2..0 (blue)
   rgba.ConstrDbl  (0 ,0 ,1 );
   rgbb.ConstrDbl  (1 ,1 ,1 );
   fill_color_array(@gradient_colors ,@rgba ,@rgbb );

   x1:=83.5;
   y1:=119 + i * 3;
   x2:=114.5;
   y2:=119 + i * 3;

   calc_linear_gradient_transform(x1 ,y1 ,x2 ,y2 ,@gradient_mtx );
   dash_gradient.draw            (x1 ,y1 ,x2 ,y2 ,2.0 - (i - 1 ) / 10.0, 3.0 );

  // integral line width, horz aligned (mipmap test)
   rgba.ConstrDbl(1 ,1 ,1 );
   ren_sl.color_ (@rgba );

   if i <= 10 then
    dash.draw(
     125.5 ,119.5 + (i + 2 ) * (i / 2.0 ) ,
     135.5 ,119.5 + (i + 2 ) * (i / 2.0 ) ,i ,0.0 );


  // fractional line width 0..2, 1 px H
  //-----------------
   dash.draw(17.5 + i * 4 ,192 ,18.5 + i * 4 ,192 ,i / 10.0 ,0 );

  // fractional line positioning, 1 px H
  //-----------------
   dash.draw(
    17.5 + i * 4 + (i - 1 ) / 10.0 ,186 ,
    18.5 + i * 4 + (i - 1 ) / 10.0 ,186 ,
    1.0 ,0 );

  end;


// Triangles
 for i:=1 to 13 do
  begin
   rgba.ConstrDbl  (1 ,1 ,1 );
   rgbb.ConstrDbl  (i mod 2 ,(i mod 3 ) * 0.5 ,(i mod 5 ) * 0.25 );
   fill_color_array(@gradient_colors ,@rgba ,@rgbb );

   calc_linear_gradient_transform(
    _width - 150 ,
    _height - 20 - i * (i + 1.5 ) ,
    _width - 20,
    _height - 20 - i * (i + 1 ) ,
    @gradient_mtx );

   ras.reset;
   ras.move_to_d(_width - 150 ,_height - 20 - i * (i + 1.5 ) );
   ras.line_to_d(_width - 20  ,_height - 20 - i * (i + 1 ) );
   ras.line_to_d(_width - 20  ,_height - 20 - i * (i + 2 ) );

   render_scanlines(@ras ,@sl ,@ren_gradient );

  end;

// Reset AA Gamma and render the controls
 gm_pw.Construct(1.0 );
 ras.gamma      (@gm_pw );

 render_ctrl(@ras ,@sl ,@ren_sl ,@m_slider_gamma );

// Free AGG resources
 ras.Destruct;
 sl.Destruct;

 dash.Destruct;

 span_allocator_.Destruct;
 gradient_colors.Destruct;
 dash_gradient.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
var
 pixf : pixel_formats;

 ren_base : renderer_base;
 ren_sl   : renderer_scanline_aa_solid;

 ras : rasterizer_scanline_aa;
 sl  : scanline_u8;

 rgba ,
 rgbb ,
 rgbc : aggclr;

 i : int;

 w ,h ,t1 ,t2 ,t3 ,r ,x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double;

 ell : ellipse;
 buf : array[0..255 ] of char;

 gradient_func     : gradient_x;               // The gradient function
 gradient_mtx      : trans_affine;             // Affine transformer
 span_interpolator : span_interpolator_linear; // Span interpolator
 span_allocator_   : span_allocator;           // Span Allocator
 gradient_colors   : pod_auto_array;           // The gradient colors
 span_gradient_    : span_gradient;
 ren_gradient      : renderer_scanline_aa;
 dash_gradient     : dashed_line;

 span_gouraud : span_gouraud_rgba;
 ren_gouraud  : renderer_scanline_aa; 

begin
 RandSeed:=123;

// Initialize structures
 pixfmt_bgr24_gamma(pixf ,rbuf_window ,@m_gamma );

 ren_base.Construct(@pixf );
 ren_sl.Construct  (@ren_base );

 ras.Construct;
 sl.Construct;

 rgba.ConstrInt(0 ,0 ,0 );
 ren_base.clear(@rgba );

 w:=_width;
 h:=_height;

 ell.Construct;

// Points
 start_timer;

 for i:=0 to 20000 - 1 do
  begin
   r:=frand(20.0 ) + 1.0;

   ell.init(frand(w ) ,frand(h ) ,r / 2 ,r / 2 ,trunc(r ) + 10 );

   ras.reset;
   ras.add_path(@ell );

   render_scanlines(@ras ,@sl ,@ren_sl );

   rgba.ConstrDbl(frand(1.0 ) ,frand(1.0 ) ,frand(1.0 ) ,0.5 + frand(0.5 ) );
   ren_sl.color_ (@rgba );

  end;

 t1:=elapsed_time;

// Strokes
 gradient_func.Construct;
 gradient_mtx.Construct;
 span_interpolator.Construct(@gradient_mtx );
 span_allocator_.Construct;
 gradient_colors.Construct(256 ,sizeof(aggclr ) );
 span_gradient_.Construct(
                 @span_allocator_ ,
                 @span_interpolator ,
                 @gradient_func ,
                 @gradient_colors ,
                 0 ,100 );

 ren_gradient.Construct (@ren_base ,@span_gradient_ );
 dash_gradient.Construct(@ras ,@ren_gradient ,@sl );

 start_timer;

 for i:=0 to 2000 - 1 do
  begin
   x1:=frand(w );
   y1:=frand(h );
   x2:=x1 + frand(w * 0.5 ) - w * 0.25;
   y2:=y1 + frand(h * 0.5 ) - h * 0.25;

   rgba.ConstrDbl  (frand(1.0 ) ,frand(1.0 ) ,frand(1.0 ) ,0.5 + frand(0.5 ) );
   rgbb.ConstrDbl  (frand(1.0 ) ,frand(1.0 ) ,frand(1.0 ) ,frand(1.0 ) );
   fill_color_array(@gradient_colors ,@rgba ,@rgbb );

   calc_linear_gradient_transform(x1 ,y1 ,x2 ,y2 ,@gradient_mtx );
   dash_gradient.draw            (x1 ,y1 ,x2 ,y2 ,10.0 ,0 );

  end;

 t2:=elapsed_time;

// Gouraud triangles
 span_gouraud.Construct(@span_allocator_ );
 ren_gouraud.Construct (@ren_base ,@span_gouraud );

 start_timer;

 for i:=0 to 2000 - 1 do
  begin
   x1:=frand(w );
   y1:=frand(h );
   x2:=x1 + frand(w * 0.4 ) - w * 0.2;
   y2:=y1 + frand(h * 0.4 ) - h * 0.2;
   x3:=x1 + frand(w * 0.4 ) - w * 0.2;
   y3:=y1 + frand(h * 0.4 ) - h * 0.2;

   rgba.ConstrDbl       (frand(1.0 ) ,frand(1.0 ) ,frand(1.0 ) ,0.5 + frand(0.5 ) );
   rgbb.ConstrDbl       (frand(1.0 ) ,frand(1.0 ) ,frand(1.0 ) ,frand(1.0 ) );
   rgbc.ConstrDbl       (frand(1.0 ) ,frand(1.0 ) ,frand(1.0 ) ,frand(1.0 ) );
   span_gouraud.colors_ (@rgba ,@rgbb ,@rgbc );
   span_gouraud.triangle(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,0.0 );

   ras.add_path(@span_gouraud );

   render_scanlines(@ras ,@sl ,@ren_gouraud );

  end;

 t3:=elapsed_time;

// Test results & Update
 sprintf (@buf[0 ] ,'Points=%.2fK/sec, '#0 ,20000.0 / t1 );
 sprintf (@buf[StrLen(@buf[0 ] ) ] ,'Lines=%.2fK/sec, '#0 ,2000.0 / t2 );
 sprintf (@buf[StrLen(@buf[0 ] ) ] ,'Triangles=%.2fK/sec'#0 ,2000.0 / t3 );
 message_(@buf[0 ] );

 update_window;

// Free AGG resources
 ras.Destruct;
 sl.Destruct;

 span_allocator_.Destruct;
 gradient_colors.Destruct;
 dash_gradient.Destruct;

end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'A test of Anti-Aliasing the same as in'#13 +
   'http://homepage.mac.com/arekkusu/bugs/invariance'#13 +
   'The performance of AGG on a typical P-IV 2GHz is:'#13 +
   'Points: 37.46K/sec, Lines: 5.04K/sec, Triangles: 7.43K/sec'#13#13 +
   'How to play with:'#13#13 +
   'Click any mouse button to run the performance test.'#13+
   'Then, after you''ll see the triangles, resize the window'#13 +
   'to return to the original rendering.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. Anti-Aliasing Test (F1-Help)' );

 if app.init(480 ,350 ,window_resize ) then
  app.run;

 app.Destruct;

END.