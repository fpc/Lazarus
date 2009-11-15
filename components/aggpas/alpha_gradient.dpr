//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 alpha_gradient ;

uses
 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,

 agg_ctrl ,
 agg_spline_ctrl ,

 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_u ,
 agg_render_scanlines ,

 agg_array ,
 agg_span_gradient ,
 agg_span_gradient_alpha ,
 agg_span_interpolator_linear ,
 agg_span_converter ,
 agg_span_allocator ,
 agg_ellipse ,
 agg_vcgen_stroke ,
 agg_trans_affine ,
 agg_math ;

{$I agg_mode.inc }

const
 flip_y = true;

type
 the_application = object(platform_support )
   m_x   ,
   m_y   : array[0..2 ] of double;
   m_dx  ,
   m_dy  : double;
   m_idx : int;

   m_alpha : spline_ctrl;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure fill_color_array(array_ : pod_auto_array_ptr; begin_ ,middle_ ,end_ : aggclr_ptr );

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

 m_alpha.Construct(2 ,2 ,200 ,30 ,6 ,not flip_y_ );

 m_idx:=-1;

 m_x[0 ]:=257;
 m_y[0 ]:=60;
 m_x[1 ]:=369;
 m_y[1 ]:=170;
 m_x[2 ]:=143;
 m_y[2 ]:=310;

 m_alpha.point_(0 ,0.0       ,0.0);
 m_alpha.point_(1 ,1.0 / 5.0 ,1.0 - 4.0 / 5.0 );
 m_alpha.point_(2 ,2.0 / 5.0 ,1.0 - 3.0 / 5.0 );
 m_alpha.point_(3 ,3.0 / 5.0 ,1.0 - 2.0 / 5.0 );
 m_alpha.point_(4 ,4.0 / 5.0 ,1.0 - 1.0 / 5.0 );
 m_alpha.point_(5 ,1.0       ,1.0 );

 m_alpha.update_spline;

 add_ctrl(@m_alpha );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_alpha.Destruct;

end;

{ FILL_COLOR_ARRAY }
procedure the_application.fill_color_array;
var
 i : unsigned;

begin
 i:=0;

 while i < 128 do
  begin
   aggclr_ptr(array_.array_operator(i ) )^:=
    begin_.gradient(middle_ ,i / 128.0 );

   inc(i );

  end;

 while i < 256 do
  begin
   aggclr_ptr(array_.array_operator(i ) )^:=
    middle_.gradient(end_ ,(i - 128 ) / 128.0 );

   inc(i );

  end;

end;

{ ON_DRAW }
procedure the_application.on_draw;
type
// Gradient shape function (linear, radial, custom, etc)
 gradient_func_type = gradient_circle;

// Alpha gradient shape function (linear, radial, custom, etc)
 gradient_alpha_func_type = gradient_xy;

// Span interpolator. This object is used in all span generators 
// that operate with transformations during iterating of the spans,
// for example, image transformers use the interpolator too.
 interpolator_type = span_interpolator_linear;

// Span allocator is an object that allocates memory for
// the array of colors that will be used to render the
// color spans. One object can be shared between different
// span generators.
 span_allocator_type = span_allocator;

// Gradient colors array adaptor
 gradient_colors_type = pod_auto_array;

// Finally, the gradient span generator working with the color_type
// color type.
 span_gradient_type = span_gradient;

// Gradient alpha array adaptor
 gradient_alpha_type = pod_auto_array;

// The alpha gradient span converter working with the color_type
// color type.
 span_gradient_alpha_type = span_gradient_alpha;

// Span converter type
 span_conv_type = span_converter;

// The gradient (plus span converter) renderer type
 renderer_gradient_type = renderer_scanline_aa;

var
 pf : pixel_formats;

 rgba ,
 rgbb ,
 rgbc : aggclr;

 ren_base  : renderer_base;
 ren_solid : renderer_scanline_aa_solid;

 sl  : scanline_u8;
 ras : rasterizer_scanline_aa;

 ell : ellipse;

 i ,w ,h : unsigned;

 parallelogram : array[0..5 ] of double;

 tas : trans_affine_scaling;
 tar : trans_affine_rotation;
 tat : trans_affine_translation;

 stroke : vcgen_stroke;

// The gradient objects declarations
 gradient_func           : gradient_func_type;       // The gradient function
 alpha_func              : gradient_alpha_func_type; // The gradient function
 gradient_mtx            ,                           // Gradient affine transformer
 alpha_mtx               : trans_affine;             // Alpha affine transformer
 span_interpolator       ,                           // Span gradient interpolator
 span_interpolator_alpha : interpolator_type;        // Span alpha interpolator
 span_allocator          : span_allocator_type;      // Span Allocator
 color_array             : gradient_colors_type;     // The gradient colors

 span_gradient       : span_gradient_type;
 alpha_array         : gradient_alpha_type;
 span_gradient_alpha : span_gradient_alpha_type;
 span_conv           : span_conv_type;
 ren_gradient        : renderer_gradient_type;

begin
// Initialize structures
 pixfmt_bgr24(pf ,rbuf_window );

 ren_base.Construct (@pf );
 ren_solid.Construct(@ren_base );

 rgba.ConstrDbl(1 ,1 ,1 );
 ren_base.clear(@rgba );

 sl.Construct;
 ras.Construct;

// Draw some background
 ell.Construct;

 RandSeed:=1234;

 w:=trunc(_width );
 h:=trunc(_height );

 for i:=0 to 99 do
  begin
   ell.init(
    Random($7fff ) mod w ,Random($7fff ) mod h ,
    Random($7fff ) mod 60 + 5 ,Random($7fff ) mod 60 + 5 ,50 );

   rgba.ConstrDbl(
    Random($7fff ) / $7fff ,
    Random($7fff ) / $7fff ,
    Random($7fff ) / $7fff ,
    Random($7fff ) / $7fff / 2.0 );

   ren_solid.color_(@rgba );
   ras.add_path    (@ell);
   render_scanlines(@ras ,@sl ,@ren_solid );

  end;

 parallelogram[0 ]:=m_x[0 ];
 parallelogram[1 ]:=m_y[0 ];
 parallelogram[2 ]:=m_x[1 ];
 parallelogram[3 ]:=m_y[1 ];
 parallelogram[4 ]:=m_x[2 ];
 parallelogram[5 ]:=m_y[2 ];

// The gradient objects initializations
 gradient_func.Construct;
 alpha_func.Construct;
 gradient_mtx.Construct;
 alpha_mtx.Construct;
 span_interpolator.Construct      (@gradient_mtx );
 span_interpolator_alpha.Construct(@alpha_mtx );
 span_allocator.Construct;
 color_array.Construct(256 ,sizeof(aggclr ) );

// Initialize the gradient span itself.
// The last two arguments are so called "d1" and "d2"
// defining two distances in pixels, where the gradient starts
// and where it ends. The actual meaning of "d1" and "d2" depands
// on the gradient function.
 span_gradient.Construct(
  @span_allocator ,
  @span_interpolator ,
  @gradient_func ,
  @color_array ,0 ,150 );

 alpha_array.Construct(256 ,sizeof(int8u ) );

 span_gradient_alpha.Construct(
  @span_interpolator_alpha ,
  @alpha_func ,
  @alpha_array ,0 ,100 );

// Span converter initialization
 span_conv.Construct(@span_gradient ,@span_gradient_alpha );

// The gradient renderer
 ren_gradient.Construct(@ren_base ,@span_conv );

// Finally we can draw a circle
 tas.Construct(0.75 ,1.2 );
 tar.Construct(pi / 3.0 );
 tat.Construct(_width / 2 ,_height / 2 );

 gradient_mtx.multiply(@tas );
 gradient_mtx.multiply(@tar );
 gradient_mtx.multiply(@tat );
 gradient_mtx.invert;

 alpha_mtx.parl_to_rect(@parallelogram ,-100 ,-100 ,100 ,100 );

 rgba.ConstrDbl(0    ,0.19 ,0.19 );
 rgbb.ConstrDbl(0.7  ,0.7  ,0.19 );
 rgbc.ConstrDbl(0.31 ,0    ,0 );

 fill_color_array(@color_array ,@rgba ,@rgbb ,@rgbc );

// Fill Alpha array
 for i:=0 to 255 do
  int8u_ptr(alpha_array.array_operator(i ) )^:=
   int8u(trunc(m_alpha._value(i / 255.0 ) * base_mask ) );

 ell.init(_width / 2 ,_height / 2 ,150 ,150 ,100 );

 ras.add_path    (@ell);
 render_scanlines(@ras ,@sl ,@ren_gradient );

// Draw the control points and the parallelogram
 rgba.ConstrDbl  (0 ,0.4 ,0.4 ,0.31 );
 ren_solid.color_(@rgba );

 ell.init        (m_x[0 ] ,m_y[0 ] ,5 ,5 ,20 );
 ras.add_path    (@ell);
 render_scanlines(@ras ,@sl ,@ren_solid );

 ell.init        (m_x[1 ] ,m_y[1 ] ,5 ,5 ,20 );
 ras.add_path    (@ell );
 render_scanlines(@ras ,@sl ,@ren_solid );

 ell.init        (m_x[2 ] ,m_y[2 ] ,5 ,5 ,20 );
 ras.add_path    (@ell );
 render_scanlines(@ras ,@sl ,@ren_solid );

 stroke.Construct;
 stroke.add_vertex(m_x[0 ] ,m_y[0 ] ,path_cmd_move_to );
 stroke.add_vertex(m_x[1 ] ,m_y[1 ] ,path_cmd_line_to );
 stroke.add_vertex(m_x[2 ] ,m_y[2 ] ,path_cmd_line_to );
 stroke.add_vertex(m_x[0 ] + m_x[2 ] - m_x[1 ] ,m_y[0 ] + m_y[2 ] - m_y[1 ] ,path_cmd_line_to );
 stroke.add_vertex(0 ,0 ,path_cmd_end_poly or path_flags_close );

 rgba.ConstrDbl  (0 ,0 ,0 );
 ren_solid.color_(@rgba );
 ras.add_path    (@stroke);
 render_scanlines(@ras ,@sl ,@ren_solid );

// Render the controls
 render_ctrl(@ras ,@sl ,@ren_solid ,@m_alpha );

// Free AGG resources
 sl.Destruct;
 ras.Destruct;

 span_allocator.Destruct;
 color_array.Destruct;
 alpha_array.Destruct;

 stroke.Destruct;

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

begin
 if flags and mouse_left <> 0 then
  begin
   i:=0;

   while i < 3 do
    begin
     if Sqrt((x - m_x[i ] ) * (x - m_x[i ] ) + (y - m_y[i ] ) * (y - m_y[i ] ) ) < 10.0 then
      begin
       m_dx :=x - m_x[i ];
       m_dy :=y - m_y[i ];
       m_idx:=i;

       break;

      end;

     inc(i );

    end;

   if i = 3 then
    if point_in_triangle(
        m_x[0 ] ,m_y[0 ] ,
        m_x[1 ] ,m_y[1 ] ,
        m_x[2 ] ,m_y[2 ] ,x ,y ) then
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
  key_left  : dx:=-0.1;
  key_right : dx:= 0.1;
  key_up    : dy:= 0.1;
  key_down  : dy:=-0.1;

 end;

 m_x[0 ]:=m_x[0 ] + dx;
 m_y[0 ]:=m_y[0 ] + dy;
 m_x[1 ]:=m_x[1 ] + dx;
 m_y[1 ]:=m_y[1 ] + dy;

 force_redraw;

 if key = key_f1 then
  message_(
   'The demo shows how to combine any span generator with alpha-channel gradient.'#13#13 +
   'How to play with:'#13#13 +
   'Use the mouse to move the parallelogram around.'#13 +
   'Use the arrow keys to move the parallelogram very precisely.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. Alpha channel gradient (F1-Help)' );

 if app.init(400 ,320 ,window_resize ) then
  app.run;

 app.Destruct;

END.