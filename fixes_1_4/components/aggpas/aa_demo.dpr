//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 aa_demo ;

uses
 agg_basics ,
 agg_platform_support ,
 agg_math ,

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
 agg_scanline_p ,
 agg_scanline_u ,
 agg_render_scanlines ,

 agg_gamma_functions ,
 agg_path_storage ,
 agg_conv_stroke ,
 agg_vertex_source ;

{$I agg_mode.inc }

const
 flip_y = true;

type
 square = object
   m_size : double;

   constructor Construct(size : double );

   procedure draw(ras : rasterizer_scanline_aa_ptr; sl : scanline_ptr; ren : renderer_scanline_ptr; x ,y : double );

  end;

 renderer_enlarged = object(renderer_scanline )
   m_ras : rasterizer_scanline_aa;
   m_sl  : scanline_u8;
   m_ren : renderer_scanline_aa_solid_ptr;

   m_square : square;
   m_color  : aggclr;
   m_size   : double;

   constructor Construct(ren : renderer_scanline_aa_solid_ptr; size : double );
   destructor  Destruct;

   procedure color  (c : aggclr_ptr ); virtual;
   procedure prepare(u : unsigned ); virtual;
   procedure render (sl : scanline_ptr ); virtual;

  end;

 the_application = object(platform_support )
   m_x ,
   m_y : array[0..2 ] of double;

   m_dx ,
   m_dy : double;

   m_idx : int;

   m_slider1 ,
   m_slider2 : slider_ctrl;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor square.Construct;
begin
 m_size:=size;

end;

{ DRAW }
procedure square.Draw;
begin
 ras.reset;

 ras.move_to_d(x * m_size ,y * m_size );
 ras.line_to_d(x * m_size + m_size ,y * m_size );
 ras.line_to_d(x * m_size + m_size ,y * m_size + m_size );
 ras.line_to_d(x * m_size ,y * m_size + m_size );

 render_scanlines(ras ,sl ,ren );

end;

{ CONSTRUCT }
constructor renderer_enlarged.Construct;
begin
 m_square.Construct(size );

 m_ras.Construct;
 m_sl.Construct;
 m_color.Construct;

 m_ren :=ren;
 m_size:=size;

end;

{ DESTRUCT }
destructor renderer_enlarged.Destruct;
begin
 m_ras.Destruct;
 m_sl.Destruct;

end;

{ COLOR }
procedure renderer_enlarged.color;
begin
 m_color:=c^;

end;

{ PREPARE }
procedure renderer_enlarged.prepare;
begin
end;

{ RENDER }
procedure renderer_enlarged.render;
var
 y ,x ,a ,
 num_pix : int;

 num_spans : unsigned;

 span   : span_u8_ptr;
 covers : int8u_ptr;

 rgba : aggclr;

begin
 y        :=sl.y;
 num_spans:=sl.num_spans;
 span     :=sl.begin_;

 repeat
  x      :=span.x;
  covers :=span.covers;
  num_pix:=span.len;

  repeat
   a:=shr_int32(covers^ * m_color.a ,8 );

   inc(ptrcomp(covers ) ,sizeof(int8u ) );

   rgba.ConstrInt(m_color.r ,m_color.g ,m_color.b ,a );
   m_ren.color_  (@rgba );
   m_square.draw (@m_ras ,@m_sl ,m_ren ,x ,y );

   inc(x );
   dec(num_pix );

  until num_pix = 0;

  dec(num_spans );

 until num_spans = 0;

end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_slider1.Construct(80 ,10 ,600 - 10 ,19 ,not flip_y );
 m_slider2.Construct(80 ,10 + 20 ,600 - 10 ,19 + 20 ,not flip_y_ );

 m_idx:=-1;

 m_x[0 ]:=57;    m_y[0 ]:=100;
 m_x[1 ]:=369;   m_y[1 ]:=170;
 m_x[2 ]:=143;   m_y[2 ]:=310;

 add_ctrl(@m_slider1 );
 add_ctrl(@m_slider2 );

 m_slider1.range_    (8.0 ,100.0 );
 m_slider1.num_steps_(23 );
 m_slider1.value_    (32.0 );

 m_slider2.range_(0.1 ,3.0 );
 m_slider2.value_(1.0 );

 m_slider1.label_('Pixel size=%1.0f' );
 m_slider2.label_('Gamma=%4.3f' );

 m_slider1.no_transform;
 m_slider2.no_transform;

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_slider1.Destruct;
 m_slider2.Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 size_mul : int;

 pixf : pixel_formats;

 rb  : renderer_base;
 ren : renderer_scanline_aa_solid;
 ras : rasterizer_scanline_aa;
 sl  : scanline_u8;

 rgba  : aggclr;
 gm_no : vertex_source;
 gm_pw : gamma_power;

 ren_en : renderer_enlarged;

 ps : path_storage;
 pg : conv_stroke;

begin
// Initialize structures
 pixfmt_bgr24(pixf ,rbuf_window );

 rb.Construct (@pixf );
 ren.Construct(@rb );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear(@rgba );

 sl.Construct;
 ras.Construct;

// Draw Zoomed Triangle
 size_mul:=trunc(m_slider1._value );

 gm_pw.Construct(m_slider2._value );
 ras.gamma      (@gm_pw );

 ren_en.Construct(@ren ,size_mul );

 rgba.ConstrInt(0 ,0 ,0 ,255 );
 ren_en.color  (@rgba );

 ras.reset;
 ras.move_to_d(m_x[0 ] / size_mul ,m_y[0 ] / size_mul );
 ras.line_to_d(m_x[1 ] / size_mul ,m_y[1 ] / size_mul );
 ras.line_to_d(m_x[2 ] / size_mul ,m_y[2 ] / size_mul );

 render_scanlines(@ras ,@sl ,@ren_en );

// Draw final triangle bottom-left
 rgba.ConstrInt(0 ,0 ,0 );
 ren.color_    (@rgba );

 render_scanlines(@ras ,@sl ,@ren );

// Draw The Supposed Triangle over
 gm_no.Construct;
 ras.gamma(@gm_no );

 ps.Construct;
 pg.Construct(@ps );
 pg.width_   (2.0 );

 rgba.ConstrInt(0 ,150 ,160 ,200 );
 ren.color_    (@rgba );

 ps.remove_all;
 ps.move_to(m_x[0 ] ,m_y[0 ] );
 ps.line_to(m_x[1 ] ,m_y[1 ] );

 ras.add_path(@pg );

 render_scanlines(@ras ,@sl ,@ren );

 ps.remove_all;
 ps.move_to(m_x[1 ] ,m_y[1 ] );
 ps.line_to(m_x[2 ] ,m_y[2 ] );

 ras.add_path(@pg );

 render_scanlines(@ras ,@sl ,@ren );

 ps.remove_all;
 ps.move_to(m_x[2 ] ,m_y[2 ] );
 ps.line_to(m_x[0 ] ,m_y[0 ] );

 ras.add_path(@pg );

 render_scanlines(@ras ,@sl ,@ren );

// Render the controls
 render_ctrl(@ras ,@sl ,@ren ,@m_slider1 );
 render_ctrl(@ras ,@sl ,@ren ,@m_slider2 );

// Free AGG resources
 ras.Destruct;
 sl.Destruct;

 pg.Destruct;
 ps.Destruct;

 ren_en.Destruct;

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
     m_x[0 ]:= dx;
     m_y[0 ]:= dy;

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
     if Sqrt((x - m_x[ i ] ) * (x - m_x[i ] ) + (y - m_y[i ] ) * (y - m_y[i ] ) ) < 10.0 then
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
begin
 if key = key_f1 then
  message_(
   'Demonstration of the Anti-Aliasing principle with Subpixel Accuracy.'#13 +
   'The triangle is rendered two times, with its "natural" size (at the bottom-left)'#13 +
   'and enlarged. To draw the enlarged version there was a special scanline'#13 +
   'renderer written (see class renderer_enlarged in the source code).'#13#13+
   'How to play with:'#13#13 +
   'You can drag the whole triangle as well as each vertex of it.'#13 +
   'Also change "Gamma" to see how it affects the quality of Anti-Aliasing.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. Anti-Aliasing Demo (F1-Help)' );

 if app.init(600 ,400 ,window_resize ) then
  app.run;

 app.Destruct;

END.