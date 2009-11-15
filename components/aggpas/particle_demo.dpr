//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 particle_demo ;

{DEFINE DISORDER }

uses
 Math ,
 agg_basics ,
 agg_platform_support ,
 agg_ctrl ,
 agg_slider_ctrl ,
 agg_cbox_ctrl ,
 agg_renderer_base ,
 agg_rendering_buffer ,
 agg_rasterizer_scanline_aa ,
 agg_scanline_u ,
 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,
 agg_pixfmt_rgba ,
 agg_renderer_scanline ,
 agg_path_storage ,
 agg_conv_transform ,
 agg_bounding_rect ,
 agg_span_allocator ,
 agg_span_gradient ,
 agg_span_interpolator_linear ,
 agg_rasterizer_outline_aa ,
 agg_rendering_buffer_dynarow ,
 agg_ellipse ,
 agg_array ,
 agg_gsv_text ,
 agg_conv_stroke ,
 agg_render_scanlines ,
 agg_trans_affine ,
 agg_math ;

{$I agg_mode.inc }

const
 flip_y = true;

var
 g_rasterizer : rasterizer_scanline_aa;

 g_scanline : scanline_u8;

 g_path : path_storage;

 g_npaths : unsigned = 0;

 g_x1 : double = 0;
 g_y1 : double = 0;
 g_x2 : double = 0;
 g_y2 : double = 0;

 g_base_dx : double = 0;
 g_base_dy : double = 0;

 g_scale : double = 1.0;

 g_skew_x : double = 0;
 g_skew_y : double = 0;

 g_nclick : int = 0;

 g_cx ,g_cy ,g_dx ,g_dy ,g_radius : array[0..999 ] of double;

 g_color1 ,g_color2 ,g_color3 : array[0..999 ] of aggclr;

 g_gradients : array[0..999 ] of pod_auto_array;

 g_angle : double = 0;

 g_center : double = 0;

 g_dc : double = 0.5;

 g_cache : array[0..999 ] of rendering_buffer_dynarow_ptr;

type
 gradient_tricolor = object(array_base )
  private
   m_c1 ,
   m_c2 ,
   m_c3 ,
   m_rc : aggclr;

  public
   constructor Construct(c1 ,c2 ,c3 : aggclr );

   function  size : unsigned; virtual;
   function  array_operator(i : unsigned ) : pointer; virtual;

   procedure colors(c1 ,c2 ,c3 : aggclr );

  end;

 the_application = object(platform_support )
  private
   m_particles ,
   m_speed     : slider_ctrl;
   m_use_cache ,
   m_run       : cbox_ctrl;

   m_run_flag       ,
   m_use_cache_flag ,
   m_first_time     : boolean;

   m_particles_value ,
   m_speed_value     : double;

  public
   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;
   procedure on_idle; virtual;
   procedure on_ctrl_change; virtual;

  end;

{ CONSTRUCT }
constructor gradient_tricolor.Construct(c1 ,c2 ,c3 : aggclr );
begin
 m_c1:=c1;
 m_c2:=c2;
 m_c3:=c3;

end;

{ SIZE }
function gradient_tricolor.size : unsigned;
begin
 result:=256;

end;

{ ARRAY_OPERATOR }
function gradient_tricolor.array_operator(i : unsigned ) : pointer;
begin
 if i <= 127 then
  begin
   i:=i * 2;

   m_rc.r:=int8u((((m_c2.r - m_c1.r ) * int(i ) ) + (m_c1.r shl 8 ) ) shr 8 );
   m_rc.g:=int8u((((m_c2.g - m_c1.g ) * int(i ) ) + (m_c1.g shl 8 ) ) shr 8 );
   m_rc.b:=int8u((((m_c2.b - m_c1.b ) * int(i ) ) + (m_c1.b shl 8 ) ) shr 8 );
   m_rc.a:=int8u((((m_c2.a - m_c1.a ) * int(i ) ) + (m_c1.a shl 8 ) ) shr 8 );

  end
 else
  begin
   i:=(i - 127 ) * 2;

   m_rc.r:=int8u((((m_c3.r - m_c2.r ) * int(i ) ) + (m_c2.r shl 8 ) ) shr 8 );
   m_rc.g:=int8u((((m_c3.g - m_c2.g ) * int(i ) ) + (m_c2.g shl 8 ) ) shr 8 );
   m_rc.b:=int8u((((m_c3.b - m_c2.b ) * int(i ) ) + (m_c2.b shl 8 ) ) shr 8 );
   m_rc.a:=int8u((((m_c3.a - m_c2.a ) * int(i ) ) + (m_c2.a shl 8 ) ) shr 8 );

  end;

 result:=@m_rc;

end;

{ COLORS }
procedure gradient_tricolor.colors(c1 ,c2 ,c3 : aggclr );
begin
 m_c1:=c1;
 m_c2:=c2;
 m_c3:=c3;

end;

{ CONSTRUCT }
constructor the_application.Construct;
var
 i : int;
 c : aggclr;

begin
 inherited Construct(format_ ,flip_y_ );

 m_particles.Construct(5   ,5      ,300 ,12      ,not flip_y_ );
 m_speed.Construct    (5   ,5 + 15 ,300 ,12 + 15 ,not flip_y_ );
 m_use_cache.Construct(320 ,5      ,'Use Bitmap Cache' ,not flip_y_ );
 m_run.Construct      (320 ,5 + 15 ,'Start the Universe!' ,not flip_y_ );

 m_run_flag      :=true;
 m_use_cache_flag:=false;

 i:=0;

 while i < 1000 do
  begin
   g_cache[i ]:=NIL;

   g_gradients[i ].Construct(256 ,sizeof(aggclr ) );

   inc(i );

  end;

 add_ctrl(@m_particles );

 m_particles.range_(10 ,1000 );
 m_particles.value_(200 );
 m_particles.label_('Number of Particles=%.0f' );
 m_particles.no_transform;

 m_particles_value:=m_particles._value;

 add_ctrl(@m_speed );

 m_speed.range_(0.025 ,2.0 );
 m_speed.value_(1.0 );
 m_speed.label_('Dark Energy=%.3f' );
 m_speed.no_transform;

 m_speed_value:=m_speed._value;
 m_first_time :=true;

 add_ctrl(@m_use_cache );

 c.ConstrDbl                (1   ,1 ,1 );
 m_use_cache.text_color_    (@c );
 c.ConstrDbl                (1   ,1 ,1 );
 m_use_cache.inactive_color_(@c );
 c.ConstrDbl                (0.8 ,0 ,0 );
 m_use_cache.active_color_  (@c );

 m_use_cache.no_transform;

 add_ctrl(@m_run );

 c.ConstrDbl          (1   ,1 ,1 );
 m_run.text_color_    (@c );
 c.ConstrDbl          (1   ,1 ,1 );
 m_run.inactive_color_(@c );
 c.ConstrDbl          (0.8 ,0 ,0 );
 m_run.active_color_  (@c );

 m_run.no_transform;
 m_run.status_(true );

end;

{ DESTRUCT }
destructor the_application.Destruct;
var
 i : int;

begin
 inherited Destruct;

 m_particles.Destruct;
 m_speed.Destruct;
 m_use_cache.Destruct;
 m_run.Destruct;

 i:=0;

 while i < 1000 do
  begin
   if g_cache[i ] <> NIL then
    dispose(g_cache[i ] ,Destruct );

   g_gradients[i ].Destruct;

   inc(i );

  end;

end;

{ RENDER_PARTICLE }
procedure render_particle(ren : renderer_base_ptr; i : unsigned; x ,y : double );
var
 grm : trans_affine;
 grf : gradient_circle;
 ell : ellipse;

 sa : span_allocator;
 sg : span_gradient;
 rg : renderer_scanline_aa;

 inter : span_interpolator_linear;

 r : double;

 tas : trans_affine_scaling;
 tat : trans_affine_translation;

begin
 grm.Construct;
 grf.Construct;
 ell.Construct;
 sa.Construct;
 inter.Construct(@grm );
 sg.Construct   (@sa ,@inter ,@grf ,@g_gradients[i ] ,0 ,10 );
 rg.Construct   (ren ,@sg );

 r:=g_radius[i ];

 grm.reset;
 tas.Construct(r / 10.0 );
 grm.multiply (@tas );
 tat.Construct(x ,y );
 grm.multiply (@tat );
 grm.invert;

 ell.init(x ,y ,r ,r ,32 );

 g_rasterizer.add_path(@ell );

 render_scanlines(@g_rasterizer ,@g_scanline ,@rg );

 sa.Destruct;
 sg.Destruct;

end;

{ ON_INIT }
procedure the_application.on_init;
var
 n ,i ,j ,d : unsigned;

 component ,da : int;

 divisor ,angle ,speed ,k : double;

 grc : gradient_tricolor;

 gr : pod_auto_array_ptr;

 pixf : pixel_formats;
 ren  : renderer_base;

begin
 n:=Trunc(m_particles._value );

 srand(123 );

 if m_use_cache._status then
  divisor:=250.0
 else
  divisor:=500.0;

 if m_first_time then
  begin
   i:=0;

   while i < n do
    begin
     g_cx[i ]:=_width / 2 {$IFDEF DISORDER } + rand mod 10 - 5 {$ENDIF };
     g_cy[i ]:=_height / 2 {$IFDEF DISORDER } + rand mod 10 - 5 {$ENDIF };

    {$IFDEF DISORDER }
     if rand and 1 <> 0 then
      g_dx[i ]:=(rand mod 5000 + 1000 ) / divisor
     else
      g_dx[i ]:=-(rand mod 5000 + 1000 ) / divisor;

     g_dy[i ]:=g_dx[i ];

     if rand and 1 <> 0 then
      g_dy[i ]:=-g_dy[i ];

    //---
     angle:=(rand mod 10000 ) / 10000.0 * (2.0 / 8.0 ) * pi;
     da   :=rand and 3;
     angle:=angle + 2.0 * pi * da / 4.0 + (pi / 10.0 );

    {$ELSE }
     angle:=(rand mod 10000 ) / 10000.0 * 2.0 * pi;

    {$ENDIF }

     speed:=((rand mod 10000 ) mod 5000 + 1000.0 ) / divisor;

     g_dx[i ]:=Cos(angle ) * speed;
     g_dy[i ]:=Sin(angle ) * speed;

     k:=1.0 - n / 2000.0;

     g_radius[i ]:=(rand mod 30 + 15 ) * k;

     g_color1[i ].ConstrInt(rand and $FF ,rand and $FF ,rand and $FF ,0 );
     g_color2[i ].ConstrInt(rand and $FF ,rand and $FF ,rand and $FF ,255 );

     component:=rand mod 3;

     if component = 0 then
      g_color2[i ].r:=255;

     if component = 1 then
      g_color2[i ].g:=255;

     if component = 2 then
      g_color2[i ].b:=255;

    {$IFDEF DISORDER }
     g_color1[i ]  :=g_color2[i ];
     g_color1[i ].a:=0;

    {$ENDIF }

     g_color3[i ].ConstrInt(rand and $FF ,rand and $FF ,rand and $FF ,0 );

     grc.Construct(g_color1[i ] ,g_color2[i ] ,g_color3[i ] );

     gr:=@g_gradients[i ];
     j :=0;

     while j < gr.size do
      begin
       move(
        grc.array_operator(j )^ ,
        gr.array_operator(j )^ ,
        sizeof(aggclr ) );

       inc(j );

      end;

     inc(i );

    end;

   m_first_time:=false;

  end;

 if m_use_cache._status then
  begin
   i:=0;

   while i < 1000 do
    begin
     if g_cache[i ] <> NIL then
      dispose(g_cache[i ] ,Destruct );

     g_cache[i ]:=NIL;

     inc(i );

    end;

   i:=0;

   while i < n do
    begin
     d:=Trunc(g_radius[i ] ) * 2;

     new(g_cache[i ] ,Construct(d ,d ,d * 4 ) );

     pixfmt_alpha_blend_rgba(pixf ,g_cache[i ] ,bgra_order );

     ren.Construct(@pixf );

     render_particle(@ren ,i ,d / 2 ,d / 2 );

     inc(i );

    end;

  end;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 i : unsigned;

 width ,height ,x ,y : int;

 pf ,pf_pre ,pixf : pixel_formats;

 r ,r_pre : renderer_base;

 rs : renderer_scanline_aa_solid;

 rgba : aggclr;

 n  : unsigned;
 tm : double;

 buf : array[0..63 ] of char;

 t  : gsv_text;
 pt : conv_stroke;

begin
 width :=rbuf_window._width;
 height:=rbuf_window._height;

// Initialize structures
 pixfmt_bgra32(pf ,rbuf_window );

 r.Construct (@pf );
 rs.Construct(@r );

 g_rasterizer.clip_box(0 ,0 ,width ,height );

 rgba.ConstrDbl(0 ,0 ,0 );
 r.clear(@rgba );

// Render
 if m_run._status then
  begin
   start_timer;

   n:=Trunc(m_particles._value );

   if m_use_cache._status then
    begin
     pixfmt_bgra32_pre(pf_pre ,rbuf_window );
     r_pre.Construct  (@pf_pre );

     i:=0;

     while i < n do
      begin
       if g_cache[i ] <> NIL then
        begin
         pixfmt_alpha_blend_rgba(pixf ,g_cache[i ] ,bgra_order );

         x:=Trunc(g_cx[i ] - g_radius[i ] ) + 1;
         y:=Trunc(g_cy[i ] - g_radius[i ] ) + 1;

         r_pre.blend_from(@pixf ,0 ,x ,y ,255 );

        end;

       inc(i );

      end;

    end
   else
    begin
     i:=0;

     while i < n do
      begin
       render_particle(@r ,i ,g_cx[i ] ,g_cy[i ] );

       inc(i );

      end;

    end;

   tm:=elapsed_time;

   t.Construct;
   t.size_(10.0 );

   pt.Construct(@t );
   pt.width_   (1.5 );

   sprintf(@buf[0 ] ,'%6.1f fps' ,1000.0 / tm );

   t.start_point_(10.0 ,35.0 );
   t.text_       (@buf[0 ] );

   g_rasterizer.add_path(@pt );

   rgba.ConstrDbl(1 ,1 ,1 );
   rs.color_     (@rgba );

   render_scanlines(@g_rasterizer ,@g_scanline ,@rs );

   t.Destruct;
   pt.Destruct;

  end;

// Render the controls
 render_ctrl(@g_rasterizer ,@g_scanline ,@rs ,@m_particles );
 render_ctrl(@g_rasterizer ,@g_scanline ,@rs ,@m_speed );
 render_ctrl(@g_rasterizer ,@g_scanline ,@rs ,@m_use_cache );
 render_ctrl(@g_rasterizer ,@g_scanline ,@rs ,@m_run );

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
begin
 if (flags and mouse_left <> 0 ) or
    (flags and mouse_right <> 0 ) then
  force_redraw;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 on_mouse_button_down(x ,y ,flags );

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'Demonstration of using the bitmap cache.'#13#13 +
   'Cached bitmaps are descended from AGG renderer_base    '#13 +
   'and on_draw method just alpha blended to the scene.' );

end;

{ ON_IDLE }
procedure the_application.on_idle;
var
 n ,i : unsigned;

 x1 ,y1 ,x2 ,y2 ,dx ,dy ,cx ,cy ,max_dist ,d : double;

begin
 n:=Trunc(m_particles._value );

 x1:=-100;
 y1:=-100;
 x2:=_width + 100;
 y2:=_height + 100;
 dx:=Cos(g_angle ) * g_center;
 dy:=Sin(g_angle ) * g_center;
 cx:=dx + _width / 2;
 cy:=dy + _height / 2;

 max_dist:=Sqrt(_width * _width / 2 + _height * _height / 2 );

 g_angle :=g_angle + 5.0 * pi / 180.0;
 g_center:=g_center + g_dc;

 if g_center > max_dist / 2 then
  begin
   g_center:=max_dist / 2;
   g_dc    :=-g_dc;

  end;

 if g_center < 10.0 then
  begin
   g_center:=10.0;
   g_dc    :=-g_dc;

  end;

 i:=0;

 while i < n do
  begin
   g_cx[i ]:=g_cx[i ] + g_dx[i ] * m_speed._value;
   g_cy[i ]:=g_cy[i ] + g_dy[i ] * m_speed._value;

   d:=calc_distance(g_cx[i ] ,g_cy[i ] ,cx ,cy );

   if d > max_dist then
    begin
     g_cx[i ]:=cx;
     g_cy[i ]:=cy;

    end;

  {$IFDEF DISORDER }
  {
   if g_cx[i ] < x1 then
    begin
     g_cx[i ]:=cx;
     g_cy[i ]:=cy;

    end;

   if g_cx[i ] > x2 then
    begin
     g_cx[i ]:=cx;
     g_cy[i ]:=cy;

    end;

   if g_cy[i ] < y1 then
    begin
     g_cx[i ]:=cx;
     g_cy[i ]:=cy;

    end;

   if g_cy[i ] > y2 then
    begin
     g_cx[i ]:=cx;
     g_cy[i ]:=cy;

    end;

  {}
   if g_cx[i ] < x1 then
    begin
     g_cx[i ]:=x1;
     g_dx[i ]:=-g_dx[i ];

    end;

   if g_cx[i ] > x2 then
    begin
     g_cx[i ]:=x2;
     g_dx[i ]:=-g_dx[i];

    end;

   if g_cy[i ] < y1 then
    begin
     g_cy[i ]:=y1;
     g_dy[i ]:=-g_dy[i ];

    end;

   if g_cy[i ] > y2 then
    begin
     g_cy[i ]:=y2;
     g_dy[i ]:=-g_dy[i ];

    end;

  {}
  {$ENDIF }

   inc(i );

  end;

 force_redraw;

end;

{ ON_CTRL_CHANGE }
procedure the_application.on_ctrl_change;
var
 stop ,over : boolean;

begin
 if m_run_flag <> m_run._status then
  begin
   wait_mode_(not m_run._status );

   m_run_flag:=m_run._status;

   if m_run_flag then
    begin
     m_first_time:=true;

     on_init;

    end;

  end
 else
  begin
   stop:=false;
   over:=false;

   if m_use_cache._status <> m_use_cache_flag then
    begin
     m_use_cache_flag:=m_use_cache._status;

     stop:=false;
     over:=true;

    end;

   if m_particles._value <> m_particles_value then
    begin
     m_particles_value:=m_particles._value;

     stop:=true;
     over:=false;

    end;

   if m_speed._value <> m_speed_value then
    begin
     m_speed_value:=m_speed._value;

     stop:=false;
     over:=false;

    end;

   if stop then
    begin
     wait_mode_   (true );
     m_run.status_(false );

    end
   else
    if over then
     on_init;

  end;

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgra32 ,flip_y );
 app.caption_ ('Renesis project -- Particles demo. (F1-Help)' );

 g_scanline.Construct;
 g_rasterizer.Construct;
 g_path.Construct;
 
 if app.init(600 ,500 ,window_resize ) then
  begin
   app.wait_mode_(false );
   app.run;

  end;

 g_rasterizer.Destruct;
 g_scanline.Destruct;
 g_path.Destruct;

 app.Destruct;

END.