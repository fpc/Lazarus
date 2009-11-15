{mac_copy:compositing.bmp}
//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 compositing ;

uses
 Math ,SysUtils ,

 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgba ,

 agg_ctrl ,
 agg_rbox_ctrl ,
 agg_slider_ctrl ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_u ,
 agg_render_scanlines ,

 agg_math ,
 agg_array ,
 agg_ellipse ,
 agg_conv_stroke ,
 agg_trans_affine ,
 agg_rounded_rect ,
 agg_span_gradient ,
 agg_span_allocator ,
 agg_gsv_text ,
 agg_span_interpolator_linear ,
 file_utils_ ;

{$I agg_mode.inc }

const
 flip_y = true;

 base_shift = agg_color.base_shift;

type
 gradient_linear_color = object(array_base )
   m_c1  ,
   m_c2  ,
   m_res : aggclr;

   constructor Construct; overload;
   constructor Construct(c1 ,c2 : aggclr_ptr ); overload;

   procedure colors(c1 ,c2 : aggclr_ptr );

   function  size : unsigned; virtual;
   function  array_operator(i : unsigned ) : pointer; virtual;

  end;

 the_application = object(platform_support )
   m_alpha_src ,
   m_alpha_dst : slider_ctrl;
   m_comp_op   : rbox_ctrl;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure render_scene(rbuf : rendering_buffer_ptr; pixf : pixel_formats_ptr );

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ gradient_affine }
procedure gradient_affine(var mtx : trans_affine; x1 ,y1 ,x2 ,y2 : double; gradient_d2 : double = 100.0 );
var
 dx ,dy : double;

 tas : trans_affine_scaling;
 tar : trans_affine_rotation;
 tat : trans_affine_translation;

begin
 mtx.Construct;

 dx:=x2 - x1;
 dy:=y2 - y1;

 mtx.reset;

 tas.Construct(Sqrt(dx * dx + dy * dy ) / gradient_d2 ); mtx.multiply(@tas );
 tar.Construct(ArcTan2(dy ,dx ) ); mtx.multiply(@tar );
 tat.Construct(x1 ,y1 ); mtx.multiply(@tat );

 mtx.invert;

end;

{ circle }
procedure circle(rbase : renderer_base_ptr; c1 ,c2 : aggclr_ptr; x1 ,y1 ,x2 ,y2 ,shadow_alpha : double );
var
 color_func : gradient_linear_color;

 gradient_func  : gradient_x;
 gradient_mtx   : trans_affine;
 span_gradient_ : span_gradient;
 ren_gradient   : renderer_scanline_aa;
 ren_solid      : renderer_scanline_aa_solid;

 span_interpolator : span_interpolator_linear;
 span_allocator_   : span_allocator;

 rgba : aggclr;

 ras : rasterizer_scanline_aa;
 sl  : scanline_u8;
 ell : ellipse;

 r : double;

begin
 gradient_func.Construct;                      // The gradient function
 gradient_affine(gradient_mtx ,x1 ,y1 ,x2 ,y2 ,100 );

 span_interpolator.Construct(@gradient_mtx );  // Span interpolator
 span_allocator_.Construct;                    // Span Allocator
 color_func.Construct(c1 ,c2 );

 span_gradient_.Construct(
  @span_allocator_ ,
  @span_interpolator,
  @gradient_func,
  @color_func,
  0 ,100 );

 ren_gradient.Construct(rbase ,@span_gradient_ );
 ras.Construct;
 sl.Construct;

 r:=calc_distance(x1 ,y1 ,x2 ,y2 ) / 2;

 ell.Construct      ((x1 + x2 ) / 2 + 5 ,(y1 + y2 ) / 2 - 3 ,r ,r ,100 );
 ren_solid.Construct(rbase );
 rgba.ConstrDbl     (0.6 ,0.6 ,0.6 ,0.7 * shadow_alpha );
 ren_solid.color_   (@rgba );
 ras.add_path       (@ell );
 render_scanlines   (@ras ,@sl ,@ren_solid );

 ell.init        ((x1 + x2 ) / 2 ,(y1 + y2 ) / 2 ,r ,r ,100 );
 ras.add_path    (@ell );
 render_scanlines(@ras ,@sl ,@ren_gradient );

// Free
 ras.Destruct;
 sl.Destruct;
 span_allocator_.Destruct;

end;

{ dst_shape }
procedure dst_shape(rbase : renderer_base_ptr; c1 ,c2 : aggclr_ptr; x1 ,y1 ,x2 ,y2 : double );
var
 color_func : gradient_linear_color;
 shape      : rounded_rect;

 gradient_func : gradient_x;
 gradient_mtx  : trans_affine;
 ren_gradient  : renderer_scanline_aa;

 span_interpolator : span_interpolator_linear;
 span_allocator_   : span_allocator;
 span_gradient_    : span_gradient;

 ras : rasterizer_scanline_aa;
 sl  : scanline_u8;

begin
 gradient_func.Construct;                      // The gradient function
 gradient_affine(gradient_mtx ,x1 ,y1 ,x2 ,y2 ,100 );

 span_interpolator.Construct(@gradient_mtx);   // Span interpolator
 span_allocator_.Construct;                    // Span Allocator
 color_func.Construct(c1 ,c2 );

 span_gradient_.Construct(
  @span_allocator_ ,
  @span_interpolator ,
  @gradient_func ,
  @color_func ,
  0 ,100 );

 ren_gradient.Construct(rbase ,@span_gradient_ );
 ras.Construct;
 sl.Construct;

 shape.Construct (x1 ,y1 ,x2 ,y2 ,40 );
 ras.add_path    (@shape );
 render_scanlines(@ras ,@sl ,@ren_gradient );

// Free
 ras.Destruct;
 sl.Destruct;
 span_allocator_.Destruct;

end;

{ CONSTRUCT }
constructor gradient_linear_color.Construct;
begin
 m_c1.Construct;
 m_c2.Construct;
 m_res.Construct;

end;

{ CONSTRUCT }
constructor gradient_linear_color.Construct(c1 ,c2 : aggclr_ptr );
begin
 m_res.Construct;

 m_c1:=c1^;
 m_c2:=c2^;

end;

{ COLORS }
procedure gradient_linear_color.colors;
begin
 m_c1:=c1^;
 m_c2:=c2^;

end;

{ SIZE }
function gradient_linear_color.size;
begin
 result:=256

end;

{ ARRAY_OPERATOR }
function gradient_linear_color.array_operator;
begin
 i:=i shl (base_shift - 8 );

 m_res.r:=int8u((((m_c2.r - m_c1.r ) * i ) + (m_c1.r shl base_shift ) ) shr base_shift );
 m_res.g:=int8u((((m_c2.g - m_c1.g ) * i ) + (m_c1.g shl base_shift ) ) shr base_shift );
 m_res.b:=int8u((((m_c2.b - m_c1.b ) * i ) + (m_c1.b shl base_shift ) ) shr base_shift );
 m_res.a:=int8u((((m_c2.a - m_c1.a ) * i ) + (m_c1.a shl base_shift ) ) shr base_shift );

 result:=@m_res;

end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_alpha_src.Construct(5 ,5 ,400 ,11 ,not flip_y_ );
 m_alpha_dst.Construct(5 ,5 + 15 ,400 ,11 + 15 ,not flip_y_ );
 m_comp_op.Construct  (420 ,5.0 ,420 + 170.0 ,395.0 ,not flip_y_ );

 m_alpha_src.label_('Src Alpha=%.2f' );
 m_alpha_src.value_(1.0 );

 add_ctrl(@m_alpha_src );

 m_alpha_dst.label_('Dst Alpha=%.2f' );
 m_alpha_dst.value_(0.75 );

 add_ctrl(@m_alpha_dst );

 m_comp_op.text_size_(7 );
 m_comp_op.add_item ('clear' );
 m_comp_op.add_item ('src' );
 m_comp_op.add_item ('dst' );
 m_comp_op.add_item ('src-over' );
 m_comp_op.add_item ('dst-over' );
 m_comp_op.add_item ('src-in' );
 m_comp_op.add_item ('dst-in' );
 m_comp_op.add_item ('src-out' );
 m_comp_op.add_item ('dst-out' );
 m_comp_op.add_item ('src-atop' );
 m_comp_op.add_item ('dst-atop' );
 m_comp_op.add_item ('xor' );
 m_comp_op.add_item ('plus' );
 m_comp_op.add_item ('minus' );
 m_comp_op.add_item ('multiply' );
 m_comp_op.add_item ('screen' );
 m_comp_op.add_item ('overlay' );
 m_comp_op.add_item ('darken' );
 m_comp_op.add_item ('lighten' );
 m_comp_op.add_item ('color-dodge' );
 m_comp_op.add_item ('color-burn' );
 m_comp_op.add_item ('hard-light' );
 m_comp_op.add_item ('soft-light' );
 m_comp_op.add_item ('difference' );
 m_comp_op.add_item ('exclusion' );
 m_comp_op.add_item ('contrast' );
 m_comp_op.cur_item_(3 );

 add_ctrl(@m_comp_op );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_alpha_src.Destruct;
 m_alpha_dst.Destruct;
 m_comp_op.Destruct;

end;

{ RENDER_SCENE }
procedure the_application.render_scene;
var
 ren_pixf ,px : pixel_formats;
 renderer ,rb : renderer_base;

 rgba ,rgbb : aggclr;

 v : double;

begin
 pixfmt_custom_blend_rgba(ren_pixf ,rbuf ,@comp_op_adaptor_rgba ,bgra_order );

 renderer.Construct(@ren_pixf );
 rb.Construct      (pixf );

 pixfmt_bgra32(px ,rbuf_img(1 ) );
 rb.blend_from(@px ,NIL ,250 ,180 ,unsigned(trunc(m_alpha_src._value * 255 ) ) );

 rgba.ConstrInt($FD ,$F0 ,$6F ,unsigned(trunc(m_alpha_src._value * 255 ) ) );
 rgbb.ConstrInt($FE ,$9F ,$34 ,unsigned(trunc(m_alpha_src._value * 255 ) ) );

 circle(
  @rb ,@rgba ,@rgbb ,
  70 * 3 ,100 + 24 * 3 ,37 * 3 ,100 + 79 * 3 ,
  m_alpha_src._value );

 ren_pixf.comp_op_(m_comp_op._cur_item );

 if m_comp_op._cur_item = 25 then // Contrast
  begin
   v:=m_alpha_dst._value;

   rgba.ConstrDbl(v ,v ,v );

   dst_shape(
    @renderer ,
    @rgba ,@rgba ,
    300 + 50 ,100 + 24 * 3 ,107 + 50 ,100 + 79 * 3 );

  end
 else
  begin
   rgba.ConstrInt($7F ,$C1 ,$FF ,unsigned(trunc(m_alpha_dst._value * 255 ) ) );
   rgbb.ConstrInt($05 ,$00 ,$5F ,unsigned(trunc(m_alpha_dst._value * 255 ) ) );

   dst_shape(
    @renderer ,
    @rgba ,@rgbb ,
    300 + 50 ,100 + 24 * 3 ,107 + 50 ,100 + 79 * 3 );

  end;

end;

{ ON_INIT }
procedure the_application.on_init;
begin
end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf ,pixf2 ,pixf_pre : pixel_formats;

 rgba : aggclr;

 rb ,rb2 ,rb_pre : renderer_base;

 ras : rasterizer_scanline_aa;
 sl  : scanline_u8;
 ren : renderer_scanline_aa_solid;

 y ,x : unsigned;

 tm  : double;
 buf : array[0..63 ] of char;
 t   : gsv_text;
 pt  : conv_stroke;

begin
// Initialize structures
 pixfmt_bgra32(pixf ,rbuf_window );
 rb.Construct (@pixf );

 rgba.ConstrInt(255 ,255 ,255 );
 rb.clear      (@rgba );

 y:=0;

 while y < rb.height do
  begin
   x:=((y shr 3 ) and 1) shl 3;

   while x < rb.width do
    begin
     rgba.ConstrInt($df ,$df ,$df );

     rb.copy_bar(x ,y ,x + 7 ,y + 7 ,@rgba );

     inc(x ,16 );

    end;

   inc(y ,8 );

  end;

 create_img(0 ,rbuf_window._width ,rbuf_window._height ); // agg_platform_support functionality

 pixfmt_bgra32(pixf2 ,rbuf_img(0 ) );
 rb2.Construct(@pixf2 );

 rgba.ConstrInt(0 ,0 ,0 ,0 );
 rb2.clear     (@rgba );

 pixfmt_bgra32_pre(pixf_pre ,rbuf_window );
 rb_pre.Construct (@pixf_pre );

// Render Scene 
 start_timer;

 render_scene(rbuf_img(0 ) ,@pixf2 );

 tm:=elapsed_time;

 rb_pre.blend_from(@pixf2 );

// Render Text
 ras.Construct;
 sl.Construct;
 ren.Construct(@rb );

 t.Construct;
 t.size_(10.0 );

 pt.Construct(@t );
 pt.width_   (1.5 );

 sprintf       (@buf[0 ] ,'%3.2f ms' ,tm );
 t.start_point_(10.0 ,35.0 );
 t.text_       (@buf[0 ] );

 ras.add_path    (@pt );
 rgba.ConstrDbl  (0 ,0 ,0 );
 ren.color_      (@rgba );
 render_scanlines(@ras ,@sl ,@ren );

// Render the controls
 render_ctrl(@ras ,@sl ,@ren ,@m_alpha_src );
 render_ctrl(@ras ,@sl ,@ren ,@m_alpha_dst );
 render_ctrl(@ras ,@sl ,@ren ,@m_comp_op );

// Free AGG resources
 ras.Destruct;
 sl.Destruct;

 t.Destruct;
 pt.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
begin
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
   'AGG is fully compatible with all SVG 1.2 extended compositing modes.  '#13#13 +
   'How to play with:'#13#13 +
   'Try to change the alpha values of the source an destination images,'#13 +
   'to see, how a particular operation composes the resulting image.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;
 buf : array [0..255 ] of char;
 ext : string[10 ];

 img_name ,p ,n ,x : shortstring;

BEGIN
 app.Construct(pix_format_bgra32 ,flip_y );
 app.caption_ ('AGG Example. Compositing Modes (F1-Help)' );

 img_name:='compositing';

{$IFDEF WIN32 }
 if ParamCount > 0 then
  begin
   spread_name(ParamStr(1 ) ,p ,n ,x );

   img_name:=fold_name(p ,n ,'' );

  end;

{$ENDIF }  

 if not app.load_img(1 ,img_name ) then
  begin
   img_name:=img_name + #0;
   ext     :=app._img_ext + #0;

   if img_name = 'compositing'#0 then
    begin
     sprintf(@buf[0 ]             ,'File not found: %s' ,ptrcomp(@img_name[1 ] ) );
     sprintf(@buf[StrLen(@buf ) ] ,'%s. '#13'Download http://www.antigrain.com/' ,ptrcomp(@ext[1 ] ) );
     sprintf(@buf[StrLen(@buf ) ] ,'%s' ,ptrcomp(@img_name[1 ] ) );
     sprintf(@buf[StrLen(@buf ) ] ,'%s'#13'or copy it from another directory if available.' ,ptrcomp(@ext[1 ] ) );

    end
   else
    begin
     sprintf(@buf[0 ]             ,'File not found: %s' ,ptrcomp(@img_name[1 ] ) );
     sprintf(@buf[StrLen(@buf ) ] ,'%s' ,ptrcomp(@ext[1 ] ) );

    end;

   app.message_(@buf[0 ] );

  end
 else
  if app.init(600 ,400 ,window_resize ) then
   app.run;

 app.Destruct;

END.