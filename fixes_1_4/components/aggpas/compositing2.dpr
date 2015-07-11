//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 compositing2 ;

uses
 agg_basics ,
 agg_color ,
 agg_platform_support ,
 agg_ctrl ,
 agg_slider_ctrl ,
 agg_rbox_ctrl ,
 agg_rendering_buffer ,
 agg_rasterizer_scanline_aa ,
 agg_scanline_u ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_render_scanlines ,
 agg_rounded_rect ,
 agg_pixfmt ,
 agg_pixfmt_rgba ,
 agg_span_allocator ,
 agg_span_gradient ,
 agg_gsv_text ,
 agg_span_interpolator_linear ,
 agg_array ,
 agg_trans_affine ,
 agg_ellipse ,
 agg_conv_transform ;

{$I agg_mode.inc }

const
 flip_y = true;

type
 the_application = object(platform_support )
  private
   m_alpha_dst ,
   m_alpha_src : slider_ctrl;
   m_comp_op   : rbox_ctrl;

   m_ramp1 ,
   m_ramp2 : pod_auto_array;

   m_ras : rasterizer_scanline_aa;
   m_sl  : scanline_u8;

  public
   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure radial_shape(
              rbase : renderer_base_ptr;
              colors : array_base_ptr;
              x1 ,y1 ,x2 ,y2 : double );

   procedure render_scene(rb : renderer_base_ptr );

   procedure on_draw; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_alpha_dst.Construct(5   ,5      ,400         ,11      ,not flip_y_ );
 m_alpha_src.Construct(5   ,5 + 15 ,400         ,11 + 15 ,not flip_y_ );
 m_comp_op.Construct  (420 ,5.0    ,420 + 170.0 ,395.0   ,not flip_y_ );

 m_ramp1.Construct(256 ,sizeof(aggclr ) );
 m_ramp2.Construct(256 ,sizeof(aggclr ) );

 m_ras.Construct;
 m_sl.Construct;

 m_alpha_dst.label_('Dst Alpha=%.2f' );
 m_alpha_dst.value_(1.0 );

 add_ctrl(@m_alpha_dst );

 m_alpha_src.label_('Src Alpha=%.2f' );
 m_alpha_src.value_(1.0 );

 add_ctrl(@m_alpha_src );

 m_comp_op.text_size_(6.8 );
 m_comp_op.add_item  ('clear' );
 m_comp_op.add_item  ('src' );
 m_comp_op.add_item  ('dst' );
 m_comp_op.add_item  ('src-over' );
 m_comp_op.add_item  ('dst-over' );
 m_comp_op.add_item  ('src-in' );
 m_comp_op.add_item  ('dst-in' );
 m_comp_op.add_item  ('src-out' );
 m_comp_op.add_item  ('dst-out' );
 m_comp_op.add_item  ('src-atop' );
 m_comp_op.add_item  ('dst-atop' );
 m_comp_op.add_item  ('xor' );
 m_comp_op.add_item  ('plus' );
 m_comp_op.add_item  ('minus' );
 m_comp_op.add_item  ('multiply' );
 m_comp_op.add_item  ('screen' );
 m_comp_op.add_item  ('overlay' );
 m_comp_op.add_item  ('darken' );
 m_comp_op.add_item  ('lighten' );
 m_comp_op.add_item  ('color-dodge' );
 m_comp_op.add_item  ('color-burn' );
 m_comp_op.add_item  ('hard-light' );
 m_comp_op.add_item  ('soft-light' );
 m_comp_op.add_item  ('difference' );
 m_comp_op.add_item  ('exclusion' );
 m_comp_op.add_item  ('contrast' );
 m_comp_op.add_item  ('invert' );
 m_comp_op.add_item  ('invert-rgb' );
 m_comp_op.cur_item_ (3 );

 add_ctrl(@m_comp_op );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_alpha_dst.Destruct;
 m_alpha_src.Destruct;
 m_comp_op.Destruct;

 m_ramp1.Destruct;
 m_ramp2.Destruct;

 m_ras.Destruct;
 m_sl.Destruct;

end;

{ RADIAL_SHAPE }
procedure the_application.radial_shape;
var
 gradient_func     : gradient_radial;
 gradient_mtx      : trans_affine;
 span_interpolator : span_interpolator_linear;
 span_allocator_   : span_allocator;
 span_gradient_    : span_gradient;

 cx ,cy ,r : double;

 tas : trans_affine_scaling;
 tat : trans_affine_translation;

 ell : ellipse;

 trans : conv_transform;

 rg : renderer_scanline_aa;

begin
 gradient_func.Construct;
 gradient_mtx.Construct;
 span_interpolator.Construct(@gradient_mtx );
 span_allocator_.Construct;
 span_gradient_.Construct(
  @span_allocator_ ,
  @span_interpolator ,
  @gradient_func ,
  colors ,
  0 ,100 );

 cx:=(x1 + x2 ) / 2.0;
 cy:=(y1 + y2 ) / 2.0;

 if (x2 - x1 ) < (y2 - y1 ) then
  r:=0.5 * (x2 - x1 )
 else
  r:=0.5 * (y2 - y1 );

 tas.Construct        (r / 100.0 );
 gradient_mtx.multiply(@tas );
 tat.Construct        (cx ,cy );
 gradient_mtx.multiply(@tat );
 gradient_mtx.multiply(_trans_affine_resizing );
 gradient_mtx.invert;

 ell.Construct  (cx ,cy ,r ,r ,100 );
 trans.Construct(@ell ,_trans_affine_resizing );

 m_ras.add_path(@trans );
 rg.Construct  (rbase ,@span_gradient_ );

 render_scanlines(@m_ras ,@m_sl ,@rg );

 span_allocator_.Destruct;
 span_gradient_.Destruct;

end;

{ RENDER_SCENE }
procedure the_application.render_scene;
var
 pixf : pixel_formats;
 ren  : renderer_base;

 cx ,cy : double;

begin
 pixfmt_custom_blend_rgba(pixf ,rbuf_window ,@comp_op_adaptor_rgba ,bgra_order );

 ren.Construct(@pixf );

 pixf.comp_op_(unsigned(comp_op_difference ) );
 radial_shape (@ren ,@m_ramp1 ,50 ,50 ,50 + 320 ,50 + 320 );

 pixf.comp_op_(m_comp_op._cur_item );

 cx:=50;
 cy:=50;

 radial_shape(@ren ,@m_ramp2 ,cx + 120 - 70 ,cy + 120 - 70 ,cx + 120 + 70 ,cy + 120 + 70 );
 radial_shape(@ren ,@m_ramp2 ,cx + 200 - 70 ,cy + 120 - 70 ,cx + 200 + 70 ,cy + 120 + 70 );
 radial_shape(@ren ,@m_ramp2 ,cx + 120 - 70 ,cy + 200 - 70 ,cx + 120 + 70 ,cy + 200 + 70 );
 radial_shape(@ren ,@m_ramp2 ,cx + 200 - 70 ,cy + 200 - 70 ,cx + 200 + 70 ,cy + 200 + 70 );

end;

{ generate_color_ramp }
procedure generate_color_ramp(c : pod_auto_array_ptr; c1 ,c2 ,c3 ,c4 : aggclr_ptr );
var
 i : unsigned;

begin
 i:=0;

 while i < 85 do
  begin
   aggclr_ptr(c.array_operator(i ) )^:=c1.gradient(c2 ,i / 85.0 );

   inc(i );

  end;

 while i < 170 do
  begin
   aggclr_ptr(c.array_operator(i ) )^:=c2.gradient(c3 ,(i - 85 ) / 85.0 );

   inc(i );

  end;

 while i < 256 do
  begin
   aggclr_ptr(c.array_operator(i ) )^:=c3.gradient(c4 ,(i - 170 ) / 85.0 );

   inc(i );

  end;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;
 rgba : aggclr;

 rb : renderer_base;
 rs : renderer_scanline_aa_solid;

 c1 ,c2 ,c3 ,c4 : aggclr;

begin
// Initialize structures
// pixfmt_alpha_blend_rgba(pixf ,rbuf_window ,bgra_order ); {!}
 pixfmt_bgra32(pixf ,rbuf_window );

 rb.Construct(@pixf );
 rs.Construct(@rb );

 rgba.ConstrInt(255 ,255 ,255 );
 rb.clear      (@rgba );

// Render
 c1.ConstrDbl(0 ,0 ,0 ,m_alpha_dst._value );
 c2.ConstrDbl(0 ,0 ,1 ,m_alpha_dst._value );
 c3.ConstrDbl(0 ,1 ,0 ,m_alpha_dst._value );
 c4.ConstrDbl(1 ,0 ,0 ,0 );

 generate_color_ramp(@m_ramp1 ,@c1 ,@c2 ,@c3 ,@c4 );

 c1.ConstrDbl(0 ,0 ,0 ,m_alpha_src._value );
 c2.ConstrDbl(0 ,0 ,1 ,m_alpha_src._value );
 c3.ConstrDbl(0 ,1 ,0 ,m_alpha_src._value );
 c4.ConstrDbl(1 ,0 ,0 ,0 );

 generate_color_ramp(@m_ramp2 ,@c1 ,@c2 ,@c3 ,@c4 );

 render_scene(@rb );

// Render the controls
 render_ctrl(@m_ras ,@m_sl ,@rs ,@m_alpha_dst );
 render_ctrl(@m_ras ,@m_sl ,@rs ,@m_alpha_src );
 render_ctrl(@m_ras ,@m_sl ,@rs ,@m_comp_op );

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'Another demo example with extended compositing modes.    ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgra32 ,flip_y );
 app.caption_ ('AGG Example. Compositing Modes (F1-Help)' );

 if app.init(600 ,400 ,window_resize or window_keep_aspect_ratio ) then
  app.run;

 app.Destruct;

END.