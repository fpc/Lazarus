//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 trans_polar_ ;

uses
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
 agg_scanline_u ,
 agg_render_scanlines ,

 agg_conv_transform ,
 agg_conv_segmentator ,
 agg_vertex_source ,
 agg_trans_affine ;

{$I agg_mode.inc }

const
 flip_y = true;

type
 transformed_control = object(ctrl )
   m_ctrl     : ctrl_ptr;
   m_pipeline : vertex_source_ptr;

   constructor Construct(ctrl : ctrl_ptr; pl : vertex_source_ptr );

   function  num_paths : unsigned; virtual;
   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;
   function  _color(i : unsigned ) : aggclr_ptr; virtual;

  end;

 trans_polar_ptr = ^trans_polar; 
 trans_polar = object(trans_affine )
   m_base_angle ,
   m_base_scale ,

   m_base_x ,
   m_base_y ,

   m_translation_x ,
   m_translation_y ,

   m_spiral : double;

   constructor Construct;

   procedure base_scale (v : double );
   procedure full_circle(v : double );
   procedure base_offset(dx ,dy : double );
   procedure translation(dx ,dy : double );
   procedure spiral     (v : double );

  end;

 the_application = object(platform_support )
   m_slider1       ,
   m_slider_spiral ,
   m_slider_base_y : slider_ctrl;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_draw; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor transformed_control.Construct;
begin
 m_ctrl    :=ctrl;
 m_pipeline:=pl;

end;

{ NUM_PATHS }
function transformed_control.num_paths;
begin
 result:=m_ctrl.num_paths;

end;

{ REWIND }
procedure transformed_control.rewind;
begin
 m_pipeline.rewind(path_id );

end;

{ VERTEX }
function transformed_control.vertex;
begin
 result:=m_pipeline.vertex(x ,y );

end;

{ _COLOR }
function transformed_control._color;
begin
 result:=m_ctrl._color(i );

end;

{ trans_polar_transform }
procedure trans_polar_transform(this : trans_polar_ptr; x ,y : double_ptr );
var
 x1 ,y1 : double;

begin
 x1:=(x^ + this.m_base_x ) * this.m_base_angle;
 y1:=(y^ + this.m_base_y ) * this.m_base_scale + (x^ * this.m_spiral );

 x^:=Cos(x1 ) * y1 + this.m_translation_x;
 y^:=Sin(x1 ) * y1 + this.m_translation_y;

end;

{ CONSTRUCT }
constructor trans_polar.Construct;
begin
 inherited Construct;

 m_base_angle:=1.0;
 m_base_scale:=1.0;

 m_base_x:=0.0;
 m_base_y:=0.0;

 m_translation_x:=0.0;
 m_translation_y:=0.0;

 m_spiral:=0.0;

 transform:=@trans_polar_transform;

end;

{ BASE_SCALE }
procedure trans_polar.base_scale;
begin
 m_base_scale:=v;

end;

{ FULL_CIRCLE }
procedure trans_polar.full_circle;
begin
 m_base_angle:=2.0 * pi / v;

end;

{ BASE_OFFSET }
procedure trans_polar.base_offset;
begin
 m_base_x:=dx;
 m_base_y:=dy;

end;

{ TRANSLATION }
procedure trans_polar.translation;
begin
 m_translation_x:=dx;
 m_translation_y:=dy;

end;

{ SPIRAL }
procedure trans_polar.spiral;
begin
 m_spiral:=v;

end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_slider1.Construct      (10 ,10 ,600 - 10 ,17 ,not flip_y_ );
 m_slider_spiral.Construct(10 ,10 + 20 ,600 - 10 ,17 + 20 ,not flip_y_ );
 m_slider_base_y.Construct(10 ,10 + 40 ,600 - 10 ,17 + 40 ,not flip_y_ );

 add_ctrl(@m_slider1 );

 m_slider1.range_    (0.0 ,100.0 );
 m_slider1.num_steps_(5 );
 m_slider1.value_    (32.0);
 m_slider1.label_    ('Some Value=%1.0f' );

 add_ctrl(@m_slider_spiral );

 m_slider_spiral.label_('Spiral=%.3f' );
 m_slider_spiral.range_(-0.1 ,0.1 );
 m_slider_spiral.value_(0.0 );

 add_ctrl(@m_slider_base_y );

 m_slider_base_y.label_('Base Y=%.3f' );
 m_slider_base_y.range_(50.0 ,200.0 );
 m_slider_base_y.value_(120.0 );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_slider1.Destruct;
 m_slider_spiral.Destruct;
 m_slider_base_y.Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;

 rb  : renderer_base;
 ren : renderer_scanline_aa_solid;
 ras : rasterizer_scanline_aa;
 sl  : scanline_u8;

 rgba  : aggclr;
 trans : trans_polar;
 segm  : conv_segmentator;

 pipeline : conv_transform;

 ctrl : transformed_control;

begin
// Initialize structures
 pixfmt_bgr24(pixf ,rbuf_window );

 rb.Construct (@pixf );
 ren.Construct(@rb );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear      (@rgba );

 ras.Construct;
 sl.Construct;

// Render the controls
 render_ctrl(@ras ,@sl ,@ren ,@m_slider1 );
 render_ctrl(@ras ,@sl ,@ren ,@m_slider_spiral );
 render_ctrl(@ras ,@sl ,@ren ,@m_slider_base_y );

// Render
 trans.Construct;
 trans.full_circle(-600 );
 trans.base_scale (-1.0 );
 trans.base_offset(0.0 ,m_slider_base_y._value );
 trans.translation(_width / 2.0 ,_height / 2.0 + 30.0 );
 trans.spiral     (-m_slider_spiral._value );

 segm.Construct    (@m_slider1 );
 pipeline.Construct(@segm ,@trans );
 ctrl.Construct    (@m_slider1 ,@pipeline );

 render_ctrl(@ras ,@sl ,@ren ,@ctrl );

// Free AGG resources
 ras.Destruct;
 sl.Destruct;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'Another example of non-linear transformations requested by one of my friends. '#13 +
   'Here we render a standard AGG control in its original form (the slider in the '#13 +
   'bottom) and after the transformation. The transformer itself is not a part of '#13 +
   'AGG and just demonstrates how to write custom transformers (class trans_polar).'#13 +
   'Note that because the transformer is non-linear, we need to use conv_segmentator '#13 +
   'first. Don''t worry much about the transformed_control class, it''s just an adaptor  '#13 +
   'used to render the controls with additional transformations.'#13#13 +
   'How to play with:'#13#13 +
   'Try to drag the value of the slider at the bottom and watch how it''s being '#13 +
   'synchronized in the polar coordinates. Also change two other parameters '#13 +
   '(Spiral and Base Y) and the size of the window.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. Polar Transformer (F1-Help)' );

 if app.init(600 ,400 ,window_resize ) then
  app.run;

 app.Destruct;

END.