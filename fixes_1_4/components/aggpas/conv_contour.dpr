//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 conv_contour_ ;

uses
 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,

 agg_ctrl ,
 agg_rbox_ctrl ,
 agg_cbox_ctrl ,
 agg_slider_ctrl ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_rasterizer_scanline_aa ,
 agg_scanline_p ,
 agg_render_scanlines ,

 agg_conv_stroke ,
 agg_conv_transform ,
 agg_conv_curve ,
 agg_conv_contour ,
 agg_path_storage ,
 agg_trans_affine ,
 agg_math_stroke ;

{$I agg_mode.inc }

const
 flip_y = true;

type
 the_application = object(platform_support )
   m_close       : rbox_ctrl;
   m_width       : slider_ctrl;
   m_auto_detect : cbox_ctrl;
   m_path        : path_storage;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure compose_path;
   procedure on_draw; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_close.Construct      (10.0 ,10.0 ,130.0 ,80.0 ,not flip_y_ );
 m_width.Construct      (130 + 10.0 ,10.0 + 4.0 ,130 + 300.0 ,10.0 + 8.0 + 4.0 ,not flip_y_ );
 m_auto_detect.Construct(130 + 10.0 ,10.0 + 4.0 + 16.0 ,'Autodetect orientation if not defined' ,not flip_y_ );
 m_path.Construct;

 add_ctrl(@m_close );

 m_close.add_item ('Close' );
 m_close.add_item ('Close CW' );
 m_close.add_item ('Close CCW' );
 m_close.cur_item_(0 );

 add_ctrl(@m_width );

 m_width.range_(-100.0 ,100.0 );
 m_width.value_(0.0 );
 m_width.label_('Width=%1.2f' );

 add_ctrl(@m_auto_detect );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_close.Destruct;
 m_width.Destruct;
 m_auto_detect.Destruct;
 m_path.Destruct;

end;

{ COMPOSE_PATH }
procedure the_application.compose_path;
var
 flag : unsigned;

begin
 flag:=0;

 if m_close._cur_item = 1 then
  flag:=path_flags_cw;

 if m_close._cur_item = 2 then
  flag:=path_flags_ccw;

 m_path.remove_all;

 m_path.move_to(28.47 ,6.45 );
 m_path.curve3 (21.58 ,1.12  ,19.82 ,0.29 );
 m_path.curve3 (17.19 ,-0.93 ,14.21 ,-0.93 );
 m_path.curve3 (9.57  ,-0.93 ,6.57  ,2.25 );
 m_path.curve3 (3.56  ,5.42  ,3.56  ,10.60 );
 m_path.curve3 (3.56  ,13.87 ,5.03  ,16.26 );
 m_path.curve3 (7.03  ,19.58 ,11.99 ,22.51 );
 m_path.curve3 (16.94 ,25.44 ,28.47 ,29.64 );
 m_path.line_to(28.47 ,31.40 );
 m_path.curve3 (28.47 ,38.09 ,26.34 ,40.58 );
 m_path.curve3 (24.22 ,43.07 ,20.17 ,43.07 );
 m_path.curve3 (17.09 ,43.07 ,15.28 ,41.41 );
 m_path.curve3 (13.43 ,39.75 ,13.43 ,37.60 );
 m_path.line_to(13.53 ,34.77 );
 m_path.curve3 (13.53 ,32.52 ,12.38 ,31.30 );
 m_path.curve3 (11.23 ,30.08 ,9.38  ,30.08 );
 m_path.curve3 (7.57  ,30.08 ,6.42  ,31.35 );
 m_path.curve3 (5.27  ,32.62 ,5.27  ,34.81 );
 m_path.curve3 (5.27  ,39.01 ,9.57  ,42.53 );
 m_path.curve3 (13.87 ,46.04 ,21.63 ,46.04 );
 m_path.curve3 (27.59 ,46.04 ,31.40 ,44.04 );
 m_path.curve3 (34.28 ,42.53 ,35.64 ,39.31 );
 m_path.curve3 (36.52 ,37.21 ,36.52 ,30.71 );
 m_path.line_to(36.52 ,15.53 );
 m_path.curve3 (36.52 ,9.13  ,36.77 ,7.69 );
 m_path.curve3 (37.01 ,6.25  ,37.57 ,5.76 );
 m_path.curve3 (38.13 ,5.27  ,38.87 ,5.27 );
 m_path.curve3 (39.65 ,5.27  ,40.23 ,5.62 );
 m_path.curve3 (41.26 ,6.25  ,44.19 ,9.18 );
 m_path.line_to(44.19 ,6.45  );
 m_path.curve3 (38.72 ,-0.88 ,33.74 ,-0.88 );
 m_path.curve3 (31.35 ,-0.88 ,29.93 ,0.78 );
 m_path.curve3 (28.52 ,2.44  ,28.47 ,6.45 );

 m_path.close_polygon(flag );

 m_path.move_to(28.47 ,9.62 );
 m_path.line_to(28.47 ,26.66 );
 m_path.curve3 (21.09 ,23.73 ,18.95 ,22.51 );
 m_path.curve3 (15.09 ,20.36 ,13.43 ,18.02 );
 m_path.curve3 (11.77 ,15.67 ,11.77 ,12.89 );
 m_path.curve3 (11.77 ,9.38  ,13.87 ,7.06 );
 m_path.curve3 (15.97 ,4.74  ,18.70 ,4.74 );
 m_path.curve3 (22.41 ,4.74  ,28.47 ,9.62 );

 m_path.close_polygon(flag );

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;
 renb : renderer_base;
 ren  : renderer_scanline_aa_solid;
 rgba : aggclr;

 ras : rasterizer_scanline_aa;
 sl  : scanline_p8;

 mtx : trans_affine;
 tas : trans_affine_scaling;
 tat : trans_affine_translation;

 trans : conv_transform;
 curve : conv_curve;

 contour : conv_contour;

begin
// Initialize structures
 pixfmt_bgr24(pixf ,rbuf_window );

 renb.Construct(@pixf );
 ren.Construct (@renb );
 rgba.ConstrDbl(1 ,1 ,1 );
 renb.clear    (@rgba );

 ras.Construct;
 sl.Construct;

// Render
 mtx.Construct;

 tas.Construct(4.0 );
 mtx.multiply (@tas );

 tat.Construct(150 ,100 );
 mtx.multiply (@tat );

 trans.Construct(@m_path ,@mtx );
 curve.Construct(@trans );

 contour.Construct(@curve );

 contour.width_                  (m_width._value );
 //contour.line_join_              (miter_join );
 //contour.inner_join_             (miter_join );
 //contour.inner_miter_limit_      (4.0 );
 contour.auto_detect_orientation_(m_auto_detect._status );

 compose_path;
 ras.add_path(@contour );

 rgba.ConstrDbl(0 ,0 ,0 );
 ren.color_    (@rgba );

 render_scanlines(@ras ,@sl ,@ren );

// Render the controls
 render_ctrl(@ras ,@sl ,@ren ,@m_close );
 render_ctrl(@ras ,@sl ,@ren ,@m_width );
 render_ctrl(@ras ,@sl ,@ren ,@m_auto_detect );

// Free AGG resources
 ras.Destruct;
 sl.Destruct;

 curve.Destruct;
 contour.Destruct;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'One of the converters in AGG is conv_contour. It allows you to extend or shrink '#13 +
   'polygons. Initially, it was implemented to eliminate the "problem of adjacent edges"'#13 +
   'in the SVG Viewer, but it can be very useful in many other applications, for example,'#13 +
   'to change the font weight on the fly. The trick here is that the sign (dilation or '#13 +
   'shrinking) depends on the vertex order - clockwise or counterclockwise. '#13 +
   'In the conv_contour you can control the behavior. Sometimes you need to preserve '#13 +
   'the dilation regardless of the initial orientation, sometimes it should depend '#13 +
   'on the orientation. The glyph ‘a’ has a "hole" whose orientation differs from '#13 +
   'the main contour. To change the "weight" correctly, you need to keep the orientation '#13 +
   'as it is originally defined. If you turn "Autodetect orientation…" on, the glyph will '#13 +
   'be extended or shrinked incorrectly.'#13#13 +
   'How to play with:'#13#13 +
   'The radio buttons control the orientation flad assigned to all polygons.'#13 +
   '"Close" doesn''t add the flag.'#13 +
   '"Close CW" and "Close CCW" add "clockwise" or "counterclockwise" flag respectively.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. Contour Tool & Polygon Orientation (F1-Help)' );

 if app.init(440 ,330 ,0 ) then
  app.run;

 app.Destruct;

END.