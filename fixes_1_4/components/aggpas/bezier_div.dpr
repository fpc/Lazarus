//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 bezier_div ;

uses
 Math ,SysUtils ,

 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,

 agg_ctrl ,
 agg_slider_ctrl ,
 agg_rbox_ctrl ,
 agg_cbox_ctrl ,
 agg_bezier_ctrl ,

 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_u ,
 agg_render_scanlines ,
 agg_renderer_outline_aa ,
 agg_renderer_outline_image ,

 agg_conv_transform ,
 agg_conv_stroke ,
 agg_conv_dash ,
 agg_pattern_filters_rgba ,
 agg_arc ,
 agg_array ,
 agg_curves ,
 agg_bezier_arc ,
 agg_vertex_sequence ,
 agg_math ,
 agg_math_stroke ,
 agg_path_storage ,
 agg_gsv_text ,
 agg_ellipse ;

{$I agg_mode.inc }
{$I- }
const
 flip_y = true;

type
 curve_point_ptr = ^curve_point;
 curve_point = object
   x ,y ,dist ,mu : double;

   constructor Construct; overload;
   constructor Construct(x1 ,y1 ,mu1 : double ); overload;

  end;

 the_application = object(platform_support )
   m_ctrl_color : aggclr;

   m_curve1 : bezier_ctrl;

   m_angle_tolerance     ,
   m_approximation_scale ,
   m_cusp_limit          ,
   m_width               : slider_ctrl;

   m_show_points  ,
   m_show_outline : cbox_ctrl;
   m_curve_type   ,
   m_case_type    ,
   m_inner_join   ,
   m_line_join    ,
   m_line_cap     : rbox_ctrl;

   m_cur_case_type : int;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   function  measure_time(curve : curve_ptr ) : double;
   function  find_point  (path : pod_deque_ptr; dist : double; i ,j : unsigned_ptr ) : boolean;

   function  calc_max_error(curve : curve_ptr; scale : double; max_angle_error : double_ptr ) : double;

   procedure on_draw; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;
   procedure on_ctrl_change; virtual;

  end;

{ BEZIER4_POINT }
procedure bezier4_point(
           x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 ,mu : double;
           x ,y : double_ptr );
var
 mum1 ,mum13 ,mu3 : double;

begin
 mum1 :=1 - mu;
 mum13:=mum1 * mum1 * mum1;
 mu3  :=mu * mu * mu;

 x^:=mum13 * x1 + 3 * mu * mum1 * mum1 * x2 + 3 * mu * mu * mum1 * x3 + mu3 * x4;
 y^:=mum13 * y1 + 3 * mu * mum1 * mum1 * y2 + 3 * mu * mu * mum1 * y3 + mu3 * y4;

end;

{ CONSTRUCT }
constructor curve_point.Construct;
begin
 x   :=0;
 y   :=0;
 dist:=0;
 mu  :=0;

end;

{ CONSTRUCT }
constructor curve_point.Construct(x1 ,y1 ,mu1 : double );
begin
 x   :=x1;
 y   :=y1;
 dist:=0;
 mu  :=mu1;

end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_ctrl_color.ConstrDbl(0 ,0.3 ,0.5 ,0.8 );

 m_angle_tolerance.Construct    (5.0 ,5.0 ,240.0 ,12.0 ,not flip_y_ );
 m_approximation_scale.Construct(5.0 ,17 + 5.0 , 240.0 ,17 + 12.0 ,not flip_y_ );
 m_cusp_limit.Construct         (5.0 ,17 + 17 + 5.0 ,240.0 ,17 + 17 + 12.0 ,not flip_y_ );
 m_width.Construct              (245.0 ,5.0 ,495.0 ,12.0 ,not flip_y_ );
 m_show_points.Construct        (250.0 ,15 + 5 ,'Show Points' ,not flip_y_ );
 m_show_outline.Construct       (250.0 ,30 + 5 ,'Show Stroke Outline' ,not flip_y_ );
 m_curve_type.Construct         (535.0 ,5.0 ,535.0 + 115.0 ,55.0 ,not flip_y_ );
 m_case_type.Construct          (535.0 ,60.0 ,535.0 + 115.0 ,195.0 ,not flip_y_ );
 m_inner_join.Construct         (535.0 ,200.0 ,535.0 + 115.0 ,290.0 ,not flip_y_ );
 m_line_join.Construct          (535.0 ,295.0 ,535.0 + 115.0 ,385.0 ,not flip_y_ );
 m_line_cap.Construct           (535.0 ,395.0 ,535.0 + 115.0 ,455.0 ,not flip_y_ );

 m_cur_case_type:=-1;

 m_curve1.Construct;
 m_curve1.line_color_(@m_ctrl_color );

 m_curve1.curve_(170 ,424 ,13 ,87 ,488 ,423 ,26 ,333 );
 //m_curve1.curve_(26.000 ,333.000 ,276.000 ,126.000 ,402.000 ,479.000 ,26.000 ,333.000 ); // Loop with p1==p4
 //m_curve1.curve_(378.000 ,439.000 ,378.000 ,497.000 ,487.000 ,432.000 ,14.000 ,338.000 ); // Narrow loop
 //m_curve1.curve_(288.000 ,283.000 ,232.000 ,89.000 ,66.000 ,197.000 ,456.000 ,241.000 ); // Loop
 //m_curve1.curve_(519.000 ,142.000 ,97.000 ,147.000 ,69.000 ,147.000 ,30.000 ,144.000 ); // Almost straight
 //m_curve1.curve_(100 ,100 ,200 ,100 ,100 ,200 ,200 ,200 ); // A "Z" case
 //m_curve1.curve_(150 ,150 ,350 ,150 ,150 ,150 ,350 ,150 ); // Degenerate
 //m_curve1.curve_(409 ,330 ,300 ,200 ,200 ,200 ,401 ,263 ); // Strange cusp
 //m_curve1.curve_(129 ,233 ,172 ,320 ,414 ,253 ,344 ,236 ); // Curve cap
 //m_curve1.curve_(100 ,100 ,100 ,200 ,100 ,100 ,110 ,100 ); // A "boot"
 //m_curve1.curve_(225 ,150 ,60 ,150 ,460 ,150 ,295 ,150 ); // 2----1----4----3
 //m_curve1.curve_(162.2 ,248.801 ,162.2 ,248.801 ,266 ,284 ,394 ,335 );  // Coinciding 1-2
 //m_curve1.curve_(162.200 ,248.801 ,162.200 ,248.801 ,257.000 ,301.000 ,394.000 ,335.000 ); // Coinciding 1-2
 //m_curve1.curve_(394.000 ,335.000 ,257.000 ,301.000 ,162.200 ,248.801 ,162.200 ,248.801 ); // Coinciding 3-4
 //m_curve1.curve_(84.200000 ,302.80100 ,84.200000 ,302.80100 ,79.000000 ,292.40100 ,97.001000 ,304.40100 ); // From tiger.svg
 //m_curve1.curve_(97.001000 ,304.40100 ,79.000000 ,292.40100 ,84.200000 ,302.80100 ,84.200000 ,302.80100 ); // From tiger.svg opposite dir
 //m_curve1.curve_(475 ,157 ,200 ,100 ,453 ,100 ,222 ,157 ); // Cusp, failure for Adobe SVG

 add_ctrl(@m_curve1 );

 m_curve1.no_transform;

 m_angle_tolerance.label_('Angle Tolerance=%.0f deg' );
 m_angle_tolerance.range_(0 ,90 );
 m_angle_tolerance.value_(15 );

 add_ctrl(@m_angle_tolerance );

 m_angle_tolerance.no_transform;

 m_approximation_scale.label_('Approximation Scale=%.3f' );
 m_approximation_scale.range_(0.1 ,5 );
 m_approximation_scale.value_(1.0 );

 add_ctrl(@m_approximation_scale );

 m_approximation_scale.no_transform;

 m_cusp_limit.label_('Cusp Limit=%.0f deg' );
 m_cusp_limit.range_(0 ,90);
 m_cusp_limit.value_(0 );

 add_ctrl(@m_cusp_limit );

 m_cusp_limit.no_transform;

 m_width.label_('Width=%.2f' );
 m_width.range_(0.0 ,100 );
 m_width.value_(50.0 );

 add_ctrl(@m_width );

 m_width.no_transform;

 add_ctrl(@m_show_points );

 m_show_points.no_transform;
 m_show_points.status_(true );

 add_ctrl(@m_show_outline );

 m_show_outline.no_transform;
 m_show_outline.status_(true );

 m_curve_type.add_item ('Incremental' );
 m_curve_type.add_item ('Subdiv' );
 m_curve_type.cur_item_(1 );

 add_ctrl(@m_curve_type );

 m_curve_type.no_transform;

 m_case_type.text_size_     (7 );
 m_case_type.text_thickness_(1.0 );

 m_case_type.add_item('Random' );
 m_case_type.add_item('13---24' );
 m_case_type.add_item('Smooth Cusp 1' );
 m_case_type.add_item('Smooth Cusp 2' );
 m_case_type.add_item('Real Cusp 1' );
 m_case_type.add_item('Real Cusp 2' );
 m_case_type.add_item('Fancy Stroke' );
 m_case_type.add_item('Jaw' );
 m_case_type.add_item('Ugly Jaw' );

 add_ctrl(@m_case_type );

 m_case_type.no_transform;

 m_inner_join.text_size_(8 );

 m_inner_join.add_item ('Inner Bevel' );
 m_inner_join.add_item ('Inner Miter' );
 m_inner_join.add_item ('Inner Jag' );
 m_inner_join.add_item ('Inner Round' );
 m_inner_join.cur_item_(3 );

 add_ctrl(@m_inner_join );

 m_inner_join.no_transform;

 m_line_join.text_size_(8 );

 m_line_join.add_item ('Miter Join' );
 m_line_join.add_item ('Miter Revert' );
 m_line_join.add_item ('Miter Round' );
 m_line_join.add_item ('Round Join' );
 m_line_join.add_item ('Bevel Join' );
 m_line_join.cur_item_(1 );

 add_ctrl(@m_line_join );

 m_line_join.no_transform;

 m_line_cap.text_size_(8 );

 m_line_cap.add_item ('Butt Cap' );
 m_line_cap.add_item ('Square Cap' );
 m_line_cap.add_item ('Round Cap' );
 m_line_cap.cur_item_(0 );

 add_ctrl(@m_line_cap );

 m_line_cap.no_transform;

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_angle_tolerance.Destruct;
 m_approximation_scale.Destruct;
 m_cusp_limit.Destruct;
 m_width.Destruct;
 m_show_points.Destruct;
 m_show_outline.Destruct;
 m_curve_type.Destruct;
 m_case_type.Destruct;
 m_inner_join.Destruct;
 m_line_join.Destruct;
 m_line_cap.Destruct;
 m_curve1.Destruct;

end;

{ MEASURE_TIME }
function the_application.measure_time;
var
 i : int;

 x ,y : double;

begin
 start_timer;

 for i:=0 to 99 do
  begin
   curve.init4(
    m_curve1._x1 ,m_curve1._y1 ,
    m_curve1._x2 ,m_curve1._y2 ,
    m_curve1._x3 ,m_curve1._y3 ,
    m_curve1._x4 ,m_curve1._y4 );

   curve.rewind(0 );

   while not is_stop(curve.vertex(@x ,@y ) ) do;

  end;

 result:=elapsed_time * 10;

end;

{ FIND_POINT }
function the_application.find_point;
var
 k : int;

begin
 j^:=path.size - 1;
 i^:=0;

 while j^ - i^ > 1 do
  begin
   k:=shr_int32(i^ + j^ ,1 );

   if dist < vertex_dist_ptr(path.array_operator(k ) ).dist then
    j^:=k
   else
    i^:=k;

  end;

 result:=true;

end;

{ CALC_MAX_ERROR }
function the_application.calc_max_error;
var
 cmd ,i ,idx1 ,idx2 : unsigned;

 x ,y ,curve_dist ,mu ,reference_dist ,max_error ,err ,aerr ,a1 ,a2 ,da : double;

 curve_points ,reference_points : pod_deque;

 vd : vertex_dist;
 cp : curve_point;

begin
 curve_points.Construct    (sizeof(vertex_dist ) ,8 );
 reference_points.Construct(sizeof(curve_point ) ,8 );

 curve.approximation_scale_(m_approximation_scale._value * scale );

 curve.init4(
  m_curve1._x1 ,m_curve1._y1 ,
  m_curve1._x2 ,m_curve1._y2 ,
  m_curve1._x3 ,m_curve1._y3 ,
  m_curve1._x4 ,m_curve1._y4 );

 curve.rewind(0 );

 vd.dist:=0;

 cmd:=curve.vertex(@x ,@y );

 while not is_stop(cmd ) do
  begin
   if is_vertex(cmd ) then
    begin
     vd.x:=x;
     vd.y:=y;

     curve_points.add(@vd );

    end;

   cmd:=curve.vertex(@x ,@y );

  end;

 curve_dist:=0;

 i:=1;

 while i < curve_points.size do
  begin
   vertex_dist_ptr(curve_points.array_operator(i - 1 ) ).dist:=curve_dist;

   curve_dist:=
    curve_dist +
    calc_distance(
     vertex_dist_ptr(curve_points.array_operator(i - 1 ) ).x ,
     vertex_dist_ptr(curve_points.array_operator(i - 1 ) ).y ,
     vertex_dist_ptr(curve_points.array_operator(i ) ).x ,
     vertex_dist_ptr(curve_points.array_operator(i ) ).y );

   inc(i );

  end;

 vertex_dist_ptr(curve_points.array_operator(curve_points.size - 1 ) ).dist:=curve_dist;

 for i:=0 to 4095 do
  begin
   mu:=i / 4095.0;

   bezier4_point(
    m_curve1._x1 , m_curve1._y1 ,
    m_curve1._x2 , m_curve1._y2 ,
    m_curve1._x3 , m_curve1._y3 ,
    m_curve1._x4 , m_curve1._y4 ,
    mu ,@x ,@y );

   cp.Construct(x ,y ,mu );

   reference_points.add(@cp );

  end;

 reference_dist:=0;

 i:=1;

 while i < reference_points.size do
  begin
   curve_point_ptr(reference_points.array_operator(i - 1 ) ).dist:=reference_dist;

   reference_dist:=
    reference_dist +
    calc_distance(
     curve_point_ptr(reference_points.array_operator(i - 1 ) ).x ,
     curve_point_ptr(reference_points.array_operator(i - 1 ) ).y ,
     curve_point_ptr(reference_points.array_operator(i ) ).x ,
     curve_point_ptr(reference_points.array_operator(i ) ).y );

   inc(i );

  end;

 curve_point_ptr(reference_points.array_operator(reference_points.size - 1 ) ).dist:=reference_dist;

 idx1:=0;
 idx2:=1;

 max_error:=0;

 i:=0;

 while i < reference_points.size do
  begin
   if find_point(
       @curve_points ,
       curve_point_ptr(reference_points.array_operator(i ) ).dist ,
       @idx1 ,@idx2 ) then
    begin
     err:=
      Abs(
       calc_line_point_distance(
        vertex_dist_ptr(curve_points.array_operator(idx1 ) ).x ,
        vertex_dist_ptr(curve_points.array_operator(idx1 ) ).y ,
        vertex_dist_ptr(curve_points.array_operator(idx2 ) ).x ,
        vertex_dist_ptr(curve_points.array_operator(idx2 ) ).y ,
        curve_point_ptr(reference_points.array_operator(i ) ).x ,
        curve_point_ptr(reference_points.array_operator(i ) ).y ) );

     if err > max_error then
      max_error:=err;

    end;

   inc(i );

  end;

 aerr:=0;

 i:=2;

 while i < curve_points.size do
  begin
   a1:=
    ArcTan2(
     vertex_dist_ptr(curve_points.array_operator(i - 1 ) ).y -
     vertex_dist_ptr(curve_points.array_operator(i - 2 ) ).y ,
     vertex_dist_ptr(curve_points.array_operator(i - 1 ) ).x -
     vertex_dist_ptr(curve_points.array_operator(i - 2 ) ).x );

   a2:=
    ArcTan2(
     vertex_dist_ptr(curve_points.array_operator(i ) ).y -
     vertex_dist_ptr(curve_points.array_operator(i - 1 ) ).y ,
     vertex_dist_ptr(curve_points.array_operator(i ) ).x -
     vertex_dist_ptr(curve_points.array_operator(i - 1 ) ).x );

   da:=Abs(a1 - a2 );

   if da >= pi then
    da:=2 * pi - da;

   if da > aerr then
    aerr:=da;

   inc(i );

  end;

 max_angle_error^:=aerr * 180.0 / pi;

 result:=max_error * scale;

 curve_points.Destruct;
 reference_points.Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 ren_base : renderer_base;
 rgba     : aggclr;

 pf  : pixel_formats;
 ren : renderer_scanline_aa_solid;
 ras : rasterizer_scanline_aa;
 sl  : scanline_u8;

 path    : path_storage;
 curve   : curve4;
 stroke  ,
 stroke2 : conv_stroke;

 ell : ellipse;
 buf : array[0..511 ] of char;

 t  : gsv_text;
 pt : conv_stroke;

 cmd ,num_points1 : unsigned;

 x ,y ,curve_time ,

 max_angle_error_01  ,
 max_angle_error_1   ,
 max_angle_error1    ,
 max_angle_error_10  ,
 max_angle_error_100 ,

 max_error_01  ,
 max_error_1   ,
 max_error1    ,
 max_error_10  ,
 max_error_100 : double;

 a : ellipse;

begin
// Initialize structures
 pixfmt_bgr24(pf ,rbuf_window );

 ren_base.Construct(@pf );
 rgba.ConstrDbl    (1.0 ,1.0 ,0.95 );
 ren_base.clear    (@rgba );

 ren.Construct(@ren_base );

 ras.Construct;
 sl.Construct;

// Render Curve
 path.Construct;

 curve_time:=0;

 path.remove_all;
 curve.Construct;

 curve.approximation_method_(curve_approximation_method_e(m_curve_type._cur_item ) );
 curve.approximation_scale_ (m_approximation_scale._value );

 curve.angle_tolerance_(deg2rad(m_angle_tolerance._value ) );
 curve.cusp_limit_     (deg2rad(m_cusp_limit._value ) );

 curve_time:=measure_time(@curve );

 max_angle_error_01 :=0;
 max_angle_error_1  :=0;
 max_angle_error1   :=0;
 max_angle_error_10 :=0;
 max_angle_error_100:=0;
 max_error_01       :=0;
 max_error_1        :=0;
 max_error1         :=0;
 max_error_10       :=0;
 max_error_100      :=0;

 max_error_01 :=calc_max_error(@curve ,0.01 ,@max_angle_error_01 );
 max_error_1  :=calc_max_error(@curve ,0.1  ,@max_angle_error_1 );
 max_error1   :=calc_max_error(@curve ,1    ,@max_angle_error1 );
 max_error_10 :=calc_max_error(@curve ,10   ,@max_angle_error_10 );
 max_error_100:=calc_max_error(@curve ,100  ,@max_angle_error_100 );

 curve.approximation_scale_(m_approximation_scale._value );
 curve.angle_tolerance_    (deg2rad(m_angle_tolerance._value ) );
 curve.cusp_limit_         (deg2rad(m_cusp_limit._value ) );

 curve.init4(
  m_curve1._x1 ,m_curve1._y1 ,
  m_curve1._x2 ,m_curve1._y2 ,
  m_curve1._x3 ,m_curve1._y3 ,
  m_curve1._x4 ,m_curve1._y4 );

 path.add_path(@curve ,0 ,false );

 stroke.Construct(@path );
 stroke.width_   (m_width._value );

 stroke.line_join_        (m_line_join._cur_item );
 stroke.line_cap_         (m_line_cap._cur_item );
 stroke.inner_join_       (m_inner_join._cur_item );
 stroke.inner_miter_limit_(1.01 );

 ras.add_path    (@stroke );
 rgba.ConstrDbl  (0 ,0.5 ,0 ,0.5 );
 ren.color_      (@rgba );
 render_scanlines(@ras ,@sl ,@ren );

// Render internal points
 num_points1:=0;

 path.rewind(0 );

 cmd:=path.vertex(@x ,@y );

 while not is_stop(cmd ) do
  begin
   if m_show_points._status then
    begin
     ell.Construct(x ,y ,1.5 ,1.5 ,8 );

     ras.add_path    (@ell );
     rgba.ConstrDbl  (0 ,0 ,0 ,0.5 );
     ren.color_      (@rgba );
     render_scanlines(@ras ,@sl ,@ren );

    end;

   inc(num_points1 );

   cmd:=path.vertex(@x ,@y );

  end;

// Render outline
 if m_show_outline._status then
  begin
  // Draw a stroke of the stroke to see the internals
   stroke2.Construct(@stroke );
   ras.add_path     (@stroke2 );
   rgba.ConstrDbl   (0 ,0 ,0 ,0.5 );
   ren.color_       (@rgba);
   render_scanlines (@ras ,@sl ,@ren );

  end;

// Check ellipse and arc for the number of points
 {a.Construct     (100 ,100 ,m_width.value ,m_width.value ,0 );
 ras.add_path    (@a );
 rgba.ConstrDbl  (0.5 ,0 ,0 ,0.5 );
 ren.color       (@rgba );
 render_scanlines(@ras ,@sl ,@ren );

 a.rewind(0 );

 cmd:=a.vertex(@x ,@ y);

 while not is_stop(cmd ) do
  begin
   if is_vertex(cmd ) then
    begin
     ell.Construct   (x ,y ,1.5 ,1.5 ,8 );
     ras.add_path    (@ell );
     rgba.ConstrDbl  (0 ,0 ,0 ,0.5 );
     ren.color       (@rgba );
     render_scanlines(@ras ,@sl ,@ren );

    end;

   cmd:=a.vertex(@x ,@y );

  end;{}

// Render text
 t.Construct;
 t.size_(8.0 );

 pt.Construct (@t );
 pt.line_cap_ (round_cap );
 pt.line_join_(round_join );
 pt.width_    (1.5 );

 sprintf(@buf[0 ]             ,'Num Points=%d ' ,num_points1 );
 sprintf(@buf[StrLen(@buf ) ] ,'Time=%.2fmks'#13#13 ,curve_time );
 sprintf(@buf[StrLen(@buf ) ] ,' Dist Error: x0.01=%.5f ' ,max_error_01 );
 sprintf(@buf[StrLen(@buf ) ] ,'x0.1=%.5f ' ,max_error_1 );
 sprintf(@buf[StrLen(@buf ) ] ,'x1=%.5f ' ,max_error1 );
 sprintf(@buf[StrLen(@buf ) ] ,'x10=%.5f ' ,max_error_10 );
 sprintf(@buf[StrLen(@buf ) ] ,'x100=%.5f'#13#13 ,max_error_100 );
 sprintf(@buf[StrLen(@buf ) ] ,'Angle Error: x0.01=%.1f ' ,max_angle_error_01 );
 sprintf(@buf[StrLen(@buf ) ] ,'x0.1=%.1f ' ,max_angle_error_1 );
 sprintf(@buf[StrLen(@buf ) ] ,'x1=%.1f ' ,max_angle_error1 );
 sprintf(@buf[StrLen(@buf ) ] ,'x10=%.1f ' ,max_angle_error_10 );
 sprintf(@buf[StrLen(@buf ) ] ,'x100=%.1f' ,max_angle_error_100 );

 t.start_point_(10.0 ,85.0 );
 t.text_       (@buf[0 ] );

 ras.add_path    (@pt );
 rgba.ConstrDbl  (0 ,0 ,0 );
 ren.color_      (@rgba );
 render_scanlines(@ras ,@sl ,@ren );

// Render the controls
 render_ctrl(@ras ,@sl ,@ren ,@m_curve1 );
 render_ctrl(@ras ,@sl ,@ren ,@m_angle_tolerance );
 render_ctrl(@ras ,@sl ,@ren ,@m_approximation_scale );
 render_ctrl(@ras ,@sl ,@ren ,@m_cusp_limit );
 render_ctrl(@ras ,@sl ,@ren ,@m_width );
 render_ctrl(@ras ,@sl ,@ren ,@m_show_points );
 render_ctrl(@ras ,@sl ,@ren ,@m_show_outline );
 render_ctrl(@ras ,@sl ,@ren ,@m_curve_type );
 render_ctrl(@ras ,@sl ,@ren ,@m_case_type );
 render_ctrl(@ras ,@sl ,@ren ,@m_inner_join );
 render_ctrl(@ras ,@sl ,@ren ,@m_line_join );
 render_ctrl(@ras ,@sl ,@ren ,@m_line_cap );

// Free AGG resources
 ras.Destruct;
 sl.Destruct;

 path.Destruct;
 curve.Destruct;
 stroke.Destruct;
 stroke2.Destruct;

 t.Destruct;
 pt.Destruct;

end;

{ ON_KEY }
procedure the_application.on_key;
var
 fd  : text;
 buf : array[0..255 ] of char;

begin
 if key = byte(' ' ) then
  begin
   AssignFile(fd ,'coord' );
   rewrite   (fd );

   sprintf(@buf[0 ]             ,'%.3f, ' ,m_curve1._x1 );
   sprintf(@buf[StrLen(@buf ) ] ,'%.3f, ' ,m_curve1._y1 );
   sprintf(@buf[StrLen(@buf ) ] ,'%.3f, ' ,m_curve1._x2 );
   sprintf(@buf[StrLen(@buf ) ] ,'%.3f, ' ,m_curve1._y2 );
   sprintf(@buf[StrLen(@buf ) ] ,'%.3f, ' ,m_curve1._x3 );
   sprintf(@buf[StrLen(@buf ) ] ,'%.3f, ' ,m_curve1._y3 );
   sprintf(@buf[StrLen(@buf ) ] ,'%.3f, ' ,m_curve1._x4 );
   sprintf(@buf[StrLen(@buf ) ] ,'%.3f'   ,m_curve1._y4 );

   write(fd ,PChar(@buf[0 ] ) );
   close(fd );

  end;

 if key = key_f1 then
  message_(
   'Demonstration of new methods of Bezier curve approximation. You can compare '#13 +
   'the old, incremental method with adaptive De Casteljau''s subdivion. The new '#13 +
   'method uses two criteria to stop subdivision: estimation of distance and estimation  '#13 +
   'of angle. It gives us perfectly smooth result even for very sharp turns and loops.  '#13#13 +
   'How to play with:'#13#13 +
   'Use the mouse to change the shape of the curve.'#13 +
   'Press the spacebar to dump the curve''s coordinates into the "coord" file.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

{ ON_CTRL_CHANGE }
procedure the_application.on_ctrl_change;
var
 w ,h : int;

begin
 if m_case_type._cur_item <> m_cur_case_type then
  begin
   case m_case_type._cur_item of
    0 : //m_case_type.add_item("Random");
     begin
      w:=trunc(_width - 120 );
      h:=trunc(_height - 80 );

      m_curve1.curve_(
       Random($7fff ) mod w ,Random($7fff ) mod h + 80 ,
       Random($7fff ) mod w ,Random($7fff ) mod h + 80 ,
       Random($7fff ) mod w ,Random($7fff ) mod h + 80 ,
       Random($7fff ) mod w ,Random($7fff ) mod h + 80 );

     end;

    1 : //m_case_type.add_item("13---24");
     m_curve1.curve_(150 ,150 ,350 ,150 ,150 ,150 ,350 ,150 );

    2 : //m_case_type.add_item("Smooth Cusp 1");
     m_curve1.curve_(50 ,142 ,483 ,251 ,496 ,62 ,26 ,333 );

    3 : //m_case_type.add_item("Smooth Cusp 2");
     m_curve1.curve_(50 ,142 ,484 ,251 ,496 ,62 ,26 ,333 );

    4 : //m_case_type.add_item("Real Cusp 1");
     m_curve1.curve_(100 ,100 ,300 ,200 ,200 ,200 ,200 ,100 );

    5 : //m_case_type.add_item("Real Cusp 2");
     m_curve1.curve_(475 ,157 ,200 ,100 ,453 ,100 ,222 ,157 );

    6 : //m_case_type.add_item("Fancy Stroke");
     begin
      m_curve1.curve_(129 ,233 ,32 ,283 ,258 ,285 ,159 ,232 );
      m_width.value_ (100 );

     end;

    7 : //m_case_type.add_item("Jaw");
     m_curve1.curve_(100 ,100 ,300 ,200 ,264 ,286 ,264 ,284 );

    8 : //m_case_type.add_item("Ugly Jaw");
     m_curve1.curve_(100 ,100 ,413 ,304 ,264 ,286 ,264 ,284 );

   end;

  force_redraw;

  m_cur_case_type:=m_case_type._cur_item;

 end;

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example (F1-Help)' );

 if app.init(655 ,520 ,window_resize ) then
  app.run;

 app.Destruct;

END.