{mac_copy:shapes.txt}
//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 flash_rasterizer ;

uses
 SysUtils ,
 agg_basics ,
 agg_array ,
 agg_color ,
 agg_platform_support ,
 agg_rendering_buffer ,
 agg_trans_viewport ,
 agg_path_storage ,
 agg_conv_transform ,
 agg_conv_curve ,
 agg_conv_stroke ,
 agg_gsv_text ,
 agg_scanline_u ,
 agg_scanline_bin ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_render_scanlines ,
 agg_rasterizer_scanline_aa ,
 agg_rasterizer_compound_aa ,
 agg_span_allocator ,
 agg_gamma_lut ,
 agg_pixfmt ,
 agg_pixfmt_rgba ,
 agg_bounding_rect ,
 agg_vertex_source ,
 agg_trans_affine ,
 agg_math ,
 agg_math_stroke ,
 file_utils_ ;

{$I agg_mode.inc }

const
 flip_y = false;

type
 path_style_ptr = ^path_style;
 path_style = record
   path_id : unsigned;

   left_fill  ,
   right_fill ,
   line       : int;

  end;

 compound_shape = object(vertex_source )
  private
   m_path   : path_storage;
   m_affine : trans_affine;
   m_curve  : conv_curve;
   m_trans  : conv_transform;
   m_styles : pod_bvector;

   m_x1 ,
   m_y1 ,
   m_x2 ,
   m_y2 : double;

   m_fd : api_file;

  public
   constructor Construct;
   destructor  Destruct; virtual;

   function  open(fname : AnsiString ) : boolean;
   function  read_next : boolean;

   function  operator_array(i : unsigned ) : unsigned; virtual;
   function  paths : unsigned;
   function  style(i : unsigned ) : path_style_ptr;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

   function  scale : double; overload;
   procedure scale(w ,h : double ); overload;

   procedure approximation_scale(s : double );

   function  hit_test(x ,y ,r : double ) : int;

   procedure modify_vertex(i : unsigned; x ,y : double );

  end;

// Testing class, color provider and span generator
 test_styles = object(style_handler )
  private
   m_solid_colors ,
   m_gradient     : aggclr_ptr;

  public
   constructor Construct(solid_colors ,gradient : aggclr_ptr );

   function  is_solid(style : unsigned ) : boolean; virtual;
   function  color   (style : unsigned ) : aggclr_ptr; virtual;

   procedure generate_span(span : aggclr_ptr; x ,y : int; len ,style : unsigned ); virtual;

  end;

 the_application = object(platform_support )
  private
   m_shape    : compound_shape;
   m_colors   : array[0..99 ] of aggclr;
   m_scale    : trans_affine;
   m_gamma    : gamma_lut;
   m_gradient : pod_array;

   m_point_idx ,
   m_hit_x     ,
   m_hit_y     : int;

  public
   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   function  open(fname : AnsiString ) : boolean;
   procedure read_next;

   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor compound_shape.Construct;
begin
 m_path.Construct;
 m_affine.Construct;
 m_curve.Construct (@m_path );
 m_trans.Construct (@m_curve ,@m_affine );
 m_styles.Construct(sizeof(path_style ) );

end;

{ DESTRUCT }
destructor compound_shape.Destruct;
begin
 m_path.Destruct;
 m_curve.Destruct;
 m_styles.Destruct;

 if m_fd.isOpened then
  api_close_file(m_fd );

end;

{ OPEN }
function compound_shape.open(fname : AnsiString ) : boolean;
begin
 result:=api_open_file(m_fd ,fname );

end;

{ fgets }
function fgets(buf : char_ptr; max : int; var f : api_file ) : char_ptr;
var
 read : int;

begin
 result:=buf;

 while max > 1 do
  begin
   if not api_read_file(f ,buf ,1 ,read ) then
    begin
     result:=NIL;

     exit;

    end;

   if read = 0 then
    if buf = result then
     begin
      result:=NIL;

      exit;

     end
    else
     break;

   case buf^ of
    #13 ,#10 ,#9 :
     break;

   end;

   dec(max );
   inc(ptrcomp(buf ) ,read );

  end;

 if max >= 1 then
  buf^:=#0;

end;

var
 g_buff : char_ptr;

{ strtok }
function strtok(buff : char_ptr ) : char_ptr;
begin
 result:=NIL;

 if buff <> NIL then
  g_buff:=buff;

 while (g_buff <> NIL ) and
       (g_buff^ <> #0 ) do
  begin
   if result = NIL then
    result:=g_buff;

   case g_buff^ of
    ' ' ,#13 ,#10 :
     begin
      g_buff^:=#0;

      inc(ptrcomp(g_buff ) );

      break;

     end;

   end;

   inc(ptrcomp(g_buff ) );

  end;

end;

{ READ_NEXT }
function compound_shape.read_next : boolean;
var
 ax ,ay ,cx ,cy : double;

 buf : array[0..1023 ] of char;

 ts : char_ptr;

 style_ : path_style;

 code : int;

begin
 m_path.remove_all;
 m_styles.remove_all;

 if m_fd.isOpened then
  begin
   repeat
    if fgets(@buf[0 ] ,1022 ,m_fd ) = NIL then
     begin
      result:=false;

      exit;

     end;

    if buf[0 ] = '=' then
     break;

   until false;

   while fgets(@buf[0 ] ,1022 ,m_fd ) <> NIL do
    begin
     if buf[0 ] = '!' then
      break;

     if buf[0 ] = 'P' then
      begin
      // BeginPath
       style_.path_id:=m_path.start_new_path;

       ts:=strtok(@buf[0 ] ); // Path;
       ts:=strtok(NIL ); // left_style

       Val(PChar(ts ) ,style_.left_fill ,code );

       ts:=strtok(NIL ); // right_style

       Val(PChar(ts ) ,style_.right_fill ,code );

       ts:=strtok(NIL ); // line_style

       Val(PChar(ts ) ,style_.line ,code );

       ts:=strtok(NIL ); // ax

       Val(PChar(ts ) ,ax ,code );

       ts:=strtok(NIL ); // ay

       Val(PChar(ts ) ,ay ,code );

       m_path.move_to(ax ,ay );
       m_styles.add  (@style_ );

      end;

     if buf[0 ] = 'C' then
      begin
       ts:=strtok(@buf[0 ] ); // Curve;
       ts:=strtok(NIl ); // cx

       Val(PChar(ts ) ,cx ,code );

       ts:=strtok(NIL ); // cy

       Val(PChar(ts ) ,cy ,code );

       ts:=strtok(NIL ); // ax

       Val(PChar(ts ) ,ax ,code );

       ts:=strtok(NIL ); // ay

       Val(PChar(ts ) ,ay ,code );

       m_path.curve3(cx ,cy ,ax ,ay );

      end;

     if buf[0 ] = 'L' then
      begin
       ts:=strtok(@buf[0 ] ); // Line;
       ts:=strtok(NIL ); // ax

       Val(PChar(ts ) ,ax ,code );

       ts:=strtok(NIL ); // ay

       Val(PChar(ts ) ,ay ,code );

       m_path.line_to(ax ,ay );

      end;

     if buf[0 ] = '<' then
      begin
      // EndPath
      end;

    end;

   result:=true;

  end
 else
  result:=false;

end;

{ OPERATOR_ARRAY }
function compound_shape.operator_array(i : unsigned ) : unsigned;
begin
 result:=path_style_ptr(m_styles.array_operator(i ) )^.path_id;

end;

{ PATHS }
function compound_shape.paths : unsigned;
begin
 result:=m_styles.size;

end;

{ STYLE }
function compound_shape.style(i : unsigned ) : path_style_ptr;
begin
 result:=path_style_ptr(m_styles.array_operator(i ) );

end;

{ REWIND }
procedure compound_shape.rewind(path_id : unsigned );
begin
 m_trans.rewind(path_id );

end;

{ VERTEX }
function compound_shape.vertex(x ,y : double_ptr ) : unsigned;
begin
 result:=m_trans.vertex(x ,y );

end;

{ SCALE }
function compound_shape.scale : double;
begin
 result:=m_affine.scale;

end;

{ SCALE }
procedure compound_shape.scale(w ,h : double );
var
 x1 ,y1 ,x2 ,y2 : double;

 vp : trans_viewport;

begin
 m_affine.reset;

 bounding_rect_vs(
  @m_path ,@self ,0 ,m_styles.size ,
  @x1 ,@y1 ,@x2 ,@y2 );

 if (x1 < x2 ) and
    (y1 < y2 ) then
  begin
   vp.Construct;
   vp.preserve_aspect_ratio(0.5 ,0.5 ,aspect_ratio_meet );

   vp.world_viewport (x1 ,y1 ,x2 ,y2 );
   vp.device_viewport(0  ,0  ,w  ,h );
   vp.to_affine      (@m_affine );

  end;

 m_curve.approximation_scale_(m_affine.scale );

end;

{ APPROXIMATION_SCALE }
procedure compound_shape.approximation_scale(s : double );
begin
 m_curve.approximation_scale_(m_affine.scale * s );

end;

{ HIT_TEST }
function compound_shape.hit_test(x ,y ,r : double ) : int;
var
 i ,cmd : unsigned;
 vx ,vy : double;

begin
 m_affine.inverse_transform(@m_affine ,@x ,@y );

 r:=r / m_affine.scale;
 i:=0;

 while i < m_path.total_vertices do
  begin
   cmd:=m_path.vertex_(i ,@vx ,@vy );

   if is_vertex(cmd ) then
    if calc_distance(x ,y ,vx ,vy ) <= r then
     begin
      result:=i;

      exit;

     end;

   inc(i );

  end;

 result:=-1;

end;

{ MODIFY_VERTEX }
procedure compound_shape.modify_vertex(i : unsigned; x ,y : double );
begin
 m_affine.inverse_transform(@m_affine ,@x ,@y );
 m_path.modify_vertex      (i ,x ,y );

end;

{ CONSTRUCT }
constructor test_styles.Construct(solid_colors ,gradient : aggclr_ptr );
begin
 m_solid_colors:=solid_colors;
 m_gradient    :=gradient;

end;

{ IS_SOLID }
// Suppose that style=1 is a gradient
function test_styles.is_solid(style : unsigned ) : boolean;
begin
 result:=style <> 1; //true;

end;

{ COLOR }
// Just returns a color
function test_styles.color(style : unsigned ) : aggclr_ptr;
begin
 result:=aggclr_ptr(ptrcomp(m_solid_colors ) + style * sizeof(aggclr ) );

end;

{ GENERATE_SPAN }
// Generate span. In our test case only one style (style=1)
// can be a span generator, so that, parameter "style"
// isn't used here.
procedure test_styles.generate_span(span : aggclr_ptr; x ,y : int; len ,style : unsigned );
begin
 move(
  aggclr_ptr(ptrcomp(m_gradient ) + x * sizeof(aggclr ) )^ ,
  span^ ,
  sizeof(aggclr ) * len );

end;

{ CONSTRUCT }
constructor the_application.Construct;
var
 i : unsigned;

 c1 ,c2 ,c3 : int;

begin
 inherited Construct(format_ ,flip_y_ );

 m_shape.Construct;
 m_scale.Construct;
 m_gamma.Construct_;
 m_gradient.Construct(sizeof(aggclr ) );

 m_gamma.gamma_(2.0 );

 m_point_idx:=-1;
 m_hit_x    :=-1;
 m_hit_y    :=-1;

 i:=0;

 while i < 100 do
  begin
   c1:=rand and $FF;
   c2:=rand and $FF;
   c3:=rand and $FF;

   m_colors[i ].ConstrInt(c3 ,c2 ,c1 ,230 );

   m_colors[i ].apply_gamma_dir(@m_gamma );
   m_colors[i ].premultiply;

   inc(i );

  end;

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_shape.Destruct;
 m_gamma.Destruct;
 m_gradient.Destruct;

end;

{ OPEN }
function the_application.open(fname : AnsiString ) : boolean;
begin
 result:=m_shape.open(full_file_name(fname ) );

end;

{ READ_NEXT }
procedure the_application.read_next;
begin
 m_shape.read_next;
 m_shape.scale(_width ,_height );

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;

 rgba ,c1 ,c2 : aggclr;

 ren_base : renderer_base;

 ren : renderer_scanline_aa_solid;
 ras : rasterizer_scanline_aa;

 rasc : rasterizer_compound_aa_dbl;

 sl     : scanline_u8;
 sl_bin : scanline_bin;

 i ,w : unsigned;

 shape  : conv_transform;
 stroke : conv_stroke;

 style_handler : test_styles;

 alloc : span_allocator;

 tfill ,tstroke : double;

 draw_strokes : boolean;

 buf : array[0..255 ] of char;

 t  : gsv_text;
 ts : conv_stroke;

begin
// Initialize structures
 pixfmt_bgra32_pre(pixf ,rbuf_window );

 ren_base.Construct(@pixf );

 rgba.ConstrDbl(1.0 ,1.0 ,0.95 );
 ren_base.clear(@rgba );

 ren.Construct(@ren_base );

 w:=Trunc(_width );

 m_gradient.resize(w );

 c1.ConstrInt(255 ,0 ,0   ,180 );
 c2.ConstrInt(0   ,0 ,255 ,180 );

 i:=0;

 while i < w do
  begin
   aggclr_ptr(m_gradient.array_operator(i  ) )^:=c1.gradient(@c2 ,i / _width );

   aggclr_ptr(m_gradient.array_operator(i ) ).premultiply;

   inc(i );

  end;

 ras.Construct;
 rasc.Construct;

 sl.Construct;
 sl_bin.Construct;

 shape.Construct (@m_shape ,@m_scale );
 stroke.Construct(@shape );

 style_handler.Construct(@m_colors[0 ] ,m_gradient.data );

 alloc.Construct;

 m_shape.approximation_scale(m_scale.scale );

// Fill shape
 rasc.clip_box(0 ,0 ,_width ,_height );
 rasc.reset;
// rasc.filling_rule(fill_even_odd );

 start_timer;

 i:=0;

 while i < m_shape.paths do
  begin
   if (m_shape.style(i ).left_fill >= 0 ) or
      (m_shape.style(i ).right_fill >= 0 ) then
    begin
     rasc.styles(
      m_shape.style(i ).left_fill ,
      m_shape.style(i ).right_fill );

     rasc.add_path(@shape ,m_shape.style(i ).path_id );

    end;

   inc(i );

  end;

 render_scanlines_compound(@rasc ,@sl ,@sl_bin ,@ren_base ,@alloc ,@style_handler );

 tfill:=elapsed_time();

// Hit-test test
 draw_strokes:=true;

 if (m_hit_x >= 0 ) and
    (m_hit_y >= 0 ) then
  if rasc.hit_test(m_hit_x ,m_hit_y ) then
   draw_strokes:=false;

// Draw strokes
 start_timer;

 if draw_strokes then
  begin
   ras.clip_box(0 ,0 ,_width ,_height );

   stroke.width_    (Sqrt(m_scale.scale ) );
   stroke.line_join_(round_join );
   stroke.line_cap_ (round_cap );

   i:=0;

   while i < m_shape.paths do
    begin
     ras.reset;

     if m_shape.style(i ).line >= 0 then
      begin
       ras.add_path(@stroke ,m_shape.style(i).path_id );

       rgba.ConstrInt  (0 ,0 ,0 ,128 );
       ren.color_      (@rgba );
       render_scanlines(@ras ,@sl ,@ren );

      end;

     inc(i );

    end;

  end;

 tstroke:=elapsed_time;

// Render Text
 t.Construct;
 t.size_(8.0 );
 t.flip_(true );

 ts.Construct(@t );
 ts.width_   (1.6 );
 ts.line_cap_(round_cap );

 sprintf(@buf[0 ]             ,'Fill=%.2fms ' ,tfill );
 sprintf(@buf[StrLen(@buf ) ] ,'(%dFPS) ' ,Trunc(1000.0 / tfill ) );
 sprintf(@buf[StrLen(@buf ) ] ,'Stroke=%.2fms ' ,tstroke );
 sprintf(@buf[StrLen(@buf ) ] ,'(%dFPS) ' ,Trunc(1000.0 / tstroke ) );
 sprintf(@buf[StrLen(@buf ) ] ,'Total=%.2fms ' ,tfill + tstroke );
 sprintf(@buf[StrLen(@buf ) ] ,
  '(%dFPS)'#13#13 +
  'Space: Next Shape'#13#13 +
  '+/- : ZoomIn/ZoomOut (with respect to the mouse pointer)' ,
  Trunc(1000.0 / (tfill + tstroke ) ) );

 t.start_point_(10.0 ,20.0 );
 t.text_       (@buf[0 ] );

 ras.add_path  (@ts );
 rgba.ConstrDbl(0 ,0 ,0 );
 ren.color_    (@rgba );

 render_scanlines(@ras ,@sl ,@ren );

// Gamma adjust
 if m_gamma._gamma <> 1.0 then
  pixf.apply_gamma_inv(@m_gamma ,bgra_order );

// Free AGG resources
 ras.Destruct;
 rasc.Destruct;
 sl.Destruct;
 sl_bin.Destruct;

 shape.Destruct;
 stroke.Destruct;

 alloc.Destruct;

 t.Destruct;
 ts.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
var
 xd ,yd : double;

begin
 if flags and 3 = 0 then
  on_mouse_button_up(x ,y ,flags )

 else
  if m_point_idx >= 0 then
   begin
    xd:=x;
    yd:=y;

    m_scale.inverse_transform(@m_scale ,@xd ,@yd );
    m_shape.modify_vertex    (m_point_idx ,xd ,yd );

    force_redraw;

   end;

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
var
 xd ,yd ,r : double;
 
begin
 if flags and 1 <> 0 then
  begin
   xd:=x;
   yd:=y;
   r :=4.0 / m_scale.scale;

   m_scale.inverse_transform(@m_scale ,@xd ,@yd );

   m_point_idx:=m_shape.hit_test(xd ,yd ,r );

   force_redraw;

  end;

 if flags and 2 <> 0 then
  begin
   m_hit_x:=x;
   m_hit_y:=y;

   force_redraw;

  end;

end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
 m_point_idx:=-1;
 m_hit_x    :=-1;
 m_hit_y    :=-1;

 force_redraw;

end;

{ ON_KEY }
procedure the_application.on_key;
var
 tat : trans_affine_translation;
 tas : trans_affine_scaling;
 tar : trans_affine_rotation;

begin
 if key = key_f1 then
  message_(
   'Demonstration of Flash compound shape rasterizer. The rasterizer accepts '#13 +
   'vectorial data in a form of Flash paths, that is, with two fill styles, '#13 +
   'fill on the left and fill on the right of the path. Then it produces a number '#13 +
   'of scanlines with corresponding styles and requests for the colors and/or '#13 +
   'gradients, images, etc. The algorithm takes care of anti-aliasing and perfect '#13 +
   'stitching between fill areas.'#13#13 +
   'How to play with:'#13#13 +
   'Space = Load next shape'#13 +
   '+ & - Key = ZoomIn/ZoomOut (with respect to the mouse pointer)'#13 +
   'Right & Left Key = Rotate (with respect to the mouse pointer)'#13 +
   'Left click & drag to modify shape points' );

 if key = unsigned(' ' ) then
  begin
   m_shape.read_next;
   m_shape.scale(_width ,_height );
   force_redraw;

  end;

 if (key = unsigned('+' ) ) or
    (key = key_kp_plus ) then
  begin
   tat.Construct   (-x ,-y );
   m_scale.multiply(@tat );
   tas.Construct   (1.1 );
   m_scale.multiply(@tas );
   tat.Construct   (x ,y );
   m_scale.multiply(@tat );

   force_redraw;

  end;

 if (key = unsigned('-' ) ) or
    (key = key_kp_minus ) then
  begin
   tat.Construct   (-x ,-y );
   m_scale.multiply(@tat );
   tas.Construct   (1 / 1.1 );
   m_scale.multiply(@tas );
   tat.Construct   (x ,y );
   m_scale.multiply(@tat );

   force_redraw;

  end;

 if key = key_left then
  begin
   tat.Construct   (-x ,-y );
   m_scale.multiply(@tat );
   tar.Construct   (-pi / 20.0 );
   m_scale.multiply(@tar );
   tat.Construct   (x ,y );
   m_scale.multiply(@tat );

   force_redraw;

  end;

 if key = key_right then
  begin
   tat.Construct   (-x ,-y );
   m_scale.multiply(@tat );
   tar.Construct   (pi / 20.0 );
   m_scale.multiply(@tar );
   tat.Construct   (x ,y );
   m_scale.multiply(@tat );

   force_redraw;

  end;

end;

VAR
 app : the_application;
 buf : array[0..255 ] of char;

 fname ,p ,n ,x : shortstring;

BEGIN
 app.Construct(pix_format_bgra32 ,flip_y );
 app.caption_ ('AGG Example - Flash Rasterizer (F1-Help)' );

 fname:='shapes.txt';

{$IFDEF WIN32 }
 if ParamCount > 0 then
  begin
   spread_name(ParamStr(1 ) ,p ,n ,x );

   fname:=fold_name(p ,n ,x );

  end;

{$ENDIF }

 if not app.open(fname ) then
  begin
   fname:=fname + #0;

   if StrComp(@fname[1 ] ,'shapes.txt'#0 ) = 0 then
    begin
     sprintf(@buf[0 ] ,'File not found: %s. '#13 ,unsigned(@fname[1 ] ) );
     sprintf(
      @buf[StrLen(@buf ) ] ,
      'Download http://www.antigrain.com/%s'#13 +
      'or copy it from another directory if available.' ,
      unsigned(@fname[1 ] ) );

    end
   else
    sprintf(@buf[0 ] ,'File not found: %s' ,unsigned(@fname[1 ] ) );

   app.message_(@buf[0 ] );

  end
 else
  if app.init(655 ,520 ,window_resize ) then
   begin
    app.read_next;
    app.run;

   end;

 app.Destruct;

END.