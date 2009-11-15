{mac_copy:agg.bmp}
//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 pattern_perspective ;

uses
 Math ,SysUtils ,

 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,

 agg_ctrl ,
 agg_rbox_ctrl ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_u ,
 agg_render_scanlines ,

 agg_path_storage ,
 agg_conv_transform ,
 agg_trans_affine ,
 agg_trans_bilinear ,
 agg_trans_perspective ,
 agg_span_pattern ,
 agg_span_allocator ,
 agg_image_filters ,
 agg_span_interpolator_linear ,
 agg_span_interpolator_trans ,
 agg_span_image_filter ,
 agg_span_pattern_filter_rgb ,
 interactive_polygon_ ,
 file_utils_ ;

{$I agg_mode.inc }

const
 flip_y = true;

var
 g_x1 ,
 g_y1 ,
 g_x2 ,
 g_y2 : double;

 g_rasterizer : rasterizer_scanline_aa;
 g_scanline   : scanline_u8;
 
type
 the_application = object(platform_support )
   m_quad       : interactive_polygon;
   m_trans_type : rbox_ctrl;
   m_test_flag  : boolean;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_init; virtual;
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

 m_quad.Construct      (4 ,5.0 );
 m_trans_type.Construct(460 ,5.0 ,420 + 170.0 ,60.0 ,not flip_y_ );

 m_test_flag:=false;

 m_trans_type.text_size_     (8 );
 m_trans_type.text_thickness_(1 );

 m_trans_type.add_item ('Affine' );
 m_trans_type.add_item ('Bilinear' );
 m_trans_type.add_item ('Perspective' );
 m_trans_type.cur_item_(2 );

 add_ctrl(@m_trans_type );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_quad.Destruct;
 m_trans_type.Destruct;

end;

{ ON_INIT }
procedure the_application.on_init;
var
 trans_x1 ,trans_y1 ,trans_x2 ,trans_y2 ,dx ,dy : double;

begin
 g_x1:=-150;
 g_y1:=-150;
 g_x2:=150;
 g_y2:=150;

 trans_x1:=-200;
 trans_y1:=-200;
 trans_x2:=200;
 trans_y2:=200;

 dx:=_width  / 2.0 - (trans_x2 + trans_x1 ) / 2.0;
 dy:=_height / 2.0 - (trans_y2 + trans_y1 ) / 2.0;

 m_quad.xn_ptr(0 )^:=Floor(trans_x1 + dx );
 m_quad.yn_ptr(0 )^:=Floor(trans_y1 + dy );
 m_quad.xn_ptr(1 )^:=Floor(trans_x2 + dx );// - 150;
 m_quad.yn_ptr(1 )^:=Floor(trans_y1 + dy );// + 150;
 m_quad.xn_ptr(2 )^:=Floor(trans_x2 + dx );
 m_quad.yn_ptr(2 )^:=Floor(trans_y2 + dy );
 m_quad.xn_ptr(3 )^:=Floor(trans_x1 + dx );
 m_quad.yn_ptr(3 )^:=Floor(trans_y2 + dy );

end;

{ ON_DRAW }
procedure the_application.on_draw;
const
 subdiv_shift = 2;

var
 pixf ,pixf_pre : pixel_formats;

 rb ,rb_pre : renderer_base;

 rgba : aggclr;

 r  : renderer_scanline_aa_solid;
 sa : span_allocator;
 fi : image_filter_hanning;
 sg : span_image_filter_ptr;
 ri : renderer_scanline_aa;
 wx ,
 wy : wrap_mode_reflect_auto_pow2;

 interpolator : span_interpolator_linear;
 interpsubdiv : span_interpolator_linear_subdiv;

 filter : image_filter;

 tr  : trans_affine;
 trb : trans_bilinear;
 trp : trans_perspective23;

begin
 sg:=NIL;

// Initialize structures
 pixfmt_bgr24    (pixf ,rbuf_window );
 pixfmt_bgr24_pre(pixf_pre ,rbuf_window );

 rb.Construct    (@pixf );
 rb_pre.Construct(@pixf_pre );

 r.Construct(@rb );

 if not m_test_flag then
  begin
   rgba.ConstrDbl(1 ,1 ,1 );
   rb.clear      (@rgba );

  end;

 if m_trans_type._cur_item = 0 then
  begin
  // For the affine parallelogram transformations we
  // calculate the 4-th (implicit) point of the parallelogram
   m_quad.xn_ptr(3 )^:=m_quad.xn(0 ) + (m_quad.xn(2 ) - m_quad.xn(1 ) );
   m_quad.yn_ptr(3 )^:=m_quad.yn(0 ) + (m_quad.yn(2 ) - m_quad.yn(1 ) );

  end;

 if not m_test_flag then
  begin
  // Render the "quad" tool
   g_rasterizer.add_path(@m_quad );

   rgba.ConstrDbl  (0 ,0.3 ,0.5 ,0.6 );
   r.color_        (@rgba );
   render_scanlines(@g_rasterizer ,@g_scanline ,@r );

  // Render the controls
   render_ctrl(@g_rasterizer ,@g_scanline ,@r ,@m_trans_type );

  end;

// Prepare the polygon to rasterize. Here we need to fill
// the destination (transformed) polygon.
 g_rasterizer.clip_box(0 ,0 ,_width ,_height );
 g_rasterizer.reset;
 g_rasterizer.move_to_d(m_quad.xn(0 ) ,m_quad.yn(0 ) );
 g_rasterizer.line_to_d(m_quad.xn(1 ) ,m_quad.yn(1 ) );
 g_rasterizer.line_to_d(m_quad.xn(2 ) ,m_quad.yn(2 ) );
 g_rasterizer.line_to_d(m_quad.xn(3 ) ,m_quad.yn(3 ) );

 sa.Construct;
 fi.Construct;
 filter.Construct(@fi );

// Render
 wx.Construct;
 wy.Construct;

 case m_trans_type._cur_item of
  0 :
   begin
   // Note that we consruct an affine matrix that transforms
   // a parallelogram to a rectangle, i.e., it's inverted.
   // It's actually the same as:
   // tr(g_x1, g_y1, g_x2, g_y2, m_triangle.polygon());
   // tr.invert();
    tr.Construct(parallelo_ptr(m_quad.polygon ) ,g_x1 ,g_y1 ,g_x2 ,g_y2 );

   // Also note that we can use the linear interpolator instead of
   // arbitrary span_interpolator_trans. It works much faster,
   // but the transformations must be linear and parellel.
    interpolator.Construct(@tr );

    sg:=new(
     span_pattern_filter_rgb_2x2_ptr ,
     Construct(
      @sa ,rbuf_img(0 ) ,@interpolator ,@filter ,@wx ,@wy ,bgr_order ) );

    ri.Construct    (@rb_pre ,sg );
    render_scanlines(@g_rasterizer ,@g_scanline ,@ri );

   end;

  1 :
   begin
    trb.Construct(m_quad.polygon ,g_x1 ,g_y1 ,g_x2 ,g_y2 );

    if trb.is_valid then
     begin
      interpolator.Construct(@trb );

      sg:=new(
       span_pattern_filter_rgb_2x2_ptr ,
       Construct(
        @sa ,rbuf_img(0 ) ,@interpolator ,@filter ,@wx ,@wy ,bgr_order ) );

      ri.Construct    (@rb_pre ,sg );
      render_scanlines(@g_rasterizer ,@g_scanline ,@ri );

     end;

   end;

  2 :
   begin
    trp.Construct(m_quad.polygon ,g_x1 ,g_y1 ,g_x2 ,g_y2 );

    if trp.is_valid then
     begin
      interpsubdiv.Construct(@trp );

      sg:=new(
       span_pattern_filter_rgb_2x2_ptr ,
       Construct(
        @sa ,rbuf_img(0 ) ,@interpsubdiv ,@filter ,@wx ,@wy ,bgr_order ) );

      ri.Construct    (@rb_pre ,sg );
      render_scanlines(@g_rasterizer ,@g_scanline ,@ri );

     end;

   end;

 end;

// Free AGG resources
 sa.Destruct;
 filter.Destruct;

 if sg <> NIL then
  dispose(sg ,Destruct );

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 if flags and mouse_left <> 0 then
  if m_quad.on_mouse_move(x ,y ) then
   force_redraw();

 if flags and mouse_left = 0 then
  on_mouse_button_up(x ,y ,flags );

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
var
 buf : array[0..99 ] of char;

begin
 if flags and mouse_left <> 0 then
  if m_quad.on_mouse_button_down(x ,y ) then
   force_redraw
  else
   begin
    start_timer;

    m_test_flag:=true;

    on_draw;
    on_draw;
    on_draw;
    on_draw;

    sprintf(@buf[0 ] ,'time=%.3f' ,elapsed_time );

    m_test_flag:=false;

    force_redraw;

    message_(@buf[0 ] );

   end;

end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
 if m_quad.on_mouse_button_up(x ,y ) then
  force_redraw;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'Pattern perspective transformations. Essentially it''s the same as Demo '#13 +
   '"image_perspective", but working with a repeating pattern. Can be used '#13 +
   'for texturing.'#13#13 +
   'How to play with:'#13#13 +
   'Use the left mouse button to move and distort the pattern.'#13 +
   'Click the left mouse outside the pattern to run the performance test.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;
 buf : array [0..255 ] of char;
 ext : string[10 ];

 img_name ,p ,n ,x : shortstring;

BEGIN
 g_x1:=0;
 g_y1:=0;
 g_x2:=0;
 g_y2:=0;

 g_rasterizer.Construct;
 g_scanline.Construct;

 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. Pattern Perspective Transformations (F1-Help)' );

 img_name:='agg';

{$IFDEF WIN32 }
 if ParamCount > 0 then
  begin
   spread_name(ParamStr(1 ) ,p ,n ,x );

   img_name:=fold_name(p ,n ,'' );

  end;

{$ENDIF }

 if not app.load_img(0 ,img_name ) then
  begin
   img_name:=img_name + #0;
   ext     :=app._img_ext + #0;

   if img_name = 'spheres' then
    begin
     sprintf(@buf[0 ]             ,'File not found: %s' ,ptrcomp(@img_name[1 ] ) );
     sprintf(@buf[StrLen(@buf ) ] ,'%s. Download http://www.antigrain.com/' ,ptrcomp(@ext[1 ] ) );
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
  if app.init(600 ,600 ,window_resize ) then
   app.run;

 app.Destruct;

 g_rasterizer.Destruct;
 g_scanline.Destruct;

END.