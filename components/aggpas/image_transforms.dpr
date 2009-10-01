{mac_copy:spheres.bmp}
//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 image_transforms ;

uses
 SysUtils ,

 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgba ,

 agg_ctrl ,
 agg_cbox_ctrl ,
 agg_rbox_ctrl ,
 agg_slider_ctrl ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_u ,
 agg_render_scanlines ,

 agg_ellipse ,
 agg_path_storage ,
 agg_trans_affine ,
 agg_conv_transform ,
 agg_conv_stroke ,
 agg_image_filters ,
 agg_span_allocator ,
 agg_span_image_filter ,
 agg_span_image_filter_rgba ,
 agg_span_interpolator_linear ,
 file_utils_ ;

{$I agg_mode.inc }

const
 flip_y = true;

type
 the_application = object(platform_support )
   m_polygon_angle ,
   m_polygon_scale ,

   m_image_angle   ,
   m_image_scale   : slider_ctrl;

   m_rotate_polygon  ,
   m_rotate_image    : cbox_ctrl;

   m_example : rbox_ctrl;

   m_image_center_x ,
   m_image_center_y ,

   m_polygon_cx ,
   m_polygon_cy ,
   m_image_cx   ,
   m_image_cy   ,

   m_dx   ,
   m_dy   : double;
   m_flag : int;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure create_star(ps : path_storage_ptr );

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_ctrl_change; virtual;
   procedure on_idle; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_polygon_angle.Construct (5 ,5 ,145 ,11 ,not flip_y_ );
 m_polygon_scale.Construct (5 ,5 + 14 ,145 ,12 + 14 ,not flip_y_ );
 m_image_angle.Construct   (155 ,5 ,300 ,12 ,not flip_y_ );
 m_image_scale.Construct   (155 ,5 + 14 ,300 ,12 + 14 ,not flip_y_ );
 m_rotate_polygon.Construct(5 ,5 + 14 + 14 ,'Rotate Polygon' ,not flip_y_ );
 m_rotate_image.Construct  (5 ,5 + 14 + 14 + 14 ,'Rotate Image' ,not flip_y_ );
 m_example.Construct       (-3.0 ,14 + 14 + 14 + 14 ,-3.0 ,14 + 14 + 14 + 14 ,not flip_y_ );

 m_flag:=0;

 add_ctrl(@m_polygon_angle );
 add_ctrl(@m_polygon_scale );
 add_ctrl(@m_image_angle );
 add_ctrl(@m_image_scale );
 add_ctrl(@m_rotate_polygon );
 add_ctrl(@m_rotate_image );
 add_ctrl(@m_example );

 m_polygon_angle.label_('Polygon Angle=%3.2f' );
 m_polygon_scale.label_('Polygon Scale=%3.2f' );
 m_polygon_angle.range_(-180.0 ,180.0 );
 m_polygon_scale.range_(0.1 ,5.0 );
 m_polygon_scale.value_(1.0 );

 m_image_angle.label_('Image Angle=%3.2f' );
 m_image_scale.label_('Image Scale=%3.2f' );
 m_image_angle.range_(-180.0, 180.0);
 m_image_scale.range_(0.1 ,5.0 );
 m_image_scale.value_(1.0 );

 m_example.add_item ('0' );
 m_example.add_item ('1' );
 m_example.add_item ('2' );
 m_example.add_item ('3' );
 m_example.add_item ('4' );
 m_example.add_item ('5' );
 m_example.add_item ('6' );
 m_example.cur_item_(0 );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_polygon_angle.Destruct;
 m_polygon_scale.Destruct;
 m_image_angle.Destruct;
 m_image_scale.Destruct;
 m_rotate_polygon.Destruct;
 m_rotate_image.Destruct;
 m_example.Destruct;

end;

{ CREATE_STAR }
procedure the_application.create_star;
var
 r ,r1 ,r2 ,a ,dx ,dy : double;

 nr ,i : unsigned;

begin
 r:=_initial_width;

 if _initial_height < r then
  r:=_initial_height;

 r1:=r / 3 - 8.0;
 r2:=r1 / 1.45;
 nr:=14;

 for i:=0 to nr - 1 do
  begin
   a :=pi * 2.0 * i / nr - pi / 2.0;
   dx:=Cos(a );
   dy:=Sin(a );

   if i and 1 <> 0 then
    ps.line_to(m_polygon_cx + dx * r1 ,m_polygon_cy + dy * r1 )
   else
    if i <> 0 then
     ps.line_to(m_polygon_cx + dx * r2 ,m_polygon_cy + dy * r2 )
    else
     ps.move_to(m_polygon_cx + dx * r2 ,m_polygon_cy + dy * r2 );

  end;

end;

{ ON_INIT }
procedure the_application.on_init;
begin
 m_image_center_x:=_initial_width / 2.0;
 m_image_center_y:=_initial_height / 2.0;

 m_polygon_cx:=_initial_width / 2.0;
 m_image_cx  :=m_polygon_cx;
 m_polygon_cy:=_initial_height / 2.0;
 m_image_cy  :=m_polygon_cy;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;
 rgba : aggclr;

 rb : renderer_base;
 rs : renderer_scanline_aa_solid;
 pf : rasterizer_scanline_aa;
 sl : scanline_u8;
 ps : path_storage;
 tr : conv_transform;
 e1 ,
 e2 : ellipse;
 c1 : conv_stroke;
 sa : span_allocator;
 sg : span_image_filter_ptr;
 ri : renderer_scanline_aa;
 fi : image_filter_base_ptr;

 filter : image_filter_ptr;

 interpolator : span_interpolator_linear;

 image_mtx ,polygon_mtx : trans_affine;

 tat : trans_affine_translation;
 tar : trans_affine_rotation;
 tas : trans_affine_scaling;

begin
 sg:=NIL;
 fi:=NIL;

 filter:=NIL;

// Initialize structures
 pixfmt_bgra32(pixf ,rbuf_window );

 rb.Construct(@pixf );
 rs.Construct(@rb );

 rgba.ConstrDbl(1.0 ,1.0 ,1.0 );
 rb.clear      (@rgba );

 image_mtx.Construct;
 polygon_mtx.Construct;

 tat.Construct(-m_polygon_cx ,-m_polygon_cy ); polygon_mtx.multiply(@tat );
 tar.Construct(m_polygon_angle._value * pi / 180.0 ); polygon_mtx.multiply(@tar );
 tas.Construct(m_polygon_scale._value ); polygon_mtx.multiply(@tas );
 tat.construct(m_polygon_cx ,m_polygon_cy ); polygon_mtx.multiply(@tat );

 case m_example._cur_item of
 // --------------(Example 1)
  1 :
   begin
    tat.Construct(-m_image_center_x ,-m_image_center_y ); image_mtx.multiply(@tat );
    tar.Construct(m_polygon_angle._value * pi / 180.0 ); image_mtx.multiply(@tar );
    tas.Construct(m_polygon_scale._value ); image_mtx.multiply(@tas );
    tat.Construct(m_polygon_cx ,m_polygon_cy ); image_mtx.multiply(@tat );

    image_mtx.invert;

   end;

 // --------------(Example 2)
  2 :
   begin
    tat.Construct(-m_image_center_x ,-m_image_center_y ); image_mtx.multiply(@tat );
    tar.Construct(m_image_angle._value * pi / 180.0 ); image_mtx.multiply(@tar );
    tas.Construct(m_image_scale._value ); image_mtx.multiply(@tas );
    tat.Construct(m_image_cx ,m_image_cy ); image_mtx.multiply(@tat );

    image_mtx.invert;

   end;

 // --------------(Example 3)
  3 :
   begin
    tat.Construct(-m_image_center_x ,-m_image_center_y ); image_mtx.multiply(@tat );
    tar.Construct(m_image_angle._value * pi / 180.0 ); image_mtx.multiply(@tar );
    tas.Construct(m_image_scale._value ); image_mtx.multiply(@tas );
    tat.Construct(m_polygon_cx ,m_polygon_cy ); image_mtx.multiply(@tat );

    image_mtx.invert;

   end;

 // --------------(Example 4)
  4 :
   begin
    tat.Construct(-m_image_cx ,-m_image_cy ); image_mtx.multiply(@tat );
    tar.Construct(m_polygon_angle._value * pi / 180.0 ); image_mtx.multiply(@tar );
    tas.Construct(m_polygon_scale._value ); image_mtx.multiply(@tas );
    tat.Construct(m_polygon_cx ,m_polygon_cy ); image_mtx.multiply(@tat );

    image_mtx.invert;

   end;

 // --------------(Example 5)
  5 :
   begin
    tat.Construct(-m_image_center_x ,-m_image_center_y ); image_mtx.multiply(@tat );
    tar.Construct(m_image_angle._value * pi / 180.0 ); image_mtx.multiply(@tar );
    tar.Construct(m_polygon_angle._value * pi / 180.0 ); image_mtx.multiply(@tar );
    tas.Construct(m_image_scale._value ); image_mtx.multiply(@tas );
    tas.Construct(m_polygon_scale._value ); image_mtx.multiply(@tas );
    tat.Construct(m_image_cx ,m_image_cy ); image_mtx.multiply(@tat );

    image_mtx.invert;

   end;

 // --------------(Example 6)
  6 :
   begin
    tat.Construct(-m_image_cx ,-m_image_cy ); image_mtx.multiply(@tat );
    tar.Construct(m_image_angle._value * pi / 180.0 ); image_mtx.multiply(@tar );
    tas.Construct(m_image_scale._value ); image_mtx.multiply(@tas );
    tat.Construct(m_image_cx ,m_image_cy ); image_mtx.multiply(@tat );

    image_mtx.invert;

   end;

 // --------------(Example 0, Identity matrix)
  else
   NoP;

 end;

 interpolator.Construct(@image_mtx );
 sa.Construct;

 rgba.ConstrDbl(1 ,1 ,1 ,0 );

// nearest neighbor
{ sg:=new(
  span_image_filter_rgba_nn_ptr ,
  Construct(
   @sa ,rbuf_img(0 ) ,@rgba ,@interpolator ,bgra_order ) );{}

// "hardcoded" bilinear filter
 sg:=new(
  span_image_filter_rgba_bilinear_ptr ,
  Construct(
   @sa ,rbuf_img(0 ) ,@rgba ,@interpolator ,bgra_order ) );{}

// arbitrary filter
{ fi    :=new(image_filter_spline36_ptr ,Construct );
 filter:=new(image_filter_ptr ,Construct(fi ) );

 sg:=new(
  span_image_filter_rgba_ptr ,
  Construct(
   @sa ,rbuf_img(0 ) ,@rgba ,@interpolator ,filter ,bgra_order ) );{}

// Render
 ri.Construct(@rb ,sg );

 pf.Construct;
 sl.Construct;
 ps.Construct;

 create_star(@ps );

 tr.Construct(@ps ,@polygon_mtx );

 pf.add_path     (@tr );
 render_scanlines(@pf ,@sl ,@ri );

 e1.Construct(m_image_cx ,m_image_cy ,5 ,5 ,20 );
 e2.Construct(m_image_cx ,m_image_cy ,2 ,2 ,20 );
 c1.Construct(@e1 );

 rgba.ConstrDbl  (0.7 ,0.8 ,0 );
 rs.color_       (@rgba );
 pf.add_path     (@e1);
 render_scanlines(@pf ,@sl ,@rs );

 rgba.ConstrDbl  (0 ,0 ,0 );
 rs.color_       (@rgba );
 pf.add_path     (@c1 );
 render_scanlines(@pf ,@sl ,@rs );

 pf.add_path     (@e2 );
 render_scanlines(@pf ,@sl ,@rs );

// Render the controls
 render_ctrl(@pf ,@sl ,@rs ,@m_polygon_angle );
 render_ctrl(@pf ,@sl ,@rs ,@m_polygon_scale );
 render_ctrl(@pf ,@sl ,@rs ,@m_image_angle );
 render_ctrl(@pf ,@sl ,@rs ,@m_image_scale );
 render_ctrl(@pf ,@sl ,@rs ,@m_rotate_polygon );
 render_ctrl(@pf ,@sl ,@rs ,@m_rotate_image );
 render_ctrl(@pf ,@sl ,@rs ,@m_example );

// Free AGG resources
 pf.Destruct;
 sl.Destruct;
 ps.Destruct;

 sa.Destruct;

 if sg <> NIL then
  dispose(sg ,Destruct );

 if fi <> NIL then
  dispose(fi );

 if filter <> NIL then
  dispose(filter ,Destruct );

 c1.Destruct; 

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 if flags and mouse_left <> 0 then
  begin
   if m_flag = 1 then
    begin
     m_image_cx:=x - m_dx;
     m_image_cy:=y - m_dy;

     force_redraw;

    end;

   if m_flag = 2 then
    begin
     m_polygon_cx:=x - m_dx;
     m_polygon_cy:=y - m_dy;
     
     force_redraw;

    end;

  end
 else
  on_mouse_button_up(x ,y ,flags );

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
var
 pf : rasterizer_scanline_aa;
 ps : path_storage;
 tr : conv_transform;

 polygon_mtx : trans_affine;

 tat : trans_affine_translation;
 tar : trans_affine_rotation;
 tas : trans_affine_scaling;

begin
 if flags and mouse_left <> 0 then
  if Sqrt(
      (x - m_image_cx ) * (x - m_image_cx ) +
      (y - m_image_cy ) * (y - m_image_cy ) ) < 5.0 then
   begin
    m_dx:=x - m_image_cx;
    m_dy:=y - m_image_cy;

    m_flag:=1;

   end
  else
   begin
    pf.Construct;
    polygon_mtx.Construct;

    tat.Construct(-m_polygon_cx ,-m_polygon_cy ); polygon_mtx.multiply(@tat );
    tar.Construct(m_polygon_angle._value * pi / 180.0 ); polygon_mtx.multiply(@tar );
    tas.Construct(m_polygon_scale._value ,m_polygon_scale._value ); polygon_mtx.multiply(@tas );
    tat.Construct(m_polygon_cx ,m_polygon_cy ); polygon_mtx.multiply(@tat );

    ps.Construct;

    create_star(@ps );

    tr.Construct(@ps ,@polygon_mtx );
    pf.add_path (@tr );

    if pf.hit_test(x ,y ) then
     begin
      m_dx:=x - m_polygon_cx;
      m_dy:=y - m_polygon_cy;

      m_flag:=2;

     end;

    pf.Destruct;
    ps.Destruct;

   end;

end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
 m_flag:=0;

end;

{ ON_CTRL_CHANGE }
procedure the_application.on_ctrl_change;
begin
 if m_rotate_polygon._status or
    m_rotate_image._status then
  wait_mode_(false )
 else
  wait_mode_(true );

 force_redraw;

end;

{ ON_IDLE }
procedure the_application.on_idle;
var
 redraw : boolean;

begin
 redraw:=false;

 if m_rotate_polygon._status then
  begin
   m_polygon_angle.value_(m_polygon_angle._value + 0.5 );

   if m_polygon_angle._value >= 180.0 then
    m_polygon_angle.value_(m_polygon_angle._value - 360.0 );

   redraw:=true;

  end;

 if m_rotate_image._status then
  begin
   m_image_angle.value_(m_image_angle._value + 0.5 );

   if m_image_angle._value >= 180.0 then
    m_image_angle.value_(m_image_angle._value - 360.0 );

   redraw:=true;

  end;

 if redraw then
  force_redraw;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'Affine transformations of the images. The examples demonstrates how to construct '#13 +
   'the affine transformer matrix for different cases. '#13 +
   'See the "image_transforms.txt" file for details. '#13 +
   'Now there are methods in trans_affine that allow you to construct transformations '#13 +
   'from an arbitrary parallelogram to another parallelogram. It''s very convenient and easy.'#13#13 +
   'How to play with:'#13#13 +
   'Use the left mouse button change the centre of rotation or move the polygon shape.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;
 buf : array [0..255 ] of char;
 ext : string[10 ];

 img_name ,p ,n ,x : shortstring;

BEGIN
 app.Construct(pix_format_bgra32 ,flip_y );
 app.caption_ ('Image Affine Transformations with filtering (F1-Help)' );

 img_name:='spheres';

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

   if img_name = 'spheres'#0 then
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
  if app.init(app.rbuf_img(0 )._width ,app.rbuf_img(0 )._height ,0 ) then
   app.run;

 app.Destruct;

END.