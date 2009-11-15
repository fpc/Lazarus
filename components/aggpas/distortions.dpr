{mac_copy:spheres.bmp}
//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 distortions ;

uses
 SysUtils ,

 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,

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

 agg_array ,
 agg_ellipse ,
 agg_trans_affine ,
 agg_conv_transform ,
 agg_image_filters ,
 agg_span_allocator ,
 agg_span_image_filter ,
 agg_span_image_filter_rgb ,
 agg_span_interpolator_linear ,
 agg_span_interpolator_adaptor ,
 agg_span_gradient ,
 file_utils_ ;

{$I agg_mode.inc }

const
 flip_y = true;

 g_gradient_colors : array[0..1023 ] of int8u = ( 

    255, 255, 255, 255,
    255, 255, 254, 255,
    255, 255, 254, 255,
    255, 255, 254, 255,
    255, 255, 253, 255,
    255, 255, 253, 255,
    255, 255, 252, 255,
    255, 255, 251, 255,
    255, 255, 250, 255,
    255, 255, 248, 255,
    255, 255, 246, 255,
    255, 255, 244, 255,
    255, 255, 241, 255,
    255, 255, 238, 255,
    255, 255, 235, 255,
    255, 255, 231, 255,
    255, 255, 227, 255,
    255, 255, 222, 255,
    255, 255, 217, 255,
    255, 255, 211, 255,
    255, 255, 206, 255,
    255, 255, 200, 255,
    255, 254, 194, 255,
    255, 253, 188, 255,
    255, 252, 182, 255,
    255, 250, 176, 255,
    255, 249, 170, 255,
    255, 247, 164, 255,
    255, 246, 158, 255,
    255, 244, 152, 255,
    254, 242, 146, 255,
    254, 240, 141, 255,
    254, 238, 136, 255,
    254, 236, 131, 255,
    253, 234, 126, 255,
    253, 232, 121, 255,
    253, 229, 116, 255,
    252, 227, 112, 255,
    252, 224, 108, 255,
    251, 222, 104, 255,
    251, 219, 100, 255,
    251, 216,  96, 255,
    250, 214,  93, 255,
    250, 211,  89, 255,
    249, 208,  86, 255,
    249, 205,  83, 255,
    248, 202,  80, 255,
    247, 199,  77, 255,
    247, 196,  74, 255,
    246, 193,  72, 255,
    246, 190,  69, 255,
    245, 187,  67, 255,
    244, 183,  64, 255,
    244, 180,  62, 255,
    243, 177,  60, 255,
    242, 174,  58, 255,
    242, 170,  56, 255,
    241, 167,  54, 255,
    240, 164,  52, 255,
    239, 161,  51, 255,
    239, 157,  49, 255,
    238, 154,  47, 255,
    237, 151,  46, 255,
    236, 147,  44, 255,
    235, 144,  43, 255,
    235, 141,  41, 255,
    234, 138,  40, 255,
    233, 134,  39, 255,
    232, 131,  37, 255,
    231, 128,  36, 255,
    230, 125,  35, 255,
    229, 122,  34, 255,
    228, 119,  33, 255,
    227, 116,  31, 255,
    226, 113,  30, 255,
    225, 110,  29, 255,
    224, 107,  28, 255,
    223, 104,  27, 255,
    222, 101,  26, 255,
    221,  99,  25, 255,
    220,  96,  24, 255,
    219,  93,  23, 255,
    218,  91,  22, 255,
    217,  88,  21, 255,
    216,  86,  20, 255,
    215,  83,  19, 255,
    214,  81,  18, 255,
    213,  79,  17, 255,
    212,  77,  17, 255,
    211,  74,  16, 255,
    210,  72,  15, 255,
    209,  70,  14, 255,
    207,  68,  13, 255,
    206,  66,  13, 255,
    205,  64,  12, 255,
    204,  62,  11, 255,
    203,  60,  10, 255,
    202,  58,  10, 255,
    201,  56,   9, 255,
    199,  55,   9, 255,
    198,  53,   8, 255,
    197,  51,   7, 255,
    196,  50,   7, 255,
    195,  48,   6, 255,
    193,  46,   6, 255,
    192,  45,   5, 255,
    191,  43,   5, 255,
    190,  42,   4, 255,
    188,  41,   4, 255,
    187,  39,   3, 255,
    186,  38,   3, 255,
    185,  37,   2, 255,
    183,  35,   2, 255,
    182,  34,   1, 255,
    181,  33,   1, 255,
    179,  32,   1, 255,
    178,  30,   0, 255,
    177,  29,   0, 255,
    175,  28,   0, 255,
    174,  27,   0, 255,
    173,  26,   0, 255,
    171,  25,   0, 255,
    170,  24,   0, 255,
    168,  23,   0, 255,
    167,  22,   0, 255,
    165,  21,   0, 255,
    164,  21,   0, 255,
    163,  20,   0, 255,
    161,  19,   0, 255,
    160,  18,   0, 255,
    158,  17,   0, 255,
    156,  17,   0, 255,
    155,  16,   0, 255,
    153,  15,   0, 255,
    152,  14,   0, 255,
    150,  14,   0, 255,
    149,  13,   0, 255,
    147,  12,   0, 255,
    145,  12,   0, 255,
    144,  11,   0, 255,
    142,  11,   0, 255,
    140,  10,   0, 255,
    139,  10,   0, 255,
    137,   9,   0, 255,
    135,   9,   0, 255,
    134,   8,   0, 255,
    132,   8,   0, 255,
    130,   7,   0, 255,
    128,   7,   0, 255,
    126,   6,   0, 255,
    125,   6,   0, 255,
    123,   5,   0, 255,
    121,   5,   0, 255,
    119,   4,   0, 255,
    117,   4,   0, 255,
    115,   4,   0, 255,
    113,   3,   0, 255,
    111,   3,   0, 255,
    109,   2,   0, 255,
    107,   2,   0, 255,
    105,   2,   0, 255,
    103,   1,   0, 255,
    101,   1,   0, 255,
     99,   1,   0, 255,
     97,   0,   0, 255,
     95,   0,   0, 255,
     93,   0,   0, 255,
     91,   0,   0, 255,
     90,   0,   0, 255,
     88,   0,   0, 255,
     86,   0,   0, 255,
     84,   0,   0, 255,
     82,   0,   0, 255,
     80,   0,   0, 255,
     78,   0,   0, 255,
     77,   0,   0, 255,
     75,   0,   0, 255,
     73,   0,   0, 255,
     72,   0,   0, 255,
     70,   0,   0, 255,
     68,   0,   0, 255,
     67,   0,   0, 255,
     65,   0,   0, 255,
     64,   0,   0, 255,
     63,   0,   0, 255,
     61,   0,   0, 255,
     60,   0,   0, 255,
     59,   0,   0, 255,
     58,   0,   0, 255,
     57,   0,   0, 255,
     56,   0,   0, 255,
     55,   0,   0, 255,
     54,   0,   0, 255,
     53,   0,   0, 255,
     53,   0,   0, 255,
     52,   0,   0, 255,
     52,   0,   0, 255,
     51,   0,   0, 255,
     51,   0,   0, 255,
     51,   0,   0, 255,
     50,   0,   0, 255,
     50,   0,   0, 255,
     51,   0,   0, 255,
     51,   0,   0, 255,
     51,   0,   0, 255,
     51,   0,   0, 255,
     52,   0,   0, 255,
     52,   0,   0, 255,
     53,   0,   0, 255,
     54,   1,   0, 255,
     55,   2,   0, 255,
     56,   3,   0, 255,
     57,   4,   0, 255,
     58,   5,   0, 255,
     59,   6,   0, 255,
     60,   7,   0, 255,
     62,   8,   0, 255,
     63,   9,   0, 255,
     64,  11,   0, 255,
     66,  12,   0, 255,
     68,  13,   0, 255,
     69,  14,   0, 255,
     71,  16,   0, 255,
     73,  17,   0, 255,
     75,  18,   0, 255,
     77,  20,   0, 255,
     79,  21,   0, 255,
     81,  23,   0, 255,
     83,  24,   0, 255,
     85,  26,   0, 255,
     87,  28,   0, 255,
     90,  29,   0, 255,
     92,  31,   0, 255,
     94,  33,   0, 255,
     97,  34,   0, 255,
     99,  36,   0, 255,
    102,  38,   0, 255,
    104,  40,   0, 255,
    107,  41,   0, 255,
    109,  43,   0, 255,
    112,  45,   0, 255,
    115,  47,   0, 255,
    117,  49,   0, 255,
    120,  51,   0, 255,
    123,  52,   0, 255,
    126,  54,   0, 255,
    128,  56,   0, 255,
    131,  58,   0, 255,
    134,  60,   0, 255,
    137,  62,   0, 255,
    140,  64,   0, 255,
    143,  66,   0, 255,
    145,  68,   0, 255,
    148,  70,   0, 255,
    151,  72,   0, 255,
    154,  74,   0, 255 );

type
 periodic_distortion_ptr = ^periodic_distortion;
 periodic_distortion = object(distortion )
   m_cx        ,
   m_cy        ,
   m_period    ,
   m_amplitude ,
   m_phase     : double;

   constructor Construct;

   procedure center   (x ,y : double );
   procedure period   (v : double );
   procedure amplitude(v : double );
   procedure phase    (v : double );

  end;

 distortion_wave = object(periodic_distortion )
   procedure calculate(x ,y : int_ptr ); virtual;

  end;

 distortion_swirl = object(periodic_distortion )
   procedure calculate(x ,y : int_ptr ); virtual;

  end;

 distortion_swirl_wave = object(periodic_distortion )
   procedure calculate(x ,y : int_ptr ); virtual;

  end;

 distortion_wave_swirl = object(periodic_distortion )
   procedure calculate(x ,y : int_ptr ); virtual;

  end;

 the_application = object(platform_support )
   m_angle      ,
   m_scale      ,
   m_amplitude  ,
   m_period     : slider_ctrl;
   m_distortion : rbox_ctrl;

   m_center_x ,
   m_center_y ,
   m_phase    : double;

   m_gradient_colors : pod_auto_array;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;

   procedure on_idle; virtual;
   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ calculate_wave }
procedure calculate_wave(x ,y : int_ptr; cx ,cy ,period ,amplitude ,phase : double );
var
 xd ,yd ,d ,a : double;

begin
 xd:=x^ / image_subpixel_size - cx;
 yd:=y^ / image_subpixel_size - cy;
 d :=Sqrt(xd * xd + yd * yd );

 if d > 1 then
  begin
   a :=Cos(d / (16.0 * period ) - phase ) * (1.0 / (amplitude * d ) ) + 1.0;
   x^:=trunc((xd * a + cx ) * image_subpixel_size );
   y^:=trunc((yd * a + cy ) * image_subpixel_size );

  end;

end;

{ calculate_swirl }
procedure calculate_swirl(x ,y : int_ptr; cx ,cy ,amplitude ,phase : double );
var
 xd ,yd ,a ,sa ,ca : double;

begin
 xd:=x^ / image_subpixel_size - cx;
 yd:=y^ / image_subpixel_size - cy;
 a :=(100.0 - Sqrt(xd * xd + yd * yd ) ) / 100.0 * (0.1 / -amplitude );
 sa:=Sin(a - phase / 25.0 );
 ca:=Cos(a - phase / 25.0 );
 x^:=trunc((xd * ca - yd * sa + cx ) * image_subpixel_size );
 y^:=trunc((xd * sa + yd * ca + cy ) * image_subpixel_size );

end;

{ CONSTRUCT }
constructor periodic_distortion.Construct;
begin
 m_cx       :=0.0;
 m_cy       :=0.0;
 m_period   :=0.5;
 m_amplitude:=0.5;
 m_phase    :=0.0;

end;

{ CENTER }
procedure periodic_distortion.center;
begin
 m_cx:=x;
 m_cy:=y;

end;

{ PERIOD }
procedure periodic_distortion.period;
begin
 m_period:=v;

end;

{ AMPLITUDE }
procedure periodic_distortion.amplitude;
begin
 m_amplitude:=1.0 / v;

end;

{ PHASE }
procedure periodic_distortion.phase;
begin
 m_phase:=v;

end;

{ CALCULATE }
procedure distortion_wave.calculate;
begin
 calculate_wave(x ,y ,m_cx ,m_cy ,m_period ,m_amplitude ,m_phase );

end;

{ CALCULATE }
procedure distortion_swirl.calculate;
begin
 calculate_swirl(x ,y ,m_cx ,m_cy ,m_amplitude ,m_phase );

end;

{ CALCULATE }
procedure distortion_swirl_wave.calculate;
begin
 calculate_swirl(x ,y ,m_cx ,m_cy ,m_amplitude ,m_phase );
 calculate_wave (x ,y ,m_cx ,m_cy ,m_period ,m_amplitude ,m_phase );

end;

{ CALCULATE }
procedure distortion_wave_swirl.calculate;
begin
 calculate_wave (x ,y ,m_cx ,m_cy ,m_period ,m_amplitude ,m_phase );
 calculate_swirl(x ,y ,m_cx ,m_cy ,m_amplitude ,m_phase );

end;

{ CONSTRUCT }
constructor the_application.Construct;
var
 i : unsigned;
 p : int8u_ptr;

 rgba : aggclr;

begin
 inherited Construct(format_ ,flip_y_ );

 m_gradient_colors.Construct(256 ,sizeof(aggclr ) );

 m_angle.Construct     (5       ,5      ,150       ,12      ,not flip_y_ );
 m_scale.Construct     (5       ,5 + 15 ,150       ,12 + 15 ,not flip_y_ );
 m_period.Construct    (5 + 170 ,5      ,150 + 170 ,12      ,not flip_y_ );
 m_amplitude.Construct (5 + 170 ,5 + 15 ,150 + 170 ,12 + 15 ,not flip_y_ );
 m_distortion.Construct(480     ,5      ,600       ,90      ,not flip_y_ );

 m_center_x:=0.0;
 m_center_y:=0.0;
 m_phase   :=0.0;

 add_ctrl(@m_angle );
 add_ctrl(@m_scale );
 add_ctrl(@m_amplitude );
 add_ctrl(@m_period );
 add_ctrl(@m_distortion );

 m_angle.label_('Angle=%3.2f' );
 m_scale.label_('Scale=%3.2f' );
 m_angle.range_(-180.0 ,180.0 );
 m_angle.value_(20.0 );
 m_scale.range_(0.1 ,5.0 );
 m_scale.value_(1.0 );

 m_amplitude.label_('Amplitude=%3.2f' );
 m_period.label_   ('Period=%3.2f' );
 m_amplitude.range_(0.1 ,40.0 );
 m_period.range_   (0.1 ,2.0 );
 m_amplitude.value_(10.0 );
 m_period.value_   (1.0 );

 m_distortion.add_item ('Wave' );
 m_distortion.add_item ('Swirl' );
 m_distortion.add_item ('Wave-Swirl' );
 m_distortion.add_item ('Swirl-Wave' );
 m_distortion.cur_item_(0 );

 p:=@g_gradient_colors[0 ];

 for i:=0 to 255 do
  begin
   rgba.ConstrInt(
    int8u_ptr(ptrcomp(p ) + 0 * sizeof(int8u ) )^ ,
    int8u_ptr(ptrcomp(p ) + 1 * sizeof(int8u ) )^ ,
    int8u_ptr(ptrcomp(p ) + 2 * sizeof(int8u ) )^ ,
    int8u_ptr(ptrcomp(p ) + 3 * sizeof(int8u ) )^ );

   move(
    rgba ,
    m_gradient_colors.array_operator(i )^ ,
    sizeof(aggclr ) );

   inc(ptrcomp(p ) ,4 * sizeof(int8u ) );

  end;

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_gradient_colors.Destruct;

 m_angle.Destruct;
 m_scale.Destruct;
 m_period.Destruct;
 m_amplitude.Destruct;
 m_distortion.Destruct;

end;

{ ON_INIT }
procedure the_application.on_init;
begin
 m_center_x:=rbuf_img(0 )._width / 2.0 + 10;
 m_center_y:=rbuf_img(0 )._height / 2.0 + 10 + 40;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 img_width ,img_height ,cx ,cy ,r : double;

 pixf : pixel_formats;
 rgba : aggclr;

 rb : renderer_base;
 rs : renderer_scanline_aa_solid;
 pf : rasterizer_scanline_aa;
 sl : scanline_u8;
 sa : span_allocator;
 sg : span_image_filter_ptr;
 fi : image_filter_spline36;
 ri : renderer_scanline_aa;

 filter : image_filter_ptr;

 interpolator : span_interpolator_adaptor;

 dist            : periodic_distortion_ptr;
 dist_wave       : distortion_wave;
 dist_swirl      : distortion_swirl;
 dist_wave_swirl : distortion_wave_swirl;
 dist_swirl_wave : distortion_swirl_wave;

 src_mtx ,img_mtx ,gr1_mtx ,gr2_mtx : trans_affine;

 inv : trans_affine;
 tat : trans_affine_translation;
 tar : trans_affine_rotation;
 tas : trans_affine_scaling;

 ell : ellipse;
 tr  ,
 tr2 : conv_transform;

 gradient_function : gradient_circle;
 span_gradient_    : span_gradient;

 rg : renderer_scanline_aa;

begin
 sg    :=NIL;
 filter:=NIL;

// Initialize structures
 img_width :=rbuf_img(0 )._width;
 img_height:=rbuf_img(0 )._height;

 pixfmt_bgr24(pixf ,rbuf_window );

 rb.Construct(@pixf );
 rs.Construct(@rb );

 rgba.ConstrDbl(1.0 ,1.0 ,1.0 );
 rb.clear      (@rgba );

 src_mtx.Construct;

 tat.Construct(-img_width / 2 ,-img_height / 2 ); src_mtx.multiply(@tat );
 tar.Construct(m_angle._value * pi / 180.0 ); src_mtx.multiply(@tar );
 tat.Construct(img_width / 2 + 10 ,img_height / 2 + 10 + 40 ); src_mtx.multiply(@tat );

 src_mtx.multiply(_trans_affine_resizing );

 img_mtx.Construct;

 tat.Construct(-img_width / 2 ,-img_height / 2 ); img_mtx.multiply(@tat );
 tar.Construct(m_angle._value * pi / 180.0); img_mtx.multiply(@tar );
 tas.Construct(m_scale._value ); img_mtx.multiply(@tas );
 tat.Construct(img_width / 2 + 10 ,img_height / 2 + 10 + 40 ); img_mtx.multiply(@tat );

 img_mtx.multiply(_trans_affine_resizing );
 img_mtx.invert;

 sa.Construct;

 dist:=NIL;

 dist_wave.Construct;
 dist_swirl.Construct;
 dist_wave_swirl.Construct;
 dist_swirl_wave.Construct;

 case m_distortion._cur_item of
  0 : dist:=@dist_wave;
  1 : dist:=@dist_swirl;
  2 : dist:=@dist_wave_swirl;
  3 : dist:=@dist_swirl_wave;

 end;

 dist.period   (m_period._value );
 dist.amplitude(m_amplitude._value );
 dist.phase    (m_phase );

 cx:=m_center_x;
 cy:=m_center_y;

 img_mtx.transform(@img_mtx ,@cx ,@cy );

 dist.center(cx ,cy );

 interpolator.Construct(@img_mtx ,dist );

 rgba.ConstrDbl(1 ,1 ,1 ,0 );

// Version without filtering (nearest neighbor)
{ sg:=new(
  span_image_filter_rgb_nn_ptr ,
  Construct(
   @sa ,rbuf_img(0 ) ,@rgba ,@interpolator ,bgr_order ) );{}

// Version with "hardcoded" bilinear filter
 sg:=new(
  span_image_filter_rgb_bilinear_ptr ,
  Construct(
   @sa ,rbuf_img(0 ) ,@rgba ,@interpolator ,bgr_order ) );{}

// Version with arbitrary filter
{ fi.Construct;

 new(filter ,Construct(@fi ) );

 sg:=new(
  span_image_filter_rgb_ptr ,
  Construct(
   @sa ,rbuf_img(0 ) ,@rgba ,@interpolator ,filter ,bgr_order ) );{}

// Render
 ri.Construct(@rb ,sg );
 pf.Construct;
 sl.Construct;

 r:=img_width;

 if img_height < r then
  r:=img_height;

 ell.Construct(
  img_width / 2.0 ,img_height / 2.0 ,
  r / 2.0 - 20.0 ,r / 2.0 - 20.0 ,200 );

 tr.Construct(@ell ,@src_mtx );

 pf.add_path     (@tr );
 render_scanlines(@pf ,@sl ,@ri );

 inv.Construct;

 inv.assign(_trans_affine_resizing );

 inv.invert;

 src_mtx.multiply(@inv );

 tat.Construct(img_width - img_width / 10 ,0.0 ); src_mtx.multiply(@tat );

 src_mtx.multiply(_trans_affine_resizing );

 pf.add_path     (@tr );
 rgba.ConstrInt  (0 ,0 ,0 );
 rs.color_       (@rgba );
 render_scanlines(@pf ,@sl ,@rs );

 gradient_function.Construct;
 span_gradient_.Construct(
  @sa ,@interpolator ,@gradient_function ,@m_gradient_colors ,0 ,180 );

 rg.Construct(@rb ,@span_gradient_ );

 gr1_mtx.Construct;

 tat.Construct(-img_width / 2 ,-img_height / 2 ); gr1_mtx.multiply(@tat );
 tas.Construct(0.8 ); gr1_mtx.multiply(@tas );
 tar.Construct(m_angle._value * pi / 180.0 ); gr1_mtx.multiply(@tar );
 tat.Construct(img_width - img_width / 10 + img_width / 2 + 10 ,img_height / 2 + 10 + 40 ); gr1_mtx.multiply(@tat );

 gr1_mtx.multiply(_trans_affine_resizing );

 gr2_mtx.Construct;

 tar.Construct(m_angle._value * pi / 180.0 ); gr2_mtx.multiply(@tar );
 tas.Construct(m_scale._value ); gr2_mtx.multiply(@tas );
 tat.Construct(img_width - img_width / 10 + img_width / 2 + 10 + 50 ,img_height / 2 + 10 + 40 + 50 ); gr2_mtx.multiply(@tat );

 gr2_mtx.multiply(_trans_affine_resizing );
 gr2_mtx.invert;

 cx:=m_center_x + img_width - img_width / 10;
 cy:=m_center_y;

 gr2_mtx.transform(@gr2_mtx ,@cx ,@cy );

 dist.center(cx ,cy );

 interpolator.transformer_(@gr2_mtx );

 tr2.Construct(@ell ,@gr1_mtx );

 pf.add_path     (@tr2 );
 render_scanlines(@pf ,@sl ,@rg );

// Render the controls
 render_ctrl(@pf ,@sl ,@rs ,@m_angle );
 render_ctrl(@pf ,@sl ,@rs ,@m_scale );
 render_ctrl(@pf ,@sl ,@rs ,@m_amplitude );
 render_ctrl(@pf ,@sl ,@rs ,@m_period );
 render_ctrl(@pf ,@sl ,@rs ,@m_distortion );

// Free AGG resources
 pf.Destruct;
 sl.Destruct;
 sa.Destruct;

 if sg <> NIL then
  dispose(sg ,Destruct );

 if filter <> NIL then
  dispose(filter ,Destruct );

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 if flags and 1 <> 0 then
  begin
   m_center_x:=x;
   m_center_y:=y;

   force_redraw;

  end;

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
begin
 if flags <> 0 then
  begin
   m_center_x:=x;
   m_center_y:=y;

   force_redraw;

  end;

end;

{ ON_IDLE }
procedure the_application.on_idle;
begin
 m_phase:=m_phase + (15.0 * pi / 180.0 );

 if m_phase > pi * 200.0 then
  m_phase:=m_phase - (pi * 200.0 );

 force_redraw;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'To transform an image as well as to define a color gradient you have to write '#13 +
   'several declarations. This approach can seem difficult to handle (compared with '#13 +
   'one function call), but it''s very flexible. For example, you can add an arbitrary '#13 +
   'distortion function. This mechanism is pretty much the same in image transformers '#13 +
   'and color gradients.'#13#13 +
   'How to play with:'#13#13 +
   'Try changing different parameters of the distortions.'#13 +
   'Use any mouse button to move the distortion''s epicentre.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;
 buf : array [0..255 ] of char;
 ext : string[10 ];

 img_name ,p ,n ,x : shortstring;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('Image and Gradient Distortions (F1-Help)' );

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
  if app.init(app.rbuf_img(0 )._width + 300 ,app.rbuf_img(0 )._height + 40 + 20 ,window_resize ) then
   begin
    app.wait_mode_(false );
    app.run;

   end;

 app.Destruct;

END.