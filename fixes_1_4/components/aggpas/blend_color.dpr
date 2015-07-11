//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 blend_color ;

{DEFINE AGG_GRAY8 }
{DEFINE AGG_BGR24 }
{DEFINE AGG_RGB24 }
{$DEFINE AGG_BGRA32 }
{DEFINE AGG_RGBA32 }
{DEFINE AGG_ARGB32 }
{DEFINE AGG_ABGR32 }
{DEFINE AGG_RGB565 }
{DEFINE AGG_RGB555 }

{$IFDEF AGG_GRAY8 }
{$DEFINE AGG_PF8 }
{$ELSE }
 {$IFDEF AGG_BGR24 }
 {$DEFINE AGG_PF24 }
 {$ELSE }
  {$IFDEF AGG_RGB24 }
  {$DEFINE AGG_PF24 }
  {$ELSE }
   {$IFDEF AGG_BGRA32 }
   {$DEFINE AGG_PF32 }
   {$ELSE }
    {$IFDEF AGG_RGBA32 }
    {$DEFINE AGG_PF32 }
    {$ELSE }
     {$IFDEF AGG_ARGB32 }
     {$DEFINE AGG_PF32 }
     {$ELSE }
      {$IFDEF AGG_ABGR32 }
      {$DEFINE AGG_PF32 }
      {$ELSE }
       {$IFDEF AGG_RGB555 }
       {$DEFINE AGG_PF16 }
       {$ELSE }
        {$IFDEF AGG_RGB565 }
        {$DEFINE AGG_PF16 }
        {$ELSE }
        {$ENDIF }
       {$ENDIF }
      {$ENDIF }
     {$ENDIF }
    {$ENDIF }
   {$ENDIF }
  {$ENDIF }
 {$ENDIF }
{$ENDIF }

uses
 agg_basics ,
{$IFNDEF AGG_PF8 }
 agg_pixfmt_gray ,
{$ENDIF }
{$IFNDEF AGG_PF32 }
 agg_pixfmt_rgba ,
{$ENDIF }
 agg_array ,
 agg_platform_support ,
 agg_ctrl ,
 agg_slider_ctrl ,
 agg_rbox_ctrl ,
 agg_cbox_ctrl ,
 agg_polygon_ctrl ,
 agg_renderer_base ,
 agg_rendering_buffer ,
 agg_rasterizer_scanline_aa ,
 agg_conv_curve ,
 agg_conv_contour ,
 agg_conv_stroke ,
 agg_conv_transform ,
 agg_gsv_text ,
 agg_scanline_p ,
 agg_renderer_scanline ,
 agg_bounding_rect ,
 agg_trans_perspective ,
 agg_blur ,
 agg_path_storage ,
 agg_trans_affine

{$I pixel_formats.inc }
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
 the_application = object(platform_support )
  private
   m_method  ,
   m_comp_op : rbox_ctrl;
   m_radius  : slider_ctrl;

   m_r ,
   m_g ,
   m_b : cbox_ctrl;

   m_shadow_ctrl : polygon_ctrl;

   m_path  : path_storage;
   m_shape : conv_curve;

   m_ras : rasterizer_scanline_aa;
   m_sl  : scanline_p8;

   m_shape_bounds : rect_d;

   m_gray8_buf   : pod_array;
   m_gray8_rbuf  ,
   m_gray8_rbuf2 : rendering_buffer;

   m_color_lut : pod_array;

  public
   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_resize(sx ,sy : int ); virtual;
   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
var
 shape_mtx : trans_affine;

 tas : trans_affine_scaling;
 tat : trans_affine_translation;

 i : unsigned;
 p : int8u_ptr;

 rgba : aggclr;

begin
 inherited Construct(format_ ,flip_y_ );

 m_method.Construct (10.0       ,10.0       ,130.0       ,55.0             ,not flip_y_ );
 m_comp_op.Construct(420 + 20.0 ,5.0        ,420 + 140.0 ,395.0            ,not flip_y_ );
 m_radius.Construct (130 + 10.0 ,10.0 + 4.0 ,130 + 300.0 ,10.0 + 8.0 + 4.0 ,not flip_y_ );

 m_r.Construct(10.0 ,95.0  - 30 ,'Red'   ,not flip_y_ );
 m_g.Construct(10.0 ,110.0 - 30 ,'Green' ,not flip_y_ );
 m_b.Construct(10.0 ,125.0 - 30 ,'Blue'  ,not flip_y_ );

 m_shadow_ctrl.Construct(4 );

 m_path.Construct;
 m_shape.Construct(@m_path );

 m_ras.Construct;
 m_sl.Construct;

 m_shape_bounds.Construct;

 m_gray8_buf.Construct(sizeof(int8u ) );
 m_gray8_rbuf.Construct;
 m_gray8_rbuf2.Construct;

 m_color_lut.Construct(sizeof(aggclr ) );

 add_ctrl(@m_method );

 m_method.text_size_(8 );
 m_method.add_item  ('Single Color' );
 m_method.add_item  ('Color LUT' );
 m_method.cur_item_ (1 );

 add_ctrl(@m_radius );

 m_radius.range_(0.0 ,40.0 );
 m_radius.value_(15.0 );
 m_radius.label_('Blur Radius=%1.2f' );

 add_ctrl(@m_r );
 add_ctrl(@m_g );
 add_ctrl(@m_b );

 m_r.status_(true );
 m_b.status_(true );

 add_ctrl(@m_comp_op );

 m_comp_op.text_size_(6.6 );
 m_comp_op.add_item  ('no compositions' );
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
 m_comp_op.cur_item_ (0 );

 add_ctrl(@m_shadow_ctrl );

 m_shadow_ctrl.in_polygon_check_(true );

 m_path.remove_all;
 m_path.move_to(28.47 ,6.45  );
 m_path.curve3 (21.58 ,1.12  ,19.82 ,0.29  );
 m_path.curve3 (17.19 ,-0.93 ,14.21 ,-0.93 );
 m_path.curve3 (9.57  ,-0.93 ,6.57  ,2.25  );
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
 m_path.curve3 (36.52 ,9.13  ,36.77 ,7.69  );
 m_path.curve3 (37.01 ,6.25  ,37.57 ,5.76  );
 m_path.curve3 (38.13 ,5.27  ,38.87 ,5.27  );
 m_path.curve3 (39.65 ,5.27  ,40.23 ,5.62  );
 m_path.curve3 (41.26 ,6.25  ,44.19 ,9.18  );
 m_path.line_to(44.19 ,6.45  );
 m_path.curve3 (38.72 ,-0.88 ,33.74 ,-0.88 );
 m_path.curve3 (31.35 ,-0.88 ,29.93 ,0.78  );
 m_path.curve3 (28.52 ,2.44  ,28.47 ,6.45  );
 m_path.close_polygon;

 m_path.move_to(28.47 ,9.62  );
 m_path.line_to(28.47 ,26.66 );
 m_path.curve3 (21.09 ,23.73 ,18.95, 22.51 );
 m_path.curve3 (15.09 ,20.36 ,13.43, 18.02 );
 m_path.curve3 (11.77 ,15.67 ,11.77, 12.89 );
 m_path.curve3 (11.77 ,9.38  ,13.87, 7.06  );
 m_path.curve3 (15.97 ,4.74  ,18.70, 4.74  );
 m_path.curve3 (22.41 ,4.74  ,28.47, 9.62  );
 m_path.close_polygon;

 shape_mtx.Construct;
 tas.Construct     (4.0 );
 shape_mtx.multiply(@tas );
 tat.Construct     (150 ,100 );
 shape_mtx.multiply(@tat );

 m_path.transform(@shape_mtx );

 bounding_rect_single(
  @m_shape ,0 ,
  @m_shape_bounds.x1 ,@m_shape_bounds.y1 ,
  @m_shape_bounds.x2 ,@m_shape_bounds.y2 );

 m_shadow_ctrl.xn_ptr(0 )^:=m_shape_bounds.x1;
 m_shadow_ctrl.yn_ptr(0 )^:=m_shape_bounds.y1;
 m_shadow_ctrl.xn_ptr(1 )^:=m_shape_bounds.x2;
 m_shadow_ctrl.yn_ptr(1 )^:=m_shape_bounds.y1;
 m_shadow_ctrl.xn_ptr(2 )^:=m_shape_bounds.x2;
 m_shadow_ctrl.yn_ptr(2 )^:=m_shape_bounds.y2;
 m_shadow_ctrl.xn_ptr(3 )^:=m_shape_bounds.x1;
 m_shadow_ctrl.yn_ptr(3 )^:=m_shape_bounds.y2;

 rgba.ConstrDbl           (0 ,0.3 ,0.5 ,0.3 );
 m_shadow_ctrl.line_color_(@rgba );

 m_color_lut.resize(256 );

 p:=@g_gradient_colors[0 ];
 i:=0;

 while i < 256 do
  begin
   if i > 63 then
    aggclr_ptr(m_color_lut.array_operator(i ) ).ConstrInt(
     int8u_ptr(ptrcomp(p ) + 0 * sizeof(int8u ) )^ ,
     int8u_ptr(ptrcomp(p ) + 1 * sizeof(int8u ) )^ ,
     int8u_ptr(ptrcomp(p ) + 2 * sizeof(int8u ) )^ ,
     255 )
   else
    aggclr_ptr(m_color_lut.array_operator(i ) ).ConstrInt(
     int8u_ptr(ptrcomp(p ) + 0 * sizeof(int8u ) )^ ,
     int8u_ptr(ptrcomp(p ) + 1 * sizeof(int8u ) )^ ,
     int8u_ptr(ptrcomp(p ) + 2 * sizeof(int8u ) )^ ,
     i * 4 );

  //aggclr_ptr(m_color_lut.array_operator(i ) ).premultiply;

   inc(ptrcomp(p ) ,4 * sizeof(int8u ) );
   inc(i );

  end;

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_method.Destruct;
 m_comp_op.Destruct;
 m_radius.Destruct;
 m_r.Destruct;
 m_g.Destruct;
 m_b.Destruct;

 m_shadow_ctrl.Destruct;

 m_path.Destruct;
 m_shape.Destruct;

 m_ras.Destruct;
 m_sl.Destruct;

 m_gray8_buf.Destruct;
 m_gray8_rbuf.Destruct;
 m_gray8_rbuf2.Destruct;

 m_color_lut.Destruct;

end;

{ ON_RESIZE }
procedure the_application.on_resize(sx ,sy : int );
begin
 m_gray8_buf.resize (sx * sy);
 m_gray8_rbuf.attach(m_gray8_buf.data ,sx ,sy ,sx );

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 rgba : aggclr;
 rens : renderer_scanline_aa_solid;

 pixf_gray8 ,pixf ,pixf2 ,pixf_blend : pixel_formats;

 renb_gray8 ,renb ,renb_blend : renderer_base;

 shadow_persp : trans_perspective;
 shadow_trans : conv_transform;

 bbox ,cl : rect_d;

 tm  : double;
 buf : array[0..63 ] of char;
 t   : gsv_text;
 st  : conv_stroke;

 r ,g ,b : int;

begin
// Initialize structures
 m_ras.clip_box(0 ,0 ,_width ,_height );

 pixfmt_gray8(pixf_gray8 ,@m_gray8_rbuf );

 renb_gray8.Construct(@pixf_gray8 );

 rgba.ConstrInt  (0 );
 renb_gray8.clear(@rgba );

// Testing enhanced compositing operations.
// Uncomment and replace renb.blend_from_* to renb_blend.blend_from_*
 pixfmt_custom_blend_rgba(pixf_blend ,rbuf_window ,comp_op_adaptor_rgba ,bgra_order );
 renb_blend.Construct    (@pixf_blend );

 if m_comp_op._cur_item > 0 then
  pixf_blend.comp_op_(m_comp_op._cur_item - 1 );

 pixfmt(pixf ,rbuf_window );

 renb.Construct(@pixf );
 rens.Construct(@renb );

 rgba.ConstrDbl(1 ,0.95 ,0.95 );
 renb.clear    (@rgba );

 shadow_persp.Construct(
  m_shape_bounds.x1 ,m_shape_bounds.y1 ,
  m_shape_bounds.x2 ,m_shape_bounds.y2 ,
  pointer(m_shadow_ctrl._polygon ) );

 shadow_trans.Construct(@m_shape ,@shadow_persp );

// Render the controls Before
 render_ctrl(@m_ras ,@m_sl ,@rens ,@m_method );
 render_ctrl(@m_ras ,@m_sl ,@rens ,@m_radius );
 render_ctrl(@m_ras ,@m_sl ,@rens ,@m_r );
 render_ctrl(@m_ras ,@m_sl ,@rens ,@m_g );
 render_ctrl(@m_ras ,@m_sl ,@rens ,@m_b );
 render_ctrl(@m_ras ,@m_sl ,@rens ,@m_shadow_ctrl );

 if m_r._status then
  r:=100
 else
  r:=0;

 if m_g._status then
  g:=100
 else
  g:=0;

 if m_b._status then
  b:=100
 else
  b:=0;

 start_timer;

// Render shadow
 m_ras.add_path(@shadow_trans );

 rgba.ConstrInt           (255 );
 render_scanlines_aa_solid(@m_ras ,@m_sl ,@renb_gray8 ,@rgba );

// Calculate the bounding box and extend it by the blur radius
 bbox.Construct;

 bounding_rect_single(@shadow_trans ,0 ,@bbox.x1 ,@bbox.y1 ,@bbox.x2 ,@bbox.y2 );

 bbox.x1:=bbox.x1 - m_radius._value;
 bbox.y1:=bbox.y1 - m_radius._value;
 bbox.x2:=bbox.x2 + m_radius._value;
 bbox.y2:=bbox.y2 + m_radius._value;

 cl.Construct(0 ,0 ,_width ,_height );

 if bbox.clip(@cl ) then
  begin
  // Create a new pixel renderer and attach it to the main one as a child image.
  // It returns true if the attachment suceeded. It fails if the rectangle
  // (bbox) is fully clipped.
   pixfmt_gray8(pixf2 ,@m_gray8_rbuf2 );

   if pixf2.attach(@pixf_gray8 ,Trunc(bbox.x1 ) ,Trunc(bbox.y1 ) ,Trunc(bbox.x2 ) ,Trunc(bbox.y2 ) ) then
    stack_blur_gray8(@pixf2 ,uround(m_radius._value ) ,uround(m_radius._value ) );

   if m_method._cur_item = 0 then
    begin
     rgba.ConstrInt(r ,g ,b );

     if m_comp_op._cur_item = 0 then
      renb.blend_from_color(@pixf2 ,@rgba ,NIL ,Trunc(bbox.x1 ) ,Trunc(bbox.y1 ) )
     else
      renb_blend.blend_from_color(@pixf2 ,@rgba ,NIL ,Trunc(bbox.x1 ) ,Trunc(bbox.y1 ) );

    end
   else
    if m_comp_op._cur_item = 0 then
     renb.blend_from_lut(@pixf2 ,m_color_lut.data ,NIL ,Trunc(bbox.x1 ) ,Trunc(bbox.y1 ) )
    else
     renb_blend.blend_from_lut(@pixf2 ,m_color_lut.data ,NIL ,Trunc(bbox.x1 ) ,Trunc(bbox.y1 ) );

  end;

 tm:=elapsed_time;

// Info
 t.Construct;
 t.size_(10.0 );

 st.Construct(@t );
 st.width_   (1.5 );

 sprintf(@buf[0 ] ,'%3.2f ms' ,tm );

 t.start_point_(140.0 ,30.0 );
 t.text_       (@buf[0 ] );

 m_ras.add_path(@st );

 rgba.ConstrDbl           (0 ,0 , 0 );
 render_scanlines_aa_solid(@m_ras ,@m_sl ,@renb ,@rgba );

// Render the controls After
 render_ctrl(@m_ras ,@m_sl ,@rens ,@m_comp_op );

// Free AGG resources
 t.Destruct;
 st.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 if flags and mouse_left <> 0 then
  if m_shadow_ctrl.on_mouse_move(x ,y ,false ) then
   force_redraw;

 if flags and mouse_left = 0 then
  on_mouse_button_up(x ,y ,flags );

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
begin
 if flags and mouse_left <> 0 then
  if m_shadow_ctrl.on_mouse_button_down(x ,y ) then
   force_redraw;

end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
 if m_shadow_ctrl.on_mouse_button_up(x ,y ) then
  force_redraw;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'Now you can blur rendered images rather fast!'#13 +
   'There two algorithms are used: Stack Blur by Mario Klingemann     '#13 +
   'and Fast Recursive Gaussian Filter. The speed of both methods'#13 +
   'does not depend on the filter radius. Mario''s method works 3-5'#13 +
   'times faster; it doesn''t produce exactly Gaussian response,'#13 +
   'but pretty fair for most practical purposes. The recursive filter'#13 +
   'uses floating point arithmetic and works slower. But it is true'#13 +
   'Gaussian filter, with theoretically infinite impulse response.'#13 +
   'The radius (actually 2*sigma value) can be fractional and the'#13 +
   'filter produces quite adequate result.' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format ,flip_y );
 app.caption_ ('AGG Example. Gaussian and Stack Blur (F1-Help)' );

 if app.init(570 ,400 ,0 ) then
  app.run;

 app.Destruct;

END.