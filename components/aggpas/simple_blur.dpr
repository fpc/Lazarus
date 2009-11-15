//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 simple_blur ;

uses
 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_renderer_outline_aa ,
 agg_rasterizer_scanline_aa ,
 agg_rasterizer_outline_aa ,
 agg_scanline ,
 agg_scanline_u ,
 agg_scanline_p ,
 agg_render_scanlines ,

 agg_ellipse ,
 agg_path_storage ,
 agg_conv_stroke ,
 agg_conv_transform ,
 agg_bounding_rect ,
 agg_span_generator ,
 agg_trans_affine ,
 agg_span_allocator ,
 parse_lion_ ;

{$I agg_mode.inc }

const
 flip_y = true;

var 
 g_path     : path_storage;
 g_colors   : array[0..99 ] of aggclr;
 g_path_idx : array[0..99 ] of unsigned;

 g_npaths : unsigned;

 g_x1 ,g_y1 ,g_x2 ,g_y2 ,
 g_base_dx  ,g_base_dy  ,
 g_angle    ,g_scale    ,
 g_skew_x   ,g_skew_y   : double;

 g_nclick : int;

type
 span_simple_blur_rgb24 = object(span_generator )
   m_source_image : rendering_buffer_ptr;

   constructor Construct(alloc : span_allocator_ptr ); overload;
   constructor Construct(alloc : span_allocator_ptr; src : rendering_buffer_ptr ); overload;

   procedure _source_image(src : rendering_buffer_ptr );
   function  source_image : rendering_buffer_ptr;

   function  generate(x ,y : int; len : unsigned ) : aggclr_ptr; virtual;

  end;

 the_application = object(platform_support )
   m_cx ,
   m_cy : double;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor span_simple_blur_rgb24.Construct(alloc : span_allocator_ptr );
begin
 inherited Construct(alloc );

 m_source_image:=NIL;

end;

{ CONSTRUCT }
constructor span_simple_blur_rgb24.Construct(alloc : span_allocator_ptr; src : rendering_buffer_ptr );
begin
 inherited Construct(alloc );

 m_source_image:=src;

end;

{ _SOURCE_IMAGE }
procedure span_simple_blur_rgb24._source_image;
begin
 m_source_image:=src;

end;

{ SOURCE_IMAGE }
function span_simple_blur_rgb24.source_image;
begin
 result:=m_source_image;

end;

{ GENERATE }
function span_simple_blur_rgb24.generate;
var
 span  : aggclr_ptr;
 color : array[0..3 ] of int;

 i   : int;
 ptr : int8u_ptr;

begin
 span:=_allocator.span;

 if (y < 1 ) or
    (y >= m_source_image._height - 1 ) then
  begin
   repeat
    span.ConstrInt(0 ,0 ,0 ,0 );

    inc(integer(span ) ,sizeof(aggclr ) );
    dec(len );

   until len = 0;

   result:=_allocator.span;

   exit;

  end;

 repeat
  color[0 ]:=0;
  color[1 ]:=0;
  color[2 ]:=0;
  color[3 ]:=0;

  if (x > 0 ) and
     (x < m_source_image._width - 1 ) then
   begin
    i:=3;

    repeat
     ptr:=int8u_ptr(integer(m_source_image.row(y - i + 2 ) ) + ((x - 1 ) * 3 ) * sizeof(int8u ) );

     inc(color[0 ] ,ptr^ ); inc(integer(ptr ) ,sizeof(int8u ) );
     inc(color[1 ] ,ptr^ ); inc(integer(ptr ) ,sizeof(int8u ) );
     inc(color[2 ] ,ptr^ ); inc(integer(ptr ) ,sizeof(int8u ) );
     inc(color[3 ] ,255 );

     inc(color[0 ] ,ptr^ ); inc(integer(ptr ) ,sizeof(int8u ) );
     inc(color[1 ] ,ptr^ ); inc(integer(ptr ) ,sizeof(int8u ) );
     inc(color[2 ] ,ptr^ ); inc(integer(ptr ) ,sizeof(int8u ) );
     inc(color[3 ] ,255 );

     inc(color[0 ] ,ptr^ ); inc(integer(ptr ) ,sizeof(int8u ) );
     inc(color[1 ] ,ptr^ ); inc(integer(ptr ) ,sizeof(int8u ) );
     inc(color[2 ] ,ptr^ ); inc(integer(ptr ) ,sizeof(int8u ) );
     inc(color[3 ] ,255 );

     dec(i );

    until i = 0;

    color[0 ]:=color[0 ] div 9;
    color[1 ]:=color[1 ] div 9;
    color[2 ]:=color[2 ] div 9;
    color[3 ]:=color[3 ] div 9;

   end;

  span.ConstrInt(color[2 ] ,color[1 ] ,color[0 ] ,color[3 ] );

  inc(integer(span ) ,sizeof(aggclr ) );
  inc(x );
  dec(len );

 until len = 0;

 result:=_allocator.span;

end;

{ _PARSE_LION_ }
procedure _parse_lion_;
begin
 g_npaths:=parse_lion(@g_path ,@g_colors ,@g_path_idx );

 bounding_rect(@g_path ,@g_path_idx ,0 ,g_npaths ,@g_x1 ,@g_y1 ,@g_x2 ,@g_y2 );

 g_base_dx:=(g_x2 - g_x1 ) / 2.0;
 g_base_dy:=(g_y2 - g_y1 ) / 2.0;

end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_cx:=100;
 m_cy:=102;

 _parse_lion_;

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;

 rb : renderer_base;
 rs : renderer_scanline_aa_solid;

 rgba  : aggclr;
 trans : conv_transform;

 mtx ,
 inv : trans_affine;
 tat : trans_affine_translation;
 tas : trans_affine_scaling;
 tar : trans_affine_rotation;
 taw : trans_affine_skewing;

 ras2 : rasterizer_scanline_aa;
 sl   : scanline_p8;
 sl2  : scanline_u8;

 profile : line_profile_aa;

 rp  : renderer_outline_aa;
 ras : rasterizer_outline_aa;

 ell : ellipse;

 ell_stroke1 ,
 ell_stroke2 : conv_stroke;

 sa : span_allocator;
 sg : span_simple_blur_rgb24;

 rblur : renderer_scanline_aa;

begin
// Initialize structures
 pixfmt_bgr24(pixf ,rbuf_window );

 rb.Construct(@pixf );
 rs.Construct(@rb );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear      (@rgba );

 mtx.Construct;
 trans.Construct(@g_path ,@mtx );

 tat.Construct(-g_base_dx ,-g_base_dy );
 mtx.multiply (@tat );

 tas.Construct(g_scale ,g_scale );
 mtx.multiply (@tas );

 tar.Construct(g_angle + pi );
 mtx.multiply (@tar );

 taw.Construct(g_skew_x / 1000.0 ,g_skew_y / 1000.0 );
 mtx.multiply (@taw );

 tat.Construct(_initial_width / 4 ,_initial_height / 2 );
 mtx.multiply (@tat );

 mtx.multiply(_trans_affine_resizing );

 ras2.Construct;
 sl.Construct;
 sl2.Construct;

// Full lion
 render_all_paths(@ras2 ,@sl ,@rs ,@trans ,@g_colors ,@g_path_idx ,g_npaths );

// Outline Lion
 inv.Construct;
 inv.assign(_trans_affine_resizing );

 inv.invert;
 mtx.multiply(@inv );

 tat.Construct(_initial_width / 2 ,0 );
 mtx.multiply(@tat );

 mtx.multiply(_trans_affine_resizing );

 profile.Construct;
 profile.width_(1.0 );

 rp.Construct (@rb ,@profile );
 ras.Construct(@rp );

 ras.round_cap_    (true );
 ras.accurate_join_(true );

 ras.render_all_paths(@trans ,@g_colors ,@g_path_idx ,g_npaths );

// Ellipses
 ell.Construct(m_cx ,m_cy ,100.0 ,100.0 ,100 );

 ell_stroke1.Construct(@ell );
 ell_stroke1.width_   (6.0 );
 ell_stroke2.Construct(@ell_stroke1 );
 ell_stroke2.width_   (2.0 );

 rgba.ConstrDbl(0,0.2,0);
 rs.color_     (@rgba );

 ras2.add_path   (@ell_stroke2 );
 render_scanlines(@ras2 ,@sl ,@rs );

// Blur
 sa.Construct;
 sg.Construct(@sa );

 rblur.Construct (@rb ,@sg );
 sg._source_image(rbuf_img(0 ) );

 ras2.add_path(@ell );

 copy_window_to_img(0 );
 render_scanlines  (@ras2 ,@sl2 ,@rblur );

// More blur if desired :-)
{ copy_window_to_img(0 );
 render_scanlines  (@ras2 ,@sl2 ,@rblur );

 copy_window_to_img(0 );
 render_scanlines  (@ras2 ,@sl2 ,@rblur );

 copy_window_to_img(0 );
 render_scanlines  (@ras2 ,@sl2 ,@rblur );{}

// Free AGG resources
 ras2.Destruct;
 sl.Destruct;
 sl2.Destruct;

 ras.Destruct;
 profile.Destruct;
 ell_stroke1.Destruct;
 ell_stroke2.Destruct;
 sa.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 on_mouse_button_down(x ,y ,flags );

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
begin
 if flags and mouse_left <> 0 then
  begin
   m_cx:=x;
   m_cy:=y;

   force_redraw;

  end;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'The example demonstrates how to write custom span generators. This one just applies '#13 +
   'the simplest "blur" filter 3x3 to a prerendered image. It calculates the average value '#13 +
   'of 9 neighbor pixels.'#13#13 +
   'How to play with:'#13#13 +
   'Just press the left mouse button and drag.'#13 +
   'Uncomment and recompile the part of the demo source code to get more blur.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
// Rendering
 g_path.Construct;

 g_npaths:=0;

 g_x1:=0;
 g_y1:=0;
 g_x2:=0;
 g_y2:=0;

 g_base_dx:=0;
 g_base_dy:=0;

 g_angle:=0;
 g_scale:=1.0;

 g_skew_x:=0;
 g_skew_y:=0;
 g_nclick:=0;

// App
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. Lion with Alpha-Masking (F1-Help)' );

 if app.init(512 ,400 ,window_resize ) then
  app.run;

 app.Destruct;

// Free
 g_path.Destruct;

END.