//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 perspective ;

{DEFINE AGG_GRAY8 }
{$DEFINE AGG_BGR24 }
{DEFINE AGG_RGB24 }
{DEFINE AGG_BGRA32 }
{DEFINE AGG_RGBA32 }
{DEFINE AGG_ARGB32 }
{DEFINE AGG_ABGR32 }
{DEFINE AGG_RGB565 }
{DEFINE AGG_RGB555 }

uses
 agg_basics ,
 agg_platform_support ,

 agg_ctrl ,
 agg_rbox_ctrl ,

 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_p ,

 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_render_scanlines ,

 agg_path_storage ,
 agg_bounding_rect ,
 agg_trans_affine ,
 agg_trans_bilinear ,
 agg_trans_perspective , 
 agg_conv_transform ,
 agg_conv_stroke ,
 agg_conv_clip_polygon ,
 agg_ellipse ,
 interactive_polygon_ ,
 parse_lion_

{$I pixel_formats.inc }
{$I agg_mode.inc }

const
 flip_y = true;

var
 g_rasterizer : rasterizer_scanline_aa;
 g_scanline   : scanline_p8;

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
 the_application = object(platform_support )
   m_quad       : interactive_polygon;
   m_trans_type : rbox_ctrl;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_mouse_move       (x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ _PARSE_LION_ }
procedure _parse_lion_;
begin
 g_npaths:=parse_lion(@g_path ,@g_colors ,@g_path_idx );

 bounding_rect(@g_path ,@g_path_idx ,0 ,g_npaths ,@g_x1 ,@g_y1 ,@g_x2 ,@g_y2 );

 g_base_dx:=(g_x2 - g_x1 ) / 2.0;
 g_base_dy:=(g_y2 - g_y1 ) / 2.0;

 g_path.flip_x(g_x1 ,g_x2 );
 g_path.flip_y(g_y1 ,g_y2 );

end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_quad.Construct      (4 ,5.0 );
 m_trans_type.Construct(420 ,5.0 ,420 + 130.0 ,55.0 ,not flip_y_ );

 _parse_lion_;

 m_quad.xn_ptr(0 )^:=g_x1;
 m_quad.yn_ptr(0 )^:=g_y1;
 m_quad.xn_ptr(1 )^:=g_x2;
 m_quad.yn_ptr(1 )^:=g_y1;
 m_quad.xn_ptr(2 )^:=g_x2;
 m_quad.yn_ptr(2 )^:=g_y2;
 m_quad.xn_ptr(3 )^:=g_x1;
 m_quad.yn_ptr(3 )^:=g_y2;

 m_trans_type.add_item ('Bilinear' );
 m_trans_type.add_item ('Perspective' );
 m_trans_type.cur_item_(0 );

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
 dx ,dy : double;

begin
 dx:=_width / 2.0 - (m_quad.xn_ptr(1 )^ - m_quad.xn_ptr(0 )^ ) / 2.0;
 dy:=_height / 2.0 - (m_quad.yn_ptr(2 )^ - m_quad.yn_ptr(0 )^ ) / 2.0;

 m_quad.xn_ptr(0 )^:=m_quad.xn_ptr(0 )^ + dx;
 m_quad.yn_ptr(0 )^:=m_quad.yn_ptr(0 )^ + dy;
 m_quad.xn_ptr(1 )^:=m_quad.xn_ptr(1 )^ + dx;
 m_quad.yn_ptr(1 )^:=m_quad.yn_ptr(1 )^ + dy;
 m_quad.xn_ptr(2 )^:=m_quad.xn_ptr(2 )^ + dx;
 m_quad.yn_ptr(2 )^:=m_quad.yn_ptr(2 )^ + dy;
 m_quad.xn_ptr(3 )^:=m_quad.xn_ptr(3 )^ + dx;
 m_quad.yn_ptr(3 )^:=m_quad.yn_ptr(3 )^ + dy;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;

 rb : renderer_base;
 r  : renderer_scanline_aa_solid;

 rgba : aggclr;

 tr_bi : trans_bilinear;
 tr_pe ,
 tr2   : trans_perspective23;

 trans : conv_transform;

 ell              : ellipse;
 ell_stroke       : conv_stroke;
 trans_ell        ,
 trans_ell_stroke : conv_transform;

 x ,y : double;

begin
// Initialize structures
 pixfmt(pixf ,rbuf_window );

 rb.Construct(@pixf );
 r.Construct (@rb );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear      (@rgba );

 g_rasterizer.clip_box(0 ,0 ,_width ,_height );

// Perspective rendering
 if m_trans_type._cur_item = 0 then
  begin
   tr_bi.Construct(g_x1 ,g_y1 ,g_x2 ,g_y2 ,m_quad.polygon );

   if tr_bi.is_valid then
    begin
    // Render transformed lion
     trans.Construct (@g_path ,@tr_bi );
     render_all_paths(@g_rasterizer ,@g_scanline ,@r ,@trans ,@g_colors ,@g_path_idx ,g_npaths );

    // Render transformed ellipse
     ell.Construct(
      (g_x1 + g_x2 ) * 0.5 ,(g_y1 + g_y2 ) * 0.5 ,
      (g_x2 - g_x1 ) * 0.5 ,(g_y2 - g_y1 ) * 0.5 ,200 );

     ell_stroke.Construct(@ell );
     ell_stroke.width_   (3.0 );
     trans_ell.Construct (@ell ,@tr_bi );

     trans_ell_stroke.Construct(@ell_stroke ,@tr_bi );

     g_rasterizer.add_path(@trans_ell );

     rgba.ConstrDbl(0.5 ,0.3 ,0.0 ,0.3 );
     r.color_      (@rgba );

     render_scanlines(@g_rasterizer ,@g_scanline ,@r );

     g_rasterizer.add_path(@trans_ell_stroke );

     rgba.ConstrDbl(0.0 ,0.3 ,0.2 ,1.0 );
     r.color_      (@rgba );

     render_scanlines(@g_rasterizer ,@g_scanline ,@r );

    // Free
     ell_stroke.Destruct;

    end;

  end
 else
  begin
   tr_pe.Construct(g_x1 ,g_y1 ,g_x2 ,g_y2 ,m_quad.polygon );

   if tr_pe.is_valid then
    begin
    // Render transformed lion
     trans.Construct(@g_path ,@tr_pe );

     render_all_paths(@g_rasterizer ,@g_scanline ,@r ,@trans ,@g_colors ,@g_path_idx ,g_npaths );

    // Render transformed ellipse
     ell.Construct(
      (g_x1 + g_x2 ) * 0.5 ,(g_y1 + g_y2 ) * 0.5 ,
      (g_x2 - g_x1 ) * 0.5 ,(g_y2 - g_y1 ) * 0.5 ,200 );

     ell_stroke.Construct(@ell );
     ell_stroke.width_   (3.0 );

     trans_ell.Construct       (@ell ,@tr_pe );
     trans_ell_stroke.Construct(@ell_stroke ,@tr_pe );

     g_rasterizer.add_path(@trans_ell );

     rgba.ConstrDbl(0.5 ,0.3 ,0.0 ,0.3 );
     r.color_      (@rgba );

     render_scanlines(@g_rasterizer ,@g_scanline ,@r );

     g_rasterizer.add_path(@trans_ell_stroke );

     rgba.ConstrDbl(0.0 ,0.3 ,0.2 ,1.0 );
     r.color_      (@rgba );

     render_scanlines(@g_rasterizer ,@g_scanline ,@r );

    // Free
     ell_stroke.Destruct;

    // Testing the reverse transformations
    { tr2.Construct(m_quad.polygon ,g_x1 ,g_y1 ,g_x2 ,g_y2 );

     if tr2.is_valid then
      begin
       x:=m_quad.xn_ptr(0 )^;
       y:=m_quad.yn_ptr(0 )^;

       tr2.transform         (@tr2 ,@x ,@y );
       g_rasterizer.move_to_d(x ,y );

       x:=m_quad.xn_ptr(1 )^;
       y:=m_quad.yn_ptr(1 )^;

       tr2.transform         (@tr2 ,@x ,@y );
       g_rasterizer.line_to_d(x ,y );

       x:=m_quad.xn_ptr(2 )^;
       y:=m_quad.yn_ptr(2 )^;

       tr2.transform         (@tr2 ,@x ,@y );
       g_rasterizer.line_to_d(x ,y );

       x:=m_quad.xn_ptr(3 )^;
       y:=m_quad.yn_ptr(3 )^;

       tr2.transform         (@tr2 ,@x ,@y );
       g_rasterizer.line_to_d(x ,y );

       rgba.ConstrDbl(0.5 ,0.0 ,0.0 ,0.5 );
       r.color_      (@rgba );

       render_scanlines(@g_rasterizer ,@g_scanline ,@r );

      end
     else
      message_('Singularity...' );{}

    end;

  end;

// Render the "quad" tool and controls
 g_rasterizer.add_path(@m_quad );

 rgba.ConstrDbl(0 ,0.3 ,0.5 ,0.6 );
 r.color_      (@rgba );

 render_scanlines(@g_rasterizer ,@g_scanline ,@r );
 render_ctrl     (@g_rasterizer ,@g_scanline ,@r ,@m_trans_type );

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 if flags and mouse_left <> 0 then
  if m_quad.on_mouse_move(x ,y ) then
   force_redraw;

 if flags and mouse_left = 0 then
  on_mouse_button_up(x ,y ,flags );

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
begin
 if flags and mouse_left <> 0 then
  if m_quad.on_mouse_button_down(x ,y ) then
   force_redraw;

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
   'Perspective and bilinear transformations. In general, these classes can transform '#13 +
   'an arbitrary quadrangle to another arbitrary quadrangle (with some restrictions). '#13 +
   'The example demonstrates how to transform a rectangle to a quadrangle defined by '#13 +
   '4 vertices. Note, that the perspective transformations don''t work correctly if '#13 +
   'the destination quadrangle is concave. Bilinear thansformations give a different '#13 +
   'result, but remain valid with any shape of the destination quadrangle.'#13#13 +
   'How to play with:'#13#13 +
   'You can drag the 4 corners of the quadrangle, as well as its boundaries.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
// Rendering
 g_rasterizer.Construct;
 g_scanline.Construct;
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
 app.Construct(pix_format ,flip_y );
 app.caption_ ('AGG Example. Perspective Transformations (F1-Help)' );

 if app.init(600 ,600 ,window_resize ) then
  app.run;

 app.Destruct;

// Free
 g_rasterizer.Destruct;
 g_scanline.Destruct;
 g_path.Destruct;

END.