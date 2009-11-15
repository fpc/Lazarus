//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 rasterizers ;

{$DEFINE AGG_BGR24 }

uses
 SysUtils ,

 agg_basics ,
 agg_platform_support ,
 agg_math ,

 agg_ctrl ,
 agg_slider_ctrl ,
 agg_cbox_ctrl ,

 agg_rasterizer_scanline_aa ,
 agg_rasterizer_outline ,
 agg_scanline ,
 agg_scanline_p ,
 agg_scanline_bin ,

 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_renderer_primitives ,
 agg_render_scanlines ,

 agg_path_storage ,
 agg_gamma_functions

{$I pixel_formats.inc }
{$I agg_mode.inc }

const
 flip_y = true;

type
 the_application = object(platform_support )
   m_x ,
   m_y : array[0..2 ] of double;

   m_dx ,
   m_dy : double;

   m_idx : int;

   m_gamma ,
   m_alpha : slider_ctrl;
   m_test  : cbox_ctrl;

   m_ras    : rasterizer_scanline_aa;
   m_sl_p8  : scanline_p8;
   m_sl_bin : scanline_bin;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure draw_anti_aliased;
   procedure draw_aliased;

   procedure on_draw; virtual;

   procedure on_mouse_move(x ,y : int; flags : unsigned ); virtual;

   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;
   procedure on_ctrl_change; virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_ras.Construct;
 m_sl_p8.Construct;
 m_sl_bin.Construct;

 m_idx:=-1;

 m_x[0 ]:=100 + 120; m_y[0 ]:=60;
 m_x[1 ]:=369 + 120; m_y[1 ]:=170;
 m_x[2 ]:=143 + 120; m_y[2 ]:=310;

 m_gamma.Construct(130 + 10 ,10 + 4 ,130 + 150 ,10 + 8 + 4 ,not flip_y );

 add_ctrl(@m_gamma );

 m_gamma.range_(0 ,1 );
 m_gamma.value_(0.5 );
 m_gamma.label_('Gamma=%1.2f' );
 m_gamma.no_transform;

 m_alpha.Construct(130 + 150 + 10 ,10 + 4 ,500 - 10 ,10 + 8 + 4 ,not flip_y );

 add_ctrl(@m_alpha );

 m_alpha.range_(0 ,1 );
 m_alpha.value_(1 );
 m_alpha.label_('Alpha=%1.2f' );
 m_alpha.no_transform;

 m_test.Construct(130 + 10.0 ,10.0 + 4.0 + 16.0 ,'Test Performance' ,not flip_y );

 add_ctrl(@m_test );

 m_test.no_transform;

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_alpha.Destruct;
 m_gamma.Destruct;
 m_test.Destruct;

 m_ras.Destruct;
 m_sl_p8.Destruct;
 m_sl_bin.Destruct;

end;

{ DRAW_ANTI_ALIASED }
procedure the_application.draw_anti_aliased;
var
 pixf   : pixel_formats;
 rb     : renderer_base;
 ren_aa : renderer_scanline_aa_solid;

 path : path_storage;
 rgba : aggclr;

 gamma : gamma_power;

begin
 pixfmt(pixf ,rbuf_window );

 rb.Construct    (@pixf );
 ren_aa.Construct(@rb );

// Path & Color
 path.Construct;

 path.move_to(m_x[0 ] ,m_y[0 ] );
 path.line_to(m_x[1 ] ,m_y[1 ] );
 path.line_to(m_x[2 ] ,m_y[2 ] );
 path.close_polygon;

 rgba.ConstrDbl(0.7 ,0.5 ,0.1 ,m_alpha._value );
 ren_aa.color_ (@rgba );

// Draw
 gamma.Construct(m_gamma._value * 2.0 );

 m_ras.gamma   (@gamma );
 m_ras.add_path(@path );

 render_scanlines(@m_ras ,@m_sl_p8 ,@ren_aa );

// Free
 path.Destruct;

end;

{ DRAW_ALIASED }
procedure the_application.draw_aliased;
var
 pixf    : pixel_formats;
 rb      : renderer_base;
 ren_bin : renderer_scanline_bin_solid;

 path : path_storage;
 rgba : aggclr;

 gamma : gamma_threshold;

 ren_pr   : renderer_primitives;
 ras_line : rasterizer_outline;

begin
 pixfmt(pixf ,rbuf_window );

 rb.Construct     (@pixf );
 ren_bin.Construct(@rb );

// Path & Color
 path.Construct;

 path.move_to(m_x[0 ] - 200 ,m_y[0 ] );
 path.line_to(m_x[1 ] - 200 ,m_y[1 ] );
 path.line_to(m_x[2 ] - 200 ,m_y[2 ] );
 path.close_polygon;

 rgba.ConstrDbl(0.1 ,0.5 ,0.7 ,m_alpha._value );
 ren_bin.color_(@rgba );

// Draw
 gamma.Construct(m_gamma._value );

 m_ras.gamma   (@gamma );
 m_ras.add_path(@path );

 render_scanlines(@m_ras ,@m_sl_bin ,@ren_bin );

//-- Drawing an outline with subpixel accuracy (aliased)
(* ren_pr.Construct  (@rb );
 ras_line.Construct(@ren_pr );

 rgba.ConstrDbl(0.0 ,0.0 ,0.0 );

 ren_pr._line_color(@rgba );
 ras_line.add_path (@path );(**)

// Free
 path.Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf   : pixel_formats;
 rb     : renderer_base;
 ren_aa : renderer_scanline_aa_solid;

 ras_aa : rasterizer_scanline_aa;

 rgba : aggclr;

begin
// Initialize structures
 pixfmt(pixf ,rbuf_window );

 rb.Construct    (@pixf );
 ren_aa.Construct(@rb );

 ras_aa.Construct;

// Setup colors & background
 rgba.ConstrDbl(1 ,1 ,1 );

 rb.clear(@rgba );

// Draw
 draw_anti_aliased;
 draw_aliased;

// Render controls
 render_ctrl(@ras_aa ,@m_sl_p8 ,@ren_aa ,@m_gamma );
 render_ctrl(@ras_aa ,@m_sl_p8 ,@ren_aa ,@m_alpha );
 render_ctrl(@ras_aa ,@m_sl_p8 ,@ren_aa ,@m_test );

// Free AGG resources
 ras_aa.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
var
 dx ,dy : double;

begin
 if flags and mouse_left <> 0 then
  begin
   if m_idx = 3 then
    begin
     dx:=x - m_dx;
     dy:=y - m_dy;

     m_x[1 ]:=m_x[1 ] - (m_x[0 ] - dx );
     m_y[1 ]:=m_y[1 ] - (m_y[0 ] - dy );
     m_x[2 ]:=m_x[2 ] - (m_x[0 ] - dx );
     m_y[2 ]:=m_y[2 ] - (m_y[0 ] - dy );
     m_x[0 ]:= dx;
     m_y[0 ]:= dy;

     force_redraw;
     exit;

    end;

   if m_idx >= 0 then
    begin
     m_x[m_idx ]:=x - m_dx;
     m_y[m_idx ]:=y - m_dy;

     force_redraw;

    end;

  end;

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
var
 i : unsigned;

begin
 if flags and mouse_left <> 0 then
  begin
   i:=0;

   while i < 3 do
    begin
     if (Sqrt((x - m_x[i ] ) * (x - m_x[i ] ) + (y - m_y[i ] ) * (y - m_y[i ] ) ) < 20 ) or
        (Sqrt((x - m_x[i ] + 200 ) * (x - m_x[i ] + 200 ) + (y - m_y[i ] ) * (y - m_y[i ] ) ) < 20 ) then
      begin
       m_dx :=x - m_x[i ];
       m_dy :=y - m_y[i ];
       m_idx:=i;

       break;

      end;

     inc(i );

    end;

   if i = 3 then
    if point_in_triangle(m_x[0 ] ,m_y[0 ] ,m_x[1 ] ,m_y[1 ] ,m_x[2 ] ,m_y[2 ] ,x ,y ) or
       point_in_triangle(m_x[0 ] - 200 ,m_y[0 ] ,m_x[1 ] - 200 ,m_y[1 ] ,m_x[2 ] - 200 ,m_y[2 ] ,x ,y ) then
     begin
      m_dx :=x - m_x[0 ];
      m_dy :=y - m_y[0 ];
      m_idx:= 3;

     end;

  end;

end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
 m_idx:=-1;

end;

{ ON_KEY }
procedure the_application.on_key;
var
 dx ,dy : double;

begin
 dx:=0;
 dy:=0;

 case key of
  key_left  : dx:=-0.1;
  key_right : dx:= 0.1;
  key_up    : dy:= 0.1;
  key_down  : dy:=-0.1;

 end;

 m_x[0 ]:=m_x[0 ] + dx;
 m_y[0 ]:=m_y[0 ] + dy;
 m_x[1 ]:=m_x[1 ] + dx;
 m_y[1 ]:=m_y[1 ] + dy;

 force_redraw;

 if key = key_f1 then
  message_(
   'It''s a very simple example that was written to compare the performance between '#13 +
   'Anti-Aliased and regular polygon filling. It appears that the most expensive '#13 +
   'operation is rendering of horizontal scanlines. So that, we can use the very '#13 +
   'same rasterization algorithm to draw regular, aliased polygons. Of course, it''s '#13 +
   'possible to write a special version of the rasterizer that will work faster, but '#13 +
   'won''t calculate the pixel coverage values. But on the other hand, the existing '#13 +
   'version of the rasterizer_scanline_aa allows you to change gamma, and to "dilate" '#13 +
   'or "shrink" the polygons in range of ± 1 pixel.'#13#13 +
   'How to play with:'#13#13 +
   'As usual, you can drag the triangles as well as the vertices of them. '#13 +
   'Compare the performance with different shapes and opacity.' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

{ ON_CTRL_CHANGE }
procedure the_application.on_ctrl_change;
var
 i : int;

 t1 ,t2 : double;

 buf : array[0..99 ] of char;

begin
 if m_test._status then
  begin
   on_draw;
   update_window;
   m_test.status_(false );

   start_timer;

   for i:=0 to 999 do
    draw_aliased;

   t1:=elapsed_time;

   start_timer;

   for i:=0 to 999 do
    draw_anti_aliased;

   t2:=elapsed_time;

   update_window;

   sprintf (@buf[0 ] ,'Time Aliased=%.2fms '#0 ,t1 );
   sprintf (@buf[StrLen(@buf[0 ] ) ] ,'Time Anti-Aliased=%.2fms'#0 ,t2 );
   message_(@buf[0 ] );

  end;

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format ,flip_y );
 app.caption_ ('AGG Example. Line Join (F1-Help)' );

 if app.init(500 ,330 ,window_resize ) then
  app.run;

 app.Destruct;

END.
