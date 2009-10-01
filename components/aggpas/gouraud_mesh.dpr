//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 gouraud_mesh ;

uses
 SysUtils ,
 agg_basics ,
 agg_color ,
 agg_array ,
 agg_math ,
 agg_math_stroke ,
 agg_platform_support ,
 agg_ctrl ,
 agg_slider_ctrl ,
 agg_bezier_ctrl ,
 agg_rbox_ctrl ,
 agg_cbox_ctrl ,
 agg_renderer_base ,
 agg_rendering_buffer ,
 agg_conv_transform ,
 agg_conv_stroke ,
 agg_conv_clip_polyline ,
 agg_scanline_u ,
 agg_scanline_bin ,
 agg_renderer_scanline ,
 agg_rasterizer_outline_aa ,
 agg_rasterizer_scanline_aa ,
 agg_span_allocator ,
 agg_span_gouraud_rgba ,
 agg_gamma_lut ,
 agg_arc ,
 agg_bezier_arc ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,
 agg_pixfmt_rgba ,
 agg_bounding_rect ,
 agg_vpgen_clip_polygon ,
 agg_rasterizer_compound_aa ,
 agg_gsv_text ;

{$I agg_mode.inc }

const
 flip_y = true;

type
 mesh_point_ptr = ^mesh_point;
 mesh_point = object
   x ,y ,dx ,dy : double;
   color ,dc    : aggclr;

   constructor Construct; overload;
   constructor Construct(x_ ,y_ ,dx_ ,dy_ : double; c ,dc_ : aggclr_ptr ); overload;

  end;

 mesh_triangle_ptr = ^mesh_triangle;
 mesh_triangle = object
   p1 ,p2 ,p3 : unsigned;

   constructor Construct; overload;
   constructor Construct(i ,j ,k : unsigned ); overload;

  end;

 mesh_edge_ptr = ^mesh_edge;
 mesh_edge = object
   p1 ,p2 : unsigned;
   tl ,tr : int;

   constructor Construct; overload;
   constructor Construct(p1_ ,p2_ : unsigned; tl_ ,tr_ : int ); overload;

  end;

 mesh_ctrl_ptr = ^mesh_ctrl;
 mesh_ctrl = object
   m_cols ,
   m_rows : unsigned;

   m_drag_idx : int;

   m_drag_dx ,
   m_drag_dy ,
   m_cell_w  ,
   m_cell_h  ,
   m_start_x ,
   m_start_y : double;

   m_vertices  ,
   m_triangles ,
   m_edges     : pod_bvector;

   constructor Construct;
   destructor  Destruct;

   procedure generate(
              cols ,rows : unsigned;
              cell_w ,cell_h ,start_x ,start_y : double );

   procedure randomize_points(delta : double );
   procedure rotate_colors;

   function  on_mouse_button_down(x ,y : double; flags : unsigned ) : boolean;
   function  on_mouse_move       (x ,y : double; flags : unsigned ) : boolean;
   function  on_mouse_button_up  (x ,y : double; flags : unsigned ) : boolean;

   function  num_vertices : unsigned;
   function  vertex(i : unsigned ) : mesh_point_ptr; overload;
   function  vertex(x ,y : unsigned ) : mesh_point_ptr; overload;

   function  num_triangles : unsigned;
   function  triangle(i : unsigned ) : mesh_triangle_ptr;

   function  num_edges : unsigned;
   function  edge(i : unsigned ) : mesh_edge_ptr;

  end;

 styles_gouraud = object(style_handler )
   m_triangles : pod_bvector;

   m_rgba : aggclr;

   constructor Construct(mesh : mesh_ctrl_ptr; gamma : gamma_ptr );
   destructor  Destruct;

   function  is_solid(style : unsigned ) : boolean; virtual;
   function  color   (style : unsigned ) : aggclr_ptr; virtual;

   procedure generate_span(span : aggclr_ptr; x ,y : int; len ,style : unsigned ); virtual;

  end;

 the_application = object(platform_support )
   m_mesh  : mesh_ctrl;
   m_gamma : gamma_lut;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_mouse_move(x ,y : int; flags : unsigned ); virtual;

   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;
   procedure on_idle; virtual;
   procedure on_ctrl_change; virtual;

  end;

{ random }
function random_(v1 ,v2 : double ) : double;
begin
 result:=(v2 - v1 ) * (rand mod 1000 ) / 999.0 + v1;

end;

{ CONSTRUCT }
constructor mesh_point.Construct;
begin
 x :=0;
 y :=0;
 dx:=0;
 dy:=0;

 color.Construct;
 dc.Construct;

end;

{ CONSTRUCT }
constructor mesh_point.Construct(x_ ,y_ ,dx_ ,dy_ : double; c ,dc_ : aggclr_ptr );
begin
 x :=x_;
 y :=y_;
 dx:=dx_;
 dy:=dy_;

 color.Construct(c );
 dc.Construct(dc_ );

end;

{ CONSTRUCT }
constructor mesh_triangle.Construct;
begin
 p1:=0;
 p2:=0;
 p3:=0;

end;

{ CONSTRUCT }
constructor mesh_triangle.Construct(i ,j ,k : unsigned );
begin
 p1:=i;
 p2:=j;
 p3:=k;

end;

{ CONSTRUCT }
constructor mesh_edge.Construct;
begin
 p1:=0;
 p2:=0;
 tl:=0;
 tr:=0;

end;

{ CONSTRUCT }
constructor mesh_edge.Construct(p1_ ,p2_ : unsigned; tl_ ,tr_ : int );
begin
 p1:=p1_;
 p2:=p2_;
 tl:=tl_;
 tr:=tr_;

end;

{ CONSTRUCT }
constructor mesh_ctrl.Construct;
begin
 m_vertices.Construct(sizeof(mesh_point ) );
 m_triangles.Construct(sizeof(mesh_triangle ) );
 m_edges.Construct(sizeof(mesh_edge ) );

 m_cols:=0;
 m_rows:=0;

 m_drag_idx:=-1;
 m_drag_dx :=0;
 m_drag_dy :=0;

end;

{ DESTRUCT }
destructor mesh_ctrl.Destruct;
begin
 m_vertices.Destruct;
 m_triangles.Destruct;
 m_edges.Destruct;

end;

{ GENERATE }
procedure mesh_ctrl.generate(
           cols ,rows : unsigned;
           cell_w ,cell_h ,start_x ,start_y : double );
var
 i ,j : unsigned;

 x ,dx ,dy : double;

 p1 ,p2 ,p3 ,p4 ,curr_cell ,left_cell ,bott_cell ,

 curr_t1 ,curr_t2 ,left_t1 ,left_t2 ,bott_t1 ,bott_t2 : int;

 c ,dc : aggclr;

 c1 ,c2 ,c3 : int8u;

 mp : mesh_point;
 mt : mesh_triangle;
 me : mesh_edge;

begin
 m_cols  :=cols;
 m_rows  :=rows;
 m_cell_w:=cell_w;
 m_cell_h:=cell_h;

 m_start_x:=start_x;
 m_start_y:=start_y;

 m_vertices.remove_all;

 i:=0;

 while i < m_rows do
  begin
   x:=start_x;
   j:=0;

   while j < m_cols do
    begin
     dx:=random_(-0.5 ,0.5 );
     dy:=random_(-0.5 ,0.5 );

     c1:=rand and $FF;
     c2:=rand and $FF;
     c3:=rand and $FF;

     c.ConstrInt (c3 ,c2 ,c1 );

     c1:=rand and 1;
     c2:=rand and 1;
     c3:=rand and 1;

     dc.ConstrInt(c3 ,c2 ,c1 );

     mp.Construct  (x ,start_y ,dx ,dy ,@c ,@dc );
     m_vertices.add(@mp );

     x:=x + cell_w;

     inc(j );

    end;

   start_y:=start_y + cell_h;

   inc(i );

  end;

//  4---3
//  |t2/|
//  | / |
//  |/t1|
//  1---2
 m_triangles.remove_all;
 m_edges.remove_all;

 i:=0;

 while i < m_rows - 1 do
  begin
   j:=0;

   while j < m_cols - 1 do
    begin
     p1:=i * m_cols + j;
     p2:=p1 + 1;
     p3:=p2 + m_cols;
     p4:=p1 + m_cols;

     mt.Construct   (p1 ,p2 ,p3 );
     m_triangles.add(@mt );
     mt.Construct   (p3 ,p4 ,p1 );
     m_triangles.add(@mt );

     curr_cell:=i * (m_cols - 1 ) + j;

     if j <> 0 then
      left_cell:=int(curr_cell - 1 )
     else
      left_cell:=-1;

     if i <> 0 then
      bott_cell:=int(curr_cell - (m_cols - 1 ) )
     else
      bott_cell:=-1;

     curr_t1:=curr_cell * 2;
     curr_t2:=curr_t1 + 1;

     if left_cell >= 0 then
      left_t1:=left_cell * 2
     else
      left_t1:=-1;

     if left_cell >= 0 then
      left_t2:=left_t1 + 1
     else
      left_t2:=-1;

     if bott_cell >= 0 then
      bott_t1:=bott_cell * 2
     else
      bott_t1:=-1;

     if bott_cell >= 0 then
      bott_t2:=bott_t1 + 1
     else
      bott_t2:=-1;

     me.Construct(p1 ,p2 ,curr_t1 ,bott_t2 );
     m_edges.add (@me );
     me.Construct(p1 ,p3 ,curr_t2 ,curr_t1 );
     m_edges.add (@me );
     me.Construct(p1 ,p4 ,left_t1 ,curr_t2 );
     m_edges.add (@me );

     if j = m_cols - 2 then // Last column
      begin
       me.Construct(p2 ,p3 ,curr_t1 ,-1 );
       m_edges.add (@me );

      end;

     if i = m_rows - 2 then // Last row
      begin
       me.Construct(p3 ,p4 ,curr_t2 ,-1 );
       m_edges.add (@me );

      end;

     inc(j );

    end;

   inc(i );

  end;

end;

{ RANDOMIZE_POINTS }
procedure mesh_ctrl.randomize_points(delta : double );
var
 i ,j : unsigned;

 xc ,yc ,x1 ,y1 ,x2 ,y2 : double;

 p : mesh_point_ptr;

begin
 i:=0;

 while i < m_rows do
  begin
   j:=0;

   while j < m_cols do
    begin
     xc:=j * m_cell_w + m_start_x;
     yc:=i * m_cell_h + m_start_y;
     x1:=xc - m_cell_w / 4;
     y1:=yc - m_cell_h / 4;
     x2:=xc + m_cell_w / 4;
     y2:=yc + m_cell_h / 4;

     p:=vertex(j ,i );

     p.x:=p.x + p.dx;
     p.y:=p.y + p.dy;

     if p.x < x1 then
      begin
       p.x :=x1;
       p.dx:=-p.dx;

      end;

     if p.y < y1 then
      begin
       p.y :=y1;
       p.dy:=-p.dy;

      end;

     if p.x > x2 then
      begin
       p.x :=x2;
       p.dx:=-p.dx;

      end;

     if p.y > y2 then
      begin
       p.y :=y2;
       p.dy:=-p.dy;

      end;

     inc(j );

    end;

   inc(i );

  end;

end;

{ ROTATE_COLORS }
procedure mesh_ctrl.rotate_colors;
var
 i : unsigned;

 c ,dc : aggclr_ptr;

 r ,g ,b : int;

begin
 i:=1;

 while i < m_vertices.size do
  begin
   c :=@mesh_point_ptr(m_vertices.array_operator(i ) ).color;
   dc:=@mesh_point_ptr(m_vertices.array_operator(i ) ).dc;

   if dc.r <> 0 then
    r:=c.r + 5
   else
    r:=c.r - 5;

   if dc.g <> 0 then
    g:=c.g + 5
   else
    g:=c.g - 5;

   if dc.b <> 0 then
    b:=c.b + 5
   else
    b:=c.b - 5;

   if r < 0 then
    begin
     r   :=0;
     dc.r:=dc.r xor 1;

    end;

   if r > 255 then
    begin
     r   :=255;
     dc.r:=dc.r xor 1;

    end;

   if g < 0 then
    begin
     g   :=0;
     dc.g:=dc.g xor 1;

    end;

   if g > 255 then
    begin
     g   :=255;
     dc.g:=dc.g xor 1;

    end;

   if b < 0 then
    begin
     b   :=0;
     dc.b:=dc.b xor 1;

    end;

   if b > 255 then
    begin
     b   := 255;
     dc.b:=dc.b xor 1;

    end;

   c.r:=r;
   c.g:=g;
   c.b:=b;

   inc(i );

  end;

end;

{ ON_MOUSE_BUTTON_DOWN }
function mesh_ctrl.on_mouse_button_down(x ,y : double; flags : unsigned ) : boolean;
var
 i : unsigned;

begin
 if flags and 1 <> 0 then
  begin
   i:=0;

   while i < m_vertices.size do
    begin
     if calc_distance(
         x ,y ,
         mesh_point_ptr(m_vertices.array_operator(i ) ).x ,
         mesh_point_ptr(m_vertices.array_operator(i ) ).y ) < 5 then
      begin
       m_drag_idx:=i;

       m_drag_dx:=x - mesh_point_ptr(m_vertices.array_operator(i ) ).x;
       m_drag_dy:=y - mesh_point_ptr(m_vertices.array_operator(i ) ).y;

       result:=true;

       exit;

      end;

     inc(i );

    end;

  end;

 result:=false;

end;

{ ON_MOUSE_MOVE }
function mesh_ctrl.on_mouse_move(x ,y : double; flags : unsigned ) : boolean;
begin
 if flags and 1 <> 0 then
  begin
   if m_drag_idx >= 0 then
    begin
     mesh_point_ptr(m_vertices.array_operator(m_drag_idx ) ).x:=x - m_drag_dx;
     mesh_point_ptr(m_vertices.array_operator(m_drag_idx ) ).y:=y - m_drag_dy;

     result:=true;

     exit;

    end;

  end
 else
  begin
   result:=on_mouse_button_up(x ,y ,flags );

   exit;

  end;

 result:=false;

end;

{ ON_MOUSE_BUTTON_UP }
function mesh_ctrl.on_mouse_button_up(x ,y : double; flags : unsigned ) : boolean;
begin
 result    :=m_drag_idx >= 0;
 m_drag_idx:=-1;

end;

{ NUM_VERTICES }
function mesh_ctrl.num_vertices : unsigned;
begin
 result:=m_vertices.size;

end;

{ VERTEX }
function mesh_ctrl.vertex(i : unsigned ) : mesh_point_ptr;
begin
 result:=m_vertices.array_operator(i );

end;

{ VERTEX }
function mesh_ctrl.vertex(x ,y : unsigned ) : mesh_point_ptr;
begin
 result:=m_vertices.array_operator(y * m_rows + x );

end;

{ NUM_TRIANGLES }
function mesh_ctrl.num_triangles : unsigned;
begin
 result:=m_triangles.size;

end;

{ TRIANGLE }
function mesh_ctrl.triangle(i : unsigned ) : mesh_triangle_ptr;
begin
 result:=m_triangles.array_operator(i );

end;

{ NUM_EDGES }
function mesh_ctrl.num_edges : unsigned;
begin
 result:=m_edges.size;

end;

{ EDGE }
function mesh_ctrl.edge(i : unsigned ) : mesh_edge_ptr;
begin
 result:=m_edges.array_operator(i );

end;

{ CONSTRUCT }
constructor styles_gouraud.Construct(mesh : mesh_ctrl_ptr; gamma : gamma_ptr );
var
 i : unsigned;
 t : mesh_triangle_ptr;

 p1 ,p2 ,p3 : mesh_point_ptr;
 c1 ,c2 ,c3 : aggclr;

 gouraud : span_gouraud_rgba;

begin
 m_triangles.Construct(sizeof(span_gouraud_rgba ) );
 m_rgba.ConstrInt     (0 ,0 ,0 ,0 );

 i:=0;

 while i < mesh.num_triangles do
  begin
   t :=mesh.triangle(i );
   p1:=mesh.vertex  (t.p1 );
   p2:=mesh.vertex  (t.p2 );
   p3:=mesh.vertex  (t.p3 );

   c1.Construct(@p1.color );
   c2.Construct(@p2.color );
   c3.Construct(@p3.color );

   c1.apply_gamma_dir(gamma );
   c2.apply_gamma_dir(gamma );
   c2.apply_gamma_dir(gamma );

   gouraud.Construct_(
    @c1 ,@c2 ,@c3 ,
    p1.x ,p1.y ,
    p2.x ,p2.y ,
    p3.x ,p3.y );

   gouraud.prepare_;
   m_triangles.add(@gouraud ); 

   inc(i );

  end;

end;

{ DESTRUCT }
destructor styles_gouraud.Destruct;
begin
 m_triangles.Destruct;

end;

{ IS_SOLID }
function styles_gouraud.is_solid(style : unsigned ) : boolean;
begin
 result:=false;

end;

{ COLOR }
function styles_gouraud.color(style : unsigned ) : aggclr_ptr;
begin
 result:=@m_rgba;

end;

{ GENERATE_SPAN }
procedure styles_gouraud.generate_span(span : aggclr_ptr; x ,y : int; len ,style : unsigned );
begin
 span_gouraud_rgba_ptr(
  m_triangles.array_operator(style ) ).generate_(span ,x ,y ,len );

end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_mesh.Construct;
 m_gamma.Construct_;

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_mesh.Destruct;
 m_gamma.Destruct;

// m_gamma.gamma_(2.0 ); 

end;

{ ON_INIT }
procedure the_application.on_init;
begin
 m_mesh.generate(20 ,20 ,17 ,17 ,40 ,40 );

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pf : pixel_formats;

 rgba : aggclr;

 ren_base : renderer_base;

 ren : renderer_scanline_aa_solid;
 ras : rasterizer_scanline_aa;

 sl     : scanline_u8;
 sl_bin : scanline_bin;

 rasc  : rasterizer_compound_aa_int;
 alloc : span_allocator;

 styles : styles_gouraud;

 i : unsigned;
 e : mesh_edge_ptr;

 p1 ,p2 : mesh_point_ptr;

 tm  : double;
 buf : array[0..255 ] of char;

 t  : gsv_text;
 pt : conv_stroke_math;

begin
// Initialize structures
 pixfmt_bgra32_pre(pf ,rbuf_window );

 ren_base.Construct(@pf );

 rgba.ConstrInt(0 ,0 ,0 );
 ren_base.clear(@rgba );
 ren.Construct (@ren_base );

 ras.Construct;
 sl.Construct;
 sl_bin.Construct;

 rasc.Construct;
 alloc.Construct;

 styles.Construct(@m_mesh ,@m_gamma );

 start_timer;
 rasc.reset;

//rasc.clip_box(40 ,40 ,_width - 40 ,_height - 40 ); 

 i:=0;

 while i < m_mesh.num_edges do
  begin
   e :=m_mesh.edge  (i );
   p1:=m_mesh.vertex(e.p1 );
   p2:=m_mesh.vertex(e.p2 );

   rasc.styles   (e.tl ,e.tr );
   rasc.move_to_d(p1.x ,p1.y );
   rasc.line_to_d(p2.x ,p2.y );

   inc(i );

  end;

 render_scanlines_compound(@rasc ,@sl ,@sl_bin ,@ren_base ,@alloc ,@styles );

// Info
 tm:=elapsed_time;

 t.Construct;
 t.size_(10.0 );

 pt.Construct (@t );
 pt.width_    (1.5 );
 pt.line_cap_ (round_cap );
 pt.line_join_(round_join );

 sprintf(@buf[0 ]             ,'%3.2f ms, ' ,tm );
 sprintf(@buf[StrLen(@buf ) ] ,'%d triangles, ' ,m_mesh.num_triangles );
 sprintf(@buf[StrLen(@buf ) ] ,'%.0f tri/sec' ,m_mesh.num_triangles / tm * 1000.0 );

 t.start_point_(10.0 ,10.0 );
 t.text_       (@buf[0 ] );

 ras.add_path  (@pt );
 rgba.ConstrDbl(1 ,1 ,1 );

 render_scanlines_aa_solid(@ras ,@sl ,@ren_base ,@rgba );

 if m_gamma._gamma <> 1.0 then
  pf.apply_gamma_inv(@m_gamma ,bgra_order );

// Free AGG resources
 ras.Destruct;
 sl.Destruct;
 sl_bin.Destruct;

 rasc.Destruct;
 alloc.Destruct;

 styles.Destruct;

 t.Destruct;
 pt.Destruct;

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
 if m_mesh.on_mouse_move(x ,y ,flags ) then
  force_redraw;

end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
begin
 if m_mesh.on_mouse_button_down(x ,y ,flags ) then
  force_redraw;

end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
 if m_mesh.on_mouse_button_up(x ,y ,flags ) then
  force_redraw;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'Yet another example that demonstrates the power of compound shape rasterization. '#13 +
   'Here we create a mesh of triangles and render them in one pass with multiple Gouraud   '#13 +
   'shaders (span_gouraud_rgba). The example demonstrates perfect Anti-Aliasing '#13 +
   'and perfect triangle stitching (seamless edges) at the same time.'#13#13 +
   'How to play with:'#13#13 +
   'You can modify the points of the mesh by left mouse click and drag.' );

end;

{ ON_IDLE }
procedure the_application.on_idle;
begin
 m_mesh.randomize_points(1.0 );
 m_mesh.rotate_colors;
 force_redraw;

end;

{ ON_CTRL_CHANGE }
procedure the_application.on_ctrl_change;
begin
end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgra32 ,flip_y );
 app.caption_ ('AGG Example. (F1-Help)' );

 if app.init(400 ,400 ,0 ) then
  begin
   app.wait_mode_(false );
   app.run;

  end;

 app.Destruct;

END.