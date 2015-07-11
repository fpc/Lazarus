//----------------------------------------------------------------------------
// Anti-Grain Geometry - Version 2.4 (Public License)
// Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)
//
// Anti-Grain Geometry - Version 2.4 Release Milano 3 (AggPas 2.4 RM3)
// Pascal Port By: Milan Marusinec alias Milano
//                 milan@marusinec.sk
//                 http://www.aggpas.org
// Copyright (c) 2005-2006
//
// Permission to copy, use, modify, sell and distribute this software
// is granted provided this copyright notice appears in all copies.
// This software is provided "as is" without express or implied
// warranty, and with no claim as to its suitability for any purpose.
//
//----------------------------------------------------------------------------
// Contact: mcseem@antigrain.com
//          mcseemagg@yahoo.com
//          http://www.antigrain.com
//
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 01.02.2006-Milano: Complete unit port
// 31.01.2006-Milano: Unit port establishment
//
{ agg_rasterizer_outline_aa.pas }
unit
 agg_rasterizer_outline_aa ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_color ,
 agg_line_aa_basics ,
 agg_vertex_source ,
 agg_vertex_sequence ,
 agg_renderer_outline_aa ,
 agg_ctrl ;

{ TYPES DEFINITION }
type
// Vertex (x, y) with the distance to the next one. The last vertex has
// the distance between the last and the first points
 line_aa_vertex_ptr = ^line_aa_vertex;
 line_aa_vertex = object
   x ,y ,len : int;

   constructor Construct; overload;
   constructor Construct(x_ ,y_ : int ); overload;

  end;

 draw_vars_ptr = ^draw_vars;
 draw_vars = record
   idx : unsigned;

   x1 ,y1 ,x2 ,y2 : int;

   curr ,next : line_parameters;

   lcurr ,lnext ,xb1 ,yb1 ,xb2 ,yb2 : int;

   flags : unsigned;

  end;

 rasterizer_outline_aa_ptr = ^rasterizer_outline_aa; 
 rasterizer_outline_aa = object
   m_ren : renderer_outline_ptr;

   m_src_vertices : vertex_sequence;

   m_accurate_join ,
   m_round_cap     : boolean;

   m_start_x ,
   m_start_y : int;

   constructor Construct(ren : renderer_outline_ptr );
   destructor  Destruct;

   procedure renderer(ren : renderer_outline_ptr );

   procedure draw(dv : draw_vars_ptr; start ,end_ : unsigned );

   procedure accurate_join_(v : boolean );
   function  _accurate_join : boolean;

   procedure round_cap_(v : boolean );
   function  _round_cap : boolean;

   procedure move_to(x ,y : int );
   procedure line_to(x ,y : int );

   procedure move_to_d(x ,y : double );
   procedure line_to_d(x ,y : double );

   procedure render(close_polygon : boolean );

   procedure add_vertex(x ,y : double; cmd : unsigned );
   procedure add_path  (vs : vertex_source_ptr; path_id : unsigned = 0 );

   procedure render_all_paths(
              vs : vertex_source_ptr;
              colors  : aggclr_ptr;
              path_id : unsigned_ptr;
              num_paths : unsigned );

   procedure render_ctrl(c : ctrl_ptr );

  end;

{ GLOBAL PROCEDURES }
 function cmp_dist_start(d : int ) : boolean;
 function cmp_dist_end  (d : int ) : boolean;

 function line_aa_vertex_func_operator(this ,val : line_aa_vertex_ptr ) : boolean;


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CMP_DIST_START }
function cmp_dist_start;
begin
 result:=d > 0;

end;

{ CMP_DIST_END }
function cmp_dist_end;
begin
 result:=d <= 0;

end;

{ CONSTRUCT }
constructor line_aa_vertex.Construct;
begin
end;

{ CONSTRUCT }
constructor line_aa_vertex.Construct(x_ ,y_ : int );
begin
 x  :=x_;
 y  :=y_;
 len:=0;

end;

{ LINE_AA_VERTEX_FUNC_OPERATOR }
function line_aa_vertex_func_operator;
var
 dx ,dy : double;

begin
 dx:=val.x - this.x;
 dy:=val.y - this.y;

 this.len:=trunc(Sqrt(dx * dx + dy * dy ) );

 result:=this.len > (line_subpixel_size + line_subpixel_size div 2 );

end;

{ CONSTRUCT }
constructor rasterizer_outline_aa.Construct;
begin
 m_src_vertices.Construct(sizeof(line_aa_vertex ) ,6 ,@line_aa_vertex_func_operator );

 m_ren:=ren;

 m_accurate_join:=m_ren.accurate_join_only;
 m_round_cap    :=false;

 m_start_x:=0;
 m_start_y:=0;

end;

{ DESTRUCT }
destructor rasterizer_outline_aa.Destruct;
begin
 m_src_vertices.Destruct;

end;

{ RENDERER }
procedure rasterizer_outline_aa.renderer(ren : renderer_outline_ptr );
begin
 m_ren:=ren;

end;

{ DRAW }
procedure rasterizer_outline_aa.draw;
var
 i : unsigned;
 v : line_aa_vertex_ptr;

begin
 i:=start;

 while i < end_ do
  begin
   case dv.flags of
    0 : m_ren.line3(@dv.curr ,dv.xb1 ,dv.yb1 ,dv.xb2 ,dv.yb2 );
    1 : m_ren.line2(@dv.curr ,dv.xb2 ,dv.yb2 );
    2 : m_ren.line1(@dv.curr ,dv.xb1 ,dv.yb1 );
    3 : m_ren.line0(@dv.curr );

   end;

   dv.x1:=dv.x2;
   dv.y1:=dv.y2;

   dv.lcurr:=dv.lnext;
   dv.lnext:=line_aa_vertex_ptr(m_src_vertices.array_operator(dv.idx ) ).len;

   inc(dv.idx );

   if dv.idx >= m_src_vertices.size then
    dv.idx:=0;

   v:=m_src_vertices.array_operator(dv.idx );

   dv.x2:=v.x;
   dv.y2:=v.y;

   dv.curr:=dv.next;

   dv.next.Construct(dv.x1 ,dv.y1 ,dv.x2 ,dv.y2 ,dv.lnext );

   dv.xb1:=dv.xb2;
   dv.yb1:=dv.yb2;

   if m_accurate_join then
    dv.flags:=0
   else
    begin
     dv.flags:=dv.flags shr 1;

     dv.flags:=
      dv.flags or (
       unsigned(
        dv.curr.diagonal_quadrant =
        dv.next.diagonal_quadrant ) shl 1 );
        
    end;

   if dv.flags and 2 = 0 then
    bisectrix(@dv.curr ,@dv.next ,@dv.xb2 ,@dv.yb2 );

   inc(i )

  end;

end;

{ ACCURATE_JOIN_ }
procedure rasterizer_outline_aa.accurate_join_;
begin
 if m_ren.accurate_join_only then
  m_accurate_join:=true
 else
  m_accurate_join:=v;

end;

{ _ACCURATE_JOIN }
function rasterizer_outline_aa._accurate_join;
begin
 result:=m_accurate_join;

end;

{ ROUND_CAP_ }
procedure rasterizer_outline_aa.round_cap_;
begin
 m_round_cap:=v;

end;

{ _ROUND_CAP }
function rasterizer_outline_aa._round_cap;
begin
 result:=m_round_cap;

end;

{ MOVE_TO }
procedure rasterizer_outline_aa.move_to;
var
 vt : line_aa_vertex;

begin
 m_start_x:=x;
 m_start_y:=y;

 vt.Construct(x ,y );

 m_src_vertices.modify_last(@vt );

end;

{ LINE_TO }
procedure rasterizer_outline_aa.line_to;
var
 vt : line_aa_vertex;

begin
 vt.Construct(x ,y );

 m_src_vertices.add(@vt );

end;

{ MOVE_TO_D }
procedure rasterizer_outline_aa.move_to_d;
begin
 move_to(line_coord(x ) ,line_coord(y ) );

end;

{ LINE_TO_D }
procedure rasterizer_outline_aa.line_to_d;
begin
 line_to(line_coord(x ) ,line_coord(y ) );

end;

{ RENDER }
procedure rasterizer_outline_aa.render;
var
 dv : draw_vars;
 v  : line_aa_vertex_ptr;

 x1 ,y1 ,x2 ,y2 ,lprev ,x3 ,y3 ,lnext : int;

 prev ,lp ,lp1 ,lp2 : line_parameters;

begin
 m_src_vertices.close(close_polygon );

 if close_polygon then
  if m_src_vertices.size >= 3 then
   begin
    dv.idx:=2;

    v    :=m_src_vertices.array_operator(m_src_vertices.size - 1 );
    x1   :=v.x;
    y1   :=v.y;
    lprev:=v.len;

    v :=m_src_vertices.array_operator(0 );
    x2:=v.x;
    y2:=v.y;

    dv.lcurr:=v.len;

    prev.Construct(x1 ,y1 ,x2 ,y2 ,lprev );

    v    :=m_src_vertices.array_operator(1 );
    dv.x1:=v.x;
    dv.y1:=v.y;

    dv.lnext:=v.len;

    dv.curr.Construct(x2 ,y2 ,dv.x1 ,dv.y1 ,dv.lcurr );

    v    :=m_src_vertices.array_operator(dv.idx );
    dv.x2:=v.x;
    dv.y2:=v.y;

    dv.next.Construct(dv.x1 ,dv.y1 ,dv.x2 ,dv.y2 ,dv.lnext );

    dv.xb1:=0;
    dv.yb1:=0;
    dv.xb2:=0;
    dv.yb2:=0;

    if m_accurate_join then
     dv.flags:=0
    else
     dv.flags:=
      unsigned(prev.diagonal_quadrant = dv.curr.diagonal_quadrant ) or
      (unsigned(dv.curr.diagonal_quadrant = dv.next.diagonal_quadrant ) shl 1 );

    if dv.flags and 1 = 0 then
     bisectrix(@prev ,@dv.curr ,@dv.xb1 ,@dv.yb1 );

    if dv.flags and 2 = 0 then
     bisectrix(@dv.curr ,@dv.next ,@dv.xb2 ,@dv.yb2 );

    draw(@dv ,0 ,m_src_vertices.size ); 

   end
  else
 else
  case m_src_vertices.size of
   2 :
    begin
     v    :=m_src_vertices.array_operator(0 );
     x1   :=v.x;
     y1   :=v.y;
     lprev:=v.len;
     v    :=m_src_vertices.array_operator(1 );
     x2   :=v.x;
     y2   :=v.y;

     lp.Construct(x1 ,y1 ,x2 ,y2 ,lprev );

     if m_round_cap then
      m_ren.semidot(@cmp_dist_start ,x1 ,y1 ,x1 + (y2 - y1 ) ,y1 - (x2 - x1 ) );

     m_ren.line3(
      @lp ,
      x1 + (y2 - y1 ) ,
      y1 - (x2 - x1 ) ,
      x2 + (y2 - y1 ) ,
      y2 - (x2 - x1 ) );
      
     if m_round_cap then
      m_ren.semidot(@cmp_dist_end ,x2 ,y2 ,x2 + (y2 - y1 ) ,y2 - (x2 - x1 ) );

    end;

   3 :
    begin
     v    :=m_src_vertices.array_operator(0 );
     x1   :=v.x;
     y1   :=v.y;
     lprev:=v.len;
     v    :=m_src_vertices.array_operator(1 );
     x2   :=v.x;
     y2   :=v.y;
     lnext:=v.len;
     v    :=m_src_vertices.array_operator(2 );
     x3   :=v.x;
     y3   :=v.y;

     lp1.Construct(x1 ,y1 ,x2 ,y2 ,lprev );
     lp2.Construct(x2 ,y2 ,x3 ,y3 ,lnext );

     bisectrix(@lp1 ,@lp2 ,@dv.xb1 ,@dv.yb1 );

     if m_round_cap then
      m_ren.semidot(@cmp_dist_start ,x1 ,y1 ,x1 + (y2 - y1 ) ,y1 - (x2 - x1 ) );

     m_ren.line3(
      @lp1 ,
      x1 + (y2 - y1 ) ,
      y1 - (x2 - x1 ) ,
      dv.xb1 ,
      dv.yb1 );

     m_ren.line3(
      @lp2 ,
      dv.xb1 ,
      dv.yb1 ,
      x3 + (y3 - y2 ) ,
      y3 - (x3 - x2 ) );

     if m_round_cap then
      m_ren.semidot(@cmp_dist_end ,x3 ,y3 ,x3 + (y3 - y2 ) ,y3 - (x3 - x2 ) );

    end;

   0 ,1 :
   else
    begin
     dv.idx:=3;

     v    :=m_src_vertices.array_operator(0 );
     x1   :=v.x;
     y1   :=v.y;
     lprev:=v.len;

     v :=m_src_vertices.array_operator(1 );
     x2:=v.x;
     y2:=v.y;

     dv.lcurr:=v.len;

     prev.Construct(x1 ,y1 ,x2 ,y2 ,lprev );

     v    :=m_src_vertices.array_operator(2 );
     dv.x1:=v.x;
     dv.y1:=v.y;

     dv.lnext:=v.len;

     dv.curr.Construct(x2 ,y2 ,dv.x1 ,dv.y1 ,dv.lcurr );

     v    :=m_src_vertices.array_operator(dv.idx );
     dv.x2:=v.x;
     dv.y2:=v.y;

     dv.next.Construct(dv.x1 ,dv.y1 ,dv.x2 ,dv.y2 ,dv.lnext );

     dv.xb1:=0;
     dv.yb1:=0;
     dv.xb2:=0;
     dv.yb2:=0;

     if m_accurate_join then
      dv.flags:=0
     else
      dv.flags:=
       unsigned(prev.diagonal_quadrant = dv.curr.diagonal_quadrant ) or
       (unsigned(dv.curr.diagonal_quadrant = dv.next.diagonal_quadrant ) shl 1 );

     if dv.flags and 1 = 0 then
      begin
       bisectrix  (@prev ,@dv.curr ,@dv.xb1 ,@dv.yb1 );
       m_ren.line3(
        @prev ,
        x1 + (y2 - y1 ) ,
        y1 - (x2 - x1 ) ,
        dv.xb1 ,
        dv.yb1 );

      end
     else
      m_ren.line1(@prev ,x1 + (y2 - y1 ) ,y1 - (x2 - x1 ) );

     if m_round_cap then
      m_ren.semidot(@cmp_dist_start ,x1 ,y1 ,x1 + (y2 - y1 ) ,y1 - (x2 - x1 ) );

     if dv.flags and 2 = 0 then
      bisectrix(@dv.curr ,@dv.next ,@dv.xb2 ,@dv.yb2 );

     draw(@dv ,1 ,m_src_vertices.size - 2 );

     if dv.flags and 1 = 0 then
      m_ren.line3(
       @dv.curr ,
       dv.xb1 ,
       dv.yb1 ,
       dv.curr.x2 + (dv.curr.y2 - dv.curr.y1 ) ,
       dv.curr.y2 - (dv.curr.x2 - dv.curr.x1 ) )
     else  
      m_ren.line2(
       @dv.curr ,
       dv.curr.x2 + (dv.curr.y2 - dv.curr.y1 ) ,
       dv.curr.y2 - (dv.curr.x2 - dv.curr.x1 ) );

     if m_round_cap then   
      m_ren.semidot(
       @cmp_dist_end ,dv.curr.x2 ,dv.curr.y2 ,
       dv.curr.x2 + (dv.curr.y2 - dv.curr.y1 ) ,
       dv.curr.y2 - (dv.curr.x2 - dv.curr.x1 ) );

    end;

  end;

 m_src_vertices.remove_all;

end;

{ ADD_VERTEX }
procedure rasterizer_outline_aa.add_vertex;
begin
 if is_move_to(cmd ) then
  begin
   render   (false );
   move_to_d(x ,y );
   
  end
 else
  if is_end_poly(cmd ) then
   begin
    render(is_closed(cmd ) );

    if is_closed(cmd ) then
     move_to(m_start_x ,m_start_y );

   end
  else
   line_to_d(x ,y );

end;

{ ADD_PATH }
procedure rasterizer_outline_aa.add_path;
var
 x ,y : double;
 cmd  : unsigned;

begin
 vs.rewind(path_id );

 cmd:=vs.vertex(@x ,@y );

 while not is_stop(cmd ) do
  begin
   add_vertex(x ,y ,cmd );

   cmd:=vs.vertex(@x ,@y );

  end;

 render(false );

end;

{ RENDER_ALL_PATHS }
procedure rasterizer_outline_aa.render_all_paths;
var
 i : unsigned;

begin
 for i:=0 to num_paths - 1 do
  begin
   m_ren.color_(aggclr_ptr(ptrcomp(colors ) + i * sizeof(aggclr ) ) );
   add_path    (vs ,unsigned_ptr(ptrcomp(path_id ) + i * sizeof(unsigned ) )^ );

  end;

end;

{ RENDER_CTRL }
procedure rasterizer_outline_aa.render_ctrl;
var
 i : unsigned;

begin
 for i:=0 to c.num_paths - 1 do
  begin
   m_ren.color_(c._color(i ) );
   add_path    (c ,i );

  end;

end;

END.

