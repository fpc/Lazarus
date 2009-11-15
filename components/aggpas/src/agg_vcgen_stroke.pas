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
//----------------------------------------------------------------------------
//
// Stroke generator
//
// [Pascal Port History] -----------------------------------------------------
//
// 18.10.2007-Milano: vcgen_stroke_math
// 21.12.2005-Milano: Unit port establishment
//
{ agg_vcgen_stroke.pas }
unit
 agg_vcgen_stroke ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_array ,
 agg_vertex_source ,
 agg_vertex_sequence ,
 agg_math_stroke ,
 agg_shorten_path ;

{ TYPES DEFINITION }
type
 status_e = (
  initial ,
  ready ,
  cap1 ,
  cap2 ,
  outline1 ,
  close_first ,
  outline2 ,
  out_vertices ,
  end_poly1 ,
  end_poly2 ,
  stop );

 vcgen_stroke_ptr = ^vcgen_stroke;
 vcgen_stroke = object(vertex_source )
   m_src_vertices      : vertex_sequence;
   m_out_vertices      : pod_deque;
   m_width             ,
   m_miter_limit       ,
   m_inner_miter_limit ,
   m_approx_scale      ,
   m_shorten           : double;
   m_line_cap          ,
   m_line_join         ,
   m_inner_join        ,
   m_closed            : unsigned;
   m_status            ,
   m_prev_status       : status_e;
   m_src_vertex        ,
   m_out_vertex        : unsigned;

   constructor Construct;
   destructor  Destruct; virtual;

   procedure line_cap_  (lc : unsigned );
   procedure line_join_ (lj : unsigned );
   procedure inner_join_(ij : unsigned );

   function  _line_cap : unsigned;
   function  _line_join : unsigned;
   function  _inner_join : unsigned;

   procedure width_              (w : double );
   procedure miter_limit_        (ml : double );
   procedure miter_limit_theta_  (t : double );
   procedure inner_miter_limit_  (ml : double );
   procedure approximation_scale_(as_ : double );

   function  _width : double;
   function  _miter_limit : double;
   function  _inner_miter_limit : double;
   function  _approximation_scale : double;

   procedure shorten_(s : double );
   function  _shorten : double;

  // Vertex Generator Interface
   procedure remove_all; virtual;
   procedure add_vertex(x ,y : double; cmd : unsigned ); virtual;

  // Vertex Source Interface
   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 vcgen_stroke_math_ptr = ^vcgen_stroke_math;
 vcgen_stroke_math = object(vertex_source )
   m_stroker      : math_stroke;
   m_src_vertices : vertex_sequence;
   m_out_vertices : pod_deque;

   m_shorten : double;
   m_closed  : unsigned;

   m_status      ,
   m_prev_status : status_e;

   m_src_vertex ,
   m_out_vertex : unsigned;

   constructor Construct;
   destructor  Destruct; virtual;

   procedure line_cap_  (lc : unsigned );
   procedure line_join_ (lj : unsigned );
   procedure inner_join_(ij : unsigned );

   function  _line_cap : unsigned;
   function  _line_join : unsigned;
   function  _inner_join : unsigned;

   procedure width_              (w : double );
   procedure miter_limit_        (ml : double );
   procedure miter_limit_theta_  (t : double );
   procedure inner_miter_limit_  (ml : double );
   procedure approximation_scale_(as_ : double );

   function  _width : double;
   function  _miter_limit : double;
   function  _inner_miter_limit : double;
   function  _approximation_scale : double;

   procedure shorten_(s : double );
   function  _shorten : double;

  // Vertex Generator Interface
   procedure remove_all; virtual;
   procedure add_vertex(x ,y : double; cmd : unsigned ); virtual;

  // Vertex Source Interface
   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor vcgen_stroke.Construct;
begin
 m_src_vertices.Construct(sizeof(vertex_dist ) );
 m_out_vertices.Construct(sizeof(point_type ) );

 m_width            :=0.5;
 m_miter_limit      :=4.0;
 m_inner_miter_limit:=1.01;
 m_approx_scale     :=1.0;
 m_shorten          :=0.0;
 m_line_cap         :=butt_cap;
 m_line_join        :=miter_join;
 m_inner_join       :=inner_miter;
 m_closed           :=0;
 m_status           :=initial;
 m_src_vertex       :=0;
 m_out_vertex       :=0;

end;

{ DESTRUCT }
destructor vcgen_stroke.Destruct;
begin
 m_src_vertices.Destruct;
 m_out_vertices.Destruct;

end;

{ LINE_CAP_ }
procedure vcgen_stroke.line_cap_;
begin
 m_line_cap:=lc;

end;

{ LINE_JOIN_ }
procedure vcgen_stroke.line_join_;
begin
 m_line_join:=lj;

end;

{ INNER_JOIN_ }
procedure vcgen_stroke.inner_join_;
begin
 m_inner_join:=ij;

end;

{ _LINE_CAP }
function vcgen_stroke._line_cap;
begin
 result:=m_line_cap;

end;

{ _LINE_JOIN }
function vcgen_stroke._line_join;
begin
 result:=m_line_join;

end;

{ _INNER_JOIN }
function vcgen_stroke._inner_join;
begin
 result:=m_inner_join;

end;

{ WIDHT_ }
procedure vcgen_stroke.width_;
begin
 m_width:=w * 0.5;

end;

{ MITER_LIMIT_ }
procedure vcgen_stroke.miter_limit_;
begin
 m_miter_limit:=ml;

end;

{ MITER_LIMIT_THETA_ }
procedure vcgen_stroke.miter_limit_theta_;
begin
 m_miter_limit:=1.0 / Sin(t * 0.5 );

end;

{ INNER_MITER_LIMIT_ }
procedure vcgen_stroke.inner_miter_limit_;
begin
 m_inner_miter_limit:=ml;

end;

{ APPROXIMATION_SCALE_ }
procedure vcgen_stroke.approximation_scale_;
begin
 m_approx_scale:=as_;

end;

{ _WIDTH }
function vcgen_stroke._width;
begin
 result:=m_width * 2.0;

end;

{ _MITER_LIMIT }
function vcgen_stroke._miter_limit;
begin
 result:=m_miter_limit;

end;

{ _INNER_MITER_LIMIT }
function vcgen_stroke._inner_miter_limit;
begin
 result:=m_inner_miter_limit;

end;

{ _APPROXIMATION_SCALE }
function vcgen_stroke._approximation_scale;
begin
 result:=m_approx_scale;

end;

{ SHORTEN_ }
procedure vcgen_stroke.shorten_;
begin
 m_shorten:=s;

end;

{ _SHORTEN }
function vcgen_stroke._shorten;
begin
 result:=m_shorten;

end;

{ REMOVE_ALL }
procedure vcgen_stroke.remove_all;
begin
 m_src_vertices.remove_all;

 m_closed:=0;
 m_status:=initial;

end;

{ ADD_VERTEX }
procedure vcgen_stroke.add_vertex;
var
 vd : vertex_dist;

begin
 m_status:=initial;

 vd.x:=x;
 vd.y:=y;

 vd.dist:=0;

 if is_move_to(cmd ) then
  m_src_vertices.modify_last(@vd )
 else
  if is_vertex(cmd ) then
   m_src_vertices.add(@vd )
  else
   m_closed:=get_close_flag(cmd );

end;

{ calc_butt_cap }
procedure calc_butt_cap(cap : double_00_ptr; v0 ,v1 : vertex_dist_ptr; len ,width : double );
var
 dx ,dy : double;

begin
 dx:=(v1.y - v0.y ) * width / len;
 dy:=(v1.x - v0.x ) * width / len;

 cap^[0 ]:=v0.x - dx;
 cap^[1 ]:=v0.y + dy;
 cap^[2 ]:=v0.x + dx;
 cap^[3 ]:=v0.y - dy;

end;

{ REWIND }
procedure vcgen_stroke.rewind;
begin
 if m_status = initial then
  begin
   m_src_vertices.close(boolean(m_closed <> 0 ) );

   shorten_path(@m_src_vertices ,m_shorten ,m_closed );

   if m_src_vertices.size < 3 then
    m_closed:=0;

  end;

 m_status:=ready;

 m_src_vertex:=0;
 m_out_vertex:=0;

end;

{ VERTEX }
function vcgen_stroke.vertex;
var
 c : point_type_ptr;

 cmd : unsigned;

label
 _rdy ,_out2 ,_end ;

begin
 cmd:=path_cmd_line_to;

 while not is_stop(cmd ) do
  begin
   case m_status of
    initial :
     begin
      rewind(0 );

      goto _rdy;

     end;

    ready :
     begin
     _rdy:
      if m_src_vertices.size < 2 + unsigned(m_closed <> 0 ) then
       begin
        cmd:=path_cmd_stop;

        goto _end;

       end;

      if (m_closed <> 0 ) then
       m_status:=outline1
      else
       m_status:=cap1;

      cmd:=path_cmd_move_to;

      m_src_vertex:=0;
      m_out_vertex:=0;

     end;

    cap1 :
     begin
      stroke_calc_cap(
       @m_out_vertices ,
       m_src_vertices.array_operator(0 ) ,
       m_src_vertices.array_operator(1 ) ,
       vertex_dist_ptr(m_src_vertices.array_operator(0 ) )^.dist ,
       m_line_cap ,
       m_width ,
       m_approx_scale );

      m_src_vertex :=1;
      m_prev_status:=outline1;
      m_status     :=out_vertices;
      m_out_vertex :=0;

     end;

    cap2 :
     begin
      stroke_calc_cap(
       @m_out_vertices ,
       m_src_vertices.array_operator(m_src_vertices.size - 1 ) ,
       m_src_vertices.array_operator(m_src_vertices.size - 2 ) ,
       vertex_dist_ptr(m_src_vertices.array_operator(m_src_vertices.size - 2 ) )^.dist ,
       m_line_cap ,
       m_width ,
       m_approx_scale );

      m_prev_status:=outline2;
      m_status     :=out_vertices;
      m_out_vertex :=0;

     end;

    outline1 :
     begin
      if m_closed <> 0 then
       if m_src_vertex >= m_src_vertices.size then
        begin
         m_prev_status:=close_first;
         m_status     :=end_poly1;

         goto _end;

        end
       else
      else
       if m_src_vertex >= m_src_vertices.size - 1 then
        begin
         m_status:=cap2;

         goto _end;

        end;

      stroke_calc_join(
       @m_out_vertices ,
       m_src_vertices.prev(m_src_vertex ) ,
       m_src_vertices.curr(m_src_vertex ) ,
       m_src_vertices.next(m_src_vertex ) ,
       vertex_dist_ptr(m_src_vertices.prev(m_src_vertex ) )^.dist ,
       vertex_dist_ptr(m_src_vertices.curr(m_src_vertex ) )^.dist ,
       m_width ,
       m_line_join ,
       m_inner_join ,
       m_miter_limit ,
       m_inner_miter_limit ,
       m_approx_scale );

      inc(m_src_vertex );

      m_prev_status:=m_status;
      m_status     :=out_vertices;
      m_out_vertex :=0;

     end;

    close_first :
     begin
      m_status:=outline2;

      cmd:=path_cmd_move_to;

      goto _out2;

     end;

    outline2 :
     begin
     _out2:
      if m_src_vertex <= unsigned(m_closed = 0 ) then
       begin
        m_status     :=end_poly2;
        m_prev_status:=stop;

        goto _end;

       end;

      dec(m_src_vertex );

      stroke_calc_join(
       @m_out_vertices ,
       m_src_vertices.next(m_src_vertex ) ,
       m_src_vertices.curr(m_src_vertex ) ,
       m_src_vertices.prev(m_src_vertex ) ,
       vertex_dist_ptr(m_src_vertices.curr(m_src_vertex ) )^.dist ,
       vertex_dist_ptr(m_src_vertices.prev(m_src_vertex ) )^.dist ,
       m_width ,
       m_line_join  ,
       m_inner_join ,
       m_miter_limit ,
       m_inner_miter_limit ,
       m_approx_scale );

      m_prev_status:=m_status;
      m_status     :=out_vertices;
      m_out_vertex :=0;

     end;

    out_vertices :
     if m_out_vertex >= m_out_vertices.size then
      m_status:=m_prev_status

     else
      begin
       c:=m_out_vertices.array_operator(m_out_vertex );

       inc(m_out_vertex );

       x^:=c.x;
       y^:=c.y;

       result:=cmd;

       exit;

      end;

    end_poly1 :
     begin
      m_status:=m_prev_status;

      result:=path_cmd_end_poly or path_flags_close or path_flags_ccw;

      exit;

     end;

    end_poly2 :
     begin
      m_status:=m_prev_status;

      result:=path_cmd_end_poly or path_flags_close or path_flags_cw;

      exit;

     end;

    stop :
     cmd:=path_cmd_stop;

   end;

  _end:
  end;

 result:=cmd;

end;

{ CONSTRUCT }
constructor vcgen_stroke_math.Construct;
begin
 m_stroker.Construct;
 m_src_vertices.Construct(sizeof(vertex_dist ) );
 m_out_vertices.Construct(sizeof(point_type ) );

 m_shorten:=0.0;
 m_closed :=0;
 m_status :=initial;

 m_src_vertex:=0;
 m_out_vertex:=0;

end;

{ DESTRUCT }
destructor vcgen_stroke_math.Destruct;
begin
 m_src_vertices.Destruct;
 m_out_vertices.Destruct;

end;

{ LINE_CAP_ }
procedure vcgen_stroke_math.line_cap_(lc : unsigned );
begin
 m_stroker.line_cap_(lc );

end;

{ LINE_JOIN_ }
procedure vcgen_stroke_math.line_join_(lj : unsigned );
begin
 m_stroker.line_join_(lj );

end;

{ INNER_JOIN_ }
procedure vcgen_stroke_math.inner_join_(ij : unsigned );
begin
 m_stroker.inner_join_(ij );

end;

{ _LINE_CAP }
function vcgen_stroke_math._line_cap : unsigned;
begin
 result:=m_stroker._line_cap;

end;

{ _LINE_JOIN }
function vcgen_stroke_math._line_join : unsigned;
begin
 result:=m_stroker._line_join;

end;

{ _INNER_JOIN }
function vcgen_stroke_math._inner_join : unsigned;
begin
 result:=m_stroker._inner_join;

end;

{ WIDTH_ }
procedure vcgen_stroke_math.width_(w : double );
begin
 m_stroker.width_(w );

end;

{ MITER_LIMIT_ }
procedure vcgen_stroke_math.miter_limit_(ml : double );
begin
 m_stroker.miter_limit_(ml );

end;

{ MITER_LIMIT_THETA_ }
procedure vcgen_stroke_math.miter_limit_theta_(t : double );
begin
 m_stroker.miter_limit_theta_(t );

end;

{ INNER_MITER_LIMIT_ }
procedure vcgen_stroke_math.inner_miter_limit_(ml : double );
begin
 m_stroker.inner_miter_limit_(ml );

end;

{ APPROXIMATION_SCALE_ }
procedure vcgen_stroke_math.approximation_scale_(as_ : double );
begin
 m_stroker.approximation_scale_(as_ );

end;

{ _WIDTH }
function vcgen_stroke_math._width : double;
begin
 result:=m_stroker._width;

end;

{ _MITER_LIMIT }
function vcgen_stroke_math._miter_limit : double;
begin
 result:=m_stroker._miter_limit;

end;

{ _INNER_MITER_LIMIT }
function vcgen_stroke_math._inner_miter_limit : double;
begin
 result:=m_stroker._inner_miter_limit;

end;

{ _APPROXIMATION_SCALE }
function vcgen_stroke_math._approximation_scale : double;
begin
 result:=m_stroker._approximation_scale;

end;

{ SHORTEN_ }
procedure vcgen_stroke_math.shorten_(s : double );
begin
 m_shorten:=s;

end;

{ _SHORTEN }
function vcgen_stroke_math._shorten : double;
begin
 result:=m_shorten;

end;

{ REMOVE_ALL }
procedure vcgen_stroke_math.remove_all;
begin
 m_src_vertices.remove_all;

 m_closed:=0;
 m_status:=initial;

end;

{ ADD_VERTEX }
procedure vcgen_stroke_math.add_vertex(x ,y : double; cmd : unsigned );
var
 vd : vertex_dist;

begin
 m_status:=initial;

 vd.x:=x;
 vd.y:=y;

 vd.dist:=0;

 if is_move_to(cmd ) then
  m_src_vertices.modify_last(@vd )
 else
  if is_vertex(cmd ) then
   m_src_vertices.add(@vd )
  else
   m_closed:=get_close_flag(cmd );

end;

{ REWIND }
procedure vcgen_stroke_math.rewind(path_id : unsigned );
begin
 if m_status = initial then
  begin
   m_src_vertices.close(boolean(m_closed <> 0 ) );

   shorten_path(@m_src_vertices ,m_shorten ,m_closed );

   if m_src_vertices.size < 3 then
    m_closed:=0;

  end;

 m_status:=ready;

 m_src_vertex:=0;
 m_out_vertex:=0;

end;

{ VERTEX }
function vcgen_stroke_math.vertex(x ,y : double_ptr ) : unsigned;
var
 cmd : unsigned;

 c : point_type_ptr;

label
 _rdy ,_out2 ,_end ;

begin
 cmd:=path_cmd_line_to;

 while not is_stop(cmd ) do
  begin
   case m_status of
    initial :
     begin
      rewind(0 );

      goto _rdy;

     end;

    ready :
     begin
     _rdy:
      if m_src_vertices.size < 2 + unsigned(m_closed <> 0 ) then
       begin
        cmd:=path_cmd_stop;

        goto _end;

       end;

      if (m_closed <> 0 ) then
       m_status:=outline1
      else
       m_status:=cap1;

      cmd:=path_cmd_move_to;

      m_src_vertex:=0;
      m_out_vertex:=0;

     end;

    cap1 :
     begin
      m_stroker.calc_cap(
       @m_out_vertices ,
       m_src_vertices.array_operator(0 ) ,
       m_src_vertices.array_operator(1 ) ,
       vertex_dist_ptr(m_src_vertices.array_operator(0 ) )^.dist );

      m_src_vertex :=1;
      m_prev_status:=outline1;
      m_status     :=out_vertices;
      m_out_vertex :=0;

     end;

    cap2 :
     begin
      m_stroker.calc_cap(
       @m_out_vertices ,
       m_src_vertices.array_operator(m_src_vertices.size - 1 ) ,
       m_src_vertices.array_operator(m_src_vertices.size - 2 ) ,
       vertex_dist_ptr(m_src_vertices.array_operator(m_src_vertices.size - 2 ) )^.dist );

      m_prev_status:=outline2;
      m_status     :=out_vertices;
      m_out_vertex :=0;

     end;

    outline1 :
     begin
      if m_closed <> 0 then
       if m_src_vertex >= m_src_vertices.size then
        begin
         m_prev_status:=close_first;
         m_status     :=end_poly1;

         goto _end;

        end
       else
      else
       if m_src_vertex >= m_src_vertices.size - 1 then
        begin
         m_status:=cap2;

         goto _end;

        end;

      m_stroker.calc_join(
       @m_out_vertices ,
       m_src_vertices.prev(m_src_vertex ) ,
       m_src_vertices.curr(m_src_vertex ) ,
       m_src_vertices.next(m_src_vertex ) ,
       vertex_dist_ptr(m_src_vertices.prev(m_src_vertex ) )^.dist ,
       vertex_dist_ptr(m_src_vertices.curr(m_src_vertex ) )^.dist );

      inc(m_src_vertex );

      m_prev_status:=m_status;
      m_status     :=out_vertices;
      m_out_vertex :=0;

     end;

    close_first :
     begin
      m_status:=outline2;

      cmd:=path_cmd_move_to;

      goto _out2;

     end;

    outline2 :
     begin
     _out2:
      if m_src_vertex <= unsigned(m_closed = 0 ) then
       begin
        m_status     :=end_poly2;
        m_prev_status:=stop;

        goto _end;

       end;

      dec(m_src_vertex );

      m_stroker.calc_join(
       @m_out_vertices ,
       m_src_vertices.next(m_src_vertex ) ,
       m_src_vertices.curr(m_src_vertex ) ,
       m_src_vertices.prev(m_src_vertex ) ,
       vertex_dist_ptr(m_src_vertices.curr(m_src_vertex ) )^.dist ,
       vertex_dist_ptr(m_src_vertices.prev(m_src_vertex ) )^.dist );

      m_prev_status:=m_status;
      m_status     :=out_vertices;
      m_out_vertex :=0;

     end;

    out_vertices :
     if m_out_vertex >= m_out_vertices.size then
      m_status:=m_prev_status

     else
      begin
       c:=m_out_vertices.array_operator(m_out_vertex );

       inc(m_out_vertex );

       x^:=c.x;
       y^:=c.y;

       result:=cmd;

       exit;

      end;

    end_poly1 :
     begin
      m_status:=m_prev_status;

      result:=path_cmd_end_poly or path_flags_close or path_flags_ccw;

      exit;

     end;

    end_poly2 :
     begin
      m_status:=m_prev_status;

      result:=path_cmd_end_poly or path_flags_close or path_flags_cw;

      exit;

     end;

    stop :
     cmd:=path_cmd_stop;

   end;

  _end:
  end;

 result:=cmd;

end;

END.

