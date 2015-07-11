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
// Stroke math
//
// [Pascal Port History] -----------------------------------------------------
//
// 18.10.2007-Milano: math_stroke
// 10.01.2006-Milano: stroke_calc_join ,stroke_calc_miter ,stroke_calc_arc
// 09.01.2006-Milano: stroke_calc_cap
// 19.12.2005-Milano: Unit port establishment
//
{ agg_math_stroke.pas }
unit
 agg_math_stroke ;

INTERFACE

{$I agg_mode.inc }

uses
 Math ,
 agg_basics ,
 agg_math ,
 agg_vertex_sequence ,
 agg_array ;

{ TYPES DEFINITION }
const
 butt_cap   = 0;
 square_cap = 1;
 round_cap  = 2;

 miter_join         = 0;
 miter_join_revert  = 1;
 miter_join_round   = 4;
 round_join         = 2;
 bevel_join         = 3;

 inner_bevel = 0;
 inner_miter = 1;
 inner_jag   = 2;
 inner_round = 3;

// Minimal angle to calculate round joins, less than 0.1 degree.
 stroke_theta = 0.001;

type
 line_cap_e   = int;
 line_join_e  = int;
 inner_join_e = int;

//------------------------------------------------------------math_stroke
 math_stroke = object
   m_width     ,
   m_width_abs ,
   m_width_eps : double;

   m_width_sign : int;

   m_miter_limit       ,
   m_inner_miter_limit ,
   m_approx_scale      : double;

   m_line_cap   : line_cap_e;
   m_line_join  : line_join_e;
   m_inner_join : inner_join_e;

   constructor Construct;

   procedure line_cap_  (lc : line_cap_e );
   procedure line_join_ (lj : line_join_e );
   procedure inner_join_(ij : inner_join_e );

   function  _line_cap : line_cap_e;
   function  _line_join : line_join_e;
   function  _inner_join : inner_join_e;

   procedure width_              (w : double );
   procedure miter_limit_        (ml : double );
   procedure miter_limit_theta_  (t : double );
   procedure inner_miter_limit_  (ml : double );
   procedure approximation_scale_(as_ : double );

   function  _width : double;
   function  _miter_limit : double;
   function  _inner_miter_limit : double;
   function  _approximation_scale : double;

   procedure calc_cap(
              vc : pod_bvector_ptr;
              v0 ,v1 : vertex_dist_ptr;
              len : double );

   procedure calc_join(
              vc : pod_bvector_ptr;
              v0 ,v1 ,v2 : vertex_dist_ptr;
              len1 ,len2 : double );

   procedure add_vertex(vc : pod_bvector_ptr; x ,y : double );

   procedure calc_arc(
              vc : pod_bvector_ptr;
              x ,y ,dx1 ,dy1 ,dx2 ,dy2 : double );

   procedure calc_miter(
              vc : pod_bvector_ptr;
              v0 ,v1 ,v2 : vertex_dist_ptr;
              dx1 ,dy1 ,dx2 ,dy2 : double;
              lj : line_join_e;
              mlimit ,dbevel : double );

  end;

{ GLOBAL PROCEDURES }
 procedure stroke_calc_arc(
            out_vertices : pod_deque_ptr;
            x ,y ,dx1 ,dy1 ,dx2 ,dy2 ,width ,approximation_scale : double );

 procedure stroke_calc_miter(
            out_vertices : pod_deque_ptr;
            v0 ,v1 ,v2 : vertex_dist_ptr;
            dx1 ,dy1 ,dx2 ,dy2 ,width : double;
            line_join : unsigned;
            miter_limit ,approximation_scale : double );

 procedure stroke_calc_cap(
            out_vertices : pod_deque_ptr;
            v0 ,v1 : vertex_dist_ptr;
            len : double;
            line_cap : unsigned;
            width ,approximation_scale : double );

 procedure stroke_calc_join(
            out_vertices : pod_deque_ptr;
            v0 ,v1 , v2 : vertex_dist_ptr;
            len1 ,len2 ,width : double;
            line_join ,inner_join : unsigned;
            miter_limit ,inner_miter_limit ,approximation_scale : double );


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor math_stroke.Construct;
begin
 m_width     :=0.5;
 m_width_abs :=0.5;
 m_width_eps :=0.5 / 1024.0;
 m_width_sign:=1;

 m_miter_limit      :=4.0;
 m_inner_miter_limit:=1.01;
 m_approx_scale     :=1.0;

 m_line_cap  :=butt_cap;
 m_line_join :=miter_join;
 m_inner_join:=inner_miter;

end;

{ LINE_CAP_ }
procedure math_stroke.line_cap_(lc : line_cap_e );
begin
 m_line_cap:=lc;

end;

{ LINE_JOIN_ }
procedure math_stroke.line_join_(lj : line_join_e );
begin
 m_line_join:=lj;

end;

{ INNER_JOIN_ }
procedure math_stroke.inner_join_(ij : inner_join_e );
begin
 m_inner_join:=ij;

end;

{ _LINE_CAP }
function math_stroke._line_cap : line_cap_e;
begin
 result:=m_line_cap;

end;

{ _LINE_JOIN }
function math_stroke._line_join : line_join_e;
begin
 result:=m_line_join;

end;

{ _INNER_JOIN }
function math_stroke._inner_join : inner_join_e;
begin
 result:=m_inner_join;

end;

{ WIDTH_ }
procedure math_stroke.width_(w : double );
begin
 m_width:=w * 0.5;

 if m_width < 0 then
  begin
   m_width_abs :=-m_width;
   m_width_sign:=-1;

  end
 else
  begin
   m_width_abs :=m_width;
   m_width_sign:=1;

  end;

 m_width_eps:=m_width / 1024.0;

end;

{ MITER_LIMIT_ }
procedure math_stroke.miter_limit_(ml : double );
begin
 m_miter_limit:=ml;

end;

{ MITER_LIMIT_THETA_ }
procedure math_stroke.miter_limit_theta_(t : double );
begin
 m_miter_limit:=1.0 / Sin(t * 0.5 ) ;

end;

{ INNER_MITER_LIMIT_ }
procedure math_stroke.inner_miter_limit_(ml : double );
begin
 m_inner_miter_limit:=ml;

end;

{ APPROXIMATION_SCALE_ }
procedure math_stroke.approximation_scale_(as_ : double );
begin
 m_approx_scale:=as_;

end;

{ _WIDTH }
function math_stroke._width : double;
begin
 result:=m_width * 2.0;

end;

{ _MITER_LIMIT }
function math_stroke._miter_limit : double;
begin
 result:=m_miter_limit;

end;

{ _INNER_MITER_LIMIT }
function math_stroke._inner_miter_limit : double;
begin
 result:=m_inner_miter_limit;

end;

{ _APPROXIMATION_SCALE }
function math_stroke._approximation_scale : double;
begin
 result:=m_approx_scale;

end;

{ CALC_CAP }
procedure math_stroke.calc_cap(
           vc : pod_bvector_ptr;
           v0 ,v1 : vertex_dist_ptr;
           len : double );
var
 dx1 ,dy1 ,dx2 ,dy2 ,da ,a1 : double;

 i ,n : int;

begin
 vc.remove_all;

 dx1:=(v1.y - v0.y ) / len;
 dy1:=(v1.x - v0.x ) / len;
 dx2:=0;
 dy2:=0;

 dx1:=dx1 * m_width;
 dy1:=dy1 * m_width;

 if m_line_cap <> round_cap then
  begin
   if m_line_cap = square_cap then
    begin
     dx2:=dy1 * m_width_sign;
     dy2:=dx1 * m_width_sign;

    end;

   add_vertex(vc ,v0.x - dx1 - dx2 ,v0.y + dy1 - dy2 );
   add_vertex(vc ,v0.x + dx1 - dx2 ,v0.y - dy1 - dy2 );

  end
 else
  begin
   da:=ArcCos(m_width_abs / (m_width_abs + 0.125 / m_approx_scale ) ) * 2;
   n :=int(Trunc(pi / da ) );
   da:=pi / (n + 1 );

   add_vertex(vc ,v0.x - dx1 ,v0.y + dy1 );

   if m_width_sign > 0 then
    begin
     a1:=ArcTan2(dy1 ,-dx1 );
     a1:=a1 + da;
     i :=0;

     while i < n do
      begin
       add_vertex(
        vc ,
        v0.x + Cos(a1 ) * m_width ,
        v0.y + Sin(a1 ) * m_width );

       a1:=a1 + da;

       inc(i );

      end;

    end
   else
    begin
     a1:=ArcTan2(-dy1 ,dx1 );
     a1:=a1 - da;
     i :=0;

     while i < n do
      begin
       add_vertex(
        vc ,
        v0.x + Cos(a1 ) * m_width ,
        v0.y + Sin(a1 ) * m_width );

       a1:=a1 - da;

       inc(i );

      end;


    end;

   add_vertex(vc ,v0.x + dx1 ,v0.y - dy1 );

  end;

end;

{ CALC_JOIN }
procedure math_stroke.calc_join(
           vc : pod_bvector_ptr;
           v0 ,v1 ,v2 : vertex_dist_ptr;
           len1 ,len2 : double );
var
 dx1 ,dy1 ,dx2 ,dy2 ,cp ,limit ,dx ,dy ,dbevel : double;

begin
 dx1:=m_width * (v1.y - v0.y ) / len1;
 dy1:=m_width * (v1.x - v0.x ) / len1;
 dx2:=m_width * (v2.y - v1.y ) / len2;
 dy2:=m_width * (v2.x - v1.x ) / len2;

 vc.remove_all;

 cp:=cross_product(v0.x ,v0.y ,v1.x ,v1.y ,v2.x ,v2.y );

 if (cp <> 0 ) and
    ((cp > 0 ) = (m_width > 0 ) ) then
  begin
  // Inner join
   if len1 < len2 then
    limit:=len1 / m_width_abs
   else
    limit:=len2 / m_width_abs;

   if limit < m_inner_miter_limit then
    limit:=m_inner_miter_limit;

   case m_inner_join of
    inner_miter :
     calc_miter(vc ,v0 ,v1 ,v2 ,dx1 ,dy1 ,dx2 ,dy2 ,miter_join_revert ,limit ,0 );

    inner_jag ,inner_round :
     begin
      cp:=(dx1 - dx2 ) * (dx1 - dx2 ) + (dy1 - dy2 ) * (dy1 - dy2 );

      if (cp < len1 * len1 ) and
         (cp < len2 * len2 ) then
       calc_miter(vc ,v0 ,v1 ,v2 ,dx1 ,dy1 ,dx2 ,dy2 ,miter_join_revert ,limit ,0 )
      else
       if m_inner_join = inner_jag then
        begin
         add_vertex(vc ,v1.x + dx1 ,v1.y - dy1 );
         add_vertex(vc ,v1.x       ,v1.y       );
         add_vertex(vc ,v1.x + dx2 ,v1.y - dy2 );

        end
       else
        begin
         add_vertex(vc ,v1.x + dx1 ,v1.y - dy1 );
         add_vertex(vc ,v1.x       ,v1.y       );
         calc_arc  (vc ,v1.x ,v1.y ,dx2 ,-dy2 ,dx1 ,-dy1 );
         add_vertex(vc ,v1.x       ,v1.y       );
         add_vertex(vc ,v1.x + dx2 ,v1.y - dy2 );

        end;

     end;

    else
     begin
     // inner_bevel
      add_vertex(vc ,v1.x + dx1 ,v1.y - dy1 );
      add_vertex(vc ,v1.x + dx2 ,v1.y - dy2 );

     end;

   end;

  end
 else
  begin
  // Outer join
  //---------------
  // Calculate the distance between v1 and
  // the central point of the bevel line segment
   dx:=(dx1 + dx2 ) / 2;
   dy:=(dy1 + dy2 ) / 2;

   dbevel:=Sqrt(dx * dx + dy * dy );

   if (m_line_join = round_join ) or
      (m_line_join = bevel_join ) then
    begin
    // This is an optimization that reduces the number of points
    // in cases of almost collinear segments. If there's no
    // visible difference between bevel and miter joins we'd rather
    // use miter join because it adds only one point instead of two.
    //
    // Here we calculate the middle point between the bevel points
    // and then, the distance between v1 and this middle point.
    // At outer joins this distance always less than stroke width,
    // because it's actually the height of an isosceles triangle of
    // v1 and its two bevel points. If the difference between this
    // width and this value is small (no visible bevel) we can
    // add just one point.
    //
    // The constant in the expression makes the result approximately
    // the same as in round joins and caps. You can safely comment
    // out this entire "if".
     if m_approx_scale * (m_width_abs - dbevel ) < m_width_eps then
      begin
       if calc_intersection(
           v0.x + dx1 ,v0.y - dy1 ,
           v1.x + dx1 ,v1.y - dy1 ,
           v1.x + dx2 ,v1.y - dy2 ,
           v2.x + dx2 ,v2.y - dy2 ,
           @dx ,@dy ) then
        add_vertex(vc ,dx ,dy )
       else
        add_vertex(vc ,v1.x + dx1 ,v1.y - dy1 );

       exit;

      end;

    end;

   case m_line_join of
    miter_join ,miter_join_revert ,miter_join_round :
     calc_miter(
      vc ,v0 ,v1 ,v2 ,dx1 ,dy1 ,dx2 ,dy2 ,
      m_line_join ,m_miter_limit ,dbevel );

    round_join :
     calc_arc(vc ,v1.x ,v1.y ,dx1 ,-dy1 ,dx2 ,-dy2 );

    else
     begin
     // Bevel join
      add_vertex(vc ,v1.x + dx1 ,v1.y - dy1 );
      add_vertex(vc ,v1.x + dx2 ,v1.y - dy2 );

     end;

   end;

  end;

end;

{ ADD_VERTEX }
procedure math_stroke.add_vertex(vc : pod_bvector_ptr; x ,y : double );
var
 pt : point_type;

begin
 pt.x:=x;
 pt.y:=y;

 vc.add(@pt );

end;

{ CALC_ARC }
procedure math_stroke.calc_arc(
           vc : pod_bvector_ptr;
           x ,y ,dx1 ,dy1 ,dx2 ,dy2 : double );
var
 a1 ,a2 ,da : double;

 i ,n : int;

begin
 a1:=ArcTan2(dy1 * m_width_sign ,dx1 * m_width_sign );
 a2:=ArcTan2(dy2 * m_width_sign ,dx2 * m_width_sign );
 da:=a1 - a2;
 da:=ArcCos(m_width_abs / (m_width_abs + 0.125 / m_approx_scale ) ) * 2;

 add_vertex(vc ,x + dx1 ,y + dy1 );

 if m_width_sign > 0 then
  begin
   if a1 > a2 then
    a2:=a2 + 2 * pi;

   n :=int(Trunc((a2 - a1 ) / da ) );
   da:=(a2 - a1 ) / (n + 1 );
   a1:=a1 + da;
   i :=0;

   while i < n do
    begin
     add_vertex(vc ,x + Cos(a1 ) * m_width ,y + Sin(a1 ) * m_width );

     a1:=a1 + da;

     inc(i );

    end;

  end
 else
  begin
   if a1 < a2 then
    a2:=a2 - 2 * pi;

   n :=int(Trunc((a1 - a2 ) / da ) );
   da:=(a1 - a2 ) / (n + 1 );
   a1:=a1 - da;
   i :=0;

   while i < n do
    begin
     add_vertex(vc ,x + Cos(a1 ) * m_width ,y + Sin(a1 ) * m_width );

     a1:=a1 - da;

     inc(i );

    end;


  end;

 add_vertex(vc ,x + dx2 ,y + dy2 );

end;

{ CALC_MITER }
procedure math_stroke.calc_miter(
           vc : pod_bvector_ptr;
           v0 ,v1 ,v2 : vertex_dist_ptr;
           dx1 ,dy1 ,dx2 ,dy2 : double;
           lj : line_join_e;
           mlimit ,dbevel : double );
var
 xi ,yi ,di ,lim ,x2 ,y2 ,x1 ,y1 : double;

 miter_limit_exceeded ,intersection_failed : boolean;

begin
 xi :=v1.x;
 yi :=v1.y;
 di :=1;
 lim:=m_width_abs * mlimit;

 miter_limit_exceeded:=true; // Assume the worst
 intersection_failed :=true; // Assume the worst

 if calc_intersection(
     v0.x + dx1 ,v0.y - dy1 ,
     v1.x + dx1 ,v1.y - dy1 ,
     v1.x + dx2 ,v1.y - dy2 ,
     v2.x + dx2 ,v2.y - dy2 ,
     @xi ,@yi ) then
  begin
  // Calculation of the intersection succeeded
   di:=calc_distance(v1.x ,v1.y ,xi ,yi );

   if di <= lim then
    begin
    // Inside the miter limit
     add_vertex(vc, xi, yi);

     miter_limit_exceeded:=false;

    end;

   intersection_failed:=false;

  end
 else
  begin
  // Calculation of the intersection failed, most probably
  // the three points lie one straight line.
  // First check if v0 and v2 lie on the opposite sides of vector:
  // (v1.x, v1.y) -> (v1.x+dx1, v1.y-dy1), that is, the perpendicular
  // to the line determined by vertices v0 and v1.
  // This condition determines whether the next line segments continues
  // the previous one or goes back.
   x2:=v1.x + dx1;
   y2:=v1.y - dy1;

   if (cross_product(v0.x ,v0.y ,v1.x ,v1.y ,x2 ,y2 ) < 0.0 ) =
      (cross_product(v1.x ,v1.y ,v2.x ,v2.y ,x2 ,y2 ) < 0.0 ) then
    begin
    // This case means that the next segment continues 
    // the previous one (straight line)
     add_vertex(vc ,v1.x + dx1 ,v1.y - dy1 );

     miter_limit_exceeded:=false;

    end;

  end;

// Miter limit exceeded
 if miter_limit_exceeded then
  case lj of
   miter_join_revert :
    begin
    // For the compatibility with SVG, PDF, etc,
    // we use a simple bevel join instead of
    // "smart" bevel
     add_vertex(vc ,v1.x + dx1 ,v1.y - dy1 );
     add_vertex(vc ,v1.x + dx2 ,v1.y - dy2 );

    end;

   miter_join_round :
    calc_arc(vc ,v1.x ,v1.y ,dx1 ,-dy1 ,dx2 ,-dy2 );

  // If no miter-revert, calculate new dx1, dy1, dx2, dy2
   else
    if intersection_failed then
     begin
      mlimit:=mlimit * m_width_sign;

      add_vertex(
       vc ,
       v1.x + dx1 + dy1 * mlimit ,
       v1.y - dy1 + dx1 * mlimit );

      add_vertex(
       vc ,
       v1.x + dx2 - dy2 * mlimit ,
       v1.y - dy2 - dx2 * mlimit );

     end
    else
     begin
      x1:=v1.x + dx1;
      y1:=v1.y - dy1;
      x2:=v1.x + dx2;
      y2:=v1.y - dy2;
      di:=(lim - dbevel ) / (di - dbevel );

      add_vertex(
       vc ,
       x1 + (xi - x1 ) * di ,
       y1 + (yi - y1 ) * di );

      add_vertex(
       vc ,
       x2 + (xi - x2 ) * di ,
       y2 + (yi - y2 ) * di );

     end;

  end;

end;

{ STROKE_CALC_ARC }
procedure stroke_calc_arc;
var
 pt : point_type;

 a1 ,a2 ,da : double;

 ccw : boolean;

begin
 a1:=ArcTan2(dy1 ,dx1 );
 a2:=ArcTan2(dy2 ,dx2 );
 da:=a1 - a2;

// Possible optimization. Not important at all; consumes time but happens rarely
// if Abs(da ) < stroke_theta then
//  begin
//   pt.x:=(x + x + dx1 + dx2 ) * 0.5;
//   pt.y:=(y + y + dy1 + dy2 ) * 0.5;
//
//   out_vertices.add(@pt );
//   exit;
//
//  end;
 ccw:=(da > 0.0 ) and (da < pi );

 if width < 0 then
  width:=-width;

 if approximation_scale = 0 then
  approximation_scale:=0.00001;

 da:=ArcCos(width / (width + 0.125 / approximation_scale ) ) * 2;

 pt.x:=x + dx1;
 pt.y:=y + dy1;

 out_vertices.add(@pt );

 if not ccw then
  begin
   if a1 > a2 then
    a2:=a2 + (2 * pi );

   a2:=a2 - (da / 4 );
   a1:=a1 + da;

   while a1 < a2 do
    begin
     pt.x:=x + Cos(a1 ) * width;
     pt.y:=y + Sin(a1 ) * width;

     out_vertices.add(@pt );

     a1:=a1 + da;

    end;

  end
 else
  begin
   if a1 < a2 then
    a2:=a2 - (2 * pi );

   a2:=a2 + (da / 4 );
   a1:=a1 - da;

   while a1 > a2 do
    begin
     pt.x:=x + Cos(a1 ) * width;
     pt.y:=y + Sin(a1 ) * width;

     out_vertices.add(@pt );

     a1:=a1 - da;

    end;

  end;

 pt.x:=x + dx2;
 pt.y:=y + dy2;

 out_vertices.add(@pt );

end;

{ STROKE_CALC_MITER }
procedure stroke_calc_miter;
var
 pt : point_type;

 xi ,yi ,d1 ,lim ,x2 ,y2 : double;

 miter_limit_exceeded : boolean;

begin
 xi:=v1.x;
 yi:=v1.y;

 miter_limit_exceeded:=true; // Assume the worst

 if calc_intersection(
     v0.x + dx1 ,v0.y - dy1 ,
     v1.x + dx1 ,v1.y - dy1 ,
     v1.x + dx2 ,v1.y - dy2 ,
     v2.x + dx2 ,v2.y - dy2 ,
     @xi ,@yi ) then
  begin
  // Calculation of the intersection succeeded
  //---------------------
   d1 :=calc_distance(v1.x ,v1.y ,xi ,yi );
   lim:=width * miter_limit;

   if d1 <= lim then
    begin
    // Inside the miter limit
    //---------------------
     pt.x:=xi;
     pt.y:=yi;

     out_vertices.add(@pt );

     miter_limit_exceeded:=false;

    end;

  end
 else
  begin
  // Calculation of the intersection failed, most probably
  // the three points lie one straight line.
  // First check if v0 and v2 lie on the opposite sides of vector:
  // (v1.x, v1.y) -> (v1.x+dx1, v1.y-dy1), that is, the perpendicular
  // to the line determined by vertices v0 and v1.
  // This condition determines whether the next line segments continues
  // the previous one or goes back.
  //----------------
   x2:=v1.x + dx1;
   y2:=v1.y - dy1;

   if (((x2 - v0.x ) * dy1 - (v0.y - y2 ) * dx1 < 0.0 ) <>
       ((x2 - v2.x ) * dy1 - (v2.y - y2 ) * dx1 < 0.0 ) ) then
    begin
    // This case means that the next segment continues
    // the previous one (straight line)
    //-----------------
     pt.x:=v1.x + dx1;
     pt.y:=v1.y - dy1;

     out_vertices.add(@pt );

     miter_limit_exceeded:=false;

    end;

  end;

 if miter_limit_exceeded then
 // Miter limit exceeded
 //------------------------
  case line_join of
   miter_join_revert :
    begin
    // For the compatibility with SVG, PDF, etc,
    // we use a simple bevel join instead of
    // "smart" bevel
    //-------------------
     pt.x:=v1.x + dx1;
     pt.y:=v1.y - dy1;

     out_vertices.add(@pt );

     pt.x:=v1.x + dx2;
     pt.y:=v1.y - dy2;

     out_vertices.add(@pt );

    end;

   miter_join_round :
    stroke_calc_arc(
     out_vertices ,
     v1.x ,v1.y ,dx1 ,-dy1 ,dx2 ,-dy2 ,
     width ,approximation_scale );

   else
    begin
    // If no miter-revert, calculate new dx1, dy1, dx2, dy2
    //----------------
     pt.x:=v1.x + dx1 + dy1 * miter_limit;
     pt.y:=v1.y - dy1 + dx1 * miter_limit;

     out_vertices.add(@pt );

     pt.x:=v1.x + dx2 - dy2 * miter_limit;
     pt.y:=v1.y - dy2 - dx2 * miter_limit;

     out_vertices.add(@pt );

    end;

  end;

end;

{ STROKE_CALC_CAP }
procedure stroke_calc_cap;
var
 pt : point_type;

 dx1 ,dy1 ,
 dx2 ,dy2 ,

 a1 ,a2 ,da : double;

begin
 out_vertices.remove_all;

 dx1:=(v1.y - v0.y ) / len;
 dy1:=(v1.x - v0.x ) / len;
 dx2:=0;
 dy2:=0;

 dx1:=dx1 * width;
 dy1:=dy1 * width;

 if line_cap <> round_cap then
  begin
   if line_cap = square_cap then
    begin
     dx2:=dy1;
     dy2:=dx1;

    end;

   pt.x:=v0.x - dx1 - dx2;
   pt.y:=v0.y + dy1 - dy2;

   out_vertices.add(@pt );

   pt.x:=v0.x + dx1 - dx2;
   pt.y:=v0.y - dy1 - dy2;

   out_vertices.add(@pt );

  end
 else
  begin
   a1:=ArcTan2(dy1 ,-dx1 );
   a2:=a1 + pi;

   if approximation_scale = 0 then
    approximation_scale:=0.00001;

   da:=ArcCos(width / (width + 0.125 / approximation_scale ) ) * 2;

   pt.x:=v0.x - dx1;
   pt.y:=v0.y + dy1;

   out_vertices.add(@pt );

   a1:=a1 + da;
   a2:=a2 - (da / 4 );

   while a1 < a2 do
    begin
     pt.x:=v0.x + Cos(a1 ) * width;
     pt.y:=v0.y + Sin(a1 ) * width;

     out_vertices.add(@pt );

     a1:=a1 + da;

    end;

   pt.x:=v0.x + dx1;
   pt.y:=v0.y - dy1;

   out_vertices.add(@pt );

  end;

end;

{ STROKE_CALC_JOIN }
procedure stroke_calc_join;
var
 pt : point_type;

 d ,dx1 ,dy1 ,dx2 ,dy2 : double;

begin
 dx1:=width * (v1.y - v0.y ) / len1;
 dy1:=width * (v1.x - v0.x ) / len1;

 dx2:=width * (v2.y - v1.y ) / len2;
 dy2:=width * (v2.x - v1.x ) / len2;

 out_vertices.remove_all;

 if calc_point_location(v0.x ,v0.y ,v1.x ,v1.y ,v2.x ,v2.y ) > 0 then
 // Inner join
 //---------------
  case inner_join of
   inner_miter :
    stroke_calc_miter(
     out_vertices ,
     v0 ,v1 ,v2 ,dx1 ,dy1 ,dx2 ,dy2 ,
     width ,
     miter_join_revert ,
     inner_miter_limit ,
     1.0 );

   inner_jag ,inner_round :
    begin
     d:=(dx1 - dx2 ) * (dx1 - dx2 ) + (dy1 - dy2 ) * (dy1 - dy2 );

     if (d < len1 * len1 ) and
        (d < len2 * len2 ) then
      stroke_calc_miter(
       out_vertices ,
       v0 ,v1 ,v2 ,dx1 ,dy1 ,dx2 ,dy2 ,
       width ,
       miter_join_revert ,
       inner_miter_limit ,
       1.0 )
       
     else
      if inner_join = inner_jag then
       begin
        pt.x:=v1.x + dx1;
        pt.y:=v1.y - dy1;

        out_vertices.add(@pt );

        pt.x:=v1.x;
        pt.y:=v1.y;

        out_vertices.add(@pt );

        pt.x:=v1.x + dx2;
        pt.y:=v1.y - dy2;

        out_vertices.add(@pt );

       end
      else
       begin
        pt.x:=v1.x + dx1;
        pt.y:=v1.y - dy1;

        out_vertices.add(@pt );

        pt.x:=v1.x;
        pt.y:=v1.y;

        out_vertices.add(@pt );

        stroke_calc_arc(
         out_vertices ,
         v1.x ,v1.y ,dx2 ,-dy2 ,dx1 ,-dy1 ,
         width ,approximation_scale );

        pt.x:=v1.x;
        pt.y:=v1.y;

        out_vertices.add(@pt );

        pt.x:=v1.x + dx2;
        pt.y:=v1.y - dy2;

        out_vertices.add(@pt );

       end;

    end;

   else // inner_bevel
    begin
     pt.x:=v1.x + dx1;
     pt.y:=v1.y - dy1;

     out_vertices.add(@pt );

     pt.x:=v1.x + dx2;
     pt.y:=v1.y - dy2;

     out_vertices.add(@pt );

    end;

  end
 else
 // Outer join
 //---------------
  case line_join of
   miter_join ,miter_join_revert ,miter_join_round :
    stroke_calc_miter(
     out_vertices ,
     v0 ,v1 ,v2 ,dx1 ,dy1 ,dx2 ,dy2 ,
     width ,
     line_join ,
     miter_limit ,
     approximation_scale );

   round_join :
    stroke_calc_arc(
     out_vertices ,
     v1.x ,v1.y ,dx1 ,-dy1 ,dx2 ,-dy2 ,
     width ,approximation_scale );

   else // Bevel join
    begin
     pt.x:=v1.x + dx1;
     pt.y:=v1.y - dy1;

     out_vertices.add(@pt );

     pt.x:=v1.x + dx2;
     pt.y:=v1.y - dy2;

     out_vertices.add(@pt );

    end;

  end;

end;

END.

