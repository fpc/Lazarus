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
// 19.01.2006-Milano: Complete Unit Port
// 18.01.2006-Milano: Unit port establishment
//
{ agg_vcgen_bspline.pas }
unit
 agg_vcgen_bspline ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_bspline ,
 agg_array ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 status_e = (initial ,ready ,polygon ,end_poly ,stop );

 vcgen_bspline_ptr = ^vcgen_bspline;
 vcgen_bspline = object(vertex_source )
   m_src_vertices : pod_deque;

   m_spline_x ,
   m_spline_y : bspline;

   m_interpolation_step : double;

   m_closed : unsigned;
   m_status : status_e;

   m_src_vertex : unsigned;

   m_cur_abscissa ,
   m_max_abscissa : double;

   constructor Construct;
   destructor  Destruct; virtual;

   procedure interpolation_step_(v : double );
   function  _interpolation_step : double;

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
constructor vcgen_bspline.Construct;
begin
 inherited Construct;

 m_src_vertices.Construct(sizeof(point_type ) ,6 );
 m_spline_x.Construct;
 m_spline_y.Construct;
 
 m_interpolation_step:=1.0 / 50.0;
 
 m_closed:=0;
 m_status:=initial;

 m_src_vertex:=0;

end;

{ DESTRUCT }
destructor vcgen_bspline.Destruct;
begin
 inherited Destruct;

 m_src_vertices.Destruct;
 m_spline_x.Destruct;
 m_spline_y.Destruct;

end;

{ INTERPOLATION_STEP_ }
procedure vcgen_bspline.interpolation_step_;
begin
 m_interpolation_step:=v;

end;

{ _INTERPOLATION_STEP }
function vcgen_bspline._interpolation_step;
begin
 result:=m_interpolation_step;

end;

{ REMOVE_ALL }
procedure vcgen_bspline.remove_all;
begin
 m_src_vertices.remove_all;

 m_closed:=0;
 m_status:=initial;

 m_src_vertex:=0;

end;

{ ADD_VERTEX }
procedure vcgen_bspline.add_vertex;
var
 pt : point_type;

begin
 m_status:=initial;

 if is_move_to(cmd ) then
  begin
   pt.x:=x;
   pt.y:=y;

   m_src_vertices.modify_last(@pt );

  end
 else
  if is_vertex(cmd ) then
   begin
    pt.x:=x;
    pt.y:=y;

    m_src_vertices.add(@pt );

   end
  else
   m_closed:=get_close_flag(cmd );

end;

{ REWIND }
procedure vcgen_bspline.rewind;
var
 i : unsigned;
 x : double;

begin
 m_cur_abscissa:=0.0;
 m_max_abscissa:=0.0;

 m_src_vertex:=0;

 if (m_status = initial ) and
    (m_src_vertices.size > 2 ) then
  begin
   if m_closed <> 0 then
    begin
     m_spline_x.init(m_src_vertices.size + 8 );
     m_spline_y.init(m_src_vertices.size + 8 );

     m_spline_x.add_point(0.0 ,point_type_ptr(m_src_vertices.prev(m_src_vertices.size - 3 ) )^.x );
     m_spline_y.add_point(0.0 ,point_type_ptr(m_src_vertices.prev(m_src_vertices.size - 3 ) )^.y );
     m_spline_x.add_point(1.0 ,point_type_ptr(m_src_vertices.array_operator(m_src_vertices.size - 3 ) )^.x );
     m_spline_y.add_point(1.0 ,point_type_ptr(m_src_vertices.array_operator(m_src_vertices.size - 3 ) )^.y );
     m_spline_x.add_point(2.0 ,point_type_ptr(m_src_vertices.array_operator(m_src_vertices.size - 2 ) )^.x );
     m_spline_y.add_point(2.0 ,point_type_ptr(m_src_vertices.array_operator(m_src_vertices.size - 2 ) )^.y );
     m_spline_x.add_point(3.0 ,point_type_ptr(m_src_vertices.array_operator(m_src_vertices.size - 1 ) )^.x );
     m_spline_y.add_point(3.0 ,point_type_ptr(m_src_vertices.array_operator(m_src_vertices.size - 1 ) )^.y );

    end
   else
    begin
     m_spline_x.init(m_src_vertices.size );
     m_spline_y.init(m_src_vertices.size );

    end;

   for i:=0 to m_src_vertices.size - 1 do
    begin
     if m_closed <> 0 then
      x:=i + 4
     else
      x:=i;

     m_spline_x.add_point(x ,point_type_ptr(m_src_vertices.array_operator(i ) )^.x );
     m_spline_y.add_point(x ,point_type_ptr(m_src_vertices.array_operator(i ) )^.y );

    end;

   m_cur_abscissa:=0.0;
   m_max_abscissa:=m_src_vertices.size - 1;

   if m_closed <> 0 then
    begin
     m_cur_abscissa:=4.0;
     m_max_abscissa:=m_max_abscissa + 5.0;

     m_spline_x.add_point(
      m_src_vertices.size + 4 ,
      point_type_ptr(m_src_vertices.array_operator(0 ) )^.x );

     m_spline_y.add_point(
      m_src_vertices.size + 4 ,
      point_type_ptr(m_src_vertices.array_operator(0 ) )^.y );

     m_spline_x.add_point(
      m_src_vertices.size + 5 ,
      point_type_ptr(m_src_vertices.array_operator(1 ) )^.x );

     m_spline_y.add_point(
      m_src_vertices.size + 5 ,
      point_type_ptr(m_src_vertices.array_operator(1 ) )^.y );

     m_spline_x.add_point(
      m_src_vertices.size + 6 ,
      point_type_ptr(m_src_vertices.array_operator(2 ) )^.x );

     m_spline_y.add_point(
      m_src_vertices.size + 6 ,
      point_type_ptr(m_src_vertices.array_operator(2 ) )^.y );

     m_spline_x.add_point(
      m_src_vertices.size + 7 ,
      point_type_ptr(m_src_vertices.next(2 ) )^.x );

     m_spline_y.add_point(
      m_src_vertices.size + 7 ,
      point_type_ptr(m_src_vertices.next(2 ) )^.y );

    end;

   m_spline_x.prepare;
   m_spline_y.prepare;

  end;

 m_status:=ready;

end;

{ VERTEX }
function vcgen_bspline.vertex;
var
 cmd : unsigned;

label
 _next ,_ready ,_polygon ;

begin
 cmd:=path_cmd_line_to;

_next:
 while not is_stop(cmd ) do
  case m_status of
   initial :
    begin
     rewind(0 );

     goto _ready;

    end;

   ready :
   _ready:
    begin
     if m_src_vertices.size < 2 then
      begin
       cmd:=path_cmd_stop;

       goto _next;

      end;

     if m_src_vertices.size = 2 then
      begin
       x^:=point_type_ptr(m_src_vertices.array_operator(m_src_vertex ) )^.x;
       y^:=point_type_ptr(m_src_vertices.array_operator(m_src_vertex ) )^.y;

       inc(m_src_vertex );

       if m_src_vertex = 1 then
        begin
         result:=path_cmd_move_to;

         exit;

        end;

       if m_src_vertex = 2 then
        begin
         result:=path_cmd_line_to;

         exit;

        end;

       cmd:=path_cmd_stop;

       goto _next;   

      end;

     cmd:=path_cmd_move_to;

     m_status:=polygon;

     m_src_vertex:=0;

     goto _polygon;

    end;

   polygon :
   _polygon:
    begin
     if m_cur_abscissa >= m_max_abscissa then
      if m_closed <> 0 then
       begin
        m_status:=end_poly;

        goto _next;

       end
      else
       begin
        x^:=point_type_ptr(m_src_vertices.array_operator(m_src_vertices.size - 1 ) )^.x;
        y^:=point_type_ptr(m_src_vertices.array_operator(m_src_vertices.size - 1 ) )^.y;

        m_status:=end_poly;
        result  :=path_cmd_line_to;

        exit;

       end;

     x^:=m_spline_x.get_stateful(m_cur_abscissa );
     y^:=m_spline_y.get_stateful(m_cur_abscissa );

     inc(m_src_vertex );

     m_cur_abscissa:=m_cur_abscissa + m_interpolation_step;

     if m_src_vertex = 1 then
      result:=path_cmd_move_to
     else
      result:=path_cmd_line_to;

     exit; 

    end;

   end_poly :
    begin
     m_status:=stop;
     result  :=path_cmd_end_poly or m_closed;

     exit;

    end;

   stop :
    begin
     result:=path_cmd_stop;

     exit;

    end;

  end;

 result:=cmd;

end;

END.

