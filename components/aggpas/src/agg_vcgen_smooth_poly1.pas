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
// Smooth polygon generator
//
// [Pascal Port History] -----------------------------------------------------
//
// 24.02.2006-Milano: Unit port establishment
//
{ agg_vcgen_smooth_poly1.pas }
unit
 agg_vcgen_smooth_poly1 ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_vertex_source ,
 agg_vertex_sequence ;

{ TYPES DEFINITION }
type
 status_e = (
  initial ,
  ready ,
  polygon ,
  ctrl_b ,
  ctrl_e ,
  ctrl1 ,
  ctrl2 ,
  end_poly ,
  stop );

 vcgen_smooth_poly1_ptr = ^vcgen_smooth_poly1; 
 vcgen_smooth_poly1 = object(vertex_source )
   m_src_vertices : vertex_sequence;
   m_smooth_value : double;

   m_closed : unsigned;
   m_status : status_e;

   m_src_vertex : unsigned;

   m_ctrl1_x ,
   m_ctrl1_y ,
   m_ctrl2_x ,
   m_ctrl2_y : double;

   constructor Construct;
   destructor  Destruct; virtual;

   procedure smooth_value_(v : double );
   function  _smooth_value : double;

  // Vertex Generator Interface
   procedure remove_all; virtual;
   procedure add_vertex(x ,y : double; cmd : unsigned ); virtual;

  // Vertex Source Interface
   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  // Private
   procedure calculate(v0 ,v1 ,v2 ,v3 : vertex_dist_ptr );

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor vcgen_smooth_poly1.Construct;
begin
 m_src_vertices.Construct(sizeof(vertex_dist ) ,6 );

 m_smooth_value:=0.5;

 m_closed:=0;
 m_status:=initial;

 m_src_vertex:=0;

end;

{ DESTRUCT }
destructor vcgen_smooth_poly1.Destruct;
begin
 m_src_vertices.Destruct;

end;

{ SMOOTH_VALUE_ }
procedure vcgen_smooth_poly1.smooth_value_;
begin
 m_smooth_value:=v * 0.5;

end;

{ _SMOOTH_VALUE }
function vcgen_smooth_poly1._smooth_value;
begin
 result:=m_smooth_value * 2.0;

end;

{ REMOVE_ALL }
procedure vcgen_smooth_poly1.remove_all;
begin
 m_src_vertices.remove_all;

 m_closed:=0;
 m_status:=initial;

end;

{ ADD_VERTEX }
procedure vcgen_smooth_poly1.add_vertex;
var
 vd : vertex_dist;

begin
 m_status:=initial;

 if is_move_to(cmd ) then
  begin
   vd.x:=x;
   vd.y:=y;

   vd.dist:=0;

   m_src_vertices.modify_last(@vd );

  end
 else
  if is_vertex(cmd ) then
   begin
    vd.x:=x;
    vd.y:=y;

    vd.dist:=0;

    m_src_vertices.add(@vd );

   end
  else
   m_closed:=get_close_flag(cmd );

end;

{ REWIND }
procedure vcgen_smooth_poly1.rewind;
begin
 if m_status = initial then
  m_src_vertices.close(boolean(m_closed <> 0 ) );

 m_status    :=ready;
 m_src_vertex:=0;

end;

{ VERTEX }
function vcgen_smooth_poly1.vertex;
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
     if m_src_vertices.size <  2 then
      begin
       cmd:=path_cmd_stop;

       goto _next;

      end;

     if m_src_vertices.size = 2 then
      begin
       x^:=vertex_dist_ptr(m_src_vertices.array_operator(m_src_vertex ) ).x;
       y^:=vertex_dist_ptr(m_src_vertices.array_operator(m_src_vertex ) ).y;

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
     if m_closed <> 0 then
      if m_src_vertex >= m_src_vertices.size then
       begin
        x^:=vertex_dist_ptr(m_src_vertices.array_operator(0 ) ).x;
        y^:=vertex_dist_ptr(m_src_vertices.array_operator(0 ) ).y;

        m_status:=end_poly;
        result  :=path_cmd_curve4;

        exit;

       end
      else
     else
      if m_src_vertex >= m_src_vertices.size - 1 then
       begin
        x^:=vertex_dist_ptr(m_src_vertices.array_operator(m_src_vertices.size - 1 ) ).x;
        y^:=vertex_dist_ptr(m_src_vertices.array_operator(m_src_vertices.size - 1 ) ).y;

        m_status:=end_poly;
        result  :=path_cmd_curve3;

        exit;

       end;

     calculate(
      m_src_vertices.prev(m_src_vertex ) ,
      m_src_vertices.curr(m_src_vertex ) ,
      m_src_vertices.next(m_src_vertex ) ,
      m_src_vertices.next(m_src_vertex + 1 ) );

     x^:=vertex_dist_ptr(m_src_vertices.array_operator(m_src_vertex ) ).x;
     y^:=vertex_dist_ptr(m_src_vertices.array_operator(m_src_vertex ) ).y;

     inc(m_src_vertex );

     if m_closed <> 0 then
      begin
       m_status:=ctrl1;

       if m_src_vertex = 1 then
        result:=path_cmd_move_to
       else
        result:=path_cmd_curve4;

       exit;

      end
     else
      begin
       if m_src_vertex = 1 then
        begin
         m_status:=ctrl_b;
         result  :=path_cmd_move_to;

         exit;

        end;

       if m_src_vertex >= m_src_vertices.size - 1 then
        begin
         m_status:=ctrl_e;
         result  :=path_cmd_curve3;

         exit;

        end;

       m_status:=ctrl1;
       result  :=path_cmd_curve4;

       exit;

      end;

    end;

   ctrl_b :
    begin
     x^:=m_ctrl2_x;
     y^:=m_ctrl2_y;

     m_status:=polygon;
     result  :=path_cmd_curve3;

     exit;

    end;

   ctrl_e :
    begin
     x^:=m_ctrl1_x;
     y^:=m_ctrl1_y;

     m_status:=polygon;
     result  :=path_cmd_curve3;

     exit;

    end;

   ctrl1 :
    begin
     x^:=m_ctrl1_x;
     y^:=m_ctrl1_y;

     m_status:=ctrl2;
     result  :=path_cmd_curve4;

     exit;

    end;

   ctrl2 :
    begin
     x^:=m_ctrl2_x;
     y^:=m_ctrl2_y;

     m_status:=polygon;
     result  :=path_cmd_curve4;

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

{ CALCULATE }
procedure vcgen_smooth_poly1.calculate;
var
 k1 ,k2 ,xm1 ,ym1 ,xm2 ,ym2 : double;

begin
 k1:=v0.dist / (v0.dist + v1.dist );
 k2:=v1.dist / (v1.dist + v2.dist );

 xm1:=v0.x + (v2.x - v0.x ) * k1;
 ym1:=v0.y + (v2.y - v0.y ) * k1;
 xm2:=v1.x + (v3.x - v1.x ) * k2;
 ym2:=v1.y + (v3.y - v1.y ) * k2;

 m_ctrl1_x:=v1.x + m_smooth_value * (v2.x - xm1 );
 m_ctrl1_y:=v1.y + m_smooth_value * (v2.y - ym1 );
 m_ctrl2_x:=v2.x + m_smooth_value * (v1.x - xm2 );
 m_ctrl2_y:=v2.y + m_smooth_value * (v1.y - ym2 );

end;

END.

