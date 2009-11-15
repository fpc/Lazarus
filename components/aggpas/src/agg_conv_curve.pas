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
// classes conv_curve
//
// [Pascal Port History] -----------------------------------------------------
//
// 12.02.2006-Milano: Unit port establishment
//
{ agg_conv_curve.pas }
unit
 agg_conv_curve ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_curves ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
// Curve converter class. Any path storage can have Bezier curves defined
// by their control points. There're two types of curves supported: curve3
// and curve4. Curve3 is a conic Bezier curve with 2 endpoints and 1 control
// point. Curve4 has 2 control points (4 points in total) and can be used
// to interpolate more complicated curves. Curve4, unlike curve3 can be used
// to approximate arcs, both circular and elliptical. Curves are approximated
// with straight lines and one of the approaches is just to store the whole
// sequence of vertices that approximate our curve. It takes additional
// memory, and at the same time the consecutive vertices can be calculated
// on demand.
//
// Initially, path storages are not suppose to keep all the vertices of the
// curves (although, nothing prevents us from doing so). Instead, path_storage
// keeps only vertices, needed to calculate a curve on demand. Those vertices
// are marked with special commands. So, if the path_storage contains curves
// (which are not real curves yet), and we render this storage directly,
// all we will see is only 2 or 3 straight line segments (for curve3 and
// curve4 respectively). If we need to see real curves drawn we need to
// include this class into the conversion pipeline.
//
// Class conv_curve recognizes commands path_cmd_curve3 and path_cmd_curve4
// and converts these vertices into a move_to/line_to sequence.
 conv_curve_ptr = ^conv_curve;
 conv_curve = object(curve )
   m_source : vertex_source_ptr;
   m_last_x ,
   m_last_y : double;
   m_curve3 ,
   m_curve4 : curve_ptr;

   constructor Construct(source : vertex_source_ptr; c3 : curve_ptr = NIL; c4 : curve_ptr = NIL );
   destructor  Destruct; virtual;

   procedure set_source(source : vertex_source_ptr );

   procedure approximation_method_(v : curve_approximation_method_e ); virtual;
   function  _approximation_method : curve_approximation_method_e; virtual;

   procedure approximation_scale_(s : double ); virtual;
   function  _approximation_scale : double; virtual;

   procedure angle_tolerance_(a : double ); virtual;
   function  _angle_tolerance : double; virtual;

   procedure cusp_limit_(v : double ); virtual;
   function  _cusp_limit : double; virtual;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor conv_curve.Construct;
begin
 if c3 <> NIL then
  m_curve3:=c3
 else
  m_curve3:=new(curve3_ptr ,Construct );

 if c4 <> NIL then
  m_curve4:=c4
 else
  m_curve4:=new(curve4_ptr ,Construct );

 m_source:=source;
 m_last_x:=0.0;
 m_last_y:=0.0;

end;

{ DESTRUCT }
destructor conv_curve.Destruct;
begin
 if m_curve3 <> NIL then
  dispose(m_curve3 ,Destruct );

 if m_curve4 <> NIL then
  dispose(m_curve4 ,Destruct );

end;

{ SET_SOURCE }
procedure conv_curve.set_source;
begin
 m_source:=source;

end;

{ APPROXIMATION_METHOD_ }
procedure conv_curve.approximation_method_;
begin
 m_curve3.approximation_method_(v );
 m_curve4.approximation_method_(v );

end;

{ _APPROXIMATION_METHOD }
function conv_curve._approximation_method;
begin
 result:=m_curve4._approximation_method;

end;

{ APPROXIMATION_SCALE_ }
procedure conv_curve.approximation_scale_;
begin
 m_curve3.approximation_scale_(s );
 m_curve4.approximation_scale_(s );

end;

{ _APPROXIMATION_SCALE }
function conv_curve._approximation_scale;
begin
 result:=m_curve4._approximation_scale;

end;

{ ANGLE_TOLERANCE_ }
procedure conv_curve.angle_tolerance_;
begin
 m_curve3.angle_tolerance_(a );
 m_curve4.angle_tolerance_(a );

end;

{ _ANGLE_TOLERANCE }
function conv_curve._angle_tolerance;
begin
 result:=m_curve4._angle_tolerance;

end;

{ CUSP_LIMIT_ }
procedure conv_curve.cusp_limit_;
begin
 m_curve3.cusp_limit_(v );
 m_curve4.cusp_limit_(v );

end;

{ _CUSP_LIMIT }
function conv_curve._cusp_limit;
begin
 result:=m_curve4._cusp_limit;

end;

{ REWIND }
procedure conv_curve.rewind;
begin
 m_source.rewind(path_id );

 m_last_x:=0.0;
 m_last_y:=0.0;

 m_curve3.reset;
 m_curve4.reset;

end;

{ VERTEX }
function conv_curve.vertex;
var
 ct2_x ,ct2_y ,end_x ,end_y : double;

 cmd : unsigned;

begin
 if not is_stop(m_curve3.vertex(x ,y ) ) then
  begin
   m_last_x:=x^;
   m_last_y:=y^;

   result:=path_cmd_line_to;

   exit;

  end;

 if not is_stop(m_curve4.vertex(x ,y ) ) then
  begin
   m_last_x:=x^;
   m_last_y:=y^;

   result:=path_cmd_line_to;

   exit;

  end;

 cmd:=m_source.vertex(x ,y );

 case cmd of
  path_cmd_move_to ,path_cmd_line_to :
   begin
    m_last_x:=x^;
    m_last_y:=y^;

   end;

  path_cmd_curve3 :
   begin
    m_source.vertex(@end_x ,@end_y );
    m_curve3.init3 (m_last_x ,m_last_y ,x^ ,y^ ,end_x ,end_y );

    m_curve3.vertex(x ,y ); // First call returns path_cmd_move_to
    m_curve3.vertex(x ,y ); // This is the first vertex of the curve

    cmd:=path_cmd_line_to;

   end;

  path_cmd_curve4 :
   begin
    m_source.vertex(@ct2_x ,@ct2_y );
    m_source.vertex(@end_x ,@end_y );

    m_curve4.init4 (m_last_x ,m_last_y ,x^ ,y^ ,ct2_x ,ct2_y ,end_x ,end_y );

    m_curve4.vertex(x ,y ); // First call returns path_cmd_move_to
    m_curve4.vertex(x ,y ); // This is the first vertex of the curve

    cmd:=path_cmd_line_to;

   end;

 end;

 result:=cmd;

end;

END.

