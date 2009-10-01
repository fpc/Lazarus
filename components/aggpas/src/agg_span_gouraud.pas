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
// 27.01.2006-Milano: Unit port establishment
//
{ agg_span_gouraud.pas }
unit
 agg_span_gouraud ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_math ,
 agg_span_allocator ,
 agg_span_generator ,
 agg_color ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 coord_type_ptr = ^coord_type;
 coord_type = record
   x ,y  : double;
   color : aggclr;

  end;

 span_gouraud = object(span_generator )
   m_coord  : array[0..2 ] of coord_type;
   m_x      ,
   m_y      : array[0..7 ] of double;
   m_cmd    : array[0..7 ] of unsigned;
   m_vertex : unsigned;

   constructor Construct(alloc : span_allocator_ptr ); overload;
   constructor Construct(
                alloc : span_allocator_ptr;
                c1 ,c2 ,c3 : aggclr_ptr;
                x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,d : double ); overload;

   procedure colors_ (c1 ,c2 ,c3 : aggclr_ptr );
   procedure triangle(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,d : double );

  // Vertex Source Interface to feed the coordinates to the rasterizer
   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  // Private
   procedure arrange_vertices(coord : coord_type_ptr );

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor span_gouraud.Construct(alloc : span_allocator_ptr );
begin
 inherited Construct(alloc );

 m_cmd[0 ]:=path_cmd_stop;

end;

{ CONSTRUCT }
constructor span_gouraud.Construct(
                          alloc : span_allocator_ptr;
                          c1 ,c2 ,c3 : aggclr_ptr;
                          x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,d : double );
begin
 inherited Construct(alloc );

 colors_ (c1 ,c2 ,c3 );
 triangle(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,d );

end;

{ COLORS_ }
procedure span_gouraud.colors_;
begin
 m_coord[0 ].color:=c1^;
 m_coord[1 ].color:=c2^;
 m_coord[2 ].color:=c3^;

end;

{ TRIANGLE }
// Sets the triangle and dilates it if needed.
// The trick here is to calculate beveled joins in the vertices of the
// triangle and render it as a 6-vertex polygon.
// It's necessary to achieve numerical stability.
// However, the coordinates to interpolate colors are calculated
// as miter joins (calc_intersection).
procedure span_gouraud.triangle;
begin
 m_coord[0 ].x:=x1;
 m_x[0 ]      :=x1;
 m_coord[0 ].y:=y1;
 m_y[0 ]      :=y1;
 m_coord[1 ].x:=x2;
 m_x[1 ]      :=x2;
 m_coord[1 ].y:=y2;
 m_y[1 ]      :=y2;
 m_coord[2 ].x:=x3;
 m_x[2 ]      :=x3;
 m_coord[2 ].y:=y3;
 m_y[2 ]      :=y3;

 m_cmd[0 ]:=path_cmd_move_to;
 m_cmd[1 ]:=path_cmd_line_to;
 m_cmd[2 ]:=path_cmd_line_to;
 m_cmd[3 ]:=path_cmd_stop;

 if d <> 0.0 then
  begin
   dilate_triangle(
    m_coord[0 ].x ,m_coord[0 ].y ,
    m_coord[1 ].x ,m_coord[1 ].y ,
    m_coord[2 ].x ,m_coord[2 ].y ,
    @m_x ,@m_y ,d );

   calc_intersection(
    m_x[4 ] ,m_y[4 ] ,m_x[5 ] ,m_y[5 ] ,
    m_x[0 ] ,m_y[0 ] ,m_x[1 ] ,m_y[1 ] ,
    @m_coord[0 ].x ,@m_coord[0 ].y );

   calc_intersection(
    m_x[0 ] ,m_y[0 ] ,m_x[1 ] ,m_y[1 ] ,
    m_x[2 ] ,m_y[2 ] ,m_x[3 ] ,m_y[3 ] ,
    @m_coord[1 ].x ,@m_coord[1 ].y );

   calc_intersection(
    m_x[2 ] ,m_y[2 ] ,m_x[3 ] ,m_y[3 ] ,
    m_x[4 ] ,m_y[4 ] ,m_x[5 ] ,m_y[5 ] ,
    @m_coord[2 ].x ,@m_coord[2 ].y );

   m_cmd[3 ]:=path_cmd_line_to;
   m_cmd[4 ]:=path_cmd_line_to;
   m_cmd[5 ]:=path_cmd_line_to;
   m_cmd[6 ]:=path_cmd_stop;

  end;

end;

{ REWIND }
procedure span_gouraud.rewind;
begin
 m_vertex:=0;

end;

{ VERTEX }
function span_gouraud.vertex;
begin
 x^:=m_x[m_vertex ];
 y^:=m_y[m_vertex ];

 result:=m_cmd[m_vertex ];

 inc(m_vertex );

end;

{ ARRANGE_VERTICES }
procedure span_gouraud.arrange_vertices;
var
 tmp : coord_type;

begin
 coord_type_ptr(ptrcomp(coord ) + 0 * sizeof(coord_type ) )^:=m_coord[0 ];
 coord_type_ptr(ptrcomp(coord ) + 1 * sizeof(coord_type ) )^:=m_coord[1 ];
 coord_type_ptr(ptrcomp(coord ) + 2 * sizeof(coord_type ) )^:=m_coord[2 ];

 if m_coord[0 ].y > m_coord[2 ].y then
  begin
   coord_type_ptr(ptrcomp(coord ) + 0 * sizeof(coord_type ) )^:=m_coord[2 ];
   coord_type_ptr(ptrcomp(coord ) + 2 * sizeof(coord_type ) )^:=m_coord[0 ];

  end;

 if coord_type_ptr(ptrcomp(coord ) + 0 * sizeof(coord_type ) ).y >
    coord_type_ptr(ptrcomp(coord ) + 1 * sizeof(coord_type ) ).y then
  begin
   tmp:=coord_type_ptr(ptrcomp(coord ) + 1 * sizeof(coord_type ) )^;

   coord_type_ptr(ptrcomp(coord ) + 1 * sizeof(coord_type ) )^:=
    coord_type_ptr(ptrcomp(coord ) + 0 * sizeof(coord_type ) )^;

   coord_type_ptr(ptrcomp(coord ) + 0 * sizeof(coord_type ) )^:=tmp;

  end;

 if coord_type_ptr(ptrcomp(coord ) + 1 * sizeof(coord_type ) ).y >
    coord_type_ptr(ptrcomp(coord ) + 2 * sizeof(coord_type ) ).y then
  begin
   tmp:=coord_type_ptr(ptrcomp(coord ) + 2 * sizeof(coord_type ) )^;

   coord_type_ptr(ptrcomp(coord ) + 2 * sizeof(coord_type ) )^:=
    coord_type_ptr(ptrcomp(coord ) + 1 * sizeof(coord_type ) )^;

   coord_type_ptr(ptrcomp(coord ) + 1 * sizeof(coord_type ) )^:=tmp;

  end;

end;

END.

