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
// General Polygon Clipper based on the GPC library by Alan Murta 
// Union, Intersection, XOR, A-B, B-A
// Contact the author if you intend to use it in commercial applications!
// http://www.cs.man.ac.uk/aig/staff/alan/software/
// Alan Murta (email: gpc@cs.man.ac.uk)
//
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 22.02.2006-Milano: Unit port establishment
//
{ agg_conv_gpc.pas }
unit
 agg_conv_gpc ;

INTERFACE

{$I agg_mode.inc }

uses
 gpc ,
 agg_basics ,
 agg_array ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 gpc_op_e = (

  gpc_or ,
  gpc_and ,
  gpc_xor ,
  gpc_a_minus_b ,
  gpc_b_minus_a

  );

 status = (status_move_to ,status_line_to ,status_stop );

 gpc_vertex_ptr = ^Tgpc_vertex;

 contour_header_type_ptr = ^contour_header_type;
 contour_header_type = record
   num_vertices ,
   hole_flag    : int;

   vertices : gpc_vertex_ptr;

  end;

 conv_gpc_ptr = ^conv_gpc;
 conv_gpc = object(vertex_source )
   m_src_a ,
   m_src_b : vertex_source_ptr;

   m_status    : status;
   m_vertex    ,
   m_contour   : int;
   m_operation : gpc_op_e;

   m_vertex_accumulator  ,
   m_contour_accumulator : pod_deque;

   m_poly_a ,
   m_poly_b ,
   m_result : Tgpc_polygon;

   constructor Construct(a ,b : vertex_source_ptr; op : gpc_op_e = gpc_or );
   destructor  Destruct; virtual;

   procedure set_source1(source : vertex_source_ptr );
   procedure set_source2(source : vertex_source_ptr );

   procedure operation(v : gpc_op_e );

  // Vertex Source Interface
   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  // Private
   procedure free_polygon(p : Pgpc_polygon );
   procedure free_result;
   procedure free_gpc_data;
   procedure start_contour;
   procedure add_vertex_ (x ,y : double );
   procedure end_contour (orientation : unsigned );
   procedure make_polygon(p : Pgpc_polygon );
   procedure start_extracting;

   function  next_contour : boolean;
   function  next_vertex(x ,y : double_ptr ) : boolean;

   procedure add(src : vertex_source_ptr; p : Pgpc_polygon );

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor conv_gpc.Construct;
begin
 m_vertex_accumulator.Construct (sizeof(Tgpc_vertex ) ,8 );
 m_contour_accumulator.Construct(sizeof(contour_header_type ) ,6 );

 m_src_a:=a;
 m_src_b:=b;

 m_status   :=status_move_to;
 m_vertex   :=-1;
 m_contour  :=-1;
 m_operation:=op;

 fillchar(m_poly_a ,sizeof(m_poly_a ) ,0 );
 fillchar(m_poly_b ,sizeof(m_poly_b ) ,0 );
 fillchar(m_result ,sizeof(m_result ) ,0 );

end;

{ DESTRUCT }
destructor conv_gpc.Destruct;
begin
 free_gpc_data;

 m_vertex_accumulator.Destruct;
 m_contour_accumulator.Destruct;

end;

{ SET_SOURCE1 }
procedure conv_gpc.set_source1;
begin
 m_src_a:=source;

end;

{ SET_SOURCE2 }
procedure conv_gpc.set_source2;
begin
 m_src_b:=source;

end;

{ OPERATION }
procedure conv_gpc.operation;
begin
 m_operation:=v;

end;

{ REWIND }
procedure conv_gpc.rewind;
begin
 free_result;

 m_src_a.rewind(path_id );
 m_src_b.rewind(path_id );

 add(m_src_a ,@m_poly_a );
 add(m_src_b ,@m_poly_b );

 case m_operation of
  gpc_or :
   gpc_polygon_clip(gpc.GPC_UNION ,@m_poly_a ,@m_poly_b ,@m_result );

  gpc_and :
   gpc_polygon_clip(gpc.GPC_INT ,@m_poly_a ,@m_poly_b ,@m_result );

  gpc_xor :
   gpc_polygon_clip(gpc.GPC_XOR ,@m_poly_a ,@m_poly_b ,@m_result );

  gpc_a_minus_b :
   gpc_polygon_clip(gpc.GPC_DIFF ,@m_poly_a ,@m_poly_b ,@m_result );

  gpc_b_minus_a :
   gpc_polygon_clip(gpc.GPC_DIFF ,@m_poly_b ,@m_poly_a ,@m_result );

 end;

 start_extracting;

end;

{ VERTEX }
function conv_gpc.vertex;
begin
 if m_status = status_move_to then
  if next_contour then
   begin
    if next_vertex(x ,y ) then
     begin
      m_status:=status_line_to;
      result  :=path_cmd_move_to;

      exit;

     end;

    m_status:=status_stop;
    result  :=path_cmd_end_poly or path_flags_close;

    exit;

   end
  else
 else
  begin
   if next_vertex(x ,y ) then
    begin
     result:=path_cmd_line_to;

     exit;

    end
   else
    m_status:=status_move_to;

   result:=path_cmd_end_poly or path_flags_close;

   exit;

  end;

 result:=path_cmd_stop;

end;

{ FREE_POLYGON }
procedure conv_gpc.free_polygon;
var
 i : int;

begin
 i:=0;

 while i < p.num_contours do
  begin
   agg_freemem(pointer(p.contour[i ].vertex ) ,p.contour[i].num_vertices * sizeof(Tgpc_vertex ) );

   inc(i );

  end;

 //agg_freemem(pointer(p.hole ) ,? );
 agg_freemem(pointer(p.contour ) ,p.num_contours * sizeof(Tgpc_vertex_list ) );

 fillchar(p^ ,sizeof(Tgpc_polygon ) ,0 );

end;

{ FREE_RESULT }
procedure conv_gpc.free_result;
begin
 if m_result.contour <> NIL then
  gpc_free_polygon(@m_result );

 fillchar(m_result ,sizeof(m_result ) ,0 );

end;

{ FREE_GPC_DATA }
procedure conv_gpc.free_gpc_data;
begin
 free_polygon(@m_poly_a );
 free_polygon(@m_poly_b );
 free_result;

end;

{ START_CONTOUR }
procedure conv_gpc.start_contour;
var
 h : contour_header_type;

begin
 fillchar(h ,sizeof(h ) ,0 );
 m_contour_accumulator.add(@h );
 m_vertex_accumulator.remove_all;

end;

{ ADD_VERTEX_ }
procedure conv_gpc.add_vertex_;
var
 v : Tgpc_vertex;

begin
 v.x:=x;
 v.y:=y;

 m_vertex_accumulator.add(@v );

end;

{ END_CONTOUR }
procedure conv_gpc.end_contour;
var
 h : contour_header_type_ptr;
 d ,
 s : gpc_vertex_ptr;
 i : int;

begin
 if m_contour_accumulator.size <> 0 then
  if m_vertex_accumulator.size > 2 then
   begin
    h:=m_contour_accumulator.array_operator(m_contour_accumulator.size - 1 ); 

    h.num_vertices:=m_vertex_accumulator.size;
    h.hole_flag   :=0;

    // TO DO: Clarify the "holes"
    // if is_cw(orientation ) then h.hole_flag:=1;

    agg_getmem(pointer(h.vertices ) ,h.num_vertices * sizeof(Tgpc_vertex ) );

    d:=h.vertices;

    for i:=0 to h.num_vertices - 1 do
     begin
      s:=m_vertex_accumulator.array_operator(i );

      d.x:=s.x;
      d.y:=s.y;

      inc(ptrcomp(d ) ,sizeof(Tgpc_vertex ) );

     end;

   end
  else
   m_vertex_accumulator.remove_last;

end;

{ MAKE_POLYGON }
procedure conv_gpc.make_polygon;
var
 i : int;
 h : contour_header_type_ptr;

//ph : int_ptr;
 pv : Pgpc_vertex_list;

begin
 free_polygon(p );

 if m_contour_accumulator.size <> 0 then
  begin
   p.num_contours:=m_contour_accumulator.size;

   // TO DO: Clarify the "holes"
   // p.hole = new int[p.num_contours];

   p.hole:=NIL;

   agg_getmem(pointer(p.contour ) ,p.num_contours * sizeof(Tgpc_vertex_list ) );

   //ph:=p.hole;
   pv:=Pgpc_vertex_list(p.contour );

   if p.num_contours > 0 then
    for i:=0 to p.num_contours - 1 do
     begin
      h:=m_contour_accumulator.array_operator(i );

      //*ph++ = h.hole_flag;
      pv.num_vertices:=h.num_vertices;
      pv.vertex      :=Pgpc_vertex_array(h.vertices );

      inc(ptrcomp(pv ) ,sizeof(Tgpc_vertex_list ) );

     end;

  end;

end;

{ START_EXTRACTING }
procedure conv_gpc.start_extracting;
begin
 m_status :=status_move_to;
 m_contour:=-1;
 m_vertex :=-1;

end;

{ NEXT_CONTOUR }
function conv_gpc.next_contour;
begin
 inc(m_contour );

 if m_contour < m_result.num_contours then
  begin
   m_vertex:=-1;

   result:=true;

  end
 else
  result:=false;

end;

{ NEXT_VERTEX }
function conv_gpc.next_vertex;
var
 vlist : Pgpc_vertex_list;

 v : gpc_vertex_ptr;

begin
 vlist:=@m_result.contour[m_contour ];

 inc(m_vertex );

 if m_vertex < vlist.num_vertices then
  begin
   v:=@vlist.vertex[m_vertex ];

   x^:=v.x;
   y^:=v.y;

   result:=true;

  end
 else
  result:=false; 

end;

{ ADD }
procedure conv_gpc.add;
var
 cmd ,orientation : unsigned;

 x ,y ,start_x ,start_y : double;

 line_to : boolean;

begin
 start_x:=0.0;
 start_y:=0.0;
 line_to:=false;

 orientation:=0;

 m_contour_accumulator.remove_all;

 cmd:=src.vertex(@x ,@y );

 while not is_stop(cmd ) do
  begin
   if is_vertex(cmd ) then
    begin
     if is_move_to(cmd ) then
      begin
       if line_to then
        begin
         end_contour(orientation );

         orientation:=0;

        end;

       start_contour;

       start_x:=x;
       start_y:=y;

      end;

     add_vertex_(x ,y );

     line_to:=true;

    end
   else
    if is_end_poly(cmd ) then
     begin
      orientation:=get_orientation(cmd );

      if line_to and
         is_closed(cmd ) then
       add_vertex_(start_x ,start_y );  

     end;

   cmd:=src.vertex(@x ,@y );

  end;

 if line_to then
  end_contour(orientation );

 make_polygon(p );

end;

END.

