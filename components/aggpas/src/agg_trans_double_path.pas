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
// 20.02.2006-Milano: Unit port establishment
//
{ agg_trans_double_path.pas }
unit
 agg_trans_double_path ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_vertex_source ,
 agg_vertex_sequence ,
 agg_trans_affine ;


{ TYPES DEFINITION }
type
 status_e = (initial ,making_path ,ready );

 trans_double_path_ptr = ^trans_double_path;
 trans_double_path = object(trans_affine )
   m_src_vertices1 ,
   m_src_vertices2 : vertex_sequence;

   m_base_length ,
   m_base_height ,
   m_kindex1     ,
   m_kindex2     : double;

   m_status1 ,
   m_status2 : status_e;

   m_preserve_x_scale : boolean;

   constructor Construct;
   destructor  Destruct;

   procedure base_length_(v : double );
   function  _base_length : double;

   procedure base_height_(v : double );
   function  _base_height : double;

   procedure preserve_x_scale_(f : boolean );
   function  _preserve_x_scale : boolean;

   procedure reset; virtual;

   procedure move_to1(x ,y : double );
   procedure line_to1(x ,y : double );
   procedure move_to2(x ,y : double );
   procedure line_to2(x ,y : double );
   procedure finalize_paths;

   procedure add_paths(vs1 ,vs2 : vertex_source_ptr; path1_id : unsigned = 0; path2_id : unsigned = 0 );

   function  total_length1 : double;
   function  total_length2 : double;

   function  finalize_path(vertices : vertex_sequence_ptr ) : double;

   procedure transform1(vertices : vertex_sequence_ptr; kindex ,kx : double; x ,y : double_ptr );

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ _transform }
procedure _transform(this : trans_double_path_ptr; x ,y : double_ptr );
var
 x1 ,y1 ,x2 ,y2 ,dd : double;

begin
 if (this.m_status1 = ready ) and
    (this.m_status2 = ready ) then
  begin
   if this.m_base_length > 1e-10 then
    x^:=
     x^ *
     (vertex_dist_ptr(this.m_src_vertices1.array_operator(this.m_src_vertices1.size - 1 ) ).dist /
      this.m_base_length );

   x1:=x^;
   y1:=y^;
   x2:=x^;
   y2:=y^;
   dd:=
    vertex_dist_ptr(this.m_src_vertices2.array_operator(this.m_src_vertices2.size - 1 ) ).dist /
    vertex_dist_ptr(this.m_src_vertices1.array_operator(this.m_src_vertices1.size - 1 ) ).dist;

   this.transform1(@this.m_src_vertices1 ,this.m_kindex1 ,1.0 ,@x1 ,@y1 );
   this.transform1(@this.m_src_vertices2 ,this.m_kindex2 ,dd  ,@x2 ,@y2 );

   x^:=x1 + y^ * (x2 - x1 ) / this.m_base_height;
   y^:=y1 + y^ * (y2 - y1 ) / this.m_base_height;

  end;

end;

{ CONSTRUCT }
constructor trans_double_path.Construct;
begin
 inherited Construct;

 transform:=@_transform;

 m_src_vertices1.Construct(sizeof(vertex_dist ) );
 m_src_vertices2.Construct(sizeof(vertex_dist ) );

 m_kindex1:=0.0;
 m_kindex2:=0.0;

 m_base_length:=0.0;
 m_base_height:=1.0;

 m_status1:=initial;
 m_status2:=initial;

 m_preserve_x_scale:=true;

end;

{ DESTRUCT }
destructor trans_double_path.Destruct;
begin
 m_src_vertices1.Destruct;
 m_src_vertices2.Destruct;

end;

{ BASE_LENGTH_ }
procedure trans_double_path.base_length_;
begin
 m_base_length:=v;

end;

{ _BASE_LENGTH }
function trans_double_path._base_length;
begin
 result:=m_base_length;

end;

{ BASE_HEIGHT_ }
procedure trans_double_path.base_height_;
begin
 m_base_height:=v;

end;

{ _BASE_HEIGHT }
function trans_double_path._base_height;
begin
 result:=m_base_height;

end;

{ PRESERVE_X_SCALE_ }
procedure trans_double_path.preserve_x_scale_;
begin
 m_preserve_x_scale:=f;

end;

{ _PRESERVE_X_SCALE }
function trans_double_path._preserve_x_scale;
begin
 result:=m_preserve_x_scale;

end;

{ RESET }
procedure trans_double_path.reset;
begin
 m_src_vertices1.remove_all;
 m_src_vertices2.remove_all;

 m_kindex1:=0.0;
 m_kindex1:=0.0;
 m_status1:=initial;
 m_status2:=initial;

end;

{ MOVE_TO1 }
procedure trans_double_path.move_to1;
var
 vd : vertex_dist;

begin
 if m_status1 = initial then
  begin
   vd.x:=x;
   vd.y:=y;

   vd.dist:=0;

   m_src_vertices1.modify_last(@vd );

   m_status1:=making_path;

  end
 else
  line_to1(x ,y );

end;

{ LINE_TO1 }
procedure trans_double_path.line_to1;
var
 vd : vertex_dist;

begin
 if m_status1 = making_path then
  begin
   vd.x:=x;
   vd.y:=y;

   vd.dist:=0;

   m_src_vertices1.add(@vd );

  end;

end;

{ MOVE_TO2 }
procedure trans_double_path.move_to2;
var
 vd : vertex_dist;

begin
 if m_status2 = initial then
  begin
   vd.x:=x;
   vd.y:=y;

   vd.dist:=0;

   m_src_vertices2.modify_last(@vd );

   m_status2:=making_path;

  end
 else
  line_to2(x ,y );

end;

{ LINE_TO2 }
procedure trans_double_path.line_to2;
var
 vd : vertex_dist;

begin
 if m_status2 = making_path then
  begin
   vd.x:=x;
   vd.y:=y;

   vd.dist:=0;

   m_src_vertices2.add(@vd );

  end;

end;

{ FINALIZE_PATHS }
procedure trans_double_path.finalize_paths;
begin
 if (m_status1 = making_path ) and
    (m_src_vertices1.size > 1 ) and
    (m_status2 = making_path ) and
    (m_src_vertices2.size > 1 ) then
  begin
   m_kindex1:=finalize_path(@m_src_vertices1 );
   m_kindex2:=finalize_path(@m_src_vertices2 );
   m_status1:=ready;
   m_status2:=ready;

  end;

end;

{ ADD_PATHS }
procedure trans_double_path.add_paths;
var
 x ,y : double;
 cmd  : unsigned;

begin
 vs1.rewind(path1_id );

 cmd:=vs1.vertex(@x ,@y );

 while not is_stop(cmd ) do
  begin
   if is_move_to(cmd ) then
    move_to1(x ,y )
   else
    if is_vertex(cmd ) then
     line_to1(x ,y );

   cmd:=vs1.vertex(@x ,@y );

  end;

 vs2.rewind(path2_id );

 cmd:=vs2.vertex(@x ,@y );

 while not is_stop(cmd ) do
  begin
   if is_move_to(cmd ) then
    move_to2(x ,y )
   else
    if is_vertex(cmd ) then
     line_to2(x ,y );

   cmd:=vs2.vertex(@x ,@y );

  end;

 finalize_paths;

end;

{ TOTAL_LENGTH1 }
function trans_double_path.total_length1;
begin
 if m_base_length >= 1e-10 then
  result:=m_base_length
 else
  if m_status1 = ready then
   result:=vertex_dist_ptr(m_src_vertices1.array_operator(m_src_vertices1.size - 1 ) ).dist
  else
   result:=0.0;

end;

{ TOTAL_LENGTH2 }
function trans_double_path.total_length2;
begin
 if m_base_length >= 1e-10 then
  result:=m_base_length
 else
  if m_status2 = ready then
   result:=vertex_dist_ptr(m_src_vertices2.array_operator(m_src_vertices2.size - 1 ) ).dist
  else
   result:=0.0;

end;

{ FINALIZE_PATH }
function trans_double_path.finalize_path;
var
 i : unsigned;
 v : vertex_dist_ptr;

 d ,dist : double;

begin
 vertices.close(false );

 if vertices.size > 2 then
  if vertex_dist_ptr(vertices.array_operator(vertices.size - 2 ) ).dist * 10.0 <
     vertex_dist_ptr(vertices.array_operator(vertices.size - 3 ) ).dist then
   begin
    d:=
     vertex_dist_ptr(vertices.array_operator(vertices.size - 3 ) ).dist +
     vertex_dist_ptr(vertices.array_operator(vertices.size - 2 ) ).dist;

    move(
     vertices.array_operator(vertices.size - 1 )^ ,
     vertices.array_operator(vertices.size - 2 )^ ,
     sizeof(vertex_dist ) );

    vertices.remove_last;

    vertex_dist_ptr(vertices.array_operator(vertices.size - 2 ) ).dist:=d;

   end;

 dist:=0;

 for i:=0 to vertices.size - 1 do
  begin
   v:=vertices.array_operator(i );
   d:=v.dist;

   v.dist:=dist;
   dist  :=dist + d;

  end;

 result:=(vertices.size - 1 ) / dist;

end;

{ TRANSFORM1 }
procedure trans_double_path.transform1;
var
 x1 ,y1 ,dx ,dy ,d ,dd : double;

 i ,j ,k : unsigned;

begin
 x1:=0.0;
 y1:=0.0;
 dx:=1.0;
 dy:=1.0;
 d :=0.0;
 dd:=1.0;

 x^:=x^ * kx;

 if x^ < 0.0 then
  begin
  // Extrapolation on the left
   x1:=vertex_dist_ptr(vertices.array_operator(0 ) ).x;
   y1:=vertex_dist_ptr(vertices.array_operator(0 ) ).y;
   dx:=vertex_dist_ptr(vertices.array_operator(1 ) ).x - x1;
   dy:=vertex_dist_ptr(vertices.array_operator(1 ) ).y - y1;
   dd:=vertex_dist_ptr(vertices.array_operator(1 ) ).dist - vertex_dist_ptr(vertices.array_operator(0 ) ).dist;
   d :=x^;

  end
 else
  if x^ > vertex_dist_ptr(vertices.array_operator(vertices.size - 1 ) ).dist then
   begin
    i:=vertices.size - 2;
    j:=vertices.size - 1;

    x1:=vertex_dist_ptr(vertices.array_operator(j ) ).x;
    y1:=vertex_dist_ptr(vertices.array_operator(j ) ).y;
    dx:=x1 - vertex_dist_ptr(vertices.array_operator(i ) ).x;
    dy:=y1 - vertex_dist_ptr(vertices.array_operator(i ) ).y;
    dd:=vertex_dist_ptr(vertices.array_operator(j ) ).dist - vertex_dist_ptr(vertices.array_operator(i ) ).dist;
    d :=x^ - vertex_dist_ptr(vertices.array_operator(j ) ).dist;

   end
  else
   begin
   // Interpolation
    i:=0;
    j:=vertices.size - 1;

    if m_preserve_x_scale then
     begin
      i:=0;

      while j - i > 1 do
       begin
        k:=(i + j ) shr 1;

        if x^ < vertex_dist_ptr(vertices.array_operator(k ) ).dist then
         j:=k
        else
         i:=k;

       end;

      d :=vertex_dist_ptr(vertices.array_operator(i ) ).dist;
      dd:=vertex_dist_ptr(vertices.array_operator(j ) ).dist - d;
      d :=x^ - d;

     end
    else
     begin
      i :=trunc(x^ * kindex );
      j :=i + 1;
      dd:=vertex_dist_ptr(vertices.array_operator(j ) ).dist - vertex_dist_ptr(vertices.array_operator(i ) ).dist;
      d :=((x^ * kindex ) - i ) * dd;

     end;

    x1:=vertex_dist_ptr(vertices.array_operator(i ) ).x;
    y1:=vertex_dist_ptr(vertices.array_operator(i ) ).y;
    dx:=vertex_dist_ptr(vertices.array_operator(j ) ).x - x1;
    dy:=vertex_dist_ptr(vertices.array_operator(j ) ).y - y1;

   end;

 x^:=x1 + dx * d / dd;
 y^:=y1 + dy * d / dd;

end;

END.

