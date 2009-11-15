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
// 13.02.2006-Milano: Unit port establishment
//
{ agg_trans_single_path.pas }
unit
 agg_trans_single_path ;

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

 trans_single_path_ptr = ^trans_single_path;
 trans_single_path = object(trans_affine )
   m_src_vertices : vertex_sequence;
   m_base_length  ,
   m_kindex       : double;

   m_status : status_e;

   m_preserve_x_scale : boolean;

   constructor Construct;
   destructor  Destruct;

   procedure base_length_(v : double );
   function  _base_length : double;

   procedure preserve_x_scale_(f : boolean );
   function  _preserve_x_scale : boolean;

   procedure reset; virtual;

   procedure move_to(x ,y : double );
   procedure line_to(x ,y : double );
   procedure finalize_path;

   procedure add_path(vs : vertex_source_ptr; path_id : unsigned = 0 );

   function  total_length : double;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ _transform }
procedure _transform(this : trans_single_path_ptr; x ,y : double_ptr );
var
 x1 ,y1 ,dx ,dy ,d ,dd ,x2 ,y2 : double;

 i ,j ,k : unsigned;

begin
 if this.m_status = ready then
  begin
   if this.m_base_length > 1e-10 then
    x^:=
     x^ *
     (vertex_dist_ptr(this.m_src_vertices.array_operator(this.m_src_vertices.size - 1 ) ).dist /
      this.m_base_length );

   x1:=0.0;
   y1:=0.0;
   dx:=1.0;
   dy:=1.0;
   d :=0.0;
   dd:=1.0;

   if x^ < 0.0 then
    begin
    // Extrapolation on the left
     x1:=vertex_dist_ptr(this.m_src_vertices.array_operator(0 ) ).x;
     y1:=vertex_dist_ptr(this.m_src_vertices.array_operator(0 ) ).y;
     dx:=vertex_dist_ptr(this.m_src_vertices.array_operator(1 ) ).x - x1;
     dy:=vertex_dist_ptr(this.m_src_vertices.array_operator(1 ) ).y - y1;

     dd:=
      vertex_dist_ptr(this.m_src_vertices.array_operator(1 ) ).dist -
      vertex_dist_ptr(this.m_src_vertices.array_operator(0 ) ).dist;

     d:=x^;

    end
   else
    if x^ > vertex_dist_ptr(this.m_src_vertices.array_operator(this.m_src_vertices.size - 1 ) ).dist then
     begin
     // Extrapolation on the right
      i:=this.m_src_vertices.size - 2;
      j:=this.m_src_vertices.size - 1;

      x1:=vertex_dist_ptr(this.m_src_vertices.array_operator(j ) ).x;
      y1:=vertex_dist_ptr(this.m_src_vertices.array_operator(j ) ).y;
      dx:=x1 - vertex_dist_ptr(this.m_src_vertices.array_operator(i ) ).x;
      dy:=y1 - vertex_dist_ptr(this.m_src_vertices.array_operator(i ) ).y;

      dd:=
       vertex_dist_ptr(this.m_src_vertices.array_operator(j ) ).dist -
       vertex_dist_ptr(this.m_src_vertices.array_operator(i ) ).dist;

      d:=x^ - vertex_dist_ptr(this.m_src_vertices.array_operator(j ) ).dist;

     end
    else
     begin
     // Interpolation
      i:=0;
      j:=this.m_src_vertices.size - 1;

      if this.m_preserve_x_scale then
       begin
        i:=0;

        while j - i > 1 do
         begin
          k:=(i + j ) shr 1;

          if x^ < vertex_dist_ptr(this.m_src_vertices.array_operator(k ) ).dist then
           j:=k
          else
           i:=k;

         end;

        d :=vertex_dist_ptr(this.m_src_vertices.array_operator(i ) ).dist;
        dd:=vertex_dist_ptr(this.m_src_vertices.array_operator(j ) ).dist - d;
        d :=x^ - d;

       end
      else
       begin
        i:=trunc(x^ * this.m_kindex );
        j:=i + 1;

        dd:=
         vertex_dist_ptr(this.m_src_vertices.array_operator(j ) ).dist -
         vertex_dist_ptr(this.m_src_vertices.array_operator(i ) ).dist;

        d:=((x^ * this.m_kindex ) - i ) * dd;

       end;

      x1:=vertex_dist_ptr(this.m_src_vertices.array_operator(i ) ).x;
      y1:=vertex_dist_ptr(this.m_src_vertices.array_operator(i ) ).y;
      dx:=vertex_dist_ptr(this.m_src_vertices.array_operator(j ) ).x - x1;
      dy:=vertex_dist_ptr(this.m_src_vertices.array_operator(j ) ).y - y1;

     end;

   x2:=x1 + dx * d / dd;
   y2:=y1 + dy * d / dd;
   x^:=x2 - y^ * dy / dd;
   y^:=y2 + y^ * dx / dd;

  end;

end;

{ CONSTRUCT }
constructor trans_single_path.Construct;
begin
 inherited Construct;

 transform:=@_transform;

 m_src_vertices.Construct(sizeof(vertex_dist ) );

 m_base_length:=0.0;
 m_kindex     :=0.0;

 m_status:=initial;

 m_preserve_x_scale:=true;

end;

{ DESTRUCT }
destructor trans_single_path.Destruct;
begin
 m_src_vertices.Destruct;

end;

{ BASE_LENGTH_ }
procedure trans_single_path.base_length_;
begin
 m_base_length:=v;

end;

{ _BASE_LENGTH }
function trans_single_path._base_length;
begin
 result:=m_base_length;

end;

{ PRESERVE_X_SCALE_ }
procedure trans_single_path.preserve_x_scale_;
begin
 m_preserve_x_scale:=f;

end;

{ _PRESERVE_X_SCALE }
function trans_single_path._preserve_x_scale;
begin
 result:=m_preserve_x_scale;

end;

{ RESET }
procedure trans_single_path.reset;
begin
 m_src_vertices.remove_all;

 m_kindex:=0.0;
 m_status:=initial;

end;

{ MOVE_TO }
procedure trans_single_path.move_to;
var
 vd : vertex_dist;

begin
 if m_status = initial then
  begin
   vd.x:=x;
   vd.y:=y;

   vd.dist:=0;

   m_src_vertices.modify_last(@vd );

   m_status:=making_path;

  end
 else
  line_to(x ,y );

end;

{ LINE_TO }
procedure trans_single_path.line_to;
var
 vd : vertex_dist;

begin
 if m_status = making_path then
  begin
   vd.x:=x;
   vd.y:=y;

   vd.dist:=0;

   m_src_vertices.add(@vd );

  end;

end;

{ FINALIZE_PATH }
procedure trans_single_path.finalize_path;
var
 i : unsigned;
 v : vertex_dist_ptr;

 dist ,d : double;

begin
 if (m_status = making_path ) and
    (m_src_vertices.size > 1 ) then
  begin
   m_src_vertices.close(false );

   if m_src_vertices.size > 2 then
    if vertex_dist_ptr(m_src_vertices.array_operator(m_src_vertices.size - 2 ) ).dist * 10.0 <
       vertex_dist_ptr(m_src_vertices.array_operator(m_src_vertices.size - 3 ) ).dist then
     begin
      d:=
       vertex_dist_ptr(m_src_vertices.array_operator(m_src_vertices.size - 3 ) ).dist +
       vertex_dist_ptr(m_src_vertices.array_operator(m_src_vertices.size - 2 ) ).dist;

      move(
       m_src_vertices.array_operator(m_src_vertices.size - 1 )^ ,
       m_src_vertices.array_operator(m_src_vertices.size - 2 )^ ,
       sizeof(vertex_dist ) );

      m_src_vertices.remove_last;

      vertex_dist_ptr(m_src_vertices.array_operator(m_src_vertices.size - 2 ) ).dist:=d;

     end;

   dist:=0.0;

   for i:=0 to m_src_vertices.size - 1 do
    begin
     v:=m_src_vertices.array_operator(i );
     d:=v.dist;

     v.dist:=dist;
     dist  :=dist + d;

    end;

   m_kindex:=(m_src_vertices.size - 1 ) / dist;
   m_status:=ready;

  end;

end;

{ ADD_PATH }
procedure trans_single_path.add_path;
var
 x ,y : double;
 cmd  : unsigned;

begin
 vs.rewind(path_id );

 cmd:=vs.vertex(@x ,@y );

 while not is_stop(cmd ) do
  begin
   if is_move_to(cmd ) then
    move_to(x ,y )
   else
    if is_vertex(cmd ) then
     line_to(x ,y );

   cmd:=vs.vertex(@x ,@y );

  end;

 finalize_path;

end;

{ TOTAL_LENGTH }
function trans_single_path.total_length;
begin
 if m_base_length >= 1e-10 then
  begin
   result:=m_base_length;

   exit;

  end;

 if m_status = ready then
  result:=vertex_dist_ptr(m_src_vertices.array_operator(m_src_vertices.size - 1 ) ).dist
 else
  result:=0.0

end;

END.

