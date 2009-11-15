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
// vertex_sequence container and vertex_dist struct
//
// [Pascal Port History] -----------------------------------------------------
//
// 19.12.2005-Milano: Unit port establishment
//
{ agg_vertex_sequence.pas }
unit
 agg_vertex_sequence ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_array ;

{ TYPES DEFINITION }
type
 func_vertex_sequence = function(this ,val : pointer ) : boolean;

//----------------------------------------------------------vertex_sequence
// Modified agg::pod_deque. The data is interpreted as a sequence of vertices.
 vertex_sequence_ptr = ^vertex_sequence;
 vertex_sequence = object(pod_deque )
   func_operator_vertex_sequence : func_vertex_sequence;

   constructor Construct(entry_sz : unsigned; s_ : unsigned = 6; fovs : func_vertex_sequence = NIL );

   procedure add(val : pointer );

   procedure modify_last(val : pointer);

   procedure close(remove_flag : boolean );

  end;

// Coinciding points maximal distance (Epsilon)
const
 vertex_dist_epsilon : double = 1e-14;

//-------------------------------------------------------------vertex_dist
// Vertex (x, y) with the distance to the next one. The last vertex has
// distance between the last and the first points if the polygon is closed
// and 0.0 if it's a polyline.
type
 vertex_dist_ptr = ^vertex_dist;
 vertex_dist = record
   x ,y ,dist : double;

  end;

 vertex_dist_cmd_ptr = ^vertex_dist_cmd;
 vertex_dist_cmd = record
   x ,y ,dist : double;

   cmd : unsigned;

  end;

{ GLOBAL PROCEDURES }
 function  vertex_dist_func_operator(this ,val : vertex_dist_ptr ) : boolean;


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
uses
 agg_math ;
 
{ UNIT IMPLEMENTATION }
{ FUNC_OPERATOR_VERTEX_DIST }
function vertex_dist_func_operator;
var
 ret : boolean;

begin
 this.dist:=calc_distance(this.x ,this.y ,val.x ,val.y );

 ret:=this.dist > vertex_dist_epsilon;

 if not ret then
  this.dist:=1 / vertex_dist_epsilon;

 result:=ret; 

end;

{ CONSTRUCT }
constructor vertex_sequence.Construct;
begin
 inherited Construct(entry_sz ,s_ );

 if @fovs = NIL then
  func_operator_vertex_sequence:=@vertex_dist_func_operator
 else
  func_operator_vertex_sequence:=fovs;

end;

{ ADD }
procedure vertex_sequence.add;
begin
 if size > 1 then
  if not func_operator_vertex_sequence(
          array_operator(size - 2 ) ,
          array_operator(size - 1 ) ) then
   remove_last;
   
 inherited add(val );

end;

{ MODIFY_LAST }
procedure vertex_sequence.modify_last;
begin
 remove_last;

 add(val );

end;

{ CLOSE }
procedure vertex_sequence.close;
var
 t : pointer;

begin
 while size > 1 do
  begin
   if func_operator_vertex_sequence(
       array_operator(size - 2 ) ,
       array_operator(size - 1 ) ) then
    break;

   t:=array_operator(size - 1 );

   remove_last;
   modify_last(t );

  end;

 if remove_flag then
  while size > 1 do
   begin
    if func_operator_vertex_sequence(
        array_operator(size - 1 ) ,
        array_operator(0 ) ) then
     break;

    remove_last; 

   end;

end;

END.

