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
// 20.12.2005-Milano: Unit port establishment
//
{ agg_shorten_path.pas }
unit
 agg_shorten_path ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_vertex_sequence ;
 
{ GLOBAL PROCEDURES }
 procedure shorten_path(vs : vertex_sequence_ptr; s : double; closed : unsigned = 0 );

 
IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ SHORTEN_PATH }
procedure shorten_path;
var
 n : int;

 d ,x ,y : double;

 prev ,last : vertex_dist_ptr;

begin
 if (s > 0.0 ) and
    (vs.size > 1 ) then
  begin
   n:=vs.size - 2;

   while n <> 0 do
    begin
     d:=vertex_dist_ptr(vs.array_operator(n ) )^.dist;

     if d > s then
      break;

     vs.remove_last;

     s:=s - d;

     dec(n );

    end;

   if vs.size < 2 then
    vs.remove_all
    
   else
    begin
     n:=vs.size - 1;

     prev:=vs.array_operator(n - 1 );
     last:=vs.array_operator(n );

     d:=(prev.dist - s ) / prev.dist;

     x:=prev.x + (last.x - prev.x ) * d;
     y:=prev.y + (last.y - prev.y ) * d;
     last.x:=x;
     last.y:=y;

     if not vs.func_operator_vertex_sequence(prev ,last ) then
      vs.remove_last;
     
     vs.close(boolean(closed <> 0 ) );

    end;

  end;

end;

END.

