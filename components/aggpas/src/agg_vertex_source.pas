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
// Pascal replacement of the vertex_source templetized concept from C++.
// This file is originaly not a part of the AGG.
//
// [Pascal Port History] -----------------------------------------------------
//
// 19.12.2005-Milano: Unit port establishment
//
{ agg_vertex_source.pas }
unit
 agg_vertex_source ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ;

{ TYPES DEFINITION }
type
 vertex_source_ptr = ^vertex_source;
 vertex_source = object
   constructor Construct;
   destructor  Destruct; virtual;

   procedure remove_all; virtual;
   procedure add_vertex(x ,y : double; cmd : unsigned ); virtual;

   function  num_paths : unsigned; virtual;
   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

   function  func_operator_gamma(x : double ) : double; virtual;
   function  operator_array     (i : unsigned ) : unsigned; virtual; abstract;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor vertex_source.Construct;
begin
end;

{ DESTRUCT }
destructor vertex_source.Destruct;
begin
end;

{ REMOVE_ALL }
procedure vertex_source.remove_all;
begin
end;

{ ADD_VERTEX }
procedure vertex_source.add_vertex;
begin
end;

{ NUM_PATHS }
function vertex_source.num_paths;
begin
 result:=0;

end;

{ REWIND }
procedure vertex_source.rewind;
begin
end;

{ VERTEX }
function vertex_source.vertex;
begin
end;

{ FUNC_OPERATOR_GAMMA }
function vertex_source.func_operator_gamma;
begin
 result:=x;

end;

END.

