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
// 15.01.2006-Milano: Unit port establishment
//
{ agg_scanline.pas }
unit
 agg_scanline ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ;

{ TYPES DEFINITION }
type
 span_ptr = ^span;
 span = record
   x   ,
   len : int16;

   covers : int8u_ptr;

  end;

 span_obj_ptr = ^span_obj;
 span_obj = object
   function  x      : int; virtual;
   function  len    : int; virtual;
   function  covers : int8u_ptr; virtual;

   procedure inc_operator; virtual;

  end;

 scanline_ptr = ^scanline;
 scanline = object
   procedure reset(min_x ,max_x : int ); virtual; abstract;
   procedure reset_spans; virtual; abstract;

   procedure finalize (y_ : int ); virtual; abstract;
   procedure add_cell (x : int; cover : unsigned ); virtual; abstract;
   procedure add_cells(x : int; len : unsigned; covers : int8u_ptr ); virtual; abstract;
   procedure add_span (x : int; len ,cover : unsigned ); virtual; abstract;

   function  y : int; virtual; abstract;
   function  num_spans : unsigned; virtual; abstract;
   function  begin_ : pointer; virtual; abstract;

   function  sz_of_span : unsigned; virtual; abstract;
   function  is_plain_span : boolean; virtual;
   function  is_embedded : boolean; virtual;

   procedure init (ptr : int8u_ptr; dx ,dy : int ); virtual; abstract;
   procedure setup(scanline_idx : unsigned ); virtual; abstract;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ X }
function span_obj.x;
begin
 result:=0;

end;

{ LEN }
function span_obj.len;
begin
 result:=0;

end;

{ COVERS }
function span_obj.covers;
begin
 result:=NIL;

end;

{ INC_OPERATOR }
procedure span_obj.inc_operator;
begin
end;

{ IS_PLAIN_SPAN }
function scanline.is_plain_span;
begin
 result:=true;

end;

{ IS_EMBEDDED }
function scanline.is_embedded;
begin
 result:=false;

end;

END.

