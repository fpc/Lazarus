//----------------------------------------------------------------------------
// Anti-Grain Geometry - Version 2.4 (Public License)
// Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)
//
// Anti-Grain Geometry - Version 2.4 Release Milano 3 (AggPas 2.4 RM3)
// Pascal Port By: Milan Marusinec alias Milano
//                 milan@marusinec.sk
//                 http://www.aggpas.org
// Copyright (c) 2005-2007
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
// class rendering_buffer_dynarow
//
// [Pascal Port History] -----------------------------------------------------
//
// 06.10.2007-Milano: Unit port establishment
//
{ agg_rendering_buffer_dynarow.pas }
unit
 agg_rendering_buffer_dynarow ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_rendering_buffer ;

{ TYPES DEFINITION }
type
//===============================================rendering_buffer_dynarow
// Rendering buffer class with dynamic allocation of the rows.
// The rows are allocated as needed when requesting for span_ptr().
// The class automatically calculates min_x and max_x for each row.
// Generally it's more efficient to use this class as a temporary buffer
// for rendering a few lines and then to blend it with another buffer.
 rendering_buffer_dynarow_ptr = ^rendering_buffer_dynarow;
 rendering_buffer_dynarow = object(rendering_buffer )
  private
   m_buff   : row_data_type_ptr; // Pointers to each row of the buffer
   m_alloc  ,

   m_byte_width : unsigned;      // Width in bytes

  public
   constructor Construct(width ,height ,byte_width : unsigned );
   destructor  Destruct; virtual;

   procedure init(width ,height ,byte_width : unsigned );

   function  _width : unsigned;
   function  _height : unsigned;
   function  _byte_width : unsigned;

   function  row_xy(x ,y : int; len : unsigned ) : int8u_ptr; virtual;
   function  row   (y : unsigned ) : int8u_ptr; virtual;

  end;

{ GLOBAL VARIABLES & CONSTANTS }
{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
// Allocate and clear the buffer
constructor rendering_buffer_dynarow.Construct(width ,height ,byte_width : unsigned );
begin
 m_alloc:=sizeof(row_data_type ) * height;

 agg_getmem(pointer(m_buff ) ,m_alloc );

 m_width :=width;
 m_height:=height;

 m_byte_width:=byte_width;

 FillChar(m_buff^ ,m_alloc ,0 );

end;

{ DESTRUCT }
destructor rendering_buffer_dynarow.Destruct;
begin
 init(0 ,0 ,0 );

end;

{ INIT }
// Allocate and clear the buffer
procedure rendering_buffer_dynarow.init(width ,height ,byte_width : unsigned );
var
 i : unsigned;

begin
 i:=0;

 while i < m_height do
  begin
   agg_freemem(
    pointer(
     row_data_type_ptr(ptrcomp(m_buff ) + i * sizeof(row_data_type ) ).ptr ) ,
     m_byte_width );

   inc(i );

  end;

 agg_freemem(pointer(m_buff ) ,m_alloc );

 m_buff:=NIL;

 if (width <> 0 ) and
    (height <> 0 ) then
  begin
   m_width :=width;
   m_height:=height;

   m_byte_width:=byte_width;

   m_alloc:=sizeof(row_data_type ) * height;

   agg_getmem(pointer(m_buff ) ,m_alloc );
   FillChar  (m_buff^ ,m_alloc ,0 );

  end;

end;

{ _WIDTH }
function rendering_buffer_dynarow._width : unsigned;
begin
 result:=m_width;

end;

{ _HEIGHT }
function rendering_buffer_dynarow._height : unsigned;
begin
 result:=m_height;

end;

{ _BYTE_WIDTH }
function rendering_buffer_dynarow._byte_width : unsigned;
begin
 result:=m_byte_width;

end;

{ ROW_XY }
// The main function used for rendering. Returns pointer to the
// pre-allocated span. Memory for the row is allocated as needed.
function rendering_buffer_dynarow.row_xy(x ,y : int; len : unsigned ) : int8u_ptr;
var
 r : row_data_type_ptr;
 p : int8u_ptr;

 x2 : int;

begin
 r :=row_data_type_ptr(ptrcomp(m_buff ) + y * sizeof(row_data_type ) );
 x2:=x + len - 1;

 if r.ptr <> NIL then
  begin
   if x  < r.x1 then
    r.x1:=x;

   if x2 > r.x2 then
    r.x2:=x2;

  end
 else
  begin
   agg_getmem(pointer(p ) ,m_byte_width );

   r.ptr:=p;
   r.x1 :=x;
   r.x2 :=x2;

   FillChar(p^ ,m_byte_width ,0 );

  end;

 result:=r.ptr;

end;

{ ROW }
function rendering_buffer_dynarow.row(y : unsigned ) : int8u_ptr;
begin
 result:=row_xy(0 ,y ,m_width );

end;

END.

