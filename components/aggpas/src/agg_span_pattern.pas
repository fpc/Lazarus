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
// Adaptation for high precision colors has been sponsored by 
// Liberty Technology Systems, Inc., visit http://lib-sys.com
//
// Liberty Technology Systems, Inc. is the provider of
// PostScript and PDF technology for software developers.
// 
// [Pascal Port History] -----------------------------------------------------
//
// 28.02.2006-Milano: Unit port establishment
//
{ agg_span_pattern.pas }
unit
 agg_span_pattern ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }

uses
 agg_basics ,
 agg_color ,
 agg_rendering_buffer ,
 agg_span_generator ,
 agg_span_allocator ;

{ TYPES DEFINITION }
const
 base_mask = agg_color.base_mask;

type
 span_pattern_base = object(span_generator )
   m_src      : rendering_buffer_ptr;
   m_offset_x ,
   m_offset_y : unsigned;
   m_alpha    : int8u;

   constructor Construct(alloc : span_allocator_ptr ); overload;
   constructor Construct(
                alloc : span_allocator_ptr;
                src : rendering_buffer_ptr;
                offset_x ,offset_y : unsigned;
                alpha : double ); overload;

   function  _source_image : rendering_buffer_ptr;
   function  _offset_x : unsigned;
   function  _offset_y : unsigned;
   function  _alpha : double;
   function  _alpha_int : int8u;

   procedure source_image_(v : rendering_buffer_ptr );

   procedure offset_x_(v : unsigned );
   procedure offset_y_(v : unsigned );
   procedure alpha_   (v : double );

  end;

 wrap_mode_ptr = ^wrap_mode;
 wrap_mode = object
   constructor Construct;

   procedure init(size : unsigned ); virtual; abstract;

   function  func_operator(v : int ) : unsigned; virtual; abstract;
   function  inc_operator : unsigned; virtual; abstract;

  end;

 wrap_mode_repeat = object(wrap_mode )
   m_size  ,
   m_add   ,
   m_value : unsigned;

   procedure init(size : unsigned ); virtual;

   function  func_operator(v : int ) : unsigned; virtual;
   function  inc_operator : unsigned; virtual;

  end;

 wrap_mode_repeat_pow2 = object(wrap_mode )
   m_mask  ,
   m_value : unsigned;

   procedure init(size : unsigned ); virtual;

   function  func_operator(v : int ) : unsigned; virtual;
   function  inc_operator : unsigned; virtual;

  end;

 wrap_mode_repeat_auto_pow2 = object(wrap_mode )
   m_size  ,
   m_add   ,
   m_mask  ,
   m_value : unsigned;

   procedure init(size : unsigned ); virtual;

   function  func_operator(v : int ) : unsigned; virtual;
   function  inc_operator : unsigned; virtual;

  end;

 wrap_mode_reflect = object(wrap_mode )
   m_size  ,
   m_size2 ,
   m_add   ,
   m_value : unsigned;

   procedure init(size : unsigned ); virtual;

   function  func_operator(v : int ) : unsigned; virtual;
   function  inc_operator : unsigned; virtual;

  end;

 wrap_mode_reflect_pow2 = object(wrap_mode )
   m_size  ,
   m_mask  ,
   m_value : unsigned;

   procedure init(size : unsigned ); virtual;

   function  func_operator(v : int ) : unsigned; virtual;
   function  inc_operator : unsigned; virtual;

  end;

 wrap_mode_reflect_auto_pow2 = object(wrap_mode )
   m_size  ,
   m_size2 ,
   m_add   ,
   m_mask  ,
   m_value : unsigned;

   procedure init(size : unsigned ); virtual;

   function  func_operator(v : int ) : unsigned; virtual;
   function  inc_operator : unsigned; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor span_pattern_base.Construct(alloc : span_allocator_ptr );
begin
 inherited Construct(alloc );

 m_src     :=NIL;
 m_offset_x:=0;
 m_offset_y:=0;
 m_alpha   :=0;

end;

{ CONSTRUCT }
constructor span_pattern_base.Construct(
             alloc : span_allocator_ptr;
             src : rendering_buffer_ptr;
             offset_x ,offset_y : unsigned;
             alpha : double );
begin
 inherited Construct(alloc );

 m_src     :=src;
 m_offset_x:=offset_x;
 m_offset_y:=offset_y;
 m_alpha   :=int8u(trunc(alpha * base_mask ) );

end;

{ _SOURCE_IMAGE }
function span_pattern_base._source_image;
begin
 result:=m_src;

end;

{ _OFFSET_X }
function span_pattern_base._offset_x;
begin
 result:=m_offset_x;

end;

{ _OFFSET_Y }
function span_pattern_base._offset_y;
begin
 result:=m_offset_y;

end;

{ _ALPHA }
function span_pattern_base._alpha;
begin
 result:=m_alpha / base_mask;

end;

{ _ALPHA_INT }
function span_pattern_base._alpha_int;
begin
 result:=m_alpha;

end;

{ SOURCE_IMAGE_ }
procedure span_pattern_base.source_image_;
begin
 m_src:=v;

end;

{ OFFSET_X_ }
procedure span_pattern_base.offset_x_;
begin
 m_offset_x:=v;

end;

{ OFFSET_Y_ }
procedure span_pattern_base.offset_y_;
begin
 m_offset_y:=v;

end;

{ ALPHA_ }
procedure span_pattern_base.alpha_;
begin
 m_alpha:=int8u(trunc(v * base_mask ) );

end;

{ CONSTRUCT }
constructor wrap_mode.Construct;
begin
end;

{ INIT }
procedure wrap_mode_repeat.init;
begin
 m_size :=size;
 m_add  :=size * ($3FFFFFFF div size );
 m_value:=0;

end;

{ FUNC_OPERATOR }
function wrap_mode_repeat.func_operator;
begin
 m_value:=(unsigned(v ) + m_add ) mod m_size;
 result :=m_value;

end;

{ INC_OPERATOR }
function wrap_mode_repeat.inc_operator;
begin
 inc(m_value );

 if m_value >= m_size then
  m_value:=0;

 result:=m_value;

end;

{ INIT }
procedure wrap_mode_repeat_pow2.init;
begin
 m_mask:=1;

 while m_mask < size do
  m_mask:=(m_mask shl 1 ) or 1;

 m_mask:=m_mask shr 1;

end;

{ FUNC_OPERATOR }
function wrap_mode_repeat_pow2.func_operator;
begin
 m_value:=unsigned(v ) and m_mask;
 result :=m_value;

end;

{ INC_OPERATOR }
function wrap_mode_repeat_pow2.inc_operator;
begin
 inc(m_value );

 if m_value > m_mask then
  m_value:=0;

 result:=m_value;

end;

{ INIT }
procedure wrap_mode_repeat_auto_pow2.init;
begin
 m_size:=size;
 m_add :=size * ($3FFFFFFF div size );

 if m_size and (m_size - 1 ) <> 0 then
  m_mask:=0
 else
  m_mask:=m_size - 1;

 m_value:=0;

end;

{ FUNC_OPERATOR }
function wrap_mode_repeat_auto_pow2.func_operator;
begin
 if m_mask <> 0 then
  begin
   m_value:=unsigned(unsigned(v ) and m_mask );
   result :=m_value;

  end
 else
  begin
   m_value:=unsigned((unsigned(v ) + m_add ) mod m_size );
   result :=m_value;

  end;

end;

{ INC_OPERATOR }
function wrap_mode_repeat_auto_pow2.inc_operator;
begin
 inc(m_value );

 if m_value >= m_size then
  m_value:=0;

 result:=m_value;

end;

{ INIT }
procedure wrap_mode_reflect.init;
begin
 m_size :=size;
 m_size2:=size * 2;
 m_add  :=m_size2 * ($3FFFFFFF div m_size2 );
 m_value:=0;

end;

{ FUNC_OPERATOR }
function wrap_mode_reflect.func_operator;
begin
 m_value:=(unsigned(v ) + m_add ) mod m_size2;

 if m_value >= m_size then
  result:=m_size2 - m_value - 1
 else
  result:=m_value;

end;

{ INC_OPERATOR }
function wrap_mode_reflect.inc_operator;
begin
 inc(m_value );

 if m_value >= m_size2 then
  m_value:=0;

 if m_value >= m_size then
  result:=m_size2 - m_value - 1
 else
  result:=m_value;

end;

{ INIT }
procedure wrap_mode_reflect_pow2.init;
begin
 m_mask:=1;
 m_size:=1;

 while m_mask < size do
  begin
   m_mask:=(m_mask shl 1 ) or 1;
   m_size:=m_size shl 1;

  end;

end;

{ FUNC_OPERATOR }
function wrap_mode_reflect_pow2.func_operator;
begin
 m_value:=unsigned(v ) and m_mask;

 if m_value >= m_size then
  result:=m_mask - m_value
 else
  result:=m_value;

end;

{ INC_OPERATOR }
function wrap_mode_reflect_pow2.inc_operator;
begin
 inc(m_value );

 m_value:=m_value and m_mask;

 if m_value >= m_size then
  result:=m_mask - m_value
 else
  result:=m_value;

end;

{ INIT }
procedure wrap_mode_reflect_auto_pow2.init;
begin
 m_size :=size;
 m_size2:=size * 2;
 m_add  :=m_size2 * ($3FFFFFFF div m_size2 );

 if m_size2 and (m_size2 - 1 ) <> 0 then
  m_mask:=0
 else
  m_mask:=m_size2 - 1;

 m_value:=0;

end;

{ FUNC_OPERATOR }
function wrap_mode_reflect_auto_pow2.func_operator;
begin
 if m_mask <> 0 then
  m_value:=unsigned(v ) and m_mask
 else
  m_value:=(unsigned(v ) + m_add ) mod m_size2;

 if m_value >= m_size then
  result:=m_size2 - m_value - 1
 else
  result:=m_value;

end;

{ INC_OPERATOR }
function wrap_mode_reflect_auto_pow2.inc_operator;
begin
 inc(m_value );

 if m_value >= m_size2 then
  m_value:=0;

 if m_value >= m_size then
  result:=m_size2 - m_value - 1
 else
  result:=m_value;

end;

END.

