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
// 05.03.2006-Milano: Unit port establishment
//
{ agg_span_image_resample.pas }
unit
 agg_span_image_resample ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_color ,
 agg_span_image_filter ,
 agg_span_interpolator_linear ,
 agg_rendering_buffer ,
 agg_span_allocator ,
 agg_image_filters ;

{ TYPES DEFINITION }
type
 span_image_resample_ptr = ^span_image_resample;
 span_image_resample = object(span_image_filter )
   m_scale_limit ,
   m_blur_x      ,
   m_blur_y      : int;

   constructor Construct(alloc : span_allocator_ptr ); overload;
   constructor Construct(
                alloc        : span_allocator_ptr;
                src          : rendering_buffer_ptr;
                back_color   : aggclr_ptr;
                interpolator : span_interpolator_ptr;
                filter       : image_filter_lut_ptr ); overload;

   function  _scale_limit : int;
   procedure scale_limit_(v : int );

   function  _blur_x : double;
   function  _blur_y : double;
   procedure blur_x_(v : double );
   procedure blur_y_(v : double );
   procedure blur_  (v : double );

  end;

 span_image_resample_affine = object(span_image_filter )
   m_rx     ,
   m_ry     ,
   m_rx_inv ,
   m_ry_inv : int;

   m_scale_limit ,
   m_blur_x      ,
   m_blur_y      : double;

   constructor Construct(alloc : span_allocator_ptr ); overload;
   constructor Construct(
                alloc        : span_allocator_ptr;
                src          : rendering_buffer_ptr;
                back_color   : aggclr_ptr;
                interpolator : span_interpolator_ptr;
                filter       : image_filter_lut_ptr ); overload;

   function  _scale_limit : int;
   procedure scale_limit_(v : int );

   function  _blur_x : double;
   function  _blur_y : double;
   procedure blur_x_(v : double );
   procedure blur_y_(v : double );
   procedure blur_  (v : double );

   procedure prepare(max_span_len : unsigned ); virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor span_image_resample.Construct(alloc : span_allocator_ptr );
begin
 inherited Construct(alloc );

 m_scale_limit:=20;

 m_blur_x:=image_subpixel_size;
 m_blur_y:=image_subpixel_size;

end;

{ CONSTRUCT }
constructor span_image_resample.Construct(
             alloc        : span_allocator_ptr;
             src          : rendering_buffer_ptr;
             back_color   : aggclr_ptr;
             interpolator : span_interpolator_ptr;
             filter       : image_filter_lut_ptr );
begin
 inherited Construct(alloc ,src ,back_color ,interpolator ,filter );

 m_scale_limit:=20;

 m_blur_x:=image_subpixel_size;
 m_blur_y:=image_subpixel_size;

end;

{ _SCALE_LIMIT }
function span_image_resample._scale_limit;
begin
 result:=m_scale_limit;

end;

{ SCALE_LIMIT_ }
procedure span_image_resample.scale_limit_;
begin
 m_scale_limit:=v;

end;

{ _BLUR_X }
function span_image_resample._blur_x;
begin
 result:=m_blur_x / image_subpixel_size;

end;

{ _BLUR_Y }
function span_image_resample._blur_y;
begin
 result:=m_blur_y / image_subpixel_size;

end;

{ BLUR_X_ }
procedure span_image_resample.blur_x_;
begin
 m_blur_x:=trunc(v * image_subpixel_size + 0.5 );

end;

{ BLUR_Y_ }
procedure span_image_resample.blur_y_;
begin
 m_blur_y:=trunc(v * image_subpixel_size + 0.5 );

end;

{ BLUR_ }
procedure span_image_resample.blur_;
begin
 m_blur_x:=trunc(v * image_subpixel_size + 0.5 );
 m_blur_y:=m_blur_x;

end;

{ CONSTRUCT }
constructor span_image_resample_affine.Construct(alloc : span_allocator_ptr );
begin
 inherited Construct(alloc );

 m_scale_limit:=200.0;

 m_blur_x:=1.0;
 m_blur_y:=1.0;

end;

{ CONSTRUCT }
constructor span_image_resample_affine.Construct(
             alloc        : span_allocator_ptr;
             src          : rendering_buffer_ptr;
             back_color   : aggclr_ptr;
             interpolator : span_interpolator_ptr;
             filter       : image_filter_lut_ptr );
begin
 inherited Construct(alloc ,src ,back_color ,interpolator ,filter );

 m_scale_limit:=200.0;

 m_blur_x:=1.0;
 m_blur_y:=1.0;

end;

{ _SCALE_LIMIT }
function span_image_resample_affine._scale_limit;
begin
 result:=trunc(m_scale_limit );

end;

{ SCALE_LIMIT_ }
procedure span_image_resample_affine.scale_limit_;
begin
 m_scale_limit:=v;

end;

{ _BLUR_X }
function span_image_resample_affine._blur_x;
begin
 result:=m_blur_x;

end;

{ _BLUR_Y }
function span_image_resample_affine._blur_y;
begin
 result:=m_blur_y;

end;

{ BLUR_X_ }
procedure span_image_resample_affine.blur_x_;
begin
 m_blur_x:=v;

end;

{ BLUR_Y_ }
procedure span_image_resample_affine.blur_y_;
begin
 m_blur_y:=v;

end;

{ BLUR_ }
procedure span_image_resample_affine.blur_;
begin
 m_blur_x:=v;
 m_blur_y:=v;

end;

{ PREPARE }
procedure span_image_resample_affine.prepare;
var
 scale_x ,scale_y : double;

begin
 inherited prepare(max_span_len );

 _interpolator._transformer.scaling_abs(@scale_x ,@scale_y );

 m_rx    :=image_subpixel_size;
 m_ry    :=image_subpixel_size;
 m_rx_inv:=image_subpixel_size;
 m_ry_inv:=image_subpixel_size;

 scale_x:=scale_x * m_blur_x;
 scale_y:=scale_y * m_blur_y;

 if scale_x * scale_y > m_scale_limit then
  begin
   scale_x:=scale_x * m_scale_limit / (scale_x * scale_y );
   scale_y:=scale_y * m_scale_limit / (scale_x * scale_y );

  end;

 if scale_x > 1.0001 then
  begin
   if scale_x > m_scale_limit then
    scale_x:=m_scale_limit;

   m_rx    :=trunc(scale_x * image_subpixel_size + 0.5 );
   m_rx_inv:=trunc(1.0 / scale_x * image_subpixel_size + 0.5);

  end;

 if scale_y > 1.0001 then
  begin
   if scale_y > m_scale_limit then
    scale_y:=m_scale_limit;

   m_ry    :=trunc(scale_y * image_subpixel_size + 0.5 );
   m_ry_inv:=trunc(1.0 / scale_y * image_subpixel_size + 0.5 );

  end;

end;

END.

