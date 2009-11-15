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
// 23.06.2006-Milano: ptrcomp adjustments
// 06.03.2006-Milano: Unit port establishment
//
{ agg_span_pattern_resample_gray.pas {untested}
unit
 agg_span_pattern_resample_gray ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
 agg_basics ,
 agg_color ,
 agg_span_pattern ,
 agg_span_image_resample ,
 agg_span_interpolator_linear ,
 agg_rendering_buffer ,
 agg_span_allocator ,
 agg_image_filters ;

{ TYPES DEFINITION }
const
 base_shift      = agg_color.base_shift;
 base_mask       = agg_color.base_mask;
 downscale_shift = image_filter_shift;

type
 span_pattern_resample_gray_affine_ptr = ^span_pattern_resample_gray_affine;
 span_pattern_resample_gray_affine = object(span_image_resample_affine )
   m_wrap_mode_x ,
   m_wrap_mode_y : wrap_mode_ptr;

   constructor Construct(alloc : span_allocator_ptr; WX ,WY : wrap_mode_ptr ); overload;
   constructor Construct(
                alloc : span_allocator_ptr;
                src : rendering_buffer_ptr;
                interpolator : span_interpolator_ptr;
                filter : image_filter_lut_ptr;
                WX ,WY : wrap_mode_ptr ); overload;

   procedure source_image_(src : rendering_buffer_ptr );

   function  generate(x ,y : int; len : unsigned ) : aggclr_ptr; virtual;

  end;

 span_pattern_resample_gray_ptr = ^span_pattern_resample_gray; 
 span_pattern_resample_gray = object(span_image_resample )
   m_wrap_mode_x ,
   m_wrap_mode_y : wrap_mode_ptr;

   constructor Construct(alloc : span_allocator_ptr; WX ,WY : wrap_mode_ptr ); overload;
   constructor Construct(
                alloc : span_allocator_ptr;
                src : rendering_buffer_ptr;
                interpolator : span_interpolator_ptr;
                filter : image_filter_lut_ptr;
                WX ,WY : wrap_mode_ptr ); overload;

   procedure source_image_(src : rendering_buffer_ptr );

   function  generate(x ,y : int; len : unsigned ) : aggclr_ptr; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor span_pattern_resample_gray_affine.Construct(alloc : span_allocator_ptr; WX ,WY : wrap_mode_ptr );
begin
 inherited Construct(alloc );

 m_wrap_mode_x:=WX;
 m_wrap_mode_y:=WY;

 m_wrap_mode_x.init(1 );
 m_wrap_mode_y.init(1 );

end;

{ CONSTRUCT }
constructor span_pattern_resample_gray_affine.Construct(
             alloc : span_allocator_ptr;
             src : rendering_buffer_ptr;
             interpolator : span_interpolator_ptr;
             filter : image_filter_lut_ptr;
             WX ,WY : wrap_mode_ptr );
var
 rgba : aggclr;

begin
 rgba.ConstrInt(0 ,0 );

 inherited Construct(alloc ,src ,@rgba ,interpolator ,filter );

 m_wrap_mode_x:=WX;
 m_wrap_mode_y:=WY;

 m_wrap_mode_x.init(src._width );
 m_wrap_mode_y.init(src._height );

end;

{ SOURCE_IMAGE_ }
procedure span_pattern_resample_gray_affine.source_image_;
begin
 inherited source_image_(src );

 m_wrap_mode_x.init(src._width );
 m_wrap_mode_y.init(src._height );

end;

{ GENERATE }
function span_pattern_resample_gray_affine.generate;
var
 span : aggclr_ptr;
 intr : span_interpolator_ptr;

 fg ,diameter ,filter_size ,radius_x ,radius_y ,maxx ,maxy ,y_lr ,y_hr ,
 total_weight ,x_lr_ini ,x_hr_ini ,weight_y ,x_lr ,x_hr ,weight : int;

 row_ptr ,fg_ptr : int8u_ptr;

 weight_array : int16_ptr;

begin
 span:=_allocator.span;
 intr:=_interpolator;

 intr.begin_(x + filter_dx_dbl ,y + filter_dy_dbl ,len );

 diameter   :=_filter.diameter;
 filter_size:=diameter shl image_subpixel_shift;

 radius_x:=shr_int32(diameter * m_rx ,1 );
 radius_y:=shr_int32(diameter * m_ry ,1 );

 maxx:=_source_image._width - 1;
 maxy:=_source_image._height - 1;

 weight_array:=_filter.weight_array;

 repeat
  intr.coordinates(@x ,@y );

  inc(x ,filter_dx_int - radius_x );
  inc(y ,filter_dy_int - radius_y );

  fg:=image_filter_size div 2;

  y_lr:=m_wrap_mode_y.func_operator(shr_int32(y ,image_subpixel_shift ) );
  y_hr:=
   shr_int32(
    (image_subpixel_mask - (y and image_subpixel_mask ) ) *
    m_ry_inv ,image_subpixel_shift );

  total_weight:=0;

  x_lr_ini:=shr_int32(x ,image_subpixel_shift );
  x_hr_ini:=
   shr_int32(
    (image_subpixel_mask - (x and image_subpixel_mask ) ) *
    m_rx_inv ,image_subpixel_shift );

  repeat
   weight_y:=int16_ptr(ptrcomp(weight_array ) + y_hr * sizeof(int16 ) )^;

   x_lr:=m_wrap_mode_x.func_operator(x_lr_ini );
   x_hr:=x_hr_ini;

   row_ptr:=_source_image.row(y_lr );

   repeat
    fg_ptr:=int8u_ptr(ptrcomp(row_ptr ) + x_lr * sizeof(int8u ) );
    weight:=
     shr_int32(
      weight_y *
      int16_ptr(ptrcomp(weight_array ) + x_hr * sizeof(int16 ) )^ +
      image_filter_size div 2 ,downscale_shift );

    inc(fg ,fg_ptr^ * weight );
    inc(total_weight ,weight );
    inc(x_hr ,m_rx_inv );

    x_lr:=m_wrap_mode_x.inc_operator;

   until x_hr >= filter_size;

   inc(y_hr ,m_ry_inv );

   y_lr:=m_wrap_mode_y.inc_operator;

  until y_hr >= filter_size;

  fg:=fg div total_weight;

  if fg < 0 then
   fg:=0;

  if fg > base_mask then
   fg:=base_mask;

  span.v:=int8u(fg );
  span.a:=int8u(base_mask );

  inc(ptrcomp(span ) ,sizeof(aggclr ) );

  intr.inc_operator;

  dec(len );

 until len = 0;

 result:=_allocator.span;

end;

{ CONSTRUCT }
constructor span_pattern_resample_gray.Construct(alloc : span_allocator_ptr; WX ,WY : wrap_mode_ptr );
begin
 inherited Construct(alloc );

 m_wrap_mode_x:=WX;
 m_wrap_mode_y:=WY;

 m_wrap_mode_x.init(1 );
 m_wrap_mode_y.init(1 );

end;

{ CONSTRUCT }
constructor span_pattern_resample_gray.Construct(
             alloc : span_allocator_ptr;
             src : rendering_buffer_ptr;
             interpolator : span_interpolator_ptr;
             filter : image_filter_lut_ptr;
             WX ,WY : wrap_mode_ptr );
var
 rgba : aggclr;

begin
 rgba.ConstrInt(0 ,0 );

 inherited Construct(alloc ,src ,@rgba ,interpolator ,filter );

 m_wrap_mode_x:=WX;
 m_wrap_mode_y:=WY;

 m_wrap_mode_x.init(src._width );
 m_wrap_mode_y.init(src._height );

end;

{ SOURCE_IMAGE_ }
procedure span_pattern_resample_gray.source_image_;
begin
 inherited source_image_(src );

 m_wrap_mode_x.init(src._width );
 m_wrap_mode_y.init(src._height );

end;

{ GENERATE }
function span_pattern_resample_gray.generate;
var
 span : aggclr_ptr;
 intr : span_interpolator_ptr;

 fg ,diameter ,filter_size ,rx ,ry ,rx_inv ,ry_inv ,radius_x ,radius_y ,
 maxx ,maxy ,y_lr ,y_hr ,total_weight ,x_lr_ini ,x_hr_ini ,weight_y ,
 x_lr ,x_hr ,weight : int;

 row_ptr ,fg_ptr : int8u_ptr;

 weight_array : int16_ptr;

begin
 span:=_allocator.span;
 intr:=_interpolator;

 intr.begin_(x + filter_dx_dbl ,y + filter_dy_dbl ,len );

 diameter    :=_filter.diameter;
 filter_size :=diameter shl image_subpixel_shift;
 weight_array:=_filter.weight_array;

 repeat
  rx_inv:=image_subpixel_size;
  ry_inv:=image_subpixel_size;

  intr.coordinates(@x ,@y );
  intr.local_scale(@rx ,@ry );

  rx:=shr_int32(rx * m_blur_x ,image_subpixel_shift );
  ry:=shr_int32(ry * m_blur_y ,image_subpixel_shift );

  if rx < image_subpixel_size then
   rx:=image_subpixel_size
  else
   begin
    if rx > image_subpixel_size * m_scale_limit then
     rx:=image_subpixel_size * m_scale_limit;

    rx_inv:=image_subpixel_size * image_subpixel_size div rx;

   end;

  if ry < image_subpixel_size then
   ry:=image_subpixel_size
  else
   begin
    if ry > image_subpixel_size * m_scale_limit then
     ry:=image_subpixel_size * m_scale_limit;

    ry_inv:=image_subpixel_size * image_subpixel_size div ry;

   end;

  radius_x:=shr_int32(diameter * rx ,1 );
  radius_y:=shr_int32(diameter * ry ,1 );

  maxx:=_source_image._width - 1;
  maxy:=_source_image._height - 1;

  inc(x ,filter_dx_int - radius_x );
  inc(y ,filter_dy_int - radius_y );

  fg:=image_filter_size div 2;

  y_lr:=m_wrap_mode_y.func_operator(shr_int32(y ,image_subpixel_shift ) );
  y_hr:=
   shr_int32(
    (image_subpixel_mask - (y and image_subpixel_mask ) ) *
    ry_inv ,image_subpixel_shift );

  total_weight:=0;

  x_lr_ini:=shr_int32(x ,image_subpixel_shift );
  x_hr_ini:=
   shr_int32(
    (image_subpixel_mask - (x and image_subpixel_mask ) ) *
    rx_inv ,image_subpixel_shift );

  repeat
   weight_y:=int16_ptr(ptrcomp(weight_array ) + y_hr * sizeof(int16 ) )^;

   x_lr:=m_wrap_mode_x.func_operator(x_lr_ini );
   x_hr:=x_hr_ini;

   row_ptr:=_source_image.row(y_lr );

   repeat
    fg_ptr:=int8u_ptr(ptrcomp(row_ptr ) + x_lr * sizeof(int8u ) );
    weight:=
     shr_int32(
      weight_y *
      int16_ptr(ptrcomp(weight_array ) + x_hr * sizeof(int16 ) )^ +
      image_filter_size div 2 ,downscale_shift );

    inc(fg ,fg_ptr^ * weight );
    inc(total_weight ,weight );
    inc(x_hr ,rx_inv );

    x_lr:=m_wrap_mode_x.inc_operator;

   until x_hr > filter_size;

   inc(y_hr ,ry_inv );

   y_lr:=m_wrap_mode_y.inc_operator;

  until y_hr >= filter_size;

  fg:=fg div total_weight;

  if fg < 0 then
   fg:=0;

  if fg > base_mask then
   fg:=base_mask;

  span.v:=int8u(fg );
  span.a:=int8u(base_mask );

  inc(ptrcomp(span ) ,sizeof(aggclr ) );

  intr.inc_operator;

  dec(len );

 until len = 0;

 result:=_allocator.span;

end;

END.

