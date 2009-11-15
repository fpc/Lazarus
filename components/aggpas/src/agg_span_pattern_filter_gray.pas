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
// classes span_pattern_filter_gray*
//
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 06.03.2006-Milano: Unit port establishment
//
{ agg_span_pattern_filter_gray.pas {untested}
unit
 agg_span_pattern_filter_gray ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
 agg_basics ,
 agg_color ,
 agg_span_pattern ,
 agg_span_image_filter ,
 agg_span_interpolator_linear ,
 agg_rendering_buffer ,
 agg_span_allocator ,
 agg_image_filters ;

{ TYPES DEFINITION }
const
 base_shift = agg_color.base_shift;
 base_mask  = agg_color.base_mask;

type
 span_pattern_filter_gray_nn_ptr = ^span_pattern_filter_gray_nn;
 span_pattern_filter_gray_nn = object(span_image_filter )
   m_wrap_mode_x ,
   m_wrap_mode_y : wrap_mode_ptr;

   constructor Construct(alloc : span_allocator_ptr; WX ,WY : wrap_mode_ptr ); overload;
   constructor Construct(
                alloc : span_allocator_ptr;
                src : rendering_buffer_ptr;
                interpolator : span_interpolator_ptr;
                WX ,WY : wrap_mode_ptr ); overload;

   procedure source_image_(src : rendering_buffer_ptr );

   function  generate(x ,y : int; len : unsigned ) : aggclr_ptr; virtual;

  end;

 span_pattern_filter_gray_bilinear_ptr = ^span_pattern_filter_gray_bilinear;
 span_pattern_filter_gray_bilinear = object(span_image_filter )
   m_wrap_mode_x ,
   m_wrap_mode_y : wrap_mode_ptr;

   constructor Construct(alloc : span_allocator_ptr; WX ,WY : wrap_mode_ptr ); overload;
   constructor Construct(
                alloc : span_allocator_ptr;
                src : rendering_buffer_ptr;
                interpolator : span_interpolator_ptr;
                WX ,WY : wrap_mode_ptr ); overload;

   procedure source_image_(src : rendering_buffer_ptr );

   function  generate(x ,y : int; len : unsigned ) : aggclr_ptr; virtual;

  end;

 span_pattern_filter_gray_2x2_ptr = ^span_pattern_filter_gray_2x2; 
 span_pattern_filter_gray_2x2 = object(span_image_filter )
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

 span_pattern_filter_gray_ptr = ^span_pattern_filter_gray; 
 span_pattern_filter_gray = object(span_image_filter )
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
constructor span_pattern_filter_gray_nn.Construct(alloc : span_allocator_ptr; WX ,WY : wrap_mode_ptr );
begin
 inherited Construct(alloc );

 m_wrap_mode_x:=WX;
 m_wrap_mode_y:=WY;

 m_wrap_mode_x.init(1 );
 m_wrap_mode_y.init(1 );

end;

{ CONSTRUCT }
constructor span_pattern_filter_gray_nn.Construct(
             alloc : span_allocator_ptr;
             src : rendering_buffer_ptr;
             interpolator : span_interpolator_ptr;
             WX ,WY : wrap_mode_ptr );
var
 rgba : aggclr;

begin
 rgba.ConstrInt(0 ,0 );

 inherited Construct(alloc ,src ,@rgba ,interpolator ,NIL );

 m_wrap_mode_x:=WX;
 m_wrap_mode_y:=WY;

 m_wrap_mode_x.init(src._width );
 m_wrap_mode_y.init(src._height );

end;

{ SOURCE_IMAGE_ }
procedure span_pattern_filter_gray_nn.source_image_;
begin
 inherited source_image_(src );

 m_wrap_mode_x.init(src._width );
 m_wrap_mode_y.init(src._height );

end;

{ GENERATE }
function span_pattern_filter_gray_nn.generate;
var
 span : aggclr_ptr;

begin
 _interpolator.begin_(x + filter_dx_dbl ,y + filter_dy_dbl ,len );

 span:=_allocator.span;

 repeat
  _interpolator.coordinates(@x ,@y );

  x:=m_wrap_mode_x.func_operator(shr_int32(x ,image_subpixel_shift ) );
  y:=m_wrap_mode_y.func_operator(shr_int32(y ,image_subpixel_shift ) );

  span.v:=int8u_ptr(ptrcomp(_source_image.row(y ) ) + x * sizeof(int8u ) )^;
  span.a:=base_mask;

  inc(ptrcomp(span ) ,sizeof(aggclr ) );

  _interpolator.inc_operator;

  dec(len );

 until len = 0;

 result:=_allocator.span;

end;

{ CONSTRUCT }
constructor span_pattern_filter_gray_bilinear.Construct(alloc : span_allocator_ptr; WX ,WY : wrap_mode_ptr );
begin
 inherited Construct(alloc );

 m_wrap_mode_x:=WX;
 m_wrap_mode_y:=WY;

 m_wrap_mode_x.init(1 );
 m_wrap_mode_y.init(1 );

end;

{ CONSTRUCT }
constructor span_pattern_filter_gray_bilinear.Construct(
             alloc : span_allocator_ptr;
             src : rendering_buffer_ptr;
             interpolator : span_interpolator_ptr;
             WX ,WY : wrap_mode_ptr );
var
 rgba : aggclr;

begin
 rgba.ConstrInt(0 ,0 );

 inherited Construct(alloc ,src ,@rgba ,interpolator ,NIL );

 m_wrap_mode_x:=WX;
 m_wrap_mode_y:=WY;

 m_wrap_mode_x.init(src._width );
 m_wrap_mode_y.init(src._height );

end;

{ SOURCE_IMAGE_ }
procedure span_pattern_filter_gray_bilinear.source_image_;
begin
 inherited source_image_(src );

 m_wrap_mode_x.init(src._width );
 m_wrap_mode_y.init(src._height );

end;

{ GENERATE }
function span_pattern_filter_gray_bilinear.generate;
var
 fg ,x1 ,x2 ,y1 ,y2 : unsigned;

 x_hr ,y_hr ,x_lr ,y_lr : int;

 span : aggclr_ptr;

 ptr1 ,ptr2 : int8u_ptr;

begin
 _interpolator.begin_(x + filter_dx_dbl ,y + filter_dy_dbl ,len );

 span:=_allocator.span;

 repeat
  _interpolator.coordinates(@x_hr ,@y_hr );

  dec(x_hr ,filter_dx_int );
  dec(y_hr ,filter_dy_int );

  x_lr:=shr_int32(x_hr ,image_subpixel_shift );
  y_lr:=shr_int32(y_hr ,image_subpixel_shift );

  x1:=m_wrap_mode_x.func_operator(x_lr );
  x2:=m_wrap_mode_x.inc_operator;

  y1:=m_wrap_mode_y.func_operator(y_lr );
  y2:=m_wrap_mode_y.inc_operator;

  ptr1:=_source_image.row(y1 );
  ptr2:=_source_image.row(y2 );

  fg:=image_subpixel_size * image_subpixel_size div 2;

  x_hr:=x_hr and image_subpixel_mask;
  y_hr:=y_hr and image_subpixel_mask;

  inc(fg ,int8u_ptr(ptrcomp(ptr1 ) + x1 * sizeof(int8u ) )^ * (image_subpixel_size - x_hr ) * (image_subpixel_size - y_hr ) );
  inc(fg ,int8u_ptr(ptrcomp(ptr1 ) + x2 * sizeof(int8u ) )^ * x_hr * (image_subpixel_size - y_hr ) );
  inc(fg ,int8u_ptr(ptrcomp(ptr2 ) + x1 * sizeof(int8u ) )^ * (image_subpixel_size - x_hr ) * y_hr );
  inc(fg ,int8u_ptr(ptrcomp(ptr2 ) + x2 * sizeof(int8u ) )^ * x_hr * y_hr );

  span.v:=int8u(fg shr (image_subpixel_shift * 2 ) );
  span.a:=base_mask;

  inc(ptrcomp(span ) ,sizeof(aggclr ) );

  _interpolator.inc_operator;

  dec(len );

 until len = 0;

 result:=_allocator.span;

end;

{ CONSTRUCT }
constructor span_pattern_filter_gray_2x2.Construct(alloc : span_allocator_ptr; WX ,WY : wrap_mode_ptr );
begin
 inherited Construct(alloc );

 m_wrap_mode_x:=WX;
 m_wrap_mode_y:=WY;

 m_wrap_mode_x.init(1 );
 m_wrap_mode_y.init(1 );

end;

{ CONSTRUCT }
constructor span_pattern_filter_gray_2x2.Construct(
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
procedure span_pattern_filter_gray_2x2.source_image_;
begin
 inherited source_image_(src );

 m_wrap_mode_x.init(src._width );
 m_wrap_mode_y.init(src._height );

end;

{ GENERATE }
function span_pattern_filter_gray_2x2.generate;
var
 fg ,x1 ,x2 ,y1 ,y2 : unsigned;

 x_hr ,y_hr ,x_lr ,y_lr : int;

 ptr1 ,ptr2 : int8u_ptr;

 span : aggclr_ptr;

 weight_array : int16_ptr;

begin
 _interpolator.begin_(x + filter_dx_dbl ,y + filter_dy_dbl ,len );

 span:=_allocator.span;

 weight_array:=
  int16_ptr(
   ptrcomp(_filter.weight_array ) +
   shr_int32(_filter.diameter div 2 - 1 ,image_subpixel_shift ) );

 repeat
  _interpolator.coordinates(@x_hr ,@y_hr );

  inc(x_hr ,filter_dx_int );
  inc(y_hr ,filter_dy_int );

  x_lr:=shr_int32(x_hr ,image_subpixel_shift );
  y_lr:=shr_int32(y_hr ,image_subpixel_shift );

  x1:=m_wrap_mode_x.func_operator(x_lr );
  x2:=m_wrap_mode_x.inc_operator;

  y1:=m_wrap_mode_y.func_operator(y_lr );
  y2:=m_wrap_mode_y.inc_operator;

  ptr1:=_source_image.row(y1 );
  ptr2:=_source_image.row(y2 );

  fg:=image_filter_size div 2;

  x_hr:=x_hr and image_subpixel_mask;
  y_hr:=y_hr and image_subpixel_mask;

  inc(fg ,
   int8u_ptr(ptrcomp(ptr1 ) + x1 * sizeof(int8u ) )^ *
   shr_int32(
    int16_ptr(ptrcomp(weight_array ) + (x_hr + image_subpixel_size ) * sizeof(int16 ) )^ *
    int16_ptr(ptrcomp(weight_array ) + (y_hr + image_subpixel_size ) * sizeof(int16 ) )^ +
    image_filter_size div 2 ,image_filter_shift ) );

  inc(fg ,
   int8u_ptr(ptrcomp(ptr1 ) + x2 * sizeof(int8u ) )^ *
   shr_int32(
    int16_ptr(ptrcomp(weight_array ) + x_hr * sizeof(int16 ) )^ *
    int16_ptr(ptrcomp(weight_array ) + (y_hr + image_subpixel_size ) * sizeof(int16 ) )^ +
    image_filter_size div 2 ,image_filter_shift ) );

  inc(fg ,
   int8u_ptr(ptrcomp(ptr2 ) + x1 * sizeof(int8u ) )^ *
   shr_int32(
    int16_ptr(ptrcomp(weight_array ) + (x_hr + image_subpixel_size ) * sizeof(int16 ) )^ *
    int16_ptr(ptrcomp(weight_array ) + y_hr * sizeof(int16 ) )^ +
    image_filter_size div 2 ,image_filter_shift ) );

  inc(fg ,
   int8u_ptr(ptrcomp(ptr2 ) + x2 * sizeof(int8u ) )^ *
   shr_int32(
    int16_ptr(ptrcomp(weight_array ) + x_hr * sizeof(int16 ) )^ *
    int16_ptr(ptrcomp(weight_array ) + y_hr * sizeof(int16 ) )^ +
    image_filter_size div 2 ,image_filter_shift ) );

  fg:=fg shr image_filter_shift;

  if fg > base_mask then
   fg:=base_mask;

  span.v:=int8u(fg );
  span.a:=base_mask;

  inc(ptrcomp(span ) ,sizeof(aggclr ) );

  _interpolator.inc_operator;

  dec(len );

 until len = 0;

 result:=_allocator.span;

end;

{ CONSTRUCT }
constructor span_pattern_filter_gray.Construct(alloc : span_allocator_ptr; WX ,WY : wrap_mode_ptr );
begin
 inherited Construct(alloc );

 m_wrap_mode_x:=WX;
 m_wrap_mode_y:=WY;

 m_wrap_mode_x.init(1 );
 m_wrap_mode_y.init(1 );

end;

{ CONSTRUCT }
constructor span_pattern_filter_gray.Construct(
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
procedure span_pattern_filter_gray.source_image_;
begin
 inherited source_image_(src );

 m_wrap_mode_x.init(src._width );
 m_wrap_mode_y.init(src._height );

end;

{ GENERATE }
function span_pattern_filter_gray.generate;
var
 fg ,start ,x_count ,weight_y ,x_hr ,y_hr ,x_fract ,y_lr ,x_int ,x_lr : int;

 diameter ,y_count : unsigned;

 row_ptr : int8u_ptr;

 weight_array : int16_ptr;

 span : aggclr_ptr;

begin
 _interpolator.begin_(x + filter_dx_dbl ,y + filter_dy_dbl ,len );

 diameter:=_filter.diameter;
 start   :=_filter.start;

 weight_array:=_filter.weight_array;

 span:=_allocator.span;

 repeat
  _interpolator.coordinates(@x ,@y );

  dec(x ,filter_dx_int );
  dec(y ,filter_dy_int );

  x_hr:=x;
  y_hr:=y;

  x_fract:=x_hr and image_subpixel_mask;
  y_count:=diameter;

  y_lr :=m_wrap_mode_y.func_operator(shr_int32(y ,image_subpixel_shift ) + start );
  x_int:=shr_int32(x ,image_subpixel_shift ) + start;

  y_hr:=image_subpixel_mask - (y_hr and image_subpixel_mask );
  fg  :=image_filter_size div 2;

  repeat
   x_count :=diameter;
   weight_y:=int16_ptr(ptrcomp(weight_array ) + y_hr * sizeof(int16 ) )^;

   x_hr:=image_subpixel_mask - x_fract;
   x_lr:=m_wrap_mode_x.func_operator(x_int );

   row_ptr:=_source_image.row(y_lr );

   repeat
    inc(fg ,
     int8u_ptr(ptrcomp(row_ptr ) + x_lr * sizeof(int8u ) )^ *
     shr_int32(
      weight_y *
      int16_ptr(ptrcomp(weight_array ) + x_hr * sizeof(int16 ) )^ +
      image_filter_size div 2 ,image_filter_shift ) );

    inc(x_hr ,image_subpixel_size );

    x_lr:=m_wrap_mode_x.inc_operator;

    dec(x_count );

   until x_count = 0;

   inc(y_hr ,image_subpixel_size );

   y_lr:=m_wrap_mode_y.inc_operator;

   dec(y_count );

  until y_count = 0;

  fg:=shr_int32(fg ,image_filter_shift );

  if fg < 0 then
   fg:=0;

  if fg > base_mask then
   fg:=base_mask;

  span.v:=int8u(fg );
  span.a:=base_mask;

  inc(ptrcomp(span ) ,sizeof(aggclr ) );

  _interpolator.inc_operator;

  dec(len );

 until len = 0;

 result:=_allocator.span;

end;

END.

