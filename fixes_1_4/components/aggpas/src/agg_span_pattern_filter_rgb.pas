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
//----------------------------------------------------------------------------
//
// classes span_pattern_filter_rgb*
//
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 06.03.2006-Milano: Unit port establishment
//
{ agg_span_pattern_filter_rgb.pas }
unit
 agg_span_pattern_filter_rgb ;

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
 span_pattern_filter_rgb_nn_ptr = ^span_pattern_filter_rgb_nn;
 span_pattern_filter_rgb_nn = object(span_image_filter )
   m_wrap_mode_x ,
   m_wrap_mode_y : wrap_mode_ptr;

   m_order : order_type;

   constructor Construct(alloc : span_allocator_ptr; WX ,WY : wrap_mode_ptr; order : order_type ); overload;
   constructor Construct(
                alloc : span_allocator_ptr;
                src : rendering_buffer_ptr;
                interpolator : span_interpolator_ptr;
                WX ,WY : wrap_mode_ptr;
                order : order_type ); overload;

   procedure source_image_(src : rendering_buffer_ptr );

   function  generate(x ,y : int; len : unsigned ) : aggclr_ptr; virtual;

  end;

 span_pattern_filter_rgb_bilinear_ptr = ^span_pattern_filter_rgb_bilinear;
 span_pattern_filter_rgb_bilinear = object(span_image_filter )
   m_wrap_mode_x ,
   m_wrap_mode_y : wrap_mode_ptr;

   m_order : order_type;

   constructor Construct(alloc : span_allocator_ptr; WX ,WY : wrap_mode_ptr; order : order_type ); overload;
   constructor Construct(
                alloc : span_allocator_ptr;
                src : rendering_buffer_ptr;
                interpolator : span_interpolator_ptr;
                WX ,WY : wrap_mode_ptr;
                order : order_type ); overload;

   procedure source_image_(src : rendering_buffer_ptr );

   function  generate(x ,y : int; len : unsigned ) : aggclr_ptr; virtual;

  end;

 span_pattern_filter_rgb_2x2_ptr = ^span_pattern_filter_rgb_2x2;
 span_pattern_filter_rgb_2x2 = object(span_image_filter )
   m_wrap_mode_x ,
   m_wrap_mode_y : wrap_mode_ptr;

   m_order : order_type;

   constructor Construct(alloc : span_allocator_ptr; WX ,WY : wrap_mode_ptr; order : order_type ); overload;
   constructor Construct(
                alloc : span_allocator_ptr;
                src : rendering_buffer_ptr;
                interpolator : span_interpolator_ptr;
                filter : image_filter_lut_ptr;
                WX ,WY : wrap_mode_ptr;
                order : order_type ); overload;

   procedure source_image_(src : rendering_buffer_ptr );

   function  generate(x ,y : int; len : unsigned ) : aggclr_ptr; virtual;

  end;

 span_pattern_filter_rgb_ptr = ^span_pattern_filter_rgb;
 span_pattern_filter_rgb = object(span_image_filter )
   m_wrap_mode_x ,
   m_wrap_mode_y : wrap_mode_ptr;

   m_order : order_type;

   constructor Construct(alloc : span_allocator_ptr; WX ,WY : wrap_mode_ptr; order : order_type ); overload;
   constructor Construct(
                alloc : span_allocator_ptr;
                src : rendering_buffer_ptr;
                interpolator : span_interpolator_ptr;
                filter : image_filter_lut_ptr;
                WX ,WY : wrap_mode_ptr;
                order : order_type ); overload;

   procedure source_image_(src : rendering_buffer_ptr );

   function  generate(x ,y : int; len : unsigned ) : aggclr_ptr; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor span_pattern_filter_rgb_nn.Construct(alloc : span_allocator_ptr; WX ,WY : wrap_mode_ptr; order : order_type );
begin
 inherited Construct(alloc );

 m_order:=order;

 m_wrap_mode_x:=WX;
 m_wrap_mode_y:=WY;

 m_wrap_mode_x.init(1 );
 m_wrap_mode_y.init(1 );

end;

{ CONSTRUCT }
constructor span_pattern_filter_rgb_nn.Construct(
             alloc : span_allocator_ptr;
             src : rendering_buffer_ptr;
             interpolator : span_interpolator_ptr;
             WX ,WY : wrap_mode_ptr;
             order : order_type );
var
 rgba : aggclr;

begin
 rgba.ConstrInt(0 ,0 ,0 ,0 );

 inherited Construct(alloc ,src ,@rgba ,interpolator ,NIL );

 m_order:=order;

 m_wrap_mode_x:=WX;
 m_wrap_mode_y:=WY;

 m_wrap_mode_x.init(src._width );
 m_wrap_mode_y.init(src._height );

end;

{ SOURCE_IMAGE_ }
procedure span_pattern_filter_rgb_nn.source_image_;
begin
 inherited source_image_(src );

 m_wrap_mode_x.init(src._width );
 m_wrap_mode_y.init(src._height );

end;

{ GENERATE }
function span_pattern_filter_rgb_nn.generate;
var
 span : aggclr_ptr;
 intr : span_interpolator_ptr;

 fg_ptr : int8u_ptr;

begin
 span:=_allocator.span;
 intr:=_interpolator;

 intr.begin_(x + filter_dx_dbl ,y + filter_dy_dbl ,len );

 repeat
  intr.coordinates(@x ,@y );

  x:=m_wrap_mode_x.func_operator(shr_int32(x ,image_subpixel_shift ) );
  y:=m_wrap_mode_y.func_operator(shr_int32(y ,image_subpixel_shift ) );

  fg_ptr:=int8u_ptr(ptrcomp(_source_image.row(y ) ) + x * 3 * sizeof(int8u ) );

  span.r:=int8u_ptr(ptrcomp(fg_ptr ) + m_order.R * sizeof(int8u ) )^;
  span.g:=int8u_ptr(ptrcomp(fg_ptr ) + m_order.G * sizeof(int8u ) )^;
  span.b:=int8u_ptr(ptrcomp(fg_ptr ) + m_order.B * sizeof(int8u ) )^;
  span.a:=base_mask;

  inc(ptrcomp(span ) ,sizeof(aggclr ) );

  intr.inc_operator;

  dec(len );

 until len = 0;

 result:=_allocator.span;

end;

{ CONSTRUCT }
constructor span_pattern_filter_rgb_bilinear.Construct(alloc : span_allocator_ptr; WX ,WY : wrap_mode_ptr; order : order_type );
begin
 inherited Construct(alloc );

 m_order:=order;

 m_wrap_mode_x:=WX;
 m_wrap_mode_y:=WY;

 m_wrap_mode_x.init(1 );
 m_wrap_mode_y.init(1 );

end;

{ CONSTRUCT }
constructor span_pattern_filter_rgb_bilinear.Construct(
             alloc : span_allocator_ptr;
             src : rendering_buffer_ptr;
             interpolator : span_interpolator_ptr;
             WX ,WY : wrap_mode_ptr;
             order : order_type );
var
 rgba : aggclr;

begin
 rgba.ConstrInt(0 ,0 ,0 ,0 );

 inherited Construct(alloc ,src ,@rgba ,interpolator ,NIL );

 m_order:=order;

 m_wrap_mode_x:=WX;
 m_wrap_mode_y:=WY;

 m_wrap_mode_x.init(src._width );
 m_wrap_mode_y.init(src._height );

end;

{ SOURCE_IMAGE_ }
procedure span_pattern_filter_rgb_bilinear.source_image_;
begin
 inherited source_image_(src );

 m_wrap_mode_x.init(src._width );
 m_wrap_mode_y.init(src._height );

end;

{ GENERATE }
function span_pattern_filter_rgb_bilinear.generate;
var
 span : aggclr_ptr;
 intr : span_interpolator_ptr;

 fg : array[0..2 ] of unsigned;

 fg_ptr ,ptr1 ,ptr2 : int8u_ptr;

 x_hr ,y_hr ,x_lr ,y_lr ,weight : int;

 x1 ,x2 ,y1 ,y2 : unsigned;

begin
 span:=_allocator.span;
 intr:=_interpolator;

 intr.begin_(x + filter_dx_dbl ,y + filter_dy_dbl ,len );

 repeat
  intr.coordinates(@x_hr ,@y_hr );

  dec(x_hr ,filter_dx_int );
  dec(y_hr ,filter_dy_int );

  x_lr:=shr_int32(x_hr ,image_subpixel_shift );
  y_lr:=shr_int32(y_hr ,image_subpixel_shift );

  x1:=m_wrap_mode_x.func_operator(x_lr);
  x2:=m_wrap_mode_x.inc_operator;

  x1:=x1 * 3;
  x2:=x2 * 3;

  y1:=m_wrap_mode_y.func_operator(y_lr );
  y2:=m_wrap_mode_y.inc_operator;

  ptr1:=_source_image.row(y1 );
  ptr2:=_source_image.row(y2 );

  fg[0 ]:=image_subpixel_size * image_subpixel_size div 2;
  fg[1 ]:=fg[0 ];
  fg[2 ]:=fg[0 ];

  x_hr:=x_hr and image_subpixel_mask;
  y_hr:=y_hr and image_subpixel_mask;

  fg_ptr:=int8u_ptr(ptrcomp(ptr1 ) + x1 * sizeof(int8u ) );
  weight:=(image_subpixel_size - x_hr ) * (image_subpixel_size - y_hr );

  inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
  inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
  inc(fg[2 ] ,weight * fg_ptr^ );

  fg_ptr:=int8u_ptr(ptrcomp(ptr1 ) + x2 * sizeof(int8u ) );
  weight:=x_hr * (image_subpixel_size - y_hr );

  inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
  inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
  inc(fg[2 ] ,weight * fg_ptr^ );

  fg_ptr:=int8u_ptr(ptrcomp(ptr2 ) + x1 * sizeof(int8u ) );
  weight:=(image_subpixel_size - x_hr ) * y_hr;

  inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
  inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
  inc(fg[2 ] ,weight * fg_ptr^ );

  fg_ptr:=int8u_ptr(ptrcomp(ptr2 ) + x2 * sizeof(int8u ) );
  weight:=x_hr * y_hr;

  inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
  inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
  inc(fg[2 ] ,weight * fg_ptr^ );

  span.r:=int8u(fg[m_order.R ] shr (image_subpixel_shift * 2 ) );
  span.g:=int8u(fg[m_order.G ] shr (image_subpixel_shift * 2 ) );
  span.b:=int8u(fg[m_order.B ] shr (image_subpixel_shift * 2 ) );
  span.a:=base_mask;

  inc(ptrcomp(span ) ,sizeof(aggclr ) );

  intr.inc_operator;

  dec(len );

 until len = 0;

 result:=_allocator.span;

end;

{ CONSTRUCT }
constructor span_pattern_filter_rgb_2x2.Construct(alloc : span_allocator_ptr; WX ,WY : wrap_mode_ptr; order : order_type );
begin
 inherited Construct(alloc );

 m_order:=order;

 m_wrap_mode_x:=WX;
 m_wrap_mode_y:=WY;

 m_wrap_mode_x.init(1 );
 m_wrap_mode_y.init(1 );

end;

{ CONSTRUCT }
constructor span_pattern_filter_rgb_2x2.Construct(
             alloc : span_allocator_ptr;
             src : rendering_buffer_ptr;
             interpolator : span_interpolator_ptr;
             filter : image_filter_lut_ptr;
             WX ,WY : wrap_mode_ptr;
             order : order_type );
var
 rgba : aggclr;

begin
 rgba.ConstrInt(0 ,0 ,0 ,0 );

 inherited Construct(alloc ,src ,@rgba ,interpolator ,filter );

 m_order:=order;

 m_wrap_mode_x:=WX;
 m_wrap_mode_y:=WY;

 m_wrap_mode_x.init(src._width );
 m_wrap_mode_y.init(src._height );

end;

{ SOURCE_IMAGE_ }
procedure span_pattern_filter_rgb_2x2.source_image_;
begin
 inherited source_image_(src );

 m_wrap_mode_x.init(src._width );
 m_wrap_mode_y.init(src._height );

end;

{ GENERATE }
function span_pattern_filter_rgb_2x2.generate;
var
 span : aggclr_ptr;
 intr : span_interpolator_ptr;

 x_hr ,y_hr ,x_lr ,y_lr ,weight : int;

 x1 ,x2 ,y1 ,y2 : unsigned;

 fg : array[0..2 ] of unsigned;

 fg_ptr ,ptr1 ,ptr2 : int8u_ptr;

 weight_array : int16_ptr;

begin
 span:=_allocator.span;
 intr:=_interpolator;

 intr.begin_(x + filter_dx_dbl ,y + filter_dy_dbl ,len );

 weight_array:=
  int16_ptr(
   ptrcomp(_filter.weight_array ) +
   shr_int32(_filter.diameter div 2 - 1 ,image_subpixel_shift ) );

 repeat
  intr.coordinates(@x_hr ,@y_hr );

  dec(x_hr ,filter_dx_int );
  dec(y_hr ,filter_dy_int );

  x_lr:=shr_int32(x_hr ,image_subpixel_shift );
  y_lr:=shr_int32(y_hr ,image_subpixel_shift );

  x1:=m_wrap_mode_x.func_operator(x_lr );
  x2:=m_wrap_mode_x.inc_operator;

  x1:=x1 * 3;
  x2:=x2 * 3;

  y1:=m_wrap_mode_y.func_operator(y_lr );
  y2:=m_wrap_mode_y.inc_operator;

  ptr1:=_source_image.row(y1 );
  ptr2:=_source_image.row(y2 );

  fg[0 ]:=image_filter_size div 2;
  fg[1 ]:=fg[0 ];
  fg[2 ]:=fg[0 ];

  x_hr:=x_hr and image_subpixel_mask;
  y_hr:=y_hr and image_subpixel_mask;

  fg_ptr:=int8u_ptr(ptrcomp(ptr1 ) + x1 * sizeof(int8u ) );
  weight:=
   shr_int32(
    int16_ptr(ptrcomp(weight_array ) + (x_hr + image_subpixel_size ) * sizeof(int16 ) )^ *
    int16_ptr(ptrcomp(weight_array ) + (y_hr + image_subpixel_size ) * sizeof(int16 ) )^ +
    image_filter_size div 2 ,image_filter_shift );

  inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
  inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
  inc(fg[2 ] ,weight * fg_ptr^ );

  fg_ptr:=int8u_ptr(ptrcomp(ptr1 ) + x2 * sizeof(int8u ) );
  weight:=
   shr_int32(
    int16_ptr(ptrcomp(weight_array ) + x_hr * sizeof(int16 ) )^ *
    int16_ptr(ptrcomp(weight_array ) + (y_hr + image_subpixel_size ) * sizeof(int16 ) )^ +
    image_filter_size div 2,image_filter_shift );

  inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
  inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
  inc(fg[2 ] ,weight * fg_ptr^ );

  fg_ptr:=int8u_ptr(ptrcomp(ptr2 ) + x1 * sizeof(int8u ) );
  weight:=
   shr_int32(
    int16_ptr(ptrcomp(weight_array ) + (x_hr + image_subpixel_size ) * sizeof(int16 ) )^ *
    int16_ptr(ptrcomp(weight_array ) + y_hr * sizeof(int16 ) )^ +
    image_filter_size div 2,image_filter_shift );

  inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
  inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
  inc(fg[2 ] ,weight * fg_ptr^ );

  fg_ptr:=int8u_ptr(ptrcomp(ptr2 ) + x2 * sizeof(int8u ) );
  weight:=
   shr_int32(
    int16_ptr(ptrcomp(weight_array ) + x_hr * sizeof(int16 ) )^ *
    int16_ptr(ptrcomp(weight_array ) + y_hr * sizeof(int16 ) )^ +
    image_filter_size div 2,image_filter_shift );

  inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
  inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
  inc(fg[2 ] ,weight * fg_ptr^ );

  fg[0 ]:=fg[0 ] shr image_filter_shift;
  fg[1 ]:=fg[1 ] shr image_filter_shift;
  fg[2 ]:=fg[2 ] shr image_filter_shift;

  if fg[0 ] > base_mask then
   fg[0 ]:=base_mask;

  if fg[1 ] > base_mask then
   fg[1 ]:=base_mask;

  if fg[2 ] > base_mask then
   fg[2 ]:=base_mask;

  span.r:=int8u(fg[m_order.R ] );
  span.g:=int8u(fg[m_order.G ] );
  span.b:=int8u(fg[m_order.B ] );
  span.a:=base_mask;

  inc(ptrcomp(span ) ,sizeof(aggclr ) );

  intr.inc_operator;

  dec(len );

 until len = 0;

 result:=_allocator.span;

end;

{ CONSTRUCT }
constructor span_pattern_filter_rgb.Construct(alloc : span_allocator_ptr; WX ,WY : wrap_mode_ptr; order : order_type );
begin
 inherited Construct(alloc );

 m_order:=order;

 m_wrap_mode_x:=WX;
 m_wrap_mode_y:=WY;

 m_wrap_mode_x.init(1 );
 m_wrap_mode_y.init(1 );

end;

{ CONSTRUCT }
constructor span_pattern_filter_rgb.Construct(
             alloc : span_allocator_ptr;
             src : rendering_buffer_ptr;
             interpolator : span_interpolator_ptr;
             filter : image_filter_lut_ptr;
             WX ,WY : wrap_mode_ptr;
             order : order_type );
var
 rgba : aggclr;

begin
 rgba.ConstrInt(0 ,0 ,0 ,0 );

 inherited Construct(alloc ,src ,@rgba ,interpolator ,filter );

 m_order:=order;

 m_wrap_mode_x:=WX;
 m_wrap_mode_y:=WY;

 m_wrap_mode_x.init(src._width );
 m_wrap_mode_y.init(src._height );

end;

{ SOURCE_IMAGE_ }
procedure span_pattern_filter_rgb.source_image_;
begin
 inherited source_image_(src );

 m_wrap_mode_x.init(src._width );
 m_wrap_mode_y.init(src._height );

end;

{ GENERATE }
function span_pattern_filter_rgb.generate;
var
 span : aggclr_ptr;
 intr : span_interpolator_ptr;

 fg : array[0..2 ] of int;

 diameter ,y_count : unsigned;

 start ,x_count ,weight_y ,x_hr ,y_hr ,x_fract ,y_lr ,x_int ,x_lr ,weight : int;

 row_ptr ,fg_ptr : int8u_ptr;

 weight_array : int16_ptr;

begin
 span:=_allocator.span;
 intr:=_interpolator;

 intr.begin_(x + filter_dx_dbl ,y + filter_dy_dbl ,len );

 diameter    :=_filter.diameter;
 start       :=_filter.start;
 weight_array:=_filter.weight_array;

 repeat
  intr.coordinates(@x ,@y );

  dec(x ,filter_dx_int );
  dec(y ,filter_dy_int );

  x_hr:=x;
  y_hr:=y;

  x_fract:=x_hr and image_subpixel_mask;
  y_count:=diameter;

  y_lr :=m_wrap_mode_y.func_operator(shr_int32(y ,image_subpixel_shift ) + start );
  x_int:=shr_int32(x ,image_subpixel_shift ) + start;
  y_hr :=image_subpixel_mask - (y_hr and image_subpixel_mask );

  fg[0 ]:=image_filter_size div 2;
  fg[1 ]:=fg[0 ];
  fg[2 ]:=fg[0 ];

  repeat
   x_count :=diameter;
   weight_y:=int16_ptr(ptrcomp(weight_array ) + y_hr * sizeof(int16 ) )^;

   x_hr:=image_subpixel_mask - x_fract;
   x_lr:=m_wrap_mode_x.func_operator(x_int );

   row_ptr:=_source_image.row(y_lr );

   repeat
    fg_ptr:=int8u_ptr(ptrcomp(row_ptr ) + x_lr * 3 * sizeof(int8u ) );
    weight:=
     shr_int32(
      weight_y *
      int16_ptr(ptrcomp(weight_array ) + x_hr * sizeof(int16 ) )^ +
      image_filter_size div 2 ,image_filter_shift );

    inc(fg[0 ] ,fg_ptr^ * weight ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    inc(fg[1 ] ,fg_ptr^ * weight ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    inc(fg[2 ] ,fg_ptr^ * weight );
    inc(x_hr ,image_subpixel_size );

    x_lr:=m_wrap_mode_x.inc_operator;

    dec(x_count );

   until x_count = 0;

   inc(y_hr ,image_subpixel_size );

   y_lr:=m_wrap_mode_y.inc_operator;

   dec(y_count );

  until y_count = 0;

  fg[0 ]:=shr_int32(fg[0 ] ,image_filter_shift );
  fg[1 ]:=shr_int32(fg[1 ] ,image_filter_shift );
  fg[2 ]:=shr_int32(fg[2 ] ,image_filter_shift );

  if fg[0 ] < 0 then
   fg[0 ]:=0;

  if fg[1 ] < 0 then
   fg[1 ]:=0;

  if fg[2 ] < 0 then
   fg[2 ]:=0;

  if fg[0 ] > base_mask then
   fg[0 ]:=base_mask;

  if fg[1 ] > base_mask then
   fg[1 ]:=base_mask;

  if fg[2 ] > base_mask then
   fg[2 ]:=base_mask;

  span.r:=int8u(fg[m_order.R ] );
  span.g:=int8u(fg[m_order.G ] );
  span.b:=int8u(fg[m_order.B ] );
  span.a:=base_mask;

  inc(ptrcomp(span ) ,sizeof(aggclr ) );

  intr.inc_operator;

  dec(len );

 until len = 0;

 result:=_allocator.span;

end;

END.

