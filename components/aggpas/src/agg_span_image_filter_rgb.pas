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
// 23.06.2006-Milano: ptrcomp adjustments
// 01.03.2006-Milano: Unit port establishment
//
{ agg_span_image_filter_rgb.pas }
unit
 agg_span_image_filter_rgb ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
 agg_basics ,
 agg_color ,
 agg_image_filters ,
 agg_span_image_filter ,
 agg_span_allocator ,
 agg_span_interpolator_linear ,
 agg_rendering_buffer ;

{ TYPES DEFINITION }
const
 base_shift = agg_color.base_shift;
 base_mask  = agg_color.base_mask;

type
 span_image_filter_rgb_nn_ptr = ^span_image_filter_rgb_nn;
 span_image_filter_rgb_nn = object(span_image_filter )
   m_order : order_type;

   constructor Construct(alloc : span_allocator_ptr; order : order_type ); overload;
   constructor Construct(
                alloc      : span_allocator_ptr;
                src        : rendering_buffer_ptr;
                back_color : aggclr_ptr;
                inter      : span_interpolator_ptr;
                order      : order_type ); overload;

   function  generate(x ,y : int; len : unsigned ) : aggclr_ptr; virtual;

  end;

 span_image_filter_rgb_bilinear_ptr = ^span_image_filter_rgb_bilinear;
 span_image_filter_rgb_bilinear = object(span_image_filter )
   m_order : order_type;

   constructor Construct(alloc : span_allocator_ptr; order : order_type ); overload;
   constructor Construct(
                alloc      : span_allocator_ptr;
                src        : rendering_buffer_ptr;
                back_color : aggclr_ptr;
                inter      : span_interpolator_ptr;
                order      : order_type ); overload;

   function  generate(x ,y : int; len : unsigned ) : aggclr_ptr; virtual;

  end;

 span_image_filter_rgb_2x2_ptr = ^span_image_filter_rgb_2x2;
 span_image_filter_rgb_2x2 = object(span_image_filter )
   m_order : order_type;

   constructor Construct(alloc : span_allocator_ptr; order : order_type ); overload;
   constructor Construct(
                alloc      : span_allocator_ptr;
                src        : rendering_buffer_ptr;
                back_color : aggclr_ptr;
                inter      : span_interpolator_ptr;
                filter     : image_filter_lut_ptr;
                order      : order_type ); overload;

   function  generate(x ,y : int; len : unsigned ) : aggclr_ptr; virtual;

  end;

 span_image_filter_rgb_ptr = ^span_image_filter_rgb; 
 span_image_filter_rgb = object(span_image_filter )
   m_order : order_type;

   constructor Construct(alloc : span_allocator_ptr; order : order_type ); overload;
   constructor Construct(
                alloc      : span_allocator_ptr;
                src        : rendering_buffer_ptr;
                back_color : aggclr_ptr;
                inter      : span_interpolator_ptr;
                filter     : image_filter_lut_ptr;
                order      : order_type ); overload;

   function  generate(x ,y : int; len : unsigned ) : aggclr_ptr; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor span_image_filter_rgb_nn.Construct(alloc : span_allocator_ptr; order : order_type );
begin
 inherited Construct(alloc );

 m_order:=order;

end;

{ CONSTRUCT }
constructor span_image_filter_rgb_nn.Construct(
             alloc      : span_allocator_ptr;
             src        : rendering_buffer_ptr;
             back_color : aggclr_ptr;
             inter      : span_interpolator_ptr;
             order      : order_type );
begin
 inherited Construct(alloc ,src ,back_color ,inter ,NIL );

 m_order:=order;

end;

{ GENERATE }
function span_image_filter_rgb_nn.generate;
var
 fg : array[0..2 ] of unsigned;

 src_alpha : unsigned;

 fg_ptr : int8u_ptr;

 span : aggclr_ptr;

 maxx ,maxy : int;

begin
 _interpolator.begin_(x + filter_dx_dbl ,y + filter_dy_dbl ,len );

 span:=_allocator.span;

 maxx:=_source_image._width - 1;
 maxy:=_source_image._height - 1;

 repeat
  _interpolator.coordinates(@x ,@y );

  x:=shr_int32(x ,image_subpixel_shift );
  y:=shr_int32(y ,image_subpixel_shift );

  if (x >= 0 ) and
     (y >= 0 ) and
     (x <= maxx ) and
     (y <= maxy ) then
   begin
    fg_ptr:=int8u_ptr(ptrcomp(_source_image.row(y ) ) + (x + x + x ) * sizeof(int8u ) );

    fg[0 ]:=fg_ptr^; inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    fg[1 ]:=fg_ptr^; inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    fg[2 ]:=fg_ptr^; inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

    src_alpha:=base_mask;

   end
  else
   begin
    fg[m_order.R ]:=_background_color.r;
    fg[m_order.G ]:=_background_color.g;
    fg[m_order.B ]:=_background_color.b;
    src_alpha     :=_background_color.a;

   end;

  span.r:=int8u(fg[m_order.R ] );
  span.g:=int8u(fg[m_order.G ] );
  span.b:=int8u(fg[m_order.B ] );
  span.a:=int8u(src_alpha );

  inc(ptrcomp(span ) ,sizeof(aggclr ) );

  _interpolator.inc_operator;

  dec(len );

 until len = 0;

 result:=_allocator.span;

end;

{ CONSTRUCT }
constructor span_image_filter_rgb_bilinear.Construct(alloc : span_allocator_ptr; order : order_type );
begin
 inherited Construct(alloc );

 m_order:=order;

end;

{ CONSTRUCT }
constructor span_image_filter_rgb_bilinear.Construct(
             alloc      : span_allocator_ptr;
             src        : rendering_buffer_ptr;
             back_color : aggclr_ptr;
             inter      : span_interpolator_ptr;
             order      : order_type );
begin
 inherited Construct(alloc ,src ,back_color ,inter ,NIL );

 m_order:=order;

end;

{ GENERATE }
function span_image_filter_rgb_bilinear.generate;
var
 fg : array[0..2 ] of unsigned;

 src_alpha ,weight : unsigned;

 back_r ,back_g ,back_b ,back_a : int8u;

 fg_ptr : int8u_ptr;

 span : aggclr_ptr;

 maxx ,maxy ,x_hr ,y_hr ,x_lr ,y_lr : int;

begin
 _interpolator.begin_(x + filter_dx_dbl ,y + filter_dy_dbl ,len );

 back_r:=_background_color.r;
 back_g:=_background_color.g;
 back_b:=_background_color.b;
 back_a:=_background_color.a;

 span:=_allocator.span;

 maxx:=_source_image._width - 1;
 maxy:=_source_image._height - 1;

 repeat
  _interpolator.coordinates(@x_hr ,@y_hr );

  dec(x_hr ,filter_dx_int );
  dec(y_hr ,filter_dy_int );

  x_lr:=shr_int32(x_hr ,image_subpixel_shift );
  y_lr:=shr_int32(y_hr ,image_subpixel_shift );

  if (x_lr >= 0 ) and
     (y_lr >= 0 ) and
     (x_lr < maxx ) and
     (y_lr < maxy ) then
   begin
    fg[0 ]:=image_subpixel_size * image_subpixel_size div 2;
    fg[1 ]:=fg[0 ];
    fg[2 ]:=fg[0 ];

    x_hr:=x_hr and image_subpixel_mask;
    y_hr:=y_hr and image_subpixel_mask;

    fg_ptr:=int8u_ptr(ptrcomp(_source_image.row(y_lr ) ) + (x_lr + x_lr + x_lr ) * sizeof(int8u ) );
    weight:=(image_subpixel_size - x_hr ) * (image_subpixel_size - y_hr );

    inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    inc(fg[2 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

    weight:=x_hr * (image_subpixel_size - y_hr );

    inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    inc(fg[2 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

    fg_ptr:=_source_image.next_row(int8u_ptr(ptrcomp(fg_ptr ) - 6 * sizeof(int8u ) ) );
    weight:=(image_subpixel_size - x_hr ) * y_hr;

    inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    inc(fg[2 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

    weight:=x_hr * y_hr;

    inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    inc(fg[2 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

    fg[0 ]:=fg[0 ] shr (image_subpixel_shift * 2 );
    fg[1 ]:=fg[1 ] shr (image_subpixel_shift * 2 );
    fg[2 ]:=fg[2 ] shr (image_subpixel_shift * 2 );

    src_alpha:=base_mask;

   end
  else
   begin
    if (x_lr < -1 ) or
       (y_lr < -1 ) or
       (x_lr > maxx ) or
       (y_lr > maxy ) then
     begin
      fg[m_order.R ]:=back_r;
      fg[m_order.G ]:=back_g;
      fg[m_order.B ]:=back_b;
      src_alpha     :=back_a;

     end
    else
     begin
      fg[0 ]   :=image_subpixel_size * image_subpixel_size div 2;
      fg[1 ]   :=fg[0 ];
      fg[2 ]   :=fg[0 ];
      src_alpha:=fg[0 ];

      x_hr:=x_hr and image_subpixel_mask;
      y_hr:=y_hr and image_subpixel_mask;

      weight:=(image_subpixel_size - x_hr ) * (image_subpixel_size - y_hr );

      if (x_lr >= 0 ) and
         (y_lr >= 0 ) and
         (x_lr <= maxx ) and
         (y_lr <= maxy ) then
       begin
        fg_ptr:=int8u_ptr(ptrcomp(_source_image.row(y_lr ) ) + (x_lr + x_lr + x_lr ) * sizeof(int8u ) );

        inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
        inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
        inc(fg[2 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

        inc(src_alpha ,weight * base_mask );

       end
      else
       begin
        inc(fg[m_order.R ] ,back_r * weight );
        inc(fg[m_order.G ] ,back_g * weight );
        inc(fg[m_order.B ] ,back_b * weight );

        inc(src_alpha ,back_a * weight );

       end;

      inc(x_lr );

      weight:=x_hr * (image_subpixel_size - y_hr );

      if (x_lr >= 0 ) and
         (y_lr >= 0 ) and
         (x_lr <= maxx ) and
         (y_lr <= maxy ) then
       begin
        fg_ptr:=int8u_ptr(ptrcomp(_source_image.row(y_lr ) ) + (x_lr + x_lr + x_lr ) * sizeof(int8u ) );

        inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
        inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
        inc(fg[2 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

        inc(src_alpha ,weight * base_mask );

       end
      else
       begin
        inc(fg[m_order.R ] ,back_r * weight );
        inc(fg[m_order.G ] ,back_g * weight );
        inc(fg[m_order.B ] ,back_b * weight );

        inc(src_alpha ,back_a * weight );

       end;

      dec(x_lr );
      inc(y_lr );

      weight:=(image_subpixel_size - x_hr ) * y_hr;

      if (x_lr >= 0 ) and
         (y_lr >= 0 ) and
         (x_lr <= maxx ) and
         (y_lr <= maxy ) then
       begin
        fg_ptr:=int8u_ptr(ptrcomp(_source_image.row(y_lr ) ) + (x_lr + x_lr + x_lr ) * sizeof(int8u ) );

        inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
        inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
        inc(fg[2 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

        inc(src_alpha ,weight * base_mask );

       end
      else
       begin
        inc(fg[m_order.R ] ,back_r * weight );
        inc(fg[m_order.G ] ,back_g * weight );
        inc(fg[m_order.B ] ,back_b * weight );

        inc(src_alpha ,back_a * weight );

       end;

      inc(x_lr );

      weight:=x_hr * y_hr;

      if (x_lr >= 0 ) and
         (y_lr >= 0 ) and
         (x_lr <= maxx ) and
         (y_lr <= maxy ) then
       begin
        fg_ptr:=int8u_ptr(ptrcomp(_source_image.row(y_lr ) ) + (x_lr + x_lr + x_lr ) * sizeof(int8u ) );

        inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
        inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
        inc(fg[2 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

        inc(src_alpha ,weight * base_mask );

       end
      else
       begin
        inc(fg[m_order.R ] ,back_r * weight );
        inc(fg[m_order.G ] ,back_g * weight );
        inc(fg[m_order.B ] ,back_b * weight );

        inc(src_alpha ,back_a * weight );

       end;

      fg[0 ]:=fg[0 ] shr (image_subpixel_shift * 2 );
      fg[1 ]:=fg[0 ] shr (image_subpixel_shift * 2 );
      fg[2 ]:=fg[0 ] shr (image_subpixel_shift * 2 );

      src_alpha:=src_alpha shr (image_subpixel_shift * 2 );

     end;

   end;

  span.r:=int8u(fg[m_order.R ] );
  span.g:=int8u(fg[m_order.G ] );
  span.b:=int8u(fg[m_order.B ] );
  span.a:=int8u(src_alpha );

  inc(ptrcomp(span ) ,sizeof(aggclr ) );

  _interpolator.inc_operator;

  dec(len );

 until len = 0;

 result:=_allocator.span;

end;

{ CONSTRUCT }
constructor span_image_filter_rgb_2x2.Construct(alloc : span_allocator_ptr; order : order_type );
begin
 inherited Construct(alloc );

 m_order:=order;

end;

{ CONSTRUCT }
constructor span_image_filter_rgb_2x2.Construct(
             alloc      : span_allocator_ptr;
             src        : rendering_buffer_ptr;
             back_color : aggclr_ptr;
             inter      : span_interpolator_ptr;
             filter     : image_filter_lut_ptr;
             order      : order_type );
begin
 inherited Construct(alloc ,src ,back_color ,inter ,filter );

 m_order:=order;

end;

{ GENERATE }
function span_image_filter_rgb_2x2.generate;
var
 fg : array[0..2 ] of unsigned;

 src_alpha ,weight : unsigned;

 back_r ,back_g ,back_b ,back_a : int8u;

 fg_ptr : int8u_ptr;

 span : aggclr_ptr;

 weight_array : int16_ptr;

 maxx ,maxy ,x_hr ,y_hr ,x_lr ,y_lr : int;

begin
 _interpolator.begin_(x + filter_dx_dbl ,y + filter_dy_dbl ,len );

 back_r:=_background_color.r;
 back_g:=_background_color.g;
 back_b:=_background_color.b;
 back_a:=_background_color.a;

 span:=_allocator.span;

 weight_array:=
  int16_ptr(
   ptrcomp(_filter.weight_array ) +
   ((_filter.diameter div 2 - 1 ) shl image_subpixel_shift ) * sizeof(int16 ) );

 maxx:=_source_image._width - 1;
 maxy:=_source_image._height - 1;

 repeat
  _interpolator.coordinates(@x_hr ,@y_hr );

  dec(x_hr ,filter_dx_int );
  dec(y_hr ,filter_dy_int );

  x_lr:=shr_int32(x_hr ,image_subpixel_shift );
  y_lr:=shr_int32(y_hr ,image_subpixel_shift );

  if (x_lr >= 0 ) and
     (y_lr >= 0 ) and
     (x_lr < maxx ) and
     (y_lr < maxy ) then
   begin
    fg[0 ]:=image_filter_size div 2;
    fg[1 ]:=fg[0 ];
    fg[2 ]:=fg[0 ];

    x_hr:=x_hr and image_subpixel_mask;
    y_hr:=y_hr and image_subpixel_mask;

    fg_ptr:=int8u_ptr(ptrcomp(_source_image.row(y_lr ) ) + (x_lr + x_lr + x_lr ) * sizeof(int8u ) );
    weight:=
     shr_int32(
      int16_ptr(ptrcomp(weight_array ) + (x_hr + image_subpixel_size ) * sizeof(int16 ) )^ *
      int16_ptr(ptrcomp(weight_array ) + (y_hr + image_subpixel_size ) * sizeof(int16 ) )^ +
      image_filter_size div 2 ,image_filter_shift );

    inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    inc(fg[2 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

    weight:=
     shr_int32(
      int16_ptr(ptrcomp(weight_array ) + x_hr * sizeof(int16 ) )^ *
      int16_ptr(ptrcomp(weight_array ) + (y_hr + image_subpixel_size ) * sizeof(int16 ) )^ +
      image_filter_size div 2 ,image_filter_shift );

    inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    inc(fg[2 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

    fg_ptr:=_source_image.next_row(int8u_ptr(ptrcomp(fg_ptr ) - 6 * sizeof(int8u ) ) );
    weight:=
     shr_int32(
      int16_ptr(ptrcomp(weight_array ) + (x_hr + image_subpixel_size ) * sizeof(int16 ) )^ *
      int16_ptr(ptrcomp(weight_array ) + y_hr * sizeof(int16 ) )^ +
      image_filter_size div 2 ,image_filter_shift );

    inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    inc(fg[2 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

    weight:=
     shr_int32(
      int16_ptr(ptrcomp(weight_array ) + x_hr * sizeof(int16 ) )^ *
      int16_ptr(ptrcomp(weight_array ) + y_hr * sizeof(int16 ) )^ +
      image_filter_size div 2 ,image_filter_shift );

    inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
    inc(fg[2 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

    fg[0 ]:=fg[0 ] shr image_filter_shift;
    fg[1 ]:=fg[1 ] shr image_filter_shift;
    fg[2 ]:=fg[2 ] shr image_filter_shift;

    src_alpha:=base_mask;

    if fg[0 ] > base_mask then
     fg[0 ]:=base_mask;

    if fg[1 ] > base_mask then
     fg[1 ]:=base_mask;

    if fg[2 ] > base_mask then
     fg[2 ]:=base_mask;

   end
  else
   begin
    if (x_lr < -1 ) or
       (y_lr < -1 ) or
       (x_lr > maxx ) or
       (y_lr > maxy ) then
     begin
      fg[m_order.R ]:=back_r;
      fg[m_order.G ]:=back_g;
      fg[m_order.B ]:=back_b;
      src_alpha     :=back_a;

     end
    else
     begin
      fg[0 ]   :=image_filter_size div 2;
      fg[1 ]   :=fg[0 ];
      fg[2 ]   :=fg[0 ];
      src_alpha:=fg[0 ];

      x_hr:=x_hr and image_subpixel_mask;
      y_hr:=y_hr and image_subpixel_mask;

      weight:=
       shr_int32(
        int16_ptr(ptrcomp(weight_array ) + (x_hr + image_subpixel_size ) * sizeof(int16 ) )^ *
        int16_ptr(ptrcomp(weight_array ) + (y_hr + image_subpixel_size ) * sizeof(int16 ) )^ +
        image_filter_size div 2 ,image_filter_shift );

      if (x_lr >= 0 ) and
         (y_lr >= 0 ) and
         (x_lr <= maxx ) and
         (y_lr <= maxy ) then
       begin
        fg_ptr:=int8u_ptr(ptrcomp(_source_image.row(y_lr ) ) + (x_lr + x_lr + x_lr ) * sizeof(int8u ) );

        inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
        inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
        inc(fg[2 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

        inc(src_alpha ,weight * base_mask );

       end
      else
       begin
        inc(fg[m_order.R ] ,back_r * weight );
        inc(fg[m_order.G ] ,back_g * weight );
        inc(fg[m_order.B ] ,back_b * weight );

        inc(src_alpha ,back_a * weight );

       end;

      inc(x_lr );

      weight:=
       shr_int32(
        int16_ptr(ptrcomp(weight_array ) + x_hr * sizeof(int16 ) )^ *
        int16_ptr(ptrcomp(weight_array ) + (y_hr + image_subpixel_size ) * sizeof(int16 ) )^ +
        image_filter_size div 2 ,image_filter_shift );

      if (x_lr >= 0 ) and
         (y_lr >= 0 ) and
         (x_lr <= maxx ) and
         (y_lr <= maxy ) then
       begin
        fg_ptr:=int8u_ptr(ptrcomp(_source_image.row(y_lr) ) + (x_lr + x_lr + x_lr ) * sizeof(int8u ) );

        inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
        inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
        inc(fg[2 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

        inc(src_alpha ,weight * base_mask );

       end
      else
       begin
        inc(fg[m_order.R ] ,back_r * weight );
        inc(fg[m_order.G ] ,back_g * weight );
        inc(fg[m_order.B ] ,back_b * weight );

        inc(src_alpha ,back_a * weight );

       end;

      dec(x_lr );
      inc(y_lr );

      weight:=
       shr_int32(
        int16_ptr(ptrcomp(weight_array ) + (x_hr + image_subpixel_size ) * sizeof(int16 ) )^ *
        int16_ptr(ptrcomp(weight_array ) + y_hr * sizeof(int16 ) )^ +
        image_filter_size div 2 ,image_filter_shift );

      if (x_lr >= 0 ) and
         (y_lr >= 0 ) and
         (x_lr <= maxx ) and
         (y_lr <= maxy ) then
       begin
        fg_ptr:=int8u_ptr(ptrcomp(_source_image.row(y_lr ) ) + (x_lr + x_lr + x_lr ) * sizeof(int8u ) );

        inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
        inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
        inc(fg[2 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

        inc(src_alpha ,weight * base_mask );

       end
      else
       begin
        inc(fg[m_order.R ] ,back_r * weight );
        inc(fg[m_order.G ] ,back_g * weight );
        inc(fg[m_order.B ] ,back_b * weight );

        inc(src_alpha ,back_a * weight );

       end;

      inc(x_lr );

      weight:=
       shr_int32(
        int16_ptr(ptrcomp(weight_array ) + x_hr * sizeof(int16 ) )^ *
        int16_ptr(ptrcomp(weight_array ) + y_hr * sizeof(int16 ) )^ +
        image_filter_size div 2 ,image_filter_shift );

      if (x_lr >= 0 ) and
         (y_lr >= 0 ) and
         (x_lr <= maxx ) and
         (y_lr <= maxy ) then
       begin
        fg_ptr:=int8u_ptr(ptrcomp(_source_image.row(y_lr ) ) + (x_lr + x_lr + x_lr ) * sizeof(int8u ) );

        inc(fg[0 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
        inc(fg[1 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
        inc(fg[2 ] ,weight * fg_ptr^ ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

        inc(src_alpha ,weight * base_mask );

       end
      else
       begin
        inc(fg[m_order.R] ,back_r * weight );
        inc(fg[m_order.G] ,back_g * weight );
        inc(fg[m_order.B] ,back_b * weight );

        inc(src_alpha ,back_a * weight );

       end;

      fg[0 ]:=fg[0 ] shr image_filter_shift;
      fg[1 ]:=fg[0 ] shr image_filter_shift;
      fg[2 ]:=fg[0 ] shr image_filter_shift;

      src_alpha:=src_alpha shr image_filter_shift;

      if src_alpha > base_mask then
       src_alpha:=base_mask;

      if fg[0 ] > src_alpha then
       fg[0 ]:=src_alpha;

      if fg[1 ] > src_alpha then
       fg[1 ]:=src_alpha;

      if fg[2 ] > src_alpha then
       fg[2 ]:=src_alpha;

     end;

   end;

  span.r:=int8u(fg[m_order.R ] );
  span.g:=int8u(fg[m_order.G ] );
  span.b:=int8u(fg[m_order.B ] );
  span.a:=int8u(src_alpha );

  inc(ptrcomp(span ) ,sizeof(aggclr ) );

  _interpolator.inc_operator;

  dec(len );

 until len = 0;

 result:=_allocator.span;

end;

{ CONSTRUCT }
constructor span_image_filter_rgb.Construct(alloc : span_allocator_ptr; order : order_type );
begin
 inherited Construct(alloc );

 m_order:=order;

end;

{ CONSTRUCT }
constructor span_image_filter_rgb.Construct(
             alloc      : span_allocator_ptr;
             src        : rendering_buffer_ptr;
             back_color : aggclr_ptr;
             inter      : span_interpolator_ptr;
             filter     : image_filter_lut_ptr;
             order      : order_type );
begin
 inherited Construct(alloc ,src ,back_color ,inter ,filter );

 m_order:=order;

end;

{ GENERATE }
function span_image_filter_rgb.generate;
var
 fg : array[0..2 ] of int;

 src_alpha ,start ,start1 ,maxx ,maxy ,maxx2 ,maxy2 ,x_count ,weight_y ,weight ,
 x_hr ,y_hr ,x_lr ,y_lr ,x_fract : int;

 back_r ,back_g ,back_b ,back_a : int8u;

 fg_ptr : int8u_ptr;

 diameter ,step_back ,y_count : unsigned;

 weight_array : int16_ptr;

 span : aggclr_ptr;

begin
 _interpolator.begin_(x + filter_dx_dbl ,y + filter_dy_dbl ,len );

 back_r:=_background_color.r;
 back_g:=_background_color.g;
 back_b:=_background_color.b;
 back_a:=_background_color.a;

 diameter    :=_filter.diameter;
 start       :=_filter.start;
 start1      :=start - 1;
 weight_array:=_filter.weight_array;

 step_back:=diameter * 3;

 span:=_allocator.span;

 maxx:=_source_image._width + start - 2;
 maxy:=_source_image._height + start - 2;

 maxx2:=_source_image._width - start - 1;
 maxy2:=_source_image._height - start - 1;

 repeat
  _interpolator.coordinates(@x ,@y );

  dec(x ,filter_dx_int );
  dec(y ,filter_dy_int );

  x_hr:=x;
  y_hr:=y;

  x_lr:=shr_int32(x_hr ,image_subpixel_shift );
  y_lr:=shr_int32(y_hr ,image_subpixel_shift );

  fg[0 ]:=image_filter_size div 2;
  fg[1 ]:=fg[0 ];
  fg[2 ]:=fg[0 ];

  x_fract:=x_hr and image_subpixel_mask;
  y_count:=diameter;

  if (x_lr >= -start ) and
     (y_lr >= -start ) and
     (x_lr <= maxx ) and
     (y_lr <= maxy ) then
   begin
    y_hr  :=image_subpixel_mask - (y_hr and image_subpixel_mask );
    fg_ptr:=int8u_ptr(ptrcomp(_source_image.row(y_lr + start ) ) + (x_lr + start ) * 3 * sizeof(int8u ) );

    repeat
     x_count :=diameter;
     weight_y:=int16_ptr(ptrcomp(weight_array ) + y_hr * sizeof(int16 ) )^;
     x_hr    :=image_subpixel_mask - x_fract;

     repeat
      weight:=
       shr_int32(
        weight_y *
        int16_ptr(ptrcomp(weight_array ) + x_hr * sizeof(int16 ) )^ +
        image_filter_size div 2 ,image_filter_shift );

      inc(fg[0 ] ,fg_ptr^ * weight ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
      inc(fg[1 ] ,fg_ptr^ * weight ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
      inc(fg[2 ] ,fg_ptr^ * weight ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

      inc(x_hr ,image_subpixel_size );
      dec(x_count );

     until x_count = 0;

     inc(y_hr ,image_subpixel_size );

     fg_ptr:=_source_image.next_row(int8u_ptr(ptrcomp(fg_ptr ) - step_back * sizeof(int8u ) ) );

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

    src_alpha:=base_mask;

   end
  else
   begin
    if (x_lr < start1 ) or
       (y_lr < start1 ) or
       (x_lr > maxx2 ) or
       (y_lr > maxy2 ) then
     begin
      fg[m_order.R ]:=back_r;
      fg[m_order.G ]:=back_g;
      fg[m_order.B ]:=back_b;
      src_alpha     :=back_a;

     end
    else
     begin
      src_alpha:=image_filter_size div 2;

      y_lr:=shr_int32(y ,image_subpixel_shift ) + start;
      y_hr:=image_subpixel_mask - (y_hr and image_subpixel_mask );

      repeat
       x_count :=diameter;
       weight_y:=int16_ptr(ptrcomp(weight_array ) + y_hr * sizeof(int16 ) )^;

       x_lr:=shr_int32(x ,image_subpixel_shift ) + start;
       x_hr:=image_subpixel_mask - x_fract;

       repeat
        weight:=
         shr_int32(
          weight_y *
          int16_ptr(ptrcomp(weight_array ) + x_hr * sizeof(int16 ) )^ +
          image_filter_size div 2 ,image_filter_shift );

        if (x_lr >= 0 ) and
           (y_lr >= 0 ) and
           (x_lr < trunc(_source_image._width ) ) and
           (y_lr < trunc(_source_image._height ) ) then
         begin
          fg_ptr:=int8u_ptr(ptrcomp(_source_image.row(y_lr ) ) + x_lr * 3 * sizeof(int8u ) );

          inc(fg[0 ] ,fg_ptr^ * weight ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
          inc(fg[1 ] ,fg_ptr^ * weight ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );
          inc(fg[2 ] ,fg_ptr^ * weight ); inc(ptrcomp(fg_ptr ) ,sizeof(int8u ) );

          inc(src_alpha ,base_mask * weight );

         end
        else
         begin
          inc(fg[m_order.R ] ,back_r * weight );
          inc(fg[m_order.G ] ,back_g * weight );
          inc(fg[m_order.B ] ,back_b * weight );

          inc(src_alpha ,back_a * weight );

         end;

        inc(x_hr ,image_subpixel_size );
        inc(x_lr );
        dec(x_count );

       until x_count = 0;

       inc(y_hr ,image_subpixel_size );
       inc(y_lr );
       dec(y_count );

      until y_count = 0;

      fg[0 ]:=shr_int32(fg[0 ] ,image_filter_shift );
      fg[1 ]:=shr_int32(fg[1 ] ,image_filter_shift );
      fg[2 ]:=shr_int32(fg[2 ] ,image_filter_shift );

      src_alpha:=shr_int32(src_alpha ,image_filter_shift );

      if fg[0 ] < 0 then
       fg[0 ]:=0;

      if fg[1 ] < 0 then
       fg[1 ]:=0;

      if fg[2 ] < 0 then
       fg[2 ]:=0;

      if src_alpha < 0 then
       src_alpha:=0;

      if src_alpha > base_mask then
       src_alpha:=base_mask;

      if fg[0 ] > src_alpha then
       fg[0 ]:=src_alpha;

      if fg[1 ] > src_alpha then
       fg[1 ]:=src_alpha;

      if fg[2 ] > src_alpha then
       fg[2 ]:=src_alpha;

     end;

   end;

  span.r:=int8u(fg[m_order.R ] );
  span.g:=int8u(fg[m_order.G ] );
  span.b:=int8u(fg[m_order.B ] );
  span.a:=int8u(src_alpha );

  inc(ptrcomp(span ) ,sizeof(aggclr ) );

  _interpolator.inc_operator;

  dec(len );

 until len = 0;

 result:=_allocator.span;

end;

END.

