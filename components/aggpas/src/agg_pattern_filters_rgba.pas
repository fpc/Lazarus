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
// 02.02.2006-Milano: Unit port establishment
//
{ agg_pattern_filters_rgba.pas }
unit
 agg_pattern_filters_rgba ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_line_aa_basics ,
 agg_color ;

{ TYPES DEFINITION }
type
 pattern_filter_ptr = ^pattern_filter;
 pattern_filter = object
   constructor Construct;

   function  dilation : unsigned; virtual; abstract;
   procedure pixel_low_res (buf : pointer; p : aggclr_ptr; x ,y : int ); virtual; abstract;
   procedure pixel_high_res(buf : pointer; p : aggclr_ptr; x ,y : int ); virtual; abstract;

  end;

 pattern_filter_nn = object(pattern_filter )
   function  dilation : unsigned; virtual;
   procedure pixel_low_res (buf : pointer; p : aggclr_ptr; x ,y : int ); virtual;
   procedure pixel_high_res(buf : pointer; p : aggclr_ptr; x ,y : int ); virtual;

  end;

 pattern_filter_bilinear_rgba = object(pattern_filter )
   function  dilation : unsigned; virtual;
   procedure pixel_low_res (buf : pointer; p : aggclr_ptr; x ,y : int ); virtual;
   procedure pixel_high_res(buf : pointer; p : aggclr_ptr; x ,y : int ); virtual;

  end;

 pattern_filter_bilinear_gray8 = object(pattern_filter_bilinear_rgba )
   procedure pixel_high_res(buf : pointer; p : aggclr_ptr; x ,y : int ); virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor pattern_filter.Construct;
begin
end;

{ DILATION }
function pattern_filter_nn.dilation;
begin
 result:=0;

end;

{ PIXEL_LOW_RES }
procedure pattern_filter_nn.pixel_low_res;
begin
 p^.Construct(
  rgba8_ptr(
   ptrcomp(rgba8_ptr_ptr(ptrcomp(buf ) + y * sizeof(pointer ) )^ ) +
   x * sizeof(rgba8 ) )^ );

end;

{ PIXEL_HIGH_RES }
procedure pattern_filter_nn.pixel_high_res;
begin
 p^.Construct(
  rgba8_ptr(
   ptrcomp(rgba8_ptr_ptr(ptrcomp(buf ) + shr_int32(y ,line_subpixel_shift ) * sizeof(pointer ) )^ ) +
   shr_int32(x ,line_subpixel_shift ) * sizeof(rgba8 ) )^ );

end;

{ DILATION }
function pattern_filter_bilinear_rgba.dilation;
begin
 result:=1;

end;

{ PIXEL_LOW_RES }
procedure pattern_filter_bilinear_rgba.pixel_low_res;
begin
 p^.Construct(
  rgba8_ptr(
   ptrcomp(rgba8_ptr_ptr(ptrcomp(buf ) + y * sizeof(pointer ) )^ ) +
   x * sizeof(rgba8 ) )^ );

end;

{ PIXEL_HIGH_RES }
procedure pattern_filter_bilinear_rgba.pixel_high_res;
var
 r ,g ,b ,a ,weight : int32u;

 x_lr ,y_lr : int;

 ptr : rgba8_ptr;

begin
 r:=line_subpixel_size * line_subpixel_size div 2;
 g:=r;
 b:=g;
 a:=b;

 x_lr:=shr_int32(x ,line_subpixel_shift );
 y_lr:=shr_int32(y ,line_subpixel_shift );

 x:=x and line_subpixel_mask;
 y:=y and line_subpixel_mask;

 ptr:=
  rgba8_ptr(
   ptrcomp(rgba8_ptr_ptr(ptrcomp(buf ) + y_lr * sizeof(pointer ) )^ ) +
   x_lr * sizeof(rgba8 ) );

 weight:=(line_subpixel_size - x ) * (line_subpixel_size - y );

 inc(r ,weight * ptr.r );
 inc(g ,weight * ptr.g );
 inc(b ,weight * ptr.b );
 inc(a ,weight * ptr.a );

 inc(ptrcomp(ptr ) ,sizeof(rgba8 ) );

 weight:=x * (line_subpixel_size - y );

 inc(r ,weight * ptr.r );
 inc(g ,weight * ptr.g );
 inc(b ,weight * ptr.b );
 inc(a ,weight * ptr.a );

 ptr:=
  rgba8_ptr(
   ptrcomp(rgba8_ptr_ptr(ptrcomp(buf ) + (y_lr + 1 ) * sizeof(pointer ) )^ ) +
   x_lr * sizeof(rgba8 ) );

 weight:=(line_subpixel_size - x ) * y;

 inc(r ,weight * ptr.r );
 inc(g ,weight * ptr.g );
 inc(b ,weight * ptr.b );
 inc(a ,weight * ptr.a );

 inc(ptrcomp(ptr ) ,sizeof(rgba8 ) );

 weight:=x * y;

 inc(r ,weight * ptr.r );
 inc(g ,weight * ptr.g );
 inc(b ,weight * ptr.b );
 inc(a ,weight * ptr.a );

 p.r:=int8u(r shr (line_subpixel_shift * 2 ) );
 p.g:=int8u(g shr (line_subpixel_shift * 2 ) );
 p.b:=int8u(b shr (line_subpixel_shift * 2 ) );
 p.a:=int8u(a shr (line_subpixel_shift * 2 ) );

end;

{ PIXEL_HIGH_RES }
procedure pattern_filter_bilinear_gray8.pixel_high_res;
var
 r ,g ,b ,a ,weight : int32u;

 x_lr ,y_lr : int;

 ptr : rgba8_ptr;

begin
 r:=line_subpixel_size * line_subpixel_size div 2;
 g:=r;
 b:=g;
 a:=b;

 x_lr:=shr_int32(x ,line_subpixel_shift );
 y_lr:=shr_int32(y ,line_subpixel_shift );

 x:=x and line_subpixel_mask;
 y:=y and line_subpixel_mask;

 ptr:=
  rgba8_ptr(
   ptrcomp(rgba8_ptr_ptr(ptrcomp(buf ) + y_lr * sizeof(pointer ) )^ ) +
   x_lr * sizeof(rgba8 ) );

 weight:=(line_subpixel_size - x ) * (line_subpixel_size - y );

 inc(r ,weight * ptr.r );
 inc(g ,weight * ptr.g );
 inc(b ,weight * ptr.b );
 inc(a ,weight * ptr.a );

 inc(ptrcomp(ptr ) ,sizeof(rgba8 ) );

 weight:=x * (line_subpixel_size - y );

 inc(r ,weight * ptr.r );
 inc(g ,weight * ptr.g );
 inc(b ,weight * ptr.b );
 inc(a ,weight * ptr.a );

 ptr:=
  rgba8_ptr(
   ptrcomp(rgba8_ptr_ptr(ptrcomp(buf ) + (y_lr + 1 ) * sizeof(pointer ) )^ ) +
   x_lr * sizeof(rgba8 ) );

 weight:=(line_subpixel_size - x ) * y;

 inc(r ,weight * ptr.r );
 inc(g ,weight * ptr.g );
 inc(b ,weight * ptr.b );
 inc(a ,weight * ptr.a );

 inc(ptrcomp(ptr ) ,sizeof(rgba8 ) );

 weight:=x * y;

 inc(r ,weight * ptr.r );
 inc(g ,weight * ptr.g );
 inc(b ,weight * ptr.b );
 inc(a ,weight * ptr.a );

 p.r:=int8u(r shr (line_subpixel_shift * 2 ) );
 p.g:=int8u(g shr (line_subpixel_shift * 2 ) );
 p.b:=int8u(b shr (line_subpixel_shift * 2 ) );
 p.v:=(p.r * 77 + p.g * 150 + p.b * 29 ) shr 8;
 p.a:=int8u(a shr (line_subpixel_shift * 2 ) );

end;

END.

