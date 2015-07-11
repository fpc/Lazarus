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
{ agg_span_interpolator_persp.pas }
unit
 agg_span_interpolator_persp ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
 agg_basics ,
 agg_trans_perspective ,
 agg_span_interpolator_linear ,
 agg_dda_line ;

{ TYPES DEFINITION }
type
 span_interpolator_persp_exact = object(span_interpolator )
   m_trans_dir ,
   m_trans_inv : trans_perspective23;
   m_iterator  : agg_trans_perspective.iterator_x23;
   m_scale_x   ,
   m_scale_y   : dda2_line_interpolator;

   constructor Construct(SS : unsigned = 8 ); overload;

  // Arbitrary quadrangle transformations
   constructor Construct(src ,dst : double_ptr; SS : unsigned = 8 ); overload;

  // Direct transformations
   constructor Construct(x1 ,y1 ,x2 ,y2 : double; quad : double_ptr; SS : unsigned = 8 ); overload;

  // Reverse transformations
   constructor Construct(quad : double_ptr; x1 ,y1 ,x2 ,y2 : double; SS : unsigned = 8 ); overload;

  // Set the transformations using two arbitrary quadrangles.
   procedure quad_to_quad(src ,dst : double_ptr );

  // Set the direct transformations, i.e., rectangle -> quadrangle
   procedure rect_to_quad(x1 ,y1 ,x2 ,y2 : double; quad : double_ptr );

  // Set the reverse transformations, i.e., quadrangle -> rectangle
   procedure quad_to_rect(quad : double_ptr; x1 ,y1 ,x2 ,y2 : double );

  // Check if the equations were solved successfully
   function  is_valid : boolean;

  // Span interpolator interface
   procedure begin_(x ,y  : double; len : unsigned ); virtual;

   procedure resynchronize(xe ,ye : double; len : unsigned ); virtual;

   procedure inc_operator; virtual;
   procedure coordinates(x ,y : int_ptr ); virtual;

   procedure local_scale(x ,y : int_ptr ); virtual;

   procedure transform(x ,y : double_ptr );

  end;

 span_interpolator_persp_lerp = object(span_interpolator )
   m_trans_dir ,
   m_trans_inv : trans_perspective23;

   m_coord_x ,
   m_coord_y ,
   m_scale_x ,
   m_scale_y : dda2_line_interpolator;

   constructor Construct(SS : unsigned = 8 ); overload;

  // Arbitrary quadrangle transformations
   constructor Construct(src ,dst : double_ptr; SS : unsigned = 8 ); overload;

  // Direct transformations
   constructor Construct(x1 ,y1 ,x2 ,y2 : double; quad : double_ptr; SS : unsigned = 8 ); overload;

  // Reverse transformations
   constructor Construct(quad : double_ptr; x1 ,y1 ,x2 ,y2 : double; SS : unsigned = 8 ); overload;

  // Set the transformations using two arbitrary quadrangles.
   procedure quad_to_quad(src ,dst : double_ptr );

  // Set the direct transformations, i.e., rectangle -> quadrangle
   procedure rect_to_quad(x1 ,y1 ,x2 ,y2 : double; quad : double_ptr );

  // Set the reverse transformations, i.e., quadrangle -> rectangle
   procedure quad_to_rect(quad : double_ptr; x1 ,y1 ,x2 ,y2 : double );

  // Check if the equations were solved successfully
   function  is_valid : boolean;

  // Span interpolator interface
   procedure begin_(x ,y  : double; len : unsigned ); virtual;

   procedure resynchronize(xe ,ye : double; len : unsigned ); virtual;

   procedure inc_operator; virtual;
   procedure coordinates(x ,y : int_ptr ); virtual;

   procedure local_scale(x ,y : int_ptr ); virtual;

   procedure transform(x ,y : double_ptr );

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor span_interpolator_persp_exact.Construct(SS : unsigned = 8 );
begin
 inherited Construct(SS );

 m_trans_dir.Construct;
 m_trans_inv.Construct;
 m_iterator.Construct;

end;

{ CONSTRUCT }
constructor span_interpolator_persp_exact.Construct(src ,dst : double_ptr; SS : unsigned = 8 );
begin
 inherited Construct(SS );

 m_trans_dir.Construct;
 m_trans_inv.Construct;
 m_iterator.Construct;

 quad_to_quad(src ,dst );

end;

{ CONSTRUCT }
constructor span_interpolator_persp_exact.Construct(x1 ,y1 ,x2 ,y2 : double; quad : double_ptr; SS : unsigned = 8 );
begin
 inherited Construct(SS );

 m_trans_dir.Construct;
 m_trans_inv.Construct;
 m_iterator.Construct;

 rect_to_quad(x1 ,y1 ,x2 ,y2 ,quad );

end;

{ CONSTRUCT }
constructor span_interpolator_persp_exact.Construct(quad : double_ptr; x1 ,y1 ,x2 ,y2 : double; SS : unsigned = 8 );
begin
 inherited Construct(SS );

 m_trans_dir.Construct;
 m_trans_inv.Construct;
 m_iterator.Construct;

 quad_to_rect(quad ,x1 ,y1 ,x2 ,y2 );

end;

{ QUAD_TO_QUAD }
procedure span_interpolator_persp_exact.quad_to_quad;
begin
 m_trans_dir.quad_to_quad(src ,dst );
 m_trans_inv.quad_to_quad(dst ,src );

end;

{ RECT_TO_QUAD }
procedure span_interpolator_persp_exact.rect_to_quad;
var
 src : array[0..7 ] of double;

begin
 src[0 ]:=x1;
 src[6 ]:=x1;
 src[2 ]:=x2;
 src[4 ]:=x2;
 src[1 ]:=y1;
 src[3 ]:=y1;
 src[5 ]:=y2;
 src[7 ]:=y2;

 quad_to_quad(@src ,quad );

end;

{ QUAD_TO_RECT }
procedure span_interpolator_persp_exact.quad_to_rect;
var
 dst : array[0..7 ] of double;

begin
 dst[0 ]:=x1;
 dst[6 ]:=x1;
 dst[2 ]:=x2;
 dst[4 ]:=x2;
 dst[1 ]:=y1;
 dst[3 ]:=y1;
 dst[5 ]:=y2;
 dst[7 ]:=y2;

 quad_to_quad(quad ,@dst );

end;

{ IS_VALID }
function span_interpolator_persp_exact.is_valid;
begin
 result:=m_trans_dir.is_valid;

end;

{ BEGIN_ }
procedure span_interpolator_persp_exact.begin_;
var
 xt ,yt ,dx ,dy ,delta : double;

 sx1 ,sy1 ,sx2 ,sy2 : int;

begin
 m_iterator:=m_trans_dir.begin_(x ,y ,1.0 );

 xt:=m_iterator.x;
 yt:=m_iterator.y;

 delta:=1 / subpixel_size;

 dx:=xt + delta;
 dy:=yt;

 m_trans_inv.transform(@m_trans_inv ,@dx ,@dy );

 dx :=dx - x;
 dy :=dy - y;
 sx1:=shr_int32(trunc(subpixel_size / Sqrt(dx * dx + dy * dy ) ) ,subpixel_shift );
 dx :=xt;
 dy :=yt + delta;

 m_trans_inv.transform(@m_trans_inv ,@dx ,@dy );

 dx :=dx - x;
 dy :=dy - y;
 sy1:=shr_int32(trunc(subpixel_size / Sqrt(dx * dx + dy * dy ) ) ,subpixel_shift );

 x :=x + len;
 xt:=x;
 yt:=y;

 m_trans_dir.transform(@m_trans_dir ,@xt ,@yt );

 dx:=xt + delta;
 dy:=yt;

 m_trans_inv.transform(@m_trans_inv ,@dx ,@dy );

 dx :=dx- x;
 dy :=dy - y;
 sx2:=shr_int32(trunc(subpixel_size / Sqrt(dx * dx + dy * dy ) ) ,subpixel_shift );
 dx :=xt;
 dy :=yt + delta;

 m_trans_inv.transform(@m_trans_inv ,@dx ,@dy );

 dx :=dx - x;
 dy :=dy - y;
 sy2:=shr_int32(trunc(subpixel_size / Sqrt(dx * dx + dy * dy ) ) ,subpixel_shift );

 m_scale_x.Construct(sx1 ,sx2 ,len );
 m_scale_y.Construct(sy1 ,sy2 ,len );

end;

{ RESYNCHRONIZE }
procedure span_interpolator_persp_exact.resynchronize;
var
 sx1 ,sy1 ,sx2 ,sy2 : int;

 xt ,yt ,delta ,dx ,dy : double;

begin
// Assume x1,y1 are equal to the ones at the previous end point
 sx1:=m_scale_x._y;
 sy1:=m_scale_y._y;

// Calculate transformed coordinates at x2,y2
 xt:=xe;
 yt:=ye;

 m_trans_dir.transform(@m_trans_dir ,@xt ,@yt );

 delta:=1 / subpixel_size;

// Calculate scale by X at x2,y2
 dx:=xt + delta;
 dy:=yt;

 m_trans_inv.transform(@m_trans_inv ,@dx ,@dy );

 dx :=dx - xe;
 dy :=dy - ye;
 sx2:=shr_int32(trunc(subpixel_size / Sqrt(dx * dx + dy * dy ) ) ,subpixel_shift );

// Calculate scale by Y at x2,y2
 dx:=xt;
 dy:=yt + delta;

 m_trans_inv.transform(@m_trans_inv ,@dx ,@dy );

 dx :=dx - xe;
 dy :=dy - ye;
 sy2:=shr_int32(trunc(subpixel_size / Sqrt(dx * dx + dy * dy ) ) ,subpixel_shift );

// Initialize the interpolators
 m_scale_x.Construct(sx1 ,sx2 ,len );
 m_scale_y.Construct(sy1 ,sy2 ,len );

end;

{ INC_OPERATOR }
procedure span_interpolator_persp_exact.inc_operator;
begin
 m_iterator.inc_operator;
 m_scale_x.plus_operator;
 m_scale_y.plus_operator;

end;

{ COORDINATES }
procedure span_interpolator_persp_exact.coordinates;
begin
 x^:=trunc(m_iterator.x * subpixel_size + 0.5 );
 y^:=trunc(m_iterator.y * subpixel_size + 0.5 );

end;

{ LOCAL_SCALE }
procedure span_interpolator_persp_exact.local_scale;
begin
 x^:= m_scale_x._y;
 y^:= m_scale_y._y;

end;

{ TRANSFORM }
procedure span_interpolator_persp_exact.transform;
begin
 m_trans_dir.transform(@m_trans_dir ,x ,y );

end;

{ CONSTRUCT }
constructor span_interpolator_persp_lerp.Construct(SS : unsigned = 8 );
begin
 inherited Construct(SS );

 m_trans_dir.Construct;
 m_trans_inv.Construct;

end;

{ CONSTRUCT }
constructor span_interpolator_persp_lerp.Construct(src ,dst : double_ptr; SS : unsigned = 8 );
begin
 inherited Construct(SS );

 m_trans_dir.Construct;
 m_trans_inv.Construct;

 quad_to_quad(src ,dst );

end;

{ CONSTRUCT }
constructor span_interpolator_persp_lerp.Construct(x1 ,y1 ,x2 ,y2 : double; quad : double_ptr; SS : unsigned = 8 );
begin
 inherited Construct(SS );

 m_trans_dir.Construct;
 m_trans_inv.Construct;

 rect_to_quad(x1 ,y1 ,x2 ,y2 ,quad );

end;

{ CONSTRUCT }
constructor span_interpolator_persp_lerp.Construct(quad : double_ptr; x1 ,y1 ,x2 ,y2 : double; SS : unsigned = 8 );
begin
 inherited Construct(SS );

 m_trans_dir.Construct;
 m_trans_inv.Construct;

 quad_to_rect(quad ,x1 ,y1 ,x2 ,y2 );

end;

{ QUAD_TO_QUAD }
procedure span_interpolator_persp_lerp.quad_to_quad;
begin
 m_trans_dir.quad_to_quad(src ,dst );
 m_trans_inv.quad_to_quad(dst ,src );

end;

{ RECT_TO_QUAD }
procedure span_interpolator_persp_lerp.rect_to_quad;
var
 src : array[0..7 ] of double;

begin
 src[0 ]:=x1;
 src[6 ]:=x1;
 src[2 ]:=x2;
 src[4 ]:=x2;
 src[1 ]:=y1;
 src[3 ]:=y1;
 src[5 ]:=y2;
 src[7 ]:=y2;

 quad_to_quad(@src ,quad );

end;

{ QUAD_TO_RECT }
procedure span_interpolator_persp_lerp.quad_to_rect;
var
 dst : array[0..7 ] of double;

begin
 dst[0 ]:=x1;
 dst[6 ]:=x1;
 dst[2 ]:=x2;
 dst[4 ]:=x2;
 dst[1 ]:=y1;
 dst[3 ]:=y1;
 dst[5 ]:=y2;
 dst[7 ]:=y2;

 quad_to_quad(quad ,@dst );

end;

{ IS_VALID }
function span_interpolator_persp_lerp.is_valid;
begin
 result:=m_trans_dir.is_valid;

end;

{ BEGIN_ }
procedure span_interpolator_persp_lerp.begin_;
var
 xt ,yt ,dx ,dy ,delta : double;

 x1 ,y1 ,sx1 ,sy1 ,x2 ,y2 ,sx2 ,sy2 : int;

begin
// Calculate transformed coordinates at x1,y1
 xt:=x;
 yt:=y;

 m_trans_dir.transform(@m_trans_dir ,@xt ,@yt );

 x1:=trunc(xt * subpixel_size );
 y1:=trunc(yt * subpixel_size );

 delta:=1 / subpixel_size;

// Calculate scale by X at x1,y1
 dx:=xt + delta;
 dy:=yt;

 m_trans_inv.transform(@m_trans_inv ,@dx ,@dy );

 dx :=dx - x;
 dy :=dy - y;
 sx1:=shr_int32(trunc(subpixel_size / Sqrt(dx * dx + dy * dy ) ) ,subpixel_shift );

// Calculate scale by Y at x1,y1
 dx:=xt;
 dy:=yt + delta;

 m_trans_inv.transform(@m_trans_inv ,@dx ,@dy );

 dx :=dx - x;
 dy :=dy - y;
 sy1:=shr_int32(trunc(subpixel_size / Sqrt(dx * dx + dy * dy ) ) ,subpixel_shift );

// Calculate transformed coordinates at x2,y2
 x :=x + len;
 xt:=x;
 yt:=y;

 m_trans_dir.transform(@m_trans_dir ,@xt ,@yt );

 x2:=trunc(xt * subpixel_size );
 y2:=trunc(yt * subpixel_size );

// Calculate scale by X at x2,y2
 dx:=xt + delta;
 dy:=yt;

 m_trans_inv.transform(@m_trans_inv ,@dx ,@dy );

 dx :=dx - x;
 dy :=dy - y;
 sx2:=shr_int32(trunc(subpixel_size / Sqrt(dx * dx + dy * dy ) ) ,subpixel_shift );

// Calculate scale by Y at x2,y2
 dx:=xt;
 dy:=yt + delta;

 m_trans_inv.transform(@m_trans_inv ,@dx ,@dy );

 dx :=dx - x;
 dy :=dy - y;
 sy2:=shr_int32(trunc(subpixel_size / Sqrt(dx * dx + dy * dy ) ) ,subpixel_shift );

// Initialize the interpolators
 m_coord_x.Construct(x1  ,x2  ,len );
 m_coord_y.Construct(y1  ,y2  ,len );
 m_scale_x.Construct(sx1 ,sx2 ,len );
 m_scale_y.Construct(sy1 ,sy2 ,len );

end;

{ RESYNCHRONIZE }
procedure span_interpolator_persp_lerp.resynchronize;
var
 x1 ,y1 ,sx1 ,sy1 ,x2 ,y2 ,sx2 ,sy2 : int;

 xt ,yt ,delta ,dx ,dy : double;

begin
// Assume x1,y1 are equal to the ones at the previous end point
 x1 :=m_coord_x._y;
 y1 :=m_coord_y._y;
 sx1:=m_scale_x._y;
 sy1:=m_scale_y._y;

// Calculate transformed coordinates at x2,y2
 xt:=xe;
 yt:=ye;

 m_trans_dir.transform(@m_trans_dir ,@xt ,@yt );

 x2:=trunc(xt * subpixel_size );
 y2:=trunc(yt * subpixel_size );

 delta:=1 / subpixel_size;

// Calculate scale by X at x2,y2
 dx:=xt + delta;
 dy:=yt;

 m_trans_inv.transform(@m_trans_inv ,@dx ,@dy );

 dx :=dx - xe;
 dy :=dy - ye;
 sx2:=shr_int32(trunc(subpixel_size / Sqrt(dx * dx + dy * dy ) ) ,subpixel_shift );

// Calculate scale by Y at x2,y2
 dx:=xt;
 dy:=yt + delta;

 m_trans_inv.transform(@m_trans_inv ,@dx ,@dy );

 dx :=dx - xe;
 dy :=dy - ye;
 sy2:=shr_int32(trunc(subpixel_size / Sqrt(dx * dx + dy * dy ) ) ,subpixel_shift );

// Initialize the interpolators
 m_coord_x.Construct(x1  ,x2  ,len );
 m_coord_y.Construct(y1  ,y2  ,len );
 m_scale_x.Construct(sx1 ,sx2 ,len );
 m_scale_y.Construct(sy1 ,sy2 ,len );

end;

{ INC_OPERATOR }
procedure span_interpolator_persp_lerp.inc_operator;
begin
 m_coord_x.plus_operator;
 m_coord_y.plus_operator;
 m_scale_x.plus_operator;
 m_scale_y.plus_operator;

end;

{ COORDINATES }
procedure span_interpolator_persp_lerp.coordinates;
begin
 x^:=m_coord_x._y;
 y^:=m_coord_y._y;

end;

{ LOCAL_SCALE }
procedure span_interpolator_persp_lerp.local_scale;
begin
 x^:=m_scale_x._y;
 y^:=m_scale_y._y;

end;

{ TRANSFORM }
procedure span_interpolator_persp_lerp.transform;
begin
 m_trans_dir.transform(@m_trans_dir ,x ,y );

end;

END.

