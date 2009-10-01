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
// 12.10.2007-Milano: gradient_radial_focus_extended
// 23.06.2006-Milano: ptrcomp adjustments
// 27.01.2006-Milano: Unit port establishment
//
{ agg_span_gradient.pas }
unit
 agg_span_gradient ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
 Math ,
 agg_basics ,
 agg_span_allocator ,
 agg_span_generator ,
 agg_math ,
 agg_array ,
 agg_span_interpolator_linear ,
 agg_color ;

{ TYPES DEFINITION }
const
 gradient_subpixel_shift = 4;                              //-----gradient_subpixel_shift
 gradient_subpixel_size  = 1 shl gradient_subpixel_shift;  //-----gradient_subpixel_size
 gradient_subpixel_mask  = gradient_subpixel_size - 1;     //-----gradient_subpixel_mask

type
 gradient_linear_color_ptr = ^gradient_linear_color;

 gradient_ptr = ^gradient;

 span_gradient = object(span_generator )
   downscale_shift : int;

   m_interpolator      : span_interpolator_ptr;
   m_gradient_function : gradient_ptr;
   m_color_function    : array_base_ptr;

   m_d1 ,
   m_d2 : int;

   constructor Construct(alloc : span_allocator_ptr ); overload;
   constructor Construct(
                alloc : span_allocator_ptr;
                inter : span_interpolator_ptr;
                gradient_function : gradient_ptr;
                color_function    : array_base_ptr;
                d1 ,d2 : double ); overload;

   function _interpolator : span_interpolator_ptr;
   function _gradient_function : gradient_ptr;
   function _color_function : array_base_ptr;
   function _d1 : double;
   function _d2 : double;

   procedure interpolator_     (i : span_interpolator_ptr );
   procedure gradient_function_(gf : gradient_ptr );
   procedure color_function_   (cf : array_base_ptr );

   procedure d1_(v : double );
   procedure d2_(v : double );

   function  generate(x ,y : int; len : unsigned ) : aggclr_ptr; virtual;

  end;

 gradient_linear_color = object(pod_auto_array )
   m_c1  ,
   m_c2  ,
   m_res : aggclr;

   constructor Construct(c1 ,c2 : aggclr_ptr; size_ : unsigned = 256 );

   function  size : unsigned; virtual;
   function  array_operator(i : unsigned ) : pointer; virtual;

   procedure colors_(c1 ,c2 : aggclr_ptr; size_ : unsigned = 256 );

  end;

 gradient = object
   constructor Construct;

   function  calculate(x ,y ,d : int ) : int; virtual; abstract;

  end;

// Actually the same as radial. Just for compatibility
 gradient_circle = object(gradient )
   function  calculate(x ,y ,d : int ) : int; virtual;

  end;

 gradient_radial = object(gradient )
   function  calculate(x ,y ,d : int ) : int; virtual;

  end;

 gradient_radial_d = object(gradient )
   function  calculate(x ,y ,d : int ) : int; virtual;

  end;

 gradient_radial_focus = object(gradient )
   m_radius  ,
   m_focus_x ,
   m_focus_y : int;

   m_radius2 ,
   m_trivial : double;

   constructor Construct; overload;
   constructor Construct(r ,fx ,fy : double ); overload;

   procedure init(r ,fx ,fy : double );

   function  radius : double;
   function  focus_x : double;
   function  focus_y : double;

   function  calculate(x ,y ,d : int ) : int; virtual;

   procedure update_values;

  end;

 gradient_radial_focus_extended = object(gradient )
   m_r   ,
   m_fx  ,
   m_fy  : int;
   m_r2  ,
   m_fx2 ,
   m_fy2 ,
   m_mul : double;

   constructor Construct; overload;
   constructor Construct(r ,fx ,fy : double ); overload;

   procedure init(r ,fx ,fy : double );

   function  radius : double;
   function  focus_x : double;
   function  focus_y : double;

   function  calculate(x ,y ,d : int ) : int; virtual;

   procedure update_values;

  end;

 gradient_x = object(gradient )
   function  calculate(x ,y ,d : int ) : int; virtual;

  end;

 gradient_y = object(gradient )
   function  calculate(x ,y ,d : int ) : int; virtual;

  end;

 gradient_diamond = object(gradient )
   function  calculate(x ,y ,d : int ) : int; virtual;

  end;

 gradient_xy = object(gradient )
   function  calculate(x ,y ,d : int ) : int; virtual;

  end;

 gradient_sqrt_xy = object(gradient )
   function  calculate(x ,y ,d : int ) : int; virtual;

  end;

 gradient_conic = object(gradient )
   function  calculate(x ,y ,d : int ) : int; virtual;

  end;

 gradient_repeat_adaptor = object(gradient )
   m_gradient : gradient_ptr;

   constructor Construct(gradient : gradient_ptr );

   function  calculate(x ,y ,d : int ) : int; virtual;

  end;

 gradient_reflect_adaptor = object(gradient )
   m_gradient : gradient_ptr;

   constructor Construct(gradient : gradient_ptr );

   function  calculate(x ,y ,d : int ) : int; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor span_gradient.Construct(alloc : span_allocator_ptr );
begin
 inherited Construct(alloc );

end;

{ CONSTRUCT }
constructor span_gradient.Construct(
                           alloc : span_allocator_ptr;
                           inter : span_interpolator_ptr;
                           gradient_function : gradient_ptr;
                           color_function    : array_base_ptr;
                           d1 ,d2 : double );
begin
 inherited Construct(alloc );

 m_interpolator     :=inter;
 m_gradient_function:=gradient_function;
 m_color_function   :=color_function;

 downscale_shift:=m_interpolator.subpixel_shift - gradient_subpixel_shift;

 m_d1:=trunc(d1 * gradient_subpixel_size );
 m_d2:=trunc(d2 * gradient_subpixel_size );

end;

{ _INTERPOLATOR }
function span_gradient._interpolator;
begin
 result:=m_interpolator;

end;

{ _GRADIENT_FUNCTION }
function span_gradient._gradient_function;
begin
 result:=m_gradient_function;

end;

{ _COLOR_FUNCTION }
function span_gradient._color_function;
begin
 result:=m_color_function;

end;

{ _D1 }
function span_gradient._d1;
begin
 result:=m_d1 / gradient_subpixel_size;

end;

{ _D2 }
function span_gradient._d2;
begin
 result:=m_d2 / gradient_subpixel_size;

end;

{ INTERPOLATOR_ }
procedure span_gradient.interpolator_;
begin
 m_interpolator:=i;

end;

{ GRADIENT_FUNCTION_ }
procedure span_gradient.gradient_function_;
begin
 m_gradient_function:=gf;

end;

{ COLOR_FUNCTION_ }
procedure span_gradient.color_function_;
begin
 m_color_function:=cf;

end;

{ D1_ }
procedure span_gradient.d1_;
begin
 m_d1:=trunc(v * gradient_subpixel_size );

end;

{ D2_ }
procedure span_gradient.d2_;
begin
 m_d2:=trunc(v * gradient_subpixel_size );

end;

{ GENERATE }
function span_gradient.generate;
var
 span : aggclr_ptr;

 dd ,d : int;

begin
 span:=_allocator.span;

 dd:=m_d2 - m_d1;

 if dd < 1 then
  dd:=1;

 m_interpolator.begin_(x + 0.5 ,y + 0.5 ,len );

 repeat
  m_interpolator.coordinates(@x ,@y );

  d:=
   m_gradient_function.calculate(
    shr_int32(x ,downscale_shift ) ,
    shr_int32(y ,downscale_shift ) ,m_d2 );

  d:=((d - m_d1 ) * m_color_function.size ) div dd;

  if d < 0 then
   d:=0;

  if d >= m_color_function.size then
   d:=m_color_function.size - 1;

  span^:=aggclr_ptr(m_color_function.array_operator(d ) )^;

  inc(ptrcomp(span ) ,sizeof(aggclr ) );

  m_interpolator.inc_operator;

  dec(len );

 until len = 0;

 result:=_allocator.span;

end;

{ CONSTRUCT }
constructor gradient_linear_color.Construct;
begin
 m_c1:=c1^;
 m_c2:=c2^;

 m_size:=size_;

end;

{ SIZE }
function gradient_linear_color.size;
begin
 result:=m_size;

end;

{ ARRAY_OPERATOR }
function gradient_linear_color.array_operator;
begin
 m_res :=m_c1.gradient(@m_c2 ,i / (m_size - 1 ) );
 result:=@m_res;

end;

{ COLORS_ }
procedure gradient_linear_color.colors_;
begin
 m_c1:=c1^;
 m_c2:=c2^;

 m_size:=size;

end;

{ CONSTRUCT }
constructor gradient.Construct;
begin
end;

{ CALCULATE }
function gradient_circle.calculate;
begin
 result:=fast_sqrt(x * x + y * y );

end;

{ CALCULATE }
function gradient_radial.calculate;
begin
 result:=fast_sqrt(x * x + y * y );

end;

{ CALCULATE }
function gradient_radial_d.calculate;
begin
 result:=trunc(Sqrt(x * x + y * y ) );

end;

{ CONSTRUCT }
constructor gradient_radial_focus.Construct;
begin
 m_radius :=100 * gradient_subpixel_size;
 m_focus_x:=0;
 m_focus_y:=0;

 update_values;

end;

{ CONSTRUCT }
constructor gradient_radial_focus.Construct(r ,fx ,fy : double );
begin
 m_radius :=trunc(r  * gradient_subpixel_size );
 m_focus_x:=trunc(fx * gradient_subpixel_size );
 m_focus_y:=trunc(fy * gradient_subpixel_size );

 update_values;

end;

{ INIT }
procedure gradient_radial_focus.init;
begin
 m_radius :=trunc(r  * gradient_subpixel_size );
 m_focus_x:=trunc(fx * gradient_subpixel_size );
 m_focus_y:=trunc(fy * gradient_subpixel_size );

 update_values;

end;

{ RADIUS }
function gradient_radial_focus.radius;
begin
 result:=m_radius / gradient_subpixel_size;

end;

{ FOCUS_X }
function gradient_radial_focus.focus_x;
begin
 result:=m_focus_x / gradient_subpixel_size;

end;

{ FOCUS_Y }
function gradient_radial_focus.focus_y;
begin
 result:=m_focus_y / gradient_subpixel_size;

end;

{ CALCULATE }
function gradient_radial_focus.calculate;
var
 solution_x   ,
 solution_y   ,
 slope ,yint  ,
 a ,b ,c ,det ,
 int_to_focus ,
 cur_to_focus : double;

begin
// Special case to avoid divide by zero or very near zero
 if x = m_focus_x then
  begin
   solution_x:=m_focus_x;
   solution_y:=0.0;

   if y > m_focus_y then
    solution_y:=solution_y + m_trivial
   else
    solution_y:=solution_y - m_trivial;

  end
 else
  begin
  // Slope of the focus-current line
   slope:=(y - m_focus_y ) / (x - m_focus_x );

  // y-intercept of that same line
   yint:=y - (slope * x ); 

  // Use the classical quadratic formula to calculate
  // the intersection point
   a:=(slope * slope ) + 1;
   b:=2 * slope * yint;
   c:=yint * yint - m_radius2;

   det:=Sqrt((b * b ) - (4.0 * a * c ) );

   solution_x:=-b;

  // Choose the positive or negative root depending
  // on where the X coord lies with respect to the focus.
   if x < m_focus_x then
    solution_x:=solution_x - det
   else
    solution_x:=solution_x + det;

   solution_x:=solution_x / (2.0 * a );

  // Calculating of Y is trivial
   solution_y:=(slope * solution_x ) + yint;

  end;

// Calculate the percentage (0...1) of the current point along the
// focus-circumference line and return the normalized (0...d) value
 solution_x:=solution_x - m_focus_x;
 solution_y:=solution_y - m_focus_y;

 int_to_focus:=solution_x * solution_x + solution_y * solution_y;
 cur_to_focus:=
  (x - m_focus_x ) * (x - m_focus_x ) +
  (y - m_focus_y ) * (y - m_focus_y );

 result:=trunc(Sqrt(cur_to_focus / int_to_focus ) * m_radius );

end;

{ UPDATE_VALUES }
procedure gradient_radial_focus.update_values;
var
 dist ,r ,a : double;

begin
// For use in the quadratic equation
 m_radius2:=m_radius * m_radius;

 dist:=Sqrt(m_focus_x * m_focus_x + m_focus_y * m_focus_y );

// Test if distance from focus to center is greater than the radius
// For the sake of assurance factor restrict the point to be
// no further than 99% of the radius.
 r:=m_radius * 0.99;

 if dist > r then
  begin
  // clamp focus to radius
  // x = r cos theta, y = r sin theta
   a:=ArcTan2(m_focus_y ,m_focus_x );

   m_focus_x:=trunc(r * Cos(a ) );
   m_focus_y:=trunc(r * Sin(a ) );

  end;

// Calculate the solution to be used in the case where x == focus_x
 m_trivial:=Sqrt(m_radius2 - (m_focus_x * m_focus_x ) );

end;

{ CONSTRUCT }
constructor gradient_radial_focus_extended.Construct;
begin
 m_r :=100 * gradient_subpixel_size;
 m_fx:=0;
 m_fy:=0;

 update_values;

end;

{ CONSTRUCT }
constructor gradient_radial_focus_extended.Construct(r ,fx ,fy : double );
begin
 m_r :=iround(r  * gradient_subpixel_size );
 m_fx:=iround(fx * gradient_subpixel_size );
 m_fy:=iround(fy * gradient_subpixel_size );

 update_values;

end;

{ INIT }
procedure gradient_radial_focus_extended.init(r ,fx ,fy : double );
begin
 m_r :=iround(r  * gradient_subpixel_size );
 m_fx:=iround(fx * gradient_subpixel_size );
 m_fy:=iround(fy * gradient_subpixel_size );

 update_values;

end;

{ RADIUS }
function gradient_radial_focus_extended.radius : double;
begin
 result:=m_r / gradient_subpixel_size;

end;

{ FOCUS_X }
function gradient_radial_focus_extended.focus_x : double;
begin
 result:=m_fx / gradient_subpixel_size;

end;

{ FOCUS_Y }
function gradient_radial_focus_extended.focus_y : double;
begin
 result:=m_fy / gradient_subpixel_size;

end;

{ CALCULATE }
function gradient_radial_focus_extended.calculate(x ,y ,d : int ) : int;
var
 dx ,dy ,d2 ,d3 : double;

begin
 dx:=x - m_fx;
 dy:=y - m_fy;
 d2:=dx * m_fy - dy * m_fx;
 d3:=m_r2 * (dx * dx + dy * dy ) - d2 * d2;

 result:=iround((dx * m_fx + dy * m_fy + Sqrt(Abs(d3 ) ) ) * m_mul );

end;

{ UPDATE_VALUES }
// Calculate the invariant values. In case the focal center
// lies exactly on the gradient circle the divisor degenerates
// into zero. In this case we just move the focal center by
// one subpixel unit possibly in the direction to the origin (0,0)
// and calculate the values again.
procedure gradient_radial_focus_extended.update_values;
var
 d : double;

begin
 m_r2 :=m_r  * m_r;
 m_fx2:=m_fx * m_fx;
 m_fy2:=m_fy * m_fy;

 d:=(m_r2 - (m_fx2 + m_fy2 ) );

 if d = 0 then
  begin
   if m_fx <> 0 then
    if m_fx < 0 then
     inc(m_fx )
    else
     dec(m_fx );

   if m_fy <> 0 then
    if m_fy < 0 then
     inc(m_fy )
    else
     dec(m_fy );

   m_fx2:=m_fx * m_fx;
   m_fy2:=m_fy * m_fy;

   d:=(m_r2 - (m_fx2 + m_fy2 ) );

  end;

 m_mul:=m_r / d;

end;

{ CALCULATE }
function gradient_x.calculate;
begin
 result:=x;

end;

{ CALCULATE }
function gradient_y.calculate;
begin
 result:=y;

end;

{ CALCULATE }
function gradient_diamond.calculate;
var
 ax ,ay : int;

begin
 ax:=Abs(x );
 ay:=Abs(y );

 if ax > ay then
  result:=ax
 else
  result:=ay; 

end;

{ CALCULATE }
function gradient_xy.calculate;
begin
 if d = 0 then
  result:=0
 else
  result:=Abs(x ) * Abs(y ) div d;

end;

{ CALCULATE }
function gradient_sqrt_xy.calculate;
begin
 result:=fast_sqrt(Abs(x ) * Abs(y ) );

end;

{ CALCULATE }
function gradient_conic.calculate;
begin
 result:=trunc(Abs(ArcTan2(y ,x ) ) * d / pi );

end;

{ CONSTRUCT }
constructor gradient_repeat_adaptor.Construct;
begin
 m_gradient:=gradient;

end;

{ CALCULATE }
function gradient_repeat_adaptor.calculate;
begin
 if d = 0 then
  result:=0
 else
  result:=m_gradient.calculate(x ,y ,d ) mod d;

 if result < 0 then
  inc(result ,d );

end;

{ CONSTRUCT }
constructor gradient_reflect_adaptor.Construct;
begin
 m_gradient:=gradient;

end;

{ CALCULATE }
function gradient_reflect_adaptor.calculate;
var
 d2 : int;

begin
 d2:=d shl 1;

 if d2 = 0 then
  result:=0
 else
  result:=m_gradient.calculate(x ,y ,d ) mod d2;

 if result < 0 then
  inc(result ,d2 );

 if result >= d then
  result:=d2 - result;

end;

END.

