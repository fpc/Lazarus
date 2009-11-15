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
// The Stack Blur Algorithm was invented by Mario Klingemann, 
// mario@quasimondo.com and described here:
// http://incubator.quasimondo.com/processing/fast_blur_deluxe.php
// (search phrase "Stackblur: Fast But Goodlooking"). 
// The major improvement is that there's no more division table
// that was very expensive to create for large blur radii. Insted,
// for 8-bit per channel and radius not exceeding 254 the division is
// replaced by multiplication and shift.
//
// [Pascal Port History] -----------------------------------------------------
//
// 11.10.2007-Milano: recursive_blur & finished OK
// 10.10.2007-Milano: stack_blur
// 09.10.2007-Milano: Unit port establishment
//
{ agg_blur.pas }
unit
 agg_blur ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
 agg_basics ,
 agg_array ,
 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_transposer ;

{ GLOBAL VARIABLES & CONSTANTS }

{ TYPES DEFINITION }
type
 stack_blur = object
  private
   m_buf   ,
   m_stack : pod_vector;

  public
   constructor Construct;
   destructor  Destruct;

   procedure blur_x(img : pixel_formats_ptr; radius : unsigned );
   procedure blur_y(img : pixel_formats_ptr; radius : unsigned );

   procedure blur(img : pixel_formats_ptr; radius : unsigned );

  end;

 recursive_blur = object
  private
   m_sum1 ,
   m_sum2 ,
   m_buf  : pod_vector;

  public
   constructor Construct;
   destructor  Destruct;

   procedure blur_x(img : pixel_formats_ptr; radius : double );
   procedure blur_y(img : pixel_formats_ptr; radius : double );

   procedure blur(img : pixel_formats_ptr; radius : double );

  end;

{ GLOBAL PROCEDURES }
 procedure stack_blur_gray8 (img : pixel_formats_ptr; rx ,ry : unsigned );
 procedure stack_blur_rgb24 (img : pixel_formats_ptr; rx ,ry : unsigned );
 procedure stack_blur_rgba32(img : pixel_formats_ptr; rx ,ry : unsigned );


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
const
 g_stack_blur8_mul : array[0..254 ] of int16u = (
  512 ,512 ,456 ,512 ,328 ,456 ,335 ,512 ,405 ,328 ,271 ,456 ,388 ,335 ,292 ,512 ,
  454 ,405 ,364 ,328 ,298 ,271 ,496 ,456 ,420 ,388 ,360 ,335 ,312 ,292 ,273 ,512 ,
  482 ,454 ,428 ,405 ,383 ,364 ,345 ,328 ,312 ,298 ,284 ,271 ,259 ,496 ,475 ,456 ,
  437 ,420 ,404 ,388 ,374 ,360 ,347 ,335 ,323 ,312 ,302 ,292 ,282 ,273 ,265 ,512 ,
  497 ,482 ,468 ,454 ,441 ,428 ,417 ,405 ,394 ,383 ,373 ,364 ,354 ,345 ,337 ,328 ,
  320 ,312 ,305 ,298 ,291 ,284 ,278 ,271 ,265 ,259 ,507 ,496 ,485 ,475 ,465 ,456 ,
  446 ,437 ,428 ,420 ,412 ,404 ,396 ,388 ,381 ,374 ,367 ,360 ,354 ,347 ,341 ,335 ,
  329 ,323 ,318 ,312 ,307 ,302 ,297 ,292 ,287 ,282 ,278 ,273 ,269 ,265 ,261 ,512 ,
  505 ,497 ,489 ,482 ,475 ,468 ,461 ,454 ,447 ,441 ,435 ,428 ,422 ,417 ,411 ,405 ,
  399 ,394 ,389 ,383 ,378 ,373 ,368 ,364 ,359 ,354 ,350 ,345 ,341 ,337 ,332 ,328 ,
  324 ,320 ,316 ,312 ,309 ,305 ,301 ,298 ,294 ,291 ,287 ,284 ,281 ,278 ,274 ,271 ,
  268 ,265 ,262 ,259 ,257 ,507 ,501 ,496 ,491 ,485 ,480 ,475 ,470 ,465 ,460 ,456 ,
  451 ,446 ,442 ,437 ,433 ,428 ,424 ,420 ,416 ,412 ,408 ,404 ,400 ,396 ,392 ,388 ,
  385 ,381 ,377 ,374 ,370 ,367 ,363 ,360 ,357 ,354 ,350 ,347 ,344 ,341 ,338 ,335 ,
  332 ,329 ,326 ,323 ,320 ,318 ,315 ,312 ,310 ,307 ,304 ,302 ,299 ,297 ,294 ,292 ,
  289 ,287 ,285 ,282 ,280 ,278 ,275 ,273 ,271 ,269 ,267 ,265 ,263 ,261 ,259 );

 g_stack_blur8_shr : array[0..254 ] of int8u = (
  9  ,11 ,12 ,13 ,13 ,14 ,14 ,15 ,15 ,15 ,15 ,16 ,16 ,16 ,16 ,17 ,
  17 ,17 ,17 ,17 ,17 ,17 ,18 ,18 ,18 ,18 ,18 ,18 ,18 ,18 ,18 ,19 ,
  19 ,19 ,19 ,19 ,19 ,19 ,19 ,19 ,19 ,19 ,19 ,19 ,19 ,20 ,20 ,20 ,
  20 ,20 ,20 ,20 ,20 ,20 ,20 ,20 ,20 ,20 ,20 ,20 ,20 ,20 ,20 ,21 ,
  21 ,21 ,21 ,21 ,21 ,21 ,21 ,21 ,21 ,21 ,21 ,21 ,21 ,21 ,21 ,21 ,
  21 ,21 ,21 ,21 ,21 ,21 ,21 ,21 ,21 ,21 ,22 ,22 ,22 ,22 ,22 ,22 ,
  22 ,22 ,22 ,22 ,22 ,22 ,22 ,22 ,22 ,22 ,22 ,22 ,22 ,22 ,22 ,22 ,
  22 ,22 ,22 ,22 ,22 ,22 ,22 ,22 ,22 ,22 ,22 ,22 ,22 ,22 ,22 ,23 ,
  23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,
  23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,
  23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,23 ,
  23 ,23 ,23 ,23 ,23 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,
  24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,
  24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,
  24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,
  24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 ,24 );

type
 stack_calculator_ptr = ^stack_calculator;
 stack_calculator = object
   v ,
   r ,
   g ,
   b ,
   a : unsigned;

   procedure clear;

   procedure add  (c : aggclr );
   procedure add_ (c : aggclr; k : unsigned );
   procedure add__(c : stack_calculator );
   procedure sub  (c : aggclr );
   procedure sub_ (c : stack_calculator );

   procedure calc_pix (c : aggclr_ptr; div_ : unsigned );
   procedure calc_pix_(c : aggclr_ptr; mul_ ,shr_ : unsigned );

  end;

 gauss_calculator_ptr = ^gauss_calculator;
 gauss_calculator = object
   v ,
   r ,
   g ,
   b ,
   a : double;

   procedure from_pix(c : aggclr );

   procedure calc(
              b1 ,b2 ,b3 ,b4 : double;
              c1 ,c2 ,c3 ,c4 : gauss_calculator_ptr );

   procedure to_pix(c : aggclr_ptr );

  end;

{ UNIT IMPLEMENTATION }
{ CLEAR }
procedure stack_calculator.clear;
begin
 v:=0;
 r:=0;
 g:=0;
 b:=0;
 a:=0;

end;

{ ADD }
procedure stack_calculator.add(c : aggclr );
begin
 inc(v ,c.v );
 inc(r ,c.r );
 inc(g ,c.g );
 inc(b ,c.b );
 inc(a ,c.a );

end;

{ ADD__ }
procedure stack_calculator.add__(c : stack_calculator );
begin
 inc(v ,c.v );
 inc(r ,c.r );
 inc(g ,c.g );
 inc(b ,c.b );
 inc(a ,c.a );

end;

{ ADD_ }
procedure stack_calculator.add_(c : aggclr; k : unsigned );
begin
 inc(v ,c.v * k );
 inc(r ,c.r * k );
 inc(g ,c.g * k );
 inc(b ,c.b * k );
 inc(a ,c.a * k );

end;

{ SUB }
procedure stack_calculator.sub(c : aggclr );
begin
 dec(v ,c.v );
 dec(r ,c.r );
 dec(g ,c.g );
 dec(b ,c.b );
 dec(a ,c.a );

end;

{ SUB_ }
procedure stack_calculator.sub_(c : stack_calculator );
begin
 dec(v ,c.v );
 dec(r ,c.r );
 dec(g ,c.g );
 dec(b ,c.b );
 dec(a ,c.a );
 
end;

{ CALC_PIX }
procedure stack_calculator.calc_pix(c : aggclr_ptr; div_ : unsigned );
begin
 c.v:=int8u(v div div_ );
 c.r:=int8u(r div div_ );
 c.g:=int8u(g div div_ );
 c.b:=int8u(b div div_ );
 c.a:=int8u(a div div_ );

end;

{ CALC_PIX_ }
procedure stack_calculator.calc_pix_(c : aggclr_ptr; mul_ ,shr_ : unsigned );
begin
 c.v:=int8u((v * mul_ ) shr shr_ );
 c.r:=int8u((r * mul_ ) shr shr_ );
 c.g:=int8u((g * mul_ ) shr shr_ );
 c.b:=int8u((b * mul_ ) shr shr_ );
 c.a:=int8u((a * mul_ ) shr shr_ );

end;

{ FROM_PIX }
procedure gauss_calculator.from_pix(c : aggclr );
begin
 v:=c.v;
 r:=c.r;
 g:=c.g;
 b:=c.b;
 a:=c.a;

end;

{ CALC }
procedure gauss_calculator.calc(
           b1 ,b2 ,b3 ,b4 : double;
           c1 ,c2 ,c3 ,c4 : gauss_calculator_ptr );
begin
 v:=b1 * c1.v + b2 * c2.v + b3 * c3.v + b4 * c4.v;
 r:=b1 * c1.r + b2 * c2.r + b3 * c3.r + b4 * c4.r;
 g:=b1 * c1.g + b2 * c2.g + b3 * c3.g + b4 * c4.g;
 b:=b1 * c1.b + b2 * c2.b + b3 * c3.b + b4 * c4.b;
 a:=b1 * c1.a + b2 * c2.a + b3 * c3.a + b4 * c4.a;

end;

{ TO_PIX }
procedure gauss_calculator.to_pix(c : aggclr_ptr );
begin
 c.v:=int8u(uround(v ) );
 c.r:=int8u(uround(r ) );
 c.g:=int8u(uround(g ) );
 c.b:=int8u(uround(b ) );
 c.a:=int8u(uround(a ) );

end;

{ CONSTRUCT }
constructor stack_blur.Construct;
begin
 m_buf.Construct  (sizeof(aggclr ) );
 m_stack.Construct(sizeof(aggclr ) );

end;

{ DESTRUCT }
destructor stack_blur.Destruct;
begin
 m_buf.Destruct;
 m_stack.Destruct;

end;

{ BLUR_X }
procedure stack_blur.blur_x(img : pixel_formats_ptr; radius : unsigned );
var
 x ,y ,xp ,i ,stack_ptr ,stack_start ,
 
 w ,h ,wm ,div_ ,div_sum ,mul_sum ,shr_sum ,max_val : unsigned;

 pix : aggclr;

 stack_pix ,_c : aggclr_ptr;

 sum ,sum_in ,sum_out : stack_calculator;

begin
 if radius < 1 then
  exit;

 w   :=img._width;
 h   :=img._height;
 wm  :=w - 1;
 div_:=radius * 2 + 1;

 div_sum:=(radius + 1 ) * (radius + 1 );
 mul_sum:=0;
 shr_sum:=0;
 max_val:=base_mask;

 if (max_val <= 255 ) and
    (radius < 255 ) then
  begin
   mul_sum:=g_stack_blur8_mul[radius ];
   shr_sum:=g_stack_blur8_shr[radius ];

  end;

 m_buf.allocate  (w    ,128 );
 m_stack.allocate(div_ ,32 );

 y:=0;

 while y < h do
  begin
   sum.clear;
   sum_in.clear;
   sum_out.clear;

   pix:=img.pixel(img ,0 ,y );

   i:=0;

   while i <= radius do
    begin
     move(pix ,m_stack.array_operator(i )^ ,sizeof(aggclr ) );

     sum.add_   (pix ,i + 1 );
     sum_out.add(pix );

     inc(i );

    end;

   i:=1;

   while i <= radius do
    begin
     if i > wm then
      pix:=img.pixel(img ,wm ,y )
     else
      pix:=img.pixel(img ,i ,y );

     move(pix ,m_stack.array_operator(i + radius )^ ,sizeof(aggclr ) );

     sum.add_  (pix ,radius + 1 - i );
     sum_in.add(pix );

     inc(i );

    end;

   stack_ptr:=radius;

   x:=0;

   while x < w do
    begin
     if mul_sum <> 0 then
      sum.calc_pix_(aggclr_ptr(m_buf.array_operator(x ) ) ,mul_sum ,shr_sum )
     else
      sum.calc_pix(aggclr_ptr(m_buf.array_operator(x ) ) ,div_sum );

     sum.sub_(sum_out );

     stack_start:=stack_ptr + div_ - radius;

     if stack_start >= div_ then
      dec(stack_start ,div_ );

     stack_pix:=m_stack.array_operator(stack_start );

     sum_out.sub(stack_pix^ );

     xp:=x + radius + 1;

     if xp > wm then
      xp:=wm;

     pix:=img.pixel(img ,xp ,y );

     stack_pix^:=pix;

     sum_in.add(pix );
     sum.add__ (sum_in );

     inc(stack_ptr );

     if stack_ptr >= div_ then
      stack_ptr:=0;

     stack_pix:=m_stack.array_operator(stack_ptr );

     sum_out.add(stack_pix^ );
     sum_in.sub (stack_pix^ );

     inc(x );

    end;

    _c:=m_buf.array_operator(0 );

   img.copy_color_hspan(img ,0 ,y ,w ,_c );

   inc(y );

  end;

end;

{ BLUR_Y }
procedure stack_blur.blur_y(img : pixel_formats_ptr; radius : unsigned );
var
 img2 : pixel_formats_transposer;

begin
 pixfmt_transposer(img2 ,img );
 blur_x           (@img2 ,radius );

end;

{ BLUR }
procedure stack_blur.blur(img : pixel_formats_ptr; radius : unsigned );
var
 img2 : pixel_formats_transposer;

begin
 blur_x           (img ,radius );
 pixfmt_transposer(img2 ,img );
 blur_x           (@img2 ,radius );

end;

{ CONSTRUCT }
constructor recursive_blur.Construct;
begin
 m_sum1.Construct(sizeof(gauss_calculator ) );
 m_sum2.Construct(sizeof(gauss_calculator ) );
 m_buf.Construct (sizeof(aggclr ) );

end;

{ DESTRUCT }
destructor recursive_blur.Destruct;
begin
 m_sum1.Destruct;
 m_sum2.Destruct;
 m_buf.Destruct;

end;

{ BLUR_X }
procedure recursive_blur.blur_x(img : pixel_formats_ptr; radius : double );
var
 s ,q ,q2 ,q3 ,b0 ,b1 ,b2 ,b3 ,b : double;

 w ,h ,wm ,x ,y : int;

 c : gauss_calculator;

 g0 ,g1 : gauss_calculator_ptr;

begin
 if radius < 0.62 then
  exit;

 if img._width < 3 then
  exit;

 s:=radius * 0.5;

 if s < 2.5 then
  q:=3.97156 - 4.14554 * Sqrt(1 - 0.26891 * s )
 else
  q:=0.98711 * s - 0.96330;

 q2:=q * q;
 q3:=q2 * q;
 b0:=1.0 / (1.578250 + 2.444130 * q + 1.428100 * q2 + 0.422205 * q3 );
 b1:=2.44413 * q + 2.85619 * q2 + 1.26661 * q3;
 b2:=-1.42810 * q2 + -1.26661 * q3;
 b3:=0.422205 * q3;
 b :=1 - (b1 + b2 + b3 ) * b0;
 b1:=b1 * b0;
 b2:=b2 * b0;
 b3:=b3 * b0;
 w :=img._width;
 h :=img._height;
 wm:=w - 1;

 m_sum1.allocate(w );
 m_sum2.allocate(w );
 m_buf.allocate (w );

 y:=0;

 while y < h do
  begin
   g0:=gauss_calculator_ptr(m_sum1.array_operator(0 ) );

   c.from_pix(img.pixel(img ,0 ,y ) );
   g0.calc(
    b ,b1 ,b2 ,b3 ,
    @c ,@c ,@c ,@c );

   g1:=gauss_calculator_ptr(m_sum1.array_operator(1 ) );

   c.from_pix(img.pixel(img ,1 ,y ) );
   g1.calc(
    b ,b1 ,b2 ,b3 ,
    @c ,g0 ,g0 ,g0 );

   c.from_pix          (img.pixel(img ,2 ,y ) );
   gauss_calculator_ptr(m_sum1.array_operator(2 ) ).calc(
    b ,b1 ,b2 ,b3 ,
    @c ,g1 ,g0 ,g0 );

   x:=3;

   while x < w do
    begin
     c.from_pix(img.pixel(img ,x ,y ) );

     gauss_calculator_ptr(m_sum1.array_operator(x ) ).calc(
      b ,b1 ,b2 ,b3 ,
      @c ,
      gauss_calculator_ptr(m_sum1.array_operator(x - 1 ) ) ,
      gauss_calculator_ptr(m_sum1.array_operator(x - 2 ) ) ,
      gauss_calculator_ptr(m_sum1.array_operator(x - 3 ) ) );

     inc(x );

    end;

   g0:=gauss_calculator_ptr(m_sum1.array_operator(wm ) );
   g1:=gauss_calculator_ptr(m_sum2.array_operator(wm ) );

   g1.calc(
    b ,b1 ,b2 ,b3 ,
    g0 ,g0 ,g0 ,g0 );

   gauss_calculator_ptr(m_sum2.array_operator(wm - 1 ) ).calc(
    b ,b1 ,b2 ,b3 ,
    gauss_calculator_ptr(m_sum1.array_operator(wm - 1 ) ) ,
    g1 ,g1 ,g1 );

   gauss_calculator_ptr(m_sum2.array_operator(wm - 2 ) ).calc(
    b ,b1 ,b2 ,b3 ,
    gauss_calculator_ptr(m_sum1.array_operator(wm - 2 ) ) ,
    gauss_calculator_ptr(m_sum2.array_operator(wm - 1 ) ) ,
    g1 ,g1 );

   g1.to_pix(
    aggclr_ptr(m_buf.array_operator(wm ) ) );

   gauss_calculator_ptr(m_sum2.array_operator(wm - 1 ) ).to_pix(
    aggclr_ptr(m_buf.array_operator(wm - 1 ) ) );

   gauss_calculator_ptr(m_sum2.array_operator(wm - 2 ) ).to_pix(
    aggclr_ptr(m_buf.array_operator(wm - 2 ) ) );

   x:=wm - 3;

   while x >= 0 do
    begin
     gauss_calculator_ptr(m_sum2.array_operator(x ) ).calc(
      b ,b1 ,b2 ,b3 ,
      gauss_calculator_ptr(m_sum1.array_operator(x ) ) ,
      gauss_calculator_ptr(m_sum2.array_operator(x + 1 ) ) ,
      gauss_calculator_ptr(m_sum2.array_operator(x + 2 ) ) ,
      gauss_calculator_ptr(m_sum2.array_operator(x + 3 ) ) );

     gauss_calculator_ptr(m_sum2.array_operator(x ) ).to_pix(
      aggclr_ptr(m_buf.array_operator(x ) ) );

     dec(x );

    end;

   img.copy_color_hspan(img ,0 ,y ,w ,m_buf.array_operator(0 ) );

   inc(y );

  end;

end;

{ BLUR_Y }
procedure recursive_blur.blur_y(img : pixel_formats_ptr; radius : double );
var
 img2 : pixel_formats_transposer;

begin
 pixfmt_transposer(img2 ,img );
 blur_x           (@img2 ,radius );

end;

{ BLUR }
procedure recursive_blur.blur(img : pixel_formats_ptr; radius : double );
var
 img2 : pixel_formats_transposer;

begin
 blur_x           (img ,radius );
 pixfmt_transposer(img2 ,img );
 blur_x           (@img2 ,radius );

end;

{ STACK_BLUR_GRAY8 }
procedure stack_blur_gray8(img : pixel_formats_ptr; rx ,ry : unsigned );
var
 stride : int;

 x ,y ,xp ,yp ,i ,pix ,stack_pix ,sum ,sum_in ,sum_out ,

 stack_ptr ,stack_start ,w ,h ,wm ,hm ,div_ ,mul_sum ,shr_sum : unsigned;

 src_pix_ptr ,dst_pix_ptr : int8u_ptr;

 stack : pod_vector;

begin
 w :=img._width;
 h :=img._height;
 wm:=w - 1;
 hm:=h - 1;

 stack.Construct(sizeof(int8u ) );

 if rx > 0 then
  begin
   if rx > 254 then
    rx:=254;

   div_:=rx * 2 + 1;

   mul_sum:=g_stack_blur8_mul[rx ];
   shr_sum:=g_stack_blur8_shr[rx ];

   stack.allocate(div_ );

   y:=0;

   while y < h do
    begin
     sum    :=0;
     sum_in :=0;
     sum_out:=0;

     src_pix_ptr:=img.pix_ptr(0 ,y );
     pix        :=src_pix_ptr^;

     i:=0;

     while i <= rx do
      begin
       int8u_ptr(stack.array_operator(i ) )^:=pix;

       inc(sum ,pix * (i + 1 ) );
       inc(sum_out ,pix );

       inc(i );

      end;

     i:=1;

     while i <= rx do
      begin
       if i <= wm then
        inc(ptrcomp(src_pix_ptr ) ,img.m_step );

       pix:=src_pix_ptr^;

       int8u_ptr(stack.array_operator(i + rx ) )^:=pix;

       inc(sum    ,pix * (rx + 1 - i ) );
       inc(sum_in ,pix );

       inc(i );

      end;

     stack_ptr:=rx;
     xp       :=rx;

     if xp > wm then
      xp:=wm;

     src_pix_ptr:=img.pix_ptr(xp ,y );
     dst_pix_ptr:=img.pix_ptr(0 ,y );

     x:=0;

     while x < w do
      begin
       dst_pix_ptr^:=int8u((sum * mul_sum ) shr shr_sum );

       inc(ptrcomp(dst_pix_ptr ) ,img.m_step );
       dec(sum ,sum_out );

       stack_start:=stack_ptr + div_ - rx;

       if stack_start >= div_ then
        dec(stack_start ,div_ );

       dec(sum_out ,int8u_ptr(stack.array_operator(stack_start ) )^ );

       if xp < wm then
        begin
         inc(ptrcomp(src_pix_ptr ) ,img.m_step );

         pix:=src_pix_ptr^;

         inc(xp );

        end;

       int8u_ptr(stack.array_operator(stack_start ) )^:=pix;

       inc(sum_in ,pix );
       inc(sum ,sum_in );

       inc(stack_ptr );

       if stack_ptr >= div_ then
        stack_ptr:=0;

       stack_pix:=int8u_ptr(stack.array_operator(stack_ptr ) )^;

       inc(sum_out ,stack_pix );
       dec(sum_in  ,stack_pix );

       inc(x );

      end;

     inc(y );

    end;

  end;

 if ry > 0 then
  begin
   if ry > 254 then
    ry:=254;

   div_:=ry * 2 + 1;

   mul_sum:=g_stack_blur8_mul[ry ];
   shr_sum:=g_stack_blur8_shr[ry ];

   stack.allocate(div_ );

   stride:=img._stride;

   x:=0;

   while x < w do
    begin
     sum    :=0;
     sum_in :=0;
     sum_out:=0;

     src_pix_ptr:=img.pix_ptr(x ,0 );
     pix        :=src_pix_ptr^;

     i:=0;

     while i <= ry do
      begin
       int8u_ptr(stack.array_operator(i ) )^:=pix;

       inc(sum ,pix * (i + 1 ) );
       inc(sum_out ,pix );

       inc(i );

      end;

     i:=1;

     while i <= ry do
      begin
       if i <= hm then
        inc(ptrcomp(src_pix_ptr ) ,stride );

       pix:=src_pix_ptr^;

       int8u_ptr(stack.array_operator(i + ry ) )^:=pix;

       inc(sum ,pix * (ry + 1 - i ) );
       inc(sum_in ,pix );

       inc(i );

      end;

     stack_ptr:=ry;
     yp       :=ry;

     if yp > hm then
      yp:=hm;

     src_pix_ptr:=img.pix_ptr(x ,yp );
     dst_pix_ptr:=img.pix_ptr(x ,0 );

     y:=0;

     while y < h do
      begin
       dst_pix_ptr^:=int8u((sum * mul_sum ) shr shr_sum );

       inc(ptrcomp(dst_pix_ptr ) ,stride );
       dec(sum ,sum_out );

       stack_start:=stack_ptr + div_ - ry;

       if stack_start >= div_ then
        dec(stack_start ,div_ );

       dec(sum_out ,int8u_ptr(stack.array_operator(stack_start ) )^ );

       if yp < hm then
        begin
         inc(ptrcomp(src_pix_ptr ) ,stride );

         pix:=src_pix_ptr^;

         inc(yp );

        end;

       int8u_ptr(stack.array_operator(stack_start ) )^:=pix;

       inc(sum_in ,pix );
       inc(sum ,sum_in );

       inc(stack_ptr );

       if stack_ptr >= div_ then
        stack_ptr:=0;

       stack_pix:=int8u_ptr(stack.array_operator(stack_ptr ) )^;

       inc(sum_out ,stack_pix );
       dec(sum_in ,stack_pix );

       inc(y );

      end;

     inc(x );

    end;

  end;

 stack.Destruct;

end;

{ STACK_BLUR_RGB24 }
procedure stack_blur_rgb24(img : pixel_formats_ptr; rx ,ry : unsigned );
var
 R ,G ,B ,stride : int;

 x ,y ,xp ,yp ,i ,stack_ptr ,stack_start ,

 sum_r ,sum_g ,sum_b ,

 sum_in_r ,sum_in_g ,sum_in_b ,

 sum_out_r ,sum_out_g ,sum_out_b ,

 w ,h ,wm ,hm ,div_ ,mul_sum ,shr_sum : unsigned;

 src_pix_ptr ,dst_pix_ptr : int8u_ptr;

 stack_pix_ptr : aggclr_ptr;

 stack : pod_array;

begin
 R:=img.m_order.R;
 G:=img.m_order.G;
 B:=img.m_order.B;

 w :=img._width;
 h :=img._height;
 wm:=w - 1;
 hm:=h - 1;

 stack.Construct(sizeof(aggclr ) );

 if rx > 0 then
  begin
   if rx > 254 then
    rx:=254;

   div_   :=rx * 2 + 1;
   mul_sum:=g_stack_blur8_mul[rx ];
   shr_sum:=g_stack_blur8_shr[rx ];

   stack.allocate(div_ );

   y:=0;

   while y < h do
    begin
     sum_r    :=0;
     sum_g    :=0;
     sum_b    :=0;
     sum_in_r :=0;
     sum_in_g :=0;
     sum_in_b :=0;
     sum_out_r:=0;
     sum_out_g:=0;
     sum_out_b:=0;

     src_pix_ptr:=img.pix_ptr(0 ,y );

     i:=0;

     while i <= rx do
      begin
       stack_pix_ptr:=stack.array_operator(i );

       stack_pix_ptr.r:=int8u_ptr(ptrcomp(src_pix_ptr ) + R )^;
       stack_pix_ptr.g:=int8u_ptr(ptrcomp(src_pix_ptr ) + G )^;
       stack_pix_ptr.b:=int8u_ptr(ptrcomp(src_pix_ptr ) + B )^;

       inc(sum_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ * (i + 1 ) );
       inc(sum_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ * (i + 1 ) );
       inc(sum_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ * (i + 1 ) );

       inc(sum_out_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ );
       inc(sum_out_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ );
       inc(sum_out_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ );

       inc(i );

      end;

     i:=1;

     while i <= rx do
      begin
       if i <= wm then
        inc(ptrcomp(src_pix_ptr ) ,img.m_pix_width ); 

       stack_pix_ptr:=stack.array_operator(i + rx );

       stack_pix_ptr.r:=int8u_ptr(ptrcomp(src_pix_ptr ) + R )^;
       stack_pix_ptr.g:=int8u_ptr(ptrcomp(src_pix_ptr ) + G )^;
       stack_pix_ptr.b:=int8u_ptr(ptrcomp(src_pix_ptr ) + B )^;

       inc(sum_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ * (rx + 1 - i ) );
       inc(sum_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ * (rx + 1 - i ) );
       inc(sum_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ * (rx + 1 - i ) );

       inc(sum_in_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ );
       inc(sum_in_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ );
       inc(sum_in_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ );

       inc(i );

      end;

     stack_ptr:=rx;
     xp       :=rx;

     if xp > wm then
      xp:=wm;

     src_pix_ptr:=img.pix_ptr(xp ,y );
     dst_pix_ptr:=img.pix_ptr(0  ,y );

     x:=0;

     while x < w do
      begin
       int8u_ptr(ptrcomp(dst_pix_ptr ) + R )^:=int8u((sum_r * mul_sum ) shr shr_sum );
       int8u_ptr(ptrcomp(dst_pix_ptr ) + G )^:=int8u((sum_g * mul_sum ) shr shr_sum );
       int8u_ptr(ptrcomp(dst_pix_ptr ) + B )^:=int8u((sum_b * mul_sum ) shr shr_sum );

       inc(ptrcomp(dst_pix_ptr ) ,img.m_pix_width );

       dec(sum_r ,sum_out_r );
       dec(sum_g ,sum_out_g );
       dec(sum_b ,sum_out_b );

       stack_start:=stack_ptr + div_ - rx;

       if stack_start >= div_ then
        dec(stack_start ,div_ );

       stack_pix_ptr:=stack.array_operator(stack_start );

       dec(sum_out_r ,stack_pix_ptr.r );
       dec(sum_out_g ,stack_pix_ptr.g );
       dec(sum_out_b ,stack_pix_ptr.b );

       if xp < wm then
        begin
         inc(ptrcomp(src_pix_ptr ) ,img.m_pix_width );
         inc(xp );

        end;

       stack_pix_ptr.r:=int8u_ptr(ptrcomp(src_pix_ptr ) + R )^;
       stack_pix_ptr.g:=int8u_ptr(ptrcomp(src_pix_ptr ) + G )^;
       stack_pix_ptr.b:=int8u_ptr(ptrcomp(src_pix_ptr ) + B )^;

       inc(sum_in_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ );
       inc(sum_in_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ );
       inc(sum_in_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ );

       inc(sum_r ,sum_in_r );
       inc(sum_g ,sum_in_g );
       inc(sum_b ,sum_in_b );

       inc(stack_ptr );

       if stack_ptr >= div_ then
        stack_ptr:=0;

       stack_pix_ptr:=stack.array_operator(stack_ptr );

       inc(sum_out_r ,stack_pix_ptr.r );
       inc(sum_out_g ,stack_pix_ptr.g );
       inc(sum_out_b ,stack_pix_ptr.b );
       dec(sum_in_r ,stack_pix_ptr.r );
       dec(sum_in_g ,stack_pix_ptr.g );
       dec(sum_in_b ,stack_pix_ptr.b );

       inc(x );

      end;

     inc(y );

    end;

  end;

 if ry > 0 then
  begin
   if ry > 254 then
    ry:=254;

   div_:=ry * 2 + 1;

   mul_sum:=g_stack_blur8_mul[ry ];
   shr_sum:=g_stack_blur8_shr[ry ];

   stack.allocate(div_ );

   stride:=img._stride;

   x:=0;

   while x < w do
    begin
     sum_r    :=0;
     sum_g    :=0;
     sum_b    :=0;
     sum_in_r :=0;
     sum_in_g :=0;
     sum_in_b :=0;
     sum_out_r:=0;
     sum_out_g:=0;
     sum_out_b:=0;

     src_pix_ptr:=img.pix_ptr(x ,0 );

     i:=0;

     while i <= ry do
      begin
       stack_pix_ptr:=stack.array_operator(i );

       stack_pix_ptr.r:=int8u_ptr(ptrcomp(src_pix_ptr ) + R )^;
       stack_pix_ptr.g:=int8u_ptr(ptrcomp(src_pix_ptr ) + G )^;
       stack_pix_ptr.b:=int8u_ptr(ptrcomp(src_pix_ptr ) + B )^;

       inc(sum_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ * (i + 1 ) );
       inc(sum_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ * (i + 1 ) );
       inc(sum_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ * (i + 1 ) );
       inc(sum_out_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ );
       inc(sum_out_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ );
       inc(sum_out_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ );

       inc(i );

      end;

     i:=1;

     while i <= ry do
      begin
       if i <= hm then
        inc(ptrcomp(src_pix_ptr ) ,stride );

       stack_pix_ptr:=stack.array_operator(i + ry );

       stack_pix_ptr.r:=int8u_ptr(ptrcomp(src_pix_ptr ) + R )^;
       stack_pix_ptr.g:=int8u_ptr(ptrcomp(src_pix_ptr ) + G )^;
       stack_pix_ptr.b:=int8u_ptr(ptrcomp(src_pix_ptr ) + B )^;

       inc(sum_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ * (ry + 1 - i ) );
       inc(sum_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ * (ry + 1 - i ) );
       inc(sum_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ * (ry + 1 - i ) );
       inc(sum_in_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ );
       inc(sum_in_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ );
       inc(sum_in_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ );

       inc(i );

      end;

     stack_ptr:=ry;
     yp       :=ry;

     if yp > hm then
      yp:=hm;

     src_pix_ptr:=img.pix_ptr(x ,yp );
     dst_pix_ptr:=img.pix_ptr(x ,0 );

     y:=0;

     while y < h do
      begin
       int8u_ptr(ptrcomp(dst_pix_ptr ) + R )^:=int8u((sum_r * mul_sum ) shr shr_sum );
       int8u_ptr(ptrcomp(dst_pix_ptr ) + G )^:=int8u((sum_g * mul_sum ) shr shr_sum );
       int8u_ptr(ptrcomp(dst_pix_ptr ) + B )^:=int8u((sum_b * mul_sum ) shr shr_sum );

       inc(ptrcomp(dst_pix_ptr ) ,stride );

       dec(sum_r ,sum_out_r );
       dec(sum_g ,sum_out_g );
       dec(sum_b ,sum_out_b );

       stack_start:=stack_ptr + div_ - ry;

       if stack_start >= div_ then
        dec(stack_start ,div_ );

       stack_pix_ptr:=stack.array_operator(stack_start );

       dec(sum_out_r ,stack_pix_ptr.r );
       dec(sum_out_g ,stack_pix_ptr.g );
       dec(sum_out_b ,stack_pix_ptr.b );

       if yp < hm then
        begin
         inc(ptrcomp(src_pix_ptr ) ,stride );

         inc(yp );

        end;

       stack_pix_ptr.r:=int8u_ptr(ptrcomp(src_pix_ptr ) + R )^;
       stack_pix_ptr.g:=int8u_ptr(ptrcomp(src_pix_ptr ) + G )^;
       stack_pix_ptr.b:=int8u_ptr(ptrcomp(src_pix_ptr ) + B )^;

       inc(sum_in_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ );
       inc(sum_in_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ );
       inc(sum_in_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ );
       inc(sum_r ,sum_in_r );
       inc(sum_g ,sum_in_g );
       inc(sum_b ,sum_in_b );

       inc(stack_ptr );

       if stack_ptr >= div_ then
        stack_ptr:=0;

       stack_pix_ptr:=stack.array_operator(stack_ptr );

       inc(sum_out_r ,stack_pix_ptr.r );
       inc(sum_out_g ,stack_pix_ptr.g );
       inc(sum_out_b ,stack_pix_ptr.b );
       dec(sum_in_r ,stack_pix_ptr.r );
       dec(sum_in_g ,stack_pix_ptr.g );
       dec(sum_in_b ,stack_pix_ptr.b );

       inc(y );

      end;

     inc(x );

    end;

  end;

 stack.Destruct;

end;

{ STACK_BLUR_RGBA32 }
procedure stack_blur_rgba32(img : pixel_formats_ptr; rx ,ry : unsigned );
var
 R ,G ,B ,A ,stride : int;

 x ,y ,xp ,yp ,i ,stack_ptr ,stack_start ,

 sum_r ,sum_g ,sum_b ,sum_a ,

 sum_in_r ,sum_in_g ,sum_in_b ,sum_in_a ,

 sum_out_r ,sum_out_g ,sum_out_b ,sum_out_a ,

 w ,h ,wm ,hm ,div_ ,mul_sum ,shr_sum : unsigned;

 src_pix_ptr ,dst_pix_ptr : int8u_ptr;

 stack_pix_ptr : aggclr_ptr;

 stack : pod_array;

begin
 R:=img.m_order.R;
 G:=img.m_order.G;
 B:=img.m_order.B;
 A:=img.m_order.A;

 w :=img._width;
 h :=img._height;
 wm:=w - 1;
 hm:=h - 1;

 stack.Construct(sizeof(aggclr ) );

 if rx > 0 then
  begin
   if rx > 254 then
    rx:=254;

   div_   :=rx * 2 + 1;
   mul_sum:=g_stack_blur8_mul[rx ];
   shr_sum:=g_stack_blur8_shr[rx ];

   stack.allocate(div_ );

   y:=0;

   while y < h do
    begin
     sum_r    :=0;
     sum_g    :=0;
     sum_b    :=0;
     sum_a    :=0;
     sum_in_r :=0;
     sum_in_g :=0;
     sum_in_b :=0;
     sum_in_a :=0;
     sum_out_r:=0;
     sum_out_g:=0;
     sum_out_b:=0;
     sum_out_a:=0;

     src_pix_ptr:=img.pix_ptr(0 ,y );

     i:=0;

     while i <= rx do
      begin
       stack_pix_ptr:=stack.array_operator(i );

       stack_pix_ptr.r:=int8u_ptr(ptrcomp(src_pix_ptr ) + R )^;
       stack_pix_ptr.g:=int8u_ptr(ptrcomp(src_pix_ptr ) + G )^;
       stack_pix_ptr.b:=int8u_ptr(ptrcomp(src_pix_ptr ) + B )^;
       stack_pix_ptr.a:=int8u_ptr(ptrcomp(src_pix_ptr ) + A )^;

       inc(sum_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ * (i + 1 ) );
       inc(sum_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ * (i + 1 ) );
       inc(sum_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ * (i + 1 ) );
       inc(sum_a ,int8u_ptr(ptrcomp(src_pix_ptr ) + A )^ * (i + 1 ) );

       inc(sum_out_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ );
       inc(sum_out_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ );
       inc(sum_out_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ );
       inc(sum_out_a ,int8u_ptr(ptrcomp(src_pix_ptr ) + A )^ );

       inc(i );

      end;

     i:=1;

     while i <= rx do
      begin
       if i <= wm then
        inc(ptrcomp(src_pix_ptr ) ,img.m_pix_width );

       stack_pix_ptr:=stack.array_operator(i + rx );

       stack_pix_ptr.r:=int8u_ptr(ptrcomp(src_pix_ptr ) + R )^;
       stack_pix_ptr.g:=int8u_ptr(ptrcomp(src_pix_ptr ) + G )^;
       stack_pix_ptr.b:=int8u_ptr(ptrcomp(src_pix_ptr ) + B )^;
       stack_pix_ptr.a:=int8u_ptr(ptrcomp(src_pix_ptr ) + A )^;

       inc(sum_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ * (rx + 1 - i ) );
       inc(sum_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ * (rx + 1 - i ) );
       inc(sum_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ * (rx + 1 - i ) );
       inc(sum_a ,int8u_ptr(ptrcomp(src_pix_ptr ) + A )^ * (rx + 1 - i ) );

       inc(sum_in_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ );
       inc(sum_in_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ );
       inc(sum_in_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ );
       inc(sum_in_a ,int8u_ptr(ptrcomp(src_pix_ptr ) + A )^ );

       inc(i );

      end;

     stack_ptr:=rx;
     xp       :=rx;

     if xp > wm then
      xp:=wm;

     src_pix_ptr:=img.pix_ptr(xp ,y );
     dst_pix_ptr:=img.pix_ptr(0  ,y );

     x:=0;

     while x < w do
      begin
       int8u_ptr(ptrcomp(dst_pix_ptr ) + R )^:=int8u((sum_r * mul_sum ) shr shr_sum );
       int8u_ptr(ptrcomp(dst_pix_ptr ) + G )^:=int8u((sum_g * mul_sum ) shr shr_sum );
       int8u_ptr(ptrcomp(dst_pix_ptr ) + B )^:=int8u((sum_b * mul_sum ) shr shr_sum );
       int8u_ptr(ptrcomp(dst_pix_ptr ) + A )^:=int8u((sum_a * mul_sum ) shr shr_sum );

       inc(ptrcomp(dst_pix_ptr ) ,img.m_pix_width );

       dec(sum_r ,sum_out_r );
       dec(sum_g ,sum_out_g );
       dec(sum_b ,sum_out_b );
       dec(sum_a ,sum_out_a );

       stack_start:=stack_ptr + div_ - rx;

       if stack_start >= div_ then
        dec(stack_start ,div_ );

       stack_pix_ptr:=stack.array_operator(stack_start );

       dec(sum_out_r ,stack_pix_ptr.r );
       dec(sum_out_g ,stack_pix_ptr.g );
       dec(sum_out_b ,stack_pix_ptr.b );
       dec(sum_out_a ,stack_pix_ptr.a );

       if xp < wm then
        begin
         inc(ptrcomp(src_pix_ptr ) ,img.m_pix_width );
         inc(xp );

        end;

       stack_pix_ptr.r:=int8u_ptr(ptrcomp(src_pix_ptr ) + R )^;
       stack_pix_ptr.g:=int8u_ptr(ptrcomp(src_pix_ptr ) + G )^;
       stack_pix_ptr.b:=int8u_ptr(ptrcomp(src_pix_ptr ) + B )^;
       stack_pix_ptr.a:=int8u_ptr(ptrcomp(src_pix_ptr ) + A )^;

       inc(sum_in_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ );
       inc(sum_in_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ );
       inc(sum_in_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ );
       inc(sum_in_a ,int8u_ptr(ptrcomp(src_pix_ptr ) + A )^ );

       inc(sum_r ,sum_in_r );
       inc(sum_g ,sum_in_g );
       inc(sum_b ,sum_in_b );
       inc(sum_a ,sum_in_a );

       inc(stack_ptr );

       if stack_ptr >= div_ then
        stack_ptr:=0;

       stack_pix_ptr:=stack.array_operator(stack_ptr );

       inc(sum_out_r ,stack_pix_ptr.r );
       inc(sum_out_g ,stack_pix_ptr.g );
       inc(sum_out_b ,stack_pix_ptr.b );
       inc(sum_out_a ,stack_pix_ptr.a );
       dec(sum_in_r ,stack_pix_ptr.r );
       dec(sum_in_g ,stack_pix_ptr.g );
       dec(sum_in_b ,stack_pix_ptr.b );
       dec(sum_in_a ,stack_pix_ptr.a );

       inc(x );

      end;

     inc(y );

    end;

  end;

 if ry > 0 then
  begin
   if ry > 254 then
    ry:=254;

   div_:=ry * 2 + 1;

   mul_sum:=g_stack_blur8_mul[ry ];
   shr_sum:=g_stack_blur8_shr[ry ];

   stack.allocate(div_ );

   stride:=img._stride;

   x:=0;

   while x < w do
    begin
     sum_r    :=0;
     sum_g    :=0;
     sum_b    :=0;
     sum_a    :=0;
     sum_in_r :=0;
     sum_in_g :=0;
     sum_in_b :=0;
     sum_in_a :=0;
     sum_out_r:=0;
     sum_out_g:=0;
     sum_out_b:=0;
     sum_out_a:=0;

     src_pix_ptr:=img.pix_ptr(x ,0 );

     i:=0;

     while i <= ry do
      begin
       stack_pix_ptr:=stack.array_operator(i );

       stack_pix_ptr.r:=int8u_ptr(ptrcomp(src_pix_ptr ) + R )^;
       stack_pix_ptr.g:=int8u_ptr(ptrcomp(src_pix_ptr ) + G )^;
       stack_pix_ptr.b:=int8u_ptr(ptrcomp(src_pix_ptr ) + B )^;
       stack_pix_ptr.a:=int8u_ptr(ptrcomp(src_pix_ptr ) + A )^;

       inc(sum_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ * (i + 1 ) );
       inc(sum_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ * (i + 1 ) );
       inc(sum_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ * (i + 1 ) );
       inc(sum_a ,int8u_ptr(ptrcomp(src_pix_ptr ) + A )^ * (i + 1 ) );
       inc(sum_out_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ );
       inc(sum_out_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ );
       inc(sum_out_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ );
       inc(sum_out_a ,int8u_ptr(ptrcomp(src_pix_ptr ) + A )^ );

       inc(i );

      end;

     i:=1;

     while i <= ry do
      begin
       if i <= hm then
        inc(ptrcomp(src_pix_ptr ) ,stride );

       stack_pix_ptr:=stack.array_operator(i + ry );

       stack_pix_ptr.r:=int8u_ptr(ptrcomp(src_pix_ptr ) + R )^;
       stack_pix_ptr.g:=int8u_ptr(ptrcomp(src_pix_ptr ) + G )^;
       stack_pix_ptr.b:=int8u_ptr(ptrcomp(src_pix_ptr ) + B )^;
       stack_pix_ptr.a:=int8u_ptr(ptrcomp(src_pix_ptr ) + A )^;

       inc(sum_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ * (ry + 1 - i ) );
       inc(sum_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ * (ry + 1 - i ) );
       inc(sum_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ * (ry + 1 - i ) );
       inc(sum_a ,int8u_ptr(ptrcomp(src_pix_ptr ) + A )^ * (ry + 1 - i ) );
       inc(sum_in_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ );
       inc(sum_in_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ );
       inc(sum_in_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ );
       inc(sum_in_a ,int8u_ptr(ptrcomp(src_pix_ptr ) + A )^ );

       inc(i );

      end;

     stack_ptr:=ry;
     yp       :=ry;

     if yp > hm then
      yp:=hm;

     src_pix_ptr:=img.pix_ptr(x ,yp );
     dst_pix_ptr:=img.pix_ptr(x ,0 );

     y:=0;

     while y < h do
      begin
       int8u_ptr(ptrcomp(dst_pix_ptr ) + R )^:=int8u((sum_r * mul_sum ) shr shr_sum );
       int8u_ptr(ptrcomp(dst_pix_ptr ) + G )^:=int8u((sum_g * mul_sum ) shr shr_sum );
       int8u_ptr(ptrcomp(dst_pix_ptr ) + B )^:=int8u((sum_b * mul_sum ) shr shr_sum );
       int8u_ptr(ptrcomp(dst_pix_ptr ) + A )^:=int8u((sum_a * mul_sum ) shr shr_sum );

       inc(ptrcomp(dst_pix_ptr ) ,stride );

       dec(sum_r ,sum_out_r );
       dec(sum_g ,sum_out_g );
       dec(sum_b ,sum_out_b );
       dec(sum_a ,sum_out_a );

       stack_start:=stack_ptr + div_ - ry;

       if stack_start >= div_ then
        dec(stack_start ,div_ );

       stack_pix_ptr:=stack.array_operator(stack_start );

       dec(sum_out_r ,stack_pix_ptr.r );
       dec(sum_out_g ,stack_pix_ptr.g );
       dec(sum_out_b ,stack_pix_ptr.b );
       dec(sum_out_a ,stack_pix_ptr.a );

       if yp < hm then
        begin
         inc(ptrcomp(src_pix_ptr ) ,stride );

         inc(yp );

        end;

       stack_pix_ptr.r:=int8u_ptr(ptrcomp(src_pix_ptr ) + R )^;
       stack_pix_ptr.g:=int8u_ptr(ptrcomp(src_pix_ptr ) + G )^;
       stack_pix_ptr.b:=int8u_ptr(ptrcomp(src_pix_ptr ) + B )^;
       stack_pix_ptr.a:=int8u_ptr(ptrcomp(src_pix_ptr ) + A )^;

       inc(sum_in_r ,int8u_ptr(ptrcomp(src_pix_ptr ) + R )^ );
       inc(sum_in_g ,int8u_ptr(ptrcomp(src_pix_ptr ) + G )^ );
       inc(sum_in_b ,int8u_ptr(ptrcomp(src_pix_ptr ) + B )^ );
       inc(sum_in_a ,int8u_ptr(ptrcomp(src_pix_ptr ) + A )^ );
       inc(sum_r ,sum_in_r );
       inc(sum_g ,sum_in_g );
       inc(sum_b ,sum_in_b );
       inc(sum_a ,sum_in_a );

       inc(stack_ptr );

       if stack_ptr >= div_ then
        stack_ptr:=0;

       stack_pix_ptr:=stack.array_operator(stack_ptr );

       inc(sum_out_r ,stack_pix_ptr.r );
       inc(sum_out_g ,stack_pix_ptr.g );
       inc(sum_out_b ,stack_pix_ptr.b );
       inc(sum_out_a ,stack_pix_ptr.a );
       dec(sum_in_r ,stack_pix_ptr.r );
       dec(sum_in_g ,stack_pix_ptr.g );
       dec(sum_in_b ,stack_pix_ptr.b );
       dec(sum_in_a ,stack_pix_ptr.a );

       inc(y );

      end;

     inc(x );

    end;

  end;

 stack.Destruct;

end;

END.

