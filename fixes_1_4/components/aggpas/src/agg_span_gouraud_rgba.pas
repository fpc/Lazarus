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
// 27.01.2006-Milano: Unit port establishment
//
{ agg_span_gouraud_rgba.pas }
unit
 agg_span_gouraud_rgba ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_color ,
 agg_dda_line ,
 agg_span_gouraud ,
 agg_span_allocator ,
 agg_math ;

{ TYPES DEFINITION }
const
 subpixel_shift = 4;
 subpixel_size  = 1 shl subpixel_shift;

type
 rgba_calc_ptr = ^rgba_calc;
 rgba_calc = object
   m_x1 ,m_y1 ,m_dx ,m_1dy : double;

   m_r1 ,m_g1 ,m_b1 ,m_a1 ,
   m_dr ,m_dg ,m_db ,m_da ,
   m_r  ,m_g  ,m_b  ,m_a  ,m_x : int;

   function  round(v : double ) : int;
   procedure init (c1 ,c2 : coord_type_ptr );
   procedure calc (y : double );

  end;

 span_gouraud_rgba_ptr = ^span_gouraud_rgba; 
 span_gouraud_rgba = object(span_gouraud )
   m_swap : boolean;
   m_y2   : int;

   m_rgba1 ,
   m_rgba2 ,
   m_rgba3 : rgba_calc;

   constructor Construct(alloc : span_allocator_ptr ); overload;
   constructor Construct(
                alloc : span_allocator_ptr;
                c1 ,c2 ,c3 : aggclr_ptr;
                x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,d : double ); overload;

   procedure prepare (max_span_len : unsigned ); virtual;
   function  generate(x ,y : int; len : unsigned ) : aggclr_ptr; virtual;

  // Agg 2.4 impl
   constructor Construct_(
                c1 ,c2 ,c3 : aggclr_ptr;
                x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double;
                d : double = 0 );

   procedure prepare_;
   procedure generate_(span : aggclr_ptr; x ,y : int; len : unsigned );

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ ROUND }
function rgba_calc.round;
begin
 if v < 0.0 then
  result:=trunc(v - 0.5 )
 else
  result:=trunc(v + 0.5 );

end;

{ INIT }
procedure rgba_calc.init;
var
 dy : double;

begin
 m_x1:=c1.x - 0.5;
 m_y1:=c1.y - 0.5;
 m_dx:=c2.x - c1.x;

 dy:=c2.y - c1.y;

 if dy < 1e-5 then
  m_1dy:=1e5
 else
  m_1dy:=1.0 / dy;

 m_r1:=c1.color.r;
 m_g1:=c1.color.g;
 m_b1:=c1.color.b;
 m_a1:=c1.color.a;
 m_dr:=c2.color.r - m_r1;
 m_dg:=c2.color.g - m_g1;
 m_db:=c2.color.b - m_b1;
 m_da:=c2.color.a - m_a1;

end;

{ CALC }
procedure rgba_calc.calc;
var
 k : double;

begin
 k:=(y - m_y1 ) * m_1dy;

 if k < 0.0 then
  k:=0.0;

 if k > 1.0 then
  k:=1.0;

 m_r:=m_r1 + self.round(m_dr * k );
 m_g:=m_g1 + self.round(m_dg * k );
 m_b:=m_b1 + self.round(m_db * k );
 m_a:=m_a1 + self.round(m_da * k );
 m_x:=self.round((m_x1 + m_dx * k ) * subpixel_size );

end;

{ CONSTRUCT }
constructor span_gouraud_rgba.Construct(alloc : span_allocator_ptr );
begin
 inherited Construct(alloc );

end;

{ CONSTRUCT }
constructor span_gouraud_rgba.Construct(
                               alloc : span_allocator_ptr;
                               c1 ,c2 ,c3 : aggclr_ptr;
                               x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,d : double );
begin
 inherited Construct(alloc ,c1 ,c2 ,c3 ,x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,d );

end;

{ PREPARE }
procedure span_gouraud_rgba.prepare;
var
 coord : array[0..2 ] of coord_type;

begin
 inherited prepare(max_span_len );

 arrange_vertices(@coord );

 m_y2:=trunc(coord[1 ].y );

 m_swap:=
  calc_point_location(
   coord[0 ].x ,coord[0 ].y,
   coord[2 ].x ,coord[2 ].y,
   coord[1 ].x ,coord[1 ].y ) < 0.0;

 m_rgba1.init(@coord[0 ] ,@coord[2 ] );
 m_rgba2.init(@coord[0 ] ,@coord[1 ] );
 m_rgba3.init(@coord[1 ] ,@coord[2 ] );

end;

{ GENERATE }
function span_gouraud_rgba.generate;
const
 lim = agg_color.base_mask;

var
 pc1 ,pc2 ,t : rgba_calc_ptr;

 nlen ,start ,vr ,vg ,vb ,va : int;

 r ,g ,b ,a : dda_line_interpolator;

 span : aggclr_ptr;

begin
 m_rgba1.calc(y ); //(m_rgba1.m_1dy > 2) ? m_rgba1.m_y1 : y);

 pc1:=@m_rgba1;
 pc2:=@m_rgba2;

 if y <= m_y2 then
 // Bottom part of the triangle (first subtriangle)
  m_rgba2.calc(y + m_rgba2.m_1dy )
 else
  begin
  // Upper part (second subtriangle)
   m_rgba3.calc(y - m_rgba3.m_1dy );

   pc2:=@m_rgba3;

  end;

 if m_swap then
  begin
  // It means that the triangle is oriented clockwise,
  // so that we need to swap the controlling structures
   t  :=pc2;
   pc2:=pc1;
   pc1:=t;

  end;

// Get the horizontal length with subpixel accuracy
// and protect it from division by zero
 nlen:=Abs(pc2.m_x - pc1.m_x );

 if nlen <= 0 then
  nlen:=1;

 r.Construct(pc1.m_r ,pc2.m_r ,nlen ,14 );
 g.Construct(pc1.m_g ,pc2.m_g ,nlen ,14 );
 b.Construct(pc1.m_b ,pc2.m_b ,nlen ,14 );
 a.Construct(pc1.m_a ,pc2.m_a ,nlen ,14 );

// Calculate the starting point of the gradient with subpixel
// accuracy and correct (roll back) the interpolators.
// This operation will also clip the beginning of the span
// if necessary.
 start:=pc1.m_x - (x shl subpixel_shift );

 r.dec_operator(start );
 g.dec_operator(start );
 b.dec_operator(start );
 a.dec_operator(start );

 inc(nlen ,start );

 span:=_allocator.span;

// Beginning part of the span. Since we rolled back the
// interpolators, the color values may have overflow.
// So that, we render the beginning part with checking
// for overflow. It lasts until "start" is positive;
// typically it's 1-2 pixels, but may be more in some cases.
 while (len <> 0 ) and
       (start > 0 ) do
  begin
   vr:=r._y;
   vg:=g._y;
   vb:=b._y;
   va:=a._y;

   if vr < 0 then
    vr:=0;

   if vr > lim then
    vr:=lim;

   if vg < 0 then
    vg:=0;

   if vg > lim then
    vg:=lim;

   if vb < 0 then
    vb:=0;

   if vb > lim then
    vb:=lim;

   if va < 0 then
    va:=0;

   if va > lim then
    va:=lim;

   span.r:=int8u(vr );
   span.g:=int8u(vg );
   span.b:=int8u(vb );
   span.a:=int8u(va );

   r.inc_operator(subpixel_size );
   g.inc_operator(subpixel_size );
   b.inc_operator(subpixel_size );
   a.inc_operator(subpixel_size );

   dec(nlen ,subpixel_size );
   dec(start ,subpixel_size );
   inc(ptrcomp(span ) ,sizeof(aggclr ) );
   dec(len );

  end;

// Middle part, no checking for overflow.
// Actual spans can be longer than the calculated length
// because of anti-aliasing, thus, the interpolators can
// overflow. But while "nlen" is positive we are safe.
 while (len <> 0 ) and
       (nlen > 0 ) do
  begin
   span.r:=int8u(r._y );
   span.g:=int8u(g._y );
   span.b:=int8u(b._y );
   span.a:=int8u(a._y );

   r.inc_operator(subpixel_size );
   g.inc_operator(subpixel_size );
   b.inc_operator(subpixel_size );
   a.inc_operator(subpixel_size );

   dec(nlen ,subpixel_size );
   inc(ptrcomp(span ) ,sizeof(aggclr ) );
   dec(len );

  end;

// Ending part; checking for overflow.
// Typically it's 1-2 pixels, but may be more in some cases.
 while len <> 0 do
  begin
   vr:=r._y;
   vg:=g._y;
   vb:=b._y;
   va:=a._y;

   if vr < 0 then
    vr:=0;

   if vr > lim then
    vr:=lim;

   if vg < 0 then
    vg:=0;

   if vg > lim then
    vg:=lim;

   if vb < 0 then
    vb:=0;

   if vb > lim then
    vb:=lim;

   if va < 0 then
    va:=0;

   if va > lim then
    va:=lim;

   span.r:=int8u(vr );
   span.g:=int8u(vg );
   span.b:=int8u(vb );
   span.a:=int8u(va );

   r.inc_operator(subpixel_size );
   g.inc_operator(subpixel_size );
   b.inc_operator(subpixel_size );
   a.inc_operator(subpixel_size );

   inc(ptrcomp(span ) ,sizeof(aggclr ) );
   dec(len );

  end;

 result:=_allocator.span;

end;

{ CONSTRUCT_ }
constructor span_gouraud_rgba.Construct_(
             c1 ,c2 ,c3 : aggclr_ptr;
             x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double;
             d : double = 0 );
begin
 inherited Construct(NIL ,c1 ,c2 ,c3 ,x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,d );

end;

{ PREPARE_ }
procedure span_gouraud_rgba.prepare_;
var
 coord : array[0..2 ] of coord_type;

begin
 arrange_vertices(@coord );

 m_y2:=int(Trunc(coord[1 ].y ) );

 m_swap:=
  cross_product(
   coord[0 ].x ,coord[0 ].y,
   coord[2 ].x ,coord[2 ].y,
   coord[1 ].x ,coord[1 ].y ) < 0.0;

 m_rgba1.init(@coord[0 ] ,@coord[2 ] );
 m_rgba2.init(@coord[0 ] ,@coord[1 ] );
 m_rgba3.init(@coord[1 ] ,@coord[2 ] );

end;

{ GENERATE_ }
procedure span_gouraud_rgba.generate_(span : aggclr_ptr; x ,y : int; len : unsigned );
const
 lim = agg_color.base_mask;

var
 pc1 ,pc2 ,t : rgba_calc_ptr;

 nlen ,start ,vr ,vg ,vb ,va : int;

 r ,g ,b ,a : dda_line_interpolator;

begin
 m_rgba1.calc(y ); //(m_rgba1.m_1dy > 2) ? m_rgba1.m_y1 : y);

 pc1:=@m_rgba1;
 pc2:=@m_rgba2;

 if y <= m_y2 then
 // Bottom part of the triangle (first subtriangle)
  m_rgba2.calc(y + m_rgba2.m_1dy )
 else
  begin
  // Upper part (second subtriangle)
   m_rgba3.calc(y - m_rgba3.m_1dy );

   pc2:=@m_rgba3;

  end;

 if m_swap then
  begin
  // It means that the triangle is oriented clockwise,
  // so that we need to swap the controlling structures
   t  :=pc2;
   pc2:=pc1;
   pc1:=t;

  end;

// Get the horizontal length with subpixel accuracy
// and protect it from division by zero
 nlen:=Abs(pc2.m_x - pc1.m_x );

 if nlen <= 0 then
  nlen:=1;

 r.Construct(pc1.m_r ,pc2.m_r ,nlen ,14 );
 g.Construct(pc1.m_g ,pc2.m_g ,nlen ,14 );
 b.Construct(pc1.m_b ,pc2.m_b ,nlen ,14 );
 a.Construct(pc1.m_a ,pc2.m_a ,nlen ,14 );

// Calculate the starting point of the gradient with subpixel
// accuracy and correct (roll back) the interpolators.
// This operation will also clip the beginning of the span
// if necessary.
 start:=pc1.m_x - (x shl subpixel_shift );

 r.dec_operator(start );
 g.dec_operator(start );
 b.dec_operator(start );
 a.dec_operator(start );

 inc(nlen ,start );

// Beginning part of the span. Since we rolled back the
// interpolators, the color values may have overflow.
// So that, we render the beginning part with checking
// for overflow. It lasts until "start" is positive;
// typically it's 1-2 pixels, but may be more in some cases.
 while (len <> 0 ) and
       (start > 0 ) do
  begin
   vr:=r._y;
   vg:=g._y;
   vb:=b._y;
   va:=a._y;

   if vr < 0 then
    vr:=0;

   if vr > lim then
    vr:=lim;

   if vg < 0 then
    vg:=0;

   if vg > lim then
    vg:=lim;

   if vb < 0 then
    vb:=0;

   if vb > lim then
    vb:=lim;

   if va < 0 then
    va:=0;

   if va > lim then
    va:=lim;

   span.r:=int8u(vr );
   span.g:=int8u(vg );
   span.b:=int8u(vb );
   span.a:=int8u(va );

   r.inc_operator(subpixel_size );
   g.inc_operator(subpixel_size );
   b.inc_operator(subpixel_size );
   a.inc_operator(subpixel_size );

   dec(nlen ,subpixel_size );
   dec(start ,subpixel_size );
   inc(ptrcomp(span ) ,sizeof(aggclr ) );
   dec(len );

  end;

// Middle part, no checking for overflow.
// Actual spans can be longer than the calculated length
// because of anti-aliasing, thus, the interpolators can
// overflow. But while "nlen" is positive we are safe.
 while (len <> 0 ) and
       (nlen > 0 ) do
  begin
   span.r:=int8u(r._y );
   span.g:=int8u(g._y );
   span.b:=int8u(b._y );
   span.a:=int8u(a._y );

   r.inc_operator(subpixel_size );
   g.inc_operator(subpixel_size );
   b.inc_operator(subpixel_size );
   a.inc_operator(subpixel_size );

   dec(nlen ,subpixel_size );
   inc(ptrcomp(span ) ,sizeof(aggclr ) );
   dec(len );

  end;

// Ending part; checking for overflow.
// Typically it's 1-2 pixels, but may be more in some cases.
 while len <> 0 do
  begin
   vr:=r._y;
   vg:=g._y;
   vb:=b._y;
   va:=a._y;

   if vr < 0 then
    vr:=0;

   if vr > lim then
    vr:=lim;

   if vg < 0 then
    vg:=0;

   if vg > lim then
    vg:=lim;

   if vb < 0 then
    vb:=0;

   if vb > lim then
    vb:=lim;

   if va < 0 then
    va:=0;

   if va > lim then
    va:=lim;

   span.r:=int8u(vr );
   span.g:=int8u(vg );
   span.b:=int8u(vb );
   span.a:=int8u(va );

   r.inc_operator(subpixel_size );
   g.inc_operator(subpixel_size );
   b.inc_operator(subpixel_size );
   a.inc_operator(subpixel_size );

   inc(ptrcomp(span ) ,sizeof(aggclr ) );
   dec(len );

  end;

end;

END.

