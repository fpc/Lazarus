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
// 16.11.2005-Milano: Unit port establishment
//
{ agg_color.pas }
unit
 agg_color ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }

uses
 Math ,
 agg_basics ;

{ TYPES DEFINITION }
type
 order_rgb  = record R ,G ,B : byte end;
 order_bgr  = record B ,G ,R : byte end;
 order_rgba = record R ,G ,B ,A : byte end;
 order_bgra = record B ,G ,R ,A : byte end;
 order_argb = record A ,R ,G ,B : byte end;
 order_abgr = record A ,B ,G ,R : byte end;

 order_type = record
   R ,G ,B ,A : int8u;

  end;

const
 base_shift = 8;
 base_size  = 1 shl base_shift;
 base_mask  = base_size - 1;

 rgb_order  : order_type = (R:0; G:1; B:2; A:3 );
 bgr_order  : order_type = (R:2; G:1; B:0; A:3 );
 rgba_order : order_type = (R:0; G:1; B:2; A:3 );
 bgra_order : order_type = (R:2; G:1; B:0; A:3 );
 argb_order : order_type = (R:1; G:2; B:3; A:0 );
 abgr_order : order_type = (R:3; G:2; B:1; A:0 );

type
 rgba8_ptr_ptr = ^rgba8_ptr;
 rgba8_ptr = ^rgba8;
 rgba8 = object
   r ,g ,b ,a : int8u;

   constructor Construct(r_ ,g_ ,b_ : unsigned; a_ : unsigned = base_mask );

   procedure no_color;
   function  gradient(c : rgba8; k : double ) : rgba8;

  end;

 aggclr_ptr = ^aggclr;
 aggclr = object
   v ,r ,g ,b ,a : int8u;

   constructor Construct; overload;
   constructor Construct(rgba : rgba8 ); overload;
   constructor Construct(clr : aggclr_ptr ); overload;
   constructor ConstrInt(v_ : unsigned; a_ : unsigned = base_mask ); overload;
   constructor ConstrInt(r_ ,g_ ,b_ : unsigned; a_ : unsigned = base_mask ); overload;
   constructor ConstrMix(r_ ,g_ ,b_ : unsigned; a_ : double = 1.0 );
   constructor ConstrDbl(r_ ,g_ ,b_ : double; a_ : double = 1.0 );
   constructor ConstrPre(r_ ,g_ ,b_ : double; a_ : double = 1.0 );

   constructor from_wavelength(wl ,gamma : double );

   function  gradient (c : aggclr_ptr; k : double ) : aggclr;
   function  gradient8(c : aggclr_ptr; k : double ) : rgba8;

   procedure add(c : aggclr_ptr; cover : unsigned );

   function  opacity_(a_ : double ) : aggclr_ptr;
   function  _opacity : double;

   procedure clear;
   function  premultiply : aggclr_ptr;
   procedure apply_gamma_dir(gamma_ : gamma_ptr );

  end;

{ GLOBAL PROCEDURES }
 function  rgb8_packed    (v : unsigned ) : rgba8;

IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor rgba8.Construct;
begin
 b{*}:=int8u(r_ );
 g:=int8u(g_ );
 r:=int8u(b_ );
 a:=int8u(a_ );

end;

{ NO_COLOR }
procedure rgba8.no_color;
begin
 r:=0;
 g:=0;
 b:=0;
 a:=0;

end;

{ GRADIENT }
function rgba8.gradient(c : rgba8; k : double ) : rgba8;
var
 ik  : int32u; // calc_type

begin
 ik:=Trunc(k * base_size );

 result.r:=int8u(int32u(r ) + (((int32u(c.r ) - r ) * ik ) shr base_shift ) );
 result.g:=int8u(int32u(g ) + (((int32u(c.g ) - g ) * ik ) shr base_shift ) );
 result.b:=int8u(int32u(b ) + (((int32u(c.b ) - b ) * ik ) shr base_shift ) );
 result.a:=int8u(int32u(a ) + (((int32u(c.a ) - a ) * ik ) shr base_shift ) );

end;

{ CONSTRUCT }
constructor aggclr.Construct;
begin
 v:=0;
 r:=0;
 g:=0;
 b:=0;
 a:=0;

end;

{ CONSTRUCT }
constructor aggclr.Construct(rgba : rgba8 );
begin
 v:=(rgba.r * 77 + rgba.g * 150 + rgba.b * 29 ) shr 8;
 r:=rgba.r;
 g:=rgba.g;
 b:=rgba.b;
 a:=rgba.a;

end;

{ CONSTRUCT }
constructor aggclr.Construct(clr : aggclr_ptr );
begin
 v:=clr.v;
 r:=clr.r;
 g:=clr.g;
 b:=clr.b;
 a:=clr.a;

end;

{ CONSTRINT }
constructor aggclr.ConstrInt(v_ : unsigned; a_ : unsigned = base_mask );
begin
 v:=int8u(v_ );
 r:=0;
 g:=0;
 b:=0;
 a:=int8u(a_ );

end;

{ CONSTRINT }
constructor aggclr.ConstrInt(r_ ,g_ ,b_ : unsigned; a_ : unsigned = base_mask );
begin
 v:=(r_ * 77 + g_ * 150 + b_ * 29 ) shr 8;
 r:=int8u(r_ );
 g:=int8u(g_ );
 b:=int8u(b_ );
 a:=int8u(a_ );

end;

{ CONSTRDBL }
constructor aggclr.ConstrDbl;
begin
 v:=trunc((0.299 * r_ + 0.587 * g_ + 0.114 * b_ ) * base_mask + 0.5 );
 r:=trunc(r_ * base_mask + 0.5 );
 g:=trunc(g_ * base_mask + 0.5 );
 b:=trunc(b_ * base_mask + 0.5 );
 a:=trunc(a_ * base_mask + 0.5 );

end;

{ CONSTRPRE }
constructor aggclr.ConstrPre;
begin
 r_:=r_ * a_;
 g_:=g_ * a_;
 b_:=b_ * a_;

 ConstrDbl(r_ ,g_ ,b_ ,a_ );

end;

{ CONSTRMIX }
constructor aggclr.ConstrMix;
begin
 v:=(r_ * 77 + g_ * 150 + b_ * 29 ) shr 8;
 r:=r_;
 g:=g_;
 b:=b_;
 a:=trunc(a_ * base_mask + 0.5 );

end;

{ FROM_WAVELENGTH }
constructor aggclr.from_wavelength(wl ,gamma : double );
var
 tr ,tg ,tb ,ta ,s : double;

begin
 tr:=0;
 tg:=0;
 tb:=0;
 ta:=0;

 if (wl >= 380.0 ) and
    (wl <= 440.0 ) then
  begin
   tr:=-1.0 * (wl - 440.0 ) / (440.0 - 380.0 );
   tb:=1.0;

  end
 else
  if (wl >= 440.0 ) and
     (wl <= 490.0 ) then
   begin
    tg:=(wl - 440.0 ) / (490.0 - 440.0 );
    tb:=1.0;

   end
  else
   if (wl >= 490.0 ) and
      (wl <= 510.0 ) then
    begin
     tg:=1.0;
     tb:=-1.0 * (wl - 510.0 ) / (510.0 - 490.0 );

    end
   else
    if (wl >= 510.0 ) and
       (wl <= 580.0 ) then
     begin
      tr:=(wl - 510.0 ) / (580.0 - 510.0 );
      tg:=1.0;

     end
    else
     if (wl >= 580.0 ) and
        (wl <= 645.0 ) then
      begin
       tr:=1.0;
       tg:=-1.0 * (wl - 645.0 ) / (645.0 - 580.0 );

      end
     else
      if (wl >= 645.0 ) and
         (wl <= 780.0 ) then
       tr:=1.0;

 s:=1.0;

 if wl > 700.0 then
  s:=0.3 + 0.7 * (780.0 - wl ) / (780.0 - 700.0 )
 else
  if wl < 420.0 then
   s:=0.3 + 0.7 * (wl - 380.0 ) / (420.0 - 380.0 );

 tr:=Power(tr * s ,gamma );
 tg:=Power(tg * s ,gamma );
 tb:=Power(tb * s ,gamma );

 ConstrDbl(tr ,tg ,tb );

end;

{ GRADIENT }
function aggclr.gradient;
var
 ik : unsigned;

begin
 ik:=trunc(k * base_size );

 result.r:=int8u(r + (((c.r - r ) * ik ) shr base_shift ) );
 result.g:=int8u(g + (((c.g - g ) * ik ) shr base_shift ) );
 result.b:=int8u(b + (((c.b - b ) * ik ) shr base_shift ) );
 result.a:=int8u(a + (((c.a - a ) * ik ) shr base_shift ) );

end;

{ GRADIENT8 }
function aggclr.gradient8;
var
 ik : unsigned;

begin
 ik:=trunc(k * base_size );

 result.r:=int8u(r + (((c.r - r ) * ik ) shr base_shift ) );
 result.g:=int8u(g + (((c.g - g ) * ik ) shr base_shift ) );
 result.b:=int8u(b + (((c.b - b ) * ik ) shr base_shift ) );
 result.a:=int8u(a + (((c.a - a ) * ik ) shr base_shift ) );

end;

{ ADD }
procedure aggclr.add(c : aggclr_ptr; cover : unsigned );
var
 cv ,cr ,cg ,cb ,ca : int32u;

begin
 if cover = cover_mask then
  if c.a = base_mask then
   Construct(c )
  else
   begin
    cv:=v + c.v;

    if cv > int32u(base_mask ) then
     v:=int8u(base_mask )
    else
     v:=int8u(cv );

    cr:=r + c.r;

    if cr > int32u(base_mask ) then
     r:=int8u(base_mask )
    else
     r:=int8u(cr );

    cg:=g + c.g;

    if cg > int32u(base_mask ) then
     g:=int8u(base_mask )
    else
     g:=int8u(cg );

    cb:=b + c.b;

    if cb > int32u(base_mask ) then
     b:=int8u(base_mask )
    else
     b:=int8u(cb );

    ca:=a + c.a;

    if ca > int32u(base_mask ) then
     a:=int8u(base_mask )
    else
     a:=int8u(ca );

   end
 else
  begin
   cv:=v + ((c.v * cover + cover_mask div 2 ) shr cover_shift );
   cr:=r + ((c.r * cover + cover_mask div 2 ) shr cover_shift );
   cg:=g + ((c.g * cover + cover_mask div 2 ) shr cover_shift );
   cb:=b + ((c.b * cover + cover_mask div 2 ) shr cover_shift );
   ca:=a + ((c.a * cover + cover_mask div 2 ) shr cover_shift );

   if cv > int32u(base_mask ) then
    v:=int8u(base_mask )
   else
    v:=int8u(cv );

   if cr > int32u(base_mask ) then
    r:=int8u(base_mask )
   else
    r:=int8u(cr );

   if cg > int32u(base_mask ) then
    g:=int8u(base_mask )
   else
    g:=int8u(cg );

   if cb > int32u(base_mask ) then
    b:=int8u(base_mask )
   else
    b:=int8u(cb );

   if ca > int32u(base_mask ) then
    a:=int8u(base_mask )
   else
    a:=int8u(ca );

  end;

end;

{ OPACITY_ }
function aggclr.opacity_;
begin
 if a_ < 0.0 then
  a_:=0.0;

 if a_ > 1.0 then
  a_:=1.0;

 a:=trunc(a_ * base_mask + 0.5 );

 result:=@self;

end;

{ _OPACITY }
function aggclr._opacity;
begin
 result:=a / base_mask;

end;

{ CLEAR }
procedure aggclr.clear;
begin
 v:=0;
 r:=0;
 g:=0;
 b:=0;
 a:=0;

end;

{ PREMULTIPLY }
function aggclr.premultiply : aggclr_ptr;
begin
 if a = base_mask then
  begin
   result:=@self;

   exit;

  end;

 if a = 0 then
  begin
   v:=0;
   r:=0;
   g:=0;
   b:=0;

   result:=@self;

   exit;

  end;

 v:=int8u((v * a ) shr base_shift );
 r:=int8u((r * a ) shr base_shift );
 g:=int8u((g * a ) shr base_shift );
 b:=int8u((b * a ) shr base_shift );

end;

{ APPLY_GAMMA_DIR }
procedure aggclr.apply_gamma_dir;
begin
 v:=int8u(gamma_.dir(v ) );
 r:=int8u(gamma_.dir(r ) );
 g:=int8u(gamma_.dir(g ) );
 b:=int8u(gamma_.dir(b ) );

end;

{ RGB8_PACKED }
function rgb8_packed;
begin
 result.r:=(v shr 16) and $FF;
 result.g:=(v shr 8) and $FF;
 result.b:=v and $FF;
 result.a:=base_mask;

end;

END.

{*}
