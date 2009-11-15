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
// 22.02.2006-Milano: sbool_ ... _bin
// 23.01.2006-Milano: sbool_intersect_shapes
// 20.01.2006-Milano: Unit port establishment
//
{ agg_scanline_boolean_algebra.pas }
unit
 agg_scanline_boolean_algebra ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
 agg_basics ,
 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_renderer_scanline ;

{ TYPES DEFINITION }
type
 sbool_op_e = (
  sbool_or ,            //----sbool_or
  sbool_and ,           //----sbool_and
  sbool_xor ,           //----sbool_xor
  sbool_xor_saddle ,    //----sbool_xor_saddle
  sbool_xor_abs_diff ,  //----sbool_xor_abs_diff
  sbool_a_minus_b ,     //----sbool_a_minus_b
  sbool_b_minus_a );    //----sbool_b_minus_a

 sbool_functor_ptr = ^sbool_functor;

 sbool_functor1 = procedure(this : sbool_functor_ptr; span : span_ptr; x : int; len : unsigned; sl : scanline_ptr );
 sbool_functor2 = procedure(this : sbool_functor_ptr; span1 ,span2 : span_ptr; x : int; len : unsigned; sl : scanline_ptr );
 sbool_formula  = function (this : sbool_functor_ptr; a ,b : unsigned ) : unsigned;

 sbool_functor = object
   cover_shift ,
   cover_size  ,
   cover_mask  ,
   cover_full  : unsigned;

   functor1 : sbool_functor1;
   functor2 : sbool_functor2;
   formula  : sbool_formula;

   constructor Construct1(f1 : sbool_functor1; CoverShift : unsigned = agg_basics.cover_shift );
   constructor Construct2(f2 : sbool_functor2; CoverShift : unsigned = agg_basics.cover_shift );

  end;

{ GLOBAL PROCEDURES }
 procedure sbool_subtract_shapes_aa(
            sg1 ,
            sg2 : rasterizer_scanline_ptr;
            sl1 ,
            sl2 ,
            sl  : scanline_ptr;
            ren : renderer_scanline_ptr );

 procedure sbool_xor_shapes_abs_diff_aa(
            sg1 ,
            sg2 : rasterizer_scanline_ptr;
            sl1 ,
            sl2 ,
            sl  : scanline_ptr;
            ren : renderer_scanline_ptr );

 procedure sbool_xor_shapes_saddle_aa(
            sg1 ,
            sg2 : rasterizer_scanline_ptr;
            sl1 ,
            sl2 ,
            sl  : scanline_ptr;
            ren : renderer_scanline_ptr );

 procedure sbool_xor_shapes_aa(
            sg1 ,
            sg2 : rasterizer_scanline_ptr;
            sl1 ,
            sl2 ,
            sl  : scanline_ptr;
            ren : renderer_scanline_ptr );

 procedure sbool_intersect_shapes_aa(
            sg1 ,
            sg2 : rasterizer_scanline_ptr;
            sl1 ,
            sl2 ,
            sl  : scanline_ptr;
            ren : renderer_scanline_ptr );

 procedure sbool_unite_shapes_aa(
            sg1 ,
            sg2 : rasterizer_scanline_ptr;
            sl1 ,
            sl2 ,
            sl  : scanline_ptr;
            ren : renderer_scanline_ptr );

 procedure sbool_combine_shapes_aa(
            op  : sbool_op_e;
            sg1 ,
            sg2 : rasterizer_scanline_ptr;
            sl1 ,
            sl2 ,
            sl  : scanline_ptr;
            ren : renderer_scanline_ptr );

 procedure sbool_subtract_shapes_bin(
            sg1 ,
            sg2 : rasterizer_scanline_ptr;
            sl1 ,
            sl2 ,
            sl  : scanline_ptr;
            ren : renderer_scanline_ptr );

 procedure sbool_xor_shapes_bin(
            sg1 ,
            sg2 : rasterizer_scanline_ptr;
            sl1 ,
            sl2 ,
            sl  : scanline_ptr;
            ren : renderer_scanline_ptr );

 procedure sbool_intersect_shapes_bin(
            sg1 ,
            sg2 : rasterizer_scanline_ptr;
            sl1 ,
            sl2 ,
            sl  : scanline_ptr;
            ren : renderer_scanline_ptr );

 procedure sbool_unite_shapes_bin(
            sg1 ,
            sg2 : rasterizer_scanline_ptr;
            sl1 ,
            sl2 ,
            sl  : scanline_ptr;
            ren : renderer_scanline_ptr );

 procedure sbool_combine_shapes_bin(
            op  : sbool_op_e;
            sg1 ,
            sg2 : rasterizer_scanline_ptr;
            sl1 ,
            sl2 ,
            sl  : scanline_ptr;
            ren : renderer_scanline_ptr );

IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT1 }
constructor sbool_functor.Construct1;
begin
 cover_shift:=CoverShift;
 cover_size :=1 shl cover_shift;
 cover_mask :=cover_size - 1;
 cover_full :=cover_mask;

 functor1:=f1;
 functor2:=NIL;
 formula :=NIL;

end;

{ CONSTRUCT2 }
constructor sbool_functor.Construct2;
begin
 cover_shift:=CoverShift;
 cover_size :=1 shl cover_shift;
 cover_mask :=cover_size - 1;
 cover_full :=cover_mask;

 functor1:=NIL;
 functor2:=f2;
 formula :=NIL;

end;

{ sbool_add_span_empty }
// Functor.
// Add nothing. Used in conbine_shapes_sub
procedure sbool_add_span_empty(this : sbool_functor_ptr; span : span_ptr; x : int; len : unsigned; sl : scanline_ptr );
begin
end;

{ sbool_combine_spans_empty }
// Functor.
// Combine two spans as empty ones. The functor does nothing
// and is used to XOR binary spans.
procedure sbool_combine_spans_empty(this : sbool_functor_ptr; span1 ,span2 : span_ptr; x : int; len : unsigned; sl : scanline_ptr );
begin
end;

{ sbool_add_span_aa }
// Functor.
// Add an anti-aliased span
// anti-aliasing information, but only X and Length. The function
// is compatible with any type of scanlines.
procedure sbool_add_span_aa(this : sbool_functor_ptr; span : span_ptr; x : int; len : unsigned; sl : scanline_ptr );
var
 covers : int8u_ptr;

begin
 if span.len < 0 then
  sl.add_span(x ,len ,span.covers^ )
 else
  if span.len > 0 then
   begin
    covers:=span.covers;

    if span.x < x then
     inc(ptrcomp(covers ) ,x - span.x );

    sl.add_cells(x ,len ,covers );

   end;

end;

{ sbool_unite_spans_aa }
// Functor.
// Unite two spans preserving the anti-aliasing information.
// The result is added to the "sl" scanline.
procedure sbool_unite_spans_aa(this : sbool_functor_ptr; span1 ,span2 : span_ptr; x : int; len : unsigned; sl : scanline_ptr );
var
 cover   : unsigned;
 covers1 ,
 covers2 : int8u_ptr;

begin
// Calculate the operation code and choose the
// proper combination algorithm.
// 0 = Both spans are of AA type
// 1 = span1 is solid, span2 is AA
// 2 = span1 is AA, span2 is solid
// 3 = Both spans are of solid type
 case unsigned(span1.len < 0 ) or (unsigned(span2.len < 0 ) shl 1 ) of
  0 : // Both are AA spans
   begin
    covers1:=span1.covers;
    covers2:=span2.covers;

    if span1.x < x then
     inc(ptrcomp(covers1 ) ,(x - span1.x ) * sizeof(int8u ) );

    if span2.x < x then
     inc(ptrcomp(covers2 ) ,(x - span2.x ) * sizeof(int8u ) );

    repeat
     cover:=
      this.cover_mask * this.cover_mask -
      (this.cover_mask - covers1^ ) *
      (this.cover_mask - covers2^ );

     inc(ptrcomp(covers1 ) ,sizeof(int8u ) );
     inc(ptrcomp(covers2 ) ,sizeof(int8u ) );

     if cover = this.cover_full * this.cover_full then
      sl.add_cell(x ,this.cover_full )
     else
      sl.add_cell(x ,cover shr this.cover_shift );

     inc(x );
     dec(len );

    until len = 0;

   end;

  1 : // span1 is solid, span2 is AA
   begin
    covers2:=span2.covers;

    if span2.x < x then
     inc(ptrcomp(covers2 ) ,(x - span2.x ) * sizeof(int8u ) );

    if span1.covers^ = this.cover_full then
     sl.add_span(x ,len ,this.cover_full )
    else
     repeat
      cover:=
       this.cover_mask * this.cover_mask -
       (this.cover_mask - span1.covers^ ) *
       (this.cover_mask - covers2^ );

      inc(ptrcomp(covers2 ) ,sizeof(int8u ) );

      if cover = this.cover_full * this.cover_full then
       sl.add_cell(x ,this.cover_full )
      else
       sl.add_cell(x ,cover shr this.cover_shift );

      inc(x );
      dec(len );

     until len = 0;

   end;

  2 : // span1 is AA, span2 is solid
   begin
    covers1:=span1.covers;

    if span1.x < x then
     inc(ptrcomp(covers1 ) ,(x - span1.x ) * sizeof(int8u ) );

    if span2.covers^ = this.cover_full then
     sl.add_span(x ,len ,this.cover_full )
    else
     repeat
      cover:=
       this.cover_mask * this.cover_mask -
       (this.cover_mask - covers1^ ) *
       (this.cover_mask - span2.covers^ );

      inc(ptrcomp(covers1 ) ,sizeof(int8u ) );

      if cover = this.cover_full * this.cover_full then
       sl.add_cell(x ,this.cover_full )
      else
       sl.add_cell(x ,cover shr this.cover_shift );

      inc(x ); 
      dec(len );

     until len = 0;

   end;

  3 : // Both are solid spans
   begin
    cover:=
     this.cover_mask * this.cover_mask -
     (this.cover_mask - span1.covers^ ) *
     (this.cover_mask - span2.covers^ );

    if cover = this.cover_full * this.cover_full then
     sl.add_span(x ,len ,this.cover_full )
    else
     sl.add_span(x ,len ,cover shr this.cover_shift );

   end;

 end;

end;

{ sbool_combine_spans_bin }
// Functor.
// Combine two binary encoded spans, i.e., when we don't have any
// anti-aliasing information, but only X and Length. The function
// is compatible with any type of scanlines.
procedure sbool_combine_spans_bin(this : sbool_functor_ptr; span1 ,span2 : span_ptr; x : int; len : unsigned; sl : scanline_ptr );
begin
 sl.add_span(x ,len ,this.cover_full );

end;

{ sbool_add_span_bin }
// Functor.
// Add a binary span
procedure sbool_add_span_bin(this : sbool_functor_ptr; span : span_ptr; x : int; len : unsigned; sl : scanline_ptr );
begin
 sl.add_span(x ,len ,this.cover_full );

end;

{ sbool_intersect_spans_aa }
// Functor.
// Intersect two spans preserving the anti-aliasing information.
// The result is added to the "sl" scanline.
procedure sbool_intersect_spans_aa(this : sbool_functor_ptr; span1 ,span2 : span_ptr; x : int; len : unsigned; sl : scanline_ptr );
var
 cover   : unsigned;
 covers1 ,
 covers2 : int8u_ptr;

begin
// Calculate the operation code and choose the
// proper combination algorithm.
// 0 = Both spans are of AA type
// 1 = span1 is solid, span2 is AA
// 2 = span1 is AA, span2 is solid
// 3 = Both spans are of solid type
 case unsigned(span1.len < 0 ) or (unsigned(span2.len < 0 ) shl 1 ) of
  0 : // Both are AA spans
   begin
    covers1:=span1.covers;
    covers2:=span2.covers;

    if span1.x < x then
     inc(ptrcomp(covers1 ) ,(x - span1.x ) * sizeof(int8u ) );

    if span2.x < x then
     inc(ptrcomp(covers2 ) ,(x - span2.x ) * sizeof(int8u ) );

    repeat
     cover:=covers1^ * covers2^;

     inc(ptrcomp(covers1 ) ,sizeof(int8u ) );
     inc(ptrcomp(covers2 ) ,sizeof(int8u ) );

     if cover = this.cover_full * this.cover_full then
      sl.add_cell(x ,this.cover_full )
     else
      sl.add_cell(x ,cover shr this.cover_shift );

     inc(x );
     dec(len );

    until len = 0;

   end;

  1 : // span1 is solid, span2 is AA
   begin
    covers2:=span2.covers;

    if span2.x < x then
     inc(ptrcomp(covers2 ) ,(x - span2.x ) );

    if span1.covers^ = this.cover_full then
     sl.add_cells(x ,len ,covers2 )
    else
     repeat
      cover:=span1.covers^ * covers2^;

      inc(ptrcomp(covers2 ) ,sizeof(int8u ) );

      if cover = this.cover_full * this.cover_full then
       sl.add_cell(x ,this.cover_full )
      else
       sl.add_cell(x ,cover shr this.cover_shift );

      inc(x );
      dec(len );

     until len = 0;

   end;

  2 : // span1 is AA, span2 is solid
   begin
    covers1:=span1.covers;

    if span1.x < x then
     inc(ptrcomp(covers1 ) ,(x - span1.x ) * sizeof(int8u ) );

    if span2.covers^ = this.cover_full then
     sl.add_cells(x ,len ,covers1 )
    else
     repeat
      cover:=covers1^ * span2.covers^;

      inc(ptrcomp(covers1 ) ,sizeof(int8u ) );

      if cover = this.cover_full * this.cover_full then
       sl.add_cell(x ,this.cover_full )
      else
       sl.add_cell(x ,cover shr this.cover_shift );

      inc(x );
      dec(len );

     until len = 0;

   end;

  3 : // Both are solid spans
   begin
    cover:=span1.covers^ * span2.covers^;

    if cover = this.cover_full * this.cover_full then
     sl.add_span(x ,len ,this.cover_full )
    else
     sl.add_span(x ,len ,cover shr this.cover_shift );

   end;

 end;

end;

{ sbool_xor_spans_aa }
// Functor.
// XOR two spans preserving the anti-aliasing information.
// The result is added to the "sl" scanline.
procedure sbool_xor_spans_aa(this : sbool_functor_ptr; span1 ,span2 : span_ptr; x : int; len : unsigned; sl : scanline_ptr );
var
 cover   : unsigned;
 covers1 ,
 covers2 : int8u_ptr;

begin
// Calculate the operation code and choose the
// proper combination algorithm.
// 0 = Both spans are of AA type
// 1 = span1 is solid, span2 is AA
// 2 = span1 is AA, span2 is solid
// 3 = Both spans are of solid type
 case unsigned(span1.len < 0 ) or (unsigned(span2.len < 0 ) shl 1 ) of
  0 : // Both are AA spans
   begin
    covers1:=span1.covers;
    covers2:=span2.covers;

    if span1.x < x then
     inc(ptrcomp(covers1 ) ,(x - span1.x ) * sizeof(int8u ) );

    if span2.x < x then
     inc(ptrcomp(covers2 ) ,(x - span2.x ) * sizeof(int8u ) );

    repeat
     cover:=this.formula(this ,covers1^ ,covers2^ );

     inc(ptrcomp(covers1 ) ,sizeof(int8u ) );
     inc(ptrcomp(covers2 ) ,sizeof(int8u ) );

     if cover <> 0 then
      sl.add_cell(x ,cover );

     inc(x );
     dec(len );

    until len = 0;

   end;

  1 : // span1 is solid, span2 is AA
   begin
    covers2:=span2.covers;

    if span2.x < x then
     inc(ptrcomp(covers2 ) ,(x - span2.x ) * sizeof(int8u ) );

    repeat
     cover:=this.formula(this ,span1.covers^ ,covers2^ );

     inc(ptrcomp(covers2 ) ,sizeof(int8u ) );

     if cover <> 0 then
      sl.add_cell(x ,cover );

     inc(x );
     dec(len );

    until len = 0;

   end;

  2 : // span1 is AA, span2 is solid
   begin
    covers1:=span1.covers;

    if span1.x < x then
     inc(ptrcomp(covers1 ) ,(x - span1.x ) * sizeof(int8u ) );

    repeat
     cover:=this.formula(this ,covers1^ ,span2.covers^ );

     inc(ptrcomp(covers1 ) ,sizeof(int8u ) );

     if cover <> 0 then
      sl.add_cell(x ,cover );

     inc(x );
     dec(len );

    until len = 0;

   end;

  3 : // Both are solid spans
   begin
    cover:=this.formula(this ,span1.covers^ ,span2.covers^ );

    if cover <> 0 then
     sl.add_span(x ,len ,cover );

   end;

 end;

end;

{ sbool_subtract_spans_aa }
// Functor.
// Unite two spans preserving the anti-aliasing information.
// The result is added to the "sl" scanline.
procedure sbool_subtract_spans_aa(this : sbool_functor_ptr; span1 ,span2 : span_ptr; x : int; len : unsigned; sl : scanline_ptr );
var
 cover   : unsigned;
 covers1 ,
 covers2 : int8u_ptr;

begin
// Calculate the operation code and choose the
// proper combination algorithm.
// 0 = Both spans are of AA type
// 1 = span1 is solid, span2 is AA
// 2 = span1 is AA, span2 is solid
// 3 = Both spans are of solid type
 case unsigned(span1.len < 0 ) or (unsigned(span2.len < 0 ) shl 1 ) of
  0 : // Both are AA spans
   begin
    covers1:=span1.covers;
    covers2:=span2.covers;

    if span1.x < x then
     inc(ptrcomp(covers1 ) ,(x - span1.x ) * sizeof(int8u ) );

    if span2.x < x then
     inc(ptrcomp(covers2 ) ,(x - span2.x ) * sizeof(int8u ) );

    repeat
     cover:=covers1^ * (this.cover_mask - covers2^ );

     inc(ptrcomp(covers1 ) ,sizeof(int8u ) );
     inc(ptrcomp(covers2 ) ,sizeof(int8u ) );

     if cover <> 0 then
      if cover = this.cover_full * this.cover_full then
       sl.add_cell(x ,this.cover_full )
      else
       sl.add_cell(x ,cover shr this.cover_shift );

     inc(x );
     dec(len );

    until len = 0;

   end;

  1 : // span1 is solid, span2 is AA
   begin
    covers2:=span2.covers;

    if span2.x < x then
     inc(ptrcomp(covers2 ) ,(x - span2.x ) * sizeof(int8u ) );

    repeat
     cover:=span1.covers^ * (this.cover_mask - covers2^ );

     inc(ptrcomp(covers2 ) ,sizeof(int8u ) );

     if cover <> 0 then
      if cover = this.cover_full * this.cover_full then
       sl.add_cell(x ,this.cover_full )
      else
       sl.add_cell(x ,cover shr this.cover_shift );

     inc(x );
     dec(len );

    until len = 0;

   end;

  2 : // span1 is AA, span2 is solid
   begin
    covers1:=span1.covers;

    if span1.x < x then
     inc(ptrcomp(covers1 ) ,(x - span1.x ) * sizeof(int8u ) );

    if span2.covers^ <> this.cover_full then
     repeat
      cover:=covers1^ * (this.cover_mask - span2.covers^ );

      inc(ptrcomp(covers1 ) ,sizeof(int8u ) );

      if cover <> 0 then
       if cover = this.cover_full * this.cover_full then
        sl.add_cell(x ,this.cover_full )
       else
        sl.add_cell(x ,cover shr this.cover_shift );

      inc(x );
      dec(len );

     until len = 0;

   end;

  3 : // Both are solid spans
   begin
    cover:=span1.covers^ * (this.cover_mask - span2.covers^ );

    if cover <> 0 then
     if cover = this.cover_full * this.cover_full then
      sl.add_span(x ,len ,this.cover_full )
     else
      sl.add_span(x ,len ,cover shr this.cover_shift );

   end;

 end;

end;

{ sbool_xor_formula_linear }
function sbool_xor_formula_linear(this : sbool_functor_ptr; a ,b : unsigned ) : unsigned;
var
 cover : unsigned;

begin
 cover:=a + b;

 if cover > this.cover_mask then
  cover:=this.cover_mask + this.cover_mask - cover;

 result:=cover;

end;

{ sbool_xor_formula_saddle }
function sbool_xor_formula_saddle(this : sbool_functor_ptr; a ,b : int ) : unsigned;
var
 k : unsigned;

begin
 k:=a * b;

 if k = this.cover_mask * this.cover_mask then
  result:=0
 else
  begin
   a:=(this.cover_mask * this.cover_mask - (a shl this.cover_shift ) + k ) shr this.cover_shift;
   b:=(this.cover_mask * this.cover_mask - (b shl this.cover_shift ) + k ) shr this.cover_shift;

   result:=this.cover_mask - ((a * b ) shr this.cover_shift);

  end;

end;

{ sbool_xor_formula_abs_diff }
function sbool_xor_formula_abs_diff(this : sbool_functor_ptr; a ,b : int ) : unsigned;
begin
 result:=Abs(a - b );

end;

{ sbool_add_spans_and_render }
procedure sbool_add_spans_and_render(
           sl1 ,
           sl  : scanline_ptr;
           ren : renderer_scanline_ptr;
           add_span : sbool_functor_ptr );

var
 ss ,num_spans : unsigned;

 span : span_ptr;

begin
 sl.reset_spans;

 ss       :=sl1.sz_of_span;
 span     :=sl1.begin_;
 num_spans:=sl1.num_spans;

 repeat
  add_span.functor1(add_span ,span ,span.x ,Abs(span.len ) ,sl );

  dec(num_spans );

  if num_spans = 0 then
   break;

  inc(ptrcomp(span ) ,ss ); 

 until false;

 sl.finalize(sl1.y );
 ren.render (sl );

end;

{ sbool_unite_scanlines }
// Unite two scanlines, "sl1" and "sl2" and generate a new "sl" one.
// The combine_spans functor can be of type sbool_combine_spans_bin or
// sbool_intersect_spans_aa. First is a general functor to combine
// two spans without Anti-Aliasing, the second preserves the AA
// information, but works slower
procedure sbool_unite_scanlines(
           sl1 ,
           sl2 ,
           sl  : scanline_ptr;
           add_span1 ,
           add_span2 ,
           combine_spans : sbool_functor_ptr );
const
 invalid_b = $FFFFFFF;
 invalid_e = invalid_b - 1;

var
 num1 ,num2 ,ss1 ,ss2 : unsigned;

 xb1 ,xb2 ,xe1 ,xe2 ,xb ,xe ,len : int;

 span1 ,span2 : span_ptr;

begin
 sl.reset_spans;

 num1:=sl1.num_spans;
 num2:=sl2.num_spans;

// Initialize the spans as invalid
 xb1:=invalid_b;
 xb2:=invalid_b;
 xe1:=invalid_e;
 xe2:=invalid_e;

// Initialize span1 if there are spans
 if num1 <> 0 then
  begin
   span1:=sl1.begin_;
   ss1  :=sl1.sz_of_span;
   xb1  :=span1.x;
   xe1  :=xb1 + Abs(span1.len ) - 1;

   dec(num1 );

  end;

// Initialize span2 if there are spans
 if num2 <> 0 then
  begin
   span2:=sl2.begin_;
   ss2  :=sl2.sz_of_span;
   xb2  :=span2.x;
   xe2  :=xb2 + Abs(span2.len ) - 1;

   dec(num2 );

  end;

 repeat
 // Retrieve a new span1 if it's invalid
  if (num1 <> 0 ) and
     (xb1 > xe1 ) then
   begin
    dec(num1 );
    inc(ptrcomp(span1 ) ,ss1 );

    xb1:=span1.x;
    xe1:=xb1 + abs(span1.len ) - 1;

   end;

 // Retrieve a new span2 if it's invalid
  if (num2 <> 0 ) and
     (xb2 > xe2 ) then
   begin
    dec(num2 );
    inc(ptrcomp(span2 ) ,ss2 );

    xb2:=span2.x;
    xe2:=xb2 + Abs(span2.len ) - 1;

   end;

  if (xb1 > xe1 ) and (xb2 > xe2 ) then
   break;

 // Calculate the intersection
  xb:=xb1;
  xe:=xe1;

  if xb < xb2 then
   xb:=xb2;

  if xe > xe2 then
   xe:=xe2;

  len:=xe - xb + 1; // The length of the intersection

  if len > 0 then
   begin
   // The spans intersect,
   // add the beginning of the span
    if xb1 < xb2 then
     begin
      add_span1.functor1(add_span1 ,span1 ,xb1 ,xb2 - xb1 ,sl );

      xb1:=xb2;

     end
    else
     if xb2 < xb1 then
      begin
       add_span2.functor1(add_span2 ,span2 ,xb2 ,xb1 - xb2 ,sl );

       xb2:=xb1;

      end;

   // Add the combination part of the spans
    combine_spans.functor2(combine_spans ,span1 ,span2 ,xb ,len ,sl );

   // Invalidate the fully processed span or both
    if xe1 < xe2 then
     begin
     // Invalidate span1 and eat
     // the processed part of span2
      xb1:=invalid_b;
      xe1:=invalid_e;

      inc(xb2 ,len );

     end
    else
     if xe2 < xe1 then
      begin
      // Invalidate span2 and eat
      // the processed part of span1
       xb2:=invalid_b;
       xe2:=invalid_e;

       inc(xb1 ,len );

      end
     else
      begin
       xb1:=invalid_b;  // Invalidate both
       xb2:=invalid_b;
       xe1:=invalid_e;
       xe2:=invalid_e;

      end;

   end
  else
  // The spans do not intersect
   if xb1 < xb2 then
    begin
    // Advance span1
     if xb1 <= xe1 then
      add_span1.functor1(add_span1 ,span1 ,xb1 ,xe1 - xb1 + 1 ,sl );

     xb1:=invalid_b; // Invalidate
     xe1:=invalid_e;
     
    end
   else
    begin
    // Advance span2
     if xb2 <= xe2 then
      add_span2.functor1(add_span2 ,span2 ,xb2 ,xe2 - xb2 + 1 ,sl );

     xb2:=invalid_b; // Invalidate
     xe2:=invalid_e;

    end;

 until false;

end;

{ sbool_unite_shapes }
// Unite the scanline shapes. Here the "Scanline Generator"
// abstraction is used. ScanlineGen1 and ScanlineGen2 are
// the generators, and can be of type rasterizer_scanline_aa<>.
// There function requires three scanline containers that can be
// of different type.
// "sl1" and "sl2" are used to retrieve scanlines from the generators,
// "sl" is ised as the resulting scanline to render it.
// The external "sl1" and "sl2" are used only for the sake of
// optimization and reusing of the scanline objects.
// the function calls sbool_unite_scanlines with CombineSpansFunctor
// as the last argument. See sbool_unite_scanlines for details.
procedure sbool_unite_shapes(
           sg1 ,
           sg2 : rasterizer_scanline_ptr;
           sl1 ,
           sl2 ,
           sl  : scanline_ptr;
           ren : renderer_scanline_ptr;
           add_span1 ,
           add_span2 ,
           combine_spans : sbool_functor_ptr );
var
 flag1 ,flag2 : boolean;

 r1 ,r2 ,ur : rect;

begin
// Prepare the scanline generators.
// If anyone of them doesn't contain
// any scanlines, then return.
 flag1:=sg1.rewind_scanlines;
 flag2:=sg2.rewind_scanlines;

 if not flag1 and not flag2 then
  exit;

// Get the bounding boxes
 r1.Construct(sg1._min_x ,sg1._min_y ,sg1._max_x ,sg1._max_y );
 r2.Construct(sg2._min_x ,sg2._min_y ,sg2._max_x ,sg2._max_y );

// Calculate the union of the bounding boxes
 ur:=unite_rectangles(@r1 ,@r2 );

 if not ur.is_valid then
  exit;

 ren.prepare(unsigned(ur.x2 - ur.x2 + 2 ) );

// Reset the scanlines and get two first ones
 sl.reset(ur.x1 ,ur.x2 );

 if flag1 then
  begin
   sl1.reset(sg1._min_x ,sg1._max_x );

   flag1:=sg1.sweep_scanline(sl1 );

  end;

 if flag2 then
  begin
   sl2.reset(sg2._min_x ,sg2._max_x );

   flag2:=sg2.sweep_scanline(sl2 );

  end;

// The main loop
// Here we synchronize the scanlines with
// the same Y coordinate.
 while flag1 or flag2 do
  if flag1 and flag2 then
   if sl1.y = sl2.y then
    begin
    // The Y coordinates are the same.
    // Combine the scanlines, render if they contain any spans,
    // and advance both generators to the next scanlines
     sbool_unite_scanlines(
      sl1 ,sl2 ,sl ,
      add_span1 ,add_span2 ,combine_spans );

     if sl.num_spans <> 0 then
      begin
       sl.finalize(sl1.y );
       ren.render (sl );

      end;

     flag1:=sg1.sweep_scanline(sl1 );
     flag2:=sg2.sweep_scanline(sl2 );

    end
   else
    if sl1.y < sl2.y then
     begin
      sbool_add_spans_and_render(sl1 ,sl ,ren ,add_span1 );

      flag1:=sg1.sweep_scanline(sl1 );

     end
    else
     begin
      sbool_add_spans_and_render(sl2 ,sl ,ren ,add_span2 );

      flag2:=sg2.sweep_scanline(sl2 );

     end
  else
   begin
    if flag1 then
     begin
      sbool_add_spans_and_render(sl1 ,sl ,ren ,add_span1 );

      flag1:=sg1.sweep_scanline(sl1 );

     end;

    if flag2 then
     begin
      sbool_add_spans_and_render(sl2 ,sl ,ren ,add_span2 );

      flag2:=sg2.sweep_scanline(sl2 );

     end;

   end;

end;

{ sbool_intersect_scanlines }
// Intersect two scanlines, "sl1" and "sl2" and generate a new "sl" one.
// The combine_spans functor can be of type sbool_combine_spans_bin or
// sbool_intersect_spans_aa. First is a general functor to combine
// two spans without Anti-Aliasing, the second preserves the AA
// information, but works slower
procedure sbool_intersect_scanlines(
           sl1 ,
           sl2 ,
           sl  : scanline_ptr;
           combine_spans : sbool_functor_ptr );
var
 num1 ,num2   : unsigned;
 span1 ,span2 : span_ptr;

 xb1 ,xb2 ,xe1 ,xe2 ,ss1 ,ss2 : int;

 advance_span1 ,advance_both : boolean;

begin
 sl.reset_spans;

 num1:=sl1.num_spans;

 if num1 = 0 then
  exit;

 num2:=sl2.num_spans;

 if num2 = 0 then
  exit;

 span1:=sl1.begin_;
 ss1  :=sl1.sz_of_span;
 span2:=sl2.begin_;
 ss2  :=sl2.sz_of_span;

 while(num1 <> 0 ) and
      (num2 <> 0 ) do
  begin
   xb1:=span1.x;
   xb2:=span2.x;
   xe1:=xb1 + Abs(span1.len ) - 1;
   xe2:=xb2 + Abs(span2.len ) - 1;

  // Determine what spans we should advance in the next step
  // The span with the least ending X should be advanced
  // advance_both is just an optimization when we ending
  // coordinates are the same and we can advance both
   advance_span1:= xe1 <  xe2;
   advance_both := xe1 = xe2;

  // Find the intersection of the spans
  // and check if they intersect
   if xb1 < xb2 then
    xb1:=xb2;

   if xe1 > xe2 then
    xe1:=xe2;

   if xb1 <= xe1 then
    combine_spans.functor2(combine_spans ,span1 ,span2 ,xb1 ,xe1 - xb1 + 1 ,sl );

  // Advance the spans
   if advance_both then
    begin
     dec(num1 );
     dec(num2 );

     if num1 <> 0 then
      inc(ptrcomp(span1 ) ,ss1 );

     if num2 <> 0 then
      inc(ptrcomp(span2 ) ,ss2 );

    end
   else
    if advance_span1 then
     begin
      dec(num1 );

      if num1 <> 0 then
       inc(ptrcomp(span1 ) ,ss1 );

     end
    else
     begin
      dec(num2 );

      if num2 <> 0 then
       inc(ptrcomp(span2 ) ,ss2 );

     end;

  end;

end;

{ sbool_intersect_shapes }
// Intersect the scanline shapes. Here the "Scanline Generator"
// abstraction is used. ScanlineGen1 and ScanlineGen2 are
// the generators, and can be of type rasterizer_scanline_aa<>.
// There function requires three scanline containers that can be of
// different types.
// "sl1" and "sl2" are used to retrieve scanlines from the generators,
// "sl" is ised as the resulting scanline to render it.
// The external "sl1" and "sl2" are used only for the sake of
// optimization and reusing of the scanline objects.
// the function calls sbool_intersect_scanlines with CombineSpansFunctor
// as the last argument. See sbool_intersect_scanlines for details.
procedure sbool_intersect_shapes(
           sg1 ,
           sg2 : rasterizer_scanline_ptr;
           sl1 ,
           sl2 ,
           sl  : scanline_ptr;
           ren : renderer_scanline_ptr;
           combine_spans : sbool_functor_ptr );
var
 r1 ,r2 ,ir : rect;

begin
// Prepare the scanline generators.
// If anyone of them doesn't contain
// any scanlines, then return.
 if not sg1.rewind_scanlines then
  exit;

 if not sg2.rewind_scanlines then
  exit;

// Get the bounding boxes
 r1.Construct(sg1._min_x ,sg1._min_y ,sg1._max_x ,sg1._max_y );
 r2.Construct(sg2._min_x ,sg2._min_y ,sg2._max_x ,sg2._max_y );
 
// Calculate the intersection of the bounding
// boxes and return if they don't intersect.
 ir:=intersect_rectangles(@r1 ,@r2 );

 if not ir.is_valid then
  exit;

// Reset the scanlines and get two first ones
 sl.reset (ir.x1 ,ir.x2 );
 sl1.reset(sg1._min_x ,sg1._max_x );
 sl2.reset(sg2._min_x ,sg2._max_x );

 if not sg1.sweep_scanline(sl1 ) then
  exit;

 if not sg2.sweep_scanline(sl2 ) then
  exit;

 ren.prepare(unsigned(ir.x2 - ir.x1 + 2 ) );

// The main loop
// Here we synchronize the scanlines with
// the same Y coordinate, ignoring all other ones.
// Only scanlines having the same Y-coordinate
// are to be combined.
 repeat
  while sl1.y < sl2.y do
   if not sg1.sweep_scanline(sl1 ) then
    exit;

  while sl2.y < sl1.y do
   if not sg2.sweep_scanline(sl2 ) then
    exit;

  if sl1.y = sl2.y then
   begin
   // The Y coordinates are the same.
   // Combine the scanlines, render if they contain any spans,
   // and advance both generators to the next scanlines
    sbool_intersect_scanlines(sl1 ,sl2 ,sl ,combine_spans );

    if sl.num_spans <> 0 then
     begin
      sl.finalize(sl1.y );
      ren.render (sl );

     end;

    if not sg1.sweep_scanline(sl1 ) then
     exit;

    if not sg2.sweep_scanline(sl2 ) then
     exit;

   end;

 until false;

end;

{ sbool_subtract_shapes }
// Subtract the scanline shapes, "sg1-sg2". Here the "Scanline Generator"
// abstraction is used. ScanlineGen1 and ScanlineGen2 are
// the generators, and can be of type rasterizer_scanline_aa<>.
// There function requires three scanline containers that can be of
// different types.
// "sl1" and "sl2" are used to retrieve scanlines from the generators,
// "sl" is ised as the resulting scanline to render it.
// The external "sl1" and "sl2" are used only for the sake of
// optimization and reusing of the scanline objects.
// the function calls sbool_intersect_scanlines with CombineSpansFunctor
// as the last argument. See combine_scanlines_sub for details.
procedure sbool_subtract_shapes(
           sg1 ,
           sg2 : rasterizer_scanline_ptr;
           sl1 ,
           sl2 ,
           sl  : scanline_ptr;
           ren : renderer_scanline_ptr;
           add_span1 ,
           combine_spans : sbool_functor_ptr );
var
 r1 : rect;

 flag1 ,flag2 : boolean;

 add_span2 : sbool_functor;

begin
// Prepare the scanline generators.
// Here "sg1" is master, "sg2" is slave.
 if not sg1.rewind_scanlines then
  exit;

 flag2:=sg2.rewind_scanlines;

// Get the bounding box
 r1.Construct(sg1._min_x ,sg1._min_y ,sg1._max_x ,sg1._max_y );

// Reset the scanlines and get two first ones
 sl.reset (sg1._min_x ,sg1._max_x );
 sl1.reset(sg1._min_x ,sg1._max_x );
 sl2.reset(sg2._min_x ,sg2._max_x );

 if not sg1.sweep_scanline(sl1 ) then
  exit;

 if flag2 then
  flag2:=sg2.sweep_scanline(sl2 );

 ren.prepare(unsigned(sg1._max_x - sg1._min_x + 2 ) );

// A fake span2 processor
 add_span2.Construct1(@sbool_add_span_empty );

// The main loop
// Here we synchronize the scanlines with
// the same Y coordinate, ignoring all other ones.
// Only scanlines having the same Y-coordinate
// are to be combined.
 flag1:=true;

 repeat
 // Synchronize "slave" with "master"
  while flag2 and
        (sl2.y < sl1.y ) do
   flag2:=sg2.sweep_scanline(sl2 );

  if flag2 and
     (sl2.y = sl1.y ) then
   begin
   // The Y coordinates are the same.
   // Combine the scanlines and render if they contain any spans.
    sbool_unite_scanlines(sl1 ,sl2 ,sl ,add_span1 ,@add_span2 ,combine_spans );

    if sl.num_spans <> 0 then
     begin
      sl.finalize(sl1.y );
      ren.render (sl );

     end;

   end
  else
   sbool_add_spans_and_render(sl1 ,sl ,ren ,add_span1 );

 // Advance the "master"
  flag1:=sg1.sweep_scanline(sl1 ); 

 until not flag1;

end;

{ SBOOL_SUBSTRACT_SHAPES_AA }
// Subtract shapes "sg1-sg2" with anti-aliasing
// See intersect_shapes_aa for more comments
procedure sbool_subtract_shapes_aa;
var
 add_functor     ,
 combine_functor : sbool_functor;

begin
 add_functor.Construct1    (@sbool_add_span_aa );
 combine_functor.Construct2(@sbool_subtract_spans_aa );

 sbool_subtract_shapes(
  sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren ,
  @add_functor ,
  @combine_functor );

end;

{ SBOOL_XOR_SHAPES_ABS_DIFF_AA }
// Apply eXclusive OR to two anti-aliased scanline shapes.
// There's the absolute difference used to calculate
// Anti-Aliasing values, that is:
// a XOR b : abs(a-b)
// See intersect_shapes_aa for more comments
procedure sbool_xor_shapes_abs_diff_aa;
var
 add_functor     ,
 combine_functor : sbool_functor;

begin
 add_functor.Construct1    (@sbool_add_span_aa );
 combine_functor.Construct2(@sbool_xor_spans_aa );

 combine_functor.formula:=@sbool_xor_formula_abs_diff;

 sbool_unite_shapes(
  sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren ,
  @add_functor ,
  @add_functor ,
  @combine_functor );

end;

{ SBOOL_XOR_SHAPES_SADDLE_AA }
// Apply eXclusive OR to two anti-aliased scanline shapes.
// There's the classical "Saddle" used to calculate the
// Anti-Aliasing values, that is:
// a XOR b : 1-((1-a+a*b)*(1-b+a*b))
// See intersect_shapes_aa for more comments
procedure sbool_xor_shapes_saddle_aa;
var
 add_functor     ,
 combine_functor : sbool_functor;

begin
 add_functor.Construct1    (@sbool_add_span_aa );
 combine_functor.Construct2(@sbool_xor_spans_aa );

 combine_functor.formula:=@sbool_xor_formula_saddle;

 sbool_unite_shapes(
  sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren ,
  @add_functor ,
  @add_functor ,
  @combine_functor );

end;

{ SBOOL_XOR_SHAPES_AA }
// Apply eXclusive OR to two anti-aliased scanline shapes. There's
// a modified "Linear" XOR used instead of classical "Saddle" one.
// The reason is to have the result absolutely conststent with what
// the scanline rasterizer produces.
// See intersect_shapes_aa for more comments
procedure sbool_xor_shapes_aa;
var
 add_functor     ,
 combine_functor : sbool_functor;

begin
 add_functor.Construct1    (@sbool_add_span_aa );
 combine_functor.Construct2(@sbool_xor_spans_aa );

 combine_functor.formula:=@sbool_xor_formula_linear;

 sbool_unite_shapes(
  sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren ,
  @add_functor ,
  @add_functor ,
  @combine_functor );

end;

{ SBOOL_INTERSECT_SHAPES_AA }
// Intersect two anti-aliased scanline shapes.
// Here the "Scanline Generator" abstraction is used.
// ScanlineGen1 and ScanlineGen2 are the generators, and can be of
// type rasterizer_scanline_aa<>. There function requires three
// scanline containers that can be of different types.
// "sl1" and "sl2" are used to retrieve scanlines from the generators,
// "sl" is ised as the resulting scanline to render it.
// The external "sl1" and "sl2" are used only for the sake of
// optimization and reusing of the scanline objects.
procedure sbool_intersect_shapes_aa;
var
 combine_functor : sbool_functor;

begin
 combine_functor.Construct2(@sbool_intersect_spans_aa );

 sbool_intersect_shapes(
  sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren ,
  @combine_functor );

end;

{ SBOOL_UNITE_SHAPES_AA }
// Unite two anti-aliased scanline shapes
// See intersect_shapes_aa for more comments
procedure sbool_unite_shapes_aa;
var
 add_functor     ,
 combine_functor : sbool_functor;

begin
 add_functor.Construct1    (@sbool_add_span_aa );
 combine_functor.Construct2(@sbool_unite_spans_aa );

 sbool_unite_shapes(
  sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren ,
  @add_functor ,
  @add_functor ,
  @combine_functor );

end;

{ SBOOL_COMBINE_SHAPES_AA }
procedure sbool_combine_shapes_aa;
begin
 case op of
  sbool_or :
   sbool_unite_shapes_aa(sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren );

  sbool_and :
   sbool_intersect_shapes_aa(sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren );

  sbool_xor :
   sbool_xor_shapes_aa(sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren );

  sbool_xor_saddle :
   sbool_xor_shapes_saddle_aa(sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren );

  sbool_xor_abs_diff :
   sbool_xor_shapes_abs_diff_aa(sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren );

  sbool_a_minus_b :
   sbool_subtract_shapes_aa(sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren );

  sbool_b_minus_a :
   sbool_subtract_shapes_aa(sg2 ,sg1 ,sl2 ,sl1 ,sl ,ren );

 end;

end;

{ SBOOL_SUBTRACT_SHAPES_BIN }
procedure sbool_subtract_shapes_bin;
var
 add_functor     ,
 combine_functor : sbool_functor;

begin
 add_functor.Construct1    (@sbool_add_span_bin );
 combine_functor.Construct2(@sbool_combine_spans_empty );

 sbool_subtract_shapes(
  sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren ,
  @add_functor ,
  @combine_functor );

end;

{ SBOOL_XOR_SHAPES_BIN }
procedure sbool_xor_shapes_bin;
var
 add_functor     ,
 combine_functor : sbool_functor;

begin
 add_functor.Construct1    (@sbool_add_span_bin );
 combine_functor.Construct2(@sbool_combine_spans_empty );

 sbool_unite_shapes(
  sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren ,
  @add_functor ,
  @add_functor ,
  @combine_functor );

end;

{ SBOOL_INTERSECT_SHAPES_BIN }
procedure sbool_intersect_shapes_bin;
var
 combine_functor : sbool_functor;

begin
 combine_functor.Construct2(@sbool_combine_spans_bin );

 sbool_intersect_shapes(
  sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren ,
  @combine_functor );

end;

{ SBOOL_UNITE_SHAPES_BIN }
procedure sbool_unite_shapes_bin;
var
 add_functor     ,
 combine_functor : sbool_functor;

begin
 add_functor.Construct1    (@sbool_add_span_bin );
 combine_functor.Construct2(@sbool_combine_spans_bin );

 sbool_unite_shapes(
  sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren ,
  @add_functor ,
  @add_functor ,
  @combine_functor );

end;

{ SBOOL_COMBINE_SHAPES_BIN }
procedure sbool_combine_shapes_bin;
begin
 case op of
  sbool_or :
   sbool_unite_shapes_bin(sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren );

  sbool_and :
   sbool_intersect_shapes_bin(sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren );

  sbool_xor ,sbool_xor_saddle ,sbool_xor_abs_diff :
   sbool_xor_shapes_bin(sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren );

  sbool_a_minus_b :
   sbool_subtract_shapes_bin(sg1 ,sg2 ,sl1 ,sl2 ,sl ,ren );

  sbool_b_minus_a :
   sbool_subtract_shapes_bin(sg2 ,sg1 ,sl2 ,sl1 ,sl ,ren );

 end;

end;

END.

