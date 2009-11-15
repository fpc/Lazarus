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
// conv_stroke
//
// [Pascal Port History] -----------------------------------------------------
//
// 18.10.2007-Milano: conv_stroke_math
// 21.12.2005-Milano: Unit port establishment
//
{ agg_conv_stroke.pas }
unit
 agg_conv_stroke ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_vertex_source ,
 agg_vcgen_stroke ,
 agg_conv_adaptor_vcgen ;

{ TYPES DEFINITION }
type
 conv_stroke_ptr = ^conv_stroke;
 conv_stroke = object(conv_adaptor_vcgen )
   the_generator : vcgen_stroke;

   constructor Construct(vs : vertex_source_ptr );
   destructor  Destruct; virtual;

   procedure line_cap_  (lc : unsigned );
   procedure line_join_ (lj : unsigned );
   procedure inner_join_(ij : unsigned );

   function  _line_cap : unsigned;
   function  _line_join : unsigned;
   function  _inner_join : unsigned;

   procedure width_              (w : double );
   procedure miter_limit_        (ml : double );
   procedure miter_limit_theta_  (t : double );
   procedure inner_miter_limit_  (ml : double );
   procedure approximation_scale_(_as_ : double );

   function  _width : double;
   function  _miter_limit : double;
   function  _inner_miter_limit : double;
   function  _approximation_scale : double;

   procedure shorten_(s : double );
   function  _shorten : double;

  end;

 conv_stroke_math = object(conv_adaptor_vcgen )
   the_generator : vcgen_stroke_math;

   constructor Construct(vs : vertex_source_ptr );
   destructor  Destruct; virtual;

   procedure line_cap_  (lc : unsigned );
   procedure line_join_ (lj : unsigned );
   procedure inner_join_(ij : unsigned );

   function  _line_cap : unsigned;
   function  _line_join : unsigned;
   function  _inner_join : unsigned;

   procedure width_              (w : double );
   procedure miter_limit_        (ml : double );
   procedure miter_limit_theta_  (t : double );
   procedure inner_miter_limit_  (ml : double );
   procedure approximation_scale_(_as_ : double );

   function  _width : double;
   function  _miter_limit : double;
   function  _inner_miter_limit : double;
   function  _approximation_scale : double;

   procedure shorten_(s : double );
   function  _shorten : double;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor conv_stroke.Construct;
begin
 the_generator.Construct;

 inherited Construct(vs ,@the_generator );

end;

{ DESTRUCT }
destructor conv_stroke.Destruct;
begin
 inherited Destruct;

 the_generator.Destruct;

end;

{ LINE_CAP_ }
procedure conv_stroke.line_cap_;
begin
 vcgen_stroke_ptr(generator ).line_cap_(lc );

end;

{ LINE_JOIN_ }
procedure conv_stroke.line_join_;
begin
 vcgen_stroke_ptr(generator ).line_join_(lj );

end;

{ INNER_JOIN_ }
procedure conv_stroke.inner_join_;
begin
 vcgen_stroke_ptr(generator ).inner_join_(ij );

end;

{ _LINE_CAP }
function conv_stroke._line_cap;
begin
 result:=vcgen_stroke_ptr(generator )._line_cap;

end;

{ _LINE_JOIN }
function conv_stroke._line_join;
begin
 result:=vcgen_stroke_ptr(generator )._line_join;

end;

{ _INNER_JOIN }
function conv_stroke._inner_join;
begin
 result:=vcgen_stroke_ptr(generator )._inner_join;

end;

{ WIDHT_ }
procedure conv_stroke.width_;
begin
 vcgen_stroke_ptr(generator ).width_(w );

end;

{ MITER_LIMIT_ }
procedure conv_stroke.miter_limit_;
begin
 vcgen_stroke_ptr(generator ).miter_limit_(ml );

end;

{ MITER_LIMIT_THETA_ }
procedure conv_stroke.miter_limit_theta_;
begin
 vcgen_stroke_ptr(generator ).miter_limit_theta_(t );

end;

{ INNER_MITER_LIMIT_ }
procedure conv_stroke.inner_miter_limit_;
begin
 vcgen_stroke_ptr(generator ).inner_miter_limit_(ml );

end;

{ APPROXIMATION_SCALE_ }
procedure conv_stroke.approximation_scale_;
begin
 vcgen_stroke_ptr(generator ).approximation_scale_(_as_ );

end;

{ _WIDTH }
function conv_stroke._width;
begin
 result:=vcgen_stroke_ptr(generator )._width;

end;

{ _MITER_LIMIT }
function conv_stroke._miter_limit;
begin
 result:=vcgen_stroke_ptr(generator )._miter_limit;

end;

{ _INNER_MITER_LIMIT }
function conv_stroke._inner_miter_limit;
begin
 result:=vcgen_stroke_ptr(generator )._inner_miter_limit;

end;

{ _APPROXIMATION_SCALE }
function conv_stroke._approximation_scale;
begin
 result:=vcgen_stroke_ptr(generator )._approximation_scale;

end;

{ SHORTEN_ }
procedure conv_stroke.shorten_;
begin
 vcgen_stroke_ptr(generator ).shorten_(s );

end;

{ _SHORTEN }
function conv_stroke._shorten;
begin
 result:=vcgen_stroke_ptr(generator )._shorten;

end;

{ CONSTRUCT }
constructor conv_stroke_math.Construct;
begin
 the_generator.Construct;

 inherited Construct(vs ,@the_generator );

end;

{ DESTRUCT }
destructor conv_stroke_math.Destruct;
begin
 inherited Destruct;

 the_generator.Destruct;

end;

{ LINE_CAP_ }
procedure conv_stroke_math.line_cap_;
begin
 vcgen_stroke_math_ptr(generator ).line_cap_(lc );

end;

{ LINE_JOIN_ }
procedure conv_stroke_math.line_join_;
begin
 vcgen_stroke_math_ptr(generator ).line_join_(lj );

end;

{ INNER_JOIN_ }
procedure conv_stroke_math.inner_join_;
begin
 vcgen_stroke_math_ptr(generator ).inner_join_(ij );

end;

{ _LINE_CAP }
function conv_stroke_math._line_cap;
begin
 result:=vcgen_stroke_math_ptr(generator )._line_cap;

end;

{ _LINE_JOIN }
function conv_stroke_math._line_join;
begin
 result:=vcgen_stroke_math_ptr(generator )._line_join;

end;

{ _INNER_JOIN }
function conv_stroke_math._inner_join;
begin
 result:=vcgen_stroke_math_ptr(generator )._inner_join;

end;

{ WIDHT_ }
procedure conv_stroke_math.width_;
begin
 vcgen_stroke_math_ptr(generator ).width_(w );

end;

{ MITER_LIMIT_ }
procedure conv_stroke_math.miter_limit_;
begin
 vcgen_stroke_math_ptr(generator ).miter_limit_(ml );

end;

{ MITER_LIMIT_THETA_ }
procedure conv_stroke_math.miter_limit_theta_;
begin
 vcgen_stroke_math_ptr(generator ).miter_limit_theta_(t );

end;

{ INNER_MITER_LIMIT_ }
procedure conv_stroke_math.inner_miter_limit_;
begin
 vcgen_stroke_math_ptr(generator ).inner_miter_limit_(ml );

end;

{ APPROXIMATION_SCALE_ }
procedure conv_stroke_math.approximation_scale_;
begin
 vcgen_stroke_math_ptr(generator ).approximation_scale_(_as_ );

end;

{ _WIDTH }
function conv_stroke_math._width;
begin
 result:=vcgen_stroke_math_ptr(generator )._width;

end;

{ _MITER_LIMIT }
function conv_stroke_math._miter_limit;
begin
 result:=vcgen_stroke_math_ptr(generator )._miter_limit;

end;

{ _INNER_MITER_LIMIT }
function conv_stroke_math._inner_miter_limit;
begin
 result:=vcgen_stroke_math_ptr(generator )._inner_miter_limit;

end;

{ _APPROXIMATION_SCALE }
function conv_stroke_math._approximation_scale;
begin
 result:=vcgen_stroke_math_ptr(generator )._approximation_scale;

end;

{ SHORTEN_ }
procedure conv_stroke_math.shorten_;
begin
 vcgen_stroke_math_ptr(generator ).shorten_(s );

end;

{ _SHORTEN }
function conv_stroke_math._shorten;
begin
 result:=vcgen_stroke_math_ptr(generator )._shorten;

end;

END.

