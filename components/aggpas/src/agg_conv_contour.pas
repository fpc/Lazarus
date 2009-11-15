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
// 12.02.2006-Milano: Unit port establishment
//
{ agg_conv_contour.pas }
unit
 agg_conv_contour ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_vcgen_contour ,
 agg_conv_adaptor_vcgen ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 conv_contour = object(conv_adaptor_vcgen )
   the_generator : vcgen_contour;

   constructor Construct(vs : vertex_source_ptr );
   destructor  Destruct; virtual;

   procedure line_join_ (lj : unsigned );
   procedure inner_join_(ij : unsigned );

   procedure width_(w : double );

   procedure miter_limit_        (ml : double );
   procedure miter_limit_theta_  (t : double );
   procedure inner_miter_limit_  (ml : double );
   procedure approximation_scale_(_as_ : double );

   procedure auto_detect_orientation_(v : boolean );

   function  _line_join : unsigned;
   function  _inner_join : unsigned;
   function  _width : double;
   function  _miter_limit : double;
   function  _inner_miter_limit : double;
   function  _approximation_scale : double;
   function  _auto_detect_orientation : boolean;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor conv_contour.Construct;
begin
 the_generator.Construct;

 inherited Construct(vs ,@the_generator );

end;

{ DESTRUCT }
destructor conv_contour.Destruct;
begin
 inherited Destruct;

 the_generator.Destruct;

end;

{ LINE_JOIN_ }
procedure conv_contour.line_join_;
begin
 vcgen_contour_ptr(generator ).line_join_(lj );

end;

{ INNER_JOIN_ }
procedure conv_contour.inner_join_;
begin
 vcgen_contour_ptr(generator ).inner_join_(ij );

end;

{ WIDTH_ }
procedure conv_contour.width_;
begin
 vcgen_contour_ptr(generator ).width_(w );

end;

{ MITER_LIMIT_ }
procedure conv_contour.miter_limit_;
begin
 vcgen_contour_ptr(generator ).miter_limit_(ml );

end;

{ MITER_LIMIT_THETA_ }
procedure conv_contour.miter_limit_theta_;
begin
 vcgen_contour_ptr(generator ).miter_limit_theta_(t );

end;

{ INNER_MITER_LIMIT_ }
procedure conv_contour.inner_miter_limit_;
begin
 vcgen_contour_ptr(generator ).inner_miter_limit_(ml);

end;

{ APPROXIMATION_SCALE_ }
procedure conv_contour.approximation_scale_;
begin
 vcgen_contour_ptr(generator ).approximation_scale_(_as_ );

end;

{ AUTO_DETECT_ORIENTATION_ }
procedure conv_contour.auto_detect_orientation_;
begin
 vcgen_contour_ptr(generator ).auto_detect_orientation_(v );

end;

{ _LINE_JOIN }
function conv_contour._line_join;
begin
 result:=vcgen_contour_ptr(generator )._line_join;

end;

{ _INNER_JOIN }
function conv_contour._inner_join;
begin
 result:=vcgen_contour_ptr(generator )._inner_join;

end;

{ _WIDTH }
function conv_contour._width;
begin
 result:=vcgen_contour_ptr(generator )._width;

end;

{ _MITER_LIMIT }
function conv_contour._miter_limit;
begin
 result:=vcgen_contour_ptr(generator )._miter_limit;

end;

{ _INNER_MITER_LIMIT }
function conv_contour._inner_miter_limit;
begin
 result:=vcgen_contour_ptr(generator )._inner_miter_limit;

end;

{ _APPROXIMATION_SCALE }
function conv_contour._approximation_scale;
begin
 result:=vcgen_contour_ptr(generator )._approximation_scale;

end;

{ _AUTO_DETECT_ORIENTATION }
function conv_contour._auto_detect_orientation;
begin
 result:=vcgen_contour_ptr(generator )._auto_detect_orientation;

end;

END.

