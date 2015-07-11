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
// 25.02.2006-Milano: Unit port establishment
//
{ agg_conv_smooth_poly1.pas }
unit
 agg_conv_smooth_poly1 ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_vcgen_smooth_poly1 ,
 agg_conv_adaptor_vcgen ,
 agg_conv_curve ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 conv_smooth_poly1 = object(conv_adaptor_vcgen )
   the_generator : vcgen_smooth_poly1;

   constructor Construct(vs : vertex_source_ptr );
   destructor  Destruct; virtual;

   procedure smooth_value_(v : double );
   function  _smooth_value : double;

  end;

 conv_smooth_poly1_curve = object(conv_curve )
   m_smooth : conv_smooth_poly1;

   constructor Construct(vs : vertex_source_ptr );
   destructor  Destruct; virtual;

   procedure smooth_value_(v : double );
   function  _smooth_value : double;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor conv_smooth_poly1.Construct;
begin
 the_generator.Construct;

 inherited Construct(vs ,@the_generator );

end;

{ DESTRUCT }
destructor conv_smooth_poly1.Destruct;
begin
 inherited Destruct;

 the_generator.Destruct;

end;

{ SMOOTH_VALUE_ }
procedure conv_smooth_poly1.smooth_value_;
begin
 vcgen_smooth_poly1_ptr(generator ).smooth_value_(v );

end;

{ _SMOOTH_VALUE }
function conv_smooth_poly1._smooth_value;
begin
 result:=vcgen_smooth_poly1_ptr(generator )._smooth_value;

end;

{ CONSTRUCT }
constructor conv_smooth_poly1_curve.Construct;
begin
 m_smooth.Construct(vs );

 inherited Construct(@m_smooth );

end;

{ DESTRUCT }
destructor conv_smooth_poly1_curve.Destruct;
begin
 inherited Destruct;

 m_smooth.Destruct;

end;

{ SMOOTH_VALUE_ }
procedure conv_smooth_poly1_curve.smooth_value_;
begin
 vcgen_smooth_poly1_ptr(m_smooth.generator ).smooth_value_(v );

end;

{ _SMOOTH_VALUE }
function conv_smooth_poly1_curve._smooth_value;
begin
 result:=vcgen_smooth_poly1_ptr(m_smooth.generator )._smooth_value;

end;

END.

