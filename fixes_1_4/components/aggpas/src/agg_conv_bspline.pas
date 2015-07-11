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
// 18.01.2006-Milano: Unit port establishment
//
{ agg_conv_bspline.pas }
unit
 agg_conv_bspline ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_vcgen_bspline ,
 agg_conv_adaptor_vcgen ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 conv_bspline = object(conv_adaptor_vcgen )
   the_generator : vcgen_bspline;

   constructor Construct(vs : vertex_source_ptr );
   destructor  Destruct; virtual;

   procedure interpolation_step_(v : double );
   function  _interpolation_step : double;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor conv_bspline.Construct;
begin
 the_generator.Construct;

 inherited Construct(vs ,@the_generator );

end;

{ DESTRUCT }
destructor conv_bspline.Destruct;
begin
 inherited Destruct;

 the_generator.Destruct;
 
end;

{ INTERPOLATION_STEP_ }
procedure conv_bspline.interpolation_step_;
begin
 vcgen_bspline_ptr(generator ).interpolation_step_(v );

end;

{ _INTERPOLATION_STEP }
function conv_bspline._interpolation_step;
begin
 result:=vcgen_bspline_ptr(generator )._interpolation_step;

end;

END.

