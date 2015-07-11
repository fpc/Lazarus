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
// 25.01.2006-Milano: Unit port establishment
//
{ agg_conv_segmentator.pas }
unit
 agg_conv_segmentator ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_vertex_source ,
 agg_conv_adaptor_vpgen ,
 agg_vpgen_segmentator ;

{ TYPES DEFINITION }
type
 conv_segmentator = object(conv_adaptor_vpgen )
   the_generator : vpgen_segmentator;

   constructor Construct(vs : vertex_source_ptr );
   destructor  Destruct; virtual;

   procedure approximation_scale_(s : double );
   function  _approximation_scale : double;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor conv_segmentator.Construct;
begin
 the_generator.Construct;

 inherited Construct(vs ,@the_generator );

end;

{ DESTRUCT }
destructor conv_segmentator.Destruct;
begin
 inherited Destruct;

 the_generator.Destruct;

end;

{ APPROXIMATION_SCALE_ }
procedure conv_segmentator.approximation_scale_;
begin
 vpgen.approximation_scale_(s );

end;

{ _APPROXIMATION_SCALE }
function conv_segmentator._approximation_scale;
begin
 result:=vpgen._approximation_scale;

end;

END.

