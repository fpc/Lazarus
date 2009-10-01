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
// 26.02.2006-Milano: Unit port establishment
//
{ agg_conv_marker_adaptor.pas }
unit
 agg_conv_marker_adaptor ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_conv_adaptor_vcgen ,
 agg_vcgen_vertex_sequence ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
 conv_marker_adaptor_ptr = ^conv_marker_adaptor;
 conv_marker_adaptor = object(conv_adaptor_vcgen )
   the_generator : vcgen_vertex_sequence;

   constructor Construct(vs : vertex_source_ptr );
   destructor  Destruct; virtual;

   procedure shorten_(s : double );
   function  _shorten : double;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor conv_marker_adaptor.Construct;
begin
 the_generator.Construct;

 inherited Construct(vs ,@the_generator );

end;

{ DESTRUCT }
destructor conv_marker_adaptor.Destruct;
begin
 inherited Destruct;

 the_generator.Destruct;

end;

{ SHORTEN_ }
procedure conv_marker_adaptor.shorten_;
begin
 the_generator.shorten_(s );

end;

{ _SHORTEN }
function conv_marker_adaptor._shorten;
begin
 result:=the_generator._shorten;

end;

END.

