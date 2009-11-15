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
// conv_dash
//
// [Pascal Port History] -----------------------------------------------------
//
// 26.01.2006-Milano: Unit port establishment
//
{ agg_conv_dash.pas }
unit
 agg_conv_dash ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_vertex_source ,
 agg_conv_adaptor_vcgen ,
 agg_vcgen_dash ;

{ TYPES DEFINITION }
type
 conv_dash = object(conv_adaptor_vcgen )
   the_generator : vcgen_dash;

   constructor Construct(vs : vertex_source_ptr );
   destructor  Destruct; virtual;

   procedure remove_all_dashes;
   procedure add_dash  (dash_len ,gap_len : double );
   procedure dash_start(ds : double );

   procedure shorten_(s : double );
   function  _shorten : double;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor conv_dash.Construct;
begin
 the_generator.Construct;

 inherited Construct(vs ,@the_generator );

end;

{ DESTRUCT }
destructor conv_dash.Destruct;
begin
 inherited Destruct;

 the_generator.Destruct;

end;

{ REMOVE_ALL_DASHES }
procedure conv_dash.remove_all_dashes;
begin
 vcgen_dash_ptr(generator ).remove_all_dashes;

end;

{ ADD_DASH }
procedure conv_dash.add_dash;
begin
 vcgen_dash_ptr(generator ).add_dash(dash_len ,gap_len );

end;

{ DASH_START }
procedure conv_dash.dash_start;
begin
 vcgen_dash_ptr(generator ).dash_start(ds );

end;

{ SHORTEN_ }
procedure conv_dash.shorten_;
begin
 vcgen_dash_ptr(generator ).shorten_(s );

end;

{ _SHORTEN }
function conv_dash._shorten;
begin
 result:=vcgen_dash_ptr(generator )._shorten;

end;

END.

