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
// 16.02.2006-Milano: Unit port establishment
//
{ agg_font_engine.pas }
unit
 agg_font_engine ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_scanline_storage_aa ,
 agg_scanline_storage_bin ,
 agg_path_storage_integer ;

{ TYPES DEFINITION }
type
 gray8_adaptor_type_ptr = ^gray8_adaptor_type;
 gray8_adaptor_type = serialized_scanlines_adaptor_aa8;

 gray8_scanline_type_ptr = ^gray8_scanline_type;
 gray8_scanline_type = embedded_scanline_sa;

 mono_adaptor_type_ptr = ^mono_adaptor_type;
 mono_adaptor_type = serialized_scanlines_adaptor_bin;

 mono_scanline_type_ptr = ^mono_scanline_type;
 mono_scanline_type = embedded_scanline_a;

 scanlines_aa_type_ptr = ^scanlines_aa_type;
 scanlines_aa_type = scanline_storage_aa8;

 scanlines_bin_type_ptr = ^scanlines_bin_type;
 scanlines_bin_type = scanline_storage_bin;

 path_adaptor_type_ptr = ^path_adaptor_type;
 path_adaptor_type = serialized_integer_path_adaptor;

 font_engine_ptr = ^font_engine;
 font_engine = object
  // Interface mandatory to implement for font_cache_manager
   function  font_signature : PChar; virtual; abstract;
   function  change_stamp : int; virtual; abstract;

   function  prepare_glyph(glyph_code : unsigned ) : boolean; virtual; abstract;

   function  glyph_index : unsigned; virtual; abstract;
   function  data_size : unsigned; virtual; abstract;
   function  data_type : unsigned; virtual; abstract;
   function  bounds : rect_ptr; virtual; abstract;
   function  advance_x : double; virtual; abstract;
   function  advance_y : double; virtual; abstract;

   procedure write_glyph_to(data : int8u_ptr ); virtual; abstract;
   function  add_kerning   (first ,second : unsigned; x ,y : double_ptr ) : boolean; virtual; abstract;

   function  flag32 : boolean; virtual; abstract;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }



END.

