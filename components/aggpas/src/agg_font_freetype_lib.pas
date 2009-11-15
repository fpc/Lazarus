//----------------------------------------------------------------------------
// Anti-Grain Geometry - Version 2.4 (Public License)
// Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)
//
// Anti-Grain Geometry - Version 2.4 Release Milano 3 (AggPas 2.4 RM3)
// Pascal Port By: Milan Marusinec alias Milano
//                 milan@marusinec.sk
//                 http://www.aggpas.org
// Copyright (c) 2005-2007
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
// 17.09.2007-Milano: Finished OK
// 16.09.2007-Milano: Unit port establishment
//
{ agg_font_freetype_lib.pas }
unit
 agg_font_freetype_lib ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ;

{ GLOBAL VARIABLES & CONSTANTS }
type
 FT_Encoding = array[0..3 ] of char;

const
{$IFDEF AGG_WINDOWS }
 ft_lib = 'freetype.dll';

{$ENDIF }

{$IFDEF AGG_LINUX }
 ft_lib = 'freetype.so';

{$ENDIF }

{$IFDEF AGG_MACOSX }
 ft_lib = 'libfreetype';

{$ENDIF }

 FT_CURVE_TAG_ON    = 1;
 FT_CURVE_TAG_CONIC = 0;
 FT_CURVE_TAG_CUBIC = 2;

 FT_FACE_FLAG_SCALABLE = 1 shl 0;
 FT_FACE_FLAG_KERNING  = 1 shl 6;

 FT_ENCODING_NONE : FT_Encoding = (#0 ,#0 ,#0 ,#0 );

 FT_LOAD_DEFAULT        = $0000;
 FT_LOAD_NO_HINTING     = $0002;
 FT_LOAD_FORCE_AUTOHINT = $0020;

 FT_RENDER_MODE_NORMAL = 0;
 FT_RENDER_MODE_LIGHT  = FT_RENDER_MODE_NORMAL + 1;
 FT_RENDER_MODE_MONO   = FT_RENDER_MODE_LIGHT + 1;
 FT_RENDER_MODE_LCD    = FT_RENDER_MODE_MONO + 1;
 FT_RENDER_MODE_LCD_V  = FT_RENDER_MODE_LCD + 1;
 FT_RENDER_MODE_MAX    = FT_RENDER_MODE_LCD_V + 1;

 FT_KERNING_DEFAULT  = 0;
 FT_KERNING_UNFITTED = 1;
 FT_KERNING_UNSCALED = 2;

 FT_STYLE_FLAG_ITALIC = 1 shl 0;
 FT_STYLE_FLAG_BOLD   = 1 shl 1;

{ TYPES DEFINITION }
type
 FT_Byte    = byte;
 FT_Short   = smallint;
 FT_UShort  = word;
 FT_Int     = longint;
 FT_UInt    = longword;
 FT_Int32   = longint;
 FT_Long    = longint;
 FT_ULong   = longword;
 FT_Fixed   = longint;
 FT_Pos     = longint;
 FT_Error   = longint;
 FT_F26Dot6 = longint;

 FT_Byte_ptr  = ^FT_Byte;
 FT_Short_ptr = ^FT_Short;

 FT_Render_Mode = FT_Int;

 FT_Library_ptr_ptr = ^FT_Library_ptr;
 FT_Library_ptr = ^FT_Library;
 FT_Library = packed record
  end;

 FT_Subglyph_ptr = ^FT_Subglyph; 
 FT_Subglyph = packed record  // TODO
  end;

 FT_Bitmap_Size = record
  height ,
  width  : FT_Short;

 end;

 AFT_Bitmap_Size    = array [0..1023] of FT_Bitmap_Size;
 FT_Bitmap_Size_ptr = ^AFT_Bitmap_Size;

 FT_Charmap_ptr     = ^FT_Charmap;
 FT_Charmap_ptr_ptr = ^FT_Charmap_ptr;

 FT_Generic_Finalizer = procedure(AnObject : pointer ); cdecl;

 FT_Generic = packed record
  data      : pointer;
  finalizer : FT_Generic_Finalizer;

 end;

 FT_BBox_ptr = ^FT_BBox;
 FT_BBox = packed record
  xMin ,
  yMin ,
  xMax ,
  yMax : FT_Pos;

 end;

 FT_Vector_ptr = ^FT_Vector;
 FT_Vector = packed record
  x ,
  y : FT_Pos;

 end;

 FT_Bitmap_ptr = ^FT_Bitmap;
 FT_Bitmap = packed record
  rows   ,
  width  ,
  pitch  : FT_Int;

  buffer : pointer;

  num_grays    : FT_Short;
  pixel_mode   ,
  palette_mode : char;

  palette : pointer;

 end;

 FT_Outline_ptr = ^FT_Outline;
 FT_Outline = packed record
  n_contours ,
  n_points   : FT_Short;

  points : FT_Vector_ptr;
  tags   : PChar;

  contours : FT_Short_ptr;
  flags    : FT_Int;

 end;

 FT_Glyph_Metrics = packed record
  width  ,
  height ,

  horiBearingX ,
  horiBearingY ,
  horiAdvance  ,
  vertBearingX ,
  vertBearingY ,
  vertAdvance  : FT_Pos;

 end;

 FT_Face_ptr_ptr = ^FT_Face_ptr;
 FT_Face_ptr = ^FT_Face;

 FT_GlyphSlot_ptr = ^FT_GlyphSlot;
 FT_GlyphSlot = packed record
  alibrary : FT_Library_ptr;

  face  : FT_Face_ptr;
  next  : FT_GlyphSlot_ptr;
  flags : FT_UInt;

  generic : FT_Generic;
  metrics : FT_Glyph_Metrics;

  linearHoriAdvance ,
  linearVertAdvance : FT_Fixed;

  advance : FT_Vector;
  format  : longword;
  bitmap  : FT_Bitmap;

  bitmap_left ,
  bitmap_top  : FT_Int;

  outline : FT_Outline;

  num_subglyphs : FT_UInt;
  subglyphs     : FT_SubGlyph_ptr;
  control_data  : pointer;
  control_len   : longint;

  other : pointer;

 end;

 FT_Size_Metrics = record
  x_ppem  ,
  y_ppem  : FT_UShort;
  x_scale ,
  y_scale : FT_Fixed;

  ascender    ,
  descender   ,
  height      ,
  max_advance : FT_Pos;

 end;

 FT_Size_ptr = ^FT_Size;
 FT_Size = record
  face    : FT_Face_ptr;
  generic : FT_Generic;
  metrics : FT_Size_Metrics;
  //internal : FT_Size_Internal;

 end;

 FT_Face = packed record
  num_faces   ,
  face_index  ,
  face_flags  ,
  style_flags ,
  num_glyphs  : FT_Long;
  family_name ,
  style_name  : PChar;

  num_fixed_sizes : FT_Int;
  available_sizes : FT_Bitmap_Size_ptr; // is array

  num_charmaps : FT_Int;
  charmaps     : FT_CharMap_ptr_ptr;    // is array

  generic : FT_Generic;
  bbox    : FT_BBox;

  units_per_EM : FT_UShort;

  ascender  ,
  descender ,
  height    ,

  max_advance_width   ,
  max_advance_height  ,
  underline_position  ,
  underline_thickness : FT_Short;

  glyph   : FT_GlyphSlot_ptr;
  size    : FT_Size_ptr;
  charmap : FT_CharMap_ptr;

 end;

 FT_Charmap = packed record
  face     : FT_Face_ptr;
  encoding : FT_Encoding;

  platform_id ,
  encoding_id : FT_UShort;

 end;

{ GLOBAL PROCEDURES }
 function  FT_CURVE_TAG  (flag : char ) : char;
 function  FT_IS_SCALABLE(face : FT_Face_ptr ) : boolean;
 function  FT_HAS_KERNING(face : FT_Face_ptr ) : boolean;

 function  FT_Init_FreeType(alibrary : FT_Library_ptr ) : FT_Error; cdecl; external ft_lib name 'FT_Init_FreeType';

 function  FT_Done_FreeType(alibrary : FT_Library_ptr ) : FT_Error; cdecl; external ft_lib name 'FT_Done_FreeType';

 function  FT_Attach_File(face : FT_Face_ptr; filepathname : PChar ) : FT_Error; cdecl; external ft_lib name 'FT_Attach_File';

 function  FT_New_Memory_Face(
            library_ : FT_Library_ptr;
            file_base : FT_Byte_ptr;
            file_size ,
            face_index : FT_Long;
            var aface : FT_Face_ptr ) : FT_Error; cdecl; external ft_lib name 'FT_New_Memory_Face';

 function  FT_New_Face(
            library_ : FT_Library_ptr;
            filepathname : PChar;
            face_index : FT_Long;
            var aface : FT_Face_ptr ) : FT_Error; cdecl; external ft_lib name 'FT_New_Face';

 function  FT_Done_Face(face : FT_Face_ptr ) : FT_Error; cdecl; external ft_lib name 'FT_Done_Face';

 function  FT_Select_Charmap(face : FT_Face_ptr; encoding : FT_Encoding ) : FT_Error; cdecl; external ft_lib name 'FT_Select_Charmap';

 function  FT_Get_Char_Index(face : FT_Face_ptr; charcode : FT_ULong ) : FT_UInt; cdecl; external ft_lib name 'FT_Get_Char_Index';

 function  FT_Load_Glyph(
            face : FT_Face_ptr;
            glyph_index : FT_UInt ;
            load_flags : FT_Int32 ) : FT_Error; cdecl; external ft_lib name 'FT_Load_Glyph';

 function  FT_Render_Glyph(slot : FT_GlyphSlot_ptr; render_mode : FT_Render_Mode ) : FT_Error; cdecl; external ft_lib name 'FT_Render_Glyph';

 function  FT_Get_Kerning(
            face : FT_Face_ptr;
            left_glyph ,right_glyph ,kern_mode : FT_UInt;
            akerning : FT_Vector_ptr ) : FT_Error; cdecl; external ft_lib name 'FT_Get_Kerning';

 function  FT_Set_Char_Size(
            face : FT_Face_ptr;
            char_width ,char_height : FT_F26dot6;
            horz_res ,vert_res : FT_UInt) : FT_Error; cdecl; external ft_lib name 'FT_Set_Char_Size';

 function  FT_Set_Pixel_Sizes(
            face : FT_Face_ptr;
            pixel_width ,pixel_height : FT_UInt ) : FT_Error; cdecl; external ft_lib name 'FT_Set_Pixel_Sizes';


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ FT_CURVE_TAG }
function FT_CURVE_TAG(flag : char ) : char;
begin
 result:=char(int8u(flag ) and 3 );

end;

{ FT_IS_SCALABLE }
function FT_IS_SCALABLE(face : FT_Face_ptr ) : boolean;
begin
 result:=boolean(face.face_flags and FT_FACE_FLAG_SCALABLE );

end;

{ FT_HAS_KERNING }
function FT_HAS_KERNING(face : FT_Face_ptr ) : boolean;
begin
 result:=boolean(face.face_flags and FT_FACE_FLAG_KERNING );

end;

END.

