(*******************************************************************
 *
 *  TTTypes.pas                                                  1.0
 *
 *    Global types definitions
 *
 *  Copyright 1996, 1997 by
 *  David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 *  This file is part of the FreeType project, and may only be used
 *  modified and distributed under the terms of the FreeType project
 *  license, LICENSE.TXT. By continuing to use, modify or distribute
 *  this file you indicate that you have read the license and
 *  understand and accept it fully.
 *
 ******************************************************************)

unit TTTypes;

interface

type

  TError      = boolean;

{$IFDEF OS2}
  TT_Int    = Longint;
{$ELSE}
  TT_Int    = Integer;
{$ENDIF}

  TT_Long   = longint;
  TT_ULong  = longword;
  TT_Short  = integer;
  TT_UShort = word;

  TT_Fixed  = LongInt;  (* Signed Fixed 16.16 Float *)

  TT_FWord  = SmallInt; (* Distance in FUnits *)
  TT_UFWord = Word;     (* Unsigned Distance  *)

  TT_F2Dot14 = Integer; (* signed fixed float 2.14 used for *)
                        (* unary vectors, with layout :     *)
                        (*                                  *)
                        (*  s : 1  -- sign bit              *)
                        (*  m : 1  -- mantissa bit          *)
                        (*  f : 14 -- unsigned fractional   *)
                        (*                                  *)
                        (*  's:m' is the 2-bit signed int   *)
                        (*  value to which the *positive*   *)
                        (*  fractional part should be       *)
                        (*  added.                          *)
                        (*                                  *)

  TT_F26Dot6 = LongInt; (* 26.6 fixed float, used for pixel coordinates *)

  TT_Pos     = Longint; (* funits or 26.6, depending on context *)

const
  TT_Flag_On_Curve  = $01;  (* Point is On curve *)

type
  (******************************************************)
  (*  a simple unit vector type                         *)
  (*                                                    *)
  TT_UnitVector = record

      x : TT_F2Dot14;
      y : TT_F2Dot14;
  end;

  (******************************************************)
  (*  a simple vector type                              *)
  (*                                                    *)
  TT_Vector = record

      x : TT_Pos;
      y : TT_Pos;
  end;

  (******************************************************)
  (*  a simple 2x2 matrix type                          *)
  (*                                                    *)
  TT_Matrix = record

      xx, xy : TT_Fixed;
      yx, yy : TT_Fixed;
  end;

  (******************************************************)
  (*  a glyph's bounding box                            *)
  (*                                                    *)
  TT_BBox = record

    xMin, yMin : TT_Pos;
    xMax, yMax : TT_Pos;
  end;

  (******************************************************)
  (*  kerning info between two glyphs                   *)
  (*                                                    *)
  TT_KerningInfo = record
    kerning_x, kerning_y: TT_FWord;
    minimum_x, minimum_y: TT_FWord;
    found: boolean;
  end;

  (******************************************************)
  (*  the engine's error condition type - 0 always      *)
  (*  means success.                                    *)
  (*                                                    *)
  TT_Error = TT_Int;

  TT_Points_Table = array[0..99] of TT_Vector;
  TT_Points       = ^TT_Points_Table;

  TT_Coordinates  = array[0..99] of TT_Pos;
  TT_PCoordinates = ^TT_Coordinates;

  TT_TouchTable   = array[0..9] of byte;
  TT_PTouchTable  = ^TT_TouchTable;

  TT_ConStarts  = array[0..9] of word;
  TT_PConStarts = ^TT_ConStarts;

  (******************************************************)
  (*  glyph outline description                         *)
  (*                                                    *)
  TT_Outline = record

      n_points   : integer;
      n_contours : integer;

      points     : TT_Points;       (* array of point coordinates *)
      flags      : TT_PTouchTable;  (* array of point flags       *)
      conEnds    : TT_PConStarts;    (* array of contours ends    *)

      owner     : Boolean;          (* this flag is set when the outline *)
                                    (* owns the arrays it uses.          *)

      high_precision : Boolean;
      second_pass    : Boolean;
      dropout_mode   : Byte;
  end;

  (******************************************************)
  (*  glyph metrics structure                           *)
  (*                                                    *)
  TT_Glyph_Metrics = record

      bbox     : TT_BBox;
      bearingX : TT_Pos;
      bearingY : TT_Pos;
      advance  : TT_Pos;
  end;

  (******************************************************)
  (*  big glyph metrics structure                       *)
  (*                                                    *)
  TT_Big_Glyph_Metrics = record

      bbox         : TT_BBox;
      horiBearingX : TT_Pos;
      horiBearingY : TT_Pos;
      horiAdvance  : TT_Pos;
      vertBearingX : TT_Pos;
      vertBearingY : TT_Pos;
      vertAdvance  : TT_Pos;
  end;

  TDirectRenderingFunction = procedure( x,y,tx: integer;
                                     data: pointer ) of object;


  (******************************************************)
  (*  instance metrics. used to return information to   *)
  (*  clients regarding an instance's important state   *)
  (*                                                    *)
  TT_Instance_Metrics = record

      pointsize    : integer;

      x_ppem       : integer;
      y_ppem       : integer;

      x_scale      : TT_Fixed;
      y_scale      : TT_Fixed;

      x_resolution : integer;
      y_resolution : integer;
  end;

const
  TT_Flow_Down = -1;
  TT_Flow_Up   = +1;

type

  (******************************************************)
  (*  a record used to describe a bitmap or pixmap to   *)
  (*  the rasterizer.                                   *)
  (*                                                    *)
  TT_Raster_Map  = record

      Rows   : TT_Int;      (* rows number of the bitmap    *)
      Cols   : TT_Int;      (* columns (bytes) per row      *)
      Width  : TT_Int;      (* pixels per row               *)
      Flow   : TT_Int;      (* bit/pixmap's flow            *)
      Buffer : pointer;      (* bit/pixmap data              *)
      Size   : longint;      (* bit/pixmap data size (bytes) *)
  end;

  (******************************************************)
  (*  The TrueType font header table structure          *)
  (*                                                    *)
  TT_Header = record

      table_version   : TT_Fixed;
      font_revision   : TT_Fixed;

      checksum_adjust : TT_Long;
      magic_number    : TT_Long;

      flags           : TT_UShort;
      units_per_EM    : TT_UShort;

      created         : array[0..1] of TT_Long;
      modified        : array[0..1] of TT_Long;

      xMin, yMin      : TT_FWord;
      xMax, yMax      : TT_FWord;

      mac_style       : TT_UShort;
      lowest_rec_PPEM : TT_UShort;
      font_direction  : TT_Short;

      index_to_loc_format : TT_Short;
      glyph_data_format   : TT_Short;
  end;

  (******************************************************)
  (*  The TrueType horizontal header table structure    *)
  (*                                                    *)
  TT_Horizontal_Header = record

      version   : TT_Fixed;
      ascender  : TT_FWord;
      descender : TT_FWord;
      line_gap  : TT_FWord;

      advance_Width_Max      : TT_UShort;
      min_left_side_bearing  : TT_Short;
      min_right_side_bearing : TT_Short;
      xMax_extent            : TT_Short;
      caret_slope_rise       : TT_Short;
      caret_slope_run        : TT_Short;

      reserved  : array[0..4] of TT_SHort;

      metric_data_format     : TT_Short;
      number_of_HMetrics     : TT_UShort;

      (* the following are not part of the header in the file *)

      short_metrics : Pointer;
      long_metrics  : Pointer;
  end;

  (******************************************************)
  (*  The TrueType vertical header table structure      *)
  (*                                                    *)
  TT_Vertical_Header = record

      version   : TT_Fixed;
      ascender  : TT_FWord;
      descender : TT_FWord;
      line_gap  : TT_FWord;

      advance_Height_Max      : TT_UShort;
      min_top_side_bearing    : TT_Short;
      min_bottom_side_bearing : TT_Short;
      yMax_extent             : TT_Short;
      caret_slope_rise        : TT_Short;
      caret_slope_run         : TT_Short;

      reserved  : array[0..4] of TT_SHort;

      metric_data_format     : TT_Short;
      number_of_VMetrics     : TT_UShort;

      (* the following are not part of the header in the file *)

      short_metrics : Pointer;
      long_metrics  : Pointer;
  end;

  (******************************************************)
  (*  The TrueType OS/2 table structure                 *)
  (*                                                    *)
  TT_OS2 = record
      version             : TT_UShort;   (* $0001 *)
      xAvgCharWidth       : TT_Short;
      usWeightClass       : TT_UShort;
      usWidthClass        : TT_UShort;
      fsType              : TT_Short;
      ySubscriptXSize     : TT_Short;
      ySubscriptYSize     : TT_Short;
      ySubScriptXOffset   : TT_Short;
      ySubscriptYOffset   : TT_Short;
      ySuperscriptXSize   : TT_Short;
      ySuperscriptYSize   : TT_Short;
      ySuperscriptXOffset : TT_Short;
      ySuperscriptYOffset : TT_Short;
      yStrikeoutSize      : TT_Short;
      yStrikeoutPosition  : TT_Short;
      sFamilyClass        : TT_Short;
      panose              : array[0..9] of Byte;
      ulUnicodeRange1     : TT_ULong;   (* bits  0-31  *)
      ulUnicodeRange2     : TT_ULong;   (* bits 32-63  *)
      ulUnicodeRange3     : TT_ULong;   (* bits 64-95  *)
      ulUnicodeRange4     : TT_ULong;   (* bits 96-127 *)
      achVendID           : array[0..3] of Byte;
      fsSelection         : TT_UShort;
      usFirstCharIndex    : TT_UShort;
      usLastCharIndex     : TT_UShort;
      sTypoAscender       : TT_Short;
      sTypoDescender      : TT_Short;
      sTypoLineGap        : TT_Short;
      usWinAscent         : TT_UShort;
      usWinDescent        : TT_UShort;

      (* only version 1 tables *)
      ulCodePageRange1    : TT_ULong;
      ulCodePageRange2    : TT_ULong;

      (* only version 2 tables *)
      sxHeight            : TT_Short;
      sCapHeight          : TT_Short;
      usDefaultChar       : TT_UShort;
      usBreakChar         : TT_UShort;
      usMaxContext        : TT_UShort;
  end;

  (******************************************************)
  (*  The TrueType Postscript table structure           *)
  (*                                                    *)
  TT_Postscript = record

      FormatType         : TT_Fixed;
      italicAngle        : TT_Fixed;
      underlinePosition  : TT_Short;
      underlineThickness : TT_Short;
      isFixedPitch       : TT_ULong;
      minMemType42       : TT_ULong;
      maxMemType42       : TT_ULong;
      minMemType1        : TT_ULong;
      maxMemType1        : TT_ULong;
  end;

  (******************************************************)
  (*  face properties. use to report important face     *)
  (*  data to clients                                   *)
  (*                                                    *)
  TT_Face_Properties = record

      num_glyphs   : integer;
      max_points   : integer;
      max_contours : integer;
      max_faces    : integer;

      header       : ^TT_Header;
      horizontal   : ^TT_Horizontal_Header;
      vertical     : ^TT_Vertical_Header;
      os2          : ^TT_OS2;
      postscript   : ^TT_Postscript;
  end;

  (******************************************************)
  (*  Objects handle types                              *)
  (*                                                    *)
  TT_Stream   = record z : Pointer; end;
  TT_Face     = record z : Pointer; end;
  TT_Instance = record z : Pointer; end;
  TT_Glyph    = record z : Pointer; end;
  TT_CharMap  = record z : Pointer; end;

  TT_Gray_Palette = packed array[0..4] of byte;
  PTT_Gray_Palette = ^TT_Gray_Palette;

  (******************************************************************)
  (*                                                                *)
  (*                         ERROR CODES                            *)
  (*                                                                *)
  (******************************************************************)

const
  (* ------------------- Success is always 0 ---------------------- *)

  TT_Err_Ok                      =  0;

  (* -------------------------------------------------------------- *)

  TT_Err_Invalid_Face_Handle     = $0001;
  TT_Err_Invalid_Instance_Handle = $0002;
  TT_Err_Invalid_Glyph_Handle    = $0003;
  TT_Err_Invalid_CharMap_Handle  = $0004;
  TT_Err_Invalid_Result_Address  = $0005;
  TT_Err_Invalid_Glyph_Index     = $0006;
  TT_Err_Invalid_Argument        = $0007;
  TT_Err_Could_Not_Open_File     = $0008;
  TT_Err_File_Is_Not_Collection  = $0009;

  TT_Err_Table_Missing           = $000A;
  TT_Err_Invalid_Horiz_Metrics   = $000B;
  TT_Err_Invalid_Vert_Metrics    = $000B;
  TT_Err_Invalid_CharMap_Format  = $000C;

  TT_Err_Invalid_File_Format     = $0010;
  TT_Err_File_Error              = $0011;

  TT_Err_Invalid_Engine          = $0020;
  TT_Err_Too_Many_Extensions     = $0021;
  TT_Err_Extensions_Unsupported  = $0022;
  TT_Err_Invalid_Extension_Id    = $0023;

  TT_Err_No_Vertical_Data        = $0030;

  TT_Err_Max_Profile_Missing     = $0080;
  TT_Err_Header_Table_Missing    = $0081;
  TT_Err_Horiz_Header_Missing    = $0082;
  TT_Err_Locations_Missing       = $0083;
  TT_Err_Name_Table_Missing      = $0084;
  TT_Err_CMap_Table_Missing      = $0085;
  TT_Err_Hmtx_Table_Missing      = $0086;
  TT_Err_OS2_Table_Missing       = $0087;
  TT_Err_Post_Table_Missing      = $0088;

  (* -------------------------------------------------------------- *)

  TT_Err_Out_Of_Memory           = $0100;

  (* -------------------------------------------------------------- *)

  TT_Err_Invalid_File_Offset     = $0200;
  TT_Err_Invalid_File_Read       = $0201;
  TT_Err_Invalid_Frame_Access    = $0202;

  (* -------------------------------------------------------------- *)

  TT_Err_Too_Many_Points         = $0300;
  TT_Err_Too_Many_Contours       = $0301;
  TT_Err_Invalid_Composite       = $0302;
  TT_Err_Too_Many_Ins            = $0303;

  (* -------------------------------------------------------------- *)

  TT_Err_Invalid_Opcode          = $0400;
  TT_Err_Too_Few_Arguments       = $0401;
  TT_Err_Stack_Overflow          = $0402;
  TT_Err_Code_Overflow           = $0403;
  TT_Err_Bad_Argument            = $0404;
  TT_Err_Divide_By_Zero          = $0405;
  TT_Err_Storage_Overflow        = $0406;
  TT_Err_Cvt_Overflow            = $0407;
  TT_Err_Invalid_Reference       = $0408;
  TT_Err_Invalid_Distance        = $0409;
  TT_Err_Interpolate_Twilight    = $040A;
  TT_Err_Debug_Opcode            = $040B;
  TT_Err_ENDF_In_Exec_Stream     = $040C;
  TT_Err_Out_Of_CodeRanges       = $040D;
  TT_Err_Nested_DEFs             = $040E;
  TT_Err_Invalid_CodeRange       = $040F;
  TT_Err_Invalid_Displacement    = $0410;
  TT_Err_Execution_Too_Long      = $0411;
  TT_Err_Too_Many_FuncDefs       = $0412;
  TT_Err_Too_Many_InsDefs        = $0413;

  TT_Err_Nested_Frame_Access     = $0500;
  TT_Err_Invalid_Cache_List      = $0501;
  TT_Err_Could_Not_Find_Context  = $0502;
  TT_Err_UNlisted_Object         = $0503;

  TT_Err_Raster_Pool_Overflow    = $0600;
  TT_Err_Raster_Negative_Height  = $0601;
  TT_Err_Invalid_Value           = $0602;
  TT_Err_Raster_Not_Initialised  = $0603;

type
  TFreeTypeCustomRasterizer = class
    function Render_Glyph( var glyph  : TT_Outline;
                           var target : TT_Raster_Map ) : TError; virtual; abstract;

    (* Render one glyph in the target bitmap (1-bit per pixel)       *)

    function Render_Gray_Glyph( var glyph   : TT_Outline;
                                var target  : TT_Raster_Map;
                                palette : PTT_Gray_Palette ) : TError; virtual; abstract;

    (* Render one gray-level glyph in the target pixmap              *)
    (* palette points to an array of 5 colors used for the rendering *)
    (* use nil to reuse the last palette. Default is VGA graylevels  *)

    function Render_Gray_Glyph_HQ( var glyph   : TT_Outline;
                                var target  : TT_Raster_Map ) : TError; virtual; abstract;

    function Render_Directly_Gray_Glyph( var glyph   : TT_Outline;
                                x,y,tx,ty: integer;
                                OnRender: TDirectRenderingFunction;
                                palette : PTT_Gray_Palette) : TError; virtual; abstract;

    function Render_Directly_Gray_Glyph_HQ( var glyph   : TT_Outline;
                                x,y,tx,ty: integer;
                                OnRender: TDirectRenderingFunction) : TError; virtual; abstract;

    procedure Set_Raster_Palette(const palette: TT_Gray_Palette); virtual; abstract;
  end;

type
  (*********************** SIMPLE PRIMITIVE TYPES *******************)

  (* BYTE is already defined in Pascal       *)
  (* They are equivalent to C unsigned chars *)

  UShort   = Word;          (* unsigned short integer, must be on 16 bits *)
  Short    = Smallint;       (* signed short integer,   must be on 16 bits *)

  Long     = Longint;
  ULong    = LongWord;     (* unsigned long integer, must be on 32 bits *)


{$IFDEF USE32}
  Int = LongInt;      (* the 'int' type is used for loop counters and  *)
{$ELSE}               (* indexes.. Their size must be the one a given  *)
  Int = Integer;      (* system handles most easily ( 16 bits on Turbo *)
{$ENDIF}              (* and 32 on Virtual Pascals )                   *)

  TByteArray = array[0..1000] of Byte;
  PByte      = ^TByteArray;

  TShortArray = array[0..1000] of Short;
  PShort      = ^TShortArray;

  TUShortArray = array[0..1000] of UShort;
  PUShort      = ^TUShortArray;

  TStorage    = array[0..16000] of Long;
  PStorage    = ^TStorage;
  PLong       = PStorage;
  PULong      = PStorage;

  (***************** FreeType Internal Types *****************************)

  TCoordinates = array[0..1023] of TT_F26Dot6;
  PCoordinates = ^TCoordinates;

  PTouchTable  = PByte;

  TVecRecord = record
                 n     : Int;           (* number of points            *)
                 org_x : PCoordinates;  (* original coordinates arrays *)
                 org_y : PCoordinates;
                 cur_x : PCoordinates;  (* current coordinates arrays  *)
                 cur_y : PCoordinates;
                 touch : PTouchTable;   (* touch flags array           *)
               end;
  (* This type is used to describe each point zone in the interpreter  *)

const
  Success = False;
  Failure = True;


implementation

end.
