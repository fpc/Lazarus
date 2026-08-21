{ Declarations for Windows meta files

  Infos taken from
  - http://msdn.microsoft.com/en-us/library/cc250370.aspx
  - http://wvware.sourceforge.net/caolan/ora-wmf.html
  - http://www.symantec.com/avcenter/reference/inside.the.windows.meta.file.format.pdf
}

unit lmfWMF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  // WMF Record types
  META_EOF = $0000;
  META_REALIZEPALETTE = $0035;
  META_SETPALENTRIES = $0037;
  META_SETBKMODE = $0102;
  META_SETMAPMODE = $0103;
  META_SETROP2 = $0104;
  META_SETRELABS = $0105;
  META_SETPOLYFILLMODE = $0106;
  META_SETSTRETCHBLTMODE = $0107;
  META_SETTEXTCHAREXTRA = $0108;
  META_RESTOREDC = $0127;
  META_RESIZEPALETTE = $0139;
  META_DIBCREATEPATTERNBRUSH = $0142;
  META_SETLAYOUT = $0149;
  META_SETBKCOLOR = $0201;
  META_SETTEXTCOLOR = $0209;
  META_OFFSETVIEWPORTORG = $0211;
  META_LINETO = $0213;
  META_MOVETO = $0214;
  META_OFFSETCLIPRGN = $0220;
  META_FILLREGION = $0228;
  META_SETMAPPERFLAGS = $0231;
  META_SELECTPALETTE = $0234;
  META_POLYGON = $0324;
  META_POLYLINE = $0325;
  META_SETTEXTJUSTIFICATION = $020A;
  META_SETWINDOWORG = $020B;
  META_SETWINDOWEXT = $020C;
  META_SETVIEWPORTORG = $020D;
  META_SETVIEWPORTEXT = $020E;
  META_OFFSETWINDOWORG = $020F;
  META_SCALEWINDOWEXT = $0410;
  META_SCALEVIEWPORTEXT = $0412;
  META_EXCLUDECLIPRECT = $0415;
  META_INTERSECTCLIPRECT = $0416;
  META_ELLIPSE = $0418;
  META_FLOODFILL = $0419;
  META_FRAMEREGION = $0429;
  META_ANIMATEPALETTE = $0436;
  META_TEXTOUT = $0521;
  META_POLYPOLYGON = $0538;
  META_EXTFLOODFILL = $0548;
  META_RECTANGLE = $041B;
  META_SETPIXEL = $041F;
  META_ROUNDRECT = $061C;
  META_PATBLT = $061D;

  META_SAVEDC = $001E;
  META_PIE = $081A;
  META_STRETCHBLT = $0B23;
  META_ESCAPE = $0626;
  META_INVERTREGION = $012A;
  META_PAINTREGION = $012B;
  META_SELECTCLIPREGION = $012C;
  META_SELECTOBJECT = $012D;
  META_SETTEXTALIGN = $012E;
  META_ARC = $0817;
  META_CHORD = $0830;
  META_BITBLT = $0922;
  META_EXTTEXTOUT = $0a32;
  META_SETDIBTODEV = $0d33;
  META_DIBBITBLT = $0940;
  META_DIBSTRETCHBLT = $0b41;
  META_STRETCHDIB = $0f43;
  META_DELETEOBJECT = $01F0;
  META_CREATEPALETTE = $00F7;
  META_CREATEPATTERNBRUSH = $01F9;
  META_CREATEPENINDIRECT = $02FA;
  META_CREATEFONTINDIRECT = $02FB;
  META_CREATEBRUSHINDIRECT = $02FC;
  META_CREATEREGION = $06FF;

  // WMF Magic number in Placeable Meta Header
  WMF_MAGIC_NUMBER = $9AC6CDD7;

type
  TWMFHeader = packed record
    FileType: Word;        // Type of metafile (0=memory, 1=disk)
    HeaderSize: Word;      // Size of header in WORDS (always 9)
    Version: Word;         // Version of Microsoft Windows used
    FileSize: DWord;       // Total size of the metafile in WORDs
    NumOfObjects: Word;    // Number of objects in the file
    MaxRecordSize: DWord;  // The size of largest record in WORDs
    NumOfParams: Word;     // Not Used (always 0)
  end;
  PWMFHeader = ^TWMFHeader;

 { Placeable Metafiles (file extension .APM) were created by Aldus Corporation
   as a non-standard way of specifying how a metafile is mapped and scaled on an
   output device. Placeable metafiles are quite wide-spread, but not directly
   supported by the Windows API.
   Placeable Metafiles are limited to 64K in length.
   Each placeable metafile begins with a 22-byte header followed by a standard
   metafile. }
  TPlaceableMetaHeader = packed record
    Key: DWord;               // Magic number (always 9AC6CDD7h)
    Handle: Word;             // Metafile HANDLE number (always 0)
    Left: SmallInt;           // Left coordinate in metafile units
    Top: SmallInt;            // Top coordinate in metafile units
    Right: SmallInt;          // Right coordinate in metafile units
    Bottom: SmallInt;         // Bottom coordinate in metafile units
    Inch: Word;               // Number of metafile units per inch
    Reserved: DWord;          // Reserved (always 0)
    Checksum: Word;           // Checksum value for previous 10 WORDs
  end;
  PPlaceableMetaHeader = ^TPlaceableMetaHeader;

  TWMFRecord = packed record
    Size: DWord;              // Total size of the record in WORDs
    Func: Word;               // Function number (defined in WINDOWS.H)
    // Parameters[]: Word;    // Parameter values passed to function - will be read separately
  end;

  TWMFColorRecord = packed record
    ColorRED: Byte;
    ColorGREEN: Byte;
    ColorBLUE: Byte;
    Reserved: Byte;
  end;
  PWMFColorRecord = ^TWMFColorRecord;

  TWMFPenRecord = packed record
    Style: Word;
    Width: Word;
    Ignored1: Word;
    ColorRED: Byte;
    ColorGREEN: Byte;
    ColorBLUE: Byte;
    Ignored2: Byte;
  end;
  PWMFPenRecord = ^TWMFPenRecord;

  TWMFBrushRecord = packed record
    Style: Word;
    ColorRED: Byte;
    ColorGREEN: Byte;
    ColorBLUE: Byte;
    Reserved: Byte;
    // Brush hatch/pattern data of variable length follow
    case integer of
      0: (Hatch: Word);
      // pattern not yet implemented here...
  end;
  PWMFBrushRecord = ^TWMFBrushRecord;

  TWMFFontRecord = packed record
    Height: SmallInt;    // signed int!
    Width: SmallInt;
    Escapement: SmallInt;
    Orientation: SmallInt;
    Weight: SmallInt;
    Italic: Byte;
    UnderLine: Byte;
    Strikeout: Byte;
    CharSet: Byte;
    OutPrecision: Byte;
    ClipPrecision: Byte;
    Quality: Byte;
    PitchAndFamily: byte;
    FaceName: array[0..31] of AnsiChar;
    // FaceName will be handled separately immediately afterwards.
  end;
  PWMFFontRecord = ^TWMFFontRecord;

  TWMFPointRecord = packed record
    Y, X: SmallInt;             // reverse order (y, x) as through-out wmf
  end;
  PWMFPointRecord = ^TWMFPointRecord;

  TWMFPointXYRecord = packed record
    X, Y: SmallInt;             // Regular order (x,y) as needed by polygons
  end;
  PWMFPointXYRecord = ^TWMFPointXYRecord;

  TWMFLineRecord = packed record
    NumPts: word;               // must be 2
    P1: TWMFPointXYRecord;
    P2: TWMFPointXYRecord;
  end;

  TWMFRectRecord = packed record
    Bottom: SmallInt;
    Right: SmallInt;
    Top: SmallInt;
    Left: SmallInt;
  end;
  PWMFRectRecord = ^TWMFRectRecord;

  TWMFRoundRectRecord = packed record
    RY: SmallInt;
    RX: SmallInt;
    Bottom: SmallInt;
    Right: SmallInt;
    Top: SmallInt;
    Left: SmallInt;
  end;
  PWMFRoundRectRecord = ^TWMFRoundRectRecord;

  TWMFArcRecord = packed record
    YEndArc: SmallInt;        // y coordinate of end pt of radial line to arc end point
    XEndArc: SmallInt;        // x coordinate of end pt of radial line to arc end point
    YStartArc: SmallInt;      // y coordinate of end pt of radial line to arc start point
    XStartArc: SmallInt;      // x coordinate of end pt of radial line to arc start point
    Bottom: SmallInt;         // y coordinate of bottom of bounding rectangle
    Right: SmallInt;          // x coordinate of right edge of bounding rectangle
    Top: SmallInt;            // y coordinate of top of bounding rectangle
    Left: SmallInt;           // x coordinate of left of bounding rectangle
  end;
  PWMFArcRecord = ^TWMFArcRecord;

  TWMFSetPolyFillModeRecord = packed record
    PolyFillMode: word;       // 1 (ALTERNATE) or 2 (WINDING)
    Reserved: word;
  end;

  TWMFExtTextOutRecord = packed record
    Y: SmallInt;
    X: SmallInt;
    Len: SmallInt;    // String length
    Options: Word;
    // Rectangle and text follow, plus optional inter-character spacings (Dx)
  end;
  PWMFExtTextOutRecord = ^TWMFExtTextOutRecord;

  {Clipboard metafiles are also based on the standard metafile format, but are
   preceded by an additional 8- or 16-byte header that allows the position of
   the metafile on the Clipboard viewer. If the Clipboard metafile was created
   using a 16-bit version of Windows (Windows and Windows for Workgroups) this
   header will contain 2-byte fields arranged in the following structure. If the
   clipboard metafile was created under a 32-bit Windows environment (Windows NT
   and Windows 95) this header will contain the same fields as the Win16 WMF
   header, but the fields are 32 bytes in length. }
  TWMFClipboard16MetaHeader = packed record
    MappingMode: SmallInt;    // see MM_XXXX constants
    Width: SmallInt;          // Width in units of MappingMode
    Height: SmallInt;         // Height in units of MappingMode
    Handle: Word;             // Handle to the metafile in memory
  end;

  TWMFClipboard32MetaHeader = packed record
    MappingMode: LongInt;     // see MM_XXXX constants
    Width: LongInt;           // Width in units of MappingMode
    Height: LongInt;          // Height in units of MappingMode
    Handle: DWord;            // Handle to the metafile in memory
  end;

  TWMFPaletteColorRecord = packed record
    Values: Byte;                    // NOTE: reverse order!
    ColorBLUE: Byte;
    ColorGREEN: Byte;
    ColorRED: Byte;
  end;
  PWMFPaletteColorRecord = ^TWMFPaletteColorRecord;

  TWMFStretchDIBRecord = packed record
    RasterOperation: DWord;
    ColorUsage: Word;
    SrcHeight: SmallInt;
    SrcWidth: SmallInt;
    SrcY: SmallInt;
    SrcX: SmallInt;
    DestHeight: SmallInt;
    DestWidth: SmallInt;
    DestX: SmallInt;
    DestY: SmallInt;
    // the remainder is handled separately:
    // - TWMFBitmapCoreHeader or TWMFBitmapInfoHeader
    // - optional: Colors
    // - BitmapBuffer
    //
  end;
  PWMFStretchDIBRecord = ^TWMFStretchDIBRecord;

  TWMFDIBStretchBltRecord = packed record
    RasterOperation: DWord;
    SrcHeight: SmallInt;
    SrcWidth: SmallInt;
    SrcY: SmallInt;
    SrcX: SmallInt;
    DestHeight: SmallInt;
    DestWidth: SmallInt;
    DestY: SmallInt;
    DestX: SmallInt;
    // Following: BitmapInfoHeader plus pixel data.
  end;
  PWMFDIBStretchBltRecord = ^TWMFDIBStretchBltRecord;

 function WMF_GetRecordTypeName(ARecordType: Word): String;

 implementation

 function WMF_GetRecordTypeName(ARecordType: Word): String;
begin
  Result := '';
  case ARecordType of
    META_EOF : Result := 'META_EOF';
    META_REALIZEPALETTE: Result := 'META_REALIZEPALETTE';
    META_SETPALENTRIES: Result := 'META_SETPALENTRIES';
    META_SETBKMODE: Result := 'META_SETBKMODE';
    META_SETMAPMODE: Result := 'META_SETMAPMODE';
    META_SETROP2: Result := 'META_SETROP2';
    META_SETRELABS: Result := 'META_SETRELABS';
    META_SETPOLYFILLMODE: Result := 'META_SETPOLYFILLMODE';
    META_SETSTRETCHBLTMODE: Result := 'META_SETSTRETCHBLTMODE';
    META_SETTEXTCHAREXTRA: Result := 'META_SETTEXTCHAREXTRA';
    META_RESTOREDC: Result := 'META_RESTOREDC';
    META_RESIZEPALETTE: Result := 'META_RESIZEPALETTE';
    META_DIBCREATEPATTERNBRUSH: Result := 'META_DIBCREATEPATTERNBRUSH';
    META_SETLAYOUT: Result := 'META_SETLAYOUT';
    META_SETBKCOLOR: Result := 'META_SETBKCOLOR';
    META_SETTEXTCOLOR: Result := 'META_SETTEXTCOLOR';
    META_OFFSETVIEWPORTORG: Result := 'META_OFFSETVIEWPORTORG';
    META_LINETO: Result := 'META_LINETO';
    META_MOVETO: Result := 'META_MOVETO';
    META_OFFSETCLIPRGN: Result := 'META_OFFSETCLIPRGN';
    META_FILLREGION: Result := 'META_FILLREGION';
    META_SETMAPPERFLAGS: Result := 'META_SETMAPPERFLAGS';
    META_SELECTPALETTE: Result := 'META_SELECTPALETTE';
    META_POLYGON: Result := 'META_POLYGON';
    META_POLYLINE: Result := 'META_POLYLINE';
    META_SETTEXTJUSTIFICATION: Result := 'META_SETTEXTJUSTIFICATION';
    META_SETWINDOWORG: Result := 'META_SETWINDOWORG';
    META_SETWINDOWEXT: Result := 'META_SETWINDOWEXT';
    META_SETVIEWPORTORG: Result := 'META_SETVIEWPORTORG';
    META_SETVIEWPORTEXT: Result := 'META_SETVIEWPORTEXT';
    META_OFFSETWINDOWORG: Result := 'META_OFFSETWINDOWORG';
    META_SCALEWINDOWEXT: Result := 'META_SCALEWINDOWEXT';
    META_SCALEVIEWPORTEXT: Result := 'META_SCALEVIEWPORTEXT';
    META_EXCLUDECLIPRECT: Result := 'META_EXCLUDECLIPRECT';
    META_INTERSECTCLIPRECT: Result := 'META_INTERSECTCLIPRECT';
    META_ELLIPSE: Result := 'META_ELLIPSE';
    META_FLOODFILL: Result := 'META_FLOODFILL';
    META_FRAMEREGION: Result := 'META_FRAMEREGION';
    META_ANIMATEPALETTE: Result := 'META_ANIMATEPALETTE';
    META_TEXTOUT: Result := 'META_TEXTOUT';
    META_POLYPOLYGON: Result := 'META_POLYPOLYGON';
    META_EXTFLOODFILL: Result := 'META_EXTFLOODFILL';
    META_RECTANGLE: Result := 'META_RECTANGLE';
    META_SETPIXEL: Result := 'META_SETPIXEL';
    META_ROUNDRECT: Result := 'META_ROUNDRECT';
    META_PATBLT: Result := 'META_PATBLT';
    META_SAVEDC: Result := 'META_SAVEDC';
    META_PIE: Result := 'META_PIE';
    META_STRETCHBLT: Result := 'META_STRETCHBLT';
    META_ESCAPE: Result := 'META_ESCAPE';
    META_INVERTREGION: Result := 'META_INVERTREGION';
    META_PAINTREGION: Result := 'META_PAINTREGION';
    META_SELECTCLIPREGION: Result := 'META_SELECTCLIPREGION';
    META_SELECTOBJECT: Result := 'META_SELECTOBJECT';
    META_SETTEXTALIGN: Result := 'META_SETTEXTALIGN';
    META_ARC: Result := 'META_ARC';
    META_CHORD: Result := 'META_CHORD';
    META_BITBLT: Result := 'META_BITBLT';
    META_EXTTEXTOUT: Result := 'META_EXTTEXTOUT';
    META_SETDIBTODEV: Result := 'META_SETDIBTODEV';
    META_DIBBITBLT: Result := 'META_DIBBITBLT';
    META_DIBSTRETCHBLT: Result := 'META_DIBSTRETCHBLT';
    META_STRETCHDIB: Result := 'META_STRETCHDIB';
    META_DELETEOBJECT: Result := 'META_DELETEOBJECT';
    META_CREATEPALETTE: Result := 'META_CREATEPALETTE';
    META_CREATEPATTERNBRUSH: Result := 'META_CREATEPATTERNBRUSH';
    META_CREATEPENINDIRECT: Result := 'META_CREATEPENINDIRECT';
    META_CREATEFONTINDIRECT: Result := 'META_CREATEFONTINDIRECT';
    META_CREATEBRUSHINDIRECT: Result := 'META_CREATEBRUSHINDIRECT';
    META_CREATEREGION: Result := 'META_CREATEREGION';
    else Result := '(unknown)';
  end;
end;

end.

