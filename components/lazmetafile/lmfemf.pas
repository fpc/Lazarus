{ Declarations for Windows enhanced metafiles

  Infos taken from
  - https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-emf
}

unit lmfEMF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  // EMF record types
  EMR_HEADER = $00000001;
  EMR_POLYBEZIER = $00000002;
  EMR_POLYGON = $00000003;
  EMR_POLYLINE = $00000004;
  EMR_POLYBEZIERTO = $00000005;
  EMR_POLYLINETO = $00000006;
  EMR_POLYPOLYLINE = $00000007;
  EMR_POLYPOLYGON = $00000008;
  EMR_SETWINDOWEXTEX = $00000009;
  EMR_SETWINDOWORGEX = $000000A;
  EMR_SETVIEWPORTEXTEX = $0000000B;
  EMR_SETVIEWPORTORGEX = $0000000C;
  EMR_SETBRUSHORGEX = $0000000D;
  EMR_EOF = $0000000E;
  EMR_SETPIXELV = $0000000F;
  EMR_SETMAPPERFLAGS = $00000010;
  EMR_SETMAPMODE = $00000011;
  EMR_SETBKMODE = $00000012;
  EMR_SETPOLYFILLMODE = $00000013;
  EMR_SETROP2 = $00000014;
  EMR_SETSTRETCHBLTMODE = $00000015;
  EMR_SETTEXTALIGN = $00000016;
  EMR_SETCOLORADJUSTMENT = $00000017;
  EMR_SETTEXTCOLOR = $00000018;
  EMR_SETBKCOLOR = $00000019;
  EMR_OFFSETCLIPRGN = $0000001A;
  EMR_MOVETOEX = $0000001B;
  EMR_SETMETARGN = $0000001C;
  EMR_EXCLUDECLIPRECT = $0000001D;
  EMR_INTERSECTCLIPRECT = $0000001E;
  EMR_SCALEVIEWPORTEXTEX = $0000001F;
  EMR_SCALEWINDOWEXTEX = $00000020;
  EMR_SAVEDC = $00000021;
  EMR_RESTOREDC = $00000022;
  EMR_SETWORLDTRANSFORM = $00000023;
  EMR_MODIFYWORLDTRANSFORM = $00000024;
  EMR_SELECTOBJECT = $00000025;
  EMR_CREATEPEN = $00000026;
  EMR_CREATEBRUSHINDIRECT = $00000027;
  EMR_DELETEOBJECT = $00000028;
  EMR_ANGLEARC = $00000029;
  EMR_ELLIPSE = $0000002A;
  EMR_RECTANGLE = $0000002B;
  EMR_ROUNDRECT = $0000002C;
  EMR_ARC = $0000002D;
  EMR_CHORD = $0000002E;
  EMR_PIE = $0000002F;
  EMR_SELECTPALETTE = $00000030;
  EMR_CREATEPALETTE = $00000031;
  EMR_SETPALETTEENTRIES = $00000032;
  EMR_RESIZEPALETTE = $00000033;
  EMR_REALIZEPALETTE = $00000034;
  EMR_EXTFLOODFILL = $00000035;
  EMR_LINETO = $00000036;
  EMR_ARCTO = $00000037;
  EMR_POLYDRAW = $00000038;
  EMR_SETARCDIRECTION = $00000039;
  EMR_SETMITERLIMIT = $0000003A;
  EMR_BEGINPATH = $0000003B;
  EMR_ENDPATH = $0000003C;
  EMR_CLOSEFIGURE = $0000003D;
  EMR_FILLPATH = $0000003E;
  EMR_STROKEANDFILLPATH = $0000003F;
  EMR_STROKEPATH = $00000040;
  EMR_FLATTENPATH = $00000041;
  EMR_WIDENPATH = $00000042;
  EMR_SELECTCLIPPATH = $00000043;
  EMR_ABORTPATH = $00000044;
  EMR_COMMENT = $00000046;
  EMR_FILLRGN = $00000047;
  EMR_FRAMERGN = $00000048;
  EMR_INVERTRGN = $00000049;
  EMR_PAINTRGN = $0000004A;
  EMR_EXTSELECTCLIPRGN = $0000004B;
  EMR_BITBLT = $0000004C;
  EMR_STRETCHBLT = $0000004D;
  EMR_MASKBLT = $0000004E;
  EMR_PLGBLT = $0000004F;
  EMR_SETDIBITSTODEVICE = $00000050;
  EMR_STRETCHDIBITS = $00000051;
  EMR_EXTCREATEFONTINDIRECTW = $00000052;
  EMR_EXTTEXTOUTA = $00000053;
  EMR_EXTTEXTOUTW = $00000054;
  EMR_POLYBEZIER16 = $00000055;
  EMR_POLYGON16 = $00000056;
  EMR_POLYLINE16 = $00000057;
  EMR_POLYBEZIERTO16 = $00000058;
  EMR_POLYLINETO16 = $00000059;
  EMR_POLYPOLYLINE16 = $0000005A;
  EMR_POLYPOLYGON16 = $0000005B;
  EMR_POLYDRAW16 = $0000005C;
  EMR_CREATEMONOBRUSH = $0000005D;
  EMR_CREATEDIBPATTERNBRUSHPT = $0000005E;
  EMR_EXTCREATEPEN = $0000005F;
  EMR_POLYTEXTOUTA = $00000060;
  EMR_POLYTEXTOUTW = $00000061;
  EMR_SETICMMODE = $00000062;
  EMR_CREATECOLORSPACE = $00000063;
  EMR_SETCOLORSPACE = $00000064;
  EMR_DELETECOLORSPACE = $00000065;
  EMR_GLSRECORD = $00000066;
  EMR_GLSBOUNDEDRECORD = $00000067;
  EMR_PIXELFORMAT = $00000068;
  EMR_DRAWESCAPE = $00000069;
  EMR_EXTESCAPE = $0000006A;
  EMR_SMALLTEXTOUT = $0000006C;
  EMR_FORCEUFIMAPPING = $0000006D;
  EMR_NAMEDESCAPE = $0000006E;
  EMR_COLORCORRECTPALETTE = $0000006F;
  EMR_SETICMPROFILEA = $00000070;
  EMR_SETICMPROFILEW = $00000071;
  EMR_ALPHABLEND = $00000072;
  EMR_SETLAYOUT = $00000073;
  EMR_TRANSPARENTBLT = $00000074;
  EMR_GRADIENTFILL = $00000076;
  EMR_SETLINKEDUFIS = $00000077;
  EMR_SETTEXTJUSTIFICATION = $00000078;
  EMR_COLORMATCHTOTARGETW = $00000079;
  EMR_CREATECOLORSPACEW = $0000007A;

  AD_COUNTERCLOCKWISE = 1;
  AD_CLOCKWISE = 2;

  // ExtTextOut options
  ETO_TRANSPARENT = $00000000;
  ETO_OPAQUE = $00000002;
  ETO_CLIPPED = $00000004;
  ETO_GLYPH_INDEX = $00000010;
  ETO_RTLREADING = $00000080;
  ETO_NO_RECT = $00000100;
  ETO_SMALL_CHARS = $00000200;
  ETO_NUMERICSLOCAL = $00000400;
  ETO_NUMERICSLATIN = $00000800;
  ETO_IGNORELANGUAGE = $00001000;
  ETO_PDY = $00002000;
  ETO_REVERSE_INDEX_MAP = $00010000;

type
  TEnhancedMetaHeader = packed record      // 80 bytes
    RecordType: DWord;        // Record type, must be 00000001h for EMF
    RecordSize: DWord;        // Size of the record in bytes
    BoundsLeft: LongInt;      // Left inclusive bounds, logical metafile units
    BoundsTop: LongInt;       // Top inclusive bounds, logical metafile units
    BoundsRight: LongInt;     // Right inclusive bounds, logical metafile units
    BoundsBottom: LongInt;    // Bottom inclusive bounds, logical metafile units
    FrameLeft: LongInt;       // Left side of inclusive picture frame, in 0.01 mm
    FrameTop: LongInt;        // Top side of inclusive picture frame, in 0.01 mm
    FrameRight: LongInt;      // Right side of inclusive picture frame, in 0.01 mm
    FrameBottom: LongInt;     // Bottom side of inclusive picture frame, in 0.01 mm
    Signature: DWord;         // Signature ID (always $464D4520)
    Version: DWord;           // Version of the metafile, always $00010000
    Size: DWord;              // Size of the metafile in bytes
    NumOfRecords: DWord;      // Number of records in the metafile
    NumOfHandles: Word;       // Number of handles in the handle table
    Reserved: Word;           // Not used (always 0)
    SizeOfDescrip: DWord;     // Length of description string (16-bit chars) in WORDs, incl zero
    OffsOfDescrip: DWord;     // Offset of description string in metafile (from beginning)
    NumPalEntries: DWord;     // Number of color palette entries
    WidthDevPixels: LongInt;  // Width of display device in pixels
    HeightDevPixels: LongInt; // Height of display device in pixels
    WidthDevMM: LongInt;      // Width of display device in millimeters (e.g. printer page size)
    HeightDevMM: LongInt;     // Height of display device in millimeters
  end;
  PEnhancedMetaHeader = ^TEnhancedMetaHeader;

  TEMFRecord = packed record
    Func: DWord;              // Function number (defined in WINDOWS.H)
    Size: DWord;              // Total size of the record in BYTEs (including Func and Size fields)
    // Parameters[]: DWord;   // Parameter values passed to function - will be read separately
  end;
  PEMFRecord = ^TEMFRecord;

  TEMFColorRecord = packed record
    ColorRED: Byte;
    ColorGREEN: Byte;
    ColorBLUE: Byte;
    Reserved: Byte;
  end;

  TEMFPointLRecord = packed record
    x, y: LongInt;
  end;
  PEMFPointLRecord = ^TEMFPointLRecord;

  TEMFPointSRecord = packed record
    x, y: SmallInt;
  end;
  PEMFPointSRecord = ^TEMFPointSRecord;

  TEMFRectLRecord = packed record
    Left: LongInt;
    Top: LongInt;
    Right: LongInt;
    Bottom: LongInt;
  end;
  PEMFRectLRecord = ^TEMFRectLRecord;

  TEMFRoundRectRecord = packed record
    Left: LongInt;
    Top: LongInt;
    Right: LongInt;
    Bottom: LongInt;
    CornerWidth: DWord;
    CornerHeight: DWord;
  end;
  PEMFRoundRectRecord = ^TEMFRoundRectRecord;

  TEMFEndOfFileRecord = packed record
    NumPalEntries: DWord;     // Number of items in optional palette
    OffPalEntries: DWord;     // Offset to optional palette (from beginning of this full record)
    // Following: Color palette data + offset back to begin of this record (= Size)
  end;

  TEMFArcRecord = packed record
    Box: TEMFRectLRecord;       // Rectangle enclosing the full ellipse
    StartPt: TEMFPointLRecord;  // Radial point defining start of arc
    EndPt: TEMFPointLRecord;    // Radial point defining end of arc
  end;
  PEMFArcRecord = ^TEMFArcRecord;

  TEMFBitBLTRecord = packed record
    BoundsLeft: LongInt;
    BoundsTop: LongInt;
    BoundRight: LongInt;
    BoundsBottom: LongInt;
    xDest: LongInt;
    yDest: LongInt;
    cxDest: LongInt;
    cyDest: LongInt;
    BitBltRasterOp: DWord;
    xSrc: LongInt;
    ySrc: LongInt;
    Transform: array[0..5] of Single;
    BkColorSrc: DWord;
    UsageSrc: DWord;
    offBmiSrc: DWord;
    cbBmiSrc: DWord;
    offBitsSrc: DWord;
    cbBitsSrc: DWord;
    // Following: bitmap buffer
  end;
  PEMFBitBLTRecord = ^TEMFBitBLTRecord;

  TEMFStretchBLTRecord = packed record
    BoundsLeft: LongInt;
    BoundsTop: LongInt;
    BoundsRight: LongInt;
    BoundsBottom: LongInt;
    xDest: LongInt;
    yDest: LongInt;
    cxDest: LongInt;
    cyDest: LongInt;
    BitBltRasterOp: DWord;
    xSrc: LongInt;
    ySrc: LongInt;
    Transform: array[0..5] of Single;
    BkColorSrc: DWord;
    UsageSrc: DWord;
    offBmiSrc: DWord;
    cbBmiSrc: DWord;
    offBitsSrc: DWord;
    cbBitsSrc: DWord;
    cxSrc: LongInt;
    cySrc: LongInt;
    // Following bitmap buffer (info header + pixel bits)
  end;


  TEMFStretchDIBitsRecord = packed record
    BoundsLeft: LongInt;
    BoundsTop: LongInt;
    BoundRight: LongInt;
    BoundsBottom: LongInt;
    xDest: LongInt;
    yDest: LongInt;
    xSrc: LongInt;
    ySrc: LongInt;
    cxSrc: LongInt;
    cySrc: LongInt;
    offBmiSrc: DWord;
    cbBmiSrc: DWord;
    offBitsSrc: DWord;
    cbBitsSrc: DWord;
    UsageSrc: DWord;
    BitBltRasterOp: DWord;
    cxDest: DWord;
    cyDest: DWord;
    // Following: Bitmap buffer
  end;
  PEMFStretchDIBitsRecord = ^TEMFStretchDIBitsRecord;

  TEMFBrushRecord = packed record
    BrushStyle: DWord;        // BS_SOLID, BS_NULL, BS_HATCHED
    ColorRED: Byte;
    ColorGREEN: Byte;
    ColorBLUE: Byte;
    Reserved: Byte;
    BrushHatch: DWord;        // HS_SOLIDCLR, HS_DITHEREDCLR, HS_SOLIDTEXTCLR, HS_DITHEREDTEXTCLR, HS_SOLIDBKCLR, HS_DITHEREDBKCLR
  end;
  PEMFBrushRecord = ^TEMFBrushRecord;

  TEMFLogFontRecord = packed record
    Height: LongInt;
    Width: LongInt;
    Escapement: LongInt;
    Orientation: LongInt;
    Weight: LongInt;
    Italic: Byte;
    Underline: Byte;
    Strikeout: Byte;
    CharSet: Byte;
    OutPrecision: Byte;
    ClipPrecision: Byte;
    Quality: Byte;
    PitchAndFamily: Byte;
    FaceName: array[0..31] of WideChar;
  end;
  PEMFLogFontRecord = ^TEMFLogFontRecord;

  TEMFLogFontExRecord = packed record
    LogFont: TEMFLogFontRecord;
    FullName: array[0..63] of WideChar;
    Style: array[0..31] of WideChar;
    Script: array[0..31] of WideChar;
  end;

  // Designvector is part of the LogFontExDv record used by EMR_EXTCREATEFONTINDIRECTW
  TEMFDesignVectorRecord = packed record
    Signature: DWord;  // must be $08007664
    NumAxes: DWord;    // 0..16
    // Following: array of 32-bit signed integers
  end;

  TEMFLogPenRecord = packed record
    PenStyle: DWord;
    Width: DWord;
    Reserved: DWord;
    ColorRED: Byte;
    ColorGREEN: Byte;
    ColorBLUE: Byte;
    ColorReserved: Byte;
  end;
  PEMFLogPenRecord = ^TEMFLogPenRecord;

  TEMFLogPenExRecord = packed record
    PenStyle: DWord;
    Width: DWord;
    BrushStyle: DWord;
    ColorRED: Byte;
    ColorGREEN: Byte;
    ColorBLUE: Byte;
    ColorReserved: Byte;
    BrushHatch: DWord;
    NumStyleEntries: DWord;
    // Following: array of DWords with dash and gap lengths
  end;
  PEMFLogPenExRecord = ^TEMFLogPenExRecord;

  TEMFExtCreatePenRecord = packed record
    offBmi: DWord;    // Offset to bitmap header
    cbBmi: DWord;     // Size of bitmap header
    offBits: DWord;   // Offset to bitmap data bits
    cbBits: DWord;    // Size of bitmap data bits
    Pen: TEMFLogPenExRecord;
    // Follwing: Pen pattern data, bitmap header and data
  end;
  PEMFExtCreatePenRecord = ^TEMFExtCreatePenRecord;

  TEMFExtTextOutRecord = packed record
    BoundsLeft: LongInt;
    BoundsTop: LongInt;
    BoundsRight: LongInt;
    BoundsBottom: LongInt;
    GraphicsMode: DWord;  // 1 = GM_COMPATIBLE, 2 = GM_ADVANCED
    XScale: Single;
    YScale: Single;
    // Following: an EMFTextRecord (see next)
  end;
  PEMFExtTextOutRecord = ^TEMFExtTextOutRecord;

  TEMFTextRecord = packed record
    RefPtX: DWord;
    RefPtY: DWord;
    NumChars: DWord;
    OffsToString: DWord;    // Offset to string (ansichar or widechar)
    Options: DWord;
    RectLeft: DWord;
    RectTop: DWord;
    RectRight: DWord;
    RectBottom: DWord;
    OffsToDx: DWord;        // Offset to inter-character spacings
    // Following: string buffer and Dx buffer
  end;
  PEMFTextRecord = ^TEMFTextRecord;

  TEMFSmallTextOutRecord = packed record
    RefPtX: DWord;
    RefPtY: DWord;
    NumChars: DWord;
    Options: DWord;
    GraphicsMode: DWord;
    XScale: Single;
    YScale: Single;
    RectLeft: DWord;  // but only if ETO_NO_RECT is not included in Options !
    RectTop: DWord;
    RectRight: DWord;
    RectBottom: DWord;
    { following: string in 8-bit compressed widechars }
  end;
  PEMFSmallTextOutRecord = ^TEMFSmallTextOutRecord;

  TEMFPolyLineRecord = packed record
    BoundsLeft: LongInt;
    BoundsTop: LongInt;
    BoundsRight: LongInt;
    BoundsBottom: LongInt;
    NumPts: DWord;
    // Following: array of TEMFPointLRecords of polyline points
  end;
  PEMFPolyLineRecord = ^TEMFPolyLineRecord;

  TEMFGradientFillRecord = packed record
    BoundsLeft: LongInt;
    BoundsTop: LongInt;
    BoundsRight: LongInt;
    BoundsBottom: LongInt;
    nVer: DWord;      // number of vertices
    nTri: DWord;      // number of triangles/rectangles
    FillMode: DWord;  // 0=GRADIENT_FILL_RECT_H, 1=GRADIENT_FILL_RECT_V, 2=GRADIENT_FILL_TRIANGLE
    // Following: array of vertices and array of vertexindices
  end;
  PEMFGradientFillRecord = ^TEMFGradientFillRecord;

  TEMFExtFloodFillRecord = packed record
    StartPt: TEMFPointLRecord;
    Color: TEMFColorRecord;
    Mode: DWord;
  end;
  PEMFExtFloodFillRecord = ^TEMFExtFloodFillRecord;

function EMF_GetRecordTypeName(ARecordType: DWord): String;


implementation

function EMF_GetRecordTypeName(ARecordType: DWord): String;
begin
  case ARecordType of
    EMR_HEADER: Result := 'EMR_Header';
    EMR_POLYBEZIER: Result := 'EMR_PolyBezier';
    EMR_POLYGON: Result := 'EMR_Polygon';
    EMR_POLYLINE: Result := 'EMR_PolyLine';
    EMR_POLYBEZIERTO: Result := 'EMR_PolyBezierTo';
    EMR_POLYLINETO: Result := 'EMR_PolyLineTo';
    EMR_POLYPOLYLINE: Result := 'EMR_PolyPolyLine';
    EMR_POLYPOLYGON: Result := 'EMR_PolyPolygon';
    EMR_SETWINDOWEXTEX: Result := 'EMR_SetWindowExtEx';
    EMR_SETWINDOWORGEX: Result := 'EMR_SetWindowOrgEx';
    EMR_SETVIEWPORTEXTEX: Result := 'EMR_SetViewPortExtEx';
    EMR_SETVIEWPORTORGEX: Result := 'EMR_SetViewportOrgEx';
    EMR_SETBRUSHORGEX: Result := 'EMR_SetBrushOrgEx';
    EMR_EOF: Result := 'EMR_EOF';
    EMR_SETPIXELV: Result := 'EMR_SetPixelV';
    EMR_SETMAPPERFLAGS: Result := 'EMR_SetMapperFlags';
    EMR_SETMAPMODE: Result := 'EMR_SetMapMode';
    EMR_SETBKMODE: Result := 'EMR_SetBkMode';
    EMR_SETPOLYFILLMODE: Result := 'EMR_SetPolyFillMode';
    EMR_SETROP2: Result := 'EMR_SetROP2';
    EMR_SETSTRETCHBLTMODE: Result := 'EMR_SetStretchBLTMode';
    EMR_SETTEXTALIGN: Result := 'EMR_SetTextAlign';
    EMR_SETCOLORADJUSTMENT: Result := 'EMR_SetColorAdjustment';
    EMR_SETTEXTCOLOR: Result := 'EMR_SetTextColor';
    EMR_SETBKCOLOR: Result := 'EMR_SetBkColor';
    EMR_OFFSETCLIPRGN: Result := 'EMR_OffsetClipRGN';
    EMR_MOVETOEX: Result := 'EMR_MoveToEx';
    EMR_SETMETARGN: Result := 'EMR_SetMetaRGN';
    EMR_EXCLUDECLIPRECT: Result := 'EMR_ExcludeClipRect';
    EMR_INTERSECTCLIPRECT: Result := 'EMR_IntersectClipRect';
    EMR_SCALEVIEWPORTEXTEX: Result := 'EMR_ScaleViewportExtExt';
    EMR_SCALEWINDOWEXTEX: Result := 'EMR_ScaleWindowExtEx';
    EMR_SAVEDC: Result := 'EMR_SaveDC';
    EMR_RESTOREDC: Result := 'EMR_RestoreDC';
    EMR_SETWORLDTRANSFORM: Result := 'EMR_SetWorldTransform';
    EMR_MODIFYWORLDTRANSFORM: Result := 'EMR_ModifyWorldTransform';
    EMR_SELECTOBJECT: Result := 'EMR_SelectObject';
    EMR_CREATEPEN: Result := 'EMR_CreatePen';
    EMR_CREATEBRUSHINDIRECT: Result := 'EMR_CreateBrushIndirect';
    EMR_DELETEOBJECT: Result := 'EMR_DeleteObject';
    EMR_ANGLEARC: Result := 'EMR_AngleArc';
    EMR_ELLIPSE: Result := 'EMR_Ellipse';
    EMR_RECTANGLE: Result := 'EMR_Rectangle';
    EMR_ROUNDRECT: Result := 'EMR_RoundRect';
    EMR_ARC: Result := 'EMR_Arc';
    EMR_CHORD: Result := 'EMR_Chord';
    EMR_PIE: Result := 'EMR_Pie';
    EMR_SELECTPALETTE: Result := 'EMR_SelectPalette';
    EMR_CREATEPALETTE: Result := 'EMR_CreatePalette';
    EMR_SETPALETTEENTRIES: Result := 'EMR_SetPaletteEntries';
    EMR_RESIZEPALETTE: Result := 'EMR_ResizePalette';
    EMR_REALIZEPALETTE: Result := 'EMR_RealizePalette';
    EMR_EXTFLOODFILL: Result := 'EMR_ExtFloodFill';
    EMR_LINETO: Result := 'EMR_LineTo';
    EMR_ARCTO: Result := 'EMR_ArcTo';
    EMR_POLYDRAW: Result := 'EMR_PolyDraw';
    EMR_SETARCDIRECTION: Result := 'EMR_SetArcDirection';
    EMR_SETMITERLIMIT: Result := 'EMR_SetMiterLimit';
    EMR_BEGINPATH: Result := 'EMR_BeginPath';
    EMR_ENDPATH: Result := 'EMR_EndPath';
    EMR_CLOSEFIGURE: Result := 'EMR_CloseFigure';
    EMR_FILLPATH: Result := 'EMR_FillPath';
    EMR_STROKEANDFILLPATH: Result := 'EMR_StrokeAndFillPath';
    EMR_STROKEPATH: Result := 'EMR_StrokePath';
    EMR_FLATTENPATH: Result := 'EMR_FlattenPath';
    EMR_WIDENPATH: Result := 'EMR_WidenPath';
    EMR_SELECTCLIPPATH: Result := 'EMR_SelectClipPath';
    EMR_ABORTPATH: Result := 'EMR_AbortPath';
    EMR_COMMENT: Result := 'EMR_Comment';
    EMR_FILLRGN: Result := 'EMR_FillRgn';
    EMR_FRAMERGN: Result := 'EMR_FrameRgn';
    EMR_INVERTRGN: Result := 'EMR_InvertRgn';
    EMR_PAINTRGN: Result := 'EMR_PaintRgn';
    EMR_EXTSELECTCLIPRGN: Result := 'EMR_ExtSelectClipRgn';
    EMR_BITBLT: Result := 'EMR_BitBLT';
    EMR_STRETCHBLT: Result := 'EMR_StretchBLT';
    EMR_MASKBLT: Result := 'EMR_MaskBLT';
    EMR_PLGBLT: Result := 'EMR_PLGBLT';
    EMR_SETDIBITSTODEVICE: Result := 'EMR_SetDIBitsToDevice';
    EMR_STRETCHDIBITS: Result := 'EMR_StretchDIBits';
    EMR_EXTCREATEFONTINDIRECTW: Result := 'EMR_CreateFontIndirectW';
    EMR_EXTTEXTOUTA: Result := 'EMR_ExtTextOutA';
    EMR_EXTTEXTOUTW: Result := 'EMR_ExtTextOutW';
    EMR_POLYBEZIER16: Result := 'EMR_PolyBezier16';
    EMR_POLYGON16: Result := 'EMR_Polygon16';
    EMR_POLYLINE16: Result := 'EMR_PolyLine16';
    EMR_POLYBEZIERTO16: Result := 'EMR_PolyBezierTo16';
    EMR_POLYLINETO16: Result := 'EMR_PolyLineTo16';
    EMR_POLYPOLYLINE16: Result := 'EMR_PolyPolyLine16';
    EMR_POLYPOLYGON16: Result := 'EMR_PolyPolygon16';
    EMR_POLYDRAW16: Result := 'EMR_PolyDraw16';
    EMR_CREATEMONOBRUSH: Result := 'EMR_CreateMonoBrush';
    EMR_CREATEDIBPATTERNBRUSHPT: Result := 'EMR_CreateDIBPatternBrushPt';
    EMR_EXTCREATEPEN: Result := 'EMR_ExtCreatePen';
    EMR_POLYTEXTOUTA: Result := 'EMR_PolyTextOutA';
    EMR_POLYTEXTOUTW: Result := 'EMR_PolyTextOutW';
    EMR_SETICMMODE: Result := 'EMR_SetICMMode';
    EMR_CREATECOLORSPACE: Result := 'EMR_CreateColorSpace';
    EMR_SETCOLORSPACE: Result := 'EMR_SetColorSpace';
    EMR_DELETECOLORSPACE: Result := 'EMR_DeleteColorSpace';
    EMR_GLSRECORD: Result := 'EMR_GLSRecord';
    EMR_GLSBOUNDEDRECORD: Result := 'EMR_GLSBoundedRecord';
    EMR_PIXELFORMAT: Result := 'EMR_PixelFormat';
    EMR_DRAWESCAPE: Result := 'EMR_DrawEscape';
    EMR_EXTESCAPE: Result := 'EMR_Escape';
    EMR_SMALLTEXTOUT: Result := 'EMR_SmallTextOut';
    EMR_FORCEUFIMAPPING: Result := 'EMR_ForcedUFIMapping';
    EMR_NAMEDESCAPE: Result := 'EMR_NamedEscape';
    EMR_COLORCORRECTPALETTE: Result := 'EMR_ColorCorrectPalette';
    EMR_SETICMPROFILEA: Result := 'EMR_SetICMProfileA';
    EMR_SETICMPROFILEW: Result := 'EMR_SetICMProfileW';
    EMR_ALPHABLEND: Result := 'EMR_AlphaBlend';
    EMR_SETLAYOUT: Result := 'EMR_SetLayout';
    EMR_TRANSPARENTBLT: Result := 'EMR_TransparentBlt';
    EMR_GRADIENTFILL: Result := 'EMR_GradientFill';
    EMR_SETLINKEDUFIS: Result := 'EMR_SetLinkedUFIS';
    EMR_SETTEXTJUSTIFICATION: Result := 'EMR_SetTextJustification';
    EMR_COLORMATCHTOTARGETW: Result := 'EMR_ColorMatchToTargetW';
    EMR_CREATECOLORSPACEW: Result := 'EMR_CreateColorSpaceW';
    else Result := '(unknown, Value ' + IntToStr(ARecordType) + ')';
  end;
end;

end.

