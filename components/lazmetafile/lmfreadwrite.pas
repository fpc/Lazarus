unit lmfReadWrite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  Graphics, LCLType,
  lmf, lmfWMF;

type
  TWMFWriter = class(TlmfWriter)
  private
    FImage: TlmfImage;
    FMaxRecordSize: Int64;
    FLogicalMaxX: Word;        // Max x coordinate used for scaling, in logical units
    FLogicalMaxY: Word;        // Max y coordinate used for scaling, in logical units
    FScalingFactor: Double;    // Conversion to logical units
    FObjTable: TFPList;        // List with WMF objects (pen, brush, ...)

    // Specific WMF records
    procedure WriteBkColor(AStream: TStream; AColor: TColor);
    procedure WriteBkMode(AStream: TStream; AMode: Word);
    procedure WriteEOF(AStream: TStream);
    procedure WriteMapMode(AStream: TStream; AMode: Word);
    procedure WriteTextAlign(AStream: TStream; AValue: Word);
    procedure WriteWindowExt(AStream: TStream);
    procedure WriteWindowOrg(AStream: TStream);
  protected
    // General routines
    function CalcChecksum(P: PWord; ASize: Word): Word;
    function MakeWMFColorRecord(AColor: TColor): TWMFColorRecord;
    procedure PrepareScaling;
    // General WMR record writing
    procedure WriteRecords(AStream: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteToStream(AStream: TStream; AImage: TlmfImage); override;

    // needed by lmfObjects for writing to wmf:
    function AddToObjTable(AItem: TComponent): Integer; override;
    function FindInObjTable(AItem: TComponent): Integer; override;
    function ScaleX(x: Double): Integer; override;
    function ScaleY(y: Double): Integer; override;
    function ScaleSizeX(x: Double): Integer; override;
    function ScaleSizeY(y: Double): Integer; override;
    procedure WriteWMFRecord(AStream: TStream; AFunc: word; ASize: Integer); override; overload;
    procedure WriteWMFRecord(AStream: TStream; AFunc: Word; const AParams; ASize: Integer); override; overload;
    procedure WriteWMFParams(AStream: TStream; const AParams; ASize: Integer); override;
  end;

  TEMFWriter = class(TlmfWriter)
  private
  public
    constructor Create;
    procedure WriteToStream(AStream: TStream; AImage: TlmfImage); override;
  end;

function WMF_GetRecordTypeName(ARecordType: Word): String;


implementation

uses
  lmfObj;

const
  ONE_INCH = 25.4;     // 1 inch = 25.4 mm
  SIZE_OF_WORD = 2;

type
  TEnhancedMetaHeader = packed record      // 80 bytes
    RecordType: DWord;        // Record type, must be 00000001h for EMF
    RecordSize: DWord;        // Size of the record in bytes
    BoundsLeft: LongInt;      // Left inclusive bounds
    BoundsRight: LongInt;     // Right inclusive bounds
    BoundsTop: LongInt;       // Top inclusive bounds
    BoundsBottom: LongInt;    // Bottom inclusive bounds
    FrameLeft: LongInt;       // Left side of inclusive picture frame
    FrameRight: LongInt;      // Right side of inclusive picture frame
    FrameTop: LongInt;        // Top side of inclusive picture frame
    FrameBottom: LongInt;     // Bottom side of inclusive picture frame
    Signature: DWord;         // Signature ID (always $464D4520)
    Version: DWord;           // Version of the metafile, always $00000100
    Size: DWord;              // Size of the metafile in bytes
    NumOfRecords: DWord;      // Number of records in the metafile
    NumOfHandles: Word;       // Number of handles in the handle table
    Reserved: Word;           // Not used (always 0)
    SizeOfDescrip: DWord;     // Length of description string (16-bit chars) in WORDs, incl zero
    OffsOfDescrip: DWord;     // Offset of description string in metafile (from beginning)
    NumPalEntries: DWord;     // Number of color palette entries
    WidthDevPixels: LongInt;  // Width of display device in pixels
    HeightDevPixels: LongInt; // Height of display device in pixels
    WidthDevMM: LongInt;      // Width of display device in millimeters
    HeightDevMM: LongInt;     // Height of display device in millimeters
  end;

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

  TWMFBitmapCoreHeader = packed record
    HeaderSize: DWord;
    Width: Word;
    Height: Word;
    Planes: Word;
    BitCount: Word;
  end;
  PWMFBitmapCoreHeader = ^TWMFBitmapCoreHeader;

  TWMFBitmapInfoHeader = packed record
    HeaderSize: DWord;
    Width: LongInt;
    Height: LongInt;
    Planes: Word;
    BitCount: Word;
    Compression: DWord;
    ImageSize: DWord;
    XPelsPerMeter: DWord;
    YPelsPerMeter: DWord;
    ColorsUsed: DWord;
    ColorImporant: DWord;
  end;
  PWMFBitmapInfoHeader = ^TWMFBitmapInfoHeader;

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

  // Brush styles
  BS_SOLID = $0000;
  BS_NULL = $0001;
  BS_HATCHED = $0002;
  BS_PATTERN = $0003;
  BS_INDEXED = $0004;
  BS_DIBPATTERN = $0005;
  BS_DIBPATTERNPT = $0006;
  BS_PATTERN8X8 = $0007;
  BS_DIBPATTERN8X8 = $0008;
  BS_MONOPATTERN = $0009;

  // Character sets
  ANSI_CHARSET = $00000000;
  DEFAULT_CHARSET = $00000001;
  SYMBOL_CHARSET = $00000002;
  MAC_CHARSET = $0000004D;
  SHIFTJIS_CHARSET = $00000080;
  HANGUL_CHARSET = $00000081;
  JOHAB_CHARSET = $00000082;
  GB2312_CHARSET = $00000086;
  CHINESEBIG5_CHARSET = $00000088;
  GREEK_CHARSET = $000000A1;
  TURKISH_CHARSET = $000000A2;
  VIETNAMESE_CHARSET = $000000A3;
  HEBREW_CHARSET = $000000B1;
  ARABIC_CHARSET = $000000B2;
  BALTIC_CHARSET = $000000BA;
  RUSSIAN_CHARSET = $000000CC;
  THAI_CHARSET = $000000DE;
  EASTEUROPE_CHARSET = $000000EE;
  OEM_CHARSET = $000000FF;
          (*
  // ExtTextOutOptions flags
  ETO_OPAQUE = $0002;
  ETO_CLIPPED = $0004;
  ETO_GLYPHINDEX = $0010;
  ETO_RTLREADING = $0080;
  ETO_NUMERICSLOCAL = $0400;
  ETO_NUMERICSLATIN = $0800;
  ETO_PDY = $2000;
                *)
  // Family font
  FF_DONTCARE = $00;
  FF_ROMAN = $01;
  FF_SWISS = $02;
  FF_MODERN = $03;
  FF_SCRIPT = $04;
  FF_DECORATIVE = $05;

  // Flood fill
  FLOODFILLBORDER = $0000;
  FLOODFILLSURFACE = $0001;

  // Font quality
  DEFAULT_QUALITY = $00;
  DRAFT_QUALITY = $01;
  PROOF_QUALITY = $02;
  NONANTIALIASED_QUALITY = $03;
  ANTIALIASED_QUALITY = $04;
  CLEARTYPE_QUALITY = $05;

  // Hatch style
  HS_HORIZONTAL = $0000;
  HS_VERTICAL = $0001;
  HS_FDIAGONAL = $0002; // \\\
  HS_BDIAGONAL = $0003; // ///
  HS_CROSS = $0004;     // +++
  HS_DIAGCROSS = $0005; // xxxx

  // Map mode
  MM_TEXT = $0001;         // 1 logical unit = 1 device pixel. +x right, +y down
  MM_LOMETRIC = $0002;     // 1 logical unit = 0.1 mm. +x right, +y up
  MM_HIMETRIC = $0003;     // 1 logical unit = 0.01 mm. +x right, +y up
  MM_LOENGLISH = $0004;    // 1 logical unit = 0.01 inch. +x right, +y up
  MM_HIENGLISH = $0005;    // 1 logical unit = 0.001 inch. +x right, +y up
  MM_TWIPS = $0006;        // 1 logical unit = 1/20 point = 1/1440 inch (twip). +x right, +y up
  MM_ISOTROPIC = $0007;    // arbitrary units, equally scaled axes. --> META_SETWINDOWEXT, META_SETWINDOWORG
  MM_ANISOTROPIC = $0008;  // arbitrary units, arbitrarily scaled axes.

  // Metafile enumeration
  MEMORYMETAFILE = $0001;  // Metafile is stored in memory
  DISKMETAFILE = $0002;    // ... on disk.

  // Background MixMode for text, hatched brushes and other nonsolid pen styles
  BM_TRANSPARENT = $0001;
  BM_OPAQUE = $0002;

  // PitchFont
  DEFAULT_PITCH = 0;
  FIXED_PITCH = 1;
  VARIABLE_PITCH = 2;
             (*
  // TextAlignment flags
  TA_NOUPDATECP = $0000;
  TA_LEFT = $0000;
  TA_TOP = $0000;
  TA_UPDATECP = $0001;
  TA_RIGHT = $0002;
  TA_CENTER = $0006;   // Value is correct ($0004 looks more reasonable, though)
  TA_BOTTOM = $0008;
  TA_BASELINE = $0018;
  TA_RTLREADING = $0100;

  // Vertical text alignment flags
  // Used if font has vertical baseline, such as Kanji.
  VTA_TOP = $0000;
  VTA_RIGHT = $0000;
  VTA_BOTTOM = $0002;
  VTA_CENTER = $0006;  // why not $0004?
  VTA_BASELINE = $0018;
               *)
  // Ternary Raster Operations
  BLACKNESS = $00;
  NOTSRCERASE = $11;
  NOTSRCCOPY = $33;
  SRCERASE = $44;
  DSTINVERT = $55;
  SRCINVERT = $66;
  MERGEPAINT = $BB;
  MERGECOPY = $C0;
  SRCCOPY = $CC;
  SRCPAINT = $FF;
  PATCOPY = $F0;
  PATPAINT = $FB;
  WHITENESS = $FF;
  // ... plus many more...

  // Color usage
  DIB_RGB_COLORS = $0000;
  DIB_PAL_COLORS = $0001;
  DIB_PAL_INDICES = $0002;

  // Compression
  BI_RGB = $0000;
  BI_RLE8 = $0001;
  BI_RLE4 = $0002;
  BI_BITFIELDS = $0003;
  BI_JPEG = $0004;
  BI_PNG = $0005;
  BI_CMYK = $000B;
  BI_CMYKRLE8 = $000C;
  BI_CMYKRLE4 = $000D;

  // Arc direction (EMF)
  AD_COUNTERCLOCKWISE = $00000001;
  AD_CLOCKWISE = $00000002;

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
  end;
end;

{ TWMFWriter }

constructor TWMFWriter.Create;
begin
  inherited Create;
  FObjTable := TFPList.Create;
end;

destructor TWMFWriter.Destroy;
begin
  FObjTable.Free;  // Do not destroy the objects, they are owned by the image.
  inherited;
end;

{ Calculate the checksum of the PlaceableHeader (without the Checksum field) }
function TWMFWriter.CalcChecksum(P: PWord; ASize: Word): Word;
var
  n: Integer;
begin
  Result := 0;
  n := 0;
  while n < ASize do begin
    Result := Result xor P^;
    inc(P);
    inc(n, SIZE_OF_WORD);
  end;
end;

function TWMFWriter.AddToObjTable(AItem: TComponent): Integer;
begin
  Result := FObjTable.Add(AItem);
end;

function TWMFWriter.FindInObjTable(AItem: TComponent): Integer;
begin
  Result := FObjTable.IndexOf(AItem);
end;

function TWMFWriter.MakeWMFColorRecord(AColor: TColor): TWMFColorRecord;
var
  c: TRGBQuad;
begin
  c := TRGBQuad(AColor);
  Result.ColorRED := c.rgbRed;
  Result.ColorGREEN := c.rgbGreen;
  Result.ColorBLUE := c.rgbBlue;
  Result.Reserved := 0;
end;

procedure TWMFWriter.PrepareScaling;
const
  MAXINT16 = 30000;   // should be 32767, but reduce to avoid overflows...
var
  maxx, maxy: Double;
  w, h: Double;
begin
  w := FImage.Width;
  h := FImage.Height;

  // wmf stores coordinates in "logical units" where 1 logical unit is
  // 1/100 mm = 10 microns (in MM_HIMETRIC mode)
  FScalingFactor := ONE_INCH * 100;
  maxx := FScalingFactor * w;
  maxy := FScalingFactor * h;
  // wmf is 16 bit only! --> reduce magnification if numbers get too big
  if Max(maxx, maxy) > MAXINT16 then
  begin
    FScalingFactor := trunc(MAXINT16 / Max(w, h));
    maxx := FImage.Width * FScalingFactor;
    maxy := FImage.Height * FScalingFactor;
  end;
  FLogicalMaxX := trunc(maxx);
  FLogicalMaxY := trunc(maxy);
end;

{ Scaling routines which convert to wmf-specific units ("logical units").
  We silently assume that there is no offset in origin. }
function TWMFWriter.ScaleSizeX(x: Double): Integer;
begin
  Result := Round(x * FScalingFactor);
end;

function TWMFWriter.ScaleSizeY(y: Double): Integer;
begin
  Result := Round(y * FScalingFactor);
end;

function TWMFWriter.ScaleX(x: Double): Integer;
begin
  Result := ScaleSizeX(x);
end;

// Assuming that the y origin is at the top of the page.
function TWMFWriter.ScaleY(y: Double): Integer;
begin
  Result := ScaleSizeY(y);
end;

procedure TWMFWriter.WriteBkColor(AStream: TStream; AColor: TColor);
var
  rec: TWMFColorRecord;
begin
  rec := MakeWMFColorRecord(AColor);
  WriteWMFRecord(AStream, META_SETBKCOLOR, rec, SizeOf(rec));
end;

procedure TWMFWriter.WriteBkMode(AStream: TStream; AMode: Word);
var
  mode: DWord;
begin
  if AMode in [BM_TRANSPARENT, BM_OPAQUE] then begin
    mode := AMode;
    WriteWMFRecord(AStream, META_SETBKMODE, mode, SizeOf(mode));
  end;
end;

procedure TWMFWriter.WriteEOF(AStream: TStream);
begin
  WriteWMFRecord(AStream, META_EOF, 0);
end;

procedure TWMFWriter.WriteMapMode(AStream: TStream; AMode: Word);
begin
  WriteWMFRecord(AStream, META_SETMAPMODE, AMode, SizeOf(AMode));
end;

procedure TWMFWriter.WriteRecords(AStream: TStream);
var
  i: Integer;
  item: TlmfObject;
begin
  // Setup defaults
  WriteWindowExt(AStream);
  WriteWindowOrg(AStream);
  WriteMapMode(AStream, MM_ANISOTROPIC);
  WriteBkColor(AStream, clWhite);
  WriteBkMode(AStream, BM_TRANSPARENT);
  WriteTextAlign(AStream, TA_TOP or TA_LEFT);

  // Write object records of the drawing
  for i := 0 to FImage.List.ComponentCount-1 do
  begin
    item := TlmfObject(FImage.List.Components[i]);
    item.WriteWMFRecord(FImage, Self, AStream);
  end;

  // Last record must be an EOF record.
  WriteEOF(AStream);
end;

procedure TWMFWriter.WriteTextAlign(AStream: TStream; AValue: word);
begin
  WriteWMFRecord(AStream, META_SETTEXTALIGN, AValue, SizeOf(AValue));
end;

procedure TWMFWriter.WriteToStream(AStream: TStream; AImage: TlmfImage);
var
  placeableHeader: TPlaceableMetaHeader;
  wmfHeader: TWMFHeader;
  startPos: Int64;
begin
  FImage := AImage;
  startPos := AStream.Position;

  //MakeObjTable;
  PrepareScaling;

  // Write placeholder for WMF header and placeable header (because we don't
  // know FMaxRecordSize yet), will be rewritten with correct values later.
  placeableHeader := Default(TPlaceableMetaHeader);
  wmfHeader := Default(TWMFHeader);
  AStream.Write(placeableHeader, SizeOf(TPlaceableMetaHeader));
  AStream.Write(wmfHeader, SizeOf(TWMFHeader));

  // Write the records of the image
  WriteRecords(AStream);

  // Go back to the beginning of the file and write the headers. Use correct
  // header fields now.
  placeableHeader := Default(TPlaceableMetaHeader);
  with placeableHeader do begin
    Key := WMF_MAGIC_NUMBER;
    Handle := 0;
    Reserved := 0;
    Inch := ScaleX(ONE_INCH);
    Left := 0;
    Top := 0;
    Right := FImage.Width;
    Bottom := FImage.Height;
    Checksum := CalcChecksum(@placeableHeader, SizeOf(TPlaceableMetaHeader));
  end;
  AStream.Position := startPos;
  AStream.WriteBuffer(placeableHeader, SizeOf(TPlaceableMetaHeader));

  wmfHeader := Default(TWMFHeader);
  with wmfHeader do begin
    FileType := 1;
    HeaderSize := 9;
    Version := $0300;
    NumOfObjects := FImage.List.ComponentCount;
    MaxRecordSize := FMaxRecordSize;
    FileSize := AStream.Size div SIZE_OF_WORD;
    NumOfParams := 0;
  end;
  AStream.WriteBuffer(wmfHeader, SizeOf(TWMFHeader));
end;

procedure TWMFWriter.WriteWindowExt(AStream: TStream);
var
  params: Array[0..1] of word;
begin
  params[0] := FLogicalMaxY;
  params[1] := FLogicalMaxX;
  WriteWMFRecord(AStream, META_SETWINDOWEXT, params, SizeOf(params));
end;

procedure TWMFWriter.WriteWindowOrg(AStream: TStream);
var
  params: Array[0..1] of word;
begin
  params[0] := 0;
  params[1] := 0;
  WriteWMFRecord(AStream, META_SETWINDOWORG, params, Sizeof(params));
end;

{ ASize is in bytes }
procedure TWMFWriter.WriteWMFRecord(AStream: TStream;
  AFunc: Word; ASize: Integer);
var
  rec: TWMFRecord;
begin
  rec.Size := (SizeOf(TWMFRecord) + ASize) div SIZE_OF_WORD;
  rec.Func := AFunc;
  AStream.WriteBuffer(rec, SizeOf(TWMFRecord));
  FMaxRecordSize := Max(FMaxRecordSize, rec.Size);
end;

{ ASize is the size of the parameter part, in bytes }
procedure TWMFWriter.WriteWMFRecord(AStream: TStream;
  AFunc: Word; const AParams; ASize: Integer);
var
  rec: TWMFRecord;
begin
  rec.Size := (SizeOf(TWMFRecord) + ASize) div SIZE_OF_WORD;
  rec.Func := AFunc;
  AStream.WriteBuffer(rec, SizeOf(TWMFRecord));
  AStream.WriteBuffer(AParams, ASize);
end;

{ ASize is in bytes }
procedure TWMFWriter.WriteWMFParams(AStream: TStream;
  const AParams; ASize: Integer);
begin
  AStream.WriteBuffer(AParams, ASize);
end;


{ TEMFWriter }

constructor TEMFWriter.Create;
begin
  inherited Create;
end;

procedure TEMFWriter.WriteToStream(AStream: TStream; AImage: TlmfImage);
begin
  //
end;

end.

