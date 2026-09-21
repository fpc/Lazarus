{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Font registry and glyph outlines over freetype.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.freetype;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.Math, Api.Freetypeh,
     fpsvg.types;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, math, freetypeh, fpsvg.types;
{$ENDIF FPC_DOTTEDUNITS}

type
  ESVGFreeType = class(ESVGError);

  TSVGFreeTypeProvider = class;

  { One face of one file, at one size.
    Every measure it reports is scaled from design units to that size.
    Slant leans the outlines to the right for a face used in place of a missing italic,
    and Bold thickens them in design units for a face used in place of a missing bold. }
  TSVGFreeTypeFont = class(TObject, ISVGFont)
  private
    FFace: PFT_Face;
    FName: String;
    FSize: Double;
    FScale: Double;
    FSlant: Double;
    FBold: Double;
    FKerning: Boolean;
    FUnitsPerEm: Integer;
    function LoadGlyph(aGlyph: Cardinal): Boolean;
  public
    constructor Create(aFace: PFT_Face; const aName: String; aSize: Double;
      aSlant, aBold: Double; aKerning: Boolean);
    function GetFontName: TSVGString;
    function GetUnitsPerEm: Integer;
    function GetSize: Double;
    function GetAscent: Double;
    function GetDescent: Double;
    function GetUnderlinePosition: Double;
    function GetUnderlineThickness: Double;
    function GetGlyphIndex(aCodePoint: Cardinal): Cardinal;
    function GetGlyphForRun(const aCodes: TSVGCodePointArray; aFrom: Integer;
      out aCount: Integer): Cardinal;
    function GetGlyphNamed(const aName: TSVGString): Cardinal;
    function GetGlyphAdvance(aGlyph: Cardinal): Double;
    function GetGlyphVerticalAdvance(aGlyph: Cardinal): Double;
    procedure GetGlyphVerticalOrigin(aGlyph: Cardinal;
      out aX, aY: Double);
    function GetGlyphKerning(aLeft, aRight: Cardinal): Double;
    function GetGlyphVerticalKerning(aAbove,
      aBelow: Cardinal): Double;
    function GetSmallCaps: Boolean;
    function GetGlyphOutline(aGlyph: Cardinal; aPath: TSVGPath): Boolean;
  end;

  { A font file the registry knows, with the values read from its face.
    The face is opened again only when a request chooses this file. }
  TSVGFontEntry = record
    FileName : String;
    Family   : String;
    Weight   : Integer;
    Style    : TSVGFontStyle;
    Stretch  : TSVGFontStretch;
    Face     : PFT_Face;
  end;
  TSVGFontEntryArray = array of TSVGFontEntry;

  { A code point already looked up, and the file that covered it.
    An index of -1 means that nothing covered it. }
  TSVGCoverCache = record
    CodePoint : Cardinal;
    Entry     : Integer;
  end;
  TSVGCoverCacheArray = array of TSVGCoverCache;

  { A registry of font files. It resolves a family list to a face.
    Creating one never raises: without the freetype library it resolves nothing. }
  TSVGFreeTypeProvider = class(TObject, ISVGFontProvider)
  private
    FLibrary: PFT_Library;
    FAvailable: Boolean;
    FEntries: TSVGFontEntryArray;
    FEntryCount: Integer;
    FFonts: TFPList;
    FDefaultFamily: String;
    FGeneric: array[TSVGGenericFamily] of String;
    FGenericKnown: array[TSVGGenericFamily] of Boolean;
    FCoverage: ISVGFontCoverage;
    FReadFaceTables: Boolean;
    FKerning: Boolean;
    FCovers: TSVGCoverCacheArray;
    FCoverCount: Integer;
    function CachedCover(aCodePoint: Cardinal): Integer;
    procedure RememberCover(aCodePoint: Cardinal; aIndex: Integer);
    function FaceOf(aIndex: Integer): PFT_Face;
    function IndexOfFamily(const aFamily: String; aWeight: Integer;
      aStyle: TSVGFontStyle; aStretch: TSVGFontStretch): Integer;
    function IndexOfStyle(aWeight: Integer; aStyle: TSVGFontStyle;
      aStretch: TSVGFontStretch): Integer;
    function IndexOfGeneric(aGeneric: TSVGGenericFamily; aWeight: Integer;
      aStyle: TSVGFontStyle; aStretch: TSVGFontStretch): Integer;
    function IndexOfCovering(const aRequest: TSVGFontRequest;
      aCodePoint: Cardinal): Integer;
    function HasFamily(const aFamily: String): Boolean;
    function FamilyLike(const aWanted, aAvoid: String): String;
    function GetGenericFamily(aGeneric: TSVGGenericFamily): String;
    procedure SetGenericFamily(aGeneric: TSVGGenericFamily;
      const aValue: String);
    function GetEntry(aIndex: Integer): TSVGFontEntry;
  public
    constructor Create;
    destructor Destroy; override;
    // Records a font file. False when it holds no readable face.
    function AddFontFile(const aFileName: String): Boolean;
    // Records a font file under the given family, weight and style
    // instead of the ones the face itself declares.
    function AddFontResource(const aFamily: TSVGString; aWeight: Integer;
      aStyle: TSVGFontStyle; const aFileName: String): Boolean;
    // Records every font file in a directory. Returns how many were added.
    function AddFontPath(const aDirectory: String): Integer;
    // Records the font files in the usual places for this platform.
    function AddSystemFonts: Integer;
    function ResolveFont(const aRequest: TSVGFontRequest): ISVGFont;
    function ResolveCover(aCodePoint: Cardinal;
      const aRequest: TSVGFontRequest): ISVGFont;
    // True when the freetype library could be loaded.
    property Available: Boolean read FAvailable;
    // Number of font files recorded.
    property EntryCount: Integer read FEntryCount;
    // A recorded font file, by index.
    property Entries[aIndex: Integer]: TSVGFontEntry read GetEntry;
    // Family used when a named font cannot be found.
    property DefaultFamily: String read FDefaultFamily write FDefaultFamily;
    { The registered family used for a generic name.
      Reading picks one of the registered families;
      writing sets it explicitly. }
    property GenericFamily[aGeneric: TSVGGenericFamily]: String
      read GetGenericFamily write SetGenericFamily;
    // Returns a file that covers a character the requested face has no glyph for.
    property Coverage: ISVGFontCoverage read FCoverage write FCoverage;
    // Read the weight and the width of a face from its OS/2 table when
    // its style name gives neither.
    // True by default. Set it to False to use the style names alone.
    property ReadFaceTables: Boolean read FReadFaceTables
      write FReadFaceTables;
    // Draw a pair of glyphs closer together according to the kern table
    // of the file. False by default: that table is the old format, a
    // modern face keeps its pairs elsewhere, and the reference images of
    // the W3C suite were drawn without it.
    property Kerning: Boolean read FKerning write FKerning;
  end;

// The directories that fonts are looked for in, one per line.
function SVGSystemFontPaths: String;
// The weight given by a style name: 100 for Thin up to 900 for Black.
// False when the name gives none, as Regular and Book do; 
function SVGWeightOfFaceName(const aText: String;
  out aWeight: Integer): Boolean;
// The width given by a family or style name. False when there is none availabe.
function SVGStretchOfFaceName(const aText: String;
  out aStretch: TSVGFontStretch): Boolean;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ELSE FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ENDIF FPC_DOTTEDUNITS}

{$IFDEF VER3_2}
// Calls the 3.2.2 binding does not declare. The library exports them;
// only the Pascal header is missing, and the trunk binding has both.
type
  TFT_Sfnt_Tag = (FT_SFNT_HEAD, FT_SFNT_MAXP, FT_SFNT_OS2, FT_SFNT_HHEA,
    FT_SFNT_VHEA, FT_SFNT_POST, FT_SFNT_PCLT, FT_SFNT_MAX);

  { The head of the OS/2 table of a face. Only the fields read here are
    declared. The rest of the table follows them and is not read. }
  PTT_OS = ^TTT_OS2;
  TTT_OS2 = record
    version       : FT_UShort;
    xAvgCharWidth : FT_Short;
    usWeightClass : FT_UShort;
    usWidthClass  : FT_UShort;
  end;

function FT_Get_Sfnt_Table(face: PFT_Face; tag: TFT_Sfnt_Tag): Pointer;
  cdecl; external FreeTypeDLL name 'FT_Get_Sfnt_Table';
function FT_Outline_Embolden(outline: PFT_Outline; strength: FT_Pos): Integer;
  cdecl; external FreeTypeDLL name 'FT_Outline_Embolden';
{$ENDIF}

const
  // The tangent of twelve degrees, which is about how far the italic of a
  // face leans when the face has one.
  SVGObliqueSlant = 0.2126;
  // The fraction of an em a face is thickened by when it is used in place
  // of a bold. The freetype demonstration programs use this value.
  SVGSyntheticBold = 1 / 24;
  // From this weight up a face counts as bold.
  SVGBoldWeightFrom = 600;
  FTCurveTagOn = 1;
  FTCurveTagCubic = 2;
  { The families used for each generic name, best first.
    The lists hold faces shipped by the free desktops and by Windows and
    macOS, so one of them is present almost everywhere. }
  { The Liberation faces come first: they have the widths of Arial, Times
    New Roman and Courier New, the faces a document written for the web
    was drawn against.}
  SerifFamilies: array[0..7] of String = (
    'Liberation Serif', 'DejaVu Serif', 'Times New Roman', 'Times',
    'Nimbus Roman', 'FreeSerif', 'Noto Serif', 'Bitstream Vera Serif');
  SansFamilies: array[0..7] of String = (
    'Liberation Sans', 'DejaVu Sans', 'Arial', 'Helvetica',
    'Nimbus Sans', 'FreeSans', 'Noto Sans', 'Bitstream Vera Sans');
  MonoFamilies: array[0..7] of String = (
    'Liberation Mono', 'DejaVu Sans Mono', 'Courier New', 'Courier',
    'Nimbus Mono PS', 'FreeMono', 'Noto Sans Mono', 'Bitstream Vera Sans Mono');
  CursiveFamilies: array[0..3] of String = (
    'Comic Sans MS', 'URW Chancery L', 'Z003', 'Apple Chancery');
  FantasyFamilies: array[0..3] of String = (
    'Impact', 'Papyrus', 'D050000L', 'Copperplate');


function SVGWeightOfFaceName(const aText: String;
  out aWeight: Integer): Boolean;

var
  lName: String;

begin
  Result := True;
  lName := LowerCase(aText);
  if Pos('thin', lName) + Pos('hairline', lName) > 0 then
    aWeight := 100
  else if Pos('extralight', lName) + Pos('extra light', lName)
          + Pos('ultralight', lName) + Pos('ultra light', lName) > 0 then
    aWeight := 200
  else if Pos('light', lName) > 0 then
    aWeight := 300
  else if Pos('extrabold', lName) + Pos('extra bold', lName)
          + Pos('ultrabold', lName) + Pos('ultra bold', lName) > 0 then
    aWeight := 800
  else if Pos('black', lName) + Pos('heavy', lName) > 0 then
    aWeight := 900
  else if Pos('semibold', lName) + Pos('semi bold', lName)
          + Pos('demibold', lName) + Pos('demi bold', lName) > 0 then
    aWeight := 600
  else if Pos('bold', lName) > 0 then
    aWeight := SVGBoldFontWeight
  else if Pos('medium', lName) > 0 then
    aWeight := 500
  else
    Result := False;
end;


function SVGStretchOfFaceName(const aText: String;
  out aStretch: TSVGFontStretch): Boolean;

var
  lName: String;

begin
  Result := True;
  lName := LowerCase(aText);
  if Pos('ultra condensed', lName) + Pos('ultracondensed', lName) > 0 then
    aStretch := fsUltraCondensed
  else if Pos('extra condensed', lName) + Pos('extracondensed', lName) > 0 then
    aStretch := fsExtraCondensed
  else if Pos('semi condensed', lName) + Pos('semicondensed', lName) > 0 then
    aStretch := fsSemiCondensed
  else if Pos('condensed', lName) + Pos('narrow', lName) > 0 then
    aStretch := fsCondensed
  else if Pos('ultra expanded', lName) + Pos('ultraexpanded', lName) > 0 then
    aStretch := fsUltraExpanded
  else if Pos('extra expanded', lName) + Pos('extraexpanded', lName) > 0 then
    aStretch := fsExtraExpanded
  else if Pos('semi expanded', lName) + Pos('semiexpanded', lName) > 0 then
    aStretch := fsSemiExpanded
  else if Pos('expanded', lName) + Pos('extended', lName) > 0 then
    aStretch := fsExpanded
  else
    Result := False;
end;


function SVGSystemFontPaths: String;

begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('SystemRoot') + '\Fonts';
  {$ELSE}
    {$IFDEF DARWIN}
    Result := '/System/Library/Fonts' + LineEnding + '/Library/Fonts';
    {$ELSE}
    Result := '/usr/share/fonts' + LineEnding + '/usr/local/share/fonts';
    {$ENDIF}
  {$ENDIF}
end;


// True when the extension is one that freetype is likely to open.
// A woff is a face packed for the web and freetype unpacks it,
// so a directory of them should be read.
function IsFontFile(const aName: String): Boolean;

var
  lExtension: String;

begin
  lExtension := LowerCase(ExtractFileExt(aName));
  Result := (lExtension = '.ttf') or (lExtension = '.otf')
         or (lExtension = '.ttc') or (lExtension = '.pfb')
         or (lExtension = '.woff') or (lExtension = '.woff2');
end;


{ TSVGFreeTypeFont }

constructor TSVGFreeTypeFont.Create(aFace: PFT_Face; const aName: String;
  aSize, aSlant, aBold: Double; aKerning: Boolean);

begin
  inherited Create;
  FFace := aFace;
  FName := aName;
  FSize := aSize;
  FSlant := aSlant;
  FBold := aBold;
  FKerning := aKerning;
  FUnitsPerEm := aFace^.units_per_EM;
  if FUnitsPerEm <= 0 then
    FUnitsPerEm := 1000;
  FScale := aSize / FUnitsPerEm;
end;


function TSVGFreeTypeFont.GetFontName: TSVGString;

begin
  Result := FName;
end;


function TSVGFreeTypeFont.GetUnitsPerEm: Integer;

begin
  Result := FUnitsPerEm;
end;


function TSVGFreeTypeFont.GetSize: Double;

begin
  Result := FSize;
end;


function TSVGFreeTypeFont.GetAscent: Double;

begin
  Result := FFace^.ascender * FScale;
end;


function TSVGFreeTypeFont.GetDescent: Double;

begin
  Result := -FFace^.descender * FScale;
end;


// The post table counts upwards from the baseline while this counts down
// the page, so the sign is reversed.
function TSVGFreeTypeFont.GetUnderlinePosition: Double;

begin
  Result := -FFace^.underline_position * FScale;
end;


function TSVGFreeTypeFont.GetUnderlineThickness: Double;

begin
  Result := FFace^.underline_thickness * FScale;
end;


function TSVGFreeTypeFont.GetGlyphIndex(aCodePoint: Cardinal): Cardinal;

begin
  Result := FT_Get_Char_Index(FFace, aCodePoint);
end;


function TSVGFreeTypeFont.LoadGlyph(aGlyph: Cardinal): Boolean;

begin
  // Loading unscaled keeps the coordinates the font was drawn in. They are
  // scaled to user units once here, in Double, instead of twice in the
  // fixed point arithmetic of freetype.
  Result := FT_Load_Glyph(FFace, aGlyph,
    FT_LOAD_NO_SCALE or FT_LOAD_NO_BITMAP) = 0;
end;


// A face from a file draws one code point at a time here. What its own
// tables make of a longer run is left to freetype and is never used.
function TSVGFreeTypeFont.GetGlyphForRun(const aCodes: TSVGCodePointArray;
  aFrom: Integer; out aCount: Integer): Cardinal;

begin
  aCount := 1;
  Result := 0;
  if aFrom < Length(aCodes) then
    Result := GetGlyphIndex(aCodes[aFrom]);
end;


// Only a font written out in the document itself gives its glyphs the
// names an altGlyph can use.
function TSVGFreeTypeFont.GetGlyphNamed(const aName: TSVGString): Cardinal;

begin
  if aName = '' then ;
  Result := 0;
end;


function TSVGFreeTypeFont.GetGlyphAdvance(aGlyph: Cardinal): Double;

begin
  Result := 0;
  if not LoadGlyph(aGlyph) then
    Exit;
  // Thickening widens the glyph, so it advances by the amount it grew.
  Result := (FFace^.glyph^.metrics.horiAdvance + FBold) * FScale;
end;


// A face without vertical metrics advances by its own height, from ascent
// to descent, which is the line spacing of the face itself. One em would
// draw the column tighter than the face is tall.
function TSVGFreeTypeFont.GetGlyphVerticalAdvance(aGlyph: Cardinal): Double;

begin
  Result := GetAscent + GetDescent;
  if (FFace = nil) or ((FFace^.face_flags and FT_FACE_FLAG_VERTICAL) = 0) then
    Exit;
  if not LoadGlyph(aGlyph) then
    Exit;
  Result := FFace^.glyph^.metrics.vertAdvance * FScale;
end;


procedure TSVGFreeTypeFont.GetGlyphVerticalOrigin(aGlyph: Cardinal;
  out aX, aY: Double);

begin
  aX := -GetGlyphAdvance(aGlyph) / 2;
  aY := GetAscent;
  if (FFace = nil) or ((FFace^.face_flags and FT_FACE_FLAG_VERTICAL) = 0) then
    Exit;
  if not LoadGlyph(aGlyph) then
    Exit;
  // The horizontal bearings are measured from one origin and the
  // vertical ones from the other, so the two together say how far apart
  // the origins stand.
  aX := -(FFace^.glyph^.metrics.horiBearingX
        - FFace^.glyph^.metrics.vertBearingX) * FScale;
  aY := (FFace^.glyph^.metrics.horiBearingY
       + FFace^.glyph^.metrics.vertBearingY) * FScale;
end;


// How much closer the kern table of the face draws a pair. FreeType reads
// that table and not the pair positioning of a modern face, so a face
// that keeps its kerning only there reports none.
function TSVGFreeTypeFont.GetGlyphKerning(aLeft, aRight: Cardinal): Double;

var
  lKern: FT_Vector;

begin
  Result := 0;
  if not FKerning or (FFace = nil) or (aLeft = 0) or (aRight = 0)
     or ((FFace^.face_flags and FT_FACE_FLAG_KERNING) = 0) then
    Exit;
  if FT_Get_Kerning(FFace, aLeft, aRight, FT_KERNING_UNSCALED, lKern) <> 0 then
    Exit;
  // The table says which way the pen moves; this says how much closer the
  // pair is drawn, which is the opposite sign.
  Result := -lKern.x * FScale;
end;


// FreeType reads the kern table, which pairs glyphs drawn side by side
// and holds nothing for a column. A face with such a pair keeps it in the
// positioning of a modern face, which is not read here.
function TSVGFreeTypeFont.GetGlyphVerticalKerning(aAbove,
  aBelow: Cardinal): Double;

begin
  if (aAbove = 0) or (aBelow = 0) then ;
  Result := 0;
end;


// Nothing is read from a system face here. An opentype face keeps its
// small capitals behind a feature, which is not examined.
function TSVGFreeTypeFont.GetSmallCaps: Boolean;

begin
  Result := False;
end;


function TSVGFreeTypeFont.GetGlyphOutline(aGlyph: Cardinal;
  aPath: TSVGPath): Boolean;

var
  lOutline: PFT_Outline;
  lPoints: PFT_Vector;
  lTags: PAnsiChar;
  lContours: ^SmallInt;
  C, I, lFirst, lLast, lCount: Integer;
  lStartX, lStartY, lPrevX, lPrevY, lX, lY, lCX, lCY: Double;
  lC1X, lC1Y: Double;
  lHavePrev: Boolean;

  function TagOf(aIndex: Integer): Byte;
  begin
    Result := Byte(lTags[lFirst + (aIndex mod lCount)]);
  end;

  procedure PointOf(aIndex: Integer; out aX, aY: Double);
  var
    lVector: PFT_Vector;
  begin
    lVector := lPoints;
    Inc(lVector, lFirst + (aIndex mod lCount));
    aX := lVector^.x * FScale;
    aY := -lVector^.y * FScale;
    // Above the baseline y is negative, so a point there moves right and
    // the letter leans the way an italic does.
    if FSlant <> 0 then
      aX := aX - aY * FSlant;
  end;

begin
  Result := False;
  if not LoadGlyph(aGlyph) then
    Exit;
  lOutline := @FFace^.glyph^.outline;
  if (lOutline^.n_contours <= 0) or (lOutline^.n_points <= 0) then
    Exit;
  if FBold > 0 then
    FT_Outline_Embolden(lOutline, Round(FBold));
  lPoints := lOutline^.points;
  lTags := lOutline^.tags;
  lContours := lOutline^.contours;
  lLast := -1;
  for C := 0 to lOutline^.n_contours - 1 do
    begin
    lFirst := lLast + 1;
    lLast := lContours[C];
    lCount := lLast - lFirst + 1;
    if lCount <= 0 then
      Continue;
    // A contour may start on an off-curve point. The midpoint between it
    // and its neighbour lies on the curve, and starts the subpath.
    if (TagOf(0) and FTCurveTagOn) <> 0 then
      PointOf(0, lStartX, lStartY)
    else
      begin
      PointOf(0, lPrevX, lPrevY);
      PointOf(lCount - 1, lX, lY);
      if (TagOf(lCount - 1) and FTCurveTagOn) <> 0 then
        begin
        lStartX := lX;
        lStartY := lY;
        end
      else
        begin
        lStartX := (lPrevX + lX) / 2;
        lStartY := (lPrevY + lY) / 2;
        end;
      end;
    aPath.MoveTo(lStartX, lStartY);
    lHavePrev := False;
    lPrevX := 0;
    lPrevY := 0;
    I := 0;
    while I < lCount do
      begin
      Inc(I);
      PointOf(I, lX, lY);
      if (TagOf(I) and FTCurveTagOn) <> 0 then
        begin
        if lHavePrev then
          aPath.QuadTo(lPrevX, lPrevY, lX, lY)
        else
          aPath.LineTo(lX, lY);
        lHavePrev := False;
        Continue;
        end;
      if (TagOf(I) and FTCurveTagCubic) <> 0 then
        begin
        // Both controls of a cubic lie off the curve: the point reached
        // here and the one after it. The third point ends the curve.
        lC1X := lX;
        lC1Y := lY;
        PointOf(I + 1, lCX, lCY);
        Inc(I, 2);
        PointOf(I, lX, lY);
        aPath.CubicTo(lC1X, lC1Y, lCX, lCY, lX, lY);
        lHavePrev := False;
        Continue;
        end;
      if lHavePrev then
        begin
        lCX := (lPrevX + lX) / 2;
        lCY := (lPrevY + lY) / 2;
        aPath.QuadTo(lPrevX, lPrevY, lCX, lCY);
        end;
      lPrevX := lX;
      lPrevY := lY;
      lHavePrev := True;
      end;
    if lHavePrev then
      aPath.QuadTo(lPrevX, lPrevY, lStartX, lStartY);
    aPath.Close;
    end;
  Result := True;
end;


{ TSVGFreeTypeProvider }

constructor TSVGFreeTypeProvider.Create;

begin
  inherited Create;
  FFonts := TFPList.Create;
  FDefaultFamily := '';
  FReadFaceTables := True;
  FAvailable := False;
  try
    FAvailable := FT_Init_FreeType(FLibrary) = 0;
  except
    FAvailable := False;
  end;
end;


destructor TSVGFreeTypeProvider.Destroy;

var
  I: Integer;

begin
  if FFonts <> nil then
    begin
    for I := 0 to FFonts.Count - 1 do
      TSVGFreeTypeFont(FFonts[I]).Free;
    FreeAndNil(FFonts);
    end;
  if FAvailable then
    begin
    for I := 0 to FEntryCount - 1 do
      if FEntries[I].Face <> nil then
        FT_Done_Face(FEntries[I].Face);
    FT_Done_FreeType(FLibrary);
    end;
  FEntries := nil;
  inherited Destroy;
end;


function TSVGFreeTypeProvider.GetEntry(aIndex: Integer): TSVGFontEntry;

begin
  if (aIndex < 0) or (aIndex >= FEntryCount) then
    raise ESVGFreeType.CreateFmt(SErrFontIndexOutOfRange, [aIndex]);
  Result := FEntries[aIndex];
end;


function TSVGFreeTypeProvider.AddFontFile(const aFileName: String): Boolean;

var
  lFace: PFT_Face;
  lTable: PTT_OS;
  lBytes: RawByteString;

begin
  Result := False;
  if not FAvailable or not FileExists(aFileName) then
    Exit;
  lFace := nil;
  lBytes := SVGFileNameBytes(aFileName);
  if FT_New_Face(FLibrary, PAnsiChar(lBytes), 0, lFace) <> 0 then
    Exit;
  if lFace = nil then
    Exit;
  if FEntryCount = Length(FEntries) then
    SetLength(FEntries, 8 + FEntryCount * 2);
  FEntries[FEntryCount].FileName := aFileName;
  FEntries[FEntryCount].Face := nil;
  if lFace^.family_name = nil then
    FEntries[FEntryCount].Family := ChangeFileExt(
      ExtractFileName(aFileName), '')
  else
    FEntries[FEntryCount].Family := StrPas(lFace^.family_name);
  lTable := nil;
  if FReadFaceTables then
    lTable := PTT_OS(FT_Get_Sfnt_Table(lFace, FT_SFNT_OS2));
  // The style name is read first. When it gives a weight it is right far
  // more often than the table, which a face cut from a variable font
  // often leaves at 400 whatever the face is called. The table is used
  // when the name states nothing, which a bold flag alone cannot do.
  if not SVGWeightOfFaceName(StrPas(lFace^.style_name),
                             FEntries[FEntryCount].Weight) then
    if (lTable <> nil) and (lTable^.usWeightClass >= 100)
       and (lTable^.usWeightClass <= 1000) then
      FEntries[FEntryCount].Weight := lTable^.usWeightClass
    else if (lFace^.style_flags and FT_STYLE_FLAG_BOLD) <> 0 then
      FEntries[FEntryCount].Weight := SVGBoldFontWeight
    else
      FEntries[FEntryCount].Weight := SVGNormalFontWeight;
  if (lFace^.style_flags and FT_STYLE_FLAG_ITALIC) <> 0 then
    FEntries[FEntryCount].Style := fnItalic
  else
    FEntries[FEntryCount].Style := fnNormal;
  // usWidthClass runs from 1 to 9, ultra-condensed to ultra-expanded,
  // which is the order of TSVGFontStretch.
  if not SVGStretchOfFaceName(
           FEntries[FEntryCount].Family + ' ' + StrPas(lFace^.style_name),
           FEntries[FEntryCount].Stretch) then
    if (lTable <> nil) and (lTable^.usWidthClass >= 1)
       and (lTable^.usWidthClass <= 9) then
      FEntries[FEntryCount].Stretch :=
        TSVGFontStretch(lTable^.usWidthClass - 1)
    else
      FEntries[FEntryCount].Stretch := fsNormal;
  // The values read from the face are kept and the face itself is closed.
  // A system holds thousands of faces, and few are ever used.
  FT_Done_Face(lFace);
  Inc(FEntryCount);
  // A family added since a generic name was last resolved may be a better
  // answer than the one picked then.
  FillChar(FGenericKnown, SizeOf(FGenericKnown), 0);
  Result := True;
end;


function TSVGFreeTypeProvider.AddFontPath(const aDirectory: String): Integer;

var
  lSearch: TSearchRec;
  lPath, lName: String;

begin
  Result := 0;
  if not FAvailable or (aDirectory = '') then
    Exit;
  lPath := IncludeTrailingPathDelimiter(aDirectory);
  if FindFirst(lPath + '*', faAnyFile, lSearch) <> 0 then
    Exit;
  try
    repeat
      lName := lSearch.Name;
      if (lName = '.') or (lName = '..') then
        Continue;
      if (lSearch.Attr and faDirectory) <> 0 then
        begin
        Inc(Result, AddFontPath(lPath + lName));
        Continue;
        end;
      if IsFontFile(lName) and AddFontFile(lPath + lName) then
        Inc(Result);
    until FindNext(lSearch) <> 0;
  finally
    FindClose(lSearch);
  end;
end;


function TSVGFreeTypeProvider.AddSystemFonts: Integer;

var
  lPaths: TStringList;
  I: Integer;

begin
  Result := 0;
  lPaths := TStringList.Create;
  try
    lPaths.Text := SVGSystemFontPaths;
    for I := 0 to lPaths.Count - 1 do
      if DirectoryExists(lPaths[I]) then
        Inc(Result, AddFontPath(lPaths[I]));
  finally
    lPaths.Free;
  end;
end;


function TSVGFreeTypeProvider.FaceOf(aIndex: Integer): PFT_Face;

var
  lFace: PFT_Face;
  lBytes: RawByteString;

begin
  Result := FEntries[aIndex].Face;
  if Result <> nil then
    Exit;
  lFace := nil;
  lBytes := SVGFileNameBytes(FEntries[aIndex].FileName);
  if FT_New_Face(FLibrary, PAnsiChar(lBytes), 0,
                 lFace) <> 0 then
    Exit;
  FEntries[aIndex].Face := lFace;
  Result := lFace;
end;


// How far to thicken a face used in place of a bold it does not have, in
// the design units of that face. Zero when the face is bold enough
// already, and when no bold was requested.
function BoldFor(const aEntry: TSVGFontEntry; const aRequest: TSVGFontRequest;
  aFace: PFT_Face): Double;

var
  lPerEm: Integer;

begin
  Result := 0;
  if (aRequest.Weight < SVGBoldWeightFrom)
     or (aEntry.Weight >= SVGBoldWeightFrom) then
    Exit;
  lPerEm := aFace^.units_per_EM;
  if lPerEm <= 0 then
    lPerEm := 1000;
  Result := lPerEm * SVGSyntheticBold;
end;


// How far to lean a face used in place of an italic it does not have.
// Zero when the face is already slanted, and when none was requested.
function SlantFor(const aEntry: TSVGFontEntry;
  const aRequest: TSVGFontRequest): Double;

begin
  Result := 0;
  if (aRequest.Style <> fnNormal) and (aEntry.Style = fnNormal) then
    Result := SVGObliqueSlant;
end;


// How far a face is from the request. The slant counts for more than the
// width, and the width for more than the weight, so a face of the right
// slant in the wrong weight wins over an upright face of the right
// weight. A family without any italic still answers with the face of the
// right weight and width, which is then slanted here.
function FaceGap(const aEntry: TSVGFontEntry; aWeight: Integer;
  aStyle: TSVGFontStyle; aStretch: TSVGFontStretch): Integer;

begin
  Result := Abs(Ord(aEntry.Stretch) - Ord(aStretch)) * 1000
    + Abs(aEntry.Weight - aWeight);
  if (aEntry.Style <> fnNormal) <> (aStyle <> fnNormal) then
    Result := Result + 100000;
end;


function TSVGFreeTypeProvider.IndexOfFamily(const aFamily: String;
  aWeight: Integer; aStyle: TSVGFontStyle;
  aStretch: TSVGFontStretch): Integer;

var
  I, lNear, lNearScore, lScore: Integer;

begin
  Result := -1;
  lNear := -1;
  lNearScore := High(Integer);
  for I := 0 to FEntryCount - 1 do
    begin
    if not SameText(FEntries[I].Family, aFamily) then
      Continue;
    lScore := FaceGap(FEntries[I], aWeight, aStyle, aStretch);
    if lScore < lNearScore then
      begin
      lNearScore := lScore;
      lNear := I;
      end;
    end;
  Result := lNear;
end;


// The entry that last covered a code point, -2 when it was never looked
// up.
function TSVGFreeTypeProvider.CachedCover(aCodePoint: Cardinal): Integer;

var
  I: Integer;

begin
  Result := -2;
  for I := 0 to FCoverCount - 1 do
    if FCovers[I].CodePoint = aCodePoint then
      Exit(FCovers[I].Entry);
end;


procedure TSVGFreeTypeProvider.RememberCover(aCodePoint: Cardinal;
  aIndex: Integer);

begin
  if FCoverCount = Length(FCovers) then
    SetLength(FCovers, Max(16, FCoverCount * 2));
  FCovers[FCoverCount].CodePoint := aCodePoint;
  FCovers[FCoverCount].Entry := aIndex;
  Inc(FCoverCount);
end;


// The first family of the request that holds a glyph for the code point.
// CSS runs the list again for a character the face of the run cannot
// draw, so the families of the document are tried before anything the
// system happens to have.
function TSVGFreeTypeProvider.IndexOfCovering(
  const aRequest: TSVGFontRequest; aCodePoint: Cardinal): Integer;

var
  lPosition, lIndex: Integer;
  lFamily: String;
  lGeneric: TSVGGenericFamily;
  lFace: PFT_Face;

begin
  Result := -1;
  lPosition := 1;
  while lPosition <= Length(aRequest.Families) do
    begin
    lFamily := SVGNextFamily(aRequest.Families, lPosition);
    if lFamily = '' then
      Continue;
    lGeneric := SVGGenericFamilyOf(lFamily);
    if lGeneric = gfNone then
      lIndex := IndexOfFamily(lFamily, aRequest.Weight, aRequest.Style,
        aRequest.Stretch)
    else
      lIndex := IndexOfGeneric(lGeneric, aRequest.Weight, aRequest.Style,
        aRequest.Stretch);
    if lIndex < 0 then
      Continue;
    lFace := FaceOf(lIndex);
    if (lFace <> nil) and (FT_Get_Char_Index(lFace, aCodePoint) <> 0) then
      Exit(lIndex);
    end;
end;


function TSVGFreeTypeProvider.ResolveCover(aCodePoint: Cardinal;
  const aRequest: TSVGFontRequest): ISVGFont;

var
  lIndex: Integer;
  lFile: String;
  lFace: PFT_Face;
  lFont: TSVGFreeTypeFont;

begin
  Result := nil;
  if not FAvailable or (aRequest.Size <= 0) then
    Exit;
  lIndex := IndexOfCovering(aRequest, aCodePoint);
  if lIndex >= 0 then
    begin
    lFace := FaceOf(lIndex);
    if lFace = nil then
      Exit;
    lFont := TSVGFreeTypeFont.Create(lFace, FEntries[lIndex].Family,
      aRequest.Size, SlantFor(FEntries[lIndex], aRequest),
      BoldFor(FEntries[lIndex], aRequest, lFace), FKerning);
    FFonts.Add(lFont);
    Exit(lFont);
    end;
  // No family of the document holds it, so the system is asked for a face
  // that does. That answer is the same for every request, so it is
  // remembered under the code point alone.
  lIndex := CachedCover(aCodePoint);
  if lIndex = -2 then
    begin
    lIndex := -1;
    if FCoverage <> nil then
      begin
      lFile := FCoverage.CoverFor(aCodePoint, aRequest);
      if (lFile <> '') and AddFontFile(lFile) then
        lIndex := FEntryCount - 1;
      end;
    RememberCover(aCodePoint, lIndex);
    end;
  if lIndex < 0 then
    Exit;
  lFace := FaceOf(lIndex);
  if lFace = nil then
    Exit;
  lFont := TSVGFreeTypeFont.Create(lFace, FEntries[lIndex].Family,
    aRequest.Size, SlantFor(FEntries[lIndex], aRequest),
    BoldFor(FEntries[lIndex], aRequest, lFace), FKerning);
  FFonts.Add(lFont);
  Result := lFont;
end;


function TSVGFreeTypeProvider.AddFontResource(const aFamily: TSVGString;
  aWeight: Integer; aStyle: TSVGFontStyle;
  const aFileName: String): Boolean;

begin
  Result := AddFontFile(aFileName);
  if not Result then
    Exit;
  // The name the document gives the font takes precedence over the name
  // in the face. A stylesheet requesting ZC must resolve even though the
  // face calls itself ZalamanderCaps.
  FEntries[FEntryCount - 1].Family := aFamily;
  FEntries[FEntryCount - 1].Weight := aWeight;
  FEntries[FEntryCount - 1].Style := aStyle;
  FillChar(FGenericKnown, SizeOf(FGenericKnown), 0);
end;


function TSVGFreeTypeProvider.HasFamily(const aFamily: String): Boolean;

var
  I: Integer;

begin
  Result := False;
  for I := 0 to FEntryCount - 1 do
    if SameText(FEntries[I].Family, aFamily) then
      Exit(True);
end;


// The first recorded family whose name contains aWanted and not aAvoid.
function TSVGFreeTypeProvider.FamilyLike(const aWanted,
  aAvoid: String): String;

var
  I: Integer;
  lName: String;

begin
  Result := '';
  for I := 0 to FEntryCount - 1 do
    begin
    lName := LowerCase(FEntries[I].Family);
    if Pos(aWanted, lName) = 0 then
      Continue;
    if (aAvoid <> '') and (Pos(aAvoid, lName) > 0) then
      Continue;
    Exit(FEntries[I].Family);
    end;
end;


function TSVGFreeTypeProvider.GetGenericFamily(
  aGeneric: TSVGGenericFamily): String;

  function FirstPresent(const aNames: array of String): String;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to High(aNames) do
      if HasFamily(aNames[I]) then
        Exit(aNames[I]);
  end;

begin
  if FGenericKnown[aGeneric] then
    Exit(FGeneric[aGeneric]);
  case aGeneric of
    gfSerif:
      begin
      Result := FirstPresent(SerifFamilies);
      if Result = '' then
        Result := FamilyLike('serif', 'sans');
      end;
    gfSansSerif:
      begin
      Result := FirstPresent(SansFamilies);
      if Result = '' then
        Result := FamilyLike('sans', 'mono');
      end;
    gfMonospace:
      begin
      Result := FirstPresent(MonoFamilies);
      if Result = '' then
        Result := FamilyLike('mono', '');
      end;
    gfCursive:
      Result := FirstPresent(CursiveFamilies);
    gfFantasy:
      Result := FirstPresent(FantasyFamilies);
  else
    Result := '';
  end;
  // The two decorative generics stand for faces a system need not have,
  // and CSS lets either one resolve to anything.
  if (Result = '') and (aGeneric = gfCursive) then
    Result := GetGenericFamily(gfSerif);
  if (Result = '') and (aGeneric = gfFantasy) then
    Result := GetGenericFamily(gfSansSerif);
  FGeneric[aGeneric] := Result;
  FGenericKnown[aGeneric] := True;
end;


procedure TSVGFreeTypeProvider.SetGenericFamily(aGeneric: TSVGGenericFamily;
  const aValue: String);

begin
  FGeneric[aGeneric] := aValue;
  FGenericKnown[aGeneric] := True;
end;


function TSVGFreeTypeProvider.IndexOfGeneric(aGeneric: TSVGGenericFamily;
  aWeight: Integer; aStyle: TSVGFontStyle;
  aStretch: TSVGFontStretch): Integer;

var
  lFamily: String;

begin
  Result := -1;
  lFamily := GetGenericFamily(aGeneric);
  if lFamily <> '' then
    Result := IndexOfFamily(lFamily, aWeight, aStyle, aStretch);
end;


// The first recorded file whose face has the weight and style wanted.
function TSVGFreeTypeProvider.IndexOfStyle(aWeight: Integer;
  aStyle: TSVGFontStyle; aStretch: TSVGFontStretch): Integer;

var
  I, lNear, lNearScore, lScore: Integer;

begin
  Result := -1;
  lNear := -1;
  lNearScore := High(Integer);
  for I := 0 to FEntryCount - 1 do
    begin
    lScore := FaceGap(FEntries[I], aWeight, aStyle, aStretch);
    if lScore < lNearScore then
      begin
      lNearScore := lScore;
      lNear := I;
      end;
    end;
  Result := lNear;
end;


function TSVGFreeTypeProvider.ResolveFont(
  const aRequest: TSVGFontRequest): ISVGFont;

var
  lIndex, lPosition: Integer;
  lFamily: String;
  lFont: TSVGFreeTypeFont;
  lFace: PFT_Face;
  lGeneric: TSVGGenericFamily;

begin
  Result := nil;
  if not FAvailable or (FEntryCount = 0) or (aRequest.Size <= 0) then
    Exit;
  lIndex := -1;
  lPosition := 1;
  while (lIndex < 0) and (lPosition <= Length(aRequest.Families)) do
    begin
    lFamily := SVGNextFamily(aRequest.Families, lPosition);
    if lFamily <> '' then
      begin
      lGeneric := SVGGenericFamilyOf(lFamily);
      if lGeneric = gfNone then
        lIndex := IndexOfFamily(lFamily, aRequest.Weight, aRequest.Style,
          aRequest.Stretch)
      else
        lIndex := IndexOfGeneric(lGeneric, aRequest.Weight, aRequest.Style,
          aRequest.Stretch);
      end;
    end;
  if (lIndex < 0) and (FDefaultFamily <> '') then
    lIndex := IndexOfFamily(FDefaultFamily, aRequest.Weight, aRequest.Style,
      aRequest.Stretch);
  // No name in the list is recorded. The kind of the first known name
  // decides: Georgia gives a serif, Arial gives a sans.
  lPosition := 1;
  while (lIndex < 0) and (lPosition <= Length(aRequest.Families)) do
    begin
    lFamily := SVGNextFamily(aRequest.Families, lPosition);
    if lFamily <> '' then
      begin
      lGeneric := SVGFamilyKindOf(lFamily);
      if lGeneric <> gfNone then
        lIndex := IndexOfGeneric(lGeneric, aRequest.Weight, aRequest.Style,
          aRequest.Stretch);
      end;
    end;
  // An unknown family falls back to the face used for sans-serif, which is
  // also the face for a document that gives no family at all.
  if lIndex < 0 then
    lIndex := IndexOfGeneric(gfSansSerif, aRequest.Weight, aRequest.Style,
      aRequest.Stretch);
  // Failing that, the requested weight and style still narrow the
  // choice.
  if lIndex < 0 then
    lIndex := IndexOfStyle(aRequest.Weight, aRequest.Style, aRequest.Stretch);
  if lIndex < 0 then
    lIndex := 0;
  lFace := FaceOf(lIndex);
  if lFace = nil then
    Exit;
  lFont := TSVGFreeTypeFont.Create(lFace, FEntries[lIndex].Family,
    aRequest.Size, SlantFor(FEntries[lIndex], aRequest),
    BoldFor(FEntries[lIndex], aRequest, lFace), FKerning);
  FFonts.Add(lFont);
  Result := lFont;
end;


end.
