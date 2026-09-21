{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Font provider drawing on the GDI font calls of Windows.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.gdi;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

{$IFNDEF WINDOWS}
{$FATAL This unit builds for Windows only. Use fpsvg.fonts.provider for the font provider of the platform being built for.}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, fpsvg.types;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, fpsvg.types;
{$ENDIF FPC_DOTTEDUNITS}

type
  ESVGGDIFonts = class(ESVGError);

  { One face of the system at one size, drawn through GDI. Every measure it
    reports is in user units at that size. The face is held in a memory
    device context of its own, the outline calls reading whatever is
    selected into one. }
  TSVGGDIFont = class(TObject, ISVGFont)
  private
    FDC: THandle;
    FFont: THandle;
    FPrevious: THandle;
    FName: String;
    FSize: Double;
    FScale: Double;
    FUnitsPerEm: Integer;
    FAscent: Double;
    FDescent: Double;
    FUnderlinePosition: Double;
    FUnderlineThickness: Double;
    FKerning: Boolean;
    FPairsRead: Boolean;
    FPairs: array of record
      Left, Right : Word;
      Amount      : Double;
    end;
    procedure ReadPairs;
  public
    { Wraps a font created at its own em square, so that the outline calls
      answer in design units. The device context and the font are freed
      with this object. }
    constructor Create(aDC, aFont, aPrevious: THandle; const aName: String;
      aSize: Double; aUnitsPerEm: Integer; aKerning: Boolean);
    destructor Destroy; override;
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
    procedure GetGlyphVerticalOrigin(aGlyph: Cardinal; out aX, aY: Double);
    function GetGlyphKerning(aLeft, aRight: Cardinal): Double;
    function GetGlyphVerticalKerning(aAbove, aBelow: Cardinal): Double;
    function GetSmallCaps: Boolean;
    function GetGlyphOutline(aGlyph: Cardinal; aPath: TSVGPath): Boolean;
    // The metrics of the face, in design units.
    property Ascent: Double read FAscent write FAscent;
    property Descent: Double read FDescent write FDescent;
    property UnderlinePosition: Double read FUnderlinePosition
      write FUnderlinePosition;
    property UnderlineThickness: Double read FUnderlineThickness
      write FUnderlineThickness;
  end;

  { A family a font file was registered under: the name the document gives
    it, and the name Windows knows it by. }
  TSVGGDIResource = record
    Declared : String;
    Actual   : String;
    FileName : String;
    Weight   : Integer;
    Style    : TSVGFontStyle;
  end;
  TSVGGDIResourceArray = array of TSVGGDIResource;

  { Resolves a font request through GDI, which is part of Windows and needs
    no library beside it. A character that no family of the request covers
    is looked for in the families of the system, the ones Windows falls
    back to first. }
  TSVGGDIProvider = class(TObject, ISVGFontProvider)
  private
    FAvailable: Boolean;
    FFonts: TFPList;
    FResources: TSVGGDIResourceArray;
    FResourceCount: Integer;
    FDefaultFamily: String;
    FGeneric: array[TSVGGenericFamily] of String;
    FFamilies: TStringList;
    FKerning: Boolean;
    FCovers: TStringList;
    function SystemFamilies: TStringList;
    function FaceOfFamily(const aFamily: String;
      const aRequest: TSVGFontRequest): TSVGGDIFont;
    function ResourceOf(const aFamily: String; aWeight: Integer;
      aStyle: TSVGFontStyle): String;
    function CoveringFamily(aCodePoint: Cardinal;
      const aRequest: TSVGFontRequest): String;
    function GetGenericFamily(aGeneric: TSVGGenericFamily): String;
    procedure SetGenericFamily(aGeneric: TSVGGenericFamily;
      const aValue: String);
    function GetResource(aIndex: Integer): TSVGGDIResource;
    function GetFamilyCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    // Hands a font file to Windows for the life of this process. False
    // when it holds no face that can be read.
    function AddFontFile(const aFileName: String): Boolean;
    // The same, under the family, weight and style given, which is the
    // name a request for it comes in under.
    function AddFontResource(const aFamily: TSVGString; aWeight: Integer;
      aStyle: TSVGFontStyle; const aFileName: String): Boolean;
    // Registers every font file in a directory. Returns how many were
    // added.
    function AddFontPath(const aDirectory: String): Integer;
    // Counts the families the system holds. Windows reads no directory of
    // its own, so nothing is loaded here.
    function AddSystemFonts: Integer;
    function ResolveFont(const aRequest: TSVGFontRequest): ISVGFont;
    // The same face as the object behind it, which a caller wanting the
    // device context or the metrics needs. Nil when nothing resolves.
    function ResolveFace(const aRequest: TSVGFontRequest): TSVGGDIFont;
    function ResolveCover(aCodePoint: Cardinal;
      const aRequest: TSVGFontRequest): ISVGFont;
    // Always True: GDI is part of Windows.
    property Available: Boolean read FAvailable;
    // Number of families the system holds, zero until they are counted.
    property FamilyCount: Integer read GetFamilyCount;
    // Number of files handed to Windows by this provider.
    property ResourceCount: Integer read FResourceCount;
    // A registered file, by index.
    property Resources[aIndex: Integer]: TSVGGDIResource read GetResource;
    // Family used when a requested font cannot be found.
    property DefaultFamily: String read FDefaultFamily write FDefaultFamily;
    { The family used for a generic name. Reading gives the face Windows
      ships for it; writing sets it explicitly. }
    property GenericFamily[aGeneric: TSVGGenericFamily]: String
      read GetGenericFamily write SetGenericFamily;
    { Draw a pair of glyphs closer together according to the kern table of
      the face. False by default, as it is for freetype. }
    property Kerning: Boolean read FKerning write FKerning;
  end;

// True when this build can resolve fonts through GDI.
function SVGGDIFontsAvailable: Boolean;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses WinApi.Windows;
{$ELSE FPC_DOTTEDUNITS}
uses windows;
{$ENDIF FPC_DOTTEDUNITS}

const
  { Calls and values the RTL header does not declare. }
  GGO_GLYPH_INDEX = $0080;
  GGI_MARK_NONEXISTING_GLYPHS = $0001;
  FR_PRIVATE = $10;
  TT_PRIM_CSPLINE = 3;
  GDIError = DWORD($FFFFFFFF);

function GetGlyphIndicesW(aDC: HDC; aText: LPCWSTR; aCount: Integer;
  aIndices: PWord; aFlags: DWORD): DWORD; stdcall;
  external 'gdi32' name 'GetGlyphIndicesW';
function GetCharWidthI(aDC: HDC; aFirst, aCount: UINT; aIndices: PWord;
  aWidths: PInteger): WINBOOL; stdcall;
  external 'gdi32' name 'GetCharWidthI';
function AddFontResourceExW(aFileName: LPCWSTR; aFlags: DWORD;
  aReserved: Pointer): Integer; stdcall;
  external 'gdi32' name 'AddFontResourceExW';
function RemoveFontResourceExW(aFileName: LPCWSTR; aFlags: DWORD;
  aReserved: Pointer): WINBOOL; stdcall;
  external 'gdi32' name 'RemoveFontResourceExW';

const
  // The height a face is first created at, before its em square is known.
  ProbeHeight = 1000;
  // From this weight up a face counts as bold.
  SVGBoldWeightFrom = 600;
  { The families Windows ships for each generic name. }
  GenericFamilies: array[TSVGGenericFamily] of String = (
    '', 'Times New Roman', 'Arial', 'Comic Sans MS', 'Impact',
    'Courier New');
  { The families Windows itself falls back through, best first. A
    character no requested family holds is looked for in these before the
    rest of the system. }
  FallbackFamilies: array[0..9] of String = (
    'Segoe UI', 'Segoe UI Symbol', 'Segoe UI Emoji', 'Arial Unicode MS',
    'Microsoft YaHei', 'Meiryo', 'MS Gothic', 'SimSun', 'Malgun Gothic',
    'Nirmala UI');


function SVGGDIFontsAvailable: Boolean;

begin
  Result := True;
end;


// The value of a 16.16 fixed point number.
function FixedValue(const aFixed: FIXED): Double;

begin
  Result := aFixed.value + aFixed.fract / 65536;
end;


// The code point as the one or two UTF-16 units GDI takes. Returns how
// many were written.
function UnitsOfCodePoint(aCodePoint: Cardinal;
  var aUnits: array of WideChar): Integer;

begin
  if aCodePoint > $FFFF then
    begin
    aUnits[0] := WideChar($D800 + ((aCodePoint - $10000) shr 10));
    aUnits[1] := WideChar($DC00 + ((aCodePoint - $10000) and $3FF));
    Result := 2;
    end
  else
    begin
    aUnits[0] := WideChar(aCodePoint);
    Result := 1;
    end;
end;


// Adds the family of an enumerated face to the list the walk fills.
function EnumFamily(var aLogFont: ENUMLOGFONTEXW;
  var aMetric: NEWTEXTMETRICEXW; aType: longint; aData: LPARAM): longint;
  stdcall;

var
  lName: TSVGString;
  lFamilies: TStringList;

begin
  Result := 1;
  lFamilies := TStringList(PtrUInt(aData));
  lName := UTF8Encode(UnicodeString(
    PWideChar(@aLogFont.elfLogFont.lfFaceName[0])));
  // A family whose name starts with an at sign is the vertical writing
  // form of another. It draws the same characters turned on their side.
  if (lName <> '') and (lName[1] <> '@')
     and (lFamilies.IndexOf(lName) < 0) then
    lFamilies.Add(lName);
end;


// The families the system holds, one per line.
procedure ReadFamilies(aInto: TStringList);

var
  lDC: HDC;
  lLogFont: LOGFONTW;

begin
  aInto.Clear;
  lDC := CreateCompatibleDC(0);
  if lDC = 0 then
    Exit;
  try
    FillChar(lLogFont, SizeOf(lLogFont), 0);
    lLogFont.lfCharSet := DEFAULT_CHARSET;
    EnumFontFamiliesExW(lDC, @lLogFont, @EnumFamily,
      LPARAM(PtrUInt(aInto)), 0);
  finally
    DeleteDC(lDC);
  end;
end;


{ TSVGGDIFont }

constructor TSVGGDIFont.Create(aDC, aFont, aPrevious: THandle;
  const aName: String; aSize: Double; aUnitsPerEm: Integer;
  aKerning: Boolean);

begin
  inherited Create;
  FDC := aDC;
  FFont := aFont;
  FPrevious := aPrevious;
  FName := aName;
  FSize := aSize;
  FUnitsPerEm := aUnitsPerEm;
  if FUnitsPerEm <= 0 then
    FUnitsPerEm := 1000;
  FScale := FSize / FUnitsPerEm;
  FKerning := aKerning;
end;


destructor TSVGGDIFont.Destroy;

begin
  if (FDC <> 0) and (FPrevious <> 0) then
    SelectObject(FDC, FPrevious);
  if FFont <> 0 then
    DeleteObject(FFont);
  if FDC <> 0 then
    DeleteDC(FDC);
  inherited Destroy;
end;


function TSVGGDIFont.GetFontName: TSVGString;

begin
  Result := FName;
end;


function TSVGGDIFont.GetUnitsPerEm: Integer;

begin
  Result := FUnitsPerEm;
end;


function TSVGGDIFont.GetSize: Double;

begin
  Result := FSize;
end;


function TSVGGDIFont.GetAscent: Double;

begin
  Result := FAscent * FScale;
end;


function TSVGGDIFont.GetDescent: Double;

begin
  Result := FDescent * FScale;
end;


function TSVGGDIFont.GetUnderlinePosition: Double;

begin
  // GDI counts the position up from the baseline, and a caller here counts
  // it down the page.
  Result := -FUnderlinePosition * FScale;
end;


function TSVGGDIFont.GetUnderlineThickness: Double;

begin
  Result := FUnderlineThickness * FScale;
end;


function TSVGGDIFont.GetGlyphIndex(aCodePoint: Cardinal): Cardinal;

var
  lUnits: array[0..1] of WideChar;
  lIndices: array[0..1] of Word;
  lCount: Integer;

begin
  Result := 0;
  // GDI maps one UTF-16 unit at a time, so a code point above the basic
  // plane cannot be asked about this way.
  if aCodePoint > $FFFF then
    Exit;
  lCount := UnitsOfCodePoint(aCodePoint, lUnits);
  lIndices[0] := $FFFF;
  if GetGlyphIndicesW(FDC, @lUnits[0], lCount, @lIndices[0],
                      GGI_MARK_NONEXISTING_GLYPHS) = GDIError then
    Exit;
  if lIndices[0] = $FFFF then
    Exit;
  Result := lIndices[0];
end;


function TSVGGDIFont.GetGlyphForRun(const aCodes: TSVGCodePointArray;
  aFrom: Integer; out aCount: Integer): Cardinal;

begin
  // GDI forms no ligatures of its own, that being the work of a shaping
  // engine, so one code point is drawn at a time.
  aCount := 0;
  Result := 0;
  if (aFrom < 0) or (aFrom > High(aCodes)) then
    Exit;
  Result := GetGlyphIndex(aCodes[aFrom]);
  if Result <> 0 then
    aCount := 1;
end;


function TSVGGDIFont.GetGlyphNamed(const aName: TSVGString): Cardinal;

begin
  // GDI gives no way to reach a glyph by its name.
  Result := 0;
end;


function TSVGGDIFont.GetGlyphAdvance(aGlyph: Cardinal): Double;

var
  lIndex: Word;
  lWidth: Integer;

begin
  Result := 0;
  lIndex := Word(aGlyph);
  lWidth := 0;
  if not GetCharWidthI(FDC, 0, 1, @lIndex, @lWidth) then
    Exit;
  Result := lWidth * FScale;
end;


function TSVGGDIFont.GetGlyphVerticalAdvance(aGlyph: Cardinal): Double;

begin
  // GDI keeps the vertical metrics of a face in its own vertical form,
  // which is a family of its own here, so the em is used instead.
  Result := FSize;
end;


procedure TSVGGDIFont.GetGlyphVerticalOrigin(aGlyph: Cardinal;
  out aX, aY: Double);

begin
  aX := -GetGlyphAdvance(aGlyph) / 2;
  aY := GetAscent;
end;


procedure TSVGGDIFont.ReadPairs;

var
  lPairs: array of KERNINGPAIR;
  lCount, I: DWORD;

begin
  FPairsRead := True;
  lCount := GetKerningPairsW(FDC, 0, nil);
  if (lCount = 0) or (lCount > 65536) then
    Exit;
  SetLength(lPairs, lCount);
  if GetKerningPairsW(FDC, lCount, @lPairs[0]) = 0 then
    Exit;
  SetLength(FPairs, lCount);
  for I := 0 to lCount - 1 do
    begin
    // The pairs name characters, and a glyph is what a caller asks about.
    FPairs[I].Left := Word(GetGlyphIndex(lPairs[I].wFirst));
    FPairs[I].Right := Word(GetGlyphIndex(lPairs[I].wSecond));
    FPairs[I].Amount := lPairs[I].iKernAmount;
    end;
end;


function TSVGGDIFont.GetGlyphKerning(aLeft, aRight: Cardinal): Double;

var
  I: Integer;

begin
  Result := 0;
  if not FKerning then
    Exit;
  if not FPairsRead then
    ReadPairs;
  for I := 0 to High(FPairs) do
    if (FPairs[I].Left = aLeft) and (FPairs[I].Right = aRight) then
      // The table says which way the pen moves; this says how much closer
      // the pair is drawn, which is the opposite sign.
      Exit(-FPairs[I].Amount * FScale);
end;


function TSVGGDIFont.GetGlyphVerticalKerning(aAbove,
  aBelow: Cardinal): Double;

begin
  Result := 0;
end;


function TSVGGDIFont.GetSmallCaps: Boolean;

begin
  Result := False;
end;


function TSVGGDIFont.GetGlyphOutline(aGlyph: Cardinal;
  aPath: TSVGPath): Boolean;

var
  lMetrics: GLYPHMETRICS;
  lMatrix: MAT2;
  CurveHeader: PtrUInt;
  lSize: DWORD;
  lBuffer: array of Byte;
  lBase, lAt, lEnd, lCurveAt: PtrUInt;
  lHeader: ^TTPOLYGONHEADER;
  lCurve: ^TTPOLYCURVE;
  lPoints: PPOINTFX;
  I: Integer;
  lStartX, lStartY, lX, lY, lCX, lCY, lNextX, lNextY: Double;
  lC1X, lC1Y, lC2X, lC2Y: Double;

  procedure PointOf(aIndex: Integer; out aPX, aPY: Double);
  var
    lPoint: PPOINTFX;
  begin
    lPoint := lPoints;
    Inc(lPoint, aIndex);
    aPX := FixedValue(lPoint^.x);
    aPY := -FixedValue(lPoint^.y);
  end;

begin
  Result := False;
  CurveHeader := SizeOf(TTPOLYCURVE) - SizeOf(POINTFX);
  FillChar(lMatrix, SizeOf(lMatrix), 0);
  lMatrix.eM11.value := 1;
  lMatrix.eM22.value := 1;
  FillChar(lMetrics, SizeOf(lMetrics), 0);
  lSize := GetGlyphOutlineW(FDC, aGlyph, GGO_NATIVE or GGO_GLYPH_INDEX,
    @lMetrics, 0, nil, @lMatrix);
  if (lSize = GDIError) or (lSize = 0) then
    Exit;
  SetLength(lBuffer, lSize);
  if GetGlyphOutlineW(FDC, aGlyph, GGO_NATIVE or GGO_GLYPH_INDEX,
                      @lMetrics, lSize, @lBuffer[0], @lMatrix) = GDIError then
    Exit;
  lBase := PtrUInt(@lBuffer[0]);
  lAt := lBase;
  while lAt + SizeOf(TTPOLYGONHEADER) <= lBase + lSize do
    begin
    lHeader := Pointer(lAt);
    if (lHeader^.cb < SizeOf(TTPOLYGONHEADER))
       or (lAt + lHeader^.cb > lBase + lSize) then
      Break;
    lEnd := lAt + lHeader^.cb;
    lPoints := @lHeader^.pfxStart;
    PointOf(0, lStartX, lStartY);
    aPath.MoveTo(lStartX, lStartY);
    lCurveAt := lAt + SizeOf(TTPOLYGONHEADER);
    while lCurveAt + CurveHeader <= lEnd do
      begin
      lCurve := Pointer(lCurveAt);
      lPoints := @lCurve^.apfx[0];
      case lCurve^.wType of
        TT_PRIM_LINE:
          for I := 0 to lCurve^.cpfx - 1 do
            begin
            PointOf(I, lX, lY);
            aPath.LineTo(lX, lY);
            end;
        TT_PRIM_QSPLINE:
          // Every point but the last is a control point, and the curve
          // passes through the middle of each pair of them.
          for I := 0 to Integer(lCurve^.cpfx) - 2 do
            begin
            PointOf(I, lCX, lCY);
            PointOf(I + 1, lNextX, lNextY);
            if I < Integer(lCurve^.cpfx) - 2 then
              begin
              lX := (lCX + lNextX) / 2;
              lY := (lCY + lNextY) / 2;
              end
            else
              begin
              lX := lNextX;
              lY := lNextY;
              end;
            aPath.QuadTo(lCX, lCY, lX, lY);
            end;
        TT_PRIM_CSPLINE:
          begin
          I := 0;
          while I + 2 < Integer(lCurve^.cpfx) do
            begin
            PointOf(I, lC1X, lC1Y);
            PointOf(I + 1, lC2X, lC2Y);
            PointOf(I + 2, lX, lY);
            aPath.CubicTo(lC1X, lC1Y, lC2X, lC2Y, lX, lY);
            Inc(I, 3);
            end;
          end;
      end;
      lCurveAt := lCurveAt + CurveHeader
                + PtrUInt(lCurve^.cpfx) * SizeOf(POINTFX);
      end;
    aPath.Close;
    Result := True;
    lAt := lEnd;
    end;
end;


{ TSVGGDIProvider }

constructor TSVGGDIProvider.Create;

begin
  inherited Create;
  FAvailable := SVGGDIFontsAvailable;
  FFonts := TFPList.Create;
  FCovers := TStringList.Create;
end;


destructor TSVGGDIProvider.Destroy;

var
  I: Integer;
  lName: UnicodeString;

begin
  if FFonts <> nil then
    begin
    for I := 0 to FFonts.Count - 1 do
      TSVGGDIFont(FFonts[I]).Free;
    FreeAndNil(FFonts);
    end;
  for I := 0 to FResourceCount - 1 do
    begin
    lName := UnicodeString(FResources[I].FileName);
    RemoveFontResourceExW(PWideChar(lName), FR_PRIVATE, nil);
    end;
  FreeAndNil(FFamilies);
  FreeAndNil(FCovers);
  inherited Destroy;
end;


function TSVGGDIProvider.GetResource(aIndex: Integer): TSVGGDIResource;

begin
  Result := FResources[aIndex];
end;


function TSVGGDIProvider.GetFamilyCount: Integer;

begin
  Result := 0;
  if FFamilies <> nil then
    Result := FFamilies.Count;
end;


function TSVGGDIProvider.GetGenericFamily(
  aGeneric: TSVGGenericFamily): String;

begin
  Result := FGeneric[aGeneric];
  if Result = '' then
    Result := GenericFamilies[aGeneric];
end;


procedure TSVGGDIProvider.SetGenericFamily(aGeneric: TSVGGenericFamily;
  const aValue: String);

begin
  FGeneric[aGeneric] := aValue;
end;


function TSVGGDIProvider.SystemFamilies: TStringList;

begin
  if FFamilies = nil then
    begin
    FFamilies := TStringList.Create;
    FFamilies.CaseSensitive := False;
    ReadFamilies(FFamilies);
    end;
  Result := FFamilies;
end;


function TSVGGDIProvider.ResourceOf(const aFamily: String;
  aWeight: Integer; aStyle: TSVGFontStyle): String;

var
  I: Integer;

begin
  Result := '';
  for I := 0 to FResourceCount - 1 do
    if SameText(FResources[I].Declared, aFamily)
       and (FResources[I].Style = aStyle)
       and ((FResources[I].Weight >= SVGBoldWeightFrom)
            = (aWeight >= SVGBoldWeightFrom)) then
      Exit(FResources[I].Actual);
  for I := 0 to FResourceCount - 1 do
    if SameText(FResources[I].Declared, aFamily) then
      Exit(FResources[I].Actual);
end;


function TSVGGDIProvider.FaceOfFamily(const aFamily: String;
  const aRequest: TSVGFontRequest): TSVGGDIFont;

var
  lDC: HDC;
  lLogFont: LOGFONTW;
  lFont, lPrevious: THandle;
  lName: UnicodeString;
  lFace: array[0..LF_FACESIZE + 1] of WideChar;
  lMetrics: ^OUTLINETEXTMETRICW;
  lSize: UINT;
  lEm: Integer;

  // Creates the face at a height and selects it into the context.
  function Make(aHeight: Integer): Boolean;
  begin
    Result := False;
    FillChar(lLogFont, SizeOf(lLogFont), 0);
    lLogFont.lfHeight := -aHeight;
    lLogFont.lfWeight := aRequest.Weight;
    if aRequest.Style <> fnNormal then
      lLogFont.lfItalic := 1;
    lLogFont.lfCharSet := DEFAULT_CHARSET;
    lLogFont.lfOutPrecision := OUT_TT_PRECIS;
    lLogFont.lfQuality := PROOF_QUALITY;
    lName := UnicodeString(aFamily);
    if Length(lName) > LF_FACESIZE - 1 then
      SetLength(lName, LF_FACESIZE - 1);
    Move(PWideChar(lName)^, lLogFont.lfFaceName[0],
      Length(lName) * SizeOf(WideChar));
    lFont := CreateFontIndirectW(@lLogFont);
    if lFont = 0 then
      Exit;
    lPrevious := SelectObject(lDC, lFont);
    Result := True;
  end;

begin
  Result := nil;
  if aFamily = '' then
    Exit;
  lDC := CreateCompatibleDC(0);
  if lDC = 0 then
    Exit;
  lFont := 0;
  lPrevious := 0;
  lMetrics := nil;
  try
    if not Make(ProbeHeight) then
      Exit;
    // GDI answers every request with something, so the face it reports
    // back is the only way to tell whether it had this one.
    FillChar(lFace, SizeOf(lFace), 0);
    GetTextFaceW(lDC, LF_FACESIZE, @lFace[0]);
    if not SameText(UTF8Encode(UnicodeString(PWideChar(@lFace[0]))),
                    aFamily) then
      Exit;
    lSize := GetOutlineTextMetricsW(lDC, 0, nil);
    if lSize = 0 then
      Exit;
    lMetrics := GetMem(lSize);
    FillChar(lMetrics^, lSize, 0);
    lMetrics^.otmSize := lSize;
    if GetOutlineTextMetricsW(lDC, lSize, lMetrics) = 0 then
      Exit;
    lEm := lMetrics^.otmEMSquare;
    if lEm <= 0 then
      lEm := ProbeHeight;
    // Made again over its own em square, the outline calls answer in the
    // design units of the face.
    SelectObject(lDC, lPrevious);
    DeleteObject(lFont);
    lFont := 0;
    if not Make(lEm) then
      Exit;
    if GetOutlineTextMetricsW(lDC, lSize, lMetrics) = 0 then
      Exit;
    Result := TSVGGDIFont.Create(lDC, lFont, lPrevious,
      UTF8Encode(UnicodeString(PWideChar(@lFace[0]))), aRequest.Size, lEm,
      FKerning);
    Result.Ascent := lMetrics^.otmAscent;
    Result.Descent := Abs(lMetrics^.otmDescent);
    Result.UnderlinePosition := lMetrics^.otmsUnderscorePosition;
    Result.UnderlineThickness := lMetrics^.otmsUnderscoreSize;
    lDC := 0;
    lFont := 0;
  finally
    if lMetrics <> nil then
      FreeMem(lMetrics);
    if (lDC <> 0) and (lFont <> 0) then
      begin
      SelectObject(lDC, lPrevious);
      DeleteObject(lFont);
      end;
    if lDC <> 0 then
      DeleteDC(lDC);
  end;
  if Result <> nil then
    FFonts.Add(Result);
end;


function TSVGGDIProvider.AddFontFile(const aFileName: String): Boolean;

begin
  Result := AddFontResource('', 0, fnNormal, aFileName);
end;


function TSVGGDIProvider.AddFontResource(const aFamily: TSVGString;
  aWeight: Integer; aStyle: TSVGFontStyle;
  const aFileName: String): Boolean;

var
  lName: UnicodeString;
  lBefore, lAfter: TStringList;
  lActual: String;
  I: Integer;

begin
  Result := False;
  if not FileExists(aFileName) then
    Exit;
  lBefore := TStringList.Create;
  lAfter := TStringList.Create;
  try
    ReadFamilies(lBefore);
    lBefore.CaseSensitive := False;
    lName := UnicodeString(aFileName);
    if AddFontResourceExW(PWideChar(lName), FR_PRIVATE, nil) = 0 then
      Exit;
    Result := True;
    FreeAndNil(FFamilies);
    // Windows gives no name for what it just read, so the families are
    // counted again and the one that appeared is the face of the file.
    ReadFamilies(lAfter);
    lActual := '';
    for I := 0 to lAfter.Count - 1 do
      if lBefore.IndexOf(lAfter[I]) < 0 then
        begin
        lActual := lAfter[I];
        Break;
        end;
    if FResourceCount = Length(FResources) then
      SetLength(FResources, 8 + FResourceCount * 2);
    FResources[FResourceCount].Declared := aFamily;
    FResources[FResourceCount].Actual := lActual;
    FResources[FResourceCount].FileName := aFileName;
    FResources[FResourceCount].Weight := aWeight;
    FResources[FResourceCount].Style := aStyle;
    Inc(FResourceCount);
  finally
    lBefore.Free;
    lAfter.Free;
  end;
end;


function TSVGGDIProvider.AddFontPath(const aDirectory: String): Integer;

var
  lSearch: TSearchRec;
  lPath: String;

begin
  Result := 0;
  lPath := IncludeTrailingPathDelimiter(aDirectory);
  if FindFirst(lPath + '*', faAnyFile, lSearch) <> 0 then
    Exit;
  try
    repeat
      if (lSearch.Attr and faDirectory) = 0 then
        if AddFontFile(lPath + lSearch.Name) then
          Inc(Result);
    until FindNext(lSearch) <> 0;
  finally
    FindClose(lSearch);
  end;
end;


function TSVGGDIProvider.AddSystemFonts: Integer;

begin
  Result := SystemFamilies.Count;
end;


function TSVGGDIProvider.ResolveFace(
  const aRequest: TSVGFontRequest): TSVGGDIFont;

var
  lAt: Integer;
  lFamily, lActual: String;
  lGeneric, lKind: TSVGGenericFamily;

begin
  Result := nil;
  lAt := 1;
  // The list is walked in the order it is written, and the first family
  // the system holds answers.
  repeat
    lFamily := SVGNextFamily(aRequest.Families, lAt);
    if lFamily = '' then
      Break;
    lActual := ResourceOf(lFamily, aRequest.Weight, aRequest.Style);
    if lActual <> '' then
      begin
      Result := FaceOfFamily(lActual, aRequest);
      if Result <> nil then
        Exit;
      end;
    lGeneric := SVGGenericFamilyOf(lFamily);
    if lGeneric <> gfNone then
      lFamily := GetGenericFamily(lGeneric);
    Result := FaceOfFamily(lFamily, aRequest);
    if Result <> nil then
      Exit;
    // A family the system lacks is drawn with a face of the same kind:
    // Georgia gives a serif, Arial a sans.
    lKind := SVGFamilyKindOf(lFamily);
    if lKind <> gfNone then
      begin
      Result := FaceOfFamily(GetGenericFamily(lKind), aRequest);
      if Result <> nil then
        Exit;
      end;
  until False;
  lFamily := FDefaultFamily;
  if lFamily = '' then
    lFamily := GetGenericFamily(gfSansSerif);
  Result := FaceOfFamily(lFamily, aRequest);
end;


function TSVGGDIProvider.ResolveFont(
  const aRequest: TSVGFontRequest): ISVGFont;

var
  lFont: TSVGGDIFont;

begin
  Result := nil;
  lFont := ResolveFace(aRequest);
  if lFont <> nil then
    Result := lFont;
end;


function TSVGGDIProvider.CoveringFamily(aCodePoint: Cardinal;
  const aRequest: TSVGFontRequest): String;

var
  I, lIndex: Integer;
  lKey: String;
  lFace: TSVGGDIFont;

  // True when the family holds a glyph for the code point.
  function Covers(const aFamily: String): Boolean;
  begin
    Result := False;
    lFace := FaceOfFamily(aFamily, aRequest);
    if lFace = nil then
      Exit;
    Result := lFace.GetGlyphIndex(aCodePoint) <> 0;
    FFonts.Remove(lFace);
    lFace.Free;
  end;

begin
  Result := '';
  lKey := IntToStr(aCodePoint);
  lIndex := FCovers.IndexOfName(lKey);
  if lIndex >= 0 then
    Exit(FCovers.ValueFromIndex[lIndex]);
  // The families Windows falls back through are tried before the rest of
  // the system, which is walked in the order it enumerates.
  for I := 0 to High(FallbackFamilies) do
    if Covers(FallbackFamilies[I]) then
      begin
      Result := FallbackFamilies[I];
      Break;
      end;
  if Result = '' then
    for I := 0 to SystemFamilies.Count - 1 do
      if Covers(SystemFamilies[I]) then
        begin
        Result := SystemFamilies[I];
        Break;
        end;
  FCovers.Add(lKey + '=' + Result);
end;


function TSVGGDIProvider.ResolveCover(aCodePoint: Cardinal;
  const aRequest: TSVGFontRequest): ISVGFont;

var
  lFamily: String;
  lFace: TSVGGDIFont;

begin
  Result := nil;
  lFamily := CoveringFamily(aCodePoint, aRequest);
  if lFamily = '' then
    Exit;
  lFace := FaceOfFamily(lFamily, aRequest);
  if lFace = nil then
    Exit;
  Result := lFace;
end;


end.
