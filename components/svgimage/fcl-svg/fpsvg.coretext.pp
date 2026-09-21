{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Font provider drawing on Core Text, the font engine of macOS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.coretext;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

{$IFNDEF DARWIN}
{$FATAL This unit builds for macOS only. Use fpsvg.fonts.provider for the font provider of the platform being built for.}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, fpsvg.types;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, fpsvg.types;
{$ENDIF FPC_DOTTEDUNITS}

type
  ESVGCoreTextFonts = class(ESVGError);

  { One face of the system at one size, resolved through Core Text. Every
    measure it reports is in user units at that size. Slant leans the
    outlines to the right for a face used in place of a missing italic. }
  TSVGCoreTextFont = class(TObject, ISVGFont)
  private
    FFont: Pointer;
    FName: String;
    FSize: Double;
    FSlant: Double;
    FUnitsPerEm: Integer;
  public
    // Wraps a CTFontRef created at aSize. The reference is released with
    // this object.
    constructor Create(aFont: Pointer; const aName: String;
      aSize, aSlant: Double);
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
    // The CTFontRef the face was resolved to.
    property Handle: Pointer read FFont;
    // How far the outlines lean, zero for a face that leans by itself.
    property Slant: Double read FSlant;
  end;

  { A family a font file was registered under: the name the document gives
    it, and the name Core Text knows it by. }
  TSVGCoreTextResource = record
    Declared : String;
    Actual   : String;
    Weight   : Integer;
    Style    : TSVGFontStyle;
  end;
  TSVGCoreTextResourceArray = array of TSVGCoreTextResource;

  { Resolves a font request through Core Text, which is part of macOS and
    needs no library beside it. A character that no family of the request
    covers is drawn with the face Core Text falls back to, so this needs no
    coverage source either. }
  TSVGCoreTextProvider = class(TObject, ISVGFontProvider)
  private
    FAvailable: Boolean;
    FFonts: TFPList;
    FResources: TSVGCoreTextResourceArray;
    FResourceCount: Integer;
    FDefaultFamily: String;
    FGeneric: array[TSVGGenericFamily] of String;
    FFamilyCount: Integer;
    function Keep(aFont: Pointer; const aName: String;
      aSize, aSlant: Double): TSVGCoreTextFont;
    function ResourceOf(const aFamily: String; aWeight: Integer;
      aStyle: TSVGFontStyle): String;
    function FontOfFamily(const aFamily: String;
      const aRequest: TSVGFontRequest; out aResolved: String): Pointer;
    function GetGenericFamily(aGeneric: TSVGGenericFamily): String;
    procedure SetGenericFamily(aGeneric: TSVGGenericFamily;
      const aValue: String);
    function GetResource(aIndex: Integer): TSVGCoreTextResource;
  public
    constructor Create;
    destructor Destroy; override;
    // Hands a font file to Core Text for the life of this process. False
    // when it holds no face that can be read.
    function AddFontFile(const aFileName: String): Boolean;
    // The same, under the family, weight and style given, which is the
    // name a request for it comes in under.
    function AddFontResource(const aFamily: TSVGString; aWeight: Integer;
      aStyle: TSVGFontStyle; const aFileName: String): Boolean;
    // Registers every font file in a directory. Returns how many were
    // added.
    function AddFontPath(const aDirectory: String): Integer;
    // Counts the faces the system holds. Core Text reads no directory of
    // its own, so nothing is loaded here.
    function AddSystemFonts: Integer;
    function ResolveFont(const aRequest: TSVGFontRequest): ISVGFont;
    // The same face as the object behind it, which a caller wanting the
    // Core Text handle or the slant needs. Nil when nothing resolves.
    function ResolveFace(
      const aRequest: TSVGFontRequest): TSVGCoreTextFont;
    function ResolveCover(aCodePoint: Cardinal;
      const aRequest: TSVGFontRequest): ISVGFont;
    // Always True: Core Text is part of macOS.
    property Available: Boolean read FAvailable;
    // Number of faces the system holds, zero until AddSystemFonts.
    property FamilyCount: Integer read FFamilyCount;
    // Number of files handed to Core Text by this provider.
    property ResourceCount: Integer read FResourceCount;
    // A registered file, by index.
    property Resources[aIndex: Integer]: TSVGCoreTextResource
      read GetResource;
    // Family used when a requested font cannot be found.
    property DefaultFamily: String read FDefaultFamily write FDefaultFamily;
    { The family used for a generic name. Reading gives the face macOS
      ships for it; writing sets it explicitly. }
    property GenericFamily[aGeneric: TSVGGenericFamily]: String
      read GetGenericFamily write SetGenericFamily;
  end;

// True when this build can resolve fonts through Core Text.
function SVGCoreTextFontsAvailable: Boolean;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses MacOsApi.MacTypes, MacOsApi.CFBase, MacOsApi.CFError,
     MacOsApi.CFString, MacOsApi.CFURL, MacOsApi.CFArray,
     MacOsApi.CFDictionary, MacOsApi.CFNumber, MacOsApi.CGBase,
     MacOsApi.CGGeometry, MacOsApi.CGFont, MacOsApi.CGPath,
     MacOsApi.CTFont, MacOsApi.CTFontDescriptor,
     MacOsApi.CTFontTraits, MacOsApi.CTFontCollection,
     MacOsApi.CTFontManager;
{$ELSE FPC_DOTTEDUNITS}
uses MacTypes, CFBase, CFError, CFString, CFURL, CFArray, CFDictionary,
     CFNumber, CGBase, CGGeometry, CGFont, CGPath, CTFont,
     CTFontDescriptor, CTFontTraits, CTFontCollection, CTFontManager;
{$ENDIF FPC_DOTTEDUNITS}

{$linkframework CoreText}
{$linkframework CoreGraphics}
{$linkframework CoreFoundation}

const
  // The tangent of twelve degrees, which is about how far the italic of a
  // face leans when the face has one.
  SVGObliqueSlant = 0.2126;
  // From this weight up a face counts as bold.
  SVGBoldWeightFrom = 600;
  { The families macOS ships for each generic name. }
  GenericFamilies: array[TSVGGenericFamily] of String = (
    '', 'Times New Roman', 'Helvetica', 'Apple Chancery', 'Papyrus',
    'Menlo');
  { The weight Core Text gives a face, from -1 for the thinnest to 1 for
    the heaviest, at each hundred of the CSS scale. }
  CoreTextWeights: array[1..9] of Double = (
    -0.8, -0.6, -0.4, 0.0, 0.23, 0.3, 0.4, 0.56, 0.62);


function SVGCoreTextFontsAvailable: Boolean;

begin
  Result := True;
end;


// A CFString holding the text. The caller releases it.
function CFStringOf(const aText: TSVGString): CFStringRef;

var
  lBytes: RawByteString;

begin
  // Core Text is handed the bytes as UTF-8, which is what the text of a
  // document already is.
  lBytes := aText;
  Result := CFStringCreateWithCString(nil, PAnsiChar(lBytes),
    kCFStringEncodingUTF8);
end;


// The text of a CFString, empty when the reference is nil.
function StringOfCF(aText: CFStringRef): TSVGString;

var
  lBuffer: array[0..511] of AnsiChar;

begin
  Result := '';
  if aText = nil then
    Exit;
  lBuffer[0] := #0;
  if CFStringGetCString(aText, @lBuffer[0], SizeOf(lBuffer),
                        kCFStringEncodingUTF8) then
    Result := SVGTextOfUTF8(PAnsiChar(@lBuffer[0]));
end;


// The code point as the one or two UTF-16 units Core Text takes. Returns
// how many were written.
function UnitsOfCodePoint(aCodePoint: Cardinal;
  var aUnits: array of UniChar): Integer;

begin
  if aCodePoint > $FFFF then
    begin
    aUnits[0] := UniChar($D800 + ((aCodePoint - $10000) shr 10));
    aUnits[1] := UniChar($DC00 + ((aCodePoint - $10000) and $3FF));
    Result := 2;
    end
  else
    begin
    aUnits[0] := UniChar(aCodePoint);
    Result := 1;
    end;
end;


// The Core Text weight, from -1 to 1, for a CSS weight of 100 to 900.
function CoreTextWeightOf(aWeight: Integer): Double;

var
  lStep: Integer;

begin
  lStep := Round(aWeight / 100);
  if lStep < 1 then
    lStep := 1;
  if lStep > 9 then
    lStep := 9;
  Result := CoreTextWeights[lStep];
end;


// The Core Text width, from -1 to 1, for a font-stretch value.
function CoreTextWidthOf(aStretch: TSVGFontStretch): Double;

begin
  Result := (Ord(aStretch) - Ord(fsNormal)) / 4;
end;


// A descriptor for a family at a weight, a slant and a width. The caller
// releases it. Nil when the dictionaries cannot be built.
function DescriptorOf(const aFamily: TSVGString; aWeight: Integer;
  aStyle: TSVGFontStyle; aStretch: TSVGFontStretch): CTFontDescriptorRef;

var
  lName: CFStringRef;
  lTraits, lAttributes: CFDictionaryRef;
  lKeys, lValues: array[0..1] of UnivPtr;
  lTraitKeys, lTraitValues: array[0..2] of UnivPtr;
  lWeight, lWidth: Float64;
  lSymbolic: SInt32;
  lCount, I: Integer;

begin
  Result := nil;
  lTraits := nil;
  lAttributes := nil;
  lName := CFStringOf(aFamily);
  if lName = nil then
    Exit;
  lWeight := CoreTextWeightOf(aWeight);
  lWidth := CoreTextWidthOf(aStretch);
  lCount := 0;
  lTraitKeys[lCount] := kCTFontWeightTrait;
  lTraitValues[lCount] := CFNumberCreate(nil, kCFNumberFloat64Type, @lWeight);
  Inc(lCount);
  lTraitKeys[lCount] := kCTFontWidthTrait;
  lTraitValues[lCount] := CFNumberCreate(nil, kCFNumberFloat64Type, @lWidth);
  Inc(lCount);
  if aStyle <> fnNormal then
    begin
    lSymbolic := kCTFontTraitItalic;
    lTraitKeys[lCount] := kCTFontSymbolicTrait;
    lTraitValues[lCount] := CFNumberCreate(nil, kCFNumberSInt32Type,
      @lSymbolic);
    Inc(lCount);
    end;
  try
    lTraits := CFDictionaryCreate(nil, UnivPtrPtr(@lTraitKeys[0]),
      UnivPtrPtr(@lTraitValues[0]), lCount,
      @kCFTypeDictionaryKeyCallBacks, @kCFTypeDictionaryValueCallBacks);
    if lTraits = nil then
      Exit;
    lKeys[0] := kCTFontFamilyNameAttribute;
    lValues[0] := lName;
    lKeys[1] := kCTFontTraitsAttribute;
    lValues[1] := lTraits;
    lAttributes := CFDictionaryCreate(nil, UnivPtrPtr(@lKeys[0]),
      UnivPtrPtr(@lValues[0]), 2,
      @kCFTypeDictionaryKeyCallBacks, @kCFTypeDictionaryValueCallBacks);
    if lAttributes = nil then
      Exit;
    Result := CTFontDescriptorCreateWithAttributes(lAttributes);
  finally
    for I := 0 to lCount - 1 do
      if lTraitValues[I] <> nil then
        CFRelease(lTraitValues[I]);
    if lTraits <> nil then
      CFRelease(lTraits);
    if lAttributes <> nil then
      CFRelease(lAttributes);
    CFRelease(lName);
  end;
end;


// True when the face leans by itself, which says whether an italic request
// has to be leaned by hand.
function IsSlanted(aFont: CTFontRef): Boolean;

begin
  Result := (CTFontGetSymbolicTraits(aFont) and kCTFontTraitItalic) <> 0;
end;


// The families a file holds, one per line. Empty when Core Text reads no
// face from it.
function FamiliesOfFile(const aFileName: String): TSVGString;

var
  lPath: CFStringRef;
  lURL: CFURLRef;
  lDescriptors: CFArrayRef;
  lDescriptor: CTFontDescriptorRef;
  lName: CFTypeRef;
  I: CFIndex;

begin
  Result := '';
  lPath := CFStringOf(aFileName);
  if lPath = nil then
    Exit;
  lURL := CFURLCreateWithFileSystemPath(nil, lPath, kCFURLPOSIXPathStyle,
    False);
  CFRelease(lPath);
  if lURL = nil then
    Exit;
  lDescriptors := CTFontManagerCreateFontDescriptorsFromURL(lURL);
  CFRelease(lURL);
  if lDescriptors = nil then
    Exit;
  try
    for I := 0 to CFArrayGetCount(lDescriptors) - 1 do
      begin
      lDescriptor := CTFontDescriptorRef(CFArrayGetValueAtIndex(
        lDescriptors, I));
      if lDescriptor = nil then
        Continue;
      lName := CTFontDescriptorCopyAttribute(lDescriptor,
        kCTFontFamilyNameAttribute);
      if lName = nil then
        Continue;
      if Result <> '' then
        Result := Result + LineEnding;
      Result := Result + StringOfCF(CFStringRef(lName));
      CFRelease(lName);
      end;
  finally
    CFRelease(lDescriptors);
  end;
end;


// Hands a font file to Core Text for the life of this process.
function RegisterFile(const aFileName: String): Boolean;

var
  lPath: CFStringRef;
  lURL: CFURLRef;
  lError: CFErrorRef;

begin
  Result := False;
  lPath := CFStringOf(aFileName);
  if lPath = nil then
    Exit;
  lURL := CFURLCreateWithFileSystemPath(nil, lPath, kCFURLPOSIXPathStyle,
    False);
  CFRelease(lPath);
  if lURL = nil then
    Exit;
  lError := nil;
  Result := CTFontManagerRegisterFontsForURL(lURL,
    kCTFontManagerScopeProcess, lError) <> 0;
  if lError <> nil then
    CFRelease(lError);
  CFRelease(lURL);
end;


{ TSVGCoreTextFont }

constructor TSVGCoreTextFont.Create(aFont: Pointer; const aName: String;
  aSize, aSlant: Double);

begin
  inherited Create;
  FFont := aFont;
  FName := aName;
  FSize := aSize;
  FSlant := aSlant;
  FUnitsPerEm := 0;
  if FFont <> nil then
    FUnitsPerEm := CTFontGetUnitsPerEm(CTFontRef(FFont));
  if FUnitsPerEm <= 0 then
    FUnitsPerEm := 1000;
end;


destructor TSVGCoreTextFont.Destroy;

begin
  if FFont <> nil then
    CFRelease(CTFontRef(FFont));
  inherited Destroy;
end;


function TSVGCoreTextFont.GetFontName: TSVGString;

begin
  Result := FName;
end;


function TSVGCoreTextFont.GetUnitsPerEm: Integer;

begin
  Result := FUnitsPerEm;
end;


function TSVGCoreTextFont.GetSize: Double;

begin
  Result := FSize;
end;


function TSVGCoreTextFont.GetAscent: Double;

begin
  Result := CTFontGetAscent(CTFontRef(FFont));
end;


function TSVGCoreTextFont.GetDescent: Double;

begin
  Result := CTFontGetDescent(CTFontRef(FFont));
end;


function TSVGCoreTextFont.GetUnderlinePosition: Double;

begin
  // Core Text counts the position up from the baseline, and a caller here
  // counts it down the page.
  Result := -CTFontGetUnderlinePosition(CTFontRef(FFont));
end;


function TSVGCoreTextFont.GetUnderlineThickness: Double;

begin
  Result := CTFontGetUnderlineThickness(CTFontRef(FFont));
end;


function TSVGCoreTextFont.GetGlyphIndex(aCodePoint: Cardinal): Cardinal;

var
  lUnits: array[0..1] of UniChar;
  lGlyphs: array[0..1] of CGGlyph;
  lCount: Integer;

begin
  Result := 0;
  lGlyphs[0] := 0;
  lGlyphs[1] := 0;
  lCount := UnitsOfCodePoint(aCodePoint, lUnits);
  if CTFontGetGlyphsForCharacters(CTFontRef(FFont), @lUnits[0],
                                  @lGlyphs[0], lCount) = 0 then
    Exit;
  Result := lGlyphs[0];
end;


function TSVGCoreTextFont.GetGlyphForRun(const aCodes: TSVGCodePointArray;
  aFrom: Integer; out aCount: Integer): Cardinal;

begin
  // Core Text forms ligatures while it lays out a line, which this does
  // not use, so one code point is drawn at a time.
  aCount := 0;
  Result := 0;
  if (aFrom < 0) or (aFrom > High(aCodes)) then
    Exit;
  Result := GetGlyphIndex(aCodes[aFrom]);
  if Result <> 0 then
    aCount := 1;
end;


function TSVGCoreTextFont.GetGlyphNamed(const aName: TSVGString): Cardinal;

var
  lName: CFStringRef;

begin
  Result := 0;
  if aName = '' then
    Exit;
  lName := CFStringOf(aName);
  if lName = nil then
    Exit;
  try
    Result := CTFontGetGlyphWithName(CTFontRef(FFont), lName);
  finally
    CFRelease(lName);
  end;
end;


function TSVGCoreTextFont.GetGlyphAdvance(aGlyph: Cardinal): Double;

var
  lGlyph: CGGlyph;
  lAdvance: CGSize;

begin
  lGlyph := CGGlyph(aGlyph);
  lAdvance.width := 0;
  lAdvance.height := 0;
  CTFontGetAdvancesForGlyphs(CTFontRef(FFont), kCTFontOrientationHorizontal,
    @lGlyph, @lAdvance, 1);
  Result := lAdvance.width;
end;


function TSVGCoreTextFont.GetGlyphVerticalAdvance(aGlyph: Cardinal): Double;

var
  lGlyph: CGGlyph;
  lAdvance: CGSize;

begin
  lGlyph := CGGlyph(aGlyph);
  lAdvance.width := 0;
  lAdvance.height := 0;
  CTFontGetAdvancesForGlyphs(CTFontRef(FFont), kCTFontOrientationVertical,
    @lGlyph, @lAdvance, 1);
  Result := Abs(lAdvance.height);
  if Result = 0 then
    Result := FSize;
end;


procedure TSVGCoreTextFont.GetGlyphVerticalOrigin(aGlyph: Cardinal;
  out aX, aY: Double);

var
  lGlyph: CGGlyph;
  lTranslation: CGSize;

begin
  aX := -GetGlyphAdvance(aGlyph) / 2;
  aY := GetAscent;
  lGlyph := CGGlyph(aGlyph);
  lTranslation.width := 0;
  lTranslation.height := 0;
  CTFontGetVerticalTranslationsForGlyphs(CTFontRef(FFont), @lGlyph,
    @lTranslation, 1);
  if (lTranslation.width = 0) and (lTranslation.height = 0) then
    Exit;
  // Core Text gives the offset from the horizontal origin to the vertical
  // one, counting y upwards. The pen sits on the vertical origin and the
  // outline is drawn from the horizontal one, which is the way back.
  aX := -lTranslation.width;
  aY := lTranslation.height;
end;


function TSVGCoreTextFont.GetGlyphKerning(aLeft, aRight: Cardinal): Double;

begin
  // Core Text kerns a pair while it lays out a line, and offers no lookup
  // of its own.
  Result := 0;
end;


function TSVGCoreTextFont.GetGlyphVerticalKerning(aAbove,
  aBelow: Cardinal): Double;

begin
  Result := 0;
end;


function TSVGCoreTextFont.GetSmallCaps: Boolean;

begin
  Result := False;
end;


type
  { What a walk over a CGPath appends to, and how far it leans the
    outline. }
  TSVGPathWalk = record
    Path  : TSVGPath;
    Slant : Double;
    Drawn : Boolean;
  end;
  PSVGPathWalk = ^TSVGPathWalk;


// Appends one element of a CGPath to the path of the walk, with y counted
// down the page as user space counts it.
procedure AppendElement(aInfo: UnivPtr;
  const aElement: CGPathElement); mwpascal;

var
  lWalk: PSVGPathWalk;
  lPoints: CGPointPtr;
  lX, lY, lC1X, lC1Y, lC2X, lC2Y: Double;

  procedure PointOf(aIndex: Integer; out aPX, aPY: Double);
  var
    lPoint: CGPointPtr;
  begin
    lPoint := lPoints;
    Inc(lPoint, aIndex);
    aPX := lPoint^.x;
    aPY := -lPoint^.y;
    // Above the baseline y is negative, so a point there moves right and
    // the letter leans the way an italic does.
    if lWalk^.Slant <> 0 then
      aPX := aPX - aPY * lWalk^.Slant;
  end;

begin
  lWalk := PSVGPathWalk(aInfo);
  lPoints := aElement.points;
  case aElement.typ of
    kCGPathElementMoveToPoint:
      begin
      PointOf(0, lX, lY);
      lWalk^.Path.MoveTo(lX, lY);
      lWalk^.Drawn := True;
      end;
    kCGPathElementAddLineToPoint:
      begin
      PointOf(0, lX, lY);
      lWalk^.Path.LineTo(lX, lY);
      end;
    kCGPathElementAddQuadCurveToPoint:
      begin
      PointOf(0, lC1X, lC1Y);
      PointOf(1, lX, lY);
      lWalk^.Path.QuadTo(lC1X, lC1Y, lX, lY);
      end;
    kCGPathElementAddCurveToPoint:
      begin
      PointOf(0, lC1X, lC1Y);
      PointOf(1, lC2X, lC2Y);
      PointOf(2, lX, lY);
      lWalk^.Path.CubicTo(lC1X, lC1Y, lC2X, lC2Y, lX, lY);
      end;
    kCGPathElementCloseSubpath:
      lWalk^.Path.Close;
  end;
end;


function TSVGCoreTextFont.GetGlyphOutline(aGlyph: Cardinal;
  aPath: TSVGPath): Boolean;

var
  lPath: CGPathRef;
  lWalk: TSVGPathWalk;

begin
  Result := False;
  if (FFont = nil) or (aPath = nil) then
    Exit;
  lPath := CTFontCreatePathForGlyph(CTFontRef(FFont), CGGlyph(aGlyph), nil);
  if lPath = nil then
    Exit;
  try
    lWalk.Path := aPath;
    lWalk.Slant := FSlant;
    lWalk.Drawn := False;
    CGPathApply(lPath, @lWalk, @AppendElement);
    Result := lWalk.Drawn;
  finally
    CGPathRelease(lPath);
  end;
end;


{ TSVGCoreTextProvider }

constructor TSVGCoreTextProvider.Create;

begin
  inherited Create;
  FAvailable := SVGCoreTextFontsAvailable;
  FFonts := TFPList.Create;
end;


destructor TSVGCoreTextProvider.Destroy;

var
  I: Integer;

begin
  if FFonts <> nil then
    begin
    for I := 0 to FFonts.Count - 1 do
      TSVGCoreTextFont(FFonts[I]).Free;
    FreeAndNil(FFonts);
    end;
  inherited Destroy;
end;


function TSVGCoreTextProvider.Keep(aFont: Pointer; const aName: String;
  aSize, aSlant: Double): TSVGCoreTextFont;

begin
  Result := TSVGCoreTextFont.Create(aFont, aName, aSize, aSlant);
  FFonts.Add(Result);
end;


function TSVGCoreTextProvider.GetResource(
  aIndex: Integer): TSVGCoreTextResource;

begin
  Result := FResources[aIndex];
end;


function TSVGCoreTextProvider.GetGenericFamily(
  aGeneric: TSVGGenericFamily): String;

begin
  Result := FGeneric[aGeneric];
  if Result = '' then
    Result := GenericFamilies[aGeneric];
end;


procedure TSVGCoreTextProvider.SetGenericFamily(aGeneric: TSVGGenericFamily;
  const aValue: String);

begin
  FGeneric[aGeneric] := aValue;
end;


function TSVGCoreTextProvider.ResourceOf(const aFamily: String;
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


function TSVGCoreTextProvider.AddFontFile(const aFileName: String): Boolean;

begin
  Result := AddFontResource('', 0, fnNormal, aFileName);
end;


function TSVGCoreTextProvider.AddFontResource(const aFamily: TSVGString;
  aWeight: Integer; aStyle: TSVGFontStyle;
  const aFileName: String): Boolean;

var
  lFamilies: TStringList;

begin
  Result := False;
  if not FileExists(aFileName) then
    Exit;
  lFamilies := TStringList.Create;
  try
    lFamilies.Text := FamiliesOfFile(aFileName);
    if lFamilies.Count = 0 then
      Exit;
    if not RegisterFile(aFileName) then
      Exit;
    Result := True;
    if aFamily = '' then
      Exit;
    // A document may give the face a family of its own, which is then the
    // name a request for it comes in under.
    if FResourceCount = Length(FResources) then
      SetLength(FResources, 8 + FResourceCount * 2);
    FResources[FResourceCount].Declared := aFamily;
    FResources[FResourceCount].Actual := lFamilies[0];
    FResources[FResourceCount].Weight := aWeight;
    FResources[FResourceCount].Style := aStyle;
    Inc(FResourceCount);
  finally
    lFamilies.Free;
  end;
end;


function TSVGCoreTextProvider.AddFontPath(const aDirectory: String): Integer;

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


function TSVGCoreTextProvider.AddSystemFonts: Integer;

var
  lCollection: CTFontCollectionRef;
  lDescriptors: CFArrayRef;

begin
  Result := 0;
  lCollection := CTFontCollectionCreateFromAvailableFonts(nil);
  if lCollection = nil then
    Exit;
  try
    lDescriptors := CTFontCollectionCreateMatchingFontDescriptors(
      lCollection);
    if lDescriptors = nil then
      Exit;
    Result := Integer(CFArrayGetCount(lDescriptors));
    CFRelease(lDescriptors);
  finally
    CFRelease(lCollection);
  end;
  FFamilyCount := Result;
end;


function TSVGCoreTextProvider.FontOfFamily(const aFamily: String;
  const aRequest: TSVGFontRequest; out aResolved: String): Pointer;

var
  lDescriptor: CTFontDescriptorRef;
  lFont: CTFontRef;
  lName: CFStringRef;

begin
  Result := nil;
  aResolved := '';
  if aFamily = '' then
    Exit;
  lDescriptor := DescriptorOf(aFamily, aRequest.Weight, aRequest.Style,
    aRequest.Stretch);
  if lDescriptor = nil then
    Exit;
  lFont := CTFontCreateWithFontDescriptor(lDescriptor, aRequest.Size, nil);
  CFRelease(lDescriptor);
  if lFont = nil then
    Exit;
  // Core Text answers every request with something, so the family it
  // reports back is the only way to tell whether it had this one.
  lName := CTFontCopyFamilyName(lFont);
  aResolved := StringOfCF(lName);
  if lName <> nil then
    CFRelease(lName);
  if not SameText(aResolved, aFamily) then
    begin
    CFRelease(lFont);
    aResolved := '';
    Exit;
    end;
  Result := lFont;
end;


function TSVGCoreTextProvider.ResolveFace(
  const aRequest: TSVGFontRequest): TSVGCoreTextFont;

var
  lAt: Integer;
  lFamily, lResolved, lActual: String;
  lFont: Pointer;
  lGeneric, lKind: TSVGGenericFamily;

  function SlantOf(aFont: Pointer): Double;
  begin
    Result := 0;
    if (aRequest.Style <> fnNormal) and not IsSlanted(CTFontRef(aFont)) then
      Result := SVGObliqueSlant;
  end;

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
      lFont := FontOfFamily(lActual, aRequest, lResolved);
      if lFont <> nil then
        Exit(Keep(lFont, lResolved, aRequest.Size, SlantOf(lFont)));
      end;
    lGeneric := SVGGenericFamilyOf(lFamily);
    if lGeneric <> gfNone then
      lFamily := GetGenericFamily(lGeneric);
    lFont := FontOfFamily(lFamily, aRequest, lResolved);
    if lFont <> nil then
      Exit(Keep(lFont, lResolved, aRequest.Size, SlantOf(lFont)));
    // A family the system lacks is drawn with a face of the same kind:
    // Georgia gives a serif, Arial a sans.
    lKind := SVGFamilyKindOf(lFamily);
    if lKind <> gfNone then
      begin
      lFont := FontOfFamily(GetGenericFamily(lKind), aRequest, lResolved);
      if lFont <> nil then
        Exit(Keep(lFont, lResolved, aRequest.Size, SlantOf(lFont)));
      end;
  until False;
  lFamily := FDefaultFamily;
  if lFamily = '' then
    lFamily := GetGenericFamily(gfSansSerif);
  lFont := FontOfFamily(lFamily, aRequest, lResolved);
  if lFont = nil then
    Exit;
  Result := Keep(lFont, lResolved, aRequest.Size, SlantOf(lFont));
end;


function TSVGCoreTextProvider.ResolveFont(
  const aRequest: TSVGFontRequest): ISVGFont;

var
  lFont: TSVGCoreTextFont;

begin
  Result := nil;
  lFont := ResolveFace(aRequest);
  if lFont <> nil then
    Result := lFont;
end;


function TSVGCoreTextProvider.ResolveCover(aCodePoint: Cardinal;
  const aRequest: TSVGFontRequest): ISVGFont;

var
  lBase: TSVGCoreTextFont;
  lBaseFont, lCover: CTFontRef;
  lUnits: array[0..1] of UniChar;
  lGlyphs: array[0..1] of CGGlyph;
  lText, lName: CFStringRef;
  lRange: CFRange;
  lCount: Integer;

begin
  Result := nil;
  lBase := ResolveFace(aRequest);
  lBaseFont := nil;
  if lBase <> nil then
    lBaseFont := CTFontRef(lBase.Handle);
  lCount := UnitsOfCodePoint(aCodePoint, lUnits);
  lText := CFStringCreateWithCharacters(nil, @lUnits[0], lCount);
  if lText = nil then
    Exit;
  try
    lRange.location := 0;
    lRange.length := lCount;
    // Core Text answers even without a font to start from. It then falls
    // back from the system font.
    lCover := CTFontCreateForString(lBaseFont, lText, lRange);
    if lCover = nil then
      Exit;
    // Core Text hands back a face for every character, the one it started
    // from included. A face without the glyph covers nothing.
    lGlyphs[0] := 0;
    if (CTFontGetGlyphsForCharacters(lCover, @lUnits[0], @lGlyphs[0],
                                     lCount) = 0) or (lGlyphs[0] = 0) then
      begin
      CFRelease(lCover);
      Exit;
      end;
    lName := CTFontCopyFamilyName(lCover);
    Result := Keep(lCover, StringOfCF(lName), aRequest.Size, 0);
    if lName <> nil then
      CFRelease(lName);
  finally
    CFRelease(lText);
  end;
end;


end.
