{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Faces read from the font elements of an SVG document.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.svgfont;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}
{$modeswitch advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.Math, fpsvg.types, fpsvg.dom,
     fpsvg.read, fpsvg.path;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, math, fpsvg.types, fpsvg.dom, fpsvg.read,
     fpsvg.path;
{$ENDIF FPC_DOTTEDUNITS}

type
  ESVGFontRead = class(ESVGError);

  TSVGFontElementArray = array of TSVGElement;

  { One glyph of a font element: the outline it is drawn from and the
    room it takes, both in the design units of the face. }
  TSVGFontGlyph = record
    Code    : Cardinal;
    Name    : TSVGString;
    Advance : Double;
    Outline : TSVGString;
    // The text the glyph covers, and how many code points that is.
    // A ligature covers more than one. A glyph that an altGlyph selects
    // by name may cover none.
    Text    : TSVGString;
    Chars   : Integer;
    ID      : TSVGString;
    // The values for text running down the page: how far the pen moves,
    // and the vertical origin as an offset from the horizontal one,
    // counting upwards as a font does.
    VertAdvance : Double;
    VertOriginX : Double;
    VertOriginY : Double;
  end;
  TSVGFontGlyphArray = array of TSVGFontGlyph;

  { One pair of glyphs that are drawn closer together than their advances
    would place them, and by how much in design units. }
  TSVGFontKern = record
    Pair   : Int64;
    Amount : Double;
  end;
  TSVGFontKernArray = array of TSVGFontKern;

  { The glyphs and metrics of one font element, read once and then shared
    by every size the document asks for. }
  TSVGFontFace = class(TObject)
  private
    FFamily: TSVGString;
    FUnitsPerEm: Integer;
    FAscent, FDescent: Double;
    FDefaultAdvance: Double;
    FDefaultVertAdvance: Double;
    FDefaultVertOriginX, FDefaultVertOriginY: Double;
    FHasVertOriginX, FHasVertOriginY: Boolean;
    FWeight: Integer;
    FStyle: TSVGFontStyle;
    FUnderline: Double;
    FSmallCaps: Boolean;
    FUnderlineThickness: Double;
    FGlyphs: TSVGFontGlyphArray;
    FCount: Integer;
    // The glyphs that cover a single code point, sorted by that code
    // point, and the longest run that any glyph here covers.
    FOrder: array of Integer;
    FOrdered: Integer;
    FLongest: Integer;
    FMissing: TSVGFontGlyph;
    FHasMissing: Boolean;
    FKerns: TSVGFontKernArray;
    FKernCount: Integer;
    FVKerns: TSVGFontKernArray;
    FVKernCount: Integer;
    FRange: TSVGString;
    procedure ReadFontFace(aElement: TSVGElement);
    procedure AddGlyph(aElement: TSVGElement);
    procedure ReadVertical(aElement: TSVGElement;
      var aGlyph: TSVGFontGlyph);
    procedure SetMissing(aElement: TSVGElement);
    procedure AddKern(aElement: TSVGElement; aVertical: Boolean);
    procedure Sort;
    procedure SortKerns(var aKerns: TSVGFontKernArray; aCount: Integer);
    function KernIn(const aKerns: TSVGFontKernArray; aCount: Integer;
      aLeft, aRight: Cardinal): Double;
    function GetVariant: TSVGFontVariant;
  public
    // Reads a font element. The element is not kept.
    constructor Create(aFont: TSVGElement);
    // Registers the family the face answers to. That is the family given
    // by the font-face rule that loaded the file, not the family of the
    // face itself.
    procedure NameAs(const aFamily: TSVGString);
    // Narrows the face to the code points of a range, as requested by the
    // font-face rule that loaded the file. An empty range leaves the face
    // answering for every code point it has a glyph for.
    procedure RestrictTo(const aRange: TSVGString);
    // Index of the glyph for a code point, or zero when the face holds
    // none. Indices count from one.
    function IndexOf(aCodePoint: Cardinal): Cardinal;
    // The glyph that covers the longest run of the code points from aFrom
    // on. aCount returns how many of them it covers.
    function IndexOfRun(const aCodes: TSVGCodePointArray; aFrom: Integer;
      out aCount: Integer): Cardinal;
    // The glyph with an id or a glyph-name, zero when there is none.
    function IndexOfName(const aName: TSVGString): Cardinal;
    // A glyph by index. Index zero is the missing glyph.
    function Glyph(aIndex: Cardinal): TSVGFontGlyph;
    // How much closer together a pair of glyphs is drawn when they follow
    // one another along a line, in design units.
    function Kerning(aLeft, aRight: Cardinal): Double;
    // The same for a pair drawn one above the other in a column.
    function VerticalKerning(aAbove, aBelow: Cardinal): Double;
    // True when the face draws its lower case as small capitals.
    property SmallCaps: Boolean read FSmallCaps;
    // The font-variant the face declares, in the form a request uses.
    property Variant: TSVGFontVariant read GetVariant;
    // The family given by the font-face element.
    property Family: TSVGString read FFamily;
    // Design units per em, a thousand when the face states none.
    property UnitsPerEm: Integer read FUnitsPerEm;
    // Height above the baseline, in design units.
    property Ascent: Double read FAscent;
    // Depth below the baseline, in design units and positive.
    property Descent: Double read FDescent;
    // The weight the face is drawn at, four hundred when it states none.
    property Weight: Integer read FWeight;
    // Whether the face is an upright one or a leaning one.
    property Style: TSVGFontStyle read FStyle;
    // Where the face puts an underline, in design units counting down from
    // the baseline, and how thick it draws it. Zero when the face states
    // neither.
    property UnderlinePosition: Double read FUnderline;
    property UnderlineThickness: Double read FUnderlineThickness;
    // Number of glyphs the face holds, the missing one aside.
    property GlyphCount: Integer read FCount;
    // Whether the face draws anything for a code point it has no glyph
    // for.
    property HasMissingGlyph: Boolean read FHasMissing;
    // Number of glyph pairs the face draws closer together along a line.
    property KernCount: Integer read FKernCount;
    // Number of pairs it draws closer together down a column.
    property VerticalKernCount: Integer read FVKernCount;
    // The code points the face answers for, empty when it answers for all
    // of them.
    property Range: TSVGString read FRange;
  end;

  { A face of an SVG font at one size. }
  TSVGDocumentFont = class(TInterfacedObject, ISVGFont)
  private
    FFace: TSVGFontFace;
    FSize: Double;
    FScale: Double;
    FScratch: TSVGPath;
  public
    // Wraps a face at a size. The face is borrowed, not owned.
    constructor Create(aFace: TSVGFontFace; aSize: Double);
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
    procedure GetGlyphVerticalOrigin(aGlyph: Cardinal;
      out aX, aY: Double);
    function GetGlyphKerning(aLeft, aRight: Cardinal): Double;
    function GetGlyphVerticalKerning(aAbove,
      aBelow: Cardinal): Double;
    function GetSmallCaps: Boolean;
    function GetGlyphOutline(aGlyph: Cardinal; aPath: TSVGPath): Boolean;
  end;

  { A provider that uses the faces a document declares before it asks the
    system for one. }
  TSVGDocumentFontProvider = class(TInterfacedObject, ISVGFontProvider)
  private
    FBase: ISVGFontProvider;
    FFaces: array of TSVGFontFace;
    FCount: Integer;
    // The fonts handed out by ResolveFont, owned: these interfaces are
    // not reference counted.
    FFonts: TFPList;
    function IndexOfFace(const aFamily: TSVGString; aWeight: Integer;
      aStyle: TSVGFontStyle; aVariant: TSVGFontVariant): Integer;
    function NearestFace(const aFamily: TSVGString; aWeight: Integer;
      aStyle: TSVGFontStyle; aVariant: TSVGFontVariant;
      aStrict: Boolean): Integer;
  public
    // Wraps a provider. That provider answers for every family the
    // document declares no face for, and may be nil.
    constructor Create(aBase: ISVGFontProvider);
    destructor Destroy; override;
    // Reads a font element and keeps the face it holds. A family may
    // declare a face for each weight, slant and variant; a second face
    // with the same three is skipped, so the first one wins.
    procedure AddFont(aFont: TSVGElement);
    // Reads every font element of a document.
    procedure AddDocument(aDocument: TSVGDocument);
    // Reads a font element under a family the caller chooses, for a
    // font-face rule that loads another file.
    procedure AddFontAs(const aFamily, aRange: TSVGString; aFont: TSVGElement);
    function ResolveFont(const aRequest: TSVGFontRequest): ISVGFont;
    function ResolveCover(aCodePoint: Cardinal;
      const aRequest: TSVGFontRequest): ISVGFont;
    function AddFontResource(const aFamily: TSVGString; aWeight: Integer;
      aStyle: TSVGFontStyle; const aFileName: String): Boolean;
    // Number of faces read from the document.
    property FaceCount: Integer read FCount;
  end;

// The font elements a document holds, in document order.
function SVGFontElementsOf(aDocument: TSVGDocument): TSVGFontElementArray;
// The family of a font element, empty when it has none.
function SVGFontFamilyOf(aFont: TSVGElement): TSVGString;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ELSE FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ENDIF FPC_DOTTEDUNITS}

const
  SVGDefaultUnitsPerEm = 1000;
  // Two glyph indices make one key, and no face here holds this many.
  SVGKernStride = 1 shl 20;
  // A face that states no ascent or descent is read as filling the em the
  // way most faces do.
  SVGDefaultAscentShare = 0.8;
  SVGDefaultDescentShare = 0.2;


// A number written on a font element, or aDefault when it has none, or
// the value is not a number.
function NumberOf(aElement: TSVGElement; const aName: TSVGString;
  aDefault: Double): Double;

var
  lValue: Double;

begin
  Result := aDefault;
  if (aElement <> nil) and aElement.HasAttribute(aName)
     and TryStrToSVGNumber(Trim(aElement.Attributes[aName]), lValue) then
    Result := lValue;
end;


// The weight a font-face is drawn at. The attribute may hold a list of
// weights, and the first one is taken. A face that states none is
// normal.
function WeightOf(const aValue: TSVGString): Integer;

var
  lAt, lNumber, lCode: Integer;
  lFirst: TSVGString;

begin
  Result := SVGNormalFontWeight;
  lAt := 1;
  while (lAt <= Length(aValue)) and (aValue[lAt] <= ' ') do
    Inc(lAt);
  lFirst := '';
  while (lAt <= Length(aValue)) and (aValue[lAt] > ' ') do
    begin
    lFirst := lFirst + aValue[lAt];
    Inc(lAt);
    end;
  if SameText(lFirst, 'bold') then
    Exit(SVGBoldFontWeight);
  if (lFirst = '') or SameText(lFirst, 'normal')
     or SameText(lFirst, 'all') then
    Exit;
  Val(lFirst, lNumber, lCode);
  if (lCode = 0) and (lNumber >= 1) and (lNumber <= 1000) then
    Result := lNumber;
end;


// The first element child of the given tag, or nil.
function ChildNamed(aElement: TSVGElement; const aTag: TSVGString): TSVGElement;

var
  I: Integer;

begin
  Result := nil;
  if aElement = nil then
    Exit;
  for I := 0 to aElement.ChildCount - 1 do
    if (aElement[I] is TSVGElement)
       and (TSVGElement(aElement[I]).TagName = aTag) then
      Exit(TSVGElement(aElement[I]));
end;


function SVGFontFamilyOf(aFont: TSVGElement): TSVGString;

var
  lFace: TSVGElement;

begin
  Result := '';
  lFace := ChildNamed(aFont, 'font-face');
  if lFace <> nil then
    Result := Trim(lFace.AttributeDef('font-family', ''));
end;


// Adds every font element under an element to the array.
procedure CollectFonts(aElement: TSVGElement; var aFonts: TSVGFontElementArray;
  var aCount: Integer);

var
  I: Integer;

begin
  if aElement.TagName = 'font' then
    begin
    if aCount = Length(aFonts) then
      SetLength(aFonts, aCount * 2 + 8);
    aFonts[aCount] := aElement;
    Inc(aCount);
    end;
  for I := 0 to aElement.ChildCount - 1 do
    if aElement[I] is TSVGElement then
      CollectFonts(TSVGElement(aElement[I]), aFonts, aCount);
end;


function SVGFontElementsOf(aDocument: TSVGDocument): TSVGFontElementArray;

var
  lCount: Integer;

begin
  Result := nil;
  lCount := 0;
  if (aDocument <> nil) and (aDocument.Root <> nil) then
    CollectFonts(aDocument.Root, Result, lCount);
  SetLength(Result, lCount);
end;


{ TSVGDocumentFontProvider }

constructor TSVGDocumentFontProvider.Create(aBase: ISVGFontProvider);

begin
  inherited Create;
  FBase := aBase;
  FFonts := TFPList.Create;
end;


destructor TSVGDocumentFontProvider.Destroy;

var
  I: Integer;

begin
  for I := 0 to FFonts.Count - 1 do
    TSVGDocumentFont(FFonts[I]).Free;
  FreeAndNil(FFonts);
  for I := 0 to FCount - 1 do
    FFaces[I].Free;
  FFaces := nil;
  FBase := nil;
  inherited Destroy;
end;


function TSVGDocumentFontProvider.IndexOfFace(const aFamily: TSVGString;
  aWeight: Integer; aStyle: TSVGFontStyle;
  aVariant: TSVGFontVariant): Integer;

var
  I: Integer;

begin
  Result := -1;
  for I := 0 to FCount - 1 do
    if SameText(FFaces[I].Family, aFamily) and (FFaces[I].Weight = aWeight)
    and (FFaces[I].Style = aStyle) and (FFaces[I].Variant = aVariant) then
      Exit(I);
end;


// The declared face of a family that is nearest to the request, or -1
// when the family is not declared at all. A face of the wrong slant is
// further away than any difference in weight, which is the order CSS
// matches in.
// True when a face of that slant can be used for the requested one. CSS
// matches the slant before anything else and lets an italic request take
// an oblique face; every other value must match exactly, and a family
// without an acceptable slant is skipped for the next in the list.
function SlantAnswers(aWanted, aFace: TSVGFontStyle): Boolean;

begin
  Result := (aWanted = aFace)
         or ((aWanted = fnItalic) and (aFace = fnOblique));
end;


// How far a face is from the requested slant. An italic request takes an
// oblique face when the family has no italic one; an oblique request
// takes an italic face less readily.
function StyleGap(aWanted, aFace: TSVGFontStyle): Integer;

begin
  if aWanted = aFace then
    Result := 0
  else if (aWanted = fnItalic) and (aFace = fnOblique) then
    Result := 1000
  else if (aWanted = fnOblique) and (aFace = fnItalic) then
    Result := 2000
  else
    Result := 100000;
end;


// The face of a family closest to a request, or -1 when the family has
// none. With aStrict, only a face whose slant matches the requested one
// is considered.
function TSVGDocumentFontProvider.NearestFace(const aFamily: TSVGString;
  aWeight: Integer; aStyle: TSVGFontStyle; aVariant: TSVGFontVariant;
  aStrict: Boolean): Integer;

var
  I, lGap, lBest: Integer;

begin
  Result := -1;
  lBest := 0;
  for I := 0 to FCount - 1 do
    if SameText(FFaces[I].Family, aFamily)
       and (not aStrict or SlantAnswers(aStyle, FFaces[I].Style)) then
      begin
      // CSS2 matches the slant before the variant and the variant before
      // the weight, so the three are scored an order of magnitude apart.
      lGap := Abs(FFaces[I].Weight - aWeight);
      if FFaces[I].Variant <> aVariant then
        Inc(lGap, 10000);
      Inc(lGap, StyleGap(aStyle, FFaces[I].Style));
      if (Result < 0) or (lGap < lBest) then
        begin
        Result := I;
        lBest := lGap;
        end;
      end;
end;


procedure TSVGDocumentFontProvider.AddFontAs(const aFamily, aRange: TSVGString;
  aFont: TSVGElement);

var
  lFace: TSVGFontFace;

begin
  if (aFont = nil) or (aFamily = '') then
    Exit;
  lFace := TSVGFontFace.Create(aFont);
  if lFace.GlyphCount = 0 then
    begin
    lFace.Free;
    Exit;
    end;
  lFace.NameAs(aFamily);
  if aRange <> '' then
    lFace.RestrictTo(aRange);
  // The weight, the slant and the variant come from the face itself, so
  // the family can hold one of each and only a repeat is dropped.
  if IndexOfFace(aFamily, lFace.Weight, lFace.Style, lFace.Variant) >= 0 then
    begin
    lFace.Free;
    Exit;
    end;
  if FCount = Length(FFaces) then
    SetLength(FFaces, FCount * 2 + 4);
  FFaces[FCount] := lFace;
  Inc(FCount);
end;


procedure TSVGDocumentFontProvider.AddFont(aFont: TSVGElement);

begin
  AddFontAs(SVGFontFamilyOf(aFont), '', aFont);
end;


procedure TSVGDocumentFontProvider.AddDocument(aDocument: TSVGDocument);

var
  lFonts: TSVGFontElementArray;
  I: Integer;

begin
  lFonts := SVGFontElementsOf(aDocument);
  for I := 0 to High(lFonts) do
    AddFont(lFonts[I]);
end;


function TSVGDocumentFontProvider.ResolveFont(
  const aRequest: TSVGFontRequest): ISVGFont;

var
  lAt, lIndex, lLoose: Integer;
  lFamily: TSVGString;
  lProbe: TSVGFontRequest;
  lOne: ISVGFont;
  lNew: TSVGDocumentFont;

begin
  Result := nil;
  lAt := 1;
  lLoose := -1;
  // The list is walked in the order it is written, so a family declared by
  // the document is used only when no family before it answers.
  while lAt <= Length(aRequest.Families) do
    begin
    lFamily := SVGNextFamily(aRequest.Families, lAt);
    if lFamily = '' then
      Continue;
    lIndex := NearestFace(lFamily, aRequest.Weight, aRequest.Style,
      aRequest.Variant, True);
    // A declared family answers every weight and every variant asked of
    // it, with the nearest face it holds. The slant is the one property
    // it can fail on, and a family that fails it is skipped for the next
    // in the list.
    if lIndex >= 0 then
      begin
      lNew := TSVGDocumentFont.Create(FFaces[lIndex], aRequest.Size);
      FFonts.Add(lNew);
      Exit(lNew);
      end;
    if lLoose < 0 then
      lLoose := NearestFace(lFamily, aRequest.Weight, aRequest.Style,
        aRequest.Variant, False);
    if FBase = nil then
      Continue;
    // The provider below always answers with something, so the family it
    // reports back is the only way to tell whether it had this one. The
    // rest of the request is passed on unchanged: otherwise a bold face
    // would come back regular.
    lProbe := aRequest;
    lProbe.Families := lFamily;
    lOne := FBase.ResolveFont(lProbe);
    if (lOne <> nil) and SameText(lOne.GetFontName, lFamily) then
      Exit(lOne);
    end;
  if FBase <> nil then
    Result := FBase.ResolveFont(aRequest);
  // A face declared by the document is closer to what it meant than no
  // face at all, so a drawing with no other family to try and no fonts
  // below it keeps a declared face of another slant.
  if (Result = nil) and (lLoose >= 0) then
    begin
    lNew := TSVGDocumentFont.Create(FFaces[lLoose], aRequest.Size);
    FFonts.Add(lNew);
    Result := lNew;
    end;
end;


function TSVGDocumentFontProvider.ResolveCover(aCodePoint: Cardinal;
  const aRequest: TSVGFontRequest): ISVGFont;

begin
  Result := nil;
  if FBase <> nil then
    Result := FBase.ResolveCover(aCodePoint, aRequest);
end;


function TSVGDocumentFontProvider.AddFontResource(const aFamily: TSVGString;
  aWeight: Integer; aStyle: TSVGFontStyle;
  const aFileName: String): Boolean;

begin
  Result := (FBase <> nil)
        and FBase.AddFontResource(aFamily, aWeight, aStyle, aFileName);
end;



// The code point of a U+ item, or the two ends of a U+ range.
// False when the item is not written in that form.
function ParseCodeItem(const aItem: TSVGString;
  out aLow, aHigh: Cardinal): Boolean;

var
  lText, lFrom, lTo: TSVGString;
  lDash: Integer;

  function Hex(const aText: TSVGString; aWild: AnsiChar;
    out aValue: Cardinal): Boolean;
  var
    I: Integer;
    lDigit: Integer;
  begin
    Result := aText <> '';
    aValue := 0;
    for I := 1 to Length(aText) do
      begin
      // A question mark matches any digit, so the low end of the range
      // takes zero there and the high end fifteen.
      if aText[I] = '?' then
        lDigit := Ord(aWild)
      else if aText[I] in ['0'..'9'] then
        lDigit := Ord(aText[I]) - Ord('0')
      else if UpCase(aText[I]) in ['A'..'F'] then
        lDigit := Ord(UpCase(aText[I])) - Ord('A') + 10
      else
        Exit(False);
      aValue := aValue * 16 + Cardinal(lDigit);
      end;
  end;

begin
  Result := False;
  aLow := 0;
  aHigh := 0;
  lText := Trim(aItem);
  if (Length(lText) < 3) or not SameText(Copy(lText, 1, 2), 'U+') then
    Exit;
  lText := Copy(lText, 3, Length(lText));
  lDash := Pos('-', lText);
  if lDash > 0 then
    begin
    lFrom := Copy(lText, 1, lDash - 1);
    lTo := Copy(lText, lDash + 1, Length(lText));
    Result := Hex(lFrom, #0, aLow) and Hex(lTo, #15, aHigh);
    end
  else
    Result := Hex(lText, #0, aLow) and Hex(lText, #15, aHigh);
end;


// True when a code point is in a u1, u2 or unicode-range list.
// An empty list holds none.
function CodeInList(const aList: TSVGString; aCode: Cardinal): Boolean;

var
  lAt, lStart, lIndex: Integer;
  lItem: TSVGString;
  lLow, lHigh: Cardinal;

begin
  Result := False;
  lAt := 1;
  while lAt <= Length(aList) do
    begin
    lStart := lAt;
    while (lAt <= Length(aList)) and (aList[lAt] <> ',') do
      Inc(lAt);
    lItem := Trim(Copy(aList, lStart, lAt - lStart));
    Inc(lAt);
    if lItem = '' then
      Continue;
    if ParseCodeItem(lItem, lLow, lHigh) then
      begin
      if (aCode >= lLow) and (aCode <= lHigh) then
        Exit(True);
      end
    else
      begin
      // Anything else is the character itself, and only a single
      // character is looked for.
      lIndex := 1;
      if (SVGNextCodePoint(lItem, lIndex) = aCode)
         and (lIndex > Length(lItem)) then
        Exit(True);
      end;
    end;
end;


// True when a name is in a g1 or g2 list.
function NameInList(const aList, aName: TSVGString): Boolean;

var
  lAt, lStart: Integer;

begin
  Result := False;
  if aName = '' then
    Exit;
  lAt := 1;
  while lAt <= Length(aList) do
    begin
    lStart := lAt;
    while (lAt <= Length(aList)) and (aList[lAt] <> ',') do
      Inc(lAt);
    if Trim(Copy(aList, lStart, lAt - lStart)) = aName then
      Exit(True);
    Inc(lAt);
    end;
end;


{ TSVGFontFace }

constructor TSVGFontFace.Create(aFont: TSVGElement);

var
  I: Integer;
  lChild: TSVGElement;

begin
  inherited Create;
  if aFont = nil then
    raise ESVGFontRead.Create(SErrNoFontElement);
  FUnitsPerEm := SVGDefaultUnitsPerEm;
  FWeight := SVGNormalFontWeight;
  FStyle := fnNormal;
  FDefaultAdvance := NumberOf(aFont, 'horiz-adv-x', 0);
  FDefaultVertAdvance := NumberOf(aFont, 'vert-adv-y', -1);
  FHasVertOriginX := aFont.HasAttribute('vert-origin-x');
  FDefaultVertOriginX := NumberOf(aFont, 'vert-origin-x', 0);
  FHasVertOriginY := aFont.HasAttribute('vert-origin-y');
  FDefaultVertOriginY := NumberOf(aFont, 'vert-origin-y', 0);
  ReadFontFace(ChildNamed(aFont, 'font-face'));
  for I := 0 to aFont.ChildCount - 1 do
    begin
    if not (aFont[I] is TSVGElement) then
      Continue;
    lChild := TSVGElement(aFont[I]);
    if lChild.TagName = 'glyph' then
      AddGlyph(lChild)
    else if lChild.TagName = 'missing-glyph' then
      SetMissing(lChild);
    end;
  SetLength(FGlyphs, FCount);
  Sort;
  // The pairs refer to glyphs, so they are read once the glyphs are in
  // place and can be looked up.
  for I := 0 to aFont.ChildCount - 1 do
    begin
    if not (aFont[I] is TSVGElement) then
      Continue;
    if TSVGElement(aFont[I]).TagName = 'hkern' then
      AddKern(TSVGElement(aFont[I]), False)
    else if TSVGElement(aFont[I]).TagName = 'vkern' then
      AddKern(TSVGElement(aFont[I]), True);
    end;
  SetLength(FKerns, FKernCount);
  SetLength(FVKerns, FVKernCount);
  SortKerns(FKerns, FKernCount);
  SortKerns(FVKerns, FVKernCount);
end;


procedure TSVGFontFace.NameAs(const aFamily: TSVGString);

begin
  FFamily := aFamily;
end;


procedure TSVGFontFace.RestrictTo(const aRange: TSVGString);

begin
  FRange := Trim(aRange);
end;


procedure TSVGFontFace.ReadFontFace(aElement: TSVGElement);

begin
  if aElement <> nil then
    begin
    FFamily := Trim(aElement.AttributeDef('font-family', ''));
    FUnitsPerEm := Round(NumberOf(aElement, 'units-per-em',
      SVGDefaultUnitsPerEm));
    if FUnitsPerEm <= 0 then
      FUnitsPerEm := SVGDefaultUnitsPerEm;
    FRange := Trim(aElement.AttributeDef('unicode-range', ''));
    FWeight := WeightOf(aElement.AttributeDef('font-weight', ''));
    if SameText(Trim(aElement.AttributeDef('font-style', '')), 'italic') then
      FStyle := fnItalic
    else if SameText(Trim(aElement.AttributeDef('font-style', '')),
                     'oblique') then
      FStyle := fnOblique;
    end;
  FSmallCaps := SameText(Trim(aElement.AttributeDef('font-variant', '')),
    'small-caps');
  FAscent := NumberOf(aElement, 'ascent',
    FUnitsPerEm * SVGDefaultAscentShare);
  // The attribute is written as a depth below the baseline, which is
  // negative; it is stored the other way up here.
  FDescent := -NumberOf(aElement, 'descent',
    -FUnitsPerEm * SVGDefaultDescentShare);
  if FDescent < 0 then
    FDescent := 0;
  // These are written as in a post table, with the position counting up
  // from the baseline, so the sign is flipped here.
  FUnderline := -NumberOf(aElement, 'underline-position', 0);
  FUnderlineThickness := NumberOf(aElement, 'underline-thickness', 0);
end;


procedure TSVGFontFace.AddGlyph(aElement: TSVGElement);

var
  lText, lID: TSVGString;
  lIndex, lChars: Integer;
  lCode, lNext: Cardinal;

begin
  lText := aElement.AttributeDef('unicode', '');
  lID := aElement.AttributeDef('id', '');
  // A glyph that covers no text and has no name can never be selected.
  if (lText = '') and (lID = '')
  and (aElement.AttributeDef('glyph-name', '') = '') then
    Exit;
  lIndex := 1;
  lChars := 0;
  lCode := 0;
  while lIndex <= Length(lText) do
    begin
    lNext := SVGNextCodePoint(lText, lIndex);
    if lChars = 0 then
      lCode := lNext;
    Inc(lChars);
    end;
  if FCount = Length(FGlyphs) then
    SetLength(FGlyphs, FCount * 2 + 16);
  FGlyphs[FCount].Code := lCode;
  FGlyphs[FCount].Text := lText;
  FGlyphs[FCount].Chars := lChars;
  FGlyphs[FCount].ID := lID;
  FGlyphs[FCount].Name := aElement.AttributeDef('glyph-name', '');
  FGlyphs[FCount].Advance := NumberOf(aElement, 'horiz-adv-x',
    FDefaultAdvance);
  ReadVertical(aElement, FGlyphs[FCount]);
  FGlyphs[FCount].Outline := aElement.AttributeDef('d', '');
  if lChars > FLongest then
    FLongest := lChars;
  Inc(FCount);
end;


// The values of a glyph for text running down the page. When neither the
// glyph nor the font states them, SVG puts the vertical origin half an
// advance across and one ascent up, and moves the pen by one em.
procedure TSVGFontFace.ReadVertical(aElement: TSVGElement;
  var aGlyph: TSVGFontGlyph);

var
  lAdvance, lOriginX, lOriginY: Double;

begin
  lAdvance := FDefaultVertAdvance;
  if lAdvance < 0 then
    lAdvance := FUnitsPerEm;
  if FHasVertOriginX then
    lOriginX := FDefaultVertOriginX
  else
    lOriginX := aGlyph.Advance / 2;
  if FHasVertOriginY then
    lOriginY := FDefaultVertOriginY
  else
    lOriginY := FAscent;
  aGlyph.VertAdvance := NumberOf(aElement, 'vert-adv-y', lAdvance);
  aGlyph.VertOriginX := NumberOf(aElement, 'vert-origin-x', lOriginX);
  aGlyph.VertOriginY := NumberOf(aElement, 'vert-origin-y', lOriginY);
end;


procedure TSVGFontFace.SetMissing(aElement: TSVGElement);

begin
  FMissing.Code := 0;
  FMissing.Advance := NumberOf(aElement, 'horiz-adv-x', FDefaultAdvance);
  ReadVertical(aElement, FMissing);
  FMissing.Outline := aElement.AttributeDef('d', '');
  FHasMissing := True;
end;


// Reads one hkern element. A side is given by its characters, by its
// glyph names, or by both, and a glyph belongs to that side when either
// list holds it. Every pair the two sides make is recorded.
procedure TSVGFontFace.AddKern(aElement: TSVGElement; aVertical: Boolean);

var
  I, J: Integer;
  lU1, lU2, lG1, lG2: TSVGString;
  lAmount: Double;
  lLeft: array of Integer;
  lLeftCount: Integer;

  function OnSide(aIndex: Integer; const aCodes, aNames: TSVGString): Boolean;
  begin
    Result := ((aCodes <> '') and CodeInList(aCodes, FGlyphs[aIndex].Code))
           or ((aNames <> '') and NameInList(aNames, FGlyphs[aIndex].Name));
  end;

begin
  lAmount := NumberOf(aElement, 'k', 0);
  if lAmount = 0 then
    Exit;
  lU1 := aElement.AttributeDef('u1', '');
  lU2 := aElement.AttributeDef('u2', '');
  lG1 := aElement.AttributeDef('g1', '');
  lG2 := aElement.AttributeDef('g2', '');
  if ((lU1 = '') and (lG1 = '')) or ((lU2 = '') and (lG2 = '')) then
    Exit;
  SetLength(lLeft, FCount);
  lLeftCount := 0;
  for I := 0 to FCount - 1 do
    if OnSide(I, lU1, lG1) then
      begin
      lLeft[lLeftCount] := I;
      Inc(lLeftCount);
      end;
  if lLeftCount = 0 then
    Exit;
  for J := 0 to FCount - 1 do
    begin
    if not OnSide(J, lU2, lG2) then
      Continue;
    for I := 0 to lLeftCount - 1 do
      if aVertical then
        begin
        if FVKernCount = Length(FVKerns) then
          SetLength(FVKerns, FVKernCount * 2 + 32);
        FVKerns[FVKernCount].Pair := Int64(lLeft[I] + 1) * SVGKernStride
          + Int64(J) + 1;
        FVKerns[FVKernCount].Amount := lAmount;
        Inc(FVKernCount);
        end
      else
        begin
        if FKernCount = Length(FKerns) then
          SetLength(FKerns, FKernCount * 2 + 32);
        FKerns[FKernCount].Pair := Int64(lLeft[I] + 1) * SVGKernStride
          + Int64(J) + 1;
        FKerns[FKernCount].Amount := lAmount;
        Inc(FKernCount);
        end;
    end;
end;


procedure TSVGFontFace.SortKerns(var aKerns: TSVGFontKernArray;
  aCount: Integer);

var
  I, J: Integer;
  lKern: TSVGFontKern;

begin
  for I := 1 to aCount - 1 do
    begin
    lKern := aKerns[I];
    J := I;
    while (J > 0) and (aKerns[J - 1].Pair > lKern.Pair) do
      begin
      aKerns[J] := aKerns[J - 1];
      Dec(J);
      end;
    aKerns[J] := lKern;
    end;
end;


function TSVGFontFace.GetVariant: TSVGFontVariant;

begin
  if FSmallCaps then
    Result := fvSmallCaps
  else
    Result := fvNormal;
end;


function TSVGFontFace.KernIn(const aKerns: TSVGFontKernArray;
  aCount: Integer; aLeft, aRight: Cardinal): Double;

var
  lLow, lHigh, lMid: Integer;
  lWanted: Int64;

begin
  Result := 0;
  if (aLeft = 0) or (aRight = 0) or (aCount = 0) then
    Exit;
  lWanted := Int64(aLeft) * SVGKernStride + Int64(aRight);
  lLow := 0;
  lHigh := aCount - 1;
  while lLow <= lHigh do
    begin
    lMid := (lLow + lHigh) div 2;
    if aKerns[lMid].Pair = lWanted then
      Exit(aKerns[lMid].Amount)
    else if aKerns[lMid].Pair < lWanted then
      lLow := lMid + 1
    else
      lHigh := lMid - 1;
    end;
end;


function TSVGFontFace.Kerning(aLeft, aRight: Cardinal): Double;

begin
  Result := KernIn(FKerns, FKernCount, aLeft, aRight);
end;


// Nothing here draws down a column yet, so this is read and kept but
// never used.
function TSVGFontFace.VerticalKerning(aAbove, aBelow: Cardinal): Double;

begin
  Result := KernIn(FVKerns, FVKernCount, aAbove, aBelow);
end;


// Builds the order in which the single code point glyphs are searched.
// The glyphs themselves stay in document order, so a glyph keeps the index
// that a name or a run resolves to.
procedure TSVGFontFace.Sort;

var
  I, J, lAt: Integer;

begin
  SetLength(FOrder, FCount);
  FOrdered := 0;
  for I := 0 to FCount - 1 do
    if FGlyphs[I].Chars = 1 then
      begin
      FOrder[FOrdered] := I;
      Inc(FOrdered);
      end;
  for I := 1 to FOrdered - 1 do
    begin
    lAt := FOrder[I];
    J := I;
    while (J > 0) and (FGlyphs[FOrder[J - 1]].Code > FGlyphs[lAt].Code) do
      begin
      FOrder[J] := FOrder[J - 1];
      Dec(J);
      end;
    FOrder[J] := lAt;
    end;
end;


function TSVGFontFace.IndexOf(aCodePoint: Cardinal): Cardinal;

var
  lLow, lHigh, lMid: Integer;

begin
  Result := 0;
  // A face narrowed to a range answers for nothing outside it. A code
  // point it does not cover is looked for in another face.
  if (FRange <> '') and not CodeInList(FRange, aCodePoint) then
    Exit;
  lLow := 0;
  lHigh := FOrdered - 1;
  while lLow <= lHigh do
    begin
    lMid := (lLow + lHigh) div 2;
    if FGlyphs[FOrder[lMid]].Code = aCodePoint then
      Exit(Cardinal(FOrder[lMid]) + 1)
    else if FGlyphs[FOrder[lMid]].Code < aCodePoint then
      lLow := lMid + 1
    else
      lHigh := lMid - 1;
    end;
  // Inside the range the face answers for everything: a code point
  // without a glyph is drawn with the missing glyph.
  if FHasMissing then
    Result := Cardinal(FCount) + 1;
end;


// The glyph that covers the longest run of the code points from aFrom on.
// The longest one wins, so a face holding both "f" and "ffl" draws all
// three characters with one glyph instead of the first one alone.
function TSVGFontFace.IndexOfRun(const aCodes: TSVGCodePointArray;
  aFrom: Integer; out aCount: Integer): Cardinal;

var
  I, J, lBest, lLength: Integer;
  lText: TSVGString;
  lAt: Integer;
  lSame: Boolean;

begin
  aCount := 1;
  Result := 0;
  lBest := 1;
  if FLongest > 1 then
    for I := 0 to FCount - 1 do
      begin
      lLength := FGlyphs[I].Chars;
      if (lLength <= lBest) or (aFrom + lLength > Length(aCodes)) then
        Continue;
      if (FRange <> '') and not CodeInList(FRange, FGlyphs[I].Code) then
        Continue;
      lText := FGlyphs[I].Text;
      lAt := 1;
      lSame := True;
      for J := 0 to lLength - 1 do
        if SVGNextCodePoint(lText, lAt) <> aCodes[aFrom + J] then
          begin
          lSame := False;
          Break;
          end;
      if lSame then
        begin
        lBest := lLength;
        Result := Cardinal(I) + 1;
        end;
      end;
  if Result <> 0 then
    begin
    aCount := lBest;
    Exit;
    end;
  if aFrom < Length(aCodes) then
    Result := IndexOf(aCodes[aFrom]);
end;


function TSVGFontFace.IndexOfName(const aName: TSVGString): Cardinal;

var
  I: Integer;

begin
  Result := 0;
  if aName = '' then
    Exit;
  for I := 0 to FCount - 1 do
    if (FGlyphs[I].ID = aName) or (FGlyphs[I].Name = aName) then
      Exit(Cardinal(I) + 1);
end;


function TSVGFontFace.Glyph(aIndex: Cardinal): TSVGFontGlyph;

begin
  if (aIndex = 0) or (aIndex > Cardinal(FCount)) then
    Result := FMissing
  else
    Result := FGlyphs[aIndex - 1];
end;


{ TSVGDocumentFont }

constructor TSVGDocumentFont.Create(aFace: TSVGFontFace; aSize: Double);

begin
  inherited Create;
  if aFace = nil then
    raise ESVGFontRead.Create(SErrNoFaceToDraw);
  FFace := aFace;
  FSize := aSize;
  FScale := aSize / aFace.UnitsPerEm;
  FScratch := TSVGPath.Create;
end;


destructor TSVGDocumentFont.Destroy;

begin
  FreeAndNil(FScratch);
  inherited Destroy;
end;


function TSVGDocumentFont.GetFontName: TSVGString;

begin
  Result := FFace.Family;
end;


function TSVGDocumentFont.GetUnitsPerEm: Integer;

begin
  Result := FFace.UnitsPerEm;
end;


function TSVGDocumentFont.GetSize: Double;

begin
  Result := FSize;
end;


function TSVGDocumentFont.GetAscent: Double;

begin
  Result := FFace.Ascent * FScale;
end;


function TSVGDocumentFont.GetDescent: Double;

begin
  Result := FFace.Descent * FScale;
end;


function TSVGDocumentFont.GetUnderlinePosition: Double;

begin
  Result := FFace.UnderlinePosition * FScale;
end;


function TSVGDocumentFont.GetUnderlineThickness: Double;

begin
  Result := FFace.UnderlineThickness * FScale;
end;


function TSVGDocumentFont.GetGlyphIndex(aCodePoint: Cardinal): Cardinal;

begin
  Result := FFace.IndexOf(aCodePoint);
end;


function TSVGDocumentFont.GetGlyphForRun(const aCodes: TSVGCodePointArray;
  aFrom: Integer; out aCount: Integer): Cardinal;

begin
  Result := FFace.IndexOfRun(aCodes, aFrom, aCount);
end;


function TSVGDocumentFont.GetGlyphNamed(const aName: TSVGString): Cardinal;

begin
  Result := FFace.IndexOfName(aName);
end;


function TSVGDocumentFont.GetGlyphAdvance(aGlyph: Cardinal): Double;

begin
  Result := FFace.Glyph(aGlyph).Advance * FScale;
end;


function TSVGDocumentFont.GetGlyphVerticalAdvance(aGlyph: Cardinal): Double;

begin
  Result := FFace.Glyph(aGlyph).VertAdvance * FScale;
end;


procedure TSVGDocumentFont.GetGlyphVerticalOrigin(aGlyph: Cardinal;
  out aX, aY: Double);

begin
  aX := -FFace.Glyph(aGlyph).VertOriginX * FScale;
  aY := FFace.Glyph(aGlyph).VertOriginY * FScale;
end;


function TSVGDocumentFont.GetGlyphKerning(aLeft, aRight: Cardinal): Double;

begin
  Result := FFace.Kerning(aLeft, aRight) * FScale;
end;


function TSVGDocumentFont.GetGlyphVerticalKerning(aAbove,
  aBelow: Cardinal): Double;

begin
  Result := FFace.VerticalKerning(aAbove, aBelow) * FScale;
end;


function TSVGDocumentFont.GetSmallCaps: Boolean;

begin
  Result := FFace.SmallCaps;
end;


function TSVGDocumentFont.GetGlyphOutline(aGlyph: Cardinal;
  aPath: TSVGPath): Boolean;

var
  I: Integer;
  lSegment: TSVGPathSegment;
  lOutline: TSVGString;

  // The design grid has y running up and the em as its unit, and user
  // space has y running down and the size as its unit.
  function At(aIndex: Integer): TSVGPoint;
  begin
    Result := TSVGPoint.Create(lSegment.Points[aIndex].X * FScale,
      -lSegment.Points[aIndex].Y * FScale);
  end;

begin
  Result := False;
  lOutline := FFace.Glyph(aGlyph).Outline;
  if Trim(lOutline) = '' then
    Exit;
  FScratch.Clear;
  if not FScratch.TryParse(lOutline) then
    Exit;
  for I := 0 to FScratch.SegmentCount - 1 do
    begin
    lSegment := FScratch[I];
    case lSegment.Kind of
      skMoveTo: aPath.MoveTo(At(0).X, At(0).Y);
      skLineTo: aPath.LineTo(At(0).X, At(0).Y);
      skCubicTo: aPath.CubicTo(At(0).X, At(0).Y, At(1).X, At(1).Y,
                   At(2).X, At(2).Y);
      skClose: aPath.Close;
    end;
    end;
  Result := True;
end;


end.
