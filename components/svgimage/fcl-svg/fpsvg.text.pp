{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Text layout: characters to positioned glyph runs, against any ISVGFont.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.text;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}
{$modeswitch advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.Math, fpsvg.types, fpsvg.dom,
     fpsvg.read, fpsvg.style, fpsvg.geom;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, math, fpsvg.types, fpsvg.dom, fpsvg.read,
     fpsvg.style, fpsvg.geom;
{$ENDIF FPC_DOTTEDUNITS}

type
  ESVGText = class(ESVGError);

  TSVGLengthAdjust = (laSpacing, laSpacingAndGlyphs);

  { One run of glyphs sharing an element, a font and a computed style.
    Left, Right and Baseline bound the run in user space, which is where a
    decoration line is drawn. Turned is set when the text is laid along a
    path, where a straight line cannot follow it; the three bounds are
    then meaningless. }
  TSVGTextRun = record
    Element  : TSVGElement;
    Font     : ISVGFont;
    Style    : TSVGComputedStyle;
    Glyphs   : TSVGGlyphArray;
    Left     : Double;
    Right    : Double;
    Baseline : Double;
    Turned   : Boolean;
  end;
  TSVGTextRunArray = array of TSVGTextRun;

  { A decoration line that one element requested, and the text it runs
    under. SVG paints it with the paint of that element and not with the
    paint of the text it crosses, so the style here is the one that
    declared the line. }
  TSVGTextBand = record
    Style    : TSVGComputedStyle;
    Font     : ISVGFont;
    Lines    : TSVGTextDecorations;
    Waiting  : Boolean;
    First    : Integer;
    Last     : Integer;
    Left     : Double;
    Right    : Double;
    Baseline : Double;
    Turned   : Boolean;
    // Set when the text runs down the page. Left and Right are then the
    // ends of the column, and Baseline is the line it runs down.
    Vertical : Boolean;
  end;
  TSVGTextBandArray = array of TSVGTextBand;

  { A character of the collected text, with the positions its element gave
    it and the span it belongs to. }
  TSVGTextChar = record
    CodePoint : Cardinal;
    Span      : Integer;
    Capital   : Boolean;
    HasX, HasY, HasDX, HasDY : Boolean;
    X, Y, DX, DY : Double;
    Advance      : Double;
    Angle        : Double;
    Dropped      : Boolean;
    // A glyph chosen by the document, in place of the one the face holds
    // for the code point. Merged marks a code point that an earlier glyph
    // already covers: it is drawn nowhere and takes no room.
    Glyph        : Cardinal;
    HasGlyph     : Boolean;
    Merged       : Boolean;
    // The quarter turn that the glyph orientation gives it in a column.
    // It is added into Angle, and read again to place the outline.
    Turn         : Double;
    Font         : ISVGFont;
  end;
  TSVGTextCharArray = array of TSVGTextChar;

  { A path that characters are laid along, and the position along it where
    they start. }
  TSVGTextPathRun = record
    Metrics : TSVGPathMetrics;
    Offset  : Double;
    Origin  : Double;
    Known   : Boolean;
  end;
  TSVGTextPathRunArray = array of TSVGTextPathRun;

  { An element contributing characters, with the font it resolved to.
    Shift is how far down the page baseline-shift moved this element and
    everything above it, so a shift inside a shift adds up. }
  TSVGTextSpan = record
    Element : TSVGElement;
    Style   : TSVGComputedStyle;
    Font    : ISVGFont;
    Small   : ISVGFont;
    Path    : Integer;
    Shift   : Double;
  end;
  TSVGTextSpanArray = array of TSVGTextSpan;

  { The x, y, dx and dy lists of one element, and the position where its
    content began.
    Serial numbers the frames in the order they open, so a frame keeps its
    number after the frames beside it have opened and closed. }
  TSVGTextFrame = record
    X, Y, DX, DY : TSVGDoubleArray;
    Rot          : TSVGDoubleArray;
    Base         : Integer;
    Serial       : Integer;
  end;
  TSVGTextFrameArray = array of TSVGTextFrame;

  { Turns a text element into positioned glyph runs. Measures only: it
    names no backend and draws nothing. }
  TSVGTextLayout = class(TObject)
  private
    FChars: TSVGTextCharArray;
    FCharCount: Integer;
    FStretch: Double;
    FSpans: TSVGTextSpanArray;
    FSpanCount: Integer;
    FFrames: TSVGTextFrameArray;
    FFrameCount: Integer;
    FRuns: TSVGTextRunArray;
    FRunCount: Integer;
    FVertical: Boolean;
    FPaths: TSVGTextPathRunArray;
    FPathCount: Integer;
    FSpanPath: Integer;
    FSawPath: Boolean;
    FPendingSpan: Integer;
    FPendingSerial: Integer;
    FFrameSerial: Integer;
    FBands: TSVGTextBandArray;
    FBandCount: Integer;
    FStyles: TSVGStyleResolver;
    FFonts: ISVGFontProvider;
    FContext: TSVGLengthContext;
    FPending: Boolean;
    FStarted: Boolean;
    FBounds: TSVGRect;
    function GetRun(aIndex: Integer): TSVGTextRun;
    procedure Reset;
    function AddSpan(aElement: TSVGElement;
      const aStyle: TSVGComputedStyle; aParent: Integer): Integer;
    procedure AddChar(aCodePoint: Cardinal; aSpan: Integer);
    procedure AddGlyphChar(aGlyph: Cardinal; aSpan: Integer;
      aFace: ISVGFont);
    function GlyphOf(aIndex: Integer; aFont: ISVGFont): Cardinal;
    function SameSpanRun(aFrom, aCount: Integer): Integer;
    procedure MergeRuns;
    function CollectAltGlyph(aElement: TSVGElement; aSpan: Integer): Boolean;
    procedure PushFrame(aElement: TSVGElement;
      const aStyle: TSVGComputedStyle);
    procedure PopFrame;
    procedure ApplyFrames(aDepth: Integer);
    function OpenWhenPending: Integer;
    procedure Collect(aElement: TSVGElement; aSpan: Integer;
      const aStyle: TSVGComputedStyle; aPreserve: Boolean);
    procedure CollectText(const aText: TSVGString; aSpan: Integer;
      aPreserve: Boolean);
    procedure CollectReferenced(aElement: TSVGElement; aSpan: Integer;
      aPreserve: Boolean);
    function GetBand(aIndex: Integer): TSVGTextBand;
    function OpenBand(const aStyle: TSVGComputedStyle; aFont: ISVGFont;
      aLines: TSVGTextDecorations): Integer;
    function FirstOfBand(aBand: Integer): Integer;
    procedure MeasureBands;
    function FontFor(aChar: Integer): ISVGFont;
    function BaselineShift(aChar: Integer; aFont: ISVGFont): Double;
    procedure Place;
    procedure AnchorChunks(aElement: TSVGElement);
    function Along(aChar: Integer): Double;
    function Across(aChar: Integer): Double;
    procedure MoveAlong(aChar: Integer; aBy: Double);
    procedure PutAlong(aChar: Integer; aTo: Double);
    function OpensChunk(aChar: Integer): Boolean;
    function TurnOf(aChar: Integer): Double;
    function StandsInForSmallCaps(aSpan: Integer): Boolean;
    function AddPath(aElement: TSVGElement): Integer;
    procedure MapOntoPaths;
    procedure BuildRuns;
  public
    constructor Create;
    destructor Destroy; override;
    // Lays out a text element. False when it holds nothing to draw, or no
    // font could be had for it.
    function Layout(aText: TSVGElement; aStyles: TSVGStyleResolver;
      aFonts: ISVGFontProvider; const aStyle: TSVGComputedStyle;
      const aContext: TSVGLengthContext): Boolean;
    // Box the laid out glyphs occupy, in the user space of the element.
    property Bounds: TSVGRect read FBounds;
    // Number of runs the last layout produced.
    property RunCount: Integer read FRunCount;
    // Run by index, in the order the characters appear.
    property Runs[aIndex: Integer]: TSVGTextRun read GetRun; default;
    // Number of decoration lines the last layout produced.
    property DecorationCount: Integer read FBandCount;
    // Decoration line by index, in the order the elements declared them.
    property Decorations[aIndex: Integer]: TSVGTextBand read GetBand;
  end;

// True when SVG keeps a character upright in a column: the ideographic
// and full width characters, which stay upright and are never rotated.
function SVGStandsUpright(aCode: Cardinal): Boolean;
// True when the element or an ancestor requests that whitespace is kept.
function SVGPreservesSpace(aElement: TSVGElement): Boolean;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ELSE FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ENDIF FPC_DOTTEDUNITS}

const
  ReplacementChar = $FFFD;
  SVGTextPathMissing = -2;
  // A face without real small capitals draws a lower case letter as its
  // capital at this fraction of the size.
  SVGSmallCapScale = 0.8;

// True when a quarter turn has laid the glyph on its side, so that it
// runs down the column on its width instead of its height.
function SVGLaidSideways(aTurn: Double): Boolean;

begin
  Result := SameValue(aTurn, 90) or SameValue(aTurn, 270);
end;


// How much closer a face draws a pair of glyphs in a column.
// Two upright glyphs use the vertical kerning pairs of the font.
// Two glyphs laid on their side are drawn like a line across the page and
// use the horizontal pairs.
// A pair with one glyph of each kind is left alone.
function SVGKernDown(aFont: ISVGFont; aAbove, aBelow: Cardinal;
  aAboveTurn, aBelowTurn: Double): Double;

begin
  Result := 0;
  if SVGLaidSideways(aAboveTurn) <> SVGLaidSideways(aBelowTurn) then
    Exit;
  if SVGLaidSideways(aAboveTurn) then
    Result := aFont.GetGlyphKerning(aAbove, aBelow)
  else
    Result := aFont.GetGlyphVerticalKerning(aAbove, aBelow);
end;


function SVGStandsUpright(aCode: Cardinal): Boolean;

const
  Ranges: array[0..12, 0..1] of Cardinal = (
    ($1100, $11FF),    // Hangul jamo
    ($2E80, $303E),    // CJK radicals, Kangxi, CJK punctuation
    ($3041, $33FF),    // kana, Bopomofo, compatibility jamo, enclosed CJK
    ($3400, $4DBF),    // CJK ideographs, extension A
    ($4E00, $9FFF),    // CJK ideographs
    ($A000, $A4CF),    // Yi
    ($AC00, $D7AF),    // Hangul syllables
    ($F900, $FAFF),    // CJK compatibility ideographs
    ($FE10, $FE1F),    // vertical forms
    ($FE30, $FE4F),    // CJK compatibility forms
    ($FF00, $FF60),    // full width forms
    ($FFE0, $FFE6),    // full width signs
    ($20000, $3FFFD)); // CJK ideographs, the later extensions

var
  I: Integer;

begin
  Result := False;
  for I := Low(Ranges) to High(Ranges) do
    if (aCode >= Ranges[I, 0]) and (aCode <= Ranges[I, 1]) then
      Exit(True);
end;


function SVGPreservesSpace(aElement: TSVGElement): Boolean;

var
  lNode: TSVGElement;
  lValue: TSVGString;

begin
  Result := False;
  lNode := aElement;
  while lNode <> nil do
    begin
    lValue := lNode.AttributeDef('xml:space', '');
    if lValue = '' then
      lValue := lNode.AttributeDef('space', '');
    if lValue <> '' then
      Exit(SameText(Trim(lValue), 'preserve'));
    lNode := lNode.Parent;
    end;
end;


// The value of xml:space on this element alone, or aDefault when it has
// none. XML inherits the attribute, so aDefault is the value of the
// parent.
function PreservesSpaceHere(aElement: TSVGElement;
  aDefault: Boolean): Boolean;

var
  lValue: TSVGString;

begin
  Result := aDefault;
  lValue := aElement.AttributeDef('xml:space', '');
  if lValue = '' then
    lValue := aElement.AttributeDef('space', '');
  if lValue <> '' then
    Result := SameText(Trim(lValue), 'preserve');
end;


// The capital of a letter, or the letter itself when it has none or is
// already one. Only ASCII and the Latin-1 letters are mapped.
function CapitalOf(aCodePoint: Cardinal): Cardinal;

begin
  Result := aCodePoint;
  if (aCodePoint >= Ord('a')) and (aCodePoint <= Ord('z')) then
    Result := aCodePoint - 32
  else if (aCodePoint >= $E0) and (aCodePoint <= $FE)
          and (aCodePoint <> $F7) then
    Result := aCodePoint - 32
  else if aCodePoint = $FF then
    Result := $178;
end;


// True when a code point is one of the whitespace characters XML collapses.
function IsTextSpace(aCodePoint: Cardinal): Boolean;

begin
  Result := (aCodePoint = 32) or (aCodePoint = 9) or (aCodePoint = 10)
         or (aCodePoint = 13);
end;


// Reads a list of numbers from an attribute; empty when it is absent.
function NumberListOf(aElement: TSVGElement;
  const aName: TSVGString): TSVGDoubleArray;

begin
  Result := nil;
  if not aElement.HasAttribute(aName) then
    Exit;
  if not TryStrToSVGNumberList(aElement.Attributes[aName], Result) then
    Result := nil;
end;


// The x, y, dx or dy list of an element, in user units. Each entry is a
// length and may have a unit, so a text can be placed two em down the
// page.
function CoordinateListOf(aElement: TSVGElement; const aName: TSVGString;
  const aContext: TSVGLengthContext;
  aAxis: TSVGLengthAxis): TSVGDoubleArray;

var
  lLengths: TSVGLengthArray;
  I: Integer;

begin
  Result := nil;
  if not aElement.HasAttribute(aName) then
    Exit;
  if not TryStrToSVGLengthList(aElement.Attributes[aName], lLengths) then
    Exit;
  SetLength(Result, Length(lLengths));
  for I := 0 to High(lLengths) do
    Result[I] := aContext.Resolve(lLengths[I], aAxis);
end;


{ TSVGTextLayout }

constructor TSVGTextLayout.Create;

begin
  inherited Create;
end;


destructor TSVGTextLayout.Destroy;

begin
  Reset;
  inherited Destroy;
end;


procedure TSVGTextLayout.Reset;

var
  I: Integer;

begin
  for I := 0 to FCharCount - 1 do
    FChars[I].Font := nil;
  FCharCount := 0;
  FStretch := 1;
  FFrameCount := 0;
  for I := 0 to FSpanCount - 1 do
    FSpans[I].Font := nil;
  FSpanCount := 0;
  for I := 0 to FRunCount - 1 do
    begin
    FRuns[I].Font := nil;
    FRuns[I].Glyphs := nil;
    end;
  FRunCount := 0;
  FFonts := nil;
  FPathCount := 0;
  FBandCount := 0;
  FSpanPath := -1;
  FSawPath := False;
  FPending := False;
  FPendingSpan := -1;
  FPendingSerial := -1;
  FFrameSerial := 0;
  FStarted := False;
  FBounds := TSVGRect.Empty;
end;


function TSVGTextLayout.GetRun(aIndex: Integer): TSVGTextRun;

begin
  if (aIndex < 0) or (aIndex >= FRunCount) then
    raise ESVGText.CreateFmt(SErrRunIndexOutOfRange, [aIndex]);
  Result := FRuns[aIndex];
end;


function TSVGTextLayout.AddSpan(aElement: TSVGElement;
  const aStyle: TSVGComputedStyle; aParent: Integer): Integer;

var
  lRequest: TSVGFontRequest;
  lSize, lHeight, lShift: Double;

begin
  if FSpanCount = Length(FSpans) then
    SetLength(FSpans, Max(4, FSpanCount * 2));
  FSpans[FSpanCount].Element := aElement;
  FSpans[FSpanCount].Style := aStyle;
  FSpans[FSpanCount].Path := FSpanPath;
  lSize := FContext.Resolve(aStyle.FontSize, laVertical);
  lRequest := TSVGFontRequest.Create(aStyle.FontFamily, lSize);
  lRequest.Weight := aStyle.FontWeight;
  lRequest.Style := aStyle.FontStyle;
  lRequest.Variant := aStyle.FontVariant;
  lRequest.Stretch := aStyle.FontStretch;
  FSpans[FSpanCount].Small := nil;
  if FFonts = nil then
    FSpans[FSpanCount].Font := nil
  else
    begin
    FSpans[FSpanCount].Font := FFonts.ResolveFont(lRequest);
    // A face with real small capitals draws them itself. For a face
    // without them, a capital at a smaller size is used instead.
    if StandsInForSmallCaps(FSpanCount) then
      begin
      lRequest.Size := lSize * SVGSmallCapScale;
      FSpans[FSpanCount].Small := FFonts.ResolveFont(lRequest);
      end;
    end;
  // Without a face there is nothing to measure, so the size is used as the
  // height.
  lHeight := lSize;
  if FSpans[FSpanCount].Font <> nil then
    lHeight := FSpans[FSpanCount].Font.GetAscent
      + FSpans[FSpanCount].Font.GetDescent;
  lShift := 0;
  if aParent >= 0 then
    lShift := FSpans[aParent].Shift;
  FSpans[FSpanCount].Shift := lShift
    - aStyle.Shift.Resolve(lSize, lHeight, FContext.XHeight, FContext.DPI);
  Result := FSpanCount;
  Inc(FSpanCount);
end;


// The glyph a character is drawn with: the one the document chose for it,
// or the one its face holds for its code point.
function TSVGTextLayout.GlyphOf(aIndex: Integer; aFont: ISVGFont): Cardinal;

begin
  if FChars[aIndex].HasGlyph then
    Result := FChars[aIndex].Glyph
  else if aFont <> nil then
    Result := aFont.GetGlyphIndex(FChars[aIndex].CodePoint)
  else
    Result := 0;
end;


// Assigns a run of code points that one glyph covers to the first of
// them, and marks the rest as covered. The run has to sit in one span: a
// ligature belongs to a face, and a face belongs to a span.
procedure TSVGTextLayout.MergeRuns;

var
  I, J, lCount: Integer;
  lCodes: TSVGCodePointArray;
  lFont: ISVGFont;
  lGlyph: Cardinal;

begin
  if FCharCount = 0 then
    Exit;
  SetLength(lCodes, FCharCount);
  for I := 0 to FCharCount - 1 do
    lCodes[I] := FChars[I].CodePoint;
  I := 0;
  while I < FCharCount do
    begin
    lFont := FontFor(I);
    if (lFont = nil) or FChars[I].HasGlyph then
      begin
      Inc(I);
      Continue;
      end;
    lGlyph := lFont.GetGlyphForRun(lCodes, I, lCount);
    if (lGlyph = 0) or (lCount < 2) or (I + lCount > FCharCount) then
      begin
      Inc(I);
      Continue;
      end;
    // A run that reaches past its span, or over a character placed by a
    // list of positions, is not one glyph after all.
    lCount := SameSpanRun(I, lCount);
    if lCount < 2 then
      begin
      Inc(I);
      Continue;
      end;
    lGlyph := lFont.GetGlyphForRun(lCodes, I, J);
    if (lGlyph = 0) or (J <> lCount) then
      begin
      Inc(I);
      Continue;
      end;
    FChars[I].Glyph := lGlyph;
    FChars[I].HasGlyph := True;
    for J := I + 1 to I + lCount - 1 do
      FChars[J].Merged := True;
    Inc(I, lCount);
    end;
end;


// How much of a run of aCount code points from aFrom belongs to one span
// and is placed by the pen alone. Zero when the first one is not.
function TSVGTextLayout.SameSpanRun(aFrom, aCount: Integer): Integer;

var
  J: Integer;

begin
  Result := 1;
  for J := aFrom + 1 to aFrom + aCount - 1 do
    begin
    if (FChars[J].Span <> FChars[aFrom].Span)
    or FChars[J].HasX or FChars[J].HasY or FChars[J].HasDX or FChars[J].HasDY
    or FChars[J].HasGlyph then
      Exit;
    Inc(Result);
    end;
end;


// True when the element requests small capitals and its face has none, so
// that every lower case letter is drawn as a capital at a smaller size.
function TSVGTextLayout.StandsInForSmallCaps(aSpan: Integer): Boolean;

begin
  Result := (aSpan >= 0)
    and (FSpans[aSpan].Style.FontVariant = fvSmallCaps)
    and ((FSpans[aSpan].Font = nil) or not FSpans[aSpan].Font.GetSmallCaps);
end;


procedure TSVGTextLayout.AddChar(aCodePoint: Cardinal; aSpan: Integer);

var
  lCapital: Cardinal;

begin
  if FCharCount = Length(FChars) then
    SetLength(FChars, Max(16, FCharCount * 2));
  FillChar(FChars[FCharCount], SizeOf(TSVGTextChar), 0);
  if StandsInForSmallCaps(aSpan) then
    begin
    lCapital := CapitalOf(aCodePoint);
    if lCapital <> aCodePoint then
      begin
      aCodePoint := lCapital;
      FChars[FCharCount].Capital := True;
      end;
    end;
  FChars[FCharCount].CodePoint := aCodePoint;
  FChars[FCharCount].Span := aSpan;
  Inc(FCharCount);
end;


// Adds a position in the run for a glyph chosen by the document. It has
// no code point: an altGlyph may draw any glyphs in place of the
// characters it holds.
procedure TSVGTextLayout.AddGlyphChar(aGlyph: Cardinal; aSpan: Integer;
  aFace: ISVGFont);

begin
  if FCharCount = Length(FChars) then
    SetLength(FChars, Max(16, FCharCount * 2));
  FillChar(FChars[FCharCount], SizeOf(TSVGTextChar), 0);
  FChars[FCharCount].CodePoint := 0;
  FChars[FCharCount].Span := aSpan;
  FChars[FCharCount].Glyph := aGlyph;
  FChars[FCharCount].HasGlyph := True;
  FChars[FCharCount].Font := aFace;
  Inc(FCharCount);
end;


// The family a font element declares, taken from the font-face it holds.
function FamilyOfFontElement(aFont: TSVGElement): TSVGString;

var
  I: Integer;

begin
  Result := '';
  if aFont = nil then
    Exit;
  for I := 0 to aFont.ChildCount - 1 do
    if (aFont[I] is TSVGElement)
    and SameText(TSVGElement(aFont[I]).TagName, 'font-face') then
      Exit(Trim(TSVGElement(aFont[I]).AttributeDef('font-family', '')));
end;


// Draws an altGlyph with the glyphs it requests, in place of the
// characters it holds. A glyphRef points to a glyph element of the
// document, which need not belong to the font the run is drawn in, so
// each glyph is drawn with the face of the font that holds it. SVG 1.1
// makes the whole element fall back to its characters when one glyph
// cannot be found, so the list is resolved before any glyph is placed.
// False when it falls back.
function TSVGTextLayout.CollectAltGlyph(aElement: TSVGElement;
  aSpan: Integer): Boolean;

var
  lTarget: TSVGElement;
  lGlyphs: TSVGCodePointArray;
  lFaces: TSVGFontArray;
  lCount, I: Integer;
  lSize: Double;
  lHasItems: Boolean;

  // The glyph a reference points to, and the face of the font that holds
  // it. Zero when the reference leads nowhere, or the face is unavailable.
  function GlyphNamed(const aRef: TSVGString; out aFace: ISVGFont): Cardinal;

  var
    lGlyph: TSVGElement;
    lFamily: TSVGString;
    lRequest: TSVGFontRequest;

  begin
    Result := 0;
    aFace := nil;
    if (aElement.Document = nil) or (FFonts = nil) then
      Exit;
    lGlyph := aElement.Document.ResolveReference(aRef);
    if lGlyph = nil then
      Exit;
    lFamily := FamilyOfFontElement(lGlyph.Parent);
    if lFamily = '' then
      Exit;
    lRequest := TSVGFontRequest.Create(lFamily, lSize);
    aFace := FFonts.ResolveFont(lRequest);
    if aFace = nil then
      Exit;
    Result := aFace.GetGlyphNamed(SVGReferenceToID(aRef));
    if Result = 0 then
      aFace := nil;
  end;

  // Gathers the glyphs of the references under a node. False when one of
  // them leads nowhere, which makes the whole list unusable.
  function TakeGlyphList(aNode: TSVGElement): Boolean;

  var
    J: Integer;
    lOne: Cardinal;
    lFace: ISVGFont;

  begin
    Result := False;
    lCount := 0;
    for J := 0 to aNode.ChildCount - 1 do
      begin
      if not (aNode[J] is TSVGGlyphRefElement) then
        Continue;
      lOne := GlyphNamed(SVGHRefOf(TSVGElement(aNode[J])), lFace);
      if lOne = 0 then
        begin
        lCount := 0;
        Exit;
        end;
      if lCount = Length(lGlyphs) then
        begin
        SetLength(lGlyphs, lCount * 2 + 4);
        SetLength(lFaces, lCount * 2 + 4);
        end;
      lGlyphs[lCount] := lOne;
      lFaces[lCount] := lFace;
      Inc(lCount);
      end;
    Result := lCount > 0;
  end;

begin
  Result := False;
  if aElement.Document = nil then
    Exit;
  lSize := FContext.Resolve(FSpans[aSpan].Style.FontSize, laVertical);
  lTarget := aElement.Document.ResolveReference(SVGHRefOf(aElement));
  if lTarget = nil then
    Exit;
  SetLength(lGlyphs, 4);
  SetLength(lFaces, 4);
  lCount := 0;
  if lTarget is TSVGAltGlyphDefElement then
    begin
    lHasItems := False;
    for I := 0 to lTarget.ChildCount - 1 do
      if lTarget[I] is TSVGAltGlyphItemElement then
        lHasItems := True;
    if lHasItems then
      begin
      // The first list whose glyphs can all be found is the one that is
      // used, so a list with a missing glyph is skipped instead of
      // putting the whole element back to its characters.
      for I := 0 to lTarget.ChildCount - 1 do
        if (lTarget[I] is TSVGAltGlyphItemElement)
        and TakeGlyphList(TSVGElement(lTarget[I])) then
          Break;
      if lCount = 0 then
        Exit;
      end
    else if not TakeGlyphList(lTarget) then
      Exit;
    end
  else
    begin
    // The href may point to a single glyph instead of a list of them.
    SetLength(lFaces, 1);
    lGlyphs[0] := GlyphNamed(SVGHRefOf(aElement), lFaces[0]);
    if lGlyphs[0] = 0 then
      Exit;
    lCount := 1;
    end;
  for I := 0 to lCount - 1 do
    begin
    AddGlyphChar(lGlyphs[I], aSpan, lFaces[I]);
    ApplyFrames(FFrameCount);
    end;
  FStarted := True;
  Result := True;
end;


procedure TSVGTextLayout.PushFrame(aElement: TSVGElement;
  const aStyle: TSVGComputedStyle);

begin
  if FFrameCount = Length(FFrames) then
    SetLength(FFrames, Max(4, FFrameCount * 2));
  FFrames[FFrameCount].X := CoordinateListOf(aElement, 'x', FContext,
    laHorizontal);
  FFrames[FFrameCount].Y := CoordinateListOf(aElement, 'y', FContext,
    laVertical);
  FFrames[FFrameCount].DX := CoordinateListOf(aElement, 'dx', FContext,
    laHorizontal);
  FFrames[FFrameCount].DY := CoordinateListOf(aElement, 'dy', FContext,
    laVertical);
  FFrames[FFrameCount].Rot := NumberListOf(aElement, 'rotate');
  FFrames[FFrameCount].Base := FCharCount;
  FFrames[FFrameCount].Serial := FFrameSerial;
  Inc(FFrameSerial);
  Inc(FFrameCount);
end;


procedure TSVGTextLayout.PopFrame;

begin
  if FFrameCount > 0 then
    Dec(FFrameCount);
end;


// How many of the open elements were already open when the waiting space
// was read. Frames open in order, so those are the first ones.
function TSVGTextLayout.OpenWhenPending: Integer;

begin
  Result := 0;
  while (Result < FFrameCount)
        and (FFrames[Result].Serial <= FPendingSerial) do
    Inc(Result);
end;


// Gives the character just added the positions the open elements specify
// for it. Only the first aDepth frames are read, which is every open
// element for an ordinary character, and the elements a waiting space was
// read in.
procedure TSVGTextLayout.ApplyFrames(aDepth: Integer);

var
  I, J, K: Integer;

begin
  // The innermost element that gives a position for a character wins, so
  // the stack is read from the top down and the first entry found is
  // taken.
  for I := aDepth - 1 downto 0 do
    begin
    J := FCharCount - 1;
    K := J - FFrames[I].Base;
    if K < 0 then
      Continue;
    if not FChars[J].HasX and (K < Length(FFrames[I].X)) then
      begin
      FChars[J].HasX := True;
      FChars[J].X := FFrames[I].X[K];
      end;
    if not FChars[J].HasY and (K < Length(FFrames[I].Y)) then
      begin
      FChars[J].HasY := True;
      FChars[J].Y := FFrames[I].Y[K];
      end;
    if not FChars[J].HasDX and (K < Length(FFrames[I].DX)) then
      begin
      FChars[J].HasDX := True;
      FChars[J].DX := FFrames[I].DX[K];
      end;
    if not FChars[J].HasDY and (K < Length(FFrames[I].DY)) then
      begin
      FChars[J].HasDY := True;
      FChars[J].DY := FFrames[I].DY[K];
      end;
    end;
  // A rotate list continues past its end: the last angle rotates every
  // character after it. The innermost element with any angle therefore
  // gives this one, and no outer list is read.
  for I := aDepth - 1 downto 0 do
    begin
    J := FCharCount - 1;
    K := J - FFrames[I].Base;
    if (K < 0) or (Length(FFrames[I].Rot) = 0) then
      Continue;
    if K > High(FFrames[I].Rot) then
      K := High(FFrames[I].Rot);
    FChars[J].Angle := FFrames[I].Rot[K];
    Break;
    end;
end;


procedure TSVGTextLayout.CollectText(const aText: TSVGString; aSpan: Integer;
  aPreserve: Boolean);

var
  I, lIndex, lDepth: Integer;
  lCode: Cardinal;

begin
  lIndex := 1;
  while lIndex <= Length(aText) do
    begin
    lCode := SVGNextCodePoint(aText, lIndex);
    if IsTextSpace(lCode) then
      begin
      if aPreserve then
        begin
        AddChar(32, aSpan);
        ApplyFrames(FFrameCount);
        FStarted := True;
        end
      else if FStarted then
        begin
        // The space represents whitespace of this element. It is added only
        // once a character follows it, which may be in a child.
        if not FPending then
          begin
          FPendingSpan := aSpan;
          FPendingSerial := FFrameSerial - 1;
          end;
        FPending := True;
        end;
      Continue;
      end;
    if FPending then
      begin
      AddChar(32, FPendingSpan);
      // The elements opened since the space was read give no position
      // for it, and their lists address the characters after it, so each
      // of those lists starts one character later.
      lDepth := OpenWhenPending;
      for I := lDepth to FFrameCount - 1 do
        Inc(FFrames[I].Base);
      ApplyFrames(lDepth);
      FPending := False;
      end;
    AddChar(lCode, aSpan);
    ApplyFrames(FFrameCount);
    FStarted := True;
    end;
end;


procedure TSVGTextLayout.Collect(aElement: TSVGElement; aSpan: Integer;
  const aStyle: TSVGComputedStyle; aPreserve: Boolean);

var
  I, lSpan, lOuter, lBand: Integer;
  lChild: TSVGElement;
  lChildStyle: TSVGComputedStyle;
  lLines: TSVGTextDecorations;
  lPreserve: Boolean;

begin
  for I := 0 to aElement.ChildCount - 1 do
    begin
    if aElement[I] is TSVGTextNode then
      begin
      CollectText(TSVGTextNode(aElement[I]).Text, aSpan, aPreserve);
      Continue;
      end;
    if not (aElement[I] is TSVGElement) then
      Continue;
    lChild := TSVGElement(aElement[I]);
    // A link inside a text holds characters of that text and is laid out
    // like a tspan, apart from the position attributes it cannot have.
    if not ((lChild is TSVGTSpanElement) or (lChild is TSVGTextPathElement)
            or (lChild is TSVGTRefElement) or (lChild is TSVGAElement)
            or (lChild is TSVGAltGlyphElement)) then
      Continue;
    lChildStyle := FStyles.ComputeStyle(lChild, aStyle);
    if not lChildStyle.IsDisplayed then
      Continue;
    lPreserve := PreservesSpaceHere(lChild, aPreserve);
    lOuter := FSpanPath;
    try
      if lChild is TSVGTextPathElement then
        begin
        FSawPath := True;
        FSpanPath := AddPath(lChild);
        if FSpanPath < 0 then
          FSpanPath := SVGTextPathMissing;
        end;
      lSpan := AddSpan(lChild, lChildStyle, aSpan);
      lLines := SVGDecorationsOf(SVGDeclaredValue(lChild, 'text-decoration'));
      lBand := -1;
      if lLines <> [] then
        lBand := OpenBand(lChildStyle, FSpans[lSpan].Font, lLines);
      PushFrame(lChild, lChildStyle);
      try
        if lChild is TSVGTRefElement then
          CollectReferenced(lChild, lSpan, lPreserve)
        // An altGlyph whose glyphs cannot all be found is drawn with
        // its own characters, like a tspan.
        else if not ((lChild is TSVGAltGlyphElement)
                     and CollectAltGlyph(lChild, lSpan)) then
          Collect(lChild, lSpan, lChildStyle, lPreserve);
      finally
        PopFrame;
      end;
      if lBand >= 0 then
        FBands[lBand].Last := FCharCount - 1;
    finally
      FSpanPath := lOuter;
    end;
    end;
end;


function TSVGTextLayout.GetBand(aIndex: Integer): TSVGTextBand;

begin
  Result := FBands[aIndex];
end;


// Starts a line that an element requested. It runs from the next
// character collected to the last one that element contributes.
function TSVGTextLayout.OpenBand(const aStyle: TSVGComputedStyle;
  aFont: ISVGFont; aLines: TSVGTextDecorations): Integer;

begin
  if FBandCount = Length(FBands) then
    SetLength(FBands, Max(4, FBandCount * 2));
  // A space seen before this element is added only once a character
  // follows it, and that character lands inside this element. The line
  // does not run under that space.
  FBands[FBandCount].Waiting := FPending;
  FBands[FBandCount].Style := aStyle;
  FBands[FBandCount].Font := aFont;
  FBands[FBandCount].Lines := aLines;
  FBands[FBandCount].First := FCharCount;
  FBands[FBandCount].Last := FCharCount - 1;
  Result := FBandCount;
  Inc(FBandCount);
end;


// The first character the line runs under, skipping a space that was
// waiting to be added when the line opened.
function TSVGTextLayout.FirstOfBand(aBand: Integer): Integer;

begin
  Result := FBands[aBand].First;
  if FBands[aBand].Waiting and (Result <= FBands[aBand].Last)
     and (FChars[Result].CodePoint = 32) then
    Inc(Result);
end;


// Bounds every line by the characters it runs under, once those have been
// placed.
procedure TSVGTextLayout.MeasureBands;

var
  I, J: Integer;
  lFirst: Boolean;

begin
  for I := 0 to FBandCount - 1 do
    begin
    FBands[I].Left := 0;
    FBands[I].Right := 0;
    FBands[I].Baseline := 0;
    FBands[I].Turned := False;
    FBands[I].Vertical := FVertical;
    lFirst := True;
    for J := FirstOfBand(I) to FBands[I].Last do
      begin
      if FChars[J].Dropped then
        Continue;
      if FSpans[FChars[J].Span].Path >= 0 then
        FBands[I].Turned := True;
      if lFirst then
        begin
        FBands[I].Left := Along(J);
        FBands[I].Right := Along(J);
        FBands[I].Baseline := Across(J);
        lFirst := False;
        end;
      if Along(J) < FBands[I].Left then
        FBands[I].Left := Along(J);
      if Along(J) + FChars[J].Advance > FBands[I].Right then
        FBands[I].Right := Along(J) + FChars[J].Advance;
      end;
    end;
end;


function TSVGTextLayout.FontFor(aChar: Integer): ISVGFont;

var
  lSpan: Integer;
  lStyle: TSVGComputedStyle;
  lRequest: TSVGFontRequest;
  lCover: ISVGFont;

begin
  Result := FChars[aChar].Font;
  if Result <> nil then
    Exit;
  lSpan := FChars[aChar].Span;
  if FChars[aChar].Capital and (FSpans[lSpan].Small <> nil) then
    Result := FSpans[lSpan].Small
  else
    Result := FSpans[lSpan].Font;
  FChars[aChar].Font := Result;
  if (Result = nil) or (FFonts = nil) then
    Exit;
  // A space takes up room without a glyph, and a face that has the
  // character needs no substitute.
  if IsTextSpace(FChars[aChar].CodePoint) then
    Exit;
  if Result.GetGlyphIndex(FChars[aChar].CodePoint) <> 0 then
    Exit;
  lStyle := FSpans[lSpan].Style;
  lRequest := TSVGFontRequest.Create(lStyle.FontFamily,
    FContext.Resolve(lStyle.FontSize, laVertical));
  lRequest.Weight := lStyle.FontWeight;
  lRequest.Style := lStyle.FontStyle;
  lRequest.Variant := lStyle.FontVariant;
  lRequest.Stretch := lStyle.FontStretch;
  lCover := FFonts.ResolveCover(FChars[aChar].CodePoint, lRequest);
  if lCover = nil then
    Exit;
  Result := lCover;
  FChars[aChar].Font := lCover;
end;


// How far the baseline of a character moves from the point it was given:
// the amount from dominant-baseline plus the amount from baseline-shift.
// A positive result moves it down the page.
function TSVGTextLayout.BaselineShift(aChar: Integer;
  aFont: ISVGFont): Double;

var
  lAscent, lDescent: Double;

begin
  Result := FSpans[FChars[aChar].Span].Shift;
  if aFont = nil then
    Exit;
  lAscent := aFont.GetAscent;
  lDescent := aFont.GetDescent;
  case FSpans[FChars[aChar].Span].Style.Baseline of
    // Half the height of a lower case letter. SVG measures the middle
    // baseline from there.
    dbMiddle: Result := Result + FContext.XHeight / 2;
    dbCentral: Result := Result + (lAscent - lDescent) / 2;
    dbMathematical: Result := Result + lAscent / 2;
    // The hanging baseline sits near the top of the letters, so the text
    // hangs below the point it was given.
    dbHanging: Result := Result + lAscent * 0.8;
    dbBeforeEdge: Result := Result + lAscent;
    dbAfterEdge, dbIdeographic: Result := Result - lDescent;
  end;
end;


procedure TSVGTextLayout.Place;

var
  I: Integer;
  lPenX, lPenY, lAdvance: Double;
  lFont, lLastFont: ISVGFont;
  lGlyph, lLastGlyph: Cardinal;
  lLastTurn: Double;
  lStyle: TSVGComputedStyle;

begin
  lPenX := 0;
  lPenY := 0;
  lLastFont := nil;
  lLastGlyph := 0;
  lLastTurn := 0;
  for I := 0 to FCharCount - 1 do
    begin
    lFont := FontFor(I);
    lStyle := FSpans[FChars[I].Span].Style;
    if FChars[I].Merged then
      begin
      FChars[I].X := lPenX;
      FChars[I].Y := lPenY;
      FChars[I].Advance := 0;
      Continue;
      end;
    lGlyph := GlyphOf(I, lFont);
    FChars[I].Turn := TurnOf(I);
    // A pair that the face kerns is set closer together, but only when
    // this character takes its position from the pen instead of being
    // placed on its own.
    if FChars[I].HasX then
      lPenX := FChars[I].X
    else if not FVertical and (lFont <> nil) and (lFont = lLastFont) then
      lPenX := lPenX - lFont.GetGlyphKerning(lLastGlyph, lGlyph);
    if FChars[I].HasY then
      lPenY := FChars[I].Y
    else if FVertical and (lFont <> nil) and (lFont = lLastFont) then
      lPenY := lPenY - SVGKernDown(lFont, lLastGlyph, lGlyph, lLastTurn,
        FChars[I].Turn);
    if FChars[I].HasDX then
      lPenX := lPenX + FChars[I].DX;
    if FChars[I].HasDY then
      lPenY := lPenY + FChars[I].DY;
    // The shift moves the glyph, not the pen: the next character starts
    // from the baseline this one was given. In a column the baseline runs
    // down the page, so the shift moves the glyph across it, and it is
    // subtracted because the two count in opposite directions.
    if FVertical then
      begin
      FChars[I].X := lPenX - BaselineShift(I, lFont);
      FChars[I].Y := lPenY;
      end
    else
      begin
      FChars[I].X := lPenX;
      FChars[I].Y := lPenY + BaselineShift(I, lFont);
      end;
    FChars[I].Angle := FChars[I].Angle + FChars[I].Turn;
    lAdvance := 0;
    if lFont <> nil then
      // A glyph laid on its side runs down the column on the width it
      // would have taken across the page.
      if FVertical and not SVGLaidSideways(FChars[I].Turn) then
        lAdvance := lFont.GetGlyphVerticalAdvance(lGlyph)
      else
        lAdvance := lFont.GetGlyphAdvance(lGlyph);
    lAdvance := lAdvance + lStyle.LetterSpacing;
    if FChars[I].CodePoint = 32 then
      lAdvance := lAdvance + lStyle.WordSpacing;
    if FVertical then
      lPenY := lPenY + lAdvance
    else
      lPenX := lPenX + lAdvance;
    FChars[I].Advance := lAdvance;
    lLastFont := lFont;
    lLastGlyph := lGlyph;
    lLastTurn := FChars[I].Turn;
    end;
end;


// The quarter turn a glyph takes in a column. Only a vertical writing
// mode turns anything, and auto keeps the ideographic and full width
// characters upright and lays the rest on their side.
function TSVGTextLayout.TurnOf(aChar: Integer): Double;

begin
  Result := 0;
  if not FVertical then
    Exit;
  case FSpans[FChars[aChar].Span].Style.GlyphOrientation of
    go90: Result := 90;
    go180: Result := 180;
    go270: Result := 270;
    goAuto:
      if not SVGStandsUpright(FChars[aChar].CodePoint) then
        Result := 90;
  end;
end;


// The coordinate a line runs along: down the page for a vertical writing
// mode and across it otherwise.
function TSVGTextLayout.Along(aChar: Integer): Double;

begin
  if FVertical then
    Result := FChars[aChar].Y
  else
    Result := FChars[aChar].X;
end;


// The coordinate across a line: the one a decoration is set off from.
function TSVGTextLayout.Across(aChar: Integer): Double;

begin
  if FVertical then
    Result := FChars[aChar].X
  else
    Result := FChars[aChar].Y;
end;


procedure TSVGTextLayout.MoveAlong(aChar: Integer; aBy: Double);

begin
  if FVertical then
    FChars[aChar].Y := FChars[aChar].Y + aBy
  else
    FChars[aChar].X := FChars[aChar].X + aBy;
end;


procedure TSVGTextLayout.PutAlong(aChar: Integer; aTo: Double);

begin
  if FVertical then
    FChars[aChar].Y := aTo
  else
    FChars[aChar].X := aTo;
end;


// A chunk opens wherever a character was given a position of its own
// along the line: an x for a line running across the page, a y for one
// running down it.
function TSVGTextLayout.OpensChunk(aChar: Integer): Boolean;

begin
  if FVertical then
    Result := FChars[aChar].HasY
  else
    Result := FChars[aChar].HasX;
end;


procedure TSVGTextLayout.AnchorChunks(aElement: TSVGElement);

var
  I, J, lStart: Integer;
  lLeft, lRight, lShift, lTarget, lSpare: Double;
  lLength: TSVGLength;
  lAdjust: TSVGLengthAdjust;

begin
  lTarget := -1;
  if aElement.HasAttribute('textLength')
     and lLength.TryParse(aElement.Attributes['textLength'])
     and (lLength.Value >= 0) then
    if FVertical then
      lTarget := FContext.Resolve(lLength, laVertical)
    else
      lTarget := FContext.Resolve(lLength, laHorizontal);
  lAdjust := laSpacing;
  // A glyph is only ever drawn wider, never taller, so a line running
  // down the page spreads the gaps for either value.
  if not FVertical
     and SameText(Trim(aElement.AttributeDef('lengthAdjust', 'spacing')),
                  'spacingAndGlyphs') then
    lAdjust := laSpacingAndGlyphs;
  // textLength measures the whole element, so it is spread before the chunks
  // are anchored rather than inside each one.
  if (lTarget >= 0) and (FCharCount > 1) then
    begin
    lLeft := Along(0);
    lRight := Along(FCharCount - 1) + FChars[FCharCount - 1].Advance;
    lSpare := lTarget - (lRight - lLeft);
    if lAdjust = laSpacingAndGlyphs then
      begin
      // Both the gaps and the letters widen, so the whole run is scaled
      // about its start and every glyph is drawn that much wider.
      if lRight - lLeft > 0 then
        begin
        FStretch := lTarget / (lRight - lLeft);
        for I := 0 to FCharCount - 1 do
          begin
          FChars[I].X := lLeft + (FChars[I].X - lLeft) * FStretch;
          FChars[I].Advance := FChars[I].Advance * FStretch;
          end;
        end;
      end
    else
      for I := 1 to FCharCount - 1 do
        MoveAlong(I, lSpare * I / (FCharCount - 1));
    end;
  lStart := 0;
  I := 0;
  while I <= FCharCount do
    begin
    if (I = FCharCount) or ((I > lStart) and OpensChunk(I)) then
      begin
      lLeft := Along(lStart);
      lRight := Along(I - 1) + FChars[I - 1].Advance;
      lShift := 0;
      case FSpans[FChars[lStart].Span].Style.TextAnchor of
        taMiddle: lShift := -(lRight - lLeft) / 2;
        taEnd: lShift := -(lRight - lLeft);
      end;
      if lShift <> 0 then
        for J := lStart to I - 1 do
          MoveAlong(J, lShift);
      lStart := I;
      end;
    Inc(I);
    end;
end;


// Collects the text of the element a tref points to, in place of the
// content of the tref itself. Only character data is taken, so the child
// elements of the target contribute their text and nothing else.
procedure TSVGTextLayout.CollectReferenced(aElement: TSVGElement;
  aSpan: Integer; aPreserve: Boolean);

var
  lTarget: TSVGElement;

  procedure Walk(aNode: TSVGElement);
  var
    I: Integer;
  begin
    for I := 0 to aNode.ChildCount - 1 do
      if aNode[I] is TSVGTextNode then
        CollectText(TSVGTextNode(aNode[I]).Text, aSpan, aPreserve)
      else if aNode[I] is TSVGElement then
        Walk(TSVGElement(aNode[I]));
  end;

begin
  if aElement.Document = nil then
    Exit;
  lTarget := aElement.Document.ResolveReference(SVGHRefOf(aElement));
  if (lTarget = nil) or (lTarget = aElement) then
    Exit;
  Walk(lTarget);
end;


function TSVGTextLayout.AddPath(aElement: TSVGElement): Integer;

var
  lTarget: TSVGElement;
  lPath: TSVGPath;
  lLength: TSVGLength;
  lTotal: Double;

begin
  Result := -1;
  if aElement.Document = nil then
    Exit;
  lTarget := aElement.Document.ResolveReference(SVGHRefOf(aElement));
  if not (lTarget is TSVGPathElement) then
    Exit;
  if FPathCount = Length(FPaths) then
    SetLength(FPaths, Max(2, FPathCount * 2));
  lPath := TSVGPath.Create;
  try
    if not BuildSVGShapePath(lTarget, lPath, FContext) then
      Exit;
    FPaths[FPathCount].Metrics := TSVGPathMetrics.Create(lPath,
      SVGDefaultFlatness);
  finally
    lPath.Free;
  end;
  lTotal := FPaths[FPathCount].Metrics.TotalLength;
  if lTotal <= 0 then
    Exit;
  FPaths[FPathCount].Offset := 0;
  if aElement.HasAttribute('startOffset')
     and lLength.TryParse(aElement.Attributes['startOffset']) then
    if lLength.LengthUnit = luPercent then
      FPaths[FPathCount].Offset := lTotal * lLength.Value / 100
    else
      FPaths[FPathCount].Offset := FContext.Resolve(lLength, laHorizontal);
  Result := FPathCount;
  Inc(FPathCount);
end;


procedure TSVGTextLayout.MapOntoPaths;

var
  I, lPath: Integer;
  lDistance, lOffset: Double;
  lPoint, lTangent: TSVGPoint;

begin
  for I := 0 to FCharCount - 1 do
    begin
    lPath := FSpans[FChars[I].Span].Path;
    // A textPath that points to something other than a path is in error.
    // The characters it holds are left undrawn instead of being piled up
    // at the origin.
    if lPath = SVGTextPathMissing then
      begin
      FChars[I].Dropped := True;
      Continue;
      end;
    if lPath < 0 then
      Continue;
    lDistance := FPaths[lPath].Offset + FChars[I].X + FChars[I].Advance / 2;
    if not FPaths[lPath].Metrics.PlaceAt(lDistance, lPoint, lTangent) then
      begin
      FChars[I].Dropped := True;
      Continue;
      end;
    // The middle of the glyph's advance sits on the path and the glyph
    // rotates with the tangent there, so its origin steps back half an
    // advance. Its own y moves it off the path along the normal.
    lOffset := FChars[I].Y;
    FChars[I].Angle := FChars[I].Angle
      + RadToDeg(ArcTan2(lTangent.Y, lTangent.X));
    FChars[I].X := lPoint.X - lTangent.X * FChars[I].Advance / 2
      - lTangent.Y * lOffset;
    FChars[I].Y := lPoint.Y - lTangent.Y * FChars[I].Advance / 2
      + lTangent.X * lOffset;
    end;
end;


procedure TSVGTextLayout.BuildRuns;

var
  I, lCount: Integer;
  lSpan: Integer;
  lFont: ISVGFont;
  lGlyph: Cardinal;
  lTop, lBottom, lLeft, lRight, lX, lY: Double;

begin
  I := 0;
  while I < FCharCount do
    begin
    lSpan := FChars[I].Span;
    lFont := FChars[I].Font;
    if lFont = nil then
      begin
      Inc(I);
      Continue;
      end;
    if FRunCount = Length(FRuns) then
      SetLength(FRuns, Max(4, FRunCount * 2));
    FRuns[FRunCount].Element := FSpans[lSpan].Element;
    FRuns[FRunCount].Font := lFont;
    FRuns[FRunCount].Style := FSpans[lSpan].Style;
    FRuns[FRunCount].Left := FChars[I].X;
    FRuns[FRunCount].Right := FChars[I].X;
    FRuns[FRunCount].Baseline := FChars[I].Y;
    FRuns[FRunCount].Turned := False;
    lCount := 0;
    SetLength(FRuns[FRunCount].Glyphs, 8);
    while (I < FCharCount) and (FChars[I].Span = lSpan)
          and (FChars[I].Font = lFont) do
      begin
      if FChars[I].Dropped or FChars[I].Merged then
        begin
        Inc(I);
        Continue;
        end;
      lGlyph := GlyphOf(I, lFont);
      if lCount = Length(FRuns[FRunCount].Glyphs) then
        SetLength(FRuns[FRunCount].Glyphs, lCount * 2);
      lX := FChars[I].X;
      lY := FChars[I].Y;
      // The pen sits on the vertical baseline while the outline is drawn
      // from the horizontal origin, so the pen is moved from the one to
      // the other here and nowhere else.
      if FVertical and SVGLaidSideways(FChars[I].Turn) then
        // A quarter turn puts the width of the glyph down the column and
        // its height across it, so it is centred on the column.
        lX := lX - (lFont.GetAscent - lFont.GetDescent) / 2
      else if FVertical then
        begin
        lFont.GetGlyphVerticalOrigin(lGlyph, lLeft, lTop);
        lX := lX + lLeft;
        lY := lY + lTop;
        end;
      FRuns[FRunCount].Glyphs[lCount] := TSVGGlyph.CreateTurned(lGlyph,
        lX, lY, FChars[I].Angle);
      FRuns[FRunCount].Glyphs[lCount].Stretch := FStretch;
      if FSpans[FChars[I].Span].Path >= 0 then
        FRuns[FRunCount].Turned := True;
      if FVertical and SVGLaidSideways(FChars[I].Turn) then
        begin
        lLeft := lX - lFont.GetDescent;
        lRight := lX + lFont.GetAscent;
        lTop := FChars[I].Y;
        lBottom := FChars[I].Y + FChars[I].Advance;
        end
      else if FVertical then
        begin
        lLeft := lX;
        lRight := lX + lFont.GetGlyphAdvance(lGlyph);
        lTop := FChars[I].Y;
        lBottom := FChars[I].Y + FChars[I].Advance;
        end
      else
        begin
        lLeft := FChars[I].X;
        lRight := FChars[I].X + FChars[I].Advance;
        lTop := FChars[I].Y - lFont.GetAscent;
        lBottom := FChars[I].Y + lFont.GetDescent;
        end;
      if lLeft < FRuns[FRunCount].Left then
        FRuns[FRunCount].Left := lLeft;
      if lRight > FRuns[FRunCount].Right then
        FRuns[FRunCount].Right := lRight;
      Inc(lCount);
      FBounds := FBounds.Union(TSVGRect.Create(lLeft, lTop, lRight, lBottom));
      Inc(I);
      end;
    SetLength(FRuns[FRunCount].Glyphs, lCount);
    if lCount = 0 then
      FRuns[FRunCount].Font := nil
    else
      Inc(FRunCount);
    end;
end;


function TSVGTextLayout.Layout(aText: TSVGElement;
  aStyles: TSVGStyleResolver; aFonts: ISVGFontProvider;
  const aStyle: TSVGComputedStyle;
  const aContext: TSVGLengthContext): Boolean;

var
  lSpan, lBand: Integer;
  lLines: TSVGTextDecorations;

begin
  Reset;
  Result := False;
  if (aText = nil) or (aStyles = nil) then
    Exit;
  FStyles := aStyles;
  FFonts := aFonts;
  FContext := aContext;
  lSpan := AddSpan(aText, aStyle, -1);
  // A line on the text element, or on a group above it, runs under
  // everything the text holds and takes the paint of the text element.
  lLines := aStyle.Decoration;
  lBand := -1;
  if lLines <> [] then
    lBand := OpenBand(aStyle, FSpans[lSpan].Font, lLines);
  PushFrame(aText, aStyle);
  try
    Collect(aText, lSpan, aStyle, SVGPreservesSpace(aText));
  finally
    PopFrame;
  end;
  if lBand >= 0 then
    FBands[lBand].Last := FCharCount - 1;
  if FCharCount = 0 then
    Exit;
  // Text on a path follows the path, whatever the writing mode says.
  FVertical := (aStyle.WritingMode = wmTB) and not FSawPath;
  MergeRuns;
  Place;
  AnchorChunks(aText);
  if FSawPath then
    MapOntoPaths;
  MeasureBands;
  BuildRuns;
  Result := FRunCount > 0;
end;


end.
