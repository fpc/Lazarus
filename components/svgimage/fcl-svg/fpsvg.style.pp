{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    The SVG property cascade, inheritance and computed values.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.style;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}
{$modeswitch advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.Math, System.StrUtils,
     FpCss.Tree, FpCss.Parser, FpCss.ValueParser, FpCss.Resolver,
     fpsvg.types, fpsvg.dom, fpsvg.read;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, math, strutils, fpcsstree, fpcssparser,
     fpcssresparser, fpcssresolver, fpsvg.types, fpsvg.dom,
     fpsvg.read;
{$ENDIF FPC_DOTTEDUNITS}

type
  ESVGStyle = class(ESVGError);

  TSVGDisplay = (sdInline, sdNone);
  TSVGVisibility = (svVisible, svHidden, svCollapse);
  TSVGOverflow = (ovVisible, ovHidden);
  TSVGTextAnchor = (taStart, taMiddle, taEnd);

  { Which way a text runs. wmLRTB lays the characters across the page and
    is what SVG uses when a document sets none; wmTB lays them down it. }
  TSVGWritingMode = (wmLRTB, wmTB);

  { How a glyph is turned in text running down the page.
    goAuto keeps the ideographic and full width glyphs upright and turns
    the rest a quarter turn clockwise. SVG uses it when a document sets
    none. }
  TSVGGlyphOrientation = (goAuto, go0, go90, go180, go270);

  { Where the baseline of a text run sits, relative to the point it was
    given. dbAuto leaves it on the baseline, and SVG uses it when a
    document sets none. }
  TSVGBaseline = (dbAuto, dbMiddle, dbCentral, dbHanging, dbMathematical,
                  dbBeforeEdge, dbAfterEdge, dbIdeographic);

  { The lines a text run is drawn with. A run may have more than one. }
  TSVGTextDecoration = (tdUnderline, tdOverline, tdLineThrough);
  TSVGTextDecorations = set of TSVGTextDecoration;

  { Whether the lower case letters of a text are drawn as small capitals. }
  TSVGFontVariant = fpsvg.types.TSVGFontVariant;

  { The kind of a baseline-shift value. skBaseline leaves the baseline
    alone, and is what SVG uses when a document sets none. }
  TSVGShiftKind = (skBaseline, skSub, skSuper, skLength);

  { A baseline-shift value. Amount is read for skLength only: a length, or
    a percentage of the font size. }
  TSVGBaselineShift = record
    Kind   : TSVGShiftKind;
    Amount : TSVGLength;
    // A value that leaves the baseline unchanged.
    class function None: TSVGBaselineShift; static;
    // How far up the page the shift moves the baseline, in user units.
    // A negative result moves it down. aFaceHeight is the height of the
    // face, from its ascent to its descent.
    function Resolve(aFontSize, aFaceHeight, aXHeight, aDPI: Double): Double;
  end;

  { One side of a clip rectangle. Offset moves that side inwards from the
    side of the viewport it faces; an auto side stays where it is. }
  TSVGClipEdge = record
    IsAuto : Boolean;
    Offset : TSVGLength;
    // A side that stays on the viewport.
    class function Auto: TSVGClipEdge; static;
  end;

  { The rectangle of a clip value. Shaped is False for the auto value,
    which clips to the viewport and no tighter. }
  TSVGClipShape = record
    Shaped : Boolean;
    Top    : TSVGClipEdge;
    Right  : TSVGClipEdge;
    Bottom : TSVGClipEdge;
    Left   : TSVGClipEdge;
    // The value that clips to the viewport alone.
    class function Auto: TSVGClipShape; static;
    // The viewport with each given side moved inwards, in the coordinates
    // of the viewport itself. Empty when the sides cross.
    function Narrow(const aViewport: TSVGRect;
      const aContext: TSVGLengthContext): TSVGRect;
    // "auto", or the four sides in rect() notation.
    function ToString: TSVGString;
  end;

  { Every SVG property the cascade resolves. }
  TSVGProperty = (
    prFill, prFillOpacity, prFillRule,
    prStroke, prStrokeOpacity, prStrokeWidth, prStrokeLinecap,
    prStrokeLinejoin, prStrokeMiterlimit, prStrokeDasharray, prStrokeDashoffset,
    prOpacity, prColor, prDisplay, prVisibility,
    prFontFamily, prFontSize, prClipRule,
    prOverflow, prClipPath, prMask, prFilter,
    prTextAnchor, prFontWeight, prFontStyle, prLetterSpacing, prWordSpacing,
    prColorInterpolation, prFilterInterpolation,
    prMarkerStart, prMarkerMid, prMarkerEnd,
    prDominantBaseline, prAlignmentBaseline, prTextDecoration,
    prBaselineShift, prFontStretch, prFontVariant,
    prStopColor, prStopOpacity, prClip,
    prWritingMode, prGlyphOrientationVertical);
  TSVGProperties = set of TSVGProperty;

  { The resolved property set of one element. }
  TSVGComputedStyle = record
    Fill             : TSVGPaint;
    FillOpacity      : Double;
    FillRule         : TSVGFillRule;
    Stroke           : TSVGPaint;
    StrokeOpacity    : Double;
    Pen              : TSVGPen;
    Opacity          : Double;
    Color            : TSVGColor;
    Display          : TSVGDisplay;
    Visibility       : TSVGVisibility;
    FontFamily       : TSVGString;
    StopColor        : TSVGColor;
    StopOpacity      : Double;
    FontSize         : TSVGLength;
    ClipRule         : TSVGFillRule;
    Overflow         : TSVGOverflow;
    Clip             : TSVGClipShape;
    ClipPath         : TSVGString;
    Mask             : TSVGString;
    Filter           : TSVGString;
    TextAnchor       : TSVGTextAnchor;
    FontWeight       : Integer;
    FontStyle        : TSVGFontStyle;
    FontStretch      : TSVGFontStretch;
    FontVariant      : TSVGFontVariant;
    LetterSpacing    : Double;
    WordSpacing      : Double;
    ColorInterpolation : TSVGColorInterpolation;
    FilterInterpolation : TSVGColorInterpolation;
    MarkerStart      : TSVGString;
    MarkerMid        : TSVGString;
    MarkerEnd        : TSVGString;
    Baseline         : TSVGBaseline;
    Shift            : TSVGBaselineShift;
    Decoration       : TSVGTextDecorations;
    WritingMode      : TSVGWritingMode;
    GlyphOrientation : TSVGGlyphOrientation;
    // The value SVG 1.1 gives every property when a document sets none.
    class function Initial: TSVGComputedStyle; static;
    // The style a child starts from: the inherited properties are copied,
    // and the rest go back to their SVG initial values.
    function Inherit: TSVGComputedStyle;
    // True when the element and its children draw something.
    function IsDisplayed: Boolean;
    // True when the element itself paints, ignoring its children.
    function IsPainted: Boolean;
    // The property values as text, one per line, in declaration order.
    function ToString: TSVGString;
  end;

  { Parses a paint value. It needs the document to look up server
    references. }
  TSVGPaintStyleHelper = record helper for TSVGPaint
    // Parses none, a colour, currentColor, or a url() server reference.
    function TryParse(const aText: TSVGString; const aCurrentColor: TSVGColor;
      aDocument: TSVGDocument): Boolean;
  end;

  { A font that a stylesheet declared, and the files it offers for it.
    The sources are in the order the rule lists them, best first. }
  TSVGFontFaceRule = record
    Family  : TSVGString;
    Weight  : Integer;
    Style   : TSVGFontStyle;
    Sources : TStringArray;
  end;
  TSVGFontFaceRuleArray = array of TSVGFontFaceRule;

  { Cascade over a document: stylesheets, presentation attributes and
    inheritance, resolved into a TSVGComputedStyle per element. }
  TSVGStyleResolver = class(TObject)
  private
    FResolver: TCSSResolver;
    FRegistry: TCSSRegistry;
    FLinkHistory: ISVGLinkHistory;
    FDocument: TSVGDocument;
    FAttributeIDs: array[TSVGProperty] of TCSSNumericalID;
    FSheetCount: Integer;
    FFontFaces: TSVGFontFaceRuleArray;
    FFontFaceCount: Integer;
    FServers: TStringList;
    FContext: TSVGLengthContext;
    FDocuments: ISVGDocumentResolver;
    FStyleSheets: ISVGStyleSheetResolver;
    FSheetDocs: TFPList;
    FSheetCSS: TFPList;
    function BuildGradient(aElement: TSVGElement;
      out aGradient: TSVGGradient; aDepth: Integer): Boolean;
    procedure ReadGradientStops(aElement: TSVGElement;
      var aGradient: TSVGGradient; aDepth: Integer);
    procedure SetLinkHistory(aValue: ISVGLinkHistory);
    procedure RegisterProperties;
    procedure RegisterAttributesOf(aElement: TSVGElement);
    procedure RegisterValueFunctions;
    procedure ApplyDroppedDeclarations(var aStyle: TSVGComputedStyle;
      aElement: TSVGElement; aRules: TCSSSharedRuleList;
      const aParent: TSVGComputedStyle);
    procedure ApplyDroppedRuleDeclarations(var aStyle: TSVGComputedStyle;
      aRules: TCSSSharedRuleList; const aParent: TSVGComputedStyle);
    procedure ApplyNamedDeclaration(var aStyle: TSVGComputedStyle;
      const aName, aValue: TSVGString; const aParent: TSVGComputedStyle);
    procedure RegisterElementTypes;
    procedure CollectStyleSheets(aElement: TSVGElement; aCSS: TCSSResolver;
      aDocument: TSVGDocument; var aCount: Integer);
    function LoadSheetsOf(aDocument: TSVGDocument;
      aCSS: TCSSResolver): Integer;
    function ExpandImports(const aText: RawByteString;
      const aPrefix: TSVGString; aDocument: TSVGDocument;
      aDepth: Integer): RawByteString;
    function CSSFor(aDocument: TSVGDocument): TCSSResolver;
    procedure CollectFontFaces(const aSource: RawByteString);
    function GetFontFace(aIndex: Integer): TSVGFontFaceRule;
    function PaintTargetOf(const aReference: TSVGString): TSVGElement;
    function MergedPattern(aElement: TSVGElement): TSVGElement;
    function CascadedValue(aValues: TCSSAttributeValues; aProperty: TSVGProperty;
      out aValue: TSVGString): Boolean;
    procedure ApplyDeclaration(var aStyle: TSVGComputedStyle;
      aProperty: TSVGProperty; const aValue: TSVGString;
      const aParent: TSVGComputedStyle);
    procedure ApplyPaint(var aPaint: TSVGPaint; const aValue: TSVGString;
      const aCurrentColor: TSVGColor);
  public
    constructor Create;
    destructor Destroy; override;
    // Loads the stylesheets of a document and prepares the resolver.
    procedure LoadDocument(aDocument: TSVGDocument);
    // Number of font-face rules found in the document's stylesheets.
    property FontFaceCount: Integer read FFontFaceCount;
    // Font-face rule by index, in the order the stylesheets declare them.
    property FontFaces[aIndex: Integer]: TSVGFontFaceRule read GetFontFace;
    // Forgets the document. The resolver is then ready for another one.
    procedure Unload;
    // The computed style of an element, given its parent's computed style.
    function ComputeStyle(aElement: TSVGElement;
      const aParent: TSVGComputedStyle): TSVGComputedStyle;
    // The computed style of an element. Walks up to the root to compute
    // the parent styles it needs.
    function ComputeStyleOf(aElement: TSVGElement): TSVGComputedStyle;
    // The paint server that an element defines, built once and then
    // cached. Returns nil when the element defines none.
    function PaintServerOf(aElement: TSVGElement): ISVGPaintServer;
    // The element that a paint server was built from, or nil.
    function ServerElementOf(aServer: ISVGPaintServer): TSVGElement;
    // Number of stylesheets collected from the document.
    property StyleSheetCount: Integer read FSheetCount;
    // The context that userSpaceOnUse lengths resolve against.
    property LengthContext: TSVGLengthContext read FContext write FContext;
    // Which of the links have been followed before. Without it no link
    // has been followed: :link matches every link and :visited matches
    // none.
    property LinkHistory: ISVGLinkHistory read FLinkHistory
      write SetLinkHistory;
    // Supplies the documents that paint references in other files point
    // to. Without it, such a reference resolves to nothing.
    property Documents: ISVGDocumentResolver read FDocuments
      write FDocuments;
    // Supplies the text of the stylesheets of xml-stylesheet
    // instructions. Without it, such a sheet is not loaded.
    property StyleSheets: ISVGStyleSheetResolver read FStyleSheets
      write FStyleSheets;
  end;

  { Base class for the paint servers of a document. }
  TSVGPaintServerObject = class(TObject, ISVGPaintServer)
  private
    FID: TSVGString;
    FElement: TSVGElement;
    FOwnsElement: Boolean;
  public
    constructor Create(const aID: TSVGString);
    destructor Destroy; override;
    function GetPaintServerKind: TSVGPaintServerKind; virtual; abstract;
    function GetPaintServerID: TSVGString;
    // Fills aGradient for a gradient server. False for anything else.
    function GetGradient(out aGradient: TSVGGradient): Boolean; virtual;
    // The element that the server was built from. The server owns it when
    // it was copied out of another document.
    property Element: TSVGElement read FElement;
  end;

  { A gradient element, resolved into geometry a backend can paint. }
  TSVGGradientServer = class(TSVGPaintServerObject)
  private
    FGradient: TSVGGradient;
  public
    constructor Create(const aID: TSVGString; const aGradient: TSVGGradient);
    function GetPaintServerKind: TSVGPaintServerKind; override;
    function GetGradient(out aGradient: TSVGGradient): Boolean; override;
    // The resolved gradient.
    property Gradient: TSVGGradient read FGradient;
  end;

  { A pattern element that a paint refers to. The tile is child content
    and is drawn by the renderer; the paint only holds the identity of the
    server. }
  TSVGPatternServer = class(TSVGPaintServerObject)
  public
    function GetPaintServerKind: TSVGPaintServerKind; override;
  end;

  TSVGUseResult = (urOK, urNoReference, urCycle, urTooDeep, urNotAUse);

  { Expands use elements into a copy of the content they refer to. }
  TSVGUseExpander = class(TObject)
  private
    FDocument: TSVGDocument;
    FActive: TStringList;
    FMaxDepth: Integer;
    function ExpandInto(aUse: TSVGElement; aDepth: Integer;
      out aResult: TSVGElement): TSVGUseResult;
    function ExpandNested(aElement: TSVGElement;
      aDepth: Integer): TSVGUseResult;
  public
    constructor Create(aDocument: TSVGDocument);
    destructor Destroy; override;
    // Builds the subtree of a use element, expanding nested uses in place.
    // The caller owns aResult. It is nil unless the result is urOK.
    function Expand(aUse: TSVGElement; out aResult: TSVGElement): TSVGUseResult;
    // Expands every use element in a subtree, in place, against the
    // document this expander was built for.
    function ExpandSubtree(aRoot: TSVGElement): TSVGUseResult;
    // How deep a chain of use elements may go before it is refused.
    property MaxDepth: Integer read FMaxDepth write FMaxDepth;
  end;

// The value an element sets for a property itself: its inline style
// first, its presentation attribute second. Empty when the element sets
// none, an inherited value included.
// Points every element of a copy at the element it was copied from.
// Selectors then match against the original position.
procedure LinkToOrigin(aCopy, aOrigin: TSVGElement);
function SVGDeclaredValue(aElement: TSVGElement; const aName: TSVGString): TSVGString;
// The lines a text-decoration value requests.
function SVGDecorationsOf(const aText: TSVGString): TSVGTextDecorations;
// True when the element is drawn in place, and not only when something
// refers to it.
function SVGIsRenderedElement(aElement: TSVGElement): Boolean;
// True when the property inherits from the parent element.
function SVGPropertyInherits(aProperty: TSVGProperty): Boolean;
// The CSS name of a property, as written in a stylesheet or an attribute.
function SVGPropertyName(aProperty: TSVGProperty): TSVGString;
// The property with the given CSS name. False when there is no such
// property.
function TryStrToSVGProperty(const aName: TSVGString;
  out aProperty: TSVGProperty): Boolean;

implementation

const
  PropertyNames: array[TSVGProperty] of TSVGString = (
    'fill', 'fill-opacity', 'fill-rule',
    'stroke', 'stroke-opacity', 'stroke-width', 'stroke-linecap',
    'stroke-linejoin', 'stroke-miterlimit', 'stroke-dasharray',
    'stroke-dashoffset',
    'opacity', 'color', 'display', 'visibility',
    'font-family', 'font-size', 'clip-rule',
    'overflow', 'clip-path', 'mask', 'filter',
    'text-anchor', 'font-weight', 'font-style', 'letter-spacing',
    'word-spacing', 'color-interpolation',
    'color-interpolation-filters',
    'marker-start', 'marker-mid', 'marker-end',
    'dominant-baseline', 'alignment-baseline', 'text-decoration',
    'baseline-shift', 'font-stretch', 'font-variant',
    'stop-color', 'stop-opacity', 'clip',
    'writing-mode', 'glyph-orientation-vertical');

  InheritedProperties: TSVGProperties = [
    prFill, prFillOpacity, prFillRule,
    prStroke, prStrokeOpacity, prStrokeWidth, prStrokeLinecap,
    prStrokeLinejoin, prStrokeMiterlimit, prStrokeDasharray, prStrokeDashoffset,
    prColor, prVisibility, prFontFamily, prFontSize, prClipRule,
    prTextAnchor, prFontWeight, prFontStyle, prLetterSpacing, prWordSpacing,
    prColorInterpolation, prFilterInterpolation,
    prMarkerStart, prMarkerMid, prMarkerEnd,
    prTextDecoration, prFontStretch, prFontVariant,
    prWritingMode, prGlyphOrientationVertical];

  DisplayNames: array[TSVGDisplay] of TSVGString = ('inline', 'none');
  VisibilityNames: array[TSVGVisibility] of TSVGString =
    ('visible', 'hidden', 'collapse');
  FillRuleNames: array[TSVGFillRule] of TSVGString = ('nonzero', 'evenodd');
  OverflowNames: array[TSVGOverflow] of TSVGString = ('visible', 'hidden');
  WritingModeNames: array[TSVGWritingMode] of TSVGString = ('lr-tb', 'tb');
  OrientationNames: array[TSVGGlyphOrientation] of TSVGString =
    ('auto', '0', '90', '180', '270');
  AnchorNames: array[TSVGTextAnchor] of TSVGString = ('start', 'middle', 'end');
  MixingNames: array[TSVGColorInterpolation] of TSVGString =
    ('sRGB', 'linearRGB');
  VariantNames: array[TSVGFontVariant] of TSVGString = ('normal', 'small-caps');
  FontStyleNames: array[TSVGFontStyle] of TSVGString =
    ('normal', 'italic', 'oblique');

// The element a reference points to. It is looked up in the document that
// holds aFrom, which is another file when the gradient came from one.
function ReferenceFrom(aFrom: TSVGElement;
  const aReference: TSVGString): TSVGElement;

begin
  Result := nil;
  if (aFrom <> nil) and (aFrom.Document <> nil) then
    Result := aFrom.Document.ResolveReference(aReference);
end;


class function TSVGBaselineShift.None: TSVGBaselineShift;

begin
  Result.Kind := skBaseline;
  Result.Amount := TSVGLength.Zero;
end;


function TSVGBaselineShift.Resolve(aFontSize, aFaceHeight, aXHeight,
  aDPI: Double): Double;

begin
  case Kind of
    // No face here states where it puts a subscript or a superscript, so
    // both move by half the height of the face.
    skSub: Result := -aFaceHeight / 2;
    skSuper: Result := aFaceHeight / 2;
    // A percentage counts against the font size, not against the viewport.
    skLength: Result := Amount.Resolve(aFontSize, aFontSize, aXHeight, aDPI);
  else
    Result := 0;
  end;
end;


class function TSVGClipEdge.Auto: TSVGClipEdge;

begin
  Result.IsAuto := True;
  Result.Offset := TSVGLength.Zero;
end;


class function TSVGClipShape.Auto: TSVGClipShape;

begin
  Result.Shaped := False;
  Result.Top := TSVGClipEdge.Auto;
  Result.Right := TSVGClipEdge.Auto;
  Result.Bottom := TSVGClipEdge.Auto;
  Result.Left := TSVGClipEdge.Auto;
end;


function TSVGClipShape.Narrow(const aViewport: TSVGRect;
  const aContext: TSVGLengthContext): TSVGRect;

begin
  Result := aViewport;
  if not Shaped then
    Exit;
  if not Top.IsAuto then
    Result.Top := aViewport.Top + aContext.Resolve(Top.Offset, laVertical);
  if not Right.IsAuto then
    Result.Right := aViewport.Right
                  - aContext.Resolve(Right.Offset, laHorizontal);
  if not Bottom.IsAuto then
    Result.Bottom := aViewport.Bottom
                   - aContext.Resolve(Bottom.Offset, laVertical);
  if not Left.IsAuto then
    Result.Left := aViewport.Left + aContext.Resolve(Left.Offset, laHorizontal);
end;


function TSVGClipShape.ToString: TSVGString;

  function EdgeText(const aEdge: TSVGClipEdge): TSVGString;

  begin
    if aEdge.IsAuto then
      Result := 'auto'
    else
      Result := aEdge.Offset.ToString;
  end;

begin
  if not Shaped then
    Exit('auto');
  Result := 'rect(' + EdgeText(Top) + ',' + EdgeText(Right) + ','
          + EdgeText(Bottom) + ',' + EdgeText(Left) + ')';
end;


// Reads one side of a clip rectangle: the keyword auto, or a length.
function TryStrToSVGClipEdge(const aText: TSVGString;
  var aEdge: TSVGClipEdge): Boolean;

begin
  if SameText(Trim(aText), 'auto') then
    begin
    aEdge := TSVGClipEdge.Auto;
    Exit(True);
    end;
  aEdge.IsAuto := False;
  Result := aEdge.Offset.TryParse(aText);
end;


// Reads a clip value: the keyword auto, or rect() with four sides, given
// top first and separated by commas or whitespace. SVG 1.1 takes the
// sides as offsets from the side of the viewport each one faces.
function TryStrToSVGClipShape(const aText: TSVGString;
  var aShape: TSVGClipShape): Boolean;

var
  lText: TSVGString;
  lParts: array[0..3] of TSVGString;
  lEdges: array[0..3] of TSVGClipEdge;
  lCount, lStart, I: Integer;

begin
  lText := Trim(aText);
  if SameText(lText, 'auto') then
    begin
    aShape := TSVGClipShape.Auto;
    Exit(True);
    end;
  if not SameText(Copy(lText, 1, 5), 'rect(') then
    Exit(False);
  if lText[Length(lText)] <> ')' then
    Exit(False);
  lText := Copy(lText, 6, Length(lText) - 6);
  lCount := 0;
  lStart := 0;
  for I := 1 to Length(lText) + 1 do
    if (I <= Length(lText))
    and not (lText[I] in [',', ' ', #9, #10, #13]) then
      begin
      if lStart = 0 then
        lStart := I;
      end
    else if lStart <> 0 then
      begin
      if lCount = Length(lParts) then
        Exit(False);
      lParts[lCount] := Copy(lText, lStart, I - lStart);
      Inc(lCount);
      lStart := 0;
      end;
  if lCount <> Length(lParts) then
    Exit(False);
  for I := 0 to High(lParts) do
    if not TryStrToSVGClipEdge(lParts[I], lEdges[I]) then
      Exit(False);
  aShape.Shaped := True;
  aShape.Top := lEdges[0];
  aShape.Right := lEdges[1];
  aShape.Bottom := lEdges[2];
  aShape.Left := lEdges[3];
  Result := True;
end;


// Reads a baseline-shift value: the keywords baseline, sub and super, a
// length, or a percentage of the font size.
function TryStrToSVGBaselineShift(const aText: TSVGString;
  var aShift: TSVGBaselineShift): Boolean;

var
  lText: TSVGString;
  lLength: TSVGLength;

begin
  Result := True;
  lText := LowerCase(Trim(aText));
  if lText = 'baseline' then
    aShift := TSVGBaselineShift.None
  else if lText = 'sub' then
    aShift.Kind := skSub
  else if lText = 'super' then
    aShift.Kind := skSuper
  else if lLength.TryParse(lText) then
    begin
    aShift.Kind := skLength;
    aShift.Amount := lLength;
    end
  else
    Result := False;
end;


// Reads a dominant-baseline or alignment-baseline keyword. Leaves the
// target unchanged when the keyword is not one this draws differently.
function TryStrToSVGBaseline(const aText: TSVGString;
  var aBaseline: TSVGBaseline): Boolean;

var
  lText: TSVGString;

begin
  lText := LowerCase(Trim(aText));
  Result := True;
  if (lText = 'middle') then
    aBaseline := dbMiddle
  else if lText = 'central' then
    aBaseline := dbCentral
  else if lText = 'hanging' then
    aBaseline := dbHanging
  else if lText = 'mathematical' then
    aBaseline := dbMathematical
  else if (lText = 'text-before-edge') or (lText = 'before-edge') then
    aBaseline := dbBeforeEdge
  else if (lText = 'text-after-edge') or (lText = 'after-edge') then
    aBaseline := dbAfterEdge
  else if lText = 'ideographic' then
    aBaseline := dbIdeographic
  else if (lText = 'auto') or (lText = 'alphabetic') or (lText = 'baseline')
          or (lText = 'no-change') or (lText = 'reset-size')
          or (lText = 'use-script') then
    aBaseline := dbAuto
  else
    Result := False;
end;


// The lines a text-decoration value requests. A line it does not list is
// left out, so none gives an empty set.
function SVGDecorationsOf(const aText: TSVGString): TSVGTextDecorations;

var
  lText: TSVGString;

begin
  Result := [];
  lText := LowerCase(aText);
  if Pos('underline', lText) > 0 then
    Include(Result, tdUnderline);
  if Pos('overline', lText) > 0 then
    Include(Result, tdOverline);
  if Pos('line-through', lText) > 0 then
    Include(Result, tdLineThrough);
end;


// Reads a text-anchor keyword. Leaves the target unchanged when unknown.
// Reads a color-interpolation value. The value auto may be either space
// and is taken as sRGB, the value SVG uses when a document sets none.
function TryStrToSVGColorInterpolation(const aText: TSVGString;
  var aMixing: TSVGColorInterpolation): Boolean;

var
  lText: TSVGString;

begin
  lText := Trim(aText);
  Result := True;
  if SameText(lText, 'linearRGB') then
    aMixing := ciLinearRGB
  else if SameText(lText, 'sRGB') or SameText(lText, 'auto') then
    aMixing := ciSRGB
  else
    Result := False;
end;


function TryStrToSVGTextAnchor(const aText: TSVGString;
  var aAnchor: TSVGTextAnchor): Boolean;

begin
  Result := True;
  if SameText(aText, 'start') then
    aAnchor := taStart
  else if SameText(aText, 'middle') then
    aAnchor := taMiddle
  else if SameText(aText, 'end') then
    aAnchor := taEnd
  else
    Result := False;
end;


// Reads a writing-mode keyword. The two values that lay text down the
// page give wmTB. The two right to left values are read as left to
// right.
function TryStrToSVGWritingMode(const aText: TSVGString;
  var aMode: TSVGWritingMode): Boolean;

begin
  Result := True;
  if SameText(aText, 'tb') or SameText(aText, 'tb-rl') then
    aMode := wmTB
  else if SameText(aText, 'lr-tb') or SameText(aText, 'lr')
          or SameText(aText, 'rl-tb') or SameText(aText, 'rl') then
    aMode := wmLRTB
  else
    Result := False;
end;


// Reads a glyph-orientation-vertical value: the keyword auto, or an
// angle in degrees which SVG allows only at the quarter turns.
function TryStrToSVGGlyphOrientation(const aText: TSVGString;
  var aOrientation: TSVGGlyphOrientation): Boolean;

var
  lText: TSVGString;
  lAngle: Double;

begin
  Result := True;
  lText := Trim(aText);
  if SameText(lText, 'auto') then
    begin
    aOrientation := goAuto;
    Exit(True);
    end;
  if EndsText('deg', lText) then
    lText := Copy(lText, 1, Length(lText) - 3);
  if not TryStrToSVGNumber(lText, lAngle) then
    Exit(False);
  lAngle := lAngle - Int(lAngle / 360) * 360;
  if lAngle < 0 then
    lAngle := lAngle + 360;
  if SameValue(lAngle, 0) then
    aOrientation := go0
  else if SameValue(lAngle, 90) then
    aOrientation := go90
  else if SameValue(lAngle, 180) then
    aOrientation := go180
  else if SameValue(lAngle, 270) then
    aOrientation := go270
  else
    Result := False;
end;


// Reads a font-style keyword. Leaves the target unchanged when unknown.
function TryStrToSVGFontStyle(const aText: TSVGString;
  var aStyle: TSVGFontStyle): Boolean;

begin
  Result := True;
  if SameText(aText, 'normal') then
    aStyle := fnNormal
  else if SameText(aText, 'italic') then
    aStyle := fnItalic
  else if SameText(aText, 'oblique') then
    aStyle := fnOblique
  else
    Result := False;
end;


// Reads a font weight. bolder and lighter move 300 from the parent value.
procedure ApplyFontWeight(var aTarget: Integer; const aValue: TSVGString;
  aParent: Integer);

var
  lNumber, lCode: Integer;

begin
  if SameText(aValue, 'normal') then
    aTarget := SVGNormalFontWeight
  else if SameText(aValue, 'bold') then
    aTarget := SVGBoldFontWeight
  else if SameText(aValue, 'bolder') then
    aTarget := Min(900, aParent + 300)
  else if SameText(aValue, 'lighter') then
    aTarget := Max(100, aParent - 300)
  else
    begin
    Val(Trim(aValue), lNumber, lCode);
    if (lCode = 0) and (lNumber >= 1) and (lNumber <= 1000) then
      aTarget := lNumber;
    end;
end;


// Reads a font-stretch keyword. wider and narrower move one step from the
// width of the parent element, and stop at the ends of the scale.
procedure ApplyFontStretch(var aTarget: TSVGFontStretch;
  const aValue: TSVGString; aParent: TSVGFontStretch);

var
  lText: TSVGString;

begin
  lText := LowerCase(Trim(aValue));
  if lText = 'wider' then
    begin
    aTarget := aParent;
    if aTarget < High(TSVGFontStretch) then
      Inc(aTarget);
    end
  else if lText = 'narrower' then
    begin
    aTarget := aParent;
    if aTarget > Low(TSVGFontStretch) then
      Dec(aTarget);
    end
  else if (lText = 'normal') or (SVGFontStretchName(SVGFontStretchOf(lText))
          = lText) then
    aTarget := SVGFontStretchOf(lText);
end;


// Reads a spacing length. normal means no extra spacing.
procedure ApplySpacing(var aTarget: Double; const aValue: TSVGString);

var
  lLength: TSVGLength;

begin
  if SameText(Trim(aValue), 'normal') then
    aTarget := 0
  else if lLength.TryParse(aValue) then
    aTarget := lLength.Value;
end;


// Reads an overflow keyword. SVG 1.1 makes auto visible and scroll
// hidden.
function TryStrToSVGOverflow(const aText: TSVGString;
  var aOverflow: TSVGOverflow): Boolean;

begin
  Result := True;
  if SameText(aText, 'visible') or SameText(aText, 'auto') then
    aOverflow := ovVisible
  else if SameText(aText, 'hidden') or SameText(aText, 'scroll') then
    aOverflow := ovHidden
  else
    Result := False;
end;


// The value that an inline style gives a property, empty when it sets
// none. A semicolon inside brackets does not end a declaration.
function SVGInlineDeclaration(aElement: TSVGElement;
  const aName: TSVGString): TSVGString;

var
  lText, lItem: TSVGString;
  lStart, lDepth, I, lColon: Integer;

  function ItemValue: TSVGString;
  begin
    Result := '';
    lColon := Pos(':', lItem);
    if (lColon > 0) and SameText(Trim(Copy(lItem, 1, lColon - 1)), aName) then
      Result := Trim(Copy(lItem, lColon + 1, Length(lItem)));
  end;

begin
  Result := '';
  if not aElement.HasAttribute('style') then
    Exit;
  lText := aElement.Attributes['style'];
  lStart := 1;
  lDepth := 0;
  for I := 1 to Length(lText) do
    case lText[I] of
      '(': Inc(lDepth);
      ')': if lDepth > 0 then Dec(lDepth);
      ';':
        if lDepth = 0 then
          begin
          lItem := Copy(lText, lStart, I - lStart);
          Result := ItemValue;
          if Result <> '' then
            Exit;
          lStart := I + 1;
          end;
    end;
  lItem := Copy(lText, lStart, Length(lText));
  Result := ItemValue;
end;


// The value an element gives a property. The inline style comes first,
// then the presentation attribute.
function SVGDeclaredValue(aElement: TSVGElement; const aName: TSVGString): TSVGString;

begin
  Result := SVGInlineDeclaration(aElement, aName);
  if Result = '' then
    Result := aElement.Attributes[aName];
end;


// The reference in a clip-path or mask value, empty for none.
function SVGReferenceOf(const aText: TSVGString): TSVGString;

begin
  if SameText(Trim(aText), 'none') then
    Result := ''
  else
    Result := SVGReferenceText(aText);
end;


// Reads a fill or clip rule keyword. Leaves the target unchanged when
// unknown.
function TryStrToSVGFillRule(const aText: TSVGString;
  var aRule: TSVGFillRule): Boolean;

begin
  Result := True;
  if SameText(aText, 'nonzero') then
    aRule := frNonZero
  else if SameText(aText, 'evenodd') then
    aRule := frEvenOdd
  else
    Result := False;
end;


// Reads a line cap keyword. Leaves the target unchanged when unknown.
function TryStrToSVGLineCap(const aText: TSVGString; var aCap: TSVGLineCap): Boolean;

begin
  Result := True;
  if SameText(aText, 'butt') then
    aCap := lcButt
  else if SameText(aText, 'round') then
    aCap := lcRound
  else if SameText(aText, 'square') then
    aCap := lcSquare
  else
    Result := False;
end;


// Reads a line join keyword. Leaves the target unchanged when unknown.
function TryStrToSVGLineJoin(const aText: TSVGString;
  var aJoin: TSVGLineJoin): Boolean;

begin
  Result := True;
  if SameText(aText, 'miter') then
    aJoin := ljMiter
  else if SameText(aText, 'round') then
    aJoin := ljRound
  else if SameText(aText, 'bevel') then
    aJoin := ljBevel
  else
    Result := False;
end;


// Reads a visibility keyword. Leaves the target unchanged when unknown.
function TryStrToSVGVisibility(const aText: TSVGString;
  var aVisibility: TSVGVisibility): Boolean;

begin
  Result := True;
  if SameText(aText, 'visible') then
    aVisibility := svVisible
  else if SameText(aText, 'hidden') then
    aVisibility := svHidden
  else if SameText(aText, 'collapse') then
    aVisibility := svCollapse
  else
    Result := False;
end;


// Reads a dash pattern: a list of numbers separated by whitespace or
// commas.
function TryStrToSVGDashArray(const aText: TSVGString;
  out aValues: TSVGDoubleArray): Boolean;

const
  Breaks = [' ', #9, #10, #13, ','];

var
  lCount, I, lStart: Integer;
  lLength: TSVGLength;

begin
  SetLength(aValues, 4);
  lCount := 0;
  I := 1;
  while I <= Length(aText) do
    begin
    while (I <= Length(aText)) and (aText[I] in Breaks) do
      Inc(I);
    if I > Length(aText) then
      Break;
    lStart := I;
    while (I <= Length(aText)) and not (aText[I] in Breaks) do
      Inc(I);
    // A dash is a length like any other and may have a unit.
    if not lLength.TryParse(Copy(aText, lStart, I - lStart)) then
      begin
      aValues := nil;
      Exit(False);
      end;
    if lCount = Length(aValues) then
      SetLength(aValues, lCount * 2);
    aValues[lCount] := lLength.Value;
    Inc(lCount);
    end;
  SetLength(aValues, lCount);
  Result := lCount > 0;
end;


function SVGPropertyInherits(aProperty: TSVGProperty): Boolean;

begin
  Result := aProperty in InheritedProperties;
end;


function SVGPropertyName(aProperty: TSVGProperty): TSVGString;

begin
  Result := PropertyNames[aProperty];
end;


function TryStrToSVGProperty(const aName: TSVGString;
  out aProperty: TSVGProperty): Boolean;

var
  P: TSVGProperty;
  lName: TSVGString;

begin
  aProperty := prFill;
  lName := LowerCase(Trim(aName));
  for P := Low(TSVGProperty) to High(TSVGProperty) do
    if PropertyNames[P] = lName then
      begin
      aProperty := P;
      Exit(True);
      end;
  Result := False;
end;


{ TSVGPaintStyleHelper }

function TSVGPaintStyleHelper.TryParse(const aText: TSVGString;
  const aCurrentColor: TSVGColor; aDocument: TSVGDocument): Boolean;

var
  lText, lTail, lReference: TSVGString;
  lColor: TSVGColor;
  lElement: TSVGElement;

begin
  lText := Trim(aText);
  if SameText(lText, 'none') then
    begin
    Self := TSVGPaint.None;
    Exit(True);
    end;
  if SameText(lText, 'currentColor') then
    begin
    Self := TSVGPaint.CreateColor(aCurrentColor);
    Exit(True);
    end;
  if (Length(lText) > 4) and SameText(Copy(lText, 1, 4), 'url(') then
    begin
    if aDocument = nil then
      Exit(False);
    lTail := Trim(Copy(lText, Pos(')', lText) + 1, Length(lText)));
    lReference := Copy(lText, 1, Pos(')', lText));
    if SVGReferencePath(lReference) <> '' then
      lElement := aDocument.Root
    else
      lElement := aDocument.ResolveReference(lReference);
    Self := TSVGPaint.CreateServer(nil);
    if SameText(lTail, 'none') then
      Self.Fallback := pfNone
    else if SameText(lTail, 'currentColor') then
      begin
      Self.Fallback := pfColor;
      Self.Color := aCurrentColor;
      end
    else if (lTail <> '') and lColor.TryParse(lTail) then
      begin
      Self.Fallback := pfColor;
      Self.Color := lColor;
      end;
    // With no server, the fallback is the whole value. With neither, the
    // value is empty and the old paint is kept.
    if lElement = nil then
      begin
      Self := Self.Resolved;
      Result := Self.Kind <> spServer;
      end
    else
      Result := True;
    Exit;
    end;
  Result := lColor.TryParse(lText);
  if Result then
    Self := TSVGPaint.CreateColor(lColor);
end;


function SVGIsRenderedElement(aElement: TSVGElement): Boolean;

begin
  Result := (aElement <> nil)
        and not (aElement is TSVGDefsElement)
        and not (aElement is TSVGSymbolElement)
        and not (aElement is TSVGTitleElement)
        and not (aElement is TSVGDescElement)
        and not (aElement is TSVGMetadataElement)
        and not (aElement is TSVGStyleElement)
        and not (aElement is TSVGMarkerElement)
        and not (aElement is TSVGClipPathElement)
        and not (aElement is TSVGMaskElement)
        and not (aElement is TSVGPatternElement)
        and not (aElement is TSVGLinearGradientElement)
        and not (aElement is TSVGRadialGradientElement)
        and not (aElement is TSVGForeignElement);
end;


// Copies the properties that a use element passes to its target. The
// geometry attributes are left out: the expansion has used them already.
procedure CopyUseAttributes(aUse, aGroup: TSVGElement);

var
  I: Integer;
  lName: TSVGString;

begin
  for I := 0 to aUse.AttributeCount - 1 do
    begin
    lName := aUse.AttributeNames[I];
    if (lName = 'x') or (lName = 'y') or (lName = 'href')
       or (lName = 'width') or (lName = 'height') or (lName = 'id') then
      Continue;
    aGroup.Attributes[lName] := aUse.Attributes[lName];
    end;
end;


{ TSVGGradientServer }

constructor TSVGPaintServerObject.Create(const aID: TSVGString);

begin
  inherited Create;
  FID := aID;
end;


destructor TSVGPaintServerObject.Destroy;

begin
  if FOwnsElement then
    FreeAndNil(FElement);
  inherited Destroy;
end;


function TSVGPaintServerObject.GetPaintServerID: TSVGString;

begin
  Result := FID;
end;


function TSVGPaintServerObject.GetGradient(out aGradient: TSVGGradient): Boolean;

begin
  Result := False;
end;


function TSVGPatternServer.GetPaintServerKind: TSVGPaintServerKind;

begin
  Result := pkPattern;
end;


constructor TSVGGradientServer.Create(const aID: TSVGString;
  const aGradient: TSVGGradient);

begin
  inherited Create(aID);
  FGradient := aGradient;
end;


function TSVGGradientServer.GetPaintServerKind: TSVGPaintServerKind;

begin
  Result := FGradient.Kind;
end;


function TSVGGradientServer.GetGradient(out aGradient: TSVGGradient): Boolean;

begin
  aGradient := FGradient;
  Result := True;
end;


// The gradient an attribute is read from: the given one when it has the
// attribute, otherwise the nearest gradient in its chain of references
// that has it. SVG 1.1 lets a gradient take every attribute it does not
// set itself from the gradient it refers to, not only its stops. Nil when
// no gradient in the chain has it.
function GradientAttributeSource(aElement: TSVGElement;
  const aName: TSVGString): TSVGElement;

var
  lDepth: Integer;
  lNext: TSVGElement;

begin
  Result := aElement;
  lDepth := 0;
  while (Result <> nil) and not Result.HasAttribute(aName) do
    begin
    Inc(lDepth);
    if lDepth > 8 then
      Exit(nil);
    lNext := ReferenceFrom(Result, SVGHRefOf(Result));
    if lNext = Result then
      Exit(nil);
    Result := lNext;
    end;
end;


// Reads a gradient coordinate. Under objectBoundingBox units it is a
// fraction of the box, and otherwise a length in the viewport.
function GradientCoordinate(aElement: TSVGElement; const aName: TSVGString;
  aUnits: TSVGGradientUnits; const aContext: TSVGLengthContext;
  aAxis: TSVGLengthAxis; aDefault: Double): Double;

var
  lLength: TSVGLength;

begin
  aElement := GradientAttributeSource(aElement, aName);
  if aElement = nil then
    Exit(aDefault);
  lLength := TSVGLength.Create(aDefault, luNumber);
  lLength.ParseDef(aElement.Attributes[aName]);
  if aUnits = guObjectBoundingBox then
    begin
    if lLength.LengthUnit = luPercent then
      Result := lLength.Value / 100
    else
      Result := lLength.Value;
    end
  else
    Result := aContext.Resolve(lLength, aAxis);
end;


// Reads the gradientUnits attribute. It defaults to the bounding box.
function GradientUnitsOf(aElement: TSVGElement): TSVGGradientUnits;

begin
  aElement := GradientAttributeSource(aElement, 'gradientUnits');
  if (aElement <> nil)
  and SameText(Trim(aElement.Attributes['gradientUnits']), 'userSpaceOnUse') then
    Result := guUserSpaceOnUse
  else
    Result := guObjectBoundingBox;
end;


// Reads the spreadMethod attribute. It defaults to pad.
function SpreadMethodOf(aElement: TSVGElement): TSVGSpreadMethod;

var
  lText: TSVGString;

begin
  aElement := GradientAttributeSource(aElement, 'spreadMethod');
  if aElement = nil then
    Exit(smPad);
  lText := Trim(aElement.Attributes['spreadMethod']);
  if SameText(lText, 'reflect') then
    Result := smReflect
  else if SameText(lText, 'repeat') then
    Result := smRepeat
  else
    Result := smPad;
end;


procedure TSVGStyleResolver.ReadGradientStops(aElement: TSVGElement;
  var aGradient: TSVGGradient; aDepth: Integer);

var
  I: Integer;
  lChild, lInherited: TSVGElement;
  lStop: TSVGGradientStop;
  lOffset: TSVGLength;
  lStopStyle: TSVGComputedStyle;
  lLast: Double;

begin
  lLast := 0;
  for I := 0 to aElement.ChildCount - 1 do
    begin
    if not (aElement[I] is TSVGStopElement) then
      Continue;
    lChild := TSVGElement(aElement[I]);
    lOffset := TSVGLength.Zero;
    lOffset.ParseDef(lChild.Attributes['offset']);
    if lOffset.LengthUnit = luPercent then
      lOffset := TSVGLength.Create(lOffset.Value / 100, luNumber);
    // The colour and opacity of a stop go through the cascade, so a stop
    // inherits from the position where its gradient is written.
    lStopStyle := ComputeStyleOf(lChild);
    // Offsets never decrease, so a stop below the last one is raised to it.
    lLast := Max(lLast, SVGClamp(lOffset.Value, 0, 1));
    lStop := TSVGGradientStop.Create(lLast, lStopStyle.StopColor,
      lStopStyle.StopOpacity);
    aGradient.AddStop(lStop);
    end;
  if aGradient.HasStops or (aDepth >= 8) then
    Exit;
  // A gradient without stops takes the stops of the gradient it
  // references.
  lInherited := ReferenceFrom(aElement, SVGHRefOf(aElement));
  if (lInherited <> nil) and (lInherited <> aElement) then
    ReadGradientStops(lInherited, aGradient, aDepth + 1);
end;


function TSVGStyleResolver.BuildGradient(aElement: TSVGElement;
  out aGradient: TSVGGradient; aDepth: Integer): Boolean;

var
  lUnits: TSVGGradientUnits;
  lCentre, lFocus: TSVGPoint;
  lRadius: Double;
  lTemplate: TSVGElement;

begin
  Result := False;
  if aElement = nil then
    Exit;
  lUnits := GradientUnitsOf(aElement);
  if aElement is TSVGLinearGradientElement then
    begin
    aGradient := TSVGGradient.CreateLinear(
      TSVGPoint.Create(
        GradientCoordinate(aElement, 'x1', lUnits, FContext, laHorizontal, 0),
        GradientCoordinate(aElement, 'y1', lUnits, FContext, laVertical, 0)),
      TSVGPoint.Create(
        GradientCoordinate(aElement, 'x2', lUnits, FContext, laHorizontal, 1),
        GradientCoordinate(aElement, 'y2', lUnits, FContext, laVertical, 0)));
    Result := True;
    end
  else if aElement is TSVGRadialGradientElement then
    begin
    lCentre := TSVGPoint.Create(
      GradientCoordinate(aElement, 'cx', lUnits, FContext, laHorizontal, 0.5),
      GradientCoordinate(aElement, 'cy', lUnits, FContext, laVertical, 0.5));
    lRadius := GradientCoordinate(aElement, 'r', lUnits, FContext,
      laDiagonal, 0.5);
    lFocus := TSVGPoint.Create(
      GradientCoordinate(aElement, 'fx', lUnits, FContext, laHorizontal,
        lCentre.X),
      GradientCoordinate(aElement, 'fy', lUnits, FContext, laVertical,
        lCentre.Y));
    aGradient := TSVGGradient.CreateRadial(lCentre, lRadius, lFocus);
    Result := True;
    end;
  if not Result then
    Exit;
  aGradient.Units := lUnits;
  aGradient.Spread := SpreadMethodOf(aElement);
  lTemplate := GradientAttributeSource(aElement, 'gradientTransform');
  if lTemplate <> nil then
    aGradient.Transform.ReadAttributeNamed(lTemplate, 'gradientTransform');
  aGradient.Mixing := ComputeStyleOf(aElement).ColorInterpolation;
  ReadGradientStops(aElement, aGradient, aDepth);
  aGradient.ClampFocus;
end;


// True when a node has an element among its children.
function HasElementChild(aElement: TSVGElement): Boolean;

var
  I: Integer;

begin
  Result := True;
  for I := 0 to aElement.ChildCount - 1 do
    if aElement[I] is TSVGElement then
      Exit;
  Result := False;
end;


// Copies into a pattern what the pattern it refers to has and it lacks:
// every missing attribute, and the tile when it has no children.
procedure TakeFromPattern(aTarget, aSource: TSVGElement);

var
  I: Integer;
  lName: TSVGString;

begin
  for I := 0 to aSource.AttributeCount - 1 do
    begin
    lName := aSource.AttributeNames[I];
    if (lName = 'id') or (lName = 'href') then
      Continue;
    if not aTarget.HasAttribute(lName) then
      aTarget.Attributes[lName] := aSource.Attributes[lName];
    end;
  if HasElementChild(aTarget) or not HasElementChild(aSource) then
    Exit;
  for I := 0 to aSource.ChildCount - 1 do
    aTarget.AppendChild(aSource[I].Clone);
end;


function TSVGStyleResolver.PaintTargetOf(
  const aReference: TSVGString): TSVGElement;

var
  lPath: TSVGString;
  lSource: TSVGDocument;

begin
  Result := nil;
  if FDocument = nil then
    Exit;
  lPath := SVGReferencePath(aReference);
  if lPath = '' then
    Exit(FDocument.ResolveReference(aReference));
  if FDocuments = nil then
    Exit;
  lSource := FDocuments.ResolveDocument(lPath, FDocument.BaseURI);
  if lSource <> nil then
    Result := lSource.ElementByID(SVGReferenceFragment(aReference));
end;


// A pattern built from the chain of patterns it refers to, or nil when
// there is no chain. The result is a new element that the caller owns.
function TSVGStyleResolver.MergedPattern(aElement: TSVGElement): TSVGElement;

var
  lTemplate: TSVGElement;
  lDepth: Integer;

begin
  Result := nil;
  lTemplate := ReferenceFrom(aElement, SVGHRefOf(aElement));
  if not (lTemplate is TSVGPatternElement) or (lTemplate = aElement) then
    Exit;
  Result := TSVGElement(aElement.Clone);
  lDepth := 0;
  while (lTemplate is TSVGPatternElement) and (lTemplate <> aElement)
        and (lDepth < 8) do
    begin
    TakeFromPattern(Result, lTemplate);
    lTemplate := ReferenceFrom(lTemplate, SVGHRefOf(lTemplate));
    Inc(lDepth);
    end;
end;


function TSVGStyleResolver.PaintServerOf(aElement: TSVGElement): ISVGPaintServer;

var
  lIndex: Integer;
  lGradient: TSVGGradient;
  lServer: TSVGPaintServerObject;
  lMerged: TSVGElement;
  lKey: TSVGString;
  lForeign: Boolean;

begin
  Result := nil;
  if (aElement = nil) or (FDocument = nil) or (aElement.ID = '') then
    Exit;
  lForeign := aElement.Document <> FDocument;
  lKey := aElement.ID;
  if lForeign and (aElement.Document <> nil) then
    lKey := aElement.Document.BaseURI + '#' + lKey;
  lIndex := FServers.IndexOf(lKey);
  if lIndex >= 0 then
    Exit(TSVGPaintServerObject(FServers.Objects[lIndex]));
  lMerged := nil;
  if aElement is TSVGPatternElement then
    begin
    lServer := TSVGPatternServer.Create(lKey);
    lMerged := MergedPattern(aElement);
    end
  else if BuildGradient(aElement, lGradient, 0) then
    lServer := TSVGGradientServer.Create(lKey, lGradient)
  else
    Exit;
  // A built gradient is plain data. A pattern is drawn from its children,
  // so a pattern merged from a chain, or taken from another file, is
  // copied into this document.
  if lMerged <> nil then
    begin
    lServer.FElement := lMerged;
    lServer.FElement.AdoptInto(FDocument);
    lServer.FOwnsElement := True;
    end
  else if lForeign and (lServer is TSVGPatternServer) then
    begin
    lServer.FElement := TSVGElement(aElement.Clone);
    lServer.FElement.AdoptInto(FDocument);
    lServer.FOwnsElement := True;
    end
  else
    lServer.FElement := aElement;
  FServers.AddObject(lKey, lServer);
  Result := lServer;
end;


function TSVGStyleResolver.ServerElementOf(
  aServer: ISVGPaintServer): TSVGElement;

var
  lIndex: Integer;

begin
  Result := nil;
  if aServer = nil then
    Exit;
  lIndex := FServers.IndexOf(aServer.GetPaintServerID);
  if lIndex >= 0 then
    Result := TSVGPaintServerObject(FServers.Objects[lIndex]).Element;
end;


{ TSVGUseExpander }

constructor TSVGUseExpander.Create(aDocument: TSVGDocument);

begin
  inherited Create;
  FDocument := aDocument;
  FActive := TStringList.Create;
  FActive.CaseSensitive := True;
  FMaxDepth := 16;
end;


destructor TSVGUseExpander.Destroy;

begin
  FreeAndNil(FActive);
  inherited Destroy;
end;


function TSVGUseExpander.ExpandNested(aElement: TSVGElement;
  aDepth: Integer): TSVGUseResult;

var
  I: Integer;
  lChild, lExpanded: TSVGElement;

begin
  Result := urOK;
  I := 0;
  while I < aElement.ChildCount do
    begin
    if aElement[I] is TSVGElement then
      begin
      lChild := TSVGElement(aElement[I]);
      if lChild is TSVGUseElement then
        begin
        Result := ExpandInto(lChild, aDepth + 1, lExpanded);
        if Result <> urOK then
          Exit;
        aElement.ReplaceChild(lChild, lExpanded);
        end
      else
        begin
        Result := ExpandNested(lChild, aDepth);
        if Result <> urOK then
          Exit;
        end;
      end;
    Inc(I);
    end;
end;


procedure LinkToOrigin(aCopy, aOrigin: TSVGElement);

var
  I: Integer;

begin
  aCopy.CSSOrigin := aOrigin;
  for I := 0 to aCopy.ChildCount - 1 do
    if (aCopy[I] is TSVGElement) and (I < aOrigin.ChildCount)
       and (aOrigin[I] is TSVGElement) then
      LinkToOrigin(TSVGElement(aCopy[I]), TSVGElement(aOrigin[I]));
end;


function TSVGUseExpander.ExpandInto(aUse: TSVGElement; aDepth: Integer;
  out aResult: TSVGElement): TSVGUseResult;

var
  lID: TSVGString;
  lTarget, lGroup, lCopy: TSVGElement;
  lX, lY, lTransform: TSVGString;

begin
  aResult := nil;
  if aDepth > FMaxDepth then
    Exit(urTooDeep);
  lID := SVGReferenceToID(SVGHRefOf(aUse));
  if (lID = '') or (FDocument = nil) then
    Exit(urNoReference);
  if FActive.IndexOf(lID) >= 0 then
    Exit(urCycle);
  lTarget := FDocument.ElementByID(lID);
  if lTarget = nil then
    Exit(urNoReference);
  FActive.Add(lID);
  try
    lGroup := CreateSVGElement('g');
    try
      CopyUseAttributes(aUse, lGroup);
      lX := aUse.AttributeDef('x', '0');
      lY := aUse.AttributeDef('y', '0');
      if (lX <> '0') or (lY <> '0') then
        begin
        lTransform := Trim(lGroup.AttributeDef('transform', ''));
        if lTransform <> '' then
          lTransform := lTransform + ' ';
        lGroup.Attributes['transform'] :=
          lTransform + Format('translate(%s,%s)', [lX, lY]);
        end;
      lCopy := TSVGElement(lTarget.Clone);
      LinkToOrigin(lCopy, lTarget);
      lGroup.AppendChild(lCopy);
      if lCopy is TSVGUseElement then
        begin
        Result := ExpandInto(lCopy, aDepth + 1, aResult);
        if Result <> urOK then
          Exit;
        lGroup.ReplaceChild(lCopy, aResult);
        end
      else
        begin
        Result := ExpandNested(lCopy, aDepth);
        if Result <> urOK then
          Exit;
        end;
      aResult := lGroup;
      lGroup := nil;
      Result := urOK;
    finally
      lGroup.Free;
    end;
  finally
    FActive.Delete(FActive.IndexOf(lID));
  end;
end;


function TSVGUseExpander.ExpandSubtree(aRoot: TSVGElement): TSVGUseResult;

begin
  FActive.Clear;
  if aRoot = nil then
    Exit(urNoReference);
  Result := ExpandNested(aRoot, 0);
end;


function TSVGUseExpander.Expand(aUse: TSVGElement;
  out aResult: TSVGElement): TSVGUseResult;

begin
  aResult := nil;
  if not (aUse is TSVGUseElement) then
    Exit(urNotAUse);
  FActive.Clear;
  Result := ExpandInto(aUse, 0, aResult);
  if Result <> urOK then
    FreeAndNil(aResult);
end;


{ TSVGStyleResolver }

constructor TSVGStyleResolver.Create;

begin
  inherited Create;
  FServers := TStringList.Create;
  FServers.Sorted := True;
  FServers.Duplicates := dupIgnore;
  FServers.CaseSensitive := True;
  FServers.OwnsObjects := True;
  FContext := TSVGLengthContext.Default;
  FSheetDocs := TFPList.Create;
  FSheetCSS := TFPList.Create;
  FRegistry := TCSSRegistry.Create;
  FRegistry.Init;
  FResolver := TCSSResolver.Create(nil);
  FResolver.CSSRegistry := FRegistry;
  RegisterElementTypes;
  RegisterProperties;
  RegisterValueFunctions;
  // A selector can only use a pseudo class the registry knows. Of the
  // dynamic ones, only :link and :visited can be answered by a drawing:
  // hover and keyboard focus have no meaning in a document.
  FRegistry.AddPseudoClass('link');
  FRegistry.AddPseudoClass('visited');
end;


destructor TSVGStyleResolver.Destroy;

begin
  Unload;
  FreeAndNil(FSheetDocs);
  FreeAndNil(FSheetCSS);
  FreeAndNil(FServers);
  FreeAndNil(FResolver);
  FreeAndNil(FRegistry);
  inherited Destroy;
end;


procedure TSVGStyleResolver.RegisterElementTypes;

var
  I: Integer;

begin
  for I := 0 to SVGElementTagCount - 1 do
    FRegistry.AddType(SVGElementTag(I));
end;


// The documents already loaded are updated as well, so a history set
// after loading still reaches them.
procedure TSVGStyleResolver.SetLinkHistory(aValue: ISVGLinkHistory);

var
  I: Integer;

begin
  FLinkHistory := aValue;
  if FDocument <> nil then
    FDocument.LinkHistory := aValue;
  for I := 0 to FSheetDocs.Count - 1 do
    TSVGDocument(FSheetDocs[I]).LinkHistory := aValue;
end;


procedure TSVGStyleResolver.RegisterProperties;

var
  P: TSVGProperty;
  lDesc: TCSSAttributeDesc;

begin
  for P := Low(TSVGProperty) to High(TSVGProperty) do
    begin
    lDesc := FRegistry.AddAttribute(PropertyNames[P], '',
      SVGPropertyInherits(P));
    lDesc.AllowUnknownIdentifiers := True;
    FAttributeIDs[P] := lDesc.Index;
    end;
end;


// Registers a name for every attribute the document uses. An attribute
// selector only matches an attribute the registry knows, and the
// properties registered above are the style ones alone: cx, or a name a
// document invented, is not among them.
procedure TSVGStyleResolver.RegisterAttributesOf(aElement: TSVGElement);

var
  I: Integer;
  lName: TSVGString;
  lDesc: TCSSAttributeDesc;

begin
  for I := 0 to aElement.AttributeCount - 1 do
    begin
    lName := aElement.AttributeNames[I];
    if (lName = '') or (FRegistry.IndexOfAttributeName(lName) > 0) then
      Continue;
    lDesc := FRegistry.AddAttribute(lName, '', False);
    lDesc.AllowUnknownIdentifiers := True;
    end;
  for I := 0 to aElement.ChildCount - 1 do
    if aElement[I] is TSVGElement then
      RegisterAttributesOf(TSVGElement(aElement[I]));
end;


procedure TSVGStyleResolver.RegisterValueFunctions;

const
  Functions: array[0..4] of TSVGString = ('url', 'rgb', 'rgba', 'hsl', 'hsla');

var
  I: Integer;

begin
  // The registry drops a declaration whose value calls a function it does
  // not know. A fill set through a style attribute would be lost.
  for I := Low(Functions) to High(Functions) do
    if FRegistry.IndexOfAttrFunction(Functions[I]) <= 0 then
      FRegistry.AddAttrFunction(Functions[I]);
end;


// The text of a declaration after its colon, which is the value.
function SVGDeclarationValue(aDeclaration: TCSSDeclarationElement): TSVGString;

var
  lColon: Integer;

begin
  Result := aDeclaration.AsString;
  lColon := Pos(':', Result);
  if lColon = 0 then
    Result := ''
  else
    Result := Trim(Copy(Result, lColon + 1, Length(Result)));
end;


// The name a declaration sets, lowercased.
function SVGDeclarationKey(aDeclaration: TCSSDeclarationElement): TSVGString;

begin
  Result := '';
  if aDeclaration.KeyCount > 0 then
    Result := LowerCase(Trim(aDeclaration.Keys[0].AsString));
end;


// Strips the quotes that a css string or url may be written with.
function SVGUnquoted(const aText: TSVGString): TSVGString;

begin
  Result := Trim(aText);
  if Length(Result) < 2 then
    Exit;
  if ((Result[1] = '"') and (Result[Length(Result)] = '"'))
     or ((Result[1] = '''') and (Result[Length(Result)] = '''')) then
    Result := Copy(Result, 2, Length(Result) - 2);
end;


// The urls written with url() in a src descriptor, in order.
function SVGURLTokens(const aValue: TSVGString): TStringArray;

var
  I, lStart, lCount: Integer;
  lQuote: AnsiChar;

begin
  SetLength(Result, 0);
  lCount := 0;
  I := 1;
  while I <= Length(aValue) do
    begin
    if (aValue[I] <> 'u') and (aValue[I] <> 'U') then
      begin
      Inc(I);
      Continue;
      end;
    if LowerCase(Copy(aValue, I, 4)) <> 'url(' then
      begin
      Inc(I);
      Continue;
      end;
    Inc(I, 4);
    while (I <= Length(aValue)) and (aValue[I] = ' ') do
      Inc(I);
    lStart := I;
    if (I <= Length(aValue)) and ((aValue[I] = '"') or (aValue[I] = '''')) then
      begin
      lQuote := aValue[I];
      Inc(I);
      lStart := I;
      while (I <= Length(aValue)) and (aValue[I] <> lQuote) do
        Inc(I);
      end
    else
      // An unquoted url ends at the closing bracket or at whitespace. The
      // css tree puts the format() that follows a src inside the same
      // brackets.
      while (I <= Length(aValue)) and (aValue[I] <> ')')
            and (aValue[I] <> ' ') do
        Inc(I);
    SetLength(Result, lCount + 1);
    Result[lCount] := Trim(Copy(aValue, lStart, I - lStart));
    Inc(lCount);
    end;
end;


// The urls that a src descriptor lists, in order. A url the stylesheet
// quoted arrives here as a bare string: the css tree removes the url()
// around it.
function SVGSourceURLs(const aValue: TSVGString): TStringArray;

var
  I, lStart, lCount: Integer;
  lQuote: AnsiChar;
  lPart: TSVGString;

begin
  Result := SVGURLTokens(aValue);
  if Length(Result) > 0 then
    Exit;
  lCount := 0;
  lQuote := #0;
  lStart := 1;
  for I := 1 to Length(aValue) + 1 do
    begin
    if I <= Length(aValue) then
      begin
      if lQuote <> #0 then
        begin
        if aValue[I] = lQuote then
          lQuote := #0;
        Continue;
        end;
      if (aValue[I] = '"') or (aValue[I] = '''') then
        begin
        lQuote := aValue[I];
        Continue;
        end;
      if aValue[I] <> ',' then
        Continue;
      end;
    lPart := Trim(Copy(aValue, lStart, I - lStart));
    lStart := I + 1;
    if Pos('format(', LowerCase(lPart)) > 0 then
      lPart := Trim(Copy(lPart, 1, Pos('format(', LowerCase(lPart)) - 1));
    lPart := SVGUnquoted(lPart);
    if lPart = '' then
      Continue;
    SetLength(Result, lCount + 1);
    Result[lCount] := lPart;
    Inc(lCount);
    end;
end;


// The weight of a font-weight descriptor, 400 when it states none.
function SVGDescriptorWeight(const aValue: TSVGString): Integer;

var
  lText: TSVGString;

begin
  lText := LowerCase(Trim(aValue));
  if lText = 'bold' then
    Result := 700
  else if lText = 'normal' then
    Result := 400
  else
    begin
    Result := StrToIntDef(lText, 0);
    if Result <= 0 then
      Result := 400;
    end;
end;


procedure TSVGStyleResolver.CollectFontFaces(const aSource: RawByteString);

var
  lStream: TMemoryStream;
  lParser: TCSSParser;
  lTree, lChild, lInner: TCSSElement;
  lRule: TCSSAtRuleElement;
  I, J: Integer;
  lKey: TSVGString;
  lFace: TSVGFontFaceRule;

begin
  // The parser reads the sheet as the bytes it was written with.
  lStream := TMemoryStream.Create;
  if aSource <> '' then
    lStream.Write(aSource[1], Length(aSource));
  lStream.Position := 0;
  lParser := nil;
  lTree := nil;
  try
    try
      lParser := TCSSParser.Create(lStream);
      lTree := lParser.Parse;
    except
      // A stylesheet the parser cannot read declares no usable fonts. The
      // cascade reports the error itself.
      Exit;
    end;
    if not (lTree is TCSSChildrenElement) then
      Exit;
    for I := 0 to TCSSChildrenElement(lTree).ChildCount - 1 do
      begin
      lChild := TCSSChildrenElement(lTree).Children[I];
      if not (lChild is TCSSAtRuleElement) then
        Continue;
      lRule := TCSSAtRuleElement(lChild);
      if not SameText(Trim(lRule.AtKeyWord), '@font-face') then
        Continue;
      lFace.Family := '';
      lFace.Weight := 400;
      lFace.Style := fnNormal;
      SetLength(lFace.Sources, 0);
      for J := 0 to lRule.ChildCount - 1 do
        begin
        lInner := lRule.Children[J];
        if not (lInner is TCSSDeclarationElement) then
          Continue;
        lKey := SVGDeclarationKey(TCSSDeclarationElement(lInner));
        if lKey = 'font-family' then
          lFace.Family := SVGUnquoted(
            SVGDeclarationValue(TCSSDeclarationElement(lInner)))
        else if lKey = 'font-weight' then
          lFace.Weight := SVGDescriptorWeight(
            SVGDeclarationValue(TCSSDeclarationElement(lInner)))
        else if lKey = 'font-style' then
          begin
          if not SameText(Trim(SVGDeclarationValue(
               TCSSDeclarationElement(lInner))), 'normal') then
            lFace.Style := fnItalic;
          end
        else if lKey = 'src' then
          lFace.Sources := SVGSourceURLs(
            SVGDeclarationValue(TCSSDeclarationElement(lInner)));
        end;
      if (lFace.Family = '') or (Length(lFace.Sources) = 0) then
        Continue;
      if FFontFaceCount = Length(FFontFaces) then
        SetLength(FFontFaces, Max(4, FFontFaceCount * 2));
      FFontFaces[FFontFaceCount] := lFace;
      Inc(FFontFaceCount);
      end;
  finally
    lTree.Free;
    lParser.Free;
    lStream.Free;
  end;
end;


function TSVGStyleResolver.GetFontFace(aIndex: Integer): TSVGFontFaceRule;

begin
  Result := FFontFaces[aIndex];
end;


// Puts every property name of a stylesheet in the case the cascade knows
// it by. CSS accepts a property name in any case, but the registry looks
// it up exactly. Only a name at the start of a declaration is changed: an
// id, a class, and a name inside a comment or a string keep their case.
// Pass aInline for the declarations of a style attribute, which start at
// the front of the text instead of after a brace.
function SVGCanonicalPropertyNames(const aText: RawByteString;
  aInline: Boolean): RawByteString;

const
  NameChars = ['a'..'z', 'A'..'Z', '0'..'9', '-', '_'];
  Blanks = [' ', #9, #10, #13];

var
  I, lStart, lStop: Integer;
  lQuote, lPrev: AnsiChar;
  lName: TSVGString;
  lProperty: TSVGProperty;

begin
  Result := aText;
  I := 1;
  lPrev := #0;
  while I <= Length(Result) do
    begin
    if (I < Length(Result)) and (Result[I] = '/') and (Result[I + 1] = '*') then
      begin
      Inc(I, 2);
      while (I < Length(Result))
            and not ((Result[I] = '*') and (Result[I + 1] = '/')) do
        Inc(I);
      Inc(I, 2);
      Continue;
      end;
    if Result[I] in Blanks then
      begin
      Inc(I);
      Continue;
      end;
    if Result[I] in ['''', '"'] then
      begin
      lQuote := Result[I];
      Inc(I);
      while (I <= Length(Result)) and (Result[I] <> lQuote) do
        Inc(I);
      Inc(I);
      lPrev := lQuote;
      Continue;
      end;
    if not (Result[I] in NameChars) then
      begin
      lPrev := Result[I];
      Inc(I);
      Continue;
      end;
    lStart := I;
    while (I <= Length(Result)) and (Result[I] in NameChars) do
      Inc(I);
    lName := Copy(Result, lStart, I - lStart);
    lStop := I;
    while (lStop <= Length(Result)) and (Result[lStop] in Blanks) do
      Inc(lStop);
    if (lStop <= Length(Result)) and (Result[lStop] = ':')
       and ((lPrev = '{') or (lPrev = ';') or (aInline and (lPrev = #0)))
       and TryStrToSVGProperty(lName, lProperty)
       and (lName <> PropertyNames[lProperty]) then
      begin
      Delete(Result, lStart, Length(lName));
      Insert(PropertyNames[lProperty], Result, lStart);
      I := lStart + Length(PropertyNames[lProperty]);
      end;
    lPrev := Result[I - 1];
    end;
end;


// Where the next @import rule of a stylesheet starts, or zero. Text
// inside a comment does not count.
function SVGFindImport(const aText: TSVGString; aFrom: Integer): Integer;

var
  I: Integer;

begin
  Result := 0;
  I := aFrom;
  while I < Length(aText) do
    begin
    if (aText[I] = '/') and (aText[I + 1] = '*') then
      begin
      Inc(I, 2);
      while (I < Length(aText))
            and not ((aText[I] = '*') and (aText[I + 1] = '/')) do
        Inc(I);
      Inc(I, 2);
      Continue;
      end;
    if (aText[I] = '@') and SameText(Copy(aText, I, 7), '@import') then
      Exit(I);
    Inc(I);
    end;
end;


// The stylesheet of an @import rule, empty when the rule has none. A
// media query after the name is dropped with it.
function SVGImportHRef(const aRule: TSVGString): TSVGString;

var
  lAt, lStop: Integer;
  lQuote: AnsiChar;

begin
  Result := '';
  lAt := Pos('url(', LowerCase(aRule));
  if lAt > 0 then
    begin
    Inc(lAt, 3);
    lStop := PosEx(')', aRule, lAt);
    if lStop = 0 then
      Exit;
    Result := Trim(Copy(aRule, lAt + 1, lStop - lAt - 1));
    end
  else
    begin
    lAt := 1;
    while (lAt <= Length(aRule)) and not (aRule[lAt] in ['''', '"']) do
      Inc(lAt);
    if lAt > Length(aRule) then
      Exit;
    lQuote := aRule[lAt];
    lStop := PosEx(TSVGString(lQuote), aRule, lAt + 1);
    if lStop = 0 then
      Exit;
    Result := Copy(aRule, lAt + 1, lStop - lAt - 1);
    end;
  if (Length(Result) > 1) and (Result[1] in ['''', '"'])
     and (Result[Length(Result)] = Result[1]) then
    Result := Copy(Result, 2, Length(Result) - 2);
  Result := Trim(Result);
end;


// The directory part of an href, empty when it has none.
function SVGHRefDirectory(const aHRef: TSVGString): TSVGString;

var
  I: Integer;

begin
  Result := '';
  for I := Length(aHRef) downto 1 do
    if aHRef[I] = '/' then
      Exit(Copy(aHRef, 1, I));
end;


// An href read inside a stylesheet, expressed relative to the document
// that loaded the sheet. An href that is already absolute is left alone.
function SVGJoinHRef(const aPrefix, aHRef: TSVGString): TSVGString;

begin
  if (aPrefix = '') or (aHRef = '') or (aHRef[1] = '/')
     or (Pos('://', aHRef) > 0) then
    Result := aHRef
  else
    Result := aPrefix + aHRef;
end;


procedure TSVGStyleResolver.CollectStyleSheets(aElement: TSVGElement;
  aCSS: TCSSResolver; aDocument: TSVGDocument; var aCount: Integer);

var
  I: Integer;
  lSource: RawByteString;

begin
  if aElement is TSVGStyleElement then
    begin
    lSource := SVGCanonicalPropertyNames(
      Trim(ExpandImports(aElement.TextContent, '', aDocument, 0)), False);
    if lSource <> '' then
      begin
      aCSS.AddStyleSheet(cssoAuthor, Format('svg-style-%d', [aCount]),
        lSource);
      Inc(aCount);
      if aCSS = FResolver then
        CollectFontFaces(lSource);
      end;
    Exit;
    end;
  for I := 0 to aElement.ChildCount - 1 do
    if aElement[I] is TSVGElement then
      CollectStyleSheets(TSVGElement(aElement[I]), aCSS, aDocument, aCount);
end;


// Replaces every @import rule of a stylesheet with the text of the sheet
// it refers to. The imported rules then come before the rules that follow
// them. A sheet that cannot be read leaves nothing behind.
function TSVGStyleResolver.ExpandImports(const aText: RawByteString;
  const aPrefix: TSVGString; aDocument: TSVGDocument;
  aDepth: Integer): RawByteString;

var
  lAt, lStop: Integer;
  lHRef: TSVGString;
  lText: RawByteString;

begin
  Result := aText;
  if (FStyleSheets = nil) or (aDepth >= 8) then
    Exit;
  lAt := 1;
  repeat
    lAt := SVGFindImport(Result, lAt);
    if lAt = 0 then
      Exit;
    lStop := PosEx(';', Result, lAt);
    if lStop = 0 then
      Exit;
    lHRef := SVGImportHRef(Copy(Result, lAt, lStop - lAt + 1));
    lText := '';
    if lHRef <> '' then
      lText := FStyleSheets.ResolveStyleSheet(SVGJoinHRef(aPrefix, lHRef),
        aDocument.BaseURI);
    if Trim(lText) <> '' then
      lText := ExpandImports(lText,
        SVGJoinHRef(aPrefix, SVGHRefDirectory(lHRef)), aDocument, aDepth + 1)
    else
      lText := '';
    Delete(Result, lAt, lStop - lAt + 1);
    Insert(lText, Result, lAt);
    Inc(lAt, Length(lText));
  until False;
end;


// Adds the stylesheets of a document to a CSS resolver, in document
// order: first the files of its xml-stylesheet instructions, then its
// style elements. Returns how many sheets were added.
function TSVGStyleResolver.LoadSheetsOf(aDocument: TSVGDocument;
  aCSS: TCSSResolver): Integer;

var
  I: Integer;
  lText: RawByteString;

begin
  Result := 0;
  if FStyleSheets <> nil then
    for I := 0 to aDocument.StyleSheetCount - 1 do
      begin
      lText := SVGCanonicalPropertyNames(
        Trim(ExpandImports(FStyleSheets.ResolveStyleSheet(
          aDocument.StyleSheetHRef[I], aDocument.BaseURI),
          SVGHRefDirectory(aDocument.StyleSheetHRef[I]), aDocument, 0)),
        False);
      if lText = '' then
        Continue;
      aCSS.AddStyleSheet(cssoAuthor, Format('svg-sheet-%d', [Result]),
        lText);
      Inc(Result);
      if aCSS = FResolver then
        CollectFontFaces(lText);
      end;
  if aDocument.Root <> nil then
    CollectStyleSheets(aDocument.Root, aCSS, aDocument, Result);
end;


procedure TSVGStyleResolver.LoadDocument(aDocument: TSVGDocument);

begin
  Unload;
  FDocument := aDocument;
  if FDocument = nil then
    Exit;
  FDocument.CSSResolver := FResolver;
  FDocument.LinkHistory := FLinkHistory;
  if FDocument.Root <> nil then
    RegisterAttributesOf(FDocument.Root);
  FSheetCount := LoadSheetsOf(FDocument, FResolver);
  FResolver.Init;
end;


procedure TSVGStyleResolver.Unload;

var
  I: Integer;

begin
  if FDocument <> nil then
    begin
    FDocument.CSSResolver := nil;
    FDocument := nil;
    end;
  for I := 0 to FSheetDocs.Count - 1 do
    TSVGDocument(FSheetDocs[I]).CSSResolver := nil;
  for I := 0 to FSheetCSS.Count - 1 do
    TCSSResolver(FSheetCSS[I]).Free;
  FSheetDocs.Clear;
  FSheetCSS.Clear;
  FSheetCount := 0;
  FFontFaceCount := 0;
  FServers.Clear;
  FResolver.Clear;
end;


// The CSS resolver a document's own elements are matched against. A
// document other than the loaded one gets a resolver of its own, sharing
// the registry so that the property ids stay valid.
function TSVGStyleResolver.CSSFor(aDocument: TSVGDocument): TCSSResolver;

var
  I: Integer;
  lCSS: TCSSResolver;

begin
  if (aDocument = nil) or (aDocument = FDocument) then
    Exit(FResolver);
  I := FSheetDocs.IndexOf(aDocument);
  if I >= 0 then
    Exit(TCSSResolver(FSheetCSS[I]));
  lCSS := TCSSResolver.Create(nil);
  lCSS.CSSRegistry := FRegistry;
  FSheetDocs.Add(aDocument);
  FSheetCSS.Add(lCSS);
  aDocument.CSSResolver := lCSS;
  aDocument.LinkHistory := FLinkHistory;
  if aDocument.Root <> nil then
    RegisterAttributesOf(aDocument.Root);
  LoadSheetsOf(aDocument, lCSS);
  lCSS.Init;
  Result := lCSS;
end;


function TSVGStyleResolver.CascadedValue(aValues: TCSSAttributeValues;
  aProperty: TSVGProperty; out aValue: TSVGString): Boolean;

var
  lIndex: Integer;

begin
  aValue := '';
  Result := aValues <> nil;
  if not Result then
    Exit;
  lIndex := aValues.IndexOf(FAttributeIDs[aProperty]);
  Result := lIndex >= 0;
  if Result then
    aValue := Trim(FResolver.Detokenize(aValues.Values[lIndex].Tokens));
  Result := Result and (aValue <> '');
end;


// Reads a number. Keeps the current value when the text is malformed.
procedure ApplyNumber(var aTarget: Double; const aValue: TSVGString);

var
  lNumber: Double;
  lCode: Integer;

begin
  Val(Trim(aValue), lNumber, lCode);
  if lCode = 0 then
    aTarget := lNumber;
end;


// Reads an opacity. SVG clamps it to the range 0 to 1.
procedure ApplyOpacity(var aTarget: Double; const aValue: TSVGString);

var
  lNumber: Double;

begin
  lNumber := aTarget;
  ApplyNumber(lNumber, aValue);
  if lNumber < 0 then
    lNumber := 0
  else if lNumber > 1 then
    lNumber := 1;
  aTarget := lNumber;
end;


// Reads a dash pattern. Both none and a malformed list mean no dashes.
procedure ApplyDashes(var aPen: TSVGPen; const aValue: TSVGString);

var
  lValues: TSVGDoubleArray;
  I: Integer;
  lTotal: Double;

begin
  if SameText(Trim(aValue), 'none') then
    begin
    aPen.Dashes := nil;
    Exit;
    end;
  if not TryStrToSVGDashArray(aValue, lValues) then
    Exit;
  lTotal := 0;
  for I := 0 to High(lValues) do
    begin
    if lValues[I] < 0 then
      Exit;
    lTotal := lTotal + lValues[I];
    end;
  if lTotal <= 0 then
    begin
    aPen.Dashes := nil;
    Exit;
    end;
  aPen.Dashes := lValues;
end;


procedure TSVGStyleResolver.ApplyDeclaration(var aStyle: TSVGComputedStyle;
  aProperty: TSVGProperty; const aValue: TSVGString;
  const aParent: TSVGComputedStyle);

var
  lText: TSVGString;
  lLength: TSVGLength;

begin
  lText := Trim(aValue);
  if SameText(lText, 'inherit') then
    begin
    case aProperty of
      prFill: aStyle.Fill := aParent.Fill;
      prFillOpacity: aStyle.FillOpacity := aParent.FillOpacity;
      prFillRule: aStyle.FillRule := aParent.FillRule;
      prStroke: aStyle.Stroke := aParent.Stroke;
      prStrokeOpacity: aStyle.StrokeOpacity := aParent.StrokeOpacity;
      prStrokeWidth: aStyle.Pen.Width := aParent.Pen.Width;
      prStrokeLinecap: aStyle.Pen.Cap := aParent.Pen.Cap;
      prStrokeLinejoin: aStyle.Pen.Join := aParent.Pen.Join;
      prStrokeMiterlimit: aStyle.Pen.MiterLimit := aParent.Pen.MiterLimit;
      prStrokeDasharray: aStyle.Pen.Dashes := aParent.Pen.Dashes;
      prStrokeDashoffset: aStyle.Pen.DashOffset := aParent.Pen.DashOffset;
      prOpacity: aStyle.Opacity := aParent.Opacity;
      prColor: aStyle.Color := aParent.Color;
      prStopColor: aStyle.StopColor := aParent.StopColor;
      prStopOpacity: aStyle.StopOpacity := aParent.StopOpacity;
      prDisplay: aStyle.Display := aParent.Display;
      prVisibility: aStyle.Visibility := aParent.Visibility;
      prFontFamily: aStyle.FontFamily := aParent.FontFamily;
      prFontSize: aStyle.FontSize := aParent.FontSize;
      prClipRule: aStyle.ClipRule := aParent.ClipRule;
      prOverflow: aStyle.Overflow := aParent.Overflow;
      prClip: aStyle.Clip := aParent.Clip;
      prClipPath: aStyle.ClipPath := aParent.ClipPath;
      prMask: aStyle.Mask := aParent.Mask;
      prFilter: aStyle.Filter := aParent.Filter;
      prTextAnchor: aStyle.TextAnchor := aParent.TextAnchor;
      prColorInterpolation:
        aStyle.ColorInterpolation := aParent.ColorInterpolation;
      prFilterInterpolation:
        aStyle.FilterInterpolation := aParent.FilterInterpolation;
      prMarkerStart: aStyle.MarkerStart := aParent.MarkerStart;
      prMarkerMid: aStyle.MarkerMid := aParent.MarkerMid;
      prMarkerEnd: aStyle.MarkerEnd := aParent.MarkerEnd;
      prTextDecoration: aStyle.Decoration := aParent.Decoration;
      prDominantBaseline, prAlignmentBaseline:
        aStyle.Baseline := aParent.Baseline;
      prBaselineShift: aStyle.Shift := aParent.Shift;
      prFontWeight: aStyle.FontWeight := aParent.FontWeight;
      prFontStyle: aStyle.FontStyle := aParent.FontStyle;
      prFontStretch: aStyle.FontStretch := aParent.FontStretch;
      prFontVariant: aStyle.FontVariant := aParent.FontVariant;
      prLetterSpacing: aStyle.LetterSpacing := aParent.LetterSpacing;
      prWordSpacing: aStyle.WordSpacing := aParent.WordSpacing;
      prWritingMode: aStyle.WritingMode := aParent.WritingMode;
      prGlyphOrientationVertical:
        aStyle.GlyphOrientation := aParent.GlyphOrientation;
    end;
    Exit;
    end;
  case aProperty of
    prFill: ApplyPaint(aStyle.Fill, lText, aStyle.Color);
    prFillOpacity: ApplyOpacity(aStyle.FillOpacity, lText);
    prFillRule: TryStrToSVGFillRule(lText, aStyle.FillRule);
    prStroke: ApplyPaint(aStyle.Stroke, lText, aStyle.Color);
    prStrokeOpacity: ApplyOpacity(aStyle.StrokeOpacity, lText);
    prStrokeWidth:
      if lLength.TryParse(lText) and (lLength.Value >= 0) then
        aStyle.Pen.Width := lLength.Value;
    prStrokeLinecap: TryStrToSVGLineCap(lText, aStyle.Pen.Cap);
    prStrokeLinejoin: TryStrToSVGLineJoin(lText, aStyle.Pen.Join);
    prStrokeMiterlimit: ApplyNumber(aStyle.Pen.MiterLimit, lText);
    prStrokeDasharray: ApplyDashes(aStyle.Pen, lText);
    prStrokeDashoffset:
      if lLength.TryParse(lText) then
        aStyle.Pen.DashOffset := lLength.Value;
    prOpacity: ApplyOpacity(aStyle.Opacity, lText);
    prColor:
      if SameText(lText, 'currentColor') then
        aStyle.Color := aParent.Color
      else
        aStyle.Color.TryParse(lText);
    // currentColor is the colour of the element the stop belongs to, which
    // is computed before the loop that reaches here.
    prStopColor:
      if SameText(lText, 'currentColor') then
        aStyle.StopColor := aStyle.Color
      else
        aStyle.StopColor.TryParse(lText);
    prStopOpacity: ApplyOpacity(aStyle.StopOpacity, lText);
    prDisplay: aStyle.Display := TSVGDisplay(Ord(SameText(lText, 'none')));
    prVisibility: TryStrToSVGVisibility(lText, aStyle.Visibility);
    prFontFamily: aStyle.FontFamily := lText;
    prFontSize:
      if lLength.TryParse(lText) and (lLength.Value >= 0) then
        aStyle.FontSize := lLength;
    prClipRule: TryStrToSVGFillRule(lText, aStyle.ClipRule);
    prOverflow: TryStrToSVGOverflow(lText, aStyle.Overflow);
    prClip: TryStrToSVGClipShape(lText, aStyle.Clip);
    prClipPath: aStyle.ClipPath := SVGReferenceOf(lText);
    prMask: aStyle.Mask := SVGReferenceOf(lText);
    prFilter: aStyle.Filter := SVGReferenceOf(lText);
    prTextAnchor: TryStrToSVGTextAnchor(lText, aStyle.TextAnchor);
    prWritingMode: TryStrToSVGWritingMode(lText, aStyle.WritingMode);
    prGlyphOrientationVertical:
      TryStrToSVGGlyphOrientation(lText, aStyle.GlyphOrientation);
    prColorInterpolation:
      TryStrToSVGColorInterpolation(lText, aStyle.ColorInterpolation);
    prFilterInterpolation:
      TryStrToSVGColorInterpolation(lText, aStyle.FilterInterpolation);
    prMarkerStart: aStyle.MarkerStart := SVGReferenceOf(lText);
    prMarkerMid: aStyle.MarkerMid := SVGReferenceOf(lText);
    prMarkerEnd: aStyle.MarkerEnd := SVGReferenceOf(lText);
    prDominantBaseline, prAlignmentBaseline:
      TryStrToSVGBaseline(lText, aStyle.Baseline);
    prBaselineShift: TryStrToSVGBaselineShift(lText, aStyle.Shift);
    prTextDecoration: aStyle.Decoration := SVGDecorationsOf(lText);
    prFontWeight: ApplyFontWeight(aStyle.FontWeight, lText, aParent.FontWeight);
    prFontStyle: TryStrToSVGFontStyle(lText, aStyle.FontStyle);
    prFontStretch:
      ApplyFontStretch(aStyle.FontStretch, lText, aParent.FontStretch);
    prFontVariant:
      if SameText(Trim(lText), 'small-caps') then
        aStyle.FontVariant := fvSmallCaps
      else if SameText(Trim(lText), 'normal') then
        aStyle.FontVariant := fvNormal;
    prLetterSpacing: ApplySpacing(aStyle.LetterSpacing, lText);
    prWordSpacing: ApplySpacing(aStyle.WordSpacing, lText);
  end;
end;


// The text of a value as it was written. A url is reassembled here: the
// scanner consumes its closing bracket without storing it, so the text in
// the tree cannot be read back unchanged.
function CSSValueText(aElement: TCSSElement): TSVGString;

var
  I: Integer;

begin
  if aElement is TCSSURLElement then
    begin
    Result := Trim(TCSSURLElement(aElement).Value);
    // The value is the address alone. A CSS engine that keeps the whole
    // url() is accepted as well, apart from the closing bracket that
    // older ones drop.
    if SameText(Copy(Result, 1, 4), 'url(') then
      begin
      if (Result <> '') and (Result[Length(Result)] <> ')') then
        Result := Result + ')';
      end
    else
      Result := 'url(' + Result + ')';
    end
  else if aElement is TCSSListElement then
    begin
    Result := '';
    for I := 0 to TCSSListElement(aElement).ChildCount - 1 do
      begin
      if Result <> '' then
        Result := Result + ' ';
      Result := Result + CSSValueText(TCSSListElement(aElement).Children[I]);
      end;
    end
  else
    Result := aElement.AsString;
end;


// True when the CSS resolver drops a declaration with this value instead
// of cascading it. It has no token for a function call, so the url() of a
// reference and the rect() of a clip are both lost.
function SVGDroppedByTheCascade(const aValue: TSVGString): Boolean;

var
  lValue: TSVGString;

begin
  lValue := LowerCase(aValue);
  Result := (Pos('url(', lValue) > 0) or (Pos('rect(', lValue) > 0);
end;


// Applies one declaration whose value holds a url. The marker shorthand
// sets the three marker properties at once.
procedure TSVGStyleResolver.ApplyNamedDeclaration(var aStyle: TSVGComputedStyle;
  const aName, aValue: TSVGString; const aParent: TSVGComputedStyle);

var
  lProperty: TSVGProperty;

begin
  if SameText(Trim(aName), 'marker') then
    begin
    ApplyDeclaration(aStyle, prMarkerStart, aValue, aParent);
    ApplyDeclaration(aStyle, prMarkerMid, aValue, aParent);
    ApplyDeclaration(aStyle, prMarkerEnd, aValue, aParent);
    end
  else if TryStrToSVGProperty(aName, lProperty) then
    ApplyDeclaration(aStyle, lProperty, aValue, aParent);
end;


// Applies the declarations with a url from the rules that matched an
// element. The list runs from the weakest rule to the strongest, so the
// last one applied wins.
procedure TSVGStyleResolver.ApplyDroppedRuleDeclarations(var aStyle: TSVGComputedStyle;
  aRules: TCSSSharedRuleList; const aParent: TSVGComputedStyle);

var
  I, J, K: Integer;
  lRule: TCSSRuleElement;
  lDecl: TCSSDeclarationElement;
  lValue: TSVGString;

begin
  if aRules = nil then
    Exit;
  for I := 0 to High(aRules.Rules) do
    begin
    lRule := aRules.Rules[I].Rule;
    if lRule = nil then
      Continue;
    for J := 0 to lRule.ChildCount - 1 do
      begin
      if not (lRule.Children[J] is TCSSDeclarationElement) then
        Continue;
      lDecl := TCSSDeclarationElement(lRule.Children[J]);
      if lDecl.KeyCount = 0 then
        Continue;
      lValue := '';
      for K := 0 to lDecl.ChildCount - 1 do
        begin
        if lValue <> '' then
          lValue := lValue + ' ';
        lValue := lValue + CSSValueText(lDecl.Children[K]);
        end;
      if not SVGDroppedByTheCascade(lValue) then
        Continue;
      ApplyNamedDeclaration(aStyle, Trim(lDecl.Keys[0].AsString), lValue, aParent);
      end;
    end;
end;


procedure TSVGStyleResolver.ApplyDroppedDeclarations(
  var aStyle: TSVGComputedStyle; aElement: TSVGElement;
  aRules: TCSSSharedRuleList; const aParent: TSVGComputedStyle);

var
  lText, lItem, lName, lValue: TSVGString;
  lStart, lDepth, I, lColon: Integer;
  lProperty: TSVGProperty;

  procedure TakeItem;
  begin
    lColon := Pos(':', lItem);
    if lColon = 0 then
      Exit;
    lName := Trim(Copy(lItem, 1, lColon - 1));
    lValue := Trim(Copy(lItem, lColon + 1, Length(lItem)));
    if not SVGDroppedByTheCascade(lValue) then
      Exit;
    ApplyNamedDeclaration(aStyle, lName, lValue, aParent);
  end;

begin
  // A rule is weaker than the style attribute, so it goes on first.
  ApplyDroppedRuleDeclarations(aStyle, aRules, aParent);
  if not aElement.HasAttribute('style') then
    Exit;
  lText := aElement.Attributes['style'];
  lStart := 1;
  lDepth := 0;
  for I := 1 to Length(lText) do
    case lText[I] of
      '(': Inc(lDepth);
      ')': if lDepth > 0 then Dec(lDepth);
      ';':
        if lDepth = 0 then
          begin
          lItem := Copy(lText, lStart, I - lStart);
          TakeItem;
          lStart := I + 1;
          end;
    end;
  lItem := Copy(lText, lStart, Length(lText));
  TakeItem;
end;


procedure TSVGStyleResolver.ApplyPaint(var aPaint: TSVGPaint;
  const aValue: TSVGString; const aCurrentColor: TSVGColor);

var
  lParsed: TSVGPaint;

begin
  if not lParsed.TryParse(aValue, aCurrentColor, FDocument) then
    Exit;
  // The helper only recognises a url as a server reference. The element it
  // points to becomes a paint server here, where the cache is.
  if lParsed.Kind = spServer then
    begin
    // A value may have a fallback after the reference. That fallback is
    // not part of the reference.
    lParsed.Server := PaintServerOf(
      PaintTargetOf(Copy(Trim(aValue), 1, Pos(')', Trim(aValue)))));
    if lParsed.Server = nil then
      begin
      lParsed := lParsed.Resolved;
      if lParsed.Kind = spServer then
        Exit;
      end;
    end;
  aPaint := lParsed;
end;


function TSVGStyleResolver.ComputeStyle(aElement: TSVGElement;
  const aParent: TSVGComputedStyle): TSVGComputedStyle;

var
  P: TSVGProperty;
  lRules: TCSSSharedRuleList;
  lValues: TCSSAttributeValues;
  lInline: TCSSRuleElement;
  lCSS: TCSSResolver;
  lValue: TSVGString;

begin
  Result := aParent.Inherit;
  if aElement = nil then
    Exit;
  lValues := nil;
  lRules := nil;
  lInline := nil;
  // A copy made for a use is styled as if it stood at the position of the
  // original, so selectors are matched against the element it was copied
  // from, in the cascade of the document that holds it. Its own
  // attributes are the same either way.
  if aElement.CSSOrigin = nil then
    lCSS := FResolver
  else
    lCSS := CSSFor(aElement.CSSOrigin.Document);
  try
    if aElement.HasAttribute('style') then
      lInline := lCSS.ParseInlineStyle(
        SVGCanonicalPropertyNames(aElement.Attributes['style'], True))
        as TCSSRuleElement;
    if aElement.CSSOrigin <> nil then
      lCSS.Compute(aElement.CSSOrigin, lInline, lRules, lValues)
    else
      lCSS.Compute(aElement, lInline, lRules, lValues);
    // The colour resolves first, because currentColor in any other
    // property reads it.
    if CascadedValue(lValues, prColor, lValue) then
      ApplyDeclaration(Result, prColor, lValue, aParent)
    else if aElement.HasAttribute('color') then
      ApplyDeclaration(Result, prColor, aElement.Attributes['color'], aParent);
    for P := Low(TSVGProperty) to High(TSVGProperty) do
      begin
      if P = prColor then
        Continue;
      if CascadedValue(lValues, P, lValue) then
        ApplyDeclaration(Result, P, lValue, aParent)
      else if aElement.HasAttribute(PropertyNames[P]) then
        ApplyDeclaration(Result, P, aElement.Attributes[PropertyNames[P]],
          aParent);
      end;
    // A declaration whose value calls a function never reaches the
    // cascade. Those are read from the rules and the inline style here,
    // in the same order of priority.
    ApplyDroppedDeclarations(Result, aElement, lRules, aParent);
  finally
    lValues.Free;
    lInline.Free;
  end;
end;


function TSVGStyleResolver.ComputeStyleOf(aElement: TSVGElement): TSVGComputedStyle;

begin
  if aElement = nil then
    Result := TSVGComputedStyle.Initial
  else if aElement.Parent = nil then
    Result := ComputeStyle(aElement, TSVGComputedStyle.Initial)
  else
    Result := ComputeStyle(aElement, ComputeStyleOf(aElement.Parent));
end;


{ TSVGComputedStyle }

class function TSVGComputedStyle.Initial: TSVGComputedStyle;

begin
  Result.Fill := TSVGPaint.CreateColor(TSVGColor.Black);
  Result.FillOpacity := 1;
  Result.FillRule := frNonZero;
  Result.Stroke := TSVGPaint.None;
  Result.StrokeOpacity := 1;
  Result.Pen := TSVGPen.Default;
  Result.Opacity := 1;
  Result.Color := TSVGColor.Black;
  Result.StopColor := TSVGColor.Black;
  Result.StopOpacity := 1;
  Result.Display := sdInline;
  Result.Visibility := svVisible;
  Result.FontFamily := '';
  Result.FontSize := TSVGLength.Create(16, luNumber);
  Result.ClipRule := frNonZero;
  // overflow only reaches an element that establishes a viewport, and the
  // SVG 1.1 user agent stylesheet hides all of those.
  Result.Overflow := ovHidden;
  Result.Clip := TSVGClipShape.Auto;
  Result.ClipPath := '';
  Result.Mask := '';
  Result.Filter := '';
  Result.TextAnchor := taStart;
  Result.ColorInterpolation := ciSRGB;
  // SVG applies a filter to linear light unless a document says
  // otherwise.
  Result.FilterInterpolation := ciLinearRGB;
  Result.Baseline := dbAuto;
  Result.Shift := TSVGBaselineShift.None;
  Result.Decoration := [];
  Result.MarkerStart := '';
  Result.MarkerMid := '';
  Result.MarkerEnd := '';
  Result.FontWeight := SVGNormalFontWeight;
  Result.FontStyle := fnNormal;
  Result.FontStretch := fsNormal;
  Result.FontVariant := fvNormal;
  Result.LetterSpacing := 0;
  Result.WordSpacing := 0;
  Result.WritingMode := wmLRTB;
  Result.GlyphOrientation := goAuto;
end;


function TSVGComputedStyle.Inherit: TSVGComputedStyle;

begin
  Result := Self;
  Result.Opacity := 1;
  Result.StopColor := TSVGColor.Black;
  Result.StopOpacity := 1;
  Result.Display := sdInline;
  // The shift moves the baseline that the children then sit on, so they
  // must not shift again by the same amount.
  Result.Shift := TSVGBaselineShift.None;
  Result.Overflow := ovHidden;
  Result.Clip := TSVGClipShape.Auto;
  Result.ClipPath := '';
  Result.Mask := '';
  Result.Filter := '';
end;


function TSVGComputedStyle.IsDisplayed: Boolean;

begin
  Result := Display <> sdNone;
end;


function TSVGComputedStyle.IsPainted: Boolean;

begin
  Result := IsDisplayed and (Visibility = svVisible) and (Opacity > 0);
end;


function TSVGComputedStyle.ToString: TSVGString;

begin
  Result := 'fill=' + Fill.ToString
    + ' fill-opacity=' + SVGFormatFloat(FillOpacity)
    + ' fill-rule=' + FillRuleNames[FillRule] + LineEnding
    + 'stroke=' + Stroke.ToString
    + ' stroke-opacity=' + SVGFormatFloat(StrokeOpacity)
    + ' ' + Pen.ToString + LineEnding
    + 'opacity=' + SVGFormatFloat(Opacity)
    + ' color=' + Color.ToString
    + ' display=' + DisplayNames[Display]
    + ' visibility=' + VisibilityNames[Visibility] + LineEnding
    + 'font-family="' + FontFamily + '"'
    + ' font-size=' + FontSize.ToString
    + ' clip-rule=' + FillRuleNames[ClipRule] + LineEnding
    + 'overflow=' + OverflowNames[Overflow]
    + ' clip=' + Clip.ToString
    + ' clip-path="' + ClipPath + '"'
    + ' mask="' + Mask + '"' + LineEnding
    + 'stop-color=' + StopColor.ToString
    + ' stop-opacity=' + SVGFormatFloat(StopOpacity) + LineEnding
    + 'color-interpolation=' + MixingNames[ColorInterpolation] + LineEnding
    + 'text-anchor=' + AnchorNames[TextAnchor]
    + ' font-weight=' + IntToStr(FontWeight)
    + ' font-style=' + FontStyleNames[FontStyle]
    + ' font-stretch=' + SVGFontStretchName(FontStretch)
    + ' font-variant=' + VariantNames[FontVariant]
    + ' letter-spacing=' + SVGFormatFloat(LetterSpacing)
    + ' word-spacing=' + SVGFormatFloat(WordSpacing) + LineEnding
    + 'writing-mode=' + WritingModeNames[WritingMode]
    + ' glyph-orientation-vertical=' + OrientationNames[GlyphOrientation];
end;


end.
