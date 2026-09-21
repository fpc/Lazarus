{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Render tree walk: CTM and style stacks, viewports, layers and conditions.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.render;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}
{$modeswitch advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.Math, fpsvg.types, fpsvg.dom,
     fpsvg.read, fpsvg.path, fpsvg.style, fpsvg.geom, fpsvg.text,
     fpsvg.svgfont, fpsvg.backend;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, math, fpsvg.types, fpsvg.dom, fpsvg.read,
     fpsvg.path, fpsvg.style, fpsvg.geom, fpsvg.text, fpsvg.svgfont,
     fpsvg.backend;
{$ENDIF FPC_DOTTEDUNITS}

const
  SVGDefaultDocumentWidth = 300;
  SVGDefaultDocumentHeight = 150;
  SVGDefaultUseDepth = 8;
  SVGMaxDocumentDepth = 4;
  { How deep markers may nest before they are refused. }
  SVGMaxMarkerDepth = 2;
  // A clip path may have a clip path of its own, which may have another.
  // This is how far that chain is followed before the document is taken
  // to refer to itself.
  SVGMaxClipDepth = 8;
  SVGDefaultLanguage = 'en';
  SVGMaxPatternTiles = 4096;

type
  ESVGRender = class(ESVGError);

  { Asks the program for a font file that the resolver could not find.
    Set aFileName to a readable file, or leave it be to decline. }
  TSVGFontFileNeededEvent = procedure(aSender: TObject;
    const aURL, aBaseURI: String; var aFileName: String) of object;

  { Finds the font files of the stylesheets, next to the document that
    loaded them. A url it cannot find on disk is passed to
    OnFontFileNeeded. }
  TSVGFileFontResolver = class(TObject, ISVGFontFileResolver)
  private
    FBasePath: String;
    FSearchPath: String;
    FOnFontFileNeeded: TSVGFontFileNeededEvent;
    FLookups: Integer;
  public
    constructor Create(const aBasePath: String);
    function ResolveFontFile(const aURL, aBaseURI: String): String;
    // Directory a relative url is resolved against, used when the
    // document holding it has no location.
    property BasePath: String read FBasePath write FBasePath;
    // Directory searched for the bare file name when the url leads
    // nowhere.
    property SearchPath: String read FSearchPath write FSearchPath;
    // Number of urls looked up.
    property Lookups: Integer read FLookups;
    // Called when no file was found for a url, so the program can supply
    // one.
    property OnFontFileNeeded: TSVGFontFileNeededEvent read FOnFontFileNeeded
      write FOnFontFileNeeded;
  end;

  { Reads the stylesheet of an href from a file. }
  TSVGFileStyleSheetResolver = class(TObject, ISVGStyleSheetResolver)
  private
    FBasePath: String;
    FReads: Integer;
  public
    constructor Create(const aBasePath: String);
    function ResolveStyleSheet(const aHRef,
      aBaseURI: String): RawByteString;
    // Directory a relative href is resolved against, used when the
    // document holding it has no location.
    property BasePath: String read FBasePath write FBasePath;
    // Number of files actually read.
    property Reads: Integer read FReads;
  end;

  { Reads the documents that references point to from the file system, and
    keeps each one, so that a file used many times is read once. }
  TSVGFileDocumentResolver = class(TObject, ISVGDocumentResolver)
  private
    FBasePath: String;
    FCache: TStringList;
    FReads: Integer;
  public
    constructor Create(const aBasePath: String);
    destructor Destroy; override;
    function ResolveDocument(const aPath, aBaseURI: String): TSVGDocument;
    // Directory a relative reference is resolved against, used when the
    // document holding it has no location.
    property BasePath: String read FBasePath write FBasePath;
    // Number of files actually read. A second reference to the same file
    // does not add to it.
    property Reads: Integer read FReads;
  end;

  { The values the walk passes from an element down into its children. }
  TSVGRenderState = record
    CTM      : TSVGMatrix;
    Viewport : TSVGRect;
    Style    : TSVGComputedStyle;
    Alpha    : Double;
    // The state a render starts from: no transform, the initial SVG
    // style, and the viewport that percentages are measured against.
    constructor Create(const aViewport: TSVGRect);
  end;

  { Walks a document and drives a backend. It knows of no rasterizer and
    of no surface. }
  TSVGRenderer = class(TObject)
  private
    FBackend      : TSVGRenderBackend;
    FDocument     : TSVGDocument;
    FStyles       : TSVGStyleResolver;
    FExpander     : TSVGUseExpander;
    FPath         : TSVGPath;
    FClipPath     : TSVGPath;
    FBoundsPath   : TSVGPath;
    FBoundsPoly   : TSVGPolyPath;
    FImages       : ISVGImageResolver;
    FDocuments    : ISVGDocumentResolver;
    FLinkHistory  : ISVGLinkHistory;
    FFonts        : ISVGFontProvider;
    FFontFiles    : ISVGFontFileResolver;
    FFontsLoaded  : Integer;
    FLayout       : TSVGTextLayout;
    FDPI          : Double;
    FView         : TSVGView;
    FLanguage     : String;
    FElementCount : Integer;
    FMaxUseDepth  : Integer;
    FPatternDepth : Integer;
    FDocumentDepth : Integer;
    FOpenDocuments : TStringList;
    // The elements that the feImage primitives of the chain being drawn
    // refer to, one per primitive and nil for every other kind.
    FFilterImages : array of TSVGElement;
    FMarkerDepth   : Integer;
    FClipDepth     : Integer;
    FTextPath      : TSVGPath;
    FBaseURI       : String;
    FStyleSheets   : ISVGStyleSheetResolver;
    FDocumentFonts : ISVGFontProvider;
    function ContextOf(const aState: TSVGRenderState): TSVGLengthContext;
    function TransformOf(aElement: TSVGElement): TSVGMatrix;
    function LengthOf(aElement: TSVGElement; const aName: String;
      const aContext: TSVGLengthContext; aAxis: TSVGLengthAxis;
      aDefault: Double): Double;
    function ConditionsPass(aElement: TSVGElement): Boolean;
    function RootFillsItsViewport(aDocument: TSVGDocument): Boolean;
    function LayerNeeded(const aStyle: TSVGComputedStyle): Boolean;
    procedure GetSetDocuments(aValue: ISVGDocumentResolver);
    procedure GetSetLinkHistory(aValue: ISVGLinkHistory);
    procedure GetSetStyleSheets(aValue: ISVGStyleSheetResolver);
    procedure LoadDocumentFonts(aDocument: TSVGDocument);
    procedure LoadNamedFonts(aElement: TSVGElement;
      aProvider: TSVGDocumentFontProvider);
    function TextFonts: ISVGFontProvider;
    procedure RenderTextStroke(const aRun: TSVGTextRun;
      const aState: TSVGRenderState; aOpacity: Double);
    procedure RenderDecoration(const aBand: TSVGTextBand;
      const aState: TSVGRenderState; aOpacity: Double);
    procedure RenderMarkers(aElement: TSVGElement;
      const aState: TSVGRenderState);
    procedure RenderMarkerAt(aMarker: TSVGElement;
      const aVertex: TSVGPathVertex; const aState: TSVGRenderState);
    procedure RenderElement(aElement: TSVGElement;
      const aParent: TSVGRenderState);
    procedure RenderChildren(aElement: TSVGElement;
      const aState: TSVGRenderState; aFirstMatch: Boolean);
    procedure RenderContainer(aElement: TSVGElement;
      const aState: TSVGRenderState; aFirstMatch: Boolean);
    procedure RenderViewport(aElement: TSVGElement;
      const aState: TSVGRenderState);
    procedure RenderShape(aElement: TSVGElement;
      const aState: TSVGRenderState);
    procedure RenderUse(aUse: TSVGElement; const aState: TSVGRenderState);
    function UseSubtree(aUse: TSVGElement;
      out aKey: String): TSVGElement;
    function ExternalUseSubtree(aUse: TSVGElement;
      const aPath, aFragment: String; out aKey: String): TSVGElement;
    function ExternalDocumentOf(aFrom: TSVGElement;
      const aPath: String): TSVGDocument;
    function RenderImageDocument(aElement: TSVGElement;
      const aState: TSVGRenderState; const aPath: String): Boolean;
    procedure RenderImage(aElement: TSVGElement;
      const aState: TSVGRenderState);
    procedure RenderText(aElement: TSVGElement;
      const aState: TSVGRenderState);
    function BuildTextPath(aElement: TSVGElement;
      const aState: TSVGRenderState; aPath: TSVGPath): Boolean;
    procedure RenderSelf(aElement: TSVGElement;
      const aState: TSVGRenderState);
    procedure ReadPrimitive(aElement: TSVGElement; aKind: TSVGFilterKind;
      aIndex: Integer; aBoxUnits: Boolean; const aBounds, aRegion: TSVGRect;
      const aContext: TSVGLengthContext; aResults: TStringList;
      out aPrimitive: TSVGFilterPrimitive);
    function PrimitiveColour(aElement: TSVGElement;
      const aName: String; const aFallback: TSVGColor): TSVGColor;
    function FilterChainOf(aElement, aFilter: TSVGElement;
      const aState: TSVGRenderState; out aChain: TSVGFilterChain): Boolean;
    procedure RenderFiltered(aElement: TSVGElement;
      const aState: TSVGRenderState);
    procedure RenderShielded(aElement, aClip, aMask: TSVGElement;
      const aState: TSVGRenderState);
    procedure RenderClipAsMask(aClip: TSVGElement;
      const aState: TSVGRenderState; const aBounds: TSVGRect);
    procedure FillClipShapes(aElement: TSVGElement;
      const aState: TSVGRenderState);
    procedure RenderMaskContent(aMask: TSVGElement;
      const aState: TSVGRenderState; const aBounds: TSVGRect);
    function PaintWithPattern(aPath: TSVGPath; const aPaint: TSVGPaint;
      const aState: TSVGRenderState; const aBounds: TSVGRect;
      aRule: TSVGFillRule; const aPen: TSVGPen; aStroked: Boolean;
      aOpacity: Double): Boolean;
    function ReferencedElement(aFrom: TSVGElement; const aReference: String;
      aClass: TSVGElementClass): TSVGElement;
    function SingleClipShape(aClip: TSVGElement): TSVGElement;
    function UnitsTransform(aElement: TSVGElement; const aName: String;
      const aBounds: TSVGRect): TSVGMatrix;
    function BoundsOf(aElement: TSVGElement;
      const aState: TSVGRenderState): TSVGRect;
    function PathBounds(aPath: TSVGPath): TSVGRect;
  public
    constructor Create;
    destructor Destroy; override;
    // The device size a document requests, from its width and height, or
    // from its viewBox when those are absent. False when there is nothing
    // to render.
    function DocumentSize(aDocument: TSVGDocument;
      out aWidth, aHeight: Integer): Boolean;
    // Renders a document into a frame of the size it requests.
    procedure Render(aDocument: TSVGDocument; aBackend: TSVGRenderBackend);
    // Number of fonts the last render loaded from font-face rules.
    property FontsLoaded: Integer read FFontsLoaded;
    // Renders a document into a frame of the given device size, scaled to
    // fill it. This stretches a document whose shape differs from the
    // frame.
    procedure RenderToSize(aDocument: TSVGDocument; aBackend: TSVGRenderBackend;
      aWidth, aHeight: Integer);
    // Renders a document into a frame of the given device size. It is
    // scaled by the same amount in both directions and centred, so the
    // drawing keeps its shape and the spare room stays empty.
    procedure RenderToFit(aDocument: TSVGDocument; aBackend: TSVGRenderBackend;
      aWidth, aHeight: Integer);
    // Renders a document into a frame the caller has already begun, under
    // the given transform and viewport.
    procedure RenderInFrame(aDocument: TSVGDocument;
      aBackend: TSVGRenderBackend; const aCTM: TSVGMatrix;
      const aViewport: TSVGRect);
    // Number of elements the last render visited.
    property ElementCount: Integer read FElementCount;
    // Dots per inch that absolute units resolve against.
    property DPI: Double read FDPI write FDPI;
    // Language that systemLanguage tests are answered against.
    property Language: String read FLanguage write FLanguage;
    // How deep a chain of use elements may go before it is refused.
    property MaxUseDepth: Integer read FMaxUseDepth write FMaxUseDepth;
    // Supplies the pixels of image elements. Without one, an image draws
    // nothing.
    property Images: ISVGImageResolver read FImages write FImages;
    // Supplies the documents that references in other files point to.
    // Without it, such a reference draws nothing.
    property Documents: ISVGDocumentResolver read FDocuments
      write GetSetDocuments;
    // Supplies the faces of text elements. Without one, text draws
    // nothing.
    property Fonts: ISVGFontProvider read FFonts write FFonts;
    // Says which links have been followed before, so that :visited
    // matches them. Without it no link has been followed, and every link
    // with a target matches :link.
    property LinkHistory: ISVGLinkHistory read FLinkHistory
      write GetSetLinkHistory;
    // The view the next render frames the root with. Set it to
    // TSVGView.None, its initial value, to use the viewBox and
    // preserveAspectRatio of the root itself.
    property View: TSVGView read FView write FView;
    // Supplies the files the font-face rules of a document request.
    // Without it, such a rule loads no font.
    property FontFiles: ISVGFontFileResolver read FFontFiles
      write FFontFiles;
    // Supplies the stylesheets of a document's xml-stylesheet
    // instructions. Without it, such a sheet is not loaded.
    property StyleSheets: ISVGStyleSheetResolver read FStyleSheets
      write GetSetStyleSheets;
  end;

// Renders a document to a backend, using a renderer created for it.
procedure RenderSVGDocument(aDocument: TSVGDocument;
  aBackend: TSVGRenderBackend);

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ELSE FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ENDIF FPC_DOTTEDUNITS}

const
  { The feature strings this renderer answers requiredFeatures with.
    A feature that is left out makes a switch skip content that this
    renderer can draw, so the list is extended along with the code. }
  SupportedFeatures: array[0..21] of String = (
    'http://www.w3.org/TR/SVG11/feature#CoreAttribute',
    'http://www.w3.org/TR/SVG11/feature#Structure',
    'http://www.w3.org/TR/SVG11/feature#BasicStructure',
    'http://www.w3.org/TR/SVG11/feature#ContainerAttribute',
    'http://www.w3.org/TR/SVG11/feature#ConditionalProcessing',
    'http://www.w3.org/TR/SVG11/feature#Style',
    'http://www.w3.org/TR/SVG11/feature#Shape',
    'http://www.w3.org/TR/SVG11/feature#Gradient',
    'http://www.w3.org/TR/SVG11/feature#Pattern',
    'http://www.w3.org/TR/SVG11/feature#PaintAttribute',
    'http://www.w3.org/TR/SVG11/feature#BasicPaintAttribute',
    'http://www.w3.org/TR/SVG11/feature#OpacityAttribute',
    'http://www.w3.org/TR/SVG11/feature#GraphicsAttribute',
    'http://www.w3.org/TR/SVG11/feature#BasicGraphicsAttribute',
    'http://www.w3.org/TR/SVG11/feature#Clip',
    'http://www.w3.org/TR/SVG11/feature#BasicClip',
    'http://www.w3.org/TR/SVG11/feature#Mask',
    'http://www.w3.org/TR/SVG11/feature#Image',
    'http://www.w3.org/TR/SVG11/feature#Text',
    'http://www.w3.org/TR/SVG11/feature#BasicText',
    'http://www.w3.org/TR/SVG11/feature#ViewportAttribute',
    'http://www.w3.org/TR/SVG11/feature#XlinkAttribute');

// Splits a comma separated attribute value into trimmed, non-empty items.
procedure SplitConditionList(const aValue: String; aList: TStrings);

var
  lItem: String;
  I: Integer;

begin
  aList.Clear;
  lItem := '';
  for I := 1 to Length(aValue) do
    if aValue[I] = ',' then
      begin
      if Trim(lItem) <> '' then
        aList.Add(Trim(lItem));
      lItem := '';
      end
    else
      lItem := lItem + aValue[I];
  if Trim(lItem) <> '' then
    aList.Add(Trim(lItem));
end;


// True when a language tag is the language itself, or a region of it.
function LanguageMatches(const aTag, aLanguage: String): Boolean;

var
  lPrefix: String;

begin
  Result := SameText(aTag, aLanguage);
  if Result then
    Exit;
  lPrefix := aTag;
  if Pos('-', lPrefix) > 0 then
    lPrefix := Copy(lPrefix, 1, Pos('-', lPrefix) - 1);
  Result := SameText(lPrefix, aLanguage);
end;


// True when the feature string is one this renderer implements.
function FeatureSupported(const aFeature: String): Boolean;

var
  I: Integer;

begin
  Result := True;
  for I := Low(SupportedFeatures) to High(SupportedFeatures) do
    if aFeature = SupportedFeatures[I] then
      Exit;
  Result := False;
end;


// The first child of a node that is an element, or nil.
function FirstElementChild(aElement: TSVGElement): TSVGElement;

var
  I: Integer;

begin
  Result := nil;
  for I := 0 to aElement.ChildCount - 1 do
    if aElement[I] is TSVGElement then
      Exit(TSVGElement(aElement[I]));
end;


// True when a path is absolute, so it needs no base directory.
function SVGIsAbsolutePath(const aName: String): Boolean;

begin
  Result := (aName <> '') and ((aName[1] = PathDelim) or (aName[1] = '/'));
  {$IFDEF WINDOWS}
  Result := Result or ((Length(aName) >= 2) and (aName[2] = ':'));
  {$ENDIF}
end;


// True when a reference points to a document, and not to a raster image.
function SVGNamesADocument(const aPath: String): Boolean;

var
  lExtension: String;

begin
  // A data URI points to no file and declares its content itself.
  if SVGIsDataURI(aPath) then
    Exit(SVGDataURIHoldsSVG(aPath));
  lExtension := LowerCase(ExtractFileExt(aPath));
  Result := (lExtension = '.svg') or (lExtension = '.svgz');
end;


// True when a paint refers to a pattern, and not to a colour or a
// gradient.
function IsPatternPaint(const aPaint: TSVGPaint): Boolean;

begin
  Result := (aPaint.Kind = spServer) and (aPaint.Server <> nil)
        and (aPaint.Server.GetPaintServerKind = pkPattern);
end;


// A coordinate in bounding box units: a bare number, or a percentage of 1.
function BoxFraction(aElement: TSVGElement; const aName: String;
  aDefault: Double): Double;

var
  lLength: TSVGLength;

begin
  lLength := TSVGLength.Create(aDefault, luNumber);
  lLength.ReadAttribute(aElement, aName);
  if lLength.LengthUnit = luPercent then
    Result := lLength.Value / 100
  else
    Result := lLength.Value;
end;


// The number SVG gives a blend mode, normal when the name is unknown.
function SVGBlendModeOf(const aName: String): Integer;

const
  Names: array[0..4] of String = ('normal', 'multiply', 'screen', 'darken',
    'lighten');

var
  I: Integer;

begin
  Result := 0;
  for I := Low(Names) to High(Names) do
    if SameText(Trim(aName), Names[I]) then
      Exit(I);
end;


// The number SVG gives a morphology operator, erode when the name is
// unknown.
function SVGMorphologyOperatorOf(const aName: String): Integer;

begin
  Result := 0;
  if SameText(Trim(aName), 'dilate') then
    Result := 1;
end;


// The number SVG gives a composite operator, over when the name is
// unknown.
function SVGCompositeOperatorOf(const aName: String): Integer;

const
  Names: array[0..5] of String = ('over', 'in', 'out', 'atop', 'xor',
    'arithmetic');

var
  I: Integer;

begin
  Result := 0;
  for I := Low(Names) to High(Names) do
    if SameText(Trim(aName), Names[I]) then
      Exit(I);
end;


// The number SVG gives a colour matrix type, a plain matrix when the name
// is unknown.
function SVGColorMatrixTypeOf(const aName: String): Integer;

const
  Names: array[0..3] of String = ('matrix', 'saturate', 'hueRotate',
    'luminanceToAlpha');

var
  I: Integer;

begin
  Result := 0;
  for I := Low(Names) to High(Names) do
    if SameText(Trim(aName), Names[I]) then
      Exit(I);
end;


// The kind of primitive an element is. False when it is not a primitive.
function SVGFilterKindOf(aElement: TSVGElement;
  out aKind: TSVGFilterKind): Boolean;

const
  Tags: array[TSVGFilterKind] of String = ('feFlood', 'feGaussianBlur',
    'feOffset', 'feMerge', 'feComposite', 'feColorMatrix', 'feBlend',
    'feTile', 'feImage', 'feMorphology', 'feComponentTransfer',
    'feTurbulence', 'feDiffuseLighting', 'feSpecularLighting',
    'feConvolveMatrix', 'feDisplacementMap');

var
  lKind: TSVGFilterKind;

begin
  Result := False;
  if aElement = nil then
    Exit;
  for lKind := Low(TSVGFilterKind) to High(TSVGFilterKind) do
    if aElement.TagName = Tags[lKind] then
      begin
      aKind := lKind;
      Exit(True);
      end;
end;


// The numbers an attribute holds, empty when it holds none.
function SVGNumbersOf(aElement: TSVGElement; const aName: String):
  TSVGDoubleArray;

begin
  Result := nil;
  if not aElement.HasAttribute(aName)
     or not TryStrToSVGNumberList(aElement.Attributes[aName], Result) then
    Result := nil;
end;


// One number an attribute holds, or the fallback when it holds none.
function SVGNumberOf(aElement: TSVGElement; const aName: String;
  aDefault: Double): Double;

begin
  Result := aDefault;
  if aElement.HasAttribute(aName) then
    if not TryStrToSVGNumber(Trim(aElement.Attributes[aName]), Result) then
      Result := aDefault;
end;


// The input of a primitive: the result stored under that name when the
// chain holds one, or the source a name reserved by SVG selects. Any
// other name gives the source graphic, and no name at all gives the
// primitive before this one.
function SVGFilterInputOf(const aName: String; aResults: TStringList;
  aIndex: Integer): Integer;

var
  lAt: Integer;

begin
  if Trim(aName) = '' then
    begin
    if aIndex = 0 then
      Exit(SVGFilterSourceGraphic);
    Exit(aIndex - 1);
    end;
  lAt := aResults.IndexOf(Trim(aName));
  if lAt >= 0 then
    Exit(PtrInt(aResults.Objects[lAt]));
  if SameText(Trim(aName), 'SourceAlpha') then
    Exit(SVGFilterSourceAlpha);
  if SameText(Trim(aName), 'BackgroundImage') then
    Exit(SVGFilterBackgroundImage);
  if SameText(Trim(aName), 'BackgroundAlpha') then
    Exit(SVGFilterBackgroundAlpha);
  if SameText(Trim(aName), 'FillPaint') then
    Exit(SVGFilterFillPaint);
  if SameText(Trim(aName), 'StrokePaint') then
    Exit(SVGFilterStrokePaint);
  Result := SVGFilterSourceGraphic;
end;


// The number SVG gives an edge mode, duplicate when the name is unknown.
function SVGEdgeModeOf(const aName: String): Integer;

begin
  Result := 0;
  if SameText(Trim(aName), 'wrap') then
    Result := 1
  else if SameText(Trim(aName), 'none') then
    Result := 2;
end;


// The number of the channel a name selects, alpha when the name is
// unknown.
function SVGChannelOf(const aName: String): Integer;

begin
  Result := 3;
  if SameText(Trim(aName), 'R') then
    Result := 0
  else if SameText(Trim(aName), 'G') then
    Result := 1
  else if SameText(Trim(aName), 'B') then
    Result := 2;
end;


// The transfer function of one channel of a component transfer, read from
// the feFunc element of that channel.
procedure SVGReadTransfer(aElement: TSVGElement;
  out aTransfer: TSVGFilterTransfer);

const
  Names: array[0..4] of String = ('identity', 'table', 'discrete', 'linear',
    'gamma');

var
  I: Integer;
  lType: String;

begin
  FillChar(aTransfer, SizeOf(aTransfer), 0);
  aTransfer.Slope := 1;
  aTransfer.Amplitude := 1;
  aTransfer.Exponent := 1;
  if aElement = nil then
    Exit;
  lType := Trim(aElement.AttributeDef('type', 'identity'));
  for I := Low(Names) to High(Names) do
    if SameText(lType, Names[I]) then
      aTransfer.Kind := I;
  aTransfer.Table := SVGNumbersOf(aElement, 'tableValues');
  aTransfer.Slope := SVGNumberOf(aElement, 'slope', 1);
  aTransfer.Intercept := SVGNumberOf(aElement, 'intercept', 0);
  aTransfer.Amplitude := SVGNumberOf(aElement, 'amplitude', 1);
  aTransfer.Exponent := SVGNumberOf(aElement, 'exponent', 1);
  aTransfer.Offset := SVGNumberOf(aElement, 'offset', 0);
end;


// The light of a lighting primitive, read from the child element that
// declares it. Numbers three and up hold its kind and its position.
procedure ReadLight(aElement: TSVGElement; aBoxUnits: Boolean;
  const aBounds: TSVGRect; var aPrimitive: TSVGFilterPrimitive);

  // The position of a light. In object bounding box units a coordinate is
  // measured against the box of the element, and a height against its
  // diagonal.
  function Place(aChild: TSVGElement; const aName: String;
    aAxis: TSVGLengthAxis): Double;
  begin
    Result := SVGNumberOf(aChild, aName, 0);
    if not aBoxUnits then
      Exit;
    Result := Result * aBounds.PercentBase(aAxis);
    case aAxis of
      laHorizontal: Result := Result + aBounds.Left;
      laVertical: Result := Result + aBounds.Top;
    end;
  end;

var
  I: Integer;
  lChild: TSVGElement;

begin
  for I := 0 to aElement.ChildCount - 1 do
    begin
    if not (aElement[I] is TSVGElement) then
      Continue;
    lChild := TSVGElement(aElement[I]);
    if lChild.TagName = 'feDistantLight' then
      begin
      aPrimitive.Numbers[3] := 0;
      aPrimitive.Numbers[4] := SVGNumberOf(lChild, 'azimuth', 0);
      aPrimitive.Numbers[5] := SVGNumberOf(lChild, 'elevation', 0);
      Exit;
      end;
    if lChild.TagName = 'fePointLight' then
      begin
      aPrimitive.Numbers[3] := 1;
      aPrimitive.Numbers[4] := Place(lChild, 'x', laHorizontal);
      aPrimitive.Numbers[5] := Place(lChild, 'y', laVertical);
      aPrimitive.Numbers[6] := Place(lChild, 'z', laDiagonal);
      Exit;
      end;
    if lChild.TagName = 'feSpotLight' then
      begin
      aPrimitive.Numbers[3] := 2;
      aPrimitive.Numbers[4] := Place(lChild, 'x', laHorizontal);
      aPrimitive.Numbers[5] := Place(lChild, 'y', laVertical);
      aPrimitive.Numbers[6] := Place(lChild, 'z', laDiagonal);
      aPrimitive.Numbers[7] := Place(lChild, 'pointsAtX', laHorizontal);
      aPrimitive.Numbers[8] := Place(lChild, 'pointsAtY', laVertical);
      aPrimitive.Numbers[9] := Place(lChild, 'pointsAtZ', laDiagonal);
      aPrimitive.Numbers[10] := SVGNumberOf(lChild, 'specularExponent', 1);
      // A negative cone angle marks a spot without a limit: it lights
      // everything in the direction it points.
      if lChild.HasAttribute('limitingConeAngle') then
        aPrimitive.Numbers[11] := SVGNumberOf(lChild,
          'limitingConeAngle', 0)
      else
        aPrimitive.Numbers[11] := -1;
      Exit;
      end;
    end;
end;


// True when a units attribute selects bounding box units.
function IsBoxUnits(aElement: TSVGElement; const aName, aDefault: String): Boolean;

begin
  Result := SameText(Trim(aElement.AttributeDef(aName, aDefault)),
    'objectBoundingBox');
end;


// Copies the attributes that a use element passes to the group it expands
// to: all of them except the ones that place and size the use itself.
procedure SVGCopyUseAttributes(aUse, aGroup: TSVGElement);

var
  I: Integer;
  lName: String;

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


// Rewrites every url(#id) in a value so that it points into aPath.
function SVGRebaseURLs(const aValue, aPath: String): String;

var
  I, lMark: Integer;

begin
  Result := aValue;
  I := 1;
  while I <= Length(Result) - 3 do
    begin
    if not SameText(Copy(Result, I, 4), 'url(') then
      begin
      Inc(I);
      Continue;
      end;
    lMark := I + 4;
    while (lMark <= Length(Result)) and (Result[lMark] in [' ', #9]) do
      Inc(lMark);
    if (lMark <= Length(Result)) and (Result[lMark] in ['''', '"']) then
      Inc(lMark);
    if (lMark <= Length(Result)) and (Result[lMark] = '#') then
      begin
      Insert(aPath, Result, lMark);
      Inc(lMark, Length(aPath));
      end;
    I := lMark;
    end;
end;


// Points the local references of a copied subtree at the file it came from.
procedure SVGRebaseReferences(aElement: TSVGElement; const aPath: String);

const
  { Every attribute whose value may hold a url(#id). }
  ReferenceAttributes: array[0..9] of String = ('fill', 'stroke', 'clip-path',
    'mask', 'filter', 'marker', 'marker-start', 'marker-mid', 'marker-end',
    'style');

var
  I: Integer;
  lOld, lNew: String;

begin
  for I := 0 to High(ReferenceAttributes) do
    begin
    if not aElement.HasAttribute(ReferenceAttributes[I]) then
      Continue;
    lOld := aElement.Attributes[ReferenceAttributes[I]];
    lNew := SVGRebaseURLs(lOld, aPath);
    if lNew <> lOld then
      aElement.Attributes[ReferenceAttributes[I]] := lNew;
    end;
  for I := 0 to aElement.ChildCount - 1 do
    if aElement[I] is TSVGElement then
      SVGRebaseReferences(TSVGElement(aElement[I]), aPath);
end;


// Copies the width and height that a use element imposes on the viewport
// it refers to.
procedure CopyUseSize(aUse, aTarget: TSVGElement);

begin
  if aUse.HasAttribute('width') then
    aTarget.Attributes['width'] := aUse.Attributes['width'];
  if aUse.HasAttribute('height') then
    aTarget.Attributes['height'] := aUse.Attributes['height'];
end;


// Replaces a symbol with the svg element it renders as, and returns it.
function SymbolAsViewport(aParent, aSymbol: TSVGElement): TSVGElement;

var
  I: Integer;
  lSVG: TSVGElement;

begin
  lSVG := CreateSVGElement('svg');
  for I := 0 to aSymbol.AttributeCount - 1 do
    lSVG.Attributes[aSymbol.AttributeNames[I]] :=
      aSymbol.Attributes[aSymbol.AttributeNames[I]];
  for I := 0 to aSymbol.ChildCount - 1 do
    lSVG.AppendChild(aSymbol[I].Clone);
  aParent.ReplaceChild(aSymbol, lSVG);
  Result := lSVG;
end;


{ TSVGFileStyleSheetResolver }

constructor TSVGFileStyleSheetResolver.Create(const aBasePath: String);

begin
  inherited Create;
  FBasePath := aBasePath;
end;


function TSVGFileStyleSheetResolver.ResolveStyleSheet(
  const aHRef, aBaseURI: String): RawByteString;

var
  lName, lBase: String;
  lFile: TFileStream;

begin
  Result := '';
  lName := Trim(aHRef);
  if lName = '' then
    Exit;
  if Copy(lName, 1, 7) = 'file://' then
    lName := Copy(lName, 8, Length(lName));
  if not SVGIsAbsolutePath(lName) then
    begin
    lBase := FBasePath;
    if aBaseURI <> '' then
      lBase := ExtractFilePath(aBaseURI);
    if lBase <> '' then
      lName := IncludeTrailingPathDelimiter(lBase) + lName;
    end;
  if not FileExists(lName) then
    Exit;
  Inc(FReads);
  // The sheet is read as bytes: the line endings and the encoding are the
  // ones the file was written with, and the CSS engine reads both.
  try
    lFile := TFileStream.Create(lName, fmOpenRead or fmShareDenyWrite);
    try
      SetLength(Result, lFile.Size);
      if lFile.Size > 0 then
        lFile.ReadBuffer(Result[1], lFile.Size);
    finally
      lFile.Free;
    end;
  except
    Result := '';
  end;
end;


{ TSVGFileDocumentResolver }

constructor TSVGFileDocumentResolver.Create(const aBasePath: String);

begin
  inherited Create;
  FBasePath := aBasePath;
  FCache := TStringList.Create;
  FCache.Sorted := True;
  FCache.Duplicates := dupIgnore;
  FCache.OwnsObjects := True;
end;


destructor TSVGFileDocumentResolver.Destroy;

begin
  FreeAndNil(FCache);
  inherited Destroy;
end;


{ TSVGFileFontResolver }

constructor TSVGFileFontResolver.Create(const aBasePath: String);

begin
  inherited Create;
  FBasePath := aBasePath;
end;


function TSVGFileFontResolver.ResolveFontFile(const aURL,
  aBaseURI: String): String;

var
  lName, lBase: String;

begin
  Result := '';
  lName := Trim(aURL);
  if lName = '' then
    Exit;
  Inc(FLookups);
  if Copy(lName, 1, 7) = 'file://' then
    lName := Copy(lName, 8, Length(lName));
  if not SVGIsAbsolutePath(lName) then
    begin
    lBase := FBasePath;
    if aBaseURI <> '' then
      lBase := ExtractFilePath(aBaseURI);
    if lBase <> '' then
      lName := IncludeTrailingPathDelimiter(lBase) + lName;
    end;
  lName := ExpandFileName(lName);
  if FileExists(lName) then
    Exit(lName);
  if FSearchPath <> '' then
    begin
    lName := IncludeTrailingPathDelimiter(FSearchPath)
      + ExtractFileName(Trim(aURL));
    if FileExists(lName) then
      Exit(lName);
    end;
  if Assigned(FOnFontFileNeeded) then
    begin
    lName := '';
    FOnFontFileNeeded(Self, aURL, aBaseURI, lName);
    if (lName <> '') and FileExists(lName) then
      Result := lName;
    end;
end;


// The key a document is held under while it is drawn. A reference to a
// document that is already open is recognised by this key.
function SVGDocumentKey(const aBaseURI: String): String;

begin
  Result := Trim(aBaseURI);
  if Result = '' then
    Exit;
  if Copy(Result, 1, 7) = 'file://' then
    Result := Copy(Result, 8, Length(Result));
  Result := ExpandFileName(Result);
end;


function TSVGFileDocumentResolver.ResolveDocument(
  const aPath, aBaseURI: String): TSVGDocument;

var
  lName, lBase: String;
  lIndex: Integer;
  lDocument: TSVGDocument;

begin
  Result := nil;
  lName := Trim(aPath);
  if lName = '' then
    Exit;
  // A data URI contains the document instead of pointing to a file. It goes
  // into the same cache, under the URI it arrived in.
  if SVGIsDataURI(lName) then
    begin
    lIndex := FCache.IndexOf(lName);
    if lIndex >= 0 then
      Exit(TSVGDocument(FCache.Objects[lIndex]));
    lDocument := ReadSVGDataURI(lName);
    FCache.AddObject(lName, lDocument);
    Exit(lDocument);
    end;
  if Copy(lName, 1, 7) = 'file://' then
    lName := Copy(lName, 8, Length(lName));
  if not SVGIsAbsolutePath(lName) then
    begin
    lBase := FBasePath;
    if aBaseURI <> '' then
      lBase := ExtractFilePath(aBaseURI);
    if lBase <> '' then
      lName := IncludeTrailingPathDelimiter(lBase) + lName;
    end;
  lName := ExpandFileName(lName);
  lIndex := FCache.IndexOf(lName);
  if lIndex >= 0 then
    Exit(TSVGDocument(FCache.Objects[lIndex]));
  if not FileExists(lName) then
    Exit;
  Inc(FReads);
  lDocument := nil;
  try
    lDocument := ReadSVGFile(lName);
  except
    FreeAndNil(lDocument);
  end;
  // A file that cannot be read is remembered as unreadable, so it is not
  // tried again for every reference to it.
  FCache.AddObject(lName, lDocument);
  Result := lDocument;
end;


procedure RenderSVGDocument(aDocument: TSVGDocument;
  aBackend: TSVGRenderBackend);

var
  lRenderer: TSVGRenderer;

begin
  lRenderer := TSVGRenderer.Create;
  try
    lRenderer.Render(aDocument, aBackend);
  finally
    lRenderer.Free;
  end;
end;


{ TSVGRenderState }

constructor TSVGRenderState.Create(const aViewport: TSVGRect);

begin
  CTM := TSVGMatrix.Identity;
  Viewport := aViewport;
  Style := TSVGComputedStyle.Initial;
  Alpha := 1;
end;


{ TSVGRenderer }

constructor TSVGRenderer.Create;

begin
  inherited Create;
  FStyles := TSVGStyleResolver.Create;
  FPath := TSVGPath.Create;
  FClipPath := TSVGPath.Create;
  FBoundsPath := TSVGPath.Create;
  FTextPath := TSVGPath.Create;
  FBoundsPoly := TSVGPolyPath.Create;
  FLayout := TSVGTextLayout.Create;
  FOpenDocuments := TStringList.Create;
  FDPI := 96;
  FView := TSVGView.None;
  FLanguage := SVGDefaultLanguage;
  FMaxUseDepth := SVGDefaultUseDepth;
end;


destructor TSVGRenderer.Destroy;

begin
  FreeAndNil(FExpander);
  FreeAndNil(FLayout);
  FreeAndNil(FOpenDocuments);
  FreeAndNil(FBoundsPoly);
  FreeAndNil(FBoundsPath);
  FreeAndNil(FTextPath);
  FreeAndNil(FClipPath);
  FreeAndNil(FPath);
  FreeAndNil(FStyles);
  inherited Destroy;
end;


function TSVGRenderer.ContextOf(const aState: TSVGRenderState): TSVGLengthContext;

var
  lFontSize: Double;

begin
  Result := TSVGLengthContext.Create(aState.Viewport);
  Result.DPI := FDPI;
  lFontSize := aState.Style.FontSize.Resolve(Result.FontSize, Result.FontSize,
    Result.XHeight, FDPI);
  if lFontSize > 0 then
    begin
    Result.FontSize := lFontSize;
    Result.XHeight := lFontSize / 2;
    end;
end;


function TSVGRenderer.TransformOf(aElement: TSVGElement): TSVGMatrix;

begin
  Result := TSVGMatrix.Identity;
  Result.ReadAttribute(aElement);
end;


function TSVGRenderer.LengthOf(aElement: TSVGElement; const aName: String;
  const aContext: TSVGLengthContext; aAxis: TSVGLengthAxis;
  aDefault: Double): Double;

var
  lLength: TSVGLength;

begin
  lLength := TSVGLength.Create(aDefault, luNumber);
  lLength.ReadAttribute(aElement, aName);
  Result := aContext.Resolve(lLength, aAxis);
end;


function TSVGRenderer.ConditionsPass(aElement: TSVGElement): Boolean;

var
  lList: TStringList;
  I: Integer;

begin
  Result := True;
  if not (aElement.HasAttribute('requiredFeatures')
          or aElement.HasAttribute('requiredExtensions')
          or aElement.HasAttribute('systemLanguage')) then
    Exit;
  lList := TStringList.Create;
  try
    if aElement.HasAttribute('requiredExtensions') then
      begin
      SplitConditionList(aElement.Attributes['requiredExtensions'], lList);
      if lList.Count > 0 then
        Exit(False);
      end;
    if aElement.HasAttribute('requiredFeatures') then
      begin
      SplitConditionList(aElement.Attributes['requiredFeatures'], lList);
      if lList.Count = 0 then
        Exit(False);
      for I := 0 to lList.Count - 1 do
        if not FeatureSupported(lList[I]) then
          Exit(False);
      end;
    if aElement.HasAttribute('systemLanguage') then
      begin
      SplitConditionList(aElement.Attributes['systemLanguage'], lList);
      Result := False;
      for I := 0 to lList.Count - 1 do
        if LanguageMatches(lList[I], FLanguage) then
          Exit(True);
      end;
  finally
    lList.Free;
  end;
end;


function TSVGRenderer.LayerNeeded(const aStyle: TSVGComputedStyle): Boolean;

begin
  Result := (aStyle.Opacity < 1)
        and (bcGroupOpacity in FBackend.Capabilities);
end;


function TSVGRenderer.RootFillsItsViewport(
  aDocument: TSVGDocument): Boolean;

var
  lLength: TSVGLength;

  function IsRelative(const aName: String): Boolean;
  begin
    Result := True;
    if not aDocument.Root.HasAttribute(aName) then
      Exit;
    lLength := TSVGLength.Zero;
    lLength.ParseDef(aDocument.Root.Attributes[aName]);
    Result := lLength.LengthUnit = luPercent;
  end;

begin
  Result := (aDocument <> nil) and (aDocument.Root is TSVGSVGElement)
        and IsRelative('width') and IsRelative('height');
end;


function TSVGRenderer.DocumentSize(aDocument: TSVGDocument;
  out aWidth, aHeight: Integer): Boolean;

var
  lRoot: TSVGElement;
  lBox: TSVGRect;
  lContext: TSVGLengthContext;
  lWidth, lHeight: Double;

begin
  aWidth := 0;
  aHeight := 0;
  Result := (aDocument <> nil) and (aDocument.Root is TSVGSVGElement);
  if not Result then
    Exit;
  lRoot := aDocument.Root;
  lWidth := SVGDefaultDocumentWidth;
  lHeight := SVGDefaultDocumentHeight;
  lBox := TSVGRect.Empty;
  if lBox.ReadViewBoxAttribute(lRoot) and not lBox.IsEmpty then
    begin
    lWidth := lBox.Width;
    lHeight := lBox.Height;
    end;
  // A percentage on the outermost element measures against the fallback
  // size.
  lContext := TSVGLengthContext.Create(
    TSVGRect.CreateSize(0, 0, lWidth, lHeight));
  lContext.DPI := FDPI;
  lWidth := LengthOf(lRoot, 'width', lContext, laHorizontal, lWidth);
  lHeight := LengthOf(lRoot, 'height', lContext, laVertical, lHeight);
  Result := (lWidth > 0) and (lHeight > 0);
  if not Result then
    Exit;
  aWidth := Max(1, Ceil(lWidth));
  aHeight := Max(1, Ceil(lHeight));
end;


procedure TSVGRenderer.Render(aDocument: TSVGDocument;
  aBackend: TSVGRenderBackend);

var
  lWidth, lHeight: Integer;

begin
  if not DocumentSize(aDocument, lWidth, lHeight) then
    Exit;
  RenderToSize(aDocument, aBackend, lWidth, lHeight);
end;


procedure TSVGRenderer.RenderToSize(aDocument: TSVGDocument;
  aBackend: TSVGRenderBackend; aWidth, aHeight: Integer);

var
  lWidth, lHeight: Integer;
  lCTM: TSVGMatrix;

begin
  if aBackend = nil then
    raise ESVGRender.Create(SErrNoBackendToRenderWith);
  if (aWidth <= 0) or (aHeight <= 0) then
    raise ESVGRender.CreateFmt(SErrFrameHasNoExtent,
      [aWidth, aHeight]);
  lCTM := TSVGMatrix.Identity;
  if RootFillsItsViewport(aDocument) then
    begin
    // A root sized in percentages uses the frame as its viewport. Its own
    // viewBox and preserveAspectRatio then place the drawing inside it.
    lWidth := aWidth;
    lHeight := aHeight;
    end
  else if not DocumentSize(aDocument, lWidth, lHeight) then
    begin
    lWidth := aWidth;
    lHeight := aHeight;
    end
  else if (lWidth <> aWidth) or (lHeight <> aHeight) then
    lCTM := TSVGMatrix.Scaling(aWidth / lWidth, aHeight / lHeight);
  aBackend.BeginFrame(aWidth, aHeight);
  try
    RenderInFrame(aDocument, aBackend, lCTM,
      TSVGRect.CreateSize(0, 0, lWidth, lHeight));
  finally
    aBackend.EndFrame;
  end;
end;


procedure TSVGRenderer.RenderToFit(aDocument: TSVGDocument;
  aBackend: TSVGRenderBackend; aWidth, aHeight: Integer);

var
  lWidth, lHeight: Integer;
  lScale: Double;
  lCTM: TSVGMatrix;

begin
  if aBackend = nil then
    raise ESVGRender.Create(SErrNoBackendToRenderWith);
  if (aWidth <= 0) or (aHeight <= 0) then
    raise ESVGRender.CreateFmt(SErrFrameHasNoExtent,
      [aWidth, aHeight]);
  // A root sized in percentages already places itself in the frame, using
  // its own preserveAspectRatio.
  if RootFillsItsViewport(aDocument)
     or not DocumentSize(aDocument, lWidth, lHeight) then
    begin
    RenderToSize(aDocument, aBackend, aWidth, aHeight);
    Exit;
    end;
  lScale := Min(aWidth / lWidth, aHeight / lHeight);
  lCTM := TSVGMatrix.Scaling(lScale, lScale).Compose(TSVGMatrix.Translation(
    (aWidth - lWidth * lScale) / 2, (aHeight - lHeight * lScale) / 2));
  aBackend.BeginFrame(aWidth, aHeight);
  try
    RenderInFrame(aDocument, aBackend, lCTM,
      TSVGRect.CreateSize(0, 0, lWidth, lHeight));
  finally
    aBackend.EndFrame;
  end;
end;


procedure TSVGRenderer.RenderInFrame(aDocument: TSVGDocument;
  aBackend: TSVGRenderBackend; const aCTM: TSVGMatrix;
  const aViewport: TSVGRect);

var
  lState: TSVGRenderState;

begin
  if aBackend = nil then
    raise ESVGRender.Create(SErrNoBackendToRenderWith);
  FElementCount := 0;
  if (aDocument = nil) or (aDocument.Root = nil) then
    Exit;
  FBackend := aBackend;
  FDocument := aDocument;
  FBaseURI := aDocument.BaseURI;
  FOpenDocuments.Clear;
  FOpenDocuments.Add(SVGDocumentKey(aDocument.BaseURI));
  FExpander := TSVGUseExpander.Create(aDocument);
  FStyles.LoadDocument(aDocument);
  LoadDocumentFonts(aDocument);
  try
    lState := TSVGRenderState.Create(aViewport);
    lState.CTM := aCTM;
    FStyles.LengthContext := ContextOf(lState);
    RenderElement(aDocument.Root, lState);
  finally
    FStyles.Unload;
    FreeAndNil(FExpander);
    FDocumentFonts := nil;
    FBackend := nil;
    FDocument := nil;
  end;
end;


procedure TSVGRenderer.LoadDocumentFonts(aDocument: TSVGDocument);

var
  I, J: Integer;
  lRule: TSVGFontFaceRule;
  lFile: String;
  lFonts: TSVGDocumentFontProvider;
  lHeld: ISVGFontProvider;

begin
  FFontsLoaded := 0;
  lFonts := TSVGDocumentFontProvider.Create(FFonts);
  lHeld := lFonts;
  lFonts.AddDocument(aDocument);
  LoadNamedFonts(aDocument.Root, lFonts);
  if lFonts.FaceCount > 0 then
    FDocumentFonts := lHeld
  else
    FDocumentFonts := nil;
  if (FFonts = nil) or (FFontFiles = nil) then
    Exit;
  for I := 0 to FStyles.FontFaceCount - 1 do
    begin
    lRule := FStyles.FontFaces[I];
    // The sources of a rule are alternatives. The first one that gives a
    // readable face is used.
    for J := 0 to High(lRule.Sources) do
      begin
      lFile := FFontFiles.ResolveFontFile(lRule.Sources[J],
        aDocument.BaseURI);
      if lFile = '' then
        Continue;
      if FFonts.AddFontResource(lRule.Family, lRule.Weight, lRule.Style,
                                lFile) then
        begin
        Inc(FFontsLoaded);
        Break;
        end;
      end;
    end;
end;


// Reads the faces that font-face elements load from other files. A face
// is registered under the family the document gives it, whatever family
// the file itself declares.
procedure TSVGRenderer.LoadNamedFonts(aElement: TSVGElement;
  aProvider: TSVGDocumentFontProvider);

var
  I, J: Integer;
  lSource: TSVGDocument;
  lFont, lChild, lURI: TSVGElement;
  lFamily, lHRef, lPath: String;

begin
  if aElement = nil then
    Exit;
  if (aElement.TagName = 'font-face') and (FDocuments <> nil)
     and not ((aElement.Parent is TSVGElement)
              and (TSVGElement(aElement.Parent).TagName = 'font')) then
    begin
    lFamily := Trim(aElement.AttributeDef('font-family', ''));
    lURI := nil;
    for I := 0 to aElement.ChildCount - 1 do
      if (aElement[I] is TSVGElement)
         and (TSVGElement(aElement[I]).TagName = 'font-face-src') then
        begin
        lChild := TSVGElement(aElement[I]);
        for J := 0 to lChild.ChildCount - 1 do
          if (lChild[J] is TSVGElement)
             and (TSVGElement(lChild[J]).TagName = 'font-face-uri') then
            begin
            lURI := TSVGElement(lChild[J]);
            Break;
            end;
        Break;
        end;
    if (lFamily <> '') and (lURI <> nil) then
      begin
      lHRef := SVGHRefOf(lURI);
      lPath := SVGReferencePath(lHRef);
      if lPath <> '' then
        begin
        lSource := ExternalDocumentOf(lURI, lPath);
        if lSource <> nil then
          begin
          lFont := lSource.ElementByID(SVGReferenceFragment(lHRef));
          if (lFont <> nil) and (lFont.TagName = 'font') then
            aProvider.AddFontAs(lFamily,
              aElement.AttributeDef('unicode-range', ''), lFont);
          end;
        end;
      end;
    Exit;
    end;
  for I := 0 to aElement.ChildCount - 1 do
    if aElement[I] is TSVGElement then
      LoadNamedFonts(TSVGElement(aElement[I]), aProvider);
end;


// The faces text is drawn from: the ones the document declares, or the
// ones the program supplied when the document declares none.
function TSVGRenderer.TextFonts: ISVGFontProvider;

begin
  if FDocumentFonts <> nil then
    Result := FDocumentFonts
  else
    Result := FFonts;
end;


procedure TSVGRenderer.RenderElement(aElement: TSVGElement;
  const aParent: TSVGRenderState);

var
  lState: TSVGRenderState;
  lClip, lMask: TSVGElement;
  lMixed: Boolean;

begin
  if not SVGIsRenderedElement(aElement) then
    Exit;
  if not ConditionsPass(aElement) then
    Exit;
  if aElement is TSVGUseElement then
    begin
    RenderUse(aElement, aParent);
    Exit;
    end;
  lState := aParent;
  lState.Style := FStyles.ComputeStyle(aElement, aParent.Style);
  if not lState.Style.IsDisplayed then
    Exit;
  Inc(FElementCount);
  lState.CTM := TransformOf(aElement).Compose(aParent.CTM);
  lClip := ReferencedElement(aElement, lState.Style.ClipPath,
    TSVGClipPathElement);
  lMask := ReferencedElement(aElement, lState.Style.Mask, TSVGMaskElement);
  // The colour space the drawing of this element is composited in. It is
  // put back for whatever follows.
  lMixed := lState.Style.ColorInterpolation
        <> aParent.Style.ColorInterpolation;
  if lMixed then
    FBackend.SetColorInterpolation(lState.Style.ColorInterpolation);
  try
    if (lClip = nil) and (lMask = nil) then
      RenderFiltered(aElement, lState)
    else
      RenderShielded(aElement, lClip, lMask, lState);
  finally
    if lMixed then
      FBackend.SetColorInterpolation(aParent.Style.ColorInterpolation);
  end;
end;


procedure TSVGRenderer.RenderSelf(aElement: TSVGElement;
  const aState: TSVGRenderState);

begin
  if aElement is TSVGSVGElement then
    RenderViewport(aElement, aState)
  else if aElement is TSVGImageElement then
    RenderImage(aElement, aState)
  else if aElement is TSVGTextElement then
    RenderText(aElement, aState)
  else if IsSVGShapeElement(aElement) then
    RenderShape(aElement, aState)
  else if aElement.IsContainer then
    RenderContainer(aElement, aState, aElement is TSVGSwitchElement);
end;


procedure TSVGRenderer.RenderChildren(aElement: TSVGElement;
  const aState: TSVGRenderState; aFirstMatch: Boolean);

var
  I: Integer;
  lChild: TSVGElement;

begin
  for I := 0 to aElement.ChildCount - 1 do
    begin
    if not (aElement[I] is TSVGElement) then
      Continue;
    lChild := TSVGElement(aElement[I]);
    if not aFirstMatch then
      begin
      RenderElement(lChild, aState);
      Continue;
      end;
    if not SVGIsRenderedElement(lChild) or not ConditionsPass(lChild) then
      Continue;
    RenderElement(lChild, aState);
    Exit;
    end;
end;


procedure TSVGRenderer.RenderContainer(aElement: TSVGElement;
  const aState: TSVGRenderState; aFirstMatch: Boolean);

var
  lState: TSVGRenderState;
  lLayer: Boolean;

begin
  lState := aState;
  lLayer := LayerNeeded(aState.Style);
  if lLayer then
    FBackend.PushLayer(TSVGRect.Empty, aState.Style.Opacity, True)
  else
    lState.Alpha := aState.Alpha * aState.Style.Opacity;
  try
    RenderChildren(aElement, lState, aFirstMatch);
  finally
    if lLayer then
      FBackend.PopLayer;
  end;
end;


procedure TSVGRenderer.RenderViewport(aElement: TSVGElement;
  const aState: TSVGRenderState);

var
  lState: TSVGRenderState;
  lContext: TSVGLengthContext;
  lSaved: TSVGLengthContext;
  lBox, lPort, lClip: TSVGRect;
  lRatio: TSVGPreserveAspectRatio;
  lX, lY, lWidth, lHeight: Double;
  lClipped, lViewed: Boolean;

begin
  lContext := ContextOf(aState);
  // x and y place a viewport within the one above it, and SVG says they
  // have no effect on the outermost svg, which has none above it. A
  // document drawn for an image is given the x and y of the image and is
  // not the outermost svg of the drawing.
  lX := 0;
  lY := 0;
  if (FDocument = nil) or (aElement <> FDocument.Root) then
    begin
    lX := LengthOf(aElement, 'x', lContext, laHorizontal, 0);
    lY := LengthOf(aElement, 'y', lContext, laVertical, 0);
    end;
  lWidth := LengthOf(aElement, 'width', lContext, laHorizontal,
    aState.Viewport.Width);
  lHeight := LengthOf(aElement, 'height', lContext, laVertical,
    aState.Viewport.Height);
  if (lWidth <= 0) or (lHeight <= 0) then
    Exit;
  lPort := TSVGRect.CreateSize(lX, lY, lWidth, lHeight);
  lState := aState;
  lBox := TSVGRect.Empty;
  // A view reframes the root and nothing else. What it leaves out is
  // read from the element as usual.
  lViewed := (FDocument <> nil) and (aElement = FDocument.Root);
  if lViewed and FView.HasViewBox then
    lBox := FView.ViewBox
  else if not lBox.ReadViewBoxAttribute(aElement) then
    lBox := TSVGRect.Empty;
  if not lBox.IsEmpty then
    begin
    lRatio := TSVGPreserveAspectRatio.Default;
    if lViewed and FView.HasRatio then
      lRatio := FView.Ratio
    else
      lRatio.ReadAttribute(aElement);
    lState.CTM := lRatio.ViewBoxTransform(lBox, lPort).Compose(aState.CTM);
    lState.Viewport := lBox;
    end
  else
    begin
    lState.CTM := TSVGMatrix.Translation(lX, lY).Compose(aState.CTM);
    lState.Viewport := TSVGRect.CreateSize(0, 0, lWidth, lHeight);
    end;
  // The transform of a view moves what the view frames, so it goes under
  // the viewBox rather than over it.
  if lViewed and FView.HasTransform then
    lState.CTM := FView.Transform.Compose(lState.CTM);
  lClipped := (aState.Style.Overflow = ovHidden)
          and (bcClipPath in FBackend.Capabilities);
  if lClipped then
    begin
    lClip := aState.Style.Clip.Narrow(lPort, lContext);
    FClipPath.Clear;
    FClipPath.AddRect(lClip.Left, lClip.Top, lClip.Width, lClip.Height, 0, 0);
    FBackend.PushClip(FClipPath, aState.CTM, frNonZero);
    end;
  lSaved := FStyles.LengthContext;
  FStyles.LengthContext := ContextOf(lState);
  try
    RenderContainer(aElement, lState, False);
  finally
    FStyles.LengthContext := lSaved;
    if lClipped then
      FBackend.PopClip;
  end;
end;


procedure TSVGRenderer.RenderShape(aElement: TSVGElement;
  const aState: TSVGRenderState);

var
  lFill, lStroke, lLayer: Boolean;
  lAlpha, lReach: Double;
  lPaint: TSVGPaint;
  lBox: TSVGRect;

begin
  if aState.Style.Visibility <> svVisible then
    Exit;
  if not BuildSVGShapePath(aElement, FPath, ContextOf(aState)) then
    Exit;
  if FPath.IsEmpty then
    Exit;
  lFill := aState.Style.Fill.Kind <> spNone;
  lStroke := (aState.Style.Stroke.Kind <> spNone)
         and (aState.Style.Pen.Width > 0);
  if not (lFill or lStroke) then
    Exit;
  // A shape that paints twice needs a layer. With one paint, the opacity
  // folds into it.
  lAlpha := aState.Alpha;
  lLayer := lFill and lStroke and LayerNeeded(aState.Style);
  if lLayer then
    begin
    // A miter reaches the limit times half the width past the vertex,
    // which is further than any cap or other join.
    lReach := aState.Style.Pen.MiterLimit;
    if lReach < 1 then
      lReach := 1;
    lReach := lReach * aState.Style.Pen.Width / 2;
    lBox := PathBounds(FPath);
    lBox := TSVGRect.Create(lBox.Left - lReach, lBox.Top - lReach,
      lBox.Right + lReach, lBox.Bottom + lReach);
    FBackend.PushLayer(lBox.Transform(aState.CTM),
      aState.Style.Opacity, True);
    end
  else
    lAlpha := lAlpha * aState.Style.Opacity;
  try
    if lFill then
      begin
      lPaint := aState.Style.Fill;
      if IsPatternPaint(lPaint)
         and PaintWithPattern(FPath, lPaint, aState, PathBounds(FPath),
               aState.Style.FillRule, aState.Style.Pen, False,
               aState.Style.FillOpacity * lAlpha) then
        begin
        // Every tile of the pattern is a shape of its own and has built
        // its path in this one. The shape being drawn needs that path
        // back for its stroke and its markers.
        BuildSVGShapePath(aElement, FPath, ContextOf(aState));
        lPaint := TSVGPaint.None;
        end
      else if IsPatternPaint(lPaint) then
        // An empty pattern paints nothing, so its fallback is used
        // instead.
        lPaint := lPaint.Resolved;
      if lPaint.Kind <> spNone then
        FBackend.FillPath(FPath, aState.CTM, lPaint,
          aState.Style.FillRule, aState.Style.FillOpacity * lAlpha);
      end;
    if lStroke then
      begin
      // A gradient strokes directly. A pattern is drawn from its
      // children, so the line the pen draws becomes the shape it is
      // clipped to.
      lPaint := aState.Style.Stroke;
      if IsPatternPaint(lPaint) then
        begin
        if PaintWithPattern(FPath, lPaint, aState, PathBounds(FPath),
             frNonZero, aState.Style.Pen, True,
             aState.Style.StrokeOpacity * lAlpha) then
          begin
          BuildSVGShapePath(aElement, FPath, ContextOf(aState));
          lPaint := TSVGPaint.None;
          end
        else
          lPaint := lPaint.Resolved;
        end;
      if (lPaint.Kind <> spNone) and not IsPatternPaint(lPaint) then
        FBackend.StrokePath(FPath, aState.CTM, lPaint,
          aState.Style.Pen, aState.Style.StrokeOpacity * lAlpha);
      end;
  finally
    if lLayer then
      FBackend.PopLayer;
  end;
  RenderMarkers(aElement, aState);
end;


function TSVGRenderer.ReferencedElement(aFrom: TSVGElement;
  const aReference: String; aClass: TSVGElementClass): TSVGElement;

var
  lPath: String;
  lSource: TSVGDocument;

begin
  Result := nil;
  if (aReference = '') or (FDocument = nil) then
    Exit;
  lPath := SVGReferencePath(aReference);
  if lPath = '' then
    Result := FDocument.ElementByID(SVGReferenceToID(aReference))
  else
    begin
    lSource := ExternalDocumentOf(aFrom, lPath);
    if lSource <> nil then
      Result := lSource.ElementByID(SVGReferenceFragment(aReference));
    end;
  if not (Result is aClass) then
    Result := nil;
end;


// Whether a child of a clip path contributes a silhouette. The silhouette
// is the bare geometry, so the paint of the shape does not matter: a fill
// of none, an opacity of zero and a stroke width of eighty all leave it
// unchanged. Whether the child is drawn at all does matter: a child that
// is not drawn contributes nothing.
function SVGDrawsSilhouette(const aStyle: TSVGComputedStyle): Boolean;

begin
  Result := aStyle.IsDisplayed and (aStyle.Visibility = svVisible);
end;


function TSVGRenderer.SingleClipShape(aClip: TSVGElement): TSVGElement;

var
  I: Integer;
  lChild: TSVGElement;

begin
  Result := nil;
  // A clip path with its own clip is cut by that clip after its children
  // have joined. A single path cannot express that either.
  if aClip.HasAttribute('clip-path') then
    Exit;
  for I := 0 to aClip.ChildCount - 1 do
    begin
    if not (aClip[I] is TSVGElement) then
      Continue;
    lChild := TSVGElement(aClip[I]);
    // A child with its own clip is cut before its silhouette joins the
    // others. A single path cannot express that.
    if not IsSVGShapeElement(lChild)
       or lChild.HasAttribute('clip-path') then
      Exit(nil);
    // A shape that is not drawn contributes no silhouette, which leaves
    // the clip path empty. One path cannot express that, so the
    // silhouette is built step by step.
    if not SVGDrawsSilhouette(FStyles.ComputeStyleOf(lChild)) then
      Exit(nil);
    if Result <> nil then
      Exit(nil);
    Result := lChild;
    end;
end;


function TSVGRenderer.UnitsTransform(aElement: TSVGElement;
  const aName: String; const aBounds: TSVGRect): TSVGMatrix;

begin
  if IsBoxUnits(aElement, aName, '') then
    Result := aBounds.UnitSquareTransform
  else
    Result := TSVGMatrix.Identity;
end;


function TSVGRenderer.PathBounds(aPath: TSVGPath): TSVGRect;

begin
  // Flattening measures the curve itself. Its control points lie outside
  // it.
  FBoundsPoly.Flatten(aPath, TSVGMatrix.Identity, SVGDefaultFlatness);
  Result := FBoundsPoly.Bounds;
end;


function TSVGRenderer.BoundsOf(aElement: TSVGElement;
  const aState: TSVGRenderState): TSVGRect;

var
  I: Integer;
  lChild: TSVGElement;
  lBounds: TSVGRect;
  lState: TSVGRenderState;
  lGroup: TSVGElement;
  lPort: TSVGRect;
  lRatio: TSVGPreserveAspectRatio;
  lSource: ISVGImageSource;
  lWhole: TSVGRect;

begin
  Result := TSVGRect.Empty;
  if not SVGIsRenderedElement(aElement) then
    Exit;
  lState := aState;
  lState.Style := FStyles.ComputeStyle(aElement, aState.Style);
  if not lState.Style.IsDisplayed then
    Exit;
  if IsSVGShapeElement(aElement) then
    begin
    if BuildSVGShapePath(aElement, FBoundsPath, ContextOf(lState)) then
      Result := PathBounds(FBoundsPath);
    Exit;
    end;
  // The box of an image is the area it draws. That is the whole viewport
  // when the two have the same shape, and less of it when
  // preserveAspectRatio keeps the drawing inside the viewport.
  if aElement is TSVGImageElement then
    begin
    lPort := TSVGRect.CreateSize(
      LengthOf(aElement, 'x', ContextOf(lState), laHorizontal, 0),
      LengthOf(aElement, 'y', ContextOf(lState), laVertical, 0),
      LengthOf(aElement, 'width', ContextOf(lState), laHorizontal, 0),
      LengthOf(aElement, 'height', ContextOf(lState), laVertical, 0));
    Result := lPort;
    if (FImages = nil) or lPort.IsEmpty then
      Exit;
    lSource := FImages.ResolveImage(SVGHRefOf(aElement),
      SVGElementBase(aElement, FBaseURI));
    if (lSource = nil) or (lSource.GetWidth <= 0)
       or (lSource.GetHeight <= 0) then
      Exit;
    lWhole := TSVGRect.CreateSize(0, 0, lSource.GetWidth, lSource.GetHeight);
    lRatio := TSVGPreserveAspectRatio.Default;
    lRatio.ReadAttribute(aElement);
    // A drawing scaled to cover the viewport runs past it and is cut
    // back to it, so the box is the viewport either way.
    Result := lWhole.Transform(lRatio.ViewBoxTransform(lWhole, lPort))
      .Intersect(lPort);
    Exit;
    end;
  if aElement is TSVGTextElement then
    begin
    if (FFonts <> nil) and FLayout.Layout(aElement, FStyles, FFonts,
         lState.Style, ContextOf(lState)) then
      Result := FLayout.Bounds;
    Exit;
    end;
  if aElement is TSVGUseElement then
    begin
    FExpander.MaxDepth := FMaxUseDepth;
    if FExpander.Expand(aElement, lGroup) <> urOK then
      Exit;
    try
      lGroup.AdoptInto(FDocument);
      Result := BoundsOf(lGroup, aState);
    finally
      lGroup.Free;
    end;
    Exit;
    end;
  if not aElement.IsContainer then
    Exit;
  for I := 0 to aElement.ChildCount - 1 do
    begin
    if not (aElement[I] is TSVGElement) then
      Continue;
    lChild := TSVGElement(aElement[I]);
    lBounds := BoundsOf(lChild, lState);
    if lBounds.IsEmpty then
      Continue;
    lBounds := lBounds.Transform(TransformOf(lChild));
    if Result.IsEmpty then
      Result := lBounds
    else
      Result := Result.Union(lBounds);
    end;
end;


procedure TSVGRenderer.FillClipShapes(aElement: TSVGElement;
  const aState: TSVGRenderState);

var
  I: Integer;
  lChild, lGroup, lSource, lNested: TSVGElement;
  lChildState: TSVGRenderState;
  lBounds: TSVGRect;
  lIsUse, lIsText: Boolean;

begin
  for I := 0 to aElement.ChildCount - 1 do
    begin
    if not (aElement[I] is TSVGElement) then
      Continue;
    lChild := TSVGElement(aElement[I]);
    if not ConditionsPass(lChild) then
      Continue;
    lIsUse := lChild is TSVGUseElement;
    lIsText := lChild is TSVGTextElement;
    lGroup := nil;
    try
      if lIsUse then
        begin
        // A use contributes the silhouette of the content it refers to.
        // The group it expands to takes the offset and the attributes of
        // the use.
        FExpander.MaxDepth := FMaxUseDepth;
        if FExpander.Expand(lChild, lGroup) <> urOK then
          Continue;
        lGroup.AdoptInto(FDocument);
        lSource := lGroup;
        end
      else
        begin
        if not (lIsText or IsSVGShapeElement(lChild)) then
          Continue;
        lSource := lChild;
        end;
      lChildState := aState;
      lChildState.Style := FStyles.ComputeStyle(lSource, aState.Style);
      if not SVGDrawsSilhouette(lChildState.Style) then
        Continue;
      lChildState.CTM := TransformOf(lSource).Compose(aState.CTM);
      lBounds := TSVGRect.Empty;
      if not lIsUse then
        begin
        // The glyphs of a text become one path, which then joins the
        // silhouette the way the outline of a shape does.
        if lIsText then
          begin
          if not BuildTextPath(lChild, lChildState, FClipPath) then
            Continue;
          end
        else if not BuildSVGShapePath(lChild, FClipPath,
                                      ContextOf(lChildState)) then
          Continue;
        lBounds := PathBounds(FClipPath);
        end;
      lNested := ReferencedElement(lChild, lChildState.Style.ClipPath,
        TSVGClipPathElement);
      // A child of a clip path may have its own clip. That cuts its
      // silhouette before it joins the others.
      if lNested <> nil then
        FBackend.PushLayer(TSVGRect.Empty, 1, True);
      try
        if lIsUse then
          FillClipShapes(lSource, lChildState)
        else
          FBackend.FillPath(FClipPath, lChildState.CTM,
            TSVGPaint.CreateColor(TSVGColor.FromBytes(255, 255, 255, 255)),
            lChildState.Style.ClipRule, 1);
        if lNested <> nil then
          begin
          if lIsUse then
            lBounds := BoundsOf(lSource, aState);
          RenderClipAsMask(lNested, lChildState, lBounds);
          end;
      finally
        if lNested <> nil then
          FBackend.PopLayer;
      end;
    finally
      lGroup.Free;
    end;
    end;
end;


// True when SVG puts markers on this shape. That is the shapes drawn from
// a list of points, and not the closed primitives.
function TakesMarkers(aElement: TSVGElement): Boolean;

begin
  Result := (aElement is TSVGPathElement)
         or (aElement is TSVGLineElement)
         or (aElement is TSVGPolylineElement)
         or (aElement is TSVGPolygonElement);
end;


procedure TSVGRenderer.RenderMarkers(aElement: TSVGElement;
  const aState: TSVGRenderState);

var
  lVertices: TSVGPathVertexArray;
  I: Integer;
  lReference: String;
  lMarker: TSVGElement;

begin
  if (aState.Style.MarkerStart = '') and (aState.Style.MarkerMid = '')
     and (aState.Style.MarkerEnd = '') then
    Exit;
  if not TakesMarkers(aElement) then
    Exit;
  if FMarkerDepth >= SVGMaxMarkerDepth then
    Exit;
  // The vertices are read before anything else is drawn: the shapes of a
  // marker build into the same path they come from.
  lVertices := SVGPathVertices(FPath);
  Inc(FMarkerDepth);
  try
    for I := 0 to High(lVertices) do
      begin
      case lVertices[I].Kind of
        vkStart: lReference := aState.Style.MarkerStart;
        vkEnd: lReference := aState.Style.MarkerEnd;
      else
        lReference := aState.Style.MarkerMid;
      end;
      lMarker := ReferencedElement(aElement, lReference,
        TSVGMarkerElement);
      if lMarker <> nil then
        RenderMarkerAt(lMarker, lVertices[I], aState);
      end;
  finally
    Dec(FMarkerDepth);
  end;
end;


procedure TSVGRenderer.RenderMarkerAt(aMarker: TSVGElement;
  const aVertex: TSVGPathVertex; const aState: TSVGRenderState);

var
  lState: TSVGRenderState;
  lContext, lSaved: TSVGLengthContext;
  lWidth, lHeight, lRefX, lRefY, lScale, lAngle: Double;
  lBox, lClip: TSVGRect;
  lRatio: TSVGPreserveAspectRatio;
  lViewBox, lPlace: TSVGMatrix;
  lReference: TSVGPoint;
  lOrient: String;
  lClipped: Boolean;

begin
  lContext := ContextOf(aState);
  lWidth := LengthOf(aMarker, 'markerWidth', lContext, laHorizontal, 3);
  lHeight := LengthOf(aMarker, 'markerHeight', lContext, laVertical, 3);
  if (lWidth <= 0) or (lHeight <= 0) then
    Exit;
  lRefX := LengthOf(aMarker, 'refX', lContext, laHorizontal, 0);
  lRefY := LengthOf(aMarker, 'refY', lContext, laVertical, 0);
  // markerUnits says which units the marker is drawn in: those of the
  // stroke it decorates, or those of the user space around it.
  lScale := 1;
  if not SameText(Trim(aMarker.AttributeDef('markerUnits', 'strokeWidth')),
                  'userSpaceOnUse') then
    lScale := aState.Style.Pen.Width;
  if lScale <= 0 then
    Exit;
  lOrient := Trim(aMarker.AttributeDef('orient', '0'));
  if SameText(lOrient, 'auto') then
    lAngle := aVertex.AutoAngle
  else if not TryStrToSVGNumber(lOrient, lAngle) then
    lAngle := 0;
  lState := aState;
  lBox := TSVGRect.Empty;
  if lBox.ReadViewBoxAttribute(aMarker) and not lBox.IsEmpty then
    begin
    lRatio := TSVGPreserveAspectRatio.Default;
    lRatio.ReadAttribute(aMarker);
    lViewBox := lRatio.ViewBoxTransform(lBox,
      TSVGRect.CreateSize(0, 0, lWidth, lHeight));
    lState.Viewport := lBox;
    end
  else
    begin
    lViewBox := TSVGMatrix.Identity;
    lState.Viewport := TSVGRect.CreateSize(0, 0, lWidth, lHeight);
    end;
  // refX and refY give a point in the marker's own space. That point is
  // placed on the vertex, however the marker is rotated or scaled.
  lReference := lViewBox.Transform(TSVGPoint.Create(lRefX, lRefY));
  lPlace := TSVGMatrix.Translation(-lReference.X, -lReference.Y)
    .Compose(TSVGMatrix.Scaling(lScale, lScale))
    .Compose(TSVGMatrix.Rotation(lAngle))
    .Compose(TSVGMatrix.Translation(aVertex.Point.X, aVertex.Point.Y))
    .Compose(aState.CTM);
  lState.CTM := lViewBox.Compose(lPlace);
  // A marker takes the style of its own element, not that of the shape it
  // decorates: a marker on a red line is only red when it says so.
  lState.Style := FStyles.ComputeStyleOf(aMarker);
  if not lState.Style.IsDisplayed then
    Exit;
  lClipped := (lState.Style.Overflow = ovHidden)
          and (bcClipPath in FBackend.Capabilities);
  if lClipped then
    begin
    lClip := lState.Style.Clip.Narrow(
      TSVGRect.CreateSize(0, 0, lWidth, lHeight), lContext);
    FClipPath.Clear;
    FClipPath.AddRect(lClip.Left, lClip.Top, lClip.Width, lClip.Height, 0, 0);
    FBackend.PushClip(FClipPath, lPlace, frNonZero);
    end;
  lSaved := FStyles.LengthContext;
  FStyles.LengthContext := ContextOf(lState);
  try
    RenderContainer(aMarker, lState, False);
  finally
    FStyles.LengthContext := lSaved;
    if lClipped then
      FBackend.PopClip;
  end;
end;


procedure TSVGRenderer.RenderClipAsMask(aClip: TSVGElement;
  const aState: TSVGRenderState; const aBounds: TSVGRect);

var
  lState: TSVGRenderState;
  lOwn: TSVGElement;

begin
  if FClipDepth >= SVGMaxClipDepth then
    Exit;
  Inc(FClipDepth);
  FBackend.PushLayer(TSVGRect.Empty, 1, True);
  try
    lState := aState;
    lState.CTM := UnitsTransform(aClip, 'clipPathUnits', aBounds)
      .Compose(aState.CTM);
    lState.Style := FStyles.ComputeStyle(aClip, aState.Style);
    FillClipShapes(aClip, lState);
    // A clip path may have its own clip path. It then clips to the
    // overlap of the two, so the second one cuts the silhouette of the
    // first before that silhouette becomes the mask.
    lOwn := ReferencedElement(aClip, lState.Style.ClipPath,
      TSVGClipPathElement);
    if (lOwn <> nil) and (lOwn <> aClip) then
      RenderClipAsMask(lOwn, aState, aBounds);
  finally
    FBackend.PopLayerAsMask(mmAlpha);
    Dec(FClipDepth);
  end;
end;


procedure TSVGRenderer.RenderMaskContent(aMask: TSVGElement;
  const aState: TSVGRenderState; const aBounds: TSVGRect);

var
  lState: TSVGRenderState;
  lRegion: TSVGRect;
  lContext: TSVGLengthContext;
  lClipped, lMixed: Boolean;

begin
  FBackend.PushLayer(TSVGRect.Empty, 1, True);
  try
    lState := aState;
    lState.Style := FStyles.ComputeStyle(aMask, aState.Style);
    lContext := ContextOf(aState);
    // The region defaults to a border of one tenth of the box on each
    // side.
    if IsBoxUnits(aMask, 'maskUnits', 'objectBoundingBox') then
      lRegion := TSVGRect.CreateSize(
        BoxFraction(aMask, 'x', -0.1), BoxFraction(aMask, 'y', -0.1),
        BoxFraction(aMask, 'width', 1.2), BoxFraction(aMask, 'height', 1.2))
        .Transform(aBounds.UnitSquareTransform)
    else
      lRegion := TSVGRect.CreateSize(
        LengthOf(aMask, 'x', lContext, laHorizontal,
          aBounds.Left - aBounds.Width / 10),
        LengthOf(aMask, 'y', lContext, laVertical,
          aBounds.Top - aBounds.Height / 10),
        LengthOf(aMask, 'width', lContext, laHorizontal,
          aBounds.Width * 1.2),
        LengthOf(aMask, 'height', lContext, laVertical,
          aBounds.Height * 1.2));
    lClipped := not lRegion.IsEmpty
            and (bcClipPath in FBackend.Capabilities);
    if lClipped then
      begin
      FClipPath.Clear;
      FClipPath.AddRect(lRegion.Left, lRegion.Top, lRegion.Width,
        lRegion.Height, 0, 0);
      FBackend.PushClip(FClipPath, aState.CTM, frNonZero);
      end;
    try
      lState.CTM := UnitsTransform(aMask, 'maskContentUnits', aBounds)
        .Compose(aState.CTM);
      RenderChildren(aMask, lState, False);
    finally
      if lClipped then
        FBackend.PopClip;
    end;
  finally
    // The luminance of a mask is computed in the colour space of the
    // mask element, not in the one the shape it hides was drawn in.
    lMixed := lState.Style.ColorInterpolation
          <> aState.Style.ColorInterpolation;
    if lMixed then
      FBackend.SetColorInterpolation(lState.Style.ColorInterpolation);
    try
      FBackend.PopLayerAsMask(mmLuminance);
    finally
      if lMixed then
        FBackend.SetColorInterpolation(aState.Style.ColorInterpolation);
    end;
  end;
end;


// A colour of a primitive. currentColor is the colour at the position of
// the primitive: for a filter, that is where the filter stands in the
// document, not where it is used.
function TSVGRenderer.PrimitiveColour(aElement: TSVGElement;
  const aName: String; const aFallback: TSVGColor): TSVGColor;

var
  lText: String;

begin
  Result := aFallback;
  lText := Trim(aElement.AttributeDef(aName, ''));
  if lText = '' then
    Exit;
  if SameText(lText, 'currentColor') then
    Result := FStyles.ComputeStyleOf(aElement).Color
  else if not Result.TryParse(lText) then
    Result := aFallback;
end;


// Reads one primitive: its inputs, its values, and the box it writes when
// it declares one. A primitive that gives its box in object bounding box
// units has it measured against the box of the element.
procedure TSVGRenderer.ReadPrimitive(aElement: TSVGElement;
  aKind: TSVGFilterKind; aIndex: Integer; aBoxUnits: Boolean;
  const aBounds, aRegion: TSVGRect; const aContext: TSVGLengthContext;
  aResults: TStringList; out aPrimitive: TSVGFilterPrimitive);

  // A length of the primitive, measured against the box of the element
  // when the filter uses bounding box units.
  function Measure(const aName: String; aAxis: TSVGLengthAxis;
    aOrigin, aWhenSilent: Double): Double;
  begin
    // An axis the primitive omits is taken from the filter region.
    if not aElement.HasAttribute(aName) then
      Result := aWhenSilent
    // In box units a corner is measured from the corner of the box and a
    // width from zero; a percentage is the fraction as it is written.
    else if aBoxUnits then
      Result := aOrigin + BoxFraction(aElement, aName, 0)
        * aBounds.PercentBase(aAxis)
    else
      Result := LengthOf(aElement, aName, aContext, aAxis, 0);
  end;

var
  I: Integer;
  lNumbers: TSVGDoubleArray;
  lChild: TSVGElement;
  lText: String;

begin
  FillChar(aPrimitive, SizeOf(aPrimitive), 0);
  aPrimitive.Kind := aKind;
  aPrimitive.Opacity := 1;
  for I := 0 to 3 do
    SVGReadTransfer(nil, aPrimitive.Transfer[I]);
  aPrimitive.Colour := TSVGColor.Black;
  aPrimitive.Ratio := TSVGPreserveAspectRatio.Default;
  aPrimitive.Ratio.ReadAttribute(aElement);
  SetLength(aPrimitive.Inputs, 1);
  aPrimitive.Inputs[0] := SVGFilterInputOf(aElement.AttributeDef('in', ''),
    aResults, aIndex);
  aPrimitive.HasRegion := aElement.HasAttribute('x')
    or aElement.HasAttribute('y') or aElement.HasAttribute('width')
    or aElement.HasAttribute('height');
  if aPrimitive.HasRegion then
    aPrimitive.Region := TSVGRect.CreateSize(
      Measure('x', laHorizontal, aBounds.Left, aRegion.Left),
      Measure('y', laVertical, aBounds.Top, aRegion.Top),
      Measure('width', laHorizontal, 0, aRegion.Width),
      Measure('height', laVertical, 0, aRegion.Height));
  case aKind of
    fkGaussianBlur:
      begin
      lNumbers := SVGNumbersOf(aElement, 'stdDeviation');
      SetLength(aPrimitive.Numbers, 2);
      if Length(lNumbers) > 0 then
        begin
        aPrimitive.Numbers[0] := lNumbers[0];
        aPrimitive.Numbers[1] := lNumbers[High(lNumbers)];
        end;
      end;
    fkOffset:
      begin
      SetLength(aPrimitive.Numbers, 2);
      aPrimitive.Numbers[0] := Measure('dx', laHorizontal, 0, 0);
      aPrimitive.Numbers[1] := Measure('dy', laVertical, 0, 0);
      end;
    fkFlood:
      begin
      aPrimitive.Inputs := nil;
      aPrimitive.Colour := PrimitiveColour(aElement, 'flood-color',
        TSVGColor.Black);
      aPrimitive.Opacity := SVGNumberOf(aElement, 'flood-opacity', 1);
      end;
    fkMerge:
      begin
      aPrimitive.Inputs := nil;
      for I := 0 to aElement.ChildCount - 1 do
        begin
        if not (aElement[I] is TSVGElement) then
          Continue;
        lChild := TSVGElement(aElement[I]);
        if lChild.TagName <> 'feMergeNode' then
          Continue;
        SetLength(aPrimitive.Inputs, Length(aPrimitive.Inputs) + 1);
        aPrimitive.Inputs[High(aPrimitive.Inputs)] := SVGFilterInputOf(
          lChild.AttributeDef('in', ''), aResults, aIndex);
        end;
      end;
    fkComposite, fkBlend, fkDisplacementMap:
      begin
      SetLength(aPrimitive.Inputs, 2);
      aPrimitive.Inputs[1] := SVGFilterInputOf(
        aElement.AttributeDef('in2', ''), aResults, aIndex);
      if aKind = fkDisplacementMap then
        begin
        SetLength(aPrimitive.Numbers, 3);
        aPrimitive.Numbers[0] := SVGNumberOf(aElement, 'scale', 0);
        aPrimitive.Numbers[1] := SVGChannelOf(
          aElement.AttributeDef('xChannelSelector', 'A'));
        aPrimitive.Numbers[2] := SVGChannelOf(
          aElement.AttributeDef('yChannelSelector', 'A'));
        end;
      if aKind = fkBlend then
        aPrimitive.Operation := SVGBlendModeOf(
          aElement.AttributeDef('mode', 'normal'));
      if aKind = fkComposite then
        begin
        aPrimitive.Operation := SVGCompositeOperatorOf(
          aElement.AttributeDef('operator', 'over'));
        SetLength(aPrimitive.Numbers, 4);
        aPrimitive.Numbers[0] := SVGNumberOf(aElement, 'k1', 0);
        aPrimitive.Numbers[1] := SVGNumberOf(aElement, 'k2', 0);
        aPrimitive.Numbers[2] := SVGNumberOf(aElement, 'k3', 0);
        aPrimitive.Numbers[3] := SVGNumberOf(aElement, 'k4', 0);
        end;
      end;
    fkColorMatrix:
      begin
      aPrimitive.Operation := SVGColorMatrixTypeOf(
        aElement.AttributeDef('type', 'matrix'));
      aPrimitive.Numbers := SVGNumbersOf(aElement, 'values');
      end;
    fkMorphology:
      begin
      aPrimitive.Operation := SVGMorphologyOperatorOf(
        aElement.AttributeDef('operator', 'erode'));
      lNumbers := SVGNumbersOf(aElement, 'radius');
      SetLength(aPrimitive.Numbers, 2);
      if Length(lNumbers) > 0 then
        begin
        aPrimitive.Numbers[0] := lNumbers[0];
        aPrimitive.Numbers[1] := lNumbers[High(lNumbers)];
        end;
      end;
    fkComponentTransfer:
      for I := 0 to aElement.ChildCount - 1 do
        begin
        if not (aElement[I] is TSVGElement) then
          Continue;
        lChild := TSVGElement(aElement[I]);
        if lChild.TagName = 'feFuncR' then
          SVGReadTransfer(lChild, aPrimitive.Transfer[0])
        else if lChild.TagName = 'feFuncG' then
          SVGReadTransfer(lChild, aPrimitive.Transfer[1])
        else if lChild.TagName = 'feFuncB' then
          SVGReadTransfer(lChild, aPrimitive.Transfer[2])
        else if lChild.TagName = 'feFuncA' then
          SVGReadTransfer(lChild, aPrimitive.Transfer[3]);
        end;
    fkConvolveMatrix:
      begin
      lNumbers := SVGNumbersOf(aElement, 'order');
      aPrimitive.Order[0] := 3;
      aPrimitive.Order[1] := 3;
      if Length(lNumbers) > 0 then
        begin
        aPrimitive.Order[0] := Max(1, Round(lNumbers[0]));
        aPrimitive.Order[1] := Max(1, Round(lNumbers[High(lNumbers)]));
        end;
      aPrimitive.Numbers := SVGNumbersOf(aElement, 'kernelMatrix');
      aPrimitive.Divisor := SVGNumberOf(aElement, 'divisor', 0);
      aPrimitive.Bias := SVGNumberOf(aElement, 'bias', 0);
      aPrimitive.Target[0] := Round(SVGNumberOf(aElement, 'targetX',
        aPrimitive.Order[0] div 2));
      aPrimitive.Target[1] := Round(SVGNumberOf(aElement, 'targetY',
        aPrimitive.Order[1] div 2));
      aPrimitive.Preserve := SameText(Trim(
        aElement.AttributeDef('preserveAlpha', 'false')), 'true');
      aPrimitive.Operation := SVGEdgeModeOf(
        aElement.AttributeDef('edgeMode', 'duplicate'));
      end;
    fkTurbulence:
      begin
      aPrimitive.Inputs := nil;
      lNumbers := SVGNumbersOf(aElement, 'baseFrequency');
      SetLength(aPrimitive.Numbers, 4);
      if Length(lNumbers) > 0 then
        begin
        aPrimitive.Numbers[0] := lNumbers[0];
        aPrimitive.Numbers[1] := lNumbers[High(lNumbers)];
        end;
      aPrimitive.Numbers[2] := SVGNumberOf(aElement, 'numOctaves', 1);
      aPrimitive.Numbers[3] := SVGNumberOf(aElement, 'seed', 0);
      if SameText(Trim(aElement.AttributeDef('type', 'turbulence')),
                  'fractalNoise') then
        aPrimitive.Operation := 1;
      end;
    fkDiffuseLighting, fkSpecularLighting:
      begin
      SetLength(aPrimitive.Numbers, 12);
      aPrimitive.Numbers[0] := SVGNumberOf(aElement, 'surfaceScale', 1);
      aPrimitive.Numbers[1] := SVGNumberOf(aElement, 'diffuseConstant', 1);
      if aKind = fkSpecularLighting then
        aPrimitive.Numbers[1] := SVGNumberOf(aElement,
          'specularConstant', 1);
      aPrimitive.Numbers[2] := SVGNumberOf(aElement,
        'specularExponent', 1);
      aPrimitive.Colour := PrimitiveColour(aElement, 'lighting-color',
        TSVGColor.FromBytes(255, 255, 255, 255));
      ReadLight(aElement, aBoxUnits, aBounds, aPrimitive);
      end;
    fkImage:
      begin
      aPrimitive.Inputs := nil;
      // An feImage that refers to an element of a document draws that
      // element into a layer while the chain is drawn. One that refers to
      // a file takes the pixels of the file.
      if SVGReferencePath(SVGHRefOf(aElement)) = '' then
        FFilterImages[aIndex] := ReferencedElement(aElement,
          SVGHRefOf(aElement), TSVGElement)
      else if FImages <> nil then
        aPrimitive.Image := FImages.ResolveImage(SVGHRefOf(aElement),
          SVGElementBase(aElement, FBaseURI));
      end;
  end;
end;


// True when the element or an ancestor requests a background to gather.
function SVGBackgroundEnabled(aElement: TSVGElement): Boolean;

var
  lAt: TSVGElement;

begin
  Result := False;
  lAt := aElement;
  while lAt <> nil do
    begin
    if SameText(Copy(Trim(lAt.AttributeDef('enable-background', '')), 1, 3),
         'new') then
      Exit(True);
    if not (lAt.Parent is TSVGElement) then
      Exit;
    lAt := TSVGElement(lAt.Parent);
    end;
end;


// Resolves a filter for the element it is drawn on: the box it covers and
// the primitives that fill it, each with its inputs and its values, all in
// user space. False when the filter holds no primitive: SVG then draws
// nothing at all.
function TSVGRenderer.FilterChainOf(aElement, aFilter: TSVGElement;
  const aState: TSVGRenderState; out aChain: TSVGFilterChain): Boolean;

var
  I, J, lCount: Integer;
  lBounds: TSVGRect;
  lContext: TSVGLengthContext;
  lChild: TSVGElement;
  lKind: TSVGFilterKind;
  lResults: TStringList;
  lBox: Boolean;
  lStyle: TSVGComputedStyle;

begin
  Result := False;
  aChain.Region := TSVGRect.Empty;
  aChain.CTM := aState.CTM;
  aChain.Primitives := nil;
  aChain.Linear := True;
  aChain.FillPaint := TSVGPaint.None;
  aChain.StrokePaint := TSVGPaint.None;
  aChain.FillOpacity := 1;
  aChain.StrokeOpacity := 1;
  aChain.Bounds := TSVGRect.Empty;
  aChain.Background := False;
  if (aElement = nil) or (aFilter = nil) then
    Exit;
  lBounds := BoundsOf(aElement, aState);
  lContext := ContextOf(aState);
  // The region defaults to a border of one tenth of the box on each side,
  // as a mask does.
  if IsBoxUnits(aFilter, 'filterUnits', 'objectBoundingBox') then
    aChain.Region := TSVGRect.CreateSize(
      BoxFraction(aFilter, 'x', -0.1), BoxFraction(aFilter, 'y', -0.1),
      BoxFraction(aFilter, 'width', 1.2), BoxFraction(aFilter, 'height', 1.2))
      .Transform(lBounds.UnitSquareTransform)
  else
    aChain.Region := TSVGRect.CreateSize(
      LengthOf(aFilter, 'x', lContext, laHorizontal,
        lBounds.Left - lBounds.Width / 10),
      LengthOf(aFilter, 'y', lContext, laVertical,
        lBounds.Top - lBounds.Height / 10),
      LengthOf(aFilter, 'width', lContext, laHorizontal,
        lBounds.Width * 1.2),
      LengthOf(aFilter, 'height', lContext, laVertical,
        lBounds.Height * 1.2));
  if aChain.Region.IsEmpty then
    Exit;
  lStyle := FStyles.ComputeStyle(aFilter, aState.Style);
  aChain.Linear := lStyle.FilterInterpolation <> ciSRGB;
  aChain.FillPaint := aState.Style.Fill;
  aChain.StrokePaint := aState.Style.Stroke;
  aChain.FillOpacity := aState.Style.FillOpacity;
  aChain.StrokeOpacity := aState.Style.StrokeOpacity;
  aChain.Bounds := lBounds;
  aChain.Background := SVGBackgroundEnabled(aElement);
  lBox := IsBoxUnits(aFilter, 'primitiveUnits', 'userSpaceOnUse');
  lResults := TStringList.Create;
  try
    lCount := 0;
    SetLength(aChain.Primitives, aFilter.ChildCount);
    SetLength(FFilterImages, aFilter.ChildCount);
    for I := 0 to aFilter.ChildCount - 1 do
      FFilterImages[I] := nil;
    for I := 0 to aFilter.ChildCount - 1 do
      begin
      if not (aFilter[I] is TSVGElement) then
        Continue;
      lChild := TSVGElement(aFilter[I]);
      if not SVGFilterKindOf(lChild, lKind) then
        Continue;
      ReadPrimitive(lChild, lKind, lCount, lBox, lBounds, aChain.Region,
        lContext, lResults, aChain.Primitives[lCount]);
      if lChild.AttributeDef('result', '') <> '' then
        lResults.AddObject(Trim(lChild.Attributes['result']),
          TObject(PtrInt(lCount)));
      Inc(lCount);
      end;
    SetLength(aChain.Primitives, lCount);
    SetLength(FFilterImages, lCount);
    Result := lCount > 0;
  finally
    lResults.Free;
  end;
end;


// Draws the element through the filter of its style. The filter works on
// the drawing of the element, and a clip or a mask then works on the
// result of the filter. That is the order SVG puts them in.
procedure TSVGRenderer.RenderFiltered(aElement: TSVGElement;
  const aState: TSVGRenderState);

var
  lFilter: TSVGElement;
  lChain: TSVGFilterChain;
  lImages: array of TSVGElement;
  lInner: TSVGRenderState;
  lOpacity: Double;
  I: Integer;

begin
  // An element without a filter is drawn directly, and so is any element
  // when the backend cannot filter.
  if (aState.Style.Filter = '')
     or not (bcFilter in FBackend.Capabilities) then
    begin
    RenderSelf(aElement, aState);
    Exit;
    end;
  // SVG draws nothing for an element whose filter refers to something
  // that is not a filter. A filter without primitives gives a clear
  // surface, which draws nothing either.
  lFilter := ReferencedElement(aElement, aState.Style.Filter,
    TSVGFilterElement);
  if (lFilter = nil)
     or not FilterChainOf(aElement, lFilter, aState, lChain) then
    Exit;
  lImages := FFilterImages;
  for I := 0 to High(lImages) do
    begin
    if lImages[I] = nil then
      Continue;
    FBackend.PushLayer(lChain.Region.Transform(aState.CTM), 1, True);
    try
      RenderElement(lImages[I], aState);
    finally
      FBackend.PopLayerAsFilterImage(I);
    end;
    end;
  // The filter works on the element without its opacity. The opacity is
  // applied to the result, and is no part of what the chain reads.
  lInner := aState;
  lOpacity := 1;
  if bcGroupOpacity in FBackend.Capabilities then
    begin
    lOpacity := aState.Style.Opacity;
    lInner.Style.Opacity := 1;
    end;
  FBackend.PushLayer(lChain.Region.Transform(aState.CTM), lOpacity, True);
  try
    RenderSelf(aElement, lInner);
  finally
    FBackend.PopLayerAsFilter(lChain);
  end;
end;


procedure TSVGRenderer.RenderShielded(aElement, aClip, aMask: TSVGElement;
  const aState: TSVGRenderState);

var
  lShape: TSVGElement;
  lBounds: TSVGRect;
  lShapeState: TSVGRenderState;
  lClipByMask, lLayer, lPushed: Boolean;

begin
  if not (bcClipPath in FBackend.Capabilities) then
    aClip := nil;
  if not (bcMask in FBackend.Capabilities) then
    aMask := nil;
  if (aClip = nil) and (aMask = nil) then
    begin
    RenderSelf(aElement, aState);
    Exit;
    end;
  lBounds := TSVGRect.Empty;
  if (aClip <> nil) or (aMask <> nil) then
    lBounds := BoundsOf(aElement, aState);
  lShape := nil;
  if aClip <> nil then
    lShape := SingleClipShape(aClip);
  lClipByMask := (aClip <> nil) and (lShape = nil);
  lLayer := (aMask <> nil) or lClipByMask;
  lPushed := False;
  if lLayer then
    FBackend.PushLayer(TSVGRect.Empty, 1, True);
  try
    if lShape <> nil then
      begin
      lShapeState := aState;
      lShapeState.CTM := UnitsTransform(aClip, 'clipPathUnits', lBounds)
        .Compose(aState.CTM);
      lShapeState.Style := FStyles.ComputeStyle(lShape,
        FStyles.ComputeStyle(aClip, aState.Style));
      lShapeState.CTM := TransformOf(lShape).Compose(lShapeState.CTM);
      // A shape with no geometry gives an empty silhouette, and a clip
      // path of nothing keeps nothing. The clip is pushed either way, and
      // is empty when there was no path to build.
      if not BuildSVGShapePath(lShape, FClipPath, ContextOf(lShapeState)) then
        FClipPath.Clear;
      FBackend.PushClip(FClipPath, lShapeState.CTM,
        lShapeState.Style.ClipRule);
      lPushed := True;
      end;
    try
      RenderFiltered(aElement, aState);
    finally
      if lPushed then
        FBackend.PopClip;
    end;
    // The clip is applied first. The mask then weighs the pixels the clip
    // kept.
    if lClipByMask then
      RenderClipAsMask(aClip, aState, lBounds);
    if aMask <> nil then
      RenderMaskContent(aMask, aState, lBounds);
  finally
    if lLayer then
      FBackend.PopLayer;
  end;
end;


procedure TSVGRenderer.RenderImage(aElement: TSVGElement;
  const aState: TSVGRenderState);

var
  lSource: ISVGImageSource;
  lContext: TSVGLengthContext;
  lPort, lIntrinsic, lClip: TSVGRect;
  lRatio: TSVGPreserveAspectRatio;
  lCTM: TSVGMatrix;
  lClipped: Boolean;
  lHRef: String;

begin
  if aState.Style.Visibility <> svVisible then
    Exit;
  lHRef := SVGHRefOf(aElement);
  if SVGNamesADocument(SVGReferencePath(lHRef))
     and RenderImageDocument(aElement, aState, SVGReferencePath(lHRef)) then
    Exit;
  if FImages = nil then
    Exit;
  lSource := FImages.ResolveImage(lHRef,
    SVGElementBase(aElement, FBaseURI));
  if lSource = nil then
    Exit;
  if (lSource.GetWidth <= 0) or (lSource.GetHeight <= 0) then
    Exit;
  lContext := ContextOf(aState);
  lPort := TSVGRect.CreateSize(
    LengthOf(aElement, 'x', lContext, laHorizontal, 0),
    LengthOf(aElement, 'y', lContext, laVertical, 0),
    LengthOf(aElement, 'width', lContext, laHorizontal, 0),
    LengthOf(aElement, 'height', lContext, laVertical, 0));
  if lPort.IsEmpty then
    Exit;
  lIntrinsic := TSVGRect.CreateSize(0, 0, lSource.GetWidth, lSource.GetHeight);
  lRatio := TSVGPreserveAspectRatio.Default;
  lRatio.ReadAttribute(aElement);
  lCTM := lRatio.ViewBoxTransform(lIntrinsic, lPort).Compose(aState.CTM);
  lClipped := (aState.Style.Overflow = ovHidden)
          and (bcClipPath in FBackend.Capabilities);
  if lClipped then
    begin
    lClip := aState.Style.Clip.Narrow(lPort, lContext);
    FClipPath.Clear;
    FClipPath.AddRect(lClip.Left, lClip.Top, lClip.Width, lClip.Height, 0, 0);
    FBackend.PushClip(FClipPath, aState.CTM, frNonZero);
    end;
  try
    FBackend.DrawImage(lSource, lIntrinsic, lCTM,
      aState.Style.Opacity * aState.Alpha);
  finally
    if lClipped then
      FBackend.PopClip;
  end;
end;


// Draws the tiles of a pattern into the shape a path makes: its inside,
// or the line the pen draws along it when aStroked. The shape is clipped
// to and the content of the pattern drawn once per tile across it.
function TSVGRenderer.PaintWithPattern(aPath: TSVGPath;
  const aPaint: TSVGPaint; const aState: TSVGRenderState;
  const aBounds: TSVGRect; aRule: TSVGFillRule; const aPen: TSVGPen;
  aStroked: Boolean; aOpacity: Double): Boolean;

var
  lPattern: TSVGElement;
  lContext: TSVGLengthContext;
  lState, lTileState: TSVGRenderState;
  lToPattern, lFromPattern, lContent: TSVGMatrix;
  lBox, lSpan: TSVGRect;
  lX, lY, lWidth, lHeight: Double;
  lFirstCol, lLastCol, lFirstRow, lLastRow, lCol, lRow: Integer;
  lRatio: TSVGPreserveAspectRatio;
  lCover: TSVGRect;
  lObjectUnits: Boolean;

begin
  Result := False;
  if (FPatternDepth > 0) or not (bcClipPath in FBackend.Capabilities) then
    Exit;
  if aStroked and (aPen.Width <= 0) then
    Exit;
  // A stroked line reaches a pen width beyond the path on every side,
  // joins and caps included. A straight line has no box, but the line
  // drawn along it does.
  lCover := aBounds;
  if aStroked then
    lCover := TSVGRect.Create(lCover.Left - aPen.Width,
      lCover.Top - aPen.Width, lCover.Right + aPen.Width,
      lCover.Bottom + aPen.Width);
  if lCover.IsEmpty then
    Exit;
  // The style layer knows which document the server came from. It is not
  // this one when the paint referred to another file.
  lPattern := FStyles.ServerElementOf(aPaint.Server);
  if not (lPattern is TSVGPatternElement) then
    Exit;
  lContext := ContextOf(aState);
  lObjectUnits := IsBoxUnits(lPattern, 'patternUnits', 'objectBoundingBox');
  if lObjectUnits then
    begin
    lX := aBounds.Left + BoxFraction(lPattern, 'x', 0) * aBounds.Width;
    lY := aBounds.Top + BoxFraction(lPattern, 'y', 0) * aBounds.Height;
    lWidth := BoxFraction(lPattern, 'width', 0) * aBounds.Width;
    lHeight := BoxFraction(lPattern, 'height', 0) * aBounds.Height;
    end
  else
    begin
    lX := LengthOf(lPattern, 'x', lContext, laHorizontal, 0);
    lY := LengthOf(lPattern, 'y', lContext, laVertical, 0);
    lWidth := LengthOf(lPattern, 'width', lContext, laHorizontal, 0);
    lHeight := LengthOf(lPattern, 'height', lContext, laVertical, 0);
    end;
  if (lWidth <= 0) or (lHeight <= 0) then
    Exit;
  lToPattern := TSVGMatrix.Identity;
  lToPattern.ReadAttributeNamed(lPattern, 'patternTransform');
  if not lToPattern.Invert(lFromPattern) then
    Exit;
  // The tiles to draw are the ones that meet the shape, measured in the
  // space the pattern transform maps from.
  lSpan := lCover.Transform(lFromPattern);
  lFirstCol := Floor((lSpan.Left - lX) / lWidth);
  lLastCol := Ceil((lSpan.Right - lX) / lWidth) - 1;
  lFirstRow := Floor((lSpan.Top - lY) / lHeight);
  lLastRow := Ceil((lSpan.Bottom - lY) / lHeight) - 1;
  if (lLastCol < lFirstCol) or (lLastRow < lFirstRow) then
    Exit;
  if (lLastCol - lFirstCol + 1) * (lLastRow - lFirstRow + 1)
     > SVGMaxPatternTiles then
    Exit;
  lContent := TSVGMatrix.Identity;
  lBox := TSVGRect.Empty;
  if lBox.ReadViewBoxAttribute(lPattern) and not lBox.IsEmpty then
    begin
    lRatio := TSVGPreserveAspectRatio.Default;
    lRatio.ReadAttribute(lPattern);
    lContent := lRatio.ViewBoxTransform(lBox,
      TSVGRect.CreateSize(0, 0, lWidth, lHeight));
    end
  else if IsBoxUnits(lPattern, 'patternContentUnits', 'userSpaceOnUse') then
    lContent := TSVGMatrix.Scaling(aBounds.Width, aBounds.Height);
  lState := aState;
  // The content of a pattern sits where the pattern element sits, and
  // inherits from there. The element being filled is not its parent, and
  // taking the stroke of that element would draw a line around every
  // tile.
  lState.Style := FStyles.ComputeStyleOf(lPattern);
  lState.Viewport := TSVGRect.CreateSize(0, 0, lWidth, lHeight);
  Result := True;
  if aStroked then
    FBackend.PushStrokeClip(aPath, aState.CTM, aPen)
  else
    FBackend.PushClip(aPath, aState.CTM, aRule);
  Inc(FPatternDepth);
  try
    for lRow := lFirstRow to lLastRow do
      for lCol := lFirstCol to lLastCol do
        begin
        lTileState := lState;
        lTileState.Alpha := aOpacity;
        lTileState.CTM := lContent.Compose(TSVGMatrix.Translation(
          lX + lCol * lWidth, lY + lRow * lHeight))
          .Compose(lToPattern).Compose(aState.CTM);
        RenderChildren(lPattern, lTileState, False);
        end;
  finally
    Dec(FPatternDepth);
    FBackend.PopClip;
  end;
end;


procedure TSVGRenderer.RenderText(aElement: TSVGElement;
  const aState: TSVGRenderState);

var
  I: Integer;
  lRun: TSVGTextRun;
  lPaint: TSVGPaint;
  lAlpha, lRunAlpha: Double;
  lFill, lStroke, lLayer: Boolean;

begin
  if (TextFonts = nil) or (aState.Style.Visibility <> svVisible) then
    Exit;
  if not FLayout.Layout(aElement, FStyles, TextFonts, aState.Style,
                        ContextOf(aState)) then
    Exit;
  lAlpha := aState.Alpha * aState.Style.Opacity;
  for I := 0 to FLayout.RunCount - 1 do
    begin
    lRun := FLayout.Runs[I];
    if lRun.Style.Visibility <> svVisible then
      Continue;
    lFill := lRun.Style.Fill.Kind <> spNone;
    lStroke := (lRun.Style.Stroke.Kind <> spNone)
           and (lRun.Style.Pen.Width > 0);
    // A run that paints twice needs a layer, for the same reason a shape
    // does: the two paints overlap along the edge of every glyph.
    lRunAlpha := lAlpha;
    lLayer := lFill and lStroke and LayerNeeded(lRun.Style);
    if lLayer then
      begin
      FBackend.PushLayer(TSVGRect.Empty, lRun.Style.Opacity, True);
      lRunAlpha := aState.Alpha;
      end;
    try
      if lFill then
        begin
        lPaint := lRun.Style.Fill;
        // A pattern is drawn from its children, so the glyphs become the
        // shape it is clipped to rather than a paint handed over.
        if IsPatternPaint(lPaint) then
          begin
          FTextPath.Clear;
          if SVGAppendGlyphRun(lRun.Font, lRun.Glyphs, FTextPath)
             and PaintWithPattern(FTextPath, lPaint, aState,
                   PathBounds(FTextPath), frNonZero, lRun.Style.Pen,
                   False, lRun.Style.FillOpacity * lRunAlpha) then
            lPaint := TSVGPaint.None
          else
            lPaint := lPaint.Resolved;
          end;
        if lPaint.Kind <> spNone then
          FBackend.DrawGlyphRun(lRun.Font, lRun.Glyphs, aState.CTM,
            lPaint, lRun.Style.FillOpacity * lRunAlpha);
        end;
      if lStroke then
        RenderTextStroke(lRun, aState, lRunAlpha);
    finally
      if lLayer then
        FBackend.PopLayer;
    end;
    end;
  // A line belongs to the element that requested it, not to the runs it
  // crosses, so the lines are drawn once all the runs are drawn.
  for I := 0 to FLayout.DecorationCount - 1 do
    RenderDecoration(FLayout.Decorations[I], aState, lAlpha);
end;


// Lays out a text element and gathers the outlines of all its glyphs into
// one path, in the user space of the element. This is the bare geometry:
// the paint of the text, and whether it paints at all, is left to the
// caller. False when the text lays out to nothing, or no outline is
// available.
function TSVGRenderer.BuildTextPath(aElement: TSVGElement;
  const aState: TSVGRenderState; aPath: TSVGPath): Boolean;

var
  I: Integer;
  lRun: TSVGTextRun;

begin
  Result := False;
  aPath.Clear;
  if TextFonts = nil then
    Exit;
  if not FLayout.Layout(aElement, FStyles, TextFonts, aState.Style,
                        ContextOf(aState)) then
    Exit;
  for I := 0 to FLayout.RunCount - 1 do
    begin
    lRun := FLayout.Runs[I];
    if SVGAppendGlyphRun(lRun.Font, lRun.Glyphs, aPath) then
      Result := True;
    end;
end;


// Strokes the outlines of a run with the pen of its style. The glyphs
// become one path, which the backend strokes as it strokes any other.
procedure TSVGRenderer.RenderTextStroke(const aRun: TSVGTextRun;
  const aState: TSVGRenderState; aOpacity: Double);

var
  lPaint: TSVGPaint;

begin
  if aRun.Font = nil then
    Exit;
  FTextPath.Clear;
  if not SVGAppendGlyphRun(aRun.Font, aRun.Glyphs, FTextPath) then
    Exit;
  if FTextPath.IsEmpty then
    Exit;
  // A gradient strokes directly. A pattern is drawn from its children,
  // so the line the pen draws round the glyphs becomes the shape it is
  // clipped to.
  lPaint := aRun.Style.Stroke;
  if IsPatternPaint(lPaint) then
    begin
    if PaintWithPattern(FTextPath, lPaint, aState, PathBounds(FTextPath),
         frNonZero, aRun.Style.Pen, True,
         aRun.Style.StrokeOpacity * aOpacity) then
      Exit;
    lPaint := lPaint.Resolved;
    end;
  if IsPatternPaint(lPaint) then
    Exit;
  FBackend.StrokePath(FTextPath, aState.CTM, lPaint, aRun.Style.Pen,
    aRun.Style.StrokeOpacity * aOpacity);
end;


// Draws the lines that one element requested, under, over or through the
// text it holds. They take the fill and the stroke of that element, and
// not those of the text they cross.
procedure TSVGRenderer.RenderDecoration(const aBand: TSVGTextBand;
  const aState: TSVGRenderState; aOpacity: Double);

var
  lThickness, lSize, lUnder: Double;

  procedure Bar(aOffset: Double);
  begin
    FPath.Clear;
    if aBand.Vertical then
      FPath.AddRect(aBand.Baseline + aOffset - lThickness / 2, aBand.Left,
        lThickness, aBand.Right - aBand.Left, 0, 0)
    else
      FPath.AddRect(aBand.Left, aBand.Baseline + aOffset - lThickness / 2,
        aBand.Right - aBand.Left, lThickness, 0, 0);
    if aBand.Style.Fill.Kind <> spNone then
      FBackend.FillPath(FPath, aState.CTM, aBand.Style.Fill, frNonZero,
        aBand.Style.FillOpacity * aOpacity);
    if (aBand.Style.Stroke.Kind <> spNone) and (aBand.Style.Pen.Width > 0) then
      FBackend.StrokePath(FPath, aState.CTM, aBand.Style.Stroke,
        aBand.Style.Pen, aBand.Style.StrokeOpacity * aOpacity);
  end;

begin
  if aBand.Lines = [] then
    Exit;
  // Text laid along a path has a glyph at every angle, so one straight bar
  // cannot cross it.
  if aBand.Turned or (aBand.Right <= aBand.Left) or (aBand.Font = nil) then
    Exit;
  lSize := aBand.Font.GetSize;
  if lSize <= 0 then
    Exit;
  // A face that states where it puts an underline is followed. For one
  // that states nothing, these are the fractions of the size that common
  // faces use: a line 0.045 of the size thick, 0.063 below the baseline,
  // and a strike a quarter above it.
  lThickness := aBand.Font.GetUnderlineThickness;
  if lThickness <= 0 then
    lThickness := lSize * 0.045;
  lUnder := aBand.Font.GetUnderlinePosition;
  if lUnder <= 0 then
    lUnder := lSize * 0.063
  else
    // The face gives the top of the line, and the bar is drawn about its
    // middle.
    lUnder := lUnder + lThickness / 2;
  // A column has no baseline running across it to place a line against,
  // so the two lines are drawn on the sides of the em box and the strike
  // down the middle. Under is a quarter turn clockwise from the direction
  // the text runs, as it is for text across the page, which puts it on
  // the left of the column.
  if aBand.Vertical then
    begin
    if tdUnderline in aBand.Lines then
      Bar(-lSize / 2);
    if tdOverline in aBand.Lines then
      Bar(lSize / 2);
    if tdLineThrough in aBand.Lines then
      Bar(0);
    Exit;
    end;
  if tdUnderline in aBand.Lines then
    Bar(lUnder);
  if tdOverline in aBand.Lines then
    Bar(-aBand.Font.GetAscent);
  if tdLineThrough in aBand.Lines then
    Bar(-lSize / 4);
end;


procedure TSVGRenderer.GetSetDocuments(aValue: ISVGDocumentResolver);

begin
  FDocuments := aValue;
  FStyles.Documents := aValue;
end;


procedure TSVGRenderer.GetSetLinkHistory(aValue: ISVGLinkHistory);

begin
  FLinkHistory := aValue;
  FStyles.LinkHistory := aValue;
end;


procedure TSVGRenderer.GetSetStyleSheets(aValue: ISVGStyleSheetResolver);

begin
  FStyleSheets := aValue;
  FStyles.StyleSheets := aValue;
end;


function TSVGRenderer.ExternalDocumentOf(aFrom: TSVGElement;
  const aPath: String): TSVGDocument;

begin
  Result := nil;
  if (FDocuments = nil) or (FDocumentDepth >= SVGMaxDocumentDepth) then
    Exit;
  Result := FDocuments.ResolveDocument(aPath,
    SVGElementBase(aFrom, FBaseURI));
end;


function TSVGRenderer.ExternalUseSubtree(aUse: TSVGElement;
  const aPath, aFragment: String; out aKey: String): TSVGElement;

var
  lSource: TSVGDocument;
  lTarget, lGroup, lCopy: TSVGElement;
  lExpander: TSVGUseExpander;
  lX, lY, lTransform: String;

begin
  Result := nil;
  aKey := '';
  lSource := ExternalDocumentOf(aUse, aPath);
  if lSource = nil then
    Exit;
  // A subtree of a document already being drawn closes a cycle, and
  // nothing is drawn for one that closes it.
  aKey := SVGDocumentKey(lSource.BaseURI);
  if FOpenDocuments.IndexOf(aKey) >= 0 then
    Exit;
  if aFragment = '' then
    lTarget := lSource.Root
  else
    lTarget := lSource.ElementByID(aFragment);
  if lTarget = nil then
    Exit;
  lGroup := CreateSVGElement('g');
  try
    SVGCopyUseAttributes(aUse, lGroup);
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
    // Nested uses in the copy refer to ids of the file it came from, so
    // they are expanded there before the copy is brought over.
    lExpander := TSVGUseExpander.Create(lSource);
    try
      lExpander.MaxDepth := FMaxUseDepth;
      lExpander.ExpandSubtree(lCopy);
    finally
      lExpander.Free;
    end;
    // A local reference in the copy points to an id of the file it came
    // from, not to an id of this file.
    if lSource.BaseURI <> '' then
      SVGRebaseReferences(lCopy, lSource.BaseURI);
    Result := lGroup;
    lGroup := nil;
  finally
    lGroup.Free;
  end;
end;


function TSVGRenderer.UseSubtree(aUse: TSVGElement;
  out aKey: String): TSVGElement;

var
  lHRef, lPath: String;

begin
  aKey := '';
  lHRef := SVGHRefOf(aUse);
  lPath := SVGReferencePath(lHRef);
  if lPath <> '' then
    begin
    Inc(FDocumentDepth);
    try
      Result := ExternalUseSubtree(aUse, lPath, SVGReferenceFragment(lHRef),
        aKey);
    finally
      Dec(FDocumentDepth);
    end;
    Exit;
    end;
  FExpander.MaxDepth := FMaxUseDepth;
  if FExpander.Expand(aUse, Result) <> urOK then
    Result := nil;
end;


function TSVGRenderer.RenderImageDocument(aElement: TSVGElement;
  const aState: TSVGRenderState; const aPath: String): Boolean;

var
  lSource: TSVGDocument;
  lRoot: TSVGElement;
  lContext: TSVGLengthContext;
  lOuterBase, lKey: String;

begin
  Result := False;
  lSource := ExternalDocumentOf(aElement, aPath);
  if (lSource = nil) or not (lSource.Root is TSVGSVGElement) then
    Exit;
  // A document that refers to one already being drawn closes a cycle, and
  // SVG draws nothing for an image that closes one. A document inside a
  // URI has no file name, so the URI is used as its key.
  if SVGIsDataURI(aPath) then
    lKey := aPath
  else
    lKey := SVGDocumentKey(lSource.BaseURI);
  if FOpenDocuments.IndexOf(lKey) >= 0 then
    Exit;
  lContext := ContextOf(aState);
  lRoot := TSVGElement(lSource.Root.Clone);
  try
    LinkToOrigin(lRoot, lSource.Root);
    // A gradient or a clip that the document refers to inside itself is
    // its own, and not one of the document that drew it. Every reference
    // is therefore rewritten to point into the file or the URI it came
    // from.
    SVGRebaseReferences(lRoot, aPath);
    // The image element gives the viewport that the document is drawn
    // into. Its own aspect ratio places the drawing inside that viewport.
    lRoot.Attributes['x'] := aElement.AttributeDef('x', '0');
    lRoot.Attributes['y'] := aElement.AttributeDef('y', '0');
    lRoot.Attributes['width'] := aElement.AttributeDef('width', '0');
    lRoot.Attributes['height'] := aElement.AttributeDef('height', '0');
    if aElement.HasAttribute('preserveAspectRatio') then
      lRoot.Attributes['preserveAspectRatio'] :=
        aElement.Attributes['preserveAspectRatio'];
    // The clip narrows that same viewport, so it travels with it. Taking
    // it from the computed style also takes a clip that a stylesheet set.
    if aState.Style.Clip.Shaped then
      lRoot.Attributes['clip'] := aState.Style.Clip.ToString;
    lRoot.AdoptInto(FDocument);
    lOuterBase := FBaseURI;
    FBaseURI := lSource.BaseURI;
    Inc(FDocumentDepth);
    FOpenDocuments.Add(lKey);
    try
      RenderElement(lRoot, aState);
    finally
      FOpenDocuments.Delete(FOpenDocuments.Count - 1);
      Dec(FDocumentDepth);
      FBaseURI := lOuterBase;
    end;
    Result := True;
  finally
    lRoot.Free;
  end;
end;


procedure TSVGRenderer.RenderUse(aUse: TSVGElement;
  const aState: TSVGRenderState);

var
  lGroup, lChild: TSVGElement;
  lForeign: Boolean;
  lKey: String;

begin
  // The depth stays raised while the subtree is drawn, not only while it
  // is built. A subtree from another file may refer back to this file.
  lForeign := SVGReferencePath(SVGHRefOf(aUse)) <> '';
  if lForeign then
    begin
    if FDocumentDepth >= SVGMaxDocumentDepth then
      Exit;
    Inc(FDocumentDepth);
    end;
  try
    lGroup := UseSubtree(aUse, lKey);
    if lGroup = nil then
      Exit;
    try
      // The expansion is pointed at the document so selectors match in it.
      lGroup.AdoptInto(FDocument);
      lChild := FirstElementChild(lGroup);
      if lChild is TSVGSymbolElement then
        lChild := SymbolAsViewport(lGroup, lChild);
      if lChild is TSVGSVGElement then
        CopyUseSize(aUse, lChild);
      // The file the subtree came from stays open while it is drawn, so
      // that a reference in it back to that file closes a cycle.
      if lKey <> '' then
        FOpenDocuments.Add(lKey);
      try
        RenderElement(lGroup, aState);
      finally
        if lKey <> '' then
          FOpenDocuments.Delete(FOpenDocuments.Count - 1);
      end;
    finally
      lGroup.Free;
    end;
  finally
    if lForeign then
      Dec(FDocumentDepth);
  end;
end;


end.
