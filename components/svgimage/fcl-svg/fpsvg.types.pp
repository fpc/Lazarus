{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Geometry, colour and paint types shared by the whole SVG pipeline.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.types;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}
{$modeswitch advancedrecords}
{$INTERFACES CORBA}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, FpImage;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, fpimage;
{$ENDIF FPC_DOTTEDUNITS}

const
  SVGNormalFontWeight = 400;
  SVGBoldFontWeight = 700;

type
  ESVGError = class(Exception);

  { The text of a document: UTF-8, whatever string type the RTL is built
    with. Indexing it gives bytes and Length counts them, and it converts
    to and from the String of the RTL without loss. }
  TSVGString = UTF8String;

  TSVGColor = type TFPColor;
  PSVGColor = ^TSVGColor;
  TSVGColorArray = array of TSVGColor;

  { Methods for TSVGColor. }
  TSVGColorHelper = record helper for TSVGColor
    // Builds a colour from 8-bit sRGB components, expanding each to 16 bits.
    class function FromBytes(aRed, aGreen, aBlue, aAlpha: Byte): TSVGColor; static;
    // Fully transparent black.
    class function Transparent: TSVGColor; static;
    // Opaque black.
    class function Black: TSVGColor; static;
    // Looks up an SVG colour keyword. The name is not case sensitive.
    class function FromName(const aName: TSVGString;
      out aColor: TSVGColor): Boolean; static;
    // True when every channel is an exact multiple of 257.
    function IsEightBit: Boolean;
    // Hex form, 8-bit when exact and 16-bit otherwise.
    function ToString: String;
    // Parses #rgb, #rrggbb, rgb(), rgba() or a colour keyword.
    function TryParse(const aText: TSVGString): Boolean;
  end;

  TSVGDoubleArray = array of Double;

  TSVGFillRule = (frNonZero, frEvenOdd);
  TSVGLineCap  = (lcButt, lcRound, lcSquare);
  TSVGLineJoin = (ljMiter, ljRound, ljBevel);
  TSVGMaskMode = (mmLuminance, mmAlpha);


const
  { Pre-defined SVG filter sources.
    Any other input is the index of an earlier primitive in the chain  }

  SVGFilterSourceGraphic = -1;
  SVGFilterSourceAlpha = -2;
  SVGFilterBackgroundImage = -3;
  SVGFilterBackgroundAlpha = -4;
  SVGFilterFillPaint = -5;
  SVGFilterStrokePaint = -6;

type
  TSVGLengthUnit = (luNumber, luPercent, luEm, luEx, luPx, luCm, luMm,
                    luIn, luPt, luPc);
  TSVGLengthAxis = (laHorizontal, laVertical, laDiagonal);

  TSVGAspectAlign = (paNone,
    paXMinYMin, paXMidYMin, paXMaxYMin,
    paXMinYMid, paXMidYMid, paXMaxYMid,
    paXMinYMax, paXMidYMax, paXMaxYMax);
  TSVGMeetOrSlice = (msMeet, msSlice);

  { The colour space that channels are mixed in.
    SVG uses sRGB when a document sets no color-interpolation.
    The value auto may resolve to either space. }
  TSVGColorInterpolation = (ciSRGB, ciLinearRGB);

  TSVGPaintServerKind = (pkLinearGradient, pkRadialGradient, pkPattern);
  TSVGSpreadMethod = (smPad, smReflect, smRepeat);
  TSVGGradientUnits = (guObjectBoundingBox, guUserSpaceOnUse);
  TSVGPaintKind = (spNone, spColor, spServer);
  { The paint to use when the paint server draws nothing. }
  TSVGPaintFallback = (pfAbsent, pfNone, pfColor);
  TSVGPathSegmentKind = (skMoveTo, skLineTo, skCubicTo, skClose);

  TSVGPoint = record
    X, Y: Double;
    constructor Create(aX, aY: Double);
    // The two coordinates separated by a space.
    function ToString: String;
  end;
  PSVGPoint = ^TSVGPoint;
  TSVGPointArray = array of TSVGPoint;

  TSVGMatrix = record
    a, b, c, d, e, f: Double;
    constructor Create(aA, aB, aC, aD, aE, aF: Double);
    // Matrix leaving every point unchanged.
    class function Identity: TSVGMatrix; static;
    // Translation matrix.
    class function Translation(aTX, aTY: Double): TSVGMatrix; static;
    // Scaling matrix.
    class function Scaling(aSX, aSY: Double): TSVGMatrix; static;
    // Rotation matrix, angle in degrees, positive towards the y axis.
    class function Rotation(aDegrees: Double): TSVGMatrix; static;
    // Rotation about a centre, angle in degrees.
    class function RotationAbout(aDegrees, aCX, aCY: Double): TSVGMatrix; static;
    // Horizontal skew matrix, angle in degrees.
    class function SkewingX(aDegrees: Double): TSVGMatrix; static;
    // Vertical skew matrix, angle in degrees.
    class function SkewingY(aDegrees: Double): TSVGMatrix; static;
    // True when the matrix is the identity.
    function IsIdentity: Boolean;
    // Determinant of the linear part.
    function Determinant: Double;
    // Largest factor by which the matrix can stretch a unit vector.
    function MaxScale: Double;
    // Matrix that applies this one first, then aSecond.
    function Compose(const aSecond: TSVGMatrix): TSVGMatrix;
    // Inverts the matrix. False when it cannot be inverted, and aResult
    // is then unchanged.
    function Invert(out aResult: TSVGMatrix): Boolean;
    // Applies the matrix to a point.
    function Transform(const aPoint: TSVGPoint): TSVGPoint;
    // Applies the linear part only, ignoring translation.
    function TransformVector(const aPoint: TSVGPoint): TSVGPoint;
    // "identity", or the six coefficients in brackets.
    function ToString: String;
  end;

  TSVGRect = record
    Left, Top, Right, Bottom: Double;
    constructor Create(aLeft, aTop, aRight, aBottom: Double);
    constructor CreateSize(aX, aY, aWidth, aHeight: Double);
    // Empty Rectangle.
    class function Empty: TSVGRect; static;
    // True when the rectangle has no positive extent.
    function IsEmpty: Boolean;
    // Horizontal extent, zero when empty.
    function Width: Double;
    // Vertical extent, zero when empty.
    function Height: Double;
    // Smallest rectangle covering this one and aOther.
    function Union(const aOther: TSVGRect): TSVGRect;
    // Overlap with aOther, empty when they do not meet.
    function Intersect(const aOther: TSVGRect): TSVGRect;
    // Bounding box of the four transformed corners.
    function Transform(const aMatrix: TSVGMatrix): TSVGRect;
    // The size that a percentage on the given axis is measured against.
    function PercentBase(aAxis: TSVGLengthAxis): Double;
    // Maps the unit square onto this rectangle; identity when it is empty.
    function UnitSquareTransform: TSVGMatrix;
    // "empty", or the four edges in brackets.
    function ToString: String;
  end;

  TSVGLength = record
    Value      : Double;
    LengthUnit : TSVGLengthUnit;
    constructor Create(aValue: Double; aUnit: TSVGLengthUnit);
    // Length of zero user units.
    class function Zero: TSVGLength; static;
    // True when the length needs no viewport or font size to resolve.
    function IsAbsolute: Boolean;
    // Resolves to user units against a viewport size and a font size.
    function Resolve(aPercentBase, aFontSize, aXHeight, aDPI: Double): Double;
    // The value followed by its unit suffix.
    function ToString: String;
  end;
  TSVGLengthArray = array of TSVGLength;

  { Everything a length needs to resolve to user units. }
  TSVGLengthContext = record
    Viewport : TSVGRect;
    FontSize : Double;
    XHeight  : Double;
    DPI      : Double;
    constructor Create(const aViewport: TSVGRect);
    // A viewport of 100 by 100, at the initial SVG font size and 96 dots per inch.
    class function Default: TSVGLengthContext; static;
    // Resolves a length, taking percentages along the given axis.
    function Resolve(const aLength: TSVGLength; aAxis: TSVGLengthAxis): Double;
  end;

  TSVGPreserveAspectRatio = record
    Align       : TSVGAspectAlign;
    MeetOrSlice : TSVGMeetOrSlice;
    constructor Create(aAlign: TSVGAspectAlign; aMeetOrSlice: TSVGMeetOrSlice);
    { What SVG uses when a document sets no preserveAspectRatio:
      scale the drawing to fit inside the viewport, and centre it there. }
    class function Default: TSVGPreserveAspectRatio; static;
    { The matrix that maps the viewBox onto the viewport.
      The identity when either of them has no width or height.}
    function ViewBoxTransform(const aViewBox, aViewport: TSVGRect): TSVGMatrix;
    // The alignment keyword, such as xMidYMid, with " slice" after it
    // when the drawing is scaled to cover the viewport.
    function ToString: String;
  end;

  { A named view of a document: a viewBox, an aspect ratio and an extra
    transform. Each of the three is optional; a missing one is taken from
    the element the view is applied to. Only an svgView(...) fragment can
    set a transform. }
  TSVGView = record
    Name         : String;
    ViewBox      : TSVGRect;
    HasViewBox   : Boolean;
    Ratio        : TSVGPreserveAspectRatio;
    HasRatio     : Boolean;
    Transform    : TSVGMatrix;
    HasTransform : Boolean;
    // A view that changes nothing.
    class function None: TSVGView; static;
  end;
  TSVGViewArray = array of TSVGView;

  TSVGGradientStop = record
    Offset  : Double;
    Color   : TSVGColor;
    Opacity : Double;
    constructor Create(aOffset: Double; const aColor: TSVGColor;
      aOpacity: Double);
    // The stop colour, with its opacity put into the alpha channel.
    function EffectiveColor: TSVGColor;
    // The offset, colour and opacity as text.
    function ToString: String;
  end;
  TSVGGradientStopArray = array of TSVGGradientStop;

  { A linear or radial gradient. Its coordinates are in the space given
    by Units. }
  TSVGGradient = record
    Kind      : TSVGPaintServerKind;
    Mixing    : TSVGColorInterpolation;
    Units     : TSVGGradientUnits;
    Spread    : TSVGSpreadMethod;
    Transform : TSVGMatrix;
    First     : TSVGPoint;
    Second    : TSVGPoint;
    Focus     : TSVGPoint;
    Radius    : Double;
    Stops     : TSVGGradientStopArray;
    // A linear gradient running between two points.
    class function CreateLinear(const aStart, aEnd: TSVGPoint): TSVGGradient; static;
    // A radial gradient of the given centre, radius and focus.
    class function CreateRadial(const aCentre: TSVGPoint; aRadius: Double;
      const aFocus: TSVGPoint): TSVGGradient; static;
    // Appends a stop. The caller adds stops in order of rising offset.
    procedure AddStop(const aStop: TSVGGradientStop);
    // Moves a focus that lies outside the circle back onto its edge.
    procedure ClampFocus;
    // Maps a gradient parameter into the range 0 to 1, using the spread method.
    function ApplySpread(aOffset: Double): Double;
    // The colour at a gradient parameter, after the spread and the stops.
    // Channels mix in the space given by Mixing. Alpha always mixes linearly.
    function ColorAt(aOffset: Double): TSVGColor;
    // The gradient parameter of a point that is already in gradient space.
    function OffsetAt(const aPoint: TSVGPoint): Double;
    // Maps the unit square onto a bounding box, for object bounding box units.
    class function BoxTransform(const aBounds: TSVGRect): TSVGMatrix; static;
    // True when the gradient has at least one stop to paint with.
    function HasStops: Boolean;
    // The geometry, spread, units and stops as text.
    function ToString: String;
  end;

  { Gradient or pattern that a paint refers to; resolved by fpsvg.style. }
  ISVGPaintServer = interface
    // Kind of server, for backends that special-case gradients.
    function GetPaintServerKind: TSVGPaintServerKind;
    // Stable identifier of the server, unique within a document.
    function GetPaintServerID: TSVGString;
    // Fills aGradient for a gradient server; False for anything else.
    function GetGradient(out aGradient: TSVGGradient): Boolean;
  end;

  TSVGPaint = record
    Kind     : TSVGPaintKind;
    Color    : TSVGColor;
    Server   : ISVGPaintServer;
    Fallback : TSVGPaintFallback;
    constructor CreateColor(const aColor: TSVGColor);
    constructor CreateServer(const aServer: ISVGPaintServer);
    // Paint drawing nothing.
    class function None: TSVGPaint; static;
    // The paint to use when the paint server draws nothing. A paint without server returns itself.
    function Resolved: TSVGPaint;
    // "none", a colour, or the server name and its fallback.
    function ToString: String;
  end;

  TSVGPen = record
    Width      : Double;
    Cap        : TSVGLineCap;
    Join       : TSVGLineJoin;
    MiterLimit : Double;
    Dashes     : TSVGDoubleArray;
    DashOffset : Double;
    constructor Create(aWidth: Double; aCap: TSVGLineCap; aJoin: TSVGLineJoin);
    { The default SVG stroke: one unit wide, butt caps, miter joins, and no dashes. }
    class function Default: TSVGPen; static;
    // True when a dash pattern is set.
    function IsDashed: Boolean;
    // Width, cap, join, miter limit and the dash pattern when set.
    function ToString: String;
  end;

  { Read-only pixel source handed to a backend for <image>. }
  ISVGImageSource = interface
    // Width in pixels.
    function GetWidth: Integer;
    // Height in pixels.
    function GetHeight: Integer;
    // Colour of a single pixel; result is undefined outside the bounds.
    function GetPixel(aX, aY: Integer): TSVGColor;
    // Copies aCount pixels of row aY starting at aX; False when unsupported.
    function GetRow(aY, aX, aCount: Integer; aDest: PSVGColor): Boolean;
  end;

  { Says which links have been followed before.
    Used to determine the :visited pseudo-class in <a> link tags }
  ISVGLinkHistory = interface
    { Whether the target of a link has been visited.
      The reference is as the document wrote it, to resolve against the base location. }
    function WasVisited(const aHRef, aBaseURI: String): Boolean;
  end;

  { Supplies the pixels for an image reference. }
  ISVGImageResolver = interface
    // The image for a reference, resolved against a base location.
    // Returns nil when the image cannot be read.
    function ResolveImage(const aHRef, aBaseURI: String): ISVGImageSource;
  end;

  { A run of code points. Text is read into this before it is laid out. }
  TSVGCodePointArray = array of Cardinal;

  ISVGFont = interface;
  TSVGFontArray = array of ISVGFont;

  TSVGPath = class;

  TSVGFontStyle = (fnNormal, fnItalic, fnOblique);
  TSVGFontVariant = (fvNormal, fvSmallCaps);

  { The generic family names of CSS. None of them is a real font. }
  TSVGGenericFamily = (gfNone, gfSerif, gfSansSerif, gfCursive, gfFantasy,
                       gfMonospace);

  { How wide the letters of a face are drawn, narrowest first. A face is
    fsNormal unless it says otherwise. }
  TSVGFontStretch = (fsUltraCondensed, fsExtraCondensed, fsCondensed,
    fsSemiCondensed, fsNormal, fsSemiExpanded, fsExpanded, fsExtraExpanded,
    fsUltraExpanded);

  { A request for a font: the family list, the size and the font
    properties. }
  TSVGFontRequest = record
    Families : TSVGString;
    Size     : Double;
    Weight   : Integer;
    Style    : TSVGFontStyle;
    Variant  : TSVGFontVariant;
    Stretch  : TSVGFontStretch;
    constructor Create(const aFamilies: TSVGString; aSize: Double);
    // The families, the size and the properties as text.
    function ToString: String;
  end;

  { A face resolved at one size.
    Every measure it reports is in user units at that size,
    not in the design units of the underlying face. }
  ISVGFont = interface
    // Family name as resolved, for diagnostics and tracing.
    function GetFontName: TSVGString;
    // Design units per em of the underlying face.
    function GetUnitsPerEm: Integer;
    // Size the face was resolved at, in user units.
    function GetSize: Double;
    // Height of the face above the baseline.
    function GetAscent: Double;
    // Depth of the face below the baseline, as a positive number.
    function GetDescent: Double;
    // Where the face puts an underline, as the distance from the baseline
    // down to the top of it, and how thick it draws it.
    // Zero for both means the caller decides where to put it.
    function GetUnderlinePosition: Double;
    function GetUnderlineThickness: Double;
    // Glyph the face holds for a code point; zero when it holds none.
    function GetGlyphIndex(aCodePoint: Cardinal): Cardinal;
    // The glyph the face sets for the code points from aFrom on.
    // aCount returns how many code points were consumed.
    // When aCount >1, it means the face had a ligature glyph for it.
    // When aCount = 0, the face has no corresponding glyph.
    function GetGlyphForRun(const aCodes: TSVGCodePointArray; aFrom: Integer;
      out aCount: Integer): Cardinal;
    // Index of a named glyph (as in glyphRef, altGlyph). Zero when none is found.
    function GetGlyphNamed(const aName: TSVGString): Cardinal;
    // Horizontal advance of a glyph.
    function GetGlyphAdvance(aGlyph: Cardinal): Double;
    // How far the pen moves down for a glyph in text running down the page.
    // One em when the face does not specify it.
    function GetGlyphVerticalAdvance(aGlyph: Cardinal): Double;
    // Offset from the pen to the outline of a glyph, for text running down
    // the page, in user units with y running down.
    // When the face does not specify it, the glyph is centred on the
    // baseline and hung from the ascent.
    procedure GetGlyphVerticalOrigin(aGlyph: Cardinal; out aX, aY: Double);
    // How much closer together a pair of glyphs is drawn than their
    // advances alone would place them. Zero when the face has no kerning.
    function GetGlyphKerning(aLeft, aRight: Cardinal): Double;
    // The same for a pair drawn one above the other in a column.
    function GetGlyphVerticalKerning(aAbove, aBelow: Cardinal): Double;
    // True when the face has real small capitals for its lower case.
    function GetSmallCaps: Boolean;
    // Appends the outline of a glyph placed at the origin, with y running
    // down as it does in user space; False when the glyph has no outline.
    function GetGlyphOutline(aGlyph: Cardinal; aPath: TSVGPath): Boolean;
  end;

  { Finds a font file for a character that the requested face does not
    have. }
  ISVGFontCoverage = interface
    // Return a font file name for a font holding a glyph for the code point.
    // aRequest is the font the element requested. Empty when no file is found.
    function CoverFor(aCodePoint: Cardinal;
      const aRequest: TSVGFontRequest): String;
  end;

  { Resolves a font request to a face. }
  ISVGFontProvider = interface
    // The face that best matches a request, or nil when there is none.
    function ResolveFont(const aRequest: TSVGFontRequest): ISVGFont;
    // A face that holds a glyph for a code point the requested face does
    // not cover, or nil when there is none.
    function ResolveCover(aCodePoint: Cardinal;
      const aRequest: TSVGFontRequest): ISVGFont;
    // Registers a font file under the given family, weight and style
    // instead of the ones the face itself declares. False when the file
    // holds no face that can be read.
    function AddFontResource(const aFamily: TSVGString; aWeight: Integer;
      aStyle: TSVGFontStyle; const aFileName: String): Boolean;
  end;

  { Font resolved by fpsvg.text. }
  TSVGFontHandle = ISVGFont;

  { The operation a filter primitive applies to the pixels it is given. }
  TSVGFilterKind = (fkFlood, fkGaussianBlur, fkOffset, fkMerge, fkComposite,
    fkColorMatrix, fkBlend, fkTile, fkImage, fkMorphology,
    fkComponentTransfer, fkTurbulence, fkDiffuseLighting,
    fkSpecularLighting, fkConvolveMatrix, fkDisplacementMap);

  TSVGIntegerArray = array of Integer;

  { The transfer function of one channel of a component transfer.
    Kind is the SVG number for identity, table, discrete, linear or gamma.
    Each kind uses only the fields that belong to it. }
  TSVGFilterTransfer = record
    Kind      : Integer;
    Table     : TSVGDoubleArray;
    Slope     : Double;
    Intercept : Double;
    Amplitude : Double;
    Exponent  : Double;
    Offset    : Double;
  end;
  TSVGFilterTransferArray = array[0..3] of TSVGFilterTransfer;

  { One primitive of a filter, resolved into user space.
    Inputs holds what it reads: the index of an earlier primitive in the
    chain, or one of the SVGFilterSource values.
    Numbers holds the values of the kind: two deviations for a blur, one
    step for an offset, four constants for a composite, twenty for a
    colour matrix. }
  TSVGFilterPrimitive = record
    Kind      : TSVGFilterKind;
    Inputs    : TSVGIntegerArray;
    Numbers   : TSVGDoubleArray;
    Colour    : TSVGColor;
    Opacity   : Double;
    Operation : Integer;
    // The box the primitive writes.
    // When it is empty, the whole region of the filter is written.
    Region    : TSVGRect;
    HasRegion : Boolean;
    // The transfer function of each channel, red first.
    Transfer  : TSVGFilterTransferArray;
    // The grid of a convolution: its width and height, the cell over the
    // pixel, the divisor of the sum, the value added to it, and whether
    // the alpha is left unchanged.
    Order     : array[0..1] of Integer;
    Target    : array[0..1] of Integer;
    Divisor   : Double;
    Bias      : Double;
    Preserve  : Boolean;
    // The pixels an image primitive draws, and how they are fitted into
    // its box. Nil for every other kind.
    Image     : ISVGImageSource;
    Ratio     : TSVGPreserveAspectRatio;
  end;
  TSVGFilterPrimitiveArray = array of TSVGFilterPrimitive;

  { A filter resolved for the element it is drawn on: the box it covers in
    user space, and the primitives that fill it. The last primitive is the
    one that is drawn.
    Linear is set when the primitives work on linear light instead of the
    values written in the document. }
  TSVGFilterChain = record
    Region     : TSVGRect;
    // Maps the region and the lengths of the primitives into the space
    // the backend draws in.
    CTM        : TSVGMatrix;
    Primitives : TSVGFilterPrimitiveArray;
    Linear     : Boolean;
    // The paints the element draws with, and their opacities. The
    // FillPaint and StrokePaint inputs are planes filled with them.
    // Bounds is the box a paint in object bounding box units is resolved
    // against: the box of the element, not of the plane.
    FillPaint      : TSVGPaint;
    StrokePaint    : TSVGPaint;
    FillOpacity    : Double;
    StrokeOpacity  : Double;
    Bounds         : TSVGRect;
    // True when an ancestor requested a background to be gathered.
    // The BackgroundImage and BackgroundAlpha inputs read it.
    Background     : Boolean;
  end;

  TSVGGlyph = record
    GlyphID : Cardinal;
    X, Y    : Double;
    Angle   : Double;
    // Horizontal scale of the glyph outline. One is the natural width,
    // and zero means the same.
    Stretch : Double;
    constructor Create(aGlyphID: Cardinal; aX, aY: Double);
    constructor CreateTurned(aGlyphID: Cardinal; aX, aY, aAngle: Double);
    // The glyph id, its position, its angle in degrees when set, and its
    // stretch when it is not one.
    function ToString: String;
  end;
  TSVGGlyphArray = array of TSVGGlyph;

  TSVGPathSegment = record
    Kind   : TSVGPathSegmentKind;
    Points : array[0..2] of TSVGPoint;
    // Number of points this kind of segment uses.
    function PointCount: Integer;
    // The segment keyword followed by the points it uses.
    function ToString: String;
  end;
  TSVGPathSegmentArray = array of TSVGPathSegment;

  { Sequence of subpaths in user space. Quadratics are stored as cubics. }
  TSVGPath = class
  private
    FSegments : TSVGPathSegmentArray;
    FCount    : Integer;
    FCurrent  : TSVGPoint;
    FStart    : TSVGPoint;
    FHasCurrent : Boolean;
    function GetSegment(aIndex: Integer): TSVGPathSegment;
    procedure Grow;
    procedure NeedCurrentPoint;
  public
    // Discards all segments.
    procedure Clear;
    // Starts a new subpath at the given point.
    procedure MoveTo(aX, aY: Double);
    // Appends a straight segment from the current point.
    procedure LineTo(aX, aY: Double);
    // Appends a cubic Bezier from the current point.
    procedure CubicTo(aC1X, aC1Y, aC2X, aC2Y, aX, aY: Double);
    // Appends a quadratic Bezier, converted exactly to a cubic.
    procedure QuadTo(aCX, aCY, aX, aY: Double);
    // Closes the current subpath and returns the current point to its start.
    procedure Close;
    // Replaces the contents with a copy of aPath.
    procedure Assign(aPath: TSVGPath);
    // True when the path holds no segments.
    function IsEmpty: Boolean;
    // Bounding box of every point, control points included. Empty for an
    // empty path.
    function ControlBounds: TSVGRect;
    // Number of segments.
    property SegmentCount: Integer read FCount;
    // Segment by index, in construction order.
    property Segments[aIndex: Integer]: TSVGPathSegment read GetSegment; default;
    // The point that the next segment starts from.
    property CurrentPoint: TSVGPoint read FCurrent;
  end;

  { Cursor over a string, reading the SVG number and list grammars. }
  TSVGScanner = record
    Text : TSVGString;
    Pos  : Integer;
    constructor Create(const aText: TSVGString);
    // True when nothing but the end of the text remains.
    function AtEnd: Boolean;
    // The character at the cursor, or #0 past the end.
    function Current: AnsiChar;
    // Advances past whitespace; True when any was skipped.
    function SkipWSP: Boolean;
    // Advances past whitespace and at most one comma.
    function SkipWSPComma: Boolean;
    // Advances past aChar when it is at the cursor.
    function SkipChar(aChar: AnsiChar): Boolean;
    // Reads one number of the SVG grammar.
    function ScanNumber(out aValue: Double): Boolean;
    // Reads a single-digit arc flag.
    function ScanFlag(out aValue: Boolean): Boolean;
    // Reads a run of ASCII letters, empty when none are at the cursor.
    function ScanName: TSVGString;
  end;

  TSVGImageLoadFunc = function(const aURL: String): ISVGImageSource;

// Restricts a value to a range.
function SVGClamp(const aValue, aLow, aHigh: Double): Double;
// True when the character is SVG whitespace.
function SVGIsWSP(aChar: AnsiChar): Boolean;
// Reads the code point at aIndex and steps past it. Returns the
// replacement character on a malformed sequence.
function SVGNextCodePoint(const aText: TSVGString;
  var aIndex: Integer): Cardinal;
// The family at aIndex of a comma separated font-family list, unquoted,
// stepping past it. Empty when the list is spent.
function SVGNextFamily(const aList: TSVGString;
  var aIndex: Integer): TSVGString;
// The colour of a CSS2 system name. False when the name is not one of
// them.
function SVGSystemColor(const aName: TSVGString; out aColor: TSVGColor): Boolean;
// Sets the colour of a CSS2 system name. False when the name is not one
// of them.
function SVGSetSystemColor(const aName: TSVGString;
  const aColor: TSVGColor): Boolean;
// Resets every CSS2 system name to its built-in colour.
procedure SVGResetSystemColors;
// Appends the outlines of a glyph run to a path, each glyph placed and
// rotated as its own entry specifies. False for an empty run, and when
// the font gave no outline for any glyph.
function SVGAppendGlyphRun(aFont: ISVGFont; const aGlyphs: TSVGGlyphArray;
  aTarget: TSVGPath): Boolean;
// The linear-light value of an sRGB channel, in the 0 to 1 range.
function SVGToLinear(aChannel: Word): Double;
// The sRGB channel nearest a linear-light value in the 0 to 1 range.
function SVGFromLinear(aValue: Double): Word;
// The generic family for a name. gfNone when the name is a real family.
function SVGGenericFamilyOf(const aName: TSVGString): TSVGGenericFamily;
// The generic family a well known font belongs to. gfNone when the name is
// not one of them.
function SVGFamilyKindOf(const aName: TSVGString): TSVGGenericFamily;
// The CSS name of a font-stretch value.
function SVGFontStretchName(aStretch: TSVGFontStretch): String;
// The font-stretch value for a CSS name. fsNormal when the name is not
// one of them.
function SVGFontStretchOf(const aName: TSVGString): TSVGFontStretch;
// Formats a number with at most 6 decimals, locale-independent, without -0.
function SVGFormatFloat(const aValue: Double): String;
{ Which of two texts sorts first, byte for byte: negative when the first
  does, positive when the second does, zero when they are the same. The
  compare of the RTL sorts a string of the platform, which is a different
  order and, in the unicode RTL, a different length. }
function SVGCompareText(const aFirst, aSecond: TSVGString): Integer;
{ The bytes a C library is handed a file name as: UTF-8 on Unix, the code
  page of the system on Windows. }
function SVGFileNameBytes(const aFileName: String): RawByteString;
// The file name a C library handed back, from those same bytes.
function SVGFileNameOfBytes(const aBytes: RawByteString): String;
// The text of a document, from bytes a C library wrote in UTF-8.
function SVGTextOfUTF8(const aBytes: RawByteString): TSVGString;

// Installs the routine that turns an <image> reference into pixels.
procedure SetSVGImageLoader(aLoader: TSVGImageLoadFunc);
// Resolves an <image> reference. Returns nil when no loader is installed.
function LoadSVGImage(const aURL: String): ISVGImageSource;

const
  SVGOpaque: Word = $FFFF;
  SVGFormatDecimals = 6;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.Math, fpsvg.strings;
{$ELSE FPC_DOTTEDUNITS}
uses math, fpsvg.strings;
{$ENDIF FPC_DOTTEDUNITS}

const
  SVGReplacementChar = $FFFD;

type
  TSVGColorName = record
    Name : TSVGString;
    RGB  : Cardinal;
  end;

const
{$i fpsvg.colors.inc}

  MatrixEpsilon = 1e-12;
  UnitSuffixes: array[TSVGLengthUnit] of String =
    ('', '%', 'em', 'ex', 'px', 'cm', 'mm', 'in', 'pt', 'pc');
  AlignNames: array[TSVGAspectAlign] of String =
    ('none', 'xMinYMin', 'xMidYMin', 'xMaxYMin',
     'xMinYMid', 'xMidYMid', 'xMaxYMid',
     'xMinYMax', 'xMidYMax', 'xMaxYMax');
  MeetOrSliceNames: array[TSVGMeetOrSlice] of String = ('meet', 'slice');
  LineCapNames: array[TSVGLineCap] of String = ('butt', 'round', 'square');
  LineJoinNames: array[TSVGLineJoin] of String = ('miter', 'round', 'bevel');
  ServerKindNames: array[TSVGPaintServerKind] of String =
    ('linear-gradient', 'radial-gradient', 'pattern');
  SegmentNames: array[TSVGPathSegmentKind] of String =
    ('moveto', 'lineto', 'cubicto', 'close');

var
  GImageLoader: TSVGImageLoadFunc = nil;

function SVGClamp(const aValue, aLow, aHigh: Double): Double;

begin
  if aValue < aLow then
    Result := aLow
  else if aValue > aHigh then
    Result := aHigh
  else
    Result := aValue;
end;


type
  TSVGSystemColorEntry = record
    Name    : TSVGString;
    Initial : Cardinal;
    Value   : TSVGColor;
    Chosen  : Boolean;
  end;

const
  SVGSystemColorCount = 28;
  { Default values for the CSS2 system names. CSS2 leaves the real values
    to the system, so this table is only a fallback: names that alias one
    another share a value, the shadows run from dark to light, and every
    text colour contrasts with its background.
    Use SVGSetSystemColor for the values of a real desktop. }
  SVGSystemColorDefaults: array[0..SVGSystemColorCount - 1] of
    record Name: TSVGString; RGB: Cardinal; end = (
    (Name: 'activeborder'; RGB: $D4D0C8),
    (Name: 'activecaption'; RGB: $000080),
    (Name: 'appworkspace'; RGB: $808080),
    (Name: 'background'; RGB: $008080),
    (Name: 'buttonface'; RGB: $D4D0C8),
    (Name: 'buttonhighlight'; RGB: $FFFFFF),
    (Name: 'buttonshadow'; RGB: $808080),
    (Name: 'buttontext'; RGB: $000000),
    (Name: 'captiontext'; RGB: $FFFFFF),
    (Name: 'graytext'; RGB: $808080),
    (Name: 'highlight'; RGB: $000080),
    (Name: 'highlighttext'; RGB: $FFFFFF),
    (Name: 'inactiveborder'; RGB: $D4D0C8),
    (Name: 'inactivecaption'; RGB: $808080),
    (Name: 'inactivecaptiontext'; RGB: $D4D0C8),
    (Name: 'infobackground'; RGB: $FFFFE1),
    (Name: 'infotext'; RGB: $000000),
    (Name: 'menu'; RGB: $D4D0C8),
    (Name: 'menutext'; RGB: $000000),
    (Name: 'scrollbar'; RGB: $D4D0C8),
    (Name: 'threeddarkshadow'; RGB: $404040),
    (Name: 'threedface'; RGB: $D4D0C8),
    (Name: 'threedhighlight'; RGB: $FFFFFF),
    (Name: 'threedlightshadow'; RGB: $D4D0C8),
    (Name: 'threedshadow'; RGB: $808080),
    (Name: 'window'; RGB: $FFFFFF),
    (Name: 'windowframe'; RGB: $000000),
    (Name: 'windowtext'; RGB: $000000));

var
  SVGSystemColors: array[0..SVGSystemColorCount - 1] of TSVGColor;
  SVGSystemColorSet: Boolean = False;

// Fills the table from the built-in colours on first use.
procedure NeedSystemColors;

var
  I: Integer;

begin
  if SVGSystemColorSet then
    Exit;
  for I := 0 to SVGSystemColorCount - 1 do
    SVGSystemColors[I] := TSVGColor.FromBytes(
      SVGSystemColorDefaults[I].RGB shr 16,
      (SVGSystemColorDefaults[I].RGB shr 8) and $FF,
      SVGSystemColorDefaults[I].RGB and $FF, 255);
  SVGSystemColorSet := True;
end;


// Index of a system name in the table, or -1.
function SystemColorIndex(const aName: TSVGString): Integer;

var
  I: Integer;
  lName: String;

begin
  Result := -1;
  lName := LowerCase(Trim(aName));
  for I := 0 to SVGSystemColorCount - 1 do
    if SVGSystemColorDefaults[I].Name = lName then
      Exit(I);
end;


function SVGSystemColor(const aName: TSVGString; out aColor: TSVGColor): Boolean;

var
  lIndex: Integer;

begin
  aColor := TSVGColor.Transparent;
  lIndex := SystemColorIndex(aName);
  Result := lIndex >= 0;
  if not Result then
    Exit;
  NeedSystemColors;
  aColor := SVGSystemColors[lIndex];
end;


function SVGSetSystemColor(const aName: TSVGString;
  const aColor: TSVGColor): Boolean;

var
  lIndex: Integer;

begin
  lIndex := SystemColorIndex(aName);
  Result := lIndex >= 0;
  if not Result then
    Exit;
  NeedSystemColors;
  SVGSystemColors[lIndex] := aColor;
end;


procedure SVGResetSystemColors;

begin
  SVGSystemColorSet := False;
  NeedSystemColors;
end;


function SVGNextCodePoint(const aText: TSVGString;
  var aIndex: Integer): Cardinal;

var
  lByte, lExtra, I: Integer;

begin
  Result := SVGReplacementChar;
  if (aIndex < 1) or (aIndex > Length(aText)) then
    Exit;
  lByte := Ord(aText[aIndex]);
  Inc(aIndex);
  if lByte < $80 then
    Exit(lByte);
  if (lByte and $E0) = $C0 then
    begin
    Result := lByte and $1F;
    lExtra := 1;
    end
  else if (lByte and $F0) = $E0 then
    begin
    Result := lByte and $0F;
    lExtra := 2;
    end
  else if (lByte and $F8) = $F0 then
    begin
    Result := lByte and $07;
    lExtra := 3;
    end
  else
    Exit(SVGReplacementChar);
  for I := 1 to lExtra do
    begin
    if (aIndex > Length(aText)) or ((Ord(aText[aIndex]) and $C0) <> $80) then
      Exit(SVGReplacementChar);
    Result := (Result shl 6) or (Ord(aText[aIndex]) and $3F);
    Inc(aIndex);
    end;
end;


// The family at aIndex of a comma separated font-family list, unquoted.
function SVGNextFamily(const aList: TSVGString;
  var aIndex: Integer): TSVGString;

var
  lStart: Integer;

begin
  Result := '';
  while (aIndex <= Length(aList)) and (aList[aIndex] = ',') do
    Inc(aIndex);
  lStart := aIndex;
  while (aIndex <= Length(aList)) and (aList[aIndex] <> ',') do
    Inc(aIndex);
  Result := Trim(Copy(aList, lStart, aIndex - lStart));
  if Length(Result) >= 2 then
    if ((Result[1] = '"') and (Result[Length(Result)] = '"'))
       or ((Result[1] = '''') and (Result[Length(Result)] = '''')) then
      Result := Copy(Result, 2, Length(Result) - 2);
end;


// Appends the outline of one glyph to a path, placed and rotated as the
// glyph itself specifies.
procedure AppendOneGlyph(aOutline: TSVGPath; const aGlyph: TSVGGlyph;
  aTarget: TSVGPath);

var
  I: Integer;
  lSegment: TSVGPathSegment;
  lPlace: TSVGMatrix;
  lA, lB, lC: TSVGPoint;

begin
  lPlace := TSVGMatrix.Translation(aGlyph.X, aGlyph.Y);
  if aGlyph.Angle <> 0 then
    lPlace := TSVGMatrix.Rotation(aGlyph.Angle).Compose(lPlace);
  // The outline widens around the origin, before it is moved to the
  // position of the glyph.
  if (aGlyph.Stretch > 0) and (aGlyph.Stretch <> 1) then
    lPlace := TSVGMatrix.Scaling(aGlyph.Stretch, 1).Compose(lPlace);
  for I := 0 to aOutline.SegmentCount - 1 do
    begin
    lSegment := aOutline[I];
    case lSegment.Kind of
      skMoveTo:
        begin
        lA := lPlace.Transform(lSegment.Points[0]);
        aTarget.MoveTo(lA.X, lA.Y);
        end;
      skLineTo:
        begin
        lA := lPlace.Transform(lSegment.Points[0]);
        aTarget.LineTo(lA.X, lA.Y);
        end;
      skCubicTo:
        begin
        lA := lPlace.Transform(lSegment.Points[0]);
        lB := lPlace.Transform(lSegment.Points[1]);
        lC := lPlace.Transform(lSegment.Points[2]);
        aTarget.CubicTo(lA.X, lA.Y, lB.X, lB.Y, lC.X, lC.Y);
        end;
      skClose:
        aTarget.Close;
    end;
    end;
end;


function SVGAppendGlyphRun(aFont: ISVGFont; const aGlyphs: TSVGGlyphArray;
  aTarget: TSVGPath): Boolean;

var
  I: Integer;
  lOutline: TSVGPath;

begin
  Result := False;
  if (aFont = nil) or (aTarget = nil) then
    Exit;
  lOutline := TSVGPath.Create;
  try
    for I := 0 to High(aGlyphs) do
      begin
      lOutline.Clear;
      if not aFont.GetGlyphOutline(aGlyphs[I].GlyphID, lOutline) then
        Continue;
      AppendOneGlyph(lOutline, aGlyphs[I], aTarget);
      Result := True;
      end;
  finally
    lOutline.Free;
  end;
end;


function SVGToLinear(aChannel: Word): Double;

var
  lValue: Double;

begin
  lValue := aChannel / 65535;
  if lValue <= 0.04045 then
    Result := lValue / 12.92
  else
    Result := Power((lValue + 0.055) / 1.055, 2.4);
end;


function SVGFromLinear(aValue: Double): Word;

var
  lValue: Double;

begin
  if aValue <= 0.0031308 then
    lValue := aValue * 12.92
  else
    lValue := 1.055 * Power(aValue, 1 / 2.4) - 0.055;
  Result := Round(SVGClamp(lValue, 0, 1) * 65535);
end;


function SVGIsWSP(aChar: AnsiChar): Boolean;

begin
  Result := aChar in [#$20, #$09, #$0D, #$0A];
end;


function SVGCompareText(const aFirst, aSecond: TSVGString): Integer;

var
  lCount: Integer;

begin
  lCount := Length(aFirst);
  if Length(aSecond) < lCount then
    lCount := Length(aSecond);
  Result := 0;
  if lCount > 0 then
    Result := CompareByte(aFirst[1], aSecond[1], lCount);
  if Result = 0 then
    Result := Length(aFirst) - Length(aSecond);
end;


function SVGFileNameBytes(const aFileName: String): RawByteString;

{$IFDEF WINDOWS}
var
  lAnsi: AnsiString;
{$ENDIF}

begin
  {$IFDEF WINDOWS}
  // The calls that take a name as bytes read it in the code page of the
  // system, whatever the string of the RTL holds.
  lAnsi := AnsiString(aFileName);
  Result := lAnsi;
  {$ELSE}
  Result := UTF8Encode(aFileName);
  {$ENDIF}
end;


function SVGFileNameOfBytes(const aBytes: RawByteString): String;

{$IFDEF WINDOWS}
var
  lAnsi: AnsiString;
{$ENDIF}

begin
  {$IFDEF WINDOWS}
  lAnsi := aBytes;
  Result := String(lAnsi);
  {$ELSE}
  Result := String(UTF8String(aBytes));
  {$ENDIF}
end;


function SVGTextOfUTF8(const aBytes: RawByteString): TSVGString;

begin
  Result := aBytes;
end;


function SVGFormatFloat(const aValue: Double): String;

var
  I: Integer;

begin
  Str(aValue:0:SVGFormatDecimals, Result);
  I := Length(Result);
  while (I > 1) and (Result[I] = '0') do
    Dec(I);
  if (I > 1) and (Result[I] = '.') then
    Dec(I);
  SetLength(Result, I);
  if Result = '-0' then
    Result := '0';
end;


procedure SetSVGImageLoader(aLoader: TSVGImageLoadFunc);

begin
  GImageLoader := aLoader;
end;


function LoadSVGImage(const aURL: String): ISVGImageSource;

begin
  if Assigned(GImageLoader) then
    Result := GImageLoader(aURL)
  else
    Result := nil;
end;


{ TSVGColorHelper }

class function TSVGColorHelper.FromBytes(aRed, aGreen, aBlue,
  aAlpha: Byte): TSVGColor;

begin
  Result.Red := aRed * 257;
  Result.Green := aGreen * 257;
  Result.Blue := aBlue * 257;
  Result.Alpha := aAlpha * 257;
end;


class function TSVGColorHelper.Transparent: TSVGColor;

begin
  Result := TSVGColor.FromBytes(0, 0, 0, 0);
end;


class function TSVGColorHelper.Black: TSVGColor;

begin
  Result := TSVGColor.FromBytes(0, 0, 0, 255);
end;


class function TSVGColorHelper.FromName(const aName: TSVGString;
  out aColor: TSVGColor): Boolean;

var
  lLow, lHigh, lMiddle, lCompare: Integer;
  lName: TSVGString;

begin
  aColor := TSVGColor.Transparent;
  // The table is sorted by byte and the search keeps to bytes: the
  // compare of a string of the RTL follows the locale instead.
  lName := LowerCase(Trim(aName));
  lLow := 0;
  lHigh := SVGColorNameCount - 1;
  while lLow <= lHigh do
    begin
    lMiddle := (lLow + lHigh) div 2;
    lCompare := SVGCompareText(lName, SVGColorNames[lMiddle].Name);
    if lCompare = 0 then
      begin
      aColor := TSVGColor.FromBytes(SVGColorNames[lMiddle].RGB shr 16,
        (SVGColorNames[lMiddle].RGB shr 8) and $FF,
        SVGColorNames[lMiddle].RGB and $FF, 255);
      Exit(True);
      end;
    if lCompare < 0 then
      lHigh := lMiddle - 1
    else
      lLow := lMiddle + 1;
    end;
  Result := SVGSystemColor(lName, aColor);
end;


// Reads one rgb() component: a number, or a percentage of 255.
function ScanColorComponent(var aScanner: TSVGScanner; out aValue: Byte): Boolean;

var
  lValue: Double;

begin
  aValue := 0;
  aScanner.SkipWSPComma;
  Result := aScanner.ScanNumber(lValue);
  if not Result then
    Exit;
  if aScanner.SkipChar('%') then
    lValue := lValue * 255 / 100;
  aValue := Round(SVGClamp(lValue, 0, 255));
end;


// Reads the alpha of rgba(), which is a fraction between 0 and 1.
function ScanColorAlpha(var aScanner: TSVGScanner; out aValue: Byte): Boolean;

var
  lValue: Double;

begin
  aValue := 255;
  aScanner.SkipWSPComma;
  Result := aScanner.ScanNumber(lValue);
  if not Result then
    Exit;
  if aScanner.SkipChar('%') then
    lValue := lValue / 100;
  aValue := Round(SVGClamp(lValue * 255, 0, 255));
end;


// Expands a hex digit to a byte by repeating it, as #rgb needs.
function HexDigitValue(aChar: AnsiChar; out aValue: Integer): Boolean;

begin
  aValue := 0;
  case aChar of
    '0'..'9': aValue := Ord(aChar) - Ord('0');
    'a'..'f': aValue := Ord(aChar) - Ord('a') + 10;
    'A'..'F': aValue := Ord(aChar) - Ord('A') + 10;
  else
    Exit(False);
  end;
  Result := True;
end;


// Parses #rgb, #rrggbb and their eight-digit forms.
function ParseHexColor(const aText: String; out aColor: TSVGColor): Boolean;

var
  lDigits: array[0..7] of Integer;
  I, lCount: Integer;

begin
  aColor := TSVGColor.Transparent;
  lCount := Length(aText) - 1;
  Result := lCount in [3, 4, 6, 8];
  if not Result then
    Exit;
  for I := 0 to lCount - 1 do
    if not HexDigitValue(aText[I + 2], lDigits[I]) then
      Exit(False);
  case lCount of
    3, 4:
      begin
      if lCount = 3 then
        lDigits[3] := 15;
      for I := 0 to 3 do
        lDigits[I] := lDigits[I] * 17;
      aColor := TSVGColor.FromBytes(lDigits[0], lDigits[1], lDigits[2],
        lDigits[3]);
      end;
    6, 8:
      begin
      if lCount = 6 then
        begin
        lDigits[6] := 15;
        lDigits[7] := 15;
        end;
      aColor := TSVGColor.FromBytes(lDigits[0] * 16 + lDigits[1],
        lDigits[2] * 16 + lDigits[3], lDigits[4] * 16 + lDigits[5],
        lDigits[6] * 16 + lDigits[7]);
      end;
  end;
end;


function TSVGColorHelper.TryParse(const aText: TSVGString): Boolean;

var
  lText, lName: String;
  lScanner: TSVGScanner;
  lRed, lGreen, lBlue, lAlpha: Byte;
  lParsed: TSVGColor;

begin
  lText := Trim(aText);
  if lText = '' then
    Exit(False);
  if lText[1] = '#' then
    begin
    Result := ParseHexColor(lText, lParsed);
    if Result then
      Self := lParsed;
    Exit;
    end;
  lScanner := TSVGScanner.Create(lText);
  lName := LowerCase(lScanner.ScanName);
  if (lName = 'rgb') or (lName = 'rgba') then
    begin
    lScanner.SkipWSP;
    if not lScanner.SkipChar('(') then
      Exit(False);
    if not (ScanColorComponent(lScanner, lRed)
            and ScanColorComponent(lScanner, lGreen)
            and ScanColorComponent(lScanner, lBlue)) then
      Exit(False);
    lAlpha := 255;
    if (lName = 'rgba') and not ScanColorAlpha(lScanner, lAlpha) then
      Exit(False);
    lScanner.SkipWSPComma;
    Result := lScanner.SkipChar(')') and lScanner.AtEnd;
    if Result then
      Self := TSVGColor.FromBytes(lRed, lGreen, lBlue, lAlpha);
    Exit;
    end;
  Result := TSVGColor.FromName(lText, lParsed);
  if Result then
    Self := lParsed;
end;


function TSVGColorHelper.IsEightBit: Boolean;

begin
  Result := ((Red mod 257) = 0) and ((Green mod 257) = 0)
        and ((Blue mod 257) = 0) and ((Alpha mod 257) = 0);
end;


function TSVGColorHelper.ToString: String;

begin
  if IsEightBit then
    Result := LowerCase(Format('#%.2x%.2x%.2x%.2x',
      [Red div 257, Green div 257, Blue div 257, Alpha div 257]))
  else
    Result := LowerCase(Format('#%.4x%.4x%.4x%.4x', [Red, Green, Blue, Alpha]));
end;


{ TSVGPoint }

constructor TSVGPoint.Create(aX, aY: Double);

begin
  X := aX;
  Y := aY;
end;


function TSVGPoint.ToString: String;

begin
  Result := SVGFormatFloat(X) + ' ' + SVGFormatFloat(Y);
end;


{ TSVGMatrix }

constructor TSVGMatrix.Create(aA, aB, aC, aD, aE, aF: Double);

begin
  a := aA;
  b := aB;
  c := aC;
  d := aD;
  e := aE;
  f := aF;
end;


class function TSVGMatrix.Identity: TSVGMatrix;

begin
  Result := TSVGMatrix.Create(1, 0, 0, 1, 0, 0);
end;


class function TSVGMatrix.Translation(aTX, aTY: Double): TSVGMatrix;

begin
  Result := TSVGMatrix.Create(1, 0, 0, 1, aTX, aTY);
end;


class function TSVGMatrix.Scaling(aSX, aSY: Double): TSVGMatrix;

begin
  Result := TSVGMatrix.Create(aSX, 0, 0, aSY, 0, 0);
end;


class function TSVGMatrix.Rotation(aDegrees: Double): TSVGMatrix;

var
  lSin, lCos: Double;

begin
  SinCos(DegToRad(aDegrees), lSin, lCos);
  Result := TSVGMatrix.Create(lCos, lSin, -lSin, lCos, 0, 0);
end;


class function TSVGMatrix.RotationAbout(aDegrees, aCX, aCY: Double): TSVGMatrix;

begin
  Result := TSVGMatrix.Translation(-aCX, -aCY)
    .Compose(TSVGMatrix.Rotation(aDegrees))
    .Compose(TSVGMatrix.Translation(aCX, aCY));
end;


class function TSVGMatrix.SkewingX(aDegrees: Double): TSVGMatrix;

begin
  Result := TSVGMatrix.Create(1, 0, Tan(DegToRad(aDegrees)), 1, 0, 0);
end;


class function TSVGMatrix.SkewingY(aDegrees: Double): TSVGMatrix;

begin
  Result := TSVGMatrix.Create(1, Tan(DegToRad(aDegrees)), 0, 1, 0, 0);
end;


function TSVGMatrix.IsIdentity: Boolean;

begin
  Result := (a = 1) and (b = 0) and (c = 0)
        and (d = 1) and (e = 0) and (f = 0);
end;


function TSVGMatrix.Determinant: Double;

begin
  Result := a * d - b * c;
end;


function TSVGMatrix.MaxScale: Double;

begin
  Result := Sqrt(Max(a * a + b * b, c * c + d * d));
end;


function TSVGMatrix.Compose(const aSecond: TSVGMatrix): TSVGMatrix;

begin
  Result.a := aSecond.a * a + aSecond.c * b;
  Result.b := aSecond.b * a + aSecond.d * b;
  Result.c := aSecond.a * c + aSecond.c * d;
  Result.d := aSecond.b * c + aSecond.d * d;
  Result.e := aSecond.a * e + aSecond.c * f + aSecond.e;
  Result.f := aSecond.b * e + aSecond.d * f + aSecond.f;
end;


function TSVGMatrix.Invert(out aResult: TSVGMatrix): Boolean;

var
  lDet: Double;

begin
  lDet := Determinant;
  Result := Abs(lDet) > MatrixEpsilon;
  if not Result then
    Exit;
  aResult.a := d / lDet;
  aResult.b := -b / lDet;
  aResult.c := -c / lDet;
  aResult.d := a / lDet;
  aResult.e := (c * f - d * e) / lDet;
  aResult.f := (b * e - a * f) / lDet;
end;


function TSVGMatrix.Transform(const aPoint: TSVGPoint): TSVGPoint;

begin
  Result.X := a * aPoint.X + c * aPoint.Y + e;
  Result.Y := b * aPoint.X + d * aPoint.Y + f;
end;


function TSVGMatrix.TransformVector(const aPoint: TSVGPoint): TSVGPoint;

begin
  Result.X := a * aPoint.X + c * aPoint.Y;
  Result.Y := b * aPoint.X + d * aPoint.Y;
end;


function TSVGMatrix.ToString: String;

begin
  if IsIdentity then
    Exit('identity');
  Result := '[' + SVGFormatFloat(a) + ' ' + SVGFormatFloat(b)
    + ' ' + SVGFormatFloat(c) + ' ' + SVGFormatFloat(d)
    + ' ' + SVGFormatFloat(e) + ' ' + SVGFormatFloat(f) + ']';
end;


{ TSVGRect }

constructor TSVGRect.Create(aLeft, aTop, aRight, aBottom: Double);

begin
  Left := aLeft;
  Top := aTop;
  Right := aRight;
  Bottom := aBottom;
end;


constructor TSVGRect.CreateSize(aX, aY, aWidth, aHeight: Double);

begin
  Left := aX;
  Top := aY;
  Right := aX + aWidth;
  Bottom := aY + aHeight;
end;


class function TSVGRect.Empty: TSVGRect;

begin
  Result := TSVGRect.Create(Infinity, Infinity, NegInfinity, NegInfinity);
end;


function TSVGRect.IsEmpty: Boolean;

begin
  Result := (Right <= Left) or (Bottom <= Top);
end;


function TSVGRect.Width: Double;

begin
  if IsEmpty then
    Result := 0
  else
    Result := Right - Left;
end;


function TSVGRect.Height: Double;

begin
  if IsEmpty then
    Result := 0
  else
    Result := Bottom - Top;
end;


function TSVGRect.Union(const aOther: TSVGRect): TSVGRect;

begin
  if IsEmpty then
    Result := aOther
  else if aOther.IsEmpty then
    Result := Self
  else
    Result := TSVGRect.Create(Min(Left, aOther.Left), Min(Top, aOther.Top),
      Max(Right, aOther.Right), Max(Bottom, aOther.Bottom));
end;


function TSVGRect.Intersect(const aOther: TSVGRect): TSVGRect;

begin
  Result := TSVGRect.Create(Max(Left, aOther.Left), Max(Top, aOther.Top),
    Min(Right, aOther.Right), Min(Bottom, aOther.Bottom));
  if Result.IsEmpty then
    Result := TSVGRect.Empty;
end;


function TSVGRect.Transform(const aMatrix: TSVGMatrix): TSVGRect;

var
  lCorner: array[0..3] of TSVGPoint;
  I: Integer;

begin
  if IsEmpty then
    Exit(TSVGRect.Empty);
  lCorner[0] := aMatrix.Transform(TSVGPoint.Create(Left, Top));
  lCorner[1] := aMatrix.Transform(TSVGPoint.Create(Right, Top));
  lCorner[2] := aMatrix.Transform(TSVGPoint.Create(Right, Bottom));
  lCorner[3] := aMatrix.Transform(TSVGPoint.Create(Left, Bottom));
  Result := TSVGRect.Create(lCorner[0].X, lCorner[0].Y, lCorner[0].X, lCorner[0].Y);
  for I := 1 to 3 do
    begin
    Result.Left := Min(Result.Left, lCorner[I].X);
    Result.Top := Min(Result.Top, lCorner[I].Y);
    Result.Right := Max(Result.Right, lCorner[I].X);
    Result.Bottom := Max(Result.Bottom, lCorner[I].Y);
    end;
end;


function TSVGRect.UnitSquareTransform: TSVGMatrix;

begin
  if IsEmpty then
    Result := TSVGMatrix.Identity
  else
    Result := TSVGMatrix.Create(Width, 0, 0, Height, Left, Top);
end;


function TSVGRect.PercentBase(aAxis: TSVGLengthAxis): Double;

begin
  case aAxis of
    laHorizontal: Result := Width;
    laVertical: Result := Height;
    laDiagonal: Result := Sqrt(Width * Width + Height * Height) / Sqrt(2);
  end;
end;


function TSVGRect.ToString: String;

begin
  if IsEmpty then
    Exit('empty');
  Result := '[' + SVGFormatFloat(Left) + ' ' + SVGFormatFloat(Top)
    + ' ' + SVGFormatFloat(Right) + ' ' + SVGFormatFloat(Bottom) + ']';
end;


{ TSVGLength }

constructor TSVGLength.Create(aValue: Double; aUnit: TSVGLengthUnit);

begin
  Value := aValue;
  LengthUnit := aUnit;
end;


class function TSVGLength.Zero: TSVGLength;

begin
  Result := TSVGLength.Create(0, luNumber);
end;


function TSVGLength.IsAbsolute: Boolean;

begin
  Result := not (LengthUnit in [luPercent, luEm, luEx]);
end;


function TSVGLength.Resolve(aPercentBase, aFontSize, aXHeight,
  aDPI: Double): Double;

begin
  case LengthUnit of
    luNumber, luPx: Result := Value;
    luPercent: Result := Value * aPercentBase / 100;
    luEm: Result := Value * aFontSize;
    luEx: Result := Value * aXHeight;
    luIn: Result := Value * aDPI;
    luCm: Result := Value * aDPI / 2.54;
    luMm: Result := Value * aDPI / 25.4;
    luPt: Result := Value * aDPI / 72;
    luPc: Result := Value * aDPI / 6;
  end;
end;


function TSVGLength.ToString: String;

begin
  Result := SVGFormatFloat(Value) + UnitSuffixes[LengthUnit];
end;


{ TSVGLengthContext }

constructor TSVGLengthContext.Create(const aViewport: TSVGRect);

begin
  Viewport := aViewport;
  FontSize := 16;
  XHeight := 8;
  DPI := 96;
end;


class function TSVGLengthContext.Default: TSVGLengthContext;

begin
  Result := TSVGLengthContext.Create(TSVGRect.CreateSize(0, 0, 100, 100));
end;


function TSVGLengthContext.Resolve(const aLength: TSVGLength;
  aAxis: TSVGLengthAxis): Double;

begin
  Result := aLength.Resolve(Viewport.PercentBase(aAxis), FontSize, XHeight, DPI);
end;


{ TSVGPreserveAspectRatio }

constructor TSVGPreserveAspectRatio.Create(aAlign: TSVGAspectAlign;
  aMeetOrSlice: TSVGMeetOrSlice);

begin
  Align := aAlign;
  MeetOrSlice := aMeetOrSlice;
end;


class function TSVGPreserveAspectRatio.Default: TSVGPreserveAspectRatio;

begin
  Result := TSVGPreserveAspectRatio.Create(paXMidYMid, msMeet);
end;


function TSVGPreserveAspectRatio.ViewBoxTransform(const aViewBox,
  aViewport: TSVGRect): TSVGMatrix;

var
  lBoxWidth, lBoxHeight, lPortWidth, lPortHeight: Double;
  lScaleX, lScaleY, lTX, lTY: Double;

begin
  lBoxWidth := aViewBox.Width;
  lBoxHeight := aViewBox.Height;
  lPortWidth := aViewport.Width;
  lPortHeight := aViewport.Height;
  if (lBoxWidth <= 0) or (lBoxHeight <= 0)
     or (lPortWidth <= 0) or (lPortHeight <= 0) then
    Exit(TSVGMatrix.Identity);
  lScaleX := lPortWidth / lBoxWidth;
  lScaleY := lPortHeight / lBoxHeight;
  if Align <> paNone then
    begin
    if MeetOrSlice = msMeet then
      lScaleX := Min(lScaleX, lScaleY)
    else
      lScaleX := Max(lScaleX, lScaleY);
    lScaleY := lScaleX;
    end;
  lTX := aViewport.Left - aViewBox.Left * lScaleX;
  lTY := aViewport.Top - aViewBox.Top * lScaleY;
  case Align of
    paXMidYMin, paXMidYMid, paXMidYMax:
      lTX := lTX + (lPortWidth - lBoxWidth * lScaleX) / 2;
    paXMaxYMin, paXMaxYMid, paXMaxYMax:
      lTX := lTX + (lPortWidth - lBoxWidth * lScaleX);
  end;
  case Align of
    paXMinYMid, paXMidYMid, paXMaxYMid:
      lTY := lTY + (lPortHeight - lBoxHeight * lScaleY) / 2;
    paXMinYMax, paXMidYMax, paXMaxYMax:
      lTY := lTY + (lPortHeight - lBoxHeight * lScaleY);
  end;
  Result := TSVGMatrix.Create(lScaleX, 0, 0, lScaleY, lTX, lTY);
end;


function TSVGPreserveAspectRatio.ToString: String;

begin
  Result := AlignNames[Align];
  if (Align <> paNone) and (MeetOrSlice <> msMeet) then
    Result := Result + ' ' + MeetOrSliceNames[MeetOrSlice];
end;


{ TSVGView }

class function TSVGView.None: TSVGView;

begin
  Result.Name := '';
  Result.ViewBox := TSVGRect.Empty;
  Result.HasViewBox := False;
  Result.Ratio := TSVGPreserveAspectRatio.Default;
  Result.HasRatio := False;
  Result.Transform := TSVGMatrix.Identity;
  Result.HasTransform := False;
end;


{ TSVGGradientStop }

constructor TSVGGradientStop.Create(aOffset: Double; const aColor: TSVGColor;
  aOpacity: Double);

begin
  Offset := SVGClamp(aOffset, 0, 1);
  Color := aColor;
  Opacity := SVGClamp(aOpacity, 0, 1);
end;


function TSVGGradientStop.EffectiveColor: TSVGColor;

begin
  Result := Color;
  Result.Alpha := Round(Result.Alpha * Opacity);
end;


function TSVGGradientStop.ToString: String;

begin
  Result := SVGFormatFloat(Offset) + ':' + Color.ToString;
  if Opacity < 1 then
    Result := Result + '@' + SVGFormatFloat(Opacity);
end;


{ TSVGGradient }

class function TSVGGradient.CreateLinear(const aStart,
  aEnd: TSVGPoint): TSVGGradient;

begin
  Result.Kind := pkLinearGradient;
  Result.Units := guObjectBoundingBox;
  Result.Spread := smPad;
  Result.Transform := TSVGMatrix.Identity;
  Result.First := aStart;
  Result.Second := aEnd;
  Result.Focus := aStart;
  Result.Radius := 0;
  Result.Stops := nil;
end;


class function TSVGGradient.CreateRadial(const aCentre: TSVGPoint;
  aRadius: Double; const aFocus: TSVGPoint): TSVGGradient;

begin
  Result.Kind := pkRadialGradient;
  Result.Units := guObjectBoundingBox;
  Result.Spread := smPad;
  Result.Transform := TSVGMatrix.Identity;
  Result.First := aCentre;
  Result.Second := aCentre;
  Result.Focus := aFocus;
  Result.Radius := aRadius;
  Result.Stops := nil;
end;


procedure TSVGGradient.AddStop(const aStop: TSVGGradientStop);

begin
  SetLength(Stops, Length(Stops) + 1);
  Stops[High(Stops)] := aStop;
end;


function TSVGGradient.HasStops: Boolean;

begin
  Result := Length(Stops) > 0;
end;


procedure TSVGGradient.ClampFocus;

var
  lDX, lDY, lDistance, lScale: Double;

begin
  if (Kind <> pkRadialGradient) or (Radius <= 0) then
    Exit;
  lDX := Focus.X - First.X;
  lDY := Focus.Y - First.Y;
  lDistance := Sqrt(lDX * lDX + lDY * lDY);
  if lDistance < Radius * 0.999 then
    Exit;
  if lDistance = 0 then
    Exit;
  lScale := Radius * 0.999 / lDistance;
  Focus := TSVGPoint.Create(First.X + lDX * lScale, First.Y + lDY * lScale);
end;


function TSVGGradient.ApplySpread(aOffset: Double): Double;

begin
  case Spread of
    smPad: Result := SVGClamp(aOffset, 0, 1);
    smRepeat:
      begin
      Result := aOffset - Floor(aOffset);
      if Result < 0 then
        Result := Result + 1;
      end;
    smReflect:
      begin
      Result := Abs(aOffset);
      Result := Result - 2 * Floor(Result / 2);
      if Result > 1 then
        Result := 2 - Result;
      end;
  end;
end;


function TSVGGradient.ColorAt(aOffset: Double): TSVGColor;

var
  I: Integer;
  lT, lSpan: Double;
  lLow, lHigh: TSVGColor;

begin
  if not HasStops then
    Exit(TSVGColor.Transparent);
  lT := ApplySpread(aOffset);
  if lT <= Stops[0].Offset then
    Exit(Stops[0].EffectiveColor);
  if lT >= Stops[High(Stops)].Offset then
    Exit(Stops[High(Stops)].EffectiveColor);
  for I := 0 to High(Stops) - 1 do
    if (lT >= Stops[I].Offset) and (lT <= Stops[I + 1].Offset) then
      begin
      lSpan := Stops[I + 1].Offset - Stops[I].Offset;
      if lSpan <= 0 then
        Exit(Stops[I + 1].EffectiveColor);
      lT := (lT - Stops[I].Offset) / lSpan;
      lLow := Stops[I].EffectiveColor;
      lHigh := Stops[I + 1].EffectiveColor;
      if Mixing = ciLinearRGB then
        begin
        Result.Red := SVGFromLinear(SVGToLinear(lLow.Red)
          + (SVGToLinear(lHigh.Red) - SVGToLinear(lLow.Red)) * lT);
        Result.Green := SVGFromLinear(SVGToLinear(lLow.Green)
          + (SVGToLinear(lHigh.Green) - SVGToLinear(lLow.Green)) * lT);
        Result.Blue := SVGFromLinear(SVGToLinear(lLow.Blue)
          + (SVGToLinear(lHigh.Blue) - SVGToLinear(lLow.Blue)) * lT);
        end
      else
        begin
        Result.Red := Round(lLow.Red + (lHigh.Red - lLow.Red) * lT);
        Result.Green := Round(lLow.Green + (lHigh.Green - lLow.Green) * lT);
        Result.Blue := Round(lLow.Blue + (lHigh.Blue - lLow.Blue) * lT);
        end;
      // Alpha is not a colour, so it mixes linearly in either space.
      Result.Alpha := Round(lLow.Alpha + (lHigh.Alpha - lLow.Alpha) * lT);
      Exit;
      end;
  Result := Stops[High(Stops)].EffectiveColor;
end;


function TSVGGradient.OffsetAt(const aPoint: TSVGPoint): Double;

var
  lDX, lDY, lLengthSquared: Double;
  lEX, lEY, lDot, lA, lC, lDiscriminant: Double;

begin
  if Kind = pkLinearGradient then
    begin
    lDX := Second.X - First.X;
    lDY := Second.Y - First.Y;
    lLengthSquared := lDX * lDX + lDY * lDY;
    if lLengthSquared <= 0 then
      Exit(1);
    Exit(((aPoint.X - First.X) * lDX + (aPoint.Y - First.Y) * lDY)
      / lLengthSquared);
    end;
  if Radius <= 0 then
    Exit(1);
  // The point lies on the circle with centre Focus + t*(First - Focus) and
  // radius t*Radius. Solving that quadratic for t gives the parameter.
  lDX := aPoint.X - Focus.X;
  lDY := aPoint.Y - Focus.Y;
  lEX := First.X - Focus.X;
  lEY := First.Y - Focus.Y;
  lA := lEX * lEX + lEY * lEY - Radius * Radius;
  lDot := lDX * lEX + lDY * lEY;
  lC := lDX * lDX + lDY * lDY;
  if lA = 0 then
    begin
    if lDot = 0 then
      Exit(0);
    Exit(lC / (2 * lDot));
    end;
  lDiscriminant := lDot * lDot - lA * lC;
  if lDiscriminant < 0 then
    Exit(1);
  Result := (lDot - Sqrt(lDiscriminant)) / lA;
end;


class function TSVGGradient.BoxTransform(const aBounds: TSVGRect): TSVGMatrix;

begin
  Result := aBounds.UnitSquareTransform;
end;


function TSVGGradient.ToString: String;

var
  I: Integer;

begin
  if Kind = pkLinearGradient then
    Result := 'linear ' + First.ToString + ' ' + Second.ToString
  else
    Result := 'radial ' + First.ToString + ' r=' + SVGFormatFloat(Radius)
      + ' focus=' + Focus.ToString;
  if Units = guUserSpaceOnUse then
    Result := Result + ' units=userSpaceOnUse'
  else
    Result := Result + ' units=objectBoundingBox';
  case Spread of
    smPad: Result := Result + ' spread=pad';
    smReflect: Result := Result + ' spread=reflect';
    smRepeat: Result := Result + ' spread=repeat';
  end;
  if not Transform.IsIdentity then
    Result := Result + ' transform=' + Transform.ToString;
  Result := Result + ' stops=[';
  for I := 0 to High(Stops) do
    begin
    if I > 0 then
      Result := Result + ' ';
    Result := Result + Stops[I].ToString;
    end;
  Result := Result + ']';
end;


{ TSVGPaint }

constructor TSVGPaint.CreateColor(const aColor: TSVGColor);

begin
  Kind := spColor;
  Color := aColor;
  Server := nil;
  Fallback := pfAbsent;
end;


constructor TSVGPaint.CreateServer(const aServer: ISVGPaintServer);

begin
  Kind := spServer;
  Color := TSVGColor.Transparent;
  Server := aServer;
  Fallback := pfAbsent;
end;


class function TSVGPaint.None: TSVGPaint;

begin
  Result.Kind := spNone;
  Result.Color := TSVGColor.Transparent;
  Result.Server := nil;
  Result.Fallback := pfAbsent;
end;


function TSVGPaint.Resolved: TSVGPaint;

begin
  Result := Self;
  if Kind <> spServer then
    Exit;
  case Fallback of
    pfNone: Result := TSVGPaint.None;
    pfColor: Result := TSVGPaint.CreateColor(Color);
  end;
end;


function TSVGPaint.ToString: String;

begin
  case Kind of
    spNone: Result := 'none';
    spColor: Result := 'color(' + Color.ToString + ')';
    spServer:
      begin
      if Server = nil then
        Result := 'server(nil)'
      else
        Result := 'server(' + ServerKindNames[Server.GetPaintServerKind]
          + ' "' + Server.GetPaintServerID + '")';
      case Fallback of
        pfNone: Result := Result + ' or none';
        pfColor: Result := Result + ' or ' + Color.ToString;
      end;
      end;
  end;
end;


{ TSVGPen }

constructor TSVGPen.Create(aWidth: Double; aCap: TSVGLineCap;
  aJoin: TSVGLineJoin);

begin
  Width := aWidth;
  Cap := aCap;
  Join := aJoin;
  MiterLimit := 4;
  Dashes := nil;
  DashOffset := 0;
end;


class function TSVGPen.Default: TSVGPen;

begin
  Result := TSVGPen.Create(1, lcButt, ljMiter);
end;


function TSVGPen.IsDashed: Boolean;

begin
  Result := Length(Dashes) > 0;
end;


function TSVGPen.ToString: String;

var
  I: Integer;

begin
  Result := 'width=' + SVGFormatFloat(Width)
    + ' cap=' + LineCapNames[Cap]
    + ' join=' + LineJoinNames[Join]
    + ' miter-limit=' + SVGFormatFloat(MiterLimit);
  if not IsDashed then
    Exit;
  Result := Result + ' dashes=[';
  for I := 0 to High(Dashes) do
    begin
    if I > 0 then
      Result := Result + ' ';
    Result := Result + SVGFormatFloat(Dashes[I]);
    end;
  Result := Result + '] dash-offset=' + SVGFormatFloat(DashOffset);
end;


{ TSVGGlyph }

constructor TSVGGlyph.Create(aGlyphID: Cardinal; aX, aY: Double);

begin
  GlyphID := aGlyphID;
  X := aX;
  Y := aY;
  Angle := 0;
  Stretch := 1;
end;


constructor TSVGGlyph.CreateTurned(aGlyphID: Cardinal; aX, aY, aAngle: Double);

begin
  GlyphID := aGlyphID;
  X := aX;
  Y := aY;
  Angle := aAngle;
  Stretch := 1;
end;


function TSVGGlyph.ToString: String;

begin
  Result := Format('glyph %d ', [GlyphID])
    + SVGFormatFloat(X) + ' ' + SVGFormatFloat(Y);
  if Angle <> 0 then
    Result := Result + ' ' + SVGFormatFloat(Angle);
  if (Stretch > 0) and (Stretch <> 1) then
    Result := Result + ' stretch ' + SVGFormatFloat(Stretch);
end;


{ TSVGPathSegment }

function TSVGPathSegment.PointCount: Integer;

begin
  case Kind of
    skMoveTo, skLineTo: Result := 1;
    skCubicTo: Result := 3;
  else
    Result := 0;
  end;
end;


function TSVGPathSegment.ToString: String;

var
  I: Integer;

begin
  Result := SegmentNames[Kind];
  for I := 0 to PointCount - 1 do
    Result := Result + ' ' + Points[I].ToString;
end;


{ TSVGScanner }

constructor TSVGScanner.Create(const aText: TSVGString);

begin
  Text := aText;
  Pos := 1;
end;


function TSVGScanner.AtEnd: Boolean;

begin
  Result := Pos > Length(Text);
end;


function TSVGScanner.Current: AnsiChar;

begin
  if AtEnd then
    Result := #0
  else
    Result := Text[Pos];
end;


function TSVGScanner.SkipWSP: Boolean;

var
  lStart: Integer;

begin
  lStart := Pos;
  while not AtEnd and SVGIsWSP(Text[Pos]) do
    Inc(Pos);
  Result := Pos > lStart;
end;


function TSVGScanner.SkipWSPComma: Boolean;

begin
  Result := SkipWSP;
  if not AtEnd and (Text[Pos] = ',') then
    begin
    Inc(Pos);
    SkipWSP;
    Result := True;
    end;
end;


function TSVGScanner.SkipChar(aChar: AnsiChar): Boolean;

begin
  Result := not AtEnd and (Text[Pos] = aChar);
  if Result then
    Inc(Pos);
end;


function TSVGScanner.ScanNumber(out aValue: Double): Boolean;

var
  lStart, lCode, lDigits: Integer;

begin
  aValue := 0;
  lStart := Pos;
  if not AtEnd and (Text[Pos] in ['+', '-']) then
    Inc(Pos);
  lDigits := 0;
  while not AtEnd and (Text[Pos] in ['0'..'9']) do
    begin
    Inc(Pos);
    Inc(lDigits);
    end;
  if not AtEnd and (Text[Pos] = '.') then
    begin
    Inc(Pos);
    while not AtEnd and (Text[Pos] in ['0'..'9']) do
      begin
      Inc(Pos);
      Inc(lDigits);
      end;
    end;
  if lDigits = 0 then
    begin
    Pos := lStart;
    Exit(False);
    end;
  if not AtEnd and (Text[Pos] in ['e', 'E']) then
    begin
    lCode := Pos;
    Inc(Pos);
    if not AtEnd and (Text[Pos] in ['+', '-']) then
      Inc(Pos);
    if not AtEnd and (Text[Pos] in ['0'..'9']) then
      while not AtEnd and (Text[Pos] in ['0'..'9']) do
        Inc(Pos)
    else
      Pos := lCode;
    end;
  Val(Copy(Text, lStart, Pos - lStart), aValue, lCode);
  Result := lCode = 0;
  if not Result then
    Pos := lStart;
end;


function TSVGScanner.ScanFlag(out aValue: Boolean): Boolean;

begin
  aValue := False;
  Result := not AtEnd and (Text[Pos] in ['0', '1']);
  if not Result then
    Exit;
  aValue := Text[Pos] = '1';
  Inc(Pos);
end;


function TSVGScanner.ScanName: TSVGString;

var
  lStart: Integer;

begin
  lStart := Pos;
  while not AtEnd and (Text[Pos] in ['a'..'z', 'A'..'Z']) do
    Inc(Pos);
  Result := Copy(Text, lStart, Pos - lStart);
end;


type
  { A font whose appearance is known without having the file. A document
    requesting one the system lacks falls back to a face of the same
    kind. }
  TSVGKnownFamily = record
    Name : String;
    Kind : TSVGGenericFamily;
  end;

const
  KnownFamilies: array[0..47] of TSVGKnownFamily = (
    (Name: 'arial'; Kind: gfSansSerif),
    (Name: 'arial black'; Kind: gfSansSerif),
    (Name: 'arial narrow'; Kind: gfSansSerif),
    (Name: 'calibri'; Kind: gfSansSerif),
    (Name: 'candara'; Kind: gfSansSerif),
    (Name: 'century gothic'; Kind: gfSansSerif),
    (Name: 'corbel'; Kind: gfSansSerif),
    (Name: 'franklin gothic medium'; Kind: gfSansSerif),
    (Name: 'futura'; Kind: gfSansSerif),
    (Name: 'geneva'; Kind: gfSansSerif),
    (Name: 'gill sans'; Kind: gfSansSerif),
    (Name: 'helvetica'; Kind: gfSansSerif),
    (Name: 'helvetica neue'; Kind: gfSansSerif),
    (Name: 'lucida grande'; Kind: gfSansSerif),
    (Name: 'lucida sans'; Kind: gfSansSerif),
    (Name: 'lucida sans unicode'; Kind: gfSansSerif),
    (Name: 'ms sans serif'; Kind: gfSansSerif),
    (Name: 'myriad'; Kind: gfSansSerif),
    (Name: 'optima'; Kind: gfSansSerif),
    (Name: 'segoe ui'; Kind: gfSansSerif),
    (Name: 'tahoma'; Kind: gfSansSerif),
    (Name: 'trebuchet ms'; Kind: gfSansSerif),
    (Name: 'verdana'; Kind: gfSansSerif),
    (Name: 'baskerville'; Kind: gfSerif),
    (Name: 'book antiqua'; Kind: gfSerif),
    (Name: 'bookman'; Kind: gfSerif),
    (Name: 'bookman old style'; Kind: gfSerif),
    (Name: 'cambria'; Kind: gfSerif),
    (Name: 'century schoolbook'; Kind: gfSerif),
    (Name: 'charter'; Kind: gfSerif),
    (Name: 'constantia'; Kind: gfSerif),
    (Name: 'didot'; Kind: gfSerif),
    (Name: 'garamond'; Kind: gfSerif),
    (Name: 'georgia'; Kind: gfSerif),
    (Name: 'hoefler text'; Kind: gfSerif),
    (Name: 'ms serif'; Kind: gfSerif),
    (Name: 'new century schoolbook'; Kind: gfSerif),
    (Name: 'palatino'; Kind: gfSerif),
    (Name: 'palatino linotype'; Kind: gfSerif),
    (Name: 'rockwell'; Kind: gfSerif),
    (Name: 'times'; Kind: gfSerif),
    (Name: 'times new roman'; Kind: gfSerif),
    (Name: 'utopia'; Kind: gfSerif),
    (Name: 'andale mono'; Kind: gfMonospace),
    (Name: 'consolas'; Kind: gfMonospace),
    (Name: 'courier'; Kind: gfMonospace),
    (Name: 'courier new'; Kind: gfMonospace),
    (Name: 'lucida console'; Kind: gfMonospace));

function SVGGenericFamilyOf(const aName: TSVGString): TSVGGenericFamily;

var
  lName: String;

begin
  lName := Trim(aName);
  if SameText(lName, 'serif') then
    Result := gfSerif
  else if SameText(lName, 'sans-serif') then
    Result := gfSansSerif
  else if SameText(lName, 'cursive') then
    Result := gfCursive
  else if SameText(lName, 'fantasy') then
    Result := gfFantasy
  else if SameText(lName, 'monospace') then
    Result := gfMonospace
  else
    Result := gfNone;
end;


function SVGFamilyKindOf(const aName: TSVGString): TSVGGenericFamily;

var
  I: Integer;
  lName: String;

begin
  Result := gfNone;
  lName := LowerCase(Trim(aName));
  for I := 0 to High(KnownFamilies) do
    if KnownFamilies[I].Name = lName then
      Exit(KnownFamilies[I].Kind);
end;


function SVGFontStretchName(aStretch: TSVGFontStretch): String;

const
  Names: array[TSVGFontStretch] of String = ('ultra-condensed',
    'extra-condensed', 'condensed', 'semi-condensed', 'normal',
    'semi-expanded', 'expanded', 'extra-expanded', 'ultra-expanded');

begin
  Result := Names[aStretch];
end;


function SVGFontStretchOf(const aName: TSVGString): TSVGFontStretch;

var
  I: TSVGFontStretch;
  lName: String;

begin
  Result := fsNormal;
  lName := LowerCase(Trim(aName));
  for I := Low(TSVGFontStretch) to High(TSVGFontStretch) do
    if SVGFontStretchName(I) = lName then
      Exit(I);
end;


{ TSVGFontRequest }

constructor TSVGFontRequest.Create(const aFamilies: TSVGString;
  aSize: Double);

begin
  Families := aFamilies;
  Size := aSize;
  Weight := SVGNormalFontWeight;
  Style := fnNormal;
  Variant := fvNormal;
  Stretch := fsNormal;
end;


function TSVGFontRequest.ToString: String;

const
  StyleNames: array[TSVGFontStyle] of String = ('normal', 'italic', 'oblique');
  VariantNames: array[TSVGFontVariant] of String = ('normal', 'small-caps');

begin
  Result := 'families="' + Families + '"'
    + ' size=' + SVGFormatFloat(Size)
    + ' weight=' + IntToStr(Weight)
    + ' style=' + StyleNames[Style]
    + ' variant=' + VariantNames[Variant]
    + ' stretch=' + SVGFontStretchName(Stretch);
end;


{ TSVGPath }

procedure TSVGPath.Grow;

var
  lCapacity: Integer;

begin
  lCapacity := Length(FSegments);
  if lCapacity = 0 then
    lCapacity := 8
  else
    lCapacity := lCapacity * 2;
  SetLength(FSegments, lCapacity);
end;


procedure TSVGPath.NeedCurrentPoint;

begin
  if not FHasCurrent then
    raise ESVGError.Create(SErrSegmentBeforeMoveTo);
end;


function TSVGPath.GetSegment(aIndex: Integer): TSVGPathSegment;

begin
  if (aIndex < 0) or (aIndex >= FCount) then
    raise ESVGError.CreateFmt(SErrSegmentIndexOutOfRange, [aIndex]);
  Result := FSegments[aIndex];
end;


procedure TSVGPath.Clear;

begin
  FCount := 0;
  FHasCurrent := False;
  FCurrent := TSVGPoint.Create(0, 0);
  FStart := FCurrent;
end;


procedure TSVGPath.MoveTo(aX, aY: Double);

begin
  if FCount = Length(FSegments) then
    Grow;
  FSegments[FCount].Kind := skMoveTo;
  FSegments[FCount].Points[0] := TSVGPoint.Create(aX, aY);
  Inc(FCount);
  FCurrent := TSVGPoint.Create(aX, aY);
  FStart := FCurrent;
  FHasCurrent := True;
end;


procedure TSVGPath.LineTo(aX, aY: Double);

begin
  NeedCurrentPoint;
  if FCount = Length(FSegments) then
    Grow;
  FSegments[FCount].Kind := skLineTo;
  FSegments[FCount].Points[0] := TSVGPoint.Create(aX, aY);
  Inc(FCount);
  FCurrent := TSVGPoint.Create(aX, aY);
end;


procedure TSVGPath.CubicTo(aC1X, aC1Y, aC2X, aC2Y, aX, aY: Double);

begin
  NeedCurrentPoint;
  if FCount = Length(FSegments) then
    Grow;
  FSegments[FCount].Kind := skCubicTo;
  FSegments[FCount].Points[0] := TSVGPoint.Create(aC1X, aC1Y);
  FSegments[FCount].Points[1] := TSVGPoint.Create(aC2X, aC2Y);
  FSegments[FCount].Points[2] := TSVGPoint.Create(aX, aY);
  Inc(FCount);
  FCurrent := TSVGPoint.Create(aX, aY);
end;


procedure TSVGPath.QuadTo(aCX, aCY, aX, aY: Double);

var
  lC1X, lC1Y, lC2X, lC2Y: Double;

begin
  NeedCurrentPoint;
  lC1X := FCurrent.X + (2 / 3) * (aCX - FCurrent.X);
  lC1Y := FCurrent.Y + (2 / 3) * (aCY - FCurrent.Y);
  lC2X := aX + (2 / 3) * (aCX - aX);
  lC2Y := aY + (2 / 3) * (aCY - aY);
  CubicTo(lC1X, lC1Y, lC2X, lC2Y, aX, aY);
end;


procedure TSVGPath.Close;

begin
  NeedCurrentPoint;
  if FCount = Length(FSegments) then
    Grow;
  FSegments[FCount].Kind := skClose;
  Inc(FCount);
  FCurrent := FStart;
end;


procedure TSVGPath.Assign(aPath: TSVGPath);

var
  I: Integer;

begin
  if aPath = Self then
    Exit;
  if aPath = nil then
    begin
    Clear;
    Exit;
    end;
  SetLength(FSegments, aPath.FCount);
  for I := 0 to aPath.FCount - 1 do
    FSegments[I] := aPath.FSegments[I];
  FCount := aPath.FCount;
  FCurrent := aPath.FCurrent;
  FStart := aPath.FStart;
  FHasCurrent := aPath.FHasCurrent;
end;


function TSVGPath.IsEmpty: Boolean;

begin
  Result := FCount = 0;
end;


function TSVGPath.ControlBounds: TSVGRect;

var
  I, J: Integer;

begin
  Result := TSVGRect.Empty;
  for I := 0 to FCount - 1 do
    for J := 0 to FSegments[I].PointCount - 1 do
      begin
      Result.Left := Min(Result.Left, FSegments[I].Points[J].X);
      Result.Top := Min(Result.Top, FSegments[I].Points[J].Y);
      Result.Right := Max(Result.Right, FSegments[I].Points[J].X);
      Result.Bottom := Max(Result.Bottom, FSegments[I].Points[J].Y);
      end;
end;


end.
