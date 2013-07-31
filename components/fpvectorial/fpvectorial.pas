{
fpvectorial.pas

Vector graphics document

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho
         Pedro Sol Pegorini L de Lima
}
unit fpvectorial;

{$ifdef fpc}
  {$mode delphi}
{$endif}

{$define USE_LCL_CANVAS}
{$ifdef USE_LCL_CANVAS}
  {$define USE_CANVAS_CLIP_REGION}
  {.$define DEBUG_CANVAS_CLIP_REGION}
{$endif}
{.$define FPVECTORIAL_DEBUG_DIMENSIONS}
{.$define FPVECTORIAL_TOCANVAS_DEBUG}
{.$define FPVECTORIAL_DEBUG_BLOCKS}

interface

uses
  Classes, SysUtils, Math, TypInfo,
  // FCL-Image
  fpcanvas, fpimage,
  // LCL
  lazutf8
  {$ifdef USE_LCL_CANVAS}
  , Graphics, LCLIntf, LCLType, intfgraphics, graphtype
  {$endif}
  ;

type
  TvVectorialFormat = (
    { Multi-purpose document formats }
    vfPDF, vfSVG, vfSVGZ, vfCorelDrawCDR, vfWindowsMetafileWMF, vfODG,
    { CAD formats }
    vfDXF,
    { Geospatial formats }
    vfLAS, vfLAZ,
    { Printing formats }
    vfPostScript, vfEncapsulatedPostScript,
    { GCode formats }
    vfGCodeAvisoCNCPrototipoV5, vfGCodeAvisoCNCPrototipoV6,
    { Formula formats }
    vfMathML,
    { Text Document formats }
    vfODT,
    { Raster Image formats }
    vfRAW
    );

  TvProgressEvent = procedure (APercentage: Byte) of object;

  {@@ This routine is called to add an item of caption AStr to an item
    AParent, which is a pointer to another item as returned by a previous call
    of this same proc. If AParent = nil then it should add the item to the
    top of the tree. In all cases this routine should return a pointer to the
    newly created item.
  }
  TvDebugAddItemProc = function (AStr: string; AParent: Pointer): Pointer of object;

const
  { Default extensions }
  { Multi-purpose document formats }
  STR_PDF_EXTENSION = '.pdf';
  STR_POSTSCRIPT_EXTENSION = '.ps';
  STR_SVG_EXTENSION = '.svg';
  STR_SVGZ_EXTENSION = '.svgz';
  STR_CORELDRAW_EXTENSION = '.cdr';
  STR_WINMETAFILE_EXTENSION = '.wmf';
  STR_AUTOCAD_EXCHANGE_EXTENSION = '.dxf';
  STR_ENCAPSULATEDPOSTSCRIPT_EXTENSION = '.eps';
  STR_LAS_EXTENSION = '.las';
  STR_LAZ_EXTENSION = '.laz';
  STR_RAW_EXTENSION = '.raw';
  STR_MATHML_EXTENSION = '.mathml';
  STR_ODG_EXTENSION = '.odg';

  STR_FPVECTORIAL_TEXT_HEIGHT_SAMPLE = 'Ćą';

type
  TvCustomVectorialWriter = class;
  TvCustomVectorialReader = class;
  TvVectorialPage = class;
  TvTextPageSequence = class;

  { Pen, Brush and Font }

  TvPen = record
    Color: TFPColor;
    Style: TFPPenStyle;
    Width: Integer;
  end;

  TvBrushKind = (bkSimpleBrush, bkRadialGradient);
  TvCoordinateUnit = (vcuDocumentUnit, vcuPercentage);

  TvBrush = record
    Color: TFPColor;
    Style: TFPBrushStyle;
    Kind: TvBrushKind;
    // Gradient filling support
    Gradient_cx, Gradient_cy, Gradient_r, Gradient_fx, Gradient_fy: Double;
    Gradient_cx_Unit, Gradient_cy_Unit, Gradient_r_Unit, Gradient_fx_Unit, Gradient_fy_Unit: TvCoordinateUnit;
    Gradient_colors: array of TFPColor;
  end;

  TvFont = record
    Color: TFPColor;
    Size: integer;
    Name: utf8string;
    {@@
      Font orientation is measured in degrees and uses the
      same direction as the LCL TFont.orientation, which is counter-clockwise.
      Zero is the normal, horizontal, orientation, directed to the right.
    }
    Orientation: Double;
    Bold: boolean;
    Italic: boolean;
    Underline: boolean;
    StrikeThrough: boolean;
  end;

  TvSetPenBrushAndFontElement = (
    spbfPenColor, spbfPenStyle, spbfPenWidth,
    spbfBrushColor, spbfBrushStyle, spbfBrushGradient,
    spbfFontColor, spbfFontSize, spbfFontName, spbfFontBold, spbfFontItalic
    );
  TvSetPenBrushAndFontElements = set of TvSetPenBrushAndFontElement;

  TvStyle = class
    Name: string;
    Pen: TvPen;
    Brush: TvBrush;
    Font: TvFont;
    SetElements: TvSetPenBrushAndFontElements;
    //
    MarginTop, MarginBottom, MarginLeft, MarginRight: Double; // in mm
  end;

  { Coordinates and polyline segments }

  T3DPoint = record
    X, Y, Z: Double;
  end;

  P3DPoint = ^T3DPoint;

  TSegmentType = (
    st2DLine, st2DLineWithPen, st2DBezier,
    st3DLine, st3DBezier, stMoveTo,
    st2DEllipticalArc);

  {@@
    The coordinates in fpvectorial are given in millimiters and
    the starting point is in the bottom-left corner of the document.
    The X grows to the right and the Y grows to the top.
  }
  { TPathSegment }

  TPathSegment = class
  public
    SegmentType: TSegmentType;
    // Fields for linking the list
    Previous: TPathSegment;
    Next: TPathSegment;
    procedure Move(ADeltaX, ADeltaY: Double); virtual;
    procedure Rotate(AAngle: Double; ABase: T3DPoint); virtual; // Angle in radians
    procedure CalculateBoundingBox(ADest: TFPCustomCanvas; var ALeft, ATop, ARight, ABottom: Double); virtual;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; virtual;
  end;

  {@@
    In a 2D segment, the X and Y coordinates represent usually the
    final point of the segment, being that it starts where the previous
    segment ends. The exception is for the first segment of all, which simply
    holds the starting point for the drawing and should always be of the type
    stMoveTo.
  }

  { T2DSegment }

  T2DSegment = class(TPathSegment)
  public
    X, Y: Double;
    procedure Move(ADeltaX, ADeltaY: Double); override;
    procedure Rotate(AAngle: Double; ABase: T3DPoint); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  T2DSegmentWithPen = class(T2DSegment)
  public
    Pen: TvPen;
  end;

  {@@
    In Bezier segments, we remain using the X and Y coordinates for the ending point.
    The starting point is where the previous segment ended, so that the intermediary
    bezier control points are [X2, Y2] and [X3, Y3].
  }

  { T2DBezierSegment }

  T2DBezierSegment = class(T2DSegment)
  public
    X2, Y2: Double;
    X3, Y3: Double;
    procedure Move(ADeltaX, ADeltaY: Double); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  { T3DSegment }

  T3DSegment = class(TPathSegment)
  public
    {@@
      Coordinates of the end of the segment.
      For the first segment, this is the starting point.
    }
    X, Y, Z: Double;
    procedure Move(ADeltaX, ADeltaY: Double); override;
  end;

  { T3DBezierSegment }

  T3DBezierSegment = class(T3DSegment)
  public
    X2, Y2, Z2: Double;
    X3, Y3, Z3: Double;
    procedure Move(ADeltaX, ADeltaY: Double); override;
  end;

  // Elliptical Arc
  // See http://www.w3.org/TR/SVG/paths.html#PathDataEllipticalArcCommands

  { T2DEllipticalArcSegment }

  T2DEllipticalArcSegment = class(T2DSegment)
  private
    E1, E2: T3DPoint;
    function AlignedEllipseCenterEquationT1(AParam: Double): Double;
  public
    RX, RY, XRotation: Double;
    LeftmostEllipse, ClockwiseArcFlag: Boolean;
    CX, CY: Double;
    procedure CalculateCenter;
    procedure CalculateEllipseBoundingBox(ADest: TFPCustomCanvas; var ALeft, ATop, ARight, ABottom: Double);
  end;


  TvFindEntityResult = (vfrNotFound, vfrFound, vfrSubpartFound);

  TvRenderInfo = record
    BackgroundColor: TFPColor;
    AdjustPenColorToBackground: Boolean;
    Selected: Boolean;
    ForceRenderBlock: Boolean; // Blocks are usually invisible, but when rendering an insert, their drawing can be forced
  end;

  { Now all elements }

  {@@
    All elements should derive from TvEntity, regardless of whatever properties
    they might contain.
  }

  { TvEntity }

  TvEntity = class
  public
    //not used currently Parent: TvEntity; // Might be nil if this is placed directly in the page!!!
    X, Y, Z: Double;
    constructor Create; virtual;
    procedure Clear; virtual;
    // in CalculateBoundingBox always remember to treat correctly the case of ADest=nil!!!
    // This cased is utilized to guess the size of a document even before getting a canvas to draw at
    procedure CalculateBoundingBox(ADest: TFPCustomCanvas; var ALeft, ATop, ARight, ABottom: Double); virtual;
    procedure ExpandBoundingBox(ADest: TFPCustomCanvas; var ALeft, ATop, ARight, ABottom: Double); // helper to help CalculateBoundingBox
    {@@ ASubpart is only valid if this routine returns vfrSubpartFound }
    function TryToSelect(APos: TPoint; var ASubpart: Cardinal): TvFindEntityResult; virtual;
    procedure Move(ADeltaX, ADeltaY: Double); virtual;
    procedure MoveSubpart(ADeltaX, ADeltaY: Double; ASubpart: Cardinal); virtual;
    function  GetSubpartCount: Integer; virtual;
    procedure PositionSubparts(ADest: TFPCustomCanvas; ABaseX, ABaseY: Double); virtual;
    procedure Rotate(AAngle: Double; ABase: T3DPoint); virtual; // Angle in radians
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); virtual;
    function AdjustColorToBackground(AColor: TFPColor; ARenderInfo: TvRenderInfo): TFPColor;
    function GetNormalizedPos(APage: TvVectorialPage; ANewMin, ANewMax: Double): T3DPoint;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; virtual;
    function GenerateDebugStrForFPColor(AColor: TFPColor): string;
  end;

  TvEntityClass = class of TvEntity;

  { TvNamedEntity }

  TvNamedEntity = class(TvEntity)
  protected
    FExtraDebugStr: string;
  public
    Name: string;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  { TvEntityWithPen }

  TvEntityWithPen = class(TvNamedEntity)
  public
    {@@ The global Pen for the entire entity. In the case of paths, individual
        elements might be able to override this setting. }
    Pen: TvPen;
    constructor Create; override;
    procedure ApplyPenToCanvas(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo);
    procedure AssignPen(APen: TvPen);
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
  end;

  { TvEntityWithPenAndBrush }

  TvEntityWithPenAndBrush = class(TvEntityWithPen)
  public
    {@@ The global Brush for the entire entity. In the case of paths, individual
        elements might be able to override this setting. }
    Brush: TvBrush;
    constructor Create; override;
    procedure ApplyBrushToCanvas(ADest: TFPCustomCanvas);
    procedure AssignBrush(ABrush: TvBrush);
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  { TvEntityWithPenBrushAndFont }

  TvTextAnchor = (vtaStart, vtaMiddle, vtaEnd);

  TvEntityWithPenBrushAndFont = class(TvEntityWithPenAndBrush)
  public
    Font: TvFont;
    TextAnchor: TvTextAnchor;
    constructor Create; override;
    procedure ApplyFontToCanvas(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; AMulX: Double = 1.0);
    procedure AssignFont(AFont: TvFont);
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  TvClipMode = (vcmNonzeroWindingRule, vcmEvenOddRule);

  TPath = class(TvEntityWithPenAndBrush)
  private
    // Used to speed up sequencial access in MoveSubpart
    FCurMoveSubPartIndex: Integer;
    FCurMoveSubPartSegment: TPathSegment;
    //
  public
    Len: Integer;
    Points: TPathSegment;   // Beginning of the double-linked list
    PointsEnd: TPathSegment;// End of the double-linked list
    CurPoint: TPathSegment; // Used in PrepareForSequentialReading and Next
    ClipPath: TPath;
    ClipMode: TvClipMode;
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(ASource: TPath);
    procedure PrepareForSequentialReading;
    function Next(): TPathSegment;
    procedure CalculateBoundingBox(ADest: TFPCustomCanvas; var ALeft, ATop, ARight, ABottom: Double); override;
    procedure AppendSegment(ASegment: TPathSegment);
    procedure AppendMoveToSegment(AX, AY: Double);
    procedure AppendLineToSegment(AX, AY: Double);
    procedure Move(ADeltaX, ADeltaY: Double); override;
    procedure MoveSubpart(ADeltaX, ADeltaY: Double; ASubpart: Cardinal); override;
    function  MoveToSubpart(ASubpart: Cardinal): TPathSegment;
    function  GetSubpartCount: Integer; override;
    procedure Rotate(AAngle: Double; ABase: T3DPoint); override;
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
    procedure RenderInternalPolygon(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  {@@
    TvText represents a text entity.

    The text starts in X, Y and grows upwards, towards a bigger Y (fpvectorial coordinates)
    or smaller Y (LCL coordinates).
    It has the opposite direction of text in the LCL TCanvas.
  }


  { TvText }

  TvText = class(TvEntityWithPenBrushAndFont)
  public
    Value: TStringList;
    constructor Create; override;
    destructor Destroy; override;
    function TryToSelect(APos: TPoint; var ASubpart: Cardinal): TvFindEntityResult; override;
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  {@@
  }

  { TvCircle }

  TvCircle = class(TvEntityWithPenAndBrush)
  public
    Radius: Double;
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
  end;

  {@@
  }

  { TvCircularArc }

  TvCircularArc = class(TvEntityWithPenAndBrush)
  public
    Radius: Double;
    {@@ The Angle is measured in degrees in relation to the positive X axis }
    StartAngle, EndAngle: Double;
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
  end;

  {@@
  }

  { TvEllipse }

  TvEllipse = class(TvEntityWithPenAndBrush)
  public
    // Mandatory fields
    HorzHalfAxis: Double; // This half-axis is the horizontal one when Angle=0
    VertHalfAxis: Double; // This half-axis is the vertical one when Angle=0
    {@@ The Angle is measured in degrees in relation to the positive X axis }
    Angle: Double;
    procedure CalculateBoundingBox(ADest: TFPCustomCanvas; var ALeft, ATop, ARight, ABottom: Double); override;
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
  end;

  { TvRectangle }

  TvRectangle = class(TvEntityWithPenBrushAndFont)
  public
    // A text displayed in the center of the square, usually empty
    Text: string;
    // Mandatory fields
    CX, CY, CZ: Double;
    // Corner rounding, zero indicates no rounding
    RX, RY: Double;
    procedure CalculateBoundingBox(ADest: TFPCustomCanvas; var ALeft, ATop, ARight, ABottom: Double); override;
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  { TvPolygon }

  TvPolygon = class(TvEntityWithPenBrushAndFont)
  public
    // A text displayed in the center of the square, usually empty
    Text: string;
    // All points of the polygon
    Points: array of T3DPoint;
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
  end;

  {@@
   DimensionLeft ---text--- DimensionRight
                 |        |
                 |        | BaseRight
                 |
                 | BaseLeft
  }

  { TvAlignedDimension }

  TvAlignedDimension = class(TvEntityWithPen)
  public
    // Mandatory fields
    BaseLeft, BaseRight, DimensionLeft, DimensionRight: T3DPoint;
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  {@@

  }

  { TvRadialDimension }

  TvRadialDimension = class(TvEntityWithPen)
  public
    // Mandatory fields
    IsDiameter: Boolean; // If false, it is a radius, if true, it is a diameter
    Center, DimensionLeft, DimensionRight: T3DPoint; // Diameter uses both, Radius uses only DImensionLeft
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  { TvArcDimension }

  TvArcDimension = class(TvEntityWithPen)
  private
    // Calculated fields
    AngleBase, ArcLeft, ArcRight: T3DPoint;
    al, bl, ar, br, AngleLeft, AngleRight: Double;
  public
    // Mandatory fields
    ArcValue, ArcRadius: Double; // ArcValue is in degrees
    TextPos, BaseLeft, BaseRight, DimensionLeft, DimensionRight: T3DPoint;
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
    procedure CalculateExtraArcInfo;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  {@@
   Vectorial images can contain raster images inside them and this entity
   represents this.

   If the Width and Height differ from the same data in the image, then
   the raster image will be stretched.

   Note that TFPCustomImage does not implement a storage, so the property
   RasterImage should be filled with either a FPImage.TFPMemoryImage or with
   a TLazIntfImage. The property RasterImage might be nil.
  }

  { TvRasterImage }

  TvRasterImage = class(TvNamedEntity)
  public
    RasterImage: TFPCustomImage;
    Top, Left, Width, Height: Double;
    procedure CreateRGB888Image(AWidth, AHeight: Cardinal);
    procedure InitializeWithConvertionOf3DPointsToHeightMap(APage: TvVectorialPage; AWidth, AHeight: Integer);
  end;

  { TvPoint }

  // Keep TvPoint as small as possible in memory foot-print for LAS support
  TvPoint = class(TvEntity)
  public
    Pen: TvPen;
    {constructor Create; override;
    procedure ApplyPenToCanvas(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo);
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;}
  end;

  { TvArrow }

  //
  // The arrow look like this:
  //
  // A<------|B
  //         |
  //         |C
  //
  // A -> X,Y,Z
  // B -> Base
  // C -> ExtraLineBase, which exists if HasExtraLine=True

  TvArrow = class(TvEntityWithPenAndBrush)
  public
    Base: T3DPoint;
    HasExtraLine: Boolean;
    ExtraLineBase: T3DPoint;
    ArrowLength: Double;
    ArrowBaseLength: Double;
    procedure CalculateBoundingBox(ADest: TFPCustomCanvas; var ALeft, ATop, ARight, ABottom: Double); override;
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
  end;

  {@@
    The elements bellow describe a formula

    The main element of a formula is TvFormula which contains a horizontal list of
    the elements of the formula. Those can then have sub-elements

    The formula starts in X, Y and grows downwards, towards a smaller Y
  }

  TvFormula = class;

  TvFormulaElementKind = (
    // Basic symbols
    fekVariable,  // Text is the text of the variable
    fekEqual,     // = symbol
    fekSubtraction, // - symbol
    fekMultiplication, // either a point . or a small x
    fekSum,       // + symbol
    fekPlusMinus, // The +/- symbol
    fekLessThan, // The < symbol
    fekLessOrEqualThan, // The <= symbol
    fekGreaterThan, // The > symbol
    fekGreaterOrEqualThan, // The >= symbol
    fekHorizontalLine,
    // More complex elements
    fekFraction,  // a division with Formula on the top and AdjacentFormula in the bottom
    fekRoot,      // A root. For example sqrt(something). Number gives the root, usually 2, and inside it goes a Formula
    fekPower,     // A Formula elevated to a AdjacentFormula, example: 2^5
    fekSubscript, // A Formula with a subscripted element AdjacentFormula, example: Xi
    fekSummation, // Sum of a variable given by Text set by Formula in the bottom and going up to AdjacentFormula in the top
    fekFormula    // A formula, stored in Formula
    );

  { TvFormulaElement }

  TvFormulaElement = class
  public
    Kind: TvFormulaElementKind;
    Text: string;
    Number: Double;
    Formula: TvFormula;
    AdjacentFormula: TvFormula;
  public
    Top, Left, Width, Height: Double;
    function CalculateHeight(ADest: TFPCustomCanvas): Double; // in milimeters
    function CalculateWidth(ADest: TFPCustomCanvas): Double; // in milimeters
    function AsText: string;
    procedure PositionSubparts(ADest: TFPCustomCanvas; ABaseX, ABaseY: Double);
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); virtual;
    procedure GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer); virtual;
  end;

  { TvFormula }

  TvFormula = class(TvEntityWithPenBrushAndFont)
  private
    FCurIndex: Integer;
    procedure CallbackDeleteElement(data,arg:pointer);
  protected
    FElements: TFPList; // of TvFormulaElement
    SpacingBetweenElementsX, SpacingBetweenElementsY: Integer;
  public
    Top, Left, Width, Height: Double;
    constructor Create; override;
    destructor Destroy; override;
    //
    function GetFirstElement: TvFormulaElement;
    function GetNextElement: TvFormulaElement;
    procedure AddElement(AElement: TvFormulaElement);
    function  AddElementWithKind(AKind: TvFormulaElementKind): TvFormulaElement;
    function  AddElementWithKindAndText(AKind: TvFormulaElementKind; AText: string): TvFormulaElement;
    procedure Clear; override;
    //
    function CalculateHeight(ADest: TFPCustomCanvas): Double; virtual; // in milimeters
    function CalculateWidth(ADest: TFPCustomCanvas): Double; virtual; // in milimeters
    procedure PositionSubparts(ADest: TFPCustomCanvas; ABaseX, ABaseY: Double); override;
    procedure CalculateBoundingBox(ADest: TFPCustomCanvas; var ALeft, ATop, ARight, ABottom: Double); override;
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  { TvVerticalFormulaStack }

  TvVerticalFormulaStack = class(TvFormula)
  public
    function CalculateHeight(ADest: TFPCustomCanvas): Double; override; // in milimeters
    function CalculateWidth(ADest: TFPCustomCanvas): Double; override; // in milimeters
    procedure PositionSubparts(ADest: TFPCustomCanvas; ABaseX, ABaseY: Double); override;
    procedure CalculateBoundingBox(ADest: TFPCustomCanvas; var ALeft, ATop, ARight, ABottom: Double); override;
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
    //function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  {@@
    A EntityWithSubEntities may have Pen, Brush and/or Font data associated with it, but it is disabled by default
    This data can be active recursively in all children of the group if set in the field
    SetPenBrushAndFontElements
  }

  { TvEntityWithSubEntities }

  TvEntityWithSubEntities = class(TvEntityWithPenBrushAndFont)
  private
    FCurIndex: Integer;
    procedure CallbackDeleteElement(data,arg:pointer);
  protected
    FElements: TFPList; // of TvEntity
  public
    SetPenBrushAndFontElements: TvSetPenBrushAndFontElements;// This is not currently implemented!
    constructor Create; override;
    destructor Destroy; override;
    //
    function GetFirstEntity: TvEntity;
    function GetNextEntity: TvEntity;
    procedure AddEntity(AEntity: TvEntity);
    procedure Rotate(AAngle: Double; ABase: T3DPoint); override;
    procedure Clear; override;
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
    function FindEntityWithNameAndType(AName: string; AType: TvEntityClass {= TvEntity}; ARecursively: Boolean = False): TvEntity;
  end;

  {@@
    A block is a group of other elements. It is not rendered directly into the drawing,
    but instead is rendered via another item, called TvInsert
  }

  { TvBlock }

  TvBlock = class(TvEntityWithSubEntities)
  public
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
  end;

  {@@
    A "Insert" inserts a copy of any other element in the specified position.
    Usually TvBlock entities are inserted, but any entity can be inserted.
  }

  { TvInsert }

  TvInsert = class(TvNamedEntity)
  public
    InsertEntity: TvEntity; // The entity to be inserted
    RotationAngle: Double; // in angles, normal is zero
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  {@@
    Layers are groups of elements.
    Layers are similar to blocks and the diference is that the layer draws
    its contents, while the block doesnt, and it cannot be pasted with an TvInsert.
  }

  { TvLayer }

  TvLayer = class(TvEntityWithSubEntities)
  public
  end;

  {@@
    TvRichText represents a sequence of elements ordered as characters.
    The elements might be richly formatted text, but also any other elements.

    The basic element to build the sequence is TvText. Note that the X, Y positions
    of elements will be all adjusted to fit the TvRichText area
  }

  TvRichTextAutoExpand = (rtaeNone, etaeWidth, etaeHeight);

  { TvRichText }

  TvRichText = class(TvEntityWithSubEntities)
  public
    Width, Height: Double;
    AutoExpand: TvRichTextAutoExpand;
    Style: string;
    constructor Create; override;
    destructor Destroy; override;
    function AddText: TvText;
    function TryToSelect(APos: TPoint; var ASubpart: Cardinal): TvFindEntityResult; override;
    procedure Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  { TvVectorialDocument }

  TvVectorialDocument = class
  private
    FOnProgress: TvProgressEvent;
    FPages: TFPList;
    FStyles: TFPList;
    FCurrentPageIndex: Integer;
    function CreateVectorialWriter(AFormat: TvVectorialFormat): TvCustomVectorialWriter;
    function CreateVectorialReader(AFormat: TvVectorialFormat): TvCustomVectorialReader;
  public
    Width, Height: Double; // in millimeters
    Name: string;
    Encoding: string; // The encoding on which to save the file, if empty UTF-8 will be utilized. This value is filled when reading
    ForcedEncodingOnRead: string; // if empty, no encoding will be forced when reading, but it can be set to a LazUtils compatible value
    // User-Interface information
    ZoomLevel: Double; // 1 = 100%
    { Selection fields }
    SelectedElement: TvEntity;
    { Base methods }
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TvVectorialDocument);
    procedure AssignTo(ADest: TvVectorialDocument);
    procedure WriteToFile(AFileName: string; AFormat: TvVectorialFormat); overload;
    procedure WriteToFile(AFileName: string); overload;
    procedure WriteToStream(AStream: TStream; AFormat: TvVectorialFormat);
    procedure WriteToStrings(AStrings: TStrings; AFormat: TvVectorialFormat);
    procedure ReadFromFile(AFileName: string; AFormat: TvVectorialFormat); overload;
    procedure ReadFromFile(AFileName: string); overload;
    procedure ReadFromStream(AStream: TStream; AFormat: TvVectorialFormat);
    procedure ReadFromStrings(AStrings: TStrings; AFormat: TvVectorialFormat);
    class function GetFormatFromExtension(AFileName: string): TvVectorialFormat;
    function  GetDetailedFileFormat(): string;
    procedure GuessDocumentSize();
    procedure GuessGoodZoomLevel(AScreenSize: Integer = 500);
    { Page methods }
    function GetPage(AIndex: Integer): TvVectorialPage;
    function GetPageCount: Integer;
    function GetCurrentPage: TvVectorialPage;
    procedure SetCurrentPage(AIndex: Integer);
    function AddPage(): TvVectorialPage;
    function AddTextPageSequence(): TvTextPageSequence;
    { Style methods }
    function AddStyle(): TvStyle;
    procedure AddStandardTextDocumentStyles;
    function GetStyleCount: Integer;
    function GetStyle(AIndex: Integer): TvStyle;
    { Data removing methods }
    procedure Clear; virtual;
    { Debug methods }
    procedure GenerateDebugTree(ADestRoutine: TvDebugAddItemProc);
    { Events }
    property OnProgress: TvProgressEvent read FOnProgress write FOnprogress;
  end;

  { TvVectorialPage }

  TvVectorialPage = class
  private
    FEntities: TFPList; // of TvEntity
    FTmpPath: TPath;
    FTmpText: TvText;
    FCurrentLayer: TvEntityWithSubEntities;
    //procedure RemoveCallback(data, arg: pointer);
    procedure ClearTmpPath();
    procedure AppendSegmentToTmpPath(ASegment: TPathSegment);
    procedure CallbackDeleteEntity(data,arg:pointer);
  public
    // Document size for page-based documents
    Width, Height: Double; // in millimeters
    // Document size for other documents
    MinX, MinY, MinZ, MaxX, MaxY, MaxZ: Double;
    //
    BackgroundColor: TFPColor;
    RenderInfo: TvRenderInfo; // Prepared by the reader with info on how to draw the page
    //
    Owner: TvVectorialDocument;
    { Base methods }
    constructor Create(AOwner: TvVectorialDocument); virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TvVectorialPage); virtual;
    { Data reading methods }
    function  GetEntity(ANum: Cardinal): TvEntity;
    function  GetEntitiesCount: Integer;
    function  GetLastEntity(): TvEntity;
    function  FindAndSelectEntity(Pos: TPoint): TvFindEntityResult;
    function  FindEntityWithNameAndType(AName: string; AType: TvEntityClass {= TvEntity}; ARecursively: Boolean = False): TvEntity;
    { Data removing methods }
    procedure Clear; virtual;
    function  DeleteEntity(AIndex: Cardinal): Boolean;
    function  RemoveEntity(AEntity: TvEntity; AFreeAfterRemove: Boolean = True): Boolean;
    { Data writing methods }
    function AddEntity(AEntity: TvEntity): Integer;
    function  AddPathCopyMem(APath: TPath; AOnlyCreate: Boolean = False): TPath;
    procedure StartPath(AX, AY: Double); overload;
    procedure StartPath(); overload;
    procedure AddMoveToPath(AX, AY: Double);
    procedure AddLineToPath(AX, AY: Double); overload;
    procedure AddLineToPath(AX, AY: Double; AColor: TFPColor); overload;
    procedure AddLineToPath(AX, AY, AZ: Double); overload;
    procedure GetCurrentPathPenPos(var AX, AY: Double);
    procedure GetTmpPathStartPos(var AX, AY: Double);
    procedure AddBezierToPath(AX1, AY1, AX2, AY2, AX3, AY3: Double); overload;
    procedure AddBezierToPath(AX1, AY1, AZ1, AX2, AY2, AZ2, AX3, AY3, AZ3: Double); overload;
    procedure AddEllipticalArcToPath(ARadX, ARadY, AXAxisRotation, ADestX, ADestY: Double; ALeftmostEllipse, AClockwiseArcFlag: Boolean); // See http://www.w3.org/TR/SVG/paths.html#PathDataEllipticalArcCommands
    procedure SetBrushColor(AColor: TFPColor);
    procedure SetBrushStyle(AStyle: TFPBrushStyle);
    procedure SetPenColor(AColor: TFPColor);
    procedure SetPenStyle(AStyle: TFPPenStyle);
    procedure SetPenWidth(AWidth: Integer);
    procedure SetClipPath(AClipPath: TPath; AClipMode: TvClipMode);
    function  EndPath(AOnlyCreate: Boolean = False): TPath;
    function  AddText(AX, AY, AZ: Double; FontName: string; FontSize: integer; AText: utf8string; AOnlyCreate: Boolean = False): TvText; overload;
    function  AddText(AX, AY: Double; AStr: utf8string; AOnlyCreate: Boolean = False): TvText; overload;
    function  AddText(AX, AY, AZ: Double; AStr: utf8string; AOnlyCreate: Boolean = False): TvText; overload;
    function AddCircle(ACenterX, ACenterY, ARadius: Double; AOnlyCreate: Boolean = False): TvCircle;
    function AddCircularArc(ACenterX, ACenterY, ARadius, AStartAngle, AEndAngle: Double; AColor: TFPColor; AOnlyCreate: Boolean = False): TvCircularArc;
    function AddEllipse(CenterX, CenterY, HorzHalfAxis, VertHalfAxis, Angle: Double; AOnlyCreate: Boolean = False): TvEllipse;
    function AddBlock(AName: string; AX, AY, AZ: Double): TvBlock;
    function AddInsert(AX, AY, AZ: Double; AInsertEntity: TvEntity): TvInsert;
    // Layers
    function AddLayer(AName: string): TvLayer;
    function AddLayerAndSetAsCurrent(AName: string): TvLayer;
    procedure ClearLayerSelection();
    function SetCurrentLayer(ALayer: TvEntityWithSubEntities): Boolean;
    function GetCurrentLayer: TvEntityWithSubEntities;
    // Dimensions
    function AddAlignedDimension(BaseLeft, BaseRight, DimLeft, DimRight: T3DPoint; AOnlyCreate: Boolean = False): TvAlignedDimension;
    function AddRadialDimension(AIsDiameter: Boolean; ACenter, ADimLeft, ADimRight: T3DPoint; AOnlyCreate: Boolean = False): TvRadialDimension;
    function AddArcDimension(AArcValue, AArcRadius: Double; ABaseLeft, ABaseRight, ADimLeft, ADimRight, ATextPos: T3DPoint; AOnlyCreate: Boolean): TvArcDimension;
    //
    function AddPoint(AX, AY, AZ: Double): TvPoint;
    { Drawing methods }
    procedure PositionEntitySubparts(ADest: TFPCustomCanvas; ABaseX, ABaseY: Double);
    procedure DrawBackground(ADest: TFPCustomCanvas);
    procedure Render(ADest: TFPCustomCanvas;
      ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
    { Debug methods }
    procedure GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer);
    //
    property Entities[AIndex: Cardinal]: TvEntity read GetEntity;
  end;

  { TvTextPageSequence }

  TvTextPageSequence = class(TvVectorialPage)
  public
    Footer, Header: TvRichText;
    MainText: TvRichText;
    { Base methods }
    constructor Create(AOwner: TvVectorialDocument); override;
    destructor Destroy; override;
    procedure Assign(ASource: TvVectorialPage); override;
    { Data writing methods }
    function AddParagraph: TvRichText;
  end;

  {@@ TvVectorialReader class reference type }

  TvVectorialReaderClass = class of TvCustomVectorialReader;

  { TvCustomVectorialReader }

  TvCustomVectorialReader = class
  public
    { General reading methods }
    constructor Create; virtual;
    procedure ReadFromFile(AFileName: string; AData: TvVectorialDocument); virtual;
    procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); virtual;
    procedure ReadFromStrings(AStrings: TStrings; AData: TvVectorialDocument); virtual;
  end;

  {@@ TvVectorialWriter class reference type }

  TvVectorialWriterClass = class of TvCustomVectorialWriter;

  {@@ TvCustomVectorialWriter }

  { TvCustomVectorialWriter }

  TvCustomVectorialWriter = class
  public
    { General writing methods }
    constructor Create; virtual;
    procedure WriteToFile(AFileName: string; AData: TvVectorialDocument); virtual;
    procedure WriteToStream(AStream: TStream; AData: TvVectorialDocument); virtual;
    procedure WriteToStrings(AStrings: TStrings; AData: TvVectorialDocument); virtual;
  end;

  {@@ List of registered formats }

  TvVectorialFormatData = record
    ReaderClass: TvVectorialReaderClass;
    WriterClass: TvVectorialWriterClass;
    ReaderRegistered: Boolean;
    WriterRegistered: Boolean;
    Format: TvVectorialFormat;
  end;

var
  GvVectorialFormats: array of TvVectorialFormatData;

procedure RegisterVectorialReader(
  AReaderClass: TvVectorialReaderClass;
  AFormat: TvVectorialFormat);
procedure RegisterVectorialWriter(
  AWriterClass: TvVectorialWriterClass;
  AFormat: TvVectorialFormat);
function Make2DPoint(AX, AY: Double): T3DPoint;

implementation

uses fpvutils;

const
  Str_Error_Nil_Path = ' The program attempted to add a segment before creating a path';

{@@
  Registers a new reader for a format
}
procedure RegisterVectorialReader(
  AReaderClass: TvVectorialReaderClass;
  AFormat: TvVectorialFormat);
var
  i, len: Integer;
  FormatInTheList: Boolean;
begin
  len := Length(GvVectorialFormats);
  FormatInTheList := False;

  { First search for the format in the list }
  for i := 0 to len - 1 do
  begin
    if GvVectorialFormats[i].Format = AFormat then
    begin
      if GvVectorialFormats[i].ReaderRegistered then
       raise Exception.Create('RegisterVectorialReader: Reader class for format ' {+ AFormat} + ' already registered.');

      GvVectorialFormats[i].ReaderRegistered := True;
      GvVectorialFormats[i].ReaderClass := AReaderClass;

      FormatInTheList := True;
      Break;
    end;
  end;

  { If not already in the list, then add it }
  if not FormatInTheList then
  begin
    SetLength(GvVectorialFormats, len + 1);

    GvVectorialFormats[len].ReaderClass := AReaderClass;
    GvVectorialFormats[len].WriterClass := nil;
    GvVectorialFormats[len].ReaderRegistered := True;
    GvVectorialFormats[len].WriterRegistered := False;
    GvVectorialFormats[len].Format := AFormat;
  end;
end;

{@@
  Registers a new writer for a format
}
procedure RegisterVectorialWriter(
  AWriterClass: TvVectorialWriterClass;
  AFormat: TvVectorialFormat);
var
  i, len: Integer;
  FormatInTheList: Boolean;
begin
  len := Length(GvVectorialFormats);
  FormatInTheList := False;

  { First search for the format in the list }
  for i := 0 to len - 1 do
  begin
    if GvVectorialFormats[i].Format = AFormat then
    begin
      if GvVectorialFormats[i].WriterRegistered then
       raise Exception.Create('RegisterVectorialWriter: Writer class for format ' + {AFormat +} ' already registered.');

      GvVectorialFormats[i].WriterRegistered := True;
      GvVectorialFormats[i].WriterClass := AWriterClass;

      FormatInTheList := True;
      Break;
    end;
  end;

  { If not already in the list, then add it }
  if not FormatInTheList then
  begin
    SetLength(GvVectorialFormats, len + 1);

    GvVectorialFormats[len].ReaderClass := nil;
    GvVectorialFormats[len].WriterClass := AWriterClass;
    GvVectorialFormats[len].ReaderRegistered := False;
    GvVectorialFormats[len].WriterRegistered := True;
    GvVectorialFormats[len].Format := AFormat;
  end;
end;

function Make2DPoint(AX, AY: Double): T3DPoint;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := 0;
end;

{ T2DEllipticalArcSegment }

function T2DEllipticalArcSegment.AlignedEllipseCenterEquationT1(
  AParam: Double): Double;
var
  lLeftSide, lRightSide, lArg: Double;
begin
  // E1.Y - RY*sin(t1) = E2.Y - RY*sin(arccos((- E1.X + RX*cos(t1) + E2.X)/RX))
  lLeftSide := E1.Y - RY*sin(AParam);
  lArg := (- E1.X + RX*cos(AParam) + E2.X)/RX;
  if (lArg > 1) or (lArg < -1) then Exit($FFFFFFFF);
  lRightSide := E2.Y - RY*sin(arccos(lArg));
  Result := lLeftSide - lRightSide;
  if Result < 0 then Result := -1* Result;
end;

procedure T2DEllipticalArcSegment.CalculateCenter;
var
  XStart, YStart, lT1, lT2: Double;
  CX1, CY1, CX2, CY2, LeftMostX, LeftMostY, RightMostX, RightMostY: Double;
  RotatedCenter: T3DPoint;
begin
  // Rotated Ellipse equation:
  // (xcosθ+ysinθ)^2 / RX^2 + (ycosθ−xsinθ)^2 / RY^2 = 1
  //
  // parametrized:
  // x = Cx + RX*cos(t)*cos(phi) - RY*sin(t)*sin(phi)  [1]
  // y = Cy + RY*sin(t)*cos(phi) + RX*cos(t)*sin(phi)  [2]

  if Previous = nil then
  begin
    CX := X - RX*Cos(0)*Cos(XRotation) + RY*Sin(0)*Sin(XRotation);
    CY := Y - RY*Sin(0)*Cos(XRotation) - RX*Cos(0)*Sin(XRotation);
    Exit;
  end;

  XStart := T2DSegment(Previous).X;
  YStart := T2DSegment(Previous).Y;

  //  Solve by rotating everything to align the ellipse to the axises and then rotating back again
  E1 := Rotate3DPointInXY(Make3DPoint(XStart,YStart,0), Make3DPoint(0,0,0),-1*XRotation);
  E2 := Rotate3DPointInXY(Make3DPoint(X,Y,0), Make3DPoint(0,0,0),-1*XRotation);

  // parametrized:
  // CX = E1.X - RX*cos(t1)
  // CY = E1.Y - RY*sin(t1)
  // CX = E2.X - RX*cos(t2)
  // CY = E2.Y - RY*sin(t2)
  //
  // E1.X - RX*cos(t1) = E2.X - RX*cos(t2)
  // E1.Y - RY*sin(t1) = E2.Y - RY*sin(t2)
  //
  // (- E1.X + RX*cos(t1) + E2.X)/RX = cos(t2)
  // arccos((- E1.X + RX*cos(t1) + E2.X)/RX) = t2
  //
  // E1.Y - RY*sin(t1) = E2.Y - RY*sin(arccos((- E1.X + RX*cos(t1) + E2.X)/RX))

  // SolveNumerically

  lT1 := SolveNumericallyAngle(AlignedEllipseCenterEquationT1, 0.0001, 20);

  CX1 := E1.X - RX*cos(lt1);
  CY1 := E1.Y - RY*sin(lt1);

  // Rotate back!
  RotatedCenter := Rotate3DPointInXY(Make3DPoint(CX1,CY1,0), Make3DPoint(0,0,0),XRotation);
  CX1 := RotatedCenter.X;
  CY1 := RotatedCenter.Y;

  // The other ellipse is simetrically positioned
  if (CX1 > Xstart) then
    CX2 := X - (CX1-Xstart)
  else
    CX2 := Xstart - (CX1-X);
  //
  if (CY1 > Y) then
    CY2 := Ystart - (CY1-Y)
  else
    CY2 := Y - (CY1-Ystart);

  // Achar qual é a da esquerda e qual a da direita
  if CX1 < CX2 then
  begin
    LeftMostX := CX1;
    LeftMostY := CY1;
    RightMostX := CX2;
    RightMostY := CY2;
  end
  else
  begin
    LeftMostX := CX2;
    LeftMostY := CY2;
    RightMostX := CX1;
    RightMostY := CY1;
  end;

  if LeftmostEllipse then
  begin
    CX := LeftMostX;
    CY := LeftMostY;
  end
  else
  begin
    CX := RightMostX;
    CY := RightMostY;
  end;
end;

procedure T2DEllipticalArcSegment.CalculateEllipseBoundingBox(ADest: TFPCustomCanvas;
  var ALeft, ATop, ARight, ABottom: Double);
var
  MajorAxis, MinorAxis: Double;
  t1, t2, t3: Double;
  x1, x2, x3: Double;
  y1, y2, y3: Double;
begin
  ALeft := 0;
  ATop := 0;
  ARight := 0;
  ABottom := 0;
  if Previous = nil then Exit;

  // Alligned Ellipse equation:
  // x^2 / RX^2 + Y^2 / RY^2 = 1
  //
  // Rotated Ellipse equation:
  // (xcosθ+ysinθ)^2 / RX^2 + (ycosθ−xsinθ)^2 / RY^2 = 1
  //
  // parametrized:
  // x = Cx + a*cos(t)*cos(phi) - b*sin(t)*sin(phi)  [1]
  // y = Cy + b*sin(t)*cos(phi) + a*cos(t)*sin(phi)  [2]
  // ...where ellipse has centre (h,k) semimajor axis a and semiminor axis b, and is rotated through angle phi.
  //
  // You can then differentiate and solve for gradient = 0:
  // 0 = dx/dt = -a*sin(t)*cos(phi) - b*cos(t)*sin(phi)
  // => tan(t) = -b*tan(phi)/a   [3]
  // => t = arctan(-b*tan(phi)/a) + n*Pi   [4]
  //
  // And the same for Y
  // 0 = dy/dt = b*cos(t)*cos(phi) - a*sin(t)*sin(phi)
  // a*sin(t)/cos(t) = b*cos(phi)/sin(phi)
  // => tan(t) = b*cotan(phi)/a
  // => t = arctan(b*cotan(phi)/a) + n*Pi   [5]
  //
  // calculate some values of t for n in -1, 0, 1 and see which are the smaller, bigger ones
  // done!

  CalculateCenter();

  if XRotation = 0 then
  begin
    ALeft := CX-RX;
    ARight := CX+RX;
    ATop := CY-RY;
    ABottom := CY+RY;
  end
  else
  begin
    // Search for the minimum and maximum X
    t1 := arctan(-RY*tan(XRotation)/RX);
    t2 := arctan(-RY*tan(XRotation)/RX) + Pi/2;
    t3 := arctan(-RY*tan(XRotation)/RX) + Pi;

    x1 := Cx + RX*Cos(t1)*Cos(XRotation)-RY*Sin(t1)*Sin(XRotation);
    x2 := Cx + RX*Cos(t2)*Cos(XRotation)-RY*Sin(t2)*Sin(XRotation);
    x3 := Cx + RX*Cos(t3)*Cos(XRotation)-RY*Sin(t3)*Sin(XRotation);

    ALeft := Min(x1, x2);
    ALeft := Min(ALeft, x3);

    ARight := Max(x1, x2);
    ARight := Max(ARight, x3);

    // Now the same for Y

    t1 := arctan(RY*cotan(XRotation)/RX);
    t2 := arctan(RY*cotan(XRotation)/RX) + Pi/2;
    t3 := arctan(RY*cotan(XRotation)/RX) + 3*Pi/2;

    y1 := CY + RY*Sin(t1)*Cos(XRotation)+RX*Cos(t1)*Sin(XRotation);
    y2 := CY + RY*Sin(t2)*Cos(XRotation)+RX*Cos(t2)*Sin(XRotation);
    y3 := CY + RY*Sin(t3)*Cos(XRotation)+RX*Cos(t3)*Sin(XRotation);

    ATop := Min(y1, y2);
    ATop := Min(ATop, y3);

    ABottom := Max(y1, y2);
    ABottom := Max(ABottom, y3);
  end;
end;

{ TvVerticalFormulaStack }

function TvVerticalFormulaStack.CalculateHeight(ADest: TFPCustomCanvas): Double;
var
  lElement: TvFormulaElement;
begin
  Result := 0;
  lElement := GetFirstElement();
  while lElement <> nil do
  begin
    Result := Result + lElement.CalculateHeight(ADest) + SpacingBetweenElementsY;
    lElement := GetNextElement;
  end;
  // Remove an extra spacing, since it is added even to the last item
  Result := Result - SpacingBetweenElementsY;
  // Cache the result
  Height := Result;
end;

function TvVerticalFormulaStack.CalculateWidth(ADest: TFPCustomCanvas): Double;
var
  lElement: TvFormulaElement;
begin
  Result := 0;

  lElement := GetFirstElement();
  while lElement <> nil do
  begin
    Result := Max(Result, lElement.CalculateWidth(ADest));
    lElement := GetNextElement;
  end;

  // Cache the result
  Width := Result;
end;

procedure TvVerticalFormulaStack.PositionSubparts(ADest: TFPCustomCanvas;
  ABaseX, ABaseY: Double);
var
  lElement: TvFormulaElement;
  lPosX: Double = 0;
  lPosY: Double = 0;
begin
  CalculateHeight(ADest);
  CalculateWidth(ADest);
  Left := ABaseX;
  Top := ABaseY;

  // Then calculate the position of each element
  lElement := GetFirstElement();
  while lElement <> nil do
  begin
    lElement.Left := Left;
    lElement.Top := Top - lPosY;
    lPosY := lPosY + lElement.Height + SpacingBetweenElementsY;

    lElement.PositionSubparts(ADest, ABaseX, ABaseY);

    lElement := GetNextElement();
  end;
end;

procedure TvVerticalFormulaStack.CalculateBoundingBox(ADest: TFPCustomCanvas;
  var ALeft, ATop, ARight, ABottom: Double);
begin
  inherited CalculateBoundingBox(ADest, ALeft, ATop, ARight, ABottom);
end;

procedure TvVerticalFormulaStack.Render(ADest: TFPCustomCanvas;
  ARenderInfo: TvRenderInfo; ADestX: Integer; ADestY: Integer; AMulX: Double;
  AMulY: Double);
begin
  inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
end;

{ TPathSegment }

procedure TPathSegment.Move(ADeltaX, ADeltaY: Double);
begin

end;

procedure TPathSegment.Rotate(AAngle: Double; ABase: T3DPoint);
begin

end;

procedure TPathSegment.CalculateBoundingBox(ADest: TFPCustomCanvas; var ALeft,
  ATop, ARight, ABottom: Double);
begin
  ALeft := 0;
  ATop := 0;
  ARight := 0;
  ABottom := 0;
end;

function TPathSegment.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr, lTypeStr: string;
begin
  lTypeStr := GetEnumName(TypeInfo(TSegmentType), integer(SegmentType));
  lStr := Format('[%s] Type=%s', [Self.ClassName, lTypeStr]);
  Result := ADestRoutine(lStr, APageItem);
end;

{ T2DSegment }

procedure T2DSegment.Move(ADeltaX, ADeltaY: Double);
begin
  X := X + ADeltaX;
  Y := Y + ADeltaY;
end;

procedure T2DSegment.Rotate(AAngle: Double; ABase: T3DPoint);
var
  lRes: T3DPoint;
begin
  inherited Rotate(AAngle, ABase);
  lRes := fpvutils.Rotate3DPointInXY(Make3DPoint(X, Y, 0), ABase, AAngle);
  X := lRes.X;
  Y := lRes.Y;
end;

function T2DSegment.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr, lTypeStr: string;
begin
  lTypeStr := GetEnumName(TypeInfo(TSegmentType), integer(SegmentType));
  lStr := Format('[%s] Type=%s X=%f Y=%f', [Self.ClassName, lTypeStr, X, Y]);
  Result := ADestRoutine(lStr, APageItem);
end;

{ T2DBezierSegment }

procedure T2DBezierSegment.Move(ADeltaX, ADeltaY: Double);
begin
  inherited Move(ADeltaX, ADeltaY);
  X2 := X2 + ADeltaX;
  Y2 := Y2 + ADeltaY;
  X3 := X3 + ADeltaX;
  Y3 := Y3 + ADeltaY;
end;

function T2DBezierSegment.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr: string;
begin
  lStr := Format('[%s] X=%f Y=%f CX2=%f CY2=%f CX3=%f CY3=%f', [Self.ClassName, X, Y, X2, Y2, X3, Y3]);
  Result := ADestRoutine(lStr, APageItem);
end;

{ T3DSegment }

procedure T3DSegment.Move(ADeltaX, ADeltaY: Double);
begin
  X := X + ADeltaX;
  Y := Y + ADeltaY;
end;

{ T3DBezierSegment }

procedure T3DBezierSegment.Move(ADeltaX, ADeltaY: Double);
begin
  inherited Move(ADeltaX, ADeltaY);
  X2 := X2 + ADeltaX;
  Y2 := Y2 + ADeltaY;
  X3 := X3 + ADeltaX;
  Y3 := Y3 + ADeltaY;
end;

{ TvEntity }

constructor TvEntity.Create;
begin
end;

procedure TvEntity.Clear;
begin
  X := 0.0;
  Y := 0.0;
  Z := 0.0;
end;

procedure TvEntity.CalculateBoundingBox(ADest: TFPCustomCanvas; var ALeft, ATop, ARight, ABottom: Double);
begin
  ALeft := X;
  ATop := Y;
  ARight := X+1;
  ABottom := Y+1;
end;

procedure TvEntity.ExpandBoundingBox(ADest: TFPCustomCanvas; var ALeft, ATop, ARight, ABottom: Double);
var
  lLeft, lTop, lRight, lBottom: Double;
begin
  CalculateBoundingBox(ADest, lLeft, lTop, lRight, lBottom);
  if lLeft < ALeft then ALeft := lLeft;
  if lTop < ATop then ATop := lTop;
  if lRight > ARight then ARight := lRight;
  if lBottom > ABottom then ABottom := lBottom;
end;

function TvEntity.TryToSelect(APos: TPoint; var ASubpart: Cardinal): TvFindEntityResult;
begin
  Result := vfrNotFound;
end;

procedure TvEntity.Move(ADeltaX, ADeltaY: Double);
begin
  X := X + ADeltaX;
  Y := Y + ADeltaY;
end;

procedure TvEntity.MoveSubpart(ADeltaX, ADeltaY: Double; ASubpart: Cardinal);
begin

end;

function TvEntity.GetSubpartCount: Integer;
begin
  Result := 0;
end;

procedure TvEntity.PositionSubparts(ADest: TFPCustomCanvas; ABaseX,
  ABaseY: Double);
begin

end;

procedure TvEntity.Rotate(AAngle: Double; ABase: T3DPoint);
begin

end;

procedure TvEntity.Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double);
begin

end;

function TvEntity.AdjustColorToBackground(AColor: TFPColor; ARenderInfo: TvRenderInfo): TFPColor;
begin
  Result := AColor;
  if not ARenderInfo.AdjustPenColorToBackground then Exit;
  // Adjust only if the contranst is really low
  if (Abs(AColor.Red - ARenderInfo.BackgroundColor.Red) <= $100) and
     (Abs(AColor.Green - ARenderInfo.BackgroundColor.Green) <= $100) and
     (Abs(AColor.Blue - ARenderInfo.BackgroundColor.Blue) <= $100) then
  begin
    if (ARenderInfo.BackgroundColor.Red <= $1000) and
       (ARenderInfo.BackgroundColor.Green <= $1000) and
       (ARenderInfo.BackgroundColor.Blue <= $1000) then
      Result := colWhite
    else Result := colBlack;
  end;
end;

function TvEntity.GetNormalizedPos(APage: TvVectorialPage; ANewMin,
  ANewMax: Double): T3DPoint;
begin
  Result.X := (X - APage.MinX) * (ANewMax - ANewMin) / (APage.MaxX - APage.MinX) + ANewMin;
  Result.Y := (Y - APage.MinY) * (ANewMax - ANewMin) / (APage.MaxY - APage.MinY) + ANewMin;
  Result.Z := (Z - APage.MinZ) * (ANewMax - ANewMin) / (APage.MaxZ - APage.MinZ) + ANewMin;
end;

function TvEntity.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr: string;
begin
  lStr := Format('[%s] X=%f Y=%f', [Self.ClassName, X, Y]);
  Result := ADestRoutine(lStr, APageItem);
end;

function TvEntity.GenerateDebugStrForFPColor(AColor: TFPColor): string;
begin
  Result := IntToHex(AColor.Red div $100, 2) + IntToHex(AColor.Green div $100, 2) + IntToHex(AColor.Blue div $100, 2) + IntToHex(AColor.Alpha div $100, 2);
end;

{ TvNamedEntity }

function TvNamedEntity.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr: string;
begin
  lStr := Format('[%s] Name="%s" X=%f Y=%f' + FExtraDebugStr, [Self.ClassName, Name, X, Y]);
  Result := ADestRoutine(lStr, APageItem);
end;

{ TvEntityWithPen }

constructor TvEntityWithPen.Create;
begin
  inherited Create;
  Pen.Style := psSolid;
  Pen.Color := colBlack;
  Pen.Width := 1;
end;

procedure TvEntityWithPen.ApplyPenToCanvas(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo);
begin
  ADest.Pen.FPColor := AdjustColorToBackground(Pen.Color, ARenderInfo);
  ADest.Pen.Width := 1;//Pen.Width;
  ADest.Pen.Style := Pen.Style;
end;

procedure TvEntityWithPen.AssignPen(APen: TvPen);
begin
  Pen.Style := APen.Style;
  Pen.Color := APen.Color;
  Pen.Width := APen.Width;
end;

procedure TvEntityWithPen.Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double);
begin
  inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
  ApplyPenToCanvas(ADest, ARenderInfo);
end;

{ TvEntityWithPenAndBrush }

constructor TvEntityWithPenAndBrush.Create;
begin
  inherited Create;
  Brush.Style := bsClear;
  Brush.Color := colBlue;
end;

procedure TvEntityWithPenAndBrush.ApplyBrushToCanvas(ADest: TFPCustomCanvas);
begin
  ADest.Brush.FPColor := Brush.Color;
  ADest.Brush.Style := Brush.Style;
end;

procedure TvEntityWithPenAndBrush.AssignBrush(ABrush: TvBrush);
begin
  Brush.Style := ABrush.Style;
  Brush.Color := ABrush.Color;
end;

procedure TvEntityWithPenAndBrush.Render(ADest: TFPCustomCanvas;
   ARenderInfo: TvRenderInfo; ADestX: Integer; ADestY: Integer; AMulX: Double; AMulY: Double);
begin
  inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
  ApplyBrushToCanvas(ADest);
end;

function TvEntityWithPenAndBrush.GenerateDebugTree(
  ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer;
var
  lStr: string;
  lCurPathSeg: TPathSegment;
begin
  lStr := Format('[%s] Name=%s Pen.Color=%s Pen.Style=%s Brush.Color=%s Brush.Style=%s',
    [Self.ClassName, Self.Name,
    GenerateDebugStrForFPColor(Pen.Color),
    GetEnumName(TypeInfo(TFPPenStyle), integer(Pen.Style)),
    GenerateDebugStrForFPColor(Brush.Color),
    GetEnumName(TypeInfo(TFPBrushStyle), integer(Brush.Style))
    ]);
  Result := ADestRoutine(lStr, APageItem);
end;


{ TvEntityWithPenBrushAndFont }

constructor TvEntityWithPenBrushAndFont.Create;
begin
  inherited Create;
  Font.Color := colBlack;
end;

procedure TvEntityWithPenBrushAndFont.ApplyFontToCanvas(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; AMulX: Double = 1.0);
var
  i: Integer;
  {$ifdef USE_LCL_CANVAS}
  ALCLDest: TCanvas absolute ADest;
  {$endif}
  //
  LowerDim: T3DPoint;
begin
  ADest.Font.Size := Round(AmulX * Font.Size);
  ADest.Font.Bold := Font.Bold;
  ADest.Font.Italic := Font.Italic;
  ADest.Font.Underline := Font.Underline;
  {$IF (FPC_FULLVERSION<=20600) or (FPC_FULLVERSION=20602)}
  ADest.Font.StrikeTrough := Font.StrikeThrough; //old version with typo
  {$ELSE}
  ADest.Font.StrikeThrough := Font.StrikeThrough;
  {$ENDIF}

  {$ifdef USE_LCL_CANVAS}
  ALCLDest.Font.Orientation := Round(Font.Orientation * 16);
  {$endif}
  ADest.Font.FPColor := AdjustColorToBackground(Font.Color, ARenderInfo);
end;

procedure TvEntityWithPenBrushAndFont.AssignFont(AFont: TvFont);
begin
  Font.Color := AFont.Color;
  Font.Size := AFont.Size;
  Font.Name := AFont.Name;
  Font.Orientation := AFont.Orientation;
  Font.Bold := AFont.Bold;
  Font.Italic := AFont.Italic;
  Font.Underline := AFont.Underline;
  Font.StrikeThrough := AFont.StrikeThrough;
end;

procedure TvEntityWithPenBrushAndFont.Render(ADest: TFPCustomCanvas;
  ARenderInfo: TvRenderInfo; ADestX: Integer; ADestY: Integer; AMulX: Double;
  AMulY: Double);
begin
  inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
  ApplyFontToCanvas(ADest, ARenderInfo, AMulX);
end;

function TvEntityWithPenBrushAndFont.GenerateDebugTree(
  ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer;
var
  lStr: string;
  lCurPathSeg: TPathSegment;
begin
  Result := inherited GenerateDebugTree(ADestRoutine, APageItem);
  // Add the font debug info in a sub-item
  lStr := Format('[Font] Color=%s Size=%d Name=%s Orientation=%f Bold=%s Italic=%s Underline=%s StrikeThrough=%s',
    [GenerateDebugStrForFPColor(Font.Color),
    Font.Size, Font.Name, Font.Orientation,
    BoolToStr(Font.Bold),
    BoolToStr(Font.Italic),
    BoolToStr(Font.Underline),
    BoolToStr(Font.StrikeThrough)
    ]);
  ADestRoutine(lStr, Result);
end;

{ TPath }

constructor TPath.Create;
begin
  inherited Create;
  FCurMoveSubPartIndex := -1;
end;

//GM: Follow the path to cleanly release the chained list!
destructor TPath.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TPath.Clear;
var
  p, pp, np: TPathSegment;
begin
  p:=PointsEnd;
  if (p<>nil) then
  begin
    np:=p.Next;
    while (p<>nil) do
    begin
      pp:=p.Previous;
      p.Next:=nil;
      p.Previous:=nil;
      FreeAndNil(p);
      p:=pp;
    end;
    p:=np;
    while (p<>nil) do
    begin
      np:=p.Next;
      p.Next:=nil;
      p.Previous:=nil;
      FreeAndNil(p);
      p:=np;
    end;
  end;
  PointsEnd:=nil;
  Points:=nil;

  inherited Clear;
end;

procedure TPath.Assign(ASource: TPath);
begin
  Len := ASource.Len;
  Points := ASource.Points;
  PointsEnd := ASource.PointsEnd;
  CurPoint := ASource.CurPoint;
  Pen := ASource.Pen;
  Brush := ASource.Brush;
  ClipPath := ASource.ClipPath;
  ClipMode := ASource.ClipMode;
end;

procedure TPath.PrepareForSequentialReading;
begin
  CurPoint := nil;
end;

function TPath.Next(): TPathSegment;
begin
  if CurPoint = nil then Result := Points
  else Result := CurPoint.Next;

  CurPoint := Result;
end;

procedure TPath.CalculateBoundingBox(ADest: TFPCustomCanvas; var ALeft, ATop, ARight, ABottom: Double);
var
  lSegment: TPathSegment;
  l2DSegment: T2DSegment;
  lFirstValue: Boolean = True;
begin
  inherited CalculateBoundingBox(ADest, ALeft, ATop, ARight, ABottom);

  PrepareForSequentialReading();
  lSegment := Next();
  while lSegment <> nil do
  begin
    if lSegment is T2DSegment then
    begin
      l2DSegment := T2DSegment(lSegment);
      if lFirstValue then
      begin
        ALeft := l2DSegment.X;
        ATop := l2DSegment.Y;
        ARight := l2DSegment.X;
        ABottom := l2DSegment.Y;
        lFirstValue := False;
      end
      else
      begin
        if l2DSegment.X < ALeft then ALeft := l2DSegment.X;
        if l2DSegment.Y < ATop then ATop := l2DSegment.Y;
        if l2DSegment.X > ARight then ARight := l2DSegment.X;
        if l2DSegment.Y > ABottom then ABottom := l2DSegment.Y;
      end;
    end;

    lSegment := Next();
  end;
end;

procedure TPath.AppendSegment(ASegment: TPathSegment);
var
  L: Integer;
begin
  // Check if we are the first segment in the tmp path
  if PointsEnd = nil then
  begin
    if Len <> 0 then
      Exception.Create('[TPath.AppendSegment] Assertion failed Len <> 0 with PointsEnd = nil');

    Points := ASegment;
    PointsEnd := ASegment;
    Len := 1;
    Exit;
  end;

  L := Len;
  Inc(Len);

  // Adds the element to the end of the list
  PointsEnd.Next := ASegment;
  ASegment.Previous := PointsEnd;
  PointsEnd := ASegment;
end;

procedure TPath.AppendMoveToSegment(AX, AY: Double);
var
  segment: T2DSegment;
begin
  segment := T2DSegment.Create;
  segment.SegmentType := stMoveTo;
  segment.X := AX;
  segment.Y := AY;
  AppendSegment(segment);
end;

procedure TPath.AppendLineToSegment(AX, AY: Double);
var
  segment: T2DSegment;
begin
  segment := T2DSegment.Create;
  segment.SegmentType := st2DLine;
  segment.X := AX;
  segment.Y := AY;
  AppendSegment(segment);
end;

procedure TPath.Move(ADeltaX, ADeltaY: Double);
var
  i: Integer;
begin
  inherited Move(ADeltaX, ADeltaY);
  for i := 0 to GetSubpartCount()-1 do
  begin
    MoveSubpart(ADeltaX, ADeltaY, i);
  end;
end;

procedure TPath.MoveSubpart(ADeltaX, ADeltaY: Double; ASubpart: Cardinal);
var
  lCurPart: TPathSegment;
begin
  if (ASubPart < 0) or (ASubPart > Len) then
    raise Exception.Create(Format('[TPath.MoveSubpart] Invalid index %d', [ASubpart]));

  // Move to the subpart
  lCurPart := MoveToSubpart(ASubpart);

  // Do the change
  lCurPart.Move(ADeltaX, ADeltaY);
end;

function TPath.MoveToSubpart(ASubpart: Cardinal): TPathSegment;
var
  i: Integer;
begin
  if (ASubPart < 0) or (ASubPart > Len) then
    raise Exception.Create(Format('[TPath.MoveToSubpart] Invalid index %d', [ASubpart]));

  // Move to the subpart
  if (ASubPart = FCurMoveSubPartIndex) then
  begin
    Result := FCurMoveSubPartSegment;
  end
  else if (FCurMoveSubPartSegment <> nil) and (ASubPart = FCurMoveSubPartIndex + 1) then
  begin
    Result := FCurMoveSubPartSegment.Next;
    FCurMoveSubPartIndex := FCurMoveSubPartIndex + 1;
    FCurMoveSubPartSegment := Result;
  end
  else if (FCurMoveSubPartSegment <> nil) and (ASubPart = FCurMoveSubPartIndex - 1) then
  begin
    Result := FCurMoveSubPartSegment.Previous;
    FCurMoveSubPartIndex := FCurMoveSubPartIndex - 1;
    FCurMoveSubPartSegment := Result;
  end
  else
  begin
    Result := Points;

    for i := 0 to ASubpart-1 do
      Result := Result.Next;

    FCurMoveSubPartIndex := ASubpart;
    FCurMoveSubPartSegment := Result;
  end;
end;

function TPath.GetSubpartCount: Integer;
begin
  Result := Len;
end;

procedure TPath.Rotate(AAngle: Double; ABase: T3DPoint);
var
  i: Integer;
  lCurPart: TPathSegment;
begin
  inherited Rotate(AAngle, ABase);
  for i := 0 to GetSubpartCount()-1 do
  begin
    // Move to the subpart
    lCurPart := MoveToSubpart(i);
    // Rotate it
    lCurPart.Rotate(AAngle, ABase);
  end;
end;

procedure TPath.Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double);

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  j, k: Integer;
  PosX, PosY: Double; // Not modified by ADestX, etc
  CoordX, CoordY: Integer;
  CurSegment: TPathSegment;
  Cur2DSegment: T2DSegment absolute CurSegment;
  Cur2DBSegment: T2DBezierSegment absolute CurSegment;
  Cur2DArcSegment: T2DEllipticalArcSegment absolute CurSegment;
  // For bezier
  CurX, CurY: Integer; // Not modified by ADestX, etc
  CoordX2, CoordY2, CoordX3, CoordY3, CoordX4, CoordY4: Integer;
  CurveLength: Integer;
  t: Double;
  // For polygons
  Points: array of TPoint;
  // for elliptical arcs
  BoxLeft, BoxTop, BoxRight, BoxBottom: Double;
  EllipseRect: TRect;
   // Clipping Region
  {$ifdef USE_LCL_CANVAS}
  ClipRegion, OldClipRegion: HRGN;
  ACanvas: TCanvas absolute ADest;
  {$endif}
begin
  PosX := 0;
  PosY := 0;
  ADest.Brush.Style := bsClear;

  ADest.MoveTo(ADestX, ADestY);

  // Set the path Pen and Brush options
  ADest.Pen.Style := Pen.Style;
  ADest.Pen.Width := Round(Pen.Width * AMulX);
  if ADest.Pen.Width < 1 then ADest.Pen.Width := 1;
  if (Pen.Width <= 2) and (ADest.Pen.Width > 2) then ADest.Pen.Width := 2;
  if (Pen.Width <= 5) and (ADest.Pen.Width > 5) then ADest.Pen.Width := 5;
  ADest.Pen.FPColor := AdjustColorToBackground(Pen.Color, ARenderInfo);
  ADest.Brush.FPColor := Brush.Color;

  // Prepare the Clipping Region, if any
  {$ifdef USE_CANVAS_CLIP_REGION}
  if ClipPath <> nil then
  begin
    OldClipRegion := LCLIntf.CreateEmptyRegion();
    GetClipRgn(ACanvas.Handle, OldClipRegion);
    ClipRegion := ConvertPathToRegion(ClipPath, ADestX, ADestY, AMulX, AMulY);
    SelectClipRgn(ACanvas.Handle, ClipRegion);
    DeleteObject(ClipRegion);
    // debug info
    {$ifdef DEBUG_CANVAS_CLIP_REGION}
    ConvertPathToPoints(CurPath.ClipPath, ADestX, ADestY, AMulX, AMulY, Points);
    ACanvas.Polygon(Points);
    {$endif}
  end;
  {$endif}

  // useful in some paths, like stars!
  RenderInternalPolygon(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);

  //
  // For other paths, draw more carefully
  //
  ADest.Pen.Style := Pen.Style;
  PrepareForSequentialReading;

  for j := 0 to Len - 1 do
  begin
    //WriteLn('j = ', j);
    CurSegment := TPathSegment(Next());

    case CurSegment.SegmentType of
    stMoveTo:
    begin
      CoordX := CoordToCanvasX(Cur2DSegment.X);
      CoordY := CoordToCanvasY(Cur2DSegment.Y);
      ADest.MoveTo(CoordX, CoordY);
      PosX := Cur2DSegment.X;
      PosY := Cur2DSegment.Y;
      {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
      Write(Format(' M%d,%d', [CoordY, CoordY]));
      {$endif}
    end;
    // This element can override temporarely the Pen
    st2DLineWithPen:
    begin
      ADest.Pen.FPColor := AdjustColorToBackground(T2DSegmentWithPen(Cur2DSegment).Pen.Color, ARenderInfo);

      CoordX := CoordToCanvasX(PosX);
      CoordY := CoordToCanvasY(PosY);
      CoordX2 := CoordToCanvasX(Cur2DSegment.X);
      CoordY2 := CoordToCanvasY(Cur2DSegment.Y);
      ADest.Line(CoordX, CoordY, CoordX2, CoordY2);

      PosX := Cur2DSegment.X;
      PosY := Cur2DSegment.Y;

      ADest.Pen.FPColor := Pen.Color;

      {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
      Write(Format(' L%d,%d', [CoordToCanvasX(Cur2DSegment.X), CoordToCanvasY(Cur2DSegment.Y)]));
      {$endif}
    end;
    st2DLine, st3DLine:
    begin
      CoordX := CoordToCanvasX(PosX);
      CoordY := CoordToCanvasY(PosY);
      CoordX2 := CoordToCanvasX(Cur2DSegment.X);
      CoordY2 := CoordToCanvasY(Cur2DSegment.Y);
      ADest.Line(CoordX, CoordY, CoordX2, CoordY2);
      PosX := Cur2DSegment.X;
      PosY := Cur2DSegment.Y;
      {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
      Write(Format(' L%d,%d', [CoordX, CoordY]));
      {$endif}
    end;
    { To draw a bezier we need to divide the interval in parts and make
      lines between this parts }
    st2DBezier, st3DBezier:
    begin
      CoordX := CoordToCanvasX(PosX);
      CoordY := CoordToCanvasY(PosY);
      CoordX2 := CoordToCanvasX(Cur2DBSegment.X2);
      CoordY2 := CoordToCanvasY(Cur2DBSegment.Y2);
      CoordX3 := CoordToCanvasX(Cur2DBSegment.X3);
      CoordY3 := CoordToCanvasY(Cur2DBSegment.Y3);
      CoordX4 := CoordToCanvasX(Cur2DBSegment.X);
      CoordY4 := CoordToCanvasY(Cur2DBSegment.Y);
      SetLength(Points, 0);
      AddBezierToPoints(
        Make2DPoint(CoordX, CoordY),
        Make2DPoint(CoordX2, CoordY2),
        Make2DPoint(CoordX3, CoordY3),
        Make2DPoint(CoordX4, CoordY4),
        Points
      );

      ADest.Brush.Style := Brush.Style;
      if Length(Points) >= 3 then
        ADest.Polygon(Points);

      PosX := Cur2DSegment.X;
      PosY := Cur2DSegment.Y;

      {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
      Write(Format(' ***C%d,%d %d,%d %d,%d %d,%d',
        [CoordToCanvasX(PosX), CoordToCanvasY(PosY),
         CoordToCanvasX(Cur2DBSegment.X2), CoordToCanvasY(Cur2DBSegment.Y2),
         CoordToCanvasX(Cur2DBSegment.X3), CoordToCanvasY(Cur2DBSegment.Y3),
         CoordToCanvasX(Cur2DBSegment.X), CoordToCanvasY(Cur2DBSegment.Y)]));
      {$endif}
    end;
    // Alligned Ellipse equation:
    // x^2 / RX^2 + Y^2 / RY^2 = 1
    //
    // Rotated Ellipse equation:
    // (xcosθ+ysinθ)^2 / RX^2 + (ycosθ−xsinθ)^2 / RY^2 = 1
    //
    // parametrized:
    // x = Cx + a*cos(t)*cos(phi) - b*sin(t)*sin(phi)  [1]
    // y = Cy + b*sin(t)*cos(phi) + a*cos(t)*sin(phi)  [2]
    // ...where ellipse has centre (h,k) semimajor axis a and semiminor axis b, and is rotated through angle phi.
    //
    // You can then differentiate and solve for gradient = 0:
    // 0 = dx/dt = -a*sin(t)*cos(phi) - b*cos(t)*sin(phi)
    // => tan(t) = -b*tan(phi)/a   [3]
    // => t = arctan(-b*tan(phi)/a) + n*Pi   [4]
    //
    // calculate some values of t for n in -2, -1, 0, 1, 2 and see which are the smaller, bigger ones
    // done!
    st2DEllipticalArc:
    begin
      CoordX := CoordToCanvasX(PosX);
      CoordY := CoordToCanvasY(PosY);
      CoordX2 := CoordToCanvasX(Cur2DArcSegment.RX);
      CoordY2 := CoordToCanvasY(Cur2DArcSegment.RY);
      CoordX3 := CoordToCanvasX(Cur2DArcSegment.XRotation);
      CoordX4 := CoordToCanvasX(Cur2DArcSegment.X);
      CoordY4 := CoordToCanvasY(Cur2DArcSegment.Y);
      SetLength(Points, 0);

      Cur2DArcSegment.CalculateEllipseBoundingBox(nil, BoxLeft, BoxTop, BoxRight, BoxBottom);

      EllipseRect.Left := CoordToCanvasX(BoxLeft);
      EllipseRect.Top := CoordToCanvasY(BoxTop);
      EllipseRect.Right := CoordToCanvasX(BoxRight);
      EllipseRect.Bottom := CoordToCanvasY(BoxBottom);

      {$ifdef FPVECTORIAL_TOCANVAS_ELLIPSE_VISUALDEBUG}
      ACanvas.Pen.Color := clRed;
      ACanvas.Brush.Style := bsClear;
      ACanvas.Rectangle(
        EllipseRect.Left, EllipseRect.Top, EllipseRect.Right, EllipseRect.Bottom);
      {$endif}

      ADest.Brush.Style := Brush.Style;

      // Arc draws counterclockwise
      if Cur2DArcSegment.ClockwiseArcFlag then
      begin
        ACanvas.Arc(
          EllipseRect.Left, EllipseRect.Top, EllipseRect.Right, EllipseRect.Bottom,
          CoordX4, CoordY4, CoordX, CoordY);
      end
      else
      begin
        ACanvas.Arc(
          EllipseRect.Left, EllipseRect.Top, EllipseRect.Right, EllipseRect.Bottom,
          CoordX, CoordY, CoordX4, CoordY4);
      end;

      PosX := Cur2DArcSegment.X;
      PosY := Cur2DArcSegment.Y;

      {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
      {Write(Format(' ***C%d,%d %d,%d %d,%d %d,%d',
        [CoordToCanvasX(PosX), CoordToCanvasY(PosY),
         CoordToCanvasX(Cur2DBSegment.X2), CoordToCanvasY(Cur2DBSegment.Y2),
         CoordToCanvasX(Cur2DBSegment.X3), CoordToCanvasY(Cur2DBSegment.Y3),
         CoordToCanvasX(Cur2DBSegment.X), CoordToCanvasY(Cur2DBSegment.Y)]));}
      {$endif}
    end;
    end;
  end;
  {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
  WriteLn('');
  {$endif}

  // Restores the previous Clip Region
  {$ifdef USE_CANVAS_CLIP_REGION}
  if ClipPath <> nil then
  begin
    SelectClipRgn(ACanvas.Handle, OldClipRegion); //Using OldClipRegion crashes in Qt
  end;
  {$endif}
end;

procedure TPath.RenderInternalPolygon(ADest: TFPCustomCanvas;
  ARenderInfo: TvRenderInfo; ADestX: Integer; ADestY: Integer; AMulX: Double;
  AMulY: Double);

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  j, k: Integer;
  CoordX, CoordY: Integer;
  CurSegment: TPathSegment;
  Cur2DSegment: T2DSegment absolute CurSegment;
  Cur2DBSegment: T2DBezierSegment absolute CurSegment;
  Cur2DArcSegment: T2DEllipticalArcSegment absolute CurSegment;
  // For bezier
  // For polygons
  MultiPoints: array of array of TPoint;
  lCurPoligon, lCurPoligonStartIndex: Integer;
begin
  //
  // For solid paths, draw a polygon for the main internal area
  //
  // If there is a move-to in the middle of the path, we should
  // draw then multiple poligons
  //
  if Brush.Style <> bsClear then
  begin
    PrepareForSequentialReading;

    {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
    Write(' Solid Path Internal Area');
    {$endif}
    ADest.Brush.Style := Brush.Style;
    ADest.Pen.Style := psClear;

    SetLength(MultiPoints, 1);
    SetLength(MultiPoints[0], Len);
    lCurPoligon := 0;
    lCurPoligonStartIndex := 0;

    for j := 0 to Len - 1 do
    begin
      //WriteLn('j = ', j);
      CurSegment := TPathSegment(Next());

      if (j > 0) and (CurSegment.SegmentType = stMoveTo) then
      begin
        SetLength(MultiPoints[lCurPoligon], j-lCurPoligonStartIndex);
        Inc(lCurPoligon);
        SetLength(MultiPoints, lCurPoligon+1);
        SetLength(MultiPoints[lCurPoligon], Len);
        lCurPoligonStartIndex := j;
      end;

      CoordX := CoordToCanvasX(Cur2DSegment.X);
      CoordY := CoordToCanvasY(Cur2DSegment.Y);

      MultiPoints[lCurPoligon][j-lCurPoligonStartIndex].X := CoordX;
      MultiPoints[lCurPoligon][j-lCurPoligonStartIndex].Y := CoordY;

      {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
      Write(Format(' P%d,%d', [CoordY, CoordY]));
      {$endif}
    end;

    // Cut off excess from the last poligon
    SetLength(MultiPoints[lCurPoligon], Len-lCurPoligonStartIndex);

    // Draw each polygon now
    for j := 0 to lCurPoligon do
    begin
      ADest.Polygon(MultiPoints[j]);
    end;

    {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
    Write(' Now the details ');
    {$endif}
  end;
end;

function TPath.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr: string;
  lCurPathSeg: TPathSegment;
begin
  lStr := Format('[%s] Name=%s Pen.Color=%s Pen.Style=%s Brush.Color=%s Brush.Style=%s',
    [Self.ClassName, Self.Name,
    GenerateDebugStrForFPColor(Pen.Color),
    GetEnumName(TypeInfo(TFPPenStyle), integer(Pen.Style)),
    GenerateDebugStrForFPColor(Brush.Color),
    GetEnumName(TypeInfo(TFPBrushStyle), integer(Brush.Style))
    ]);
  Result := ADestRoutine(lStr, APageItem);
  // Add sub-entities
  PrepareForSequentialReading();
  lCurPathSeg := Next();
  while lCurPathSeg <> nil do
  begin
    lCurPathSeg.GenerateDebugTree(ADestRoutine, Result);
    lCurPathSeg := Next();
  end;
end;

{ TvText }

constructor TvText.Create;
begin
  inherited Create;
  Value := TStringList.Create;
  Font.Color := colBlack;
end;

destructor TvText.Destroy;
begin
  Value.Free;
  inherited Destroy;
end;

function TvText.TryToSelect(APos: TPoint; var ASubpart: Cardinal): TvFindEntityResult;
var
  lProximityFactor: Integer;
begin
  lProximityFactor := 5;
  if (APos.X > X - lProximityFactor) and (APos.X < X + lProximityFactor)
    and (APos.Y > Y - lProximityFactor) and (APos.Y < Y + lProximityFactor) then
    Result := vfrFound
  else Result := vfrNotFound;
end;

procedure TvText.Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double);

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  i: Integer;
  //
  LowerDim: T3DPoint;
  XAnchorAdjustment: Integer;
  lLongestLine, lLineWidth: Integer;
  {$ifdef USE_LCL_CANVAS}
  ACanvas: TCanvas absolute ADest;
  {$endif}
begin
  inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);

  // Don't draw anything if we have alpha=zero
  if Font.Color.Alpha = 0 then Exit;

  // if an anchor is set, use it
  // to do this, first search for the longest line
  lLongestLine := 0;
  for i := 0 to Value.Count - 1 do
  begin
    lLineWidth := ACanvas.TextWidth(Value.Strings[i]);
    if lLineWidth > lLongestLine then
      lLongestLine := lLineWidth;
  end;
  case TextAnchor of
  vtaMiddle: XAnchorAdjustment := -1 * lLongestLine div 2;
  vtaEnd:    XAnchorAdjustment := -1 * lLongestLine;
  else
    XAnchorAdjustment := 0;
  end;

  // TvText supports multiple lines
  for i := 0 to Value.Count - 1 do
  begin
    if Font.Size = 0 then
      LowerDim.Y := CoordToCanvasY(Y) + 12 * (i - Value.Count)
    else
    begin
      LowerDim.Y := Y + Font.Size * 1.2 * (Value.Count - i);
      LowerDim.Y := CoordToCanvasY(LowerDim.Y);
    end;

    ADest.Font.FPColor := AdjustColorToBackground(Font.Color, ARenderInfo);
    ADest.TextOut(CoordToCanvasX(X)+XAnchorAdjustment, Round(LowerDim.Y), Value.Strings[i]);
  end;
end;

function TvText.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr: string;
begin
  lStr := Format('[%s] Name=%s X=%f Y=%f Text="%s" Color=%s Size=%d Name=%s Orientation=%f Bold=%s Italic=%s Underline=%s StrikeThrough=%s TextAnchor=%s',
    [
    Self.ClassName, Name, X, Y, Value.Text,
    GenerateDebugStrForFPColor(Font.Color),
    Font.Size, Font.Name, Font.Orientation,
    BoolToStr(Font.Bold),
    BoolToStr(Font.Italic),
    BoolToStr(Font.Underline),
    BoolToStr(Font.StrikeThrough),
    GetEnumName(TypeInfo(TvTextAnchor), integer(TextAnchor))
  ]);
  Result := ADestRoutine(lStr, APageItem);
end;

{ TvCircle }

procedure TvCircle.Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double);

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

begin
  inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
  ADest.Ellipse(
    CoordToCanvasX(X - Radius),
    CoordToCanvasY(Y - Radius),
    CoordToCanvasX(X + Radius),
    CoordToCanvasY(Y + Radius)
    );
end;

{ TvCircularArc }

procedure TvCircularArc.Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double);

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  FinalStartAngle, FinalEndAngle: double;
  BoundsLeft, BoundsTop, BoundsRight, BoundsBottom,
   IntStartAngle, IntAngleLength, IntTmp: Integer;
  {$ifdef USE_LCL_CANVAS}
  ALCLDest: TCanvas absolute ADest;
  {$endif}
begin
  inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
  {$ifdef USE_LCL_CANVAS}
  // ToDo: Consider a X axis inversion
  // If the Y axis is inverted, then we need to mirror our angles as well
  BoundsLeft := CoordToCanvasX(X - Radius);
  BoundsTop := CoordToCanvasY(Y - Radius);
  BoundsRight := CoordToCanvasX(X + Radius);
  BoundsBottom := CoordToCanvasY(Y + Radius);
  {if AMulY > 0 then
  begin}
    FinalStartAngle := StartAngle;
    FinalEndAngle := EndAngle;
  {end
  else // AMulY is negative
  begin
    // Inverting the angles generates the correct result for Y axis inversion
    if CurArc.EndAngle = 0 then FinalStartAngle := 0
    else FinalStartAngle := 360 - 1* CurArc.EndAngle;
    if CurArc.StartAngle = 0 then FinalEndAngle := 0
    else FinalEndAngle := 360 - 1* CurArc.StartAngle;
  end;}
  IntStartAngle := Round(16*FinalStartAngle);
  IntAngleLength := Round(16*(FinalEndAngle - FinalStartAngle));
  // On Gtk2 and Carbon, the Left really needs to be to the Left of the Right position
  // The same for the Top and Bottom
  // On Windows it works fine either way
  // On Gtk2 if the positions are inverted then the arcs are screwed up
  // In Carbon if the positions are inverted, then the arc is inverted
  if BoundsLeft > BoundsRight then
  begin
    IntTmp := BoundsLeft;
    BoundsLeft := BoundsRight;
    BoundsRight := IntTmp;
  end;
  if BoundsTop > BoundsBottom then
  begin
    IntTmp := BoundsTop;
    BoundsTop := BoundsBottom;
    BoundsBottom := IntTmp;
  end;
  // Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer);
  {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
//    WriteLn(Format('Drawing Arc Center=%f,%f Radius=%f StartAngle=%f AngleLength=%f',
//      [CurArc.CenterX, CurArc.CenterY, CurArc.Radius, IntStartAngle/16, IntAngleLength/16]));
  {$endif}
  ALCLDest.Arc(
    BoundsLeft, BoundsTop, BoundsRight, BoundsBottom,
    IntStartAngle, IntAngleLength
    );
  // Debug info
//      {$define FPVECTORIALDEBUG}
//      {$ifdef FPVECTORIALDEBUG}
//      WriteLn(Format('Drawing Arc x1y1=%d,%d x2y2=%d,%d start=%d end=%d',
//        [BoundsLeft, BoundsTop, BoundsRight, BoundsBottom, IntStartAngle, IntAngleLength]));
//      {$endif}
{      ADest.TextOut(CoordToCanvasX(CurArc.CenterX), CoordToCanvasY(CurArc.CenterY),
    Format('R=%d S=%d L=%d', [Round(CurArc.Radius*AMulX), Round(FinalStartAngle),
    Abs(Round((FinalEndAngle - FinalStartAngle)))]));
  ADest.Pen.Color := TColor($DDDDDD);
  ADest.Rectangle(
    BoundsLeft, BoundsTop, BoundsRight, BoundsBottom);
  ADest.Pen.Color := clBlack;}
  {$endif}
end;

{ TvEllipse }

procedure TvEllipse.CalculateBoundingBox(ADest: TFPCustomCanvas; var ALeft, ATop, ARight, ABottom: Double);
var
  t, tmp: Double;
begin
  // First do the trivial
  ALeft := X - HorzHalfAxis;
  ARight := X + HorzHalfAxis;
  ATop := Y - VertHalfAxis;
  ABottom := Y + VertHalfAxis;
  {
    To calculate the bounding rectangle we can do this:

    Ellipse equations:You could try using the parametrized equations for an ellipse rotated at an arbitrary angle:

    x = CenterX + MajorHalfAxis*cos(t)*cos(Angle) - MinorHalfAxis*sin(t)*sin(Angle)
    y = CenterY + MinorHalfAxis*sin(t)*cos(Angle) + MajorHalfAxis*cos(t)*sin(Angle)

    You can then differentiate and solve for gradient = 0:
    0 = dx/dt = -MajorHalfAxis*sin(t)*cos(Angle) - MinorHalfAxis*cos(t)*sin(Angle)
    =>
    tan(t) = -MinorHalfAxis*tan(Angle)/MajorHalfAxis
    =>
    t = cotang(-MinorHalfAxis*tan(Angle)/MajorHalfAxis)

    On the other axis:

    0 = dy/dt = b*cos(t)*cos(phi) - a*sin(t)*sin(phi)
    =>
    tan(t) = b*cot(phi)/a
  }
  if Angle <> 0.0 then
  begin
    t := cotan(-VertHalfAxis*tan(Angle)/HorzHalfAxis);
    tmp := X + HorzHalfAxis*cos(t)*cos(Angle) - VertHalfAxis*sin(t)*sin(Angle);
    ARight := Round(tmp);
  end;
end;

procedure TvEllipse.Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double);

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  PointList: array[0..6] of TPoint;
  f: TPoint;
  dk, x1, x2, y1, y2: Integer;
  fx1, fy1, fx2, fy2: Double;
  {$ifdef USE_LCL_CANVAS}
  ALCLDest: TCanvas absolute ADest;
  {$endif}
begin
  inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);

  CalculateBoundingBox(ADest, fx1, fy1, fx2, fy2);
  x1 := CoordToCanvasX(fx1);
  x2 := CoordToCanvasX(fx2);
  y1 := CoordToCanvasY(fy1);
  y2 := CoordToCanvasY(fy2);

  {$ifdef USE_LCL_CANVAS}
  if Angle <> 0 then
  begin
    dk := Round(0.654 * Abs(y2-y1));
    f.x := Round(X);
    f.y := Round(Y - 1);
    PointList[0] := Rotate2DPoint(Point(x1, f.y), f, Angle) ;  // Startpoint
    PointList[1] := Rotate2DPoint(Point(x1,  f.y - dk), f, Angle);
    //Controlpoint of Startpoint first part
    PointList[2] := Rotate2DPoint(Point(x2- 1,  f.y - dk), f, Angle);
    //Controlpoint of secondpoint first part
    PointList[3] := Rotate2DPoint(Point(x2 -1 , f.y), f, Angle);
    // Firstpoint of secondpart
    PointList[4] := Rotate2DPoint(Point(x2-1 , f.y + dk), f, Angle);
    // Controllpoint of secondpart firstpoint
    PointList[5] := Rotate2DPoint(Point(x1, f.y +  dk), f, Angle);
    // Conrollpoint of secondpart endpoint
    PointList[6] := PointList[0];   // Endpoint of
     // Back to the startpoint
    ALCLDest.PolyBezier(Pointlist[0]);
  end
  else
  {$endif}
  begin
    ADest.Ellipse(x1, y1, x2, y2);
  end;
end;

{ TvRectangle }

procedure TvRectangle.CalculateBoundingBox(ADest: TFPCustomCanvas; var ALeft,
  ATop, ARight, ABottom: Double);
begin
  ALeft := X;
  ARight := X + CX;
  ATop := Y;
  ABottom := Y + CY;
end;

procedure TvRectangle.Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double);

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  x1, x2, y1, y2: Integer;
  fx1, fy1, fx2, fy2: Double;
begin
  inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);

  CalculateBoundingBox(ADest, fx1, fy1, fx2, fy2);
  x1 := CoordToCanvasX(fx1);
  x2 := CoordToCanvasX(fx2);
  y1 := CoordToCanvasY(fy1);
  y2 := CoordToCanvasY(fy2);

  {$ifdef USE_LCL_CANVAS}
  if (RX = 0) and (RY = 0) then
    ADest.Rectangle(x1, y1, x2, y2)
  else
    LCLIntf.RoundRect(TCanvas(ADest).Handle, x1, y1, x2, y2, Round(rx), Round(ry));
  {$else}
  ADest.Rectangle(x1, y1, x2, y2)
  {$endif}
end;

function TvRectangle.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr: string;
  lCurPathSeg: TPathSegment;
begin
  Result := inherited GenerateDebugTree(ADestRoutine, APageItem);
  // Add the font debug info in a sub-item
  lStr := Format('[TvRectangle] Text=%s CX=%f CY=%f CZ=%f RX=%f RY=%f',
    [Text,
    CX, CY, CZ,
    RX, RY
    ]);
  ADestRoutine(lStr, Result);
end;

{ TvPolygon }

procedure TvPolygon.Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo;
  ADestX: Integer; ADestY: Integer; AMulX: Double; AMulY: Double);

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  lPoints: array of TPoint;
  i: Integer;
begin
  inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);

  SetLength(lPoints, Length(Points));
  for i := 0 to Length(Points)-1 do
  begin
    lPoints[i].X := CoordToCanvasX(Points[i].X);
    lPoints[i].Y := CoordToCanvasY(Points[i].Y);
  end;

  ADest.Polygon(lPoints);
end;

{ TvAlignedDimension }

procedure TvAlignedDimension.Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double);

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  Points: array of TPoint;
  UpperDim, LowerDim: T3DPoint;
  {$ifdef USE_LCL_CANVAS}
  ALCLDest: TCanvas absolute ADest;
  {$endif}
begin
  ADest.Pen.FPColor := AdjustColorToBackground(colBlack, ARenderInfo);
  ADest.Pen.Width := 1;
  ADest.Pen.Style := psSolid;
  //
  // Draws this shape:
  // horizontal     vertical
  // ___
  // | |     or   ---| X cm
  //   |           --|
  // Which marks the dimension
  ADest.MoveTo(CoordToCanvasX(BaseRight.X), CoordToCanvasY(BaseRight.Y));
  ADest.LineTo(CoordToCanvasX(DimensionRight.X), CoordToCanvasY(DimensionRight.Y));
  ADest.LineTo(CoordToCanvasX(DimensionLeft.X), CoordToCanvasY(DimensionLeft.Y));
  ADest.LineTo(CoordToCanvasX(BaseLeft.X), CoordToCanvasY(BaseLeft.Y));
  // Now the arrows
  // horizontal
  SetLength(Points, 3);
  if DimensionRight.Y = DimensionLeft.Y then
  begin
    ADest.Brush.FPColor := AdjustColorToBackground(colBlack, ARenderInfo);
    ADest.Brush.Style := bsSolid;
    // Left arrow
    Points[0] := Point(CoordToCanvasX(DimensionLeft.X), CoordToCanvasY(DimensionLeft.Y));
    Points[1] := Point(Points[0].X + 7, Points[0].Y - 3);
    Points[2] := Point(Points[0].X + 7, Points[0].Y + 3);
    ADest.Polygon(Points);
    // Right arrow
    Points[0] := Point(CoordToCanvasX(DimensionRight.X), CoordToCanvasY(DimensionRight.Y));
    Points[1] := Point(Points[0].X - 7, Points[0].Y - 3);
    Points[2] := Point(Points[0].X - 7, Points[0].Y + 3);
    ADest.Polygon(Points);
    ADest.Brush.Style := bsClear;
    // Dimension text
    Points[0].X := CoordToCanvasX((DimensionLeft.X+DimensionRight.X)/2);
    Points[0].Y := CoordToCanvasY(DimensionLeft.Y);
    LowerDim.X := DimensionRight.X-DimensionLeft.X;
    ADest.Font.Size := 10;
    ADest.Font.Orientation := 0;
    ADest.Font.FPColor := AdjustColorToBackground(colBlack, ARenderInfo);
    ADest.TextOut(Points[0].X, Points[0].Y-Round(ADest.Font.Size*1.5), Format('%.1f', [LowerDim.X]));
  end
  else
  begin
    ADest.Brush.FPColor := AdjustColorToBackground(colBlack, ARenderInfo);
    ADest.Brush.Style := bsSolid;
    // There is no upper/lower preference for DimensionLeft/Right, so we need to check
    if DimensionLeft.Y > DimensionRight.Y then
    begin
      UpperDim := DimensionLeft;
      LowerDim := DimensionRight;
    end
    else
    begin
      UpperDim := DimensionRight;
      LowerDim := DimensionLeft;
    end;
    // Upper arrow
    Points[0] := Point(CoordToCanvasX(UpperDim.X), CoordToCanvasY(UpperDim.Y));
    Points[1] := Point(Points[0].X + Round(AMulX), Points[0].Y - Round(AMulY*3));
    Points[2] := Point(Points[0].X - Round(AMulX), Points[0].Y - Round(AMulY*3));
    ADest.Polygon(Points);
    // Lower arrow
    Points[0] := Point(CoordToCanvasX(LowerDim.X), CoordToCanvasY(LowerDim.Y));
    Points[1] := Point(Points[0].X + Round(AMulX), Points[0].Y + Round(AMulY*3));
    Points[2] := Point(Points[0].X - Round(AMulX), Points[0].Y + Round(AMulY*3));
    ADest.Polygon(Points);
    ADest.Brush.Style := bsClear;
    // Dimension text
    Points[0].X := CoordToCanvasX(DimensionLeft.X);
    Points[0].Y := CoordToCanvasY((DimensionLeft.Y+DimensionRight.Y)/2);
    LowerDim.Y := DimensionRight.Y-DimensionLeft.Y;
    if LowerDim.Y < 0 then LowerDim.Y := -1 * LowerDim.Y;
    ADest.Font.Size := 10;
    ADest.Font.Orientation := 900;
    ADest.Font.FPColor := AdjustColorToBackground(colBlack, ARenderInfo);
    ADest.TextOut(Points[0].X-Round(ADest.Font.Size*1.5), Points[0].Y, Format('%.1f', [LowerDim.Y]));
    ADest.Font.Orientation := 0;
  end;
  SetLength(Points, 0);

  {$IFDEF FPVECTORIAL_DEBUG_DIMENSIONS}
  WriteLn(Format('[TvAlignedDimension.Render] BaseRightXY=%f | %f DimensionRightXY=%f | %f DimensionLeftXY=%f | %f',
    [BaseRight.X, BaseRight.Y, DimensionRight.X, DimensionRight.Y, DimensionLeft.X, DimensionLeft.Y]));
  {$ENDIF}

{      // Debug info
  ADest.TextOut(CoordToCanvasX(CurDim.BaseRight.X), CoordToCanvasY(CurDim.BaseRight.Y), 'BR');
  ADest.TextOut(CoordToCanvasX(CurDim.DimensionRight.X), CoordToCanvasY(CurDim.DimensionRight.Y), 'DR');
  ADest.TextOut(CoordToCanvasX(CurDim.DimensionLeft.X), CoordToCanvasY(CurDim.DimensionLeft.Y), 'DL');
  ADest.TextOut(CoordToCanvasX(CurDim.BaseLeft.X), CoordToCanvasY(CurDim.BaseLeft.Y), 'BL');}
end;

function TvAlignedDimension.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr: string;
  lCurPathSeg: TPathSegment;
begin
  Result := inherited GenerateDebugTree(ADestRoutine, APageItem);
  // Add the font debug info in a sub-item
  lStr := Format('[TvAlignedDimension] BaseLeft=%f %f BaseRight=%f %f DimensionLeft=%f %f DimensionRight=%f %f',
    [BaseLeft.X, BaseLeft.Y,
     BaseRight.X, BaseRight.Y,
     DimensionLeft.X, DimensionLeft.Y,
     DimensionRight.X, DimensionRight.Y
    ]);
  ADestRoutine(lStr, Result);
end;

{ TvRadialDimension }

procedure TvRadialDimension.Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double);

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  Points: array of TPoint;
  lAngle, lRadius: Double;
  {$ifdef USE_LCL_CANVAS}
  ALCLDest: TCanvas absolute ADest;
  {$endif}
begin
  ADest.Pen.FPColor := AdjustColorToBackground(colBlack, ARenderInfo);
  ADest.Pen.Width := 1;
  ADest.Pen.Style := psSolid;

  // The size of the radius of the circle
  lRadius := sqrt(sqr(Center.X - DimensionLeft.X) + sqr(Center.Y - DimensionLeft.Y));
  // The angle to the first dimension
  lAngle := arctan((DimensionLeft.Y - Center.Y) / (DimensionLeft.X - Center.X));

  // Get an arrow in the right part of the circle
  SetLength(Points, 3);
  ADest.Brush.FPColor := AdjustColorToBackground(colBlack, ARenderInfo);
  ADest.Brush.Style := bsSolid;
  Points[0] := Point(CoordToCanvasX(Center.X + lRadius),     CoordToCanvasY(Center.Y));
  Points[1] := Point(CoordToCanvasX(Center.X + lRadius*0.8), CoordToCanvasY(Center.Y - lRadius*0.1));
  Points[2] := Point(CoordToCanvasX(Center.X + lRadius*0.8), CoordToCanvasY(Center.Y + lRadius*0.1));
  // Now rotate it to the actual position
  Points[0] := Rotate2DPoint(Points[0], Point(CoordToCanvasX(Center.X), CoordToCanvasY(Center.Y)),  lAngle);
  Points[1] := Rotate2DPoint(Points[1], Point(CoordToCanvasX(Center.X),  CoordToCanvasY(Center.Y)), lAngle);
  Points[2] := Rotate2DPoint(Points[2], Point(CoordToCanvasX(Center.X),  CoordToCanvasY(Center.Y)), lAngle);

  if not IsDiameter then
  begin
    // Basic line
    ADest.MoveTo(CoordToCanvasX(Center.X), CoordToCanvasY(Center.Y));
    ADest.LineTo(CoordToCanvasX(DimensionLeft.X), CoordToCanvasY(DimensionLeft.Y));

    // Draw the arrow
    ADest.Polygon(Points);
    ADest.Brush.Style := bsClear;

    // Dimension text
    Points[0].X := CoordToCanvasX(Center.X);
    Points[0].Y := CoordToCanvasY(Center.Y);
    ADest.Font.Size := 10;
    ADest.Font.FPColor := AdjustColorToBackground(colBlack, ARenderInfo);
    ADest.TextOut(Points[0].X, Points[0].Y, Format('%.1f', [lRadius]));
  end
  else
  begin
    // Basic line
    ADest.MoveTo(CoordToCanvasX(DimensionLeft.X), CoordToCanvasY(DimensionLeft.Y));
    ADest.LineTo(CoordToCanvasX(DimensionRight.X), CoordToCanvasY(DimensionRight.Y));

    // Draw the first arrow
    ADest.Polygon(Points);
    ADest.Brush.Style := bsClear;

    // And the second
    Points[0] := Point(CoordToCanvasX(Center.X + lRadius),     CoordToCanvasY(Center.Y));
    Points[1] := Point(CoordToCanvasX(Center.X + lRadius*0.8), CoordToCanvasY(Center.Y - lRadius*0.1));
    Points[2] := Point(CoordToCanvasX(Center.X + lRadius*0.8), CoordToCanvasY(Center.Y + lRadius*0.1));
    // Now rotate it to the actual position
    Points[0] := Rotate2DPoint(Points[0], Point(CoordToCanvasX(Center.X), CoordToCanvasY(Center.Y)),  lAngle + Pi);
    Points[1] := Rotate2DPoint(Points[1], Point(CoordToCanvasX(Center.X),  CoordToCanvasY(Center.Y)), lAngle + Pi);
    Points[2] := Rotate2DPoint(Points[2], Point(CoordToCanvasX(Center.X),  CoordToCanvasY(Center.Y)), lAngle + Pi);
    //
    ADest.Polygon(Points);
    ADest.Brush.Style := bsClear;

    // Dimension text
    Points[0].X := CoordToCanvasX(Center.X);
    Points[0].Y := CoordToCanvasY(Center.Y);
    ADest.Font.Size := 10;
    ADest.Font.FPColor := AdjustColorToBackground(colBlack, ARenderInfo);
    ADest.TextOut(Points[0].X, Points[0].Y, Format('%.1f', [lRadius * 2]));
  end;

  SetLength(Points, 0);
end;

function TvRadialDimension.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr, lIsDiameterStr: string;
  lCurPathSeg: TPathSegment;
begin
  Result := inherited GenerateDebugTree(ADestRoutine, APageItem);
  // Add the font debug info in a sub-item
  if IsDiameter then lIsDiameterStr := 'true' else lIsDiameterStr := 'false';
  lStr := Format('[TvAlignedDimension] IsDiameter=%s Center=%f %f DimensionLeft=%f %f DimensionRight=%f %f',
    [lIsDiameterStr,
     Center.X, Center.Y,
     DimensionLeft.X, DimensionLeft.Y,
     DimensionRight.X, DimensionRight.Y
    ]);
  ADestRoutine(lStr, Result);
end;

{ TvArcDimension }

procedure TvArcDimension.Render(ADest: TFPCustomCanvas;
  ARenderInfo: TvRenderInfo; ADestX: Integer; ADestY: Integer; AMulX: Double;
  AMulY: Double);

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  Points: array of TPoint;
  lAngleLeft, lAngleRight: Double;
  lTriangleCenter, lTriangleCorner: T3DPoint;
  {$ifdef USE_LCL_CANVAS}
  ALCLDest: TCanvas absolute ADest;
  {$endif}
begin
  ADest.Pen.FPColor := colYellow;//AdjustColorToBackground(colBlack, ARenderInfo);
  ADest.Pen.Width := 1;
  ADest.Pen.Style := psSolid;

  // Debug lines
  //ADest.Line(CoordToCanvasX(BaseLeft.X), CoordToCanvasY(BaseLeft.Y), CoordToCanvasX(DimensionLeft.X), CoordToCanvasY(DimensionLeft.Y));
  //ADest.Line(CoordToCanvasX(BaseRight.X), CoordToCanvasY(BaseRight.Y), CoordToCanvasX(DimensionRight.X), CoordToCanvasY(DimensionRight.Y));

  // Now the arc
  ALCLDest.Arc(
    CoordToCanvasX(BaseLeft.X - ArcRadius), CoordToCanvasY(BaseLeft.Y - ArcRadius),
    CoordToCanvasX(BaseLeft.X + ArcRadius), CoordToCanvasY(BaseLeft.Y + ArcRadius),
    CoordToCanvasX(DimensionRight.X), CoordToCanvasY(DimensionRight.Y),
    CoordToCanvasX(DimensionLeft.X),  CoordToCanvasY(DimensionLeft.Y));

  // Now the arrows
  SetLength(Points, 3);
  CalculateExtraArcInfo();
  ADest.Brush.FPColor := colYellow;//AdjustColorToBackground(colBlack, ARenderInfo);
  ADest.Brush.Style := bsSolid;

  // Left Arrow
  Points[0] := Point(CoordToCanvasX(ArcLeft.X), CoordToCanvasY(ArcLeft.Y));
  lTriangleCenter.X := Cos(AngleLeft+Pi/2) * -(ArcRadius/10) + ArcLeft.X;
  lTriangleCenter.Y := Sin(AngleLeft+Pi/2) * -(ArcRadius/10) + ArcLeft.Y;
  lTriangleCorner := Rotate3DPointInXY(lTriangleCenter, ArcLeft, Pi * 10 / 180);
  Points[1] := Point(CoordToCanvasX(lTriangleCorner.X), CoordToCanvasY(lTriangleCorner.Y));
  lTriangleCorner := Rotate3DPointInXY(lTriangleCenter, ArcLeft, - Pi * 10 / 180);
  Points[2] := Point(CoordToCanvasX(lTriangleCorner.X), CoordToCanvasY(lTriangleCorner.Y));
  ADest.Polygon(Points);

  // Right Arrow
  Points[0] := Point(CoordToCanvasX(ArcRight.X), CoordToCanvasY(ArcRight.Y));
  lTriangleCenter.X := Cos(AngleRight+Pi/2) * (ArcRadius/10) + ArcRight.X;
  lTriangleCenter.Y := Sin(AngleRight+Pi/2) * (ArcRadius/10) + ArcRight.Y;
  lTriangleCorner := Rotate3DPointInXY(lTriangleCenter, ArcRight, Pi * 10 / 180);
  Points[1] := Point(CoordToCanvasX(lTriangleCorner.X), CoordToCanvasY(lTriangleCorner.Y));
  lTriangleCorner := Rotate3DPointInXY(lTriangleCenter, ArcRight, - Pi * 10 / 180);
  Points[2] := Point(CoordToCanvasX(lTriangleCorner.X), CoordToCanvasY(lTriangleCorner.Y));
  ADest.Polygon(Points);
  ADest.Brush.Style := bsClear;

  // Dimension text
  Points[0].X := CoordToCanvasX(TextPos.X);
  Points[0].Y := CoordToCanvasY(TextPos.Y);
  ADest.Font.Size := 10;
  ADest.Font.Orientation := 0;
  ADest.Font.FPColor := colYellow;//AdjustColorToBackground(colBlack, ARenderInfo);
  ADest.TextOut(Points[0].X, Points[0].Y-Round(ADest.Font.Size*1.5), Format('%.1fº', [ArcValue]));
end;

procedure TvArcDimension.CalculateExtraArcInfo;
begin
  // Line equation of the Left line
  AngleLeft := arctan(Abs(BaseLeft.Y-DimensionLeft.Y)/Abs(BaseLeft.X-DimensionLeft.X));
  if DimensionLeft.X<BaseLeft.X then AngleLeft := Pi-AngleLeft;
  al := Tan(AngleLeft);
  bl := BaseLeft.Y - al * BaseLeft.X;

  // Line equation of the Right line
  AngleRight := arctan(Abs(BaseRight.Y-DimensionRight.Y)/Abs(BaseRight.X-DimensionRight.X));
  if DimensionRight.X<BaseRight.X then AngleRight := Pi-AngleRight;
  ar := Tan(AngleRight);
  br := BaseRight.Y - ar * BaseRight.X;

  // The lines meet at the AngleBase
  AngleBase.X := (bl - br) / (ar - al);
  AngleBase.Y := al * AngleBase.X + bl;

  //  And also now the left and right points of the arc
  ArcLeft.X := Cos(AngleLeft) * ArcRadius + AngleBase.X;
  ArcLeft.Y := Sin(AngleLeft) * ArcRadius + AngleBase.Y;
  ArcRight.X := Cos(AngleRight) * ArcRadius + AngleBase.X;
  ArcRight.Y := Sin(AngleRight) * ArcRadius + AngleBase.Y;
end;

function TvArcDimension.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr, lIsDiameterStr: string;
  lCurPathSeg: TPathSegment;
begin
  Result := inherited GenerateDebugTree(ADestRoutine, APageItem);
  // Add the font debug info in a sub-item
  lStr := Format('[TvArcDimension] ArcValue=%f ArcRadius=%f TextPos=%f %f BaseLeft=%f %f BaseRight=%f %f DimensionLeft=%f %f DimensionRight=%f %f',
    [ArcValue, ArcRadius,
     TextPos.X, TextPos.Y,
     BaseLeft.X, BaseLeft.Y,
     BaseRight.X, BaseRight.Y,
     DimensionLeft.X, DimensionLeft.Y,
     DimensionRight.X, DimensionRight.Y
    ]);
  ADestRoutine(lStr, Result);
end;

{ TvRasterImage }

procedure TvRasterImage.CreateRGB888Image(AWidth, AHeight: Cardinal);
{$ifdef USE_LCL_CANVAS}
var
  AImage: TLazIntfImage;
  lRawImage: TRawImage;
{$endif}
begin
{$ifdef USE_LCL_CANVAS}
  lRawImage.Init;
  lRawImage.Description.Init_BPP24_R8G8B8_BIO_TTB(AWidth, AHeight);
  lRawImage.CreateData(True);
  AImage := TLazIntfImage.Create(0,0);
  AImage.SetRawImage(lRawImage);

  RasterImage := AImage;
{$endif}
end;

procedure TvRasterImage.InitializeWithConvertionOf3DPointsToHeightMap(APage: TvVectorialPage; AWidth, AHeight: Integer);
var
  lEntity: TvEntity;
  i: Integer;
  lPos: TPoint;
  lValue: TFPColor;
  PreviousValue: Word;
  PreviousCount: Integer;
begin
  lValue := colBlack;

  // First setup the map and initialize it
  if RasterImage <> nil then RasterImage.Free;
  RasterImage := TFPMemoryImage.create(AWidth, AHeight);

  // Now go through all points and attempt to fit them to our grid
  for i := 0 to APage.GetEntitiesCount - 1 do
  begin
    lEntity := APage.GetEntity(i);
    if lEntity is TvPoint then
    begin
      lPos.X := Round((lEntity.X - APage.MinX) * AWidth / (APage.MaxX - APage.MinX));
      lPos.Y := Round((lEntity.Y - APage.MinY) * AHeight / (APage.MaxY - APage.MinY));

      if lPos.X >= AWidth then lPos.X := AWidth-1;
      if lPos.Y >= AHeight then lPos.Y := AHeight-1;
      if lPos.X < 0 then lPos.X := 0;
      if lPos.Y < 0 then lPos.Y := 0;

      // Calculate the height of this point
      PreviousValue := lValue.Red;
      lValue.Red := Round((lEntity.Z - APage.MinZ) * $FFFF / (APage.MaxZ - APage.MinZ));

      // And apply it as a fraction of the total number of points which fall in this square
      // we store the number of points in the Alpha channel
      PreviousCount := lValue.Alpha div $100;
      lValue.Red := Round((PreviousCount * PreviousValue + lValue.Red) / (PreviousCount + 1));

      lValue.Green := lValue.Red;
      lValue.Blue := lValue.Red;
      lValue.Alpha := lValue.Alpha + $100;
      //lValue.alpha:=;
      RasterImage.Colors[lPos.X, lPos.Y] := lValue;
    end;
  end;
end;

{ TvArrow }

procedure TvArrow.CalculateBoundingBox(ADest: TFPCustomCanvas; var ALeft, ATop,
  ARight, ABottom: Double);
begin

end;

procedure TvArrow.Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double);

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  lArrow, lBase, lExtraBase: TPoint;
  lPointD, lPointE, lPointF: T3DPoint;
  lPoints: array[0..2] of TPoint;
  AlfaAngle: Double;
begin
  ApplyPenToCanvas(ADest, ARenderInfo);
  ApplyBrushToCanvas(ADest);

  lArrow.X := CoordToCanvasX(X);
  lArrow.Y := CoordToCanvasY(Y);
  lBase.X := CoordToCanvasX(Base.X);
  lBase.Y := CoordToCanvasY(Base.Y);
  lExtraBase.X := CoordToCanvasX(ExtraLineBase.X);
  lExtraBase.Y := CoordToCanvasY(ExtraLineBase.Y);

  // Start with the lines

  ADest.Line(lArrow, lBase);

  if HasExtraLine then
    ADest.Line(lBase, lExtraBase);

  // Now draw the arrow head
  lPoints[0].X := CoordToCanvasX(X);
  lPoints[0].Y := CoordToCanvasY(Y);
  //
  // Here a lot of trigonometry comes to play, it is hard to explain in text, but in essence
  //
  // A line L is formed by the points A (Arrow head) and B (Base)
  // Our smaller triangle starts at a point D in this line which has length ArrowLength counting from A
  // This forms a rectangle triangle with a line paralel to the X axis
  // Alfa is the angle between A and the line parallel to the X axis
  //
  // This brings this equations:
  // AlfaAngle := arctg((B.Y - A.Y) / (B.X - A.X));
  // Sin(Alfa) := (D.Y - A.Y) / ArrowLength
  // Cos(Alfa) := (D.X - A.X) / ArrowLength
  //
  // Then at this point D we start a line perpendicular to the line L
  // And with this line we progress a length of ArrowBaseLength/2
  // This line, the D point and a line parallel to the Y axis for another
  // rectangle triangle with the same Alfa angle at the point D
  // The point at the end of the hipotenuse of this triangle is our point E
  // So we have more equations:
  //
  // Sin(Alfa) := (E.x - D.X) / (ArrowBaseLength/2)
  // Cos(Alfa) := (E.Y - D.Y) / (ArrowBaseLength/2)
  //
  // And the same in the opposite direction for our point F:
  //
  // Sin(Alfa) := (D.X - F.X) / (ArrowBaseLength/2)
  // Cos(Alfa) := (D.Y - F.Y) / (ArrowBaseLength/2)
  //
  if (Base.X - X) = 0 then
    AlfaAngle := 0
  else
    AlfaAngle := ArcTan((Base.Y - Y) / (Base.X - X));
  lPointD.Y := Sin(AlfaAngle) * ArrowLength + Y;
  lPointD.X := Cos(AlfaAngle) * ArrowLength + X;
  lPointE.X := Sin(AlfaAngle) * (ArrowBaseLength/2) + lPointD.X;
  lPointE.Y := Cos(AlfaAngle) * (ArrowBaseLength/2) + lPointD.Y;
  lPointF.X := - Sin(AlfaAngle) * (ArrowBaseLength/2) + lPointD.X;
  lPointF.Y := - Cos(AlfaAngle) * (ArrowBaseLength/2) + lPointD.Y;
  lPoints[1].X := CoordToCanvasX(lPointE.X);
  lPoints[1].Y := CoordToCanvasY(lPointE.Y);
  lPoints[2].X := CoordToCanvasX(lPointF.X);
  lPoints[2].Y := CoordToCanvasY(lPointF.Y);
  ADest.Polygon(lPoints);
end;

{ TvFormulaElement }

function TvFormulaElement.CalculateHeight(ADest: TFPCustomCanvas): Double;
var
  lLineHeight: Integer;
begin
  if ADest <> nil then
    lLineHeight := TCanvas(ADest).TextHeight(STR_FPVECTORIAL_TEXT_HEIGHT_SAMPLE) + 2
  else
    lLineHeight := 15;

  case Kind of
    //fekVariable,  // Text is the text of the variable
    //fekEqual,     // = symbol
    //fekSubtraction, // - symbol
    //fekMultiplication, // either a point . or a small x
    //fekSum,       // + symbol
    //fekPlusMinus, // The +/- symbol
    fekHorizontalLine: Result := 5;
    fekFraction:
    begin
      Formula.CalculateHeight(ADest);
      AdjacentFormula.CalculateHeight(ADest);
      Result := Formula.Height + AdjacentFormula.Height * 1.2;
    end;
    fekRoot: Result := Formula.CalculateHeight(ADest) * 1.2;
    fekPower: Result := lLineHeight * 1.2;
    fekSummation: Result := lLineHeight * 1.5;
    fekFormula: Result := Formula.CalculateHeight(ADest);
  else
    Result := lLineHeight;
  end;

  Height := Result;
end;

function TvFormulaElement.CalculateWidth(ADest: TFPCustomCanvas): Double;
var
  lText: String;
begin
  Result := 0;

  lText := AsText;
  if lText <> '' then
  begin
    if ADest = nil then Result := 10 * UTF8Length(lText)
    else Result := TCanvas(ADest).TextWidth(lText);
  end;

  case Kind of
    fekMultiplication: Result := 0;
    fekHorizontalLine: Result := 25;
    //
    fekFraction:
    begin
      Formula.CalculateWidth(ADest);
      AdjacentFormula.CalculateWidth(ADest);
      Result := Max(Formula.Width, AdjacentFormula.Width);
    end;
    fekRoot: Result := Formula.CalculateWidth(ADest) + 10;
    fekPower, fekSubscript:
    begin
      Result := Formula.CalculateWidth(ADest) +
        AdjacentFormula.CalculateWidth(ADest) / 2;
    end;
    fekSummation: Result := 8;
    fekFormula: Result := Formula.CalculateWidth(ADest);
  else
  end;

  Width := Result;
end;

function TvFormulaElement.AsText: string;
begin
  case Kind of
    fekVariable:  Result := Text;
    fekEqual:     Result := '=';
    fekSubtraction: Result := '-';
    fekMultiplication: Result := 'x';
    fekSum:       Result := '+';
    fekPlusMinus: Result := '+/-';
    fekLessThan:  Result := '<';
    fekLessOrEqualThan: Result := '<=';
    fekGreaterThan: Result := '>';
    fekGreaterOrEqualThan: Result := '>=';
    fekHorizontalLine: Result := '=';
  // More complex elements
  else
    Result := Format('[%s]', [GetEnumName(TypeInfo(TvFormulaElementKind), integer(Kind))]);
  end;
end;

procedure TvFormulaElement.PositionSubparts(ADest: TFPCustomCanvas; ABaseX,
  ABaseY: Double);
var
  lCentralizeFactor: Double = 0;
  lCentralizeFactorAdj: Double = 0;
begin
  case Self.Kind of
    fekFraction:
    begin
      // Check which fraction is the largest and centralize the other one
      Self.Formula.CalculateWidth(ADest);
      Self.AdjacentFormula.CalculateWidth(ADest);
      if Self.Formula.Width > Self.AdjacentFormula.Width then
      begin
        lCentralizeFactor := 0;
        lCentralizeFactorAdj := Self.Formula.Width / 2 - Self.AdjacentFormula.Width / 2;
      end
      else
      begin
        lCentralizeFactor := Self.AdjacentFormula.Width / 2 - Self.Formula.Width / 2;
        lCentralizeFactorAdj := 0;
      end;

      Self.Formula.PositionSubparts(ADest, Self.Left + lCentralizeFactor, Self.Top);
      Self.AdjacentFormula.PositionSubparts(ADest, Self.Left + lCentralizeFactorAdj, Self.Top - Self.Formula.Height - 3);
    end;
    fekRoot:
    begin
      // Give a factor for the root drawing
      Self.Formula.PositionSubparts(ADest, Self.Left + 10, Self.Top);
    end;
    fekPower:
    begin
      Self.Formula.PositionSubparts(ADest, Self.Left, Self.Top);
      Self.AdjacentFormula.PositionSubparts(ADest, Self.Left + Self.Formula.Width, Self.Top);
    end;
    fekSubscript:
    begin
      Self.Formula.PositionSubparts(ADest, Self.Left, Self.Top);
      Self.AdjacentFormula.PositionSubparts(ADest, Self.Left + Self.Formula.Width, Self.Top - Self.Formula.Height / 2);
    end;
    fekSummation:
    begin
      // main/bottom formula
      Self.Formula.PositionSubparts(ADest, Self.Left, Self.Top - 30);
      // top formula
      Self.AdjacentFormula.PositionSubparts(ADest, Self.Left, Self.Top);
    end;
    fekFormula:
    begin
      Self.Formula.PositionSubparts(ADest, Self.Left, Self.Top);
    end;
  end;
end;

procedure TvFormulaElement.Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double);

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  LeftC, TopC: Integer;
  lPt: array[0..3] of TPoint;
  lOldFontSize: Integer;
  lStr: string;
begin
  LeftC := CoordToCanvasX(Left);
  TopC := CoordToCanvasY(Top);

  case Kind of
    fekVariable: ADest.TextOut(LeftC, TopC, Text);
    fekEqual:    ADest.TextOut(LeftC, TopC, '=');
    fekSubtraction: ADest.TextOut(LeftC, TopC, '-');
    fekMultiplication:
    begin
      // Don't draw anything, leave an empty space, it looks better
      //ADest.TextOut(LeftC, TopC, 'x'); // × -> Unicode times symbol
    end;
    fekSum:      ADest.TextOut(LeftC, TopC, '+');
    fekPlusMinus:ADest.TextOut(LeftC, TopC, '±');
    fekLessThan: ADest.TextOut(LeftC, TopC, '<');
    fekLessOrEqualThan: ADest.TextOut(LeftC, TopC, '≤');
    fekGreaterThan: ADest.TextOut(LeftC, TopC, '>');
    fekGreaterOrEqualThan: ADest.TextOut(LeftC, TopC, '≥');
    fekHorizontalLine: ADest.Line(LeftC, TopC, CoordToCanvasX(Left+Width), TopC);
    // Complex ones
    fekFraction:
    begin
      Formula.Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
      AdjacentFormula.Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);

      // Division line
      lPt[0].X := CoordToCanvasX(Formula.Left);
      lPt[1].X := CoordToCanvasX(Formula.Left + Formula.Width);
      lPt[0].Y := CoordToCanvasY(Formula.Top - Formula.Height);
      lPt[1].Y := CoordToCanvasY(Formula.Top - Formula.Height);
      ADest.Line(lPt[0].X, lPt[0].Y, lPt[1].X, lPt[1].Y);
    end;
    fekRoot:
    begin
      Formula.Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);

      // Root drawing
      lPt[0].X := CoordToCanvasX(Left);
      lPt[0].Y := CoordToCanvasY(Top - Formula.Height * 0.7 + 5);
      // diagonal down
      lPt[1].X := CoordToCanvasX(Left + 5);
      lPt[1].Y := CoordToCanvasY(Top - Formula.Height * 0.7);
      // up
      lPt[2].X := CoordToCanvasX(Left + 5);
      lPt[2].Y := CoordToCanvasY(Top);
      // straight right
      lPt[3].X := CoordToCanvasX(Left + Formula.Width);
      lPt[3].Y := CoordToCanvasY(Top);
      //
      ADest.Polyline(lPt);
    end;
    fekPower:
    begin
      Formula.Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
      // The superscripted power
      lOldFontSize := ADest.Font.Size;
      if lOldFontSize = 0 then ADest.Font.Size := 5
      else ADest.Font.Size := lOldFontSize div 2;
      AdjacentFormula.Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
      ADest.Font.Size := lOldFontSize;
    end;
    fekSubscript:
    begin
      Formula.Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
      // The subscripted item
      lOldFontSize := ADest.Font.Size;
      if lOldFontSize = 0 then ADest.Font.Size := 5
      else ADest.Font.Size := lOldFontSize div 2;
      AdjacentFormula.Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
      ADest.Font.Size := lOldFontSize;
    end;
    fekSummation:
    begin
      // Draw the summation symbol
      lOldFontSize := ADest.Font.Size;
      ADest.Font.Size := 15;
      lStr := #$E2#$88#$91; // Unicode Character 'N-ARY SUMMATION' (U+2211)
      ADest.TextOut(LeftC, TopC, lStr);
      ADest.Font.Size := lOldFontSize;

      // Draw the bottom/main formula
      Formula.Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);

      // Draw the top formula
      AdjacentFormula.Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
    end;
    fekFormula:
    begin
      // Draw the formula
      Formula.Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
    end;
  end;
end;

procedure TvFormulaElement.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer);
var
  lDBGItem, lDBGFormula, lDBGFormulaBottom: Pointer;
  lStr: string;
begin
  lStr := Format('%s [%s]', [Self.AsText(), GetEnumName(TypeInfo(TvFormulaElementKind), integer(Kind))]);
  lStr := lStr + Format(' Left=%f Top=%f Width=%f Height=%f', [Left, Top, Width, Height]);
  lDBGItem := ADestRoutine(lStr, APageItem);

  case Kind of
    fekFraction, fekPower, fekSubscript, fekSummation:
    begin
      lDBGFormula := ADestRoutine('Main Formula', lDBGItem);
      Formula.GenerateDebugTree(ADestRoutine, lDBGFormula);
      if Kind in [fekPower, fekSummation] then
        lDBGFormulaBottom := ADestRoutine('Top Formula', lDBGItem)
      else
        lDBGFormulaBottom := ADestRoutine('Bottom Formula', lDBGItem);
      AdjacentFormula.GenerateDebugTree(ADestRoutine, lDBGFormulaBottom);
    end;
    fekRoot: Formula.GenerateDebugTree(ADestRoutine, lDBGItem);
    //fekSomatory: Result := 1.5;
    fekFormula: Formula.GenerateDebugTree(ADestRoutine, lDBGItem);
  end;
end;

{ TvFormula }

procedure TvFormula.CallbackDeleteElement(data, arg: pointer);
begin
  TvFormulaElement(data).Free;
end;

constructor TvFormula.Create;
begin
  inherited Create;
  FElements := TFPList.Create;
  SpacingBetweenElementsX := 5;
  SpacingBetweenElementsY := 1; // elements already give a fair amount of vertical spacing in their own area
end;

destructor TvFormula.Destroy;
begin
  FElements.Free;
  inherited Destroy;
end;

function TvFormula.GetFirstElement: TvFormulaElement;
begin
  if FElements.Count = 0 then Exit(nil);
  Result := FElements.Items[0];
  FCurIndex := 1;
end;

function TvFormula.GetNextElement: TvFormulaElement;
begin
  if FElements.Count <= FCurIndex then Exit(nil);
  Result := FElements.Items[FCurIndex];
  Inc(FCurIndex);
end;

procedure TvFormula.AddElement(AElement: TvFormulaElement);
begin
  FElements.Add(AElement);
end;

function TvFormula.AddElementWithKind(AKind: TvFormulaElementKind): TvFormulaElement;
begin
  Result := AddElementWithKindAndText(AKind, '');
end;

function TvFormula.AddElementWithKindAndText(AKind: TvFormulaElementKind;
  AText: string): TvFormulaElement;
begin
  Result := TvFormulaElement.Create;
  Result.Kind := AKind;
  Result.Text := AText;
  AddElement(Result);

  case AKind of
    fekFraction, fekPower, fekSubscript, fekSummation:
    begin
      Result.Formula := TvFormula.Create;
      Result.AdjacentFormula := TvFormula.Create;
    end;
    fekRoot:
    begin
      Result.Formula := TvFormula.Create;
    end;
  end;
end;

procedure TvFormula.Clear;
begin
  inherited Clear;
  FElements.ForEachCall(CallbackDeleteElement, nil);
  FElements.Clear;
end;

function TvFormula.CalculateHeight(ADest: TFPCustomCanvas): Double;
var
  lElement: TvFormulaElement;
begin
  if ADest <> nil then
    Result := TCanvas(ADest).TextHeight(STR_FPVECTORIAL_TEXT_HEIGHT_SAMPLE) + 2
  else
    Result := 15;

  lElement := GetFirstElement();
  while lElement <> nil do
  begin
    Result := Max(Result, lElement.CalculateHeight(ADest));
    lElement := GetNextElement;
  end;

  // Cache the result
  Height := Result;
end;

function TvFormula.CalculateWidth(ADest: TFPCustomCanvas): Double;
var
  lElement: TvFormulaElement;
begin
  Result := 0;
  lElement := GetFirstElement();
  while lElement <> nil do
  begin
    if lElement.Kind <> fekMultiplication then
      Result := Result + lElement.CalculateWidth(ADest) + SpacingBetweenElementsX;
    lElement := GetNextElement;
  end;
  // Remove an extra spacing, since it is added even to the last item
  Result := Result - SpacingBetweenElementsX;
  // Cache the result
  Width := Result;
end;

procedure TvFormula.PositionSubparts(ADest: TFPCustomCanvas; ABaseX, ABaseY: Double);
var
  lElement: TvFormulaElement;
  lPosX: Double = 0;
  lPosY: Double = 0;
  lMaxHeight: Double = 0;
begin
  CalculateHeight(ADest);
  CalculateWidth(ADest);
  Left := ABaseX;
  Top := ABaseY;

  // Then calculate the position of each element
  lElement := GetFirstElement();
  if lElement = nil then Exit;
  while lElement <> nil do
  begin
    lElement.Left := Left + lPosX;
    lPosX := lPosX + lElement.Width + SpacingBetweenElementsX;
    lElement.Top := Top;
    lMaxHeight := Max(lMaxHeight, lElement.Height);

    lElement.PositionSubparts(ADest, ABaseX, ABaseY);

    lElement := GetNextElement();
  end;

  // Go back and make a second loop to
  // check if there are any high elements in the same line,
  // and if yes, centralize the smaller ones
  lElement := GetFirstElement();
  if lElement = nil then Exit;
  while lElement <> nil do
  begin
    if lElement.Height < lMaxHeight then
    begin
      lElement.Top := Top - lMaxHeight / 2 + lElement.Height / 2;
    end;

    lElement := GetNextElement();
  end;
end;

procedure TvFormula.CalculateBoundingBox(ADest: TFPCustomCanvas; var ALeft, ATop, ARight,
  ABottom: Double);
begin
  ALeft := X;
  ATop := Y;
  ARight := CalculateWidth(ADest);
  if ADest = nil then ABottom := CalculateHeight(ADest) * 15
  else ABottom := CalculateHeight(ADest) * TCanvas(ADest).TextHeight('Źç');
  ARight := X + ARight;
  ABottom := Y + ABottom;
end;

procedure TvFormula.Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double);
var
  lElement: TvFormulaElement;
begin
  inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);

  // First position all elements
  PositionSubparts(ADest, Left, Top);

  // Now draw them all
  lElement := GetFirstElement();
  if lElement = nil then Exit;
  while lElement <> nil do
  begin
    lElement.Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);

    lElement := GetNextElement();
  end;
end;

function TvFormula.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lFormulaElement: TvFormulaElement;
  lStr: string;
begin
  lStr := Format('[%s]', [Self.ClassName]);
  lStr := lStr + Format(' Left=%f Top=%f Width=%f Height=%f', [Left, Top, Width, Height]);
  Result := ADestRoutine(lStr, APageItem);

  lFormulaElement := GetFirstElement();
  while lFormulaElement <> nil do
  begin
    lFormulaElement.GenerateDebugTree(ADestRoutine, Result);

    lFormulaElement := GetNextElement()
  end;
end;

{ TvEntityWithSubEntities }

procedure TvEntityWithSubEntities.CallbackDeleteElement(data, arg: pointer);
begin
  TvEntity(data).Free;
end;

constructor TvEntityWithSubEntities.Create;
begin
  inherited Create;
  FElements := TFPList.Create;
end;

destructor TvEntityWithSubEntities.Destroy;
begin
  FElements.Free;
  inherited Destroy;
end;

function TvEntityWithSubEntities.GetFirstEntity: TvEntity;
begin
  if FElements.Count = 0 then Exit(nil);
  Result := FElements.Items[0];
  FCurIndex := 1;
end;

function TvEntityWithSubEntities.GetNextEntity: TvEntity;
begin
  if FElements.Count <= FCurIndex then Exit(nil);
  Result := FElements.Items[FCurIndex];
  Inc(FCurIndex);
end;

procedure TvEntityWithSubEntities.AddEntity(AEntity: TvEntity);
begin
  //AEntity.Parent := Self;
  FElements.Add(AEntity);
end;

procedure TvEntityWithSubEntities.Rotate(AAngle: Double; ABase: T3DPoint);
var
  i: Integer;
begin
  for i := 0 to FElements.Count-1 do
  begin
    TvEntity(FElements.Items[i]).Rotate(AAngle, ABase);
  end;
end;

procedure TvEntityWithSubEntities.Clear;
begin
  inherited Clear;
  FElements.ForEachCall(CallbackDeleteElement, nil);
  FElements.Clear;
end;

procedure TvEntityWithSubEntities.Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo;
  ADestX: Integer; ADestY: Integer; AMulX: Double; AMulY: Double);
var
  lEntity: TvEntity;
begin
  inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
  lEntity := GetFirstEntity();
  while lEntity <> nil do
  begin
    {$IFDEF FPVECTORIAL_DEBUG_BLOCKS}
    //WriteLn(Format('[TvInsert.Render] Name=%s Block=%s Entity=%s EntityXY=%f | %f BlockXY=%f | %f InsertXY=%f | %f',
    //  [Name, Block.Name, lEntity.ClassName, lEntity.X, lEntity.Y, Block.X, Block.Y, X, Y]));
    {$ENDIF}

    // Render
    lEntity.Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMuly);

    lEntity := GetNextEntity();
  end;
end;

function TvEntityWithSubEntities.GenerateDebugTree(
  ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer;
var
  lStr: string;
  lCurEntity: TvEntity;
begin
  lStr := Format('[%s] Name="%s"' + FExtraDebugStr, [Self.ClassName, Self.Name]);

  // Add styles
  // Pen
  if spbfPenColor in SetPenBrushAndFontElements then
    lStr := lStr + Format(' Pen.Color=%s', [GenerateDebugStrForFPColor(Pen.Color)]);
  if spbfPenStyle in SetPenBrushAndFontElements then
    lStr := lStr + Format(' Pen.Style=%s', [GetEnumName(TypeInfo(TFPPenStyle), integer(Pen.Style))]);
  if spbfPenWidth in SetPenBrushAndFontElements then
    lStr := lStr + Format(' Pen.Width=%d', [Pen.Width]);
  // Brush
  if spbfBrushColor in SetPenBrushAndFontElements then
    lStr := lStr + Format(' Brush.Color=%s', [GenerateDebugStrForFPColor(Brush.Color)]);
  if spbfBrushStyle in SetPenBrushAndFontElements then
    lStr := lStr + Format(' Brush.Style=%s', [GetEnumName(TypeInfo(TFPBrushStyle), integer(Brush.Style))]);
  // Font
  if spbfFontColor in SetPenBrushAndFontElements then
    lStr := lStr + Format(' Font.Color=%s', [GenerateDebugStrForFPColor(Font.Color)]);
  if spbfFontSize in SetPenBrushAndFontElements then
    lStr := lStr + Format(' Font.Size=%d', [Font.Size]);

  Result := ADestRoutine(lStr, APageItem);

  // Add sub-entities
  lCurEntity := GetFirstEntity();
  while lCurEntity <> nil do
  begin
    lCurEntity.GenerateDebugTree(ADestRoutine, Result);
    lCurEntity := GetNextEntity();
  end;
end;

function TvEntityWithSubEntities.FindEntityWithNameAndType(AName: string;
  AType: TvEntityClass; ARecursively: Boolean): TvEntity;
var
  lCurEntity: TvEntity;
  lCurName: String;
begin
  Result := nil;
  lCurEntity := GetFirstEntity();
  while lCurEntity <> nil do
  begin
    if (lCurEntity is TvNamedEntity) then
      lCurName := TvNamedEntity(lCurEntity).Name
    else
      lCurName := '';

    if (lCurEntity is AType) and
      (lCurEntity is TvNamedEntity) and (lCurName = AName) then
    begin
      Result := lCurEntity;
      Exit;
    end;

    if ARecursively and (lCurEntity is TvEntityWithSubEntities) then
    begin
      Result := TvEntityWithSubEntities(lCurEntity).FindEntityWithNameAndType(AName, AType, True);
      if Result <> nil then Exit;
    end;

    lCurEntity := GetNextEntity();
  end;
end;

{ TvInsert }

procedure TvInsert.Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double);
var
  lEntity: TvEntity;
  OldForceRenderBlock: Boolean;
begin
  inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
  if InsertEntity = nil then Exit;
  // If we are inserting a block, make sure it will render its contents
  OldForceRenderBlock := ARenderInfo.ForceRenderBlock;
  ARenderInfo.ForceRenderBlock := True;
  // If necessary rotate the canvas
  if RotationAngle <> 0 then
  begin
    InsertEntity.Rotate(RotationAngle, Make3DPoint(0, 0, 0));
  end;
  // Alter the position of the elements to consider the positioning of the BLOCK and of the INSERT
  InsertEntity.Move(X, Y);
  // Render
  InsertEntity.Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMuly);
  // Change them back
  InsertEntity.Move(-X, -Y);
  // And unrotate it back again
  if RotationAngle <> 0 then
  begin
    InsertEntity.Rotate(-1 * RotationAngle, Make3DPoint(0, 0, 0));
  end;
  ARenderInfo.ForceRenderBlock := OldForceRenderBlock;
end;

function TvInsert.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
begin
  FExtraDebugStr := Format(' RotationAngle(degrees)=%f', [RotationAngle * 180 / Pi]);
  if (InsertEntity <> nil) and (InsertEntity is TvNamedEntity) then
    FExtraDebugStr := FExtraDebugStr + Format(' InsertEntity="%s"', [TvNamedEntity(InsertEntity).Name]);
  Result:=inherited GenerateDebugTree(ADestRoutine, APageItem);
end;

{ TvBlock }

procedure TvBlock.Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double);
var
  lEntity: TvEntity;
begin
  // blocks are invisible by themselves
  //inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
  if not ARenderInfo.ForceRenderBlock then Exit;

  lEntity := GetFirstEntity();
  while lEntity <> nil do
  begin
    {$IFDEF FPVECTORIAL_DEBUG_BLOCKS}
    WriteLn(Format('[TvInsert.Render] Name=%s Block=%s Entity=%s EntityXY=%f | %f BlockXY=%f | %f InsertXY=%f | %f',
      [Name, Block.Name, lEntity.ClassName, lEntity.X, lEntity.Y, Block.X, Block.Y, X, Y]));
    {$ENDIF}

    // Alter the position of the elements to consider the positioning of the BLOCK and of the INSERT
    lEntity.Move(X, Y);
    // Render
    lEntity.Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMuly);
    // Change them back
    lEntity.Move(-X, -Y);

    lEntity := GetNextEntity();
  end;
end;

{ TvRichText }

constructor TvRichText.Create;
begin
  inherited Create;
end;

destructor TvRichText.Destroy;
begin
  inherited Destroy;
end;

function TvRichText.AddText: TvText;
begin
  Result := TvText.Create;
  AddEntity(Result);
end;

function TvRichText.TryToSelect(APos: TPoint; var ASubpart: Cardinal
  ): TvFindEntityResult;
begin
  Result:=inherited TryToSelect(APos, ASubpart);
end;

procedure TvRichText.Render(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo;
  ADestX: Integer; ADestY: Integer; AMulX: Double; AMulY: Double);
begin
  inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
end;

function TvRichText.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
begin
  Result:=inherited GenerateDebugTree(ADestRoutine, APageItem);
end;

{ TvVectorialPage }

procedure TvVectorialPage.ClearTmpPath;
var
  segment, oldsegment: TPathSegment;
begin
  FTmpPath.Points := nil;
  FTmpPath.PointsEnd := nil;
  FTmpPath.Len := 0;
  FTmpPath.Brush.Color := colBlue;
  FTmpPath.Brush.Style := bsClear;
  FTmpPath.Pen.Color := colBlack;
  FTmpPath.Pen.Style := psSolid;
  FTmpPath.Pen.Width := 1;
end;

procedure TvVectorialPage.AppendSegmentToTmpPath(ASegment: TPathSegment);
begin
  FTmpPath.AppendSegment(ASegment);
end;

procedure TvVectorialPage.CallbackDeleteEntity(data, arg: pointer);
begin
  if (data <> nil) then
    TvEntity(data).Free;
end;

constructor TvVectorialPage.Create(AOwner: TvVectorialDocument);
begin
  inherited Create;

  FEntities := TFPList.Create;
  FTmpPath := TPath.Create;
  Owner := AOwner;
  Clear();
  BackgroundColor := colWhite;
  System.FillChar(RenderInfo, SizeOf(RenderInfo), #0);
  RenderInfo.BackgroundColor := colWhite;
end;

destructor TvVectorialPage.Destroy;
begin
  Clear;

  if FTmpPath <> nil then
  begin
    FTmpPath.Free;
    FTmpPath := nil;
  end;

  FEntities.Free;
  FEntities := nil;

  inherited Destroy;
end;

procedure TvVectorialPage.Assign(ASource: TvVectorialPage);
var
  i: Integer;
begin
  Clear;

  for i := 0 to ASource.GetEntitiesCount - 1 do
    Self.AddEntity(ASource.GetEntity(i));
end;

function TvVectorialPage.GetEntity(ANum: Cardinal): TvEntity;
begin
  if ANum >= FEntities.Count then raise Exception.Create('TvVectorialDocument.GetEntity: Entity number out of bounds');

  Result := TvEntity(FEntities.Items[ANum]);

  if Result = nil then raise Exception.Create(Format('TvVectorialDocument.GetEntity: Invalid Entity number ANum=%d', [ANum]));
end;

function TvVectorialPage.GetEntitiesCount: Integer;
begin
  Result := FEntities.Count;
end;

function TvVectorialPage.GetLastEntity(): TvEntity;
begin
  Result:=TvEntity(FEntities.Last);
end;

function TvVectorialPage.FindAndSelectEntity(Pos: TPoint): TvFindEntityResult;
var
  lEntity: TvEntity;
  i: Integer;
  lSubpart: Cardinal;
begin
  Result := vfrNotFound;

  for i := 0 to GetEntitiesCount() - 1 do
  begin
    lEntity := GetEntity(i);

    Result := lEntity.TryToSelect(Pos, lSubpart);

    if Result <> vfrNotFound then
    begin
      Owner.SelectedElement := lEntity;
      Exit;
    end;
  end;
end;

function TvVectorialPage.FindEntityWithNameAndType(AName: string;
  AType: TvEntityClass; ARecursively: Boolean): TvEntity;
var
  i: Integer;
  lCurEntity: TvEntity;
  lCurName: String;
begin
  Result := nil;
  for i := 0 to GetEntitiesCount()-1 do
  begin
    lCurEntity := GetEntity(i);

    if (lCurEntity is TvNamedEntity) then
      lCurName := TvNamedEntity(lCurEntity).Name
    else
      lCurName := '';

    if (lCurEntity is AType) and
      (lCurEntity is TvNamedEntity) and (lCurName = AName) then
    begin
      Result := lCurEntity;
      Exit;
    end;

    if ARecursively and (lCurEntity is TvEntityWithSubEntities) then
    begin
      Result := TvEntityWithSubEntities(lCurEntity).FindEntityWithNameAndType(AName, AType, True);
      if Result <> nil then Exit;
    end;
  end;
end;

procedure TvVectorialPage.Clear;
begin
  FEntities.ForEachCall(CallbackDeleteEntity, nil);
  FEntities.Clear();
  ClearTmpPath();
  ClearLayerSelection();
end;

{@@
  Returns if the entity was really deleted or false if there is no entity with this index
}
function TvVectorialPage.DeleteEntity(AIndex: Cardinal): Boolean;
var
  lEntity: TvEntity;
begin
  Result := False;
  if AIndex >= GetEntitiesCount() then Exit;;
  lEntity := GetEntity(AIndex);
  if lEntity = nil then Exit;
  FEntities.Delete(AIndex);
  lEntity.Free;
  Result := True;
end;

function TvVectorialPage.RemoveEntity(AEntity: TvEntity; AFreeAfterRemove: Boolean = True): Boolean;
begin
  Result := False;
  if AEntity = nil then Exit;
  FEntities.Remove(AEntity);
  if AFreeAfterRemove then AEntity.Free;
  Result := True;
end;

{@@
  Adds an entity to the document and returns it's current index
}
function TvVectorialPage.AddEntity(AEntity: TvEntity): Integer;
begin
  if FCurrentLayer = nil then
  begin
    Result := FEntities.Count;
    //AEntity.Parent := nil;
    FEntities.Add(Pointer(AEntity));
  end
  // If a layer is selected as current, add elements to it instead
  else
  begin
    Result := FCurrentLayer.GetSubpartCount();
    //AEntity.Parent := FCurrentLayer;
    FCurrentLayer.AddEntity(AEntity);
  end;
end;

function TvVectorialPage.AddPathCopyMem(APath: TPath; AOnlyCreate: Boolean = False): TPath;
var
  lPath: TPath;
  Len: Integer;
begin
  lPath := TPath.Create;
  lPath.Assign(APath);
  Result := lPath;
  if not AOnlyCreate then AddEntity(lPath);
  //WriteLn(':>TvVectorialDocument.AddPath 1 Len = ', Len);
end;

{@@
  Starts writing a Path in multiple steps.
  Should be followed by zero or more calls to AddPointToPath
  and by a call to EndPath to effectively add the data.

  @see    EndPath, AddPointToPath
}
procedure TvVectorialPage.StartPath(AX, AY: Double);
var
  segment: T2DSegment;
begin
  ClearTmpPath();

  FTmpPath.Len := 1;
  segment := T2DSegment.Create;
  segment.SegmentType := stMoveTo;
  segment.X := AX;
  segment.Y := AY;

  FTmpPath.Points := segment;
  FTmpPath.PointsEnd := segment;
end;

procedure TvVectorialPage.StartPath;
begin
  ClearTmpPath();
end;

procedure TvVectorialPage.AddMoveToPath(AX, AY: Double);
var
  segment: T2DSegment;
begin
  segment := T2DSegment.Create;
  segment.SegmentType := stMoveTo;
  segment.X := AX;
  segment.Y := AY;

  AppendSegmentToTmpPath(segment);
end;

{@@
  Adds one more point to the end of a Path being
  writing in multiple steps.

  Does nothing if not called between StartPath and EndPath.

  Can be called multiple times to add multiple points.

  @see    StartPath, EndPath
}
procedure TvVectorialPage.AddLineToPath(AX, AY: Double);
var
  segment: T2DSegment;
begin
  segment := T2DSegment.Create;
  segment.SegmentType := st2DLine;
  segment.X := AX;
  segment.Y := AY;

  AppendSegmentToTmpPath(segment);
end;

procedure TvVectorialPage.AddLineToPath(AX, AY: Double; AColor: TFPColor);
var
  segment: T2DSegmentWithPen;
begin
  segment := T2DSegmentWithPen.Create;
  segment.SegmentType := st2DLineWithPen;
  segment.X := AX;
  segment.Y := AY;
  segment.Pen.Color := AColor;

  AppendSegmentToTmpPath(segment);
end;

procedure TvVectorialPage.AddLineToPath(AX, AY, AZ: Double);
var
  segment: T3DSegment;
begin
  segment := T3DSegment.Create;
  segment.SegmentType := st3DLine;
  segment.X := AX;
  segment.Y := AY;
  segment.Z := AZ;

  AppendSegmentToTmpPath(segment);
end;

{@@
  Gets the current Pen Pos in the temporary path
}
procedure TvVectorialPage.GetCurrentPathPenPos(var AX, AY: Double);
begin
  // Check if we are the first segment in the tmp path
  if FTmpPath.PointsEnd = nil then raise Exception.Create('[TvVectorialDocument.GetCurrentPathPenPos] One cannot obtain the Pen Pos if there are no segments in the temporary path');

  AX := T2DSegment(FTmpPath.PointsEnd).X;
  AY := T2DSegment(FTmpPath.PointsEnd).Y;
end;

procedure TvVectorialPage.GetTmpPathStartPos(var AX, AY: Double);
begin
  AX := 0;
  AY := 0;
  if (FTmpPath = nil) or (FTmpPath.GetSubpartCount() <= 0) or (FTmpPath.Points = nil) then Exit;
  if FTmpPath.Points is T2DSegment then
  begin
    AX := T2DSegment(FTmpPath.Points).X;
    AY := T2DSegment(FTmpPath.Points).Y;
  end;
end;

{@@
  Adds a bezier element to the path. It starts where the previous element ended
  and it goes throw the control points [AX1, AY1] and [AX2, AY2] and ends
  in [AX3, AY3].
}
procedure TvVectorialPage.AddBezierToPath(AX1, AY1, AX2, AY2, AX3, AY3: Double);
var
  segment: T2DBezierSegment;
begin
  segment := T2DBezierSegment.Create;
  segment.SegmentType := st2DBezier;
  segment.X := AX3;
  segment.Y := AY3;
  segment.X2 := AX1;
  segment.Y2 := AY1;
  segment.X3 := AX2;
  segment.Y3 := AY2;

  AppendSegmentToTmpPath(segment);
end;

procedure TvVectorialPage.AddBezierToPath(AX1, AY1, AZ1, AX2, AY2, AZ2, AX3, AY3, AZ3: Double);
var
  segment: T3DBezierSegment;
begin
  segment := T3DBezierSegment.Create;
  segment.SegmentType := st3DBezier;
  segment.X := AX3;
  segment.Y := AY3;
  segment.Z := AZ3;
  segment.X2 := AX1;
  segment.Y2 := AY1;
  segment.Z2 := AZ1;
  segment.X3 := AX2;
  segment.Y3 := AY2;
  segment.Z3 := AZ2;

  AppendSegmentToTmpPath(segment);
end;

procedure TvVectorialPage.AddEllipticalArcToPath(ARadX, ARadY, AXAxisRotation,
  ADestX, ADestY: Double; ALeftmostEllipse, AClockwiseArcFlag: Boolean);
var
  segment: T2DEllipticalArcSegment;
begin
  segment := T2DEllipticalArcSegment.Create;
  segment.SegmentType := st2DEllipticalArc;
  segment.X := ADestX;
  segment.Y := ADestY;
  segment.RX := ARadX;
  segment.RY := ARadY;
  segment.XRotation := AXAxisRotation;
  segment.LeftmostEllipse := ALeftmostEllipse;
  segment.ClockwiseArcFlag := AClockwiseArcFlag;

  AppendSegmentToTmpPath(segment);
end;

procedure TvVectorialPage.SetBrushColor(AColor: TFPColor);
begin
  FTmPPath.Brush.Color := AColor;
end;

procedure TvVectorialPage.SetBrushStyle(AStyle: TFPBrushStyle);
begin
  FTmPPath.Brush.Style := AStyle;
end;

procedure TvVectorialPage.SetPenColor(AColor: TFPColor);
begin
  FTmPPath.Pen.Color := AColor;
end;

procedure TvVectorialPage.SetPenStyle(AStyle: TFPPenStyle);
begin
  FTmPPath.Pen.Style := AStyle;
end;

procedure TvVectorialPage.SetPenWidth(AWidth: Integer);
begin
  FTmPPath.Pen.Width := AWidth;
end;

procedure TvVectorialPage.SetClipPath(AClipPath: TPath; AClipMode: TvClipMode);
begin
  FTmPPath.ClipPath := AClipPath;
  FTmPPath.ClipMode := AClipMode;
end;

{@@
  Finishes writing a Path, which was created in multiple
  steps using StartPath and AddPointToPath,
  to the document.

  Does nothing if there wasn't a previous correspondent call to
  StartPath.

  @see    StartPath, AddPointToPath
}
function  TvVectorialPage.EndPath(AOnlyCreate: Boolean = False): TPath;
begin
  if FTmPPath.Len = 0 then Exit;
  Result := AddPathCopyMem(FTmPPath, AOnlyCreate);
  ClearTmpPath();
end;

function TvVectorialPage.AddText(AX, AY, AZ: Double; FontName: string;
  FontSize: integer; AText: utf8string; AOnlyCreate: Boolean = False): TvText;
var
  lText: TvText;
begin
  lText := TvText.Create;
  lText.Value.Text := AText;
  lText.X := AX;
  lText.Y := AY;
  lText.Z := AZ;
  lText.Font.Name := FontName;
  lText.Font.Size := FontSize;
  if not AOnlyCreate then AddEntity(lText);
  Result := lText;
end;

function TvVectorialPage.AddText(AX, AY: Double; AStr: utf8string; AOnlyCreate: Boolean = False): TvText;
begin
  Result := AddText(AX, AY, 0, '', 10, AStr, AOnlyCreate);
end;

function TvVectorialPage.AddText(AX, AY, AZ: Double; AStr: utf8string; AOnlyCreate: Boolean = False): TvText;
begin
  Result := AddText(AX, AY, AZ, '', 10, AStr, AOnlyCreate);
end;

function TvVectorialPage.AddCircle(ACenterX, ACenterY, ARadius: Double; AOnlyCreate: Boolean = False): TvCircle;
var
  lCircle: TvCircle;
begin
  lCircle := TvCircle.Create;
  lCircle.X := ACenterX;
  lCircle.Y := ACenterY;
  lCircle.Radius := ARadius;
  Result := lCircle;
  if not AOnlyCreate then AddEntity(lCircle);
end;

function TvVectorialPage.AddCircularArc(ACenterX, ACenterY, ARadius,
  AStartAngle, AEndAngle: Double; AColor: TFPColor; AOnlyCreate: Boolean = False): TvCircularArc;
var
  lCircularArc: TvCircularArc;
begin
  lCircularArc := TvCircularArc.Create;
  lCircularArc.X := ACenterX;
  lCircularArc.Y := ACenterY;
  lCircularArc.Radius := ARadius;
  lCircularArc.StartAngle := AStartAngle;
  lCircularArc.EndAngle := AEndAngle;
  lCircularArc.Pen.Color := AColor;
  Result := lCircularArc;
  if not AOnlyCreate then AddEntity(lCircularArc);
end;

function TvVectorialPage.AddEllipse(CenterX, CenterY, HorzHalfAxis,
  VertHalfAxis, Angle: Double; AOnlyCreate: Boolean = False): TvEllipse;
var
  lEllipse: TvEllipse;
begin
  lEllipse := TvEllipse.Create;
  lEllipse.X := CenterX;
  lEllipse.Y := CenterY;
  lEllipse.HorzHalfAxis := HorzHalfAxis;
  lEllipse.VertHalfAxis := VertHalfAxis;
  lEllipse.Angle := Angle;
  Result := lEllipse;
  if not AOnlyCreate then AddEntity(lEllipse);
end;

function TvVectorialPage.AddBlock(AName: string; AX, AY, AZ: Double): TvBlock;
var
  lBlock: TvBlock;
begin
  lBlock := TvBlock.Create;
  lBlock.X := AX;
  lBlock.Y := AY;
  lBlock.Name := AName;
  AddEntity(lBlock);
  Result := lBlock;
end;

function TvVectorialPage.AddInsert(AX, AY, AZ: Double; AInsertEntity: TvEntity): TvInsert;
var
  lInsert: TvInsert;
begin
  lInsert := TvInsert.Create;
  lInsert.X := AX;
  lInsert.Y := AY;
  lInsert.InsertEntity := AInsertEntity;
  AddEntity(lInsert);
  Result := lInsert;
end;

function TvVectorialPage.AddLayer(AName: string): TvLayer;
begin
  Result := TvLayer.Create;
  Result.Name := AName;
  AddEntity(Result);
end;

function TvVectorialPage.AddLayerAndSetAsCurrent(AName: string): TvLayer;
begin
  Result := AddLayer(AName);
  FCurrentLayer := Result;
end;

procedure TvVectorialPage.ClearLayerSelection;
begin
  FCurrentLayer := nil;
end;

function TvVectorialPage.SetCurrentLayer(ALayer: TvEntityWithSubEntities): Boolean;
begin
  Result := True;
  FCurrentLayer := ALayer;
end;

function TvVectorialPage.GetCurrentLayer: TvEntityWithSubEntities;
begin
  Result := FCurrentLayer;
end;


function TvVectorialPage.AddAlignedDimension(BaseLeft, BaseRight, DimLeft,
  DimRight: T3DPoint; AOnlyCreate: Boolean = False): TvAlignedDimension;
var
  lDim: TvAlignedDimension;
begin
  lDim := TvAlignedDimension.Create;
  lDim.BaseLeft := BaseLeft;
  lDim.BaseRight := BaseRight;
  lDim.DimensionLeft := DimLeft;
  lDim.DimensionRight := DimRight;
  Result := lDim;
  if not AOnlyCreate then AddEntity(lDim);
end;

function TvVectorialPage.AddRadialDimension(AIsDiameter: Boolean; ACenter,
  ADimLeft, ADimRight: T3DPoint; AOnlyCreate: Boolean = False): TvRadialDimension;
var
  lDim: TvRadialDimension;
begin
  lDim := TvRadialDimension.Create;
  lDim.IsDiameter := AIsDiameter;
  lDim.Center := ACenter;
  lDim.DimensionLeft := ADimLeft;
  lDim.DimensionRight := ADimRight;
  Result := lDim;
  if not AOnlyCreate then AddEntity(lDim);
end;

function TvVectorialPage.AddArcDimension(AArcValue, AArcRadius: Double; ABaseLeft, ABaseRight, ADimLeft, ADimRight, ATextPos: T3DPoint; AOnlyCreate: Boolean): TvArcDimension;
var
  lDim: TvArcDimension;
begin
  lDim := TvArcDimension.Create;
  lDim.BaseLeft := ABaseLeft;
  lDim.BaseRight := ABaseRight;
  lDim.DimensionLeft := ADimLeft;
  lDim.DimensionRight := ADimRight;
  lDim.ArcRadius := AArcRadius;
  lDim.ArcValue := AArcValue;
  lDim.TextPos := ATextPos;
  Result := lDim;
  if not AOnlyCreate then AddEntity(lDim);
end;

function TvVectorialPage.AddPoint(AX, AY, AZ: Double): TvPoint;
var
  lPoint: TvPoint;
begin
  lPoint := TvPoint.Create;
  lPoint.X := AX;
  lPoint.Y := AY;
  lPoint.Z := AZ;
  AddEntity(lPoint);
  Result := lPoint;
end;

procedure TvVectorialPage.PositionEntitySubparts(ADest: TFPCustomCanvas; ABaseX,
  ABaseY: Double);
var
  i: Integer;
begin
  for i := 0 to GetEntitiesCount()-1 do
    GetEntity(0).PositionSubparts(ADest, ABaseX, ABaseY);
end;

procedure TvVectorialPage.DrawBackground(ADest: TFPCustomCanvas);
begin
  ADest.Pen.Style := psClear;
  ADest.Brush.Style := bsSolid;
  ADest.Brush.FPColor := BackgroundColor;
  ADest.FillRect(0, 0, ADest.Width, ADest.Height);
  ADest.Pen.Style := psSolid;
end;

{@@
  This function draws a FPVectorial vectorial page to a TFPCustomCanvas
  descendent, such as TCanvas from the LCL.

  Be careful that by default this routine does not execute coordinate transformations,
  and that FPVectorial works with a start point in the bottom-left corner, with
  the X growing to the right and the Y growing to the top. This will result in
  an image in TFPCustomCanvas mirrored in the Y axis in relation with the document
  as seen in a PDF viewer, for example. This can be easily changed with the
  provided parameters. To have the standard view of an image viewer one could
  use this function like this:

  ASource.Render(ADest, 0, ASource.Height, 1.0, -1.0);
}
procedure TvVectorialPage.Render(ADest: TFPCustomCanvas;
  ADestX: Integer; ADestY: Integer; AMulX: Double; AMulY: Double);
var
  i: Integer;
  CurEntity: TvEntity;
begin
  {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
  WriteLn(':>DrawFPVectorialToCanvas');
  {$endif}

  for i := 0 to GetEntitiesCount - 1 do
  begin
    {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
    Write(Format('[Path] ID=%d', [i]));
    {$endif}

    CurEntity := GetEntity(i);

    RenderInfo.BackgroundColor := BackgroundColor;
    CurEntity.Render(ADest, RenderInfo, ADestX, ADestY, AMulX, AMulY);
  end;

  {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
  WriteLn(':<DrawFPVectorialToCanvas');
  {$endif}
end;

procedure TvVectorialPage.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer);
var
  lCurEntity: TvEntity;
  i: Integer;
begin
  for i := 0 to FEntities.Count - 1 do
  begin
    lCurEntity := TvEntity(FEntities.Items[i]);
    lCurEntity.GenerateDebugTree(ADestRoutine, APageItem);
  end;
end;

{ TvTextPageSequence }

constructor TvTextPageSequence.Create(AOwner: TvVectorialDocument);
begin
  inherited Create(AOwner);
end;

destructor TvTextPageSequence.Destroy;
begin
  inherited Destroy;
end;

procedure TvTextPageSequence.Assign(ASource: TvVectorialPage);
begin
  inherited Assign(ASource);
end;

function TvTextPageSequence.AddParagraph: TvRichText;
begin
  Result := TvRichText.Create;
  AddEntity(Result);
end;

{ TvVectorialDocument }

{@@
  Constructor.
}
constructor TvVectorialDocument.Create;
begin
  inherited Create;

  FPages := TFPList.Create;
  FCurrentPageIndex := -1;
  FStyles := TFPList.Create;
end;

{@@
  Destructor.
}
destructor TvVectorialDocument.Destroy;
begin
  Clear;

  FPages.Free;
  FPages := nil;
  FStyles.Free;
  FStyles := nil;

  inherited Destroy;
end;

procedure TvVectorialDocument.Assign(ASource: TvVectorialDocument);
//var
//  i: Integer;
begin
//  Clear;
//
//  for i := 0 to ASource.GetEntitiesCount - 1 do
//    Self.AddEntity(ASource.GetEntity(i));
end;

procedure TvVectorialDocument.AssignTo(ADest: TvVectorialDocument);
begin
  ADest.Assign(Self);
end;

{@@
  Convenience method which creates the correct
  writer object for a given vector graphics document format.
}
function TvVectorialDocument.CreateVectorialWriter(AFormat: TvVectorialFormat): TvCustomVectorialWriter;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Length(GvVectorialFormats) - 1 do
    if GvVectorialFormats[i].Format = AFormat then
    begin
      if GvVectorialFormats[i].WriterClass <> nil then
        Result := GvVectorialFormats[i].WriterClass.Create;

      Break;
    end;

  if Result = nil then raise Exception.Create('Unsupported vector graphics format.');
end;

{@@
  Convenience method which creates the correct
  reader object for a given vector graphics document format.
}
function TvVectorialDocument.CreateVectorialReader(AFormat: TvVectorialFormat): TvCustomVectorialReader;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Length(GvVectorialFormats) - 1 do
    if GvVectorialFormats[i].Format = AFormat then
    begin
      if GvVectorialFormats[i].ReaderClass <> nil then
        Result := GvVectorialFormats[i].ReaderClass.Create;

      Break;
    end;

  if Result = nil then raise Exception.Create('Unsupported vector graphics format.');
end;

{@@
  Writes the document to a file.

  If the file doesn't exist, it will be created.
}
procedure TvVectorialDocument.WriteToFile(AFileName: string; AFormat: TvVectorialFormat);
var
  AWriter: TvCustomVectorialWriter;
begin
  AWriter := CreateVectorialWriter(AFormat);

  try
    AWriter.WriteToFile(AFileName, Self);
  finally
    AWriter.Free;
  end;
end;

procedure TvVectorialDocument.WriteToFile(AFileName: string);
var
  lFormat: TvVectorialFormat;
begin
  lFormat := GetFormatFromExtension(ExtractFileExt(AFileName));
  WriteToFile(AFileName, lFormat);
end;

{@@
  Writes the document to a stream
}
procedure TvVectorialDocument.WriteToStream(AStream: TStream; AFormat: TvVectorialFormat);
var
  AWriter: TvCustomVectorialWriter;
begin
  AWriter := CreateVectorialWriter(AFormat);

  try
    AWriter.WriteToStream(AStream, Self);
  finally
    AWriter.Free;
  end;
end;

procedure TvVectorialDocument.WriteToStrings(AStrings: TStrings;
  AFormat: TvVectorialFormat);
var
  AWriter: TvCustomVectorialWriter;
begin
  AWriter := CreateVectorialWriter(AFormat);

  try
    AWriter.WriteToStrings(AStrings, Self);
  finally
    AWriter.Free;
  end;
end;

{@@
  Reads the document from a file.

  Any current contents in this object will be removed.
}
procedure TvVectorialDocument.ReadFromFile(AFileName: string;
  AFormat: TvVectorialFormat);
var
  AReader: TvCustomVectorialReader;
begin
  Self.Clear;

  AReader := CreateVectorialReader(AFormat);
  try
    AReader.ReadFromFile(AFileName, Self);
  finally
    AReader.Free;
  end;
end;

{@@
  Reads the document from a file.  A variant that auto-detects the format from the extension and other factors.
}
procedure TvVectorialDocument.ReadFromFile(AFileName: string);
var
  lFormat: TvVectorialFormat;
begin
  lFormat := GetFormatFromExtension(ExtractFileExt(AFileName));
  ReadFromFile(AFileName, lFormat);
end;

{@@
  Reads the document from a stream.

  Any current contents in this object will be removed.
}
procedure TvVectorialDocument.ReadFromStream(AStream: TStream;
  AFormat: TvVectorialFormat);
var
  AReader: TvCustomVectorialReader;
begin
  Self.Clear;

  AReader := CreateVectorialReader(AFormat);
  try
    AReader.ReadFromStream(AStream, Self);
  finally
    AReader.Free;
  end;
end;

procedure TvVectorialDocument.ReadFromStrings(AStrings: TStrings;
  AFormat: TvVectorialFormat);
var
  AReader: TvCustomVectorialReader;
begin
  Self.Clear;

  AReader := CreateVectorialReader(AFormat);
  try
    AReader.ReadFromStrings(AStrings, Self);
  finally
    AReader.Free;
  end;
end;

class function TvVectorialDocument.GetFormatFromExtension(AFileName: string
  ): TvVectorialFormat;
var
  lExt: string;
begin
  lExt := ExtractFileExt(AFileName);
  if AnsiCompareText(lExt, STR_PDF_EXTENSION) = 0 then Result := vfPDF
  else if AnsiCompareText(lExt, STR_POSTSCRIPT_EXTENSION) = 0 then Result := vfPostScript
  else if AnsiCompareText(lExt, STR_SVG_EXTENSION) = 0 then Result := vfSVG
  else if AnsiCompareText(lExt, STR_SVGZ_EXTENSION) = 0 then Result := vfSVGZ
  else if AnsiCompareText(lExt, STR_CORELDRAW_EXTENSION) = 0 then Result := vfCorelDrawCDR
  else if AnsiCompareText(lExt, STR_WINMETAFILE_EXTENSION) = 0 then Result := vfWindowsMetafileWMF
  else if AnsiCompareText(lExt, STR_AUTOCAD_EXCHANGE_EXTENSION) = 0 then Result := vfDXF
  else if AnsiCompareText(lExt, STR_ENCAPSULATEDPOSTSCRIPT_EXTENSION) = 0 then Result := vfEncapsulatedPostScript
  else if AnsiCompareText(lExt, STR_LAS_EXTENSION) = 0 then Result := vfLAS
  else if AnsiCompareText(lExt, STR_LAZ_EXTENSION) = 0 then Result := vfLAZ
  else if AnsiCompareText(lExt, STR_RAW_EXTENSION) = 0 then Result := vfRAW
  else if AnsiCompareText(lExt, STR_MATHML_EXTENSION) = 0 then Result := vfMathML
  else if AnsiCompareText(lExt, STR_ODG_EXTENSION) = 0 then Result := vfODG
  else
    raise Exception.Create('TvVectorialDocument.GetFormatFromExtension: The extension (' + lExt + ') doesn''t match any supported formats.');
end;

function  TvVectorialDocument.GetDetailedFileFormat(): string;
begin

end;

procedure TvVectorialDocument.GuessDocumentSize();
var
  i, j: Integer;
  lEntity: TvEntity;
  lLeft, lTop, lRight, lBottom: Double;
  CurPage: TvVectorialPage;
begin
  lLeft := 0;
  lTop := 0;
  lRight := 0;
  lBottom := 0;

  for j := 0 to GetPageCount()-1 do
  begin
    CurPage := GetPage(j);
    for i := 0 to CurPage.GetEntitiesCount() - 1 do
    begin
      lEntity := CurPage.GetEntity(I);
      lEntity.ExpandBoundingBox(nil, lLeft, lTop, lRight, lBottom);
    end;
  end;

  Width := lRight - lLeft;
  Height := lBottom - lTop;
end;

procedure TvVectorialDocument.GuessGoodZoomLevel(AScreenSize: Integer);
begin
  ZoomLevel := AScreenSize / Height;
end;

function TvVectorialDocument.GetPage(AIndex: Integer): TvVectorialPage;
begin
  Result := TvVectorialPage(FPages.Items[AIndex]);
end;

function TvVectorialDocument.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

function TvVectorialDocument.GetCurrentPage: TvVectorialPage;
begin
  if FCurrentPageIndex >= 0 then
    Result := GetPage(FCurrentPageIndex)
  else
    Result := nil;
end;

procedure TvVectorialDocument.SetCurrentPage(AIndex: Integer);
begin
  FCurrentPageIndex := AIndex;
end;

function TvVectorialDocument.AddPage: TvVectorialPage;
begin
  Result := TvVectorialPage.Create(Self);
  FPages.Add(Result);
  if FCurrentPageIndex < 0 then FCurrentPageIndex := FPages.Count-1;
end;

function TvVectorialDocument.AddTextPageSequence: TvTextPageSequence;
begin
  Result := TvTextPageSequence.Create(Self);
  FPages.Add(Result);
  if FCurrentPageIndex < 0 then FCurrentPageIndex := FPages.Count-1;
end;

function TvVectorialDocument.AddStyle: TvStyle;
begin
  Result := TvStyle.Create;
  FStyles.Add(Result);
end;

procedure TvVectorialDocument.AddStandardTextDocumentStyles;
var
  lCurStyle: TvStyle;
begin
  lCurStyle.Name := 'Text Body';
  lCurStyle.Font.Size := 12;
  lCurStyle.Font.Name := 'Times New Roman';
  lCurStyle.MarginTop := 0;
  lCurStyle.MarginBottom := 2.12;
  lCurStyle.SetElements := [spbfFontSize, spbfFontName];

  lCurStyle := AddStyle();
  lCurStyle.Name := 'Heading 1';
  lCurStyle.Font.Size := 16;
  lCurStyle.Font.Name := 'Arial';
  lCurStyle.Font.Bold := True;
  lCurStyle.MarginTop := 4.23;
  lCurStyle.MarginBottom := 2.12;
  lCurStyle.MarginTop := 4.23;
  lCurStyle.MarginBottom := 2.12;
  lCurStyle.SetElements := [spbfFontSize, spbfFontName, spbfFontBold, spbfFontItalic];

  lCurStyle := AddStyle();
  lCurStyle.Name := 'Heading 2';
  lCurStyle.Font.Size := 14;
  lCurStyle.Font.Name := 'Arial';
  lCurStyle.Font.Bold := True;
  lCurStyle.Font.Italic := True;
  lCurStyle.SetElements := [spbfFontSize, spbfFontName, spbfFontBold, spbfFontItalic];
end;

function TvVectorialDocument.GetStyleCount: Integer;
begin
  Result := FStyles.Count;
end;

function TvVectorialDocument.GetStyle(AIndex: Integer): TvStyle;
begin
  Result := TvStyle(FStyles.Items[AIndex]);
end;

{@@
  Clears all data in the document
}
// GM: Release memory for each page
procedure TvVectorialDocument.Clear;
var
  i: integer;
  p: TvVectorialPage;
begin
  for i:=FPages.Count-1 downto 0 do
  begin
    p := TvVectorialPage(FPages[i]);
    p.Clear;
    FreeAndNil(p);
  end;
  FPages.Clear;
  FCurrentPageIndex:=-1;
end;

procedure TvVectorialDocument.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc);
var
  i: integer;
  p: TvVectorialPage;
  lPageItem: Pointer;
begin
  for i:=0 to FPages.Count-1 do
  begin
    p := TvVectorialPage(FPages[i]);
    lPageItem := ADestRoutine(Format('Page %d', [i]), nil);
    p.GenerateDebugTree(ADestRoutine, lPageItem);
  end;
end;

{ TvCustomVectorialReader }

constructor TvCustomVectorialReader.Create;
begin
  inherited Create;
end;

procedure TvCustomVectorialReader.ReadFromFile(AFileName: string; AData: TvVectorialDocument);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    ReadFromStream(FileStream, AData);
  finally
    FileStream.Free;
  end;
end;

procedure TvCustomVectorialReader.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
var
  AStringStream: TStringStream;
  AStrings: TStringList;
begin
  AStringStream := TStringStream.Create('');
  AStrings := TStringList.Create;
  try
    AStringStream.CopyFrom(AStream, AStream.Size);
    AStringStream.Seek(0, soFromBeginning);
    AStrings.Text := AStringStream.DataString;
    ReadFromStrings(AStrings, AData);
  finally
    AStringStream.Free;
    AStrings.Free;
  end;
end;

procedure TvCustomVectorialReader.ReadFromStrings(AStrings: TStrings;
  AData: TvVectorialDocument);
var
  AStringStream: TStringStream;
begin
  AStringStream := TStringStream.Create('');
  try
    AStringStream.WriteString(AStrings.Text);
    AStringStream.Seek(0, soFromBeginning);
    ReadFromStream(AStringStream, AData);
  finally
    AStringStream.Free;
  end;
end;

{ TsCustomSpreadWriter }

constructor TvCustomVectorialWriter.Create;
begin
  inherited Create;
end;

{@@
  Default file writting method.

  Opens the file and calls WriteToStream

  @param  AFileName The output file name.
                   If the file already exists it will be replaced.
  @param  AData     The Workbook to be saved.

  @see    TsWorkbook
}
procedure TvCustomVectorialWriter.WriteToFile(AFileName: string; AData: TvVectorialDocument);
var
  OutputFile: TFileStream;
begin
  OutputFile := TFileStream.Create(AFileName, fmCreate or fmOpenWrite);
  try
    WriteToStream(OutputFile, AData);
  finally
    OutputFile.Free;
  end;
end;

{@@
  The default stream writer just uses WriteToStrings
}
procedure TvCustomVectorialWriter.WriteToStream(AStream: TStream;
  AData: TvVectorialDocument);
var
  lStringList: TStringList;
begin
  lStringList := TStringList.Create;
  try
    WriteToStrings(lStringList, AData);
    lStringList.SaveToStream(AStream);
  finally
    lStringList.Free;
  end;
end;

procedure TvCustomVectorialWriter.WriteToStrings(AStrings: TStrings;
  AData: TvVectorialDocument);
begin

end;

finalization

  SetLength(GvVectorialFormats, 0);

end.

