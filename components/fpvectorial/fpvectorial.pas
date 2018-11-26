{
fpvectorial.pas

Vector graphics document

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho
}
unit fpvectorial;

{$ifdef fpc}
  {$mode objfpc}{$h+}
{$endif}

{$define USE_LCL_CANVAS}
{$ifdef USE_LCL_CANVAS}
  {$define USE_CANVAS_CLIP_REGION}
  {.$define DEBUG_CANVAS_CLIP_REGION}
{$endif}
{.$define FPVECTORIAL_DEBUG_DIMENSIONS}
{.$define FPVECTORIAL_TOCANVAS_DEBUG}
{.$define FPVECTORIAL_DEBUG_BLOCKS}
{.$define FPVECTORIAL_AUTOFIT_DEBUG}
{.$define FPVECTORIAL_SUPPORT_LAZARUS_1_6}
// visual debugs
{.$define FPVECTORIAL_TOCANVAS_ELLIPSE_VISUALDEBUG}
{.$define FPVECTORIAL_RENDERINFO_VISUALDEBUG}

interface

uses
  Classes, SysUtils, Math, TypInfo, contnrs, types,
  // FCL-Image
  fpcanvas, fpimage, fpwritebmp,
  // lazutils
  laz2_dom,
  // LCL
  lazutf8, lazregions
  {$ifdef USE_LCL_CANVAS}
  , Graphics, LCLIntf, LCLType, intfgraphics, graphtype, interfacebase
  {$endif}
  ;

type
  TvVectorialFormat = (
    vfUnknown,
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
    vfODT, vfDOCX, vfHTML,
    { Raster Image formats }
    vfRAW
    );

  TvPageFormat = (vpA4, vpA3, vpA2, vpA1, vpA0);

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
  STR_ODT_EXTENSION = '.odt';
  STR_DOCX_EXTENSION = '.docx';
  STR_HTML_EXTENSION = '.html';

  STR_FPVECTORIAL_TEXT_HEIGHT_SAMPLE = 'Ćą';

  NUM_MAX_LISTSTYLES = 8;  // OpenDocument Limit is 10, MS Word Limit is 9

  // Convenience constant to convert text size points to mm
  FPV_TEXT_POINT_TO_MM = 0.35278;

  TWO_PI = 2.0 * pi;

type
  TvCustomVectorialWriter = class;
  TvCustomVectorialReader = class;
  TvPage = class;
  TvVectorialPage = class;
  TvTextPageSequence = class;
  TvEntity = class;
  TPath = class;
  TvVectorialDocument = class;
  TvEmbeddedVectorialDoc = class;
  TvRenderer = class;

  { Coordinates }

  T2DPoint = record
    X, Y: Double;
  end;
  P2DPoint = ^T2DPoint;

  T3DPoint = record
    X, Y, Z: Double;
  end;
  P3DPoint = ^T3DPoint;

  T2DPointsArray = array of T2DPoint;
  T3DPointsArray = array of T3DPoint;
  TPointsArray = array of TPoint;

  { Pen, Brush and Font }

  TvPen = record
    Color: TFPColor;
    Style: TFPPenStyle;
    Width: Integer;
    Pattern: array of LongWord;
  end;
  PvPen = ^TvPen;

  TvBrushKind = (bkSimpleBrush, bkHorizontalGradient, bkVerticalGradient,
    bkOtherLinearGradient, bkRadialGradient);
  TvCoordinateUnit = (vcuDocumentUnit, vcuPercentage);

  TvGradientFlag = (gfRelStartX, gfRelStartY, gfRelEndX, gfRelEndY, gfRelToUserSpace);
  TvGradientFlags = set of TvGradientFlag;

  TvGradientColor = record
    Color: TFPColor;
    Position: Double;   // 0 ... 1
  end;

  TvGradientColors = array of TvGradientColor;

  TvBrush = record
    Color: TFPColor;
    Style: TFPBrushStyle;
    Kind: TvBrushKind;
    Image: TFPCustomImage;
    // Gradient filling support
    Gradient_start: T2DPoint; // Start/end point of gradient, in pixels by default,
    Gradient_end: T2DPoint;   // but if gfRel* in flags relative to entity boundary or user space
    Gradient_flags: TvGradientFlags;
    Gradient_cx, Gradient_cy, Gradient_r, Gradient_fx, Gradient_fy: Double;
    Gradient_cx_Unit, Gradient_cy_Unit, Gradient_r_Unit, Gradient_fx_Unit, Gradient_fy_Unit: TvCoordinateUnit;
    Gradient_colors: TvGradientColors;
  end;
  PvBrush = ^TvBrush;

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
  PvFont = ^TvFont;

  TvSetStyleElement = (
    // Pen, Brush and Font
    spbfPenColor, spbfPenStyle, spbfPenWidth,
    spbfBrushColor, spbfBrushStyle, spbfBrushGradient, spbfBrushKind,
    spbfFontColor, spbfFontSize, spbfFontName, spbfFontBold, spbfFontItalic,
    spbfFontUnderline, spbfFontStrikeThrough, spbfAlignment,
    // TextAnchor
    spbfTextAnchor,
    // Page style
    sseMarginTop, sseMarginBottom, sseMarginLeft, sseMarginRight
    );

  TvSetStyleElements = set of TvSetStyleElement;
  // for backwards compatibility, obsolete
  TvSetPenBrushAndFontElement = TvSetStyleElement;
  TvSetPenBrushAndFontElements = TvSetStyleElements;

  TvStyleKind = (
    // Paragraph kinds
    vskTextBody, vskHeading,
    // Text-span kind
    vskTextSpan);

  TvStyleAlignment = (vsaLeft, vsaRight, vsaJustifed, vsaCenter);

  TvTextAnchor = (vtaStart, vtaMiddle, vtaEnd);

  { TvStyle }

  TvStyle = class
  protected
    FExtraDebugStr: string;
  public
    Name: string;
    Parent: TvStyle; // Can be nil
    Kind: TvStyleKind;
    Alignment: TvStyleAlignment;
    HeadingLevel: Integer;
    //
    Pen: TvPen;
    Brush: TvBrush;
    Font: TvFont;
    TextAnchor: TvTextAnchor;
    // Page style
    MarginTop, MarginBottom, MarginLeft, MarginRight: Double; // in mm
    SuppressSpacingBetweenSameParagraphs : Boolean;
    //
    SetElements: TvSetStyleElements;
    //
    Constructor Create;

    function GetKind: TvStyleKind; // takes care of parenting
    procedure Clear(); virtual;
    procedure CopyFrom(AFrom: TvStyle);
    procedure CopyFromEntity(AEntity: TvEntity);
    procedure ApplyOverFromPen(APen: PvPen; ASetElements: TvSetStyleElements);
    procedure ApplyOverFromBrush(ABrush: PvBrush; ASetElements: TvSetStyleElements);
    procedure ApplyOverFromFont(AFont: PvFont; ASetElements: TvSetStyleElements);
    procedure ApplyOver(AFrom: TvStyle); virtual;
    procedure ApplyIntoEntity(ADest: TvEntity); virtual;
    function CreateStyleCombinedWithParent: TvStyle;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; virtual;
  end;

  TvListStyleKind = (vlskBullet, vlskNumeric);

  TvNumberFormat = (vnfDecimal,      // 0, 1, 2, 3...
                    vnfLowerLetter,  // a, b, c, d...
                    vnfLowerRoman,   // i, ii, iii, iv....
                    vnfUpperLetter,  // A, B, C, D...
                    vnfUpperRoman);  // I, II, III, IV....
  { TvListLevelStyle }

  TvListLevelStyle = Class
    Kind : TvListStyleKind;
    Level : Integer;
    Start : Integer; // For numbered lists only

    // Define the "leader", the stuff in front of each list item
    Prefix : String;
    Suffix : String;
    Bullet : String; // Only applies to Kind=vlskBullet
    NumberFormat : TvNumberFormat; // Only applies to Kind=vlskNumeric
    DisplayLevels : Boolean; // Only applies to numbered lists.
                             // If true, style is 1.1.1.1.
                             //     else style is 1.
    LeaderFontName : String; // Not used by odt...

    MarginLeft : Double; // mm
    HangingIndent : Double; //mm
    Alignment : TvStyleAlignment;

    Constructor Create;
  end;

 { TvListStyle }

  TvListStyle = class
  private
    ListLevelStyles : TFPList;
  public
    Name : String;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function AddListLevelStyle : TvListLevelStyle;
    function GetListLevelStyleCount : Integer;
    function GetListLevelStyle(AIndex: Integer): TvListLevelStyle;
  end;

  { Polyline segments }

  TSegmentType = (
    st2DLine, st2DLineWithPen, st2DBezier,
    st3DLine, st3DBezier, stMoveTo,
    st2DEllipticalArc);

  {@@
    The coordinates in fpvectorial are given in millimeters and
    the starting point is in the bottom-left corner of the document.
    The X grows to the right and the Y grows to the top.
  }
  { TPathSegment }

  TPathSegment = class
  protected
    FPath: TPath;
  public
    SegmentType: TSegmentType;
    // Fields for linking the list
    Previous: TPathSegment;
    Next: TPathSegment;
    // mathematical methods
    function GetLength(): Double; virtual;
    function GetPointAndTangentForDistance(ADistance: Double; out AX, AY, ATangentAngle: Double): Boolean; virtual; // ATangentAngle in radians
    function GetStartPoint(out APoint: T3DPoint): Boolean;
    // edition methods
    procedure Move(ADeltaX, ADeltaY: Double); virtual;
    procedure Rotate(AAngle: Double; ABase: T3DPoint); virtual; // Angle in radians
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; virtual;
    // rendering
    procedure AddToPoints(ADestX, ADestY: Integer; AMulX, AMulY: Double; var Points: TPointsArray); virtual;
    // helper methods
    function UseTopLeftCoordinates: Boolean;
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
    // mathematical methods
    function GetLength(): Double; override;
    function GetPointAndTangentForDistance(ADistance: Double; out AX, AY, ATangentAngle: Double): Boolean; override;
    // edition methods
    procedure Move(ADeltaX, ADeltaY: Double); override;
    procedure Rotate(AAngle: Double; ABase: T3DPoint); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
    // rendering
    procedure AddToPoints(ADestX, ADestY: Integer; AMulX, AMulY: Double; var Points: TPointsArray); override;
  end;

  T2DSegmentWithPen = class(T2DSegment)
  public
    Pen: TvPen;
  end;

  {@@
    In Bezier segments, we remain using the X and Y coordinates for the ending point.
    The starting point is where the previous segment ended, so that the intermediary
    bezier control points are [X2, Y2] and [X3, Y3].

    Equations:

    B(t) = (1-t)³ [Prev.X, Prev.Y] + 3 (1-t)² t [X2, Y2] + 3 (1-t) t² [X3, Y3] + t³ [X,Y], 0<=t<=1

    B'(t) = 3 (1-t)² [X2-Prev.X, Y2-Prev.Y] + 6 (1-t) t [X3-X2, Y3-Y2] + 3 t² [X-X3,Y-Y3]
  }

  { T2DBezierSegment }

  T2DBezierSegment = class(T2DSegment)
  public
    X2, Y2: Double;
    X3, Y3: Double;
    // mathematical methods
    function GetLength(): Double; override;
    function GetPointAndTangentForDistance(ADistance: Double; out AX, AY, ATangentAngle: Double): Boolean; override;
    // edition methods
    procedure Move(ADeltaX, ADeltaY: Double); override;
    procedure Rotate(AAngle: Double; ABase: T3DPoint); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
    // rendering
    procedure AddToPoints(ADestX, ADestY: Integer; AMulX, AMulY: Double; var Points: TPointsArray); override;
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
    // rendering
    procedure AddToPoints(ADestX, ADestY: Integer; AMulX, AMulY: Double; var Points: TPointsArray); override;
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
    RX, RY: Double; // RX and RY are the X and Y half axis sizes
    XRotation: Double;  // rotation of x axis, in radians
    LeftmostEllipse, ClockwiseArcFlag: Boolean;
    CX, CY: Double; // Ellipse center
    CenterSetByUser: Boolean; // defines if we should use LeftmostEllipse to calculate the center, or if CX, CY is set directly
    procedure BezierApproximate(var Points: T3dPointsArray);
    procedure PolyApproximate(var Points: T3dPointsArray);
    procedure CalculateCenter;
    procedure CalculateEllipseBoundingBox(out ALeft, ATop, ARight, ABottom: Double);
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
    procedure AddToPoints(ADestX, ADestY: Integer; AMulX, AMulY: Double; var Points: TPointsArray); override;
    procedure Move(ADeltaX, ADeltaY: Double); override;
    procedure Rotate(AAngle: Double; ABase: T3DPoint); override;
  end;

  TvFindEntityResult = (vfrNotFound, vfrFound, vfrSubpartFound);

  TvRenderInfo = record
    // Input to the rendering, provided by the Document or some other
    // top-level entity and propagated down to all sub-entities
    Page: TvPage;
    Renderer: TvRenderer;
    BackgroundColor: TFPColor;
    AdjustPenColorToBackground: Boolean;
    Selected: TvEntity;
    Canvas: TFPCustomCanvas;
    DestX: Integer;
    DestY: Integer;
    MulX: Double;
    MulY: Double;

    // Input to the rendering, other inputs
    ForceRenderBlock: Boolean; // Blocks are usually invisible, but when rendering an insert, their drawing can be forced

    // Fields which are output from the rendering process
    EntityCanvasMinXY, EntityCanvasMaxXY: TPoint; // The size utilized in the canvas to draw this entity, in pixels

    // errors
    SelfEntity: TvEntity;
    Parent: TvEntity;
    Errors: TStrings;
  end;

  TvEntityFeatures = record
    DrawsUpwards: Boolean; // TvText, TvEmbeddedVectorialDoc, etc draws upwards, but in the future we might have entities drawing downwards
    DrawsUpwardHeightAdjustment: Integer; // in Canvas pixels
    FirstLineHeight: Integer; // in Canvas pixels
    TotalHeight: Integer; // in Canvas pixels
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
    constructor Create(APage: TvPage); virtual;
    procedure Clear; virtual;
    procedure SetPage(APage: TvPage); virtual;
    // in CalculateBoundingBox always remember to treat correctly the case of ADest=nil!!!
    // This cased is utilized to guess the size of a document even before getting a canvas to draw at
    procedure CalculateBoundingBox(constref ARenderInfo: TvRenderInfo; out ALeft, ATop, ARight, ABottom: Double); virtual;
    function CalculateSizeInCanvas(constref ARenderInfo: TvRenderInfo; APageHeight: Integer; AZoom: Double; out ALeft, ATop, AWidth, AHeight: Integer): Boolean;
    procedure CalculateHeightInCanvas(constref ARenderInfo: TvRenderInfo; out AHeight: Integer);
    // helper functions for CalculateBoundingBox & TvRenderInfo
    procedure ExpandBoundingBox(constref ARenderInfo: TvRenderInfo; var ALeft, ATop, ARight, ABottom: Double);
    class procedure CalcEntityCanvasMinMaxXY(var ARenderInfo: TvRenderInfo; APointX, APointY: Integer);
    class procedure CalcEntityCanvasMinMaxXY_With2Points(var ARenderInfo: TvRenderInfo; AX1, AY1, AX2, AY2: Integer);
    procedure MergeRenderInfo(var AFrom, ATo: TvRenderInfo);
    class procedure InitializeRenderInfo(var ARenderInfo: TvRenderInfo; ASelf: TvEntity; ACreateObjs: Boolean = False);
    class procedure FinalizeRenderInfo(var ARenderInfo: TvRenderInfo);
    class procedure CopyAndInitDocumentRenderInfo(out ATo: TvRenderInfo; AFrom: TvRenderInfo; ACopyMinMax: Boolean = False; AAsChild: Boolean = True);
    function RenderInfo_GenerateParentTree(constref ARenderInfo: TvRenderInfo): string;
    function CentralizeY_InHeight(constref ARenderInfo: TvRenderInfo; AHeight: Double): Double;
    function  GetHeight(constref ARenderInfo: TvRenderInfo): Double;
    function  GetWidth(constref ARenderInfo: TvRenderInfo): Double;
    {@@ ASubpart is only valid if this routine returns vfrSubpartFound }
    function GetLineIntersectionPoints(ACoord: Double;
      ACoordIsX: Boolean): TDoubleDynArray; virtual; // get all points where the entity inner area crosses a line
    function TryToSelect(APos: TPoint; var ASubpart: Cardinal; ASnapFlexibility: Integer = 5): TvFindEntityResult; virtual;
    procedure Move(ADeltaX, ADeltaY: Double); virtual;
    procedure MoveSubpart(ADeltaX, ADeltaY: Double; ASubpart: Cardinal); virtual;
    function  GetSubpartCount: Integer; virtual;
    procedure PositionSubparts(constref ARenderInfo: TvRenderInfo; ABaseX, ABaseY: Double); virtual;
    procedure Scale(ADeltaScaleX, ADeltaScaleY: Double); virtual;
    procedure Rotate(AAngle: Double; ABase: T3DPoint); virtual; // Angle in radians
    // ADoDraw = False means that no drawing will actually be done, only the size info will be filled in ARenderInfo
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); virtual;
    function AdjustColorToBackground(AColor: TFPColor; ARenderInfo: TvRenderInfo): TFPColor;
    function GetNormalizedPos(APage: TvVectorialPage; ANewMin, ANewMax: Double): T3DPoint;
    function GetEntityFeatures(constref ARenderInfo: TvRenderInfo): TvEntityFeatures; virtual;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; virtual;
    class function GenerateDebugStrForFPColor(AColor: TFPColor): string;
    class function GenerateDebugStrForString(AValue: string): string;
  end;

  TvEntityClass = class of TvEntity;

  { TvNamedEntity }

  TvNamedEntity = class(TvEntity)
  protected
    FExtraDebugStr: string;
    FPage: TvPage;
  public
    Name: string;
    constructor Create(APage: TvPage); override;
    procedure SetPage(APage: TvPage); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  { TvEntityWithPen }

  TvEntityWithPen = class(TvNamedEntity)
  protected
    function CreatePath: TPath; virtual;
  public
    {@@ The global Pen for the entire entity. In the case of paths, individual
        elements might be able to override this setting. }
    Pen: TvPen;
    constructor Create(APage: TvPage); override;
    procedure ApplyPenToCanvas(constref ARenderInfo: TvRenderInfo); overload;
    procedure ApplyPenToCanvas(constref ARenderInfo: TvRenderInfo; APen: TvPen); overload;
    procedure AssignPen(APen: TvPen);
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
  end;

  { TvEntityWithPenAndBrush }

  TvClipMode = (vcmNonzeroWindingRule, vcmEvenOddRule);

  TvEntityWithPenAndBrush = class(TvEntityWithPen)
  public
    procedure CalcGradientVector(out AGradientStart, AGradientEnd: T2dPoint;
      const ARect: TRect; ADestX: Integer = 0; ADestY: Integer = 0;
      AMulX: Double = 1.0; AMulY: Double = 1.0);
    procedure DrawPolygon(var ARenderInfo: TvRenderInfo;
      const APoints: TPointsArray; const APolyStarts: TIntegerDynArray; ARect: TRect);
    procedure DrawPolygonBrushLinearGradient(var ARenderInfo: TvRenderInfo;
      const APoints: TPointsArray;const APolyStarts: TIntegerDynArray;
      ARect: TRect; AGradientStart, AGradientEnd: T2DPoint);
    procedure DrawPolygonBrushRadialGradient(var ARenderInfo: TvRenderInfo;
      const APoints: TPointsArray; ARect: TRect);
    procedure DrawNativePolygonBrushRadialGradient(var ARenderInfo: TvRenderInfo;
      const APoints: TPointsArray; ARect: TRect);
    procedure DrawPolygonBorderOnly(var ARenderInfo: TvRenderInfo; const APoints: TPointsArray);
  public
    {@@ The global Brush for the entire entity. In the case of paths, individual
        elements might be able to override this setting. }
    Brush: TvBrush;
    WindingRule: TvClipMode;
    constructor Create(APage: TvPage); override;
    procedure ApplyBrushToCanvas(constref ARenderInfo: TvRenderInfo); overload;
    procedure ApplyBrushToCanvas(constref ARenderInfo: TvRenderInfo; ABrush: PvBrush); overload;
    procedure AssignBrush(ABrush: PvBrush);
    procedure DrawBrush(var ARenderInfo: TvRenderInfo);
    procedure DrawBrushGradient(var ARenderInfo: TvRenderInfo; x1, y1, x2, y2: Integer); virtual;
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  { TvEntityWithPenBrushAndFont }

  TvEntityWithPenBrushAndFont = class(TvEntityWithPenAndBrush)
  public
    Font: TvFont;
    TextAnchor: TvTextAnchor;
    constructor Create(APage: TvPage); override;
    procedure ApplyFontToCanvas(ARenderInfo: TvRenderInfo); overload;
    procedure ApplyFontToCanvas(ARenderInfo: TvRenderInfo; AFont: TvFont); overload;
    procedure AssignFont(AFont: TvFont);
    procedure Rotate(AAngle: Double; ABase: T3DPoint); override; // Angle in radians
    procedure Scale(ADeltaScaleX, ADeltaScaleY: Double); override;
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  { TvEntityWithStyle }

  TvEntityWithStyle = class(TvEntityWithPenBrushAndFont)
  public
    Style: TvStyle; // can be nil!
    constructor Create(APage: TvPage); override;
    destructor Destroy; override;
    function GetCombinedStyle(AParent: TvEntityWithStyle): TvStyle;
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
  end;

  TPath = class(TvEntityWithPenAndBrush)
  private
    // Used to speed up sequencial access in MoveSubpart
    FCurMoveSubPartIndex: Integer;
    FCurMoveSubPartSegment: TPathSegment;
    //
  public
    FPolyPoints: TPointsArray;
    FPolyStarts: TIntegerDynArray;
  public
    Len: Integer;
    Points: TPathSegment;   // Beginning of the double-linked list
    PointsEnd: TPathSegment;// End of the double-linked list
    CurPoint: TPathSegment; // Used in PrepareForSequentialReading and Next
    CurWalkDistanceInCurSegment: Double;// Used in PrepareForWalking and NextWalk
    ClipPath: TPath;
    ClipMode: TvClipMode;
    constructor Create(APage: TvPage); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(ASource: TPath);
    procedure PrepareForSequentialReading;
    procedure PrepareForWalking;
    function Next(): TPathSegment;
    function NextWalk(ADistance: Double; out AX, AY, ATangentAngle: Double): Boolean;
    procedure CalculateBoundingBox(constref ARenderInfo: TvRenderInfo; out ALeft, ATop, ARight, ABottom: Double); override;
    procedure AppendSegment(ASegment: TPathSegment);
    procedure AppendMoveToSegment(AX, AY: Double);
    procedure AppendLineToSegment(AX, AY: Double);
    procedure AppendEllipticalArc(ARadX, ARadY, AXAxisRotation, ADestX, ADestY: Double; ALeftmostEllipse, AClockwiseArcFlag: Boolean); // See http://www.w3.org/TR/SVG/paths.html#PathDataEllipticalArcCommands
    procedure AppendEllipticalArcWithCenter(ARadX, ARadY, AXAxisRotation, ADestX, ADestY, ACenterX, ACenterY: Double; AClockwiseArcFlag: Boolean); // See http://www.w3.org/TR/SVG/paths.html#PathDataEllipticalArcCommands
    function GetLineIntersectionPoints(ACoord: Double; ACoordIsX: Boolean): TDoubleDynArray; override;
    procedure Move(ADeltaX, ADeltaY: Double); override;
    procedure MoveSubpart(ADeltaX, ADeltaY: Double; ASubpart: Cardinal); override;
    function  MoveToSubpart(ASubpart: Cardinal): TPathSegment;
    function  GetSubpartCount: Integer; override;
    procedure Rotate(AAngle: Double; ABase: T3DPoint); override; // Angle in radians
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    procedure RenderInternalPolygon(constref ARenderInfo: TvRenderInfo);
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  {@@
    TvText represents a text entity.

    The text starts in X, Y and grows upwards, towards a bigger Y (fpvectorial coordinates)
    or smaller Y (LCL coordinates).
    It has the opposite direction of text in the LCL TCanvas.
  }


  { TvText }

  TvText = class(TvEntityWithStyle)
  private
    function GetTextMetric_Descender_px(constref ARenderInfo: TvRenderInfo): Integer;
  public
    Value: TStringList;
    Render_NextText_X: Integer;
    Render_Use_NextText_X: Boolean;
    constructor Create(APage: TvPage); override;
    destructor Destroy; override;
    function TryToSelect(APos: TPoint; var ASubpart: Cardinal; ASnapFlexibility: Integer = 5): TvFindEntityResult; override;
    procedure CalculateBoundingBox(constref ARenderInfo: TvRenderInfo; out ALeft, ATop, ARight, ABottom: Double); override;
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    function GetEntityFeatures(constref ARenderInfo: TvRenderInfo): TvEntityFeatures; override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  { TvCurvedText }

  // TvCurvedText supports only one line
  TvCurvedText = class(TvText)
  public
    Path: TPath;
    //constructor Create(APage: TvPage); override;
    //destructor Destroy; override;
    //function TryToSelect(APos: TPoint; var ASubpart: Cardinal): TvFindEntityResult; override;
    //procedure CalculateBoundingBox(ADest: TFPCustomCanvas; out ALeft, ATop, ARight, ABottom: Double); override;
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    //function GetEntityFeatures: TvEntityFeatures; override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  TvFieldKind = (vfkNumPages, vfkPage, vfkAuthor, vfkDateCreated, vfkDate);

  { TvField }

  TvField = Class(TvEntityWithStyle)
  public
    Kind : TvFieldKind;

    DateFormat : String;            // Only for Kind in (vfkDateCreated, vfkDate)
                                    // Date Format is similar to MS Specification
    NumberFormat : TvNumberFormat;  // Only for Kind in (vfkNumPages, vfkPage)

    constructor Create(APage : TvPage); override;
  end;

  {@@
  }

  { TvCircle }

  TvCircle = class(TvEntityWithPenAndBrush)
  protected
    function CreatePath: TPath; override;
  public
    Radius: Double;
    procedure CalculateBoundingBox(constref ARenderInfo: TvRenderInfo; out ALeft, ATop, ARight, ABottom: Double); override;
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    procedure Rotate(AAngle: Double; ABase: T3DPoint); override; // Angle in radians
  end;

  {@@
  }

  { TvCircularArc }

  TvCircularArc = class(TvEntityWithPenAndBrush)
  public
    Radius: Double;
    {@@ The Angle is measured in degrees in relation to the positive X axis }
    StartAngle, EndAngle: Double;
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
  end;

  {@@
  }

  { TvEllipse }

  TvEllipse = class(TvEntityWithPenAndBrush)
  protected
    function CreatePath: TPath; override;
  public
    // Mandatory fields
    HorzHalfAxis: Double; // This half-axis is the horizontal one when Angle=0
    VertHalfAxis: Double; // This half-axis is the vertical one when Angle=0
    {@@ The Angle is measured in radians in relation to the positive X axis }
    Angle: Double;
    function GetLineIntersectionPoints(ACoord: Double; ACoordIsX: Boolean): TDoubleDynArray; override;
    function TryToSelect(APos: TPoint; var ASubpart: Cardinal; ASnapFlexibility: Integer = 5): TvFindEntityResult; override;
    procedure CalculateBoundingBox(constref ARenderInfo: TvRenderInfo; out ALeft, ATop, ARight, ABottom: Double); override;
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    procedure Rotate(AAngle: Double; ABase: T3DPoint); override; // Angle in radians
  end;

  { TvRectangle }
  { The point (X,Y) refers to the left/top corner of the rectangle! }

  TvRectangle = class(TvEntityWithPenBrushAndFont)
  protected
    function CreatePath: TPath; override;
  public
    // A text displayed in the center of the square, usually empty
    Text: string;
    // Mandatory fields
    CX, CY, CZ: Double;  // CX = width, CY = height, CZ = depth
    // Corner rounding, zero indicates no rounding
    RX, RY: Double;
    // The Angle is measured in radians relative to the positive X axis.
    // Center of rotation is (X,Y).
    Angle: Double;
    procedure CalculateBoundingBox(constref ARenderInfo: TvRenderInfo; out ALeft, ATop, ARight, ABottom: Double); override;
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    procedure Rotate(AAngle: Double; ABase: T3DPoint); override; // Angle in radians
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  { TvPolygon }

  TvPolygon = class(TvEntityWithPenBrushAndFont)
  public
    // A text displayed in the center of the square, usually empty
    Text: string;
    // All points of the polygon
    Points: array of T3DPoint;
    procedure CalculateBoundingBox(constref ARenderInfo: TvRenderInfo; out ALeft, ATop, ARight, ABottom: Double); override;
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
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
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
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
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
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
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    procedure CalculateExtraArcInfo;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  {@@
   Vectorial images can contain raster images inside them and this entity
   represents this.

   If the Width and Height differ from the same data in the image, then
   the raster image will be stretched.

   X,Y represents the top-left corner of the image

   Note that TFPCustomImage does not implement a storage, so the property
   RasterImage should be filled with either a FPImage.TFPMemoryImage or with
   a TLazIntfImage. The property RasterImage might be nil.
  }

  { TvRasterImage }

  TvRasterImage = class(TvNamedEntity)
  public
    RasterImage: TFPCustomImage;
    Width, Height: Double;
    AltText: string;
    destructor Destroy; override;
    procedure CalculateBoundingBox(constref ARenderInfo: TvRenderInfo; out ALeft, ATop, ARight, ABottom: Double); override;
    procedure CreateRGB888Image(AWidth, AHeight: Cardinal);
    procedure CreateImageFromFile(AFilename: string);
    procedure CreateImageFromStream(AStream: TStream; Handler:TFPCustomImageReader);
    procedure InitializeWithConvertionOf3DPointsToHeightMap(APage: TvVectorialPage; AWidth, AHeight: Integer);
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  { TvPoint }

  // Keep TvPoint as small as possible in memory foot-print for LAS support
  TvPoint = class(TvEntity)
  public
    Pen: TvPen;
    {constructor Create; override;
    procedure ApplyPenToCanvas(ADest: TFPCustomCanvas; ARenderInfo: TvRenderInfo);
    procedure Render(ADest: TFPCustomCanvas; var ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
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
    procedure CalculateBoundingBox(constref ARenderInfo: TvRenderInfo; out ALeft, ATop, ARight, ABottom: Double); override;
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
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
    // More complex elements, utilized for graphical representation of formula
    fekFraction,  // a division with Formula on the top and AdjacentFormula in the bottom
    fekRoot,      // A root. For example sqrt(something). Number gives the root, usually 2, and inside it goes a Formula
    fekPower,     // A Formula elevated to a AdjacentFormula, example: 2^5
    fekSubscript, // A Formula with a subscripted element AdjacentFormula, example: Xi
    fekSummation, // Sum of a variable given by Text set by Formula in the bottom and going up to AdjacentFormula in the top
    fekFormula,   // A formula, stored in Formula
    // Elements utilized for formulas for infix to RPN converion, not utilized for graphical representations
    fekParentesesOpen,
    freParentesesClose
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
    function CalculateHeight(ADest: TFPCustomCanvas): Double; // in millimeters
    function CalculateWidth(ADest: TFPCustomCanvas): Double; // in millimeters
    function AsText: string;
    procedure PositionSubparts(constref ARenderInfo: TvRenderInfo; ABaseX, ABaseY: Double);
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); virtual;
    procedure GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer); virtual;
    class function GetPrecedenceFromKind(AKind: TvFormulaElementKind): Byte; // 0 is the smallest precedence
    class function IsLeftAssociativeFromKind(AKind: TvFormulaElementKind): Boolean;
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
    constructor Create(APage: TvPage); override;
    destructor Destroy; override;
    //
    function GetFirstElement: TvFormulaElement;
    function GetNextElement: TvFormulaElement;
    procedure AddElement(AElement: TvFormulaElement);
    function  AddElementWithKind(AKind: TvFormulaElementKind): TvFormulaElement;
    function  AddElementWithKindAndText(AKind: TvFormulaElementKind; AText: string): TvFormulaElement;
    procedure AddItemsByConvertingInfixToRPN(AInfix: TFPList {of TvFormulaElement});
    procedure AddItemsByConvertingInfixStringToRPN(AStr: string);
    procedure TokenizeInfixString(AStr: string; AOutput: TFPList);
    function  CalculateRPNFormulaValue: Double;
    procedure Clear; override;
    //
    function CalculateHeight(ADest: TFPCustomCanvas): Double; virtual; // in millimeters
    function CalculateWidth(ADest: TFPCustomCanvas): Double; virtual; // in millimeters
    procedure PositionSubparts(constref ARenderInfo: TvRenderInfo; ABaseX, ABaseY: Double); override;
    procedure CalculateBoundingBox(constref ARenderInfo: TvRenderInfo; out ALeft, ATop, ARight, ABottom: Double); override;
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  { TvVerticalFormulaStack }

  TvVerticalFormulaStack = class(TvFormula)
  public
    function CalculateHeight(ADest: TFPCustomCanvas): Double; override; // in millimeters
    function CalculateWidth(ADest: TFPCustomCanvas): Double; override; // in millimeters
    procedure PositionSubparts(constref ARenderInfo: TvRenderInfo; ABaseX, ABaseY: Double); override;
    //function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  {@@
    A EntityWithSubEntities may have Pen, Brush and/or Font data associated with it, but it is disabled by default
    This data can be active recursively in all children of the group if set in the field
    SetPenBrushAndFontElements
  }

  { TvEntityWithSubEntities }

  TvEntityWithSubEntities = class(TvEntityWithStyle)
  private
    FCurIndex: Integer;
    procedure CallbackDeleteElement(data,arg:pointer);
  protected
    FElements: TFPList; // of TvEntity
  public
    SetPenBrushAndFontElements: TvSetPenBrushAndFontElements;// This is not currently implemented!
    constructor Create(APage: TvPage); override;
    destructor Destroy; override;
    //
    function GetFirstEntity: TvEntity;
    function GetNextEntity: TvEntity;
    function GetEntitiesCount: Integer;
    function GetEntity(AIndex: Integer): TvEntity;
    function AddEntity(AEntity: TvEntity): Integer;
    function GetEntityIndex(AEntity : TvEntity) : Integer;
    function  DeleteEntity(AIndex: Cardinal): Boolean;
    function  RemoveEntity(AEntity: TvEntity; AFreeAfterRemove: Boolean = True): Boolean;
    procedure Rotate(AAngle: Double; ABase: T3DPoint); override;
    procedure Clear; override;
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
    function FindEntityWithReference(AEntity: TvEntity): Integer;
    function FindEntityWithNameAndType(AName: string; AType: TvEntityClass {= TvEntity}; ARecursively: Boolean = False): TvEntity;
  end;

  {@@
    A block is a group of other elements. It is not rendered directly into the drawing,
    but instead is rendered via another item, called TvInsert
  }

  { TvBlock }

  TvBlock = class(TvEntityWithSubEntities)
  public
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
  end;

  {@@
    A "Insert" inserts a copy of any other element in the specified position.
    Usually TvBlock entities are inserted, but any entity can be inserted.
  }

  { TvInsert }

  TvInsert = class(TvEntityWithStyle) // instead of TvNamedEntity so that it can pass its own style info to the InsertEntity
  public
    InsertEntity: TvEntity; // The entity to be inserted
    RotationAngle: Double; // in angles, normal is zero
    SetElements: TvSetStyleElements; // Defines which of Pen, Brush and Font will be applied to InsertEntity
    constructor Create(APage: TvPage); override;
    destructor Destroy; override;
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
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
    TvParagraph represents a sequence of elements ordered as characters
    in a paragraph.
    The elements might be richly formatted text, but also images.

    The basic element to build the sequence is TvText. Note that the X, Y positions
    of elements will be all adjusted to fit the TvParagraph area
  }

  TvRichTextAutoExpand = (rtaeNone, etaeWidth, etaeHeight);

  { TvParagraph }

  TvParagraph = class(TvEntityWithSubEntities)
  public
    Width, Height: Double;
    AutoExpand: TvRichTextAutoExpand;
    ListStyle : TvListStyle; // For Bulleted or Numbered Lists...
    YPos_NeedsAdjustment_DelFirstLineBodyHeight: Boolean; // SVG coordinates for text are cumbersome, we need this
    constructor Create(APage: TvPage); override;
    destructor Destroy; override;
    function AddText(AText: string): TvText;
    function AddCurvedText(AText: string): TvCurvedText;
    function AddField(AKind : TvFieldKind): TvField;
    function AddRasterImage: TvRasterImage;
    function AddEmbeddedVectorialDoc: TvEmbeddedVectorialDoc;
    procedure CalculateBoundingBox(constref ARenderInfo: TvRenderInfo; out ALeft, ATop, ARight, ABottom: Double); override;
    function TryToSelect(APos: TPoint; var ASubpart: Cardinal; ASnapFlexibility: Integer = 5): TvFindEntityResult; override;
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  {@@
    TvList represents a list of bulleted texts, like:

    * First level
      - Second level
    * First level again

    The basic element to build the sequence is TvParagraph
  }

  { TvList }

  TvList = class(TvEntityWithSubEntities)
  public
    Parent : TvList;
    ListStyle : TvListStyle;

    constructor Create(APage: TvPage); override;  // MJT 31/08 added override;
    destructor Destroy; override;
    // helper function to add the most often used sub-entities
    function AddParagraph(ASimpleText: string): TvParagraph;
    function AddList: TvList;
    // other helper functions
    function GetLevel: Integer;
    function GetBulletSize: Double;
    procedure DrawBullet(ADest: TFPCustomCanvas; var ARenderInfo: TvRenderInfo;
      ALevel: Integer; AX, AY: Double; ADestX: Integer = 0; ADestY: Integer = 0;
      AMulX: Double = 1.0; AMulY: Double = 1.0; ADoDraw: Boolean = True);
    // overrides
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    //function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;}
  end;

  {@@
    TvRichText represents a sequence of text paragraphs.

    The basic element to build the sequence is TvParagraph. Note that the X, Y positions
    of elements will be all adjusted to fit the TvRichText area
  }

  // Forward reference as Table Cells are TvRichText which in turn
  // can also contain tables...
  TvTable = class;
  TvTableRow = class;
(*
  TvImage = Class;
*)
  { TvRichText }

  TvRichText = class(TvEntityWithSubEntities)
  public
    Width, Height: Double;
    SpacingLeft, SpacingRight, SpacingTop, SpacingBottom: Double; // space around each side
    AutoExpand: TvRichTextAutoExpand;
    constructor Create(APage: TvPage); override;
    destructor Destroy; override;
    // Data writing methods
    function AddParagraph: TvParagraph;
    function AddList: TvList;
    function AddTable: TvTable;
    function AddEmbeddedVectorialDoc: TvEmbeddedVectorialDoc;
    function AddRasterImage: TvRasterImage;
    // Functions for rendering and calculating sizes
    procedure GetEffectiveCellSpacing(out ATopSpacing, ALeftSpacing, ARightSpacing, ABottomSpacing: Double); virtual;
    function CalculateCellHeight_ForWidth(constref ARenderInfo: TvRenderInfo; AWidth: Double): Double; virtual;
    function CalculateMaxNeededWidth(constref ARenderInfo: TvRenderInfo): Double; virtual;
    function TryToSelect(APos: TPoint; var ASubpart: Cardinal; ASnapFlexibility: Integer = 5): TvFindEntityResult; override;
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  (*  Support for Adding Tables to the document
      Each Cell is a TvRichText to allow full formatted text contents
  *)

  TvUnits = (dimMillimeter, dimPercent, dimPoint);

  TvDimension = record
    Value : Double;
    Units : TvUnits;
  end;

  // Use tbtDefault if you don't want the Border settings to be written out
  TvTableBorderType = (tbtSingle, tbtDashed, tbtDouble, tbtNone, tbtDefault);

  TvTableBorder = record
    LineType : TvTableBorderType;
    Spacing : Double; // mm, default 0
    Color : TFPColor; // Ignored if (0, 0, 0, 0)
    Width : Double;   // mm, default 0.  Should really be in point for fine control
  end;

  // Can be applied to Tables AND Cells
  TvTableBorders = record
    Left : TvTableBorder;
    Right : TvTableBorder;
    Top : TvTableBorder;
    Bottom : TvTableBorder;
    InsideHoriz : TvTableBorder;  //  InsideXXX not normally applied to cells
    InsideVert : TvTableBorder;   //    (MS Word Table Styles has an exception)
  end;

  { TvTableCell }

  TvVerticalAlignment = (vaTop, vaBottom, vaCenter, cvaBoth);
  // Horizontal alignment taken from Paragraph Style

  TvTableCell = Class(TvRichText)
  public
    // MJT to Felipe:  It may be that Borders can be
    // added to TvRichText if odt supports paragraph
    // borders, in which case we can refactor a little and
    // rename TvTableBorders
    Row: TvTableRow;
    Borders: TvTableBorders;                  // Defaults to be ignored (tbtDefault)
    PreferredWidth: TvDimension;              // Optional
    VerticalAlignment: TvVerticalAlignment;   // Defaults to vaTop
    BackgroundColor: TFPColor;                // Optional
    BackgroundColorValid: Boolean;
    SpannedCols: Integer;                     // For merging horiz cells.  Default 1.
                                              // See diagram above TvTable Class
    SpacingDataValid: Boolean;                // TvRichText defines spacing, SpacingTop, SpacingLeft, etc
                                              // if SpacingDataValid is false use Row.Table.CallSpacing
                                              // instead. Units for SpacingTop, etc, in mm. Spacing is the
                                              // empty area around Cells (but inside them) without content.

    constructor Create(APage: TvPage); override;
    function GetEffectiveBorder(): TvTableBorders;
    procedure GetEffectiveCellSpacing(out ATopSpacing, ALeftSpacing, ARightSpacing, ABottomSpacing: Double); override;
    class procedure DrawBorder(ABorder: TvTableBorders;
      AX, AY, AWidth, AHeight: double;
      ADest: TFPCustomCanvas; var ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
    class function GenerateDebugStrForBorders(ABorders: TvTableBorders): string;
  end;

  { TvTableRow }

  TvTableRow = Class(TvNamedEntity)
  private
    Cells: TFPList; // of TvTableCell
  Public
    Table: TvTable;                // Link to the parent table
    Height: Double;                // Units mm.  Use 0 for default height
    Header: Boolean;               // Repeat row across pages
    AllowSplitAcrossPage : Boolean;// Can this Row split across multiple pages?
    BackgroundColor: TFPColor;     // Optional
    BackgroundColorValid: Boolean;
    // row spacing data in mm, necessary for docx among other formats
    CellSpacing: Double;
    SpacingDataValid: Boolean;

    constructor create(APage : TvPage); override;
    destructor destroy; override;

    function AddCell: TvTableCell;
    function GetCellCount: Integer;
    function GetCell(AIndex: Integer): TvTableCell;
    function GetCellColNr(ACell: TvTableCell): Integer;
    function CalculateMaxCellSpacing_Y(): Double;
    //
    procedure CalculateBoundingBox(constref ARenderInfo: TvRenderInfo; out ALeft, ATop, ARight, ABottom: Double); override;
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  (*
      Note on the grid used for the table

      For the table shown below, three ColWidths must be defined.

      First row should only have 2 cells. First cell spans 2 columns.
      Second row should only have 2 cells. Second cell spans 2 columns.
      Third row should have 3 cells.  Each cell only spans 1 column (default)

   X,Y +-----+------+---------+
       |            |         |
       +-----+----------------+
       |     |                |
       +-----+------+---------+
       |     |      |         |
       +-----+------+---------+

       The table draws at X,Y and downwards
  *)

  // TvTable.Style should be a Table Style, not a Paragraph Style
  // and is optional.
  TvTable = class(TvEntityWithStyle)
  private
    Rows: TFPList;
    ColWidthsInMM: array of Double;   // calculated during Render
    TableWidth, TableHeight: Double;  // in mm; calculated during Render
    procedure CalculateColWidths(constref ARenderInfo: TvRenderInfo);
    procedure CalculateRowHeights(constref ARenderInfo: TvRenderInfo);
  public
    ColWidths: array of Double;       // Can be left empty for simple tables
                                      // MUST be fully defined for merging cells
    ColWidthsUnits : TvUnits;         // Cannot mix ColWidth Units.
    Borders : TvTableBorders;         // Defaults: single/black/inside and out
    PreferredWidth : TvDimension;     // Optional. Units mm.
    SpacingBetweenCells: Double;      // Units mm. Gap between Cells.
    CellSpacingLeft, CellSpacingRight, CellSpacingTop,
      CellSpacingBottom: Double;      // space around each side of cells, in mm
    BackgroundColor : TFPColor;       // Optional.

    constructor create(APage : TvPage); override;
    destructor destroy; override;

    function AddRow: TvTableRow;
    function GetRowCount : Integer;
    function GetRow(AIndex: Integer) : TvTableRow;
    //
    function GetColCount(): Integer;
    //
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  { TvEmbeddedVectorialDoc }

  TvEmbeddedVectorialDoc = class(TvEntity)
  private
    FWidth, FHeight: Double;
  public
    Document: TvVectorialDocument;
    constructor create(APage : TvPage); override;
    destructor destroy; override;
    procedure UpdateDocumentSize();
    function GetWidth: Double;
    function GetHeight: Double;
    procedure SetWidth(AValue: Double);
    procedure SetHeight(AValue: Double);
    procedure CalculateBoundingBox(constref ARenderInfo: TvRenderInfo; out ALeft, ATop, ARight, ABottom: Double); override;
    procedure Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True); override;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  TvVectorialReaderFlag = (vrfSVG_UseBottomLeftCoords, vrfWMF_UseBottomLeftCoords);
  TvVectorialReaderFlags = set of TvVectorialReaderFlag;

  TvVectorialReaderSettings = record
    VecReaderFlags: TvVectorialReaderFlags;
    HelperToolPath: string;
  end;

  { TvVectorialDocument }

  TvVectorialDocument = class
  private
    FOnProgress: TvProgressEvent;
    FPages: TFPList;
    FStyles: TFPList;
    FListStyles: TFPList;
    FCurrentPageIndex: Integer;
    FRenderer: TvRenderer;
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
    // List of common styles, for conveniently finding them
    StyleTextBody, StyleHeading1, StyleHeading2, StyleHeading3,
      StyleHeading4, StyleHeading5, StyleHeading6: TvStyle;
    StyleTextBodyCentralized, StyleTextBodyBold: TvStyle; // text body modifications
    StyleHeading1Centralized, StyleHeading2Centralized, StyleHeading3Centralized: TvStyle; // heading modifications
    StyleBulletList, StyleNumberList : TvListStyle;
    StyleTextSpanBold, StyleTextSpanItalic, StyleTextSpanUnderline: TvStyle;
    // Reader properties
    ReaderSettings: TvVectorialReaderSettings;
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
    procedure ReadFromXML(ADoc: TXMLDocument; AFormat: TvVectorialFormat);
    class function GetFormatFromExtension(AFileName: string; ARaiseException: Boolean = True): TvVectorialFormat;
    function  GetDetailedFileFormat(): string;
    procedure GuessDocumentSize();
    procedure GuessGoodZoomLevel(AScreenSize: Integer = 500);
    { Page methods }
    function GetPage(AIndex: Integer): TvPage;
    function GetPageIndex(APage : TvPage): Integer;
    function GetPageAsVectorial(AIndex: Integer): TvVectorialPage;
    function GetPageAsText(AIndex: Integer): TvTextPageSequence;
    function GetPageCount: Integer;
    function GetCurrentPage: TvPage;
    function GetCurrentPageAsVectorial: TvVectorialPage;
    procedure SetCurrentPage(AIndex: Integer);
    procedure SetDefaultPageFormat(AFormat: TvPageFormat);
    function AddPage(AUseTopLeftCoords: Boolean = False): TvVectorialPage;
    function AddTextPageSequence(): TvTextPageSequence;
    { Style methods }
    function AddStyle(): TvStyle;
    function AddListStyle: TvListStyle;
    procedure AddStandardTextDocumentStyles(AFormat: TvVectorialFormat);
    function GetStyleCount: Integer;
    function GetStyle(AIndex: Integer): TvStyle;
    function FindStyleIndex(AStyle: TvStyle): Integer;
    function GetListStyleCount: Integer;
    function GetListStyle(AIndex: Integer): TvListStyle;
    function FindListStyleIndex(AListStyle: TvListStyle): Integer;
    { Data removing methods }
    procedure Clear; virtual;
    { Drawer selection methods }
    function GetRenderer: TvRenderer;
    procedure SetRenderer(ARenderer: TvRenderer);
    procedure ClearRenderer();
    { Debug methods }
    procedure GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer = nil);
    { Events }
    property OnProgress: TvProgressEvent read FOnProgress write FOnprogress;
  end;

  { TvPage }

  TvPage = class
  private
    procedure InitializeRenderInfo(out ARenderInfo: TvRenderInfo; ACanvas: TFPCustomCanvas; AEntity: TvEntity);
  protected
    FOwner: TvVectorialDocument;
    FUseTopLeftCoordinates: Boolean;
  public
    // Document size for page-based documents
    Width, Height: Double; // in millimeters, may be 0 to use TvVectorialDocument defaults
    // Document size for other documents
    MinX, MinY, MinZ, MaxX, MaxY, MaxZ: Double;
    // Other basic document information
    BackgroundColor: TFPColor;
    AdjustPenColorToBackground: Boolean;
    RenderInfo: TvRenderInfo; // Prepared by the reader with info on how to draw the page
    { Base methods }
    constructor Create(AOwner: TvVectorialDocument); virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TvPage); virtual;
    procedure SetPageFormat(AFormat: TvPageFormat);
    { Data reading methods }
    procedure CalculateDocumentSize; virtual;
    function  GetEntity(ANum: Cardinal): TvEntity; virtual; abstract;
    function  GetEntitiesCount: Integer; virtual; abstract;
    function  GetLastEntity(): TvEntity; virtual; abstract;
    function  GetEntityIndex(AEntity : TvEntity) : Integer; virtual; abstract;
    function  FindAndSelectEntity(Pos: TPoint): TvFindEntityResult; virtual; abstract;
    function  FindEntityWithNameAndType(AName: string; AType: TvEntityClass {= TvEntity}; ARecursively: Boolean = False): TvEntity; virtual; abstract;
    { Data removing methods }
    procedure Clear; virtual; abstract;
    function  DeleteEntity(AIndex: Cardinal): Boolean; virtual; abstract;
    function  RemoveEntity(AEntity: TvEntity; AFreeAfterRemove: Boolean = True): Boolean; virtual; abstract;
    { Data writing methods }
    function AddEntity(AEntity: TvEntity): Integer; virtual; abstract;
    { Drawing methods }
    procedure DrawBackground(ADest: TFPCustomCanvas); virtual; abstract;
    procedure RenderPageBorder(ADest: TFPCustomCanvas;
      ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); virtual; abstract;
    procedure Render(ADest: TFPCustomCanvas;
      ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0;
      ADoDraw: Boolean = true); virtual; abstract;
    procedure AutoFit(ADest: TFPCustomCanvas; AWidth, AHeight, ARenderHeight: Integer; out ADeltaX, ADeltaY: Integer; out AZoom: Double); virtual;
    procedure GetNaturalRenderPos(var APageHeight: Integer; out AMulY: Double); virtual; abstract;
    procedure SetNaturalRenderPos(AUseTopLeftCoords: Boolean); virtual;
    function HasNaturalRenderPos: Boolean;
    function GetTopLeftCoords_Adjustment(): Double;
    { Debug methods }
    procedure GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer); virtual; abstract;

    property UseTopLeftCoordinates: Boolean read FUseTopLeftCoordinates write FUseTopLeftCoordinates;
  end;

  { TvVectorialPage }

  TvVectorialPage = class(TvPage)
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
    Owner: TvVectorialDocument;
    { Base methods }
    constructor Create(AOwner: TvVectorialDocument); override;
    destructor Destroy; override;
    procedure Assign(ASource: TvPage); override;
    { Data reading methods }
    function  GetEntity(ANum: Cardinal): TvEntity; override;
    function  GetEntitiesCount: Integer; override;
    function  GetLastEntity(): TvEntity; override;
    function  GetEntityIndex(AEntity : TvEntity) : Integer; override;
    function  FindAndSelectEntity(Pos: TPoint): TvFindEntityResult; override;
    function  FindEntityWithNameAndType(AName: string; AType: TvEntityClass {= TvEntity}; ARecursively: Boolean = False): TvEntity; override;
    { Data removing methods }
    procedure Clear; override;
    function  DeleteEntity(AIndex: Cardinal): Boolean; override;
    function  RemoveEntity(AEntity: TvEntity; AFreeAfterRemove: Boolean = True): Boolean; override;
    { Data writing methods }
    function AddEntity(AEntity: TvEntity): Integer; override;
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
    procedure AddEllipticalArcWithCenterToPath(ARadX, ARadY, AXAxisRotation, ADestX, ADestY, ACenterX, ACenterY: Double; AClockwiseArcFlag: Boolean);
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
    procedure PositionEntitySubparts(constref ARenderInfo: TvRenderInfo; ABaseX, ABaseY: Double);
    procedure DrawBackground(ADest: TFPCustomCanvas); override;
    procedure RenderPageBorder(ADest: TFPCustomCanvas;
      ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
    procedure Render(ADest: TFPCustomCanvas; ADestX: Integer = 0; ADestY: Integer = 0;
      AMulX: Double = 1.0; AMulY: Double = 1.0; ADoDraw: Boolean = true); override;
    procedure GetNaturalRenderPos(var APageHeight: Integer; out AMulY: Double); override;
    { Debug methods }
    procedure GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer); override;
    //
    property Entities[AIndex: Cardinal]: TvEntity read GetEntity;
  end;

  { TvTextPageSequence }

  {@@ Represents a sequence of text pages up to a page break }

  TvTextPageSequence = class(TvPage)
  public
    Footer, Header: TvRichText;
    MainText: TvRichText;
    { Base methods }
    constructor Create(AOwner: TvVectorialDocument); override;
    destructor Destroy; override;
    procedure Assign(ASource: TvPage); override;
    { Data reading methods }
    function  GetEntity(ANum: Cardinal): TvEntity; override;
    function  GetEntitiesCount: Integer; override;
    function  GetLastEntity(): TvEntity; override;
    function  GetEntityIndex(AEntity : TvEntity) : Integer; override;
    function  FindAndSelectEntity(Pos: TPoint): TvFindEntityResult; override;
    function  FindEntityWithNameAndType(AName: string; AType: TvEntityClass {= TvEntity}; ARecursively: Boolean = False): TvEntity; override;
    { Data removing methods }
    procedure Clear; override;
    function  DeleteEntity(AIndex: Cardinal): Boolean; override;
    function  RemoveEntity(AEntity: TvEntity; AFreeAfterRemove: Boolean = True): Boolean; override;
    { Data writing methods }
    function AddEntity(AEntity: TvEntity): Integer; override;
    { Data writing methods }
    function AddParagraph: TvParagraph;
    function AddList: TvList;
    function AddTable: TvTable;
    function AddEmbeddedVectorialDoc: TvEmbeddedVectorialDoc;
    //function AddImage: TvImage;
    { Drawing methods }
    procedure DrawBackground(ADest: TFPCustomCanvas); override;
    procedure RenderPageBorder(ADest: TFPCustomCanvas;
      ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
    procedure Render(ADest: TFPCustomCanvas; ADestX: Integer = 0; ADestY: Integer = 0;
      AMulX: Double = 1.0; AMulY: Double = 1.0; ADoDraw: Boolean = true); override;
    procedure GetNaturalRenderPos(var APageHeight: Integer; out AMulY: Double); override;
    { Debug methods }
    procedure GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer); override;
  end;

  {@@ TvVectorialReader class reference type }

  TvVectorialReaderClass = class of TvCustomVectorialReader;

  { TvCustomVectorialReader }

  TvCustomVectorialReader = class
  protected
    FFilename: string;
    class function GetTextContentsFromNode(ANode: TDOMNode): DOMString;
    class function RemoveLineEndingsAndTrim(AStr: string): string;
  public
    Settings: TvVectorialReaderSettings;
    { General reading methods }
    constructor Create; virtual;
    procedure ReadFromFile(AFileName: string; AData: TvVectorialDocument); virtual;
    procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); virtual;
    procedure ReadFromStrings(AStrings: TStrings; AData: TvVectorialDocument); virtual;
    procedure ReadFromXML(ADoc: TXMLDocument; AData: TvVectorialDocument); virtual;
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

  TvRenderer = class
  public
    procedure BeginRender(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean); virtual; abstract;
    procedure EndRender(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean); virtual; abstract;
    // TPath
    procedure TPath_Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean; APath: TPath); virtual; abstract;
  end;
  TvRendererClass = class of TvRenderer;

var
  GvVectorialFormats: array of TvVectorialFormatData;

const
  FormulaOperators = [fekSubtraction, fekMultiplication, fekSum, fekFraction, fekRoot, fekPower];

procedure RegisterVectorialReader(
  AReaderClass: TvVectorialReaderClass;
  AFormat: TvVectorialFormat);
procedure RegisterVectorialWriter(
  AWriterClass: TvVectorialWriterClass;
  AFormat: TvVectorialFormat);

function Make2DPoint(AX, AY: Double): T3DPoint;
function Dimension(AValue : Double; AUnits : TvUnits) : TvDimension;
function ConvertDimensionToMM(ADimension: TvDimension; ATotalSize: Double): Double;
procedure RegisterDefaultRenderer(ARenderer: TvRendererClass);

implementation

uses fpvutils;

const
  Str_Error_Nil_Path = ' The program attempted to add a segment before creating a path';
  INVALID_RENDERINFO_CANVAS_XY = Low(Integer);
  Str_Line_Height_Tester = 'Áç';

{$ifdef FPVECTORIAL_AUTOFIT_DEBUG}
var
  AutoFitDebug: TStrings = nil;
{$endif}

var
  gDefaultRenderer: TvRendererClass = nil;

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
      //if GvVectorialFormats[i].ReaderRegistered then
       //raise Exception.Create('RegisterVectorialReader: Reader class for format ' {+ AFormat} + ' already registered.');

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

function Dimension(AValue: Double; AUnits: TvUnits): TvDimension;
begin
  Result.Value := AValue;
  Result.Units := AUnits;
end;

function ConvertDimensionToMM(ADimension: TvDimension; ATotalSize: Double): Double;
begin
  case ADimension.Units of
  dimMillimeter: Result := ADimension.Value;
  dimPercent:    Result := ATotalSize * ADimension.Value;
  dimPoint:      Result := ADimension.Value; // ToDo
  end;
end;

procedure RegisterDefaultRenderer(ARenderer: TvRendererClass);
begin
  gDefaultRenderer := ARenderer;
end;

{ TvStyle }

constructor TvStyle.Create;
begin
  // Defaults
  SuppressSpacingBetweenSameParagraphs:=False;
end;

function TvStyle.GetKind: TvStyleKind;
begin
  if Parent = nil then Result := Kind
  else Result := Parent.GetKind();
end;

procedure TvStyle.Clear;
begin
  Name := '';
  Parent := nil;
  Kind := vskTextBody;
  Alignment := vsaLeft;

  //
  {Pen.Color := col;
  Brush := nil;
  Font := nil;}
  SetElements := [];
  //
  MarginTop := 0;
  MarginBottom := 0;
  MarginLeft := 0;
  MarginRight := 0;
  //
end;

procedure TvStyle.CopyFrom(AFrom: TvStyle);
begin
  Clear();
  ApplyOver(AFrom);
end;

procedure TvStyle.CopyFromEntity(AEntity: TvEntity);
begin

end;

procedure TvStyle.ApplyOverFromPen(APen: PvPen; ASetElements: TvSetStyleElements);
begin
  if spbfPenColor in ASetElements then
    Pen.Color := APen^.Color;
  if spbfPenStyle in ASetElements then
    Pen.Style := APen^.Style;
  if spbfPenWidth in ASetElements then
    Pen.Width := APen^.Width;

  SetElements += ASetElements * [spbfPenColor, spbfPenStyle, spbfPenWidth];
end;

procedure TvStyle.ApplyOverFromBrush(ABrush: PvBrush; ASetElements: TvSetStyleElements);
begin
  if spbfBrushColor in ASetElements then
    Brush.Color := ABrush^.Color;
  if spbfBrushStyle in ASetElements then
    Brush.Style := ABrush^.Style;
  {if spbfBrushGradient in ASetElements then
    Brush.Gra := AFrom.Brush.Style;}
  if spbfBrushKind in ASetElements then
    Brush.Kind := ABrush^.Kind;

  SetElements += ASetElements * [spbfBrushColor, spbfBrushStyle, spbfBrushGradient, spbfBrushKind];
end;

procedure TvStyle.ApplyOverFromFont(AFont: PvFont; ASetElements: TvSetStyleElements);
begin

end;

procedure TvStyle.ApplyOver(AFrom: TvStyle);
begin
  if AFrom = nil then Exit;

  // Pen

  ApplyOverFromPen(@AFrom.Pen, AFrom.SetElements);

  // Brush

  ApplyOverFromBrush(@AFrom.Brush, AFrom.SetElements);

  // Font

  //ApplyOverFromFont(@AFrom.Font, AFrom.SetElements);
  if spbfFontColor in AFrom.SetElements then
    Font.Color := AFrom.Font.Color;
  if spbfFontSize in AFrom.SetElements then
    Font.Size := AFrom.Font.Size;
  if spbfFontName in AFrom.SetElements then
    Font.Name := AFrom.Font.Name;
  if spbfFontBold in AFrom.SetElements then
    Font.Bold := AFrom.Font.Bold;
  if spbfFontItalic in AFrom.SetElements then
    Font.Italic := AFrom.Font.Italic;
  If spbfFontUnderline in AFrom.SetElements then
    Font.Underline := AFrom.Font.Underline;
  If spbfFontStrikeThrough in AFrom.SetElements then
    Font.StrikeThrough := AFrom.Font.StrikeThrough;
  If spbfAlignment in AFrom.SetElements then
    Alignment := AFrom.Alignment;

  // TextAnchor
  if spbfTextAnchor in AFrom.SetElements then
    TextAnchor := AFrom.TextAnchor;

  // Style

  if sseMarginTop in AFrom.SetElements then
    MarginTop := AFrom.MarginTop;
  If sseMarginBottom in AFrom.SetElements then
    MarginBottom := AFrom.MarginBottom;
  If sseMarginLeft in AFrom.SetElements then
    MarginLeft := AFrom.MarginLeft;
  If sseMarginRight in AFrom.SetElements then
    MarginRight := AFrom.MarginRight;

  // Other
  SuppressSpacingBetweenSameParagraphs:=AFrom.SuppressSpacingBetweenSameParagraphs;

  SetElements := AFrom.SetElements + SetElements;
end;

procedure TvStyle.ApplyIntoEntity(ADest: TvEntity);
var
  lCurEntity: TvEntity;
  ADestWithPen: TvEntityWithPen absolute ADest;
  ADestWithBrush: TvEntityWithPenAndBrush absolute ADest;
  ADestWithFont: TvEntityWithPenBrushAndFont absolute ADest;
begin
  if ADest = nil then Exit;

  if ADest is TvEntityWithSubEntities then
  begin
    lCurEntity := (ADest as TvEntityWithSubEntities).GetFirstEntity();
    while lCurEntity <> nil do
    begin
      ApplyIntoEntity(lCurEntity);
      lCurEntity := (ADest as TvEntityWithSubEntities).GetNextEntity();
    end;
    Exit;
  end;

  // Pen
  if ADest is TvEntityWithPen then
  begin
    if spbfPenColor in SetElements then
      ADestWithPen.Pen.Color := Pen.Color;
    if spbfPenStyle in SetElements then
      ADestWithPen.Pen.Style := Pen.Style;
    if spbfPenWidth in SetElements then
      ADestWithPen.Pen.Width := Pen.Width;
  end;

  // Brush
  if ADest is TvEntityWithPenAndBrush then
  begin
    if spbfBrushColor in SetElements then
      ADestWithBrush.Brush.Color := Brush.Color;
    if spbfBrushStyle in SetElements then
      ADestWithBrush.Brush.Style := Brush.Style;
    {if spbfBrushGradient in SetElements then
      ADestWithBrush.Gra := AFrom.Brush.Style;}
    if spbfBrushKind in SetElements then
      ADestWithBrush.Brush.Kind := Brush.Kind;
  end;

  // Font
  if ADest is TvEntityWithPenBrushAndFont then
  begin
    if spbfFontColor in SetElements then
      ADestWithFont.Font.Color := Font.Color;
    if spbfFontSize in SetElements then
      ADestWithFont.Font.Size := Font.Size;
    if spbfFontName in SetElements then
      ADestWithFont.Font.Name := Font.Name;
    if spbfFontBold in SetElements then
      ADestWithFont.Font.Bold := Font.Bold;
    if spbfFontItalic in SetElements then
      ADestWithFont.Font.Italic := Font.Italic;
    If spbfFontUnderline in SetElements then
      ADestWithFont.Font.Underline := Font.Underline;
    If spbfFontStrikeThrough in SetElements then
      ADestWithFont.Font.StrikeThrough := Font.StrikeThrough;
    {If spbfAlignment in SetElements then
      ADestWithFont.Alignment := Alignment; }

    // TextAnchor
    if spbfTextAnchor in SetElements then
      ADestWithFont.TextAnchor := TextAnchor;
  end;
end;

function TvStyle.CreateStyleCombinedWithParent: TvStyle;
begin
  Result := TvStyle.Create;
  Result.CopyFrom(Self);
  if Parent <> nil then Result.ApplyOver(Parent);
end;

function TvStyle.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr, lParentName: string;
begin
  if Parent <> nil then lParentName := Parent.Name
  else lParentName := '<No Parent>';

  lStr := Format('[%s] Name=%s Parent=%s',
    [Self.ClassName, Name, lParentName]);

  if spbfPenColor in SetElements then
    lStr := lStr + Format(' Pen.Color=%s', [TvEntity.GenerateDebugStrForFPColor(Pen.Color)]);
{    spbfPenStyle, spbfPenWidth,
    spbfBrushColor, spbfBrushStyle, spbfBrushGradient,}
  if spbfFontColor in SetElements then
    lStr := lStr + Format(' Font.Color=%s', [TvEntity.GenerateDebugStrForFPColor(Pen.Color)]);
  if spbfFontSize in SetElements then
    lStr := lStr + Format(' Font.Size=%d', [Font.Size]);
  if spbfFontName in SetElements then
    lStr := lStr + ' Font.Name=' + Font.Name;
  if spbfFontBold in SetElements then
    if Font.Bold then lStr := lStr + Format(' Font.Bold=%s', [BoolToStr(Font.Bold)]);
  if spbfFontItalic in SetElements then
    if Font.Italic then lStr := lStr + Format(' Font.Bold=%s', [BoolToStr(Font.Italic)]);
{
    spbfFontUnderline, spbfFontStrikeThrough, spbfAlignment,
    // Page style
    sseMarginTop, sseMarginBottom, sseMarginLeft, sseMarginRight
    );
   Font.Size, Font.Name, Font.Orientation,
    BoolToStr(Font.Underline),
    BoolToStr(Font.StrikeThrough),
    GetEnumName(TypeInfo(TvTextAnchor), integer(TextAnchor))}
  lStr := lStr + FExtraDebugStr;
  Result := ADestRoutine(lStr, APageItem);
end;

{ TvListLevelStyle }

constructor TvListLevelStyle.Create;
begin
  Start := 1;
  Bullet := '&#183;';
  LeaderFontName := 'Symbol';
  Alignment := vsaLeft;
end;

{ TvListStyle }

constructor TvListStyle.Create;
begin
  ListLevelStyles:=TFPList.Create;
end;

destructor TvListStyle.Destroy;
begin
  Clear;
  ListLevelStyles.Free;
  ListLevelStyles := Nil;

  inherited Destroy;
end;

procedure TvListStyle.Clear;
var
  i: Integer;
begin
  for i := ListLevelStyles.Count-1 downto 0 do
  begin
    TvListLevelStyle(ListLevelStyles[i]).free;
    ListLevelStyles.Delete(i);
  end;
end;

function TvListStyle.AddListLevelStyle: TvListLevelStyle;
begin
  Result := TvListLevelStyle.Create;
  ListLevelStyles.Add(Result);
end;

function TvListStyle.GetListLevelStyleCount: Integer;
begin
  Result := ListLevelStyles.Count;
end;

function TvListStyle.GetListLevelStyle(AIndex : Integer): TvListLevelStyle;
begin
  Result := TvListLevelStyle(ListLevelStyles[Aindex]);
end;

{ TvTableCell }

constructor TvTableCell.Create(APage: TvPage);
begin
  inherited Create(APage);

  Borders.Left.LineType:=tbtDefault;
  Borders.Right.LineType:=tbtDefault;
  Borders.Top.LineType:=tbtDefault;
  Borders.Bottom.LineType:=tbtDefault;
  Borders.InsideHoriz.LineType:=tbtDefault;
  Borders.InsideVert.LineType:=tbtDefault;

  SpacingLeft := 2;
  SpacingRight := 2;
  SpacingTop := 2;
  SpacingBottom := 2;

  SpannedCols := 1;
end;

function TvTableCell.GetEffectiveBorder(): TvTableBorders;
begin
  Result := Borders;
  if (Row <> nil) and (Row.Table <> nil) then
  begin
    if Borders.Left.LineType = tbtDefault then
      Result.Left := Row.Table.Borders.Left;
    if Borders.Right.LineType = tbtDefault then
      Result.Right := Row.Table.Borders.Right;
    if Borders.Top.LineType = tbtDefault then
      Result.Top := Row.Table.Borders.Top;
    if Borders.Bottom.LineType = tbtDefault then
      Result.Bottom := Row.Table.Borders.Bottom;
  end;
end;

procedure TvTableCell.GetEffectiveCellSpacing(out ATopSpacing, ALeftSpacing, ARightSpacing, ABottomSpacing: Double);
begin
  ATopSpacing := 0;
  ALeftSpacing := 0;
  ARightSpacing := 0;
  ABottomSpacing := 0;
  if SpacingDataValid then
  begin
    ATopSpacing := SpacingTop;
    ALeftSpacing := SpacingLeft;
    ARightSpacing := SpacingRight;
    ABottomSpacing := SpacingBottom;
  end
  else if (Row <> nil) and (Row.SpacingDataValid) then
  begin
    ATopSpacing := Row.CellSpacing;
    ALeftSpacing := Row.CellSpacing;
    ARightSpacing := Row.CellSpacing;
    ABottomSpacing := Row.CellSpacing;
  end
  else if (Row <> nil) and (Row.Table <> nil) then
  begin
    ATopSpacing := Row.Table.CellSpacingLeft;
    ALeftSpacing := Row.Table.CellSpacingTop;
    ARightSpacing := Row.Table.CellSpacingRight;
    ABottomSpacing := Row.Table.CellSpacingBottom;
  end;
end;

class procedure TvTableCell.DrawBorder(ABorder: TvTableBorders;
  AX, AY, AWidth, AHeight: double;
  ADest: TFPCustomCanvas; var ARenderInfo: TvRenderInfo; ADestX: Integer = 0;
  ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

begin
  CalcEntityCanvasMinMaxXY(ARenderInfo, CoordToCanvasX(AX), CoordToCanvasY(AY));
  CalcEntityCanvasMinMaxXY(ARenderInfo, CoordToCanvasX(AX), CoordToCanvasY(AY+AHeight));
  ADest.Pen.Style := psSolid;
  ADest.Pen.FPColor := colBlack;
  if ABorder.Left.LineType <> tbtNone then
  begin
    ADest.Pen.Width := Round(ABorder.Left.Width * AMulX);
    ADest.Line(
      CoordToCanvasX(AX),
      CoordToCanvasY(AY),
      CoordToCanvasX(AX),
      CoordToCanvasY(AY+AHeight));
  end;
  if ABorder.Right.LineType <> tbtNone then
  begin
    ADest.Pen.Width := Round(ABorder.Right.Width * AMulX);
    ADest.Line(
      CoordToCanvasX(AX+AWidth),
      CoordToCanvasY(AY),
      CoordToCanvasX(AX+AWidth),
      CoordToCanvasY(AY+AHeight));
  end;
  if ABorder.Top.LineType <> tbtNone then
  begin
    ADest.Pen.Width := Round(ABorder.Top.Width * AMulX);
    ADest.Line(
      CoordToCanvasX(AX),
      CoordToCanvasY(AY),
      CoordToCanvasX(AX+AWidth),
      CoordToCanvasY(AY));
  end;
  if ABorder.Bottom.LineType <> tbtNone then
  begin
    ADest.Pen.Width := Round(ABorder.Bottom.Width * AMulX);
    ADest.Line(
      CoordToCanvasX(AX),
      CoordToCanvasY(AY+AHeight),
      CoordToCanvasX(AX+AWidth),
      CoordToCanvasY(AY+AHeight));
  end;
end;

procedure TvTableCell.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;
  //
  lBorders: TvTableBorders;
  CellWidth, CellHeight, lCellSpacingX, lCellSpacingY, lTmp: Double;
  lColNr: Integer;
  i: Integer;
begin
  // draw borders
  if (Row <> nil) and (Row.Table <> nil) and ADoDraw then
  begin
    lBorders := GetEffectiveBorder();
    lColNr := Row.GetCellColNr(Self);

    CellWidth := 0;
    for i := lColNr to lColNr+SpannedCols-1 do
    begin
      CellWidth := CellWidth + Row.Table.ColWidthsInMM[i];
    end;
    CellHeight := Row.Height;

    TvTableCell.DrawBorder(lBorders, X, Y, CellWidth, CellHeight,
      ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
  end;

  GetEffectiveCellSpacing(lCellSpacingX, lCellSpacingY, lTmp, lTmp);
  X := X + lCellSpacingX;
  Y := Y + lCellSpacingY;
  inherited Render(ARenderInfo, ADoDraw);
  X := X - lCellSpacingX;
  Y := Y - lCellSpacingY;
end;

function TvTableCell.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer;
begin
  FExtraDebugStr := Format(' Borders=%s PreferredWidth=%f VerticalAlignment=%s' +
    ' BackgroundColor=%s SpannedCols=%d',
    [GenerateDebugStrForBorders(Borders),
     PreferredWidth.Value,
     GetEnumName(TypeInfo(TvVerticalAlignment), integer(VerticalAlignment)),
     GenerateDebugStrForFPColor(BackgroundColor), SpannedCols]);

  Result:=inherited GenerateDebugTree(ADestRoutine, APageItem);
end;

class function TvTableCell.GenerateDebugStrForBorders(ABorders: TvTableBorders): string;
begin
  Result := Format('L=%s:%f T=%s:%f R=%s:%f B=%s:%f',
    [GetEnumName(TypeInfo(TvTableBorderType), integer(ABorders.Left.LineType)),
     ABorders.Left.Width,
     GetEnumName(TypeInfo(TvTableBorderType), integer(ABorders.Top.LineType)),
     ABorders.Top.Width,
     GetEnumName(TypeInfo(TvTableBorderType), integer(ABorders.Right.LineType)),
     ABorders.Right.Width,
     GetEnumName(TypeInfo(TvTableBorderType), integer(ABorders.Bottom.LineType)),
     ABorders.Bottom.Width]);
end;

{ TvTable }

// Returns the table width
procedure TvTable.CalculateColWidths(constref ARenderInfo: TvRenderInfo);
var
  CurRow: TvTableRow;
  CurCell: TvTableCell;
  lWidth: Double;
  col, row, i: Integer;
  //DebugStr: string;
  OriginalColWidthsInMM: array of Double;
  CurRowTableWidth: Double;
begin
  SetLength(ColWidthsInMM, GetColCount());

  // Process predefined widths
  for col := 0 to Length(ColWidthsInMM)-1 do
  begin
    ColWidthsInMM[col] := 0;
    if Length(ColWidths) > col then
      ColWidthsInMM[col] := ConvertDimensionToMM(Dimension(ColWidths[col], ColWidthsUnits), FPage.Width);
  end;

  // Process initial value for non-predefined widths
  OriginalColWidthsInMM := Copy(ColWidthsInMM, 0, Length(ColWidthsInMM));
  TableWidth := 0;
  for row := 0 to GetRowCount()-1 do
  begin
    CurRow := GetRow(row);
    CurRowTableWidth := 0;

    for col := 0 to CurRow.GetCellCount()-1 do
    begin
      CurCell := CurRow.GetCell(col);
      //DebugStr := ((CurCell.GetFirstEntity() as TvParagraph).GetFirstEntity() as TvText).Value.Text;

      // skip cells with span since they are complex
      // skip columns with width pre-set
      if (OriginalColWidthsInMM[col] > 0) then
      begin
        CurRowTableWidth := CurRowTableWidth + ColWidthsInMM[col];
        Continue;
      end;
      if (CurCell.SpannedCols > 1) then
      begin
        CurRowTableWidth := CurRowTableWidth + ColWidthsInMM[col];
        for i := 0 to CurCell.SpannedCols-1 do
          CurRowTableWidth := CurRowTableWidth + ColWidthsInMM[col+i];
        Continue;
      end;

      lWidth := CurCell.CalculateMaxNeededWidth(ARenderInfo);
      ColWidthsInMM[col] := Max(ColWidthsInMM[col], lWidth);
      CurRowTableWidth := CurRowTableWidth + ColWidthsInMM[col];
    end;

    TableWidth := Max(TableWidth, CurRowTableWidth);
  end;

  // If it goes over the page width, recalculate with equal sizes (in the future do better)
  if FPage.Width <= 0 then Exit;
  if TableWidth <= FPage.Width then Exit;
  TableWidth := FPage.Width;
  for col := 0 to Length(ColWidthsInMM)-1 do
  begin
    ColWidthsInMM[col] := FPage.Width / GetRowCount();
  end;
end;

procedure TvTable.CalculateRowHeights(constref ARenderInfo: TvRenderInfo);
var
  col, row: Integer;
  CurRow: TvTableRow;
  CurCell: TvTableCell;
  lCellHeight: Double;
begin
  TableHeight := 0;

  for row := 0 to GetRowCount()-1 do
  begin
    CurRow := GetRow(row);
    CurRow.Height := 0;

    for col := 0 to CurRow.GetCellCount()-1 do
    begin
      CurCell := CurRow.GetCell(col);
      lCellHeight := CurCell.CalculateCellHeight_ForWidth(ARenderInfo, ColWidthsInMM[col]);
      CurRow.Height := Max(CurRow.Height, lCellHeight);
    end;

    CurRow.Height := CurRow.Height + CurRow.CalculateMaxCellSpacing_Y();
    TableHeight := TableHeight + SpacingBetweenCells;
    CurRow.Y := TableHeight;
    TableHeight := TableHeight + CurRow.Height;
  end;

  TableHeight := TableHeight + SpacingBetweenCells;
end;

constructor TvTable.create(APage: TvPage);
begin
  inherited Create(APage);
  Rows := TFPList.Create;

  // Use default cell border widths of 0.5 pts, like Word or Writer.
  Borders.Left.Width := 0.5 * FPV_TEXT_POINT_TO_MM;
  Borders.Right.Width := 0.5 * FPV_TEXT_POINT_TO_MM;
  Borders.Top.Width := 0.5 * FPV_TEXT_POINT_TO_MM;
  Borders.Bottom.Width := 0.5 * FPV_TEXT_POINT_TO_MM;
  Borders.InsideHoriz.Width := 0.5 * FPV_TEXT_POINT_TO_MM;
  Borders.InsideVert.Width := 0.5 * FPV_TEXT_POINT_TO_MM;
end;

destructor TvTable.destroy;
var
  i: Integer;
begin
  for i := Rows.Count-1 downto 0 do
  begin
    TvTableRow(Rows.Last).Free;
    Rows.Delete(Rows.Count-1);
  end;

  Rows.Free;
  Rows := nil;

  inherited destroy;
end;

function TvTable.AddRow: TvTableRow;
begin
  Result := TvTableRow.create(FPage);
  Result.Table := Self;
  Rows.Add(result);
end;

function TvTable.GetRowCount: Integer;
begin
  Result := Rows.Count;
end;

function TvTable.GetRow(AIndex: Integer): TvTableRow;
begin
  Result := TvTableRow(Rows[AIndex]);
end;

function TvTable.GetColCount(): Integer;
var
  row, col, CurRowColCount: Integer;
  CurRow: TvTableRow;
  CurCell: TvTableCell;
begin
  Result := 0;
  for row := 0 to GetRowCount()-1 do
  begin
    CurRow := GetRow(row);
    CurRowColCount := 0;
    for col := 0 to CurRow.GetCellCount()-1 do
    begin
      CurCell := CurRow.GetCell(col);
      CurRowColCount := CurRowColCount + CurCell.SpannedCols;
    end;

    Result := Max(Result, CurRowColCount);
  end;
end;

procedure TvTable.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

  function DeltaToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(AmulY * ACoord);
  end;

var
  row: Integer;
  CurRow: TvTableRow;
  lEntityRenderInfo: TvRenderInfo;
begin
  InitializeRenderInfo(ARenderInfo, Self);

  // First calculate the column widths and heights
  CalculateColWidths(ARenderInfo);

  // Now calculate the row heights
  CalculateRowHeights(ARenderInfo);

  // Draw the table border
  if ADoDraw then
  begin
    TvTableCell.DrawBorder(Borders, X, Y, TableWidth, TableHeight,
      ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
  end;

  // Now draw the table
  for row := 0 to GetRowCount()-1 do
  begin
    CurRow := GetRow(row);

    // changes from pos relative inside table (calculated in CalculateRowHeights) to absolute pos
    CurRow.Y := Y + CurRow.Y;

    CopyAndInitDocumentRenderInfo(lEntityRenderInfo, ARenderInfo);
    CurRow.Render(lEntityRenderInfo, ADoDraw);
    //MergeRenderInfo(lEntityRenderInfo, ARenderInfo); no need to merge, since TvTableCell.DrawBorder calculates the proper size
  end;
end;

function TvTable.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  i: Integer;
  lCurRow: TvTableRow;
begin
  Result := inherited GenerateDebugTree(ADestRoutine, APageItem);

  // data which goes into a separate item
  FExtraDebugStr := 'ColWidthsInMM=';
  for i := 0 to Length(ColWidthsInMM)-1 do
    FExtraDebugStr := FExtraDebugStr + Format('[%d]=%f ', [i, ColWidthsInMM[i]]);
  ADestRoutine(FExtraDebugStr, Result);

  // Add rows
  for i := 0 to GetRowCount()-1 do
  begin
    lCurRow := GetRow(i);
    lCurRow.GenerateDebugTree(ADestRoutine, Result);
  end;
end;

{ TvEmbeddedVectorialDoc }

constructor TvEmbeddedVectorialDoc.create(APage: TvPage);
begin
  inherited create(APage);
  Document := TvVectorialDocument.Create();
  FWidth := -1;
  FHeight := -1;
end;

destructor TvEmbeddedVectorialDoc.destroy;
begin
  Document.Free;
  inherited destroy;
end;

procedure TvEmbeddedVectorialDoc.UpdateDocumentSize;
begin
  if (Document.Width = 0) or (Document.Height = 0) then
  begin
    Document.GuessDocumentSize();
  end;
end;

function TvEmbeddedVectorialDoc.GetWidth: Double;
begin
  if FWidth >= 0 then
    Result := FWidth
  else
    Result := Document.Width;
end;

function TvEmbeddedVectorialDoc.GetHeight: Double;
begin
  if FHeight >= 0 then
    Result := FHeight
  else
    Result := Document.Height;
end;

procedure TvEmbeddedVectorialDoc.SetWidth(AValue: Double);
begin
  FWidth := AValue;
end;

procedure TvEmbeddedVectorialDoc.SetHeight(AValue: Double);
begin
  FHeight := AValue;
end;

procedure TvEmbeddedVectorialDoc.CalculateBoundingBox(constref ARenderInfo: TvRenderInfo;
  out ALeft, ATop, ARight, ABottom: Double);
begin
  UpdateDocumentSize();
  ALeft := X;
  ATop := Y;
  ARight := X + GetWidth();
  ABottom := Y + GetHeight();
end;

procedure TvEmbeddedVectorialDoc.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  lPage: TvPage;
  lX_px, lY_px, lWidth_px, lHeight_px, lPageHeight, lDeltaX, lDeltaY: Integer;
  lMulY, lZoom: Double;
begin
  inherited Render(ARenderInfo, ADoDraw);

  if Document.GetPageCount() = 0 then Exit;

  lPage := Document.GetPage(0);
  lPageHeight := Round(lPage.Height);
  lPage.GetNaturalRenderPos(lPageHeight, lMulY);

  UpdateDocumentSize();
  lX_px := CoordToCanvasX(X);
  lY_px := CoordToCanvasY(Y);

  // Ignore MulX/MulY here so that it doesn't affect AutoFit, this fixes an
  // issue where embeded svg in html was getting out of proportion if the zoom
  // was different than 1.0
  // Calculate the standard zoom (zoom with mulx=1.0)
  lWidth_px := Round(GetWidth());
  lHeight_px := Round(GetHeight());
  lPage.AutoFit(ADest, lWidth_px, lHeight_px, lHeight_px, lDeltaX, lDeltaY, lZoom);
  lZoom := Abs(lZoom);
  lX_px += lDeltaX;
  lY_px += lDeltaY;
  if AmulY * lMulY < 0 then
  begin
    lY_px := lY_px + lHeight_px;
  end;
  // recalculate lWidth_px/height considering now mulx/muly
  lWidth_px := Abs(CoordToCanvasX(GetWidth()));
  lHeight_px := Abs(CoordToCanvasY(GetHeight()));

  if ADoDraw then
  begin
    lPage.Render(ADest, lX_px, lY_px, AMulX * lZoom, AMulY * lMulY * lZoom);
    {ADest.Pen.FPColor := colRed;
    ADest.Pen.Style := psSolid;
    ADest.Rectangle(CoordToCanvasX(X), CoordToCanvasY(lY), CoordToCanvasX(X+Width), CoordToCanvasY(lY+Height));
    ADest.Rectangle(lX_px, lY_px, lX_px+100, lY_px+100);}
  end;

  if (ARenderInfo.Errors <> nil) and (lPage.RenderInfo.Errors <> nil) then
    ARenderInfo.Errors.AddStrings(lPage.RenderInfo.Errors);
  CalcEntityCanvasMinMaxXY(ARenderInfo, CoordToCanvasX(X), CoordToCanvasY(Y));
  CalcEntityCanvasMinMaxXY(ARenderInfo,
    CoordToCanvasX(X + Document.Width),
    CoordToCanvasY(Y + Document.Height));
  CalcEntityCanvasMinMaxXY(ARenderInfo, lX_px, lY_px);
  CalcEntityCanvasMinMaxXY(ARenderInfo, lX_px+lWidth_px, lY_px+lHeight_px);
end;

function TvEmbeddedVectorialDoc.GenerateDebugTree(
  ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer;
begin
  Result:=inherited GenerateDebugTree(ADestRoutine, APageItem);
  Document.GenerateDebugTree(ADestRoutine, Result);
end;

{ TvTableRow }

constructor TvTableRow.create(APage: TvPage);
begin
  inherited create(APage);

  Cells := TFPList.Create;

  Header := False;
end;

destructor TvTableRow.destroy;
Var
  i : Integer;
begin
  for i := Cells.Count-1 downto 0 do
  begin
    TvTableCell(Cells.Last).Free;
    Cells.Delete(Cells.Count-1);
  end;

  Cells.Free;
  Cells := Nil;

  inherited destroy;
end;

function TvTableRow.AddCell : TvTableCell;
begin
  Result := TvTableCell.Create(FPage);
  Result.Row := Self;
  Cells.Add(Result);
end;

function TvTableRow.GetCellCount: Integer;
begin
  Result := Cells.Count;
end;

function TvTableRow.GetCell(AIndex: Integer): TvTableCell;
begin
  Result := TvTableCell(Cells[AIndex]);
end;

function TvTableRow.GetCellColNr(ACell: TvTableCell): Integer;
begin
  Result := Cells.IndexOf(Pointer(ACell));
end;

function TvTableRow.CalculateMaxCellSpacing_Y(): Double;
Var
  i : Integer;
  CurCell: TvTableCell;
  lTopSpacing, lLeftSpacing, lRightSpacing, lBottomSpacing: Double;
begin
  Result := 0;
  for i := 0 to GetCellCount()-1 do
  begin
    CurCell := GetCell(i);
    CurCell.GetEffectiveCellSpacing(lTopSpacing, lLeftSpacing, lRightSpacing, lBottomSpacing);
    Result := Max(Result, lBottomSpacing+lTopSpacing);
  end;
end;

procedure TvTableRow.CalculateBoundingBox(constref ARenderInfo: TvRenderInfo;
  out ALeft, ATop, ARight, ABottom: Double);
begin
  ALeft := X;
  ATop := Y;
  ARight := X + FPage.Width;
  ABottom := Y + Height;
end;

procedure TvTableRow.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  CurCell: TvTableCell;
  i: Integer;
  CurX_mm: Double = 0.0;
  lEntityRenderInfo: TvRenderInfo;
begin
  InitializeRenderInfo(ARenderInfo, Self);

  for i := 0 to GetCellCount()-1 do
  begin
    CurCell := GetCell(i);
    CurCell.X := CurX_mm;
    CurCell.Y := Y;
    //ADest.Line(CoordToCanvasX(CurX_mm), CoordToCanvasY(Y), CoordToCanvasX(CurX_mm+1), CoordToCanvasY(Y+1));
    CopyAndInitDocumentRenderInfo(lEntityRenderInfo, ARenderInfo);
    CurCell.Render(lEntityRenderInfo, ADoDraw);
    if (Table <> nil) then
    begin
      if (Length(Table.ColWidthsInMM) > i) then
        CurX_mm := CurX_mm + Table.ColWidthsInMM[i];
    end;

    MergeRenderInfo(lEntityRenderInfo, ARenderInfo);
  end;
end;

function TvTableRow.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  i: Integer;
  lCurCell: TvTableCell;
begin
  FExtraDebugStr := Format(' Height=%f CellSpacing=%f SpacingDataValid=%s',
    [Height, CellSpacing, BoolToStr(SpacingDataValid)]);
  Result := inherited GenerateDebugTree(ADestRoutine, APageItem);

  // Add cells
  for i := 0 to GetCellCount()-1 do
  begin
    lCurCell := GetCell(i);
    lCurCell.GenerateDebugTree(ADestRoutine, Result);
  end;
end;

{ T2DEllipticalArcSegment }

// wp: no longer needed...
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

procedure T2DEllipticalArcSegment.BezierApproximate(var Points: T3dPointsArray);
var
  P1, P2, P3, P4: T3dPoint;
  startangle, endangle: Double;
  startanglePi2, endanglePi2: Double;
  xstart, ystart: Double;
  nextx, nexty: Double;
  angle: Double;
  n: Integer;
begin
  SetLength(Points, 30);
  n := 0;

  xstart := T2DSegment(Previous).X;
  ystart := T2DSegment(Previous).Y;
  startangle := CalcEllipsePointAngle(xstart, ystart, RX, RY, CX, CY, XRotation);
  endangle := CalcEllipsePointAngle(X, Y, RX, RY, CX, CY, XRotation);
  if endangle < 0 then endangle := 2*pi + endangle;

  angle := arctan2(-1,1);
  angle := radtodeg(angle);

  angle := radtodeg(startangle);
  angle := radtodeg(endangle);

  // Since the algorithm for bezier approximation requires that the angle
  // between start and end is at most pi/3 we have to progress in pi/3 steps.
  angle := startangle + pi/3;
  while true do
  begin
    if angle >= endangle then begin
      EllipticalArcToBezier(CX, CY, RX, RY, startAngle, endangle, Points[n], Points[n+1], Points[n+2], Points[n+3]);
      inc(n, 4);
      break;
    end else
      EllipticalArcToBezier(CX, CY, RX, RY, startangle, angle, Points[n], Points[n+1], Points[n+2], Points[n+3]);
    inc(n, 4);
    startangle := angle;
    angle := angle + pi/2;
  end;
  SetLength(Points, n);
end;

procedure T2DEllipticalArcSegment.PolyApproximate(var Points: T3dPointsArray);
const
  BUFSIZE = 100;
var
  t, tstart, tend, dt: Double;
  xstart, ystart: Double;
  n: Integer;
  done: Boolean;
  clockwise: Boolean;
begin
  n := 0;
  SetLength(Points, BUFSIZE);

  dt := DegToRad(1.0);  // 1-degree increments

  xstart := T2DSegment(Previous).X;
  ystart := T2DSegment(Previous).Y;
  tstart := CalcEllipsePointAngle(xstart, ystart, RX, RY, CX, CY, XRotation);
  tend := CalcEllipsePointAngle(X, Y, RX, RY, CX, CY, XRotation);

  // Flip clockwise flag in case of top/left coordinates
  clockwise := ClockwiseArcFlag xor UseTopLeftCoordinates;

  if clockwise then
  begin
    // clockwise --> angle decreases --> tstart must be > tend
    dt := -dt;
    if tstart < tend then tstart := TWO_PI + tstart;
  end else
  begin
    // counter-clockwise --> angle increases --> tstart must be < tend
    if tend < tstart then tend := TWO_PI + tend;
  end;

  done := false;
  t := tstart;
  while not done do begin
    if (clockwise and (t < tend)) or         // angle decreases
       (not clockwise and (t > tend)) then   // angle increases
    begin
      t := tend;
      done := true;
    end;
    if n >= Length(Points) then
      SetLength(Points, Length(Points) + BUFSIZE);
    CalcEllipsePoint(t, RX, RY, CX, CY, XRotation, Points[n].x, Points[n].y);
    inc(n);
    t := t + dt;      // Note: dt is < 0 in clockwise case
  end;
  SetLength(Points, n);
end;

procedure T2DEllipticalArcSegment.Move(ADeltaX, ADeltaY: Double);
begin
  inherited Move(ADeltaX, ADeltaY);

  E1.X := E1.X + ADeltaX;
  E1.X := E1.Y + ADeltaY;

  E2.X := E2.X + ADeltaX;
  E2.X := E2.Y + ADeltaY;

  CX := CX + ADeltaX;
  CY := CY + ADeltaY;
end;


procedure T2DEllipticalArcSegment.Rotate(AAngle: Double; ABase: T3dPoint);
var
  p: T3DPoint;
begin
  inherited Rotate(AAngle, ABase);

  p := fpvutils.Rotate3DPointInXY(E1, ABase, AAngle);
  E1.X := p.X;
  E1.Y := p.Y;

  p := fpvutils.Rotate3DPointInXY(E2, ABase, AAngle);
  E2.X := p.X;
  E2.Y := p.Y;

  p := fpvutils.Rotate3DPointInXY(Make2dPoint(CX, CY), ABase, AAngle);
  CX := p.X;
  CY := p.Y;
end;

// wp: no longer needed...
procedure T2DEllipticalArcSegment.CalculateCenter;
var
  XStart, YStart, lT1: Double;
  CX1, CY1, CX2, CY2, LeftMostX, LeftMostY, RightMostX, RightMostY: Double;
  RotatedCenter: T3DPoint;
begin
  if CenterSetByUser then Exit;

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
  E1 := Rotate3DPointInXY(Make3DPoint(XStart,YStart), Make3DPoint(0,0),-1*XRotation);
  E2 := Rotate3DPointInXY(Make3DPoint(X,Y), Make3DPoint(0,0),-1*XRotation);

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

  lT1 := SolveNumericallyAngle(@AlignedEllipseCenterEquationT1, 0.0001, 20);

  CX1 := E1.X - RX*cos(lt1);
  CY1 := E1.Y - RY*sin(lt1);

  // Rotate back!
  RotatedCenter := Rotate3DPointInXY(Make3DPoint(CX1,CY1), Make3DPoint(0,0),XRotation);
  CX1 := RotatedCenter.X;
  CY1 := RotatedCenter.Y;

  // The other ellipse is symmetrically positioned
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

procedure T2DEllipticalArcSegment.CalculateEllipseBoundingBox(out ALeft, ATop, ARight, ABottom: Double);
var
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
    ATop := CY+RY;
    ABottom := CY-RY;
  end
  else
  begin
    // Search for the minimum and maximum X
    // There are two solutions in each 2pi range
    t1 := arctan(-RY*tan(XRotation)/RX);
    t2 := arctan(-RY*tan(XRotation)/RX) + pi; //Pi/2;    // why add pi/2 ??
//    t3 := arctan(-RY*tan(XRotation)/RX) + Pi;

    x1 := Cx + RX*Cos(t1)*Cos(XRotation)-RY*Sin(t1)*Sin(XRotation);
    x2 := Cx + RX*Cos(t2)*Cos(XRotation)-RY*Sin(t2)*Sin(XRotation);
//    x3 := Cx + RX*Cos(t3)*Cos(XRotation)-RY*Sin(t3)*Sin(XRotation);

    ALeft := Min(x1, x2);
  //  ALeft := Min(ALeft, x3);

    ARight := Max(x1, x2);
    //ARight := Max(ARight, x3);

    // Now the same for Y

    t1 := arctan(RY*cotan(XRotation)/RX);
    t2 := arctan(RY*cotan(XRotation)/RX) + pi; //Pi/2;   // why add pi/2 ??
//    t3 := arctan(RY*cotan(XRotation)/RX) + 3*Pi/2;

    y1 := CY + RY*Sin(t1)*Cos(XRotation)+RX*Cos(t1)*Sin(XRotation);
    y2 := CY + RY*Sin(t2)*Cos(XRotation)+RX*Cos(t2)*Sin(XRotation);
//    y3 := CY + RY*Sin(t3)*Cos(XRotation)+RX*Cos(t3)*Sin(XRotation);

    ATop := Max(y1, y2);
  //  ATop := Max(ATop, y3);

    ABottom := Min(y1, y2);
//    ABottom := Min(ABottom, y3);
    {
    ATop := Min(y1, y2);
    ATop := Min(ATop, y3);

    ABottom := Max(y1, y2);
    ABottom := Max(ABottom, y3);
    }
  end;
end;

function T2DEllipticalArcSegment.GenerateDebugTree(
  ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer;
var
  lStr: string;
  lStrLeftmostEllipse, lStrClockwiseArcFlag: string;
begin
  if LeftmostEllipse then lStrLeftmostEllipse := 'true'
  else lStrLeftmostEllipse := 'false';
  if ClockwiseArcFlag then lStrClockwiseArcFlag := 'true'
  else lStrClockwiseArcFlag := 'false';
  lStr := Format('[%s] X=%f Y=%f RX=%f RY=%f LeftmostEllipse=%s ClockwiseArcFlag=%s CX=%f CY=%f',
    [Self.ClassName, X, Y, RX, RY, lStrLeftmostEllipse, lStrClockwiseArcFlag, CX, CY]);
  Result := ADestRoutine(lStr, APageItem);
end;

procedure T2DEllipticalArcSegment.AddToPoints(ADestX, ADestY: Integer;
  AMulX, AMulY: Double; var Points: TPointsArray);
var
  pts3D: T3DPointsArray;
  i, n: Integer;
begin
  SetLength(pts3d, 0);
  PolyApproximate(pts3D);
  n := Length(Points);
  SetLength(Points, n + Length(pts3D) - 1);  // we don't need the start point --> -1
  for i:=0 to High(pts3D)-1 do    // i=0 is end point of prev segment -> we can skip it.
  begin
    Points[n].X := CoordToCanvasX(pts3D[i].X, ADestX, AMulX);
    Points[n].Y := CoordToCanvasY(pts3D[i].Y, ADestY, AMulY);
    inc(n);
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

procedure TvVerticalFormulaStack.PositionSubparts(constref ARenderInfo: TvRenderInfo;
  ABaseX, ABaseY: Double);
var
  lElement: TvFormulaElement;
  lPosX: Double = 0;
  lPosY: Double = 0;
begin
  CalculateHeight(ARenderInfo.Canvas);
  CalculateWidth(ARenderInfo.Canvas);
  Left := ABaseX;
  Top := ABaseY;

  // Then calculate the position of each element
  lElement := GetFirstElement();
  while lElement <> nil do
  begin
    lElement.Left := Left;
    lElement.Top := Top - lPosY;
    lPosY := lPosY + lElement.Height + SpacingBetweenElementsY;

    lElement.PositionSubparts(ARenderInfo, ABaseX, ABaseY);

    lElement := GetNextElement();
  end;
end;

{ TPathSegment }

function TPathSegment.GetLength: Double;
begin
  Result := 0;
end;

function TPathSegment.GetPointAndTangentForDistance(ADistance: Double; out AX, AY, ATangentAngle: Double): Boolean;
begin
  Result := False;
  AX := 0;
  AY := 0;
  ATangentAngle := 0;
end;

function TPathSegment.GetStartPoint(out APoint: T3DPoint): Boolean;
begin
  Result := False;
  if Previous = nil then Exit;
  if (Previous is T3DSegment) then
  begin
    Result := True;
    APoint.X := T3DSegment(Previous).X;
    APoint.Y := T3DSegment(Previous).Y;
    APoint.Z := T3DSegment(Previous).Z;
    Exit;
  end;
  if (Previous is T2DSegment) then
  begin
    Result := True;
    APoint.X := T2DSegment(Previous).X;
    APoint.Y := T2DSegment(Previous).Y;
    APoint.Z := 0;
    Exit;
  end;
end;

procedure TPathSegment.Move(ADeltaX, ADeltaY: Double);
begin

end;

procedure TPathSegment.Rotate(AAngle: Double; ABase: T3DPoint);
begin

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

procedure TPathSegment.AddToPoints(ADestX, ADestY: Integer; AMulX, AMulY: Double;
  var Points: TPointsArray);
begin
  // Override by descendants
end;

function TPathSegment.UseTopLeftCoordinates: Boolean;
begin
  Result := (FPath <> nil) and FPath.FPage.UseTopLeftCoordinates;
end;


{ T2DSegment }

function T2DSegment.GetLength: Double;
var
  lStartPoint: T3DPoint;
begin
  Result := 0;
  if not GetStartPoint(lStartPoint) then Exit;
  Result := sqrt(sqr(X - lStartPoint.X) + sqr(Y + lStartPoint.Y));
end;

function T2DSegment.GetPointAndTangentForDistance(ADistance: Double; out AX,
  AY, ATangentAngle: Double): Boolean;
var
  lStartPoint: T3DPoint;
begin
  Result:=inherited GetPointAndTangentForDistance(ADistance, AX, AY, ATangentAngle);
  if not GetStartPoint(lStartPoint) then Exit;
  Result := LineEquation_GetPointAndTangentForLength(lStartPoint, Make3DPoint(X, Y), ADistance, AX, AY, ATangentAngle);
end;

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
  lRes := fpvutils.Rotate3DPointInXY(Make3DPoint(X, Y), ABase, AAngle);
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

procedure T2DSegment.AddToPoints(ADestX, ADestY: Integer; AMulX, AMulY: Double;
  var Points: TPointsArray);
var
  n: Integer;
begin
  n := Length(Points);
  SetLength(Points, n + 1);
  Points[n].X := CoordToCanvasX(Points[n].X, ADestX, AMulX);
  Points[n].Y := CoordToCanvasY(Points[n].Y, ADestY, AMulY);
end;

{ T2DBezierSegment }

function T2DBezierSegment.GetLength: Double;
var
  lStartPoint: T3DPoint;
begin
  Result := 0;
  if not GetStartPoint(lStartPoint) then Exit;
  Result := BezierEquation_GetLength(lStartPoint, Make3DPoint(X2, Y2),
    Make3DPoint(X3, Y3), Make3DPoint(X, Y), 0);
end;

function T2DBezierSegment.GetPointAndTangentForDistance(ADistance: Double; out
  AX, AY, ATangentAngle: Double): Boolean;
var
  lStartPoint: T3DPoint;
begin
  Result:=inherited GetPointAndTangentForDistance(ADistance, AX, AY,
    ATangentAngle);
  if not GetStartPoint(lStartPoint) then Exit;
  Result := BezierEquation_GetPointAndTangentForLength(lStartPoint, Make3DPoint(X2, Y2),
    Make3DPoint(X3, Y3), Make3DPoint(X, Y), ADistance, AX, AY, ATangentAngle);
end;

procedure T2DBezierSegment.Move(ADeltaX, ADeltaY: Double);
begin
  inherited Move(ADeltaX, ADeltaY);
  X2 := X2 + ADeltaX;
  Y2 := Y2 + ADeltaY;
  X3 := X3 + ADeltaX;
  Y3 := Y3 + ADeltaY;
end;

procedure T2DBezierSegment.Rotate(AAngle: Double; ABase: T3dPoint);
var
  p: T3DPoint;
begin
  inherited Rotate(AAngle, ABase);

  p := fpvutils.Rotate3DPointInXY(Make3DPoint(X2, Y2), ABase, AAngle);
  X2 := p.X;
  Y2 := p.Y;

  p := fpvutils.Rotate3DPointInXY(Make3DPoint(X3, Y3), ABase, AAngle);
  X3 := p.X;
  Y3 := p.Y;
end;

function T2DBezierSegment.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr: string;
begin
  lStr := Format('[%s] X=%f Y=%f CX2=%f CY2=%f CX3=%f CY3=%f', [Self.ClassName, X, Y, X2, Y2, X3, Y3]);
  Result := ADestRoutine(lStr, APageItem);
end;

procedure T2DBezierSegment.AddToPoints(ADestX, ADestY: Integer; AMulX, AMulY: Double;
  var Points: TPointsArray);
var
  pts: TPointsArray;
  coordX, coordY, coord2X, coord2Y, coord3X, coord3Y, coord4X, coord4Y: Integer;
  i, n: Integer;
  prev: TPoint;
begin
  if not (Previous is T2DSegment) then
    raise Exception.Create('T2DBezierSegment must follow a T2DSegment.');

  coordX := CoordToCanvasX(T2DSegment(Previous).X, ADestX, AMulX);  // start pt
  coordY := CoordToCanvasY(T2DSegment(Previous).Y, ADestY, AMulY);
  coord4X := CoordToCanvasX(X, ADestX, AMulX);   // end pt
  coord4Y := CoordToCanvasY(Y, ADestY, AMulY);
  coord2X := CoordToCanvasX(X2, ADestX, AMulX);  // ctrl pt 1
  coord2Y := CoordToCanvasY(Y2, ADestY, AMulY);
  coord3X := CoordToCanvasX(X3, ADestX, AMulX);  // ctrl pt 2
  coord3Y := CoordToCanvasY(Y3, ADestY, AMulY);

  SetLength(pts, 0);
  AddBezierToPoints(
    Make3DPoint(coordX, coordY),
    Make3DPoint(coord2X, coord2Y),
    Make3DPoint(coord3X, coord3Y),
    Make3DPoint(coord4X, coord4Y),
    pts);

  if Length(pts) = 0 then
    exit;

  n := Length(Points);
  prev := Points[n-1];
  SetLength(Points, n + Length(pts));
  for i:=0 to High(pts) do
  begin
    if (pts[i].X = prev.X) and (pts[i].Y = prev.Y) then   // skip subsequent coincident points
      Continue;
    Points[n] := pts[i];
    prev := pts[i];
    inc(n);
  end;
  SetLength(Points, n);
end;

{ T3DSegment }

procedure T3DSegment.Move(ADeltaX, ADeltaY: Double);
begin
  X := X + ADeltaX;
  Y := Y + ADeltaY;
end;

{ This is preliminary... }
procedure T3DSegment.AddToPoints(ADestX, ADestY: Integer; AMulX, AMulY: Double;
  var Points: TPointsArray);
var
  n: Integer;
begin
  n := Length(Points);
  SetLength(Points, n + 1);
  Points[n].X := CoordToCanvasX(Points[n].X, ADestX, AMulX);
  Points[n].Y := CoordToCanvasY(Points[n].Y, ADestY, AMulY);
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

constructor TvEntity.Create(APage: TvPage);
begin
end;

procedure TvEntity.Clear;
begin
  X := 0.0;
  Y := 0.0;
  Z := 0.0;
end;

procedure TvEntity.SetPage(APage: TvPage);
begin

end;

procedure TvEntity.CalculateBoundingBox(constref ARenderInfo: TvRenderInfo;
  out ALeft, ATop, ARight, ABottom: Double);
begin
  ALeft := X;
  ATop := Y;
  ARight := X+1;
  ABottom := Y+1;
end;

// returns false if the element is invisible
function TvEntity.CalculateSizeInCanvas(constref ARenderInfo: TvRenderInfo;
  APageHeight: Integer; AZoom: Double;
  out ALeft, ATop, AWidth, AHeight: Integer): Boolean;
var
  lRenderInfo: TvRenderInfo;
  lMulY: Double;
begin
  Result := True;
  CopyAndInitDocumentRenderInfo(lRenderInfo, ARenderInfo);
  ARenderInfo.Page.GetNaturalRenderPos(APageHeight, lMulY);
  AZoom := Abs(AZoom);
  lRenderInfo.DestX := 0;
  lRenderInfo.DestY := APageHeight;
  lRenderInfo.MulX := AZoom;
  lRenderInfo.MulY := AZoom * lMulY;
  Render(lRenderInfo, False);
  ALeft := lRenderInfo.EntityCanvasMinXY.X;
  ATop := lRenderInfo.EntityCanvasMinXY.Y;
  AWidth := lRenderInfo.EntityCanvasMaxXY.X - lRenderInfo.EntityCanvasMinXY.X;
  AHeight := lRenderInfo.EntityCanvasMaxXY.Y - lRenderInfo.EntityCanvasMinXY.Y;
  if (lRenderInfo.EntityCanvasMinXY.X = INVALID_RENDERINFO_CANVAS_XY) or
     (lRenderInfo.EntityCanvasMinXY.Y = INVALID_RENDERINFO_CANVAS_XY) or
     (lRenderInfo.EntityCanvasMaxXY.Y = INVALID_RENDERINFO_CANVAS_XY) or
     (lRenderInfo.EntityCanvasMaxXY.Y = INVALID_RENDERINFO_CANVAS_XY) then
     Result := False;
end;

procedure TvEntity.CalculateHeightInCanvas(constref ARenderInfo: TvRenderInfo; out AHeight: Integer);
var
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;
  //
  lRenderInfo: TvRenderInfo;
begin
  lRenderInfo.Canvas := ARenderInfo.Canvas;
  lRenderInfo.DestX := 0;
  lRenderInfo.DestY := 0;
  lRenderInfo.MulX := AMulX;
  lRenderInfo.MulY := AMulY;
  Render(lRenderInfo, False);
  AHeight := lRenderInfo.EntityCanvasMaxXY.Y - lRenderInfo.EntityCanvasMinXY.Y;
end;

procedure TvEntity.ExpandBoundingBox(constref ARenderInfo: TvRenderInfo; var ALeft, ATop, ARight, ABottom: Double);
var
  lLeft, lTop, lRight, lBottom: Double;
begin
  CalculateBoundingBox(ARenderInfo, lLeft, lTop, lRight, lBottom);
  if lLeft < ALeft then ALeft := lLeft;
  if lTop < ATop then ATop := lTop;
  if lRight > ARight then ARight := lRight;
  if lBottom > ABottom then ABottom := lBottom;
end;

class procedure TvEntity.CalcEntityCanvasMinMaxXY(
  var ARenderInfo: TvRenderInfo; APointX, APointY: Integer);
begin
  if ARenderInfo.EntityCanvasMinXY.X = INVALID_RENDERINFO_CANVAS_XY then
    ARenderInfo.EntityCanvasMinXY.X := APointX
  else ARenderInfo.EntityCanvasMinXY.X := Min(ARenderInfo.EntityCanvasMinXY.X, APointX);
  if ARenderInfo.EntityCanvasMinXY.Y = INVALID_RENDERINFO_CANVAS_XY then
    ARenderInfo.EntityCanvasMinXY.Y := APointY
  else ARenderInfo.EntityCanvasMinXY.Y := Min(ARenderInfo.EntityCanvasMinXY.Y, APointY);
  if ARenderInfo.EntityCanvasMaxXY.X = INVALID_RENDERINFO_CANVAS_XY then
    ARenderInfo.EntityCanvasMaxXY.X := APointX
  else ARenderInfo.EntityCanvasMaxXY.X := Max(ARenderInfo.EntityCanvasMaxXY.X, APointX);
  if ARenderInfo.EntityCanvasMaxXY.Y = INVALID_RENDERINFO_CANVAS_XY then
    ARenderInfo.EntityCanvasMaxXY.Y := APointY
  else ARenderInfo.EntityCanvasMaxXY.Y := Max(ARenderInfo.EntityCanvasMaxXY.Y, APointY);
end;

class procedure TvEntity.CalcEntityCanvasMinMaxXY_With2Points(
  var ARenderInfo: TvRenderInfo; AX1, AY1, AX2, AY2: Integer);
begin
  CalcEntityCanvasMinMaxXY(ARenderInfo, AX1, AY1);
  CalcEntityCanvasMinMaxXY(ARenderInfo, AX2, AY2);
end;

procedure TvEntity.MergeRenderInfo(var AFrom, ATo: TvRenderInfo);
begin
  CalcEntityCanvasMinMaxXY(ATo, AFrom.EntityCanvasMinXY.X, AFrom.EntityCanvasMinXY.Y);
  CalcEntityCanvasMinMaxXY(ATo, AFrom.EntityCanvasMaxXY.X, AFrom.EntityCanvasMaxXY.Y);
end;

class procedure TvEntity.InitializeRenderInfo(var ARenderInfo: TvRenderInfo; ASelf: TvEntity; ACreateObjs: Boolean);
begin
  // Don't change these because otherwise we lose the value set by the page
  // See CopyAndInitDocumentRenderInfo
  // ARenderInfo.BackgroundColor := colBlack;
  // ARenderInfo.AdjustPenColorToBackground := True;
  // ARenderInfo.Selected := nil;
  // ATo.Parent := AFrom.Self;

  ARenderInfo.EntityCanvasMinXY := Point(INVALID_RENDERINFO_CANVAS_XY, INVALID_RENDERINFO_CANVAS_XY);
  ARenderInfo.EntityCanvasMaxXY := Point(INVALID_RENDERINFO_CANVAS_XY, INVALID_RENDERINFO_CANVAS_XY);
  ARenderInfo.ForceRenderBlock := False;

  ARenderInfo.SelfEntity := ASelf;
  if ACreateObjs then
    ARenderInfo.Errors := TStringList.Create;
end;

class procedure TvEntity.FinalizeRenderInfo(var ARenderInfo: TvRenderInfo);
begin
  if ARenderInfo.Errors <> nil then
    ARenderInfo.Errors.Free;
  ARenderInfo.Errors := nil;
end;

class procedure TvEntity.CopyAndInitDocumentRenderInfo(out ATo: TvRenderInfo;
  AFrom: TvRenderInfo; ACopyMinMax: Boolean = False; AAsChild: Boolean = True);
begin
  InitializeRenderInfo(ATo, nil);
  ATo.DestX := AFrom.DestX;
  ATo.DestY := AFrom.DestY;
  ATo.MulX := AFrom.MulX;
  ATo.MulY := AFrom.MulY;
  ATo.Page := AFrom.Page;
  ATo.Canvas := AFrom.Canvas;
  ATo.Renderer := AFrom.Renderer;
  ATo.AdjustPenColorToBackground := AFrom.AdjustPenColorToBackground;
  ATo.BackgroundColor := AFrom.BackgroundColor;
  ATo.Selected := AFrom.Selected;
  if AAsChild then
  begin
    ATo.Parent := AFrom.SelfEntity;
  end
  else
  begin
    ATo.SelfEntity := AFrom.SelfEntity;
    ATo.Parent := AFrom.Parent;
  end;
  ATo.Errors := AFrom.Errors;
  if ACopyMinMax then
  begin
    ATo.EntityCanvasMinXY := AFrom.EntityCanvasMinXY;
    ATo.EntityCanvasMaxXY := AFrom.EntityCanvasMaxXY;
  end;
end;

function TvEntity.RenderInfo_GenerateParentTree(constref ARenderInfo: TvRenderInfo): string;
var
  lCurEntity: TvEntity;
begin
  lCurEntity := Self;
  Result := '';
  while lCurEntity <> nil do
  begin
    if Result <> '' then
      Result := '->' + Result;
    Result := lCurEntity.ClassName + Result;
    if lCurEntity is TvNamedEntity then
      Result := TvNamedEntity(lCurEntity).Name + ':' + Result;
    lCurEntity := ARenderInfo.Parent;
  end;
end;

function TvEntity.CentralizeY_InHeight(constref ARenderInfo: TvRenderInfo; AHeight: Double): Double;
var
  lHeight: Double;
begin
  lHeight := GetHeight(ARenderInfo);
  Result := Y + Abs(AHeight - lHeight) / 2;
end;

function TvEntity.GetHeight(constref ARenderInfo: TvRenderInfo): Double;
var
  ALeft, ATop, ARight, ABottom: Double;
begin
  CalculateBoundingBox(ARenderInfo, ALeft, ATop, ARight, ABottom);
  Result := Abs(ATop - ABottom);
end;

function TvEntity.GetWidth(constref ARenderInfo: TvRenderInfo): Double;
var
  ALeft, ATop, ARight, ABottom: Double;
begin
  CalculateBoundingBox(ARenderInfo, ALeft, ATop, ARight, ABottom);
  Result := Abs(ALeft - ARight);
end;

function TvEntity.GetLineIntersectionPoints(ACoord: Double;
  ACoordIsX: Boolean): TDoubleDynArray;
begin
  SetLength(Result, 0);
end;

function TvEntity.TryToSelect(APos: TPoint; var ASubpart: Cardinal; ASnapFlexibility: Integer = 5): TvFindEntityResult;
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

procedure TvEntity.PositionSubparts(constref ARenderInfo: TvRenderInfo; ABaseX, ABaseY: Double);
begin

end;

procedure TvEntity.Scale(ADeltaScaleX, ADeltaScaleY: Double);
begin

end;

procedure TvEntity.Rotate(AAngle: Double; ABase: T3DPoint);
begin

end;

procedure TvEntity.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
begin
  InitializeRenderInfo(ARenderInfo, Self);
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

function TvEntity.GetEntityFeatures(constref ARenderInfo: TvRenderInfo): TvEntityFeatures;
begin
  Result.DrawsUpwards := False;
  Result.DrawsUpwardHeightAdjustment := 0;
  Result.FirstLineHeight := 0;
  Result.TotalHeight := 0;
end;

function TvEntity.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr: string;
begin
  lStr := Format('[%s] X=%f Y=%f', [Self.ClassName, X, Y]);
  Result := ADestRoutine(lStr, APageItem);
end;

class function TvEntity.GenerateDebugStrForFPColor(AColor: TFPColor): string;
begin
  Result := IntToHex(AColor.Red div $100, 2) + IntToHex(AColor.Green div $100, 2) + IntToHex(AColor.Blue div $100, 2) + IntToHex(AColor.Alpha div $100, 2);
end;

// modified c-style string quoting
class function TvEntity.GenerateDebugStrForString(AValue: string): string;
begin
  Result := AValue;
  Result := StringReplace(Result, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, #$7, '\a', [rfReplaceAll]);
  Result := StringReplace(Result, #$8, '\b', [rfReplaceAll]);
  Result := StringReplace(Result, #$C, '\f', [rfReplaceAll]);
  Result := StringReplace(Result, #$A, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #$D, '\r', [rfReplaceAll]);
  Result := StringReplace(Result, #$9, '\t', [rfReplaceAll]);
  Result := StringReplace(Result, #$B, '\v', [rfReplaceAll]);
end;

{ TvNamedEntity }

constructor TvNamedEntity.Create(APage: TvPage);
begin
  inherited Create(APage);
  FPage := APage;
end;

procedure TvNamedEntity.SetPage(APage: TvPage);
begin
  FPage := APage;
end;

function TvNamedEntity.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr: string;
begin
  lStr := Format('[%s] Name="%s" X=%f Y=%f' + FExtraDebugStr, [Self.ClassName, Name, X, Y]);
  Result := ADestRoutine(lStr, APageItem);
end;

{ TvEntityWithPen }

constructor TvEntityWithPen.Create(APage: TvPage);
begin
  inherited Create(APage);
  Pen.Style := psSolid;
  Pen.Color := colBlack;
  Pen.Width := 1;
end;

procedure TvEntityWithPen.ApplyPenToCanvas(constref ARenderInfo: TvRenderInfo);
begin
  ApplyPenToCanvas(ARenderInfo, Pen);
end;

procedure TvEntityWithPen.ApplyPenToCanvas(constref ARenderInfo: TvRenderInfo; APen: TvPen);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
begin
  if ADest = nil then
    exit;
  ADest.Pen.FPColor := AdjustColorToBackground(APen.Color, ARenderInfo);
  ADest.Pen.Width := Max(1, APen.Width);   // wp: why was here "1;//APen.Width;" ???
  ADest.Pen.Style := APen.Style;
  {$ifdef USE_LCL_CANVAS}
  if (APen.Style = psPattern) then
  begin
    TCanvas(ADest).Pen.SetPattern(APen.Pattern);
    if APen.Width = 1 then TCanvas(ADest).Pen.Cosmetic := false;
  end;
  {$endif}
end;

procedure TvEntityWithPen.AssignPen(APen: TvPen);
begin
  Pen.Style := APen.Style;
  Pen.Color := APen.Color;
  Pen.Width := APen.Width;
end;

function TvEntityWithPen.CreatePath: TPath;
begin
  Result := nil;
end;

procedure TvEntityWithPen.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
begin
  inherited Render(ARenderInfo, ADoDraw);
  ApplyPenToCanvas(ARenderInfo);
end;

{ TvEntityWithPenAndBrush }

constructor TvEntityWithPenAndBrush.Create(APage: TvPage);
begin
  inherited Create(APage);
  Brush.Style := bsClear;
  Brush.Color := colBlue;
end;

procedure TvEntityWithPenAndBrush.ApplyBrushToCanvas(constref ARenderInfo: TvRenderInfo);
begin
  ApplyBrushToCanvas(ARenderInfo, @Brush);
end;

procedure TvEntityWithPenAndBrush.ApplyBrushToCanvas(constref ARenderInfo: TvRenderInfo;
  ABrush: PvBrush);
begin
  if ARenderInfo.Canvas = nil then
    exit;
  ARenderInfo.Canvas.Brush.FPColor := ABrush^.Color;
  ARenderInfo.Canvas.Brush.Style := ABrush^.Style;
end;

procedure TvEntityWithPenAndBrush.AssignBrush(ABrush: PvBrush);
begin
  Brush := ABrush^;
end;

{ Calculates the canvas coordinates of the gradient vector (i.e. x,y of start
  and end of gradient.
  ARect is the bounding box of the shape in which the gradient will be painted.
  It must be in canvas coordinates (pixels).
  Note that the gradient vector need not be along the edges of this rectangle. }
procedure TvEntityWithPenAndBrush.CalcGradientVector(
  out AGradientStart, AGradientEnd: T2dPoint; const ARect: TRect;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
begin
  AGradientStart := Point2D(Brush.Gradient_start.X, Brush.Gradient_start.Y);
  AGradientEnd := Point2D(Brush.Gradient_end.X, Brush.Gradient_end.Y);
  if (gfRelToUserSpace in Brush.Gradient_flags) then
  begin
    if (gfRelStartX in Brush.Gradient_flags) then
      AGradientStart.X := AGradientStart.X * FPage.Width;
    if (gfRelStartY in Brush.Gradient_flags) then
      AGradientStart.Y := AGradientStart.Y * FPage.Height;
    if (gfRelEndX in Brush.Gradient_flags) then
      AGradientEnd.X := AGradientEnd.X * FPage.Width;
    if (gfRelEndY in Brush.Gradient_flags) then
      AGradientEnd.Y := AGradientEnd.Y * FPage.Height;
    AGradientStart.X := CoordToCanvasX(AGradientStart.X, ADestX, AMulX);
    AGradientStart.Y := CoordToCanvasY(AGradientStart.Y, ADestY, AMulY);
    AGradientEnd.X := CoordToCanvasX(AGradientEnd.X, ADestX, AMulX);
    AGradientEnd.Y := CoordToCanvasY(AGradientEnd.Y, ADestY, AMulY);
  end else
  begin
    if (gfRelStartX in Brush.Gradient_flags) then
      AGradientStart.X := ARect.Left + AGradientStart.X * (ARect.Right - ARect.Left)
    else
      AGradientStart.X := CoordToCanvasX(AGradientStart.X, ADestX, AMulX);
    if (gfRelStartY in Brush.Gradient_flags) then
      AGradientStart.Y := ARect.Top + AGradientStart.Y * (ARect.Bottom - ARect.Left)
    else
      AGradientStart.Y := CoordToCanvasY(AGradientStart.Y, ADestY, AMulY);
    if (gfRelEndX in Brush.Gradient_flags) then
      AGradientEnd.X := ARect.Left + AGradientEnd.X * (ARect.Right - ARect.Left) else
      AGradientEnd.X := CoordToCanvasX(AGradientEnd.X, ADestX, AMulX);
    if (gfRelEndY in Brush.Gradient_flags) then
      AGradientEnd.Y := ARect.Top + AGradientEnd.Y * (ARect.Bottom - ARect.Top) else
      AGradientEnd.Y := CoordToCanvasY(AGradientEnd.Y, ADestY, AMulY);
  end;
end;

{ Fills a polygon with the color of the current brush. The routine can handle
  non-contiguous polygons (holes!) correctly using the ScanLine algorithm and
  the even-odd rule
  http://www.tutorialspoint.com/computer_graphics/polygon_filling_algorithm.htm

  The array APoints must be in canvas units.

  NOTES:
  - The method only performs a solid fill, i.e. Brush.Style is ignored
  - The method modifies the current pen. }
procedure TvEntityWithPenAndBrush.DrawPolygon(var ARenderInfo: TvRenderInfo; const APoints: TPointsArray;
  const APolyStarts: TIntegerDynArray; ARect: TRect);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  //
  scanlineY, scanLineY1, scanLineY2: Integer;
  lPoints, pts: T2DPointsArray;
  j: Integer;
begin
  if ARect.Top < ARect.Bottom then
  begin
    scanLineY1 := ARect.Top;
    scanLineY2 := ARect.Bottom;
  end else
  begin
    scanLineY1 := ARect.Bottom;
    scanLineY2 := ARect.Top;
  end;

  // Prepare points as needed by the GetLinePolygonIntersectionPoints procedure
  SetLength(pts, Length(APoints));
  for j := 0 to High(APoints) do
    pts[j] := Point2D(APoints[j].X, APoints[j].Y);

  // Prepare parameters and polygon points
  ADest.Pen.Style := psSolid;
  ADest.Pen.Width := 1;
  ADest.Pen.FPColor := Brush.Color;

  // Fill polygon by drawing horizontal line segments
  scanlineY := scanlineY1;
  while (scanlineY <= scanlineY2) do begin
    // Find intersection points of horizontal scan line with polygon
    // with polygon
    lPoints := GetLinePolygonIntersectionPoints(scanlineY, pts, APolyStarts, false);
    if Length(lPoints) < 2 then begin
      inc(scanlineY);
      Continue;
    end;
    // Draw lines between intersection points, skip every second pair
    j := 0;
    while j < High(lPoints) do
    begin
      ADest.Line(round(lPoints[j].X), round(lPoints[j].Y), round(lPoints[j+1].X), round(lPoints[j+1].Y));
      inc(j, 2);
    end;
    // Proceed to next scan line
    inc(scanlineY);
  end;
end;

{ Paints the border around the shape. Ignores the brush.
  APoints must be in canvas units. }
procedure TvEntityWithPenAndBrush.DrawPolygonBorderOnly(
  var ARenderInfo: TvRenderInfo; const APoints: TPointsArray);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  //
  j: Integer;
begin
  if Pen.Style <> psClear then
  begin
    ApplyPenToCanvas(ARenderInfo);
    ADest.MoveTo(APoints[0].X, APoints[0].Y);
    for j:=1 to High(APoints) do
      ADest.LineTo(APoints[j].X, APoints[j].Y);
  end;
end;

{ Fills the entity with a linear gradient.
  Assumes that the boundary is already in canvas units and is specified by
  polygon APoints.
  NOTE: The method modifies the current pen. }
procedure TvEntityWithPenAndBrush.DrawPolygonBrushLinearGradient(
  var ARenderInfo: TvRenderInfo;
  const APoints: TPointsArray; const APolyStarts: TIntegerDynArray;
  ARect: TRect; AGradientStart, AGradientEnd: T2DPoint);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  //
  lPoints, pts: T2DPointsArray;
  i, j: Integer;
  pf: Double;          // fraction of path travelled along gradient vector
  px, py: Double;
  phi: Double;
  sinphi, cosphi: float;
  coord, coord1, coord2, dcoord: Double;
  coordIsX: Boolean;
  p1, p2: T2dPoint;
  gv: T2dPoint;        // gradient vector
  gvlen: Double;       // length of gradient vector
  gstart: Double;      // Gradient start point (1-dim)
  dir: Integer;
  lStr: String;
begin
  // Direction of gradient vector. The gradient vector begins at the first
  // color position and ends at the last color position specified in the
  // brush's Gradient_colors.
  gv := Point2D(AGradientEnd.X-AGradientStart.X, AGradientEnd.Y-AGradientStart.Y);
  gvlen := sqrt(sqr(gv.x) + sqr(gv.y));
  if gvlen = 0 then
    exit;

  // Find boundary points where the gradient starts and ends. The gradient is
  // always travered from 0% to 100% color fractions.
  p1 := Point2D(
    IfThen(AGradientEnd.x > AGradientStart.x, ARect.Left, ARect.Right),
    IfThen(AGradientEnd.Y > AGradientStart.y, ARect.Top, ARect.Bottom)
  );
  p2 := Point2D(
    IfThen(AGradientEnd.x > AGradientStart.x, ARect.Right, ARect.Left),
    IfThen(AGradientEnd.Y > AGradientStart.y, ARect.Bottom, ARect.Top)
  );

  // Prepare parameters and polygon points
  ADest.Pen.Style := psSolid;
  ADest.Pen.Width := 1;

  SetLength(pts, Length(APoints));
  case Brush.Kind of
    bkVerticalGradient:
      begin // Run vertically, horizontal lines have same color
        coord1 := p1.y;
        coord2 := p2.y;
        dcoord := IfThen(AGradientEnd.Y > AGradientStart.Y, 1.0, -1.0);
        gstart := coord1;
        dir := round(dcoord);
        for i := 0 to High(APoints) do
          pts[i] := Point2D(APoints[i].X, APoints[i].Y);
        coordIsX := false;
        gstart := coord1;
      end;
    bkHorizontalGradient:
      begin  // Run horizontally, vertical lines have same color
        coord1 := p1.x;
        coord2 := p2.x;
        dcoord := IfThen(AGradientEnd.X > AGradientStart.X, 1.0, -1.0);
        gstart := coord1;
        dir := round(dcoord);
        for i := 0 to High(APoints) do
          pts[i] := Point2D(APoints[i].X, APoints[i].Y);
        coordIsX := true;
      end;
    bkOtherLinearGradient:
      begin  // Run along gradient vector, lines perpendicular to gradient vector
        phi := arctan2(gv.y, gv.x);
        Sincos(phi, sinphi, cosphi);
        coordIsX := (abs(sinphi) <= sin(pi/4));
        if not coordIsX then begin
          phi := -(pi/2 - phi);
          Sincos(phi, sinphi, cosphi);
        end;
        // p1 is the boundary point around which the shape is rotated in order to
        // to get the gradient vector in horizontal or vertical direction for
        // easier finding of intersection points.
        // Projection of vector from GradientStart to p1 onto gradient vector
        coord1 := (((p1.x - AGradientStart.X)*gv.x) + (p1.y - AGradientStart.Y)*gv.y) / gvlen;
        // dto for p2.
        coord2 := (((p2.x - AGradientStart.X)*gv.x) + (p2.y - AGradientStart.Y)*gv.Y) / gvlen;
        // Steps for walking along the gradient vector. Note: too-wide steps
        // could result in painting gaps, but this is avoided by using a
        // 2-pixel wide pen below.
        dcoord := 1.0; // --- some gaps with 1.0 / abs(cosphi);
        gstart := -coord1;
        dir := +1;
        // Rotate polygon point such that gradient axis is parallel to x axis
        // (if angle < 45°) or y axis (if angle > 45°)
        // Rotation center is the projection of the corner of the bounding box
        // onto the gradient vector
        p1 := Point2D(
          AGradientStart.X + coord1 * gv.x / gvlen,
          AGradientStart.Y + coord1 * gv.y / gvlen
        );
        for j := 0 to High(APoints) do
        begin
          px := APoints[j].X - p1.x;
          py := APoints[j].Y - p1.y;
          pts[j] := Point2D(px*cosPhi + py*sinPhi, -px*sinPhi + py*cosPhi);
        end;
        // Begin painting at corner
        coord2 := coord2 - coord1;
        coord1 := 0;
        ADest.Pen.Width := 2;  // make sure that there are no gaps due to rounding errors
      end;
  end;

  // Draw gradient
  coord := coord1;
  while ((dcoord > 0) and (coord <= coord2)) or (dcoord < 0) and (coord >= coord2) do
  begin
    // Find intersection points of gradient line (normal to gradient vector)
    // with polygon
    lPoints := GetLinePolygonIntersectionPoints(coord, pts, APolyStarts, coordIsX);
    if Length(lPoints) < 2 then begin
      coord := coord + dcoord;
      Continue;
    end;

    // Prepare intersection points for painting
    case Brush.Kind of
      bkVerticalGradient:
        // Add loop variable as mssing y coordinate of intersection points
        for j := 0 to High(lPoints) do lPoints[j].Y := coord;
      bkHorizontalGradient:
        // Add loop variable as mssing x coordinate of intersection points
        for j := 0 to High(lPoints) do lPoints[j].X := coord;
      bkOtherLinearGradient:
        // Rotate back
        for j := 0 to High(lPoints) do
          lPoints[j] := Point2D(
            lPoints[j].X * cosPhi - lPoints[j].Y * sinPhi + p1.x,
            lPoints[j].X * sinPhi + lPoints[j].Y * cosPhi + p1.y
          );
    end;

    // Determine color from fraction (pf) of path travelled along gradient vector
    pf := (coord - gstart) * dir / gvlen;
    if Length(Brush.Gradient_colors) > 0 then
    begin
      ADest.Pen.FPColor := GradientColor(Brush.Gradient_colors, pf);
    end
    else
    begin
      lStr := RenderInfo_GenerateParentTree(ARenderInfo);
      if ARenderInfo.Errors <> nil then
        ARenderInfo.Errors.Add(Format('[%s] Empty Brush.Gradient_colors', [lStr]));
      ADest.Pen.FPColor := colBlack;
    end;

    // Draw gradient lines between intersection points
    j := 0;
    while j < High(lPoints) do
    begin
      ADest.Line(round(lPoints[j].X), round(lPoints[j].Y), round(lPoints[j+1].X), round(lPoints[j+1].Y));
      inc(j, 2);
    end;

    // Proceed to next line
    coord := coord + dcoord;
  end;
end;

procedure TvEntityWithPenAndBrush.DrawPolygonBrushRadialGradient(
  var ARenderInfo: TvRenderInfo;
  const APoints: TPointsArray; ARect: TRect);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  //
  i, j: Integer;
  lx, ly: Integer;
  lDist: Double;
  lGradient_cx_px, lGradient_cy_px, lGradient_r_px, lGradient_fx_px, lGradient_fy_px: Double;
  lWidth, lHeight, lBiggestHalfSide: Integer;
  lBiggestSizeIsY: Boolean;
  lColor: TFPColor;

  function Gradient_value_to_px(AValue: Double; AUnit: TvCoordinateUnit; AIsY: Boolean): Integer;
  var
    lSideLen: Integer;
  begin
    Result := 0;
    if AIsY then lSideLen := (ARect.Bottom-ARect.Top)
    else lSideLen := (ARect.Right-ARect.Left);
    case AUnit of
    //vcuDocumentUnit: Result := ;
    vcuPercentage:   Result := Round(lSideLen * AValue);
    end;
  end;

  function Distance_To_RadialGradient_Color(ADist: Double): TFPColor;
  var
    k: Integer;
  begin
    Result := colTransparent;
    for k := 0 to Length(Brush.Gradient_colors)-1 do
    begin
      if k = 0 then
      begin
        Result := Brush.Gradient_colors[k].Color;
        Continue;
      end;

      if ADist < Brush.Gradient_colors[k].Position then
      begin
        Result := MixColors(
          Brush.Gradient_colors[k-1].Color, Brush.Gradient_colors[k].Color,
          ADist - Brush.Gradient_colors[k-1].Position,
          Brush.Gradient_colors[k].Position - Brush.Gradient_colors[k-1].Position);
        Exit;
      end;
    end;
  end;

begin
  lWidth := (ARect.Right-ARect.Left);
  lHeight := (ARect.Bottom-ARect.Top);
  lBiggestSizeIsY := lHeight > lWidth;
  if lBiggestSizeIsY then lBiggestHalfSide := Round(lHeight / 2)
  else lBiggestHalfSide := Round(lWidth / 2);

  // Calculate Gradient_X_px
  lGradient_cx_px := Gradient_value_to_px(Brush.Gradient_cx, Brush.Gradient_cx_Unit, False);
  lGradient_cy_px := Gradient_value_to_px(Brush.Gradient_cy, Brush.Gradient_cy_Unit, True);
  lGradient_r_px := Gradient_value_to_px(Brush.Gradient_r, Brush.Gradient_r_Unit, lBiggestSizeIsY);
  lGradient_fx_px := Gradient_value_to_px(Brush.Gradient_fx, Brush.Gradient_fx_Unit, False);
  lGradient_fy_px := Gradient_value_to_px(Brush.Gradient_fy, Brush.Gradient_fy_Unit, True);

  // pixel-by-pixel version
  for i := 0 to lWidth-1 do
  begin
    for J := 0 to lHeight-1 do
    begin
      lx := ARect.Left + i;
      ly := ARect.Top + j;
      if not IsPointInPolygon(lx, ly, APoints) then Continue;

      lDist := sqrt(sqr(i-lGradient_cx_px)+sqr(j-lGradient_cy_px));
      lDist := lDist / lBiggestHalfSide;
      lDist := Min(Max(0, lDist), 1);
      lColor := Distance_To_RadialGradient_Color(lDist);
      ADest.Colors[lx, ly] := AlphaBlendColor(ADest.Colors[lx, ly], lColor);
    end;
  end;
end;

procedure TvEntityWithPenAndBrush.DrawNativePolygonBrushRadialGradient(
  var ARenderInfo: TvRenderInfo; const APoints: TPointsArray; ARect: TRect);
{$ifndef FPVECTORIAL_SUPPORT_LAZARUS_1_6}
{$ifdef USE_LCL_CANVAS}
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  //
  lLogRadGrad: TLogRadialGradient;
  lBrush, lOldBrush: HBRUSH;
  i: Integer;

  function Gradient_value_to_px(AValue: Double; AUnit: TvCoordinateUnit; AIsY: Boolean): Integer;
  var
    lSideLen: Integer;
  begin
    Result := 0;
    if AIsY then lSideLen := (ARect.Bottom-ARect.Top)
    else lSideLen := (ARect.Right-ARect.Left);
    case AUnit of
    vcuDocumentUnit: Result := Round(AValue);
    vcuPercentage:   Result := Round(lSideLen * AValue);
    end;
  end;

{$endif}
{$endif}
begin
  {$ifndef FPVECTORIAL_SUPPORT_LAZARUS_1_6}
  {$ifdef USE_LCL_CANVAS}
  lLogRadGrad.radCenterX := Gradient_value_to_px(Brush.Gradient_cx, Brush.Gradient_cx_Unit, False);
  lLogRadGrad.radCenterY := Gradient_value_to_px(Brush.Gradient_cy, Brush.Gradient_cy_Unit, False);
  lLogRadGrad.radRadius := Gradient_value_to_px(Brush.Gradient_r, Brush.Gradient_r_Unit, True);
  lLogRadGrad.radFocalX := Gradient_value_to_px(Brush.Gradient_fx, Brush.Gradient_fx_Unit, True);
  lLogRadGrad.radFocalY := Gradient_value_to_px(Brush.Gradient_fy, Brush.Gradient_fy_Unit, False);

  SetLength(lLogRadGrad.radStops, Length(Brush.Gradient_colors));
  for i := 0 to Length(Brush.Gradient_colors)-1 do
  begin
    lLogRadGrad.radStops[i].radColorA := Brush.Gradient_colors[i].Color.alpha;
    lLogRadGrad.radStops[i].radColorR := Brush.Gradient_colors[i].Color.red;
    lLogRadGrad.radStops[i].radColorG := Brush.Gradient_colors[i].Color.green;
    lLogRadGrad.radStops[i].radColorB := Brush.Gradient_colors[i].Color.blue;
    lLogRadGrad.radStops[i].radPosition := Brush.Gradient_colors[i].Position;
  end;

  lBrush := LCLIntf.CreateBrushWithRadialGradient(lLogRadGrad);
  lOldBrush := TCanvas(ADest).Brush.Handle;
  TCanvas(ADest).Brush.Handle := lBrush;
  TCanvas(ADest).Polygon(APoints);
  TCanvas(ADest).Brush.Handle := lOldBrush;
  {$endif}
  {$endif}
end;

procedure TvEntityWithPenAndBrush.DrawBrushGradient(
  var ARenderInfo: TvRenderInfo; x1, y1, x2, y2: Integer);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;
  //
  tmpPath: TPath;
  polypoints: TPointsArray;
  polystarts: TIntegerDynArray;
  lRect: TRect;
  gv1, gv2: T2dPoint;
  j: Integer;
begin
  tmpPath := CreatePath;
  if tmpPath = nil then
    exit;
  try
    ConvertPathToPolygons(tmpPath, ADestX, ADestY, AMulX, AMulY, polypoints, polystarts);

    // Boundary rect of shape filled with a gradient
    lRect := Rect(x1, y1, x2, y2);
    NormalizeRect(lRect);

    case Brush.Kind of
      bkHorizontalGradient,
      bkVerticalGradient,
      bkOtherLinearGradient:
        begin
          // calculate gradient vector
          CalcGradientVector(gv1, gv2, lRect, ADestX, ADestY, AMulX, AMulY);
          // Draw the gradient
          DrawPolygonBrushLinearGradient(ARenderInfo, polyPoints, polystarts, lRect, gv1, gv2);
        end;
      bkRadialGradient:
       {$ifdef USE_LCL_CANVAS}
        if Widgetset.GetLCLCapability(lcRadialGradientBrush) = LCL_CAPABILITY_YES then
          DrawNativePolygonBrushRadialGradient(ARenderInfo, polypoints, Bounds(0, 0, 1, 1))
        else
       {$endif}
          DrawPolygonBrushRadialGradient(ARenderInfo, polypoints, lRect);
    end;

    // Paint outline
    DrawPolygonBorderOnly(ARenderInfo, polyPoints);

  finally
    tmpPath.Free;
  end;
end;
(*
{ Fills the entity's shape with a gradient.
  Assumes that the boundary is in fpv units and provides parameters (ADestX,
  ADestY, AMulX, AMulY) for conversion to canvas pixels. }
procedure TvEntityWithPenAndBrush.DrawBrushGradient(ADest: TFPCustomCanvas;
  var ARenderInfo: TvRenderInfo; x1, y1, x2, y2: Integer;
  ADestX: Integer; ADestY: Integer; AMulX: Double; AMulY: Double);

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

  function CanvasToCoordY(ACanvas: Integer): Double;
  begin
    Result := (ACanvas - ADestY) / AmulY;
  end;

  function CanvasToCoordX(ACanvas: Integer): Double;
  begin
    Result := (ACanvas - ADestX) / AmulX;
  end;

var
  i, j: Integer;
  lPoints: TDoubleDynArray;
  lCanvasPts: array[0..1] of Integer;
  lColor, lColor1, lColor2: TFPColor;
begin
  if not (Brush.Kind in [bkVerticalGradient, bkHorizontalGradient]) then
    Exit;

  lColor1 := Brush.Gradient_colors[1].Color;
  lColor2 := Brush.Gradient_colors[0].Color;
  if Brush.Kind = bkVerticalGradient then
  begin
    for i := y1 to y2 do
    begin
      lPoints := GetLineIntersectionPoints(CanvasToCoordY(i), False);
      if Length(lPoints) < 2 then Continue;
      lColor := MixColors(lColor1, lColor2, i-y1, y2-y1);
      ADest.Pen.FPColor := lColor;
      ADest.Pen.Style := psSolid;
      j := 0;
      while j < Length(lPoints) do
      begin
        lCanvasPts[0] := CoordToCanvasX(lPoints[j]);
        lCanvasPts[1] := CoordToCanvasX(lPoints[j+1]);
        ADest.Line(lCanvasPts[0], i, lCanvasPts[1], i);
        inc(j, 2);
      end;
    end;
  end
  else if Brush.Kind = bkHorizontalGradient then
  begin
    for i := x1 to x2 do
    begin
      lPoints := GetLineIntersectionPoints(CanvasToCoordX(i), True);
      if Length(lPoints) < 2 then Continue;
      lColor := MixColors(lColor1, lColor2, i-x1, x2-x1);
      ADest.Pen.FPColor := lColor;
      ADest.Pen.Style := psSolid;
      j := 0;
      while (j+1 < Length(lPoints)) do
      begin
        lCanvasPts[0] := CoordToCanvasY(lPoints[j]);
        lCanvasPts[1] := CoordToCanvasY(lPoints[j+1]);
        ADest.Line(i, lCanvasPts[0], i, lCanvasPts[1]);
        inc(j , 2);
      end;
    end;
  end;
end;      *)

procedure TvEntityWithPenAndBrush.DrawBrush(var ARenderInfo: TvRenderInfo);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;
  //
  tmpPath: TPath;
  polypoints: TPointsArray;
  polystarts: TIntegerDynArray;
begin
  tmpPath := CreatePath;
  if tmpPath = nil then
    exit;
  try
    ConvertPathToPolygons(tmpPath, ADestX, ADestY, AMulX, AMulY, polypoints, polystarts);
    ADest.Polygon(polypoints);
  finally
    tmpPath.Free;
  end;
end;

procedure TvEntityWithPenAndBrush.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
begin
  inherited Render(ARenderInfo, ADoDraw);
  ApplyBrushToCanvas(ARenderInfo);
end;

function TvEntityWithPenAndBrush.GenerateDebugTree(
  ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer;
var
  lStr: string;
begin
  lStr := Format('[%s] Name=%s X=%f Y=%f Pen=[Color=%s Style=%s] Brush=[Color=%s Style=%s Kind=%s] %s',
    [Self.ClassName, Self.Name, X, Y,
    GenerateDebugStrForFPColor(Pen.Color),
    GetEnumName(TypeInfo(TFPPenStyle), integer(Pen.Style)),
    GenerateDebugStrForFPColor(Brush.Color),
    GetEnumName(TypeInfo(TFPBrushStyle), integer(Brush.Style)),
    GetEnumName(TypeInfo(TvBrushKind), integer(Brush.Kind)),
    FExtraDebugStr]);
  Result := ADestRoutine(lStr, APageItem);
end;


{ TvEntityWithPenBrushAndFont }

constructor TvEntityWithPenBrushAndFont.Create(APage: TvPage);
begin
  inherited Create(APage);
  Font.Color := colBlack;
  Font.Size := 10;
end;

procedure TvEntityWithPenBrushAndFont.ApplyFontToCanvas(ARenderInfo: TvRenderInfo);
begin
  ApplyFontToCanvas(ARenderInfo, Font);
end;

procedure TvEntityWithPenBrushAndFont.ApplyFontToCanvas(
  ARenderInfo: TvRenderInfo; AFont: TvFont);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;
  //
  {$ifdef USE_LCL_CANVAS}
  ALCLDest: TCanvas absolute ARenderInfo.Canvas;
  {$endif}
  lFPColor: TFPColor;
begin
  if ADest = nil then
    exit;
  ADest.Font.Name := AFont.Name;
  if AFont.Size = 0 then AFont.Size := 10;
  ADest.Font.Size := Round(AmulX * AFont.Size);
  ADest.Font.Bold := AFont.Bold;
  ADest.Font.Italic := AFont.Italic;
  ADest.Font.Underline := AFont.Underline;
  {$IF (FPC_FULLVERSION<=20600) or (FPC_FULLVERSION=20602)}
  ADest.Font.StrikeTrough := AFont.StrikeThrough; //old version with typo
  {$ELSE}
  ADest.Font.StrikeThrough := AFont.StrikeThrough;
  {$ENDIF}

  {$ifdef USE_LCL_CANVAS}
  ALCLDest.Font.Orientation := Round(AFont.Orientation * 10);  // wp: was * 16
  {$endif}
  lFPColor := AdjustColorToBackground(AFont.Color, ARenderInfo);
  ADest.Font.FPColor := lFPColor;
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

procedure TvEntityWithPenBrushAndFont.Rotate(AAngle: Double; ABase: T3DPoint);
begin
  inherited Rotate(AAngle, ABase);
  Font.Orientation := RadToDeg(AAngle);
end;

procedure TvEntityWithPenBrushAndFont.Scale(ADeltaScaleX, ADeltaScaleY: Double);
begin
  inherited Scale(ADeltaScaleX, ADeltaScaleY);
  Font.Size := Round(Font.Size * ADeltaScaleX);
end;

procedure TvEntityWithPenBrushAndFont.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
begin
  inherited Render(ARenderInfo, ADoDraw);
  ApplyFontToCanvas(ARenderInfo);
end;

function TvEntityWithPenBrushAndFont.GenerateDebugTree(
  ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer;
var
  lStr: string;
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

{ TvEntityWithStyle }

constructor TvEntityWithStyle.Create(APage: TvPage);
begin
  inherited Create(APage);
end;

destructor TvEntityWithStyle.Destroy;
begin
  inherited Destroy;
end;

function TvEntityWithStyle.GetCombinedStyle(AParent: TvEntityWithStyle): TvStyle;
begin
  if (AParent <> nil) and (Style = nil) then Result := AParent.Style
  else Result := Style;
end;

procedure TvEntityWithStyle.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
begin
  inherited Render(ARenderInfo, ADoDraw);
  if (Style <> nil) then
  begin
    ApplyPenToCanvas(ARenderInfo, Style.Pen);
    ApplyBrushToCanvas(ARenderInfo, @Style.Brush);
    ApplyFontToCanvas(ARenderInfo, Style.Font);
  end;
end;

{ TPath }

constructor TPath.Create(APage: TvPage);
begin
  inherited Create(APage);
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

procedure TPath.PrepareForWalking;
begin
  PrepareForSequentialReading();
  CurWalkDistanceInCurSegment := 0;
  Next();
end;

function TPath.Next(): TPathSegment;
begin
  if CurPoint = nil then Result := Points
  else Result := CurPoint.Next;

  CurPoint := Result;
end;

// Walk is walking a distance in the path and obtaining the point where we land and the current tangent
// Returns true if successful, false otherwise
// ATangentAngle - In radians
function TPath.NextWalk(ADistance: Double; out AX, AY, ATangentAngle: Double): Boolean;
var
  lCurPoint: TPathSegment;
  lCurPointLen: Double;
begin
  Result := False;
  lCurPoint := CurPoint;
  CurWalkDistanceInCurSegment := ADistance + CurWalkDistanceInCurSegment;
  if lCurPoint = nil then Exit;
  lCurPointLen := lCurPoint.GetLength();

  // get the current segment
  while CurWalkDistanceInCurSegment >= lCurPointLen do
  begin
    CurWalkDistanceInCurSegment := CurWalkDistanceInCurSegment - lCurPointLen;
    lCurPoint := Next();
    if lCurPoint = nil then Exit;
    lCurPointLen := lCurPoint.GetLength();
  end;

  Result := lCurPoint.GetPointAndTangentForDistance(CurWalkDistanceInCurSegment, AX, AY, ATangentAngle);
end;

procedure TPath.CalculateBoundingBox(constref ARenderInfo: TvRenderInfo;
  out ALeft, ATop, ARight, ABottom: Double);
var
  lSegment: TPathSegment;
  l2DSegment: T2DSegment;
  lFirstValue: Boolean = True;
begin
  inherited CalculateBoundingBox(ARenderInfo, ALeft, ATop, ARight, ABottom);

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
  ASegment.FPath := self;

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

procedure TPath.AppendEllipticalArc(ARadX, ARadY, AXAxisRotation, ADestX,
  ADestY: Double; ALeftmostEllipse, AClockwiseArcFlag: Boolean);
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

  AppendSegment(segment);
end;

procedure TPath.AppendEllipticalArcWithCenter(ARadX, ARadY, AXAxisRotation,
  ADestX, ADestY, ACenterX, ACenterY: Double; AClockwiseArcFlag: Boolean);
var
  segment: T2DEllipticalArcSegment;
begin
  segment := T2DEllipticalArcSegment.Create;
  segment.SegmentType := st2DEllipticalArc;
  segment.X := ADestX;
  segment.Y := ADestY;
  segment.RX := ARadX;
  segment.RY := ARadY;
  segment.CX := ACenterX;
  segment.CY := ACenterY;
  segment.XRotation := AXAxisRotation;
  segment.LeftmostEllipse := False; // which value would it have?
  segment.ClockwiseArcFlag := AClockwiseArcFlag;
  segment.CenterSetByUser := True;

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

{ Only correct for straight segments. This must have been checked before! }
function TPath.GetLineIntersectionPoints(ACoord: Double;
  ACoordIsX: Boolean): TDoubleDynArray;
const
  COUNT = 100;
var
  seg: TPathSegment;
  seg2D: T2DSegment; // absolute seg;
  j: Integer;
  p, p1, p2: T3DPoint;
  n: Integer;
begin
  SetLength(Result, COUNT);
  PrepareForSequentialReading;
  n := 0;
  if ACoordIsX then
    for j:=0 to Len-1 do
    begin
      seg := TPathSegment(Next);
      if seg.GetStartPoint(p) and (seg is T2DSegment) then
      begin
        seg2D := T2DSegment(seg);
        if p.X < seg2D.X then begin
          p1 := Make3DPoint(p.X, p.Y);
          p2 := Make3DPoint(seg2D.X, seg2D.Y);
        end else
        begin
          p1 := Make3DPoint(seg2D.X, seg2D.Y);
          p2 := Make3DPoint(p.X, p.Y);
        end;
        if (p1.X < ACoord) and (ACoord <= p2.X) then
        begin
          if n >= Length(Result) then
            SetLength(Result, Length(Result) + COUNT);
          if (p1.X = p2.X) then
            Result[n] := p1.Y else
            Result[n] := p1.Y + (ACoord - p1.X) * (p2.Y - p1.Y) / (p2.X - p1.X);
          inc(n);
        end;
      end;
    end
  else
    for j := 0 to Len-1 do
    begin
      seg := TPathSegment(Next);
      if seg.GetStartPoint(p) and (seg is T2DSegment) then
      begin
        seg2D := T2DSegment(seg);
        if p.Y < seg2D.Y then
        begin
          p1 := Make3DPoint(p.X, p.Y);
          p2 := Make3DPoint(seg2D.X, seg2D.Y);
        end else
        begin
          p1 := Make3DPoint(seg2D.X, seg2D.Y);
          p2 := Make3DPoint(p.X, p.Y);
        end;
        if (p1.Y < ACoord) and (ACoord <= p2.Y) then
        begin
          if n >= Length(Result) then
            SetLength(Result, Length(Result) + COUNT);
          if (p1.Y = p2.Y) then
            Result[n] := p1.X else
            Result[n] := p1.X + (ACoord - p1.Y) * (p2.X - p1.x) / (p2.Y - p1.Y);
          inc(n);
        end;
      end;
    end;
  SetLength(Result, n);
end;

procedure TPath.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
begin
  inherited Render(ARenderInfo, ADoDraw);
  ARenderInfo.Renderer.TPath_Render(ARenderInfo, ADoDraw, Self);
end;


(*
procedure TPath.Render(ADest: TFPCustomCanvas; var ARenderInfo: TvRenderInfo; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double; ADoDraw: Boolean);

  function CanFill: Boolean;
  var
    seg: TPathSegment;
    j: Integer;
  begin
    Result := true;
    PrepareForSequentialReading;
    for j := 0 to Len - 1 do
    begin
      seg := TPathSegment(Next());
      if seg.SegmentType in [st2DBezier, st3dBezier, st2DEllipticalArc] then
      begin
        Result := false;
        exit;
      end;
    end;
  end;

const
  POINT_BUFFER = 100;
var
  i, j: Integer;
  PosX, PosY: Double; // Not modified by ADestX, etc
  CoordX, CoordY: Integer;
  CurSegment: TPathSegment;
  Cur2DSegment: T2DSegment absolute CurSegment;
  Cur2DBSegment: T2DBezierSegment absolute CurSegment;
  Cur2DArcSegment: T2DEllipticalArcSegment absolute CurSegment;
  x1, y1, x2, y2: Integer;
  // For bezier
  CoordX2, CoordY2, CoordX3, CoordY3, CoordX4, CoordY4, CoordX5, CoordY5: Integer;
  // For polygons
  lPoints, pts: array of TPoint;
  NumPoints: Integer;
  pts3d: T3dPointsArray = nil;
  // for elliptical arcs
  BoxLeft, BoxTop, BoxRight, BoxBottom: Double;
  EllipseRect: TRect;
   // Clipping Region
  {$ifdef USE_LCL_CANVAS}
  ClipRegion, OldClipRegion: HRGN;
  ACanvas: TCanvas absolute ADest;
  {$endif}
begin
  inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY, ADoDraw);

  PosX := 0;
  PosY := 0;
//  ADest.Brush.Style := bsClear;

  ADest.MoveTo(ADestX, ADestY);
                           {
  // Set the path Pen and Brush options
  ADest.Pen.Style := Pen.Style;
  ADest.Pen.Width := Round(Pen.Width * AMulX);
  if ADest.Pen.Width < 1 then ADest.Pen.Width := 1;
  if (Pen.Width <= 2) and (ADest.Pen.Width > 2) then ADest.Pen.Width := 2;
  if (Pen.Width <= 5) and (ADest.Pen.Width > 5) then ADest.Pen.Width := 5;
  ADest.Pen.FPColor := AdjustColorToBackground(Pen.Color, ARenderInfo);
  {$ifdef USE_LCL_CANVAS}
  if (Pen.Style = psPattern)  then
    ACanvas.Pen.SetPattern(Pen.Pattern);
  {$endif}
  ADest.Brush.FPColor := Brush.Color;
                            }
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
    ConvertPathToPoints(CurPath.ClipPath, ADestX, ADestY, AMulX, AMulY);
    ACanvas.Polygon(lPoints);
    {$endif}
  end;
  {$endif}

  // useful in some paths, like stars!
  {  -- wp: causes artifacts in case of concave path
  if ADoDraw then
    RenderInternalPolygon(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY);
  }

  if CanFill then
  begin
    // Manually fill polygon with gradient
    {$IFDEF USE_LCL_CANVAS}
    if ADoDraw and (Brush.Kind in [bkHorizontalGradient, bkVerticalGradient]) then
    begin
      x1 := MaxInt;
      y1 := MaxInt;
      x2 := -MaxInt;
      y2 := -MaxInt;
      PrepareForSequentialReading;
      for j := 0 to Len - 1 do
      begin
        CurSegment := TPathSegment(Next);
        CoordX := CoordToCanvasX(Cur2DSegment.X, ADestX, AMulX);
        CoordY := CoordToCanvasY(Cur2DSegment.Y, ADestY, AMulY);
        x1 := Min(x1, CoordX);
        y1 := Min(y1, CoordY);
        x2 := Max(x2, CoordX);
        y2 := Max(y2, CoordY);
      end;
      DrawBrushGradient(ADest, ARenderInfo, x1, y1, x2, y2, ADestX, ADestY, AMulX, AMulY);
    end;
    {$ENDIF}
  end;

  //
  // For other paths, draw more carefully
  //
  ApplyPenToCanvas(ADest, ARenderInfo, Pen);  // Restore pen
  PrepareForSequentialReading;

  SetLength(lPoints, POINT_BUFFER);
  NumPoints := 0;

  for j := 0 to Len - 1 do
  begin
    //WriteLn('j = ', j);
    CurSegment := TPathSegment(Next());

    case CurSegment.SegmentType of
    stMoveTo:
    begin
      CoordX := CoordToCanvasX(Cur2DSegment.X, ADestX, AMulX);
      CoordY := CoordToCanvasY(Cur2DSegment.Y, ADestY, AMulY);

      if ADoDraw then
      begin
        // Draw previous polygon
        if NumPoints > 0 then
          begin
          SetLength(lPoints, NumPoints);
          if Length(lPoints) = 2 then
            ADest.Line(lPoints[0].X, lPoints[0].Y, lPoints[1].X, lPoints[1].Y)
          else
            ADest.Polygon(lPoints);
          // Start new polygon
          SetLength(lPoints, POINT_BUFFER);
          NumPoints := 0;
        end;

        lPoints[0].X := CoordX;
        lPoints[0].Y := CoordY;
        NumPoints := 1;
      end;


      {
      if ADoDraw then
        ADest.MoveTo(CoordX, CoordY);
        }
      CalcEntityCanvasMinMaxXY(ARenderInfo, CoordX, CoordY);
      PosX := Cur2DSegment.X;
      PosY := Cur2DSegment.Y;
      {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
      Write(Format(' M%d,%d', [CoordX, CoordY]));
      {$endif}
    end;

    // This element can override temporarely the Pen

    // TO DO: Paint these segments with correct pen at end !!!!


    st2DLineWithPen:
    begin
      ADest.Pen.FPColor := AdjustColorToBackground(T2DSegmentWithPen(Cur2DSegment).Pen.Color, ARenderInfo);
      CoordX := CoordToCanvasX(PosX, ADestX, AMulX);
      CoordY := CoordToCanvasY(PosY, ADestY, AMulY);
      CoordX2 := CoordToCanvasX(Cur2DSegment.X, ADestX, AMulX);
      CoordY2 := CoordToCanvasY(Cur2DSegment.Y, ADestY, AMulY);
      CalcEntityCanvasMinMaxXY_With2Points(ARenderInfo, CoordX, CoordY, CoordX2, CoordY2);
      if ADoDraw then
      begin
        if NumPoints >= Length(lPoints) then
          SetLength(lPoints, Length(lPoints) + POINT_BUFFER);
        lPoints[NumPoints].X := CoordX2;
        lPoints[NumPoints].Y := CoordY2;
        inc(NumPoints);
//        ADest.Line(CoordX, CoordY, CoordX2, CoordY2);
      end;

      PosX := Cur2DSegment.X;
      PosY := Cur2DSegment.Y;

      ADest.Pen.FPColor := Pen.Color;

      {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
      Write(Format(' L%d,%d', [CoordX2, CoordY2]));
      {$endif}
    end;

    st2DLine, st3DLine:
    begin
      CoordX := CoordToCanvasX(PosX, ADestX, AMulX);
      CoordY := CoordToCanvasY(PosY, ADestY, AMulY);
      CoordX2 := CoordToCanvasX(Cur2DSegment.X, ADestX, AMulX);
      CoordY2 := CoordToCanvasY(Cur2DSegment.Y, ADestY, AMulY);
      CalcEntityCanvasMinMaxXY_With2Points(ARenderInfo, CoordX, CoordY, CoordX2, CoordY2);
      if ADoDraw then
      begin
        if NumPoints >= Length(lPoints) then
          SetLength(lPoints, Length(lPoints) + POINT_BUFFER);
        lPoints[NumPoints].X := CoordX2;
        lPoints[NumPoints].Y := CoordY2;
        inc(NumPoints);
//        ADest.Line(CoordX, CoordY, CoordX2, CoordY2);
      end;
      PosX := Cur2DSegment.X;
      PosY := Cur2DSegment.Y;
      {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
      Write(Format(' L%d,%d', [CoordX2, CoordY2]));
      {$endif}
    end;

    { To draw a bezier we need to divide the interval in parts and make
      lines between this parts }
    st2DBezier, st3DBezier:
    begin
      CoordX := CoordToCanvasX(PosX, ADestX, AMulX);
      CoordY := CoordToCanvasY(PosY, ADestY, AMulY);
      CoordX2 := CoordToCanvasX(Cur2DBSegment.X2, ADestX, AMulX);
      CoordY2 := CoordToCanvasY(Cur2DBSegment.Y2, ADestY, AMulY);
      CoordX3 := CoordToCanvasX(Cur2DBSegment.X3, ADestX, AMulX);
      CoordY3 := CoordToCanvasY(Cur2DBSegment.Y3, ADestY, AMulY);
      CoordX4 := CoordToCanvasX(Cur2DBSegment.X, ADestX, AMulX);
      CoordY4 := CoordToCanvasY(Cur2DBSegment.Y, ADestY, AMulY);
//      SetLength(lPoints, 0);
      CalcEntityCanvasMinMaxXY_With2Points(ARenderInfo, CoordX, CoordY, CoordX2, CoordY2);
      CalcEntityCanvasMinMaxXY_With2Points(ARenderInfo, CoordX3, CoordY3, CoordX4, CoordY4);
      SetLength(pts, 0);
      AddBezierToPoints(
        Make2DPoint(CoordX, CoordY),
        Make2DPoint(CoordX2, CoordY2),
        Make2DPoint(CoordX3, CoordY3),
        Make2DPoint(CoordX4, CoordY4),
        pts //lPoints
      );

      if ADoDraw then
      begin
        if NumPoints + Length(pts) >= POINT_BUFFER then
          SetLength(lPoints, NumPoints + Length(pts));
        for i:=0 to High(pts) do
        begin
          lPoints[NumPoints].X := pts[i].X;
          lPoints[NumPoints].Y := pts[i].Y;
          inc(numPoints);
        end;
      end;

      ADest.Brush.Style := Brush.Style;
      {
      if (Length(lPoints) >= 3) and ADoDraw then
        ADest.Polygon(lPoints);
       }
      PosX := Cur2DSegment.X;
      PosY := Cur2DSegment.Y;

      {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
      Write(Format(' ***C%d,%d %d,%d %d,%d %d,%d',
        [CoordToCanvasX(PosX, ADestX, AMulX), CoordToCanvasY(PosY, ADestY, AMulY),
         CoordToCanvasX(Cur2DBSegment.X2, ADestX, AMulX), CoordToCanvasY(Cur2DBSegment.Y2, ADestY, AMulY),
         CoordToCanvasX(Cur2DBSegment.X3, ADestX, AMulX), CoordToCanvasY(Cur2DBSegment.Y3, ADestY, AMulY),
         CoordToCanvasX(Cur2DBSegment.X, ADestX, AMulX), CoordToCanvasY(Cur2DBSegment.Y, ADestY, AMulY)]));
      {$endif}
    end;

    st2DEllipticalArc:
    begin
      CoordX := CoordToCanvasX(PosX, ADestX, AMulX);                   // start point of segment
      CoordY := CoordToCanvasY(PosY, ADestY, AMulY);
      CoordX2 := CoordToCanvasX(Cur2DArcSegment.RX, ADestX, AMulX);    // major axis radius
      CoordY2 := CoordToCanvasY(Cur2DArcSegment.RY, ADestY, AMulY);    // minor axis radius
      CoordX3 := CoordToCanvasX(Cur2DArcSegment.XRotation, 0, sign(AMulX));   // axis rotation angle
      CoordX4 := CoordToCanvasX(Cur2DArcSegment.X, ADestX, AMulX);     // end point of segment
      CoordY4 := CoordToCanvasY(Cur2DArcSegment.Y, ADestY, AMulY);
      CoordX5 := CoordToCanvasX(Cur2DArcSegment.Cx, ADestX, AMulX);    // Ellipse center
      CoordY5 := CoordToCanvasY(Cur2DArcSegment.Cy, ADestY, AMulY);
//      SetLength(lPoints, 0);

      Cur2DArcSegment.CalculateEllipseBoundingBox(nil, BoxLeft, BoxTop, BoxRight, BoxBottom);

      EllipseRect.Left := CoordToCanvasX(BoxLeft, ADestX, AMulX);
      EllipseRect.Top := CoordToCanvasY(BoxTop, ADestY, AMulY);
      EllipseRect.Right := CoordToCanvasX(BoxRight, ADestX, AMulX);
      EllipseRect.Bottom := CoordToCanvasY(BoxBottom, ADestY, AMulY);

      {$ifdef FPVECTORIAL_TOCANVAS_ELLIPSE_VISUALDEBUG}
      ACanvas.Pen.Color := clRed;
      ACanvas.Brush.Style := bsClear;
      ACanvas.Rectangle(  // Ellipse bounding box
        EllipseRect.Left, EllipseRect.Top, EllipseRect.Right, EllipseRect.Bottom);
      ACanvas.Line(CoordX5-5, CoordY5, CoordX5+5, CoordY5);  // Ellipse center
      ACanvas.Line(CoordX5, CoordY5-5, CoordX5, CoordY5+5);
      ACanvas.Pen.Color := clBlue;
      ACanvas.Line(CoordX-5, CoordY, CoordX+5, CoordY);      // Start point
      ACanvas.Line(CoordX, CoordY-5, CoordX, CoordY+5);
      ACanvas.Line(CoordX4-5, CoordY4, CoordX4+5, CoordY4);  // End point
      ACanvas.Line(CoordX4, CoordY4-5, CoordX4, CoordY4+5);

      {$endif}

//      ADest.Brush.Style := Brush.Style;
      CalcEntityCanvasMinMaxXY_With2Points(ARenderInfo,
        EllipseRect.Left, EllipseRect.Top, EllipseRect.Right, EllipseRect.Bottom);

      if ADoDraw then
      begin
        Cur2DArcSegment.PolyApproximate(pts3D);
//        Cur2DArcSegment.BezierApproximate(pts3D);
        if NumPoints + Length(pts3D) >= POINT_BUFFER then
          SetLength(lPoints, NumPoints + Length(pts3D));
        for i:=1 to High(pts3D) do        // i=0 is end point of prev segment -> we can skip it.
        begin
          lPoints[NumPoints].X := CoordToCanvasX(pts3D[i].X, ADestX, AMulX);
          lPoints[NumPoints].Y := CoordToCanvasY(pts3D[i].Y, ADestY, AMulY);
          inc(numPoints);
        end;
        {
        SetLength(lPoints, Length(pts3D));
        for i:=0 to High(pts3D) do
        begin
          lPoints[i].X := CoordToCanvasX(pts3D[i].X, ADestX, AMulX);
          lPoints[i].Y := CoordToCanvasY(pts3D[i].Y, ADestY, AMulY);
        end;
        ADest.Polygon(lPoints);
        }
                               {
        i := 0;
        while i < Length(lPoints) do
        begin
          ADest.Polygon([lPoints[i], lPoints[i+1], lPoints[i+2], lPoints[i+3]]);
          inc(i, 4);
        end;
        }
        {
        // Arc draws counterclockwise
        if Cur2DArcSegment.ClockwiseArcFlag then
          ACanvas.Arc(
            EllipseRect.Left, EllipseRect.Top, EllipseRect.Right, EllipseRect.Bottom,
            CoordX4, CoordY4, CoordX, CoordY)
        else
          ACanvas.Arc(
            EllipseRect.Left, EllipseRect.Top, EllipseRect.Right, EllipseRect.Bottom,
            CoordX, CoordY, CoordX4, CoordY4);
        end;
        }
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

  // Draw polygon
  if ADoDraw then begin
    SetLength(lPoints, NumPoints);
    if Length(lPoints) = 2 then
      ADest.Line(lPoints[0].X, lPoints[0].Y, lPoints[1].X, lPoints[1].Y)
    else
      ADest.Polygon(lPoints);
  end;

  // Restores the previous Clip Region
  {$ifdef USE_CANVAS_CLIP_REGION}
  if ClipPath <> nil then
  begin
    SelectClipRgn(ACanvas.Handle, OldClipRegion); //Using OldClipRegion crashes in Qt
  end;
  {$endif}
end;
                             *)

procedure TPath.RenderInternalPolygon(constref ARenderInfo: TvRenderInfo);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;
  //

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  j: Integer;
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
  lStr := Format('[%s] Name=%s Pen.Color=%s Pen.Style=%s Brush.Color=%s Brush.Style=%s'
    + ' Brush.Kind=%s',
    [Self.ClassName, Self.Name,
    GenerateDebugStrForFPColor(Pen.Color),
    GetEnumName(TypeInfo(TFPPenStyle), integer(Pen.Style)),
    GenerateDebugStrForFPColor(Brush.Color),
    GetEnumName(TypeInfo(TFPBrushStyle), integer(Brush.Style)),
    GetEnumName(TypeInfo(TvBrushKind), integer(Brush.Kind))
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

function TvText.GetTextMetric_Descender_px(constref ARenderInfo: TvRenderInfo): Integer;
var
  {$ifdef USE_LCL_CANVAS}
  ACanvas: TCanvas absolute ARenderInfo.Canvas;
  tm: TLCLTextMetric;
  {$else}
  lFontSizePx: Integer;
  lTextSize: TSize;
  {$endif}
begin
  Result := 0;

  {$IFDEF USE_LCL_CANVAS}
  if ACanvas.GetTextMetrics(tm) then
    Result := tm.Descender;
  {$ELSE}
  lFontSizePx := Font.Size;        // is without multiplier!
  if lFontSizePx = 0 then lFontSizePx := 10;
  lTextSize := ADest.TextExtent(Str_Line_Height_Tester);
  Result := (lTextSize.CY - lFontSizePx) div 2;  // rough estimate only
  {$ENDIF}
end;

constructor TvText.Create(APage: TvPage);
begin
  inherited Create(APage);
  Value := TStringList.Create;
  Font.Color := colBlack;
end;

destructor TvText.Destroy;
begin
  Value.Free;
  inherited Destroy;
end;

function TvText.TryToSelect(APos: TPoint; var ASubpart: Cardinal; ASnapFlexibility: Integer = 5): TvFindEntityResult;
var
  lProximityFactor: Integer;
begin
  lProximityFactor := ASnapFlexibility;
  if (APos.X > X - lProximityFactor) and (APos.X < X + lProximityFactor)
    and (APos.Y > Y - lProximityFactor) and (APos.Y < Y + lProximityFactor) then
    Result := vfrFound
  else Result := vfrNotFound;
end;

procedure TvText.CalculateBoundingBox(constref ARenderInfo: TvRenderInfo;
  out ALeft, ATop, ARight, ABottom: Double);
var
  i: Integer;
  lSize: TSize;
  lWidth, lHeight: Integer;
  lRenderInfo: TvRenderInfo;
  lText: String;
  {$ifdef USE_LCL_CANVAS}
  ACanvas: TCanvas absolute ARenderInfo.Canvas;
  {$endif}
begin
  //lText := Value.Text; // For debugging
  InitializeRenderInfo(lRenderInfo, Self);
  lRenderInfo.Canvas := ARenderInfo.Canvas;
  lRenderInfo.DestX := ARenderInfo.DestX;
  lRenderInfo.DestY := ARenderInfo.DestY;
  lRenderInfo.MulX := ARenderInfo.MulX;
  lRenderInfo.MulY := ARenderInfo.MulY;
  inherited Render(lRenderInfo, False);

  ALeft := X;
  ATop := Y;
  lWidth := 0;
  lHeight := 0;
  ARight := ALeft;
  ABottom := ATop;
  if (ARenderInfo.Canvas = nil) or (not (ARenderInfo.Canvas is TCanvas)) then Exit;

  for i := 0 to Value.Count-1 do
  begin
    lText := Value.Strings[i];
    lSize := ACanvas.TextExtent(lText);
    lWidth := Max(lWidth, lSize.cx);
    lSize := ACanvas.TextExtent(Str_Line_Height_Tester);
    lHeight := lHeight + lSize.cy + 2;
  end;

  ALeft := X;
  ATop := Y - lHeight;
  ARight := ALeft + lWidth;
  ABottom := Y;
end;

procedure TvText.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
const
  LINE_SPACING = 0.2;  // fraction of font height for line spacing
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;
  //
  i: Integer;
  //
  pt, refPt: TPoint;
  LowerDimY, UpperDimY, CurDimY: Double;
  XAnchorAdjustment: Integer;
  lLongestLine, lLineWidth, lFontSizePx, lFontDescenderPx: Integer;
  lText: string;
  lDescender: Integer;
  phi: Double;
  {$ifdef USE_LCL_CANVAS}
  ACanvas: TCanvas absolute ARenderInfo.Canvas;
  lTextSize: TSize;
  lTextWidth: Integer;
  {$endif}
begin
  lText := Value.Text + Format(' F=%d', [ACanvas.Font.Size]); // for debugging
  inherited Render(ARenderInfo, ADoDraw);

  InitializeRenderInfo(ARenderInfo, Self);

  // Don't draw anything if we have alpha=zero
  if Font.Color.Alpha = 0 then Exit;
  ADest.Font.FPColor := AdjustColorToBackground(Font.Color, ARenderInfo);

  // Font metric
  lFontSizePx := Font.Size;        // is without multiplier!
  if lFontSizePx = 0 then lFontSizePx := 10;
  lTextSize := ADest.TextExtent(Str_Line_Height_Tester);
  lDescender := GetTextMetric_Descender_px(ARenderInfo);

  // Angle of text rotation
  phi := sign(AMulY) * DegToRad(Font.Orientation);

  // Reference point of the entity (X,Y) in pixels
  // rotation center in case of rotated text
  refPt := Point(
    round(CoordToCanvasX(X, ADestX, AMulX)),
    round(CoordToCanvasY(Y, ADestY, AMulY))
  );

  // if an anchor is set, use it
  // to do this, first search for the longest line
  XAnchorAdjustment := 0;
  if TextAnchor <> vtaStart then
  begin
    lLongestLine := 0;
    for i := 0 to Value.Count - 1 do
    begin
      lLineWidth := ACanvas.TextWidth(Value.Strings[i]);   // contains multiplier
      if lLineWidth > lLongestLine then
        lLongestLine := lLineWidth;
    end;
    case TextAnchor of
      vtaMiddle : XAnchorAdjustment := -lLongestLine div 2;
      vtaEnd    : XAnchorAdjustment := -lLongestLine;
    end;
  end;

  // Begin first line at reference point and grow downwards.
  // ...
  // We need to keep the order of lines drawing correct regardless of
  // the drawing direction
  lowerDimY := refPt.Y - (lTextSize.CY - lDescender); // lowerDim.Y := refPt.Y + lFontSizePx * (1 + LINE_SPACING) * Value.Count * AMulY
  upperDimY := refPt.Y;
  curDimY := IfThen(AMulY < 0, lowerDimY, upperDimY);

  // TvText supports multiple lines
  for i := 0 to Value.Count - 1 do
  begin
    lText := Value.Strings[i];
    if not Render_Use_NextText_X then
      Render_NextText_X := refPt.X + XAnchorAdjustment;

    // Start point of text, rotated around the reference point
    pt := Point(round(Render_NextText_X), round(curDimY));  // before rotation
    pt := Rotate2dPoint(pt, refPt, -Phi);                   // after rotation

    // Paint line
    if ADoDraw then
    begin
      ADest.TextOut(pt.x, pt.y, lText);
    end;

    // Calc text boundaries respecting text rotation.
    CalcEntityCanvasMinMaxXY(ARenderInfo, pt.x, pt.y);
    lTextSize := ACanvas.TextExtent(lText);
    lTextWidth := lTextSize.cx;
    // Reserve vertical space for </br> and similar line ending constructs
    if (lText = '') then
      lTextSize.cy := ACanvas.TextHeight(STR_FPVECTORIAL_TEXT_HEIGHT_SAMPLE);
    // other end of the text
    pt := Point(round(Render_NextText_X) + lTextWidth, round(curDimY) + lTextSize.cy );
    pt := Rotate2dPoint(pt, refPt, -Phi);
    CalcEntityCanvasMinMaxXY(ARenderInfo, pt.x, pt.y);

    // Prepare next line
    Render_NextText_X := Render_NextText_X + lTextWidth;
    curDimY := IfThen(AMulY < 0,
      curDimY - (lFontSizePx * (1 + LINE_SPACING) * AMulY),
      curDimY + (lFontSizePx * (1 + LINE_SPACING) * AMulY));
    // wp: isn't this the same as
    // curDimY := curDimY + (lFontSizePx * (1 + LINE_SPACING) * abs(AMulY);
  end;
end;

function TvText.GetEntityFeatures(constref ARenderInfo: TvRenderInfo): TvEntityFeatures;
var
  ActualText: String;
  lHeight_px: Integer = 0;
begin
  // Calculate the total height
  CalculateHeightInCanvas(ARenderInfo, lHeight_px);
  Result.TotalHeight := lHeight_px;

  Result.DrawsUpwardHeightAdjustment := 0;
  if (not FPage.UseTopLeftCoordinates) then
    Result.DrawsUpwardHeightAdjustment := lHeight_px;

  Result.FirstLineHeight := 0;
  if (Value.Count > 0) then
  begin
    ActualText := Value.Text;
    Value.Text := Value.Strings[0];
    CalculateHeightInCanvas(ARenderInfo, lHeight_px);
    Result.FirstLineHeight := lHeight_px - GetTextMetric_Descender_px(ARenderInfo);

    Value.Text := ActualText;
  end;
  Result.DrawsUpwards := True;
end;

function TvText.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr, lValueStr: string;
begin
  lValueStr := GenerateDebugStrForString(Value.Text);
  lStr := Format('[%s] Name=%s X=%f Y=%f Text="%s" [.Font=>] Color=%s Size=%d Name=%s Orientation=%f Bold=%s Italic=%s Underline=%s StrikeThrough=%s TextAnchor=%s',
    [
    Self.ClassName, Name, X, Y, lValueStr,
    GenerateDebugStrForFPColor(Font.Color),
    Font.Size, Font.Name, Font.Orientation,
    BoolToStr(Font.Bold),
    BoolToStr(Font.Italic),
    BoolToStr(Font.Underline),
    BoolToStr(Font.StrikeThrough),
    GetEnumName(TypeInfo(TvTextAnchor), integer(TextAnchor))
  ]);
  Result := ADestRoutine(lStr, APageItem);
  // Add the style as a sub-item
  if Style <> nil then
  begin
    Style.GenerateDebugTree(ADestRoutine, Result);
  end;
end;

{ TvCurvedText }

procedure TvCurvedText.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;
  //
  i, lCharLen: Integer;
  lText, lUTF8Char: string;
  lX, lY, lTangentAngle, lTextHeight: Double;
  pt: TPoint;
  //lLeft, lTop, lWidth, lHeight: Integer;
begin
  inherited Render(ARenderInfo, False);

  InitializeRenderInfo(ARenderInfo, Self);
       (*
  if not ADoDraw then
  begin
    //Path.CalculateSizeInCanvas(ADest, lLeft, lTop, lWidth, lHeight);
    Exit;
  end;   *)

  // Don't draw anything if we have alpha=zero
  if Font.Color.Alpha = 0 then Exit;
  if Path = nil then Exit;

  ADest.Font.FPColor := AdjustColorToBackground(Font.Color, ARenderInfo);
  if Value.Count = 0 then Exit;
  lText := Value.Strings[0];
  Render_NextText_X := CoordToCanvasX(X, ADestX, AMulX);

  Path.PrepareForWalking();
  Path.NextWalk(0, lX, lY, lTangentAngle);

  // render each character separately
  for i := 0 to UTF8Length(lText)-1 do
  begin
    lUTF8Char := UTF8Copy(lText, i+1, 1);
    ADest.Font.Orientation := Round(Math.radtodeg(lTangentAngle)*10);

    // Without adjustment the text is down bellow the path, but we want it on top of it
    {lTextHeight := Abs(AMulY) * ADest.TextHeight(lUTF8Char);
    lX := lX - Sin(Pi / 2 - lTangentAngle) * lTextHeight;
    lY := lY + Cos(Pi / 2 - lTangentAngle) * lTextHeight;}

    pt := Point(CoordToCanvasX(lX, ADestX, AMulX), CoordToCanvasY(lY, ADestY, AMulY));
    CalcEntityCanvasMinMaxXY(ARenderInfo, pt.x, pt.y);

    if ADoDraw then
      ADest.TextOut(pt.X, pt.Y, lUTF8Char);

    lCharLen := ADest.TextWidth(lUTF8Char);
    Path.NextWalk(lCharLen, lX, lY, lTangentAngle);
  end;
end;

function TvCurvedText.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
begin
  Result:=inherited GenerateDebugTree(ADestRoutine, APageItem);
  if Path <> nil then
    Path.GenerateDebugTree(ADestRoutine, Result);
end;

{ TvField }

constructor TvField.Create(APage: TvPage);
begin
  inherited Create(APage);

  DateFormat := 'dd/MM/yyyy hh:mm:ss';
  NumberFormat := vnfDecimal;
end;

{ TvCircle }

procedure TvCircle.CalculateBoundingBox(constref ARenderInfo: TvRenderInfo;
  out ALeft, ATop, ARight, ABottom: Double);
begin
  ALeft := X - Radius;
  ARight := X + Radius;
  ATop := Y + Radius;
  ABottom := Y - Radius;
end;

function TvCircle.CreatePath: TPath;
begin
  Result := TPath.Create(FPage);
  Result.AppendMoveToSegment(X + Radius, Y);
  Result.AppendEllipticalArcWithCenter(Radius, Radius, 0, X - Radius, Y, X, Y, true);
  Result.AppendEllipticalArcWithCenter(Radius, Radius, 0, X + Radius, Y, X, Y, true);
end;

procedure TvCircle.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;
  //
  x1, y1, x2, y2: Integer;
begin
  inherited Render(ARenderInfo, ADoDraw);

  x1 := CoordToCanvasX(X - Radius, ADestX, AMulX);
  y1 := CoordToCanvasY(Y - Radius, ADestY, AMulY);
  x2 := CoordToCanvasX(X + Radius, ADestX, AMulX);
  y2 := CoordToCanvasY(Y + Radius, ADestY, AMulY);
  CalcEntityCanvasMinMaxXY_With2Points(ARenderInfo, x1, y1, x2, y2);

  if ADoDraw then
  begin
    if Brush.Kind <> bkSimpleBrush then
      // Draw gradient and border
      DrawBrushGradient(ARenderInfo, x1, y1, x2, y2)
    else
      // Draw uniform fill and border
      DrawBrush(ARenderInfo);
  end;
end;

procedure TvCircle.Rotate(AAngle: Double; ABase: T3DPoint);
var
  ctr: T3dPoint;
begin
  ctr := Rotate3dPointInXY(Make3dPoint(X,Y), ABase, -AAngle);
    // use inverted angle due to sign convention in Rotate3DPointInXY
  X := ctr.X;
  Y := ctr.Y;
end;

{ TvCircularArc }

procedure TvCircularArc.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;
  //

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
  inherited Render(ARenderInfo, ADoDraw);
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

function TvEllipse.CreatePath: TPath;
var
  p1, p2: T2dPoint;
begin
  Result := TPath.Create(FPage);

  CalcEllipsePoint(0,  HorzHalfAxis,VertHalfAxis, X,Y, Angle, p1.x, p1.y);
  CalcEllipsePoint(pi, HorzHalfAxis,VertHalfAxis, X,Y, Angle, p2.x, p2.y);

  Result.AppendMoveToSegment(p1.x, p1.y);
  Result.AppendEllipticalArcWithCenter(HorzHalfAxis, VertHalfAxis, Angle, p2.x, p2.y, X, Y, false);
  Result.AppendEllipticalArcWithCenter(HorzHalfAxis, VertHalfAxis, Angle, p1.x, p1.y, X, Y, false);
end;

// wp: no longer needed
function TvEllipse.GetLineIntersectionPoints(ACoord: Double; ACoordIsX: Boolean): TDoubleDynArray;
begin
  SetLength(Result, 2);
  // this is for axis-aligned ellipses
  // (X-Xcenter)^2 / Rx^2 + (Y-Ycenter)^2 / Ry^2 <= 1
  if ACoordIsX then
  begin
    // Y = sqrt( 1 - (X-Xcenter)^2 / Rx^2 ) * Ry + Ycenter
    Result[0] := Max(0, 1-sqr(ACoord-X) / sqr(HorzHalfAxis));
    Result[0] := sqrt(Result[0]) * VertHalfAxis + Y;
    Result[1] := Max(0, 1-sqr(ACoord-X) / sqr(HorzHalfAxis));
    Result[1] := -1 * sqrt(Result[1]) * VertHalfAxis + Y;
  end
  else
  begin
    Result[0] := Max(0, 1-sqr(ACoord-Y) / sqr(VertHalfAxis));
    Result[0] := sqrt(Result[0]) * HorzHalfAxis + X;
    Result[1] := Max(0, 1-sqr(ACoord-Y) / sqr(VertHalfAxis));
    Result[1] := -1 * sqrt(Result[1]) * HorzHalfAxis + X;
  end;
end;

function TvEllipse.TryToSelect(APos: TPoint; var ASubpart: Cardinal;
  ASnapFlexibility: Integer): TvFindEntityResult;
begin
  // this is for axis-aligned ellipses
  // (X-Xcenter)^2 / Rx^2 + (Y-Ycenter)^2 / Ry^2 <= 1
  Result := vfrNotFound;
  //Result := vfrFound;
end;

procedure TvEllipse.CalculateBoundingBox(constref ARenderInfo: TvRenderInfo;
  out ALeft, ATop, ARight, ABottom: Double);
var
  t, tmp: Double;
begin
  { To calculate the bounding rectangle we can do this:

    Ellipse equations:
    You could try using the parametrized equations for an ellipse rotated by
    an arbitrary angle:

    x = cx + a*cos(t)*cos(Angle) - b*sin(t)*sin(Angle)
    y = cy + b*sin(t)*cos(Angle) + a*cos(t)*sin(Angle)

    You can then differentiate and solve for gradient = 0:
    0 = dx/dt = -a*sin(t)*cos(Angle) - b*cos(t)*sin(Angle)
      ==> tan(t) = -b*tan(Angle)/a
      ==> t = arctan(-b*tan(Angle)/a)
      ==> left and right corner of bounding box

    On the other axis:
    0 = dy/dt = b*cos(t)*cos(Angle) - a*sin(t)*sin(Angle)
      ==> tan(t) = b*cot(Angle)/a
      ==> t = arctan(b*cot(Angle)/a)
      ==> top and bottom corner of bounding box
  }
  if Angle <> 0.0 then
  begin
    t := arctan(-VertHalfAxis*tan(Angle)/HorzHalfAxis);
    tmp := abs(HorzHalfAxis*cos(t)*cos(Angle) - VertHalfAxis*sin(t)*sin(Angle));
    ALeft := X - Round(tmp);
    ARight := X + Round(tmp);
    t := arctan(VertHalfAxis*cot(Angle) / HorzHalfAxis);
    tmp := abs(VertHalfAxis*sin(t)*cos(Angle) + HorzHalfAxis*cos(t)*sin(Angle));
    ATop := Y + Round(tmp);
    ABottom := Y - Round(tmp);
  end else
  begin
    ALeft := X - HorzHalfAxis;
    ARight := X + HorzHalfAxis;
    ATop := Y + VertHalfAxis;       // wp: changed from - to +
    ABottom := Y - VertHalfAxis;    // ... and this from + to -
  end;
end;

procedure TvEllipse.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;
  //
  x1, y1, x2, y2: Integer;
  fx1, fx2, fy1, fy2: Double;
begin
  inherited Render(ARenderInfo, ADoDraw);

  CalculateBoundingBox(ARenderInfo, fx1, fy1, fx2, fy2);
  x1 := CoordToCanvasX(fx1, ADestX, AMulX);
  x2 := CoordToCanvasX(fx2, ADestX, AMulX);
  y1 := CoordToCanvasY(fy1, ADestY, AMulY);
  y2 := CoordToCanvasY(fy2, ADestY, AMulY);
  CalcEntityCanvasMinMaxXY_With2Points(ARenderInfo, x1, y1, x2, y2);

  if ADoDraw then
  begin
    if Brush.Kind <> bkSimpleBrush then
      // Draw gradient and border
      DrawBrushGradient(ARenderInfo, x1, y1, x2, y2)
    else
      // Draw uniform fill and border
      DrawBrush(ARenderInfo);
//      ADest.Ellipse(x1, y1, x2, y2);
  end;
end;
                                              (*
procedure TvEllipse.Render(ADest: TFPCustomCanvas; var ARenderInfo: TvRenderInfo; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double; ADoDraw: Boolean);

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
  inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY, ADoDraw);

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
     if ADoDraw then
      ALCLDest.PolyBezier(Pointlist[0]);
  end
  else
  {$endif}
  begin
    if ADoDraw then ADest.Ellipse(x1, y1, x2, y2);
  end;
  // Apply brush gradient
  if x1 > x2 then
  begin
    dk := x1;
    x1 := x2;
    x2 := dk;
  end;
  if y1 > y2 then
  begin
    dk := y1;
    y1 := y2;
    y2 := dk;
  end;
  DrawBrushGradient(ADest, ARenderInfo, x1, y1, x2, y2, ADestX, ADestY, AMulX, AMulY);

  CalcEntityCanvasMinMaxXY_With2Points(ARenderInfo, x1, y1, x2, y2);
end;                                            *)

procedure TvEllipse.Rotate(AAngle: Double; ABase: T3DPoint);
var
  ctr: T3dPoint;
begin
  ctr := Rotate3dPointInXY(Make3dPoint(X,Y), ABase, -AAngle);
    // use inverted angle due to sign convention in Rotate3DPointInXY
  X := ctr.X;
  Y := ctr.Y;
  Angle := AAngle + Angle;
end;


{ TvRectangle }

procedure TvRectangle.CalculateBoundingBox(constref ARenderInfo: TvRenderInfo;
  out ALeft, ATop, ARight, ABottom: Double);
var
  pts: Array[0..3] of T3DPoint;
  j: Integer;
begin
  if Angle <> 0 then
  begin
    pts[0] := Make3dPoint(X, Y);  // corner points, ignoring rounded corner!
    pts[1] := Make3dPoint(X+CX, Y);
    pts[2] := Make3dPoint(X+CX, Y-CY);
    pts[3] := Make3dPoint(X, Y-CY);
    for j:=0 to High(pts) do
      pts[j] := Rotate3DPointInXY(pts[j], pts[0], -Angle);   // left/top is rot center!
      // Use inverted angle due to sign convention in Rotate3DPointInXY
    ALeft := pts[0].x;
    ARight := Pts[0].x;
    ATop := pts[0].y;
    ABottom := pts[0].y;
    for j:=1 to High(pts) do
    begin
      ALeft := Min(ALeft, pts[j].x);
      ARight := Max(ARight, pts[j].x);
      ATop := Max(ATop, pts[j].y);
      ABottom := Min(ABottom, pts[j].y);
    end;
  end else
  begin
    ALeft := X;
    ATop := Y;
    ARight := X + CX;
    ABottom := IfThen(FPage.FUseTopLeftCoordinates, Y + CY, Y - CY);
  end;
end;

function TvRectangle.CreatePath: TPath;
var
  pts: T3dPointsArray;
  ctr: T3dPoint;
  j: Integer;
  phi, lYAdj: Double;
begin
  lYAdj := FPage.GetTopLeftCoords_Adjustment(); // top/left: +1, bottom/left: -1

  if (RX > 0) and (RY > 0) then
  begin
    SetLength(pts, 9);
    pts[0] := Make3dPoint(X, Y+lYAdj*RY);           {    1              2    }
    pts[1] := Make3dPoint(X+RX, Y);                 {  0,8                3  }
    pts[2] := Make3dPoint(X+CX-RX, Y);              {                        }
    pts[3] := Make3dPoint(X+CX, Y+lYAdj*RY);        {                        }
    pts[4] := Make3dPoint(X+CX, Y+lYAdj*(CY-RY));   {  7                  4  }
    pts[5] := Make3dPoint(X+CX-RX, Y+lYAdj*CY);     {    6              5    }
    pts[6] := Make3dPoint(X+RX, Y+lYAdj*CY);
    pts[7] := Make3dPoint(X, Y+lYAdj*(CY-RY));
    pts[8] := Make3dPoint(X, Y+lYAdj*RY);
  end
  else
  begin
    SetLength(pts, 5);
    pts[0] := Make3dPoint(X, Y);
    pts[1] := Make3dPoint(X+CX, Y);
    pts[2] := Make3dPoint(X+CX, Y+lYAdj*CY);
    pts[3] := Make3dPoint(X, Y+lYAdj*CY);
    pts[4] := Make3dPoint(X, Y);
  end;
  ctr := Make3DPoint(X, Y);  // Rotation center
  phi := -Angle;             // Angle must be inverted due to sign convention in Rotate3DPointInXY
  for j:=0 to High(pts) do
    pts[j] := Rotate3DPointInXY(pts[j], ctr, phi);

  Result := TPath.Create(FPage);
  if (RX > 0) and (RY > 0) then
  begin
    Result.AppendMoveToSegment(pts[0].x, pts[0].y);
    Result.AppendEllipticalArcWithCenter(RX, RY, phi, pts[1].x, pts[1].y,
      pts[1].x, pts[0].y, true);
    Result.AppendLineToSegment(pts[2].x, pts[2].y);
    Result.AppendEllipticalArcWithCenter(RX, RY, phi, pts[3].x, pts[3].y,
      pts[2].x, pts[3].y, true);
    Result.AppendLineToSegment(pts[4].x, pts[4].y);
    Result.AppendEllipticalArcWithCenter(RX, RY, phi, pts[5].x, pts[5].y,
      pts[5].x, pts[4].y, true);
    Result.AppendLineToSegment(pts[6].x, pts[6].y);
    Result.AppendEllipticalArcWithCenter(RX, RY, phi, pts[7].x, pts[7].y,
      pts[6].x, pts[7].y, true);
    Result.AppendLineToSegment(pts[8].x, pts[8].y);
  end else
  begin
    Result.AppendMoveToSegment(pts[0].x, pts[0].y);
    Result.AppendLineToSegment(pts[1].x, pts[1].y);
    Result.AppendLineToSegment(pts[2].x, pts[2].y);
    Result.AppendLineToSegment(pts[3].x, pts[3].y);
    Result.AppendLineToSegment(pts[4].x, pts[4].y);
  end;
end;

procedure TvRectangle.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;
  //
  x1, y1, x2, y2: Integer;
  fx1, fy1, fx2, fy2: Double;
begin
  inherited Render(ARenderInfo, ADoDraw);

  CalculateBoundingBox(ARenderInfo, fx1, fy1, fx2, fy2);
  x1 := CoordToCanvasX(fx1, ADestX, AMulX);
  x2 := CoordToCanvasX(fx2, ADestX, AMulX);
  y1 := CoordToCanvasY(fy1, ADestY, AMulY);
  y2 := CoordToCanvasY(fy2, ADestY, AMulY);
  CalcEntityCanvasMinMaxXY_With2Points(ARenderInfo, x1, y1, x2, y2);

  if ADoDraw then
  begin
    if Brush.Kind <> bkSimpleBrush then
      // Draw gradient and border
      DrawBrushGradient(ARenderInfo, x1, y1, x2, y2)
    else
      // Draw uniform fill and border
      DrawBrush(ARenderInfo);
  end;
end;
                            (*
procedure TvRectangle.Render(ADest: TFPCustomCanvas; var ARenderInfo: TvRenderInfo; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double; ADoDraw: Boolean);

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
  inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY, ADoDraw);

  CalculateBoundingBox(ADest, fx1, fy1, fx2, fy2);
  x1 := CoordToCanvasX(fx1);
  x2 := CoordToCanvasX(fx2);
  y1 := CoordToCanvasY(fy1);
  y2 := CoordToCanvasY(fy2);

  if ADoDraw then
  begin
    {$ifdef USE_LCL_CANVAS}
    if (RX = 0) and (RY = 0) then
      ADest.Rectangle(x1, y1, x2, y2)
    else
      LCLIntf.RoundRect(TCanvas(ADest).Handle, x1, y1, x2, y2, Round(rx), Round(ry));
    {$else}
    ADest.Rectangle(x1, y1, x2, y2)
    {$endif}
  end;

  CalcEntityCanvasMinMaxXY_With2Points(ARenderInfo, x1, y1, x2, y2);
end;                 *)

procedure TvRectangle.Rotate(AAngle: Double; ABase: T3DPoint);
var
  ref: T3dPoint;  // reference point of rectangle
begin
  ref := Rotate3dPointInXY(Make3dPoint(X, Y), ABase, -AAngle);
  X := ref.X;
  Y := ref.Y;
  Angle := AAngle + Angle;
end;

function TvRectangle.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr: string;
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

procedure TvPolygon.CalculateBoundingBox(constref ARenderInfo: TvRenderInfo;
  out ALeft, ATop, ARight, ABottom: Double);
var
  i: Integer;
begin
  inherited CalculateBoundingBox(ARenderInfo, ALeft, ATop, ARight, ABottom);
  for i := 0 to Length(Points)-1 do
  begin
    ALeft := Min(ALeft, Points[i].X);
    ATop := Min(ATop, Points[i].Y);
    ARight := Max(ARight, Points[i].X);
    ABottom := Max(ABottom, Points[i].Y);
  end;
end;

procedure TvPolygon.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;
  //
  lPoints: array of TPoint;
  i: Integer;
  x1, x2, y1, y2: Integer;
  polystarts: TIntegerDynArray;
  lRect: TRect;
  gv1, gv2: T2DPoint;
begin
  inherited Render(ARenderInfo, ADoDraw);

  x1 := MaxInt;
  y1 := maxInt;
  x2 := -MaxInt;
  y2 := -MaxInt;
  SetLength(lPoints, Length(Points));
  for i := 0 to High(Points) do
  begin
    lPoints[i].X := CoordToCanvasX(Points[i].X, ADestX, AMulX);
    lPoints[i].Y := CoordToCanvasY(Points[i].Y, ADestY, AMulY);
    x1 := min(x1, lPoints[i].X);
    y1 := min(y1, lPoints[i].Y);
    x2 := max(x2, lPoints[i].X);
    y2 := max(y2, lPoints[i].Y);
  end;
  CalcEntityCanvasMinMaxXY_With2Points(ARenderInfo, x1, y1, x2, y2);

  if ADoDraw then
    if (Length(lPoints) > 2) then
    begin
      case Brush.Kind of
        bkSimpleBrush:
          ADest.Polygon(lPoints);  // fills the polygon and paints the border
        bkHorizontalGradient,
        bkVerticalGradient,
        bkOtherLinearGradient:
          begin
            // Border will be drawn later (gradient painting needs its own pen)
            ADest.Pen.Style := psClear;
            // Boundary rect of shape to be filled by a gradient
            lRect := Rect(x1, y1, x2, y2);
            // Calculate gradient vector
            CalcGradientVector(gv1, gv2, lRect, ADestX, ADestY, AMulX, AMulY);
            // Indexes where polygon starts: no multiple polygones here
            SetLength(polyStarts, 1);
            polyStarts[0] := 0;
            // Draw the gradient
            DrawPolygonBrushLinearGradient(ARenderInfo, lPoints, polyStarts, lRect, gv1, gv2);
            // Draw border
            DrawPolygonBorderOnly(ARenderInfo, lPoints);
          end;
        bkRadialGradient:
          begin
            // Border will be drawn later (gradient painting needs its own pen)
            ADest.Pen.Style := psClear;
            // Draw the gradient
            DrawPolygonBrushRadialGradient(ARenderInfo, lPoints, lRect);
            // Draw border
            DrawPolygonBorderOnly(ARenderInfo, lPoints);
          end;
      end;
    end;
end;


{ TvAlignedDimension }

procedure TvAlignedDimension.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;

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
  txt: String;
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
    CalcEntityCanvasMinMaxXY(ARenderInfo, Points[0].X, Points[1].Y);
    if ADoDraw then ADest.Polygon(Points);
    // Right arrow
    Points[0] := Point(CoordToCanvasX(DimensionRight.X), CoordToCanvasY(DimensionRight.Y));
    Points[1] := Point(Points[0].X - 7, Points[0].Y - 3);
    Points[2] := Point(Points[0].X - 7, Points[0].Y + 3);
    CalcEntityCanvasMinMaxXY(ARenderInfo, Points[0].X, Points[2].Y);
    if ADoDraw then ADest.Polygon(Points);
    ADest.Brush.Style := bsClear;
    // Dimension text
    LowerDim.X := DimensionRight.X-DimensionLeft.X;
    ADest.Font.Size := 10;
    ADest.Font.Orientation := 0;
    ADest.Font.FPColor := AdjustColorToBackground(colBlack, ARenderInfo);
    txt := Format('%.1f', [LowerDim.X]);
    Points[0].X := CoordToCanvasX((DimensionLeft.X+DimensionRight.X)/2)-ADest.TextWidth(txt) div 2;
    Points[0].Y := CoordToCanvasY(DimensionLeft.Y);
    if ADoDraw then
      ADest.TextOut(Points[0].X, Points[0].Y-Round(ADest.Font.Size*1.5), txt);
    CalcEntityCanvasMinMaxXY_With2Points(ARenderInfo,
      Points[0].X, Points[0].Y - round(ADest.Font.Size*1.5),
      Points[0].X + ADest.TextWidth(txt), Points[0].Y);
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
    if ADoDraw then ADest.Polygon(Points);
    CalcEntityCanvasMinMaxXY(ARenderInfo, Points[1].X, Points[0].Y);
    // Lower arrow
    Points[0] := Point(CoordToCanvasX(LowerDim.X), CoordToCanvasY(LowerDim.Y));
    Points[1] := Point(Points[0].X + Round(AMulX), Points[0].Y + Round(AMulY*3));
    Points[2] := Point(Points[0].X - Round(AMulX), Points[0].Y + Round(AMulY*3));
    if ADoDraw then ADest.Polygon(Points);
    CalcEntityCanvasMinMaxXY(ARenderInfo, Points[2].X, Points[0].Y);
    ADest.Brush.Style := bsClear;
    // Dimension text
    LowerDim.Y := DimensionRight.Y-DimensionLeft.Y;
    if LowerDim.Y < 0 then LowerDim.Y := -1 * LowerDim.Y;
    ADest.Font.Size := 10;
    ADest.Font.Orientation := 900;
    ADest.Font.FPColor := AdjustColorToBackground(colBlack, ARenderInfo);
    txt := Format('%.1f', [LowerDim.Y]);
    Points[0].X := CoordToCanvasX(DimensionLeft.X);
    Points[0].Y := CoordToCanvasY((DimensionLeft.Y+DimensionRight.Y)/2)
      - sign(AMulY) * ADest.TextWidth(txt) div 2;
    if ADoDraw then
      ADest.TextOut(Points[0].X-Round(ADest.Font.Size*1.5), Points[0].Y, txt);
    ADest.Font.Orientation := 0;
    CalcEntityCanvasMinMaxXY_With2Points(ARenderInfo,
      Points[0].X - Round(ADest.Font.Size*1.5), Points[0].Y,
      Points[0].X, Points[0].Y + ADest.TextWidth(txt)
      );
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

procedure TvRadialDimension.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;

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

  if ADoDraw then
  begin
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
  end;

  SetLength(Points, 0);
end;

function TvRadialDimension.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr, lIsDiameterStr: string;
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

procedure TvArcDimension.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;

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
  lTriangleCenter, lTriangleCorner: T3DPoint;
  {$ifdef USE_LCL_CANVAS}
  ALCLDest: TCanvas absolute ADest;
  {$endif}
  txt: String;
begin
  ADest.Pen.FPColor := colYellow;//AdjustColorToBackground(colBlack, ARenderInfo);
  ADest.Pen.Width := 1;
  ADest.Pen.Style := psSolid;

  // Debug lines
  //ADest.Line(CoordToCanvasX(BaseLeft.X), CoordToCanvasY(BaseLeft.Y), CoordToCanvasX(DimensionLeft.X), CoordToCanvasY(DimensionLeft.Y));
  //ADest.Line(CoordToCanvasX(BaseRight.X), CoordToCanvasY(BaseRight.Y), CoordToCanvasX(DimensionRight.X), CoordToCanvasY(DimensionRight.Y));

  // Now the arc
  if ADoDraw then
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
  if ADoDraw then
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
  if ADoDraw then
    ADest.Brush.Style := bsClear;

  // Dimension text
  Points[0].X := CoordToCanvasX(TextPos.X);
  Points[0].Y := CoordToCanvasY(TextPos.Y);
  ADest.Font.Size := 10;
  ADest.Font.Orientation := 0;
  ADest.Font.FPColor := colYellow;//AdjustColorToBackground(colBlack, ARenderInfo);
  txt := Format('%.1fº', [ArcValue]);
  if ADoDraw then
    ADest.TextOut(Points[0].X, Points[0].Y-Round(ADest.Font.Size*1.5), txt);
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
  lStr: string;
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

destructor TvRasterImage.Destroy;
begin
  if Assigned(RasterImage) then RasterImage.Free;
  inherited Destroy;
end;

procedure TvRasterImage.CalculateBoundingBox(constref ARenderInfo: TvRenderInfo;
  out ALeft, ATop, ARight, ABottom: Double);
begin
  ALeft := X;
  ATop := Y;
  ARight := X + Width;
  ABottom := Y + Height;
end;

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
  AImage := TLazIntfImage.Create(AWidth, AHeight);
  AImage.SetRawImage(lRawImage);

  RasterImage := AImage;
{$endif}
end;

procedure TvRasterImage.CreateImageFromFile(AFilename: string);
{$ifdef USE_LCL_CANVAS}
var
  AImage: TLazIntfImage;
  lRawImage: TRawImage;
{$endif}
begin
{$ifdef USE_LCL_CANVAS}
  lRawImage.Init;
  lRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(0,0);
  lRawImage.CreateData(false);
  AImage := TLazIntfImage.Create(0,0);
  AImage.SetRawImage(lRawImage);
  AImage.LoadFromFile(AFilename);

  RasterImage := AImage;
{$endif}
end;

procedure TvRasterImage.CreateImageFromStream(AStream: TStream; Handler:TFPCustomImageReader);
{$ifdef USE_LCL_CANVAS}
var
  AImage: TLazIntfImage;
  lRawImage: TRawImage;
{$endif}
begin
{$ifdef USE_LCL_CANVAS}
  lRawImage.Init;
  lRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(0,0);
  lRawImage.CreateData(false);
  AImage := TLazIntfImage.Create(0,0);
  AImage.SetRawImage(lRawImage);
  AImage.LoadFromStream(AStream, Handler);

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

procedure TvRasterImage.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  lFinalX, lFinalY, lFinalW, lFinalH: Integer;
  {$ifdef USE_LCL_CANVAS}
  lBitmap: TBitmap;
  lMemoryStream: TMemoryStream;
  lImageWriter: TFPWriterBMP;
  {$endif}
begin
  if (RasterImage = nil) then Exit;
  if (RasterImage.Width = 0) or (RasterImage.Height = 0) then Exit;

  lFinalX := CoordToCanvasX(X);
  lFinalY := CoordToCanvasY(Y);

  {$ifdef USE_LCL_CANVAS}
  lBitmap := TBitmap.Create;
  lMemoryStream := TMemoryStream.Create;
  lImageWriter := TFPWriterBMP.Create;
  try
    // Previous try, but didn't work for some particular PNG images =(
    // For example: qr_www_lazarus_freepascal_org.svg
    // The image appeared corrupted in Qt, as if with wrong pixel format =(
    // It also didn't work in Gtk at all due to not matching Gdk format =(
    // But if it worked it would have been faster =)
    // Old code:
    // lBitmap.LoadFromIntfImage(TLazIntfImage(RasterImage));
    // New code:
    RasterImage.SaveToStream(lMemoryStream, lImageWriter);
    lMemoryStream.Position := 0;
    lBitmap.LoadFromStream(lMemoryStream);

    // without stretch support
    //TCanvas(ADest).Draw(lFinalX, lFinalY, lBitmap);

    // with stretch support
    lFinalW := Round(Width * AMulX);
    if lFinalW < 0 then lFinalW := lFinalW * -1;
    lFinalH := Round(Height * AMulY);
    if lFinalH < 0 then lFinalH := lFinalH * -1;
    if ADoDraw then
      TCanvas(ADest).StretchDraw(Bounds(lFinalX, lFinalY, lFinalW, lFinalH), lBitmap);
  finally
    lImageWriter.Free;
    lMemoryStream.Free;
    lBitmap.Free;
  end;
  {$endif}

  CalcEntityCanvasMinMaxXY(ARenderInfo, lFinalX, lFinalY);
  CalcEntityCanvasMinMaxXY(ARenderInfo, lFinalX+lFinalW, lFinalY+lFinalH);

  //ADest.Draw(lFinalX, lFinalY, RasterImage); doesnt work
end;

function TvRasterImage.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
var
  lStr: string;
begin
  Result := inherited GenerateDebugTree(ADestRoutine, APageItem);
  // Add the debug info in a sub-item
  if RasterImage <> nil then
  begin
    lStr := Format('[TvRasterImage] Width=%f Height=%f RasterImage.Width=%d RasterImage.Height=%d AltText=%s',
      [Width, Height, RasterImage.Width, RasterImage.Height, AltText]);
    ADestRoutine(lStr, Result);
  end;
end;

{ TvArrow }

procedure TvArrow.CalculateBoundingBox(constref ARenderInfo: TvRenderInfo; out
  ALeft, ATop, ARight, ABottom: Double);
begin

end;

procedure TvArrow.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;

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
  ApplyPenToCanvas(ARenderInfo);
  ApplyBrushToCanvas(ARenderInfo);

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
  if ADoDraw then ADest.Polygon(lPoints);
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

procedure TvFormulaElement.PositionSubparts(constref ARenderInfo: TvRenderInfo;
  ABaseX, ABaseY: Double);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  //
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

      Self.Formula.PositionSubparts(ARenderInfo, Self.Left + lCentralizeFactor, Self.Top);
      Self.AdjacentFormula.PositionSubparts(ARenderInfo, Self.Left + lCentralizeFactorAdj, Self.Top - Self.Formula.Height - 3);
    end;
    fekRoot:
    begin
      // Give a factor for the root drawing
      Self.Formula.PositionSubparts(ARenderInfo, Self.Left + 10, Self.Top);
    end;
    fekPower:
    begin
      Self.Formula.PositionSubparts(ARenderInfo, Self.Left, Self.Top);
      Self.AdjacentFormula.PositionSubparts(ARenderInfo, Self.Left + Self.Formula.Width, Self.Top);
    end;
    fekSubscript:
    begin
      Self.Formula.PositionSubparts(ARenderInfo, Self.Left, Self.Top);
      Self.AdjacentFormula.PositionSubparts(ARenderInfo, Self.Left + Self.Formula.Width, Self.Top - Self.Formula.Height / 2);
    end;
    fekSummation:
    begin
      // main/bottom formula
      Self.Formula.PositionSubparts(ARenderInfo, Self.Left, Self.Top - 30);
      // top formula
      Self.AdjacentFormula.PositionSubparts(ARenderInfo, Self.Left, Self.Top);
    end;
    fekFormula:
    begin
      Self.Formula.PositionSubparts(ARenderInfo, Self.Left, Self.Top);
    end;
  end;
end;

procedure TvFormulaElement.Render(var ARenderInfo: TvRenderInfo;
  ADoDraw: Boolean);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;

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

  if ADoDraw then
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
        Formula.Render(ARenderInfo, ADoDraw);
        AdjacentFormula.Render(ARenderInfo, ADoDraw);

        // Division line
        lPt[0].X := CoordToCanvasX(Formula.Left);
        lPt[1].X := CoordToCanvasX(Formula.Left + Formula.Width);
        lPt[0].Y := CoordToCanvasY(Formula.Top - Formula.Height);
        lPt[1].Y := CoordToCanvasY(Formula.Top - Formula.Height);
        ADest.Line(lPt[0].X, lPt[0].Y, lPt[1].X, lPt[1].Y);
      end;
      fekRoot:
      begin
        Formula.Render(ARenderInfo, ADoDraw);

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
        Formula.Render(ARenderInfo, ADoDraw);
        // The superscripted power
        lOldFontSize := ADest.Font.Size;
        if lOldFontSize = 0 then ADest.Font.Size := 5
        else ADest.Font.Size := lOldFontSize div 2;
        AdjacentFormula.Render(ARenderInfo, ADoDraw);
        ADest.Font.Size := lOldFontSize;
      end;
      fekSubscript:
      begin
        Formula.Render(ARenderInfo, ADoDraw);
        // The subscripted item
        lOldFontSize := ADest.Font.Size;
        if lOldFontSize = 0 then ADest.Font.Size := 5
        else ADest.Font.Size := lOldFontSize div 2;
        AdjacentFormula.Render(ARenderInfo, ADoDraw);
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
        Formula.Render(ARenderInfo, ADoDraw);

        // Draw the top formula
        AdjacentFormula.Render(ARenderInfo, ADoDraw);
      end;
      fekFormula:
      begin
        // Draw the formula
        Formula.Render(ARenderInfo, ADoDraw);
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

// http://en.wikipedia.org/wiki/Shunting-yard_algorithm
class function TvFormulaElement.GetPrecedenceFromKind(
  AKind: TvFormulaElementKind): Byte;
begin
  Result := 0;
  case AKind of
  fekSubtraction, fekSum: Result := 2;
  fekMultiplication, fekFraction: Result := 3;
  //fekRoot,   // A root. For example sqrt(something). Number gives the root, usually 2, and inside it goes a Formula
  fekPower: Result := 4;
  end;
end;

// See http://en.wikipedia.org/wiki/Shunting-yard_algorithm
class function TvFormulaElement.IsLeftAssociativeFromKind(
  AKind: TvFormulaElementKind): Boolean;
begin
  Result := True;
  case AKind of
  fekPower: Result := False;
  end;
end;

{ TvFormula }

procedure TvFormula.CallbackDeleteElement(data, arg: pointer);
begin
  TvFormulaElement(data).Free;
end;

constructor TvFormula.Create(APage: TvPage);
begin
  inherited Create(APage);
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
  Result := TvFormulaElement(FElements.Items[0]);
  FCurIndex := 1;
end;

function TvFormula.GetNextElement: TvFormulaElement;
begin
  if FElements.Count <= FCurIndex then Exit(nil);
  Result := TvFormulaElement(FElements.Items[FCurIndex]);
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
      Result.Formula := TvFormula.Create(FPage);
      Result.AdjacentFormula := TvFormula.Create(FPage);
    end;
    fekRoot:
    begin
      Result.Formula := TvFormula.Create(FPage);
    end;
  end;
end;

// Based on:
// http://en.wikipedia.org/wiki/Shunting-yard_algorithm
procedure TvFormula.AddItemsByConvertingInfixToRPN(AInfix: TFPList);
var
  OperatorStack: TObjectStack;
  i: Integer;
  CurItem: TvFormulaElement;

  procedure PopFromStackIntoList(APopTopOperators: Boolean; APopUntilParenteses: Boolean);
  var
    lElement: TvFormulaElement;
    lAllowContinue: Boolean;
  begin
    while OperatorStack.Count > 0 do
    begin
      lElement := OperatorStack.Pop() as TvFormulaElement;

      // while there is an operator token, o2, at the top of the stack, and
      // either o1 is left-associative and its precedence is equal to that of o2,
      // or o1 has precedence less than that of o2,
      if APopTopOperators then
      begin
        if not (lElement.Kind in FormulaOperators) then Exit;

        lAllowContinue := TvFormulaElement.IsLeftAssociativeFromKind(lElement.Kind)
         and (TvFormulaElement.GetPrecedenceFromKind(lElement.Kind) =
              TvFormulaElement.GetPrecedenceFromKind(CurItem.Kind));
        lAllowContinue := lAllowContinue or
          (TvFormulaElement.GetPrecedenceFromKind(lElement.Kind) >
          TvFormulaElement.GetPrecedenceFromKind(CurItem.Kind));
        if not lAllowContinue then Exit;
      end;

      if APopUntilParenteses and (lElement.Kind = fekParentesesOpen) then Exit;

      FElements.Add(lElement);
    end;
  end;

begin
  Clear();

  OperatorStack := TObjectStack.Create;
  try
    for i := 0 to AInfix.Count-1 do
    begin
      CurItem := TvFormulaElement(AInfix.Items[i]);
      case CurItem.Kind of
      fekVariable:
      begin
        FElements.Add(CurItem);
      end;
      fekSubtraction, fekMultiplication, fekSum, fekFraction:
      begin
        PopFromStackIntoList(True, False);
        OperatorStack.Push(CurItem);
      end;
      fekParentesesOpen:
      begin
        OperatorStack.Push(CurItem);
      end;
      freParentesesClose:
      begin
        PopFromStackIntoList(False, True);
      end;
      end;
    end;

    PopFromStackIntoList(True, False);
  finally
    OperatorStack.Free;
  end;
end;

procedure TvFormula.AddItemsByConvertingInfixStringToRPN(AStr: string);
var
  lInfix: TFPList;
begin
  lInfix := TFPList.Create;
  try
    TokenizeInfixString(AStr, lInfix);
    AddItemsByConvertingInfixToRPN(lInfix);
  finally
    lInfix.Free;
  end;
end;

procedure TvFormula.TokenizeInfixString(AStr: string; AOutput: TFPList);
const
  Str_Space: Char = ' ';

  procedure AddToken(AStr: string);
  var
    lToken: TvFormulaElement;
    lStr: string;
    FPointSeparator: TFormatSettings;
  begin
    FPointSeparator := DefaultFormatSettings;
    FPointSeparator.DecimalSeparator := '.';
    FPointSeparator.ThousandSeparator := '#';// disable the thousand separator

    lStr := Trim(AStr);
    if lStr = '' then Exit;

    lToken := TvFormulaElement.Create;

    // Moves
    case lStr[1] of
    '*': lToken.Kind := fekMultiplication;
    '/': lToken.Kind := fekFraction;
    '+': lToken.Kind := fekSum;
    '-': lToken.Kind := fekSubtraction;
    '(': lToken.Kind := fekParentesesOpen;
    ')': lToken.Kind := freParentesesClose;
    else
      lToken.Kind := fekVariable;
      lToken.Number := StrToFloat(AStr, FPointSeparator);
    end;

    AOutput.Add(lToken);
  end;

var
  i: Integer;
  lTmpStr: string = '';
  lState: Integer;
  lCurChar: Char;
begin
  lState := 0;

  i := 1;
  while i <= Length(AStr) do
  begin
    case lState of
    0: // Adding to the tmp string
    begin
      lCurChar := AStr[i];
      if lCurChar = Str_Space then
      begin
        //lState := 1;
        AddToken(lTmpStr);
        lTmpStr := '';
      end
      else if lCurChar in ['/', '*', '+', '-', '(', ')'] then
      begin
        if lTmpStr <> '' then AddToken(lTmpStr);
        lTmpStr := '';
        lState := 0;
        AddToken(lCurChar);
      end
      else
      begin
        lTmpStr := lTmpStr + lCurChar;
      end;
    end;
    end;

    Inc(i);
  end;

  // If there is a token still to be added, add it now
  if (lState = 0) and (lTmpStr <> '') then AddToken(lTmpStr);
end;

// The formula must be in RPN for this to work
function TvFormula.CalculateRPNFormulaValue: Double;
var
  lOperand_A, lOperand_B, CurElement: TvFormulaElement;
  i: Integer;
begin
  lOperand_A := nil;
  lOperand_B := nil;
  Result := 0;
  for i := 0 to FElements.Count-1 do
  begin
    CurElement := TvFormulaElement(FElements.Items[i]);
    case CurElement.Kind of
    fekVariable:
    begin
      if lOperand_A = nil then lOperand_A := CurElement
      else lOperand_B := CurElement;
    end;
    fekSubtraction:
    begin
      lOperand_A.Number := lOperand_A.Number - lOperand_B.Number;
      lOperand_B := nil;
    end;
    fekMultiplication:
    begin
      lOperand_A.Number := lOperand_A.Number * lOperand_B.Number;
      lOperand_B := nil;
    end;
    fekSum:
    begin
      lOperand_A.Number := lOperand_A.Number + lOperand_B.Number;
      lOperand_B := nil;
    end;
    fekFraction:
    begin
      lOperand_A.Number := lOperand_A.Number / lOperand_B.Number;
      lOperand_B := nil;
    end;
    end;
  end;

  Result := lOperand_A.Number;
end;

procedure TvFormula.Clear;
begin
  inherited Clear;
  FElements.ForEachCall(@CallbackDeleteElement, nil);
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

procedure TvFormula.PositionSubparts(constref ARenderInfo: TvRenderInfo;
  ABaseX, ABaseY: Double);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  //
  lElement: TvFormulaElement;
  lPosX: Double = 0;
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

    lElement.PositionSubparts(ARenderInfo, ABaseX, ABaseY);

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

procedure TvFormula.CalculateBoundingBox(constref ARenderInfo: TvRenderInfo;
  out ALeft, ATop, ARight, ABottom: Double);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
begin
  ALeft := X;
  ATop := Y;
  ARight := CalculateWidth(ADest);
  if ADest = nil then ABottom := CalculateHeight(ADest) * 15
  else ABottom := CalculateHeight(ADest) * TCanvas(ADest).TextHeight('Źç');
  ARight := X + ARight;
  ABottom := Y + ABottom;
end;

procedure TvFormula.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  lElement: TvFormulaElement;
begin
  inherited Render(ARenderInfo, ADoDraw);

  // First position all elements
  PositionSubparts(ARenderInfo, Left, Top);

  // Now draw them all
  lElement := GetFirstElement();
  if lElement = nil then Exit;
  while lElement <> nil do
  begin
    lElement.Render(ARenderInfo, ADoDraw);

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

constructor TvEntityWithSubEntities.Create(APage: TvPage);
begin
  inherited Create(APage);
  FElements := TFPList.Create;
end;

destructor TvEntityWithSubEntities.Destroy;
var
  i: Integer;
begin
  for i:= FElements.Count-1 downto 0 do
    TvEntity(FElements[i]).Free;
  FElements.Free;
  inherited Destroy;
end;

function TvEntityWithSubEntities.GetFirstEntity: TvEntity;
begin
  if FElements.Count = 0 then Exit(nil);
  Result := TvEntity(FElements.Items[0]);
  FCurIndex := 1;
end;

function TvEntityWithSubEntities.GetNextEntity: TvEntity;
begin
  if FElements.Count <= FCurIndex then Exit(nil);
  Result := TvEntity(FElements.Items[FCurIndex]);
  Inc(FCurIndex);
end;

function TvEntityWithSubEntities.GetEntitiesCount: Integer;
begin
  Result := FElements.Count;
end;

function TvEntityWithSubEntities.GetEntity(AIndex: Integer): TvEntity;
begin
  Result := TvEntity(FElements.Items[AIndex]);
end;

function TvEntityWithSubEntities.AddEntity(AEntity: TvEntity): Integer;
begin
  //AEntity.Parent := Self;
  AEntity.SetPage(Self.FPage);
  Result := FElements.Add(AEntity);
end;

function TvEntityWithSubEntities.GetEntityIndex(AEntity: TvEntity): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FElements.Count-1 do
    if TvEntity(FElements.Items[i]) = AEntity then Exit(i);
end;

function TvEntityWithSubEntities.DeleteEntity(AIndex: Cardinal): Boolean;
var
  lEntity: TvEntity;
begin
  lEntity := TvEntity(FElements.Items[AIndex]);
  FElements.Remove(lEntity);
  lEntity.Free;
  Result := True;
end;

function TvEntityWithSubEntities.RemoveEntity(AEntity: TvEntity;
  AFreeAfterRemove: Boolean): Boolean;
var
  lIndex: Integer;
begin
  Result := False;
  lIndex := FindEntityWithReference(AEntity);
  if lIndex < 0 then Exit;
  if AFreeAfterRemove then DeleteEntity(lIndex)
  else FElements.Remove(AEntity);
  Result := True;
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
  FElements.ForEachCall(@CallbackDeleteElement, nil);
  FElements.Clear;
end;

procedure TvEntityWithSubEntities.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  lEntity: TvEntity;
  rinfo: TvRenderInfo;
  isFirst: Boolean;
begin
  rinfo := ARenderInfo;
  inherited Render(ARenderInfo, ADoDraw);
  isFirst := true;
  lEntity := GetFirstEntity();
  while lEntity <> nil do
  begin
    {$IFDEF FPVECTORIAL_DEBUG_BLOCKS}
    //WriteLn(Format('[TvInsert.Render] Name=%s Block=%s Entity=%s EntityXY=%f | %f BlockXY=%f | %f InsertXY=%f | %f',
    //  [Name, Block.Name, lEntity.ClassName, lEntity.X, lEntity.Y, Block.X, Block.Y, X, Y]));
    {$ENDIF}

    // Render
    lEntity.Render(ARenderInfo, ADoDraw);

    if isFirst then
    begin
      rinfo := ARenderInfo;
      isFirst := false;
    end else
      CalcEntityCanvasMinMaxXY_With2Points(rinfo,
        ARenderInfo.EntityCanvasMinXY.X,
        ARenderInfo.EntityCanvasMinXY.Y,
        ARenderInfo.EntityCanvasMaxXY.X,
        ARenderInfo.EntityCanvasMaxXY.Y
      );

    {$ifdef FPVECTORIAL_AUTOFIT_DEBUG}
    if AutoFitDebug <> nil then AutoFitDebug.Add(Format('=[%s] MinX=%d MinY=%d MaxX=%d MaxY=%d',
      [lEntity.ClassName, ARenderInfo.EntityCanvasMinXY.X, ARenderInfo.EntityCanvasMinXY.Y,
       ARenderInfo.EntityCanvasMaxXY.X, ARenderInfo.EntityCanvasMaxXY.Y]));
    {$endif}

    lEntity := GetNextEntity();
  end;

  ARenderInfo := rinfo;
end;

function TvEntityWithSubEntities.GenerateDebugTree(
  ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer;
var
  lStr: string;
  lCurEntity: TvEntity;
begin
  lStr := Format('[%s] Name="%s" X=%f Y=%f' + FExtraDebugStr,
    [Self.ClassName, Self.Name, X, Y]);

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

function TvEntityWithSubEntities.FindEntityWithReference(AEntity: TvEntity
  ): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FElements.Count - 1 do
  begin
    if TvEntity(FElements.Items[i]) = AEntity then Exit(i);
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

constructor TvInsert.Create(APage: TvPage);
begin
  inherited Create(APage);
  Style := TvStyle.Create;
end;

destructor TvInsert.Destroy;
begin
  FreeAndNil(Style);
  inherited Destroy;
end;

procedure TvInsert.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  OldForceRenderBlock: Boolean;
begin
  inherited Render(ARenderInfo, ADoDraw);
  if InsertEntity = nil then Exit;
  // If we are inserting a block, make sure it will render its contents
  OldForceRenderBlock := ARenderInfo.ForceRenderBlock;
  ARenderInfo.ForceRenderBlock := True;
  // If necessary rotate the canvas
  if RotationAngle <> 0 then
  begin
    InsertEntity.Rotate(RotationAngle, Make3DPoint(0, 0));
  end;
  // Alter the position of the elements to consider the positioning of the BLOCK and of the INSERT
  InsertEntity.Move(X, Y);
  Style.ApplyOverFromPen(@Pen, SetElements);
  Style.ApplyOverFromBrush(@Brush, SetElements);
  Style.ApplyOverFromFont(@Font, SetElements);
  Style.ApplyIntoEntity(InsertEntity);
  // Render
  InsertEntity.Render(ARenderInfo, ADoDraw);
  // Change them back
  InsertEntity.Move(-X, -Y);
  // And unrotate it back again
  if RotationAngle <> 0 then
  begin
    InsertEntity.Rotate(-1 * RotationAngle, Make3DPoint(0, 0));
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

procedure TvBlock.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  lEntity: TvEntity;
begin
  // blocks are invisible by themselves
  //inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY, ADoDraw);
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
    lEntity.Render(ARenderInfo, ADoDraw);
    // Change them back
    lEntity.Move(-X, -Y);

    lEntity := GetNextEntity();
  end;
end;

{ TvParagraph }

constructor TvParagraph.Create(APage: TvPage);
begin
  inherited Create(APage);
end;

destructor TvParagraph.Destroy;
begin
  inherited Destroy;
end;

function TvParagraph.AddText(AText: string): TvText;
begin
  Result := TvText.Create(FPage);
  Result.Value.Text := AText;
  AddEntity(Result);
end;

function TvParagraph.AddCurvedText(AText: string): TvCurvedText;
begin
  Result := TvCurvedText.Create(FPage);
  Result.Value.Text := AText;
  AddEntity(Result);
end;

function TvParagraph.AddField(AKind: TvFieldKind): TvField;
begin
  Result := TvField.Create(FPage);
  Result.Kind := AKind;
  AddEntity(Result);
end;

function TvParagraph.AddRasterImage: TvRasterImage;
begin
  Result := TvRasterImage.Create(FPage);
  AddEntity(Result);
end;

function TvParagraph.AddEmbeddedVectorialDoc: TvEmbeddedVectorialDoc;
begin
  Result := TvEmbeddedVectorialDoc.Create(FPage);
  AddEntity(Result);
end;

procedure TvParagraph.CalculateBoundingBox(constref ARenderInfo: TvRenderInfo;
  out ALeft, ATop, ARight, ABottom: Double);
var
  lEntity: TvEntity;
  lCurWidth: Double = 0.0;
  lCurHeight: Double = 0.0;
  lLeft, lTop, lRight, lBottom: Double;
  lText: TvText absolute lEntity;
  {$ifdef USE_LCL_CANVAS}
  ACanvas: TCanvas absolute ARenderInfo.Canvas;
  {$endif}
begin
  ALeft := X;
  ATop := Y;
  ARight := X;
  ABottom := Y;

  lEntity := GetFirstEntity();
  while lEntity <> nil do
  begin
    if Style <> nil then
      Style.ApplyIntoEntity(lEntity);
    lEntity.CalculateBoundingBox(ARenderInfo, lLeft, lTop, lRight, lBottom);
    lCurWidth := lCurWidth + (lRight - lLeft);
    lCurHeight := Max(lCurHeight, Abs(lTop - lBottom));
    lEntity := GetNextEntity();
  end;

  ALeft := X;
  ATop := Y - lCurHeight;
  ARight := X + lCurWidth;
  ABottom := Y;
end;

function TvParagraph.TryToSelect(APos: TPoint; var ASubpart: Cardinal; ASnapFlexibility: Integer = 5): TvFindEntityResult;
begin
  Result:=inherited TryToSelect(APos, ASubpart, ASnapFlexibility);
end;

procedure TvParagraph.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;
  //
  lCurWidth: Double = 0.0;
  lLeft, lTop, lRight, lBottom: Double;
  OldTextX: Double = 0.0;
  OldTextY: Double = 0.0;
  lEntity: TvEntity;
  lText: TvText; // absolute lEntity;
  lPrevText: TvText = nil;
  lFirstText: Boolean = True;
  lResetOldStyle: Boolean = False;
  lEntityRenderInfo: TvRenderInfo;
  CurX, CurY, lHeight_px: Integer;
  lFeatures: TvEntityFeatures;
begin
  InitializeRenderInfo(ARenderInfo, Self);
  InitializeRenderInfo(lEntityRenderInfo, Self);

  // Don't call inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY, ADoDraw);
  lEntity := GetFirstEntity();
  while lEntity <> nil do
  begin
    lFeatures := lEntity.GetEntityFeatures(ARenderInfo);
    lHeight_px := 0;
    if YPos_NeedsAdjustment_DelFirstLineBodyHeight then
      lHeight_px := -1 * lFeatures.FirstLineHeight;
    if (lFeatures.DrawsUpwardHeightAdjustment > 0) then
      lHeight_px := lFeatures.DrawsUpwardHeightAdjustment - lFeatures.FirstLineHeight;

    if lEntity is TvText then
    begin
      lText := TvText(lEntity);  // cannot debug with "absolute"...

      // Set the text style if not already set
      lResetOldStyle := False;
      if (Style <> nil) and (lText.Style = nil) then
      begin
        lText.Style := Style;
        lResetOldStyle := True
      end;

      // Direct text position setting resets the auto-positioning
      if (OldTextX <> lText.X) or (OldTextY <> lText.Y) then
      begin
        lCurWidth := 0;
        lFirstText := True;
      end;

      OldTextX := lText.X;
      OldTextY := lText.Y;
      CurX := CoordToCanvasX(lText.X + X + lCurWidth, ADestX, AMulX);
      CurY := CoordToCanvasY(lText.Y + Y, ADestY, AMulY);
      lText.X := 0;
      lText.Y := 0;
      CurY += lHeight_px;
      lText.Render_Use_NextText_X := not lFirstText;
      if lText.Render_Use_NextText_X then
        lText.Render_NextText_X := lPrevText.Render_NextText_X;

      // Style apply
      if Style <> nil then
        Style.ApplyIntoEntity(lText);

      CopyAndInitDocumentRenderInfo(lEntityRenderInfo, ARenderInfo);
      lEntityRenderInfo.DestX := CurX;
      lEntityRenderInfo.DestY := CurY;
      lText.Render(lEntityRenderInfo, ADoDraw);
      lText.CalculateBoundingBox(ARenderInfo, lLeft, lTop, lRight, lBottom);
      lCurWidth := lCurWidth + Abs(lRight - lLeft);
      lFirstText := False;
      lPrevText := lText;

      lText.X := OldTextX;
      lText.Y := OldTextY;
      if lResetOldStyle then
        TvText(lEntity).Style := nil;
    end
    else
    begin
      OldTextX := lEntity.X;
      OldTextY := lEntity.Y;
      lEntity.X := lEntity.X + X + lCurWidth;
      lEntity.Y := lEntity.Y + Y;

      CopyAndInitDocumentRenderInfo(lEntityRenderInfo, ARenderInfo);
      lEntityRenderInfo.Canvas := ADest;
      lEntityRenderInfo.DestX := ADestX;
      lEntityRenderInfo.DestY := ADestY + lHeight_px;
      lEntityRenderInfo.MulX := AMulX;
      lEntityRenderInfo.MulY := AMulY;
      lEntity.Render(lEntityRenderInfo, ADoDraw);

      lEntity.X := OldTextX;
      lEntity.Y := OldTextY;
    end;

    MergeRenderInfo(lEntityRenderInfo, ARenderInfo);

    lEntity := GetNextEntity();
  end;
end;

function TvParagraph.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
begin
  Result:=inherited GenerateDebugTree(ADestRoutine, APageItem);
end;

{ TvList }

constructor TvList.Create(APage: TvPage);
begin
  inherited Create(APage);

  Parent := Nil;
end;

destructor TvList.Destroy;
begin
  inherited Destroy;
end;

function TvList.AddParagraph(ASimpleText: string): TvParagraph;
begin
  Result := TvParagraph.Create(FPage);
  // TODO:
//  if FPage <> nil then
//    Result.ListStyle := FPage.FOwner.GetListStyleByLevel(ALevel);
  if ASimpleText <> '' then
    Result.AddText(ASimpleText);
  AddEntity(Result);
end;

function TvList.AddList: TvList;
begin
  Result := TvList.Create(FPage);

  Result.Style := Style;
  Result.ListStyle := ListStyle;
  Result.Parent := Self;

  AddEntity(Result);
end;

function TvList.GetLevel: Integer;
var
  oListItem : TvList;
begin
  Result := 0;

  oListItem := Parent;

  while (oListItem<>Nil) do
  begin
    oListItem := oListItem.Parent;

    inc(Result);
  end;
end;

function TvList.GetBulletSize: Double;
begin
  Result := Font.Size;
  if Result = 0 then Result := 10;
  Result := Result * 1.5; // for upper/lower spacing
end;

procedure TvList.DrawBullet(ADest: TFPCustomCanvas;
  var ARenderInfo: TvRenderInfo; ALevel: Integer; AX, AY: Double;
  ADestX: Integer; ADestY: Integer; AMulX: Double; AMulY: Double;
  ADoDraw: Boolean);

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  lBulletSpacing: Double;
  lLevel: Integer;
begin
  lBulletSpacing := GetBulletSize() / 2;
  ADest.Pen.Style := psSolid;
  ADest.Pen.FPColor := colBlack;
  ADest.Brush.Style := bsSolid;
  ADest.Brush.FPColor := colBlack;
  lLevel := GetLevel();

  // level 0  - filled circle
  // level 1  - circle with empty filling
  // lebel 2+ - filled square
  case lLevel of
    1: ADest.Brush.Style := bsClear;
  end;

  case lLevel of
  0, 1:
  begin
    ADest.Ellipse(CoordToCanvasX(AX + lBulletSpacing), CoordToCanvasY(AY + lBulletSpacing*4), // ToDo: Figure out why this needs to be like that for curved_text.html to render well
      CoordToCanvasX(AX + lBulletSpacing*2), CoordToCanvasY(AY + lBulletSpacing*5));
  end;
  else
    ADest.Rectangle(CoordToCanvasX(AX + lBulletSpacing), CoordToCanvasY(AY + lBulletSpacing*4),
      CoordToCanvasX(AX + lBulletSpacing*2), CoordToCanvasY(AY + lBulletSpacing*5));
  end;
end;

procedure TvList.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean = True);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  lEntity: TvEntity;
  lPara: TvParagraph absolute lEntity;
  lList: TvList absolute lEntity;
  lEntityRenderInfo: TvRenderInfo;
  CurX, CurY, lBulletSize, lItemHeight: Double;
  lHeight_px: Integer;
begin
  InitializeRenderInfo(ARenderInfo, Self);

  // Don't call inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY, ADoDraw);

  lBulletSize := GetBulletSize() * Abs(AMulX);
  CurX := X + lBulletSize;
  CurY := Y;

  lEntity := GetFirstEntity();
  while lEntity <> nil do
  begin
    // handle both directions of drawing
    lHeight_px := 0;
    lEntity.CalculateHeightInCanvas(ARenderInfo, lHeight_px);

    // draw the bullet (if necessary)
    if lEntity is TvParagraph then
    begin
      DrawBullet(ADest, lEntityRenderInfo, GetLevel(),
        X, CurY, ADestX, ADestY+lHeight_px, AMulX, AMulY, ADoDraw);
    end;

    // attempt to centralize the item
    lEntity.X := CurX;
    lEntity.Y := CurY;
    lItemHeight := lEntity.GetHeight(ARenderInfo);
    if lItemHeight < lBulletSize then
    begin
      lItemHeight := lBulletSize;
      lEntity.Y := lEntity.CentralizeY_InHeight(ARenderInfo, lBulletSize);
    end;

    // draw the item
    lEntityRenderInfo.Canvas := ADest;
    lEntityRenderInfo.DestX := ADestX;
    lEntityRenderInfo.DestY := ADestY+lHeight_px;
    lEntityRenderInfo.MulX := AMulX;
    lEntityRenderInfo.MulY := AMulY;
    lEntity.Render(lEntityRenderInfo, ADoDraw);

    // prepare next loop iteration
    MergeRenderInfo(lEntityRenderInfo, ARenderInfo);
    CurY := CurY + lItemHeight;
    lEntity := GetNextEntity();
  end;
end;

{ TvRichText }

constructor TvRichText.Create(APage: TvPage);
begin
  inherited Create(APage);
end;

destructor TvRichText.Destroy;
begin
  inherited Destroy;
end;

function TvRichText.AddParagraph: TvParagraph;
begin
  Result := TvParagraph.Create(FPage);
  AddEntity(Result);
end;

function TvRichText.AddList: TvList;
begin
  Result := TvList.Create(FPage);
  AddEntity(Result);
end;

function TvRichText.AddTable: TvTable;
begin
  Result := TvTable.Create(FPage);
  AddEntity(Result);
end;

function TvRichText.AddEmbeddedVectorialDoc: TvEmbeddedVectorialDoc;
begin
  Result := TvEmbeddedVectorialDoc.Create(FPage);
  AddEntity(Result);
end;

function TvRichText.AddRasterImage: TvRasterImage;
begin
  Result := TvRasterImage.Create(FPage);
  AddEntity(Result);
end;

// this function is for descendents to override with a different behavior such as TvTableCell
procedure TvRichText.GetEffectiveCellSpacing(out ATopSpacing, ALeftSpacing, ARightSpacing, ABottomSpacing: Double);
begin
  ATopSpacing := SpacingTop;
  ALeftSpacing := SpacingLeft;
  ARightSpacing := SpacingRight;
  ABottomSpacing := SpacingBottom;
end;

function TvRichText.CalculateCellHeight_ForWidth(constref ARenderInfo: TvRenderInfo; AWidth: Double): Double;
var
  lCurHeight: Double = 0.0;
  lLeft, lTop, lRight, lBottom, lSpacingTop, lSpacingBottom, lTmp: Double;
  lEntity: TvEntity;
  //lParagraph: TvParagraph absolute lEntity;
begin
  Result := 0;
  lEntity := GetFirstEntity();
  while lEntity <> nil do
  begin
    lEntity.X := X;
    lEntity.Y := Y + Result;
    lEntity.CalculateBoundingBox(ARenderInfo, lLeft, lTop, lRight, lBottom);
    Result := Result + (lBottom - lTop);

    lEntity := GetNextEntity();
  end;

  GetEffectiveCellSpacing(lTmp, lSpacingTop, lTmp, lSpacingBottom);
  Result := Result + lSpacingTop + lSpacingBottom;
end;

function TvRichText.CalculateMaxNeededWidth(constref ARenderInfo: TvRenderInfo): Double;
var
  lLeft, lTop, lRight, lBottom: Double;
  lEntity: TvEntity;
  //lParagraph: TvParagraph absolute lEntity;
begin
  Result := 0;

  // if the width is not yet known, calculate it
  if Width <= 0 then
  begin
    lEntity := GetFirstEntity();
    while lEntity <> nil do
    begin
      lEntity.X := X;
      lEntity.Y := Y + Result;
      lEntity.CalculateBoundingBox(ARenderInfo, lLeft, lTop, lRight, lBottom);
      Result := Max(Result, (lRight - lLeft));

      lEntity := GetNextEntity();
    end;
  end;

  Result := Result + SpacingLeft + SpacingRight;
end;

function TvRichText.TryToSelect(APos: TPoint; var ASubpart: Cardinal; ASnapFlexibility: Integer = 5): TvFindEntityResult;
begin
  Result:=inherited TryToSelect(APos, ASubpart, ASnapFlexibility);
end;

procedure TvRichText.Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean);
var
  lCurHeight: Double = 0.0;
  lLeft, lTop, lRight, lBottom: Double;
  lHeight_px: Integer;
  lEntity: TvEntity;
  //lParagraph: TvParagraph absolute lEntity;
  lEntityRenderInfo: TvRenderInfo;
begin
  InitializeRenderInfo(ARenderInfo, Self);

  // Don't call inherited Render(ADest, ARenderInfo, ADestX, ADestY, AMulX, AMulY, ADoDraw);
  lEntity := GetFirstEntity();
  while lEntity <> nil do
  begin
    lEntity.X := X;
    lEntity.Y := Y + lCurHeight;
    lHeight_px := lEntity.GetEntityFeatures(ARenderInfo).DrawsUpwardHeightAdjustment;
    CopyAndInitDocumentRenderInfo(lEntityRenderInfo, ARenderInfo);
    lEntityRenderInfo.Canvas := ARenderInfo.Canvas;
    lEntityRenderInfo.DestX := ARenderInfo.DestX;
    lEntityRenderInfo.DestY := ARenderInfo.DestY + lHeight_px;
    lEntityRenderInfo.Canvas := ARenderInfo.Canvas;
    lEntityRenderInfo.Canvas := ARenderInfo.Canvas;
    lEntity.Render(lEntityRenderInfo, ADoDraw);
    lEntity.CalculateBoundingBox(ARenderInfo, lLeft, lTop, lRight, lBottom);
    lCurHeight := lCurHeight + (lBottom - lTop);

    lEntity := GetNextEntity();
    MergeRenderInfo(lEntityRenderInfo, ARenderInfo);
  end;
end;

function TvRichText.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
begin
  Result:=inherited GenerateDebugTree(ADestRoutine, APageItem);
end;

{ TvPage }

procedure TvPage.InitializeRenderInfo(out ARenderInfo: TvRenderInfo;
  ACanvas: TFPCustomCanvas; AEntity: TvEntity);
begin
  FillChar(ARenderInfo, SizeOf(TvRenderInfo), #0);
  TvEntity.InitializeRenderInfo(ARenderInfo, AEntity, True);
  ARenderInfo.Canvas := ACanvas;
  ARenderInfo.Page := Self;
  ARenderInfo.Renderer := FOwner.FRenderer;
end;

constructor TvPage.Create(AOwner: TvVectorialDocument);
begin
  inherited Create;
  FOwner := AOwner;
  AdjustPenColorToBackground := true;
  System.FillChar(RenderInfo, SizeOf(RenderInfo), #0);
  TvEntity.InitializeRenderInfo(RenderInfo, nil, True);
end;

destructor TvPage.Destroy;
begin
  TvEntity.FinalizeRenderInfo(RenderInfo);
  inherited Destroy;
end;

procedure TvPage.Assign(ASource: TvPage);
begin

end;

procedure TvPage.SetPageFormat(AFormat: TvPageFormat);
begin
  case AFormat of
  vpA4:
  begin
    Width := 210;
    Height := 297;
  end;
  else
    Width := 210;
    Height := 297;
  end;
end;

procedure TvPage.CalculateDocumentSize;
var
  i: Integer;
  lCurEntity: TvEntity;
  lLeft, lTop, lRight, lBottom: Double;
  lBmp: TBitmap;
  lRenderInfo: TvRenderInfo;
begin
  MinX := 0;
  MinY := 0;
  MinZ := 0;
  MaxX := 0;
  MaxY := 0;
  MaxZ := 0;
  lBmp := TBitmap.Create;
  for i := 0 to GetEntitiesCount() -1 do
  begin
    lCurEntity := GetEntity(i);
    lRenderInfo.Canvas := lBmp.Canvas;
    lCurEntity.CalculateBoundingBox(lRenderInfo, lLeft, lTop, lRight, lBottom);
    MinX := Min(MinX, lLeft);
    MinY := Min(MinY, lTop);
    MaxX := Max(MaxX, lRight);
    MaxY := Max(MaxY, lBottom);
  end;
  lBmp.Free;
  Width := MaxX - MinX;
  Height := MaxY - MinY;
end;

procedure TvPage.AutoFit(ADest: TFPCustomCanvas; AWidth, AHeight, ARenderHeight: Integer;
  out ADeltaX, ADeltaY: Integer; out AZoom: Double);
var
  lCurEntity: TvEntity;
  lLeft, lTop, lWidth, lHeight: Integer;
  lMinX, lMinY, lMaxX, lMaxY, lNaturalHeightDiff: Integer;
  lZoomFitX, lZoomFitY, lNaturalMulY: Double;

  function CalculateAllEntitySizes(): Boolean;
  var
    i: Integer;
    lRenderInfo: TvRenderInfo;
  begin
    Result := True;

    lMinX := High(Integer);
    lMinY := High(Integer);
    lMaxX := Low(Integer);
    lMaxY := Low(Integer);

    if Self is TvVectorialPage then
    begin
      for i := 0 to GetEntitiesCount() - 1 do
      begin
        lCurEntity := TvEntity(GetEntity(i));
        InitializeRenderInfo(lRenderInfo, ADest, lCurEntity);
        if lCurEntity.CalculateSizeInCanvas(lRenderInfo, ARenderHeight, AZoom, lLeft, lTop, lWidth, lHeight) then
        begin
          lMinX := Min(lMinX, lLeft);
          lMinY := Min(lMinY, lTop);
          lMaxX := Max(lMaxX, lLeft + lWidth);
          lMaxY := Max(lMaxY, lTop  + lHeight);
        end;
        {$ifdef FPVECTORIAL_AUTOFIT_DEBUG}
        AutoFitDebug.Add(Format('[%s] MinX=%d MinY=%d MaxX=%d MaxY=%D', [lCurEntity.ClassName, lMinX, lMinY, lMaxX, lMaxY]));
        {$endif}
      end;

      lMinX := Min(lMinX, lLeft);
      lMinY := Min(lMinY, lTop);
      lMaxX := Max(lMaxX, lLeft + lWidth);
      lMaxY := Max(lMaxY, lTop  + lHeight);
    end
    else
    begin
      Render(ADest, 0, 0, AZoom, AZoom * lNaturalMulY, False);
      lMinX := RenderInfo.EntityCanvasMinXY.X;
      lMinY := RenderInfo.EntityCanvasMinXY.Y;
      lMaxX := RenderInfo.EntityCanvasMaxXY.X;
      lMaxY := RenderInfo.EntityCanvasMaxXY.Y;
    end;

    if (lMinX = High(Integer)) or (lMinY = High(Integer)) or
       (lMaxX = Low(Integer)) or(lMaxY = Low(Integer)) then
       Exit(False);

    lWidth := lMaxX - lMinX;
    lHeight := lMaxY - lMinY;
    if (lWidth = 0) or (lHeight = 0) then Exit(False);
  end;

begin
  {$ifdef FPVECTORIAL_AUTOFIT_DEBUG}
  AutoFitDebug := TStringList.Create;
  try
  {$endif}
  ADeltaX := 0;
  ADeltaY := 0;
  GetNaturalRenderPos(lNaturalHeightDiff, lNaturalMulY);

  // First Calculate the zoom

  AZoom := 1;
  if not CalculateAllEntitySizes() then Exit;

  lZoomFitX := AWidth / lWidth;
  lZoomFitY := AHeight / lHeight;
  AZoom := Min(lZoomFitX, lZoomFitY) * 0.9;

  // Now DeltaX, DeltaY

  if not CalculateAllEntitySizes() then Exit;
  ADeltaX := Round(-1 * lMinX) + AWidth div 2 - lWidth div 2;
  ADeltaY := Round(-1 * lMinY) + (AHeight div 2 - lHeight div 2);

  {$ifdef FPVECTORIAL_RENDERINFO_VISUALDEBUG}
  ADest.Brush.Style := bsClear;
  ADest.Pen.FPColor := colRed;
  ADest.Pen.Style := psSolid;
  ADest.Rectangle(lMinX+ADeltaX, lMinY+ADeltaY, lMaxX+ADeltaX, lMaxY+ADeltaY);
  {$endif}

  {$ifdef FPVECTORIAL_AUTOFIT_DEBUG}
  finally
    {$ifdef Windows}
    AutoFitDebug.SaveToFile('C:\Programas\autofit.txt');
    {$else}
    AutoFitDebug.SaveToFile('/Users/felipe/autofit.txt');
    {$endif}
    AutoFitDebug.Free;
    AutoFitDebug := nil;
  end;
  {$endif}
end;

procedure TvPage.SetNaturalRenderPos(AUseTopLeftCoords: Boolean);
begin
  FUseTopLeftCoordinates := AUseTopLeftCoords;
end;

function TvPage.HasNaturalRenderPos: Boolean;
begin
  Result := FUseTopLeftCoordinates;
end;

function TvPage.GetTopLeftCoords_Adjustment: Double;
begin
  if UseTopLeftCoordinates then
    Result := 1
  else
    Result := -1;
end;


{ TvVectorialPage }

procedure TvVectorialPage.ClearTmpPath;
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
  inherited Create(AOwner);

  FEntities := TFPList.Create;
  FTmpPath := TPath.Create(Self);
  Owner := AOwner;
  Clear();
  BackgroundColor := colWhite;
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

procedure TvVectorialPage.Assign(ASource: TvPage);
var
  i: Integer;
  AVecSource: TvVectorialPage absolute ASource;
begin
  if not (ASource is TvVectorialPage) then Exit;
  Clear;

  for i := 0 to AVecSource.GetEntitiesCount - 1 do
    Self.AddEntity(AVecSource.GetEntity(i));
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

function TvVectorialPage.GetEntityIndex(AEntity: TvEntity): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to GetEntitiesCount()-1 do
    if TvEntity(FEntities.Items[i]) = AEntity then Exit(i);
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
  FEntities.ForEachCall(@CallbackDeleteEntity, nil);
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
  AEntity.SetPage(Self);
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
  //Len: Integer;
begin
  lPath := TPath.Create(Self);
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

procedure TvVectorialPage.AddEllipticalArcWithCenterToPath(ARadX, ARadY,
  AXAxisRotation, ADestX, ADestY, ACenterX, ACenterY: Double;
  AClockwiseArcFlag: Boolean);
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
  segment.CX := ACenterX;
  segment.CY := ACenterY;
  segment.ClockwiseArcFlag := AClockwiseArcFlag;
  segment.CenterSetByUser := true;

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
  if FTmpPath.Len = 0 then Exit;
  Result := AddPathCopyMem(FTmpPath, AOnlyCreate);
  Result.FPage := self;
  ClearTmpPath();
end;

function TvVectorialPage.AddText(AX, AY, AZ: Double; FontName: string;
  FontSize: integer; AText: utf8string; AOnlyCreate: Boolean = False): TvText;
var
  lText: TvText;
begin
  lText := TvText.Create(Self);
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
  lCircle := TvCircle.Create(Self);
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
  lCircularArc := TvCircularArc.Create(Self);
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
  lEllipse := TvEllipse.Create(Self);
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
  lBlock := TvBlock.Create(Self);
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
  lInsert := TvInsert.Create(Self);
  lInsert.X := AX;
  lInsert.Y := AY;
  lInsert.InsertEntity := AInsertEntity;
  AddEntity(lInsert);
  Result := lInsert;
end;

function TvVectorialPage.AddLayer(AName: string): TvLayer;
begin
  Result := TvLayer.Create(Self);
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
  lDim := TvAlignedDimension.Create(Self);
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
  lDim := TvRadialDimension.Create(Self);
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
  lDim := TvArcDimension.Create(Self);
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
  lPoint := TvPoint.Create(Self);
  lPoint.X := AX;
  lPoint.Y := AY;
  lPoint.Z := AZ;
  AddEntity(lPoint);
  Result := lPoint;
end;

procedure TvVectorialPage.PositionEntitySubparts(constref
  ARenderInfo: TvRenderInfo; ABaseX, ABaseY: Double);
var
  i: Integer;
begin
  for i := 0 to GetEntitiesCount()-1 do
    GetEntity(i).PositionSubparts(ARenderInfo, ABaseX, ABaseY);
end;

procedure TvVectorialPage.DrawBackground(ADest: TFPCustomCanvas);
begin
  ADest.Pen.Style := psClear;
  ADest.Brush.Style := bsSolid;
  ADest.Brush.FPColor := BackgroundColor;
  ADest.FillRect(0, 0, ADest.Width, ADest.Height);
  ADest.Pen.Style := psSolid;
end;

procedure TvVectorialPage.RenderPageBorder(ADest: TFPCustomCanvas;
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
  lLeft, lTop, lRight, lBottom: Integer;
begin
  // Fix the min/max values
  if MinX = MaxX then MaxX := MinX + Width;
  if MinY = MaxY then MaxY := MinY + Height;

  lLeft := CoordToCanvasX(MinX);
  lTop := CoordToCanvasY(MaxY);
  lRight := CoordToCanvasX(MaxX);
  lBottom := CoordToCanvasY(MinY);

  ADest.Brush.Style := bsClear;
  ADest.Pen.FPColor := colBlack;
  ADest.Pen.Style := psSolid;
  ADest.Pen.Width := 1;
  ADest.Rectangle(lLeft, lTop, lRight, lBottom);
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

  Set ADoDraw to falses in order to just get the bounding box of all entities
  on the page in RenderInfo.EnitityCanvasMinXY/EntityCanvasMaxXY.
}
procedure TvVectorialPage.Render(ADest: TFPCustomCanvas;
  ADestX: Integer; ADestY: Integer; AMulX: Double; AMulY: Double;
  ADoDraw: Boolean = true);
var
  i: Integer;
  CurEntity: TvEntity;
  rinfo: TvRenderInfo;
begin
  {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
  WriteLn(':>DrawFPVectorialToCanvas');
  {$endif}

  InitializeRenderInfo(RenderInfo, ADest, nil);
  InitializeRenderInfo(rInfo, ADest, nil);
  TvEntity.CopyAndInitDocumentRenderInfo(rInfo, RenderInfo, False, False);
  if Assigned(FOwner.FRenderer) then FOwner.FRenderer.BeginRender(RenderInfo, ADoDraw);

  for i := 0 to GetEntitiesCount - 1 do
  begin
    {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
    Write(Format('[Path] ID=%d', [i]));
    {$endif}

    CurEntity := GetEntity(i);

    RenderInfo.BackgroundColor := BackgroundColor;
    RenderInfo.AdjustPenColorToBackground := AdjustPenColorToBackground;
    RenderInfo.DestX := ADestX;
    RenderInfo.DestY := ADestY;
    RenderInfo.MulX := AMulX;
    RenderInfo.MulY := AMulY;

    CurEntity.Render(RenderInfo, ADoDraw);

    if i = 0 then
      rInfo := RenderInfo
    else
    begin
      rInfo.EntityCanvasMinXY.X := Min(rInfo.EntityCanvasMinXY.X, RenderInfo.EntityCanvasMinXY.X);
      rInfo.EntityCanvasMinXY.Y := Min(rInfo.EntityCanvasMinXY.Y, RenderInfo.EntityCanvasMinXY.Y);
      rInfo.EntityCanvasMaxXY.X := Max(rInfo.EntityCanvasMaxXY.X, RenderInfo.EntityCanvasMaxXY.X);
      rInfo.EntityCanvasMaxXY.Y := Max(rInfo.EntityCanvasMaxXY.Y, RenderInfo.EntityCanvasMaxXY.Y);
    end;
  end;

  if Assigned(FOwner.FRenderer) then FOwner.FRenderer.EndRender(RenderInfo, ADoDraw);
  TvEntity.CopyAndInitDocumentRenderInfo(RenderInfo, rInfo, True, False);

  {$ifdef FPVECTORIAL_RENDERINFO_VISUALDEBUG}
  ADest.Brush.Style := bsClear;
  ADest.Pen.FPColor := colRed;
  ADest.Rectangle(rInfo.EntityCanvasMinXY.X, RenderInfo.EntityCanvasMinXY.Y,
    rInfo.EntityCanvasMaxXY.X, rInfo.EntityCanvasMaxXY.Y);
  {$endif}
  {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
  WriteLn(':<DrawFPVectorialToCanvas');
  {$endif}
end;

procedure TvVectorialPage.GetNaturalRenderPos(var APageHeight: Integer; out AMulY: Double);
begin
  if FUseTopLeftCoordinates then
  begin
    APageHeight := 0;
    AMulY := 1.0;
  end
  else
  begin
    AMulY := -1.0;
  end;
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

  FUseTopLeftCoordinates := True;
  Footer := TvRichText.Create(Self);
  Header := TvRichText.Create(Self);
  MainText := TvRichText.Create(Self);
end;

destructor TvTextPageSequence.Destroy;
begin
  Footer.Free;
  Header.Free;
  MainText.Free;

  inherited Destroy;
end;

procedure TvTextPageSequence.Assign(ASource: TvPage);
begin
  inherited Assign(ASource);
end;

function TvTextPageSequence.GetEntity(ANum: Cardinal): TvEntity;
begin
  Result := MainText.GetEntity(ANum);
end;

function TvTextPageSequence.GetEntitiesCount: Integer;
begin
  Result := MainText.GetEntitiesCount();
end;

function TvTextPageSequence.GetLastEntity: TvEntity;
begin
  Result := MainText.GetEntity(MainText.GetEntitiesCount()-1);
end;

function TvTextPageSequence.GetEntityIndex(AEntity: TvEntity): Integer;
begin
  Result := MainText.GetEntityIndex(AEntity);
end;

function TvTextPageSequence.FindAndSelectEntity(Pos: TPoint
  ): TvFindEntityResult;
begin

end;

function TvTextPageSequence.FindEntityWithNameAndType(AName: string;
  AType: TvEntityClass; ARecursively: Boolean): TvEntity;
begin

end;

procedure TvTextPageSequence.Clear;
begin
  MainText.Clear;
end;

function TvTextPageSequence.DeleteEntity(AIndex: Cardinal): Boolean;
begin
  Result := MainText.DeleteEntity(AIndex);
end;

function TvTextPageSequence.RemoveEntity(AEntity: TvEntity;
  AFreeAfterRemove: Boolean): Boolean;
begin
  Result := True;
  MainText.Clear;
end;

function TvTextPageSequence.AddEntity(AEntity: TvEntity): Integer;
begin
  AEntity.SetPage(Self);
  Result := MainText.AddEntity(AEntity);
end;

function TvTextPageSequence.AddParagraph: TvParagraph;
begin
  Result := MainText.AddParagraph();
end;

function TvTextPageSequence.AddList: TvList;
begin
  Result := MainText.AddList();
end;

function TvTextPageSequence.AddTable: TvTable;
begin
  Result := MainText.AddTable;
end;

function TvTextPageSequence.AddEmbeddedVectorialDoc: TvEmbeddedVectorialDoc;
begin
  Result := MainText.AddEmbeddedVectorialDoc;
end;

procedure TvTextPageSequence.DrawBackground(ADest: TFPCustomCanvas);
begin

end;

procedure TvTextPageSequence.RenderPageBorder(ADest: TFPCustomCanvas;
  ADestX: Integer; ADestY: Integer; AMulX: Double; AMulY: Double);
begin

end;

procedure TvTextPageSequence.Render(ADest: TFPCustomCanvas; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double; ADoDraw: Boolean = true);

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
  CurEntity: TvEntity;
  CurY_px: Integer = 0;
  lHeight_px: Integer;
  lBoundsLeft, lBoundsTop, lBoundsRight, lBoundsBottom: Double;
  lSumRenderInfo: TvRenderInfo;
begin
  {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
  WriteLn(':>TvTextPageSequence.Render');
  {$endif}
  CurY_px := ADestY;
  InitializeRenderInfo(RenderInfo, ADest, nil);
  TvEntity.CopyAndInitDocumentRenderInfo(lSumRenderInfo, RenderInfo);

  for i := 0 to GetEntitiesCount - 1 do
  begin
    {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
    Write(Format('[Path] ID=%d', [i]));
    {$endif}

    CurEntity := GetEntity(i);

    CurEntity.X := 0;
    CurEntity.Y := 0;
    lHeight_px := CurEntity.GetEntityFeatures(lSumRenderInfo).TotalHeight;
    RenderInfo.BackgroundColor := BackgroundColor;
    RenderInfo.DestX := ADestX;
    RenderInfo.DestY := CurY_px + lHeight_px;
    RenderInfo.MulX := AMulX;
    RenderInfo.MulY := AMulY;
    CurEntity.Render(RenderInfo, ADoDraw);
    // Store the old position in X/Y but don't use it, we use this to debug out the position
    CurEntity.X := ADestX;
    CurEntity.Y := CurY_px;
    lHeight_px := Abs(RenderInfo.EntityCanvasMaxXY.Y - RenderInfo.EntityCanvasMinXY.Y);
    CurY_px := CurY_px + lHeight_px;

    TvEntity.CalcEntityCanvasMinMaxXY_With2Points(lSumRenderInfo,
      RenderInfo.EntityCanvasMinXY.X, RenderInfo.EntityCanvasMinXY.Y,
      RenderInfo.EntityCanvasMaxXY.X, RenderInfo.EntityCanvasMaxXY.Y);
  end;

  TvEntity.CopyAndInitDocumentRenderInfo(RenderInfo, lSumRenderInfo, True);

  {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
  WriteLn(':<TvTextPageSequence.Render');
  {$endif}
end;

procedure TvTextPageSequence.GetNaturalRenderPos(var APageHeight: Integer; out AMulY: Double);
begin
  APageHeight := 0;
  AMulY := 1.0;
end;

procedure TvTextPageSequence.GenerateDebugTree(
  ADestRoutine: TvDebugAddItemProc; APageItem: Pointer);
var
  lCurEntity: TvEntity;
  i: Integer;
begin
  for i := 0 to MainText.GetEntitiesCount() - 1 do
  begin
    lCurEntity := MainText.GetEntity(i);
    lCurEntity.GenerateDebugTree(ADestRoutine, APageItem);
  end;
end;

(*
function TvTextPageSequence.AddImage: TvImage;
begin
  Result := MainText.AddImage;
end;
*)
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
  FListStyles := TFPList.Create;
  if gDefaultRenderer <> nil then
    FRenderer := gDefaultRenderer.Create;
end;

{@@
  Destructor.
}
destructor TvVectorialDocument.Destroy;
begin
  Clear();

  FPages.Free;
  FPages := nil;
  FStyles.Free;
  FStyles := nil;
  FListStyles.Free;
  FListStyles := nil;

  ClearRenderer();

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
  lFormat := GetFormatFromExtension(AFileName);
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
    AReader.Settings := ReaderSettings;
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
  lFormat := GetFormatFromExtension(AFileName);
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
    AReader.Settings := ReaderSettings;
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
    AReader.Settings := ReaderSettings;
    AReader.ReadFromStrings(AStrings, Self);
  finally
    AReader.Free;
  end;
end;

procedure TvVectorialDocument.ReadFromXML(ADoc: TXMLDocument; AFormat: TvVectorialFormat);
var
  AReader: TvCustomVectorialReader;
begin
  Self.Clear;

  AReader := CreateVectorialReader(AFormat);
  try
    AReader.ReadFromXML(ADoc, Self);
  finally
    AReader.Free;
  end;
end;

class function TvVectorialDocument.GetFormatFromExtension(AFileName: string;
  ARaiseException: Boolean = True): TvVectorialFormat;
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
  else if AnsiCompareText(lExt, STR_DOCX_EXTENSION) = 0 then Result := vfDOCX
  else if AnsiCompareText(lExt, STR_HTML_EXTENSION) = 0 then Result := vfHTML
  else if ARaiseException then
    raise Exception.Create('TvVectorialDocument.GetFormatFromExtension: The extension (' + lExt + ') doesn''t match any supported formats.')
  else
    Result := vfUnknown;
end;

function  TvVectorialDocument.GetDetailedFileFormat(): string;
begin

end;

procedure TvVectorialDocument.GuessDocumentSize();
var
  i, j: Integer;
  lEntity: TvEntity;
  lLeft, lTop, lRight, lBottom: Double;
  CurPage: TvPage;
  lRenderInfo: TvRenderInfo;
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
      TvEntity.InitializeRenderInfo(lRenderInfo, nil);
      lEntity.ExpandBoundingBox(lRenderInfo, lLeft, lTop, lRight, lBottom);
    end;
  end;

  Width := lRight - lLeft;
  Height := lBottom - lTop;
end;

procedure TvVectorialDocument.GuessGoodZoomLevel(AScreenSize: Integer);
begin
  If Height<>0 Then
    ZoomLevel := AScreenSize / Height;
end;

function TvVectorialDocument.GetPage(AIndex: Integer): TvPage;
begin
  Result := TvPage(FPages.Items[AIndex]);
end;

function TvVectorialDocument.GetPageIndex(APage: TvPage): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FPages.Count-1 do
    if TvPage(FPages.Items[i]) = APage then Exit(i);
end;

function TvVectorialDocument.GetPageAsVectorial(AIndex: Integer): TvVectorialPage;
var
  lPage: TvPage;
begin
  lPage := GetPage(AIndex);
  if lPage is TvVectorialPage then
    Result := TvVectorialPage(lPage)
  else
    Result := nil;
end;

function TvVectorialDocument.GetPageAsText(AIndex: Integer): TvTextPageSequence;
var
  lPage: TvPage;
begin
  lPage := GetPage(AIndex);
  if lPage is TvTextPageSequence then
    Result := TvTextPageSequence(lPage)
  else
    Result := nil;
end;

function TvVectorialDocument.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

function TvVectorialDocument.GetCurrentPage: TvPage;
begin
  if FCurrentPageIndex >= 0 then
    Result := GetPage(FCurrentPageIndex)
  else
    Result := nil;
end;

function TvVectorialDocument.GetCurrentPageAsVectorial: TvVectorialPage;
var
  lCurPage: TvPage;
begin
  lCurPage := GetCurrentPage();
  if (lCurPage <> nil) and (lCurPage is TvVectorialPage) then
    Result := TvVectorialPage(lCurPage)
  else
    Result := nil
end;

procedure TvVectorialDocument.SetCurrentPage(AIndex: Integer);
begin
  FCurrentPageIndex := AIndex;
end;

procedure TvVectorialDocument.SetDefaultPageFormat(AFormat: TvPageFormat);
begin
  case AFormat of
  vpA4:
  begin
    Width := 210;
    Height := 297;
  end;
  else
    Width := 210;
    Height := 297;
  end;
end;

function TvVectorialDocument.AddPage(AUseTopLeftCoords: Boolean = False): TvVectorialPage;
begin
  Result := TvVectorialPage.Create(Self);
  Result.Width := Width;
  Result.Height := Height;
  Result.SetNaturalRenderPos(AUseTopLeftCoords);
  FPages.Add(Result);
  if FCurrentPageIndex < 0 then FCurrentPageIndex := FPages.Count-1;
end;

function TvVectorialDocument.AddTextPageSequence: TvTextPageSequence;
begin
  Result := TvTextPageSequence.Create(Self);
  Result.Width := Width;
  Result.Height := Height;
  FPages.Add(Result);
  if FCurrentPageIndex < 0 then FCurrentPageIndex := FPages.Count-1;
end;

function TvVectorialDocument.AddStyle: TvStyle;
begin
  Result := TvStyle.Create;
  FStyles.Add(Result);
end;

function TvVectorialDocument.AddListStyle: TvListStyle;
begin
  Result := TvListStyle.Create;
  FListStyles.Add(Result);
end;

procedure TvVectorialDocument.AddStandardTextDocumentStyles(AFormat: TvVectorialFormat);
var
  lTextBody, lBaseHeading, lCurStyle: TvStyle;
  lCurListStyle : TvListStyle;
  i: Integer;
  lCurListLevelStyle: TvListLevelStyle;
begin
  lTextBody := AddStyle();
  lTextBody.Name := 'Text Body';
  lTextBody.Kind := vskTextBody;
  lTextBody.Font.Size := 12;
  lTextBody.Font.Name := 'Times New Roman';
  lTextBody.Brush.Style := bsClear;
  lTextBody.Alignment := vsaJustifed;
  lTextBody.MarginTop := 0;
  lTextBody.MarginBottom := 2.12;
  lTextBody.SetElements := [spbfFontSize, spbfFontName, spbfAlignment,
    sseMarginTop, sseMarginBottom, spbfBrushStyle];
  StyleTextBody := lTextBody;

  // Headings
  lBaseHeading := AddStyle();
  lBaseHeading.Name := 'Heading';
  lBaseHeading.Kind := vskHeading;
  lBaseHeading.Font.Size := 14;
  lBaseHeading.Font.Name := 'Arial';
  lBaseHeading.Brush.Style := bsClear;
  lBaseHeading.MarginTop := 4.23;
  lBaseHeading.MarginBottom := 2.12;
  lBaseHeading.SetElements := [spbfFontSize, spbfFontName, sseMarginTop, sseMarginBottom];

  lCurStyle := AddStyle();
  lCurStyle.Name := 'Heading 1';
  lCurStyle.Parent := lBaseHeading;
  lCurStyle.HeadingLevel := 1;
  lCurStyle.Font.Bold := True;
  case AFormat of
    vfHTML: lCurStyle.Font.Size := 20;
  else
    lCurStyle.Font.Size := Round(1.15 * lBaseHeading.Font.Size);
  end;
  lCurStyle.Brush.Style := bsClear;
  lCurStyle.SetElements := [spbfFontSize, spbfFontBold];
  StyleHeading1 := lCurStyle;

  lCurStyle := AddStyle();
  lCurStyle.Name := 'Heading 2';
  lCurStyle.Parent := lBaseHeading;
  lCurStyle.HeadingLevel := 2;
  lCurStyle.Font.Bold := True;
  case AFormat of
    vfHTML: lCurStyle.Font.Size := 16;
  else
    lCurStyle.Font.Size := 14;
    lCurStyle.Font.Italic := True;
  end;
  lCurStyle.Brush.Style := bsClear;
  lCurStyle.SetElements := [spbfFontSize, spbfFontBold, spbfFontItalic];
  StyleHeading2 := lCurStyle;

  lCurStyle := AddStyle();
  lCurStyle.Name := 'Heading 3';
  lCurStyle.Parent := lBaseHeading;
  lCurStyle.HeadingLevel := 3;
  lCurStyle.Font.Bold := True;
  lCurStyle.Font.Size := 14;
  lCurStyle.Brush.Style := bsClear;
  lCurStyle.SetElements := [spbfFontSize, spbfFontName, spbfFontBold];
  StyleHeading3 := lCurStyle;

  lCurStyle := AddStyle();
  lCurStyle.Name := 'Heading 4';
  lCurStyle.Parent := lBaseHeading;
  lCurStyle.HeadingLevel := 4;
  lCurStyle.Font.Size := 12;
  lCurStyle.Font.Bold := True;
  lCurStyle.Brush.Style := bsClear;
  lCurStyle.SetElements := [spbfFontSize, spbfFontName, spbfFontBold];
  StyleHeading4 := lCurStyle;

  lCurStyle := AddStyle();
  lCurStyle.Name := 'Heading 5';
  lCurStyle.Parent := lBaseHeading;
  lCurStyle.HeadingLevel := 5;
  lCurStyle.Font.Size := 10;
  lCurStyle.Font.Bold := True;
  lCurStyle.Brush.Style := bsClear;
  lCurStyle.SetElements := [spbfFontSize, spbfFontName, spbfFontBold];
  StyleHeading5 := lCurStyle;

  lCurStyle := AddStyle();
  lCurStyle.Name := 'Heading 6';
  lCurStyle.Parent := lBaseHeading;
  lCurStyle.HeadingLevel := 6;
  lCurStyle.Font.Size := 8;
  lCurStyle.Font.Bold := True;
  lCurStyle.Brush.Style := bsClear;
  lCurStyle.SetElements := [spbfFontSize, spbfFontName, spbfFontBold];
  StyleHeading6 := lCurStyle;

  // ---------------------------------
  // Centralized paragraph styles
  // ---------------------------------

  StyleTextBodyCentralized := AddStyle();
  StyleTextBodyCentralized.ApplyOver(StyleTextBody);
  StyleTextBodyCentralized.Name := 'Text Body Centered';
  StyleTextBodyCentralized.Alignment := vsaCenter;
  StyleTextBodyCentralized.SetElements := StyleTextBodyCentralized.SetElements + [spbfAlignment];

  StyleTextBodyBold := AddStyle();
  StyleTextBodyBold.ApplyOver(StyleTextBody);
  StyleTextBodyBold.Name := 'Text Body Bold';
  StyleTextBodyBold.Font.Bold := True;
  StyleTextBodyBold.SetElements := StyleTextBodyCentralized.SetElements + [spbfFontBold];

  StyleHeading1Centralized := AddStyle();
  StyleHeading1Centralized.ApplyOver(StyleHeading1);
  StyleHeading1Centralized.Name := 'Heading 1 Centered';
  StyleHeading1Centralized.Alignment := vsaCenter;
  StyleHeading1Centralized.SetElements := StyleHeading1Centralized.SetElements + [spbfAlignment];

  StyleHeading2Centralized := AddStyle();
  StyleHeading2Centralized.ApplyOver(StyleHeading2);
  StyleHeading2Centralized.Name := 'Heading 2 Centered';
  StyleHeading2Centralized.Alignment := vsaCenter;
  StyleHeading2Centralized.SetElements := StyleHeading2Centralized.SetElements + [spbfAlignment];

  StyleHeading3Centralized := AddStyle();
  StyleHeading3Centralized.ApplyOver(StyleHeading3);
  StyleHeading3Centralized.Name := 'Heading 3 Centered';
  StyleHeading3Centralized.Alignment := vsaCenter;
  StyleHeading3Centralized.SetElements := StyleHeading3Centralized.SetElements + [spbfAlignment];

  // ---------------------------------
  // Bullet List Items
  // ---------------------------------

  lCurListStyle := AddListStyle();
  lCurListStyle.Name := 'Bullet List Style';
  StyleBulletList := lCurListStyle;

  for i := 0 To NUM_MAX_LISTSTYLES-1 Do
  begin
    lCurListLevelStyle := StyleBulletList.AddListLevelStyle;
    lCurListLevelStyle.Kind := vlskBullet;
    lCurListLevelStyle.Level := i;

    // Bullet is positioned at MarginLeft - HangingIndent
    lCurListLevelStyle.MarginLeft := 16.35*(i + 1);
    lCurListLevelStyle.HangingIndent := 6.35;
  end;

  lCurListStyle := AddListStyle();
  lCurListStyle.Name := 'Numbered List Style';
  StyleNumberList := lCurListStyle;

  for i := 0 To NUM_MAX_LISTSTYLES-1 Do
  begin
    lCurListLevelStyle := StyleNumberList.AddListLevelStyle;
    lCurListLevelStyle.Kind := vlskNumeric;
    lCurListLevelStyle.NumberFormat := vnfDecimal;
    lCurListLevelStyle.Level := i;

    lCurListLevelStyle.Prefix := '';
    lCurListLevelStyle.Suffix := '.';
    lCurListLevelStyle.DisplayLevels := True;  // 1.1.1.1.
    lCurListLevelStyle.LeaderFontName := 'Arial';

    // For MS Word
    // Bullet is positioned at MarginLeft - HangingIndent
    lCurListLevelStyle.MarginLeft := 16.35*(i + 1);
    lCurListLevelStyle.HangingIndent := 6.35 + 3*i;
  end;

  // ---------------------------------
  // Text Span Items
  // ---------------------------------
  StyleTextSpanBold := AddStyle();
  StyleTextSpanBold.Kind := vskTextSpan; // This implies this style should not be applied to Paragraphs
  StyleTextSpanBold.Name := 'Bold';
  StyleTextSpanBold.Font.Bold := True;
  StyleTextSpanBold.Brush.Style := bsClear;
  StyleTextSpanBold.SetElements := StyleTextSpanBold.SetElements + [spbfFontBold];

  StyleTextSpanItalic := AddStyle();
  StyleTextSpanItalic.Kind := vskTextSpan; // This implies this style should not be applied to Paragraphs
  StyleTextSpanItalic.Name := 'Italic';
  StyleTextSpanItalic.Font.Italic := True;
  StyleTextSpanItalic.Brush.Style := bsClear;
  StyleTextSpanItalic.SetElements := StyleTextSpanItalic.SetElements + [spbfFontItalic];

  StyleTextSpanUnderline := AddStyle();
  StyleTextSpanUnderline.Kind := vskTextSpan; // This implies this style should not be applied to Paragraphs
  StyleTextSpanUnderline.Name := 'Underline';
  StyleTextSpanUnderline.Font.Underline := True;
  StyleTextSpanUnderline.Brush.Style := bsClear;
  StyleTextSpanUnderline.SetElements := StyleTextSpanUnderline.SetElements + [spbfFontUnderline];
end;

function TvVectorialDocument.GetStyleCount: Integer;
begin
  Result := FStyles.Count;
end;

function TvVectorialDocument.GetStyle(AIndex: Integer): TvStyle;
begin
  Result := TvStyle(FStyles.Items[AIndex]);
end;

function TvVectorialDocument.FindStyleIndex(AStyle: TvStyle): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to GetStyleCount()-1 do
    if GetStyle(i) = AStyle then Exit(i);
end;

function TvVectorialDocument.GetListStyleCount: Integer;
begin
  Result := FListStyles.Count;
end;

function TvVectorialDocument.GetListStyle(AIndex: Integer): TvListStyle;
begin
  Result := TvListStyle(FListStyles.Items[AIndex]);
end;

function TvVectorialDocument.FindListStyleIndex(AListStyle: TvListStyle): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to GetListStyleCount()-1 do
    if GetListStyle(i) = AListStyle then Exit(i);
end;


{@@
  Clears all data in the document
}
// GM: Release memory for each page
procedure TvVectorialDocument.Clear;
var
  i: integer;
  p: TvPage;
begin
  for i:=0 to FStyles.Count-1 do
    TvStyle(FStyles[i]).Free;
  FStyles.Clear;

  for i:=0 to FListStyles.Count-1 do
    TvListStyle(FListStyles[i]).Free;
  FListStyles.Clear;

  for i:=FPages.Count-1 downto 0 do
  begin
    p := TvPage(FPages[i]);
    p.Clear;
    FreeAndNil(p);
  end;
  FPages.Clear;
  FCurrentPageIndex:=-1;
end;

function TvVectorialDocument.GetRenderer: TvRenderer;
begin
  Result := FRenderer;
end;

procedure TvVectorialDocument.SetRenderer(ARenderer: TvRenderer);
begin
  ClearRenderer();
  FRenderer := ARenderer;
end;

procedure TvVectorialDocument.ClearRenderer;
begin
  if FRenderer <> nil then FreeAndNil(FRenderer);
end;

procedure TvVectorialDocument.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer);
var
  i, lTmpInt: integer;
  p: TvPage;
  lPageItem: Pointer;
  lDebugStr: string;
  lTmpY: Double;
begin
  for i:=0 to FPages.Count-1 do
  begin
    p := TvPage(FPages[i]);

    lDebugStr := 'Origin=';
    p.GetNaturalRenderPos(lTmpInt, lTmpY);
    if lTmpY > 0 then
      lDebugStr += 'top-left'
    else
      lDebugStr += 'bottom-left';


    lPageItem := ADestRoutine(Format('Page %d : %s %s Width=%f Height=%f MinX=%f MaxX=%f MinY=%f MaxY=%f',
      [i, p.ClassName, lDebugStr, p.Width, p.Height, p.MinX, p.MaxX, p.MinY, p.MaxY]), APageItem);
    p.GenerateDebugTree(ADestRoutine, lPageItem);
  end;
end;

{ TvCustomVectorialReader }

class function TvCustomVectorialReader.GetTextContentsFromNode(ANode: TDOMNode): DOMString;
var
  lNodeTextTmp: DOMString;
  lContentNode: TDOMNode;
begin
  Result := '';

  for lContentNode in ANode.GetEnumeratorAllChildren() do
  begin
    if lContentNode is TDOMText then
      lNodeTextTmp := TDOMText(lContentNode).TextContent
    else if lContentNode is TDOMEntityReference then
    begin
      lNodeTextTmp := UTF8LowerCase(lContentNode.NodeName);
      case lNodeTextTmp of
      'pi': lNodeTextTmp := 'π';
      'invisibletimes': lNodeTextTmp := '';
      else
        lNodeTextTmp := '';//lContentNode.NodeName;
      end;
    end
    else
      lNodeTextTmp := lContentNode.NodeName;

    Result := Result + lNodeTextTmp;
  end;
end;

class function TvCustomVectorialReader.RemoveLineEndingsAndTrim(AStr: string): string;
begin
  Result := Trim(AStr);
  Result := StringReplace(Result, #13, '', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '', [rfReplaceAll]);
end;

constructor TvCustomVectorialReader.Create;
begin
  inherited Create;
end;

procedure TvCustomVectorialReader.ReadFromFile(AFileName: string; AData: TvVectorialDocument);
var
  FileStream: TFileStream;
begin
  FFilename := AFilename;
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

procedure TvCustomVectorialReader.ReadFromXML(ADoc: TXMLDocument; AData: TvVectorialDocument);
begin
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

