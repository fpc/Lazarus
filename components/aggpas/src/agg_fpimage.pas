// Copyright (c) 2007 - 2008
//
// Permission to copy, use, modify, sell and distribute this software
// is granted provided this copyright notice appears in all copies.
// This software is provided "as is" without express or implied
// warranty, and with no claim as to its suitability for any purpose.
//
//
//
unit agg_fpimage;


{$mode objfpc}{$H+}

interface

{$IFDEF LINUX}
  {$DEFINE AGG2D_USE_FREETYPE}
{$ENDIF}
{$IFDEF FREEBSD}
  {$DEFINE AGG2D_USE_FREETYPE}
{$ENDIF}
{$IFDEF WINDOWS}
  {$DEFINE AGG2D_USE_WINFONTS}
{$ENDIF}
{$IFNDEF AGG2D_USE_WINFONTS}
 {$IFNDEF AGG2D_USE_FREETYPE}
  {$DEFINE AGG2D_NO_FONT}
 {$ENDIF}
{$ENDIF}

uses
  agg_basics ,
  agg_array ,
  agg_trans_affine ,
  agg_trans_viewport ,
  agg_path_storage ,
  agg_conv_stroke ,
  agg_conv_transform ,
  agg_conv_curve ,
  agg_rendering_buffer ,
  agg_renderer_base ,
  agg_renderer_scanline ,
  agg_span_gradient ,
  agg_span_image_filter_rgba ,
  agg_span_image_resample_rgba ,
  agg_span_converter ,
  agg_span_interpolator_linear ,
  agg_span_allocator ,
  agg_rasterizer_scanline_aa ,
  agg_gamma_functions ,
  agg_scanline_u ,
  agg_scanline,
  agg_arc ,
  agg_bezier_arc ,
  agg_rounded_rect ,
  agg_font_engine ,
  agg_font_cache_manager ,
  agg_pixfmt ,
  agg_pixfmt_rgb ,
  agg_pixfmt_rgba ,
  agg_color ,
  agg_math_stroke ,
  agg_image_filters ,
  agg_vertex_source ,
  agg_render_scanlines ,

  {$IFDEF AGG2D_USE_FREETYPE}
  agg_font_freetype,
  {$ENDIF }
  {$IFDEF AGG2D_USE_WINFONTS}
  agg_font_win32_tt,
  {$ENDIF }

  Math, types ,
  {$IFDEF WINDOWS}
  Windows ,
  {$ENDIF}

  Classes, SysUtils, FPimage, FPCanvas;

  { GLOBAL VARIABLES & CONSTANTS }
const
  // LineJoin
  AGG_JoinMiter = miter_join;
  AGG_JoinRound = round_join;
  AGG_JoinBevel = bevel_join;

  // LineCap
  AGG_CapButt   = butt_cap;
  AGG_CapSquare = square_cap;
  AGG_CapRound  = round_cap;

  // TextAlignment
  AGG_AlignLeft   = 0;
  AGG_AlignRight  = 1;
  AGG_AlignCenter = 2;
  AGG_AlignTop    = AGG_AlignRight;
  AGG_AlignBottom = AGG_AlignLeft;

  // BlendMode
  AGG_BlendAlpha      = end_of_comp_op_e;
  AGG_BlendClear      = comp_op_clear;
  AGG_BlendSrc        = comp_op_src;
  AGG_BlendDst        = comp_op_dst;
  AGG_BlendSrcOver    = comp_op_src_over;
  AGG_BlendDstOver    = comp_op_dst_over;
  AGG_BlendSrcIn      = comp_op_src_in;
  AGG_BlendDstIn      = comp_op_dst_in;
  AGG_BlendSrcOut     = comp_op_src_out;
  AGG_BlendDstOut     = comp_op_dst_out;
  AGG_BlendSrcAtop    = comp_op_src_atop;
  AGG_BlendDstAtop    = comp_op_dst_atop;
  AGG_BlendXor        = comp_op_xor;
  AGG_BlendAdd        = comp_op_plus;
  AGG_BlendSub        = comp_op_minus;
  AGG_BlendMultiply   = comp_op_multiply;
  AGG_BlendScreen     = comp_op_screen;
  AGG_BlendOverlay    = comp_op_overlay;
  AGG_BlendDarken     = comp_op_darken;
  AGG_BlendLighten    = comp_op_lighten;
  AGG_BlendColorDodge = comp_op_color_dodge;
  AGG_BlendColorBurn  = comp_op_color_burn;
  AGG_BlendHardLight  = comp_op_hard_light;
  AGG_BlendSoftLight  = comp_op_soft_light;
  AGG_BlendDifference = comp_op_difference;
  AGG_BlendExclusion  = comp_op_exclusion;
  AGG_BlendContrast   = comp_op_contrast;

{ TYPES DEFINITION }
type
  PAggColor = ^TAggColor;
  TAggColor = rgba8;

  TAggRectD = agg_basics.rect_d;

  TAggAffine = trans_affine;
  PAggAffine = trans_affine_ptr;

  TAggFontRasterizer = gray8_adaptor_type;
  PAggFontRasterizer = gray8_adaptor_type_ptr;

  TAggFontScanline = gray8_scanline_type;
  PAggFontScanline = gray8_scanline_type_ptr;

  {$IFDEF AGG2D_USE_FREETYPE }
  TAggFontEngine = font_engine_freetype_int32;
  {$ENDIF}
  {$IFDEF AGG2D_USE_WINFONTS }
  TAggFontEngine = font_engine_win32_tt_int32;

  {$ENDIF }

  TAggGradient  = (
    AGG_Solid ,
    AGG_Linear ,
    AGG_Radial );
  TAggDirection = (
    AGG_CW,
    AGG_CCW );

  TAggLineJoin  = int;
  TAggLineCap   = int;
  TAggBlendMode = comp_op_e;

  TAggTextAlignment = int;

  TAggDrawPathFlag = (
    AGG_FillOnly ,
    AGG_StrokeOnly ,
    AGG_FillAndStroke ,
    AGG_FillWithLineColor );

  TAggViewportOption = (
    AGG_Anisotropic ,
    AGG_XMinYMin ,
    AGG_XMidYMin ,
    AGG_XMaxYMin ,
    AGG_XMinYMid ,
    AGG_XMidYMid ,
    AGG_XMaxYMid ,
    AGG_XMinYMax ,
    AGG_XMidYMax ,
    AGG_XMaxYMax );

  TAggImageFilter = (
    AGG_NoFilter ,
    AGG_Bilinear ,
    AGG_Hanning ,
    AGG_Hermite ,
    AGG_Quadric ,
    AGG_Bicubic ,
    AGG_Catrom ,
    AGG_Spline16 ,
    AGG_Spline36 ,
    AGG_Blackman144 );

  TAggImageResample = (
    AGG_NoResample ,
    AGG_ResampleAlways ,
    AGG_ResampleOnZoomOut );

  TAggFontCacheType = (
    AGG_RasterFontCache ,
    AGG_VectorFontCache );

  PAggTransformations = ^TAggTransformations;
  TAggTransformations = record
    affineMatrix : array[0..5 ] of double;
  end;

  { TAggRasterizerGamma }

  TAggRasterizerGamma = object(vertex_source )
    m_alpha : gamma_multiply;
    m_gamma : gamma_power;

    constructor Construct(alpha ,gamma : double );

    function func_operator_gamma(x : double ) : double; virtual;
    function operator_array(i: unsigned): unsigned; virtual;
  end;

  { TAggFP_renderer_scanline_aa }

  TAggFP_renderer_scanline_aa = object(renderer_scanline_aa)
    procedure add_path(vs: vertex_source_ptr; path_id: unsigned=0); virtual;
    procedure add_vertex(x, y: double; cmd: unsigned); virtual;
    procedure clip_box(x1, y1, x2, y2: double); virtual;
    procedure color_(c: aggclr_ptr); virtual;
    procedure filling_rule(filling_rule_: filling_rule_e); virtual;
    procedure gamma(gamma_function: vertex_source_ptr); virtual;
    procedure reset; virtual;
    function _max_x: int; virtual;
    function _max_y: int; virtual;
    function _min_x: int; virtual;
    function _min_y: int; virtual;
    function hit_test(tx, ty: int): boolean; virtual;
    function sweep_scanline(sl: scanline_ptr): boolean; virtual;
    function sweep_scanline_em(sl: scanline_ptr): boolean; virtual;
    function rewind_scanlines: boolean; virtual;
    procedure sort; virtual;
  end;

type
  TAggFPImage = class;

  TAggFPImgPixelFormat = (
    afpimRGB24,
    afpimRGBA32
    );

  TAggFPImgOperation = (
    afpioResized,
    afpioPixelFormatChanged,
    afpioDestroying
    );
  TAggFPImgEvent = procedure(TheImage: TAggFPImage;
                             Operation: TAggFPImgOperation) of object;

  { TAggFPImage }

  TAggFPImage = class(TFPCustomImage)
  private
    FData: PByte;
    FListeners: array of TAggFPImgEvent;
    FPixelFormat: TAggFPImgPixelFormat;
  protected
    procedure SetInternalColor(x, y: integer; const Value: TFPColor); override;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel(x, y: integer): integer; override;
    procedure SetInternalPixel(x, y: integer; Value: integer); override;
    procedure SetPixelFormat(const AValue: TAggFPImgPixelFormat); virtual;
    procedure SetUsePalette(Value: boolean); override;
    procedure ReallocData; virtual;
  public
    RenderingBuffer: rendering_buffer;
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetSize(AWidth, AHeight: integer); override;
    procedure AddListener(Event: TAggFPImgEvent);
    procedure RemoveListener(Event: TAggFPImgEvent);
    procedure NotifyListeners(Operation: TAggFPImgOperation);
    property PixelFormat: TAggFPImgPixelFormat read FPixelFormat write SetPixelFormat;
    property Data: PByte read FData;
    function DataSize: PtrUInt; // total size of Data in bytes
    function LineSize: PtrUInt; // size of a line in bytes including padding
  end;

  { TAggFPBrush }

  TAggFPBrush = class(TFPCustomBrush)
  private
    FAggColor: TAggColor;
    FAggFillEvenOdd: boolean;
  protected
    procedure SetFPColor(const AValue: TFPColor); override;
    procedure SetAggColor(const AValue: TAggColor); virtual;
    procedure SetStyle(AValue: TFPBrushStyle); override;
    procedure SetAggFillEvenOdd(const AValue: boolean); virtual;
    procedure DoCopyProps(From: TFPCanvasHelper); override;
  public
    property AggColor: TAggColor read FAggColor write SetAggColor;
    property AggFillEvenOdd: boolean read FAggFillEvenOdd write SetAggFillEvenOdd;
    property Pattern; // not supported, always 0
    property Image; // not supported, always nil
    property Style; // not supported, always bsSolid
  end;

  { TAggFPPen }

  TAggFPPen = class(TFPCustomPen)
  private
    FAggColor: TAggColor;
    FAggLineCap: TAggLineCap;
    FAggLineJoin: TAggLineJoin;
    FAggLineWidth: double;
  protected
    procedure SetAggLineCap(const AValue: TAggLineCap); virtual;
    procedure SetAggLineJoin(const AValue: TAggLineJoin); virtual;
    procedure SetAggLineWidth(const AValue: double); virtual;
    procedure SetWidth(AValue: Integer); override;
    procedure SetFPColor(const AValue: TFPColor); override;
    procedure SetAggColor(const AValue: TAggColor); virtual;
    procedure DoCopyProps(From: TFPCanvasHelper); override;
  public
    constructor Create; override;
    property AggLineCap: TAggLineCap read FAggLineCap write SetAggLineCap default AGG_CapRound;
    property AggLineJoin: TAggLineJoin read FAggLineJoin write SetAggLineJoin default AGG_JoinRound;
    property AggLineWidth: double read FAggLineWidth write SetAggLineWidth;
    property AggColor: TAggColor read FAggColor write SetAggColor;
    property Pattern; // not supported, always 0
    property Style; // not supported, always psSolid
    property Mode; // not supported, always pmBlack
  end;

  { TAggFPFont }

  TAggFPFont = class(TFPCustomFont)
  private
    FAggAlignX: TAggTextAlignment;
    FAggAlignY: TAggTextAlignment;
    FAggAngle: double;
    FAggCache: TAggFontCacheType;
    FAggColor: TAggColor;
    FAggFlipY: boolean;
    FAggHeight: double;
    FAggHinting: boolean;
    FAggUseOnlyFont: boolean;
  protected
    procedure DoCopyProps(From: TFPCanvasHelper); override;
    procedure SetFPColor(const AValue: TFPColor); override;
    procedure SetAggColor(const AValue: TAggColor); virtual;
    procedure SetAggAlignX(const AValue: TAggTextAlignment); virtual;
    procedure SetAggAlignY(const AValue: TAggTextAlignment); virtual;
    procedure SetAggAngle(const AValue: double); virtual;
    procedure SetAggFlipY(const AValue: boolean); virtual;
    procedure SetAggHeight(const AValue: double); virtual;
    procedure SetAggHinting(const AValue: boolean); virtual;
    procedure SetSize(AValue: integer); override;
  public
    constructor Create; override;
    procedure LoadFromFile(aFilename : String;
              const NewHeight: double = 10.0;
              const NewBold: boolean = false;
              const NewItalic: boolean = false;
              const NewCache : TAggFontCacheType = AGG_VectorFontCache;
              const NewAngle : double = 0.0 ;
              const NewHinting: boolean = true);
    function AggHeightToSize(const h: double): double; virtual;
    function SizeToAggHeight(const s: double): double; virtual;
    property AggColor: TAggColor read FAggColor write SetAggColor;
    property AggAlignX: TAggTextAlignment read FAggAlignX write SetAggAlignX default AGG_AlignLeft;
    property AggAlignY: TAggTextAlignment read FAggAlignY write SetAggAlignY default AGG_AlignBottom;
    property AggHinting: boolean read FAggHinting write SetAggHinting default True;// only freetype
    property AggAngle: double read FAggAngle write SetAggAngle;
    property AggCache: TAggFontCacheType read FAggCache;
    property AggHeight: double read FAggHeight write SetAggHeight;
    property AggFlipY: boolean read FAggFlipY write SetAggFlipY;
    property AggUseOnlyFont: boolean read FAggUseOnlyFont write FAggUseOnlyFont default True;// do not use Pen and Brush
    property Bold; // only windows
    property Italic; // only windows
    property Underline; // not supported
    property StrikeTrough; // not supported
  end;

  { TAggFPPath }

  TAggFPPath = class(TFPCanvasHelper)
  private
    FAggColor: TAggColor;
    procedure SetAggColor(const AValue: TAggColor);
  protected
    procedure DoCopyProps(From: TFPCanvasHelper); override;
  public
    m_path: path_storage;
    m_convCurve: conv_curve;
    constructor Create; override;
    destructor Destroy; override;
    property AggColor: TAggColor read FAggColor write SetAggColor;
  end;

  { TAggFPCanvas }

  TAggFPCanvas = class(TFPCustomCanvas)
  protected
    FAggBrush: TAggFPBrush;
    FAggFont: TAggFPFont;
    FAggPen: TAggFPPen;
    FAggPath: TAggFPPath;
    FImage: TAggFPImage;
    FUseUTF8: boolean;

    m_rbuf : rendering_buffer;

    m_pixFormat ,m_pixFormatComp ,m_pixFormatPre ,m_pixFormatCompPre : pixel_formats;
    m_renBase   ,m_renBaseComp   ,m_renBasePre   ,m_renBaseCompPre   : renderer_base;

    m_renSolid ,m_renSolidComp : renderer_scanline_aa_solid;

    m_allocator : span_allocator;
    m_clipBox   : TAggRectD;

    m_blendMode ,m_imageBlendMode : TAggBlendMode;

    m_imageBlendColor : TAggColor;

    m_scanline   : scanline_u8;
    m_rasterizer : rasterizer_scanline_aa;

    m_masterAlpha ,m_antiAliasGamma : double;

    m_fillGradient ,m_lineGradient : pod_auto_array;

    m_fillGradientFlag ,m_lineGradientFlag : TAggGradient;

    m_fillGradientMatrix ,m_lineGradientMatrix : trans_affine;

    m_fillGradientD1 ,
    m_lineGradientD1 ,
    m_fillGradientD2 ,
    m_lineGradientD2 : double;

    m_imageFilter    : TAggImageFilter;
    m_imageResample  : TAggImageResample;
    m_imageFilterLut : image_filter_lut;

    m_fillGradientInterpolator ,
    m_lineGradientInterpolator : span_interpolator_linear;

    m_linearGradientFunction : gradient_x;
    m_radialGradientFunction : gradient_circle;

    m_evenOddFlag : boolean;

    m_transform : trans_affine;

    m_convStroke : conv_stroke;

    m_pathTransform ,m_strokeTransform : conv_transform;

    {$IFDEF AGG2D_USE_WINFONTS }
    m_fontDC : HDC;
    {$ENDIF }

    {$IFNDEF AGG2D_NO_FONT}
    m_fontEngine       : TAggFontEngine;
    m_fontCacheManager : font_cache_manager;
    {$ENDIF}

    // Other Pascal-specific members
    m_gammaNone  : gamma_none;
    m_gammaAgg2D : TAggRasterizerGamma;

    m_ifBilinear    : image_filter_bilinear;
    m_ifHanning     : image_filter_hanning;
    m_ifHermite     : image_filter_hermite;
    m_ifQuadric     : image_filter_quadric;
    m_ifBicubic     : image_filter_bicubic;
    m_ifCatrom      : image_filter_catrom;
    m_ifSpline16    : image_filter_spline16;
    m_ifSpline36    : image_filter_spline36;
    m_ifBlackman144 : image_filter_blackman144;

    procedure Agg2DRenderer_render(renBase: renderer_base_ptr;
      renSolid: renderer_scanline_aa_solid_ptr; fillColor_, UseFont: boolean);
    procedure Agg2DRenderer_render(
               renBase : renderer_base_ptr;
               renSolid : renderer_scanline_aa_solid_ptr;
               ras : gray8_adaptor_type_ptr;
               sl : gray8_scanline_type_ptr;
               UseFont: boolean );
    procedure addLine(const x1 ,y1 ,x2 ,y2 : double );
    function GetAggTransformations: TAggTransformations;

    procedure render(fillColor_: boolean; UseFont: boolean = false);
    procedure render(ras : PAggFontRasterizer; sl : PAggFontScanline );
    procedure Agg2DRenderer_renderImage(
                   img : TAggFPImage;
                   renBase : renderer_base_ptr;
                   interpolator : span_interpolator_linear_ptr );

    procedure SetAggTransformations(const AValue: TAggTransformations);
    procedure SetAntiAliasGamma(const AValue: double);
    procedure SetBlendMode(const AValue: TAggBlendMode);
    procedure SetImageFilter(const AValue: TAggImageFilter);
    procedure SetImageResample(const AValue: TAggImageResample);
    procedure SetMasterAlpha(const AValue: double);
    procedure UpdateRasterizerGamma;

  protected
    procedure DoCopyRect(DstX, DstY: integer; SrcCanvas: TFPCustomCanvas;
      const SourceRect: TRect); override;
    function DoCreateDefaultBrush: TFPCustomBrush; override;
    function DoCreateDefaultFont: TFPCustomFont; override;
    function DoCreateDefaultPen: TFPCustomPen; override;
    function DoCreateDefaultPath: TAggFPPath; virtual;
    function DoCreateDefaultImage: TAggFPImage; virtual;
    procedure DoDraw(x, y: integer; const SrcImage: TFPCustomImage); override;
    procedure DoEllipse(const Bounds: TRect); override;
    procedure DoEllipseFill(const Bounds: TRect); override;
    procedure DoFloodFill(x, y: integer); override;
    function DoGetTextHeight(str: string): integer; override;
    procedure DoGetTextSize(str: string; var w, h: integer); override;
    function DoGetTextWidth(str: string): integer; override;
    procedure DoLine(x1, y1, x2, y2: integer); override;
    procedure DoPolygon(const points: array of TPoint); override;
    procedure DoPolygonFill(const points: array of TPoint); override;
    procedure DoPolyline(const points: array of TPoint); override;
    procedure DoRectangle(const Bounds: TRect); override;
    procedure DoRectangleFill(const Bounds: TRect); override;
    procedure DoTextOut(x, y: integer; str: string); override;
    function GetColor(x, y: integer): TFPColor; override;
    function GetHeight: integer; override;
    function GetWidth: integer; override;
    procedure SetColor(x, y: integer; const Value: TFPColor); override;
    procedure SetHeight(AValue: integer); override;
    procedure SetWidth(AValue: integer); override;
    procedure OnImageOperation(Img: TAggFPImage; Operation: TAggFPImgOperation); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearSettings; virtual;

    procedure AggClearAll(const c : TAggColor );
    procedure AggClearAll(const r ,g ,b : byte; a : byte = 255 );

    property Image: TAggFPImage read FImage;
    property Pen: TAggFPPen read FAggPen;
    property Brush: TAggFPBrush read FAggBrush;
    property Font: TAggFPFont read FAggFont;
    property Path: TAggFPPath read FAggPath;

    procedure Erase; override;

    // special AggPas functions, prefixed with Agg to avoid name clashing

    property AggBlendMode: TAggBlendMode read m_blendMode write SetBlendMode;
    property AggMasterAlpha: double read m_masterAlpha write SetMasterAlpha;
    property AggAntiAliasGamma: double read m_antiAliasGamma write SetAntiAliasGamma;

    // Basic AggPas Shapes
    procedure AggLine     (const x1 ,y1 ,x2 ,y2 : double );
    procedure AggTriangle (const x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double );
    procedure AggRectangle(const x1 ,y1 ,x2 ,y2 : double );

    procedure AggRoundedRect(const x1 ,y1 ,x2 ,y2 ,r : double );
    procedure AggRoundedRect(const x1 ,y1 ,x2 ,y2 ,rx ,ry : double );
    procedure AggRoundedRect(const x1 ,y1 ,x2 ,y2 ,
                                   rxBottom ,ryBottom ,
                                   rxTop ,ryTop : double );

    procedure AggEllipse(const cx ,cy ,rx ,ry : double );

    procedure AggArc (const cx ,cy ,rx ,ry ,start ,endangle : double ); // start: 0 at 3'o clock, clockwise in rad: 180deg = 1pi
    procedure AggStar(const cx, cy, r1, r2, startAngle: double; numRays: integer);

    procedure AggCurve(const x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double );
    procedure AggCurve(const x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 : double );

    procedure AggPolygon (const xy : PDouble; numPoints : integer );
    procedure AggPolyline(const xy : PDouble; numPoints : integer );

    procedure AggFillLinearGradient(const x1 ,y1 ,x2 ,y2 : double; c1 ,c2 : TAggColor; profile : double = 1.0 );
    procedure AggLineLinearGradient(const x1 ,y1 ,x2 ,y2 : double; c1 ,c2 : TAggColor; profile : double = 1.0 );

    procedure AggFillRadialGradient(const x ,y ,r : double; c1 ,c2 : TAggColor; profile : double = 1.0 );
    procedure AggLineRadialGradient(const x ,y ,r : double; c1 ,c2 : TAggColor; profile : double = 1.0 );

    procedure AggFillRadialGradient(const x ,y ,r : double; c1 ,c2 ,c3 : TAggColor );
    procedure AggLineRadialGradient(const x ,y ,r : double; c1 ,c2 ,c3 : TAggColor );

    procedure AggFillRadialGradient(const x ,y ,r : double );
    procedure AggLineRadialGradient(const x ,y ,r : double );

    // Path Commands
    procedure AggResetPath;

    procedure AggMoveTo (const x ,y : double );
    procedure AggMoveRel(const dx ,dy : double );

    procedure AggLineTo (const x ,y : double );
    procedure AggLineRel(const dx ,dy : double );

    procedure AggHorLineTo (const x : double );
    procedure AggHorLineRel(const dx : double );

    procedure AggVerLineTo (const y : double );
    procedure AggVerLineRel(const dy : double );

    procedure AggArcTo(const
              rx ,ry ,angle : double;
              largeArcFlag ,sweepFlag : boolean;
              const x ,y : double );

    procedure AggArcRel(const
              rx ,ry ,angle : double;
              largeArcFlag ,sweepFlag : boolean;
              const dx ,dy : double );

    procedure AggQuadricCurveTo (const xCtrl ,yCtrl ,xTo ,yTo : double );
    procedure AggQuadricCurveRel(const dxCtrl ,dyCtrl ,dxTo ,dyTo : double );
    procedure AggQuadricCurveTo (const xTo ,yTo : double );
    procedure AggQuadricCurveRel(const dxTo ,dyTo : double );

    procedure AggCubicCurveTo (const xCtrl1 ,yCtrl1 ,xCtrl2 ,yCtrl2 ,xTo ,yTo : double );
    procedure AggCubicCurveRel(const dxCtrl1 ,dyCtrl1 ,dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo : double );
    procedure AggCubicCurveTo (const xCtrl2 ,yCtrl2 ,xTo ,yTo : double );
    procedure AggCubicCurveRel(const dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo : double );

    procedure AggAddEllipse(const cx ,cy ,rx ,ry : double; dir : TAggDirection );

    procedure AggClosePolygon;
    procedure AggDrawPath(flag: TAggDrawPathFlag = AGG_FillAndStroke;
                          UseFont: boolean = false);

    // Clipping
    procedure AggSetClipBox(const x1 ,y1 ,x2 ,y2 : double );
    function  AggGetClipBox : TAggRectD;
    procedure AggClearClipBox(const c : TAggColor );
    procedure AggClearClipBox(r ,g ,b : byte; a : byte = 255 );
    function  AggInClipBox(const worldX ,worldY : double ) : boolean;

    // Affine Transformations
    property AggTransformations: TAggTransformations read GetAggTransformations
                                                    write SetAggTransformations;
    procedure AggResetTransformations;

    procedure AggAffine(const tr : PAggAffine );
    procedure AggAffine(const tr : PAggTransformations );

    procedure AggRotate   (const angle : double );
    procedure AggScale    (const sx ,sy : double );
    procedure AggSkew     (const sx ,sy : double );
    procedure AggTranslate(const x ,y : double );

    procedure AggParallelogram(const x1 ,y1 ,x2 ,y2 : double; para : PDouble );

    procedure AggViewport(const
              worldX1  ,worldY1  ,worldX2  ,worldY2 ,
              screenX1 ,screenY1 ,screenX2 ,screenY2 : double;
              const opt : TAggViewportOption = AGG_XMidYMid );

    // Coordinates Conversions
    procedure AggWorldToScreen(x ,y : PDouble );
    procedure AggScreenToWorld(x ,y : PDouble );
    function  AggWorldToScreen(const scalar : double ) : double;
    function  AggScreenToWorld(const scalar : double ) : double;

    procedure AggAlignPoint(x ,y : PDouble );

    // Image Rendering
    property AggImageFilter: TAggImageFilter read m_imageFilter write SetImageFilter;
    property AggImageResample: TAggImageResample read m_imageResample write SetImageResample;

    procedure AggRenderImage(
              img : TAggFPImage;
              x1 ,y1 ,x2 ,y2 : integer;
              parl : PDouble );

    procedure AggTransformImage(
              SrcImage: TAggFPImage;
              const imgX1 ,imgY1 ,imgX2 ,imgY2 : integer;
              const dstX1 ,dstY1 ,dstX2 ,dstY2 : double );

    procedure AggTransformImage(
              SrcImage: TAggFPImage;
              const dstX1 ,dstY1 ,dstX2 ,dstY2 : double );

    procedure AggTransformImage(
              SrcImage: TAggFPImage;
              imgX1 ,imgY1 ,imgX2 ,imgY2 : integer;
              parallelo : PDouble );

    procedure AggTransformImage(SrcImage: TAggFPImage; parallelo : PDouble );

    procedure AggTransformImagePath(
              SrcImage: TAggFPImage;
              const imgX1 ,imgY1 ,imgX2 ,imgY2 : integer;
              const dstX1 ,dstY1 ,dstX2 ,dstY2 : double );

    procedure AggTransformImagePath(
              SrcImage: TAggFPImage;
              const dstX1 ,dstY1 ,dstX2 ,dstY2 : double );

    procedure AggTransformImagePath(
              SrcImage: TAggFPImage;
              const imgX1 ,imgY1 ,imgX2 ,imgY2 : integer;
              const parallelo : PDouble );

    procedure AggTransformImagePath(SrcImage: TAggFPImage; parallelo : PDouble );

    procedure AggCopyImage(
              SrcImage: TAggFPImage;
              const imgX1 ,imgY1 ,imgX2 ,imgY2 : integer;
              const dstX ,dstY : double );

    procedure AggCopyImage(SrcImage: TAggFPImage; const dstX ,dstY: double);

    // text
    function AggTextWidth(str : AnsiString ) : double; virtual;
    function AggTextHeight(str : AnsiString ) : double; virtual;
    procedure AggTextOut(
              const x ,y : double;
              str : AnsiString;
              roundOff : boolean = false;
              const ddx : double = 0.0;
              const ddy : double = 0.0 ); virtual;
    property UseUTF8: boolean read FUseUTF8 write FUseUTF8;
  end;

function FPToAggColor(const c: TFPColor): TAggColor;
function AggToFPColor(const c: TAggColor): TFPColor;

function AggUTF8CharToUnicode(p: PChar; out CharLen: int): int32u;

implementation

var
 g_approxScale : double = 2.0;

type
  //PAggSpanConvImageBlend = ^TAggSpanConvImageBlend;

  { TAggSpanConvImageBlend }

  TAggSpanConvImageBlend = object(span_convertor )
  private
    m_mode  : TAggBlendMode;
    m_color : TAggColor;
    m_pixel : pixel_formats_ptr; // m_pixFormatCompPre
  public
    constructor Construct(m : TAggBlendMode; c : TAggColor; p : pixel_formats_ptr );
    procedure convert(span : aggclr_ptr; x ,y : int; len : unsigned ); virtual;
  end;

{ TAggSpanConvImageBlend }

constructor TAggSpanConvImageBlend.Construct(m: TAggBlendMode; c: TAggColor;
  p: pixel_formats_ptr);
begin
  m_mode :=m;
  m_color:=c;
  m_pixel:=p;
end;

procedure TAggSpanConvImageBlend.convert(span: aggclr_ptr; x, y: int;
  len: unsigned);
var
 l2 ,a : unsigned;
 s2 : PAggColor;
begin
 if (m_mode <> AGG_BlendDst ) and
    (m_pixel <> NIL ) then
  begin{!}
   l2:=len;
   s2:=PAggColor(span );

   repeat
    comp_op_adaptor_clip_to_dst_rgba_pre(
     m_pixel ,
     unsigned(m_mode ) ,
     int8u_ptr(s2 ) ,
     m_color.r ,
     m_color.g ,
     m_color.b ,
     base_mask ,
     cover_full );

    inc(ptrcomp(s2 ) ,sizeof(aggclr ) );
    dec(l2 );

   until l2 = 0;

  end;

 if m_color.a < base_mask then
  begin
   l2:=len;
   s2:=PAggColor(span );
   a :=m_color.a;

   repeat
    s2^.r:=(s2^.r * a ) shr base_shift;
    s2^.g:=(s2^.g * a ) shr base_shift;
    s2^.b:=(s2^.b * a ) shr base_shift;
    s2^.a:=(s2^.a * a ) shr base_shift;

    inc(ptrcomp(s2 ) ,sizeof(aggclr ) );
    dec(l2 );

   until l2 = 0;

  end;
end;

function FPToAggColor(const c: TFPColor): TAggColor;
begin
  Result.r:=c.red shr 8;
  Result.g:=c.green shr 8;
  Result.b:=c.blue shr 8;
  Result.a:=c.alpha shr 8;
end;

function AggToFPColor(const c: TAggColor): TFPColor;
begin
  Result.red:=c.r or (c.r shl 8);
  Result.green:=c.g or (c.g shl 8);
  Result.blue:=c.b or (c.b shl 8);
  Result.alpha:=c.a or (c.a shl 8);
end;

function AggUTF8CharToUnicode(p: PChar; out CharLen: int): int32u;
begin
  if p=nil then begin
    Result:=0;
    CharLen:=0;
    exit;
  end;
  if ord(p^)<%11000000 then begin
    // regular single byte character (#0 is a normal char, this is pascal ;)
  end
  else if ((ord(p^) and %11100000) = %11000000) then begin
    // could be double byte character
    if (ord(p[1]) and %11000000) = %10000000 then begin
      Result:=((ord(p^) and %00011111) shl 6)
              or (ord(p[1]) and %00111111);
      CharLen:=2;
      exit;
    end;
  end
  else if ((ord(p^) and %11110000) = %11100000) then begin
    // could be triple byte character
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000) then begin
      Result:=((ord(p^) and %00011111) shl 12)
              or ((ord(p[1]) and %00111111) shl 6)
              or (ord(p[2]) and %00111111);
      CharLen:=3;
      exit;
    end;
  end
  else if ((ord(p^) and %11111000) = %11110000) then begin
    // could be 4 byte character
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000)
    and ((ord(p[3]) and %11000000) = %10000000) then begin
      Result:=((ord(p^) and %00001111) shl 18)
              or ((ord(p[1]) and %00111111) shl 12)
              or ((ord(p[2]) and %00111111) shl 6)
              or (ord(p[3]) and %00111111);
      CharLen:=4;
      exit;
    end;
  end
  else begin
    // invalid character
  end;
  Result:=ord(p^);
  CharLen:=1;
end;


{ TAggFPImage }

procedure TAggFPImage.SetPixelFormat(const AValue: TAggFPImgPixelFormat);
begin
  if FPixelFormat=AValue then exit;
  FPixelFormat:=AValue;
  ReallocData;
  NotifyListeners(afpioPixelFormatChanged);
end;

procedure TAggFPImage.SetUsePalette(Value: boolean);
begin
  if Value then
    raise Exception.Create('palette not supported by '+ClassName);
end;

procedure TAggFPImage.ReallocData;
begin
  ReAllocMem(fData,DataSize);
  RenderingBuffer.Destruct;
  RenderingBuffer.Construct(FData, Width, Height, LineSize);
end;

constructor TAggFPImage.Create(AWidth, AHeight: integer);
begin
  RenderingBuffer.Construct;
  inherited Create(AWidth, AHeight);
end;

procedure TAggFPImage.SetInternalColor(x, y: integer; const Value: TFPColor);
var
  p: PByte;
begin
  if (x>=0) and (y>=0) and (x<Width) and (y<Height) then begin
    p:=@FData[y*Width+x];
    case PixelFormat of
    afpimRGB24:
      begin
        p[0]:=Value.red shr 8;
        p[1]:=Value.green shr 8;
        p[2]:=Value.blue shr 8;
      end;
    afpimRGBA32:
      begin
        p[0]:=Value.red shr 8;
        p[1]:=Value.green shr 8;
        p[2]:=Value.blue shr 8;
        p[3]:=Value.alpha shr 8;
      end;
    end;
  end;
end;

function TAggFPImage.GetInternalColor(x, y: integer): TFPColor;
var
  p: PByte;
begin
  if (x>=0) and (y>=0) and (x<Width) and (y<Height) then begin
    p:=@FData[y*Width+x];
    case PixelFormat of
    afpimRGB24:
      begin
        Result.red:=p[0];
        Result.red:=Result.red or (Result.red shl 8);
        Result.green:=p[1];
        Result.green:=Result.green or (Result.green shl 8);
        Result.blue:=p[2];
        Result.blue:=Result.blue or (Result.blue shl 8);
        Result.alpha:=alphaOpaque;
      end;
    afpimRGBA32:
      begin
        Result.red:=p[0];
        Result.red:=Result.red or (Result.red shl 8);
        Result.green:=p[1];
        Result.green:=Result.green or (Result.green shl 8);
        Result.blue:=p[2];
        Result.blue:=Result.blue or (Result.blue shl 8);
        Result.alpha:=p[3];
        Result.alpha:=Result.alpha or (Result.alpha shl 8);
      end;
    end;
  end;
end;

function TAggFPImage.GetInternalPixel(x, y: integer): integer;
var
  p: PByte;
begin
  p:=@FData[y*Width+x];
  case PixelFormat of
  afpimRGB24:
    begin
      Result:=PInteger(p)^;
      {$IFDEF ENDIAN_LITTLE}
      Result:=Result and $ffffff;
      {$ELSE}
      Result:=Result shr 8;
      {$ENDIF}
    end;
  afpimRGBA32: Result:=PInteger(p)^;
  end;
end;

procedure TAggFPImage.SetInternalPixel(x, y: integer; Value: integer);
var
  p: PByte;
begin
  p:=@FData[y*Width+x];
  case PixelFormat of
  afpimRGB24:
    begin
      {$IFDEF ENDIAN_LITTLE}
      // ToDo
      {$ELSE}
      // ToDo
      {$ENDIF}
    end;
  afpimRGBA32: PInteger(p)^:=Value;
  end;
end;

destructor TAggFPImage.Destroy;
begin
  NotifyListeners(afpioDestroying);
  ReAllocMem(FData,0);
  inherited Destroy;
  RenderingBuffer.Destruct;
end;

procedure TAggFPImage.Assign(Source: TPersistent);
var
  Src: TAggFPImage;
begin
  if Source is TAggFPImage then begin
    Src:=TAggFPImage(Source);
    SetSize(0,0);
    PixelFormat:=Src.PixelFormat;
    SetSize(Src.Width,Src.Height);
    System.Move(Src.Data^,FData^,DataSize);
  end else
    inherited Assign(Source);
end;

procedure TAggFPImage.SetSize(AWidth, AHeight: integer);
begin
  if (Width=AWidth) and (Height=AHeight) then exit;
  inherited SetSize(AWidth, AHeight);
  ReallocData;
  NotifyListeners(afpioResized);
end;

procedure TAggFPImage.AddListener(Event: TAggFPImgEvent);
begin
  SetLength(FListeners,length(FListeners)+1);
  FListeners[length(FListeners)-1]:=Event;
end;

procedure TAggFPImage.RemoveListener(Event: TAggFPImgEvent);
var
  i: Integer;
begin
  for i:=length(FListeners)-1 downto 0 do begin
    // compare Code and Data, do not use =
    if CompareMem(@FListeners[i],@Event,SizeOf(TAggFPImgEvent)) then begin
      // delete
      if i<length(FListeners) then
        System.Move(FListeners[i+1],FListeners[i],
          SizeOf(TAggFPImgEvent)*(length(FListeners)-i-1));
      SetLength(FListeners,length(FListeners)-1);
    end;
  end;
end;

procedure TAggFPImage.NotifyListeners(Operation: TAggFPImgOperation);
var
  i: Integer;
begin
  for i:=length(FListeners)-1 downto 0 do
    FListeners[i](Self,Operation);
end;

function TAggFPImage.DataSize: PtrUInt;
begin
  Result:=Width*Height;
  case PixelFormat of
  afpimRGB24: Result:=Result*3;
  afpimRGBA32: Result:=Result*4;
  end;
end;

function TAggFPImage.LineSize: PtrUInt;
begin
  Result:=Width;
  case PixelFormat of
  afpimRGB24:  Result:=Result*3;
  afpimRGBA32: Result:=Result*4;
  end;
end;

{ TAggFPCanvas }

procedure TAggFPCanvas.render(fillColor_: boolean; UseFont: boolean);
begin
  if (m_blendMode = AGG_BlendAlpha ) or
     (Image.FPixelFormat = afpimRGB24 )
  then
    Agg2DRenderer_render(@m_renBase ,@m_renSolid ,fillColor_, UseFont )
  else
    Agg2DRenderer_render(@m_renBaseComp ,@m_renSolidComp ,fillColor_, UseFont );
end;

procedure TAggFPCanvas.render(ras: PAggFontRasterizer; sl: PAggFontScanline);
begin
  if (m_blendMode = AGG_BlendAlpha ) or
    (Image.PixelFormat = afpimRGB24 ) then
    Agg2DRenderer_render(@m_renBase ,@m_renSolid ,ras ,sl, Font.AggUseOnlyFont )
  else
    Agg2DRenderer_render(@m_renBaseComp ,@m_renSolidComp ,ras ,sl, Font.AggUseOnlyFont);
end;

procedure TAggFPCanvas.SetAggTransformations(const AValue: TAggTransformations);
begin
  m_transform.load_from(@AValue.affineMatrix[0 ] );

  Path.m_convCurve.approximation_scale_ (AggWorldToScreen(1.0 ) * g_approxScale );
  m_convStroke.approximation_scale_(AggWorldToScreen(1.0 ) * g_approxScale );
end;

procedure TAggFPCanvas.SetAntiAliasGamma(const AValue: double);
begin
  if m_antiAliasGamma=AValue then exit;
  m_antiAliasGamma:=AValue;
  UpdateRasterizerGamma;
end;

procedure TAggFPCanvas.SetBlendMode(const AValue: TAggBlendMode);
begin
  if m_blendMode=AValue then exit;
  m_blendMode:=AValue;
  m_pixFormatComp.comp_op_   (unsigned(m_blendMode ) );
  m_pixFormatCompPre.comp_op_(unsigned(m_blendMode ) );
end;

procedure TAggFPCanvas.SetImageFilter(const AValue: TAggImageFilter);
begin
  if AValue=m_imageFilter then exit;
  m_imageFilter:=AValue;
  case m_imageFilter of
  AGG_Bilinear :
   m_imageFilterLut.calculate(@m_ifBilinear ,true );

  AGG_Hanning :
   m_imageFilterLut.calculate(@m_ifHanning ,true );

  AGG_Hermite :
   m_imageFilterLut.calculate(@m_ifHermite ,true );

  AGG_Quadric :
   m_imageFilterLut.calculate(@m_ifQuadric ,true );

  AGG_Bicubic :
   m_imageFilterLut.calculate(@m_ifBicubic ,true );

  AGG_Catrom :
   m_imageFilterLut.calculate(@m_ifCatrom ,true );

  AGG_Spline16 :
   m_imageFilterLut.calculate(@m_ifSpline16 ,true );

  AGG_Spline36 :
   m_imageFilterLut.calculate(@m_ifSpline36 ,true );

  AGG_Blackman144 :
   m_imageFilterLut.calculate(@m_ifBlackman144 ,true );

 end;
end;

procedure TAggFPCanvas.SetImageResample(const AValue: TAggImageResample);
begin
  if AValue=m_imageResample then exit;
  m_imageResample:=AValue;
end;

procedure TAggFPCanvas.SetMasterAlpha(const AValue: double);
begin
  if AValue=m_masterAlpha then exit;
  m_masterAlpha:=AValue;
  UpdateRasterizerGamma;
end;

procedure TAggFPCanvas.ClearSettings;
begin
  m_renBase.reset_clipping       (true );
  m_renBaseComp.reset_clipping   (true );
  m_renBasePre.reset_clipping    (true );
  m_renBaseCompPre.reset_clipping(true );

  AggResetTransformations;

  Pen.AggLineWidth:=1.0;
  Pen.FPColor:=colBlack;
  Pen.AggLineCap:=AGG_CapRound;
  Pen.AggLineJoin:=AGG_JoinRound;

  Brush.FPColor:=colWhite;
  m_fillGradientFlag:=AGG_Solid;

  AggSetClipBox (0 ,0 ,Width ,Height );

  AggImageFilter:=AGG_Bilinear;
  AggImageResample:=AGG_NoResample;

  m_masterAlpha   :=1.0;
  m_antiAliasGamma:=1.0;

  m_rasterizer.gamma(@m_gammaNone );

  m_blendMode:=AGG_BlendAlpha;

  Brush.AggFillEvenOdd:=false;

  m_blendMode:=AGG_BlendClear;
  AggBlendMode:=AGG_BlendAlpha;

  //TextAlignment(AGG_AlignLeft ,AGG_AlignBottom );
  //FlipText(false );

  AggResetPath;
end;

procedure TAggFPCanvas.AggClearAll(const c: TAggColor);
var
  clr : aggclr;
begin
  clr.Construct  (c );
  m_renBase.clear(@clr );
end;

procedure TAggFPCanvas.AggClearAll(const r, g, b: byte; a: byte);
var
  clr : TAggColor;
begin
  clr.Construct(r ,g ,b ,a );
  AggClearAll     (clr );
end;

procedure TAggFPCanvas.Erase;
begin
  AggClearAll(FPToAggColor(colTransparent));
end;

procedure TAggFPCanvas.UpdateRasterizerGamma;
begin
  m_gammaAgg2D.Construct(m_masterAlpha ,m_antiAliasGamma );
  m_rasterizer.gamma    (@m_gammaAgg2D );
end;

procedure TAggFPCanvas.DoCopyRect(DstX, DstY: integer;
  SrcCanvas: TFPCustomCanvas; const SourceRect: TRect);
var
  y: LongInt;
  x: LongInt;
  dx: Integer;
  dy: Integer;
  r: TRect;
begin
  if SrcCanvas is TAggFPCanvas then begin
    AggCopyImage(TAggFPCanvas(SrcCanvas).Image,
       SourceRect.Left,SourceRect.Top,SourceRect.Right,SourceRect.Bottom,
       DstX+0.5,DstY+0.5);
  end else begin
    dx:=DstX-SourceRect.Left;
    dy:=DstY-SourceRect.Top;
    r:=SourceRect;
    r.Left:=Max(r.Left,0);
    r.Left:=Max(r.Left+dx,0)-dx;
    r.Top:=Max(r.Top,0);
    r.Top:=Max(r.Top+dy,0)-dy;
    r.Right:=Min(r.Right,SrcCanvas.Width);
    r.Right:=Min(r.Right+dx,Width)-dx;
    r.Bottom:=Min(r.Bottom,SrcCanvas.Height);
    r.Bottom:=Min(r.Bottom+dy,Height)-dy;
    for y:=r.Top to r.Bottom-1 do begin
      for x:=r.Left to r.Right-1 do begin
        Image.Colors[x+dx,y+dy]:=SrcCanvas.Colors[x,y];
      end;
    end;
  end;
end;

function TAggFPCanvas.DoCreateDefaultBrush: TFPCustomBrush;
begin
  Result:=TAggFPBrush.Create;
end;

function TAggFPCanvas.DoCreateDefaultFont: TFPCustomFont;
begin
  Result:=TAggFPFont.Create;
end;

function TAggFPCanvas.DoCreateDefaultPen: TFPCustomPen;
begin
  Result:=TAggFPPen.Create;
end;

function TAggFPCanvas.DoCreateDefaultPath: TAggFPPath;
begin
  Result:=TAggFPPath.Create;
end;

function TAggFPCanvas.DoCreateDefaultImage: TAggFPImage;
begin
  Result:=TAggFPImage.Create(0,0);
end;

procedure TAggFPCanvas.DoDraw(x, y: integer; const SrcImage: TFPCustomImage);
var
  r: TRect;
  SrcY: LongInt;
  SrcX: LongInt;
begin
  if SrcImage is TAggFPImage then begin
    AggCopyImage(TAggFPImage(SrcImage),x+0.5,y+0.5);
  end else begin
    r:=Classes.Rect(0,0,SrcImage.Width,SrcImage.Height);
    r.Left:=Max(r.Left+x,0)-x;
    r.Top:=Max(r.Top+y,0)-y;
    r.Right:=Min(r.Right+x,Width)-x;
    r.Bottom:=Min(r.Bottom+y,Height)-y;
    for SrcY:=r.Top to r.Bottom-1 do begin
      for SrcX:=r.Left to r.Right-1 do begin
        Image.Colors[SrcX+x,SrcY+y]:=SrcImage.Colors[SrcX,SrcY];
      end;
    end;
  end;
end;

procedure TAggFPCanvas.DoEllipse(const Bounds: TRect);
var
  el : bezier_arc;
  cx: Integer;
  cy: Integer;
  rx: Integer;
  ry: Integer;
begin
  Path.m_path.remove_all;

  cx:=(Bounds.Left+Bounds.Right) div 2;
  cy:=(Bounds.Top+Bounds.Bottom) div 2;
  rx:=Abs(Bounds.Right-Bounds.Left) div 2;
  ry:=Abs(Bounds.Bottom-Bounds.Top) div 2;
  el.Construct(cx+0.5 ,cy+0.5 ,rx ,ry ,0 ,2 * pi );

  Path.m_path.add_path(@el ,0 ,false );
  Path.m_path.close_polygon;

  AggDrawPath(AGG_StrokeOnly);
end;

procedure TAggFPCanvas.DoEllipseFill(const Bounds: TRect);
var
  el : bezier_arc;
  cx: Integer;
  cy: Integer;
  rx: Integer;
  ry: Integer;
begin
  Path.m_path.remove_all;

  cx:=(Bounds.Left+Bounds.Right) div 2;
  cy:=(Bounds.Top+Bounds.Bottom) div 2;
  rx:=Abs(Bounds.Right-Bounds.Left) div 2;
  ry:=Abs(Bounds.Bottom-Bounds.Top) div 2;
  el.Construct(cx+0.5 ,cy+0.5 ,rx ,ry ,0 ,2 * pi );

  Path.m_path.add_path(@el ,0 ,false );
  Path.m_path.close_polygon;

  AggDrawPath(AGG_FillOnly);
end;

procedure TAggFPCanvas.DoFloodFill(x, y: integer);
begin
  raise Exception.Create('TAggFPCanvas.DoFloodFill not supported by AggPas');
end;

function TAggFPCanvas.DoGetTextHeight(str: string): integer;
begin
  Result:=ceil(AggTextHeight(str));
end;

procedure TAggFPCanvas.DoGetTextSize(str: string; var w, h: integer);
begin
  w:=DoGetTextWidth(str);
  h:=DoGetTextHeight(str);
end;

function TAggFPCanvas.DoGetTextWidth(str: string): integer;
begin
  Result:=ceil(AggTextWidth(str));
end;

procedure TAggFPCanvas.DoLine(x1, y1, x2, y2: integer);
begin
  Path.m_path.remove_all;
  Path.m_path.move_to(x1+0.5 ,y1+0.5 );
  Path.m_path.line_to(x2+0.5 ,y2+0.5 );
  AggDrawPath(AGG_StrokeOnly );
end;

procedure TAggFPCanvas.DoPolygon(const points: array of TPoint);
var
  p: TPoint;
  i: Integer;
begin
  if length(Points)<=1 then exit;
  Path.m_path.remove_all;
  i:=low(points);
  p:=points[i];
  Path.m_path.move_to(p.x+0.5 ,p.y+0.5 );
  inc(i);
  while i<=high(points) do begin
    p:=points[i];
    Path.m_path.line_to(p.x+0.5,p.y+0.5);
    inc(i);
  end;
  AggClosePolygon;
  AggDrawPath(AGG_StrokeOnly );
end;

procedure TAggFPCanvas.DoPolygonFill(const points: array of TPoint);
var
  p: TPoint;
  i: Integer;
begin
  if length(Points)<=1 then exit;
  Path.m_path.remove_all;
  i:=low(points);
  p:=points[i];
  Path.m_path.move_to(p.x+0.5 ,p.y+0.5 );
  inc(i);
  while i<=high(points) do begin
    p:=points[i];
    Path.m_path.line_to(p.x+0.5,p.y+0.5);
    inc(i);
  end;
  AggClosePolygon;
  AggDrawPath(AGG_FillOnly );
end;

procedure TAggFPCanvas.DoPolyline(const points: array of TPoint);
var
  p: TPoint;
  i: Integer;
begin
  if length(Points)<=1 then exit;
  Path.m_path.remove_all;
  i:=low(points);
  p:=points[i];
  Path.m_path.move_to(p.x+0.5 ,p.y+0.5 );
  inc(i);
  while i<=high(points) do begin
    p:=points[i];
    Path.m_path.line_to(p.x+0.5,p.y+0.5);
    inc(i);
  end;
  AggDrawPath(AGG_StrokeOnly );
end;

procedure TAggFPCanvas.DoRectangle(const Bounds: TRect);
begin
  Path.m_path.remove_all;
  Path.m_path.move_to(Bounds.Left+0.5,Bounds.Top+0.5);
  Path.m_path.line_to(Bounds.Right+0.5,Bounds.Top+0.5);
  Path.m_path.line_to(Bounds.Right+0.5,Bounds.Bottom+0.5);
  Path.m_path.line_to(Bounds.Left+0.5,Bounds.Bottom+0.5);
  AggClosePolygon;
  AggDrawPath(AGG_StrokeOnly);
end;

procedure TAggFPCanvas.DoRectangleFill(const Bounds: TRect);
begin
  Path.m_path.remove_all;
  Path.m_path.move_to(Bounds.Left+0.5,Bounds.Top+0.5);
  Path.m_path.line_to(Bounds.Right+0.5,Bounds.Top+0.5);
  Path.m_path.line_to(Bounds.Right+0.5,Bounds.Bottom+0.5);
  Path.m_path.line_to(Bounds.Left+0.5,Bounds.Bottom+0.5);
  AggClosePolygon;
  AggDrawPath(AGG_FillAndStroke);
end;

procedure TAggFPCanvas.DoTextOut(x, y: integer; str: string);
begin
  AggTextOut(x+0.5,y+0.5,str);
end;

function TAggFPCanvas.GetColor(x, y: integer): TFPColor;
begin
  Result:=FImage.Colors[x,y];
end;

function TAggFPCanvas.GetHeight: integer;
begin
  Result:=FImage.Height;
end;

function TAggFPCanvas.GetWidth: integer;
begin
  Result:=FImage.Width;
end;

procedure TAggFPCanvas.SetColor(x, y: integer; const Value: TFPColor);
begin
  FImage.Colors[x,y]:=Value;
end;

procedure TAggFPCanvas.SetHeight(AValue: integer);
begin
  FImage.Height:=AValue;
end;

procedure TAggFPCanvas.SetWidth(AValue: integer);
begin
  FImage.Width:=AValue;
end;

procedure TAggFPCanvas.OnImageOperation(Img: TAggFPImage;
  Operation: TAggFPImgOperation);
begin
  if Img<>Image then exit;
  case Operation of
  afpioResized,afpioPixelFormatChanged:
    begin
      // Todo: shrink buffer (m_rbuf.attach only grows)
      if (Image.Width>0) and (Image.Height>0) then begin
        //writeln('TAggFPCanvas.OnImageOperation ',Image.Width,',',Image.Height,' ',Image.LineSize);
        m_rbuf.attach(
          Image.Data ,
          Image.Width ,
          Image.Height ,
          Image.LineSize);
       case Image.PixelFormat of
        afpimRGB24:
          begin
            pixfmt_rgb24(m_pixFormat ,@m_rbuf );
            pixfmt_rgb24(m_pixFormatPre ,@m_rbuf );
          end;

        afpimRGBA32:
          begin
            pixfmt_rgba32           (m_pixFormat ,@m_rbuf );
            pixfmt_custom_blend_rgba(m_pixFormatComp ,@m_rbuf ,
                                     @comp_op_adaptor_rgba ,rgba_order );
            pixfmt_rgba32           (m_pixFormatPre ,@m_rbuf );
            pixfmt_custom_blend_rgba(m_pixFormatCompPre ,@m_rbuf ,
                                     @comp_op_adaptor_rgba ,rgba_order );
          end;
        end;

        { Reset state }
        m_renBase.reset_clipping       (true );
        m_renBaseComp.reset_clipping   (true );
        m_renBasePre.reset_clipping    (true );
        m_renBaseCompPre.reset_clipping(true );

        AggSetClipBox (0 ,0 ,Width ,Height );
      end;
    end;
  afpioDestroying:
    begin
      FImage:=nil;
    end;
  end;
end;

constructor TAggFPCanvas.Create;
begin
  inherited Create;

  FUseUTF8:=true;

  FAggFont := TAggFPFont(inherited Font);
  FAggPen := TAggFPPen(inherited Pen);
  FAggBrush := TAggFPBrush(inherited Brush);
  FAggPath := DoCreateDefaultPath;
  FAggPath.AllocateResources(Self);

  FImage:=DoCreateDefaultImage;
  FImage.AddListener(@OnImageOperation);

  m_rbuf.Construct;

  pixfmt_rgba32           (m_pixFormat ,@m_rbuf );
  pixfmt_custom_blend_rgba(m_pixFormatComp ,@m_rbuf ,@comp_op_adaptor_rgba ,rgba_order );
  pixfmt_rgba32           (m_pixFormatPre ,@m_rbuf );
  pixfmt_custom_blend_rgba(m_pixFormatCompPre ,@m_rbuf ,@comp_op_adaptor_rgba ,rgba_order );

  m_renBase.Construct       (@m_pixFormat );
  m_renBaseComp.Construct   (@m_pixFormatComp );
  m_renBasePre.Construct    (@m_pixFormatPre );
  m_renBaseCompPre.Construct(@m_pixFormatCompPre );

  m_renSolid.Construct    (@m_renBase );
  m_renSolidComp.Construct(@m_renBaseComp );

  m_allocator.Construct;
  m_clipBox.Construct(0 ,0 ,0 ,0 );

  m_blendMode     :=AGG_BlendAlpha;
  m_imageBlendMode:=AGG_BlendDst;

  m_imageBlendColor.Construct(0 ,0 ,0 );

  m_scanline.Construct;
  m_rasterizer.Construct;

  m_masterAlpha   :=1.0;
  m_antiAliasGamma:=1.0;

  m_fillGradient.Construct(256 ,sizeof(aggclr ) );
  m_lineGradient.Construct(256 ,sizeof(aggclr ) );

  m_fillGradientFlag:=AGG_Solid;
  m_lineGradientFlag:=AGG_Solid;

  m_fillGradientMatrix.Construct;
  m_lineGradientMatrix.Construct;

  m_fillGradientD1:=0.0;
  m_lineGradientD1:=0.0;
  m_fillGradientD2:=100.0;
  m_lineGradientD2:=100.0;

  m_imageFilter  :=AGG_Bilinear;
  m_imageResample:=AGG_NoResample;

  m_gammaNone.Construct;

  m_ifBilinear.Construct;
  m_ifHanning.Construct;
  m_ifHermite.Construct;
  m_ifQuadric.Construct;
  m_ifBicubic.Construct;
  m_ifCatrom.Construct;
  m_ifSpline16.Construct;
  m_ifSpline36.Construct;
  m_ifBlackman144.Construct;

  m_imageFilterLut.Construct(@m_ifBilinear ,true );

  m_linearGradientFunction.Construct;
  m_radialGradientFunction.Construct;

  m_fillGradientInterpolator.Construct(@m_fillGradientMatrix );
  m_lineGradientInterpolator.Construct(@m_lineGradientMatrix );

  Pen.FAggLineWidth:=1;
  m_evenOddFlag:=false;

  m_transform.Construct;

  m_convStroke.Construct(@Path.m_convCurve );

  m_pathTransform.Construct  (@Path.m_convCurve ,@m_transform );
  m_strokeTransform.Construct(@m_convStroke ,@m_transform );

  {$IFDEF AGG2D_USE_FREETYPE }
  m_fontEngine.Construct;
  {$ENDIF }
  {$IFDEF AGG2D_USE_WINFONTS}
  m_fontDC:=GetDC(0);
  m_fontEngine.Construct(m_fontDC);
  {$ENDIF }
  {$IFNDEF AGG2D_NO_FONT}
  m_fontCacheManager.Construct(@m_fontEngine);
  {$ENDIF}

  m_convStroke.line_cap_(Pen.AggLineCap);
  m_convStroke.line_join_(Pen.AggLineJoin);

  ClearSettings;
end;

destructor TAggFPCanvas.Destroy;
begin
  m_rbuf.Destruct;

  m_allocator.Destruct;

  m_scanline.Destruct;
  m_rasterizer.Destruct;

  m_fillGradient.Destruct;
  m_lineGradient.Destruct;

  m_imageFilterLut.Destruct;

  m_convStroke.Destruct;

  {$IFNDEF AGG2D_NO_FONT}
  m_fontEngine.Destruct;
  m_fontCacheManager.Destruct;
  {$ENDIF}

  {$IFDEF AGG2D_USE_WINFONTS}
  ReleaseDC(0,m_fontDC);
  {$ENDIF }

  FAggPath.DeallocateResources;
  FreeAndNil(FAggPath);

  FreeAndNil(FImage);
  inherited Destroy;
  // set resources to nil, so that dangling pointers are spotted early
  FAggFont:=nil;
  FAggPen:=nil;
  FAggBrush:=nil;
end;

procedure TAggFPCanvas.AggResetPath;
begin
  Path.m_path.remove_all;
  Path.m_path.move_to(0,0);
end;

procedure TAggFPCanvas.AggMoveTo(const x, y: double);
begin
  Path.m_path.move_to(x ,y );
end;

procedure TAggFPCanvas.AggMoveRel(const dx, dy: double);
begin
  Path.m_path.move_rel(dx ,dy );
end;

procedure TAggFPCanvas.AggLineTo(const x, y: double);
begin
  Path.m_path.line_to(x ,y );
end;

procedure TAggFPCanvas.AggLineRel(const dx, dy: double);
begin
  Path.m_path.line_rel(dx ,dy );
end;

procedure TAggFPCanvas.AggHorLineTo(const x: double);
begin
  Path.m_path.hline_to(x );
end;

procedure TAggFPCanvas.AggHorLineRel(const dx: double);
begin
  Path.m_path.hline_rel(dx );
end;

procedure TAggFPCanvas.AggVerLineTo(const y: double);
begin
  Path.m_path.vline_to(y );
end;

procedure TAggFPCanvas.AggVerLineRel(const dy: double);
begin
  Path.m_path.vline_rel(dy );
end;

procedure TAggFPCanvas.AggArcTo(const rx, ry, angle: double; largeArcFlag,
  sweepFlag: boolean; const x, y: double);
begin
  Path.m_path.arc_to(rx ,ry ,angle ,largeArcFlag ,sweepFlag ,x ,y );
end;

procedure TAggFPCanvas.AggArcRel(const rx, ry, angle: double; largeArcFlag,
  sweepFlag: boolean; const dx, dy: double);
begin
  Path.m_path.arc_rel(rx ,ry ,angle ,largeArcFlag ,sweepFlag ,dx ,dy );
end;

procedure TAggFPCanvas.AggQuadricCurveTo(const xCtrl, yCtrl, xTo, yTo: double);
begin
  Path.m_path.curve3(xCtrl ,yCtrl ,xTo ,yTo );
end;

procedure TAggFPCanvas.AggQuadricCurveRel(const dxCtrl, dyCtrl, dxTo,
  dyTo: double);
begin
  Path.m_path.curve3_rel(dxCtrl ,dyCtrl ,dxTo ,dyTo );
end;

procedure TAggFPCanvas.AggQuadricCurveTo(const xTo, yTo: double);
begin
  Path.m_path.curve3(xTo ,yTo );
end;

procedure TAggFPCanvas.AggQuadricCurveRel(const dxTo, dyTo: double);
begin
  Path.m_path.curve3_rel(dxTo ,dyTo );
end;

procedure TAggFPCanvas.AggCubicCurveTo(const xCtrl1, yCtrl1, xCtrl2, yCtrl2,
  xTo, yTo: double);
begin
  Path.m_path.curve4(xCtrl1 ,yCtrl1 ,xCtrl2 ,yCtrl2 ,xTo ,yTo );
end;

procedure TAggFPCanvas.AggCubicCurveRel(const dxCtrl1, dyCtrl1, dxCtrl2,
  dyCtrl2, dxTo, dyTo: double);
begin
  Path.m_path.curve4_rel(dxCtrl1 ,dyCtrl1 ,dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo );
end;

procedure TAggFPCanvas.AggCubicCurveTo(const xCtrl2, yCtrl2, xTo, yTo: double);
begin
  Path.m_path.curve4(xCtrl2 ,yCtrl2 ,xTo ,yTo );
end;

procedure TAggFPCanvas.AggCubicCurveRel(const dxCtrl2, dyCtrl2, dxTo,
  dyTo: double);
begin
  Path.m_path.curve4_rel(dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo );
end;

procedure TAggFPCanvas.AggAddEllipse(const cx, cy, rx, ry: double;
  dir: TAggDirection);
var
  ar : bezier_arc;
begin
  if dir = AGG_CCW then
    ar.Construct(cx ,cy ,rx ,ry ,0 ,2 * pi )
  else
    ar.Construct(cx ,cy ,rx ,ry ,0 ,-2 * pi );

  Path.m_path.add_path(@ar ,0 ,false );
  AggClosePolygon;
end;

procedure TAggFPCanvas.AggClosePolygon;
begin
  Path.m_path.close_polygon;
end;

procedure TAggFPCanvas.AggDrawPath(flag: TAggDrawPathFlag; UseFont: boolean);
begin
  m_rasterizer.reset;
  if flag in [AGG_FillOnly,AGG_FillAndStroke] then begin
    if (not UseFont and (Brush.FAggColor.a <> 0))
    or (UseFont and (Font.FAggColor.a <> 0)) then
    begin
      m_rasterizer.add_path(@m_pathTransform );
      render(true,UseFont);
    end;
  end;
  if flag in [AGG_StrokeOnly,AGG_FillAndStroke] then begin
    if (not UseFont and (Pen.FAggColor.a <> 0 ) and (Pen.FAggLineWidth > 0.0 ))
    or (UseFont and (Font.FAggColor.a<>0)) then
    begin
      m_rasterizer.add_path(@m_strokeTransform );
      render(false,UseFont);
    end;
  end;
  if flag=AGG_FillWithLineColor then begin
    if (not UseFont and (Pen.FAggColor.a <> 0))
    or (UseFont and (Font.FAggColor.a<>0)) then
    begin
      m_rasterizer.add_path(@m_pathTransform);
      render(false,UseFont);
    end;
  end;
end;

procedure TAggFPCanvas.AggLine(const x1, y1, x2, y2: double);
begin
  Path.m_path.remove_all;

  addLine (x1 ,y1 ,x2 ,y2 );
  AggDrawPath(AGG_StrokeOnly );
end;

procedure TAggFPCanvas.AggTriangle(const x1, y1, x2, y2, x3, y3: double);
begin
  Path.m_path.remove_all;
  Path.m_path.move_to(x1 ,y1 );
  Path.m_path.line_to(x2 ,y2 );
  Path.m_path.line_to(x3 ,y3 );
  Path.m_path.close_polygon;

  AggDrawPath(AGG_FillAndStroke );
end;

procedure TAggFPCanvas.AggRectangle(const x1, y1, x2, y2: double);
begin
  Path.m_path.remove_all;
  Path.m_path.move_to(x1 ,y1 );
  Path.m_path.line_to(x2 ,y1 );
  Path.m_path.line_to(x2 ,y2 );
  Path.m_path.line_to(x1 ,y2 );
  Path.m_path.close_polygon;

  AggDrawPath(AGG_FillAndStroke );
end;

procedure TAggFPCanvas.AggRoundedRect(const x1, y1, x2, y2, r: double);
var
  rc : rounded_rect;
begin
  Path.m_path.remove_all;
  rc.Construct(x1 ,y1 ,x2 ,y2 ,r );

  rc.normalize_radius;
  rc.approximation_scale_(AggWorldToScreen(1.0 ) * g_approxScale );

  Path.m_path.add_path(@rc ,0 ,false );

  AggDrawPath(AGG_FillAndStroke );
end;

procedure TAggFPCanvas.AggRoundedRect(const x1, y1, x2, y2, rx, ry: double);
var
  rc : rounded_rect;
begin
  Path.m_path.remove_all;
  rc.Construct;

  rc.rect  (x1 ,y1 ,x2 ,y2 );
  rc.radius(rx ,ry );
  rc.normalize_radius;

  Path.m_path.add_path(@rc ,0 ,false );

  AggDrawPath(AGG_FillAndStroke );
end;

procedure TAggFPCanvas.AggRoundedRect(const x1, y1, x2, y2, rxBottom, ryBottom,
  rxTop, ryTop: double);
var
  rc : rounded_rect;
begin
  Path.m_path.remove_all;
  rc.Construct;

  rc.rect  (x1 ,y1 ,x2 ,y2 );
  rc.radius(rxBottom ,ryBottom ,rxTop ,ryTop );
  rc.normalize_radius;

  rc.approximation_scale_(AggWorldToScreen(1.0 ) * g_approxScale );

  Path.m_path.add_path(@rc ,0 ,false );

  AggDrawPath(AGG_FillAndStroke );
end;

procedure TAggFPCanvas.AggEllipse(const cx, cy, rx, ry: double);
var
  el : bezier_arc;
begin
  Path.m_path.remove_all;

  el.Construct(cx ,cy ,rx ,ry ,0 ,2 * pi );

  Path.m_path.add_path(@el ,0 ,false );
  AggClosePolygon;

  AggDrawPath(AGG_FillAndStroke );
end;

procedure TAggFPCanvas.AggArc(const cx, cy, rx, ry, start, endangle: double);
var
  ar : agg_arc.arc;
begin
  Path.m_path.remove_all;

  ar.Construct(cx ,cy ,rx ,ry ,endangle ,start ,false );

  Path.m_path.add_path(@ar ,0 ,false );

  AggDrawPath(AGG_StrokeOnly );
end;

procedure TAggFPCanvas.AggStar(const cx, cy, r1, r2, startAngle: double;
  numRays: integer);
var
  da, a, x, y: double;
  i : int;
begin
  Path.m_path.remove_all;

  da:=pi / numRays;
  a :=startAngle;

  i:=0;

  while i < numRays do begin
    x:=Cos(a) * r2 + cx;
    y:=Sin(a) * r2 + cy;

    if i <> 0 then
      Path.m_path.line_to(x ,y)
    else
      Path.m_path.move_to(x ,y);

    a:=a + da;

    Path.m_path.line_to(Cos(a ) * r1 + cx ,Sin(a ) * r1 + cy);

    a:=a + da;

    inc(i);
  end;

  AggClosePolygon;
  AggDrawPath(AGG_FillAndStroke );
end;

procedure TAggFPCanvas.AggCurve(const x1, y1, x2, y2, x3, y3: double);
begin
  Path.m_path.remove_all;
  Path.m_path.move_to(x1 ,y1 );
  Path.m_path.curve3 (x2 ,y2 ,x3 ,y3 );

  AggDrawPath(AGG_StrokeOnly );
end;

procedure TAggFPCanvas.AggCurve(const x1, y1, x2, y2, x3, y3, x4, y4: double);
begin
  Path.m_path.remove_all;
  Path.m_path.move_to(x1 ,y1 );
  Path.m_path.curve4 (x2 ,y2 ,x3 ,y3 ,x4 ,y4 );

  AggDrawPath(AGG_StrokeOnly );
end;

procedure TAggFPCanvas.AggPolygon(const xy: PDouble; numPoints: integer);
begin
  Path.m_path.remove_all;
  Path.m_path.add_poly(double_2_ptr(xy ) ,numPoints );

  AggClosePolygon;
  AggDrawPath(AGG_FillAndStroke );
end;

procedure TAggFPCanvas.AggPolyline(const xy: PDouble; numPoints: integer);
begin
  Path.m_path.remove_all;
  Path.m_path.add_poly(double_2_ptr(xy ) ,numPoints );

  AggDrawPath(AGG_StrokeOnly );
end;

procedure TAggFPCanvas.AggFillLinearGradient(const x1, y1, x2, y2: double; c1,
  c2: TAggColor; profile: double);
var
  i ,startGradient ,endGradient : int;

  k ,angle : double;

  c : TAggColor;

  clr : aggclr;
  tar : trans_affine_rotation;
  tat : trans_affine_translation;
begin
  startGradient:=128 - Trunc(profile * 127.0 );
  endGradient  :=128 + Trunc(profile * 127.0 );

  if endGradient <= startGradient then
  endGradient:=startGradient + 1;

  k:=1.0 / (endGradient - startGradient );
  i:=0;

  while i < startGradient do
  begin
    clr.Construct(c1 );

    move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
    inc (i );
  end;

  while i < endGradient do
  begin
    c:=c1.gradient(c2 ,(i - startGradient ) * k );

    clr.Construct(c );

    move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
    inc (i );
  end;

  while i < 256 do
  begin
    clr.Construct(c2 );

    move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
    inc (i );
  end;

  angle:=ArcTan2(y2 - y1 ,x2 - x1 );

  m_fillGradientMatrix.reset;

  tar.Construct(angle );

  m_fillGradientMatrix.multiply(@tar );

  tat.Construct(x1 ,y1 );

  m_fillGradientMatrix.multiply(@tat );
  m_fillGradientMatrix.multiply(@m_transform );
  m_fillGradientMatrix.invert;

  m_fillGradientD1  :=0.0;
  m_fillGradientD2  :=Sqrt((x2 - x1 ) * (x2 - x1 ) + (y2 - y1 ) * (y2 - y1 ) );
  m_fillGradientFlag:=AGG_Linear;
end;

procedure TAggFPCanvas.AggLineLinearGradient(const x1, y1, x2, y2: double; c1,
  c2: TAggColor; profile: double);
var
  i ,startGradient ,endGradient : int;

  k ,angle : double;

  c : TAggColor;

  clr : aggclr;
  tar : trans_affine_rotation;
  tat : trans_affine_translation;

begin
  startGradient:=128 - Trunc(profile * 128.0 );
  endGradient  :=128 + Trunc(profile * 128.0 );

  if endGradient <= startGradient then
  endGradient:=startGradient + 1;

  k:=1.0 / (endGradient - startGradient );
  i:=0;

  while i < startGradient do
  begin
    clr.Construct(c1 );

    move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
    inc (i );
  end;

  while i < endGradient do
  begin
    c:=c1.gradient(c2 ,(i - startGradient) * k );

    clr.Construct(c );

    move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
    inc (i );
  end;

  while i < 256 do
  begin
    clr.Construct(c2 );

    move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
    inc (i );
  end;

  angle:=ArcTan2(y2 - y1 ,x2 - x1 );

  m_lineGradientMatrix.reset;

  tar.Construct(angle );

  m_lineGradientMatrix.multiply(@tar );

  tat.Construct(x1 ,y1 );

  m_lineGradientMatrix.multiply(@tat );
  m_lineGradientMatrix.multiply(@m_transform );
  m_lineGradientMatrix.invert;

  m_lineGradientD1  :=0.0;
  m_lineGradientD2  :=Sqrt((x2 - x1 ) * (x2 - x1 ) + (y2 - y1 ) * (y2 - y1 ) );
  m_lineGradientFlag:=AGG_Linear;

  Pen.FPColor:=colBlack;  // Set some real color
end;

procedure TAggFPCanvas.AggFillRadialGradient(const x, y, r: double; c1,
  c2: TAggColor; profile: double);
var
  i ,startGradient ,endGradient : int;

  k : double;
  c : TAggColor;

  clr : aggclr;
  tat : trans_affine_translation;

begin
  startGradient:=128 - Trunc(profile * 127.0 );
  endGradient  :=128 + Trunc(profile * 127.0 );

  if endGradient <= startGradient then
  endGradient:=startGradient + 1;

  k:=1.0 / (endGradient - startGradient );
  i:=0;

  while i < startGradient do
  begin
    clr.Construct(c1 );

    move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
    inc (i );
  end;

  while i < endGradient do
  begin
    c:=c1.gradient(c2 ,(i - startGradient ) * k );

    clr.Construct(c );

    move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
    inc (i );
  end;

  while i < 256 do
  begin
    clr.Construct(c2 );

    move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
    inc (i );
  end;

  m_fillGradientD2:=AggWorldToScreen(r );

  AggWorldToScreen(@x ,@y );

  m_fillGradientMatrix.reset;

  tat.Construct(x ,y );

  m_fillGradientMatrix.multiply(@tat );
  m_fillGradientMatrix.invert;

  m_fillGradientD1  :=0;
  m_fillGradientFlag:=AGG_Radial;
end;

procedure TAggFPCanvas.AggLineRadialGradient(const x, y, r: double; c1,
  c2: TAggColor; profile: double);
var
  i ,startGradient ,endGradient : int;

  k : double;
  c : TAggColor;

  clr : aggclr;
  tat : trans_affine_translation;

begin
  startGradient:=128 - Trunc(profile * 128.0 );
  endGradient  :=128 + Trunc(profile * 128.0 );

  if endGradient <= startGradient then
  endGradient:=startGradient + 1;

  k:=1.0 / (endGradient - startGradient );
  i:=0;

  while i < startGradient do
  begin
    clr.Construct(c1 );

    move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
    inc (i );
  end;

  while i < endGradient do
  begin
    c:=c1.gradient(c2 ,(i - startGradient ) * k );

    clr.Construct(c );

    move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
    inc (i );
  end;

  while i < 256 do
  begin
    clr.Construct(c2 );

    move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
    inc (i );
  end;

  m_lineGradientD2:=AggWorldToScreen(r );

  AggWorldToScreen(@x ,@y );

  m_lineGradientMatrix.reset;

  tat.Construct(x ,y );

  m_lineGradientMatrix.multiply(@tat );
  m_lineGradientMatrix.invert;

  m_lineGradientD1  :=0;
  m_lineGradientFlag:=AGG_Radial;

  Pen.FPColor:=colBlack;  // Set some real color
end;

procedure TAggFPCanvas.AggFillRadialGradient(const x, y, r: double; c1, c2,
  c3: TAggColor);
var
  i : int;
  c : TAggColor;

  clr : aggclr;
  tat : trans_affine_translation;

begin
  i:=0;

  while i < 128 do
  begin
    c:=c1.gradient(c2 ,i / 127.0 );

    clr.Construct(c );

    move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
    inc (i );
  end;

  while i < 256 do
  begin
    c:=c2.gradient(c3 ,(i - 128 ) / 127.0 );

    clr.Construct(c );

    move(clr ,m_fillGradient.array_operator(i )^ ,sizeof(aggclr ) );
    inc (i );
  end;

  m_fillGradientD2:=AggWorldToScreen(r );

  AggWorldToScreen(@x ,@y );

  m_fillGradientMatrix.reset;

  tat.Construct(x ,y );

  m_fillGradientMatrix.multiply(@tat );
  m_fillGradientMatrix.invert;

  m_fillGradientD1  :=0;
  m_fillGradientFlag:=AGG_Radial;
end;

procedure TAggFPCanvas.AggLineRadialGradient(const x, y, r: double; c1, c2,
  c3: TAggColor);
var
  i : int;
  c : TAggColor;

  clr : aggclr;
  tat : trans_affine_translation;

begin
  i:=0;

  while i < 128 do
  begin
    c:=c1.gradient(c2 ,i / 127.0 );

    clr.Construct(c );

    move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
    inc (i );
  end;

  while i < 256 do
  begin
    c:=c2.gradient(c3 ,(i - 128 ) / 127.0 );

    clr.Construct(c );

    move(clr ,m_lineGradient.array_operator(i )^ ,sizeof(aggclr ) );
    inc (i );
  end;

  m_lineGradientD2:=AggWorldToScreen(r );

  AggWorldToScreen(@x ,@y );

  m_lineGradientMatrix.reset;

  tat.Construct(x ,y );

  m_lineGradientMatrix.multiply(@tat );
  m_lineGradientMatrix.invert;

  m_lineGradientD1  :=0;
  m_lineGradientFlag:=AGG_Radial;

  Pen.FPColor:=colBlack; // Set some real color
end;

procedure TAggFPCanvas.AggFillRadialGradient(const x, y, r: double);
var
  tat : trans_affine_translation;

begin
  m_fillGradientD2:=AggWorldToScreen(r );

  AggWorldToScreen(@x ,@y );

  m_fillGradientMatrix.reset;

  tat.Construct(x ,y );

  m_fillGradientMatrix.multiply(@tat );
  m_fillGradientMatrix.invert;

  m_fillGradientD1:=0;
end;

procedure TAggFPCanvas.AggLineRadialGradient(const x, y, r: double);
var
  tat : trans_affine_translation;

begin
  m_lineGradientD2:=AggWorldToScreen(r );

  AggWorldToScreen(@x ,@y );

  m_lineGradientMatrix.reset;

  tat.Construct(x ,y );

  m_lineGradientMatrix.multiply(@tat );
  m_lineGradientMatrix.invert;

  m_lineGradientD1:=0;
end;

procedure TAggFPCanvas.AggSetClipBox(const x1, y1, x2, y2: double);
var
  rx1 ,ry1 ,rx2 ,ry2 : int;
begin
  m_clipBox.Construct(x1 ,y1 ,x2 ,y2 );

  rx1:=Trunc(x1 );
  ry1:=Trunc(y1 );
  rx2:=Trunc(x2 );
  ry2:=Trunc(y2 );

  m_renBase.clip_box_       (rx1 ,ry1 ,rx2 ,ry2 );
  m_renBaseComp.clip_box_   (rx1 ,ry1 ,rx2 ,ry2 );
  m_renBasePre.clip_box_    (rx1 ,ry1 ,rx2 ,ry2 );
  m_renBaseCompPre.clip_box_(rx1 ,ry1 ,rx2 ,ry2 );

  m_rasterizer.clip_box(x1 ,y1 ,x2 ,y2 );
end;

function TAggFPCanvas.AggGetClipBox: TAggRectD;
begin
  result:=m_clipBox;
end;

procedure TAggFPCanvas.AggClearClipBox(const c: TAggColor);
var
  clr : aggclr;
begin
  clr.Construct(c );
  m_renBase.copy_bar(0 ,0 ,m_renBase.width ,m_renBase.height ,@clr );
end;

procedure TAggFPCanvas.AggClearClipBox(r, g, b: byte; a: byte);
var
  clr : TAggColor;
begin
  clr.Construct(r ,g ,b ,a );
  AggClearClipBox (clr );
end;

function TAggFPCanvas.AggInClipBox(const worldX, worldY: double): boolean;
begin
  AggWorldToScreen(@worldX ,@worldY );
  result:=m_renBase.inbox(Trunc(worldX ) ,Trunc(worldY ) );
end;

procedure TAggFPCanvas.AggResetTransformations;
begin
  m_transform.reset;
end;

procedure TAggFPCanvas.AggAffine(const tr: PAggAffine);
begin
  m_transform.multiply(tr);

  Path.m_convCurve.approximation_scale_ (AggWorldToScreen(1.0 ) * g_approxScale );
  m_convStroke.approximation_scale_(AggWorldToScreen(1.0 ) * g_approxScale );
end;

procedure TAggFPCanvas.AggAffine(const tr: PAggTransformations);
var
 ta : trans_affine;
begin
  ta.Construct(
    tr^.affineMatrix[0 ] ,tr^.affineMatrix[1 ] ,tr^.affineMatrix[2 ] ,
    tr^.affineMatrix[3 ] ,tr^.affineMatrix[4 ] ,tr^.affineMatrix[5 ] );

  AggAffine(PAggAffine(@ta ) );
end;

procedure TAggFPCanvas.AggRotate(const angle: double);
var
  tar : trans_affine_rotation;
begin
  tar.Construct(angle );
  m_transform.multiply(@tar );
end;

procedure TAggFPCanvas.AggScale(const sx, sy: double);
var
  tas : trans_affine_scaling;
begin
  tas.Construct(sx ,sy );

  m_transform.multiply(@tas );

  Path.m_convCurve.approximation_scale_ (AggWorldToScreen(1.0 ) * g_approxScale );
  m_convStroke.approximation_scale_(AggWorldToScreen(1.0 ) * g_approxScale );
end;

procedure TAggFPCanvas.AggSkew(const sx, sy: double);
var
  tas : trans_affine_skewing;
begin
  tas.Construct(sx ,sy );
  m_transform.multiply(@tas );
end;

procedure TAggFPCanvas.AggTranslate(const x, y: double);
var
  tat : trans_affine_translation;
begin
  tat.Construct(x ,y );
  m_transform.multiply(@tat );
end;

procedure TAggFPCanvas.AggParallelogram(const x1, y1, x2, y2: double; para: PDouble
  );
var
  ta : trans_affine;
begin
  ta.Construct(x1 ,y1 ,x2 ,y2 ,parallelo_ptr(para ) );

  m_transform.multiply(@ta );

  Path.m_convCurve.approximation_scale_ (AggWorldToScreen(1.0 ) * g_approxScale );
  m_convStroke.approximation_scale_(AggWorldToScreen(1.0 ) * g_approxScale );
end;

procedure TAggFPCanvas.AggViewport(const worldX1, worldY1, worldX2, worldY2,
  screenX1, screenY1, screenX2, screenY2: double; const opt: TAggViewportOption
  );
var
  vp : trans_viewport;
  mx : trans_affine;

begin
  vp.Construct;

  case opt of
  AGG_Anisotropic :
   vp.preserve_aspect_ratio(0.0 ,0.0 ,aspect_ratio_stretch );

  AGG_XMinYMin :
   vp.preserve_aspect_ratio(0.0 ,0.0 ,aspect_ratio_meet );

  AGG_XMidYMin :
   vp.preserve_aspect_ratio(0.5 ,0.0 ,aspect_ratio_meet );

  AGG_XMaxYMin :
   vp.preserve_aspect_ratio(1.0 ,0.0 ,aspect_ratio_meet );

  AGG_XMinYMid :
   vp.preserve_aspect_ratio(0.0 ,0.5 ,aspect_ratio_meet );

  AGG_XMidYMid :
   vp.preserve_aspect_ratio(0.5 ,0.5 ,aspect_ratio_meet );

  AGG_XMaxYMid :
   vp.preserve_aspect_ratio(1.0 ,0.5 ,aspect_ratio_meet );

  AGG_XMinYMax :
   vp.preserve_aspect_ratio(0.0 ,1.0 ,aspect_ratio_meet );

  AGG_XMidYMax :
   vp.preserve_aspect_ratio(0.5 ,1.0 ,aspect_ratio_meet );

  AGG_XMaxYMax :
   vp.preserve_aspect_ratio(1.0 ,1.0 ,aspect_ratio_meet );

  end;

  vp.world_viewport (worldX1  ,worldY1  ,worldX2  ,worldY2 );
  vp.device_viewport(screenX1 ,screenY1 ,screenX2 ,screenY2 );

  mx.Construct;

  vp.to_affine        (@mx );
  m_transform.multiply(@mx );

  Path.m_convCurve.approximation_scale_ (AggWorldToScreen(1.0 ) * g_approxScale );
  m_convStroke.approximation_scale_(AggWorldToScreen(1.0 ) * g_approxScale );
end;

procedure TAggFPCanvas.AggWorldToScreen(x, y: PDouble);
begin
  m_transform.transform(@m_transform ,double_ptr(x ) ,double_ptr(y ) );
end;

procedure TAggFPCanvas.AggScreenToWorld(x, y: PDouble);
begin
  m_transform.inverse_transform(@m_transform ,double_ptr(x ) ,double_ptr(y ) );
end;

function TAggFPCanvas.AggWorldToScreen(const scalar: double): double;
var
  x1 ,y1 ,x2 ,y2 : double;
begin
  x1:=0;
  y1:=0;
  x2:=scalar;
  y2:=scalar;

  AggWorldToScreen(@x1 ,@y1 );
  AggWorldToScreen(@x2 ,@y2 );

  result:=Sqrt((x2 - x1 ) * (x2 - x1 ) + (y2 - y1 ) * (y2 - y1 ) ) * 0.7071068;
end;

function TAggFPCanvas.AggScreenToWorld(const scalar: double): double;
var
  x1 ,y1 ,x2 ,y2 : double;
begin
  x1:=0;
  y1:=0;
  x2:=scalar;
  y2:=scalar;

  AggScreenToWorld(@x1 ,@y1 );
  AggScreenToWorld(@x2 ,@y2 );

  result:=Sqrt((x2 - x1 ) * (x2 - x1 ) + (y2 - y1 ) * (y2 - y1 ) ) * 0.7071068;
end;

procedure TAggFPCanvas.AggAlignPoint(x, y: PDouble);
begin
  AggWorldToScreen(x ,y );

  x^:=Floor(x^ ) + 0.5;
  y^:=Floor(y^ ) + 0.5;

  AggScreenToWorld(x ,y );
end;

procedure TAggFPCanvas.AggRenderImage(img: TAggFPImage; x1, y1, x2,
  y2: integer; parl: PDouble);
var
  mtx : trans_affine;
  interpolator : span_interpolator_linear;
begin
  mtx.Construct(x1 ,y1 ,x2 ,y2 ,parallelo_ptr(parl ) );
  mtx.multiply (@m_transform );
  mtx.invert;

  m_rasterizer.reset;
  m_rasterizer.add_path(@m_pathTransform );

  interpolator.Construct(@mtx );

  if (m_blendMode = AGG_BlendAlpha ) or
    (Image.PixelFormat = afpimRGB24 ) then
    Agg2DRenderer_renderImage(img ,@m_renBasePre ,@interpolator )
  else
    Agg2DRenderer_renderImage(img ,@m_renBaseCompPre ,@interpolator );
end;

procedure TAggFPCanvas.AggTransformImage(SrcImage: TAggFPImage; const imgX1,
  imgY1, imgX2, imgY2: integer; const dstX1, dstY1, dstX2, dstY2: double);
var
  parall : array[0..5 ] of double;
begin
  AggResetPath;
  AggMoveTo(dstX1 ,dstY1 );
  AggLineTo(dstX2 ,dstY1 );
  AggLineTo(dstX2 ,dstY2 );
  AggLineTo(dstX1 ,dstY2 );
  AggClosePolygon;

  parall[0 ]:=dstX1;
  parall[1 ]:=dstY1;
  parall[2 ]:=dstX2;
  parall[3 ]:=dstY1;
  parall[4 ]:=dstX2;
  parall[5 ]:=dstY2;

  AggRenderImage(SrcImage ,imgX1 ,imgY1 ,imgX2 ,imgY2 ,@parall[0 ] );
end;

procedure TAggFPCanvas.AggTransformImage(SrcImage: TAggFPImage; const dstX1,
  dstY1, dstX2, dstY2: double);
var
  parall : array[0..5 ] of double;
begin
  AggResetPath;
  AggMoveTo(dstX1 ,dstY1 );
  AggLineTo(dstX2 ,dstY1 );
  AggLineTo(dstX2 ,dstY2 );
  AggLineTo(dstX1 ,dstY2 );
  AggClosePolygon;

  parall[0 ]:=dstX1;
  parall[1 ]:=dstY1;
  parall[2 ]:=dstX2;
  parall[3 ]:=dstY1;
  parall[4 ]:=dstX2;
  parall[5 ]:=dstY2;

  AggRenderImage(SrcImage ,0 ,0 ,SrcImage.Width ,SrcImage.Height ,@parall[0 ] );
end;

procedure TAggFPCanvas.AggTransformImage(SrcImage: TAggFPImage; imgX1, imgY1,
  imgX2, imgY2: integer; parallelo: PDouble);
begin
  AggResetPath;

  AggMoveTo(
    PDouble(ptrcomp(parallelo ) + 0 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 1 * sizeof(double ) )^ );

  AggLineTo(
    PDouble(ptrcomp(parallelo ) + 2 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 3 * sizeof(double ) )^ );

  AggLineTo(
    PDouble(ptrcomp(parallelo ) + 4 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 5 * sizeof(double ) )^ );

  AggLineTo(
    PDouble(ptrcomp(parallelo ) + 0 * sizeof(double ) )^ +
    PDouble(ptrcomp(parallelo ) + 4 * sizeof(double ) )^ -
    PDouble(ptrcomp(parallelo ) + 2 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 1 * sizeof(double ) )^ +
    PDouble(ptrcomp(parallelo ) + 5 * sizeof(double ) )^ -
    PDouble(ptrcomp(parallelo ) + 3 * sizeof(double ) )^ );

  AggClosePolygon;

  AggRenderImage(SrcImage ,imgX1 ,imgY1 ,imgX2 ,imgY2 ,parallelo );
end;

procedure TAggFPCanvas.AggTransformImage(SrcImage: TAggFPImage; parallelo: PDouble
  );
begin
  AggResetPath;

  AggMoveTo(
    PDouble(ptrcomp(parallelo ) + 0 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 1 * sizeof(double ) )^ );

  AggLineTo(
    PDouble(ptrcomp(parallelo ) + 2 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 3 * sizeof(double ) )^ );

  AggLineTo(
    PDouble(ptrcomp(parallelo ) + 4 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 5 * sizeof(double ) )^ );

  AggLineTo(
    PDouble(ptrcomp(parallelo ) + 0 * sizeof(double ) )^ +
    PDouble(ptrcomp(parallelo ) + 4 * sizeof(double ) )^ -
    PDouble(ptrcomp(parallelo ) + 2 * sizeof(double ) )^ ,
    PDouble(ptrcomp(parallelo ) + 1 * sizeof(double ) )^ +
    PDouble(ptrcomp(parallelo ) + 5 * sizeof(double ) )^ -
    PDouble(ptrcomp(parallelo ) + 3 * sizeof(double ) )^ );

  AggClosePolygon;

  AggRenderImage(SrcImage ,0 ,0 ,SrcImage.Width ,SrcImage.Height ,parallelo );
end;

procedure TAggFPCanvas.AggTransformImagePath(SrcImage: TAggFPImage; const imgX1,
  imgY1, imgX2, imgY2: integer; const dstX1, dstY1, dstX2, dstY2: double);
var
  parall : array[0..5 ] of double;
begin
  parall[0 ]:=dstX1;
  parall[1 ]:=dstY1;
  parall[2 ]:=dstX2;
  parall[3 ]:=dstY1;
  parall[4 ]:=dstX2;
  parall[5 ]:=dstY2;

  AggRenderImage(SrcImage ,imgX1 ,imgY1 ,imgX2 ,imgY2 ,@parall[0 ] );
end;

procedure TAggFPCanvas.AggTransformImagePath(SrcImage: TAggFPImage; const dstX1,
  dstY1, dstX2, dstY2: double);
var
  parall : array[0..5 ] of double;
begin
  parall[0 ]:=dstX1;
  parall[1 ]:=dstY1;
  parall[2 ]:=dstX2;
  parall[3 ]:=dstY1;
  parall[4 ]:=dstX2;
  parall[5 ]:=dstY2;

  AggRenderImage(SrcImage ,0 ,0 ,SrcImage.Width ,SrcImage.Height ,@parall[0 ] );
end;

procedure TAggFPCanvas.AggTransformImagePath(SrcImage: TAggFPImage; const imgX1,
  imgY1, imgX2, imgY2: integer; const parallelo: PDouble);
begin
  AggRenderImage(SrcImage ,imgX1 ,imgY1 ,imgX2 ,imgY2 ,parallelo );
end;

procedure TAggFPCanvas.AggTransformImagePath(SrcImage: TAggFPImage;
  parallelo: PDouble);
begin
  AggRenderImage(SrcImage ,0 ,0 ,SrcImage.Width ,SrcImage.Height ,parallelo );
end;

procedure TAggFPCanvas.AggCopyImage(SrcImage: TAggFPImage; const imgX1, imgY1,
  imgX2, imgY2: integer; const dstX, dstY: double);
var
  r: agg_basics.rect;
begin
  AggWorldToScreen(@dstX ,@dstY );
  r.Construct  (imgX1 ,imgY1 ,imgX2 ,imgY2 );

  m_renBase.copy_from(@SrcImage.RenderingBuffer ,@r ,
                      Trunc(dstX ) - imgX1 ,Trunc(dstY ) - imgY1 );
end;

procedure TAggFPCanvas.AggCopyImage(SrcImage: TAggFPImage; const dstX,
  dstY: double);
begin
  AggWorldToScreen(@dstX ,@dstY );
  m_renBase.copy_from(@SrcImage.RenderingBuffer ,NIL ,Trunc(dstX ) ,Trunc(dstY ) );
end;

function TAggFPCanvas.AggTextWidth(str: AnsiString): double;
{$IFDEF AGG2D_NO_FONT}
begin

end;
{$ELSE}
var
  x ,y  : double;
  first : boolean;
  glyph : glyph_cache_ptr;
  str_  : PChar;
  charlen: int;
  char_id: int32u;
begin
  if str='' then exit(0);
  x:=0;
  y:=0;

  first:=true;
  str_ :=@str[1 ];

  while str_^ <> #0 do
  begin
    if UseUTF8 then
    begin
      char_id:=AggUTF8CharToUnicode(str_,charlen);
      inc(str_,charlen);
    end else begin
      char_id:=int32u(char_ptr(str_)^);
      inc(str_,sizeof(char));
    end;
    glyph:=m_fontCacheManager.glyph(char_id);

    if glyph <> NIL then
    begin
      if not first then
        m_fontCacheManager.add_kerning(@x ,@y );

      x:=x + glyph^.advance_x;
      y:=y + glyph^.advance_y;

      first:=false;
    end;

  end;

  if Font.FAggCache = AGG_VectorFontCache then
    result:=x
  else
    result:=AggScreenToWorld(x );
end;
{$ENDIF}

function TAggFPCanvas.AggTextHeight(str: AnsiString): double;
begin
  {$IFDEF AGG2D_NO_FONT}
  Result:=0;
  {$ELSE}
  Result:=m_fontEngine._height;
  {$ENDIF}
end;

procedure TAggFPCanvas.AggTextOut(const x, y: double; str: AnsiString;
  roundOff: boolean; const ddx: double; const ddy: double);
{$IFDEF AGG2D_NO_FONT}
begin

end;
{$ELSE}
var
  dx ,dy ,asc ,start_x ,start_y : double;

  glyph : glyph_cache_ptr;

  mtx  : trans_affine;
  str_ : PChar;

  tat : trans_affine_translation;
  tar : trans_affine_rotation;

  tr : conv_transform;
  charlen: int;
  char_id: int32u;
  First: Boolean;

begin
  if Str='' then exit;

  dx:=0.0;
  dy:=0.0;

  case Font.AggAlignX of
  AGG_AlignCenter :
    dx:=-AggTextWidth(str ) * 0.5;

  AGG_AlignRight :
    dx:=-AggTextWidth(str );

  end;

  asc  :=Font.AggHeight;
  glyph:=m_fontCacheManager.glyph(int32u('H' ) );

  if glyph <> NIL then
    asc:=glyph^.bounds.y2 - glyph^.bounds.y1;

  if Font.FAggCache = AGG_RasterFontCache then
    asc:=AggScreenToWorld(asc );

  case Font.AggAlignY of
  AGG_AlignCenter :
    dy:=-asc * 0.5;

  AGG_AlignTop :
    dy:=-asc;

  end;

  if m_fontEngine._flip_y then
    dy:=-dy;

  mtx.Construct;

  start_x:=x + dx;
  start_y:=y + dy;

  if roundOff then
  begin
    start_x:=Trunc(start_x );
    start_y:=Trunc(start_y );
  end;

  start_x:=start_x + ddx;
  start_y:=start_y + ddy;

  tat.Construct(-x ,-y );
  mtx.multiply (@tat );

  tar.Construct(Font.AggAngle );
  mtx.multiply (@tar );

  tat.Construct(x ,y );
  mtx.multiply (@tat );

  tr.Construct(m_fontCacheManager.path_adaptor ,@mtx );

  if Font.FAggCache = AGG_RasterFontCache then
    AggWorldToScreen(@start_x ,@start_y );

  str_:=@str[1 ];
  First:=true;

  while str_^ <> #0 do
  begin
    if UseUTF8 then
    begin
      char_id:=AggUTF8CharToUnicode(str_,charlen);
      inc(str_,charlen);
    end else begin
      char_id:=int32u(char_ptr(str_)^);
      inc(str_,sizeof(char));
    end;
    glyph:=m_fontCacheManager.glyph(char_id);

    if glyph <> NIL then
    begin
      if First then
      begin
        m_fontCacheManager.add_kerning(@x ,@y );
        First:=false;
      end;

      m_fontCacheManager.init_embedded_adaptors(glyph ,start_x ,start_y );

      if glyph^.data_type = glyph_data_outline then
      begin
        Path.m_path.remove_all;
        Path.m_path.add_path(@tr ,0 ,false );

        if Font.AggUseOnlyFont then
          AggDrawPath(AGG_FillOnly,true)
        else
          AggDrawPath(AGG_FillAndStroke,true);
      end;

      if glyph^.data_type = glyph_data_gray8 then
      begin
        Render(
          m_fontCacheManager.gray8_adaptor ,
          m_fontCacheManager.gray8_scanline );
      end;

      start_x:=start_x + glyph^.advance_x;
      start_y:=start_y + glyph^.advance_y;
    end;
  end;
end;
{$ENDIF}

{ AGG2DRENDERER_RENDER }
procedure TAggFPCanvas.Agg2DRenderer_render(
           renBase : renderer_base_ptr;
           renSolid : renderer_scanline_aa_solid_ptr;
           fillColor_ , UseFont: boolean );
var
 span : span_gradient;
 ren  : TAggFP_renderer_scanline_aa;
 clr  : aggclr;

begin
  if (fillColor_ and
     (m_fillGradientFlag = AGG_Linear ) ) or
    (not fillColor_ and
     (m_lineGradientFlag = AGG_Linear ) ) then
  if fillColor_ then
   begin
    span.Construct(
     @m_allocator ,
     @m_fillGradientInterpolator ,
     @m_linearGradientFunction ,
     @m_fillGradient ,
     m_fillGradientD1 ,
     m_fillGradientD2 );

    ren.Construct   (renBase ,@span );
    render_scanlines(@m_rasterizer ,@m_scanline ,@ren );

   end
  else
   begin
    span.Construct(
     @m_allocator ,
     @m_lineGradientInterpolator ,
     @m_linearGradientFunction ,
     @m_lineGradient ,
     m_lineGradientD1 ,
     m_lineGradientD2 );

    ren.Construct   (renBase ,@span );
    render_scanlines(@m_rasterizer ,@m_scanline ,@ren );

   end
  else
  if (fillColor_ and
      (m_fillGradientFlag = AGG_Radial ) ) or
     (not fillColor_ and
      (m_lineGradientFlag = AGG_Radial ) ) then
  begin
    if fillColor_ then
    begin
     span.Construct(
      @m_allocator ,
      @m_fillGradientInterpolator ,
      @m_radialGradientFunction ,
      @m_fillGradient ,
      m_fillGradientD1 ,
      m_fillGradientD2 );

      ren.Construct   (renBase ,@span );
      render_scanlines(@m_rasterizer ,@m_scanline ,@ren );

    end
    else
    begin
     span.Construct(
      @m_allocator ,
      @m_lineGradientInterpolator ,
      @m_radialGradientFunction ,
      @m_lineGradient ,
      m_lineGradientD1 ,
      m_lineGradientD2 );

     ren.Construct   (renBase ,@span );
     render_scanlines(@m_rasterizer ,@m_scanline ,@ren );

    end;
  end
  else
  begin
    if UseFont then
      clr.Construct(Font.FAggColor )
    else if fillColor_ then
      clr.Construct(Brush.FAggColor )
    else
      clr.Construct(Pen.FAggColor );

    renSolid^.color_ (@clr );
    render_scanlines(@m_rasterizer ,@m_scanline ,renSolid );
  end;
end;

procedure TAggFPCanvas.Agg2DRenderer_render(
           renBase : renderer_base_ptr;
           renSolid : renderer_scanline_aa_solid_ptr;
           ras : gray8_adaptor_type_ptr;
           sl : gray8_scanline_type_ptr;
           UseFont: boolean);
var
 span : span_gradient;
 ren  : renderer_scanline_aa;
 clr  : aggclr;

begin
  if UseFont then
  begin
    clr.Construct(Brush.fAggColor );
    renSolid^.color_ (@clr );
    render_scanlines(ras ,sl ,renSolid );
  end
  else if m_fillGradientFlag = AGG_Linear then
  begin
   span.Construct(
    @m_allocator ,
    @m_fillGradientInterpolator ,
    @m_linearGradientFunction ,
    @m_fillGradient ,
    m_fillGradientD1 ,
    m_fillGradientD2 );

   ren.Construct   (renBase ,@span );
   render_scanlines(ras ,sl ,@ren );

  end
  else
  if m_fillGradientFlag = AGG_Radial then
   begin
    span.Construct(
     @m_allocator ,
     @m_fillGradientInterpolator ,
     @m_radialGradientFunction ,
     @m_fillGradient ,
     m_fillGradientD1 ,
     m_fillGradientD2 );

    ren.Construct   (renBase ,@span );
    render_scanlines(ras ,sl ,@ren );

   end
  else
  begin
    clr.Construct(Brush.fAggColor );
    renSolid^.color_ (@clr );
    render_scanlines(ras ,sl ,renSolid );
  end;
end;

procedure TAggFPCanvas.addLine(const x1, y1, x2, y2: double);
begin
  Path.m_path.move_to(x1 ,y1 );
  Path.m_path.line_to(x2 ,y2 );
end;

function TAggFPCanvas.GetAggTransformations: TAggTransformations;
begin
  m_transform.store_to(@result.affineMatrix[0]);
end;

procedure TAggFPCanvas.Agg2DRenderer_renderImage(
           img : TAggFPImage;
           renBase : renderer_base_ptr;
           interpolator : span_interpolator_linear_ptr );
var
 blend : TAggSpanConvImageBlend;

 si : span_image_filter_rgba;
 sg : span_image_filter_rgba_nn;
 sb : span_image_filter_rgba_bilinear;
 s2 : span_image_filter_rgba_2x2;
 sa : span_image_resample_rgba_affine;
 sc : span_converter;
 ri : renderer_scanline_aa;

 clr : aggclr;

 resample : boolean;

 sx ,sy : double;

begin
 case Image.PixelFormat of
  afpimRGBA32 :
   blend.Construct(m_imageBlendMode ,m_imageBlendColor ,@m_pixFormatCompPre );

  else
   blend.Construct(m_imageBlendMode ,m_imageBlendColor ,NIL );

 end;

 if m_imageFilter = AGG_NoFilter then
  begin
   clr.ConstrInt(0 ,0 ,0 ,0 );
   sg.Construct (@m_allocator ,@img.RenderingBuffer ,@clr ,interpolator ,rgba_order );
   sc.Construct (@sg ,@blend );
   ri.Construct (renBase ,@sc );

   render_scanlines(@m_rasterizer ,@m_scanline ,@ri );

  end
 else
  begin
   resample:=m_imageResample = AGG_ResampleAlways;

   if m_imageResample = AGG_ResampleOnZoomOut then
    begin
     interpolator^._transformer^.scaling_abs(@sx ,@sy );

     if (sx > 1.125 ) or
        (sy > 1.125 ) then
      resample:=true;

    end;

   if resample then
    begin
     clr.ConstrInt(0 ,0 ,0 ,0 );
     sa.Construct(
      @m_allocator ,
      @img.RenderingBuffer ,
      @clr ,
      interpolator ,
      @m_imageFilterLut ,
      rgba_order );

     sc.Construct(@sa ,@blend );
     ri.Construct(renBase ,@sc );

     render_scanlines(@m_rasterizer ,@m_scanline ,@ri );

    end
   else
    if m_imageFilter = AGG_Bilinear then
     begin
      clr.ConstrInt(0 ,0 ,0 ,0 );
      sb.Construct(
       @m_allocator ,
       @img.RenderingBuffer ,
       @clr ,
       interpolator ,
       rgba_order );

      sc.Construct(@sb ,@blend );
      ri.Construct(renBase ,@sc );

      render_scanlines(@m_rasterizer ,@m_scanline ,@ri );

     end
    else
     if m_imageFilterLut.diameter = 2 then
      begin
       clr.ConstrInt(0 ,0 ,0 ,0 );
       s2.Construct(
        @m_allocator ,
        @img.RenderingBuffer ,
        @clr ,
        interpolator,
        @m_imageFilterLut ,
        rgba_order );

       sc.Construct(@s2 ,@blend );
       ri.Construct(renBase ,@sc );

       render_scanlines(@m_rasterizer ,@m_scanline ,@ri );

      end
     else
      begin
       clr.ConstrInt(0 ,0 ,0 ,0 );
       si.Construct(
        @m_allocator ,
        @img.RenderingBuffer ,
        @clr ,
        interpolator ,
        @m_imageFilterLut ,
        rgba_order );

       sc.Construct(@si ,@blend );
       ri.Construct(renBase ,@sc );

       render_scanlines(@m_rasterizer ,@m_scanline ,@ri );

      end;

  end;

end;

{ TAggFPPen }

procedure TAggFPPen.SetFPColor(const AValue: TFPColor);
begin
  if FPColor=AValue then exit;
  inherited SetFPColor(AValue);
  FAggColor:=FPToAggColor(AValue);
end;

procedure TAggFPPen.SetAggColor(const AValue: TAggColor);
begin
  FPColor:=AggToFPColor(AValue);
end;

procedure TAggFPPen.DoCopyProps(From: TFPCanvasHelper);
var
  Src: TAggFPPen;
begin
  inherited DoCopyProps(From);
  if From is TAggFPPen then begin
    Src:=TAggFPPen(From);
    FAggColor:=Src.FAggColor;
    AggLineCap:=Src.AggLineCap;
    AggLineJoin:=Src.AggLineJoin;
    AggLineWidth:=Src.AggLineWidth;
  end;
end;

constructor TAggFPPen.Create;
begin
  inherited Create;
  FAggLineCap:=AGG_CapRound;
  FAggLineJoin:=AGG_JoinRound;
  FAggLineWidth:=1.0;
end;

procedure TAggFPPen.SetAggLineCap(const AValue: TAggLineCap);
begin
  if FAggLineCap=AValue then exit;
  FAggLineCap:=AValue;
  TAggFPCanvas(Canvas).m_convStroke.line_cap_(FAggLineCap);
end;

procedure TAggFPPen.SetAggLineJoin(const AValue: TAggLineJoin);
begin
  if FAggLineJoin=AValue then exit;
  FAggLineJoin:=AValue;
  TAggFPCanvas(Canvas).m_convStroke.line_join_(FAggLineJoin);
end;

procedure TAggFPPen.SetAggLineWidth(const AValue: double);
begin
  if FAggLineWidth=AValue then exit;
  FAggLineWidth:=AValue;
  inherited SetWidth(round(AValue));
  TAggFPCanvas(Canvas).m_convStroke.width_(FAggLineWidth);
end;

procedure TAggFPPen.SetWidth(AValue: Integer);
var
  NewWidth: Double;
begin
  NewWidth:=double(AValue);
  if NewWidth=AggLineWidth then exit;
  inherited SetWidth(AValue);
  AggLineWidth:=NewWidth;
end;

{ CONSTRUCT }
constructor TAggRasterizerGamma.Construct(alpha ,gamma : double );
begin
  m_alpha.Construct(alpha );
  m_gamma.Construct(gamma );
end;

{ FUNC_OPERATOR_GAMMA }
function TAggRasterizerGamma.func_operator_gamma(x : double ) : double;
begin
  result:=m_alpha.func_operator_gamma(m_gamma.func_operator_gamma(x ) );
end;

function TAggRasterizerGamma.operator_array(i: unsigned): unsigned;
begin
  Result:=i;
end;

{ TAggFPBrush }

procedure TAggFPBrush.SetAggFillEvenOdd(const AValue: boolean);
begin
  if FAggFillEvenOdd=AValue then exit;
  FAggFillEvenOdd:=AValue;

  if FAggFillEvenOdd then
    TAggFPCanvas(Canvas).m_rasterizer.filling_rule(fill_even_odd )
  else
    TAggFPCanvas(Canvas).m_rasterizer.filling_rule(fill_non_zero );
end;

procedure TAggFPBrush.DoCopyProps(From: TFPCanvasHelper);
var
  Src: TAggFPBrush;
begin
  inherited DoCopyProps(From);
  if From is TAggFPBrush then begin
    Src:=TAggFPBrush(From);
    FAggColor:=Src.FAggColor;
    AggFillEvenOdd:=Src.AggFillEvenOdd;
  end;
end;

procedure TAggFPBrush.SetFPColor(const AValue: TFPColor);
begin
  if FPColor=AValue then exit;
  inherited SetFPColor(AValue);
  FAggColor:=FPToAggColor(AValue);
end;

procedure TAggFPBrush.SetAggColor(const AValue: TAggColor);
begin
  FPColor:=AggToFPColor(AValue);
end;

procedure TAggFPBrush.SetStyle(AValue: TFPBrushStyle);
begin
  if Style=AValue then exit;
  inherited SetStyle(AValue);
  // not supported by aggpas
end;

{ TAggFPFont }

procedure TAggFPFont.SetAggAlignX(const AValue: TAggTextAlignment);
begin
  if FAggAlignX=AValue then exit;
  FAggAlignX:=AValue;
end;

procedure TAggFPFont.SetAggAlignY(const AValue: TAggTextAlignment);
begin
  if FAggAlignY=AValue then exit;
  FAggAlignY:=AValue;
end;

procedure TAggFPFont.SetAggAngle(const AValue: double);
begin
  if FAggAngle=AValue then exit;
  FAggAngle:=AValue;
end;

procedure TAggFPFont.SetAggFlipY(const AValue: boolean);
begin
  if FAggFlipY=AValue then exit;
  FAggFlipY:=AValue;
  {$IFNDEF AGG2D_NO_FONT}
  TAggFPCanvas(Canvas).m_fontEngine.flip_y_(not FAggFlipY);
  {$ENDIF}
end;

procedure TAggFPFont.SetAggHeight(const AValue: double);
{$IFDEF AGG2D_USE_FREETYPE}
var
  c: TAggFPCanvas;
{$ENDIF}
begin
  if FAggHeight=AValue then exit;
  FAggHeight:=AValue;
  inherited SetSize(round(AggHeightToSize(FAggHeight)));
  {$IFDEF AGG2D_USE_FREETYPE}
  c:=TAggFPCanvas(Canvas);
  if FAggCache = AGG_VectorFontCache then
    c.m_fontEngine.height_(FAggHeight )
  else
    c.m_fontEngine.height_(c.AggWorldToScreen(FAggHeight ) );
  {$ELSE}
  // ToDo
  {$ENDIF}
end;

procedure TAggFPFont.SetAggHinting(const AValue: boolean);
begin
  if FAggHinting=AValue then exit;
  FAggHinting:=AValue;
  {$IFDEF AGG2D_USE_FREETYPE}
  TAggFPCanvas(Canvas).m_fontEngine.hinting_(FAggHinting );
  {$ENDIF}
end;

procedure TAggFPFont.SetSize(AValue: integer);
begin
  AggHeight:=SizeToAggHeight(AValue);
end;

function TAggFPFont.AggHeightToSize(const h: double): double;
begin
  Result:=h;
end;

function TAggFPFont.SizeToAggHeight(const s: double): double;
begin
  Result:=s;
end;

procedure TAggFPFont.SetFPColor(const AValue: TFPColor);
begin
  if FPColor=AValue then exit;
  inherited SetFPColor(AValue);
  FAggColor:=FPToAggColor(AValue);
end;

procedure TAggFPFont.SetAggColor(const AValue: TAggColor);
begin
  FPColor:=AggToFPColor(AValue);
end;

procedure TAggFPFont.DoCopyProps(From: TFPCanvasHelper);
var
  Src: TAggFPFont;
begin
  inherited DoCopyProps(From);
  if From is TAggFPFont then begin
    Src:=TAggFPFont(From);
    FAggColor:=Src.FAggColor;
    AggAlignX:=Src.AggAlignX;
    AggAlignY:=Src.AggAlignY;
    AggAngle:=Src.AggAngle;
    AggHinting:=Src.AggHinting;
    FAggHeight:=Src.AggHeight;
  end;
end;

constructor TAggFPFont.Create;
begin
  inherited Create;
  FAggAlignX:=AGG_AlignLeft;
  FAggAlignY:=AGG_AlignBottom;
  FAggHinting:=true;
  FAggUseOnlyFont:=true;
end;

procedure TAggFPFont.LoadFromFile(aFilename: String; const NewHeight: double;
  const NewBold: boolean; const NewItalic: boolean;
  const NewCache: TAggFontCacheType; const NewAngle: double;
  const NewHinting: boolean);
{$IFDEF AGG2D_USE_WINFONTS }
var
  b : int;
{$ENDIF}
var
  c: TAggFPCanvas;
begin
  FAggAngle:=NewAngle;
  FAggHeight:=NewHeight;
  inherited SetSize(round(FAggHeight));
  FAggCache:=NewCache;
  FAggHinting:=NewHinting;
  inherited SetFlags(5,NewBold); // Bold
  inherited SetFlags(6,NewItalic); // Italic
  inherited SetFlags(7,false); // Underline
  inherited SetFlags(8,false); // StrikeTrough

  c:=TAggFPCanvas(Canvas);

  {$IFDEF AGG2D_USE_FREETYPE }
  if FAggCache = AGG_VectorFontCache then
    c.m_fontEngine.load_font(PChar(@AFileName[1 ] ) ,0 ,glyph_ren_outline )
  else
    c.m_fontEngine.load_font(PChar(@AFileName[1 ] ) ,0 ,glyph_ren_agg_gray8 );

  c.m_fontEngine.hinting_(FAggHinting );

  if FAggCache = AGG_VectorFontCache then
    c.m_fontEngine.height_(FAggHeight )
  else
    c.m_fontEngine.height_(c.AggWorldToScreen(FAggHeight ) );

  {$ENDIF }
  {$IFDEF AGG2D_USE_WINFONTS}

  c.m_fontEngine.hinting_(FAggHinting );

  if Bold then
    b:=700
  else
    b:=400;

  if FAggCache = AGG_VectorFontCache then
    c.m_fontEngine.create_font_(PChar(@AFileName[1 ] ) ,glyph_ren_outline ,
      FAggHeight ,0.0 ,b ,Italic )
  else
    c.m_fontEngine.create_font_(PChar(@AFileName[1 ] ) ,glyph_ren_agg_gray8 ,
      c.AggWorldToScreen(FAggHeight) ,0.0 ,b ,Italic );
  {$ENDIF }

  {$IFNDEF AGG2D_NO_FONT}
  TAggFPCanvas(Canvas).m_fontEngine.flip_y_(not FAggFlipY);
  {$ENDIF}
end;

{ TAggFP_renderer_scanline_aa }

procedure TAggFP_renderer_scanline_aa.add_path(vs: vertex_source_ptr;
  path_id: unsigned);
begin
  raise Exception.Create('not usable');
end;

procedure TAggFP_renderer_scanline_aa.add_vertex(x, y: double; cmd: unsigned);
begin
  raise Exception.Create('not usable');
end;

procedure TAggFP_renderer_scanline_aa.clip_box(x1, y1, x2, y2: double);
begin
  raise Exception.Create('not usable');
end;

procedure TAggFP_renderer_scanline_aa.color_(c: aggclr_ptr);
begin
  raise Exception.Create('not usable');
end;

procedure TAggFP_renderer_scanline_aa.filling_rule(filling_rule_: filling_rule_e
  );
begin
  raise Exception.Create('not usable');
end;

procedure TAggFP_renderer_scanline_aa.gamma(gamma_function: vertex_source_ptr);
begin
  raise Exception.Create('not usable');
end;

procedure TAggFP_renderer_scanline_aa.reset;
begin
  raise Exception.Create('not usable');
end;

function TAggFP_renderer_scanline_aa._max_y: int;
begin
  raise Exception.Create('not usable');
  Result:=0;
end;

function TAggFP_renderer_scanline_aa._max_x: int;
begin
  raise Exception.Create('not usable');
  Result:=0;
end;

function TAggFP_renderer_scanline_aa._min_x: int;
begin
  raise Exception.Create('not usable');
  Result:=0;
end;

function TAggFP_renderer_scanline_aa._min_y: int;
begin
  raise Exception.Create('not usable');
  Result:=0;
end;

function TAggFP_renderer_scanline_aa.hit_test(tx, ty: int): boolean;
begin
  raise Exception.Create('not usable');
  Result:=false;
end;

function TAggFP_renderer_scanline_aa.sweep_scanline(sl: scanline_ptr): boolean;
begin
  raise Exception.Create('not usable');
  Result:=false;
end;

function TAggFP_renderer_scanline_aa.sweep_scanline_em(sl: scanline_ptr
  ): boolean;
begin
  raise Exception.Create('not usable');
  Result:=false;
end;

function TAggFP_renderer_scanline_aa.rewind_scanlines: boolean;
begin
  raise Exception.Create('not usable');
  Result:=false;
end;

procedure TAggFP_renderer_scanline_aa.sort;
begin
  raise Exception.Create('not usable');
end;

{ TAggFPPath }

procedure TAggFPPath.SetAggColor(const AValue: TAggColor);
begin
  FPColor:=AggToFPColor(AValue);
end;

procedure TAggFPPath.DoCopyProps(From: TFPCanvasHelper);
var
  Src: TAggFPPath;
begin
  inherited DoCopyProps(From);
  if From is TAggFPPath then begin
    Src:=TAggFPPath(From);
    FAggColor:=Src.FAggColor;
  end;
end;

constructor TAggFPPath.Create;
begin
  inherited Create;
  m_path.Construct;
  m_convCurve.Construct (@m_path );
end;

destructor TAggFPPath.Destroy;
begin
  m_convCurve.Destruct;
  m_path.Destruct;
  inherited Destroy;
end;

end.

