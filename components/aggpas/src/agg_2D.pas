//----------------------------------------------------------------------------
// Agg2D - Version 1.0
// Based on Anti-Grain Geometry
// Copyright (C) 2005 Maxim Shemanarev (http://www.antigrain.com)
//
// Agg2D - Version 1.0 Release Milano 3 (AggPas 2.3 RM3)
// Pascal Port By: Milan Marusinec alias Milano
//                 milan@marusinec.sk
//                 http://www.aggpas.org
// Copyright (c) 2007
//
// Permission to copy, use, modify, sell and distribute this software
// is granted provided this copyright notice appears in all copies.
// This software is provided "as is" without express or implied
// warranty, and with no claim as to its suitability for any purpose.
//
//----------------------------------------------------------------------------
// Contact: mcseem@antigrain.com
//          mcseemagg@yahoo.com
//          http://www.antigrain.com
//
// [Pascal Port History] -----------------------------------------------------
//
// 22.08.2007-Milano: Unit port establishment
// 23.08.2007-Milano: Porting
// 11.09.2007-Milano: -"-
// 13.09.2007-Milano: -"-, Finished OK
//
{ agg_2D.pas }
unit
 agg_2D ;

INTERFACE

{$I agg_mode.inc }

// With this define uncommented you can use FreeType font engine
{ $DEFINE AGG2D_USE_FREETYPE }

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
 agg_arc ,
 agg_bezier_arc ,
 agg_rounded_rect ,
 agg_font_engine ,
 agg_font_cache_manager ,
 agg_pixfmt ,
 agg_pixfmt_rgba ,
 agg_color ,
 agg_math_stroke ,
 agg_image_filters ,
 agg_vertex_source ,
 agg_render_scanlines ,

{$IFDEF AGG2D_USE_FREETYPE }
 agg_font_freetype ,

{$ENDIF }
{$IFDEF AGG2D_USE_WINFONTS}
 agg_font_win32_tt ,
 Windows ,

{$ENDIF }

 Math ;

{ GLOBAL VARIABLES & CONSTANTS }
const
// LineJoin
 JoinMiter = miter_join;
 JoinRound = round_join;
 JoinBevel = bevel_join;

// LineCap
 CapButt   = butt_cap;
 CapSquare = square_cap;
 CapRound  = round_cap;

// TextAlignment
 AlignLeft   = 0;
 AlignRight  = 1;
 AlignCenter = 2;
 AlignTop    = AlignRight;
 AlignBottom = AlignLeft;

// BlendMode
 BlendAlpha      = end_of_comp_op_e;
 BlendClear      = comp_op_clear;
 BlendSrc        = comp_op_src;
 BlendDst        = comp_op_dst;
 BlendSrcOver    = comp_op_src_over;
 BlendDstOver    = comp_op_dst_over;
 BlendSrcIn      = comp_op_src_in;
 BlendDstIn      = comp_op_dst_in;
 BlendSrcOut     = comp_op_src_out;
 BlendDstOut     = comp_op_dst_out;
 BlendSrcAtop    = comp_op_src_atop;
 BlendDstAtop    = comp_op_dst_atop;
 BlendXor        = comp_op_xor;
 BlendAdd        = comp_op_plus;
 BlendSub        = comp_op_minus;
 BlendMultiply   = comp_op_multiply;
 BlendScreen     = comp_op_screen;
 BlendOverlay    = comp_op_overlay;
 BlendDarken     = comp_op_darken;
 BlendLighten    = comp_op_lighten;
 BlendColorDodge = comp_op_color_dodge;
 BlendColorBurn  = comp_op_color_burn;
 BlendHardLight  = comp_op_hard_light;
 BlendSoftLight  = comp_op_soft_light;
 BlendDifference = comp_op_difference;
 BlendExclusion  = comp_op_exclusion;
 BlendContrast   = comp_op_contrast;

{ TYPES DEFINITION }
type
 Color_ptr = ^Color;
 Color     = rgba8;

 Rect_ = agg_basics.rect;
 RectD = agg_basics.rect_d;

 Affine     = trans_affine;
 Affine_ptr = trans_affine_ptr;

 FontRasterizer     = gray8_adaptor_type;
 FontRasterizer_ptr = gray8_adaptor_type_ptr;

 FontScanline     = gray8_scanline_type;
 FontScanline_ptr = gray8_scanline_type_ptr;

{$IFDEF AGG2D_USE_FREETYPE }
 FontEngine = font_engine_freetype_int32;

{$ENDIF }
{$IFDEF AGG2D_USE_WINFONTS}
 FontEngine = font_engine_win32_tt_int32;

{$ENDIF }

 Gradient  = (Solid ,Linear ,Radial );
 Direction = (CW, CCW );

 LineJoin_  = int;
 LineCap_   = int;
 BlendMode_ = comp_op_e;

 TextAlignment = int;

 DrawPathFlag = (
  FillOnly ,
  StrokeOnly ,
  FillAndStroke ,
  FillWithLineColor );

 ViewportOption = (
  Anisotropic ,
  XMinYMin ,
  XMidYMin ,
  XMaxYMin ,
  XMinYMid ,
  XMidYMid ,
  XMaxYMid ,
  XMinYMax ,
  XMidYMax ,
  XMaxYMax );

 ImageFilter_ = (
  NoFilter ,
  Bilinear ,
  Hanning ,
  Hermite ,
  Quadric ,
  Bicubic ,
  Catrom ,
  Spline16 ,
  Spline36 ,
  Blackman144 );

 ImageResample_ = (
  NoResample ,
  ResampleAlways ,
  ResampleOnZoomOut );

 FontCacheType = (
  RasterFontCache ,
  VectorFontCache );

 Transformations_ptr = ^Transformations_;
 Transformations_ = record
   affineMatrix : array[0..5 ] of double;

  end;

 Image_ptr = ^Image;
 Image = object
   renBuf : rendering_buffer;

   constructor Construct; overload;
   constructor Construct(buf : int8u_ptr; width_ ,height_ : unsigned; stride : int ); overload;
   destructor  Destruct;

   procedure attach(buf : int8u_ptr; width_ ,height_ : unsigned; stride : int );

   function  width : int;
   function  height : int;

   procedure premultiply;
   procedure demultiply;

  end;

 Agg2DRasterizerGamma = object(vertex_source )
   m_alpha : gamma_multiply;
   m_gamma : gamma_power;

   constructor Construct(alpha ,gamma : double );

   function func_operator_gamma(x : double ) : double; virtual;

  end;

 Agg2D_ptr = ^Agg2D;
 Agg2D = object
  private
   m_rbuf : rendering_buffer;

   m_pixFormat ,m_pixFormatComp ,m_pixFormatPre ,m_pixFormatCompPre : pixel_formats;
   m_renBase   ,m_renBaseComp   ,m_renBasePre   ,m_renBaseCompPre   : renderer_base;

   m_renSolid ,m_renSolidComp : renderer_scanline_aa_solid;

   m_allocator : span_allocator;
   m_clipBox   : RectD;

   m_blendMode ,m_imageBlendMode : BlendMode_;

   m_imageBlendColor : Color;

   m_scanline   : scanline_u8;
   m_rasterizer : rasterizer_scanline_aa;

   m_masterAlpha ,m_antiAliasGamma : double;

   m_fillColor ,m_lineColor : Color;

   m_fillGradient ,m_lineGradient : pod_auto_array;

   m_lineCap  : LineCap_;
   m_lineJoin : LineJoin_;

   m_fillGradientFlag ,m_lineGradientFlag : Gradient;

   m_fillGradientMatrix ,m_lineGradientMatrix : trans_affine;

   m_fillGradientD1 ,
   m_lineGradientD1 ,
   m_fillGradientD2 ,
   m_lineGradientD2 ,
   m_textAngle      : double;
   m_textAlignX     ,
   m_textAlignY     : TextAlignment;
   m_textHints      : boolean;
   m_fontHeight     ,
   m_fontAscent     ,
   m_fontDescent    : double;
   m_fontCacheType  : FontCacheType;

   m_imageFilter    : ImageFilter_;
   m_imageResample  : ImageResample_;
   m_imageFilterLut : image_filter_lut;

   m_fillGradientInterpolator ,
   m_lineGradientInterpolator : span_interpolator_linear;

   m_linearGradientFunction : gradient_x;
   m_radialGradientFunction : gradient_circle;

   m_lineWidth   : double;
   m_evenOddFlag : boolean;

   m_path      : path_storage;
   m_transform : trans_affine;

   m_convCurve  : conv_curve;
   m_convStroke : conv_stroke;

   m_pathTransform ,m_strokeTransform : conv_transform;

  {$IFNDEF AGG2D_NO_FONT}
   m_fontEngine       : FontEngine;
   m_fontCacheManager : font_cache_manager;
  {$ENDIF}
  {$IFDEF AGG2D_USE_WINFONTS }
   m_fontDC : HDC;
  {$ENDIF }

  // Other Pascal-specific members
   m_gammaNone  : gamma_none;
   m_gammaAgg2D : Agg2DRasterizerGamma;

   m_ifBilinear    : image_filter_bilinear;
   m_ifHanning     : image_filter_hanning;
   m_ifHermite     : image_filter_hermite;
   m_ifQuadric     : image_filter_quadric;
   m_ifBicubic     : image_filter_bicubic;
   m_ifCatrom      : image_filter_catrom;
   m_ifSpline16    : image_filter_spline16;
   m_ifSpline36    : image_filter_spline36;
   m_ifBlackman144 : image_filter_blackman144;

  public
   constructor Construct;
   destructor  Destruct;

  // Setup
   procedure attach(buf : int8u_ptr; width_ ,height_ : unsigned; stride : int ); overload;
   procedure attach(img : Image_ptr ); overload;

   procedure clipBox(x1 ,y1 ,x2 ,y2 : double ); overload;
   function  clipBox : RectD; overload;

   procedure clearAll(c : Color ); overload;
   procedure clearAll(r ,g ,b : unsigned; a : unsigned = 255 ); overload;

   procedure clearClipBox(c : Color ); overload;
   procedure clearClipBox(r ,g ,b : unsigned; a : unsigned = 255 ); overload;

  // Conversions
   procedure worldToScreen(x ,y : double_ptr ); overload;
   procedure screenToWorld(x ,y : double_ptr ); overload;
   function  worldToScreen(scalar : double ) : double; overload;
   function  screenToWorld(scalar : double ) : double; overload;

   procedure alignPoint(x ,y : double_ptr );

   function  inBox(worldX ,worldY : double ) : boolean;

  // General Attributes
   procedure blendMode(m : BlendMode_ ); overload;
   function  blendMode : BlendMode_; overload;

   procedure imageBlendMode(m : BlendMode_ ); overload;
   function  imageBlendMode : BlendMode_; overload;

   procedure imageBlendColor(c : Color ); overload;
   procedure imageBlendColor(r ,g ,b : unsigned; a : unsigned = 255 ); overload;
   function  imageBlendColor : Color; overload;

   procedure masterAlpha(a : double ); overload;
   function  masterAlpha : double; overload;

   procedure antiAliasGamma(g : double ); overload;
   function  antiAliasGamma : double; overload;

   procedure fillColor(c : Color ); overload;
   procedure fillColor(r ,g ,b : unsigned; a : unsigned = 255 ); overload;
   procedure noFill;

   procedure lineColor(c : Color ); overload;
   procedure lineColor(r ,g ,b : unsigned; a : unsigned = 255 ); overload;
   procedure noLine;

   function  fillColor : Color; overload;
   function  lineColor : Color; overload;

   procedure fillLinearGradient(x1 ,y1 ,x2 ,y2 : double; c1 ,c2 : Color; profile : double = 1.0 );
   procedure lineLinearGradient(x1 ,y1 ,x2 ,y2 : double; c1 ,c2 : Color; profile : double = 1.0 );

   procedure fillRadialGradient(x ,y ,r : double; c1 ,c2 : Color; profile : double = 1.0 ); overload;
   procedure lineRadialGradient(x ,y ,r : double; c1 ,c2 : Color; profile : double = 1.0 ); overload;

   procedure fillRadialGradient(x ,y ,r : double; c1 ,c2 ,c3 : Color ); overload;
   procedure lineRadialGradient(x ,y ,r : double; c1 ,c2 ,c3 : Color ); overload;

   procedure fillRadialGradient(x ,y ,r : double ); overload;
   procedure lineRadialGradient(x ,y ,r : double ); overload;

   procedure lineWidth (w : double );
   function  lineWidth_(w : double ) : double;

   procedure lineCap(cap : LineCap_ ); overload;
   function  lineCap : LineCap_; overload;

   procedure lineJoin(join : LineJoin_ ); overload;
   function  lineJoin : LineJoin_; overload;

   procedure fillEvenOdd(evenOddFlag : boolean ); overload;
   function  fillEvenOdd : boolean; overload;

  // Transformations
   function  transformations : Transformations_; overload;
   procedure transformations(tr : Transformations_ptr ); overload;
   procedure resetTransformations;

   procedure affine(tr : Affine_ptr ); overload;
   procedure affine(tr : Transformations_ptr ); overload;

   procedure rotate   (angle : double );
   procedure scale    (sx ,sy : double );
   procedure skew     (sx ,sy : double );
   procedure translate(x ,y : double );

   procedure parallelogram(x1 ,y1 ,x2 ,y2 : double; para : double_ptr );

   procedure viewport(
              worldX1  ,worldY1  ,worldX2  ,worldY2 ,
              screenX1 ,screenY1 ,screenX2 ,screenY2 : double;
              opt : ViewportOption = XMidYMid );

  // Basic Shapes
   procedure line     (x1 ,y1 ,x2 ,y2 : double );
   procedure triangle (x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double );
   procedure rectangle(x1 ,y1 ,x2 ,y2 : double );

   procedure roundedRect(x1 ,y1 ,x2 ,y2 ,r : double ); overload;
   procedure roundedRect(x1 ,y1 ,x2 ,y2 ,rx ,ry : double ); overload;
   procedure roundedRect(
              x1 ,y1 ,x2 ,y2 ,
              rxBottom ,ryBottom ,
              rxTop ,ryTop : double ); overload;

   procedure ellipse(cx ,cy ,rx ,ry : double );
             
   procedure arc (cx ,cy ,rx ,ry ,start ,sweep : double );
   procedure star(cx ,cy ,r1 ,r2 ,startAngle : double; numRays : int );

   procedure curve(x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double ); overload;
   procedure curve(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 : double ); overload;

   procedure polygon (xy : double_ptr; numPoints : int );
   procedure polyline(xy : double_ptr; numPoints : int );

  // Text
   procedure flipText(flip : boolean );

   procedure font(
              fileName : char_ptr; height : double;
              bold : boolean = false;
              italic : boolean = false;
              ch : FontCacheType = RasterFontCache;
              angle : double = 0.0 );

   function  fontHeight : double;

   procedure textAlignment(alignX ,alignY : TextAlignment );

   function  textHints : boolean; overload;
   procedure textHints(hints : boolean ); overload;
   function  textWidth(str : char_ptr ) : double;

   procedure text(
              x ,y : double; str : char_ptr;
              roundOff : boolean = false;
              ddx : double = 0.0;
              ddy : double = 0.0 );

  // Path commands
   procedure resetPath;

   procedure moveTo (x ,y : double );
   procedure moveRel(dx ,dy : double );

   procedure lineTo (x ,y : double );
   procedure lineRel(dx ,dy : double );

   procedure horLineTo (x : double );
   procedure horLineRel(dx : double );

   procedure verLineTo (y : double );
   procedure verLineRel(dy : double );

   procedure arcTo(
              rx ,ry ,angle : double;
              largeArcFlag ,sweepFlag : boolean;
              x ,y : double );

   procedure arcRel(
              rx ,ry ,angle : double;
              largeArcFlag ,sweepFlag : boolean;
              dx ,dy : double );

   procedure quadricCurveTo (xCtrl ,yCtrl ,xTo ,yTo : double ); overload;
   procedure quadricCurveRel(dxCtrl ,dyCtrl ,dxTo ,dyTo : double ); overload;
   procedure quadricCurveTo (xTo ,yTo : double ); overload;
   procedure quadricCurveRel(dxTo ,dyTo : double ); overload;

   procedure cubicCurveTo (xCtrl1 ,yCtrl1 ,xCtrl2 ,yCtrl2 ,xTo ,yTo : double ); overload;
   procedure cubicCurveRel(dxCtrl1 ,dyCtrl1 ,dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo : double ); overload;
   procedure cubicCurveTo (xCtrl2 ,yCtrl2 ,xTo ,yTo : double ); overload;
   procedure cubicCurveRel(xCtrl2 ,yCtrl2 ,xTo ,yTo : double ); overload;

   procedure addEllipse(cx ,cy ,rx ,ry : double; dir : Direction );
   procedure closePolygon;

   procedure drawPath(flag : DrawPathFlag = FillAndStroke );

   procedure drawPathNoTransform(flag : DrawPathFlag = FillAndStroke );

  // Image Transformations
   procedure imageFilter(f : ImageFilter_ ); overload;
   function  imageFilter : ImageFilter_; overload;

   procedure imageResample(f : ImageResample_ ); overload;
   function  imageResample : ImageResample_; overload;

   procedure transformImage(
              img : Image_ptr;
              imgX1 ,imgY1 ,imgX2 ,imgY2 : int;
              dstX1 ,dstY1 ,dstX2 ,dstY2 : double ); overload;

   procedure transformImage(
              img : Image_ptr;
              dstX1 ,dstY1 ,dstX2 ,dstY2 : double ); overload;

   procedure transformImage(
              img : Image_ptr;
              imgX1 ,imgY1 ,imgX2 ,imgY2 : int;
              parallelogram_ : double_ptr ); overload;

   procedure transformImage(img : Image_ptr; parallelogram_ : double_ptr ); overload;

   procedure transformImagePath(
              img : Image_ptr;
              imgX1 ,imgY1 ,imgX2 ,imgY2 : int;
              dstX1 ,dstY1 ,dstX2 ,dstY2 : double ); overload;

   procedure transformImagePath(
              img : Image_ptr;
              dstX1 ,dstY1 ,dstX2 ,dstY2 : double ); overload;

   procedure transformImagePath(
              img : Image_ptr;
              imgX1 ,imgY1 ,imgX2 ,imgY2 : int;
              parallelogram_ : double_ptr ); overload;

   procedure transformImagePath(img : Image_ptr; parallelogram_ : double_ptr ); overload;

  // Image Blending (no transformations available)
   procedure blendImage(
              img : Image_ptr;
              imgX1 ,imgY1 ,imgX2 ,imgY2 : int;
              dstX ,dstY : double; alpha : unsigned = 255 ); overload;

   procedure blendImage(img : Image_ptr; dstX ,dstY : double; alpha : unsigned = 255 ); overload;

  // Copy image directly, together with alpha-channel
   procedure copyImage(
              img : Image_ptr;
              imgX1 ,imgY1 ,imgX2 ,imgY2 : int;
              dstX ,dstY : double ); overload;

   procedure copyImage(img : Image_ptr; dstX ,dstY : double ); overload;

  private
   procedure render(fillColor_ : boolean ); overload;
   procedure render(ras : FontRasterizer_ptr; sl : FontScanline_ptr ); overload;

   procedure addLine(x1 ,y1 ,x2 ,y2 : double );
   procedure updateRasterizerGamma;
   procedure renderImage(
              img : Image_ptr;
              x1 ,y1 ,x2 ,y2 : int;
              parl : double_ptr );

  end;

 SpanConvImageBlend_ptr = ^SpanConvImageBlend;
 SpanConvImageBlend = object(span_convertor )
  private
   m_mode  : BlendMode_;
   m_color : Color;
   m_pixel : pixel_formats_ptr; // m_pixFormatCompPre

  public
   constructor Construct(m : BlendMode_; c : Color; p : pixel_formats_ptr );

   procedure convert(span : aggclr_ptr; x ,y : int; len : unsigned ); virtual;

  end;

{ GLOBAL PROCEDURES }
// Auxiliary
 function  pi : double;
 function  deg2Rad(v : double ) : double;
 function  rad2Deg(v : double ) : double;

 function  operator_is_equal    (c1 ,c2 : Color_ptr ) : boolean;
 function  operator_is_not_equal(c1 ,c2 : Color_ptr ) : boolean;

 procedure Agg2DRenderer_render(
            gr : Agg2D_ptr;
            renBase : renderer_base_ptr;
            renSolid : renderer_scanline_aa_solid_ptr;
            fillColor_ : boolean ); overload;

 procedure Agg2DRenderer_render(
            gr : Agg2D_ptr;
            renBase : renderer_base_ptr;
            renSolid : renderer_scanline_aa_solid_ptr;
            ras : gray8_adaptor_type_ptr;
            sl : gray8_scanline_type_ptr ); overload;

 procedure Agg2DRenderer_renderImage(
            gr : Agg2D_ptr;
            img : Image_ptr;
            renBase : renderer_base_ptr;
            interpolator : span_interpolator_linear_ptr );

 function  Agg2DUsesFreeType : boolean;
 function  Agg2DUsesWin32TrueType : boolean;

IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
var
 g_approxScale : double = 2.0;

{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor Image.Construct;
begin
end;

{ CONSTRUCT }
constructor Image.Construct(buf : int8u_ptr; width_ ,height_ : unsigned; stride : int );
begin
 renBuf.Construct(buf ,width_ ,height_ ,stride );

end;

{ DESTRUCT }
destructor Image.Destruct;
begin
 renBuf.Destruct;

end;

{ ATTACH }
procedure Image.attach(buf : int8u_ptr; width_ ,height_ : unsigned; stride : int );
begin
 renBuf.attach(buf ,width_ ,height_ ,stride );

end;

{ WIDTH }
function Image.width : int;
begin
 result:=renBuf._width;

end;

{ HEIGHT }
function Image.height : int;
begin
 result:=renBuf._height;

end;

{ PREMULTIPLY }
procedure Image.premultiply;
var
 pixf : pixel_formats;

begin
{ pixfmt_rgba32(pixf ,@renBuf );

 pixf.premultiply; {!}

end;

{ DEMULTIPLY }
procedure Image.demultiply;
var
 pixf : pixel_formats;

begin
{ pixfmt_rgba32(pixf ,@renBuf );

 pixf.demultiply; {!}

end;

{ CONSTRUCT }
constructor Agg2DRasterizerGamma.Construct(alpha ,gamma : double );
begin
 m_alpha.Construct(alpha );
 m_gamma.Construct(gamma );

end;

{ FUNC_OPERATOR_GAMMA }
function Agg2DRasterizerGamma.func_operator_gamma(x : double ) : double;
begin
 result:=m_alpha.func_operator_gamma(m_gamma.func_operator_gamma(x ) );

end;

{ CONSTRUCT }
constructor Agg2D.Construct;
begin
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

 m_blendMode     :=BlendAlpha;
 m_imageBlendMode:=BlendDst;

 m_imageBlendColor.Construct(0 ,0 ,0 );

 m_scanline.Construct;
 m_rasterizer.Construct;

 m_masterAlpha   :=1.0;
 m_antiAliasGamma:=1.0;

 m_fillColor.Construct(255 ,255 ,255 );
 m_lineColor.Construct(0   ,0   ,0 );

 m_fillGradient.Construct(256 ,sizeof(aggclr ) );
 m_lineGradient.Construct(256 ,sizeof(aggclr ) );

 m_lineCap :=CapRound;
 m_lineJoin:=JoinRound;

 m_fillGradientFlag:=Solid;
 m_lineGradientFlag:=Solid;

 m_fillGradientMatrix.Construct;
 m_lineGradientMatrix.Construct;

 m_fillGradientD1:=0.0;
 m_lineGradientD1:=0.0;
 m_fillGradientD2:=100.0;
 m_lineGradientD2:=100.0;

 m_textAngle  :=0.0;
 m_textAlignX :=AlignLeft;
 m_textAlignY :=AlignBottom;
 m_textHints  :=true;
 m_fontHeight :=0.0;
 m_fontAscent :=0.0;
 m_fontDescent:=0.0;

 m_fontCacheType:=RasterFontCache;
 m_imageFilter  :=Bilinear;
 m_imageResample:=NoResample;

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

 m_lineWidth  :=1;
 m_evenOddFlag:=false;

 m_path.Construct;
 m_transform.Construct;

 m_convCurve.Construct (@m_path );
 m_convStroke.Construct(@m_convCurve );

 m_pathTransform.Construct  (@m_convCurve ,@m_transform );
 m_strokeTransform.Construct(@m_convStroke ,@m_transform );

{$IFDEF AGG2D_USE_FREETYPE }
 m_fontEngine.Construct;

{$ENDIF }
{$IFDEF AGG2D_USE_WINFONTS}

 m_fontDC:=GetDC(0 );

 m_fontEngine.Construct(m_fontDC );

{$ENDIF }
{$IFNDEF AGG2D_NO_FONT}
 m_fontCacheManager.Construct(@m_fontEngine );
{$ENDIF}


 lineCap (m_lineCap );
 lineJoin(m_lineJoin );

end;

{ DESTRUCT }
destructor Agg2D.Destruct;
begin
 m_rbuf.Destruct;

 m_allocator.Destruct;

 m_scanline.Destruct;
 m_rasterizer.Destruct;

 m_fillGradient.Destruct;
 m_lineGradient.Destruct;

 m_imageFilterLut.Destruct;
 m_path.Destruct;

 m_convCurve.Destruct;
 m_convStroke.Destruct;

{$IFNDEF AGG2D_NO_FONT}
 m_fontEngine.Destruct;
 m_fontCacheManager.Destruct;
{$ENDIF}
{$IFDEF AGG2D_USE_WINFONTS }
 ReleaseDC(0 ,m_fontDC );

{$ENDIF }

end;

{ ATTACH }
procedure Agg2D.attach(buf : int8u_ptr; width_ ,height_ : unsigned; stride : int );
begin
 m_rbuf.attach(buf ,width_ ,height_ ,stride );

 m_renBase.reset_clipping       (true );
 m_renBaseComp.reset_clipping   (true );
 m_renBasePre.reset_clipping    (true );
 m_renBaseCompPre.reset_clipping(true );

 resetTransformations;

 lineWidth(1.0 );
 lineColor(0   ,0   ,0 );
 fillColor(255 ,255 ,255 );

 textAlignment(AlignLeft ,AlignBottom );

 clipBox (0 ,0 ,width_ ,height_ );
 lineCap (CapRound );
 lineJoin(JoinRound );
 flipText(false );

 imageFilter  (Bilinear );
 imageResample(NoResample );

 m_masterAlpha   :=1.0;
 m_antiAliasGamma:=1.0;

 m_rasterizer.gamma(@m_gammaNone );

 m_blendMode:=BlendAlpha;

end;

{ ATTACH }
procedure Agg2D.attach(img : Image_ptr );
begin
 attach(img.renBuf._buf ,img.renBuf._width ,img.renBuf._height ,img.renBuf._stride );

end;

{ CLIPBOX }
procedure Agg2D.clipBox(x1 ,y1 ,x2 ,y2 : double );
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

{ CLIPBOX }
function Agg2D.clipBox : RectD;
begin
 result:=m_clipBox;

end;

{ CLEARALL }
procedure Agg2D.clearAll(c : Color );
var
 clr : aggclr;

begin
 clr.Construct  (c );
 m_renBase.clear(@clr );

end;

{ CLEARALL }
procedure Agg2D.clearAll(r ,g ,b : unsigned; a : unsigned = 255 );
var
 clr : Color;

begin
 clr.Construct(r ,g ,b ,a );
 clearAll     (clr );

end;

{ CLEARCLIPBOX }
procedure Agg2D.clearClipBox(c : Color );
var
 clr : aggclr;

begin
 clr.Construct(c );

 m_renBase.copy_bar(0 ,0 ,m_renBase.width ,m_renBase.height ,@clr );

end;

{ CLEARCLIPBOX }
procedure Agg2D.clearClipBox(r ,g ,b : unsigned; a : unsigned = 255 );
var
 clr : Color;

begin
 clr.Construct(r ,g ,b ,a );
 clearClipBox (clr );

end;

{ WORLDTOSCREEN }
procedure Agg2D.worldToScreen(x ,y : double_ptr );
begin
 m_transform.transform(@m_transform ,x ,y );

end;

{ SCREENTOWORLD }
procedure Agg2D.screenToWorld(x ,y : double_ptr );
begin
 m_transform.inverse_transform(@m_transform ,x ,y );

end;

{ WORLDTOSCREEN }
function Agg2D.worldToScreen(scalar : double ) : double;
var
 x1 ,y1 ,x2 ,y2 : double;

begin
 x1:=0;
 y1:=0;
 x2:=scalar;
 y2:=scalar;

 worldToScreen(@x1 ,@y1 );
 worldToScreen(@x2 ,@y2 );

 result:=Sqrt((x2 - x1 ) * (x2 - x1 ) + (y2 - y1 ) * (y2 - y1 ) ) * 0.7071068;

end;

{ SCREENTOWORLD }
function Agg2D.screenToWorld(scalar : double ) : double;
var
 x1 ,y1 ,x2 ,y2 : double;

begin
 x1:=0;
 y1:=0;
 x2:=scalar;
 y2:=scalar;

 screenToWorld(@x1 ,@y1 );
 screenToWorld(@x2 ,@y2 );

 result:=Sqrt((x2 - x1 ) * (x2 - x1 ) + (y2 - y1 ) * (y2 - y1 ) ) * 0.7071068;

end;

{ ALIGNPOINT }
procedure Agg2D.alignPoint(x ,y : double_ptr );
begin
 worldToScreen(x ,y );

 x^:=Floor(x^ ) + 0.5;
 y^:=Floor(y^ ) + 0.5;

 screenToWorld(x ,y );

end;

{ INBOX }
function Agg2D.inBox(worldX ,worldY : double ) : boolean;
begin
 worldToScreen(@worldX ,@worldY );

 result:=m_renBase.inbox(Trunc(worldX ) ,Trunc(worldY ) );

end;

{ BLENDMODE }
procedure Agg2D.blendMode(m : BlendMode_ );
begin
 m_blendMode:=m;

 m_pixFormatComp.comp_op_   (unsigned(m ) );
 m_pixFormatCompPre.comp_op_(unsigned(m ) );

end;

{ BLENDMODE }
function Agg2D.blendMode : BlendMode_;
begin
 result:=m_blendMode;

end;

{ IMAGEBLENDMODE }
procedure Agg2D.imageBlendMode(m : BlendMode_ );
begin
 m_imageBlendMode:=m;

end;

{ IMAGEBLENDMODE }
function Agg2D.imageBlendMode : BlendMode_;
begin
 result:=m_imageBlendMode;

end;

{ IMAGEBLENDCOLOR }
procedure Agg2D.imageBlendColor(c : Color );
begin
 m_imageBlendColor:=c;

end;

{ IMAGEBLENDCOLOR }
procedure Agg2D.imageBlendColor(r ,g ,b : unsigned; a : unsigned = 255 );
var
 clr : Color;

begin
 clr.Construct  (r ,g ,b ,a );
 imageBlendColor(clr );

end;

{ IMAGEBLENDCOLOR }
function Agg2D.imageBlendColor : Color;
begin
 result:=m_imageBlendColor;

end;

{ MASTERALPHA }
procedure Agg2D.masterAlpha(a : double );
begin
 m_masterAlpha:=a;

 updateRasterizerGamma;

end;

{ MASTERALPHA }
function Agg2D.masterAlpha : double;
begin
 result:=m_masterAlpha;

end;

{ ANTIALIASGAMMA }
procedure Agg2D.antiAliasGamma(g : double );
begin
 m_antiAliasGamma:=g;

 updateRasterizerGamma;

end;

{ ANTIALIASGAMMA }
function Agg2D.antiAliasGamma : double;
begin
 result:=m_antiAliasGamma;

end;

{ FILLCOLOR }
procedure Agg2D.fillColor(c : Color );
begin
 m_fillColor       :=c;
 m_fillGradientFlag:=Solid;

end;

{ FILLCOLOR }
procedure Agg2D.fillColor(r ,g ,b : unsigned; a : unsigned = 255 );
var
 clr : Color;

begin
 clr.Construct(r ,g ,b ,a );
 fillColor    (clr );

end;

{ NOFILL }
procedure Agg2D.noFill;
var
 clr : Color;

begin
 clr.Construct(0 ,0 ,0 ,0 );
 fillColor    (clr );

end;

{ LINECOLOR }
procedure Agg2D.lineColor(c : Color );
begin
 m_lineColor       :=c;
 m_lineGradientFlag:=Solid;

end;

{ LINECOLOR }
procedure Agg2D.lineColor(r ,g ,b : unsigned; a : unsigned = 255 );
var
 clr : Color;

begin
 clr.Construct(r ,g ,b ,a );
 lineColor    (clr );

end;

{ NOLINE }
procedure Agg2D.noLine;
var
 clr : Color;

begin
 clr.Construct(0 ,0 ,0 ,0 );
 lineColor    (clr );

end;

{ FILLCOLOR }
function Agg2D.fillColor : Color;
begin
 result:=m_fillColor;

end;

{ LINECOLOR }
function Agg2D.lineColor : Color;
begin
 result:=m_lineColor;

end;

{ FILLLINEARGRADIENT }
procedure Agg2D.fillLinearGradient(x1 ,y1 ,x2 ,y2 : double; c1 ,c2 : Color; profile : double = 1.0 );
var
 i ,startGradient ,endGradient : int;

 k ,angle : double;

 c : Color;

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
 m_fillGradientFlag:=Linear;

 m_fillColor.Construct(0 ,0 ,0 );  // Set some real color

end;

{ LINELINEARGRADIENT }
procedure Agg2D.lineLinearGradient(x1 ,y1 ,x2 ,y2 : double; c1 ,c2 : Color; profile : double = 1.0 );
var
 i ,startGradient ,endGradient : int;

 k ,angle : double;

 c : Color;

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
 m_lineGradientMatrix.multiply(@m_transform ); {!}
 m_lineGradientMatrix.invert;

 m_lineGradientD1  :=0.0;
 m_lineGradientD2  :=Sqrt((x2 - x1 ) * (x2 - x1 ) + (y2 - y1 ) * (y2 - y1 ) );
 m_lineGradientFlag:=Linear;

 m_lineColor.Construct(0 ,0 ,0 );  // Set some real color

end;

{ FILLRADIALGRADIENT }
procedure Agg2D.fillRadialGradient(x ,y ,r : double; c1 ,c2 : Color; profile : double = 1.0 );
var
 i ,startGradient ,endGradient : int;

 k : double;
 c : Color;

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

 m_fillGradientD2:=worldToScreen(r );

 worldToScreen(@x ,@y );

 m_fillGradientMatrix.reset;

 tat.Construct(x ,y );

 m_fillGradientMatrix.multiply(@tat );
 m_fillGradientMatrix.invert;

 m_fillGradientD1  :=0;
 m_fillGradientFlag:=Radial;

 m_fillColor.Construct(0 ,0 ,0 );  // Set some real color

end;

{ LINERADIALGRADIENT }
procedure Agg2D.lineRadialGradient(x ,y ,r : double; c1 ,c2 : Color; profile : double = 1.0 );
var
 i ,startGradient ,endGradient : int;

 k : double;
 c : Color;

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

 m_lineGradientD2:=worldToScreen(r );

 worldToScreen(@x ,@y );

 m_lineGradientMatrix.reset;

 tat.Construct(x ,y );

 m_lineGradientMatrix.multiply(@tat );
 m_lineGradientMatrix.invert;

 m_lineGradientD1  :=0;
 m_lineGradientFlag:=Radial;

 m_lineColor.Construct(0 ,0 ,0 );  // Set some real color

end;

{ FILLRADIALGRADIENT }
procedure Agg2D.fillRadialGradient(x ,y ,r : double; c1 ,c2 ,c3 : Color );
var
 i : int;
 c : Color;

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

 m_fillGradientD2:=worldToScreen(r );

 worldToScreen(@x ,@y );

 m_fillGradientMatrix.reset;

 tat.Construct(x ,y );

 m_fillGradientMatrix.multiply(@tat );
 m_fillGradientMatrix.invert;

 m_fillGradientD1  :=0;
 m_fillGradientFlag:=Radial;

 m_fillColor.Construct(0 ,0 ,0 ); // Set some real color

end;

{ LINERADIALGRADIENT }
procedure Agg2D.lineRadialGradient(x ,y ,r : double; c1 ,c2 ,c3 : Color );
var
 i : int;
 c : Color;

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

 m_lineGradientD2:=worldToScreen(r );

 worldToScreen(@x ,@y );

 m_lineGradientMatrix.reset;

 tat.Construct(x ,y );

 m_lineGradientMatrix.multiply(@tat );
 m_lineGradientMatrix.invert;

 m_lineGradientD1  :=0;
 m_lineGradientFlag:=Radial;

 m_lineColor.Construct(0 ,0 ,0 ); // Set some real color

end;

{ FILLRADIALGRADIENT }
procedure Agg2D.fillRadialGradient(x ,y ,r : double );
var
 tat : trans_affine_translation;

begin
 m_fillGradientD2:=worldToScreen(r );

 worldToScreen(@x ,@y );

 m_fillGradientMatrix.reset;

 tat.Construct(x ,y );

 m_fillGradientMatrix.multiply(@tat );
 m_fillGradientMatrix.invert;

 m_fillGradientD1:=0;

end;

{ LINERADIALGRADIENT }
procedure Agg2D.lineRadialGradient(x ,y ,r : double );
var
 tat : trans_affine_translation;

begin
 m_lineGradientD2:=worldToScreen(r );

 worldToScreen(@x ,@y );

 m_lineGradientMatrix.reset;

 tat.Construct(x ,y );

 m_lineGradientMatrix.multiply(@tat );
 m_lineGradientMatrix.invert;

 m_lineGradientD1:=0;

end;

{ LINEWIDTH }
procedure Agg2D.lineWidth(w : double );
begin
 m_lineWidth:=w;

 m_convStroke.width_(w );

end;

{ LINEWIDTH_ }
function Agg2D.lineWidth_(w : double ) : double;
begin
 result:=m_lineWidth;

end;

{ LINECAP }
procedure Agg2D.lineCap(cap : LineCap_ );
begin
 m_lineCap:=cap;

 m_convStroke.line_cap_(cap );

end;

{ LINECAP }
function Agg2D.lineCap : LineCap_;
begin
 result:=m_lineCap;

end;

{ LINEJOIN }
procedure Agg2D.lineJoin(join : LineJoin_ );
begin
 m_lineJoin:=join;

 m_convStroke.line_join_(join );

end;

{ LINEJOIN }
function Agg2D.lineJoin : LineJoin_;
begin
 result:=m_lineJoin;

end;

{ FILLEVENODD }
procedure Agg2D.fillEvenOdd(evenOddFlag : boolean );
begin
 m_evenOddFlag:=evenOddFlag;

 if evenOddFlag then
  m_rasterizer.filling_rule(fill_even_odd )
 else
  m_rasterizer.filling_rule(fill_non_zero );

end;

{ FILLEVENODD }
function Agg2D.fillEvenOdd : boolean;
begin
 result:=m_evenOddFlag;

end;

{ TRANSFORMATIONS }
function Agg2D.transformations : Transformations_;
begin
 m_transform.store_to(@result.affineMatrix[0 ] );

end;

{ TRANSFORMATIONS }
procedure Agg2D.transformations(tr : Transformations_ptr );
begin
 m_transform.load_from(@tr.affineMatrix[0 ] );

 m_convCurve.approximation_scale_ (worldToScreen(1.0 ) * g_approxScale );
 m_convStroke.approximation_scale_(worldToScreen(1.0 ) * g_approxScale );

end;

{ RESETTRANSFORMATIONS }
procedure Agg2D.resetTransformations;
begin
 m_transform.reset;

end;

{ AFFINE }
procedure Agg2D.affine(tr : Affine_ptr );
begin
 m_transform.multiply(tr );

 m_convCurve.approximation_scale_ (worldToScreen(1.0 ) * g_approxScale );
 m_convStroke.approximation_scale_(worldToScreen(1.0 ) * g_approxScale );

end;

{ AFFINE }
procedure Agg2D.affine(tr : Transformations_ptr );
var
 ta : trans_affine;

begin
 ta.Construct(
  tr.affineMatrix[0 ] ,tr.affineMatrix[1 ] ,tr.affineMatrix[2 ] ,
  tr.affineMatrix[3 ] ,tr.affineMatrix[4 ] ,tr.affineMatrix[5 ] );

 affine(Affine_ptr(@ta ) );

end;

{ ROTATE }
procedure Agg2D.rotate(angle : double );
var
 tar : trans_affine_rotation;

begin
 tar.Construct(angle );

 m_transform.multiply(@tar );

end;

{ SCALE }
procedure Agg2D.scale(sx ,sy : double );
var
 tas : trans_affine_scaling;

begin
 tas.Construct(sx ,sy );

 m_transform.multiply(@tas );

 m_convCurve.approximation_scale_ (worldToScreen(1.0 ) * g_approxScale );
 m_convStroke.approximation_scale_(worldToScreen(1.0 ) * g_approxScale );

end;

{ SKEW }
procedure Agg2D.skew(sx ,sy : double );
var
 tas : trans_affine_skewing;

begin
 tas.Construct(sx ,sy );

 m_transform.multiply(@tas );

end;

{ TRANSLATE }
procedure Agg2D.translate(x ,y : double );
var
 tat : trans_affine_translation;

begin
 tat.Construct(x ,y );

 m_transform.multiply(@tat );

end;

{ PARALLELOGRAM }
procedure Agg2D.parallelogram(x1 ,y1 ,x2 ,y2 : double; para : double_ptr );
var
 ta : trans_affine;

begin
 ta.Construct(x1 ,y1 ,x2 ,y2 ,parallelo_ptr(para ) );

 m_transform.multiply(@ta );

 m_convCurve.approximation_scale_ (worldToScreen(1.0 ) * g_approxScale );
 m_convStroke.approximation_scale_(worldToScreen(1.0 ) * g_approxScale );

end;

{ VIEWPORT }
procedure Agg2D.viewport(
           worldX1  ,worldY1  ,worldX2  ,worldY2 ,
           screenX1 ,screenY1 ,screenX2 ,screenY2 : double;
           opt : ViewportOption = XMidYMid );
var
 vp : trans_viewport;
 mx : trans_affine;

begin
 vp.Construct;

 case opt of
  Anisotropic :
   vp.preserve_aspect_ratio(0.0 ,0.0 ,aspect_ratio_stretch );

  XMinYMin :
   vp.preserve_aspect_ratio(0.0 ,0.0 ,aspect_ratio_meet );

  XMidYMin :
   vp.preserve_aspect_ratio(0.5 ,0.0 ,aspect_ratio_meet );

  XMaxYMin :
   vp.preserve_aspect_ratio(1.0 ,0.0 ,aspect_ratio_meet );

  XMinYMid :
   vp.preserve_aspect_ratio(0.0 ,0.5 ,aspect_ratio_meet );

  XMidYMid :
   vp.preserve_aspect_ratio(0.5 ,0.5 ,aspect_ratio_meet );

  XMaxYMid :
   vp.preserve_aspect_ratio(1.0 ,0.5 ,aspect_ratio_meet );

  XMinYMax :
   vp.preserve_aspect_ratio(0.0 ,1.0 ,aspect_ratio_meet );

  XMidYMax :
   vp.preserve_aspect_ratio(0.5 ,1.0 ,aspect_ratio_meet );

  XMaxYMax :
   vp.preserve_aspect_ratio(1.0 ,1.0 ,aspect_ratio_meet );

 end;

 vp.world_viewport (worldX1  ,worldY1  ,worldX2  ,worldY2 );
 vp.device_viewport(screenX1 ,screenY1 ,screenX2 ,screenY2 );

 mx.Construct;

 vp.to_affine        (@mx );
 m_transform.multiply(@mx );

 m_convCurve.approximation_scale_ (worldToScreen(1.0 ) * g_approxScale );
 m_convStroke.approximation_scale_(worldToScreen(1.0 ) * g_approxScale );

end;

{ LINE }
procedure Agg2D.line(x1 ,y1 ,x2 ,y2 : double );
begin
 m_path.remove_all;

 addLine (x1 ,y1 ,x2 ,y2 );
 drawPath(StrokeOnly );

end;

{ TRIANGLE }
procedure Agg2D.triangle(x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double );
begin
 m_path.remove_all;
 m_path.move_to(x1 ,y1 );
 m_path.line_to(x2 ,y2 );
 m_path.line_to(x3 ,y3 );
 m_path.close_polygon;

 drawPath(FillAndStroke );

end;

{ RECTANGLE }
procedure Agg2D.rectangle(x1 ,y1 ,x2 ,y2 : double );
begin
 m_path.remove_all;
 m_path.move_to(x1 ,y1 );
 m_path.line_to(x2 ,y1 );
 m_path.line_to(x2 ,y2 );
 m_path.line_to(x1 ,y2 );
 m_path.close_polygon;

 drawPath(FillAndStroke );

end;

{ ROUNDEDRECT }
procedure Agg2D.roundedRect(x1 ,y1 ,x2 ,y2 ,r : double );
var
 rc : rounded_rect;

begin
 m_path.remove_all;
 rc.Construct(x1 ,y1 ,x2 ,y2 ,r );

 rc.normalize_radius;
 rc.approximation_scale_(worldToScreen(1.0 ) * g_approxScale );

 m_path.add_path(@rc ,0 ,false );

 drawPath(FillAndStroke );

end;

{ ROUNDEDRECT }
procedure Agg2D.roundedRect(x1 ,y1 ,x2 ,y2 ,rx ,ry : double );
var
 rc : rounded_rect;

begin
 m_path.remove_all;
 rc.Construct;

 rc.rect  (x1 ,y1 ,x2 ,y2 );
 rc.radius(rx ,ry );
 rc.normalize_radius;

 m_path.add_path(@rc ,0 ,false );

 drawPath(FillAndStroke );

end;

{ ROUNDEDRECT }
procedure Agg2D.roundedRect(
           x1 ,y1 ,x2 ,y2 ,
           rxBottom ,ryBottom ,
           rxTop ,ryTop : double );
var
 rc : rounded_rect;

begin
 m_path.remove_all;
 rc.Construct;

 rc.rect  (x1 ,y1 ,x2 ,y2 );
 rc.radius(rxBottom ,ryBottom ,rxTop ,ryTop );
 rc.normalize_radius;

 rc.approximation_scale_(worldToScreen(1.0 ) * g_approxScale );

 m_path.add_path(@rc ,0 ,false );

 drawPath(FillAndStroke );

end;

{ ELLIPSE }
procedure Agg2D.ellipse(cx ,cy ,rx ,ry : double );
var
 el : bezier_arc;

begin
 m_path.remove_all;

 el.Construct(cx ,cy ,rx ,ry ,0 ,2 * pi );

 m_path.add_path(@el ,0 ,false );
 m_path.close_polygon;

 drawPath(FillAndStroke );

end;

{ ARC }
procedure Agg2D.arc(cx ,cy ,rx ,ry ,start ,sweep : double );
var
 ar : {bezier_}agg_arc.arc;

begin
 m_path.remove_all;

 ar.Construct(cx ,cy ,rx ,ry ,start ,sweep ,false );

 m_path.add_path(@ar ,0 ,false );

 drawPath(StrokeOnly );

end;

{ STAR }
procedure Agg2D.star(cx ,cy ,r1 ,r2 ,startAngle : double; numRays : int );
var
 da ,a ,x ,y : double;

 i : int;

begin
 m_path.remove_all;

 da:=pi / numRays;
 a :=startAngle;

 i:=0;

 while i < numRays do
  begin
   x:=Cos(a ) * r2 + cx;
   y:=Sin(a ) * r2 + cy;

   if i <> 0 then
    m_path.line_to(x ,y )
   else
    m_path.move_to(x ,y );

   a:=a + da;

   m_path.line_to(Cos(a ) * r1 + cx ,Sin(a ) * r1 + cy );

   a:=a + da;

   inc(i );

  end;

 closePolygon;
 drawPath(FillAndStroke );

end;

{ CURVE }
procedure Agg2D.curve(x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double );
begin
 m_path.remove_all;
 m_path.move_to(x1 ,y1 );
 m_path.curve3 (x2 ,y2 ,x3 ,y3 );

 drawPath(StrokeOnly );

end;

{ CURVE }
procedure Agg2D.curve(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 : double );
begin
 m_path.remove_all;
 m_path.move_to(x1 ,y1 );
 m_path.curve4 (x2 ,y2 ,x3 ,y3 ,x4 ,y4 );

 drawPath(StrokeOnly );

end;

{ POLYGON }
procedure Agg2D.polygon(xy : double_ptr; numPoints : int );
begin
 m_path.remove_all;
 m_path.add_poly(double_2_ptr(xy ) ,numPoints );

 closePolygon;
 drawPath(FillAndStroke );

end;

{ POLYLINE }
procedure Agg2D.polyline(xy : double_ptr; numPoints : int );
begin
 m_path.remove_all;
 m_path.add_poly(double_2_ptr(xy ) ,numPoints );

 drawPath(StrokeOnly );

end;

{ FLIPTEXT }
procedure Agg2D.flipText(flip : boolean );
begin
 {$IFNDEF AGG2D_NO_FONT}
 m_fontEngine.flip_y_(flip );
 {$ENDIF}
end;

{ FONT }
procedure Agg2D.font(
           fileName : char_ptr; height : double;
           bold : boolean = false;
           italic : boolean = false;
           ch : FontCacheType = RasterFontCache;
           angle : double = 0.0 );
var
 b : int;

begin
 m_textAngle    :=angle;
 m_fontHeight   :=height;
 m_fontCacheType:=ch;

{$IFDEF AGG2D_USE_FREETYPE }
 if ch = VectorFontCache then
  m_fontEngine.load_font(PChar(fileName ) ,0 ,glyph_ren_outline )
 else
  m_fontEngine.load_font(PChar(fileName ) ,0 ,glyph_ren_agg_gray8 );

 m_fontEngine.hinting_(m_textHints );

 if ch = VectorFontCache then
  m_fontEngine.height_(height )
 else
  m_fontEngine.height_(worldToScreen(height ) );

{$ENDIF }
{$IFDEF AGG2D_USE_WINFONTS}

 m_fontEngine.hinting_(m_textHints );

 if bold then
  b:=700
 else
  b:=400;

 if ch = VectorFontCache then
  m_fontEngine.create_font_(PChar(fileName ) ,glyph_ren_outline ,height ,0.0 ,b ,italic )
 else
  m_fontEngine.create_font_(PChar(fileName ) ,glyph_ren_agg_gray8 ,worldToScreen(height) ,0.0 ,b ,italic );

{$ENDIF }

end;

{ FONTHEIGHT }
function Agg2D.fontHeight : double;
begin
 result:=m_fontHeight;

end;

{ TEXTALIGNMENT }
procedure Agg2D.textAlignment(alignX ,alignY : TextAlignment );
begin
 m_textAlignX:=alignX;
 m_textAlignY:=alignY;

end;

{ TEXTHINTS }
function Agg2D.textHints : boolean;
begin
 result:=m_textHints;

end;

{ TEXTHINTS }
procedure Agg2D.textHints(hints : boolean );
begin
 m_textHints:=hints;

end;

{ TEXTWIDTH }
function Agg2D.textWidth(str : char_ptr ) : double;
{$IFDEF AGG2D_NO_FONT}
begin
  Result:=0;
end;
{$ELSE}
var
 x ,y  : double;
 first : boolean;
 glyph : glyph_cache_ptr;

begin
 x:=0;
 y:=0;

 first:=true;

 while str^ <> #0 do
  begin
   glyph:=m_fontCacheManager.glyph(int32u(str^ ) );

   if glyph <> NIL then
    begin
     if not first then
      m_fontCacheManager.add_kerning(@x ,@y );

     x:=x + glyph.advance_x;
     y:=y + glyph.advance_y;

     first:=false; {!}

    end;

   inc(ptrcomp(str ) );

  end;

 if m_fontCacheType = VectorFontCache then
  result:=x
 else
  result:=screenToWorld(x );

end;
{$ENDIF}

{ TEXT }
procedure Agg2D.text(
           x ,y : double; str : char_ptr;
           roundOff : boolean = false;
           ddx : double = 0.0;
           ddy : double = 0.0 );
{$IFDEF AGG2D_NO_FONT}
begin

end;
{$ELSE}
var
 dx ,dy ,asc ,start_x ,start_y : double;

 glyph : glyph_cache_ptr;

 mtx : trans_affine;

 i : int;

 tat : trans_affine_translation;
 tar : trans_affine_rotation;

 tr : conv_transform;

begin
 dx:=0.0;
 dy:=0.0;

 case m_textAlignX of
  AlignCenter :
   dx:=-textWidth(str ) * 0.5;

  AlignRight :
   dx:=-textWidth(str );

 end;

 asc  :=fontHeight;
 glyph:=m_fontCacheManager.glyph(int32u('H' ) );

 if glyph <> NIL then
  asc:=glyph.bounds.y2 - glyph.bounds.y1;

 if m_fontCacheType = RasterFontCache then
  asc:=screenToWorld(asc );

 case m_textAlignY of
  AlignCenter :
   dy:=-asc * 0.5;

  AlignTop :
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

 tar.Construct(m_textAngle );
 mtx.multiply (@tar );

 tat.Construct(x ,y );
 mtx.multiply (@tat );

 tr.Construct(m_fontCacheManager.path_adaptor ,@mtx );

 if m_fontCacheType = RasterFontCache then
  worldToScreen(@start_x ,@start_y );

 i:=0;

 while char_ptr(ptrcomp(str ) + i * sizeof(char ) )^ <> #0 do
  begin
   glyph:=m_fontCacheManager.glyph(int32u(char_ptr(ptrcomp(str ) + i * sizeof(char ) )^ ) );

   if glyph <> NIL then
    begin
     if i <> 0 then
      m_fontCacheManager.add_kerning(@x ,@y );

     m_fontCacheManager.init_embedded_adaptors(glyph ,start_x ,start_y );

     if glyph.data_type = glyph_data_outline then
      begin
       m_path.remove_all;
       m_path.add_path(@tr ,0 ,false );

       drawPath;

      end;

     if glyph.data_type = glyph_data_gray8 then
      begin
       render(
        m_fontCacheManager.gray8_adaptor ,
        m_fontCacheManager.gray8_scanline );

      end;

     start_x:=start_x + glyph.advance_x;
     start_y:=start_y + glyph.advance_y;

    end;

   inc(i );

  end;

end;
{$ENDIF}

{ RESETPATH }
procedure Agg2D.resetPath;
begin
 m_path.remove_all;

end;

{ MOVETO }
procedure Agg2D.moveTo(x ,y : double );
begin
 m_path.move_to(x ,y );

end;

{ MOVEREL }
procedure Agg2D.moveRel(dx ,dy : double );
begin
 m_path.move_rel(dx ,dy );

end;

{ LINETO }
procedure Agg2D.lineTo(x ,y : double );
begin
 m_path.line_to(x ,y );

end;

{ LINEREL }
procedure Agg2D.lineRel(dx ,dy : double );
begin
 m_path.line_rel(dx ,dy );

end;

{ HORLINETO }
procedure Agg2D.horLineTo(x : double );
begin
 m_path.hline_to(x );

end;

{ HORLINEREL }
procedure Agg2D.horLineRel(dx : double );
begin
 m_path.hline_rel(dx );

end;

{ VERLINETO }
procedure Agg2D.verLineTo(y : double );
begin
 m_path.vline_to(y );

end;

{ VERLINEREL }
procedure Agg2D.verLineRel(dy : double );
begin
 m_path.vline_rel(dy );

end;

{ ARCTO }
procedure Agg2D.arcTo(
           rx ,ry ,angle : double;
           largeArcFlag ,sweepFlag : boolean;
           x ,y : double );
begin
 m_path.arc_to(rx ,ry ,angle ,largeArcFlag ,sweepFlag ,x ,y );

end;

{ ARCREL }
procedure Agg2D.arcRel(
           rx ,ry ,angle : double;
           largeArcFlag ,sweepFlag : boolean;
           dx ,dy : double );
begin
 m_path.arc_rel(rx ,ry ,angle ,largeArcFlag ,sweepFlag ,dx ,dy );

end;

{ QUADRICCURVETO }
procedure Agg2D.quadricCurveTo (xCtrl ,yCtrl ,xTo ,yTo : double );
begin
 m_path.curve3(xCtrl ,yCtrl ,xTo ,yTo );

end;

{ QUADRICCURVEREL }
procedure Agg2D.quadricCurveRel(dxCtrl ,dyCtrl ,dxTo ,dyTo : double );
begin
 m_path.curve3_rel(dxCtrl ,dyCtrl ,dxTo ,dyTo );

end;

{ QUADRICCURVETO }
procedure Agg2D.quadricCurveTo (xTo ,yTo : double );
begin
 m_path.curve3(xTo ,yTo );

end;

{ QUADRICCURVEREL }
procedure Agg2D.quadricCurveRel(dxTo ,dyTo : double );
begin
 m_path.curve3_rel(dxTo ,dyTo );

end;

{ CUBICCURVETO }
procedure Agg2D.cubicCurveTo (xCtrl1 ,yCtrl1 ,xCtrl2 ,yCtrl2 ,xTo ,yTo : double );
begin
 m_path.curve4(xCtrl1 ,yCtrl1 ,xCtrl2 ,yCtrl2 ,xTo ,yTo );

end;

{ CUBICCURVEREL }
procedure Agg2D.cubicCurveRel(dxCtrl1 ,dyCtrl1 ,dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo : double );
begin
 m_path.curve4_rel(dxCtrl1 ,dyCtrl1 ,dxCtrl2 ,dyCtrl2 ,dxTo ,dyTo );

end;

{ CUBICCURVETO }
procedure Agg2D.cubicCurveTo (xCtrl2 ,yCtrl2 ,xTo ,yTo : double );
begin
 m_path.curve4(xCtrl2 ,yCtrl2 ,xTo ,yTo );

end;

{ CUBICCURVEREL }
procedure Agg2D.cubicCurveRel(xCtrl2 ,yCtrl2 ,xTo ,yTo : double );
begin
 m_path.curve4_rel(xCtrl2 ,yCtrl2 ,xTo ,yTo );

end;

{ ADDELLIPSE }
procedure Agg2D.addEllipse(cx ,cy ,rx ,ry : double; dir : Direction );
var
 ar : bezier_arc;

begin
 if dir = CCW then
  ar.Construct(cx ,cy ,rx ,ry ,0 ,2 * pi )
 else
  ar.Construct(cx ,cy ,rx ,ry ,0 ,-2 * pi );

 m_path.add_path(@ar ,0 ,false );
 m_path.close_polygon;

end;

{ CLOSEPOLYGON }
procedure Agg2D.closePolygon;
begin
 m_path.close_polygon;

end;

{ DRAWPATH }
procedure Agg2D.drawPath(flag : DrawPathFlag = FillAndStroke );
begin
 m_rasterizer.reset;

 case flag of
  FillOnly :
   if m_fillColor.a <> 0 then
    begin
     m_rasterizer.add_path(@m_pathTransform );

     render(true );

    end;

  StrokeOnly :
   if (m_lineColor.a <> 0 ) and
      (m_lineWidth > 0.0 ) then
    begin
     m_rasterizer.add_path(@m_strokeTransform );

     render(false );

    end;

  FillAndStroke :
   begin
    if m_fillColor.a <> 0 then
     begin
      m_rasterizer.add_path(@m_pathTransform );

      render(true );

     end;

    if (m_lineColor.a <> 0 ) and
       (m_lineWidth > 0.0 ) then
     begin
      m_rasterizer.add_path(@m_strokeTransform );

      render(false );

     end;

   end;

  FillWithLineColor :
   if m_lineColor.a <> 0 then
    begin
     m_rasterizer.add_path(@m_pathTransform );

     render(false );

    end;

 end;

end;

{ DRAWPATHNOTRANSFORM }
procedure Agg2D.drawPathNoTransform(flag : DrawPathFlag = FillAndStroke );
begin
end;

{ IMAGEFILTER }
procedure Agg2D.imageFilter(f : ImageFilter_ );
begin
 m_imageFilter:=f;

 case f of
  Bilinear :
   m_imageFilterLut.calculate(@m_ifBilinear ,true );

  Hanning :
   m_imageFilterLut.calculate(@m_ifHanning ,true );

  Hermite :
   m_imageFilterLut.calculate(@m_ifHermite ,true );

  Quadric :
   m_imageFilterLut.calculate(@m_ifQuadric ,true );

  Bicubic :
   m_imageFilterLut.calculate(@m_ifBicubic ,true );

  Catrom :
   m_imageFilterLut.calculate(@m_ifCatrom ,true );

  Spline16 :
   m_imageFilterLut.calculate(@m_ifSpline16 ,true );

  Spline36 :
   m_imageFilterLut.calculate(@m_ifSpline36 ,true );

  Blackman144 :
   m_imageFilterLut.calculate(@m_ifBlackman144 ,true );

 end;

end;

{ IMAGEFILTER }
function Agg2D.imageFilter : ImageFilter_;
begin
 result:=m_imageFilter;

end;

{ IMAGERESAMPLE }
procedure Agg2D.imageResample(f : ImageResample_ );
begin
 m_imageResample:=f;

end;

{ IMAGERESAMPLE }
function Agg2D.imageResample : ImageResample_;
begin
 result:=m_imageResample;

end;

{ TRANSFORMIMAGE }
procedure Agg2D.transformImage(
           img : Image_ptr;
           imgX1 ,imgY1 ,imgX2 ,imgY2 : int;
           dstX1 ,dstY1 ,dstX2 ,dstY2 : double );
var
 parall : array[0..5 ] of double;

begin
 resetPath;
 moveTo(dstX1 ,dstY1 );
 lineTo(dstX2 ,dstY1 );
 lineTo(dstX2 ,dstY2 );
 lineTo(dstX1 ,dstY2 );
 closePolygon;

 parall[0 ]:=dstX1;
 parall[1 ]:=dstY1;
 parall[2 ]:=dstX2;
 parall[3 ]:=dstY1;
 parall[4 ]:=dstX2;
 parall[5 ]:=dstY2;

 renderImage(img ,imgX1 ,imgY1 ,imgX2 ,imgY2 ,@parall[0 ] );

end;

{ TRANSFORMIMAGE }
procedure Agg2D.transformImage(
           img : Image_ptr;
           dstX1 ,dstY1 ,dstX2 ,dstY2 : double );
var
 parall : array[0..5 ] of double;

begin
 resetPath;
 moveTo(dstX1 ,dstY1 );
 lineTo(dstX2 ,dstY1 );
 lineTo(dstX2 ,dstY2 );
 lineTo(dstX1 ,dstY2 );
 closePolygon;

 parall[0 ]:=dstX1;
 parall[1 ]:=dstY1;
 parall[2 ]:=dstX2;
 parall[3 ]:=dstY1;
 parall[4 ]:=dstX2;
 parall[5 ]:=dstY2;

 renderImage(img ,0 ,0 ,img.renBuf._width ,img.renBuf._height ,@parall[0 ] );

end;

{ TRANSFORMIMAGE }
procedure Agg2D.transformImage(
           img : Image_ptr;
           imgX1 ,imgY1 ,imgX2 ,imgY2 : int;
           parallelogram_ : double_ptr );
begin
 resetPath;

 moveTo(
  double_ptr(ptrcomp(parallelogram_ ) + 0 * sizeof(double ) )^ ,
  double_ptr(ptrcomp(parallelogram_ ) + 1 * sizeof(double ) )^ );

 lineTo(
  double_ptr(ptrcomp(parallelogram_ ) + 2 * sizeof(double ) )^ ,
  double_ptr(ptrcomp(parallelogram_ ) + 3 * sizeof(double ) )^ );

 lineTo(
  double_ptr(ptrcomp(parallelogram_ ) + 4 * sizeof(double ) )^ ,
  double_ptr(ptrcomp(parallelogram_ ) + 5 * sizeof(double ) )^ );

 lineTo(
  double_ptr(ptrcomp(parallelogram_ ) + 0 * sizeof(double ) )^ +
  double_ptr(ptrcomp(parallelogram_ ) + 4 * sizeof(double ) )^ -
  double_ptr(ptrcomp(parallelogram_ ) + 2 * sizeof(double ) )^ ,
  double_ptr(ptrcomp(parallelogram_ ) + 1 * sizeof(double ) )^ +
  double_ptr(ptrcomp(parallelogram_ ) + 5 * sizeof(double ) )^ -
  double_ptr(ptrcomp(parallelogram_ ) + 3 * sizeof(double ) )^ );

 closePolygon;

 renderImage(img ,imgX1 ,imgY1 ,imgX2 ,imgY2 ,parallelogram_ );

end;

{ TRANSFORMIMAGE }
procedure Agg2D.transformImage(img : Image_ptr; parallelogram_ : double_ptr );
begin
 resetPath;

 moveTo(
  double_ptr(ptrcomp(parallelogram_ ) + 0 * sizeof(double ) )^ ,
  double_ptr(ptrcomp(parallelogram_ ) + 1 * sizeof(double ) )^ );

 lineTo(
  double_ptr(ptrcomp(parallelogram_ ) + 2 * sizeof(double ) )^ ,
  double_ptr(ptrcomp(parallelogram_ ) + 3 * sizeof(double ) )^ );

 lineTo(
  double_ptr(ptrcomp(parallelogram_ ) + 4 * sizeof(double ) )^ ,
  double_ptr(ptrcomp(parallelogram_ ) + 5 * sizeof(double ) )^ );

 lineTo(
  double_ptr(ptrcomp(parallelogram_ ) + 0 * sizeof(double ) )^ +
  double_ptr(ptrcomp(parallelogram_ ) + 4 * sizeof(double ) )^ -
  double_ptr(ptrcomp(parallelogram_ ) + 2 * sizeof(double ) )^ ,
  double_ptr(ptrcomp(parallelogram_ ) + 1 * sizeof(double ) )^ +
  double_ptr(ptrcomp(parallelogram_ ) + 5 * sizeof(double ) )^ -
  double_ptr(ptrcomp(parallelogram_ ) + 3 * sizeof(double ) )^ );

 closePolygon;

 renderImage(img ,0 ,0 ,img.renBuf._width ,img.renBuf._height ,parallelogram_ );

end;

{ TRANSFORMIMAGEPATH }
procedure Agg2D.transformImagePath(
           img : Image_ptr;
           imgX1 ,imgY1 ,imgX2 ,imgY2 : int;
           dstX1 ,dstY1 ,dstX2 ,dstY2 : double );
var
 parall : array[0..5 ] of double;

begin
 parall[0 ]:=dstX1;
 parall[1 ]:=dstY1;
 parall[2 ]:=dstX2;
 parall[3 ]:=dstY1;
 parall[4 ]:=dstX2;
 parall[5 ]:=dstY2;

 renderImage(img ,imgX1 ,imgY1 ,imgX2 ,imgY2 ,@parall[0 ] );

end;

{ TRANSFORMIMAGEPATH }
procedure Agg2D.transformImagePath(
           img : Image_ptr;
           dstX1 ,dstY1 ,dstX2 ,dstY2 : double );
var
 parall : array[0..5 ] of double;

begin
 parall[0 ]:=dstX1;
 parall[1 ]:=dstY1;
 parall[2 ]:=dstX2;
 parall[3 ]:=dstY1;
 parall[4 ]:=dstX2;
 parall[5 ]:=dstY2;

 renderImage(img ,0 ,0 ,img.renBuf._width ,img.renBuf._height ,@parall[0 ] );

end;

{ TRANSFORMIMAGEPATH }
procedure Agg2D.transformImagePath(
           img : Image_ptr;
           imgX1 ,imgY1 ,imgX2 ,imgY2 : int;
           parallelogram_ : double_ptr );
begin
 renderImage(img ,imgX1 ,imgY1 ,imgX2 ,imgY2 ,parallelogram_ );

end;

{ TRANSFORMIMAGEPATH }
procedure Agg2D.transformImagePath(img : Image_ptr; parallelogram_ : double_ptr );
begin
 renderImage(img ,0 ,0 ,img.renBuf._width ,img.renBuf._height ,parallelogram_ );

end;

{ BLENDIMAGE }
procedure Agg2D.blendImage(
           img : Image_ptr;
           imgX1 ,imgY1 ,imgX2 ,imgY2 : int;
           dstX ,dstY : double; alpha : unsigned = 255 );
var
 pixF : pixel_formats;

 r : agg_basics.rect;

begin
 worldToScreen(@dstX ,@dstY );
 pixfmt_rgba32(pixF ,@img.renBuf );
 r.Construct  (imgX1 ,imgY1 ,imgX2 ,imgY2 );

 if m_blendMode = BlendAlpha then
  m_renBasePre.blend_from(@pixF ,@r ,Trunc(dstX ) - imgX1 ,Trunc(dstY ) - imgY1 ,alpha )
 else
  m_renBaseCompPre.blend_from(@pixF ,@r ,Trunc(dstX ) - imgX1 ,Trunc(dstY ) - imgY1 ,alpha );

end;

{ BLENDIMAGE }
procedure Agg2D.blendImage(img : Image_ptr; dstX ,dstY : double; alpha : unsigned = 255 );
var
 pixF : pixel_formats;

begin
 worldToScreen(@dstX ,@dstY );
 pixfmt_rgba32(pixF ,@img.renBuf );

 m_renBasePre.blend_from(@pixF ,NIL ,Trunc(dstX ) ,Trunc(dstY ) ,alpha );

 if m_blendMode = BlendAlpha then
  m_renBasePre.blend_from(@pixF ,NIL ,Trunc(dstX ) ,Trunc(dstY ) ,alpha )
 else
  m_renBaseCompPre.blend_from(@pixF ,NIL ,Trunc(dstX ) ,Trunc(dstY ) ,alpha );

end;

{ COPYIMAGE }
procedure Agg2D.copyImage(
           img : Image_ptr;
           imgX1 ,imgY1 ,imgX2 ,imgY2 : int;
           dstX ,dstY : double );
var
 r : agg_basics.rect;

begin
 worldToScreen(@dstX ,@dstY );
 r.Construct  (imgX1 ,imgY1 ,imgX2 ,imgY2 );

 m_renBase.copy_from(@img.renBuf ,@r ,Trunc(dstX ) - imgX1 ,Trunc(dstY ) - imgY1 );

end;

{ COPYIMAGE }
procedure Agg2D.copyImage(img : Image_ptr; dstX ,dstY : double );
begin
 worldToScreen(@dstX ,@dstY );

 m_renBase.copy_from(@img.renBuf ,NIL ,Trunc(dstX ) ,Trunc(dstY ) );

end;

{ RENDER }
procedure Agg2D.render(fillColor_ : boolean );
begin
 if m_blendMode = BlendAlpha then
  Agg2DRenderer_render(@self ,@m_renBase ,@m_renSolid ,fillColor_ )
 else
  Agg2DRenderer_render(@self ,@m_renBaseComp ,@m_renSolidComp ,fillColor_ );

end;

{ RENDER }
procedure Agg2D.render(ras : FontRasterizer_ptr; sl : FontScanline_ptr );
begin
 if m_blendMode = BlendAlpha then
  Agg2DRenderer_render(@self ,@m_renBase ,@m_renSolid ,ras ,sl )
 else
  Agg2DRenderer_render(@self ,@m_renBaseComp ,@m_renSolidComp ,ras ,sl );

end;

{ ADDLINE }
procedure Agg2D.addLine(x1 ,y1 ,x2 ,y2 : double );
begin
 m_path.move_to(x1 ,y1 );
 m_path.line_to(x2 ,y2 );

end;

{ UPDATERASTERIZERGAMMA }
procedure Agg2D.updateRasterizerGamma;
begin
 m_gammaAgg2D.Construct(m_masterAlpha ,m_antiAliasGamma );
 m_rasterizer.gamma    (@m_gammaAgg2D );

end;

{ RENDERIMAGE }
procedure Agg2D.renderImage(
           img : Image_ptr;
           x1 ,y1 ,x2 ,y2 : int;
           parl : double_ptr );
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

 if m_blendMode = BlendAlpha then
  Agg2DRenderer_renderImage(@self ,img ,@m_renBasePre ,@interpolator )
 else
  Agg2DRenderer_renderImage(@self ,img ,@m_renBaseCompPre ,@interpolator );

end;

{ CONSTRUCT }
constructor SpanConvImageBlend.Construct(m : BlendMode_; c : Color; p : pixel_formats_ptr );
begin
 m_mode :=m;
 m_color:=c;
 m_pixel:=p;

end;

{ CONVERT }
procedure SpanConvImageBlend.convert(span : aggclr_ptr; x ,y : int; len : unsigned );
var
 l2 ,a : unsigned;

 s2 : Color_ptr;

begin
 if m_mode <> BlendDst then
  begin
   l2:=len;
   s2:=Color_ptr(span );

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

    inc(ptrcomp(s2 ) ,sizeof(Color ) );
    dec(l2 );

   until l2 = 0;

  end;

 if m_color.a < base_mask then
  begin
   l2:=len;
   s2:=Color_ptr(span );
   a :=m_color.a;

   repeat
    s2.r:=(s2.r * a ) shr base_shift;
    s2.g:=(s2.g * a ) shr base_shift;
    s2.b:=(s2.b * a ) shr base_shift;
    s2.a:=(s2.a * a ) shr base_shift;

    inc(ptrcomp(s2 ) ,sizeof(Color ) );
    dec(l2 );

   until l2 = 0;

  end;

end;

{ PI }
function pi : double;
begin
 result:=agg_basics.pi;

end;

{ DEG2RAD }
function deg2Rad(v : double ) : double;
begin
 result:=v * agg_basics.pi / 180.0;

end;

{ RAD2DEG }
function rad2Deg(v : double ) : double;
begin
 result:=v * 180.0 / agg_basics.pi;

end;

{ OPERATOR_IS_EQUAL }
function operator_is_equal(c1 ,c2 : Color_ptr ) : boolean;
begin
 result:=
  (c1.r = c2.r ) and
  (c1.g = c2.g ) and
  (c1.b = c2.b ) and
  (c1.a = c2.a );

end;

{ OPERATOR_IS_NOT_EQUAL }
function operator_is_not_equal(c1 ,c2 : Color_ptr ) : boolean;
begin
 result:=not operator_is_equal(c1 ,c2 );

end;

{ AGG2DRENDERER_RENDER }
procedure Agg2DRenderer_render(
           gr : Agg2D_ptr;
           renBase : renderer_base_ptr;
           renSolid : renderer_scanline_aa_solid_ptr;
           fillColor_ : boolean );
var
 span : span_gradient;
 ren  : renderer_scanline_aa;
 clr  : aggclr;

begin
 if (fillColor_ and
     (gr.m_fillGradientFlag = Linear ) ) or
    (not fillColor_ and
     (gr.m_lineGradientFlag = Linear ) ) then
  if fillColor_ then
   begin
    span.Construct(
     @gr.m_allocator ,
     @gr.m_fillGradientInterpolator ,
     @gr.m_linearGradientFunction ,
     @gr.m_fillGradient ,
     gr.m_fillGradientD1 ,
     gr.m_fillGradientD2 );

    ren.Construct   (renBase ,@span );
    render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ren );

   end
  else
   begin
    span.Construct(
     @gr.m_allocator ,
     @gr.m_lineGradientInterpolator ,
     @gr.m_linearGradientFunction ,
     @gr.m_lineGradient ,
     gr.m_lineGradientD1 ,
     gr.m_lineGradientD2 );

    ren.Construct   (renBase ,@span );
    render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ren );

   end
 else
  if (fillColor_ and
      (gr.m_fillGradientFlag = Radial ) ) or
     (not fillColor_ and
      (gr.m_lineGradientFlag = Radial ) ) then
   if fillColor_ then
    begin
     span.Construct(
      @gr.m_allocator ,
      @gr.m_fillGradientInterpolator ,
      @gr.m_radialGradientFunction ,
      @gr.m_fillGradient ,
      gr.m_fillGradientD1 ,
      gr.m_fillGradientD2 );

      ren.Construct   (renBase ,@span );
      render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ren );

    end
   else
    begin
     span.Construct(
      @gr.m_allocator ,
      @gr.m_lineGradientInterpolator ,
      @gr.m_radialGradientFunction ,
      @gr.m_lineGradient ,
      gr.m_lineGradientD1 ,
      gr.m_lineGradientD2 );

     ren.Construct   (renBase ,@span );
     render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ren );

    end
  else
   begin
    if fillColor_ then
     clr.Construct(gr.m_fillColor )
    else
     clr.Construct(gr.m_lineColor );

    renSolid.color_ (@clr );
    render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,renSolid );

   end;

end;

{ AGG2DRENDERER_RENDER }
procedure Agg2DRenderer_render(
           gr : Agg2D_ptr;
           renBase : renderer_base_ptr;
           renSolid : renderer_scanline_aa_solid_ptr;
           ras : gray8_adaptor_type_ptr;
           sl : gray8_scanline_type_ptr );
var
 span : span_gradient;
 ren  : renderer_scanline_aa;
 clr  : aggclr;

begin
 if gr.m_fillGradientFlag = Linear then
  begin
   span.Construct(
    @gr.m_allocator ,
    @gr.m_fillGradientInterpolator ,
    @gr.m_linearGradientFunction ,
    @gr.m_fillGradient ,
    gr.m_fillGradientD1 ,
    gr.m_fillGradientD2 );

   ren.Construct   (renBase ,@span );
   render_scanlines(ras ,sl ,@ren );

  end
 else
  if gr.m_fillGradientFlag = Radial then
   begin
    span.Construct(
     @gr.m_allocator ,
     @gr.m_fillGradientInterpolator ,
     @gr.m_radialGradientFunction ,
     @gr.m_fillGradient ,
     gr.m_fillGradientD1 ,
     gr.m_fillGradientD2 );

    ren.Construct   (renBase ,@span );
    render_scanlines(ras ,sl ,@ren );

   end
  else
   begin
    clr.Construct   (gr.m_fillColor );
    renSolid.color_ (@clr );
    render_scanlines(ras ,sl ,renSolid );

   end;

end;

{ AGG2DRENDERER_RENDERIMAGE }
procedure Agg2DRenderer_renderImage(
           gr : Agg2D_ptr;
           img : Image_ptr;
           renBase : renderer_base_ptr;
           interpolator : span_interpolator_linear_ptr );
var
 blend : SpanConvImageBlend;

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
 blend.Construct(gr.m_imageBlendMode ,gr.m_imageBlendColor ,@gr.m_pixFormatCompPre );

 if gr.m_imageFilter = NoFilter then
  begin
   clr.ConstrInt(0 ,0 ,0 ,0 );
   sg.Construct (@gr.m_allocator ,@img.renBuf ,@clr ,interpolator ,rgba_order );
   sc.Construct (@sg ,@blend );
   ri.Construct (renBase ,@sc );

   render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ri );

  end
 else
  begin
   resample:=gr.m_imageResample = ResampleAlways;

   if gr.m_imageResample = ResampleOnZoomOut then
    begin
     interpolator._transformer.scaling_abs(@sx ,@sy );

     if (sx > 1.125 ) or
        (sy > 1.125 ) then
      resample:=true;

    end;

   if resample then
    begin
     clr.ConstrInt(0 ,0 ,0 ,0 );
     sa.Construct(
      @gr.m_allocator ,
      @img.renBuf ,
      @clr ,
      interpolator ,
      @gr.m_imageFilterLut ,
      rgba_order );

     sc.Construct(@sa ,@blend );
     ri.Construct(renBase ,@sc );

     render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ri );

    end
   else
    if gr.m_imageFilter = Bilinear then
     begin
      clr.ConstrInt(0 ,0 ,0 ,0 );
      sb.Construct(
       @gr.m_allocator ,
       @img.renBuf ,
       @clr ,
       interpolator ,
       rgba_order );

      sc.Construct(@sb ,@blend );
      ri.Construct(renBase ,@sc );

      render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ri );

     end
    else
     if gr.m_imageFilterLut.diameter = 2 then
      begin
       clr.ConstrInt(0 ,0 ,0 ,0 );
       s2.Construct(
        @gr.m_allocator ,
        @img.renBuf ,
        @clr ,
        interpolator,
        @gr.m_imageFilterLut ,
        rgba_order );

       sc.Construct(@s2 ,@blend );
       ri.Construct(renBase ,@sc );

       render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ri );

      end
     else
      begin
       clr.ConstrInt(0 ,0 ,0 ,0 );
       si.Construct(
        @gr.m_allocator ,
        @img.renBuf ,
        @clr ,
        interpolator ,
        @gr.m_imageFilterLut ,
        rgba_order );

       sc.Construct(@si ,@blend );
       ri.Construct(renBase ,@sc );

       render_scanlines(@gr.m_rasterizer ,@gr.m_scanline ,@ri );

      end;

  end;

end;

{ AGG2DUSESFREETYPE }
function Agg2DUsesFreeType : boolean;
begin
{$IFDEF AGG2D_USE_FREETYPE }
 result:=true;

{$ELSE }
 result:=false;

{$ENDIF }

end;

function Agg2DUsesWin32TrueType: boolean;
begin
{$IFDEF AGG2D_USE_WINFONTS }
 result:=true;

{$ELSE }
 result:=false;

{$ENDIF }
end;

END.

{!}

