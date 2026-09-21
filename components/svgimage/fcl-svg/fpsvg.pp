{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Easy access to all SVG identifiers

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses fpsvg.strings, fpsvg.types, fpsvg.backend, fpsvg.trace, fpsvg.dom,
     fpsvg.path, fpsvg.read, fpsvg.anim, fpsvg.style, fpsvg.geom,
     fpsvg.text, fpsvg.svgfont, fpsvg.render;
{$ELSE FPC_DOTTEDUNITS}
uses fpsvg.strings, fpsvg.types, fpsvg.backend, fpsvg.trace, fpsvg.dom,
     fpsvg.path, fpsvg.read, fpsvg.anim, fpsvg.style, fpsvg.geom,
     fpsvg.text, fpsvg.svgfont, fpsvg.render;
{$ENDIF FPC_DOTTEDUNITS}

type
  ESVGError = fpsvg.types.ESVGError;
  TSVGColor = fpsvg.types.TSVGColor;
  TSVGPoint = fpsvg.types.TSVGPoint;
  TSVGRect = fpsvg.types.TSVGRect;
  TSVGMatrix = fpsvg.types.TSVGMatrix;
  TSVGPath = fpsvg.types.TSVGPath;
  TSVGPaint = fpsvg.types.TSVGPaint;
  TSVGPen = fpsvg.types.TSVGPen;
  TSVGFillRule = fpsvg.types.TSVGFillRule;
  TSVGLineCap = fpsvg.types.TSVGLineCap;
  TSVGLineJoin = fpsvg.types.TSVGLineJoin;
  TSVGMaskMode = fpsvg.types.TSVGMaskMode;
  TSVGLength = fpsvg.types.TSVGLength;
  TSVGLengthUnit = fpsvg.types.TSVGLengthUnit;
  TSVGLengthAxis = fpsvg.types.TSVGLengthAxis;
  TSVGPreserveAspectRatio = fpsvg.types.TSVGPreserveAspectRatio;
  TSVGView = fpsvg.types.TSVGView;
  TSVGViewArray = fpsvg.types.TSVGViewArray;
  TSVGLengthContext = fpsvg.types.TSVGLengthContext;
  TSVGAspectAlign = fpsvg.types.TSVGAspectAlign;
  TSVGMeetOrSlice = fpsvg.types.TSVGMeetOrSlice;
  ISVGImageSource = fpsvg.types.ISVGImageSource;
  ISVGPaintServer = fpsvg.types.ISVGPaintServer;
  ISVGFont = fpsvg.types.ISVGFont;

  ESVGBackend = fpsvg.backend.ESVGBackend;
  TSVGRenderBackend = fpsvg.backend.TSVGRenderBackend;
  TSVGRenderBackendClass = fpsvg.backend.TSVGRenderBackendClass;
  TSVGBackendRegistry = fpsvg.backend.TSVGBackendRegistry;
  TSVGBackendCapability = fpsvg.backend.TSVGBackendCapability;
  TSVGBackendCapabilities = fpsvg.backend.TSVGBackendCapabilities;

  TSVGTraceBackend = fpsvg.trace.TSVGTraceBackend;
  TSVGPolyPath = fpsvg.geom.TSVGPolyPath;

  ESVGDOM = fpsvg.dom.ESVGDOM;
  TSVGNode = fpsvg.dom.TSVGNode;
  TSVGTextNode = fpsvg.dom.TSVGTextNode;
  TSVGElement = fpsvg.dom.TSVGElement;
  TSVGElementClass = fpsvg.dom.TSVGElementClass;
  TSVGDocument = fpsvg.dom.TSVGDocument;

  ESVGRead = fpsvg.read.ESVGRead;
  TSVGReader = fpsvg.read.TSVGReader;
  TSVGReadOption = fpsvg.read.TSVGReadOption;
  TSVGReadOptions = fpsvg.read.TSVGReadOptions;

  ESVGAnim = fpsvg.anim.ESVGAnim;
  TSVGAnimation = fpsvg.anim.TSVGAnimation;
  TSVGAnimationArray = fpsvg.anim.TSVGAnimationArray;
  TSVGAnimationKind = fpsvg.anim.TSVGAnimationKind;
  TSVGCalcMode = fpsvg.anim.TSVGCalcMode;
  TSVGValueKind = fpsvg.anim.TSVGValueKind;
  TSVGTransformKind = fpsvg.anim.TSVGTransformKind;
  TSVGMotionRotate = fpsvg.anim.TSVGMotionRotate;
  TSVGTimeEntry = fpsvg.anim.TSVGTimeEntry;
  TSVGTimeEntryKind = fpsvg.anim.TSVGTimeEntryKind;
  TSVGTimeEntryArray = fpsvg.anim.TSVGTimeEntryArray;
  TSVGRestart = fpsvg.anim.TSVGRestart;
  TSVGTimeline = fpsvg.anim.TSVGTimeline;

  ESVGStyle = fpsvg.style.ESVGStyle;
  TSVGComputedStyle = fpsvg.style.TSVGComputedStyle;
  TSVGStyleResolver = fpsvg.style.TSVGStyleResolver;
  TSVGUseExpander = fpsvg.style.TSVGUseExpander;

  ESVGFontRead = fpsvg.svgfont.ESVGFontRead;
  TSVGFontFace = fpsvg.svgfont.TSVGFontFace;
  TSVGDocumentFont = fpsvg.svgfont.TSVGDocumentFont;
  TSVGDocumentFontProvider = fpsvg.svgfont.TSVGDocumentFontProvider;
  ESVGText = fpsvg.text.ESVGText;
  TSVGTextLayout = fpsvg.text.TSVGTextLayout;
  TSVGTextRun = fpsvg.text.TSVGTextRun;

  ESVGRender = fpsvg.render.ESVGRender;
  TSVGRenderState = fpsvg.render.TSVGRenderState;
  TSVGRenderer = fpsvg.render.TSVGRenderer;

implementation

end.
