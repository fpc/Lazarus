{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    SVG render backend writing a PDF document with fcl-pdf.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.pdf;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, FpPdf.Pdf, fpsvg.types, fpsvg.backend,
     fpsvg.geom;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, fppdf, fpsvg.types, fpsvg.backend, fpsvg.geom;
{$ENDIF FPC_DOTTEDUNITS}

type
  ESVGPDF = class(ESVGBackend);

  { One offscreen layer: the form it draws into, and how it is composited
    onto the layer below. }
  TSVGPDFLayer = record
    Form    : TPDFForm;
    Opacity : Double;
    Clips   : Integer;
    BBox    : TPDFDimensions;
  end;
  TSVGPDFLayerArray = array of TSVGPDFLayer;

  { Draws into a PDF document. Every frame becomes a page of its own, of
    the size the frame was started with.
    Paths, clips, gradients, layers and masks become what PDF has for
    them; text becomes the outlines of its glyphs; filters are not
    applied, and the drawing they would change is written unfiltered. }
  TSVGPDFBackend = class(TSVGRenderBackend)
  private
    FDocument: TPDFDocument;
    FOwnsDocument: Boolean;
    FSection: TPDFSection;
    FPage: TPDFPage;
    FStarted: Boolean;
    FInFrame: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FScale: Double;
    FFlatness: Double;
    FBase: TSVGMatrix;
    FLayers: TSVGPDFLayerArray;
    FLayerCount: Integer;
    FClips: Integer;
    FPath: TSVGPath;
    FPoly: TSVGPolyPath;
    FStroked: TSVGPolyPath;
    FImageSources: array of ISVGImageSource;
    FImageNumbers: array of Integer;
    FPatternKeys: TStringList;
    function GetDocument: TPDFDocument;
    function GetTarget: TPDFPage;
    function BBoxOf(const aBounds: TSVGRect): TPDFDimensions;
    procedure NeedFrame(const aWhat: String);
    procedure EmitPath(aPath: TSVGPath);
    procedure EmitPolygons(aPoly: TSVGPolyPath);
    procedure EmitMatrix(const aCTM: TSVGMatrix);
    procedure EmitPen(const aPen: TSVGPen);
    function AlphaState(aFillAlpha, aStrokeAlpha: Double): Integer;
    function PaintState(const aPaint: TSVGPaint; const aCTM: TSVGMatrix;
      const aBounds: TSVGRect; aOpacity: Double; aStroke: Boolean;
      out aPattern: Integer): Integer;
    function PatternFor(const aGradient: TSVGGradient;
      const aCTM: TSVGMatrix; const aBounds: TSVGRect; aAlpha: Boolean;
      const aKey: TSVGString): Integer;
    function AlphaMaskFor(const aGradient: TSVGGradient;
      const aCTM: TSVGMatrix; const aBounds: TSVGRect;
      const aKey: TSVGString): Integer;
    function GradientOf(const aPaint: TSVGPaint;
      out aGradient: TSVGGradient): Boolean;
    function ImageNumber(aImage: ISVGImageSource): Integer;
    function PathBounds(aPath: TSVGPath): TSVGRect;
    procedure SetScale(const aValue: Double);
  public
    constructor Create; override;
    destructor Destroy; override;
    // The name the backend registers under.
    class function BackendName: String; override;
    // What the backend does itself; the renderer emulates the rest.
    class function Capabilities: TSVGBackendCapabilities; override;

    // Writes the document to a file.
    procedure SaveToFile(const aFileName: String);
    // Writes the document to a stream.
    procedure SaveToStream(aStream: TStream);
    // Draws into a document of the caller instead of one of its own. The
    // document is not started, saved or freed by the backend.
    procedure SetTarget(aDocument: TPDFDocument);

    procedure BeginFrame(aWidth, aHeight: Integer); override;
    procedure EndFrame; override;
    procedure FillPath(aPath: TSVGPath; const aCTM: TSVGMatrix;
      const aPaint: TSVGPaint; aRule: TSVGFillRule; aOpacity: Double); override;
    procedure StrokePath(aPath: TSVGPath; const aCTM: TSVGMatrix;
      const aPaint: TSVGPaint; const aPen: TSVGPen;
      aOpacity: Double); override;
    procedure PushClip(aPath: TSVGPath; const aCTM: TSVGMatrix;
      aRule: TSVGFillRule); override;
    procedure PushStrokeClip(aPath: TSVGPath; const aCTM: TSVGMatrix;
      const aPen: TSVGPen); override;
    procedure PopClip; override;
    procedure SetColorInterpolation(aSpace: TSVGColorInterpolation); override;
    procedure PushLayer(const aBounds: TSVGRect; aOpacity: Double;
      aIsolate: Boolean); override;
    procedure PopLayer; override;
    procedure PopLayerAsMask(aMode: TSVGMaskMode); override;
    procedure DrawGlyphRun(aFont: TSVGFontHandle;
      const aGlyphs: TSVGGlyphArray; const aCTM: TSVGMatrix;
      const aPaint: TSVGPaint; aOpacity: Double); override;
    procedure DrawImage(aImage: ISVGImageSource; const aRect: TSVGRect;
      const aCTM: TSVGMatrix; aOpacity: Double); override;

    // The document the backend writes into, created on first use.
    property Document: TPDFDocument read GetDocument;
    // The page of the frame being drawn, nil outside a frame.
    property Page: TPDFPage read FPage;
    // PDF points per SVG user unit. One by default, which makes a
    // document of 100 by 100 user units a page of 100 by 100 points.
    property Scale: Double read FScale write SetScale;
    // How far a curve of a stroke outline may lie from the curve itself,
    // in device units.
    property Flatness: Double read FFlatness write FFlatness;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.Math, fpsvg.strings;
{$ELSE FPC_DOTTEDUNITS}
uses math, fpsvg.strings;
{$ENDIF FPC_DOTTEDUNITS}

const
  DefaultFlatness = 0.25;
  // How many times a gradient that repeats or reflects is written out on
  // each side of its axis.
  MaxGradientPeriods = 32;

resourcestring
  SErrPDFNoFrame = 'The PDF backend has no frame to %s in.';
  SErrPDFNoLayer = 'The PDF backend has no layer to %s.';
  SErrPDFNoClip = 'The PDF backend has no clip to pop.';
  SErrPDFMaskWithoutParent = 'A mask needs a layer under it.';

// The colour as PDF writes it, without its alpha.
function PDFColorOf(const aColor: TSVGColor): TARGBColor;

begin
  Result := (TARGBColor(aColor.Red shr 8) shl 16)
         or (TARGBColor(aColor.Green shr 8) shl 8)
         or TARGBColor(aColor.Blue shr 8);
end;


// The matrix as fcl-pdf takes it.
function PDFMatrixOf(const aMatrix: TSVGMatrix): TPDFTransformMatrix;

begin
  Result.A := aMatrix.a;
  Result.B := aMatrix.b;
  Result.C := aMatrix.c;
  Result.D := aMatrix.d;
  Result.E := aMatrix.e;
  Result.F := aMatrix.f;
end;


// The six numbers of a matrix, to tell two of them apart.
function MatrixKey(const aMatrix: TSVGMatrix): TSVGString;

begin
  Result := Format('%.4f;%.4f;%.4f;%.4f;%.4f;%.4f',
    [aMatrix.a, aMatrix.b, aMatrix.c, aMatrix.d, aMatrix.e, aMatrix.f]);
end;


{ TSVGPDFBackend }

constructor TSVGPDFBackend.Create;

begin
  inherited Create;
  FScale := 1;
  FFlatness := DefaultFlatness;
  FPath := TSVGPath.Create;
  FPoly := TSVGPolyPath.Create;
  FStroked := TSVGPolyPath.Create;
  FPatternKeys := TStringList.Create;
  FPatternKeys.Sorted := True;
  FBase := TSVGMatrix.Identity;
end;


destructor TSVGPDFBackend.Destroy;

begin
  FPatternKeys.Free;
  FStroked.Free;
  FPoly.Free;
  FPath.Free;
  if FOwnsDocument then
    FDocument.Free;
  inherited Destroy;
end;


class function TSVGPDFBackend.BackendName: String;

begin
  Result := 'pdf';
end;


class function TSVGPDFBackend.Capabilities: TSVGBackendCapabilities;

begin
  Result := [bcClipPath, bcMask, bcGroupOpacity, bcDashes];
end;


function TSVGPDFBackend.GetDocument: TPDFDocument;

begin
  if FDocument = nil then
    begin
    FDocument := TPDFDocument.Create(nil);
    FOwnsDocument := True;
    FDocument.Infos.Producer := 'fcl-svg';
    FDocument.Options := FDocument.Options + [poCompressText]
      - [poPageOriginAtTop];
    FDocument.DefaultUnitOfMeasure := uomPixels;
    end;
  Result := FDocument;
end;


// Draws into a document of the caller. It replaces the one the backend
// made itself, if it made one.
procedure TSVGPDFBackend.SetTarget(aDocument: TPDFDocument);

begin
  if FInFrame then
    raise ESVGPDF.CreateFmt(SErrPDFNoFrame, ['change the document']);
  if FOwnsDocument then
    FreeAndNil(FDocument);
  FDocument := aDocument;
  FOwnsDocument := False;
  FStarted := aDocument <> nil;
  FSection := nil;
end;


procedure TSVGPDFBackend.SetScale(const aValue: Double);

begin
  if aValue > 0 then
    FScale := aValue;
end;


procedure TSVGPDFBackend.NeedFrame(const aWhat: String);

begin
  if not FInFrame then
    raise ESVGPDF.CreateFmt(SErrPDFNoFrame, [aWhat]);
end;


// The page or the form that is being drawn into.
function TSVGPDFBackend.GetTarget: TPDFPage;

begin
  if FLayerCount > 0 then
    Result := FLayers[FLayerCount - 1].Form
  else
    Result := FPage;
end;


procedure TSVGPDFBackend.BeginFrame(aWidth, aHeight: Integer);

var
  lPaper: TPDFPaper;

begin
  if FInFrame then
    EndFrame;
  if aWidth <= 0 then
    aWidth := 1;
  if aHeight <= 0 then
    aHeight := 1;
  FWidth := aWidth;
  FHeight := aHeight;
  GetDocument;
  if not FStarted then
    begin
    FDocument.StartDocument;
    FStarted := True;
    end;
  if FSection = nil then
    FSection := FDocument.Sections.AddSection;
  FPage := FDocument.Pages.AddPage;
  lPaper.W := aWidth * FScale;
  lPaper.H := aHeight * FScale;
  lPaper.Printable.L := 0;
  lPaper.Printable.T := 0;
  lPaper.Printable.R := lPaper.W;
  lPaper.Printable.B := lPaper.H;
  FPage.Paper := lPaper;
  FPage.UnitOfMeasure := uomPixels;
  FSection.AddPage(FPage);
  // A page has its origin at the bottom left and y running up, a document
  // has it at the top left and y running down.
  FBase := TSVGMatrix.Create(FScale, 0, 0, -FScale, 0, aHeight * FScale);
  FLayerCount := 0;
  FClips := 0;
  FPatternKeys.Clear;
  SetLength(FImageSources, 0);
  SetLength(FImageNumbers, 0);
  FInFrame := True;
end;


procedure TSVGPDFBackend.EndFrame;

begin
  if not FInFrame then
    Exit;
  while FLayerCount > 0 do
    PopLayer;
  while FClips > 0 do
    PopClip;
  FInFrame := False;
end;


procedure TSVGPDFBackend.SaveToFile(const aFileName: String);

begin
  if FInFrame then
    EndFrame;
  Document.SaveToFile(aFileName);
end;


procedure TSVGPDFBackend.SaveToStream(aStream: TStream);

begin
  if FInFrame then
    EndFrame;
  Document.SaveToStream(aStream);
end;


// The box of a rectangle of the document, in the coordinates of the page.
// An empty rectangle gives the whole page.
function TSVGPDFBackend.BBoxOf(const aBounds: TSVGRect): TPDFDimensions;

var
  lRect: TSVGRect;

begin
  if aBounds.IsEmpty then
    begin
    Result.L := 0;
    Result.B := 0;
    Result.R := FWidth * FScale;
    Result.T := FHeight * FScale;
    Exit;
    end;
  lRect := aBounds.Transform(FBase);
  Result.L := lRect.Left;
  Result.B := lRect.Top;
  Result.R := lRect.Right;
  Result.T := lRect.Bottom;
  if Result.T < Result.B then
    begin
    Result.B := lRect.Bottom;
    Result.T := lRect.Top;
    end;
end;


// Writes the path in the coordinates it was built in. The caller has put
// the matrix that places them in effect.
procedure TSVGPDFBackend.EmitPath(aPath: TSVGPath);

var
  I: Integer;
  lSegment: TSVGPathSegment;
  lTarget: TPDFPage;

begin
  lTarget := GetTarget;
  for I := 0 to aPath.SegmentCount - 1 do
    begin
    lSegment := aPath[I];
    case lSegment.Kind of
      skMoveTo:
        lTarget.MoveTo(lSegment.Points[0].X, lSegment.Points[0].Y);
      skLineTo:
        lTarget.LineTo(lSegment.Points[0].X, lSegment.Points[0].Y);
      skCubicTo:
        lTarget.CubicCurveTo(lSegment.Points[0].X, lSegment.Points[0].Y,
          lSegment.Points[1].X, lSegment.Points[1].Y,
          lSegment.Points[2].X, lSegment.Points[2].Y, 0, False);
      skClose:
        lTarget.ClosePath;
    end;
    end;
end;


// Writes the subpaths of a flattened path, each of them closed.
procedure TSVGPDFBackend.EmitPolygons(aPoly: TSVGPolyPath);

var
  I, J: Integer;
  lTarget: TPDFPage;

begin
  lTarget := GetTarget;
  for I := 0 to aPoly.SubPathCount - 1 do
    begin
    if aPoly.PointCount[I] < 2 then
      Continue;
    for J := 0 to aPoly.PointCount[I] - 1 do
      if J = 0 then
        lTarget.MoveTo(aPoly.Points[I, J].X, aPoly.Points[I, J].Y)
      else
        lTarget.LineTo(aPoly.Points[I, J].X, aPoly.Points[I, J].Y);
    lTarget.ClosePath;
    end;
end;


// Puts the matrix of a drawing in effect: the one the renderer gave,
// followed by the one that maps the document onto the page.
procedure TSVGPDFBackend.EmitMatrix(const aCTM: TSVGMatrix);

begin
  GetTarget.ConcatMatrix(PDFMatrixOf(aCTM.Compose(FBase)));
end;


// Sets the width, the ends and the dashes of a pen.
procedure TSVGPDFBackend.EmitPen(const aPen: TSVGPen);

var
  lTarget: TPDFPage;
  lDashes: TDashArray;
  I: Integer;

begin
  lTarget := GetTarget;
  lTarget.SetLineWidth(aPen.Width);
  case aPen.Cap of
    lcRound: lTarget.SetLineCapStyle(plcsRoundCap);
    lcSquare: lTarget.SetLineCapStyle(plcsProjectingSquareCap);
  else
    lTarget.SetLineCapStyle(plcsButtCap);
  end;
  case aPen.Join of
    ljRound: lTarget.SetLineJoinStyle(pljsRoundJoin);
    ljBevel: lTarget.SetLineJoinStyle(pljsBevelJoin);
  else
    lTarget.SetLineJoinStyle(pljsMiterJoin);
  end;
  if aPen.MiterLimit > 0 then
    lTarget.SetMiterLimit(aPen.MiterLimit);
  SetLength(lDashes, Length(aPen.Dashes));
  for I := 0 to Length(aPen.Dashes) - 1 do
    lDashes[I] := aPen.Dashes[I];
  lTarget.SetDashPattern(lDashes, aPen.DashOffset);
end;


// The graphics state holding these two alphas. A negative one is left as
// it is.
function TSVGPDFBackend.AlphaState(aFillAlpha, aStrokeAlpha: Double): Integer;

begin
  Result := Document.AddGraphicsState(aFillAlpha, aStrokeAlpha);
end;


// The bounding box of a path, tight enough for the box a gradient in
// object bounding box units is resolved against.
function TSVGPDFBackend.PathBounds(aPath: TSVGPath): TSVGRect;

begin
  FPoly.Flatten(aPath, TSVGMatrix.Identity, FFlatness);
  Result := FPoly.Bounds;
end;


// The gradient of a paint, if it has one to paint with.
function TSVGPDFBackend.GradientOf(const aPaint: TSVGPaint;
  out aGradient: TSVGGradient): Boolean;

begin
  Result := (aPaint.Kind = spServer) and (aPaint.Server <> nil)
        and (aPaint.Server.GetPaintServerKind in
             [pkLinearGradient, pkRadialGradient])
        and aPaint.Server.GetGradient(aGradient)
        and aGradient.HasStops;
end;


// The stops of a gradient, repeated over the periods the spread method
// asks for. aFrom and aTo return the range of periods written.
procedure GradientStops(const aGradient: TSVGGradient; aPeriods: Integer;
  aAlpha: Boolean; out aStops: array of TPDFShadingStop;
  out aCount: Integer; out aFrom, aTo: Integer);

var
  I, J, lIndex: Integer;
  lColor: TSVGColor;
  lOffset: Double;
  lSpan: Double;
  lLevel: Byte;

begin
  if aGradient.Spread = smPad then
    begin
    aFrom := 0;
    aTo := 1;
    end
  else
    begin
    aFrom := -aPeriods;
    aTo := aPeriods + 1;
    end;
  lSpan := aTo - aFrom;
  aCount := 0;
  for I := aFrom to aTo - 1 do
    for J := 0 to Length(aGradient.Stops) - 1 do
      begin
      // A gradient that reflects runs backwards over every other period.
      if (aGradient.Spread = smReflect) and Odd(Abs(I) mod 2) then
        lIndex := Length(aGradient.Stops) - 1 - J
      else
        lIndex := J;
      lColor := aGradient.Stops[lIndex].EffectiveColor;
      lOffset := aGradient.Stops[lIndex].Offset;
      if (aGradient.Spread = smReflect) and Odd(Abs(I) mod 2) then
        lOffset := 1 - lOffset;
      if aCount > High(aStops) then
        Exit;
      aStops[aCount].Offset := (I - aFrom + lOffset) / lSpan;
      if aAlpha then
        begin
        lLevel := lColor.Alpha shr 8;
        aStops[aCount].Color := (TARGBColor(lLevel) shl 16)
          or (TARGBColor(lLevel) shl 8) or TARGBColor(lLevel);
        end
      else
        aStops[aCount].Color := PDFColorOf(lColor);
      Inc(aCount);
      end;
end;


// The pattern painting a gradient, in the space the paint is used in.
// aAlpha writes the opacity of the stops in grey instead of their colour.
function TSVGPDFBackend.PatternFor(const aGradient: TSVGGradient;
  const aCTM: TSVGMatrix; const aBounds: TSVGRect; aAlpha: Boolean;
  const aKey: TSVGString): Integer;

var
  lSpace: TSVGMatrix;
  lStops: array[0..1023] of TPDFShadingStop;
  lCount, lFrom, lTo, lShading, lIndex, lPeriods: Integer;
  lStart, lStop, lCentre, lFocus: TSVGPoint;
  lRadius: Double;
  lFullKey: TSVGString;

begin
  lFullKey := aKey;
  lIndex := FPatternKeys.IndexOf(lFullKey);
  if lIndex >= 0 then
    Exit(PtrInt(FPatternKeys.Objects[lIndex]));
  // A gradient of the unit square is placed on the box of the shape, and
  // its own transform runs before both.
  lSpace := aGradient.Transform;
  if aGradient.Units = guObjectBoundingBox then
    lSpace := lSpace.Compose(TSVGGradient.BoxTransform(aBounds));
  lSpace := lSpace.Compose(aCTM).Compose(FBase);
  lPeriods := MaxGradientPeriods;
  GradientStops(aGradient, lPeriods, aAlpha, lStops, lCount, lFrom, lTo);
  if lCount = 0 then
    Exit(-1);
  if aGradient.Kind = pkLinearGradient then
    begin
    lStart.X := aGradient.First.X + lFrom * (aGradient.Second.X - aGradient.First.X);
    lStart.Y := aGradient.First.Y + lFrom * (aGradient.Second.Y - aGradient.First.Y);
    lStop.X := aGradient.First.X + lTo * (aGradient.Second.X - aGradient.First.X);
    lStop.Y := aGradient.First.Y + lTo * (aGradient.Second.Y - aGradient.First.Y);
    lShading := Document.AddAxialShading(lStart.X, lStart.Y, lStop.X, lStop.Y,
      Slice(lStops, lCount), True, True, aAlpha);
    end
  else
    begin
    lCentre := aGradient.Second;
    lFocus := aGradient.Focus;
    lRadius := aGradient.Radius * (lTo);
    if lRadius <= 0 then
      Exit(-1);
    lShading := Document.AddRadialShading(lFocus.X, lFocus.Y, 0,
      lCentre.X, lCentre.Y, lRadius, Slice(lStops, lCount), True, True, aAlpha);
    end;
  Result := Document.AddShadingPattern(lShading, PDFMatrixOf(lSpace));
  FPatternKeys.AddObject(lFullKey, TObject(PtrInt(Result)));
end;


// The graphics state whose soft mask holds the opacity of the stops of a
// gradient, or -1 when every stop is opaque.
function TSVGPDFBackend.AlphaMaskFor(const aGradient: TSVGGradient;
  const aCTM: TSVGMatrix; const aBounds: TSVGRect;
  const aKey: TSVGString): Integer;

var
  I: Integer;
  lNeeded: Boolean;
  lPattern: Integer;
  lForm: TPDFForm;
  lBox: TPDFDimensions;

begin
  Result := -1;
  lNeeded := False;
  for I := 0 to Length(aGradient.Stops) - 1 do
    if aGradient.Stops[I].EffectiveColor.Alpha < $FFFF then
      lNeeded := True;
  if not lNeeded then
    Exit;
  lPattern := PatternFor(aGradient, aCTM, aBounds, True, aKey + ';alpha');
  if lPattern < 0 then
    Exit;
  lBox := BBoxOf(TSVGRect.Empty);
  lForm := Document.AddForm(lBox);
  lForm.SetPatternFill(lPattern);
  lForm.MoveTo(lBox.L, lBox.B);
  lForm.LineTo(lBox.R, lBox.B);
  lForm.LineTo(lBox.R, lBox.T);
  lForm.LineTo(lBox.L, lBox.T);
  lForm.ClosePath;
  lForm.FillPath;
  Result := Document.AddGraphicsState(-1, -1, pbmNormal, psmLuminosity,
    lForm.Index);
end;


{ The state to draw a paint with, and the pattern it fills with.
  aPattern returns -1 for a paint that is a plain colour, which the caller
  then sets itself. }
function TSVGPDFBackend.PaintState(const aPaint: TSVGPaint;
  const aCTM: TSVGMatrix; const aBounds: TSVGRect; aOpacity: Double;
  aStroke: Boolean; out aPattern: Integer): Integer;

var
  lGradient: TSVGGradient;
  lKey: TSVGString;
  lAlpha, lFill, lStroke: Double;

begin
  aPattern := -1;
  Result := -1;
  lAlpha := aOpacity;
  if lAlpha < 0 then
    lAlpha := 0
  else if lAlpha > 1 then
    lAlpha := 1;
  if GradientOf(aPaint, lGradient) then
    begin
    lKey := aPaint.Server.GetPaintServerID + ';' + MatrixKey(aCTM);
    if lGradient.Units = guObjectBoundingBox then
      lKey := lKey + Format(';%.3f;%.3f;%.3f;%.3f',
        [aBounds.Left, aBounds.Top, aBounds.Right, aBounds.Bottom]);
    aPattern := PatternFor(lGradient, aCTM, aBounds, False, lKey);
    Result := AlphaMaskFor(lGradient, aCTM, aBounds, lKey);
    if Result >= 0 then
      begin
      // The mask holds the opacity of the stops; the opacity of the
      // element multiplies it.
      if aStroke then
        Document.GraphicsStates[Result].StrokeAlpha := lAlpha
      else
        Document.GraphicsStates[Result].FillAlpha := lAlpha;
      Exit;
      end;
    end
  else
    lAlpha := lAlpha * (aPaint.Color.Alpha / $FFFF);
  if lAlpha >= 1 then
    Exit;
  if aStroke then
    begin
    lFill := -1;
    lStroke := lAlpha;
    end
  else
    begin
    lFill := lAlpha;
    lStroke := -1;
    end;
  Result := AlphaState(lFill, lStroke);
end;


procedure TSVGPDFBackend.FillPath(aPath: TSVGPath; const aCTM: TSVGMatrix;
  const aPaint: TSVGPaint; aRule: TSVGFillRule; aOpacity: Double);

var
  lTarget: TPDFPage;
  lState, lPattern: Integer;

begin
  NeedFrame('fill');
  if (aPath = nil) or aPath.IsEmpty or (aPaint.Kind = spNone) then
    Exit;
  lState := PaintState(aPaint, aCTM, PathBounds(aPath), aOpacity, False,
    lPattern);
  lTarget := GetTarget;
  lTarget.PushGraphicsStack;
  try
    if lState >= 0 then
      lTarget.SetGraphicsState(lState);
    EmitMatrix(aCTM);
    if lPattern >= 0 then
      lTarget.SetPatternFill(lPattern)
    else
      lTarget.SetColor(PDFColorOf(aPaint.Color), False);
    EmitPath(aPath);
    if aRule = frEvenOdd then
      lTarget.FillEvenOddPath
    else
      lTarget.FillPath;
  finally
    lTarget.PopGraphicsStack;
  end;
end;


procedure TSVGPDFBackend.StrokePath(aPath: TSVGPath; const aCTM: TSVGMatrix;
  const aPaint: TSVGPaint; const aPen: TSVGPen; aOpacity: Double);

var
  lTarget: TPDFPage;
  lState, lPattern: Integer;

begin
  NeedFrame('stroke');
  if (aPath = nil) or aPath.IsEmpty or (aPaint.Kind = spNone)
     or (aPen.Width <= 0) then
    Exit;
  lState := PaintState(aPaint, aCTM, PathBounds(aPath), aOpacity, True,
    lPattern);
  lTarget := GetTarget;
  lTarget.PushGraphicsStack;
  try
    if lState >= 0 then
      lTarget.SetGraphicsState(lState);
    EmitMatrix(aCTM);
    if lPattern >= 0 then
      lTarget.SetPatternStroke(lPattern)
    else
      lTarget.SetColor(PDFColorOf(aPaint.Color), True);
    EmitPen(aPen);
    EmitPath(aPath);
    lTarget.StrokePath;
  finally
    lTarget.PopGraphicsStack;
  end;
end;


procedure TSVGPDFBackend.PushClip(aPath: TSVGPath; const aCTM: TSVGMatrix;
  aRule: TSVGFillRule);

var
  lTarget: TPDFPage;
  lInverse: TSVGMatrix;

begin
  NeedFrame('clip');
  lTarget := GetTarget;
  lTarget.PushGraphicsStack;
  if FLayerCount > 0 then
    Inc(FLayers[FLayerCount - 1].Clips)
  else
    Inc(FClips);
  if (aPath = nil) or aPath.IsEmpty
     or not aCTM.Compose(FBase).Invert(lInverse) then
    begin
    // A clip that covers nothing leaves nothing of what follows.
    lTarget.MoveTo(0, 0);
    lTarget.ClosePath;
    lTarget.ClipPath;
    Exit;
    end;
  EmitMatrix(aCTM);
  EmitPath(aPath);
  if aRule = frEvenOdd then
    lTarget.ClipPathEvenOdd
  else
    lTarget.ClipPath;
  // The clip stays; the matrix it was written in does not.
  lTarget.ConcatMatrix(PDFMatrixOf(lInverse));
end;


procedure TSVGPDFBackend.PushStrokeClip(aPath: TSVGPath;
  const aCTM: TSVGMatrix; const aPen: TSVGPen);

var
  lTarget: TPDFPage;
  lInverse: TSVGMatrix;
  lScale, lTolerance: Double;

begin
  NeedFrame('clip');
  lTarget := GetTarget;
  lTarget.PushGraphicsStack;
  if FLayerCount > 0 then
    Inc(FLayers[FLayerCount - 1].Clips)
  else
    Inc(FClips);
  lScale := aCTM.MaxScale;
  if lScale <= 0 then
    lScale := 1;
  lTolerance := FFlatness / lScale;
  if (aPath = nil) or aPath.IsEmpty or (aPen.Width <= 0)
     or not aCTM.Compose(FBase).Invert(lInverse) then
    begin
    lTarget.MoveTo(0, 0);
    lTarget.ClosePath;
    lTarget.ClipPath;
    Exit;
    end;
  // PDF clips to a path, so the outline the pen would paint is built here
  // and clipped to as a shape.
  FPoly.Flatten(aPath, TSVGMatrix.Identity, lTolerance);
  if aPen.IsDashed then
    begin
    FStroked.BuildDashes(FPoly, aPen.Dashes, aPen.DashOffset);
    FPoly.Assign(FStroked);
    end;
  FStroked.BuildStroke(FPoly, aPen, lTolerance);
  EmitMatrix(aCTM);
  EmitPolygons(FStroked);
  lTarget.ClipPath;
  lTarget.ConcatMatrix(PDFMatrixOf(lInverse));
end;


procedure TSVGPDFBackend.PopClip;

begin
  NeedFrame('pop a clip');
  if FLayerCount > 0 then
    begin
    if FLayers[FLayerCount - 1].Clips = 0 then
      raise ESVGPDF.Create(SErrPDFNoClip);
    Dec(FLayers[FLayerCount - 1].Clips);
    end
  else
    begin
    if FClips = 0 then
      raise ESVGPDF.Create(SErrPDFNoClip);
    Dec(FClips);
    end;
  GetTarget.PopGraphicsStack;
end;


// PDF composites in the colour space of the group, which is sRGB here.
procedure TSVGPDFBackend.SetColorInterpolation(aSpace: TSVGColorInterpolation);

begin
  if aSpace = ciSRGB then ;
end;


procedure TSVGPDFBackend.PushLayer(const aBounds: TSVGRect; aOpacity: Double;
  aIsolate: Boolean);

var
  lBox: TPDFDimensions;

begin
  NeedFrame('draw a layer');
  if FLayerCount = Length(FLayers) then
    SetLength(FLayers, Max(8, FLayerCount * 2));
  lBox := BBoxOf(aBounds);
  FLayers[FLayerCount].Form := Document.AddForm(lBox);
  FLayers[FLayerCount].Form.Isolated := aIsolate;
  FLayers[FLayerCount].Opacity := aOpacity;
  FLayers[FLayerCount].Clips := 0;
  FLayers[FLayerCount].BBox := lBox;
  Inc(FLayerCount);
end;


procedure TSVGPDFBackend.PopLayer;

var
  lLayer: TSVGPDFLayer;
  lTarget: TPDFPage;
  lState: Integer;
  lAlpha: Double;

begin
  NeedFrame('pop a layer');
  if FLayerCount = 0 then
    raise ESVGPDF.CreateFmt(SErrPDFNoLayer, ['pop']);
  Dec(FLayerCount);
  lLayer := FLayers[FLayerCount];
  while lLayer.Clips > 0 do
    begin
    lLayer.Form.PopGraphicsStack;
    Dec(lLayer.Clips);
    end;
  lTarget := GetTarget;
  lAlpha := lLayer.Opacity;
  if lAlpha < 0 then
    lAlpha := 0
  else if lAlpha > 1 then
    lAlpha := 1;
  lTarget.PushGraphicsStack;
  try
    if lAlpha < 1 then
      begin
      lState := AlphaState(lAlpha, lAlpha);
      lTarget.SetGraphicsState(lState);
      end;
    lTarget.DrawForm(lLayer.Form.Index);
  finally
    lTarget.PopGraphicsStack;
  end;
end;


{ The layer holds the mask; what the layer under it has drawn so far is
  wrapped into a form of its own and drawn again through the mask, so that
  what is drawn after this stays unmasked. }
procedure TSVGPDFBackend.PopLayerAsMask(aMode: TSVGMaskMode);

var
  lMask, lParent, lWrapper: TPDFForm;
  lKind: TPDFSoftMaskKind;
  lState: Integer;

begin
  NeedFrame('pop a layer');
  if FLayerCount = 0 then
    raise ESVGPDF.CreateFmt(SErrPDFNoLayer, ['use as a mask']);
  Dec(FLayerCount);
  lMask := FLayers[FLayerCount].Form;
  while FLayers[FLayerCount].Clips > 0 do
    begin
    lMask.PopGraphicsStack;
    Dec(FLayers[FLayerCount].Clips);
    end;
  if FLayerCount = 0 then
    raise ESVGPDF.Create(SErrPDFMaskWithoutParent);
  if aMode = mmAlpha then
    lKind := psmAlpha
  else
    lKind := psmLuminosity;
  lParent := FLayers[FLayerCount - 1].Form;
  lWrapper := Document.AddForm(FLayers[FLayerCount - 1].BBox);
  lWrapper.Isolated := lParent.Isolated;
  lState := Document.AddGraphicsState(-1, -1, pbmNormal, lKind, lMask.Index);
  lWrapper.PushGraphicsStack;
  lWrapper.SetGraphicsState(lState);
  lWrapper.DrawForm(lParent.Index);
  lWrapper.PopGraphicsStack;
  FLayers[FLayerCount - 1].Form := lWrapper;
end;


procedure TSVGPDFBackend.DrawGlyphRun(aFont: TSVGFontHandle;
  const aGlyphs: TSVGGlyphArray; const aCTM: TSVGMatrix;
  const aPaint: TSVGPaint; aOpacity: Double);

begin
  NeedFrame('draw text');
  if (aFont = nil) or (Length(aGlyphs) = 0) then
    Exit;
  // The run becomes one path, which fills as a single shape.
  FPath.Clear;
  SVGAppendGlyphRun(aFont, aGlyphs, FPath);
  if FPath.IsEmpty then
    Exit;
  FillPath(FPath, aCTM, aPaint, frNonZero, aOpacity);
end;


// The number the document draws an image by, adding it on first use.
function TSVGPDFBackend.ImageNumber(aImage: ISVGImageSource): Integer;

var
  I, X, Y, lWidth, lHeight, lPos: Integer;
  lRGB, lAlpha: TBytes;
  lRow: array of TSVGColor;
  lColor: TSVGColor;
  lOpaque: Boolean;

begin
  for I := 0 to Length(FImageSources) - 1 do
    if FImageSources[I] = aImage then
      Exit(FImageNumbers[I]);
  lWidth := aImage.GetWidth;
  lHeight := aImage.GetHeight;
  if (lWidth <= 0) or (lHeight <= 0) then
    Exit(-1);
  SetLength(lRGB, lWidth * lHeight * 3);
  SetLength(lAlpha, lWidth * lHeight);
  SetLength(lRow, lWidth);
  lOpaque := True;
  for Y := 0 to lHeight - 1 do
    begin
    if not aImage.GetRow(Y, 0, lWidth, @lRow[0]) then
      for X := 0 to lWidth - 1 do
        lRow[X] := aImage.GetPixel(X, Y);
    for X := 0 to lWidth - 1 do
      begin
      lColor := lRow[X];
      lPos := (Y * lWidth + X);
      lRGB[lPos * 3] := lColor.Red shr 8;
      lRGB[lPos * 3 + 1] := lColor.Green shr 8;
      lRGB[lPos * 3 + 2] := lColor.Blue shr 8;
      lAlpha[lPos] := lColor.Alpha shr 8;
      if lColor.Alpha < $FFFF then
        lOpaque := False;
      end;
    end;
  if lOpaque then
    SetLength(lAlpha, 0);
  Result := Document.Images.AddRawImage(lWidth, lHeight, lRGB, lAlpha);
  I := Length(FImageSources);
  SetLength(FImageSources, I + 1);
  SetLength(FImageNumbers, I + 1);
  FImageSources[I] := aImage;
  FImageNumbers[I] := Result;
end;


procedure TSVGPDFBackend.DrawImage(aImage: ISVGImageSource;
  const aRect: TSVGRect; const aCTM: TSVGMatrix; aOpacity: Double);

var
  lTarget: TPDFPage;
  lNumber, lState: Integer;
  lPlace: TSVGMatrix;
  lAlpha: Double;

begin
  NeedFrame('draw an image');
  if (aImage = nil) or aRect.IsEmpty then
    Exit;
  lNumber := ImageNumber(aImage);
  if lNumber < 0 then
    Exit;
  lAlpha := aOpacity;
  if lAlpha > 1 then
    lAlpha := 1;
  if lAlpha <= 0 then
    Exit;
  lTarget := GetTarget;
  lTarget.PushGraphicsStack;
  try
    if lAlpha < 1 then
      begin
      lState := AlphaState(lAlpha, lAlpha);
      lTarget.SetGraphicsState(lState);
      end;
    EmitMatrix(aCTM);
    // The image fills the unit square, its first row at the top, which is
    // the smaller y of the rectangle.
    lPlace := TSVGMatrix.Create(aRect.Right - aRect.Left, 0,
      0, aRect.Top - aRect.Bottom, aRect.Left, aRect.Bottom);
    lTarget.ConcatMatrix(PDFMatrixOf(lPlace));
    lTarget.DrawImageXObject(lNumber);
  finally
    lTarget.PopGraphicsStack;
  end;
end;


initialization
  SVGBackends.RegisterBackend(TSVGPDFBackend);
end.
