{
Writes an SVG Document

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho
}
unit svgvectorialwriter;

{$mode objfpc}{$H+}

{.$define FPVECTORIAL_SVGWRITER_TEXT_OFFSET}

interface

uses
  Classes, SysUtils, math, fpvectorial, fpvutils, fpcanvas;

type
  { TvSVGVectorialWriter }

  TvSVGVectorialWriter = class(TvCustomVectorialWriter)
  private
    FPointSeparator, FCommaSeparator: TFormatSettings;
    FLayerIndex: Integer;
    FPathIndex: Integer;
    FGradientIndex: Integer;

    // helper routines
    procedure ConvertFPVCoordinatesToSVGCoordinates(APage: TvVectorialPage;
      const ASrcX, ASrcY: Double; var ADestX, ADestY: double);
    procedure ConvertFPVSizeToSVGSize(const ASrcX, ASrcY: Double;
      var ADestX, ADestY: double);
    function FloatToSVGStr(x: Double): String;
    function GetBrushAsXMLStyle(ABrush: TvBrush): String;
    function GetGradientBrushAsXML(ABrush: TvBrush): String;
    function GetPenAsXMLStyle(APen: TvPen): String;
    procedure PrepareGradients(AStrings: TStrings; ADoc: TvVectorialDocument;
      APage: TvVectorialPage);

    procedure WriteDocumentSize(AStrings: TStrings; AData: TvVectorialDocument);
    procedure WriteDocumentName(AStrings: TStrings; AData: TvVectorialDocument);

    // Writing of svg entities
    procedure WriteCircle(AStrings: TStrings; ADoc: TvVectorialDocument;
      APage: TvVectorialPage; ACircle: TvCircle);
    procedure WriteEllipse(AStrings: TStrings; ADoc: TvVectorialDocument;
      APage: TvVectorialPage; AEllipse: TvEllipse);
    procedure WriteEntities(AStrings: TStrings; ADoc: TvVectorialDocument;
      APage: TvVectorialPage);
    procedure WriteEntity(AStrings: TStrings; ADoc: TvVectorialDocument;
      APage: TvVectorialPage; AEntity: TvEntity);
    procedure WriteLayer(AStrings: TStrings; ADoc: TvVectorialDocument;
      APage: TvVectorialPage; ALayer: Tvlayer);
    procedure WritePath(AStrings: TStrings; ADoc: TvVectorialDocument;
      APage: TvVectorialPage; APath: TPath);
    procedure WritePolygon(AStrings: TStrings;ADoc: TvVectorialDocument;
      APage: TvVectorialPage; APolygon: TvPolygon);
    procedure WriteRectangle(AStrings: TStrings; ADoc: TvVectorialDocument;
      APage: TvVectorialPage; ARectangle: TvRectangle);
    procedure WriteText(AStrings: TStrings; ADoc: TvVectorialDocument;
      APage: TvVectorialPage; AText: TvText);
    {
    procedure WriteLayer(layer: TvLayer; AStrings: TStrings; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure WritePath(AIndex: Integer; APath: TPath; AStrings: TStrings; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure WriteText(AStrings: TStrings; lText: TvText; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure WriteCircle(circle: TvCircle; AStrings: TStrings; AData: TvVectorialPage);
    procedure WriteEntities(AStrings: TStrings; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    }
  public
    { General reading methods }
    procedure WriteToStrings(AStrings: TStrings; AData: TvVectorialDocument); override;
  end;

implementation

const
  // SVG requires hardcoding a DPI value

  // The Opera Browser and Inkscape use 90 DPI, so we follow that

  // 1 Inch = 25.4 milimiters
  // 90 inches per pixel = (1 / 90) * 25.4 = 0.2822
  // FLOAT_MILLIMETERS_PER_PIXEL = 0.3528; // DPI 72 = 1 / 72 inches per pixel

  FLOAT_MILLIMETERS_PER_PIXEL = 0.2822; // DPI 90 = 1 / 90 inches per pixel
  FLOAT_PIXELS_PER_MILLIMETER = 3.5433; // DPI 90 = 1 / 90 inches per pixel


{ TvSVGVectorialWriter }

procedure TvSVGVectorialWriter.ConvertFPVCoordinatesToSVGCoordinates(
  APage: TvVectorialPage;
  const ASrcX, ASrcY: Double; var ADestX, ADestY: double);
begin
  ADestX := ASrcX * FLOAT_PIXELS_PER_MILLIMETER;
  ADestY := (APage.Height - ASrcY) * FLOAT_PIXELS_PER_MILLIMETER;
end;

procedure TvSVGVectorialWriter.ConvertFPVSizeToSVGSize(
  const ASrcX, ASrcY: Double; var ADestX, ADestY: double);
begin
  ADestX := ASrcX * FLOAT_PIXELS_PER_MILLIMETER;
  ADestY := ASrcY * FLOAT_PIXELS_PER_MILLIMETER;
end;

function TvSVGVectorialWriter.FloatToSVGStr(x: Double): String;
begin
  Result := FloatToStr(x, FPointSeparator);
end;

function TvSVGVectorialWriter.GetBrushAsXMLStyle(ABrush: TvBrush): String;
var
  fillStr: String;
begin
  if ABrush.Kind = bkSimpleBrush then begin
    if ABrush.Style = bsClear then
      fillStr := 'none'
    else
      fillStr := '#' + FPColorToRGBHexString(ABrush.Color)
  end else begin
    inc(FGradientIndex);
    fillStr := Format('url(#gradient%d)', [FGradientIndex]);
  end;

  Result := Format('fill:%s;', [fillStr]);
end;

function TvSVGVectorialWriter.GetGradientBrushAsXML(ABrush: TvBrush): String;
var
  gradientCol: TvGradientColor;
  colorStr, gradientColors, gradientParams: String;
  x1Str, y1Str, x2Str, y2Str: String;
  cxstr, cystr, rstr, fxstr, fystr: String;
  gradientTag: String;
begin
  if ABrush.Kind = bkRadialGradient then begin
    gradientTag := 'radialGradient';
    if ABrush.Gradient_cx_Unit = vcuPercentage then
      cxstr := Format('%f%%', [ABrush.Gradient_cx*100], FPointSeparator) else
      cxstr := FloatToSVGStr(ABrush.Gradient_cx);
    if ABrush.Gradient_cy_Unit = vcuPercentage then
      cystr := Format('%f%%', [ABrush.Gradient_cy*100], FPointSeparator) else
      cystr := FloatToSVGStr(ABrush.Gradient_cy);
    if ABrush.Gradient_r_Unit = vcuPercentage then
      rstr := Format('%f%%', [ABrush.Gradient_r*100], FPointSeparator) else
      rstr := FloatToSVGStr(ABrush.Gradient_r);
    if ABrush.Gradient_fx_Unit = vcuPercentage then
      fxstr := Format('%f%%', [ABrush.Gradient_fx*100], FPointSeparator) else
      fxstr := FloatToSVGStr(ABrush.Gradient_fx);
    if ABrush.Gradient_fy_Unit = vcuPercentage then
      fystr := Format('%f%%', [ABrush.Gradient_fy*100], FPointSeparator) else
      fystr := FloatToSVGStr(ABrush.Gradient_fy);
    gradientParams := Format('cx="%s" cy="%s" r="%s" fx="%s" fy="%s"',
      [cxstr, cystr, rstr, fxstr, fystr]);
  end else begin
    gradientTag := 'linearGradient';
    if gfRelStartX in ABrush.Gradient_flags then
      x1Str := Format('%f%%', [ABrush.Gradient_start.X*100], FPointSeparator) else
      x1Str := FloatToSVGStr(ABrush.Gradient_start.X);
    if gfRelEndX in ABrush.Gradient_flags then
      x2Str := Format('%f%%', [ABrush.Gradient_end.X*100], FPointSeparator) else
      x2Str := FloatToSVGStr(ABrush.Gradient_end.X);
    if gfRelStartY in ABrush.Gradient_flags then
      y1Str := Format('%f%%', [ABrush.Gradient_start.Y*100], FPointSeparator) else
      y1Str := FloatToSVGStr(ABrush.Gradient_start.Y);
    if gfRelEndY in ABrush.Gradient_flags then
      y2Str := Format('%f%%', [ABrush.Gradient_end.Y*100], FPointSeparator) else
      y2Str := FloatToSVGStr(ABrush.Gradient_end.Y);
    gradientParams := Format('x1="%s" y1="%s" x2="%s" y2="%s"',
      [x1Str, y1Str, x2Str, y2Str]);
  end;

  gradientColors := '';
  for gradientCol in ABrush.Gradient_colors do begin
    colorStr := '#' + FPColorToRGBHexString(gradientCol.Color);
    gradientColors := gradientColors + Format('<stop offset="%f%%" stop-color="%s" />', [
      gradientCol.Position*100, colorStr], FPointSeparator);
  end;

  Result := Format(
    '    <%s id="gradient%d" %s>%s</%s>', [
    gradientTag, FGradientIndex, gradientParams, gradientColors, gradientTag]);
end;

function TvSVGVectorialWriter.GetPenAsXMLStyle(APen: TvPen): String;
var
  colorStr: String;
  penWidth: Integer;
begin
  if APen.Style = psClear then
    colorStr := 'none' else
    colorStr := '#' + FPColorToRGBHexString(APen.Color);

  if APen.Width >= 1 then
    penWidth := APen.Width
  else
    penWidth := 1;

  Result := Format(
    'stroke:%s; stroke-width:%dpx; stroke-linecap:butt; stroke-linejoin:miter; stroke-opacity:1;', [
    colorStr, penwidth
  ]);

  case APen.Style of
    psDash       : Result := Result + 'stroke-dasharray: 9, 5;';
    psDot        : Result := Result + 'stroke-dasharray: 3, 5;';
    psDashDot    : Result := Result + 'stroke-dasharray: 9, 5, 3, 5;';
    psDashDotDot : Result := Result + 'stroke-dasharray: 9, 5, 3, 5, 3, 5;';
  end;
end;

{ Iterates through all entities of the page and creates a <defs> node containing
  all gradient definitions. Gradients are identified by a continuous number
  reset before processing. }
procedure TvSVGVectorialWriter.PrepareGradients(AStrings: TStrings;
  ADoc: TvVectorialDocument; APage: TvVectorialPage);

  procedure ProcessGradient(ABrush: TvBrush);
  var
    gradient: String;
  begin
    if FGradientIndex = 0 then
      AStrings.Add('  <defs>');
    inc(FGradientIndex);
    AStrings.Add(GetGradientBrushAsXML(ABrush));
  end;

  procedure ProcessEntity(AEntity: TvEntity);
  var
    entity: TvEntity;
    brush: TvBrush;
  begin
    if AEntity is TvEntityWithPenAndBrush then begin
      brush := TvEntityWithPenAndBrush(AEntity).Brush;
      if IsGradientBrush(brush) then
        ProcessGradient(TvEntityWithPenAndBrush(AEntity).Brush);
    end;
    if AEntity is TvLayer then begin
      entity := TvLayer(AEntity).GetFirstEntity;
      while entity <> nil do begin
        ProcessEntity(entity);
        entity := TvLayer(AEntity).GetNextEntity;
      end;
    end;
  end;

var
  entity: TvEntity;
  i: Integer;
begin
  FGradientIndex := 0;
  for i := 0 to APage.GetEntitiesCount() - 1 do
  begin
    entity := APage.GetEntity(i);
    ProcessEntity(entity);
  end;
  if FGradientIndex > 0 then
    AStrings.Add('  </defs>');
  FGradientIndex := 0;
end;

procedure TvSVGVectorialWriter.WriteDocumentSize(AStrings: TStrings; AData: TvVectorialDocument);
begin
  AStrings.Add('  width="' + FloatToSVGStr(AData.Width) + 'mm"');
  AStrings.Add('  height="' + FloatToSVGStr(AData.Height) + 'mm"');
end;

procedure TvSVGVectorialWriter.WriteDocumentName(AStrings: TStrings; AData: TvVectorialDocument);
begin
  AStrings.Add('  sodipodi:docname="New document 1">');
end;

procedure TvSVGVectorialWriter.WriteCircle(AStrings: TStrings;
  ADoc: TvVectorialDocument; APage: TvVectorialPage; ACircle: TvCircle);
var
  cx, cy, cr, dtmp: double;
  circleStr: string;
begin
  ConvertFPVCoordinatesToSVGCoordinates(APage, ACircle.X, ACircle.Y, cx, cy);
  ConvertFPVSizeToSVGSize(ACircle.Radius, 0, cr, dtmp);
  circleStr := Format('  <circle cx="%g" cy="%g" r="%g" style="%s %s">', [
    cx, cy, cr,
    GetPenAsXMLStyle(ACircle.Pen),
    GetBrushAsXMLStyle(ACircle.Brush)
    ], FPointSeparator);
  AStrings.Add(circleStr);
end;

// to do: "Angle" missing
procedure TvSVGVectorialWriter.WriteEllipse(AStrings: TStrings;
  ADoc: TvVectorialDocument; APage: TvVectorialPage; AEllipse: TvEllipse);
var
  cx, cy, rx, ry: double;
  ellipseStr: string;
begin
  ConvertFPVCoordinatesToSVGCoordinates(APage, AEllipse.X, AEllipse.Y, cx, cy);
  ConvertFPVSizeToSVGSize(AEllipse.HorzHalfAxis, AEllipse.VertHalfAxis, rx, ry);
  ellipseStr := Format('  <ellipse cx="%g" cy="%g" rx="%g" ry="%g" style="%s %s">', [
    cx, cy, rx, ry,
    GetPenAsXMLStyle(AEllipse.Pen),
    GetBrushAsXMLStyle(AEllipse.Brush)
    ], FPointSeparator);
  AStrings.Add(ellipseStr);
end;

procedure TvSVGVectorialWriter.WriteEntities(AStrings: TStrings;
  ADoc: TvVectorialDocument; APage: TvVectorialPage);
var
  lEntity: TvEntity;
  i: Integer;
begin
  for i := 0 to APage.GetEntitiesCount() - 1 do
  begin
    lEntity := APage.GetEntity(i);
    WriteEntity(AStrings, ADoc, APage, lEntity);
  end;
end;

procedure TvSVGVectorialWriter.WriteEntity(AStrings: TStrings;
  ADoc: TvVectorialDocument; APage: TvVectorialPage; AEntity: TvEntity);
begin
  if AEntity is TPath then
    WritePath(AStrings, ADoc, APage, TPath(AEntity))
  else
  if AEntity is TvText then
    WriteText(AStrings, ADoc, APage, TvText(AEntity))
  else
  if AEntity is TvCircle then
    WriteCircle(AStrings, ADoc, APage, TvCircle(AEntity))
  else
  if AEntity is TvEllipse then
    WriteEllipse(AStrings, ADoc, APage, TvEllipse(AEntity))
  else
  if AEntity is TvLayer then
    WriteLayer(AStrings, ADoc, APage, TvLayer(AEntity))
  else
  if AEntity is TvRectangle then
    WriteRectangle(AStrings, ADoc, APage, TvRectangle(AEntity))
  else
  if AEntity is TvPolygon then
    WritePolygon(AStrings, ADoc, APage, TvPolygon(AEntity));
end;

procedure TvSVGVectorialWriter.WriteLayer(AStrings: TStrings;
  ADoc: TvVectorialDocument; APage: TvVectorialPage; ALayer: Tvlayer);
var
  lEntity: TvEntity;
begin
  inc(FLayerIndex);
  AStrings.Add('  <g id="layer' + IntToStr(FLayerIndex) + '">');
  lEntity := ALayer.GetFirstEntity;
  while lEntity <> nil do begin
    WriteEntity(AStrings, ADoc, APage, lEntity);
    lEntity := ALayer.GetNextEntity;
  end;
  AStrings.Add('  </g>');
end;

{@@
  SVG Coordinate system measures things only in pixels, so that we have to
  hardcode a DPI value for the screen, which is usually 72.
  FPVectorial uses only millimeters (mm).

  The initial point in FPVectorial is in the bottom-left corner of the document
  and it grows to the top and to the right. In SVG, on the other hand, the
  initial point is in the top-left corner, growing to the bottom and right.
  Besides that, coordinates in SVG are also lengths in comparison to the
  previous point and not absolute coordinates.

  SVG uses commas "," to separate the X,Y coordinates, so it always uses points
  "." as decimal separators and uses no thousand separators
}
procedure TvSVGVectorialWriter.WritePath(AStrings: TStrings;
  ADoc: TvVectorialDocument; APage: TvVectorialPage; APath: TPath);
var
  j: Integer;
  PathStr: string;
  PtX, PtY, OldPtX, OldPtY: double;
  BezierCP1X, BezierCP1Y, BezierCP2X, BezierCP2Y: double;
  segment: TPathSegment;
  l2DSegment: T2DSegment absolute segment;
  l2DBSegment: T2DBezierSegment absolute segment;
  styleStr: string;
begin
  OldPtX := 0;
  OldPtY := 0;
  PathStr := '';

  APath.PrepareForSequentialReading();

  for j := 0 to APath.Len - 1 do
  begin
    segment := TPathSegment(APath.Next());

    if (segment.SegmentType <> st2DLine)
      and (segment.SegmentType <> st2DLineWithPen)
      and (segment.SegmentType <> stMoveTo)
      and (segment.SegmentType <> st2DBezier)
      then Break; // unsupported line type

    // Coordinate conversion from fpvectorial to SVG
    ConvertFPVCoordinatesToSVGCoordinates(APage, l2DSegment.X, l2DSegment.Y, PtX, PtY);
    PtX := PtX - OldPtX;
    PtY := PtY - OldPtY;

    if (segment.SegmentType = stMoveTo) then
      PathStr := PathStr + Format('m %g,%g ', [PtX, PtY], FPointSeparator)
    else
    if (segment.SegmentType = st2DLine) or
       (segment.SegmentType = st2DLineWithPen)
    then
      PathStr := PathStr + Format('l %g,%g ', [PtX, PtY], FPointSeparator)
    else
    if (segment.SegmentType = st2DBezier) then
    begin
      // Converts all coordinates to absolute values
      ConvertFPVCoordinatesToSVGCoordinates(
        APage, l2DBSegment.X2, l2DBSegment.Y2, BezierCP1X, BezierCP1Y);
      ConvertFPVCoordinatesToSVGCoordinates(
        APage, l2DBSegment.X3, l2DBSegment.Y3, BezierCP2X, BezierCP2Y);

      // Transforms them into values relative to the initial point
      BezierCP1X := BezierCP1X - OldPtX;
      BezierCP1Y := BezierCP1Y - OldPtY;
      BezierCP2X := BezierCP2X - OldPtX;
      BezierCP2Y := BezierCP2Y - OldPtY;

      // PtX and PtY already contains the destination point

      // Now render our 2D cubic bezier
      PathStr := PathStr + Format('c %g,%g %g,%g %g,%g ',
        [BezierCP1X, BezierCP1Y, BezierCP2X, BezierCP2Y, PtX, PtY],
        FPointSeparator
      );
    end;

    // Store the current position for future points
    OldPtX := OldPtX + PtX;
    OldPtY := OldPtY + PtY;
  end;

  // Now effectively write the path
  AStrings.Add('  <path');
  styleStr := Format('    style="%s %s"', [
    GetPenAsXMLStyle(APath.Pen),
    GetBrushAsXMLStyle(APath.Brush)
  ]);
  AStrings.Add(styleStr);
  AStrings.Add('    d="' + PathStr + '"');
  inc(FPathIndex);
  AStrings.Add('    id="path' + IntToStr(FPathIndex) + '" />');
end;

procedure TvSVGVectorialWriter.WritePolygon(AStrings: TStrings;
  ADoc: TvVectorialDocument; APage: TvVectorialPage; APolygon: TvPolygon);
var
  pointsStr: String;
  styleStr: String;
  ptX, ptY: Double;
  i: Integer;
begin
  // Collect point coordinates in a string as x,y pairs
  pointsStr := '';
  for i:=0 to High(APolygon.Points) do begin
    ConvertFPVCoordinatesToSVGCoordinates(APage, APolygon.Points[i].X, APolygon.Points[i].Y, ptX, ptY);
    if i=0 then
      pointsStr := Format('%f,%f', [ptX, ptY], FPointSeparator) else
      pointsStr := Format('%s %f,%f', [pointsStr, ptX, ptY], FPointSeparator);
  end;

  // Collect style items.
  styleStr := GetPenAsXMLStyle(APolygon.Pen) + ' ' + GetBrushAsXMLStyle(APolygon.Brush);
  if APolygon.WindingRule = vcmNonZeroWindingRule then
    styleStr := styleStr + 'fill-rule:nonzero;' else
    styleStr := styleStr + 'fill-rule:evenodd;';

  // write the polygon xml
  AStrings.Add('  <polygon');
  AStrings.Add('    points="' + pointsStr + '"');
  AStrings.Add('    style="' + styleStr + '" />');
end;

procedure TvSVGVectorialWriter.WriteRectangle(AStrings: TStrings;
  ADoc: TvVectorialDocument; APage: TvVectorialPage; ARectangle: TvRectangle);
var
  cx, cy, w, h, rx, ry: double;
  rectStr: string;
  styleStr: String;
begin
  ConvertFPVCoordinatesToSVGCoordinates(APage, ARectangle.X, ARectangle.Y, cx, cy);
  ConvertFPVSizeToSVGSize(ARectangle.CX, ARectangle.CY, w, h);
  ConvertFPVSizeToSVGSize(ARectangle.RX, ARectangle.RY, rx, ry);
  rectStr := Format(
    '  <rect x="%g" y="%g" width="%g" height="%g"', [cx, cy, w, h], FPointSeparator);
  if rx <> 0 then
    rectStr := rectStr + Format(' rx="%g"', [rx], FPointSeparator);
  if ry <> 0 then
    rectStr := rectStr + Format(' ry="%g"', [ry], FPointSeparator);
  styleStr := Format(' style="%s %s"', [
    GetPenAsXMLStyle(ARectangle.Pen),
    GetBrushAsXMLStyle(ARectangle.Brush)
  ]);
  rectStr := rectStr + styleStr + '/>';
  AStrings.Add(rectStr);
end;

procedure TvSVGVectorialWriter.WriteText(AStrings: TStrings;
  ADoc: TvVectorialDocument; APage: TvVectorialPage; AText: TvText);
const
  TEXT_ANCHORS: array[TvTextAnchor] of string = ('start', 'middle', 'end');
  TEXT_DECO: array[0..3] of string = ('none', 'underline', 'line-through', 'line-through,underline');
var
  FontSize: Integer;
  TextStr: String;
  PtX, PtY: double;
begin
  ConvertFPVCoordinatesToSVGCoordinates(APage, AText.X, AText.Y, PtX, PtY);
  TextStr := AText.Value.Text;
  FontSize:= ceil(AText.Font.Size * FLOAT_PIXELS_PER_MILLIMETER);

  AStrings.Add('  <text ');
  // Discussion about this offset in bugs 22091 and 26817
  {$IFDEF FPVECTORIAL_SVGWRITER_TEXT_OFFSET}
  AStrings.Add('    x="' + FloatToStr(PtX+0.5*lText.Font.Size, FPointSeparator) + '"');
  AStrings.Add('    y="' + FloatToStr(PtY-6.0*lText.Font.Size, FPointSeparator) + '"');
  {$ELSE}
  AStrings.Add('    x="' + FloatToSVGStr(PtX) + '"');
  AStrings.Add('    y="' + FloatToSVGStr(PtY) + '"');
  {$ENDIF}

  if AText.TextAnchor <> vtaStart then AStrings.Add(
        Format('    text-anchor="%s"', [TEXT_ANCHORS[AText.TextAnchor]]));

  if AText.Font.Bold then
  AStrings.Add('    font-weight="bold"');

  if AText.Font.Italic then
  AStrings.Add('    font-style="oblique"');

  if AText.Font.Underline or AText.Font.Strikethrough then
    AStrings.Add(
        Format('    text-decoration="%s"', [TEXT_DECO[ord(AText.Font.UnderLine)+2*ord(AText.Font.StrikeThrough)]]));

  if AText.Font.Orientation <> 0 then
    AStrings.Add(
        Format('    transform="rotate(%g,%g,%g)"', [-AText.Font.Orientation, PtX, PtY], FPointSeparator));

  AStrings.Add(
        Format('    font-family="%s"', [AText.Font.Name]));

  AStrings.Add(
        Format('    font-size="%d"', [FontSize]));

  AStrings.Add(
        Format('    fill="#%s"', [FPColorToRGBHexString(AText.Font.Color)]));

  AStrings.Add('  >');
  AStrings.Add(TextStr);
  AStrings.Add('  </text>');
end;

procedure TvSVGVectorialWriter.WriteToStrings(AStrings: TStrings;
  AData: TvVectorialDocument);
var
  lPage: TvVectorialPage;
begin
  // Format seetings to convert a string to a float
  FPointSeparator := DefaultFormatSettings;
  FPointSeparator.DecimalSeparator := '.';
  FPointSeparator.ThousandSeparator := '#';// disable the thousand separator
  FCommaSeparator := DefaultFormatSettings;
  FCommaSeparator.DecimalSeparator := ',';
  FCommaSeparator.ThousandSeparator := '#';// disable the thousand separator

  // Headers
  AStrings.Add('<?xml version="1.0" encoding="UTF-8" standalone="no"?>');
  AStrings.Add('<!-- Created with fpVectorial (http://wiki.lazarus.freepascal.org/fpvectorial) -->');
  AStrings.Add('');
  AStrings.Add('<svg');
  AStrings.Add('  xmlns:dc="http://purl.org/dc/elements/1.1/"');
  AStrings.Add('  xmlns:cc="http://creativecommons.org/ns#"');
  AStrings.Add('  xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"');
  AStrings.Add('  xmlns:svg="http://www.w3.org/2000/svg"');
  AStrings.Add('  xmlns="http://www.w3.org/2000/svg"');
  AStrings.Add('  xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"');
  WriteDocumentSize(AStrings, AData);
  AStrings.Add('  id="svg2"');
  AStrings.Add('  version="1.1"');
  WriteDocumentName(AStrings, AData);

  lPage := AData.GetPageAsVectorial(0);

  // Prepare gradient definitions
  PrepareGradients(AStrings, AData, lPage);

  // Now data
  FLayerIndex := 1;
  FPathIndex := 1;
  AStrings.Add('  <g id="layer1">');
  WriteEntities(AStrings, AData, lPage);
  AStrings.Add('  </g>');

  // finalization
  AStrings.Add('</svg>');
end;


initialization

  RegisterVectorialWriter(TvSVGVectorialWriter, vfSVG);

end.

