{
Writes an SVG Document

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho
}
unit svgvectorialwriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, fpvectorial, fpvutils, fpcanvas;

type
  { TvSVGVectorialWriter }

  TvSVGVectorialWriter = class(TvCustomVectorialWriter)
  private
    FPointSeparator, FCommaSeparator: TFormatSettings;
    procedure WriteDocumentSize(AStrings: TStrings; AData: TvVectorialDocument);
    procedure WriteDocumentName(AStrings: TStrings; AData: TvVectorialDocument);
    procedure WritePath(AIndex: Integer; APath: TPath; AStrings: TStrings; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure WriteText(AStrings: TStrings; lText: TvText; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure WriteEntities(AStrings: TStrings; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ConvertFPVCoordinatesToSVGCoordinates(
      const AData: TvVectorialPage;
      const ASrcX, ASrcY: Double; var ADestX, ADestY: double);
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
  // FLOAT_MILIMETERS_PER_PIXEL = 0.3528; // DPI 72 = 1 / 72 inches per pixel

  FLOAT_MILIMETERS_PER_PIXEL = 0.2822; // DPI 90 = 1 / 90 inches per pixel
  FLOAT_PIXELS_PER_MILIMETER = 3.5433; // DPI 90 = 1 / 90 inches per pixel

{ TvSVGVectorialWriter }

procedure TvSVGVectorialWriter.WriteDocumentSize(AStrings: TStrings; AData: TvVectorialDocument);
begin
  AStrings.Add('  width="' + FloatToStr(AData.Width, FPointSeparator) + 'mm"');
  AStrings.Add('  height="' + FloatToStr(AData.Height, FPointSeparator) + 'mm"');
end;

procedure TvSVGVectorialWriter.WriteDocumentName(AStrings: TStrings; AData: TvVectorialDocument);
begin
  AStrings.Add('  sodipodi:docname="New document 1">');
end;

{@@
  SVG Coordinate system measures things only in pixels, so that we have to
  hardcode a DPI value for the screen, which is usually 72.
  FPVectorial uses only milimeters (mm).

  The initial point in FPVectorial is in the bottom-left corner of the document
  and it grows to the top and to the right. In SVG, on the other hand, the
  initial point is in the top-left corner, growing to the bottom and right.
  Besides that, coordinates in SVG are also lengths in comparison to the
  previous point and not absolute coordinates.

  SVG uses commas "," to separate the X,Y coordinates, so it always uses points
  "." as decimal separators and uses no thousand separators
}
procedure TvSVGVectorialWriter.WritePath(AIndex: Integer; APath: TPath; AStrings: TStrings;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  j: Integer;
  PathStr: string;
  PtX, PtY, OldPtX, OldPtY: double;
  BezierCP1X, BezierCP1Y, BezierCP2X, BezierCP2Y: double;
  segment: TPathSegment;
  l2DSegment: T2DSegment absolute segment;
  l2DBSegment: T2DBezierSegment absolute segment;
  // Pen properties
  lPenWidth: Integer;
  lPenColor: string;
  // Brush properties
  lFillColor: string;
begin
  OldPtX := 0;
  OldPtY := 0;
  PathStr := '';

  APath.PrepareForSequentialReading();

  for j := 0 to APath.Len - 1 do
  begin
    segment := TPathSegment(APath.Next());

    if (segment.SegmentType <> st2DLine)
      and (segment.SegmentType <> stMoveTo)
      and (segment.SegmentType <> st2DBezier)
      then Break; // unsupported line type

    // Coordinate conversion from fpvectorial to SVG
    ConvertFPVCoordinatesToSVGCoordinates(
      AData, l2DSegment.X, l2DSegment.Y, PtX, PtY);
    PtX := PtX - OldPtX;
    PtY := PtY - OldPtY;

    if (segment.SegmentType = stMoveTo) then
    begin
      PathStr := PathStr + 'm '
        + FloatToStr(PtX, FPointSeparator) + ','
        + FloatToStr(PtY, FPointSeparator) + ' ';
    end
    else if (segment.SegmentType = st2DLine) then
    begin
      PathStr := PathStr + 'l '
        + FloatToStr(PtX, FPointSeparator) + ','
        + FloatToStr(PtY, FPointSeparator) + ' ';
    end
    else if (segment.SegmentType = st2DBezier) then
    begin
      // Converts all coordinates to absolute values
      ConvertFPVCoordinatesToSVGCoordinates(
        AData, l2DBSegment.X2, l2DBSegment.Y2, BezierCP1X, BezierCP1Y);
      ConvertFPVCoordinatesToSVGCoordinates(
        AData, l2DBSegment.X3, l2DBSegment.Y3, BezierCP2X, BezierCP2Y);

      // Transforms them into values relative to the initial point
      BezierCP1X := BezierCP1X - OldPtX;
      BezierCP1Y := BezierCP1Y - OldPtY;
      BezierCP2X := BezierCP2X - OldPtX;
      BezierCP2Y := BezierCP2Y - OldPtY;

      // PtX and PtY already contains the destination point

      // Now render our 2D cubic bezier
      PathStr := PathStr + 'c '
        + FloatToStr(BezierCP1X, FPointSeparator) + ','
        + FloatToStr(BezierCP1Y, FPointSeparator) + ' '
        + FloatToStr(BezierCP2X, FPointSeparator) + ','
        + FloatToStr(BezierCP2Y, FPointSeparator) + ' '
        + FloatToStr(PtX, FPointSeparator) + ','
        + FloatToStr(PtY, FPointSeparator) + ' '
        ;
    end;

    // Store the current position for future points
    OldPtX := OldPtX + PtX;
    OldPtY := OldPtY + PtY;
  end;

  // Get the Pen Width
  if APath.Pen.Width >= 1 then lPenWidth := APath.Pen.Width
  else lPenWidth := 1;

  // Get the Pen Color and Style
  if APath.Pen.Style = psClear then lPenColor := 'none'
  else lPenColor := '#' + FPColorToRGBHexString(APath.Pen.Color);

  // Get the Brush color and style
  if APath.Brush.Style = bsClear then lFillColor := 'none'
  else lFillColor := '#' + FPColorToRGBHexString(APath.Brush.Color);

  // Now effectively write the path
  AStrings.Add('  <path');
  AStrings.Add(Format('    style="fill:%s;stroke:%s;stroke-width:%dpx;'
   + 'stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"',
   [lFillColor, lPenColor, lPenWidth]));
  AStrings.Add('    d="' + PathStr + '"');
  AStrings.Add('  id="path' + IntToStr(AIndex) + '" />');
end;

procedure TvSVGVectorialWriter.ConvertFPVCoordinatesToSVGCoordinates(
  const AData: TvVectorialPage; const ASrcX, ASrcY: Double; var ADestX,
  ADestY: double);
begin
  ADestX := ASrcX / FLOAT_MILIMETERS_PER_PIXEL;
  ADestY := (AData.Height - ASrcY) / FLOAT_MILIMETERS_PER_PIXEL;
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

  // Now data
  AStrings.Add('  <g id="layer1">');
  lPage := AData.GetPage(0);
  WriteEntities(AStrings, lPage, AData);
  AStrings.Add('  </g>');

  // finalization
  AStrings.Add('</svg>');
end;

procedure TvSVGVectorialWriter.WriteText(AStrings: TStrings; lText: TvText;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  i, j, FontSize: Integer;
  TextStr, FontName, SVGFontFamily: string;
  PtX, PtY: double;
begin
  TextStr := '';

  ConvertFPVCoordinatesToSVGCoordinates(
      AData, lText.X, lText.Y, PtX, PtY);

  TextStr := lText.Value.Text;
  FontSize:= ceil(lText.Font.Size / FLOAT_MILIMETERS_PER_PIXEL);
  SVGFontFamily := 'Arial, sans-serif';//lText.FontName;

  AStrings.Add('  <text ');
  AStrings.Add('    x="' + FloatToStr(PtX, FPointSeparator) + '"');
  AStrings.Add('    y="' + FloatToStr(PtY, FPointSeparator) + '"');
//    AStrings.Add('    font-size="' + IntToStr(FontSize) + '"'); Doesn't seam to work, we need to use the tspan
  AStrings.Add('    font-family="' + SVGFontFamily + '">');
  AStrings.Add('    <tspan ');
  AStrings.Add('      style="font-size:' + IntToStr(FontSize) + '" ');
//    AStrings.Add('      id="tspan2828" ');
  AStrings.Add('    >');
  AStrings.Add(TextStr + '</tspan></text>');
end;

procedure TvSVGVectorialWriter.WriteEntities(AStrings: TStrings;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  lEntity: TvEntity;
  i, j: Integer;
begin
  for i := 0 to AData.GetEntitiesCount() - 1 do
  begin
    lEntity := AData.GetEntity(i);

    if lEntity is TPath then WritePath(i, TPath(lEntity), AStrings, AData, ADoc)
    else if lEntity is TvText then WriteText(AStrings, TvText(lEntity), AData, ADoc);
  end;
end;

initialization

  RegisterVectorialWriter(TvSVGVectorialWriter, vfSVG);

end.

