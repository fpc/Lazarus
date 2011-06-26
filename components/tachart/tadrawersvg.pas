{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

Authors: Alexander Klenin

}
unit TADrawerSVG;

{$H+}

interface

uses
  Classes, FPImage, FPCanvas, TAChartUtils, TADrawUtils;

type

  { TSVGDrawer }

  TSVGDrawer = class(TBasicDrawer, IChartDrawer)
  strict private
    FAntialiasingMode: TChartAntialiasingMode;
    FBrushColor: TFPColor;
    FClippingPathId: Integer;
    FFont: TFPCustomFont;
    FFontAngle: Double;
    FPen: TFPCustomPen;
    FPrevPos: TPoint;
    FStream: TStream;

    function FontSize: Integer; inline;
    function PointsToStr(
      const APoints: array of TPoint; AStartIndex, ANumPts: Integer): String;

    procedure SetBrush(ABrush: TFPCustomBrush);
    procedure SetFont(AFont: TFPCustomFont);
    procedure SetPen(APen: TFPCustomPen);

    function StyleFill: String;
    function StyleStroke: String;

    procedure WriteFmt(const AFormat: String; AParams: array of const);
    procedure WriteStr(const AString: String);
  strict protected
    function GetFontAngle: Double; override;
    function SimpleTextExtent(const AText: String): TPoint; override;
    procedure SimpleTextOut(AX, AY: Integer; const AText: String); override;
  public
    constructor Create(AStream: TStream; AWriteDocType: Boolean);
    destructor Destroy; override;
  public
    procedure AddToFontOrientation(ADelta: Integer);
    procedure ClippingStart;
    procedure ClippingStart(const AClipRect: TRect);
    procedure ClippingStop;
    procedure DrawingBegin(const ABoundingBox: TRect); override;
    procedure DrawingEnd; override;
    procedure Ellipse(AX1, AY1, AX2, AY2: Integer);
    procedure FillRect(AX1, AY1, AX2, AY2: Integer);
    function GetBrushColor: TChartColor;
    procedure Line(AX1, AY1, AX2, AY2: Integer);
    procedure Line(const AP1, AP2: TPoint);
    procedure LineTo(AX, AY: Integer); override;
    procedure MoveTo(AX, AY: Integer); override;
    procedure Polygon(
      const APoints: array of TPoint; AStartIndex, ANumPts: Integer); override;
    procedure Polyline(
      const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
    procedure PrepareSimplePen(AColor: TChartColor);
    procedure RadialPie(
      AX1, AY1, AX2, AY2: Integer;
      AStartAngle16Deg, AAngleLength16Deg: Integer);
    procedure Rectangle(const ARect: TRect);
    procedure Rectangle(AX1, AY1, AX2, AY2: Integer);
    procedure SetAntialiasingMode(AValue: TChartAntialiasingMode);
    procedure SetBrushColor(AColor: TChartColor);
    procedure SetBrushParams(AStyle: TFPBrushStyle; AColor: TChartColor);
    procedure SetPenParams(AStyle: TFPPenStyle; AColor: TChartColor);
  end;

implementation

uses
  Math, SysUtils, TAGeometry;

const
  RECT_FMT =
    '<rect x="%d" y="%d" width="%d" height="%d" style="%s"/>';

function ColorToHex(AColor: TFPColor): String;
begin
  if AColor = colBlack then
    Result := 'black'
  else if AColor = colWhite then
    Result := 'white'
  else
    with AColor do
      Result := Format('#%.2x%.2x%.2x', [red shr 8, green shr 8, blue shr 8]);
end;

{ TSVGDrawer }

procedure TSVGDrawer.AddToFontOrientation(ADelta: Integer);
begin
  FFontAngle += OrientToRad(ADelta);
end;

procedure TSVGDrawer.ClippingStart(const AClipRect: TRect);
begin
  FClippingPathId += 1;
  WriteFmt('<clipPath id="clip%d">', [FClippingPathId]);
  with AClipRect do
    WriteFmt(RECT_FMT, [Left, Top, Right - Left, Bottom - Top, '']);
  WriteStr('</clipPath>');
  ClippingStart;
end;

procedure TSVGDrawer.ClippingStart;
begin
  WriteFmt('<g clip-path="url(#clip%d)">', [FClippingPathId]);
end;

procedure TSVGDrawer.ClippingStop;
begin
  WriteStr('</g>');
end;

constructor TSVGDrawer.Create(AStream: TStream; AWriteDocType: Boolean);
begin
  FStream := AStream;
  FPen := TFPCustomPen.Create;
  if AWriteDocType then begin
    WriteStr('<?xml version="1.0"?>');
    WriteStr('<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.0//EN"');
    WriteStr('"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">');
  end;
end;

destructor TSVGDrawer.Destroy;
begin
  FreeAndNil(FPen);
  inherited Destroy;
end;

procedure TSVGDrawer.DrawingBegin(const ABoundingBox: TRect);
begin
  FAntialiasingMode := amDontCare;
  with ABoundingBox do
    WriteFmt(
      '<svg ' +
      'xmlns="http://www.w3.org/2000/svg" ' +
      'xmlns:xlink="http://www.w3.org/1999/xlink" ' +
      'width="%dpx" height="%dpx" viewBox="%d %d %d %d">',
      [Right - Left, Bottom - Top, Left, Top, Right, Bottom]);
  FClippingPathId := 0;
end;

procedure TSVGDrawer.DrawingEnd;
begin
  if FAntialiasingMode <> amDontCare then
    WriteStr('</g>');
  WriteStr('</svg>');
end;

procedure TSVGDrawer.Ellipse(AX1, AY1, AX2, AY2: Integer);
var
  cx, cy, rx, ry: Integer;
begin
  BoundingBoxToCenterAndHalfRadius(AX1, AY1, AX2, AY2, cx, cy, rx, ry);
  WriteFmt(
    '<ellipse cx="%d" cy="%d" rx="%d" ry="%d" style="%s"/>',
    [cx, cy, rx, ry, StyleFill + StyleStroke]);
end;

procedure TSVGDrawer.FillRect(AX1, AY1, AX2, AY2: Integer);
begin
  WriteFmt(RECT_FMT, [AX1, AY1, AX2 - AX1, AY2 - AY1, StyleFill]);
end;

function TSVGDrawer.FontSize: Integer;
begin
  Result := IfThen(FFont.Size = 0, 8, FFont.Size);
end;

function TSVGDrawer.GetBrushColor: TChartColor;
begin
  Result := FPColorToChartColor(FBrushColor);
end;

function TSVGDrawer.GetFontAngle: Double;
begin
  Result := FFontAngle;
end;

procedure TSVGDrawer.Line(AX1, AY1, AX2, AY2: Integer);
begin
  WriteFmt(
    '<line x1="%d" y1="%d" x2="%d" y2="%d" style="%s"/>',
    [AX1, AY1, AX2, AY2, StyleStroke]);
end;

procedure TSVGDrawer.Line(const AP1, AP2: TPoint);
begin
  Line(AP1.X, AP1.Y, AP2.X, AP2.Y);
end;

procedure TSVGDrawer.LineTo(AX, AY: Integer);
begin
  Line(FPrevPos.X, FPrevPos.Y, AX, AY);
  FPrevPos := Point(AX, AY);
end;

procedure TSVGDrawer.MoveTo(AX, AY: Integer);
begin
  FPrevPos := Point(AX, AY);
end;

function TSVGDrawer.PointsToStr(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer): String;
var
  i: Integer;
begin
  if ANumPts < 0 then
    ANumPts := Length(APoints) - AStartIndex;
  Result := '';
  for i := 0 to ANumPts - 1 do
    with APoints[i + AStartIndex] do
      Result += Format('%d %d ', [X, Y]);
end;

procedure TSVGDrawer.Polygon(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
begin
  WriteFmt(
    '<polygon points="%s" style="%s"/>',
    [PointsToStr(APoints, AStartIndex, ANumPts), StyleFill + StyleStroke]);
end;

procedure TSVGDrawer.Polyline(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
begin
  WriteFmt(
    '<polyline points="%s" style="stroke: %s;"/>',
    [PointsToStr(APoints, AStartIndex, ANumPts), StyleStroke]);
end;

procedure TSVGDrawer.PrepareSimplePen(AColor: TChartColor);
begin
  FPen.FPColor := FChartColorToFPColorFunc(AColor);
  FPen.Style := psSolid;
  FPen.Width := 1;
end;

procedure TSVGDrawer.RadialPie(
  AX1, AY1, AX2, AY2: Integer; AStartAngle16Deg, AAngleLength16Deg: Integer);
begin
  Unused(AX1, AY1);
  Unused(AX2, AY2);
  Unused(AStartAngle16Deg, AAngleLength16Deg);
end;

procedure TSVGDrawer.Rectangle(AX1, AY1, AX2, AY2: Integer);
begin
  WriteFmt(
    RECT_FMT, [AX1, AY1, AX2 - AX1, AY2 - AY1, StyleFill + StyleStroke]);
end;

procedure TSVGDrawer.Rectangle(const ARect: TRect);
begin
  with ARect do
    Rectangle(Left, Top, Right, Bottom);
end;

procedure TSVGDrawer.SetAntialiasingMode(AValue: TChartAntialiasingMode);
const
  AM_TO_CSS: array [amOn .. amOff] of String =
    ('geometricPrecision', 'crispEdges');
begin
  if FAntialiasingMode = AValue then exit;
  if FAntialiasingMode <> amDontCare then
    WriteStr('</g>');
  FAntialiasingMode := AValue;
  if FAntialiasingMode <> amDontCare then
    WriteFmt('<g style="shape-rendering: %s">',[AM_TO_CSS[FAntialiasingMode]]);
end;

procedure TSVGDrawer.SetBrush(ABrush: TFPCustomBrush);
begin
  FBrushColor := ABrush.FPColor;
end;

procedure TSVGDrawer.SetBrushColor(AColor: TChartColor);
begin
  FBrushColor := FChartColorToFPColorFunc(AColor);
end;

procedure TSVGDrawer.SetBrushParams(
  AStyle: TFPBrushStyle; AColor: TChartColor);
begin
  FBrushColor := FChartColorToFPColorFunc(AColor);
  Unused(AStyle);
end;

procedure TSVGDrawer.SetFont(AFont: TFPCustomFont);
begin
  FFont := AFont;
end;

procedure TSVGDrawer.SetPen(APen: TFPCustomPen);
begin
  FPen.FPColor := APen.FPColor;
  FPen.Style := APen.Style;
  FPen.Width := APen.Width;
end;

procedure TSVGDrawer.SetPenParams(AStyle: TFPPenStyle; AColor: TChartColor);
begin
  FPen.FPColor := FChartColorToFPColorFunc(AColor);
  FPen.Style := AStyle;
end;

function TSVGDrawer.SimpleTextExtent(const AText: String): TPoint;
begin
  // SVG does not have a way to determine text size.
  // Use some heuristics.
  Result.X := FontSize * Length(AText) * 2 div 3;
  Result.Y := FontSize;
end;

procedure TSVGDrawer.SimpleTextOut(AX, AY: Integer; const AText: String);
var
  p: TPoint;
begin
  p := RotatePoint(Point(0, FontSize), -FFontAngle) + Point(AX, AY);
  WriteFmt(
    '<text x="%d" y="%d" textLength="%d" ' +
    'style="stroke: none; fill: %s; font-family: %s; font-size: %dpt;">' +
    '%s</text>',
    [p.X, p.Y, SimpleTextExtent(AText).X,
      ColorToHex(FFont.FPColor), FFont.Name, FontSize, AText]);
end;

function TSVGDrawer.StyleFill: String;
begin
  Result := Format('fill:%s;', [ColorToHex(FBrushColor)]);
end;

function TSVGDrawer.StyleStroke: String;
const
  PEN_DASHARRAY: array [TFPPenStyle] of String =
    ('', '2,2', '1,1', '2,1,1,1', '2,1,1,1,1,1', '', '', '');
begin
  if FPen.Style = psClear then
    exit('stroke: none');
  Result := 'stroke:' + ColorToHex(FPen.FPColor) + ';';
  if FPen.Width <> 1 then
    Result += 'stroke-width:' + IntToStr(FPen.Width) + ';';
  if PEN_DASHARRAY[FPen.Style] <> '' then
    Result += 'stroke-dasharray:' + PEN_DASHARRAY[FPen.Style] + ';';
end;

procedure TSVGDrawer.WriteFmt(const AFormat: String; AParams: array of const);
begin
  WriteStr(Format(AFormat, AParams));
end;

procedure TSVGDrawer.WriteStr(const AString: String);
var
  le: String = LineEnding;
begin
  FStream.WriteBuffer(AString[1], Length(AString));
  FStream.WriteBuffer(le[1], Length(le));
end;

end.

