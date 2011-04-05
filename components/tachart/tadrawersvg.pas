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
  Classes, FPImage, FPCanvas, TADrawUtils;

type

  { TSVGDrawer }

  TSVGDrawer = class(TBasicDrawer, IChartDrawer)
  strict private
    FBrushColor: TFPColor;
    FClippingPathId: Integer;
    FFont: TFPCustomFont;
    FFontAngle: Double;
    FPenColor: TFPColor;
    FPrevPos: TPoint;
    FStream: TStream;

    function FontSize: Integer; inline;
    procedure InternalPolyline(
      const APoints: array of TPoint; AStartIndex, ANumPts: Integer;
      const AFill: String);

    procedure SetBrush(ABrush: TFPCustomBrush);
    procedure SetFont(AFont: TFPCustomFont);
    procedure SetPen(APen: TFPCustomPen);

    procedure WriteFmt(const AFormat: String; AParams: array of const);
    procedure WriteStr(const AString: String);
  strict protected
    function GetFontAngle: Double; override;
    function SimpleTextExtent(const AText: String): TPoint; override;
    procedure SimpleTextOut(AX, AY: Integer; const AText: String); override;
  public
    constructor Create(AStream: TStream; AWriteDocType: Boolean);
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
      const APoints: array of TPoint;
      AStartIndex: Integer = 0; ANumPts: Integer = -1); override;
    procedure Polyline(
      const APoints: array of TPoint; AStartIndex: Integer = 0;
      ANumPts: Integer = -1; AEndPoint: Boolean = false);
    procedure PrepareSimplePen(AColor: TChartColor);
    procedure RadialPie(
      AX1, AY1, AX2, AY2: Integer;
      AStartAngle16Deg, AAngleLength16Deg: Integer);
    procedure Rectangle(const ARect: TRect);
    procedure Rectangle(AX1, AY1, AX2, AY2: Integer);
    procedure SetBrushColor(AColor: TChartColor);
    procedure SetBrushParams(AStyle: TFPBrushStyle; AColor: TChartColor);
    procedure SetPenParams(AStyle: TFPPenStyle; AColor: TChartColor);
  end;

implementation

uses
  Math, SysUtils, TAChartUtils, TAGeometry;

const
  RECT_FMT =
    '<rect x="%d" y="%d" width="%d" height="%d" ' +
    'style="stroke: %s; fill: %s;"/>';

function ColorToHex(AColor: TFPColor): String;
begin
  With AColor do
    Result := '#' +
      IntToHex(red shr 8, 2) +
      IntToHex(green shr 8, 2) +
      IntToHex(blue shr 8, 2);
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
    WriteFmt(RECT_FMT, [Left, Top, Right - Left, Bottom - Top, 'black', 'none']);
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
  if AWriteDocType then begin
    WriteStr('<?xml version="1.0"?>');
    WriteStr('<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.0//EN"');
    WriteStr('"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">');
  end;
end;

procedure TSVGDrawer.DrawingBegin(const ABoundingBox: TRect);
begin
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
  WriteStr('</svg>');
end;

procedure TSVGDrawer.Ellipse(AX1, AY1, AX2, AY2: Integer);
var
  cx, cy, rx, ry: Integer;
begin
  BoundingBoxToCenterAndHalfRadius(AX1, AY1, AX2, AY2, cx, cy, rx, ry);
  WriteFmt('<ellipse cx="%d" cy="%d" rx="%d" ry="%d"/>', [cx, cy, rx, ry]);
end;

procedure TSVGDrawer.FillRect(AX1, AY1, AX2, AY2: Integer);
begin
  WriteFmt(
    RECT_FMT,
    [AX1, AY1, AX2 - AX1, AY2 - AY1, 'none', ColorToHex(FBrushColor)]);
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

procedure TSVGDrawer.InternalPolyline(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer;
  const AFill: String);
var
  s: String;
  i: Integer;
begin
  if ANumPts < 0 then
    ANumPts := Length(APoints) - AStartIndex;
  s := '';
  for i := 0 to ANumPts - 1 do
    s += Format('%d %d ', [APoints[i].X, APoints[i].Y]);
  WriteFmt(
    '<polyline points="%s" style="stroke: %s; fill: %s"/>',
    [s, ColorToHex(FPenColor), AFill]);
end;

procedure TSVGDrawer.Line(AX1, AY1, AX2, AY2: Integer);
begin
  WriteFmt(
    '<line x1="%d" y1="%d" x2="%d" y2="%d" style="stroke: %s;"/>',
    [AX1, AY1, AX2, AY2, ColorToHex(FPenColor)]);
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

procedure TSVGDrawer.Polygon(
  const APoints: array of TPoint; AStartIndex: Integer; ANumPts: Integer);
begin
  InternalPolyline(APoints, AStartIndex, ANumPts, ColorToHex(FBrushColor));
end;

procedure TSVGDrawer.Polyline(
  const APoints: array of TPoint; AStartIndex: Integer; ANumPts: Integer;
  AEndPoint: Boolean);
begin
  Unused(AEndPoint);
  InternalPolyline(APoints, AStartIndex, ANumPts, 'none');
end;

procedure TSVGDrawer.PrepareSimplePen(AColor: TChartColor);
begin
  FPenColor := FChartColorToFPColorFunc(AColor);
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
    RECT_FMT,
    [AX1, AY1, AX2 - AX1, AY2 - AY1,
      ColorToHex(FPenColor), ColorToHex(FBrushColor)]);
end;

procedure TSVGDrawer.Rectangle(const ARect: TRect);
begin
  with ARect do
    Rectangle(Left, Top, Right, Bottom);
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
  FPenColor := APen.FPColor;
end;

procedure TSVGDrawer.SetPenParams(AStyle: TFPPenStyle; AColor: TChartColor);
begin
  FPenColor := FChartColorToFPColorFunc(AColor);
  Unused(AStyle);
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
    '<text x="%d" y="%d" ' +
    'style="stroke: none; fill: %s; font-family: %s; font-size: %dpt;">' +
    '%s</text>',
    [p.X, p.Y, ColorToHex(FFont.FPColor), FFont.Name, FontSize, AText]);
end;

procedure TSVGDrawer.WriteFmt(const AFormat: String; AParams: array of const);
begin
  WriteStr(Format(AFormat, AParams));
end;

procedure TSVGDrawer.WriteStr(const AString: String);
begin
  FStream.WriteBuffer(AString[1], Length(AString));
  FStream.WriteBuffer(LineEnding[1], Length(LineEnding));
end;

end.

