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
unit TADrawerBGRA;

{$H+}

interface

uses
  BGRABitmap, BGRABitmapTypes, Classes, FPCanvas, TADrawUtils;

type

  { TBGRABitmapDrawer }
  TBGRABitmapDrawer = class(TBasicDrawer, IChartDrawer)
  strict private
    FBrushColor: TBGRAPixel;
    FBrushStyle: TFPBrushStyle;
    FCanvas: TBGRABitmap;
    FClipRect: TRect;
    FFontColor: TBGRAPixel;
    FFontOrientation: Integer;
    FPenColor: TBGRAPixel;
    FPenWidth: Integer;
    FPrevPoint: TPoint;

    procedure SetBrush(ABrush: TFPCustomBrush);
    procedure SetFont(AFont: TFPCustomFont);
    procedure SetPen(APen: TFPCustomPen);
  strict protected
    function GetFontAngle: Double; override;
    procedure SetAntialiasingMode(AValue: TChartAntialiasingMode);
    function SimpleTextExtent(const AText: String): TPoint; override;
    procedure SimpleTextOut(AX, AY: Integer; const AText: String); override;
  public
    constructor Create(ACanvas: TBGRABitmap);
  public
    procedure AddToFontOrientation(ADelta: Integer);
    procedure ClippingStart;
    procedure ClippingStart(const AClipRect: TRect);
    procedure ClippingStop;
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
    procedure SetBrushColor(AColor: TChartColor);
    procedure SetBrushParams(AStyle: TFPBrushStyle; AColor: TChartColor);
    procedure SetPenParams(AStyle: TFPPenStyle; AColor: TChartColor);
  end;

implementation

uses
  TAChartUtils, TAGeometry;

function PointsToPointsF(
  APoints: array of TPoint; AStartIndex, ANumPts: Integer): ArrayOfTPointF;
var
  i: Integer;
begin
  Assert(ANumPts >= 0);
  SetLength(Result, ANumPts);
  for i := 0 to ANumPts - 1 do
    with APoints[i + AStartIndex] do
      Result[i] := PointF(X, Y);
end;

{ TBGRABitmapDrawer }

procedure TBGRABitmapDrawer.AddToFontOrientation(ADelta: Integer);
begin
  FFontOrientation += ADelta;
end;

procedure TBGRABitmapDrawer.ClippingStart(const AClipRect: TRect);
begin
  FClipRect := AClipRect;
  ClippingStart;
end;

procedure TBGRABitmapDrawer.ClippingStart;
begin
  FCanvas.ClipRect := FClipRect;
end;

procedure TBGRABitmapDrawer.ClippingStop;
begin
  FCanvas.NoClip;
end;

constructor TBGRABitmapDrawer.Create(ACanvas: TBGRABitmap);
begin
  FCanvas := ACanvas;
end;

procedure TBGRABitmapDrawer.Ellipse(AX1, AY1, AX2, AY2: Integer);
var
  cx, cy, rx, ry: Integer;
begin
  BoundingBoxToCenterAndHalfRadius(AX1, AY1, AX2, AY2, cx, cy, rx, ry);
  FCanvas.FillEllipseAntialias(cx, cy, rx, ry, FBrushColor);
  FCanvas.EllipseAntialias(cx, cy, rx, ry, FPenColor, 1.0);
end;

procedure TBGRABitmapDrawer.FillRect(AX1, AY1, AX2, AY2: Integer);
var
  bt: TBGRACustomBitmap;
begin
  if FBrushStyle = bsSolid then
    FCanvas.FillRect(AX1, AY1, AX2, AY2, FBrushColor, dmSet)
  else begin
    bt := FCanvas.CreateBrushTexture(
      FBrushStyle, FBrushColor, BGRAPixelTransparent);
    try
      FCanvas.FillRect(AX1, AY1, AX2, AY2, bt, dmSet)
    finally
      bt.Free;
    end;
  end;
end;

function TBGRABitmapDrawer.GetBrushColor: TChartColor;
begin
  Result := TChartColor(BGRAToColor(FBrushColor));
end;

function TBGRABitmapDrawer.GetFontAngle: Double;
begin
  Result := 0.0;
end;

procedure TBGRABitmapDrawer.Line(AX1, AY1, AX2, AY2: Integer);
begin
  FCanvas.DrawLineAntialias(AX1, AY1, AX2, AY2, FPenColor, FPenWidth);
end;

procedure TBGRABitmapDrawer.Line(const AP1, AP2: TPoint);
begin
  FCanvas.DrawLineAntialias(AP1.X, AP1.Y, AP2.X, AP2.Y, FPenColor, FPenWidth);
end;

procedure TBGRABitmapDrawer.LineTo(AX, AY: Integer);
var
  p: TPoint;
begin
  p := Point(AX, AY);
  Line(FPrevPoint, p);
  FPrevPoint := p;
end;

procedure TBGRABitmapDrawer.MoveTo(AX, AY: Integer);
begin
  FPrevPoint := Point(AX, AY);
end;

procedure TBGRABitmapDrawer.Polygon(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
var
  bt: TBGRACustomBitmap;
begin
  if FBrushStyle = bsSolid then
    FCanvas.DrawPolygonAntialias(
      PointsToPointsF(APoints, AStartIndex, ANumPts), FBrushColor, FPenWidth)
  else begin
    bt := FCanvas.CreateBrushTexture(
      FBrushStyle, FBrushColor, BGRAPixelTransparent);
    try
      FCanvas.DrawPolygonAntialias(
        PointsToPointsF(APoints, AStartIndex, ANumPts), bt, FPenWidth);
    finally
      bt.Free;
    end;
  end;
end;

procedure TBGRABitmapDrawer.Polyline(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
begin
  FCanvas.DrawPolyLineAntialias(
    PointsToPointsF(APoints, AStartIndex, ANumPts), FPenColor, FPenWidth);
end;

procedure TBGRABitmapDrawer.PrepareSimplePen(AColor: TChartColor);
begin
  FPenColor := ColorToBGRA(AColor);
  FCanvas.PenStyle := psSolid;
end;

procedure TBGRABitmapDrawer.RadialPie(
  AX1, AY1, AX2, AY2: Integer; AStartAngle16Deg, AAngleLength16Deg: Integer);
begin
  Unused(AX1, AY1);
  Unused(AX2, AY2);
  Unused(AStartAngle16Deg, AAngleLength16Deg);
end;

procedure TBGRABitmapDrawer.Rectangle(AX1, AY1, AX2, AY2: Integer);
var
  bt: TBGRACustomBitmap;
begin
  if FBrushStyle = bsSolid then
    FCanvas.RectangleAntialias(
      AX1, AY1, AX2, AY2, FPenColor, FPenWidth, FBrushColor)
  else begin
    bt := FCanvas.CreateBrushTexture(
      FBrushStyle, FBrushColor, BGRAPixelTransparent);
    try
      FCanvas.RectangleAntialias(AX1, AY1, AX2, AY2, bt, FPenWidth);
    finally
      bt.Free;
    end;
  end;
end;

procedure TBGRABitmapDrawer.Rectangle(const ARect: TRect);
begin
  with ARect do
    Rectangle(Left, Top, Right, Bottom)
end;

procedure TBGRABitmapDrawer.SetAntialiasingMode(AValue: TChartAntialiasingMode);
begin
  FCanvas.FontAntialias := AValue = amOn;
end;

procedure TBGRABitmapDrawer.SetBrush(ABrush: TFPCustomBrush);
begin
  FBrushColor := FPColorToBGRA(ABrush.FPColor);
  FBrushStyle := ABrush.Style;
end;

procedure TBGRABitmapDrawer.SetBrushColor(AColor: TChartColor);
begin
  FBrushColor := ColorToBGRA(AColor);
end;

procedure TBGRABitmapDrawer.SetBrushParams(
  AStyle: TFPBrushStyle; AColor: TChartColor);
begin
  Unused(AStyle);
  FBrushColor := ColorToBGRA(AColor);
  FBrushStyle := AStyle;
end;

procedure TBGRABitmapDrawer.SetFont(AFont: TFPCustomFont);
begin
  FCanvas.FontName := AFont.Name;
  FCanvas.FontHeight := AFont.Size * 96 div 72;
  FFontOrientation := FGetFontOrientationFunc(AFont);
  FFontColor := FPColorToBGRA(AFont.FPColor);
  // TODO: FontStyle
end;

procedure TBGRABitmapDrawer.SetPen(APen: TFPCustomPen);
begin
  FCanvas.PenStyle := APen.Style;
  FPenWidth := APen.Width;
  // TODO: JoinStyle
  FPenColor := FPColorToBGRA(APen.FPColor);
end;

procedure TBGRABitmapDrawer.SetPenParams(
  AStyle: TFPPenStyle; AColor: TChartColor);
begin
  FCanvas.PenStyle := AStyle;
  FPenColor := ColorToBGRA(AColor);
end;

function TBGRABitmapDrawer.SimpleTextExtent(const AText: String): TPoint;
begin
  Result := FCanvas.TextSize(AText);
end;

procedure TBGRABitmapDrawer.SimpleTextOut(AX, AY: Integer; const AText: String);
begin
  FCanvas.TextOutAngle(
    AX, AY, FFontOrientation, AText, FFontColor, taLeftJustify);
end;

end.

