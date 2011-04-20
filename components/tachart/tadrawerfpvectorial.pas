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
unit TADrawerFPVectorial;

{$H+}

interface

uses
  Classes, FPCanvas, FPVectorial, TADrawUtils;

type

  { TFPVectorialDrawer }

  TFPVectorialDrawer = class(TBasicDrawer, IChartDrawer)
  strict private
    FBrushColor: TvColor;
    FCanvas: TvVectorialDocument;
    FPenColor: TvColor;
  strict protected
    function GetFontAngle: Double; override;
    procedure SetBrush(ABrush: TFPCustomBrush);
    procedure SetFont(AFont: TFPCustomFont);
    procedure SetPen(APen: TFPCustomPen);
    function SimpleTextExtent(const AText: String): TPoint; override;
    procedure SimpleTextOut(AX, AY: Integer; const AText: String); override;
  public
    constructor Create(ACanvas: TvVectorialDocument);
  public
    procedure AddToFontOrientation(ADelta: Integer);
    procedure ClippingStart;
    procedure ClippingStart(const AClipRect: TRect);
    procedure ClippingStop;
    procedure Ellipse(AX1, AY1, AX2, AY2: Integer);
    procedure FillRect(AX1, AY1, AX2, AY2: Integer);
    function GetBrushColor: TChartColor;
    procedure Line(AX1, AY1, AX2, AY2: Integer);
    procedure Line(const AP1, AP2: TPoint); overload;
    procedure LineTo(AX, AY: Integer); override;
    procedure MoveTo(AX, AY: Integer); override; overload;
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
  end experimental;

implementation

uses
  TAChartUtils, TAGeometry;

function ChartColorToVColor(AColor: TChartColor): TvColor;
begin
  Result.Alpha := 255;
  Result.Red := AColor;
  Result.Green := AColor shr 8;
  Result.Blue := AColor shr 16;
end;

function VColorToChartColor(AColor: TvColor): TChartColor;
begin
  with AColor do
    Result := Red + (Green shl 8) + (Blue shl 16);
end;

{ TFPVectorialDrawer }

procedure TFPVectorialDrawer.AddToFontOrientation(ADelta: Integer);
begin

end;

procedure TFPVectorialDrawer.ClippingStart(const AClipRect: TRect);
begin
  Unused(AClipRect); // Not implemented.
end;

procedure TFPVectorialDrawer.ClippingStart;
begin
  // Not implemented.
end;

procedure TFPVectorialDrawer.ClippingStop;
begin
  // Not implemented.
end;

constructor TFPVectorialDrawer.Create(ACanvas: TvVectorialDocument);
begin
  inherited Create;
  FCanvas := ACanvas;
end;

procedure TFPVectorialDrawer.Ellipse(AX1, AY1, AX2, AY2: Integer);
var
  cx, cy, rx, ry: Integer;
begin
  BoundingBoxToCenterAndHalfRadius(AX1, AY1, AX2, AY2, cx, cy, rx, ry);
  FCanvas.AddEllipse(cx, cy, 0, rx, ry, 0.0);
end;

procedure TFPVectorialDrawer.FillRect(AX1, AY1, AX2, AY2: Integer);
begin
  // Not implemented.
end;

function TFPVectorialDrawer.GetBrushColor: TChartColor;
begin
  Result := VColorToChartColor(FBrushColor);
end;

function TFPVectorialDrawer.GetFontAngle: Double;
begin
  Result := 0.0;
end;

procedure TFPVectorialDrawer.Line(AX1, AY1, AX2, AY2: Integer);
begin
  FCanvas.StartPath(AX1, AY1);
  FCanvas.AddLineToPath(AX2, AY2, FPenColor);
  FCanvas.EndPath();
end;

procedure TFPVectorialDrawer.Line(const AP1, AP2: TPoint);
begin
  Line(AP1.X, AP1.Y, AP2.X, AP2.Y);
end;

procedure TFPVectorialDrawer.LineTo(AX, AY: Integer);
begin
  FCanvas.AddLineToPath(AX, AY, FPenColor);
end;

procedure TFPVectorialDrawer.MoveTo(AX, AY: Integer);
begin
  FCanvas.EndPath();
  FCanvas.StartPath(AX, AY);
end;

procedure TFPVectorialDrawer.Polygon(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
begin
  // Not implemented.
  Polyline(APoints, AStartIndex, ANumPts);
end;

procedure TFPVectorialDrawer.Polyline(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
var
  i: Integer;
begin
  MoveTo(APoints[AStartIndex]);
  for i := 1 to ANumPts - 1 do
    with APoints[i + AStartIndex] do
      FCanvas.AddLineToPath(X, Y, FPenColor);
  FCanvas.EndPath();
end;

procedure TFPVectorialDrawer.PrepareSimplePen(AColor: TChartColor);
begin
  FPenColor := ChartColorToVColor(AColor);
end;

procedure TFPVectorialDrawer.RadialPie(
  AX1, AY1, AX2, AY2: Integer; AStartAngle16Deg, AAngleLength16Deg: Integer);
begin
  // Not implemented.
end;

procedure TFPVectorialDrawer.Rectangle(AX1, AY1, AX2, AY2: Integer);
begin
  // Not implemented.
end;

procedure TFPVectorialDrawer.Rectangle(const ARect: TRect);
begin
  // Not implemented.
end;

procedure TFPVectorialDrawer.SetBrush(ABrush: TFPCustomBrush);
begin
  FBrushColor := ChartColorToVColor(FPColorToChartColor(ABrush.FPColor));
end;

procedure TFPVectorialDrawer.SetBrushColor(AColor: TChartColor);
begin
  FBrushColor := ChartColorToVColor(AColor);
end;

procedure TFPVectorialDrawer.SetBrushParams(
  AStyle: TFPBrushStyle; AColor: TChartColor);
begin
  Unused(AStyle);
  SetBrushColor(AColor);
end;

procedure TFPVectorialDrawer.SetFont(AFont: TFPCustomFont);
begin
  // Not implemented.
end;

procedure TFPVectorialDrawer.SetPen(APen: TFPCustomPen);
begin
  FPenColor := ChartColorToVColor(FPColorToChartColor(APen.FPColor));
end;

procedure TFPVectorialDrawer.SetPenParams(
  AStyle: TFPPenStyle; AColor: TChartColor);
begin
  Unused(AStyle);
  FPenColor := ChartColorToVColor(AColor);
end;

function TFPVectorialDrawer.SimpleTextExtent(const AText: String): TPoint;
begin
  Result.X := Length(AText) * 8;
  Result.Y := 8;
end;

procedure TFPVectorialDrawer.SimpleTextOut(
  AX, AY: Integer; const AText: String);
begin
  FCanvas.AddText(AX, AY, 0, AText);
end;

end.

