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
  BGRABitmap, BGRABitmapTypes, BGRACanvas, Classes, FPCanvas,
  TAChartUtils, TADrawUtils;

type

  TBGRABitmapDrawer = class(TBasicDrawer, IChartDrawer)
  strict private
    FBitmap: TBGRABitmap;

    function Canvas: TBGRACanvas; inline;
    procedure SetBrush(ABrush: TFPCustomBrush);
    procedure SetFont(AFont: TFPCustomFont);
    procedure SetPen(APen: TFPCustomPen);
  strict protected
    function GetFontAngle: Double; override;
    procedure SetAntialiasingMode(AValue: TChartAntialiasingMode);
    function SimpleTextExtent(const AText: String): TPoint; override;
    procedure SimpleTextOut(AX, AY: Integer; const AText: String); override;
  public
    constructor Create(ABitmap: TBGRABitmap);
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
  Graphics, TAGeometry;

{ TBGRABitmapDrawer }

procedure TBGRABitmapDrawer.AddToFontOrientation(ADelta: Integer);
begin
  with Canvas.Font do
    Orientation := Orientation + ADelta;
end;

function TBGRABitmapDrawer.Canvas: TBGRACanvas;
begin
  Result := FBitmap.CanvasBGRA;
end;

procedure TBGRABitmapDrawer.ClippingStart(const AClipRect: TRect);
begin
  Canvas.ClipRect := AClipRect;
  ClippingStart;
end;

procedure TBGRABitmapDrawer.ClippingStart;
begin
  Canvas.Clipping := true;
end;

procedure TBGRABitmapDrawer.ClippingStop;
begin
  Canvas.Clipping := false;
end;

constructor TBGRABitmapDrawer.Create(ABitmap: TBGRABitmap);
begin
  FBitmap := ABitmap;
end;

procedure TBGRABitmapDrawer.Ellipse(AX1, AY1, AX2, AY2: Integer);
begin
  Canvas.Ellipse(AX1, AY1, AX2, AY2);
end;

procedure TBGRABitmapDrawer.FillRect(AX1, AY1, AX2, AY2: Integer);
begin
  Canvas.FillRect(AX1, AY1, AX2, AY2);
end;

function TBGRABitmapDrawer.GetBrushColor: TChartColor;
begin
  Result := TChartColor(Canvas.Brush.Color);
end;

function TBGRABitmapDrawer.GetFontAngle: Double;
begin
  Result := 0.0;
end;

procedure TBGRABitmapDrawer.Line(AX1, AY1, AX2, AY2: Integer);
begin
  Canvas.MoveTo(AX1, AY1);
  Canvas.LineTo(AX2, AY2);
end;

procedure TBGRABitmapDrawer.Line(const AP1, AP2: TPoint);
begin
  Canvas.MoveTo(AP1);
  Canvas.LineTo(AP2);
end;

procedure TBGRABitmapDrawer.LineTo(AX, AY: Integer);
begin
  Canvas.LineTo(AX, AY);
end;

procedure TBGRABitmapDrawer.MoveTo(AX, AY: Integer);
begin
  Canvas.MoveTo(AX, AY);
end;

procedure TBGRABitmapDrawer.Polygon(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
begin
  Canvas.Polygon(APoints, false, AStartIndex, ANumPts);
end;

procedure TBGRABitmapDrawer.Polyline(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
begin
  Canvas.Polyline(APoints, AStartIndex, ANumPts);
end;

procedure TBGRABitmapDrawer.PrepareSimplePen(AColor: TChartColor);
begin
  Canvas.Pen.Color := AColor;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 1;
end;

procedure TBGRABitmapDrawer.RadialPie(
  AX1, AY1, AX2, AY2: Integer; AStartAngle16Deg, AAngleLength16Deg: Integer);
begin
  Canvas.RadialPie(
    AX1, AY1, AX2, AY2, AStartAngle16Deg, AAngleLength16Deg);
end;

procedure TBGRABitmapDrawer.Rectangle(AX1, AY1, AX2, AY2: Integer);
begin
  Canvas.Rectangle(AX1, AY1, AX2, AY2);
end;

procedure TBGRABitmapDrawer.Rectangle(const ARect: TRect);
begin
  with ARect do
    Rectangle(Left, Top, Right, Bottom)
end;

procedure TBGRABitmapDrawer.SetAntialiasingMode(AValue: TChartAntialiasingMode);
begin
  Canvas.AntialiasingMode := TAntialiasingMode(AValue);
  Canvas.Font.Antialiasing := AValue = TADrawUtils.amOn;
end;

procedure TBGRABitmapDrawer.SetBrush(ABrush: TFPCustomBrush);
begin
  Canvas.Brush.BGRAColor := FPColorToBGRA(ABrush.FPColor);
  Canvas.Brush.Style := ABrush.Style;
end;

procedure TBGRABitmapDrawer.SetBrushColor(AColor: TChartColor);
begin
  Canvas.Brush.Color := AColor;
end;

procedure TBGRABitmapDrawer.SetBrushParams(
  AStyle: TFPBrushStyle; AColor: TChartColor);
begin
  Canvas.Brush.Color := AColor;
  Canvas.Brush.Style := AStyle;
end;

procedure TBGRABitmapDrawer.SetFont(AFont: TFPCustomFont);
begin
  Canvas.Font.Name := AFont.Name;
  Canvas.Font.Height := AFont.Size * 96 div 72;
  Canvas.Font.Orientation := FGetFontOrientationFunc(AFont);
  Canvas.Font.BGRAColor := FPColorToBGRA(AFont.FPColor);
  // TODO: FontStyle
end;

procedure TBGRABitmapDrawer.SetPen(APen: TFPCustomPen);
begin
  Canvas.Pen.Style := APen.Style;
  Canvas.Pen.Width := APen.Width;
  // TODO: JoinStyle
  Canvas.Pen.BGRAColor := FPColorToBGRA(APen.FPColor);
end;

procedure TBGRABitmapDrawer.SetPenParams(
  AStyle: TFPPenStyle; AColor: TChartColor);
begin
  Canvas.Pen.Style := AStyle;
  Canvas.Pen.Color := AColor;
end;

function TBGRABitmapDrawer.SimpleTextExtent(const AText: String): TPoint;
begin
  Result := Canvas.TextExtent(AText);
end;

procedure TBGRABitmapDrawer.SimpleTextOut(AX, AY: Integer; const AText: String);
begin
  Canvas.TextOut(AX, AY, AText);
end;

initialization
  // Suppress incorrect "TAGeometry is unused" hint
  Unused(DoublePoint(0, 0));

end.

