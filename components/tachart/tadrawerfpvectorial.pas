{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}
unit TADrawerFPVectorial;

{$H+}

interface

uses
  Classes, FPCanvas, FPImage, FPVectorial, TAChartUtils, TADrawUtils;

type

  { TFPVectorialDrawer }

  TFPVectorialDrawer = class(TBasicDrawer, IChartDrawer)
  strict private
    FBoundingBox: TRect;
    FBrushColor: TFPColor;
    FBrushStyle: TFPBrushStyle;
    FCanvas: TvVectorialPage;
    FFontSize: Integer;
    FPenColor: TFPColor;
    FPenStyle: TFPPenStyle;
    FPenWidth: Integer;

    procedure AddLine(AX, AY: Integer);
    procedure ApplyBrush;
    procedure ApplyPen;
    function InvertY(AY: Integer): Integer; inline;
  strict protected
    function GetFontAngle: Double; override;
    procedure SetBrush(ABrush: TFPCustomBrush);
    procedure SetFont(AFont: TFPCustomFont);
    procedure SetPen(APen: TFPCustomPen);
    function SimpleTextExtent(const AText: String): TPoint; override;
    procedure SimpleTextOut(AX, AY: Integer; const AText: String); override;
  public
    constructor Create(ACanvas: TvVectorialPage);
  public
    procedure AddToFontOrientation(ADelta: Integer);
    procedure ClippingStart;
    procedure ClippingStart(const AClipRect: TRect);
    procedure ClippingStop;
    procedure DrawingBegin(const ABoundingBox: TRect); override;
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
  Math, TAGeometry;

{ TFPVectorialDrawer }

procedure TFPVectorialDrawer.AddLine(AX, AY: Integer);
begin
  FCanvas.AddLineToPath(AX, InvertY(AY));
end;

procedure TFPVectorialDrawer.AddToFontOrientation(ADelta: Integer);
begin
  // Not implemented.
  Unused(ADelta);
end;

procedure TFPVectorialDrawer.ApplyBrush;
begin
  FCanvas.SetBrushColor(FBrushColor);
  FCanvas.SetBrushStyle(FBrushStyle);
end;

procedure TFPVectorialDrawer.ApplyPen;
begin
  FCanvas.SetPenColor(FPenColor);
  FCanvas.SetPenStyle(FPenStyle);
  FCanvas.SetPenWidth(FPenWidth);
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

constructor TFPVectorialDrawer.Create(ACanvas: TvVectorialPage);
begin
  inherited Create;
  FCanvas := ACanvas;
end;

procedure TFPVectorialDrawer.DrawingBegin(const ABoundingBox: TRect);
begin
  FBoundingBox := ABoundingBox;
end;

procedure TFPVectorialDrawer.Ellipse(AX1, AY1, AX2, AY2: Integer);
var
  e: TEllipse;
begin
  e.InitBoundingBox(AX1, AY1, AX2, AY2);
  FCanvas.AddEllipse(e.FC.X, InvertY(Round(e.FC.Y)), e.FR.x, e.FR.Y, 0.0);
end;

procedure TFPVectorialDrawer.FillRect(AX1, AY1, AX2, AY2: Integer);
begin
  MoveTo(AX1, AY1);
  AddLine(AX2, AY1);
  AddLine(AX2, AY2);
  AddLine(AX1, AY2);
  AddLine(AX1, AY1);
  FCanvas.SetBrushStyle(bsClear);
  FCanvas.SetPenColor(FPenColor);
  FCanvas.EndPath();
end;

function TFPVectorialDrawer.GetBrushColor: TChartColor;
begin
  Result := FPColorToChartColor(FBrushColor);
end;

function TFPVectorialDrawer.GetFontAngle: Double;
begin
  Result := 0.0;
end;

function TFPVectorialDrawer.InvertY(AY: Integer): Integer;
begin
  with FBoundingBox do
    Result := Bottom - Top - AY;
end;

procedure TFPVectorialDrawer.Line(AX1, AY1, AX2, AY2: Integer);
begin
  FCanvas.StartPath(AX1, InvertY(AY1));
  AddLine(AX2, AY2);
  FCanvas.SetPenColor(FPenColor);
  FCanvas.EndPath();
end;

procedure TFPVectorialDrawer.Line(const AP1, AP2: TPoint);
begin
  Line(AP1.X, AP1.Y, AP2.X, AP2.Y);
end;

procedure TFPVectorialDrawer.LineTo(AX, AY: Integer);
begin
  FCanvas.AddLineToPath(AX, InvertY(AY), FPenColor);
end;

procedure TFPVectorialDrawer.MoveTo(AX, AY: Integer);
begin
  FCanvas.EndPath();
  FCanvas.StartPath(AX, InvertY(AY));
end;

procedure TFPVectorialDrawer.Polygon(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
var
  i: Integer;
begin
  if ANumPts <= 0 then exit;
  MoveTo(APoints[AStartIndex]);
  for i := 1 to ANumPts - 1 do
    with APoints[i + AStartIndex] do
      AddLine(X, Y);
  ApplyBrush;
  ApplyPen;
  FCanvas.EndPath();
end;

procedure TFPVectorialDrawer.Polyline(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
var
  i: Integer;
begin
  if ANumPts <= 0 then exit;
  MoveTo(APoints[AStartIndex]);
  for i := 1 to ANumPts - 1 do
    with APoints[i + AStartIndex] do
      AddLine(X, Y);
  ApplyPen;
  FCanvas.EndPath();
end;

procedure TFPVectorialDrawer.PrepareSimplePen(AColor: TChartColor);
begin
  FPenColor := FChartColorToFPColorFunc(AColor);
  FPenStyle := psSolid;
  FPenWidth := 1;
end;

procedure TFPVectorialDrawer.RadialPie(
  AX1, AY1, AX2, AY2: Integer; AStartAngle16Deg, AAngleLength16Deg: Integer);
var
  e: TEllipse;
  p: TPointArray;
begin
  e.InitBoundingBox(AX1, AY1, AX2, AY2);
  p := e.TesselateRadialPie(
    Deg16ToRad(AStartAngle16Deg), Deg16ToRad(AAngleLength16Deg), 4);
  Polygon(p, 0, Length(p));
end;

procedure TFPVectorialDrawer.Rectangle(AX1, AY1, AX2, AY2: Integer);
begin
  MoveTo(AX1, AY1);
  AddLine(AX2, AY1);
  AddLine(AX2, AY2);
  AddLine(AX1, AY2);
  AddLine(AX1, AY1);
  ApplyBrush;
  ApplyPen;
  FCanvas.EndPath();
end;

procedure TFPVectorialDrawer.Rectangle(const ARect: TRect);
begin
  with ARect do
    Rectangle(Left, Top, Right, Bottom);
end;

procedure TFPVectorialDrawer.SetBrush(ABrush: TFPCustomBrush);
begin
  FBrushColor := ABrush.FPColor;
  FBrushStyle := ABrush.Style;
end;

procedure TFPVectorialDrawer.SetBrushColor(AColor: TChartColor);
begin
  FBrushColor := FChartColorToFPColorFunc(AColor);
end;

procedure TFPVectorialDrawer.SetBrushParams(
  AStyle: TFPBrushStyle; AColor: TChartColor);
begin
  SetBrushColor(AColor);
  FBrushStyle := AStyle;
end;

procedure TFPVectorialDrawer.SetFont(AFont: TFPCustomFont);
begin
  FFontSize := IfThen(AFont.Size = 0, 10, AFont.Size);
end;

procedure TFPVectorialDrawer.SetPen(APen: TFPCustomPen);
begin
  FPenColor := APen.FPColor;
  FPenStyle := APen.Style;
  FPenWidth := APen.Width;
end;

procedure TFPVectorialDrawer.SetPenParams(
  AStyle: TFPPenStyle; AColor: TChartColor);
begin
  FPenStyle := AStyle;
  FPenColor := FChartColorToFPColorFunc(AColor);
end;

function TFPVectorialDrawer.SimpleTextExtent(const AText: String): TPoint;
begin
  Result.X := FFontSize * Length(AText) * 2 div 3;
  Result.Y := FFontSize;
end;

procedure TFPVectorialDrawer.SimpleTextOut(
  AX, AY: Integer; const AText: String);
begin
  // FPVectorial uses lower-left instead of upper-left corner as text start.
  FCanvas.AddText(AX, InvertY(AY) - FFontSize, 0, AText);
end;

end.

