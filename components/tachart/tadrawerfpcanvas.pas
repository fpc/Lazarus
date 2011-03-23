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
unit TADrawerFPCanvas;

{$H+}

interface

uses
  Classes, FPCanvas, FTFont, TAChartUtils, TADrawUtils;

type

  { TFPCanvasDrawer }

  TFPCanvasDrawer = class(TBasicDrawer, IChartDrawer)
  strict private
    FCanvas: TFPCustomCanvas;
    FFont: TFreeTypeFont;

    procedure EnsureFont;
    procedure SetBrush(ABrush: TFPCustomBrush);
    procedure SetFont(AFont: TFPCustomFont);
    procedure SetPen(APen: TFPCustomPen);
  strict protected
    function GetFontAngle: Double; override;
    function SimpleTextExtent(const AText: String): TPoint; override;
    procedure SimpleTextOut(AX, AY: Integer; const AText: String); override;
  public
    constructor Create(ACanvas: TFPCustomCanvas);
    destructor Destroy; override;
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
  SysUtils, TAGeometry;

type
  TFPCanvasHelperCrack = class(TFPCanvasHelper);

procedure AssignFPCanvasHelper(ADest, ASrc: TFPCanvasHelper);
var
  d: TFPCanvasHelperCrack absolute ADest;
begin
  d.DoCopyProps(ASrc);
end;

function CopyArray(
  const A: array of TPoint; AStart, ACount: Integer): TPointArray;
var
  i: Integer;
begin
  SetLength(Result, ACount);
  for i := 0 to ACount - 1 do
    Result[i] := A[i + AStart];
end;

{ TFPCanvasDrawer }

procedure TFPCanvasDrawer.AddToFontOrientation(ADelta: Integer);
begin
  EnsureFont;
  FFont.Angle := FFont.Angle + OrientToRad(ADelta);
end;

procedure TFPCanvasDrawer.ClippingStart(const AClipRect: TRect);
begin
  Unused(AClipRect);
  FCanvas.ClipRect := AClipRect;
  ClippingStart;
end;

procedure TFPCanvasDrawer.ClippingStart;
begin
  // FIXME: FPCanvas.Clipping is broken
  // FCanvas.Clipping := true;
end;

procedure TFPCanvasDrawer.ClippingStop;
begin
  FCanvas.Clipping := false;
end;

constructor TFPCanvasDrawer.Create(ACanvas: TFPCustomCanvas);
begin
  inherited Create;
  FCanvas := ACanvas;
end;

destructor TFPCanvasDrawer.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TFPCanvasDrawer.Ellipse(AX1, AY1, AX2, AY2: Integer);
begin
  FCanvas.Ellipse(AX1, AY1, AX2, AY2);
end;

procedure TFPCanvasDrawer.EnsureFont;
begin
  if FFont <> nil then exit;
  FFont := TFreeTypeFont.Create;
  FFont.Resolution := 72;
  FFont.AntiAliased := false;
  FCanvas.Font := FFont;
end;

procedure TFPCanvasDrawer.FillRect(AX1, AY1, AX2, AY2: Integer);
begin
  // FIXME
  FCanvas.Rectangle(AX1, AY1, AX2, AY2);
end;

function TFPCanvasDrawer.GetBrushColor: TChartColor;
begin
  Result := FPColorToChartColor(FCanvas.Brush.FPColor);
end;

function TFPCanvasDrawer.GetFontAngle: Double;
begin
  Result := 0.0;
end;

procedure TFPCanvasDrawer.Line(AX1, AY1, AX2, AY2: Integer);
begin
  FCanvas.Line(AX1, AY1, AX2, AY2);
end;

procedure TFPCanvasDrawer.Line(const AP1, AP2: TPoint);
begin
  FCanvas.Line(AP1, AP2);
end;

procedure TFPCanvasDrawer.LineTo(AX, AY: Integer);
begin
  FCanvas.LineTo(AX, AY);
end;

procedure TFPCanvasDrawer.MoveTo(AX, AY: Integer);
begin
  FCanvas.MoveTo(AX, AY);
end;

procedure TFPCanvasDrawer.Polygon(
  const APoints: array of TPoint; AStartIndex: Integer; ANumPts: Integer);
begin
  if (ANumPts < 0) and (AStartIndex = 0) then
    FCanvas.Polygon(APoints)
  else
    FCanvas.Polygon(CopyArray(APoints, AStartIndex, ANumPts));
end;

procedure TFPCanvasDrawer.Polyline(
  const APoints: array of TPoint;
  AStartIndex: Integer; ANumPts: Integer; AEndPoint: Boolean);
begin
  Unused(AEndPoint);
  if (ANumPts < 0) and (AStartIndex = 0) then
    FCanvas.Polyline(APoints)
  else
    FCanvas.Polyline(CopyArray(APoints, AStartIndex, ANumPts));
end;

procedure TFPCanvasDrawer.PrepareSimplePen(AColor: TChartColor);
begin
  FCanvas.Pen.FPColor := FChartColorToFPColorFunc(AColor);
  FCanvas.Pen.Width := 1;
  FCanvas.Pen.Style := psSolid;
end;

procedure TFPCanvasDrawer.RadialPie(
  AX1, AY1, AX2, AY2: Integer; AStartAngle16Deg, AAngleLength16Deg: Integer);
begin
  Unused(AX1, AY1);
  Unused(AX2, AY2);
  Unused(AStartAngle16Deg, AAngleLength16Deg);
  raise EChartError.Create('TOpenGLDrawer.RadialPie not implemented');
end;

procedure TFPCanvasDrawer.Rectangle(AX1, AY1, AX2, AY2: Integer);
begin
  FCanvas.Rectangle(AX1, AY1, AX2, AY2);
end;

procedure TFPCanvasDrawer.Rectangle(const ARect: TRect);
begin
  FCanvas.Rectangle(ARect);
end;

procedure TFPCanvasDrawer.SetBrush(ABrush: TFPCustomBrush);
begin
  AssignFPCanvasHelper(FCanvas.Brush, ABrush);
end;

procedure TFPCanvasDrawer.SetBrushColor(AColor: TChartColor);
begin
  FCanvas.Brush.FPColor := FChartColorToFPColorFunc(AColor);
end;

procedure TFPCanvasDrawer.SetBrushParams(
  AStyle: TFPBrushStyle; AColor: TChartColor);
begin
  FCanvas.Brush.Style := AStyle;
  FCanvas.Brush.FPColor := FChartColorToFPColorFunc(AColor);
end;

procedure TFPCanvasDrawer.SetFont(AFont: TFPCustomFont);
begin
  EnsureFont;
  AssignFPCanvasHelper(FFont, AFont);
  // DoCopyProps performs direct variable assignment, so call SetName by hand.
  FFont.Name := AFont.Name;
  FFont.Angle := OrientToRad(FGetFontOrientationFunc(AFont));
end;

procedure TFPCanvasDrawer.SetPen(APen: TFPCustomPen);
begin
  AssignFPCanvasHelper(FCanvas.Pen, APen);
end;

procedure TFPCanvasDrawer.SetPenParams(AStyle: TFPPenStyle; AColor: TChartColor);
begin
  FCanvas.Pen.Style := AStyle;
  FCanvas.Pen.FPColor := FChartColorToFPColorFunc(AColor);
end;

function TFPCanvasDrawer.SimpleTextExtent(const AText: String): TPoint;
begin
  EnsureFont;
  FCanvas.GetTextSize(AText, Result.X, Result.Y);
end;

procedure TFPCanvasDrawer.SimpleTextOut(AX, AY: Integer; const AText: String);
var
  p: TPoint;
begin
  EnsureFont;
  // FreeType uses lower-left instead of upper-left corner as starting position.
  p := RotatePoint(Point(0, FCanvas.GetTextHeight(AText)), -FFont.Angle);
  FCanvas.TextOut(p.X + AX, p.Y + AY , AText);
end;

end.

