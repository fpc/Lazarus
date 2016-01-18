{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}
unit TADrawerFPCanvas;

{$H+}

interface

{$DEFINE USE_FTFONT}
{$IF (FPC_VERSION = 2) and (FPC_RELEASE <= 4) and defined(WIN64)}
  {$UNDEF USE_FTFONT}
{$ENDIF}

uses
  Classes, FPCanvas, {$IFDEF USE_FTFONT}FTFont,{$ENDIF}
  TAChartUtils, TADrawUtils;

type

  { TFPCanvasDrawer }

  TFPCanvasDrawer = class(TBasicDrawer, IChartDrawer)
  strict private
    FCanvas: TFPCustomCanvas;
    {$IFDEF USE_FTFONT}FFont: TFreeTypeFont;{$ENDIF}

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
      const APoints: array of TPoint; AStartIndex, ANumPts: Integer); override;
    procedure Polyline(
      const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
    procedure PrepareSimplePen(AColor: TChartColor);
    procedure PutPixel(AX, AY: Integer; AColor: TChartColor);
    procedure RadialPie(
      AX1, AY1, AX2, AY2: Integer;
      AStartAngle16Deg, AAngleLength16Deg: Integer);
    procedure Rectangle(const ARect: TRect);
    procedure Rectangle(AX1, AY1, AX2, AY2: Integer);
    procedure ResetFont;
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

{ TFPCanvasDrawer }

procedure TFPCanvasDrawer.AddToFontOrientation(ADelta: Integer);
begin
  EnsureFont;
  {$IFDEF USE_FTFONT}
  FFont.Angle := FFont.Angle + OrientToRad(ADelta);
  {$ELSE}
  Unused(ADelta);
  {$ENDIF}
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
  {$IFDEF USE_FTFONT}FreeAndNil(FFont);{$ENDIF}
  inherited Destroy;
end;

procedure TFPCanvasDrawer.Ellipse(AX1, AY1, AX2, AY2: Integer);
begin
  FCanvas.Ellipse(AX1, AY1, AX2, AY2);
end;

procedure TFPCanvasDrawer.EnsureFont;
begin
  {$IFDEF USE_FTFONT}
  if FFont <> nil then exit;
  FFont := TFreeTypeFont.Create;
  FFont.Resolution := 72;
  FFont.AntiAliased := false;
  FCanvas.Font := FFont;
  {$ENDIF}
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
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
begin
  if (ANumPts = Length(APoints)) and (AStartIndex = 0) then
    FCanvas.Polygon(APoints)
  else
    FCanvas.Polygon(CopyPoints(APoints, AStartIndex, ANumPts));
end;

procedure TFPCanvasDrawer.Polyline(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
begin
  if (ANumPts = Length(APoints)) and (AStartIndex = 0) then
    FCanvas.Polyline(APoints)
  else
    FCanvas.Polyline(CopyPoints(APoints, AStartIndex, ANumPts));
end;

procedure TFPCanvasDrawer.PrepareSimplePen(AColor: TChartColor);
begin
  FCanvas.Pen.FPColor := FChartColorToFPColorFunc(AColor);
  FCanvas.Pen.Width := 1;
  FCanvas.Pen.Style := psSolid;
end;

procedure TFPCanvasDrawer.PutPixel(AX, AY: Integer; AColor: TChartColor);
begin
  FCanvas.Colors[AX, AY] := FChartColorToFPColorFunc(AColor);
end;

procedure TFPCanvasDrawer.RadialPie(
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

procedure TFPCanvasDrawer.Rectangle(AX1, AY1, AX2, AY2: Integer);
begin
  FCanvas.Rectangle(AX1, AY1, AX2, AY2);
end;

procedure TFPCanvasDrawer.Rectangle(const ARect: TRect);
begin
  FCanvas.Rectangle(ARect);
end;

procedure TFPCanvasDrawer.ResetFont;
begin
  FCanvas.Font.Orientation := 0;
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
  FCanvas.Brush.FPColor := FChartColorToFPColorFunc(AColor);
  FCanvas.Brush.Style := AStyle;
end;

procedure TFPCanvasDrawer.SetFont(AFont: TFPCustomFont);
begin
  EnsureFont;
  {$IFDEF USE_FTFONT}
  AssignFPCanvasHelper(FFont, AFont);
  // DoCopyProps performs direct variable assignment, so call SetName by hand.
  FFont.Name := AFont.Name;
  FFont.Angle := OrientToRad(FGetFontOrientationFunc(AFont));
  {$ELSE}
  Unused(AFont);
  {$ENDIF}
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
{$IFDEF USE_FTFONT}
var
  p: TPoint;
{$ENDIF}
begin
  EnsureFont;
  {$IFDEF USE_FTFONT}
  // FreeType uses lower-left instead of upper-left corner as starting position.
  p := RotatePoint(Point(0, FCanvas.GetTextHeight(AText)), -FFont.Angle);
  FCanvas.TextOut(p.X + AX, p.Y + AY , AText);
  {$ELSE}
  Unused(AX, AY);
  Unused(AText);
  {$ENDIF}
end;

end.

