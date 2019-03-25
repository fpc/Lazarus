{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}
unit TADrawerAggPas;

{$H+}

interface

uses
  Classes, FPCanvas, FPImage, Agg_LCL, TAChartUtils, TADrawUtils;

type

  { TAggPasDrawer }

  TAggPasDrawer = class(TBasicDrawer, IChartDrawer)
  strict private
    function ApplyTransparency(AColor: TFPColor): TFPColor;
    procedure SetBrush(ABrush: TFPCustomBrush);
    procedure SetFont(AFont: TFPCustomFont);
    procedure SetPen(APen: TFPCustomPen);
  strict protected
    FCanvas: TAggLCLCanvas;
    function SimpleTextExtent(const AText: String): TPoint; override;
    procedure SimpleTextOut(AX, AY: Integer; const AText: String); override;
  public
    constructor Create(ACanvas: TAggLCLCanvas);
  public
    procedure AddToFontOrientation(ADelta: Integer);
    procedure ClippingStart;
    procedure ClippingStart(const AClipRect: TRect);
    procedure ClippingStop;
    procedure Ellipse(AX1, AY1, AX2, AY2: Integer);
    procedure FillRect(AX1, AY1, AX2, AY2: Integer);
    function GetBrushColor: TChartColor;
    function GetFontAngle: Double; override;
    function GetFontColor: TFPColor; override;
    function GetFontName: String; override;
    function GetFontSize: Integer; override;
    function GetFontStyle: TChartFontStyles; override;
    function GetPenColor: TChartColor;
    procedure Line(AX1, AY1, AX2, AY2: Integer);
    procedure Line(const AP1, AP2: TPoint);
    procedure LineTo(AX, AY: Integer); override;
    procedure MoveTo(AX, AY: Integer); override;
    procedure Polygon(
      const APoints: array of TPoint; AStartIndex, ANumPts: Integer); override;
    procedure Polyline(
      const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
    procedure PrepareSimplePen(AColor: TChartColor);
    procedure PutImage(AX, AY: Integer; AImage: TFPCustomImage); override;
    procedure RadialPie(
      AX1, AY1, AX2, AY2: Integer;
      AStartAngle16Deg, AAngleLength16Deg: Integer);
    procedure Rectangle(const ARect: TRect);
    procedure Rectangle(AX1, AY1, AX2, AY2: Integer);
    procedure ResetFont;
    procedure SetBrushColor(AColor: TChartColor);
    procedure SetBrushParams(AStyle: TFPBrushStyle; AColor: TChartColor);
    procedure SetPenColor(AColor: TChartColor);
    procedure SetPenParams(AStyle: TFPPenStyle; AColor: TChartColor);
  end;

implementation

uses
  Math, TAGeometry;

{ TAggPasDrawer }

procedure TAggPasDrawer.AddToFontOrientation(ADelta: Integer);
begin
  with FCanvas.Font do
    AggAngle := AggAngle + OrientToRad(ADelta);
end;

function TAggPasDrawer.ApplyTransparency(AColor: TFPColor): TFPColor;
begin
  Result := AColor;
  Result.alpha := (255 - FTransparency) shl 8;
end;

procedure TAggPasDrawer.ClippingStart(const AClipRect: TRect);
begin
  FCanvas.ClipRect := AClipRect;
  FCanvas.Clipping := true;
end;

procedure TAggPasDrawer.ClippingStart;
begin
  FCanvas.Clipping := true;
end;

procedure TAggPasDrawer.ClippingStop;
begin
  FCanvas.Clipping := false;
end;

constructor TAggPasDrawer.Create(ACanvas: TAggLCLCanvas);
begin
  inherited Create;
  FCanvas := ACanvas;
end;

procedure TAggPasDrawer.Ellipse(AX1, AY1, AX2, AY2: Integer);
begin
  FCanvas.Ellipse(AX1, AY1, AX2, AY2);
end;

procedure TAggPasDrawer.FillRect(AX1, AY1, AX2, AY2: Integer);
begin
  FCanvas.FillRect(AX1, AY1, AX2, AY2);
end;

function TAggPasDrawer.GetBrushColor: TChartColor;
begin
  Result := FCanvas.Brush.Color;
end;

function TAggPasDrawer.GetFontAngle: Double;
begin
  Result := FCanvas.Font.AggAngle;
end;

function TAggPasDrawer.GetFontColor: TFPColor;
begin
  Result.Red := FCanvas.Font.AggColor.r shl 8;
  Result.Green := FCanvas.Font.AggColor.g shl 8;
  Result.Blue := FCanvas.Font.AggColor.b shl 8;
end;

function TAggPasDrawer.GetFontName: String;
begin
  Result := FCanvas.Font.Name;
end;

function TAggPasDrawer.GetFontSize: Integer;
begin
  if FCanvas.Font.AggHeight = 0 then
    Result := DEFAULT_FONT_SIZE else
    Result := round(FCanvas.Font.AggHeight * 72 / 96);
end;

function TAggPasDrawer.GetFontStyle: TChartFontStyles;
begin
  Result := [];
  if FCanvas.Font.Bold then Include(Result, cfsBold);
  if FCanvas.Font.Italic then Include(Result, cfsItalic);
  if FCanvas.Font.Underline then Include(Result, cfsUnderline);
  if FCanvas.Font.StrikeThrough then Include(Result, cfsStrikeout);
end;

function TAggPasDrawer.GetPenColor: TChartColor;
begin
  Result := FCanvas.Pen.Color;
end;

procedure TAggPasDrawer.Line(AX1, AY1, AX2, AY2: Integer);
begin
  FCanvas.Line(AX1, AY1, AX2, AY2);
end;

procedure TAggPasDrawer.Line(const AP1, AP2: TPoint);
begin
  FCanvas.Line(AP1, AP2);
end;

procedure TAggPasDrawer.LineTo(AX, AY: Integer);
begin
  FCanvas.LineTo(AX, AY);
end;

procedure TAggPasDrawer.MoveTo(AX, AY: Integer);
begin
  FCanvas.MoveTo(AX, AY);
end;

procedure TAggPasDrawer.Polygon(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
begin
  if ANumPts <= 0 then exit;
  FCanvas.Polygon(APoints, false, AStartIndex, ANumPts);
  FCanvas.Polyline(APoints, AStartIndex, ANumPts);
  FCanvas.Line(APoints[ANumPts - 1], APoints[0])
end;

procedure TAggPasDrawer.Polyline(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
begin
  FCanvas.Polyline(APoints, AStartIndex, ANumPts);
end;

procedure TAggPasDrawer.PrepareSimplePen(AColor: TChartColor);
begin
  with FCanvas.Pen do begin
    FPColor := ApplyTransparency(ChartColorToFPColor(ColorOrMono(AColor)));
    Style := psSolid;
    Mode := pmCopy;
    Width := 1;
  end;
end;

procedure TAggPasDrawer.PutImage(AX, AY: Integer; AImage: TFPCustomImage);
var
  x, y: Integer;
begin
  // FCanvas.Draw ignores alpha values.
  for y := 0 to AImage.Height - 1 do
    for x := 0 to AImage.Width - 1 do
      if AImage[x, y].alpha > 0 then
        FCanvas.Colors[AX + x, AY + y] := AImage[x, y];
end;

procedure TAggPasDrawer.RadialPie(
  AX1, AY1, AX2, AY2: Integer;
  AStartAngle16Deg, AAngleLength16Deg: Integer);
begin
  FCanvas.RadialPie(
    AX1, AY1, AX2, AY2, AStartAngle16Deg, AAngleLength16Deg);
end;

procedure TAggPasDrawer.Rectangle(AX1, AY1, AX2, AY2: Integer);
begin
  FCanvas.Rectangle(AX1, AY1, AX2, AY2);
end;

procedure TAggPasDrawer.Rectangle(const ARect: TRect);
begin
  FCanvas.Rectangle(ARect);
end;

procedure TAggPasDrawer.ResetFont;
begin
  FCanvas.Font.Orientation := 0;
end;

procedure TAggPasDrawer.SetBrush(ABrush: TFPCustomBrush);
begin
  with FCanvas.Brush do begin
    Style := ABrush.Style;
    Image := ABrush.Image;
    Pattern := ABrush.Pattern;
    FPColor := ApplyTransparency(FPColorOrMono(ABrush.FPColor));
  end;
end;

procedure TAggPasDrawer.SetBrushColor(AColor: TChartColor);
begin
  FCanvas.Brush.FPColor :=
    ApplyTransparency(ChartColorToFPColor(ColorOrMono(AColor)));
end;

procedure TAggPasDrawer.SetBrushParams(
  AStyle: TFPBrushStyle; AColor: TChartColor);
begin
  SetBrushColor(AColor);
  FCanvas.Brush.Style := AStyle;
end;

procedure TAggPasDrawer.SetFont(AFont: TFPCustomFont);
var
  f: TAggLCLFont;
  fontSize: Integer;
begin
  f := FCanvas.Font;
  fontSize := IfThen(AFont.Size = 0, DEFAULT_FONT_SIZE, AFont.Size);
  // This should be: FCanvas.Font.DoCopyProps(AFont);
  f.LoadFromFile(
    AFont.Name, f.SizeToAggHeight(fontSize), AFont.Bold, AFont.Italic);
  f.FPColor := ApplyTransparency(FPColorOrMono(AFont.FPColor));
  f.AggAngle := -OrientToRad(FGetFontOrientationFunc(AFont));
end;

type
  TAggLCLPenCrack = class(TAggLCLPen);

procedure TAggPasDrawer.SetPen(APen: TFPCustomPen);
begin
  TAggLCLPenCrack(FCanvas.Pen).DoCopyProps(APen);
  FCanvas.Pen.FPColor := ApplyTransparency(FPColorOrMono(APen.FPColor));
end;

procedure TAggPasDrawer.SetPenColor(AColor: TChartColor);
begin
  FCanvas.Pen.FPColor :=
    ApplyTransparency(ChartColorToFPColor(ColorOrMono(AColor)));
end;

procedure TAggPasDrawer.SetPenParams(AStyle: TFPPenStyle; AColor: TChartColor);
begin
  FCanvas.Pen.Style := AStyle;
  FCanvas.Pen.FPColor :=
    ApplyTransparency(ChartColorToFPColor(ColorOrMono(AColor)));
end;

function TAggPasDrawer.SimpleTextExtent(const AText: String): TPoint;
begin
  Result := FCanvas.TextExtent(AText);
end;

procedure TAggPasDrawer.SimpleTextOut(AX, AY: Integer; const AText: String);
var
  h: Integer;
  p: TPoint;
begin
  h := SimpleTextExtent('Tg').y;
  p := RotatePoint(Point(0, h * 9 div 10), FCanvas.Font.AggAngle);
    // * 9/10 fits to fit the text into bounding box
  FCanvas.TextOut(AX + p.X, AY + p.Y - h, AText);
    // -h to avoid rotated text drifting away
end;

initialization
  // Suppress incorrect "TAGeometry is unused" hint
  Unused(DoublePoint(0, 0));

end.

