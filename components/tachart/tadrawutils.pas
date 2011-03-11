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

unit TADrawUtils;

{$mode objfpc}

interface

uses
  Classes, Graphics, FPCanvas, SysUtils, Types;

type
  TChartColor = -$7FFFFFFF-1..$7FFFFFFF;

const
  Colors: array [1..15] of TColor = (
    clRed, clGreen, clYellow, clBlue, clWhite, clGray, clFuchsia,
    clTeal, clNavy, clMaroon, clLime, clOlive, clPurple, clSilver, clAqua);

type

  ISimpleTextOut = interface
    procedure SimpleTextOut(AX, AY: Integer; const AText: String);
    function SimpleTextExtent(const AText: String): TPoint;
    function GetFontAngle: Double;
  end;

  { TChartTextOut }

  TChartTextOut = class
  strict private
    FAlignment: TAlignment;
    FPos: TPoint;
    FSimpleTextOut: ISimpleTextOut;
    FText1: String;
    FText2: TStrings;
    FWidth: Integer;

    procedure DoTextOutList;
    procedure DoTextOutString;
  public
    constructor Create(ASimpleTextOut: ISimpleTextOut);
  public
    function Alignment(AAlignment: TAlignment): TChartTextOut;
    procedure Done;
    function Pos(AX, AY: Integer): TChartTextOut;
    function Pos(const APos: TPoint): TChartTextOut;
    function Text(const AText: String): TChartTextOut;
    function Text(const AText: TStrings): TChartTextOut;
    function Width(AWidth: Integer): TChartTextOut;
  end;

  { IChartDrawer }

  IChartDrawer = interface
    procedure AddToFontOrientation(ADelta: Integer);
    procedure ClippingStart(const AClipRect: TRect);
    procedure ClippingStart;
    procedure ClippingStop;
    procedure DrawLineDepth(AX1, AY1, AX2, AY2, ADepth: Integer);
    procedure DrawLineDepth(const AP1, AP2: TPoint; ADepth: Integer);
    procedure Ellipse(AX1, AY1, AX2, AY2: Integer);
    procedure FillRect(AX1, AY1, AX2, AY2: Integer);
    function GetBrushColor: TChartColor;
    function GetCanvas: TCanvas;
    function HasCanvas: Boolean;
    procedure Line(AX1, AY1, AX2, AY2: Integer);
    procedure Line(const AP1, AP2: TPoint);
    procedure LineTo(AX, AY: Integer);
    procedure LineTo(const AP: TPoint);
    procedure MoveTo(AX, AY: Integer);
    procedure MoveTo(const AP: TPoint);
    procedure Polygon(
      const APoints: array of TPoint;
      AStartIndex: Integer = 0; ANumPts: Integer = -1);
    procedure Polyline(
      const APoints: array of TPoint; AStartIndex: Integer = 0;
      ANumPts: Integer = -1; AEndPoint: Boolean = false);
    procedure PrepareSimplePen(AColor: TChartColor);
    procedure RadialPie(
      AX1, AY1, AX2, AY2: Integer;
      AStartAngle16Deg, AAngleLength16Deg: Integer);
    procedure Rectangle(const ARect: TRect);
    procedure Rectangle(AX1, AY1, AX2, AY2: Integer);
    function Scale(ADistance: Integer): Integer;
    procedure SetBrushColor(AColor: TChartColor);
    procedure SetBrush(ABrush: TFPCustomBrush);
    procedure SetBrushParams(AStyle: TFPBrushStyle; AColor: TChartColor);
    procedure SetFont(AValue: TFPCustomFont);
    procedure SetPen(APen: TFPCustomPen);
    procedure SetPenParams(AStyle: TFPPenStyle; AColor: TChartColor);
    function TextExtent(const AText: String): TPoint;
    function TextExtent(AText: TStrings): TPoint;
    function TextOut: TChartTextOut;

    property Brush: TFPCustomBrush write SetBrush;
    property BrushColor: TChartColor read GetBrushColor write SetBrushColor;
    property Canvas: TCanvas read GetCanvas;
    property Font: TFPCustomFont write SetFont;
    property Pen: TFPCustomPen write SetPen;
  end;

  { TFPCanvasDrawer }

  TFPCanvasDrawer = class(TInterfacedObject, ISimpleTextOut)
  strict protected
    function GetFontAngle: Double; virtual; abstract;
    function SimpleTextExtent(const AText: String): TPoint; virtual; abstract;
    procedure SimpleTextOut(AX, AY: Integer; const AText: String); virtual; abstract;
  public
    procedure DrawLineDepth(AX1, AY1, AX2, AY2, ADepth: Integer);
    procedure DrawLineDepth(const AP1, AP2: TPoint; ADepth: Integer);
    procedure LineTo(AX, AY: Integer); virtual; abstract;
    procedure LineTo(const AP: TPoint);
    procedure MoveTo(AX, AY: Integer); virtual; abstract;
    procedure MoveTo(const AP: TPoint);
    procedure Polygon(
      const APoints: array of TPoint;
      AStartIndex: Integer = 0; ANumPts: Integer = -1); virtual; abstract;
    function Scale(ADistance: Integer): Integer; virtual;
    function TextExtent(const AText: String): TPoint;
    function TextExtent(AText: TStrings): TPoint;
    function TextOut: TChartTextOut;
  end;

  { TCanvasDrawer }

  TCanvasDrawer = class(TFPCanvasDrawer, IChartDrawer, ISimpleTextOut)
  private
    FCanvas: TCanvas;
    procedure SetBrush(ABrush: TFPCustomBrush);
    procedure SetFont(AFont: TFPCustomFont);
    procedure SetPen(APen: TFPCustomPen);
  strict protected
    function GetFontAngle: Double; override;
    function SimpleTextExtent(const AText: String): TPoint; override;
    procedure SimpleTextOut(AX, AY: Integer; const AText: String); override;
  public
    procedure AddToFontOrientation(ADelta: Integer);
    procedure ClippingStart;
    procedure ClippingStart(const AClipRect: TRect);
    procedure ClippingStop;
    constructor Create(ACanvas: TCanvas);
    procedure Ellipse(AX1, AY1, AX2, AY2: Integer);
    procedure FillRect(AX1, AY1, AX2, AY2: Integer);
    function GetBrushColor: TChartColor;
    function GetCanvas: TCanvas;
    function HasCanvas: Boolean;
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

procedure PrepareXorPen(ACanvas: TCanvas);

implementation

uses
  Math, TAChartUtils, TAGeometry;

const
  LINE_INTERVAL = 2;

procedure PrepareXorPen(ACanvas: TCanvas);
begin
  with ACanvas do begin
    Brush.Style := bsClear;
    Pen.Style := psSolid;
    Pen.Mode := pmXor;
    Pen.Color := clWhite;
    Pen.Width := 1;
  end;
end;

{ TChartTextOut }

function TChartTextOut.Alignment(AAlignment: TAlignment): TChartTextOut;
begin
  FAlignment := AAlignment;
  Result := Self;
end;

constructor TChartTextOut.Create(ASimpleTextOut: ISimpleTextOut);
begin
  FSimpleTextOut := ASimpleTextOut;
  FAlignment := taLeftJustify;
end;

procedure TChartTextOut.Done;
begin
  if FText2 = nil then
    DoTextOutString
  else
    DoTextOutList;
  Free;
end;

procedure TChartTextOut.DoTextOutList;
var
  i: Integer;
  a: Double;
  lineExtent, p: TPoint;
begin
  a := -FSimpleTextOut.GetFontAngle;
  for i := 0 to FText2.Count - 1 do begin
    lineExtent := FSimpleTextOut.SimpleTextExtent(FText2[i]);
    p := FPos;
    case FAlignment of
      taCenter: p += RotatePoint(Point((FWidth - lineExtent.X) div 2, 0), a);
      taRightJustify: p += RotatePoint(Point(FWidth - lineExtent.X, 0), a);
    end;
    FSimpleTextOut.SimpleTextOut(p.X, p.Y, FText2[i]);
    FPos += RotatePoint(Point(0, lineExtent.Y + LINE_INTERVAL), a);
  end;
end;

procedure TChartTextOut.DoTextOutString;
begin
  if System.Pos(LineEnding, FText1) = 0 then begin
    FSimpleTextOut.SimpleTextOut(FPos.X, FPos.Y, FText1);
    exit;
  end;
  FText2 := TStringList.Create;
  try
    FText2.Text := FText1;
    DoTextOutList;
  finally
    FText2.Free;
  end;
end;

function TChartTextOut.Pos(AX, AY: Integer): TChartTextOut;
begin
  FPos := Point(AX, AY);
  Result := Self;
end;

function TChartTextOut.Pos(const APos: TPoint): TChartTextOut;
begin
  FPos := APos;
  Result := Self;
end;

function TChartTextOut.Text(const AText: String): TChartTextOut;
begin
  FText1 := AText;
  Result := Self;
end;

function TChartTextOut.Text(const AText: TStrings): TChartTextOut;
begin
  FText2 := AText;
  Result := Self;
end;

function TChartTextOut.Width(AWidth: Integer): TChartTextOut;
begin
  FWidth := AWidth;
  Result := Self;
end;

{ TFPCanvasDrawer }

procedure TFPCanvasDrawer.DrawLineDepth(AX1, AY1, AX2, AY2, ADepth: Integer);
begin
  DrawLineDepth(Point(AX1, AY1), Point(AX2, AY2), ADepth);
end;

procedure TFPCanvasDrawer.DrawLineDepth(const AP1, AP2: TPoint; ADepth: Integer);
var
  d: TPoint;
begin
  d := Point(ADepth, -ADepth);
  Polygon([AP1, AP1 + d, AP2 + d, AP2]);
end;

procedure TFPCanvasDrawer.LineTo(const AP: TPoint);
begin
  LineTo(AP.X, AP.Y)
end;

procedure TFPCanvasDrawer.MoveTo(const AP: TPoint);
begin
  MoveTo(AP.X, AP.Y)
end;

function TFPCanvasDrawer.Scale(ADistance: Integer): Integer;
begin
  Result := ADistance;
end;

function TFPCanvasDrawer.TextExtent(const AText: String): TPoint;
var
  sl: TStrings;
begin
  if Pos(LineEnding, AText) = 0 then
    exit(SimpleTextExtent(AText));
  sl := TStringList.Create;
  try
    sl.Text := AText;
    Result := TextExtent(sl);
  finally
    sl.Free;
  end;
end;

function TFPCanvasDrawer.TextExtent(AText: TStrings): TPoint;
var
  i: Integer;
begin
  Result := Size(0, -LINE_INTERVAL);
  for i := 0 to AText.Count - 1 do
    with SimpleTextExtent(AText[i]) do begin
      Result.X := Max(Result.X, X);
      Result.Y += Y + LINE_INTERVAL;
    end;
end;

function TFPCanvasDrawer.TextOut: TChartTextOut;
begin
  Result := TChartTextOut.Create(Self);
end;

{ TCanvasDrawer }

procedure TCanvasDrawer.AddToFontOrientation(ADelta: Integer);
begin
  with FCanvas.Font do
    Orientation := Orientation + ADelta;
end;

procedure TCanvasDrawer.ClippingStart(const AClipRect: TRect);
begin
  FCanvas.ClipRect := AClipRect;
  FCanvas.Clipping := true;
end;

procedure TCanvasDrawer.ClippingStart;
begin
  FCanvas.Clipping := true;
end;

procedure TCanvasDrawer.ClippingStop;
begin
  FCanvas.Clipping := false;
end;

constructor TCanvasDrawer.Create(ACanvas: TCanvas);
begin
  FCanvas := ACanvas;
end;

procedure TCanvasDrawer.Ellipse(AX1, AY1, AX2, AY2: Integer);
begin
  FCanvas.Ellipse(AX1, AY1, AX2, AY2);
end;

procedure TCanvasDrawer.FillRect(AX1, AY1, AX2, AY2: Integer);
begin
  FCanvas.FillRect(AX1, AY1, AX2, AY2);
end;

function TCanvasDrawer.GetBrushColor: TChartColor;
begin
  Result := FCanvas.Brush.Color;
end;

function TCanvasDrawer.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TCanvasDrawer.GetFontAngle: Double;
begin
  Result := OrientToRad(FCanvas.Font.Orientation);
end;

function TCanvasDrawer.HasCanvas: Boolean;
begin
  Result := true;
end;

procedure TCanvasDrawer.Line(AX1, AY1, AX2, AY2: Integer);
begin
  FCanvas.Line(AX1, AY1, AX2, AY2);
end;

procedure TCanvasDrawer.Line(const AP1, AP2: TPoint);
begin
  FCanvas.Line(AP1, AP2);
end;

procedure TCanvasDrawer.LineTo(AX, AY: Integer);
begin
  FCanvas.LineTo(AX, AY);
end;

procedure TCanvasDrawer.MoveTo(AX, AY: Integer);
begin
  FCanvas.MoveTo(AX, AY);
end;

procedure TCanvasDrawer.Polygon(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
begin
  FCanvas.Polygon(APoints, false, AStartIndex, ANumPts);
end;

procedure TCanvasDrawer.Polyline(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer;
  AEndPoint: Boolean);
begin
  FCanvas.Polyline(APoints, AStartIndex, ANumPts);
  if AEndPoint then begin
    // Polyline does not draw the end point.
    if ANumPts < 0 then
      ANumPts := Length(APoints);
    with APoints[ANumPts - 1] do
      FCanvas.Pixels[X, Y] := FCanvas.Pen.Color;
  end;
end;

procedure TCanvasDrawer.PrepareSimplePen(AColor: TChartColor);
begin
  with FCanvas.Pen do begin
    Color := AColor;
    Style := psSolid;
    Mode := pmCopy;
    Width := 1;
  end;
end;

procedure TCanvasDrawer.RadialPie(
  AX1, AY1, AX2, AY2: Integer;
  AStartAngle16Deg, AAngleLength16Deg: Integer);
begin
  FCanvas.RadialPie(
    AX1, AY1, AX2, AY2, AStartAngle16Deg, AAngleLength16Deg);
end;

procedure TCanvasDrawer.Rectangle(AX1, AY1, AX2, AY2: Integer);
begin
  FCanvas.Rectangle(AX1, AY1, AX2, AY2);
end;

procedure TCanvasDrawer.Rectangle(const ARect: TRect);
begin
  FCanvas.Rectangle(ARect);
end;

procedure TCanvasDrawer.SetBrush(ABrush: TFPCustomBrush);
begin
  FCanvas.Brush.Assign(ABrush);
end;

procedure TCanvasDrawer.SetBrushColor(AColor: TChartColor);
begin
  FCanvas.Brush.Color := AColor;
end;

procedure TCanvasDrawer.SetBrushParams(
  AStyle: TFPBrushStyle; AColor: TChartColor);
begin
  FCanvas.Brush.Style := AStyle;
  FCanvas.Brush.Color := AColor;
end;

procedure TCanvasDrawer.SetFont(AFont: TFPCustomFont);
begin
  FCanvas.Font.Assign(AFont);
end;

procedure TCanvasDrawer.SetPen(APen: TFPCustomPen);
begin
  FCanvas.Pen.Assign(APen);
end;

procedure TCanvasDrawer.SetPenParams(AStyle: TFPPenStyle; AColor: TChartColor);
begin
  FCanvas.Pen.Style := AStyle;
  FCanvas.Pen.Color := AColor;
end;

function TCanvasDrawer.SimpleTextExtent(const AText: String): TPoint;
begin
  Result := FCanvas.TextExtent(AText);
end;

procedure TCanvasDrawer.SimpleTextOut(AX, AY: Integer; const AText: String);
begin
  FCanvas.TextOut(AX, AY, AText);
end;

end.

