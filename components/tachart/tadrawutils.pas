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

{$mode objfpc}{$H+}

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
  //TCanvas = TFPCustomCanvas;

  TPenBrushFont = set of (pbfPen, pbfBrush, pbfFont);

  { TPenBrushFontRecall }

  TPenBrushFontRecall = class
  private
    FBrush: TBrush;
    FCanvas: TCanvas;
    FFont: TFont;
    FPen: TPen;
  public
    constructor Create(ACanvas: TCanvas; AParams: TPenBrushFont);
    destructor Destroy; override;
    procedure Recall;
  end;

  TChartTextOut = class;

  { IChartDrawer }

  IChartDrawer = interface
    function GetCanvas: TCanvas;
    function HasCanvas: Boolean;
    procedure Line(AX1, AY1, AX2, AY2: Integer);
    procedure PrepareSimplePen(AColor: TChartColor);
    procedure RadialPie(
      AX1, AY1, AX2, AY2: Integer;
      AStartAngle16Deg, AAngleLength16Deg: Integer);
    procedure Rectangle(const ARect: TRect);
    procedure SetBrush(APen: TFPCustomBrush);
    procedure SetBrushParams(AStyle: TBrushStyle; AColor: TChartColor);
    procedure SetFont(AValue: TFPCustomFont);
    procedure SetPen(APen: TFPCustomPen);
    function TextExtent(const AText: String): TPoint;
    function TextExtent(AText: TStrings): TPoint;
    function TextOut: TChartTextOut;

    property Brush: TFPCustomBrush write SetBrush;
    property Canvas: TCanvas read GetCanvas;
    property Font: TFPCustomFont write SetFont;
    property Pen: TFPCustomPen write SetPen;
  end;

  { TChartTextOut }

  TChartTextOut = class
    FAlignment: TAlignment;
    FDrawer: IChartDrawer;
    FPos: TPoint;
    FText1: String;
    FText2: TStrings;
    FWidth: Integer;
  public
    constructor Create(ADrawer: IChartDrawer);
  public
    function Alignment(AAlignment: TAlignment): TChartTextOut;
    procedure Done;
    function Pos(AX, AY: Integer): TChartTextOut;
    function Pos(const APos: TPoint): TChartTextOut;
    function Text(const AText: String): TChartTextOut;
    function Text(const AText: TStrings): TChartTextOut;
    function Width(AWidth: Integer): TChartTextOut;
  end;

  { TCanvasDrawer }

  TCanvasDrawer = class(TInterfacedObject, IChartDrawer)
  private
    FCanvas: TCanvas;
    procedure SetBrush(ABrush: TFPCustomBrush);
    procedure SetFont(AFont: TFPCustomFont);
    procedure SetPen(APen: TFPCustomPen);
  public
    constructor Create(ACanvas: TCanvas);
    function GetCanvas: TCanvas;
    function HasCanvas: Boolean;

    procedure Line(AX1, AY1, AX2, AY2: Integer);
    procedure PrepareSimplePen(AColor: TChartColor);
    procedure RadialPie(
      AX1, AY1, AX2, AY2: Integer;
      AStartAngle16Deg, AAngleLength16Deg: Integer);
    procedure Rectangle(const ARect: TRect);
    procedure SetBrushParams(AStyle: TBrushStyle; AColor: TChartColor);
    function TextExtent(const AText: String): TPoint;
    function TextExtent(AText: TStrings): TPoint;
    function TextOut: TChartTextOut;
  end;

procedure DrawLineDepth(ACanvas: TCanvas; AX1, AY1, AX2, AY2, ADepth: Integer);
procedure DrawLineDepth(ACanvas: TCanvas; const AP1, AP2: TPoint; ADepth: Integer);

function MultiLineTextExtent(ACanvas: TCanvas; const AText: String): TPoint;
function MultiLineTextExtent(ACanvas: TCanvas; AText: TStrings): TPoint;
procedure MultiLineTextOut(
  ACanvas: TCanvas; APos: TPoint; const AText: String;
  AAlignment: TAlignment; AWidth: Integer);
procedure MultiLineTextOut(
  ACanvas: TCanvas; APos: TPoint; AText: TStrings;
  AAlignment: TAlignment; AWidth: Integer);

procedure PrepareSimplePen(ACanvas: TCanvas; AColor: TColor);
procedure PrepareXorPen(ACanvas: TCanvas);

function TypicalTextHeight(ACanvas: TCanvas): Integer;

implementation

uses
  Math, TAChartUtils;

procedure DrawLineDepth(ACanvas: TCanvas; AX1, AY1, AX2, AY2, ADepth: Integer);
begin
  DrawLineDepth(ACanvas, Point(AX1, AY1), Point(AX2, AY2), ADepth);
end;

procedure DrawLineDepth(
  ACanvas: TCanvas; const AP1, AP2: TPoint; ADepth: Integer);
var
  d: TSize;
begin
  d := Size(ADepth, -ADepth);
  ACanvas.Polygon([AP1, AP1 + d, AP2 + d, AP2]);
end;

const
  LINE_INTERVAL = 2;

function MultiLineTextExtent(ACanvas: TCanvas; const AText: String): TPoint;
var
  sl: TStrings;
begin
  if Pos(LineEnding, AText) = 0 then
    exit(ACanvas.TextExtent(AText));
  sl := TStringList.Create;
  try
    sl.Text := AText;
    Result := MultiLineTextExtent(ACanvas, sl);
  finally
    sl.Free;
  end;
end;

function MultiLineTextExtent(ACanvas: TCanvas; AText: TStrings): TPoint;
var
  i: Integer;
begin
  Result := Size(0, -LINE_INTERVAL);
  for i := 0 to AText.Count - 1 do
    with ACanvas.TextExtent(AText[i]) do begin
      Result.X := Max(Result.X, cx);
      Result.Y += cy + LINE_INTERVAL;
    end;
end;

procedure MultiLineTextOut(
  ACanvas: TCanvas; APos: TPoint; const AText: String; AAlignment: TAlignment;
  AWidth: Integer);
var
  sl: TStrings;
begin
  if Pos(LineEnding, AText) = 0 then begin
    ACanvas.TextOut(APos.X, APos.Y, AText);
    exit;
  end;
  sl := TStringList.Create;
  try
    sl.Text := AText;
    MultiLineTextOut(ACanvas, APos, sl, AAlignment, AWidth);
  finally
    sl.Free;
  end;
end;

procedure MultiLineTextOut(
  ACanvas: TCanvas; APos: TPoint; AText: TStrings; AAlignment: TAlignment;
  AWidth: Integer);
var
  i: Integer;
  a: Double;
  lineExtent, p: TPoint;
begin
  a := -OrientToRad(ACanvas.Font.Orientation);
  for i := 0 to AText.Count - 1 do begin
    lineExtent := ACanvas.TextExtent(AText[i]);
    p := APos;
    case AAlignment of
      taCenter: p += RotatePoint(Point((AWidth - lineExtent.X) div 2, 0), a);
      taRightJustify: p += RotatePoint(Point(AWidth - lineExtent.X, 0), a);
    end;
    ACanvas.TextOut(p.X, p.Y, AText[i]);
    APos += RotatePoint(Point(0, lineExtent.Y + LINE_INTERVAL), a);
  end;
end;

procedure PrepareSimplePen(ACanvas: TCanvas; AColor: TColor);
begin
  with ACanvas.Pen do begin
    Color := AColor;
    Style := psSolid;
    Mode := pmCopy;
    Width := 1;
  end;
end;

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

function TypicalTextHeight(ACanvas: TCanvas): Integer;
const
  TYPICAL_TEXT = 'Iy';
begin
  Result := ACanvas.TextHeight(TYPICAL_TEXT);
end;

{ TChartTextOut }

function TChartTextOut.Alignment(AAlignment: TAlignment): TChartTextOut;
begin
  FAlignment := AAlignment;
  Result := Self;
end;

constructor TChartTextOut.Create(ADrawer: IChartDrawer);
begin
  FDrawer := ADrawer;
  FAlignment := taLeftJustify;
end;

procedure TChartTextOut.Done;
begin
  if FText2 = nil then
    MultiLineTextOut(FDrawer.Canvas, FPos, FText1, FAlignment, FWidth)
  else
    MultiLineTextOut(FDrawer.Canvas, FPos, FText2, FAlignment, FWidth);
  Free;
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

{ TCanvasDrawer }

constructor TCanvasDrawer.Create(ACanvas: TCanvas);
begin
  FCanvas := ACanvas;
end;

function TCanvasDrawer.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TCanvasDrawer.HasCanvas: Boolean;
begin
  Result := true;
end;

procedure TCanvasDrawer.Line(AX1, AY1, AX2, AY2: Integer);
begin
  FCanvas.Line(AX1, AY1, AX2, AY2);
end;

procedure TCanvasDrawer.PrepareSimplePen(AColor: TChartColor);
begin
  TADrawUtils.PrepareSimplePen(FCanvas, AColor);
end;

procedure TCanvasDrawer.RadialPie(
  AX1, AY1, AX2, AY2: Integer;
  AStartAngle16Deg, AAngleLength16Deg: Integer);
begin
  FCanvas.RadialPie(
    AX1, AY1, AX2, AY2, AStartAngle16Deg, AAngleLength16Deg);
end;

procedure TCanvasDrawer.Rectangle(const ARect: TRect);
begin
  FCanvas.Rectangle(ARect);
end;

procedure TCanvasDrawer.SetBrush(ABrush: TFPCustomBrush);
begin
  FCanvas.Brush.Assign(ABrush);
end;

procedure TCanvasDrawer.SetBrushParams(
  AStyle: TBrushStyle; AColor: TChartColor);
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

function TCanvasDrawer.TextExtent(const AText: String): TPoint;
begin
  Result := MultiLineTextExtent(FCanvas, AText);
end;

function TCanvasDrawer.TextExtent(AText: TStrings): TPoint;
begin
  Result := MultiLineTextExtent(FCanvas, AText);
end;

function TCanvasDrawer.TextOut: TChartTextOut;
begin
  Result := TChartTextOut.Create(Self);
end;

{ TPenBrushFontRecall }

constructor TPenBrushFontRecall.Create(ACanvas: TCanvas; AParams: TPenBrushFont);
begin
  inherited Create;
  FCanvas := ACanvas;
  if pbfPen in AParams then begin
    FPen := TPen.Create;
    FPen.Assign(FCanvas.Pen);
  end;
  if pbfBrush in AParams then begin
    FBrush := TBrush.Create;
    FBrush.Assign(FCanvas.Brush);
  end;
  if pbfFont in AParams then begin
    FFont := TFont.Create;
    FFont.Assign(FCanvas.Font);
  end;
end;

destructor TPenBrushFontRecall.Destroy;
begin
  Recall;
  inherited;
end;

procedure TPenBrushFontRecall.Recall;
begin
  if FPen <> nil then begin
    FCanvas.Pen.Assign(FPen);
    FreeAndNil(FPen);
  end;
  if FBrush <> nil then begin
    FCanvas.Brush.Assign(FBrush);
    FreeAndNil(FBrush);
  end;
  if FFont <> nil then begin
    FCanvas.Font.Assign(FFont);
    FreeAndNil(FFont);
  end;
end;

end.

