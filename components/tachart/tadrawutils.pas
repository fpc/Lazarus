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
  Classes, Graphics, SysUtils, Types;

const
  Colors: array [1..15] of TColor = (
    clRed, clGreen, clYellow, clBlue, clWhite, clGray, clFuchsia,
    clTeal, clNavy, clMaroon, clLime, clOlive, clPurple, clSilver, clAqua);

type
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

procedure DrawLineDepth(ACanvas: TCanvas; AX1, AY1, AX2, AY2, ADepth: Integer);
procedure DrawLineDepth(ACanvas: TCanvas; const AP1, AP2: TPoint; ADepth: Integer);

function MultiLineTextExtent(ACanvas: TCanvas; const AText: String): TPoint;
function MultiLineTextExtent(ACanvas: TCanvas; AText: TStrings): TPoint;
procedure MultiLineTextOut(ACanvas: TCanvas; APos: TPoint; const AText: String);
procedure MultiLineTextOut(
  ACanvas: TCanvas; APos: TPoint; AText: TStrings; AAlignment: TAlignment;
  AWidth: Integer);

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
    MultiLineTextExtent(ACanvas, sl);
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

procedure MultiLineTextOut(ACanvas: TCanvas; APos: TPoint; const AText: String);
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
    MultiLineTextOut(ACanvas, APos, sl, taLeftJustify, 0);
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

