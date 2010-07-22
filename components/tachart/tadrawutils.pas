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
  Classes, Graphics, SysUtils;

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

procedure PrepareSimplePen(ACanvas: TCanvas; AColor: TColor);
procedure PrepareXorPen(ACanvas: TCanvas);

function TypicalTextHeight(ACanvas: TCanvas): Integer;

implementation

uses
  Types, TAChartUtils;

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

