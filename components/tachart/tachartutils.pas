{
 /***************************************************************************
                               TAChartUtils.pas
                               ----------------
              Component Library Standard Graph Utiliity Functions


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

}

unit TAChartUtils;

{$IFDEF fpc}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  Graphics, Types;

const
  MaxColor = 15;
  Colors: array [1..MaxColor] of TColor = (
    clRed, clGreen, clYellow, clBlue, clWhite, clGray, clFuchsia,
    clTeal, clNavy, clMaroon, clLime, clOlive, clPurple, clSilver, clAqua);

type
  TDoublePoint = record
    X, Y: Double;
  end;

  TPointDistFunc = function (const A, B: TPoint): Integer;

  TAxisScale = (asIncreasing, asDecreasing, asLogIncreasing, asLogDecreasing);

  TPenBrushFont = set of (pbfPen, pbfBrush, pbfFont);

  { TPenBrushFontRecall }

  TPenBrushFontRecall = class
  private
    FPen: TPen;
    FBrush: TBrush;
    FFont: TFont;
    FCanvas: TCanvas;
  public
    constructor Create(ACanvas: TCanvas; AParams: TPenBrushFont);
    destructor Destroy; override;
    procedure Recall;
  end;

procedure CalculateIntervals(
  AMin, AMax: Double; AxisScale: TAxisScale; out AStart, AStep: Double);

function EqualPoints(const A, B: TPoint): Boolean; inline;

procedure Exchange(var A, B: Integer); overload;
procedure Exchange(var A, B: Double); overload;

function PointDist(const A, B: TPoint): Integer; inline;
function PointDistX(const A, B: TPoint): Integer; inline;

procedure RotateLabel(
  Canvas: TCanvas; x, y: Integer; const St: String; RotDegree: Integer);

implementation

uses
  Math, SysUtils, LCLIntF, LCLType;

procedure CalculateIntervals(
  AMin, AMax: Double; AxisScale: TAxisScale; out AStart, AStep: Double);
var
  extent, extentTmp, stepCount, scale, maxStepCount, m: Double;
  i: Integer;
const
  GOOD_STEPS: array [1..3] of Double = (0.2, 0.5, 1.0);
begin
  extent := AMax - AMin;
  AStep := 1;
  AStart := AMin;
  if extent <= 0 then exit;

  maxStepCount := 0;
  scale := 1.0;
  for i := Low(GOOD_STEPS) to High(GOOD_STEPS) do begin
    extentTmp := extent / GOOD_STEPS[i];
    m := power(10, Round(log10(extentTmp)));
    while extentTmp * m > 10 do
      m *= 0.1;
    while extentTmp * m <= 1 do
      m *= 10;
    stepCount := extentTmp * m;
    if stepCount > maxStepCount then begin
      maxStepCount := stepCount;
      scale := m;
      AStep := GOOD_STEPS[i] / m;
    end;
  end;
  case AxisScale of
    asIncreasing: begin
      // If 0 is in the interval, set it as a mark.
      if InRange(0, AMin, AMax) then
        AStart := 0
      else
        AStart := Round((AMin - AStep) * scale) / scale;
      while AStart > AMin do AStart -= AStep;
    end;
    asDecreasing: begin
      // If 0 is in the interval, set it as a mark.
      if InRange(0, AMin, AMax) then
        AStart := 0
      else
        AStart := Round((AMax + AStep) * scale) / scale;
      while AStart < AMax do AStart += AStep;
    end;
    asLogIncreasing: begin
      // FIXME: asLogIncreasing is still not implemented.
      // The following is the same code for asIncreasing;
      // If 0 is in the interval, set it as a mark.
      if InRange(0, AMin, AMax) then
        AStart := 0
      else
        AStart := Round((AMin - AStep) * scale) / scale;
      while AStart > AMin do AStart -= AStep;
    end;
    asLogDecreasing: begin
      // FIXME: asLogDecreasing is still not implemented.
      // The following is the same code for asIncreasing;
      // If 0 is in the interval, set it as a mark.
      if InRange(0, AMin, AMax) then
        AStart := 0
      else
        AStart := Round((AMax + AStep) * scale) / scale;
      while AStart < AMax do AStart += AStep;
    end;
  end; {case AxisScale}
end;

function EqualPoints(const A, B: TPoint): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

procedure Exchange(var A, B: Integer); overload;
var
  t: Integer;
begin
  t := A;
  A := B;
  B := t;
end;

procedure Exchange(var A, B: Double); overload;
var
  t: Double;
begin
  t := A;
  A := B;
  B := t;
end;

function PointDist(const A, B: TPoint): Integer;
begin
  Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;

function PointDistX(const A, B: TPoint): Integer;
begin
  Result := Abs(A.X - B.X);
end;

procedure RotateLabel(
  Canvas: TCanvas; x, y: Integer; const St: String; RotDegree: Integer);
var
  OldFont, NewFont: HFONT;
  LogRec: TLOGFONT;
  DC: HDC;
begin
  with Canvas do begin
    Brush.Style := bsClear;
    GetObject(Font.Handle, SizeOf(LogRec), @LogRec);
    LogRec.lfEscapement   := RotDegree * 10;
    LogRec.lfOrientation  := 0;
    LogRec.lfOutPrecision := OUT_TT_ONLY_PRECIS;
    NewFont := CreateFontIndirect(LogRec);
    DC := Handle;
  end;
  OldFont := SelectObject(DC, NewFont);
  TextOut(DC, X, Y, @St[1], Length(St));
  DeleteObject(SelectObject(DC, OldFont));
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
