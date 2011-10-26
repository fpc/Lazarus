unit customdrawn_extra1;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils,
  // fpimage
  fpcanvas, fpimgcanv, fpimage,
  // LCL -> Use only TForm, TWinControl, TCanvas and TLazIntfImage
  Graphics, Controls, LCLType, LCLIntf, IntfGraphics,
  //
  customdrawncontrols, customdrawnutils;

type
  TCDButtonDrawerGrad = class(TCDButtonDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDButton: TCDButton); override;
    procedure DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
      FState: TCDButtonState); override;
  end;

  TCDTrackBarDrawerGraph = class(TCDTrackBarDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; FPImg: TLazIntfImage;
      CDTrackBar: TCDTrackBar); override;
    procedure GetGeometry(var ALeftBorder, ARightBorder: Integer); override;
  end;

implementation

procedure TCDButtonDrawerGrad.DrawToIntfImage(ADest: TFPImageCanvas;
  CDButton: TCDButton);
begin

end;

procedure TCDButtonDrawerGrad.DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
  FState: TCDButtonState);
var
  TmpB: TBitmap;
  Str: string;
begin
  // Button shape -> This crashes in Gtk2
  TmpB := TBitmap.Create;
  TmpB.Width := CDButton.Width;
  TmpB.Height := CDButton.Height;
  TmpB.Canvas.Brush.Color := CDButton.Color;
  TmpB.Canvas.Brush.Style := bsSolid;
  TmpB.Canvas.RoundRect(0, 0, TmpB.Width, TmpB.Height, 8, 8);
  //  CDButton.SetShape(TmpB);

  with TmpB.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := CDButton.Parent.Color;
    Pen.Color := Brush.Color;
    Rectangle(0, 0, Width, Height);
    FillRect(0, 0, Width, Height);
    Brush.Color := GetAColor(CDButton.Color, 90);
  end;

  // Button image
  case FState of
    bbsDown:
    begin
      DrawCDButtonDown(TmpB.Canvas, CDButton.GetRGBBackgroundColor);
    end;
    bbsFocused:
      //GradientFill(GetUColor(CDButton.Color, 50), GetAColor(CDButton.Color, 60), TmpB.Canvas);
      GradientFill(clWhite, GetAColor(CDButton.Color, 96), TmpB.Canvas);
    else
      //GradientFill(GetUColor(CDButton.Color, 10), GetAColor(CDButton.Color, 20), TmpB.Canvas);
      GradientFill(clWhite, CDButton.Color, TmpB.Canvas);
  end;

  ADest.Draw(0, 0, TmpB);

  TmpB.Free;

  // Button text
  ADest.Font.Assign(CDButton.Font);
  ADest.Brush.Style := bsClear;
  ADest.Pen.Style := psSolid;
  Str := CDButton.Caption;
  ADest.TextOut((CDButton.Width - ADest.TextWidth(Str)) div 2,
    (CDButton.Height - ADest.TextHeight(Str)) div 2, Str);
end;

{ TCDTrackBarDrawer }

procedure TCDTrackBarDrawerGraph.DrawToIntfImage(ADest: TFPImageCanvas;
  FPImg: TLazIntfImage; CDTrackBar: TCDTrackBar);
const
  CDBarEdge = 18;
var
  lDrawingBottom, StepsCount, i: Integer;
  pStart, pEnd: integer; // for drawing the decorative bars
  dRect: TRect;
  pStepWidth, pHalfStepWidth: Integer;
begin
  // Sanity check
  if CDTrackBar.Max - CDTrackBar.Min <= 0 then
    raise Exception.Create('[TCDTrackBarDrawerGraph.DrawToIntfImage] Max-Min must be at least 1');

  // Preparations
  StepsCount := CDTrackBar.Max - CDTrackBar.Min + 1;
  pStepWidth := (CDTrackBar.Width - CDBarEdge) div StepsCount;
  pHalfStepWidth := (CDTrackBar.Width - CDBarEdge) div (StepsCount * 2);

  // The bottom part of the drawing
  lDrawingBottom := CDTrackBar.Height - 10;

  // Background

  if CDTrackBar.Parent = nil then
    ADest.Brush.FPColor := colLtGray
  else
    ADest.Brush.FPColor := TColorToFPColor(ColorToRGB(CDTrackBar.Color));
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psClear;
  ADest.Rectangle(0, 0, CDTrackBar.Width, CDTrackBar.Height);
  ADest.Brush.FPColor := TColorToFPColor(ColorToRGB($006BB6E6));

  // Draws the double-sided arrow in the center of the slider

  ADest.Pen.Style := psSolid;
  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($006BB6E6));
  ADest.Line(0, lDrawingBottom, CDTrackBar.Width, lDrawingBottom);
  ADest.Line(3, lDrawingBottom - 1, 6, lDrawingBottom - 1);
  ADest.Line(5, lDrawingBottom - 2, 6, lDrawingBottom - 2);
  ADest.Line(3, lDrawingBottom + 1, 6, lDrawingBottom + 1);
  ADest.Line(5, lDrawingBottom + 2, 6, lDrawingBottom + 2);
  ADest.Line(CDTrackBar.Width - 1 - 3, lDrawingBottom - 1, CDTrackBar.Width - 1 - 6, lDrawingBottom - 1);
  ADest.Line(CDTrackBar.Width - 1 - 5, lDrawingBottom - 2, CDTrackBar.Width - 1 - 6, lDrawingBottom - 2);
  ADest.Line(CDTrackBar.Width - 1 - 3, lDrawingBottom + 1, CDTrackBar.Width - 1 - 6, lDrawingBottom + 1);
  ADest.Line(CDTrackBar.Width - 1 - 5, lDrawingBottom + 2, CDTrackBar.Width - 1 - 6, lDrawingBottom + 2);
  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB(clGray));
  ADest.Brush.FPColor := TColorToFPColor(ColorToRGB($00F0F0F0));

  // Draws the decorative bars and also the slider button

  pStart := 10 - 1;
  for i := 0 to StepsCount - 1 do
  begin
    // Draw the decorative bars
    dRect := Bounds(
      pStart + pHalfStepWidth,
      lDrawingBottom - 5 - i,
      Round(pStepWidth)-3,
      4 + i);

    ADest.Brush.Style := bsSolid;
    ADest.Pen.Style := psSolid;
    ADest.Pen.FPColor := colBlack;
    if i + CDTrackBar.Min <= CDTrackBar.Position then
      ADest.Brush.FPColor := colDkGray
    else
      ADest.Brush.FPColor := colWhite;

    ADest.Rectangle(dRect);

    // Draw the slider

    if i + CDTrackBar.Min = CDTrackBar.Position then
    begin
      ADest.Brush.FPColor := TColorToFPColor(ColorToRGB($006BB6E6));
      ADest.Brush.Style := bsSolid;
      ADest.Rectangle(pStart, lDrawingBottom + 1, pStart + 10, lDrawingBottom + 6);
      ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($005BA6C6));
      ADest.RecTangle(pStart, lDrawingBottom + 2, pStart + 10, lDrawingBottom + 7);
      ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($006BB6E6));
      ADest.RecTangle(pStart, lDrawingBottom, pStart + 10, lDrawingBottom + 2);
    end;
    pStart := pStart + pStepWidth;
  end;

  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($007BC6F6));
  ADest.Line(7, lDrawingBottom - 1, CDTrackBar.Width - 8, lDrawingBottom - 1);
  ADest.Line(7, lDrawingBottom + 1, CDTrackBar.Width - 8, lDrawingBottom + 1);
  ADest.Colors[2, lDrawingBottom - 1] := ADest.Pen.FPColor;
  ADest.Colors[4, lDrawingBottom - 2] := ADest.Pen.FPColor;
  ADest.Colors[2, lDrawingBottom + 1] := ADest.Pen.FPColor;
  ADest.Colors[4, lDrawingBottom + 2] := ADest.Pen.FPColor;
  ADest.Colors[6, lDrawingBottom - 3] := ADest.Pen.FPColor;
  ADest.Colors[6, lDrawingBottom + 3] := ADest.Pen.FPColor;
  ADest.Colors[CDTrackBar.Width - 1 - 2, lDrawingBottom - 1] := ADest.Pen.FPColor;
  ADest.Colors[CDTrackBar.Width - 1 - 4, lDrawingBottom - 2] := ADest.Pen.FPColor;
  ADest.Colors[CDTrackBar.Width - 1 - 2, lDrawingBottom + 1] := ADest.Pen.FPColor;
  ADest.Colors[CDTrackBar.Width - 1 - 4, lDrawingBottom + 2] := ADest.Pen.FPColor;
  ADest.Colors[CDTrackBar.Width - 1 - 6, lDrawingBottom - 3] := ADest.Pen.FPColor;
  ADest.Colors[CDTrackBar.Width - 1 - 6, lDrawingBottom + 3] := ADest.Pen.FPColor;
end;

procedure TCDTrackBarDrawerGraph.GetGeometry(var ALeftBorder,
  ARightBorder: Integer);
begin
  ALeftBorder := 9;
  ARightBorder := 9;
end;

initialization
  RegisterButtonDrawer(TCDButtonDrawerGrad.Create, dsExtra1);
  RegisterTrackBarDrawer(TCDTrackBarDrawerGraph.Create, dsExtra1);
end.

