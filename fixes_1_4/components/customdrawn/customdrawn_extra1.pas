unit customdrawn_extra1;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils, Types,
  // LCL -> Use only TForm, TWinControl, TCanvas and TLazIntfImage
  Graphics, Controls, LCLType,
  //
  customdrawndrawers, customdrawn_common;

type

  { TCDDrawerExtra1 }

  TCDDrawerExtra1 = class(TCDDrawerCommon)
  public
    function GetMeasures(AMeasureID: Integer): Integer; override;
    // ===================================
    // Common Controls Tab
    // ===================================
    procedure DrawTrackBar(ADest: TCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDPositionedCStateEx); override;
  end;
{  TCDButtonDrawerGrad = class(TCDButtonDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDButton: TCDButton); override;
    procedure DrawToCanvas(ADest: TCanvas; CDButton: TCDButton); override;
  end;

  TCDTrackBarDrawerGraph = class(TCDTrackBarDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; FPImg: TLazIntfImage;
      CDTrackBar: TCDTrackBar); override;
    procedure GetGeometry(var ALeftBorder, ARightBorder: Integer); override;
  end;}

implementation

{procedure TCDButtonDrawerGrad.DrawToIntfImage(ADest: TFPImageCanvas;
  CDButton: TCDButton);
begin

end;

procedure TCDButtonDrawerGrad.DrawToCanvas(ADest: TCanvas; CDButton: TCDButton);
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
  if CDButton.IsDown then
    DrawCDButtonDown(TmpB.Canvas, CDButton.GetRGBBackgroundColor)
  else if CDButton.Focused then
    //GradientFill(GetUColor(CDButton.Color, 50), GetAColor(CDButton.Color, 60), TmpB.Canvas);
    GradientFill(clWhite, GetAColor(CDButton.Color, 96), TmpB.Canvas)
  else
    //GradientFill(GetUColor(CDButton.Color, 10), GetAColor(CDButton.Color, 20), TmpB.Canvas);
    GradientFill(clWhite, CDButton.Color, TmpB.Canvas);

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

procedure TCDTrackBarDrawerGraph.GetGeometry(var ALeftBorder,
  ARightBorder: Integer);
begin
  ALeftBorder := 9;
  ARightBorder := 9;
end;

  RegisterTrackBarDrawer(TCDTrackBarDrawerGraph.Create, dsExtra1);}

{ TCDDrawerExtra1 }

function TCDDrawerExtra1.GetMeasures(AMeasureID: Integer): Integer;
begin
  case AMeasureId of
  TCDTRACKBAR_LEFT_SPACING:  Result := 9;
  TCDTRACKBAR_RIGHT_SPACING: Result := 9;
  else
    Result:=inherited GetMeasures(AMeasureID);
  end;
end;

procedure TCDDrawerExtra1.DrawTrackBar(ADest: TCanvas;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDPositionedCStateEx);
var
  lDrawingBottom, StepsCount, i: Integer;
  pStart, pEnd: integer; // for drawing the decorative bars
  dRect: TRect;
  pStepWidth, pHalfStepWidth: Integer;
  CDBarEdge: Integer;
begin
  CDBarEdge := GetMeasures(TCDTRACKBAR_LEFT_SPACING)
    + GetMeasures(TCDTRACKBAR_RIGHT_SPACING);

  // Preparations
  StepsCount := AStateEx.PosCount;
  pStepWidth := (ASize.cx - CDBarEdge) div StepsCount;
  pHalfStepWidth := (ASize.cx - CDBarEdge) div (StepsCount * 2);

  // The bottom part of the drawing
  lDrawingBottom := ASize.cy - 10;

  // Background

  ADest.Brush.Color := AStateEx.ParentRGBColor;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psSolid;
  ADest.Pen.Color := AStateEx.ParentRGBColor;
  ADest.Rectangle(0, 0, ASize.cx, ASize.cy);

  // Draws the double-sided arrow in the center of the slider

  ADest.Brush.Color := ColorToRGB($006BB6E6);
  ADest.Pen.Style := psSolid;
  ADest.Pen.Color := ColorToRGB($006BB6E6);
  ADest.Line(0, lDrawingBottom, ASize.cx, lDrawingBottom);
  ADest.Line(3, lDrawingBottom - 1, 6, lDrawingBottom - 1);
  ADest.Line(5, lDrawingBottom - 2, 6, lDrawingBottom - 2);
  ADest.Line(3, lDrawingBottom + 1, 6, lDrawingBottom + 1);
  ADest.Line(5, lDrawingBottom + 2, 6, lDrawingBottom + 2);
  ADest.Line(ASize.cx - 1 - 3, lDrawingBottom - 1, ASize.cx - 1 - 6, lDrawingBottom - 1);
  ADest.Line(ASize.cx - 1 - 5, lDrawingBottom - 2, ASize.cx - 1 - 6, lDrawingBottom - 2);
  ADest.Line(ASize.cx - 1 - 3, lDrawingBottom + 1, ASize.cx - 1 - 6, lDrawingBottom + 1);
  ADest.Line(ASize.cx - 1 - 5, lDrawingBottom + 2, ASize.cx - 1 - 6, lDrawingBottom + 2);
  ADest.Pen.Color := ColorToRGB(clGray);
  ADest.Brush.Color := ColorToRGB($00F0F0F0);

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
    ADest.Pen.Color := clBlack;
    if i <= AStateEx.Position then
      ADest.Brush.Color := clDkGray
    else
      ADest.Brush.Color := clWhite;

    ADest.Rectangle(dRect);

    // Draw the slider

    if i = AStateEx.Position then
    begin
      ADest.Brush.FPColor := TColorToFPColor(ColorToRGB($006BB6E6));
      ADest.Brush.Style := bsSolid;
      ADest.Rectangle(pStart, lDrawingBottom + 1, pStart + 10, lDrawingBottom + 6);
      ADest.Pen.Color := ColorToRGB($005BA6C6);
      ADest.RecTangle(pStart, lDrawingBottom + 2, pStart + 10, lDrawingBottom + 7);
      ADest.Pen.Color := ColorToRGB($006BB6E6);
      ADest.RecTangle(pStart, lDrawingBottom, pStart + 10, lDrawingBottom + 2);
    end;
    pStart := pStart + pStepWidth;
  end;

  ADest.Pen.Color := ColorToRGB($007BC6F6);
  ADest.Line(7, lDrawingBottom - 1, ASize.cx - 8, lDrawingBottom - 1);
  ADest.Line(7, lDrawingBottom + 1, ASize.cx - 8, lDrawingBottom + 1);
  ADest.Pixels[2, lDrawingBottom - 1] := ADest.Pen.Color;
  ADest.Pixels[4, lDrawingBottom - 2] := ADest.Pen.Color;
  ADest.Pixels[2, lDrawingBottom + 1] := ADest.Pen.Color;
  ADest.Pixels[4, lDrawingBottom + 2] := ADest.Pen.Color;
  ADest.Pixels[6, lDrawingBottom - 3] := ADest.Pen.Color;
  ADest.Pixels[6, lDrawingBottom + 3] := ADest.Pen.Color;
  ADest.Pixels[ASize.cx - 1 - 2, lDrawingBottom - 1] := ADest.Pen.Color;
  ADest.Pixels[ASize.cx - 1 - 4, lDrawingBottom - 2] := ADest.Pen.Color;
  ADest.Pixels[ASize.cx - 1 - 2, lDrawingBottom + 1] := ADest.Pen.Color;
  ADest.Pixels[ASize.cx - 1 - 4, lDrawingBottom + 2] := ADest.Pen.Color;
  ADest.Pixels[ASize.cx - 1 - 6, lDrawingBottom - 3] := ADest.Pen.Color;
  ADest.Pixels[ASize.cx - 1 - 6, lDrawingBottom + 3] := ADest.Pen.Color;
end;

initialization
  RegisterDrawer(TCDDrawerExtra1.Create, dsExtra1);
end.

