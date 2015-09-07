unit CustomDrawn_Android;

{$mode objfpc}{$H+}
{ $define CD_UseImageResources}

interface

uses
  // RTL
  Classes, SysUtils, Types, Math,
  // fpimage
  fpcanvas, fpimgcanv, fpimage,
  // LCL -> Use only TForm, TWinControl, TCanvas and TLazIntfImage
  Graphics, Controls, LCLType, LCLIntf, IntfGraphics, LResources, Forms,
  //
  customdrawndrawers, customdrawn_common;

type

  { TCDDrawerAndroid }

  TCDDrawerAndroid = class(TCDDrawerCommon)
  private
    //bmpCheckbox, bmpCheckboxChecked: TBitmap;
    // Alternative checkbox drawing, not currently utilized
    procedure DrawCheckBoxBitmap(ADest: TFPCustomCanvas; ADestPos: TPoint; AState: TCDControlState; ASize: Integer; ABackgroundColor: TFPColor);
    // Makes pixels in each corner transparent for a rounded effect
    procedure DrawTransparentRoundCorners(ADest: TFPCustomCanvas; ADestPos: TPoint; ASize: TSize; AColor: TFPColor);
    // Draws a vertical line with different first and last pixels
    procedure DrawVerticalLineWithFirstLast(ADest: TFPCustomCanvas;
      X, Y1, Y2: Integer; AColorTop, AColorMiddle, AColorEnd: TFPColor);
    // Draws a line alternating between two colors
    procedure DrawAndroidAlternatedHorzLine(ADest: TCanvas; X1, X2,
      Y: Integer; AColor1, AColor2: TColor);
    // Draws 2 mixed gradients which alternate between one another on each pixel
    procedure DrawAndroidMixedVertGradientFill(ADest: TCanvas; ARect: TRect;
      AStart1, AStop1, AStart2, AStop2: TColor);
    // Fills a rectangular area alternating between two pixels
    procedure DrawAndroidMixedFill(ADest: TCanvas; ARect: TRect; AColor1, AColor2: TColor);
    // Draws a circle with a border and a gradient inside it
    procedure DrawAndroidGradientCircle(ADest: TCanvas; ADestPos: TPoint; ASize: TSize; ABorderColor, ATopColor, ABottomColor: TColor);
  public
    procedure CreateResources; override;
    procedure LoadResources; override;
    procedure FreeResources; override;
    //procedure LoadFallbackPaletteColors; override;
    function GetDrawStyle: TCDDrawStyle; override;
    // General
    function GetMeasures(AMeasureID: Integer): Integer; override;
{    function GetMeasuresEx(ADest: TCanvas; AMeasureID: Integer;
      AState: TCDControlState; AStateEx: TCDControlStateEx): Integer; virtual; abstract;
    procedure CalculatePreferredSize(ADest: TCanvas; AControlId: TCDControlID;
      AState: TCDControlState; AStateEx: TCDControlStateEx;
      var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); virtual; abstract;
    function GetColor(AColorID: Integer): TColor; virtual; abstract;
    function GetClientArea(ADest: TCanvas; ASize: TSize; AControlId: TCDControlID;
      AState: TCDControlState; AStateEx: TCDControlStateEx): TRect; virtual; abstract;}
    // General drawing routines
    {procedure DrawFocusRect(ADest: TCanvas; ADestPos: TPoint; ASize: TSize); override;
    procedure DrawRaisedFrame(ADest: TCanvas; ADestPos: TPoint; ASize: TSize); override;
    procedure DrawSunkenFrame(ADest: TCanvas; ADestPos: TPoint; ASize: TSize); override;
    procedure DrawShallowSunkenFrame(ADest: TCanvas; ADestPos: TPoint; ASize: TSize); override;}
    procedure DrawTickmark(ADest: TFPCustomCanvas; ADestPos: TPoint; AState: TCDControlState); override;
    {procedure DrawSlider(ADest: TCanvas; ADestPos: TPoint; ASize: TSize; AState: TCDControlState); override;
    procedure DrawCompactArrow(ADest: TCanvas; ADestPos: TPoint; ADirection: TCDControlState); override;}
    // ===================================
    // Standard Tab
    // ===================================
    // TCDButton
    procedure DrawButton(ADest: TFPCustomCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDButtonStateEx); override;
    // TCDEdit
    procedure DrawEditBackground(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
    procedure DrawEditFrame(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
    // TCDCheckBox
    procedure DrawCheckBoxSquare(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    // TCDRadioButton
    procedure DrawRadioButtonCircle(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
  end;

implementation

const
  ANDROID_DPI = 'vldpi';

  // Actually Android buttons are much more complex then this,
  // but this approximation works well
  ANDROID_BUTTON_CORNERS = $006B696B;

  ANDROID_BUTTON_FIRST_LINE_A = $00F7F3F7;
  ANDROID_BUTTON_FIRST_LINE_B = $00EFF3EF;
  ANDROID_BUTTON_SECOND_LINE_A = $00EFF3EF;
  ANDROID_BUTTON_SECOND_LINE_B = $00F7F3F7;
  ANDROID_BUTTON_THIRD_LINE_A = $00EFEFEF;
  ANDROID_BUTTON_THIRD_LINE_B = $00F7F3F7;

  ANDROID_BUTTON_TOP_GRADIENT_A = $00EFEFEF;
  ANDROID_BUTTON_TOP_GRADIENT_B = $00EFEBEF;
  ANDROID_BUTTON_MIDDLE_GRADIENT_A = $00CBCECB;
  ANDROID_BUTTON_MIDDLE_GRADIENT_B = $00CECFCE;
  ANDROID_BUTTON_BOTTOM_GRADIENT_A = $00BDBABD;
  ANDROID_BUTTON_BOTTOM_GRADIENT_B = $00BABDBA;

  ANDROID_BUTTON_PREPRELAST_LINE_A = $00C6C3C6;
  ANDROID_BUTTON_PREPRELAST_LINE_B = $00C6C3C6;
  ANDROID_BUTTON_PRELAST_LINE_A = $00C6CBC6;
  ANDROID_BUTTON_PRELAST_LINE_B = $00CECBCE;
  ANDROID_BUTTON_LAST_LINE_A = $00D6D3D6;
  ANDROID_BUTTON_LAST_LINE_B = $00D6D7D6;

  // Sunken variants

  ANDROID_BUTTON_SUNKEN_FIRST_LINE_A = $0066F366;
  ANDROID_BUTTON_SUNKEN_FIRST_LINE_B = $0066F366;
  ANDROID_BUTTON_SUNKEN_SECOND_LINE_A = $0066F366;
  ANDROID_BUTTON_SUNKEN_SECOND_LINE_B = $0066F366;
  ANDROID_BUTTON_SUNKEN_THIRD_LINE_A = $0066EF66;
  ANDROID_BUTTON_SUNKEN_THIRD_LINE_B = $0066F366;

  ANDROID_BUTTON_SUNKEN_TOP_GRADIENT_A = $0066EF66;
  ANDROID_BUTTON_SUNKEN_TOP_GRADIENT_B = $0066EB66;
  ANDROID_BUTTON_SUNKEN_MIDDLE_GRADIENT_A = $0033CE33;
  ANDROID_BUTTON_SUNKEN_MIDDLE_GRADIENT_B = $0033CF33;
  ANDROID_BUTTON_SUNKEN_BOTTOM_GRADIENT_A = $0000BA00;
  ANDROID_BUTTON_SUNKEN_BOTTOM_GRADIENT_B = $0000BD00;

  ANDROID_BUTTON_SUNKEN_PREPRELAST_LINE_A = $0000C300;
  ANDROID_BUTTON_SUNKEN_PREPRELAST_LINE_B = $0000C300;
  ANDROID_BUTTON_SUNKEN_PRELAST_LINE_A = $0000CB00;
  ANDROID_BUTTON_SUNKEN_PRELAST_LINE_B = $0000CB00;
  ANDROID_BUTTON_SUNKEN_LAST_LINE_A = $0000D300;
  ANDROID_BUTTON_SUNKEN_LAST_LINE_B = $0000D700;

  // Checkbox

var
  ANDROID_CHECKBOX_A: array[0..29] of TColor =
    ($F5F5F5, $EBF0EC, $EBF0EC, $EBEBEB, $EBEBEB,
     $E1E6E2, $EBE6EA, $E1E1E1, $E1E1E1, $D6D0D7,
     $E1DDE0, $CDCECD, $D6D3D5, $CDCECD, $CDCECD,
     $C3C4C3, $CCC9CC, $C3C0C2, $C3C4C3, $BABFBB,
     $C3C0C2, $BABFBB, $BABBBA, $AFB0AF, $AFACAF,
     $AFACAF, $AFACAF, $AFB0AF, $BABDBA, $C3C4C3);
  ANDROID_CHECKBOX_B: array[0..29] of TColor =
    ($EBF0EC, $F5F0F4, $EBEBEB, $EBE6EA, $E1E6E2,
     $EBE6EA, $E1E1E1, $D6DCD7, $D6DCD7, $D6DCD7,
     $D6D7D6, $D5D3D6, $CDCECD, $CCC9CC, $C3C4C3,
     $C3C4C3, $C3C0C2, $C3C0C2, $BABFBB, $BABBBA,
     $BABBBA, $B9B6B9, $AFB6B1, $AFB0AF, $AFB0AF,
     $AFB0AF, $AFB0AF, $AFB0AF, $BABDBA, $CCC9CC);

const
  ANDROID_CHECKBOX_CORNER_DARK_GRAY = $585A58;
  ANDROID_CHECKBOX_CORNER_GRAY = $8A8C8A;

{procedure TCDButtonDrawerAndroid.DrawToIntfImage(ADest: TFPImageCanvas;
  CDButton: TCDButton);
begin

end;

procedure TCDButtonDrawerAndroid.DrawToCanvas(ADest: TCanvas; CDButton: TCDButton);
var
  //TmpB: TBitmap;
  Str: string;
begin
  // Button shape -> This crashes in Gtk2
(*  TmpB.Canvas.Brush.Color := CDButton.Color;
  TmpB.Canvas.Brush.Style := bsSolid;
  TmpB.Canvas.RoundRect(0, 0, TmpB.Width, TmpB.Height, 8, 8);
  CDButton.SetShape(TmpB);
  ADest.Draw(0, 0, TmpB);
  TmpB.Free;
  *)

  ADest.Brush.Color := CDButton.Parent.Color;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Color := ADest.Brush.Color;
  ADest.RecTangle(0, 0, CDButton.Width, CDButton.Height);

  // Button image
  if CDButton.IsDown then
    DrawCDButtonDown(ADest, CDButton.GetRGBBackgroundColor)
  else if CDButton.Focused then
    DrawAndroidButton(ADest, GetAColor(CDButton.Color, 98))
  else
    DrawAndroidButton(ADest, GetAColor(CDButton.Color, 96));

  // Button text
  ADest.Font.Assign(CDButton.Font);
  ADest.Brush.Style := bsClear;
  ADest.Pen.Style := psSolid;
  Str := CDButton.Caption;
  ADest.TextOut((CDButton.Width - ADest.TextWidth(Str)) div 2,
    (CDButton.Height - ADest.TextHeight(Str)) div 2, Str);
end;

initialization
  RegisterButtonDrawer(TCDButtonDrawerAndroid.Create, dsAndroid);}

{ TCDDrawerAndroid }

procedure TCDDrawerAndroid.DrawCheckBoxBitmap(ADest: TFPCustomCanvas; ADestPos: TPoint; AState: TCDControlState; ASize: Integer; ABackgroundColor: TFPColor);
var
  i, scaledI: Integer;
  lDest: TCanvas;
  lValue5, lValue7, lValue12, lValue17, lValue18, lValueSum24: Integer;
begin
  lDest := TCanvas(ADest);
  lValue5 := DPIAdjustment(5);
  lValue7 := DPIAdjustment(7);
  lValue12 := DPIAdjustment(12);
  lValue18 := DPIAdjustment(18);
  lValue17 := DPIAdjustment(17);
  lValueSum24 := lValue7+lValue17;

  // Background
  for i := 0 to ASize-1 do
  begin
    scaledI := Round(i * 30 / ASize);
    DrawAndroidAlternatedHorzLine(lDest, 0, ASize-1, i, ANDROID_CHECKBOX_A[scaledI], ANDROID_CHECKBOX_B[scaledI]);
  end;

{  // Corners >>> The corners look bad if the background isn't black
  ADest.Colors[ADestPos.X+0, ADestPos.Y+0] := colBlack;
  ADest.Colors[ADestPos.X+1, ADestPos.Y+0] := colBlack;
  ADest.Colors[ADestPos.X+0, ADestPos.Y+1] := colBlack;
  lDest.Pixels[ADestPos.X+0, ADestPos.Y+2] := ANDROID_CHECKBOX_CORNER_DARK_GRAY;
  lDest.Pixels[ADestPos.X+2, ADestPos.Y+0] := ANDROID_CHECKBOX_CORNER_DARK_GRAY;
  lDest.Pixels[ADestPos.X+1, ADestPos.Y+1] := ANDROID_CHECKBOX_CORNER_GRAY;
  //
  ADest.Colors[ADestPos.X+ASize-1, ADestPos.Y+0] := colBlack;
  ADest.Colors[ADestPos.X+ASize-2, ADestPos.Y+0] := colBlack;
  ADest.Colors[ADestPos.X+ASize-1, ADestPos.Y+1] := colBlack;
  lDest.Pixels[ADestPos.X+ASize-1, ADestPos.Y+2] := ANDROID_CHECKBOX_CORNER_DARK_GRAY;
  lDest.Pixels[ADestPos.X+ASize-3, ADestPos.Y+0] := ANDROID_CHECKBOX_CORNER_DARK_GRAY;
  lDest.Pixels[ADestPos.X+ASize-4, ADestPos.Y+1] := ANDROID_CHECKBOX_CORNER_GRAY;
  //
  ADest.Colors[ADestPos.X+0, ADestPos.Y+ASize-1] := colBlack;
  ADest.Colors[ADestPos.X+1, ADestPos.Y+ASize-1] := colBlack;
  ADest.Colors[ADestPos.X+0, ADestPos.Y+ASize-2] := colBlack;
  lDest.Pixels[ADestPos.X+0, ADestPos.Y+ASize-3] := ANDROID_CHECKBOX_CORNER_DARK_GRAY;
  lDest.Pixels[ADestPos.X+2, ADestPos.Y+ASize-1] := ANDROID_CHECKBOX_CORNER_DARK_GRAY;
  lDest.Pixels[ADestPos.X+1, ADestPos.Y+ASize-2] := ANDROID_CHECKBOX_CORNER_GRAY;
  //
  ADest.Colors[ADestPos.X+ASize-1, ADestPos.Y+ASize-1] := colBlack;
  ADest.Colors[ADestPos.X+ASize-2, ADestPos.Y+ASize-1] := colBlack;
  ADest.Colors[ADestPos.X+ASize-1, ADestPos.Y+ASize-2] := colBlack;
  lDest.Pixels[ADestPos.X+ASize-1, ADestPos.Y+ASize-3] := ANDROID_CHECKBOX_CORNER_DARK_GRAY;
  lDest.Pixels[ADestPos.X+ASize-3, ADestPos.Y+ASize-1] := ANDROID_CHECKBOX_CORNER_DARK_GRAY;
  lDest.Pixels[ADestPos.X+ASize-2, ADestPos.Y+ASize-2] := ANDROID_CHECKBOX_CORNER_GRAY;  }

  // Tickmark
  if csfOff in AState then
  begin
    // first 6 descending lines
    for i := 0 to lValue5 do
      DrawVerticalLineWithFirstLast(ADest, lValue7+i, lValue12+i, lValue18+i,
        TColorToFPColor($828081), TColorToFPColor($AFB0AF), TColorToFPColor($9D9E9D));
    // now 11 ascending lines
    for i := lValue5+1 to lValue17 do
      DrawVerticalLineWithFirstLast(ADest, lValue7+i, lValue12+lValue5*2-i, lValue18+lValue5*2-i,
        TColorToFPColor($939193), TColorToFPColor($AFB0AF), TColorToFPColor($9D9E9D));
    // left part adjusts
    lDest.Pixels[lValue7, lValue12] := $A6A7A6;
    lDest.Pixels[lValue7-1, lValue12+1] := $828482;
    lDest.Pixels[lValue7-2, lValue12+2] := $949193;
    lDest.Pixels[lValue7-2, lValue12+3] := $9D9E9D;
    lDest.Pixels[lValue7-2, lValue12+4] := $A6A7A6;
    lDest.Pixels[lValue7-1, lValue12+2] := $A6A3A5;
    lDest.Pixels[lValue7-1, lValue12+3] := $AFACAF;
    lDest.Pixels[lValue7-1, lValue12+4] := $A6A7A6;
    lDest.Pixels[lValue7-1, lValue12+5] := $9DA29E;
    for i := 1 to lValue18 - lValue12 - 6 do
    begin
      lDest.Pixels[lValue7-2, lValue12+4+i] := $A6A7A6;
      lDest.Pixels[lValue7-1, lValue12+5+i] := $9DA29E;
    end;
    // right part adjusts
    lDest.Pixels[lValueSum24, lValue12-6] := $9D9A9C;
    lDest.Pixels[lValueSum24, lValue12-5] := $AFBDAF;
    lDest.Pixels[lValueSum24, lValue12-4] := $BABBBA;
    lDest.Pixels[lValueSum24, lValue12-3] := $BABBBA;
    lDest.Pixels[lValueSum24, lValue12-2] := $B9B6B9;
    lDest.Pixels[lValueSum24, lValue12-1] := $9D9E9D;
    lDest.Pixels[lValueSum24+1,  lValue12-6] := $AFB0AF;
    lDest.Pixels[lValueSum24+1,  lValue12-5] := $A6A7A6;
    lDest.Pixels[lValueSum24+1,  lValue12-4] := $B9B6B9;
    lDest.Pixels[lValueSum24+1,  lValue12-3] := $BABBBA;
    lDest.Pixels[lValueSum24+1,  lValue12-2] := $9D9E9D;
    for i := 1 to lValue18 - lValue12 - 6 do
    begin
      lDest.Pixels[lValueSum24,  lValue12-6-i] := $9D9A9C;
      lDest.Pixels[lValueSum24+1,  lValue12-6-i] := $AFB0AF;
    end;
  end
  else
  begin
    // first 6 descending lines
    for i := 0 to lValue5 do
      DrawVerticalLineWithFirstLast(ADest, lValue7+i, lValue12+i, lValue18+i,
        TColorToFPColor($007500), TColorToFPColor($00D300), TColorToFPColor($089A08));
    // now 11 ascending lines
    for i := lValue5+1 to lValue17 do
      DrawVerticalLineWithFirstLast(ADest, lValue7+i, lValue12+lValue5*2-i, lValue18+lValue5*2-i,
        TColorToFPColor($009200), TColorToFPColor($00D300), TColorToFPColor($089A08));
    // left part adjusts
    lDest.Pixels[lValue7, lValue12] := $849E84;
    lDest.Pixels[lValue7-1, lValue12+1] := $187518;
    lDest.Pixels[lValue7-2, lValue12+2] := $188A18;
    lDest.Pixels[lValue7-2, lValue12+3] := $109E10;
    lDest.Pixels[lValue7-2, lValue12+4] := $73A273;
    lDest.Pixels[lValue7-1, lValue12+2] := $00A600;
    lDest.Pixels[lValue7-1, lValue12+3] := $00BE00;
    lDest.Pixels[lValue7-1, lValue12+4] := $00B200;
    lDest.Pixels[lValue7-1, lValue12+5] := $4A9E4A;
    for i := 1 to lValue18 - lValue12 - 6 do
    begin
      lDest.Pixels[lValue7-2, lValue12+4+i] := $73A273;
      lDest.Pixels[lValue7-1, lValue12+5+i] := $4A9E4A;
    end;
    // right part adjusts
    lDest.Pixels[lValueSum24, lValue12-6] := $427D42;
    lDest.Pixels[lValueSum24, lValue12-5] := $00A200;
    lDest.Pixels[lValueSum24, lValue12-4] := $00C700;
    lDest.Pixels[lValueSum24, lValue12-3] := $00B200;
    lDest.Pixels[lValueSum24, lValue12-2] := $31A231;
    lDest.Pixels[lValueSum24, lValue12-1] := $089A08;
    lDest.Pixels[lValueSum24+1,  lValue12-6] := $739E73;
    lDest.Pixels[lValueSum24+1,  lValue12-5] := $009200;
    lDest.Pixels[lValueSum24+1,  lValue12-4] := $00AA00;
    lDest.Pixels[lValueSum24+1,  lValue12-3] := $4AA64A;
    lDest.Pixels[lValueSum24+1,  lValue12-2] := $089A08;
    for i := 1 to lValue18 - lValue12 - 6 do
    begin
      lDest.Pixels[lValueSum24,  lValue12-6-i] := $427D42;
      lDest.Pixels[lValueSum24+1,  lValue12-6-i] := $739E73;
    end;
  end;

  DrawTransparentRoundCorners(ADest, ADestPos, Size(ASize, ASize), ABackgroundColor);
end;

procedure TCDDrawerAndroid.DrawTransparentRoundCorners(ADest: TFPCustomCanvas;
  ADestPos: TPoint; ASize: TSize; AColor: TFPColor);
begin
  ADest.Colors[ADestPos.X+0, ADestPos.Y+0] := AColor;
  ADest.Colors[ADestPos.X+1, ADestPos.Y+0] := AColor;
  ADest.Colors[ADestPos.X+0, ADestPos.Y+1] := AColor;
  ADest.Colors[ADestPos.X+ASize.cx-1, ADestPos.Y+0] := AColor;
  ADest.Colors[ADestPos.X+ASize.cx-2, ADestPos.Y+0] := AColor;
  ADest.Colors[ADestPos.X+ASize.cx-1, ADestPos.Y+1] := AColor;
  ADest.Colors[ADestPos.X+0, ADestPos.Y+ASize.cy-1] := AColor;
  ADest.Colors[ADestPos.X+1, ADestPos.Y+ASize.cy-1] := AColor;
  ADest.Colors[ADestPos.X+0, ADestPos.Y+ASize.cy-2] := AColor;
  ADest.Colors[ADestPos.X+ASize.cx-1, ADestPos.Y+ASize.cy-1] := AColor;
  ADest.Colors[ADestPos.X+ASize.cx-2, ADestPos.Y+ASize.cy-1] := AColor;
  ADest.Colors[ADestPos.X+ASize.cx-1, ADestPos.Y+ASize.cy-2] := AColor;
end;

procedure TCDDrawerAndroid.DrawVerticalLineWithFirstLast(
  ADest: TFPCustomCanvas; X, Y1, Y2: Integer; AColorTop, AColorMiddle,
  AColorEnd: TFPColor);
begin
  ADest.Colors[X, Y1] := AColorTop;
  ADest.Pen.FPColor := AColorMiddle;
  ADest.Pen.Style := psSolid;
  ADest.Line(X, Y1+1, X, Y2);
  ADest.Colors[X, Y2] := AColorEnd;
end;

procedure TCDDrawerAndroid.DrawAndroidAlternatedHorzLine(ADest: TCanvas;
  X1, X2, Y: Integer; AColor1, AColor2: TColor);
var
  i: Integer;
begin
  for i := X1 to X2-1 do
  begin
    if i mod 2 = 0 then ADest.Pixels[i, Y] := AColor1
    else ADest.Pixels[i, Y] := AColor2;
  end;
end;

procedure TCDDrawerAndroid.DrawAndroidMixedVertGradientFill(ADest: TCanvas;
  ARect: TRect; AStart1, AStop1, AStart2, AStop2: TColor);
var
  RStart1, RStop1, RStart2, RStop2: Byte;
  GStart1, GStop1, GStart2, GStop2: Byte;
  BStart1, BStop1, BStart2, BStop2: Byte;
  RDiff1, GDiff1, BDiff1: Integer;
  RDiff2, GDiff2, BDiff2: Integer;
  Count, I: Integer;
  lColor1, lColor2: TColor;
begin
  if IsRectEmpty(ARect) then Exit;

  RedGreenBlue(ColorToRGB(AStart1), RStart1, GStart1, BStart1);
  RedGreenBlue(ColorToRGB(AStop1),  RStop1,  GStop1,  BStop1);
  RedGreenBlue(ColorToRGB(AStart2), RStart2, GStart2, BStart2);
  RedGreenBlue(ColorToRGB(AStop2),  RStop2,  GStop2,  BStop2);

  RDiff1 := RStop1 - RStart1;
  GDiff1 := GStop1 - GStart1;
  BDiff1 := BStop1 - BStart1;

  RDiff2 := RStop2 - RStart2;
  GDiff2 := GStop2 - GStart2;
  BDiff2 := BStop2 - BStart2;

  Count := ARect.Bottom - ARect.Top;

  for I := 0 to Count-1 do
  begin
    lColor1 := RGBToColor(RStart1 + (i * RDiff1) div Count,
                            GStart1 + (i * GDiff1) div Count,
                            BStart1 + (i * BDiff1) div Count);
    lColor2 := RGBToColor(RStart2 + (i * RDiff2) div Count,
                            GStart2 + (i * GDiff2) div Count,
                            BStart2 + (i * BDiff2) div Count);

    // draw left to right, because LineTo does not draw last pixel
    DrawAndroidAlternatedHorzLine(ADest, ARect.Left, ARect.Right, ARect.Top+I, lColor1, lColor2);
  end;
end;

procedure TCDDrawerAndroid.DrawAndroidMixedFill(ADest: TCanvas; ARect: TRect;
  AColor1, AColor2: TColor);
var
  I: Integer;
  lColor1, lColor2: TColor;
begin
  for I := 0 to (ARect.Bottom - ARect.Top)-1 do
  begin
    if i mod 2 = 0 then
    begin
      lColor1 := AColor1;
      lColor2 := AColor2;
    end
    else
    begin
      lColor1 := AColor2;
      lColor2 := AColor1;
    end;
    // draw left to right, because LineTo does not draw last pixel
    DrawAndroidAlternatedHorzLine(ADest, ARect.Left, ARect.Right, ARect.Top+I, lColor1, lColor2);
  end;
end;

procedure TCDDrawerAndroid.DrawAndroidGradientCircle(ADest: TCanvas;
  ADestPos: TPoint; ASize: TSize; ABorderColor, ATopColor, ABottomColor: TColor);
var
  i, x, y, center_x, center_y, radius, Count: Integer;
  RStart1, RStop1: Byte;
  GStart1, GStop1: Byte;
  BStart1, BStop1: Byte;
  RDiff1, GDiff1, BDiff1: Integer;
  lColor: TColor;
begin
  center_x := ADestPos.X + ASize.cx div 2;
  center_y := ADestPos.Y + ASize.cy div 2;
  radius := ASize.cx div 2;
  Count := ASize.cx;

  RedGreenBlue(ColorToRGB(ATopColor), RStart1, GStart1, BStart1);
  RedGreenBlue(ColorToRGB(ABottomColor),  RStop1,  GStop1,  BStop1);

  RDiff1 := RStop1 - RStart1;
  GDiff1 := GStop1 - GStart1;
  BDiff1 := BStop1 - BStart1;

  // First the inside part
  for x := ADestPos.X to ADestPos.X+ASize.CX-1 do
    for y := ADestPos.Y to ADestPos.Y+ASize.CY-1 do
    begin
      if Sqr(x-center_x) + Sqr(y - center_y) < Sqr(radius) then
      begin
        i := Y - ADestPos.Y;
        lColor := RGBToColor(RStart1 + (i * RDiff1) div Count,
                             GStart1 + (i * GDiff1) div Count,
                             BStart1 + (i * BDiff1) div Count);
        ADest.Pixels[x, y] := lColor;
      end;
    end;

  // Now the border
  ADest.Pen.Style := psSolid;
  ADest.Pen.Color := ABorderColor;
  ADest.Brush.Style := bsClear;
  ADest.Ellipse(ADestPos.X, ADestPos.Y, ADestPos.X+ASize.CX, ADestPos.Y+ASize.CY);
end;

procedure TCDDrawerAndroid.CreateResources;
begin
{  bmpCheckbox := TBitmap.Create;
  bmpCheckboxChecked := TBitmap.Create;}
end;

procedure TCDDrawerAndroid.LoadResources;
begin
(*  {$ifdef CD_UseImageResources}
  bmpCheckbox.LoadFromLazarusResource('android_checkbox');
  bmpCheckboxChecked.LoadFromLazarusResource('android_checkbox_checked');
  {$else}
  bmpCheckbox.Width := 30;
  bmpCheckbox.Height := 30;
  bmpCheckboxChecked.Width := 30;
  bmpCheckboxChecked.Height := 30;
  DrawCheckBoxBitmap(bmpCheckbox.Canvas, Point(0, 0), [csfOff], 30);
  DrawCheckBoxBitmap(bmpCheckboxChecked.Canvas, Point(0, 0), [csfOn], 30);
  {$endif}

  // DPI adjustment
  lDPI := Max(96, Screen.PixelsPerInch);
  ScaleRasterImage(bmpCheckbox, 160, lDPI);
  ScaleRasterImage(bmpCheckboxChecked, 160, lDPI);*)
end;

procedure TCDDrawerAndroid.FreeResources;
begin
{  bmpCheckbox.Free;
  bmpCheckboxChecked.Free;}
end;

function TCDDrawerAndroid.GetDrawStyle: TCDDrawStyle;
begin
  Result := dsAndroid;
end;

function TCDDrawerAndroid.GetMeasures(AMeasureID: Integer): Integer;
begin
  Result := 0;
  case AMeasureID of
{  TCDEDIT_LEFT_TEXT_SPACING: Result := 4;
  TCDEDIT_RIGHT_TEXT_SPACING: Result := 3;
  TCDEDIT_TOP_TEXT_SPACING: Result := 3;
  TCDEDIT_BOTTOM_TEXT_SPACING: Result := 3;}
  //
  TCDCHECKBOX_SQUARE_HALF_HEIGHT: Floor(GetMeasures(TCDCHECKBOX_SQUARE_HEIGHT)/2);
  TCDCHECKBOX_SQUARE_HEIGHT: Result := DPIAdjustment(30);
  //
  TCDRADIOBUTTON_CIRCLE_HEIGHT: Result := DPIAdjustment(20); // Must be dividable by 4
  //
  TCDCOMBOBOX_DEFAULT_HEIGHT: Result := 50;
  //
{  TCDSCROLLBAR_BUTTON_WIDTH: Result := 17;
  TCDSCROLLBAR_LEFT_SPACING: Result := 17;
  TCDSCROLLBAR_RIGHT_SPACING: Result := 17;
  TCDSCROLLBAR_LEFT_BUTTON_POS: Result := 0;
  TCDSCROLLBAR_RIGHT_BUTTON_POS: Result := -17;
  //
  TCDTRACKBAR_LEFT_SPACING: Result := 9;
  TCDTRACKBAR_RIGHT_SPACING: Result := 9;
  TCDTRACKBAR_TOP_SPACING: Result := 5;
  TCDTRACKBAR_FRAME_HEIGHT: Result := 17;
  //
  TCDLISTVIEW_COLUMN_LEFT_SPACING:  Result := 10;
  TCDLISTVIEW_COLUMN_RIGHT_SPACING: Result := 10;
  TCDLISTVIEW_COLUMN_TEXT_LEFT_SPACING:  Result := 5;
  TCDLISTVIEW_LINE_TOP_SPACING: Result := 3;
  TCDLISTVIEW_LINE_BOTTOM_SPACING: Result := 3;}
  else
    Result := inherited GetMeasures(AMeasureID);
  end;
end;

procedure TCDDrawerAndroid.DrawTickmark(ADest: TFPCustomCanvas; ADestPos: TPoint; AState: TCDControlState);
begin

end;

procedure TCDDrawerAndroid.DrawButton(ADest: TFPCustomCanvas; ADestPos: TPoint; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDButtonStateEx);
var
  lDest: TCanvas absolute ADest;
  Str: string;
  lGlyphLeftSpacing: Integer = 0;
  lTextOutPos: TPoint;
  lGlyphCaptionHeight: Integer;
  lRect: TRect;
begin
  if not (ADest is TCanvas) then Exit; // ToDo

{  // Darker corners >>> Don't draw them, they look bad in a light background
  lColor := ANDROID_BUTTON_CORNERS;
  lDest.Pixels[1, 1] := lColor;
  lDest.Pixels[2, 0] := lColor;
  lDest.Pixels[0, 2] := lColor;
  lDest.Pixels[ASize.cx-3, 0] := lColor;
  lDest.Pixels[ASize.cx-2, 1] := lColor;
  lDest.Pixels[ASize.cx-1, 2] := lColor;
  lDest.Pixels[0, ASize.cy-3] := lColor;
  lDest.Pixels[1, ASize.cy-2] := lColor;
  lDest.Pixels[2, ASize.cy-1] := lColor;
  lDest.Pixels[ASize.cx-1, ASize.cy-3] := lColor;
  lDest.Pixels[ASize.cx-2, ASize.cy-2] := lColor;
  lDest.Pixels[ASize.cx-3, ASize.cy-1] := lColor; }

  // Button image
  if csfSunken in AState then
  begin
    // Top lines
    DrawAndroidAlternatedHorzLine(lDest, 0, ASize.cx, 0, ANDROID_BUTTON_SUNKEN_FIRST_LINE_A, ANDROID_BUTTON_SUNKEN_FIRST_LINE_B);
    DrawAndroidAlternatedHorzLine(lDest, 0, ASize.cx, 1, ANDROID_BUTTON_SUNKEN_SECOND_LINE_A, ANDROID_BUTTON_SUNKEN_SECOND_LINE_B);
    DrawAndroidAlternatedHorzLine(lDest, 0, ASize.cx, 2, ANDROID_BUTTON_SUNKEN_THIRD_LINE_A, ANDROID_BUTTON_SUNKEN_THIRD_LINE_B);

    // The central gradient
    lRect := Bounds(0, 3, ASize.cx, (ASize.cy-6) div 3+1);
    DrawAndroidMixedVertGradientFill(lDest, lRect, ANDROID_BUTTON_SUNKEN_TOP_GRADIENT_A,
      ANDROID_BUTTON_SUNKEN_MIDDLE_GRADIENT_A, ANDROID_BUTTON_SUNKEN_TOP_GRADIENT_B, ANDROID_BUTTON_SUNKEN_MIDDLE_GRADIENT_B);
    lRect := Bounds(0, 3+(ASize.cy-6) div 3, ASize.cx, (ASize.cy-6) div 3+1);
    DrawAndroidMixedFill(lDest, lRect, ANDROID_BUTTON_SUNKEN_MIDDLE_GRADIENT_A, ANDROID_BUTTON_SUNKEN_MIDDLE_GRADIENT_B);
    lRect := Bounds(0, 3+2*(ASize.cy-6) div 3, ASize.cx, (ASize.cy-6) div 3+1);
    DrawAndroidMixedVertGradientFill(lDest, lRect, ANDROID_BUTTON_SUNKEN_MIDDLE_GRADIENT_A,
      ANDROID_BUTTON_SUNKEN_BOTTOM_GRADIENT_A, ANDROID_BUTTON_SUNKEN_MIDDLE_GRADIENT_B, ANDROID_BUTTON_SUNKEN_BOTTOM_GRADIENT_B);

    // Bottom lines
    DrawAndroidAlternatedHorzLine(lDest, 0, ASize.cx, ASize.cy-3, ANDROID_BUTTON_SUNKEN_PREPRELAST_LINE_A, ANDROID_BUTTON_SUNKEN_PREPRELAST_LINE_B);
    DrawAndroidAlternatedHorzLine(lDest, 0, ASize.cx, ASize.cy-2, ANDROID_BUTTON_SUNKEN_PRELAST_LINE_A, ANDROID_BUTTON_SUNKEN_PRELAST_LINE_B);
    DrawAndroidAlternatedHorzLine(lDest, 0, ASize.cx, ASize.cy-1, ANDROID_BUTTON_SUNKEN_LAST_LINE_A, ANDROID_BUTTON_SUNKEN_LAST_LINE_B);
  end
  else
  begin
    // Top lines
    DrawAndroidAlternatedHorzLine(lDest, 0, ASize.cx, 0, ANDROID_BUTTON_FIRST_LINE_A, ANDROID_BUTTON_FIRST_LINE_B);
    DrawAndroidAlternatedHorzLine(lDest, 0, ASize.cx, 1, ANDROID_BUTTON_SECOND_LINE_A, ANDROID_BUTTON_SECOND_LINE_B);
    DrawAndroidAlternatedHorzLine(lDest, 0, ASize.cx, 2, ANDROID_BUTTON_THIRD_LINE_A, ANDROID_BUTTON_THIRD_LINE_B);

    // The central gradient
    lRect := Bounds(0, 3, ASize.cx, (ASize.cy-6) div 3+1);
    DrawAndroidMixedVertGradientFill(lDest, lRect, ANDROID_BUTTON_TOP_GRADIENT_A,
      ANDROID_BUTTON_MIDDLE_GRADIENT_A, ANDROID_BUTTON_TOP_GRADIENT_B, ANDROID_BUTTON_MIDDLE_GRADIENT_B);
    lRect := Bounds(0, 3+(ASize.cy-6) div 3, ASize.cx, (ASize.cy-6) div 3+1);
    DrawAndroidMixedFill(lDest, lRect, ANDROID_BUTTON_MIDDLE_GRADIENT_A, ANDROID_BUTTON_MIDDLE_GRADIENT_B);
    lRect := Bounds(0, 3+2*(ASize.cy-6) div 3, ASize.cx, (ASize.cy-6) div 3+1);
    DrawAndroidMixedVertGradientFill(lDest, lRect, ANDROID_BUTTON_MIDDLE_GRADIENT_A,
      ANDROID_BUTTON_BOTTOM_GRADIENT_A, ANDROID_BUTTON_MIDDLE_GRADIENT_B, ANDROID_BUTTON_BOTTOM_GRADIENT_B);

    // Bottom lines
    DrawAndroidAlternatedHorzLine(lDest, 0, ASize.cx, ASize.cy-3, ANDROID_BUTTON_PREPRELAST_LINE_A, ANDROID_BUTTON_PREPRELAST_LINE_B);
    DrawAndroidAlternatedHorzLine(lDest, 0, ASize.cx, ASize.cy-2, ANDROID_BUTTON_PRELAST_LINE_A, ANDROID_BUTTON_PRELAST_LINE_B);
    DrawAndroidAlternatedHorzLine(lDest, 0, ASize.cx, ASize.cy-1, ANDROID_BUTTON_LAST_LINE_A, ANDROID_BUTTON_LAST_LINE_B);
  end;

  // Background corners
  DrawTransparentRoundCorners(ADest, Point(0, 0), ASize, AStateEx.FPParentRGBColor);

  // The full focus rect is too invasive for Android, just make a underline font style instead
  //if csfHasFocus in AState then
  //  DrawFocusRect(lDest, Point(5, 5), Size(ASize.cx-10, ASize.cy-10));

  // Position calculations
  ADest.Font.Assign(AStateEx.Font);
  if csfHasFocus in AState then
    ADest.Font.Underline := True;
  Str := AStateEx.Caption;
  lGlyphCaptionHeight := Max(lDest.TextHeight(Str), AStateEx.Glyph.Height);
  lTextOutPos.X := (ASize.cx - lDest.TextWidth(Str) - AStateEx.Glyph.Width) div 2;
  lTextOutPos.Y := (ASize.cy - lGlyphCaptionHeight) div 2;
  lTextOutPos.X := Max(lTextOutPos.X, 5);
  lTextOutPos.Y := Max(lTextOutPos.Y, 5);

  // Button glyph
  if not AStateEx.Glyph.Empty then
  begin
    lDest.Draw(lTextOutPos.X, lTextOutPos.Y, AStateEx.Glyph);
    lGlyphLeftSpacing := AStateEx.Glyph.Width+5;
  end;

  // Button text
  lTextOutPos.X := lTextOutPos.X + lGlyphLeftSpacing;
  lTextOutPos.Y := (ASize.cy - lDest.TextHeight(Str)) div 2;
  ADest.Brush.Style := bsClear;
  ADest.Pen.Style := psSolid;
  if csfSunken in AState then
  begin
    Inc(lTextOutPos.X);
    Inc(lTextOutPos.Y);
  end;
  lDest.TextOut(lTextOutPos.X, lTextOutPos.Y, Str)
end;

procedure TCDDrawerAndroid.DrawEditBackground(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDEditStateEx);
var
  lRect: TRect;
begin
  // The first half is a gradient
  lRect := Bounds(3, 3, ASize.cx-5, (ASize.cy-5) div 2);
  ADest.GradientFill(lRect, $D8DAD6, $F3F6F4, gdVertical);
  // The second is a plain color
  lRect := Bounds(3, 3+(ASize.cy-6) div 2, ASize.cx-6, (ASize.cy-5) div 2);
  ADest.Brush.Color := $F3F6F4;
  ADest.Brush.Style := bsSolid;
  ADest.FillRect(lRect);
end;

procedure TCDDrawerAndroid.DrawEditFrame(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDEditStateEx);
begin
  ADest.Pen.Style := psSolid;
  if csfHasFocus in AState then
  begin
    // Top lines
    ADest.Pen.Color := $6BB0FF;
    ADest.Line(2, 0, ASize.cx-1, 0);
    ADest.Pen.Color := $3E9FFE;
    ADest.Line(1, 1, ASize.cx, 1);
    ADest.Pen.Color := $2897FD;
    ADest.Line(0, 2, ASize.cx+1, 2);
    // Left&Right
    ADest.Pen.Color := $007FFE;
    ADest.Line(0, 3, 0, ASize.CY-2);
    ADest.Line(1, 3, 1, ASize.CY-2);
    ADest.Line(ASize.cx-1, 3, ASize.cx-1, ASize.CY-2);
    ADest.Line(ASize.cx-2, 3, ASize.cx-2, ASize.CY-2);
    ADest.Pen.Color := $96B1CA;
    ADest.Line(2, 3, 2, ASize.CY-2);
    ADest.Line(ASize.cx-3, 3, ASize.cx-3, ASize.CY-2);
    // Bottom
    ADest.Pen.Color := $0075FD;
    ADest.Line(0, ASize.cy-3, ASize.cx-1, ASize.cy-3);
    ADest.Pen.Color := $0079F6;
    ADest.Line(1, ASize.cy-2, ASize.cx-2, ASize.cy-2);
    ADest.Pen.Color := $007FFE;
    ADest.Line(2, ASize.cy-1, ASize.cx-3, ASize.cy-1);
  end
  else
  begin
    // Top lines
    ADest.Pen.Color := $737674;
    ADest.Line(2, 0, ASize.cx-1, 0);
    ADest.Pen.Color := $B6B9B7;
    ADest.Line(1, 1, ASize.cx, 1);
    ADest.Pen.Color := $C3C6C4;
    ADest.Line(0, 2, ASize.cx+1, 2);
    // Left&Right
    ADest.Pen.Color := $565956;
    ADest.Line(0, 3, 0, ASize.CY-2);
    ADest.Line(ASize.cx-1, 3, ASize.cx-1, ASize.CY-2);
    ADest.Pen.Color := $D1D4D1;
    ADest.Line(1, 3, 1, ASize.CY-2);
    ADest.Line(ASize.cx-2, 3, ASize.cx-2, ASize.CY-2);
    ADest.Pen.Color := $DCE0DC;
    ADest.Line(2, 3, 2, ASize.CY-2);
    ADest.Line(ASize.cx-3, 3, ASize.cx-3, ASize.CY-2);
    // Bottom
    ADest.Pen.Color := $D4D7D5;
    ADest.Line(0, ASize.cy-3, ASize.cx-1, ASize.cy-3);
    ADest.Pen.Color := $D4D7D5;// is actually $FCFFFD but looks strange
    ADest.Line(1, ASize.cy-2, ASize.cx-2, ASize.cy-2);
    ADest.Pen.Color := $3B3E3C;
    ADest.Line(2, ASize.cy-1, ASize.cx-3, ASize.cy-1);
  end;

  // Transparent corners
  DrawTransparentRoundCorners(ADest, ADestPos, ASize, AStateEx.FPParentRGBColor);
end;

procedure TCDDrawerAndroid.DrawCheckBoxSquare(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDControlStateEx);
var
  lCheckboxSquare: Integer;
begin
  lCheckboxSquare := GetMeasures(TCDCHECKBOX_SQUARE_HEIGHT);

  //if csfOn in AState then ADest.Draw(0, 0, bmpCheckboxChecked)
  //else ADest.Draw(0, 0, bmpCheckbox);

  DrawCheckBoxBitmap(ADest, ADestPos, AState, lCheckboxSquare, AStateEx.FPParentRGBColor);

  // Transparent corners
  DrawTransparentRoundCorners(ADest, ADestPos,
    Size(lCheckboxSquare, lCheckboxSquare), AStateEx.FPParentRGBColor);
end;

procedure TCDDrawerAndroid.DrawRadioButtonCircle(ADest: TCanvas;
  ADestPos: TPoint; ASize: TSize; AState: TCDControlState;
  AStateEx: TCDControlStateEx);
var
  lSize: TSize;
begin
  lSize.cx := GetMeasures(TCDRADIOBUTTON_CIRCLE_HEIGHT);
  lSize.cy := lSize.cx;
  // external circle
  DrawAndroidGradientCircle(ADest, ADestPos, lSize, $848484, $EFEFEF, $BDBDBD);
  // internal circle
  if csfOn in AState then
    DrawAndroidGradientCircle(ADest, Point(ADestPos.X+lSize.cx div 4, ADestPos.y+lSize.cy div 4),
      Size(lSize.cx div 2, lSize.cy div 2), $317931, $21DB10, $00BE00)
  else
    DrawAndroidGradientCircle(ADest, Point(ADestPos.X+lSize.cx div 4, ADestPos.y+lSize.cy div 4),
      Size(lSize.cx div 2, lSize.cy div 2), $9C9A9C, $CECECE, $BDBDBD);
end;

initialization
  {$ifdef CD_UseImageResources}
  {$include customdrawnimages/android.lrs}
  {$endif}
  RegisterDrawer(TCDDrawerAndroid.Create, dsAndroid);
end.

