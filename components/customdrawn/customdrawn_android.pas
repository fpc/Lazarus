unit customdrawn_android;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils, Types, Math,
  // fpimage
  fpcanvas, fpimgcanv, fpimage,
  // LCL -> Use only TForm, TWinControl, TCanvas and TLazIntfImage
  Graphics, Controls, LCLType, LCLIntf, IntfGraphics, LResources,
  //
  customdrawndrawers, customdrawn_common;

type

  { TCDDrawerAndroid }

  TCDDrawerAndroid = class(TCDDrawerCommon)
  private
    bmpCheckbox, bmpCheckboxChecked: TBitmap;
    // Draws a line alternating between two colors
    procedure DrawAndroidAlternatedHorzLine(ADest: TCanvas; X1, X2,
      Y: Integer; AColor1, AColor2: TColor);
    // Draws 2 mixed gradients which alternate between one another on each pixel
    procedure DrawAndroidMixedVertGradientFill(ADest: TCanvas; ARect: TRect;
      AStart1, AStop1, AStart2, AStop2: TColor);
    // Fills a rectangular area alternating between two pixels
    procedure DrawAndroidMixedFill(ADest: TCanvas; ARect: TRect; AColor1, AColor2: TColor);
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
    procedure DrawTickmark(ADest: TCanvas; ADestPos: TPoint); override;
    {procedure DrawSlider(ADest: TCanvas; ADestPos: TPoint; ASize: TSize; AState: TCDControlState); override;
    procedure DrawCompactArrow(ADest: TCanvas; ADestPos: TPoint; ADirection: TCDControlState); override;}
    // ===================================
    // Standard Tab
    // ===================================
    // TCDButton
    procedure DrawButton(ADest: TCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDButtonStateEx); override;
{    // TCDEdit
    procedure DrawEditBackground(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;}
    // TCDCheckBox
    procedure DrawCheckBoxSquare(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
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
{  TmpB.Canvas.Brush.Color := CDButton.Color;
  TmpB.Canvas.Brush.Style := bsSolid;
  TmpB.Canvas.RoundRect(0, 0, TmpB.Width, TmpB.Height, 8, 8);
  CDButton.SetShape(TmpB);
  ADest.Draw(0, 0, TmpB);
  TmpB.Free;
  }

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

procedure TCDDrawerAndroid.CreateResources;
begin
  bmpCheckbox := TBitmap.Create;
  bmpCheckboxChecked := TBitmap.Create;
end;

procedure TCDDrawerAndroid.LoadResources;
begin
  bmpCheckbox.LoadFromLazarusResource('android_checkbox');
  bmpCheckboxChecked.LoadFromLazarusResource('android_checkbox_checked');

  // for now hardcoded to ldpi
  ScaleRasterImage(bmpCheckbox, 160, 96);
  ScaleRasterImage(bmpCheckboxChecked, 160, 96);
end;

procedure TCDDrawerAndroid.FreeResources;
begin
  bmpCheckbox.Free;
  bmpCheckboxChecked.Free;
end;

function TCDDrawerAndroid.GetDrawStyle: TCDDrawStyle;
begin
  Result := dsAndroid;
end;

function TCDDrawerAndroid.GetMeasures(AMeasureID: Integer): Integer;
begin
  case AMeasureID of
{  TCDEDIT_LEFT_TEXT_SPACING: Result := 4;
  TCDEDIT_RIGHT_TEXT_SPACING: Result := 3;
  TCDEDIT_TOP_TEXT_SPACING: Result := 3;
  TCDEDIT_BOTTOM_TEXT_SPACING: Result := 3;}
  //
  TCDCHECKBOX_SQUARE_HALF_HEIGHT: Result := 9;
  TCDCHECKBOX_SQUARE_HEIGHT: Result := 18;
{  //
  TCDRADIOBUTTON_CIRCLE_HEIGHT: Result := 15;
  //
  TCDSCROLLBAR_BUTTON_WIDTH: Result := 17;
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

procedure TCDDrawerAndroid.DrawTickmark(ADest: TCanvas; ADestPos: TPoint);
begin
  // Don't draw anything, tickmarks are impressed into the general images
end;

procedure TCDDrawerAndroid.DrawButton(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDButtonStateEx);
var
  Str: string;
  lGlyphLeftSpacing: Integer = 0;
  lTextOutPos: TPoint;
  lGlyphCaptionHeight: Integer;
  lColor: TColor;
  lRect: TRect;
begin
  // Background corners
  lColor := AStateEx.ParentRGBColor;
  ADest.Pixels[0, 0] := lColor;
  ADest.Pixels[1, 0] := lColor;
  ADest.Pixels[0, 1] := lColor;
  ADest.Pixels[ASize.cx-1, 0] := lColor;
  ADest.Pixels[ASize.cx-2, 0] := lColor;
  ADest.Pixels[ASize.cx-1, 1] := lColor;
  ADest.Pixels[0, ASize.cy-1] := lColor;
  ADest.Pixels[1, ASize.cy-1] := lColor;
  ADest.Pixels[0, ASize.cy-2] := lColor;
  ADest.Pixels[ASize.cx-1, ASize.cy-1] := lColor;
  ADest.Pixels[ASize.cx-2, ASize.cy-1] := lColor;
  ADest.Pixels[ASize.cx-1, ASize.cy-2] := lColor;

  // Darker corners
  lColor := ANDROID_BUTTON_CORNERS;
  ADest.Pixels[1, 1] := lColor;
  ADest.Pixels[2, 0] := lColor;
  ADest.Pixels[0, 2] := lColor;
  ADest.Pixels[ASize.cx-3, 0] := lColor;
  ADest.Pixels[ASize.cx-2, 1] := lColor;
  ADest.Pixels[ASize.cx-1, 2] := lColor;
  ADest.Pixels[0, ASize.cy-3] := lColor;
  ADest.Pixels[1, ASize.cy-2] := lColor;
  ADest.Pixels[2, ASize.cy-1] := lColor;
  ADest.Pixels[ASize.cx-1, ASize.cy-3] := lColor;
  ADest.Pixels[ASize.cx-2, ASize.cy-2] := lColor;
  ADest.Pixels[ASize.cx-3, ASize.cy-1] := lColor;

  // Button image
  if csfSunken in AState then
  begin
    // Top lines
    DrawAndroidAlternatedHorzLine(ADest, 3, ASize.cx-3, 0, ANDROID_BUTTON_SUNKEN_FIRST_LINE_A, ANDROID_BUTTON_SUNKEN_FIRST_LINE_B);
    DrawAndroidAlternatedHorzLine(ADest, 2, ASize.cx-2, 1, ANDROID_BUTTON_SUNKEN_SECOND_LINE_A, ANDROID_BUTTON_SUNKEN_SECOND_LINE_B);
    DrawAndroidAlternatedHorzLine(ADest, 1, ASize.cx-1, 2, ANDROID_BUTTON_SUNKEN_THIRD_LINE_A, ANDROID_BUTTON_SUNKEN_THIRD_LINE_B);

    // The central gradient
    lRect := Bounds(0, 3, ASize.cx, (ASize.cy-6) div 3+1);
    DrawAndroidMixedVertGradientFill(ADest, lRect, ANDROID_BUTTON_SUNKEN_TOP_GRADIENT_A,
      ANDROID_BUTTON_SUNKEN_MIDDLE_GRADIENT_A, ANDROID_BUTTON_SUNKEN_TOP_GRADIENT_B, ANDROID_BUTTON_SUNKEN_MIDDLE_GRADIENT_B);
    lRect := Bounds(0, 3+(ASize.cy-6) div 3, ASize.cx, (ASize.cy-6) div 3+1);
    DrawAndroidMixedFill(ADest, lRect, ANDROID_BUTTON_SUNKEN_MIDDLE_GRADIENT_A, ANDROID_BUTTON_SUNKEN_MIDDLE_GRADIENT_B);
    lRect := Bounds(0, 3+2*(ASize.cy-6) div 3, ASize.cx, (ASize.cy-6) div 3+1);
    DrawAndroidMixedVertGradientFill(ADest, lRect, ANDROID_BUTTON_SUNKEN_MIDDLE_GRADIENT_A,
      ANDROID_BUTTON_SUNKEN_BOTTOM_GRADIENT_A, ANDROID_BUTTON_SUNKEN_MIDDLE_GRADIENT_B, ANDROID_BUTTON_SUNKEN_BOTTOM_GRADIENT_B);

    // Bottom lines
    DrawAndroidAlternatedHorzLine(ADest, 1, ASize.cx-1, ASize.cy-3, ANDROID_BUTTON_SUNKEN_PREPRELAST_LINE_A, ANDROID_BUTTON_SUNKEN_PREPRELAST_LINE_B);
    DrawAndroidAlternatedHorzLine(ADest, 2, ASize.cx-2, ASize.cy-2, ANDROID_BUTTON_SUNKEN_PRELAST_LINE_A, ANDROID_BUTTON_SUNKEN_PRELAST_LINE_B);
    DrawAndroidAlternatedHorzLine(ADest, 3, ASize.cx-3, ASize.cy-1, ANDROID_BUTTON_SUNKEN_LAST_LINE_A, ANDROID_BUTTON_SUNKEN_LAST_LINE_B);
  end
  else
  begin
    // Top lines
    DrawAndroidAlternatedHorzLine(ADest, 3, ASize.cx-3, 0, ANDROID_BUTTON_FIRST_LINE_A, ANDROID_BUTTON_FIRST_LINE_B);
    DrawAndroidAlternatedHorzLine(ADest, 2, ASize.cx-2, 1, ANDROID_BUTTON_SECOND_LINE_A, ANDROID_BUTTON_SECOND_LINE_B);
    DrawAndroidAlternatedHorzLine(ADest, 1, ASize.cx-1, 2, ANDROID_BUTTON_THIRD_LINE_A, ANDROID_BUTTON_THIRD_LINE_B);

    // The central gradient
    lRect := Bounds(0, 3, ASize.cx, (ASize.cy-6) div 3+1);
    DrawAndroidMixedVertGradientFill(ADest, lRect, ANDROID_BUTTON_TOP_GRADIENT_A,
      ANDROID_BUTTON_MIDDLE_GRADIENT_A, ANDROID_BUTTON_TOP_GRADIENT_B, ANDROID_BUTTON_MIDDLE_GRADIENT_B);
    lRect := Bounds(0, 3+(ASize.cy-6) div 3, ASize.cx, (ASize.cy-6) div 3+1);
    DrawAndroidMixedFill(ADest, lRect, ANDROID_BUTTON_MIDDLE_GRADIENT_A, ANDROID_BUTTON_MIDDLE_GRADIENT_B);
    lRect := Bounds(0, 3+2*(ASize.cy-6) div 3, ASize.cx, (ASize.cy-6) div 3+1);
    DrawAndroidMixedVertGradientFill(ADest, lRect, ANDROID_BUTTON_MIDDLE_GRADIENT_A,
      ANDROID_BUTTON_BOTTOM_GRADIENT_A, ANDROID_BUTTON_MIDDLE_GRADIENT_B, ANDROID_BUTTON_BOTTOM_GRADIENT_B);

    // Bottom lines
    DrawAndroidAlternatedHorzLine(ADest, 1, ASize.cx-1, ASize.cy-3, ANDROID_BUTTON_PREPRELAST_LINE_A, ANDROID_BUTTON_PREPRELAST_LINE_B);
    DrawAndroidAlternatedHorzLine(ADest, 2, ASize.cx-2, ASize.cy-2, ANDROID_BUTTON_PRELAST_LINE_A, ANDROID_BUTTON_PRELAST_LINE_B);
    DrawAndroidAlternatedHorzLine(ADest, 3, ASize.cx-3, ASize.cy-1, ANDROID_BUTTON_LAST_LINE_A, ANDROID_BUTTON_LAST_LINE_B);
  end;

  if csfHasFocus in AState then
    DrawFocusRect(ADest, Point(5, 5), Size(ASize.cx-10, ASize.cy-10));

  // Position calculations
  ADest.Font.Assign(AStateEx.Font);
  Str := AStateEx.Caption;
  lGlyphCaptionHeight := Max(ADest.TextHeight(Str), AStateEx.Glyph.Height);
  lTextOutPos.X := (ASize.cx - ADest.TextWidth(Str) - AStateEx.Glyph.Width) div 2;
  lTextOutPos.Y := (ASize.cy - lGlyphCaptionHeight) div 2;
  lTextOutPos.X := Max(lTextOutPos.X, 5);
  lTextOutPos.Y := Max(lTextOutPos.Y, 5);

  // Button glyph
  if not AStateEx.Glyph.Empty then
  begin
    ADest.Draw(lTextOutPos.X, lTextOutPos.Y, AStateEx.Glyph);
    lGlyphLeftSpacing := AStateEx.Glyph.Width+5;
  end;

  // Button text
  lTextOutPos.X := lTextOutPos.X + lGlyphLeftSpacing;
  lTextOutPos.Y := (ASize.cy - ADest.TextHeight(Str)) div 2;
  ADest.Brush.Style := bsClear;
  ADest.Pen.Style := psSolid;
  if csfSunken in AState then
  begin
    Inc(lTextOutPos.X);
    Inc(lTextOutPos.Y);
  end;
  ADest.TextOut(lTextOutPos.X, lTextOutPos.Y, Str)
end;

procedure TCDDrawerAndroid.DrawCheckBoxSquare(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDControlStateEx);
begin
  if csfOn in AState then ADest.Draw(0, 0, bmpCheckboxChecked)
  else ADest.Draw(0, 0, bmpCheckbox);
end;

initialization
  {$I customdrawnimages/android.lrs}

  RegisterDrawer(TCDDrawerAndroid.Create, dsAndroid);
end.

