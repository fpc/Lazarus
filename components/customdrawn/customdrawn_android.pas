unit customdrawn_android;

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
    bmpCheckbox, bmpCheckboxChecked: TBitmap;
    // Alternative checkbox drawing, not currently utilized
    procedure DrawCheckBoxBitmap(ADest: TFPCustomCanvas; AState: TCDControlState);
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
    procedure DrawButton(ADest: TFPCustomCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDButtonStateEx); override;
    // TCDEdit
    procedure DrawEditBackground(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
    procedure DrawEditFrame(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
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

procedure TCDDrawerAndroid.DrawCheckBoxBitmap(ADest: TFPCustomCanvas; AState: TCDControlState);
var
  i: Integer;
  lDest: TCanvas;
begin
  lDest := TCanvas(ADest);
  // Background
  for i := 0 to 29 do
    DrawAndroidAlternatedHorzLine(lDest, 0, 31, i, ANDROID_CHECKBOX_A[i], ANDROID_CHECKBOX_B[i]);

  // Corners
  ADest.Colors[0, 0] := colBlack;
  ADest.Colors[1, 0] := colBlack;
  ADest.Colors[0, 1] := colBlack;
  lDest.Pixels[0, 2] := ANDROID_CHECKBOX_CORNER_DARK_GRAY;
  lDest.Pixels[2, 0] := ANDROID_CHECKBOX_CORNER_DARK_GRAY;
  lDest.Pixels[1, 1] := ANDROID_CHECKBOX_CORNER_GRAY;
  //
  ADest.Colors[29, 0] := colBlack;
  ADest.Colors[28, 0] := colBlack;
  ADest.Colors[29, 1] := colBlack;
  lDest.Pixels[29, 2] := ANDROID_CHECKBOX_CORNER_DARK_GRAY;
  lDest.Pixels[27, 0] := ANDROID_CHECKBOX_CORNER_DARK_GRAY;
  lDest.Pixels[26, 1] := ANDROID_CHECKBOX_CORNER_GRAY;
  //
  ADest.Colors[0, 29] := colBlack;
  ADest.Colors[1, 29] := colBlack;
  ADest.Colors[0, 28] := colBlack;
  lDest.Pixels[0, 27] := ANDROID_CHECKBOX_CORNER_DARK_GRAY;
  lDest.Pixels[2, 29] := ANDROID_CHECKBOX_CORNER_DARK_GRAY;
  lDest.Pixels[1, 28] := ANDROID_CHECKBOX_CORNER_GRAY;
  //
  ADest.Colors[29, 29] := colBlack;
  ADest.Colors[28, 29] := colBlack;
  ADest.Colors[29, 28] := colBlack;
  lDest.Pixels[29, 27] := ANDROID_CHECKBOX_CORNER_DARK_GRAY;
  lDest.Pixels[27, 29] := ANDROID_CHECKBOX_CORNER_DARK_GRAY;
  lDest.Pixels[28, 28] := ANDROID_CHECKBOX_CORNER_GRAY;

  // Tickmark
  if csfOff in AState then
  begin
    // first 6 descending lines
    for i := 0 to 5 do
      DrawVerticalLineWithFirstLast(ADest, 7+i, 12+i, 18+i,
        TColorToFPColor($828081), TColorToFPColor($AFB0AF), TColorToFPColor($9D9E9D));
    // now 11 ascending lines
    for i := 6 to 17 do
      DrawVerticalLineWithFirstLast(ADest, 7+i, 12+10-i, 18+10-i,
        TColorToFPColor($939193), TColorToFPColor($AFB0AF), TColorToFPColor($9D9E9D));
    // left part adjusts
    lDest.Pixels[7, 12] := $A6A7A6;
    lDest.Pixels[6, 13] := $828482;
    lDest.Pixels[5, 14] := $949193;
    lDest.Pixels[5, 15] := $9D9E9D;
    lDest.Pixels[5, 16] := $A6A7A6;
    lDest.Pixels[6, 14] := $A6A3A5;
    lDest.Pixels[6, 15] := $AFACAF;
    lDest.Pixels[6, 16] := $A6A7A6;
    lDest.Pixels[6, 17] := $9DA29E;
    // right part adjusts
    lDest.Pixels[24,  6] := $9D9A9C;
    lDest.Pixels[24,  7] := $AFBDAF;
    lDest.Pixels[24,  8] := $CCC9CC;
    lDest.Pixels[24,  9] := $BABBBA;
    lDest.Pixels[24, 10] := $B9B6B9;
    lDest.Pixels[25,  6] := $AFB0AF;
    lDest.Pixels[25,  7] := $A6A7A6;
    lDest.Pixels[25,  8] := $B9B6B9;
    lDest.Pixels[25,  9] := $BABBBA;
  end
  else
  begin
    // first 6 descending lines
    for i := 0 to 5 do
      DrawVerticalLineWithFirstLast(ADest, 7+i, 12+i, 18+i,
        TColorToFPColor($007500), TColorToFPColor($00D300), TColorToFPColor($089A08));
    // now 11 ascending lines
    for i := 6 to 17 do
      DrawVerticalLineWithFirstLast(ADest, 7+i, 12+10-i, 18+10-i,
        TColorToFPColor($009200), TColorToFPColor($00D300), TColorToFPColor($089A08));
    // left part adjusts
    lDest.Pixels[7, 12] := $849E84;
    lDest.Pixels[6, 13] := $187518;
    lDest.Pixels[5, 14] := $188A18;
    lDest.Pixels[5, 15] := $109E10;
    lDest.Pixels[5, 16] := $73A273;
    lDest.Pixels[6, 14] := $00A600;
    lDest.Pixels[6, 15] := $00BE00;
    lDest.Pixels[6, 16] := $00B200;
    lDest.Pixels[6, 17] := $4A9E4A;
    // right part adjusts
    lDest.Pixels[24,  6] := $427D42;
    lDest.Pixels[24,  7] := $00A200;
    lDest.Pixels[24,  8] := $00C700;
    lDest.Pixels[24,  9] := $00B200;
    lDest.Pixels[24, 10] := $31A231;
    lDest.Pixels[25,  6] := $739E73;
    lDest.Pixels[25,  7] := $009200;
    lDest.Pixels[25,  8] := $00AA00;
    lDest.Pixels[25,  9] := $4AA64A;
  end;
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

procedure TCDDrawerAndroid.CreateResources;
begin
  bmpCheckbox := TBitmap.Create;
  bmpCheckboxChecked := TBitmap.Create;
end;

procedure TCDDrawerAndroid.LoadResources;
var
  lDPI: Word;
begin
  {$ifdef CD_UseImageResources}
  bmpCheckbox.LoadFromLazarusResource('android_checkbox');
  bmpCheckboxChecked.LoadFromLazarusResource('android_checkbox_checked');
  {$else}
  bmpCheckbox.Width := 30;
  bmpCheckbox.Height := 30;
  bmpCheckboxChecked.Width := 30;
  bmpCheckboxChecked.Height := 30;
  DrawCheckBoxBitmap(bmpCheckbox.Canvas, [csfOff]);
  DrawCheckBoxBitmap(bmpCheckboxChecked.Canvas, [csfOn]);
  {$endif}

  // DPI adjustment
  lDPI := Max(96, Screen.PixelsPerInch);
  ScaleRasterImage(bmpCheckbox, 160, lDPI);
  ScaleRasterImage(bmpCheckboxChecked, 160, lDPI);
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
  TCDCHECKBOX_SQUARE_HALF_HEIGHT: Floor(GetMeasures(TCDCHECKBOX_SQUARE_HEIGHT)/2);
  TCDCHECKBOX_SQUARE_HEIGHT: Result := DPIAdjustment(18);
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

procedure TCDDrawerAndroid.DrawTickmark(ADest: TFPCustomCanvas; ADestPos: TPoint; AState: TCDControlState);
begin

end;

procedure TCDDrawerAndroid.DrawButton(ADest: TFPCustomCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDButtonStateEx);
var
  lDest: TCanvas absolute ADest;
  Str: string;
  lGlyphLeftSpacing: Integer = 0;
  lTextOutPos: TPoint;
  lGlyphCaptionHeight: Integer;
  lColor: TColor;
  lRect: TRect;
begin
  if not (ADest is TCanvas) then Exit; // ToDo
  // Background corners
  DrawTransparentRoundCorners(ADest, Point(0, 0), ASize, AStateEx.FPParentRGBColor);

  // Darker corners
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
  lDest.Pixels[ASize.cx-3, ASize.cy-1] := lColor;

  // Button image
  if csfSunken in AState then
  begin
    // Top lines
    DrawAndroidAlternatedHorzLine(lDest, 3, ASize.cx-3, 0, ANDROID_BUTTON_SUNKEN_FIRST_LINE_A, ANDROID_BUTTON_SUNKEN_FIRST_LINE_B);
    DrawAndroidAlternatedHorzLine(lDest, 2, ASize.cx-2, 1, ANDROID_BUTTON_SUNKEN_SECOND_LINE_A, ANDROID_BUTTON_SUNKEN_SECOND_LINE_B);
    DrawAndroidAlternatedHorzLine(lDest, 1, ASize.cx-1, 2, ANDROID_BUTTON_SUNKEN_THIRD_LINE_A, ANDROID_BUTTON_SUNKEN_THIRD_LINE_B);

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
    DrawAndroidAlternatedHorzLine(lDest, 1, ASize.cx-1, ASize.cy-3, ANDROID_BUTTON_SUNKEN_PREPRELAST_LINE_A, ANDROID_BUTTON_SUNKEN_PREPRELAST_LINE_B);
    DrawAndroidAlternatedHorzLine(lDest, 2, ASize.cx-2, ASize.cy-2, ANDROID_BUTTON_SUNKEN_PRELAST_LINE_A, ANDROID_BUTTON_SUNKEN_PRELAST_LINE_B);
    DrawAndroidAlternatedHorzLine(lDest, 3, ASize.cx-3, ASize.cy-1, ANDROID_BUTTON_SUNKEN_LAST_LINE_A, ANDROID_BUTTON_SUNKEN_LAST_LINE_B);
  end
  else
  begin
    // Top lines
    DrawAndroidAlternatedHorzLine(lDest, 3, ASize.cx-3, 0, ANDROID_BUTTON_FIRST_LINE_A, ANDROID_BUTTON_FIRST_LINE_B);
    DrawAndroidAlternatedHorzLine(lDest, 2, ASize.cx-2, 1, ANDROID_BUTTON_SECOND_LINE_A, ANDROID_BUTTON_SECOND_LINE_B);
    DrawAndroidAlternatedHorzLine(lDest, 1, ASize.cx-1, 2, ANDROID_BUTTON_THIRD_LINE_A, ANDROID_BUTTON_THIRD_LINE_B);

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
    DrawAndroidAlternatedHorzLine(lDest, 1, ASize.cx-1, ASize.cy-3, ANDROID_BUTTON_PREPRELAST_LINE_A, ANDROID_BUTTON_PREPRELAST_LINE_B);
    DrawAndroidAlternatedHorzLine(lDest, 2, ASize.cx-2, ASize.cy-2, ANDROID_BUTTON_PRELAST_LINE_A, ANDROID_BUTTON_PRELAST_LINE_B);
    DrawAndroidAlternatedHorzLine(lDest, 3, ASize.cx-3, ASize.cy-1, ANDROID_BUTTON_LAST_LINE_A, ANDROID_BUTTON_LAST_LINE_B);
  end;

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
begin
  if csfOn in AState then ADest.Draw(0, 0, bmpCheckboxChecked)
  else ADest.Draw(0, 0, bmpCheckbox);

  // Transparent corners
  DrawTransparentRoundCorners(ADest, ADestPos,
    Size(GetMeasures(TCDCHECKBOX_SQUARE_HEIGHT), GetMeasures(TCDCHECKBOX_SQUARE_HEIGHT)),
    AStateEx.FPParentRGBColor);
end;

initialization
  {$ifdef CD_UseImageResources}
  {$include customdrawnimages/android.lrs}
  {$endif}
  RegisterDrawer(TCDDrawerAndroid.Create, dsAndroid);
end.

