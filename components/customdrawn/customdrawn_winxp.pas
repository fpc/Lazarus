unit customdrawn_winxp;

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

  { TCDDrawerWinXP }

  TCDDrawerWinXP = class(TCDDrawerCommon)
  public
    //procedure LoadFallbackPaletteColors; override;
    // General
    function GetMeasures(AMeasureID: Integer): Integer; override;
    //function GetMeasuresEx(ADest: TCanvas; AMeasureID: Integer;
      //AState: TCDControlState; AStateEx: TCDControlStateEx): Integer; override;
    //
    function GetDrawStyle: TCDDrawStyle; override;
    procedure LoadFallbackPaletteColors; override;
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
    // TCDEdit
    procedure DrawEditFrame(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
    // TCDCheckBox
    procedure DrawCheckBoxSquare(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
  end;

implementation

const

  // Button

  WINXP_BUTTON_BODY_TOP = $00FFFFFF;
  WINXP_BUTTON_BODY_BOTTOM = $00EAF0F0;

  WINXP_BUTTON_BODY_LINES_PREPRELAST = $00E6EBEC;
  WINXP_BUTTON_BODY_LINES_PRELAST = $00D6DFE2;
  WINXP_BUTTON_BODY_LINES_LAST = $00C5D0D6;

  WINXP_BUTTON_SUNKEN_BODY_BOTTOM = $00E3E9EA;

  WINXP_BUTTON_FRAME_DARK_BLUE = $00743C00;
  WINXP_BUTTON_FRAME_MED_DARK_BLUE = $00A27055;
  WINXP_BUTTON_FRAME_MEDIUM_BLUE = $00A8957A;
  WINXP_BUTTON_FRAME_LIGHT_BLUE = $00DDCFC0;

  WINXP_BUTTON_DISABLED_BODY = $00EAF4F5;

  WINXP_BUTTON_FRAME_DARK_DISABLED = $00BAC7C9;
  WINXP_BUTTON_FRAME_MEDIUM_DISABLED = $00CAD6D8;
  WINXP_BUTTON_FRAME_LIGHT_DISABLED = $00E2EDEE;

  WINXP_BUTTON_FOCUS_TOP = $00FFE7CE;
  WINXP_BUTTON_FOCUS_TOP_GRAD = $00F6D4BC;
  WINXP_BUTTON_FOCUS_BOTTOM_GRAD = $00E4AD89;
  WINXP_BUTTON_FOCUS_BOTTOM = $00EF8269;

  WINXP_BUTTON_MOUSEOVER_TOP = $00CFF0FF;
  WINXP_BUTTON_MOUSEOVER_TOP_GRAD = $0089D8FD;
  WINXP_BUTTON_MOUSEOVER_BOTTOM_GRAD = $0030B3F8;
  WINXP_BUTTON_MOUSEOVER_BOTTOM = $000097E5;

  // CheckBox

  WINXP_CHECKBOX_GRADIENT_1 = $00D6DED6;
  WINXP_CHECKBOX_GRADIENT_2 = $00CED6D6;
  WINXP_CHECKBOX_GRADIENT_3 = $00D6DED6;
  WINXP_CHECKBOX_GRADIENT_4 = $00D6D6D6;
  WINXP_CHECKBOX_GRADIENT_5 = $00DEE7DE;
  WINXP_CHECKBOX_GRADIENT_6 = $00D6DEDE;
  WINXP_CHECKBOX_GRADIENT_7 = $00E7E7DE;
  WINXP_CHECKBOX_GRADIENT_8 = $00DEE7E7;
  WINXP_CHECKBOX_GRADIENT_9 = $00EFEFE7;
  WINXP_CHECKBOX_GRADIENT_10 = $00E7E7E7;

  WINXP_CHECKBOX_GRADIENT_DIAGONAL = $00D6DED6; // The biggest diagonal

  WINXP_CHECKBOX_GRADIENT_11 = $00EFEFF7;
  WINXP_CHECKBOX_GRADIENT_12 = $00F7EFEF;
  WINXP_CHECKBOX_GRADIENT_13 = $00EFEFEF;
  WINXP_CHECKBOX_GRADIENT_14 = $00F7FFF7;
  WINXP_CHECKBOX_GRADIENT_15 = $00F7F7FF;
  WINXP_CHECKBOX_GRADIENT_16 = $00FFF7F7;
  WINXP_CHECKBOX_GRADIENT_17 = $00F7F7F7;
  WINXP_CHECKBOX_GRADIENT_18 = $00F7FFF7;
  WINXP_CHECKBOX_GRADIENT_19 = $00F7F7FF;
  WINXP_CHECKBOX_GRADIENT_20 = $00FFF7F7;

  WINXP_TICKMARK = $0021A521;

  WINXP_FRAME_BLUE = $00B99D7F;
  WINXP_FORM       = $00D8E9EC;

{ TCDDrawerWinXP }

function TCDDrawerWinXP.GetMeasures(AMeasureID: Integer): Integer;
begin
  case AMeasureID of
  TCDCHECKBOX_SQUARE_HALF_HEIGHT: Result := 6;
  TCDCHECKBOX_SQUARE_HEIGHT: Result := 13;
  else
    Result:=inherited GetMeasures(AMeasureID);
  end;
end;

function TCDDrawerWinXP.GetDrawStyle: TCDDrawStyle;
begin
  Result := dsWinXP;
end;

procedure TCDDrawerWinXP.LoadFallbackPaletteColors;
begin
  Palette.BtnFace := WINXP_FORM;
  Palette.Form := WINXP_FORM;
end;

procedure TCDDrawerWinXP.DrawTickmark(ADest: TCanvas; ADestPos: TPoint);
var
  i: Integer;
begin
  ADest.Pen.Color := WINXP_TICKMARK;
  ADest.Pen.Style := psSolid;
  // 3 lines going down and to the right
  for i := 0 to 2 do
    ADest.Line(ADestPos.X+i, ADestPos.Y+i, ADestPos.X+i, ADestPos.Y+3+i);
  // Now 4 lines going up and to the right
  for i := 3 to 6 do
    ADest.Line(ADestPos.X+i, ADestPos.Y+4-i, ADestPos.X+i, ADestPos.Y+3+4-i);
end;

procedure TCDDrawerWinXP.DrawButton(ADest: TCanvas;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDButtonStateEx);
var
  Str: string;
  lColor: TColor;
  lRect: TRect;
  lFrameDark, lFrameMedDark, lFrameMedium, lFrameLight: TColor;
  lSelTop, lSelTopGrad, lSelBottomGrad, lSelBottom: TColor;
begin
  // Background corners
  lColor := AStateEx.ParentRGBColor;
  ADest.Pixels[0, 0] := lColor;
  ADest.Pixels[ASize.cx-1, 0] := lColor;
  ADest.Pixels[0, ASize.cy-1] := lColor;
  ADest.Pixels[ASize.cx-1, ASize.cy-1] := lColor;

  // Main body
  if csfSunken in AState then
  begin
    ADest.Brush.Style := bsSolid;
    ADest.Brush.Color := WINXP_BUTTON_SUNKEN_BODY_BOTTOM;
    ADest.Pen.Color := WINXP_BUTTON_SUNKEN_BODY_BOTTOM;
    ADest.Rectangle(1, 1, ASize.cx-1, ASize.cy-1);
  end
  else if csfEnabled in AState then
  begin
    // First the gradient
    lRect := Bounds(1, 1, ASize.cx-4, ASize.cy-5);
    ADest.GradientFill(lRect, WINXP_BUTTON_BODY_TOP, WINXP_BUTTON_BODY_BOTTOM, gdVertical);
    // Now the extra lines which make the bottom-right
    ADest.Pen.Color := WINXP_BUTTON_BODY_LINES_PREPRELAST;
    ADest.Line(1, ASize.cy-4, ASize.cx-3, ASize.cy-4);
    ADest.Line(ASize.cx-3, ASize.cy-4, ASize.cx-3, 1);
    ADest.Pen.Color := WINXP_BUTTON_BODY_LINES_PRELAST;
    ADest.Line(2, ASize.cy-3, ASize.cx-2, ASize.cy-3);
    ADest.Line(ASize.cx-2, ASize.cy-3, ASize.cx-2, 2);
    ADest.Pen.Color := WINXP_BUTTON_BODY_LINES_LAST;
    ADest.Line(3, ASize.cy-1, ASize.cx-3, ASize.cy-1);
  end
  else // disabled
  begin
    ADest.Brush.Style := bsSolid;
    ADest.Brush.Color := WINXP_BUTTON_DISABLED_BODY;
    ADest.Pen.Color := WINXP_BUTTON_DISABLED_BODY;
    ADest.Rectangle(1, 1, ASize.cx-1, ASize.cy-1);
  end;

  // Now the frame
  if csfEnabled in AState then
  begin
    lFrameDark := WINXP_BUTTON_FRAME_DARK_BLUE;
    lFrameMedDark := WINXP_BUTTON_FRAME_MED_DARK_BLUE;
    lFrameMedium := WINXP_BUTTON_FRAME_MEDIUM_BLUE;
    lFrameLight := WINXP_BUTTON_FRAME_LIGHT_BLUE;
  end
  else
  begin
    lFrameDark := WINXP_BUTTON_FRAME_DARK_DISABLED;
    lFrameMedDark := WINXP_BUTTON_FRAME_MEDIUM_DISABLED;
    lFrameMedium := WINXP_BUTTON_FRAME_MEDIUM_DISABLED;
    lFrameLight := WINXP_BUTTON_FRAME_LIGHT_DISABLED;
  end;

  ADest.Pixels[1, 0] := lFrameMedium;
  ADest.Pixels[0, 1] := lFrameMedium;
  ADest.Pixels[1, 1] := lFrameMedDark;
  ADest.Pixels[2, 1] := lFrameLight;
  ADest.Pixels[1, 2] := lFrameLight;

  ADest.Pixels[ASize.cx-2, 0] := lFrameMedium;
  ADest.Pixels[ASize.cx-1, 1] := lFrameMedium;
  ADest.Pixels[ASize.cx-2, 1] := lFrameMedDark;
  ADest.Pixels[ASize.cx-3, 1] := lFrameLight;
  ADest.Pixels[ASize.cx-2, 2] := lFrameLight;

  ADest.Pixels[1, ASize.cy-1] := lFrameMedium;
  ADest.Pixels[0, ASize.cy-2] := lFrameMedium;
  ADest.Pixels[1, ASize.cy-2] := lFrameMedDark;
  ADest.Pixels[2, ASize.cy-2] := lFrameLight;
  ADest.Pixels[1, ASize.cy-3] := lFrameLight;

  ADest.Pixels[ASize.cx-2, ASize.cy-1] := lFrameMedium;
  ADest.Pixels[ASize.cx-1, ASize.cy-2] := lFrameMedium;
  ADest.Pixels[ASize.cx-2, ASize.cy-2] := lFrameMedDark;
  ADest.Pixels[ASize.cx-3, ASize.cy-2] := lFrameLight;
  ADest.Pixels[ASize.cx-2, ASize.cy-3] := lFrameLight;

  ADest.Pen.Color := lFrameDark;
  ADest.Line(2, 0, ASize.cx-2, 0);
  ADest.Line(2, ASize.cy-1, ASize.cx-2, ASize.cy-1);
  ADest.Line(0, 2, 0, ASize.cy-2);
  ADest.Line(ASize.cx-1, 2, ASize.cx-1, ASize.cy-2);

  // Now focus / mouseover indication, note that both disappear when sunked in WinXP
  if ((csfHasFocus in AState) or (csfMouseOver in AState)) and not (csfSunken in AState) then
  begin
    if (csfHasFocus in AState) then
    begin
      lSelTop := WINXP_BUTTON_FOCUS_TOP;
      lSelTopGrad := WINXP_BUTTON_FOCUS_TOP_GRAD;
      lSelBottomGrad := WINXP_BUTTON_FOCUS_BOTTOM_GRAD;
      lSelBottom := WINXP_BUTTON_FOCUS_BOTTOM;
    end
    else
    begin
      lSelTop := WINXP_BUTTON_MOUSEOVER_TOP;
      lSelTopGrad := WINXP_BUTTON_MOUSEOVER_TOP_GRAD;
      lSelBottomGrad := WINXP_BUTTON_MOUSEOVER_BOTTOM_GRAD;
      lSelBottom := WINXP_BUTTON_MOUSEOVER_BOTTOM;
    end;

    // Top
    ADest.Pen.Color := lSelTop;
    ADest.Line(2, 1, ASize.cx-2, 1);
    ADest.Pen.Color := lSelTopGrad;
    ADest.Line(1, 2, ASize.cx-1, 2);
    // Gradient
    lRect := Bounds(1, 2, 2, ASize.cy-4);
    ADest.GradientFill(lRect, lSelTopGrad, lSelBottomGrad, gdVertical);
    lRect := Bounds(ASize.cx-3, 2, 2, ASize.cy-4);
    ADest.GradientFill(lRect, lSelTopGrad, lSelBottomGrad, gdVertical);
    // Bottom
    ADest.Pen.Color := lSelBottomGrad;
    ADest.Line(1, ASize.cy-3, ASize.cx-1, ASize.cy-3);
    ADest.Pen.Color := lSelBottom;
    ADest.Line(2, ASize.cy-2, ASize.cx-2, ASize.cy-2);
  end;

  // Button text, note that it doesn't move downwards when clicked in WinXP
  ADest.Font.Assign(AStateEx.Font);
  ADest.Brush.Style := bsClear;
  ADest.Pen.Style := psSolid;
  Str := AStateEx.Caption;
  ADest.TextOut((ASize.cx - ADest.TextWidth(Str)) div 2,
    (ASize.cy - ADest.TextHeight(Str)) div 2, Str);
end;

procedure TCDDrawerWinXP.DrawEditFrame(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDEditStateEx);
begin
  // The frame
  ADest.Brush.Style := bsClear;
  ADest.Pen.Color := WINXP_FRAME_BLUE;
  ADest.Pen.Style := psSolid;
  ADest.Rectangle(0, 0, ASize.cx, ASize.cy);
end;

procedure TCDDrawerWinXP.DrawCheckBoxSquare(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDControlStateEx);
begin
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Pen.Color := clBlack;
  ADest.Rectangle(0,0,13,13);
  ADest.Pixels[1,1] := WINXP_CHECKBOX_GRADIENT_1;
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_2;
  ADest.Line(1,2,3,0);
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_3;
  ADest.Line(1,3,4,0);
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_4;
  ADest.Line(1,4,5,0);
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_5;
  ADest.Line(1,5,6,0);
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_6;
  ADest.Line(1,6,7,0);
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_7;
  ADest.Line(1,7,8,0);
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_8;
  ADest.Line(1,8,9,0);
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_9;
  ADest.Line(1,9,10,0);
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_10;
  ADest.Line(1,10,11,0);
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_DIAGONAL;
  ADest.Line(1,11,12,0);
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_11;
  ADest.Line(2,11,12,1);
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_12;
  ADest.Line(3,11,12,2);
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_13;
  ADest.Line(4,11,12,3);
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_14;
  ADest.Line(5,11,12,4);
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_15;
  ADest.Line(6,11,12,5);
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_16;
  ADest.Line(7,11,12,6);
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_17;
  ADest.Line(8,11,12,7);
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_18;
  ADest.Line(9,11,12,8);
  ADest.Pen.Color := WINXP_CHECKBOX_GRADIENT_19;
  ADest.Line(10,11,12,9);
  ADest.Pixels[11,11] := WINXP_CHECKBOX_GRADIENT_20;
end;

initialization
  RegisterDrawer(TCDDrawerWinXP.Create, dsWinXP);
end.

