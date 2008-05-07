{%MainUnit fpguiint.pp}

{******************************************************************************
  All FPGUI Winapi implementations.
  This are the implementations of the overrides of the FPGUI Interface for the
  methods defined in the
  lcl/include/winapi.inc


  !! Keep alphabetical !!


 ******************************************************************************
 Implementation
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

//##apiwiz##sps##   // Do not remove, no wizard declaration before this line

{------------------------------------------------------------------------------
  Function: BeginPaint
  Params:
  Returns:

  This function is Called:
  - Once on every OnPaint event
 ------------------------------------------------------------------------------}
function TFpGuiWidgetSet.BeginPaint(Handle: hWnd; Var PS : TPaintStruct): hdc;
var
  PrivateWidget: TFPGUIPrivateWidget absolute Handle;
  DC: TFpGuiDeviceContext;
begin
  {$ifdef VerboseFPGUIWinAPI}
    WriteLn('Trace:> [WinAPI BeginPaint] Handle=', dbghex(Handle));
  {$endif}

  if PrivateWidget <> nil then
    DC := TFpGuiDeviceContext.Create(PrivateWidget.Widget.Canvas)
  else
    DC := TFpGuiDeviceContext.Create(nil);

  PS.hdc := HDC(DC);

  Result := PS.hdc;

  {$ifdef VerboseFPGUIWinAPI}
    WriteLn('Trace:< [WinAPI BeginPaint] Result=', dbghex(Result));
  {$endif}
end;

function TFpGuiWidgetSet.EndPaint(Handle: hwnd; var PS: TPaintStruct): Integer;
var
  DC: TFpGuiDeviceContext;
begin
  DC := TFpGuiDeviceContext(PS.hdc);
  
  DC.Free;
end;

function TFpGuiWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  ADC: TFpGuiDeviceContext absolute DC;
  AStr: string;
begin
  AStr := string(Str);

  ADC.fpgCanvas.DrawText(X, Y, AStr);
end;

function TFpGuiWidgetSet.GetSysColor(nIndex: Integer): DWORD;
begin
  if (nIndex < 0) or (nIndex > MAX_SYS_COLORS) then
  begin
    {$ifdef VerboseFPGUIWinAPI}
      WriteLn('Trace:Unknown lcl system color: [TFpGuiWidgetSet.GetSysColor]');
    {$endif}
    exit;
  end;

  case nIndex of
{    COLOR_SCROLLBAR               : Result:=clGray;
    COLOR_BACKGROUND              : Result:=;
    COLOR_ACTIVECAPTION           : Result:=;
    COLOR_INACTIVECAPTION         : Result:=;
    COLOR_MENU                    : Result:=;}
    COLOR_WINDOW                  : Result:=clWhite;
{    COLOR_WINDOWFRAME             : Result:=;
    COLOR_MENUTEXT                : Result:=;
    COLOR_WINDOWTEXT              : Result:=;
    COLOR_CAPTIONTEXT             : Result:=GetColor(QPaletteActive,   QPaletteText);
    COLOR_ACTIVEBORDER            : Result:=GetColor(QPaletteActive,   QPaletteWindow);
    COLOR_INACTIVEBORDER          : Result:=GetColor(QPaletteInactive, QPaletteWindow);
    COLOR_APPWORKSPACE            : Result:=GetColor(QPaletteActive,   QPaletteWindow);
    COLOR_HIGHLIGHT               : Result:=GetColor(QPaletteActive,   QPaletteHighlight);
    COLOR_HIGHLIGHTTEXT           : Result:=GetColor(QPaletteActive,   QPaletteHighlightedText);}
    COLOR_BTNFACE                 : Result:=clLtGray;
{    COLOR_BTNSHADOW               : Result:=GetColor(QPaletteActive,   QPaletteDark);
    COLOR_GRAYTEXT                : Result:=GetColor(QPaletteActive,   QPaletteText);
    COLOR_BTNTEXT                 : Result:=GetColor(QPaletteActive,   QPaletteButtonText);
    COLOR_INACTIVECAPTIONTEXT     : Result:=GetColor(QPaletteInactive, QPaletteText);
    COLOR_BTNHIGHLIGHT            : Result:=GetColor(QPaletteActive,   QPaletteLight);
    COLOR_3DDKSHADOW              : Result:=GetColor(QPaletteActive,   QPaletteShadow);
    COLOR_3DLIGHT                 : Result:=GetColor(QPaletteActive,   QPaletteMidlight);
    COLOR_INFOTEXT                : Result:=GetClInfo(False);
    COLOR_INFOBK                  : Result:=GetClInfo(True);
    // PBD: 25 is unassigned in all the docs I can find
    //      if someone finds what this is supposed to be then fill it in
    //      note defaults below, and cl[ColorConst] in graphics
    COLOR_HOTLIGHT                : Result:=GetColor(QPaletteActive,   QPaletteLight);
    COLOR_GRADIENTACTIVECAPTION   : Result:=GetColor(QPaletteActive,   QPaletteHighlight);
    COLOR_GRADIENTINACTIVECAPTION : Result:=GetColor(QPaletteInactive, QPaletteBase);
    COLOR_FORM                    : Result:=GetColor(QPaletteActive,   QPaletteWindow);

    COLOR_clForeground..COLOR_clHighlightedText
                                  : Result:=GetColor(QPaletteActive,   nIndex - COLOR_clForeground);
    COLOR_clNormalForeground..COLOR_clNormalHighlightedText
                                  : Result:=GetColor(QPaletteInactive, nIndex - COLOR_clNormalForeground);
    COLOR_clDisabledForeground..COLOR_clDisabledHighlightedText
                                  : Result:=GetColor(QPaletteDisabled, nIndex - COLOR_clDisabledForeground);
    COLOR_clActiveForeground..COLOR_clActiveHighlightedText
                                  : Result:=GetColor(QPaletteActive,   nIndex - COLOR_clActiveForeground);}
  else
    Result:=0;
  end;
end;

function TFpGuiWidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
var
  ADC: TFpGuiDeviceContext absolute DC;
begin
  ADC.fpgCanvas.DrawRectangle(X1, Y1, X2 - X1, Y2 - Y1);
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

