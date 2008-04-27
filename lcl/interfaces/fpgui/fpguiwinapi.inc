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

function TFpGuiWidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
var
  ADC: TFpGuiDeviceContext absolute DC;
begin
  ADC.fpgCanvas.DrawRectangle(X1, Y1, X2 - X1, Y2 - Y1);
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

