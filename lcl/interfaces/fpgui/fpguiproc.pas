{
 *****************************************************************************
 *                                FpGuiProc.pp                               *
 *                              ---------------                              *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit fpguiproc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base, Graphics,LCLType;

function TColorToTfpgColor(AColor: TColor): TfpgColor;
function TTfpgColorToTColor(AColor: TfpgColor): TColor;
procedure TfpgRectToRect(const AfpguiRect: TfpgRect; var ARect: TRect); inline;
procedure TRectTofpgRect(const ARect: TRect; var AfpguiRect: TfpgRect); inline;
function TFontToTfpgFontDesc(const AFont: TFont): string;
procedure AdjustRectToOrg(var ARect: TRect; const AOrgPoint: TPoint); overload;
procedure AdjustRectToOrg(var AfpgRect: TfpgRect; const AOrgPoint: TPoint); overload;
function GetSysColorRGB(const Index: Integer): DWORD;
procedure AdjustToZeroLeftTop(var ARect: TRect);

implementation

{
  Converts from TColor to TfpgColor

  TfpgColor   = type longword;    // Always in RRGGBB (Alpha, Red, Green, Blue) format!!
}
function TColorToTfpgColor(AColor: TColor): TfpgColor;
var
  RGBColor: TColor;
  RGBTriple: fpg_base.TRGBTriple;
begin
  RGBColor := ColorToRGB(AColor);
  RGBTriple.Alpha := 0;
  RGBTriple.Red := Graphics.Red(RGBColor);
  RGBTriple.Green := Graphics.Green(RGBColor);
  RGBTriple.Blue := Graphics.Blue(RGBColor);

  Result := RGBTripleTofpgColor(RGBTriple);
end;

function TTfpgColorToTColor(AColor: TfpgColor): TColor;
var
  RGBTriple: fpg_base.TRGBTriple;
begin
  RGBTriple:=fpgColorToRGBTriple(AColor);
  Result:=RGBToColor(RGBTriple.Red,RGBTriple.Green,RGBTriple.Blue);
end;

procedure TfpgRectToRect(const AfpguiRect: TfpgRect; var ARect: TRect);
begin
  with AfpguiRect do begin
    ARect:=Rect(Left,Top,Width,Height);
  end;
end;

procedure TRectTofpgRect(const ARect: TRect; var AfpguiRect: TfpgRect); inline;
begin
  with ARect do begin
    AfpguiRect.Left:=Left;
    AfpguiRect.Top:=Top;
    AfpguiRect.Width:=Right-Left;
    AfpguiRect.Height:=Bottom-Top;
  end;
end;

function TFontToTfpgFontDesc(const AFont: TFont): string;
var
  fontdesc: string;
begin
  fontdesc:=AFont.Name;
  if AFont.Size>0 then
    { TODO -oJose Mejuto : This conversion seems to be performed somewhere in the WinAPI interface, but now keep it here while the right position is not found }
    fontdesc:=fontdesc+'-'+inttostr(round((AFont.Size*AFont.PixelsPerInch) / 96));
  if fsBold in AFont.Style then
    fontdesc:=fontdesc+':bold';
  Result:=fontdesc;
end;

procedure AdjustRectToOrg(var ARect: TRect; const AOrgPoint: TPoint);
begin
  With ARect,AOrgPoint do begin
    Left:=Left-x;
    Top:=Top-y;
  end;
end;

procedure AdjustRectToOrg(var AfpgRect: TfpgRect; const AOrgPoint: TPoint);
  overload;
begin
  With AfpgRect,AOrgPoint do begin
    Left:=Left-x;
    Top:=Top-y;
  end;
end;

function GetSysColorRGB(const Index: Integer): DWORD;
begin
  case Index of
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

procedure AdjustToZeroLeftTop(var ARect: TRect);
begin
  ARect.Right:=ARect.Right-ARect.Left;
  ARect.Bottom:=ARect.Bottom-ARect.Top;
  ARect.Left:=0;
  ARect.Top:=0;
end;

end.

