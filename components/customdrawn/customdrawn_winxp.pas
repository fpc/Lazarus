unit customdrawn_winxp;

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
  TCDButtonDrawerXPTB = class(TCDButtonDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDButton: TCDButton); override;
    procedure DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
      FState: TCDButtonState); override;
  end;

implementation

procedure TCDButtonDrawerXPTB.DrawToIntfImage(ADest: TFPImageCanvas;
  CDButton: TCDButton);
begin

end;

procedure TCDButtonDrawerXPTB.DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
  FState: TCDButtonState);
var
  Str: string;
begin
  case FState of
    bbsDown:
    begin
      DrawCDButtonDown(ADest, CDButton.GetRGBBackgroundColor);
    end;
    bbsFocused:
    begin
      DrawXPTaskbarButton(ADest, GetAColor(CDButton.Color, 98));
    end;
    else
      DrawXPTaskbarButton(ADest, CDButton.Color);
  end;

  // Button text
  ADest.Font.Assign(CDButton.Font);
  ADest.Brush.Style := bsClear;
  ADest.Pen.Style := psSolid;
  Str := CDButton.Caption;
  ADest.TextOut((CDButton.Width - ADest.TextWidth(Str)) div 2,
    (CDButton.Height - ADest.TextHeight(Str)) div 2, Str);
end;

initialization
  RegisterButtonDrawer(TCDButtonDrawerXPTB.Create, dsWinXP);
end.

