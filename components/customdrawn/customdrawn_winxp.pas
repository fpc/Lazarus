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

{type
  TCDButtonDrawerXPTB = class(TCDButtonDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDButton: TCDButton); override;
    procedure DrawToCanvas(ADest: TCanvas; CDButton: TCDButton); override;
  end;}

implementation

{procedure TCDButtonDrawerXPTB.DrawToIntfImage(ADest: TFPImageCanvas;
  CDButton: TCDButton);
begin

end;

procedure TCDButtonDrawerXPTB.DrawToCanvas(ADest: TCanvas; CDButton: TCDButton);
var
  Str: string;
begin
  if CDButton.IsDown then
    DrawCDButtonDown(ADest, CDButton.GetRGBBackgroundColor)
  else if CDButton.Focused then
    DrawXPTaskbarButton(ADest, GetAColor(CDButton.Color, 98))
  else
    DrawXPTaskbarButton(ADest, CDButton.Color);

  // Button text
  ADest.Font.Assign(CDButton.Font);
  ADest.Brush.Style := bsClear;
  ADest.Pen.Style := psSolid;
  Str := CDButton.Caption;
  ADest.TextOut((CDButton.Width - ADest.TextWidth(Str)) div 2,
    (CDButton.Height - ADest.TextHeight(Str)) div 2, Str);
end;

initialization
  RegisterButtonDrawer(TCDButtonDrawerXPTB.Create, dsWinXP);}
end.

