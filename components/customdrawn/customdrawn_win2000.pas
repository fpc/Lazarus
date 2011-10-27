unit customdrawn_win2000;

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
  TCDButtonDrawerWin2k = class(TCDButtonDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDButton: TCDButton); override;
    procedure DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
      FState: TCDButtonState); override;
  end;

implementation

procedure TCDButtonDrawerWin2k.DrawToIntfImage(ADest: TFPImageCanvas;
  CDButton: TCDButton);
begin

end;

procedure TCDButtonDrawerWin2k.DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
  FState: TCDButtonState);
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

  with TmpB.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := CDButton.Color;
    Pen.Color := clWhite;
    Pen.Style := psSolid;
    Rectangle(0, 0, Width - 1, Height - 1);
    Pen.Color := clWhite;
    Line(0, 0, Width - 1, 0);
    Line(0, 0, 0, Height - 1);
    Pen.Color := clGray;
    Line(0, Height - 1, Width - 1, Height - 1);
    Line(Width - 1, Height - 1, Width - 1, -1);
    Pen.Color := $0099A8AC;
    Line(1, Height - 2, Width - 2, Height - 2);
    Line(Width - 2, Height - 2, Width - 2, 0);
    Pen.Color := $00E2EFF1;
    Line(1, 1, Width - 2, 1);
    Line(1, 1, 1, Height - 2);
  end;

  // Button image
  if FState.IsDown then
  begin
      with TmpB.Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := CDButton.Color;
        Pen.Color := clWhite;
        Pen.Style := psSolid;
        Rectangle(0, 0, Width - 1, Height - 1);
        Pen.Color := clGray;
        Line(0, 0, Width - 1, 0);
        Line(0, 0, 0, Height - 1);
        Pen.Color := clWhite;
        Line(0, Height - 1, Width - 1, Height - 1);
        Line(Width - 1, Height - 1, Width - 1, -1);
        Pen.Color := $00E2EFF1;
        Line(1, Height - 2, Width - 2, Height - 2);
        Line(Width - 2, Height - 2, Width - 2, 0);
        Pen.Color := $0099A8AC;
        Line(1, 1, Width - 2, 1);
        Line(1, 1, 1, Height - 2);
      end;
    end
  else if CDButton.Focused then
  begin
      with TmpB.Canvas do
        DrawFocusRect(Rect(3, 3, Width - 4, Height - 4))
  end;

  ADest.Draw(0, 0, TmpB);

  TmpB.Free;

  // Button text
  ADest.Font.Assign(CDButton.Font);
  ADest.Brush.Style := bsClear;
  ADest.Pen.Style := psSolid;
  Str := CDButton.Caption;
  if FState.IsDown then
    ADest.TextOut((CDButton.Width - ADest.TextWidth(Str)) div 2 + 1,
      (CDButton.Height - ADest.TextHeight(Str)) div 2 + 1, Str)
  else
    ADest.TextOut((CDButton.Width - ADest.TextWidth(Str)) div 2,
      (CDButton.Height - ADest.TextHeight(Str)) div 2, Str);
end;

initialization
  RegisterButtonDrawer(TCDButtonDrawerWin2k.Create, dsWin2000);
end.

