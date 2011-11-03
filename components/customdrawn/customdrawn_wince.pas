unit customdrawn_wince;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils, Types,
  // LCL -> Use only TForm, TWinControl, TCanvas and TLazIntfImage
  Graphics, Controls, LCLType,
  // Others only for types
  StdCtrls,
  //
  customdrawndrawers, customdrawn_common;

type
  TCDDrawerWinCE = class(TCDDrawerCommon)
  public
    // ===================================
    // Standard Tab
    // ===================================
    // TCDButton
    procedure DrawButton(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
  end;

implementation

procedure TCDDrawerWinCE.DrawButton(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDControlStateEx);
var
  TmpB: TBitmap;
  Str: string;
begin
  // Button shape -> This crashes in Gtk2
  TmpB := TBitmap.Create;
  TmpB.Width := ASize.cx;
  TmpB.Height := ASize.cy;
  TmpB.Canvas.Brush.Color := AStateEx.RGBColor;
  TmpB.Canvas.Brush.Style := bsSolid;
  TmpB.Canvas.RoundRect(0, 0, TmpB.Width, TmpB.Height, 8, 8);
  //  CDButton.SetShape(TmpB);

  // Button image
  if csfSunken in AState then
  begin
    TmpB.Canvas.Brush.Style := bsSolid;
    TmpB.Canvas.Brush.Color := RGBToColor(230, 230, 230);
    TmpB.Canvas.Pen.Color := clBlack;
    TmpB.Canvas.Pen.Style := psSolid;
    TmpB.Canvas.Rectangle(0, 0, TmpB.Canvas.Width, TmpB.Canvas.Height);
  end
  else if csfHasFocus in AState then
  begin
    with TmpB.Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := RGBToColor($FD, $FD, $FD);
        Pen.Color := clBlack;
        Pen.Style := psSolid;
        Rectangle(0, 0, Width, Height);
        Rectangle(1, 1, Width - 1, Height - 1); // The border is thicken when focused
      end;
  end
  else
  begin
    with TmpB.Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := AStateEx.RGBColor;
        Pen.Color := clBlack;
        Pen.Style := psSolid;
        Rectangle(0, 0, Width, Height);
      end;
  end;

  ADest.Draw(0, 0, TmpB);

  TmpB.Free;

  // Button text
  ADest.Font.Assign(AStateEx.Font);
  ADest.Brush.Style := bsClear;
  ADest.Pen.Style := psSolid;
  Str := AStateEx.Caption;
  ADest.TextOut((ASize.cx - ADest.TextWidth(Str)) div 2,
    (ASize.cy - ADest.TextHeight(Str)) div 2, Str);
end;

initialization
  RegisterDrawer(TCDDrawerWinCE.Create, dsWinCE);
end.

