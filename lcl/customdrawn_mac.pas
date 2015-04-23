unit CustomDrawn_Mac;

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

  { TCDDrawerMac }

  TCDDrawerMac = class(TCDDrawerCommon)
  public
    // ===================================
    // Common Controls Tab
    // ===================================
    // TCDToolBar
    procedure DrawToolBarItem(ADest: TCanvas; ASize: TSize;
      ACurItem: TCDToolBarItem; AX, AY: Integer;
      AState: TCDControlState; AStateEx: TCDToolBarStateEx); override;
  end;

implementation

{ TCDDrawerMac }

procedure TCDDrawerMac.DrawToolBarItem(ADest: TCanvas; ASize: TSize;
  ACurItem: TCDToolBarItem; AX, AY: Integer; AState: TCDControlState;
  AStateEx: TCDToolBarStateEx);
var
  lX, lY1, lY2: Integer;

  procedure DrawToolBarItemBorder();
  begin
    ADest.Pen.Style := psSolid;
    ADest.Pen.Color := $AFAFAF;
    ADest.Brush.Style := bsClear;
    ADest.Rectangle(Bounds(AX, AY, ASize.cx, ASize.cy));
  end;

begin
  // tikDivider is centralized, tikSeparator is left-aligned
  if ACurItem.Kind in [tikSeparator, tikDivider] then
  begin
    lX := AX;
    if ACurItem.Kind = tikDivider then
      lX := AX + ASize.CX div 2 - 1;

    lY1 := AY;
    lY2 := AY+ASize.CY;

    ADest.Pen.Style := psSolid;
    ADest.Pen.Color := $DCDEE1;
    ADest.Line(lX+1, lY1, lX+1, lY2);
    ADest.Line(lX+3, lY1, lX+3, lY2);
    ADest.Pen.Style := psSolid;
    ADest.Pen.Color := $93979E;
    ADest.Line(lX+2, lY1, lX+2, lY2);
  end
  else
  begin
    if csfSunken in AState then
    begin
      ADest.GradientFill(Bounds(AX, AY, ASize.CX, ASize.CY),
        $C4C4C4, $DBDBDB, gdVertical);
      DrawToolBarItemBorder();
    end
    else if csfMouseOver in AState then
    begin
      ADest.GradientFill(Bounds(AX, AY, ASize.CX, ASize.CY),
        $E3E3E3, $F7F7F7, gdVertical);
      DrawToolBarItemBorder();
    end;
  end;
end;

initialization
  RegisterDrawer(TCDDrawerMac.Create, dsMacOSX);
end.

