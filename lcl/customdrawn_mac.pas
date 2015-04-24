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
    procedure DrawExpandTriangle(ADest: TCanvas; ASize: TSize;
      AX, AY: Integer; AFacing: TCDControlStateFlag);
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

procedure TCDDrawerMac.DrawExpandTriangle(ADest: TCanvas; ASize: TSize; AX,
  AY: Integer; AFacing: TCDControlStateFlag);
var
  lPoints: array of TPoint;
  R: TRect;
begin
  SetLength(lPoints, 3);
  R := Bounds(AX, AY, ASize.CX, ASize.CY);

  case AFacing of
  csfLeftArrow:
  begin
    lPoints[0] := Types.Point(R.Right-1, R.Top);
    lPoints[1] := Types.Point(R.Right-1, R.Bottom-2);
    lPoints[2] := Types.Point(R.Left+1, (R.Top + R.Bottom-2) div 2);
  end;
  csfRightArrow: // face right
  begin
    lPoints[0] := Types.Point(R.Left+1, R.Top);
    lPoints[1] := Types.Point(R.Left+1, R.Bottom-2);
    lPoints[2] := Types.Point(R.Right-1, (R.Top + R.Bottom-2) div 2);
  end;
  csfDownArrow: // face down
  begin
    lPoints[0] := Types.Point(R.Left, R.Top);
    lPoints[1] := Types.Point(R.Right-2, R.Top);
    lPoints[2] := Types.Point((R.Left + R.Right-2) div 2, R.Bottom-2);
  end;
  csfUpArrow:
  begin
    lPoints[0] := Types.Point(R.Left, R.Bottom-2);
    lPoints[1] := Types.Point(R.Right-2, R.Bottom-2);
    lPoints[2] := Types.Point((R.Left + R.Right-2) div 2, R.Top);
  end;
  else
    Exit;
  end;

  // select the appropriate brush & pen
  ADest.Brush.Color := $797979;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Color := $797979;
  ADest.Pen.Style := psSolid;

  // Draw the triangle
  ADest.Polygon(lPoints);
end;

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
  case ACurItem.Kind of
  tikSeparator, tikDivider:
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
  end;
  tikButton, tikCheckButton, tikDropDownButton:
  begin
    if ACurItem.SubpartKind = tiskArrow then
    begin
      // Centralize the arrow in the available space
      lX := AX - ASize.CX div 2;
      lY1 := AY - ASize.CY div 2;
      DrawExpandTriangle(ADest, ASize, lX, lY1, csfDownArrow);
      Exit;
    end;

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
end;

initialization
  RegisterDrawer(TCDDrawerMac.Create, dsMacOSX);
end.

