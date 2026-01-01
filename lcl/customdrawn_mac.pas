unit CustomDrawn_Mac;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils, Types, fpcanvas, fpimage, Math,
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
    //
    procedure DrawMacSquareButton(ADest: TFPCustomCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDButtonStateEx);
  public
    function GetMeasures(AMeasureID: Integer): Integer; override;
    // ===================================
    // Standard Tab
    // ===================================
    // TCDButton
    procedure DrawButton(ADest: TFPCustomCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDButtonStateEx); override;
    // ===================================
    // Common Controls Tab
    // ===================================
    // TCDToolBar
    procedure DrawToolBarItem(ADest: TCanvas; ASize: TSize;
      ACurItem: TCDToolBarItem; AX, AY: Integer;
      AState: TCDControlState; AStateEx: TCDToolBarStateEx); override;
  end;

implementation

const

  // Button

  MAC_SQUARE_BUTTON_FOCUS_FRAME_OUTTER = $00D7BE9F;
  MAC_SQUARE_BUTTON_FOCUS_FRAME_INNER = $00F7DEBF; // actually it is a gradient as well
  //
  MAC_SQUARE_BUTTON_FRAME = $00AFAFAF;
  MAC_SQUARE_BUTTON_FOCUS_GRADIENT_TOP = $00E3E3E3;
  MAC_SQUARE_BUTTON_FOCUS_GRADIENT_BOTTOM = $00F7F7F7;
  //
  MAC_SQUARE_BUTTON_SUNKEN_GRADIENT_TOP = $00AFAFAF;
  MAC_SQUARE_BUTTON_SUNKEN_GRADIENT_BOTTOM = $00C5C5C5;


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

procedure TCDDrawerMac.DrawMacSquareButton(ADest: TFPCustomCanvas;
  ADestPos: TPoint; ASize: TSize; AState: TCDControlState; AStateEx: TCDButtonStateEx);
var
  lDest: TCanvas absolute ADest;
  Str: string;
  lColor: TColor;
  lRect: TRect;
  lFrameDark, lFrameMedDark, lFrameMedium, lFrameLight: TColor;
  lSelTop, lSelTopGrad, lSelBottomGrad, lSelBottom: TColor;
  lGradientTop, lGradientBottom: TColor;
  lPosX, lPosY: Integer;
begin
  // Main body with gradient
  if csfSunken in AState then
  begin
    lGradientTop := MAC_SQUARE_BUTTON_SUNKEN_GRADIENT_TOP;
    lGradientBottom := MAC_SQUARE_BUTTON_SUNKEN_GRADIENT_BOTTOM;
  end
  else// if csfEnabled in AState then
  begin
    lGradientTop := MAC_SQUARE_BUTTON_FOCUS_GRADIENT_TOP;
    lGradientBottom := MAC_SQUARE_BUTTON_FOCUS_GRADIENT_BOTTOM;
  end;
  lRect := Bounds(ADestPos.X, ADestPos.Y, ASize.cx, ASize.cy);
  lDest.GradientFill(lRect, lGradientTop, lGradientBottom, gdVertical);

  // outter rectangle
  lColor := AStateEx.ParentRGBColor;
  ADest.Brush.Style := bsClear;
  if (csfHasFocus in AState) and not (csfSunken in AState) then
    lDest.Pen.Color := MAC_SQUARE_BUTTON_FOCUS_FRAME_OUTTER
  else
    lDest.Pen.Color := MAC_SQUARE_BUTTON_FRAME;
  ADest.Rectangle(Bounds(ADestPos.X, ADestPos.Y, ASize.cx, ASize.cy));
  //ADest.Rectangle(1, 1, ASize.cx-1, ASize.cy-1);

  // inner rectangle (only for focused)
  if (csfHasFocus in AState) and not (csfSunken in AState) then
  begin
    lDest.Pen.Color := MAC_SQUARE_BUTTON_FOCUS_FRAME_INNER;
    ADest.Rectangle(Bounds(ADestPos.X+1, ADestPos.Y+1, ASize.cx-2, ASize.cy-2));
    //ADest.Rectangle(2, 2, ASize.cx-2, ASize.cy-2);
    //ADest.Rectangle(3, 3, ASize.cx-3, ASize.cy-3);
  end;

  // Button text
  if AStateEx.Font <> nil then
    ADest.Font.Assign(AStateEx.Font);
  ADest.Brush.Style := bsClear;
  ADest.Pen.Style := psSolid;
  if (csfSunken in AState) then
    ADest.Font.FPColor := colWhite;
  Str := AStateEx.Caption;
  lPosX := ADestPos.X + (ASize.cx - lDest.TextWidth(Str)) div 2;
  lPosY := ADestPos.Y + (ASize.cy - lDest.TextHeight(Str)) div 2;
  lDest.TextOut(lPosX, lPosY, Str);
end;

function TCDDrawerMac.GetMeasures(AMeasureID: Integer): Integer;
begin
  case AMeasureID of
    //
    TCDTOOLBAR_ITEM_ARROW_WIDTH: Result := 10;
  else
    Result:=inherited GetMeasures(AMeasureID);
  end;
end;

procedure TCDDrawerMac.DrawButton(ADest: TFPCustomCanvas; ADestPos: TPoint; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDButtonStateEx);
begin
  DrawMacSquareButton(ADest, ADestPos, ASize, AState, AStateEx);
end;

procedure TCDDrawerMac.DrawToolBarItem(ADest: TCanvas; ASize: TSize;
  ACurItem: TCDToolBarItem; AX, AY: Integer; AState: TCDControlState;
  AStateEx: TCDToolBarStateEx);
var
  lX, lY1, lY2, lEffWidth: Integer;

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
      if ACurItem.Width > 0 then
        lEffWidth := ACurItem.Width
      else
        lEffWidth := Min(ASize.CX, GetMeasures(TCDTOOLBAR_ITEM_ARROW_WIDTH));
      lX := AX + (ASize.CX - lEffWidth) div 2;
      lY1 := AY + (ASize.CY - lEffWidth) div 2;
      ASize.CY := lEffWidth;
      ASize.CX := lEffWidth;
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

