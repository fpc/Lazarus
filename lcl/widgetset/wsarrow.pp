{ $Id$}
{
 *****************************************************************************
 *                                WSArrow.pp                                 * 
 *                                ----------                                 * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit WSArrow;

{$mode objfpc}{$H+}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Types, Math, Arrow, Graphics,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, WSFactory;

type
  { TWSArrow }
  
  TWSArrowClass = class of TWSArrow;
  TWSArrow = class(TWSCustomControl)
  published
    class procedure SetType(const AArrow: TArrow; const AArrowType: TArrowType; 
      const AShadowType: TShadowType); virtual;
    class procedure DrawArrow(const AArrow: TArrow; const ACanvas: TCanvas); virtual;
  end;

  { WidgetSetRegistration }

  procedure RegisterArrow;

implementation

{ TWSArrow }

class procedure TWSArrow.SetType(const AArrow: TArrow; const AArrowType: TArrowType;
  const AShadowType: TShadowType);
begin
end;

class procedure TWSArrow.DrawArrow(const AArrow: TArrow;
  const ACanvas: TCanvas);
var
  P: Array [0..2] of TPoint;
  R: TRect;
  S: Integer;
begin
  R := AArrow.ClientRect;
  InflateRect(R, -1, -1);
  // arrow bounds are square
  S := Min(R.Right - R.Left, R.Bottom - R.Top);
  R := Bounds((R.Left + R.Right - S) div 2, (R.Top + R.Bottom - S) div 2, S, S);

  ACanvas.Brush.Color := clBlack;
  ACanvas.Pen.Color := clBlack;

  case Ord(AArrow.ArrowType) of
    0: // up
      begin
        P[0] := Point(R.Left, R.Bottom);
        P[1] := Point((R.Left + R.Right) div 2, R.Top);
        P[2] := R.BottomRight;
      end;
    1: // down
      begin
        P[0] := R.TopLeft;
        P[1] := Point(R.Right, R.Top);
        P[2] := Point((R.Left + R.Right) div 2, R.Bottom);
      end;
    2: // left
      begin
        P[0] := R.BottomRight;
        P[1] := Point(R.Left, (R.Top + R.Bottom) div 2);
        P[2] := Point(R.Right, R.Top);
      end;
    3: // right
      begin
        P[0] := R.TopLeft;
        P[1] := Point(R.Right, (R.Top + R.Bottom) div 2);
        P[2] := Point(R.Left, R.Bottom);
      end;
  end;

  ACanvas.Polygon(P);
end;

{ WidgetSetRegistration }

procedure RegisterArrow;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterArrow;
//  if not WSRegisterArrow then
//    RegisterWSComponent(TArrow, TWSArrow);
  Done := True;
end;

end.
