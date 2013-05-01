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
{$I lcl_defines.inc}

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
  LCLProc, LCLIntf, LCLType, Math, Classes, Arrow, Graphics,
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

class procedure TWSArrow.DrawArrow(const AArrow: TArrow; const ACanvas: TCanvas);
const
  SpaceFactor = 5;
var
  drawRect: TRect;
  RegionHandle: HRGN;
  Pt: array[0..2] of TPoint;
  w, h, arrowSize1, arrowSize2, cx, cy, Space: integer;
begin
  drawRect := AArrow.ClientRect;
  w := drawRect.Right - drawRect.Left;
  h := drawRect.Bottom - drawRect.Top;
  Space := Min(w, h) div SpaceFactor;
  Dec(w, Space * 2);
  Dec(h, Space * 2);
  arrowSize1 := min(w,h) - 2;
  if (arrowSize1 mod 2) = 0 then Inc(arrowSize1);
  arrowSize2 := (arrowSize1 div 2) + 1;

  case AArrow.ArrowType of
    atUp:
      begin
        cx := drawRect.Left + Space + (max((w-arrowSize1) div 2,0)) - 1;
        cy := drawRect.Top + Space + (max((h-arrowSize2) div 2,0)) - 1;
        Pt[0].x := cx + (arrowSize1 div 2) + 1;
        Pt[0].y := cy;
        Pt[1].x := cx + arrowSize1 + 1;
        Pt[1].y := cy + arrowSize2 + 1;
        Pt[2].X := cx;
        Pt[2].y := cy + arrowSize2 + 1;
      end;
    atDown:
      begin
        cx := drawRect.Left + Space + (max((w-arrowSize1) div 2,0));
        cy := drawRect.Top + Space + (max((h-arrowSize2) div 2,0)) + 1;
        Pt[0].X := cx;
        Pt[0].y := cy;
        Pt[1].x := cx + arrowSize1;
        Pt[1].y := cy;
        Pt[2].x := cx + (arrowSize1 div 2);
        Pt[2].y := cy + arrowSize2;
     end;
   atLeft:
     begin
       cx := drawRect.Left + Space + (max((w-arrowSize2) div 2,0)) - 1;
       cy := drawRect.Top + Space + (max((h-arrowSize1) div 2,0));
       Pt[0].X := cx + arrowSize2;
       Pt[0].y := cy;
       Pt[1].x := cx + arrowSize2;
       Pt[1].y := cy + arrowSize1 + 1;
       Pt[2].x := cx;
       Pt[2].y := cy + (arrowSize1 div 2) + 1;
     end;
   atRight:
     begin
       cx := drawRect.Left + Space + (max((w-arrowSize2) div 2,0)) + 1;
       cy := drawRect.Top + Space + (max((h-arrowSize1) div 2,0));
       Pt[0].X := cx;
       Pt[0].y := cy;
       Pt[1].x := cx + arrowSize2;
       Pt[1].y := cy + (arrowSize1 div 2) + 1;
       Pt[2].x := cx;
       Pt[2].y := cy + arrowSize1 + 1;
    end;
   else
     exit;
  end;
  aCanvas.Brush.Style := bsClear;
  RegionHandle := CreatePolygonRgn(Pt, 3, ALTERNATE);
  try
    aCanvas.FillRect(drawRect);
    aCanvas.Brush.Color := aCanvas.Pen.Color; //TODO: what to do with brush color ?
    FillRgn(aCanvas.Handle, RegionHandle, aCanvas.Brush.Reference.Handle);
  finally
    DeleteObject(RegionHandle);
  end;
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
