{ $Id$}
{
 *****************************************************************************
 *                              Win32WSArrow.pp                              * 
 *                              ---------------                              * 
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
unit Win32WSArrow;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Windows, Arrow, Graphics,
////////////////////////////////////////////////////
  WSArrow, WSLCLClasses;

type

  { TWin32WSArrow }

  TWin32WSArrow = class(TWSArrow)
  published
    class procedure SetType(const AArrow: TArrow; const AArrowType: TArrowType; 
      const AShadowType: TShadowType); override;
    class procedure DrawArrow(const AArrow: TArrow; const ACanvas: TCanvas); override;
  end;


implementation

{ TWin32WSArrow }

class procedure TWin32WSArrow.SetType(const AArrow: TArrow; const AArrowType: TArrowType;
  const AShadowType: TShadowType);
begin
  // TODO: implement me!
end;

class procedure TWin32WSArrow.DrawArrow(const AArrow: TArrow;
  const ACanvas: TCanvas);
const
    { up, down, left, right }
  ArrowTypeToState: array[TArrowType] of dword = (DFCS_SCROLLUP, DFCS_SCROLLDOWN,
    DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT);
var
  drawRect: Windows.RECT;
  canvasHandle: HDC;
begin
  drawRect := AArrow.ClientRect;
  canvasHandle := ACanvas.Handle;
  Windows.FillRect(canvasHandle, drawRect, GetSysColorBrush(COLOR_BTNFACE));
  dec(drawRect.Left, 2);
  dec(drawRect.Top, 2);
  inc(drawRect.Right, 2);
  inc(drawRect.Bottom, 2);
  Windows.DrawFrameControl(canvasHandle, drawRect, DFC_SCROLL, ArrowTypeToState[AArrow.ArrowType]);
end;

end.
