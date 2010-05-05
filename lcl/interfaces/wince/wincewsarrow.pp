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
unit WinCEWSArrow;

{$mode objfpc}{$H+}

interface

uses
  // Libs
  Windows,
  // LCL
  Arrow, Graphics, win32compat,
  // Widgetset
  WSArrow, WSLCLClasses;

type

  { TWinCEWSArrow }

  TWinCEWSArrow = class(TWSArrow)
  published
    class procedure SetType(const AArrow: TArrow; const AArrowType: TArrowType; 
      const AShadowType: TShadowType); override;
    class procedure DrawArrow(const AArrow: TArrow; const ACanvas: TCanvas); override;
  end;


implementation

{ TWinCEWSArrow }

class procedure TWinCEWSArrow.SetType(const AArrow: TArrow; const AArrowType: TArrowType;
  const AShadowType: TShadowType);
begin
  // TODO: implement me!
end;

class procedure TWinCEWSArrow.DrawArrow(const AArrow: TArrow;
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
  Windows.FillRect(canvasHandle, drawRect, GetSysColorBrush(COLOR_BTNFACE or SYS_COLOR_INDEX_FLAG));
  dec(drawRect.Left, 2);
  dec(drawRect.Top, 2);
  inc(drawRect.Right, 2);
  inc(drawRect.Bottom, 2);
  Windows.DrawFrameControl(canvasHandle, @drawRect, DFC_SCROLL, ArrowTypeToState[AArrow.ArrowType]);
end;

end.
