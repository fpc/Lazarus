{ $Id$}
{
 *****************************************************************************
 *                            Win32WSControls.pp                             * 
 *                            ------------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit Win32WSControls;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Controls,
////////////////////////////////////////////////////
  WSControls, WSLCLClasses;

type

  { TWin32WSDragImageList }

  TWin32WSDragImageList = class(TWSDragImageList)
  private
  protected
  public
  end;

  { TWin32WSControl }

  TWin32WSControl = class(TWSControl)
  private
  protected
  public
    class procedure SetCursor(const AControl: TControl; const ACursor: TCursor); override;
  end;

  { TWin32WSWinControl }

  TWin32WSWinControl = class(TWSWinControl)
  private
  protected
  public
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
  end;

  { TWin32WSGraphicControl }

  TWin32WSGraphicControl = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TWin32WSCustomControl }

  TWin32WSCustomControl = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TWin32WSImageList }

  TWin32WSImageList = class(TWSImageList)
  private
  protected
  public
  end;


implementation

uses
  Windows, Win32Int, InterfaceBase;

procedure TWin32WSControl.SetCursor(const AControl: TControl; const ACursor: TCursor);
begin
  Windows.SetCursor(Windows.LoadCursor(0, LclCursorToWin32CursorMap[ACursor]));
end;

procedure TWin32WSWinControl.SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
begin
  TWin32WidgetSet(InterfaceObject).RecreateWnd(AWinControl);
end;

procedure TWin32WSWinControl.SetColor(const AWinControl: TWinControl);
begin
  // TODO: to be implemented, had no implementation in LM_SETCOLOR message
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TDragImageList, TWin32WSDragImageList);
  RegisterWSComponent(TControl, TWin32WSControl);
  RegisterWSComponent(TWinControl, TWin32WSWinControl);
//  RegisterWSComponent(TGraphicControl, TWin32WSGraphicControl);
//  RegisterWSComponent(TCustomControl, TWin32WSCustomControl);
//  RegisterWSComponent(TImageList, TWin32WSImageList);
////////////////////////////////////////////////////
end.
