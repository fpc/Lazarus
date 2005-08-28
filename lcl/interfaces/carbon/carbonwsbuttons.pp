{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSButtons.pp                           *
 *                              --------------                               *
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
unit CarbonWSButtons;

{$mode objfpc}{$H+}

interface

uses     
  // libs
  Carbon, 
  // LCL
  Controls, Buttons, LCLType,
  // widgetset
  WSButtons, WSLCLClasses,
  // interface
  CarbonDef, CarbonProc,
  CarbonWSControls;

type

  { TCarbonWSButton }

  TCarbonWSButton = class(TWSButton)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCarbonWSBitBtn }

  TCarbonWSBitBtn = class(TWSBitBtn)
  private
  protected
  public
  end;

  { TCarbonWSSpeedButton }

  TCarbonWSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
  end;


implementation

  { TCarbonWSButton }

function TCarbonWSButton.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  Button: TCustomButton;
  Control: ControlRef;
  CFString: CFStringRef;
  R: Rect;
  Info: PWidgetInfo;
begin
  Result := 0;
  Button := AWinControl as TCustomButton;

  R.Left := AParams.X;
  R.Top := AParams.Y;
  R.Right := AParams.X + AParams.Width;
  R.Bottom := AParams.Y + AParams.Height;

  CFString := CFStringCreateWithCString(nil, AParams.Caption, DEFAULT_CFSTRING_ENCODING);
  if CreatePushButtonControl(WindowRef(AParams.WndParent), R, CFString, Control) = noErr
  then Result := TLCLIntfHandle(Control);
  CFRelease(Pointer(CFString));
  if Result = 0 then Exit;
  
  Info := CreateWidgetInfo(Control, AWinControl);
  TCarbonWSWinControlPrivateClass(WSPrivate).SetEvents(Info);
end;


initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomButton, TCarbonWSButton);
//  RegisterWSComponent(TCustomBitBtn, TCarbonWSBitBtn);
//  RegisterWSComponent(TCustomSpeedButton, TCarbonWSSpeedButton);
////////////////////////////////////////////////////
end.
