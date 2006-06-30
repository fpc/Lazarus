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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  FPCMacOSAll,
  // LCL
  Controls, Buttons, LCLType,
  // widgetset
  WSButtons, WSLCLClasses,
  // interface
  CarbonDef, CarbonProc, CarbonPrivate,
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

class function TCarbonWSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  //Button: TCustomButton;
  Control: ControlRef;
  CFString: CFStringRef;
  R: FPCMacOSAll.Rect;
  Info: PWidgetInfo;
begin
  Result := 0;
  //Button := AWinControl as TCustomButton;

  // create the button at bounds with title
  R:=GetCarbonRect(AParams.X,AParams.Y,
                   AParams.X + AParams.Width,AParams.Y + AParams.Height);

  CFString := CFStringCreateWithCString(nil, Pointer(AParams.Caption),
                                        DEFAULT_CFSTRING_ENCODING);
  if CreatePushButtonControl(WindowRef(AParams.WndParent), R,
    CFString, Control) = noErr
  then
    Result := TLCLIntfHandle(Control);
  CFRelease(Pointer(CFString));
  if Result = 0 then Exit;

  // add the info (our data, like which TWinControl belong to this carbon widget)
  Info := CreateWidgetInfo(Control, AWinControl, cwtControlRef);
  
  // register events (e.g. mouse, focus, keyboard, size, ...)
  TCarbonPrivateHandleClass(WSPrivate).RegisterEvents(Info);
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
