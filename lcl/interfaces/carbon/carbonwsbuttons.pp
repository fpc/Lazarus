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
  Controls, Buttons, LCLType, LCLProc,
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
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCarbonWSSpeedButton }

  TCarbonWSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
  end;


implementation

{ TCarbonWSButton }

{------------------------------------------------------------------------------
  Method:  TCarbonWSButton.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface
  
  Creates new button control in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Control: ControlRef;
  CFString: CFStringRef;
  Info: PWidgetInfo;
begin
  Result := 0;

  // create the button at bounds with title
  CreateCarbonString(AParams.Caption, CFString);
  try
    if CreatePushButtonControl(GetTopParentWindow(AWinControl), ParamsToCarbonRect(AParams),
      CFString, Control) = noErr
    then
      Result := TLCLIntfHandle(Control);
  finally
    FreeCarbonString(CFString);
  end;
  if Result = 0 then Exit;

  // add the info (our data, like which TWinControl belong to this carbon widget)
  Info := CreateCtrlWidgetInfo(Control, AWinControl);
  
  // register events (e.g. mouse, focus, keyboard, size, ...)
  TCarbonPrivateHandleClass(WSPrivate).RegisterEvents(Info);
end;


{ TCarbonWSBitBtn }

{------------------------------------------------------------------------------
  Method:  TCarbonWSBitBtn.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new bitmap button control in Carbon interface with the specified
  parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Control: ControlRef;
  CFString: CFStringRef;
  Info: PWidgetInfo;
begin
  Result := 0;

  // create the button at bounds with title
  CreateCarbonString(AParams.Caption, CFString);
  try
    if CreateBevelButtonControl(GetTopParentWindow(AWinControl), ParamsToCarbonRect(AParams),
      CFString, kControlBevelButtonNormalBevel, kControlBehaviorPushbutton,
      nil, 0, 0, 0, Control) = noErr
    then
      Result := TLCLIntfHandle(Control);
  finally
    FreeCarbonString(CFString);
  end;
  if Result = 0 then Exit;

  // add the info (our data, like which TWinControl belong to this carbon widget)
  Info := CreateCtrlWidgetInfo(Control, AWinControl);

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
  RegisterWSComponent(TCustomBitBtn, TCarbonWSBitBtn);
//  RegisterWSComponent(TCustomSpeedButton, TCarbonWSSpeedButton);
////////////////////////////////////////////////////
end.
