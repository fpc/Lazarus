{ $Id$}
{
 *****************************************************************************
 *                            CarbonWSControls.pp                            *
 *                              ---------------                              *
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
unit CarbonWSControls;

{$mode objfpc}{$H+}

interface

uses
  // libs
  FPCMacOSAll, CarbonUtils, CarbonExtra,
  // LCL
  Controls, LCLType, LMessages, LCLProc,
  // widgetset
  WSControls, WSLCLClasses, WSProc,
  // interface
  CarbonDef, CarbonProc, CarbonPrivate;

type

  { TCarbonWSDragImageList }

  TCarbonWSDragImageList = class(TWSDragImageList)
  private
  protected
  public
  end;

  { TCarbonWSControl }

  TCarbonWSControl = class(TWSControl)
  private
  protected
  public
  end;

  { TCarbonWSWinControl }

  TCarbonWSWinControl = class(TWSWinControl)
  private
  protected
  public
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

  { TCarbonWSGraphicControl }

  TCarbonWSGraphicControl = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TCarbonWSCustomControl }

  TCarbonWSCustomControl = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TCarbonWSImageList }

  TCarbonWSImageList = class(TWSImageList)
  private
  protected
  public
  end;


implementation

  { TCarbonWSWinControl }

procedure TCarbonWSWinControl.SetText(const AWinControl: TWinControl; const AText: String);
var
  CFString: CFStringRef;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetText')
  then Exit;

  CFString := CFStringCreateWithCString(nil, Pointer(PChar(AText)), DEFAULT_CFSTRING_ENCODING);
  SetControlTitleWithCFString(ControlRef(AWinControl.Handle), CFString);
  CFRelease(Pointer(CFString));
end;

procedure TCarbonWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWincontrol, 'DestroyHandle')
  then Exit;
  DisposeControl(ControlRef(AWinControl.Handle));
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TDragImageList, TCarbonWSDragImageList);
//  RegisterWSComponent(TControl, TCarbonWSControl);
  RegisterWSComponent(TWinControl, TCarbonWSWinControl, TCarbonPrivateHiView);
//  RegisterWSComponent(TGraphicControl, TCarbonWSGraphicControl);
//  RegisterWSComponent(TCustomControl, TCarbonWSCustomControl);
//  RegisterWSComponent(TImageList, TCarbonWSImageList);
////////////////////////////////////////////////////

finalization
//  if MCarbonWSWinControl_Dispose_UPP <> nil then DisposeEventHandlerUPP(MCarbonWSWinControl_Dispose_UPP);
//  if MCarbonWSWinControl_Hit_UPP <> nil then DisposeEventHandlerUPP(MCarbonWSWinControl_Hit_UPP);

end.
