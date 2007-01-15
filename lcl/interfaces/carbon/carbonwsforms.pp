{ $Id$}
{
 *****************************************************************************
 *                             CarbonWSForms.pp                              *
 *                               ------------                                *
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
unit CarbonWSForms;

{$mode objfpc}{$H+}

interface

uses
  // libs
  FPCMacOSAll, CarbonUtils, CarbonExtra,
  // LCL
  Controls, Forms, LCLType, LMessages, LCLProc,
  // widgetset
  WSForms, WSLCLClasses, WSProc,
  // interface
  CarbonDef, CarbonProc, CarbonPrivate,
  CarbonWSControls;

type

  { TCarbonWSScrollingWinControl }

  TCarbonWSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
  end;

  { TCarbonWSScrollBox }

  TCarbonWSScrollBox = class(TWSScrollBox)
  private
  protected
  public
  end;

  { TCarbonWSCustomFrame }

  TCarbonWSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TCarbonWSFrame }

  TCarbonWSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TCarbonWSCustomForm }
  TCarbonWSCustomFormClass = class of TCarbonWSCustomForm;
  TCarbonWSCustomForm = class(TWSCustomForm)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TCarbonWSForm }

  TCarbonWSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TCarbonWSHintWindow }

  TCarbonWSHintWindow = class(TWSHintWindow)
  private
  protected
  public
  end;

  { TCarbonWSScreen }

  TCarbonWSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TCarbonWSApplicationProperties }

  TCarbonWSApplicationProperties = class(TWSApplicationProperties)
  private
  protected
  public
  end;


implementation

  { TCarbonWSCustomForm }


class function TCarbonWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Window: FPCMacOSAll.WindowRef;
  CFString: CFStringRef;
  NewBounds: FPCMacOSAll.Rect;
  Info: PWidgetInfo;
  NewWindowClass: Integer;
  MinSize: HISize;
  MaxSize: HISize;
begin
  Result := 0;

  NewBounds.Left := AParams.X;
  NewBounds.Top := AParams.Y;
  NewBounds.Right := AParams.X + AParams.Width;
  NewBounds.Bottom := AParams.Y + AParams.Height;
  
  DebugLn('TCarbonWSCustomForm.CreateHandle NewBounds=',dbgs(NewBounds),' Title=',AParams.Caption);

  NewWindowClass:=kDocumentWindowClass;
                                                    
  if CreateNewWindow(NewWindowClass,
                     kWindowCompositingAttribute or 
                     kWindowStandardDocumentAttributes or
                     kWindowStandardHandlerAttribute or
                     kWindowLiveResizeAttribute or
                     kWindowInWindowMenuAttribute,
                     NewBounds, Window
                    ) = noErr
  then Result := TLCLIntfHandle(Window);
  if Result = 0 then Exit;

  CFString := CFStringCreateWithCString(nil, Pointer(AParams.Caption),
                                        DEFAULT_CFSTRING_ENCODING);
  SetWindowTitleWithCFString(Window, CFString);
  CFRelease(Pointer(CFString));

  Info := CreateWndWidgetInfo(Window, AWinControl);
  TCarbonPrivateHandleClass(WSPrivate).RegisterEvents(Info);
  
  MinSize.width:=AWinControl.Constraints.EffectiveMinWidth;
  MinSize.height:=AWinControl.Constraints.EffectiveMinHeight;
  MaxSize.width:=AWinControl.Constraints.EffectiveMaxWidth;
  MaxSize.height:=AWinControl.Constraints.EffectiveMaxHeight;
  if MaxSize.width<=0 then
    MaxSize.width:=10000;
  if MaxSize.height<=0 then
    MaxSize.height:=10000;
  SetWindowResizeLimits(Window,@MinSize,@MaxSize);

  // The window was created hidden so show it.
  ShowWindow(Window);
end;

class procedure TCarbonWSCustomForm.DestroyHandle(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWincontrol, 'DestroyHandle')
  then Exit;

  DisposeWindow(WindowRef(AWinControl.Handle));
end;

class procedure TCarbonWSCustomForm.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  CFString: CFStringRef;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetText')
  then Exit;

  CFString := CFStringCreateWithCString(nil, Pointer(PChar(AText)),
                                        DEFAULT_CFSTRING_ENCODING);
  SetWindowTitleWithCFString(WindowRef(AWinControl.Handle), CFString);
  CFRelease(Pointer(CFString));
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollingWinControl, TCarbonWSScrollingWinControl);
//  RegisterWSComponent(TScrollBox, TCarbonWSScrollBox);
//  RegisterWSComponent(TCustomFrame, TCarbonWSCustomFrame);
//  RegisterWSComponent(TFrame, TCarbonWSFrame);
  RegisterWSComponent(TCustomForm, TCarbonWSCustomForm, TCarbonPrivateWindow);
//  RegisterWSComponent(TForm, TCarbonWSForm);
//  RegisterWSComponent(THintWindow, TCarbonWSHintWindow);
//  RegisterWSComponent(TScreen, TCarbonWSScreen);
//  RegisterWSComponent(TApplicationProperties, TCarbonWSApplicationProperties);
////////////////////////////////////////////////////

end.
