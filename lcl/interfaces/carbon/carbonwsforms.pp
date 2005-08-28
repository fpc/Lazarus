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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  Carbon, CarbonUtils, CarbonExtra,
  // LCL
  Controls, Forms, LCLType, LMessages, LCLProc, 
  // widgetset
  WSForms, WSLCLClasses, WSProc,
  // interface                  
  CarbonDef, CarbonProc, 
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

  TCarbonWSCustomForm = class(TWSCustomForm)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  TCarbonWSCustomFormPrivate = class(TCarbonWSWinControlPrivate)
  private
  protected
    class procedure InstallWindowHandler(AInfo: PWidgetInfo; AClass: TFourCC; AKind: UInt32; AHandler: Pointer; var AUPP: EventHandlerUPP);
  public
    class procedure SetEvents(AInfo: PWidgetInfo); override;
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

var  
  MCarbonWSCustomForm_Close_UPP: EventHandlerUPP = nil;
  MCarbonWSCustomForm_Closed_UPP: EventHandlerUPP = nil;
  MCarbonWSCustomForm_MouseDown_UPP: EventHandlerUPP = nil;
  
function CarbonWSCustomForm_Close(ANextHandler: EventHandlerCallRef;
                                  AEvent: EventRef;
                                  AInfo: PWidgetInfo): OSStatus; stdcall;  
var
  Msg: TLMessage;
begin            
  // Do canclose query, if false then exit
  
  FillChar(Msg, SizeOf(Msg),0);
  Msg.msg := LM_CLOSEQUERY;
  
  // Message results : 0 - do nothing, 1 - destroy or hide window 
  if DeliverMessage(AInfo^.LCLObject, Msg) = 0
  then begin
    Result := eventNotHandledErr;
    Exit;
  end;
  
  Result := CallNextEventHandler(ANextHandler, AEvent); 
end;

function CarbonWSCustomForm_Closed(ANextHandler: EventHandlerCallRef;
                                   AEvent: EventRef;
                                   AInfo: PWidgetInfo): OSStatus; stdcall;  
var
  Msg: TLMessage;
begin
  Result := CallNextEventHandler(ANextHandler, AEvent); 

  FillChar(Msg, SizeOf(Msg),0);
  Msg.msg := LM_DESTROY;
  DeliverMessage(AInfo^.LCLObject, Msg);
  
  FreeWidgetInfo(AInfo);
end;

function CarbonWSCustomForm_MouseDown(ANextHandler: EventHandlerCallRef;
                                      AEvent: EventRef;
                                      AInfo: PWidgetInfo): OSStatus; stdcall;  
begin
  Result := CallNextEventHandler(ANextHandler, AEvent); 
  //WriteLN('Form: MouseDown');
end; 
                                
// ---------------

function TCarbonWSCustomForm.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; 
var
  Form: TCustomForm;
  Window: WindowRef;
  CFString: CFStringRef;
  R: Rect;                
  Info: PWidgetInfo;
begin               
  Result := 0;
  Form := AWinControl as TCustomForm;
  
  R.Left := AParams.X;
  R.Top := AParams.Y;
  R.Right := AParams.X + AParams.Width;
  R.Bottom := AParams.Y + AParams.Height;

  if CreateNewWindow(kDocumentWindowClass,  
                     kWindowStandardDocumentAttributes or 
                     kWindowStandardHandlerAttribute or 
                     // $100 {kWindowMetalAttribute} or
                     kWindowInWindowMenuAttribute, 
                     R, Window
                    ) = noErr
  then Result := TLCLIntfHandle(Window);
  if Result = 0 then Exit;
                              
  CFString := CFStringCreateWithCString(nil, AParams.Caption, DEFAULT_CFSTRING_ENCODING);
  SetWindowTitleWithCFString(Window, CFString);                            
  CFRelease(Pointer(CFString));
      
  Info := CreateWidgetInfo(Window, AWinControl);
  TCarbonWSWinControlPrivateClass(WSPrivate).SetEvents(Info);

  // The window was created hidden so show it.
  ShowWindow(Window);
end;

procedure TCarbonWSCustomForm.DestroyHandle(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWincontrol, 'DestroyHandle')
  then Exit;
  
  DisposeWindow(WindowRef(AWinControl.Handle));
end;

procedure TCarbonWSCustomForm.SetText(const AWinControl: TWinControl; const AText: String); 
var         
  CFString: CFStringRef;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetText')
  then Exit;

  CFString := CFStringCreateWithCString(nil, PChar(AText), DEFAULT_CFSTRING_ENCODING);
  SetWindowTitleWithCFString(WindowRef(AWinControl.Handle), CFString);
  CFRelease(Pointer(CFString));
end;

  { TCarbonWSCustomFormPrivate }

procedure TCarbonWSCustomFormPrivate.InstallWindowHandler(AInfo: PWidgetInfo; AClass: TFourCC; AKind: UInt32; AHandler: Pointer; var AUPP: EventHandlerUPP);
var
  eventSpec: EventTypeSpec;
begin 
  if AUPP = nil
  then AUPP := NewEventHandlerUPP(EventHandlerProcPtr(AHandler));

  eventSpec := MakeEventSpec(AClass, AKind);
  InstallWindowEventHandler(AInfo^.Widget, AUPP, 1, eventSpec, Pointer(AInfo), nil);
end;
  
procedure TCarbonWSCustomFormPrivate.SetEvents(AInfo: PWidgetInfo);
begin       
  InstallWindowHandler(AInfo, kEventClassWindow, kEventWindowClose, @CarbonWSCustomForm_Close, MCarbonWSCustomForm_Close_UPP);
  InstallWindowHandler(AInfo, kEventClassWindow, kEventWindowClosed, @CarbonWSCustomForm_Closed, MCarbonWSCustomForm_Closed_UPP);
  InstallWindowHandler(AInfo, kEventClassMouse, kEventMouseDown, @CarbonWSCustomForm_MouseDown, MCarbonWSCustomForm_MouseDown_UPP);
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
  RegisterWSComponent(TCustomForm, TCarbonWSCustomForm, TCarbonWSCustomFormPrivate);
//  RegisterWSComponent(TForm, TCarbonWSForm);
//  RegisterWSComponent(THintWindow, TCarbonWSHintWindow);
//  RegisterWSComponent(TScreen, TCarbonWSScreen);
//  RegisterWSComponent(TApplicationProperties, TCarbonWSApplicationProperties);
////////////////////////////////////////////////////

finalization
  if MCarbonWSCustomForm_Close_UPP <> nil then DisposeEventHandlerUPP(MCarbonWSCustomForm_Close_UPP);
  if MCarbonWSCustomForm_Closed_UPP <> nil then DisposeEventHandlerUPP(MCarbonWSCustomForm_Closed_UPP);
  if MCarbonWSCustomForm_MouseDown_UPP <> nil then DisposeEventHandlerUPP(MCarbonWSCustomForm_MouseDown_UPP);

end.
