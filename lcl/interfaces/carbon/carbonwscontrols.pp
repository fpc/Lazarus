{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSControls.pp                              * 
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
  Carbon, CarbonUtils, CarbonExtra,
  // LCL
  Controls, LCLType, LMessages, LCLProc, 
  // widgetset
  WSControls, WSLCLClasses, WSProc, 
  // interface
  CarbonDef, CarbonProc;

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

  TCarbonWSWinControlPrivate = class(TWSPrivate)
  private
  protected
    class procedure InstallControlHandler(AInfo: PWidgetInfo; AClass: TFourCC; AKind: UInt32; AHandler: Pointer; var AUPP: EventHandlerUPP);
  public
    class procedure SetEvents(AInfo: PWidgetInfo); virtual;
  end;
  TCarbonWSWinControlPrivateClass = class of TCarbonWSWinControlPrivate;

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
  
var  
  MCarbonWSWinControl_Dispose_UPP: EventHandlerUPP = nil;
  MCarbonWSWinControl_Hit_UPP: EventHandlerUPP = nil;
  
  
function CarbonWSWinControl_Dispose(ANextHandler: EventHandlerCallRef;
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

function CarbonWSWinControl_Hit(ANextHandler: EventHandlerCallRef;
                                AEvent: EventRef;
                                AInfo: PWidgetInfo): OSStatus; stdcall;  
var
  Msg: TLMessage;
begin
  Result := CallNextEventHandler(ANextHandler, AEvent); 
  FillChar(Msg, SizeOf(Msg),0);
  Msg.msg := LM_CLICKED;
  DeliverMessage(AInfo^.LCLObject, Msg);
end; 

// ---------------

procedure TCarbonWSWinControl.SetText(const AWinControl: TWinControl; const AText: String);
var         
  CFString: CFStringRef;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetText')
  then Exit;

  CFString := CFStringCreateWithCString(nil, PChar(AText), DEFAULT_CFSTRING_ENCODING);
  SetControlTitleWithCFString(ControlRef(AWinControl.Handle), CFString);
  CFRelease(Pointer(CFString));
end;

procedure TCarbonWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
  DisposeControl(ControlRef(AWinControl.Handle));
end;

  { TCarbonWSWinControlPrivate }

procedure TCarbonWSWinControlPrivate.InstallControlHandler(AInfo: PWidgetInfo; AClass: TFourCC; AKind: UInt32; AHandler: Pointer; var AUPP: EventHandlerUPP);
var
  eventSpec: EventTypeSpec;
begin 
  if AUPP = nil
  then AUPP := NewEventHandlerUPP(EventHandlerProcPtr(AHandler));

  eventSpec := MakeEventSpec(AClass, AKind);
  InstallControlEventHandler(AInfo^.Widget, AUPP, 1, eventSpec, Pointer(AInfo), nil);
end;
  
procedure TCarbonWSWinControlPrivate.SetEvents(AInfo: PWidgetInfo); 
begin           
  InstallControlHandler(AInfo, kEventClassControl, kEventControlDispose, @CarbonWSWinControl_Dispose, MCarbonWSWinControl_Dispose_UPP);
  InstallControlHandler(AInfo, kEventClassControl, kEventControlHit, @CarbonWSWinControl_Hit, MCarbonWSWinControl_Hit_UPP);
  WriteLN('controls: Events set')
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
  RegisterWSComponent(TWinControl, TCarbonWSWinControl, TCarbonWSWinControlPrivate);
//  RegisterWSComponent(TGraphicControl, TCarbonWSGraphicControl);
//  RegisterWSComponent(TCustomControl, TCarbonWSCustomControl);
//  RegisterWSComponent(TImageList, TCarbonWSImageList);
////////////////////////////////////////////////////

finalization
  if MCarbonWSWinControl_Dispose_UPP <> nil then DisposeEventHandlerUPP(MCarbonWSWinControl_Dispose_UPP);
  if MCarbonWSWinControl_Hit_UPP <> nil then DisposeEventHandlerUPP(MCarbonWSWinControl_Hit_UPP);

end.
