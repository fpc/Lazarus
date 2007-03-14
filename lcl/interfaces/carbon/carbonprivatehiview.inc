{%MainUnit carbonprivate.pp}
{ $Id: $}
{
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

// ==================================================================
// H A N D L E R S 
// ==================================================================

function CarbonPrivateHIView_Hit(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AInfo: TCarbonWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Value: UInt32;
  I: Integer;
  RadioButton: TRadioButton;
begin
  DebugLn('PrivateHiView_Hit: ', (AInfo.LCLObject as TComponent).Name);
  Result := CallNextEventHandler(ANextHandler, AEvent);
  
  LCLSendClickedMsg(AInfo.LCLObject as TControl);
end;

function CarbonPrivateHIView_MouseMove(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AInfo: TCarbonWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  {$ifdef VerboseMouse}
  DebugLn('PrivateHiView_MouseMove: ', (AInfo.LCLObject as TComponent).Name);
  {$endif}
  Result := CallNextEventHandler(ANextHandler, AEvent);
end;

function CarbonPrivateHIView_SetFocusPart(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AInfo: TCarbonWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
  //Focus: ControlTypeCode;
begin
  DebugLn('PrivateHiView_SetFocusPart: ', (AInfo.LCLObject as TComponent).Name);
  Result := CallNextEventHandler(ANextHandler, AEvent);
  
  (*if GetEventParameter(AEvent, kEventParamControlPart, typeControlPartCode, nil,
                      SizeOf(Boolean), nil, @Focus) <> noErr then Focus := kControlFocusNoPart;
                      
  
  FillChar(Msg, SizeOf(Msg), 0);
  if Focus then Msg.msg := LM_SETFOCUS
  else Msg.msg := LM_FILLFOCUS;
  DeliverMessage(AInfo.LCLObject, Msg);*)
end;

function CarbonPrivateHIView_SetCursor(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AInfo: TCarbonWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
{
  Msg: TLMessage;
}
  ACursor: TCursor;
begin
  // too much messages in terminal
  // DebugLn('PrivateHiView_SetCursor: ', (AInfo.LCLObject as TComponent).Name);
  CallNextEventHandler(ANextHandler, AEvent);
  
{
  Paul Ishenin: maybe we should ask control about it cursor via LM_SetCursor ???
  
  FillChar(Msg, SizeOf(Msg), 0);
  Msg.msg := LM_SETCURSOR;
  DeliverMessage(AInfo.LCLObject, Msg);
}

  ACursor := Screen.Cursor;
  if ACursor = crDefault then
  begin
    if (AInfo.LCLObject is TControl) then
      ACursor := TControl(AInfo.LCLObject).Cursor;
  end;
  WidgetSet.SetCursor(Screen.Cursors[ACursor]);
  Result := noErr; // cursor was setted
end;

// ==================================================================
// C L A S S 
// ==================================================================

{ TCarbonWSWinControlPrivate }

class procedure TCarbonPrivateHiView.RegisterEvents(AInfo: TCarbonWidgetInfo);
var
  MouseSpec: array [0..6] of EventTypeSpec;
  TmpSpec: EventTypeSpec;
begin
  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlDispose);
  InstallControlEventHandler(AInfo.Widget,
    RegisterEventHandler(@CarbonPrivateCommon_Dispose),
    1, @TmpSpec, Pointer(AInfo), nil);

  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlHit);
  InstallControlEventHandler(AInfo.Widget,
    RegisterEventHandler(@CarbonPrivateHIView_Hit),
    1, @TmpSpec, Pointer(AInfo), nil);

  MouseSpec[0] := MakeEventSpec(kEventClassMouse, kEventMouseMoved);
  MouseSpec[1] := MakeEventSpec(kEventClassMouse, kEventMouseDragged);
  
  InstallControlEventHandler(AInfo.Widget,
    RegisterEventHandler(@CarbonPrivateHIView_MouseMove),
    2, @MouseSpec[0], Pointer(AInfo), nil);

  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlDraw);
  InstallControlEventHandler(AInfo.Widget,
    RegisterEventHandler(@CarbonPrivateCommon_Draw),
    1, @TmpSpec, Pointer(AInfo), nil);
    
  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlBoundsChanged);
  InstallControlEventHandler(AInfo.Widget,
    RegisterEventHandler(@CarbonPrivateCommon_BoundsChanged),
    1, @TmpSpec, Pointer(AInfo), nil);
    
  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlSetFocusPart);
  InstallControlEventHandler(AInfo.Widget,
    RegisterEventHandler(@CarbonPrivateHIView_SetFocusPart),
    1, @TmpSpec, Pointer(AInfo), nil);
  
  // cursor set
  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlSetCursor);
  InstallControlEventHandler(AInfo.Widget,
    RegisterEventHandler(@CarbonPrivateHIView_SetCursor),
    1, @TmpSpec, Pointer(AInfo), nil);
    
  DebugLn(TWinControl(AInfo.LCLObject).Name,':', TWinControl(Ainfo.LCLObject).ClassName ,' Events set')
end;

class procedure TCarbonPrivateHiView.UnregisterEvents;
begin
  UnregisterEventHandler(@CarbonPrivateCommon_Dispose);
  UnregisterEventHandler(@CarbonPrivateHIView_Hit);
  UnregisterEventHandler(@CarbonPrivateHIView_MouseMove);
  UnregisterEventHandler(@CarbonPrivateCommon_Draw);
  UnregisterEventHandler(@CarbonPrivateCommon_BoundsChanged);
  UnregisterEventHandler(@CarbonPrivateHIView_SetCursor);
end;
