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

function CarbonCommon_Dispose(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  Result := CallNextEventHandler(ANextHandler, AEvent);

  LCLSendDestroyMsg(AWidget.LCLObject); // widget is disposed in DestroyHandle
end;

function CarbonCommon_Draw(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  AStruct: PPaintStruct;
begin
  {$IFDEF VerbosePaint}
    Debugln('CarbonCommon_Draw ', DbgSName(AWidget.LCLObject));
  {$ENDIF}
  
  // first let carbon draw/update
  Result := CallNextEventHandler(ANextHandler, AEvent);
  
  GetEventParameter(AEvent, kEventParamCGContextRef, typeCGContextRef, nil,
    SizeOf(CGContextRef), nil, @(AWidget.Context.CGContext));
  
  AWidget.Context.Reset;
  
  if (AWidget is TCarbonControl) and
    (cceDraw in (AWidget as TCarbonControl).GetValidEvents) then
    (AWidget as TCarbonControl).Draw;

  
  New(AStruct);
  FillChar(AStruct^, SizeOf(TPaintStruct), 0);
  AStruct^.hdc := HDC(AWidget.Context);
  try
    {$IFDEF VerbosePaint}
      DebugLn('CarbonCommon_Draw LM_PAINT to ', DbgSName(AWidget.LCLObject));
    {$ENDIF}
    LCLSendPaintMsg(AWidget.LCLObject, HDC(AWidget.Context), AStruct);
  finally
    Dispose(AStruct);
  end;
end;

function CarbonCommon_BoundsChanged(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  AWinControl: TWinControl;
  WidgetBounds, OldBounds, ResizedBounds: TRect;
  Resized: Boolean;
begin
  {$IFDEF VerboseCommonEvent}
    DebugLn('CarbonCommon_BoundsChanged ', DbgSName(AWidget.LCLObject));
  {$ENDIF}
  // first let carbon draw/update
  Result := CallNextEventHandler(ANextHandler, AEvent);
  
  if AWidget.LCLObject is TWinControl then
  begin
    AWinControl := TWinControl(AWidget.LCLObject);
    AWidget.GetBounds(WidgetBounds);
    
    OldBounds := AWinControl.BoundsRect;
    
    // form expects client size
    if AWidget is TCarbonWindow then
    begin
      AWidget.GetClientRect(ResizedBounds);
      OffsetRect(ResizedBounds, WidgetBounds.Left, WidgetBounds.Top);
    end
    else ResizedBounds := WidgetBounds;

    // then send a LM_SIZE message
    if (OldBounds.Right - OldBounds.Left <> WidgetBounds.Right - WidgetBounds.Left) or
       (OldBounds.Bottom - OldBounds.Top <> WidgetBounds.Bottom - WidgetBounds.Top) then
    begin
      LCLSendSizeMsg(AWinControl, ResizedBounds.Right - ResizedBounds.Left,
        ResizedBounds.Bottom - ResizedBounds.Top, Size_SourceIsInterface);
      Resized := True;
    end
    else Resized := False;

    // then send a LM_MOVE message
    if (OldBounds.Left <> WidgetBounds.Left) or
       (OldBounds.Top <> WidgetBounds.Top) then
    begin
      LCLSendMoveMsg(AWinControl, ResizedBounds.Left,
        ResizedBounds.Top, Move_SourceIsInterface);
    end;
    
    // invalidate control canvas
    if Resized then AWidget.Invalidate;
    
    // invalidate parent client area, previously covered by control
    if (AWinControl.Parent <> nil) and AWinControl.Parent.HandleAllocated then
    begin
      TCarbonWidget(AWinControl.Parent.Handle).Invalidate(@OldBounds);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Name: CarbonCommon_TrackProgress
  Handles all mouse dragging events
 ------------------------------------------------------------------------------}
procedure CarbonCommon_TrackProgress(AControl: ControlRef;
  APartCode: ControlPartCode); {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Msg: TLMMouseMove;
  P: TPoint;
  Widget: TCarbonWidget;
begin
  {$IFDEF VerboseMouse}
    DebugLn('CarbonCommon_TrackProgress');
  {$ENDIF}

  Widget := GetCarbonWidget(AControl);

  if Widget <> nil then
  begin
    P := Widget.GetMousePos;

    if Widget is TCarbonControl then
      if cceDoAction in (Widget as TCarbonControl).GetValidEvents then
        (Widget as TCarbonControl).DoAction(APartCode);

    FillChar(Msg, SizeOf(TLMMouseMove), 0);
    Msg.Msg := LM_MOUSEMOVE;
    Msg.XPos := P.Y;
    Msg.YPos := P.X;
    Msg.Keys := GetCarbonMsgKeyState;
    DeliverMessage(Widget.LCLObject, Msg);
  end;
end;

{------------------------------------------------------------------------------
  Name: CarbonCommon_Track
  Handles/Creates LM_MOUSEMOVE, LM_MOUSEUP events while dragging
 ------------------------------------------------------------------------------}
function CarbonCommon_Track(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
const
  MSGKIND: array[1..3] of Integer = (LM_LBUTTONUP, LM_RBUTTONUP, LM_MBUTTONUP);
var
  ActionUPP, OldActionUPP: ControlActionUPP;
  P: TPoint;
  Msg: TLMMouse;
  MouseButton: EventMouseButton;
  ControlPart: ControlPartCode;
begin
  {$IFDEF VerboseMouse}
    DebugLn('CarbonCommon_Track ', DbgSName(AWidget.LCLObject));
  {$ENDIF}
  GetEventParameter(AEvent, kEventParamControlAction, typeControlActionUPP, nil, SizeOf(ActionUPP), nil, @OldActionUPP);
  GetEventParameter(AEvent, kEventParamMouseButton, typeMouseButton, nil, SizeOf(EventMouseButton), nil, @MouseButton);

  ActionUPP := NewControlActionUPP(@CarbonCommon_TrackProgress);
  SetEventParameter(AEvent, kEventParamControlAction, typeControlActionUPP,
    SizeOf(ActionUPP), @ActionUPP);

  // this does not return until the mouse is released
  Result := CallNextEventHandler(ANextHandler, AEvent);

  SetEventParameter(AEvent, kEventParamControlAction, typeControlActionUPP,
    SizeOf(OldActionUPP), @OldActionUPP);
  DisposeControlActionUPP(ActionUPP);

  FillChar(Msg, SizeOf(Msg), 0);

  if  (MouseButton >= Low(MSGKIND)) and (MouseButton <= High(MSGKIND)) then
    Msg.Msg := MSGKIND[MouseButton];

  P := AWidget.GetMousePos;
  Msg.XPos := P.X;
  Msg.YPos := P.Y;
  Msg.Keys := GetCarbonMsgKeyState;

  if (AWidget is TCarbonControl) and
    (cceHit in (AWidget as TCarbonControl).GetValidEvents) then
  begin
    GetEventParameter(AEvent, kEventParamControlPart, typeControlPartCode, nil,
      SizeOf(ControlPartCode), nil, @ControlPart);

    {$IFDEF VerboseMouse}
      DebugLn('CarbonCommon_Track Control Part ' + DbgS(ControlPart) +
        ' Button: ' + DbgS(MouseButton));
    {$ENDIF}
    if (ControlPart > 0) and (ControlPart < 128) then
    begin
      // Mouse up will be fired on hit
      SavedMouseUpMsg := Msg;
      Exit;
    end;
  end;

  DeliverMessage(AWidget.LCLObject, Msg);
end;

function CarbonCommon_CursorChange(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  AStatus: OSSTATUS;
  ALocation: FPCMacOSAll.Point;
  AModifiers: UInt32;
  ACursorWasSet: Boolean;
  ACursor: TCursor;
  
  Widget:  TCarbonWidget; //
  Control: ControlRef;    // the control we are dealing with
                          // or the rootcontrol if none found
begin
  Result := CallNextEventHandler(ANextHandler, AEvent);
  
  AStatus := GetEventParameter(AEvent, kEventParamMouseLocation, typeQDPoint, nil,
    SizeOf(ALocation), nil, @ALocation);
  if AStatus = noErr then
  begin
    AStatus := GetEventParameter(AEvent, kEventParamKeyModifiers, typeUInt32, nil,
      SizeOf(AModifiers), nil, @AModifiers);
    if AStatus = noErr then
    begin
      //Find out which control the mouse event should occur for
      Control := nil;
      if HIViewGetViewForMouseEvent(AWidget.Content, AEvent, Control) <> noErr then Exit;
      if Control = nil then Exit;

      Widget := GetCarbonWidget(Control);
      if Widget = nil then Exit;
      
      GlobalToLocal(ALocation);
      AStatus := HandleControlSetCursor(Control, ALocation,
        AModifiers, ACursorWasSet);

      if AStatus <> noErr then
        ACursorWasSet := False;

      if not ACursorWasSet then
      begin
        ACursor := Screen.Cursor;
        if ACursor = crDefault then
          ACursor := Widget.LCLObject.Cursor;
        WidgetSet.SetCursor(Screen.Cursors[ACursor]);
      end;
      Result := noErr;
    end;
  end;
end;

function CarbonCommon_ContextualMenuClick(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Msg: TLMMouse;
  P: TPoint;
begin
  {$IFDEF VerboseCommonEvent}
    DebugLn('CarbonCommon_ContextualMenuClick: ', DbgSName(AWidget.LCLObject));
  {$ENDIF}
  // Result := CallNextEventHandler(ANextHandler, AEvent);

  P := AWidget.GetMousePos;

  FillChar(Msg, SizeOf(TLMMouse), 0);
  Msg.Msg := LM_CONTEXTMENU;
  Msg.Pos.X := P.X;
  Msg.Pos.Y := P.Y;
  
  DeliverMessage(AWidget.LCLObject, Msg);
  Result := noErr; // do not propagate
end;

function CarbonCommon_SetFocusPart(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  FocusPart: ControlPartCode;
begin
  Result := CallNextEventHandler(ANextHandler, AEvent);

  if GetEventParameter(AEvent, kEventParamControlPart, typeControlPartCode, nil,
    SizeOf(ControlPartCode), nil, @FocusPart) <> noErr then Exit;

  {$IFDEF VerboseCommonEvent}
    DebugLn('CarbonCommon_SetFocusPart: ', DbgSName(AWidget.LCLObject), ' ' +
      IntToStr(Integer(FocusPart)));
  {$ENDIF}

  if FocusPart <> kControlFocusNoPart then
    LCLSendSetFocusMsg(AWidget.LCLObject)
  else
    LCLSendKillFocusMsg(AWidget.LCLObject);
end;

function CarbonCommon_GetNextFocusCandidate(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  StartControl, NextControl: ControlRef;
  FocusPart: ControlPartCode;
  TabIndex: Integer;
  TabList: TFPList;
  AControl: TCarbonWidget;
begin
  {$IFDEF VerboseCommonEvent}
    DebugLn('CarbonCommon_GetNextFocusCandidate: ', DbgSName(AWidget.LCLObject));
  {$ENDIF}
  //Result := CallNextEventHandler(ANextHandler, AEvent);
  
  if GetEventParameter(AEvent, kEventParamStartControl, typeControlRef, nil,
    SizeOf(ControlRef), nil, @StartControl) <> noErr then Exit;
  if GetEventParameter(AEvent, kEventParamControlPart, typeControlPartCode, nil,
    SizeOf(ControlPartCode), nil, @FocusPart) <> noErr then Exit;
  
  TabIndex := 0;
  TabList := TFPList.Create;
  try
    (AWidget.LCLObject.GetTopParent as TWinControl).GetTabOrderList(TabList);
    AControl := GetCarbonWidget(StartControl);
    
    if AControl <> nil then
    begin
      TabIndex := TabList.IndexOf(AControl.LCLObject);
      if TabIndex >= 0 then
      begin
        if FocusPart = kControlFocusNextPart then
        begin
          Inc(TabIndex);
          if TabIndex >= TabList.Count then TabIndex := 0;
        end
        else
        begin
          Dec(TabIndex);
          if TabIndex < 0 then TabIndex := TabList.Count - 1;
        end;
      end
      else TabIndex := 0;
    end;
    
    if TabIndex < TabList.Count then
      NextControl := AsControlRef(TWinControl(TabList[TabIndex]).Handle)
    else
      NextControl := nil;
    
    OSError(SetEventParameter(AEvent, kEventParamNextControl, typeControlRef,
        SizeOf(ControlRef), @NextControl), 'CarbonCommon_GetNextFocusCandidate',
        'SetEventParameter');
  finally
    TabList.Free;
  end;
  
  Result := noErr;
end;

function CarbonCommon_SetCursor(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
{
  Msg: TLMessage;
}
  ACursor: TCursor;
begin
  // too much messages in terminal
  // DebugLn('CarbonCommon_SetCursor: ', AWidget.LCLObject.Name);
  CallNextEventHandler(ANextHandler, AEvent);

{
  Paul Ishenin: maybe we should ask control about it cursor via LM_SetCursor ???

  FillChar(Msg, SizeOf(Msg), 0);
  Msg.msg := LM_SETCURSOR;
  DeliverMessage(AWidget.LCLObject, Msg);
}

  ACursor := Screen.Cursor;
  if ACursor = crDefault then
  begin
    ACursor := AWidget.LCLObject.Cursor;
  end;
  WidgetSet.SetCursor(Screen.Cursors[ACursor]);
  Result := noErr; // cursor was setted
end;
