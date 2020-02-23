{%MainUnit carbonprivate.pp}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

// ==================================================================
// H A N D L E R S 
// ==================================================================

{------------------------------------------------------------------------------
  Name: CarbonCommon_Dispose
 ------------------------------------------------------------------------------}
function CarbonCommon_Dispose(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  Result := CallNextEventHandler(ANextHandler, AEvent);
  if Assigned(AWidget) and Assigned(AWidget.LCLObject) then
    LCLSendDestroyMsg(AWidget.LCLObject); // widget is disposed in DestroyHandle
end;

{------------------------------------------------------------------------------
  Name: CarbonCommon_Draw
  Handles draw event
 ------------------------------------------------------------------------------}
function CarbonWindow_ContentDraw(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  {%H-}AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  if not isRepaint then
    Result := CallNextEventHandler(ANextHandler, AEvent)
  else
    Result := noErr;
end;


{------------------------------------------------------------------------------
  Name: CarbonCommon_Draw
  Handles draw event
 ------------------------------------------------------------------------------}
function CarbonCommon_Draw(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  AStruct : PPaintStruct;
  Win     : WindowRef;
  Content : HIViewRef;
  ClpShape: HIShapeRef;
  mr      : CGRect;
  invr    : TRect;
begin
  if isRepaint then
  begin
    Result := noErr;
    Exit;
  end;

  {$IFDEF VerbosePaint}
    Debugln('CarbonCommon_Draw ', DbgSName(AWidget.LCLObject));
  {$ENDIF}

  inc(IsDrawEvent);

  AWidget.Context := TCarbonControlContext.Create(AWidget);
  try
    // set canvas context
    if OSError(
      GetEventParameter(AEvent, kEventParamCGContextRef, typeCGContextRef, nil,
        SizeOf(CGContextRef), nil, @(AWidget.Context.CGContext)),
      'CarbonCommon_Draw', SGetEvent, 'kEventParamCGContextRef') then Exit;

    if GetEventParameter(AEvent, kEventParamShape, typeHIShapeRef, nil,
      SizeOf(ClpShape), nil, @ClpShape)=noErr then
    begin
      HIShapeGetBounds(ClpShape, mr);
      invr := CGRectToRect(mr);
      TCarbonControlContext(AWidget.Context).ClipShapeRef := ClpShape;
    end
    else begin
      AWidget.GetBounds(invr);
      TCarbonControlContext(AWidget.Context).ClipShapeRef := nil;
    end;

    AWidget.Context.Reset;

    // let carbon draw/update
    if not AWidget.HasPaint then
      Result := CallNextEventHandler(ANextHandler, AEvent)
    else Result := 0;

    if (AWidget is TCarbonControl) and
      (cceDraw in (AWidget as TCarbonControl).GetValidEvents) then
    begin
      (AWidget as TCarbonControl).Draw;
    end;

    New(AStruct);
    FillChar(AStruct^, SizeOf(TPaintStruct), 0);
    AStruct^.hdc := HDC(AWidget.Context);
    AStruct^.fErase := False;
    AStruct^.rcPaint := invr;
    try
      {$IFDEF VerbosePaint}
        DebugLn('CarbonCommon_Draw LM_PAINT to ', DbgSName(AWidget.LCLObject));
      {$ENDIF}
      LCLSendPaintMsg(AWidget.LCLObject, HDC(AWidget.Context), AStruct);
    finally
      Dispose(AStruct);
    end;
    if AWidget.HasCaret then DrawCaret;
    
    // resetting clip region for the next paint
    TCarbonControlContext(AWidget.Context).SetClipRegion(nil, 0);
    

  finally
    FreeAndNil(AWidget.Context);
    dec(IsDrawEvent);
    if (IsDrawEvent = 0) and InvalidPaint then
    begin
      if not IsRepaint then
      begin
        IsRepaint := true;
        Win:=HIViewGetWindow(AWidget.Widget);
        HIViewFindByID( HIViewGetRoot(Win), kHIViewWindowContentID, Content{%H-});
        //HIViewSetNeedsDisplay(Content, true);
        HIViewRender(HIViewGetRoot(Win));
        IsRepaint:=false;
      end;
      InvalidPaint:=false;
    end;
  end;
  {$IFDEF VerbosePaint}
    Debugln('CarbonCommon_Draw end ', DbgSName(AWidget.LCLObject));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Name: CarbonCommon_BoundsChanged
  Handles bounds changing
 ------------------------------------------------------------------------------}
function CarbonCommon_BoundsChanged(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  {$IFDEF VerboseCommonEvent}
    DebugLn('CarbonCommon_BoundsChanged ', DbgSName(AWidget.LCLObject));
  {$ENDIF}
  // first let carbon draw/update
  Result := CallNextEventHandler(ANextHandler, AEvent);
  
  AWidget.BoundsChanged;
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
    if Widget is TCarbonControl then
      if cceDoAction in (Widget as TCarbonControl).GetValidEvents then
        (Widget as TCarbonControl).DoAction(APartCode);
  
    P := Widget.GetMousePos;
    if (LastMousePos.X = P.X) and (LastMousePos.Y = P.Y) then Exit;
    LastMousePos := P;

    FillChar(Msg{%H-}, SizeOf(TLMMouseMove), 0);
    Msg.Msg := LM_MOUSEMOVE;
    Msg.XPos := P.X;
    Msg.YPos := P.Y;
    Msg.Keys := GetCarbonMsgKeyState;

    if Widget.NeedDeliverMouseEvent(Msg.Msg, Msg) then
    begin
      NotifyApplicationUserInput(Widget.LCLObject, Msg.Msg);
      DeliverMessage(Widget.LCLObject, Msg);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Name: CarbonCommon_Track
  Handles/Creates LM_MOUSEMOVE, LM_MOUSEUP events while dragging
 ------------------------------------------------------------------------------}
function CarbonCommon_Track(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  ActionUPP, OldActionUPP: ControlActionUPP;
  P: TPoint;
  Msg: TLMMouse;
  MouseButton: Integer;
  ControlPart: ControlPartCode;
const
  SName = 'CarbonCommon_Track';
  SControlAction = 'kEventParamControlAction';
begin
  {$IFDEF VerboseMouse}
    DebugLn('CarbonCommon_Track ', DbgSName(AWidget.LCLObject));
  {$ENDIF}
  if OSError(
    GetEventParameter(AEvent, kEventParamControlAction, typeControlActionUPP,
      nil, SizeOf(ActionUPP), nil, @OldActionUPP), SName, SGetEvent,
    SControlAction) then Exit;

  if PostponedDown then
  begin
    PostponedDown := False;
    if AWidget.NeedDeliverMouseEvent(PostponedDownMsg.Msg, PostponedDownMsg) then
    begin
      NotifyApplicationUserInput(AWidget.LCLObject, PostponedDownMsg.Msg);
      DeliverMessage(AWidget.LCLObject, PostponedDownMsg);
    end;
  end;

  MouseButton := GetCarbonMouseButton(AEvent);

  ActionUPP := NewControlActionUPP(@CarbonCommon_TrackProgress);
  try

    if OSError(
      SetEventParameter(AEvent, kEventParamControlAction, typeControlActionUPP,
        SizeOf(ActionUPP), @ActionUPP), SName, SSetEvent, SControlAction) then Exit;

    // this does not return until the mouse is released
    LastMousePos := AWidget.GetMousePos;
    (* this is added in r34345 to fix #19680, but opened #20748, so comment it.
    if DragManager.IsDragging then
      Result := noErr
    else
    *)
    Result := CallNextEventHandler(ANextHandler, AEvent);
    if OSError(
      SetEventParameter(AEvent, kEventParamControlAction, typeControlActionUPP,
        SizeOf(OldActionUPP), @OldActionUPP), SName, SSetEvent, SControlAction) then Exit;
  finally
    DisposeControlActionUPP(ActionUPP);
  end;

  // if button state has not changed, then there's no need to emulate mouse up
  // because the button has not yet been released!
  if MouseButton{%H-}=GetCurrentEventButtonState then Exit;

  FillChar(Msg{%H-}, SizeOf(Msg), 0);

  P := AWidget.GetMousePos;
  Msg.XPos := P.X;
  Msg.YPos := P.Y;
  Msg.Keys := GetCarbonMsgKeyState;
  Msg.Msg := CheckMouseButtonDownUp(TLCLIntfHandle(AWidget), AWidget.LCLObject, LastMouse,
    AWidget.LCLObject.ClientToScreen(P), MouseButton, False);
  case LastMouse.ClickCount of
    2: Msg.Keys := msg.Keys or MK_DOUBLECLICK;
    3: Msg.Keys := msg.Keys or MK_TRIPLECLICK;
    4: Msg.Keys := msg.Keys or MK_QUADCLICK;
  end;

  LastMousePos := P;
  
  if (AWidget is TCarbonControl) and
    (cceHit in (AWidget as TCarbonControl).GetValidEvents) then
  begin
    if OSError(
      GetEventParameter(AEvent, kEventParamControlPart, typeControlPartCode, nil,
        SizeOf(ControlPartCode), nil, @ControlPart), SName, SGetEvent, SControlPart) then Exit;

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

  if AWidget.NeedDeliverMouseEvent(Msg.Msg, Msg) then
  begin
    DeliverMessage(AWidget.LCLObject, Msg);
  
    NotifyApplicationUserInput(AWidget.LCLObject, Msg.Msg);
    CarbonWidgetSet.SetCaptureWidget(0); // capture is released
  end;
end;

{------------------------------------------------------------------------------
  Name: CarbonCommon_CursorChange
  Cursor changing
 ------------------------------------------------------------------------------}
function CarbonCommon_CursorChange(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  ALocation: MacOSAll.Point;
  AModifiers: UInt32;
  ACursorWasSet: Boolean;
  
  Widget:  TCarbonWidget; //
  Control: ControlRef;    // the control we are dealing with
                          // or the rootcontrol if none found
const
  SName = 'CarbonCommon_CursorChange';
begin
  Result := CallNextEventHandler(ANextHandler, AEvent);
  
  if OSError(GetEventParameter(AEvent, kEventParamMouseLocation, typeQDPoint, nil,
      SizeOf(ALocation), nil, @ALocation), SName, SGetEvent,
    'kEventParamMouseLocation') then Exit;

  if OSError(GetEventParameter(AEvent, kEventParamKeyModifiers, typeUInt32, nil,
      SizeOf(AModifiers), nil, @AModifiers), SName, SGetEvent, SKeyModifiers) then Exit;

  //Find out which control the mouse event should occur for
  Control := nil;
  if OSError(HIViewGetViewForMouseEvent(AWidget.Content, AEvent, Control),
    SName, SViewForMouse) then Exit;
  if Control = nil then Exit;

  Widget := GetCarbonWidget(Control);
  if Widget = nil then Exit;
  
  if Screen.RealCursor = crDefault then // we can change cursor
  begin
    ACursorWasSet := False;
    
    // if widget has default cursor set - get it from Carbon
    if (Widget.Cursor<>0) and TCarbonCursor(Widget.Cursor).Default then
    begin
      GlobalToLocal(ALocation);

      if OSError(HandleControlSetCursor(Control, ALocation, EventModifiers(AModifiers), ACursorWasSet),
        SName, 'HandleControlSetCursor') then ACursorWasSet := False;
    end;
    
    if not ACursorWasSet then WidgetSet.SetCursor(Widget.Cursor);
  end;
      
  Result := noErr;
end;

{------------------------------------------------------------------------------
  Name: CarbonCommon_ContextualMenuClick
  PopupMenu auto popup support
 ------------------------------------------------------------------------------}
function CarbonCommon_ContextualMenuClick(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Msg: TLMContextMenu;
  P: MacOSAll.Point;
begin
  {$IFDEF VerboseCommonEvent}
    DebugLn('CarbonCommon_ContextualMenuClick: ', DbgSName(AWidget.LCLObject));
  {$ENDIF}

  GetGlobalMouse(P{%H-});

  FillChar(Msg{%H-}, SizeOf(TLMContextMenu), 0);
  Msg.Msg := LM_CONTEXTMENU;
  Msg.hWnd := HWND(AWidget);
  Msg.Pos.X := P.h;
  Msg.Pos.Y := P.v;
  
  if DeliverMessage(AWidget.LCLObject, Msg) <> 0 then
    Result := noErr // do not propagate
  else
    Result := CallNextEventHandler(ANextHandler, AEvent); 
end;

{------------------------------------------------------------------------------
  Name: CarbonCommon_SetFocusPart
  Handles set or kill focus
 ------------------------------------------------------------------------------}
function CarbonCommon_SetFocusPart(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  FocusPart: ControlPartCode;
const
  SName = 'CarbonCommon_SetFocusPart';
begin
  if not Assigned(AWidget) then
  begin
    Result := CallNextEventHandler(ANextHandler, AEvent);
    Exit;
  end;

  AWidget.BeginEventProc;
  try
    if not (AWidget is TCarbonCustomControl) or (AWidget is TCarbonWindow) then
      Result := CallNextEventHandler(ANextHandler, AEvent)
    else
      Result := noErr;

    if OSError(
      GetEventParameter(AEvent, kEventParamControlPart, typeControlPartCode, nil,
        SizeOf(ControlPartCode), nil, @FocusPart), SName,
      SGetEvent, SControlPart) then Exit;

    if FocusPart <> kControlFocusNoPart then
    begin
      CarbonWidgetSet.SetFocusedWidget(HWND(AWidget));
      AWidget.FocusSet;
      CarbonWidgetSet.SetFocusedWidget(0);
    end
    else
    begin
      if CarbonWidgetSet.GetFocusedWidget = HWND(AWidget) then
        CarbonWidgetSet.SetFocusedWidget(0);
      AWidget.FocusKilled;
    end;
  finally
    AWidget.EndEventProc;
  end;
end;

{------------------------------------------------------------------------------
  Name: CarbonCommon_GetNextFocusCandidate
  TabOrder and TabStop support
 ------------------------------------------------------------------------------}
function CarbonCommon_GetNextFocusCandidate(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  StartControl, NextControl: ControlRef;
  FocusPart: ControlPartCode;
const
  SName = 'CarbonCommon_GetNextFocusCandidate';
begin
  {$IFDEF VerboseCommonEvent}
    DebugLn('CarbonCommon_GetNextFocusCandidate: ', DbgSName(AWidget.LCLObject));
  {$ENDIF}
  Result:=CallNextEventHandler(ANextHandler, AEvent);
  Exit;

  StartControl := nil;
  if OSError(GetEventParameter(AEvent, kEventParamStartControl, typeControlRef,
      nil, SizeOf(ControlRef), nil, @StartControl), SName, SGetEvent,
    'kEventParamStartControl', eventParameterNotFoundErr) then Exit;
    
  if OSError(
    GetEventParameter(AEvent, kEventParamControlPart, typeControlPartCode, nil,
      SizeOf(ControlPartCode), nil, @FocusPart), SName, SGetEvent, SControlPart) then Exit;
  
  NextControl := AWidget.GetNextFocus(GetCarbonWidget(StartControl), FocusPart = kControlFocusNextPart);
    
  if NextControl = nil then
    Result := CallNextEventHandler(ANextHandler, AEvent)
  else
  begin
    OSError(SetEventParameter(AEvent, kEventParamNextControl, typeControlRef,
      SizeOf(ControlRef), @NextControl), SName, SSetEvent, 'kEventParamNextControl');
  
    Result := noErr;
  end;
end;

{------------------------------------------------------------------------------
  Name: CarbonCommon_SetCursor
  Sets cursor
 ------------------------------------------------------------------------------}
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

  ACursor := Screen.RealCursor;
  if ACursor = crDefault then
  begin
    ACursor := AWidget.LCLObject.Cursor;
  end;
  WidgetSet.SetCursor(Screen.Cursors[ACursor]);
  
  Result := noErr; // cursor was setted
end;

function CarbonCommon_User({%H-}ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  AMessage: TLMessage;
begin
  Result := GetEventParameter(AEvent, MakeFourCC('wmsg'), MakeFourCC('wmsg'), nil,
     SizeOf(TLMessage), nil, @AMessage);
  if Result = noErr then
  begin
    AMessage.Result := DeliverMessage(AWidget.LCLObject, AMessage);
    SetEventParameter(AEvent, MakeFourCC('wmsg'), MakeFourCC('wmsg'),
      SizeOf(TLMessage), @AMessage);
  end;
end;

