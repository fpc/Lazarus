{%MainUnit carbonprivate.pp}
{ $Id: $}
{
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

// ==================================================================
// H A N D L E R S 
// ==================================================================

function CarbonPrivateWindow_Close(ANextHandler: EventHandlerCallRef;
                                   AEvent: EventRef;
                                   AInfo: PWidgetInfo): OSStatus; mwpascal;
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

function CarbonPrivateWindow_Closed(ANextHandler: EventHandlerCallRef;
                                    AEvent: EventRef;
                                    AInfo: PWidgetInfo): OSStatus; mwpascal;
var
  Msg: TLMessage;
  PrivateClass: TCarbonPrivateWindowClass;
begin
  Result := CallNextEventHandler(ANextHandler, AEvent);
  PrivateClass := TCarbonPrivateWindowClass(AInfo^.WSClass.WSPrivate);

  FillChar(Msg, SizeOf(Msg),0);
  Msg.msg := LM_DESTROY;
  DeliverMessage(AInfo^.LCLObject, Msg);
  
  PrivateClass.UnregisterEvents;
  FreeWidgetInfo(AInfo);
end;

//Generic function that handles all types of mouse events

function CarbonPrivateWindow_MouseProc(ANextHandler: EventHandlerCallRef;
                                AEvent: EventRef;
                                AWIndowInfo: PWidgetInfo): OSStatus; mwpascal;
var
  Control: ControlRef;  // the control we are dealing with
                        // or the rootcontrol if none found          
                                
//
// helper functions used commonly
//
  function GetClickCount: Integer;
  var
    ClickCount: UInt32;
  begin
    GetEventParameter(AEvent, kEventParamClickCount, typeUInt32, nil, SizeOf(ClickCount), nil, @ClickCount);
    Result := ClickCount;
  end;

  function GetMouseButton:Integer;
  var
    MouseButton: EventMouseButton;
  begin
    GetEventParameter(AEvent, kEventParamMouseButton, typeMouseButton, nil, SizeOf(MouseButton), nil, @MouseButton);
    Result := MouseButton;
  end;

  function GetMousePoint: TPoint;
  var
    MousePoint: HIPoint;//QDPoint;
  begin
    GetEventParameter(AEvent, kEventParamWindowMouseLocation, typeHIPoint, nil, SizeOf(MousePoint), nil, @MousePoint);
    HIViewConvertPoint(MousePoint, nil, Control);
    Result.X := Round(MousePoint.X);
    Result.Y := Round(MousePoint.Y);
    // WriteLn('Mouse to Widget Coords: X=',Result.X,' Y=',Result.Y);
  end;
//
// handler functions
//
  procedure HandleMouseDownEvent(var AMsg);
  const
    MSGKIND: array[1..4, 1..3] of Integer = (
      (LM_LBUTTONDOWN, LM_RBUTTONDOWN, LM_MBUTTONDOWN),
      (LM_LBUTTONDBLCLK, LM_RBUTTONDBLCLK, LM_MBUTTONDBLCLK),
      (LM_LBUTTONTRIPLECLK, LM_RBUTTONTRIPLECLK, LM_MBUTTONTRIPLECLK),
      (LM_LBUTTONQUADCLK, LM_RBUTTONQUADCLK, LM_MBUTTONQUADCLK)
    );
  var
    MouseButton: Integer;
    ClickCount: Integer;
    MousePoint: TPoint;
    Msg: ^TLMMouse;
  begin
    Msg := @AMsg;

    ClickCount := GetClickCount;
    MouseButton := GetMouseButton;
    MousePoint := GetMousePoint;
    
    if (ClickCount < Low(MSGKIND))
    or (ClickCount > Low(MSGKIND))
    then ClickCount := 1;
    
    if  (MouseButton >= Low(MSGKIND))
    and (MouseButton <= High(MSGKIND))
    then Msg^.Msg := MSGKIND[ClickCount, MouseButton];

    Msg^.XPos := MousePoint.X;
    Msg^.YPos := MousePoint.Y;

    //LMMouse.Keys;
    {$Warning CarbonPrivateWindow_MouseProc LMMouse.Keys TODO}
  end;

  procedure HandleMouseUpEvent(var AMsg);
  const 
    MSGKIND: array[1..3] of Integer = (LM_LBUTTONUP, LM_RBUTTONUP, LM_MBUTTONUP);
  var
    MouseButton: Integer;
    MousePoint: TPoint;
    Msg: ^TLMMouse;
  begin
    // this is not called if NextHandler is called on MouseDown
    // perhaps mousetracking can fix this
    Msg := @AMsg;

    MouseButton := GetMouseButton;
    MousePoint := GetMousePoint;
    
    if  (MouseButton >= Low(MSGKIND))
    and (MouseButton <= High(MSGKIND))
    then Msg^.Msg := MSGKIND[MouseButton];

    Msg^.XPos := MousePoint.X;
    Msg^.YPos := MousePoint.Y;

    //LMMouse.Keys;
  end;

  procedure HandleMouseMovedEvent(var AMsg);
  var
    MousePoint: TPoint;
    MSg: ^TLMMouseMove;
  begin
    Msg := @AMsg;
    
    MousePoint := GetMousePoint;
    
    Msg^.Msg := LM_MOUSEMOVE;
    Msg^.XPos := MousePoint.X;
    Msg^.YPos := MousePoint.Y;
  end;

  procedure HandleMouseDraggedEvent(var AMsg);
  begin
    //TODO
  end;

  procedure HandleMouseWheelEvent(var AMsg);
  begin
    //TODO should be simple
  end;
  
var
  Msg: record
    Message: TLMessage;
    Extra: array[0..20] of Byte; // some messages are a bit larger, make some room
  end;  
  EventKind: UInt32;
  Root: ControlRef;
  Info: PWidgetInfo;
begin
  FillChar(Msg, SizeOf(Msg), 0);

  //Find out which control the mouse event should occur for
  GetRootControl(AWindowInfo^.Widget, Root);
  Control := nil;
  HIViewGetViewForMouseEvent(Root, AEvent, Control);
  if Control = nil 
  then Control := Root;
  
  // if a control other than root is found, send the message 
  // to the control instead of the window 
  // if a lower control without widgetInfo is found, use its parent
  // Note: HIViewGetViewForMouseEvent returns the root if noting found
  Info := nil;
  while Control <> Root do
  begin
    Info := GetWidgetInfo(Pointer(Control));
    if Info <> nil then Break;
    Control := HIViewGetSuperview(Control);
  end;
  if (Info = nil) or (Control = Root)
  then Info := AWindowInfo;
  
  EventKind := GetEventKind(AEvent);
  case EventKind of
    kEventMouseDown       : HandleMouseDownEvent(Msg);
    kEventMouseUp         : HandleMouseUpEvent(Msg);
    kEventMouseMoved      : HandleMouseMovedEvent(Msg);
    kEventMouseDragged    : HandleMouseDraggedEvent(Msg);

    //For the enter and exit events tracking must be enabled
    //tracking is enabled by defining a rect that you want to track
    // TODO: Tracking
    kEventMouseEntered    : Msg.Message.Msg := CM_MOUSEENTER;
    kEventMouseExited     : Msg.Message.Msg := CM_MOUSELEAVE;

    kEventMouseWheelMoved : HandleMouseWheelEvent(Msg);
  end;
  
  if DeliverMessage(Info^.LCLObject, Msg) = 0 then begin
    Result := CallNextEventHandler(ANextHandler, AEvent);
  end
  else begin
    // the LCL does not want the event propagated
    Result := noErr;
  end;
end;

// ==================================================================
// C L A S S 
// ==================================================================

  { TCarbonPrivateWindow }

procedure TCarbonPrivateWindow.RegisterEvents(AInfo: PWidgetInfo);
var
  MouseSpec: array [0..6] of EventTypeSpec;
  TmpSpec: EventTypeSpec;
begin
  TmpSpec := MakeEventSpec(kEventClassWindow, kEventWindowClose);
  InstallWindowEventHandler(AInfo^.Widget, RegisterEventHandler(@CarbonPrivateWindow_Close), 
    1, @TmpSpec, Pointer(AInfo), nil);

  TmpSpec := MakeEventSpec(kEventClassWindow, kEventWindowClosed);
  InstallWindowEventHandler(AInfo^.Widget, RegisterEventHandler(@CarbonPrivateWindow_Closed), 
    1, @TmpSpec, Pointer(AInfo), nil);

  MouseSpec[0].eventClass := kEventClassMouse;
  MouseSpec[0].eventKind := kEventMouseDown;
  MouseSpec[1].eventClass := kEventClassMouse;
  MouseSpec[1].eventKind := kEventMouseUp;
  MouseSpec[2].eventClass := kEventClassMouse;
  MouseSpec[2].eventKind := kEventMouseMoved;
  MouseSpec[3].eventClass := kEventClassMouse;
  MouseSpec[3].eventKind := kEventMouseDragged;
  MouseSpec[4].eventClass := kEventClassMouse;
  MouseSpec[4].eventKind := kEventMouseEntered;
  MouseSpec[5].eventClass := kEventClassMouse;
  MouseSpec[5].eventKind := kEventMouseExited;
  MouseSpec[6].eventClass := kEventClassMouse;
  MouseSpec[6].eventKind := kEventMouseWheelMoved;

  InstallWindowEventHandler(AInfo^.Widget, RegisterEventHandler(@CarbonPrivateWindow_MouseProc),
    7, @MouseSpec[0], Pointer(AInfo), nil);

end;

procedure TCarbonPrivateWindow.UnregisterEvents;
begin                      
  UnregisterEventHandler(@CarbonPrivateWindow_Close);
  UnregisterEventHandler(@CarbonPrivateWindow_Closed); 
  UnregisterEventHandler(@CarbonPrivateWindow_MouseProc);
end;

