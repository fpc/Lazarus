{%MainUnit carbonint.pas}

{ $Id$ }
{******************************************************************************
  All utility method implementations of the TCarbonWidgetSet class are here.


 ******************************************************************************
 Implementation
 ******************************************************************************

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

{$DEFINE DebugEventLoop}

{ TCarbonWidgetSet }

{
The only drawback to making your own event loop dispatching calls in the main
application thread is that you won't get the standard application event handler
installed. Specifically, the RunApplicationEventLoop function installs handlers
to do the following:
* Allow clicks in the menu bar to begin menu tracking
* Dispatch Apple events by calling AEProcessAppleEvent
* Respond to quit Apple events by quitting RunApplicationEventLoop.

One way to work around this limitation is by creating a dummy custom event
handler. When you are ready to process events, create the dummy event yourself,
post it to the queue and then call RunApplicationEventLoop (to install the
standard application event handler). The dummy event handler can then process
the events manually. For an example of using this method, see Technical
Q&A 1061 in Developer Documentation Technical Q&As.

}

// From: Technical Q&A 1061 in Developer Documentation Technical Q&As
// MWE: modified to fit the LCL, but the basic idea comes from Q&A 1061

function QuitEventHandler(inHandlerCallRef: EventHandlerCallRef;
                          inEvent: EventRef;
                          inUserData: Pointer): OSStatus; stdcall;
  // This event handler is used to override the kEventClassApplication
  // kEventAppQuit event while inside our event loop (EventLoopEventHandler).
  // It simply calls through to the next handler and, if that handler returns
  // noErr (indicating that the application is doing to quit), it sets
  // a Boolean to tell our event loop to quit as well.
  // MWE: in our case, terminates the app also
begin
  Result := CallNextEventHandler(inHandlerCallRef, inEvent);
  if Result <> noErr then Exit;
  if (Widgetset <> nil) and TCarbonWidgetSet(Widgetset).FTerminating then Exit;
  TCarbonWidgetSet(Widgetset).FTerminating := True;
  if Application = nil then Exit;
  Application.Terminate;
end;


function EventLoopEventHandler(inHandlerCallRef: EventHandlerCallRef;
                               inEvent: EventRef;
                               inUserData: Pointer): OSStatus; stdcall;
  // This code contains the standard Carbon event dispatch loop,
  // as per "Inside Macintosh: Handling Carbon Events", Listing 3-10,
  // except:
  //
  // o this loop supports yielding to cooperative threads based on the
  //   application maintaining the gNumberOfRunningThreads global
  //   variable, and
  //
  // o it also works around a problem with the Inside Macintosh code
  //   which unexpectedly quits when run on traditional Mac OS 9.
  //
  // See RunApplicationEventLoopWithCooperativeThreadSupport for
  // an explanation of why this is inside a Carbon event handler.
  //
  // The code in Inside Mac has a problem in that it quits the
  // event loop when ReceiveNextEvent returns an error.  This is
  // wrong because ReceiveNextEvent can return eventLoopQuitErr
  // when you call WakeUpProcess on traditional Mac OS.  So, rather
  // than relying on an error from ReceiveNextEvent, this routine tracks
  // whether the application is really quitting by installing a
  // customer handler for the kEventClassApplication/kEventAppQuit
  // Carbon event.  All the custom handler does is call through
  // to the previous handler and, if it returns noErr (which indicates
  // the application is quitting, it sets quitNow so that our event
  // loop quits.
  //
  // Note that this approach continues to support QuitApplicationEventLoop,
  // which is a simple wrapper that just posts a kEventClassApplication/
  // kEventAppQuit event to the event loop.

var
  QuitUPP: EventHandlerUPP;
  QuitHandler: EventHandlerRef;
  TmpSpec: EventTypeSpec;
  Loop: TApplicationMainLoop = nil;
  Dummy: LongWord;
begin
  // Get our TApplicationMainLoop
  Result := GetEventParameter(inEvent, MakeFourCC('Loop'), MakeFourCC('TAML'),
                              @Dummy, SizeOf(Loop), @Dummy, @Loop);
  if Result <> noErr then Exit;
  if not Assigned(Loop) then Exit;

  // Install our override on the kEventClassApplication, kEventAppQuit event.
  QuitUPP := NewEventHandlerUPP(EventHandlerProcPtr(Pointer(@QuitEventHandler)));
  //todo: raise exception ??
  if QuitUPP = nil then Exit;

  try
    TmpSpec := MakeEventSpec(kEventClassApplication, kEventAppQuit);
    Result := InstallApplicationEventHandler(QuitUPP, 1, @TmpSpec, nil, @QuitHandler);
    if Result <> noErr then Exit;

    try
      // Run our event loop until quitNow is set.
      Loop;
    finally
      FPCMacOSAll.RemoveEventHandler(QuitHandler);
    end;
  finally
    DisposeEventHandlerUPP(QuitUPP);
  end;

(*
  theTarget := GetEventDispatcherTarget;
  repeat
    if MNumberOfRunningThreads = 0
    then timeToWaitForEvent := kEventDurationForever
    else timeToWaitForEvent := kEventDurationNoWait;

    Result := ReceiveNextEvent(0, nil, timeToWaitForEvent, true, theEvent);
    if Result = noErr
    then begin
      SendEventToEventTarget(theEvent, theTarget);
      ReleaseEvent(theEvent);
    end;
    if MNumberOfRunningThreads > 0
    then YieldToAnyThread;
  until quitNow;
*)
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.AppInit
  Params:  ScreenInfo
  Returns: Nothing

  Initialize Carbon Widget Set
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
var
  ScreenDC: HDC;
begin
  WakeMainThread := @OnWakeMainThread;
  
  // fill the screen info
  ScreenDC := GetDC(0);
  try
    ScreenInfo.PixelsPerInchX := GetDeviceCaps(ScreenDC, LOGPIXELSX);
    ScreenInfo.PixelsPerInchY := GetDeviceCaps(ScreenDC, LOGPIXELSY);
    ScreenInfo.ColorDepth := GetDeviceCaps(ScreenDC, BITSPIXEL);
  finally
    ReleaseDC(0, ScreenDC);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.AppRun
  Params:  ALoop
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
  // A reimplementation of RunApplicationEventLoop that supports
  // yielding time to cooperative threads.  It relies on the
  // rest of your application to maintain a global variable,
  // gNumberOfRunningThreads, that reflects the number of threads
  // that are ready to run.
var
  EventLoopUPP: EventHandlerUPP;
  EventLoopHandler: EventHandlerRef;
  DummyEvent: EventRef;
  EventSpec: EventTypeSpec;
  CurMainEventQueue: EventQueueRef;
begin
  DummyEvent := nil;

  // Create a UPP for EventLoopEventHandler and QuitEventHandler

  EventLoopUPP := NewEventHandlerUPP(EventHandlerProcPtr(
                                     Pointer(@EventLoopEventHandler)));
  if EventLoopUPP = nil then
    RaiseGDBException('TCarbonWidgetSet.AppRun no eventhandler');

  try
    // Install EventLoopEventHandler, create a dummy event and post it,
    // and then call RunApplicationEventLoop.  The rationale for this
    // is as follows:  We want to unravel RunApplicationEventLoop so
    // that we can can yield to cooperative threads.  In fact, the
    // core code for RunApplicationEventLoop is pretty easy (you
    // can see it above in EventLoopEventHandler).  However, if you
    // just execute this code you miss out on all the standard event
    // handlers.  These are relatively easy to reproduce (handling
    // the quit event and so on), but doing so is a pain because
    // a) it requires a bunch boilerplate code, and b) if Apple
    // extends the list of standard event handlers, your application
    // wouldn't benefit.  So, we execute our event loop from within
    // a Carbon event handler that we cause to be executed by
    // explicitly posting an event to our event loop.  Thus, the
    // standard event handlers are installed while our event loop runs.

    EventSpec := MakeEventSpec(LCLCarbonEventClass,LCLCarbonEventKindMain);
    if InstallApplicationEventHandler(EventLoopUPP, 1, @EventSpec, nil,
                                      @EventLoopHandler) <> noErr
    then
      RaiseGDBException('TCarbonWidgetSet.AppRun install eventhandler failed');

    try
      if CreateEvent(nil, EventSpec.eventClass, EventSpec.eventKind,
                     GetCurrentEventTime, kEventAttributeNone,
                     DummyEvent) <> noErr
      then
        RaiseGDBException('TCarbonWidgetSet.AppRun create first dummy event failed');

      try
        if SetEventParameter(DummyEvent, MakeFourCC('Loop'),
                             MakeFourCC('TAML'), SizeOf(ALoop),
                             @ALoop) <> noErr
        then
          RaiseGDBException('TCarbonWidgetSet.AppRun setparam to first event failed');

        //DebuglnThrea dLog('TCarbonWidgetSet.AppRun '+dbgs(GetMainEventQueue));
        CurMainEventQueue:=GetMainEventQueue;
        if PostEventToQueue(CurMainEventQueue, DummyEvent,
                            kEventPriorityHigh) <> noErr
        then
          RaiseGDBException('TCarbonWidgetSet.AppRun post dummy event failed');
        fMainEventQueue:=CurMainEventQueue;

        RunApplicationEventLoop;
      finally
        fMainEventQueue:=nil;
        ReleaseEvent(DummyEvent);
      end;
    finally
      FPCMacOSAll.RemoveEventHandler(EventLoopHandler);
    end;
  finally
    DisposeEventHandlerUPP(EventLoopUPP);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.AppProcessMessages
  Params:  None
  Returns: Nothing

  Handle all pending messages
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.AppProcessMessages;
var
  Target: EventTargetRef;
  Event: EventRef;
  CurEventClass: TEventInt;
  CurEventKind: TEventInt;
begin
  Target := GetEventDispatcherTarget;
  CurEventClass.Chars[4] := #0;
  CurEventKind.Chars[4] := #0;
  repeat
    if ReceiveNextEvent(0, nil, kEventDurationNoWait, True, Event) <> noErr then
      Break;
    CurEventClass.Int := GetEventClass(Event);
    CurEventKind.Int := GetEventKind(Event);
    {$IFDEF DebugEventLoop}
    DebugLn('EventClass: "',CurEventClass.Chars,'" EventKind: ',IntToStr(CurEventKind.Int));
    {$ENDIF}
    if CurEventClass.Chars=LCLCarbonEventClass then begin
      // internal carbon intf message
      if (CurEventKind.Chars=LCLCarbonEventKindWake) and IsMultiThread then
      begin
        // a thread is waiting -> synchronize
        CheckSynchronize;
      end;
    end;

    SendEventToEventTarget(Event, Target);
    ReleaseEvent(Event);
  until Application.Terminated;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.AppWaitMessage
  Params:  None
  Returns: Nothing

  Passes execution control to Carbon
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.AppWaitMessage;
var
  Event: EventRef;
begin
  // Simply wait forever for the next event.
  // Don't pull it, so we can handle it later.
  ReceiveNextEvent(0, nil, kEventDurationForever, False, Event);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.Create
  Params:  None
  Returns: Nothing

  Constructor for the class
 ------------------------------------------------------------------------------}
constructor TCarbonWidgetSet.Create;
begin
  CarbonWidgetSet:=Self;
  inherited Create;
  FTerminating := False;
  FTimerMap := TMap.Create(its4, SizeOf(TFNTimerProc));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class
 ------------------------------------------------------------------------------}
destructor TCarbonWidgetSet.Destroy;
begin
  FreeAndNil(FTimerMap);
  
  inherited Destroy;
  CarbonWidgetSet:=nil;
end;

procedure TCarbonWidgetSet.PassCmdLineOptions;
begin
  inherited PassCmdLineOptions;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.SendCheckSynchronizeMessage
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.SendCheckSynchronizeMessage;
var
  EventSpec: EventTypeSpec;
  DummyEvent: EventRef;
begin
  if fMainEventQueue=nil then exit;
  
  //DebuglnThreadLog('TCarbonWidgetSet.SendCheckSynchronizeMessage ');
  EventSpec := MakeEventSpec(LCLCarbonEventClass,LCLCarbonEventKindWake);
  DummyEvent:=nil;
  try
    if CreateEvent(nil, EventSpec.eventClass, EventSpec.eventKind,
                       0{GetCurrentEventTime}, kEventAttributeNone,
                       DummyEvent) <> noErr
    then begin
      //DebuglnThreadLog('TCarbonWidgetSet.SendCheckSynchronizeMessage create event FAILED');
      exit;
    end;

    //DebuglnThreadLog('TCarbonWidgetSet.SendCheckSynchronizeMessage GetMainEventQueue='+dbgs(GetMainEventQueue));
    if PostEventToQueue(fMainEventQueue, DummyEvent,
                        kEventPriorityHigh) <> noErr
    then begin
      //DebuglnThreadLog('TCarbonWidgetSet.SendCheckSynchronizeMessage post event FAILED');
      exit;
    end;
  finally
    if DummyEvent<>nil then
      ReleaseEvent(DummyEvent);
  end;
  //DebuglnThreadLog('TCarbonWidgetSet.SendCheckSynchronizeMessage END');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.OnWakeMainThread
  Params:  Sender
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.OnWakeMainThread(Sender: TObject);
begin
  SendCheckSynchronizeMessage;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.AppTerminate
  Params:  None
  Returns: Nothing

  Tells Carbon to halt the application
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.AppTerminate;
begin
  if FTerminating then Exit;
  QuitApplicationEventLoop;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.AppMinimize
  Params:  None
  Returns: Nothing

  Minimizes the whole application to the taskbar
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.AppMinimize;
begin
  CollapseAllWindows(True);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.AppRestore
  Params:  None
  Returns: Nothing

  Restore the whole minimized application from the taskbar
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.AppRestore;
begin
  CollapseAllWindows(False);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.AppBringToFront
  Params:  None
  Returns: Nothing

  Brings the entire application on top of all other non-topmost programs
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.AppBringToFront;
var
  I: Integer;
begin
  CollapseAllWindows(False);
  
  for I := 0 to Pred(Screen.CustomFormZOrderCount) do
  begin
    BringToFront(WindowRef(Screen.CustomFormsZOrdered[I].Handle));
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.WidgetSetName
  Params:  None
  Returns: Name of Carbon widget set

  Returns name of widget set.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.WidgetSetName: string;
begin
  Result:='carbon';
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.DCGetPixel
  Params:  CanvasHandle - canvas handle to get color from
           X, Y         - position
  Returns: Color at specified point

  Gets the color of the specified pixel on the canvas
  Not implemented!
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer
  ): TGraphicsColor;
begin
  Result := clNone;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.DCSetPixel
  Params:  CanvasHandle - canvas handle to get color from
           X, Y         - position
           AColor       - new color for specified position
  Returns: Nothing

  Sets the color of the specified pixel on the canvas
  Not implemented!
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer;
  AColor: TGraphicsColor);
begin

end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.DCReDraw
  Params:  CanvasHandle - canvas handle to redraw
  Returns: Nothing

  Redraws (the window of) a canvas
  Not implemented!
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.DCRedraw(CanvasHandle: HDC);
begin

end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.SetDesigning
  Params:  AComponent - component to set designing
  Returns: Nothing

  Not implemented!
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.SetDesigning(AComponent: TComponent);
begin

end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.CreateComponent
  Params:  Sender
  Returns: Nothing

  Tells Carbon to create a control
  Deprecated! Implement TCarbonWS*.CreateHandle method instead.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreateComponent(Sender: TObject): THandle;
begin
  DebugLn('WARNING: TCarbonWidgetSet.CreateComponent is deprecated, should not be called!! Go whack somebody with a large throut!');
  Result:=0;
end;

{------------------------------------------------------------------------------
  Method:  TimerCallback
  Params:  inTimer    - timer reference
           inUserData - user data passed when installing timer
  Returns: Nothing

  Calls the timer function associated with specified timer
 ------------------------------------------------------------------------------}
procedure TimerCallback(inTimer: EventLoopTimerRef; inUserData: UnivPtr);
var
  TimerFunc: TFNTimerProc;
begin
  if CarbonWidgetSet = nil then Exit;
  if CarbonWidgetSet.FTimerMap.GetData(inTimer, TimerFunc)
  then TimerFunc;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.CreateTimer
  Params:  Interval  - new timer interval
           TimerFunc - new timer callback
  Returns: A Timer id

  Creates new timer with specified interval and callback function
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreateTimer(Interval: integer; TimerFunc: TFNTimerProc): THandle;
var
  Timer: EventLoopTimerRef;
begin
  Result := 0;
  if (Interval > 0) and (TimerFunc <> nil) then
  begin
    if InstallEventLoopTimer(GetMainEventLoop,
      Interval / 1000, Interval / 1000, // converts msec -> sec
      EventLoopTimerUPP(@TimerCallback), nil, Timer) = noErr then
    begin
      FTimerMap.Add(Timer, TimerFunc);
      Result := THandle(Timer);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.Destroy
  Params:  TimerHandle - Timer id to destroy
  Returns: If the function succeeds

  Destroys specified timer
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.DestroyTimer(TimerHandle: THandle): boolean;
begin
  Result := FTimerMap.Delete(TimerHandle);
  if Result // valid timer
  then RemoveEventLoopTimer(EventLoopTimerRef(TimerHandle));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.IsValidDC
  Params:  DC -  handle to a device context (TCarbonDeviceContext)
  Returns: If the DC is valid
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.IsValidDC(const DC: HDC): Boolean;
var
  AObject: TObject;
begin
  if DC = 0 then
  begin
    Result := False;
    Exit;
  end;
  
  AObject := TObject(DC);
  Result := (AObject is TCarbonDeviceContext);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.IsValidGDIObject
  Params:  GDIObject - handle to a GDI Object (TCarbonFont, ...)
  Returns: If the GDIObject is valid

  Remark: All handles for GDI objects must be pascal objects so we can
 distinguish between them
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.IsValidGDIObject(const GDIObject: HGDIOBJ): Boolean;
var
  AObject: TObject;
begin
  if GDIObject = 0 then
  begin
    Result := False;
    Exit;
  end;

  AObject := TObject(GDIObject);
  Result := (AObject is TCarbonFont);
end;
