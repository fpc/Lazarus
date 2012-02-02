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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{ TCarbonWidgetSet }

{
  This event handler will fix the focus indication in AXApplication for
  standard controls where it gets it wrong. Necessary to support accessibility
  for TMemo / TEdit for example
}
function AppAccessibilityEventHandler(inHandlerCallRef: EventHandlerCallRef;
                          inEvent: EventRef;
                          inUserData: Pointer): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  lAXRole, lInputStr: CFStringRef;
  lInputAXObject: AXUIElementRef;
  EventKind: UInt32;
  lInputPasStr: string;
  lElement, lElement2: AXUIElementRef;
  lAXArray: CFMutableArrayRef;
begin
  Result := CallNextEventHandler(inHandlerCallRef, inEvent);

  GetEventParameter(inEvent, kEventParamAccessibleObject,
    typeCFTypeRef, nil, SizeOf(AXUIElementRef), nil, @lInputAXObject);

  EventKind := GetEventKind(inEvent);
  case EventKind of
    kEventAccessibleGetNamedAttribute:
    begin
      GetEventParameter(inEvent, kEventParamAccessibleAttributeName,
        typeCFStringRef, nil, SizeOf(CFStringRef), nil, @lInputStr);

      lInputPasStr := CFStringToStr(lInputStr);

      if lInputPasStr = 'AXFocusedUIElement' then
      begin
        // First interfere only if the element returned is in our black list
        // for example: memo border
        GetEventParameter(inEvent, kEventParamAccessibleAttributeValue,
          typeCFTypeRef, nil, SizeOf(AXUIElementRef), nil, @lElement);

        AXUIElementCopyAttributeValue(lElement, CFSTR('AXRoleDescription'), lAXRole);
        lInputPasStr := CFStringToStr(lAXRole);
        if lInputPasStr = 'memoborder' then
        begin
          AXUIElementCopyAttributeValue(lElement, CFSTR('AXChildren'), lAXArray);
          lElement2 := CFArrayGetValueAtIndex(lAXArray, 0);
          SetEventParameter(inEvent, kEventParamAccessibleAttributeValue, typeCFTypeRef,
            SizeOf(AXUIElementRef), @lElement2);

          Result := noErr;
          Exit;
        end;
      end;
    end; // kEventAccessibleGetNamedAttribute
  end; // case EventKind of
end;

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
                          inUserData: Pointer): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
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
                               inUserData: Pointer): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
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
begin
  // Get our TApplicationMainLoop
  Result := noErr;
  if (not Assigned(inUserData)) or TCarbonWidgetSet(inUserData).FUserTerm then Exit;
  Loop := TCarbonWidgetSet(inUserData).FAppLoop;
  if not Assigned(Loop) then Exit;

  // Install our override on the kEventClassApplication, kEventAppQuit event.
  QuitUPP := NewEventHandlerUPP(EventHandlerProcPtr(Pointer(@QuitEventHandler)));
  //todo: raise exception ??
  if QuitUPP = nil then Exit;

  try
    TmpSpec := MakeEventSpec(kEventClassApplication, kEventAppQuit);
    if not InstallApplicationEventHandler(QuitUPP, 1, @TmpSpec, nil, @QuitHandler) then Exit;
    try
      // Run our event loop until quitNow is set.
      Loop;
    finally
      MacOSAll.RemoveEventHandler(QuitHandler);
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
  Name:  CarbonApp_CommandProcess
  Handles main menu and context menus commands
 ------------------------------------------------------------------------------}
function CarbonApp_CommandProcess(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Command: HICommandExtended;
  CarbonMenu: TCarbonMenu;
  Msg: TLMessage;
  S: LongWord;
  AllowMenu: Boolean;
  Focused: HWND;
  HotChar: Char;
const SName = 'CarbonApp_CommandProcess';
begin
  {$IFDEF VerboseAppEvent}
    DebugLn('CarbonApp_CommandProcess');
  {$ENDIF}
  
  if not OSError(
    GetEventParameter(AEvent, kEventParamDirectObject,
      typeHICommand, nil, SizeOf(HICommand), nil, @Command),
    SName, 'GetEventParameter') then
  begin
    {$IFDEF VerboseMenu}
       DebugLn('CarbonApp_CommandProcess MenuRef: ' + DbgS(Command.menuRef) +
         ' Item: ' + DbgS(Command.menuItemIndex) + ' CommandID: ' + DbgS(Command.commandID) +
         ' Attrs: ' + DbgS(Command.attributes));
    {$ENDIF}
      
    // check command and send "click" message to menu item
    if (Command.commandID = MENU_FOURCC) and
      (Command.attributes and kHICommandFromMenu > 0) and
      (Command.menuRef <> nil) then
    begin
      if not OSError(GetMenuItemProperty(Command.menuRef, Command.menuItemIndex,
          LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(TCarbonMenu), S, @CarbonMenu),
        SName, 'GetMenuItemProperty') then
      begin
        {$IFDEF VerboseMenu}
           DebugLn('CarbonApp_CommandProcess CarbonMenu: ' + DbgS(CarbonMenu));
        {$ENDIF}
        if CarbonMenu <> nil then
        begin
          Hotchar:=CarbonMenu.GetShortCutKey;
          { CommandProcess is fired before a keyboard event                              }
          { we must check if the control has default system handlers on the hot-key used }
          { if so, CommandProcess is not processed, and the key values event are sent    }
          { to the control by the system.                                                }
          {                                                                              }
          { Another possible solution of the problem, is to Post another custom event    }
          { to the loop, and report LCL about Menu pressed after the event arrives,      }
          { though it might seem, like interface is lagging }
          if (CarbonMenu.Parent.Dismissed<>kHIMenuDismissedBySelection) and (HotChar<>#0) then
          begin
            AllowMenu := True;
            Focused:=GetFocus;
            if (Focused<>0) and (TObject(Focused) is TCarbonControl) then
            begin
              TCarbonControl(Focused).AllowMenuProcess(HotChar, GetCarbonShiftState, AllowMenu);
              if not AllowMenu then
              begin
                Result:=eventNotHandledErr;
                CarbonMenu.Parent.Dismissed:=0;
                Exit;
              end;
            end;
          end;

          if CarbonMenu.Parent.Dismissed=kHIMenuDismissedBySelection then begin
            FillChar(Msg, SizeOf(Msg), 0);
            Msg.msg := LM_ACTIVATE;
            CarbonMenu.LCLMenuItem.Dispatch(Msg);
            if assigned(CarbonMenu.Parent) then  // if parent not closed
              CarbonMenu.Parent.Dismissed:=0;
            Result := noErr;
            Exit;
          end else
            Result:=CallNextEventHandler(ANextHandler, AEvent);

        end;
      end;
    end;
  end;
  
  Result := CallNextEventHandler(ANextHandler, AEvent);
end;

{------------------------------------------------------------------------------
  Name:  CarbonApp_Shown
  Handles application show
 ------------------------------------------------------------------------------}
function CarbonApp_Shown(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  {$IFDEF VerboseAppEvent}
    DebugLn('CarbonApp_Shown');
  {$ENDIF}
  
  Result := CallNextEventHandler(ANextHandler, AEvent);
  
  Application.IntfAppRestore;
end;

{------------------------------------------------------------------------------
  Name:  CarbonApp_Hidden
  Handles application hide
 ------------------------------------------------------------------------------}
function CarbonApp_Hidden(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  {$IFDEF VerboseAppEvent}
    DebugLn('CarbonApp_Hidden');
  {$ENDIF}

  Result := CallNextEventHandler(ANextHandler, AEvent);

  Application.IntfAppMinimize;
end;

{------------------------------------------------------------------------------
  Name:  CarbonApp_Deactivated
  Handles application deactivation
 ------------------------------------------------------------------------------}
function CarbonApp_Deactivated(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  {$IFDEF VerboseAppEvent}
    DebugLn('CarbonApp_Deactivate');
  {$ENDIF}

  Result := CallNextEventHandler(ANextHandler, AEvent);

  Application.IntfAppDeactivate;
end;

{------------------------------------------------------------------------------
  Name:  CarbonApp_Activated
  Handles application activation
 ------------------------------------------------------------------------------}
function CarbonApp_Activated(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  {$IFDEF VerboseAppEvent}
    DebugLn('CarbonApp_Activate');
  {$ENDIF}

  Result := CallNextEventHandler(ANextHandler, AEvent);

  Application.IntfAppActivate;
end;



{------------------------------------------------------------------------------
  Name:  CarbonApp_Open
  Handles application open
 ------------------------------------------------------------------------------}
function CarbonApp_Open(var AEvent: AppleEvent; var Reply: AppleEvent;
  Data: SInt32): OSErr; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  DocList: AEDescList;
  FileCount: Integer;
  FileIdx: Integer;
  Keyword: AEKeyword;
  FileDesc: AEDesc;
  FileRef: FSRef;
  FileURL: CFURLRef;
  FileCFStr: CFStringRef;
  Files: Array of String;
const
  SName = 'OpenDocEventHandler';
begin
  {$IFDEF VerboseAppEvent}
    DebugLn('CarbonApp_Open');
  {$ENDIF}
  
  if OSError(AEGetParamDesc(AEvent, keyDirectObject, typeAEList, DocList),
    SName, 'AEGetParamDesc') then Exit;

  try
    if OSError(AECountItems(DocList, FileCount), SName, 'AECountItems') then Exit;


    SetLength(Files, 0);
    
    for FileIdx := 1 to FileCount do
    begin
      if OSError(AEGetNthDesc(DocList, FileIdx, typeFSRef, @Keyword, FileDesc),
        SName, 'AEGetNthDesc') then Continue;

      if OSError(AEGetDescData(FileDesc, @FileRef, SizeOf(FSRef)),
        SName, 'AEGetDescData') then Continue;

      if OSError(AEDisposeDesc(FileDesc),
        SName, 'AEDisposeDesc') then Continue;

      FileURL := CFURLCreateFromFSRef(kCFAllocatorDefault, FileRef);
      FileCFStr := CFURLCopyFileSystemPath(FileURL, kCFURLPOSIXPathStyle);
      try
        SetLength(Files, Length(Files) + 1);
        Files[High(Files)] := CFStringToStr(FileCFStr);
      finally
        FreeCFString(FileURL);
        FreeCFString(FileCFStr);
      end;
    end;
    
    if Length(Files) > 0 then
    begin
      if Application <> nil then
      begin
        if Application.MainForm <> nil then
          Application.MainForm.IntfDropFiles(Files);

        Application.IntfDropFiles(Files);
      end;
    end;
  finally
    AEDisposeDesc(DocList);
  end;

  Result := noErr;
end;

{------------------------------------------------------------------------------
  Name:  CarbonApp_Quit
  Handles application quit
 ------------------------------------------------------------------------------}
function CarbonApp_Quit(var AEvent: AppleEvent; var Reply: AppleEvent;
  Data: SInt32): OSErr; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  {$IFDEF VerboseAppEvent}
    DebugLn('CarbonApp_Quit');
  {$ENDIF}

  if (Application <> nil) and (Application.MainForm <> nil) then
  begin
    Application.MainForm.Close;
  end;
  
  Result := noErr;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.AppInit
  Params:  ScreenInfo

  Initialize Carbon Widget Set
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
var
  ScreenDC: HDC;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCarbonWidgetSet.AppInit');
  {$ENDIF}
  
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

  fMainEventQueue:=GetMainEventQueue;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.AppRun
  Params:  ALoop
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
  // A reimplementation of RunApplicationEventLoop that supports
  // yielding time to cooperative threads.  It relies on the
  // rest of your application to maintain a global variable,
  // gNumberOfRunningThreads, that reflects the number of threads
  // that are ready to run.
var
  DummyEvent: EventRef;
  EventSpec: EventTypeSpec;
  EventLoopUPP, AccessibilityUPP: EventHandlerUPP;
  EventLoopHandler, AccessibilityHandle: EventHandlerRef;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCarbonWidgetSet.AppRun');
  {$ENDIF}
  FAppLoop:=ALoop;
  DummyEvent := nil;

  // Accessibility for AXApplication
  AccessibilityUPP := NewEventHandlerUPP(EventHandlerProcPtr(Pointer(@AppAccessibilityEventHandler)));
  EventSpec := MakeEventSpec(kEventClassAccessibility, kEventAccessibleGetNamedAttribute);
  InstallApplicationEventHandler(AccessibilityUPP, 1, @EventSpec, Self, @AccessibilityHandle);

  // Create a UPP for EventLoopEventHandler and QuitEventHandler

  EventLoopUPP := NewEventHandlerUPP(EventHandlerProcPtr(
                                     Pointer(@EventLoopEventHandler)));
  if EventLoopUPP = nil then
    RaiseGDBException('TCarbonWidgetSet.InitMainLoop no eventhandler');

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

  EventSpec := MakeEventSpec(LCLCarbonEventClass, LCLCarbonEventKindMain);
  if not InstallApplicationEventHandler(EventLoopUPP, 1, @EventSpec, Self,
                                    @EventLoopHandler) then Exit;
  try
    if CreateEvent(nil, EventSpec.eventClass, EventSpec.eventKind, 0,
      kEventAttributeNone, DummyEvent) <> noErr
    then
      RaiseGDBException('TCarbonWidgetSet.InitMainLoop create first dummy event failed');

    try
      {if SetEventParameter(DummyEvent, MakeFourCC('Loop'),
                           MakeFourCC('TAML'), SizeOf(ALoop),
                           @ALoop) <> noErr
      then
        RaiseGDBException('TCarbonWidgetSet.InitMainLoop setparam to first event failed');}

            //DebuglnThrea dLog('TCarbonWidgetSet.AppRun '+dbgs(GetMainEventQueue));
      if PostEventToQueue(FMainEventQueue, DummyEvent,
                          kEventPriorityHigh) <> noErr
      then
        RaiseGDBException('TCarbonWidgetSet.AppRun post dummy event failed');
    finally
      ReleaseEvent(DummyEvent);
    end;

    SignalFirstAppEvent;
    if not FUserTerm then
    begin
      RunApplicationEventLoop;
    end;
    FAppStdEvents:=True;

  finally
    MacOSAll.RemoveEventHandler(EventLoopHandler);
    DisposeEventHandlerUPP(EventLoopUPP);
  end;

  {$IFDEF VerboseObject}
    DebugLn('TCarbonWidgetSet.AppRun END');
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.AppProcessMessages

  Handle all pending messages
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.AppProcessMessages;
var
  Target: EventTargetRef;
  Event: EventRef;
  CurEventClass: TEventInt;
  CurEventKind: TEventInt;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCarbonWidgetSet.AppProcessMessages');
  {$ENDIF}

  if not FAppStdEvents then InstallStandardEventHandler(GetApplicationEventTarget);

  Target := GetEventDispatcherTarget;
  CurEventClass.Chars[4] := #0;
  CurEventKind.Chars[4] := #0;
  repeat
    FreePendingWidgets;
    if ReceiveNextEvent(0, nil, kEventDurationNoWait, True,
      Event) <> noErr then Break;
      
    CurEventClass.Int := GetEventClass(Event);
    CurEventKind.Int := GetEventKind(Event);

    {$IFDEF DebugEventLoop}
    DebugLn('EventClass: "',CurEventClass.Chars,'" EventKind: ',IntToStr(CurEventKind.Int));
    {$ENDIF}

    if CurEventClass.Chars = LCLCarbonEventClass then
    begin
      // internal carbon intf message
      if (CurEventKind.Chars = LCLCarbonEventKindWake) and IsMultiThread then
      begin
        // a thread is waiting -> synchronize
        CheckSynchronize;
      end;
    end;

    SendEventToEventTarget(Event, Target);
    ReleaseEvent(Event);
    
    if Clipboard <> nil then
      if Clipboard.OwnerShips > 0 then Clipboard.CheckOwnerShip;

  until Application.Terminated;
  
  {$IFDEF VerboseObject}
    DebugLn('TCarbonWidgetSet.AppProcessMessages END');
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.AppWaitMessage

  Passes execution control to Carbon
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.AppWaitMessage;
var
  Event: EventRef;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCarbonWidgetSet.AppWaitMessage');
  {$ENDIF}
  
  // Simply wait forever for the next event.
  // Don't pull it, so we can handle it later.
  OSError(ReceiveNextEvent(0, nil, kEventDurationForever, False, Event),
    Self, 'AppWaitMessage', 'ReceiveNextEvent');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.Create

  Constructor for the class
 ------------------------------------------------------------------------------}
constructor TCarbonWidgetSet.Create;
begin
  CarbonWidgetSet := Self;
  inherited Create;
  FTerminating := False;
  fMenuEnabled := True;

  FTimerMap := TMap.Create(its4, SizeOf(TWSTimerProc));
  FCurrentCursor := 0;
  FMainMenu := 0;
  FCaptureWidget := 0;
  
  RegisterEvents;

  { if using Cocoa, we need an autorelease pool
    and we also need to initialize NSApplication }
  {$ifdef CarbonUseCocoa}
    pool := NSAutoreleasePool.Create;
    
    NSApplicationLoad();
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.Destroy

  Destructor for the class
 ------------------------------------------------------------------------------}
destructor TCarbonWidgetSet.Destroy;
begin
  CaretWidgetSetReleased;

  FreeAndNil(FTimerMap);
  DisposeAEEventHandlerUPP(FOpenEventHandlerUPP);
  DisposeAEEventHandlerUPP(FQuitEventHandlerUPP);

  inherited Destroy;
  CarbonWidgetSet := nil;

  // if using Cocoa, release autorelease the pool
  {$ifdef CarbonUseCocoa}
    if pool <> nil then pool.Free;
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.RawImage_DescriptionFromCarbonBitmap

  Creates a rawimage description for a carbonbitmap
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.RawImage_DescriptionFromCarbonBitmap(out ADesc: TRawImageDescription; ABitmap: TCarbonBitmap): Boolean;
var
  Prec, Shift, BPR: Byte;
  AlphaInfo: CGImageAlphaInfo;
begin
  ADesc.Init;

  case ABitmap.BitmapType of
    cbtMono, cbtGray: ADesc.Format := ricfGray;
  else
    ADesc.Format := ricfRGBA;
  end;

  ADesc.Width := CGImageGetWidth(ABitmap.CGImage);
  ADesc.Height := CGImageGetHeight(ABitmap.CGImage);

  //ADesc.PaletteColorCount := 0;

  ADesc.BitOrder := riboReversedBits;
  ADesc.ByteOrder := riboMSBFirst;

  BPR := CGImageGetBytesPerRow(ABitmap.CGImage) and $FF;
  if BPR and $F = 0 then ADesc.LineEnd := rileDQWordBoundary     // 128bit aligned
  else if BPR and $7 = 0 then ADesc.LineEnd := rileQWordBoundary //  64bit aligned
  else if BPR and $3 = 0 then ADesc.LineEnd := rileWordBoundary  //  32bit aligned
  else if BPR and $1 = 0 then ADesc.LineEnd := rileByteBoundary  //   8bit aligned
  else ADesc.LineEnd := rileTight;

  ADesc.LineOrder := riloTopToBottom;
  ADesc.BitsPerPixel := CGImageGetBitsPerPixel(ABitmap.CGImage);

  ADesc.MaskBitOrder := riboReversedBits;
  ADesc.MaskBitsPerPixel := 1;
  ADesc.MaskLineEnd := rileByteBoundary;
  // ADesc.MaskShift := 0;

  Prec := CGImageGetBitsPerComponent(ABitmap.CGImage) and $FF;
  AlphaInfo := CGImageGetAlphaInfo(ABitmap.CGImage);

  if AlphaInfo <> kCGImageAlphaOnly
  then begin
    ADesc.RedPrec := Prec;
    ADesc.GreenPrec := Prec;
    ADesc.BluePrec := Prec;
  end;

  // gray or mono
  if ADesc.Format = ricfGray then Exit;

  // alpha
  case AlphaInfo of
    kCGImageAlphaNone,
    kCGImageAlphaNoneSkipLast,
    kCGImageAlphaNoneSkipFirst: begin
      ADesc.Depth := Prec * 3;
      // ADesc.AlphaPrec := 0;
    end;
  else
    ADesc.Depth := Prec * 4;
    ADesc.AlphaPrec := Prec;
  end;

  case AlphaInfo of
    kCGImageAlphaNone,
    kCGImageAlphaNoneSkipLast: begin
      // RGBx
      Shift := 32 - Prec;
      ADesc.RedShift := Shift;
      Dec(Shift, Prec);
      ADesc.GreenShift := Shift;
      Dec(Shift, Prec);
      ADesc.BlueShift := Shift;
    end;
    kCGImageAlphaNoneSkipFirst: begin
      // xRGB
      Shift := 0;
      ADesc.BlueShift := Shift;
      Inc(Shift, Prec);
      ADesc.GreenShift := Shift;
      Inc(Shift, Prec);
      ADesc.RedShift := Shift;
    end;
    kCGImageAlphaPremultipliedFirst,
    kCGImageAlphaFirst: begin
      // ARGB
      Shift := 32 - Prec;
      ADesc.AlphaShift := Shift;
      Dec(Shift, Prec);
      ADesc.RedShift := Shift;
      Dec(Shift, Prec);
      ADesc.GreenShift := Shift;
      Dec(Shift, Prec);
      ADesc.BlueShift := Shift;
    end;
    kCGImageAlphaPremultipliedLast,
    kCGImageAlphaLast: begin
      // RGBA
      Shift := 32 - Prec;
      ADesc.RedShift := Shift;
      Dec(Shift, Prec);
      ADesc.GreenShift := Shift;
      Dec(Shift, Prec);
      ADesc.BlueShift := Shift;
      Dec(Shift, Prec);
      ADesc.AlphaShift := Shift;
    end;
    kCGImageAlphaOnly: begin
      // A
      //ADesc.AlphaShift := 0;
    end;
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.RawImage_FromCarbonBitmap

  Creates a rawimage description for a carbonbitmap
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.RawImage_FromCarbonBitmap(out ARawImage: TRawImage; ABitmap, AMask: TCarbonBitmap; ARect: PRect = nil): Boolean;
begin
  FillChar(ARawImage, SizeOf(ARawImage), 0);
  RawImage_DescriptionFromCarbonBitmap(ARawImage.Description, ABitmap);

  ARawImage.DataSize := ABitmap.DataSize;
  ReAllocMem(ARawImage.Data, ARawImage.DataSize);
  if ARawImage.DataSize > 0 then
    System.Move(ABitmap.Data^, ARawImage.Data^, ARawImage.DataSize);

  Result := True;
  
  if AMask = nil then
  begin
    ARawImage.Description.MaskBitsPerPixel := 0;
    Exit;
  end;

  if AMask.Depth > 1
  then begin
    DebugLn('[WARNING] RawImage_FromCarbonBitmap: AMask.Depth > 1');
    Exit;
  end;

  ARawImage.MaskSize := AMask.DataSize;
  ReAllocMem(ARawImage.Mask, ARawImage.MaskSize);
  if ARawImage.MaskSize > 0 then
    System.Move(AMask.Data^, ARawImage.Mask^, ARawImage.MaskSize);
end;

function TCarbonWidgetSet.RawImage_DescriptionToBitmapType(
  ADesc: TRawImageDescription;
  out bmpType: TCarbonBitmapType): Boolean;
begin
  Result := False;

  if ADesc.Format = ricfGray
  then
  begin
    if ADesc.Depth = 1 then bmpType := cbtMono
    else bmpType := cbtGray;
  end
  else if ADesc.Depth = 1
  then bmpType := cbtMono
  else if ADesc.AlphaPrec <> 0
  then begin
    if ADesc.ByteOrder = riboMSBFirst
    then begin
      if  (ADesc.AlphaShift = 24)
      and (ADesc.RedShift   = 16)
      and (ADesc.GreenShift = 8 )
      and (ADesc.BlueShift  = 0 )
      then bmpType := cbtARGB
      else
      if  (ADesc.AlphaShift = 0)
      and (ADesc.RedShift   = 24)
      and (ADesc.GreenShift = 16 )
      and (ADesc.BlueShift  = 8 )
      then bmpType := cbtRGBA
      else
      if  (ADesc.AlphaShift = 0 )
      and (ADesc.RedShift   = 8 )
      and (ADesc.GreenShift = 16)
      and (ADesc.BlueShift  = 24)
      then bmpType := cbtBGRA
      else Exit;
    end
    else begin
      if  (ADesc.AlphaShift = 24)
      and (ADesc.RedShift   = 16)
      and (ADesc.GreenShift = 8 )
      and (ADesc.BlueShift  = 0 )
      then bmpType := cbtBGRA
      else
      if  (ADesc.AlphaShift = 0 )
      and (ADesc.RedShift   = 8 )
      and (ADesc.GreenShift = 16)
      and (ADesc.BlueShift  = 24)
      then bmpType := cbtARGB
      else
      if  (ADesc.AlphaShift = 24 )
      and (ADesc.RedShift   = 0 )
      and (ADesc.GreenShift = 8)
      and (ADesc.BlueShift  = 16)
      then bmpType := cbtRGBA
      else Exit;
    end;
  end
  else begin
    bmpType := cbtRGB;
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.GetImagePixelData
  
  Used by RawImage_FromDevice. Copies the data from a CGImageRef into a local
  buffer.

  The buffer is created using GetMem, and the caller is responsible for using
  FreeMem to free the returned pointer.
  
  This function throws exceptions in case of errors and may return a nil pointer.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetImagePixelData(AImage: CGImageRef; var bitmapByteCount: PtrUInt): Pointer;
var
  bitmapData: Pointer;
  context: CGContextRef = nil;
  colorSpace: CGColorSpaceRef;
  bitmapBytesPerRow, pixelsWide, pixelsHigh: PtrUInt;
  imageRect: CGRect;
begin
  Result := nil;

   // Get image width, height. The entire image is used.
  pixelsWide := CGImageGetWidth(AImage);
  pixelsHigh := CGImageGetHeight(AImage);
  imageRect.origin.x := 0.0;
  imageRect.origin.y := 0.0;
  imageRect.size.width := pixelsWide;
  imageRect.size.height := pixelsHigh;

  // The target format is fixed in ARGB, with 32-bits depth and
  // 8-bits per channel, the default image format on the LCL
  bitmapBytesPerRow   := (pixelsWide * 4);
  bitmapByteCount     := (bitmapBytesPerRow * pixelsHigh);

  // Use the generic RGB color space.
  colorSpace := CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
  if (colorSpace = nil) then RaiseColorSpaceError;

  // Allocate memory for image data. This is the destination in memory
  // where any drawing to the bitmap context will be rendered.
  bitmapData := System.GetMem( bitmapByteCount );
  if (bitmapData = nil) then RaiseMemoryAllocationError;

  { Creates the bitmap context.

    Regardless of what the source image format is, it will be converted
    over to the format specified here by CGBitmapContextCreate. }
  context := CGBitmapContextCreate(bitmapData,
                                   pixelsWide,
                                   pixelsHigh,
                                   8,      // bits per component
                                   bitmapBytesPerRow,
                                   colorSpace,
                                   kCGImageAlphaNoneSkipFirst); // The function fails with kCGImageAlphaFirst
  if (context = nil) then
  begin
    System.FreeMem(bitmapData);
    RaiseContextCreationError;
  end;

  // Draw the image to the bitmap context. Once we draw, the memory
  // allocated for the context for rendering will then contain the
  // raw image data in the specified color space.
  CGContextDrawImage(context, imageRect, AImage);

  // Now we can get a pointer to the image data associated with the context.
  // ToDo: Verify if we should copy this data to a new buffer
  Result := CGBitmapContextGetData(context);

  { Clean-up }
  CGColorSpaceRelease(colorSpace);
  CGContextRelease(context);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.CreateThemeServices
  Returns: Theme Services object for Carbon interface
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreateThemeServices: TThemeServices;
begin
  Result := TCarbonThemeServices.Create;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.PassCmdLineOptions

  Not used
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.PassCmdLineOptions;
begin
  inherited PassCmdLineOptions;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.SendCheckSynchronizeMessage
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.SendCheckSynchronizeMessage;
var
  EventSpec: EventTypeSpec;
  DummyEvent: EventRef;
begin
  if FMainEventQueue=nil then exit;
  
  {$IFDEF VerboseObject}
    DebugLnThreadLog('TCarbonWidgetSet.SendCheckSynchronizeMessage ');
  {$ENDIF}
  
  EventSpec := MakeEventSpec(LCLCarbonEventClass,LCLCarbonEventKindWake);
  DummyEvent:=nil;
  try
    if CreateEvent(nil, EventSpec.eventClass, EventSpec.eventKind,
      0{GetCurrentEventTime}, kEventAttributeNone, DummyEvent) <> noErr then
    begin
      {$IFDEF VerboseObject}
        DebugLnThreadLog('TCarbonWidgetSet.SendCheckSynchronizeMessage Create event FAILED');
      {$ENDIF}
      
      Exit;
    end;
    
    {$IFDEF VerboseObject}
      DebugLnThreadLog('TCarbonWidgetSet.SendCheckSynchronizeMessage GetMainEventQueue='+dbgs(GetMainEventQueue));
    {$ENDIF}

    if PostEventToQueue(FMainEventQueue, DummyEvent,
      kEventPriorityHigh) <> noErr then
    begin
      {$IFDEF VerboseObject}
        DebugLnThreadLog('TCarbonWidgetSet.SendCheckSynchronizeMessage Post event FAILED');
      {$ENDIF}
      
      Exit;
    end;
  finally
    if DummyEvent <> nil then ReleaseEvent(DummyEvent);
  end;
  
  {$IFDEF VerboseObject}
    DebugLnThreadLog('TCarbonWidgetSet.SendCheckSynchronizeMessage END');
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.OnWakeMainThread
  Params:  Sender
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.OnWakeMainThread(Sender: TObject);
begin
  // wait infinite for the first (dummy) event sent to the main event queue
  WaitFirstAppEvent;
  SendCheckSynchronizeMessage;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.RegisterEvents
  Registers events for Carbon application
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.RegisterEvents;
var
  TmpSpec: EventTypeSpec;
const
  SName = 'RegisterEvents';
begin
  //DebugLn('TCarbonWidgetSet.RegisterEvents');
  TmpSpec := MakeEventSpec(kEventClassCommand, kEventCommandProcess);
  InstallApplicationEventHandler(RegisterEventHandler(@CarbonApp_CommandProcess),
    1, @TmpSpec, nil, @FAEventHandlerRef[0]);
    
  TmpSpec := MakeEventSpec(kEventClassApplication, kEventAppShown);
  InstallApplicationEventHandler(RegisterEventHandler(@CarbonApp_Shown),
    1, @TmpSpec, nil, @FAEventHandlerRef[1]);
    
  TmpSpec := MakeEventSpec(kEventClassApplication, kEventAppHidden);
  InstallApplicationEventHandler(RegisterEventHandler(@CarbonApp_Hidden),
    1, @TmpSpec, nil, @FAEventHandlerRef[2]);

  TmpSpec := MakeEventSpec(kEventClassApplication, kEventAppDeactivated);
  InstallApplicationEventHandler(RegisterEventHandler(@CarbonApp_Deactivated),
    1, @TmpSpec, nil, @FAEventHandlerRef[3]);

  TmpSpec := MakeEventSpec(kEventClassApplication, kEventAppActivated);
  InstallApplicationEventHandler(RegisterEventHandler(@CarbonApp_Activated),
    1, @TmpSpec, nil, @FAEventHandlerRef[4]);

  FOpenEventHandlerUPP := NewAEEventHandlerUPP(AEEventHandlerProcPtr(@CarbonApp_Open));
  FQuitEventHandlerUPP := NewAEEventHandlerUPP(AEEventHandlerProcPtr(@CarbonApp_Quit));
  OSError(
    AEInstallEventHandler(kCoreEventClass, kAEOpenDocuments, FOpenEventHandlerUPP, 0, False),
    Self, SName, 'AEInstallEventHandler');
  OSError(
    AEInstallEventHandler(kCoreEventClass, kAEOpenContents, FOpenEventHandlerUPP, 0, False),
    Self, SName, 'AEInstallEventHandler');
  OSError(
    AEInstallEventHandler(kCoreEventClass, kAEQuitApplication, FQuitEventHandlerUPP, 0, False),
    Self, SName, 'AEInstallEventHandler');
end;


{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.AppTerminate

  Tells Carbon to halt the application
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.AppTerminate;
var i:integer;
const
  SName = 'AppTerminate';
begin
  if FTerminating then Exit;
  {$IFDEF VerboseObject}
    DebugLn('TCarbonWidgetSet.AppTerminate');
  {$ENDIF}
  FUserTerm:=True;
  QuitApplicationEventLoop;
  for i:=0 to 4 do
    OSError(MacOSALL.RemoveEventHandler(FAEventHandlerRef[i]),
    TClass(Self), SName, 'RemoveEventHandler');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.AppMinimize

  Minimizes the whole application to the taskbar
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.AppMinimize;
var
  Proc: ProcessSerialNumber;
const
  SName = 'AppMinimize';
begin
  {$IFDEF VerboseObject}
    DebugLn('TCarbonWidgetSet.AppMinimize');
  {$ENDIF}
  
  // hide process
  if OSError(GetCurrentProcess(Proc), Self, SName, SGetCurrentProc) then Exit;
    OSError(ShowHideProcess(Proc, False), Self, SName, SShowHideProc);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.AppRestore

  Restores the whole minimized application from the taskbar
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.AppRestore;
var
  Proc: ProcessSerialNumber;
const
  SName = 'AppRestore';
begin
  {$IFDEF VerboseObject}
    DebugLn('TCarbonWidgetSet.AppRestore');
  {$ENDIF}

  // show process
  if OSError(GetCurrentProcess(Proc), Self, SName, SGetCurrentProc) then Exit;
  OSError(ShowHideProcess(Proc, True), Self, SName, SShowHideProc);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.AppBringToFront

  Brings the entire application on top of all other non-topmost programs
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.AppBringToFront;
var
  Proc: ProcessSerialNumber;
const SName = 'AppBringToFront';
begin
  {$IFDEF VerboseObject}
    DebugLn('TCarbonWidgetSet.AppBringToFront');
  {$ENDIF}
  
  (*
    According to Carbon Development Tips & Tricks:
    34. How do I bring all my windows to the front?
  *)

  if OSError(GetCurrentProcess(Proc), Self, SName, SGetCurrentProc) then Exit;
  OSError(SetFrontProcess(Proc), Self, SName, 'SetFrontProcess');
end;

procedure TCarbonWidgetSet.AppSetIcon(const Small, Big: HICON);
begin
  if Big <> 0 then
    SetApplicationDockTileImage(TCarbonBitmap(Big).CGImage)
  else
    RestoreApplicationDockTileImage;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.AppSetTitle
  Params:  ATitle - New application title

  Changes the application title
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.AppSetTitle(const ATitle: string);
begin
  // not supported
end;

function TCarbonWidgetSet.GetLCLCapability(ACapability: TLCLCapability): PtrUInt;
begin
  case ACapability of
    lcCanDrawOutsideOnPaint,
    lcNeedMininimizeAppWithMainForm,
    lcApplicationTitle,
    lcFormIcon,
    lcReceivesLMClearCutCopyPasteReliably:
      Result := LCL_CAPABILITY_NO;
    lcAntialiasingEnabledByDefault:
      Result := LCL_CAPABILITY_YES;
  else
    Result := inherited;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.LCLPlatform
  Returns: lpCarbon - enum value for Carbon widgetset
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:= lpCarbon;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.DCGetPixel
  Params:  CanvasHandle - Canvas handle to get color from
           X, Y         - Position
  Returns: Color of the specified pixel on the canvas
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer
  ): TGraphicsColor;
begin
  Result := clNone;
  
  {$IFDEF VerboseObject}
    DebugLn('TCarbonWidgetSet.DCGetPixel DC: ' + DbgS(CanvasHandle) + ' X: ' + DbgS(X) + ' Y: ' + DbgS(Y));
  {$ENDIF}
  
  if not CheckDC(CanvasHandle, 'DCGetPixel') then Exit;

  Result := TCarbonDeviceContext(CanvasHandle).GetPixel(X, Y);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.DCSetPixel
  Params:  CanvasHandle - Canvas handle to get color from
           X, Y         - Position
           AColor       - New color for specified position

  Sets the color of the specified pixel on the canvas
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer;
  AColor: TGraphicsColor);
begin
  {$IFDEF VerboseObject}
    DebugLn('TCarbonWidgetSet.DCSetPixel DC: ' + DbgS(CanvasHandle) + ' X: ' + DbgS(X) + ' Y: ' + DbgS(Y) + 'Color: ' + DbgS(AColor));
  {$ENDIF}

  if not CheckDC(CanvasHandle, 'DCSetPixel') then Exit;

  TCarbonDeviceContext(CanvasHandle).SetPixel(X, Y, AColor);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.DCReDraw
  Params:  CanvasHandle - Canvas handle to redraw

  Redraws (the window of) a canvas
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.DCRedraw(CanvasHandle: HDC);
begin
  {$IFDEF VerboseObject}
    DebugLn('TCarbonWidgetSet.DCRedraw DC: ' + DbgS(CanvasHandle));
  {$ENDIF}
  
  if not CheckDC(CanvasHandle, 'DCRedraw') then Exit;

  CGContextFlush(TCarbonContext(CanvasHandle).CGContext);
end;

procedure TCarbonWidgetSet.DCSetAntialiasing(CanvasHandle: HDC;
  AEnabled: Boolean);
begin
  if not CheckDC(CanvasHandle, 'DCSetAntialiasing') then Exit;

  TCarbonDeviceContext(CanvasHandle).SetAntialiasing(AEnabled);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.SetDesigning
  Params:  AComponent - Component to set designing

  Not implemented!
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.SetDesigning(AComponent: TComponent);
begin

end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.IsHelpKey
  Params:  Key   -
           Shift -
  Returns: If the specified key is determined to show help in Carbon
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.IsHelpKey(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False; // help key is Cmd + ?, will be called directly on key press
end;

{------------------------------------------------------------------------------
  Method:  TimerCallback
  Params:  inTimer    - Timer reference
           inUserData - User data passed when installing timer

  Calls the timer function associated with specified timer
 ------------------------------------------------------------------------------}
procedure TimerCallback(inTimer: EventLoopTimerRef; inUserData: UnivPtr);
var
  TimerFunc: TWSTimerProc;
begin
  {$IFDEF VerboseTimer}
    DebugLn('TimerCallback');
  {$ENDIF}
  
  if CarbonWidgetSet = nil then Exit;
  if CarbonWidgetSet.FTimerMap.GetData(inTimer, TimerFunc) then
  begin
    {$IFDEF VerboseTimer}
      DebugLn('TimerCallback Timer instaåled, calling func.');
    {$ENDIF}
    
    TimerFunc;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.CreateTimer
  Params:  Interval  - New timer interval
           TimerFunc - New timer callback
  Returns: A Timer id

  Creates new timer with specified interval and callback function
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): THandle;
var
  Timer: EventLoopTimerRef;
begin
  {$IFDEF VerboseTimer}
    DebugLn('TCarbonWidgetSet.CreateTimer Interval: ' + DbgS(Interval));
  {$ENDIF}
  Result := 0;
  
  if (Interval > 0) and (TimerFunc <> nil) then
  begin
    if OSError(InstallEventLoopTimer(GetMainEventLoop,
      Interval / 1000, Interval / 1000, // converts msec -> sec
      EventLoopTimerUPP(@TimerCallback), nil, Timer), Self,
      'CreateTimer', 'InstallEventLoopTimer') then Exit;
      
    FTimerMap.Add(Timer, TimerFunc);
    Result := THandle(Timer)
  end;
  
  {$IFDEF VerboseTimer}
    DebugLn('TCarbonWidgetSet.CreateTimer Result: ' + DbgS(Result));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.Destroy
  Params:  TimerHandle - Timer id to destroy
  Returns: If the function succeeds

  Destroys specified timer
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.DestroyTimer(TimerHandle: THandle): boolean;
begin
  {$IFDEF VerboseTimer}
    DebugLn('TCarbonWidgetSet.DestroyTimer Handle: ' + DbgS(TimerHandle));
  {$ENDIF}

  Result := FTimerMap.Delete(TimerHandle);
  
  if Result then // valid timer
    OSError(RemoveEventLoopTimer(EventLoopTimerRef(TimerHandle)), Self,
      'DestroyTimer', 'RemoveEventLoopTimer');
end;

function TCarbonWidgetSet.PrepareUserEvent(Handle: HWND; Msg: Cardinal;
  wParam: WParam; lParam: LParam; out Target: EventTargetRef): EventRef;
var
  EventSpec: EventTypeSpec;
  AMessage: TLMessage;
  Widget: TCarbonWidget;
begin
  Result := nil;
  if FMainEventQueue = nil then Exit;

  Widget := TCarbonWidget(Handle);

  if Widget is TCarbonControl then
    Target := GetControlEventTarget(Widget.Widget)
  else
  if Widget is TCarbonWindow then
    Target := GetWindowEventTarget(Widget.Widget)
  else
    Exit;

  EventSpec := MakeEventSpec(LCLCarbonEventClass, LCLCarbonEventKindUser);
  if CreateEvent(nil, EventSpec.eventClass, EventSpec.eventKind,
    0, kEventAttributeUserEvent, Result) <> noErr then
    Exit;

  AMessage.Msg := Msg;
  AMessage.LParam := lParam;
  AMessage.WParam := wParam;
  AMessage.Result := 0;
  SetEventParameter(Result, MakeFourCC('wmsg'),
                            MakeFourCC('wmsg'), SizeOf(TLMessage),
                            @AMessage);
end;

