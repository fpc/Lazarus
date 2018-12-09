{%MainUnit cocoaint.pas}

{******************************************************************************
  All utility method implementations of the TCocoaWidgetSet class are here.


 ******************************************************************************
 Implementation
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TCocoaWidgetSet }

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppInit
  Params:  ScreenInfo

  Initialize Cocoa Widget Set
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
var
  lDict: NSDictionary;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppInit');
  {$ENDIF}
  WakeMainThread := @OnWakeMainThread;
  ScreenInfo.PixelsPerInchX := CocoaBasePPI;
  ScreenInfo.PixelsPerInchY := CocoaBasePPI;

  { Creates the application NSApp object }
  FNSApp := TCocoaApplication.sharedApplication;
  FNSApp_Delegate := TAppDelegate.alloc.init;
  FNSApp.setDelegate(FNSApp_Delegate);

  // Sandboxing
  lDict := NSProcessInfo.processInfo.environment;
  SandboxingOn := lDict.valueForKey(NSStr('APP_SANDBOX_CONTAINER_ID')) <> nil;
end;

procedure TCocoaWidgetSet.SendCheckSynchronizeMessage;
var
  Event: NSEvent;
begin
  Event := NSEvent.otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2(
    NSApplicationDefined,
    NSZeroPoint,
    0,
    GetCurrentEventTime,
    0,
    nil,
    LCLEventSubTypeMessage,
    LM_NULL,
    AppHandle);
  {$ifdef BOOLFIX}
  NSApp.postEvent_atStart_(Event, Ord(False));
  {$else}
  NSApp.postEvent_atStart(Event, False);
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.OnWakeMainThread
  Params:  Sender
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.OnWakeMainThread(Sender: TObject);
begin
   SendCheckSynchronizeMessage;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppRun
  Params:  ALoop
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
begin
  NSApp.finishLaunching;
  if Assigned(ALoop) then
  begin
    TCocoaApplication(NSApp).aloop:=ALoop;
    NSApp.run();
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppProcessMessages

  Handle all pending messages
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppProcessMessages;
var
  event: NSEvent;
  pool:NSAutoReleasePool;
begin
  repeat
    pool := NSAutoreleasePool.alloc.init;
    {$ifdef BOOLFIX}
    event := NSApp.nextEventMatchingMask_untilDate_inMode_dequeue_(NSAnyEventMask, nil, NSDefaultRunLoopMode, Ord(true));
    {$else}
    event := NSApp.nextEventMatchingMask_untilDate_inMode_dequeue(NSAnyEventMask, nil, NSDefaultRunLoopMode, true);
    {$endif}
    if event <> nil then
    begin
      if (event.type_ = NSApplicationDefined) and (event.subtype = LCLEventSubTypeMessage) and (event.data1 = LM_NULL) and (event.data2 = AppHandle) then
        CheckSynchronize
      else
        NSApp.sendEvent(event);
      NSApp.updateWindows;
    end;

    SyncClipboard(); // NSPasteboard doesn't provide any notifications regarding the change
                     // Thus we have to check the clipboard on every loop

    pool.release;
  until event = nil;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppWaitMessage

  Passes execution control to Cocoa
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppWaitMessage;
var
  event : NSEvent;
  pool:NSAutoReleasePool;
begin
  pool := NSAutoreleasePool.alloc.init;
  {$ifdef BOOLFIX}
  event := NSApp.nextEventMatchingMask_untilDate_inMode_dequeue_(NSAnyEventMask, NSDate.distantFuture, NSDefaultRunLoopMode, Ord(true));
  {$else}
  event := NSApp.nextEventMatchingMask_untilDate_inMode_dequeue(NSAnyEventMask, NSDate.distantFuture, NSDefaultRunLoopMode, true);
  {$endif}
  if event <> nil then
  begin
    if (event.type_ = NSApplicationDefined) and (event.subtype = LCLEventSubTypeMessage) and (event.data1 = LM_NULL) and (event.data2 = AppHandle) then
      CheckSynchronize
    else
      NSApp.sendEvent(event);
    NSApp.updateWindows;
  end;

  SyncClipboard(); // NSPasteboard doesn't provide any notifications regarding the change
                   // Thus we have to check the clipboard on every loop
  pool.release;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.Create

  Constructor for the class
 ------------------------------------------------------------------------------}
constructor TCocoaWidgetSet.Create;
begin
  CocoaWidgetSet := Self;
  inherited Create;
  FTerminating := False;
  FCurrentCursor:= 0;
  FCaptureControl:= 0;

  NSMessageWnd := NSStringUTF8('HWND');
  NSMessageMsg := NSStringUTF8('MSG');
  NSMessageWParam := NSStringUTF8('WPARAM');
  NSMessageLParam := NSStringUTF8('LPARAM');
  NSMessageResult := NSStringUTF8('RESULT');

  DefaultBrush := TCocoaBrush.CreateDefault(True);
  DefaultPen := TCocoaPen.CreateDefault(True);
  DefaultFont := TCocoaFont.CreateDefault(True);
  DefaultBitmap := TCocoaBitmap.CreateDefault;
  DefaultContext := TCocoaBitmapContext.Create;
  DefaultContext.Bitmap := DefaultBitmap;

  ScreenContext := TCocoaContext.Create(DefaultContext.ctx);

  InitStockItems;

  fClipboard := TCocoaWSClipboard.Create; // must be here otherwise clipboard calls before Application.Initialize crash
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.Destroy

  Destructor for the class
 ------------------------------------------------------------------------------}
destructor TCocoaWidgetSet.Destroy;
begin
  inherited Destroy;

  FreeStockItems;

  ScreenContext.Free;
  DefaultContext.Free;
  DefaultBitmap.Free;
  DefaultFont.Free;
  DefaultPen.Free;
  DefaultBrush.Free;

  FreeSysColorBrushes;

  fClipboard.Free;

  // The CocoaCaret is based WidgetSet timer.
  // The GlobalCaret is freed in finalization section, which is called
  // after the destruction of the widgetset and will cause a failure.
  // Need to destroy the caret here.. or CustomTimer must be verified.
  // or CocoaCaret should not use TTimer at all (use raw cocoa timer)
  DestroyGlobalCaret;

  NSMessageWnd.release;
  NSMessageMsg.release;
  NSMessageWParam.release;
  NSMessageLParam.release;
  NSMessageResult.release;

  // NSApp.terminate(nil);   // causes app to quit immediately, which is undesirable

  CocoaWidgetSet := nil;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppTerminate

  Tells Cocoa to halt the application
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppTerminate;
begin
  if FTerminating then Exit;
  // TODO: Check if there is more cleanup to do here
  // NSApp.terminate(nil);   // causes app to quit immediately, which is undesirable
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppMinimize

  Minimizes the whole application to the taskbar
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppMinimize;
begin
  NSApp.hide(NSApp);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppRestore

  Restores the whole minimized application from the taskbar
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppRestore;
begin
  NSApp.unhide(NSApp);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppBringToFront

  Brings the entire application on top of all other non-topmost programs
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppBringToFront;
begin
  {$ifdef BOOLFIX}
  NSApp.activateIgnoringOtherApps_(Ord(True));
  {$else}
  NSApp.activateIgnoringOtherApps(True);
  {$endif}
end;

procedure TCocoaWidgetSet.AppSetIcon(const Small, Big: HICON);
begin
  if Big <> 0 then
    NSApp.setApplicationIconImage(TCocoaBitmap(Big).image)
  else
    NSApp.setApplicationIconImage(nil);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppSetTitle
  Params:  ATitle - New application title

  Changes the application title
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppSetTitle(const ATitle: string);
begin
  // There is no way to change the dock title
end;

function TCocoaWidgetSet.GetLCLCapability(ACapability: TLCLCapability): PtrUInt;
begin
  case ACapability of
    lcCanDrawOutsideOnPaint,
    lcNeedMininimizeAppWithMainForm,
    lcApplicationTitle,
    lcFormIcon,
    {$ifndef COCOA_USE_NATIVE_MODAL}
    lcModalWindow,
    {$endif}
    lcReceivesLMClearCutCopyPasteReliably:
      Result := LCL_CAPABILITY_NO;
    {$ifdef COCOA_USE_NATIVE_MODAL}
    lcModalWindow,
    {$endif}
    lcAntialiasingEnabledByDefault:
      Result := LCL_CAPABILITY_YES;
    lcTransparentWindow: Result := LCL_CAPABILITY_YES;
  else
    Result := inherited;
  end;
end;

function TCocoaWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): THandle;
var
  timer : NSTimer;
  user  : TCocoaTimerObject;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.CreateTimer');
  {$ENDIF}
  user:=TCocoaTimerObject.newWithFunc(TimerFunc);

  timer:=NSTimer.timerWithTimeInterval_target_selector_userInfo_repeats(
    Interval/1000, user, objcselector(user.timerEvent), user, True);

  NSRunLoop.currentRunLoop.addTimer_forMode(timer, NSDefaultRunLoopMode);

  {user is retained (twice, because it's target), by the timer and }
  {released (twice) on timer invalidation}
  user.release;

  Result:=THandle(timer);
end;

function TCocoaWidgetSet.DestroyTimer(TimerHandle: THandle): boolean;
var
  obj : NSObject;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.DestroyTimer');
  {$ENDIF}
  obj:=NSObject(TimerHandle);
  try
    Result:= Assigned(obj) and obj.isKindOfClass_(NSTimer);
  except
    Result:=false;
  end;
  if not Result then Exit;
  NSTimer(obj).invalidate;
end;

function TCocoaWidgetSet.NewUserEventInfo(Handle: HWND; Msg: Cardinal;
  wParam: WParam; lParam: LParam): NSMutableDictionary;
var
  LocalPool: NSAutoReleasePool;
  Keys, Objs: NSMutableArray;
begin
  // create a dinctionary
  LocalPool := NSAutoReleasePool.alloc.init;
  Keys := NSMutableArray.arrayWithObjects(
    NSMessageWnd,
    NSMessageMsg,
    NSMessageWParam,
    NSMessageLParam,
    NSMessageResult,
    nil);
  Objs := NSMutableArray.arrayWithObjects(
    NSNumber.numberWithUnsignedInteger(Handle),
    NSNumber.numberWithUnsignedLong(Msg),
    NSNumber.numberWithInteger(wParam),
    NSNumber.numberWithInteger(lParam),
    NSNumber.numberWithInteger(0),
    nil);
  Result := NSMutableDictionary.dictionaryWithObjects_forKeys(Objs, Keys);
  Result.retain;
  // release everything
  LocalPool.release;
end;

function TCocoaWidgetSet.PrepareUserEvent(Handle: HWND; Info: NSDictionary; NeedsResult: Boolean): NSEvent;
var
  Obj: NSObject;
  Win: NSWindow;
begin
  Obj := NSObject(Handle);
  if Obj.isKindOfClass(NSWindow) then
    Win := NSWindow(Obj)
  else if Obj.isKindOfClass(NSView) then
    Win := NSWindow(NSView(Handle).window)
  else if GetNSObjectView(Obj) <> nil then
    Win := NSWindow(GetNSObjectView(Obj).window)
  else
    Exit(nil);
  Result := NSEvent.otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2(
    NSApplicationDefined,
    NSZeroPoint,
    0,
    GetCurrentEventTime,
    Win.windowNumber,
    nil,
    LCLEventSubTypeMessage,
    NSInteger(Info),
    NSInteger(NeedsResult));
end;

procedure TCocoaWidgetSet.InitStockItems;
var
  LogBrush: TLogBrush;
  logPen: TLogPen;
  pool: NSAutoreleasePool;
begin
  FillChar(LogBrush, SizeOf(TLogBrush),0);
  LogBrush.lbStyle := BS_NULL;
  FStockNullBrush := HBrush(TCocoaBrush.Create(LogBrush, True));

  LogBrush.lbStyle := BS_SOLID;
  LogBrush.lbColor := $000000;
  FStockBlackBrush := HBrush(TCocoaBrush.Create(LogBrush, True));

  LogBrush.lbColor := $C0C0C0;
  FStockLtGrayBrush := HBrush(TCocoaBrush.Create(LogBrush, True));

  LogBrush.lbColor := $808080;
  FStockGrayBrush := HBrush(TCocoaBrush.Create(LogBrush, True));

  LogBrush.lbColor := $404040;
  FStockDkGrayBrush := HBrush(TCocoaBrush.Create(LogBrush, True));

  LogBrush.lbColor := $FFFFFF;
  FStockWhiteBrush := HBrush(TCocoaBrush.Create(LogBrush, True));

  LogPen.lopnStyle := PS_NULL;
  LogPen.lopnWidth := Types.Point(0, 0); // create cosmetic pens
  LogPen.lopnColor := $FFFFFF;
  FStockNullPen := HPen(TCocoaPen.Create(LogPen, True));

  LogPen.lopnStyle := PS_SOLID;
  FStockWhitePen := HPen(TCocoaPen.Create(LogPen, True));

  LogPen.lopnColor := $000000;
  FStockBlackPen := HPen(TCocoaPen.Create(LogPen, True));

  FStockSystemFont := HFont(TCocoaFont.CreateDefault(True));
  pool := NSAutoreleasePool.alloc.init;
  FStockFixedFont := HFont(TCocoaFont.Create(NSFont.userFixedPitchFontOfSize(0), True));
  pool.release;
end;

procedure TCocoaWidgetSet.FreeStockItems;

  procedure DeleteAndNilObject(var h: HGDIOBJ);
  begin
    if h <> 0 then
      TCocoaGDIObject(h).Global := False;
    DeleteObject(h);
    h := 0;
  end;

begin
  DeleteAndNilObject(FStockNullBrush);
  DeleteAndNilObject(FStockBlackBrush);
  DeleteAndNilObject(FStockLtGrayBrush);
  DeleteAndNilObject(FStockGrayBrush);
  DeleteAndNilObject(FStockDkGrayBrush);
  DeleteAndNilObject(FStockWhiteBrush);

  DeleteAndNilObject(FStockNullPen);
  DeleteAndNilObject(FStockBlackPen);
  DeleteAndNilObject(FStockWhitePen);

  DeleteAndNilObject(FStockFixedFont);
  DeleteAndNilObject(FStockSystemFont);

end;

procedure TCocoaWidgetSet.FreeSysColorBrushes;

  procedure DeleteAndNilObject(var h: HBrush);
  begin
    if h <> 0 then
    begin
      TCocoaBrush(h).Free;
      h := 0;
    end;
  end;

var
  i: integer;
begin
  for i := Low(FSysColorBrushes) to High(FSysColorBrushes) do
    DeleteAndNilObject(FSysColorBrushes[i]);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.GetAppHandle
  Returns: Returns NSApp object, created via NSApplication.sharedApplication
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.GetAppHandle: THandle;
begin
  Result:=THandle(NSApp);
end;

function TCocoaWidgetSet.CreateThemeServices: TThemeServices;
begin
  Result:=TCocoaThemeServices.Create;
end;

function TCocoaWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
begin
  Result := 0;
  if CanvasHandle <> 0 then
    Result := TCocoaContext(CanvasHandle).GetPixel(X,Y);
end;

procedure TCocoaWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
begin
  if CanvasHandle <> 0 then
    TCocoaContext(CanvasHandle).SetPixel(X,Y,AColor);
end;

procedure TCocoaWidgetSet.DCRedraw(CanvasHandle: HDC);
begin
  if CanvasHandle <> 0 then
    TCocoaContext(CanvasHandle).ctx.flushGraphics;
end;

procedure TCocoaWidgetSet.DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean);
begin
  if CanvasHandle <> 0 then
    TCocoaContext(CanvasHandle).SetAntialiasing(AEnabled);
end;

procedure TCocoaWidgetSet.SetDesigning(AComponent: TComponent);
begin

end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.LCLPlatform
  Returns: lpCocoa - enum value for Cocoa widgetset
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:= lpCocoa;
end;

{ TCocoaTimerObject }

procedure TCocoaTimerObject.timerEvent;
begin
  if Assigned(@func) then func;
end;

class function TCocoaTimerObject.newWithFunc(afunc: TWSTimerProc): TCocoaTimerObject;
begin
  Result:=alloc;
  Result.func:=afunc;
end;

procedure TAppDelegate.application_openFiles(sender: NSApplication; filenames: NSArray);
var
  lFiles: array of string;
  lNSStr: NSString;
  i: Integer;
begin
  SetLength(lFiles, filenames.count);
  for i := 0 to filenames.count-1 do
  begin
    lNSStr := NSString(filenames.objectAtIndex(i));
    lFiles[i] := NSStringToString(lNSStr);
  end;
  Application.IntfDropFiles(lFiles);
  if Application.MainForm<>nil then
    Application.MainForm.IntfDropFiles(lFiles);
end;

procedure TAppDelegate.applicationDidHide(notification: NSNotification);
begin
  Application.IntfAppMinimize;
end;

procedure TAppDelegate.applicationDidUnhide(notification: NSNotification);
begin
  Application.IntfAppRestore;
end;

procedure TAppDelegate.applicationDidBecomeActive(notification: NSNotification);
begin
  Application.IntfAppActivate;
end;

procedure TAppDelegate.applicationDidResignActive(notification: NSNotification);
begin
  Application.IntfAppDeactivate;
end;

procedure TAppDelegate.applicationDidChangeScreenParameters(notification: NSNotification);
begin
  Screen.UpdateMonitors;
  Screen.UpdateScreen;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.RawImage_DescriptionFromCocoaBitmap

  Creates a rawimage description for a cocoabitmap
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.RawImage_DescriptionFromCocoaBitmap(out ADesc: TRawImageDescription; ABitmap: TCocoaBitmap): Boolean;
var
  Prec, Shift: Byte;
  BPR: Integer;
  HasAlpha: Boolean;
begin
  ADesc.Init;

  case ABitmap.BitmapType of
    cbtMono, cbtGray: ADesc.Format := ricfGray;
  else
    ADesc.Format := ricfRGBA;
  end;

  ADesc.Width := Round(ABitmap.image.size.width);
  ADesc.Height := Round(ABitmap.image.size.Height);

  //ADesc.PaletteColorCount := 0;

  ADesc.BitOrder := riboReversedBits;
  ADesc.ByteOrder := riboMSBFirst;

  BPR := ABitmap.BytesPerRow;
  if BPR and $F = 0 then ADesc.LineEnd := rileDQWordBoundary     // 128bit aligned
  else if BPR and $7 = 0 then ADesc.LineEnd := rileQWordBoundary //  64bit aligned
  else if BPR and $3 = 0 then ADesc.LineEnd := rileWordBoundary  //  32bit aligned
  else if BPR and $1 = 0 then ADesc.LineEnd := rileByteBoundary  //   8bit aligned
  else ADesc.LineEnd := rileTight;

  ADesc.LineOrder := riloTopToBottom;
  ADesc.BitsPerPixel := ABitmap.BitsPerPixel;

  ADesc.MaskBitOrder := riboReversedBits;
  ADesc.MaskBitsPerPixel := 1;
  ADesc.MaskLineEnd := rileByteBoundary;
  // ADesc.MaskShift := 0;

  ADesc.Depth := ABitmap.Depth;
  Prec := ABitmap.BitsPerSample;

  ADesc.RedPrec := Prec;
  ADesc.GreenPrec := Prec;
  ADesc.BluePrec := Prec;

  // gray or mono
  if ADesc.Format = ricfGray then begin
    Result := true;
    Exit;
  end;

  // alpha
  if ABitmap.BitmapType in [cbtARGB, cbtRGBA] then
    ADesc.AlphaPrec := Prec;

  HasAlpha := ABitmap.ImageRep.hasAlpha;

  case ABitmap.BitmapType of
    cbtRGB: begin
      Shift := 24 - Prec;
      ADesc.RedShift := Shift;
      Dec(Shift, Prec);
      ADesc.GreenShift := Shift;
      Dec(Shift, Prec);
      ADesc.BlueShift := Shift;
    end;
    cbtARGB: begin
      Shift := 32 - Prec;
      ADesc.AlphaShift := Shift;
      Dec(Shift, Prec);
      ADesc.RedShift := Shift;
      Dec(Shift, Prec);
      ADesc.GreenShift := Shift;
      Dec(Shift, Prec);
      ADesc.BlueShift := Shift;
    end;
    cbtRGBA: begin
      Shift := 32 - Prec;
      ADesc.RedShift := Shift;
      Dec(Shift, Prec);
      ADesc.GreenShift := Shift;
      Dec(Shift, Prec);
      ADesc.BlueShift := Shift;
      Dec(Shift, Prec);
      ADesc.AlphaShift := Shift;
    end;
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.RawImage_FromCocoaBitmap

  Creates a rawimage description for a cocoabitmap
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.RawImage_FromCocoaBitmap(out ARawImage: TRawImage; ABitmap, AMask: TCocoaBitmap; ARect: PRect = nil): Boolean;
var
  lBitmapData: PByte;
begin
  FillChar(ARawImage, SizeOf(ARawImage), 0);
  RawImage_DescriptionFromCocoaBitmap(ARawImage.Description, ABitmap);

  ARawImage.DataSize := ABitmap.DataSize;
  ReAllocMem(ARawImage.Data, ARawImage.DataSize);
  lBitmapData := ABitmap.GetNonPreMultipliedData();
  if ARawImage.DataSize > 0 then
    System.Move(lBitmapData^, ARawImage.Data^, ARawImage.DataSize);

  Result := True;
  
  if AMask = nil then
  begin
    ARawImage.Description.MaskBitsPerPixel := 0;
    Exit;
  end;

  if AMask.Depth > 1
  then begin
    DebugLn('[WARNING] RawImage_FromCocoaBitmap: AMask.Depth > 1');
    Exit;
  end;

  ARawImage.MaskSize := AMask.DataSize;
  ReAllocMem(ARawImage.Mask, ARawImage.MaskSize);
  if ARawImage.MaskSize > 0 then
    System.Move(AMask.Data^, ARawImage.Mask^, ARawImage.MaskSize);
end;

function TCocoaWidgetSet.RawImage_DescriptionToBitmapType(
  ADesc: TRawImageDescription;
  out bmpType: TCocoaBitmapType): Boolean;
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
      else Exit;
    end
    else begin
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

