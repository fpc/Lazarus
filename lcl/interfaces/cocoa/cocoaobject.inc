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
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppInit');
  {$ENDIF}
  ScreenInfo.PixelsPerInchX := CocoaConfigGlobal.basePPI;
  ScreenInfo.PixelsPerInchY := CocoaConfigGlobal.basePPI;

  { Creates the application NSApp object }
  TCocoaApplication.initApplication;
  {$ifdef COCOALOOPOVERRIDE}
  NSApp.finishLaunching;
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppRun
  Params:  ALoop
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
begin
  if Assigned(ALoop) then
  begin
    TCocoaApplication(NSApp).aloop:=ALoop;
    CocoaWidgetSetService.setReadyDropFiles;
    CocoaWidgetSetService.dropWaitingFiles;
    NSApp.run();
  end;
end;

function TCocoaWidgetSet.nextEvent(const eventExpDate: NSDate): NSEvent;
begin
  {$ifdef BOOLFIX}
  Result := NSApp.nextEventMatchingMask_untilDate_inMode_dequeue_(NSAnyEventMask, eventExpDate, NSDefaultRunLoopMode, Ord(true));
  {$else}
  Result := NSApp.nextEventMatchingMask_untilDate_inMode_dequeue(NSAnyEventMask, eventExpDate, NSDefaultRunLoopMode, true);
  {$endif}
end;

// before entering RunLoop (TCocoaWidgetSet.AppRun), APP may call
// TApplication.ProcessMessages (for example, to display Splash Form in IDE).
// because it has not yet entered the message processing loop, nextEvent should
// be called multiple times to ensure various asynchronous callback situations.
// otherwise, it will cause some strange problems, and it is difficult to find
// the reason, such as the display problem of Splash Form.
// see also: https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/40484
function TCocoaWidgetSet.nextEventBeforeRunLoop(const eventExpDate: NSDate): NSEvent;
var
  i: Integer;
begin
  for i := 1 to 3 do
  begin
    Result := nextEvent(eventExpDate);
    if Assigned(Result) then
      break;
  end;
end;

procedure TCocoaWidgetSet.AppRunMessages(onlyOne: Boolean; eventExpDate: NSDate);
var
  event: NSEvent;
  pool:NSAutoReleasePool;
begin
  repeat
    pool := NSAutoreleasePool.alloc.init;
    if Assigned(TCocoaApplication(NSApp).aloop) or Assigned(eventExpDate) then
      event := nextEvent(eventExpDate)
    else
      event := nextEventBeforeRunLoop(eventExpDate);
    if event <> nil then
    begin
      NSApp.sendEvent(event);
      NSApp.updateWindows;
    end;

    SyncClipboard(); // NSPasteboard doesn't provide any notifications regarding the change
                     // Thus we have to check the clipboard on every loop

    pool.release;
  until onlyOne or (event = nil);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppProcessMessages

  Handle all pending messages
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppProcessMessages;
begin
  AppRunMessages(false, nil);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppWaitMessage

  Passes execution control to Cocoa
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppWaitMessage;
begin
  AppRunMessages(true, NSDate.distantFuture);
end;

function TCocoaWidgetSet.BeginMessageProcess: TLCLHandle;
begin
  Result := TLCLHandle(NSAutoreleasePool.alloc.init);
end;

procedure TCocoaWidgetSet.EndMessageProcess(context: TLCLHandle);
begin
  NSAutoreleasePool(context).release;
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

  DefaultBrush := TCocoaBrush.CreateDefault(True);
  DefaultPen := TCocoaPen.CreateDefault(True);
  DefaultFont := TCocoaFont.CreateDefault(True);
  DefaultBitmap := TCocoaBitmap.CreateDefault;
  DefaultContext := TCocoaBitmapContext.Create;
  DefaultContext.Bitmap := DefaultBitmap;

  ScreenContext := TCocoaContext.Create(DefaultContext.ctx);

  _gdiObject:= TCocoaWidgetSetGDIObject.Create;

  fClipboard := TCocoaWSClipboard.Create; // must be here otherwise clipboard calls before Application.Initialize crash
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.Destroy

  Destructor for the class
 ------------------------------------------------------------------------------}
destructor TCocoaWidgetSet.Destroy;
begin
  CocoaWidgetSetService.releaseWaitingLCLObjects(0);
  inherited Destroy;

  ScreenContext.Free;
  DefaultContext.Free;
  DefaultBitmap.Free;
  DefaultFont.Free;
  DefaultPen.Free;
  DefaultBrush.Free;

  _gdiObject.Free;

  fClipboard.Free;

  // The CocoaCaret is based WidgetSet timer.
  // The GlobalCaret is freed in finalization section, which is called
  // after the destruction of the widgetset and will cause a failure.
  // Need to destroy the caret here.. or CustomTimer must be verified.
  // or CocoaCaret should not use TTimer at all (use raw cocoa timer)
  DestroyGlobalCaret;

  // NSApp.terminate(nil);   // causes app to quit immediately, which is undesirable

  // Must release the Main autorelease pool here.
  // Some objects still in the pool my depend on releasing Widgetset objects
  // (i.e. images). If autorelease pool is released After the widgetset object
  // then it finalization of WS dependent objects would fail (suppressed AVs)
  // and would cause leaks. (see #35400)
  CocoaWidgetSetService.finalAutoreleaseMainPool;

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

  {$ifdef COCOALOOPNATIVE}
  NSApp.stop(nil);
  {$else}
  TCocoaApplicationUtil.wakeupEventLoop;
  {$endif}
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

// NSModalPanelWindowLevel has higher priority than NSFloatingWindowLevel
// on Cocoa, so nothing needs to be done
function TCocoaWidgetSet.AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean
  ): Boolean;
begin
  Result:= true;
end;

function TCocoaWidgetSet.AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean
  ): Boolean;
begin
  Result:= true;
end;

function TCocoaWidgetSet.GetLCLCapability(ACapability: TLCLCapability): PtrUInt;
begin
  case ACapability of
    lcCanDrawOutsideOnPaint,
    lcNeedMininimizeAppWithMainForm,
    {$ifndef COCOA_USE_NATIVE_MODAL}
    lcModalWindow,
    {$endif}
    lcApplicationTitle:
      Result := LCL_CAPABILITY_NO;
    {$ifdef COCOA_USE_NATIVE_MODAL}
    lcModalWindow,
    {$endif}
    lcFormIcon,
    lcAntialiasingEnabledByDefault,
    lcTransparentWindow,
    lcCanDrawHidden:
      Result := LCL_CAPABILITY_YES;
    lcAccelleratorKeys:
      Result := LCL_CAPABILITY_NO;
    lcTextHint:
      if NSAppKitVersionNumber >= NSAppKitVersionNumber10_10 then
        Result := LCL_CAPABILITY_YES
      else
        Result := LCL_CAPABILITY_NO;
  else
    Result := inherited;
  end;
end;

function TCocoaWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): TLCLHandle;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.CreateTimer');
  {$ENDIF}
  Result:=TLCLHandle(TCocoaTimerObject.alloc.initWithInterval_func(Interval, TimerFunc));
end;

function TCocoaWidgetSet.DestroyTimer(TimerHandle: TLCLHandle): boolean;
var
  obj : NSObject;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.DestroyTimer');
  {$ENDIF}
  obj:=NSObject(TimerHandle);
  try
    Result:= Assigned(obj) and obj.isKindOfClass_(TCocoaTimerObject);
  except
    Result:=false;
  end;
  if not Result then Exit;
  TCocoaTimerObject(obj).invalidate;
  obj.release;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.GetAppHandle
  Returns: Returns NSApp object, created via NSApplication.sharedApplication
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.GetAppHandle: TLCLHandle;
begin
  Result:=TLCLHandle(NSApp);
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

function TCocoaTimerObject.initWithInterval_func(interval: integer;
  timerFunc: TWSTimerProc): id;
begin
  Self:=TCocoaTimerObject(inherited init);
  Result:=Self;
  if not Assigned(Result) then Exit;
  func:=timerFunc;
  // timer maintains a strong reference to Self until it's invalidate is called
  timer:=NSTimer.timerWithTimeInterval_target_selector_userInfo_repeats(
    interval/1000, Self, objcselector(timerFireMethod), nil, True);
  if timer = nil then Exit;
  timer.retain;
  // adding timer to all "common" loop mode.
  NSRunLoop.currentRunLoop.addTimer_forMode(timer, NSDefaultRunLoopMode);
  NSRunLoop.currentRunLoop.addTimer_forMode(timer, NSModalPanelRunLoopMode);
  NSRunLoop.currentRunLoop.addTimer_forMode(timer, NSEventTrackingRunLoopMode);
end;

procedure TCocoaTimerObject.invalidate;
begin
  if timer=nil then Exit;
  func:=nil;
  timer.invalidate;
  timer.release;
  timer:=nil;
end;

procedure TCocoaTimerObject.timerFireMethod(atimer: NSTimer);
begin
  if Assigned(func) then func;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.RawImage_DescriptionFromCocoaBitmap

  Creates a rawimage description for a cocoabitmap
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.RawImage_DescriptionFromCocoaBitmap(out ADesc: TRawImageDescription; ABitmap: TCocoaBitmap): Boolean;
var
  Prec, Shift: Byte;
  BPR: Integer;
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
      if  (ADesc.AlphaShift = 24)
      and (ADesc.RedShift   = 0 )
      and (ADesc.GreenShift = 8 )
      and (ADesc.BlueShift  = 16)
      then bmpType := cbtABGR
      else
      if  (ADesc.AlphaShift = 0 )
      and (ADesc.RedShift   = 24)
      and (ADesc.GreenShift = 16)
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
      if  (ADesc.AlphaShift = 0 )
      and (ADesc.RedShift   = 8 )
      and (ADesc.GreenShift = 16)
      and (ADesc.BlueShift  = 24)
      then bmpType := cbtARGB
      else
      if  (ADesc.AlphaShift = 0 )
      and (ADesc.RedShift   = 24)
      and (ADesc.GreenShift = 16)
      and (ADesc.BlueShift  = 8 )
      then bmpType := cbtABGR
      else
      if  (ADesc.AlphaShift = 24)
      and (ADesc.RedShift   = 0 )
      and (ADesc.GreenShift = 8 )
      and (ADesc.BlueShift  = 16)
      then bmpType := cbtRGBA
      else
      if  (ADesc.AlphaShift = 24)
      and (ADesc.RedShift   = 16)
      and (ADesc.GreenShift = 8 )
      and (ADesc.BlueShift  = 0 )
      then bmpType := cbtBGRA
      else Exit;
    end;
  end
  else begin
    bmpType := cbtRGB;
  end;

  Result := True;
end;

