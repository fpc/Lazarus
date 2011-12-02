{%MainUnit customdrawnint.pas}

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

{ TCDWidgetSet }

{------------------------------------------------------------------------------
  Method:  TCDWidgetSet.AppInit
  Params:  ScreenInfo

  Initialize Carbon Widget Set
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
begin
  {$IFDEF VerboseObject}
    DebugLn('TCDWidgetSet.AppInit');
  {$ENDIF}

  delegate:=TCDAppDelegate.alloc;

  { Creates the application NSApp object }
  NsApp := NSApplication.sharedApplication;
  NSApp.setDelegate(delegate);
end;

{------------------------------------------------------------------------------
  Method:  TCDWidgetSet.AppRun
  Params:  ALoop
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
begin
  {$IFDEF VerboseObject}
    DebugLn('TCDWidgetSet.AppRun');
  {$ENDIF}

  { Enters main message loop }
  NSApp.run;
end;

{------------------------------------------------------------------------------
  Method:  TCDWidgetSet.AppProcessMessages

  Handle all pending messages
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppProcessMessages;
var
  event : NSEvent;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCDWidgetSet.AppProcessMessages');
  {$ENDIF}

  event:=NSApp.nextEventMatchingMask_untilDate_inMode_dequeue(NSAnyEventMask, nil, NSDefaultRunLoopMode, true);
  NSApp.sendEvent(event);

  {$IFDEF VerboseObject}
    DebugLn('TCDWidgetSet.AppProcessMessages END');
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  TCDWidgetSet.AppWaitMessage

  Passes execution control to Cocoa
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppWaitMessage;
var
  event : NSEvent;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCDWidgetSet.AppWaitMessage');
  {$ENDIF}
  event:=NSApp.nextEventMatchingMask_untilDate_inMode_dequeue(NSAnyEventMask, NSDate.distantFuture, NSDefaultRunLoopMode, true);
  NSApp.sendEvent(event);
end;

{------------------------------------------------------------------------------
  Method:  TCDWidgetSet.Create

  Constructor for the class
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.BackendCreate;
begin
  {  Creates the AutoreleasePool }
  pool := NSAutoreleasePool(NSAutoreleasePool.alloc).init;

  { Prepares the Native DC for the ScreenDC }

  ScreenBitmap := TCocoaBitmap.Create(ScreenBitmapWidth, ScreenBitmapHeight,
    32, 4, cbaDWord, cbtARGB, ScreenBitmapRawImage.Data);
  ScreenDC.NativeDC := PtrInt(TCocoaContext.Create);
  TCocoaContext(ScreenDC.NativeDC).ctx := NSGraphicsContext.graphicsContextWithBitmapImageRep(ScreenBitmap.imagerep);
end;

{------------------------------------------------------------------------------
  Method:  TCDWidgetSet.Destroy

  Destructor for the class
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.BackendDestroy;
begin
  {  Releases the AutoreleasePool }
  pool.release;
end;

{------------------------------------------------------------------------------
  Method:  TCDWidgetSet.AppTerminate

  Tells Carbon to halt the application
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppTerminate;
begin
  if FTerminating then Exit;
  {$IFDEF VerboseObject}
    DebugLn('TCDWidgetSet.AppTerminate');
  {$ENDIF}

  NSApp.terminate(nil);
end;

{------------------------------------------------------------------------------
  Method:  TCDWidgetSet.AppMinimize

  Minimizes the whole application to the taskbar
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppMinimize;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCDWidgetSet.AppMinimize');
  {$ENDIF}
  NSApp.miniaturizeAll(nil);
end;

{------------------------------------------------------------------------------
  Method:  TCDWidgetSet.AppRestore

  Restores the whole minimized application from the taskbar
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppRestore;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCDWidgetSet.AppRestore');
  {$ENDIF}
  NSApp.activateIgnoringOtherApps(False);
end;

{------------------------------------------------------------------------------
  Method:  TCDWidgetSet.AppBringToFront

  Brings the entire application on top of all other non-topmost programs
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppBringToFront;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCDWidgetSet.AppBringToFront');
  {$ENDIF}
  NSApp.activateIgnoringOtherApps(True);
end;

procedure TCDWidgetSet.AppSetIcon(const Small, Big: HICON);
begin
  if Big <> 0 then
    NSApp.setApplicationIconImage(TCocoaBitmap(Big).image)
  else
    NSApp.setApplicationIconImage(nil);
end;

{------------------------------------------------------------------------------
  Method:  TCDWidgetSet.AppSetTitle
  Params:  ATitle - New application title

  Changes the application title
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppSetTitle(const ATitle: string);
var
  ns: NSString;
begin
{  if not Assigned(NSApp.dockTile) then Exit;
  //todo: setBadgeLabel is for 10.5 only, should be removed
  if NSApp.dockTile.respondsToSelector_(objcselector('setBadgeLabel:')) then
  begin
    ns := NSStringUtf8(ATitle);
    NSApp.dockTile.setBadgeLabel(NSStringUtf8(ATitle));
    ns.release;
  end;}
end;

procedure TCDWidgetSet.AppSetVisible(const AVisible: Boolean);
begin
end;

function TCDWidgetSet.AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
end;

function TCDWidgetSet.AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
end;

procedure TCDWidgetSet.AppSetMainFormOnTaskBar(const DoSet: Boolean);
begin

end;

function TCDWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): THandle;
var
  timer : NSTimer;
  user  : TCDTimerObject;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCDWidgetSet.CreateTimer');
  {$ENDIF}
  user:=TCDTimerObject.initWithFunc(TimerFunc);

  timer:=NSTimer.timerWithTimeInterval_target_selector_userInfo_repeats(
    Interval/1000, user, objcselector(user.timerEvent), user, True);

  NSRunLoop.currentRunLoop.addTimer_forMode(timer, NSDefaultRunLoopMode);

  {user is retained (twice, because it's target), by the timer and }
  {released (twice) on timer invalidation}
  user.release;

  Result:=THandle(timer);
end;

function TCDWidgetSet.DestroyTimer(TimerHandle: THandle): boolean;
var
  obj : NSObject;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCDWidgetSet.DestroyTimer');
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

{------------------------------------------------------------------------------
  Method:  TCDWidgetSet.GetAppHandle
  Returns: Returns NSApp object, created via NSApplication.sharedApplication
 ------------------------------------------------------------------------------}
(*function TCDWidgetSet.GetAppHandle: THandle;
begin
  Result:=THandle(NSApp);
end;

function TCDWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
begin
  Result:=0;
end;

procedure TCDWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
begin

end;

procedure TCDWidgetSet.DCRedraw(CanvasHandle: HDC);
begin

end;

procedure TCDWidgetSet.DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean);
begin
  inherited DCSetAntialiasing(CanvasHandle, AEnabled);
end;

procedure TCDWidgetSet.SetDesigning(AComponent: TComponent);
begin

end;

{------------------------------------------------------------------------------
  Method:  TCDWidgetSet.LCLPlatform
  Returns: lpCocoa - enum value for Cocoa widgetset
 ------------------------------------------------------------------------------}
function TCDWidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:= lpCocoa;
end;

procedure InternalInit;
begin
end;

procedure InternalFinal;
begin
  if Assigned(ScreenContext) then ScreenContext.Free;
end;*)


{ TCDAppDelegate }

function TCDAppDelegate.applicationShouldTerminate(sender: NSApplication): NSApplicationTerminateReply;
begin
  Result := NSTerminateNow;
end;

{ TCDTimerObject }

procedure TCDTimerObject.timerEvent;
begin
  if Assigned(@func) then func;
end;

class function TCDTimerObject.initWithFunc(afunc: TWSTimerProc): TCDTimerObject;
begin
  Result:=alloc;
  Result.func:=afunc;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.RawImage_DescriptionFromCarbonBitmap

  Creates a rawimage description for a carbonbitmap
 ------------------------------------------------------------------------------}
(*function TCDWidgetSet.RawImage_DescriptionFromCocoaBitmap(out ADesc: TRawImageDescription; ABitmap: TCDBitmap): Boolean;
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

{  ADesc.Width := CGImageGetWidth(ABitmap.CGImage);
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

  Result := True;}
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.RawImage_FromCarbonBitmap

  Creates a rawimage description for a carbonbitmap
 ------------------------------------------------------------------------------}
function TCDWidgetSet.RawImage_FromCocoaBitmap(out ARawImage: TRawImage; ABitmap, AMask: TCDBitmap; ARect: PRect = nil): Boolean;
begin
  FillChar(ARawImage, SizeOf(ARawImage), 0);
  RawImage_DescriptionFromCocoaBitmap(ARawImage.Description, ABitmap);

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

function TCDWidgetSet.RawImage_DescriptionToBitmapType(
  ADesc: TRawImageDescription;
  out bmpType: TCDBitmapType): Boolean;
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
end;*)


