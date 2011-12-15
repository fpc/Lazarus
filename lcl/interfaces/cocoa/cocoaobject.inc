{%MainUnit cocoaint.pas}

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

{ TCocoaWidgetSet }

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppInit
  Params:  ScreenInfo

  Initialize Carbon Widget Set
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppInit');
  {$ENDIF}

  delegate:=TCocoaAppDelegate.alloc;

  { Creates the application NSApp object }
  FNsApp := NSApplication.sharedApplication;
  FNSApp.setDelegate(delegate);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppRun
  Params:  ALoop
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppRun');
  {$ENDIF}

  { Enters main message loop }
  NSApp.run;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppProcessMessages

  Handle all pending messages
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppProcessMessages;
var
  event : NSEvent;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppProcessMessages');
  {$ENDIF}

  event:=NSApp.nextEventMatchingMask_untilDate_inMode_dequeue(NSAnyEventMask, nil, NSDefaultRunLoopMode, true);
  NSApp.sendEvent(event);

  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppProcessMessages END');
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppWaitMessage

  Passes execution control to Cocoa
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppWaitMessage;
var
  event : NSEvent;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppWaitMessage');
  {$ENDIF}
  event:=NSApp.nextEventMatchingMask_untilDate_inMode_dequeue(NSAnyEventMask, NSDate.distantFuture, NSDefaultRunLoopMode, true);
  NSApp.sendEvent(event);
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

  {  Creates the AutoreleasePool }
  pool := NSAutoreleasePool(NSAutoreleasePool.alloc).init;
  InitStockItems;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.Destroy

  Destructor for the class
 ------------------------------------------------------------------------------}
destructor TCocoaWidgetSet.Destroy;
begin
  inherited Destroy;

  FreeSysColorBrushes;
  FreeStockItems;

  CocoaWidgetSet := nil;

  {  Releases the AutoreleasePool }
  pool.release;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppTerminate

  Tells Carbon to halt the application
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppTerminate;
begin
  if FTerminating then Exit;
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppTerminate');
  {$ENDIF}

  NSApp.terminate(nil);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppMinimize

  Minimizes the whole application to the taskbar
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppMinimize;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppMinimize');
  {$ENDIF}
  NSApp.miniaturizeAll(nil);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppRestore

  Restores the whole minimized application from the taskbar
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppRestore;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppRestore');
  {$ENDIF}
  NSApp.activateIgnoringOtherApps(False);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppBringToFront

  Brings the entire application on top of all other non-topmost programs
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppBringToFront;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppBringToFront');
  {$ENDIF}
  NSApp.activateIgnoringOtherApps(True);
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
var
  ns: NSString;
begin
  if not Assigned(NSApp.dockTile) then Exit;
  //todo: setBadgeLabel is for 10.5 only, should be removed
  if NSApp.dockTile.respondsToSelector_(objcselector('setBadgeLabel:')) then
  begin
    ns := NSStringUtf8(ATitle);
    NSApp.dockTile.setBadgeLabel(NSStringUtf8(ATitle));
    ns.release;
  end;
end;

function TCocoaWidgetSet.GetLCLCapability(ACapability: TLCLCapability): PtrUInt;
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

function TCocoaWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): THandle;
var
  timer : NSTimer;
  user  : TCocoaTimerObject;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.CreateTimer');
  {$ENDIF}
  user:=TCocoaTimerObject.initWithFunc(TimerFunc);

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

procedure TCocoaWidgetSet.InitStockItems;
var
  LogBrush: TLogBrush;
  logPen : TLogPen;
begin
  FillChar(LogBrush,SizeOf(TLogBrush),0);
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

{  LogPen.lopnStyle := PS_NULL;
  LogPen.lopnWidth := Point(0, 0); // create cosmetic pens
  LogPen.lopnColor := $FFFFFF;
  FStockNullPen := CreatePenIndirect(LogPen);
  TQtPen(FStockNullPen).FShared := True;

  LogPen.lopnStyle := PS_SOLID;
  FStockWhitePen := CreatePenIndirect(LogPen);
  TQtPen(FStockWhitePen).FShared := True;

  LogPen.lopnColor := $000000;
  FStockBlackPen := CreatePenIndirect(LogPen);
  TQtPen(FStockBlackPen).FShared := True;
  }
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

function TCocoaWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
begin
  Result:=0;
end;

procedure TCocoaWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
begin

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

procedure InternalInit;
begin
end;

procedure InternalFinal;
begin
  if Assigned(ScreenContext) then ScreenContext.Free;
end;


{ TCocoaAppDelegate }

function TCocoaAppDelegate.applicationShouldTerminate(sender: NSApplication): NSApplicationTerminateReply;
begin
  Result := NSTerminateNow;
end;

{ TCocoaTimerObject }

procedure TCocoaTimerObject.timerEvent;
begin
  if Assigned(@func) then func;
end;

class function TCocoaTimerObject.initWithFunc(afunc: TWSTimerProc): TCocoaTimerObject;
begin
  Result:=alloc;
  Result.func:=afunc;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.RawImage_DescriptionFromCarbonBitmap

  Creates a rawimage description for a carbonbitmap
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.RawImage_DescriptionFromCocoaBitmap(out ADesc: TRawImageDescription; ABitmap: TCocoaBitmap): Boolean;
var
  Prec, Shift, BPR: Byte;
begin
  ADesc.Init;

  case ABitmap.BitmapType of
    cbtMono, cbtGray: ADesc.Format := ricfGray;
  else
    ADesc.Format := ricfRGBA;
  end;

  with ABitmap.image.size do
  begin
    ADesc.Width := Round(width);
    ADesc.Height := Round(Height);
  end;

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
  if ADesc.Format = ricfGray then Exit;

  // alpha
  if ABitmap.BitmapType in [cbtARGB, cbtRGBA, cbtBGRA] then
    ADesc.AlphaPrec := Prec;

  case ABitmap.BitmapType of
    cbtRGB: begin
      Shift := 32 - Prec;
      ADesc.RedShift := Shift;
      Dec(Shift, Prec);
      ADesc.GreenShift := Shift;
      Dec(Shift, Prec);
      ADesc.BlueShift := Shift;
    end;
    cbtBGR: begin
      Shift := 32 - Prec;
      ADesc.BlueShift := Shift;
      Dec(Shift, Prec);
      ADesc.GreenShift := Shift;
      Dec(Shift, Prec);
      ADesc.RedShift := Shift;
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
    cbtBGRA: begin
      Shift := 32 - Prec;
      ADesc.BlueShift := Shift;
      Dec(Shift, Prec);
      ADesc.GreenShift := Shift;
      Dec(Shift, Prec);
      ADesc.RedShift := Shift;
      Dec(Shift, Prec);
      ADesc.AlphaShift := Shift;
    end;
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.RawImage_FromCarbonBitmap

  Creates a rawimage description for a carbonbitmap
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.RawImage_FromCocoaBitmap(out ARawImage: TRawImage; ABitmap, AMask: TCocoaBitmap; ARect: PRect = nil): Boolean;
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

