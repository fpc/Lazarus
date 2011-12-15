{$MainForm customdrawnwsforms.pp}

// redefines:
function XmbLookupString(p1: PXIC; ev: PXKeyPressedEvent; str: PChar; len: longword; ks: PKeySym; stat: PStatus): longint; cdecl; external;
function Xutf8LookupString(p1: PXIC; ev: PXKeyPressedEvent; str: PChar; len: longword; ks: PKeySym; stat: PStatus): longint; cdecl; external;

{ TCDWSCustomForm }

class procedure TCDWSCustomForm.UpdateMotifWMHints(const AWinControl: TWinControl; CanMaximize: Boolean);
type
  PMotifWmHints = ^TMotifWmHints;
  TMotifWmHints = packed record
    Flags, Functions, Decorations: LongWord;
    InputMode: LongInt;
    Status: LongWord;
  end;
const
  MWM_HINTS_FUNCTIONS = 1;
  MWM_HINTS_DECORATIONS = 2;
  FuncAll = 1;
  FuncResize = 2;
  FuncMove = 4;
  FuncMinimize = 8;
  FuncMaximize = 16;
  FuncClose = 32;
  DecorAll = 1;
  DecorBorder = 2;
  DecorResizeH = 4;
  DecorTitle = 8;
  DecorMenu = 16;
  DecorMinimize = 32;
  DecorMaximize = 64;
var
  PropType: TAtom;
  PropFormat: LongInt;
  PropItemCount, PropBytesAfter: LongWord;
  Hints: PMotifWmHints;
  NewHints: TMotifWmHints;
  lWindow: TWindow;
  lWindowInfo: TX11WindowInfo;
begin
  lWindowInfo := TX11WindowInfo(AWinControl.Handle);
  lWindow := lWindowInfo.Window;

  if CDWidgetSet.FWMHints = 0 then
    CDWidgetSet.FWMHints :=
      XInternAtom(CDWidgetSet.FDisplay, '_MOTIF_WM_HINTS', False);

  XGetWindowProperty(CDWidgetSet.FDisplay, lWindow,
    CDWidgetSet.FWMHints, 0, 5, False, AnyPropertyType, @PropType,
    @PropFormat, @PropItemCount, @PropBytesAfter, @Hints);

  NewHints.Flags := MWM_HINTS_FUNCTIONS or MWM_HINTS_DECORATIONS;
  NewHints.Functions := FuncResize or FuncMove or FuncMinimize or FuncClose;

  if {(woToolWindow in WindowOptions) or (woWindow in WindowOptions) or
   (woPopup in WindowOptions)}True then
    NewHints.Decorations := DecorBorder or DecorTitle or DecorMenu or DecorMinimize
  else
    NewHints.Decorations := 0;
  if CanMaximize then
  begin
    NewHints.Functions := NewHints.Functions or FuncMaximize;
    NewHints.Decorations := NewHints.Decorations or DecorMaximize;
  end;

  if Assigned(Hints) then
  begin
    Hints^.Flags := Hints^.Flags or NewHints.Flags;
    Hints^.Decorations := NewHints.Decorations;
    Hints^.Functions := NewHints.Functions;
  end else
    Hints := @NewHints;

  XChangeProperty(CDWidgetSet.FDisplay, lWindow,
    CDWidgetSet.FWMHints, CDWidgetSet.FWMHints,
    32, PropModeReplace, Pointer(Hints), 5);
  if Hints <> @NewHints then
    XFree(Hints);
end;

class procedure TCDWSCustomForm.SetPosition(const AWinControl: TWinControl; const APosition: TPoint);
var
  Supplied: PtrInt;
  SizeHints: PXSizeHints;

  dx, dy: integer;
  lx, ly: integer;
  cw : PWindow;
  lWindow: TWindow;
  lWindowInfo: TX11WindowInfo;
begin
  lWindowInfo := TX11WindowInfo(AWinControl.Handle);
  lWindow := lWindowInfo.Window;

  if AWinControl.Parent = nil then
  begin
    {$Note This doesn't work yet. I want to position a new window relative to
      another window. Used for popup windows, like the TComboBox dropdown. }
    {$IFDEF DEBUG} writeln('SetPosition with no Parent'); {$ENDIF}
    lx := APosition.x;
    ly := APosition.y;

    XTranslateCoordinates(CDWidgetSet.FDisplay, lWindow,
        XDefaultRootWindow(CDWidgetSet.FDisplay),
        lx, ly, @dx, @dy, @cw);
    lx := dx;
    ly := dy;
  end
  else
  begin
    {$IFDEF DEBUG} writeln('SetPosition inside parent'); {$ENDIF}
    lx := APosition.x;
    ly := APosition.y;
  end;
  {$IFDEF DEBUG} Writeln(Format('was (%d,%d) and is now (%d,%d)', [APosition.x, APosition.y, lx, ly])); {$ENDIF}

  SizeHints := XAllocSizeHints;
  XGetWMNormalHints(CDWidgetSet.FDisplay, lWindow, SizeHints, @Supplied);
  SizeHints^.flags := SizeHints^.flags or PPosition;
  SizeHints^.x := lx;
  SizeHints^.y := ly;
  XSetWMNormalHints(CDWidgetSet.FDisplay, lWindow, SizeHints);
  XFree(SizeHints);
  XMoveWindow(CDWidgetSet.FDisplay, lWindow, lx, ly);
end;

class procedure TCDWSCustomForm.SetSize(const AWinControl: TWinControl; const ASize: TSize);
var
  ChangeMask: Cardinal;
  Changes: TXWindowChanges;
  lWindow: TWindow;
  lWindowInfo: TX11WindowInfo;
begin
  lWindowInfo := TX11WindowInfo(AWinControl.Handle);
  lWindow := lWindowInfo.Window;

  ChangeMask := CWWidth or CWHeight;
  Changes.Width := ASize.cx;
  Changes.Height := ASize.cy;

  if ChangeMask <> 0 then
    XConfigureWindow(CDWidgetSet.FDisplay, lWindow, ChangeMask, @Changes);
end;

class procedure TCDWSCustomForm.SetMinMaxSize(const AWinControl: TWinControl; const AMinSize, AMaxSize: TSize);
var
  Supplied: PtrInt;
  SizeHints: PXSizeHints;
  CanMaximize: Boolean;
  lWindow: TWindow;
  lWindowInfo: TX11WindowInfo;
begin
  lWindowInfo := TX11WindowInfo(AWinControl.Handle);
  lWindow := lWindowInfo.Window;

  CanMaximize := (AMaxSize.cx = 0) or (AMaxSize.cy = 0) or
    (AMaxSize.cx > AMinSize.cx) or (AMaxSize.cy > AMinSize.cy);
  UpdateMotifWMHints(AWinControl, CanMaximize);

  SizeHints := XAllocSizeHints;
  XGetWMNormalHints(CDWidgetSet.FDisplay, lWindow, SizeHints, @Supplied);
  with SizeHints^ do
  begin
    if (AMinSize.cx > 0) or (AMinSize.cy > 0) then
    begin
      flags := flags or PMinSize;
      min_width := AMinSize.cx;
      min_height := AMinSize.cy;
    end else
      flags := flags and not PMinSize;

    if (AMaxSize.cx > 0) or (AMaxSize.cy > 0) then
    begin
      flags := flags or PMaxSize;
      if AMaxSize.cx > 0 then
        max_width := AMaxSize.cx
      else
        max_width := 32767;
      if AMaxSize.cy > 0 then
        max_height := AMaxSize.cy
      else
        max_height := 32767;
    end else
      flags := flags and not PMaxSize;
  end;

  XSetWMNormalHints(CDWidgetSet.FDisplay, lWindow, SizeHints);
  XFree(SizeHints);
end;

class procedure TCDWSCustomForm.CreateX11Canvas(AWindowInfo: TX11WindowInfo);
var
  DummyWnd: PWindow;
  DummyInt: LongInt;
  GCValues: XLib.TXGCValues;
  FWidth, FHeight: Integer;
begin
  XGetGeometry(CDWidgetSet.FDisplay, AWindowInfo.Window, @DummyWnd, @DummyInt, @DummyInt,
    @FWidth, @FHeight, @DummyInt, @DummyInt);

  GCValues.graphics_exposures := 0;
  AWindowInfo.GC := XCreateGC(CDWidgetSet.FDisplay, AWindowInfo.Window, GCGraphicsExposures, @GCValues);
//  if not Assigned(GC) then
//    raise EX11Error.Create(SGCCreationFailed);

//  XSetLineAttributes(GFApplication.Handle, GC, 0,
//    LineSolid, CapNotLast, JoinMiter);

//  FFont := AFont;
//  FXftDraw := XftDrawCreate(CDWidgetSet.FDisplay, AWindowInfo.Window,
//    XDefaultVisual(CDWidgetSet.FDisplay, XDefaultScreen(CDWidgetSet.FDisplay)),
//    XDefaultColormap(CDWidgetSet.FDisplay, XDefaultScreen(CDWidgetSet.FDisplay)));

//  FRegion := XCreateRegion;
//  Resized(Width, Height);	// Set up a proper clipping region

  //

{  XGetWindowAttributes(CDWidgetSet.FDisplay, AWindowInfo.Window, @Attr);
  FVisual := Attr.Visual;

  case Attr.Depth of
    1: PixelFormat.FormatType := ftMono;
    4: PixelFormat.FormatType := ftPal4;
    8: PixelFormat.FormatType := ftPal8;
    16: PixelFormat.FormatType := ftRGB16;
    24: PixelFormat.FormatType := ftRGB24;
    32: PixelFormat.FormatType := ftRGB32;
    else
      raise EX11Error.CreateFmt(SWindowUnsupportedPixelFormat, [Attr.Depth]);
  end;

  if Attr.Depth >= 16 then
  begin
    PixelFormat.RedMask   := Visual^.red_mask;
    PixelFormat.GreenMask := Visual^.green_mask;
    PixelFormat.BlueMask  := Visual^.blue_mask;
  end;}
end;

(*
 * Error handling.
 */
static int ErrorFlag = 0;
  static int HandleXError( Display *dpy, XErrorEvent *event )
  {
      ErrorFlag = 1;
      return 0;
  *)

{
 There are 2 ways to put an image into a X11 Window which everyone uses, even OpenGL:

 XPutImage and XShmPutImage

 Because XPutImage is so slow as to be unusable, we will always try to use XShmPutImage
}
class procedure TCDWSCustomForm.DrawRawImageToGC(ARawImage: TRawImage;
  ADestWindowInfo: TX11WindowInfo; ADestX, ADestY, ADestWidth, ADestHeight: Integer);
var
  UsePutImage: Boolean = False;
  major, minor, ignore: cint;
  {$IFDEF VerboseCDPaintProfiler}
  lTimeStart: TDateTime;
  {$ENDIF}
  pixmaps: cint;
begin
  {$IFDEF VerboseCDPaintProfiler}
  lTimeStart := NowUTC();
  {$ENDIF}

  // First check if XShm is available
  UsePutImage := True;
{  if not XQueryExtension(CDWidgetSet.FDisplay, 'MIT-SHM', @ignore, @ignore, @ignore) then UsePutImage := True
  else if not XShmQueryVersion(CDWidgetSet.FDisplay, @major, @minor, @pixmaps) then UsePutImage := True;
  if pixmaps <> 2 then UsePutImage := True;}

  if UsePutImage then DrawRawImageToGC_XPutImage(ARawImage, ADestWindowInfo, ADestX, ADestY, ADestWidth, ADestHeight)
  else DrawRawImageToGC_XShmPutImage(ARawImage, ADestWindowInfo, ADestX, ADestY, ADestWidth, ADestHeight);

  {$IFDEF VerboseCDPaintProfiler}
  DebugLn(Format('[TCDWSCustomForm.DrawRawImageToGC] Paint duration: %d ms', [DateTimeToMilliseconds(NowUTC() - lTimeStart)]));
  {$ENDIF}
end;

// XShm tutorial:
// http://personales.mundivia.es/jap/xshm.htm
class function TCDWSCustomForm.alloc_xshm_image(dpy: PDisplay; vis: PVisual;
  width, height, depth: Integer; out shminfo: TXShmSegmentInfo): PXImage;
var
  img: XLib.PXImage;
  ctx: TGC;
begin
  Result := nil;

  {
  * We have to do a _lot_ of error checking here to be sure we can
  * really use the XSHM extension.  It seems different servers trigger
  * errors at different points if the extension won't work.  Therefore
  * we have to be very careful...
  }

  img := XShmCreateImage(dpy, vis, depth,
                        ZPixmap, nil, @shminfo,
                        width, height );
  if (img = nil) then
  begin
   DebugLn('XShmCreateImage failed!');
   Exit;
  end;

(*  shminfo.shmid := shmget( IPC_PRIVATE, img^.bytes_per_line
  		       * img->height, IPC_CREAT or 0777 );
  if (shminfo.shmid < 0) then
  begin
   DebugLn('error in shmget. alloc_back_buffer: Shared memory error (shmget), disabling.');
   XDestroyImage( img );
   //c->shm = 0;
   Exit;
  end;

  img^.data := shmat( shminfo.shmid, 0, 0 );
  shminfo.shmaddr := img^.data;
  (*   if (shminfo.shmaddr == (char * ) -1) {
    perror("alloc_back_buffer");
    XDestroyImage( img );
    img = NULL;
    printf("shmat failed\n");
    return NULL;
  }

  shminfo.readOnly = False;
  ErrorFlag = 0;
  XSetErrorHandler( HandleXError );
  // This may trigger the X protocol error we're ready to catch: */
  XShmAttach( dpy, &shminfo );
  XSync( dpy, False );

  if (ErrorFlag) {
    /* we are on a remote display, this error is normal, don't print it */
    XFlush( dpy );
    ErrorFlag = 0;
    XDestroyImage( img );
    shmdt( shminfo.shmaddr );
    shmctl( shminfo.shmid, IPC_RMID, 0 );
    return NULL;
  }

  shmctl( shminfo.shmid, IPC_RMID, 0 ); // nobody else needs it*)*)

(*#ifdef OPTIONAL_PART
   /* An error may still occur upon the first XShmPutImage.  So it's a */
   /* good idea to test it here.  However, we need a window to put the */
   /* image into, etc.... */
   gc = XCreateGC( dpy, window, 0, NULL );
   XShmPutImage( dpy, window, gc,
  	       img, 0, 0, 0, 0, 1, 1 /*one pixel*/, False );
   XSync( dpy, False );
   XFreeGC( dpy, gc );
   XSetErrorHandler( NULL );
   if (ErrorFlag) {
      XFlush( dpy );
      ErrorFlag = 0;
      XDestroyImage( img );
      shmdt( shminfo.shmaddr );
      shmctl( shminfo.shmid, IPC_RMID, 0 );
      return NULL;
   }
#endif*)

  Result := img;
end;

class procedure TCDWSCustomForm.destroy_xshm_image(img: PXImage; var shminfo: TXShmSegmentInfo);
begin
   XShmDetach(CDWidgetSet.FDisplay, @shminfo );
   XDestroyImage( img );
//   shmdt( shminfo.shmaddr );
end;

class procedure TCDWSCustomForm.DrawRawImageToGC_XShmPutImage(ARawImage: TRawImage;
  ADestWindowInfo: TX11WindowInfo; ADestX, ADestY, ADestWidth, ADestHeight: Integer);
var
  img: XLib.PXImage;
  shminfo: TXShmSegmentInfo;
begin
  // make shared XImage
  img := alloc_xshm_image(CDWidgetSet.FDisplay, ADestWindowInfo.Attr.visual, ADestWidth, ADestHeight, ADestWindowInfo.ColorDepth, shminfo);
  if (img = nil) then
  begin
    DebugLn('[TCDWSCustomForm.DrawRawImageToGC_XShmPutImage] couldn''t allocate shared XImage');
    Exit;
  end;

  // Now you can render into the img->data buffer
  // ???

  // Draw the image in the window
  XShmPutImage(CDWidgetSet.FDisplay, ADestWindowInfo.Window, ADestWindowInfo.GC,
    img, 0, 0, ADestX, ADestY, ADestWidth, ADestHeight, False);

  // Destroy image
  destroy_xshm_image(img, shminfo);
end;

class procedure TCDWSCustomForm.DrawRawImageToGC_XPutImage(ARawImage: TRawImage;
  ADestWindowInfo: TX11WindowInfo; ADestX, ADestY, ADestWidth, ADestHeight: Integer);
var
  Image: XLib.PXImage;
  XImage: XLib.TXImage;
  lScanlineSize: cint;
  lScreen: cint;
  lVisual: PVisual;
  lDepth: cint;
begin
  lScanlineSize := (ADestWindowInfo.Canvas.Width*ADestWindowInfo.ColorDepth) div 8;
  lScreen := DefaultScreen(CDWidgetSet.FDisplay);
  lVisual := {DefaultVisual(CDWidgetSet.FDisplay, lScreen); //} ADestWindowInfo.Attr.Visual;
  lDepth := ADestWindowInfo.ColorDepth;

  XImage.Width := ADestWidth;
  XImage.Height := ADestHeight;
  XImage.xoffset        := 0;
  XImage.obdata         := #0;
  XImage.byte_order     := LSBFirst;
  XImage.bitmap_bit_order := MSBFirst;
  XImage.bitmap_pad     := 32;
  XImage.bytes_per_line := 0;

  {     if acolordepth = 1 then
       begin
         format         := XYBitmap;
         bitmap_unit    := 8;
         depth          := 1;
         bits_per_pixel := 1;
         red_mask       := 1;
         green_mask     := 0;
         blue_mask      := 0;
       end
      else
    begin}
  XImage.format      := ZPixmap;
  XImage.bitmap_unit := 32;

  // only truecolor 24/32 displays supported now, otherwise color conversion required!
  // this must be match for the display !!!
  XImage.depth := ADestWindowInfo.ColorDepth;
  XImage.bits_per_pixel := 24;

  // Pixel mask
  XImage.red_mask   := lVisual^.red_mask;
  XImage.green_mask := lVisual^.green_mask;
  XImage.blue_mask  := lVisual^.blue_mask;

  XImage.Data := PChar(ARawImage.Data);

  XInitImage(@XImage);

// Create a native Image
// No idea why but using XCreateImage it ends up using a format which I have no idea which is,
// and the missmatch criples the image, XInitImage works fine
//  Image := XCreateImage(CDWidgetSet.FDisplay, lVisual,
//    lDepth, ZPixmap, 0, PChar(ARawImage.Data),
//    ADestWidth, ADestHeight, 8, 0);

  {$IFDEF VerboseCDForms}
    DebugLn(Format('[TCDWSCustomForm.DrawRawImageToGC_XPutImage] XImage=%x Data=%x'
      + ' ColorDepth:%d Width=%d Height=%d ScanlineSize=%d'
      + ' red_mask=%x green_mask=%x blue_mask=%x',
      [PtrInt(Image), PtrInt(ARawImage.Data), lDepth,
       ADestWidth, ADestHeight, lScanlineSize,
       XImage.red_mask, XImage.green_mask, XImage.blue_mask]));
  {$ENDIF}

  // Note that XPutImage is slow
  XPutImage(CDWidgetSet.FDisplay, ADestWindowInfo.Window, ADestWindowInfo.GC,
    @XImage, 0, 0, ADestX, ADestY, ADestWidth, ADestHeight);

  // Free the native image -> Used only together with XCreateImage
//  Image.data := nil;
//  XDestroyImage(Image);
end;

class procedure TCDWSCustomForm.EvKeyPressed(const AWinControl: TWinControl;
  AWindowInfo: TX11WindowInfo; var Event: TXKeyEvent);
var
  lKey: Word;
  lForm: TCDForm;
  KeySym: TKeySym;
begin
  lForm := TCDForm(AWinControl.Handle);
  KeySym := StartComposing(Event);

  lKey := X11KeyToLCLKey(KeySym);
  CallbackKeyDown(lForm, lKey);

  //if (FXEvent^.xkey.state and (ControlMask or Mod1Mask)) = 0 then EndComposing;
end;

class procedure TCDWSCustomForm.EvKeyReleased(const AWinControl: TWinControl;
  AWindowInfo: TX11WindowInfo; var Event: TXKeyEvent);
{var
  KeySym: TKeySym;}
begin
{  KeySym := StartComposing(FXEvent^);

  inherited EvKeyReleased(AKey);

  // Do not call EndComposing, as this would generate duplicate KeyChar events!}
end;

class procedure TCDWSCustomForm.EvMousePressed(const AWinControl: TWinControl; AWindowInfo: TX11WindowInfo;
  var Event: TXButtonEvent);
var
  MouseButton: TMouseButton;
begin
  if XButtonToMouseButton(Event.button, MouseButton) then
  begin
    CallbackMouseDown(TCDForm(AWinControl.Handle), Event.x, Event.y, MouseButton, []);
  end
  else
  begin
{   if Event.button = 4 then Sum := -1
   else Sum := 1;

   // Check for other mouse wheel messages in the queue
   while XCheckTypedWindowEvent(GFApplication.Handle,
    WindowEntry.Handle, X.ButtonPress, @NewEvent) do
   begin
   if NewEvent.xbutton.Button = 4 then Dec(Sum)
     else if NewEvent.xbutton.Button = 5 then Inc(Sum)
     else
     begin
       XPutBackEvent(GFApplication.Handle, @NewEvent);
       break;
     end;
   end;

   WindowEntry.EvMouseWheel(
    Sum, Point(XEvent.xbutton.x, XEvent.xbutton.y));}
  end;
end;

class procedure TCDWSCustomForm.EvMouseReleased(const AWinControl: TWinControl; AWindowInfo: TX11WindowInfo;
  var Event: TXButtonEvent);
var
  MouseButton: TMouseButton;
  lTarget: TWinControl;
  lEventPos: TPoint;
begin
  lTarget := AWindowInfo.LastMouseDownControl;
  if lTarget = nil then
    lTarget := FindControlWhichReceivedEvent(TCustomForm(AWinControl), AWindowInfo.Children, Event.x, Event.y);
  lEventPos := FormPosToControlPos(lTarget, Event.x, Event.y);

  { Release events are only for mouse buttons, and not mouse wheel moviments }
  if (Event.button >= 1) and (Event.button <= 3) then
  begin
    XButtonToMouseButton(Event.button, MouseButton);

    LCLSendMouseUpMsg(lTarget, lEventPos.x, lEventPos.y, MouseButton, []);
    LCLSendClickedMsg(lTarget);
  end;
end;

class procedure TCDWSCustomForm.EvMouseEnter(const AWinControl: TWinControl; AWindowInfo: TX11WindowInfo);
begin
end;

class procedure TCDWSCustomForm.EvMouseLeave(const AWinControl: TWinControl; AWindowInfo: TX11WindowInfo);
begin
end;

class procedure TCDWSCustomForm.EvMouseMove(const AWinControl: TWinControl; AWindowInfo: TX11WindowInfo;
  var Event: TXMotionEvent);
var
  lTarget: TWinControl;
  lEventPos: TPoint;
begin
  lTarget := FindControlWhichReceivedEvent(TCustomForm(AWinControl), AWindowInfo.Children, Event.x, Event.y);

  lEventPos := FormPosToControlPos(lTarget, Event.x, Event.y);

  LCLSendMouseMoveMsg(lTarget, lEventPos.x, lEventPos.y, []);
end;

// Top-level windows receive LM_ACTIVATE while constrols receive LM_SETFOCUS
// See http://blogs.msdn.com/b/jfoscoding/archive/2006/08/02/686141.aspx
// Here we have 1 handle per window, so the X11 backend should only send LM_ACTIVATE
class procedure TCDWSCustomForm.EvFocusIn(const AWinControl: TWinControl; AWindowInfo: TX11WindowInfo);
begin
  LCLSendActivateMsg(AWinControl, True, false);
end;

class procedure TCDWSCustomForm.EvFocusOut(const AWinControl: TWinControl; AWindowInfo: TX11WindowInfo);
begin
  LCLSendDeactivateStartMsg(AWinControl);
end;

class procedure TCDWSCustomForm.EvPaint(const AWinControl: TWinControl; AWindowInfo: TX11WindowInfo);
var
  lWidth, lHeight: Integer;
  lBitmap, lMask: HBITMAP;
  lRawImage: TRawImage;
  {$IFDEF VerboseCDPaintProfiler}
  lTimeStart, lCDTimeEnd, lNativeTimeStart: TDateTime;
  {$ENDIF}
begin
  {$IFDEF VerboseCDPaintProfiler}
  lTimeStart := NowUTC();
  {$ENDIF}
  {$IFDEF VerboseCDForms}
    DebugLn(Format('[TCDWSCustomForm.EvPaint] AWindowInfo: %x', [PtrInt(AWindowInfo)]));
  {$ENDIF}
  if (AWinControl = nil) or (AWindowInfo = nil) then Exit;

  lWidth := Round(AWinControl.width);
  lHeight := Round(AWinControl.height);

  // Prepare the non-native image and canvas
  {$IFDEF VerboseCDForms}
    DebugLn(Format('[TCDWSCustomForm.EvPaint] Visual: Red-Mask: %x Green-Mask: %x Blue-Mask: %x'
      + ' bits_per_rgb=%d c_class=%d',
      [AWindowInfo.Attr.visual^.red_mask, AWindowInfo.Attr.visual^.green_mask, AWindowInfo.Attr.visual^.blue_mask,
       AWindowInfo.Attr.Visual^.bits_per_rgb, AWindowInfo.Attr.Visual^.c_class]));
   { c_class values:
   StaticGray = 0;
   GrayScale = 1;
   StaticColor = 2;
   PseudoColor = 3;
   TrueColor = 4;
   DirectColor = 5;}
  {$ENDIF}
  UpdateControlLazImageAndCanvas(AWindowInfo.Image, AWindowInfo.Canvas, lWidth, lHeight, clfBGR24);

  RenderForm(AWindowInfo.Image, AWindowInfo.Canvas, TCustomForm(AWinControl));

  {$IFDEF VerboseCDPaintProfiler}
  lCDTimeEnd := NowUTC();
  lNativeTimeStart := NowUTC();
  {$ENDIF}

  // Now render it into the control
  AWindowInfo.Image.GetRawImage(lRawImage);
  DrawRawImageToGC(lRawImage, AWindowInfo, 0, 0, lWidth, lHeight);

  {$IFDEF VerboseCDPaintProfiler}
  DebugLn(Format('[TCDWSCustomForm.EvPaint] Total Paint duration: %d ms of this CustomDrawn: %d ms Native: %d',
    [DateTimeToMilliseconds(NowUTC() - lTimeStart),
     DateTimeToMilliseconds(lCDTimeEnd - lTimeStart),
     DateTimeToMilliseconds(NowUTC() - lNativeTimeStart)
    ]));
  {$ENDIF}
end;

class procedure TCDWSCustomForm.EvConfigureNotify(const AWinControl: TWinControl; AWindowInfo: TX11WindowInfo;
  var Event: TXConfigureEvent);
begin
  // Without this while, it will get stuck in a loop resizing endlessly
  while XCheckTypedWindowEvent(CDWidgetset.FDisplay, AWindowInfo.Window, X.ConfigureNotify, @Event) do;

  {$ifdef VerboseCDEvents}
  DebugLn(Format('LCL-CustomDrawn-X11: X11 event received: %s X=%d Y=%d', [GetXEventName(Event._type), Event.x, Event.y]));
  {$endif}

  // Move event
  if (Event.x <> AWinControl.Left) or (Event.y <> AWinControl.Top) then
  begin
    LCLSendMoveMsg(AWinControl, Event.x, Event.y);
  end;
  // Size event
  if (Event.Width <> AWinControl.Width) or (Event.Height <> AWinControl.Height) then
  begin
    //SIZENORMAL, SIZEICONIC, SIZEFULLSCREEN, SIZEZOOMSHOW, SIZEZOOMHIDE.
    LCLSendSizeMsg(AWinControl, Event.Width, Event.Height, SIZENORMAL);

    //    TX11Canvas(Canvas).Resized(ClientWidth, ClientHeight); seams unnecessary here
  end;
end;

class procedure TCDWSCustomForm.EvClientMessage(const AWinControl: TWinControl;
  AWindowInfo: TX11WindowInfo; var Event: TXClientMessageEvent);
var
  CanClose: Boolean;
begin
  if Event.message_type = CDWidgetset.FWMProtocols then
  begin
    if Event.Data.l[0] = CDWidgetset.FWMDeleteWindow then
    begin
      // Message results : 0 - do nothing, 1 - destroy window (felipe: is this comment correct? taken from lcl-cocoa)
      CanClose:=LCLSendCloseQueryMsg(AWinControl)>0;
      if {CanClose} True then // CanClose is returning false -> ToDo: find out why
      begin
        LCLSendCloseUpMsg(AWinControl);
        XDestroyWindow(CDWidgetset.FDisplay, AWinControl.Handle);
      end;
    end
    else
      DebugLn(Format('LCL-CustomDrawn-X11: Unknown client protocol message: %d', [Event.Data.l[0]]));
  end
  else
    DebugLn(Format('LCL-CustomDrawn-X11: Unknown client message: %d', [Event.message_type]));
end;

class function TCDWSCustomForm.StartComposing(const Event: TXKeyEvent): TKeySym;
var
  len: integer;
begin
  SetLength(CDWidgetset.ComposeBuffer, 20);
  // Xutf8LookupString returns the size of FComposeBuffer in bytes.
  len := Xutf8LookupString(CDWidgetset.InputContext, @Event, @CDWidgetset.ComposeBuffer[1],
      Length(CDWidgetset.ComposeBuffer), @Result, @CDWidgetset.ComposeStatus);
  SetLength(CDWidgetset.ComposeBuffer, len);
  // if overflow occured, then previous SetLength() would have fixed the buffer
  // size, so run Xutf8LookupString again to read correct value.
  if CDWidgetset.ComposeStatus = XBufferOverflow then
    Xutf8LookupString(CDWidgetset.InputContext, @Event, @CDWidgetset.ComposeBuffer[1],
      Length(CDWidgetset.ComposeBuffer), @Result, @CDWidgetset.ComposeStatus);
end;

class function TCDWSCustomForm.X11KeyToLCLKey(AX11Key: TKeySym): Word;
{const
  Table_20aX: array[$20a0..$20ac] of Word = (keyEcuSign, keyColonSign,
     keyCruzeiroSign, keyFFrancSign, keyLiraSign, keyMillSign, keyNairaSign,
     keyPesetaSign, keyRupeeSign, keyWonSign, keyNewSheqelSign, keyDongSign,
     keyEuroSign);
 Table_feXX: array[$fe50..$fe60] of Word = (keyDeadGrave, keyDeadAcute,
    keyDeadCircumflex, keyDeadTilde, keyDeadMacron,keyDeadBreve,
    keyDeadAbovedot, keyDeadDiaeresis, keyDeadRing, keyDeadDoubleacute,
    keyDeadCaron, keyDeadCedilla, keyDeadOgonek, keyDeadIota,
    keyDeadVoicedSound, keyDeadSemivoicedSound, keyDeadBelowdot);
 Table_ff5X: array[$ff50..$ff58] of Word = (keyHome, keyLeft, keyUp, keyRight,
    keyDown, keyPrior, keyNext, keyEnd, keyBegin);
 Table_ff6X: array[$ff60..$ff6b] of Word = (keySelect, keyPrintScreen,
     keyExecute, keyInsert, keyNIL, keyUndo, keyRedo, keyMenu, keyFind,
     keyCancel, keyHelp, keyBreak);
  Table_ff9X: array[$ff91..$ff9f] of Word = (keyPF1, keyPF2, keyPF3, keyPF4,
     keyP7, keyP4, keyP8, keyP6, keyP2, keyP9, keyP3, keyP1, keyP5, keyP0,
   keyPDecimal);
  Table_ffeX: array[$ffe1..$ffee] of Word = (keyShiftL, keyShiftR, keyCtrlL,
  keyCtrlR, keyCapsLock, keyShiftLock, keyMetaL, keyMetaR, keyAltL, keyAltR,
     keySuperL, keySuperR, keyHyperL, keyHyperR);  }
begin
 case AX11Key of
   $20: Result := VK_SPACE;
{ 0x0021   U0021   .   # exclam
 0x0022   U0022   .   # quotedbl
 0x0023   U0023   .   # numbersign
 0x0024   U0024   .   # dollar
 0x0025   U0025   .   # percent
 0x0026   U0026   .   # ampersand
 0x0027   U0027   .   # apostrophe
 0x0027   U0027   .   # quoteright	/* deprecated */
 0x0028   U0028   .   # parenleft
 0x0029   U0029   .   # parenright
 0x002a   U002a   .   # asterisk
 0x002b   U002b   .   # plus
 0x002c   U002c   .   # comma
 0x002d   U002d   .   # minus
 0x002e   U002e   .   # period
 0x002f   U002f   .   # slash
 0x0030   U0030   .   # 0
 0x0031   U0031   .   # 1
 0x0032   U0032   .   # 2
 0x0033   U0033   .   # 3
 0x0034   U0034   .   # 4
 0x0035   U0035   .   # 5
 0x0036   U0036   .   # 6
 0x0037   U0037   .   # 7
 0x0038   U0038   .   # 8
 0x0039   U0039   .   # 9
 0x003a   U003a   .   # colon
 0x003b   U003b   .   # semicolon
 0x003c   U003c   .   # less
 0x003d   U003d   .   # equal
 0x003e   U003e   .   # greater
 0x003f   U003f   .   # question
 0x0040   U0040   .   # at      }
    $41: Result := VK_A;
{ 0x0042   U0042   .   # B
 0x0043   U0043   .   # C
 0x0044   U0044   .   # D
 0x0045   U0045   .   # E
 0x0046   U0046   .   # F
 0x0047   U0047   .   # G
 0x0048   U0048   .   # H
 0x0049   U0049   .   # I
 0x004a   U004a   .   # J
 0x004b   U004b   .   # K
 0x004c   U004c   .   # L
 0x004d   U004d   .   # M
 0x004e   U004e   .   # N
 0x004f   U004f   .   # O
 0x0050   U0050   .   # P
 0x0051   U0051   .   # Q
 0x0052   U0052   .   # R
 0x0053   U0053   .   # S
 0x0054   U0054   .   # T
 0x0055   U0055   .   # U
 0x0056   U0056   .   # V
 0x0057   U0057   .   # W
 0x0058   U0058   .   # X
 0x0059   U0059   .   # Y
 0x005a   U005a   .   # Z     }
{ 0x005b   U005b   .   # bracketleft
 0x005c   U005c   .   # backslash
 0x005d   U005d   .   # bracketright
 0x005e   U005e   .   # asciicircum
 0x005f   U005f   .   # underscore
 0x0060   U0060   .   # grave
 0x0060   U0060   .   # quoteleft	/* deprecated */ }
    $61: Result := VK_A;
{ 0x0062   U0062   .   # b
 0x0063   U0063   .   # c
 0x0064   U0064   .   # d
 0x0065   U0065   .   # e
 0x0066   U0066   .   # f
 0x0067   U0067   .   # g
 0x0068   U0068   .   # h
 0x0069   U0069   .   # i
 0x006a   U006a   .   # j
 0x006b   U006b   .   # k
 0x006c   U006c   .   # l
 0x006d   U006d   .   # m
 0x006e   U006e   .   # n
 0x006f   U006f   .   # o
 0x0070   U0070   .   # p
 0x0071   U0071   .   # q
 0x0072   U0072   .   # r
 0x0073   U0073   .   # s
 0x0074   U0074   .   # t
 0x0075   U0075   .   # u
 0x0076   U0076   .   # v
 0x0077   U0077   .   # w
 0x0078   U0078   .   # x
 0x0079   U0079   .   # y
 0x007a   U007a   .   # z      }
 {   0..Ord('a')-1, Ord('z')+1..$bf, $f7:
 826       Result := KeySym;
 827     Ord('a')..Ord('z'), $c0..$f6, $f8..$ff:
 828       Result := KeySym - 32;  // ignore case: convert lowercase a-z to A-Z keysyms;
 829     $20a0..$20ac: Result := Table_20aX[KeySym];
 830     $fe20: Result := keyTab;
 831     $fe50..$fe60: Result := Table_feXX[KeySym];
 832     XK_BackSpace:   Result := keyBackspace;
 833     XK_Tab:         Result := keyTab;
 834     XK_Linefeed:    Result := keyLinefeed;
 835     $ff0b: Result := keyClear;
 836     $ff0d: Result := keyReturn;
 837     $ff13: Result := keyPause;
 838     $ff14: Result := keyScrollLock;
 839     $ff15: Result := keySysRq;
 840     $ff1b: Result := keyEscape;
 841     $ff50..$ff58: Result := Table_ff5X[KeySym];
 842     $ff60..$ff6b: Result := Table_ff6X[KeySym];
 843     $ff7e: Result := keyModeSwitch;
 844     $ff7f: Result := keyNumLock;
 845     $ff80: Result := keyPSpace;
 846     $ff89: Result := keyPTab;
 847     $ff8d: Result := keyPEnter;
 848     $ff91..$ff9f: Result := Table_ff9X[KeySym];
 849     $ffaa: Result := keyPAsterisk;
 850     $ffab: Result := keyPPlus;
 851     $ffac: Result := keyPSeparator;
 852     $ffad: Result := keyPMinus;
 853     $ffae: Result := keyPDecimal;
 854     $ffaf: Result := keyPSlash;
 855     $ffb0..$ffb9: Result := keyP0 + KeySym - $ffb0;
 856     $ffbd: Result := keyPEqual;
 857     $ffbe..$ffe0: Result := keyF1 + KeySym - $ffbe;
 858     $ffe1..$ffee: Result := Table_ffeX[KeySym];
 859     $ffff: Result := keyDelete;  }
 else
   Result := 0;
 end;

 (*864 {$IFDEF Debug}
 865   if Result = keyNIL then
 866     WriteLn('fpGFX/X11: Unknown KeySym: $', IntToHex(KeySym, 4));
 867 {$ENDIF}*)
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomForm.CreateHandle
  Params:  None
  Returns: Nothing

  Creates a Windows CE Form, initializes it according to it´s properties and shows it
 ------------------------------------------------------------------------------}
class function TCDWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
const
  WindowHints: TXWMHints = (
    flags: InputHint or StateHint or WindowGroupHint;
    input: 1;
    initial_state: NormalState;
    icon_pixmap: 0;
    icon_window: 0;
    icon_x: 0;
    icon_y: 0;
    icon_mask: 0;
    window_group: 0;
  );
var
  Colormap: TColormap;
  Attr: TXSetWindowAttributes;
  SizeHints: TXSizeHints;
  ClassHint: PXClassHint;
  lParentHandle: X.TWindow;
  mask: longword;
  lWindowInfo: TX11WindowInfo;
  lWindow: TWindow;
  AForm: TCustomForm absolute AWinControl;
begin
  {$ifdef VerboseCDForms}
  DebugLn(Format(':>[TCDWSCustomForm.CreateHandle] AWinControl=%x Name=%s: %s',
    [PtrInt(AWinControl), AWinControl.Name, AWinControl.ClassName]));
  {$endif}
  Colormap := XDefaultColormap(CDWidgetSet.FDisplay, XDefaultScreen(CDWidgetSet.FDisplay));
  Attr.Colormap := Colormap;

  SizeHints.flags     := XUtil.PSize;
  SizeHints.x         := 0; // If it doesnt start with zero, setting bounds later on fails, no idea why
  SizeHints.y         := 0;
  SizeHints.width     := 200;
  SizeHints.height    := 200;

  { Make sure we use the correct parent handle }
{  if FParent <> nil then
    lParentHandle := TX11Window(FParent).Handle
  else}
    lParentHandle := XDefaultRootWindow(CDWidgetSet.FDisplay);

  { setup attributes and masks }
  if (AForm.BorderStyle in [bsNone, bsToolWindow]) then
  begin
    Attr.Override_Redirect := 1;    // this removes window borders
    mask := CWOverrideRedirect;// or CWColormap;
  end
{  else if (woPopup in WindowOptions) then
  begin
    Attr.Override_Redirect := True;    // this removes window borders
    Attr.save_under := True;
    mask := CWOverrideRedirect or CWSaveUnder;
  end}
  else
  begin
    Attr.Override_Redirect := 0;
    mask := CWColormap;
  end;

  lWindow := XCreateWindow(
    CDWidgetSet.FDisplay,
    lParentHandle,                      // parent
    SizeHints.x, SizeHints.x,           // position (top, left)
    SizeHints.width, SizeHints.height,  // default size (width, height)
    0,                                  // border size
    CopyFromParent,                     // depth
    InputOutput,                        // class
    XDefaultVisual(CDWidgetSet.FDisplay, XDefaultScreen(CDWidgetSet.FDisplay)),  // visual
    mask,
    @Attr);

  if lWindow = 0 then
    raise Exception.Create('[TCDWSCustomForm.CreateHandle] Window creation failed');

  XSelectInput(CDWidgetSet.FDisplay, lWindow, KeyPressMask or KeyReleaseMask
    or ButtonPressMask or ButtonReleaseMask
    or EnterWindowMask or LeaveWindowMask
    or ButtonMotionMask or PointerMotionMask
    or ExposureMask
    or FocusChangeMask
    or StructureNotifyMask
//    or PropertyChangeMask
    );

//  if (not (woX11SkipWMHints in WindowOptions)) and (woWindow in WindowOptions) then
//  begin
    XSetStandardProperties(CDWidgetSet.FDisplay, lWindow, nil, nil, 0,
     argv, argc, @SizeHints);

    XSetWMNormalHints(CDWidgetSet.FDisplay, lWindow, @SizeHints);

    WindowHints.flags := WindowGroupHint;
    WindowHints.window_group := CDWidgetSet.LeaderWindow;
    XSetWMHints(CDWidgetSet.FDisplay, lWindow, @WindowHints);

    XChangeProperty(CDWidgetSet.FDisplay, lWindow, CDWidgetSet.ClientLeaderAtom, 33, 32,
     PropModeReplace, @CDWidgetSet.LeaderWindow, 1);

     // We want to get a Client Message when the user tries to close this window
    if CDWidgetSet.FWMProtocols = 0 then
     CDWidgetSet.FWMProtocols := XInternAtom(CDWidgetSet.FDisplay, 'WM_PROTOCOLS', False);
    if CDWidgetSet.FWMDeleteWindow = 0 then
     CDWidgetSet.FWMDeleteWindow := XInternAtom(CDWidgetSet.FDisplay, 'WM_DELETE_WINDOW', False);

     // send close event instead of quitting the whole application...
     XSetWMProtocols(CDWidgetSet.FDisplay, lWindow, @CDWidgetSet.FWMDeleteWindow, 1);
//   end;

  { Child windows do not appear until parent (lParentHandle) is mapped }
//  if FParent <> nil then
//    XMapSubwindows(CDWidgetSet.FDisplay, lParentHandle);

  // for modal windows, this is necessary
//  if (woModal in WindowOptions) then
//    XSetTransientForHint(GFApplication.Handle, Handle, Handle);

  // Add the window to the list of windows
  lWindowInfo := TX11WindowInfo.Create;
  lWindowInfo.Window := lWindow;
  lWindowInfo.LCLForm := TCustomForm(AWinControl);
  XGetWindowAttributes(CDWidgetSet.FDisplay, lWindow, @lWindowInfo.Attr);
  lWindowInfo.Colormap := XDefaultColormap(CDWidgetSet.FDisplay, XDefaultScreen(CDWidgetSet.FDisplay));
  lWindowInfo.ColorDepth := lWindowInfo.Attr.depth;
  CreateX11Canvas(lWindowInfo);
  Result := TLCLIntfHandle(lWindowInfo);

  {$ifdef VerboseCDForms}
  DebugLn(Format(':<[TCDWSCustomForm.CreateHandle] Result=%x',
    [Result]));
  {$endif}
end;

class procedure TCDWSCustomForm.DestroyHandle(const AWinControl: TWinControl);
begin

end;

class procedure TCDWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
 const ABorderIcons: TBorderIcons);
begin
end;

class procedure TCDWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
          const AFormBorderStyle: TFormBorderStyle);
begin
  RecreateWnd(AForm);
end;

class procedure TCDWSCustomForm.SetBounds(const AWinControl: TWinControl;
    const ALeft, ATop, AWidth, AHeight: Integer);
begin
  {$ifdef VerboseCDForms}
  DebugLn(Format('[TCDWSCustomForm.SetBounds] AWinControl=%x ALeft=%d ATop=%d AWidth=%d AHeight=%d',
    [PtrInt(AWinControl), ALeft, ATop, AWidth, AHeight]));
  {$endif}
  SetPosition(AWinControl, Point(ALeft, ATop));
  SetSize(AWinControl, Size(AWidth, AHeight));
end;

class procedure TCDWSCustomForm.SetIcon(const AForm: TCustomForm; const Small, Big: HICON);
begin
end;

class procedure TCDWSCustomForm.SetShowInTaskbar(const AForm: TCustomForm;
  const AValue: TShowInTaskbar);
begin
end;

class procedure TCDWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
end;

class procedure TCDWSCustomForm.ShowHide(const AWinControl: TWinControl);
var
  lIndex: Integer;
  lWindow: TWindow;
  lWindowInfo: TX11WindowInfo;
begin
  lWindowInfo := TX11WindowInfo(AWinControl.Handle);
  lWindow := lWindowInfo.Window;

  if AWinControl.Visible then
  begin
    {$ifdef VerboseCDForms}
    DebugLn(Format('[TCDWSCustomForm.ShowHide] Visible=True AWinControl=%x Handle=%x',
      [PtrInt(AWinControl), PtrInt(AWinControl.Handle)]));
    {$endif}
    XMapRaised(CDWidgetSet.FDisplay, lWindow);
  end
  else
  begin
    {$ifdef VerboseCDForms}
    DebugLn(Format('[TCDWSCustomForm.ShowHide] Visible=False AWinControl=%x', [PtrInt(AWinControl)]));
    {$endif}
    // Don't remove it here, wait for a X11 Destroy event
  end;
end;

class function TCDWSCustomForm.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  s: PChar;
  lWindow: TWindow;
  lWindowInfo: TX11WindowInfo;
begin
  lWindowInfo := TX11WindowInfo(AWinControl.Handle);
  lWindow := lWindowInfo.Window;

  XFetchName(CDWidgetSet.FDisplay, lWindow, @s);
  AText := s;
  XFree(s);
end;

class function TCDWSCustomForm.GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean;
var
  lText: string;
begin
  Result := GetText(AWinControl, lText);
  ALength := Length(lText);
end;

class procedure TCDWSCustomForm.SetText(const AWinControl: TWinControl; const AText: String);
var
  tp: TXTextProperty;
  lWindow: TWindow;
  lWindowInfo: TX11WindowInfo;
begin
  lWindowInfo := TX11WindowInfo(AWinControl.Handle);
  lWindow := lWindowInfo.Window;

  tp.value    := PCUChar(AText);
  tp.encoding := XA_WM_NAME;
  tp.format   := 8;
  tp.nitems   := UTF8Length(AText);

  XSetWMName(CDWidgetSet.FDisplay, lWindow, @tp);
  XStoreName(CDWidgetSet.FDisplay, lWindow, PChar(AText));
  XSetIconName(CDWidgetSet.FDisplay, lWindow, PChar(AText));
  XSetWMIconName(CDWidgetSet.FDisplay, lWindow, @tp);
end;

class function TCDWSCustomForm.GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
end;

class function TCDWSCustomForm.GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
end;


