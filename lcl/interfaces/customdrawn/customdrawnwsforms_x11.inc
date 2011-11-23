{$MainForm customdrawnwsforms.pp}

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
begin
  lWindow := TWindow(AWinControl.Handle);

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
begin
  lWindow := TWindow(AWinControl.Handle);

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
begin
  lWindow := TWindow(AWinControl.Handle);

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
  lWindow: TWindow;
  CanMaximize: Boolean;
begin
  lWindow := TWindow(AWinControl.Handle);

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

  {$IFDEF VerboseCDWindow}
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
{var
  KeySym: TKeySym;}
begin
{  KeySym := StartComposing(FXEvent^);

  inherited EvKeyPressed(AKey);

  if (FXEvent^.xkey.state and (ControlMask or Mod1Mask)) = 0 then EndComposing;}
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
    LCLSendMouseDownMsg(AWinControl, Event.x, Event.y, MouseButton, []);
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
begin
  { Release events are only for mouse buttons, and not mouse wheel moviments }
  if (Event.button >= 1) and (Event.button <= 3) then
  begin
    XButtonToMouseButton(Event.button, MouseButton);

    LCLSendMouseUpMsg(AWinControl, Event.x, Event.y, MouseButton, []);
    LCLSendClickedMsg(AWinControl);
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
begin
  LCLSendMouseMoveMsg(AWinControl, Event.x, Event.y, []);
end;

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
  struct : TPaintStruct;
  lWidth, lHeight: Integer;
  lBitmap, lMask: HBITMAP;
  lRawImage: TRawImage;
  {$IFDEF VerboseCDPaintProfiler}
  lTimeStart: TDateTime;
  {$ENDIF}
begin
  {$IFDEF VerboseCDPaintProfiler}
  lTimeStart := NowUTC();
  {$ENDIF}
  {$IFDEF VerboseCDWindow}
    DebugLn(Format('[TCDWSCustomForm.EvPaint] AWindowInfo: %x', [PtrInt(AWindowInfo)]));
  {$ENDIF}
  if (AWinControl = nil) or (AWindowInfo = nil) then Exit;

  lWidth := Round(AWinControl.width);
  lHeight := Round(AWinControl.height);

  FillChar(struct, SizeOf(TPaintStruct), 0);

  // Prepare the non-native image and canvas
  {$IFDEF VerboseCDWindow}
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

  struct.hdc := HDC(AWindowInfo.Canvas);

  // Send the paint message to the LCL
  {$IFDEF VerboseCDWindow}
    DebugLn(Format('[TCDWSCustomForm.EvPaint] OnPaint event started context: %x', [struct.hdc]));
  {$ENDIF}
  LCLSendPaintMsg(AWinControl, struct.hdc, @struct);
  {$IFDEF VerboseCDWindow}
    DebugLn('[TCDWSCustomForm.EvPaint] OnPaint event ended');
  {$ENDIF}

  // Now render it into the control
  AWindowInfo.Image.GetRawImage(lRawImage);
  DrawRawImageToGC(lRawImage, AWindowInfo, 0, 0, lWidth, lHeight);

  {$IFDEF VerboseCDPaintProfiler}
  DebugLn(Format('[TCDWSCustomForm.EvPaint] Paint duration: %d ms', [DateTimeToMilliseconds(NowUTC() - lTimeStart)]));
  {$ENDIF}
end;

class procedure TCDWSCustomForm.EvConfigureNotify(const AWinControl: TWinControl; AWindowInfo: TX11WindowInfo;
  var Event: TXConfigureEvent);
begin
  // Remove other repeated events
  //while XCheckTypedWindowEvent(CDWidget.FDisplay, AWindowInfo.Window, X.ConfigureNotify, @Event) do;

{  // Move event
  if (Event.x <> AWinControl.Left) or (Event.y <> AWinControl.Top) then
  begin
    LCLSendMoveMsg(AWinControl, Event.x, Event.y);
  end;
  // Size event
  if (Event.Width <> AWinControl.Width) or (Event.Height <> AWinControl.Height) then
  begin
    //SIZENORMAL, SIZEICONIC, SIZEFULLSCREEN, SIZEZOOMSHOW, SIZEZOOMHIDE.
    LCLSendSizeMsg(AWinControl, Event.Width, Event.Height, SIZENORMAL);
//    TX11Canvas(Canvas).Resized(ClientWidth, ClientHeight);
  end;}
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
  AForm: TCustomForm absolute AWinControl;
begin
  {$ifdef VerboseCDWindow}
  DebugLn(Format(':>[TCDWSCustomForm.CreateHandle] AWinControl=%x Name=%s: %s',
    [PtrInt(AWinControl), AWinControl.Name, AWinControl.ClassName]));
  {$endif}
  Colormap := XDefaultColormap(CDWidgetSet.FDisplay, XDefaultScreen(CDWidgetSet.FDisplay));
  Attr.Colormap := Colormap;

  SizeHints.flags     := XUtil.PSize;
  SizeHints.x         := 0;
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

  Result := XCreateWindow(
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

  if Result = 0 then
    raise Exception.Create('[TCDWSCustomForm.CreateHandle] Window creation failed');

  XSelectInput(CDWidgetSet.FDisplay, Result, KeyPressMask or KeyReleaseMask
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
    XSetStandardProperties(CDWidgetSet.FDisplay, Result, nil, nil, 0,
     argv, argc, @SizeHints);

    XSetWMNormalHints(CDWidgetSet.FDisplay, Result, @SizeHints);

    WindowHints.flags := WindowGroupHint;
    WindowHints.window_group := CDWidgetSet.LeaderWindow;
    XSetWMHints(CDWidgetSet.FDisplay, Result, @WindowHints);

    XChangeProperty(CDWidgetSet.FDisplay, Result, CDWidgetSet.ClientLeaderAtom, 33, 32,
     PropModeReplace, @CDWidgetSet.LeaderWindow, 1);

     // We want to get a Client Message when the user tries to close this window
    if CDWidgetSet.FWMProtocols = 0 then
     CDWidgetSet.FWMProtocols := XInternAtom(CDWidgetSet.FDisplay, 'WM_PROTOCOLS', False);
    if CDWidgetSet.FWMDeleteWindow = 0 then
     CDWidgetSet.FWMDeleteWindow := XInternAtom(CDWidgetSet.FDisplay, 'WM_DELETE_WINDOW', False);

     // send close event instead of quitting the whole application...
     XSetWMProtocols(CDWidgetSet.FDisplay, Result, @CDWidgetSet.FWMDeleteWindow, 1);
//   end;

  { Child windows do not appear until parent (lParentHandle) is mapped }
//  if FParent <> nil then
//    XMapSubwindows(CDWidgetSet.FDisplay, lParentHandle);

  // for modal windows, this is necessary
//  if (woModal in WindowOptions) then
//    XSetTransientForHint(GFApplication.Handle, Handle, Handle);

  // Add the window to the list of windows
  lWindowInfo := TX11WindowInfo.Create;
  lWindowInfo.Window := TWindow(Result);
  lWindowInfo.LCLControl := AWinControl;
  XGetWindowAttributes(CDWidgetSet.FDisplay, Result, @lWindowInfo.Attr);
  lWindowInfo.Colormap := XDefaultColormap(CDWidgetSet.FDisplay, XDefaultScreen(CDWidgetSet.FDisplay));
  lWindowInfo.ColorDepth := lWindowInfo.Attr.depth;
  CreateX11Canvas(lWindowInfo);
  CDWidgetset.WindowList.Add(lWindowInfo);

  {$ifdef VerboseCDWindow}
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
  {$ifdef VerboseCDWindow}
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
  lWindow: TWindow;
  lIndex: Integer;
  lWindowInfo: TX11WindowInfo;
begin
  lWindow := TWindow(AWinControl.Handle);
  if AWinControl.Visible then
  begin
    {$ifdef VerboseCDWindow}
    DebugLn(Format('[TCDWSCustomForm.ShowHide] Visible=True AWinControl=%x Handle=%x',
      [PtrInt(AWinControl), PtrInt(AWinControl.Handle)]));
    {$endif}
    XMapRaised(CDWidgetSet.FDisplay, lWindow);
  end
  else
  begin
    {$ifdef VerboseCDWindow}
    DebugLn(Format('[TCDWSCustomForm.ShowHide] Visible=False AWinControl=%x', [PtrInt(AWinControl)]));
    {$endif}
    // Don't remove it here, wait for a X11 Destroy event
  end;
end;

class function TCDWSCustomForm.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  s: PChar;
  lWindow: TWindow;
begin
  lWindow := TWindow(AWinControl.Handle);
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
begin
  lWindow := TWindow(AWinControl.Handle);

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


