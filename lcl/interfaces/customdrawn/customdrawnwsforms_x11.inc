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

  ChangeMask := 0;

  if ASize.cx <> AWinControl.ClientWidth then
  begin
    ChangeMask := CWWidth;
    Changes.Width := ASize.cx;
  end;

  if ASize.cy <> AWinControl.ClientHeight then
  begin
    ChangeMask := ChangeMask or CWHeight;
    Changes.Height := ASize.cy;
  end;

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

class procedure TCDWSCustomForm.EvPaint(const AWinControl: TWinControl; AWindowInfo: TX11WindowInfo);
var
  struct : TPaintStruct;
  lWidth, lHeight: Integer;
  lBitmap, lMask: HBITMAP;
  lRawImage: TRawImage;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn(Format('[TCDWSCustomForm.EvPaint] AWindowInfo: %x', [PtrInt(AWindowInfo)]));
  {$ENDIF}
  if (AWinControl = nil) or (AWindowInfo = nil) then Exit;

  lWidth := Round(AWinControl.width);
  lHeight := Round(AWinControl.height);

  FillChar(struct, SizeOf(TPaintStruct), 0);

  // Prepare the non-native image and canvas
  UpdateControlLazImageAndCanvas(AWindowInfo.Image,
    AWindowInfo.Canvas, lWidth, lHeight);

  struct.hdc := HDC(AWindowInfo.Canvas);

  // Send the paint message to the LCL
  {$IFDEF VerboseWinAPI}
    DebugLn(Format('[TCDWSCustomForm.EvPaint] OnPaint event started context: %x', [struct.hdc]));
  {$ENDIF}
  LCLSendPaintMsg(AWinControl, struct.hdc, @struct);
  {$IFDEF VerboseWinAPI}
    DebugLn('[TCDWSCustomForm.EvPaint] OnPaint event ended');
  {$ENDIF}

  // Now render it into the control
  AWindowInfo.Image.GetRawImage(lRawImage);
  //Cocoa_RawImage_CreateBitmaps(lRawImage, lBitmap, lMask, True);
  //Context.DrawBitmap(0, 0, TCocoaBitmap(lBitmap));
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

  {$ifdef VerboseCDWindow}
  DebugLn(Format(':<[TCDWSCustomForm.CreateHandle] Result=%x',
    [Result]));
  {$endif}
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
    lWindowInfo := TX11WindowInfo.Create;
    lWindowInfo.Window := lWindow;
    lWindowInfo.LCLControl := AWinControl;
    CDWidgetset.WindowList.Add(lWindowInfo);
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


