{$MainForm customdrawnwsforms.pp}

{ TCDWSCustomForm }

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
begin
  lWindow := TWindow(AWinControl.Handle);
  if AWinControl.Visible then
    XMapRaised(CDWidgetSet.FDisplay, lWindow);
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


