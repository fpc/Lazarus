{%MainUnit qtint.pp}

function IsOldKDEInstallation: Boolean;
var
  Display: PDisplay;
  i: longint;
begin
  //if we are under KDE4, it's already recent one.
  Result := not ((GetEnvironmentVariable('KDE_SESSION_VERSION') = '4')
    or QX11Info_isCompositingManagerRunning);
  if Result then
    exit;

  //if we are under other WM KDE_SESSION_VERSION does not exist
  //so we must check for XOrg version.
  Display := QX11Info_display();
  i := XVendorRelease(Display);

  //X, XOrg between 5.0 - 7.04 are old ones and contains KDE-3.5
  //Fedora marks XOrg 7.XX as 1.XX, so we have to check that too.
  Result := ((i >= 50000000) and (i <= 70040000)) or
    ((i > 0) and (i <= 10400000));
end;

function IsCurrentDesktop(AWidget: QWidgetH): Boolean;
var
  Display: PDisplay;
  ScreenNum: Integer;
  RootWin: TWindow;
  WMAtom: TAtom;

  typeReturned: TAtom;
  formatReturned: Integer;
  nitemsReturned: PtrInt;
  unused: PtrInt;
  WidgetIndex, DesktopIndex: Pointer;
  WidgetWin: TWindow;
begin
  Result := True;
  if AWidget = nil then
    exit;
  Display := QX11Info_display();
  if Display = nil then
    exit;
  ScreenNum := QX11Info_appScreen();
  RootWin := XRootWindow(Display, ScreenNum);
  WMAtom := XInternAtom(Display,'_NET_WM_DESKTOP', True);
  WidgetWin := TWindow(QWidget_winId(AWidget));

  if (WMAtom > 0) and (WidgetWin <> 0) then
  begin
    WidgetIndex := nil;
    DesktopIndex := nil;
    // first get our desktop num (virtual desktop !)
    if XGetWindowProperty(Display, WidgetWin, WMAtom, 0, 4, False, XA_CARDINAL,
         @typeReturned, @formatReturned, @nitemsReturned,
         @unused, @WidgetIndex) = Success then
      begin
        if (typeReturned = XA_CARDINAL) and (formatReturned = 32) and
          (WidgetIndex <> nil) then
        begin
          // now get current active desktop index
          WMAtom := XInternAtom(Display,'_NET_CURRENT_DESKTOP', True);
          if XGetWindowProperty(Display, RootWin, WMAtom, 0, 4, False,
            XA_CARDINAL, @typeReturned, @formatReturned, @nitemsReturned,
            @unused, @DesktopIndex) = Success then
          begin
            if (typeReturned = XA_CARDINAL) and (formatReturned = 32) and
              (DesktopIndex <> nil) then
              Result := PtrUint(WidgetIndex^) = PtrUint(DesktopIndex^);
          end;
        end;

        if WidgetIndex <> nil then
          XFree(WidgetIndex);
        if DesktopIndex <> nil then
          XFree(DesktopIndex);
        WidgetIndex := nil;
        DesktopIndex := nil;
    end;
  end;
end;

function GetWindowManager: String;
{used to get window manager name, so we can handle different wm's behaviour
 eg. kde vs. gnome}
var
  Display: PDisplay;
  ScreenNum: Integer;
  RootWin: TWindow;
  WMAtom: TAtom;
  WMWindow: TWindow;

  typeReturned: TAtom;
  formatReturned: Integer;
  nitemsReturned: PtrInt;
  unused: PtrInt;
  data: Pointer;
begin
  Result := '';
  Display := QX11Info_display();

  if Display = nil then
    exit;

  ScreenNum := QX11Info_appScreen();
  RootWin := XRootWindow(Display, ScreenNum);
  WMAtom := XInternAtom(Display,'_NET_WM_DESKTOP', True);

  if WMAtom > 0 then
  begin
    WMAtom := XInternAtom(Display,'_NET_SUPPORTING_WM_CHECK', False);
    if WMAtom > 0 then
    begin
      data := nil;
      WMWindow := 0;
      if XGetWindowProperty(Display, RootWin, WMAtom, 0, 1024, False, XA_WINDOW,
        @typeReturned, @formatReturned, @nitemsReturned,
        @unused, @data) = Success then
        begin
          if (typeReturned = XA_WINDOW) and (formatReturned = 32) and
            (Data <> nil) then
          begin
            // this is our window manager window
            WMWindow := TWindow(Data^);
            XFree(Data);
            Data := nil;
          end;
          if WMWindow = 0 then
            exit;
          WMAtom := XInternAtom(Display,'UTF8_STRING', False);
          if XGetWindowProperty(Display, WMWindow,
            XInternAtom(Display,'_NET_WM_NAME', False), 0, 1024, False,
            WMAtom, @typeReturned, @formatReturned, @nitemsReturned,
            @unused, @data) = Success then
          begin
            if (typeReturned = WMAtom) and (formatReturned = 8) then
              Result := StrPas(Data);
            if Data <> nil then
              XFree(Data);
            Data := nil;
          end;
       end;
    end;
  end;
end;

procedure SetSkipX11Taskbar(Widget: QWidgetH; const ASkipTaskBar: Boolean);
const
  _NET_WM_STATE_REMOVE = 0;
  _NET_WM_STATE_ADD = 1;
  _NET_WM_STATE_TOGGLE = 2;
var
   Display: PDisplay;
   RootWin: TWindow;
   ScreenNum: Integer;
   XClient: TXClientMessageEvent;
   WMAtom: TAtom;
begin
  Display := QX11Info_display();

  if Display = nil then
    exit;
  ScreenNum := QX11Info_appScreen();
  RootWin := XRootWindow(Display, ScreenNum);

  // _NET_WM_STATE_SKIP_TASKBAR
  FillChar(XClient, SizeOf(XClient), 0);
  XClient._type := ClientMessage;
  XClient.window := QWidget_winId(Widget);
  WMAtom := XInternAtom(Display,'_NET_WM_STATE', False);
  XClient.message_type := WMAtom;
  XClient.format := 32;

  if ASkipTaskBar then
    Xclient.data.l[0] := _NET_WM_STATE_ADD
  else
    Xclient.data.l[0] := _NET_WM_STATE_REMOVE;
  WMAtom := XInternAtom(Display,'_NET_WM_STATE_SKIP_TASKBAR', True);
  Xclient.data.l[1] := WMAtom;
  Xclient.data.l[2] := 0;
  Xclient.data.l[3] := 0;
  Xclient.data.l[4] := 0;
  XSendEvent (Display, RootWin, False,
	      SubstructureRedirectMask or SubstructureNotifyMask,
	      @Xclient);
end;

