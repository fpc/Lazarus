{%MainUnit qtint.pp}
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

