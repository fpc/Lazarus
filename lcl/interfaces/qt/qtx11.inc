function IsCurrentDesktop(AWidget: QWidgetH): Boolean;
var
  Display: PDisplay;
  ScreenNum: Integer;
  RootWin: TWindow;
  WMAtom: TAtom;

  typeReturned: TAtom;
  formatReturned: Integer;
  nitemsReturned: LongWord;
  unused: LongWord;
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

