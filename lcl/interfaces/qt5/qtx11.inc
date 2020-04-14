{%MainUnit qtint.pp}

{helper function ,we don't have QX11Info::isCompositingManagerRunning class in Qt5}
function QX11Info_isCompositingManagerRunning: boolean;
var
  XDisplay: PDisplay;
  WMAtom: TAtom;
begin
  if IsWayland then
    exit(False);
  Result := QX11Info_isPlatformX11();
  if not Result then
    exit;
  XDisplay := QX11Info_display;
  WMAtom := XInternAtom(XDisplay,'_NET_WM_CM_S0', False);
  if WMAtom > 0 then
    Result := XGetSelectionOwner(XDisplay, WMAtom) <> 0;
end;

function IsWayland: Boolean;
var
  APlatform: WideString;
begin
  Result := False;
  QGuiApplication_platformName(@APlatform);
  Result := APlatform = 'wayland';
  // not QX11Info_isPlatformX11() and (GetEnvironmentVariable('XDG_SESSION_TYPE') = 'wayland');
end;

function IsOldKDEInstallation: Boolean;
var
  Display: PDisplay;
  i: longint;
begin
  if IsWayland then
    exit(False);
  //if we are under KDE4, it's already recent one.
  Result := not ((GetEnvironmentVariable('KDE_SESSION_VERSION') = '4') or
    (GetEnvironmentVariable('KDE_SESSION_VERSION') = '5')
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

function GetKdeSessionVersion: integer;
var
  S: String;
begin
  S := GetEnvironmentVariable('KDE_SESSION_VERSION');
  Result := StrToIntDef(S, -1);
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
  if (AWidget = nil) or IsWayland then
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

function X11Raise(AHandle: PtrUInt): boolean;
var
   Display: PDisplay;
   RootWin: TWindow;
   ScreenNum: Integer;
   XClient: TXClientMessageEvent;
   WMAtom: TAtom;
begin
  Result := False;
  if IsWayland then
    exit;
  Display := QX11Info_display();

  if Display = nil then
    exit;

  ScreenNum := QX11Info_appScreen();
  RootWin := XRootWindow(Display, ScreenNum);

  XClient._type := ClientMessage;
  XClient.window := AHandle;
  WMAtom := XInternAtom(Display,'_NET_ACTIVE_WINDOW', False);
  XClient.message_type := WMATom;
  XClient.format := 32;
  XClient.data.l[0] := 1;
  XClient.data.l[1] := CurrentTime; // _NET_WM_USER_TIME
  XClient.data.l[2] := 0;
  Result := XSendEvent(Display, RootWin, False,
	      SubstructureRedirectMask or SubstructureNotifyMask,
	      @XClient) <> Success;
end;

function X11GetActiveWindow: QWidgetH;
var
  Display: PDisplay;
  RootWin: TWindow;
  ScreenNum: Integer;
  WMAtom: TAtom;
  ActualTypeReturn: TAtom;
  ActualFormatReturn: LongInt;
  NItemsReturn, BytesAfterReturn: Cardinal;
  Ptr: Pointer;
  Valid: Boolean;
begin
  Result := nil;
  if IsWayland then
  begin
    Result := QApplication_activeWindow();
    exit;
  end;
  Display := QX11Info_display();

  if Display = nil then
    exit;
  Ptr := nil;
  ScreenNum := QX11Info_appScreen();
  RootWin := XRootWindow(Display, ScreenNum);
  WMAtom := XInternAtom(Display,'_NET_ACTIVE_WINDOW', False);
  Valid := XGetWindowProperty(Display, RootWin, WMAtom, 0, 1, False,
                                 AnyPropertyType, @ActualTypeReturn,
                                 @ActualFormatReturn, @NItemsReturn,
                                 @BytesAfterReturn, @Ptr) = Success;
  if Valid and (Ptr <> nil) and (ActualTypeReturn = XA_WINDOW) and
    (ActualFormatReturn = 32) then
  begin
    RootWin := TWindow(Ptr^);
    try
     Result := QWidget_find(RootWin);
    finally
      XFree(Ptr);
    end;
  end else
  if Assigned(Ptr) then
    XFree(Ptr);
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

  if IsWayland then
  begin
    Result := 'wayland';
    exit;
  end;

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
          if WMWindow > 0 then
          begin
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
  if Result = '' then
  begin
    // usually older window managers does not support _NET_SUPPORTING_WM_CHECK
    Result := GetEnvironmentVariableUTF8('XDG_SESSION_DESKTOP');
    if Result = '' then
      Result := GetEnvironmentVariableUTF8('DESKTOP_SESSION');
  end;
end;

function SetTransientForHint(Widget: QWidgetH; ATransientWin: QWidgetH): boolean;
var
   Display: PDisplay;
   AWin: PtrUInt;
begin
  Result := False;
  if (Widget = nil) or IsWayland then
    exit;
  Display := QX11Info_display();

  if Display = nil then
    exit;
  if ATransientWin <> nil then
    AWin := QWidget_winID(ATransientWin)
  else
    AWin := 0;
  Result := XSetTransientForHint(Display, QWidget_winId(Widget), AWin) = 1;
end;

procedure SetSkipX11Taskbar(Widget: QWidgetH; const ASkipTaskBar: Boolean);
const
  _NET_WM_STATE_REMOVE = 0;
  _NET_WM_STATE_ADD = 1;
  {_NET_WM_STATE_TOGGLE = 2;}
var
   Display: PDisplay;
   RootWin: TWindow;
   ScreenNum: Integer;
   XClient: TXClientMessageEvent;
   WMAtom: TAtom;
begin
  if IsWayland then
    exit;
  Display := QX11Info_display();

  if Display = nil then
    exit;
  ScreenNum := QX11Info_appScreen();
  RootWin := XRootWindow(Display, ScreenNum);

  // _NET_WM_STATE_SKIP_TASKBAR
  XClient.format := 0; // shutup compiler
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

function GetAlwaysOnTopX11(Widget: QWidgetH): boolean;
var
  Display: PDisplay;
  X11Window: TWindow;
  WMAtom: TAtom;
  typeReturned: TAtom;
  formatReturned: Integer;
  nitemsReturned: PtrInt;
  unused: PtrInt;
  data: Pointer;
begin
  Result := False;
  if IsWayland then
    exit;
  Display := QX11Info_display();

  if Display = nil then
    exit;

  X11Window := TWindow(QWidget_winId(Widget));
  if X11Window = 0 then
    exit;

  WMAtom := XInternAtom(Display,'_NET_WM_STATE', False);
  if WMAtom > 0 then
  begin
    data := nil;
    if XGetWindowProperty(Display, X11Window, WMAtom, 0, 1024, False, XA_ATOM,
      @typeReturned, @formatReturned, @nitemsReturned,
      @unused, @data) = Success then
    begin
      if (typeReturned = XA_ATOM) and (formatReturned = 32) and
        (Data <> nil) then
      begin
        while nitemsReturned > 0 do
        begin
          // make happy ancient x11 or old kde ?
          if XInternAtom(Display,'_NET_WM_STATE_STAYS_ON_TOP', False) = TAtom(Data^) then
            Result := True
          else
          if XInternAtom(Display,'_NET_WM_STATE_ABOVE', False) = TAtom(Data^) then
            Result := True;
          dec(nItemsReturned);
          if Result or (nItemsReturned = 0) then
            break;
          inc(Data);
        end;
        if nitemsReturned > 0 then
          XFree(Data);
        Data := nil;
      end;
    end;
  end;
end;

function GetKeyLockState(const AKey: Byte): Boolean;
var
  Display: PDisplay;
  n: Cardinal;
begin
  Result := False;
  if IsWayland then
    exit;
  Display := QX11Info_display();

  if (Display = nil) then
    exit;

  if (XkbGetIndicatorState(Display, XkbUseCoreKbd, @n) = Success) then
  begin
    if (AKey = VK_LCL_CAPSLOCK) then
      Result := (n and $01) = 1
    else
    if (AKey = VK_NUMLOCK) then
      Result := (n and $02) = 2;
  end;
end;

procedure MapX11Window(AWinID: PtrUInt);
begin
  if IsWayland then
    exit;
  XMapWindow(QX11Info_display(), TWindow(AWinID));
end;

{$IFDEF QtUseAccurateFrame}

function GetX11WindowRealized(AWinID: PtrUInt): boolean;
var
  d: PDisplay;
  AXWinAttr: TXWindowAttributes;
begin
  Result := False;
  d := QX11Info_display;
  XGetWindowAttributes(d, TWindow(AWinID), @AXWinAttr);
  Result := (AXWinAttr.map_installed > 0) and (AXWinAttr.map_state = 2);
end;

function GetX11WindowAttributes(AWinID: PtrUInt; out ALeft, ATop, AWidth, AHeight, ABorder: integer): boolean;
var
  d: PDisplay;
  Attr: TXWindowAttributes;
begin
  Result := False;
  d := QX11Info_display;
  ALeft := MAXINT;
  ATop := MAXINT;
  AWidth := MAXINT;
  AHeight := MAXINT;
  ABorder := MAXINT;
  XGetWindowAttributes(d, TWindow(AWinID), @Attr);
  Result := (Attr.map_installed > 0) and (Attr.map_state = 2);
  if Result then
  begin
    ALeft := Attr.x;
    ATop := Attr.y;
    AWidth := Attr.width;
    AHeight := Attr.height;
    ABorder := Attr.border_width;
  end;
end;

function GetX11WindowPos(AWinID: PtrUInt; out ALeft, ATop: integer): boolean;
var
  D: PDisplay;
  ARootWindow, AChildWin: TWindow;
  AXWinAttr: TXWindowAttributes;
begin
  Result := False;
  ALeft := 0;
  ATop := 0;
  D := QX11Info_display;
  if D = nil then
    exit;
  ARootWindow := XRootWindow(D, QX11Info_appScreen);
  XTranslateCoordinates(d, TWindow(AWinID), ARootWindow, 0, 0, @ALeft, @ATop, @AChildWin);
  XGetWindowAttributes(d, TWindow(AWinID), @AXWinAttr);
  ALeft := ALeft - AXWinAttr.x;
  ATop := ATop - AXWinAttr.y;
  Result := (AXWinAttr.map_installed > 0) and (AXWinAttr.map_state = 2);
end;

function SetX11WindowPos(AWinID: PtrUInt; const ALeft, ATop: integer): boolean;
var
  D: PDisplay;
  ARootWindow, AChildWin: TWindow;
  AResult: integer;
begin
  Result := False;
  D := QX11Info_display;
  if D = nil then
    exit;
  Result := XMoveWindow(d, AWinID, ALeft, ATop) <> BadWindow;
end;

var
  InternalExtentsAtom: TAtom = 0; {find extents atom}
  InternalExtentsCounter: Integer = 0; {counter for bad wm, eg for blackbox}

function propNotifyFrameExtents({%H-}ADisplay: PDisplay; AEvent:PXEvent;
  AData : TXPointer): TBoolResult;cdecl;
var
  AWin: PPtrUInt;
begin
  Result := False;
  AWin := PPtrUInt(AData);
  if (AEvent^.xany.window = AWin^) then
    inc(InternalExtentsCounter);
  if (AEvent^.xany._type = PropertyNotify) and (AEvent^.xany.window = AWin^) then
  begin
    if (AEvent^.xproperty.atom = InternalExtentsAtom) then
      Result := True;
  end;
  if InternalExtentsCounter > 50 then
  begin
    Result := True;
    InternalExtentsCounter := -BadImplementation;
  end;
end;

function AskX11_NET_REQUEST_FRAME_EXTENTS(AWinID: PtrUInt; out AMargins: TRect): boolean;
var
  AAtom: TAtom;
  AEvent: TXEvent;
  d: PDisplay;
  AScreen: Integer;
  ARootWindow: TWindow;
  ReturnEvent: TXEvent;
begin
  Result := False;
  AMargins := Rect(0, 0, 0, 0);
  d := QX11Info_display;

  AAtom := XInternAtom(d, '_NET_REQUEST_FRAME_EXTENTS', True);

  if AAtom = None then
    AAtom := XInternAtom(d, '_NET_REQUEST_FRAME_EXTENTS', False);

  if AAtom > 0 then
  begin
    if InternalExtentsAtom = 0 then
      InternalExtentsAtom := XInternAtom(d,'_NET_FRAME_EXTENTS', True);
    if InternalExtentsAtom > 0 then
    begin
      AScreen := QX11Info_appScreen();
      ARootWindow := XRootWindow(d, AScreen);
      AEvent._type := ClientMessage;
      AEvent.xclient._type := ClientMessage;
      AEvent.xclient.message_type := AAtom; // extents_request_atom;
      AEvent.xclient.display := d;
      AEvent.xclient.window := AWinID;
      AEvent.xclient.format := 32;
      AEvent.xclient.data.l[0] := 0;
      AEvent.xclient.data.l[1] := 0;
      AEvent.xclient.data.l[2] := 0;
      AEvent.xclient.data.l[3] := 0;
      AEvent.xclient.data.l[4] := 0;


      XSendEvent (d, ARootWindow, False, SubstructureRedirectMask or
        SubstructureNotifyMask, @AEvent);

      InternalExtentsCounter := 0;

      if XIfEvent(d, @ReturnEvent,
        @propNotifyFrameExtents, TXPointer(@AWinID)) = Success then
      begin
        Result := GetX11RectForAtom(AWinID, '_NET_FRAME_EXTENTS', AMargins);
        if not Result and (InternalExtentsCounter = -BadImplementation) then
        begin
          DebugLn('QtLCL WARNING: "'+GetWindowManager+'" wm bad implementation of _NET_REQUEST_FRAME_EXTENTS.');
          Result := True;
          AMargins := Rect(2, 27, 2, 2);
        end;
      end;
    end else
    begin
      DebugLn('QtLCL error: "'+GetWindowManager+'" wm does not support _NET_FRAME_EXTENTS (2).');
    end;
  end else
  begin
    DebugLn('QtLCL error: "'+GetWindowManager+'" wm does not support _NET_REQUEST_FRAME_EXTENTS.');
  end;
end;

function GetX11WindowGeometry(AWinID: PtrUInt; out ARect: TRect): boolean;
var
  d: PDisplay;
  ARootWin, X11Window: TWindow;
  ABorder, ADepth: integer;
begin
  Result := False;
  d := QX11Info_display();
  with ARect do
  begin
    Left := 0;
    Top := 0;
    Right := 0;
    Bottom := 0;
  end;
  if d = nil then
    exit;

  if AWinID = 0 then
  begin
    X11Window := XRootWindow(d, QX11Info_appScreen);
  end else
    X11Window := TWindow(AWinID);
  if X11Window = 0 then
    exit;
  Result := XGetGeometry(d, X11Window, @ARootWin, @ARect.Left, @ARect.Top, @ARect.Right, @ARect.Bottom, @ABorder, @ADepth) = 1;
end;

{$IF FPC_FULLVERSION <= 20602}
type
  PUint32 = PDWord;
{$ENDIF}

function GetX11RectForAtom(AWinID: PtrUInt; const AAtomName: string; out ARect: TRect): boolean;
var
  d: PDisplay;
  X11Window: TWindow;
  WMAtom: TAtom;
  typeReturned: TAtom;
  formatReturned: Integer;
  nitemsReturned: PtrInt;
  unused: PtrInt;
  data: Pointer;
  ANewData: PUInt32;
begin
  Result := False;
  d := QX11Info_display();
  with ARect do
  begin
    Left := 0;
    Top := 0;
    Right := 0;
    Bottom := 0;
  end;
  if d = nil then
    exit;

  if AWinID = 0 then
  begin
    X11Window := XRootWindow(d, QX11Info_appScreen);
  end else
    X11Window := TWindow(AWinID);
  if X11Window = 0 then
    exit;

  {_KDE_NET_WM_SHADOW, find others}
  WMAtom := XInternAtom(d,PChar(AAtomName), True);
  if WMAtom > 0 then
  begin
    data := nil;
    if XGetWindowProperty(d, X11Window, WMAtom, 0, 4, False, XA_CARDINAL,
      @typeReturned, @formatReturned, @nitemsReturned,
      @unused, @data) = Success then
    begin
      if (typeReturned = XA_CARDINAL) and (formatReturned = 32) and
        (Data <> nil) then
      begin
        ANewData := Data;
        while nitemsReturned > 0 do
        begin
          with ARect do
          begin
            if Left = 0 then
              Left := Integer(ANewData^)
            else
            if Right = 0 then
              Right := Integer(ANewData^)
            else
            if Top = 0 then
              Top := Integer(ANewData^)
            else
            if Bottom = 0 then
              Bottom := Integer(ANewData^);
          end;
          dec(nItemsReturned);
          if Result or (nItemsReturned = 0) then
            break;
          inc(ANewData);
        end;
        Result := True;
        if nitemsReturned > 0 then
          XFree(Data);
        Data := nil;
      end;
    end;
  end else
  begin
    DebugLn('QtLCL error: "'+GetWindowManager+'" wm does not support '+AAtomName+'.');
  end;
end;

function GetX11SupportedAtoms(AWinID: PtrUInt; AList: TStrings): boolean;
var
  d: PDisplay;
  ARoot: TWindow;
  WMAtom: TAtom;
  typeReturned: TAtom;
  formatReturned: Integer;
  nitemsReturned: PtrInt;
  unused: PtrInt;
  data: Pointer;
  ANewData: PAtom;
begin
  Result := False;
  d := QX11Info_display;
  if d = nil then
    exit;
  ARoot := XRootWindow(d, QX11Info_appScreen);

  WMAtom := XInternAtom(d, '_NET_SUPPORTED', True);
  if WMAtom = 0 then
  begin
    DebugLn('ERROR : QtLCL: _NET_SUPPORTED not provided by wm ',GetWindowManager);
    exit;
  end;

  data := nil;
  if XGetWindowProperty(d, ARoot, WMAtom, 0, (65536 div sizeof(LongInt)), False, AnyPropertyType,
    @typeReturned, @formatReturned, @nitemsReturned,
    @unused, @data) = Success then
  begin
    if (typeReturned = XA_ATOM) and (formatReturned = 32) and
      (Data <> nil) then
    begin
      ANewData := Data;
      while nitemsReturned > 0 do
      begin
        AList.AddObject(XGetAtomName(d, ANewData^),TObject(ANewData^));
        dec(nItemsReturned);
        if Result or (nItemsReturned = 0) then
          break;
        inc(ANewData);
      end;
      Result := True;
      if nitemsReturned > 0 then
        XFree(Data);
      Data := nil;
    end;
  end;
end;

{$ENDIF}

