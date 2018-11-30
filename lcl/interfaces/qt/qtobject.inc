{%MainUnit qtint.pp}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
//---------------------------------------------------------------

{$IFDEF HASX11}
// palette initialisation for X11, fixed delayed initialization of palettes
// with various styles under X11.
procedure QtX11InitializePalettes;
var
  Palette: QPaletteH;
  LineEditPalette: QPaletteH;
  ComboBoxPalette: QPaletteH;
  TextEditPalette: QPaletteH;
  Brush: QBrushH;
begin
  //palette for disabled viewports and edit controls
  Palette := QPalette_create();
  QApplication_palette(Palette);
  Brush := QPalette_window(Palette);
  QPalette_setBrush(Palette, QPaletteDisabled, QPaletteBase, Brush);
  QApplication_setPalette(Palette);
  QPalette_destroy(Palette);

  LineEditPalette := QPalette_create();
  ComboBoxPalette := QPalette_create();
  TextEditPalette := QPalette_create();

  //palette for inactive titlebars
  Palette := QPalette_create();
  QApplication_palette(Palette);

  // save original palette for QLineEdit, QComboBox, QTextEdit
  QApplication_palette(LineEditPalette, 'QLineEdit');
  QApplication_palette(ComboBoxPalette, 'QComboBox');
  QApplication_palette(TextEditPalette, 'QTextEdit');

  Brush := QPalette_dark(Palette);
  QPalette_setBrush(Palette, QPaletteInactive, QPaletteHighlight, Brush);
  QApplication_setPalette(Palette);
  QPalette_destroy(Palette);

  // restore original palettes for QLineEdit, QComboBox, QTextEdit
  // otherwise we can have inactive selection color when setting
  // selection on invisible widgets
  QApplication_setPalette(LineEditPalette,'QLineEdit');
  QApplication_setPalette(ComboBoxPalette,'QComboBox');
  QApplication_setPalette(TextEditPalette,'QTextEdit');

  QPalette_destroy(LineEditPalette);
  QPalette_destroy(ComboBoxPalette);
  QPalette_destroy(TextEditPalette);
end;
{$ENDIF}
{------------------------------------------------------------------------------
  Method: TQtWidgetSet.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TQtWidgetSet.Create;
begin
  FLastWFPMousePos := Point(MaxInt, MaxInt);
  FLastWFPResult := 0;
  inherited Create;
  {$IFDEF QtUseAccurateFrame}
  FWSFrameMargins := Rect(0, 0, 0, 0);
  {$ENDIF}
  FIsLibraryInstance := QCoreApplication_instance() <> nil;
  if FIsLibraryInstance then
    App := QApplicationH(QCoreApplication_instance())
  else
    App := QApplication_Create(@argc, argv);
  {$IFDEF QtUseNativeEventLoop}
  FMainTimerID := -1;
  {$ENDIF}
  {$J+}
  QtVersionInt(QtVersionMajor, QtVersionMinor, QtVersionMicro);
  {$J-}
  FCachedMenuBarHeight := -1;
  FAppEvenFilterHook := nil;
  FAppFocusChangedHook := nil;

  QtGDIObjects := TQtGDIObjects.Create;
  InitStockItems;
  QtWidgetSet := Self;
  ClearCachedColors;
  FDockImage := nil;
  FDragImageLock := False;
  System.InitCriticalSection(CriticalSection);
  SavedHandlesList := TMap.Create(TMapIdType(ituPtrSize), SizeOf(TObject));
  FSocketEventMap := TMap.Create(TMapIdType(its4), SizeOf(Pointer));
  SysTrayIconsList := TFPList.Create;
  StayOnTopList := nil;
  FAppActive := False;
  {$IFDEF HASX11}
  SavedHintHandlesList := TFPList.Create;
  FMinimizedByPager := False;
  FLastMinimizeEvent := 0;
  if not FIsLibraryInstance and
    ((QtVersionMajor = 4) and (QtVersionMinor < 6)) or IsOldKDEInstallation then
      QtX11InitializePalettes;
  FWindowManagerName := LowerCase(GetWindowManager);
  // metacity wm forks. marco = mint mate wm, gnome shell = gnome 3 wm
  if (FWindowManagerName = 'marco') or (FWindowManagerName = 'gnome shell') or
    (UTF8Pos('mutter', FWindowManagerName) > 0) then
    FWindowManagerName := 'metacity';
  {$ENDIF}
  {$IFDEF DARWIN}
  // do not swap meta and ctrl keys, issue #20897
  if not FIsLibraryInstance and (QtVersionMajor = 4) and (QtVersionMinor > 5) then
    QCoreApplication_setAttribute(QtAA_MacDontSwapCtrlAndMeta, True);
  {$ENDIF}
  FGlobalActions := TFPList.Create;
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TQtWidgetSet.Destroy;
begin
  if FDockImage <> nil then
    QRubberBand_destroy(FDockImage);
  DestroyGlobalCaret;
  Clipboard.Free;
  FreeStockItems;
  FreeSysColorBrushes;
  QtDefaultPrinter.Free;
  QtWidgetSet := nil;
  
  if SavedDCList<>nil then
    SavedDCList.Free;
    
  QtDefaultContext.Free;
  QtScreenContext.Free;

  ClearCachedColors;

  if StayOnTopList <> nil then
  begin
    StayOnTopList.Free;
    StayOnTopList := nil;
  end;

  if SavedHandlesList <> nil then
  begin
    SavedHandlesList.Free;
    SavedHandlesList := nil;
  end;
  {$IFDEF HASX11}
  if SavedHintHandlesList <> nil then
  begin
    SavedHintHandlesList.Free;
    SavedHintHandlesList := nil;
  end;
  {$ENDIF}

  if SysTrayIconsList <> nil then
  begin
    SysTrayIconsList.Free;
    SysTrayIconsList := nil;
  end;

  FSocketEventMap.Free;
  FGlobalActions.Free;

  System.DoneCriticalsection(CriticalSection);

  if Assigned(QtGDIObjects) then
    FreeThenNil(QtGDIObjects);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Creates a new timer and sets the callback event.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): THandle;
var
  QtTimer: TQtTimer;
begin
  QtTimer := TQtTimer.CreateTimer(Interval, TimerFunc, App);
  
  Result := THandle(QtTimer);
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destroys a timer.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.DestroyTimer(TimerHandle: THandle): boolean;
begin
  TQtTimer(TimerHandle).Free;
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.AppInit
  Params:  None
  Returns: Nothing

  Initializes the application
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
var
  ScreenDC: HDC;
begin
  WakeMainThread := @OnWakeMainThread;

  {
    check whether this hook crashes on linux & darwin and why it is so
    we need this hook to catch release messages
  }
  // install global event filter
  FAppEvenFilterHook := QObject_hook_create(App);
  QObject_hook_hook_events(FAppEvenFilterHook, @EventFilter);

    // install focus change slot

  FAppFocusChangedHook := QApplication_hook_create(App);
  QApplication_hook_hook_focusChanged(FAppFocusChangedHook, @FocusChanged);

  if not FIsLibraryInstance then
  begin
    {$IF DEFINED(HAIKU) OR DEFINED(QTOPIA)}
    FAppSessionQuit := nil;
    FAppSaveSessionRequest := nil;
    {$ELSE}
    FAppSessionQuit := QApplication_hook_create(App);
    QApplication_hook_hook_commitDataRequest(FAppSessionQuit, @SlotCommitDataRequest);
    FAppSaveSessionRequest := QApplication_hook_create(App);
    QApplication_hook_hook_saveStateRequest(FAppSaveSessionRequest, @SlotSaveDataRequest);
    {$ENDIF}
  end else
  begin
    FAppSessionQuit := nil;
    FAppSaveSessionRequest := nil;
  end;


  ScreenDC := GetDC(0);
  try
    {$IFDEF DARWIN}
    // this code is removed in r57679, and cocoa/carbon are fixed separatelly, so
    // qt/qt5 wasn't good with standard mac ppi 72. Cocoa ws uses CocoaBasePPI const.
    // issue #34625
    ScreenInfo.PixelsPerInchX := 96;
    ScreenInfo.PixelsPerInchY := 96;
	  {$ELSE}
    ScreenInfo.PixelsPerInchX := GetDeviceCaps(ScreenDC, LOGPIXELSX);
    ScreenInfo.PixelsPerInchY := GetDeviceCaps(ScreenDC, LOGPIXELSY);
	  {$ENDIF}
    ScreenInfo.ColorDepth := GetDeviceCaps(ScreenDC, BITSPIXEL);
  finally
    ReleaseDC(0, ScreenDC);
  end;
  
  QtDefaultPrinter;
  {$IFNDEF MSWINDOWS}
  // initialize clipboard
  ClipBoard;
  {$ENDIF}
  // initialize default app font name
  SetDefaultAppFontName;
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.AppRun
  Params:  None
  Returns: Nothing

  Enter the main message loop
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
{$IFDEF QtUseNativeEventLoop}
var
  ATimer: QTimerH;
{$ENDIF}
begin
  {$IFDEF QtUseNativeEventLoop}
  ATimer := QTimer_create(QCoreApplication_instance());
  if (StyleName = 'gtk') or (StyleName = 'gtk+') then
    QTimer_setInterval(ATimer, 1) {issue #31191}
  else
    QTimer_setInterval(ATimer, 0);
  QTimer_start(ATimer);
  FMainTimerID := QTimer_timerId(ATimer);
  QApplication_exec();
  {$ELSE}
  // use LCL loop
  if Assigned(ALoop) then
    ALoop;
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.AppWaitMessage
  Params:  None
  Returns: Nothing

  Waits until a message arrives, processes that and returns control out of the function
  
  Utilized on Modal dialogs
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.AppWaitMessage;
begin
  {we cannot call directly processEvents() with this flag
   since it produces AV's sometimes, so better check is there
   any pending event.}
  QCoreApplication_processEvents(QEventLoopWaitForMoreEvents);
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.AppProcessMessages
  Params:  None
  Returns: Nothing

  Processes all messages on the quoue
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.AppProcessMessages;
begin
  QCoreApplication_processEvents(QEventLoopAllEvents);
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.AppTerminate
  Params:  None
  Returns: Nothing

  Implements Application.Terminate and MainForm.Close.
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.AppTerminate;
begin
  // free hooks
  QObject_hook_destroy(FAppEvenFilterHook);
  QApplication_hook_destroy(FAppFocusChangedHook);
  // do not quit application if we are library
  if not FIsLibraryInstance then
  begin
    {$IFNDEF HAIKU}
    if Assigned(FAppSessionQuit) then
    begin
      QApplication_hook_destroy(FAppSessionQuit);
      FAppSessionQuit := nil;
    end;
    if Assigned(FAppSaveSessionRequest) then
    begin
      QApplication_hook_destroy(FAppSaveSessionRequest);
      FAppSaveSessionRequest := nil;
    end;
    {$ENDIF}
    QCoreApplication_quit;
  end;
end;

procedure TQtWidgetSet.AppMinimize;
{$IFDEF HASX11}
var
  i: Integer;
  AForm: TCustomForm;
  States: QtWindowStates;
  AWidget: TQtWidget;
{$ENDIF}
begin
  if (Application.MainForm <> nil) and (Application.MainForm.HandleAllocated) then
  begin
    {$IFDEF HASX11}
    HideAllHints;
    // first minimize all designed forms
    for i := 0 to Screen.CustomFormZOrderCount-1 do
    begin
      AForm := Screen.CustomFormsZOrdered[i];
      if not AForm.HandleAllocated or Assigned(AForm.Parent) then
        continue;
      {$IFDEF DEBUGQTAPPMINIMIZE}
      DebugLn('1.MUST MINIMIZE ',dbgsName(AForm),' design ? ',dbgs(csDesigning in AForm.ComponentState),
       ' HANDLEVISIBLE ',dbgs(TQtWidget(AForm.Handle).getVisible),' FORMVISIBLE=',dbgs(AForm.Visible));
      {$ENDIF}
      AWidget := TQtWidget(AForm.Handle);
      if AWidget.getVisible and
        (csDesigning in AForm.ComponentState) then
      begin
        States := AWidget.getWindowState;
        {$IFDEF DEBUGQTAPPMINIMIZE}
        DebugLn('1.  **** TRYING TO MINIMIZE ',dbgsName(AForm),' already minimized ? ',dbgs(AWidget.isMinimized));
        {$ENDIF}
        if not AWidget.isMinimized then
          AWidget.setWindowState(States or QtWindowMinimized);
      end;
    end;
    for i := 0 to Screen.CustomFormZOrderCount-1 do
    begin
      AForm := Screen.CustomFormsZOrdered[i];
      if not AForm.HandleAllocated or Assigned(AForm.Parent) or
        (csDesigning in AForm.ComponentState) then
        continue;
      {$IFDEF DEBUGQTAPPMINIMIZE}
      DebugLn('2. MUST MINIMIZE ',dbgsName(AForm),' design ? ',dbgs(csDesigning in AForm.ComponentState),
       ' HANDLEVISIBLE ',dbgs(TQtWidget(AForm.Handle).getVisible),' FORMVISIBLE=',dbgs(AForm.Visible));
      {$ENDIF}
      AWidget := TQtWidget(AForm.Handle);
      if AWidget.getVisible and
         not (AForm.FormStyle in [fsMDIChild, fsSplash]) and
         not (AForm.BorderStyle = bsNone) then
      begin
        States := AWidget.getWindowState;
        {$IFDEF DEBUGQTAPPMINIMIZE}
        DebugLn('2.  **** TRYING TO MINIMIZE ',dbgsName(AForm),' already minimized ? ',dbgs(AWidget.isMinimized));
        {$ENDIF}
        if not AWidget.isMinimized then
          AWidget.setWindowState(States or QtWindowMinimized);
      end;
    end;
    {$ELSE}
    TQtMainWindow(Application.MainForm.Handle).ShowMinimized;
    {$ENDIF}
  end;
end;

procedure TQtWidgetSet.AppRestore;
{$IFDEF HASX11}
var
  i: Integer;
  AForm: TCustomForm;
  States: QtWindowStates;
  AWidget: TQtWidget;
{$ENDIF}
begin
  if (Application.MainForm <> nil) and (Application.MainForm.HandleAllocated) then
  begin
    {$IFDEF HASX11}
    if Screen = nil then exit;
    for i := Screen.CustomFormZOrderCount-1 downto 0 do
    begin
      AForm := Screen.CustomFormsZOrdered[i];
      if not AForm.HandleAllocated or Assigned(AForm.Parent) then
        continue;
      {$IFDEF DEBUGQTAPPMINIMIZE}
      DebugLn('MUST RESTORE ',dbgsName(AForm),' design ? ',dbgs(csDesigning in AForm.ComponentState),
       ' HANDLEVISIBLE ',dbgs(TQtWidget(AForm.Handle).getVisible),' FORMVISIBLE=',dbgs(AForm.Visible));
      {$ENDIF}
      AWidget := TQtWidget(AForm.Handle);
      if AWidget.getVisible and
        ((not (AForm.FormStyle in [fsMDIChild, fsSplash]) and
         not (AForm.BorderStyle = bsNone)) or
        (csDesigning in AForm.ComponentState)) then
      begin
        States := AWidget.getWindowState;
        {$IFDEF DEBUGQTAPPMINIMIZE}
        DebugLn('TRYING TO RESTORE ',dbgsName(AForm),' already minimized ? ',dbgs(AWidget.isMinimized));
        {$ENDIF}
        if AWidget.isMinimized then
          AWidget.setWindowState(States and not QtWindowMinimized);
      end;
    end;
    RestoreAllHints;
    {$ELSE}
    TQtMainWindow(Application.MainForm.Handle).ShowNormal;
    {$ENDIF}
  end;
end;

procedure TQtWidgetSet.AppBringToFront;
begin
  if (Application.MainForm <> nil) and
    (Application.MainForm.HandleAllocated) and
    (TQtMainWindow(Application.MainForm.Handle).getVisible) then
    TQtMainWindow(Application.MainForm.Handle).BringToFront;
end;

procedure TQtWidgetSet.AppSetIcon(const Small, Big: HICON);
var
  DoDestroyIcon: Boolean;
  Icon: QIconH;
begin
  DoDestroyIcon := Big = 0;
  if DoDestroyIcon then
    Icon := QIcon_create()
  else
    Icon := TQtIcon(Big).Handle;
  QApplication_setWindowIcon(Icon);
  if DoDestroyIcon then
    QIcon_destroy(Icon);
end;


procedure TQtWidgetSet.AppSetTitle(const ATitle: string);
var
  W: WideString;
begin
  W := {%H-}ATitle;
  QCoreApplication_setApplicationName(@W);
end;

function TQtWidgetSet.AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
  Result := True;
  QtRemoveStayOnTop(ASystemTopAlso);
end;

function TQtWidgetSet.AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
  Result := True;
  QtRestoreStayOnTop(ASystemTopAlso);
end;

procedure TQtWidgetSet.SetOverrideCursor(const AValue: TObject);
begin
  if AValue = nil then
    QApplication_restoreOverrideCursor()
  else
  if FOverrideCursor = nil then
    QApplication_setOverrideCursor(TQtCursor(AValue).Handle)
  else
    QApplication_changeOverrideCursor(TQtCursor(AValue).Handle);
  FOverrideCursor := AValue;
end;

type
  TQtTempFormStyleSet = Set of TFormStyle;
const
  TQtTopForms: Array[Boolean] of TQtTempFormStyleSet = (fsAllNonSystemStayOnTop,
    fsAllStayOnTop);

procedure TQtWidgetSet.QtRemoveStayOnTop(const ASystemTopAlso: Boolean = False);
var
  i: Integer;
  AForm: TCustomForm;
  W: TQtMainWindow;
  Flags: QtWindowFlags;
begin
  if StayOnTopList = nil then
    StayOnTopList := TMap.Create(TMapIdType(ituPtrSize), SizeOf(TObject));
  for i := 0 to Screen.CustomFormZOrderCount - 1 do
  begin
    AForm := Screen.CustomFormsZOrdered[i];
    if AForm.HandleAllocated then
    begin
      W := TQtMainWindow(AForm.Handle);
      if (AForm.Parent = nil) and
        (AForm.FormStyle in TQtTopForms[ASystemTopAlso]) and W.GetVisible and
         not W.IsMdiChild and not W.IsModal and not w.isMinimized then
      begin
        Flags := W.windowFlags;
        if (Flags and QtWindowStaysOnTopHint) <> 0 then
        begin
          W.BeginUpdate;
          W.setAttribute(QtWA_ShowWithoutActivating, True);
          W.setWindowFlags(Flags and not QtWindowStaysOnTopHint);
          W.Show;
          W.EndUpdate;
          if not StayOnTopList.HasId(W) then
            StayOnTopList.Add(W, W);
        end;
      end;
    end;
  end;
end;

procedure TQtWidgetSet.QtRestoreStayOnTop(const ASystemTopAlso: Boolean = False);
var
  i: Integer;
  AForm: TCustomForm;
  W: TQtMainWindow;
  Flags: QtWindowFlags;
begin
  if StayOnTopList = nil then
    exit;
  for i := Screen.CustomFormZOrderCount - 1 downto 0 do
  begin
    AForm := Screen.CustomFormsZOrdered[i];
    if AForm.HandleAllocated then
    begin
      W := TQtMainWindow(AForm.Handle);
      if (AForm.Parent = nil) and
        (AForm.FormStyle in TQtTopForms[ASystemTopAlso]) and W.GetVisible and
        not W.IsMdiChild and not W.IsModal and not W.isMinimized then
      begin
        if StayOnTopList.HasId(W) then
        begin
          W.BeginUpdate;
          Flags := W.windowFlags;
          W.setWindowFlags(Flags or QtWindowStaysOnTopHint);
          W.Show;
          W.setAttribute(QtWA_ShowWithoutActivating, False);
          W.EndUpdate;
        end;
      end;
    end;
  end;
  StayOnTopList.Free;
  StayOnTopList := nil;
end;

procedure TQtWidgetSet.SetDefaultAppFontName;
var
  AppFont: QFontH;
begin
  FCachedMenuBarHeight := -1;
  AppFont := QFont_create();
  QApplication_font(AppFont);
  QFont_family(AppFont, @FDefaultAppFontName);
  QFont_destroy(AppFont);
end;

function TQtWidgetSet.CreateThemeServices: TThemeServices;
begin
  Result := TQtThemeServices.Create;
end;

function TQtWidgetSet.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
var
  AObject: TQtObject;
  W: TQtWidget;
  LCLEvent: QLCLMessageEventH;
  ASequence: QKeySequenceH;
  AKey: WideString;
  AParent: QWidgetH;
  R: TRect;
  AQtPoint: TQtPoint;

  function IsAnyWindowActive: Boolean;
  begin
    Result := (QApplication_activeWindow() <> nil) or
      (QApplication_activeModalWidget() <> nil) or
      (QApplication_activePopupWidget() <> nil);
  end;

  function IsSystemTrayWidget: boolean;
  var
    AName: WideString;
    AWidget: QWidgetH;
    RGeom: TRect;
    AFlags: QtWindowFlags;
    i: Integer;
  begin
    Result := False;
    if QObject_isWidgetType(Sender) then
    begin
      AWidget := QWidgetH(Sender);
      QObject_objectName(Sender, @AName);
      if Copy(AName, 1, 16) = 'qtlclsystrayicon' then
      begin
        for i := 0 to SysTrayIconsList.Count - 1 do
        begin
          RGeom := TQtSystemTrayIcon(SysTrayIconsList.Items[i]).GetGeometry;
          if TQtSystemTrayIcon(SysTrayIconsList.Items[i]).SysTrayWidget = nil then
          begin
            if QApplication_widgetAt(RGeom.Left, RGeom.Top) = AWidget then
              TQtSystemTrayIcon(SysTrayIconsList.Items[i]).AttachSysTrayWidget(AWidget);
          end;
        end;
        exit(True);
      end;
      if QWidget_isWindow(AWidget) and (QWidget_parentWidget(AWidget) = nil) then
      begin
        AFlags := QWidget_windowFlags(AWidget);

        if QWidget_testAttribute(AWidget, QtWA_AlwaysShowToolTips) and
          QWidget_testAttribute(AWidget, QtWA_PaintOnScreen) and
          QWidget_testAttribute(AWidget, QtWA_NoSystemBackground) and
          not QWidget_testAttribute(AWidget, QtWA_QuitOnClose) and
          {$IFDEF HASX11}
          (AFlags and QtX11BypassWindowManagerHint = QtX11BypassWindowManagerHint) and
          {$ENDIF}
          (AFlags and QtFramelessWindowHint = QtFramelessWindowHint) then
        begin
          if HwndFromWidgetH(AWidget) = 0 then
          begin
            // we must find it by geometry, but it's innacurate since
            // qt systrayicon widget returns -1,-1 for left & top, so we
            // use QApplication_widgetAt().
            // Another problem is that QSystemTrayIcon geometry is updated
            // too late, much after QEventShow/QEventShowToParent
            // so no way to catch private QWidget until we enter
            // it by mouse.
            // For that reason we use LCLQt_RegisterSystemTrayIcon event.

            {$IFDEF DEBUGSYSTRAYICON}
            DebugLn('====****** Inherits private ? ',dbgs(QObject_inherits(AWidget, 'QSystemTrayIconSys')),
            ' mouseTracking=',dbgs(QWidget_hasMouseTracking(AWidget)));
            {$ENDIF}
            for i := 0 to SysTrayIconsList.Count - 1 do
            begin
              RGeom := TQtSystemTrayIcon(SysTrayIconsList.Items[i]).GetGeometry;
              if (QApplication_widgetAt(RGeom.Left, RGeom.Top) = AWidget) then
              begin
                AName := 'qtlclsystrayicon_' {%H-}+ dbgHex(PtrUInt(AWidget));
                QObject_setObjectName(Sender, @AName);
                TQtSystemTrayIcon(SysTrayIconsList.Items[i]).AttachSysTrayWidget(AWidget);
                {$IFDEF DEBUGSYSTRAYICON}
                DebugLn('Attached systemtrayicon[',dbgs(I),'] with geometry ',dbgs(RGeom),' dbg=',
                  dbgsName(TQtSystemTrayIcon(SysTrayIconsList.Items[i]).FTrayIcon),
                  ' position=',dbgs(TQtSystemTrayIcon(SysTrayIconsList.Items[i]).GetPosition));
                {$ENDIF}
                TQtSystemTrayIcon(SysTrayIconsList.Items[i]).UpdateSystemTrayWidget;
                Result := True;
                break;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

begin
  Result := False;

  // find QSystemTrayIcon
  if ((QEvent_type(Event) = LCLQt_RegisterSystemTrayIcon) or
     (QEvent_type(Event) = QEventPaint) or
    (QEvent_type(Event) = QEventEnter)) and
    Assigned(SysTrayIconsList) and (SysTrayIconsList.Count > 0) and
    QObject_isWidgetType(Sender) and (QObject_parent(Sender) = nil) and
    QWidget_isWindow(QWidgetH(Sender)) and QWidget_isVisible(QWidgetH(Sender)) and
    (QWidget_focusPolicy(QWidgetH(Sender)) = QtNoFocus) then
  begin
    AParent := QWidgetH(Sender);
    QWidget_geometry(AParent, @R);
    if (R.Left = -1) and (R.Top = -1) and (R.Right > 0) and (R.Bottom > 0) then
    begin
      AQtPoint.x := 0;
      AQtPoint.y := 0;
      QWidget_mapToGlobal(AParent, @AQtPoint, @AQtPoint);
      {$IFDEF DEBUGSYSTRAYICON}
      DebugLn('EVENT: ',dbgs(QEvent_type(Event)),' Sender 0x',dbgHex(PtrUInt(Sender)),' geometry ',dbgs(R),' QtPt.X=',dbgs(AQtPoint.x),' QtPt.Y=',dbgs(AQtPoint.y));
      {$ENDIF}
      if (QEvent_type(Event) = LCLQt_RegisterSystemTrayIcon) then
      begin
        if IsSystemTrayWidget then
        begin
          Result := True;
          {$IFDEF DEBUGSYSTRAYICON}
          DebugLn('Found SystemTrayIcon via event ',dbgs(QEvent_type(Event)),' SYSTRAYICON 0x',dbgHex(PtrUInt(Sender)));
          {$ENDIF}
          exit;
        end;
      end else
      if ((QEvent_type(Event) = QEventPaint) and (AQtPoint.x > 0) and (AQtPoint.y > 0)) or
        (QEvent_type(Event) = QEventEnter) then
      begin
        LCLEvent := QLCLMessageEvent_create(LCLQt_RegisterSystemTrayIcon, 0, 0, 0, 0);
        QCoreApplication_postEvent(AParent, LCLEvent);
      end;
    end;
  end;

  case QEvent_type(Event) of
    {$IFDEF QtUseNativeEventLoop}
    QEventTimer:
    begin
      if (QTimerEvent_timerId(QTimerEventH(Event)) = FMainTimerID) and
        Assigned(Application) and not Application.Terminated then
          Application.Idle(True);
    end;
    {$ENDIF}
    QEventShortcutOverride: // issue #22827
    begin
      QKeyEvent_text(QKeyEventH(Event), @AKey);
      if (QKeyEvent_modifiers(QKeyEventH(Event)) = QtAltModifier) and (AKey <> '') then
      begin
        ASequence := QKeySequence_create(QKeyEvent_modifiers(QKeyEventH(Event))
          or QKeyEvent_Key(QKeyEventH(Event)));
        try
          AParent := QWidget_parentWidget(QWidgetH(Sender));
          if AParent <> nil then
            Result := QApplication_notify(App, AParent, Event);
        finally
          QKeySequence_destroy(ASequence);
        end;
      end;
    end;

    QEventApplicationFontChange: SetDefaultAppFontName;
    QEventStyleChange:
      begin
        if (Sender = QCoreApplication_instance()) then
        begin
          FCachedMenuBarHeight := -1;
          ThemeServices.IntfDoOnThemeChange;
        end;
      end;
    QEventApplicationActivate:
    begin
      LCLEvent := QLCLMessageEvent_create(LCLQt_ApplicationActivate);
       // activate it imediatelly (high priority)
      QCoreApplication_postEvent(Sender, LCLEvent, 1 {high priority});
    end;
    LCLQt_ApplicationActivate:
      if Assigned(Application) and not FAppActive then
      begin
        FAppActive := True;
        {$IF DEFINED(QTDEBUGAPPACTIVATE) OR DEFINED(VerboseQtEvents)}
        DebugLn('TQtWidgetSet.EventFilter: Application is activated - time ',dbgs(GetTickCount));
        {$ENDIF}
        // check if activated form is StayOnTop, if it's so, we must
        // eat next appdeactivate & appactivate since we are changing form
        // flags !
        if (StayOnTopList <> nil) then
        begin
          W := TQtWidget(GetActiveWindow);
        end else
          W := nil;
        Application.IntfAppActivate;
        QtRestoreStayOnTop;
        if (W <> nil) and Assigned(StayOnTopList) and StayOnTopList.HasId(W) then
          W.Activate;
        Result := True;
      end;

    QEventApplicationDeactivate:
    begin
      // we must check if we are ready for deactivation (low priority)
      // this is 2way check. LCLQt_ApplicationDeActivate sends
      // LCLQt_ApplicationDeActivate_Check to be 100% sure if needed.
      LCLEvent := QLCLMessageEvent_create(LCLQt_ApplicationDeActivate);
      QCoreApplication_postEvent(Sender, LCLEvent, -$FF);
    end;

    LCLQt_ApplicationDeactivate:
      begin
        if Assigned(Application) and FAppActive then
        begin
          if not IsAnyWindowActive then
          begin
            QCoreApplication_sendPostedEvents(nil, QEventWindowActivate);
            QCoreApplication_processEvents(QEventLoopAllEvents, 10 {msec});
          end;

          // if there's active window after posting from queue, just exit ...
          // app is not deactivated.
          if IsAnyWindowActive then
            exit(True);

          // to be 100% sure that we are really deactivated, send check
          // event with pretty low priority. We need
          // LCLQt_ApplicationDeActivate_Check to avoid infinite loop inside
          // this event with same code.
          LCLEvent := QLCLMessageEvent_create(LCLQt_ApplicationDeActivate_Check);
          QCoreApplication_postEvent(Sender, LCLEvent, -$FFFF);
          Result := True;
        end;
      end;

    LCLQt_ApplicationDeactivate_Check:
      if Assigned(Application) and FAppActive then
      begin
        // 1st send posted events, and process few events from queue
        if not IsAnyWindowActive then
        begin
          QCoreApplication_sendPostedEvents(nil, QEventWindowActivate);
          QCoreApplication_processEvents(QEventLoopAllEvents, 10 {msec});
        end;

        // if there's active window after posting from queue, just exit ...
        // app is not deactivated.
        if IsAnyWindowActive then
        begin
          {$IF DEFINED(QTDEBUGAPPACTIVATE) OR DEFINED(VerboseQtEvents)}
          DebugLn('NOTICE: TQtWidgetSet.EventFilter: App deactivation called with active windows ... ignoring.');
          {$ENDIF}
          QEvent_ignore(Event);
          exit(True);
        end;
        {$IF DEFINED(QTDEBUGAPPACTIVATE) OR DEFINED(VerboseQtEvents)}
        DebugLn('TQtWidgetSet.EventFilter: Application is deactivated - time ',dbgs(GetTickCount));
        {$ENDIF}
        FAppActive := False;
        Application.IntfAppDeactivate;
        QtRemoveStayOnTop;
        Result := True;
      end;

    QEventApplicationPaletteChange:
      begin
        if Sender = App then
        begin
          ClearCachedColors;
          FreeSysColorBrushes(True);
        end;
      end;
    QEventShow,
    QEventHide:
      begin
        // invalidate widgetAt cache.
        if QObject_isWidgetType(Sender) and IsValidWidgetAtCachePointer then
          InvalidateWidgetAtCache;
      end;
    LCLQt_Destroy:
      begin
        AObject := TQtObject({%H-}Pointer(QLCLMessageEvent_getWParam(QLCLMessageEventH(Event))));
        //WriteLn('Catched free for: ', PtrUInt(AObject), ' : ', AObject.ClassName);
        AObject.Free;
        Result := True;
        QEvent_Accept(Event);
      end;
    LCLQt_CheckSynchronize:
      begin
        // a thread is waiting -> synchronize
        CheckSynchronize;
      end;
  end;
end;

procedure TQtWidgetSet.FocusChanged(aold: QWidgetH; anew: QWidgetH); cdecl;
var
  OldWidget, NewWidget: TQtWidget;
  Msg: TLMessage;
  FocusedQtWidget: QWidgetH;
  FocusedTQtWidget: TQtWidget;

  {qt is tricky about focus, we don't want to inform LCL when qt internally
  kills focus on an inactive form. eg. TTreeView->Editor enabled}
  function CheckIfActiveForm(AWidget: TQtWidget): Boolean;
  var
    AForm: TCustomForm;
    AMainWin: TQtMainWindow;
    QtEdit: IQtEdit;
  begin
    Result := True;
    if Assigned(AWidget) and Assigned(AWidget.LCLObject) then
    begin
      AMainWin := nil;
      if (csDesigning in AWidget.LCLObject.ComponentState) then
        exit;
      if TQtWidget(AWidget.LCLObject.Handle) is TQtMainWindow then
        AMainWin := TQtMainWindow(AWidget);

      if AMainWin = nil then
      begin
        AForm := GetParentForm(AWidget.LCLObject);
        if Assigned(AForm) and (AForm.HandleAllocated) then
          AMainWin := TQtMainWindow(AForm.Handle);
      end;
      Result := AMainWin <> nil;
      if not Result then
      begin
        {$IF DEFINED(VerboseFocus) OR DEFINED(DebugQtFocus)}
        WriteLn('TQtWidgetSet.FocusChanged: CheckIfActiveForm *** NO FORM ?!? ***');
        {$ENDIF}
        exit;
      end;

      if AMainWin.IsMdiChild then
      begin
        Result :=
          QMdiArea_activeSubWindow(QMdiSubWindow_mdiArea(QMdiSubWindowH(AMainWin.Widget))) =
            QMdiSubWindowH(AMainWin.Widget);
      end else
      begin
        Result := True; // issue #31440 QWidget_isActiveWindow(AMainWin.Widget);
      end;
      if (AMainWin <> AWidget) and not Supports(AWidget, IQtEdit, QtEdit) then
        Result := True;
    end;
  end;

  {checks when qtmdi is doing weird thing (trying to loop itself)}
  function MDIFocusFixNeeded: Boolean;
  var
    OldWin, NewWin: TCustomForm;
    // H: HWND;
  begin
    Result := False;

    if Assigned(OldWidget.LCLObject) then
      OldWin := GetParentForm(OldWidget.LCLObject)
    else
      OldWin := nil;

    if (NewWidget <> nil) and Assigned(NewWidget.LCLObject) then
      NewWin := GetParentForm(NewWidget.LCLObject)
    else
      NewWin := nil;

    Result := (OldWin <> nil) and OldWin.HandleAllocated and
      ((OldWin = NewWin) or (NewWin = nil));

    if not Result then
      exit;

    // that's real window of our => form
    Result := TQtMainWindow(OldWin.Handle).MDIChildArea <> nil;
    // Result := Result and ((NewWin = nil) or (TQtMainWindow(NewWin.Handle).MDIChildArea <> nil));
    if Result then
      Result := (OldWin = OldWidget.LCLObject) or
        ((NewWin = nil) or (NewWin = NewWidget.LCLObject));
  end;

  procedure LostFocus;
  begin
    FillChar(Msg, SizeOf(Msg), 0);
    if IsValidHandle(HWND(OldWidget)) then
    begin
      {$IF DEFINED(VerboseFocus) OR DEFINED(DebugQtFocus)}
      WriteLn('TQtWidgetSet.FocusChanged: KILL ', dbgsName(OldWidget.LCLObject),' W.Visible ',OldWidget.getVisible,
      ' destroying ? ',csDestroying in OldWidget.LCLObject.ComponentState,
      ' handle ?!? ',OldWidget.LCLObject.HandleAllocated);
      {$ENDIF}
      Msg.msg := LM_KILLFOCUS;
      Msg.wParam := PtrInt(NewWidget);
      if ((OldWidget is TQtMainWindow) and TQtMainWindow(OldWidget).IsMdiChild and
        Assigned(TQtMainWindow(OldWidget).LCLObject) and
        not (csDesigning in TQtMainWindow(OldWidget).LCLObject.ComponentState))
        or MDIFocusFixNeeded then
      begin
        // DO NOT TRIGGER ANYTHING, THIS IS SPURIOUS EVENT FROM MDIAREA
        //issue #29528
        if TQtMainWindow(OldWidget).IsMdiChild and (TQtMainWindow(OldWidget).LCLObject.ControlCount = 0) then
        begin
          {$IF DEFINED(VerboseFocus) OR DEFINED(DebugQtFocus)}
          Writeln('TQtWidgetSet.FocusChanged: *** SEND KILL FOCUS FOR MDICHILD WITH 0 CONTROLS ***');
          {$ENDIF}
          OldWidget.DeliverMessage(Msg);
        end else
        begin
          if (OldWidget is TQtComboBox) and CheckIfActiveForm(OldWidget) then
          begin
            if (NewWidget = nil) and QWidget_isVisible(aold) and QWidget_isEnabled(aold) and
              (TQtComboBox(OldWidget).LineEdit = nil) then
            begin
              {$IF DEFINED(VerboseFocus) OR DEFINED(DebugQtFocus)}
              DebugLn('WARNING: Qt mismatched kill focus message, backfocus to ',dbgsName(OldWidget.LCLObject),
              ' LCLObject focus status ',dbgs(OldWidget.LCLObject.Focused));
              {$ENDIF}
              OldWidget.setFocus;
            end else
              OldWidget.DeliverMessage(Msg);
          end else
          begin
            {$IF DEFINED(VerboseFocus) OR DEFINED(DebugQtFocus)}
            Writeln('TQtWidgetSet.FocusChanged: *** DO NOT KILL FOCUS FOR ',dbgsName(OldWidget),' ***');
            {$ENDIF}
          end;
        end;
      end else
        if CheckIfActiveForm(OldWidget) then
        begin
          if (NewWidget = nil) and QWidget_isVisible(aold) and QWidget_isEnabled(aold) and
            (OldWidget is TQtComboBox) and (TQtComboBox(OldWidget).LineEdit = nil) then
          begin
            {$IF DEFINED(VerboseFocus) OR DEFINED(DebugQtFocus)}
            DebugLn('WARNING: Qt mismatched kill focus message, backfocus to ',dbgsName(OldWidget.LCLObject),
            ' LCLObject focus status ',dbgs(OldWidget.LCLObject.Focused));
            {$ENDIF}
            OldWidget.setFocus;
          end else
            OldWidget.DeliverMessage(Msg);
        end
        {$IF DEFINED(VerboseFocus) OR DEFINED(DebugQtFocus)}
        else
          Writeln('TQtWidgetSet.FocusChanged: ***** Cannot kill focus of ',dbgsName(OldWidget.LCLObject))
        {$ENDIF}
        ;
    end;
  end;

begin
  {$IF DEFINED(VerboseFocus) OR DEFINED(DebugQtFocus)}
  WriteLn('> ** TQtWidgetSet.FocusChanged: old: ', dbgHex(PtrUInt(aold)), ' new: ', dbgHex(PtrUInt(anew)));
  {$ENDIF}
  if (AOld <> nil) and not QWidget_isVisible(AOld) then
    OldWidget := nil
  else
    OldWidget := GetFirstQtObjectFromWidgetH(aold);

  if (ANew <> nil) then
    NewWidget := GetFirstQtObjectFromWidgetH(anew)
  else
    NewWidget := nil;

  if OldWidget = NewWidget then
  begin
    {$IF DEFINED(VerboseFocus) OR DEFINED(DebugQtFocus)}
    WriteLn('TQtWidgetSet.FocusChanged: OldWidget = NewWidget ... exiting ...');
    {$ENDIF}
    Exit;
  end;

  {Applies to all TQtWidgets which have "subwidgets" created
   by CreateFrom() eg. comboBox.}
  if (OldWidget <> nil) and
     (NewWidget <> nil) and
     (OldWidget.LCLObject = NewWidget.LCLObject) then
  begin
    {$IF DEFINED(VerboseFocus) OR DEFINED(DebugQtFocus)}
    WriteLn('TQtWidgetSet.FocusChanged: exiting ... '+
    'OldWidget.LCLObject=NewWidget.LCLObject OBJ=',dbgsName(OldWidget.LCLObject));
    {$ENDIF}
    exit;
  end;

  if IsValidHandle(HWND(NewWidget)) and (NewWidget.getOwner <> nil) then
    NewWidget := NewWidget.getOwner;
  if IsValidHandle(HWND(OldWidget)) and (OldWidget.getOwner <> nil) then
    OldWidget := OldWidget.getOwner;

  Msg.Msg := 0; // shutup compiler

  // issue #26106
  LostFocus;

  FillChar(Msg, SizeOf(Msg), 0);

  if IsValidHandle(HWND(NewWidget)) then
  begin
    {$IF DEFINED(VerboseFocus) OR DEFINED(DebugQtFocus)}
    WriteLn('TQtWidgetSet.FocusChanged: SET ', dbgsName(NewWidget.LCLObject));
    {$ENDIF}
    Msg.msg := LM_SETFOCUS;
    Msg.wParam := PtrInt(OldWidget);
    if (NewWidget is TQtMainWindow) and (TQtMainWindow(NewWidget).IsMdiChild) and
      Assigned(TQtMainWindow(NewWidget).LCLObject) and
      not (csDesigning in TQtMainWindow(NewWidget).LCLObject.ComponentState) then
    begin
      // DO NOT TRIGGER ANYTHING, THIS IS SPURIOUS EVENT FROM MDIAREA
      FocusedQtWidget := QWidget_focusWidget(NewWidget.Widget);
      {$IF DEFINED(VerboseFocus) OR DEFINED(DebugQtFocus)}
      Writeln('TQtWidgetSet.FocusChanged: *** DO NOT SET FOCUS ***',dbgHex(PtrUInt(FocusedQtWidget)));
      {$ENDIF}
      if FocusedQtWidget <> nil then
      begin
        FocusedTQtWidget := TQtWidget(HwndFromWidgetH(FocusedQtWidget));
        if FocusedTQtWidget <> nil then
        begin
          if (FocusedTQtWidget.getOwner <> nil) then
            FocusedTQtWidget := FocusedTQtWidget.getOwner;
          if FocusedTQtWidget = NewWidget then
          begin
            {$IF DEFINED(VerboseFocus) OR DEFINED(DebugQtFocus)}
            writeln('TQtWidgetSet.FocusChanged: WE CANNOT FOCUS (segfault) ',dbgsName(FocusedTQtWidget.LCLObject),
            ' Active ? ',TCustomForm(NewWidget.LCLObject).Active);
            {$ENDIF}
            if Assigned(TCustomForm(NewWidget.LCLObject).ActiveControl) then
            begin
              {$IF DEFINED(VerboseFocus) OR DEFINED(DebugQtFocus)}
              writeln('TQtWidgetSet.FocusChanged: THIS ONE SHOULD BE FOCUSED (1) : ',dbgsName(TCustomForm(NewWidget.LCLObject).ActiveControl));
              {$ENDIF}
              if TCustomForm(NewWidget.LCLObject).ActiveControl.HandleAllocated then
              begin
                // setFocus(TCustomForm(NewWidget.LCLObject).ActiveControl.Handle);
                FocusedTQtWidget := TQtWidget(TCustomForm(NewWidget.LCLObject).ActiveControl.Handle);
                if FocusedTQtWidget <> nil then
                begin
                  if (FocusedTQtWidget.getOwner <> nil) then
                    FocusedTQtWidget := FocusedTQtWidget.getOwner;
                  // first check if we are active subwin, if not then we'll trigger qt do
                  // do correct thing
                  if TQtMainWindow(NewWidget).MDIChildArea.ActiveSubWindow <> NewWidget.Widget then
                    TQtMainWindow(NewWidget).MDIChildArea.ActivateSubWindow(QMDISubWindowH(NewWidget.Widget))
                  else
                  begin
                    // if we are already active then just inform lcl
                    Msg.msg := LM_SETFOCUS;
                    if OldWidget = FocusedTQtWidget then
                      OldWidget := nil;
                    Msg.wParam := PtrInt(OldWidget);
                    FocusedTQtWidget.DeliverMessage(Msg);
                  end;
                end;
              end else
                {$IF DEFINED(VerboseFocus) OR DEFINED(DebugQtFocus)}
                writeln('TQtWidgetSet.FocusChanged: BUT NO HANDLE ... CRAP: ',dbgsName(TCustomForm(NewWidget.LCLObject).ActiveControl))
                {$ENDIF}
                ;

            end else
            begin
              // issue #29528
              if TQtMainWindow(NewWidget).LCLObject.ControlCount = 0 then
              begin
                {$IF DEFINED(VerboseFocus) OR DEFINED(DebugQtFocus)}
                writeln('TQtWidgetSet.FocusChanged: setting focus to mdiChild with 0 controls .....');
                {$ENDIF}
                Msg.msg := LM_SETFOCUS;
                if OldWidget = NewWidget then
                  OldWidget := nil;
                Msg.wParam := PtrInt(OldWidget);
                NewWidget.DeliverMessage(Msg);
              end else
              begin
                // if this happens then qt's mdi focus is real crap
                {$IF DEFINED(VerboseFocus) OR DEFINED(DebugQtFocus)}
                writeln('TQtWidgetSet.FocusChanged: WE ARE COMPLETELY OUT OF MIND WHAT TO DO (1) .....');
                {$ENDIF}
              end;
            end;

          end else
          begin
            // should never happen
            {$IF DEFINED(VerboseFocus) OR DEFINED(DebugQtFocus)}
            if Assigned(TCustomForm(NewWidget.LCLObject).ActiveControl) then
              writeln('TQtWidgetSet.FocusChanged: THIS ONE SHOULD BE FOCUSED (2) : ',dbgsName(TCustomForm(NewWidget.LCLObject).ActiveControl))
            else
              writeln('TQtWidgetSet.FocusChanged: WE ARE COMPLETELY OUT OF MIND WHAT TO DO (2) .....');
            {$ENDIF}
          end;

        end;
      end;

    end else
    begin
      if Assigned(NewWidget.LCLObject) and NewWidget.LCLObject.IsVisible then
          NewWidget.DeliverMessage(Msg);
    end;
  end;
end;

procedure TQtWidgetSet.OnWakeMainThread(Sender: TObject);
var
  Event: QEventH;
begin
  Event := QEvent_create(LCLQt_CheckSynchronize);
  QCoreApplication_postEvent(QCoreApplication_instance(), Event, 1 {high priority});
end;

procedure TQtWidgetSet.SlotCommitDataRequest(sessionManager: QSessionManagerH);
  cdecl;
var
  ACancel: Boolean;
begin
  ACancel := False;
  {$IFDEF QTDEBUGSESSIONMANAGER}
  DebugLn('TQtWidgetSet.SlotCommitDataRequest allowInteraction ? ',dbgs(QSessionManager_allowsInteraction(sessionManager)),
    ' errorInteraction ',dbgs(QSessionManager_allowsErrorInteraction(sessionManager)),
    ' phase2 ',dbgs(QSessionManager_isPhase2(sessionManager)));
  {$ENDIF}
  {$IF NOT DEFINED(HAIKU) AND NOT DEFINED(QTOPIA)}
  // if session manager does not allow interaction, then we close app without any intf calls
  if QSessionManager_allowsInteraction(sessionManager) then
  begin
    Application.IntfQueryEndSession(ACancel);
    if ACancel then
    begin
      {$IFDEF QTDEBUGSESSIONMANAGER}
      DebugLn('*** App shutdown cancelled ...***');
      {$ENDIF}
      QSessionManager_cancel(sessionManager);
    end else
    begin
      Application.IntfEndSession;
      QApplication_hook_destroy(FAppSessionQuit);
      FAppSessionQuit := nil;
      {$IFDEF QTDEBUGSESSIONMANAGER}
      DebugLn('*** App shutdown releasing sessionManager ...***');
      {$ENDIF}
      QSessionManager_release(sessionManager);
    end;
  end;
  {$ENDIF}
end;

procedure TQtWidgetSet.SlotSaveDataRequest(sessionManager: QSessionManagerH);
  cdecl;
begin
  {$IFDEF QTDEBUGSESSIONMANAGER}
  DebugLn('TQtWidgetSet.SlotSaveDataRequest ');
  {$ENDIF}
end;

function TQtWidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:= lpQT;
end;

function TQtWidgetSet.GetLCLCapability(ACapability: TLCLCapability): PtrUInt;
begin
  case ACapability of
    lcEmulatedMDI,
    lcCanDrawOutsideOnPaint: Result := LCL_CAPABILITY_NO;
    lcDragDockStartOnTitleClick: Result :=
      {$ifdef MSWINDOWS} LCL_CAPABILITY_YES {$else} LCL_CAPABILITY_NO {$endif};
    lcNeedMininimizeAppWithMainForm: Result :=
      {$ifdef HASX11} LCL_CAPABILITY_YES {$else} LCL_CAPABILITY_NO {$endif};
    {when issue #20475 is fixed, then set this to LCL_CAPABILITY_YES}
    lcReceivesLMClearCutCopyPasteReliably: Result := LCL_CAPABILITY_NO;
    lcRadialGradientBrush: Result := LCL_CAPABILITY_YES;
    lcTransparentWindow: Result := LCL_CAPABILITY_YES;
  else
    Result := inherited GetLCLCapability(ACapability);
  end;
end;

function TQtWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
var
  Color: QColorH;
begin
  Result := clNone;

  if not IsValidDC(CanvasHandle) then Exit;
  
  if (TQtDeviceContext(CanvasHandle).vImage <> nil) then
  begin
    Color := QColor_create(QImage_pixel(TQtDeviceContext(CanvasHandle).vImage.Handle, X, Y));
    Result := RGBToColor(QColor_red(Color), QColor_green(Color), QColor_blue(Color));
    QColor_destroy(Color);
  end;
end;

procedure dbgcolor(msg: string; C:TQColor);
begin
  debugLn(msg+' spec=%x alpha=%x r=%x g=%x b=%x pad=%x',[c.ColorSpec,c.Alpha,c.r,c.g,c.b,c.pad]);
end;

procedure TQtWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
var
  Color: TQColor;
  AQColor: QColorH;
  ColorRef: TColorRef;
  Pen: QPenH;
  Painter: QPainterH;
  ADevType: Integer;
begin
  if IsValidDC(CanvasHandle) then
  begin
    // WriteLn('TQtWidgetSet.DCSetPixel X=',X,' Y=',Y, ' AColor=',dbghex(AColor),' rgb ? ',dbgHex(ColorToRGB(AColor)));
    Painter := TQtDeviceContext(CanvasHandle).Widget;
    ADevType := QPaintDevice_devType(QPaintEngine_paintDevice(QPainter_paintEngine(Painter)));
    {qt private PaintDeviceFlags 2 = QPixmap 3 = QImage. issue #29256}
    if ((ADevType = 2) or (ADevType = 3)) and
     (TQtDeviceContext(CanvasHandle).vImage <> nil) and
      (TQtDeviceContext(CanvasHandle).vImage.Handle <> nil) then
    begin
      ColorRef := TColorRef(ColorToRGB(AColor));
      QColor_fromRgb(@Color, Red(ColorRef), Green(ColorRef), Blue(ColorRef));
      AQColor := QColor_create(PQColor(@Color));
      QImage_setPixel(TQtDeviceContext(CanvasHandle).vImage.Handle, X, Y, QColor_rgb(AQColor));
      QColor_destroy(AQColor);
    end else
    begin
      {Save current pen.Better save copy of pen instead of
       using painter save/restore, or saved Pen in devicecontext which
       may be null. Issue #27620}
      Pen := QPen_create(QPainter_pen(Painter));
      try
        ColorRef := TColorRef(ColorToRGB(AColor));
        QColor_fromRgb(@Color, Red(ColorRef), Green(ColorRef), Blue(ColorRef));
        QPainter_setPen(Painter, PQColor(@Color));
        QPainter_drawPoint(Painter, X,Y);
      finally
        QPainter_setPen(Painter, Pen);
        QPen_destroy(Pen);
      end;
    end;
  end;
end;

procedure TQtWidgetSet.DCRedraw(CanvasHandle: HDC);
begin
  // TODO: implement me
end;

procedure TQtWidgetSet.DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean);
var
  DC: TQtDeviceContext;
begin
  if IsValidDC(CanvasHandle) then
  begin
    if CanvasHandle = 1 then
      DC := QtDefaultContext
    else
      DC := TQtDeviceContext(CanvasHandle);
    DC.setRenderHint(QPainterAntialiasing, AEnabled);
  end;
end;

procedure TQtWidgetSet.SetDesigning(AComponent: TComponent);
begin

end;

{------------------------------------------------------------------------------
  Function: TQtWidgetSet.IsValidDC
  Params:   DC     -  handle to a device context (TQtDeviceContext)
  Returns:  True   -  if the DC is valid
 ------------------------------------------------------------------------------}
function TQtWidgetSet.IsValidDC(const DC: HDC): Boolean;
begin
  Result := (DC <> 0);
end;

{------------------------------------------------------------------------------
  Function: TQtWidgetSet.IsValidGDIObject
  Params:   GDIObject  -  handle to a GDI Object (TQtFont, TQtBrush, etc)
  Returns:  True       -  if the DC is valid
  
  Remark: All handles for GDI objects must be pascal objects so we can
 distinguish between them
 ------------------------------------------------------------------------------}
function TQtWidgetSet.IsValidGDIObject(const GDIObject: HGDIOBJ): Boolean;
var
  aObject: TObject;
begin
  Result := False;

  if not QtGDIObjects.IsValidGDIObject(GDIObject) then
    exit;
  
  aObject := TObject(GDIObject);
  try
    if aObject is TObject then
    begin
      Result :=
        (aObject is TQtFont) or
        (aObject is TQtBrush) or
        (aObject is TQtImage) or
        (aObject is TQtPen) or
        (aObject is TQtRegion);
    end;
  except
    // DebugLn(['TQtWidgetSet.IsValidGDIObject: Gdi object ', GDIObject, ' is not an object!']);
    raise Exception.CreateFmt('TQtWidgetSet.IsValidGDIObject: %u is not an object ',[PtrUInt(GDIObject)]);
  end;
end;

procedure TQtWidgetSet.AddHandle(AHandle: TObject);
begin
  System.EnterCriticalsection(CriticalSection);
  if not SavedHandlesList.HasId(AHandle) then
    SavedHandlesList.Add(AHandle, AHandle);
  System.LeaveCriticalsection(CriticalSection);
end;

procedure TQtWidgetSet.RemoveHandle(AHandle: TObject);
begin
  System.EnterCriticalsection(CriticalSection);
  if SavedHandlesList.HasId(AHandle) then
    SavedHandlesList.Delete(AHandle);
  System.LeaveCriticalsection(CriticalSection);
end;

function TQtWidgetSet.IsValidHandle(AHandle: HWND): Boolean;
begin
  if (AHandle = 0) then
    Exit(False);
  System.EnterCriticalsection(CriticalSection);
  Result := SavedHandlesList.HasId(TObject(AHandle));
  System.LeaveCriticalsection(CriticalSection);
end;

procedure TQtWidgetSet.RegisterSysTrayIcon(AHandle: TObject);
begin
  SysTrayIconsList.Add(AHandle);
end;

procedure TQtWidgetSet.UnRegisterSysTrayIcon(AHandle: TObject);
begin
  SysTrayIconsList.Remove(AHandle);
end;

function TQtWidgetSet.IsValidSysTrayIcon(AHandle: HWND): Boolean;
begin
  Result := SysTrayIconsList.IndexOf(TObject(AHandle)) >= 0;
end;

{$IFDEF HASX11}
procedure TQtWidgetSet.AddHintHandle(AHandle: TObject);
begin
  System.EnterCriticalsection(CriticalSection);
  if SavedHintHandlesList.IndexOf(AHandle) < 0 then
    SavedHintHandlesList.Add(AHandle);
  System.LeaveCriticalsection(CriticalSection);
end;

procedure TQtWidgetSet.RemoveHintHandle(AHandle: TObject);
var
  AIndex: Integer;
begin
  System.EnterCriticalsection(CriticalSection);
  AIndex := SavedHintHandlesList.IndexOf(AHandle);
  if AIndex >= 0 then
    SavedHintHandlesList.Delete(AIndex);
  System.LeaveCriticalsection(CriticalSection);
end;

function TQtWidgetSet.IsValidHintHandle(AHandle: TObject): Boolean;
begin
  if (AHandle = nil) then
    Exit(False);
  System.EnterCriticalsection(CriticalSection);
  Result := SavedHintHandlesList.IndexOf(AHandle) >= 0;
  System.LeaveCriticalsection(CriticalSection);
end;

procedure TQtWidgetSet.HideAllHints;
var
  i: Integer;
  AWidget: TQtHintWindow;
begin
  System.EnterCriticalsection(CriticalSection);
  try
    Application.CancelHint;
    if not Assigned(SavedHintHandlesList) then
      exit;
    for i := SavedHintHandlesList.Count - 1 downto 0 do
    begin
      if IsValidHintHandle(TObject(SavedHintHandlesList.Items[i])) then
      begin
        AWidget := TQtHintWindow(SavedHintHandlesList.Items[i]);
        AWidget.NeedRestoreVisible := AWidget.getVisible and
          Assigned(AWidget.LCLObject) and AWidget.LCLObject.Visible;
        AWidget.Hide;
      end;
    end;
  finally
    System.LeaveCriticalsection(CriticalSection);
  end;
end;

procedure TQtWidgetSet.RestoreAllHints;
var
  i: Integer;
  AWidget: TQtHintWindow;
begin
  System.EnterCriticalsection(CriticalSection);
  try
    if not Assigned(SavedHintHandlesList) then
      exit;
    for i := SavedHintHandlesList.Count - 1 downto 0 do
    begin
      if IsValidHintHandle(TObject(SavedHintHandlesList.Items[i])) then
      begin
        AWidget := TQtHintWindow(SavedHintHandlesList.Items[i]);
        if AWidget.NeedRestoreVisible and Assigned(AWidget.LCLObject) and
          AWIdget.LCLObject.Visible then
        begin
          AWidget.NeedRestoreVisible := False;
          AWidget.Show;
        end;
      end;
    end;
  finally
    System.LeaveCriticalsection(CriticalSection);
  end;
end;

{$ENDIF}

procedure TQtWidgetSet.ClearGlobalActions;
begin
  {$IFDEF QT_DEBUG_GLOBALACTIONS}
  writeln('TQtWidgetSet.ClearGlobalActions');
  {$ENDIF}
  FGlobalActions.Clear;
end;

procedure TQtWidgetSet.AddGlobalAction(AnAction: QActionH);
begin
  {$IFDEF QT_DEBUG_GLOBALACTIONS}
  writeln('TQtWidgetSet.AddGlobalAction() AnAction ',dbgHex(PtrUInt(AnAction)));
  {$ENDIF}
  FGlobalActions.Add(AnAction);
end;

function TQtWidgetSet.ShortcutInGlobalActions(const AMnemonicText: WideString;
  out AGlobalActionIndex: Integer): Boolean;
var
  NewKey: QKeySequenceH;
  NewStr: WideString;
  CurrentKey: QKeySequenceH;
  CurrentStr: WideString;
  Action: QActionH;
  i: Integer;
begin
  {$IFDEF QT_DEBUG_GLOBALACTIONS}
  writeln('TQtWidgetSet.ShortcutInGlobalActions ',AMnemonicText);
  {$ENDIF}
  Result := False;
  AGlobalActionIndex := -1;
  NewKey := QKeySequence_create();
  try
    QKeySequence_fromString(NewKey, @AMnemonicText);
    NewStr := '';
    QKeySequence_toString(NewKey, @NewStr);

    {$IFDEF QT_DEBUG_GLOBALACTIONS}
    writeln('TQtWidgetSet.ShortcutInGlobalActions new seq=',NewStr);
    {$ENDIF}

    for i := 0 to FGlobalActions.Count - 1 do
    begin
      Action := QActionH(FGlobalActions.Items[i]);
      CurrentStr := '';
      QAction_text(Action, @CurrentStr);
      CurrentKey := QKeySequence_create();
      try
        QKeySequence_mnemonic(CurrentKey, @CurrentStr);
        if not QKeySequence_isEmpty(CurrentKey) then
        begin
          QKeySequence_toString(CurrentKey, @CurrentStr);
          {$IFDEF QT_DEBUG_GLOBALACTIONS}
          writeln('TQtWidgetSet.ShortcutInGlobalActions CurrentKey ',
            CurrentStr,' NewKey ',NewStr,' Result ? ',CurrentStr = NewStr);
          {$ENDIF}
          Result := CurrentStr = NewStr;
          AGlobalActionIndex := i;
          if Result then
            break;
        end;
      finally
        QKeySequence_destroy(CurrentKey);
      end;
    end;
  finally
    QKeySequence_destroy(NewKey);
  end;
end;

procedure TQtWidgetSet.TriggerGlobalAction(const ActionIndex: Integer);
var
  Action: QActionH;
  MainWin: TQtMainWindow;
begin
  Action := QActionH(FGlobalActions[ActionIndex]);
  if (Action <> nil) and Assigned(Application.MainForm) and
    (Application.MainForm.HandleAllocated) then
  begin
    MainWin := TQtMainWindow(Application.MainForm.Handle);
    MainWin.Activate;
    QMenuBar_setActiveAction(QMenuBarH(MainWin.MenuBar.Widget), Action);
  end;
end;

{Params: HWND
 This function is needed by cache used in TQtWidgetSet.WindowFromPoint().
 Returns: True if we are cached (FLastWFPResult).
}
function TQtWidgetSet.IsWidgetAtCache(AHandle: HWND): Boolean;
begin
  Result := AHandle = FLastWFPResult;
end;

{Params: none
 Invalidates TQtWidgetSet.WindowFromPoint() cache (FLastWFPResult).
 Returns: nothing
}
procedure TQtWidgetSet.InvalidateWidgetAtCache;
begin
  FLastWFPResult := 0;
end;

{Params: none
 Returns: True if last cached FLastWFPResult is valid otherwise False.
}
function TQtWidgetSet.IsValidWidgetAtCachePointer: Boolean;
begin
  if FLastWFPResult = 0 then
    exit(False);
  Result := IsValidHandle(FLastWFPResult);
end;

{Params: none
 Returns last cached FLastWFPMousePos
 Returns: TPoint
}
function TQtWidgetSet.GetWidgetAtCachePoint: TPoint;
begin
  Result := FLastWFPMousePos;
end;

function TQtWidgetSet.DragImageList_BeginDrag(AImage: QImageH;
  AHotSpot: TPoint): Boolean;
var
  ASize: TSize;
  APixmap: QPixmapH;
  AMask: QBitmapH;
  ABrush: QBrushH;
  APalette: QPaletteH;
begin
  if FDragImageList = nil then
  begin
    FDragImageList := QWidget_create(nil,
      QtSubWindow or QtFramelessWindowHint or
      QtWindowStaysOnTopHint {$IFDEF HASX11}or QtX11BypassWindowManagerHint{$ENDIF});

    // do not set focus and do not activate this widget
    QWidget_setFocusPolicy(FDragImageList, QtNoFocus);
    QWidget_setAttribute(FDragImageList, QtWA_ShowWithoutActivating, True);

    QImage_size(AImage, @ASize);
    QWidget_setFixedSize(FDragImageList, @ASize);
    APixmap := QPixmap_create();
    QPixmap_fromImage(APixmap, AImage);
    AMask := QBitmap_create();
    QPixmap_mask(APixmap, AMask);
    QWidget_setMask(FDragImageList, AMask);
    // issue #26464, use QPixmap instead of QImage in QBrush constructor.
    ABrush := QBrush_create(APixmap);
    APalette := QWidget_palette(FDragImageList);
    QPalette_setBrush(APalette, QPaletteWindow, ABrush);
    QBrush_destroy(ABrush);
    QBitmap_destroy(AMask);
    QPixmap_destroy(APixmap);
    
    QWidget_setAutoFillBackground(FDragImageList, True);
    
    FDragHotSpot := AHotSpot;
  end;
  Result := FDragImageList <> nil;
end;

procedure TQtWidgetSet.DragImageList_EndDrag;
begin
  if FDragImageList <> nil then
  begin
    QObject_deleteLater(FDragImageList);
    FDragImageList := nil;
  end;
end;

function TQtWidgetSet.DragImageList_DragMove(X, Y: Integer): Boolean;
begin
  Result := FDragImageList <> nil;
  if Result then
  begin
    QWidget_raise(FDragImageList);
    QWidget_move(FDragImageList, X - FDragHotSpot.X, Y - FDragHotSpot.Y);
  end;
end;

function TQtWidgetSet.DragImageList_SetVisible(NewVisible: Boolean): Boolean;
begin
  Result := FDragImageList <> nil;
  if Result then
    QWidget_setVisible(FDragImageList, NewVisible);
end;

{$IFDEF MSWINDOWS}
function TQtWidgetSet.GetWinKeyState(AKeyState: LongInt): SHORT;
begin
  Result := Windows.GetKeyState(AKeyState);
end;
{$ENDIF}

{------------------------------------------------------------------------------
  Function: CreateDefaultFont
  Params:  none
  Returns: a TQtFont object

  Creates an default font, used for initial values
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateDefaultFont: HFONT;
var
  QtFont: TQtFont;
begin
  QtFont := TQtFont.Create(True);
  QtFont.FShared := True;
  QApplication_font(QtFont.FHandle);
  Result := HFONT(QtFont);
end;

function TQtWidgetSet.GetDefaultAppFontName: WideString;
begin
  Result := FDefaultAppFontName;
end;

procedure TQtWidgetSet.DeleteDefaultDC;
begin
  if FStockDefaultDC <> 0 then
  TQtDeviceContext(FStockDefaultDC).Free;
  FStockDefaultDC := 0;
end;

procedure TQtWidgetSet.FreeStockItems;

  procedure DeleteAndNilObject(var h: HGDIOBJ);
  begin
    if h <> 0 then
      TQtResource(h).FShared := False;
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

procedure TQtWidgetSet.FreeSysColorBrushes(const AInvalidateHandlesOnly: Boolean = False);

  procedure DeleteAndNilObject(var h: HGDIOBJ);
  begin
    if h <> 0 then
    begin
      TQtResource(h).FShared := False;
      DeleteObject(h);
      h := 0;
    end;
  end;

  procedure InvalidateHandleOnly(AIndex: Integer; h: HGDIOBJ);
  begin
    if (h <> 0) and (TQtBrush(h).FHandle <> nil) then
    begin
      QBrush_destroy(TQtBrush(h).FHandle);
      TQtBrush(h).FHandle := nil;
      getSysColorBrush(AIndex);
    end;
  end;

var
  i: integer;
begin
  for i := Low(FSysColorBrushes) to High(FSysColorBrushes) do
    if AInvalidateHandlesOnly then
      InvalidateHandleOnly(i, FSysColorBrushes[i])
    else
      DeleteAndNilObject(FSysColorBrushes[i]);
end;

function TQtWidgetSet.GetQtDefaultDC: HDC;
begin
  Result := FStockDefaultDC;
end;

procedure TQtWidgetSet.SetQtDefaultDC(Handle: HDC);
begin
  FStockDefaultDC := Handle;
end;

procedure TQtWidgetSet.InitStockItems;
var
  LogBrush: TLogBrush;
  logPen : TLogPen;
begin
  FillChar(LogBrush{%H-},SizeOf(TLogBrush),0);
  LogBrush.lbStyle := BS_NULL;
  FStockNullBrush := CreateBrushIndirect(LogBrush);
  TQtBrush(FStockNullBrush).FShared := True;
  
  LogBrush.lbStyle := BS_SOLID;
  LogBrush.lbColor := $000000;
  FStockBlackBrush := CreateBrushIndirect(LogBrush);
  TQtBrush(FStockBlackBrush).FShared := True;
  
  LogBrush.lbColor := $C0C0C0;
  FStockLtGrayBrush := CreateBrushIndirect(LogBrush);
  TQtBrush(FStockLtGrayBrush).FShared := True;
  
  LogBrush.lbColor := $808080;
  FStockGrayBrush := CreateBrushIndirect(LogBrush);
  TQtBrush(FStockGrayBrush).FShared := True;
  
  LogBrush.lbColor := $404040;
  FStockDkGrayBrush := CreateBrushIndirect(LogBrush);
  TQtBrush(FStockDkGrayBrush).FShared := True;
  
  LogBrush.lbColor := $FFFFFF;
  FStockWhiteBrush := CreateBrushIndirect(LogBrush);
  TQtBrush(FStockWhiteBrush).FShared := True;

  LogPen.lopnStyle := PS_NULL;
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

  FStockSystemFont := 0; // styles aren't initialized yet
  
  FStockDefaultDC := 0; // app must be initialized
end;

function TQtWidgetSet.GetMenuHeight: Integer;
var
  AMenuBar: QMenuBarH;
  DummyWindow: QMainWindowH;
  DummyStr: WideString;
  Size: TSize;
begin
  {$IFDEF DARWIN}
  Exit(0);
  {$ENDIF}
  if FCachedMenuBarHeight = -1 then
  begin
    DummyWindow := QMainWindow_create(QApplication_desktop());
    QWidget_setVisible(DummyWindow, False);
    AMenuBar := QMenuBar_create();
    DummyStr := 'DUMMY BAR';
    QMenuBar_addMenu(AMenuBar, @DummyStr);
    QMainWindow_setMenuBar(DummyWindow, AMenuBar);
    QMenuBar_sizeHint(AMenuBar, @Size);
    QMainWindow_destroy(DummyWindow);
    FCachedMenuBarHeight := Size.cy;
  end;
  if (FCachedMenuBarHeight <= 0) then
  begin
    FCachedMenuBarHeight := 22;
  end;
  Result := FCachedMenuBarHeight;
end;

{$IFDEF QtUseAccurateFrame}
function TQtWidgetSet.GetFrameMargins: TRect;
var
  ATempWidget: QWidgetH;
  R: TRect;
  ASize, AFrameSize: TSize;
  AID: LongWord;
  I: integer;
  B: Boolean;
  ATicks: DWORD;
  {$IFDEF DebugQtUseAccurateFrame}
  StartupTime: DWord;
  {$ENDIF}
  {$IFDEF HASX11}
  AWMName: String;
  //AShadowRect: TRect;
  //AList: TStringList;
  ALeft, ATop, AWidth, AHeight, ABorder: integer;
  {$ENDIF}
begin
  if (FWSFrameMargins.Left = 0) and (FWSFrameMargins.Top = 0) or
    (FWSFrameMargins.Right = 0) and (FWSFrameMargins.Bottom = 0) then
  begin
    {$IFDEF HASX11}
    AWMName := GetWindowManager;
    // blacklist of window managers which does not support NET_FRAME_EXTENTS
    if (AWMName = 'Blackbox') then
    begin
      DebugLn('WARNING: QtLCL: "Blackbox" wm does not implement for _NET_FRAME_EXTENTS.');
      FWSFrameMargins := Rect(1, 23, 1, 10);
      Result := FWSFrameMargins;
      exit;
    end;
    {$ENDIF}

    {$IFDEF DebugQtUseAccurateFrame}
    StartupTime := GetTickCount;
    {$ENDIF}

    ATempWidget := QWidget_create(nil, QtWindow or QtDialog);
    QWidget_setAttribute(ATempWidget, QtWA_NativeWindow, True);

    {$IFDEF HASX11}
    if QX11Info_isCompositingManagerRunning then
      QWidget_setWindowOpacity(ATempWidget, 0);
    {$ENDIF}

    QWidget_move(ATempWidget, -MAXSHORT, -MAXSHORT);
    QWidget_resize(ATempWidget, 32, 32);

    {$IFDEF HASX11}
    QWidget_setAttribute(ATempWidget, QtWA_ShowWithoutActivating);
    QWidget_show(ATempWidget);
    AID := QWidget_winID(ATempWidget);

    B := AskX11_NET_REQUEST_FRAME_EXTENTS(AID, R);

    if B and (R.Top = 0) then
    begin
      ATicks := 0;
      while R.Top = 0 do
      begin
        QWidget_size(ATempWidget, @ASize);
        QWidget_frameSize(ATempWidget, @AFrameSize);
        R.Left := (AFrameSize.cx - ASize.cx) div 2;
        R.Right := R.Left;
        R.Top := (AFrameSize.cy - ASize.cy) - R.Left;
        R.Bottom := R.Left;
        QCoreApplication_processEvents;
        ATicks += 1;
        if ATicks > 50 then
          break; // error !
      end;
      (* TODO: If shadows are enabled on KWin ask for shadow rect !
      AList := TStringList.Create;
      try
        GetX11SupportedAtoms(AID, AList);
        GetX11RectForAtom(AID,'_KDE_NET_WM_SHADOW', AShadowRect);
      finally
        AList.Free;
      end;
      *)
    end;

    if B and QX11Info_isCompositingManagerRunning then
    begin
      {TODO: some wm's have different attributes for windows with fixed sizes and modal windows,
      so we need to query attributes for such windows too.
      Currently such composited wm's is "Mutter (Muffin)", "Gnome Shell"}
      // writeln('Current WM=',GetWindowManager);
      while not GetX11WindowRealized(QWidget_winID(ATempWidget)) do
        QApplication_syncX();
      if GetX11WindowAttributes(QWidget_winID(ATempWidget), ALeft, ATop, AWidth, AHeight, ABorder) then
      begin
        if (ALeft > 0) and (ATop > 0) then
          R := Rect(ALeft, ATop, ALeft, ALeft);
      end;
    end;
    if not B then
    begin
      MapX11Window(AID);
      QApplication_syncX();
    end;

    if not B and not GetX11RectForAtom(AID, '_NET_FRAME_EXTENTS', R) then
    begin
      AId := QWidget_winID(ATempWidget);
      I := 0;
      ATicks := GetTickCount;
      while not GetX11RectForAtom(AID, '_NET_FRAME_EXTENTS', R) do
      begin
        QApplication_syncX();
        QCoreApplication_processEvents;
        inc(I);
        {if we do not get frame extents in 100ms get out}
        if GetTickCount - ATicks > 100 then
          break;
      end;
      {$IFDEF DebugQtUseAccurateFrame}
      DebugLn('TQtWidgetSet.GetFrameMargins: EXIT LOOP AFTER ',dbgs(GetTickCount - ATicks),' ms LOOP=',dbgs(i),' R=',dbgs(R));
      {$ENDIF}
    end;
    {$ELSE}
    // create native window
    QWidget_winId(ATempWidget);
    QWidget_size(ATempWidget, @ASize);
    QWidget_frameSize(ATempWidget, @AFrameSize);
    R.Left := (AFrameSize.cx - ASize.cx) div 2;
    R.Right := R.Left;
    R.Top := (AFrameSize.cy - ASize.cy) - R.Left;
    R.Bottom := R.Left;
    {$ENDIF}

    FWSFrameMargins := R;
    {$IFDEF DebugQtUseAccurateFrame}
    // just test
    {$IFDEF HASX11}
    QWidget_size(ATempWidget, @ASize);
    QWidget_frameSize(ATempWidget, @AFrameSize);
    {$ENDIF}
    with FWSFrameMargins do
      DebugLn('TQtWidgetSet.GetFrameMargins: **MARGINS=',Format('l %d t %d r %d b %d QtFrame x %d y %d compositor %s sysMetrics %d startup needed %d ms',
        [Left, Top, Right, Bottom, AFrameSize.cx - ASize.cx, AFrameSize.cy - ASize.cy, BoolToStr({$IFDEF HASX11}QX11Info_isCompositingManagerRunning{$ELSE}False{$ENDIF}), GetSystemMetrics(SM_CYCAPTION),GetTickCount - StartupTime]));
    {$ENDIF}
    QWidget_hide(ATempWidget);
    QWidget_destroy(ATempWidget);
  end;
  Result := FWSFrameMargins;
end;
{$ENDIF}

procedure TQtWidgetSet.ClearCachedColors;
var
  i: Integer;
begin
  for i := 0 to High(FCachedColors) do
  begin
    if FCachedColors[i] <> nil then
      FreeMem(FCachedColors[i]);
    FCachedColors[i] := nil;
  end;
end;

function TQtWidgetSet.GetStyleName: String;
var
  WStr: WideString;
begin
  QObject_objectName(QApplication_style, @WStr);
  Result := {%H-}WStr;
end;

//------------------------------------------------------------------------
