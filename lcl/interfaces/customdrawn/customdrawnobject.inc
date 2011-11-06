{%MainUnit customdrawnint.pas}
{
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
//---------------------------------------------------------------
(*
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

  App := QApplication_Create(@argc, argv);
  {$J+}
  QtVersionInt(QtVersionMajor, QtVersionMinor, QtVersionMicro);
  {$J-}
  InitStockItems;
  QtWidgetSet := Self;
  ClearCachedColors;
  FDockImage := nil;
  FDragImageLock := False;
  System.InitCriticalSection(CriticalSection);
  SavedHandlesList := TMap.Create(TMapIdType(ituPtrSize), SizeOf(TObject));
  FSocketEventMap := TMap.Create(TMapIdType(its4), SizeOf(Pointer));
  StayOnTopList := nil;
  FEatNextDeactivate := False;
  {$IFDEF HASX11}
  FMinimizedByPager := False;
  FLastMinimizeEvent := 0;
  if ((QtVersionMajor = 4) and (QtVersionMinor < 6)) or IsOldKDEInstallation then
    QtX11InitializePalettes;
  FWindowManagerName := LowerCase(GetWindowManager);
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
  FSocketEventMap.Free;
  FGlobalActions.Free;

  System.DoneCriticalsection(CriticalSection);

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


  ScreenDC := GetDC(0);
  try
    ScreenInfo.PixelsPerInchX := GetDeviceCaps(ScreenDC, LOGPIXELSX);
    ScreenInfo.PixelsPerInchY := GetDeviceCaps(ScreenDC, LOGPIXELSY);
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
begin
  // use LCL loop
  if Assigned(ALoop) then
    ALoop;
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
  {we must use QEventLoopDefferedDeletion because of SlotClose.
   Normal forms are NOT closed without this ...}
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

  QCoreApplication_quit;
end;

procedure TQtWidgetSet.AppMinimize;
{$IFDEF HASX11}
var
  i: Integer;
  AForm: TCustomForm;
  States: QtWindowStates;
{$ENDIF}
begin
  if (Application.MainForm <> nil) and (Application.MainForm.HandleAllocated) then
  begin
    {$IFDEF HASX11}
    for i := 0 to Screen.CustomFormZOrderCount-1 do
    begin
      AForm := Screen.CustomFormsZOrdered[i];
      if (AForm.Parent=nil) and AForm.HandleAllocated and
        TQtWidget(AForm.Handle).getVisible and
        not (AForm.FormStyle in [fsMDIChild, fsSplash]) and
        not (AForm.BorderStyle in [bsNone]) then
      begin
        States := TQtMainWindow(AForm.Handle).getWindowState;
        if not TQtMainWindow(AForm.Handle).isMinimized then
          TQtMainWindow(AForm.Handle).setWindowState(States or QtWindowMinimized);
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
{$ENDIF}
begin
  if (Application.MainForm <> nil) and (Application.MainForm.HandleAllocated) then
  begin
    {$IFDEF HASX11}
    if Screen = nil then exit;
    for i := Screen.CustomFormZOrderCount-1 downto 0 do
    begin
      AForm := Screen.CustomFormsZOrdered[i];
      if (AForm.Parent=nil) and AForm.HandleAllocated and
        TQtWidget(AForm.Handle).getVisible and
        not (AForm.FormStyle in [fsMDIChild, fsSplash]) and
        not (AForm.BorderStyle in [bsNone]) then
      begin
        States := TQtMainWindow(AForm.Handle).getWindowState;
        if TQtMainWindow(AForm.Handle).isMinimized then
          TQtMainWindow(AForm.Handle).setWindowState(States and not QtWindowMinimized);
      end;
    end;
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
  W := GetUtf8String(ATitle);
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
  W: TQtMainWindow;
begin
  Result := False;
  case QEvent_type(Event) of
    QEventApplicationFontChange: SetDefaultAppFontName;
    QEventApplicationActivate:
      if Assigned(Application) and not FEatNextDeactivate then
      begin
        // check if activated form is StayOnTop, if it's so, we must
        // eat next appdeactivate & appactivate since we are changing form
        // flags !
        if (StayOnTopList <> nil) then
        begin
          W := TQtMainWindow(HWNDFromWidgetH(QApplication_activeWindow()));
          FEatNextDeactivate := StayOnTopList.HasId(W);
        end;
        Application.IntfAppActivate;
        QtRestoreStayOnTop;
        if (FEatNextDeactivate) and (W <> nil) then
          W.Activate;
      end else
        FEatNextDeactivate := False;

    QEventApplicationDeactivate:
      if Assigned(Application) and not FEatNextDeactivate then
      begin
        Application.IntfAppDeactivate;
        QtRemoveStayOnTop;
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
        AObject := TQtObject(Pointer(QLCLMessageEvent_getWParam(QLCLMessageEventH(Event))));
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

procedure TQtWidgetSet.FocusChanged(old: QWidgetH; now: QWidgetH); cdecl;
var
  OldWidget, NewWidget: TQtWidget;
  Msg: TLMessage;
begin
  // WriteLn('old: ', PtrUInt(old), ' new: ', PtrUInt(now));
  OldWidget := GetFirstQtObjectFromWidgetH(old);
  NewWidget := GetFirstQtObjectFromWidgetH(now);

  if OldWidget = NewWidget then
    Exit;

  {Applies to all TQtWidgets which have "subwidgets" created
   by CreateFrom() eg. comboBox.}
  if (OldWidget <> nil) and
     (NewWidget <> nil) and
     (OldWidget.LCLObject = NewWidget.LCLObject) then
    exit;

  Msg.Msg := 0; // shutup compiler
  FillChar(Msg, SizeOf(Msg), 0);
  if OldWidget <> nil then
  begin
    //WriteLn('KILL: ', OldWidget.LCLObject.ClassName);
    Msg.msg := LM_KILLFOCUS;
    Msg.wParam := PtrInt(NewWidget);
    OldWidget.DeliverMessage(Msg);
  end;
  if NewWidget <> nil then
  begin
    //WriteLn('SET: ', NewWidget.LCLObject.ClassName);
    Msg.msg := LM_SETFOCUS;
    Msg.wParam := PtrInt(OldWidget);
    NewWidget.DeliverMessage(Msg);
  end;
end;

procedure TQtWidgetSet.OnWakeMainThread(Sender: TObject);
var
  Event: QEventH;
begin
  Event := QEvent_create(LCLQt_CheckSynchronize);
  QCoreApplication_postEvent(QCoreApplication_instance(), Event, 1 {high priority});
end;*)

function TCDWidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:= lpCustomDrawn;
end;

function TCDWidgetSet.GetLCLCapability(ACapability: TLCLCapability): PtrUInt;
begin
  case ACapability of
    lcCanDrawOutsideOnPaint: Result := LCL_CAPABILITY_NO;
  else
    Result := inherited GetLCLCapability(ACapability);
  end;
end;
(*
function TQtWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
var
  Color: QColorH;
begin
  Result := clNone;

  if not IsValidDC(CanvasHandle) then Exit;
  
  if (TQtDeviceContext(CanvasHandle).vImage <> nil) then
  begin
    Color := QColor_create(QImage_pixel(TQtDeviceContext(CanvasHandle).vImage.FHandle, X, Y));
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
  ASavedColor: TQColor;
  Color: TQColor;
  Pen: QPenH;
  Painter: QPainterH;
begin
  if IsValidDC(CanvasHandle) then
  begin
    //WriteLn('TQtWidgetSet.DCSetPixel X=',X,' Y=',Y, ' AColor=',dbghex(AColor));
    Painter := TQtDeviceContext(CanvasHandle).Widget;
    Pen := QPainter_pen(Painter);
    QPen_color(Pen, @ASavedColor);
    QColor_fromRgb(@Color, ColorToRGB(AColor));
    QPainter_setPen(Painter, @Color);
    QPainter_drawPoint(Painter, X,Y);
    QPainter_setPen(Painter, @ASavedColor);
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
  
  if GDIObject = 0 then Exit;
  
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
    DebugLn(['Gdi object: ', GDIObject, ' is not an object!']);
    raise;
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
      QtSubWindow or QtFramelessWindowHint or QtWindowStaysOnTopHint);

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
    ABrush := QBrush_create(AImage);
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
  FillChar(LogBrush,SizeOf(TLogBrush),0);
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
end;*)

//------------------------------------------------------------------------
