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

procedure TCDWidgetSet.AccumulatingDebugOut(AStr: string);
begin
  AccumulatedStr := AccumulatedStr + AStr;
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCDWidgetSet.Create;
begin
  inherited Create;

  CDWidgetSet := Self;
  FTerminating := False;

  BackendCreate;
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCDWidgetSet.Destroy;
begin
  BackendDestroy;

  CDWidgetSet := nil;
  inherited Destroy;
end;

(*
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
    {$ifdef CD_Cocoa}
    lcNeedMininimizeAppWithMainForm,
    lcApplicationTitle,
    lcFormIcon,
    lcReceivesLMClearCutCopyPasteReliably:
      Result := LCL_CAPABILITY_NO;
    {$endif}
    lcAntialiasingEnabledByDefault: Result := LCL_CAPABILITY_NO;
    lcAllowChildControlsInNativeControls: Result := LCL_CAPABILITY_YES
  else
    Result := inherited GetLCLCapability(ACapability);
  end;
end;

function TCDWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
var
  LazCanvas: TLazCanvas;
begin
  Result := clNone;

  if not IsValidDC(CanvasHandle) then Exit;
  LazCanvas := TLazCanvas(CanvasHandle);
  
  Result := FPColorToTColor(LazCanvas.Colors[X, Y]);
end;

procedure TCDWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
var
  LazCanvas: TLazCanvas;
begin
  if not IsValidDC(CanvasHandle) then Exit;
  LazCanvas := TLazCanvas(CanvasHandle);

  LazCanvas.Colors[X, Y] := TColorToFPColor(AColor);
end;

procedure TCDWidgetSet.DCRedraw(CanvasHandle: HDC);
begin
  // TODO: implement me
end;

procedure TCDWidgetSet.DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean);
{var
  DC: TQtDeviceContext;}
begin
{  if IsValidDC(CanvasHandle) then
  begin
    if CanvasHandle = 1 then
      DC := QtDefaultContext
    else
      DC := TQtDeviceContext(CanvasHandle);
    DC.setRenderHint(QPainterAntialiasing, AEnabled);
  end;}
end;

procedure TCDWidgetSet.SetDesigning(AComponent: TComponent);
begin

end;

//------------------------------------------------------------------------
