{%MainUnit androidint.pas}
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

{------------------------------------------------------------------------------
  Method: TAndroidWidgetSet.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TAndroidWidgetSet.Create;
begin
{  FLastWFPMousePos := Point(MaxInt, MaxInt);
  FLastWFPResult := 0;

  inherited Create;

  App := QApplication_Create(@argc, argv);
  InitStockItems;
  QtWidgetSet := Self;
  ClearCachedColors;
  FDockImage := nil;
  FDragImageLock := False;
  System.InitCriticalSection(CriticalSection);
  SavedHandlesList := TMap.Create(TMapIdType(ituPtrSize), SizeOf(TObject));
  FSocketEventMap := TMap.Create(TMapIdType(its4), SizeOf(Pointer));
  StayOnTopList := nil;
  FEatNextDeactivate := False;}
end;

{------------------------------------------------------------------------------
  Method: TAndroidWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TAndroidWidgetSet.Destroy;
begin
{  if FDockImage <> nil then
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

  System.DoneCriticalsection(CriticalSection);}

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TAndroidWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Creates a new timer and sets the callback event.
 ------------------------------------------------------------------------------}
function TAndroidWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): THandle;
begin
end;

{------------------------------------------------------------------------------
  Method: TAndroidWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destroys a timer.
 ------------------------------------------------------------------------------}
function TAndroidWidgetSet.DestroyTimer(TimerHandle: THandle): boolean;
begin
end;

{------------------------------------------------------------------------------
  Method: TAndroidWidgetSet.AppInit
  Params:  None
  Returns: Nothing

  Initializes the application
 ------------------------------------------------------------------------------}
procedure TAndroidWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
begin
//  WakeMainThread := @OnWakeMainThread;

  {
    check whether this hook crashes on linux & darwin and why it is so
    we need this hook to catch release messages
  }
{  // install global event filter
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
  
  QtDefaultPrinter;}
  // initialize default app font name
//  SetDefaultAppFontName;
end;

{------------------------------------------------------------------------------
  Method: TAndroidWidgetSet.AppRun
  Params:  None
  Returns: Nothing

  Enter the main message loop
 ------------------------------------------------------------------------------}
procedure TAndroidWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
var
  MainFormHandle: TAndroidWindow;
begin
  if Application.MainForm <> nil then
  begin
    MainFormHandle := TAndroidWindow(Application.MainForm.Handle);
    if MainFormHandle <> nil then
      Activity.setContentView(MainFormHandle.MainView);
  end;

  vAndroidPipesComm.onCreateFinished();
  vAndroidPipesComm.MessageLoop();
end;

{------------------------------------------------------------------------------
  Method: TAndroidWidgetSet.AppWaitMessage
  Params:  None
  Returns: Nothing

  Waits until a message arrives, processes that and returns control out of the function
  
  Utilized on Modal dialogs
 ------------------------------------------------------------------------------}
procedure TAndroidWidgetSet.AppWaitMessage;
begin
  {we cannot call directly processEvents() with this flag
   since it produces AV's sometimes, so better check is there
   any pending event.}
//  QCoreApplication_processEvents(QEventLoopWaitForMoreEvents);
end;

{------------------------------------------------------------------------------
  Method: TAndroidWidgetSet.AppProcessMessages
  Params:  None
  Returns: Nothing

  Processes all messages on the quoue
 ------------------------------------------------------------------------------}
procedure TAndroidWidgetSet.AppProcessMessages;
begin
  {we must use QEventLoopDefferedDeletion because of SlotClose.
   Normal forms are NOT closed without this ...}
//  QCoreApplication_processEvents(QEventLoopAllEvents);
end;

{------------------------------------------------------------------------------
  Method: TAndroidWidgetSet.AppTerminate
  Params:  None
  Returns: Nothing

  Implements Application.Terminate and MainForm.Close.
 ------------------------------------------------------------------------------}
procedure TAndroidWidgetSet.AppTerminate;
begin
  // free hooks
//  QObject_hook_destroy(FAppEvenFilterHook);
//  QApplication_hook_destroy(FAppFocusChangedHook);

//  QCoreApplication_quit;
end;

procedure TAndroidWidgetSet.AppMinimize;
begin
end;

procedure TAndroidWidgetSet.AppRestore;
begin
end;

procedure TAndroidWidgetSet.AppBringToFront;
begin
end;

procedure TAndroidWidgetSet.AppSetIcon(const Small, Big: HICON);
begin
end;


procedure TAndroidWidgetSet.AppSetTitle(const ATitle: string);
begin
end;

function TAndroidWidgetSet.AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
end;

function TAndroidWidgetSet.AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
end;

function TAndroidWidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:= lpAndroid;
end;

function TAndroidWidgetSet.GetLCLCapability(ACapability: TLCLCapability): PtrUInt;
begin
(*  case ACapability of
    lcCanDrawOutsideOnPaint: Result := LCL_CAPABILITY_NO;
    lcDragDockStartOnTitleClick: Result :=
     {$ifdef MSWINDOWS} LCL_CAPABILITY_YES {$else} LCL_CAPABILITY_NO {$endif};
     lcNeedMininimizeAppWithMainForm: Result :=
     {$ifdef HASX11} LCL_CAPABILITY_YES {$else} LCL_CAPABILITY_NO {$endif};
  else
    Result := inherited GetLCLCapability(ACapability);
  end;*)
end;

function TAndroidWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
begin
end;

procedure TAndroidWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
begin
end;

procedure TAndroidWidgetSet.DCRedraw(CanvasHandle: HDC);
begin
  // TODO: implement me
end;

procedure TAndroidWidgetSet.DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean);
begin
end;

procedure TAndroidWidgetSet.SetDesigning(AComponent: TComponent);
begin

end;

{------------------------------------------------------------------------------
  Function: TAndroidWidgetSet.IsValidDC
  Params:   DC     -  handle to a device context (TQtDeviceContext)
  Returns:  True   -  if the DC is valid
 ------------------------------------------------------------------------------}
function TAndroidWidgetSet.IsValidDC(const DC: HDC): Boolean;
begin
  Result := (DC <> 0);
end;

{------------------------------------------------------------------------------
  Function: TAndroidWidgetSet.IsValidGDIObject
  Params:   GDIObject  -  handle to a GDI Object (TQtFont, TQtBrush, etc)
  Returns:  True       -  if the DC is valid
  
  Remark: All handles for GDI objects must be pascal objects so we can
 distinguish between them
 ------------------------------------------------------------------------------}
function TAndroidWidgetSet.IsValidGDIObject(const GDIObject: HGDIOBJ): Boolean;
var
  aObject: TObject;
begin
  Result := False;
  
  if GDIObject = 0 then Exit;
  
{  aObject := TObject(GDIObject);
  try
    if aObject is TObject then
    begin
      Result :=
        (aObject is TQtFont) or
        (aObject is TQtBrush) or
        (aObject is TQtImage) or
        (aObject is TQtPen) or
        (aObject is TQTRegion);
    end;
  except
    DebugLn(['Gdi object: ', GDIObject, ' is not an object!']);
    raise;
  end;}
end;


//------------------------------------------------------------------------
