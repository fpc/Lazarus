// included by win32int.pp

{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
Constructor TWin32WidgetSet.Create;
Begin
  Inherited Create;
  FTimerData  := TList.Create;
  FMetrics.cbSize := SizeOf(FMetrics);
  FMetricsFailed := not Windows.SystemParametersInfo(SPI_GETNONCLIENTMETRICS,
    SizeOf(FMetrics), @FMetrics, 0);
  if FMetricsFailed then
  begin
    FMetrics.iMenuHeight := GetSystemMetrics(SM_CYMENU);
  end;
  OnClipBoardRequest := nil;
  // see if XP themes are available, first check if correct
  // common control library is loaded for themes support
  if ((GetFileVersion('comctl32.dll') shr 16) and $FFFF) >= 6 then
  begin
    FThemeLibrary := LoadLibrary('uxtheme.dll');
    if FThemeLibrary <> 0 then
    begin
      // load functions
      Pointer(IsThemeActive) := GetProcAddress(FThemeLibrary, 'IsThemeActive');
      Pointer(IsAppThemed) := GetProcAddress(FThemeLibrary, 'IsAppThemed');
    end else begin
      IsThemeActive := nil;
      IsAppThemed := nil;
    end;
  end;
  Pointer(InitCommonControlsEx) := GetProcAddress(GetModuleHandle('comctl32.dll'), 'InitCommonControlsEx');

  // init
  UpdateThemesActive;
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
Destructor TWin32WidgetSet.Destroy;
var
  n: integer;
  TimerInfo : PWin32TimerInfo;
Begin
  Assert(False, 'Trace:TWin32WidgetSet is being destroyed');

  n := FTimerData.Count;
  if (n > 0) then
  begin
    DebugLn(Format('[TWin32WidgetSet.Destroy] WARNING: There are %d TimerInfo structures left, I''ll free them' ,[n]));
    while (n > 0) do
    begin
      dec(n);
      TimerInfo := PWin32Timerinfo(FTimerData[n]);
      Dispose(TimerInfo);
      FTimerData.Delete(n);
    end;
  end;

  if FStockNullBrush <> 0 then
  begin
    DeleteObject(FStockNullBrush);
    DeleteObject(FStockBlackBrush);
    DeleteObject(FStockLtGrayBrush);
    DeleteObject(FStockGrayBrush);
    DeleteObject(FStockDkGrayBrush);
    DeleteObject(FStockWhiteBrush);
  end;

  if FStatusFont <> 0 then
  begin
    Windows.DeleteObject(FStatusFont);
    Windows.DeleteObject(FMessageFont);
  end;

  FTimerData.Free;

  if FAppHandle <> 0 then
    DestroyWindow(FAppHandle);

  Windows.UnregisterClass(@ClsName, System.HInstance);

  if FThemeLibrary <> 0 then
    FreeLibrary(FThemeLibrary);

  inherited Destroy;
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppInit
  Params:  None
  Returns: Nothing

  Initialize Windows
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.AppInit(var ScreenInfo: TScreenInfo);
var
  ICC: TINITCOMMONCONTROLSEX;
  LogBrush: TLOGBRUSH;
  SysMenu: HMENU;
  Handle: HWND;
  DC: HDC;
begin
  Assert(False, 'Trace:Win32Object.Init - Start');
  if not WinRegister then
  begin
    Assert(False, 'Trace:Win32Object.Init - Register Failed');
    DebugLn('Trace:Win32Object.Init - Register Failed');
    Exit;
  end;

  //Init stock objects;
  LogBrush.lbStyle := BS_NULL;
  FStockNullBrush := CreateBrushIndirect(LogBrush);
  LogBrush.lbStyle := BS_SOLID;
  LogBrush.lbColor := $000000;
  FStockBlackBrush := CreateBrushIndirect(LogBrush);
  LogBrush.lbColor := $C0C0C0;
  FStockLtGrayBrush := CreateBrushIndirect(LogBrush);
  LogBrush.lbColor := $808080;
  FStockGrayBrush := CreateBrushIndirect(LogBrush);
  LogBrush.lbColor := $404040;
  FStockDkGrayBrush := CreateBrushIndirect(LogBrush);
  LogBrush.lbColor := $FFFFFF;
  FStockWhiteBrush := CreateBrushIndirect(LogBrush);

  if FMetricsFailed then
  begin
    FStatusFont := Windows.GetStockObject(DEFAULT_GUI_FONT);
    FMessageFont := Windows.GetStockObject(DEFAULT_GUI_FONT);
  end else begin
    FStatusFont := Windows.CreateFontIndirect(@FMetrics.lfStatusFont);
    FMessageFont := Windows.CreateFontIndirect(@FMetrics.lfMessageFont);
  end;

  //TODO: Remove when the WS interface is implemented
  //      Common controls only need to be initialized when used
  //      So they are initialized in the CreateHandle for common controls
  InitCommonControls;
  ICC.dwSize := SizeOf(TINITCOMMONCONTROLSEX);
  ICC.dwICC := ICC_DATE_CLASSES;
  if InitCommonControlsEx <> nil then
    InitCommonControlsEx(@ICC);

  // Create parent of all windows, `button on taskbar'
  FAppHandle := CreateWindow(@ClsName, PChar(Application.Title), WS_POPUP or
    WS_CLIPSIBLINGS or WS_CAPTION or WS_SYSMENU or WS_MINIMIZEBOX,
    0, {Windows.GetSystemMetrics(SM_CXSCREEN) div 2,}
    0, {Windows.GetSystemMetrics(SM_CYSCREEN) div 2,}
    0, 0, HWND(nil), HMENU(nil), HInstance, nil);
  AllocWindowInfo(FAppHandle);

  // set nice main icon
  SendMessage(FAppHandle, WM_SETICON, ICON_BIG, 
    Windows.LoadIcon(MainInstance, 'MAINICON'));
  // remove useless menuitems from sysmenu
  SysMenu := Windows.GetSystemMenu(FAppHandle, False);
  Windows.DeleteMenu(SysMenu, SC_MAXIMIZE, MF_BYCOMMAND);
  Windows.DeleteMenu(SysMenu, SC_SIZE, MF_BYCOMMAND);
  Windows.DeleteMenu(SysMenu, SC_MOVE, MF_BYCOMMAND);
  
  // initialize ScreenInfo
  Handle := GetDesktopWindow;
  DC := Windows.GetDC(Handle);
  ScreenInfo.PixelsPerInchX := GetDeviceCaps(DC, LOGPIXELSX);
  ScreenInfo.PixelsPerInchY := GetDeviceCaps(DC, LOGPIXELSY);
  ScreenInfo.ColorDepth := GetDeviceCaps(DC, BITSPIXEL);
  ReleaseDC(Handle, DC);
  
  // Thread.Synchronize support
  {$IFNDEF VER1_0}{$IFNDEF VER1_9_8}
  WakeMainThread := @HandleWakeMainThread;
  {$ENDIF}{$ENDIF}

  Assert(False, 'Trace:Win32Object.Init - Exit');
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppMinimize
  Params:  None
  Returns: Nothing

  Minimizes the whole application to the taskbar
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.AppMinimize;
begin
  Windows.SendMessage(FAppHandle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppBringToFront
  Params:  None
  Returns: Nothing

  Brings the entire application on top of all other non-topmost programs
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.AppBringToFront;
begin
  SetWindowPos(FAppHandle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_SHOWWINDOW);
end;

procedure TWin32WidgetSet.SetDesigning(AComponent: TComponent);
begin
  //if Data<>nil then EnableWindow((AComponent As TWinControl).Handle, boolean(Data^));
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.SetCallback
  Params: Msg    - message for which to set a callback
          Sender - object to which callback will be sent
  Returns:  nothing

  Applies a Message to the sender
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.SetCallback(Msg: LongInt; Sender: TObject);
Var
  Window: HWnd;
Begin
  Assert(False, 'Trace:TWin32WidgetSet.SetCallback - Start');
  Assert(False, Format('Trace:TWin32WidgetSet.SetCallback - Class Name --> %S', [Sender.ClassName]));
  Assert(False, Format('Trace:TWin32WidgetSet.SetCallback - Message Name --> %S', [GetMessageName(Msg)]));
  If Sender Is TControlCanvas Then
    Window := TControlCanvas(Sender).Handle
  Else If Sender Is TCustomForm Then
    Window := TCustomForm(Sender).Handle
  Else
    Window := TWinControl(Sender).Handle;
  if Window=0 then exit;

  Assert(False, 'Trace:TWin32WidgetSet.SetCallback - Exit');
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.RemoveCallbacks
  Params:   Sender - object from which to remove callbacks
  Returns:  nothing

  Removes Call Back Signals from the sender
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.RemoveCallbacks(Sender: TObject);
Var
  Window: HWnd;
Begin
  If Sender Is TControlCanvas Then
    Window := TControlCanvas(Sender).Handle
  Else If Sender Is TCustomForm Then
    Window := TCustomForm(Sender).Handle
  Else
    Window := (Sender as TWinControl).Handle;
  if Window=0 then exit;
End;

function TWin32WidgetSet.InitHintFont(HintFont: TObject): Boolean;
begin
  TFont(HintFont).Name := FMetrics.lfStatusFont.lfFaceName;
  TFont(HintFont).Style := [];
  TFont(HintFont).Height := FMetrics.lfStatusFont.lfHeight;
  TFont(HintFont).Color := clInfoText;
  TFont(HintFont).Pitch := fpDefault;
  Result := true;
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.HandleEvents
  Params:  None
  Returns: Nothing

  Handle all pending messages
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.HandleEvents;
var
  AMessage: TMsg;
  AccelTable: HACCEL;
Begin
  While PeekMessage(AMessage, HWnd(Nil), 0, 0,PM_REMOVE) Do
  Begin
    AccelTable := GetWindowInfo(AMessage.HWnd)^.Accel;
    If (AccelTable = HACCEL(nil)) or (TranslateAccelerator(AMessage.HWnd, AccelTable, @AMessage) = 0) Then
    Begin
      TranslateMessage(@AMessage);
      DispatchMessage(@AMessage);
    End;
{$ifndef VER1_0}
    // check for pending to-be synchronized methods
    CheckSynchronize;
{$endif}
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.WaitMessage
  Params:  None
  Returns: Nothing

  Passes execution control to Windows
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.WaitMessage;
Begin
  Assert(False, 'Trace:TWin32WidgetSet.WaitMessage - Start');
  Windows.WaitMessage;
  Assert(False,'Trace:Leave wait message');
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AppTerminate
  Params:  None
  Returns: Nothing

  Tells Windows to halt and destroy
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.AppTerminate;
Begin
  Assert(False, 'Trace:TWin32WidgetSet.AppTerminate - Start');
End;

{------------------------------------------------------------------------------
  Function: CreateTimer
  Params: Interval:
          TimerFunc: Callback
  Returns: a Timer id (use this ID to destroy timer)

  Design: A timer which calls TimerCallBackProc, is created.
    The TimerCallBackProc calls the TimerFunc.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : integer;
var
  TimerInfo: PWin32TimerInfo;
begin
  Assert(False,'Trace:Create Timer: ' + IntToStr(Interval));
  Result := 0;
  if (Interval > 0) and (TimerFunc <> nil) then begin
    New(TimerInfo);
    TimerInfo^.TimerFunc := TimerFunc;
    TimerInfo^.TimerID := Windows.SetTimer(0, 0, Interval, @TimerCallBackProc);
    if TimerInfo^.TimerID=0 then
      dispose(TimerInfo)
    else begin
      FTimerData.Add(TimerInfo);
      Result := TimerInfo^.TimerID;
    end;
  end;
  Assert(False,'Trace:Result: ' + IntToStr(result));
end;

{------------------------------------------------------------------------------
  Function: DestroyTimer
  Params: TimerHandle
  Returns:
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.DestroyTimer(TimerHandle: Integer) : boolean;
var
  n : integer;
  TimerInfo : PWin32Timerinfo;
begin
  Result:= false;
  Assert(False,'Trace:removing timer: '+ IntToStr(TimerHandle));
  n := FTimerData.Count;
  while (n>0) do begin
    dec(n);
    TimerInfo := FTimerData[n];
    if (TimerInfo^.TimerID=UINT(TimerHandle)) then
    begin
      Result := Boolean(Windows.KillTimer(0, UINT(TimerHandle)));
      FTimerData.Delete(n);
      Dispose(TimerInfo);
    end;
  end;
  Assert(False,'Trace:Destroy timer Result: '+ BOOL_RESULT[result]);
end;

procedure TWin32WidgetSet.HandleWakeMainThread(Sender: TObject);
begin
  // wake up GUI thread by sending a message to it
  Windows.PostMessage(AppHandle, WM_NULL, 0, 0);
end;

procedure TWin32WidgetSet.AttachMenuToWindow(AMenuObject: TComponent);
var
  AMenu: TMenu;
  AWinControl: TWinControl;
begin
  AMenu := AMenuObject as TMenu;
  if AMenu is TMainMenu then
  begin
    AWinControl := TWinControl(AMenu.Owner);
    Windows.SetMenu(AWinControl.Handle, AMenu.Handle);
  end;
end;

{ Private methods (in no significant order) }

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.WinRegister
  Params: None
  Returns: If the window was successfully regitered

  Registers the main window class
 ------------------------------------------------------------------------------}
Function TWin32WidgetSet.WinRegister: Boolean;
Var
  WindowClass: WndClass;
Begin
  Assert(False, 'Trace:WinRegister - Start');
  With WindowClass Do
  Begin
    Style := CS_DBLCLKS{CS_HRedraw or CS_VRedraw};
    LPFnWndProc := @WindowProc;
    CbClsExtra := 0;
    CbWndExtra := 0;
    hInstance := System.HInstance;
    hIcon := Windows.LoadIcon(MainInstance, 'MAINICON');
    if hIcon = 0 then
      hIcon := Windows.LoadIcon(0, IDI_APPLICATION);
    hCursor := LoadCursor(0, IDC_ARROW);
    hbrBackground := 0; {GetSysColorBrush(Color_BtnFace);}
    LPSzMenuName := Nil;
    LPSzClassName := @ClsName;
  End;
  Result := Windows.RegisterClass(@WindowClass) <> 0;
  Assert(False, 'Trace:WinRegister - Exit');
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.UpdateThemesActive
  Params:  None
  Returns: Nothing

  Updates the field FThemesActive to save whether xp themes are active
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.UpdateThemesActive;
begin
  if (IsThemeActive <> nil) and (IsAppThemed <> nil) then
    FThemesActive := IsThemeActive() and IsAppThemed()
  else
    FThemesActive := false;
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.PaintPixmap
  Params: Surface    - The surface onto which to paint the pixmap
          PixmapData - Data necessary in drawing the pixmap
  Returns: Nothing

  Paints a pixmap on a surface (control).
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.PaintPixmap(Surface: TObject; PixmapData: Pointer);
Var
  DC: HDC;
  Pixmap: HIcon;
Begin
  DC := GetDC((Surface As TWinControl).Handle);
  Pixmap := CreatePixmapIndirect(PixmapData, 0);
  DrawIcon(DC, TWinControl(Surface).Left, TWinControl(Surface).Top, Pixmap);
  ReleaseDC(TWinControl(Surface).Handle, DC);
  DeleteObject(Pixmap);
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.NormalizeIconName
  Params: IconName - The name of the icon to normalize
  Returns: Nothing

  Adjusts an icon name to the proper format
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.NormalizeIconName(Var IconName: String);
Var
  IcoLen: Integer;
Begin
  DoDirSeparators(IconName);
  IcoLen := Pos('.xmp', LowerCase(IconName));
  If IcoLen <> 0 Then
  Begin
    Delete(IconName, IcoLen, Length('.xpm'));
    Insert('.ico', IconName, Length(IconName));
  End
End;

Procedure TWin32WidgetSet.NormalizeIconName(Var IconName: PChar);
Var
  Str: String;
Begin
  Str := String(IconName);
  NormalizeIconName(Str);
  IconName := StrToPChar(Str);
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.ResizeChild
  Params:  Sender                   - the object which invoked this function
           Left, Top, Width ,Height - new dimensions for the control
  Returns: Nothing

  Resize a window
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.ResizeChild(Sender: TWinControl;
  Left, Top, Width, Height: Integer);

Var
{$IFDEF VerboseSizeMsg}
  OldLeft: Integer;
  OldTop: Integer;
{$ENDIF}
  lRect: TRect;
  WinHandle, BuddyHandle: HWND;
  StringList: TWin32ComboBoxStringList;
  suppressMove: boolean;
Begin
  // if not Sender.HandleAllocated then exit; --> Already checked (LM_SETSIZE and LM_RECREATEWND)
  {$IFDEF VerboseSizeMsg}
  OldLeft:=Left;
  OldTop:=Top;
  {$ENDIF}
  LCLBoundsToWin32Bounds(Sender, Left, Top, Width, Height);
  {$IFDEF VerboseSizeMsg}
  writeln('TWin32WidgetSet.ResizeChild A ',Sender.Name,':',Sender.ClassName,
    ' LCL=',OldLeft,',',OldTop,',',Width,',',Height,
    ' Win32=',Left,',',Top,',',Width,',',Height,
    '');
  {$ENDIF}
  WinHandle := Sender.Handle;
  suppressMove := false;
  case Sender.FCompStyle of
  csSpinEdit:
  begin
    // detach from buddy first
    BuddyHandle := Windows.SendMessage(WinHandle, UDM_SETBUDDY, 0, 0);
    MoveWindow(BuddyHandle, Left, Top, Width, Height, True);
    // reattach
    Windows.SendMessage(WinHandle, UDM_SETBUDDY, BuddyHandle, 0);
    suppressMove := true;
  end;
  
  csGroupBox:
  begin
    // check if we have a ``container'', if so, move that
    BuddyHandle := GetWindowInfo(WinHandle)^.ParentPanel;
    if BuddyHandle <> 0 then
    begin
      MoveWindow(BuddyHandle, Left, Top, Width, Height, false);
      Left := 0;
      Top := 0;
    end;
  end;
  
  else
    if Sender is TCustomComboBox then
    begin
      StringList := TWin32ComboBoxStringList(GetWindowInfo(WinHandle)^.List);
      if StringList <> nil then
        Height := StringList.ComboHeight;
    end else
    if Sender is TCustomCalendar then
    begin
      Windows.SendMessage(WinHandle, MCM_GETMINREQRECT, 0, LPARAM(@lRect));
      Width := lRect.Right;
      Height := lRect.Bottom;
    end;
  end;
  
  if not suppressMove then
    MoveWindow(WinHandle, Left, Top, Width, Height, True);
  LCLControlSizeNeedsUpdate(Sender,False);
End;

 {------------------------------------------------------------------------------
  Function: TWin32WidgetSet.CreateComponent
  Params:   Sender - object for which to create visual representation
  Returns:  nothing

  Tells Windows to create a control
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreateComponent(Sender: TObject): THandle;
begin
  DebugLn('WARNING: TWin32WidgetSet.CreateComponent is deprecated, should not be reachable!!');
  Result := 0;
end;
 
 {------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AssignSelf
  Params: Window - The window to assign
          Data   - The data to assign to the window
  Returns: Nothing

  Assigns data to a window
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.AssignSelf(Window: HWnd; Data: Pointer);
begin
  Assert(False, 'Trace:[TWin32WidgetSet.AssignSelf] Trying to code it.  It''s probably wrong.');
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.ShowHide
  Params: Sender - The sending object
  Returns: Nothing

  Shows or hides a control
 ------------------------------------------------------------------------------}
Procedure TWin32WidgetSet.ShowHide(Sender: TObject);
Var
  Handle: HWND;
  ParentPanel: HWND;
  Flags: dword;
Begin
  If (TControl(Sender).FCompStyle = csPage) or (TControl(Sender).FCompStyle = csToolButton) then exit;
  Handle := ObjectToHWND(Sender);
  ParentPanel := GetWindowInfo(Handle)^.ParentPanel;
  if ParentPanel <> 0 then
    Handle := ParentPanel;
  If TControl(Sender).HandleObjectShouldBeVisible Then
  Begin
    Assert(False, 'Trace: [TWin32WidgetSet.ShowHide] Showing the window');
    if TControl(Sender).FCompStyle = csHintWindow then
    begin
      Windows.SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_SHOWWINDOW or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER);
    end else begin
      Flags := SW_SHOW;
      if TControl(Sender) is TCustomForm then
        case TCustomForm(Sender).WindowState of
          wsMaximized: Flags := SW_SHOWMAXIMIZED;
          wsMinimized: Flags := SW_SHOWMINIMIZED;
        end;
      Windows.ShowWindow(Handle, Flags);
      { ShowWindow does not send WM_SHOWWINDOW when creating overlapped maximized window }
      { TODO: multiple WM_SHOWWINDOW when maximizing after initial show? }
      if Flags = SW_SHOWMAXIMIZED then
        Windows.SendMessage(Handle, WM_SHOWWINDOW, 1, 0);
    end;
    If (Sender Is TCustomForm) Then
      SetClassLong(Handle, GCL_HICON, LONG(TCustomForm(Sender).GetIconHandle));
  End
  Else
  Begin
    Assert(False, 'TRACE: [TWin32WidgetSet.ShowHide] Hiding the window');
    ShowWindow(Handle, SW_HIDE);
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.DCReDraw
  Params: CanvasHandle - HDC to redraw
  Returns: Nothing

  Redraws (the window of) a canvas
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.DCRedraw(CanvasHandle: HDC);
begin
  // TODO: implement me!
  Assert(False, 'TRACE:[TWin32WidgetSet.ReDraw] Redrawing...');
  Assert(False, 'TRACE:Invalidating the window');
  Assert(False, 'TRACE:Updating the window');
  Assert(False, 'TRACE:[TWin32WidgetSet.ReDraw] Finished redrawing');
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.SetPixel
  Params:  Canvas - canvas to set color on
           X, Y   - position
           AColor - new color for specified position
  Returns: nothing

  Set the color of the specified pixel on the canvas
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
begin
  Windows.SetPixel(CanvasHandle, X, Y, AColor);
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.GetPixel
  Params:  Canvas - canvas to get color from
           X, Y   - position
  Returns: Color at specified point

  Get the color of the specified pixel on the canvas
 -----------------------------------------------------------------------------}
function  TWin32WidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
begin
  Result := Windows.GetPixel(CanvasHandle, X, Y);
end;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

{
  $Log$
  Revision 1.311  2005/07/18 12:37:58  micha
  remove TWin32WidgetSet.RecreateWnd, use the one from Controls unit

  Revision 1.310  2005/05/03 14:56:33  vincents
  fixed getting the size of a minimized window (bug 886)

  Revision 1.309  2005/04/22 10:48:42  micha
  showwindow does not send WM_SHOWWINDOW message after maximized window creation (patch from jesus)

  Revision 1.308  2005/03/03 13:13:51  vincents
  fixed thread synchronize support for fpc 1.9.9 using WakeMainThread

  Revision 1.307  2005/02/07 16:19:14  micha
  force default width and height for calendar control

  Revision 1.306  2005/01/31 17:09:17  micha
  use 0 icon handle for bsdialog forms

  Revision 1.305  2005/01/15 10:09:23  micha
  fix bug 558: title bar redraw (win32)
  USE_SYNCHRONIZE enabled per default for 1.9.x

  Revision 1.304  2005/01/03 16:29:48  micha
  take windowstate into account when showing window

  Revision 1.303  2005/01/01 18:56:47  mattias
  implemented TTIProgressBar

  Revision 1.302  2004/12/27 10:18:21  micha
  implement support for tthread.synchronize

  Revision 1.301  2004/12/16 14:29:12  micha
  fix showmodal to remember disabled windows (fixes bug 478, and more)

  Revision 1.300  2004/12/15 17:51:16  micha
  fix clientrect size on show form with menu (lazarus ide, component palette height too great)

  Revision 1.299  2004/11/04 16:57:32  micha
  remove obsolete and unused method twin32widgetset.settext

  Revision 1.298  2004/11/04 15:12:35  micha
  remove usage of fcompstyle in twin32liststringlist by using descendent class for combobox specific things

  Revision 1.297  2004/11/02 17:48:19  micha
  remove usage of fcompstyle

  Revision 1.296  2004/11/01 18:20:21  micha
  set default icon instead of lcl icon which actually is a bitmap

  Revision 1.295  2004/10/29 09:52:08  micha
  fix crash on showing tabpage
  fix painting of radiobutton in groupbox (non-tabpage-parent)

  Revision 1.294  2004/10/28 21:00:56  micha
  convert GetProp and SetProp usage to one Atom pointing to a record of fields

  Revision 1.293  2004/10/28 07:43:29  micha
  experiment: use CS_SAVEBITS class style on tabpages to reduce flickering

  Revision 1.292  2004/10/16 10:17:21  micha
  remove statusbar helper methods from general widgetset object

  Revision 1.291  2004/10/15 09:51:09  micha
  splitup of CreateComponent to widgetset CreateHandle methods

  Revision 1.290  2004/10/13 20:03:22  vincents
  From Jesus: Set default window class style to CS_DBLCLKS.

  Revision 1.289  2004/10/06 12:50:38  micha
  remove obsolete code

  Revision 1.288  2004/10/06 10:52:46  micha
  split up common dialogs code

  Revision 1.287  2004/09/24 21:34:14  micha
  convert LM_CREATE message to interface methods
  remove SendMsgToInterface, CNSendMessage and related methods
  remove TWidgetSet.IntSendMessage3; all LCL to interface messages have been converted

  Revision 1.286  2004/09/24 14:50:58  micha
  convert LM_SETDESIGNING message to TWidgetSet method

  Revision 1.285  2004/09/24 07:52:35  micha
  convert LM_SETPROPERTIES message to interface method for TCustomTrackBar
  remove message LM_SETPROPERTIES, conversion done

  Revision 1.284  2004/09/23 21:16:46  micha
  convert LM_SETPROPERTIES message to interface method for TCustomSpinEdit

  Revision 1.283  2004/09/23 20:36:30  micha
  convert LM_SETPROPERTIES message to interface method for TScrollBar

  Revision 1.282  2004/09/23 14:50:47  micha
  convert LM_SETPROPERTIES message to interface method for TCustomProgressBar

  Revision 1.281  2004/09/22 18:06:32  micha
  convert LM_SETPROPERTIES message to interface methods for TCustomListView

  Revision 1.280  2004/09/22 14:50:18  micha
  convert LM_SETPROPERTIES message for tcustomlabel to interface methods

  Revision 1.279  2004/09/21 13:28:10  micha
  convert LM_SETPROPERTIES to interface methods for TCustomEdit

  Revision 1.278  2004/09/19 19:39:10  micha
  undo removal of LM_SETDESIGNING; used by lazarus ide (main.pp)

  Revision 1.276  2004/09/19 18:50:28  micha
  convert LM_SETVALUE message to interface methods

  Revision 1.275  2004/09/18 17:07:58  micha
  convert LM_GETVALUE message to interface method

  Revision 1.274  2004/09/18 12:43:15  micha
  convert LM_DESTROY message to interface methods

  Revision 1.273  2004/09/18 11:06:47  micha
  remove LM_RECREATEWND message, as it is not used by LCL

  Revision 1.272  2004/09/18 10:52:48  micha
  convert LM_SCREENINIT message to interface method (integrated with TWidgetSet.AppInit(var ScreenInfo)

  Revision 1.271  2004/09/17 10:56:25  micha
  convert LM_SHORTCUT message to interface methods

  Revision 1.270  2004/09/17 07:55:13  micha
  convert LM_SETBORDER message to interface method
  fix widgetsets virtual methods to be published
  fix compilation debugging widgetset registration

  Revision 1.269  2004/09/16 14:32:31  micha
  convert LM_SETSELMODE message to interface method

  Revision 1.268  2004/09/16 13:57:30  micha
  convert LM_SETSEL message to interface method

  Revision 1.267  2004/09/16 13:30:48  micha
  convert LM_SORT message to interface method

  Revision 1.266  2004/09/15 19:38:56  micha
  convert LM_GETSEL message to interface method

  Revision 1.265  2004/09/15 19:04:39  micha
  convert LM_GETSELCOUNT message to interface method

  Revision 1.264  2004/09/15 18:50:34  micha
  remove LM_GETLINECOUNT message as it is not used by the LCL

  Revision 1.263  2004/09/15 17:21:22  micha
  convert LM_GETITEMINDEX and LM_SETITEMINDEX messages to interface methods

  Revision 1.262  2004/09/15 14:45:39  micha
  convert LM_GETITEMS message to interface method

  Revision 1.261  2004/09/15 07:58:00  micha
  convert LM_SETFORMICON message to interface method

  Revision 1.260  2004/09/14 21:30:37  vincents
  replaced writeln by DebugLn

  Revision 1.259  2004/09/14 15:48:28  micha
  convert LM_INVALIDATE message to interface method

  Revision 1.258  2004/09/14 14:41:17  micha
  convert LM_INSERTTOOLBUTTON and LM_DELETETOOLBUTTON messages to interface methods; warning: still very ugly code, as if it is "OldToolbar" so probably, obsolete

  Revision 1.257  2004/09/14 12:45:29  micha
  convert LM_SETTABPOSITION message to interface method

  Revision 1.256  2004/09/14 10:06:26  micha
  convert LM_REDRAW message to interface method (in twidgetset)

  Revision 1.255  2004/09/13 19:57:30  micha
  convert LM_SHOWTABS message to interface method

  Revision 1.254  2004/09/13 19:06:04  micha
  convert LM_ADDPAGE and LM_REMOVEPAGE messages to new interface methods

  Revision 1.253  2004/09/13 14:34:53  micha
  convert LM_TB_BUTTONCOUNT to interface method

  Revision 1.252  2004/09/13 13:13:46  micha
  convert LM_SHOWMODAL to interface methods

  Revision 1.251  2004/09/12 19:50:36  micha
  convert LM_SETSIZE message to new interface method

  Revision 1.250  2004/09/12 13:52:26  micha
  convert LM_SETFONT to interface method

  Revision 1.249  2004/09/12 13:30:13  micha
  remove handling of LM_SETFOCUS in interface, as it is never sent from LCL

  Revision 1.248  2004/09/12 13:21:37  micha
  remove obsolete message LM_DRAGINFOCHANGED

  Revision 1.247  2004/09/12 13:11:50  micha
  convert LM_GETPIXEL and LM_SETPIXEL to interface methods (of twidgetset, DCGetPixel and DCSetPixel)

  Revision 1.246  2004/09/11 17:29:10  micha
  convert LM_POPUPSHOW message to interface method

  Revision 1.245  2004/09/11 15:01:22  micha
  remove obsolete LM_SETFILTER and LM_SETFILENAME messages

  Revision 1.244  2004/09/11 14:54:01  micha
  convert LM_BTNDEFAULT_CHANGED message to interface method

  Revision 1.243  2004/09/11 13:38:37  micha
  convert LM_BRINGTOFRONT message to interface method
  NOTE: was only used for tapplication, not from other controls

  Revision 1.242  2004/09/11 13:06:49  micha
  convert LM_ADDCHILD message to interface method

  Revision 1.241  2004/09/10 20:19:13  micha
  convert LM_CLB_G/SETCHECKED to interface methods

  Revision 1.240  2004/09/10 18:58:22  micha
  convert LM_ATTACHMENU to interface method

  Revision 1.239  2004/09/10 17:59:58  micha
  convert LM_APPENDTEXT to interface method

  Revision 1.238  2004/09/10 14:38:29  micha
  convert lm_gettext to new interface methods
  remove lm_settext replacement settext methods in twidgetsets

  Revision 1.237  2004/09/10 11:20:44  micha
  remove LM_SETTEXT message as it is not used

  Revision 1.236  2004/09/10 09:43:13  micha
  convert LM_SETLABEL message to interface methods

  Revision 1.235  2004/09/08 20:47:17  micha
  convert LM_SHOWHIDE message to new intf method TWSWinControl.ShowHide

  Revision 1.234  2004/09/08 19:09:34  micha
  convert LM_SETCOLOR message to new intf method TWSWinControl.SetColor

  Revision 1.233  2004/09/07 10:26:17  micha
  fix logs to get rid of comment level 2 warning

  Revision 1.232  2004/09/07 10:18:11  micha
  fix win32 interface, remove lm_setlimittext (obsolete)

  Revision 1.231  2004/09/07 09:44:46  micha
  convert lcl messages to new interface using methods: LM_G/SETSELSTART, LM_G/SETSELLEN, LM_G/SETLIMITTEXT

  Revision 1.230  2004/09/06 15:08:32  micha
  implement LM_GETSELLEN en LM_GETSELSTART for win32 edit controls

  Revision 1.229  2004/08/27 08:55:23  micha
  implement tapplication.minimize for win32, stub for gtk

  Revision 1.228  2004/08/25 17:08:10  micha
  use new lcl interface methods instead of messages (for win32; twscustomlistview)

  Revision 1.227  2004/08/25 15:04:44  micha
  use new lcl interface methods instead of messages (for win32; twsbitbtn)

  Revision 1.226  2004/08/23 21:29:40  vincents
  Don't show taskbar button, until MainForm is shown.

  Revision 1.225  2004/08/09 21:12:43  mattias
  implemented FormStyle fsSplash for splash screens

  Revision 1.224  2004/07/27 00:07:48  marc
  * Fixed disabled state of bitbtns

  Revision 1.223  2004/07/16 21:49:00  mattias
  added RTTI controls

  Revision 1.222  2004/07/15 10:43:39  mattias
  added TCustomButton, TCustomBitBtn, TCustomSpeedButton

  Revision 1.221  2004/07/08 20:42:24  micha
  cleanup: do not free nil resource

  Revision 1.220  2004/07/08 20:29:05  micha
  fix win95 compatibility, do not send seperate lm_setshortcut message

  Revision 1.219  2004/07/04 20:05:44  micha
  set properties before setting subclassed window proc

  Revision 1.218  2004/06/30 20:59:11  micha
  initialize common controls: date picker

  Revision 1.217  2004/06/29 21:38:43  vincents
  fix fpc 1.0.x compilation

  Revision 1.216  2004/06/29 14:38:28  micha
  fix default button notification win32 intf

  Revision 1.215  2004/06/29 08:03:08  micha
  fix showtabs for win32 interface

  Revision 1.214  2004/06/20 21:21:49  micha
  fix GetVisible to return this control's visibility, instead introduce IsVisible to check for recursive visibility

  Revision 1.213  2004/06/20 20:36:55  micha
  remove old obsolete/commented toolbutton code
  rename lazarusform classname to window, because we use it for panels, notebookpages, etc too

  Revision 1.212  2004/06/20 13:58:15  micha
  fix combobox edit being gray

  Revision 1.211  2004/06/20 13:00:04  micha
  fix groupbox-parent bug condition, use buddy

  Revision 1.210  2004/06/19 15:10:04  micha
  fix spinedit not firing onchange event

  Revision 1.209  2004/06/18 20:47:34  vincents
  fixed pasting from clipboard

  Revision 1.208  2004/06/18 20:15:06  micha
  remove obsolete LM_LOADXPM message

  Revision 1.207  2004/06/18 19:55:43  micha
  fix xp themes drawing image on bitbtn

  Revision 1.206  2004/06/17 19:54:19  micha
  fix bug in drawing groupbox caption when it is a child of another groupbox

  Revision 1.205  2004/06/15 15:37:29  micha
  fix groupbox background erasing

  Revision 1.204  2004/06/15 14:41:38  micha
  fix drawing bug in xp with themes

  Revision 1.203  2004/06/10 18:14:09  vincents
  converted win32proc.inc to unit

  Revision 1.202  2004/05/31 19:32:34  vincents
  fixed using ecUpperCase in win32

  Revision 1.201  2004/05/31 08:21:52  mattias
  fsStayOnTop is now ignored in design mode

  Revision 1.200  2004/05/30 20:17:55  vincents
  changed radiobutton style to BS_RADIOBUTTON to prevent test program from hanging.

  Revision 1.199  2004/05/27 15:04:53  vincents
  checking a radiobutton unchecks its siblibg radiobuttons

  Revision 1.198  2004/05/21 11:18:30  micha
  use unsigned integer for control styles; fixes compiler warning

  Revision 1.197  2004/05/21 09:03:55  micha
  implement new borderstyle
  - centralize to twincontrol (protected)
  - public expose at tcustomcontrol to let interface access it

  Revision 1.196  2004/05/20 21:28:54  marc
  * Fixed win32 listview

  Revision 1.195  2004/05/16 20:59:53  vincents
  page caption not retrieved from interface, fixed setting caption for page not yet added

  Revision 1.194  2004/05/14 17:48:39  micha
  fix itemheight of listbox, handle measureitem message

  Revision 1.193  2004/05/14 15:20:47  micha
  fix sizing when menu is attached to window

  Revision 1.192  2004/05/12 15:11:46  micha
  fix sizing/non-sizing border sizes

  Revision 1.191  2004/05/12 09:46:25  micha
  fix toolbar buttons by handling them as customcontrols
  remove handledialogmessage, now handled in lcl

  Revision 1.190  2004/05/09 19:14:45  micha
  fix update menu caption at runtime (code accidently commented by me, i think)

  Revision 1.189  2004/05/09 16:24:51  micha
  fix tab order by fixing z-order at control creation

  Revision 1.188  2004/05/08 12:32:32  micha
  fix combobox height; whatever the lcl passes as height for combobox, always calculate our own

  Revision 1.187  2004/04/18 20:11:08  micha
  fix label alignment left, center, right

  Revision 1.186  2004/04/11 10:19:28  micha
  cursor management updated:
  - lcl notifies interface via WSControl.SetCursor of changes
  - fix win32 interface to respond to wm_setcursor callback and set correct cursor

  Revision 1.185  2004/04/11 07:00:30  micha
  speedup: don't redraw menubar if form is being destroyed

  Revision 1.184  2004/04/10 17:54:52  micha
  - added: [win32] mousewheel default handler sends scrollbar messages
  - fixed: lmsetcursor; partial todo

  Revision 1.183  2004/03/18 22:26:24  mattias
  fixed grids TComboBox from Jesus

  Revision 1.182  2004/03/07 12:55:53  micha
  don't create window for image

  Revision 1.181  2004/03/05 15:41:42  micha
  fix appendtext to actually *append* text, not replace

  Revision 1.180  2004/03/05 13:57:25  micha
  fix bug 208: cursor changes when creating submenu

  Revision 1.179  2004/03/05 01:04:21  marc
  * Renamed TWin32Object to TWin32WidgetSet

  Revision 1.178  2004/03/04 21:16:15  micha
  remove workaround; fpc bug fixed

  Revision 1.177  2004/02/28 14:38:47  micha
  fix transparent images for menuitems

  Revision 1.176  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.175  2004/02/26 21:01:42  micha
  fixed: focussing issue combobox

  Revision 1.174  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.172  2004/02/21 13:35:15  micha
  fixed: name clash SetCursor (message LM_SETCURSOR), and inherited SetCursor (winapi)

  Revision 1.171  2004/02/21 10:11:36  micha
    1. pressing the Return key in ObjectInspector when editing a value throws an exception
    2. placing TPairSplitter component on the form produces "Division by zero"

  Revision 1.170  2004/02/20 19:52:18  micha
  fixed: tarrow crash in win32
  added: interface function DrawArrow to draw themed arrow

  Revision 1.169  2004/02/19 14:37:20  micha
  fixed: memo eats return key (from vincent)

  Revision 1.168  2004/02/16 22:01:31  marc
  * Applied patch from Martin Smat
    this patch fixes showing menuitem initially defined as Checked=true
    or Enabled=false

  Revision 1.167  2004/02/15 19:26:48  micha
  fixed: remove GetAncestor dependency; code obsolete? works without too...

  Revision 1.166  2004/02/09 19:52:52  mattias
  implemented ByteOrder for TLazIntfImage and added call of to LM_SETFONT

  Revision 1.165  2004/02/06 23:58:44  marc
  + patch from Jesus Reyes, it enables TCustomEdit SelStart/Length

  Revision 1.164  2004/02/02 16:56:44  micha
  implement GetControlConstraints for combobox

  Revision 1.163  2004/02/01 09:58:21  mattias
  fixed showing statusbar at designtime  from vincent

  Revision 1.162  2004/01/22 18:22:37  mattias
  applied patch for dir dlgs from Vincent

  Revision 1.161  2004/01/21 10:19:16  micha
  enable tabstops for controls; implement tabstops in win32 intf

  Revision 1.160  2004/01/20 22:14:27  micha
  REVERTED: "try register globally unique properties"; implemented new WindowFromPoint not returning window if from different process (tip from vincent)

  Revision 1.158  2004/01/20 10:26:41  micha
  try register globally unique properties

  Revision 1.158  2004/01/12 08:36:34  micha
  statusbar interface dependent reimplementation (from vincent)

  Revision 1.157  2004/01/07 18:04:09  micha
  fix getselcount message for non-multiple-selection listbox

  Revision 1.156  2004/01/03 11:57:48  mattias
  applied implementation for LM_LB_GETINDEXAT  from Vincent

  Revision 1.155  2003/12/29 21:56:08  micha
  fix menuitem icon and index (from martin)

  Revision 1.154  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.153  2003/12/27 16:47:18  micha
  fix dialogs owner handle, fixes focusing issue

  Revision 1.152  2003/12/27 16:26:55  micha
  remove redundant window property "lazarus" (from martin)

  Revision 1.151  2003/12/21 11:51:35  micha
  use oldstyledialog flag

  Revision 1.150  2003/12/20 12:54:34  micha
  fix spinedit value retrieval

  Revision 1.149  2003/12/19 21:34:53  micha
  fix compiler problem; wrong code for constants

  Revision 1.148  2003/12/19 18:20:02  micha
  delay property removal until wm_destroy (thx vincent)

  Revision 1.147  2003/12/19 18:18:17  micha
  fix window activation z-order

  Revision 1.146  2003/12/18 10:59:51  micha
  fix notebook page out of bounds while destroying

  Revision 1.145  2003/12/18 10:27:26  micha
  fix fpc 1.9.x compile, limittext, mem free

  Revision 1.144  2003/12/18 10:17:00  micha
  remove non-useful variable wndlist (thx vincent)

  Revision 1.143  2003/12/18 08:51:01  micha
  fix accelerators: now registered per window

  Revision 1.142  2003/12/16 21:04:02  micha
  fix menuitem icon patch, hdcScreen released too soon

  Revision 1.141  2003/12/15 21:57:16  micha
  checklistbox, implement object+checked; from vincent

  Revision 1.140  2003/12/14 20:49:22  micha
  hintwindow focus fix

  Revision 1.139  2003/12/14 19:18:04  micha
  hint fixes: parentfont, font itself, showing/hiding + more

  Revision 1.138  2003/12/13 19:44:42  micha
  hintwindow, color, rectangle size fixes

  Revision 1.137  2003/12/07 22:40:09  mattias
  fixed resizing larger menu icons   from Martin Smat

  Revision 1.136  2003/11/28 19:54:42  micha
  fpc 1.0.10 compatibility

  Revision 1.135  2003/11/25 21:20:38  micha
  implement tchecklistbox

  Revision 1.134  2003/11/25 14:21:28  micha
  new api lclenable,checkmenuitem according to list

  Revision 1.133  2003/11/22 23:56:33  mattias
  fixed win32 intf menu height from Wojciech

  Revision 1.132  2003/11/21 20:32:01  micha
  cleanups; wm_hscroll/wm_vscroll fix

  Revision 1.131  2003/11/21 08:40:54  micha
  menuitems gone that have images, init bug

  Revision 1.130  2003/11/18 07:20:40  micha
  added "included by" notice at top of file

  Revision 1.129  2003/11/16 17:13:20  marc
  * Applied patch from Martin Smat

  Revision 1.128  2003/11/16 16:59:02  marc
  * Fixed DrawOwnerButton

  Revision 1.127  2003/11/09 10:35:19  mattias
  started Menu icons for win32 intf from Martin Smat

  Revision 1.126  2003/11/08 17:41:03  micha
  compiler warning cleanups

  Revision 1.125  2003/10/29 19:47:29  mattias
  fixed win32 compiling

  Revision 1.124  2003/10/29 15:24:28  micha
  fix popupmenu av

  Revision 1.123  2003/10/29 14:24:21  micha
  amenuobject compilation fix

  Revision 1.122  2003/10/28 14:25:37  mattias
  fixed unit circle

  Revision 1.121  2003/10/26 17:34:41  micha
  new interface method to attach a menu to window

  Revision 1.120  2003/10/23 07:45:49  micha
  cleanups; single parent window (single taskbar button)

  Revision 1.119  2003/10/21 15:06:27  micha
  spinedit fix; variables cleanup

  Revision 1.118  2003/10/06 10:53:25  mattias
  fixed redrawing BitBtns from Micha

  Revision 1.117  2003/10/06 10:50:10  mattias
  added recursion to InvalidateClientRectCache

  Revision 1.116  2003/10/02 11:18:09  mattias
  clean ups from Karl

  Revision 1.115  2003/09/30 13:05:59  mattias
  removed FMainForm by Micha

  Revision 1.114  2003/09/27 09:52:44  mattias
  TScrollBox for win32 intf from Karl

  Revision 1.113  2003/09/24 20:43:27  mattias
  fixed wordwrap from Micha

  Revision 1.112  2003/09/21 10:42:48  mattias
  implemented TBitBtn Text+Caption from Micha

  Revision 1.111  2003/09/20 13:27:49  mattias
  varois improvements for ParentColor from Micha

  Revision 1.110  2003/09/18 12:17:25  mattias
  fixed is checks for TCustomXXX controls

  Revision 1.109  2003/09/18 12:15:01  mattias
  fixed is checks for TCustomXXX controls

  Revision 1.108  2003/09/14 09:43:45  mattias
  fixed common dialogs from Karl

  Revision 1.107  2003/09/08 13:29:55  mattias
  clean up

  Revision 1.106  2003/09/08 12:21:48  mattias
  added fpImage reader/writer hooks to TBitmap

  Revision 1.105  2003/09/06 18:37:18  mattias
  fixed checkbox state and typecast bugs

  Revision 1.104  2003/08/31 17:30:49  mattias
  fixed TControl painting for win32

  Revision 1.103  2003/08/31 14:48:15  mattias
  replaced some  as  from Micha

  Revision 1.102  2003/08/30 18:55:42  mattias
  implemented sticked windows from Micha

  Revision 1.101  2003/08/28 09:10:01  mattias
  listbox and comboboxes now set sort and selection at handle creation

  Revision 1.100  2003/08/28 08:14:10  mattias
  implementation of win32 intf borderstyle from Karl

  Revision 1.99  2003/08/27 15:15:42  mattias
  improved setprop from Micha

  Revision 1.98  2003/08/27 09:33:26  mattias
  implements SET_LABEL from Micha

  Revision 1.97  2003/08/27 08:14:37  mattias
  fixed system fonts for win32 intf

  Revision 1.96  2003/08/26 08:12:33  mattias
  applied listbox/combobox patch from Karl

  Revision 1.95  2003/08/25 16:18:16  mattias
  fixed background color of TPanel and clicks of TSpeedButton from Micha

  Revision 1.94  2003/08/23 21:17:09  mattias
  several fixes for the win32 intf, added pending OnResize events

  Revision 1.93  2003/08/23 11:30:51  mattias
  fixed SetComboHeight in win32 intf and finddeclaration of overloaded proc definition

  Revision 1.92  2003/08/22 07:58:38  mattias
  started componenttree

  Revision 1.91  2003/08/21 06:52:47  mattias
  size fixes from Karl

  Revision 1.90  2003/08/19 13:15:18  mattias
  fixed notebook size from Micha

  Revision 1.89  2003/08/17 12:51:35  mattias
  added directory selection dialog from Vincent

  Revision 1.88  2003/08/17 12:26:00  mattias
  fixed parts of the win32 intf size system

  Revision 1.87  2003/08/14 10:36:55  mattias
  added TSelectDirectoryDialog

  Revision 1.86  2003/08/13 21:23:10  mattias
  fixed log

  Revision 1.85  2003/08/13 16:26:07  mattias
  fixed combobox height from Karl

  Revision 1.84  2003/08/12 16:09:54  mattias
  fixed sizing from Karl

  Revision 1.83  2003/08/12 14:02:54  mattias
  fixed keypress/keyup, createcaret on synedit focus

  Revision 1.82  2003/08/11 20:18:46  mattias
  fixed position of control in TGroupBox from Micha

  Revision 1.81  2003/08/09 16:30:34  mattias
  fixed LM_ShowModal for win32 intf from Karl

  Revision 1.80  2003/07/30 21:56:32  marc
  * Fixed LM_APPENDTEXT buffer overrun

  Revision 1.79  2003/07/30 17:41:06  mattias
  added LM_APENDTEXT from Martin Smat

  Revision 1.78  2003/07/28 06:42:42  mattias
  removed debuggging SetName, Patch from Karl Brandt

  Revision 1.77  2003/07/26 10:33:34  mattias
  fixed GetText from Martin

  Revision 1.76  2003/07/26 10:30:44  mattias
  rewritten WM_COMMAND by Micha

  Revision 1.75  2003/07/25 09:28:03  mattias
  fixed notebook page resize from Micha

  Revision 1.74  2003/07/04 17:46:27  mattias
  fixed notebook positioning from Micha

  Revision 1.73  2003/07/04 11:12:27  mattias
  improved default handler from Micha

  Revision 1.72  2003/07/04 10:12:16  mattias
  added default message handler to win32 interface

  Revision 1.71  2003/07/03 18:10:55  mattias
  added fontdialog options to win32 intf from Wojciech Malinowski

  Revision 1.70  2003/07/03 17:19:19  mattias
  added RectVisible from Micha

  Revision 1.69  2003/07/03 08:05:53  mattias
  fixed Criticalsection from Vincent

  Revision 1.68  2003/07/02 20:18:28  mattias
  more cleanups from Micha

  Revision 1.67  2003/07/02 19:35:26  mattias
  fixed AV on start from Vincent

  Revision 1.66  2003/07/02 15:57:04  mattias
  added LCL to win32 cursor mapping from Micha

  Revision 1.65  2003/07/02 15:56:15  mattias
  fixed win32 painting and started creating bitmaps from rawimages

  Revision 1.64  2003/07/01 22:02:55  mattias
  fixed formstyle and redrawing from Micha

  Revision 1.63  2003/06/29 07:16:17  mattias
  fixed compiler warnings

  Revision 1.62  2003/06/28 16:20:19  mattias
  fixed some win32 intf warnings

  Revision 1.61  2003/06/28 13:11:40  mattias
  fixed destroying windows from Micha

  Revision 1.60  2003/06/28 12:49:26  mattias
  fixed LM_SETSIZE from Micha

  Revision 1.59  2003/06/26 14:46:24  mattias
  fixed menu attaching from Micha

  Revision 1.58  2003/06/26 14:24:50  mattias
  fixed progressbar SET_PROPERTIES from Micha

  Revision 1.57  2003/06/25 20:49:58  mattias
  fixed menu destroy from Micha

  Revision 1.56  2003/06/25 20:48:48  mattias
  fixed progressbar from Micha

  Revision 1.55  2003/06/25 15:27:18  mattias
  fixed timer calling conventions from Micha

  Revision 1.54  2003/06/24 21:40:23  mattias
  fixed menu

  Revision 1.53  2003/06/24 15:57:55  mattias
  applied win32 menu patch from Micha Nelissen

  Revision 1.52  2003/06/24 08:32:03  mattias
  applied menu fix from Micha Nelissen

  Revision 1.51  2003/03/25 08:12:39  mattias
  patch from Martin Smat for menu items and default messages

  Revision 1.50  2003/03/18 18:23:07  mattias
  popupmenus for win32 intf from Martin Smat

  Revision 1.49  2003/03/11 23:14:20  mattias
  added TControl.HandleObjectShouldBeVisible

  Revision 1.48  2003/03/06 17:15:49  mattias
  applied patch from Martin Smat fixing LM_SETSIZE, LM_SETDESIGNING, LM_SCREENINIT

  Revision 1.47  2003/02/16 00:43:55  mattias
  fix from Martin Smat for TFileDialogs

  Revision 1.46  2003/02/08 10:37:32  mattias
  applied patch from Martin for TFileDialog

  Revision 1.45  2003/02/01 12:56:10  lazarus
  Keith: My brother fixed the problem where menu items default to disbaled.

  Revision 1.44  2003/01/27 11:25:40  mattias
  menu accelerator patch from Martin Smat

  Revision 1.43  2003/01/19 10:57:46  mattias
  fix WindowProc now react on menu item click from Martin

  Revision 1.42  2003/01/12 19:09:19  mattias
  patch from Martin Smat for dis/enabling menuitems

  Revision 1.41  2003/01/08 18:04:21  mattias
  patch from Martin Smat fixing submenus and menu underscores

  Revision 1.40  2003/01/01 10:46:59  mattias
  fixes for win32 listbox/combobox from Karl Brandt

  Revision 1.39  2002/12/29 18:17:49  mattias
  patch from Martin Smat fixing creating handles

  Revision 1.38  2002/12/28 21:44:51  mattias
  further cleanup

  Revision 1.37  2002/12/28 21:38:50  mattias
  cleanups

  Revision 1.36  2002/12/28 09:42:12  mattias
  toolbutton patch from Martin Smat

  Revision 1.35  2002/12/20 19:08:24  mattias
  notebook patch from vincent

  Revision 1.34  2002/12/19 19:55:37  mattias
  Fixed sending wrong List

  Revision 1.33  2002/12/16 09:02:27  mattias
  applied win32 notebook patch from Vincent

  Revision 1.32  2002/02/09 01:48:23  mattias
  renamed TinterfaceObject.Init to AppInit and TWinControls can now contain childs in gtk

  Revision 1.31  2002/12/13 10:10:44  mattias
  fixed illegal type cast

  Revision 1.30  2002/12/04 20:39:16  mattias
  patch from Vincent: clean ups and fixed crash on destroying window

  Revision 1.29  2002/12/04 19:25:10  mattias
  fix for resizing window with a menu from Martin Smat

  Revision 1.28  2002/11/26 20:51:05  mattias
  applied clipbrd patch from Vincent

  Revision 1.27  2002/11/23 13:48:48  mattias
  added Timer patch from Vincent Snijders

  Revision 1.26  2002/11/15 23:43:54  mattias
  applied patch from Karl Brandt

  Revision 1.25  2002/09/10 06:49:24  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.24  2002/08/29 17:55:04  lazarus
  Keith: Removed form sizing hack.

  Revision 1.23  2002/08/28 17:28:11  lazarus
  Keith: Win32 fixes.  Much appreciation to Markus Lüdin.

  Revision 1.22  2002/08/25 21:33:54  lazarus
  Keith: Minor sizing enhancements

  Revision 1.21  2002/06/08 19:18:34  lazarus
  Keith: Fixed some bugs that were brought to my attention; fixed compilation problem.

  Revision 1.20  2002/05/10 07:43:48  lazarus
  MG: updated licenses

  Revision 1.19  2002/04/03 03:41:29  lazarus
  Keith:
    * Removed more obsolete code
    * Compiles again!

  Revision 1.18  2002/04/03 01:52:42  lazarus
  Keith: Removed obsolete code, in preperation of a pending TWin32Object cleanup

  Revision 1.17  2002/03/17 21:36:52  lazarus
  Keith: Fixed Win32 compilation problems

  Revision 1.16  2002/02/07 08:35:12  lazarus
  Keith: Fixed persistent label captions and a few less noticable things

  Revision 1.15  2002/02/04 10:54:33  lazarus
  Keith:
    * Fixes for Win32
    * Added new listviewtest.pp example

  Revision 1.14  2002/02/03 06:06:25  lazarus
  Keith: Fixed Win32 compilation problems

  Revision 1.13  2002/02/01 10:13:09  lazarus
  Keith: Fixes for Win32

  Revision 1.12  2002/01/31 09:32:07  lazarus
  Keith:
    * Open and save dialogs can now coexist in apps (however, only one of each type of common dialog can be used per app :( )
    * Fixed make all
    * Fixed crash in Windows 98/ME

  Revision 1.11  2002/01/25 19:42:56  lazarus
  Keith: Improved events and common dialogs on Win32

  Revision 1.10  2002/01/24 07:34:50  lazarus
  Keith: Fixed some bugs

  Revision 1.9  2002/01/21 08:42:06  lazarus
  Keith: Fixed some run-time exceptions for FPC 1.1

  Revision 1.8  2002/01/18 09:07:44  lazarus
  Keith: Fixed menu creation

  Revision 1.7  2002/01/18 00:02:45  lazarus
  Keith: TCustomPage can now be a parent

  Revision 1.6  2002/01/17 03:17:44  lazarus
  Keith: Fixed TCustomPage creation

  Revision 1.4  2002/01/05 13:16:09  lazarus
  MG: win32 interface update from Keith Bowes

  Revision 1.3  2001/11/01 22:40:13  lazarus
  MG: applied Keith Bowes win32 interface updates

  Revision 1.2  2001/08/02 12:58:35  lazarus
  MG: win32 interface patch from Keith Bowes

  Revision 1.1  2000/07/13 10:28:31  michael
  + Initial import

 }

