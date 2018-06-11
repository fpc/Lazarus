{%MainUnit win32int.pp}
{ $Id$ }
{******************************************************************************
                 All GTK interface communication implementations.
                   Initial Revision  : Sun Nov 23 23:53:53 2003


  !! Keep alphabetical !!

  Support routines go to gtkproc.pp

 ******************************************************************************
 Implementation
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

//##apiwiz##sps##   // Do not remove

function TWin32WidgetSet.AddEventHandler(AHandle: THandle; AFlags: dword; 
  AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler;
var
  listlen: dword;
  lListIndex: pdword;
begin
  listlen := Length(FWaitHandles);
  if FWaitHandleCount = listlen then
  begin
    inc(listlen, 16);
    SetLength(FWaitHandles, listlen);
    SetLength(FWaitHandlers, listlen);
  end;
  New(lListIndex);
  FWaitHandles[FWaitHandleCount] := AHandle;
  FWaitHandlers[FWaitHandleCount].ListIndex := lListIndex;
  FWaitHandlers[FWaitHandleCount].UserData := AData;
  FWaitHandlers[FWaitHandleCount].OnEvent := AEventHandler;
  lListIndex^ := FWaitHandleCount;
  Inc(FWaitHandleCount);
{$ifdef DEBUG_ASYNCEVENTS}  
  DebugLn('Waiting for handle: ', IntToHex(AHandle, 8));
{$endif}
  Result := lListIndex;
end;

function TWin32WidgetSet.AddPipeEventHandler(AHandle: THandle;
  AEventHandler: TPipeEvent; AData: PtrInt): PPipeEventHandler;
var
  lHandler: PPipeEventInfo;
begin
  if AEventHandler = nil then exit(nil);
  New(lHandler);
  lHandler^.Handle := AHandle;
  lHandler^.UserData := AData;
  lHandler^.OnEvent := AEventHandler;
  lHandler^.Prev := nil;
  lHandler^.Next := FWaitPipeHandlers;
  if FWaitPipeHandlers <> nil then
    FWaitPipeHandlers^.Prev := lHandler;
  FWaitPipeHandlers := lHandler;
  Result := lHandler;
end;

function TWin32WidgetSet.AddProcessEventHandler(AHandle: THandle;
  AEventHandler: TChildExitEvent; AData: PtrInt): PProcessEventHandler;
var
  lProcessEvent: PProcessEvent;
begin
  if AEventHandler = nil then exit(nil);
  New(lProcessEvent);
  lProcessEvent^.Handle := AHandle;
  lProcessEvent^.UserData := AData;
  lProcessEvent^.OnEvent := AEventHandler;
  lProcessEvent^.Handler := AddEventHandler(AHandle, 0, 
    @HandleProcessEvent, PtrInt(lProcessEvent));
  Result := lProcessEvent;
end;

{------------------------------------------------------------------------------
  Method:  ExtUTF8Out

  As ExtTextOut except that Str is treated as UTF8
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect;
  Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result := ExtTextOut(DC, X, Y, Options, Rect, Str, Count, Dx);
end;

{------------------------------------------------------------------------------
  function FontCanUTF8(Font: HFont): boolean;

  True if font recognizes Unicode UTF8 encoding.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.FontCanUTF8(Font: HFont): boolean;
begin
  Result := True;
end;

type
  TFontIsMonoSpaceRec = record
    Name: string;
    Result: Boolean;
  end;
  PFontIsMonoSpaceRec = ^TFontIsMonoSpaceRec;

function EnumFontsCallBack(
  var LogFont: TEnumLogFontEx;
  var Metric: TNewTextMetricEx;
  FontType: Longint;
  Data: LParam):LongInt; stdcall;
var
  R: PFontIsMonoSpaceRec;
begin
  R := PFontIsMonoSpaceRec(Data);
  if ((logfont.elfLogFont.lfPitchAndFamily and FIXED_PITCH) = FIXED_PITCH)
  and (CompareStr(R^.Name, LogFont.elfLogFont.lfFaceName) = 0) then
  begin
    R^.Result := True;
    Result := 0 // we found it -> stop enumeration
  end else
    Result := 1;
end;


function TWin32WidgetSet.FontIsMonoSpace(Font: HFont): boolean;
var
  LF: LogFontA;
  Res: LongInt;
  DC: HDC;
  Rec: TFontIsMonoSpaceRec;
begin
  Result := False;
  FillChar(LF{%H-}, SizeOf(LogFontA), #0);
  Res := GetObject(Font, SizeOf(LogFontA),@LF);
  //writeln('TWin32WidgetSet.FontIsMonoSpace: Res = ',Res,' SizeOf(LogFont) = ',SizeOf(LogFontA));
  //TWin32WidgetSet.GetObject uses LogFontW and converts back to LogFontA, so Res should be SizeOf(LogFontW)
  if (Res <> SizeOf(LogFontW)) then
    Exit;
  LF.lfCharSet := DEFAULT_CHARSET;
  LF.lfPitchAndFamily := 0;
  Rec.Name := LF.lfFaceName;
  Rec.Result := False;
  DC := GetDC(0);
  try
    EnumFontFamiliesEX(DC, @LF, @EnumFontsCallback, LPARAM(@Rec), 0);
  finally
    ReleaseDC(0, DC);
  end;
  Result := Rec.Result;
end;

procedure TWin32WidgetSet.HandleProcessEvent(AData: PtrInt; AFlags: dword);
var
  lProcessEvent: PProcessEvent absolute AData;
  exitcode: dword;
begin
  if not Windows.GetExitCodeProcess(lProcessEvent^.Handle, exitcode) then
    exitcode := 0;
  lProcessEvent^.OnEvent(lProcessEvent^.UserData, cerExit, exitcode);
end;

{------------------------------------------------------------------------------
  Function: RawImage_QueryDescription
  Params: AFlags:
          ADesc:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.RawImage_QueryDescription(AFlags: TRawImageQueryFlags; var ADesc: TRawImageDescription): Boolean;
begin
  if riqfAlpha in AFlags
  then begin
    //always return rgba description
    if not (riqfUpdate in AFlags)
    then ADesc.Init;

    ADesc.Format := ricfRGBA;
    ADesc.Depth := 32;
    ADesc.BitOrder := riboReversedBits;
    ADesc.ByteOrder := riboLSBFirst;
    ADesc.LineOrder := riloTopToBottom;
    ADesc.LineEnd := rileDWordBoundary;
    ADesc.BitsPerPixel := 32;

    ADesc.AlphaPrec := 8;
    ADesc.AlphaShift := 24;

    if riqfRGB in AFlags
    then begin
      ADesc.RedPrec := 8;
      ADesc.GreenPrec := 8;
      ADesc.BluePrec := 8;
      ADesc.RedShift := 16;
      ADesc.GreenShift := 8;
      ADesc.BlueShift := 0;
    end;
    
    AFlags := AFlags - [riqfRGB, riqfAlpha, riqfUpdate];
    if AFlags = [] then Exit(True);
    
    // continue with default
    Include(AFlags, riqfUpdate);
  end;

  Result := inherited RawImage_QueryDescription(AFlags, ADesc);
  // reduce mem
  if Result and (ADesc.Depth = 24) 
  then ADesc.BitsPerPixel := 24;
end;

procedure TWin32WidgetSet.RemoveProcessEventHandler(var AHandler: PProcessEventHandler);
var
  lProcessEvent: PProcessEvent absolute AHandler;
begin
  if AHandler = nil then exit;
  RemoveEventHandler(lProcessEvent^.Handler);
  Dispose(lProcessEvent);
  AHandler := nil;
end;

procedure TWin32WidgetSet.SetRubberBandRect(const ARubberBand: HWND; const ARect: TRect);
begin
  with ARect do
    SetWindowPos(ARubberBand, 0, Left, Top, Right - Left, Bottom - Top, SWP_NOZORDER or SWP_NOACTIVATE);
end;

{------------------------------------------------------------------------------
  Function:
  Params:

  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreateStandardCursor(ACursor: SmallInt): hCursor;
begin
  Result := 0;
  if ACursor < crLow then Exit;
  if ACursor > crHigh then Exit;

  case ACursor of
    crSqlWait..crDrag, crNone:
    begin
      // TODO: load custom cursors here not in the LCL
    end;
  else
    Result := Windows.LoadCursor(0, LclCursorToWin32CursorMap[ACursor]);
  end;
end;

function DockWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
begin
  if (Msg = WM_ACTIVATE) and (LoWord(WParam) <> WA_INACTIVE) and (LParam <> 0) then
    Windows.SendMessage(LParam, WM_NCACTIVATE, 1, 0);
  Result := Windows.DefWindowProc(Window, Msg, WParam, LParam);
end;

function TWin32WidgetSet.CreateRubberBand(const ARect: TRect; const ABrush: HBrush = 0): HWND;
var
  WindowClass: Windows.WNDCLASS;
  WndClassName: String;
begin
  WndClassName := 'LazRubberBand' + IntToStr(ABrush);

  if not Windows.GetClassInfo(System.HInstance, PChar(WndClassName), WindowClass) then
  begin
    with WindowClass do
    begin
      Style := 0;
      LPFnWndProc := @DockWindowProc;
      CbClsExtra := 0;
      CbWndExtra := 0;
      hInstance := System.HInstance;
      hIcon := Windows.LoadIcon(0, IDI_APPLICATION);
      hCursor := Windows.LoadCursor(0, IDC_ARROW);
      if ABrush = 0 then
        hbrBackground := GetSysColorBrush(COLOR_HIGHLIGHT)
      else
        hbrBackground := ABrush;
      LPSzMenuName := nil;
      LPSzClassName := PChar(WndClassName);
    end;
    Windows.RegisterClass(@WindowClass);
  end;

  if WindowsVersion >= wv2000 then
  begin
    Result := CreateWindowEx(WS_EX_LAYERED or WS_EX_TRANSPARENT or WS_EX_TOPMOST or WS_EX_TOOLWINDOW,
      PChar(WndClassName), PChar(WndClassName), WS_POPUP or WS_VISIBLE,
      ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, AppHandle, 0, System.HInstance, nil);

    SetLayeredWindowAttributes(Result, 0, $30, LWA_ALPHA);
  end
  else
    Result := CreateWindowEx(WS_EX_TOPMOST or WS_EX_TOOLWINDOW,
      PChar(WndClassName), PChar(WndClassName), WS_POPUP or WS_VISIBLE,
      ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, AppHandle, 0, System.HInstance, nil);
end;

{------------------------------------------------------------------------------
  Method: CallbackAllocateHWnd
  Params:   None
  Returns:  Nothing

  Callback for the AllocateHWnd function
 ------------------------------------------------------------------------------}
procedure CallbackAllocateHWnd(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam); stdcall;
var
  Msg: TLMessage;
  PMethod: ^TLCLWndMethod;
begin
  FillChar(Msg{%H-}, SizeOf(Msg), #0);
  
  Msg.msg := uMsg;
  Msg.wParam := wParam;
  Msg.lParam := lParam;

  {------------------------------------------------------------------------------
    Here we get the callback WndMethod associated with this window
   ------------------------------------------------------------------------------}
  PMethod := {%H-}Pointer(Widgetset.GetWindowLong(ahwnd, GWL_USERDATA));

  if Assigned(PMethod) then PMethod^(Msg);
   
  Windows.DefWindowProc(ahwnd, uMsg, wParam, lParam);
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AllocateHWnd
  Params:   Method  - The callback method for the window. Can be nil
  Returns:  A window handle

  Allocates a non-visible window that can be utilized to receive and send message
  
  On Windows, you must call Windows.DefWindowProc(MyHandle, Msg.msg, Msg.wParam, msg.lParam);
  in your callback function, if you provide one at all, of course.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.AllocateHWnd(Method: TLCLWndMethod): HWND;
var
  PMethod: ^TLCLWndMethod;
begin
  Result := Windows.CreateWindow(@ClsName[0],
   '', WS_OVERLAPPED, 0, 0, 0, 0, 0, 0, MainInstance, nil);

  {------------------------------------------------------------------------------
    SetWindowLong has only space for 1 pointer on each slot, but a method is
   referenced as a structure with 2 pointers, so here we allocate memory for
   the structure before it can be used to transport data between the callback
   and this function
   ------------------------------------------------------------------------------}
  if Assigned(Method) then
  begin
    Getmem(PMethod, SizeOf(TMethod));
    PMethod^ := Method;

    Self.SetWindowLong(Result, GWL_USERDATA, {%H-}PtrInt(PMethod));
  end;
  
  Self.SetWindowLong(Result, GWL_WNDPROC, {%H-}PtrInt(@CallbackAllocateHWnd))
end;

function TWin32WidgetSet.AskUser(const DialogCaption, DialogMessage: string;
  DialogType: LongInt; Buttons: TDialogButtons; HelpCtx: Longint): LongInt;
var
  i: Integer;
  Caption: String;
  TaskConfig: TTASKDIALOGCONFIG;
  DialogButtons: PTASKDIALOG_BUTTON;
  State: TApplicationState;
begin
  //TaskDialogIndirect is available in Vista and up, but only if app was built with manifest.
  //The check for the latter is done by checking for ComCtlVersionIE6 (which is set in the manifest)
  //The availability of TaskDialogIndirect does not depend on the status of ThemeServices
  //Issue #0027664
  if (WindowsVersion >= wvVista) and (GetFileVersion(comctl32) >= ComCtlVersionIE6) then
  begin
    FillChar(TaskConfig{%H-}, SizeOf(TaskConfig), 0);
    TaskConfig.cbSize := SizeOf(TaskConfig);
    // if we skip hwndParent our form will be a root window - with the taskbar item and icon
    // this is unwanted
    if Assigned(Screen.ActiveCustomForm) then
      TaskConfig.hwndParent := Screen.ActiveCustomForm.Handle
    else
    if Assigned(Application.MainForm) then
      TaskConfig.hwndParent := Application.MainFormHandle
    else
      TaskConfig.hwndParent := AppHandle;
    TaskConfig.hInstance := HInstance;
    TaskConfig.dwFlags := TDF_ALLOW_DIALOG_CANCELLATION;
    if DialogCaption <> '' then
      Caption := DialogCaption
    else
    case DialogType of
      idDialogConfirm,
      idDialogInfo,
      idDialogWarning,
      idDialogError: Caption := GetDialogCaption(DialogType);
    else
      Caption := Application.Title;
    end;
    TaskConfig.pszWindowTitle := PWideChar(UTF8ToUTF16(Caption));

    case DialogType of
      idDialogConfirm:
        begin
          TaskConfig.hMainIcon := Windows.LoadIcon(0, IDI_QUESTION);
          TaskConfig.dwFlags := TaskConfig.dwFlags or TDF_USE_HICON_MAIN;
        end;
      idDialogInfo: TaskConfig.pszMainIcon := TD_INFORMATION_ICON;
      idDialogWarning: TaskConfig.pszMainIcon := TD_WARNING_ICON;
      idDialogError: TaskConfig.pszMainIcon := TD_ERROR_ICON;
      idDialogShield: TaskConfig.pszMainIcon := TD_SHIELD_ICON;
    else
      TaskConfig.dwFlags := TaskConfig.dwFlags or TDF_USE_HICON_MAIN;
    end;

    TaskConfig.pszContent := PWideChar(UTF8ToUTF16(DialogMessage));

    // question dialog button magic :)

    TaskConfig.cButtons := Buttons.Count;
    GetMem(DialogButtons, SizeOf(TTASKDIALOG_BUTTON) * TaskConfig.cButtons);
    for i := 0 to TaskConfig.cButtons - 1 do
    begin
      DialogButtons[i].nButtonID := Buttons[i].ModalResult;
      DialogButtons[i].pszButtonText := UTF8StringToPWideChar(Buttons[i].Caption);
    end;
    TaskConfig.pButtons := DialogButtons;
    if Assigned(Buttons.DefaultButton) then
      TaskConfig.nDefaultButton := Buttons.DefaultButton.ModalResult;

    State := SaveApplicationState;
    try
      Result := IDCANCEL;
      TaskDialogIndirect(@TaskConfig, @Result, nil, nil);
      if (Result = IDCANCEL) then
      begin
        if Assigned(Buttons.CancelButton) then
          Result := Buttons.CancelButton.ModalResult
        else
          Result := mrCancel;
      end;
    finally
      RestoreApplicationState(State);
      for i := 0 to TaskConfig.cButtons - 1 do
        FreeMem(DialogButtons[i].pszButtonText);
      FreeMem(DialogButtons);
    end;
  end
  else
    Result := inherited AskUser(DialogCaption, DialogMessage, DialogType,
      Buttons, HelpCtx);
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.DeallocateHWnd
  Params:   Wnd   - A Window handle, that was created with AllocateHWnd
  Returns:  Nothing
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.DeallocateHWnd(Wnd: HWND);
var
  PMethod: ^TLCLWndMethod;
begin
  PMethod := {%H-}Pointer(Self.GetWindowLong(Wnd, GWL_USERDATA));

  if Wnd <> 0 then Windows.DestroyWindow(Wnd);

  {------------------------------------------------------------------------------
    This must be done after DestroyWindow, otherwise a Access Violation will
   happen when WM_CLOSE message is sent to the callback

    This memory is for the TMethod structure allocated on AllocateHWnd
   ------------------------------------------------------------------------------}
  if Assigned(PMethod) then Freemem(PMethod);
end;

procedure TWin32WidgetSet.DestroyRubberBand(ARubberBand: HWND);
var
  WndClassName: array[0..255] of Char;
begin
  GetClassName(ARubberBand, @WndClassName, 255);
  // preserve the brush or it will be deleted
  SetClassLongPtr(ARubberBand, GCL_HBRBACKGROUND, 0);
  DestroyWindow(ARubberBand);
  Windows.UnRegisterClass(@WndClassName, System.HINSTANCE);
end;

procedure TWin32WidgetSet.DrawDefaultDockImage(AOldRect, ANewRect: TRect; AOperation: TDockImageOperation);
const
  LineSize = 4;

  procedure DrawHorzLine(DC: HDC; x1, x2, y: integer); inline;
  begin
    PatBlt(DC, x1, y, x2 - x1, LineSize, PATINVERT);
  end;

  procedure DrawVertLine(DC: HDC; y1, y2, x: integer); inline;
  begin
    PatBlt(DC, x, y1, LineSize, y2 - y1, PATINVERT);
  end;

  procedure DefaultDockImage(ARect: TRect);
  var
    DC: HDC;
    NewBrush, OldBrush: HBrush;
  begin
    DC := GetDCEx(0, 0, DCX_LOCKWINDOWUPDATE); // drawing during tracking
    try
      NewBrush := CreatePatternBrush(Win32WidgetSet.DotsPatternBitmap);
      OldBrush := SelectObject(DC, NewBrush);
      DrawHorzLine(DC, ARect.Left, ARect.Right, ARect.Top);
      DrawVertLine(DC, ARect.Top + LineSize, ARect.Bottom - LineSize, ARect.Left);
      DrawHorzLine(DC, ARect.Left, ARect.Right, ARect.Bottom - LineSize);
      DrawVertLine(DC, ARect.Top + LineSize, ARect.Bottom - LineSize, ARect.Right - LineSize);
      DeleteObject(SelectObject(DC, OldBrush));
    finally
      ReleaseDC(0, DC);
    end;
  end;
var
  WindowClass: WndClass;
begin
  if WindowsVersion >= wv2000 then
  begin
    case AOperation of
      disShow:
      begin
        with WindowClass do
        begin
          Style := 0;
          LPFnWndProc := @DockWindowProc;
          CbClsExtra := 0;
          CbWndExtra := 0;
          hInstance := System.HInstance;
          hIcon := Windows.LoadIcon(0, IDI_APPLICATION);
          hCursor := Windows.LoadCursor(0, IDC_ARROW);
          hbrBackground := GetSysColorBrush(COLOR_HIGHLIGHT);
          LPSzMenuName := nil;
          LPSzClassName := 'LazDockWnd';
        end;
        Windows.RegisterClass(@WindowClass);
        FDockWndHandle := CreateWindowEx(WS_EX_LAYERED or WS_EX_TRANSPARENT or WS_EX_TOPMOST or WS_EX_TOOLWINDOW,
          'LazDockWnd', 'LazDockWnd', WS_POPUP or WS_VISIBLE,
          ANewRect.Left, ANewRect.Top, ANewRect.Right - ANewRect.Left, ANewRect.Bottom - ANewRect.Top, AppHandle, 0, System.HINSTANCE, nil);

        SetLayeredWindowAttributes(FDockWndHandle, 0, $30, LWA_ALPHA);
      end;
      disHide:
        begin
          DestroyWindow(FDockWndHandle);
          Windows.UnRegisterClass('LazDockWnd', System.HINSTANCE);
        end;
      disMove:
        with ANewRect do
          SetWindowPos(FDockWndHandle, 0, Left, Top, Right - Left, Bottom - Top, SWP_NOZORDER or SWP_NOACTIVATE);
    end;
  end
  else
  begin
    if AOperation in [disMove, disHide] then
      DefaultDockImage(AOldRect);
    if AOperation in [disMove, disShow] then
      DefaultDockImage(ANewRect);
  end;
end;

procedure TWin32WidgetSet.DrawGrid(DC: HDC; const R: TRect; DX, DY: Integer);
var
  x, y: integer;
  ALogPen: TLogPen;
begin
  GetObject(GetCurrentObject(DC, OBJ_PEN), SizeOf(ALogPen), @ALogPen);
  x := R.Left;
  while x <= R.Right do
  begin
    y := R.Top;
    while y <= R.Bottom do
    begin
      DCSetPixel(DC, X, Y, ALogPen.lopnColor);
      Inc(y, DY);
    end;
    Inc(x, DX);
  end;
end;

{------------------------------------------------------------------------------
  Function: GetAcceleratorString
  Params: AVKey:
          AShiftState:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String;
begin
  //TODO: Implement
  Result := '';
end;

{------------------------------------------------------------------------------
  Function: GetControlConstraints
  Params: Constraints: TObject
  Returns: true on success

  Updates the constraints object (e.g. TSizeConstraints) with interface specific
  bounds.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetControlConstraints(Constraints: TObject): boolean;
var
  SizeConstraints: TSizeConstraints absolute Constraints;
  SizeRect: TRect;
  Height, Width: Integer;
  FixedHeight, FixedWidth: boolean;
  //MinWidth, MinHeight, MaxWidth, MaxHeight: Integer;
begin
  Result := True;

  if Constraints is TSizeConstraints then
  begin
    if (SizeConstraints.Control=nil) then exit;

    FixedHeight := false;
    FixedWidth := false;
    //MinWidth := 0;
    //MinHeight := 0;
    //MaxWidth := 0;
    //MaxHeight := 0;

    if SizeConstraints.Control is TCustomComboBox then
    begin
      // win32 combo (but not csSimple) has fixed height
      FixedHeight := TCustomComboBox(SizeConstraints.Control).Style <> csSimple;
    end;
    
    if (FixedHeight or FixedWidth)
      and TWinControl(SizeConstraints.Control).HandleAllocated then 
    begin
      Windows.GetWindowRect(TWinControl(SizeConstraints.Control).Handle, @SizeRect);

      if FixedHeight then
        Height := SizeRect.Bottom - SizeRect.Top
      else
        Height := 0;
      if FixedWidth then
        Width := SizeRect.Right - SizeRect.Left
      else
        Width := 0;
        
      SizeConstraints.SetInterfaceConstraints(Width, Height, Width, Height);
    end;
  end;
end;

function TWin32WidgetSet.GetDesignerDC(WindowHandle: HWND): HDC;
var
  OverlayWindow: HWND;
  ARect: Windows.RECT;
  WindowInfo, OverlayWindowInfo: PWin32WindowInfo;
begin
  WindowInfo := GetWin32WindowInfo(WindowHandle);
  OverlayWindow := WindowInfo^.Overlay;
  if OverlayWindow = {%H-}HWND(nil) then
  begin
    // create 'overlay' window
    Windows.GetClientRect(WindowHandle, @ARect);
    OverlayWindow := Windows.CreateWindowEx(WS_EX_TRANSPARENT,
        @ClsName, '', WS_CHILD or WS_VISIBLE,
        ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
        WindowHandle, {%H-}HMENU(nil), HInstance, nil);
    OverlayWindowInfo := AllocWindowInfo(OverlayWindow);
    OverlayWindowInfo^.DefWndProc := {%H-}Windows.WNDPROC(SetWindowLong(
        OverlayWindow, GWL_WNDPROC, {%H-}PtrInt(@OverlayWindowProc)));
    OverlayWindowInfo^.WinControl := WindowInfo^.WinControl;
    WindowInfo^.Overlay := OverlayWindow;
  end;
  // bring overlay window to front
  Windows.SetWindowPos(OverlayWindow, HWND_TOP, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE);
  Result := Windows.GetDC(OverlayWindow);
end;

function TWin32WidgetSet.IsDesignerDC(WindowHandle: HWND; DC: HDC): Boolean;
var
  OverlayWindow: HWND;
begin
  OverlayWindow := GetWin32WindowInfo(WindowHandle)^.Overlay;
  if OverlayWindow <> 0 then
    Result := Windows.WindowFromDC(DC) = OverlayWindow
  else
    Result := False;
end;

function TWin32WidgetSet.PromptUser(const DialogCaption, DialogMessage: String;
  DialogType: longint; Buttons: PLongint; ButtonCount, DefaultIndex,
  EscapeResult: Longint): Longint;
var
  i: Integer;
  Caption: String;
  TaskConfig: TTASKDIALOGCONFIG;
  DialogButtons: PTASKDIALOG_BUTTON;
  State: TApplicationState;
begin
  //TaskDialogIndirect is available in Vista and up, but only if app was built with manifest.
  //The check for the latter is done by checking for ComCtlVersionIE6 (which is set in the manifest)
  //The availability of TaskDialogIndirect does not depend on the status of ThemeServices
  //Issue #0027664
  if (WindowsVersion >= wvVista) and (GetFileVersion(comctl32) >= ComCtlVersionIE6) then
  begin
    FillChar(TaskConfig, SizeOf(TaskConfig), 0);
    TaskConfig.cbSize := SizeOf(TaskConfig);
    // if we skip hwndParent our form will be a root window - with the taskbar item and icon
    // this is unwanted
    if Assigned(Screen.ActiveCustomForm) then
      TaskConfig.hwndParent := Screen.ActiveCustomForm.Handle
    else
    if Assigned(Application.MainForm) then
      TaskConfig.hwndParent := Application.MainFormHandle
    else
      TaskConfig.hwndParent := AppHandle;
    TaskConfig.hInstance := HInstance;
    TaskConfig.dwFlags := TDF_ALLOW_DIALOG_CANCELLATION;
    if DialogCaption <> '' then
      Caption := DialogCaption
    else
    case DialogType of
      idDialogConfirm,
      idDialogInfo,
      idDialogWarning,
      idDialogError: Caption := GetDialogCaption(DialogType);
    else
      Caption := Application.Title;
    end;
    TaskConfig.pszWindowTitle := PWideChar(UTF8ToUTF16(Caption));

    case DialogType of
      idDialogConfirm:
        begin
          TaskConfig.hMainIcon := Windows.LoadIcon(0, IDI_QUESTION);
          TaskConfig.dwFlags := TaskConfig.dwFlags or TDF_USE_HICON_MAIN;
        end;
      idDialogInfo: TaskConfig.pszMainIcon := TD_INFORMATION_ICON;
      idDialogWarning: TaskConfig.pszMainIcon := TD_WARNING_ICON;
      idDialogError: TaskConfig.pszMainIcon := TD_ERROR_ICON;
      idDialogShield: TaskConfig.pszMainIcon := TD_SHIELD_ICON;
    else
      TaskConfig.dwFlags := TaskConfig.dwFlags or TDF_USE_HICON_MAIN;
    end;

    TaskConfig.pszContent := PWideChar(UTF8ToUTF16(DialogMessage));

    TaskConfig.cButtons := ButtonCount;
    GetMem(DialogButtons, SizeOf(TTASKDIALOG_BUTTON) * ButtonCount);
    for i := 0 to ButtonCount - 1 do
    begin
      DialogButtons[i].nButtonID := Buttons[i];
      DialogButtons[i].pszButtonText := UTF8StringToPWideChar(GetButtonCaption(Buttons[i]));
    end;
    TaskConfig.pButtons := DialogButtons;
    //we need idButtonXX value
    if DefaultIndex < ButtonCount then
      TaskConfig.nDefaultButton := Buttons[DefaultIndex]
    else
      TaskConfig.nDefaultButton := 0;

    State := SaveApplicationState;
    try
      Result := IDCANCEL;
      TaskDialogIndirect(@TaskConfig, @Result, nil, nil);
      if Result = IDCANCEL then
        Result := EscapeResult;
    finally
      RestoreApplicationState(State);
      for i := 0 to ButtonCount - 1 do
        FreeMem(DialogButtons[i].pszButtonText);
      FreeMem(DialogButtons);
    end;
  end
  else
    Result := inherited PromptUser(DialogCaption, DialogMessage, DialogType,
      Buttons, ButtonCount, DefaultIndex, EscapeResult);
end;

{------------------------------------------------------------------------------
  Function: RawImage_CreateBitmaps
  Params: ARawImage:
          ABitmap:
          AMask:
          ASkipMask: When set there is no mask created
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean): Boolean;
var
  ADesc: TRawImageDescription absolute ARawImage.Description;

  function DoBitmap: Boolean;
  var
    DC: HDC;
    Info: record
      Header: Windows.TBitmapInfoHeader;
      Colors: array[0..1] of Cardinal; // reserve extra color for mono bitmaps
    end;
    DstLinePtr, SrcLinePtr: PByte;
    SrcPixelPtr, DstPixelPtr: PByte;
    DstLineSize, SrcLineSize: PtrUInt;
    x, y: Integer;
    Ridx, Gidx, Bidx, Aidx, Align, SrcBytes, DstBpp: Byte;
  begin
    if (ADesc.BitsPerPixel = 1) and (ADesc.LineEnd = rileWordBoundary)
    then begin
      // default BW, word aligned bitmap
      ABitmap := Windows.CreateBitmap(ADesc.Width, ADesc.Height, 1, 1, ARawImage.Data);
      Exit(ABitmap <> 0);
    end;

    // for 24 bits images, BPP can be 24 or 32
    // 32 shouldn't be use since we don't fill the alpha channel

    if ADesc.Depth = 24
    then DstBpp := 24
    else DstBpp := ADesc.BitsPerPixel;

    FillChar(Info, SizeOf(Info), 0);
    Info.Header.biSize := SizeOf(Info.Header);
    Info.Header.biWidth := ADesc.Width;
    if ADesc.LineOrder = riloTopToBottom
    then Info.Header.biHeight := -ADesc.Height // create top to bottom
    else Info.Header.biHeight := ADesc.Height; // create bottom to top
    Info.Header.biPlanes := 1;
    Info.Header.biBitCount := DstBpp;
    Info.Header.biCompression := BI_RGB;
    {Info.Header.biSizeImage := 0;}
    { first color is black, second color is white, for monochrome bitmap }
    Info.Colors[1] := $FFFFFFFF;

    DC := Windows.GetDC(0);
    // Use createDIBSection, since only devicedepth bitmaps can be selected into a DC
    // when they are created with createDIBitmap
    //  ABitmap := Windows.CreateDIBitmap(DC, Info.Header, CBM_INIT, ARawImage.Data, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS);
    ABitmap := Windows.CreateDIBSection(DC, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS, DstLinePtr, 0, 0);
    Windows.ReleaseDC(0, DC);

    if ABitmap = 0
    then begin
      DebugLn('Windows.CreateDIBSection returns 0. Reason = ' + GetLastErrorText(Windows.GetLastError));
      Exit(False);
    end;
    if DstLinePtr = nil then Exit(False);

    DstLineSize := Windows.MulDiv(DstBpp, ADesc.Width, 8);
    // align to DWord
    Align := DstLineSize and 3;
    if Align > 0
    then Inc(DstLineSize, PtrUInt(4 - Align));

    SrcLinePtr := ARawImage.Data;
    SrcLineSize := ADesc.BytesPerLine;

    // copy the image data
    if ADesc.Depth >= 24
    then begin
      // check if a pixel copy is needed
      // 1) Windows uses alpha channel in 32 bpp modes, despite documentation statement that it is ignored. Tested under Windows XP SP3
      // Wine also relies on this undocumented behaviour!
      // So, we need to cut unused A-channel, otherwise we would get black image
      //
      // 2) incompatible channel order
      ADesc.GetRGBIndices(Ridx, Gidx, Bidx, Aidx);

      if ((ADesc.BitsPerPixel = 32) and (ADesc.Depth = 24))
      or (Bidx <> 0) or (Gidx <> 1) or (Ridx <> 2)
      then begin
        // copy pixels
        SrcBytes := ADesc.BitsPerPixel div 8;

        for y := 0 to ADesc.Height - 1 do
        begin
          DstPixelPtr := DstLinePtr;
          SrcPixelPtr := SrcLinePtr;
          for x := 0 to ADesc.Width - 1 do
          begin
            DstPixelPtr[0] := SrcPixelPtr[Bidx];
            DstPixelPtr[1] := SrcPixelPtr[Gidx];
            DstPixelPtr[2] := SrcPixelPtr[Ridx];

            Inc(DstPixelPtr, 3); //move to the next dest RGB triple
            Inc(SrcPixelPtr, SrcBytes);
          end;

          Inc(DstLinePtr, DstLineSize);
          Inc(SrcLinePtr, SrcLineSize);
        end;

        Exit(True);
      end;
    end;

    // no pixelcopy needed
    // check if we can move using one call
    if ADesc.LineEnd = rileDWordBoundary
    then begin
      Move(SrcLinePtr^, DstLinePtr^, DstLineSize * ADesc.Height);
      Exit(True);
    end;

    //Can't use just one move, as different alignment
    for y := 0 to ADesc.Height - 1 do
    begin
      Move(SrcLinePtr^, DstLinePtr^, DstLineSize);
      Inc(DstLinePtr, DstLineSize);
      Inc(SrcLinePtr, SrcLineSize);
    end;

    Result := True;
  end;

begin
  AMask := 0;
  Result := DoBitmap;
  if not Result then Exit;

  //DbgDumpBitmap(ABitmap, 'CreateBitmaps - Image');
  if ASkipMask then Exit;

  AMask := Windows.CreateBitmap(ADesc.Width, ADesc.Height, 1, 1, ARawImage.Mask);
  if AMask = 0 then  
    DebugLn('Windows.CreateBitmap returns 0. Reason = ' + GetLastErrorText(Windows.GetLastError));
  Result := AMask <> 0;
  //DbgDumpBitmap(AMask, 'CreateBitmaps - Mask');
end;

{------------------------------------------------------------------------------
  Function: RawImage_DescriptionFromBitmap
  Params: ABitmap:
          ADesc:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): Boolean;
var
  ASize: Integer;
  WinDIB: Windows.TDIBSection;
begin
  ASize := Windows.GetObject(ABitmap, SizeOf(WinDIB), @WinDIB);
  Result := ASize > 0;
  if Result then
  begin
    FillRawImageDescription(WinDIB.dsBm, ADesc);
    // if it is not DIB then alpha in bitmaps is not supported => use 0 alpha prec
    if ASize < SizeOf(WinDIB) then
      ADesc.AlphaPrec := 0;
  end;
end;

{------------------------------------------------------------------------------
  Function: RawImage_DescriptionFromDevice
  Params: ADC:
          ADesc:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean;
var
  DC: HDC;
begin
  Result := True;
  
  ADesc.Init;

  if ADC = 0
  then DC := Windows.GetDC(0)
  else DC := ADC;

  ADesc.Format := ricfRGBA;
  ADesc.Width := Windows.GetDeviceCaps(DC, HORZRES);
  ADesc.Height := Windows.GetDeviceCaps(DC, VERTRES);
  ADesc.Depth := Windows.GetDeviceCaps(DC, BITSPIXEL) * Windows.GetDeviceCaps(DC, PLANES);
  ADesc.BitOrder := riboReversedBits;
  ADesc.ByteOrder := riboLSBFirst;
  ADesc.LineOrder := riloTopToBottom;
  ADesc.LineEnd := rileDWordBoundary;
  ADesc.BitsPerPixel := ADesc.Depth;

  if (Windows.GetDeviceCaps(DC, RASTERCAPS) and RC_PALETTE) <> 0
  then begin
    // has palette
    ADesc.PaletteColorCount := Windows.GetDeviceCaps(DC, NUMCOLORS);
  end;
  
  if ADC = 0
  then Windows.ReleaseDC(0, DC);

  FillRawImageDescriptionColors(ADesc);

  ADesc.MaskBitsPerPixel := 1;
  ADesc.MaskShift := 0;
  ADesc.MaskLineEnd := rileWordBoundary;
  ADesc.MaskBitOrder := riboReversedBits;
end;

{------------------------------------------------------------------------------
  Function: RawImage_FromBitmap
  Params: ABitmap:
          AMask:
          ARect:
          ARawImage:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean;
var
  WinDIB: Windows.TDIBSection;
  WinBmp: Windows.TBitmap absolute WinDIB.dsBm;
  ASize: Integer;
  R: TRect;
begin
  ARawImage.Init;
  FillChar(WinDIB, SizeOf(WinDIB), 0);
  ASize := Windows.GetObject(ABitmap, SizeOf(WinDIB), @WinDIB);
  if ASize = 0
  then Exit(False);

  //DbgDumpBitmap(ABitmap, 'FromBitmap - Image');
  //DbgDumpBitmap(AMask, 'FromMask - Mask');

  FillRawImageDescription(WinBmp, ARawImage.Description);
  // if it is not DIB then alpha in bitmaps is not supported => use 0 alpha prec
  if ASize < SizeOf(WinDIB) then
    ARawImage.Description.AlphaPrec := 0;
    
  if ARect = nil
  then begin
    R := Rect(0, 0, WinBmp.bmWidth, WinBmp.bmHeight);
  end
  else begin
    R := ARect^;
    if R.Top > WinBmp.bmHeight then
      R.Top := WinBmp.bmHeight;
    if R.Bottom > WinBmp.bmHeight then
      R.Bottom := WinBmp.bmHeight;
    if R.Left > WinBmp.bmWidth then
      R.Left := WinBmp.bmWidth;
    if R.Right > WinBmp.bmWidth then
      R.Right := WinBmp.bmWidth;
  end;

  ARawImage.Description.Width := R.Right - R.Left;
  ARawImage.Description.Height := R.Bottom - R.Top;
  
  // copy bitmap
  Result := GetBitmapBytes(WinBmp, ABitmap, R, ARawImage.Description.LineEnd, ARawImage.Description.LineOrder, ARawImage.Data, ARawImage.DataSize);

  // check mask
  if AMask <> 0 then
  begin
    if Windows.GetObject(AMask, SizeOf(WinBmp), @WinBmp) = 0
    then Exit(False);

    Result := GetBitmapBytes(WinBmp, AMask, R, ARawImage.Description.MaskLineEnd, ARawImage.Description.LineOrder, ARawImage.Mask, ARawImage.MaskSize);
  end
  else begin
    ARawImage.Description.MaskBitsPerPixel := 0;
  end;
end;

{------------------------------------------------------------------------------
  Function: RawImage_FromDevice
  Params: ADC:
          ARect:
          ARawImage:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean;
const
  FILL_PIXEL: array[0..3] of Byte = ($00, $00, $00, $FF);
var
  Info: record
    Header: Windows.TBitmapInfoHeader;
    Colors: array[0..1] of Cardinal; // reserve extra color for mono bitmaps
  end;

  BitsPtr: Pointer;

  copyDC, fillDC: HDC;
  bmp, copyOld, fillOld, copyBmp, fillBmp: HBITMAP;
  w, h: Integer;

begin
  if Windows.GetObjectType(ADC) = OBJ_MEMDC
  then begin
    // we can use bitmap directly
    bmp := Windows.GetCurrentObject(ADC, OBJ_BITMAP);
    copyBmp := 0;
  end
  else begin
    // we need to copy the image
    // use a dibsection, so we can easily retrieve the bytes
    copyDC := Windows.CreateCompatibleDC(ADC);

    w := Windows.GetDeviceCaps(ADC, DESKTOPHORZRES);
    if w = 0
    then w := Windows.GetDeviceCaps(ADC, HORZRES);
    h := Windows.GetDeviceCaps(ADC, DESKTOPVERTRES);
    if h = 0
    then h := Windows.GetDeviceCaps(ADC, VERTRES);

    FillChar(Info, SizeOf(Info), 0);
    Info.Header.biSize := SizeOf(Info.Header);
    Info.Header.biWidth := w;
    Info.Header.biHeight := -h;
    Info.Header.biPlanes := 1;
    Info.Header.biBitCount := Windows.GetDeviceCaps(ADC, BITSPIXEL);
    Info.Header.biCompression := BI_RGB;

    copyBmp := Windows.CreateDIBSection(copyDC, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS, BitsPtr, 0, 0);
    copyOld := Windows.SelectObject(copyDC, copyBmp);

    // prefill bitmap, to create an alpha channel in case of 32bpp bitmap
    if Info.Header.biBitCount > 24
    then begin
      // using a stretchblt is faster than filling the memory ourselves,
      // which is in its turn faster than using a 24bpp bitmap
      fillBmp := Windows.CreateBitmap(1, 1, 1, 32, @FILL_PIXEL);
      fillDC := Windows.CreateCompatibleDC(ADC);
      fillOld := Windows.SelectObject(fillDC, fillBmp);

      Windows.StretchBlt(copyDC, 0, 0, w, h, fillDC, 0, 0, 1, 1, SRCCOPY);

      Windows.SelectObject(fillDC, fillOld);
      Windows.DeleteDC(fillDC);
      Windows.DeleteObject(fillBmp);

      Windows.BitBlt(copyDC, 0, 0, w, h, ADC, 0, 0, SRCPAINT);
    end
    else begin
      // copy image
      Windows.BitBlt(copyDC, 0, 0, w, h, ADC, 0, 0, SRCCOPY);
    end;

    Windows.SelectObject(copyDC, copyOld);
    Windows.DeleteDC(copyDC);

    bmp := copyBmp;
  end;
  
  if bmp = 0 then Exit(False);

  Result := RawImage_FromBitmap(ARawImage, bmp, 0, @ARect);
  if copyBmp <> 0
  then Windows.DeleteObject(copyBmp);
end;

function TWin32WidgetSet.ReleaseDesignerDC(Window: HWND; DC: HDC): Integer;
var
  OverlayWindow: HWND;
begin
  OverlayWindow := GetWin32WindowInfo(Window)^.Overlay;
  if OverlayWindow <> 0 then
    Result := Windows.ReleaseDC(OverlayWindow, DC)
  else
    Result := 0;
end;

procedure TWin32WidgetSet.RemoveEventHandler(var AHandler: PEventHandler);
var
  lListIndex: pdword absolute AHandler;
  I: dword;
begin
  if AHandler = nil then exit;
{$ifdef DEBUG_ASYNCEVENTS}
  DebugLn('Removing handle: ', IntToHex(FWaitHandles[lListIndex^], 8));
  if Length(FWaitHandles) > 0 then
    DebugLn(' WaitHandleCount=', IntToStr(FWaitHandleCount), ', WaitHandle[0]=', IntToHex(FWaitHandles[0], 8));
{$endif}
  // swap with last one
  if FWaitHandleCount >= 2 then
  begin
    I := lListIndex^;
    FWaitHandles[I] := FWaitHandles[FWaitHandleCount-1];
    FWaitHandlers[I] := FWaitHandlers[FWaitHandleCount-1];
    FWaitHandlers[I].ListIndex^ := I;
  end;
  Dec(FWaitHandleCount);
  Dispose(lListIndex);
  AHandler := nil;
end;

procedure TWin32WidgetSet.RemovePipeEventHandler(var AHandler: PPipeEventHandler);
var
  lHandler: PPipeEventInfo absolute AHandler;
begin
  if AHandler = nil then exit;
  if lHandler^.Prev <> nil then
    lHandler^.Prev^.Next := lHandler^.Next
  else
    FWaitPipeHandlers := lHandler^.Next;
  if lHandler^.Next <> nil then
    lHandler^.Next^.Prev := lHandler^.Prev;
  Dispose(lHandler);
  AHandler := nil;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
