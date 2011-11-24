{%MainUnit winceint.pp}
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
type
  TWinControlAccess = class(TWinControl);

{*************************************************************}
{            callback routines                                }
{*************************************************************}

{-----------------------------------------------------------------------------
  Function: PropEnumProc
  Params: Window - The window with the property
          Str    - The property name
          Data   - The property value
  Returns: Whether the enumeration should continue

  Enumerates and removes properties for the target window
 -----------------------------------------------------------------------------}
function PropEnumProc(Window: Hwnd; Str: PChar; Data: Handle): LongBool; stdcall;
begin
  Result:=false;
  if PtrUInt(Str) <= $FFFF then exit; // global atom handle
  //DebugLn('Trace:PropEnumProc - Start');
  //DebugLn(Format('Trace:PropEnumProc - Property %S (with value 0x%X) from window 0x%X removed',[String(Str), Data, Window]));
  RemoveProp(Window, Str);
  Result := True;
  //DebugLn('Trace:PropEnumProc - Exit');
end;

function WndClassName(Wnd: HWND): WideString; inline;
var
  winClassName: array[0..19] of WideChar;
begin
  GetClassName(Wnd, @winClassName, 20);
  Result := winClassName;
end;

{------------------------------------------------------------------------------
 Function: CallDefaultWindowProc
 Params: Window - The window that receives a message
         Msg    - The message received
         WParam - Word parameter
         LParam - Long-integer parameter
 Returns: 0 if Msg is handled; non-zero long-integer result otherwise

 Passes message on to 'default' handler. This can be a control specific window
 procedure or the default window procedure.
 ------------------------------------------------------------------------------}
function CallDefaultWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
  LParam: Windows.LParam): LResult;
var
  PrevWndProc: Windows.WNDPROC;
  setComboWindow: boolean;
  WindowInfo: PWindowInfo;
begin
  {$ifdef MSG_DEBUG}
    DebugLn('Trace:CallDefaultWindowProc - Start');
  {$endif}

  WindowInfo := GetWindowInfo(Window);
  PrevWndProc := WindowInfo^.DefWndProc;

  if (PrevWndProc = nil) or (PrevWndProc = @WindowProc) // <- prevent recursion
    then
  begin
    {$ifdef MSG_DEBUG}
      DebugLn('Trace:CallDefaultWindowProc - A');
    {$endif}
    Result := Windows.DefWindowProcW(Window, Msg, WParam, LParam)
  end
  else begin
    {$ifdef MSG_DEBUG}
      DebugLn('Trace:CallDefaultWindowProc - B ' + IntToHex(PtrInt(PrevWndProc), 8));
    {$endif}
    Result := Windows.CallWindowProc(PrevWndProc, Window, Msg, WParam, LParam);
  end;
end;

type
  TEraseBkgndCommand = (ecDefault, ecDiscard, ecDiscardNoRemove, ecDoubleBufferNoRemove);
const
  EraseBkgndStackMask = $3;
  EraseBkgndStackShift = 2;
var
  EraseBkgndStack: dword = 0;

{$ifdef MSG_DEBUG}
function EraseBkgndStackToString: string;
var
  I: dword;
begin
  SetLength(Result, 8);
  for I := 0 to 7 do
    Result[8-I] := char(ord('0') + ((EraseBkgndStack shr (I*2)) and $3));
end;
{$endif}

procedure PushEraseBkgndCommand(Command: TEraseBkgndCommand);
begin
  EraseBkgndStack := (EraseBkgndStack shl EraseBkgndStackShift) or dword(Ord(Command));
end;

var
  DisabledForms: TList = nil;

function CheckMouseMovement: boolean;
  // returns true if mouse did not move between lmousebutton down
var
  lCursorPos: TPoint;
  moveX, moveY: integer;
begin
Result := true;
{  GetCursorPos(lCursorPos);
  moveX := lCursorPos.X - MouseDownPos.X;
  moveY := lCursorPos.Y - MouseDownPos.Y;
  Result := (-3 <= moveX) and (moveX <= 3) and (-3 <= moveY) and (moveY <= 3);}
end;

{//roozbeh...any need for this?as we do not have opaque in wince!
function GetNeedParentPaint(AWindowInfo: PWindowInfo; AWinControl: TWinControl): boolean;
begin
  Result := AWindowInfo^.needParentPaint
    and ((AWinControl = nil) or not (csOpaque in AWinControl.ControlStyle));
end;}

{------------------------------------------------------------------------------
 Function: WindowProc
 Params: Window - The window that receives a message
         Msg    - The message received
         WParam - Word parameter
         LParam - Long-integer parameter
  Returns: 0 if Msg is handled; non-zero long-integer result otherwise

  Handles the messages sent to the specified window, in parameter Window, by
  Windows or other applications
 ------------------------------------------------------------------------------}
function WindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
  LParam: Windows.LParam): LResult; {$ifdef win32}stdcall{$else}cdecl{$endif};
Var
  LMessage: TLMessage;
  menuItem: TObject;
  menuHDC: HDC;
  PLMsg: PLMessage;
  R: TRect;
  P: TPoint;
  NewLeft, NewTop, NewWidth, NewHeight: integer;
  lWinControl, ChildWinControl: TWinControl;
  TargetObject: TObject;
  WinProcess: Boolean;
  NotifyUserInput: Boolean;
  OverlayWindow: HWND;
  TargetWindow: HWND;
  eraseBkgndCommand: TEraseBkgndCommand;
  WindowInfo: PWindowInfo;
  Flags: dword;
  ChildWindowInfo: PWindowInfo;
  WindowColor: Integer;

  LMScroll: TLMScroll; // used by WM_HSCROLL
  LMKey: TLMKey; // used by WM_KEYDOWN WM_KEYUP
  LMChar: TLMChar; // used by WM_CHAR
  LMMouse: TLMMouse; // used by WM_LBUTTONDBLCLK
  LMContextMenu: TLMContextMenu;
  LMMouseMove: TLMMouseMove; // used by WM_MOUSEMOVE
  LMMouseEvent: TLMMouseEvent; // used by WM_MOUSEWHEEL
  LMMove: TLMMove; // used by WM_MOVE
  LMNotify: TLMNotify; // used by WM_NOTIFY
  DrawListItemStruct: TDrawListItemStruct; //used by WM_DRAWITEM
  CancelEndSession : Boolean;//use by WM_QUERYENDSESSION

  OrgCharCode: word; // used in WM_CHAR handling


  NMHdr: PNMHdr absolute LParam; // used by WM_NOTIFY
  TmpSize: TSize; // used by WM_MEASUREITEM
  {$ifndef win32}
  Info: SHRGINFO; // used by SHRecognizeGesture in WM_LBUTTONDOWN
  {$endif}

  function GetIsNativeControl(AWindow: HWND): Boolean;
  begin
    Result := False;//WndClassName(AWindow) <> ClsName;
  end;

  { Differences with LCL-Win32:

  * Here there are zero native controls
  * Here nothing needs parent paint
  * The double buffering from LCL-Win32 is not used, instead we have a different
  architecture where double buffering is always on, it is impossible to turn it
  off because all drawings are done to the temporary image first
  }
  procedure SendPaintMessage(ControlDC: HDC);
  var
    DC: HDC;
    lBitmap, lMask: HBITMAP;
    PaintRegion: HRGN;
    PS : TPaintStruct;
    PaintMsg: TLMPaint;
    ORect: TRect;
    WindowOrg: Windows.POINT;
    WindowWidth, WindowHeight: Integer;
    DCIndex: integer;
    parLeft, parTop: integer;
    needParentPaint: boolean;
    BufferWasSaved: Boolean;
    lRawImage: TRawImage;
  begin
    if lWinControl = nil then exit;

    {$IFDEF VerboseCDMessages}
    DebugLn(Format('[SendPaintMessage]: Control:%s:%s', [lWinControl.Name, lWinControl.ClassName]));
    {$ENDIF}

    // create a paint message
    needParentPaint := False;

    GetWindowSize(Window, WindowWidth, WindowHeight);

    // Start the double buffering by checking if we need to increase the buffer
    if (WindowInfo^.BitmapWidth < WindowWidth) or (WindowInfo^.BitmapHeight < WindowHeight) then
    begin
      // first release old objects
      if WindowInfo^.BitmapDC <> 0 then
      begin
        Windows.SelectObject(WindowInfo^.BitmapDC, WindowInfo^.DCBitmapOld);
        Windows.DeleteObject(WindowInfo^.BitmapDC);
      end;
      if WindowInfo^.Bitmap <> 0 then Windows.DeleteObject(WindowInfo^.Bitmap);

      // And now create the new ones
      DC := Windows.GetDC(0);
      WindowInfo^.BitmapDC := Windows.CreateCompatibleDC(0);
      WindowInfo^.BitmapWidth := WindowWidth;
      WindowInfo^.BitmapHeight := WindowHeight;
      WindowInfo^.Bitmap := Windows.CreateCompatibleBitmap(DC, WindowWidth, WindowHeight);
      WindowInfo^.DCBitmapOld := Windows.SelectObject(WindowInfo^.BitmapDC, WindowInfo^.Bitmap);
      Windows.ReleaseDC(0, DC);
    end;

    // Prepare the non-native Canvas if necessary
    if (WindowInfo^.Image = nil) then
    begin
      WinProc_RawImage_FromBitmap(lRawImage, WindowInfo^.Bitmap, 0);
      WindowInfo^.Image := TLazIntfImage.Create(WindowWidth, WindowHeight);
      WindowInfo^.Image.SetRawImage(lRawImage);
    end;
    if (WindowInfo^.Canvas = nil) then WindowInfo^.Canvas := TLazCanvas.Create(WindowInfo^.Image);

    {$ifdef VerboseCDMessages}
    DebugLn(Format('[SendPaintMessage] WindowInfo^.Canvas=%s', [dbghex(PtrInt(WindowInfo^.Canvas))]));
    {$endif}

    // main processing
    WinProcess := false;
    try
      if ControlDC = 0 then
      begin
        // ignore first erase background on themed control, paint will do everything
        DC := Windows.BeginPaint(Window, @PS);
      end else begin
        DC := ControlDC;
        PaintRegion := 0;
      end;
      //Is not necessary to check the result of GetLCLClientBoundsOffset since
      //the false condition (lWincontrol = nil or lWincontrol <> TWinControl) is never met
      //The rect is always initialized with 0
      GetLCLClientBoundsOffset(lWinControl, ORect);
      PaintMsg.Msg := LM_PAINT;
      PaintMsg.PaintStruct := @PS;
      PaintMsg.DC := HDC(WindowInfo^.Canvas);

      // send through message to allow message override, moreover use SendMessage
      // to allow subclass window proc override this message too
{      Include(TWinControlAccess(lWinControl).FWinControlFlags, wcfEraseBackground);
      Windows.SendMessageW(lWinControl.Handle, WM_ERASEBKGND, Windows.WPARAM(DC), 0);
      Exclude(TWinControlAccess(lWinControl).FWinControlFlags, wcfEraseBackground);}

      //DCIndex := Windows.SaveDC(DC);

      {$ifdef VerboseCDMessages}
      DebugLn(Format(':> [SendPaintMessage 2] Before OnPaint Control=%s DC=%x', [lWinControl.Name, PaintMsg.DC]));
      {$endif}

      //MoveWindowOrgEx(DC, ORect.Left, ORect.Top);  <- substitute with a non-native window org change
      DeliverMessage(lWinControl, PaintMsg);
      //Windows.RestoreDC(DC, DCIndex);

      {$ifdef VerboseCDMessages}
      DebugLn(':> [SendPaintMessage 3] After OnPaint');
      {$endif}

      // Now draw all child controls
      RenderChildWinControls(WindowInfo^.Image, WindowInfo^.Canvas,
        TCDWSCustomForm.BackendGetCDWinControlList(TCustomForm(lWinControl)));

      // Now convert the rawimage to a HBITMAP and draw it to the screen
      WindowInfo^.Image.GetRawImage(lRawImage);
      WinProc_RawImage_CreateBitmaps(lRawImage, lBitmap, lMask, True);
      Windows.SelectObject(WindowInfo^.BitmapDC, lBitmap);
      Windows.BitBlt(DC, 0, 0, WindowWidth, WindowHeight, WindowInfo^.BitmapDC, 0, 0, SRCCOPY);
      Windows.SelectObject(WindowInfo^.BitmapDC, WindowInfo^.Bitmap);
      Windows.DeleteObject(lBitmap);

      if ControlDC = 0 then
        Windows.EndPaint(Window, @PS);
    finally
    end;
    {$ifdef VerboseCDMessages}
    DebugLn(':< [SendPaintMessage] Finish');
    {$endif}
  end;

  procedure SendParentPaintMessage(Window, Parent: HWND; ControlDC: HDC);
  {$ifdef DEBUG_WINDOW_ORG}
  var
    Point: TPoint;
  {$endif}
  begin
    GetWin32ControlPos(Window, Parent, P.X, P.Y);

    {$ifdef DEBUG_WINDOW_ORG}
    LCLIntf.GetWindowOrgEx(ControlDC, @Point);
    DebugLn(
      Format(':> [SendParentPaintMessage] DC=%d Moving WindowsOrg From %d,%d By DX=%d DY=%d',
      [ControlDC, Point.X, Point.Y, P.X, P.Y]));
    {$endif}

    MoveWindowOrgEx(ControlDC, -P.X, -P.Y);
    SendPaintMessage(ControlDC);
    MoveWindowOrgEx(ControlDC, P.X, P.Y);

    {$ifdef DEBUG_WINDOW_ORG}
    DebugLn(':< [SendParentPaintMessage] Finish');
    {$endif}
  end;

  procedure HandleSetCursor;
  var
    lControl: TControl;
    BoundsOffset: TRect;
    ACursor: TCursor;
  begin
    if (lWinControl <> nil) and not (csDesigning in lWinControl.ComponentState)
      and (Lo(LParam) = HTCLIENT) then
    begin
      Windows.GetCursorPos(@(Windows.POINT(P)));
      Windows.ScreenToClient(Window, @(Windows.POINT(P)));
      if GetLCLClientBoundsOffset(lWinControl, BoundsOffset) then
      begin
        Dec(P.X, BoundsOffset.Left);
        Dec(P.Y, BoundsOffset.Top);
      end;
      ACursor := Screen.Cursor;
      if ACursor = crDefault then
      begin
        // statictext controls do not get WM_SETCURSOR messages...
        lControl := lWinControl.ControlAtPos(P, [capfOnlyClientAreas,
          capfAllowWinControls, capfHasScrollOffset, capfRecursive]);
        if lControl = nil then
          lControl := lWinControl;
        ACursor := lControl.Cursor;
      end;
      if ACursor <> crDefault then
      begin
        Windows.SetCursor(Screen.Cursors[ACursor]);
        LMessage.Result := 1;
      end;
    end;
    if LMessage.Result = 0 then
    begin
      LMessage.Msg := LM_SETCURSOR;
      LMessage.WParam := WParam;
      LMessage.LParam := LParam;
    end;
    WinProcess := false;
  end;

  procedure HandleSysCommand;
  var
    ParentForm: TCustomForm;
    prevFocus: HWND;
  begin
    // forward keystroke to show window menu, if parent form has no menu
    // if wparam contains SC_KEYMENU, lparam contains key pressed
    // keymenu+space should always bring up system menu
    case (WParam and $FFF0) of
      SC_KEYMENU:
        if (lWinControl <> nil) and (lParam <> VK_SPACE) then
        begin
          ParentForm := GetParentForm(lWinControl);
          if (ParentForm <> nil) and (ParentForm.Menu = nil)
            and (Application <> nil) and (Application.MainForm <> nil)
            and (Application.MainForm <> ParentForm)
            and Application.MainForm.HandleAllocated then
          begin
            targetWindow := Application.MainForm.Handle;
            if IsWindowEnabled(targetWindow) and IsWindowVisible(targetWindow) then
            begin
              prevFocus := Windows.GetFocus;
              Windows.SetFocus(targetWindow);
              PLMsg^.Result := Windows.SendMessageW(targetWindow, WM_SYSCOMMAND, WParam, LParam);
              Windows.SetFocus(prevFocus);
              WinProcess := false;
            end;
          end;
        end;

     //roozbeh : we do not have these in wince!
     { SC_MINIMIZE:
        begin

          if (Application <> nil) and (lWinControl <> nil)
              and (Application.MainForm <> nil)
              and (Application.MainForm = lWinControl) then
                Window := TWinCEWidgetSet(WidgetSet).AppHandle;//redirection

         if (Window = TWinCEWidgetSet(WidgetSet).AppHandle)
              and (Application <> nil)
              and (Application.MainForm<>nil) then
          begin
              Windows.SetWindowPos(Window, HWND_TOP,
                Application.MainForm.Left, Application.MainForm.Top,
                Application.MainForm.Width, 0, SWP_NOACTIVATE);
                if Application.MainForm.HandleAllocated then
                 Windows.ShowWindow(Application.MainForm.Handle,SW_HIDE);

              Application.IntfAppMinimize;
          end;
        end;}

      {SC_RESTORE:
        begin

         if (Window = TWinCEWidgetSet(WidgetSet).AppHandle)
              and (Application <> nil)
              and (Application.MainForm<>nil)
              and Application.MainForm.HandleAllocated then
          begin
            PLMsg^.Result := Windows.DefWindowProc(Window, WM_SYSCOMMAND, WParam, LParam);
            Windows.ShowWindow(Application.MainForm.Handle,SW_SHOW);
            if Windows.IsWindowEnabled(Application.MainForm.Handle)
             then Windows.SetActiveWindow(Application.MainForm.Handle);
            WinProcess := false;

            Application.IntfAppRestore;
          end;
        end;}
    end;
  end;

  function HandleUnicodeChar(AChar: Word): boolean;
  var
    UTF8Char: TUTF8Char;
  begin
    Result := false;
    UTF8Char := UTF8Encode(widestring(WideChar(AChar)));
    lWinControl := WindowInfo^.WinControl;
    if Assigned(lWinControl) then
    begin
      Result:= lWinControl.IntfUTF8KeyPress(UTF8Char, 1, False);

      if UTF8Char='' then
        Result:= true;
    end;
  end;

begin
  //DebugLn('Trace:WindowProc - Start');

  LMessage.Result := 0;
  LMessage.Msg := LM_NULL;
  PLMsg := @LMessage;
  WinProcess := True;
  NotifyUserInput := False;

  //DebugLn('Trace:WindowProc - Getting Object with Callback Procedure');
  WindowInfo := GetWindowInfo(Window);

  lWinControl := WindowInfo^.WinControl;

  {$ifdef VerboseCDMessages}
    DebugLn('WindowProc lWinControl: ',DbgSName(lWinControl),' MSG=',WM_To_String(Msg));
  {$endif}
  if (IgnoreNextCharWindow <> 0) and ((Msg = WM_CHAR) or (Msg = WM_SYSCHAR)) then
  begin
    if IgnoreNextCharWindow = Window then
    begin
      IgnoreNextCharWindow := 0;
      Result := 1;
      exit;
    end;
    IgnoreNextCharWindow := 0;
  end;

  {$ifdef MSG_DEBUG}
    DebugLn('Trace:WindowProc - Case Msg of');
  {$endif}

  case Msg Of
    WM_NULL:
    begin
      CheckSynchronize;
      {TCDWidgetset(Widgetset).CheckPipeEvents;}
    end;
    WM_ACTIVATE:
    begin
      case Lo(WParam) Of
        WA_ACTIVE, WA_CLICKACTIVE:
        begin
          LMessage.Msg := LM_ACTIVATE;
        end;
        WA_INACTIVE:
        begin
          LMessage.Msg := LM_DEACTIVATE;
          // Wince => maybe cast to WM_CLOSE?
        end;
      end;
    end;
    WM_CAPTURECHANGED:
    begin
      LMessage.Msg := LM_CAPTURECHANGED;
    end;
    WM_CHAR:
    begin
     if not HandleUnicodeChar(Word(WParam)) then
      begin
      PLMsg:=@LMChar;
      with LMChar Do
      begin
        Msg := CN_CHAR;
        KeyData := LParam;
        CharCode := Word(Char(WideChar(WParam)));
        OrgCharCode := CharCode;
        Result := 0;
        //DebugLn(Format('WM_CHAR KeyData= %d CharCode= %d ',[KeyData,CharCode]));
      end;
      WinProcess := false;
    end
     else
       WinProcess := true;
   end;
    WM_CLOSE:
    begin
      if (Window = TCDWidgetSet(WidgetSet).AppHandle) and
        (Application.MainForm <> nil) then
      begin
        Windows.SendMessage(Application.MainForm.Handle, WM_CLOSE, 0, 0);
      end else begin
        LMessage.Msg := LM_CLOSEQUERY;
      end;
      // default is to destroy window, inhibit
      WinProcess := false;
    end;
    {
      WM_COMMAND

      Parameters:

      wNotifyCode = HIWORD(wParam);
      wID = LOWORD(wParam);
      hwndCtl = (HWND) lParam;

      If wID is IDOK, then we have received a close message from the
      "OK" button in the title of dialog windows.
    }
    WM_COMMAND:
    begin
      { Handles the "OK" button in the title bar of dialogs }
      if Lo(wParam) = IDOK then
      begin
        if (lWinControl is TCustomForm) and (fsModal in TCustomForm(lWinControl).FormState) then
          TCustomForm(lWinControl).ModalResult := mrOK
        else
          SendMessage(Window, WM_CLOSE, 0, 0);
      end;
(*      else
      begin
        { Handles other reasons for WM_COMMAND }

        if Hi(WParam) < 2 then //1 for accelerator 0 for menu
        begin
          {$ifdef VerboseWinCEMenu}
          DebugLn('[wincecallback] Hi(WParam) < 2');
          {$endif}
          TargetObject := GetMenuItemObject();
        end
        else // menuitem or shortcut
        begin
          TargetObject := nil;
        end;

        if (TargetObject <> nil) and (TargetObject is TMenuItem) then
        begin
          {$ifdef VerboseWinCEMenu}
          DebugLn('[wincecallback] Sending menuitem Click');
          {$endif}

          LMessage.Msg := LM_ACTIVATE;
          TargetObject.Dispatch(LMessage);
          lWinControl := nil;
        end
        else
        begin
          lWinControl := GetWindowInfo(LParam)^.WinControl;
          // buddy controls use 'awincontrol' to designate associated wincontrol
          if lWinControl = nil then
            lWinControl := GetWindowInfo(LParam)^.AWinControl;
        end;

        // no specific message found? try send a general msg
        if (LMessage.Msg = LM_NULL) and (lWinControl <> nil) then
          lWinControl.Perform(CN_COMMAND, WParam, LParam);
      end;*)
    end;
{
  * Besides the fact that LCL does not respond to LM_CREATE, this code is
    probably never reached anyway, as the callback is not set until after
    window creation

    WM_CREATE:
    begin
      //DebugLn('Trace:WindowProc - Got WM_CREATE');
      LMessage.Msg := LM_CREATE;
    end;
}
    WM_CLEAR:
    begin
      LMessage.Msg := LM_CLEAR;
    end;
    WM_COPY:
    begin
      LMessage.Msg := LM_COPY;
    end;
    WM_CUT:
    begin
      LMessage.Msg := LM_CUT;
    end;
    WM_DESTROY:
    begin
      //DebugLn('Trace:WindowProc - Got WM_DESTROY');
      if WindowInfo^.Overlay<>HWND(nil) then
        Windows.DestroyWindow(WindowInfo^.Overlay);
      LMessage.Msg := LM_DESTROY;
    end;
    WM_DESTROYCLIPBOARD:
    begin
      if assigned(OnClipBoardRequest) then begin
//        {$IFDEF VerboseWin32Clipbrd}
//        debugln('WM_DESTROYCLIPBOARD');
//        {$ENDIF}
        OnClipBoardRequest(0, nil);
        OnClipBoardRequest := nil;
        LMessage.Result := 0;
      end;
    end;
    WM_ENABLE:
    begin
      if WParam <> 0 then
        LMessage.Msg := LM_SETEDITABLE;
      if Window = TCDWidgetSet(WidgetSet).FAppHandle then
        if WParam = 0 then
          DisabledForms := Screen.DisableForms(nil, DisabledForms)
        else
          Screen.EnableForms(DisabledForms);
    end;
    WM_ENTERIDLE: Application.Idle(False);
    WM_ERASEBKGND:
    begin
      eraseBkgndCommand := TEraseBkgndCommand(EraseBkgndStack and EraseBkgndStackMask);
      if eraseBkgndCommand = ecDoubleBufferNoRemove then
      begin
      end
      else
      if eraseBkgndCommand <> ecDiscardNoRemove then
        EraseBkgndStack := EraseBkgndStack shr EraseBkgndStackShift;
      if eraseBkgndCommand in [ecDiscard, ecDiscardNoRemove] then
      begin
        Result := 0;
        exit;
      end;
      if not False or (eraseBkgndCommand = ecDoubleBufferNoRemove) then
      begin
        LMessage.Msg := LM_ERASEBKGND;
        LMessage.WParam := WParam;
        LMessage.LParam := LParam;
      end else
      begin
        SendPaintMessage(HDC(WParam));
        LMessage.Result := 1;
      end;
      WinProcess := False;
    end;

{    WM_EXITMENULOOP:
      // is it a popup menu
      if longbool(WPARAM) and assigned(WindowInfo^.PopupMenu) then
        WindowInfo^.PopupMenu.Close;
    WM_GETDLGCODE:
    begin
      LMessage.Result := DLGC_WANTALLKEYS;
      WinProcess := false;
    end;}
{
    * TODO: make it work... icon does not show up yet, so better disable it
    WM_GETICON:
    begin
      if WindowInfo^.WinControl is TCustomForm then
      begin
        LMessage.Result := TCustomForm(WindowInfo^.WinControl).GetIconHandle;
        WinProcess := false;
      end;
    end;
}
    WM_KEYDOWN:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMKey;
      with LMKey Do
      begin
        Msg := CN_KEYDOWN;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
        //DebugLn(Format('WM_KEYDOWN KeyData= %d CharCode= %d ',[KeyData,CharCode]));
        //DebugLn('  lWinControl= '+TComponent(lWinControl).Name+':'+lWinControl.ClassName);
      end;
      WinProcess := false;
    end;
    WM_KEYUP:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMKey;
      with LMKey Do
      begin
        Msg := CN_KEYUP;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
        //DebugLn(Format('WM_KEYUP KeyData= %d CharCode= %d ',[KeyData,CharCode]));
      end;
      WinProcess := false;
    end;
    WM_KILLFOCUS:
    begin
{$ifdef DEBUG_CARET}
      DebugLn('WM_KILLFOCUS received for window ', IntToHex(Window, 8));
{$endif}
      LMessage.Msg := LM_KILLFOCUS;
      LMessage.WParam := WParam;
    end;
    //TODO:LM_KILLCHAR,LM_KILLWORD,LM_KILLLINE
    WM_LBUTTONDBLCLK:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_LBUTTONDBLCLK;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
      end;
    end;
    WM_LBUTTONDOWN:
    begin
      // if mouse-click, focus-change, mouse-click, cursor hasn't moved:
      // simulate double click, assume focus change due to first mouse-click
      if (MouseDownFocusStatus = mfFocusChanged) and (MouseDownFocusWindow = Window)
          and (GetTickCount - MouseDownTime <= GetDoubleClickTime)
          and CheckMouseMovement then
      begin
        PostMessage(Window, WM_LBUTTONDBLCLK, WParam, LParam);
      end;

      {$ifndef win32}
      // Gesture recognition process to enable popup menus.
      if (lWinControl.PopupMenu <> nil) then
      begin
        Info.cbSize := SizeOf(SHRGINFO);
        Info.dwFlags := SHRG_RETURNCMD;
        Info.hwndClient := Window;
        Info.ptDown.x := Lo(LParam);
        Info.ptDown.y := Hi(LParam);

        SHRecognizeGesture(Info);
      end;
      {$endif}

      MouseDownTime := GetTickCount;
      MouseDownWindow := Window;
      MouseDownFocusWindow := 0;
      MouseDownFocusStatus := mfFocusSense;
      GetCursorPos(MouseDownPos);
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_LBUTTONDOWN;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
      end;

      // focus window
      if (Windows.GetFocus <> Window) and
          ((lWinControl = nil) or (lWinControl.CanFocus)) then
        Windows.SetFocus(Window);

    end;
    WM_LBUTTONUP:
    begin
      if (MouseDownWindow = Window) and (MouseDownFocusStatus = mfNone) then
        MouseDownFocusStatus := mfFocusSense;
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_LBUTTONUP;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
      end;
    end;
    WM_MBUTTONDBLCLK:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_MBUTTONDBLCLK;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
      end;
    end;
    WM_MBUTTONDOWN:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_MBUTTONDOWN;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
      end;
    end;
    WM_MBUTTONUP:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_MBUTTONUP;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
      end;
    end;
    WM_MOUSEHOVER:
    begin
      NotifyUserInput := True;
      LMessage.Msg := LM_ENTER;
    end;
    WM_MOUSELEAVE:
    begin
      NotifyUserInput := True;
      LMessage.Msg := LM_LEAVE;
    end;
    WM_MOUSEMOVE:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouseMove;
      with LMMouseMove Do
      begin
        Msg := LM_MOUSEMOVE;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
        // check if this is a spurious WM_MOUSEMOVE message, pos not actually changed
        if (XPos = WindowInfo^.MouseX) and (YPos = WindowInfo^.MouseY) then
        begin
          // do not fire message after all (position not changed)
          Msg := LM_NULL;
          NotifyUserInput := false;
        end else
        if WindowInfo <> @DefaultWindowInfo then
        begin
          // position changed, update window info
          WindowInfo^.MouseX := XPos;
          WindowInfo^.MouseY := YPos;
        end;
      end;
    end;
    WM_MOUSEWHEEL:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouseEvent;
      with LMMouseEvent Do
      begin
        X := SmallInt(Lo(LParam));
        Y := SmallInt(Hi(LParam));
        // check if mouse cursor within this window, otherwise send message to window the mouse is hovering over
        P.X := X;
        P.Y := Y;
        TargetWindow := TCDWidgetSet(WidgetSet).WindowFromPoint(P);
        if TargetWindow = HWND(nil) then
          exit;

        // the mousewheel message is for us
        // windows handles combobox's mousewheel messages
        if (lWinControl=nil) or (lWinControl.FCompStyle <> csComboBox) then
        begin
          Msg := LM_MOUSEWHEEL;
          Button := Lo(WParam);
          WheelDelta := SmallInt(Hi(WParam));
          State := KeysToShiftState(Button);
          UserData := Pointer(GetWindowLong(Window, GWL_USERDATA));
          WinProcess := false;
        end;
      end;
    end;
    {$IFDEF EnableWMDropFiles}
    WM_DROPFILES:
      begin
        LMessage.Msg := LM_DROPFILES;
        LMessage.WParam := WParam;
        LMessage.LParam := LParam;
      end;
    {$ENDIF}
    //TODO:LM_MOVEPAGE,LM_MOVETOROW,LM_MOVETOCOLUMN
    WM_NCACTIVATE:
    begin
      // do not allow main form to be deactivated
      if (Application <> nil) and (Application.MainForm <> nil) and
          Application.MainForm.HandleAllocated and (Window = Application.MainForm.Handle) and
          (WParam = 0) then
      begin
        WParam := 1;
      end;
    end;
    WM_NCHITTEST:
    begin
      if (lWinControl <> nil) then begin
        if (lWinControl.FCompStyle = csHintWindow) then
        begin
          LMessage.Result := HTTRANSPARENT;
          WinProcess := false;
        end;
      end;
    end;
    WM_NCLBUTTONDOWN:
    begin
      NotifyUserInput := True;
      //DebugLn('Trace:WindowProc - Got WM_NCLBUTTONDOWN');
    end;
    WM_PAINT:
    begin
      {$IFDEF DEBUG_WINCE_LABELS}
      DebugLn('[WM_PAINT]');
      {$ENDIF}
      SendPaintMessage(HDC(WParam));
      // SendPaintMessage sets winprocess to false
    end;
    WM_PRINTCLIENT:
    begin
      if ((LParam and PRF_CLIENT) = PRF_CLIENT) and (lWinControl <> nil) then
        SendPaintMessage(HDC(WParam));
    end;
    WM_PASTE:
    begin
      LMessage.Msg := LM_PASTE;
    end;
    WM_RBUTTONDBLCLK:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_RBUTTONDBLCLK;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
      end;
    end;
    WM_RBUTTONDOWN:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_RBUTTONDOWN;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
        Result := 0;
      end;
    end;
    WM_RBUTTONUP:
    begin
      NotifyUserInput := True;
      WinProcess := false;
      PLMsg:=@LMMouse;
      with LMMouse Do
      begin
        Msg := LM_RBUTTONUP;
        XPos := SmallInt(Lo(LParam));
        YPos := SmallInt(Hi(LParam));
        Keys := WParam;
        Result := 0;
      end;
    end;
    WM_CONTEXTMENU:
    begin
      WinProcess := false;
      NotifyUserInput := True;
      PLMsg := @LMContextMenu;
      with LMContextMenu do
      begin
        Msg := LM_CONTEXTMENU;
        XPos := GET_X_LPARAM(LParam);
        YPos := GET_Y_LPARAM(LParam);
        hWnd := Window;
        Result := 0;
      end;
    end;
    WM_SETCURSOR:
    begin
      HandleSetCursor;
    end;
    WM_SETFOCUS:
    begin
{$ifdef DEBUG_CARET}
      DebugLn('WM_SETFOCUS received for window ', IntToHex(Window, 8));
{$endif}
      // handle feature mouse-click, setfocus, mouse-click -> double-click
      if (Window <> MouseDownWindow) and (MouseDownFocusStatus <> mfNone) then
      begin
        MouseDownFocusStatus := mfFocusChanged;
        MouseDownFocusWindow := Window;
      end;
      LMessage.Msg := LM_SETFOCUS;
      if (lWinControl <> nil) and (lWinControl.FCompStyle = csEdit) then
        Windows.SendMessage(Window, EM_SETSEL, 0, -1);
    end;
    WM_SHOWWINDOW:
    begin
      //DebugLn('Trace:WindowProc - Got WM_SHOWWINDOW');
      with TLMShowWindow(LMessage) Do
      begin
        Msg := LM_SHOWWINDOW;
        Show := WParam <> 0;
        Status := LParam;
      end;

      if assigned(lWinControl) and ((WParam<>0) or not lWinControl.Visible)
        and ((WParam=0) or lWinControl.Visible)
        and (Application<>nil) and (lWinControl=Application.MainForm) then
      begin
        if WParam=0 then
          Flags := SW_HIDE
        else
          Flags := SW_SHOWNOACTIVATE;
        Windows.ShowWindow(TCDWidgetSet(WidgetSet).FAppHandle, Flags);
      end;
    end;
    WM_SYSCHAR:
    begin
      PLMsg:=@LMChar;
      with LMChar Do
      begin
        Msg := CN_SYSCHAR;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
        //DebugLn(Format('WM_CHAR KeyData= %d CharCode= %d ',[KeyData,CharCode]));
      end;
      WinProcess := false;
    end;
    WM_SYSCOMMAND:
    begin
      HandleSysCommand;
    end;
    WM_SYSKEYDOWN:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMKey;
      with LMKey Do
      begin
        Msg := CN_SYSKEYDOWN;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
      end;
      WinProcess := false;
    end;
    WM_SYSKEYUP:
    begin
      NotifyUserInput := True;
      PLMsg:=@LMKey;
      with LMKey Do
      begin
        Msg := CN_SYSKEYUP;
        KeyData := LParam;
        CharCode := Word(WParam);
        Result := 0;
      end;
      WinProcess := false;
    end;
    WM_TIMER:
    begin
      LMessage.Msg := LM_TIMER;
      LMessage.WParam := WParam;
      LMessage.LParam := LParam;
    end;
    WM_WINDOWPOSCHANGED:
    begin
      with TLMWindowPosMsg(LMessage) Do
      begin
        Msg := LM_WINDOWPOSCHANGED;
        Unused := WParam;
        WindowPos := PWindowPos(LParam);
      end;
      // cross-interface compatible: complete invalidate on resize
      if (PWindowPos(LParam)^.flags and SWP_NOSIZE) = 0 then
        Windows.InvalidateRect(Window, nil, true);
    end;
{    WM_LCL_SOCK_ASYNC:
    begin
      if (Window = TWinCEWidgetSet(WidgetSet).AppHandle) and
          Assigned(TWinCEWidgetSet(WidgetSet).FOnAsyncSocketMsg) then
        exit(TWinCEWidgetSet(WidgetSet).FOnAsyncSocketMsg(WParam, LParam))
    end;}
    {$ifdef wince}
    WM_HOTKEY:
    begin
      // Implements back-key sending to edits, instead of hiding the form
      // See http://bugs.freepascal.org/view.php?id=16699
      if HIWORD(lParam) = VK_ESCAPE then
      begin
        SHSendBackToFocusWindow(Msg, wParam, lParam);
        Exit;
      end;
    end;
    {$endif}
  else
    // pass along user defined messages
    if Msg >= WM_USER then
    begin
      LMessage.Msg := Msg;
      LMessage.WParam := WParam;
      LMessage.LParam := LParam;
      WinProcess := false;
    end;
  end;

  {$ifdef MSG_DEBUG}
    DebugLn('Trace:WindowProc - End Case Msg of');
  {$endif}

  if WinProcess Then
  begin
    {$ifdef MSG_DEBUG}
      DebugLn('Trace:WindowProc - if WinProcess Then');
    {$endif}
    PLMsg^.Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
    WinProcess := false;
    {$ifdef MSG_DEBUG}
      DebugLn('Trace:WindowProc - End if WinProcess Then');
    {$endif}
  end;

  case Msg of
    WM_MOVE:
    begin
      PLMsg:=@LMMove;
      with LMMove Do
      begin
        Msg := LM_MOVE;
        // MoveType := WParam;   WParam is not defined!
        MoveType := Move_SourceIsInterface;
        if (lWinControl is TCustomForm)
        and (TCustomForm(lWinControl).Parent=nil) then
        begin
          if Windows.GetWindowRect(Window,@R) then
          begin
            XPos := R.Left;
            YPos := R.Top;
          end else begin
            Msg := LM_NULL;
          end;
        end else begin
          if GetWindowRelativePosition(Window,NewLeft,NewTop) then
          begin
            XPos := NewLeft;
            YPos := NewTop;
          end else begin
            Msg := LM_NULL;
          end;
        end;
        if lWinControl <> nil then begin
          {$IFDEF VerboseSizeMsg}
          DebugLn('WinCECallBack WM_MOVE ', dbgsName(lWinControl),
            ' NewPos=',dbgs(XPos),',',dbgs(YPos));
          {$ENDIF}
          if (lWinControl.Left=XPos) and (lWinControl.Top=YPos) then
            exit;
        end;
      end;
    end;
    WM_SIZE:
    begin
      with TLMSize(LMessage) Do
      begin
        Msg := LM_SIZE;
        SizeType := WParam or Size_SourceIsInterface;
        if Window = TCDWidgetSet(WidgetSet).AppHandle then
        begin
          lWinControl := Application.MainForm;
          Window := Application.MainForm.Handle;
        end;
        GetWindowSize(Window, NewWidth, NewHeight);
        Width := NewWidth;
        Height := NewHeight;
        if lWinControl <> nil then
        begin
          {$IFDEF VerboseSizeMsg}
          GetClientRect(Window,R);
          DebugLn('WinCECallback: WM_SIZE '+ dbgsName(lWinControl)+
            ' NewSize='+ dbgs(Width)+','+dbgs(Height)+
            ' OldClientSize='+dbgs(lWinControl.CachedClientWidth)+','+dbgs(lWinControl.CachedClientHeight)+
            ' NewClientSize='+dbgs(R.Right)+','+dbgs(R.Bottom));
          {$ENDIF}
          if (lWinControl.Width<>Width) or (lWinControl.Height<>Height)
              or lWinControl.ClientRectNeedsInterfaceUpdate then
          begin
            lWinControl.DoAdjustClientRectChange;
          end;
        end;
        OverlayWindow := GetWindowInfo(Window)^.Overlay;
        if OverlayWindow <> 0 then
          Windows.SetWindowPos(OverlayWindow, HWND_TOP, 0, 0, NewWidth, NewHeight, SWP_NOMOVE);
      end;
    end;

    //roozbeh : do not have these on ce!
    {WM_ENDSESSION:
    begin
      if (Application<>nil) and (TWinCEWidgetSet(WidgetSet).AppHandle=Window) and
         (WParam>0) and (LParam=0) then
      begin
        LMessage.Msg := LM_NULL; // no need to go through delivermessage
        Application.IntfEndSession();
        LMessage.Result := 0;
      end;
    end;

    WM_QUERYENDSESSION:
    begin
      if (Application<>nil) and (TWinCEWidgetSet(WidgetSet).AppHandle=Window) and
         (LParam=0) then
      begin
        LMessage.Msg := LM_NULL; // no need to go through delivermessage
        CancelEndSession := LMessage.Result=0;
        Application.IntfQueryEndSession(CancelEndSession);
        if CancelEndSession
          then LMessage.Result := 0
          else LMessage.Result := 1;
      end;
    end;}

  end;

  // convert from wince client to lcl client pos.
  //
  // hack to prevent GetLCLClientBoundsOffset from changing mouse client
  // coordinates for TScrollingWinControls, this is required in
  // IsControlMouseMsg and ControlAtPos where unscrolled client coordinates
  // are expected.
  if (PLMsg = @LMMouseMove) and not (lWinControl is TScrollingWinControl) then
  begin
    if GetLCLClientBoundsOffset(Window, R) then
    begin
      Dec(LMMouseMove.XPos, R.Left);
      Dec(LMMouseMove.YPos, R.Top);
    end;
  end else
  if (PLMsg = @LMMouse) and not (lWinControl is TScrollingWinControl) then
  begin
    if GetLCLClientBoundsOffset(Window, R) then
    begin
      Dec(LMMouse.XPos, R.Left);
      Dec(LMMouse.YPos, R.Top);
    end;
  end;

  // application processing
  if NotifyUserInput then
    NotifyApplicationUserInput(PLMsg^.Msg);

  if (lWinControl <> nil) and (PLMsg^.Msg <> LM_NULL) then
    DeliverMessage(lWinControl, PLMsg^);

  // respond to result of LCL handling the message
  case PLMsg^.Msg of
    LM_ERASEBKGND, LM_SETCURSOR, LM_RBUTTONUP:
    begin
      if PLMsg^.Result = 0 then
        WinProcess := true;
    end;

    CN_CHAR, CN_SYSCHAR:
    begin
      // if key not yet processed, let windows process it

      WinProcess := LMChar.Result = 0;
      if (LMChar.Result=1) or (OrgCharCode<>LMChar.CharCode) then
          WParam := Word(WideChar(Char(LMChar.CharCode)));
//    WParam := LMChar.CharCode;
    end;

    CN_KEYDOWN, CN_KEYUP, CN_SYSKEYDOWN, CN_SYSKEYUP:
    begin
      // if key not yet processed, let windows process it
      WinProcess := LMKey.Result = 0;
      WParam := LMKey.CharCode;
    end;

  else
    case Msg of
      WM_LBUTTONDOWN, WM_LBUTTONUP:
      begin
        if MouseDownFocusStatus = mfFocusSense then
          MouseDownFocusStatus := mfNone;
      end;
      WM_NCDESTROY:
      begin
        //roozbeh : test this....
         // free our own data associated with window
        if DisposeWindowInfo(Window) then
          WindowInfo := nil;
        //EnumProps(Window, @PropEnumProc);
      end;
    end;
  end;

  if WinProcess then
  begin
    PLMsg^.Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
    case Msg of
      WM_CHAR, WM_KEYDOWN, WM_KEYUP,
      WM_SYSCHAR, WM_SYSKEYDOWN, WM_SYSKEYUP:
      begin
        PLMsg^.Result := 0;
        case Msg of
          WM_CHAR:
          begin
            // if want chars, then handled already
            PLMsg^.Result := CallDefaultWindowProc(Window, WM_GETDLGCODE, 0, 0) and DLGC_WANTCHARS;
            LMChar.CharCode := Word(WParam);
            LMChar.Msg := LM_CHAR;
          end;
          WM_SYSCHAR:
          begin
            LMChar.CharCode := Word(WParam);
            LMChar.Msg := LM_SYSCHAR;
          end;
          WM_KEYDOWN:
          begin
            LMKey.CharCode := Word(WParam);
            LMKey.Msg := LM_KEYDOWN;
          end;
          WM_KEYUP:
          begin
            LMKey.CharCode := Word(WParam);
            LMKey.Msg := LM_KEYUP;
          end;
          WM_SYSKEYDOWN:
          begin
            LMKey.CharCode := Word(WParam);
            LMKey.Msg := LM_SYSKEYDOWN;
          end;
          WM_SYSKEYUP:
          begin
            LMKey.CharCode := Word(WParam);
            LMKey.Msg := LM_SYSKEYUP;
          end;
        end;

        // we cannot tell for sure windows didn't want the key
        // for WM_CHAR check WM_GETDLGCODE/DLGC_WANTCHARS
        // winapi too inconsistent about return value
        if PLMsg^.Result = 0 then
          DeliverMessage(lWinControl, PLMsg^);

{        // handle the hardware back key (= VK_ESCAPE) for edit controls
        // It should work as the backspace
        if (PLMsg^.Result = 0) and (Msg = WM_KEYDOWN) and (WParam = VK_ESCAPE)
          {and (GetKeyState(VK_CONTROL) >= 0) and (GetKeyState(VK_MENU) >= 0)} then
        begin
          if (WndClassName(Window) = EditClsName) then
          begin
            Windows.SendMessage(Window, WM_KEYDOWN, VK_BACK, -1);
          end;
        end;}
      end;
    end;
  end;

  // ignore WM_(SYS)CHAR message if LCL handled WM_(SYS)KEYDOWN
  if ((Msg = WM_KEYDOWN) or (Msg = WM_SYSKEYDOWN)) then
  begin
    if (PLMsg^.Result <> 0) then
    begin
      IgnoreNextCharWindow := Window;
    end else begin
      // stop ignoring if KEYUP has come by (not all keys generate CHAR)
      // assume WM_CHAR is always preceded by WM_KEYDOWN
      IgnoreNextCharWindow := 0;
    end;
  end;

  { LMMouseEvent and LMInsertText have no Result field }

  if      PLMsg = @LMScroll     then Result := LMScroll.Result
  else if PLMsg = @LMKey        then Result := LMKey.Result
  else if PLMsg = @LMChar       then Result := LMChar.Result
  else if PLMsg = @LMMouse      then Result := LMMouse.Result
  else if PLMsg = @LMMouseMove  then Result := LMMouseMove.Result
  else if PLMsg = @LMMove       then Result := LMMove.Result
  else if PLMsg = @LMNotify     then Result := LMNotify.Result
  else if PLMsg = @LMMouseEvent then Result := 1
  else                               Result := PLMsg^.Result;

  //DebugLn('Trace:WindowProc - Exit');
end;

{------------------------------------------------------------------------------
 Procedure: TimerCallBackProc
 Params: window_hnd - handle of window for timer message, not set in this implementation
         msg        - WM_TIMER message
         idEvent    - timer identifier
         dwTime     - current system time

 Calls the timerfunction in the Timer Object in the LCL
 ------------------------------------------------------------------------------}

procedure TimerCallBackProc(window_hwnd : hwnd; msg , idEvent: UINT; dwTime: DWORD);
 {$ifdef win32}stdcall{$else}cdecl{$endif};
Var
  TimerInfo: PWinCETimerInfo;
  n: Integer;
begin
  n := FTimerData.Count;
  while (n>0) do begin
    dec(n);
    TimerInfo := FTimerData[n];
    if TimerInfo^.TimerID=idEvent then begin
      TimerInfo^.TimerFunc;
      break;
    end;
  end;
end;
