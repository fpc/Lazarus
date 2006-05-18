unit winceproc; 

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, LMessages, LCLType, LCLProc, Controls, Forms, Menus,WinCEWinAPIEmu;
  
  Type
  TEventType = (etNotify, etKey, etKeyPress, etMouseWheel, etMouseUpDown);

  PWindowInfo = ^TWindowInfo;
  TWindowInfo = record
    AccelGroup: HACCEL;
    Accel: HACCEL;
    Overlay: HWND;            // overlay, transparent window on top, used by designer
    PopupMenu: TPopupMenu;
    DefWndProc: WNDPROC;
    ParentPanel: HWND;        // if non-zero, winxp groupbox parent window hack
    WinControl: TWinControl;
    PWinControl: TWinControl; // control to paint for
    AWinControl: TWinControl; // control associated with (for buddy controls)
    List: TStrings;
    DisabledWindowList: TList;// a list of windows that were disabled when showing modal
    hasTabParent: boolean;    // has a tabpage as parent, and is winxp themed
    isTabPage: boolean;       // is window of tabpage
    isComboEdit: boolean;     // is buddy of combobox, the edit control
    isChildEdit: boolean;     // is buddy edit of a control
    isGroupBox: boolean;      // is groupbox, and does not have themed tabpage as parent
    MaxLength: dword;
    DrawItemIndex: integer;   // in case of listbox, when handling WM_DRAWITEM
    DrawItemSelected: boolean;// whether this item is selected LB_GETSEL not uptodate yet
    myButton : HWND;
    MouseX, MouseY: word; // noticing spurious WM_MOUSEMOVE messages
    case integer of
      0: (spinValue: single);
  end;

function CreatePWideCharFromString(inString : string): PWideChar;
function WideStringToString(inWideString : WideString) : String;
procedure DisposePWideChar(inPWideChar : PWideChar);
Function ObjectToHWND(Const AObject: TObject): HWND;

function AllocWindowInfo(Window: HWND): PWindowInfo;
function DisposeWindowInfo(Window: HWND): boolean;
function GetWindowInfo(Window: HWND): PWindowInfo;

//roozbeh:these are simply copy-pasted from win32...i bet most of them can be changed
//or not that much neccessary on wince!
function LCLControlSizeNeedsUpdate(Sender: TWinControl;SendSizeMsgOnDiff: boolean): boolean;
Procedure LCLBoundsToWin32Bounds(Sender: TObject;var Left, Top, Width, Height: Integer);
procedure LCLFormSizeToWin32Size(Form: TCustomForm; var AWidth, AHeight: Integer);
function GetLCLClientBoundsOffset(Sender: TObject; var ORect: TRect): boolean;
function GetLCLClientBoundsOffset(Handle: HWnd; var Rect: TRect): boolean;
procedure GetWin32ControlPos(Window, Parent: HWND; var Left, Top: integer);
function GetDesigningBorderStyle(const AForm: TCustomForm): TFormBorderStyle;
procedure UpdateWindowStyle(Handle: HWnd; Style: integer; StyleMask: integer);
function BorderStyleToWin32Flags(Style: TFormBorderStyle): DWORD;
function BorderStyleToWin32FlagsEx(Style: TFormBorderStyle): DWORD;

Function GetShiftState: TShiftState;

Function DeliverMessage(Const Target: Pointer; Var Message): Integer;
Function DeliverMessage(Const Target: TObject; Var Message: TLMessage): Integer;

function DisableWindowsProc(Window: HWND; Data: LParam): LongBool; stdcall;
procedure DisableApplicationWindows(Window: HWND);
procedure EnableApplicationWindows(Window: HWND);

function MeasureText(const AWinControl: TWinControl; Text: string; var Width, Height: integer): boolean;
function GetControlText(AHandle: HWND): string;

procedure AddToChangedMenus(Window: HWnd);


type
  PDisableWindowsInfo = ^TDisableWindowsInfo;
  TDisableWindowsInfo = record
    NewModalWindow: HWND;
    DisabledWindowList: TList;
  end;

var
  DefaultWindowInfo: TWindowInfo;
  ChangedMenus: TList; // list of HWNDs which menus needs to be redrawn


implementation

uses
  SysUtils, LCLStrConsts, Dialogs, StdCtrls, ExtCtrls,
  LCLIntf; //remove this unit when GetWindowSize is moved to TWSWinControl


function CreatePWideCharFromString(inString : string): PWideChar;
var
tmpWideChar : PWideChar;
begin
    tmpWideChar := PWideChar(SysAllocStringLen(nil,Length(inString)));//it automatically reserves +1 to string!
    MultiByteToWideChar(CP_ACP, 0, PChar(inString), -1, tmpWideChar, Length(inString));
    Result := tmpWideChar;
end;

//well this is diffrent from normal string(widestring) or other rtl functions becouse it uses windows local codepage
//better name for this?!
function WideStringToString(inWideString : WideString) : String;
var
tmpStr : PChar;
//test : string;
inStrLen: integer;
begin
//  test := string(inWideString);
  inStrLen := Length(inWideString);
  tmpStr := StrAlloc(inStrLen+1);
  WideCharToMultiByte(CP_ACP, 0, PWideChar(inWideString), -1, tmpStr, inStrLen,nil,nil);
  char((tmpStr+inStrLen)^) := #0;
  Result := string(tmpStr);
  StrDispose(tmpStr);
end;

procedure DisposePWideChar(inPWideChar: PWideChar);
begin
  SysFreeString(inPWideChar);
end;


function GetLCLClientBoundsOffset(Handle: HWnd; var Rect: TRect): boolean;
var
  OwnerObject: TObject;
begin
  OwnerObject := GetWindowInfo(Handle)^.WinControl;
  Result:=GetLCLClientBoundsOffset(OwnerObject, Rect);
end;

Procedure LCLBoundsToWin32Bounds(Sender: TObject;
  var Left, Top, Width, Height: Integer);
var
  ORect: TRect;
Begin
  if (Sender=nil) or (not (Sender is TWinControl)) then exit;
  if not GetLCLClientBoundsOffset(TWinControl(Sender).Parent, ORect) then exit;
  inc(Left, ORect.Left);
  inc(Top, ORect.Top);
End;

{-------------------------------------------------------------------------------
  function GetLCLClientOriginOffset(Sender: TObject;
    var LeftOffset, TopOffset: integer): boolean;

  Returns the difference between the client origin of a win32 handle
  and the definition of the LCL counterpart.
  For example:
    TGroupBox's client area is the area inside the groupbox frame.
    Hence, the LeftOffset is the frame width and the TopOffset is the caption
    height.
-------------------------------------------------------------------------------}
function GetLCLClientBoundsOffset(Sender: TObject; var ORect: TRect): boolean;
var
  TM: TextMetric;
  DC: HDC;
  Handle: HWND;
  TheWinControl: TWinControl;
  ARect: TRect;
Begin
  Result:=false;
  if (Sender = nil) or (not (Sender is TWinControl)) then exit;
  TheWinControl:=TWinControl(Sender);
  if not TheWinControl.HandleAllocated then exit;
  Handle := TheWinControl.Handle;
  ORect.Left := 0;
  ORect.Top := 0;
  if TheWinControl is TScrollingWinControl then
    with TScrollingWinControl(TheWinControl) do
    begin
      if HorzScrollBar <> nil then
        ORect.Left := -HorzScrollBar.Position;
      if VertScrollBar <> nil then
        ORect.Top := -VertScrollBar.Position;
    end;
  ORect.Bottom := 0;
  ORect.Right := 0;
  If (TheWinControl is TCustomGroupBox) Then
  Begin
    // The client area of a groupbox under win32 is the whole size, including
    // the frame. The LCL defines the client area without the frame.
    // -> Adjust the position
    DC := Windows.GetDC(Handle);
    // add the upper frame with the caption
    GetTextMetrics(DC, TM);
    ORect.Top := TM.TMHeight;
    // add the left frame border
    ORect.Left := 1;
    ORect.Right := -1;
    ORect.Bottom := -1;
    ReleaseDC(Handle, DC);
  End Else
  If TheWinControl is TCustomNoteBook then begin
    // Can't use complete client rect in win32 interface, top part contains the tabs
    Windows.GetClientRect(Handle, @ARect);
    ORect := ARect;
    Windows.SendMessage(Handle, TCM_AdjustRect, 0, LPARAM(@ORect));
    Dec(ORect.Right, ARect.Right);
    Dec(ORect.Bottom, ARect.Bottom);
  end;
{
  if (Windows.GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_CLIENTEDGE) <> 0 then
  begin
    Dec(LeftOffset, Windows.GetSystemMetrics(SM_CXEDGE));
    Dec(TopOffset, Windows.GetSystemMetrics(SM_CYEDGE));
  end;
}
  Result:=true;
end;


procedure GetWin32ControlPos(Window, Parent: HWND; var Left, Top: integer);
var
  parRect, winRect: Windows.TRect;
begin
  Windows.GetWindowRect(Window, @winRect);
  Windows.GetWindowRect(Parent, @parRect);
  Left := winRect.Left - parRect.Left;
  Top := winRect.Top - parRect.Top;
end;


{-------------------------------------------------------------------------------
  function LCLBoundsNeedsUpdate(Sender: TWinControl;
    SendSizeMsgOnDiff: boolean): boolean;

  Returns true if LCL bounds and win32 bounds differ for the control.
-------------------------------------------------------------------------------}
function LCLControlSizeNeedsUpdate(Sender: TWinControl;
  SendSizeMsgOnDiff: boolean): boolean;
var
  Window:HWND;
  LMessage: TLMSize;
  IntfWidth, IntfHeight: integer;
begin
  Result:=false;
  Window:= Sender.Handle;
  LCLIntf.GetWindowSize(Window, IntfWidth, IntfHeight);
  if (Sender.Width = IntfWidth)
  and (Sender.Height = IntfHeight)
  and (not Sender.ClientRectNeedsInterfaceUpdate) then
    exit;
  Result:=true;
  if SendSizeMsgOnDiff then begin
    //writeln('LCLBoundsNeedsUpdate B ',TheWinControl.Name,':',TheWinControl.ClassName,' Sending WM_SIZE');
    Sender.InvalidateClientRectCache(true);
    // send message directly to LCL, some controls not subclassed -> message
    // never reaches LCL
    with LMessage do
    begin
      Msg := LM_SIZE;
      SizeType := SIZE_RESTORED or Size_SourceIsInterface;
      Width := IntfWidth;
      Height := IntfHeight;
    end;
    DeliverMessage(Sender, LMessage);
  end;
end;


{
  Updates the window style of the window indicated by Handle.
  The new style is the Style parameter.
  Only the bits set in the StyleMask are changed,
  the other bits remain untouched.
  If the bits in the StyleMask are not used in the Style,
  there are cleared.
}
procedure UpdateWindowStyle(Handle: HWnd; Style: integer; StyleMask: integer);
var
  CurrentStyle: integer;
  NewStyle: integer;
begin
  CurrentStyle := Windows.GetWindowLong(Handle, GWL_STYLE);
  NewStyle := (Style and StyleMask) or (CurrentStyle and (not StyleMask));
  Windows.SetWindowLong(Handle, GWL_STYLE, NewStyle);
end;

function BorderStyleToWin32Flags(Style: TFormBorderStyle): DWORD;
begin
  Result := WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  case Style of
  bsSizeable, bsSizeToolWin:
    Result := Result or (WS_OVERLAPPED or WS_THICKFRAME or WS_CAPTION);
  bsSingle, bsToolWindow:
    Result := Result or (WS_OVERLAPPED or WS_BORDER or WS_CAPTION);
  bsDialog:
    Result := Result or (WS_POPUP or WS_BORDER or WS_CAPTION);
  bsNone:
    Result := Result or WS_POPUP;
  end;
end;

function BorderStyleToWin32FlagsEx(Style: TFormBorderStyle): DWORD;
begin
  Result := 0;
  case Style of
  bsDialog:
    Result := WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;
  bsToolWindow, bsSizeToolWin:
    Result := WS_EX_TOOLWINDOW;
  end;
end;

function GetDesigningBorderStyle(const AForm: TCustomForm): TFormBorderStyle;
{$NOTE Belongs in Win32WSForms, but is needed in windowproc}
begin
  if csDesigning in AForm.ComponentState then
    Result := bsSizeable
  else
    Result := AForm.BorderStyle;
end;

procedure LCLFormSizeToWin32Size(Form: TCustomForm; var AWidth, AHeight: Integer);
{$NOTE Should be moved to WSWin32Forms, if the windowproc is splitted}
var
  SizeRect: Windows.RECT;
  BorderStyle: TFormBorderStyle;
begin
  with SizeRect do
  begin
    Left := 0;
    Top := 0;
    Right := AWidth;
    Bottom := AHeight;
  end;
  BorderStyle := GetDesigningBorderStyle(Form);
  Windows.AdjustWindowRectEx(@SizeRect, BorderStyleToWin32Flags(
      BorderStyle), false, BorderStyleToWin32FlagsEx(BorderStyle));
  AWidth := SizeRect.Right - SizeRect.Left;
  AHeight := SizeRect.Bottom - SizeRect.Top;
end;

{------------------------------------------------------------------------------
  Function: ObjectToHWND
  Params: AObject - An LCL Object
  Returns: The Window handle of the given object

  Returns the Window handle of the given object, 0 if no object available
 ------------------------------------------------------------------------------}
Function ObjectToHWND(Const AObject: TObject): HWND;
Var
  Handle: HWND;
Begin
  Handle:=0;
  If Integer(AObject) = 0 Then
  Begin
    Assert (False, 'TRACE:[ObjectToHWND] Object not assigned');
  End
  Else If (AObject Is TWinControl) Then
  Begin
    If TWinControl(AObject).HandleAllocated Then
      Handle := TWinControl(AObject).Handle
  End
//  Else If (AObject Is TMenuItem) Then
//  Begin
//    If TMenuItem(AObject).HandleAllocated Then
//      Handle := TMenuItem(AObject).Handle
//  End
//  Else If (AObject Is TMenu) Then
//  Begin
//    If TMenu(AObject).HandleAllocated Then
//      Handle := TMenu(AObject).Items.Handle
//  End
//  Else If (AObject Is TCommonDialog) Then
//  Begin
//    {If TCommonDialog(AObject).HandleAllocated Then }
//    Handle := TCommonDialog(AObject).Handle
//  End
  Else
  Begin
    Assert(False, Format('Trace:[ObjectToHWND] Message received With unhandled class-type <%s>', [AObject.ClassName]));
  End;
  Result := Handle;
  If Handle = 0 Then
    Assert (False, 'Trace:[ObjectToHWND]****** Warning: handle = 0 *******');
End;


{------------------------------------------------------------------------------
  Function: GetShiftState
  Params: None
  Returns: A shift state

  Creates a TShiftState set based on the status when the function was called.
 ------------------------------------------------------------------------------}
Function GetShiftState: TShiftState;
Begin
  Result := [];
  If Hi(GetKeyState(VK_SHIFT)) = 1 Then
    Result := Result + [ssShift];
  If Hi(GetKeyState(VK_CAPITAL)) = 1 Then
    Result := Result + [ssCaps];
  If Hi(GetKeyState(VK_CONTROL)) = 1 Then
    Result := Result + [ssCtrl];
  If Hi(GetKeyState(VK_MENU)) = 1 Then
    Result := Result + [ssAlt];
  If Hi(GetKeyState(VK_SHIFT)) = 1 Then
    Result := Result + [ssShift];
  If Hi(GetKeyState(VK_CAPITAL)) = 1 Then
    Result := Result + [ssCaps];
  If Hi(GetKeyState(VK_CONTROL)) = 1 Then
    Result := Result + [ssCtrl];
  If Hi(GetKeyState(VK_NUMLOCK)) = 1 Then
    Result := Result + [ssNum];
  //TODO: ssSuper
  If Hi(GetKeyState(VK_SCROLL)) = 1 Then
    Result := Result + [ssScroll];
  If ((Hi(GetKeyState(VK_LBUTTON)) = 1) And (GetSystemMetrics(SM_SWAPBUTTON) = 0)) Or ((Hi(GetKeyState(VK_RBUTTON)) = 1) And (GetSystemMetrics(SM_SWAPBUTTON) <> 0)) Then
    Result := Result + [ssLeft];
  If Hi(GetKeyState(VK_MBUTTON)) = 1 Then
    Result := Result + [ssMiddle];
  If ((Hi(GetKeyState(VK_RBUTTON)) = 1) And (GetSystemMetrics(SM_SWAPBUTTON) = 0)) Or ((Hi(GetKeyState(VK_LBUTTON)) = 1) And (GetSystemMetrics(SM_SWAPBUTTON) <> 0)) Then
    Result := Result + [ssRight];
  //TODO: ssAltGr
End;


{------------------------------------------------------------------------------
  Function: DeliverMessage
  Params:    Message - The message to process
  Returns:   True If handled

  Generic function which calls the WindowProc if defined, otherwise the
  dispatcher
 ------------------------------------------------------------------------------}
Function DeliverMessage(Const Target: Pointer; Var Message): Integer;
Begin
  If Target = Nil Then
  begin
    DebugLn('[DeliverMessage Target: Pointer] Nil');
    Exit;
  end;
  If TObject(Target) Is TControl Then
  Begin
    TControl(Target).WinDowProc(TLMessage(Message));
  End
  Else
  Begin
    TObject(Target).Dispatch(TLMessage(Message));
  End;

  Result := TLMessage(Message).Result;
End;

{------------------------------------------------------------------------------
  Function: DeliverMessage
  Params: Target  - The target object
          Message - The message to process
  Returns: Message result

  Generic function which calls the WindowProc if defined, otherwise the
  dispatcher
 ------------------------------------------------------------------------------}
Function DeliverMessage(Const Target: TObject; Var Message: TLMessage): Integer;
Begin
  If Target = Nil Then
  begin
    DebugLn('[DeliverMessage (Target: TObject)] Nil');
    Exit;
  end;
  If Target Is TControl Then
    TControl(Target).WindowProc(Message)
  Else
    Target.Dispatch(Message);
  Result := Message.Result;
End;



function AllocWindowInfo(Window: HWND): PWindowInfo;
var
  WindowInfo: PWindowInfo;
begin
  New(WindowInfo);
  FillChar(WindowInfo^, sizeof(WindowInfo^), 0);
  WindowInfo^.DrawItemIndex := -1;
  WinCEWinAPIEmu.SetProp(Window, {PChar(dword(WindowInfoAtom)),} dword(WindowInfo));
  Result := WindowInfo;
end;

function DisposeWindowInfo(Window: HWND): boolean;
var
  WindowInfo: PWindowInfo;
begin
  WindowInfo := PWindowInfo(WinCEWinAPIEmu.GetProp(Window{, PChar(dword(WindowInfoAtom))}));
  Result := WinCEWinAPIEmu.RemoveProp(Window{, PChar(dword(WindowInfoAtom))})<>0;
  if Result then
  begin
    WindowInfo^.DisabledWindowList.Free;
    Dispose(WindowInfo);
  end;
end;

function GetWindowInfo(Window: HWND): PWindowInfo;
begin
  Result := PWindowInfo(WinCEWinAPIEmu.GetProp(Window{, PChar(dword(WindowInfoAtom))}));

  if Result = nil then Result := @DefaultWindowInfo;
end;


{-----------------------------------------------------------------------------
  Function: DisableWindowsProc
  Params: Window - handle of toplevel windows to be disabled
          Data   - handle of current window form
  Returns: Whether the enumeration should continue

  Used in LM_SHOWMODAL to disable the windows of application thread
  except the current form.
 -----------------------------------------------------------------------------}
function DisableWindowsProc(Window: HWND; Data: LParam): LongBool; stdcall;
var
  Buffer: array[0..15] of Char;
begin
  Result:=true;

  // Don't disable the current window form
  if Window = PDisableWindowsInfo(Data)^.NewModalWindow then exit;

  // Don't disable any ComboBox listboxes
  if (GetClassName(Window, @Buffer, sizeof(Buffer))<sizeof(Buffer))
    and (StrIComp(Buffer, 'ComboLBox')=0) then exit;

  if not IsWindowVisible(Window) or not IsWindowEnabled(Window) then exit;

  PDisableWindowsInfo(Data)^.DisabledWindowList.Add(Pointer(Window));
  EnableWindow(Window,False);
end;

var
  InDisableApplicationWindows: boolean = false;

procedure DisableApplicationWindows(Window: HWND);
var
  DisableWindowsInfo: PDisableWindowsInfo;
  WindowInfo: PWindowInfo;
begin
  // prevent recursive calling when the AppHandle window is disabled
  If InDisableApplicationWindows then exit;
  InDisableApplicationWindows:=true;
  New(DisableWindowsInfo);
  DisableWindowsInfo^.NewModalWindow := Window;
  DisableWindowsInfo^.DisabledWindowList := TList.Create;
  WindowInfo := GetWindowInfo(DisableWindowsInfo^.NewModalWindow);
  WindowInfo^.DisabledWindowList := DisableWindowsInfo^.DisabledWindowList;
// :)) well....
//  EnumThreadWindows(GetWindowThreadProcessId(DisableWindowsInfo^.NewModalWindow, nil),
//    @DisableWindowsProc, LPARAM(DisableWindowsInfo));
  Dispose(DisableWindowsInfo);
  InDisableApplicationWindows := false;
end;

procedure EnableApplicationWindows(Window: HWND);
var
  WindowInfo: PWindowInfo;
  I: integer;
begin
  WindowInfo := GetWindowInfo(Window);
  if WindowInfo^.DisabledWindowList <> nil then
  begin
    for I := 0 to WindowInfo^.DisabledWindowList.Count - 1 do
      EnableWindow(HWND(WindowInfo^.DisabledWindowList.Items[I]), true);
    FreeAndNil(WindowInfo^.DisabledWindowList);
  end;
end;


function MeasureText(const AWinControl: TWinControl; Text: string; var Width, Height: integer): boolean;
var
  textSize: Windows.SIZE;
  winHandle: HWND;
  canvasHandle: HDC;
  oldFontHandle: HFONT;
  tmpText : PWideChar;
begin
  winHandle := AWinControl.Handle;
  canvasHandle := GetDC(winHandle);
  oldFontHandle := SelectObject(canvasHandle, Windows.SendMessage(winHandle, WM_GetFont, 0, 0));
  DeleteAmpersands(Text);
  tmpText := CreatePWideCharFromString(Text);
  Result := Windows.GetTextExtentPoint32(canvasHandle, PWideChar(tmpText), Length(Text), @textSize);
  DisposePWideChar(tmpText);
  if Result then
  begin
    Width := textSize.cx;
    Height := textSize.cy;
  end;
  SelectObject(canvasHandle, oldFontHandle);
  ReleaseDC(winHandle, canvasHandle);
end;


function GetControlText(AHandle: HWND): string;
var
  TextLen: dword;
  tmpWideStr : PWideChar;
begin
  TextLen := GetWindowTextLength(AHandle);
  tmpWideStr := PWideChar(SysAllocStringLen(nil,TextLen + 1));
  GetWindowText(AHandle, PWideChar(tmpWideStr), TextLen + 1);
  Result := WideStringToString(widestring(tmpWideStr));
  DisposePWideChar(tmpWideStr);
end;


{-------------------------------------------------------------------------------
  procedure AddToChangedMenus(Window: HWnd);

  Adds Window to the list of windows which need to redraw the main menu.
-------------------------------------------------------------------------------}
procedure AddToChangedMenus(Window: HWnd);
begin
  if ChangedMenus.IndexOf(Pointer(Window)) = -1 then // Window handle is not yet in the list
    ChangedMenus.Add(Pointer(Window));
end;

{------------------------------------------------------------------------------
  Method: RedrawMenus
  Params:  None
  Returns: Nothing

  Redraws all changed menus
 ------------------------------------------------------------------------------}
{procedure RedrawMenus;
var
  I: integer;
begin
  for I := 0 to  ChangedMenus.Count - 1 do
    DrawMenuBar(HWND(ChangedMenus[I]));
  ChangedMenus.Clear;
end;}



initialization
  FillChar(DefaultWindowInfo, sizeof(DefaultWindowInfo), 0);
  DefaultWindowInfo.DrawItemIndex := -1;

finalization
//unless i implement enumprop i should free my tpropertylist myself!

end.


