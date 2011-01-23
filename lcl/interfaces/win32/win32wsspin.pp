{ $Id$}
{
 *****************************************************************************
 *                              Win32WSSpin.pp                               *
 *                              --------------                               *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit Win32WSSpin;

{$mode objfpc}{$H+}
{$I win32defines.inc}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  CommCtrl, Windows, Win32Extra,
  Spin, Controls, StdCtrls, LCLType, LCLProc, LMessages,
////////////////////////////////////////////////////
  WSSpin, WSLCLClasses, WSProc,
  Win32Int, Win32Proc, Win32WSStdCtrls, Win32WSControls;

type

  { TWin32WSCustomFloatSpinEdit }

  TWin32WSCustomFloatSpinEdit = class(TWSCustomFloatSpinEdit)
  published
    class procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); override;
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DefaultWndHandler(const AWinControl: TWinControl;
                       var AMessage); override;
    class function GetConstraints(const AControl: TControl;
       const AConstraints: TObject): Boolean; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
    class function GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class function GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double; override;

    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;

    class procedure UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit); override;
  end;

procedure UpdateFloatSpinEditControl(const Handle: HWND;
  const AFloatSpinEdit: TCustomFloatSpinEdit);
procedure UpdateFloatSpinEditText(const ASpinEdit: TCustomFloatSpinEdit;
  const ANewValue: Double);

implementation

uses
  SysUtils;
{ TWin32WSCustomFloatSpinEdit }

function SpinWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
begin
  // before generic window proc
  case Msg of
    WM_PAINT,
    WM_PRINTCLIENT,
    WM_ERASEBKGND:
      begin
        Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
        Exit;
      end;
    WM_DESTROY:
    begin
      DestroyWindow(GetWin32WindowInfo(Window)^.UpDown);
    end;
    WM_ENABLE:
    begin
      Windows.EnableWindow(GetWin32WindowInfo(Window)^.UpDown, WParam <> 0);
    end;
  end;
  Result := WindowProc(Window, Msg, WParam, LParam);
  // after generic window proc
  if Msg = WM_KILLFOCUS then
    UpdateFloatSpinEditControl(Window, TCustomFloatSpinEdit(GetWin32WindowInfo(Window)^.WinControl));
end;

function SpinUpDownWndProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
begin
  case Msg of
    WM_PAINT,
    WM_PRINTCLIENT,
    WM_ERASEBKGND:
      begin
        Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
        Exit;
      end;
  end;
  Result := WindowProc(Window, Msg, WParam, LParam);
end;

procedure UpdateFloatSpinEditControl(const Handle: HWND;
  const AFloatSpinEdit: TCustomFloatSpinEdit);
var
  lWindowInfo: PWin32WindowInfo;
begin
  lWindowInfo := GetWin32WindowInfo(Handle);
  if lWindowInfo <> @DefaultWindowInfo then
  begin
    lWindowInfo^.spinValue := AFloatSpinEdit.Value;
    UpdateFloatSpinEditText(AFloatSpinEdit, AFloatSpinEdit.Value);
  end;
end;

procedure UpdateFloatSpinEditText(const ASpinEdit: TCustomFloatSpinEdit;
  const ANewValue: Double);
var
  newValueText: string;
begin
  newValueText := ASpinEdit.ValueToStr(ANewValue);
  Windows.SendMessage(ASpinEdit.Handle, WM_SETTEXT, 0, Windows.LPARAM(PChar(newValueText)));
end;

class function TWin32WSCustomFloatSpinEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
const
  UDS_HOTTRACK = $0100;
  UpDownHotStyle: array[Boolean] of DWORD = (0, UDS_HOTTRACK);
var
  Params: TCreateWindowExParams;
  HotTracking: BOOL;
  UpDown: HWND;
  Info: PWin32WindowInfo;
  UpDownFlags: DWord;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    SubClassWndProc := @SpinWindowProc;
    if TCustomSpinEdit(AWinControl).BorderStyle = bsSingle then
      FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
    Flags := Flags or ES_AUTOHSCROLL;
    HotTracking := False;
    SystemParametersInfo(SPI_GETHOTTRACKING, 0, @HotTracking, 0);
    UpDownFlags := WS_CHILD or UDS_ALIGNRIGHT or UDS_ARROWKEYS or
      UpDownHotStyle[HotTracking] or ((WS_VISIBLE or WS_DISABLED) and Flags);
    {$IFDEF WindowsUnicodeSupport}
    if UnicodeEnabledOS then
    begin
      Window := CreateWindowExW(FlagsEx, PWideChar(WideString(EditClsName)),
                  PWideChar(UTF8ToUTF16(StrCaption)), Flags,
                  Left, Top, Width, Height, Parent, HMENU(nil), HInstance, nil);
      UpDown := CreateWindowExW(0, UPDOWN_CLASSW, nil, UpDownFlags,
        0, 0, 8, Height, Parent, HMENU(nil), HInstance, nil);
    end
    else
    begin
      Window := CreateWindowEx(FlagsEx, @EditClsName[0],
                  PChar(Utf8ToAnsi(StrCaption)), Flags,
                  Left, Top, Width, Height, Parent, HMENU(nil), HInstance, nil);
      UpDown := CreateWindowEx(0, UPDOWN_CLASSA, nil, UpDownFlags,
        0, 0, 8, Height, Parent, HMENU(nil), HInstance, nil);
    end;
    {$ELSE}
    Window := CreateWindowEx(FlagsEx, @EditClsName[0], PChar(StrCaption),
      Flags, Left, Top, Width, Height, Parent, HMENU(nil), HInstance, nil);
    UpDown := CreateWindowEx(0, UPDOWN_CLASSW, nil, UpDownFlags,
      0, 0, 8, Height, Parent, HMENU(nil), HInstance, nil);
    {$ENDIF}
    Windows.SendMessage(UpDown, UDM_SETBUDDY, WPARAM(Window), 0);
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, True);
  Info := GetWin32WindowInfo(Params.Window);
  Info^.UpDown := UpDown;
  UpdateFloatSpinEditControl(Params.Window, TCustomFloatSpinEdit(AWinControl));
  // init updown control
  Info := AllocWindowInfo(UpDown);
  Info^.AWinControl := AWinControl;
  {$IFDEF WindowsUnicodeSupport}
  if UnicodeEnabledOS then
    Info^.DefWndProc := Windows.WNDPROC(SetWindowLongPtrW(UpDown, GWL_WNDPROC, PtrInt(@SpinUpDownWndProc)))
  else
  {$ENDIF}
  Info^.DefWndProc := Windows.WNDPROC(SetWindowLongPtr(UpDown, GWL_WNDPROC, PtrInt(@SpinUpDownWndProc)));
  SetProp(UpDown, 'WinControl', PtrUInt(AWinControl));
  Result := Params.Window;
end;

class procedure TWin32WSCustomFloatSpinEdit.DefaultWndHandler(const AWinControl: TWinControl; var AMessage);
var
  lWindowInfo: PWin32WindowInfo;
  Message: TLMessage absolute AMessage;
  NewValue: Double;
  SpinEdit: TCustomFloatSpinEdit;
  UpDownMsg: PNMUPDOWN;
begin
  case Message.Msg of
    CN_COMMAND:
    begin
      if HIWORD(Message.WParam) = EN_CHANGE then
      begin
        lWindowInfo := GetWin32WindowInfo(AWinControl.Handle);
        if Assigned(lWindowInfo) and (lWindowInfo <> @DefaultWindowInfo) then
        begin
          SpinEdit := TCustomFloatSpinEdit(lWindowInfo^.WinControl);
          lWindowInfo^.spinValue := SpinEdit.StrToValue(SpinEdit.Text);
        end;
      end;
    end;
    CN_NOTIFY:
    begin
      if PNMHdr(Message.LParam)^.code = UDN_DELTAPOS then
      begin
        UpDownMsg := PNMUPDOWN(Message.LParam);
        lWindowInfo := GetWin32WindowInfo(AWinControl.Handle);
        SpinEdit := TCustomFloatSpinEdit(lWindowInfo^.WinControl);
        if not SpinEdit.ReadOnly then
        begin
          NewValue := SpinEdit.GetLimitedValue(
            lWindowInfo^.spinValue - UpDownMsg^.iDelta * SpinEdit.Increment);
          lWindowInfo^.spinValue := NewValue;

          UpdateFloatSpinEditText(SpinEdit, NewValue);
          Windows.SendMessage(UpDownMsg^.hdr.hwndFrom, UDM_SETPOS32, 0, 500);
        end;
      end;
    end;
  end;
  inherited DefaultWndHandler(AWinControl, AMessage);
end;

class function TWin32WSCustomFloatSpinEdit.GetConstraints(
  const AControl: TControl; const AConstraints: TObject): Boolean;
var
  SizeConstraints: TSizeConstraints absolute AConstraints;
  SizeRect: TRect;
begin
  Result := True;

  if (AConstraints is TSizeConstraints) and TWinControl(AControl).HandleAllocated then
  begin
    Windows.GetWindowRect(GetWin32WindowInfo(TWinControl(AControl).Handle)^.UpDown, @SizeRect);
    SizeConstraints.SetInterfaceConstraints(SizeRect.Right - SizeRect.Left, 0, 0, 0);
  end;
end;

class procedure TWin32WSCustomFloatSpinEdit.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  if MeasureTextForWnd(AWinControl.Handle, 'Fj', PreferredWidth, PreferredHeight) then
  begin
    PreferredWidth := 0;
    Inc(PreferredHeight, 8);
  end;
end;

class procedure TWin32WSCustomFloatSpinEdit.AdaptBounds(const AWinControl: TWinControl;
  var Left, Top, Width, Height: integer; var SuppressMove: boolean);
var
  WinHandle, UpDown: HWND;
  R: TRect;
  UpDownWidth: Integer;
  DWP: HDWP;
begin
  WinHandle := AWinControl.Handle;
  UpDown := GetWin32WindowInfo(WinHandle)^.UpDown;

{
  // detach from buddy first
  Windows.SendMessage(UpDown, UDM_SETBUDDY, 0, 0);
  MoveWindow(WinHandle, Left, Top, Width, Height, True);
  // reattach
  Windows.SendMessage(UpDown, UDM_SETBUDDY, WParam(WinHandle), 0);
}
  GetWindowRect(UpDown, @R);
  UpDownWidth := R.Right - R.Left;

  DWP := BeginDeferWindowPos(2);
  DeferWindowPos(DWP, WinHandle, UpDown, Left, Top, Width - UpDownWidth + 2, Height, SWP_NOACTIVATE);
  DeferWindowPos(DWP, UpDown, 0, Left + Width - UpDownWidth, Top, UpDownWidth, Height, SWP_NOZORDER or SWP_NOACTIVATE);
  EndDeferWindowPos(DWP);

  SuppressMove := True;
end;

class function TWin32WSCustomFloatSpinEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  Result := EditGetSelStart(ACustomEdit.Handle);
end;

class function TWin32WSCustomFloatSpinEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  Result := EditGetSelLength(ACustomEdit.Handle);
end;

class function TWin32WSCustomFloatSpinEdit.GetText(const AWinControl: TWinControl;
  var AText: string): boolean;
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetText') then
    Exit(False);
  AText := GetControlText(AWinControl.Handle);
  Result := True;
end;

class function TWin32WSCustomFloatSpinEdit.GetValue(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double;
begin
  Result := GetWin32WindowInfo(ACustomFloatSpinEdit.Handle)^.spinValue;
end;

class procedure TWin32WSCustomFloatSpinEdit.SetReadOnly
  (const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
begin
  Windows.SendMessage(ACustomEdit.Handle, EM_SETREADONLY, Windows.WPARAM(NewReadOnly), 0);
end;

class procedure TWin32WSCustomFloatSpinEdit.SetSelStart(const ACustomEdit: TCustomEdit;
  NewStart: integer);
begin
  EditSetSelStart(ACustomEdit.Handle, NewStart);
end;

class procedure TWin32WSCustomFloatSpinEdit.SetSelLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
begin
  EditSetSelLength(ACustomEdit.Handle, NewLength);
end;

class procedure TWin32WSCustomFloatSpinEdit.SetText(const AWinControl: TWinControl;
  const AText: string);
begin
  {$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS
    then Windows.SetWindowTextW(AWinControl.Handle, PWideChar(UTF8ToUTF16(AText)))
    else Windows.SetWindowText(AWinControl.Handle, PChar(Utf8ToAnsi(AText)));
  {$else}
    Windows.SetWindowText(AWinControl.Handle, PChar(AText));
  {$endif}
end;

class procedure TWin32WSCustomFloatSpinEdit.ShowHide(
  const AWinControl: TWinControl);
const
  VisibilityToFlag: array[Boolean] of UINT = (SWP_HIDEWINDOW, SWP_SHOWWINDOW);
begin
  Windows.SetWindowPos(AWinControl.Handle, 0, 0, 0, 0, 0,
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or VisibilityToFlag[AWinControl.HandleObjectShouldBeVisible]);
  Windows.SetWindowPos(GetWin32WindowInfo(AWinControl.Handle)^.UpDown, 0, 0, 0, 0, 0,
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or VisibilityToFlag[AWinControl.HandleObjectShouldBeVisible]);
end;

class procedure TWin32WSCustomFloatSpinEdit.UpdateControl(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
begin
  UpdateFloatSpinEditControl(ACustomFloatSpinEdit.Handle, ACustomFloatSpinEdit);
end;

end.
