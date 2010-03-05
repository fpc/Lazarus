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
  WSSpin, WSLCLClasses,
  Win32Int, Win32Proc, Win32WSStdCtrls, Win32WSControls;

type

  { TWin32WSCustomFloatSpinEdit }

  TWin32WSCustomFloatSpinEdit = class(TWSCustomFloatSpinEdit)
  published
    class procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); override;
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DefaultWndHandler(const AWinControl: TWinControl;
                       var AMessage); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class function  GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double; override;

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

function GetBuddyWindow(AHandle: HWND): HWND;
begin
  Result := SendMessage(AHandle, UDM_GETBUDDY, 0, 0)
end;

function SpinWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  BuddyWindow: HWND;
begin
  // before generic window proc
  case Msg of
    WM_DESTROY:
    begin
      BuddyWindow := GetBuddyWindow(Window);
      DestroyWindow(BuddyWindow);
    end;
    WM_ENABLE:
    begin
      BuddyWindow := GetBuddyWindow(Window);
      Windows.EnableWindow(BuddyWindow, WParam <> 0);
    end;
  end;
  Result := WindowProc(Window, Msg, WParam, LParam);
  // after generic window proc
  case Msg of
    WM_SETFOCUS:
    begin
      BuddyWindow := GetBuddyWindow(Window);
      Windows.SetFocus(BuddyWindow);
      // don't select text in edit, if user clicked on the up down and the edit
      // was already focused
      if HWND(WPARAM)<>BuddyWindow then ;
        // for LCL controls this is done in win32callback.inc
        Windows.SendMessage(BuddyWindow, EM_SETSEL, 0, -1);
    end;
  end;
end;

function SpinBuddyWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  AWindowInfo: PWin32WindowInfo;
begin
  Result := WindowProc(Window, Msg, WParam, LParam);
  if Msg = WM_KILLFOCUS then
  begin
    AWindowInfo := GetWin32WindowInfo(Window);
    if AWindowInfo^.AWinControl is TCustomFloatSpinEdit then
      UpdateFloatSpinEditControl(AWindowInfo^.AWinControl.Handle,
        TCustomFloatSpinEdit(AWindowInfo^.AWinControl));
  end;
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
  editHandle: HWND;
  newValueText: string;
begin
  editHandle := GetBuddyWindow(ASpinEdit.Handle);
  newValueText := ASpinEdit.ValueToStr(ANewValue);
  Windows.SendMessage(editHandle, WM_SETTEXT, 0, Windows.LPARAM(PChar(newValueText)));
end;

class function TWin32WSCustomFloatSpinEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    SubClassWndProc := @SpinWindowProc;
    {$IFDEF WindowsUnicodeSupport}
    if UnicodeEnabledOS then
      Buddy := CreateWindowExW(WS_EX_CLIENTEDGE, PWideChar(WideString(EditClsName)),
                  PWideChar(UTF8ToUTF16(StrCaption)), Flags or ES_AUTOHSCROLL,
                  Left, Top, Width, Height, Parent, HMENU(nil), HInstance, nil)
    else
      Buddy := CreateWindowEx(WS_EX_CLIENTEDGE, @EditClsName[0],
                  PChar(Utf8ToAnsi(StrCaption)), Flags or ES_AUTOHSCROLL,
                  Left, Top, Width, Height, Parent, HMENU(nil), HInstance, nil);
    {$ELSE}
    Buddy := CreateWindowEx(WS_EX_CLIENTEDGE, @EditClsName[0], PChar(StrCaption),
      Flags or ES_AUTOHSCROLL, Left, Top, Width, Height, Parent, HMENU(nil), HInstance, nil);
    {$ENDIF}
    Window := CreateUpDownControl(Flags or DWORD(WS_BORDER or UDS_ALIGNRIGHT or UDS_ARROWKEYS),
      0, 0,       // pos -  ignored for buddy
      0, 0,       // size - ignored for buddy
      Parent, 0, HInstance, Buddy,
      1000, 0, 500);
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, true);
  UpdateFloatSpinEditControl(Params.Window, TCustomFloatSpinEdit(AWinControl));
  // init buddy
  Params.SubClassWndProc := @SpinBuddyWindowProc;
  WindowCreateInitBuddy(AWinControl, Params);
  Params.BuddyWindowInfo^.isChildEdit := True;
  Params.WindowInfo^.askBuddyCoords := True;
  // make possible LCL Wincontrol identification by Buddy handle
  // TODO: should move to widget specific SetProp method
  SetProp(Params.Buddy, 'WinControl', PtrUInt(AWinControl));
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
        if lWindowInfo <> @DefaultWindowInfo then
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
        lWindowInfo := GetWin32WindowInfo(UpDownMsg^.hdr.hwndFrom);
        SpinEdit := TCustomFloatSpinEdit(lWindowInfo^.WinControl);
        if not SpinEdit.ReadOnly then
        begin
          NewValue := SpinEdit.GetLimitedValue(
            lWindowInfo^.spinValue + UpDownMsg^.iDelta * SpinEdit.Increment);
          lWindowInfo^.spinValue := NewValue;

          UpdateFloatSpinEditText(SpinEdit, NewValue);
          Windows.SendMessage(UpDownMsg^.hdr.hwndFrom, UDM_SETPOS32, 0, 500);
        end;
      end;
    end;
  end;
  inherited DefaultWndHandler(AWinControl, AMessage);
end;

class procedure TWin32WSCustomFloatSpinEdit.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  if MeasureTextForWnd(GetBuddyWindow(AWinControl.Handle), 'Fj', PreferredWidth, PreferredHeight) then
  begin
    PreferredWidth := 0;
    Inc(PreferredHeight, 8);
  end;
end;

class procedure TWin32WSCustomFloatSpinEdit.AdaptBounds(const AWinControl: TWinControl;
  var Left, Top, Width, Height: integer; var SuppressMove: boolean);
var
  WinHandle, BuddyHandle: HWND;
  R: TRect;
  WindowWidth: Integer;
begin
  WinHandle := AWinControl.Handle;
{
  // detach from buddy first
  BuddyHandle := Windows.SendMessage(WinHandle, UDM_SETBUDDY, 0, 0);
  MoveWindow(BuddyHandle, Left, Top, Width, Height, True);
  // reattach
  Windows.SendMessage(WinHandle, UDM_SETBUDDY, BuddyHandle, 0);
}
  BuddyHandle := Windows.SendMessage(WinHandle, UDM_GETBUDDY, 0, 0);

  GetWindowRect(WinHandle, @R);
  WindowWidth := R.Right - R.Left;

  MoveWindow(BuddyHandle, Left, Top, Width - WindowWidth + 2, Height, True);
  MoveWindow(WinHandle, Left + Width - WindowWidth, Top, WindowWidth, Height, True);

  SuppressMove := True;
end;

class function TWin32WSCustomFloatSpinEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  Result := EditGetSelStart(GetBuddyWindow(ACustomEdit.Handle));
end;

class function TWin32WSCustomFloatSpinEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  Result := EditGetSelLength(GetBuddyWindow(ACustomEdit.Handle));
end;

class function TWin32WSCustomFloatSpinEdit.GetText(const AWinControl: TWinControl;
  var AText: string): boolean;
begin
  Result := AWinControl.HandleAllocated;
  if not Result then
    exit;
  AText := GetControlText(GetBuddyWindow(AWinControl.Handle));
end;

class function TWin32WSCustomFloatSpinEdit.GetValue(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double;
begin
  Result := GetWin32WindowInfo(ACustomFloatSpinEdit.Handle)^.spinValue;
end;

class procedure TWin32WSCustomFloatSpinEdit.SetReadOnly
  (const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
begin
  Windows.SendMessage(GetBuddyWindow(ACustomEdit.Handle), EM_SETREADONLY,
    Windows.WPARAM(NewReadOnly), 0);
end;

class procedure TWin32WSCustomFloatSpinEdit.SetSelStart(const ACustomEdit: TCustomEdit;
  NewStart: integer);
begin
  EditSetSelStart(GetBuddyWindow(ACustomEdit.Handle), NewStart);
end;

class procedure TWin32WSCustomFloatSpinEdit.SetSelLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
begin
  EditSetSelLength(GetBuddyWindow(ACustomEdit.Handle), NewLength);
end;

class procedure TWin32WSCustomFloatSpinEdit.SetText(const AWinControl: TWinControl;
  const AText: string);
begin
  {$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS
    then Windows.SetWindowTextW(GetBuddyWindow(AWinControl.Handle), PWideChar(UTF8ToUTF16(AText)))
    else Windows.SetWindowText(GetBuddyWindow(AWinControl.Handle), PChar(Utf8ToAnsi(AText)));
  {$else}
    Windows.SetWindowText(GetBuddyWindow(AWinControl.Handle), PChar(AText));
  {$endif}
end;

class procedure TWin32WSCustomFloatSpinEdit.ShowHide(const AWinControl: TWinControl);
var
  Buddy: HWND;
begin
  // call inherited
  TWin32WSWinControl.ShowHide(AWinControl);
  Buddy := GetBuddyWindow(AWinControl.Handle);
  if AWinControl.HandleObjectShouldBeVisible then
    ShowWindow(Buddy, SW_SHOW)
  else
    ShowWindow(Buddy, SW_HIDE);
end;

class procedure TWin32WSCustomFloatSpinEdit.UpdateControl(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
begin
  UpdateFloatSpinEditControl(ACustomFloatSpinEdit.Handle, ACustomFloatSpinEdit);
end;
end.
