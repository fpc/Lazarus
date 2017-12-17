{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Abstract:
    Show a window before the LCL is initialized.
}
unit raw_window;

{$mode objfpc}{$H+}

interface

{$IFDEF Windows}
{$IFDEF HEAPTRC_WINDOW}
{$IF FPC_FULLVERSION>=20701}
uses
  SysUtils, Windows, Messages;

procedure ShowWindow(AStr : String);
{$ENDIF}
{$ENDIF}
{$ENDIF}

implementation

{$IFDEF Windows}
{$IFDEF HEAPTRC_WINDOW}
{$IF FPC_FULLVERSION>=20701}
Var
  WndHandle,
  ButtonHandle,
  EditHandle : HWND;
  OldSubProc : WNDPROC;

function isset(value: dword; bit: byte): boolean;
begin
   result := value and (1 shl pred(bit)) <> 0;
end;

function WindowWndProc(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam) : LRESULT; stdcall;
Var
  ControlCode, ControlID : Word;
begin
  Result := 0;
  Case uMsg  Of
    WM_DESTROY : PostQuitMessage(0);
    WM_COMMAND : Begin
      ControlCode := HiWord(wParam);
      ControlID := LoWord(wParam);
      Case ControlCode Of
        BN_CLICKED : If lParam = ButtonHandle Then
                       PostMessage(WndHandle, WM_CLOSE, 0, 0);
      end;
    end;
    WM_SETFOCUS:  SetFocus(EditHandle);
    Else
      Result := Windows.DefWindowProc(ahwnd, uMsg, wParam, lParam);
  End;

end;

function EditSubProc(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam) : LRESULT; stdcall;
Var
  AKeyboardState : TKeyboardState;
Begin
  Case uMsg of
    WM_KEYDOWN : Begin
      GetKeyboardState(AKeyboardState);
      If isset(AKeyboardState[VK_CONTROL], 8) And isset(AKeyboardState[VK_A], 8) Then Begin
        SendMessage(EditHandle, EM_SETSEL, 0, -1);
        Exit(0);
      end;
      If isset(AKeyboardState[VK_CONTROL], 8) And isset(AKeyboardState[VK_C], 8) Then Begin
        PostMessage(EditHandle, WM_COPY, 0, 0);
        Exit(0);
      End;
      If isset(AKeyboardState[VK_RETURN], 8) Or isset(AKeyboardState[VK_ESCAPE], 8)  Then Begin
        PostMessage(ButtonHandle, BM_CLICK, 0, 0);
        Exit(0);
      end;
    end;
  End;
  Result := CallWindowProc(OldSubProc, Ahwnd, uMsg, wParam, lParam);
end;


procedure ShowWindow(AStr : String);
Var
  A_Atom : TAtom = 0;
  WndClass : TWndClass;
  Msg: TMsg;
  ScreenWidth, ScreenHeight, MiddleX, MiddleY : LongInt;
Begin
  FillChar(WndClass, SizeOf(TWndClass), 0);

  ScreenWidth := GetSystemMetrics(SM_CXSCREEN);
  ScreenHeight := GetSystemMetrics(SM_CYSCREEN);

  MiddleX := (ScreenWidth - 500) Div 2;
  MiddleY := (ScreenHeight - 500) div 2;

  WndClass.lpszClassName:= 'HEAPTRACE_CLASS';
  WndClass.lpfnWndProc :=  @WindowWndProc;

  WndClass.hInstance := hInstance;
  WndClass.hbrBackground:= 1;
  WndClass.style := CS_HREDRAW or CS_VREDRAW;
  WndClass.hCursor := LoadCursor(0, IDC_ARROW);

  A_Atom := RegisterClass(WndClass);

  WndHandle := CreateWindow(
   WndClass.lpszClassName , // lpClassName, optional
   'Heaptrace results', // lpWindowName, optional
   WS_OVERLAPPEDWINDOW or WS_VISIBLE , // dwStyle
   MiddleX, // x
   MiddleY, // y
   500, // nWidth
   500, // nHeight
   0, // hWndParent
   0, // hMenu
   WndClass.hInstance, // hInstance
   nil  // lpParam
   );

  // Button control

  ButtonHandle := CreateWindow(
   'BUTTON' , // lpClassName, optional
   'OK', // lpWindowName, optional
   WS_TABSTOP or WS_VISIBLE or WS_CHILD or BS_DEFPUSHBUTTON , // dwStyle
   400, // x
   400, // y
   50, // nWidth
   50, // nHeight
   WndHandle, // hWndParent
   0, // hMenu
   WndClass.hInstance, // hInstance
   nil  // lpParam
   );

  // Edit control

  EditHandle := CreateWindow(
   'EDIT' , // lpClassName, optional
   NIL, // lpWindowName, optional
   WS_CHILD or WS_VISIBLE or WS_VSCROLL or WS_HSCROLL or WS_BORDER or ES_LEFT or ES_MULTILINE or ES_AUTOHSCROLL or ES_AUTOVSCROLL or ES_READONLY, // dwStyle
   10, // x
   10, // y
   450, // nWidth
   370, // nHeight
   WndHandle, // hWndParent
   0, // hMenu
   WndClass.hInstance, // hInstance
   nil  // lpParam
   );

  SetWindowText(EditHandle, PChar(UTF8ToAnsi(AStr)));

  OldSubProc := Windows.WNDPROC(GetWindowLongPtr(EditHandle, GWL_WNDPROC));
  SetWindowLongPtr(EditHandle, GWL_WNDPROC, PtrUint(@EditSubProc));

  BringWindowToTop(WndHandle);
  SetFocus(EditHandle);

  while GetMessage(Msg,0,0,0) do
    DispatchMessage(Msg);

  DestroyWindow(ButtonHandle);
  DestroyWindow(EditHandle);
  DestroyWindow(WndHandle);

  UnregisterClass(WndClass.lpszClassName, WndClass.hInstance);
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}

end.

