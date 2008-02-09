{%MainUnit win32wsextctrls.pp}
{ $Id: win32trayicon.inc 11994 2007-09-10 22:30:15Z marc $ }
{******************************************************************************
                 Implementation of TWin32WSCustomTrayIcon

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{ TWin32WSCustomTrayIcon }

const
  szClassName = 'TTrayIconClass';
  szAppTitle = 'apptitle';
  uIDTrayIcon = 25;

var
  vwsTrayIcon: TCustomTrayIcon;

{*******************************************************************
*  TrayWndProc ()
*
*  DESCRIPTION:    Window procedure that processes messages for the
*                 systray icon
*
*  PARAMETERS:     Standard Mouse Messages have this parameters:
*
*                  fwKeys = wParam;        // key flags
*                  xPos = LOWORD(lParam);  // horizontal position of cursor
*                  yPos = HIWORD(lParam);  // vertical position of cursor
*                                          //* Those positions seam to be wrong
*                                          // Use Mouse.CursorPos instead
*
*  RETURNS:        A pointer to the newly created object
*
*******************************************************************}
function TrayWndProc(Handle: HWND; iMsg: UINT; WParam_: WPARAM; LParam_:LPARAM):LRESULT; stdcall;
var
  pt: TPoint;
begin
  if iMsg = WM_USER + uIDTrayIcon then
  begin
     case LParam_ of
      WM_RBUTTONUP:
      begin
        if Assigned(vwsTrayIcon.OnMouseUp) then vwsTrayIcon.OnMouseUp(Application,
         mbRight, KeysToShiftState(WParam_), LOWORD(lParam_), HIWORD(lParam_));
        if Assigned(vwsTrayIcon.PopUpMenu) then
        begin
          pt := Mouse.CursorPos;// Gets cursor position in screen coords

          // Apparently SetForegroundWindow and PostMessage are necessary
          // because we're invoking the shortcut menu from a notification icon
          // This is an attempt to prevent from messing with the Z-order
          SetForegroundWindow(Handle);
          PostMessage(Handle, WM_NULL, 0, 0);
          vwsTrayIcon.PopUpMenu.Popup(pt.x, pt.y);
        end;
      end;
      WM_RBUTTONDOWN: if Assigned(vwsTrayIcon.OnMouseDown) then vwsTrayIcon.OnMouseDown(Application,
       mbRight, KeysToShiftState(WParam_), LOWORD(lParam_), HIWORD(lParam_));
      WM_RBUTTONDBLCLK: if Assigned(vwsTrayIcon.OnDblClick) then vwsTrayIcon.OnDblClick(Application);

      WM_MBUTTONDOWN: if Assigned(vwsTrayIcon.OnMouseDown) then vwsTrayIcon.OnMouseDown(Application,
       mbMiddle, KeysToShiftState(WParam_), LOWORD(lParam_), HIWORD(lParam_));
      WM_MBUTTONUP: if Assigned(vwsTrayIcon.OnMouseUp) then vwsTrayIcon.OnMouseUp(Application,
       mbMiddle, KeysToShiftState(WParam_), LOWORD(lParam_), HIWORD(lParam_));

      WM_LBUTTONUP:
      begin
        if Assigned(vwsTrayIcon.OnMouseUp) then vwsTrayIcon.OnMouseUp(Application,
         mbLeft, KeysToShiftState(WParam_), LOWORD(lParam_), HIWORD(lParam_));
        if Assigned(vwsTrayIcon.OnClick) then vwsTrayIcon.OnClick(Application);
      end;
      WM_LBUTTONDOWN: if Assigned(vwsTrayIcon.OnMouseDown) then vwsTrayIcon.OnMouseDown(Application,
       mbLeft, KeysToShiftState(WParam_), LOWORD(lParam_), HIWORD(lParam_));
      WM_LBUTTONDBLCLK: if Assigned(vwsTrayIcon.OnDblClick) then vwsTrayIcon.OnDblClick(Application);

      WM_MOUSEMOVE: if Assigned(vwsTrayIcon.OnMouseMove) then
       vwsTrayIcon.OnMouseMove(Application, KeysToShiftState(WParam_), LOWORD(lParam_), HIWORD(lParam_));
     end;

     Result := 1;
     Exit;
  end;

  Result := DefWindowProc(Handle, iMsg, WParam_, LParam_);
end;

{ TWin32WSCustomTrayIcon }

{*******************************************************************
*  TWin32WSCustomTrayIcon.Hide ()
*
*  DESCRIPTION:    Hides the main tray icon of the program
*
*  PARAMETERS:     None
*
*  RETURNS:        True if sucessfull, otherwise False
*
*******************************************************************}
class function TWin32WSCustomTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  tnid: TNotifyIconData;
begin
  // Fill TNotifyIconData
  FillChar(tnid, SizeOf(tnid), 0);
  tnid.cbSize := SizeOf(TNotifyIconData);
  tnid.hWnd := ATrayIcon.Handle;
  tnid.uID := uIDTrayIcon;

  // Remove the icon
  Result := Shell_NotifyIconA(NIM_DELETE, @tnid);

  // Destroys the helper Windows
  PostMessage(ATrayIcon.Handle, WM_CLOSE, 0, 0);
  PostMessage(ATrayIcon.Handle, WM_DESTROY, 0, 0);

  Application.ProcessMessages;
end;

{*******************************************************************
*  TWin32WSCustomTrayIcon.Show ()
*
*  DESCRIPTION:    Shows the main tray icon of the program
*
*  PARAMETERS:     None
*
*  RETURNS:        True if sucessfull, otherwise False
*
*******************************************************************}
class function TWin32WSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  tnid: TNotifyIconData;
  buffer: PChar;
  Window: Windows.TWndClassEx;
begin
  vwsTrayIcon := ATrayIcon;

  ZeroMemory(@Window, SizeOf(TWndClassEx));
  Window.cbSize := SizeOf(TWndClassEx);
  Window.style := CS_OWNDC;
  Window.lpfnWndProc := @TrayWndProc;
  Window.cbClsExtra := 0;
  Window.cbWndExtra := 0;
  Window.hInstance := hInstance;
//  Window.hIcon := Icon.Handle;
  Window.hCursor := LoadCursor(0, IDC_ARROW);
  Window.hbrBackground := HBRUSH(GetStockObject(NULL_BRUSH));
  Window.lpszMenuName := nil;
  Window.lpszClassName := szClassName;
//  Window.hIconSm := hSmallIcon;

  Windows.RegisterClassEx(Window);

  ATrayIcon.Handle := CreateWindowEx(
        0,            //* Ensure that there will be no button in the bar */
        szClassName,        //* Name of the registered class */
        szAppTitle,         //* Title of the window */
        0,                  //* Style of the window */
        0,                  //* x-position (at beginning) */
        0,                  //* y-position (at beginning) */
        CW_USEDEFAULT,      //* window width */
        CW_USEDEFAULT,      //* window height */
        0,                  //* handle to parent or owner window */
        0,                  //* handle to menu */
        hInstance,          //* handle to application instance */
        nil);               //* pointer to window-creation data */

  // Fill TNotifyIconData
  FillChar(tnid, SizeOf(tnid), 0);
  tnid.cbSize := SizeOf(TNotifyIconData);
  tnid.hWnd := ATrayIcon.Handle;
  tnid.uID := uIDTrayIcon;
  tnid.uFlags := NIF_MESSAGE or NIF_ICON;
  if (ATrayIcon.Hint <> '') then tnid.uFlags := tnid.uFlags or NIF_TIP;
  tnid.uCallbackMessage := WM_USER + uIDTrayIcon;
  tnid.hIcon := ATrayIcon.Icon.Handle;
  buffer := PChar(ATrayIcon.Hint);
  StrCopy(@tnid.szTip, buffer);

  // Create Taskbar icon
  Result := Shell_NotifyIconA(NIM_ADD, @tnid);
end;

{*******************************************************************
*  TWin32WSCustomTrayIcon.InternalUpdate ()
*
*  DESCRIPTION:    Makes modifications to the Icon while running
*                  i.e. without hiding it and showing again
*
*******************************************************************}
class procedure TWin32WSCustomTrayIcon.InternalUpdate(const ATrayIcon: TCustomTrayIcon);
begin

end;

{*******************************************************************
*  TWin32WSCustomTrayIcon.ShowBalloonHint ()
*
*  DESCRIPTION:    Shows a small message balloon near the tray icon
*
*******************************************************************}
class function TWin32WSCustomTrayIcon.ShowBalloonHint(
 const ATrayIcon: TCustomTrayIcon): Boolean;
{var
  NotifyData: TNotifyIconDataEx:}
begin
  Result := True;
{  NotifyData.szInfo              : array[0..255] of CHAR;
                                    DUMMYUNIONNAME      : record
                                                           case longint of
                                                               0 : ( uTimeout : UINT );
                                                               1 : ( uVersion : UINT );
                                                              end;
                                    szInfoTitle : array[0..63] of CHAR;
                                    dwInfoFlags : DWORD;

  Shell_NotifyIconA():}
end;

{*******************************************************************
*  TWin32WSCustomTrayIcon.GetPosition ()
*
*  DESCRIPTION:    Returns the position of the tray icon on the display.
*                  This function is utilized to show message boxes near
*                  the icon
*
*******************************************************************}
class function TWin32WSCustomTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint;
var
  hWndTray : HWND;
  TrayRect: TRect;
begin
  Result := Point(0, 0);

  hWndTray := FindWindow('Shell_TrayWnd', nil);

  if hWndTray = 0 then Exit;
  
{ Here we could also try to find the position of the toolbar

var
  s : string;
  l : integer;
begin
  SetLength(s, 255);
  l := GetClassName(handle, PChar(s), Length(s));

  if l = 0 then Exit(False);

  SetLength(s, l);

  if Pos('ToolbarWindow32', s) > 0 then
    begin
      lp := handle;
      result := false;
    end
  else
    result := true;
end;
}

  Windows.GetWindowRect(hWndTray, @TrayRect);

  Result.X := TrayRect.Left;
  Result.Y := TrayRect.Top;
end;

