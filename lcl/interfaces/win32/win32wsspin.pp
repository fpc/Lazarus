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
 *  See the file COPYING.LCL, included in this distribution,                 *
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

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Spin, Controls, LCLType,
////////////////////////////////////////////////////
  WSSpin, WSLCLClasses, Windows, Win32Int, WinExt,
  Win32WSStdCtrls, Win32WSControls;
  
type

  { TWin32WSCustomFloatSpinEdit }

  TWin32WSCustomFloatSpinEdit = class(TWSCustomFloatSpinEdit)
  private
  protected
  public
    class procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); override;
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function  GetSelStart(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): integer; override;
    class function  GetSelLength(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): integer; override;
    class function  GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): single; override;

    class procedure SetSelStart(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewLength: integer); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;

    class procedure UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit); override;
  end;

  { TWin32WSFloatSpinEdit }

  TWin32WSFloatSpinEdit = class(TWSFloatSpinEdit)
  private
  protected
  public
  end;


implementation

{ TWin32WSCustomFloatSpinEdit }

procedure UpdateFloatSpinEditControl(const Handle: HWND;
  const AFloatSpinEdit: TCustomFloatSpinEdit);
var
  minval, maxval: integer;
begin
  // initialize extremes
  minval := Trunc(AFloatSpinEdit.MinValue);
  maxval := Trunc(AFloatSpinEdit.MaxValue);
  if (minval = 0) and (maxval = 0) then
  begin
    minval := low(integer);
    maxval := high(integer);
  end;
  SendMessage(Handle, UDM_SETRANGE32, minval, maxval);
  SendMessage(Handle, UDM_SETPOS32, 0, LParam(Trunc(AFloatSpinEdit.Value)));
end;
  
function TWin32WSCustomFloatSpinEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    Buddy := CreateWindowEx(WS_EX_CLIENTEDGE, 'EDIT', StrCaption, Flags Or ES_AUTOHSCROLL, Left, Top, Width, Height, Parent, HMENU(Nil), HInstance, Nil);
    Window := CreateUpDownControl(Flags or DWORD(WS_BORDER or UDS_ALIGNRIGHT or UDS_NOTHOUSANDS or UDS_ARROWKEYS or UDS_WRAP or UDS_SETBUDDYINT),
      0, 0,       // pos -  ignored for buddy
      0, 0,       // size - ignored for buddy
      Parent, 0, HInstance, Buddy,
      0, 0, 0);
    UpdateFloatSpinEditControl(Window, TCustomFloatSpinEdit(AWinControl));
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, true);
  // init buddy
  Params.SubClassWndProc := @WindowProc;
  WindowCreateInitBuddy(AWinControl, Params);
  Params.BuddyWindowInfo^.isChildEdit := true;
  Result := Params.Window;
end;

procedure TWin32WSCustomFloatSpinEdit.AdaptBounds(const AWinControl: TWinControl;
  var Left, Top, Width, Height: integer; var SuppressMove: boolean);
var
  WinHandle, BuddyHandle: HWND;
begin
  WinHandle := AWinControl.Handle;
  // detach from buddy first
  BuddyHandle := Windows.SendMessage(WinHandle, UDM_SETBUDDY, 0, 0);
  MoveWindow(BuddyHandle, Left, Top, Width, Height, True);
  // reattach
  Windows.SendMessage(WinHandle, UDM_SETBUDDY, BuddyHandle, 0);
  SuppressMove := true;
end;

function  TWin32WSCustomFloatSpinEdit.GetSelStart(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit): integer;
begin
  Result := EditGetSelStart(SendMessage(ACustomFloatSpinEdit.Handle,
                            UDM_GETBUDDY, 0, 0));
end;

function  TWin32WSCustomFloatSpinEdit.GetSelLength(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit): integer;
begin
  Result := EditGetSelLength(SendMessage(ACustomFloatSpinEdit.Handle,
                             UDM_GETBUDDY, 0, 0));
end;

function  TWin32WSCustomFloatSpinEdit.GetValue(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit): single;
begin
  Result := SendMessage(ACustomFloatSpinEdit.Handle, UDM_GETPOS32, 0, 0);
end;

procedure TWin32WSCustomFloatSpinEdit.SetSelStart(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewStart: integer);
begin
  EditSetSelStart(SendMessage(ACustomFloatSpinEdit.Handle, UDM_GETBUDDY, 0, 0),
                  NewStart);
end;

procedure TWin32WSCustomFloatSpinEdit.SetSelLength(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewLength: integer);
begin
  EditSetSelLength(SendMessage(ACustomFloatSpinEdit.Handle, UDM_GETBUDDY, 0, 0),
                   NewLength);
end;

procedure TWin32WSCustomFloatSpinEdit.ShowHide(const AWinControl: TWinControl);
var
  Buddy: HWND;
begin
  // call inherited
  TWin32WSWinControl.ShowHide(AWinControl);
  Buddy := SendMessage(AWinControl.Handle, UDM_GETBUDDY, 0, 0);
  if AWinControl.HandleObjectShouldBeVisible then
    ShowWindow(Buddy, SW_SHOW)
  else
    ShowWindow(Buddy, SW_HIDE);

end;

procedure TWin32WSCustomFloatSpinEdit.UpdateControl(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
begin
  UpdateFloatSpinEditControl(ACustomFloatSpinEdit.Handle, ACustomFloatSpinEdit);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomFloatSpinEdit, TWin32WSCustomFloatSpinEdit);
//  RegisterWSComponent(TFloatSpinEdit, TWin32WSFloatSpinEdit);
////////////////////////////////////////////////////
end.
