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
  Spin,
////////////////////////////////////////////////////
  WSSpin, WSLCLClasses, Win32WSStdCtrls, Windows;

type

  { TWin32WSCustomSpinEdit }

  TWin32WSCustomSpinEdit = class(TWSCustomSpinEdit)
  private
  protected
  public
    class function  GetSelStart(const ACustomSpinEdit: TCustomSpinEdit): integer; override;
    class function  GetSelLength(const ACustomSpinEdit: TCustomSpinEdit): integer; override;
    class function  GetValue(const ACustomSpinEdit: TCustomSpinEdit): single; override;

    class procedure SetSelStart(const ACustomSpinEdit: TCustomSpinEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomSpinEdit: TCustomSpinEdit; NewLength: integer); override;
  
    class procedure UpdateControl(const ACustomSpinEdit: TCustomSpinEdit); override;
  end;

  { TWin32WSSpinEdit }

  TWin32WSSpinEdit = class(TWSSpinEdit)
  private
  protected
  public
  end;


implementation

{ TWin32WSCustomSpinEdit }

function  TWin32WSCustomSpinEdit.GetSelStart(const ACustomSpinEdit: TCustomSpinEdit): integer;
begin
  Result := EditGetSelStart(SendMessage(ACustomSpinEdit.Handle, UDM_GETBUDDY, 0, 0));
end;

function  TWin32WSCustomSpinEdit.GetSelLength(const ACustomSpinEdit: TCustomSpinEdit): integer;
begin
  Result := EditGetSelLength(SendMessage(ACustomSpinEdit.Handle, UDM_GETBUDDY, 0, 0));
end;

function  TWin32WSCustomSpinEdit.GetValue(const ACustomSpinEdit: TCustomSpinEdit): single;
begin
  Result := SendMessage(ACustomSpinEdit.Handle, UDM_GETPOS, 0, 0);
end;

procedure TWin32WSCustomSpinEdit.SetSelStart(const ACustomSpinEdit: TCustomSpinEdit; NewStart: integer);
begin
  EditSetSelStart(SendMessage(ACustomSpinEdit.Handle, UDM_GETBUDDY, 0, 0), NewStart); 
end;

procedure TWin32WSCustomSpinEdit.SetSelLength(const ACustomSpinEdit: TCustomSpinEdit; NewLength: integer);
begin
  EditSetSelLength(SendMessage(ACustomSpinEdit.Handle, UDM_GETBUDDY, 0, 0), NewLength); 
end;

procedure TWin32WSCustomSpinEdit.UpdateControl(const ACustomSpinEdit: TCustomSpinEdit);
var
  Handle: HWND;
begin
  Handle := ACustomSpinEdit.Handle;
  SendMessage(Handle, UDM_SETRANGE, 0, MakeLong(Trunc(ACustomSpinEdit.MaxValue), 
    Trunc(ACustomSpinEdit.MinValue)));
  SendMessage(Handle, UDM_SETPOS, 0, MakeLong(Trunc(ACustomSpinEdit.Value), 0));
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomSpinEdit, TWin32WSCustomSpinEdit);
//  RegisterWSComponent(TSpinEdit, TWin32WSSpinEdit);
////////////////////////////////////////////////////
end.
