{ $Id$}
{
 *****************************************************************************
 *                            Win32WSCheckLst.pp                             * 
 *                            ------------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit Win32WSCheckLst;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Classes, CheckLst, StdCtrls,
////////////////////////////////////////////////////
  WSCheckLst, WSLCLClasses, Win32Int, Win32Proc, Windows;

type

  { TWin32WSCustomCheckListBox }

  TWin32WSCustomCheckListBox = class(TWSCustomCheckListBox)
  private
  protected
  public
    class function  GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;

    class function GetItemEnabled(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): Boolean; override;
    class function GetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): TCheckBoxState; override;
    class procedure SetItemEnabled(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AEnabled: Boolean); override;
    class procedure SetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AState: TCheckBoxState); override;
  end;


implementation

class function  TWin32WSCustomCheckListBox.GetStrings(const ACustomListBox: TCustomListBox): TStrings;
var
  Handle: HWND;
begin
  Handle := ACustomListBox.Handle;
  Result := TWin32CheckListBoxStrings.Create(Handle, ACustomListBox);
  GetWindowInfo(Handle)^.List := Result;
end;

class function TWin32WSCustomCheckListBox.GetItemEnabled(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer): Boolean;
begin
  Result := TWin32CheckListBoxStrings(ACheckListBox.Items).Enabled[AIndex];
end;

class function TWin32WSCustomCheckListBox.GetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer
  ): TCheckBoxState;
begin
  Result := TWin32CheckListBoxStrings(ACheckListBox.Items).State[AIndex];
end;

class procedure TWin32WSCustomCheckListBox.SetItemEnabled(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AEnabled: Boolean);
var
  SizeRect: Windows.RECT;
  Handle: HWND;
begin
  TWin32CheckListBoxStrings(ACheckListBox.Items).Enabled[AIndex] := AEnabled;

  // redraw control
  Handle := ACheckListBox.Handle;
  Windows.SendMessage(Handle, LB_GETITEMRECT, AIndex, LPARAM(@SizeRect));
  Windows.InvalidateRect(Handle, @SizeRect, False);
end;

class procedure TWin32WSCustomCheckListBox.SetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AState: TCheckBoxState);
var
  SizeRect: Windows.RECT;
  Handle: HWND;
begin
  TWin32CheckListBoxStrings(ACheckListBox.Items).State[AIndex] := AState;

  // redraw control
  Handle := ACheckListBox.Handle;
  Windows.SendMessage(Handle, LB_GETITEMRECT, AIndex, LPARAM(@SizeRect));
  Windows.InvalidateRect(Handle, @SizeRect, False);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomCheckListBox, TWin32WSCustomCheckListBox);
////////////////////////////////////////////////////
end.
