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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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

  { TWin32WSCheckListBox }

  TWin32WSCustomCheckListBox = class(TWSCustomCheckListBox)
  private
  protected
  public
    class function  GetChecked(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): boolean; override;
    class function  GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class procedure SetChecked(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AChecked: boolean); override;
  end;


implementation

class function  TWin32WSCustomCheckListBox.GetChecked(const ACheckListBox: TCustomCheckListBox;
  const AIndex: integer): boolean;
begin
  Result := TWin32CheckListBoxStrings(ACheckListBox.Items).Checked[AIndex];
end;

class function  TWin32WSCustomCheckListBox.GetStrings(const ACustomListBox: TCustomListBox): TStrings;
var
  Handle: HWND;
begin
  Handle := ACustomListBox.Handle;
  Result := TWin32CheckListBoxStrings.Create(Handle, ACustomListBox);
  GetWindowInfo(Handle)^.List := Result;
end;

class procedure TWin32WSCustomCheckListBox.SetChecked(const ACheckListBox: TCustomCheckListBox;
  const AIndex: integer; const AChecked: boolean);
var
  SizeRect: Windows.RECT;
  Handle: HWND;
begin
  TWin32CheckListBoxStrings(ACheckListBox.Items).Checked[AIndex] := AChecked;

  // redraw control
  Handle := ACheckListBox.Handle;
  Windows.SendMessage(Handle, LB_GETITEMRECT, AIndex, LPARAM(@SizeRect));
  Windows.InvalidateRect(Handle, @SizeRect, false);
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
