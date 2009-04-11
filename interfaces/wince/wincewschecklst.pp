{
 *****************************************************************************
 *                            WinCEWSCheckLst.pp                             *
 *                            ------------------                             * 
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
unit WinCEWSCheckLst;

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
  WSCheckLst, WSLCLClasses, WinCEInt, WinCEProc, Windows;

type

  { TWinCEWSCheckListBox }

  { TWinCEWSCustomCheckListBox }

  TWinCEWSCustomCheckListBox = class(TWSCustomCheckListBox)
  published
    class function GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class function  GetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): TCheckBoxState; override;
    class procedure SetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AState: TCheckBoxState); override;
  end;


implementation

class function TWinCEWSCustomCheckListBox.GetStrings(const ACustomListBox: TCustomListBox): TStrings;
var
  Handle: HWND;
begin
  Handle := ACustomListBox.Handle;
  Result := TWinCECheckListBoxStrings.Create(Handle, ACustomListBox);
  GetWindowInfo(Handle)^.List := Result;
end;

class function TWinCEWSCustomCheckListBox.GetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer
  ): TCheckBoxState;
begin
  Result := TWinCECheckListBoxStrings(ACheckListBox.Items).State[AIndex];
end;

class procedure TWinCEWSCustomCheckListBox.SetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AState: TCheckBoxState);
var
  SizeRect: Windows.RECT;
  Handle: HWND;
begin
  TWinCECheckListBoxStrings(ACheckListBox.Items).State[AIndex] := AState;

  // redraw control
  Handle := ACheckListBox.Handle;
  Windows.SendMessage(Handle, LB_GETITEMRECT, AIndex, LPARAM(@SizeRect));
  Windows.InvalidateRect(Handle, @SizeRect, false);
end;

end.
